package compiler

// #include <stdint.h>
// extern int64_t callHostFunc(uintptr_t inst, uint64_t opcode, uint64_t* gas, uintptr_t key);
import "C"
import (
	"fmt"
	"runtime/cgo"
	"unsafe"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/common/math"
	"github.com/ethereum/go-ethereum/params"
	"github.com/holiman/uint256"
	"tinygo.org/x/go-llvm"
)

type HostFunc func(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64

type EVMHost interface {
	GetHostFunc(opcode OpCode) HostFunc
}

func createExecutionInstance(inst *EVMExecutor) cgo.Handle {
	return cgo.NewHandle(inst)
}

func removeExecutionInstance(h cgo.Handle) {
	h.Delete()
}

//export callHostFunc
func callHostFunc(inst C.uintptr_t, opcode C.uint64_t, gas *C.uint64_t, stackPtr C.uintptr_t) C.int64_t {
	h := cgo.Handle(inst)
	e, ok := h.Value().(*EVMExecutor)
	if !ok || e == nil {
		panic("execution instance not found or type mismatch")
	}

	f := e.table[OpCode(opcode)].execute
	if f == nil {
		panic(fmt.Sprintf("host function for opcode %d not found", opcode))
	}

	gas1 := uint64(*gas)
	ret := C.int64_t(f(&gas1, e, uintptr(stackPtr)))
	*gas = C.uint64_t(gas1)
	return ret
}

// getStackElement returns the slice of the 32-byte stack element
func getStackElement(stackPtr uintptr, idx int) []byte {
	return unsafe.Slice((*byte)(unsafe.Pointer(uintptr(stackPtr)-uintptr(32*idx))), 32)
}

// loadUint256 loads the stack element at idx as a uint256.Int (big-endian).
func loadUint256(stackPtr uintptr, idx int) *uint256.Int {
	b := getStackElement(stackPtr, idx)
	return new(uint256.Int).SetBytes(FromMachineToBigInplace(b))
}

func initializeHostFunction(ctx llvm.Context, module llvm.Module) (hostFuncType llvm.Type, hostFunc llvm.Value) {
	i8ptr := llvm.PointerType(ctx.Int8Type(), 0)
	i64ptr := llvm.PointerType(ctx.Int64Type(), 0)
	hostFuncType = llvm.FunctionType(ctx.Int64Type(), []llvm.Type{ctx.Int64Type(), ctx.Int64Type(), i64ptr, i8ptr}, false)
	hostFunc = llvm.AddFunction(module, "host_func", hostFuncType)
	// Set the host function's linkage to External.
	// This prevents LLVM from internalizing it during LTO/IPO passes,
	// ensuring it remains visible outside the module.
	hostFunc.SetLinkage(llvm.ExternalLinkage)
	usedArray := llvm.ConstArray(llvm.PointerType(hostFuncType, 0), []llvm.Value{hostFunc})
	// Place the `llvm.used` global in the "llvm.metadata" section.
	// LLVM recognizes this section specially, which is necessary
	// for the used-function mechanism to work correctly.
	usedGlobal := llvm.AddGlobal(module, usedArray.Type(), "llvm.used")
	usedGlobal.SetInitializer(usedArray)
	usedGlobal.SetLinkage(llvm.AppendingLinkage)
	usedGlobal.SetSection("llvm.metadata")
	return hostFuncType, hostFunc
}

func (c *EVMCompiler) initailizeHostFunctions() {
	c.hostFuncType, c.hostFunc = initializeHostFunction(c.ctx, c.module)
}

func (e *EVMExecutor) addGlobalMappingForHostFunctions() {
	e.engine.AddGlobalMapping(e.hostFunc, unsafe.Pointer(C.callHostFunc))
}

func hostOpAddMod(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64 {
	x := loadUint256(stackPtr, 0)
	y := loadUint256(stackPtr, 1)
	m := loadUint256(stackPtr, 2)
	x.AddMod(x, y, m)
	res := x.Bytes32()
	CopyFromBigToMachine(res[:], getStackElement(stackPtr, 2))

	return int64(ExecutionSuccess)
}

func hostOpSstore(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64 {
	slot := common.BytesToHash(FromMachineToBigInplace(getStackElement(stackPtr, 0)))
	value := common.BytesToHash(FromMachineToBigInplace(getStackElement(stackPtr, 1)))
	contract := e.callContext.Contract
	errno := chargeSstoreDynGas(gas, e, contract, stackPtr, e.callContext.Memory, uint64(e.callContext.Memory.Len()))
	if errno != int64(ExecutionSuccess) {
		return errno
	}

	e.evm.StateDB.SetState(contract.address, slot, value)
	return int64(ExecutionSuccess)
}

func hostOpSload(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64 {
	key := getStackElement(stackPtr, 0)
	slot := common.BytesToHash(FromMachineToBigInplace(key))
	address := e.callContext.Contract.address

	errno := chargeSloadDynGas(gas, e, address, slot, e.callContext.Memory, uint64(e.callContext.Memory.Len()))
	if errno != int64(ExecutionSuccess) {
		return errno
	}

	valueBytes := e.evm.StateDB.GetState(address, slot)
	CopyFromBigToMachine(valueBytes[:], key)

	return int64(ExecutionSuccess)
}

func chargeSstoreDynGas(gas *uint64, e *EVMExecutor, contract *Contract, stackPtr uintptr, mem *Memory, memorySize uint64) int64 {
	var gasFn gasFunc
	originGas := contract.Gas
	contract.Gas = *gas
	defer func() { contract.Gas = originGas }()

	if e.evm.chainRules.IsLondon {
		gasFn = makeGasSStoreFunc(params.SstoreClearsScheduleRefundEIP3529)
	} else {
		panic("Forks below London are not supported")
	}
	gasCost, err := gasFn(e.evm, contract, stackPtr, mem, memorySize)
	if err != nil {
		// TODO: check error?
		return int64(ExecutionOutOfGas)
	}
	// Deduct static gas already accounted for in consumeSectionGas
	// TODO: consider fork
	gasCost -= e.table[SSTORE].constantGas

	if *gas < gasCost {
		return int64(ExecutionOutOfGas)
	}
	*gas -= gasCost
	return int64(ExecutionSuccess)
}

func chargeSloadDynGas(gas *uint64, e *EVMExecutor, address common.Address, slot common.Hash, mem *Memory, memorySize uint64) int64 {
	var gasCost uint64
	var err error
	if e.evm.chainRules.IsLondon {
		gasCost, err = gasSLoadEIP2929(e.evm, address, slot, mem, memorySize)
	} else {
		panic("Forks below London are not supported")
	}
	if err != nil {
		// TODO: check error?
		return int64(ExecutionOutOfGas)
	}
	// Deduct static gas already accounted for in consumeSectionGas
	// TODO: consider fork
	gasCost -= e.table[SLOAD].constantGas

	if *gas < gasCost {
		return int64(ExecutionOutOfGas)
	}
	*gas -= gasCost
	return int64(ExecutionSuccess)
}

func chargeMemoryGasAndResize(gas *uint64, memory *Memory, offset *uint256.Int, length uint64) (uint64, int64) {
	// obtain memory size by offset + length
	memSize, overflow := calcMemSize64WithUint(offset, length)
	if overflow {
		// TODO: gas overflow error?
		return 0, int64(ExecutionOutOfGas)
	}

	// memory is expanded in words of 32 bytes. Gas is also calculated in words.
	if memSize, overflow = math.SafeMul(toWordSize(memSize), 32); overflow {
		return 0, int64(ExecutionOutOfGas)
	}

	gasCost, err := memoryGasCost(memory, memSize)
	if err != nil {
		// TODO: check error?
		return 0, int64(ExecutionOutOfGas)
	}

	if *gas < gasCost {
		return 0, int64(ExecutionOutOfGas)
	}
	*gas -= gasCost

	memory.Resize(memSize)
	return memSize, int64(ExecutionSuccess)
}

func hostOpMload(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64 {
	stack0 := getStackElement(stackPtr, 0)
	offset := new(uint256.Int).SetBytes(FromMachineToBigInplace(stack0))
	_, errno := chargeMemoryGasAndResize(gas, e.callContext.Memory, offset, 32)
	if errno != int64(ExecutionSuccess) {
		return errno
	}

	CopyFromBigToMachine(e.callContext.Memory.GetPtr(offset.Uint64(), 32), stack0)
	return int64(ExecutionSuccess)
}

func hostOpMstore(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64 {
	stack0 := getStackElement(stackPtr, 0)
	offset := new(uint256.Int).SetBytes(FromMachineToBigInplace(stack0))
	_, errno := chargeMemoryGasAndResize(gas, e.callContext.Memory, offset, 32)
	if errno != int64(ExecutionSuccess) {
		return errno
	}

	// Copy the stack value to the memory
	value := getStackElement(stackPtr, 1)
	CopyFromMachineToBig(value, e.callContext.Memory.GetPtr(offset.Uint64(), 32))

	return int64(ExecutionSuccess)
}

func hostOpMstore8(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64 {
	stack0 := getStackElement(stackPtr, 0)
	offset := new(uint256.Int).SetBytes(FromMachineToBigInplace(stack0))
	_, errno := chargeMemoryGasAndResize(gas, e.callContext.Memory, offset, 1)
	if errno != int64(ExecutionSuccess) {
		return errno
	}

	// Copy the stack value to the memory
	m := e.callContext.Memory.GetPtr(offset.Uint64(), 1)
	value := getStackElement(stackPtr, 1)
	if IsMachineBigEndian() {
		m[0] = value[31]
	} else {
		m[0] = value[0]
	}

	return int64(ExecutionSuccess)
}

// Coinbase returns the coinbase address of the block.
func hostOpCoinbase(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64 {
	stack0 := getStackElement(stackPtr, -1) // push stack

	CopyFromBigToMachine(new(uint256.Int).SetBytes(e.evm.Context.Coinbase.Bytes()).Bytes(), stack0)

	return int64(ExecutionSuccess)
}
