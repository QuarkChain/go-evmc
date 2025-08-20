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
	"github.com/holiman/uint256"
	"tinygo.org/x/go-llvm"
)

type HostFunc func(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64

type EVMHost interface {
	GetHostFunc(opcode EVMOpcode) HostFunc
}

type DefaultHost struct {
	state       map[common.Address]map[common.Hash]common.Hash
	hostFuncMap map[EVMOpcode]HostFunc
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

	f := e.host.GetHostFunc(EVMOpcode(opcode))
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

func NewDefaultHost() *DefaultHost {
	h := &DefaultHost{
		state: make(map[common.Address]map[common.Hash]common.Hash),
	}
	h.hostFuncMap = map[EVMOpcode]HostFunc{
		COINBASE: h.Coinbase,
		MLOAD:    h.Mload,
		MSTORE:   h.Mstore,
		SLOAD:    h.Sload,
		SSTORE:   h.Sstore,
	}
	return h
}

func (h *DefaultHost) GetHostFunc(opcode EVMOpcode) HostFunc {
	return h.hostFuncMap[opcode]
}

// A simple Sstore with constant 20000 gas
func (h *DefaultHost) Sstore(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64 {
	if *gas < 20000 {
		return int64(ExecutionOutOfGas)
	}
	*gas -= 20000

	addrBytes := e.callContext.Contract.address
	if _, ok := h.state[addrBytes]; !ok {
		h.state[addrBytes] = make(map[common.Hash]common.Hash)
	}

	m := h.state[addrBytes]
	keyBytes := common.BytesToHash(getStackElement(stackPtr, 0))
	valueBytes := common.BytesToHash(getStackElement(stackPtr, 1))
	m[keyBytes] = valueBytes

	return int64(ExecutionSuccess)
}

// A simple Sload with constant 5000 gas
func (h *DefaultHost) Sload(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64 {
	if *gas < 5000 {
		return int64(ExecutionOutOfGas)
	}
	*gas -= 5000

	keyOrValue := getStackElement(stackPtr, 0)

	m := h.state[e.callContext.Contract.address]
	if m == nil {
		zeroBytes(keyOrValue)
		return int64(ExecutionSuccess)
	}

	keyBytes := common.BytesToHash(keyOrValue)
	valueBytes := m[keyBytes]
	copy(keyOrValue, valueBytes[:])

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

// Mload behaves the same as Geth's.
func (h *DefaultHost) Mload(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64 {
	stack0 := getStackElement(stackPtr, 0)
	offset := new(uint256.Int).SetBytes(FromMachineToBigInplace(stack0))
	_, errno := chargeMemoryGasAndResize(gas, e.callContext.Memory, offset, 32)
	if errno != int64(ExecutionSuccess) {
		return errno
	}

	CopyFromBigToMachine(e.callContext.Memory.GetPtr(offset.Uint64(), 32), stack0)

	return int64(ExecutionSuccess)
}

// Mstore behaves the same as Geth's.
func (h *DefaultHost) Mstore(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64 {
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

// Coinbase returns the coinbase address of the block.
func (h *DefaultHost) Coinbase(gas *uint64, e *EVMExecutor, stackPtr uintptr) int64 {
	stack0 := getStackElement(stackPtr, 0)

	CopyFromBigToMachine(new(uint256.Int).SetBytes(e.evm.Context.Coinbase.Bytes()).Bytes(), stack0)

	return int64(ExecutionSuccess)
}
