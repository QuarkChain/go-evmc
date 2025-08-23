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

type HostFunc func(e *EVMExecutor, stack *Stack) int64

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

	instr := e.table[OpCode(opcode)]
	f := instr.execute
	if f == nil {
		panic(fmt.Sprintf("host function for opcode %d not found", opcode))
	}

	// Construct stack for inputs
	// TODO: may create uint256 on-demand as we don't need them all.
	stackData := make([]*uint256.Int, instr.minStack)
	for i := 0; i < instr.minStack; i++ {
		stackData[i] = loadUint256(uintptr(stackPtr), instr.minStack-i-1)
	}

	stack := &Stack{
		data:     stackData,
		stackPtr: uintptr(stackPtr),
	}

	// Set the gas in the contract, which may be used in dynamic gas.
	e.callContext.Contract.Gas = uint64(*gas)

	// All ops with a dynamic memory usage also has a dynamic gas cost.
	var memorySize uint64
	if instr.dynamicGas != nil {
		// calculate the new memory size and expand the memory to fit
		// the operation
		// Memory check needs to be done prior to evaluating the dynamic gas portion,
		// to detect calculation overflows
		if instr.memorySize != nil {
			memSize, overflow := instr.memorySize(stack)
			if overflow {
				return C.int64_t(ExecutionOutOfGas)
			}
			// memory is expanded in words of 32 bytes. Gas
			// is also calculated in words.
			if memorySize, overflow = math.SafeMul(toWordSize(memSize), 32); overflow {
				return C.int64_t(ExecutionOutOfGas)
			}
		}
		// Consume the gas and return an error if not enough gas is available.
		// cost is explicitly set so that the capture state defer method can get the proper cost
		var dynamicCost uint64
		dynamicCost, err := instr.dynamicGas(e.evm, e.callContext.Contract, stack, e.callContext.Memory, memorySize)
		if err != nil {
			return C.int64_t(ExecutionOutOfGas)
		}
		// for tracing: this gas consumption event is emitted below in the debug section.
		if e.callContext.Contract.Gas < dynamicCost {
			return C.int64_t(ExecutionOutOfGas)
		} else {
			e.callContext.Contract.Gas -= dynamicCost
		}
	}

	e.callContext.Memory.Resize(memorySize)

	ret := C.int64_t(f(e, stack))
	*gas = C.uint64_t(e.callContext.Contract.Gas)
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

func hostOpAddMod(e *EVMExecutor, stack *Stack) int64 {
	x := stack.Back(0)
	y := stack.Back(1)
	m := stack.Back(2)
	x.AddMod(x, y, m)
	res := x.Bytes32()
	CopyFromBigToMachine(res[:], getStackElement(stack.stackPtr, 2))

	return int64(ExecutionSuccess)
}

func hostOpMulMod(e *EVMExecutor, stack *Stack) int64 {
	x := stack.Back(0)
	y := stack.Back(1)
	m := stack.Back(2)
	x.MulMod(x, y, m)
	res := x.Bytes32()
	CopyFromBigToMachine(res[:], getStackElement(stack.stackPtr, 2))

	return int64(ExecutionSuccess)
}

func hostOpSstore(e *EVMExecutor, stack *Stack) int64 {
	slot := stack.Back(0).Bytes32()
	value := stack.Back(1).Bytes32()
	contract := e.callContext.Contract
	e.evm.StateDB.SetState(contract.address, slot, value)
	return int64(ExecutionSuccess)
}

func hostOpSload(e *EVMExecutor, stack *Stack) int64 {
	slot := stack.Back(0).Bytes32()
	address := e.callContext.Contract.address
	valueBytes := e.evm.StateDB.GetState(address, slot)
	CopyFromBigToMachine(valueBytes[:], getStackElement(stack.stackPtr, 0))
	return int64(ExecutionSuccess)
}

func hostOpMload(e *EVMExecutor, stack *Stack) int64 {
	stack0 := getStackElement(stack.stackPtr, 0)
	offset := stack.Back(0)
	CopyFromBigToMachine(e.callContext.Memory.GetPtr(offset.Uint64(), 32), stack0)
	return int64(ExecutionSuccess)
}

func hostOpMstore(e *EVMExecutor, stack *Stack) int64 {
	offset := stack.Back(0)
	value := stack.Back(1)
	copy(e.callContext.Memory.GetPtr(offset.Uint64(), 32), value.PaddedBytes(32))

	return int64(ExecutionSuccess)
}

func hostOpMstore8(e *EVMExecutor, stack *Stack) int64 {
	offset := stack.Back(0)
	value := stack.Back(1)

	// Copy the stack value to the memory
	m := e.callContext.Memory.GetPtr(offset.Uint64(), 1)
	if IsMachineBigEndian() {
		m[0] = byte(value.Uint64())
	} else {
		m[0] = byte(value.Uint64())
	}

	return int64(ExecutionSuccess)
}

// Address returns address of the current executing account.
func hostOpAddress(e *EVMExecutor, stack *Stack) int64 {
	stack0 := getStackElement(stack.stackPtr, -1) // push stack
	CopyFromBigToMachine(common.LeftPadBytes(e.callContext.Contract.address[:], 32), stack0)

	return int64(ExecutionSuccess)
}

// Origin returns address of the execution origination address.
func hostOpOrigin(e *EVMExecutor, stack *Stack) int64 {
	stack0 := getStackElement(stack.stackPtr, -1) // push stack
	CopyFromBigToMachine(common.LeftPadBytes(e.evm.TxContext.Origin[:], 32), stack0)

	return int64(ExecutionSuccess)
}

// Caller returns caller address.
func hostOpCaller(e *EVMExecutor, stack *Stack) int64 {
	stack0 := getStackElement(stack.stackPtr, -1) // push stack
	CopyFromBigToMachine(common.LeftPadBytes(e.callContext.Contract.caller[:], 32), stack0)
	return int64(ExecutionSuccess)
}

// Coinbase returns the coinbase address of the block.
func hostOpCoinbase(e *EVMExecutor, stack *Stack) int64 {
	stack0 := getStackElement(stack.stackPtr, -1) // push stack

	CopyFromBigToMachine(common.LeftPadBytes(e.evm.Context.Coinbase[:], 32), stack0)
	return int64(ExecutionSuccess)
}
