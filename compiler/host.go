package compiler

// #include <stdint.h>
// extern int64_t callHostFunc(uintptr_t inst, uint64_t opcode, uint64_t pc, uint64_t* gas, uint32_t* stackIdx);
import "C"
import (
	"fmt"
	"runtime/cgo"
	"unsafe"

	"github.com/ethereum/go-ethereum/common/math"
	"github.com/holiman/uint256"
	"tinygo.org/x/go-llvm"
)

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
func callHostFunc(inst C.uintptr_t, opcode C.uint64_t, pcC C.uint64_t, gas *C.uint64_t, stackIdx *C.uint32_t) C.int64_t {
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
	stack := e.callContext.Stack
	stack.resetLen(int(*stackIdx))

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
				return C.int64_t(VMErrorCodeGasUintOverflow)
			}
			// memory is expanded in words of 32 bytes. Gas
			// is also calculated in words.
			if memorySize, overflow = math.SafeMul(toWordSize(memSize), 32); overflow {
				return C.int64_t(VMErrorCodeGasUintOverflow)
			}
		}
		// Consume the gas and return an error if not enough gas is available.
		// cost is explicitly set so that the capture state defer method can get the proper cost
		dynamicCost, err := instr.dynamicGas(e.evm, e.callContext.Contract, stack, e.callContext.Memory, memorySize)
		if err != nil {
			return C.int64_t(VMErrorCodeOutOfGas)
		}
		// for tracing: this gas consumption event is emitted below in the debug section.
		if e.callContext.Contract.Gas < dynamicCost {
			return C.int64_t(VMErrorCodeOutOfGas)
		} else {
			e.callContext.Contract.Gas -= dynamicCost
		}
	}

	e.callContext.Memory.Resize(memorySize)

	// pc is already handled in compiler, so there is no need to pass pc back to native.
	pc := uint64(pcC)
	ret, err := f(&pc, e, e.callContext)
	errCode := vmErrorCodeFromErr(err)
	// Pass ret to parent context
	e.evm.internalRet = ret
	*gas = C.uint64_t(e.callContext.Contract.Gas)

	return C.int64_t(errCode)
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
	i32ptr := llvm.PointerType(ctx.Int32Type(), 0)
	i64ptr := llvm.PointerType(ctx.Int64Type(), 0)
	hostFuncType = llvm.FunctionType(ctx.Int64Type(), []llvm.Type{ctx.Int64Type(), ctx.Int64Type(), ctx.Int64Type(), i64ptr, i32ptr}, false)
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
