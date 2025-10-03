package compiler

// #include <stdint.h>
// #include "./lib/mem.h"
// typedef Memory* (*func)(uint64_t inst, void* stack, void* code, uint64_t gas, void* output);
// static Memory* execute(uint64_t f, uint64_t inst, void* stack, void* code, uint64_t gas, void* output) { return ((func)f)(inst, stack, code, gas, output); }
import "C"
import (
	"encoding/binary"
	"fmt"
	"runtime/cgo"
	"unsafe"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/crypto"
	"github.com/ethereum/go-ethereum/params"
)

type CompiledCodeVersion string
type FuncPtr uint64
type NativeLoader interface {
	// CompiledFuncPtr retrieves the function pointer associated with the codeHash and fork.
	CompiledFuncPtr(codeHash common.Hash, chainRules params.Rules, extraEips []int) (FuncPtr, CompiledCodeVersion, error)
}

// An executor to execute native compiled code within EVM.
// Data layout rules:
// - Pushed data on stack: always big-endian padded first (handled in createUint256ConstantFromBytes)
// - Stack elements: always machine-endian encoded (LLVM-native representation)
// - Memory: always big-endian encoded as in execution spec
// - Storage: always big-endian encoded as in execution spec
type EVMExecutor struct {
	callContext  *ScopeContext // current call context
	nativeLoader NativeLoader

	evm   *EVM
	table *JumpTable

	hasher    crypto.KeccakState // Keccak256 hasher instance shared across opcodes
	hasherBuf common.Hash        // Keccak256 hasher result array shared across opcodes

	readOnly   bool   // Whether to throw on stateful modifications
	returnData []byte // Last CALL's return data for subsequent reuse
	keepMemory bool   // Whether to free memory after callNativeFunction
}

// ScopeContext is the current context of the call.
type ScopeContext struct {
	Memory   *Memory
	Contract *Contract
	Stack    *Stack
}

func NewEVMExecutor(evm *EVM, nativeLoader NativeLoader) *EVMExecutor {
	table, extraEips, err := getJumpTable(evm.chainRules, evm.Config.ExtraEips)
	if err != nil {
		panic(fmt.Sprintf("Failed to get jumpTable: %s", err))
	}
	evm.Config.ExtraEips = extraEips
	e := &EVMExecutor{
		nativeLoader: nativeLoader,
		evm:          evm,
		table:        table,
		hasher:       crypto.NewKeccakState(),
	}
	return e
}

func (e *EVMExecutor) AddInstructionTable(table *JumpTable) {
	if table == nil {
		// no table is found
		return
	}
	e.table = table
}

// Run a contract.
func (e *EVMExecutor) Run(contract *Contract, input []byte, readOnly bool) (ret *EVMExecutionResult, err error) {
	// Don't bother with the execution if there's no code.
	if len(contract.Code) == 0 {
		return nil, nil
	}

	funcPtr, _, err := e.nativeLoader.CompiledFuncPtr(contract.CodeHash, e.evm.chainRules, e.evm.Config.ExtraEips)
	if err != nil {
		return nil, err
	}

	// Increment the call depth which is restricted to 1024
	e.evm.depth++
	defer func() { e.evm.depth-- }()

	// Make sure the readOnly is only set if we aren't in readOnly yet.
	// This also makes sure that the readOnly flag isn't removed for child calls.
	if readOnly && !e.readOnly {
		e.readOnly = true
		defer func() { e.readOnly = false }()
	}

	// Reset the previous call's return data. It's unimportant to preserve the old buffer
	// as every returning call will return new data anyway.
	e.returnData = nil

	prevCallContext := e.callContext
	// Prepare execution environment
	memory := &Memory{}
	stack := newstack()
	callContext := &ScopeContext{
		Contract: contract,
		Stack:    stack,
		Memory:   memory,
	}
	e.callContext = callContext

	defer func() {
		returnStack(stack)
		// here memory is not freed for checking testCase's memory
		// memory.Free()
		// Recover prev context cause e.callContext is used to store gas/memory/stack in the whole tx execution lifecycle.
		e.callContext = prevCallContext
		e.evm.internalRet = []byte{}
	}()
	contract.Input = input

	// Execute using function pointer
	inst := createExecutionInstance(e)
	defer removeExecutionInstance(inst)
	gas := contract.Gas

	// TODO: passing uint256 pointer as stack to compiled code only works for little-endianess machine!
	errorCode, gasRemainingResult, stackDepth, cmem := e.callNativeFunction(funcPtr, inst, unsafe.Pointer(&stack.data[0][0]), gas)
	if errorCode == int64(VMErrorCodeStopToken) {
		errorCode = int64(VMExecutionSuccess) // clear stop token error
	}

	if cmem != nil {
		memory.cmem = cmem
		memory.store = unsafe.Slice((*byte)(cmem.store), cmem.len)
		memory.lastGasCost = uint64(cmem.lastGasCost)
	}

	gasRemaining := uint64(gasRemainingResult)

	if errorCode != int64(VMExecutionSuccess) {
		return &EVMExecutionResult{
			Stack:        nil,
			Memory:       nil,
			Status:       ExecutionStatus(errorCode),
			GasUsed:      gas - gasRemaining,
			GasLimit:     gas,
			GasRemaining: gasRemaining,
			Ret:          e.evm.internalRet,
		}, vmErrorCodeToErr(errorCode)
	}

	// Update remaining gas of the contract call
	// contract.Gas only include hostFunc's gas, while gasRemaining accounts for hostFunc+nativeCall gas
	contract.Gas = gasRemaining

	stackByte := make([][32]byte, stackDepth)
	stack.resetLen(int(stackDepth))
	for i := 0; i < int(stackDepth); i++ {
		stackByte[i] = FromMachineToBig32Bytes(stack.data[i].Bytes32())
	}

	return &EVMExecutionResult{
		Stack:        stackByte,
		Memory:       memory,
		Status:       ExecutionStatus(errorCode),
		GasUsed:      gas - gasRemaining,
		GasLimit:     gas,
		GasRemaining: gasRemaining,
		Ret:          e.evm.internalRet,
	}, nil
}

// Helper function to call native function pointer (requires CGO)
func (e *EVMExecutor) callNativeFunction(funcPtr FuncPtr, inst cgo.Handle, stack unsafe.Pointer, gas uint64) (int64, int64, int64, *C.Memory) {
	var output [OUTPUT_SIZE]byte
	cmem := C.execute(C.uint64_t(funcPtr), C.uintptr_t(inst), stack, nil, C.uint64_t(gas), unsafe.Pointer(&output[0]))
	errorCode := binary.LittleEndian.Uint64(output[OUTPUT_IDX_ERROR_CODE*8:])
	gasUsed := binary.LittleEndian.Uint64(output[OUTPUT_IDX_GAS*8:])
	stackDepth := binary.LittleEndian.Uint64(output[OUTPUT_IDX_STACK_DEPTH*8:])
	return int64(errorCode), int64(gasUsed), int64(stackDepth), cmem
}
