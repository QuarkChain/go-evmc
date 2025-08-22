package compiler

// #include <stdint.h>
// typedef void (*func)(uint64_t inst, void* stack, void* code, uint64_t gas, void* output);
// static void execute(uint64_t f, uint64_t inst, void* stack, void* code, uint64_t gas, void* output) { ((func)f)(inst, stack, code, gas, output); }
import "C"
import (
	"encoding/binary"
	"fmt"
	"runtime/cgo"
	"unsafe"

	"github.com/ethereum/go-ethereum/common"
	"tinygo.org/x/go-llvm"
)

// An executor to execute native compiled code within EVM.
type EVMExecutor struct {
	callContext *CallContext // current call context

	ctx    llvm.Context
	module llvm.Module
	engine *llvm.ExecutionEngine

	evm *EVM

	hostFuncType llvm.Type
	hostFunc     llvm.Value
	table        *JumpTable

	loadedContracts map[common.Hash]bool
}

// CallContext is the current context of the call.
// It is similar to ScopeContext in geth, but does not have Stack because compiled code manages the stack by itself.
type CallContext struct {
	Memory   *Memory
	Contract *Contract
}

type EVMExecutorOptions struct {
}

func NewEVMExecutor(evm *EVM) *EVMExecutor {
	ctx := llvm.NewContext()
	module := ctx.NewModule("evm_module")

	hostFuncType, hostFunc := initializeHostFunction(ctx, module)

	engine, err := llvm.NewJITCompiler(module, 3)
	if err != nil {
		panic(fmt.Errorf("failed to create JIT compiler: %v", err))
	}

	table, extraEips, err := getJumpTable(evm.chainRules, evm.Config.ExtraEips)
	if err != nil {
		// return
		// TODO: log error
	}

	evm.Config.ExtraEips = extraEips

	e := &EVMExecutor{
		ctx:             ctx,
		module:          module,
		engine:          &engine,
		evm:             evm,
		hostFuncType:    hostFuncType,
		hostFunc:        hostFunc,
		loadedContracts: make(map[common.Hash]bool),
		table:           table,
	}
	e.addGlobalMappingForHostFunctions()
	return e
}

func (e *EVMExecutor) AddCompiledContract(codeHash common.Hash, compiledCode []byte) {
	if compiledCode == nil {
		// no compiled code is found
		return
	}
	if _, ok := e.loadedContracts[codeHash]; !ok {
		e.engine.AddObjectFileFromBuffer(compiledCode)
		e.loadedContracts[codeHash] = true
	}
}

// Run a contract.
func (e *EVMExecutor) Run(contract Contract, input []byte, readOnly bool) (ret *EVMExecutionResult, err error) {
	e.AddCompiledContract(contract.CodeHash, contract.CompiledCode)
	// Get function pointer for direct execution
	funcPtr := e.engine.GetFunctionAddress(GetContractFunction(contract.CodeHash))
	if funcPtr == 0 {
		return nil, fmt.Errorf("compiled contract code not found")
	}

	// Prepare execution environment
	const stackSize = 1024
	stack := make([][32]byte, stackSize)
	memory := NewMemory()
	e.callContext = &CallContext{
		Contract: &contract,
		Memory:   memory,
	}

	// Execute using function pointer
	inst := createExecutionInstance(e)
	defer removeExecutionInstance(inst)
	errorCode, gasRemainingResult, stackDepth := e.callNativeFunction(funcPtr, inst, unsafe.Pointer(&stack[0]), contract.Gas)

	gasRemaining := uint64(gasRemainingResult)

	if errorCode != int64(ExecutionSuccess) {
		return &EVMExecutionResult{
			Stack:        nil,
			Memory:       nil,
			Status:       ExecutionStatus(errorCode),
			GasUsed:      contract.Gas - gasRemaining,
			GasLimit:     contract.Gas,
			GasRemaining: gasRemaining,
		}, nil
	}

	return &EVMExecutionResult{
		Stack:        stack[:stackDepth],
		Memory:       memory,
		Status:       ExecutionStatus(errorCode),
		GasUsed:      contract.Gas - gasRemaining,
		GasLimit:     contract.Gas,
		GasRemaining: gasRemaining,
	}, nil
}

func (e *EVMExecutor) Dispose() {
	e.engine.Dispose()
	// c.module.Dispose() TODO: segfault after execute() - does engine take the ownership of the module
	e.ctx.Dispose()
}

// Helper function to call native function pointer (requires CGO)
func (e *EVMExecutor) callNativeFunction(funcPtr uint64, inst cgo.Handle, stack unsafe.Pointer, gas uint64) (int64, int64, int64) {
	var output [OUTPUT_SIZE]byte
	C.execute(C.uint64_t(funcPtr), C.uintptr_t(inst), stack, nil, C.uint64_t(gas), unsafe.Pointer(&output[0]))
	errorCode := binary.LittleEndian.Uint64(output[OUTPUT_IDX_ERROR_CODE*8:])
	gasUsed := binary.LittleEndian.Uint64(output[OUTPUT_IDX_GAS*8:])
	stackDepth := binary.LittleEndian.Uint64(output[OUTPUT_IDX_STACK_DEPTH*8:])
	return int64(errorCode), int64(gasUsed), int64(stackDepth)
}
