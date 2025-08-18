package compiler

// #include <stdint.h>
// typedef void (*func)(uint64_t inst, void* memory, void* stack, void* code, uint64_t gas, void* output);
// static void execute(uint64_t f, uint64_t inst, void* memory, void* stack, void* code, uint64_t gas, void* output) { ((func)f)(inst, memory, stack, code, gas, output); }
import "C"
import (
	"encoding/binary"
	"fmt"
	"unsafe"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/vm"
	"tinygo.org/x/go-llvm"
)

// An executor to execute native compiled code within EVM.
type EVMExecutor struct {
	callContext CallContext // current call context

	ctx    llvm.Context
	module llvm.Module
	engine *llvm.ExecutionEngine

	host         EVMHost
	hostFuncType llvm.Type
	hostFunc     llvm.Value
}

// CallContext is the current context of the call.
// It is similar to ScopeContext in geth, but does not have Stack because compiled code manages the stack by itself.
type CallContext struct {
	Memory   *vm.Memory
	Contract *Contract
}

type Contract struct {
	address      common.Address
	compiledCode []byte
}

type EVMExecutorOptions struct {
	Host EVMHost
}

func NewEVMExecutor(opts *EVMExecutorOptions) *EVMExecutor {
	ctx := llvm.NewContext()
	module := ctx.NewModule("evm_module")

	hostFuncType, hostFunc := initializeHostFunction(ctx, module)

	engine, err := llvm.NewJITCompiler(module, 3)
	if err != nil {
		panic(fmt.Errorf("failed to create JIT compiler: %v", err))
	}

	e := &EVMExecutor{
		ctx:          ctx,
		module:       module,
		engine:       &engine,
		host:         opts.Host,
		hostFuncType: hostFuncType,
		hostFunc:     hostFunc,
	}
	e.addGlobalMappingForHostFunctions()
	return e
}

// Run a contract.
func (e *EVMExecutor) Run(contract *Contract, input []byte, gasLimit uint64, readOnly bool) (ret *EVMExecutionResult, err error) {
	e.engine.AddObjectFileFromBuffer(contract.compiledCode)

	// Get function pointer for direct execution
	funcPtr := e.engine.GetFunctionAddress(GetContractFunction(contract.address))
	if funcPtr == 0 {
		return nil, fmt.Errorf("execute function address not found")
	}

	// Prepare execution environment
	// TODO: support dynamic size growth
	const stackSize = 1024
	const memorySize = 1024 * 32

	stack := make([][32]byte, stackSize)
	memory := make([]byte, memorySize)

	// Execute using function pointer
	inst := createExecutionInstance(e)
	defer removeExecutionInstance(inst)
	gasRemainingResult, stackDepth, err := e.callNativeFunction(funcPtr, inst, unsafe.Pointer(&memory[0]), unsafe.Pointer(&stack[0]), gasLimit)
	if err != nil {
		return &EVMExecutionResult{
			Stack:        nil,
			Memory:       nil,
			Status:       ExecutionError,
			Error:        err,
			GasUsed:      0,
			GasLimit:     gasLimit,
			GasRemaining: gasLimit,
		}, nil
	}

	// Check for out-of-gas condition
	if gasRemainingResult == -1 {
		return &EVMExecutionResult{
			Stack:        nil,
			Memory:       nil,
			Status:       ExecutionOutOfGas,
			Error:        fmt.Errorf("out of gas"),
			GasUsed:      gasLimit,
			GasLimit:     gasLimit,
			GasRemaining: 0,
		}, nil
	}

	gasRemaining := uint64(gasRemainingResult)

	memoryUsed := len(memory)
	for i := len(memory) - 1; i >= 0; i-- {
		if memory[i] != 0 {
			memoryUsed = i + 1
			break
		}
	}

	return &EVMExecutionResult{
		Stack:        stack[:stackDepth],
		Memory:       memory[:memoryUsed],
		Status:       ExecutionSuccess,
		Error:        nil,
		GasUsed:      gasLimit - gasRemaining,
		GasLimit:     gasLimit,
		GasRemaining: gasRemaining,
	}, nil
}

func (e *EVMExecutor) Dispose() {
	e.engine.Dispose()
	// c.module.Dispose() TODO: segfault after execute() - does engine take the ownership of the module
	e.ctx.Dispose()
}

// Helper function to call native function pointer (requires CGO)
func (e *EVMExecutor) callNativeFunction(funcPtr uint64, inst uint64, memory, stack unsafe.Pointer, gas uint64) (int64, int64, error) {
	var output [OUTPUT_SIZE]byte
	C.execute(C.uint64_t(funcPtr), C.uint64_t(inst), memory, stack, nil, C.uint64_t(gas), unsafe.Pointer(&output[0]))
	gasUsed := binary.LittleEndian.Uint64(output[OUTPUT_IDX_GAS*8:])
	stackDepth := binary.LittleEndian.Uint64(output[OUTPUT_IDX_STACK_DEPTH*8:])
	return int64(gasUsed), int64(stackDepth), nil
}
