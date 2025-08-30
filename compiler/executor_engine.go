package compiler

// #include <stdint.h>
// extern int64_t callHostFunc(uintptr_t inst, uint64_t opcode, uint64_t pc, uint64_t* gas, uint32_t* stackIdx);
import "C"
import (
	"fmt"
	"unsafe"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/crypto"
	"tinygo.org/x/go-llvm"
)

type EVMEngineConfig struct {
	CompilerOpts   *EVMCompilationOpts
	CompiledLoader MakeLoader
}

type NativeEngine struct {
	ctx             llvm.Context
	module          llvm.Module
	engine          llvm.ExecutionEngine
	hostFuncType    llvm.Type
	hostFunc        llvm.Value
	table           *JumpTable
	copts           *EVMCompilationOpts
	compiledLoader  NativeLoader
	loadedContracts map[common.Hash]bool
}

func NewNativeEngine(copts *EVMCompilationOpts, table *JumpTable, loaderFn MakeLoader) *NativeEngine {
	ctx := llvm.NewContext()
	module := ctx.NewModule("evm_module")

	hostFuncType, hostFunc := initializeHostFunction(ctx, module)
	engine, err := llvm.NewJITCompiler(module, 3)
	if err != nil {
		panic(fmt.Errorf("failed to create JIT compiler: %v", err))
	}

	engine.AddGlobalMapping(hostFunc, unsafe.Pointer(C.callHostFunc))
	nativeEngine := &NativeEngine{
		ctx:             ctx,
		module:          module,
		engine:          engine,
		hostFuncType:    hostFuncType,
		hostFunc:        hostFunc,
		table:           table,
		copts:           copts,
		loadedContracts: map[common.Hash]bool{},
	}
	nativeEngine.compiledLoader = loaderFn(nativeEngine)
	return nativeEngine
}

func (n *NativeEngine) Dispose() {
	n.compiledLoader.Dispose()
	n.module.Dispose()
	n.ctx.Dispose()
}

func (n *NativeEngine) LoadCompiledContract(contract *Contract) (uint64, error) {
	if len(contract.Code) < 1 {
		// no compiled code is found
		return 0, fmt.Errorf("contract has no code")
	}
	// Recalculate the hash because SetCallCode may not set the contract's CodeHash to the actual value.
	codeHash := crypto.Keccak256Hash(contract.Code)
	if _, ok := n.loadedContracts[codeHash]; !ok {
		compiledCode, err := n.compiledLoader.LoadCompiledContract(contract)
		if err != nil {
			return 0, err
		}
		n.engine.AddObjectFileFromBuffer(compiledCode)
		n.loadedContracts[codeHash] = true
	}

	funcPtr := n.engine.GetFunctionAddress(GetContractFunction(codeHash))
	if funcPtr == 0 {
		return 0, fmt.Errorf("compiled contract code not found")
	}
	return funcPtr, nil
}
