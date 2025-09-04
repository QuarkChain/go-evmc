package compiler

// #include <stdint.h>
// extern int64_t callHostFunc(uintptr_t inst, uint64_t opcode, uint64_t pc, uint64_t* gas, uint32_t* stackIdx);
import "C"
import (
	"fmt"
	"unsafe"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/params"
	"tinygo.org/x/go-llvm"
)

type CompiledLoader interface {
	LoadCompiledCode(codeHash common.Hash, chainRules params.Rules, extraEips []int) ([]byte, CompiledCodeVersion, error)
}

// Default NativeLoader that loads compiled code and returns its function pointer.
type NativeEngine struct {
	engine          llvm.ExecutionEngine
	compiledLoader  CompiledLoader
	loadedContracts map[common.Hash]bool
}

var _ NativeLoader = (*NativeEngine)(nil)

func NewNativeEngine(loader CompiledLoader) *NativeEngine {
	ctx := llvm.NewContext()
	module := ctx.NewModule("evm_module")

	engine, err := llvm.NewJITCompiler(module, 3)
	if err != nil {
		panic(fmt.Errorf("failed to create JIT compiler: %v", err))
	}

	nativeEngine := &NativeEngine{
		engine:          engine,
		compiledLoader:  loader,
		loadedContracts: map[common.Hash]bool{},
	}
	_, hostFunc := initializeHostFunction(ctx, module)
	engine.AddGlobalMapping(hostFunc, unsafe.Pointer(C.callHostFunc))
	return nativeEngine
}

// TODO: support forks and version management for different loader.
func (n *NativeEngine) CompiledFuncPtr(codeHash common.Hash, chainRules params.Rules, extraEips []int) (FuncPtr, CompiledCodeVersion, error) {
	if codeHash == (common.Hash{}) { // Create and Create2 are not supported in native executor.
		return FuncPtr(0), "", fmt.Errorf("codeHash:%v is nil", codeHash)
	}
	var version CompiledCodeVersion
	if _, ok := n.loadedContracts[codeHash]; !ok {
		compiledCode, ver, err := n.compiledLoader.LoadCompiledCode(codeHash, chainRules, extraEips)
		if err != nil {
			return 0, version, err
		}
		n.engine.AddObjectFileFromBuffer(compiledCode)
		n.loadedContracts[codeHash] = true
		version = ver
	}

	funcPtr := n.engine.GetFunctionAddress(GetContractFunction(codeHash))

	if funcPtr == 0 {
		return FuncPtr(0), "", fmt.Errorf("compiled contract code not found")
	}

	return FuncPtr(funcPtr), version, nil
}

func (n *NativeEngine) Dispose() {
	n.engine.Dispose()
}
