package compiler

import (
	"fmt"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/state"
	"github.com/ethereum/go-ethereum/params"
)

// Loader that retrieves code by codeHash from disk, compiles the code and emits the compiled code.
type JITLoader struct {
	codeReader state.ContractCodeReader
	copts      *EVMCompilationOpts
}

var (
	_ CompiledLoader = (*JITLoader)(nil)
)

func (l *JITLoader) LoadCompiledCode(hash common.Hash, chainRules params.Rules, extraEips []int) ([]byte, CompiledCodeVersion, error) {
	c := NewEVMCompiler(chainRules, extraEips)
	defer c.Dispose()

	code, err := l.codeReader.Code(common.Address{}, hash)
	if err != nil || len(code) == 0 {
		return []byte{}, "", fmt.Errorf("code for codeHash:%v not found", hash)
	}
	c.CompileAndOptimizeWithOpts(code, l.copts)
	return c.GetCompiledCode(), "", nil
}
