package compiler

import (
	"github.com/ethereum/go-ethereum/common"
	"github.com/holiman/uint256"
)

type Contract struct {
	caller       common.Address
	address      common.Address
	CompiledCode []byte

	CodeHash common.Hash

	Gas   uint64
	value *uint256.Int
}

// NewContract returns a new contract environment for the execution of EVM.
func NewContract(caller common.Address, address common.Address, value *uint256.Int, gas uint64, codeHash common.Hash) *Contract {
	return &Contract{
		caller:   caller,
		address:  address,
		CodeHash: codeHash,
		Gas:      gas,
		value:    value,
	}
}

func (c *Contract) SetCompiledCode(compiledCode []byte) {
	c.CompiledCode = compiledCode
}

func (c *Contract) Address() common.Address {
	return c.address
}
