package compiler

import (
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/tracing"
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

// UseGas attempts the use gas and subtracts it and returns true on success
func (c *Contract) UseGas(gas uint64, logger *tracing.Hooks, reason tracing.GasChangeReason) (ok bool) {
	if c.Gas < gas {
		return false
	}
	if logger != nil && logger.OnGasChange != nil && reason != tracing.GasChangeIgnored {
		logger.OnGasChange(c.Gas, c.Gas-gas, reason)
	}
	c.Gas -= gas
	return true
}
