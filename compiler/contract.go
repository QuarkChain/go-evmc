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

	Code     []byte
	CodeHash common.Hash
	Input    []byte

	// is the execution frame represented by this object a contract deployment
	IsDeployment bool
	IsSystemCall bool

	Gas   uint64
	value *uint256.Int
}

// NewContract returns a new contract environment for the execution of EVM.
func NewContract(caller common.Address, address common.Address, value *uint256.Int, gas uint64) *Contract {
	return &Contract{
		caller:  caller,
		address: address,
		Gas:     gas,
		value:   value,
	}
}

func (c *Contract) SetCompiledCode(compiledCode []byte) {
	c.CompiledCode = compiledCode
}

func (c *Contract) Address() common.Address {
	return c.address
}

// Caller returns the caller of the contract.
//
// Caller will recursively call caller when the contract is a delegate
// call, including that of caller's caller.
func (c *Contract) Caller() common.Address {
	return c.caller
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

// RefundGas refunds gas to the contract
func (c *Contract) RefundGas(gas uint64, logger *tracing.Hooks, reason tracing.GasChangeReason) {
	if gas == 0 {
		return
	}
	if logger != nil && logger.OnGasChange != nil && reason != tracing.GasChangeIgnored {
		logger.OnGasChange(c.Gas, c.Gas+gas, reason)
	}
	c.Gas += gas
}

// TODO: replace SetCallCode logic with direct loading of compiled code.
// SetCallCode sets the code of the contract
func (c *Contract) SetCallCode(hash common.Hash, code []byte) {
	c.Code = code
	c.CodeHash = hash
}
