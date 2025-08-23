package compiler

import (
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/state"
	"github.com/ethereum/go-ethereum/core/vm"
	"github.com/ethereum/go-ethereum/params"
)

// EVM with native code support.
type EVM struct {
	// Context provides auxiliary blockchain related information
	Context vm.BlockContext
	vm.TxContext

	// StateDB gives access to the underlying state
	StateDB vm.StateDB

	// depth is the current call stack
	depth int

	// chainConfig contains information about the current chain
	chainConfig *params.ChainConfig

	// chain rules contains the chain rules for the current epoch
	chainRules params.Rules

	// virtual machine configuration options used to initialise the evm
	Config vm.Config

	// global (to this context) ethereum virtual machine used throughout
	// the execution of the tx
	executor *EVMExecutor

	// callGasTemp holds the gas available for the current call. This is needed because the
	// available gas is calculated in gasCall* according to the 63/64 rule and later
	// applied in opCall*.
	callGasTemp uint64

	// precompiles holds the precompiled contracts for the current epoch
	precompiles map[common.Address]PrecompiledContract
}

// NewEVM constructs an EVM instance with the supplied block context, state
// database and several configs. It meant to be used throughout the entire
// state transition of a block, with the transaction context switched as
// needed by calling evm.SetTxContext.
func NewEVM(blockCtx vm.BlockContext, statedb vm.StateDB, chainConfig *params.ChainConfig, config vm.Config) *EVM {
	evm := &EVM{
		Context:     blockCtx,
		StateDB:     statedb,
		Config:      config,
		chainConfig: chainConfig,
		chainRules:  chainConfig.Rules(blockCtx.BlockNumber, blockCtx.Random != nil, blockCtx.Time),
	}
	evm.precompiles = activePrecompiledContracts(evm.chainRules)
	evm.executor = NewEVMExecutor(evm)
	return evm
}

// SetTxContext resets the EVM with a new transaction context.
// This is not threadsafe and should only be done very cautiously.
func (evm *EVM) SetTxContext(txCtx vm.TxContext) {
	if evm.chainRules.IsEIP4762 {
		txCtx.AccessEvents = state.NewAccessEvents(evm.StateDB.PointCache())
	}
	evm.TxContext = txCtx
}
