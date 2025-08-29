package compiler

// #include <stdint.h>
// typedef void (*func)(uint64_t inst, void* stack, void* code, uint64_t gas, void* output);
// static void execute(uint64_t f, uint64_t inst, void* stack, void* code, uint64_t gas, void* output) { ((func)f)(inst, stack, code, gas, output); }
import "C"
import (
	"encoding/binary"
	"runtime/cgo"
	"unsafe"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/crypto"
	"github.com/ethereum/go-ethereum/log"
)

// An executor to execute native compiled code within EVM.
// Data layout rules:
// - Pushed data on stack: always big-endian padded first (handled in createUint256ConstantFromBytes)
// - Stack elements: always machine-endian encoded (LLVM-native representation)
// - Memory: always big-endian encoded as in execution spec
// - Storage: always big-endian encoded as in execution spec
type EVMExecutor struct {
	callContext  *ScopeContext // current call context
	engine       *NativeEngine
	nativeLoader NativeLoader

	evm   *EVM
	table *JumpTable

	hasher    crypto.KeccakState // Keccak256 hasher instance shared across opcodes
	hasherBuf common.Hash        // Keccak256 hasher result array shared across opcodes

	readOnly   bool   // Whether to throw on stateful modifications
	returnData []byte // Last CALL's return data for subsequent reuse
}

// ScopeContext is the current context of the call.
type ScopeContext struct {
	Memory   *Memory
	Contract *Contract
	Stack    *Stack
}

func NewEVMExecutor(evm *EVM, copts *EVMCompilationOpts, loaderFn MakeLoader) *EVMExecutor {
	// If jump table was not initialised we set the default one.
	var table *JumpTable
	switch {
	case evm.chainRules.IsVerkle:
		// TODO replace with proper instruction set when fork is specified
		table = &verkleInstructionSet
	case evm.chainRules.IsPrague:
		table = &pragueInstructionSet
	case evm.chainRules.IsCancun:
		table = &cancunInstructionSet
	case evm.chainRules.IsShanghai:
		table = &shanghaiInstructionSet
	case evm.chainRules.IsMerge:
		table = &mergeInstructionSet
	case evm.chainRules.IsLondon:
		table = &londonInstructionSet
	case evm.chainRules.IsBerlin:
		table = &berlinInstructionSet
	case evm.chainRules.IsIstanbul:
		table = &istanbulInstructionSet
	case evm.chainRules.IsConstantinople:
		table = &constantinopleInstructionSet
	case evm.chainRules.IsByzantium:
		table = &byzantiumInstructionSet
	case evm.chainRules.IsEIP158:
		table = &spuriousDragonInstructionSet
	case evm.chainRules.IsEIP150:
		table = &tangerineWhistleInstructionSet
	case evm.chainRules.IsHomestead:
		table = &homesteadInstructionSet
	default:
		table = &frontierInstructionSet
	}
	var extraEips []int
	if len(evm.Config.ExtraEips) > 0 {
		// Deep-copy jumptable to prevent modification of opcodes in other tables
		table = copyJumpTable(table)
	}
	for _, eip := range evm.Config.ExtraEips {
		if err := EnableEIP(eip, table); err != nil {
			// Disable it, so caller can check if it's activated or not
			log.Error("EIP activation failed", "eip", eip, "error", err)
		} else {
			extraEips = append(extraEips, eip)
		}
	}
	evm.Config.ExtraEips = extraEips
	engine := NewNativeEngine(copts, table, loaderFn)
	e := &EVMExecutor{
		engine: engine,
		evm:    evm,
		table:  table,
		hasher: crypto.NewKeccakState(),
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

	funcPtr, err := e.engine.LoadCompiledContract(contract)
	if err != nil {
		return nil, err
	}

	// Increment the call depth which is restricted to 1024
	e.evm.depth++
	defer func() { e.evm.depth-- }()

	// Make sure the readOnly is only set if we aren't in readOnly yet.
	// This also makes sure that the readOnly flag isn't removed for child calls.
	if readOnly && !e.readOnly {
		prevReadonly := e.readOnly
		e.readOnly = true
		defer func() { e.readOnly = prevReadonly }()
	}

	// Reset the previous call's return data. It's unimportant to preserve the old buffer
	// as every returning call will return new data anyway.
	e.returnData = nil

	prevCallContext := e.callContext
	// Prepare execution environment
	memory := NewMemory()
	stack := newstack()
	callContext := &ScopeContext{
		Contract: contract,
		Memory:   memory,
		Stack:    stack,
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
	errorCode, gasRemainingResult, stackDepth := e.callNativeFunction(funcPtr, inst, unsafe.Pointer(&stack.data[0][0]), gas)

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

func (e *EVMExecutor) Dispose() {
	e.engine.Dispose()
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
