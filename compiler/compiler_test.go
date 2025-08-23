package compiler

import (
	"testing"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/state"
	"github.com/ethereum/go-ethereum/core/vm"
	"github.com/ethereum/go-ethereum/core/vm/runtime"
	"github.com/ethereum/go-ethereum/params"
	"github.com/holiman/uint256"
)

var defaultExecutionOpts = EVMExecutionOpts{
	BlockCtx: vm.BlockContext{},
	Config: &runtime.Config{
		ChainConfig: params.TestChainConfig,
	},
}

func TestEVMCompilerBasic(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	bytecode := []byte{
		0x60, 0x05, // PUSH1 5
		0x60, 0x03, // PUSH1 3
		0x01, // ADD
		0x00, // STOP
	}

	err := comp.CompileAndOptimize(bytecode)
	if err != nil {
		t.Fatalf("Compilation failed: %v", err)
	}

	err = comp.EmitLLVMIR("test_output.ll")
	if err != nil {
		t.Fatalf("Failed to emit LLVM IR: %v", err)
	}
}

func TestEVMCompilerParseBytecode(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	bytecode := []byte{
		0x60, 0x2A, // PUSH1 6
		0x80, // DUP1
		0x90, // SWAP1
		0x50, // POP
		0x56, // JUMP to PC 6
		0x5B, // JUMPDEST
		0x00, // STOP
	}

	instructions, err := comp.ParseBytecode(bytecode)
	if err != nil {
		t.Fatalf("Failed to parse bytecode: %v", err)
	}

	expectedOpcodes := []OpCode{
		PUSH1,
		DUP1,
		SWAP1,
		POP,
		JUMP,
		JUMPDEST,
		STOP,
	}

	if len(instructions) != len(expectedOpcodes) {
		t.Fatalf("Expected %d instructions, got %d", len(expectedOpcodes), len(instructions))
	}

	for i, instr := range instructions {
		if instr.Opcode != expectedOpcodes[i] {
			t.Errorf("Instruction %d: expected opcode %02x, got %02x", i, expectedOpcodes[i], instr.Opcode)
		}
	}

	if len(instructions[0].Data) != 1 || instructions[0].Data[0] != 0x2A {
		t.Errorf("PUSH1 instruction should have data [0x2A], got %v", instructions[0].Data)
	}
}

func TestEVMCompilerArithmetic(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	bytecode := []byte{
		0x60, 0x0A, // PUSH1 10
		0x60, 0x05, // PUSH1 5
		0x02,       // MUL
		0x60, 0x14, // PUSH1 20
		0x03,       // SUB
		0x60, 0x02, // PUSH1 2
		0x04, // DIV
		0x00, // STOP
	}

	err := comp.CompileAndOptimize(bytecode)
	if err != nil {
		t.Fatalf("Compilation failed: %v", err)
	}
}

func TestEVMCompilerControlFlow(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	bytecode := []byte{
		0x60, 0x01, // PUSH1 1
		0x60, 0x08, // PUSH1 8 (jump target)
		0x57,       // JUMPI (conditional jump)
		0x00,       // STOP (should be skipped)
		0x5B,       // JUMPDEST (PC 8)
		0x60, 0xFF, // PUSH1 255
		0x00, // STOP
	}

	err := comp.CompileAndOptimize(bytecode)
	if err != nil {
		t.Fatalf("Compilation failed: %v", err)
	}
}

func TestEVMCompilerMemoryOperations(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	bytecode := []byte{
		0x60, 0x42, // PUSH1 0x42 (value)
		0x60, 0x00, // PUSH1 0x00 (offset)
		0x52,       // MSTORE
		0x60, 0x00, // PUSH1 0x00 (offset)
		0x51, // MLOAD
		0x00, // STOP
	}

	err := comp.CompileAndOptimize(bytecode)
	if err != nil {
		t.Fatalf("Compilation failed: %v", err)
	}
}

func TestEVMExecuteSimpleAddition(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	// PUSH1 5, PUSH1 3, ADD, STOP
	bytecode := []byte{0x60, 0x05, 0x60, 0x03, 0x01, 0x00}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if result.Status != ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", result.Status)
	}

	// TODO: check stack size?
	// if len(result.Stack) != 1 {
	// 	t.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	// }

	// Check if the result is 8 (5 + 3)
	v := uint256.NewInt(8)

	if result.Stack[0] != Reverse32Bytes(v.Bytes32()) {
		t.Fatalf("Expected stack top to be %v, got %v", v.Bytes32(), result.Stack[0])
	}
}

func TestEVMExecuteMultiplication(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	// PUSH1 7, PUSH1 6, MUL, STOP
	bytecode := []byte{0x60, 0x07, 0x60, 0x06, 0x02, 0x00}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if result.Status != ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", result.Status)
	}

	// TODO:
	// if len(result.Stack) != 1 {
	// 	t.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	// }

	// Check if the result is 42 (7 * 6)
	expected := uint256.NewInt(42).Bytes32()

	if FromMachineToBig32Bytes(result.Stack[0]) != expected {
		t.Fatalf("Expected stack top to be 42, got %v", result.Stack[0])
	}
}

func TestEVMExecuteStackPushN(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	// PUSH2 0x1122, PUSH4 0xaabbccdd, PUSH12 0x0102030405060708090a0b0c, STOP
	bytecode := []byte{0x61, 0x11, 0x22, 0x63, 0xaa, 0xbb, 0xcc, 0xdd, 0x6b, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0x00}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if result.Status != ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", result.Status)
	}

	// TODO:
	// if len(result.Stack) != 1 {
	// 	t.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	// }

	// Check if the result is 0x1122, 0xaabbccdd, 0x0102030405060708090a0b0c
	expected := uint256.NewInt(0x1122)

	if FromMachineToUint256(result.Stack[0]).Cmp(expected) != 0 {
		t.Fatalf("Expected stack top to be 0x1122, got %v", result.Stack[0])
	}

	if FromMachineToUint256(result.Stack[1]).Cmp(uint256.NewInt(0xaabbccdd)) != 0 {
		t.Fatalf("Expected stack top to be 0xaabbccdd, got %v", result.Stack[1])
	}

	if FromMachineToUint256(result.Stack[2]).Cmp(uint256.MustFromHex("0x102030405060708090a0b0c")) != 0 {
		t.Fatalf("Expected stack top to be 0x0102030405060708090a0b0c, got %v", result.Stack[2])
	}
}

func TestEVMExecuteMemoryOperations(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	// PUSH1 0x42, PUSH1 0x00, MSTORE, PUSH1 0x00, MLOAD, STOP
	bytecode := []byte{0x60, 0x42, 0x60, 0x00, 0x52, 0x60, 0x00, 0x51, 0x00}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if result.Status != ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", result.Status)
	}

	if len(result.Stack) != 1 {
		t.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	}

	// Check if the loaded value is 0x42
	expected := uint256.NewInt(0x42).Bytes32()

	if FromMachineToBig32Bytes(result.Stack[0]) != expected {
		t.Fatalf("Expected stack top to be 0x42, got %v", result.Stack[0])
	}

	// Check that memory was used
	if result.Memory.Len() != 32 {
		t.Fatalf("Expected memory to be used")
	}
}

func TestEVMExecuteComparison(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	// PUSH1 5, PUSH1 3, LT, STOP (3 < 5 should be true = 1)
	bytecode := []byte{0x60, 0x05, 0x60, 0x03, 0x10, 0x00}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if result.Status != ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", result.Status)
	}

	// TODO: check stack size?
	// if len(result.Stack) != 1 {
	// 	t.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	// }

	// Check if the result is 1 (true)
	expected := uint256.NewInt(1).Bytes32()

	if FromMachineToBig32Bytes(result.Stack[0]) != expected {
		t.Fatalf("Expected stack top to be 1 (true), got %v", result.Stack[0])
	}
}

func TestEVMExecuteStackOperations(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	// PUSH1 0x42, DUP1, STOP (should have two copies of 0x42 on stack)
	bytecode := []byte{0x60, 0x42, 0x80, 0x00}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if result.Status != ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", result.Status)
	}

	if len(result.Stack) != 2 {
		t.Fatalf("Expected 2 stack items, got %d", len(result.Stack))
	}

	expected := uint256.NewInt(0x42).Bytes32()

	// Both stack items should be 0x42
	if FromMachineToBig32Bytes(result.Stack[0]) != expected || FromMachineToBig32Bytes(result.Stack[1]) != expected {
		t.Fatalf("Expected both stack items to be 0x42, got %v and %v", result.Stack[0], result.Stack[1])
	}
}

func TestEVMExecuteFib(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	n := uint32(10000000)

	result, err := comp.ExecuteCompiledWithOpts(GetFibCode(n), DefaultEVMCompilationOpts(), &EVMExecutionOpts{Config: &runtime.Config{GasLimit: uint64(n) * 100, ChainConfig: params.TestChainConfig}})
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if result.Status != ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", result.Status)
	}

	if len(result.Stack) != 1 {
		t.Fatalf("Expected 1 stack items, got %d", len(result.Stack))
	}

	expected := Fib(int(n))
	actual := FromMachineToUint256(result.Stack[0])

	if actual.Cmp(expected) != 0 {
		t.Fatalf("Expected %s, got %s", expected, actual)
	}

	var (
		evm      = vm.NewEVM(vm.BlockContext{}, &dummyStatedb{}, params.TestChainConfig, vm.Config{})
		contract = vm.NewContract(common.Address{}, common.Address{}, new(uint256.Int), uint64(n)*100, nil)
	)

	contract.Code = GetFibCode(uint32(n))
	_, err = evm.Interpreter().Run(contract, []byte{}, false)
	if err != nil {
		t.Fatal(err)
	}

	if contract.Gas != result.GasRemaining {
		t.Fatalf("Expected gas %d, got %d", contract.Gas, result.GasRemaining)
	}
}

func TestEVMExecuteFibSectionGasOptimization(t *testing.T) {
	compOpt := NewEVMCompiler()
	defer compOpt.Dispose()

	n := uint32(10000000)

	resultOpt, err := compOpt.ExecuteCompiledWithOpts(GetFibCode(n), DefaultEVMCompilationOpts(), &EVMExecutionOpts{Config: &runtime.Config{GasLimit: uint64(n) * 100, ChainConfig: params.TestChainConfig}})
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if resultOpt.Status != ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", resultOpt.Status)
	}

	actualOpt := FromMachineToUint256(resultOpt.Stack[0])

	compNoOpt := NewEVMCompiler()
	defer compNoOpt.Dispose()

	copts := DefaultEVMCompilationOpts()
	copts.DisableSectionGasOptimization = true
	resultNoOpt, err := compNoOpt.ExecuteCompiledWithOpts(GetFibCode(n), copts, &EVMExecutionOpts{Config: &runtime.Config{GasLimit: uint64(n) * 100, ChainConfig: params.TestChainConfig}})
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if resultNoOpt.Status != ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", resultNoOpt.Status)
	}

	actualNoOpt := FromMachineToUint256(resultOpt.Stack[0])
	if actualOpt.Cmp(actualNoOpt) != 0 {
		t.Fatalf("ActualOpt %s, ActualNoOpt %s", actualOpt, actualNoOpt)
	}

	if resultOpt.GasUsed != resultNoOpt.GasUsed {
		t.Fatalf("GasUsedOpt %d, GasNoOpt %d`", resultOpt.GasUsed, resultNoOpt.GasUsed)
	}
}

func BenchmarkEVMCompilerSmallContract(b *testing.B) {
	bytecode := []byte{
		0x60, 0x05, // PUSH1 5
		0x60, 0x03, // PUSH1 3
		0x01, // ADD
		0x00, // STOP
	}

	for i := 0; i < b.N; i++ {
		comp := NewEVMCompiler()
		err := comp.CompileAndOptimize(bytecode)
		if err != nil {
			b.Fatalf("Compilation failed: %v", err)
		}
		comp.Dispose()
	}
}

func BenchmarkEVMCompilerMediumContract(b *testing.B) {
	bytecode := make([]byte, 0, 200)

	for i := 0; i < 20; i++ {
		bytecode = append(bytecode, 0x60, byte(i)) // PUSH1 i
	}

	for i := 0; i < 19; i++ {
		bytecode = append(bytecode, 0x01) // ADD
	}

	bytecode = append(bytecode, 0x00) // STOP

	for i := 0; i < b.N; i++ {
		comp := NewEVMCompiler()
		err := comp.CompileAndOptimize(bytecode)
		if err != nil {
			b.Fatalf("Compilation failed: %v", err)
		}
		comp.Dispose()
	}
}

func BenchmarkEVMExecuteSimpleAddition(b *testing.B) {
	bytecode := []byte{0x60, 0x05, 0x60, 0x03, 0x01, 0x00} // PUSH1 5, PUSH1 3, ADD, STOP

	comp := NewEVMCompiler()
	defer comp.Dispose()

	// Pre-compile
	err := comp.CompileAndOptimize(bytecode)
	if err != nil {
		b.Fatalf("Compilation failed: %v", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := comp.ExecuteCompiled(bytecode)
		if err != nil {
			b.Fatalf("Execution failed: %v", err)
		}
	}
}

func BenchmarkEVMExecuteComplexArithmetic(b *testing.B) {
	// More complex arithmetic: multiple operations
	bytecode := []byte{
		0x60, 0x0A, // PUSH1 10
		0x60, 0x05, // PUSH1 5
		0x02,       // MUL (50)
		0x60, 0x14, // PUSH1 20
		0x03,       // SUB (30)
		0x60, 0x02, // PUSH1 2
		0x04,       // DIV (15)
		0x60, 0x03, // PUSH1 3
		0x01, // ADD (18)
		0x00, // STOP
	}

	comp := NewEVMCompiler()
	defer comp.Dispose()

	// Pre-compile
	err := comp.CompileAndOptimize(bytecode)
	if err != nil {
		b.Fatalf("Compilation failed: %v", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := comp.ExecuteCompiled(bytecode)
		if err != nil {
			b.Fatalf("Execution failed: %v", err)
		}
	}
}

func BenchmarkEVMExecuteMemoryOperations(b *testing.B) {
	// Memory store and load operations
	bytecode := []byte{
		0x60, 0x42, // PUSH1 0x42
		0x60, 0x00, // PUSH1 0x00
		0x52,       // MSTORE
		0x60, 0x00, // PUSH1 0x00
		0x51,       // MLOAD
		0x60, 0x20, // PUSH1 0x20 (32)
		0x52,       // MSTORE
		0x60, 0x20, // PUSH1 0x20
		0x51, // MLOAD
		0x00, // STOP
	}

	comp := NewEVMCompiler()
	defer comp.Dispose()

	// Pre-compile
	err := comp.CompileAndOptimize(bytecode)
	if err != nil {
		b.Fatalf("Compilation failed: %v", err)
	}

	// Create executor
	err = comp.CreateExecutor(&defaultExecutionOpts)
	if err != nil {
		b.Fatalf("Executor failed: %v", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := comp.Execute(&EVMExecutionOpts{Config: &runtime.Config{ChainConfig: params.TestChainConfig}})
		if err != nil {
			b.Fatalf("Execution failed: %v", err)
		}
	}
}

func BenchmarkEVMExecuteFibWithSectionGasOptimization(b *testing.B) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	// Pre-compile
	n := uint32(1000000)
	err := comp.CompileAndOptimize(GetFibCode(n))
	if err != nil {
		b.Fatalf("Compilation failed: %v", err)
	}
	err = comp.CreateExecutor(&defaultExecutionOpts)
	if err != nil {
		b.Fatalf("Engine failed: %v", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := comp.Execute(&EVMExecutionOpts{Config: &runtime.Config{GasLimit: uint64(n) * 100}})
		if err != nil {
			b.Fatalf("Execution failed: %v", err)
		}
	}
}

func BenchmarkEVMExecuteFibWithGas(b *testing.B) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	// Pre-compile
	n := uint32(1000000)
	copts := DefaultEVMCompilationOpts()
	copts.DisableSectionGasOptimization = true
	err := comp.CompileAndOptimizeWithOpts(GetFibCode(n), copts)
	if err != nil {
		b.Fatalf("Compilation failed: %v", err)
	}
	err = comp.CreateExecutor(&defaultExecutionOpts)
	if err != nil {
		b.Fatalf("Engine failed: %v", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := comp.Execute(&EVMExecutionOpts{Config: &runtime.Config{GasLimit: uint64(n) * 100}})
		if err != nil {
			b.Fatalf("Execution failed: %v", err)
		}
	}
}

func BenchmarkEVMExecuteFibWithoutGas(b *testing.B) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	// Pre-compile
	n := uint32(1000000)
	err := comp.CompileAndOptimizeWithOpts(GetFibCode(n), &EVMCompilationOpts{DisableGas: true})
	if err != nil {
		b.Fatalf("Compilation failed: %v", err)
	}
	err = comp.CreateExecutor(&defaultExecutionOpts)
	if err != nil {
		b.Fatalf("Engine failed: %v", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := comp.Execute(&EVMExecutionOpts{Config: &runtime.Config{GasLimit: uint64(n) * 100}})
		if err != nil {
			b.Fatalf("Execution failed: %v", err)
		}
	}
}

func BenchmarkNativeExecuteFib(b *testing.B) {
	for i := 0; i < b.N; i++ {
		a := Fib(1000000)
		if a.CmpUint64(0) == 0 {
			b.Fatal()
		}
	}
}

type dummyStatedb struct {
	state.StateDB
}

func BenchmarkEVMExecuteFibInterp(b *testing.B) {
	var (
		n   = uint64(1000000)
		evm = vm.NewEVM(vm.BlockContext{}, &dummyStatedb{}, params.TestChainConfig, vm.Config{})
	)

	for i := 0; i < b.N; i++ {
		contract := vm.NewContract(common.Address{}, common.Address{}, new(uint256.Int), n*100, nil)
		contract.Code = GetFibCode(uint32(n))
		_, err := evm.Interpreter().Run(contract, []byte{}, false)
		if err != nil {
			b.Fatal(err)
		}
	}
}
