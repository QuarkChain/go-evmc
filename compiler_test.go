package main

import (
	"testing"

	"github.com/QuarkChain/go-evmc/compiler"
	"github.com/holiman/uint256"
)

func TestEVMCompilerBasic(t *testing.T) {
	comp := compiler.NewEVMCompiler()
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
	comp := compiler.NewEVMCompiler()
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

	expectedOpcodes := []compiler.EVMOpcode{
		compiler.PUSH1,
		compiler.DUP1,
		compiler.SWAP1,
		compiler.POP,
		compiler.JUMP,
		compiler.JUMPDEST,
		compiler.STOP,
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
	comp := compiler.NewEVMCompiler()
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
	comp := compiler.NewEVMCompiler()
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
	comp := compiler.NewEVMCompiler()
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
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// PUSH1 5, PUSH1 3, ADD, STOP
	bytecode := []byte{0x60, 0x05, 0x60, 0x03, 0x01, 0x00}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if result.Status != compiler.ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", result.Status)
	}

	// TODO: check stack size?
	// if len(result.Stack) != 1 {
	// 	t.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	// }

	// Check if the result is 8 (5 + 3)
	v := uint256.NewInt(8)

	if result.Stack[0] != compiler.Reverse32Bytes(v.Bytes32()) {
		t.Fatalf("Expected stack top to be %v, got %v", v.Bytes32(), result.Stack[0])
	}
}

func TestEVMExecuteMultiplication(t *testing.T) {
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// PUSH1 7, PUSH1 6, MUL, STOP
	bytecode := []byte{0x60, 0x07, 0x60, 0x06, 0x02, 0x00}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if result.Status != compiler.ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", result.Status)
	}

	// TODO:
	// if len(result.Stack) != 1 {
	// 	t.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	// }

	// Check if the result is 42 (7 * 6)
	expected := uint256.NewInt(42).Bytes32()

	if compiler.FromMachineToBig32Bytes(result.Stack[0]) != expected {
		t.Fatalf("Expected stack top to be 42, got %v", result.Stack[0])
	}
}

func TestEVMExecuteMemoryOperations(t *testing.T) {
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// PUSH1 0x42, PUSH1 0x00, MSTORE, PUSH1 0x00, MLOAD, STOP
	bytecode := []byte{0x60, 0x42, 0x60, 0x00, 0x52, 0x60, 0x00, 0x51, 0x00}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if result.Status != compiler.ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", result.Status)
	}

	if len(result.Stack) != 1 {
		t.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	}

	// Check if the loaded value is 0x42
	expected := uint256.NewInt(0x42).Bytes32()

	if compiler.FromMachineToBig32Bytes(result.Stack[0]) != expected {
		t.Fatalf("Expected stack top to be 0x42, got %v", result.Stack[0])
	}

	// Check that memory was used
	if len(result.Memory) == 0 {
		t.Fatalf("Expected memory to be used")
	}
}

func TestEVMExecuteComparison(t *testing.T) {
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// PUSH1 5, PUSH1 3, LT, STOP (3 < 5 should be true = 1)
	bytecode := []byte{0x60, 0x05, 0x60, 0x03, 0x10, 0x00}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if result.Status != compiler.ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", result.Status)
	}

	// TODO: check stack size?
	// if len(result.Stack) != 1 {
	// 	t.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	// }

	// Check if the result is 1 (true)
	expected := uint256.NewInt(1).Bytes32()

	if compiler.FromMachineToBig32Bytes(result.Stack[0]) != expected {
		t.Fatalf("Expected stack top to be 1 (true), got %v", result.Stack[0])
	}
}

func TestEVMExecuteStackOperations(t *testing.T) {
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// PUSH1 0x42, DUP1, STOP (should have two copies of 0x42 on stack)
	bytecode := []byte{0x60, 0x42, 0x80, 0x00}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if result.Status != compiler.ExecutionSuccess {
		t.Fatalf("Expected success status, got %v", result.Status)
	}

	if len(result.Stack) != 2 {
		t.Fatalf("Expected 2 stack items, got %d", len(result.Stack))
	}

	expected := uint256.NewInt(0x42).Bytes32()

	// Both stack items should be 0x42
	if compiler.FromMachineToBig32Bytes(result.Stack[0]) != expected || compiler.FromMachineToBig32Bytes(result.Stack[1]) != expected {
		t.Fatalf("Expected both stack items to be 0x42, got %v and %v", result.Stack[0], result.Stack[1])
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
		comp := compiler.NewEVMCompiler()
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
		comp := compiler.NewEVMCompiler()
		err := comp.CompileAndOptimize(bytecode)
		if err != nil {
			b.Fatalf("Compilation failed: %v", err)
		}
		comp.Dispose()
	}
}

func BenchmarkEVMExecuteSimpleAddition(b *testing.B) {
	bytecode := []byte{0x60, 0x05, 0x60, 0x03, 0x01, 0x00} // PUSH1 5, PUSH1 3, ADD, STOP

	comp := compiler.NewEVMCompiler()
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

	comp := compiler.NewEVMCompiler()
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

	comp := compiler.NewEVMCompiler()
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
