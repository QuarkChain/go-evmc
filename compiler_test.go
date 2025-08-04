package main

import (
	"testing"

	"github.com/QuarkChain/go-evmc/compiler"
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
