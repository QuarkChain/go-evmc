package main

import (
	"fmt"
	"log"

	"github.com/QuarkChain/go-evmc/compiler"
)

func main() {
	fmt.Println("🧪 Testing Static PC Analysis EVM Compiler")
	fmt.Println("==========================================\n")

	testStaticPCCompilation()
	testStaticJumps()
	testStaticPCOpcode()
	
	fmt.Println("✅ All static PC tests passed!")
}

func testStaticPCCompilation() {
	fmt.Println("Test 1: Basic Static Compilation")
	
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// Simple arithmetic: PUSH1 5, PUSH1 3, ADD, STOP
	bytecode := []byte{0x60, 0x05, 0x60, 0x03, 0x01, 0x00}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		log.Fatalf("Static compilation failed: %v", err)
	}

	if len(result.Stack) != 1 {
		log.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	}

	// Check result is 8 (5 + 3)
	expected := [32]byte{}
	expected[31] = 8
	
	if result.Stack[0] != expected {
		log.Fatalf("Expected result 8, got %v", result.Stack[0])
	}

	fmt.Println("✅ Basic static compilation works")
}

func testStaticJumps() {
	fmt.Println("Test 2: Static Jump Analysis")
	
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// Conditional jump: PUSH1 1, PUSH1 8, JUMPI, PUSH1 99, STOP, JUMPDEST, PUSH1 55, STOP
	bytecode := []byte{
		0x60, 0x01, // PUSH1 1 (condition)
		0x60, 0x08, // PUSH1 8 (jump target - PC of JUMPDEST)
		0x57,       // JUMPI (conditional jump)
		0x60, 0x63, // PUSH1 99 (should be skipped)
		0x00,       // STOP (should be skipped)
		0x5B,       // JUMPDEST (PC 8)
		0x60, 0x37, // PUSH1 55
		0x00,       // STOP
	}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		log.Fatalf("Static jump compilation failed: %v", err)
	}

	if len(result.Stack) != 1 {
		log.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	}

	// Should have 55 (0x37), not 99 (0x63)
	expected := [32]byte{}
	expected[31] = 0x37
	
	if result.Stack[0] != expected {
		log.Fatalf("Expected result 0x37 (55), got %v", result.Stack[0])
	}

	fmt.Println("✅ Static jump analysis works")
}

func testStaticPCOpcode() {
	fmt.Println("Test 3: Static PC Opcode")
	
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// PC opcode test: PUSH1 5, PC, ADD, STOP
	// PC should return 3 (the PC where PC instruction is located)
	bytecode := []byte{
		0x60, 0x05, // PUSH1 5 (PC 0-1)
		0x58,       // PC (PC 2) - should push 2
		0x01,       // ADD (PC 3) - should compute 5 + 2 = 7
		0x00,       // STOP (PC 4)
	}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		log.Fatalf("Static PC opcode compilation failed: %v", err)
	}

	if len(result.Stack) != 1 {
		log.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	}

	// Should have 7 (5 + 2, where 2 is the PC of the PC instruction)
	expected := [32]byte{}
	expected[31] = 7
	
	if result.Stack[0] != expected {
		log.Fatalf("Expected result 7 (5 + PC=2), got %v", result.Stack[0])
	}

	fmt.Println("✅ Static PC opcode works correctly")
}