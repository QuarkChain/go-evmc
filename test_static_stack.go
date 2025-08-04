package main

import (
	"fmt"
	"log"

	"github.com/QuarkChain/go-evmc/compiler"
)

func main() {
	fmt.Println("🧮 Testing Static Stack Analysis EVM Compiler")
	fmt.Println("==============================================\n")

	testBasicStackOperations()
	testComplexStackManipulation()
	testStackDepthValidation()
	testDupSwapOperations()
	
	fmt.Println("✅ All static stack tests passed!")
}

func testBasicStackOperations() {
	fmt.Println("Test 1: Basic Stack Operations")
	
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// Test basic arithmetic with known stack depths
	// PUSH1 10, PUSH1 5, ADD, PUSH1 3, MUL, STOP
	// Stack depth: 0 -> 1 -> 2 -> 1 -> 2 -> 1
	bytecode := []byte{
		0x60, 0x0A, // PUSH1 10 (depth: 0->1)
		0x60, 0x05, // PUSH1 5  (depth: 1->2)
		0x01,       // ADD      (depth: 2->1, result: 15)
		0x60, 0x03, // PUSH1 3  (depth: 1->2)
		0x02,       // MUL      (depth: 2->1, result: 45)
		0x00,       // STOP
	}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		log.Fatalf("Basic stack operations failed: %v", err)
	}

	if len(result.Stack) != 1 {
		log.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	}

	expected := [32]byte{}
	expected[31] = 45
	
	if result.Stack[0] != expected {
		log.Fatalf("Expected result 45, got %v", result.Stack[0])
	}

	fmt.Println("✅ Basic stack operations work with static analysis")
}

func testComplexStackManipulation() {
	fmt.Println("Test 2: Complex Stack Manipulation")
	
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// Complex stack operations testing static depth tracking
	// PUSH1 1, PUSH1 2, PUSH1 3, PUSH1 4, SUB, ADD, MUL, STOP
	// Stack progression: [1] -> [1,2] -> [1,2,3] -> [1,2,3,4] -> [1,2,-1] -> [1,1] -> [1]
	bytecode := []byte{
		0x60, 0x01, // PUSH1 1  (stack: [1])
		0x60, 0x02, // PUSH1 2  (stack: [1,2]) 
		0x60, 0x03, // PUSH1 3  (stack: [1,2,3])
		0x60, 0x04, // PUSH1 4  (stack: [1,2,3,4])
		0x03,       // SUB      (stack: [1,2,-1]) 3-4 = -1 (underflow to large number)
		0x01,       // ADD      (stack: [1,1]) 2+(-1) = 1 
		0x02,       // MUL      (stack: [1]) 1*1 = 1
		0x00,       // STOP
	}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		log.Fatalf("Complex stack manipulation failed: %v", err)
	}

	if len(result.Stack) != 1 {
		log.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	}

	// The result should be 1 * 1 = 1
	expected := [32]byte{}
	expected[31] = 1
	
	if result.Stack[0] != expected {
		log.Fatalf("Expected result 1, got %v", result.Stack[0])
	}

	fmt.Println("✅ Complex stack manipulation works")
}

func testStackDepthValidation() {
	fmt.Println("Test 3: Stack Depth Validation")
	
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// Test that compiler detects stack underflow at compile time
	// PUSH1 5, ADD (tries to add with only 1 item on stack)
	bytecode := []byte{
		0x60, 0x05, // PUSH1 5
		0x01,       // ADD (should fail - need 2 items, have 1)
		0x00,       // STOP
	}

	// This should fail during compilation due to stack analysis
	err := comp.CompileAndOptimize(bytecode)
	if err == nil {
		log.Fatalf("Expected compilation to fail due to stack underflow, but it succeeded")
	}

	fmt.Printf("✅ Stack underflow correctly detected: %v\n", err)
}

func testDupSwapOperations() {
	fmt.Println("Test 4: DUP and SWAP Operations")
	
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// Test DUP and SWAP with static stack analysis
	// PUSH1 10, PUSH1 20, DUP2, SWAP1, SUB, STOP
	// Stack: [10] -> [10,20] -> [10,20,10] -> [10,10,20] -> [10,-10] (20-10=10, but 10-20=-10)
	bytecode := []byte{
		0x60, 0x0A, // PUSH1 10  (stack: [10])
		0x60, 0x14, // PUSH1 20  (stack: [10,20])
		0x81,       // DUP2      (stack: [10,20,10]) - duplicate 2nd item from top
		0x90,       // SWAP1     (stack: [10,10,20]) - swap top 2 items
		0x03,       // SUB       (stack: [10,-10])  - 10-20 in 256-bit arithmetic
		0x50,       // POP       (stack: [10])      - remove top item
		0x00,       // STOP
	}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		log.Fatalf("DUP/SWAP operations failed: %v", err)
	}

	if len(result.Stack) != 1 {
		log.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	}

	// Should have 10 remaining
	expected := [32]byte{}
	expected[31] = 10
	
	if result.Stack[0] != expected {
		log.Fatalf("Expected result 10, got %v", result.Stack[0])
	}

	fmt.Println("✅ DUP and SWAP operations work with static analysis")
	
	// Test more complex DUP/SWAP
	testAdvancedDupSwap()
}

func testAdvancedDupSwap() {
	fmt.Println("Test 4b: Advanced DUP/SWAP Operations")
	
	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// Test multiple DUP and SWAP operations
	// PUSH1 1, PUSH1 2, PUSH1 3, DUP3, SWAP2, ADD, STOP
	bytecode := []byte{
		0x60, 0x01, // PUSH1 1   (stack: [1])
		0x60, 0x02, // PUSH1 2   (stack: [1,2])
		0x60, 0x03, // PUSH1 3   (stack: [1,2,3])
		0x82,       // DUP3      (stack: [1,2,3,1]) - duplicate 3rd from top
		0x91,       // SWAP2     (stack: [1,1,3,2]) - swap top with 3rd
		0x01,       // ADD       (stack: [1,1,5])   - 3+2=5
		0x01,       // ADD       (stack: [1,6])     - 1+5=6  
		0x01,       // ADD       (stack: [7])       - 1+6=7
		0x00,       // STOP
	}

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		log.Fatalf("Advanced DUP/SWAP operations failed: %v", err)
	}

	if len(result.Stack) != 1 {
		log.Fatalf("Expected 1 stack item, got %d", len(result.Stack))
	}

	expected := [32]byte{}
	expected[31] = 7
	
	if result.Stack[0] != expected {
		log.Fatalf("Expected result 7, got %v", result.Stack[0])
	}

	fmt.Println("✅ Advanced DUP/SWAP operations work correctly")
}