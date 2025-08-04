package main

import (
	"fmt"
	"log"
	"strings"

	"github.com/QuarkChain/go-evmc/compiler"
)

func main() {
	fmt.Println("🚀 EVM LLVM Compiler Demo")
	fmt.Println("========================\n")

	// Demo 1: Simple arithmetic
	fmt.Println("Demo 1: Simple Arithmetic (5 + 3)")
	demo1()

	fmt.Println("\n" + strings.Repeat("-", 50) + "\n")

	// Demo 2: Complex arithmetic with multiple operations
	fmt.Println("Demo 2: Complex Arithmetic ((10 * 5) - 20) / 2 + 3")
	demo2()

	fmt.Println("\n" + strings.Repeat("-", 50) + "\n")

	// Demo 3: Memory operations
	fmt.Println("Demo 3: Memory Operations")
	demo3()

	fmt.Println("\n" + strings.Repeat("-", 50) + "\n")

	// Demo 4: Stack operations (DUP, SWAP)
	fmt.Println("Demo 4: Stack Operations")
	demo4()

	fmt.Println("\n" + strings.Repeat("-", 50) + "\n")

	// Demo 5: Comparison and logic
	fmt.Println("Demo 5: Comparison and Logic")
	demo5()

	fmt.Println("\n🎉 All demos completed successfully!")
}

func demo1() {
	bytecode := []byte{
		0x60, 0x05, // PUSH1 5
		0x60, 0x03, // PUSH1 3
		0x01,       // ADD
		0x00,       // STOP
	}

	result := executeAndShow("Simple Addition", bytecode)
	if len(result.Stack) > 0 {
		fmt.Printf("   Result: %d (expected: 8)\n", getStackValue(result.Stack[len(result.Stack)-1]))
	}
}

func demo2() {
	bytecode := []byte{
		0x60, 0x0A, // PUSH1 10
		0x60, 0x05, // PUSH1 5
		0x02,       // MUL (10 * 5 = 50)
		0x60, 0x14, // PUSH1 20
		0x03,       // SUB (50 - 20 = 30)
		0x60, 0x02, // PUSH1 2
		0x04,       // DIV (30 / 2 = 15)
		0x60, 0x03, // PUSH1 3
		0x01,       // ADD (15 + 3 = 18)
		0x00,       // STOP
	}

	result := executeAndShow("Complex Arithmetic", bytecode)
	if len(result.Stack) > 0 {
		fmt.Printf("   Result: %d (expected: 18)\n", getStackValue(result.Stack[len(result.Stack)-1]))
	}
}

func demo3() {
	bytecode := []byte{
		0x60, 0x42, // PUSH1 0x42 (value to store)
		0x60, 0x10, // PUSH1 0x10 (memory offset)
		0x52,       // MSTORE (store 0x42 at memory[0x10])
		0x60, 0x10, // PUSH1 0x10 (memory offset)
		0x51,       // MLOAD (load from memory[0x10])
		0x60, 0x20, // PUSH1 0x20 (another offset)
		0x52,       // MSTORE (store at memory[0x20])
		0x60, 0x20, // PUSH1 0x20
		0x51,       // MLOAD (load from memory[0x20])
		0x00,       // STOP
	}

	result := executeAndShow("Memory Operations", bytecode)
	if len(result.Stack) > 0 {
		fmt.Printf("   Final stack top: 0x%x (expected: 0x42)\n", getStackValue(result.Stack[len(result.Stack)-1]))
	}
	fmt.Printf("   Memory used: %d bytes\n", len(result.Memory))
}

func demo4() {
	bytecode := []byte{
		0x60, 0x11, // PUSH1 0x11
		0x60, 0x22, // PUSH1 0x22
		0x60, 0x33, // PUSH1 0x33
		0x82,       // DUP3 (duplicate 3rd item: 0x11)
		0x91,       // SWAP2 (swap top with 3rd: stack becomes [0x11, 0x22, 0x11, 0x33])
		0x00,       // STOP
	}

	result := executeAndShow("Stack Operations", bytecode)
	fmt.Printf("   Final stack depth: %d\n", len(result.Stack))
	if len(result.Stack) >= 2 {
		fmt.Printf("   Stack[0]: 0x%x\n", getStackValue(result.Stack[0]))
		fmt.Printf("   Stack[1]: 0x%x\n", getStackValue(result.Stack[1]))
	}
}

func demo5() {
	bytecode := []byte{
		0x60, 0x0A, // PUSH1 10
		0x60, 0x05, // PUSH1 5
		0x10,       // LT (5 < 10 = true = 1)
		0x60, 0xFF, // PUSH1 0xFF
		0x60, 0x00, // PUSH1 0x00
		0x14,       // EQ (0xFF == 0x00 = false = 0)
		0x16,       // AND (true AND false = false = 0)
		0x15,       // ISZERO (0 == 0 = true = 1)
		0x00,       // STOP
	}

	result := executeAndShow("Comparison and Logic", bytecode)
	if len(result.Stack) > 0 {
		fmt.Printf("   Final result: %d (expected: 1 = true)\n", getStackValue(result.Stack[len(result.Stack)-1]))
	}
}

func executeAndShow(name string, bytecode []byte) *compiler.EVMExecutionResult {
	fmt.Printf("Executing: %s\n", name)
	fmt.Printf("Bytecode: ")
	for i, b := range bytecode {
		if i > 0 {
			fmt.Printf(" ")
		}
		fmt.Printf("%02x", b)
	}
	fmt.Printf("\n")

	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		log.Printf("❌ Execution failed: %v", err)
		return nil
	}

	fmt.Printf("✅ Execution successful\n")
	fmt.Printf("   Status: %v\n", result.Status)
	fmt.Printf("   Stack depth: %d\n", len(result.Stack))

	return result
}

func getStackValue(stackItem [32]byte) uint64 {
	// Convert 32-byte array to uint64 (assuming little-endian storage)
	var value uint64
	for i := 31; i >= 24; i-- { // Take last 8 bytes
		value = (value << 8) | uint64(stackItem[i])
	}
	return value
}

