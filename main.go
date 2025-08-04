package main

import (
	"fmt"
	"log"
	"os"

	"github.com/QuarkChain/go-evmc/compiler"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go-evmc <bytecode_hex>")
		os.Exit(1)
	}

	bytecodeHex := os.Args[1]
	bytecode, err := hexToBytes(bytecodeHex)
	if err != nil {
		log.Fatalf("Failed to parse bytecode: %v", err)
	}

	fmt.Printf("Compiling EVM bytecode (%d bytes)...\n", len(bytecode))

	compiler := compiler.NewEVMCompiler()
	defer compiler.Dispose()

	err = compiler.CompileAndOptimize(bytecode)
	if err != nil {
		log.Fatalf("Compilation failed: %v", err)
	}

	fmt.Println("Compilation successful!")

	err = compiler.EmitLLVMIR("output.ll")
	if err != nil {
		log.Fatalf("Failed to emit LLVM IR: %v", err)
	}
	fmt.Println("LLVM IR written to output.ll")

	err = compiler.EmitObjectFile("output.o")
	if err != nil {
		log.Fatalf("Failed to emit object file: %v", err)
	}
	fmt.Println("Object file written to output.o")

	example()
}

func hexToBytes(hex string) ([]byte, error) {
	if len(hex)%2 != 0 {
		hex = "0" + hex
	}

	bytes := make([]byte, len(hex)/2)
	for i := 0; i < len(hex); i += 2 {
		var b byte
		for j := 0; j < 2; j++ {
			c := hex[i+j]
			var val byte
			if c >= '0' && c <= '9' {
				val = c - '0'
			} else if c >= 'a' && c <= 'f' {
				val = c - 'a' + 10
			} else if c >= 'A' && c <= 'F' {
				val = c - 'A' + 10
			} else {
				return nil, fmt.Errorf("invalid hex character: %c", c)
			}
			b = (b << 4) | val
		}
		bytes[i/2] = b
	}
	return bytes, nil
}

func example() {
	fmt.Println("\n=== Example: Simple Addition ===")

	bytecode := []byte{
		0x60, 0x05, // PUSH1 5
		0x60, 0x03, // PUSH1 3
		0x01,       // ADD
		0x00,       // STOP
	}

	comp := compiler.NewEVMCompiler()
	defer comp.Dispose()

	// Compile the bytecode
	err := comp.CompileAndOptimize(bytecode)
	if err != nil {
		log.Printf("Example compilation failed: %v", err)
		return
	}
	fmt.Println("Example compilation successful!")

	// Execute the compiled code
	fmt.Println("Executing compiled EVM code...")
	result, err := comp.ExecuteCompiled(bytecode)
	if err != nil {
		log.Printf("Execution failed: %v", err)
		return
	}

	fmt.Printf("Execution completed successfully!\n")
	fmt.Printf("Status: %v\n", result.Status)
	fmt.Printf("Stack depth: %d\n", len(result.Stack))
	
	if len(result.Stack) > 0 {
		fmt.Printf("Top stack value: ")
		for i, b := range result.Stack[len(result.Stack)-1] {
			if i < 4 { // Print first 4 bytes
				fmt.Printf("%02x", b)
			}
		}
		fmt.Printf("...\n")
	}

	if len(result.Memory) > 0 {
		fmt.Printf("Memory used: %d bytes\n", len(result.Memory))
	} else {
		fmt.Printf("No memory used\n")
	}

	// Also emit LLVM IR for inspection
	err = comp.EmitLLVMIR("example.ll")
	if err != nil {
		log.Printf("Failed to emit example LLVM IR: %v", err)
		return
	}
	fmt.Println("Example LLVM IR written to example.ll")

	// Run more complex examples
	runMoreExamples()
}

func runMoreExamples() {
	fmt.Println("\n=== More Examples ===")

	examples := []struct {
		name     string
		bytecode []byte
		desc     string
	}{
		{
			name: "Multiplication",
			bytecode: []byte{
				0x60, 0x07, // PUSH1 7
				0x60, 0x06, // PUSH1 6  
				0x02,       // MUL
				0x00,       // STOP
			},
			desc: "7 * 6 = 42",
		},
		{
			name: "Memory Store/Load",
			bytecode: []byte{
				0x60, 0x42, // PUSH1 0x42 (value)
				0x60, 0x00, // PUSH1 0x00 (offset)
				0x52,       // MSTORE
				0x60, 0x00, // PUSH1 0x00 (offset)  
				0x51,       // MLOAD
				0x00,       // STOP
			},
			desc: "Store 0x42 at memory[0], then load it back",
		},
		{
			name: "Conditional Jump",
			bytecode: []byte{
				0x60, 0x01, // PUSH1 1 (condition)
				0x60, 0x08, // PUSH1 8 (jump target)
				0x57,       // JUMPI
				0x60, 0x99, // PUSH1 0x99 (should be skipped)
				0x00,       // STOP (should be skipped)
				0x5B,       // JUMPDEST (PC 8)
				0x60, 0x55, // PUSH1 0x55 
				0x00,       // STOP
			},
			desc: "Conditional jump to push 0x55 instead of 0x99",
		},
	}

	for _, example := range examples {
		fmt.Printf("\n--- %s: %s ---\n", example.name, example.desc)
		
		comp := compiler.NewEVMCompiler()
		result, err := comp.ExecuteCompiled(example.bytecode)
		comp.Dispose()

		if err != nil {
			fmt.Printf("❌ Execution failed: %v\n", err)
			continue
		}

		fmt.Printf("✅ Execution successful\n")
		fmt.Printf("   Stack depth: %d\n", len(result.Stack))
		
		if len(result.Stack) > 0 {
			fmt.Printf("   Top stack: ")
			topValue := result.Stack[len(result.Stack)-1]
			// Find the significant part of the 256-bit value
			significant := false
			for i := 0; i < 32; i++ {
				if topValue[i] != 0 || significant {
					fmt.Printf("%02x", topValue[i])
					significant = true
				}
			}
			if !significant {
				fmt.Printf("00")
			}
			fmt.Printf("\n")
		}

		if len(result.Memory) > 0 {
			fmt.Printf("   Memory used: %d bytes\n", len(result.Memory))
			if len(result.Memory) <= 32 {
				fmt.Printf("   Memory content: ")
				for i, b := range result.Memory {
					if i < 16 { // Show first 16 bytes
						fmt.Printf("%02x ", b)
					}
				}
				fmt.Printf("\n")
			}
		}
	}
}
