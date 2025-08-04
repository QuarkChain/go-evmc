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
		0x01, // ADD
		0x00, // STOP
	}

	compiler := compiler.NewEVMCompiler()
	defer compiler.Dispose()

	err := compiler.CompileAndOptimize(bytecode)
	if err != nil {
		log.Printf("Example compilation failed: %v", err)
		return
	}

	fmt.Println("Example compilation successful!")

	err = compiler.EmitLLVMIR("example.ll")
	if err != nil {
		log.Printf("Failed to emit example LLVM IR: %v", err)
		return
	}
	fmt.Println("Example LLVM IR written to example.ll")
}
