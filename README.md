# EVM LLVM Compiler in Go

A high-performance EVM (Ethereum Virtual Machine) bytecode compiler that translates EVM bytecode to LLVM IR and native machine code using Go and LLVM.

Note: The code is in experimental stage!

## Features

- **Complete EVM Bytecode Parser**: Supports all standard EVM opcodes including arithmetic, bitwise, comparison, stack, memory, storage, and control flow operations
- **LLVM IR Generation**: Translates EVM bytecode to optimized LLVM intermediate representation
- **Stack Management**: Efficient EVM stack simulation using LLVM values
- **Memory Operations**: Full support for EVM memory operations (MLOAD, MSTORE, MSTORE8)
- **Control Flow**: Proper handling of jumps, conditional jumps, and jump destinations
- **Optimization Passes**: Built-in LLVM optimization pipeline
- **EVMC Integration**: Compatible with EVMC (Ethereum Client-VM Connector API)
- **Multiple Output Formats**: Generate LLVM IR (.ll), object files (.o), and assembly (.asm)

## Architecture

The compiler consists of several key components:

### 1. Bytecode Parser (`ParseBytecode`)
- Parses raw EVM bytecode into structured instructions
- Handles variable-length instructions (PUSH1-PUSH32)
- Maintains program counter and instruction metadata

### 2. LLVM IR Generator (`CompileBytecode`)
- Translates EVM instructions to LLVM IR
- Manages EVM stack using LLVM arrays
- Implements EVM memory model
- Generates control flow graphs for jumps

### 3. Optimization Pipeline (`OptimizeModule`)
- Applies LLVM optimization passes
- Improves performance of generated code
- Eliminates dead code and redundant operations

### 4. Code Generation
- Compiles LLVM IR to native machine code
- Supports multiple target architectures
- Generates object files for linking

### 5. EVMC Interface (`evmc/evmc.go`)
- Provides EVMC-compatible execution engine
- Integrates with Ethereum clients
- Supports host function callbacks

## Supported EVM Opcodes

### Arithmetic Operations
- `ADD`, `MUL`, `SUB`, `DIV`, `SDIV`, `MOD`, `SMOD`
- `ADDMOD`, `MULMOD`, `EXP`, `SIGNEXTEND`

### Comparison & Bitwise
- `LT`, `GT`, `SLT`, `SGT`, `EQ`, `ISZERO`
- `AND`, `OR`, `XOR`, `NOT`, `BYTE`
- `SHL`, `SHR`, `SAR`

### Stack Operations
- `POP`, `PUSH1-PUSH32`, `DUP1-DUP16`, `SWAP1-SWAP16`

### Memory Operations
- `MLOAD`, `MSTORE`, `MSTORE8`, `MSIZE`

### Control Flow
- `JUMP`, `JUMPI`, `JUMPDEST`, `PC`
- `RETURN`, `REVERT`, `STOP`

### Environment Operations
- Context and blockchain state access opcodes
- Host function integration for external calls

## Usage

### Basic Compilation

```go
package main

import (
    "github.com/QuarkChain/go-evmc/compiler"
)

func main() {
    // Create compiler instance
    comp := compiler.NewEVMCompiler()
    defer comp.Dispose()

    // EVM bytecode: PUSH1 5, PUSH1 3, ADD, STOP
    bytecode := []byte{0x60, 0x05, 0x60, 0x03, 0x01, 0x00}

    // Compile and optimize
    err := comp.CompileAndOptimize(bytecode)
    if err != nil {
        panic(err)
    }

    // Generate outputs
    comp.EmitLLVMIR("output.ll")      // LLVM IR
    comp.EmitObjectFile("output.o")    // Object file
}
```

### Command Line Interface

```bash
# Compile EVM bytecode from hex string
go run main.go "6005600301600255"

# This will generate:
# - output.ll (LLVM IR)
# - output.o (Object file)
```

### Running Tests

```bash
# Run all tests
go test -v

# Run benchmarks
go test -bench=.

# Test specific functionality
go test -run TestEVMCompilerBasic
```

## Implementation Details

### Stack Management
The EVM stack is implemented as an LLVM array of 256-bit integers. Stack operations are compiled to efficient LLVM load/store instructions with bounds checking.

### Memory Model
EVM memory is represented as a byte array with dynamic expansion. Memory operations compile to optimized byte-level access patterns.

### Control Flow
Jump instructions are compiled to LLVM branch instructions. The compiler maintains a dispatch loop that handles dynamic jumps efficiently.

### Optimization
The compiler applies standard LLVM optimizations including:
- Dead code elimination
- Constant propagation
- Control flow simplification
- Instruction combining

## Performance

The compiler is designed for high performance:
- **Compilation Speed**: Fast bytecode parsing and IR generation
- **Runtime Performance**: Optimized native code generation
- **Memory Efficiency**: Minimal runtime overhead
- **Scalability**: Handles large contracts efficiently

## Dependencies

- Go 1.23.1+
- LLVM14+ (via tinygo.org/x/go-llvm)
- CGO for EVMC integration

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

This project is licensed under the same terms as the parent research repository.

## Future Enhancements

- [x] Advanced optimization passes
- [ ] Debugger integration
- [ ] Profiling and tracing support
- [ ] Formal verification capabilities
- [x] Gas metering integration
- [ ] Precompiled contracts support
