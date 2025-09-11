# EVM LLVM Compiler in Go

A high-performance EVM (Ethereum Virtual Machine) bytecode compiler that translates EVM bytecode to LLVM IR and native machine code using Go and LLVM.

Note: The code is in an experimental stage!

## Features

- **Complete EVM Bytecode Parser**: Supports all standard EVM opcodes, including arithmetic, bitwise, comparison, stack, memory, storage, and control flow operations
- **LLVM IR Generation**: Translates EVM bytecode to optimized LLVM intermediate representation
- **Stack Management**: Efficient EVM stack simulation using LLVM uint256 and Go uint256 exchangeably 
- **Memory Operations**: Full support for EVM memory operations (MLOAD, MSTORE, MSTORE8)
- **Control Flow**: Proper handling of jumps, conditional jumps, and jump destinations
- **Optimization Passes**: Built-in LLVM optimization pipeline
- **Multiple Output Formats**: Generate LLVM IR (.ll), object files (.o), and assembly (.asm)

## Usage

### Running Tests

```bash
# Run all tests
go test -v

# Run benchmarks (use taskset to enhance reproducibility)
taskset -c 0 go test -bench=.

# Test specific functionality
go test -run TestEVMCompilerBasic
```

## Implementation Details

### Stack Management
The EVM stack is implemented as an LLVM array of 256-bit integers, which has the same memory layout as uint256 in Golang (on a little-endian machine). Stack operations are compiled to efficient LLVM load/store instructions with bounds checking.  This is also fully compatible with the Geth opcode interpreter with slight changes.

### Memory Model
EVM memory is represented as a byte array with dynamic expansion using pure host functions.  Enhanced execution with native memory code is under development.

### Control Flow
Jump instructions are compiled to LLVM branch instructions. The compiler will analyze the target of a jump during compile time.  If the static jump target is unknown, it maintains a dispatch loop that handles dynamic jumps efficiently.

### Optimization
The compiler applies standard LLVM optimizations, including:
- Dead code elimination
- Constant propagation
- Control flow simplification
- Instruction combining

## Dependencies

- Go 1.23.1+ with CGO
- LLVM14+ (via tinygo.org/x/go-llvm)

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## Future Enhancements

- [x] Advanced optimization passes
- [ ] Debugger integration
- [ ] Profiling and tracing support
- [ ] Formal verification capabilities
- [x] Gas metering integration
- [x] Precompiled contracts support
- [x] Dynamic memory growth
- [ ] Missing OPCODEs
  - [x] SHL, SHR, SAR support
  - [x] SDIV, SMOD support
  - [x] ADDMOD, MULMOD support
  - [x] SIGNEXTEND supoort
  - [x] EXP support
  - [x] MSTORE8/MLOAD8 support
  - [x] SLOAD/SSTORE support
  - [x] CALL/CREATE support
      - [x] RETURNDATASIZE/RETURNDATACOPY
  - [x] CODESIZE/CODECOPY/EXTCODESIZE/EXCCODECOPY
  - [x] BLOCKHASH/COINBASSE/TIMESTAMP/NUMBER/PREVRANDAO/GASLIMIT/CHAINID/SELFBALANCE/BASEFEE
  - [x] LOG0/LOG1/LOG2/LOG3/LOG4
  - [x] ALLDATALOAD/CALLDATASIZE/CALLDATACOPY
  - [x] ADDRESS/BALANCE/ORIGIN/CALLER/CALLVALUE/GASPRICE
- [ ] Fork support
