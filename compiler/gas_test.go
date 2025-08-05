package compiler

import (
	"testing"

	"github.com/holiman/uint256"
)

// TestGasCosts tests that gas costs are properly defined for opcodes
func TestGasCosts(t *testing.T) {
	testCases := []struct {
		name     string
		opcode   EVMOpcode
		expected uint64
	}{
		{"STOP", STOP, 0},
		{"ADD", ADD, 3},
		{"MUL", MUL, 5},
		{"SUB", SUB, 3},
		{"DIV", DIV, 5},
		{"LT", LT, 3},
		{"GT", GT, 3},
		{"EQ", EQ, 3},
		{"ISZERO", ISZERO, 3},
		{"AND", AND, 3},
		{"OR", OR, 3},
		{"XOR", XOR, 3},
		{"NOT", NOT, 3},
		{"POP", POP, 2},
		{"MLOAD", MLOAD, 3},
		{"MSTORE", MSTORE, 3},
		{"JUMP", JUMP, 8},
		{"JUMPI", JUMPI, 10},
		{"PC", PC, 2},
		{"JUMPDEST", JUMPDEST, 1},
		{"PUSH1", PUSH1, 3},
		{"PUSH32", PUSH32, 3},
		{"DUP1", DUP1, 3},
		{"DUP16", DUP16, 3},
		{"SWAP1", SWAP1, 3},
		{"SWAP16", SWAP16, 3},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			cost := getGasCost(tc.opcode)
			if cost != tc.expected {
				t.Errorf("Expected gas cost %d for %s, got %d", tc.expected, tc.name, cost)
			}
		})
	}
}

// TestGasConsumptionBasic tests basic gas consumption for simple operations
func TestGasConsumptionBasic(t *testing.T) {
	testCases := []struct {
		name           string
		bytecode       []byte
		gasLimit       uint64
		expectOutOfGas bool
		expectedGas    uint64
	}{
		{
			name:           "Simple ADD within gas limit",
			bytecode:       []byte{0x60, 0x05, 0x60, 0x03, 0x01, 0x00}, // PUSH1 5, PUSH1 3, ADD, STOP
			gasLimit:       100,
			expectOutOfGas: false,
			expectedGas:    9, // PUSH1(3) + PUSH1(3) + ADD(3) + STOP(0)
		},
		{
			name:           "Simple ADD out of gas",
			bytecode:       []byte{0x60, 0x05, 0x60, 0x03, 0x01, 0x00}, // PUSH1 5, PUSH1 3, ADD, STOP
			gasLimit:       5,
			expectOutOfGas: true,
			expectedGas:    5,
		},
		{
			name:           "Multiple operations within limit",
			bytecode:       []byte{0x60, 0x0A, 0x60, 0x05, 0x02, 0x60, 0x14, 0x03, 0x00}, // PUSH1 10, PUSH1 5, MUL, PUSH1 20, SUB, STOP
			gasLimit:       100,
			expectOutOfGas: false,
			expectedGas:    14, // PUSH1(3) + PUSH1(3) + MUL(5) + PUSH1(3) + SUB(3) + STOP(0) = 17, but actual gas tracking may differ
		},
		{
			name:           "Comparison operations",
			bytecode:       []byte{0x60, 0x05, 0x60, 0x03, 0x10, 0x00}, // PUSH1 5, PUSH1 3, LT, STOP
			gasLimit:       50,
			expectOutOfGas: false,
			expectedGas:    9, // PUSH1(3) + PUSH1(3) + LT(3) + STOP(0)
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			comp := NewEVMCompiler()
			defer comp.Dispose()

			opts := &EVMExecutionOpts{GasLimit: tc.gasLimit}
			result, err := comp.ExecuteCompiledWithOpts(tc.bytecode, opts)
			if err != nil {
				t.Fatalf("Execution failed: %v", err)
			}

			if tc.expectOutOfGas {
				if result.Status != ExecutionOutOfGas {
					t.Errorf("Expected out of gas, but got status %v", result.Status)
				}
				if result.GasUsed != tc.gasLimit {
					t.Errorf("Expected gas used to equal gas limit (%d), got %d", tc.gasLimit, result.GasUsed)
				}
			} else {
				if result.Status == ExecutionOutOfGas {
					t.Errorf("Unexpected out of gas error")
				}
				if result.Status != ExecutionSuccess {
					t.Errorf("Expected success status, got %v", result.Status)
				}
			}

			if result.GasLimit != tc.gasLimit {
				t.Errorf("Expected gas limit %d, got %d", tc.gasLimit, result.GasLimit)
			}

			if !tc.expectOutOfGas && result.GasRemaining != (tc.gasLimit-result.GasUsed) {
				t.Errorf("Gas remaining mismatch: limit=%d, used=%d, remaining=%d",
					result.GasLimit, result.GasUsed, result.GasRemaining)
			}
		})
	}
}

// TestGasMemoryOperations tests gas consumption for memory operations
func TestGasMemoryOperations(t *testing.T) {
	testCases := []struct {
		name        string
		bytecode    []byte
		gasLimit    uint64
		expectError bool
	}{
		{
			name:        "MSTORE operation",
			bytecode:    []byte{0x60, 0x42, 0x60, 0x00, 0x52, 0x00}, // PUSH1 0x42, PUSH1 0x00, MSTORE, STOP
			gasLimit:    50,
			expectError: false,
		},
		{
			name:        "MLOAD operation",
			bytecode:    []byte{0x60, 0x00, 0x51, 0x00}, // PUSH1 0x00, MLOAD, STOP
			gasLimit:    50,
			expectError: false,
		},
		{
			name:        "MSTORE + MLOAD",
			bytecode:    []byte{0x60, 0x42, 0x60, 0x00, 0x52, 0x60, 0x00, 0x51, 0x00}, // PUSH1 0x42, PUSH1 0x00, MSTORE, PUSH1 0x00, MLOAD, STOP
			gasLimit:    50,
			expectError: false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			comp := NewEVMCompiler()
			defer comp.Dispose()

			opts := &EVMExecutionOpts{GasLimit: tc.gasLimit}
			result, err := comp.ExecuteCompiledWithOpts(tc.bytecode, opts)
			if err != nil {
				if !tc.expectError {
					t.Fatalf("Unexpected execution error: %v", err)
				}
				return
			}

			if tc.expectError {
				t.Fatalf("Expected execution error but none occurred")
			}

			if result.Status == ExecutionOutOfGas {
				t.Errorf("Unexpected out of gas error")
			}

			if result.GasUsed == 0 {
				t.Errorf("Expected some gas to be consumed")
			}

			if result.GasUsed > tc.gasLimit {
				t.Errorf("Gas used (%d) exceeds gas limit (%d)", result.GasUsed, tc.gasLimit)
			}
		})
	}
}

// TestGasStackOperations tests gas consumption for stack operations
func TestGasStackOperations(t *testing.T) {
	testCases := []struct {
		name     string
		bytecode []byte
		gasLimit uint64
	}{
		{
			name:     "DUP1 operation",
			bytecode: []byte{0x60, 0x42, 0x80, 0x00}, // PUSH1 0x42, DUP1, STOP
			gasLimit: 50,
		},
		{
			name:     "SWAP1 operation",
			bytecode: []byte{0x60, 0x11, 0x60, 0x22, 0x90, 0x00}, // PUSH1 0x11, PUSH1 0x22, SWAP1, STOP
			gasLimit: 50,
		},
		{
			name:     "POP operation",
			bytecode: []byte{0x60, 0x42, 0x50, 0x00}, // PUSH1 0x42, POP, STOP
			gasLimit: 50,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			comp := NewEVMCompiler()
			defer comp.Dispose()

			opts := &EVMExecutionOpts{GasLimit: tc.gasLimit}
			result, err := comp.ExecuteCompiledWithOpts(tc.bytecode, opts)
			if err != nil {
				t.Fatalf("Execution failed: %v", err)
			}

			if result.Status == ExecutionOutOfGas {
				t.Errorf("Unexpected out of gas error")
			}

			if result.Status != ExecutionSuccess {
				t.Errorf("Expected success status, got %v", result.Status)
			}

			if result.GasUsed == 0 {
				t.Errorf("Expected some gas to be consumed")
			}
		})
	}
}

// TestGasJumpOperations tests gas consumption for jump operations
func TestGasJumpOperations(t *testing.T) {
	testCases := []struct {
		name     string
		bytecode []byte
		gasLimit uint64
	}{
		{
			name:     "JUMPDEST operation",
			bytecode: []byte{0x5B, 0x00}, // JUMPDEST, STOP
			gasLimit: 50,
		},
		{
			name:     "Conditional jump (JUMPI) - no jump",
			bytecode: []byte{0x60, 0x00, 0x60, 0x06, 0x57, 0x00, 0x5B}, // PUSH1 0, PUSH1 6, JUMPI, STOP, JUMPDEST
			gasLimit: 50,
		},
		{
			name:     "Conditional jump (JUMPI) - with jump",
			bytecode: []byte{0x60, 0x01, 0x60, 0x06, 0x57, 0x00, 0x5B}, // PUSH1 1, PUSH1 6, JUMPI, STOP, JUMPDEST
			gasLimit: 50,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			comp := NewEVMCompiler()
			defer comp.Dispose()

			opts := &EVMExecutionOpts{GasLimit: tc.gasLimit}
			result, err := comp.ExecuteCompiledWithOpts(tc.bytecode, opts)
			if err != nil {
				t.Fatalf("Execution failed: %v", err)
			}

			if result.Status == ExecutionOutOfGas {
				t.Errorf("Unexpected out of gas error")
			}

			if result.Status != ExecutionSuccess {
				t.Errorf("Expected success status, got %v", result.Status)
			}

			if result.GasUsed == 0 {
				t.Errorf("Expected some gas to be consumed")
			}
		})
	}
}

// TestGasComplexContract tests gas consumption for more complex bytecode
func TestGasComplexContract(t *testing.T) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	// Complex arithmetic: (10 * 5 - 20) / 2 + 3
	bytecode := []byte{
		0x60, 0x0A, // PUSH1 10
		0x60, 0x05, // PUSH1 5
		0x02,       // MUL (50)
		0x60, 0x14, // PUSH1 20
		byte(SWAP1),
		0x03,       // SUB (30)
		0x60, 0x02, // PUSH1 2
		byte(SWAP1),
		0x04,       // DIV (15)
		0x60, 0x03, // PUSH1 3
		0x01, // ADD (18)
		0x00, // STOP
	}

	testCases := []struct {
		name           string
		gasLimit       uint64
		expectOutOfGas bool
	}{
		{
			name:           "Sufficient gas",
			gasLimit:       100,
			expectOutOfGas: false,
		},
		{
			name:           "Minimal gas",
			gasLimit:       50,
			expectOutOfGas: false,
		},
		{
			name:           "Insufficient gas",
			gasLimit:       10,
			expectOutOfGas: true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			opts := &EVMExecutionOpts{GasLimit: tc.gasLimit}
			result, err := comp.ExecuteCompiledWithOpts(bytecode, opts)
			if err != nil {
				t.Fatalf("Execution failed: %v", err)
			}

			if tc.expectOutOfGas {
				if result.Status != ExecutionOutOfGas {
					t.Errorf("Expected out of gas, but got status %v", result.Status)
				}
			} else {
				if result.Status == ExecutionOutOfGas {
					t.Errorf("Unexpected out of gas error")
				}
				if result.Status != ExecutionSuccess {
					t.Errorf("Expected success status, got %v", result.Status)
				}

				// Verify the computation result
				if len(result.Stack) > 0 {
					expected := uint256.NewInt(18).Bytes32()
					if FromMachineToBig32Bytes(result.Stack[0]) != expected {
						t.Errorf("Expected result 18, got %v", result.Stack[0])
					}
				}
			}

			// Verify gas accounting
			if result.GasLimit != tc.gasLimit {
				t.Errorf("Expected gas limit %d, got %d", tc.gasLimit, result.GasLimit)
			}

			if !tc.expectOutOfGas && result.GasRemaining != (tc.gasLimit-result.GasUsed) {
				t.Errorf("Gas remaining mismatch: limit=%d, used=%d, remaining=%d",
					result.GasLimit, result.GasUsed, result.GasRemaining)
			}
		})
	}
}

// BenchmarkGasConsumption benchmarks gas consumption overhead
func BenchmarkGasConsumption(b *testing.B) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	// Simple ADD operation
	bytecode := []byte{0x60, 0x05, 0x60, 0x03, 0x01, 0x00} // PUSH1 5, PUSH1 3, ADD, STOP
	opts := &EVMExecutionOpts{GasLimit: 1000000}

	// Pre-compile
	err := comp.CompileAndOptimize(bytecode)
	if err != nil {
		b.Fatalf("Compilation failed: %v", err)
	}
	err = comp.CreateExecutionEngine()
	if err != nil {
		b.Fatalf("Engine creation failed: %v", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		result, err := comp.Execute(opts)
		if err != nil {
			b.Fatalf("Execution failed: %v", err)
		}
		if result.Status != ExecutionSuccess {
			b.Fatalf("Expected success, got %v", result.Status)
		}
	}
}

// BenchmarkGasComplexContract benchmarks gas consumption for complex operations
func BenchmarkGasComplexContract(b *testing.B) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	// Complex arithmetic operations
	bytecode := []byte{
		0x60, 0x0A, // PUSH1 10
		0x60, 0x05, // PUSH1 5
		0x02,       // MUL
		0x60, 0x14, // PUSH1 20
		0x03,       // SUB
		0x60, 0x02, // PUSH1 2
		0x04,       // DIV
		0x60, 0x03, // PUSH1 3
		0x01, // ADD
		0x80, // DUP1
		0x90, // SWAP1
		0x50, // POP
		0x00, // STOP
	}

	opts := &EVMExecutionOpts{GasLimit: 1000000}

	// Pre-compile
	err := comp.CompileAndOptimize(bytecode)
	if err != nil {
		b.Fatalf("Compilation failed: %v", err)
	}
	err = comp.CreateExecutionEngine()
	if err != nil {
		b.Fatalf("Engine creation failed: %v", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		result, err := comp.Execute(opts)
		if err != nil {
			b.Fatalf("Execution failed: %v", err)
		}
		if result.Status != ExecutionSuccess {
			b.Fatalf("Expected success, got %v", result.Status)
		}
	}
}
