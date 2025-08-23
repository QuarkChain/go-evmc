package compiler

import (
	"bytes"
	"testing"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/common/hexutil"
	"github.com/ethereum/go-ethereum/core/state"
	"github.com/ethereum/go-ethereum/core/tracing"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/ethereum/go-ethereum/core/vm/runtime"
	"github.com/ethereum/go-ethereum/params"
	"github.com/holiman/uint256"
)

// Helper function to create a 32-byte value (machine format) from a uint64
func uint64ToBytes32(val uint64) [32]byte {
	// Store in machine format
	bs := uint256.NewInt(val).Bytes32()
	if IsMachineBigEndian() {
		return bs
	}
	return Reverse32Bytes(bs)
}

func uint64ToBigEndianBytes32(val uint64) [32]byte {
	return uint256.NewInt(val).Bytes32()
}

// Helper function to convert [32]byte (machine format) back to uint64 for display
func bytes32ToUint64(b [32]byte) uint64 {
	var v uint256.Int
	if IsMachineBigEndian() {
		v.SetBytes(b[:])
	} else {
		v.SetBytes(Reverse(b[:]))
	}
	return v.Uint64()
}

// Helper function to decode a hex string (big-endian) and return its
// little-endian representation as a fixed-length [32]byte array
func hexToLittleEndianBytes32(val string) [32]byte {
	b := hexutil.MustDecode(val)
	// convert from big-endian to little-endian
	b = Reverse(b)
	var result [32]byte
	copy(result[:], b)
	return result
}

func getExpectedStatus(status ExecutionStatus) *ExecutionStatus {
	return &status
}

// Test case structure for opcode tests
type OpcodeTestCase struct {
	name            string
	bytecode        []byte
	gasLimit        uint64
	expectedStack   [][32]byte       // skip if nil, check machine-endian encoded
	expectedStatus  *ExecutionStatus // ExecutionSuccess if nil
	expectedGas     uint64           // skip if 0
	expectedRefund  uint64
	expectedMemory  *Memory       // skip if nil, check big-endian encoded
	expectedStorage state.Storage // skip if nil, check big-endian encoded
	originStorage   state.Storage
	originAccount   *types.StateAccount
	originCode      []byte
}

// Helper function to run individual opcode test
func runOpcodeTest(t *testing.T, testCase OpcodeTestCase) {
	t.Helper()

	comp := NewEVMCompiler()
	defer comp.Dispose()

	if testCase.gasLimit == 0 {
		testCase.gasLimit = 1000000
	}

	// create a new stateDB for each testCase to avoid state pollution.
	stateDB, _ := state.New(types.EmptyRootHash, state.NewDatabaseForTesting())
	if acct := testCase.originAccount; acct != nil {
		stateDB.AddBalance(defaultCompilationAddress, acct.Balance, tracing.BalanceChangeUnspecified)
		stateDB.SetNonce(defaultCompilationAddress, acct.Nonce, tracing.NonceChangeUnspecified)
	}
	if code := testCase.originCode; code != nil {
		stateDB.SetCode(defaultCompilationAddress, code)
	}
	if len(testCase.originStorage) > 0 {
		for key, value := range testCase.originStorage {
			stateDB.SetState(defaultCompilationAddress, key, value)
		}
	}
	// Finalise stateDB to clear dirty storage and set the above state as original storage
	stateDB.Finalise(false)

	opts := &EVMExecutionOpts{
		Config: &runtime.Config{
			ChainConfig: params.MergedTestChainConfig,
			GasLimit:    testCase.gasLimit,
			State:       stateDB,
			Origin:      defaultOriginAddress,
			Coinbase:    defaultCoinbaseAddress,
			BlockNumber: common.Big0,
			Value:       defaultCallValue,
		},
		Input: defaultInput[:],
	}

	result, err := comp.ExecuteCompiledWithOpts(testCase.bytecode, DefaultEVMCompilationOpts(), opts)

	if testCase.expectedStatus != nil {
		if *testCase.expectedStatus != result.Status {
			t.Errorf("Expected error %v but got %v", *testCase.expectedStatus, result.Status)
		}
		return
	} else if result.Status != ExecutionSuccess {
		t.Errorf("Expected success but got error %v", result.Status)
	}

	if err != nil {
		t.Fatalf("Execution failed: %v", err)
	}

	if testCase.expectedStack != nil {
		if len(result.Stack) != len(testCase.expectedStack) {
			t.Errorf("Stack length mismatch: expected %d, got %d",
				len(testCase.expectedStack), len(result.Stack))
			t.Errorf("Expected stack: %v", testCase.expectedStack)
			t.Errorf("Actual stack: %v", result.Stack)
			return
		}

		for i, expected := range testCase.expectedStack {
			if result.Stack[i] != expected {
				t.Errorf("Stack[%d] mismatch: expected %v (value: %d), got %v (value: %d)",
					i, expected, bytes32ToUint64(expected),
					result.Stack[i], bytes32ToUint64(result.Stack[i]))
			}
		}
	}

	if expectedMem := testCase.expectedMemory; expectedMem != nil {
		mem := result.Memory
		if expectedMem.lastGasCost != mem.lastGasCost {
			t.Errorf("Expected memory's lastGasCost: %v, actual lastGasCost: %v", expectedMem.lastGasCost, mem.lastGasCost)
			return
		}
		if !bytes.Equal(expectedMem.store, mem.store) {
			t.Errorf("Expected memory: %v, actual memory: %v", expectedMem.store, mem.store)
			return
		}
	}

	if expectedStorage := testCase.expectedStorage; expectedStorage != nil {
		for key, expected := range testCase.expectedStorage {
			got := stateDB.GetState(defaultCompilationAddress, key)
			if got != expected {
				t.Errorf("Storage for [key:%v] mismatch: expected %v, got %v",
					key, expected, got)
			}
		}
	}

	if testCase.expectedGas != 0 && testCase.expectedGas != result.GasUsed {
		t.Errorf("Expected gas: %v, actual gas: %v", testCase.expectedGas, result.GasUsed)
		return
	}

	if testCase.expectedRefund != stateDB.GetRefund() {
		t.Errorf("Expected refund: %v, actual refund: %v", testCase.expectedRefund, stateDB.GetRefund())
		return
	}
}

// TestArithmeticOpcodes tests all arithmetic operations
func TestArithmeticOpcodes(t *testing.T) {
	testCases := []OpcodeTestCase{
		{
			name: "ADD",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5
				0x60, 0x03, // PUSH1 3
				0x01, // ADD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(8)},
		},
		{
			name: "MUL",
			bytecode: []byte{
				0x60, 0x06, // PUSH1 6
				0x60, 0x07, // PUSH1 7
				0x02, // MUL
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(42)},
		},
		{
			name: "SUB",
			bytecode: []byte{
				0x60, 0x04, // PUSH1 4
				0x60, 0x0A, // PUSH1 10
				0x03, // SUB
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(6)},
		},
		{
			name: "DIV",
			bytecode: []byte{
				0x60, 0x04, // PUSH1 4
				0x60, 0x14, // PUSH1 20
				0x04, // DIV
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(5)},
		},
		{
			name: "DIV_BY_ZERO",
			bytecode: []byte{
				0x60, 0x0A, // PUSH1 10
				0x60, 0x00, // PUSH1 0
				0x04, // DIV
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "SDIV",
			bytecode: []byte{
				0x60, 0x04, // PUSH1 4, denominator
				0x60, 0x14, // PUSH1 20, numerator
				0x05, // SDIV
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(5)},
		},
		{
			name: "SDIV_BY_ZERO",
			bytecode: []byte{
				0x60, 0x00, // PUSH1 0, denominator
				0x60, 0x14, // PUSH1 20, numerator
				0x05, // SDIV
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "SDIV_NEGATIVE",
			bytecode: append(
				[]byte{0x7F}, // PUSH32
				append(
					hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")[:], // -1, denominator
					append(
						[]byte{0x7F}, // PUSH32
						append(
							hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE"), // -2, numerator
							0x05, // SDIV
							0x00, // STOP
						)...,
					)...,
				)...,
			),
			expectedStack: [][32]byte{uint64ToBytes32(2)},
		},
		{
			name: "SDIV_OVERFLOW",
			bytecode: append(
				[]byte{0x7F}, // PUSH32
				append(
					hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")[:], // -1, denominator
					append(
						[]byte{0x7F}, // PUSH32
						append(
							hexutil.MustDecode("0x8000000000000000000000000000000000000000000000000000000000000000"), // -2^255, numerator
							0x05, // SDIV
							0x00, // STOP
						)...,
					)...,
				)...,
			),
			expectedStack: [][32]byte{hexToLittleEndianBytes32("0x8000000000000000000000000000000000000000000000000000000000000000")}, // -2^255
		},
		{
			name: "MOD",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5
				0x60, 0x11, // PUSH1 17
				0x06, // MOD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(2)},
		},
		{
			name: "MOD_BY_ZERO",
			bytecode: []byte{
				0x60, 0x00, // PUSH1 5, denominator
				0x60, 0x11, // PUSH1 17, numerator
				0x06, // MOD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "SMOD",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5, denominator
				0x60, 0x11, // PUSH1 17, numerator
				0x07, // SMOD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(2)},
		},
		{
			name: "SMOD_BY_ZERO",
			bytecode: []byte{
				0x60, 0x00, // PUSH1 0, denominator
				0x60, 0x11, // PUSH1 17, numerator
				0x07, // SMOD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "SMOD_NEGATIVE",
			bytecode: append(
				[]byte{0x7F}, // PUSH32
				append(
					hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD")[:], // -3, denominator
					append(
						[]byte{0x7F}, // PUSH32
						append(
							hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8"), // -8, numerator
							0x07, // SMOD
							0x00, // STOP
						)...,
					)...,
				)...,
			),
			expectedStack: [][32]byte{hexToLittleEndianBytes32("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE")}, // -2
		},
		{
			name: "ADDMOD",
			bytecode: []byte{
				0x60, 0x08, // PUSH1 8, denominator
				0x60, 0x0a, // PUSH1 10, second to add
				0x60, 0x0a, // PUSH1 10, first to add
				0x08, // ADDMOD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(4)},
		},
		{
			name: "ADDMOD_ZERO",
			bytecode: []byte{
				0x60, 0x00, // PUSH1 0, denominator
				0x60, 0x0a, // PUSH1 10, second to add
				0x60, 0x0a, // PUSH1 10, first to add
				0x08, // ADDMOD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "ADDMOD_NEGATIVE",
			bytecode: append(
				[]byte{
					0x60, 0x02, // PUSH1 2, denominator
					0x60, 0x02, // PUSH1 2, second to add
					0x7F, // PUSH32
				},
				append(
					hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")[:], // -1, first to add
					0x08, // ADDMOD
					0x00, // STOP
				)...,
			),
			expectedStack: [][32]byte{uint64ToBytes32(1)},
		},
		{
			name: "MULMOD",
			bytecode: []byte{
				0x60, 0x08, // PUSH1 8, denominator
				0x60, 0x0a, // PUSH1 10, second to mul
				0x60, 0x0a, // PUSH1 10, first to mul
				0x09, // MULMOD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(4)},
		},
		{
			name: "MULMOD_ZERO",
			bytecode: []byte{
				0x60, 0x00, // PUSH1 0, denominator
				0x60, 0x0a, // PUSH1 10, second to mul
				0x60, 0x0a, // PUSH1 10, first to mul
				0x09, // MULMOD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "MULMOD_NEGATIVE",
			bytecode: append(
				[]byte{
					0x60, 0x0c, // PUSH1 12, denominator
					0x7F, // PUSH32
				},
				append(
					hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")[:], // -1=2^256-1, second to mul
					append(
						[]byte{0x7F},
						append(
							hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")[:], // -1=2^256-1, first to mul
							0x09, // MULMOD
							0x00, // STOP
						)...,
					)...,
				)...,
			),
			expectedStack: [][32]byte{uint64ToBytes32(9)},
		},
		{
			name: "EXP",
			bytecode: []byte{
				0x60, 0x02, // PUSH1 2 (exp)
				0x60, 0x03, // PUSH1 3 (base)
				0x0A, // EXP
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(9)},
			expectedGas:   3*2 + 60,
		},
		{
			name: "EXP_2BYTES",
			bytecode: []byte{
				0x61, 0x02, 0x01, // PUSH2 0x0101 (exp)
				0x60, 0x02, // PUSH1 2 (base)
				0x0A, // EXP
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
			expectedGas:   3*2 + 110,
		},
		{
			name: "SIGNEXTEND",
			bytecode: []byte{
				0x60, 0x11, // PUSH1 0x11, value
				0x60, 0x00, // PUSH1 0, byte-1
				0x0B, // SIGNEXTEND
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x11)},
		},
		{
			name: "SIGNEXTEND_NEGTIVE",
			bytecode: []byte{
				0x60, 0xFF, // PUSH1 0xFF, value
				0x60, 0x00, // PUSH1 0, byte-1
				0x0B, // SIGNEXTEND
				0x00, // STOP
			},
			expectedStack: [][32]byte{hexToLittleEndianBytes32("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")},
		},
		{
			name: "SIGNEXTEND_PARTIAL",
			bytecode: []byte{
				0x62, 0xFF, 0x11, 0x1F, // PUSH3 0xFF111F, value
				0x60, 0x01, // PUSH1 1, byte-1
				0x0B, // SIGNEXTEND
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x111F)},
		},
		{
			name: "SIGNEXTEND_PARTIAL_NEGATIVE",
			bytecode: []byte{
				0x62, 0x11, 0xF1, 0x1F, // PUSH3 0x11F11F, value
				0x60, 0x01, // PUSH1 1, byte-1
				0x0B, // SIGNEXTEND
				0x00, // STOP
			},
			expectedStack: [][32]byte{hexToLittleEndianBytes32("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF11F")},
		},
		{
			name: "SIGNEXTEND_31",
			bytecode: []byte{
				0x61, 0x11, 0x1F, // PUSH2 0x111F, value
				0x60, 0x1F, // PUSH1 31, byte-1
				0x0B, // SIGNEXTEND
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x111F)},
		},
		{
			name: "SIGNEXTEND_31_NEGATIVE",
			bytecode: []byte{
				0x62, 0xFF, 0x11, 0x1F, // PUSH3 0xFF111F, value
				0x60, 0x1F, // PUSH1 31, byte-1
				0x0B, // SIGNEXTEND
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0xFF111F)},
		},
		{
			name: "SIGNEXTEND_GT31",
			bytecode: []byte{
				0x62, 0xFF, 0x11, 0x1F, // PUSH3 0xFF111F, value
				0x60, 0x20, // PUSH1 32, byte-1
				0x0B, // SIGNEXTEND
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0xFF111F)},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			runOpcodeTest(t, tc)
		})
	}
}

// TestComparisonOpcodes tests all comparison operations
func TestComparisonOpcodes(t *testing.T) {
	testCases := []OpcodeTestCase{
		{
			name: "LT_TRUE",
			bytecode: []byte{
				0x60, 0x0A, // PUSH1 10
				0x60, 0x05, // PUSH1 5
				0x10, // LT
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(1)},
		},
		{
			name: "LT_FALSE",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5
				0x60, 0x0A, // PUSH1 10
				0x10, // LT
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "GT_TRUE",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5
				0x60, 0x0A, // PUSH1 10
				0x11, // GT
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(1)},
		},
		{
			name: "GT_FALSE",
			bytecode: []byte{
				0x60, 0x0A, // PUSH1 10
				0x60, 0x05, // PUSH1 5
				0x11, // GT
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "SLT_TRUE",
			bytecode: []byte{
				0x60, 0x0A, // PUSH1 10
				0x60, 0x05, // PUSH1 5
				0x12, // SLT
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(1)},
		},
		{
			name: "SLT_FALSE",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5
				0x60, 0x0A, // PUSH1 10
				0x12, // SLT
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "SLT_NEGATIVE_TRUE",
			bytecode: append(
				[]byte{0x7F}, // PUSH32
				append(
					hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")[:], // -1
					append(
						[]byte{0x7F}, // PUSH32
						append(
							hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE")[:], // -2
							0x12, // SLT
							0x00, // STOP
						)...,
					)...,
				)...,
			),
			expectedStack: [][32]byte{uint64ToBytes32(1)},
		},
		{
			name: "SLT_NEGATIVE_FALSE",
			bytecode: append(
				[]byte{0x7F}, // PUSH32
				append(
					hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE")[:], // -2
					append(
						[]byte{0x7F}, // PUSH32
						append(
							hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")[:], // -1
							0x12, // SLT
							0x00, // STOP
						)...,
					)...,
				)...,
			),
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "SLT_HYBRID_FALSE",
			bytecode: append(
				[]byte{0x7F}, // PUSH32
				append(
					hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")[:], // -1
					0x60, 0x05, // PUSH1 5
					0x12, // SLT
					0x00, // STOP
				)...,
			),
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "SGT_TRUE",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5
				0x60, 0x0A, // PUSH1 10
				0x13, // SGT
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(1)},
		},
		{
			name: "SGT_FALSE",
			bytecode: []byte{
				0x60, 0x0A, // PUSH1 10
				0x60, 0x05, // PUSH1 5
				0x13, // SGT
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "SGT_NEGATIVE_TRUE",
			bytecode: append(
				[]byte{0x7F}, // PUSH32
				append(
					hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE")[:], // -2
					append(
						[]byte{0x7F}, // PUSH32
						append(
							hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")[:], // -1
							0x13, // SGT
							0x00, // STOP
						)...,
					)...,
				)...,
			),
			expectedStack: [][32]byte{uint64ToBytes32(1)},
		},
		{
			name: "SGT_NEGATIVE_FALSE",
			bytecode: append(
				[]byte{0x7F}, // PUSH32
				append(
					hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")[:], // -1
					append(
						[]byte{0x7F}, // PUSH32
						append(
							hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE")[:], // -2
							0x13, // SGT
							0x00, // STOP
						)...,
					)...,
				)...,
			),
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "SGT_HYBRID_TRUE",
			bytecode: append(
				[]byte{0x7F}, // PUSH32
				append(
					hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")[:], // -1
					0x60, 0x05, // PUSH1 5
					0x13, // SGT
					0x00, // STOP
				)...,
			),
			expectedStack: [][32]byte{uint64ToBytes32(1)},
		},
		{
			name: "EQ_TRUE",
			bytecode: []byte{
				0x60, 0x07, // PUSH1 7
				0x60, 0x07, // PUSH1 7
				0x14, // EQ
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(1)},
		},
		{
			name: "EQ_FALSE",
			bytecode: []byte{
				0x60, 0x07, // PUSH1 7
				0x60, 0x08, // PUSH1 8
				0x14, // EQ
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "ISZERO_TRUE",
			bytecode: []byte{
				0x60, 0x00, // PUSH1 0
				0x15, // ISZERO
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(1)},
		},
		{
			name: "ISZERO_FALSE",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5
				0x15, // ISZERO
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			runOpcodeTest(t, tc)
		})
	}
}

// TestBitwiseOpcodes tests all bitwise operations
func TestBitwiseOpcodes(t *testing.T) {
	testCases := []OpcodeTestCase{
		{
			name: "AND",
			bytecode: []byte{
				0x60, 0xFF, // PUSH1 0xFF
				0x60, 0x0F, // PUSH1 0x0F
				0x16, // AND
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x0F)},
		},
		{
			name: "OR",
			bytecode: []byte{
				0x60, 0xF0, // PUSH1 0xF0
				0x60, 0x0F, // PUSH1 0x0F
				0x17, // OR
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0xFF)},
		},
		{
			name: "XOR",
			bytecode: []byte{
				0x60, 0xFF, // PUSH1 0xFF
				0x60, 0x0F, // PUSH1 0x0F
				0x18, // XOR
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0xF0)},
		},
		{
			name: "NOT",
			bytecode: []byte{
				0x60, 0x00, // PUSH1 0x00
				0x19, // NOT
				0x00, // STOP
			},
			expectedStack: [][32]byte{func() [32]byte {
				var result [32]byte
				for i := range result {
					result[i] = 0xFF
				}
				return result
			}()},
		},
		{
			name: "BYTE",
			bytecode: []byte{
				0x60, 0xFF, // PUSH1 0xFF, value
				0x60, 0x1F, // PUSH1 31, byte offset
				0x1A, // BYTE
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0xFF)},
		},
		{
			name: "BYTE_2BYTES",
			bytecode: []byte{
				0x61, 0xFF, 0x00, // PUSH2 0xFF00, value
				0x60, 0x1E, // PUSH1 30, byte offset
				0x1A, // BYTE
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0xFF)},
		},
		{
			name: "BYTE_OFFSET_SHORT",
			bytecode: []byte{
				0x61, 0xFF, 0x11, // PUSH2 0xFF11, value
				0x60, 0x01, // PUSH1 1, byte offset
				0x1A, // BYTE
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x00)},
		},
		{
			name: "BYTE_OFFSET_OOB",
			bytecode: []byte{
				0x61, 0xFF, 0x11, // PUSH2 0xFF11, value
				0x60, 0x20, // PUSH1 32, byte offset (out of bounds)
				0x1A, // BYTE
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x00)},
		},
		{
			name: "SHL",
			bytecode: []byte{
				0x60, 0x01, // PUSH1 0x01, shift
				0x60, 0xFF, // PUSH1 0xFF, value
				0x1B, // SHL
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x1fe)},
		},
		{
			name: "SHL_255",
			bytecode: []byte{
				0x60, 0xFF, // PUSH1 0xFF, shift
				0x60, 0xFF, // PUSH1 0xFF, value
				0x1B, // SHL
				0x00, // STOP
			},
			expectedStack: [][32]byte{hexToLittleEndianBytes32("0x8000000000000000000000000000000000000000000000000000000000000000")},
		},
		{
			name: "SHR",
			bytecode: []byte{
				0x60, 0x01, // PUSH1 0x01, shift
				0x60, 0xFF, // PUSH1 0xFF, value
				0x1C, // SHR
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x7f)},
		},
		{
			name: "SHR_255",
			bytecode: []byte{
				0x60, 0xFF, // PUSH1 0xFF, shift
				0x60, 0xFF, // PUSH1 0xFF, value
				0x1C, // SHR
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x00)},
		},
		{
			name: "SAR",
			bytecode: []byte{
				0x60, 0x01, // PUSH1 0x01, shift
				0x60, 0xFF, // PUSH1 0xFF, value
				0x1D, // SAR
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x7f)},
		},
		{
			name: "SAR_NEG",
			bytecode: append(
				[]byte{
					0x60, 0x04, // PUSH1 0x04, shift
					0x7F, // PUSH32
				},
				append(
					hexutil.MustDecode("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0")[:], // -16, value
					0x1D, // SAR
					0x00, // STOP
				)...,
			),
			expectedStack: [][32]byte{hexToLittleEndianBytes32("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")}, // -1
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			runOpcodeTest(t, tc)
		})
	}
}

// TestStackOpcodes tests stack manipulation operations
func TestStackOpcodes(t *testing.T) {
	testCases := []OpcodeTestCase{
		{
			name: "POP",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5
				0x50, // POP
				0x00, // STOP
			},
			expectedStack: [][32]byte{}, // Empty stack
		},
		{
			name: "DUP1",
			bytecode: []byte{
				0x60, 0x2A, // PUSH1 42
				0x80, // DUP1
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(42), uint64ToBytes32(42)},
		},
		{
			name: "DUP2",
			bytecode: []byte{
				0x60, 0x01, // PUSH1 1
				0x60, 0x02, // PUSH1 2
				0x81, // DUP2
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(1), uint64ToBytes32(2), uint64ToBytes32(1)},
		},
		{
			name: "SWAP1",
			bytecode: []byte{
				0x60, 0x01, // PUSH1 1
				0x60, 0x02, // PUSH1 2
				0x90, // SWAP1
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(2), uint64ToBytes32(1)},
		},
		{
			name: "SWAP2",
			bytecode: []byte{
				0x60, 0x01, // PUSH1 1
				0x60, 0x02, // PUSH1 2
				0x60, 0x03, // PUSH1 3
				0x91, // SWAP2
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(3), uint64ToBytes32(2), uint64ToBytes32(1)},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			runOpcodeTest(t, tc)
		})
	}
}

// TestPushOpcodes tests PUSH operations with different data sizes
func TestPushOpcodes(t *testing.T) {
	testCases := []OpcodeTestCase{
		{
			name: "PUSH1",
			bytecode: []byte{
				0x60, 0x42, // PUSH1 0x42
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x42)},
		},
		{
			name: "PUSH2",
			bytecode: []byte{
				0x61, 0x12, 0x34, // PUSH2 0x1234
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x1234)},
		},
		{
			name: "PUSH4",
			bytecode: []byte{
				0x63, 0x12, 0x34, 0x56, 0x78, // PUSH4 0x12345678
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x12345678)},
		},
		{
			name: "PUSH8",
			bytecode: []byte{
				0x67, 0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE, 0xF0, // PUSH8
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x123456789ABCDEF0)},
		},
		{
			name: "PUSH_1024",
			bytecode: func() []byte {
				buf := make([]byte, 0)
				for i := 0; i < 1024; i++ {
					buf = append(buf, 0x60)
					buf = append(buf, 0x05)
				}
				buf = append(buf, 0x0)
				return buf
			}(),
		},
		{
			name: "PUSH_1025_OVERFLOW",
			bytecode: func() []byte {
				buf := make([]byte, 0)
				for i := 0; i <= 1024; i++ {
					buf = append(buf, 0x60)
					buf = append(buf, 0x05)
				}
				return buf
			}(),
			expectedStatus: getExpectedStatus(ExecutionStackOverflow),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			runOpcodeTest(t, tc)
		})
	}
}

// TestMemoryOpcodes tests memory operations
func TestMemoryOpcodes(t *testing.T) {
	testCases := []OpcodeTestCase{
		{
			name: "MSTORE_MLOAD",
			bytecode: []byte{
				0x60, 0x42, // PUSH1 0x42 (value)
				0x60, 0x00, // PUSH1 0x00 (offset)
				0x52,       // MSTORE
				0x60, 0x00, // PUSH1 0x00 (offset)
				0x51, // MLOAD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x42)},
			expectedMemory: &Memory{
				store:       common.Hex2Bytes("0000000000000000000000000000000000000000000000000000000000000042"),
				lastGasCost: 3, // memory expansion cost for the last MLOAD
			},
			expectedGas: 3*5 + 3, // 3 is the linear memory expansion cost
		},
		{
			name: "MSTORE_MLOAD_EXPAND_LINEAR",
			bytecode: []byte{
				0x60, 0x42, // PUSH1 0x42 (value)
				0x60, 0x00, // PUSH1 0x00 (offset)
				0x52,       // MSTORE
				0x60, 0x40, // PUSH1 0x40 (offset)
				0x51, // MLOAD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x0)},
			expectedMemory: &Memory{
				store:       common.Hex2Bytes("000000000000000000000000000000000000000000000000000000000000004200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
				lastGasCost: 3 * 3, // memory expansion cost for the last MLOAD
			},
			expectedGas: 3*5 + 3*3, // 9 is the linear memory expansion cost
		},
		{
			name: "MSTORE_MLOAD_EXPAND_QUAD",
			bytecode: []byte{
				0x60, 0x42, // PUSH1 0x42 (value)
				0x60, 0x00, // PUSH1 0x00 (offset)
				0x52,             // MSTORE
				0x61, 0x04, 0x00, // PUSH2 0x0400 (offset)
				0x51, // MLOAD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x0)},
			expectedMemory: &Memory{
				store:       common.Hex2Bytes("000000000000000000000000000000000000000000000000000000000000004200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
				lastGasCost: 3*33 + (33*33)/512, // memory expansion cost for the last MLOAD
			},
			expectedGas: 3*5 + 3*33 + (33*33)/512,
		},
		{
			name: "MSTORE_MLOAD_EXPAND_UNALIGNED",
			bytecode: []byte{
				0x60, 0x42, // PUSH1 0x42 (value)
				0x60, 0x00, // PUSH1 0x00 (offset)
				0x52,       // MSTORE
				0x60, 0x01, // PUSH1 0x01 (offset)
				0x51, // MLOAD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x42 * 256)},
			expectedMemory: &Memory{
				store:       common.Hex2Bytes("00000000000000000000000000000000000000000000000000000000000000420000000000000000000000000000000000000000000000000000000000000000"),
				lastGasCost: 2 * 3, // memory expansion cost for the last MLOAD
			},
			expectedGas: 3*5 + 2*3,
		},
		{
			name: "MSTORE_MSTORE_EXPAND_UNALIGNED",
			bytecode: []byte{
				0x60, 0xFF, // PUSH1 0xFF (value)
				0x60, 0x00, // PUSH1 0x00 (offset)
				0x52,       // MSTORE
				0x60, 0xFF, // PUSH1 0x40 (offset)
				0x60, 0x01, // PUSH1 0x01
				0x52, // MSTORE
				0x00, // STOP
			},
			expectedStack: [][32]byte{},
			expectedMemory: &Memory{
				store:       common.Hex2Bytes("0000000000000000000000000000000000000000000000000000000000000000ff00000000000000000000000000000000000000000000000000000000000000"),
				lastGasCost: 2 * 3, // memory expansion cost for the last MSTORE
			},
			expectedGas: 3*6 + 2*3,
		},
		{
			name: "MSTORE8_MLOAD",
			bytecode: []byte{
				0x60, 0xFF, // PUSH1 0xFF (value)
				0x60, 0x00, // PUSH1 0x00 (offset)
				0x53,       // MSTORE8
				0x60, 0x00, // PUSH1 0x00 (offset)
				0x51, // MLOAD
				0x00, // STOP
			},
			expectedStack: [][32]byte{Reverse32Bytes(uint64ToBytes32(0xFF))},
			expectedMemory: &Memory{
				store:       common.Hex2Bytes("FF00000000000000000000000000000000000000000000000000000000000000"),
				lastGasCost: 1 * 3,
			},
			expectedGas: 3*5 + 1*3,
		},
		{
			name: "MSTORE8_MLOAD_OFFSET31",
			bytecode: []byte{
				0x60, 0xFF, // PUSH1 0xFF (value)
				0x60, 0x1F, // PUSH1 0x1F (offset)
				0x53,       // MSTORE8
				0x60, 0x00, // PUSH1 0x00 (offset)
				0x51, // MLOAD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0xFF)},
			expectedMemory: &Memory{
				store:       common.Hex2Bytes("00000000000000000000000000000000000000000000000000000000000000FF"),
				lastGasCost: 1 * 3,
			},
			expectedGas: 3*5 + 1*3,
		},
		{
			name: "MSTORE_MLOAD_ENCODING",
			bytecode: []byte{
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
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x42)},
			expectedMemory: &Memory{
				store:       common.Hex2Bytes("00000000000000000000000000000000000000000000000000000000000000420000000000000000000000000000000000000000000000000000000000000042"),
				lastGasCost: 6,
			},
			expectedGas: 3*5 + 6 + 3 + 6 + 3,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			runOpcodeTest(t, tc)
		})
	}
}

// TestStorageOpcodes tests storage operations
func TestStorageOpcodes(t *testing.T) {
	testCases := []OpcodeTestCase{
		{
			name: "SLOAD_EMPTY",
			bytecode: []byte{
				0x60, 0x01, // PUSH1 0x01 (key)
				0x54, // SLOAD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x0)},
			expectedGas:   3 + params.ColdSloadCostEIP2929,
		},
		{
			name: "SLOAD_WARM",
			bytecode: []byte{
				0x60, 0x42, // PUSH1 0x42 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55,       // SSTORE
				0x60, 0x01, // PUSH1 0x01 (key)
				0x54, // SLOAD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x42)},
			expectedStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x42)),
			},
			expectedGas: 3*3 + (params.SstoreSetGasEIP2200 + params.ColdSloadCostEIP2929) + (params.WarmStorageReadCostEIP2929),
		},
		{
			name: "SLOAD_OOG",
			bytecode: []byte{
				0x60, 0x01, // PUSH1 0x01 (key)
				0x54, // SLOAD
				0x00, // STOP
			},
			expectedStack:  [][32]byte{uint64ToBigEndianBytes32(0x00)},
			gasLimit:       2000,
			expectedStatus: getExpectedStatus(ExecutionOutOfGas),
		},
		{
			name: "SSTORE_ORIGIN_0_EQ_CURRENT_EQ_NEW",
			bytecode: []byte{
				0x60, 0x00, // PUSH1 0x00 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55, // SSTORE (key:new=current=origin=0)
				0x00, // STOP
			},
			expectedStack: [][32]byte{},
			expectedStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x00)),
			},
			expectedGas:    3*2 + params.ColdSloadCostEIP2929 + params.WarmStorageReadCostEIP2929,
			expectedRefund: 0,
		},
		{
			name: "SSTORE_ORIGIN_0_EQ_CURRENT_NEQ_NEW",
			bytecode: []byte{
				0x60, 0x42, // PUSH1 0x42 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55, // SSTORE (key:new≠current=origin=0)
				0x00, // STOP
			},
			expectedStack: [][32]byte{},
			expectedStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x42)),
			},
			expectedGas:    3*2 + params.SstoreSetGasEIP2200 + params.ColdSloadCostEIP2929,
			expectedRefund: 0,
		},
		{
			name: "SSTORE_ORIGIN_N0_EQ_CURRENT_NEQ_NEW",
			originStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x01)),
			},
			bytecode: []byte{
				0x60, 0x01, // PUSH1 0x01 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55,       // SSTORE (current=origin≠0)
				0x60, 0x03, // PUSH1 0x03 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55, // SSTORE (key:new≠current=origin≠0)
				0x00, // STOP
			},
			expectedStack: [][32]byte{},
			expectedStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x03)),
			},
			expectedGas:    3*4 + (params.ColdSloadCostEIP2929 + params.WarmStorageReadCostEIP2929) + (params.SstoreResetGasEIP2200 - params.ColdSloadCostEIP2929),
			expectedRefund: 0,
		},
		{
			name: "SSTORE_ORIGIN_N0_EQ_CURRENT_N0_NEQ_NEW_0",
			originStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x01)),
			},
			bytecode: []byte{
				0x60, 0x00, // PUSH1 0x00 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55, // SSTORE (key:new=0≠current=origin≠0, refund 4800)
				0x00, // STOP
			},
			expectedStack: [][32]byte{},
			expectedStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x00)),
			},
			expectedGas:    3*2 + params.ColdSloadCostEIP2929 + params.SstoreResetGasEIP2200 - params.ColdSloadCostEIP2929,
			expectedRefund: params.NetSstoreResetRefund,
		},
		{
			name: "SSTORE_ORIGIN_N0_NEQ_CURRENT_0_NEQ_NEW_N0",
			originStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x01)),
			},
			bytecode: []byte{
				0x60, 0x00, // PUSH1 0x00 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55,       // SSTORE (key:new=0≠current=origin≠0, refund 4800)
				0x60, 0x03, // PUSH1 0x03 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55, // SSTORE (key:new≠current=0≠origin≠0, refund -4800)
				0x00, // STOP
			},
			expectedStack: [][32]byte{},
			expectedStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x03)),
			},
			expectedGas:    3*4 + (params.ColdSloadCostEIP2929 + params.WarmStorageReadCostEIP2929) + (params.SstoreResetGasEIP2200 - params.ColdSloadCostEIP2929),
			expectedRefund: 0,
		},
		{
			name: "SSTORE_ORIGIN_N0_NEQ_CURRENT_N0_NEQ_NEW_0",
			originStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x01)),
			},
			bytecode: []byte{
				0x60, 0x02, // PUSH1 0x02 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55,       // SSTORE (key:new≠0≠current=origin≠0, refund 0)
				0x60, 0x00, // PUSH1 0x00 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55, // SSTORE (key:new=0≠current≠0≠origin≠0, refund 4800)
				0x00, // STOP
			},
			expectedStack: [][32]byte{},
			expectedStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x00)),
			},
			expectedGas:    3*4 + (params.ColdSloadCostEIP2929 + params.WarmStorageReadCostEIP2929) + (params.SstoreResetGasEIP2200 - params.ColdSloadCostEIP2929),
			expectedRefund: params.NetSstoreResetRefund,
		},
		{
			name: "SSTORE_ORIGIN_0_NEQ_CURRENT_N0_NEQ_NEW_EQ_ORIGIN",
			bytecode: []byte{
				0x60, 0x02, // PUSH1 0x02 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55,       // SSTORE (key:new≠0≠current=origin=0, refund 0)
				0x60, 0x00, // PUSH1 0x00 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55, // SSTORE (key:new=0≠current≠0≠origin=0, refund 20000-100)
				0x00, // STOP
			},
			expectedStack: [][32]byte{},
			expectedStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x00)),
			},
			expectedGas:    3*4 + (params.SstoreSetGasEIP2200 + params.ColdSloadCostEIP2929) + (params.WarmStorageReadCostEIP2929),
			expectedRefund: params.SstoreSetGasEIP2200 - params.WarmStorageReadCostEIP2929,
		},
		{
			name: "SSTORE_ORIGIN_N0_NEQ_CURRENT_N0_NEQ_NEW_EQ_ORIGIN",
			originStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x01)),
			},
			bytecode: []byte{
				0x60, 0x02, // PUSH1 0x02 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55,       // SSTORE (key:new≠0≠current=origin≠0, refund 0)
				0x60, 0x01, // PUSH1 0x01 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55, // SSTORE (key:new=1≠current≠0≠origin=1, refund 5000-2100-100)
				0x00, // STOP
			},
			expectedStack: [][32]byte{},
			expectedStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x01)),
			},
			expectedGas:    3*4 + (params.ColdSloadCostEIP2929 + params.WarmStorageReadCostEIP2929) + (params.SstoreResetGasEIP2200 - params.ColdSloadCostEIP2929),
			expectedRefund: params.SstoreResetGasEIP2200 - params.ColdSloadCostEIP2929 - params.WarmStorageReadCostEIP2929,
		},
		{
			name: "SSTORE_OOG",
			bytecode: []byte{
				0x60, 0x42, // PUSH1 0x42 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55, // SSTORE
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x00)},
			expectedStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x00)),
			},
			gasLimit:       15000,
			expectedStatus: getExpectedStatus(ExecutionOutOfGas),
		},
		{
			name: "SSTORE_SLOAD_ENCODING",
			originStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x01)),
			},
			bytecode: []byte{
				0x60, 0x02, // PUSH1 0x02 (value)
				0x60, 0x01, // PUSH1 0x01 (key)
				0x55,       // SSTORE
				0x60, 0x01, // PUSH1 0x01 (key)
				0x54,       // SLOAD (value)
				0x60, 0x11, // PUSH1 0x11 (key)
				0x55,       // SSTORE
				0x60, 0x11, // PUSH1 0x11 (key)
				0x54, // SLOAD
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x02)},
			expectedStorage: state.Storage{
				common.Hash(uint64ToBigEndianBytes32(0x01)): common.Hash(uint64ToBigEndianBytes32(0x02)),
				common.Hash(uint64ToBigEndianBytes32(0x11)): common.Hash(uint64ToBigEndianBytes32(0x02)),
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			runOpcodeTest(t, tc)
		})
	}
}

// TestControlFlowOpcodes tests control flow operations
func TestControlFlowOpcodes(t *testing.T) {
	testCases := []OpcodeTestCase{
		{
			name: "PC",
			bytecode: []byte{
				0x58, // PC (PC should be 0 at this point)
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0)},
		},
		{
			name: "JUMP",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5 (jump target)
				0x56,       // JUMP
				0x60, 0xFF, // PUSH1 0xFF (should be skipped)
				0x5B,       // JUMPDEST (PC = 5)
				0x60, 0x42, // PUSH1 0x42
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x42)}, // Should only have 0x42, not 0xFF
		},
		{
			name: "JUMPI_TRUE",
			bytecode: []byte{
				0x60, 0x01, // PUSH1 1 (condition: true)
				0x60, 0x07, // PUSH1 7 (jump target)
				0x57,       // JUMPI
				0x60, 0xFF, // PUSH1 0xFF (should be skipped)
				0x5B,       // JUMPDEST (PC = 7)
				0x60, 0x42, // PUSH1 0x42
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0x42)},
			expectedGas:   3 + 3 + 10 + 1 + 3,
		},
		{
			name: "JUMPI_FALSE",
			bytecode: []byte{
				0x60, 0x00, // PUSH1 0 (condition: false)
				0x60, 0x07, // PUSH1 7 (jump target)
				0x57,       // JUMPI
				0x60, 0xFF, // PUSH1 0xFF (should be executed)
				0x5B,       // JUMPDEST (PC = 7)
				0x60, 0x42, // PUSH1 0x42
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(0xFF), uint64ToBytes32(0x42)},
			expectedGas:   3 + 3 + 10 + 3 + 1 + 3,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			runOpcodeTest(t, tc)
		})
	}
}

// TestComplexPrograms tests complex combinations of opcodes
func TestComplexPrograms(t *testing.T) {
	testCases := []OpcodeTestCase{
		{
			name: "COMPLEX_ARITHMETIC",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5
				0x60, 0x03, // PUSH1 3
				0x01,       // ADD (result: 8)
				0x60, 0x02, // PUSH1 2
				0x02, // MUL (result: 16)
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(16)},
		},
		{
			name: "DUP_ADD",
			bytecode: []byte{
				0x60, 0x07, // PUSH1 7
				0x80, // DUP1 (duplicate 7)
				0x01, // ADD (7 + 7 = 14)
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(14)},
		},
		{
			name: "CONDITIONAL_LOGIC",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5
				0x60, 0x0A, // PUSH1 10
				0x11,       // GT (10 > 5 = 1)
				0x60, 0x0D, // PUSH1 13 (jump target if true)
				0x57,       // JUMPI
				0x60, 0xC8, // PUSH1 200 (false case)
				0x60, 0x12, // PUSH1 16 (jump to end)
				0x56,       // JUMP
				0x5B,       // JUMPDEST (PC = 13)
				0x60, 0x64, // PUSH1 100 (true case)
				0x5B, // JUMPDEST (PC = 16, end)
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(100)}, // Should be 100 since 10 > 5
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			runOpcodeTest(t, tc)
		})
	}
}

// TestOpcodeErrorConditions tests error conditions and edge cases
func TestOpcodeErrorConditions(t *testing.T) {
	testCases := []OpcodeTestCase{
		{
			name: "STACK_UNDERFLOW_POP",
			bytecode: []byte{
				0x50, // POP
				0x00, // STOP
			},
			expectedStatus: getExpectedStatus(ExecutionStackUnderflow),
		},
		{
			name: "STACK_UNDERFLOW_ADD",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5 (only one operand)
				0x01, // ADD (needs two operands)
				0x00, // STOP
			},
			expectedStatus: getExpectedStatus(ExecutionStackUnderflow),
		},
		{
			name: "STACK_UNDERFLOW_MSTORE",
			bytecode: []byte{
				0x60, 0x42, // PUSH1 0x42 (value)
				0x52, // MSTORE
				0x00, // STOP
			},
			expectedStatus: getExpectedStatus(ExecutionStackUnderflow),
		},
		{
			name: "STACK_UNDERFLOW_ADDMOD",
			bytecode: []byte{
				0x60, 0x08, // PUSH1 8, denominator
				0x60, 0x0a, // PUSH1 10, second to add
				0x08, // ADDMOD
				0x00, // STOP
			},
			expectedStatus: getExpectedStatus(ExecutionStackUnderflow),
		},
		{
			name: "INVALID_JUMP_TARGET",
			bytecode: []byte{
				0x60, 0x05, // PUSH1 5 (invalid jump target)
				0x56, // JUMP
				0x00, // STOP
			},
			expectedStatus: getExpectedStatus(ExecutionInvalidJumpDest),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			runOpcodeTest(t, tc)
		})
	}
}

func TestPrecompiledOpcodes(t *testing.T) {
	testCases := []OpcodeTestCase{
		{
			name: "KECCAK256",
			bytecode: append(
				[]byte{0x7F}, // PUSH32
				append(
					hexutil.MustDecode("0xFFFFFFFF00000000000000000000000000000000000000000000000000000000")[:],
					0x60, 0x00, // PUSH1 0x00
					0x52,       // MSTORE
					0x60, 0x04, // PUSH1 0x04
					0x60, 0x00, // PUSH1 0x00
					0x20, // KECCAK256
					0x00, // STOP
				)...,
			),
			expectedStack: [][32]byte{hexToLittleEndianBytes32("0x29045a592007d0c246ef02c2223570da9522d0cf0f73282c79a1bc8f0bb2c238")},
			expectedGas:   3*4 + 6 + 36,
		},
		{
			name: "KECCAK256_SIZE2",
			bytecode: []byte{
				0x60, 0x04, // PUSH1 0x04
				0x60, 0x20, // PUSH1 32
				0x20, // KECCAK256
				0x00, // STOP
			},
			expectedGas: 3*2 + 42,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			runOpcodeTest(t, tc)
		})
	}
}

// TestOpcodeBlockContext tests block context opcodes
func TestOpcodeBlockContext(t *testing.T) {
	testCases := []OpcodeTestCase{
		{
			name: "ADDRESS",
			bytecode: []byte{
				0x30, // ADDRESS
				0x00, // STOP
			},
			expectedStack: [][32]byte{func() [32]byte {
				var buf [32]byte
				CopyFromBigToMachine(defaultCompilationAddress.Bytes(), buf[:])
				return buf
			}()},
		},
		{
			name: "BALANCE",
			originAccount: &types.StateAccount{
				Balance: uint256.NewInt(10000),
			},
			bytecode: []byte{
				0x30, // ADDRESS (defaultCompilationAddress)
				0x31, // BALANCE
				0x00, // STOP
			},
			expectedStack: [][32]byte{func() [32]byte {
				var buf [32]byte
				CopyFromBigToMachine(uint256.NewInt(10000).Bytes(), buf[:])
				return buf
			}()},
			expectedGas: 102,
		},
		{
			name: "ORIGIN",
			bytecode: []byte{
				0x32, // ORIGIN
				0x00, // STOP
			},
			expectedStack: [][32]byte{func() [32]byte {
				var buf [32]byte
				CopyFromBigToMachine(defaultOriginAddress.Bytes(), buf[:])
				return buf
			}()},
		},
		{
			name: "CALLER",
			bytecode: []byte{
				0x33, // CALLER
				0x00, // STOP
			},
			expectedStack: [][32]byte{func() [32]byte {
				var buf [32]byte
				CopyFromBigToMachine(defaultCallerAddress.Bytes(), buf[:])
				return buf
			}()},
		},
		{
			name: "CALLVALUE",
			bytecode: []byte{
				0x34, // CALLVALUE
				0x00, // STOP
			},
			expectedStack: [][32]byte{func() [32]byte {
				var buf [32]byte
				CopyFromBigToMachine(defaultCallValue.Bytes(), buf[:])
				return buf
			}()},
			expectedGas: 2,
		},
		{
			name: "CALLDATALOAD",
			bytecode: []byte{
				0x60, 0x00, // PUSH1 0x00
				0x35, // CALLDATALOAD
				0x00, // STOP
			},
			expectedStack: [][32]byte{func() [32]byte {
				var buf [32]byte
				CopyFromBigToMachine(defaultInput[:], buf[:])
				return buf
			}()},
			expectedGas: 3 + 3,
		},
		{
			name: "CALLDATASIZE",
			bytecode: []byte{
				0x36, // CALLDATASIZE
				0x00, // STOP
			},
			expectedStack: [][32]byte{uint64ToBytes32(uint64(len(defaultInput)))},
			expectedGas:   2,
		},
		{
			name: "CALLDATACOPY_SIZE_0",
			bytecode: []byte{
				0x60, 0x00, // PUSH1 0 (size to copy)
				0x60, 0x20, // PUSH1 32 (offset in the calldata)
				0x60, 0x00, // PUSH1 0 (offset in the memory)
				0x37, // CALLDATACOPY
				0x00, // STOP
			},
			expectedStack: [][32]byte{},
			expectedMemory: &Memory{
				store:       []byte{},
				lastGasCost: 0,
			},
			expectedGas: 3*3 + 3,
		},
		{
			name: "CALLDATACOPY_SIZE_N0",
			bytecode: []byte{
				0x60, 0x20, // PUSH1 0x20 (size to copy)
				0x60, 0x00, // PUSH1 0 (offset in the calldata)
				0x60, 0x00, // PUSH1 0 (offset in the memory)
				0x37, // CALLDATACOPY
				0x00, // STOP
			},
			expectedStack: [][32]byte{},
			expectedMemory: &Memory{
				store:       defaultInput[:],
				lastGasCost: 3,
			},
			expectedGas: 3*3 + 9,
		},
		{
			name: "COINBASE",
			bytecode: []byte{
				0x41, // COINBASE
				0x00, // STOP
			},
			expectedStack: [][32]byte{func() [32]byte {
				var buf [32]byte
				CopyFromBigToMachine(defaultCoinbaseAddress.Bytes(), buf[:])
				return buf
			}()},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			runOpcodeTest(t, tc)
		})
	}
}

// Benchmark tests for performance measurement
func BenchmarkArithmeticOpcodes(b *testing.B) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	bytecode := []byte{
		0x60, 0x05, // PUSH1 5
		0x60, 0x03, // PUSH1 3
		0x01, // ADD
		0x00, // STOP
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := comp.ExecuteCompiled(bytecode)
		if err != nil {
			b.Fatalf("Execution failed: %v", err)
		}
	}
}

func BenchmarkComplexProgram(b *testing.B) {
	comp := NewEVMCompiler()
	defer comp.Dispose()

	bytecode := []byte{
		0x60, 0x05, // PUSH1 5
		0x60, 0x03, // PUSH1 3
		0x01,       // ADD (result: 8)
		0x60, 0x02, // PUSH1 2
		0x02, // MUL (result: 16)
		0x80, // DUP1 (duplicate 16)
		0x01, // ADD (16 + 16 = 32)
		0x00, // STOP
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := comp.ExecuteCompiled(bytecode)
		if err != nil {
			b.Fatalf("Execution failed: %v", err)
		}
	}
}
