package compiler

import (
	"encoding/binary"
	"unsafe"

	"github.com/holiman/uint256"
)

func Reverse(b []byte) []byte {
	out := make([]byte, len(b))
	for i := range b {
		out[len(b)-1-i] = b[i]
	}
	return out
}

func Reverse32Bytes(b [32]byte) [32]byte {
	var out [32]byte
	for i := range b {
		out[len(b)-1-i] = b[i]
	}
	return out
}

func FromMachineToBig32Bytes(b [32]byte) [32]byte {
	if IsMachineBigEndian() {
		return b
	}
	return Reverse32Bytes(b)
}

func FromMachineToUint256(b [32]byte) *uint256.Int {
	if IsMachineBigEndian() {
		return uint256.NewInt(0).SetBytes32(b[:])
	}
	return uint256.NewInt(0).SetBytes32(Reverse(b[:]))
}

var isMachineBigEndian *bool

func IsMachineBigEndian() bool {
	if isMachineBigEndian != nil {
		return *isMachineBigEndian
	}

	var i int32 = 1 // Use a multi-byte integer, e.g., 1 (0x00000001)

	// Get a pointer to the first byte of the integer
	// This is unsafe as it bypasses Go's type safety
	bytePtr := (*byte)(unsafe.Pointer(&i))

	// Check the value of the first byte
	isBig := false
	if *bytePtr == 0 {
		isBig = true
	}
	isMachineBigEndian = &isBig
	return isBig
}

func Fib(n int) *uint256.Int {
	a := uint256.NewInt(0)
	b := uint256.NewInt(1)
	for i := 0; i < n; i++ {
		a = a.Add(a, b)
		a, b = b, a
	}
	return b
}

func GetFibCode(n uint32) []byte {
	nbs := make([]byte, 4)
	binary.BigEndian.PutUint32(nbs, n)
	bytecode := []EVMOpcode{
		PUSH4, EVMOpcode(nbs[0]), EVMOpcode(nbs[1]), EVMOpcode(nbs[2]), EVMOpcode(nbs[3]),
		PUSH1, 0,
		PUSH1, 1,
		// off 9
		// MAINLOOP:
		JUMPDEST,
		DUP3,
		ISZERO,
		PUSH1, 30,
		JUMPI,

		DUP2,
		DUP2,
		ADD,
		SWAP2,
		POP,
		SWAP1,
		// off 21

		SWAP2,
		PUSH1, 1,
		SWAP1,
		SUB,
		SWAP2,
		PUSH1, 9,
		JUMP,
		// off 30

		JUMPDEST,
		SWAP2,
		POP,
		POP,
		STOP,
	}

	bytes := make([]byte, len(bytecode))
	for i := 0; i < len(bytecode); i++ {
		bytes[i] = byte(bytecode[i])
	}
	return bytes
}
