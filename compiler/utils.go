package compiler

import (
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
	return a
}
