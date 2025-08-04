package compiler

import (
	"unsafe"
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
