package compiler

/*
#include <byteswap.h>
#include <stdint.h>

void u256_byte_swap(void* in_ptr, void* out_ptr) {
	uint64_t* in = (uint64_t*)in_ptr;
	uint64_t* out = (uint64_t*)out_ptr;
	out[3] = bswap_64(in[0]);
	out[2] = bswap_64(in[1]);
	out[1] = bswap_64(in[2]);
	out[0] = bswap_64(in[3]);
}

void u256_byte_swap_inplace(void* buf_ptr) {
	uint64_t* buf = (uint64_t*)buf_ptr;
	uint64_t t;
	t = buf[3];
	buf[3] = bswap_64(buf[0]);
	buf[0] = bswap_64(t);
	t = buf[2];
	buf[2] = bswap_64(buf[1]);
	buf[1] = bswap_64(t);
}
*/
import "C"
import (
	"encoding/binary"
	"unsafe"

	"github.com/ethereum/go-ethereum/params"
	"github.com/holiman/uint256"
)

func Reverse(b []byte) []byte {
	out := make([]byte, len(b))
	for i := range b {
		out[len(b)-1-i] = b[i]
	}
	return out
}

func ReverseInplace(b []byte) []byte {
	for i, j := 0, len(b)-1; i < j; i, j = i+1, j-1 {
		b[i], b[j] = b[j], b[i]
	}
	return b
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

func FromMachineToBigInplace(b []byte) []byte {
	if IsMachineBigEndian() {
		return b
	}
	return ReverseInplace(b)
}

func FromBigToMachineInplace(b []byte) []byte {
	if IsMachineBigEndian() {
		return b
	}
	return ReverseInplace(b)
}

func CopyFromBigToMachine(b []byte, out []byte) {
	if IsMachineBigEndian() {
		copy(out, b)
	}
	for i := range b {
		out[len(b)-1-i] = b[i]
	}
}

func CopyFromMachineToBig(b []byte, out []byte) {
	CopyFromBigToMachine(b, out)
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

func zeroBytes(buf []byte) {
	for i := 0; i < len(buf); i++ {
		buf[i] = 0
	}
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
	bytecode := []OpCode{
		PUSH4, OpCode(nbs[0]), OpCode(nbs[1]), OpCode(nbs[2]), OpCode(nbs[3]),
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

// Swap a uint256 byte order from big to little or little to big.
// Note that inplace swap is not supported.
func uint256ByteSwap(bufIn []byte, bufOut []byte) {
	_ = bufIn[31] // bounds check hint to compiler; see golang.org/issue/14808
	_ = bufOut[31]
	C.u256_byte_swap(unsafe.Pointer(&bufIn[0]), unsafe.Pointer(&bufOut[0]))
}

// Swap a uint256 byte order from big to little or little to big.
func uint256ByteSwapInplace(buf []byte) {
	_ = buf[31] // bounds check hint to compiler; see golang.org/issue/14808
	C.u256_byte_swap_inplace(unsafe.Pointer(&buf[0]))
}

// Below are from Geth.  See the license of Geth.

// memoryGasCost calculates the quadratic gas for memory expansion. It does so
// only for the memory region that is expanded, not the total memory.
func memoryGasCost(mem *Memory, newMemSize uint64) (uint64, error) {
	if newMemSize == 0 {
		return 0, nil
	}
	// The maximum that will fit in a uint64 is max_word_count - 1. Anything above
	// that will result in an overflow. Additionally, a newMemSize which results in
	// a newMemSizeWords larger than 0xFFFFFFFF will cause the square operation to
	// overflow. The constant 0x1FFFFFFFE0 is the highest number that can be used
	// without overflowing the gas calculation.
	if newMemSize > 0x1FFFFFFFE0 {
		return 0, ErrGasUintOverflow
	}
	newMemSizeWords := toWordSize(newMemSize)
	newMemSize = newMemSizeWords * 32

	if newMemSize > uint64(mem.Len()) {
		square := newMemSizeWords * newMemSizeWords
		linCoef := newMemSizeWords * params.MemoryGas
		quadCoef := square / params.QuadCoeffDiv
		newTotalFee := linCoef + quadCoef

		fee := newTotalFee - mem.lastGasCost
		mem.lastGasCost = newTotalFee

		return fee, nil
	}
	return 0, nil
}

func getJumpTable(chainRules params.Rules, optExtraEips []int) (*JumpTable, []int, error) {
	// If jump table was not initialised we set the default one.
	var table *JumpTable
	switch {
	case chainRules.IsOsaka:
		table = &osakaInstructionSet
	case chainRules.IsVerkle:
		// TODO replace with proper instruction set when fork is specified
		table = &verkleInstructionSet
	case chainRules.IsPrague:
		table = &pragueInstructionSet
	case chainRules.IsCancun:
		table = &cancunInstructionSet
	case chainRules.IsShanghai:
		table = &shanghaiInstructionSet
	case chainRules.IsMerge:
		table = &mergeInstructionSet
	case chainRules.IsLondon:
		table = &londonInstructionSet
	case chainRules.IsBerlin:
		table = &berlinInstructionSet
	case chainRules.IsIstanbul:
		table = &istanbulInstructionSet
	case chainRules.IsConstantinople:
		table = &constantinopleInstructionSet
	case chainRules.IsByzantium:
		table = &byzantiumInstructionSet
	case chainRules.IsEIP158:
		table = &spuriousDragonInstructionSet
	case chainRules.IsEIP150:
		table = &tangerineWhistleInstructionSet
	case chainRules.IsHomestead:
		table = &homesteadInstructionSet
	default:
		table = &frontierInstructionSet
	}
	var extraEips []int
	if len(optExtraEips) > 0 {
		// Deep-copy jumptable to prevent modification of opcodes in other tables
		table = copyJumpTable(table)
	}
	for _, eip := range optExtraEips {
		if err := EnableEIP(eip, table); err != nil {
			return nil, nil, err
		} else {
			extraEips = append(extraEips, eip)
		}
	}
	return table, extraEips, nil
}
