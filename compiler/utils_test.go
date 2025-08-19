package compiler

import (
	"math/rand"
	"testing"
)

func TestU256ByteSwap(t *testing.T) {
	buf := make([]byte, 32)
	for i := byte(0); int(i) < len(buf); i++ {
		buf[i] = i
	}

	buf1 := make([]byte, 32)
	uint256ByteSwap(buf, buf1)
	for i := byte(0); int(i) < len(buf1); i++ {
		if buf1[i] != 31-i {
			t.Fatalf("Expected swap at %d, %d, got %d", i, buf1[i], 31-i)
		}
	}

	uint256ByteSwapInplace(buf)
	for i := byte(0); int(i) < len(buf); i++ {
		if buf[i] != 31-i {
			t.Fatalf("Expected swap at %d, %d, got %d", i, buf[i], 31-i)
		}
	}
}

const numSamples = 1024

var (
	b32Samples [numSamples][]byte

	_ = initSamples()
)

func initSamples() bool {
	rnd := rand.New(rand.NewSource(0)) // #nosec G404

	newRandB32 := func() []byte {
		z := make([]byte, 32)
		for i := 0; i < 32; i++ {
			z[i] = byte(rnd.Int())
		}
		return z
	}

	for i := 0; i < numSamples; i++ {
		b32Samples[i] = newRandB32()
	}

	return true
}

func BenchmarkByteSwap(b *testing.B) {
	benchmarkReverse := func(b *testing.B, samples *[numSamples][]byte) (flag bool) {
		for j := 0; j < b.N; j += numSamples {
			for i := 0; i < numSamples; i++ {
				_ = Reverse(samples[i])
			}
		}
		return
	}

	benchmarkReverseInplace := func(b *testing.B, samples *[numSamples][]byte) (flag bool) {
		for j := 0; j < b.N; j += numSamples {
			for i := 0; i < numSamples; i++ {
				ReverseInplace(samples[i])
			}
		}
		return
	}

	benchmarkU256ByteSwap := func(b *testing.B, samples *[numSamples][]byte) (flag bool) {
		for j := 0; j < b.N; j += numSamples {
			for i := 0; i < numSamples; i++ {
				out := make([]byte, 32)
				uint256ByteSwap(samples[i], out)
			}
		}
		return
	}

	benchmarkU256ByteSwapInplace := func(b *testing.B, samples *[numSamples][]byte) (flag bool) {
		for j := 0; j < b.N; j += numSamples {
			for i := 0; i < numSamples; i++ {
				uint256ByteSwapInplace(samples[i])
			}
		}
		return
	}

	b.Run("Reverse", func(b *testing.B) { benchmarkReverse(b, &b32Samples) })
	b.Run("ReverseInplace", func(b *testing.B) { benchmarkReverseInplace(b, &b32Samples) })
	b.Run("U256ByteSwap", func(b *testing.B) { benchmarkU256ByteSwap(b, &b32Samples) })
	b.Run("U256ByteSwapInplace", func(b *testing.B) { benchmarkU256ByteSwapInplace(b, &b32Samples) })
}
