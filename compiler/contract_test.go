package compiler

import (
	"fmt"
	"os"
	"testing"

	"github.com/ethereum/go-ethereum/common/hexutil"
)

type ContractTestCase struct {
	name     string
	calldata []byte
}

func readHexFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return hexutil.MustDecode(string(data)), nil
}

func BenchmarkContracts(b *testing.B) {
	testCases := []ContractTestCase{
		{name: "weth"},
	}
	for _, tc := range testCases {
		bytecode, err := readHexFile(fmt.Sprintf("../testdata/ContractHex/%v.rt.hex", tc.name))
		if err != nil {
			b.Fatalf("failed to read contract bytecode: %v", err)
		}
		b.Run(tc.name, func(b *testing.B) {
			benchmarkEVM(b, bytecode, tc.calldata, defaultGaslimit)
		})
	}
}
