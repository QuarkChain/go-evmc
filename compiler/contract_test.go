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
		{name: "airdrop", calldata: hexutil.MustDecode("0x5c975abb")},
		{name: "bswap64", calldata: hexutil.MustDecode(
			"0xff2f79f10000000000000000000000000000000000000000000000000000000000000102")},
		{name: "bswap64_opt", calldata: hexutil.MustDecode(
			"0xff2f79f10000000000000000000000000000000000000000000000000000000000000102")},
		{name: "counter", calldata: hexutil.MustDecode("0xd09de08a")},
		{name: "erc20_transfer", calldata: hexutil.MustDecode("0x30627b7c")},
		{name: "hash_10k", calldata: hexutil.MustDecode("0x30627b7c")},
		{name: "push0_proxy"},
		{name: "usdc_proxy"},
		{name: "snailtracer", calldata: hexutil.MustDecode("0x30627b7c")},

		// The following contracts are expected to revert.
		// revmc also skips these benchmarks:
		// {name: "fiat_token"},
		// {name: "seaport"},
		// {name: "uniswap_v2_pair"},
		// {name: "univ2_router"},

		{name: "poseidon_t2", calldata: hexutil.MustDecode("0x9d036e710000000000000000000000000000000000000000000000000000000000000000")},
		{name: "poseidon_t5", calldata: hexutil.MustDecode("0x8709cd8c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000003")},
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
