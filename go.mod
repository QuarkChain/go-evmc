module github.com/QuarkChain/go-evmc

go 1.23.1

require (
	github.com/ethereum/go-ethereum v1.16.2
	github.com/holiman/uint256 v1.3.2
	tinygo.org/x/go-llvm v0.0.0-20250422114502-b8f170971e74
)

require (
	golang.org/x/crypto v0.36.0 // indirect
	golang.org/x/sys v0.31.0 // indirect
)

replace tinygo.org/x/go-llvm => ../go-llvm
