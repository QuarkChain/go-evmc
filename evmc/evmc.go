package evmc

import (
	"unsafe"

	"tinygo.org/x/go-llvm"
)

// #include <stdint.h>
// #include <stdlib.h>
// 
// typedef struct evmc_uint256be {
//     uint8_t bytes[32];
// } evmc_uint256be;
//
// typedef struct evmc_address {
//     uint8_t bytes[20];
// } evmc_address;
//
// enum evmc_status_code {
//     EVMC_SUCCESS = 0,
//     EVMC_FAILURE = 1,
//     EVMC_REVERT = 2,
//     EVMC_OUT_OF_GAS = 3,
//     EVMC_INVALID_INSTRUCTION = 4,
//     EVMC_UNDEFINED_INSTRUCTION = 5,
//     EVMC_STACK_OVERFLOW = 6,
//     EVMC_STACK_UNDERFLOW = 7,
//     EVMC_BAD_JUMP_DESTINATION = 8,
//     EVMC_INVALID_MEMORY_ACCESS = 9,
//     EVMC_CALL_DEPTH_EXCEEDED = 10,
//     EVMC_STATIC_MODE_VIOLATION = 11,
//     EVMC_PRECOMPILE_FAILURE = 12,
//     EVMC_CONTRACT_VALIDATION_FAILURE = 13,
//     EVMC_ARGUMENT_OUT_OF_RANGE = 14,
//     EVMC_WASM_UNREACHABLE_INSTRUCTION = 15,
//     EVMC_WASM_TRAP = 16,
//     EVMC_INSUFFICIENT_BALANCE = 17,
//     EVMC_INTERNAL_ERROR = -1,
//     EVMC_REJECTED = -2,
//     EVMC_OUT_OF_MEMORY = -3
// };
//
// typedef struct evmc_result {
//     enum evmc_status_code status_code;
//     int64_t gas_left;
//     const uint8_t* output_data;
//     size_t output_size;
//     void (*release)(const struct evmc_result* result);
//     evmc_address create_address;
//     uint8_t padding[4];
// } evmc_result;
//
// typedef struct evmc_message {
//     int32_t kind;
//     uint32_t flags;
//     int32_t depth;
//     int64_t gas;
//     evmc_address recipient;
//     evmc_address sender;
//     const uint8_t* input_data;
//     size_t input_size;
//     evmc_uint256be value;
//     evmc_uint256be create2_salt;
//     evmc_address code_address;
// } evmc_message;
//
// typedef struct evmc_tx_context {
//     evmc_uint256be tx_gas_price;
//     evmc_address tx_origin;
//     evmc_address block_coinbase;
//     int64_t block_number;
//     int64_t block_timestamp;
//     int64_t block_gas_limit;
//     evmc_uint256be block_prev_randao;
//     evmc_uint256be chain_id;
//     evmc_uint256be block_base_fee;
// } evmc_tx_context;
//
// typedef struct evmc_host_context evmc_host_context;
//
// typedef evmc_uint256be (*evmc_get_storage_fn)(evmc_host_context* context,
//                                               const evmc_address* addr,
//                                               const evmc_uint256be* key);
//
// typedef enum evmc_storage_status (*evmc_set_storage_fn)(evmc_host_context* context,
//                                                         const evmc_address* addr,
//                                                         const evmc_uint256be* key,
//                                                         const evmc_uint256be* value);
//
// typedef evmc_uint256be (*evmc_get_balance_fn)(evmc_host_context* context,
//                                               const evmc_address* addr);
//
// typedef size_t (*evmc_get_code_size_fn)(evmc_host_context* context,
//                                         const evmc_address* addr);
//
// typedef evmc_uint256be (*evmc_get_code_hash_fn)(evmc_host_context* context,
//                                                 const evmc_address* addr);
//
// typedef size_t (*evmc_copy_code_fn)(evmc_host_context* context,
//                                     const evmc_address* addr,
//                                     size_t code_offset,
//                                     uint8_t* buffer_data,
//                                     size_t buffer_size);
//
// typedef bool (*evmc_account_exists_fn)(evmc_host_context* context,
//                                        const evmc_address* addr);
//
// typedef evmc_uint256be (*evmc_get_block_hash_fn)(evmc_host_context* context,
//                                                  int64_t number);
//
// typedef void (*evmc_emit_log_fn)(evmc_host_context* context,
//                                  const evmc_address* addr,
//                                  const uint8_t* data,
//                                  size_t data_size,
//                                  const evmc_uint256be topics[],
//                                  size_t topics_count);
//
// typedef evmc_result (*evmc_call_fn)(evmc_host_context* context,
//                                     const evmc_message* msg);
//
// typedef evmc_tx_context (*evmc_get_tx_context_fn)(evmc_host_context* context);
//
// typedef struct evmc_host_interface {
//     evmc_account_exists_fn account_exists;
//     evmc_get_storage_fn get_storage;
//     evmc_set_storage_fn set_storage;
//     evmc_get_balance_fn get_balance;
//     evmc_get_code_size_fn get_code_size;
//     evmc_get_code_hash_fn get_code_hash;
//     evmc_copy_code_fn copy_code;
//     evmc_call_fn call;
//     evmc_get_tx_context_fn get_tx_context;
//     evmc_get_block_hash_fn get_block_hash;
//     evmc_emit_log_fn emit_log;
// } evmc_host_interface;
//
// typedef evmc_result (*evmc_execute_fn)(struct evmc_vm* vm,
//                                        const evmc_host_interface* host,
//                                        evmc_host_context* context,
//                                        enum evmc_revision rev,
//                                        const evmc_message* msg,
//                                        const uint8_t* code,
//                                        size_t code_size);
//
// typedef struct evmc_vm {
//     const int abi_version;
//     const char* const name;
//     const char* const version;
//     evmc_execute_fn execute;
//     void* reserved[16];
// } evmc_vm;
//
import "C"

type EVMCAddress [20]byte
type EVMCBytes32 [32]byte

type EVMCResult struct {
	StatusCode int32
	GasLeft    int64
	Output     []byte
}

type EVMCMessage struct {
	Kind        int32
	Flags       uint32
	Depth       int32
	Gas         int64
	Recipient   EVMCAddress
	Sender      EVMCAddress
	InputData   []byte
	Value       EVMCBytes32
	Create2Salt EVMCBytes32
	CodeAddress EVMCAddress
}

type EVMCHostInterface struct {
	GetStorage    func(addr EVMCAddress, key EVMCBytes32) EVMCBytes32
	SetStorage    func(addr EVMCAddress, key EVMCBytes32, value EVMCBytes32) int
	GetBalance    func(addr EVMCAddress) EVMCBytes32
	GetCodeSize   func(addr EVMCAddress) int
	GetCodeHash   func(addr EVMCAddress) EVMCBytes32
	AccountExists func(addr EVMCAddress) bool
	GetBlockHash  func(number int64) EVMCBytes32
	EmitLog       func(addr EVMCAddress, data []byte, topics []EVMCBytes32)
	Call          func(msg EVMCMessage) EVMCResult
}

type EVMCEngine struct {
	engine llvm.ExecutionEngine
	module llvm.Module
}

func NewEVMCEngine(module llvm.Module) (*EVMCEngine, error) {
	engine, err := llvm.NewJITCompiler(module, 3)
	if err != nil {
		return nil, err
	}

	return &EVMCEngine{
		engine: engine,
		module: module,
	}, nil
}

func (e *EVMCEngine) Execute(host *EVMCHostInterface, msg *EVMCMessage, code []byte) EVMCResult {
	executeFunc := e.engine.FindFunction("execute")
	if executeFunc.IsNil() {
		return EVMCResult{
			StatusCode: int32(C.EVMC_INTERNAL_ERROR),
			GasLeft:    0,
			Output:     nil,
		}
	}

	stack := make([]EVMCBytes32, 1024)
	memory := make([]byte, 1024*32)

	stackPtr := (*C.uchar)(unsafe.Pointer(&stack[0]))
	memoryPtr := (*C.uchar)(unsafe.Pointer(&memory[0]))
	codePtr := (*C.uchar)(unsafe.Pointer(&code[0]))
	gas := C.int64_t(msg.Gas)

	args := []llvm.GenericValue{
		llvm.NewGenericValueFromPointer(unsafe.Pointer(memoryPtr)),
		llvm.NewGenericValueFromPointer(unsafe.Pointer(stackPtr)),
		llvm.NewGenericValueFromPointer(unsafe.Pointer(codePtr)),
		llvm.NewGenericValueFromInt(llvm.Int64Type(), uint64(gas), false),
	}

	e.engine.RunFunction(executeFunc, args)

	return EVMCResult{
		StatusCode: int32(C.EVMC_SUCCESS),
		GasLeft:    msg.Gas,
		Output:     nil,
	}
}

func (e *EVMCEngine) Dispose() {
	e.engine.Dispose()
}