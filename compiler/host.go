package compiler

// #include <stdint.h>
// extern int64_t callHostFunc(uint64_t inst, uint64_t opcode, uint64_t* gas, uintptr_t key);
import "C"
import (
	"fmt"
	"sync"
	"unsafe"

	"github.com/ethereum/go-ethereum/common"
	"tinygo.org/x/go-llvm"
)

type HostFunc func(gas *uint64, addr []byte, stackPtr uintptr) int64

type EVMHost interface {
	GetHostFunc(opcode EVMOpcode) HostFunc
}

type DefaultHost struct {
	state       map[common.Address]map[common.Hash]common.Hash
	hostFuncMap map[EVMOpcode]HostFunc
}

// Host function handling
//   - Find the EVMCompiler (or EVMExecutionInstance) from a global map
//     This avoid using unsafe pointer to recoever the execution instance
var executionInstanceMap = make(map[uint64]*EVMCompiler)
var executionInstanceLock sync.Mutex
var executionInstanceCounter uint64

func createExecutionInstance(inst *EVMCompiler) uint64 {
	executionInstanceLock.Lock()
	defer executionInstanceLock.Unlock()
	executionInstanceCounter++
	counter := executionInstanceCounter
	executionInstanceMap[counter] = inst
	return counter
}

func removeExecutionInstance(instId uint64) {
	executionInstanceLock.Lock()
	defer executionInstanceLock.Unlock()
	delete(executionInstanceMap, instId)
}

func getExecutionInstance(instId uint64) (inst *EVMCompiler) {
	executionInstanceLock.Lock()
	defer executionInstanceLock.Unlock()
	return executionInstanceMap[instId]
}

//export callHostFunc
func callHostFunc(inst C.uint64_t, opcode C.uint64_t, gas *C.uint64_t, stackPtr C.uintptr_t) C.int64_t {
	c := getExecutionInstance(uint64(inst))
	if c == nil {
		panic("execution instance not found")
	}

	f := c.host.GetHostFunc(EVMOpcode(opcode))
	if f == nil {
		panic(fmt.Sprintf("host function for opcode %d not found", opcode))
	}

	// TODO: get the addr from exec_inst
	gas1 := uint64(*gas)
	ret := C.int64_t(f(&gas1, common.Address{}.Bytes(), uintptr(stackPtr)))
	*gas = C.uint64_t(gas1)
	return ret
}

// getStackElement returns the slice of the 32-byte stack element
func getStackElement(stackPtr uintptr, idx int) []byte {
	return unsafe.Slice((*byte)(unsafe.Pointer(uintptr(stackPtr)-uintptr(32*idx))), 32)
}

func (c *EVMCompiler) initailizeHostFunctions() {
	i8ptr := llvm.PointerType(c.ctx.Int8Type(), 0)
	i64ptr := llvm.PointerType(c.ctx.Int64Type(), 0)
	c.hostFuncType = llvm.FunctionType(c.ctx.Int64Type(), []llvm.Type{c.ctx.Int64Type(), c.ctx.Int64Type(), i64ptr, i8ptr}, false)
	c.hostFunc = llvm.AddFunction(c.module, "host_func", c.hostFuncType)
	// Set the host function's linkage to External.
	// This prevents LLVM from internalizing it during LTO/IPO passes,
	// ensuring it remains visible outside the module.
	c.hostFunc.SetLinkage(llvm.ExternalLinkage)
	usedArray := llvm.ConstArray(llvm.PointerType(c.hostFuncType, 0), []llvm.Value{c.hostFunc})
	// Place the `llvm.used` global in the "llvm.metadata" section.
	// LLVM recognizes this section specially, which is necessary
	// for the used-function mechanism to work correctly.
	usedGlobal := llvm.AddGlobal(c.module, usedArray.Type(), "llvm.used")
	usedGlobal.SetInitializer(usedArray)
	usedGlobal.SetLinkage(llvm.AppendingLinkage)
	usedGlobal.SetSection("llvm.metadata")
}

func (c *EVMCompiler) addGlobalMappingForHostFunctions() {
	c.engine.AddGlobalMapping(c.hostFunc, unsafe.Pointer(C.callHostFunc))
}

func NewDefaultHost() *DefaultHost {
	h := &DefaultHost{
		state: make(map[common.Address]map[common.Hash]common.Hash),
	}
	h.hostFuncMap = map[EVMOpcode]HostFunc{
		SSTORE: h.Sstore,
		SLOAD:  h.Sload,
	}
	return h
}

func (h *DefaultHost) GetHostFunc(opcode EVMOpcode) HostFunc {
	return h.hostFuncMap[opcode]
}

// A simple Sstore with constant 20000 gas
func (h *DefaultHost) Sstore(gas *uint64, addr []byte, stackPtr uintptr) int64 {
	if *gas < 20000 {
		return int64(ExecutionOutOfGas)
	}
	*gas -= 20000

	addrBytes := common.BytesToAddress(addr)
	if _, ok := h.state[addrBytes]; !ok {
		h.state[addrBytes] = make(map[common.Hash]common.Hash)
	}

	m := h.state[addrBytes]
	keyBytes := common.BytesToHash(getStackElement(stackPtr, 0))
	valueBytes := common.BytesToHash(getStackElement(stackPtr, 1))
	m[keyBytes] = valueBytes

	return int64(ExecutionSuccess)
}

// A simple Sload with constant 5000 gas
func (h *DefaultHost) Sload(gas *uint64, addr []byte, stackPtr uintptr) int64 {
	if *gas < 5000 {
		return int64(ExecutionOutOfGas)
	}
	*gas -= 5000

	addrBytes := common.BytesToAddress(addr)
	keyOrValue := getStackElement(stackPtr, 0)

	m := h.state[addrBytes]
	if m == nil {
		zeroBytes(keyOrValue)
		return int64(ExecutionSuccess)
	}

	keyBytes := common.BytesToHash(keyOrValue)
	valueBytes := m[keyBytes]
	copy(keyOrValue, valueBytes[:])

	return int64(ExecutionSuccess)
}
