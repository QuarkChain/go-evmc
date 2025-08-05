package compiler

// #include <stdint.h>
// typedef void (*func)(void* memory, void* stack, uint64_t gas);
// static void execute(uint64_t f, void* memory, void* stack, uint64_t gas) { ((func)f)(memory, stack, gas); }
import "C"
import (
	"fmt"
	"os"
	"unsafe"

	"github.com/holiman/uint256"
	"tinygo.org/x/go-llvm"
)

type EVMCompiler struct {
	ctx       llvm.Context
	module    llvm.Module
	builder   llvm.Builder
	target    llvm.Target
	machine   llvm.TargetMachine
	stackType llvm.Type
	memType   llvm.Type
	engine    *llvm.ExecutionEngine
	funcPtr   uint64
}

type EVMOpcode uint8

const (
	STOP           EVMOpcode = 0x00
	ADD            EVMOpcode = 0x01
	MUL            EVMOpcode = 0x02
	SUB            EVMOpcode = 0x03
	DIV            EVMOpcode = 0x04
	SDIV           EVMOpcode = 0x05
	MOD            EVMOpcode = 0x06
	SMOD           EVMOpcode = 0x07
	ADDMOD         EVMOpcode = 0x08
	MULMOD         EVMOpcode = 0x09
	EXP            EVMOpcode = 0x0A
	SIGNEXTEND     EVMOpcode = 0x0B
	LT             EVMOpcode = 0x10
	GT             EVMOpcode = 0x11
	SLT            EVMOpcode = 0x12
	SGT            EVMOpcode = 0x13
	EQ             EVMOpcode = 0x14
	ISZERO         EVMOpcode = 0x15
	AND            EVMOpcode = 0x16
	OR             EVMOpcode = 0x17
	XOR            EVMOpcode = 0x18
	NOT            EVMOpcode = 0x19
	BYTE           EVMOpcode = 0x1A
	SHL            EVMOpcode = 0x1B
	SHR            EVMOpcode = 0x1C
	SAR            EVMOpcode = 0x1D
	SHA3           EVMOpcode = 0x20
	ADDRESS        EVMOpcode = 0x30
	BALANCE        EVMOpcode = 0x31
	ORIGIN         EVMOpcode = 0x32
	CALLER         EVMOpcode = 0x33
	CALLVALUE      EVMOpcode = 0x34
	CALLDATALOAD   EVMOpcode = 0x35
	CALLDATASIZE   EVMOpcode = 0x36
	CALLDATACOPY   EVMOpcode = 0x37
	CODESIZE       EVMOpcode = 0x38
	CODECOPY       EVMOpcode = 0x39
	GASPRICE       EVMOpcode = 0x3A
	EXTCODESIZE    EVMOpcode = 0x3B
	EXTCODECOPY    EVMOpcode = 0x3C
	RETURNDATASIZE EVMOpcode = 0x3D
	RETURNDATACOPY EVMOpcode = 0x3E
	EXTCODEHASH    EVMOpcode = 0x3F
	BLOCKHASH      EVMOpcode = 0x40
	COINBASE       EVMOpcode = 0x41
	TIMESTAMP      EVMOpcode = 0x42
	NUMBER         EVMOpcode = 0x43
	PREVRANDAO     EVMOpcode = 0x44
	GASLIMIT       EVMOpcode = 0x45
	CHAINID        EVMOpcode = 0x46
	SELFBALANCE    EVMOpcode = 0x47
	BASEFEE        EVMOpcode = 0x48
	POP            EVMOpcode = 0x50
	MLOAD          EVMOpcode = 0x51
	MSTORE         EVMOpcode = 0x52
	MSTORE8        EVMOpcode = 0x53
	SLOAD          EVMOpcode = 0x54
	SSTORE         EVMOpcode = 0x55
	JUMP           EVMOpcode = 0x56
	JUMPI          EVMOpcode = 0x57
	PC             EVMOpcode = 0x58
	MSIZE          EVMOpcode = 0x59
	GAS            EVMOpcode = 0x5A
	JUMPDEST       EVMOpcode = 0x5B
	LOG0           EVMOpcode = 0xA0
	LOG4           EVMOpcode = 0xA4
	CREATE         EVMOpcode = 0xF0
	CALL           EVMOpcode = 0xF1
	CALLCODE       EVMOpcode = 0xF2
	RETURN         EVMOpcode = 0xF3
	DELEGATECALL   EVMOpcode = 0xF4
	CREATE2        EVMOpcode = 0xF5
	STATICCALL     EVMOpcode = 0xFA
	REVERT         EVMOpcode = 0xFD
	INVALID        EVMOpcode = 0xFE
	SELFDESTRUCT   EVMOpcode = 0xFF
)

const (
	PUSH1 EVMOpcode = 0x60 + iota
	PUSH2
	PUSH3
	PUSH4
	PUSH5
	PUSH6
	PUSH7
	PUSH8
	PUSH9
	PUSH10
	PUSH11
	PUSH12
	PUSH13
	PUSH14
	PUSH15
	PUSH16
	PUSH17
	PUSH18
	PUSH19
	PUSH20
	PUSH21
	PUSH22
	PUSH23
	PUSH24
	PUSH25
	PUSH26
	PUSH27
	PUSH28
	PUSH29
	PUSH30
	PUSH31
	PUSH32
)

const (
	DUP1 EVMOpcode = 0x80 + iota
	DUP2
	DUP3
	DUP4
	DUP5
	DUP6
	DUP7
	DUP8
	DUP9
	DUP10
	DUP11
	DUP12
	DUP13
	DUP14
	DUP15
	DUP16
)

const (
	SWAP1 EVMOpcode = 0x90 + iota
	SWAP2
	SWAP3
	SWAP4
	SWAP5
	SWAP6
	SWAP7
	SWAP8
	SWAP9
	SWAP10
	SWAP11
	SWAP12
	SWAP13
	SWAP14
	SWAP15
	SWAP16
)

type EVMInstruction struct {
	Opcode EVMOpcode
	Data   []byte
	PC     uint64
}

type EVMExecutionResult struct {
	Stack  [][32]byte
	Memory []byte
	Status ExecutionStatus
	Error  error
}

type ExecutionStatus int

const (
	ExecutionSuccess ExecutionStatus = iota
	ExecutionRevert
	ExecutionError
)

func NewEVMCompiler() *EVMCompiler {
	llvm.InitializeNativeTarget()
	llvm.InitializeNativeAsmPrinter()

	ctx := llvm.NewContext()
	module := ctx.NewModule("evm_module")
	builder := ctx.NewBuilder()

	target, err := llvm.GetTargetFromTriple(llvm.DefaultTargetTriple())
	if err != nil {
		panic(fmt.Sprintf("Failed to get target: %s", err))
	}

	machine := target.CreateTargetMachine(
		llvm.DefaultTargetTriple(), "generic", "",
		llvm.CodeGenLevelDefault, llvm.RelocDefault, llvm.CodeModelDefault)

	return &EVMCompiler{
		ctx:       ctx,
		module:    module,
		builder:   builder,
		target:    target,
		machine:   machine,
		stackType: llvm.PointerType(ctx.IntType(256), 0),
		memType:   llvm.PointerType(ctx.Int8Type(), 0),
	}
}

func (c *EVMCompiler) Dispose() {
	c.builder.Dispose()
	// c.module.Dispose() TODO: segfault after execute() - does engine take the ownership of the module
	c.ctx.Dispose()
}

func (c *EVMCompiler) ParseBytecode(bytecode []byte) ([]EVMInstruction, error) {
	instructions := make([]EVMInstruction, 0)
	pc := uint64(0)

	for pc < uint64(len(bytecode)) {
		opcode := EVMOpcode(bytecode[pc])
		instr := EVMInstruction{
			Opcode: opcode,
			PC:     pc,
		}

		if opcode >= PUSH1 && opcode <= PUSH32 {
			dataSize := int(opcode - PUSH1 + 1)
			if pc+uint64(dataSize) >= uint64(len(bytecode)) {
				return nil, fmt.Errorf("invalid PUSH instruction at PC %d", pc)
			}
			instr.Data = make([]byte, dataSize)
			copy(instr.Data, bytecode[pc+1:pc+1+uint64(dataSize)])
			pc += uint64(dataSize)
		}
		instructions = append(instructions, instr)
		pc++
	}

	return instructions, nil
}

func (c *EVMCompiler) CompileBytecode(bytecode []byte) (llvm.Module, error) {
	// Use static analysis approach by default
	return c.CompileBytecodeStatic(bytecode)
}

func (c *EVMCompiler) pushStack(stack, stackPtr, value llvm.Value) {
	stackPtrVal := c.builder.CreateLoad(c.ctx.Int32Type(), stackPtr, "stack_ptr_val")
	stackElem := c.builder.CreateGEP(c.ctx.IntType(256), stack, []llvm.Value{stackPtrVal}, "stack_elem")
	c.builder.CreateStore(value, stackElem)
	newStackPtr := c.builder.CreateAdd(stackPtrVal, llvm.ConstInt(c.ctx.Int32Type(), 1, false), "new_stack_ptr")
	c.builder.CreateStore(newStackPtr, stackPtr)
}

func (c *EVMCompiler) popStack(stack, stackPtr llvm.Value) llvm.Value {
	stackPtrVal := c.builder.CreateLoad(c.ctx.Int32Type(), stackPtr, "stack_ptr_val")
	newStackPtr := c.builder.CreateSub(stackPtrVal, llvm.ConstInt(c.ctx.Int32Type(), 1, false), "new_stack_ptr")
	c.builder.CreateStore(newStackPtr, stackPtr)
	stackElem := c.builder.CreateGEP(c.ctx.IntType(256), stack, []llvm.Value{newStackPtr}, "stack_elem")
	return c.builder.CreateLoad(c.ctx.IntType(256), stackElem, "stack_value")
}

func (c *EVMCompiler) createUint256ConstantFromBytes(data []byte) llvm.Value {
	if len(data) == 0 {
		return llvm.ConstInt(c.ctx.IntType(256), 0, false)
	}

	v := uint256.NewInt(0).SetBytes(data)

	return llvm.ConstIntFromString(c.ctx.IntType(256), v.String(), 10)
}

func (c *EVMCompiler) loadFromMemory(memory, offset llvm.Value) llvm.Value {
	offsetTrunc := c.builder.CreateTrunc(offset, c.ctx.Int64Type(), "mem_offset")
	memPtr := c.builder.CreateGEP(c.ctx.Int8Type(), memory, []llvm.Value{offsetTrunc}, "mem_ptr")

	value := llvm.ConstInt(c.ctx.IntType(256), 0, false)
	for i := 0; i < 32; i++ {
		byteOffset := llvm.ConstInt(c.ctx.Int64Type(), uint64(i), false)
		bytePtr := c.builder.CreateGEP(c.ctx.Int8Type(), memPtr, []llvm.Value{byteOffset}, "byte_ptr")
		byteVal := c.builder.CreateLoad(c.ctx.Int8Type(), bytePtr, "byte_val")
		byteValExt := c.builder.CreateZExt(byteVal, c.ctx.IntType(256), "byte_ext")
		shift := c.builder.CreateShl(byteValExt, llvm.ConstInt(c.ctx.IntType(256), uint64(8*(31-i)), false), "byte_shift")
		value = c.builder.CreateOr(value, shift, "mem_value")
	}

	return value
}

func (c *EVMCompiler) storeToMemory(memory, offset, value llvm.Value) {
	offsetTrunc := c.builder.CreateTrunc(offset, c.ctx.Int64Type(), "mem_offset")
	memPtr := c.builder.CreateGEP(c.ctx.Int8Type(), memory, []llvm.Value{offsetTrunc}, "mem_ptr")

	for i := 0; i < 32; i++ {
		shift := llvm.ConstInt(c.ctx.IntType(256), uint64(8*(31-i)), false)
		shiftedValue := c.builder.CreateLShr(value, shift, "shifted_value")
		byteVal := c.builder.CreateTrunc(shiftedValue, c.ctx.Int8Type(), "byte_val")

		byteOffset := llvm.ConstInt(c.ctx.Int64Type(), uint64(i), false)
		bytePtr := c.builder.CreateGEP(c.ctx.Int8Type(), memPtr, []llvm.Value{byteOffset}, "byte_ptr")
		c.builder.CreateStore(byteVal, bytePtr)
	}
}

func (c *EVMCompiler) storeByteToMemory(memory, offset, value llvm.Value) {
	offsetTrunc := c.builder.CreateTrunc(offset, c.ctx.Int64Type(), "mem_offset")
	memPtr := c.builder.CreateGEP(c.ctx.Int8Type(), memory, []llvm.Value{offsetTrunc}, "mem_ptr")
	byteVal := c.builder.CreateTrunc(value, c.ctx.Int8Type(), "byte_val")
	c.builder.CreateStore(byteVal, memPtr)
}

func (c *EVMCompiler) EmitObjectFile(filename string) error {
	mem, err := c.machine.EmitToMemoryBuffer(c.module, llvm.ObjectFile)
	if err != nil {
		return err
	}

	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	_, err = file.Write(mem.Bytes())
	return err
}

func (c *EVMCompiler) EmitLLVMIR(filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	_, err = file.WriteString(c.module.String())
	return err
}

func (c *EVMCompiler) OptimizeModule() {
	passManager := llvm.NewFunctionPassManagerForModule(c.module)
	defer passManager.Dispose()

	passManager.InitializeFunc()
	for fn := c.module.FirstFunction(); !fn.IsNil(); fn = llvm.NextFunction(fn) {
		passManager.RunFunc(fn)
	}
	passManager.FinalizeFunc()
}

func (c *EVMCompiler) CompileAndOptimize(bytecode []byte) error {
	// Use static analysis approach by default
	return c.CompileAndOptimizeStatic(bytecode)
}

func (c *EVMCompiler) CreateExecutionEngine() error {
	engine, err := llvm.NewJITCompiler(c.module, 2)
	if err != nil {
		return fmt.Errorf("failed to create JIT compiler: %v", err)
	}

	// Get function pointer for direct execution
	funcPtr := engine.GetFunctionAddress("execute")
	if funcPtr == 0 {
		return fmt.Errorf("execute function address not found")
	}
	c.engine = &engine
	c.funcPtr = funcPtr
	return nil
}

// Execute the compiled EVM code
func (c *EVMCompiler) Execute() (*EVMExecutionResult, error) {
	// Prepare execution environment
	// TODO: support dynamic size growth
	const stackSize = 1024
	const memorySize = 1024 * 32

	stack := make([][32]byte, stackSize)
	memory := make([]byte, memorySize)

	// Execute using function pointer
	err := c.callNativeFunction(c.funcPtr, unsafe.Pointer(&memory[0]), unsafe.Pointer(&stack[0]), uint64(1000000))
	if err != nil {
		return &EVMExecutionResult{
			Stack:  nil,
			Memory: nil,
			Status: ExecutionError,
			Error:  err,
		}, nil
	}

	// Process results same as ExecuteCompiled
	stackDepth := 0
	for i := stackSize - 1; i >= 0; i-- {
		allZero := true
		for j := 0; j < 32; j++ {
			if stack[i][j] != 0 {
				allZero = false
				break
			}
		}
		if !allZero {
			stackDepth = i + 1
			break
		}
	}

	resultStack := make([][32]byte, stackDepth)
	copy(resultStack, stack[:stackDepth])

	memoryUsed := len(memory)
	for i := len(memory) - 1; i >= 0; i-- {
		if memory[i] != 0 {
			memoryUsed = i + 1
			break
		}
	}

	return &EVMExecutionResult{
		Stack:  resultStack,
		Memory: memory[:memoryUsed],
		Status: ExecutionSuccess,
		Error:  nil,
	}, nil
}

// ExecuteCompiled executes compiled EVM code using function pointer for better performance
func (c *EVMCompiler) ExecuteCompiled(bytecode []byte) (*EVMExecutionResult, error) {
	// Compile if needed
	err := c.CompileAndOptimize(bytecode)
	if err != nil {
		return nil, fmt.Errorf("compilation failed: %v", err)
	}

	// Create execution engine
	err = c.CreateExecutionEngine()
	if err != nil {
		return nil, err
	}

	return c.Execute()
}

// Helper function to call native function pointer (requires CGO)
func (c *EVMCompiler) callNativeFunction(funcPtr uint64, memory, stack unsafe.Pointer, gas uint64) error {
	C.execute(C.uint64_t(funcPtr), memory, stack, C.uint64_t(gas))
	return nil
}
