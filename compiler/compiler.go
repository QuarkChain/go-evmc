package compiler

import (
	"fmt"
	"math/big"
	"os"
	"time"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/vm/runtime"
	"github.com/ethereum/go-ethereum/params"
	"github.com/holiman/uint256"
	"tinygo.org/x/go-llvm"
)

type EVMCompiler struct {
	ctx            llvm.Context
	module         llvm.Module
	builder        llvm.Builder
	target         llvm.Target
	machine        llvm.TargetMachine
	stackType      llvm.Type
	memType        llvm.Type
	initSectionGas uint64

	contractAddress common.Address
	codeHash        common.Hash

	executor *EVMExecutor

	hostFuncType llvm.Type
	hostFunc     llvm.Value

	table     *JumpTable
	extraEips []int
}

type EVMInstruction struct {
	Opcode   OpCode
	Data     []byte
	PC       uint64
	MinStack int // Minimum stack after static analysis
}

type EVMExecutionResult struct {
	Stack        [][32]byte
	Memory       *Memory
	Status       ExecutionStatus
	GasUsed      uint64
	GasLimit     uint64
	GasRemaining uint64
}

type EVMExecutionOpts struct {
	Config   *runtime.Config
	GasLimit uint64
	Input    []byte
}

var defaultCompilationAddress = common.HexToAddress("cccccccccccccccccccccccccccccccccccccccc")
var defaultOriginAddress = common.HexToAddress("cccccccccccccccccccccccccccccccccccccccd")
var defaultCallerAddress = common.HexToAddress("ccccccccccccccccccccccccccccccccccccccce")
var defaultCoinbaseAddress = common.HexToAddress("cccccccccccccccccccccccccccccccccccccccf")
var defaultRANDAO = common.HexToHash("cccccccccccccccccccccccccccccccccccccccg")
var defaultBlobHashes = []common.Hash{{0x11}}
var defaultTime = uint64(time.Now().Unix())
var defaultInput = uint256.NewInt(200).Bytes32()
var defaultCallValue = big.NewInt(100)
var defaultGasPrice = big.NewInt(101)
var defaultBlockNumber = big.NewInt(102)
var defaultBaseFee = big.NewInt(103)
var defaultBlobBaseFee = big.NewInt(104)

type EVMCompilationOpts struct {
	DisableGas                        bool
	DisableSectionGasOptimization     bool
	DisableStackUnderflowOptimization bool
	ContractAddress                   common.Address // The address of the contract to be compiled
	ChainRules                        params.Rules
	ExtraEips                         []int
}

func DefaultEVMCompilationOpts() *EVMCompilationOpts {
	return &EVMCompilationOpts{
		DisableGas:                    false,
		DisableSectionGasOptimization: false,
		ContractAddress:               defaultCompilationAddress,
		ChainRules:                    params.Rules{IsOsaka: true},
	}
}

func GetContractFunction(codeHash common.Hash) string {
	return fmt.Sprintf("contract_%s", codeHash.Hex())
}

type ExecutionStatus int

const (
	ExecutionSuccess ExecutionStatus = iota
	ExecutionRevert
	ExecutionError
	ExecutionOutOfGas
	ExecutionStackOverflow
	ExecutionStackUnderflow
	ExecutionInvalidJumpDest
	ExecutionInvalidOpcode
	ExecutionUnknown
)

func init() {
	llvm.InitializeNativeTarget()
	llvm.InitializeNativeAsmPrinter()
}

func NewEVMCompiler() *EVMCompiler {
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
	if c.executor != nil {
		c.executor.Dispose()
	}
	c.builder.Dispose()
	// c.module.Dispose() TODO: segfault after execute() - does engine take the ownership of the module
	c.ctx.Dispose()
}

func (c *EVMCompiler) ParseBytecode(bytecode []byte) ([]EVMInstruction, error) {
	instructions := make([]EVMInstruction, 0)
	pc := uint64(0)

	for pc < uint64(len(bytecode)) {
		opcode := OpCode(bytecode[pc])
		instr := EVMInstruction{
			Opcode: opcode,
			PC:     pc,
		}

		if opcode >= PUSH0 && opcode <= PUSH32 {
			dataSize := int(opcode - PUSH0)
			instr.Data = make([]byte, dataSize)
			if pc+uint64(dataSize) >= uint64(len(bytecode)) {
				// pad zeros
				copy(instr.Data, bytecode[pc+1:])
			} else {
				copy(instr.Data, bytecode[pc+1:pc+1+uint64(dataSize)])
			}
			pc += uint64(dataSize)
		}
		instructions = append(instructions, instr)
		pc++
	}

	return instructions, nil
}

func (c *EVMCompiler) CompileBytecode(bytecode []byte) (llvm.Module, error) {
	// Use static analysis approach by default
	return c.CompileBytecodeStatic(bytecode, &EVMCompilationOpts{DisableGas: false})
}

func (c *EVMCompiler) checkStackOverflow(stackIdxVal llvm.Value, num int, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) {
	if num <= 0 {
		return
	}

	// Check if we exceed stack limit
	exceedsLimit := c.builder.CreateICmp(llvm.IntUGT, stackIdxVal, c.u64Const(uint64(1024-num)), "stack_overflow_cond")

	// Create continuation block & stack overflow block
	continueBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "stack_overflow_check_continue")
	stackOverflowBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "stack_overflow")

	// Branch to stack overflow block if limit exceeded, otherwise continue
	c.builder.CreateCondBr(exceedsLimit, stackOverflowBlock, continueBlock)

	c.builder.SetInsertPointAtEnd(stackOverflowBlock)
	// Store error code and exit
	c.builder.CreateStore(c.u64Const(uint64(ExecutionStackOverflow)), errorCodePtr)
	c.builder.CreateBr(errorBlock)

	// Insert to continueBlock so that we can insert the rest code
	c.builder.SetInsertPointAtEnd(continueBlock)
}

func (c *EVMCompiler) checkStackUnderflow(stackIdxVal llvm.Value, num uint64, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) {
	if num == 0 {
		return
	}
	// Check if we exceed stack limit
	exceedsLimit := c.builder.CreateICmp(llvm.IntULT, stackIdxVal, c.u64Const(num), "stack_underflow_cond")

	// Create continuation block & stack underflow block
	continueBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "stack_underflow_check_continue")
	stackUnderflowBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "stack_underflow")

	// Branch to stack underflow block if limit exceeded, otherwise continue
	c.builder.CreateCondBr(exceedsLimit, stackUnderflowBlock, continueBlock)

	c.builder.SetInsertPointAtEnd(stackUnderflowBlock)
	// Store error code and exit
	c.builder.CreateStore(c.u64Const(uint64(ExecutionStackUnderflow)), errorCodePtr)
	c.builder.CreateBr(errorBlock)

	// Insert to continueBlock so that we can insert the rest code
	c.builder.SetInsertPointAtEnd(continueBlock)
}

func (c *EVMCompiler) pushStackEmpty(stackIdxPtr llvm.Value) {
	stackIdxVal := c.builder.CreateLoad(c.ctx.Int64Type(), stackIdxPtr, "stack_idx_val")
	newStackIdxVal := c.builder.CreateAdd(stackIdxVal, c.u64Const(1), "new_stack_idx_val")
	c.builder.CreateStore(newStackIdxVal, stackIdxPtr)
}

func (c *EVMCompiler) pushStack(stack, stackIdxPtr, value llvm.Value) {
	stackIdxVal := c.builder.CreateLoad(c.ctx.Int64Type(), stackIdxPtr, "stack_idx_val")

	// Obtain stack value and store new stack idx
	stackElem := c.builder.CreateGEP(c.ctx.IntType(256), stack, []llvm.Value{stackIdxVal}, "stack_elem")
	c.builder.CreateStore(value, stackElem)
	newStackIdxVal := c.builder.CreateAdd(stackIdxVal, c.u64Const(1), "new_stack_idx_val")
	c.builder.CreateStore(newStackIdxVal, stackIdxPtr)
}

func (c *EVMCompiler) popStack(stack, stackIdxPtr llvm.Value) llvm.Value {
	stackIdxVal := c.builder.CreateLoad(c.ctx.Int64Type(), stackIdxPtr, "stack_idx_val")
	newStackIdxVal := c.builder.CreateSub(stackIdxVal, c.u64Const(1), "new_stack_idx_val")
	c.builder.CreateStore(newStackIdxVal, stackIdxPtr)
	stackElem := c.builder.CreateGEP(c.ctx.IntType(256), stack, []llvm.Value{newStackIdxVal}, "stack_elem")
	return c.builder.CreateLoad(c.ctx.IntType(256), stackElem, "stack_value")
}

func (c *EVMCompiler) peekStackPtr(stack, stackIdxPtr llvm.Value) llvm.Value {
	stackIdxVal := c.builder.CreateLoad(c.ctx.Int64Type(), stackIdxPtr, "stack_idx_val")
	newStackIdxVal := c.builder.CreateSub(stackIdxVal, c.u64Const(1), "new_stack_idx_val")
	return c.builder.CreateGEP(c.ctx.IntType(256), stack, []llvm.Value{newStackIdxVal}, "stack_elem")
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
		byteOffset := c.u64Const(uint64(i))
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

		byteOffset := c.u64Const(uint64(i))
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
	opts := llvm.NewPassBuilderOptions()
	defer opts.Dispose()

	c.module.RunPasses("default<O3>", c.machine, opts)
}

func (c *EVMCompiler) CompileAndOptimize(bytecode []byte) error {
	// Use static analysis approach by default
	return c.CompileAndOptimizeStatic(bytecode, &EVMCompilationOpts{DisableGas: false})
}

func (c *EVMCompiler) CompileAndOptimizeWithOpts(bytecode []byte, opts *EVMCompilationOpts) error {
	// Use static analysis approach by default
	return c.CompileAndOptimizeStatic(bytecode, opts)
}

// GetCompiledCode returns the compiled code in object file format that can be executed locally.
func (c *EVMCompiler) GetCompiledCode() []byte {
	mem, err := c.machine.EmitToMemoryBuffer(c.module, llvm.ObjectFile)
	if err != nil {
		panic(err)
	}
	return mem.Bytes()
}

func (c *EVMCompiler) CreateExecutor(opts *EVMExecutionOpts) error {
	if opts == nil {
		panic("EVMExecutionOpts must be provided.")
	}
	if opts.Config == nil {
		panic("Runtime config must be provided.")
	}
	if opts.Config.ChainConfig == nil {
		panic("ChainConfig must be provided.")
	}
	evm := NewEnv(opts.Config)
	c.executor = evm.executor
	c.executor.AddCompiledContract(c.codeHash, c.GetCompiledCode())
	c.executor.AddInstructionTable(c.table)
	return nil
}

// Execute the compiled EVM code
func (c *EVMCompiler) Execute(opts *EVMExecutionOpts) (*EVMExecutionResult, error) {
	return c.executor.Run(*NewContract(defaultCallerAddress, defaultCompilationAddress, uint256.MustFromBig(opts.Config.Value), opts.Config.GasLimit, c.codeHash), opts.Input, false)
}

// ExecuteCompiled executes compiled EVM code using function pointer for better performance
func (c *EVMCompiler) ExecuteCompiled(bytecode []byte) (*EVMExecutionResult, error) {
	opts := &EVMExecutionOpts{
		Config: &runtime.Config{
			GasLimit:    1000000,
			ChainConfig: params.TestChainConfig,
		},
	}

	return c.ExecuteCompiledWithOpts(bytecode, DefaultEVMCompilationOpts(), opts)
}

// ExecuteCompiled executes compiled EVM code using function pointer for better performance
func (c *EVMCompiler) ExecuteCompiledWithOpts(bytecode []byte, copts *EVMCompilationOpts, opts *EVMExecutionOpts) (*EVMExecutionResult, error) {
	// Compile if needed
	err := c.CompileAndOptimizeWithOpts(bytecode, copts)
	if err != nil {
		return nil, fmt.Errorf("compilation failed: %v", err)
	}

	// Create executor
	if opts == nil {
		panic("EVMExecutionOpts must be provided.")
	}
	if opts.Config == nil {
		panic("Runtime config must be provided in EVMExecutionOpts.")
	}
	err = c.CreateExecutor(opts)
	if err != nil {
		return nil, err
	}

	return c.Execute(opts)
}
