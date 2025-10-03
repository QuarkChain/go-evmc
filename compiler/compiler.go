package compiler

import (
	"fmt"
	"math"
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
	llvmEngine     llvm.ExecutionEngine
	initSectionGas uint64

	hostFuncType llvm.Type
	hostFunc     llvm.Value
	table        *JumpTable
	clibs        map[string]CHostFunc

	copts *EVMCompilationOpts
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
	Ret          []byte
}

type EVMExecutionOpts struct {
	Config *runtime.Config
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
var defaultGaslimit = uint64(math.MaxUint64)

type EVMCompilationOpts struct {
	DisableGas                        bool
	DisableSectionGasOptimization     bool
	DisableStackUnderflowOptimization bool
	DisableIROptimization             bool
	FreeMemory                        bool
}

func DefaultEVMCompilationOpts() *EVMCompilationOpts {
	return &EVMCompilationOpts{
		DisableGas:                    false,
		DisableSectionGasOptimization: false,
	}
}

func GetContractFunction(codeHash common.Hash) string {
	return fmt.Sprintf("contract_%s", codeHash.Hex())
}

func init() {
	llvm.InitializeNativeTarget()
	llvm.InitializeNativeAsmPrinter()
}

func NewEVMCompiler(chainRules params.Rules, extraEips []int) *EVMCompiler {
	ctx := llvm.NewContext()
	module := ctx.NewModule("evm_module")
	builder := ctx.NewBuilder()

	target, err := llvm.GetTargetFromTriple(llvm.DefaultTargetTriple())
	if err != nil {
		panic(fmt.Sprintf("Failed to get target: %s", err))
	}

	machine := target.CreateTargetMachine(
		llvm.DefaultTargetTriple(), "generic", "",
		llvm.CodeGenLevelDefault, llvm.RelocDefault, llvm.CodeModelJITDefault)

	table, _, err := getJumpTable(chainRules, extraEips)
	if err != nil {
		panic(fmt.Sprintf("Failed to get jumpTable: %s", err))
	}

	return &EVMCompiler{
		ctx:       ctx,
		module:    module,
		builder:   builder,
		target:    target,
		machine:   machine,
		stackType: llvm.PointerType(ctx.IntType(256), 0),
		memType:   llvm.PointerType(ctx.Int8Type(), 0),
		table:     table,
	}
}

func (c *EVMCompiler) Dispose() {
	c.builder.Dispose()
	c.module.Dispose()
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
	c.builder.CreateStore(c.u64Const(uint64(VMErrorCodeStackOverflow)), errorCodePtr)
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
	c.builder.CreateStore(c.u64Const(uint64(VMErrorCodeStackUnderflow)), errorCodePtr)
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

func popStack(ctx llvm.Context, builder llvm.Builder, stack, stackIdxPtr llvm.Value) llvm.Value {
	stackIdxVal := builder.CreateLoad(ctx.Int64Type(), stackIdxPtr, "stack_idx_val")
	newStackIdxVal := builder.CreateSub(stackIdxVal, u64Const(ctx, 1), "new_stack_idx_val")
	builder.CreateStore(newStackIdxVal, stackIdxPtr)
	stackElem := builder.CreateGEP(ctx.IntType(256), stack, []llvm.Value{newStackIdxVal}, "stack_elem")
	return builder.CreateLoad(ctx.IntType(256), stackElem, "stack_value")
}

func peekStackPtr(ctx llvm.Context, builder llvm.Builder, stack, stackIdxPtr llvm.Value) llvm.Value {
	stackIdxVal := builder.CreateLoad(ctx.Int64Type(), stackIdxPtr, "stack_idx_val")
	newStackIdxVal := builder.CreateSub(stackIdxVal, u64Const(ctx, 1), "new_stack_idx_val")
	return builder.CreateGEP(ctx.IntType(256), stack, []llvm.Value{newStackIdxVal}, "stack_elem")
}

func peekStack(ctx llvm.Context, builder llvm.Builder, stack, stackIdxPtr llvm.Value, pos uint64) llvm.Value {
	stackIdxVal := builder.CreateLoad(ctx.Int64Type(), stackIdxPtr, "stack_idx_val")
	newStackIdxVal := builder.CreateSub(stackIdxVal, u64Const(ctx, pos+1), "new_stack_idx_val")
	stackElem := builder.CreateGEP(ctx.IntType(256), stack, []llvm.Value{newStackIdxVal}, "stack_elem")
	return builder.CreateLoad(ctx.IntType(256), stackElem, "stack_value")
}

func (c *EVMCompiler) createUint256ConstantFromBytes(data []byte) llvm.Value {
	if len(data) == 0 {
		return llvm.ConstInt(c.ctx.IntType(256), 0, false)
	}

	v := uint256.NewInt(0).SetBytes(data)

	return llvm.ConstIntFromString(c.ctx.IntType(256), v.String(), 10)
}

func (c *EVMCompiler) checkU64Overflow(overflow, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) {
	continueBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "continue")
	u64OverflowBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "u64_overflow")
	// Branch to stack overflow block if limit exceeded, otherwise continue
	c.builder.CreateCondBr(overflow, u64OverflowBlock, continueBlock)

	c.builder.SetInsertPointAtEnd(u64OverflowBlock)
	// Store error code and exit
	c.builder.CreateStore(c.u64Const(uint64(VMErrorCodeGasUintOverflow)), errorCodePtr)
	c.builder.CreateBr(errorBlock)
	c.builder.SetInsertPointAtEnd(continueBlock)
}

func (c *EVMCompiler) safeMul(x, y, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) llvm.Value {
	ctx := c.ctx
	builder := c.builder
	i64 := ctx.Int64Type()
	i128 := ctx.IntType(128)

	// Extend to 128-bit
	x128 := builder.CreateZExt(x, i128, "x128")
	y128 := builder.CreateZExt(y, i128, "y128")

	// 128-bit product
	prod := builder.CreateMul(x128, y128, "prod")

	// Low 64 bits
	lo := builder.CreateTrunc(prod, i64, "lo")

	// High 64 bits
	hi := builder.CreateLShr(prod, llvm.ConstInt(i128, 64, false), "hi_shifted")
	hi64 := builder.CreateTrunc(hi, i64, "hi")

	// Check overflow: hi != 0
	hasOverflow := builder.CreateICmp(llvm.IntNE, hi64, llvm.ConstInt(i64, 0, false), "u64_overflow_safeMul")

	// Branch on overflow
	c.checkU64Overflow(hasOverflow, errorCodePtr, errorBlock)
	return lo
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
