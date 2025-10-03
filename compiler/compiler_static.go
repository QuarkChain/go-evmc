package compiler

import (
	"fmt"

	"github.com/ethereum/go-ethereum/crypto"
	"github.com/holiman/uint256"
	"tinygo.org/x/go-llvm"
)

const (
	OUTPUT_IDX_ERROR_CODE  = 0
	OUTPUT_IDX_GAS         = 1
	OUTPUT_IDX_STACK_DEPTH = 2
	OUTPUT_SIZE            = (OUTPUT_IDX_STACK_DEPTH + 1) * 8
)

var (
	INT256_NEGATIVE_1   = uint256.MustFromHex("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff").String() // -1
	INT256_NEGATIVE_MIN = uint256.MustFromHex("0x8000000000000000000000000000000000000000000000000000000000000000").String() // -2^255

	BIT_SWAP256     = "llvm.bswap.i256"
	MEMORY_NEW      = "memory_new"
	MEMORY_FREE     = "memory_free"
	MEMORY_RESIZE   = "memory_resize"
	MEMORY_SET      = "memory_set"
	MEMORY_COPY     = "memory_copy"
	MEMORY_PTR      = "memory_getptr"
	TO_WORD_SIZE    = "to_word_size"
	MEMORY_GAS_COST = "memory_gas_cost"
	MEM_CP_GAS_COST = "memory_copier_gas"
)

// PCAnalysis holds static program counter analysis results
type PCAnalysis struct {
	instructionBlocks map[uint64]llvm.BasicBlock // PC -> basic block mapping
	jumpTargets       map[uint64]bool            // Valid jump targets (JUMPDEST)
	pcToInstruction   map[uint64]*EVMInstruction // PC -> instruction mapping
	sectionGas        map[uint64]uint64          // Gas cost per section
}

// setOutputValueAt sets the output value at outputPtr
func (c *EVMCompiler) setOutputValueAt(outputPtr llvm.Value, idx int, value llvm.Value) {
	v := llvm.ConstGEP(c.ctx.Int64Type(), outputPtr, []llvm.Value{c.u64Const(uint64(idx))})
	c.builder.CreateStore(value, v)
}

func (c *EVMCompiler) u64Const(value uint64) llvm.Value {
	return llvm.ConstInt(c.ctx.Int64Type(), value, false)
}

type CHostFunc struct {
	fnType llvm.Type
	fn     llvm.Value
}

func Clibs(ctx llvm.Context, mod llvm.Module) map[string]CHostFunc {
	// C types
	i8ptr := llvm.PointerType(ctx.Int8Type(), 0)
	i32 := ctx.Int32Type()
	i64 := ctx.Int64Type()
	i256 := ctx.IntType(256)
	voidt := ctx.VoidType()

	bswap256FnType := llvm.FunctionType(i256, []llvm.Type{i256}, false)
	bswap256Fn := llvm.AddFunction(mod, BIT_SWAP256, bswap256FnType)

	// bswap64FnType := llvm.FunctionType(i256, []llvm.Type{i256}, false)
	// bswap64Fn := llvm.AddFunction(mod, BIT_SWAP64, bswap64FnType)

	// Memory* type is treated as i8*
	memPtrType := i8ptr
	// Memory* memory_new(void)
	memNewType := llvm.FunctionType(memPtrType, nil, false)
	memNewFn := llvm.AddFunction(mod, MEMORY_NEW, memNewType)

	// void memory_free(Memory*)
	memFreeType := llvm.FunctionType(voidt, []llvm.Type{memPtrType}, false)
	memFreeFn := llvm.AddFunction(mod, MEMORY_FREE, memFreeType)

	// void memory_resize(Memory*, i64)
	resizeType := llvm.FunctionType(voidt, []llvm.Type{memPtrType, i64}, false)
	memResizeFn := llvm.AddFunction(mod, MEMORY_RESIZE, resizeType)

	// uint8_t* memory_getptr(Memory* m, uint64_t offset, uint64_t size)
	memGetptrType := llvm.FunctionType(i8ptr, []llvm.Type{memPtrType, i64, i64}, false)
	memGetptrFn := llvm.AddFunction(mod, MEMORY_PTR, memGetptrType)

	// void memory_set(Memory*, i64, i64, const i8*)
	memSetType := llvm.FunctionType(voidt, []llvm.Type{memPtrType, i64, i64, i8ptr}, false)
	memSetFn := llvm.AddFunction(mod, MEMORY_SET, memSetType)

	// void memory_copy(Memory *m, uint64_t dst, uint64_t src, uint64_t len)
	memCpType := llvm.FunctionType(voidt, []llvm.Type{memPtrType, i64, i64, i64}, false)
	memCpFn := llvm.AddFunction(mod, MEMORY_COPY, memCpType)

	// int memory_gas_cost(Memory *mem, uint64_t newMemSize, uint64_t *outFee)
	memGasCostType := llvm.FunctionType(
		i32, // return int
		[]llvm.Type{memPtrType, i64, llvm.PointerType(i64, 0)},
		false,
	)
	memGasCostFn := llvm.AddFunction(mod, MEMORY_GAS_COST, memGasCostType)

	// int memory_copier_gas(Memory *mem, uint64_t words, uint64_t memorySize, uint64_t *outGas)
	memCpGasCostType := llvm.FunctionType(
		i32, // return int
		[]llvm.Type{memPtrType, i64, i64, llvm.PointerType(i64, 0)},
		false,
	)
	memCpGasCostFn := llvm.AddFunction(mod, MEM_CP_GAS_COST, memCpGasCostType)

	// uint64_t to_word_size(uint64_t size)
	toWordSizeType := llvm.FunctionType(i64, []llvm.Type{i64}, false)
	toWordSizeFn := llvm.AddFunction(mod, "to_word_size", toWordSizeType)

	memNewFn.SetLinkage(llvm.ExternalLinkage)
	memFreeFn.SetLinkage(llvm.ExternalLinkage)
	memResizeFn.SetLinkage(llvm.ExternalLinkage)
	memSetFn.SetLinkage(llvm.ExternalLinkage)
	toWordSizeFn.SetLinkage(llvm.ExternalLinkage)
	memGasCostFn.SetLinkage(llvm.ExternalLinkage)
	memCpFn.SetLinkage(llvm.ExternalLinkage)
	memCpGasCostFn.SetLinkage(llvm.ExternalLinkage)

	cHostFns := make(map[string]CHostFunc)
	cHostFns[BIT_SWAP256] = CHostFunc{bswap256FnType, bswap256Fn}
	// cHostFns[BIT_SWAP64] = CHostFunc{bswap64FnType, bswap64Fn}
	cHostFns[MEMORY_NEW] = CHostFunc{memNewType, memNewFn}
	cHostFns[MEMORY_FREE] = CHostFunc{memFreeType, memFreeFn}
	cHostFns[MEMORY_RESIZE] = CHostFunc{resizeType, memResizeFn}
	cHostFns[MEMORY_SET] = CHostFunc{memSetType, memSetFn}
	cHostFns[MEMORY_COPY] = CHostFunc{memCpType, memCpFn}
	cHostFns[MEMORY_PTR] = CHostFunc{memGetptrType, memGetptrFn}
	cHostFns[TO_WORD_SIZE] = CHostFunc{toWordSizeType, toWordSizeFn}

	cHostFns[MEMORY_GAS_COST] = CHostFunc{memGasCostType, memGasCostFn}
	cHostFns[MEM_CP_GAS_COST] = CHostFunc{memCpGasCostType, memCpGasCostFn}

	return cHostFns
}

// CompileBytecodeStatic compiles EVM bytecode using static PC analysis
func (c *EVMCompiler) CompileBytecodeStatic(bytecode []byte, opts *EVMCompilationOpts) (llvm.Module, error) {
	if opts == nil {
		panic("nil EVMCompilationOpts")
	}
	instructions, err := c.ParseBytecode(bytecode)
	if err != nil {
		return llvm.Module{}, err
	}
	c.clibs = Clibs(c.ctx, c.module)

	// Perform static analysis
	analysis := c.analyzeProgram(instructions)

	uint256Type := c.ctx.IntType(256)
	uint8PtrType := llvm.PointerType(c.ctx.Int8Type(), 0)
	uint256PtrType := llvm.PointerType(uint256Type, 0)
	uint64PtrType := llvm.PointerType(c.ctx.Int64Type(), 0)

	memPtrType := llvm.PointerType(c.ctx.Int8Type(), 0)
	execType := llvm.FunctionType(memPtrType, []llvm.Type{
		c.ctx.Int64Type(), // inst
		uint256PtrType,    // stack
		uint8PtrType,      // code (unused but kept for signature)
		c.ctx.Int64Type(), // gas limit
		uint64PtrType,     // output args
	}, false)

	execFunc := llvm.AddFunction(c.module, GetContractFunction(crypto.Keccak256Hash(bytecode)), execType)
	execFunc.SetFunctionCallConv(llvm.CCallConv)

	instParam := execFunc.Param(0)
	stackParam := execFunc.Param(1)
	gasLimitParam := execFunc.Param(3)
	outputPtrParam := execFunc.Param(4)

	// Create entry block
	entryBlock := llvm.AddBasicBlock(execFunc, "entry")
	c.builder.SetInsertPointAtEnd(entryBlock)

	// Initialize stack pointer
	stackIdxPtr := c.builder.CreateAlloca(c.ctx.Int64Type(), "stack_ptr")
	c.builder.CreateStore(c.u64Const(0), stackIdxPtr)

	memNewFn := c.clibs[MEMORY_NEW]
	memory := c.builder.CreateCall(memNewFn.fnType, memNewFn.fn, []llvm.Value{}, "call_memory_new")

	// Initialize gasPtr tracking
	gasPtr := c.builder.CreateAlloca(c.ctx.Int64Type(), "gas_used")
	c.builder.CreateStore(gasLimitParam, gasPtr)

	// Create out-of-gas block
	errorBlock := llvm.AddBasicBlock(execFunc, "error")
	errorCodePtr := c.builder.CreateAlloca(c.ctx.Int64Type(), "error_code")

	// Consume the initial section gas
	if !opts.DisableGas && !opts.DisableSectionGasOptimization {
		c.consumeGas(c.initSectionGas, gasPtr, errorCodePtr, errorBlock)
	}

	// Create exit block
	exitBlock := llvm.AddBasicBlock(execFunc, "exit")

	// Create jump table block
	dynJumpBlock := llvm.AddBasicBlock(execFunc, "jump")
	jumpTargetPtr := c.builder.CreateAlloca(uint256Type, "jump_target")
	c.builder.CreateStore(llvm.ConstInt(uint256Type, 0, false), jumpTargetPtr)

	// Create basic blocks for each instruction
	for _, instr := range instructions {
		blockName := fmt.Sprintf("pc_%d", instr.PC)
		block := llvm.AddBasicBlock(execFunc, blockName)
		analysis.instructionBlocks[instr.PC] = block
	}

	// Jump to first instruction
	if len(instructions) > 0 {
		firstBlock := analysis.instructionBlocks[instructions[0].PC]
		c.builder.CreateBr(firstBlock)
	} else {
		c.builder.CreateBr(exitBlock)
	}

	// Compile each instruction
	var prevInstr *EVMInstruction
	for _, instr := range instructions {
		block := analysis.instructionBlocks[instr.PC]
		c.builder.SetInsertPointAtEnd(block)

		nextPC := c.getNextPC(instr, instructions)
		nextBlock := exitBlock
		if nextPC != ^uint64(0) {
			if nb, ok := analysis.instructionBlocks[nextPC]; ok {
				nextBlock = nb
			}
		}

		c.compileInstructionStatic(instr, prevInstr, instParam, stackParam, stackIdxPtr, memory, gasPtr, jumpTargetPtr, errorCodePtr, analysis, nextBlock, dynJumpBlock, exitBlock, errorBlock, opts)
		prevInstr = &instr
	}

	// Finalize dynamic jump block
	c.builder.SetInsertPointAtEnd(dynJumpBlock)
	c.createDynamicJumpBlock(jumpTargetPtr, analysis, errorCodePtr, errorBlock)

	finalizeGas := func() {
		if opts.DisableGas {
			c.setOutputValueAt(outputPtrParam, OUTPUT_IDX_GAS, c.u64Const(0))
		} else {
			finalGasUsed := c.builder.CreateLoad(c.ctx.Int64Type(), gasPtr, "final_gas_used")
			c.setOutputValueAt(outputPtrParam, OUTPUT_IDX_GAS, finalGasUsed)
		}
	}

	// Finalize error block
	c.builder.SetInsertPointAtEnd(errorBlock)
	errorCode := c.builder.CreateLoad(c.ctx.Int64Type(), errorCodePtr, "")
	finalizeGas()
	c.setOutputValueAt(outputPtrParam, OUTPUT_IDX_ERROR_CODE, errorCode)

	nullMem := llvm.ConstNull(memPtrType)
	memFreeFn := c.clibs[MEMORY_FREE]
	if opts.FreeMemory {
		c.builder.CreateCall(memFreeFn.fnType, memFreeFn.fn, []llvm.Value{memory}, "")
		c.builder.CreateRet(nullMem)
	} else {
		c.builder.CreateRet(memory)
	}

	// Finalize exit block
	c.builder.SetInsertPointAtEnd(exitBlock)
	finalizeGas()
	stackDepth := c.builder.CreateLoad(c.ctx.Int64Type(), stackIdxPtr, "stack_depth")
	c.setOutputValueAt(outputPtrParam, OUTPUT_IDX_STACK_DEPTH, stackDepth)
	if opts.FreeMemory {
		c.builder.CreateCall(memFreeFn.fnType, memFreeFn.fn, []llvm.Value{memory}, "")
		c.builder.CreateRet(nullMem)
	} else {
		c.builder.CreateRet(memory)
	}

	err = llvm.VerifyModule(c.module, llvm.ReturnStatusAction)
	if err != nil {
		return llvm.Module{}, fmt.Errorf("module verification failed: %s", err)
	}

	return c.module, nil
}

// analyzeProgram performs static analysis on the instruction sequence
func (c *EVMCompiler) analyzeProgram(instructions []EVMInstruction) *PCAnalysis {
	analysis := &PCAnalysis{
		instructionBlocks: make(map[uint64]llvm.BasicBlock),
		jumpTargets:       make(map[uint64]bool),
		pcToInstruction:   make(map[uint64]*EVMInstruction),
		sectionGas:        make(map[uint64]uint64),
	}

	// Build PC to instruction mapping and identify jump targets
	sectionGas := uint64(0)
	sectionStartPC := -1
	minStack := 0
	for i := range instructions {
		instr := &instructions[i]
		analysis.pcToInstruction[instr.PC] = instr
		instr.MinStack = minStack
		// minStack = max(0, pop, minStack+pop-push)
		minStack = max(max(0, minStack+c.table[instr.Opcode].diffStack), c.table[instr.Opcode].diffStack+c.table[instr.Opcode].minStack)

		sectionGas += c.table[instr.Opcode].constantGas

		if instr.Opcode == JUMPDEST {
			analysis.jumpTargets[instr.PC] = true
		}

		if instr.Opcode == JUMPDEST || instr.Opcode == JUMP || instr.Opcode == JUMPI || instr.Opcode == STOP {
			// end of section
			if instr.Opcode == JUMPDEST {
				// For JUMPDEST, the section ends at PC-1
				// o.w., the section ends at PC.
				sectionGas -= c.table[instr.Opcode].constantGas
			}
			if sectionStartPC == -1 {
				c.initSectionGas = sectionGas
			} else if sectionStartPC == -2 {
				// section not started.  the code will never be reached
			} else {
				analysis.sectionGas[uint64(sectionStartPC)] = sectionGas
			}
			if instr.Opcode == JUMPDEST || instr.Opcode == JUMPI {
				// start of section
				if instr.Opcode == JUMPDEST {
					sectionStartPC = int(instr.PC)
					sectionGas = c.table[instr.Opcode].constantGas
					// reset minStack - always assume the stack is empty (worst case) when a section begins
					minStack = 0
				} else {
					// JUMPI
					sectionStartPC = int(instr.PC) + 1
					sectionGas = 0
				}
			} else {
				// STOP, JUMP won't start a section
				sectionStartPC = -2 // section not started yet
			}
		}
	}

	// Check if section gas is charged properly if the code does not end with STOP.
	if sectionStartPC == -1 {
		c.initSectionGas = sectionGas
	} else if sectionStartPC >= 0 {
		// end of section
		analysis.sectionGas[uint64(sectionStartPC)] = sectionGas
	} else if sectionStartPC != -2 {
		panic("invalid sectionStartPC")
	}
	return analysis
}

// getNextPC calculates the next PC for sequential execution
func (c *EVMCompiler) getNextPC(currentInstr EVMInstruction, instructions []EVMInstruction) uint64 {
	for i, instr := range instructions {
		if instr.PC == currentInstr.PC && i+1 < len(instructions) {
			return instructions[i+1].PC
		}
	}
	return ^uint64(0) // End of program marker
}

func (c *EVMCompiler) checkHostReturn(ret, errorCodePtr llvm.Value, nextBlock, errorBlock llvm.BasicBlock) {
	c.builder.CreateStore(ret, errorCodePtr)
	isNonZero := c.builder.CreateICmp(llvm.IntNE, ret, c.u64Const(0), "error_code_cond")
	c.builder.CreateCondBr(isNonZero, errorBlock, nextBlock)
}

// compileInstructionStatic compiles an instruction using static analysis with gas metering
func (c *EVMCompiler) compileInstructionStatic(instr EVMInstruction, prevInstr *EVMInstruction, execInst, stack, stackIdxPtr, memory, gasPtr, jumpTargetPtr, errorCodePtr llvm.Value, analysis *PCAnalysis, nextBlock, dynJumpBlock, exitBlock, errorBlock llvm.BasicBlock, opts *EVMCompilationOpts) {
	clibs := c.clibs
	ctx, builder := c.ctx, c.builder

	uint256Type := c.ctx.IntType(256)

	// If the code is unsupported, return error
	if instr.Opcode == INVALID || c.table[instr.Opcode].undefined {
		// Store error code and exit
		c.builder.CreateStore(c.u64Const(uint64(VMErrorCodeInvalidOpCode)), errorCodePtr)
		c.builder.CreateBr(errorBlock)
		return
	}

	// Add gas consumption for this instruction
	if !opts.DisableGas {
		if opts.DisableSectionGasOptimization {
			c.consumeGas(c.table[instr.Opcode].constantGas, gasPtr, errorCodePtr, errorBlock)
		} else {
			gasCost := analysis.sectionGas[instr.PC]
			c.consumeGas(gasCost, gasPtr, errorCodePtr, errorBlock)
		}
	}

	// Check stack overflow/underflow
	stackIdxVal := c.builder.CreateLoad(c.ctx.Int64Type(), stackIdxPtr, "")
	if opts.DisableStackUnderflowOptimization || instr.MinStack < c.table[instr.Opcode].minStack {
		c.checkStackUnderflow(stackIdxVal, uint64(c.table[instr.Opcode].minStack), errorCodePtr, errorBlock)
	}
	c.checkStackOverflow(stackIdxVal, c.table[instr.Opcode].diffStack, errorCodePtr, errorBlock)

	// Check if it is host function
	if c.table[instr.Opcode].execute != nil {
		ret := c.builder.CreateCall(c.hostFuncType, c.hostFunc, []llvm.Value{execInst, c.u64Const(uint64(instr.Opcode)), c.u64Const(uint64(instr.PC)), gasPtr, stackIdxPtr, memory}, "")
		if c.table[instr.Opcode].diffStack < 0 {
			for i := 0; i < -c.table[instr.Opcode].diffStack; i++ {
				popStack(ctx, builder, stack, stackIdxPtr)
			}
		} else {
			for i := 0; i < c.table[instr.Opcode].diffStack; i++ {
				c.pushStackEmpty(stackIdxPtr)
			}
		}
		// TODO: may not check if the opcode will not return error
		c.checkHostReturn(ret, errorCodePtr, nextBlock, errorBlock)
		return
	}

	switch instr.Opcode {
	case MLOAD:
		memSizeFn := memoryMLoadC
		gasFn := gasMLoadC
		c.consumeDynGasAndResizeMem(stack, stackIdxPtr, memory, gasPtr, errorCodePtr, errorBlock, memSizeFn, gasFn)

		opMloadC(ctx, builder, memory, stack, stackIdxPtr, clibs)
		c.builder.CreateBr(nextBlock)

	case MSTORE:
		memSizeFn := memoryMStoreC
		gasFn := gasMStoreC
		c.consumeDynGasAndResizeMem(stack, stackIdxPtr, memory, gasPtr, errorCodePtr, errorBlock, memSizeFn, gasFn)

		opMstoreC(ctx, builder, memory, stack, stackIdxPtr, clibs)
		c.builder.CreateBr(nextBlock)

	case MSTORE8:
		memSizeFn := memoryMStore8C
		gasFn := gasMStore8C
		c.consumeDynGasAndResizeMem(stack, stackIdxPtr, memory, gasPtr, errorCodePtr, errorBlock, memSizeFn, gasFn)

		opMstore8C(ctx, builder, memory, stack, stackIdxPtr, clibs)
		c.builder.CreateBr(nextBlock)

	case STOP:
		c.builder.CreateBr(exitBlock)

	case ADD:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)
		result := c.builder.CreateAdd(a, b, "add_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case MUL:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)
		result := c.builder.CreateMul(a, b, "mul_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case SUB:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)
		result := c.builder.CreateSub(a, b, "sub_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case DIV:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)
		zero := llvm.ConstInt(uint256Type, 0, false)
		isZero := c.builder.CreateICmp(llvm.IntEQ, b, zero, "div_by_zero")
		result := c.builder.CreateSelect(isZero, zero, c.builder.CreateUDiv(a, b, "div_result"), "div_safe")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case SDIV:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)
		zero := llvm.ConstInt(uint256Type, 0, false)
		int256Min := llvm.ConstIntFromString(uint256Type, INT256_NEGATIVE_MIN, 10)
		int256Negtive1 := llvm.ConstIntFromString(uint256Type, INT256_NEGATIVE_1, 10)

		isDivByZero := c.builder.CreateICmp(llvm.IntEQ, b, zero, "div_by_zero")
		isOverflow := c.builder.CreateAnd(
			c.builder.CreateICmp(llvm.IntEQ, a, int256Min, "equal_neg2^255"),
			c.builder.CreateICmp(llvm.IntEQ, b, int256Negtive1, "equal_neg1"),
			"overflow",
		)
		// sdiv result
		sdiv := c.builder.CreateSDiv(a, b, "sdiv_result")
		result := c.builder.CreateSelect(
			isDivByZero,
			zero, // division by zero → 0
			c.builder.CreateSelect(isOverflow, int256Min, sdiv, "sdiv_safe"), // overflow → int256Min
			"",
		)
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case MOD:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)
		zero := llvm.ConstInt(uint256Type, 0, false)
		isZero := c.builder.CreateICmp(llvm.IntEQ, b, zero, "mod_by_zero")
		result := c.builder.CreateSelect(isZero, zero, c.builder.CreateURem(a, b, "mod_result"), "mod_safe")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case SMOD:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)
		zero := llvm.ConstInt(uint256Type, 0, false)
		isZero := c.builder.CreateICmp(llvm.IntEQ, b, zero, "smod_by_zero")
		result := c.builder.CreateSelect(isZero, zero, c.builder.CreateSRem(a, b, "smod_result"), "smod_safe")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	// case EXP:
	// a := popStack(ctx, builder,stack, stackPtr)
	// b := popStack(ctx, builder,stack, stackPtr)
	// result := c.builder. (a, b, "exp_result")
	// c.pushStack(stack, stackPtr, result)
	// c.builder.CreateBr(nextBlock)

	case SIGNEXTEND:
		// Adapted from revm: https://github.com/bluealloy/revm/blob/fda371f73aba2c30a83c639608be78145fd1123b/crates/interpreter/src/instructions/arithmetic.rs#L89
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)
		cond := c.builder.CreateICmp(llvm.IntULT, a, llvm.ConstInt(a.Type(), 31, false), "a_lt_31")
		// helper: signextend calculation
		signExtendCalc := func() llvm.Value {
			bitIndex := c.builder.CreateAdd(
				c.builder.CreateMul(a, llvm.ConstInt(a.Type(), 8, false), "bitindex_mul"),
				llvm.ConstInt(a.Type(), 7, false),
				"bitindex",
			)
			shiftAmt := c.builder.CreateZExt(bitIndex, b.Type(), "bitindex_zext")

			oneShift := c.builder.CreateShl(llvm.ConstInt(b.Type(), 1, false), shiftAmt, "one_shift")
			mask := c.builder.CreateSub(oneShift, llvm.ConstInt(b.Type(), 1, false), "mask")

			shiftedB := c.builder.CreateLShr(b, shiftAmt, "shifted_b")
			bit := c.builder.CreateAnd(shiftedB, llvm.ConstInt(b.Type(), 1, false), "bit")

			negMask := c.builder.CreateNot(mask, "neg_mask")
			orVal := c.builder.CreateOr(b, negMask, "or_val")
			andVal := c.builder.CreateAnd(b, mask, "and_val")

			bitIsSet := c.builder.CreateICmp(llvm.IntNE, bit, llvm.ConstInt(b.Type(), 0, false), "bit_neq0")
			return c.builder.CreateSelect(bitIsSet, orVal, andVal, "signed_val")
		}
		result := c.builder.CreateSelect(cond, signExtendCalc(), b, "signextend_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case LT, GT, SLT, SGT, EQ:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)

		var pred llvm.IntPredicate
		var name string

		switch instr.Opcode {
		case LT:
			pred = llvm.IntULT
			name = "lt"
		case GT:
			pred = llvm.IntUGT
			name = "gt"
		case SLT:
			pred = llvm.IntSLT
			name = "slt"
		case SGT:
			pred = llvm.IntSGT
			name = "sgt"
		case EQ:
			pred = llvm.IntEQ
			name = "eq"
		}

		cmp := c.builder.CreateICmp(pred, a, b, name+"_cmp")
		result := c.builder.CreateZExt(cmp, uint256Type, name+"_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case ISZERO:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		zero := llvm.ConstInt(uint256Type, 0, false)
		cmp := c.builder.CreateICmp(llvm.IntEQ, a, zero, "iszero_cmp")
		result := c.builder.CreateZExt(cmp, uint256Type, "iszero_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case AND:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)
		result := c.builder.CreateAnd(a, b, "and_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case OR:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)
		result := c.builder.CreateOr(a, b, "or_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case XOR:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)
		result := c.builder.CreateXor(a, b, "xor_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case NOT:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		allOnes := llvm.ConstIntFromString(uint256Type, INT256_NEGATIVE_1, 10)
		result := c.builder.CreateXor(a, allOnes, "not_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case BYTE:
		a := popStack(ctx, builder, stack, stackIdxPtr)
		b := popStack(ctx, builder, stack, stackIdxPtr)
		// Constants
		const32 := llvm.ConstInt(uint256Type, 32, false)   // upper bound for valid index
		const31 := llvm.ConstInt(uint256Type, 31, false)   // used for position calculation
		const8 := llvm.ConstInt(uint256Type, 8, false)     // bits per byte
		constFF := llvm.ConstInt(uint256Type, 0xff, false) // mask for one byte
		zero := llvm.ConstInt(uint256Type, 0, false)       // zero for out-of-range result

		// Compare: index < 32 (check if within valid byte range)
		cond := c.builder.CreateICmp(llvm.IntULT, a, const32, "byte_in_range")

		// If in range: shift right by (31 - index) * 8 bits
		shiftCount := c.builder.CreateMul(
			c.builder.CreateSub(const31, a, "31_minus_idx"),
			const8,
			"shift_count",
		)
		shifted := c.builder.CreateLShr(b, shiftCount, "shifted")
		// Mask to get only the lowest 8 bits (the extracted byte)
		masked := c.builder.CreateAnd(shifted, constFF, "masked")

		// Select result: if in range use masked value, else use 0
		result := c.builder.CreateSelect(cond, masked, zero, "byte_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case SHL:
		shift := popStack(ctx, builder, stack, stackIdxPtr)
		value := popStack(ctx, builder, stack, stackIdxPtr)
		result := c.builder.CreateShl(value, shift, "shl_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case SHR:
		shift := popStack(ctx, builder, stack, stackIdxPtr)
		value := popStack(ctx, builder, stack, stackIdxPtr)
		result := c.builder.CreateLShr(value, shift, "shr_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case SAR:
		shift := popStack(ctx, builder, stack, stackIdxPtr)
		value := popStack(ctx, builder, stack, stackIdxPtr)
		result := c.builder.CreateAShr(value, shift, "sar_result")
		c.pushStack(stack, stackIdxPtr, result)
		c.builder.CreateBr(nextBlock)

	case POP:
		popStack(ctx, builder, stack, stackIdxPtr)
		c.builder.CreateBr(nextBlock)

	case JUMP:
		target := popStack(ctx, builder, stack, stackIdxPtr)
		c.createJump(prevInstr, target, jumpTargetPtr, errorCodePtr, analysis, dynJumpBlock, errorBlock)

	case JUMPI:
		target := popStack(ctx, builder, stack, stackIdxPtr)
		condition := popStack(ctx, builder, stack, stackIdxPtr)
		zero := llvm.ConstInt(uint256Type, 0, false)
		isNonZero := c.builder.CreateICmp(llvm.IntNE, condition, zero, "jumpi_cond")

		// Create conditional branch: jump if condition != 0, otherwise continue
		jumpBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "conditional_jump")
		c.builder.CreateCondBr(isNonZero, jumpBlock, nextBlock)

		c.builder.SetInsertPointAtEnd(jumpBlock)
		c.createJump(prevInstr, target, jumpTargetPtr, errorCodePtr, analysis, dynJumpBlock, errorBlock)

	case PC:
		// Push current PC as a constant (static analysis!)
		pcValue := llvm.ConstInt(uint256Type, instr.PC, false)
		c.pushStack(stack, stackIdxPtr, pcValue)
		c.builder.CreateBr(nextBlock)

	case JUMPDEST:
		// JUMPDEST is a no-op, charge the section gas before continue to next instruction
		c.builder.CreateBr(nextBlock)

	case MSIZE:
		msizePtr := c.builder.CreateGEP(memory.Type(), memory, []llvm.Value{
			llvm.ConstInt(c.ctx.Int32Type(), 1, false),
		}, "msize_ptr")

		msizeVal := c.builder.CreateLoad(llvm.PointerType(c.ctx.Int64Type(), 0), msizePtr, "msize_val")
		c.pushStack(stack, stackIdxPtr, msizeVal)
		c.builder.CreateBr(nextBlock)

	case MCOPY:
		memSizeFn := memoryMcopyC
		gasFn := memoryCopierGasC(2)
		c.consumeDynGasAndResizeMem(stack, stackIdxPtr, memory, gasPtr, errorCodePtr, errorBlock, memSizeFn, gasFn)

		opMcopyC(ctx, builder, memory, stack, stackIdxPtr, clibs)
		c.builder.CreateBr(nextBlock)

	default:
		if instr.Opcode >= PUSH0 && instr.Opcode <= PUSH32 {
			c.compilePushStatic(instr, stack, stackIdxPtr, nextBlock)
		} else if instr.Opcode >= DUP1 && instr.Opcode <= DUP16 {
			c.compileDupStatic(instr, stack, stackIdxPtr, nextBlock)
		} else if instr.Opcode >= SWAP1 && instr.Opcode <= SWAP16 {
			c.compileSwapStatic(instr, stack, stackIdxPtr, nextBlock)
		} else {
			// Unknown opcode, TODO: generate code to return err
			panic(fmt.Sprintf("unsupported op %d", instr.Opcode))
			// c.builder.CreateBr(nextBlock)
		}
	}
}

// createJump creates a static jump if the previous instruction is PUSH; o.w., a dynamic jump given target from stack
func (c *EVMCompiler) createJump(prevInstr *EVMInstruction, target, jumpTargetPtr, errorCodePtr llvm.Value, analysis *PCAnalysis, dynJumpBlock, errorBlock llvm.BasicBlock) {
	if prevInstr != nil && prevInstr.Opcode.IsPush() {
		dest := uint256.NewInt(0).SetBytes(prevInstr.Data)
		dest64, overflow := dest.Uint64WithOverflow()
		if block, ok := analysis.instructionBlocks[dest64]; !overflow && ok {
			c.builder.CreateBr(block)
		} else {
			c.builder.CreateStore(llvm.ConstInt(c.ctx.Int64Type(), uint64(VMErrorCodeInvalidJump), false), errorCodePtr)
			c.builder.CreateBr(errorBlock)
		}
	} else {
		c.builder.CreateStore(target, jumpTargetPtr)
		c.builder.CreateBr(dynJumpBlock)
	}
}

// createDynamicJumpBlock creates a block with switch statement for dynamic jumps
func (c *EVMCompiler) createDynamicJumpBlock(targetPtr llvm.Value, analysis *PCAnalysis, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) {
	// Truncate 256-bit target to 64-bit PC
	// TODO: error if overflow
	target := c.builder.CreateLoad(c.ctx.IntType(256), targetPtr, "jump_target_u256")
	targetPC := c.builder.CreateTrunc(target, c.ctx.Int64Type(), "jump_target")

	// Create invalid jump dest block
	invalidJumpDestBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "invalid_jump_dest")

	// Create switch instruction with all valid jump targets
	switchInstr := c.builder.CreateSwitch(targetPC, invalidJumpDestBlock, len(analysis.jumpTargets))

	for pc := range analysis.jumpTargets {
		if block, ok := analysis.instructionBlocks[pc]; ok {
			pcConstant := c.u64Const(pc)
			switchInstr.AddCase(pcConstant, block)
		} else {
			panic("jump target not found")
		}
	}

	c.builder.SetInsertPointAtEnd(invalidJumpDestBlock)
	// Store error code and exit
	c.builder.CreateStore(c.u64Const(uint64(VMErrorCodeInvalidJump)), errorCodePtr)
	c.builder.CreateBr(errorBlock)
}

// createDynamicJump creates a switch statement for dynamic jumps
func (c *EVMCompiler) createDynamicJump(target llvm.Value, analysis *PCAnalysis, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) {
	// Truncate 256-bit target to 64-bit PC
	targetPC := c.builder.CreateTrunc(target, c.ctx.Int64Type(), "jump_target")

	// Create invalid jump dest block
	invalidJumpDestBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "invalid_jump_dest")

	// Create switch instruction with all valid jump targets
	switchInstr := c.builder.CreateSwitch(targetPC, invalidJumpDestBlock, len(analysis.jumpTargets))

	// Add cases for all valid JUMPDEST locations
	for pc := range analysis.jumpTargets {
		if block, ok := analysis.instructionBlocks[pc]; ok {
			pcConstant := c.u64Const(pc)
			switchInstr.AddCase(pcConstant, block)
		}
	}

	c.builder.SetInsertPointAtEnd(invalidJumpDestBlock)
	// Store error code and exit
	c.builder.CreateStore(c.u64Const(uint64(VMErrorCodeInvalidJump)), errorCodePtr)
	c.builder.CreateBr(errorBlock)
}

// compilePushStatic compiles PUSH instructions with static next block
func (c *EVMCompiler) compilePushStatic(instr EVMInstruction, stack, stackIdxPtr llvm.Value, nextBlock llvm.BasicBlock) {
	llvmValue := c.createUint256ConstantFromBytes(instr.Data)
	c.pushStack(stack, stackIdxPtr, llvmValue)
	c.builder.CreateBr(nextBlock)
}

// compileDupStatic compiles DUP instructions with static next block
func (c *EVMCompiler) compileDupStatic(instr EVMInstruction, stack, stackIdxPtr llvm.Value, nextBlock llvm.BasicBlock) {
	n := int(instr.Opcode - DUP1 + 1)
	stackIdxVal := c.builder.CreateLoad(c.ctx.Int64Type(), stackIdxPtr, "stack_idx_val")
	index := c.builder.CreateSub(stackIdxVal, c.u64Const(uint64(n)), "dup_index")
	stackElem := c.builder.CreateGEP(c.ctx.IntType(256), stack, []llvm.Value{index}, "stack_elem")
	value := c.builder.CreateLoad(c.ctx.IntType(256), stackElem, "dup_value")
	c.pushStack(stack, stackIdxPtr, value)
	c.builder.CreateBr(nextBlock)
}

// compileSwapStatic compiles SWAP instructions with static next block
func (c *EVMCompiler) compileSwapStatic(instr EVMInstruction, stack, stackIdxPtr llvm.Value, nextBlock llvm.BasicBlock) {
	n := int(instr.Opcode - SWAP1 + 1)
	stackIdxVal := c.builder.CreateLoad(c.ctx.Int64Type(), stackIdxPtr, "stack_idx_val")

	index1 := c.builder.CreateSub(stackIdxVal, c.u64Const(1), "swap_index1")
	index2 := c.builder.CreateSub(stackIdxVal, c.u64Const(uint64(n+1)), "swap_index2")

	elem1 := c.builder.CreateGEP(c.ctx.IntType(256), stack, []llvm.Value{index1}, "swap_elem1")
	elem2 := c.builder.CreateGEP(c.ctx.IntType(256), stack, []llvm.Value{index2}, "swap_elem2")

	value1 := c.builder.CreateLoad(c.ctx.IntType(256), elem1, "swap_value1")
	value2 := c.builder.CreateLoad(c.ctx.IntType(256), elem2, "swap_value2")

	c.builder.CreateStore(value2, elem1)
	c.builder.CreateStore(value1, elem2)
	c.builder.CreateBr(nextBlock)
}

// consumeGas adds gas consumption for an opcode and checks for out-of-gas condition
func (c *EVMCompiler) consumeGas(gasCost uint64, gasPtr, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) {
	// Get gas cost for this opcode
	if gasCost == 0 {
		return // No gas consumption for this opcode
	}

	// Load current gas used
	currentGas := c.builder.CreateLoad(c.ctx.Int64Type(), gasPtr, "gas_remaining")

	// Check if we exceed gas limit
	gasCostValue := c.u64Const(gasCost)
	notExceedsLimit := c.builder.CreateICmp(llvm.IntULE, gasCostValue, currentGas, "exceeds_gas_limit")

	// Create continuation block
	continueBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "gas_check_continue")
	outOfGasBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "out_of_gas")

	// Branch to out-of-gas block if limit exceeded, otherwise continue
	c.builder.CreateCondBr(notExceedsLimit, continueBlock, outOfGasBlock)

	c.builder.SetInsertPointAtEnd(outOfGasBlock)
	// Store error code and exit
	c.builder.CreateStore(c.u64Const(uint64(VMErrorCodeOutOfGas)), errorCodePtr)
	c.builder.CreateBr(errorBlock)

	c.builder.SetInsertPointAtEnd(continueBlock)
	// Sub gas cost and store
	newGas := c.builder.CreateSub(currentGas, gasCostValue, "new_gas_used")
	c.builder.CreateStore(newGas, gasPtr)
}

func (c *EVMCompiler) consumeDyncGas(gasCost, gasPtr, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) {
	// Load current gas used
	currentGas := c.builder.CreateLoad(c.ctx.Int64Type(), gasPtr, "gas_remaining")

	// Check if we exceed gas limit
	notExceedsLimit := c.builder.CreateICmp(llvm.IntULE, gasCost, currentGas, "exceeds_gas_limit")

	// Create continuation block
	continueBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "gas_check_continue")
	outOfGasBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "out_of_gas")

	// Branch to out-of-gas block if limit exceeded, otherwise continue
	c.builder.CreateCondBr(notExceedsLimit, continueBlock, outOfGasBlock)

	c.builder.SetInsertPointAtEnd(outOfGasBlock)
	// Store error code and exit
	c.builder.CreateStore(c.u64Const(uint64(VMErrorCodeOutOfGas)), errorCodePtr)
	c.builder.CreateBr(errorBlock)

	c.builder.SetInsertPointAtEnd(continueBlock)
	// Sub gas cost and store
	newGas := c.builder.CreateSub(currentGas, gasCost, "new_gas_used")
	c.builder.CreateStore(newGas, gasPtr)
}

// CompileAndOptimizeStatic compiles using static analysis
func (c *EVMCompiler) CompileAndOptimizeStatic(bytecode []byte, opts *EVMCompilationOpts) error {
	c.initailizeHostFunctions()

	_, err := c.CompileBytecodeStatic(bytecode, opts)
	if err != nil {
		return err
	}
	// TODO: find a correct and safe way to optimize IR.
	if !opts.DisableIROptimization {
		c.OptimizeModule()
	}
	return nil
}
