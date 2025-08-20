package compiler

import (
	"fmt"

	"github.com/ethereum/go-ethereum/crypto"
	"github.com/holiman/uint256"
	"tinygo.org/x/go-llvm"
)

const (
	OUTPUT_IDX_GAS         = 0
	OUTPUT_IDX_STACK_DEPTH = 1
	OUTPUT_SIZE            = (OUTPUT_IDX_STACK_DEPTH + 1) * 8
)

var (
	INT256_NEGATIVE_1   = uint256.MustFromHex("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff").String() // -1
	INT256_NEGATIVE_MIN = uint256.MustFromHex("0x8000000000000000000000000000000000000000000000000000000000000000").String() // -2^255
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
	v := llvm.ConstGEP(c.ctx.Int64Type(), outputPtr, []llvm.Value{llvm.ConstInt(c.ctx.Int32Type(), uint64(idx), false)})
	c.builder.CreateStore(value, v)
}

// CompileBytecodeStatic compiles EVM bytecode using static PC analysis
func (c *EVMCompiler) CompileBytecodeStatic(bytecode []byte, opts *EVMCompilationOpts) (llvm.Module, error) {
	instructions, err := c.ParseBytecode(bytecode)
	if err != nil {
		return llvm.Module{}, err
	}

	// Perform static analysis
	analysis := c.analyzeProgram(instructions)

	uint256Type := c.ctx.IntType(256)
	uint8PtrType := llvm.PointerType(c.ctx.Int8Type(), 0)
	uint256PtrType := llvm.PointerType(uint256Type, 0)
	uint64PtrType := llvm.PointerType(c.ctx.Int64Type(), 0)

	execType := llvm.FunctionType(c.ctx.VoidType(), []llvm.Type{
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
	stackPtr := c.builder.CreateAlloca(c.ctx.Int64Type(), "stack_ptr")
	c.builder.CreateStore(llvm.ConstInt(c.ctx.Int64Type(), 0, false), stackPtr)

	// Initialize gasPtr tracking
	var gasPtr llvm.Value
	if !opts.DisableGas {
		gasPtr = c.builder.CreateAlloca(c.ctx.Int64Type(), "gas_used")
		c.builder.CreateStore(gasLimitParam, gasPtr)
	}

	// Create out-of-gas block
	var outOfGasBlock llvm.BasicBlock
	if !opts.DisableGas {
		outOfGasBlock = llvm.AddBasicBlock(execFunc, "out_of_gas")
	}

	// Consume the initial section gas
	if !opts.DisableGas && !opts.DisableSectionGasOptimization {
		c.consumeSectionGas(c.initSectionGas, gasPtr, outOfGasBlock)
	}

	// Create exit block
	exitBlock := llvm.AddBasicBlock(execFunc, "exit")

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

		c.compileInstructionStatic(instr, instParam, stackParam, stackPtr, gasPtr, analysis, nextBlock, exitBlock, outOfGasBlock, opts)
	}

	// Finalize out-of-gas block
	if !opts.DisableGas {
		c.builder.SetInsertPointAtEnd(outOfGasBlock)
		c.setOutputValueAt(outputPtrParam, OUTPUT_IDX_GAS, llvm.ConstInt(c.ctx.Int64Type(), ^uint64(0), false)) // Return -1 for out of gas
		c.builder.CreateRetVoid()
	}

	// Finalize exit block
	c.builder.SetInsertPointAtEnd(exitBlock)
	if opts.DisableGas {
		c.setOutputValueAt(outputPtrParam, OUTPUT_IDX_GAS, llvm.ConstInt(c.ctx.Int64Type(), uint64(0), false))
	} else {
		finalGasUsed := c.builder.CreateLoad(c.ctx.Int64Type(), gasPtr, "final_gas_used")
		c.setOutputValueAt(outputPtrParam, OUTPUT_IDX_GAS, finalGasUsed)
	}
	stackDepth := c.builder.CreateLoad(c.ctx.Int64Type(), stackPtr, "stack_depth")
	c.setOutputValueAt(outputPtrParam, OUTPUT_IDX_STACK_DEPTH, stackDepth)
	c.builder.CreateRetVoid()

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
	for i := range instructions {
		instr := &instructions[i]
		analysis.pcToInstruction[instr.PC] = instr

		sectionGas += getGasCost(instr.Opcode)

		if instr.Opcode == JUMPDEST {
			analysis.jumpTargets[instr.PC] = true
		}

		// TODO: Add CALL code
		if instr.Opcode == JUMPDEST || instr.Opcode == JUMP || instr.Opcode == JUMPI || instr.Opcode == STOP {
			// end of section
			if instr.Opcode == JUMPDEST {
				// For JUMPDEST, the section ends at PC-1
				// o.w., the section ends at PC.
				sectionGas -= getGasCost(instr.Opcode)
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
					sectionGas = getGasCost(instr.Opcode)
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
	// TODO: check if section gas is charged properly if the code does not end with STOP.

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

func (c *EVMCompiler) checkHostReturn(ret llvm.Value, nextBlock, outOfGasBlock llvm.BasicBlock) {
	switchInstr := c.builder.CreateSwitch(ret, nextBlock, 1)
	switchInstr.AddCase(llvm.ConstInt(c.ctx.Int64Type(), uint64(ExecutionOutOfGas), false), outOfGasBlock)
	// TODO: other errors
}

// compileInstructionStatic compiles an instruction using static analysis with gas metering
func (c *EVMCompiler) compileInstructionStatic(instr EVMInstruction, execInst, stack, stackPtr, gasPtr llvm.Value, analysis *PCAnalysis, nextBlock, exitBlock, outOfGasBlock llvm.BasicBlock, opts *EVMCompilationOpts) {
	uint256Type := c.ctx.IntType(256)

	// Add gas consumption for this instruction
	if !opts.DisableGas {
		if opts.DisableSectionGasOptimization {
			c.consumeGas(instr.Opcode, gasPtr, outOfGasBlock)
		} else {
			gasCost := analysis.sectionGas[instr.PC]
			c.consumeSectionGas(gasCost, gasPtr, outOfGasBlock)
		}
	}

	switch instr.Opcode {
	case STOP:
		c.builder.CreateBr(exitBlock)

	case ADD:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
		result := c.builder.CreateAdd(a, b, "add_result")
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case MUL:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
		result := c.builder.CreateMul(a, b, "mul_result")
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case SUB:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
		result := c.builder.CreateSub(a, b, "sub_result")
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case DIV:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
		zero := llvm.ConstInt(uint256Type, 0, false)
		isZero := c.builder.CreateICmp(llvm.IntEQ, b, zero, "div_by_zero")
		result := c.builder.CreateSelect(isZero, zero, c.builder.CreateUDiv(a, b, "div_result"), "div_safe")
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case SDIV:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
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
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case MOD:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
		result := c.builder.CreateURem(a, b, "mod_result") // mod by zero is already supported
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case SMOD:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
		result := c.builder.CreateSRem(a, b, "smod_result") // mod by zero is already supported
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	// case EXP:
	// a := c.popStack(stack, stackPtr)
	// b := c.popStack(stack, stackPtr)
	// result := c.builder. (a, b, "exp_result")
	// c.pushStack(stack, stackPtr, result)
	// c.builder.CreateBr(nextBlock)

	case SIGNEXTEND:
		// Adapted from revm: https://github.com/bluealloy/revm/blob/fda371f73aba2c30a83c639608be78145fd1123b/crates/interpreter/src/instructions/arithmetic.rs#L89
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
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
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case LT, GT, SLT, SGT, EQ:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)

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
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case ISZERO:
		a := c.popStack(stack, stackPtr)
		zero := llvm.ConstInt(uint256Type, 0, false)
		cmp := c.builder.CreateICmp(llvm.IntEQ, a, zero, "iszero_cmp")
		result := c.builder.CreateZExt(cmp, uint256Type, "iszero_result")
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case AND:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
		result := c.builder.CreateAnd(a, b, "and_result")
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case OR:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
		result := c.builder.CreateOr(a, b, "or_result")
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case XOR:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
		result := c.builder.CreateXor(a, b, "xor_result")
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case NOT:
		a := c.popStack(stack, stackPtr)
		allOnes := llvm.ConstIntFromString(uint256Type, INT256_NEGATIVE_1, 10)
		result := c.builder.CreateXor(a, allOnes, "not_result")
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case BYTE:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
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
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case SHL:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
		result := c.builder.CreateShl(a, b, "shl_result")
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case SHR:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
		result := c.builder.CreateLShr(a, b, "shr_result")
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case SAR:
		a := c.popStack(stack, stackPtr)
		b := c.popStack(stack, stackPtr)
		result := c.builder.CreateAShr(a, b, "sar_result")
		c.pushStack(stack, stackPtr, result)
		c.builder.CreateBr(nextBlock)

	case COINBASE:
		c.pushStackEmpty(stackPtr) // allocate a stack variable to write
		c.builder.CreateCall(c.hostFuncType, c.hostFunc, []llvm.Value{execInst, llvm.ConstInt(c.ctx.Int64Type(), uint64(instr.Opcode), false), gasPtr, c.peekStackPtr(stack, stackPtr)}, "")
		c.builder.CreateBr(nextBlock)

	case POP:
		c.popStack(stack, stackPtr)
		c.builder.CreateBr(nextBlock)

	case JUMP:
		target := c.popStack(stack, stackPtr)
		// Create dynamic jump using switch
		c.createDynamicJump(target, analysis, exitBlock)

	case JUMPI:
		target := c.popStack(stack, stackPtr)
		condition := c.popStack(stack, stackPtr)
		zero := llvm.ConstInt(uint256Type, 0, false)
		isNonZero := c.builder.CreateICmp(llvm.IntNE, condition, zero, "jumpi_cond")

		// Create conditional branch: jump if condition != 0, otherwise continue
		jumpBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "conditional_jump")
		c.builder.CreateCondBr(isNonZero, jumpBlock, nextBlock)

		c.builder.SetInsertPointAtEnd(jumpBlock)
		c.createDynamicJump(target, analysis, exitBlock)

	case JUMPDEST:
		// JUMPDEST is a no-op, charge the section gas before continue to next instruction
		c.builder.CreateBr(nextBlock)

	case PC:
		// Push current PC as a constant (static analysis!)
		pcValue := llvm.ConstInt(uint256Type, instr.PC, false)
		c.pushStack(stack, stackPtr, pcValue)
		c.builder.CreateBr(nextBlock)

	case MLOAD:
		ret := c.builder.CreateCall(c.hostFuncType, c.hostFunc, []llvm.Value{execInst, llvm.ConstInt(c.ctx.Int64Type(), uint64(instr.Opcode), false), gasPtr, c.peekStackPtr(stack, stackPtr)}, "")
		c.checkHostReturn(ret, nextBlock, outOfGasBlock)

	case MSTORE:
		ret := c.builder.CreateCall(c.hostFuncType, c.hostFunc, []llvm.Value{execInst, llvm.ConstInt(c.ctx.Int64Type(), uint64(instr.Opcode), false), gasPtr, c.peekStackPtr(stack, stackPtr)}, "")
		c.popStack(stack, stackPtr)
		c.popStack(stack, stackPtr)
		c.checkHostReturn(ret, nextBlock, outOfGasBlock)

	case MSTORE8:
		ret := c.builder.CreateCall(c.hostFuncType, c.hostFunc, []llvm.Value{execInst, llvm.ConstInt(c.ctx.Int64Type(), uint64(instr.Opcode), false), gasPtr, c.peekStackPtr(stack, stackPtr)}, "")
		c.popStack(stack, stackPtr)
		c.popStack(stack, stackPtr)
		c.checkHostReturn(ret, nextBlock, outOfGasBlock)

	case SSTORE:
		ret := c.builder.CreateCall(c.hostFuncType, c.hostFunc, []llvm.Value{execInst, llvm.ConstInt(c.ctx.Int64Type(), uint64(instr.Opcode), false), gasPtr, c.peekStackPtr(stack, stackPtr)}, "")
		c.popStack(stack, stackPtr)
		c.popStack(stack, stackPtr)
		c.checkHostReturn(ret, nextBlock, outOfGasBlock)

	case SLOAD:
		ret := c.builder.CreateCall(c.hostFuncType, c.hostFunc, []llvm.Value{execInst, llvm.ConstInt(c.ctx.Int64Type(), uint64(instr.Opcode), false), gasPtr, c.peekStackPtr(stack, stackPtr)}, "")
		c.checkHostReturn(ret, nextBlock, outOfGasBlock)

	case RETURN:
		_ = c.popStack(stack, stackPtr) // offset
		_ = c.popStack(stack, stackPtr) // size
		c.builder.CreateBr(exitBlock)

	case REVERT:
		_ = c.popStack(stack, stackPtr) // offset
		_ = c.popStack(stack, stackPtr) // size
		c.builder.CreateBr(exitBlock)

	default:
		if instr.Opcode >= PUSH1 && instr.Opcode <= PUSH32 {
			c.compilePushStatic(instr, stack, stackPtr, nextBlock)
		} else if instr.Opcode >= DUP1 && instr.Opcode <= DUP16 {
			c.compileDupStatic(instr, stack, stackPtr, nextBlock)
		} else if instr.Opcode >= SWAP1 && instr.Opcode <= SWAP16 {
			c.compileSwapStatic(instr, stack, stackPtr, nextBlock)
		} else {
			// Unknown opcode, TODO: generate code to return err
			panic(fmt.Sprintf("unsupported op %d", instr.Opcode))
			// c.builder.CreateBr(nextBlock)
		}
	}
}

// createDynamicJump creates a switch statement for dynamic jumps
func (c *EVMCompiler) createDynamicJump(target llvm.Value, analysis *PCAnalysis, exitBlock llvm.BasicBlock) {
	// Truncate 256-bit target to 64-bit PC
	targetPC := c.builder.CreateTrunc(target, c.ctx.Int64Type(), "jump_target")

	// Create switch instruction with all valid jump targets
	switchInstr := c.builder.CreateSwitch(targetPC, exitBlock, len(analysis.jumpTargets))

	// Add cases for all valid JUMPDEST locations
	for pc := range analysis.jumpTargets {
		if block, ok := analysis.instructionBlocks[pc]; ok {
			pcConstant := llvm.ConstInt(c.ctx.Int64Type(), pc, false)
			switchInstr.AddCase(pcConstant, block)
		}
	}
	// TODO: add invalid JUMPDEST
}

// compilePushStatic compiles PUSH instructions with static next block
func (c *EVMCompiler) compilePushStatic(instr EVMInstruction, stack, stackPtr llvm.Value, nextBlock llvm.BasicBlock) {
	llvmValue := c.createUint256ConstantFromBytes(instr.Data)
	c.pushStack(stack, stackPtr, llvmValue)
	c.builder.CreateBr(nextBlock)
}

// compileDupStatic compiles DUP instructions with static next block
func (c *EVMCompiler) compileDupStatic(instr EVMInstruction, stack, stackPtr llvm.Value, nextBlock llvm.BasicBlock) {
	n := int(instr.Opcode - DUP1 + 1)
	stackPtrVal := c.builder.CreateLoad(c.ctx.Int32Type(), stackPtr, "stack_ptr_val")
	index := c.builder.CreateSub(stackPtrVal, llvm.ConstInt(c.ctx.Int32Type(), uint64(n), false), "dup_index")
	stackElem := c.builder.CreateGEP(c.ctx.IntType(256), stack, []llvm.Value{index}, "stack_elem")
	value := c.builder.CreateLoad(c.ctx.IntType(256), stackElem, "dup_value")
	c.pushStack(stack, stackPtr, value)
	c.builder.CreateBr(nextBlock)
}

// compileSwapStatic compiles SWAP instructions with static next block
func (c *EVMCompiler) compileSwapStatic(instr EVMInstruction, stack, stackPtr llvm.Value, nextBlock llvm.BasicBlock) {
	n := int(instr.Opcode - SWAP1 + 1)
	stackPtrVal := c.builder.CreateLoad(c.ctx.Int32Type(), stackPtr, "stack_ptr_val")

	index1 := c.builder.CreateSub(stackPtrVal, llvm.ConstInt(c.ctx.Int32Type(), 1, false), "swap_index1")
	index2 := c.builder.CreateSub(stackPtrVal, llvm.ConstInt(c.ctx.Int32Type(), uint64(n+1), false), "swap_index2")

	elem1 := c.builder.CreateGEP(c.ctx.IntType(256), stack, []llvm.Value{index1}, "swap_elem1")
	elem2 := c.builder.CreateGEP(c.ctx.IntType(256), stack, []llvm.Value{index2}, "swap_elem2")

	value1 := c.builder.CreateLoad(c.ctx.IntType(256), elem1, "swap_value1")
	value2 := c.builder.CreateLoad(c.ctx.IntType(256), elem2, "swap_value2")

	c.builder.CreateStore(value2, elem1)
	c.builder.CreateStore(value1, elem2)
	c.builder.CreateBr(nextBlock)
}

// consumeGas adds gas consumption for an opcode and checks for out-of-gas condition
func (c *EVMCompiler) consumeGas(opcode EVMOpcode, gasPtr llvm.Value, outOfGasBlock llvm.BasicBlock) {
	// Get gas cost for this opcode
	gasCost := getGasCost(opcode)
	if gasCost == 0 {
		return // No gas consumption for this opcode
	}

	// Load current gas used
	currentGas := c.builder.CreateLoad(c.ctx.Int64Type(), gasPtr, "gas_remaining")

	// Check if we exceed gas limit
	gasCostValue := llvm.ConstInt(c.ctx.Int64Type(), gasCost, false)
	exceedsLimit := c.builder.CreateICmp(llvm.IntUGT, gasCostValue, currentGas, "exceeds_gas_limit")

	// Create continuation block
	continueBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "gas_check_continue")

	// Branch to out-of-gas block if limit exceeded, otherwise continue
	c.builder.CreateCondBr(exceedsLimit, outOfGasBlock, continueBlock)

	c.builder.SetInsertPointAtEnd(continueBlock)

	// Sub gas cost and store
	newGas := c.builder.CreateSub(currentGas, gasCostValue, "new_gas_used")
	c.builder.CreateStore(newGas, gasPtr)
}

// consumeGas adds gas consumption for an opcode and checks for out-of-gas condition
func (c *EVMCompiler) consumeSectionGas(gasCost uint64, gasPtr llvm.Value, outOfGasBlock llvm.BasicBlock) {
	// Get gas cost for this opcode
	if gasCost == 0 {
		return // No gas consumption for this opcode
	}

	// Load current gas used
	currentGas := c.builder.CreateLoad(c.ctx.Int64Type(), gasPtr, "gas_remaining")

	// Check if we exceed gas limit
	gasCostValue := llvm.ConstInt(c.ctx.Int64Type(), gasCost, false)
	exceedsLimit := c.builder.CreateICmp(llvm.IntUGT, gasCostValue, currentGas, "exceeds_gas_limit")

	// Create continuation block
	continueBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "gas_check_continue")

	// Branch to out-of-gas block if limit exceeded, otherwise continue
	c.builder.CreateCondBr(exceedsLimit, outOfGasBlock, continueBlock)

	c.builder.SetInsertPointAtEnd(continueBlock)

	// Sub gas cost and store
	newGas := c.builder.CreateSub(currentGas, gasCostValue, "new_gas_used")
	c.builder.CreateStore(newGas, gasPtr)
}

// CompileAndOptimizeStatic compiles using static analysis
func (c *EVMCompiler) CompileAndOptimizeStatic(bytecode []byte, opts *EVMCompilationOpts) error {
	c.initailizeHostFunctions()
	c.contractAddress = opts.ContractAddress
	c.codeHash = crypto.Keccak256Hash(bytecode)

	_, err := c.CompileBytecodeStatic(bytecode, opts)
	if err != nil {
		return err
	}

	c.OptimizeModule()
	return nil
}
