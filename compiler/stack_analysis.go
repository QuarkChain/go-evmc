package compiler

import (
	"fmt"

	"tinygo.org/x/go-llvm"
)

// StackAnalysis holds static stack analysis results
type StackAnalysis struct {
	stackDepth    map[uint64]int          // PC -> stack depth at that point
	stackValues   map[uint64][]llvm.Value // PC -> known stack values (for optimization)
	maxStackDepth int                     // Maximum stack depth needed
	stackSlots    map[int]llvm.Value      // Stack slot index -> LLVM alloca
}

// StackInstruction represents an instruction's stack effect
type StackInstruction struct {
	PC       uint64
	Opcode   EVMOpcode
	Data     []byte
	StackIn  int // Number of items popped from stack
	StackOut int // Number of items pushed to stack
}

// analyzeStackDepth performs static analysis of stack depth changes
func (c *EVMCompiler) analyzeStackDepth(instructions []EVMInstruction) (*StackAnalysis, error) {
	analysis := &StackAnalysis{
		stackDepth:    make(map[uint64]int),
		stackValues:   make(map[uint64][]llvm.Value),
		maxStackDepth: 0,
		stackSlots:    make(map[int]llvm.Value),
	}

	// Convert to stack instructions with stack effects
	stackInstructions := make([]StackInstruction, len(instructions))
	for i, instr := range instructions {
		stackInstructions[i] = StackInstruction{
			PC:       instr.PC,
			Opcode:   instr.Opcode,
			Data:     instr.Data,
			StackIn:  c.getStackInput(instr.Opcode),
			StackOut: c.getStackOutput(instr.Opcode),
		}
	}

	// Analyze stack depth at each instruction
	currentDepth := 0
	analysis.stackDepth[0] = 0

	for _, instr := range stackInstructions {
		// Check if we have enough items on stack
		if currentDepth < instr.StackIn {
			return nil, fmt.Errorf("stack underflow at PC %d: need %d items, have %d",
				instr.PC, instr.StackIn, currentDepth)
		}

		// Record depth before instruction
		analysis.stackDepth[instr.PC] = currentDepth

		// Update depth after instruction
		currentDepth = currentDepth - instr.StackIn + instr.StackOut

		// Track maximum depth
		if currentDepth > analysis.maxStackDepth {
			analysis.maxStackDepth = currentDepth
		}

		// Check stack overflow (EVM limit is 1024)
		if currentDepth > 1024 {
			return nil, fmt.Errorf("stack overflow at PC %d: depth %d", instr.PC, currentDepth)
		}
	}

	return analysis, nil
}

// getStackInput returns the number of stack items an opcode consumes
func (c *EVMCompiler) getStackInput(opcode EVMOpcode) int {
	switch opcode {
	case STOP, JUMPDEST, PC, MSIZE, GAS:
		return 0
	case POP, JUMP, JUMPI, MLOAD, MSTORE8, SLOAD, SSTORE, RETURN, REVERT:
		return 1
	case ADD, MUL, SUB, DIV, SDIV, MOD, SMOD, EXP, SIGNEXTEND,
		LT, GT, SLT, SGT, EQ, ISZERO, AND, OR, XOR, BYTE,
		SHL, SHR, SAR, MSTORE:
		return 2
	case ADDMOD, MULMOD:
		return 3
	default:
		if opcode >= PUSH1 && opcode <= PUSH32 {
			return 0
		}
		if opcode >= DUP1 && opcode <= DUP16 {
			return 0 // DUP doesn't consume, it duplicates
		}
		if opcode >= SWAP1 && opcode <= SWAP16 {
			return 0 // SWAP doesn't change stack size
		}
		return 0 // Conservative: assume no stack input for unknown opcodes
	}
}

// getStackOutput returns the number of stack items an opcode produces
func (c *EVMCompiler) getStackOutput(opcode EVMOpcode) int {
	switch opcode {
	case STOP, JUMP, JUMPDEST, POP, MSTORE, MSTORE8, SSTORE, RETURN, REVERT:
		return 0
	case ADD, MUL, SUB, DIV, SDIV, MOD, SMOD, EXP, SIGNEXTEND,
		LT, GT, SLT, SGT, EQ, ISZERO, AND, OR, XOR, BYTE,
		SHL, SHR, SAR, PC, MSIZE, GAS, MLOAD, SLOAD, JUMPI:
		return 1
	case ADDMOD, MULMOD:
		return 1
	default:
		if opcode >= PUSH1 && opcode <= PUSH32 {
			return 1
		}
		if opcode >= DUP1 && opcode <= DUP16 {
			return 1 // DUP adds one item
		}
		if opcode >= SWAP1 && opcode <= SWAP16 {
			return 0 // SWAP doesn't change stack size
		}
		return 0 // Conservative: assume no output for unknown opcodes
	}
}

// mapStaticStackSlots maps output stack to each stack slot
func (c *EVMCompiler) mapStaticStackSlots(analysis *StackAnalysis, entryBlock llvm.BasicBlock, outputStackParam llvm.Value) {
	c.builder.SetInsertPointAtEnd(entryBlock)

	for i := 0; i < analysis.maxStackDepth; i++ {
		stackPtrVal := llvm.ConstInt(c.ctx.Int32Type(), uint64(i), false)
		stackElem := llvm.ConstGEP(c.ctx.IntType(256), outputStackParam, []llvm.Value{stackPtrVal})
		analysis.stackSlots[i] = stackElem
	}
}

// CompileBytecodeStaticStack compiles EVM bytecode using static stack analysis
func (c *EVMCompiler) CompileBytecodeStaticStack(bytecode []byte) (llvm.Module, error) {
	instructions, err := c.ParseBytecode(bytecode)
	if err != nil {
		return llvm.Module{}, err
	}

	// Perform static analysis
	pcAnalysis := c.analyzeProgram(instructions)
	stackAnalysis, err := c.analyzeStackDepth(instructions)
	if err != nil {
		return llvm.Module{}, fmt.Errorf("stack analysis failed: %v", err)
	}

	uint256Type := c.ctx.IntType(256)
	uint8PtrType := llvm.PointerType(c.ctx.Int8Type(), 0)
	uint256PtrType := llvm.PointerType(uint256Type, 0)

	execType := llvm.FunctionType(c.ctx.VoidType(), []llvm.Type{
		uint8PtrType,      // memory
		uint256PtrType,    // output stack
		uint8PtrType,      // code (unused)
		c.ctx.Int64Type(), // gas (unused)
	}, false)

	execFunc := llvm.AddFunction(c.module, "execute", execType)
	execFunc.SetFunctionCallConv(llvm.CCallConv)

	memoryParam := execFunc.Param(0)
	outputStackParam := execFunc.Param(1)

	// Create entry block
	entryBlock := llvm.AddBasicBlock(execFunc, "entry")
	c.builder.SetInsertPointAtEnd(entryBlock)

	// Create static stack slots
	c.mapStaticStackSlots(stackAnalysis, entryBlock, outputStackParam)

	// Create exit block
	exitBlock := llvm.AddBasicBlock(execFunc, "exit")

	// Create basic blocks for each instruction
	for _, instr := range instructions {
		blockName := fmt.Sprintf("pc_%d", instr.PC)
		block := llvm.AddBasicBlock(execFunc, blockName)
		pcAnalysis.instructionBlocks[instr.PC] = block
	}

	// Jump to first instruction
	if len(instructions) > 0 {
		firstBlock := pcAnalysis.instructionBlocks[instructions[0].PC]
		c.builder.CreateBr(firstBlock)
	} else {
		c.builder.CreateBr(exitBlock)
	}

	// Compile each instruction
	for _, instr := range instructions {
		block := pcAnalysis.instructionBlocks[instr.PC]
		c.builder.SetInsertPointAtEnd(block)

		nextPC := c.getNextPC(instr, instructions)
		nextBlock := exitBlock
		if nextPC != ^uint64(0) {
			if nb, ok := pcAnalysis.instructionBlocks[nextPC]; ok {
				nextBlock = nb
			}
		}

		currentDepth := stackAnalysis.stackDepth[instr.PC]
		c.compileInstructionStaticStack(instr, memoryParam, pcAnalysis, stackAnalysis,
			currentDepth, nextBlock, exitBlock)
	}

	// Finalize exit block
	c.builder.SetInsertPointAtEnd(exitBlock)
	c.builder.CreateRetVoid()

	err = llvm.VerifyModule(c.module, llvm.ReturnStatusAction)
	if err != nil {
		return llvm.Module{}, fmt.Errorf("module verification failed: %s", err)
	}

	return c.module, nil
}

// compileInstructionStaticStack compiles an instruction using static stack analysis
func (c *EVMCompiler) compileInstructionStaticStack(instr EVMInstruction, memory llvm.Value,
	pcAnalysis *PCAnalysis, stackAnalysis *StackAnalysis, currentDepth int,
	nextBlock, exitBlock llvm.BasicBlock) {

	uint256Type := c.ctx.IntType(256)

	switch instr.Opcode {
	case STOP:
		c.builder.CreateBr(exitBlock)

	case ADD:
		// Pop two values: stack[depth-2] + stack[depth-1] -> stack[depth-2]
		a := c.loadStackSlot(stackAnalysis, currentDepth-1)
		b := c.loadStackSlot(stackAnalysis, currentDepth-2)
		result := c.builder.CreateAdd(a, b, "add_result")
		c.storeStackSlot(stackAnalysis, currentDepth-2, result)
		c.builder.CreateBr(nextBlock)

	case MUL:
		a := c.loadStackSlot(stackAnalysis, currentDepth-1)
		b := c.loadStackSlot(stackAnalysis, currentDepth-2)
		result := c.builder.CreateMul(a, b, "mul_result")
		c.storeStackSlot(stackAnalysis, currentDepth-2, result)
		c.builder.CreateBr(nextBlock)

	case SUB:
		a := c.loadStackSlot(stackAnalysis, currentDepth-1)
		b := c.loadStackSlot(stackAnalysis, currentDepth-2)
		result := c.builder.CreateSub(a, b, "sub_result")
		c.storeStackSlot(stackAnalysis, currentDepth-2, result)
		c.builder.CreateBr(nextBlock)

	case DIV:
		a := c.loadStackSlot(stackAnalysis, currentDepth-1)
		b := c.loadStackSlot(stackAnalysis, currentDepth-2)
		zero := llvm.ConstInt(uint256Type, 0, false)
		isZero := c.builder.CreateICmp(llvm.IntEQ, b, zero, "div_by_zero")
		result := c.builder.CreateSelect(isZero, zero, c.builder.CreateUDiv(a, b, "div_result"), "div_safe")
		c.storeStackSlot(stackAnalysis, currentDepth-2, result)
		c.builder.CreateBr(nextBlock)

	case LT:
		a := c.loadStackSlot(stackAnalysis, currentDepth-1)
		b := c.loadStackSlot(stackAnalysis, currentDepth-2)
		cmp := c.builder.CreateICmp(llvm.IntULT, a, b, "lt_cmp")
		result := c.builder.CreateZExt(cmp, uint256Type, "lt_result")
		c.storeStackSlot(stackAnalysis, currentDepth-2, result)
		c.builder.CreateBr(nextBlock)

	case GT:
		a := c.loadStackSlot(stackAnalysis, currentDepth-1)
		b := c.loadStackSlot(stackAnalysis, currentDepth-2)
		cmp := c.builder.CreateICmp(llvm.IntUGT, a, b, "gt_cmp")
		result := c.builder.CreateZExt(cmp, uint256Type, "gt_result")
		c.storeStackSlot(stackAnalysis, currentDepth-2, result)
		c.builder.CreateBr(nextBlock)

	case EQ:
		a := c.loadStackSlot(stackAnalysis, currentDepth-1)
		b := c.loadStackSlot(stackAnalysis, currentDepth-2)
		cmp := c.builder.CreateICmp(llvm.IntEQ, a, b, "eq_cmp")
		result := c.builder.CreateZExt(cmp, uint256Type, "eq_result")
		c.storeStackSlot(stackAnalysis, currentDepth-2, result)
		c.builder.CreateBr(nextBlock)

	case ISZERO:
		a := c.loadStackSlot(stackAnalysis, currentDepth-1)
		zero := llvm.ConstInt(uint256Type, 0, false)
		cmp := c.builder.CreateICmp(llvm.IntEQ, a, zero, "iszero_cmp")
		result := c.builder.CreateZExt(cmp, uint256Type, "iszero_result")
		c.storeStackSlot(stackAnalysis, currentDepth-1, result)
		c.builder.CreateBr(nextBlock)

	case AND:
		a := c.loadStackSlot(stackAnalysis, currentDepth-1)
		b := c.loadStackSlot(stackAnalysis, currentDepth-2)
		result := c.builder.CreateAnd(a, b, "and_result")
		c.storeStackSlot(stackAnalysis, currentDepth-2, result)
		c.builder.CreateBr(nextBlock)

	case OR:
		a := c.loadStackSlot(stackAnalysis, currentDepth-1)
		b := c.loadStackSlot(stackAnalysis, currentDepth-2)
		result := c.builder.CreateOr(a, b, "or_result")
		c.storeStackSlot(stackAnalysis, currentDepth-2, result)
		c.builder.CreateBr(nextBlock)

	case XOR:
		a := c.loadStackSlot(stackAnalysis, currentDepth-1)
		b := c.loadStackSlot(stackAnalysis, currentDepth-2)
		result := c.builder.CreateXor(a, b, "xor_result")
		c.storeStackSlot(stackAnalysis, currentDepth-2, result)
		c.builder.CreateBr(nextBlock)

	case NOT:
		a := c.loadStackSlot(stackAnalysis, currentDepth-1)
		allOnes := llvm.ConstInt(uint256Type, ^uint64(0), false)
		result := c.builder.CreateXor(a, allOnes, "not_result")
		c.storeStackSlot(stackAnalysis, currentDepth-1, result)
		c.builder.CreateBr(nextBlock)

	case POP:
		// No operation needed - just don't use the top stack value
		c.builder.CreateBr(nextBlock)

	case JUMP:
		target := c.loadStackSlot(stackAnalysis, currentDepth-1)
		c.createDynamicJump(target, pcAnalysis, exitBlock)

	case JUMPI:
		target := c.loadStackSlot(stackAnalysis, currentDepth-1)
		condition := c.loadStackSlot(stackAnalysis, currentDepth-2)
		zero := llvm.ConstInt(uint256Type, 0, false)
		isNonZero := c.builder.CreateICmp(llvm.IntNE, condition, zero, "jumpi_cond")

		jumpBlock := llvm.AddBasicBlock(c.builder.GetInsertBlock().Parent(), "conditional_jump")
		c.builder.CreateCondBr(isNonZero, jumpBlock, nextBlock)

		c.builder.SetInsertPointAtEnd(jumpBlock)
		c.createDynamicJump(target, pcAnalysis, exitBlock)

	case JUMPDEST:
		c.builder.CreateBr(nextBlock)

	case PC:
		pcValue := llvm.ConstInt(uint256Type, instr.PC, false)
		c.storeStackSlot(stackAnalysis, currentDepth, pcValue)
		c.builder.CreateBr(nextBlock)

	case MLOAD:
		offset := c.loadStackSlot(stackAnalysis, currentDepth-1)
		value := c.loadFromMemory(memory, offset)
		c.storeStackSlot(stackAnalysis, currentDepth-1, value)
		c.builder.CreateBr(nextBlock)

	case MSTORE:
		offset := c.loadStackSlot(stackAnalysis, currentDepth-1)
		value := c.loadStackSlot(stackAnalysis, currentDepth-2)
		c.storeToMemory(memory, offset, value)
		c.builder.CreateBr(nextBlock)

	case MSTORE8:
		offset := c.loadStackSlot(stackAnalysis, currentDepth-1)
		value := c.loadStackSlot(stackAnalysis, currentDepth-2)
		c.storeByteToMemory(memory, offset, value)
		c.builder.CreateBr(nextBlock)

	case RETURN, REVERT:
		// For now, just exit - could implement proper return data handling
		c.builder.CreateBr(exitBlock)

	default:
		if instr.Opcode >= PUSH1 && instr.Opcode <= PUSH32 {
			c.compilePushStaticStack(instr, stackAnalysis, currentDepth, nextBlock)
		} else if instr.Opcode >= DUP1 && instr.Opcode <= DUP16 {
			c.compileDupStaticStack(instr, stackAnalysis, currentDepth, nextBlock)
		} else if instr.Opcode >= SWAP1 && instr.Opcode <= SWAP16 {
			c.compileSwapStaticStack(instr, stackAnalysis, currentDepth, nextBlock)
		} else {
			c.builder.CreateBr(nextBlock)
		}
	}
}

// loadStackSlot loads a value from a specific stack slot
func (c *EVMCompiler) loadStackSlot(analysis *StackAnalysis, index int) llvm.Value {
	if slot, ok := analysis.stackSlots[index]; ok {
		return c.builder.CreateLoad(c.ctx.IntType(256), slot, fmt.Sprintf("stack_%d", index))
	}
	panic(fmt.Sprintf("failed to load stack slot %d", index))
}

// storeStackSlot stores a value to a specific stack slot
func (c *EVMCompiler) storeStackSlot(analysis *StackAnalysis, index int, value llvm.Value) {
	if slot, ok := analysis.stackSlots[index]; ok {
		c.builder.CreateStore(value, slot)
		return
	}
	panic(fmt.Sprintf("failed to store stack slot %d", index))
}

// compilePushStaticStack compiles PUSH instructions with static stack
func (c *EVMCompiler) compilePushStaticStack(instr EVMInstruction, stackAnalysis *StackAnalysis,
	currentDepth int, nextBlock llvm.BasicBlock) {

	value := c.createUint256ConstantFromBytes(instr.Data)
	c.storeStackSlot(stackAnalysis, currentDepth, value)
	c.builder.CreateBr(nextBlock)
}

// compileDupStaticStack compiles DUP instructions with static stack
func (c *EVMCompiler) compileDupStaticStack(instr EVMInstruction, stackAnalysis *StackAnalysis,
	currentDepth int, nextBlock llvm.BasicBlock) {

	n := int(instr.Opcode - DUP1 + 1)
	sourceIndex := currentDepth - n
	value := c.loadStackSlot(stackAnalysis, sourceIndex)
	c.storeStackSlot(stackAnalysis, currentDepth, value)
	c.builder.CreateBr(nextBlock)
}

// compileSwapStaticStack compiles SWAP instructions with static stack
func (c *EVMCompiler) compileSwapStaticStack(instr EVMInstruction, stackAnalysis *StackAnalysis,
	currentDepth int, nextBlock llvm.BasicBlock) {

	n := int(instr.Opcode - SWAP1 + 1)
	index1 := currentDepth - 1
	index2 := currentDepth - 1 - n

	value1 := c.loadStackSlot(stackAnalysis, index1)
	value2 := c.loadStackSlot(stackAnalysis, index2)

	c.storeStackSlot(stackAnalysis, index1, value2)
	c.storeStackSlot(stackAnalysis, index2, value1)
	c.builder.CreateBr(nextBlock)
}
