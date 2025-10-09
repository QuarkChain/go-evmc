// Copyright 2014 The go-ethereum Authors
// This file is part of the go-ethereum library.
//
// The go-ethereum library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The go-ethereum library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the go-ethereum library. If not, see <http://www.gnu.org/licenses/>.

package compiler

import (
	"tinygo.org/x/go-llvm"
)

type (
	gasFuncC        func(ctx llvm.Context, builder llvm.Builder, stack, stackIdxPtr, mem, newMemSize, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock, clibs map[string]CHostFunc) (gas llvm.Value)
	memorySizeFuncC func(ctx llvm.Context, builder llvm.Builder, stack, stackIdxPtr, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) (size llvm.Value)
)

var (
	gasMLoadC   = memoryGasCostC
	gasMStore8C = memoryGasCostC
	gasMStoreC  = memoryGasCostC
)

func (c *EVMCompiler) consumeDynGasAndResizeMem(stack, stackIdxPtr, memory, gasPtr, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock, memSizeFn memorySizeFuncC, gasFn gasFuncC) {
	clibs := c.clibs
	ctx, builder := c.ctx, c.builder
	memSize := memSizeFn(ctx, builder, stack, stackIdxPtr, errorCodePtr, errorBlock)

	// memory is expanded in words of 32 bytes. Gas
	// is also calculated in words.
	towordsizeFn := clibs[TO_WORD_SIZE]
	memWordSize := builder.CreateCall(towordsizeFn.fnType, towordsizeFn.fn, []llvm.Value{memSize}, "call_mem_wordsize")
	const32 := u64Const(ctx, 32)
	newNemSize := c.safeMul(memWordSize, const32, errorCodePtr, errorBlock)

	// Consume the gas and return an error if not enough gas is available.
	dynamicCost := gasFn(ctx, builder, stack, stackIdxPtr, memory, newNemSize, errorCodePtr, errorBlock, clibs)
	c.consumeDyncGas(dynamicCost, gasPtr, errorCodePtr, errorBlock)

	// resize memory
	memResizeFn := clibs[MEMORY_RESIZE]
	builder.CreateCall(memResizeFn.fnType, memResizeFn.fn, []llvm.Value{memory, newNemSize}, "")
}

func memoryGasCostC(ctx llvm.Context, builder llvm.Builder, stack, stackIdxPtr, mem, newMemSize, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock, clibs map[string]CHostFunc) llvm.Value {
	i64 := ctx.Int64Type()
	ptrToDynamicCost := builder.CreateAlloca(i64, "dyn_cost_ptr")
	memDynGas := clibs[MEMORY_GAS_COST]
	errCode := builder.CreateCall(memDynGas.fnType, memDynGas.fn, []llvm.Value{mem, newMemSize, ptrToDynamicCost}, "call_mem_gas_cost")
	// Branch on overflow
	zero := llvm.ConstInt(ctx.Int32Type(), 0, false)
	hasOverflow := builder.CreateICmp(llvm.IntNE, errCode, zero, "u64_overflow")
	checkU64Overflow(ctx, builder, hasOverflow, errorCodePtr, errorBlock)

	return builder.CreateLoad(i64, ptrToDynamicCost, "dyn_cost")
}

func memoryCopierGasC(stackpos int) gasFuncC {
	return func(ctx llvm.Context, builder llvm.Builder, stack, stackIdxPtr, mem, newMemSize, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock, clibs map[string]CHostFunc) llvm.Value {
		words := peekStack(ctx, builder, stack, stackIdxPtr, 2)
		words64 := uint64WithOverflow(ctx, builder, words, errorCodePtr, errorBlock)

		i64 := ctx.Int64Type()
		ptrToDynamicCost := builder.CreateAlloca(i64, "dyn_cost_ptr")
		memCpGasFn := clibs[MEM_CP_GAS_COST]
		errCode := builder.CreateCall(memCpGasFn.fnType, memCpGasFn.fn, []llvm.Value{mem, words64, newMemSize, ptrToDynamicCost}, "call_memcp_gas_cost")

		zero := llvm.ConstInt(ctx.Int32Type(), 0, false)
		hasOverflow := builder.CreateICmp(llvm.IntNE, errCode, zero, "u64_overflow")
		checkU64Overflow(ctx, builder, hasOverflow, errorCodePtr, errorBlock)

		return builder.CreateLoad(i64, ptrToDynamicCost, "dyn_cost")
	}
}
