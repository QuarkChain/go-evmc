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

import "tinygo.org/x/go-llvm"

func u64Const(ctx llvm.Context, value uint64) llvm.Value {
	return llvm.ConstInt(ctx.Int64Type(), value, false)
}

func calcMemSize64C(ctx llvm.Context, builder llvm.Builder, offset64, length64, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) llvm.Value {
	val := builder.CreateAdd(offset64, length64, "val")
	hasOverflow := builder.CreateICmp(llvm.IntULT, val, offset64, "u64_overflow")

	checkU64Overflow(ctx, builder, hasOverflow, errorCodePtr, errorBlock)
	return val
}

func uint64WithOverflow(ctx llvm.Context, builder llvm.Builder, value, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) llvm.Value {
	i64Type := ctx.Int64Type()
	i256Type := ctx.IntType(256)
	valueU64 := builder.CreateTrunc(value, i64Type, "offset64") // lossy
	back := builder.CreateZExt(valueU64, i256Type, "back")
	notEqual := builder.CreateICmp(llvm.IntNE, back, value, "overflow256to64")

	checkU64Overflow(ctx, builder, notEqual, errorCodePtr, errorBlock)
	return valueU64
}

func checkU64Overflow(ctx llvm.Context, builder llvm.Builder, overflow, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) {
	continueBlock := llvm.AddBasicBlock(builder.GetInsertBlock().Parent(), "continue")
	u64OverflowBlock := llvm.AddBasicBlock(builder.GetInsertBlock().Parent(), "u64_overflow")
	// Branch to stack overflow block if limit exceeded, otherwise continue
	builder.CreateCondBr(overflow, u64OverflowBlock, continueBlock)

	builder.SetInsertPointAtEnd(u64OverflowBlock)
	// Store error code and exit
	errCode := llvm.ConstInt(ctx.Int64Type(), uint64(VMErrorCodeGasUintOverflow), false)
	builder.CreateStore(errCode, errorCodePtr)
	builder.CreateBr(errorBlock)
	builder.SetInsertPointAtEnd(continueBlock)
}

func memoryMLoadC(ctx llvm.Context, builder llvm.Builder, stack, stackIdxPtr, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) llvm.Value {
	stackElem := peekStackPtr(ctx, builder, stack, stackIdxPtr)
	offset := builder.CreateLoad(ctx.IntType(256), stackElem, "mload_stack_value")
	offset64 := uint64WithOverflow(ctx, builder, offset, errorCodePtr, errorBlock)
	length64 := u64Const(ctx, 32)
	return calcMemSize64C(ctx, builder, offset64, length64, errorCodePtr, errorBlock)
}

func memoryMStore8C(ctx llvm.Context, builder llvm.Builder, stack, stackIdxPtr, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) llvm.Value {
	stackElem := peekStackPtr(ctx, builder, stack, stackIdxPtr)
	offset := builder.CreateLoad(ctx.IntType(256), stackElem, "mload_stack_value")
	offset64 := uint64WithOverflow(ctx, builder, offset, errorCodePtr, errorBlock)
	length64 := u64Const(ctx, 1)
	return calcMemSize64C(ctx, builder, offset64, length64, errorCodePtr, errorBlock)
}

func memoryMcopyC(ctx llvm.Context, builder llvm.Builder, stack, stackIdxPtr, errorCodePtr llvm.Value, errorBlock llvm.BasicBlock) llvm.Value {
	mStart := peekStack(ctx, builder, stack, stackIdxPtr, 0)
	s1 := peekStack(ctx, builder, stack, stackIdxPtr, 1)

	bitIsSet := builder.CreateICmp(llvm.IntUGT, s1, mStart, "gt_cmp")
	offset := builder.CreateSelect(bitIsSet, s1, mStart, "signed_val")

	offset64 := uint64WithOverflow(ctx, builder, offset, errorCodePtr, errorBlock)
	length := peekStack(ctx, builder, stack, stackIdxPtr, 2)
	length64 := uint64WithOverflow(ctx, builder, length, errorCodePtr, errorBlock)
	return calcMemSize64C(ctx, builder, offset64, length64, errorCodePtr, errorBlock)
}

var (
	memoryMStoreC = memoryMLoadC
)
