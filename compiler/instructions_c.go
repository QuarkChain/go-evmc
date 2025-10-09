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

func opMstoreC(ctx llvm.Context, builder llvm.Builder, memory, stack, stackIdxPtr llvm.Value, clibs map[string]CHostFunc) {
	bswapFn := clibs[BIT_SWAP256]
	// convert value from machine's endian(little-endian) to big-endian
	offset := popStack(ctx, builder, stack, stackIdxPtr)
	value := popStack(ctx, builder, stack, stackIdxPtr)

	value = builder.CreateCall(bswapFn.fnType, bswapFn.fn, []llvm.Value{value}, "mstore_val_be")

	// allocate stack space for int256
	valuePtr := builder.CreateAlloca(ctx.IntType(256), "int256_ptr")
	builder.CreateStore(value, valuePtr)
	valueBytesPtr := builder.CreateBitCast(valuePtr, llvm.PointerType(ctx.Int8Type(), 0), "value_bytes_ptr")

	memSetFn := clibs[MEMORY_SET]
	offset64 := builder.CreateTrunc(offset, ctx.Int64Type(), "offset64")
	size64 := u64Const(ctx, 32)
	builder.CreateCall(memSetFn.fnType, memSetFn.fn, []llvm.Value{
		memory,        // Memory*
		offset64,      // uint64_t offset
		size64,        // uint64_t size
		valueBytesPtr, // const uint8_t* value
	}, "")
}

func opMstore8C(ctx llvm.Context, builder llvm.Builder, memory, stack, stackIdxPtr llvm.Value, clibs map[string]CHostFunc) {
	offset := popStack(ctx, builder, stack, stackIdxPtr)
	value := popStack(ctx, builder, stack, stackIdxPtr)

	valueBytesPtr := builder.CreateAlloca(ctx.Int8Type(), "int8_ptr")
	builder.CreateStore(value, valueBytesPtr)

	memSetFn := clibs[MEMORY_SET]
	offset64 := builder.CreateTrunc(offset, ctx.Int64Type(), "offset64")
	size64 := u64Const(ctx, 1)
	builder.CreateCall(memSetFn.fnType, memSetFn.fn, []llvm.Value{
		memory,        // Memory*
		offset64,      // uint64_t offset
		size64,        // uint64_t size
		valueBytesPtr, // const uint8_t* value
	}, "")
}

func opMloadC(ctx llvm.Context, builder llvm.Builder, memory, stack, stackIdxPtr llvm.Value, clibs map[string]CHostFunc) {
	// Obtain stack value and store new stack idx
	stackElem := peekStackPtr(ctx, builder, stack, stackIdxPtr)

	offset := builder.CreateLoad(ctx.IntType(256), stackElem, "mload_stack_value")
	offset64 := builder.CreateTrunc(offset, ctx.Int64Type(), "offset64")

	// Get pointer to "store" field (index 0 in struct Memory)
	storePtr := builder.CreateGEP(memory.Type(), memory, []llvm.Value{
		// offset64,
		llvm.ConstInt(ctx.Int32Type(), 0, false),
	}, "mload_store_ptr")

	// Load the actual `uint8_t*`
	storeVal := builder.CreateLoad(llvm.PointerType(ctx.Int8Type(), 0), storePtr, "mload_store_val")

	// GEP into store[offset64]
	bytePtr := builder.CreateGEP(ctx.Int8Type(), storeVal, []llvm.Value{offset64}, "mload_byte_ptr")

	// Step 4: Cast to i256* (assuming memory is large enough and properly aligned)
	i256Ptr := builder.CreateBitCast(bytePtr, llvm.PointerType(ctx.IntType(256), 0), "mload_i256_ptr")

	// Load 256-bit big-endian value
	value := builder.CreateLoad(ctx.IntType(256), i256Ptr, "mload_val")
	// convert value to machine's endian(little-endian)
	bswapsFn := clibs[BIT_SWAP256]
	value = builder.CreateCall(bswapsFn.fnType, bswapsFn.fn, []llvm.Value{value}, "call_bitswap256")

	builder.CreateStore(value, stackElem)
}

func opMcopyC(ctx llvm.Context, builder llvm.Builder, memory, stack, stackIdxPtr llvm.Value, clibs map[string]CHostFunc) {
	dst := popStack(ctx, builder, stack, stackIdxPtr)
	src := popStack(ctx, builder, stack, stackIdxPtr)
	length := popStack(ctx, builder, stack, stackIdxPtr)

	i64 := ctx.Int64Type()
	dst64 := builder.CreateTrunc(dst, i64, "dst_64")
	src64 := builder.CreateTrunc(src, i64, "src_64")
	length64 := builder.CreateTrunc(length, i64, "length_64")
	// These values are checked for overflow during memory expansion calculation
	// (the memorySize function on the opcode).
	memCpFn := clibs[MEMORY_COPY]
	builder.CreateCall(memCpFn.fnType, memCpFn.fn, []llvm.Value{memory, dst64, src64, length64}, "")
}
