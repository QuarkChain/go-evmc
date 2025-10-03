#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdbool.h>
#include "mem.h"
#include "protocol_params.h"

#define MEMORY_WORD_SIZE 32
#define QUAD_COEFF_DIV 512 // params.QuadCoeffDiv
#define MAX_MEM_SIZE 0x1FFFFFFFE0ULL
#define VMErrorCode_GasUintOverflow 11

// Round byte size to words of 32 bytes
uint64_t to_word_size(uint64_t size)
{
    if (size > UINT64_MAX - 31)
    {
        return UINT64_MAX / 32 + 1;
    }
    return (size + MEMORY_WORD_SIZE - 1) / MEMORY_WORD_SIZE;
}

static inline uint64_t safe_mul(uint64_t x, uint64_t y, bool *overflow)
{
    __uint128_t product = (__uint128_t)x * (__uint128_t)y;
    uint64_t lo = (uint64_t)product;
    uint64_t hi = (uint64_t)(product >> 64);

    *overflow = (hi != 0);
    return lo;
}

static inline uint64_t safe_add(uint64_t x, uint64_t y, bool *overflow)
{
    uint64_t sum = x + y;
    *overflow = (sum < x); // true if overflow
    return sum;
}

// Gas calculation for memory expansion
int memory_gas_cost(Memory *mem, uint64_t newMemSize, uint64_t *outFee)
{
    if (!mem || !outFee)
    {
        fprintf(stderr, "memory_gas_cost: memory or outfee is nil\n");
        abort();
    }

    if (newMemSize == 0)
    {
        *outFee = 0;
        return 0;
    }

    if (newMemSize > MAX_MEM_SIZE)
    {
        return VMErrorCode_GasUintOverflow; // overflow
    }

    uint64_t newMemSizeWords = to_word_size(newMemSize);
    newMemSize = newMemSizeWords * MEMORY_WORD_SIZE;

    if (newMemSize > mem->len)
    {
        // quadratic gas calculation
        uint64_t square = newMemSizeWords * newMemSizeWords;
        uint64_t linCoef = newMemSizeWords * MEMORY_GAS;
        uint64_t quadCoef = square / QUAD_COEFF_DIV;
        uint64_t newTotalFee = linCoef + quadCoef;

        uint64_t fee = newTotalFee - mem->lastGasCost;
        mem->lastGasCost = newTotalFee;

        *outFee = fee;
        return 0;
    }

    *outFee = 0;
    return 0;
}

int memory_copier_gas(Memory *mem, uint64_t words, uint64_t memorySize, uint64_t *outGas)
{
    uint64_t gas = 0;

    // 1. Gas for expanding memory
    int errCode = memory_gas_cost(mem, memorySize, outGas);
    if (errCode != 0)
    {
        return errCode;
    }

    // 2. Safe multiply
    uint64_t wsize = to_word_size(words);
    bool overflow = false;
    words = safe_mul(wsize, COPY_GAS, &overflow);
    if (overflow)
    {
        return VMErrorCode_GasUintOverflow;
    }
    *outGas = safe_add(*outGas, words, &overflow);
    if (overflow)
    {
        return VMErrorCode_GasUintOverflow;
    }
    return 0;
}