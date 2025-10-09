#ifndef MEM_H
#define MEM_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    uint8_t *store;
    size_t len;
    size_t cap;
    uint64_t lastGasCost;
} Memory;

// lifecycle
Memory* memory_new(void);
void memory_free(Memory* m);

// sizing / access
void memory_resize(Memory* m, uint64_t size);
void memory_set(Memory* m, uint64_t offset, uint64_t size, const uint8_t* value);
void memory_set32(Memory* m, uint64_t offset, const uint8_t val32[32]);

// returns pointer into internal buffer (DO NOT free)
uint8_t* memory_getptr(Memory* m, uint64_t offset, uint64_t size);

// returns newly allocated copy (caller must free)
uint8_t* memory_getcopy(Memory* m, uint64_t offset, uint64_t size);

// len
uint64_t memory_len(Memory* m);

// copy (may overlap)
void memory_copy(Memory* m, uint64_t dst, uint64_t src, uint64_t len);

#ifdef __cplusplus
}
#endif

#endif // MEM_H
