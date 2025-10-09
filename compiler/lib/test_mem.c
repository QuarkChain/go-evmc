#include "mem.h"
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define CHECK(cond, msg) do { \
    if (!(cond)) { \
        fprintf(stderr, "FAIL: %s\n", msg); \
        return 1; \
    } \
} while(0)

int test_new_free() {
    Memory* m = memory_new();
    CHECK(m != NULL, "memory_new returned NULL");
    CHECK(memory_len(m) == 0, "new memory len should be 0");
    memory_free(m);
    return 0;
}

int test_resize() {
    Memory* m = memory_new();
    memory_resize(m, 10);
    CHECK(memory_len(m) == 10, "memory_len after resize 10");
    memory_resize(m, 5);
    CHECK(memory_len(m) == 5, "memory_len after shrink 5");
    memory_free(m);
    return 0;
}

int test_set_getptr_getcopy() {
    Memory* m = memory_new();
    memory_resize(m, 8);

    uint8_t data[8] = {1,2,3,4,5,6,7,8};
    memory_set(m, 0, 8, data);

    uint8_t* ptr = memory_getptr(m, 0, 8);
    CHECK(ptr != NULL, "memory_getptr returned NULL");
    CHECK(memcmp(ptr, data, 8) == 0, "memory_getptr data mismatch");

    uint8_t* copy = memory_getcopy(m, 0, 8);
    CHECK(copy != NULL, "memory_getcopy returned NULL");
    CHECK(memcmp(copy, data, 8) == 0, "memory_getcopy data mismatch");
    free(copy);

    memory_free(m);
    return 0;
}

int test_set32() {
    Memory* m = memory_new();
    memory_resize(m, 32);

    uint8_t val32[32];
    for (int i = 0; i < 32; i++) val32[i] = i;
    memory_set32(m, 0, val32);

    uint8_t* ptr = memory_getptr(m, 0, 32);
    CHECK(ptr != NULL, "memory_getptr for set32 returned NULL");
    CHECK(memcmp(ptr, val32, 32) == 0, "memory_set32 data mismatch");

    memory_free(m);
    return 0;
}

int test_copy() {
    Memory* m = memory_new();
    memory_resize(m, 16);

    uint8_t data[16];
    for (int i = 0; i < 16; i++) data[i] = i;
    memory_set(m, 0, 16, data);

    // Copy 0..8 -> 8..16
    memory_copy(m, 8, 0, 8);

    uint8_t expected[16];
    for (int i = 0; i < 8; i++) expected[i] = i;
    for (int i = 8; i < 16; i++) expected[i] = i - 8;
    uint8_t* ptr = memory_getptr(m, 0, 16);
    CHECK(memcmp(ptr, expected, 16) == 0, "memory_copy overlap mismatch");

    memory_free(m);
    return 0;
}

int main() {
    int failures = 0;

    if (test_new_free()) failures++;
    if (test_resize()) failures++;
    if (test_set_getptr_getcopy()) failures++;
    if (test_set32()) failures++;
    if (test_copy()) failures++;

    if (failures == 0) {
        printf("All tests passed ✅\n");
        return 0;
    } else {
        printf("%d tests failed ❌\n", failures);
        return 1;
    }
}
