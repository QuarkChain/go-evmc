#include "mem.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

Memory *memory_new(void)
{
    Memory *m = (Memory *)malloc(sizeof(Memory));
    if (!m)
        return NULL;
    m->store = NULL;
    m->len = 0;
    m->cap = 0;
    m->lastGasCost = 0;
    return m;
}

void memory_free(Memory *m)
{
    if (!m)
        return;
    if (m->store)
        free(m->store);
    free(m);
}

static int ensure_capacity(Memory *m, size_t newlen)
{
    if (newlen <= m->cap)
    {
        if (newlen > m->len)
            m->len = newlen;
        return 1;
    }
    size_t newcap = m->cap ? m->cap : 1;
    while (newcap < newlen)
        newcap <<= 1;
    uint8_t *newbuf = (uint8_t *)realloc(m->store, newcap);
    if (!newbuf)
        return 0;

    // zero the newly allocated region for safety
    if (newcap > m->len)
    {
        memset(newbuf + m->len, 0, newcap - m->len);
    }

    // here we left padding zeros to memory due to memory.store is littleEndian
    // uint64_t offset = newlen - m->len;
    // memcpy(newbuf + offset, newbuf, m->len);
    // if (newcap > m->len)
    // {
    //     memset(newbuf, 0, offset);
    // }
    m->store = newbuf;
    m->cap = newcap;
    m->len = newlen;
    return 1;
}

void memory_resize(Memory *m, uint64_t size)
{
    if (!m)
        return;
    if ((size_t)m->len < (size_t)size)
    {
        if (!ensure_capacity(m, (size_t)size))
        {
            fprintf(stderr, "memory_resize: allocation failed\n");
            abort();
        }
    }
}

// memory and value is big-endian, offset is also for big-endian
void memory_set(Memory *m, uint64_t offset, uint64_t size, const uint8_t *value)
{
    if (!m)
        return;
    if (size == 0)
        return;
    size_t end = (size_t)offset + (size_t)size;
    if ((size_t)m->len < end)
    {
        fprintf(stderr, "memory_set: out of bounds (did you Resize first?)\n");
        abort();
    }
    memcpy(m->store + offset, value, (size_t)size);
}

void memory_set32(Memory *m, uint64_t offset, const uint8_t val32[32])
{
    if (!m)
        return;
    size_t end = (size_t)offset + 32;
    if ((size_t)m->len < end)
    {
        fprintf(stderr, "memory_set32: out of bounds\n");
        abort();
    }
    memcpy(m->store + offset, val32, 32);
}

uint8_t *memory_getptr(Memory *m, uint64_t offset, uint64_t size)
{
    if (!m)
        return NULL;
    if (size == 0)
        return NULL;
    size_t end = (size_t)offset + (size_t)size;
    if ((size_t)m->len < end)
    {
        fprintf(stderr, "memory_getptr: out of bounds\n");
        abort();
    }
    return m->store + offset;
}

uint8_t *memory_getcopy(Memory *m, uint64_t offset, uint64_t size)
{
    if (!m)
        return NULL;
    if (size == 0)
        return NULL;
    size_t end = (size_t)offset + (size_t)size;
    if ((size_t)m->len < end)
    {
        fprintf(stderr, "memory_getcopy: out of bounds\n");
        abort();
    }
    uint8_t *out = (uint8_t *)malloc((size_t)size);
    if (!out)
    {
        fprintf(stderr, "memory_getcopy: malloc failed\n");
        abort();
    }
    memcpy(out, m->store + offset, (size_t)size);
    return out;
}

uint64_t memory_len(Memory *m)
{
    if (!m)
        return 0;
    return (uint64_t)m->len;
}

void memory_copy(Memory *m, uint64_t dst, uint64_t src, uint64_t len)
{
    if (!m)
        return;
    if (len == 0)
        return;
    size_t end1 = (size_t)dst + (size_t)len;
    size_t end2 = (size_t)src + (size_t)len;
    if ((size_t)m->len < end1 || (size_t)m->len < end2)
    {
        fprintf(stderr, "memory_copy: out of bounds\n");
        abort();
    }
    memmove(m->store + dst, m->store + src, (size_t)len);
}
