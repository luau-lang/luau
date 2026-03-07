// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <stdint.h>

#ifdef _MSC_VER
#include <intrin.h>
#endif

namespace Luau
{
namespace CodeGen
{

inline int countlz(uint32_t n)
{
#ifdef _MSC_VER
    unsigned long rl;
    return _BitScanReverse(&rl, n) ? 31 - int(rl) : 32;
#else
    return n == 0 ? 32 : __builtin_clz(n);
#endif
}

inline int countrz(uint32_t n)
{
#ifdef _MSC_VER
    unsigned long rl;
    return _BitScanForward(&rl, n) ? int(rl) : 32;
#else
    return n == 0 ? 32 : __builtin_ctz(n);
#endif
}

inline int countrz(uint64_t n)
{
#ifdef _MSC_VER

#ifdef _WIN64
    unsigned long rl;
    return _BitScanForward64(&rl, n) ? int(rl) : 64;
#else
    unsigned long rl;
    if (_BitScanForward(&rl, uint32_t(n)))
        return rl;
    return _BitScanForward(&rl, uint32_t(n >> 32)) ? int(rl) + 32 : 64;
#endif

#else
    return n == 0 ? 64 : __builtin_ctzll(n);
#endif
}

inline int lrotate(uint32_t u, int s)
{
    // MSVC doesn't recognize the rotate form that is UB-safe
#ifdef _MSC_VER
    return _rotl(u, s);
#else
    return (u << (s & 31)) | (u >> ((32 - s) & 31));
#endif
}

inline int rrotate(uint32_t u, int s)
{
    // MSVC doesn't recognize the rotate form that is UB-safe
#ifdef _MSC_VER
    return _rotr(u, s);
#else
    return (u >> (s & 31)) | (u << ((32 - s) & 31));
#endif
}

} // namespace CodeGen
} // namespace Luau
