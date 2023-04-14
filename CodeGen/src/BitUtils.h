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

} // namespace CodeGen
} // namespace Luau
