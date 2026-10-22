// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#pragma once

#include <math.h>
#include <cstdint>

#define luai_numadd(a, b) ((a) + (b))
#define luai_numsub(a, b) ((a) - (b))
#define luai_nummul(a, b) ((a) * (b))
#define luai_numdiv(a, b) ((a) / (b))
#define luai_numpow(a, b) (pow(a, b))
#define luai_numunm(a) (-(a))
#define luai_numisnan(a) ((a) != (a))
#define luai_numeq(a, b) ((a) == (b))
#define luai_numlt(a, b) ((a) < (b))
#define luai_numle(a, b) ((a) <= (b))
#define luai_inteq(a, b) ((a) == (b))

inline bool luai_veceq(const float* a, const float* b)
{
#if LUA_VECTOR_SIZE == 4
    return a[0] == b[0] && a[1] == b[1] && a[2] == b[2] && a[3] == b[3];
#else
    return a[0] == b[0] && a[1] == b[1] && a[2] == b[2];
#endif
}

inline bool luai_veceq(const double* a, const double* b)
{
#if LUA_VECTOR_SIZE == 4
    return a[0] == b[0] && a[1] == b[1] && a[2] == b[2] && a[3] == b[3];
#else
    return a[0] == b[0] && a[1] == b[1] && a[2] == b[2];
#endif
}

inline bool luai_vecisnan(const float* a)
{
#if LUA_VECTOR_SIZE == 4
    return a[0] != a[0] || a[1] != a[1] || a[2] != a[2] || a[3] != a[3];
#else
    return a[0] != a[0] || a[1] != a[1] || a[2] != a[2];
#endif
}

inline bool luai_vecisnan(const double* a)
{
#if LUA_VECTOR_SIZE == 4
    return a[0] != a[0] || a[1] != a[1] || a[2] != a[2] || a[3] != a[3];
#else
    return a[0] != a[0] || a[1] != a[1] || a[2] != a[2];
#endif
}

inline float luai_signf(float v)
{
    return v > 0.0f ? 1.0f : v < 0.0f ? -1.0f : 0.0f;
}

inline double luai_signd(double v)
{
    return v > 0.0 ? 1.0 : v < 0.0 ? -1.0 : 0.0;
}

inline float luai_clampf(float v, float min, float max)
{
    float r = v < min ? min : v;
    return r > max ? max : r;
}

inline double luai_clampd(double v, double min, double max)
{
    double r = v < min ? min : v;
    return r > max ? max : r;
}

LUAU_FASTMATH_BEGIN
inline double luai_nummod(double a, double b)
{
    return a - floor(a / b) * b;
}
LUAU_FASTMATH_END

LUAU_FASTMATH_BEGIN
inline double luai_numidiv(double a, double b)
{
    return floor(a / b);
}
LUAU_FASTMATH_END

inline float luai_lerpf(float a, float b, float t)
{
    return (t == 1.0f) ? b : a + (b - a) * t;
}

inline double luai_lerpd(double a, double b, double t)
{
    return (t == 1.0) ? b : a + (b - a) * t;
}

inline bool luai_numfitsint(double d)
{
    return d >= -2147483648.0 && d < 2147483648.0;
}

inline bool luai_numfitsint64(double d)
{
    return d >= -9223372036854775808.0 && d < 9223372036854775808.0;
}

#define luai_num2int(i, d) ((i) = (int)(d))

#define luai_num2long(i, d) ((i) = (int64_t)(d))

#define luai_fabs condvectordouble(fabs, fabsf)
#define luai_sqrt condvectordouble(sqrt, sqrtf)
#define luai_floor condvectordouble(floor, floorf)
#define luai_ceil condvectordouble(ceil, ceilf)
#define luai_sign condvectordouble(luai_signd, luai_signf)
#define luai_clamp condvectordouble(luai_clampd, luai_clampf)
#define luai_lerp condvectordouble(luai_lerpd, luai_lerpf)

// Converts a double to unsigned by truncating it to a 64-bit integer and keeping the low 32 bits, so in-range values wrap modulo 2^32.
// Values outside of (-2^63, 2^63), including NaN and infinities, convert to 0.
#if defined(_M_IX86) || defined(__i386__)
// On 32-bit x86, converting to a 64-bit integer is a slow helper call, so values that fit into int use the 32-bit conversion
inline unsigned luai_num2unsignedx86(double n)
{
    if (luai_numfitsint(n))
        return unsigned(int(n));

    return fabs(n) < 9223372036854775808.0 ? unsigned((long long)n) : 0u;
}

#define luai_num2unsigned(i, n) ((i) = luai_num2unsignedx86(n))
#elif (defined(__x86_64__) || defined(_M_X64)) && !defined(_M_ARM64EC)
#include <emmintrin.h>

// cvttsd2si returns 0x8000000000000000 for values that don't fit, which truncates to 0
#define luai_num2unsigned(i, n) ((i) = (unsigned)_mm_cvttsd_si64(_mm_set_sd(n)))
#else
inline unsigned luai_num2unsignedsafe(double n)
{
    return fabs(n) < 9223372036854775808.0 ? (unsigned)(long long)n : 0u;
}

#define luai_num2unsigned(i, n) ((i) = luai_num2unsignedsafe(n))
#endif

#define LUAI_MAXNUM2STR 48
#define LUAI_MAXINT2STR 30

LUAI_FUNC char* luai_num2str(char* buf, double n);
LUAI_FUNC char* luai_int2str(char* buf, int64_t n);

#define luai_str2num(s, p) strtod((s), (p))
#define luai_str2long(s, p, base) strtoll((s), (p), base)
