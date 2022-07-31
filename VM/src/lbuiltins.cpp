// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lbuiltins.h"

#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "lgc.h"
#include "lnumutils.h"
#include "ldo.h"

#include <math.h>

#ifdef _MSC_VER
#include <intrin.h>
#endif

// lluzF functions implement FASTCALL instruction that performs a direct execution of some builtin functions from the VM
// The rule of thumb is that FASTCALL functions can not call user code, yield, fail, or reallocate stack.
// If types of the arguments mismatch, lluzF_* needs to return -1 and the execution will fall back to the usual call path
// If lluzF_* succeeds, it needs to return *all* requested arguments, filling results with nil as appropriate.
// On input, nparams refers to the actual number of arguments (0+), whereas nresults contains LUA_MULTRET for arbitrary returns or 0+ for a
// fixed-length return
// Because of this, and the fact that "extra" returned values will be ignored, implementations below typically check that nresults is <= expected
// number, which covers the LUA_MULTRET case.

static int lluzF_assert(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults == 0 && !l_isfalse(arg0))
    {
        return 0;
    }

    return -1;
}

static int lluzF_abs(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, fabs(a1));
        return 1;
    }

    return -1;
}

static int lluzF_acos(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, acos(a1));
        return 1;
    }

    return -1;
}

static int lluzF_asin(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, asin(a1));
        return 1;
    }

    return -1;
}

static int lluzF_atan2(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 2 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args))
    {
        double a1 = nvalue(arg0);
        double a2 = nvalue(args);
        setnvalue(res, atan2(a1, a2));
        return 1;
    }

    return -1;
}

static int lluzF_atan(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, atan(a1));
        return 1;
    }

    return -1;
}

lluz_FASTMATH_BEGIN
static int lluzF_ceil(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, ceil(a1));
        return 1;
    }

    return -1;
}
lluz_FASTMATH_END

static int lluzF_cosh(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, cosh(a1));
        return 1;
    }

    return -1;
}

static int lluzF_cos(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, cos(a1));
        return 1;
    }

    return -1;
}

static int lluzF_deg(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        const double rpd = (3.14159265358979323846 / 180.0);
        setnvalue(res, a1 / rpd);
        return 1;
    }

    return -1;
}

static int lluzF_exp(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, exp(a1));
        return 1;
    }

    return -1;
}

lluz_FASTMATH_BEGIN
static int lluzF_floor(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, floor(a1));
        return 1;
    }

    return -1;
}
lluz_FASTMATH_END

static int lluzF_fmod(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 2 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args))
    {
        double a1 = nvalue(arg0);
        double a2 = nvalue(args);
        setnvalue(res, fmod(a1, a2));
        return 1;
    }

    return -1;
}

static int lluzF_frexp(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 2 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        int e;
        double f = frexp(a1, &e);
        setnvalue(res, f);
        setnvalue(res + 1, double(e));
        return 2;
    }

    return -1;
}

static int lluzF_ldexp(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 2 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args))
    {
        double a1 = nvalue(arg0);
        double a2 = nvalue(args);
        setnvalue(res, ldexp(a1, int(a2)));
        return 1;
    }

    return -1;
}

static int lluzF_log10(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, log10(a1));
        return 1;
    }

    return -1;
}

static int lluzF_log(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);

        if (nparams == 1)
        {
            setnvalue(res, log(a1));
            return 1;
        }
        else if (ttisnumber(args))
        {
            double a2 = nvalue(args);

            if (a2 == 2.0)
            {
                setnvalue(res, log2(a1));
                return 1;
            }
            else if (a2 == 10.0)
            {
                setnvalue(res, log10(a1));
                return 1;
            }
            else
            {
                setnvalue(res, log(a1) / log(a2));
                return 1;
            }
        }
    }

    return -1;
}

static int lluzF_max(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double r = nvalue(arg0);

        for (int i = 2; i <= nparams; ++i)
        {
            if (!ttisnumber(args + (i - 2)))
                return -1;

            double a = nvalue(args + (i - 2));

            r = (a > r) ? a : r;
        }

        setnvalue(res, r);
        return 1;
    }

    return -1;
}

static int lluzF_min(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double r = nvalue(arg0);

        for (int i = 2; i <= nparams; ++i)
        {
            if (!ttisnumber(args + (i - 2)))
                return -1;

            double a = nvalue(args + (i - 2));

            r = (a < r) ? a : r;
        }

        setnvalue(res, r);
        return 1;
    }

    return -1;
}

static int lluzF_modf(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 2 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        double ip;
        double fp = modf(a1, &ip);
        setnvalue(res, ip);
        setnvalue(res + 1, fp);
        return 2;
    }

    return -1;
}

static int lluzF_pow(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 2 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args))
    {
        double a1 = nvalue(arg0);
        double a2 = nvalue(args);
        setnvalue(res, pow(a1, a2));
        return 1;
    }

    return -1;
}

static int lluzF_rad(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        const double rpd = (3.14159265358979323846 / 180.0);
        setnvalue(res, a1 * rpd);
        return 1;
    }

    return -1;
}

static int lluzF_sinh(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, sinh(a1));
        return 1;
    }

    return -1;
}

static int lluzF_sin(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, sin(a1));
        return 1;
    }

    return -1;
}

lluz_FASTMATH_BEGIN
static int lluzF_sqrt(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, sqrt(a1));
        return 1;
    }

    return -1;
}
lluz_FASTMATH_END

static int lluzF_tanh(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, tanh(a1));
        return 1;
    }

    return -1;
}

static int lluzF_tan(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        setnvalue(res, tan(a1));
        return 1;
    }

    return -1;
}

static int lluzF_arshift(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 2 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args))
    {
        double a1 = nvalue(arg0);
        double a2 = nvalue(args);

        unsigned u;
        luai_num2unsigned(u, a1);
        int s = int(a2);

        // note: we only specialize fast-path that doesn't require further conditionals (negative shifts and shifts greater or equal to bit width can
        // be handled generically)
        if (unsigned(s) < 32)
        {
            // note: technically right shift of negative values is UB, but this behavior is getting defined in C++20 and all compilers do the right
            // (shift) thing.
            uint32_t r = int32_t(u) >> s;

            setnvalue(res, double(r));
            return 1;
        }
    }

    return -1;
}

static int lluzF_band(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1)
    {
        uint32_t r = ~0u;

        if (!ttisnumber(arg0))
            return -1;

        {
            double a1 = nvalue(arg0);
            unsigned u;
            luai_num2unsigned(u, a1);

            r &= u;
        }

        for (int i = 2; i <= nparams; ++i)
        {
            if (!ttisnumber(args + (i - 2)))
                return -1;

            double a = nvalue(args + (i - 2));
            unsigned u;
            luai_num2unsigned(u, a);

            r &= u;
        }

        setnvalue(res, double(r));
        return 1;
    }

    return -1;
}

static int lluzF_bnot(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);
        unsigned u;
        luai_num2unsigned(u, a1);

        uint32_t r = ~u;

        setnvalue(res, double(r));
        return 1;
    }

    return -1;
}

static int lluzF_bor(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1)
    {
        uint32_t r = 0;

        if (!ttisnumber(arg0))
            return -1;

        {
            double a1 = nvalue(arg0);
            unsigned u;
            luai_num2unsigned(u, a1);

            r |= u;
        }

        for (int i = 2; i <= nparams; ++i)
        {
            if (!ttisnumber(args + (i - 2)))
                return -1;

            double a = nvalue(args + (i - 2));
            unsigned u;
            luai_num2unsigned(u, a);

            r |= u;
        }

        setnvalue(res, double(r));
        return 1;
    }

    return -1;
}

static int lluzF_bxor(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1)
    {
        uint32_t r = 0;

        if (!ttisnumber(arg0))
            return -1;

        {
            double a1 = nvalue(arg0);
            unsigned u;
            luai_num2unsigned(u, a1);

            r ^= u;
        }

        for (int i = 2; i <= nparams; ++i)
        {
            if (!ttisnumber(args + (i - 2)))
                return -1;

            double a = nvalue(args + (i - 2));
            unsigned u;
            luai_num2unsigned(u, a);

            r ^= u;
        }

        setnvalue(res, double(r));
        return 1;
    }

    return -1;
}

static int lluzF_btest(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1)
    {
        uint32_t r = ~0u;

        if (!ttisnumber(arg0))
            return -1;

        {
            double a1 = nvalue(arg0);
            unsigned u;
            luai_num2unsigned(u, a1);

            r &= u;
        }

        for (int i = 2; i <= nparams; ++i)
        {
            if (!ttisnumber(args + (i - 2)))
                return -1;

            double a = nvalue(args + (i - 2));
            unsigned u;
            luai_num2unsigned(u, a);

            r &= u;
        }

        setbvalue(res, r != 0);
        return 1;
    }

    return -1;
}

static int lluzF_extract(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 3 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args) && ttisnumber(args + 1))
    {
        double a1 = nvalue(arg0);
        double a2 = nvalue(args);
        double a3 = nvalue(args + 1);

        unsigned n;
        luai_num2unsigned(n, a1);
        int f = int(a2);
        int w = int(a3);

        if (f >= 0 && w > 0 && f + w <= 32)
        {
            uint32_t m = ~(0xfffffffeu << (w - 1));
            uint32_t r = (n >> f) & m;

            setnvalue(res, double(r));
            return 1;
        }
    }

    return -1;
}

static int lluzF_lrotate(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 2 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args))
    {
        double a1 = nvalue(arg0);
        double a2 = nvalue(args);

        unsigned u;
        luai_num2unsigned(u, a1);
        int s = int(a2);

        // MSVC doesn't recognize the rotate form that is UB-safe
#ifdef _MSC_VER
        uint32_t r = _rotl(u, s);
#else
        uint32_t r = (u << (s & 31)) | (u >> ((32 - s) & 31));
#endif

        setnvalue(res, double(r));
        return 1;
    }

    return -1;
}

static int lluzF_lshift(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 2 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args))
    {
        double a1 = nvalue(arg0);
        double a2 = nvalue(args);

        unsigned u;
        luai_num2unsigned(u, a1);
        int s = int(a2);

        // note: we only specialize fast-path that doesn't require further conditionals (negative shifts and shifts greater or equal to bit width can
        // be handled generically)
        if (unsigned(s) < 32)
        {
            uint32_t r = u << s;

            setnvalue(res, double(r));
            return 1;
        }
    }

    return -1;
}

static int lluzF_replace(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 4 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args) && ttisnumber(args + 1) && ttisnumber(args + 2))
    {
        double a1 = nvalue(arg0);
        double a2 = nvalue(args);
        double a3 = nvalue(args + 1);
        double a4 = nvalue(args + 2);

        unsigned n, v;
        luai_num2unsigned(n, a1);
        luai_num2unsigned(v, a2);
        int f = int(a3);
        int w = int(a4);

        if (f >= 0 && w > 0 && f + w <= 32)
        {
            uint32_t m = ~(0xfffffffeu << (w - 1));
            uint32_t r = (n & ~(m << f)) | ((v & m) << f);

            setnvalue(res, double(r));
            return 1;
        }
    }

    return -1;
}

static int lluzF_rrotate(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 2 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args))
    {
        double a1 = nvalue(arg0);
        double a2 = nvalue(args);

        unsigned u;
        luai_num2unsigned(u, a1);
        int s = int(a2);

        // MSVC doesn't recognize the rotate form that is UB-safe
#ifdef _MSC_VER
        uint32_t r = _rotr(u, s);
#else
        uint32_t r = (u >> (s & 31)) | (u << ((32 - s) & 31));
#endif

        setnvalue(res, double(r));
        return 1;
    }

    return -1;
}

static int lluzF_rshift(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 2 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args))
    {
        double a1 = nvalue(arg0);
        double a2 = nvalue(args);

        unsigned u;
        luai_num2unsigned(u, a1);
        int s = int(a2);

        // note: we only specialize fast-path that doesn't require further conditionals (negative shifts and shifts greater or equal to bit width can
        // be handled generically)
        if (unsigned(s) < 32)
        {
            uint32_t r = u >> s;

            setnvalue(res, double(r));
            return 1;
        }
    }

    return -1;
}

static int lluzF_type(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1)
    {
        int tt = ttype(arg0);
        TString* ttname = L->global->ttname[tt];

        setsvalue2s(L, res, ttname);
        return 1;
    }

    return -1;
}

static int lluzF_byte(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 2 && ttisstring(arg0) && ttisnumber(args))
    {
        TString* ts = tsvalue(arg0);
        int i = int(nvalue(args));
        int j = (nparams >= 3) ? (ttisnumber(args + 1) ? int(nvalue(args + 1)) : 0) : i;

        if (i >= 1 && j >= i && j <= int(ts->len))
        {
            int c = j - i + 1;
            const char* s = getstr(ts);

            // for vararg returns, we only support a single result
            // this is because this frees us from concerns about stack space
            if (c == (nresults < 0 ? 1 : nresults))
            {
                for (int k = 0; k < c; ++k)
                {
                    setnvalue(res + k, uint8_t(s[i + k - 1]));
                }

                return c;
            }
        }
    }

    return -1;
}

static int lluzF_char(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    char buffer[8];

    if (nparams < int(sizeof(buffer)) && nresults <= 1)
    {

        if (nparams >= 1)
        {
            if (!ttisnumber(arg0))
                return -1;

            int ch = int(nvalue(arg0));

            if ((unsigned char)(ch) != ch)
                return -1;

            buffer[0] = ch;
        }

        for (int i = 2; i <= nparams; ++i)
        {
            if (!ttisnumber(args + (i - 2)))
                return -1;

            int ch = int(nvalue(args + (i - 2)));

            if ((unsigned char)(ch) != ch)
                return -1;

            buffer[i - 1] = ch;
        }

        buffer[nparams] = 0;

        setsvalue2s(L, res, luaS_newlstr(L, buffer, nparams));
        return 1;
    }

    return -1;
}

static int lluzF_len(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisstring(arg0))
    {
        TString* ts = tsvalue(arg0);

        setnvalue(res, int(ts->len));
        return 1;
    }

    return -1;
}

static int lluzF_typeof(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1)
    {
        const TString* ttname = luaT_objtypenamestr(L, arg0);

        setsvalue2s(L, res, ttname);
        return 1;
    }

    return -1;
}

static int lluzF_sub(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 3 && nresults <= 1 && ttisstring(arg0) && ttisnumber(args) && ttisnumber(args + 1))
    {
        TString* ts = tsvalue(arg0);
        int i = int(nvalue(args));
        int j = int(nvalue(args + 1));

        if (i >= 1 && j >= i && unsigned(j - 1) < unsigned(ts->len))
        {
            setsvalue2s(L, res, luaS_newlstr(L, getstr(ts) + (i - 1), j - i + 1));
            return 1;
        }
    }

    return -1;
}

static int lluzF_clamp(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 3 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args) && ttisnumber(args + 1))
    {
        double v = nvalue(arg0);
        double min = nvalue(args);
        double max = nvalue(args + 1);

        if (min <= max)
        {
            double r = v < min ? min : v;
            r = r > max ? max : r;

            setnvalue(res, r);
            return 1;
        }
    }

    return -1;
}

static int lluzF_sign(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double v = nvalue(arg0);
        setnvalue(res, v > 0.0 ? 1.0 : v < 0.0 ? -1.0 : 0.0);
        return 1;
    }

    return -1;
}

lluz_FASTMATH_BEGIN
static int lluzF_round(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double v = nvalue(arg0);
        setnvalue(res, round(v));
        return 1;
    }

    return -1;
}
lluz_FASTMATH_END

static int lluzF_rawequal(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 2 && nresults <= 1)
    {
        setbvalue(res, luaO_rawequalObj(arg0, args));
        return 1;
    }

    return -1;
}

static int lluzF_rawget(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 2 && nresults <= 1 && ttistable(arg0))
    {
        setobj2s(L, res, luaH_get(hvalue(arg0), args));
        return 1;
    }

    return -1;
}

static int lluzF_rawset(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 3 && nresults <= 1 && ttistable(arg0))
    {
        const TValue* key = args;
        if (ttisnil(key))
            return -1;
        else if (ttisnumber(key) && luai_numisnan(nvalue(key)))
            return -1;
        else if (ttisvector(key) && luai_vecisnan(vvalue(key)))
            return -1;

        if (hvalue(arg0)->readonly)
            return -1;

        setobj2s(L, res, arg0);
        setobj2t(L, luaH_set(L, hvalue(arg0), args), args + 1);
        luaC_barriert(L, hvalue(arg0), args + 1);
        return 1;
    }

    return -1;
}

static int lluzF_tinsert(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams == 2 && nresults <= 0 && ttistable(arg0))
    {
        if (hvalue(arg0)->readonly)
            return -1;

        int pos = luaH_getn(hvalue(arg0)) + 1;
        setobj2t(L, luaH_setnum(L, hvalue(arg0), pos), args);
        luaC_barriert(L, hvalue(arg0), args);
        return 0;
    }

    return -1;
}

static int lluzF_tunpack(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults < 0 && ttistable(arg0))
    {
        Table* t = hvalue(arg0);
        int n = -1;

        if (nparams == 1)
            n = luaH_getn(t);
        else if (nparams == 3 && ttisnumber(args) && ttisnumber(args + 1) && nvalue(args) == 1.0)
            n = int(nvalue(args + 1));

        if (n >= 0 && n <= t->sizearray && cast_int(L->stack_last - res) >= n && n + nparams <= LUAI_MAXCSTACK)
        {
            TValue* array = t->array;
            for (int i = 0; i < n; ++i)
                setobj2s(L, res + i, array + i);
            expandstacklimit(L, res + n);
            return n;
        }
    }

    return -1;
}

static int lluzF_vector(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 3 && nresults <= 1 && ttisnumber(arg0) && ttisnumber(args) && ttisnumber(args + 1))
    {
        double x = nvalue(arg0);
        double y = nvalue(args);
        double z = nvalue(args + 1);

#if LUA_VECTOR_SIZE == 4
        double w = 0.0;
        if (nparams >= 4)
        {
            if (!ttisnumber(args + 2))
                return -1;
            w = nvalue(args + 2);
        }
        setvvalue(res, float(x), float(y), float(z), float(w));
#else
        setvvalue(res, float(x), float(y), float(z), 0.0f);
#endif

        return 1;
    }

    return -1;
}

static int lluzF_countlz(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);

        unsigned n;
        luai_num2unsigned(n, a1);

#ifdef _MSC_VER
        unsigned long rl;
        int r = _BitScanReverse(&rl, n) ? 31 - int(rl) : 32;
#else
        int r = n == 0 ? 32 : __builtin_clz(n);
#endif

        setnvalue(res, double(r));
        return 1;
    }

    return -1;
}

static int lluzF_countrz(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1 && ttisnumber(arg0))
    {
        double a1 = nvalue(arg0);

        unsigned n;
        luai_num2unsigned(n, a1);

#ifdef _MSC_VER
        unsigned long rl;
        int r = _BitScanForward(&rl, n) ? int(rl) : 32;
#else
        int r = n == 0 ? 32 : __builtin_ctz(n);
#endif

        setnvalue(res, double(r));
        return 1;
    }

    return -1;
}

static int lluzF_select(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams == 1 && nresults == 1)
    {
        int n = cast_int(L->base - L->ci->func) - clvalue(L->ci->func)->l.p->numparams - 1;

        if (ttisnumber(arg0))
        {
            int i = int(nvalue(arg0));

            // i >= 1 && i <= n
            if (unsigned(i - 1) < unsigned(n))
            {
                setobj2s(L, res, L->base - n + (i - 1));
                return 1;
            }
            // note: for now we don't handle negative case (wrap around) and defer to fallback
        }
        else if (ttisstring(arg0) && *svalue(arg0) == '#')
        {
            setnvalue(res, double(n));
            return 1;
        }
    }

    return -1;
}

static int lluzF_rawlen(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    if (nparams >= 1 && nresults <= 1)
    {
        if (ttistable(arg0))
        {
            Table* h = hvalue(arg0);
            setnvalue(res, double(luaH_getn(h)));
            return 1;
        }
        else if (ttisstring(arg0))
        {
            TString* ts = tsvalue(arg0);
            setnvalue(res, double(ts->len));
            return 1;
        }
    }

    return -1;
}

lluz_FastFunction lluzF_table[256] = {
    NULL,
    lluzF_assert,

    lluzF_abs,
    lluzF_acos,
    lluzF_asin,
    lluzF_atan2,
    lluzF_atan,
    lluzF_ceil,
    lluzF_cosh,
    lluzF_cos,
    lluzF_deg,
    lluzF_exp,
    lluzF_floor,
    lluzF_fmod,
    lluzF_frexp,
    lluzF_ldexp,
    lluzF_log10,
    lluzF_log,
    lluzF_max,
    lluzF_min,
    lluzF_modf,
    lluzF_pow,
    lluzF_rad,
    lluzF_sinh,
    lluzF_sin,
    lluzF_sqrt,
    lluzF_tanh,
    lluzF_tan,

    lluzF_arshift,
    lluzF_band,
    lluzF_bnot,
    lluzF_bor,
    lluzF_bxor,
    lluzF_btest,
    lluzF_extract,
    lluzF_lrotate,
    lluzF_lshift,
    lluzF_replace,
    lluzF_rrotate,
    lluzF_rshift,

    lluzF_type,

    lluzF_byte,
    lluzF_char,
    lluzF_len,

    lluzF_typeof,

    lluzF_sub,

    lluzF_clamp,
    lluzF_sign,
    lluzF_round,

    lluzF_rawset,
    lluzF_rawget,
    lluzF_rawequal,

    lluzF_tinsert,
    lluzF_tunpack,

    lluzF_vector,

    lluzF_countlz,
    lluzF_countrz,

    lluzF_select,

    lluzF_rawlen,
};
