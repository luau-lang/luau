// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lvm.h"

#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "lgc.h"
#include "ldo.h"
#include "lnumutils.h"

#include <string.h>

/* limit for table tag-method chains (to avoid loops) */
#define MAXTAGLOOP 100

const TValue* luaV_tonumber(const TValue* obj, TValue* n)
{
    double num;
    if (ttisnumber(obj))
        return obj;
    if (ttisstring(obj) && luaO_str2d(svalue(obj), &num))
    {
        setnvalue(n, num);
        return n;
    }
    else
        return NULL;
}

int luaV_tostring(lua_State* L, StkId obj)
{
    if (!ttisnumber(obj))
        return 0;
    else
    {
        char s[LUAI_MAXNUMBER2STR];
        double n = nvalue(obj);
        luai_num2str(s, n);
        setsvalue2s(L, obj, luaS_new(L, s));
        return 1;
    }
}

const float* luaV_tovector(const TValue* obj)
{
    if (ttisvector(obj))
        return obj->value.v;

    return nullptr;
}

static void callTMres(lua_State* L, StkId res, const TValue* f, const TValue* p1, const TValue* p2)
{
    ptrdiff_t result = savestack(L, res);
    // RBOLOX: using stack room beyond top is technically safe here, but for very complicated reasons:
    // * The stack guarantees 1 + EXTRA_STACK room beyond stack_last (see luaD_reallocstack) will be allocated
    // * we cannot move luaD_checkstack above because the arguments are *sometimes* pointers to the lua
    // stack and checkstack may invalidate those pointers
    // * we cannot use savestack/restorestack because the arguments are sometimes on the C++ stack
    // * during stack reallocation all of the allocated stack is copied (even beyond stack_last) so these
    // values will be preserved even if they go past stack_last
    LUAU_ASSERT((L->top + 3) < (L->stack + L->stacksize));
    setobj2s(L, L->top, f);      /* push function */
    setobj2s(L, L->top + 1, p1); /* 1st argument */
    setobj2s(L, L->top + 2, p2); /* 2nd argument */
    luaD_checkstack(L, 3);
    L->top += 3;
    luaD_call(L, L->top - 3, 1);
    res = restorestack(L, result);
    L->top--;
    setobjs2s(L, res, L->top);
}

static void callTM(lua_State* L, const TValue* f, const TValue* p1, const TValue* p2, const TValue* p3)
{
    // RBOLOX: using stack room beyond top is technically safe here, but for very complicated reasons:
    // * The stack guarantees 1 + EXTRA_STACK room beyond stack_last (see luaD_reallocstack) will be allocated
    // * we cannot move luaD_checkstack above because the arguments are *sometimes* pointers to the lua
    // stack and checkstack may invalidate those pointers
    // * we cannot use savestack/restorestack because the arguments are sometimes on the C++ stack
    // * during stack reallocation all of the allocated stack is copied (even beyond stack_last) so these
    // values will be preserved even if they go past stack_last
    LUAU_ASSERT((L->top + 4) < (L->stack + L->stacksize));
    setobj2s(L, L->top, f);      /* push function */
    setobj2s(L, L->top + 1, p1); /* 1st argument */
    setobj2s(L, L->top + 2, p2); /* 2nd argument */
    setobj2s(L, L->top + 3, p3); /* 3th argument */
    luaD_checkstack(L, 4);
    L->top += 4;
    luaD_call(L, L->top - 4, 0);
}

void luaV_gettable(lua_State* L, const TValue* t, TValue* key, StkId val)
{
    int loop;
    for (loop = 0; loop < MAXTAGLOOP; loop++)
    {
        const TValue* tm;
        if (ttistable(t))
        { /* `t' is a table? */
            Table* h = hvalue(t);

            const TValue* res = luaH_get(h, key); /* do a primitive get */

            if (res != luaO_nilobject)
                L->cachedslot = gval2slot(h, res); /* remember slot to accelerate future lookups */

            if (!ttisnil(res) /* result is no nil? */
                || (tm = fasttm(L, h->metatable, TM_INDEX)) == NULL)
            { /* or no TM? */
                setobj2s(L, val, res);
                return;
            }
            /* t isn't a table, so see if it has an INDEX meta-method to look up the key with */
        }
        else if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_INDEX)))
            luaG_indexerror(L, t, key);
        if (ttisfunction(tm))
        {
            callTMres(L, val, tm, t, key);
            return;
        }
        t = tm; /* else repeat with `tm' */
    }
    luaG_runerror(L, "loop in gettable");
}

void luaV_settable(lua_State* L, const TValue* t, TValue* key, StkId val)
{
    int loop;
    TValue temp;
    for (loop = 0; loop < MAXTAGLOOP; loop++)
    {
        const TValue* tm;
        if (ttistable(t))
        { /* `t' is a table? */
            Table* h = hvalue(t);

            if (h->readonly)
                luaG_runerror(L, "Attempt to modify a readonly table");

            TValue* oldval = luaH_set(L, h, key); /* do a primitive set */

            L->cachedslot = gval2slot(h, oldval); /* remember slot to accelerate future lookups */

            if (!ttisnil(oldval) || /* result is no nil? */
                (tm = fasttm(L, h->metatable, TM_NEWINDEX)) == NULL)
            { /* or no TM? */
                setobj2t(L, oldval, val);
                luaC_barriert(L, h, val);
                return;
            }
            /* else will try the tag method */
        }
        else if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_NEWINDEX)))
            luaG_indexerror(L, t, key);
        if (ttisfunction(tm))
        {
            callTM(L, tm, t, key, val);
            return;
        }
        /* else repeat with `tm' */
        setobj(L, &temp, tm); /* avoid pointing inside table (may rehash) */
        t = &temp;
    }
    luaG_runerror(L, "loop in settable");
}

static int call_binTM(lua_State* L, const TValue* p1, const TValue* p2, StkId res, TMS event)
{
    const TValue* tm = luaT_gettmbyobj(L, p1, event); /* try first operand */
    if (ttisnil(tm))
        tm = luaT_gettmbyobj(L, p2, event); /* try second operand */
    if (ttisnil(tm))
        return 0;
    callTMres(L, res, tm, p1, p2);
    return 1;
}

static const TValue* get_compTM(lua_State* L, Table* mt1, Table* mt2, TMS event)
{
    const TValue* tm1 = fasttm(L, mt1, event);
    const TValue* tm2;
    if (tm1 == NULL)
        return NULL; /* no metamethod */
    if (mt1 == mt2)
        return tm1; /* same metatables => same metamethods */
    tm2 = fasttm(L, mt2, event);
    if (tm2 == NULL)
        return NULL;                /* no metamethod */
    if (luaO_rawequalObj(tm1, tm2)) /* same metamethods? */
        return tm1;
    return NULL;
}

static int call_orderTM(lua_State* L, const TValue* p1, const TValue* p2, TMS event)
{
    const TValue* tm1 = luaT_gettmbyobj(L, p1, event);
    const TValue* tm2;
    if (ttisnil(tm1))
        return -1; /* no metamethod? */
    tm2 = luaT_gettmbyobj(L, p2, event);
    if (!luaO_rawequalObj(tm1, tm2)) /* different metamethods? */
        return -1;
    callTMres(L, L->top, tm1, p1, p2);
    return !l_isfalse(L->top);
}

int luaV_strcmp(const TString* ls, const TString* rs)
{
    if (ls == rs)
        return 0;

    const char* l = getstr(ls);
    size_t ll = ls->len;
    const char* r = getstr(rs);
    size_t lr = rs->len;
    size_t lmin = ll < lr ? ll : lr;

    int res = memcmp(l, r, lmin);
    if (res != 0)
        return res;

    return ll == lr ? 0 : ll < lr ? -1 : 1;
}

int luaV_lessthan(lua_State* L, const TValue* l, const TValue* r)
{
    int res;
    if (ttype(l) != ttype(r))
        luaG_ordererror(L, l, r, TM_LT);
    else if (ttisnumber(l))
        return luai_numlt(nvalue(l), nvalue(r));
    else if (ttisstring(l))
        return luaV_strcmp(tsvalue(l), tsvalue(r)) < 0;
    else if ((res = call_orderTM(L, l, r, TM_LT)) == -1)
        luaG_ordererror(L, l, r, TM_LT);
    return res;
}

int luaV_lessequal(lua_State* L, const TValue* l, const TValue* r)
{
    int res;
    if (ttype(l) != ttype(r))
        luaG_ordererror(L, l, r, TM_LE);
    else if (ttisnumber(l))
        return luai_numle(nvalue(l), nvalue(r));
    else if (ttisstring(l))
        return luaV_strcmp(tsvalue(l), tsvalue(r)) <= 0;
    else if ((res = call_orderTM(L, l, r, TM_LE)) != -1) /* first try `le' */
        return res;
    else if ((res = call_orderTM(L, r, l, TM_LT)) == -1) /* error if not `lt' */
        luaG_ordererror(L, l, r, TM_LE);
    return !res;
}

int luaV_equalval(lua_State* L, const TValue* t1, const TValue* t2)
{
    const TValue* tm;
    LUAU_ASSERT(ttype(t1) == ttype(t2));
    switch (ttype(t1))
    {
    case LUA_TNIL:
        return 1;
    case LUA_TNUMBER:
        return luai_numeq(nvalue(t1), nvalue(t2));
    case LUA_TVECTOR:
        return luai_veceq(vvalue(t1), vvalue(t2));
    case LUA_TBOOLEAN:
        return bvalue(t1) == bvalue(t2); /* true must be 1 !! */
    case LUA_TLIGHTUSERDATA:
        return pvalue(t1) == pvalue(t2);
    case LUA_TUSERDATA:
    {
        tm = get_compTM(L, uvalue(t1)->metatable, uvalue(t2)->metatable, TM_EQ);
        if (!tm)
            return uvalue(t1) == uvalue(t2);
        break; /* will try TM */
    }
    case LUA_TTABLE:
    {
        tm = get_compTM(L, hvalue(t1)->metatable, hvalue(t2)->metatable, TM_EQ);
        if (!tm)
            return hvalue(t1) == hvalue(t2);
        break; /* will try TM */
    }
    default:
        return gcvalue(t1) == gcvalue(t2);
    }
    callTMres(L, L->top, tm, t1, t2); /* call TM */
    return !l_isfalse(L->top);
}

void luaV_concat(lua_State* L, int total, int last)
{
    do
    {
        StkId top = L->base + last + 1;
        int n = 2; /* number of elements handled in this pass (at least 2) */
        if (!(ttisstring(top - 2) || ttisnumber(top - 2)) || !tostring(L, top - 1))
        {
            if (!call_binTM(L, top - 2, top - 1, top - 2, TM_CONCAT))
                luaG_concaterror(L, top - 2, top - 1);
        }
        else if (tsvalue(top - 1)->len == 0) /* second op is empty? */
            (void)tostring(L, top - 2);      /* result is first op (as string) */
        else
        {
            /* at least two string values; get as many as possible */
            size_t tl = tsvalue(top - 1)->len;
            char* buffer;
            int i;
            /* collect total length */
            for (n = 1; n < total && tostring(L, top - n - 1); n++)
            {
                size_t l = tsvalue(top - n - 1)->len;
                if (l > MAXSSIZE - tl)
                    luaG_runerror(L, "string length overflow");
                tl += l;
            }

            char buf[LUA_BUFFERSIZE];
            TString* ts = nullptr;

            if (tl < LUA_BUFFERSIZE)
            {
                buffer = buf;
            }
            else
            {
                ts = luaS_bufstart(L, tl);
                buffer = ts->data;
            }

            tl = 0;
            for (i = n; i > 0; i--)
            { /* concat all strings */
                size_t l = tsvalue(top - i)->len;
                memcpy(buffer + tl, svalue(top - i), l);
                tl += l;
            }

            if (tl < LUA_BUFFERSIZE)
            {
                setsvalue2s(L, top - n, luaS_newlstr(L, buffer, tl));
            }
            else
            {
                setsvalue2s(L, top - n, luaS_buffinish(L, ts));
            }
        }
        total -= n - 1; /* got `n' strings to create 1 new */
        last -= n - 1;
    } while (total > 1); /* repeat until only 1 result left */
}

void luaV_doarith(lua_State* L, StkId ra, const TValue* rb, const TValue* rc, TMS op)
{
    TValue tempb, tempc;
    const TValue *b, *c;
    if ((b = luaV_tonumber(rb, &tempb)) != NULL && (c = luaV_tonumber(rc, &tempc)) != NULL)
    {
        double nb = nvalue(b), nc = nvalue(c);
        switch (op)
        {
        case TM_ADD:
            setnvalue(ra, luai_numadd(nb, nc));
            break;
        case TM_SUB:
            setnvalue(ra, luai_numsub(nb, nc));
            break;
        case TM_MUL:
            setnvalue(ra, luai_nummul(nb, nc));
            break;
        case TM_DIV:
            setnvalue(ra, luai_numdiv(nb, nc));
            break;
        case TM_MOD:
            setnvalue(ra, luai_nummod(nb, nc));
            break;
        case TM_POW:
            setnvalue(ra, luai_numpow(nb, nc));
            break;
        case TM_UNM:
            setnvalue(ra, luai_numunm(nb));
            break;
        default:
            LUAU_ASSERT(0);
            break;
        }
    }
    else
    {
        // vector operations that we support: v + v, v - v, v * v, s * v, v * s, v / v, s / v, v / s, -v
        const float* vb = luaV_tovector(rb);
        const float* vc = luaV_tovector(rc);

        if (vb && vc)
        {
            switch (op)
            {
            case TM_ADD:
                setvvalue(ra, vb[0] + vc[0], vb[1] + vc[1], vb[2] + vc[2], vb[3] + vc[3]);
                return;
            case TM_SUB:
                setvvalue(ra, vb[0] - vc[0], vb[1] - vc[1], vb[2] - vc[2], vb[3] - vc[3]);
                return;
            case TM_MUL:
                setvvalue(ra, vb[0] * vc[0], vb[1] * vc[1], vb[2] * vc[2], vb[3] * vc[3]);
                return;
            case TM_DIV:
                setvvalue(ra, vb[0] / vc[0], vb[1] / vc[1], vb[2] / vc[2], vb[3] / vc[3]);
                return;
            case TM_UNM:
                setvvalue(ra, -vb[0], -vb[1], -vb[2], -vb[3]);
                return;
            default:
                break;
            }
        }
        else if (vb)
        {
            c = luaV_tonumber(rc, &tempc);

            if (c)
            {
                float nc = cast_to(float, nvalue(c));

                switch (op)
                {
                case TM_MUL:
                    setvvalue(ra, vb[0] * nc, vb[1] * nc, vb[2] * nc, vb[3] * nc);
                    return;
                case TM_DIV:
                    setvvalue(ra, vb[0] / nc, vb[1] / nc, vb[2] / nc, vb[3] / nc);
                    return;
                default:
                    break;
                }
            }
        }
        else if (vc)
        {
            b = luaV_tonumber(rb, &tempb);

            if (b)
            {
                float nb = cast_to(float, nvalue(b));

                switch (op)
                {
                case TM_MUL:
                    setvvalue(ra, nb * vc[0], nb * vc[1], nb * vc[2], nb * vc[3]);
                    return;
                case TM_DIV:
                    setvvalue(ra, nb / vc[0], nb / vc[1], nb / vc[2], nb / vc[3]);
                    return;
                default:
                    break;
                }
            }
        }

        if (!call_binTM(L, rb, rc, ra, op))
        {
            luaG_aritherror(L, rb, rc, op);
        }
    }
}

void luaV_dolen(lua_State* L, StkId ra, const TValue* rb)
{
    switch (ttype(rb))
    {
    case LUA_TTABLE:
    {
        setnvalue(ra, cast_num(luaH_getn(hvalue(rb))));
        break;
    }
    case LUA_TSTRING:
    {
        setnvalue(ra, cast_num(tsvalue(rb)->len));
        break;
    }
    default:
    { /* try metamethod */
        if (!call_binTM(L, rb, luaO_nilobject, ra, TM_LEN))
            luaG_typeerror(L, rb, "get length of");
    }
    }
}
