// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details

/*
** Implementation of tables (aka arrays, objects, or hash tables).
** Tables keep its elements in two parts: an array part and a hash part.
** Non-negative integer keys are all candidates to be kept in the array
** part. The actual size of the array is the largest `n' such that at
** least half the slots between 0 and n are in use.
** Hash uses a mix of chained scatter table with Brent's variation.
** A main invariant of these tables is that, if an element is not
** in its main position (i.e. the `original' position that its hash gives
** to it), then the colliding element is in its own main position.
** Hence even when the load factor reaches 100%, performance remains good.
*/

#include "ltable.h"

#include "lstate.h"
#include "ldebug.h"
#include "lgc.h"
#include "lmem.h"
#include "lnumutils.h"

#include <string.h>

LUAU_FASTFLAGVARIABLE(LuauArrayBoundary, false)

// max size of both array and hash part is 2^MAXBITS
#define MAXBITS 26
#define MAXSIZE (1 << MAXBITS)

static_assert(offsetof(LuaNode, val) == 0, "Unexpected Node memory layout, pointer cast in gval2slot is incorrect");
// TKey is bitpacked for memory efficiency so we need to validate bit counts for worst case
static_assert(TKey{{NULL}, 0, LUA_TDEADKEY, 0}.tt == LUA_TDEADKEY, "not enough bits for tt");
static_assert(TKey{{NULL}, 0, LUA_TNIL, MAXSIZE - 1}.next == MAXSIZE - 1, "not enough bits for next");
static_assert(TKey{{NULL}, 0, LUA_TNIL, -(MAXSIZE - 1)}.next == -(MAXSIZE - 1), "not enough bits for next");

// reset cache of absent metamethods, cache is updated in luaT_gettm
#define invalidateTMcache(t) t->flags = 0

// empty hash data points to dummynode so that we can always dereference it
const LuaNode luaH_dummynode = {
    {{NULL}, 0, LUA_TNIL},   /* value */
    {{NULL}, 0, LUA_TNIL, 0} /* key */
};

#define dummynode (&luaH_dummynode)

// hash is always reduced mod 2^k
#define hashpow2(t, n) (gnode(t, lmod((n), sizenode(t))))

#define hashstr(t, str) hashpow2(t, (str)->hash)
#define hashboolean(t, p) hashpow2(t, p)

static LuaNode* hashpointer(const Table* t, const void* p)
{
    // we discard the high 32-bit portion of the pointer on 64-bit platforms as it doesn't carry much entropy anyway
    unsigned int h = unsigned(uintptr_t(p));

    // MurmurHash3 32-bit finalizer
    h ^= h >> 16;
    h *= 0x85ebca6bu;
    h ^= h >> 13;
    h *= 0xc2b2ae35u;
    h ^= h >> 16;

    return hashpow2(t, h);
}

static LuaNode* hashnum(const Table* t, double n)
{
    static_assert(sizeof(double) == sizeof(unsigned int) * 2, "expected a 8-byte double");
    unsigned int i[2];
    memcpy(i, &n, sizeof(i));

    // mask out sign bit to make sure -0 and 0 hash to the same value
    uint32_t h1 = i[0];
    uint32_t h2 = i[1] & 0x7fffffff;

    // finalizer from MurmurHash64B
    const uint32_t m = 0x5bd1e995;

    h1 ^= h2 >> 18;
    h1 *= m;
    h2 ^= h1 >> 22;
    h2 *= m;
    h1 ^= h2 >> 17;
    h1 *= m;
    h2 ^= h1 >> 19;
    h2 *= m;

    // ... truncated to 32-bit output (normally hash is equal to (uint64_t(h1) << 32) | h2, but we only really need the lower 32-bit half)
    return hashpow2(t, h2);
}

static LuaNode* hashvec(const Table* t, const float* v)
{
    unsigned int i[3];
    memcpy(i, v, sizeof(i));

    // convert -0 to 0 to make sure they hash to the same value
    i[0] = (i[0] == 0x8000000) ? 0 : i[0];
    i[1] = (i[1] == 0x8000000) ? 0 : i[1];
    i[2] = (i[2] == 0x8000000) ? 0 : i[2];

    // scramble bits to make sure that integer coordinates have entropy in lower bits
    i[0] ^= i[0] >> 17;
    i[1] ^= i[1] >> 17;
    i[2] ^= i[2] >> 17;

    // Optimized Spatial Hashing for Collision Detection of Deformable Objects
    unsigned int h = (i[0] * 73856093) ^ (i[1] * 19349663) ^ (i[2] * 83492791);

    return hashpow2(t, h);
}

/*
** returns the `main' position of an element in a table (that is, the index
** of its hash value)
*/
static LuaNode* mainposition(const Table* t, const TValue* key)
{
    switch (ttype(key))
    {
    case LUA_TNUMBER:
        return hashnum(t, nvalue(key));
    case LUA_TVECTOR:
        return hashvec(t, vvalue(key));
    case LUA_TSTRING:
        return hashstr(t, tsvalue(key));
    case LUA_TBOOLEAN:
        return hashboolean(t, bvalue(key));
    case LUA_TLIGHTUSERDATA:
        return hashpointer(t, pvalue(key));
    default:
        return hashpointer(t, gcvalue(key));
    }
}

/*
** returns the index for `key' if `key' is an appropriate key to live in
** the array part of the table, -1 otherwise.
*/
static int arrayindex(double key)
{
    int i;
    luai_num2int(i, key);

    return luai_numeq(cast_num(i), key) ? i : -1;
}

/*
** returns the index of a `key' for table traversals. First goes all
** elements in the array part, then elements in the hash part. The
** beginning of a traversal is signalled by -1.
*/
static int findindex(lua_State* L, Table* t, StkId key)
{
    int i;
    if (ttisnil(key))
        return -1; /* first iteration */
    i = ttisnumber(key) ? arrayindex(nvalue(key)) : -1;
    if (0 < i && i <= t->sizearray) /* is `key' inside array part? */
        return i - 1;               /* yes; that's the index (corrected to C) */
    else
    {
        LuaNode* n = mainposition(t, key);
        for (;;)
        { /* check whether `key' is somewhere in the chain */
            /* key may be dead already, but it is ok to use it in `next' */
            if (luaO_rawequalKey(gkey(n), key) || (ttype(gkey(n)) == LUA_TDEADKEY && iscollectable(key) && gcvalue(gkey(n)) == gcvalue(key)))
            {
                i = cast_int(n - gnode(t, 0)); /* key index in hash table */
                /* hash elements are numbered after array ones */
                return i + t->sizearray;
            }
            if (gnext(n) == 0)
                break;
            n += gnext(n);
        }
        luaG_runerror(L, "invalid key to 'next'"); /* key not found */
    }
}

int luaH_next(lua_State* L, Table* t, StkId key)
{
    int i = findindex(L, t, key); /* find original element */
    for (i++; i < t->sizearray; i++)
    { /* try first array part */
        if (!ttisnil(&t->array[i]))
        { /* a non-nil value? */
            setnvalue(key, cast_num(i + 1));
            setobj2s(L, key + 1, &t->array[i]);
            return 1;
        }
    }
    for (i -= t->sizearray; i < sizenode(t); i++)
    { /* then hash part */
        if (!ttisnil(gval(gnode(t, i))))
        { /* a non-nil value? */
            getnodekey(L, key, gnode(t, i));
            setobj2s(L, key + 1, gval(gnode(t, i)));
            return 1;
        }
    }
    return 0; /* no more elements */
}

/*
** {=============================================================
** Rehash
** ==============================================================
*/

#define maybesetaboundary(t, boundary) \
    { \
        if (FFlag::LuauArrayBoundary && t->aboundary <= 0) \
            t->aboundary = -int(boundary); \
    }

#define getaboundary(t) (t->aboundary < 0 ? -t->aboundary : t->sizearray)

static int computesizes(int nums[], int* narray)
{
    int i;
    int twotoi; /* 2^i */
    int a = 0;  /* number of elements smaller than 2^i */
    int na = 0; /* number of elements to go to array part */
    int n = 0;  /* optimal size for array part */
    for (i = 0, twotoi = 1; twotoi / 2 < *narray; i++, twotoi *= 2)
    {
        if (nums[i] > 0)
        {
            a += nums[i];
            if (a > twotoi / 2)
            {               /* more than half elements present? */
                n = twotoi; /* optimal size (till now) */
                na = a;     /* all elements smaller than n will go to array part */
            }
        }
        if (a == *narray)
            break; /* all elements already counted */
    }
    *narray = n;
    LUAU_ASSERT(*narray / 2 <= na && na <= *narray);
    return na;
}

static int countint(double key, int* nums)
{
    int k = arrayindex(key);
    if (0 < k && k <= MAXSIZE)
    {                        /* is `key' an appropriate array index? */
        nums[ceillog2(k)]++; /* count as such */
        return 1;
    }
    else
        return 0;
}

static int numusearray(const Table* t, int* nums)
{
    int lg;
    int ttlg;     /* 2^lg */
    int ause = 0; /* summation of `nums' */
    int i = 1;    /* count to traverse all array keys */
    for (lg = 0, ttlg = 1; lg <= MAXBITS; lg++, ttlg *= 2)
    {               /* for each slice */
        int lc = 0; /* counter */
        int lim = ttlg;
        if (lim > t->sizearray)
        {
            lim = t->sizearray; /* adjust upper limit */
            if (i > lim)
                break; /* no more elements to count */
        }
        /* count elements in range (2^(lg-1), 2^lg] */
        for (; i <= lim; i++)
        {
            if (!ttisnil(&t->array[i - 1]))
                lc++;
        }
        nums[lg] += lc;
        ause += lc;
    }
    return ause;
}

static int numusehash(const Table* t, int* nums, int* pnasize)
{
    int totaluse = 0; /* total number of elements */
    int ause = 0;     /* summation of `nums' */
    int i = sizenode(t);
    while (i--)
    {
        LuaNode* n = &t->node[i];
        if (!ttisnil(gval(n)))
        {
            if (ttisnumber(gkey(n)))
                ause += countint(nvalue(gkey(n)), nums);
            totaluse++;
        }
    }
    *pnasize += ause;
    return totaluse;
}

static void setarrayvector(lua_State* L, Table* t, int size)
{
    if (size > MAXSIZE)
        luaG_runerror(L, "table overflow");
    luaM_reallocarray(L, t->array, t->sizearray, size, TValue, t->memcat);
    TValue* array = t->array;
    for (int i = t->sizearray; i < size; i++)
        setnilvalue(&array[i]);
    t->sizearray = size;
}

static void setnodevector(lua_State* L, Table* t, int size)
{
    int lsize;
    if (size == 0)
    {                                           /* no elements to hash part? */
        t->node = cast_to(LuaNode*, dummynode); /* use common `dummynode' */
        lsize = 0;
    }
    else
    {
        int i;
        lsize = ceillog2(size);
        if (lsize > MAXBITS)
            luaG_runerror(L, "table overflow");
        size = twoto(lsize);
        t->node = luaM_newarray(L, size, LuaNode, t->memcat);
        for (i = 0; i < size; i++)
        {
            LuaNode* n = gnode(t, i);
            gnext(n) = 0;
            setnilvalue(gkey(n));
            setnilvalue(gval(n));
        }
    }
    t->lsizenode = cast_byte(lsize);
    t->nodemask8 = cast_byte((1 << lsize) - 1);
    t->lastfree = size; /* all positions are free */
}

static void resize(lua_State* L, Table* t, int nasize, int nhsize)
{
    if (nasize > MAXSIZE || nhsize > MAXSIZE)
        luaG_runerror(L, "table overflow");
    int oldasize = t->sizearray;
    int oldhsize = t->lsizenode;
    LuaNode* nold = t->node; /* save old hash ... */
    if (nasize > oldasize)   /* array part must grow? */
        setarrayvector(L, t, nasize);
    /* create new hash part with appropriate size */
    setnodevector(L, t, nhsize);
    if (nasize < oldasize)
    { /* array part must shrink? */
        t->sizearray = nasize;
        /* re-insert elements from vanishing slice */
        for (int i = nasize; i < oldasize; i++)
        {
            if (!ttisnil(&t->array[i]))
                setobjt2t(L, luaH_setnum(L, t, i + 1), &t->array[i]);
        }
        /* shrink array */
        luaM_reallocarray(L, t->array, oldasize, nasize, TValue, t->memcat);
    }
    /* re-insert elements from hash part */
    for (int i = twoto(oldhsize) - 1; i >= 0; i--)
    {
        LuaNode* old = nold + i;
        if (!ttisnil(gval(old)))
        {
            TValue ok;
            getnodekey(L, &ok, old);
            setobjt2t(L, luaH_set(L, t, &ok), gval(old));
        }
    }
    if (nold != dummynode)
        luaM_freearray(L, nold, twoto(oldhsize), LuaNode, t->memcat); /* free old array */
}

void luaH_resizearray(lua_State* L, Table* t, int nasize)
{
    int nsize = (t->node == dummynode) ? 0 : sizenode(t);
    resize(L, t, nasize, nsize);
}

void luaH_resizehash(lua_State* L, Table* t, int nhsize)
{
    resize(L, t, t->sizearray, nhsize);
}

static void rehash(lua_State* L, Table* t, const TValue* ek)
{
    int nums[MAXBITS + 1]; /* nums[i] = number of keys between 2^(i-1) and 2^i */
    for (int i = 0; i <= MAXBITS; i++)
        nums[i] = 0;                          /* reset counts */
    int nasize = numusearray(t, nums);        /* count keys in array part */
    int totaluse = nasize;                    /* all those keys are integer keys */
    totaluse += numusehash(t, nums, &nasize); /* count keys in hash part */
    /* count extra key */
    if (ttisnumber(ek))
        nasize += countint(nvalue(ek), nums);
    totaluse++;
    /* compute new size for array part */
    int na = computesizes(nums, &nasize);
    /* resize the table to new computed sizes */
    resize(L, t, nasize, totaluse - na);
}

/*
** }=============================================================
*/

Table* luaH_new(lua_State* L, int narray, int nhash)
{
    Table* t = luaM_new(L, Table, sizeof(Table), L->activememcat);
    luaC_link(L, t, LUA_TTABLE);
    t->metatable = NULL;
    t->flags = cast_byte(~0);
    t->array = NULL;
    t->sizearray = 0;
    t->lastfree = 0;
    t->lsizenode = 0;
    t->readonly = 0;
    t->safeenv = 0;
    t->nodemask8 = 0;
    t->node = cast_to(LuaNode*, dummynode);
    if (narray > 0)
        setarrayvector(L, t, narray);
    if (nhash > 0)
        setnodevector(L, t, nhash);
    return t;
}

void luaH_free(lua_State* L, Table* t)
{
    if (t->node != dummynode)
        luaM_freearray(L, t->node, sizenode(t), LuaNode, t->memcat);
    luaM_freearray(L, t->array, t->sizearray, TValue, t->memcat);
    luaM_free(L, t, sizeof(Table), t->memcat);
}

static LuaNode* getfreepos(Table* t)
{
    while (t->lastfree > 0)
    {
        t->lastfree--;

        LuaNode* n = gnode(t, t->lastfree);
        if (ttisnil(gkey(n)))
            return n;
    }
    return NULL; /* could not find a free place */
}

/*
** inserts a new key into a hash table; first, check whether key's main
** position is free. If not, check whether colliding node is in its main
** position or not: if it is not, move colliding node to an empty place and
** put new key in its main position; otherwise (colliding node is in its main
** position), new key goes to an empty position.
*/
static TValue* newkey(lua_State* L, Table* t, const TValue* key)
{
    LuaNode* mp = mainposition(t, key);
    if (!ttisnil(gval(mp)) || mp == dummynode)
    {
        LuaNode* othern;
        LuaNode* n = getfreepos(t); /* get a free place */
        if (n == NULL)
        {                               /* cannot find a free place? */
            rehash(L, t, key);          /* grow table */
            return luaH_set(L, t, key); /* re-insert key into grown table */
        }
        LUAU_ASSERT(n != dummynode);
        TValue mk;
        getnodekey(L, &mk, mp);
        othern = mainposition(t, &mk);
        if (othern != mp)
        { /* is colliding node out of its main position? */
            /* yes; move colliding node into free position */
            while (othern + gnext(othern) != mp)
                othern += gnext(othern);          /* find previous */
            gnext(othern) = cast_int(n - othern); /* redo the chain with `n' in place of `mp' */
            *n = *mp;                             /* copy colliding node into free pos. (mp->next also goes) */
            if (gnext(mp) != 0)
            {
                gnext(n) += cast_int(mp - n); /* correct 'next' */
                gnext(mp) = 0;                /* now 'mp' is free */
            }
            setnilvalue(gval(mp));
        }
        else
        { /* colliding node is in its own main position */
            /* new node will go into free position */
            if (gnext(mp) != 0)
                gnext(n) = cast_int((mp + gnext(mp)) - n); /* chain new position */
            else
                LUAU_ASSERT(gnext(n) == 0);
            gnext(mp) = cast_int(n - mp);
            mp = n;
        }
    }
    setnodekey(L, mp, key);
    luaC_barriert(L, t, key);
    LUAU_ASSERT(ttisnil(gval(mp)));
    return gval(mp);
}

/*
** search function for integers
*/
const TValue* luaH_getnum(Table* t, int key)
{
    /* (1 <= key && key <= t->sizearray) */
    if (cast_to(unsigned int, key - 1) < cast_to(unsigned int, t->sizearray))
        return &t->array[key - 1];
    else if (t->node != dummynode)
    {
        double nk = cast_num(key);
        LuaNode* n = hashnum(t, nk);
        for (;;)
        { /* check whether `key' is somewhere in the chain */
            if (ttisnumber(gkey(n)) && luai_numeq(nvalue(gkey(n)), nk))
                return gval(n); /* that's it */
            if (gnext(n) == 0)
                break;
            n += gnext(n);
        }
        return luaO_nilobject;
    }
    else
        return luaO_nilobject;
}

/*
** search function for strings
*/
const TValue* luaH_getstr(Table* t, TString* key)
{
    LuaNode* n = hashstr(t, key);
    for (;;)
    { /* check whether `key' is somewhere in the chain */
        if (ttisstring(gkey(n)) && tsvalue(gkey(n)) == key)
            return gval(n); /* that's it */
        if (gnext(n) == 0)
            break;
        n += gnext(n);
    }
    return luaO_nilobject;
}

/*
** main search function
*/
const TValue* luaH_get(Table* t, const TValue* key)
{
    switch (ttype(key))
    {
    case LUA_TNIL:
        return luaO_nilobject;
    case LUA_TSTRING:
        return luaH_getstr(t, tsvalue(key));
    case LUA_TNUMBER:
    {
        int k;
        double n = nvalue(key);
        luai_num2int(k, n);
        if (luai_numeq(cast_num(k), nvalue(key))) /* index is int? */
            return luaH_getnum(t, k);             /* use specialized version */
                                                  /* else go through */
    }
    default:
    {
        LuaNode* n = mainposition(t, key);
        for (;;)
        { /* check whether `key' is somewhere in the chain */
            if (luaO_rawequalKey(gkey(n), key))
                return gval(n); /* that's it */
            if (gnext(n) == 0)
                break;
            n += gnext(n);
        }
        return luaO_nilobject;
    }
    }
}

TValue* luaH_set(lua_State* L, Table* t, const TValue* key)
{
    const TValue* p = luaH_get(t, key);
    invalidateTMcache(t);
    if (p != luaO_nilobject)
        return cast_to(TValue*, p);
    else
    {
        if (ttisnil(key))
            luaG_runerror(L, "table index is nil");
        else if (ttisnumber(key) && luai_numisnan(nvalue(key)))
            luaG_runerror(L, "table index is NaN");
        else if (ttisvector(key) && luai_vecisnan(vvalue(key)))
            luaG_runerror(L, "table index contains NaN");
        return newkey(L, t, key);
    }
}

TValue* luaH_setnum(lua_State* L, Table* t, int key)
{
    /* (1 <= key && key <= t->sizearray) */
    if (cast_to(unsigned int, key - 1) < cast_to(unsigned int, t->sizearray))
        return &t->array[key - 1];
    /* hash fallback */
    const TValue* p = luaH_getnum(t, key);
    if (p != luaO_nilobject)
        return cast_to(TValue*, p);
    else
    {
        TValue k;
        setnvalue(&k, cast_num(key));
        return newkey(L, t, &k);
    }
}

TValue* luaH_setstr(lua_State* L, Table* t, TString* key)
{
    const TValue* p = luaH_getstr(t, key);
    invalidateTMcache(t);
    if (p != luaO_nilobject)
        return cast_to(TValue*, p);
    else
    {
        TValue k;
        setsvalue(L, &k, key);
        return newkey(L, t, &k);
    }
}

static LUAU_NOINLINE int unbound_search(Table* t, unsigned int j)
{
    unsigned int i = j; /* i is zero or a present index */
    j++;
    /* find `i' and `j' such that i is present and j is not */
    while (!ttisnil(luaH_getnum(t, j)))
    {
        i = j;
        j *= 2;
        if (j > cast_to(unsigned int, INT_MAX))
        { /* overflow? */
            /* table was built with bad purposes: resort to linear search */
            i = 1;
            while (!ttisnil(luaH_getnum(t, i)))
                i++;
            return i - 1;
        }
    }
    /* now do a binary search between them */
    while (j - i > 1)
    {
        unsigned int m = (i + j) / 2;
        if (ttisnil(luaH_getnum(t, m)))
            j = m;
        else
            i = m;
    }
    return i;
}

static int updateaboundary(Table* t, int boundary)
{
    if (boundary < t->sizearray && ttisnil(&t->array[boundary - 1]))
    {
        if (boundary >= 2 && !ttisnil(&t->array[boundary - 2]))
        {
            maybesetaboundary(t, boundary - 1);
            return boundary - 1;
        }
    }
    else if (boundary + 1 < t->sizearray && !ttisnil(&t->array[boundary]) && ttisnil(&t->array[boundary + 1]))
    {
        maybesetaboundary(t, boundary + 1);
        return boundary + 1;
    }

    return 0;
}

/*
** Try to find a boundary in table `t'. A `boundary' is an integer index
** such that t[i] is non-nil and t[i+1] is nil (and 0 if t[1] is nil).
*/
int luaH_getn(Table* t)
{
    int boundary = getaboundary(t);

    if (FFlag::LuauArrayBoundary && boundary > 0)
    {
        if (!ttisnil(&t->array[t->sizearray - 1]) && t->node == dummynode)
            return t->sizearray; /* fast-path: the end of the array in `t' already refers to a boundary */
        if (boundary < t->sizearray && !ttisnil(&t->array[boundary - 1]) && ttisnil(&t->array[boundary]))
            return boundary; /* fast-path: boundary already refers to a boundary in `t' */

        int foundboundary = updateaboundary(t, boundary);
        if (foundboundary > 0)
            return foundboundary;
    }

    int j = t->sizearray;

    if (j > 0 && ttisnil(&t->array[j - 1]))
    {
        // "branchless" binary search from Array Layouts for Comparison-Based Searching, Paul Khuong, Pat Morin, 2017.
        // note that clang is cmov-shy on cmovs around memory operands, so it will compile this to a branchy loop.
        TValue* base = t->array;
        int rest = j;
        while (int half = rest >> 1)
        {
            base = ttisnil(&base[half]) ? base : base + half;
            rest -= half;
        }
        int boundary = !ttisnil(base) + int(base - t->array);
        maybesetaboundary(t, boundary);
        return boundary;
    }
    /* else must find a boundary in hash part */
    else if (t->node == dummynode) /* hash part is empty? */
        return j;                  /* that is easy... */
    else
        return unbound_search(t, j);
}

Table* luaH_clone(lua_State* L, Table* tt)
{
    Table* t = luaM_new(L, Table, sizeof(Table), L->activememcat);
    luaC_link(L, t, LUA_TTABLE);
    t->metatable = tt->metatable;
    t->flags = tt->flags;
    t->array = NULL;
    t->sizearray = 0;
    t->lsizenode = 0;
    t->nodemask8 = 0;
    t->readonly = 0;
    t->safeenv = 0;
    t->node = cast_to(LuaNode*, dummynode);
    t->lastfree = 0;

    if (tt->sizearray)
    {
        t->array = luaM_newarray(L, tt->sizearray, TValue, t->memcat);
        maybesetaboundary(t, getaboundary(tt));
        t->sizearray = tt->sizearray;

        memcpy(t->array, tt->array, t->sizearray * sizeof(TValue));
    }

    if (tt->node != dummynode)
    {
        int size = 1 << tt->lsizenode;
        t->node = luaM_newarray(L, size, LuaNode, t->memcat);
        t->lsizenode = tt->lsizenode;
        t->nodemask8 = tt->nodemask8;
        memcpy(t->node, tt->node, size * sizeof(LuaNode));
        t->lastfree = tt->lastfree;
    }

    return t;
}

void luaH_clear(Table* tt)
{
    /* clear array part */
    for (int i = 0; i < tt->sizearray; ++i)
    {
        setnilvalue(&tt->array[i]);
    }

    maybesetaboundary(tt, 0);

    /* clear hash part */
    if (tt->node != dummynode)
    {
        int size = sizenode(tt);
        tt->lastfree = size;
        for (int i = 0; i < size; ++i)
        {
            LuaNode* n = gnode(tt, i);
            setnilvalue(gkey(n));
            setnilvalue(gval(n));
            gnext(n) = 0;
        }
    }

    /* back to empty -> no tag methods present */
    tt->flags = cast_byte(~0);
}
