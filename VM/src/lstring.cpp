// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lstring.h"

#include "lgc.h"
#include "lmem.h"

#include <string.h>

LUAU_FASTFLAG(LuauGcPagedSweep)

unsigned int luaS_hash(const char* str, size_t len)
{
    // Note that this hashing algorithm is replicated in BytecodeBuilder.cpp, BytecodeBuilder::getStringHash
    unsigned int a = 0, b = 0;
    unsigned int h = unsigned(len);

    // hash prefix in 12b chunks (using aligned reads) with ARX based hash (LuaJIT v2.1, lookup3)
    // note that we stop at length<32 to maintain compatibility with Lua 5.1
    while (len >= 32)
    {
#define rol(x, s) ((x >> s) | (x << (32 - s)))
#define mix(u, v, w) a ^= h, a -= rol(h, u), b ^= a, b -= rol(a, v), h ^= b, h -= rol(b, w)

        // should compile into fast unaligned reads
        uint32_t block[3];
        memcpy(block, str, 12);

        a += block[0];
        b += block[1];
        h += block[2];
        mix(14, 11, 25);
        str += 12;
        len -= 12;

#undef mix
#undef rol
    }

    // original Lua 5.1 hash for compatibility (exact match when len<32)
    for (size_t i = len; i > 0; --i)
        h ^= (h << 5) + (h >> 2) + (uint8_t)str[i - 1];

    return h;
}

void luaS_resize(lua_State* L, int newsize)
{
    if (L->global->gcstate == GCSsweepstring)
        return; /* cannot resize during GC traverse */
    TString** newhash = luaM_newarray(L, newsize, TString*, 0);
    stringtable* tb = &L->global->strt;
    for (int i = 0; i < newsize; i++)
        newhash[i] = NULL;
    /* rehash */
    for (int i = 0; i < tb->size; i++)
    {
        TString* p = tb->hash[i];
        while (p)
        {                                 /* for each node in the list */
            // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
            TString* next = (TString*)p->next; /* save next */
            unsigned int h = p->hash;
            int h1 = lmod(h, newsize); /* new position */
            LUAU_ASSERT(cast_int(h % newsize) == lmod(h, newsize));
            // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
            p->next = (GCObject*)newhash[h1]; /* chain it */
            newhash[h1] = p;
            p = next;
        }
    }
    luaM_freearray(L, tb->hash, tb->size, TString*, 0);
    tb->size = newsize;
    tb->hash = newhash;
}

static TString* newlstr(lua_State* L, const char* str, size_t l, unsigned int h)
{
    TString* ts;
    stringtable* tb;
    if (l > MAXSSIZE)
        luaM_toobig(L);
    ts = luaM_newgco(L, TString, sizestring(l), L->activememcat);
    ts->len = unsigned(l);
    ts->hash = h;
    ts->marked = luaC_white(L->global);
    ts->tt = LUA_TSTRING;
    ts->memcat = L->activememcat;
    memcpy(ts->data, str, l);
    ts->data[l] = '\0'; /* ending 0 */
    ts->atom = L->global->cb.useratom ? L->global->cb.useratom(ts->data, l) : -1;
    tb = &L->global->strt;
    h = lmod(h, tb->size);
    // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the case will not be required
    ts->next = (GCObject*)tb->hash[h]; /* chain new entry */
    tb->hash[h] = ts;
    tb->nuse++;
    if (tb->nuse > cast_to(uint32_t, tb->size) && tb->size <= INT_MAX / 2)
        luaS_resize(L, tb->size * 2); /* too crowded */
    return ts;
}

static void linkstrbuf(lua_State* L, TString* ts)
{
    global_State* g = L->global;

    if (FFlag::LuauGcPagedSweep)
    {
        // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
        ts->next = (GCObject*)g->strbufgc;
        g->strbufgc = ts;
        ts->marked = luaC_white(g);
    }
    else
    {
        GCObject* o = obj2gco(ts);
        o->gch.next = (GCObject*)g->strbufgc;
        g->strbufgc = gco2ts(o);
        o->gch.marked = luaC_white(g);
    }
}

static void unlinkstrbuf(lua_State* L, TString* ts)
{
    global_State* g = L->global;

    TString** p = &g->strbufgc;

    while (TString* curr = *p)
    {
        if (curr == ts)
        {
            // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
            *p = (TString*)curr->next;
            return;
        }
        else
        {
            // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
            p = (TString**)&curr->next;
        }
    }

    LUAU_ASSERT(!"failed to find string buffer");
}

TString* luaS_bufstart(lua_State* L, size_t size)
{
    if (size > MAXSSIZE)
        luaM_toobig(L);

    TString* ts = luaM_newgco(L, TString, sizestring(size), L->activememcat);

    ts->tt = LUA_TSTRING;
    ts->memcat = L->activememcat;
    linkstrbuf(L, ts);

    ts->len = unsigned(size);

    return ts;
}

TString* luaS_buffinish(lua_State* L, TString* ts)
{
    unsigned int h = luaS_hash(ts->data, ts->len);
    stringtable* tb = &L->global->strt;
    int bucket = lmod(h, tb->size);

    // search if we already have this string in the hash table
    // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
    for (TString* el = tb->hash[bucket]; el != NULL; el = (TString*)el->next)
    {
        if (el->len == ts->len && memcmp(el->data, ts->data, ts->len) == 0)
        {
            // string may be dead
            if (isdead(L->global, obj2gco(el)))
                changewhite(obj2gco(el));

            return el;
        }
    }

    unlinkstrbuf(L, ts);

    ts->hash = h;
    ts->data[ts->len] = '\0'; // ending 0

    // Complete string object
    ts->atom = L->global->cb.useratom ? L->global->cb.useratom(ts->data, ts->len) : -1;
    // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
    ts->next = (GCObject*)tb->hash[bucket]; // chain new entry
    tb->hash[bucket] = ts;

    tb->nuse++;
    if (tb->nuse > cast_to(uint32_t, tb->size) && tb->size <= INT_MAX / 2)
        luaS_resize(L, tb->size * 2); // too crowded

    return ts;
}

TString* luaS_newlstr(lua_State* L, const char* str, size_t l)
{
    unsigned int h = luaS_hash(str, l);
    // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
    for (TString* el = L->global->strt.hash[lmod(h, L->global->strt.size)]; el != NULL; el = (TString*)el->next)
    {
        if (el->len == l && (memcmp(str, getstr(el), l) == 0))
        {
            /* string may be dead */
            if (isdead(L->global, obj2gco(el)))
                changewhite(obj2gco(el));
            return el;
        }
    }
    return newlstr(L, str, l, h); /* not found */
}

static bool unlinkstr(lua_State* L, TString* ts)
{
    LUAU_ASSERT(FFlag::LuauGcPagedSweep);

    global_State* g = L->global;

    TString** p = &g->strt.hash[lmod(ts->hash, g->strt.size)];

    while (TString* curr = *p)
    {
        if (curr == ts)
        {
            // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
            *p = (TString*)curr->next;
            return true;
        }
        else
        {
            // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
            p = (TString**)&curr->next;
        }
    }

    return false;
}

void luaS_free(lua_State* L, TString* ts, lua_Page* page)
{
    if (FFlag::LuauGcPagedSweep)
    {
        // Unchain from the string table
        if (!unlinkstr(L, ts))
            unlinkstrbuf(L, ts); // An unlikely scenario when we have a string buffer on our hands
        else
            L->global->strt.nuse--;

        luaM_freegco(L, ts, sizestring(ts->len), ts->memcat, page);
    }
    else
    {
        L->global->strt.nuse--;

        luaM_free(L, ts, sizestring(ts->len), ts->memcat);
    }
}
