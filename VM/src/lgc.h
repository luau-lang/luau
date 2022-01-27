// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#pragma once

#include "ldo.h"
#include "lobject.h"
#include "lstate.h"

/*
** Possible states of the Garbage Collector
*/
#define GCSpause 0
#define GCSpropagate 1
#define GCSpropagateagain 2
#define GCSatomic 3
// TODO: remove with FFlagLuauGcPagedSweep
#define GCSsweepstring 4
#define GCSsweep 5

/*
** macro to tell when main invariant (white objects cannot point to black
** ones) must be kept. During a collection, the sweep
** phase may break the invariant, as objects turned white may point to
** still-black objects. The invariant is restored when sweep ends and
** all objects are white again.
*/
#define keepinvariant(g) ((g)->gcstate == GCSpropagate || (g)->gcstate == GCSpropagateagain || (g)->gcstate == GCSatomic)

/*
** some useful bit tricks
*/
#define resetbits(x, m) ((x) &= cast_to(uint8_t, ~(m)))
#define setbits(x, m) ((x) |= (m))
#define testbits(x, m) ((x) & (m))
#define bitmask(b) (1 << (b))
#define bit2mask(b1, b2) (bitmask(b1) | bitmask(b2))
#define l_setbit(x, b) setbits(x, bitmask(b))
#define resetbit(x, b) resetbits(x, bitmask(b))
#define testbit(x, b) testbits(x, bitmask(b))
#define set2bits(x, b1, b2) setbits(x, (bit2mask(b1, b2)))
#define reset2bits(x, b1, b2) resetbits(x, (bit2mask(b1, b2)))
#define test2bits(x, b1, b2) testbits(x, (bit2mask(b1, b2)))

/*
** Layout for bit use in `marked' field:
** bit 0 - object is white (type 0)
** bit 1 - object is white (type 1)
** bit 2 - object is black
** bit 3 - object is fixed (should not be collected)
*/

#define WHITE0BIT 0
#define WHITE1BIT 1
#define BLACKBIT 2
#define FIXEDBIT 3
#define WHITEBITS bit2mask(WHITE0BIT, WHITE1BIT)

#define iswhite(x) test2bits((x)->gch.marked, WHITE0BIT, WHITE1BIT)
#define isblack(x) testbit((x)->gch.marked, BLACKBIT)
#define isgray(x) (!testbits((x)->gch.marked, WHITEBITS | bitmask(BLACKBIT)))
#define isfixed(x) testbit((x)->gch.marked, FIXEDBIT)

#define otherwhite(g) (g->currentwhite ^ WHITEBITS)
#define isdead(g, v) (((v)->gch.marked & (WHITEBITS | bitmask(FIXEDBIT))) == (otherwhite(g) & WHITEBITS))

#define changewhite(x) ((x)->gch.marked ^= WHITEBITS)
#define gray2black(x) l_setbit((x)->gch.marked, BLACKBIT)

#define luaC_white(g) cast_to(uint8_t, ((g)->currentwhite) & WHITEBITS)

// Thread stack states
#define THREAD_ACTIVEBIT 0   // thread is currently active
#define THREAD_SLEEPINGBIT 1 // thread is not executing and stack should not be modified

#define luaC_threadactive(L) (testbit((L)->stackstate, THREAD_ACTIVEBIT))
#define luaC_threadsleeping(L) (testbit((L)->stackstate, THREAD_SLEEPINGBIT))

#define luaC_checkGC(L) \
    { \
        condhardstacktests(luaD_reallocstack(L, L->stacksize - EXTRA_STACK - 1)); \
        if (L->global->totalbytes >= L->global->GCthreshold) \
        { \
            condhardmemtests(luaC_validate(L), 1); \
            luaC_step(L, true); \
        } \
        else \
        { \
            condhardmemtests(luaC_validate(L), 2); \
        } \
    }

#define luaC_barrier(L, p, v) \
    { \
        if (iscollectable(v) && isblack(obj2gco(p)) && iswhite(gcvalue(v))) \
            luaC_barrierf(L, obj2gco(p), gcvalue(v)); \
    }

#define luaC_barriert(L, t, v) \
    { \
        if (iscollectable(v) && isblack(obj2gco(t)) && iswhite(gcvalue(v))) \
            luaC_barriertable(L, t, gcvalue(v)); \
    }

#define luaC_barrierfast(L, t) \
    { \
        if (isblack(obj2gco(t))) \
            luaC_barrierback(L, t); \
    }

#define luaC_objbarrier(L, p, o) \
    { \
        if (isblack(obj2gco(p)) && iswhite(obj2gco(o))) \
            luaC_barrierf(L, obj2gco(p), obj2gco(o)); \
    }

// TODO: remove with FFlagLuauGcForwardMetatableBarrier
#define luaC_objbarriert(L, t, o) \
    { \
        if (isblack(obj2gco(t)) && iswhite(obj2gco(o))) \
            luaC_barriertable(L, t, obj2gco(o)); \
    }

#define luaC_upvalbarrier(L, uv, tv) \
    { \
        if (iscollectable(tv) && iswhite(gcvalue(tv)) && (!(uv) || ((UpVal*)uv)->v != &((UpVal*)uv)->u.value)) \
            luaC_barrierupval(L, gcvalue(tv)); \
    }

#define luaC_checkthreadsleep(L) \
    { \
        if (luaC_threadsleeping(L)) \
            luaC_wakethread(L); \
    }

#define luaC_link(L, o, tt) luaC_linkobj(L, cast_to(GCObject*, (o)), tt)

LUAI_FUNC void luaC_freeall(lua_State* L);
LUAI_FUNC void luaC_step(lua_State* L, bool assist);
LUAI_FUNC void luaC_fullgc(lua_State* L);
LUAI_FUNC void luaC_linkobj(lua_State* L, GCObject* o, uint8_t tt);
LUAI_FUNC void luaC_linkupval(lua_State* L, UpVal* uv);
LUAI_FUNC void luaC_barrierupval(lua_State* L, GCObject* v);
LUAI_FUNC void luaC_barrierf(lua_State* L, GCObject* o, GCObject* v);
LUAI_FUNC void luaC_barriertable(lua_State* L, Table* t, GCObject* v);
LUAI_FUNC void luaC_barrierback(lua_State* L, Table* t);
LUAI_FUNC void luaC_validate(lua_State* L);
LUAI_FUNC void luaC_dump(lua_State* L, void* file, const char* (*categoryName)(lua_State* L, uint8_t memcat));
LUAI_FUNC int64_t luaC_allocationrate(lua_State* L);
LUAI_FUNC void luaC_wakethread(lua_State* L);
LUAI_FUNC const char* luaC_statename(int state);
