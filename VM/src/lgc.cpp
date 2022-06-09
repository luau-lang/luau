// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lgc.h"

#include "lobject.h"
#include "lstate.h"
#include "ltable.h"
#include "lfunc.h"
#include "lstring.h"
#include "ldo.h"
#include "lmem.h"
#include "ludata.h"

#include <string.h>

#define GC_SWEEPPAGESTEPCOST 16

#define GC_INTERRUPT(state) \
    { \
        void (*interrupt)(lua_State*, int) = g->cb.interrupt; \
        if (LUAU_UNLIKELY(!!interrupt)) \
            interrupt(L, state); \
    }

#define maskmarks cast_byte(~(bitmask(BLACKBIT) | WHITEBITS))

#define makewhite(g, x) ((x)->gch.marked = cast_byte(((x)->gch.marked & maskmarks) | luaC_white(g)))

#define white2gray(x) reset2bits((x)->gch.marked, WHITE0BIT, WHITE1BIT)
#define black2gray(x) resetbit((x)->gch.marked, BLACKBIT)

#define stringmark(s) reset2bits((s)->marked, WHITE0BIT, WHITE1BIT)

#define markvalue(g, o) \
    { \
        checkconsistency(o); \
        if (iscollectable(o) && iswhite(gcvalue(o))) \
            reallymarkobject(g, gcvalue(o)); \
    }

#define markobject(g, t) \
    { \
        if (iswhite(obj2gco(t))) \
            reallymarkobject(g, obj2gco(t)); \
    }

#ifdef LUAI_GCMETRICS
static void recordGcStateStep(global_State* g, int startgcstate, double seconds, bool assist, size_t work)
{
    switch (startgcstate)
    {
    case GCSpause:
        // record root mark time if we have switched to next state
        if (g->gcstate == GCSpropagate)
        {
            g->gcmetrics.currcycle.marktime += seconds;

            if (assist)
                g->gcmetrics.currcycle.markassisttime += seconds;
        }
        break;
    case GCSpropagate:
    case GCSpropagateagain:
        g->gcmetrics.currcycle.marktime += seconds;
        g->gcmetrics.currcycle.markwork += work;

        if (assist)
            g->gcmetrics.currcycle.markassisttime += seconds;
        break;
    case GCSatomic:
        g->gcmetrics.currcycle.atomictime += seconds;
        break;
    case GCSsweep:
        g->gcmetrics.currcycle.sweeptime += seconds;
        g->gcmetrics.currcycle.sweepwork += work;

        if (assist)
            g->gcmetrics.currcycle.sweepassisttime += seconds;
        break;
    default:
        LUAU_ASSERT(!"Unexpected GC state");
    }

    if (assist)
    {
        g->gcmetrics.stepassisttimeacc += seconds;
        g->gcmetrics.currcycle.assistwork += work;
    }
    else
    {
        g->gcmetrics.stepexplicittimeacc += seconds;
        g->gcmetrics.currcycle.explicitwork += work;
    }
}

static double recordGcDeltaTime(double& timer)
{
    double now = lua_clock();
    double delta = now - timer;
    timer = now;
    return delta;
}

static void startGcCycleMetrics(global_State* g)
{
    g->gcmetrics.currcycle.starttimestamp = lua_clock();
    g->gcmetrics.currcycle.pausetime = g->gcmetrics.currcycle.starttimestamp - g->gcmetrics.lastcycle.endtimestamp;
}

static void finishGcCycleMetrics(global_State* g)
{
    g->gcmetrics.currcycle.endtimestamp = lua_clock();
    g->gcmetrics.currcycle.endtotalsizebytes = g->totalbytes;

    g->gcmetrics.completedcycles++;
    g->gcmetrics.lastcycle = g->gcmetrics.currcycle;
    g->gcmetrics.currcycle = GCCycleMetrics();

    g->gcmetrics.currcycle.starttotalsizebytes = g->totalbytes;
    g->gcmetrics.currcycle.heaptriggersizebytes = g->GCthreshold;
}
#endif

static void removeentry(LuaNode* n)
{
    LUAU_ASSERT(ttisnil(gval(n)));
    if (iscollectable(gkey(n)))
        setttype(gkey(n), LUA_TDEADKEY); /* dead key; remove it */
}

static void reallymarkobject(global_State* g, GCObject* o)
{
    LUAU_ASSERT(iswhite(o) && !isdead(g, o));
    white2gray(o);
    switch (o->gch.tt)
    {
    case LUA_TSTRING:
    {
        return;
    }
    case LUA_TUSERDATA:
    {
        Table* mt = gco2u(o)->metatable;
        gray2black(o); /* udata are never gray */
        if (mt)
            markobject(g, mt);
        return;
    }
    case LUA_TUPVAL:
    {
        UpVal* uv = gco2uv(o);
        markvalue(g, uv->v);
        if (uv->v == &uv->u.value) /* closed? */
            gray2black(o);         /* open upvalues are never black */
        return;
    }
    case LUA_TFUNCTION:
    {
        gco2cl(o)->gclist = g->gray;
        g->gray = o;
        break;
    }
    case LUA_TTABLE:
    {
        gco2h(o)->gclist = g->gray;
        g->gray = o;
        break;
    }
    case LUA_TTHREAD:
    {
        gco2th(o)->gclist = g->gray;
        g->gray = o;
        break;
    }
    case LUA_TPROTO:
    {
        gco2p(o)->gclist = g->gray;
        g->gray = o;
        break;
    }
    default:
        LUAU_ASSERT(0);
    }
}

static const char* gettablemode(global_State* g, Table* h)
{
    const TValue* mode = gfasttm(g, h->metatable, TM_MODE);

    if (mode && ttisstring(mode))
        return svalue(mode);

    return NULL;
}

static int traversetable(global_State* g, Table* h)
{
    int i;
    int weakkey = 0;
    int weakvalue = 0;
    if (h->metatable)
        markobject(g, cast_to(Table*, h->metatable));

    /* is there a weak mode? */
    if (const char* modev = gettablemode(g, h))
    {
        weakkey = (strchr(modev, 'k') != NULL);
        weakvalue = (strchr(modev, 'v') != NULL);
        if (weakkey || weakvalue)
        {                         /* is really weak? */
            h->gclist = g->weak;  /* must be cleared after GC, ... */
            g->weak = obj2gco(h); /* ... so put in the appropriate list */
        }
    }

    if (weakkey && weakvalue)
        return 1;
    if (!weakvalue)
    {
        i = h->sizearray;
        while (i--)
            markvalue(g, &h->array[i]);
    }
    i = sizenode(h);
    while (i--)
    {
        LuaNode* n = gnode(h, i);
        LUAU_ASSERT(ttype(gkey(n)) != LUA_TDEADKEY || ttisnil(gval(n)));
        if (ttisnil(gval(n)))
            removeentry(n); /* remove empty entries */
        else
        {
            LUAU_ASSERT(!ttisnil(gkey(n)));
            if (!weakkey)
                markvalue(g, gkey(n));
            if (!weakvalue)
                markvalue(g, gval(n));
        }
    }
    return weakkey || weakvalue;
}

/*
** All marks are conditional because a GC may happen while the
** prototype is still being created
*/
static void traverseproto(global_State* g, Proto* f)
{
    int i;
    if (f->source)
        stringmark(f->source);
    if (f->debugname)
        stringmark(f->debugname);
    for (i = 0; i < f->sizek; i++) /* mark literals */
        markvalue(g, &f->k[i]);
    for (i = 0; i < f->sizeupvalues; i++)
    { /* mark upvalue names */
        if (f->upvalues[i])
            stringmark(f->upvalues[i]);
    }
    for (i = 0; i < f->sizep; i++)
    { /* mark nested protos */
        if (f->p[i])
            markobject(g, f->p[i]);
    }
    for (i = 0; i < f->sizelocvars; i++)
    { /* mark local-variable names */
        if (f->locvars[i].varname)
            stringmark(f->locvars[i].varname);
    }
}

static void traverseclosure(global_State* g, Closure* cl)
{
    markobject(g, cl->env);
    if (cl->isC)
    {
        int i;
        for (i = 0; i < cl->nupvalues; i++) /* mark its upvalues */
            markvalue(g, &cl->c.upvals[i]);
    }
    else
    {
        int i;
        LUAU_ASSERT(cl->nupvalues == cl->l.p->nups);
        markobject(g, cast_to(Proto*, cl->l.p));
        for (i = 0; i < cl->nupvalues; i++) /* mark its upvalues */
            markvalue(g, &cl->l.uprefs[i]);
    }
}

static void traversestack(global_State* g, lua_State* l, bool clearstack)
{
    markobject(g, l->gt);
    if (l->namecall)
        stringmark(l->namecall);
    for (StkId o = l->stack; o < l->top; o++)
        markvalue(g, o);
    /* final traversal? */
    if (g->gcstate == GCSatomic || clearstack)
    {
        StkId stack_end = l->stack + l->stacksize;
        for (StkId o = l->top; o < stack_end; o++) /* clear not-marked stack slice */
            setnilvalue(o);
    }
}

/*
** traverse one gray object, turning it to black.
** Returns `quantity' traversed.
*/
static size_t propagatemark(global_State* g)
{
    GCObject* o = g->gray;
    LUAU_ASSERT(isgray(o));
    gray2black(o);
    switch (o->gch.tt)
    {
    case LUA_TTABLE:
    {
        Table* h = gco2h(o);
        g->gray = h->gclist;
        if (traversetable(g, h)) /* table is weak? */
            black2gray(o);       /* keep it gray */
        return sizeof(Table) + sizeof(TValue) * h->sizearray + sizeof(LuaNode) * sizenode(h);
    }
    case LUA_TFUNCTION:
    {
        Closure* cl = gco2cl(o);
        g->gray = cl->gclist;
        traverseclosure(g, cl);
        return cl->isC ? sizeCclosure(cl->nupvalues) : sizeLclosure(cl->nupvalues);
    }
    case LUA_TTHREAD:
    {
        lua_State* th = gco2th(o);
        g->gray = th->gclist;

        LUAU_ASSERT(!luaC_threadsleeping(th));

        // threads that are executing and the main thread are not deactivated
        bool active = luaC_threadactive(th) || th == th->global->mainthread;

        if (!active && g->gcstate == GCSpropagate)
        {
            traversestack(g, th, /* clearstack= */ true);

            l_setbit(th->stackstate, THREAD_SLEEPINGBIT);
        }
        else
        {
            th->gclist = g->grayagain;
            g->grayagain = o;

            black2gray(o);

            traversestack(g, th, /* clearstack= */ false);
        }

        return sizeof(lua_State) + sizeof(TValue) * th->stacksize + sizeof(CallInfo) * th->size_ci;
    }
    case LUA_TPROTO:
    {
        Proto* p = gco2p(o);
        g->gray = p->gclist;
        traverseproto(g, p);
        return sizeof(Proto) + sizeof(Instruction) * p->sizecode + sizeof(Proto*) * p->sizep + sizeof(TValue) * p->sizek + p->sizelineinfo +
               sizeof(LocVar) * p->sizelocvars + sizeof(TString*) * p->sizeupvalues;
    }
    default:
        LUAU_ASSERT(0);
        return 0;
    }
}

static size_t propagateall(global_State* g)
{
    size_t work = 0;
    while (g->gray)
    {
        work += propagatemark(g);
    }
    return work;
}

/*
** The next function tells whether a key or value can be cleared from
** a weak table. Non-collectable objects are never removed from weak
** tables. Strings behave as `values', so are never removed too. for
** other objects: if really collected, cannot keep them.
*/
static int isobjcleared(GCObject* o)
{
    if (o->gch.tt == LUA_TSTRING)
    {
        stringmark(&o->ts); /* strings are `values', so are never weak */
        return 0;
    }

    return iswhite(o);
}

#define iscleared(o) (iscollectable(o) && isobjcleared(gcvalue(o)))

/*
** clear collected entries from weaktables
*/
static size_t cleartable(lua_State* L, GCObject* l)
{
    size_t work = 0;
    while (l)
    {
        Table* h = gco2h(l);
        work += sizeof(Table) + sizeof(TValue) * h->sizearray + sizeof(LuaNode) * sizenode(h);

        int i = h->sizearray;
        while (i--)
        {
            TValue* o = &h->array[i];
            if (iscleared(o))   /* value was collected? */
                setnilvalue(o); /* remove value */
        }
        i = sizenode(h);
        int activevalues = 0;
        while (i--)
        {
            LuaNode* n = gnode(h, i);

            // non-empty entry?
            if (!ttisnil(gval(n)))
            {
                // can we clear key or value?
                if (iscleared(gkey(n)) || iscleared(gval(n)))
                {
                    setnilvalue(gval(n)); /* remove value ... */
                    removeentry(n);       /* remove entry from table */
                }
                else
                {
                    activevalues++;
                }
            }
        }

        if (const char* modev = gettablemode(L->global, h))
        {
            // are we allowed to shrink this weak table?
            if (strchr(modev, 's'))
            {
                // shrink at 37.5% occupancy
                if (activevalues < sizenode(h) * 3 / 8)
                    luaH_resizehash(L, h, activevalues);
            }
        }

        l = h->gclist;
    }
    return work;
}

static void shrinkstack(lua_State* L)
{
    /* compute used stack - note that we can't use th->top if we're in the middle of vararg call */
    StkId lim = L->top;
    for (CallInfo* ci = L->base_ci; ci <= L->ci; ci++)
    {
        LUAU_ASSERT(ci->top <= L->stack_last);
        if (lim < ci->top)
            lim = ci->top;
    }

    /* shrink stack and callinfo arrays if we aren't using most of the space */
    int ci_used = cast_int(L->ci - L->base_ci); /* number of `ci' in use */
    int s_used = cast_int(lim - L->stack);      /* part of stack in use */
    if (L->size_ci > LUAI_MAXCALLS)             /* handling overflow? */
        return;                                 /* do not touch the stacks */
    if (3 * ci_used < L->size_ci && 2 * BASIC_CI_SIZE < L->size_ci)
        luaD_reallocCI(L, L->size_ci / 2); /* still big enough... */
    condhardstacktests(luaD_reallocCI(L, ci_used + 1));
    if (3 * s_used < L->stacksize && 2 * (BASIC_STACK_SIZE + EXTRA_STACK) < L->stacksize)
        luaD_reallocstack(L, L->stacksize / 2); /* still big enough... */
    condhardstacktests(luaD_reallocstack(L, s_used));
}

static void freeobj(lua_State* L, GCObject* o, lua_Page* page)
{
    switch (o->gch.tt)
    {
    case LUA_TPROTO:
        luaF_freeproto(L, gco2p(o), page);
        break;
    case LUA_TFUNCTION:
        luaF_freeclosure(L, gco2cl(o), page);
        break;
    case LUA_TUPVAL:
        luaF_freeupval(L, gco2uv(o), page);
        break;
    case LUA_TTABLE:
        luaH_free(L, gco2h(o), page);
        break;
    case LUA_TTHREAD:
        LUAU_ASSERT(gco2th(o) != L && gco2th(o) != L->global->mainthread);
        luaE_freethread(L, gco2th(o), page);
        break;
    case LUA_TSTRING:
        luaS_free(L, gco2ts(o), page);
        break;
    case LUA_TUSERDATA:
        luaU_freeudata(L, gco2u(o), page);
        break;
    default:
        LUAU_ASSERT(0);
    }
}

static void shrinkbuffers(lua_State* L)
{
    global_State* g = L->global;
    /* check size of string hash */
    if (g->strt.nuse < cast_to(uint32_t, g->strt.size / 4) && g->strt.size > LUA_MINSTRTABSIZE * 2)
        luaS_resize(L, g->strt.size / 2); /* table is too big */
}

static void shrinkbuffersfull(lua_State* L)
{
    global_State* g = L->global;
    /* check size of string hash */
    int hashsize = g->strt.size;
    while (g->strt.nuse < cast_to(uint32_t, hashsize / 4) && hashsize > LUA_MINSTRTABSIZE * 2)
        hashsize /= 2;
    if (hashsize != g->strt.size)
        luaS_resize(L, hashsize); /* table is too big */
}

static bool deletegco(void* context, lua_Page* page, GCObject* gco)
{
    // we are in the process of deleting everything
    // threads with open upvalues will attempt to close them all on removal
    // but those upvalues might point to stack values that were already deleted
    if (gco->gch.tt == LUA_TTHREAD)
    {
        lua_State* th = gco2th(gco);

        while (UpVal* uv = th->openupval)
        {
            luaF_unlinkupval(uv);
            // close the upvalue without copying the dead data so that luaF_freeupval will not unlink again
            uv->v = &uv->u.value;
        }
    }

    lua_State* L = (lua_State*)context;
    freeobj(L, gco, page);
    return true;
}

void luaC_freeall(lua_State* L)
{
    global_State* g = L->global;

    LUAU_ASSERT(L == g->mainthread);

    luaM_visitgco(L, L, deletegco);

    for (int i = 0; i < g->strt.size; i++) /* free all string lists */
        LUAU_ASSERT(g->strt.hash[i] == NULL);

    LUAU_ASSERT(L->global->strt.nuse == 0);
    LUAU_ASSERT(g->strbufgc == NULL);
}

static void markmt(global_State* g)
{
    int i;
    for (i = 0; i < LUA_T_COUNT; i++)
        if (g->mt[i])
            markobject(g, g->mt[i]);
}

/* mark root set */
static void markroot(lua_State* L)
{
    global_State* g = L->global;
    g->gray = NULL;
    g->grayagain = NULL;
    g->weak = NULL;
    markobject(g, g->mainthread);
    /* make global table be traversed before main stack */
    markobject(g, g->mainthread->gt);
    markvalue(g, registry(L));
    markmt(g);
    g->gcstate = GCSpropagate;
}

static size_t remarkupvals(global_State* g)
{
    size_t work = 0;
    for (UpVal* uv = g->uvhead.u.l.next; uv != &g->uvhead; uv = uv->u.l.next)
    {
        work += sizeof(UpVal);
        LUAU_ASSERT(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
        if (isgray(obj2gco(uv)))
            markvalue(g, uv->v);
    }
    return work;
}

static size_t atomic(lua_State* L)
{
    global_State* g = L->global;
    LUAU_ASSERT(g->gcstate == GCSatomic);

    size_t work = 0;

#ifdef LUAI_GCMETRICS
    double currts = lua_clock();
#endif

    /* remark occasional upvalues of (maybe) dead threads */
    work += remarkupvals(g);
    /* traverse objects caught by write barrier and by 'remarkupvals' */
    work += propagateall(g);

#ifdef LUAI_GCMETRICS
    g->gcmetrics.currcycle.atomictimeupval += recordGcDeltaTime(currts);
#endif

    /* remark weak tables */
    g->gray = g->weak;
    g->weak = NULL;
    LUAU_ASSERT(!iswhite(obj2gco(g->mainthread)));
    markobject(g, L); /* mark running thread */
    markmt(g);        /* mark basic metatables (again) */
    work += propagateall(g);

#ifdef LUAI_GCMETRICS
    g->gcmetrics.currcycle.atomictimeweak += recordGcDeltaTime(currts);
#endif

    /* remark gray again */
    g->gray = g->grayagain;
    g->grayagain = NULL;
    work += propagateall(g);

#ifdef LUAI_GCMETRICS
    g->gcmetrics.currcycle.atomictimegray += recordGcDeltaTime(currts);
#endif

    /* remove collected objects from weak tables */
    work += cleartable(L, g->weak);
    g->weak = NULL;

#ifdef LUAI_GCMETRICS
    g->gcmetrics.currcycle.atomictimeclear += recordGcDeltaTime(currts);
#endif

    /* flip current white */
    g->currentwhite = cast_byte(otherwhite(g));
    g->sweepgcopage = g->allgcopages;
    g->gcstate = GCSsweep;

    return work;
}

static bool sweepgco(lua_State* L, lua_Page* page, GCObject* gco)
{
    global_State* g = L->global;

    int deadmask = otherwhite(g);
    LUAU_ASSERT(testbit(deadmask, FIXEDBIT)); // make sure we never sweep fixed objects

    int alive = (gco->gch.marked ^ WHITEBITS) & deadmask;

    if (gco->gch.tt == LUA_TTHREAD)
    {
        lua_State* th = gco2th(gco);

        if (alive)
        {
            resetbit(th->stackstate, THREAD_SLEEPINGBIT);
            shrinkstack(th);
        }
    }

    if (alive)
    {
        LUAU_ASSERT(!isdead(g, gco));
        makewhite(g, gco); // make it white (for next cycle)
        return false;
    }

    LUAU_ASSERT(isdead(g, gco));
    freeobj(L, gco, page);
    return true;
}

// a version of generic luaM_visitpage specialized for the main sweep stage
static int sweepgcopage(lua_State* L, lua_Page* page)
{
    char* start;
    char* end;
    int busyBlocks;
    int blockSize;
    luaM_getpagewalkinfo(page, &start, &end, &busyBlocks, &blockSize);

    for (char* pos = start; pos != end; pos += blockSize)
    {
        GCObject* gco = (GCObject*)pos;

        // skip memory blocks that are already freed
        if (gco->gch.tt == LUA_TNIL)
            continue;

        // when true is returned it means that the element was deleted
        if (sweepgco(L, page, gco))
        {
            LUAU_ASSERT(busyBlocks > 0);

            // if the last block was removed, page would be removed as well
            if (--busyBlocks == 0)
                return int(pos - start) / blockSize + 1;
        }
    }

    return int(end - start) / blockSize;
}

static size_t gcstep(lua_State* L, size_t limit)
{
    size_t cost = 0;
    global_State* g = L->global;
    switch (g->gcstate)
    {
    case GCSpause:
    {
        markroot(L); /* start a new collection */
        LUAU_ASSERT(g->gcstate == GCSpropagate);
        break;
    }
    case GCSpropagate:
    {
        while (g->gray && cost < limit)
        {
            cost += propagatemark(g);
        }

        if (!g->gray)
        {
#ifdef LUAI_GCMETRICS
            g->gcmetrics.currcycle.propagatework = g->gcmetrics.currcycle.explicitwork + g->gcmetrics.currcycle.assistwork;
#endif

            // perform one iteration over 'gray again' list
            g->gray = g->grayagain;
            g->grayagain = NULL;

            g->gcstate = GCSpropagateagain;
        }
        break;
    }
    case GCSpropagateagain:
    {
        while (g->gray && cost < limit)
        {
            cost += propagatemark(g);
        }

        if (!g->gray) /* no more `gray' objects */
        {
#ifdef LUAI_GCMETRICS
            g->gcmetrics.currcycle.propagateagainwork =
                g->gcmetrics.currcycle.explicitwork + g->gcmetrics.currcycle.assistwork - g->gcmetrics.currcycle.propagatework;
#endif

            g->gcstate = GCSatomic;
        }
        break;
    }
    case GCSatomic:
    {
#ifdef LUAI_GCMETRICS
        g->gcmetrics.currcycle.atomicstarttimestamp = lua_clock();
        g->gcmetrics.currcycle.atomicstarttotalsizebytes = g->totalbytes;
#endif

        g->gcstats.atomicstarttimestamp = lua_clock();
        g->gcstats.atomicstarttotalsizebytes = g->totalbytes;

        cost = atomic(L); /* finish mark phase */

        LUAU_ASSERT(g->gcstate == GCSsweep);
        break;
    }
    case GCSsweep:
    {
        while (g->sweepgcopage && cost < limit)
        {
            lua_Page* next = luaM_getnextgcopage(g->sweepgcopage); // page sweep might destroy the page

            int steps = sweepgcopage(L, g->sweepgcopage);

            g->sweepgcopage = next;
            cost += steps * GC_SWEEPPAGESTEPCOST;
        }

        // nothing more to sweep?
        if (g->sweepgcopage == NULL)
        {
            // don't forget to visit main thread
            sweepgco(L, NULL, obj2gco(g->mainthread));

            shrinkbuffers(L);
            g->gcstate = GCSpause; /* end collection */
        }
        break;
    }
    default:
        LUAU_ASSERT(!"Unexpected GC state");
    }
    return cost;
}

static int64_t getheaptriggererroroffset(global_State* g)
{
    // adjust for error using Proportional-Integral controller
    // https://en.wikipedia.org/wiki/PID_controller
    int32_t errorKb = int32_t((g->gcstats.atomicstarttotalsizebytes - g->gcstats.heapgoalsizebytes) / 1024);

    // we use sliding window for the error integral to avoid error sum 'windup' when the desired target cannot be reached
    const size_t triggertermcount = sizeof(g->gcstats.triggerterms) / sizeof(g->gcstats.triggerterms[0]);

    int32_t* slot = &g->gcstats.triggerterms[g->gcstats.triggertermpos % triggertermcount];
    int32_t prev = *slot;
    *slot = errorKb;
    g->gcstats.triggerintegral += errorKb - prev;
    g->gcstats.triggertermpos++;

    // controller tuning
    // https://en.wikipedia.org/wiki/Ziegler%E2%80%93Nichols_method
    const double Ku = 0.9; // ultimate gain (measured)
    const double Tu = 2.5; // oscillation period (measured)

    const double Kp = 0.45 * Ku; // proportional gain
    const double Ti = 0.8 * Tu;
    const double Ki = 0.54 * Ku / Ti; // integral gain

    double proportionalTerm = Kp * errorKb;
    double integralTerm = Ki * g->gcstats.triggerintegral;

    double totalTerm = proportionalTerm + integralTerm;

    return int64_t(totalTerm * 1024);
}

static size_t getheaptrigger(global_State* g, size_t heapgoal)
{
    // adjust threshold based on a guess of how many bytes will be allocated between the cycle start and sweep phase
    // our goal is to begin the sweep when used memory has reached the heap goal
    const double durationthreshold = 1e-3;
    double allocationduration = g->gcstats.atomicstarttimestamp - g->gcstats.endtimestamp;

    // avoid measuring intervals smaller than 1ms
    if (allocationduration < durationthreshold)
        return heapgoal;

    double allocationrate = (g->gcstats.atomicstarttotalsizebytes - g->gcstats.endtotalsizebytes) / allocationduration;
    double markduration = g->gcstats.atomicstarttimestamp - g->gcstats.starttimestamp;

    int64_t expectedgrowth = int64_t(markduration * allocationrate);
    int64_t offset = getheaptriggererroroffset(g);
    int64_t heaptrigger = heapgoal - (expectedgrowth + offset);

    // clamp the trigger between memory use at the end of the cycle and the heap goal
    return heaptrigger < int64_t(g->totalbytes) ? g->totalbytes : (heaptrigger > int64_t(heapgoal) ? heapgoal : size_t(heaptrigger));
}

size_t luaC_step(lua_State* L, bool assist)
{
    global_State* g = L->global;

    int lim = g->gcstepsize * g->gcstepmul / 100; /* how much to work */
    LUAU_ASSERT(g->totalbytes >= g->GCthreshold);
    size_t debt = g->totalbytes - g->GCthreshold;

    GC_INTERRUPT(0);

    // at the start of the new cycle
    if (g->gcstate == GCSpause)
        g->gcstats.starttimestamp = lua_clock();

#ifdef LUAI_GCMETRICS
    if (g->gcstate == GCSpause)
        startGcCycleMetrics(g);

    double lasttimestamp = lua_clock();
#endif

    int lastgcstate = g->gcstate;

    size_t work = gcstep(L, lim);

#ifdef LUAI_GCMETRICS
    recordGcStateStep(g, lastgcstate, lua_clock() - lasttimestamp, assist, work);
#endif

    size_t actualstepsize = work * 100 / g->gcstepmul;

    // at the end of the last cycle
    if (g->gcstate == GCSpause)
    {
        // at the end of a collection cycle, set goal based on gcgoal setting
        size_t heapgoal = (g->totalbytes / 100) * g->gcgoal;
        size_t heaptrigger = getheaptrigger(g, heapgoal);

        g->GCthreshold = heaptrigger;

        g->gcstats.heapgoalsizebytes = heapgoal;
        g->gcstats.endtimestamp = lua_clock();
        g->gcstats.endtotalsizebytes = g->totalbytes;

#ifdef LUAI_GCMETRICS
        finishGcCycleMetrics(g);
#endif
    }
    else
    {
        g->GCthreshold = g->totalbytes + actualstepsize;

        // compensate if GC is "behind schedule" (has some debt to pay)
        if (g->GCthreshold >= debt)
            g->GCthreshold -= debt;
    }

    GC_INTERRUPT(lastgcstate);

    return actualstepsize;
}

void luaC_fullgc(lua_State* L)
{
    global_State* g = L->global;

#ifdef LUAI_GCMETRICS
    if (g->gcstate == GCSpause)
        startGcCycleMetrics(g);
#endif

    if (g->gcstate <= GCSatomic)
    {
        /* reset sweep marks to sweep all elements (returning them to white) */
        g->sweepgcopage = g->allgcopages;
        /* reset other collector lists */
        g->gray = NULL;
        g->grayagain = NULL;
        g->weak = NULL;
        g->gcstate = GCSsweep;
    }
    LUAU_ASSERT(g->gcstate == GCSsweep);
    /* finish any pending sweep phase */
    while (g->gcstate != GCSpause)
    {
        LUAU_ASSERT(g->gcstate == GCSsweep);
        gcstep(L, SIZE_MAX);
    }

#ifdef LUAI_GCMETRICS
    finishGcCycleMetrics(g);
    startGcCycleMetrics(g);
#endif

    /* run a full collection cycle */
    markroot(L);
    while (g->gcstate != GCSpause)
    {
        gcstep(L, SIZE_MAX);
    }
    /* reclaim as much buffer memory as possible (shrinkbuffers() called during sweep is incremental) */
    shrinkbuffersfull(L);

    size_t heapgoalsizebytes = (g->totalbytes / 100) * g->gcgoal;

    // trigger cannot be correctly adjusted after a forced full GC.
    // we will try to place it so that we can reach the goal based on
    // the rate at which we run the GC relative to allocation rate
    // and on amount of bytes we need to traverse in propagation stage.
    // goal and stepmul are defined in percents
    g->GCthreshold = g->totalbytes * (g->gcgoal * g->gcstepmul / 100 - 100) / g->gcstepmul;

    // but it might be impossible to satisfy that directly
    if (g->GCthreshold < g->totalbytes)
        g->GCthreshold = g->totalbytes;

    g->gcstats.heapgoalsizebytes = heapgoalsizebytes;

#ifdef LUAI_GCMETRICS
    finishGcCycleMetrics(g);
#endif
}

void luaC_barrierupval(lua_State* L, GCObject* v)
{
    global_State* g = L->global;
    LUAU_ASSERT(iswhite(v) && !isdead(g, v));

    if (keepinvariant(g))
        reallymarkobject(g, v);
}

void luaC_barrierf(lua_State* L, GCObject* o, GCObject* v)
{
    global_State* g = L->global;
    LUAU_ASSERT(isblack(o) && iswhite(v) && !isdead(g, v) && !isdead(g, o));
    LUAU_ASSERT(g->gcstate != GCSpause);
    /* must keep invariant? */
    if (keepinvariant(g))
        reallymarkobject(g, v); /* restore invariant */
    else                        /* don't mind */
        makewhite(g, o);        /* mark as white just to avoid other barriers */
}

void luaC_barriertable(lua_State* L, Table* t, GCObject* v)
{
    global_State* g = L->global;
    GCObject* o = obj2gco(t);

    // in the second propagation stage, table assignment barrier works as a forward barrier
    if (g->gcstate == GCSpropagateagain)
    {
        LUAU_ASSERT(isblack(o) && iswhite(v) && !isdead(g, v) && !isdead(g, o));
        reallymarkobject(g, v);
        return;
    }

    LUAU_ASSERT(isblack(o) && !isdead(g, o));
    LUAU_ASSERT(g->gcstate != GCSpause);
    black2gray(o); /* make table gray (again) */
    t->gclist = g->grayagain;
    g->grayagain = o;
}

void luaC_barrierback(lua_State* L, Table* t)
{
    global_State* g = L->global;
    GCObject* o = obj2gco(t);
    LUAU_ASSERT(isblack(o) && !isdead(g, o));
    LUAU_ASSERT(g->gcstate != GCSpause);
    black2gray(o); /* make table gray (again) */
    t->gclist = g->grayagain;
    g->grayagain = o;
}

void luaC_initobj(lua_State* L, GCObject* o, uint8_t tt)
{
    global_State* g = L->global;
    o->gch.marked = luaC_white(g);
    o->gch.tt = tt;
    o->gch.memcat = L->activememcat;
}

void luaC_initupval(lua_State* L, UpVal* uv)
{
    global_State* g = L->global;
    GCObject* o = obj2gco(uv);

    if (isgray(o))
    {
        if (keepinvariant(g))
        {
            gray2black(o); /* closed upvalues need barrier */
            luaC_barrier(L, uv, uv->v);
        }
        else
        { /* sweep phase: sweep it (turning it into white) */
            makewhite(g, o);
            LUAU_ASSERT(g->gcstate != GCSpause);
        }
    }
}

// measure the allocation rate in bytes/sec
// returns -1 if allocation rate cannot be measured
int64_t luaC_allocationrate(lua_State* L)
{
    global_State* g = L->global;
    const double durationthreshold = 1e-3; // avoid measuring intervals smaller than 1ms

    if (g->gcstate <= GCSatomic)
    {
        double duration = lua_clock() - g->gcstats.endtimestamp;

        if (duration < durationthreshold)
            return -1;

        return int64_t((g->totalbytes - g->gcstats.endtotalsizebytes) / duration);
    }

    // totalbytes is unstable during the sweep, use the rate measured at the end of mark phase
    double duration = g->gcstats.atomicstarttimestamp - g->gcstats.endtimestamp;

    if (duration < durationthreshold)
        return -1;

    return int64_t((g->gcstats.atomicstarttotalsizebytes - g->gcstats.endtotalsizebytes) / duration);
}

void luaC_wakethread(lua_State* L)
{
    if (!luaC_threadsleeping(L))
        return;

    global_State* g = L->global;

    resetbit(L->stackstate, THREAD_SLEEPINGBIT);

    if (keepinvariant(g))
    {
        GCObject* o = obj2gco(L);

        L->gclist = g->grayagain;
        g->grayagain = o;

        black2gray(o);
    }
}

const char* luaC_statename(int state)
{
    switch (state)
    {
    case GCSpause:
        return "pause";

    case GCSpropagate:
        return "mark";

    case GCSpropagateagain:
        return "remark";

    case GCSatomic:
        return "atomic";

    case GCSsweep:
        return "sweep";

    default:
        return NULL;
    }
}
