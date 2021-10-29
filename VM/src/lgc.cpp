// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lgc.h"

#include "lobject.h"
#include "lstate.h"
#include "ltable.h"
#include "lfunc.h"
#include "lstring.h"
#include "ldo.h"

#include <string.h>
#include <stdio.h>

LUAU_FASTFLAGVARIABLE(LuauRescanGrayAgain, false)
LUAU_FASTFLAGVARIABLE(LuauRescanGrayAgainForwardBarrier, false)
LUAU_FASTFLAGVARIABLE(LuauGcFullSkipInactiveThreads, false)
LUAU_FASTFLAGVARIABLE(LuauShrinkWeakTables, false)
LUAU_FASTFLAG(LuauArrayBoundary)

#define GC_SWEEPMAX 40
#define GC_SWEEPCOST 10

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

static void recordGcStateTime(global_State* g, int startgcstate, double seconds, bool assist)
{
    switch (startgcstate)
    {
    case GCSpause:
        // record root mark time if we have switched to next state
        if (g->gcstate == GCSpropagate)
            g->gcstats.currcycle.marktime += seconds;
        break;
    case GCSpropagate:
    case GCSpropagateagain:
        g->gcstats.currcycle.marktime += seconds;

        // atomic step had to be performed during the switch and it's tracked separately
        if (g->gcstate == GCSsweepstring)
            g->gcstats.currcycle.marktime -= g->gcstats.currcycle.atomictime;
        break;
    case GCSsweepstring:
    case GCSsweep:
        g->gcstats.currcycle.sweeptime += seconds;
        break;
    }

    if (assist)
        g->gcstats.stepassisttimeacc += seconds;
    else
        g->gcstats.stepexplicittimeacc += seconds;
}

static void startGcCycleStats(global_State* g)
{
    g->gcstats.currcycle.starttimestamp = lua_clock();
    g->gcstats.currcycle.waittime = g->gcstats.currcycle.starttimestamp - g->gcstats.lastcycle.endtimestamp;
}

static void finishGcCycleStats(global_State* g)
{
    g->gcstats.currcycle.endtimestamp = lua_clock();
    g->gcstats.currcycle.endtotalsizebytes = g->totalbytes;

    g->gcstats.completedcycles++;
    g->gcstats.lastcycle = g->gcstats.currcycle;
    g->gcstats.currcycle = GCCycleStats();

    g->gcstats.cyclestatsacc.markitems += g->gcstats.lastcycle.markitems;
    g->gcstats.cyclestatsacc.marktime += g->gcstats.lastcycle.marktime;
    g->gcstats.cyclestatsacc.atomictime += g->gcstats.lastcycle.atomictime;
    g->gcstats.cyclestatsacc.sweepitems += g->gcstats.lastcycle.sweepitems;
    g->gcstats.cyclestatsacc.sweeptime += g->gcstats.lastcycle.sweeptime;
}

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

    if (FFlag::LuauShrinkWeakTables)
    {
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
    }
    else
    {
        const TValue* mode = gfasttm(g, h->metatable, TM_MODE);
        if (mode && ttisstring(mode))
        { /* is there a weak mode? */
            const char* modev = svalue(mode);
            weakkey = (strchr(modev, 'k') != NULL);
            weakvalue = (strchr(modev, 'v') != NULL);
            if (weakkey || weakvalue)
            {                         /* is really weak? */
                h->gclist = g->weak;  /* must be cleared after GC, ... */
                g->weak = obj2gco(h); /* ... so put in the appropriate list */
            }
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
    markvalue(g, gt(l));
    if (l->namecall)
        stringmark(l->namecall);
    for (StkId o = l->stack; o < l->top; o++)
        markvalue(g, o);
    /* final traversal? */
    if (g->gcstate == GCSatomic || (FFlag::LuauGcFullSkipInactiveThreads && clearstack))
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

        if (FFlag::LuauGcFullSkipInactiveThreads)
        {
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

static void propagateall(global_State* g)
{
    while (g->gray)
    {
        propagatemark(g);
    }
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
static void cleartable(lua_State* L, GCObject* l)
{
    while (l)
    {
        Table* h = gco2h(l);
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

            if (FFlag::LuauShrinkWeakTables)
            {
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
            else
            {
                if (!ttisnil(gval(n)) && /* non-empty entry? */
                    (iscleared(gkey(n)) || iscleared(gval(n))))
                {
                    setnilvalue(gval(n)); /* remove value ... */
                    removeentry(n);       /* remove entry from table */
                }
            }
        }

        if (FFlag::LuauShrinkWeakTables)
        {
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
        }

        l = h->gclist;
    }
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

static void freeobj(lua_State* L, GCObject* o)
{
    switch (o->gch.tt)
    {
    case LUA_TPROTO:
        luaF_freeproto(L, gco2p(o));
        break;
    case LUA_TFUNCTION:
        luaF_freeclosure(L, gco2cl(o));
        break;
    case LUA_TUPVAL:
        luaF_freeupval(L, gco2uv(o));
        break;
    case LUA_TTABLE:
        luaH_free(L, gco2h(o));
        break;
    case LUA_TTHREAD:
        LUAU_ASSERT(gco2th(o) != L && gco2th(o) != L->global->mainthread);
        luaE_freethread(L, gco2th(o));
        break;
    case LUA_TSTRING:
        luaS_free(L, gco2ts(o));
        break;
    case LUA_TUSERDATA:
        luaS_freeudata(L, gco2u(o));
        break;
    default:
        LUAU_ASSERT(0);
    }
}

#define sweepwholelist(L, p, tc) sweeplist(L, p, SIZE_MAX, tc)

static GCObject** sweeplist(lua_State* L, GCObject** p, size_t count, size_t* traversedcount)
{
    GCObject* curr;
    global_State* g = L->global;
    int deadmask = otherwhite(g);
    size_t startcount = count;
    LUAU_ASSERT(testbit(deadmask, FIXEDBIT)); /* make sure we never sweep fixed objects */
    while ((curr = *p) != NULL && count-- > 0)
    {
        int alive = (curr->gch.marked ^ WHITEBITS) & deadmask;
        if (curr->gch.tt == LUA_TTHREAD)
        {
            sweepwholelist(L, &gco2th(curr)->openupval, traversedcount); /* sweep open upvalues */

            lua_State* th = gco2th(curr);

            if (alive)
            {
                resetbit(th->stackstate, THREAD_SLEEPINGBIT);
                shrinkstack(th);
            }
        }
        if (alive)
        { /* not dead? */
            LUAU_ASSERT(!isdead(g, curr));
            makewhite(g, curr); /* make it white (for next cycle) */
            p = &curr->gch.next;
        }
        else
        { /* must erase `curr' */
            LUAU_ASSERT(isdead(g, curr));
            *p = curr->gch.next;
            if (curr == g->rootgc)          /* is the first element of the list? */
                g->rootgc = curr->gch.next; /* adjust first */
            freeobj(L, curr);
        }
    }

    // if we didn't reach the end of the list it means that we've stopped because the count dropped below zero
    if (traversedcount)
        *traversedcount += startcount - (curr ? count + 1 : count);

    return p;
}

static void deletelist(lua_State* L, GCObject** p, GCObject* limit)
{
    GCObject* curr;
    while ((curr = *p) != limit)
    {
        if (curr->gch.tt == LUA_TTHREAD) /* delete open upvalues of each thread */
            deletelist(L, &gco2th(curr)->openupval, NULL);

        *p = curr->gch.next;
        freeobj(L, curr);
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

void luaC_freeall(lua_State* L)
{
    global_State* g = L->global;

    LUAU_ASSERT(L == g->mainthread);
    LUAU_ASSERT(L->next == NULL); /* mainthread is at the end of rootgc list */

    deletelist(L, &g->rootgc, obj2gco(L));

    for (int i = 0; i < g->strt.size; i++) /* free all string lists */
        deletelist(L, &g->strt.hash[i], NULL);

    LUAU_ASSERT(L->global->strt.nuse == 0);
    deletelist(L, &g->strbufgc, NULL);
    // unfortunately, when string objects are freed, the string table use count is decremented
    // even when the string is a buffer that wasn't placed into the table
    L->global->strt.nuse = 0;
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
    markvalue(g, gt(g->mainthread));
    markvalue(g, registry(L));
    markmt(g);
    g->gcstate = GCSpropagate;
}

static void remarkupvals(global_State* g)
{
    UpVal* uv;
    for (uv = g->uvhead.u.l.next; uv != &g->uvhead; uv = uv->u.l.next)
    {
        LUAU_ASSERT(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
        if (isgray(obj2gco(uv)))
            markvalue(g, uv->v);
    }
}

static void atomic(lua_State* L)
{
    global_State* g = L->global;
    g->gcstate = GCSatomic;
    /* remark occasional upvalues of (maybe) dead threads */
    remarkupvals(g);
    /* traverse objects caught by write barrier and by 'remarkupvals' */
    propagateall(g);
    /* remark weak tables */
    g->gray = g->weak;
    g->weak = NULL;
    LUAU_ASSERT(!iswhite(obj2gco(g->mainthread)));
    markobject(g, L); /* mark running thread */
    markmt(g);        /* mark basic metatables (again) */
    propagateall(g);
    /* remark gray again */
    g->gray = g->grayagain;
    g->grayagain = NULL;
    propagateall(g);
    cleartable(L, g->weak); /* remove collected objects from weak tables */
    g->weak = NULL;
    /* flip current white */
    g->currentwhite = cast_byte(otherwhite(g));
    g->sweepstrgc = 0;
    g->sweepgc = &g->rootgc;
    g->gcstate = GCSsweepstring;

    GC_INTERRUPT(GCSatomic);
}

static size_t singlestep(lua_State* L)
{
    size_t cost = 0;
    global_State* g = L->global;
    switch (g->gcstate)
    {
    case GCSpause:
    {
        markroot(L); /* start a new collection */
        break;
    }
    case GCSpropagate:
    {
        if (FFlag::LuauRescanGrayAgain)
        {
            if (g->gray)
            {
                g->gcstats.currcycle.markitems++;

                cost = propagatemark(g);
            }
            else
            {
                // perform one iteration over 'gray again' list
                g->gray = g->grayagain;
                g->grayagain = NULL;

                g->gcstate = GCSpropagateagain;
            }
        }
        else
        {
            if (g->gray)
            {
                g->gcstats.currcycle.markitems++;

                cost = propagatemark(g);
            }
            else /* no more `gray' objects */
            {
                double starttimestamp = lua_clock();

                g->gcstats.currcycle.atomicstarttimestamp = starttimestamp;
                g->gcstats.currcycle.atomicstarttotalsizebytes = g->totalbytes;

                atomic(L); /* finish mark phase */

                g->gcstats.currcycle.atomictime += lua_clock() - starttimestamp;
            }
        }
        break;
    }
    case GCSpropagateagain:
    {
        if (g->gray)
        {
            g->gcstats.currcycle.markitems++;

            cost = propagatemark(g);
        }
        else /* no more `gray' objects */
        {
            double starttimestamp = lua_clock();

            g->gcstats.currcycle.atomicstarttimestamp = starttimestamp;
            g->gcstats.currcycle.atomicstarttotalsizebytes = g->totalbytes;

            atomic(L); /* finish mark phase */

            g->gcstats.currcycle.atomictime += lua_clock() - starttimestamp;
        }
        break;
    }
    case GCSsweepstring:
    {
        size_t traversedcount = 0;
        sweepwholelist(L, &g->strt.hash[g->sweepstrgc++], &traversedcount);

        // nothing more to sweep?
        if (g->sweepstrgc >= g->strt.size)
        {
            // sweep string buffer list and preserve used string count
            uint32_t nuse = L->global->strt.nuse;
            sweepwholelist(L, &g->strbufgc, &traversedcount);
            L->global->strt.nuse = nuse;

            g->gcstate = GCSsweep; // end sweep-string phase
        }

        g->gcstats.currcycle.sweepitems += traversedcount;

        cost = GC_SWEEPCOST;
        break;
    }
    case GCSsweep:
    {
        size_t traversedcount = 0;
        g->sweepgc = sweeplist(L, g->sweepgc, GC_SWEEPMAX, &traversedcount);

        g->gcstats.currcycle.sweepitems += traversedcount;

        if (*g->sweepgc == NULL)
        { /* nothing more to sweep? */
            shrinkbuffers(L);
            g->gcstate = GCSpause; /* end collection */
        }
        cost = GC_SWEEPMAX * GC_SWEEPCOST;
        break;
    }
    default:
        LUAU_ASSERT(0);
    }

    return cost;
}

static int64_t getheaptriggererroroffset(GCHeapTriggerStats* triggerstats, GCCycleStats* cyclestats)
{
    // adjust for error using Proportional-Integral controller
    // https://en.wikipedia.org/wiki/PID_controller
    int32_t errorKb = int32_t((cyclestats->atomicstarttotalsizebytes - cyclestats->heapgoalsizebytes) / 1024);

    // we use sliding window for the error integral to avoid error sum 'windup' when the desired target cannot be reached
    int32_t* slot = &triggerstats->terms[triggerstats->termpos % triggerstats->termcount];
    int32_t prev = *slot;
    *slot = errorKb;
    triggerstats->integral += errorKb - prev;
    triggerstats->termpos++;

    // controller tuning
    // https://en.wikipedia.org/wiki/Ziegler%E2%80%93Nichols_method
    const double Ku = 0.9; // ultimate gain (measured)
    const double Tu = 2.5; // oscillation period (measured)

    const double Kp = 0.45 * Ku; // proportional gain
    const double Ti = 0.8 * Tu;
    const double Ki = 0.54 * Ku / Ti; // integral gain

    double proportionalTerm = Kp * errorKb;
    double integralTerm = Ki * triggerstats->integral;

    double totalTerm = proportionalTerm + integralTerm;

    return int64_t(totalTerm * 1024);
}

static size_t getheaptrigger(global_State* g, size_t heapgoal)
{
    GCCycleStats* lastcycle = &g->gcstats.lastcycle;
    GCCycleStats* currcycle = &g->gcstats.currcycle;

    // adjust threshold based on a guess of how many bytes will be allocated between the cycle start and sweep phase
    // our goal is to begin the sweep when used memory has reached the heap goal
    const double durationthreshold = 1e-3;
    double allocationduration = currcycle->atomicstarttimestamp - lastcycle->endtimestamp;

    // avoid measuring intervals smaller than 1ms
    if (allocationduration < durationthreshold)
        return heapgoal;

    double allocationrate = (currcycle->atomicstarttotalsizebytes - lastcycle->endtotalsizebytes) / allocationduration;
    double markduration = currcycle->atomicstarttimestamp - currcycle->starttimestamp;

    int64_t expectedgrowth = int64_t(markduration * allocationrate);
    int64_t offset = getheaptriggererroroffset(&g->gcstats.triggerstats, currcycle);
    int64_t heaptrigger = heapgoal - (expectedgrowth + offset);

    // clamp the trigger between memory use at the end of the cycle and the heap goal
    return heaptrigger < int64_t(g->totalbytes) ? g->totalbytes : (heaptrigger > int64_t(heapgoal) ? heapgoal : size_t(heaptrigger));
}

void luaC_step(lua_State* L, bool assist)
{
    global_State* g = L->global;
    ptrdiff_t lim = (g->gcstepsize / 100) * g->gcstepmul; /* how much to work */
    LUAU_ASSERT(g->totalbytes >= g->GCthreshold);
    size_t debt = g->totalbytes - g->GCthreshold;

    GC_INTERRUPT(0);

    // at the start of the new cycle
    if (g->gcstate == GCSpause)
        startGcCycleStats(g);

    if (assist)
        g->gcstats.currcycle.assistwork += lim;
    else
        g->gcstats.currcycle.explicitwork += lim;

    int lastgcstate = g->gcstate;
    double lastttimestamp = lua_clock();

    // always perform at least one single step
    do
    {
        lim -= singlestep(L);

        // if we have switched to a different state, capture the duration of last stage
        // this way we reduce the number of timer calls we make
        if (lastgcstate != g->gcstate)
        {
            GC_INTERRUPT(lastgcstate);

            double now = lua_clock();

            recordGcStateTime(g, lastgcstate, now - lastttimestamp, assist);

            lastttimestamp = now;
            lastgcstate = g->gcstate;
        }
    } while (lim > 0 && g->gcstate != GCSpause);

    recordGcStateTime(g, lastgcstate, lua_clock() - lastttimestamp, assist);

    // at the end of the last cycle
    if (g->gcstate == GCSpause)
    {
        // at the end of a collection cycle, set goal based on gcgoal setting
        size_t heapgoal = (g->totalbytes / 100) * g->gcgoal;
        size_t heaptrigger = getheaptrigger(g, heapgoal);

        g->GCthreshold = heaptrigger;

        finishGcCycleStats(g);

        g->gcstats.currcycle.heapgoalsizebytes = heapgoal;
        g->gcstats.currcycle.heaptriggersizebytes = heaptrigger;
    }
    else
    {
        g->GCthreshold = g->totalbytes + g->gcstepsize;

        // compensate if GC is "behind schedule" (has some debt to pay)
        if (g->GCthreshold > debt)
            g->GCthreshold -= debt;
    }

    GC_INTERRUPT(g->gcstate);
}

void luaC_fullgc(lua_State* L)
{
    global_State* g = L->global;

    if (g->gcstate == GCSpause)
        startGcCycleStats(g);

    if (g->gcstate <= GCSpropagateagain)
    {
        /* reset sweep marks to sweep all elements (returning them to white) */
        g->sweepstrgc = 0;
        g->sweepgc = &g->rootgc;
        /* reset other collector lists */
        g->gray = NULL;
        g->grayagain = NULL;
        g->weak = NULL;
        g->gcstate = GCSsweepstring;
    }
    LUAU_ASSERT(g->gcstate != GCSpause && g->gcstate != GCSpropagate && g->gcstate != GCSpropagateagain);
    /* finish any pending sweep phase */
    while (g->gcstate != GCSpause)
    {
        LUAU_ASSERT(g->gcstate == GCSsweepstring || g->gcstate == GCSsweep);
        singlestep(L);
    }

    finishGcCycleStats(g);

    /* run a full collection cycle */
    startGcCycleStats(g);

    markroot(L);
    while (g->gcstate != GCSpause)
    {
        singlestep(L);
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

    finishGcCycleStats(g);

    g->gcstats.currcycle.heapgoalsizebytes = heapgoalsizebytes;
    g->gcstats.currcycle.heaptriggersizebytes = g->GCthreshold;
}

void luaC_barrierupval(lua_State* L, GCObject* v)
{
    if (FFlag::LuauGcFullSkipInactiveThreads)
    {
        global_State* g = L->global;
        LUAU_ASSERT(iswhite(v) && !isdead(g, v));

        if (keepinvariant(g))
            reallymarkobject(g, v);
    }
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
    if (FFlag::LuauRescanGrayAgainForwardBarrier && g->gcstate == GCSpropagateagain)
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

void luaC_linkobj(lua_State* L, GCObject* o, uint8_t tt)
{
    global_State* g = L->global;
    o->gch.next = g->rootgc;
    g->rootgc = o;
    o->gch.marked = luaC_white(g);
    o->gch.tt = tt;
    o->gch.memcat = L->activememcat;
}

void luaC_linkupval(lua_State* L, UpVal* uv)
{
    global_State* g = L->global;
    GCObject* o = obj2gco(uv);
    o->gch.next = g->rootgc; /* link upvalue into `rootgc' list */
    g->rootgc = o;
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

static void validateobjref(global_State* g, GCObject* f, GCObject* t)
{
    LUAU_ASSERT(!isdead(g, t));

    if (keepinvariant(g))
    {
        /* basic incremental invariant: black can't point to white */
        LUAU_ASSERT(!(isblack(f) && iswhite(t)));
    }
}

static void validateref(global_State* g, GCObject* f, TValue* v)
{
    if (iscollectable(v))
    {
        LUAU_ASSERT(ttype(v) == gcvalue(v)->gch.tt);
        validateobjref(g, f, gcvalue(v));
    }
}

static void validatetable(global_State* g, Table* h)
{
    int sizenode = 1 << h->lsizenode;

    if (FFlag::LuauArrayBoundary)
        LUAU_ASSERT(h->lastfree <= sizenode);
    else
        LUAU_ASSERT(h->lastfree >= 0 && h->lastfree <= sizenode);

    if (h->metatable)
        validateobjref(g, obj2gco(h), obj2gco(h->metatable));

    for (int i = 0; i < h->sizearray; ++i)
        validateref(g, obj2gco(h), &h->array[i]);

    for (int i = 0; i < sizenode; ++i)
    {
        LuaNode* n = &h->node[i];

        LUAU_ASSERT(ttype(gkey(n)) != LUA_TDEADKEY || ttisnil(gval(n)));
        LUAU_ASSERT(i + gnext(n) >= 0 && i + gnext(n) < sizenode);

        if (!ttisnil(gval(n)))
        {
            TValue k = {};
            k.tt = gkey(n)->tt;
            k.value = gkey(n)->value;

            validateref(g, obj2gco(h), &k);
            validateref(g, obj2gco(h), gval(n));
        }
    }
}

static void validateclosure(global_State* g, Closure* cl)
{
    validateobjref(g, obj2gco(cl), obj2gco(cl->env));

    if (cl->isC)
    {
        for (int i = 0; i < cl->nupvalues; ++i)
            validateref(g, obj2gco(cl), &cl->c.upvals[i]);
    }
    else
    {
        LUAU_ASSERT(cl->nupvalues == cl->l.p->nups);

        validateobjref(g, obj2gco(cl), obj2gco(cl->l.p));

        for (int i = 0; i < cl->nupvalues; ++i)
            validateref(g, obj2gco(cl), &cl->l.uprefs[i]);
    }
}

static void validatestack(global_State* g, lua_State* l)
{
    validateref(g, obj2gco(l), gt(l));

    for (CallInfo* ci = l->base_ci; ci <= l->ci; ++ci)
    {
        LUAU_ASSERT(l->stack <= ci->base);
        LUAU_ASSERT(ci->func <= ci->base && ci->base <= ci->top);
        LUAU_ASSERT(ci->top <= l->stack_last);
    }

    // note: stack refs can violate gc invariant so we only check for liveness
    for (StkId o = l->stack; o < l->top; ++o)
        checkliveness(g, o);

    if (l->namecall)
        validateobjref(g, obj2gco(l), obj2gco(l->namecall));

    for (GCObject* uv = l->openupval; uv; uv = uv->gch.next)
    {
        LUAU_ASSERT(uv->gch.tt == LUA_TUPVAL);
        LUAU_ASSERT(gco2uv(uv)->v != &gco2uv(uv)->u.value);
    }
}

static void validateproto(global_State* g, Proto* f)
{
    if (f->source)
        validateobjref(g, obj2gco(f), obj2gco(f->source));

    if (f->debugname)
        validateobjref(g, obj2gco(f), obj2gco(f->debugname));

    for (int i = 0; i < f->sizek; ++i)
        validateref(g, obj2gco(f), &f->k[i]);

    for (int i = 0; i < f->sizeupvalues; ++i)
        if (f->upvalues[i])
            validateobjref(g, obj2gco(f), obj2gco(f->upvalues[i]));

    for (int i = 0; i < f->sizep; ++i)
        if (f->p[i])
            validateobjref(g, obj2gco(f), obj2gco(f->p[i]));

    for (int i = 0; i < f->sizelocvars; i++)
        if (f->locvars[i].varname)
            validateobjref(g, obj2gco(f), obj2gco(f->locvars[i].varname));
}

static void validateobj(global_State* g, GCObject* o)
{
    /* dead objects can only occur during sweep */
    if (isdead(g, o))
    {
        LUAU_ASSERT(g->gcstate == GCSsweepstring || g->gcstate == GCSsweep);
        return;
    }

    switch (o->gch.tt)
    {
    case LUA_TSTRING:
        break;

    case LUA_TTABLE:
        validatetable(g, gco2h(o));
        break;

    case LUA_TFUNCTION:
        validateclosure(g, gco2cl(o));
        break;

    case LUA_TUSERDATA:
        if (gco2u(o)->metatable)
            validateobjref(g, o, obj2gco(gco2u(o)->metatable));
        break;

    case LUA_TTHREAD:
        validatestack(g, gco2th(o));
        break;

    case LUA_TPROTO:
        validateproto(g, gco2p(o));
        break;

    case LUA_TUPVAL:
        validateref(g, o, gco2uv(o)->v);
        break;

    default:
        LUAU_ASSERT(!"unexpected object type");
    }
}

static void validatelist(global_State* g, GCObject* o)
{
    while (o)
    {
        validateobj(g, o);

        o = o->gch.next;
    }
}

static void validategraylist(global_State* g, GCObject* o)
{
    if (!keepinvariant(g))
        return;

    while (o)
    {
        LUAU_ASSERT(isgray(o));

        switch (o->gch.tt)
        {
        case LUA_TTABLE:
            o = gco2h(o)->gclist;
            break;
        case LUA_TFUNCTION:
            o = gco2cl(o)->gclist;
            break;
        case LUA_TTHREAD:
            o = gco2th(o)->gclist;
            break;
        case LUA_TPROTO:
            o = gco2p(o)->gclist;
            break;
        default:
            LUAU_ASSERT(!"unknown object in gray list");
            return;
        }
    }
}

void luaC_validate(lua_State* L)
{
    global_State* g = L->global;

    LUAU_ASSERT(!isdead(g, obj2gco(g->mainthread)));
    checkliveness(g, &g->registry);

    for (int i = 0; i < LUA_T_COUNT; ++i)
        if (g->mt[i])
            LUAU_ASSERT(!isdead(g, obj2gco(g->mt[i])));

    validategraylist(g, g->weak);
    validategraylist(g, g->gray);
    validategraylist(g, g->grayagain);

    for (int i = 0; i < g->strt.size; ++i)
        validatelist(g, g->strt.hash[i]);

    validatelist(g, g->rootgc);
    validatelist(g, g->strbufgc);

    for (UpVal* uv = g->uvhead.u.l.next; uv != &g->uvhead; uv = uv->u.l.next)
    {
        LUAU_ASSERT(uv->tt == LUA_TUPVAL);
        LUAU_ASSERT(uv->v != &uv->u.value);
        LUAU_ASSERT(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
    }
}

inline bool safejson(char ch)
{
    return unsigned(ch) < 128 && ch >= 32 && ch != '\\' && ch != '\"';
}

static void dumpref(FILE* f, GCObject* o)
{
    fprintf(f, "\"%p\"", o);
}

static void dumprefs(FILE* f, TValue* data, size_t size)
{
    bool first = true;

    for (size_t i = 0; i < size; ++i)
    {
        if (iscollectable(&data[i]))
        {
            if (!first)
                fputc(',', f);
            first = false;

            dumpref(f, gcvalue(&data[i]));
        }
    }
}

static void dumpstringdata(FILE* f, const char* data, size_t len)
{
    for (size_t i = 0; i < len; ++i)
        fputc(safejson(data[i]) ? data[i] : '?', f);
}

static void dumpstring(FILE* f, TString* ts)
{
    fprintf(f, "{\"type\":\"string\",\"cat\":%d,\"size\":%d,\"data\":\"", ts->memcat, int(sizestring(ts->len)));
    dumpstringdata(f, ts->data, ts->len);
    fprintf(f, "\"}");
}

static void dumptable(FILE* f, Table* h)
{
    size_t size = sizeof(Table) + (h->node == &luaH_dummynode ? 0 : sizenode(h) * sizeof(LuaNode)) + h->sizearray * sizeof(TValue);

    fprintf(f, "{\"type\":\"table\",\"cat\":%d,\"size\":%d", h->memcat, int(size));

    if (h->node != &luaH_dummynode)
    {
        fprintf(f, ",\"pairs\":[");

        bool first = true;

        for (int i = 0; i < sizenode(h); ++i)
        {
            const LuaNode& n = h->node[i];

            if (!ttisnil(&n.val) && (iscollectable(&n.key) || iscollectable(&n.val)))
            {
                if (!first)
                    fputc(',', f);
                first = false;

                if (iscollectable(&n.key))
                    dumpref(f, gcvalue(&n.key));
                else
                    fprintf(f, "null");

                fputc(',', f);

                if (iscollectable(&n.val))
                    dumpref(f, gcvalue(&n.val));
                else
                    fprintf(f, "null");
            }
        }

        fprintf(f, "]");
    }
    if (h->sizearray)
    {
        fprintf(f, ",\"array\":[");
        dumprefs(f, h->array, h->sizearray);
        fprintf(f, "]");
    }
    if (h->metatable)
    {
        fprintf(f, ",\"metatable\":");
        dumpref(f, obj2gco(h->metatable));
    }
    fprintf(f, "}");
}

static void dumpclosure(FILE* f, Closure* cl)
{
    fprintf(f, "{\"type\":\"function\",\"cat\":%d,\"size\":%d", cl->memcat,
        cl->isC ? int(sizeCclosure(cl->nupvalues)) : int(sizeLclosure(cl->nupvalues)));

    fprintf(f, ",\"env\":");
    dumpref(f, obj2gco(cl->env));
    if (cl->isC)
    {
        if (cl->nupvalues)
        {
            fprintf(f, ",\"upvalues\":[");
            dumprefs(f, cl->c.upvals, cl->nupvalues);
            fprintf(f, "]");
        }
    }
    else
    {
        fprintf(f, ",\"proto\":");
        dumpref(f, obj2gco(cl->l.p));
        if (cl->nupvalues)
        {
            fprintf(f, ",\"upvalues\":[");
            dumprefs(f, cl->l.uprefs, cl->nupvalues);
            fprintf(f, "]");
        }
    }
    fprintf(f, "}");
}

static void dumpudata(FILE* f, Udata* u)
{
    fprintf(f, "{\"type\":\"userdata\",\"cat\":%d,\"size\":%d,\"tag\":%d", u->memcat, int(sizeudata(u->len)), u->tag);

    if (u->metatable)
    {
        fprintf(f, ",\"metatable\":");
        dumpref(f, obj2gco(u->metatable));
    }
    fprintf(f, "}");
}

static void dumpthread(FILE* f, lua_State* th)
{
    size_t size = sizeof(lua_State) + sizeof(TValue) * th->stacksize + sizeof(CallInfo) * th->size_ci;

    fprintf(f, "{\"type\":\"thread\",\"cat\":%d,\"size\":%d", th->memcat, int(size));

    if (iscollectable(&th->l_gt))
    {
        fprintf(f, ",\"env\":");
        dumpref(f, gcvalue(&th->l_gt));
    }

    Closure* tcl = 0;
    for (CallInfo* ci = th->base_ci; ci <= th->ci; ++ci)
    {
        if (ttisfunction(ci->func))
        {
            tcl = clvalue(ci->func);
            break;
        }
    }

    if (tcl && !tcl->isC && tcl->l.p->source)
    {
        Proto* p = tcl->l.p;

        fprintf(f, ",\"source\":\"");
        dumpstringdata(f, p->source->data, p->source->len);
        fprintf(f, "\",\"line\":%d", p->abslineinfo ? p->abslineinfo[0] : 0);
    }

    if (th->top > th->stack)
    {
        fprintf(f, ",\"stack\":[");
        dumprefs(f, th->stack, th->top - th->stack);
        fprintf(f, "]");
    }
    fprintf(f, "}");
}

static void dumpproto(FILE* f, Proto* p)
{
    size_t size = sizeof(Proto) + sizeof(Instruction) * p->sizecode + sizeof(Proto*) * p->sizep + sizeof(TValue) * p->sizek + p->sizelineinfo +
                  sizeof(LocVar) * p->sizelocvars + sizeof(TString*) * p->sizeupvalues;

    fprintf(f, "{\"type\":\"proto\",\"cat\":%d,\"size\":%d", p->memcat, int(size));

    if (p->source)
    {
        fprintf(f, ",\"source\":\"");
        dumpstringdata(f, p->source->data, p->source->len);
        fprintf(f, "\",\"line\":%d", p->abslineinfo ? p->abslineinfo[0] : 0);
    }

    if (p->sizek)
    {
        fprintf(f, ",\"constants\":[");
        dumprefs(f, p->k, p->sizek);
        fprintf(f, "]");
    }

    if (p->sizep)
    {
        fprintf(f, ",\"protos\":[");
        for (int i = 0; i < p->sizep; ++i)
        {
            if (i != 0)
                fputc(',', f);
            dumpref(f, obj2gco(p->p[i]));
        }
        fprintf(f, "]");
    }

    fprintf(f, "}");
}

static void dumpupval(FILE* f, UpVal* uv)
{
    fprintf(f, "{\"type\":\"upvalue\",\"cat\":%d,\"size\":%d", uv->memcat, int(sizeof(UpVal)));

    if (iscollectable(uv->v))
    {
        fprintf(f, ",\"object\":");
        dumpref(f, gcvalue(uv->v));
    }
    fprintf(f, "}");
}

static void dumpobj(FILE* f, GCObject* o)
{
    switch (o->gch.tt)
    {
    case LUA_TSTRING:
        return dumpstring(f, gco2ts(o));

    case LUA_TTABLE:
        return dumptable(f, gco2h(o));

    case LUA_TFUNCTION:
        return dumpclosure(f, gco2cl(o));

    case LUA_TUSERDATA:
        return dumpudata(f, gco2u(o));

    case LUA_TTHREAD:
        return dumpthread(f, gco2th(o));

    case LUA_TPROTO:
        return dumpproto(f, gco2p(o));

    case LUA_TUPVAL:
        return dumpupval(f, gco2uv(o));

    default:
        LUAU_ASSERT(0);
    }
}

static void dumplist(FILE* f, GCObject* o)
{
    while (o)
    {
        dumpref(f, o);
        fputc(':', f);
        dumpobj(f, o);
        fputc(',', f);
        fputc('\n', f);

        // thread has additional list containing collectable objects that are not present in rootgc
        if (o->gch.tt == LUA_TTHREAD)
            dumplist(f, gco2th(o)->openupval);

        o = o->gch.next;
    }
}

void luaC_dump(lua_State* L, void* file, const char* (*categoryName)(lua_State* L, uint8_t memcat))
{
    global_State* g = L->global;
    FILE* f = static_cast<FILE*>(file);

    fprintf(f, "{\"objects\":{\n");
    dumplist(f, g->rootgc);
    dumplist(f, g->strbufgc);
    for (int i = 0; i < g->strt.size; ++i)
        dumplist(f, g->strt.hash[i]);

    fprintf(f, "\"0\":{\"type\":\"userdata\",\"cat\":0,\"size\":0}\n"); // to avoid issues with trailing ,
    fprintf(f, "},\"roots\":{\n");
    fprintf(f, "\"mainthread\":");
    dumpref(f, obj2gco(g->mainthread));
    fprintf(f, ",\"registry\":");
    dumpref(f, gcvalue(&g->registry));

    fprintf(f, "},\"stats\":{\n");

    fprintf(f, "\"size\":%d,\n", int(g->totalbytes));

    fprintf(f, "\"categories\":{\n");
    for (int i = 0; i < LUA_MEMORY_CATEGORIES; i++)
    {
        if (size_t bytes = g->memcatbytes[i])
        {
            if (categoryName)
                fprintf(f, "\"%d\":{\"name\":\"%s\", \"size\":%d},\n", i, categoryName(L, i), int(bytes));
            else
                fprintf(f, "\"%d\":{\"size\":%d},\n", i, int(bytes));
        }
    }
    fprintf(f, "\"none\":{}\n"); // to avoid issues with trailing ,
    fprintf(f, "}\n");
    fprintf(f, "}}\n");
}

// measure the allocation rate in bytes/sec
// returns -1 if allocation rate cannot be measured
int64_t luaC_allocationrate(lua_State* L)
{
    global_State* g = L->global;
    const double durationthreshold = 1e-3; // avoid measuring intervals smaller than 1ms

    if (g->gcstate <= GCSpropagateagain)
    {
        double duration = lua_clock() - g->gcstats.lastcycle.endtimestamp;

        if (duration < durationthreshold)
            return -1;

        return int64_t((g->totalbytes - g->gcstats.lastcycle.endtotalsizebytes) / duration);
    }

    // totalbytes is unstable during the sweep, use the rate measured at the end of mark phase
    double duration = g->gcstats.currcycle.atomicstarttimestamp - g->gcstats.lastcycle.endtimestamp;

    if (duration < durationthreshold)
        return -1;

    return int64_t((g->gcstats.currcycle.atomicstarttotalsizebytes - g->gcstats.lastcycle.endtotalsizebytes) / duration);
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

    case GCSsweepstring:
        return "sweepstring";

    case GCSsweep:
        return "sweep";

    default:
        return NULL;
    }
}
