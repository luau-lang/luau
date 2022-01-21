// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lfunc.h"

#include "lstate.h"
#include "lmem.h"
#include "lgc.h"

LUAU_FASTFLAGVARIABLE(LuauNoDirectUpvalRemoval, false)
LUAU_FASTFLAG(LuauGcPagedSweep)

Proto* luaF_newproto(lua_State* L)
{
    Proto* f = luaM_newgco(L, Proto, sizeof(Proto), L->activememcat);
    luaC_link(L, f, LUA_TPROTO);
    f->k = NULL;
    f->sizek = 0;
    f->p = NULL;
    f->sizep = 0;
    f->code = NULL;
    f->sizecode = 0;
    f->sizeupvalues = 0;
    f->nups = 0;
    f->upvalues = NULL;
    f->numparams = 0;
    f->is_vararg = 0;
    f->maxstacksize = 0;
    f->sizelineinfo = 0;
    f->linegaplog2 = 0;
    f->lineinfo = NULL;
    f->abslineinfo = NULL;
    f->sizelocvars = 0;
    f->locvars = NULL;
    f->source = NULL;
    f->debugname = NULL;
    f->debuginsn = NULL;
    return f;
}

Closure* luaF_newLclosure(lua_State* L, int nelems, Table* e, Proto* p)
{
    Closure* c = luaM_newgco(L, Closure, sizeLclosure(nelems), L->activememcat);
    luaC_link(L, c, LUA_TFUNCTION);
    c->isC = 0;
    c->env = e;
    c->nupvalues = cast_byte(nelems);
    c->stacksize = p->maxstacksize;
    c->preload = 0;
    c->l.p = p;
    for (int i = 0; i < nelems; ++i)
        setnilvalue(&c->l.uprefs[i]);
    return c;
}

Closure* luaF_newCclosure(lua_State* L, int nelems, Table* e)
{
    Closure* c = luaM_newgco(L, Closure, sizeCclosure(nelems), L->activememcat);
    luaC_link(L, c, LUA_TFUNCTION);
    c->isC = 1;
    c->env = e;
    c->nupvalues = cast_byte(nelems);
    c->stacksize = LUA_MINSTACK;
    c->preload = 0;
    c->c.f = NULL;
    c->c.cont = NULL;
    c->c.debugname = NULL;
    return c;
}

UpVal* luaF_findupval(lua_State* L, StkId level)
{
    global_State* g = L->global;
    UpVal** pp = &L->openupval;
    UpVal* p;
    while (*pp != NULL && (p = *pp)->v >= level)
    {
        LUAU_ASSERT(p->v != &p->u.value);
        if (p->v == level)
        {                                /* found a corresponding upvalue? */
            if (isdead(g, obj2gco(p)))   /* is it dead? */
                changewhite(obj2gco(p)); /* resurrect it */
            return p;
        }

        // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
        pp = (UpVal**)&p->next;
    }

    UpVal* uv = luaM_newgco(L, UpVal, sizeof(UpVal), L->activememcat); /* not found: create a new one */
    uv->tt = LUA_TUPVAL;
    uv->marked = luaC_white(g);
    uv->memcat = L->activememcat;
    uv->v = level;  /* current value lives in the stack */

    // chain the upvalue in the threads open upvalue list at the proper position
    UpVal* next = *pp;

    // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
    uv->next = (GCObject*)next;

    if (FFlag::LuauGcPagedSweep)
    {
        uv->u.l.threadprev = pp;
        if (next)
        {
            // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and the cast will not be required
            next->u.l.threadprev = (UpVal**)&uv->next;
        }
    }

    *pp = uv;

    // double link the upvalue in the global open upvalue list
    uv->u.l.prev = &g->uvhead;
    uv->u.l.next = g->uvhead.u.l.next;
    uv->u.l.next->u.l.prev = uv;
    g->uvhead.u.l.next = uv;
    LUAU_ASSERT(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
    return uv;
}
void luaF_unlinkupval(UpVal* uv)
{
    // unlink upvalue from the global open upvalue list
    LUAU_ASSERT(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
    uv->u.l.next->u.l.prev = uv->u.l.prev;
    uv->u.l.prev->u.l.next = uv->u.l.next;

    if (FFlag::LuauGcPagedSweep)
    {
        // unlink upvalue from the thread open upvalue list
        // TODO (FFlagLuauGcPagedSweep): 'next' type will change after removal of the flag and this and the following cast will not be required
        *uv->u.l.threadprev = (UpVal*)uv->next;

        if (UpVal* next = (UpVal*)uv->next)
            next->u.l.threadprev = uv->u.l.threadprev;
    }
}

void luaF_freeupval(lua_State* L, UpVal* uv, lua_Page* page)
{
    if (uv->v != &uv->u.value)                   /* is it open? */
        luaF_unlinkupval(uv);                    /* remove from open list */
    luaM_freegco(L, uv, sizeof(UpVal), uv->memcat, page); /* free upvalue */
}

void luaF_close(lua_State* L, StkId level)
{
    global_State* g = L->global; // TODO: remove with FFlagLuauNoDirectUpvalRemoval
    UpVal* uv;
    while (L->openupval != NULL && (uv = L->openupval)->v >= level)
    {
        GCObject* o = obj2gco(uv);
        LUAU_ASSERT(!isblack(o) && uv->v != &uv->u.value);

        if (!FFlag::LuauGcPagedSweep)
            L->openupval = (UpVal*)uv->next; /* remove from `open' list */

        if (FFlag::LuauGcPagedSweep && isdead(g, o))
        {
            // by removing the upvalue from global/thread open upvalue lists, L->openupval will be pointing to the next upvalue
            luaF_unlinkupval(uv);
            // close the upvalue without copying the dead data so that luaF_freeupval will not unlink again
            uv->v = &uv->u.value;
        }
        else if (!FFlag::LuauNoDirectUpvalRemoval && isdead(g, o))
        {
            luaF_freeupval(L, uv, NULL); /* free upvalue */
        }
        else
        {
            // by removing the upvalue from global/thread open upvalue lists, L->openupval will be pointing to the next upvalue
            luaF_unlinkupval(uv);
            setobj(L, &uv->u.value, uv->v);
            uv->v = &uv->u.value;  /* now current value lives here */
            luaC_linkupval(L, uv); /* link upvalue into `gcroot' list */
        }
    }
}

void luaF_freeproto(lua_State* L, Proto* f, lua_Page* page)
{
    luaM_freearray(L, f->code, f->sizecode, Instruction, f->memcat);
    luaM_freearray(L, f->p, f->sizep, Proto*, f->memcat);
    luaM_freearray(L, f->k, f->sizek, TValue, f->memcat);
    if (f->lineinfo)
        luaM_freearray(L, f->lineinfo, f->sizelineinfo, uint8_t, f->memcat);
    luaM_freearray(L, f->locvars, f->sizelocvars, struct LocVar, f->memcat);
    luaM_freearray(L, f->upvalues, f->sizeupvalues, TString*, f->memcat);
    if (f->debuginsn)
        luaM_freearray(L, f->debuginsn, f->sizecode, uint8_t, f->memcat);
    luaM_freegco(L, f, sizeof(Proto), f->memcat, page);
}

void luaF_freeclosure(lua_State* L, Closure* c, lua_Page* page)
{
    int size = c->isC ? sizeCclosure(c->nupvalues) : sizeLclosure(c->nupvalues);
    luaM_freegco(L, c, size, c->memcat, page);
}

const LocVar* luaF_getlocal(const Proto* f, int local_number, int pc)
{
    int i;
    for (i = 0; i < f->sizelocvars; i++)
    {
        if (pc >= f->locvars[i].startpc && pc < f->locvars[i].endpc)
        { /* is variable active? */
            local_number--;
            if (local_number == 0)
                return &f->locvars[i];
        }
    }
    return NULL; /* not found */
}
