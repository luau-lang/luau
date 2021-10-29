// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lfunc.h"

#include "lstate.h"
#include "lmem.h"
#include "lgc.h"

Proto* luaF_newproto(lua_State* L)
{
    Proto* f = luaM_new(L, Proto, sizeof(Proto), L->activememcat);
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
    Closure* c = luaM_new(L, Closure, sizeLclosure(nelems), L->activememcat);
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
    Closure* c = luaM_new(L, Closure, sizeCclosure(nelems), L->activememcat);
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
    GCObject** pp = &L->openupval;
    UpVal* p;
    UpVal* uv;
    while (*pp != NULL && (p = gco2uv(*pp))->v >= level)
    {
        LUAU_ASSERT(p->v != &p->u.value);
        if (p->v == level)
        {                                /* found a corresponding upvalue? */
            if (isdead(g, obj2gco(p)))   /* is it dead? */
                changewhite(obj2gco(p)); /* ressurect it */
            return p;
        }
        pp = &p->next;
    }
    uv = luaM_new(L, UpVal, sizeof(UpVal), L->activememcat); /* not found: create a new one */
    uv->tt = LUA_TUPVAL;
    uv->marked = luaC_white(g);
    uv->memcat = L->activememcat;
    uv->v = level;  /* current value lives in the stack */
    uv->next = *pp; /* chain it in the proper position */
    *pp = obj2gco(uv);
    uv->u.l.prev = &g->uvhead; /* double link it in `uvhead' list */
    uv->u.l.next = g->uvhead.u.l.next;
    uv->u.l.next->u.l.prev = uv;
    g->uvhead.u.l.next = uv;
    LUAU_ASSERT(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
    return uv;
}

static void unlinkupval(UpVal* uv)
{
    LUAU_ASSERT(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
    uv->u.l.next->u.l.prev = uv->u.l.prev; /* remove from `uvhead' list */
    uv->u.l.prev->u.l.next = uv->u.l.next;
}

void luaF_freeupval(lua_State* L, UpVal* uv)
{
    if (uv->v != &uv->u.value)                   /* is it open? */
        unlinkupval(uv);                         /* remove from open list */
    luaM_free(L, uv, sizeof(UpVal), uv->memcat); /* free upvalue */
}

void luaF_close(lua_State* L, StkId level)
{
    UpVal* uv;
    global_State* g = L->global;
    while (L->openupval != NULL && (uv = gco2uv(L->openupval))->v >= level)
    {
        GCObject* o = obj2gco(uv);
        LUAU_ASSERT(!isblack(o) && uv->v != &uv->u.value);
        L->openupval = uv->next; /* remove from `open' list */
        if (isdead(g, o))
            luaF_freeupval(L, uv); /* free upvalue */
        else
        {
            unlinkupval(uv);
            setobj(L, &uv->u.value, uv->v);
            uv->v = &uv->u.value;  /* now current value lives here */
            luaC_linkupval(L, uv); /* link upvalue into `gcroot' list */
        }
    }
}

void luaF_freeproto(lua_State* L, Proto* f)
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
    luaM_free(L, f, sizeof(Proto), f->memcat);
}

void luaF_freeclosure(lua_State* L, Closure* c)
{
    int size = c->isC ? sizeCclosure(c->nupvalues) : sizeLclosure(c->nupvalues);
    luaM_free(L, c, size, c->memcat);
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
