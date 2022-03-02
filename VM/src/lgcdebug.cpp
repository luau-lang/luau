// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lgc.h"

#include "lfunc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "ludata.h"

#include <string.h>
#include <stdio.h>

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

    LUAU_ASSERT(h->lastfree <= sizenode);

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
    validateobjref(g, obj2gco(l), obj2gco(l->gt));

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

    for (UpVal* uv = l->openupval; uv; uv = uv->u.l.threadnext)
    {
        LUAU_ASSERT(uv->tt == LUA_TUPVAL);
        LUAU_ASSERT(uv->v != &uv->u.value);
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
        LUAU_ASSERT(g->gcstate == GCSsweep);
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

static bool validategco(void* context, lua_Page* page, GCObject* gco)
{
    lua_State* L = (lua_State*)context;
    global_State* g = L->global;

    validateobj(g, gco);
    return false;
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

    validategco(L, NULL, obj2gco(g->mainthread));

    luaM_visitgco(L, L, validategco);

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

    fprintf(f, ",\"env\":");
    dumpref(f, obj2gco(th->gt));

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

static bool dumpgco(void* context, lua_Page* page, GCObject* gco)
{
    FILE* f = (FILE*)context;

    dumpref(f, gco);
    fputc(':', f);
    dumpobj(f, gco);
    fputc(',', f);
    fputc('\n', f);

    return false;
}

void luaC_dump(lua_State* L, void* file, const char* (*categoryName)(lua_State* L, uint8_t memcat))
{
    global_State* g = L->global;
    FILE* f = static_cast<FILE*>(file);

    fprintf(f, "{\"objects\":{\n");

    dumpgco(f, NULL, obj2gco(g->mainthread));

    luaM_visitgco(L, f, dumpgco);

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
