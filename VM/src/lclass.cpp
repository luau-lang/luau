// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details

#include "lclass.h"

#include "lapi.h"
#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "ltm.h"
#include "lualib.h"
#include "lvm.h"

LUAU_FASTFLAG(LuauManagedDebugNames)

LuauClass* luaR_newblankclass(lua_State* L, TString* name, bool isopen)
{
    LuauClass* classobject = luaM_newgco(L, LuauClass, sizeof(LuauClass), L->activememcat, LUA_TCLASS);
    luaC_init(L, classobject, LUA_TCLASS);
    classobject->name = name;
    classobject->super = NULL;
    classobject->staticmembers = NULL;
    classobject->memberstooffset = NULL;
    classobject->offsettomember = NULL;
    classobject->instancemetatable = NULL;
    classobject->numberofinstancemembers = 0;
    classobject->numberofallmembers = 0;
    classobject->isopen = isopen;
    classobject->hasuserinitinchain = false;

    return classobject;
}

/*
 * We rewrite both the `new` and `__init` methods because, in the inheritance
 * scenario, a LuauClass is cloned from the original and flattened out.  This
 * flattened-out LuauClass's constructors need to have their closures updated.
 * Otherwise they point at the old un-flattened LuauClass.
 */
static void luaR_setupconstructor(lua_State* L, LuauClass* classobject, LuaTable* env)
{
    TString* newKey = luaS_new(L, "new");

    // We should probably pass an empty table here rather than the global
    // environment.
    Closure* constructor = luaF_newCclosure(L, 1, env);
    constructor->c.f = luaR_constructobject;

    if (FFlag::LuauManagedDebugNames)
        constructor->c.debugname = luaS_new(L, "luaR_constructobject");
    else
        constructor->c.debugname_DEPRECATED = "luaR_constructobject";

    // Capture the classobject to construct as an upvalue.
    setclassvalue(L, &constructor->c.upvals[0], classobject);
    LUAU_ASSERT(iswhite(obj2gco(constructor)));

    constructor->c.cont = NULL;

    const TValue* offsetValue = luaH_getstr(classobject->memberstooffset, newKey);
    const double offsetDouble = nvalue(offsetValue);
    LUAU_ASSERT(offsetDouble >= classobject->numberofinstancemembers && offsetDouble < classobject->numberofallmembers);
    const uint32_t offset = uint32_t(offsetDouble) - classobject->numberofinstancemembers;

    setclvalue(L, &classobject->staticmembers[offset], constructor);
    luaC_barrier(L, classobject, &classobject->staticmembers[offset]);

    // Add the default constructor.
    //
    // If the code defines an explicit __init method, LOP_NEWCLASSMEMBER will
    // overwrite this.
    Closure* defaultCtor = luaF_newCclosure(L, 1, env);
    defaultCtor->c.f = luaR_defaultcreateobject;

    if (FFlag::LuauManagedDebugNames)
        defaultCtor->c.debugname = luaS_new(L, "luaR_defaultcreateobject");
    else
        defaultCtor->c.debugname_DEPRECATED = "luaR_defaultcreateobject";

    setclassvalue(L, &defaultCtor->c.upvals[0], classobject);
    LUAU_ASSERT(iswhite(obj2gco(defaultCtor)));

    defaultCtor->c.cont = NULL;

    TString* initKey = luaS_new(L, "__init");
    const TValue* initIndex = luaH_getstr(classobject->memberstooffset, initKey);
    const double initDouble = nvalue(initIndex);
    LUAU_ASSERT(initDouble >= classobject->numberofinstancemembers && initDouble < classobject->numberofallmembers);
    const uint32_t initOffset = uint32_t(initDouble) - classobject->numberofinstancemembers;

    setclvalue(L, &classobject->staticmembers[initOffset], defaultCtor);
    luaC_barrier(L, classobject, &classobject->staticmembers[initOffset]);
}

LuauClass* luaR_newclass(
    lua_State* L,
    TString* name,
    LuaTable* memberstooffset,
    TString** offsettomember,
    uint32_t numberofinstancemembers,
    uint32_t numberofstaticmembers,
    LuaTable* envt
)
{
    LUAU_ASSERT(L->global->GCthreshold == SIZE_MAX && "GC must be paused");
    LuauClass* classobject = luaR_newblankclass(L, name, false);

    classobject->staticmembers = luaM_newarray(L, numberofstaticmembers, TValue, classobject->memcat);
    // Initialize static members to nil, otherwise we may read uninitialized memory.
    for (uint32_t i = 0; i < numberofstaticmembers; i++)
        setnilvalue(&classobject->staticmembers[i]);

    classobject->memberstooffset = memberstooffset;
    classobject->offsettomember = offsettomember;

    classobject->numberofinstancemembers = numberofinstancemembers;
    classobject->numberofallmembers = numberofinstancemembers + numberofstaticmembers;

    luaR_setupconstructor(L, classobject, envt);

    return classobject;
}

// Registers val as a static member of classObject with name memberName at static offset staticMemberOffset and overall offset offset.
void luaR_registerstaticmember(
    lua_State* L,
    LuauClass* classObject,
    TString* memberName,
    const TValue* val,
    uint32_t offset,
    uint32_t staticMemberOffset
)
{
    setobj2class(L, &classObject->staticmembers[staticMemberOffset], val);
    luaC_barrier(L, classObject, &classObject->staticmembers[staticMemberOffset]);

    classObject->offsettomember[offset] = memberName;

    TValue* offsetVal = luaH_setstr(L, classObject->memberstooffset, memberName);
    setnvalue(offsetVal, offset);
    luaC_barrier(L, classObject->memberstooffset, offsetVal);
}

/**
Creates and returns a new LuauClass object with child's members and methods, and relevant fields inherited from parent.
This is done in the following steps:
- Check that parent is open.
- Check for illegal instance member overrides.
- Allocate a new LuauClass object.
- Point the new class's super to parent.
- Count how many static members we'll need to copy from parent, so we know how much space to allocate for the new class.
- Copy the parent's instance members.
- Copy the child's instance members.
- Copy the parent's non-overridden static members.
- Copy the child's static members.
- Add the class metatable to the new class.
- Copy the parent's instance metatable if it exists.
Rather than mutating child, we create a new LuauClass object because the LuauClass objects created at load time are stored in the relevant Proto's
constants table. If a Closure returned by luau_load contains an inheriting class and is called repeatedly, this would result in the LuauClass object
stored in the Proto's constants table being mutated repeatedly.
 */
LuauClass* luaR_inheritclass(lua_State* L, const LuauClass* child, LuauClass* parent)
{
    // First check if parent is open
    if (!parent->isopen)
        luaG_runerror(L, "Non-open class '%s' cannot be extended", getstr(parent->name));

    // Next, check for illegal instance member overrides
    if (parent->numberofinstancemembers > 0)
    {
        for (uint32_t idx = 0; idx < parent->numberofinstancemembers; idx++)
        {
            TString* memberName = parent->offsettomember[idx];
            const TValue* existing = luaH_getstr(child->memberstooffset, memberName);
            if (!ttisnil(existing))
                luaG_runerror(
                    L,
                    "Cannot override instance member '%s' of parent class '%s' in child class '%s'",
                    getstr(memberName),
                    getstr(parent->name),
                    getstr(child->name)
                );
        }
    }

    LuauClass* newClass = luaR_newblankclass(L, child->name, child->isopen);

    newClass->super = parent;

    // Count how many static members we'll actually need to copy from parent, ie non-overridden ones
    uint32_t numStaticMembersToCopy = 0;

    // We start at numberofinstancemembers so we only look at static members
    for (uint32_t idx = parent->numberofinstancemembers; idx < parent->numberofallmembers; idx++)
    {
        TString* memberName = parent->offsettomember[idx];
        const TValue* existing = luaH_getstr(child->memberstooffset, memberName);
        if (ttisnil(existing))
            numStaticMembersToCopy++;
        // TODO: Throw an error if we overwrite a static member with an instance member?
    }

    uint32_t numMembers = child->numberofallmembers + parent->numberofinstancemembers + numStaticMembersToCopy;

    newClass->offsettomember = luaM_newarray(L, numMembers, TString*, newClass->memcat);
    newClass->numberofallmembers = numMembers;

    newClass->memberstooffset = luaH_new(L, 0, numMembers);
    luaC_objbarrier(L, newClass, newClass->memberstooffset);

    newClass->hasuserinitinchain = parent->hasuserinitinchain;

    uint32_t offset = 0;

    if (parent->numberofinstancemembers > 0)
    {
        for (; offset < parent->numberofinstancemembers; offset++)
        {
            TString* memberName = parent->offsettomember[offset];

            newClass->offsettomember[offset] = memberName;

            TValue* val = luaH_setstr(L, newClass->memberstooffset, memberName);
            setnvalue(val, offset);
            luaC_barrier(L, newClass->memberstooffset, val);
        }
    }

    if (child->numberofinstancemembers > 0)
    {
        for (uint32_t idx = 0; idx < child->numberofinstancemembers; idx++, offset++)
        {
            TString* memberName = child->offsettomember[idx];

            newClass->offsettomember[offset] = memberName;

            TValue* val = luaH_setstr(L, newClass->memberstooffset, memberName);
            setnvalue(val, offset);
            luaC_barrier(L, newClass->memberstooffset, val);
        }
    }

    // We've just copied all instance members, so offset is the total number of instance members in the final class
    newClass->staticmembers = luaM_newarray(L, numMembers - offset, TValue, newClass->memcat);
    newClass->numberofinstancemembers = offset;

    // Copy static members from parent that aren't overridden in child.
    uint32_t numStaticMembersCopied = 0;
    for (uint32_t idx = parent->numberofinstancemembers; idx < parent->numberofallmembers; idx++)
    {
        TString* memberName = parent->offsettomember[idx];
        // This lookup duplicates the one we did earlier, when we counted how many static members we needed to copy. We could optimize by caching the
        // indices with static members to copy.
        const TValue* existing = luaH_getstr(child->memberstooffset, memberName);
        if (ttisnil(existing))
        {
            // This static member isn't declared in the child, so we need to copy it over from the parent
            const TValue* parentVal = &parent->staticmembers[idx - parent->numberofinstancemembers];

            luaR_registerstaticmember(L, newClass, memberName, parentVal, offset, numStaticMembersCopied);

            offset++;
            numStaticMembersCopied++;
        }
    }

    // Copy child's static members over to newClass
    for (uint32_t idx = child->numberofinstancemembers; idx < child->numberofallmembers; idx++)
    {
        TString* memberName = child->offsettomember[idx];

        const TValue* childVal = &child->staticmembers[idx - child->numberofinstancemembers];

        luaR_registerstaticmember(L, newClass, memberName, childVal, offset, numStaticMembersCopied);

        offset++;
        numStaticMembersCopied++;
    }

    LUAU_ASSERT(numStaticMembersCopied == numStaticMembersToCopy + (child->numberofallmembers - child->numberofinstancemembers));

    // Copy instance metatable
    // Ignoring the child's instance metatable is sound because it is only ever created during NEWCLASSMEMBER instructions, which are only
    // emitted after NEWCLASS.
    if (parent->instancemetatable)
    {
        newClass->instancemetatable = luaH_clone(L, parent->instancemetatable);
        luaC_objbarrier(L, newClass, newClass->instancemetatable);
    }
    else
        newClass->instancemetatable = NULL;

    luaR_setupconstructor(L, newClass, getcurrenv(L));

    return newClass;
}

void luaR_addclassmember(lua_State* L, LuauClass* classobject, TString* name, TValue* value)
{
    LUAU_ASSERT(classobject->staticmembers != nullptr);
    const TValue* offset = luaH_getstr(classobject->memberstooffset, name);
    const uint32_t offsetint = uint32_t(nvalue(offset));
    LUAU_ASSERT(offsetint >= classobject->numberofinstancemembers && offsetint < classobject->numberofallmembers);
    LUAU_ASSERT(ttisfunction(value) && value->value.gc->gch.tt == LUA_TFUNCTION);
    setobj2class(L, &classobject->staticmembers[offsetint - classobject->numberofinstancemembers], value);
    luaC_barrier(L, classobject, value);

    classobject->hasuserinitinchain |= (name == luaS_newlstr(L, "__init", 6));

    // Only metamethods in the parser's allowlist are supported (see ALLOWED_METAMETHODS in Parser.cpp)
    bool isMetamethod = (name == luaS_newlstr(L, "__tostring", 10));
    for (int i = 0; i < TM_N && !isMetamethod; i++)
        isMetamethod = (name == L->global->tmname[i]);

    if (isMetamethod)
    {
        if (!classobject->instancemetatable)
        {
            classobject->instancemetatable = luaH_new(L, 0, 1);
            luaC_objbarrier(L, classobject, classobject->instancemetatable);
        }
        TValue* dest = luaH_setstr(L, classobject->instancemetatable, name);
        setobj2t(L, dest, value);
        luaC_barrier(L, classobject->instancemetatable, value);
    }
}

int luaR_constructobject(lua_State* L)
{
    Closure* cl = clvalue(L->ci->func);
    LuauClass* classobject = classvalue(&cl->c.upvals[0]);

    LuauObject* self = luaM_newgco(L, LuauObject, sizeof(LuauObject), L->activememcat, LUA_TOBJECT);
    memset(self, 0, sizeof(LuauObject));
    luaC_init(L, self, LUA_TOBJECT);
    self->lclass = classobject;
    self->members = luaM_newarray(L, classobject->numberofinstancemembers, TValue, L->activememcat);
    self->numberofmembers = classobject->numberofinstancemembers;

    for (uint32_t idx = 0; idx < classobject->numberofinstancemembers; idx++)
        setnilvalue(&self->members[idx]);

    TString* initKey = luaS_new(L, "__init");
    const TValue* initIndex = luaH_getstr(classobject->memberstooffset, initKey);
    const uint32_t initOffset = uint32_t(nvalue(initIndex)) - classobject->numberofinstancemembers;

    const TValue* initFunction = &classobject->staticmembers[initOffset];

    int numargs = int(L->top - L->base);

    // Put self onto the stack to ensure that it unconditionally survives GC during execution of __init.
    // The reference via the `self` argument to __init is insufficient to guarantee survival because `__init` may do `self = nil` and trigger GC.
    setobjectvalue(L, L->top, self);
    L->top++;

    luaD_checkstack(L, 2 + numargs);

    StkId argsBase = L->top;
    // __init itself.
    setobj2s(L, L->top++, initFunction);

    // self
    setobjectvalue(L, L->top++, self);

    // Forward .new() arguments.
    for (int i = 0; i < numargs; i++)
        setobj2s(L, L->top++, L->base + i);

    luaD_call(L, argsBase, 0);

    // self is still at L->top - 1
    return 1;
}

int luaR_defaultcreateobject(lua_State* L)
{
    Closure* cl = clvalue(L->ci->func);
    LuauClass* classobject = classvalue(&cl->c.upvals[0]);

    if (classobject->hasuserinitinchain)
        luaL_error(L, "Class %s must define a constructor because it is derived from a class that defines one", getstr(classobject->name));

    int numargs = lua_gettop(L);
    if (numargs != 2)
        luaL_error(L, "The constructor of %s must be called with 2 arguments.  Got %d", getstr(classobject->name), numargs);

    // L->base + 0 = self
    // L->base + 1 = props (if numargs == 2)

    if (!ttisobject(L->base))
        luaL_error(L, "%s.__init must be called with an instance of the class as its first argument", getstr(classobject->name));

    LuauObject* classinst = objectvalue(L->base);
    LUAU_ASSERT(classinst);

    if (classinst->lclass != classobject)
        luaL_errorL(L, "Cannot call %s.__init on an instance of class %s", getstr(classobject->name), getstr(classinst->lclass->name));

    constexpr int propSlot = 1;

    // L->top - 1 = Temp storage for the table lookup result.
    setnilvalue(L->top);
    L->top++;

    // Use the second argument to initialize all class members.
    for (uint32_t idx = 0; idx < classobject->numberofinstancemembers; idx++)
    {
        TValue key;
        setsvalue(L, &key, classobject->offsettomember[idx]);
        luaV_gettable(L, L->base + propSlot, &key, L->top - 1);
        setobj(L, &classinst->members[idx], L->top - 1);
        luaC_barrier(L, classinst, &classinst->members[idx]);
    }

    L->top--;

    return 0;
}


void luaR_freeclass(lua_State* L, LuauClass* classobject, lua_Page* page)
{
    if (classobject->staticmembers)
    {
        luaM_freearray(
            L, classobject->staticmembers, classobject->numberofallmembers - classobject->numberofinstancemembers, TValue, classobject->memcat
        );
    }

    if (classobject->offsettomember)
        luaM_freearray(L, classobject->offsettomember, classobject->numberofallmembers, TString*, classobject->memcat);

    luaM_freegco(L, classobject, sizeof(LuauClass), classobject->memcat, page);
}

void luaR_freeobject(lua_State* L, LuauObject* classinstance, lua_Page* page)
{
    luaM_freearray(L, classinstance->members, classinstance->numberofmembers, TValue, classinstance->memcat);
    luaM_freegco(L, classinstance, sizeof(LuauObject), classinstance->memcat, page);
}
