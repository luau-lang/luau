// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details

#include "lclass.h"

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

LuauClass* luaR_newblankclass(lua_State* L, TString* name)
{
    LuauClass* classobject = luaM_newgco(L, LuauClass, sizeof(LuauClass), L->activememcat);
    luaC_init(L, classobject, LUA_TCLASS);
    classobject->name = name;
    classobject->staticmembers = NULL;
    classobject->memberstooffset = NULL;
    classobject->offsettomember = NULL;
    classobject->metatable = NULL;
    classobject->instancemetatable = NULL;
    classobject->numberofinstancemembers = 0;
    classobject->numberofallmembers = 0;

    return classobject;
}

// Initialize the metatable of the _class object_, which for now only
// contains an __call entry for the class constructor.
void luaR_addclassmetatable(lua_State* L, LuauClass* classobject)
{
    classobject->metatable = luaH_new(L, 0, 1);
    // We should probably pass an empty table here rather than the global
    // environment.
    Closure* constructor = luaF_newCclosure(L, 0, L->gt);
    constructor->c.f = luaR_createobject;

    if (FFlag::LuauManagedDebugNames)
        constructor->c.debugname = luaS_new(L, "luaR_createobject");
    else
        constructor->c.debugname_DEPRECATED = "luaR_createobject";

    constructor->c.cont = NULL;
    TValue* dest = luaH_setstr(L, classobject->metatable, L->global->tmname[TM_CALL]);
    LUAU_ASSERT(ttisnil(dest));
    setclvalue(L, dest, constructor);
    classobject->metatable->readonly = true;
}

LuauClass* luaR_newclass(
    lua_State* L,
    TString* name,
    LuaTable* memberstooffset,
    TString** offsettomember,
    uint32_t numberofinstancemembers,
    uint32_t numberofstaticmembers
)
{
    LUAU_ASSERT(L->global->GCthreshold == SIZE_MAX && "GC must be paused");
    LuauClass* classobject = luaR_newblankclass(L, name);

    classobject->staticmembers = luaM_newarray(L, numberofstaticmembers, TValue, classobject->memcat);
    // Initialize static members to nil, otherwise we may read uninitialized memory.
    for (uint32_t i = 0; i < numberofstaticmembers; i++)
        setnilvalue(&classobject->staticmembers[i]);

    classobject->memberstooffset = memberstooffset;
    classobject->offsettomember = offsettomember;

    classobject->numberofinstancemembers = numberofinstancemembers;
    classobject->numberofallmembers = numberofinstancemembers + numberofstaticmembers;

    luaR_addclassmetatable(L, classobject);
    classobject->instancemetatable = NULL;

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
- Check for illegal instance member overrides.
- Allocate a new LuauClass object.
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
LuauClass* luaR_inheritclass(lua_State* L, const LuauClass* child, const LuauClass* parent)
{
    // First, check for illegal instance member overrides
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

    LuauClass* newClass = luaR_newblankclass(L, child->name);

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

    luaR_addclassmetatable(L, newClass);

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

int luaR_createobject(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TCLASS);
    LuauClass* classobject = classvalue(L->base);
    LuauObject* classinst = luaM_newgco(L, LuauObject, sizeof(LuauObject), L->activememcat);
    luaC_init(L, classinst, LUA_TOBJECT);
    classinst->lclass = classobject;
    classinst->members = luaM_newarray(L, classobject->numberofinstancemembers, TValue, L->activememcat);
    classinst->numberofmembers = classobject->numberofinstancemembers;
    int numargs = lua_gettop(L);

    // We need to initialize all of the instance members to `nil` to start.
    for (uint32_t idx = 0; idx < classobject->numberofinstancemembers; idx++)
        setnilvalue(&classinst->members[idx]);

    // Push the class object onto the stack. We do this prior to setting the
    // fields as we may reallocate the stack as part of indexing into the
    // second argument (if present).
    setobjectvalue(L, L->top, classinst);
    L->top++;

    // Stack location to hold the table lookup result
    setnilvalue(L->top);
    L->top++;

    switch (numargs)
    {
    case 1:
        // If given no second argument, assume all class members are `nil`.
        break;
    case 2:
        // If given a second argument, use it to initialize all class members.
        for (uint32_t idx = 0; idx < classobject->numberofinstancemembers; idx++)
        {
            TValue key;
            setsvalue(L, &key, classobject->offsettomember[idx]);
            luaV_gettable(L, L->base + 1, &key, L->top - 1);
            setobj(L, &classinst->members[idx], L->top - 1);
        }
        break;
    default:
        luaL_error(L, "wrong number of arguments for constructing a '%s'", getstr(classobject->name));
    }

    L->top--;

    // Preserve the GC invariant, moving barrier back once after writing multiple objects (similar to SETLIST)
    luaC_barrierfast(L, classinst);

    return 1;
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
