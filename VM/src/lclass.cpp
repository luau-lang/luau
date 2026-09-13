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
    constructor->c.debugname = luaS_new(L, "luaR_constructobject");

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
    defaultCtor->c.debugname = luaS_new(L, "luaR_defaultcreateobject");

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

/**
 * Allocates and returns a new class object with the same members as `classobject`.
 * @param classobject The class object to clone.
 */
LUAI_FUNC LuauClass* luaR_cloneclass(lua_State* L, LuauClass* classobject)
{
    LuauClass* newclass = luaR_newblankclass(L, classobject->name, classobject->isopen);

    // newclass was just allocated, so it is white and none of the writes below need a write barrier.
    LUAU_ASSERT(iswhite(obj2gco(newclass)));

    newclass->super = classobject->super;
    newclass->hasuserinitinchain = classobject->hasuserinitinchain;

    const uint32_t numallmembers = classobject->numberofallmembers;
    const uint32_t numstaticmembers = numallmembers - classobject->numberofinstancemembers;

    // The name->offset mapping is fixed when the class shape is built and is never mutated afterwards (shapes in a Proto's constant table are
    // additionally marked readonly), so the clone shares it rather than paying for a table copy on every class definition that executes.
    newclass->memberstooffset = classobject->memberstooffset;

    newclass->offsettomember = luaM_newarray(L, numallmembers, TString*, newclass->memcat);
    memcpy(newclass->offsettomember, classobject->offsettomember, numallmembers * sizeof(TString*));

    newclass->numberofallmembers = numallmembers;

    newclass->staticmembers = luaM_newarray(L, numstaticmembers, TValue, newclass->memcat);
    memcpy(newclass->staticmembers, classobject->staticmembers, numstaticmembers * sizeof(TValue));

    newclass->numberofinstancemembers = classobject->numberofinstancemembers;

    if (classobject->instancemetatable)
        newclass->instancemetatable = luaH_clone(L, classobject->instancemetatable);

    luaR_setupconstructor(L, newclass, getcurrenv(L));

    return newclass;
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
 * Updates child with parent's instance members and non-overridden static members in the following steps:
- Check that parent is open.
- Check for illegal instance member overrides in child.
- Point child's super to parent.
- Set staticmembers and offsettomember to NULL so GC doesn't try to free them in a weird state.
- Bump member offsets in child->memberstooffset by parent->numberofinstancemembers since instances of child will have parent's instance members
come first in their memory layouts.
- Add entries for each of parent's instance members to child->memberstooffset.
- Count how many static members we need to copy over from parent to child (ie non-overridden ones).
- Resize childOffsettomember appropriately. The child class's instance and static members are moved up by parent->numberofinstancemembers, the
parent's instance members copied over at the beginning.
- Resize childStaticMembers.
- Copy each non-overridden static member from parent->staticmembers to child->staticmembers, add an entry for it to child->memberstooffset, and add
it to child->offsettomember.
- Copy parent's instance metatable if it exists. We don't need to worry about overwriting the child's instance metatable because it's only created
by NEWCLASSMEMBER instructions, which are only ever emitted after NEWCLASS. (TODO: This isn't true if we inherit lazily)
 */
void luaR_inheritclass(lua_State* L, LuauClass* child, LuauClass* parent)
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

    child->super = parent;
    luaC_objbarrier(L, child, parent);

    // TODO: might need updating for lazy inheritance
    child->hasuserinitinchain = parent->hasuserinitinchain;

    uint32_t childDeclaredStaticMembers = child->numberofallmembers - child->numberofinstancemembers;
    TValue* childStaticMembers = child->staticmembers;
    TString** childOffsetToMember = child->offsettomember;

    child->staticmembers = NULL;
    child->offsettomember = NULL;

    if (parent->numberofinstancemembers > 0)
    {
        // Bump every member offset in child->memberstooffset up by parent->numberofinstancemembers (even static members are shifted up), and then add
        // the parent's instance members to child->memberstooffset.
        for (uint32_t idx = 0; idx < child->numberofallmembers; idx++)
        {
            TString* memberName = childOffsetToMember[idx];
            TValue* offsetInChild = luaH_setstr(L, child->memberstooffset, memberName);
            setnvalue(offsetInChild, nvalue(offsetInChild) + parent->numberofinstancemembers);
        }

        for (uint32_t idx = 0; idx < parent->numberofinstancemembers; idx++)
        {
            TString* memberName = parent->offsettomember[idx];
            TValue* offsetInChild = luaH_setstr(L, child->memberstooffset, memberName);
            setnvalue(offsetInChild, idx);
            luaC_barrier(L, child->memberstooffset, offsetInChild);
        }
    }

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

    uint32_t originalChildAllMembers = child->numberofallmembers;
    uint32_t newNumberOfAllMembers = originalChildAllMembers + parent->numberofinstancemembers + numStaticMembersToCopy;

    // Resize childOffsetToMember appropriately
    if (newNumberOfAllMembers > originalChildAllMembers)
    {
        if (originalChildAllMembers == 0)
            childOffsetToMember = luaM_newarray(L, newNumberOfAllMembers, TString*, child->memcat);
        else
            luaM_reallocarray(L, childOffsetToMember, originalChildAllMembers, newNumberOfAllMembers, TString*, child->memcat);
    }

    // Make room for parent instance members
    memmove(childOffsetToMember + parent->numberofinstancemembers, childOffsetToMember, sizeof(TString*) * originalChildAllMembers);

    // Copy parent instance members to the beginning of childOffsetToMember
    memcpy(childOffsetToMember, parent->offsettomember, sizeof(TString*) * parent->numberofinstancemembers);

    child->offsettomember = childOffsetToMember;
    child->numberofallmembers = newNumberOfAllMembers;

    // Resize child->staticmembers appropriately
    if (numStaticMembersToCopy > 0)
    {
        if (childDeclaredStaticMembers == 0)
            childStaticMembers = luaM_newarray(L, numStaticMembersToCopy, TValue, child->memcat);
        else
        {
            luaM_reallocarray(
                L, childStaticMembers, childDeclaredStaticMembers, childDeclaredStaticMembers + numStaticMembersToCopy, TValue, child->memcat
            );
        }
    }

    child->staticmembers = childStaticMembers;
    child->numberofinstancemembers += parent->numberofinstancemembers;

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
            uint32_t staticMemberOffsetInChild = childDeclaredStaticMembers + numStaticMembersCopied;

            const TValue* parentVal = &parent->staticmembers[idx - parent->numberofinstancemembers];

            setobj2class(L, &child->staticmembers[staticMemberOffsetInChild], parentVal);
            luaC_barrier(L, child, &child->staticmembers[staticMemberOffsetInChild]);

            // We also need to add an entry to the memberstooffset table for this member
            uint32_t offsetInChildInt = child->numberofinstancemembers + staticMemberOffsetInChild;
            TValue* offsetInChild = luaH_setstr(L, child->memberstooffset, memberName);
            setnvalue(offsetInChild, static_cast<int>(offsetInChildInt));
            luaC_barrier(L, child->memberstooffset, offsetInChild);

            // And add it to offsettomember
            child->offsettomember[offsetInChildInt] = memberName;

            numStaticMembersCopied++;
        }
    }

    LUAU_ASSERT(numStaticMembersToCopy == numStaticMembersCopied);

    // Copy instance metatable
    if (parent->instancemetatable)
    {
        LUAU_ASSERT(!child->instancemetatable);
        child->instancemetatable = luaH_clone(L, parent->instancemetatable);
        luaC_objbarrier(L, child, child->instancemetatable);
    }
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
