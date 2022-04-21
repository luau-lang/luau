// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Module.h"

#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/Normalize.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"

#include <algorithm>

LUAU_FASTFLAGVARIABLE(DebugLuauFreezeArena, false)
LUAU_FASTFLAG(LuauLowerBoundsCalculation)
LUAU_FASTFLAG(LuauLosslessClone)

namespace Luau
{

static bool contains(Position pos, Comment comment)
{
    if (comment.location.contains(pos))
        return true;
    else if (comment.type == Lexeme::BrokenComment &&
             comment.location.begin <= pos) // Broken comments are broken specifically because they don't have an end
        return true;
    else if (comment.type == Lexeme::Comment && comment.location.end == pos)
        return true;
    else
        return false;
}

bool isWithinComment(const SourceModule& sourceModule, Position pos)
{
    auto iter = std::lower_bound(sourceModule.commentLocations.begin(), sourceModule.commentLocations.end(),
        Comment{Lexeme::Comment, Location{pos, pos}}, [](const Comment& a, const Comment& b) {
            return a.location.end < b.location.end;
        });

    if (iter == sourceModule.commentLocations.end())
        return false;

    if (contains(pos, *iter))
        return true;

    // Due to the nature of std::lower_bound, it is possible that iter points at a comment that ends
    // at pos.  We'll try the next comment, if it exists.
    ++iter;
    if (iter == sourceModule.commentLocations.end())
        return false;

    return contains(pos, *iter);
}

void TypeArena::clear()
{
    typeVars.clear();
    typePacks.clear();
}

TypeId TypeArena::addTV(TypeVar&& tv)
{
    TypeId allocated = typeVars.allocate(std::move(tv));

    asMutable(allocated)->owningArena = this;

    return allocated;
}

TypeId TypeArena::freshType(TypeLevel level)
{
    TypeId allocated = typeVars.allocate(FreeTypeVar{level});

    asMutable(allocated)->owningArena = this;

    return allocated;
}

TypePackId TypeArena::addTypePack(std::initializer_list<TypeId> types)
{
    TypePackId allocated = typePacks.allocate(TypePack{std::move(types)});

    asMutable(allocated)->owningArena = this;

    return allocated;
}

TypePackId TypeArena::addTypePack(std::vector<TypeId> types)
{
    TypePackId allocated = typePacks.allocate(TypePack{std::move(types)});

    asMutable(allocated)->owningArena = this;

    return allocated;
}

TypePackId TypeArena::addTypePack(TypePack tp)
{
    TypePackId allocated = typePacks.allocate(std::move(tp));

    asMutable(allocated)->owningArena = this;

    return allocated;
}

TypePackId TypeArena::addTypePack(TypePackVar tp)
{
    TypePackId allocated = typePacks.allocate(std::move(tp));

    asMutable(allocated)->owningArena = this;

    return allocated;
}

ScopePtr Module::getModuleScope() const
{
    LUAU_ASSERT(!scopes.empty());
    return scopes.front().second;
}

void freeze(TypeArena& arena)
{
    if (!FFlag::DebugLuauFreezeArena)
        return;

    arena.typeVars.freeze();
    arena.typePacks.freeze();
}

void unfreeze(TypeArena& arena)
{
    if (!FFlag::DebugLuauFreezeArena)
        return;

    arena.typeVars.unfreeze();
    arena.typePacks.unfreeze();
}

Module::~Module()
{
    unfreeze(interfaceTypes);
    unfreeze(internalTypes);
}

bool Module::clonePublicInterface(InternalErrorReporter& ice)
{
    LUAU_ASSERT(interfaceTypes.typeVars.empty());
    LUAU_ASSERT(interfaceTypes.typePacks.empty());

    CloneState cloneState;

    ScopePtr moduleScope = getModuleScope();

    moduleScope->returnType = clone(moduleScope->returnType, interfaceTypes, cloneState);
    if (moduleScope->varargPack)
        moduleScope->varargPack = clone(*moduleScope->varargPack, interfaceTypes, cloneState);

    if (FFlag::LuauLowerBoundsCalculation)
    {
        normalize(moduleScope->returnType, interfaceTypes, ice);
        if (moduleScope->varargPack)
            normalize(*moduleScope->varargPack, interfaceTypes, ice);
    }

    for (auto& [name, tf] : moduleScope->exportedTypeBindings)
    {
        tf = clone(tf, interfaceTypes, cloneState);
        if (FFlag::LuauLowerBoundsCalculation)
            normalize(tf.type, interfaceTypes, ice);
    }

    for (TypeId ty : moduleScope->returnType)
    {
        if (get<GenericTypeVar>(follow(ty)))
        {
            auto t = asMutable(ty);
            t->ty = AnyTypeVar{};
            t->normal = true;
        }
    }

    for (auto& [name, ty] : declaredGlobals)
    {
        ty = clone(ty, interfaceTypes, cloneState);
        if (FFlag::LuauLowerBoundsCalculation)
            normalize(ty, interfaceTypes, ice);
    }

    freeze(internalTypes);
    freeze(interfaceTypes);

    if (FFlag::LuauLosslessClone)
        return false; // TODO: make function return void.
    else
        return cloneState.encounteredFreeType;
}

} // namespace Luau
