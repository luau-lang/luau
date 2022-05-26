// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeArena.h"

LUAU_FASTFLAGVARIABLE(DebugLuauFreezeArena, false);

namespace Luau
{

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

} // namespace Luau
