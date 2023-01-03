// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeArena.h"

LUAU_FASTFLAGVARIABLE(DebugLuauFreezeArena, false);

namespace Luau
{

void TypeArena::clear()
{
    types.clear();
    typePacks.clear();
}

TypeId TypeArena::addTV(Type&& tv)
{
    TypeId allocated = types.allocate(std::move(tv));

    asMutable(allocated)->owningArena = this;

    return allocated;
}

TypeId TypeArena::freshType(TypeLevel level)
{
    TypeId allocated = types.allocate(FreeType{level});

    asMutable(allocated)->owningArena = this;

    return allocated;
}

TypeId TypeArena::freshType(Scope* scope)
{
    TypeId allocated = types.allocate(FreeType{scope});

    asMutable(allocated)->owningArena = this;

    return allocated;
}

TypeId TypeArena::freshType(Scope* scope, TypeLevel level)
{
    TypeId allocated = types.allocate(FreeType{scope, level});

    asMutable(allocated)->owningArena = this;

    return allocated;
}

TypePackId TypeArena::freshTypePack(Scope* scope)
{
    TypePackId allocated = typePacks.allocate(FreeTypePack{scope});

    asMutable(allocated)->owningArena = this;

    return allocated;
}

TypePackId TypeArena::addTypePack(std::initializer_list<TypeId> types)
{
    TypePackId allocated = typePacks.allocate(TypePack{std::move(types)});

    asMutable(allocated)->owningArena = this;

    return allocated;
}

TypePackId TypeArena::addTypePack(std::vector<TypeId> types, std::optional<TypePackId> tail)
{
    TypePackId allocated = typePacks.allocate(TypePack{std::move(types), tail});

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

    arena.types.freeze();
    arena.typePacks.freeze();
}

void unfreeze(TypeArena& arena)
{
    if (!FFlag::DebugLuauFreezeArena)
        return;

    arena.types.unfreeze();
    arena.typePacks.unfreeze();
}

} // namespace Luau
