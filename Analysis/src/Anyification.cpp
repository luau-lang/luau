// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Anyification.h"

#include "Luau/Common.h"
#include "Luau/Normalize.h"
#include "Luau/TxnLog.h"

LUAU_FASTFLAG(LuauClassTypeVarsInSubstitution)

namespace Luau
{

Anyification::Anyification(TypeArena* arena, const ScopePtr& scope, InternalErrorReporter* iceHandler, TypeId anyType, TypePackId anyTypePack)
    : Substitution(TxnLog::empty(), arena)
    , scope(NotNull{scope.get()})
    , iceHandler(iceHandler)
    , anyType(anyType)
    , anyTypePack(anyTypePack)
{
}

bool Anyification::isDirty(TypeId ty)
{
    if (ty->persistent)
        return false;

    if (const TableTypeVar* ttv = log->getMutable<TableTypeVar>(ty))
        return (ttv->state == TableState::Free || ttv->state == TableState::Unsealed);
    else if (log->getMutable<FreeTypeVar>(ty))
        return true;
    else if (get<ConstrainedTypeVar>(ty))
        return true;
    else
        return false;
}

bool Anyification::isDirty(TypePackId tp)
{
    if (tp->persistent)
        return false;

    if (log->getMutable<FreeTypePack>(tp))
        return true;
    else
        return false;
}

TypeId Anyification::clean(TypeId ty)
{
    LUAU_ASSERT(isDirty(ty));
    if (const TableTypeVar* ttv = log->getMutable<TableTypeVar>(ty))
    {
        TableTypeVar clone = TableTypeVar{ttv->props, ttv->indexer, ttv->level, TableState::Sealed};
        clone.definitionModuleName = ttv->definitionModuleName;
        clone.name = ttv->name;
        clone.syntheticName = ttv->syntheticName;
        clone.tags = ttv->tags;
        TypeId res = addType(std::move(clone));
        asMutable(res)->normal = ty->normal;
        return res;
    }
    else if (auto ctv = get<ConstrainedTypeVar>(ty))
    {
        std::vector<TypeId> copy = ctv->parts;
        for (TypeId& ty : copy)
            ty = replace(ty);
        TypeId res = copy.size() == 1 ? copy[0] : addType(UnionTypeVar{std::move(copy)});
        auto [t, ok] = normalize(res, scope, *arena, *iceHandler);
        if (!ok)
            normalizationTooComplex = true;
        return t;
    }
    else
        return anyType;
}

TypePackId Anyification::clean(TypePackId tp)
{
    LUAU_ASSERT(isDirty(tp));
    return anyTypePack;
}

bool Anyification::ignoreChildren(TypeId ty)
{
    if (FFlag::LuauClassTypeVarsInSubstitution && get<ClassTypeVar>(ty))
        return true;

    return ty->persistent;
}
bool Anyification::ignoreChildren(TypePackId ty)
{
    return ty->persistent;
}

}
