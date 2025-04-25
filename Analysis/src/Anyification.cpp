// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Anyification.h"

#include "Luau/Common.h"
#include "Luau/Normalize.h"
#include "Luau/TxnLog.h"

namespace Luau
{

Anyification::Anyification(
    TypeArena* arena,
    NotNull<Scope> scope,
    NotNull<BuiltinTypes> builtinTypes,
    InternalErrorReporter* iceHandler,
    TypeId anyType,
    TypePackId anyTypePack
)
    : Substitution(TxnLog::empty(), arena)
    , scope(scope)
    , builtinTypes(builtinTypes)
    , iceHandler(iceHandler)
    , anyType(anyType)
    , anyTypePack(anyTypePack)
{
}

Anyification::Anyification(
    TypeArena* arena,
    const ScopePtr& scope,
    NotNull<BuiltinTypes> builtinTypes,
    InternalErrorReporter* iceHandler,
    TypeId anyType,
    TypePackId anyTypePack
)
    : Anyification(arena, NotNull{scope.get()}, builtinTypes, iceHandler, anyType, anyTypePack)
{
}

bool Anyification::isDirty(TypeId ty)
{
    if (ty->persistent)
        return false;

    if (const TableType* ttv = log->getMutable<TableType>(ty))
        return (ttv->state == TableState::Free || ttv->state == TableState::Unsealed);
    else if (log->getMutable<FreeType>(ty))
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
    if (const TableType* ttv = log->getMutable<TableType>(ty))
    {
        TableType clone = TableType{ttv->props, ttv->indexer, ttv->level, TableState::Sealed};
        clone.definitionModuleName = ttv->definitionModuleName;
        clone.definitionLocation = ttv->definitionLocation;
        clone.name = ttv->name;
        clone.syntheticName = ttv->syntheticName;
        clone.tags = ttv->tags;
        TypeId res = addType(std::move(clone));
        return res;
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
    if (get<ExternType>(ty))
        return true;

    return ty->persistent;
}
bool Anyification::ignoreChildren(TypePackId ty)
{
    return ty->persistent;
}

} // namespace Luau
