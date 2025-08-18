// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Instantiation.h"

#include "Luau/Common.h"
#include "Luau/Instantiation2.h" // including for `Replacer` which was stolen since it will be kept in the new solver
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeCheckLimits.h"

#include <algorithm>

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauParametrizedAttributeSyntax)

namespace Luau
{

void Instantiation::resetState(const TxnLog* log, TypeArena* arena, NotNull<BuiltinTypes> builtinTypes, TypeLevel level, Scope* scope)
{
    Substitution::resetState(log, arena);

    this->builtinTypes = builtinTypes;

    this->level = level;
    this->scope = scope;
}

bool Instantiation::isDirty(TypeId ty)
{
    if (const FunctionType* ftv = log->getMutable<FunctionType>(ty))
    {
        if (ftv->hasNoFreeOrGenericTypes)
            return false;

        return true;
    }
    else
    {
        return false;
    }
}

bool Instantiation::isDirty(TypePackId tp)
{
    return false;
}

bool Instantiation::ignoreChildren(TypeId ty)
{
    if (log->getMutable<FunctionType>(ty))
        return true;
    else if (get<ExternType>(ty))
        return true;
    else
        return false;
}

TypeId Instantiation::clean(TypeId ty)
{
    const FunctionType* ftv = log->getMutable<FunctionType>(ty);
    LUAU_ASSERT(ftv);

    FunctionType clone = FunctionType{level, ftv->argTypes, ftv->retTypes, ftv->definition, ftv->hasSelf};
    clone.magic = ftv->magic;
    clone.tags = ftv->tags;
    clone.argNames = ftv->argNames;
    if (FFlag::LuauParametrizedAttributeSyntax)
    {
        clone.isDeprecatedFunction = ftv->isDeprecatedFunction;
        clone.deprecatedInfo = ftv->deprecatedInfo;
    }
    TypeId result = addType(std::move(clone));

    // Annoyingly, we have to do this even if there are no generics,
    // to replace any generic tables.
    reusableReplaceGenerics.resetState(log, arena, builtinTypes, level, scope, ftv->generics, ftv->genericPacks);

    // TODO: What to do if this returns nullopt?
    // We don't have access to the error-reporting machinery
    result = reusableReplaceGenerics.substitute(result).value_or(result);

    asMutable(result)->documentationSymbol = ty->documentationSymbol;
    return result;
}

TypePackId Instantiation::clean(TypePackId tp)
{
    LUAU_ASSERT(false);
    return tp;
}

void ReplaceGenerics::resetState(
    const TxnLog* log,
    TypeArena* arena,
    NotNull<BuiltinTypes> builtinTypes,
    TypeLevel level,
    Scope* scope,
    const std::vector<TypeId>& generics,
    const std::vector<TypePackId>& genericPacks
)
{
    Substitution::resetState(log, arena);

    this->builtinTypes = builtinTypes;

    this->level = level;
    this->scope = scope;

    this->generics = generics;
    this->genericPacks = genericPacks;
}

bool ReplaceGenerics::ignoreChildren(TypeId ty)
{
    if (const FunctionType* ftv = log->getMutable<FunctionType>(ty))
    {
        if (ftv->hasNoFreeOrGenericTypes)
            return true;

        // We aren't recursing in the case of a generic function which
        // binds the same generics. This can happen if, for example, there's recursive types.
        // If T = <a>(a,T)->T then instantiating T should produce T' = (X,T)->T not T' = (X,T')->T'.
        // It's OK to use vector equality here, since we always generate fresh generics
        // whenever we quantify, so the vectors overlap if and only if they are equal.
        return (!generics.empty() || !genericPacks.empty()) && (ftv->generics == generics) && (ftv->genericPacks == genericPacks);
    }
    else if (get<ExternType>(ty))
        return true;
    else
    {
        return false;
    }
}

bool ReplaceGenerics::isDirty(TypeId ty)
{
    if (const TableType* ttv = log->getMutable<TableType>(ty))
        return ttv->state == TableState::Generic;
    else if (log->getMutable<GenericType>(ty))
        return std::find(generics.begin(), generics.end(), ty) != generics.end();
    else
        return false;
}

bool ReplaceGenerics::isDirty(TypePackId tp)
{
    if (log->getMutable<GenericTypePack>(tp))
        return std::find(genericPacks.begin(), genericPacks.end(), tp) != genericPacks.end();
    else
        return false;
}

TypeId ReplaceGenerics::clean(TypeId ty)
{
    LUAU_ASSERT(isDirty(ty));
    if (const TableType* ttv = log->getMutable<TableType>(ty))
    {
        TableType clone = TableType{ttv->props, ttv->indexer, level, scope, TableState::Free};
        clone.definitionModuleName = ttv->definitionModuleName;
        clone.definitionLocation = ttv->definitionLocation;
        return addType(std::move(clone));
    }
    else if (FFlag::LuauSolverV2)
    {
        TypeId res = freshType(NotNull{arena}, builtinTypes, scope);
        getMutable<FreeType>(res)->level = level;
        return res;
    }
    else
    {
        return arena->freshType(builtinTypes, scope, level);
    }
}

TypePackId ReplaceGenerics::clean(TypePackId tp)
{
    LUAU_ASSERT(isDirty(tp));
    return addTypePack(TypePackVar(FreeTypePack{scope, level}));
}

std::optional<TypeId> instantiate(
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    NotNull<TypeCheckLimits> limits,
    NotNull<Scope> scope,
    TypeId ty
)
{
    ty = follow(ty);

    const FunctionType* ft = get<FunctionType>(ty);
    if (!ft)
        return ty;

    if (ft->generics.empty() && ft->genericPacks.empty())
        return ty;

    DenseHashMap<TypeId, TypeId> replacements{nullptr};
    DenseHashMap<TypePackId, TypePackId> replacementPacks{nullptr};

    for (TypeId g : ft->generics)
        replacements[g] = freshType(arena, builtinTypes, scope);

    for (TypePackId g : ft->genericPacks)
        replacementPacks[g] = arena->freshTypePack(scope);

    Replacer r{arena, std::move(replacements), std::move(replacementPacks)};

    if (limits->instantiationChildLimit)
        r.childLimit = *limits->instantiationChildLimit;

    std::optional<TypeId> res = r.substitute(ty);
    if (!res)
        return res;

    FunctionType* ft2 = getMutable<FunctionType>(*res);
    LUAU_ASSERT(ft != ft2);

    ft2->generics.clear();
    ft2->genericPacks.clear();

    return res;
}

} // namespace Luau
