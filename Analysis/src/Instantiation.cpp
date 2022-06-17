// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"
#include "Luau/Instantiation.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeArena.h"

namespace Luau
{

bool Instantiation::isDirty(TypeId ty)
{
    if (const FunctionTypeVar* ftv = log->getMutable<FunctionTypeVar>(ty))
    {
        if (ftv->hasNoGenerics)
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
    if (log->getMutable<FunctionTypeVar>(ty))
        return true;
    else
        return false;
}

TypeId Instantiation::clean(TypeId ty)
{
    const FunctionTypeVar* ftv = log->getMutable<FunctionTypeVar>(ty);
    LUAU_ASSERT(ftv);

    FunctionTypeVar clone = FunctionTypeVar{level, ftv->argTypes, ftv->retTypes, ftv->definition, ftv->hasSelf};
    clone.magicFunction = ftv->magicFunction;
    clone.tags = ftv->tags;
    clone.argNames = ftv->argNames;
    TypeId result = addType(std::move(clone));

    // Annoyingly, we have to do this even if there are no generics,
    // to replace any generic tables.
    ReplaceGenerics replaceGenerics{log, arena, level, ftv->generics, ftv->genericPacks};

    // TODO: What to do if this returns nullopt?
    // We don't have access to the error-reporting machinery
    result = replaceGenerics.substitute(result).value_or(result);

    asMutable(result)->documentationSymbol = ty->documentationSymbol;
    return result;
}

TypePackId Instantiation::clean(TypePackId tp)
{
    LUAU_ASSERT(false);
    return tp;
}

bool ReplaceGenerics::ignoreChildren(TypeId ty)
{
    if (const FunctionTypeVar* ftv = log->getMutable<FunctionTypeVar>(ty))
    {
        if (ftv->hasNoGenerics)
            return true;

        // We aren't recursing in the case of a generic function which
        // binds the same generics. This can happen if, for example, there's recursive types.
        // If T = <a>(a,T)->T then instantiating T should produce T' = (X,T)->T not T' = (X,T')->T'.
        // It's OK to use vector equality here, since we always generate fresh generics
        // whenever we quantify, so the vectors overlap if and only if they are equal.
        return (!generics.empty() || !genericPacks.empty()) && (ftv->generics == generics) && (ftv->genericPacks == genericPacks);
    }
    else
    {
        return false;
    }
}

bool ReplaceGenerics::isDirty(TypeId ty)
{
    if (const TableTypeVar* ttv = log->getMutable<TableTypeVar>(ty))
        return ttv->state == TableState::Generic;
    else if (log->getMutable<GenericTypeVar>(ty))
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
    if (const TableTypeVar* ttv = log->getMutable<TableTypeVar>(ty))
    {
        TableTypeVar clone = TableTypeVar{ttv->props, ttv->indexer, level, TableState::Free};
        clone.definitionModuleName = ttv->definitionModuleName;
        return addType(std::move(clone));
    }
    else
        return addType(FreeTypeVar{level});
}

TypePackId ReplaceGenerics::clean(TypePackId tp)
{
    LUAU_ASSERT(isDirty(tp));
    return addTypePack(TypePackVar(FreeTypePack{level}));
}

} // namespace Luau
