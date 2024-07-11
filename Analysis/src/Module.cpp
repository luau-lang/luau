// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Module.h"

#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/ConstraintGenerator.h"
#include "Luau/Normalize.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/Type.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/VisitType.h"

#include <algorithm>

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAGVARIABLE(LuauSkipEmptyInstantiations, false);

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

static bool isWithinComment(const std::vector<Comment>& commentLocations, Position pos)
{
    auto iter = std::lower_bound(
        commentLocations.begin(), commentLocations.end(), Comment{Lexeme::Comment, Location{pos, pos}}, [](const Comment& a, const Comment& b) {
            return a.location.end < b.location.end;
        });

    if (iter == commentLocations.end())
        return false;

    if (contains(pos, *iter))
        return true;

    // Due to the nature of std::lower_bound, it is possible that iter points at a comment that ends
    // at pos.  We'll try the next comment, if it exists.
    ++iter;
    if (iter == commentLocations.end())
        return false;

    return contains(pos, *iter);
}

bool isWithinComment(const SourceModule& sourceModule, Position pos)
{
    return isWithinComment(sourceModule.commentLocations, pos);
}

bool isWithinComment(const ParseResult& result, Position pos)
{
    return isWithinComment(result.commentLocations, pos);
}

struct ClonePublicInterface : Substitution
{
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<Module> module;

    ClonePublicInterface(const TxnLog* log, NotNull<BuiltinTypes> builtinTypes, Module* module)
        : Substitution(log, &module->interfaceTypes)
        , builtinTypes(builtinTypes)
        , module(module)
    {
        LUAU_ASSERT(module);
    }

    bool isDirty(TypeId ty) override
    {
        if (ty->owningArena == &module->internalTypes)
            return true;

        if (const FunctionType* ftv = get<FunctionType>(ty))
            return ftv->level.level != 0;
        if (const TableType* ttv = get<TableType>(ty))
            return ttv->level.level != 0;
        return false;
    }

    bool isDirty(TypePackId tp) override
    {
        return tp->owningArena == &module->internalTypes;
    }

    bool ignoreChildrenVisit(TypeId ty) override
    {
        if (ty->owningArena != &module->internalTypes)
            return true;

        return false;
    }

    bool ignoreChildrenVisit(TypePackId tp) override
    {
        if (tp->owningArena != &module->internalTypes)
            return true;

        return false;
    }

    TypeId clean(TypeId ty) override
    {
        TypeId result = clone(ty);

        if (FunctionType* ftv = getMutable<FunctionType>(result))
        {
            if (FFlag::LuauSkipEmptyInstantiations && ftv->generics.empty() && ftv->genericPacks.empty())
            {
                GenericTypeFinder marker;
                marker.traverse(result);

                if (!marker.found)
                    ftv->hasNoFreeOrGenericTypes = true;
            }

            ftv->level = TypeLevel{0, 0};
        }
        else if (TableType* ttv = getMutable<TableType>(result))
        {
            ttv->level = TypeLevel{0, 0};
        }

        return result;
    }

    TypePackId clean(TypePackId tp) override
    {
        return clone(tp);
    }

    TypeId cloneType(TypeId ty)
    {
        std::optional<TypeId> result = substitute(ty);
        if (result)
        {
            return *result;
        }
        else
        {
            module->errors.push_back(TypeError{module->scopes[0].first, UnificationTooComplex{}});
            return builtinTypes->errorRecoveryType();
        }
    }

    TypePackId cloneTypePack(TypePackId tp)
    {
        std::optional<TypePackId> result = substitute(tp);
        if (result)
        {
            return *result;
        }
        else
        {
            module->errors.push_back(TypeError{module->scopes[0].first, UnificationTooComplex{}});
            return builtinTypes->errorRecoveryTypePack();
        }
    }

    TypeFun cloneTypeFun(const TypeFun& tf)
    {
        std::vector<GenericTypeDefinition> typeParams;
        std::vector<GenericTypePackDefinition> typePackParams;

        for (GenericTypeDefinition typeParam : tf.typeParams)
        {
            TypeId ty = cloneType(typeParam.ty);
            std::optional<TypeId> defaultValue;

            if (typeParam.defaultValue)
                defaultValue = cloneType(*typeParam.defaultValue);

            typeParams.push_back(GenericTypeDefinition{ty, defaultValue});
        }

        for (GenericTypePackDefinition typePackParam : tf.typePackParams)
        {
            TypePackId tp = cloneTypePack(typePackParam.tp);
            std::optional<TypePackId> defaultValue;

            if (typePackParam.defaultValue)
                defaultValue = cloneTypePack(*typePackParam.defaultValue);

            typePackParams.push_back(GenericTypePackDefinition{tp, defaultValue});
        }

        TypeId type = cloneType(tf.type);

        return TypeFun{typeParams, typePackParams, type};
    }
};

Module::~Module()
{
    unfreeze(interfaceTypes);
    unfreeze(internalTypes);
}

void Module::clonePublicInterface(NotNull<BuiltinTypes> builtinTypes, InternalErrorReporter& ice)
{
    CloneState cloneState{builtinTypes};

    ScopePtr moduleScope = getModuleScope();

    TypePackId returnType = moduleScope->returnType;
    std::optional<TypePackId> varargPack = FFlag::DebugLuauDeferredConstraintResolution ? std::nullopt : moduleScope->varargPack;

    TxnLog log;
    ClonePublicInterface clonePublicInterface{&log, builtinTypes, this};

    returnType = clonePublicInterface.cloneTypePack(returnType);

    moduleScope->returnType = returnType;
    if (varargPack)
    {
        varargPack = clonePublicInterface.cloneTypePack(*varargPack);
        moduleScope->varargPack = varargPack;
    }

    for (auto& [name, tf] : moduleScope->exportedTypeBindings)
    {
        tf = clonePublicInterface.cloneTypeFun(tf);
    }

    for (auto& [name, ty] : declaredGlobals)
    {
        ty = clonePublicInterface.cloneType(ty);
    }

    // Copy external stuff over to Module itself
    this->returnType = moduleScope->returnType;
    this->exportedTypeBindings = moduleScope->exportedTypeBindings;
}

bool Module::hasModuleScope() const
{
    return !scopes.empty();
}

ScopePtr Module::getModuleScope() const
{
    LUAU_ASSERT(hasModuleScope());
    return scopes.front().second;
}

} // namespace Luau
