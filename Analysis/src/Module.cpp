// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Module.h"

#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/ConstraintGraphBuilder.h"
#include "Luau/Normalize.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/Type.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/TypeReduction.h"
#include "Luau/VisitType.h"

#include <algorithm>

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAGVARIABLE(LuauClonePublicInterfaceLess2, false);
LUAU_FASTFLAG(LuauSubstitutionReentrant);
LUAU_FASTFLAG(LuauClassTypeVarsInSubstitution);
LUAU_FASTFLAG(LuauSubstitutionFixMissingFields);

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

    TypeId clean(TypeId ty) override
    {
        TypeId result = clone(ty);

        if (FunctionType* ftv = getMutable<FunctionType>(result))
            ftv->level = TypeLevel{0, 0};
        else if (TableType* ttv = getMutable<TableType>(result))
            ttv->level = TypeLevel{0, 0};

        return result;
    }

    TypePackId clean(TypePackId tp) override
    {
        return clone(tp);
    }

    TypeId cloneType(TypeId ty)
    {
        LUAU_ASSERT(FFlag::LuauSubstitutionReentrant && FFlag::LuauSubstitutionFixMissingFields);

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
        LUAU_ASSERT(FFlag::LuauSubstitutionReentrant && FFlag::LuauSubstitutionFixMissingFields);

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
        LUAU_ASSERT(FFlag::LuauSubstitutionReentrant && FFlag::LuauSubstitutionFixMissingFields);

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
    LUAU_ASSERT(interfaceTypes.types.empty());
    LUAU_ASSERT(interfaceTypes.typePacks.empty());

    CloneState cloneState;

    ScopePtr moduleScope = getModuleScope();

    TypePackId returnType = moduleScope->returnType;
    std::optional<TypePackId> varargPack = FFlag::DebugLuauDeferredConstraintResolution ? std::nullopt : moduleScope->varargPack;

    TxnLog log;
    ClonePublicInterface clonePublicInterface{&log, builtinTypes, this};

    if (FFlag::LuauClonePublicInterfaceLess2)
        returnType = clonePublicInterface.cloneTypePack(returnType);
    else
        returnType = clone(returnType, interfaceTypes, cloneState);

    moduleScope->returnType = returnType;
    if (varargPack)
    {
        if (FFlag::LuauClonePublicInterfaceLess2)
            varargPack = clonePublicInterface.cloneTypePack(*varargPack);
        else
            varargPack = clone(*varargPack, interfaceTypes, cloneState);
        moduleScope->varargPack = varargPack;
    }

    for (auto& [name, tf] : moduleScope->exportedTypeBindings)
    {
        if (FFlag::LuauClonePublicInterfaceLess2)
            tf = clonePublicInterface.cloneTypeFun(tf);
        else
            tf = clone(tf, interfaceTypes, cloneState);
    }

    for (auto& [name, ty] : declaredGlobals)
    {
        if (FFlag::LuauClonePublicInterfaceLess2)
            ty = clonePublicInterface.cloneType(ty);
        else
            ty = clone(ty, interfaceTypes, cloneState);
    }

    // Copy external stuff over to Module itself
    this->returnType = moduleScope->returnType;
    if (FFlag::DebugLuauDeferredConstraintResolution)
        this->exportedTypeBindings = moduleScope->exportedTypeBindings;
    else
        this->exportedTypeBindings = std::move(moduleScope->exportedTypeBindings);
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
