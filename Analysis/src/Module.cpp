// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Module.h"

#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/ConstraintGraphBuilder.h"
#include "Luau/Normalize.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"

#include <algorithm>

LUAU_FASTFLAG(LuauLowerBoundsCalculation);
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAGVARIABLE(LuauForceExportSurfacesToBeNormal, false);
LUAU_FASTFLAGVARIABLE(LuauClonePublicInterfaceLess, false);
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

struct ForceNormal : TypeVarOnceVisitor
{
    const TypeArena* typeArena = nullptr;

    ForceNormal(const TypeArena* typeArena)
        : typeArena(typeArena)
    {
    }

    bool visit(TypeId ty) override
    {
        if (ty->owningArena != typeArena)
            return false;

        asMutable(ty)->normal = true;
        return true;
    }

    bool visit(TypeId ty, const FreeTypeVar& ftv) override
    {
        visit(ty);
        return true;
    }

    bool visit(TypePackId tp, const FreeTypePack& ftp) override
    {
        return true;
    }
};

struct ClonePublicInterface : Substitution
{
    NotNull<SingletonTypes> singletonTypes;
    NotNull<Module> module;

    ClonePublicInterface(const TxnLog* log, NotNull<SingletonTypes> singletonTypes, Module* module)
        : Substitution(log, &module->interfaceTypes)
        , singletonTypes(singletonTypes)
        , module(module)
    {
        LUAU_ASSERT(module);
    }

    bool isDirty(TypeId ty) override
    {
        if (ty->owningArena == &module->internalTypes)
            return true;

        if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty))
            return ftv->level.level != 0;
        if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
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

        if (FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(result))
            ftv->level = TypeLevel{0, 0};
        else if (TableTypeVar* ttv = getMutable<TableTypeVar>(result))
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
            return singletonTypes->errorRecoveryType();
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
            return singletonTypes->errorRecoveryTypePack();
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

void Module::clonePublicInterface(NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice)
{
    LUAU_ASSERT(interfaceTypes.typeVars.empty());
    LUAU_ASSERT(interfaceTypes.typePacks.empty());

    CloneState cloneState;

    ScopePtr moduleScope = getModuleScope();

    TypePackId returnType = moduleScope->returnType;
    std::optional<TypePackId> varargPack = FFlag::DebugLuauDeferredConstraintResolution ? std::nullopt : moduleScope->varargPack;
    std::unordered_map<Name, TypeFun>* exportedTypeBindings = &moduleScope->exportedTypeBindings;

    TxnLog log;
    ClonePublicInterface clonePublicInterface{&log, singletonTypes, this};

    if (FFlag::LuauClonePublicInterfaceLess)
        returnType = clonePublicInterface.cloneTypePack(returnType);
    else
        returnType = clone(returnType, interfaceTypes, cloneState);

    moduleScope->returnType = returnType;
    if (varargPack)
    {
        if (FFlag::LuauClonePublicInterfaceLess)
            varargPack = clonePublicInterface.cloneTypePack(*varargPack);
        else
            varargPack = clone(*varargPack, interfaceTypes, cloneState);
        moduleScope->varargPack = varargPack;
    }

    ForceNormal forceNormal{&interfaceTypes};

    if (FFlag::LuauLowerBoundsCalculation)
    {
        normalize(returnType, NotNull{this}, singletonTypes, ice);
        if (FFlag::LuauForceExportSurfacesToBeNormal)
            forceNormal.traverse(returnType);
        if (varargPack)
        {
            normalize(*varargPack, NotNull{this}, singletonTypes, ice);
            if (FFlag::LuauForceExportSurfacesToBeNormal)
                forceNormal.traverse(*varargPack);
        }
    }

    if (exportedTypeBindings)
    {
        for (auto& [name, tf] : *exportedTypeBindings)
        {
            if (FFlag::LuauClonePublicInterfaceLess)
                tf = clonePublicInterface.cloneTypeFun(tf);
            else
                tf = clone(tf, interfaceTypes, cloneState);
            if (FFlag::LuauLowerBoundsCalculation)
            {
                normalize(tf.type, NotNull{this}, singletonTypes, ice);

                // We're about to freeze the memory.  We know that the flag is conservative by design.  Cyclic tables
                // won't be marked normal.  If the types aren't normal by now, they never will be.
                forceNormal.traverse(tf.type);
                for (GenericTypeDefinition param : tf.typeParams)
                {
                    forceNormal.traverse(param.ty);

                    if (param.defaultValue)
                    {
                        normalize(*param.defaultValue, NotNull{this}, singletonTypes, ice);
                        forceNormal.traverse(*param.defaultValue);
                    }
                }
            }
        }
    }

    for (TypeId ty : returnType)
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
        if (FFlag::LuauClonePublicInterfaceLess)
            ty = clonePublicInterface.cloneType(ty);
        else
            ty = clone(ty, interfaceTypes, cloneState);
        if (FFlag::LuauLowerBoundsCalculation)
        {
            normalize(ty, NotNull{this}, singletonTypes, ice);

            if (FFlag::LuauForceExportSurfacesToBeNormal)
                forceNormal.traverse(ty);
        }
    }

    freeze(internalTypes);
    freeze(interfaceTypes);
}

ScopePtr Module::getModuleScope() const
{
    LUAU_ASSERT(!scopes.empty());
    return scopes.front().second;
}

} // namespace Luau
