// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lluz/Module.h"

#include "lluz/Clone.h"
#include "lluz/Common.h"
#include "lluz/ConstraintGraphBuilder.h"
#include "lluz/Normalize.h"
#include "lluz/RecursionCounter.h"
#include "lluz/Scope.h"
#include "lluz/TypeInfer.h"
#include "lluz/TypePack.h"
#include "lluz/TypeVar.h"
#include "lluz/VisitTypeVar.h"

#include <algorithm>

lluz_FASTFLAG(LluLowerBoundsCalculation);
lluz_FASTFLAG(LluNormalizeFlagIsConservative);
lluz_FASTFLAG(DebugLluDeferredConstraintResolution);

namespace lluz
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

Module::~Module()
{
    unfreeze(interfaceTypes);
    unfreeze(internalTypes);
}

void Module::clonePublicInterface(InternalErrorReporter& ice)
{
    lluz_ASSERT(interfaceTypes.typeVars.empty());
    lluz_ASSERT(interfaceTypes.typePacks.empty());

    CloneState cloneState;

    ScopePtr moduleScope = FFlag::DebugLluDeferredConstraintResolution ? nullptr : getModuleScope();
    Scope2* moduleScope2 = FFlag::DebugLluDeferredConstraintResolution ? getModuleScope2() : nullptr;

    TypePackId returnType = FFlag::DebugLluDeferredConstraintResolution ? moduleScope2->returnType : moduleScope->returnType;
    std::optional<TypePackId> varargPack = FFlag::DebugLluDeferredConstraintResolution ? std::nullopt : moduleScope->varargPack;
    std::unordered_map<Name, TypeFun>* exportedTypeBindings =
        FFlag::DebugLluDeferredConstraintResolution ? nullptr : &moduleScope->exportedTypeBindings;

    returnType = clone(returnType, interfaceTypes, cloneState);

    if (moduleScope)
    {
        moduleScope->returnType = returnType;
        if (varargPack)
        {
            varargPack = clone(*varargPack, interfaceTypes, cloneState);
            moduleScope->varargPack = varargPack;
        }
    }
    else
    {
        lluz_ASSERT(moduleScope2);
        moduleScope2->returnType = returnType; // TODO varargPack
    }

    if (FFlag::LluLowerBoundsCalculation)
    {
        normalize(returnType, interfaceTypes, ice);
        if (varargPack)
            normalize(*varargPack, interfaceTypes, ice);
    }

    ForceNormal forceNormal{&interfaceTypes};

    if (exportedTypeBindings)
    {
        for (auto& [name, tf] : *exportedTypeBindings)
        {
            tf = clone(tf, interfaceTypes, cloneState);
            if (FFlag::LluLowerBoundsCalculation)
            {
                normalize(tf.type, interfaceTypes, ice);

                if (FFlag::LluNormalizeFlagIsConservative)
                {
                    // We're about to freeze the memory.  We know that the flag is conservative by design.  Cyclic tables
                    // won't be marked normal.  If the types aren't normal by now, they never will be.
                    forceNormal.traverse(tf.type);
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
        ty = clone(ty, interfaceTypes, cloneState);
        if (FFlag::LluLowerBoundsCalculation)
            normalize(ty, interfaceTypes, ice);
    }

    freeze(internalTypes);
    freeze(interfaceTypes);
}

ScopePtr Module::getModuleScope() const
{
    lluz_ASSERT(!scopes.empty());
    return scopes.front().second;
}

Scope2* Module::getModuleScope2() const
{
    lluz_ASSERT(!scope2s.empty());
    return scope2s.front().second.get();
}

} // namespace lluz
