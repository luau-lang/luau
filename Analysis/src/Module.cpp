// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Module.h"

#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/Normalize.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"

#include <algorithm>

LUAU_FASTFLAG(LuauLowerBoundsCalculation);
LUAU_FASTFLAG(LuauNormalizeFlagIsConservative);

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

Module::~Module()
{
    unfreeze(interfaceTypes);
    unfreeze(internalTypes);
}

void Module::clonePublicInterface(InternalErrorReporter& ice)
{
    LUAU_ASSERT(interfaceTypes.typeVars.empty());
    LUAU_ASSERT(interfaceTypes.typePacks.empty());

    CloneState cloneState;

    ScopePtr moduleScope = getModuleScope();

    moduleScope->returnType = clone(moduleScope->returnType, interfaceTypes, cloneState);
    if (moduleScope->varargPack)
        moduleScope->varargPack = clone(*moduleScope->varargPack, interfaceTypes, cloneState);

    if (FFlag::LuauLowerBoundsCalculation)
    {
        normalize(moduleScope->returnType, interfaceTypes, ice);
        if (moduleScope->varargPack)
            normalize(*moduleScope->varargPack, interfaceTypes, ice);
    }

    ForceNormal forceNormal{&interfaceTypes};

    for (auto& [name, tf] : moduleScope->exportedTypeBindings)
    {
        tf = clone(tf, interfaceTypes, cloneState);
        if (FFlag::LuauLowerBoundsCalculation)
        {
            normalize(tf.type, interfaceTypes, ice);

            if (FFlag::LuauNormalizeFlagIsConservative)
            {
                // We're about to freeze the memory.  We know that the flag is conservative by design.  Cyclic tables
                // won't be marked normal.  If the types aren't normal by now, they never will be.
                forceNormal.traverse(tf.type);
            }
        }
    }

    for (TypeId ty : moduleScope->returnType)
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
        if (FFlag::LuauLowerBoundsCalculation)
            normalize(ty, interfaceTypes, ice);
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
