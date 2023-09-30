// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/NonStrictTypeChecker.h"

#include "Luau/Type.h"
#include "Luau/Subtyping.h"
#include "Luau/Normalize.h"
#include "Luau/Error.h"
#include "Luau/TypeArena.h"
#include "Luau/Def.h"

namespace Luau
{

struct NonStrictContext
{
    std::unordered_map<DefId, TypeId> context;

    NonStrictContext() = default;

    NonStrictContext(const NonStrictContext&) = delete;
    NonStrictContext& operator=(const NonStrictContext&) = delete;

    NonStrictContext(NonStrictContext&&) = default;
    NonStrictContext& operator=(NonStrictContext&&) = default;

    void unionContexts(const NonStrictContext& other)
    {
        // TODO: unimplemented
    }

    void intersectContexts(const NonStrictContext& other)
    {
        // TODO: unimplemented
    }

    void removeFromContext(const std::vector<DefId>& defs)
    {
        // TODO: unimplemented
    }

    std::optional<TypeId> find(const DefId& def)
    {
        // TODO: unimplemented
        return {};
    }

    // Satisfies means that for a given DefId n, and an actual type t for `n`, t satisfies the context if t <: context[n]
    // ice if the DefId is not in the context
    bool satisfies(const DefId& def, TypeId inferredType)
    {
        // TODO: unimplemented
        return false;
    }

    bool willRunTimeError(const DefId& def, TypeId inferredType)
    {
        return satisfies(def, inferredType);
    }
};

struct NonStrictTypeChecker
{

    NotNull<BuiltinTypes> builtinTypes;
    const NotNull<InternalErrorReporter> ice;
    TypeArena arena;
    Module* module;
    Normalizer normalizer;
    Subtyping subtyping;


    NonStrictTypeChecker(NotNull<BuiltinTypes> builtinTypes, Subtyping subtyping, const NotNull<InternalErrorReporter> ice,
        NotNull<UnifierSharedState> unifierState, Module* module)
        : builtinTypes(builtinTypes)
        , ice(ice)
        , module(module)
        , normalizer{&arena, builtinTypes, unifierState, /* cache inhabitance */ true}
        , subtyping{builtinTypes, NotNull{&arena}, NotNull(&normalizer), ice, NotNull{module->getModuleScope().get()}}
    {
    }
};

void checkNonStrict(NotNull<BuiltinTypes> builtinTypes, Module* module)
{
    // TODO: unimplemented
}
} // namespace Luau
