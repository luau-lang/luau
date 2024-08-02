// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/NotNull.h"
#include "Luau/Substitution.h"
#include "Luau/TypeFwd.h"

#include <memory>

namespace Luau
{

struct TypeArena;
struct Scope;
struct InternalErrorReporter;
using ScopePtr = std::shared_ptr<Scope>;

// A substitution which replaces free types by any
struct Anyification : Substitution
{
    Anyification(
        TypeArena* arena,
        NotNull<Scope> scope,
        NotNull<BuiltinTypes> builtinTypes,
        InternalErrorReporter* iceHandler,
        TypeId anyType,
        TypePackId anyTypePack
    );
    Anyification(
        TypeArena* arena,
        const ScopePtr& scope,
        NotNull<BuiltinTypes> builtinTypes,
        InternalErrorReporter* iceHandler,
        TypeId anyType,
        TypePackId anyTypePack
    );
    NotNull<Scope> scope;
    NotNull<BuiltinTypes> builtinTypes;
    InternalErrorReporter* iceHandler;

    TypeId anyType;
    TypePackId anyTypePack;
    bool normalizationTooComplex = false;
    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId tp) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId tp) override;

    bool ignoreChildren(TypeId ty) override;
    bool ignoreChildren(TypePackId ty) override;
};

} // namespace Luau
