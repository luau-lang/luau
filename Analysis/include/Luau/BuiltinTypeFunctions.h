// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeFunction.h"

namespace Luau
{

struct BuiltinTypeFunctions
{
    BuiltinTypeFunctions();
    BuiltinTypeFunctions(const BuiltinTypeFunctions&) = delete;
    void operator=(const BuiltinTypeFunctions&) = delete;

    TypeFunction userFunc;

    TypeFunction notFunc;
    TypeFunction lenFunc;
    TypeFunction unmFunc;

    TypeFunction addFunc;
    TypeFunction subFunc;
    TypeFunction mulFunc;
    TypeFunction divFunc;
    TypeFunction idivFunc;
    TypeFunction powFunc;
    TypeFunction modFunc;

    TypeFunction concatFunc;

    TypeFunction andFunc;
    TypeFunction orFunc;

    TypeFunction ltFunc;
    TypeFunction leFunc;
    TypeFunction eqFunc;

    TypeFunction refineFunc;
    TypeFunction singletonFunc;
    TypeFunction unionFunc;
    TypeFunction intersectFunc;

    TypeFunction keyofFunc;
    TypeFunction rawkeyofFunc;
    TypeFunction indexFunc;
    TypeFunction rawgetFunc;

    TypeFunction setmetatableFunc;
    TypeFunction getmetatableFunc;

    TypeFunction weakoptionalFunc;

    void addToScope(NotNull<TypeArena> arena, NotNull<Scope> scope) const;
};

const BuiltinTypeFunctions& builtinTypeFunctions_DEPRECATED();

} // namespace Luau
