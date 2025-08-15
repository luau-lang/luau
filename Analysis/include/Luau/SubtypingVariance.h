// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

namespace Luau
{

enum class SubtypingVariance
{
    // Useful for an empty hash table key. Should never arise from actual code.
    Invalid,
    Covariant,
    Contravariant,
    Invariant,
};

}
