// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

namespace Luau
{

enum class SubtypingVariance
{
    // Used for an empty key. Should never appear in actual code.
    Invalid,
    Covariant,
    // This is used to identify cases where we have a covariant + a
    // contravariant reason and we need to merge them.
    Contravariant,
    Invariant,
};

}
