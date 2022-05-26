// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

namespace Luau
{
namespace CodeGen
{

enum class Condition
{
    Overflow,
    NoOverflow,

    Carry,
    NoCarry,

    Below,
    BelowEqual,
    Above,
    AboveEqual,
    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    NotBelow,
    NotBelowEqual,
    NotAbove,
    NotAboveEqual,
    NotEqual,
    NotLess,
    NotLessEqual,
    NotGreater,
    NotGreaterEqual,

    Zero,
    NotZero,

    // TODO: ordered and unordered floating-point conditions

    Count
};

} // namespace CodeGen
} // namespace Luau
