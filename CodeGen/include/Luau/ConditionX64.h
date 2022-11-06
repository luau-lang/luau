// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

namespace Luau
{
namespace CodeGen
{

enum class ConditionX64 : uint8_t
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

    Parity,
    NotParity,

    Count
};

} // namespace CodeGen
} // namespace Luau
