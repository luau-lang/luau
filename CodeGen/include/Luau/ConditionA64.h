// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

namespace Luau
{
namespace CodeGen
{
namespace A64
{

enum class ConditionA64
{
    Equal,
    NotEqual,

    CarrySet,
    CarryClear,

    Minus,
    Plus,

    Overflow,
    NoOverflow,

    UnsignedGreater,
    UnsignedLessEqual,

    GreaterEqual,
    Less,
    Greater,
    LessEqual,

    Always,

    Count
};

} // namespace A64
} // namespace CodeGen
} // namespace Luau
