// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

namespace Luau
{
namespace CodeGen
{
namespace A64
{

// See Table C1-1 on page C1-229 of Arm ARM for A-profile architecture
enum class ConditionA64
{
    // EQ: integer (equal), floating-point (equal)
    Equal,
    // NE: integer (not equal), floating-point (not equal or unordered)
    NotEqual,

    // CS: integer (carry set), unsigned integer (greater than, equal), floating-point (greater than, equal or unordered)
    CarrySet,
    // CC: integer (carry clear), unsigned integer (less than), floating-point (less than)
    CarryClear,

    // MI: integer (negative), floating-point (less than)
    Minus,
    // PL: integer (positive or zero), floating-point (greater than, equal or unordered)
    Plus,

    // VS: integer (overflow), floating-point (unordered)
    Overflow,
    // VC: integer (no overflow), floating-point (ordered)
    NoOverflow,

    // HI: integer (unsigned higher), floating-point (greater than, or unordered)
    UnsignedGreater,
    // LS: integer (unsigned lower or same), floating-point (less than or equal)
    UnsignedLessEqual,

    // GE: integer (signed greater than or equal), floating-point (greater than or equal)
    GreaterEqual,
    // LT: integer (signed less than), floating-point (less than, or unordered)
    Less,

    // GT: integer (signed greater than), floating-point (greater than)
    Greater,
    // LE: integer (signed less than or equal), floating-point (less than, equal or unordered)
    LessEqual,

    // AL: always
    Always,

    Count
};

} // namespace A64
} // namespace CodeGen
} // namespace Luau
