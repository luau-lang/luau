// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/RegisterA64.h"

namespace Luau
{
namespace CodeGen
{

enum class AddressKindA64 : uint8_t
{
    imm, // reg + imm
    reg, // reg + reg

    // TODO:
    // reg + reg << shift
    // reg + sext(reg) << shift
    // reg + uext(reg) << shift
};

struct AddressA64
{
    AddressA64(RegisterA64 base, int off = 0)
        : kind(AddressKindA64::imm)
        , base(base)
        , offset(xzr)
        , data(off)
    {
        LUAU_ASSERT(base.kind == KindA64::x || base == sp);
        LUAU_ASSERT(off >= -256 && off < 4096);
    }

    AddressA64(RegisterA64 base, RegisterA64 offset)
        : kind(AddressKindA64::reg)
        , base(base)
        , offset(offset)
        , data(0)
    {
        LUAU_ASSERT(base.kind == KindA64::x);
        LUAU_ASSERT(offset.kind == KindA64::x);
    }

    AddressKindA64 kind;
    RegisterA64 base;
    RegisterA64 offset;
    int data;
};

using mem = AddressA64;

} // namespace CodeGen
} // namespace Luau
