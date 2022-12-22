// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"

#include <stdint.h>

namespace Luau
{
namespace CodeGen
{

enum class KindA64 : uint8_t
{
    none,
    w, // 32-bit GPR
    x, // 64-bit GPR
};

struct RegisterA64
{
    KindA64 kind : 3;
    uint8_t index : 5;

    constexpr bool operator==(RegisterA64 rhs) const
    {
        return kind == rhs.kind && index == rhs.index;
    }

    constexpr bool operator!=(RegisterA64 rhs) const
    {
        return !(*this == rhs);
    }
};

constexpr RegisterA64 w0{KindA64::w, 0};
constexpr RegisterA64 w1{KindA64::w, 1};
constexpr RegisterA64 w2{KindA64::w, 2};
constexpr RegisterA64 w3{KindA64::w, 3};
constexpr RegisterA64 w4{KindA64::w, 4};
constexpr RegisterA64 w5{KindA64::w, 5};
constexpr RegisterA64 w6{KindA64::w, 6};
constexpr RegisterA64 w7{KindA64::w, 7};
constexpr RegisterA64 w8{KindA64::w, 8};
constexpr RegisterA64 w9{KindA64::w, 9};
constexpr RegisterA64 w10{KindA64::w, 10};
constexpr RegisterA64 w11{KindA64::w, 11};
constexpr RegisterA64 w12{KindA64::w, 12};
constexpr RegisterA64 w13{KindA64::w, 13};
constexpr RegisterA64 w14{KindA64::w, 14};
constexpr RegisterA64 w15{KindA64::w, 15};
constexpr RegisterA64 w16{KindA64::w, 16};
constexpr RegisterA64 w17{KindA64::w, 17};
constexpr RegisterA64 w18{KindA64::w, 18};
constexpr RegisterA64 w19{KindA64::w, 19};
constexpr RegisterA64 w20{KindA64::w, 20};
constexpr RegisterA64 w21{KindA64::w, 21};
constexpr RegisterA64 w22{KindA64::w, 22};
constexpr RegisterA64 w23{KindA64::w, 23};
constexpr RegisterA64 w24{KindA64::w, 24};
constexpr RegisterA64 w25{KindA64::w, 25};
constexpr RegisterA64 w26{KindA64::w, 26};
constexpr RegisterA64 w27{KindA64::w, 27};
constexpr RegisterA64 w28{KindA64::w, 28};
constexpr RegisterA64 w29{KindA64::w, 29};
constexpr RegisterA64 w30{KindA64::w, 30};
constexpr RegisterA64 wzr{KindA64::w, 31};

constexpr RegisterA64 x0{KindA64::x, 0};
constexpr RegisterA64 x1{KindA64::x, 1};
constexpr RegisterA64 x2{KindA64::x, 2};
constexpr RegisterA64 x3{KindA64::x, 3};
constexpr RegisterA64 x4{KindA64::x, 4};
constexpr RegisterA64 x5{KindA64::x, 5};
constexpr RegisterA64 x6{KindA64::x, 6};
constexpr RegisterA64 x7{KindA64::x, 7};
constexpr RegisterA64 x8{KindA64::x, 8};
constexpr RegisterA64 x9{KindA64::x, 9};
constexpr RegisterA64 x10{KindA64::x, 10};
constexpr RegisterA64 x11{KindA64::x, 11};
constexpr RegisterA64 x12{KindA64::x, 12};
constexpr RegisterA64 x13{KindA64::x, 13};
constexpr RegisterA64 x14{KindA64::x, 14};
constexpr RegisterA64 x15{KindA64::x, 15};
constexpr RegisterA64 x16{KindA64::x, 16};
constexpr RegisterA64 x17{KindA64::x, 17};
constexpr RegisterA64 x18{KindA64::x, 18};
constexpr RegisterA64 x19{KindA64::x, 19};
constexpr RegisterA64 x20{KindA64::x, 20};
constexpr RegisterA64 x21{KindA64::x, 21};
constexpr RegisterA64 x22{KindA64::x, 22};
constexpr RegisterA64 x23{KindA64::x, 23};
constexpr RegisterA64 x24{KindA64::x, 24};
constexpr RegisterA64 x25{KindA64::x, 25};
constexpr RegisterA64 x26{KindA64::x, 26};
constexpr RegisterA64 x27{KindA64::x, 27};
constexpr RegisterA64 x28{KindA64::x, 28};
constexpr RegisterA64 x29{KindA64::x, 29};
constexpr RegisterA64 x30{KindA64::x, 30};
constexpr RegisterA64 xzr{KindA64::x, 31};

constexpr RegisterA64 sp{KindA64::none, 31};

} // namespace CodeGen
} // namespace Luau
