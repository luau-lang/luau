// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <cstdint>

namespace Luau
{

enum struct Polarity : uint8_t
{
    None = 0b000,
    Positive = 0b001,
    Negative = 0b010,
    Mixed = 0b011,
    Unknown = 0b100,
};

inline Polarity operator|(Polarity lhs, Polarity rhs)
{
    return Polarity(uint8_t(lhs) | uint8_t(rhs));
}

inline Polarity& operator|=(Polarity& lhs, Polarity rhs)
{
    lhs = lhs | rhs;
    return lhs;
}

inline Polarity operator&(Polarity lhs, Polarity rhs)
{
    return Polarity(uint8_t(lhs) & uint8_t(rhs));
}

inline Polarity& operator&=(Polarity& lhs, Polarity rhs)
{
    lhs = lhs & rhs;
    return lhs;
}

inline bool isPositive(Polarity p)
{
    return bool(p & Polarity::Positive);
}

inline bool isNegative(Polarity p)
{
    return bool(p & Polarity::Negative);
}

inline bool isKnown(Polarity p)
{
    return p != Polarity::Unknown;
}

inline Polarity invert(Polarity p)
{
    switch (p)
    {
    case Polarity::Positive:
        return Polarity::Negative;
    case Polarity::Negative:
        return Polarity::Positive;
    default:
        return p;
    }
}

} // namespace Luau
