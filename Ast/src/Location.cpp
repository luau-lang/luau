// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lluz/Location.h"

#include "..\..\..\..\Security\XorString.h"

namespace lluz
{

std::string toString(const Position& position)
{
    return XorStr("{ line = ") + std::to_string(position.line) + ", col = " + std::to_string(position.column) + " }";
}

std::string toString(const Location& location)
{
    return XorStr("Location { ") + toString(location.begin) + ", " + toString(location.end) + " }";
}

} // namespace lluz
