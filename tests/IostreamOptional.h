// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <ostream>

inline std::ostream& operator<<(std::ostream& lhs, const std::nullopt_t&)
{
    return lhs << "none";
}

template<typename T>
std::ostream& operator<<(std::ostream& lhs, const std::optional<T>& t)
{
    if (t)
        return lhs << *t;
    else
        return lhs << "none";
}
