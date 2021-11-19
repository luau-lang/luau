// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <ostream>
#include <optional>

namespace std
{

inline std::ostream& operator<<(std::ostream& lhs, const std::nullopt_t&)
{
    return lhs << "none";
}

template<typename T>
auto operator<<(std::ostream& lhs, const std::optional<T>& t) -> decltype(lhs << *t) // SFINAE to only instantiate << for supported types
{
    if (t)
        return lhs << *t;
    else
        return lhs << "none";
}

} // namespace std
