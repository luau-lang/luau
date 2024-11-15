// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>

namespace Luau::EqSat
{

struct Id final
{
    explicit Id(uint32_t id);

    explicit operator uint32_t() const;

    bool operator==(Id rhs) const;
    bool operator!=(Id rhs) const;

    bool operator<(Id rhs) const;

private:
    uint32_t id;
};

} // namespace Luau::EqSat

template<>
struct std::hash<Luau::EqSat::Id>
{
    size_t operator()(Luau::EqSat::Id id) const;
};
