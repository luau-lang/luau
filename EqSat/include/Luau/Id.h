// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

namespace Luau::EqSat
{

struct Id final
{
    explicit Id(size_t id);

    explicit operator size_t() const;

    bool operator==(Id rhs) const;
    bool operator!=(Id rhs) const;

private:
    size_t id;
};

} // namespace Luau::EqSat
