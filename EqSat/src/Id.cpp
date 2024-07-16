// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Id.h"

namespace Luau::EqSat
{

Id::Id(size_t id)
    : id(id)
{
}

Id::operator size_t() const
{
    return id;
}

bool Id::operator==(Id rhs) const
{
    return id == rhs.id;
}

bool Id::operator!=(Id rhs) const
{
    return id != rhs.id;
}

} // namespace Luau::EqSat

size_t std::hash<Luau::EqSat::Id>::operator()(Luau::EqSat::Id id) const
{
    return std::hash<size_t>()(size_t(id));
}
