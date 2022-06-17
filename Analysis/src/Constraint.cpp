// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Constraint.h"

namespace Luau
{

Constraint::Constraint(ConstraintV&& c, Location location)
    : c(std::move(c))
    , location(location)
{
}

} // namespace Luau
