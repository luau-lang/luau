// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details

#include "lluz/Constraint.h"

namespace lluz
{

Constraint::Constraint(ConstraintV&& c)
    : c(std::move(c))
{
}

} // namespace lluz
