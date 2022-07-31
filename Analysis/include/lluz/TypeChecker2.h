// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "lluz/Ast.h"
#include "lluz/Module.h"

namespace lluz
{

void check(const SourceModule& sourceModule, Module* module);

} // namespace lluz
