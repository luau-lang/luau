// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>

namespace Luau
{

class AstNode;

std::string toJson(AstNode* node);

} // namespace Luau
