// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>
#include <vector>

namespace Luau
{

class AstNode;
struct Comment;

std::string toJson(AstNode* node);
std::string toJson(AstNode* node, const std::vector<Comment>& commentLocations);

} // namespace Luau
