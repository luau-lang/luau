// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>

namespace lluz
{

class AstNode;

std::string toJson(AstNode* node);

} // namespace lluz
