// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Location.h"
#include "Luau/ParseOptions.h"

#include <string>

namespace Luau
{
class AstNode;
class AstStatBlock;

struct TranspileResult
{
    std::string code;
    Location errorLocation;
    std::string parseError; // Nonempty if the transpile failed
};

std::string toString(AstNode* node);
void dump(AstNode* node);

// Never fails on a well-formed AST
std::string transpile(AstStatBlock& ast);
std::string transpileWithTypes(AstStatBlock& block);

// Only fails when parsing fails
TranspileResult transpile(std::string_view source, ParseOptions options = ParseOptions{}, bool withTypes = false);

} // namespace Luau
