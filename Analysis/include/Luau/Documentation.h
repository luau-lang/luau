#pragma once

#include "Luau/DenseHash.h"
#include "Luau/Variant.h"

#include <string>
#include <vector>

namespace Luau
{

struct FunctionDocumentation;
struct TableDocumentation;
struct OverloadedFunctionDocumentation;
struct BasicDocumentation;

using Documentation = Luau::Variant<BasicDocumentation, FunctionDocumentation, TableDocumentation, OverloadedFunctionDocumentation>;
using DocumentationSymbol = std::string;

struct BasicDocumentation
{
    std::string documentation;
    std::string learnMoreLink;
};

struct FunctionParameterDocumentation
{
    std::string name;
    DocumentationSymbol documentation;
};

// Represents documentation for anything callable. This could be a method or a
// callback or a free function.
struct FunctionDocumentation
{
    std::string documentation;
    std::vector<FunctionParameterDocumentation> parameters;
    std::vector<DocumentationSymbol> returns;
    std::string learnMoreLink;
};

struct OverloadedFunctionDocumentation
{
    // This is a map of function signature to overload symbol name.
    Luau::DenseHashMap<std::string, DocumentationSymbol> overloads;
};

// Represents documentation for a table-like item, meaning "anything with keys".
// This could be a table or a class.
struct TableDocumentation
{
    std::string documentation;
    Luau::DenseHashMap<std::string, DocumentationSymbol> keys;
    std::string learnMoreLink;
};

using DocumentationDatabase = Luau::DenseHashMap<DocumentationSymbol, Documentation>;

} // namespace Luau
