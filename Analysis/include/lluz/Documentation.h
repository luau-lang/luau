#pragma once

#include "lluz/DenseHash.h"
#include "lluz/Variant.h"

#include <string>
#include <vector>

namespace lluz
{

struct FunctionDocumentation;
struct TableDocumentation;
struct OverloadedFunctionDocumentation;
struct BasicDocumentation;

using Documentation = lluz::Variant<BasicDocumentation, FunctionDocumentation, TableDocumentation, OverloadedFunctionDocumentation>;
using DocumentationSymbol = std::string;

struct BasicDocumentation
{
    std::string documentation;
    std::string learnMoreLink;
    std::string codeSample;
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
    std::string codeSample;
};

struct OverloadedFunctionDocumentation
{
    // This is a map of function signature to overload symbol name.
    lluz::DenseHashMap<std::string, DocumentationSymbol> overloads;
};

// Represents documentation for a table-like item, meaning "anything with keys".
// This could be a table or a class.
struct TableDocumentation
{
    std::string documentation;
    lluz::DenseHashMap<std::string, DocumentationSymbol> keys;
    std::string learnMoreLink;
    std::string codeSample;
};

using DocumentationDatabase = lluz::DenseHashMap<DocumentationSymbol, Documentation>;

} // namespace lluz
