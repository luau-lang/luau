// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Type.h"

#include <unordered_map>

namespace Luau
{

enum class AutocompleteContext
{
    Unknown,
    Expression,
    Statement,
    Property,
    Type,
    Keyword,
    String,
    HotComment,
};

enum class AutocompleteEntryKind
{
    Property,
    Binding,
    Keyword,
    String,
    Type,
    Module,
    GeneratedFunction,
    RequirePath,
    HotComment,
};

enum class ParenthesesRecommendation
{
    None,
    CursorAfter,
    CursorInside,
};

enum class TypeCorrectKind
{
    None,
    Correct,
    CorrectFunctionResult,
};

struct AutocompleteEntry
{
    AutocompleteEntryKind kind = AutocompleteEntryKind::Property;
    // Nullopt if kind is Keyword
    std::optional<TypeId> type = std::nullopt;
    bool deprecated = false;
    // Only meaningful if kind is Property.
    bool wrongIndexType = false;
    // Set if this suggestion matches the type expected in the context
    TypeCorrectKind typeCorrect = TypeCorrectKind::None;

    std::optional<const ExternType*> containingExternType = std::nullopt;
    std::optional<const Property*> prop = std::nullopt;
    std::optional<std::string> documentationSymbol = std::nullopt;
    Tags tags;
    ParenthesesRecommendation parens = ParenthesesRecommendation::None;
    std::optional<std::string> insertText;

    // Only meaningful if kind is Property.
    bool indexedWithSelf = false;
};

using AutocompleteEntryMap = std::unordered_map<std::string, AutocompleteEntry>;
struct AutocompleteResult
{
    AutocompleteEntryMap entryMap;
    std::vector<AstNode*> ancestry;
    AutocompleteContext context = AutocompleteContext::Unknown;

    AutocompleteResult() = default;
    AutocompleteResult(AutocompleteEntryMap entryMap, std::vector<AstNode*> ancestry, AutocompleteContext context)
        : entryMap(std::move(entryMap))
        , ancestry(std::move(ancestry))
        , context(context)
    {
    }
};

using StringCompletionCallback =
    std::function<std::optional<AutocompleteEntryMap>(std::string tag, std::optional<const ExternType*> ctx, std::optional<std::string> contents)>;

constexpr char kGeneratedAnonymousFunctionEntryName[] = "function (anonymous autofilled)";

} // namespace Luau
