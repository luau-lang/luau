// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Location.h"
#include "Luau/Type.h"

#include <unordered_map>
#include <string>
#include <memory>
#include <optional>

namespace Luau
{

struct Frontend;
struct SourceModule;
struct Module;
struct TypeChecker;

using ModulePtr = std::shared_ptr<Module>;

enum class AutocompleteContext
{
    Unknown,
    Expression,
    Statement,
    Property,
    Type,
    Keyword,
    String,
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

    std::optional<const ClassType*> containingClass = std::nullopt;
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

using ModuleName = std::string;
using StringCompletionCallback =
    std::function<std::optional<AutocompleteEntryMap>(std::string tag, std::optional<const ClassType*> ctx, std::optional<std::string> contents)>;

AutocompleteResult autocomplete(Frontend& frontend, const ModuleName& moduleName, Position position, StringCompletionCallback callback);

constexpr char kGeneratedAnonymousFunctionEntryName[] = "function (anonymous autofilled)";

} // namespace Luau
