// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Location.h"
#include "Luau/TypeVar.h"

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

enum class AutocompleteEntryKind
{
    Property,
    Binding,
    Keyword,
    String,
    Type,
    Module,
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

    std::optional<const ClassTypeVar*> containingClass = std::nullopt;
    std::optional<const Property*> prop = std::nullopt;
    std::optional<std::string> documentationSymbol = std::nullopt;
    Tags tags;
    ParenthesesRecommendation parens = ParenthesesRecommendation::None;
};

using AutocompleteEntryMap = std::unordered_map<std::string, AutocompleteEntry>;
struct AutocompleteResult
{
    AutocompleteEntryMap entryMap;
    std::vector<AstNode*> ancestry;

    AutocompleteResult() = default;
    AutocompleteResult(AutocompleteEntryMap entryMap, std::vector<AstNode*> ancestry)
        : entryMap(std::move(entryMap))
        , ancestry(std::move(ancestry))
    {
    }
};

using ModuleName = std::string;
using StringCompletionCallback = std::function<std::optional<AutocompleteEntryMap>(std::string tag, std::optional<const ClassTypeVar*> ctx)>;

struct OwningAutocompleteResult
{
    AutocompleteResult result;
    ModulePtr module;
    std::unique_ptr<SourceModule> sourceModule;
};

AutocompleteResult autocomplete(Frontend& frontend, const ModuleName& moduleName, Position position, StringCompletionCallback callback);

// Deprecated, do not use in new work.
OwningAutocompleteResult autocompleteSource(Frontend& frontend, std::string_view source, Position position, StringCompletionCallback callback);

} // namespace Luau
