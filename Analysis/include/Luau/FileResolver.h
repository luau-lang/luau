// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace Luau
{

class AstExpr;

using ModuleName = std::string;

struct SourceCode
{
    enum Type
    {
        None,
        Module,
        Script,
    };

    std::string source;
    Type type;
};

struct ModuleInfo
{
    ModuleName name;
    bool optional = false;
};

struct RequireAlias
{
    std::string alias; // Unprefixed alias name (no leading `@`).
    std::vector<std::string> tags = {};
};

struct RequireNode
{
    virtual ~RequireNode() {}

    // Get the path component representing this node.
    virtual std::string getPathComponent() const = 0;

    // Get the displayed user-facing label for this node, defaults to getPathComponent()
    virtual std::string getLabel() const
    {
        return getPathComponent();
    }

    // Get tags to attach to this node's RequireSuggestion (defaults to none).
    virtual std::vector<std::string> getTags() const
    {
        return {};
    }

    // TODO: resolvePathToNode() can ultimately be replaced with a call into
    // require-by-string's path resolution algorithm. This will first require
    // generalizing that algorithm to work with a virtual file system.
    virtual std::unique_ptr<RequireNode> resolvePathToNode(const std::string& path) const = 0;

    // Get children of this node, if any (if this node represents a directory).
    virtual std::vector<std::unique_ptr<RequireNode>> getChildren() const = 0;

    // A list of the aliases available to this node.
    virtual std::vector<RequireAlias> getAvailableAliases() const = 0;
};

struct RequireSuggestion
{
    std::string label;
    std::string fullPath;
    std::vector<std::string> tags;
};
using RequireSuggestions = std::vector<RequireSuggestion>;

struct RequireSuggester
{
    virtual ~RequireSuggester() {}
    std::optional<RequireSuggestions> getRequireSuggestions(const ModuleName& requirer, const std::optional<std::string>& pathString) const;

protected:
    virtual std::unique_ptr<RequireNode> getNode(const ModuleName& name) const = 0;

private:
    std::optional<RequireSuggestions> getRequireSuggestionsImpl(const ModuleName& requirer, const std::optional<std::string>& path) const;
};

struct FileResolver
{
    FileResolver() = default;
    FileResolver(std::shared_ptr<RequireSuggester> requireSuggester)
        : requireSuggester(std::move(requireSuggester))
    {
    }

    virtual ~FileResolver() {}

    virtual std::optional<SourceCode> readSource(const ModuleName& name) = 0;

    virtual std::optional<ModuleInfo> resolveModule(const ModuleInfo* context, AstExpr* expr)
    {
        return std::nullopt;
    }

    virtual std::string getHumanReadableModuleName(const ModuleName& name) const
    {
        return name;
    }

    virtual std::optional<std::string> getEnvironmentForModule(const ModuleName& name) const
    {
        return std::nullopt;
    }

    std::optional<RequireSuggestions> getRequireSuggestions(const ModuleName& requirer, const std::optional<std::string>& pathString) const;

    std::shared_ptr<RequireSuggester> requireSuggester;
};

struct NullFileResolver : FileResolver
{
    std::optional<SourceCode> readSource(const ModuleName& name) override
    {
        return std::nullopt;
    }
};

} // namespace Luau
