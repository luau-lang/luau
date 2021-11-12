// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>
#include <optional>

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
        Local
    };

    std::string source;
    Type type;
};

struct ModuleInfo
{
    ModuleName name;
    bool optional = false;
};

struct FileResolver
{
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

    // DEPRECATED APIS
    // These are going to be removed with LuauNewRequireTrace2
    virtual bool moduleExists(const ModuleName& name) const = 0;
    virtual std::optional<ModuleName> fromAstFragment(AstExpr* expr) const = 0;
    virtual ModuleName concat(const ModuleName& lhs, std::string_view rhs) const = 0;
    virtual std::optional<ModuleName> getParentModuleName(const ModuleName& name) const = 0;
};

struct NullFileResolver : FileResolver
{
    std::optional<SourceCode> readSource(const ModuleName& name) override
    {
        return std::nullopt;
    }
    bool moduleExists(const ModuleName& name) const override
    {
        return false;
    }
    std::optional<ModuleName> fromAstFragment(AstExpr* expr) const override
    {
        return std::nullopt;
    }
    ModuleName concat(const ModuleName& lhs, std::string_view rhs) const override
    {
        return lhs;
    }
    std::optional<ModuleName> getParentModuleName(const ModuleName& name) const override
    {
        return std::nullopt;
    }
};

} // namespace Luau
