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

struct FileResolver
{
    virtual ~FileResolver() {}

    /** Fetch the source code associated with the provided ModuleName.
     *
     * FIXME: This requires a string copy!
     *
     * @returns The actual Lua code on success.
     * @returns std::nullopt if no such file exists.  When this occurs, type inference will report an UnknownRequire error.
     */
    virtual std::optional<SourceCode> readSource(const ModuleName& name) = 0;

    /** Does the module exist?
     *
     * Saves a string copy over reading the source and throwing it away.
     */
    virtual bool moduleExists(const ModuleName& name) const = 0;

    virtual std::optional<ModuleName> fromAstFragment(AstExpr* expr) const = 0;

    /** Given a valid module name and a string of arbitrary data, figure out the concatenation.
     */
    virtual ModuleName concat(const ModuleName& lhs, std::string_view rhs) const = 0;

    /** Goes "up" a level in the hierarchy that the ModuleName represents.
     *
     * For instances, this is analogous to someInstance.Parent; for paths, this is equivalent to removing the last
     * element of the path. Other ModuleName representations may have other ways of doing this.
     *
     * @returns The parent ModuleName, if one exists.
     * @returns std::nullopt if there is no parent for this module name.
     */
    virtual std::optional<ModuleName> getParentModuleName(const ModuleName& name) const = 0;

    virtual std::optional<std::string> getHumanReadableModuleName_(const ModuleName& name) const
    {
        return name;
    }

    virtual std::optional<std::string> getEnvironmentForModule(const ModuleName& name) const = 0;

    /** LanguageService only:
     * std::optional<ModuleName> fromInstance(Instance* inst)
     */
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
    std::optional<std::string> getEnvironmentForModule(const ModuleName& name) const override
    {
        return std::nullopt;
    }
};

} // namespace Luau
