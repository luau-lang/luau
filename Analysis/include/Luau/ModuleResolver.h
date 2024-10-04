// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/FileResolver.h"

#include <memory>
#include <optional>
#include <string>

namespace Luau
{

class AstExpr;
struct Module;

using ModulePtr = std::shared_ptr<Module>;

struct ModuleResolver
{
    virtual ~ModuleResolver() {}

    /** Compute a ModuleName from an AST fragment.  This AST fragment is generally the argument to the require() function.
     *
     * @returns The ModuleInfo if the expression is a syntactically legal path.
     * @returns std::nullopt if we are unable to determine whether or not the expression is a valid path.  Type inference will
     * silently assume that it could succeed in this case.
     *
     * FIXME: This is clearly not the right behaviour longterm.  We'll want to adust this interface to be able to signal
     *  a) success,
     *  b) Definitive failure (this expression will absolutely cause require() to fail at runtime), and
     *  c) uncertainty
     */
    virtual std::optional<ModuleInfo> resolveModuleInfo(const ModuleName& currentModuleName, const AstExpr& pathExpr) = 0;

    /** Get a typechecked module from its name.
     *
     * This can return null under two circumstances: the module is unknown at compile time,
     * or there's a cycle, and we are still in the middle of typechecking the module.
     */
    virtual const ModulePtr getModule(const ModuleName& moduleName) const = 0;

    /** Is a module known at compile time?
     *
     * This function can be used to distinguish the above two cases.
     */
    virtual bool moduleExists(const ModuleName& moduleName) const = 0;

    virtual std::string getHumanReadableModuleName(const ModuleName& moduleName) const = 0;
};

struct NullModuleResolver : ModuleResolver
{
    std::optional<ModuleInfo> resolveModuleInfo(const ModuleName& currentModuleName, const AstExpr& pathExpr) override
    {
        return std::nullopt;
    }
    const ModulePtr getModule(const ModuleName& moduleName) const override
    {
        return nullptr;
    }
    bool moduleExists(const ModuleName& moduleName) const override
    {
        return false;
    }
    std::string getHumanReadableModuleName(const ModuleName& moduleName) const override
    {
        return moduleName;
    }
};

} // namespace Luau
