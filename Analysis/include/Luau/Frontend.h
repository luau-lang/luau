// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Config.h"
#include "Luau/Module.h"
#include "Luau/ModuleResolver.h"
#include "Luau/RequireTracer.h"
#include "Luau/TypeInfer.h"
#include "Luau/Variant.h"

#include <string>
#include <vector>
#include <optional>

namespace Luau
{

class AstStat;
class ParseError;
struct Frontend;
struct TypeError;
struct LintWarning;
struct TypeChecker;
struct FileResolver;
struct ModuleResolver;
struct ParseResult;
struct HotComment;

struct LoadDefinitionFileResult
{
    bool success;
    ParseResult parseResult;
    ModulePtr module;
};

LoadDefinitionFileResult loadDefinitionFile(
    TypeChecker& typeChecker, ScopePtr targetScope, std::string_view definition, const std::string& packageName);

std::optional<Mode> parseMode(const std::vector<HotComment>& hotcomments);

std::vector<std::string_view> parsePathExpr(const AstExpr& pathExpr);

// Exported only for convenient testing.
std::optional<ModuleName> pathExprToModuleName(const ModuleName& currentModuleName, const std::vector<std::string_view>& expr);

/** Try to convert an AST fragment into a ModuleName.
 * Returns std::nullopt if the expression cannot be resolved.  This will most likely happen in cases where
 * the import path involves some dynamic computation that we cannot see into at typechecking time.
 *
 * Unintuitively, weirdly-formulated modules (like game.Parent.Parent.Parent.Foo) will successfully produce a ModuleName
 * as long as it falls within the permitted syntax.  This is ok because we will fail to find the module and produce an
 * error when we try during typechecking.
 */
std::optional<ModuleName> pathExprToModuleName(const ModuleName& currentModuleName, const AstExpr& expr);

struct SourceNode
{
    bool hasDirtySourceModule() const
    {
        return dirtySourceModule;
    }

    bool hasDirtyModule(bool forAutocomplete) const
    {
        return forAutocomplete ? dirtyModuleForAutocomplete : dirtyModule;
    }

    ModuleName name;
    std::unordered_set<ModuleName> requireSet;
    std::vector<std::pair<ModuleName, Location>> requireLocations;
    bool dirtySourceModule = true;
    bool dirtyModule = true;
    bool dirtyModuleForAutocomplete = true;
    double autocompleteLimitsMult = 1.0;
};

struct FrontendOptions
{
    // When true, we retain full type information about every term in the AST.
    // Setting this to false cuts back on RAM and is a good idea for batch
    // jobs where the type graph is not deeply inspected after typechecking
    // is complete.
    bool retainFullTypeGraphs = false;

    // Run typechecking only in mode required for autocomplete (strict mode in order to get more precise type information)
    bool forAutocomplete = false;
};

struct CheckResult
{
    std::vector<TypeError> errors;
    std::vector<ModuleName> timeoutHits;
};

struct FrontendModuleResolver : ModuleResolver
{
    FrontendModuleResolver(Frontend* frontend);

    const ModulePtr getModule(const ModuleName& moduleName) const override;
    bool moduleExists(const ModuleName& moduleName) const override;
    std::optional<ModuleInfo> resolveModuleInfo(const ModuleName& currentModuleName, const AstExpr& pathExpr) override;
    std::string getHumanReadableModuleName(const ModuleName& moduleName) const override;

    Frontend* frontend;
    std::unordered_map<ModuleName, ModulePtr> modules;
};

struct Frontend
{
    struct Stats
    {
        size_t files = 0;
        size_t lines = 0;

        size_t filesStrict = 0;
        size_t filesNonstrict = 0;

        double timeRead = 0;
        double timeParse = 0;
        double timeCheck = 0;
        double timeLint = 0;
    };

    Frontend(FileResolver* fileResolver, ConfigResolver* configResolver, const FrontendOptions& options = {});

    CheckResult check(const ModuleName& name, std::optional<FrontendOptions> optionOverride = {}); // new shininess
    LintResult lint(const ModuleName& name, std::optional<LintOptions> enabledLintWarnings = {});

    /** Lint some code that has no associated DataModel object
     *
     * Since this source fragment has no name, we cannot cache its AST.  Instead,
     * we return it to the caller to use as they wish.
     */
    std::pair<SourceModule, LintResult> lintFragment(std::string_view source, std::optional<LintOptions> enabledLintWarnings = {});

    LintResult lint(const SourceModule& module, std::optional<LintOptions> enabledLintWarnings = {});

    bool isDirty(const ModuleName& name, bool forAutocomplete = false) const;
    void markDirty(const ModuleName& name, std::vector<ModuleName>* markedDirty = nullptr);

    /** Borrow a pointer into the SourceModule cache.
     *
     * Returns nullptr if we don't have it.  This could mean that the script
     * doesn't exist, or simply that its contents have changed since the previous
     * check, in which case we do not have its AST.
     *
     * IMPORTANT: this pointer is only valid until the next call to markDirty.  Do not retain it.
     */
    SourceModule* getSourceModule(const ModuleName& name);
    const SourceModule* getSourceModule(const ModuleName& name) const;

    void clearStats();
    void clear();

    ScopePtr addEnvironment(const std::string& environmentName);
    ScopePtr getEnvironmentScope(const std::string& environmentName);

    void registerBuiltinDefinition(const std::string& name, std::function<void(TypeChecker&, ScopePtr)>);
    void applyBuiltinDefinitionToEnvironment(const std::string& environmentName, const std::string& definitionName);

private:
    ModulePtr check(const SourceModule& sourceModule, Mode mode, const ScopePtr& environmentScope);

    std::pair<SourceNode*, SourceModule*> getSourceNode(CheckResult& checkResult, const ModuleName& name);
    SourceModule parse(const ModuleName& name, std::string_view src, const ParseOptions& parseOptions);

    bool parseGraph(std::vector<ModuleName>& buildQueue, CheckResult& checkResult, const ModuleName& root, bool forAutocomplete);

    static LintResult classifyLints(const std::vector<LintWarning>& warnings, const Config& config);

    ScopePtr getModuleEnvironment(const SourceModule& module, const Config& config);

    std::unordered_map<std::string, ScopePtr> environments;
    std::unordered_map<std::string, std::function<void(TypeChecker&, ScopePtr)>> builtinDefinitions;

public:
    FileResolver* fileResolver;
    FrontendModuleResolver moduleResolver;
    FrontendModuleResolver moduleResolverForAutocomplete;
    TypeChecker typeChecker;
    TypeChecker typeCheckerForAutocomplete;
    ConfigResolver* configResolver;
    FrontendOptions options;
    InternalErrorReporter iceHandler;
    TypeArena arenaForAutocomplete;

    std::unordered_map<ModuleName, SourceNode> sourceNodes;
    std::unordered_map<ModuleName, SourceModule> sourceModules;
    std::unordered_map<ModuleName, RequireTraceResult> requireTrace;

    Stats stats = {};
};

} // namespace Luau
