// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Config.h"
#include "Luau/Module.h"
#include "Luau/ModuleResolver.h"
#include "Luau/RequireTracer.h"
#include "Luau/Scope.h"
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
struct GlobalTypes;
struct TypeChecker;
struct FileResolver;
struct ModuleResolver;
struct ParseResult;
struct HotComment;

struct LoadDefinitionFileResult
{
    bool success;
    ParseResult parseResult;
    SourceModule sourceModule;
    ModulePtr module;
};

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
// TODO: Deprecate this code path when we move away from the old solver
LoadDefinitionFileResult loadDefinitionFileNoDCR(TypeChecker& typeChecker, GlobalTypes& globals, ScopePtr targetScope, std::string_view definition,
    const std::string& packageName, bool captureComments);
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

    // Run typechecking only in mode required for autocomplete (strict mode in
    // order to get more precise type information)
    bool forAutocomplete = false;

    bool runLintChecks = false;

    // If not empty, randomly shuffle the constraint set before attempting to
    // solve.  Use this value to seed the random number generator.
    std::optional<unsigned> randomizeConstraintResolutionSeed;

    std::optional<LintOptions> enabledLintWarnings;
};

struct CheckResult
{
    std::vector<TypeError> errors;

    LintResult lintResult;

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
    ScopePtr getEnvironmentScope(const std::string& environmentName) const;

    void registerBuiltinDefinition(const std::string& name, std::function<void(Frontend&, GlobalTypes&, ScopePtr)>);
    void applyBuiltinDefinitionToEnvironment(const std::string& environmentName, const std::string& definitionName);

    LoadDefinitionFileResult loadDefinitionFile(GlobalTypes& globals, ScopePtr targetScope, std::string_view source, const std::string& packageName,
        bool captureComments, bool typeCheckForAutocomplete = false);

private:
    ModulePtr check(const SourceModule& sourceModule, Mode mode, std::vector<RequireCycle> requireCycles, bool forAutocomplete = false, bool recordJsonLog = false);

    std::pair<SourceNode*, SourceModule*> getSourceNode(const ModuleName& name);
    SourceModule parse(const ModuleName& name, std::string_view src, const ParseOptions& parseOptions);

    bool parseGraph(std::vector<ModuleName>& buildQueue, const ModuleName& root, bool forAutocomplete);

    static LintResult classifyLints(const std::vector<LintWarning>& warnings, const Config& config);

    ScopePtr getModuleEnvironment(const SourceModule& module, const Config& config, bool forAutocomplete) const;

    std::unordered_map<std::string, ScopePtr> environments;
    std::unordered_map<std::string, std::function<void(Frontend&, GlobalTypes&, ScopePtr)>> builtinDefinitions;

    BuiltinTypes builtinTypes_;

public:
    const NotNull<BuiltinTypes> builtinTypes;

    FileResolver* fileResolver;
    FrontendModuleResolver moduleResolver;
    FrontendModuleResolver moduleResolverForAutocomplete;
    GlobalTypes globals;
    GlobalTypes globalsForAutocomplete;
    TypeChecker typeChecker;
    TypeChecker typeCheckerForAutocomplete;
    ConfigResolver* configResolver;
    FrontendOptions options;
    InternalErrorReporter iceHandler;

    std::unordered_map<ModuleName, SourceNode> sourceNodes;
    std::unordered_map<ModuleName, SourceModule> sourceModules;
    std::unordered_map<ModuleName, RequireTraceResult> requireTrace;

    Stats stats = {};
};

ModulePtr check(const SourceModule& sourceModule, const std::vector<RequireCycle>& requireCycles, NotNull<BuiltinTypes> builtinTypes,
    NotNull<InternalErrorReporter> iceHandler, NotNull<ModuleResolver> moduleResolver, NotNull<FileResolver> fileResolver,
    const ScopePtr& globalScope, FrontendOptions options);

ModulePtr check(const SourceModule& sourceModule, const std::vector<RequireCycle>& requireCycles, NotNull<BuiltinTypes> builtinTypes,
    NotNull<InternalErrorReporter> iceHandler, NotNull<ModuleResolver> moduleResolver, NotNull<FileResolver> fileResolver,
    const ScopePtr& globalScope, FrontendOptions options, bool recordJsonLog);

} // namespace Luau
