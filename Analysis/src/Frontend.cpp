// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Frontend.h"

#include "Luau/Common.h"
#include "Luau/Clone.h"
#include "Luau/Config.h"
#include "Luau/FileResolver.h"
#include "Luau/Parser.h"
#include "Luau/Scope.h"
#include "Luau/StringUtils.h"
#include "Luau/TimeTrace.h"
#include "Luau/TypeInfer.h"
#include "Luau/Variant.h"

#include <algorithm>
#include <chrono>
#include <stdexcept>

LUAU_FASTINT(LuauTypeInferIterationLimit)
LUAU_FASTINT(LuauTarjanChildLimit)
LUAU_FASTFLAG(LuauCyclicModuleTypeSurface)
LUAU_FASTFLAG(LuauInferInNoCheckMode)
LUAU_FASTFLAGVARIABLE(LuauKnowsTheDataModel3, false)
LUAU_FASTFLAGVARIABLE(LuauSeparateTypechecks, false)
LUAU_FASTFLAGVARIABLE(LuauAutocompleteDynamicLimits, false)
LUAU_FASTFLAGVARIABLE(LuauDirtySourceModule, false)
LUAU_FASTINTVARIABLE(LuauAutocompleteCheckTimeoutMs, 0)

namespace Luau
{

std::optional<Mode> parseMode(const std::vector<HotComment>& hotcomments)
{
    for (const HotComment& hc : hotcomments)
    {
        if (!hc.header)
            continue;

        if (hc.content == "nocheck")
            return Mode::NoCheck;

        if (hc.content == "nonstrict")
            return Mode::Nonstrict;

        if (hc.content == "strict")
            return Mode::Strict;
    }

    return std::nullopt;
}

static void generateDocumentationSymbols(TypeId ty, const std::string& rootName)
{
    // TODO: What do we do in this situation? This means that the definition
    // file is exporting a type that is also a persistent type.
    if (ty->persistent)
    {
        return;
    }

    asMutable(ty)->documentationSymbol = rootName;

    if (TableTypeVar* ttv = getMutable<TableTypeVar>(ty))
    {
        for (auto& [name, prop] : ttv->props)
        {
            prop.documentationSymbol = rootName + "." + name;
        }
    }
    else if (ClassTypeVar* ctv = getMutable<ClassTypeVar>(ty))
    {
        for (auto& [name, prop] : ctv->props)
        {
            prop.documentationSymbol = rootName + "." + name;
        }
    }
}

LoadDefinitionFileResult loadDefinitionFile(TypeChecker& typeChecker, ScopePtr targetScope, std::string_view source, const std::string& packageName)
{
    LUAU_TIMETRACE_SCOPE("loadDefinitionFile", "Frontend");

    Luau::Allocator allocator;
    Luau::AstNameTable names(allocator);

    ParseOptions options;
    options.allowDeclarationSyntax = true;

    Luau::ParseResult parseResult = Luau::Parser::parse(source.data(), source.size(), names, allocator, options);

    if (parseResult.errors.size() > 0)
        return LoadDefinitionFileResult{false, parseResult, nullptr};

    Luau::SourceModule module;
    module.root = parseResult.root;
    module.mode = Mode::Definition;

    ModulePtr checkedModule = typeChecker.check(module, Mode::Definition);

    if (checkedModule->errors.size() > 0)
        return LoadDefinitionFileResult{false, parseResult, checkedModule};

    CloneState cloneState;

    for (const auto& [name, ty] : checkedModule->declaredGlobals)
    {
        TypeId globalTy = clone(ty, typeChecker.globalTypes, cloneState);
        std::string documentationSymbol = packageName + "/global/" + name;
        generateDocumentationSymbols(globalTy, documentationSymbol);
        targetScope->bindings[typeChecker.globalNames.names->getOrAdd(name.c_str())] = {globalTy, Location(), false, {}, documentationSymbol};

        persist(globalTy);
    }

    for (const auto& [name, ty] : checkedModule->getModuleScope()->exportedTypeBindings)
    {
        TypeFun globalTy = clone(ty, typeChecker.globalTypes, cloneState);
        std::string documentationSymbol = packageName + "/globaltype/" + name;
        generateDocumentationSymbols(globalTy.type, documentationSymbol);
        targetScope->exportedTypeBindings[name] = globalTy;

        persist(globalTy.type);
    }

    return LoadDefinitionFileResult{true, parseResult, checkedModule};
}

std::vector<std::string_view> parsePathExpr(const AstExpr& pathExpr)
{
    const AstExprIndexName* indexName = pathExpr.as<AstExprIndexName>();
    if (!indexName)
        return {};

    std::vector<std::string_view> segments{indexName->index.value};

    while (true)
    {
        if (AstExprIndexName* in = indexName->expr->as<AstExprIndexName>())
        {
            segments.push_back(in->index.value);
            indexName = in;
            continue;
        }
        else if (AstExprGlobal* indexNameAsGlobal = indexName->expr->as<AstExprGlobal>())
        {
            segments.push_back(indexNameAsGlobal->name.value);
            break;
        }
        else if (AstExprLocal* indexNameAsLocal = indexName->expr->as<AstExprLocal>())
        {
            segments.push_back(indexNameAsLocal->local->name.value);
            break;
        }
        else
            return {};
    }

    std::reverse(segments.begin(), segments.end());
    return segments;
}

std::optional<std::string> pathExprToModuleName(const ModuleName& currentModuleName, const std::vector<std::string_view>& segments)
{
    if (segments.empty())
        return std::nullopt;

    std::vector<std::string_view> result;

    auto it = segments.begin();

    if (*it == "script" && !currentModuleName.empty())
    {
        result = split(currentModuleName, '/');
        ++it;
    }

    for (; it != segments.end(); ++it)
    {
        if (result.size() > 1 && *it == "Parent")
            result.pop_back();
        else
            result.push_back(*it);
    }

    return join(result, "/");
}

std::optional<std::string> pathExprToModuleName(const ModuleName& currentModuleName, const AstExpr& pathExpr)
{
    std::vector<std::string_view> segments = parsePathExpr(pathExpr);
    return pathExprToModuleName(currentModuleName, segments);
}

namespace
{

ErrorVec accumulateErrors(
    const std::unordered_map<ModuleName, SourceNode>& sourceNodes, const std::unordered_map<ModuleName, ModulePtr>& modules, const ModuleName& name)
{
    std::unordered_set<ModuleName> seen;
    std::vector<ModuleName> queue{name};

    ErrorVec result;

    while (!queue.empty())
    {
        ModuleName next = std::move(queue.back());
        queue.pop_back();

        if (seen.count(next))
            continue;
        seen.insert(next);

        auto it = sourceNodes.find(next);
        if (it == sourceNodes.end())
            continue;

        const SourceNode& sourceNode = it->second;
        queue.insert(queue.end(), sourceNode.requires.begin(), sourceNode.requires.end());

        // FIXME: If a module has a syntax error, we won't be able to re-report it here.
        // The solution is probably to move errors from Module to SourceNode

        auto it2 = modules.find(next);
        if (it2 == modules.end())
            continue;

        Module& module = *it2->second;

        std::sort(module.errors.begin(), module.errors.end(), [](const TypeError& e1, const TypeError& e2) -> bool {
            return e1.location.begin > e2.location.begin;
        });

        result.insert(result.end(), module.errors.begin(), module.errors.end());
    }

    std::reverse(result.begin(), result.end());

    return result;
}

// Given a source node (start), find all requires that start a transitive dependency path that ends back at start
// For each such path, record the full path and the location of the require in the starting module.
// Note that this is O(V^2) for a fully connected graph and produces O(V) paths of length O(V)
// However, when the graph is acyclic, this is O(V), as well as when only the first cycle is needed (stopAtFirst=true)
std::vector<RequireCycle> getRequireCycles(
    const FileResolver* resolver, const std::unordered_map<ModuleName, SourceNode>& sourceNodes, const SourceNode* start, bool stopAtFirst = false)
{
    std::vector<RequireCycle> result;

    DenseHashSet<const SourceNode*> seen(nullptr);
    std::vector<const SourceNode*> stack;
    std::vector<const SourceNode*> path;

    for (const auto& [depName, depLocation] : start->requireLocations)
    {
        std::vector<ModuleName> cycle;

        auto dit = sourceNodes.find(depName);
        if (dit == sourceNodes.end())
            continue;

        stack.push_back(&dit->second);

        while (!stack.empty())
        {
            const SourceNode* top = stack.back();
            stack.pop_back();

            if (top == nullptr)
            {
                // special marker for post-order processing
                LUAU_ASSERT(!path.empty());
                top = path.back();
                path.pop_back();

                // we reached the node! path must form a cycle now
                if (top == start)
                {
                    for (const SourceNode* node : path)
                        cycle.push_back(resolver->getHumanReadableModuleName(node->name));

                    cycle.push_back(resolver->getHumanReadableModuleName(top->name));
                    break;
                }
            }
            else if (!seen.contains(top))
            {
                seen.insert(top);

                // push marker for post-order processing
                path.push_back(top);
                stack.push_back(nullptr);

                // note: we push require edges in the opposite order
                // because it's a stack, the last edge to be pushed gets processed first
                // this ensures that the cyclic path we report is the first one in DFS order
                for (size_t i = top->requireLocations.size(); i > 0; --i)
                {
                    const ModuleName& reqName = top->requireLocations[i - 1].first;

                    auto rit = sourceNodes.find(reqName);
                    if (rit != sourceNodes.end())
                        stack.push_back(&rit->second);
                }
            }
        }

        path.clear();
        stack.clear();

        if (!cycle.empty())
        {
            result.push_back({depLocation, std::move(cycle)});

            if (stopAtFirst)
                return result;

            // note: if we didn't find a cycle, all nodes that we've seen don't depend [transitively] on start
            // so it's safe to *only* clear seen vector when we find a cycle
            // if we don't do it, we will not have correct reporting for some cycles
            seen.clear();
        }
    }

    return result;
}

double getTimestamp()
{
    using namespace std::chrono;
    return double(duration_cast<nanoseconds>(high_resolution_clock::now().time_since_epoch()).count()) / 1e9;
}

} // namespace

Frontend::Frontend(FileResolver* fileResolver, ConfigResolver* configResolver, const FrontendOptions& options)
    : fileResolver(fileResolver)
    , moduleResolver(this)
    , moduleResolverForAutocomplete(this)
    , typeChecker(&moduleResolver, &iceHandler)
    , typeCheckerForAutocomplete(&moduleResolverForAutocomplete, &iceHandler)
    , configResolver(configResolver)
    , options(options)
{
}

FrontendModuleResolver::FrontendModuleResolver(Frontend* frontend)
    : frontend(frontend)
{
}

CheckResult Frontend::check(const ModuleName& name, std::optional<FrontendOptions> optionOverride)
{
    LUAU_TIMETRACE_SCOPE("Frontend::check", "Frontend");
    LUAU_TIMETRACE_ARGUMENT("name", name.c_str());

    FrontendOptions frontendOptions = optionOverride.value_or(options);
    CheckResult checkResult;

    auto it = sourceNodes.find(name);
    if (it != sourceNodes.end() && !it->second.hasDirtyModule(frontendOptions.forAutocomplete))
    {
        // No recheck required.
        if (FFlag::LuauSeparateTypechecks)
        {
            if (frontendOptions.forAutocomplete)
            {
                auto it2 = moduleResolverForAutocomplete.modules.find(name);
                if (it2 == moduleResolverForAutocomplete.modules.end() || it2->second == nullptr)
                    throw std::runtime_error("Frontend::modules does not have data for " + name);
            }
            else
            {
                auto it2 = moduleResolver.modules.find(name);
                if (it2 == moduleResolver.modules.end() || it2->second == nullptr)
                    throw std::runtime_error("Frontend::modules does not have data for " + name);
            }

            return CheckResult{accumulateErrors(
                sourceNodes, frontendOptions.forAutocomplete ? moduleResolverForAutocomplete.modules : moduleResolver.modules, name)};
        }
        else
        {
            auto it2 = moduleResolver.modules.find(name);
            if (it2 == moduleResolver.modules.end() || it2->second == nullptr)
                throw std::runtime_error("Frontend::modules does not have data for " + name);

            return CheckResult{accumulateErrors(sourceNodes, moduleResolver.modules, name)};
        }
    }

    std::vector<ModuleName> buildQueue;
    bool cycleDetected = parseGraph(buildQueue, checkResult, name, frontendOptions.forAutocomplete);

    // Keep track of which AST nodes we've reported cycles in
    std::unordered_set<AstNode*> reportedCycles;

    double autocompleteTimeLimit = FInt::LuauAutocompleteCheckTimeoutMs / 1000.0;

    for (const ModuleName& moduleName : buildQueue)
    {
        LUAU_ASSERT(sourceNodes.count(moduleName));
        SourceNode& sourceNode = sourceNodes[moduleName];

        if (!sourceNode.hasDirtyModule(frontendOptions.forAutocomplete))
            continue;

        LUAU_ASSERT(sourceModules.count(moduleName));
        SourceModule& sourceModule = sourceModules[moduleName];

        const Config& config = configResolver->getConfig(moduleName);

        Mode mode = sourceModule.mode.value_or(config.mode);

        ScopePtr environmentScope = getModuleEnvironment(sourceModule, config);

        double timestamp = getTimestamp();

        std::vector<RequireCycle> requireCycles;

        // in NoCheck mode we only need to compute the value of .cyclic for typeck
        // in the future we could replace toposort with an algorithm that can flag cyclic nodes by itself
        // however, for now getRequireCycles isn't expensive in practice on the cases we care about, and long term
        // all correct programs must be acyclic so this code triggers rarely
        if (cycleDetected)
            requireCycles = getRequireCycles(fileResolver, sourceNodes, &sourceNode, mode == Mode::NoCheck);

        // This is used by the type checker to replace the resulting type of cyclic modules with any
        sourceModule.cyclic = !requireCycles.empty();

        if (FFlag::LuauSeparateTypechecks && frontendOptions.forAutocomplete)
        {
            // The autocomplete typecheck is always in strict mode with DM awareness
            // to provide better type information for IDE features
            if (FFlag::LuauCyclicModuleTypeSurface)
                typeCheckerForAutocomplete.requireCycles = requireCycles;

            if (autocompleteTimeLimit != 0.0)
                typeCheckerForAutocomplete.finishTime = TimeTrace::getClock() + autocompleteTimeLimit;
            else
                typeCheckerForAutocomplete.finishTime = std::nullopt;

            if (FFlag::LuauAutocompleteDynamicLimits)
            {
                // TODO: This is a dirty ad hoc solution for autocomplete timeouts
                // We are trying to dynamically adjust our existing limits to lower total typechecking time under the limit
                // so that we'll have type information for the whole file at lower quality instead of a full abort in the middle
                if (FInt::LuauTarjanChildLimit > 0)
                    typeCheckerForAutocomplete.instantiationChildLimit =
                        std::max(1, int(FInt::LuauTarjanChildLimit * sourceNode.autocompleteLimitsMult));
                else
                    typeCheckerForAutocomplete.instantiationChildLimit = std::nullopt;

                if (FInt::LuauTypeInferIterationLimit > 0)
                    typeCheckerForAutocomplete.unifierIterationLimit =
                        std::max(1, int(FInt::LuauTypeInferIterationLimit * sourceNode.autocompleteLimitsMult));
                else
                    typeCheckerForAutocomplete.unifierIterationLimit = std::nullopt;
            }

            ModulePtr moduleForAutocomplete = typeCheckerForAutocomplete.check(sourceModule, Mode::Strict);
            moduleResolverForAutocomplete.modules[moduleName] = moduleForAutocomplete;

            double duration = getTimestamp() - timestamp;

            if (moduleForAutocomplete->timeout)
            {
                checkResult.timeoutHits.push_back(moduleName);

                if (FFlag::LuauAutocompleteDynamicLimits)
                    sourceNode.autocompleteLimitsMult = sourceNode.autocompleteLimitsMult / 2.0;
            }
            else if (FFlag::LuauAutocompleteDynamicLimits && duration < autocompleteTimeLimit / 2.0)
            {
                sourceNode.autocompleteLimitsMult = std::min(sourceNode.autocompleteLimitsMult * 2.0, 1.0);
            }

            stats.timeCheck += duration;
            stats.filesStrict += 1;

            sourceNode.dirtyModuleForAutocomplete = false;
            continue;
        }

        if (FFlag::LuauCyclicModuleTypeSurface)
            typeChecker.requireCycles = requireCycles;

        ModulePtr module = typeChecker.check(sourceModule, mode, environmentScope);

        // If we're typechecking twice, we do so.
        // The second typecheck is always in strict mode with DM awareness
        // to provide better typen information for IDE features.
        if (!FFlag::LuauSeparateTypechecks && frontendOptions.typecheckTwice_DEPRECATED)
        {
            if (FFlag::LuauCyclicModuleTypeSurface)
                typeCheckerForAutocomplete.requireCycles = requireCycles;

            ModulePtr moduleForAutocomplete = typeCheckerForAutocomplete.check(sourceModule, Mode::Strict);
            moduleResolverForAutocomplete.modules[moduleName] = moduleForAutocomplete;
        }

        stats.timeCheck += getTimestamp() - timestamp;
        stats.filesStrict += mode == Mode::Strict;
        stats.filesNonstrict += mode == Mode::Nonstrict;

        if (module == nullptr)
            throw std::runtime_error("Frontend::check produced a nullptr module for " + moduleName);

        if (!frontendOptions.retainFullTypeGraphs)
        {
            // copyErrors needs to allocate into interfaceTypes as it copies
            // types out of internalTypes, so we unfreeze it here.
            unfreeze(module->interfaceTypes);
            copyErrors(module->errors, module->interfaceTypes);
            freeze(module->interfaceTypes);

            module->internalTypes.clear();
            module->astTypes.clear();
            module->astExpectedTypes.clear();
            module->astOriginalCallTypes.clear();
            module->scopes.resize(1);
        }

        if (mode != Mode::NoCheck)
        {
            for (const RequireCycle& cyc : requireCycles)
            {
                TypeError te{cyc.location, moduleName, ModuleHasCyclicDependency{cyc.path}};

                module->errors.push_back(te);
            }
        }

        ErrorVec parseErrors;

        for (const ParseError& pe : sourceModule.parseErrors)
            parseErrors.push_back(TypeError{pe.getLocation(), moduleName, SyntaxError{pe.what()}});

        module->errors.insert(module->errors.begin(), parseErrors.begin(), parseErrors.end());

        checkResult.errors.insert(checkResult.errors.end(), module->errors.begin(), module->errors.end());

        moduleResolver.modules[moduleName] = std::move(module);
        sourceNode.dirtyModule = false;
    }

    return checkResult;
}

bool Frontend::parseGraph(std::vector<ModuleName>& buildQueue, CheckResult& checkResult, const ModuleName& root, bool forAutocomplete)
{
    LUAU_TIMETRACE_SCOPE("Frontend::parseGraph", "Frontend");
    LUAU_TIMETRACE_ARGUMENT("root", root.c_str());

    // https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search
    enum Mark
    {
        None,
        Temporary,
        Permanent
    };

    DenseHashMap<SourceNode*, Mark> seen(nullptr);
    std::vector<SourceNode*> stack;
    std::vector<SourceNode*> path;
    bool cyclic = false;

    {
        auto [sourceNode, _] = getSourceNode(checkResult, root, forAutocomplete);
        if (sourceNode)
            stack.push_back(sourceNode);
    }

    while (!stack.empty())
    {
        SourceNode* top = stack.back();
        stack.pop_back();

        if (top == nullptr)
        {
            // special marker for post-order processing
            LUAU_ASSERT(!path.empty());

            top = path.back();
            path.pop_back();

            // note: topseen ref gets invalidated in any seen[] access, beware - only one seen[] access per iteration!
            Mark& topseen = seen[top];
            LUAU_ASSERT(topseen == Temporary);
            topseen = Permanent;

            buildQueue.push_back(top->name);
        }
        else
        {
            // note: topseen ref gets invalidated in any seen[] access, beware - only one seen[] access per iteration!
            Mark& topseen = seen[top];

            if (topseen != None)
            {
                cyclic |= topseen == Temporary;
                continue;
            }

            topseen = Temporary;

            // push marker for post-order processing
            stack.push_back(nullptr);
            path.push_back(top);

            // push children
            for (const ModuleName& dep : top->requires)
            {
                auto it = sourceNodes.find(dep);
                if (it != sourceNodes.end())
                {
                    // this is a critical optimization: we do *not* traverse non-dirty subtrees.
                    // this relies on the fact that markDirty marks reverse-dependencies dirty as well
                    // thus if a node is not dirty, all its transitive deps aren't dirty, which means that they won't ever need
                    // to be built, *and* can't form a cycle with any nodes we did process.
                    if (!it->second.hasDirtyModule(forAutocomplete))
                        continue;

                    // note: this check is technically redundant *except* that getSourceNode has somewhat broken memoization
                    // calling getSourceNode twice in succession will reparse the file, since getSourceNode leaves dirty flag set
                    if (seen.contains(&it->second))
                    {
                        stack.push_back(&it->second);
                        continue;
                    }
                }

                auto [sourceNode, _] = getSourceNode(checkResult, dep, forAutocomplete);
                if (sourceNode)
                {
                    stack.push_back(sourceNode);

                    // note: this assignment is paired with .contains() check above and effectively deduplicates getSourceNode()
                    seen[sourceNode] = None;
                }
            }
        }
    }

    return cyclic;
}

ScopePtr Frontend::getModuleEnvironment(const SourceModule& module, const Config& config)
{
    ScopePtr result = typeChecker.globalScope;

    if (module.environmentName)
        result = getEnvironmentScope(*module.environmentName);

    if (!config.globals.empty())
    {
        result = std::make_shared<Scope>(result);

        for (const std::string& global : config.globals)
        {
            AstName name = module.names->get(global.c_str());

            if (name.value)
                result->bindings[name].typeId = typeChecker.anyType;
        }
    }

    return result;
}

LintResult Frontend::lint(const ModuleName& name, std::optional<Luau::LintOptions> enabledLintWarnings)
{
    LUAU_TIMETRACE_SCOPE("Frontend::lint", "Frontend");
    LUAU_TIMETRACE_ARGUMENT("name", name.c_str());

    CheckResult checkResult;
    auto [_sourceNode, sourceModule] = getSourceNode(checkResult, name, false);

    if (!sourceModule)
        return LintResult{}; // FIXME: We really should do something a bit more obvious when a file is too broken to lint.

    return lint(*sourceModule, enabledLintWarnings);
}

std::pair<SourceModule, LintResult> Frontend::lintFragment(std::string_view source, std::optional<Luau::LintOptions> enabledLintWarnings)
{
    LUAU_TIMETRACE_SCOPE("Frontend::lintFragment", "Frontend");

    const Config& config = configResolver->getConfig("");

    SourceModule sourceModule = parse(ModuleName{}, source, config.parseOptions);

    uint64_t ignoreLints = LintWarning::parseMask(sourceModule.hotcomments);

    Luau::LintOptions lintOptions = enabledLintWarnings.value_or(config.enabledLint);
    lintOptions.warningMask &= ~ignoreLints;

    double timestamp = getTimestamp();

    std::vector<LintWarning> warnings = Luau::lint(sourceModule.root, *sourceModule.names.get(), typeChecker.globalScope, nullptr,
        sourceModule.hotcomments, enabledLintWarnings.value_or(config.enabledLint));

    stats.timeLint += getTimestamp() - timestamp;

    return {std::move(sourceModule), classifyLints(warnings, config)};
}

CheckResult Frontend::check(const SourceModule& module)
{
    LUAU_TIMETRACE_SCOPE("Frontend::check", "Frontend");
    LUAU_TIMETRACE_ARGUMENT("module", module.name.c_str());

    const Config& config = configResolver->getConfig(module.name);

    Mode mode = module.mode.value_or(config.mode);

    double timestamp = getTimestamp();

    ModulePtr checkedModule = typeChecker.check(module, mode);

    stats.timeCheck += getTimestamp() - timestamp;
    stats.filesStrict += mode == Mode::Strict;
    stats.filesNonstrict += mode == Mode::Nonstrict;

    if (checkedModule == nullptr)
        throw std::runtime_error("Frontend::check produced a nullptr module for module " + module.name);
    moduleResolver.modules[module.name] = checkedModule;

    return CheckResult{checkedModule->errors};
}

LintResult Frontend::lint(const SourceModule& module, std::optional<Luau::LintOptions> enabledLintWarnings)
{
    LUAU_TIMETRACE_SCOPE("Frontend::lint", "Frontend");
    LUAU_TIMETRACE_ARGUMENT("module", module.name.c_str());

    const Config& config = configResolver->getConfig(module.name);

    uint64_t ignoreLints = LintWarning::parseMask(module.hotcomments);

    LintOptions options = enabledLintWarnings.value_or(config.enabledLint);
    options.warningMask &= ~ignoreLints;

    Mode mode = module.mode.value_or(config.mode);
    if (mode != Mode::NoCheck)
    {
        options.disableWarning(Luau::LintWarning::Code_UnknownGlobal);
    }

    if (mode == Mode::Strict)
    {
        options.disableWarning(Luau::LintWarning::Code_ImplicitReturn);
    }

    ScopePtr environmentScope = getModuleEnvironment(module, config);

    ModulePtr modulePtr = moduleResolver.getModule(module.name);

    double timestamp = getTimestamp();

    std::vector<LintWarning> warnings = Luau::lint(module.root, *module.names, environmentScope, modulePtr.get(), module.hotcomments, options);

    stats.timeLint += getTimestamp() - timestamp;

    return classifyLints(warnings, config);
}

bool Frontend::isDirty(const ModuleName& name, bool forAutocomplete) const
{
    auto it = sourceNodes.find(name);
    return it == sourceNodes.end() || it->second.hasDirtyModule(forAutocomplete);
}

/*
 * Mark a file as requiring rechecking before its type information can be safely used again.
 *
 * I am not particularly pleased with the way each dirty() operation involves a BFS on reverse dependencies.
 * It would be nice for this function to be O(1)
 */
void Frontend::markDirty(const ModuleName& name, std::vector<ModuleName>* markedDirty)
{
    if (FFlag::LuauSeparateTypechecks)
    {
        if (!moduleResolver.modules.count(name) && !moduleResolverForAutocomplete.modules.count(name))
            return;
    }
    else
    {
        if (!moduleResolver.modules.count(name))
            return;
    }

    std::unordered_map<ModuleName, std::vector<ModuleName>> reverseDeps;
    for (const auto& module : sourceNodes)
    {
        for (const auto& dep : module.second.requires)
            reverseDeps[dep].push_back(module.first);
    }

    std::vector<ModuleName> queue{name};

    while (!queue.empty())
    {
        ModuleName next = std::move(queue.back());
        queue.pop_back();

        LUAU_ASSERT(sourceNodes.count(next) > 0);
        SourceNode& sourceNode = sourceNodes[next];

        if (markedDirty)
            markedDirty->push_back(next);

        if (FFlag::LuauDirtySourceModule)
        {
            LUAU_ASSERT(FFlag::LuauSeparateTypechecks);

            if (sourceNode.dirtySourceModule && sourceNode.dirtyModule && sourceNode.dirtyModuleForAutocomplete)
                continue;

            sourceNode.dirtySourceModule = true;
            sourceNode.dirtyModule = true;
            sourceNode.dirtyModuleForAutocomplete = true;
        }
        else if (FFlag::LuauSeparateTypechecks)
        {
            if (sourceNode.dirtyModule && sourceNode.dirtyModuleForAutocomplete)
                continue;

            sourceNode.dirtyModule = true;
            sourceNode.dirtyModuleForAutocomplete = true;
        }
        else
        {
            if (sourceNode.dirtyModule)
                continue;

            sourceNode.dirtyModule = true;
        }

        if (0 == reverseDeps.count(name))
            continue;

        sourceModules.erase(name);

        const std::vector<ModuleName>& dependents = reverseDeps[name];
        queue.insert(queue.end(), dependents.begin(), dependents.end());
    }
}

SourceModule* Frontend::getSourceModule(const ModuleName& moduleName)
{
    auto it = sourceModules.find(moduleName);
    if (it != sourceModules.end())
        return &it->second;
    else
        return nullptr;
}

const SourceModule* Frontend::getSourceModule(const ModuleName& moduleName) const
{
    return const_cast<Frontend*>(this)->getSourceModule(moduleName);
}

// Read AST into sourceModules if necessary.  Trace require()s.  Report parse errors.
std::pair<SourceNode*, SourceModule*> Frontend::getSourceNode(CheckResult& checkResult, const ModuleName& name, bool forAutocomplete_DEPRECATED)
{
    LUAU_TIMETRACE_SCOPE("Frontend::getSourceNode", "Frontend");
    LUAU_TIMETRACE_ARGUMENT("name", name.c_str());

    auto it = sourceNodes.find(name);
    if (it != sourceNodes.end() &&
        (FFlag::LuauDirtySourceModule ? !it->second.hasDirtySourceModule() : !it->second.hasDirtyModule(forAutocomplete_DEPRECATED)))
    {
        auto moduleIt = sourceModules.find(name);
        if (moduleIt != sourceModules.end())
            return {&it->second, &moduleIt->second};
        else
        {
            LUAU_ASSERT(!"Everything in sourceNodes should also be in sourceModules");
            return {&it->second, nullptr};
        }
    }

    double timestamp = getTimestamp();

    std::optional<SourceCode> source = fileResolver->readSource(name);
    std::optional<std::string> environmentName = fileResolver->getEnvironmentForModule(name);

    stats.timeRead += getTimestamp() - timestamp;

    if (!source)
    {
        sourceModules.erase(name);
        return {nullptr, nullptr};
    }

    const Config& config = configResolver->getConfig(name);
    ParseOptions opts = config.parseOptions;
    opts.captureComments = true;
    SourceModule result = parse(name, source->source, opts);
    result.type = source->type;

    RequireTraceResult& requireTrace = requires[name];
    requireTrace = traceRequires(fileResolver, result.root, name);

    SourceNode& sourceNode = sourceNodes[name];
    SourceModule& sourceModule = sourceModules[name];

    sourceModule = std::move(result);
    sourceModule.environmentName = environmentName;

    sourceNode.name = name;
    sourceNode.requires.clear();
    sourceNode.requireLocations.clear();

    if (FFlag::LuauDirtySourceModule)
        sourceNode.dirtySourceModule = false;

    if (FFlag::LuauSeparateTypechecks)
    {
        if (it == sourceNodes.end())
        {
            sourceNode.dirtyModule = true;
            sourceNode.dirtyModuleForAutocomplete = true;
        }
    }
    else
    {
        sourceNode.dirtyModule = true;
    }

    for (const auto& [moduleName, location] : requireTrace.requires)
        sourceNode.requires.insert(moduleName);

    sourceNode.requireLocations = requireTrace.requires;

    return {&sourceNode, &sourceModule};
}

/** Try to parse a source file into a SourceModule.
 *
 * The logic here is a little bit more complicated than we'd like it to be.
 *
 * If a file does not exist, we return none to prevent the Frontend from creating knowledge that this module exists.
 * If the Frontend thinks that the file exists, it will not produce an "Unknown require" error.
 *
 * If the file has syntax errors, we report them and synthesize an empty AST if it's not available.
 * This suppresses the Unknown require error and allows us to make a best effort to typecheck code that require()s
 * something that has broken syntax.
 * We also translate Luau::ParseError into a Luau::TypeError so that we can use a vector<TypeError> to describe the
 * result of the check()
 */
SourceModule Frontend::parse(const ModuleName& name, std::string_view src, const ParseOptions& parseOptions)
{
    LUAU_TIMETRACE_SCOPE("Frontend::parse", "Frontend");
    LUAU_TIMETRACE_ARGUMENT("name", name.c_str());

    SourceModule sourceModule;

    double timestamp = getTimestamp();

    auto parseResult = Luau::Parser::parse(src.data(), src.size(), *sourceModule.names, *sourceModule.allocator, parseOptions);

    stats.timeParse += getTimestamp() - timestamp;
    stats.files++;
    stats.lines += std::count(src.begin(), src.end(), '\n') + (src.size() && src.back() != '\n');

    if (!parseResult.errors.empty())
        sourceModule.parseErrors.insert(sourceModule.parseErrors.end(), parseResult.errors.begin(), parseResult.errors.end());

    if (parseResult.errors.empty() || parseResult.root)
    {
        sourceModule.root = parseResult.root;
        sourceModule.mode = parseMode(parseResult.hotcomments);
    }
    else
    {
        sourceModule.root = sourceModule.allocator->alloc<AstStatBlock>(Location{}, AstArray<AstStat*>{nullptr, 0});
        sourceModule.mode = Mode::NoCheck;
    }

    sourceModule.name = name;

    if (parseOptions.captureComments)
    {
        sourceModule.commentLocations = std::move(parseResult.commentLocations);
        sourceModule.hotcomments = std::move(parseResult.hotcomments);
    }

    return sourceModule;
}

std::optional<ModuleInfo> FrontendModuleResolver::resolveModuleInfo(const ModuleName& currentModuleName, const AstExpr& pathExpr)
{
    // FIXME I think this can be pushed into the FileResolver.
    auto it = frontend->requires.find(currentModuleName);
    if (it == frontend->requires.end())
    {
        // CLI-43699
        // If we can't find the current module name, that's because we bypassed the frontend's initializer
        // and called typeChecker.check directly. (This is done by autocompleteSource, for example).
        // In that case, requires will always fail.
        return std::nullopt;
    }

    const auto& exprs = it->second.exprs;

    const ModuleInfo* info = exprs.find(&pathExpr);
    if (!info)
        return std::nullopt;

    return *info;
}

const ModulePtr FrontendModuleResolver::getModule(const ModuleName& moduleName) const
{
    auto it = modules.find(moduleName);
    if (it != modules.end())
        return it->second;
    else
        return nullptr;
}

bool FrontendModuleResolver::moduleExists(const ModuleName& moduleName) const
{
    return frontend->sourceNodes.count(moduleName) != 0;
}

std::string FrontendModuleResolver::getHumanReadableModuleName(const ModuleName& moduleName) const
{
    return frontend->fileResolver->getHumanReadableModuleName(moduleName);
}

ScopePtr Frontend::addEnvironment(const std::string& environmentName)
{
    LUAU_ASSERT(environments.count(environmentName) == 0);

    if (environments.count(environmentName) == 0)
    {
        ScopePtr scope = std::make_shared<Scope>(typeChecker.globalScope);
        environments[environmentName] = scope;
        return scope;
    }
    else
        return environments[environmentName];
}

ScopePtr Frontend::getEnvironmentScope(const std::string& environmentName)
{
    LUAU_ASSERT(environments.count(environmentName) > 0);

    return environments[environmentName];
}

void Frontend::registerBuiltinDefinition(const std::string& name, std::function<void(TypeChecker&, ScopePtr)> applicator)
{
    LUAU_ASSERT(builtinDefinitions.count(name) == 0);

    if (builtinDefinitions.count(name) == 0)
        builtinDefinitions[name] = applicator;
}

void Frontend::applyBuiltinDefinitionToEnvironment(const std::string& environmentName, const std::string& definitionName)
{
    LUAU_ASSERT(builtinDefinitions.count(definitionName) > 0);

    if (builtinDefinitions.count(definitionName) > 0)
        builtinDefinitions[definitionName](typeChecker, getEnvironmentScope(environmentName));
}

LintResult Frontend::classifyLints(const std::vector<LintWarning>& warnings, const Config& config)
{
    LintResult result;
    for (const auto& w : warnings)
    {
        if (config.lintErrors || config.fatalLint.isEnabled(w.code))
            result.errors.push_back(w);
        else
            result.warnings.push_back(w);
    }

    return result;
}

void Frontend::clearStats()
{
    stats = {};
}

void Frontend::clear()
{
    sourceNodes.clear();
    sourceModules.clear();
    moduleResolver.modules.clear();
    moduleResolverForAutocomplete.modules.clear();
    requires.clear();
}

} // namespace Luau
