// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Frontend.h"

#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/Clone.h"
#include "Luau/Config.h"
#include "Luau/ConstraintGenerator.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/DataFlowGraph.h"
#include "Luau/DcrLogger.h"
#include "Luau/EqSatSimplification.h"
#include "Luau/ExpectedTypeVisitor.h"
#include "Luau/FileResolver.h"
#include "Luau/NonStrictTypeChecker.h"
#include "Luau/NotNull.h"
#include "Luau/Parser.h"
#include "Luau/Scope.h"
#include "Luau/TimeTrace.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeChecker2.h"
#include "Luau/TypeInfer.h"
#include "Luau/VisitType.h"

#include <algorithm>
#include <chrono>
#include <condition_variable>
#include <exception>
#include <mutex>
#include <string>

LUAU_FASTINT(LuauTypeInferIterationLimit)
LUAU_FASTINT(LuauTypeInferRecursionLimit)
LUAU_FASTINT(LuauTarjanChildLimit)
LUAU_FASTFLAG(LuauInferInNoCheckMode)
LUAU_FASTFLAGVARIABLE(LuauKnowsTheDataModel3)
LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAGVARIABLE(DebugLuauLogSolverToJson)
LUAU_FASTFLAGVARIABLE(DebugLuauLogSolverToJsonFile)
LUAU_FASTFLAGVARIABLE(DebugLuauForbidInternalTypes)
LUAU_FASTFLAGVARIABLE(DebugLuauForceStrictMode)
LUAU_FASTFLAGVARIABLE(DebugLuauForceNonStrictMode)
LUAU_FASTFLAGVARIABLE(LuauUseWorkspacePropToChooseSolver)
LUAU_FASTFLAGVARIABLE(DebugLuauAlwaysShowConstraintSolvingIncomplete)
LUAU_FASTFLAG(LuauLimitDynamicConstraintSolving3)
LUAU_FASTFLAG(LuauEmplaceNotPushBack)
LUAU_FASTFLAG(LuauExplicitSkipBoundTypes)
LUAU_FASTFLAG(LuauNoConstraintGenRecursionLimitIce)
LUAU_FASTFLAGVARIABLE(LuauBatchedExecuteTask)

namespace Luau
{

struct BuildQueueItem
{
    ModuleName name;
    ModuleName humanReadableName;

    // Parameters
    std::shared_ptr<SourceNode> sourceNode;
    std::shared_ptr<SourceModule> sourceModule;
    Config config;
    ScopePtr environmentScope;
    std::vector<RequireCycle> requireCycles;
    FrontendOptions options;
    bool recordJsonLog = false;

    // Queue state
    std::vector<size_t> reverseDeps;
    int dirtyDependencies = 0;
    bool processing = false;

    // Result
    std::exception_ptr exception;
    ModulePtr module;
    Frontend::Stats stats;
};

struct BuildQueueWorkState
{
    std::function<void(std::function<void()> task)> executeTask_DEPRECATED;
    std::function<void(std::vector<std::function<void()>> tasks)> executeTasks;

    std::vector<BuildQueueItem> buildQueueItems;

    std::mutex mtx;
    std::condition_variable cv;
    std::vector<size_t> readyQueueItems;

    size_t processing = 0;
    size_t remaining = 0;
};

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

    if (TableType* ttv = getMutable<TableType>(ty))
    {
        for (auto& [name, prop] : ttv->props)
        {
            std::string n;
            n.reserve(rootName.size() + 1 + name.size());
            n += rootName;
            n += ".";
            n += name;
            prop.documentationSymbol = std::move(n);
        }
    }
    else if (ExternType* etv = getMutable<ExternType>(ty))
    {
        for (auto& [name, prop] : etv->props)
        {
            std::string n;
            n.reserve(rootName.size() + 1 + name.size());
            n += rootName;
            n += ".";
            n += name;
            prop.documentationSymbol = std::move(n);
        }
    }
}

static ParseResult parseSourceForModule(std::string_view source, Luau::SourceModule& sourceModule, bool captureComments)
{
    ParseOptions options;
    options.allowDeclarationSyntax = true;
    options.captureComments = captureComments;

    Luau::ParseResult parseResult = Luau::Parser::parse(source.data(), source.size(), *sourceModule.names, *sourceModule.allocator, options);
    sourceModule.root = parseResult.root;
    sourceModule.mode = Mode::Definition;

    if (options.captureComments)
    {
        sourceModule.hotcomments = parseResult.hotcomments;
        sourceModule.commentLocations = parseResult.commentLocations;
    }

    return parseResult;
}

static void persistCheckedTypes(ModulePtr checkedModule, GlobalTypes& globals, ScopePtr targetScope, const std::string& packageName)
{
    CloneState cloneState{globals.builtinTypes};

    std::vector<TypeId> typesToPersist;
    typesToPersist.reserve(checkedModule->declaredGlobals.size() + checkedModule->exportedTypeBindings.size());

    for (const auto& [name, ty] : checkedModule->declaredGlobals)
    {
        TypeId globalTy = clone(ty, globals.globalTypes, cloneState);

        static constexpr const char infix[] = "/global/";
        constexpr int infixLength = sizeof(infix) - 1; // exclude the null terminator
        std::string documentationSymbol;
        documentationSymbol.reserve(packageName.size() + infixLength + name.size());
        documentationSymbol += packageName;
        documentationSymbol += infix;
        documentationSymbol += name;

        generateDocumentationSymbols(globalTy, documentationSymbol);
        targetScope->bindings[globals.globalNames.names->getOrAdd(name.c_str())] = {globalTy, Location(), false, {}, documentationSymbol};

        typesToPersist.push_back(globalTy);
    }

    for (const auto& [name, ty] : checkedModule->exportedTypeBindings)
    {
        TypeFun globalTy = clone(ty, globals.globalTypes, cloneState);

        static constexpr const char infix[] = "/globaltype/";
        constexpr int infixLength = sizeof(infix) - 1; // exclude the null terminator
        std::string documentationSymbol;
        documentationSymbol.reserve(packageName.size() + infixLength + name.size());
        documentationSymbol += packageName;
        documentationSymbol += infix;
        documentationSymbol += name;

        generateDocumentationSymbols(globalTy.type, documentationSymbol);
        targetScope->exportedTypeBindings[name] = globalTy;

        typesToPersist.push_back(globalTy.type);
    }

    for (TypeId ty : typesToPersist)
    {
        persist(ty);
    }
}

LoadDefinitionFileResult Frontend::loadDefinitionFile(
    GlobalTypes& globals,
    ScopePtr targetScope,
    std::string_view source,
    const std::string& packageName,
    bool captureComments,
    bool typeCheckForAutocomplete
)
{
    LUAU_TIMETRACE_SCOPE("loadDefinitionFile", "Frontend");

    Luau::SourceModule sourceModule;
    sourceModule.name = packageName;
    sourceModule.humanReadableName = packageName;

    Luau::ParseResult parseResult = parseSourceForModule(source, sourceModule, captureComments);
    if (parseResult.errors.size() > 0)
        return LoadDefinitionFileResult{false, std::move(parseResult), std::move(sourceModule), nullptr};

    Frontend::Stats dummyStats;
    ModulePtr checkedModule =
        check(sourceModule, Mode::Definition, {}, std::nullopt, /*forAutocomplete*/ false, /*recordJsonLog*/ false, dummyStats, {});

    if (checkedModule->errors.size() > 0)
        return LoadDefinitionFileResult{false, std::move(parseResult), std::move(sourceModule), std::move(checkedModule)};

    persistCheckedTypes(checkedModule, globals, std::move(targetScope), packageName);

    return LoadDefinitionFileResult{true, std::move(parseResult), std::move(sourceModule), std::move(checkedModule)};
}

namespace
{

ErrorVec accumulateErrors(
    const std::unordered_map<ModuleName, std::shared_ptr<SourceNode>>& sourceNodes,
    ModuleResolver& moduleResolver,
    const ModuleName& name
)
{
    DenseHashSet<ModuleName> seen{{}};
    std::vector<ModuleName> queue{name};

    ErrorVec result;

    while (!queue.empty())
    {
        ModuleName next = std::move(queue.back());
        queue.pop_back();

        if (seen.contains(next))
            continue;
        seen.insert(next);

        auto it = sourceNodes.find(next);
        if (it == sourceNodes.end())
            continue;

        const SourceNode& sourceNode = *it->second;
        queue.insert(queue.end(), sourceNode.requireSet.begin(), sourceNode.requireSet.end());

        // FIXME: If a module has a syntax error, we won't be able to re-report it here.
        // The solution is probably to move errors from Module to SourceNode

        auto modulePtr = moduleResolver.getModule(next);
        if (!modulePtr)
            continue;

        Module& module = *modulePtr;

        std::sort(
            module.errors.begin(),
            module.errors.end(),
            [](const TypeError& e1, const TypeError& e2) -> bool
            {
                return e1.location.begin > e2.location.begin;
            }
        );

        result.insert(result.end(), module.errors.begin(), module.errors.end());
    }

    std::reverse(result.begin(), result.end());

    return result;
}

void filterLintOptions(LintOptions& lintOptions, const std::vector<HotComment>& hotcomments, Mode mode)
{
    uint64_t ignoreLints = LintWarning::parseMask(hotcomments);

    lintOptions.warningMask &= ~ignoreLints;

    if (mode != Mode::NoCheck)
    {
        lintOptions.disableWarning(Luau::LintWarning::Code_UnknownGlobal);
    }

    if (mode == Mode::Strict)
    {
        lintOptions.disableWarning(Luau::LintWarning::Code_ImplicitReturn);
    }
}

// Given a source node (start), find all requires that start a transitive dependency path that ends back at start
// For each such path, record the full path and the location of the require in the starting module.
// Note that this is O(V^2) for a fully connected graph and produces O(V) paths of length O(V)
// However, when the graph is acyclic, this is O(V), as well as when only the first cycle is needed (stopAtFirst=true)
std::vector<RequireCycle> getRequireCycles(
    const FileResolver* resolver,
    const std::unordered_map<ModuleName, std::shared_ptr<SourceNode>>& sourceNodes,
    const SourceNode* start
)
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

        stack.push_back(dit->second.get());

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
                        cycle.push_back(node->name);

                    cycle.push_back(top->name);
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
                        stack.push_back(rit->second.get());
                }
            }
        }

        path.clear();
        stack.clear();

        if (!cycle.empty())
        {
            if (FFlag::LuauEmplaceNotPushBack)
                result.emplace_back(RequireCycle{depLocation, std::move(cycle)});
            else
                result.push_back({depLocation, std::move(cycle)});

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
    : useNewLuauSolver(FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old)
    , builtinTypes(NotNull{&builtinTypes_})
    , fileResolver(fileResolver)
    , moduleResolver(this)
    , moduleResolverForAutocomplete(this)
    , globals(builtinTypes, getLuauSolverMode())
    , globalsForAutocomplete(builtinTypes, getLuauSolverMode())
    , configResolver(configResolver)
    , options(options)
{
}

void Frontend::setLuauSolverMode(SolverMode mode)
{
    useNewLuauSolver.store(mode);
}

SolverMode Frontend::getLuauSolverMode() const
{
    if (FFlag::LuauUseWorkspacePropToChooseSolver)
        return useNewLuauSolver.load();
    else if (FFlag::LuauSolverV2)
        return SolverMode::New;
    else
        return SolverMode::Old;
}

void Frontend::parse(const ModuleName& name)
{
    LUAU_TIMETRACE_SCOPE("Frontend::parse", "Frontend");
    LUAU_TIMETRACE_ARGUMENT("name", name.c_str());

    if (getCheckResult(name, false, false))
        return;

    std::vector<ModuleName> buildQueue;
    parseGraph(buildQueue, name, false);
}

void Frontend::parseModules(const std::vector<ModuleName>& names)
{
    LUAU_TIMETRACE_SCOPE("Frontend::parseModules", "Frontend");

    DenseHashSet<Luau::ModuleName> seen{{}};

    for (const ModuleName& name : names)
    {
        if (seen.contains(name))
            continue;

        if (auto it = sourceNodes.find(name); it != sourceNodes.end() && !it->second->hasDirtySourceModule())
        {
            seen.insert(name);
            continue;
        }

        std::vector<ModuleName> queue;
        parseGraph(
            queue,
            name,
            false,
            [&seen](const ModuleName& name)
            {
                return seen.contains(name);
            }
        );

        seen.insert(name);
    }
}

CheckResult Frontend::check(const ModuleName& name, std::optional<FrontendOptions> optionOverride)
{
    LUAU_TIMETRACE_SCOPE("Frontend::check", "Frontend");
    LUAU_TIMETRACE_ARGUMENT("name", name.c_str());

    FrontendOptions frontendOptions = optionOverride.value_or(options);
    if (getLuauSolverMode() == SolverMode::New)
        frontendOptions.forAutocomplete = false;

    if (std::optional<CheckResult> result = getCheckResult(name, true, frontendOptions.forAutocomplete))
        return std::move(*result);

    std::vector<ModuleName> buildQueue;
    bool cycleDetected = parseGraph(buildQueue, name, frontendOptions.forAutocomplete);

    DenseHashSet<Luau::ModuleName> seen{{}};
    std::vector<BuildQueueItem> buildQueueItems;
    addBuildQueueItems(buildQueueItems, buildQueue, cycleDetected, seen, frontendOptions);
    LUAU_ASSERT(!buildQueueItems.empty());

    if (FFlag::DebugLuauLogSolverToJson)
    {
        LUAU_ASSERT(buildQueueItems.back().name == name);
        buildQueueItems.back().recordJsonLog = true;
    }

    checkBuildQueueItems(buildQueueItems);

    // Collect results only for checked modules, 'getCheckResult' produces a different result
    CheckResult checkResult;

    for (const BuildQueueItem& item : buildQueueItems)
    {
        if (item.module->timeout)
            checkResult.timeoutHits.push_back(item.name);

        // If check was manually cancelled, do not return partial results
        if (item.module->cancelled)
            return {};

        checkResult.errors.insert(checkResult.errors.end(), item.module->errors.begin(), item.module->errors.end());

        if (item.name == name)
            checkResult.lintResult = item.module->lintResult;
    }

    return checkResult;
}

void Frontend::queueModuleCheck(const std::vector<ModuleName>& names)
{
    moduleQueue.insert(moduleQueue.end(), names.begin(), names.end());
}

void Frontend::queueModuleCheck(const ModuleName& name)
{
    moduleQueue.push_back(name);
}

std::vector<ModuleName> Frontend::checkQueuedModules_DEPRECATED(
    std::optional<FrontendOptions> optionOverride,
    std::function<void(std::function<void()> task)> executeTask,
    std::function<bool(size_t done, size_t total)> progress
)
{
    FrontendOptions frontendOptions = optionOverride.value_or(options);
    if (getLuauSolverMode() == SolverMode::New)
        frontendOptions.forAutocomplete = false;

    // By taking data into locals, we make sure queue is cleared at the end, even if an ICE or a different exception is thrown
    std::vector<ModuleName> currModuleQueue;
    std::swap(currModuleQueue, moduleQueue);

    DenseHashSet<Luau::ModuleName> seen{{}};

    std::shared_ptr<BuildQueueWorkState> state = std::make_shared<BuildQueueWorkState>();

    for (const ModuleName& name : currModuleQueue)
    {
        if (seen.contains(name))
            continue;

        if (!isDirty(name, frontendOptions.forAutocomplete))
        {
            seen.insert(name);
            continue;
        }

        std::vector<ModuleName> queue;
        bool cycleDetected = parseGraph(
            queue,
            name,
            frontendOptions.forAutocomplete,
            [&seen](const ModuleName& name)
            {
                return seen.contains(name);
            }
        );

        addBuildQueueItems(state->buildQueueItems, queue, cycleDetected, seen, frontendOptions);
    }

    if (state->buildQueueItems.empty())
        return {};

    // We need a mapping from modules to build queue slots
    std::unordered_map<ModuleName, size_t> moduleNameToQueue;

    for (size_t i = 0; i < state->buildQueueItems.size(); i++)
    {
        BuildQueueItem& item = state->buildQueueItems[i];
        moduleNameToQueue[item.name] = i;
    }

    // Default task execution is single-threaded and immediate
    if (!executeTask)
    {
        executeTask = [](std::function<void()> task)
        {
            task();
        };
    }

    state->executeTask_DEPRECATED = executeTask;
    state->remaining = state->buildQueueItems.size();

    // Record dependencies between modules
    for (size_t i = 0; i < state->buildQueueItems.size(); i++)
    {
        BuildQueueItem& item = state->buildQueueItems[i];

        for (const ModuleName& dep : item.sourceNode->requireSet)
        {
            if (auto it = sourceNodes.find(dep); it != sourceNodes.end())
            {
                if (it->second->hasDirtyModule(frontendOptions.forAutocomplete))
                {
                    item.dirtyDependencies++;

                    state->buildQueueItems[moduleNameToQueue[dep]].reverseDeps.push_back(i);
                }
            }
        }
    }

    // In the first pass, check all modules with no pending dependencies
    for (size_t i = 0; i < state->buildQueueItems.size(); i++)
    {
        if (state->buildQueueItems[i].dirtyDependencies == 0)
            sendQueueItemTask_DEPRECATED(state, i);
    }

    // If not a single item was found, a cycle in the graph was hit
    if (state->processing == 0)
        sendQueueCycleItemTask(state);

    std::vector<size_t> nextItems;
    std::optional<size_t> itemWithException;
    bool cancelled = false;

    while (state->remaining != 0)
    {
        {
            std::unique_lock guard(state->mtx);

            // If nothing is ready yet, wait
            state->cv.wait(
                guard,
                [state]
                {
                    return !state->readyQueueItems.empty();
                }
            );

            // Handle checked items
            for (size_t i : state->readyQueueItems)
            {
                const BuildQueueItem& item = state->buildQueueItems[i];

                // If exception was thrown, stop adding new items and wait for processing items to complete
                if (item.exception)
                    itemWithException = i;

                if (item.module && item.module->cancelled)
                    cancelled = true;

                if (itemWithException || cancelled)
                    break;

                recordItemResult(item);

                // Notify items that were waiting for this dependency
                for (size_t reverseDep : item.reverseDeps)
                {
                    BuildQueueItem& reverseDepItem = state->buildQueueItems[reverseDep];

                    LUAU_ASSERT(reverseDepItem.dirtyDependencies != 0);
                    reverseDepItem.dirtyDependencies--;

                    // In case of a module cycle earlier, check if unlocked an item that was already processed
                    if (!reverseDepItem.processing && reverseDepItem.dirtyDependencies == 0)
                        nextItems.push_back(reverseDep);
                }
            }

            LUAU_ASSERT(state->processing >= state->readyQueueItems.size());
            state->processing -= state->readyQueueItems.size();

            LUAU_ASSERT(state->remaining >= state->readyQueueItems.size());
            state->remaining -= state->readyQueueItems.size();
            state->readyQueueItems.clear();
        }

        if (progress)
        {
            if (!progress(state->buildQueueItems.size() - state->remaining, state->buildQueueItems.size()))
                cancelled = true;
        }

        // Items cannot be submitted while holding the lock
        for (size_t i : nextItems)
            sendQueueItemTask_DEPRECATED(state, i);
        nextItems.clear();

        if (state->processing == 0)
        {
            // Typechecking might have been cancelled by user, don't return partial results
            if (cancelled)
                return {};

            // We might have stopped because of a pending exception
            if (itemWithException)
                recordItemResult(state->buildQueueItems[*itemWithException]);
        }

        // If we aren't done, but don't have anything processing, we hit a cycle
        if (state->remaining != 0 && state->processing == 0)
            sendQueueCycleItemTask(state);
    }

    std::vector<ModuleName> checkedModules;
    checkedModules.reserve(state->buildQueueItems.size());

    for (size_t i = 0; i < state->buildQueueItems.size(); i++)
        checkedModules.push_back(std::move(state->buildQueueItems[i].name));

    return checkedModules;
}

std::vector<ModuleName> Frontend::checkQueuedModules(
    std::optional<FrontendOptions> optionOverride,
    std::function<void(std::vector<std::function<void()>> tasks)> executeTasks,
    std::function<bool(size_t done, size_t total)> progress
)
{
    FrontendOptions frontendOptions = optionOverride.value_or(options);
    if (getLuauSolverMode() == SolverMode::New)
        frontendOptions.forAutocomplete = false;

    // By taking data into locals, we make sure queue is cleared at the end, even if an ICE or a different exception is thrown
    std::vector<ModuleName> currModuleQueue;
    std::swap(currModuleQueue, moduleQueue);

    DenseHashSet<Luau::ModuleName> seen{{}};

    std::shared_ptr<BuildQueueWorkState> state = std::make_shared<BuildQueueWorkState>();

    for (const ModuleName& name : currModuleQueue)
    {
        if (seen.contains(name))
            continue;

        if (!isDirty(name, frontendOptions.forAutocomplete))
        {
            seen.insert(name);
            continue;
        }

        std::vector<ModuleName> queue;
        bool cycleDetected = parseGraph(
            queue,
            name,
            frontendOptions.forAutocomplete,
            [&seen](const ModuleName& name)
            {
                return seen.contains(name);
            }
        );

        addBuildQueueItems(state->buildQueueItems, queue, cycleDetected, seen, frontendOptions);
    }

    if (state->buildQueueItems.empty())
        return {};

    // We need a mapping from modules to build queue slots
    std::unordered_map<ModuleName, size_t> moduleNameToQueue;

    for (size_t i = 0; i < state->buildQueueItems.size(); i++)
    {
        BuildQueueItem& item = state->buildQueueItems[i];
        moduleNameToQueue[item.name] = i;
    }

    // Default task execution is single-threaded and immediate
    if (!executeTasks)
    {
        executeTasks = [](std::vector<std::function<void()>> tasks)
        {
            for (auto& task : tasks)
                task();
        };
    }

    state->executeTasks = executeTasks;
    state->remaining = state->buildQueueItems.size();

    // Record dependencies between modules
    for (size_t i = 0; i < state->buildQueueItems.size(); i++)
    {
        BuildQueueItem& item = state->buildQueueItems[i];

        for (const ModuleName& dep : item.sourceNode->requireSet)
        {
            if (auto it = sourceNodes.find(dep); it != sourceNodes.end())
            {
                if (it->second->hasDirtyModule(frontendOptions.forAutocomplete))
                {
                    item.dirtyDependencies++;

                    state->buildQueueItems[moduleNameToQueue[dep]].reverseDeps.push_back(i);
                }
            }
        }
    }

    std::vector<size_t> nextItems;

    // In the first pass, check all modules with no pending dependencies
    for (size_t i = 0; i < state->buildQueueItems.size(); i++)
    {
        if (state->buildQueueItems[i].dirtyDependencies == 0)
            nextItems.push_back(i);
    }

    if (!nextItems.empty())
    {
        sendQueueItemTasks(state, nextItems);
        nextItems.clear();
    }

    // If not a single item was found, a cycle in the graph was hit
    if (state->processing == 0)
        sendQueueCycleItemTask(state);

    std::optional<size_t> itemWithException;
    bool cancelled = false;

    while (state->remaining != 0)
    {
        {
            std::unique_lock guard(state->mtx);

            // If nothing is ready yet, wait
            state->cv.wait(
                guard,
                [state]
                {
                    return !state->readyQueueItems.empty();
                }
            );

            // Handle checked items
            for (size_t i : state->readyQueueItems)
            {
                const BuildQueueItem& item = state->buildQueueItems[i];

                // If exception was thrown, stop adding new items and wait for processing items to complete
                if (item.exception)
                    itemWithException = i;

                if (item.module && item.module->cancelled)
                    cancelled = true;

                if (itemWithException || cancelled)
                    break;

                recordItemResult(item);

                // Notify items that were waiting for this dependency
                for (size_t reverseDep : item.reverseDeps)
                {
                    BuildQueueItem& reverseDepItem = state->buildQueueItems[reverseDep];

                    LUAU_ASSERT(reverseDepItem.dirtyDependencies != 0);
                    reverseDepItem.dirtyDependencies--;

                    // In case of a module cycle earlier, check if unlocked an item that was already processed
                    if (!reverseDepItem.processing && reverseDepItem.dirtyDependencies == 0)
                        nextItems.push_back(reverseDep);
                }
            }

            LUAU_ASSERT(state->processing >= state->readyQueueItems.size());
            state->processing -= state->readyQueueItems.size();

            LUAU_ASSERT(state->remaining >= state->readyQueueItems.size());
            state->remaining -= state->readyQueueItems.size();
            state->readyQueueItems.clear();
        }

        if (progress)
        {
            if (!progress(state->buildQueueItems.size() - state->remaining, state->buildQueueItems.size()))
                cancelled = true;
        }

        // Items cannot be submitted while holding the lock
        if (!nextItems.empty())
        {
            sendQueueItemTasks(state, nextItems);
            nextItems.clear();
        }

        if (state->processing == 0)
        {
            // Typechecking might have been cancelled by user, don't return partial results
            if (cancelled)
                return {};

            // We might have stopped because of a pending exception
            if (itemWithException)
                recordItemResult(state->buildQueueItems[*itemWithException]);
        }

        // If we aren't done, but don't have anything processing, we hit a cycle
        if (state->remaining != 0 && state->processing == 0)
            sendQueueCycleItemTask(state);
    }

    std::vector<ModuleName> checkedModules;
    checkedModules.reserve(state->buildQueueItems.size());

    for (size_t i = 0; i < state->buildQueueItems.size(); i++)
        checkedModules.push_back(std::move(state->buildQueueItems[i].name));

    return checkedModules;
}

std::optional<CheckResult> Frontend::getCheckResult(const ModuleName& name, bool accumulateNested, bool forAutocomplete)
{
    if (getLuauSolverMode() == SolverMode::New)
        forAutocomplete = false;

    auto it = sourceNodes.find(name);

    if (it == sourceNodes.end() || it->second->hasDirtyModule(forAutocomplete))
        return std::nullopt;

    auto& resolver = forAutocomplete ? moduleResolverForAutocomplete : moduleResolver;

    ModulePtr module = resolver.getModule(name);

    if (module == nullptr)
        throw InternalCompilerError("Frontend does not have module: " + name, name);

    CheckResult checkResult;

    if (module->timeout)
        checkResult.timeoutHits.push_back(name);

    if (accumulateNested)
        checkResult.errors = accumulateErrors(sourceNodes, resolver, name);
    else
        checkResult.errors.insert(checkResult.errors.end(), module->errors.begin(), module->errors.end());

    // Get lint result only for top checked module
    checkResult.lintResult = module->lintResult;

    return checkResult;
}

std::vector<ModuleName> Frontend::getRequiredScripts(const ModuleName& name)
{
    RequireTraceResult require = requireTrace[name];
    if (isDirty(name))
    {
        std::optional<SourceCode> source = fileResolver->readSource(name);
        if (!source)
        {
            return {};
        }
        const Config& config = configResolver->getConfig(name);
        ParseOptions opts = config.parseOptions;
        opts.captureComments = true;
        SourceModule result = parse(name, source->source, opts);
        result.type = source->type;
        require = traceRequires(fileResolver, result.root, name);
    }
    std::vector<std::string> requiredModuleNames;
    requiredModuleNames.reserve(require.requireList.size());
    for (const auto& [moduleName, _] : require.requireList)
    {
        requiredModuleNames.push_back(moduleName);
    }
    return requiredModuleNames;
}

bool Frontend::parseGraph(
    std::vector<ModuleName>& buildQueue,
    const ModuleName& root,
    bool forAutocomplete,
    std::function<bool(const ModuleName&)> canSkip
)
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
        auto [sourceNode, _] = getSourceNode(root);
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

            // at this point we know all valid dependencies are processed into SourceNodes
            for (const ModuleName& dep : top->requireSet)
            {
                if (auto it = sourceNodes.find(dep); it != sourceNodes.end())
                    it->second->dependents.insert(top->name);
            }
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
            for (const ModuleName& dep : top->requireSet)
            {
                auto it = sourceNodes.find(dep);
                if (it != sourceNodes.end())
                {
                    // this is a critical optimization: we do *not* traverse non-dirty subtrees.
                    // this relies on the fact that markDirty marks reverse-dependencies dirty as well
                    // thus if a node is not dirty, all its transitive deps aren't dirty, which means that they won't ever need
                    // to be built, *and* can't form a cycle with any nodes we did process.
                    if (!it->second->hasDirtyModule(forAutocomplete))
                        continue;

                    // This module might already be in the outside build queue
                    if (canSkip && canSkip(dep))
                        continue;

                    // note: this check is technically redundant *except* that getSourceNode has somewhat broken memoization
                    // calling getSourceNode twice in succession will reparse the file, since getSourceNode leaves dirty flag set
                    if (seen.contains(it->second.get()))
                    {
                        stack.push_back(it->second.get());
                        continue;
                    }
                }

                auto [sourceNode, _] = getSourceNode(dep);
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

void Frontend::addBuildQueueItems(
    std::vector<BuildQueueItem>& items,
    std::vector<ModuleName>& buildQueue,
    bool cycleDetected,
    DenseHashSet<Luau::ModuleName>& seen,
    const FrontendOptions& frontendOptions
)
{
    for (const ModuleName& moduleName : buildQueue)
    {
        if (seen.contains(moduleName))
            continue;
        seen.insert(moduleName);

        LUAU_ASSERT(sourceNodes.count(moduleName));
        std::shared_ptr<SourceNode>& sourceNode = sourceNodes[moduleName];

        if (!sourceNode->hasDirtyModule(frontendOptions.forAutocomplete))
            continue;

        LUAU_ASSERT(sourceModules.count(moduleName));
        std::shared_ptr<SourceModule>& sourceModule = sourceModules[moduleName];

        BuildQueueItem data{moduleName, fileResolver->getHumanReadableModuleName(moduleName), sourceNode, sourceModule};

        data.config = configResolver->getConfig(moduleName);
        data.environmentScope = getModuleEnvironment(*sourceModule, data.config, frontendOptions.forAutocomplete);
        data.recordJsonLog = FFlag::DebugLuauLogSolverToJson;

        // in the future we could replace toposort with an algorithm that can flag cyclic nodes by itself
        // however, for now getRequireCycles isn't expensive in practice on the cases we care about, and long term
        // all correct programs must be acyclic so this code triggers rarely
        if (cycleDetected)
            data.requireCycles = getRequireCycles(fileResolver, sourceNodes, sourceNode.get());

        data.options = frontendOptions;

        // This is used by the type checker to replace the resulting type of cyclic modules with any
        sourceModule->cyclic = !data.requireCycles.empty();

        items.push_back(std::move(data));
    }
}

static void applyInternalLimitScaling(SourceNode& sourceNode, const ModulePtr module, double limit)
{
    if (module->timeout)
        sourceNode.autocompleteLimitsMult = sourceNode.autocompleteLimitsMult / 2.0;
    else if (module->checkDurationSec < limit / 2.0)
        sourceNode.autocompleteLimitsMult = std::min(sourceNode.autocompleteLimitsMult * 2.0, 1.0);
}

void Frontend::checkBuildQueueItem(BuildQueueItem& item)
{
    SourceNode& sourceNode = *item.sourceNode;
    const SourceModule& sourceModule = *item.sourceModule;
    const Config& config = item.config;
    Mode mode;
    if (FFlag::DebugLuauForceStrictMode)
        mode = Mode::Strict;
    else if (FFlag::DebugLuauForceNonStrictMode)
        mode = Mode::Nonstrict;
    else
        mode = sourceModule.mode.value_or(config.mode);

    item.sourceModule->mode = {mode};
    ScopePtr environmentScope = item.environmentScope;
    double timestamp = getTimestamp();
    const std::vector<RequireCycle>& requireCycles = item.requireCycles;

    TypeCheckLimits typeCheckLimits;

    if (item.options.moduleTimeLimitSec)
        typeCheckLimits.finishTime = TimeTrace::getClock() + *item.options.moduleTimeLimitSec;
    else
        typeCheckLimits.finishTime = std::nullopt;

    // TODO: This is a dirty ad hoc solution for autocomplete timeouts
    // We are trying to dynamically adjust our existing limits to lower total typechecking time under the limit
    // so that we'll have type information for the whole file at lower quality instead of a full abort in the middle
    if (item.options.applyInternalLimitScaling)
    {
        if (FInt::LuauTarjanChildLimit > 0)
            typeCheckLimits.instantiationChildLimit = std::max(1, int(FInt::LuauTarjanChildLimit * sourceNode.autocompleteLimitsMult));
        else
            typeCheckLimits.instantiationChildLimit = std::nullopt;

        if (FInt::LuauTypeInferIterationLimit > 0)
            typeCheckLimits.unifierIterationLimit = std::max(1, int(FInt::LuauTypeInferIterationLimit * sourceNode.autocompleteLimitsMult));
        else
            typeCheckLimits.unifierIterationLimit = std::nullopt;
    }

    typeCheckLimits.cancellationToken = item.options.cancellationToken;

    if (item.options.forAutocomplete)
    {
        // The autocomplete typecheck is always in strict mode with DM awareness to provide better type information for IDE features
        ModulePtr moduleForAutocomplete = check(
            sourceModule,
            Mode::Strict,
            requireCycles,
            environmentScope,
            /*forAutocomplete*/ true,
            /*recordJsonLog*/ false,
            item.stats,
            std::move(typeCheckLimits)
        );

        double duration = getTimestamp() - timestamp;

        moduleForAutocomplete->checkDurationSec = duration;

        if (item.options.moduleTimeLimitSec && item.options.applyInternalLimitScaling)
            applyInternalLimitScaling(sourceNode, moduleForAutocomplete, *item.options.moduleTimeLimitSec);

        item.stats.timeCheck += duration;
        item.stats.filesStrict += 1;

        if (item.options.collectTypeAllocationStats)
        {
            item.stats.typesAllocated += moduleForAutocomplete->internalTypes.types.size();
            item.stats.typePacksAllocated += moduleForAutocomplete->internalTypes.typePacks.size();
            item.stats.boolSingletonsMinted += moduleForAutocomplete->internalTypes.boolSingletonsMinted;
            item.stats.strSingletonsMinted += moduleForAutocomplete->internalTypes.strSingletonsMinted;
            item.stats.uniqueStrSingletonsMinted += moduleForAutocomplete->internalTypes.uniqueStrSingletonsMinted.size();
        }

        if (item.options.customModuleCheck)
            item.options.customModuleCheck(sourceModule, *moduleForAutocomplete);

        item.module = moduleForAutocomplete;
        return;
    }

    ModulePtr module = check(
        sourceModule, mode, requireCycles, environmentScope, /*forAutocomplete*/ false, item.recordJsonLog, item.stats, std::move(typeCheckLimits)
    );

    double duration = getTimestamp() - timestamp;

    module->checkDurationSec = duration;

    if (item.options.moduleTimeLimitSec && item.options.applyInternalLimitScaling)
        applyInternalLimitScaling(sourceNode, module, *item.options.moduleTimeLimitSec);

    item.stats.timeCheck += duration;
    item.stats.filesStrict += (mode == Mode::Strict) ? 1 : 0;
    item.stats.filesNonstrict += (mode == Mode::Nonstrict) ? 1 : 0;

    if (item.options.collectTypeAllocationStats)
    {
        item.stats.typesAllocated += module->internalTypes.types.size();
        item.stats.typePacksAllocated += module->internalTypes.typePacks.size();
        item.stats.boolSingletonsMinted += module->internalTypes.boolSingletonsMinted;
        item.stats.strSingletonsMinted += module->internalTypes.strSingletonsMinted;
        item.stats.uniqueStrSingletonsMinted += module->internalTypes.uniqueStrSingletonsMinted.size();
    }

    if (item.options.customModuleCheck)
        item.options.customModuleCheck(sourceModule, *module);

    if ((getLuauSolverMode() == SolverMode::New) && mode == Mode::NoCheck)
        module->errors.clear();

    if (item.options.runLintChecks)
    {
        LUAU_TIMETRACE_SCOPE("lint", "Frontend");

        LintOptions lintOptions = item.options.enabledLintWarnings.value_or(config.enabledLint);
        filterLintOptions(lintOptions, sourceModule.hotcomments, mode);

        double timestamp = getTimestamp();

        std::vector<LintWarning> warnings =
            Luau::lint(sourceModule.root, *sourceModule.names, environmentScope, module.get(), sourceModule.hotcomments, lintOptions);

        item.stats.timeLint += getTimestamp() - timestamp;

        module->lintResult = classifyLints(warnings, config);
    }

    if (!item.options.retainFullTypeGraphs)
    {
        // copyErrors needs to allocate into interfaceTypes as it copies
        // types out of internalTypes, so we unfreeze it here.
        unfreeze(module->interfaceTypes);
        copyErrors(module->errors, module->interfaceTypes, builtinTypes);
        freeze(module->interfaceTypes);

        module->internalTypes.clear();
        module->defArena.allocator.clear();
        module->keyArena.allocator.clear();

        module->astTypes.clear();
        module->astTypePacks.clear();
        module->astExpectedTypes.clear();
        module->astOriginalCallTypes.clear();
        module->astOverloadResolvedTypes.clear();
        module->astForInNextTypes.clear();
        module->astResolvedTypes.clear();
        module->astResolvedTypePacks.clear();
        module->astCompoundAssignResultTypes.clear();
        module->astScopes.clear();
        module->upperBoundContributors.clear();
        module->scopes.clear();
    }

    if (mode != Mode::NoCheck)
    {
        for (const RequireCycle& cyc : requireCycles)
        {
            TypeError te{cyc.location, item.name, ModuleHasCyclicDependency{cyc.path}};

            module->errors.push_back(te);
        }
    }

    ErrorVec parseErrors;

    for (const ParseError& pe : sourceModule.parseErrors)
        if (FFlag::LuauEmplaceNotPushBack)
            parseErrors.emplace_back(pe.getLocation(), item.name, SyntaxError{pe.what()});
        else
            parseErrors.push_back(TypeError{pe.getLocation(), item.name, SyntaxError{pe.what()}});

    module->errors.insert(module->errors.begin(), parseErrors.begin(), parseErrors.end());

    item.module = module;
}

void Frontend::checkBuildQueueItems(std::vector<BuildQueueItem>& items)
{
    for (BuildQueueItem& item : items)
    {
        checkBuildQueueItem(item);

        if (item.module && item.module->cancelled)
            break;

        recordItemResult(item);
    }
}

void Frontend::recordItemResult(const BuildQueueItem& item)
{
    if (item.exception)
        std::rethrow_exception(item.exception);

    bool replacedModule = false;
    if (item.options.forAutocomplete)
    {
        replacedModule = moduleResolverForAutocomplete.setModule(item.name, item.module);
        item.sourceNode->dirtyModuleForAutocomplete = false;
    }
    else
    {
        replacedModule = moduleResolver.setModule(item.name, item.module);
        item.sourceNode->dirtyModule = false;
    }

    if (replacedModule)
    {
        LUAU_TIMETRACE_SCOPE("Frontend::invalidateDependentModules", "Frontend");
        LUAU_TIMETRACE_ARGUMENT("name", item.name.c_str());
        traverseDependents(
            item.name,
            [forAutocomplete = item.options.forAutocomplete](SourceNode& sourceNode)
            {
                bool traverseSubtree = !sourceNode.hasInvalidModuleDependency(forAutocomplete);
                sourceNode.setInvalidModuleDependency(true, forAutocomplete);
                return traverseSubtree;
            }
        );
    }

    item.sourceNode->setInvalidModuleDependency(false, item.options.forAutocomplete);

    stats.timeCheck += item.stats.timeCheck;
    stats.timeLint += item.stats.timeLint;

    stats.filesStrict += item.stats.filesStrict;
    stats.filesNonstrict += item.stats.filesNonstrict;

    if (item.options.collectTypeAllocationStats)
    {
        stats.typesAllocated += item.stats.typesAllocated;
        stats.typePacksAllocated += item.stats.typePacksAllocated;

        stats.boolSingletonsMinted += item.stats.boolSingletonsMinted;
        stats.strSingletonsMinted += item.stats.strSingletonsMinted;
        stats.uniqueStrSingletonsMinted += item.stats.uniqueStrSingletonsMinted;
    }

    stats.dynamicConstraintsCreated += item.stats.dynamicConstraintsCreated;
}

void Frontend::performQueueItemTask(std::shared_ptr<BuildQueueWorkState> state, size_t itemPos)
{
    BuildQueueItem& item = state->buildQueueItems[itemPos];

    try
    {
        checkBuildQueueItem(item);
    }
    catch (const Luau::InternalCompilerError&)
    {
        item.exception = std::current_exception();
    }

    {
        std::unique_lock guard(state->mtx);
        state->readyQueueItems.push_back(itemPos);
    }

    state->cv.notify_one();
}

void Frontend::sendQueueItemTask_DEPRECATED(std::shared_ptr<BuildQueueWorkState> state, size_t itemPos)
{
    BuildQueueItem& item = state->buildQueueItems[itemPos];

    LUAU_ASSERT(!item.processing);
    item.processing = true;

    state->processing++;

    state->executeTask_DEPRECATED(
        [this, state, itemPos]()
        {
            performQueueItemTask(state, itemPos);
        }
    );
}

void Frontend::sendQueueItemTasks(std::shared_ptr<BuildQueueWorkState> state, const std::vector<size_t>& items)
{
    std::vector<std::function<void()>> tasks;
    tasks.reserve(items.size());

    for (size_t itemPos : items)
    {
        BuildQueueItem& item = state->buildQueueItems[itemPos];

        LUAU_ASSERT(!item.processing);
        item.processing = true;

        tasks.emplace_back(
            [this, state, itemPos]()
            {
                performQueueItemTask(state, itemPos);
            }
        );
    }

    state->processing += items.size();
    state->executeTasks(std::move(tasks));
}

void Frontend::sendQueueCycleItemTask(std::shared_ptr<BuildQueueWorkState> state)
{
    for (size_t i = 0; i < state->buildQueueItems.size(); i++)
    {
        BuildQueueItem& item = state->buildQueueItems[i];

        if (!item.processing)
        {
            if (FFlag::LuauBatchedExecuteTask)
                sendQueueItemTasks(std::move(state), {i});
            else
                sendQueueItemTask_DEPRECATED(std::move(state), i);
            break;
        }
    }
}

ScopePtr Frontend::getModuleEnvironment(const SourceModule& module, const Config& config, bool forAutocomplete) const
{
    ScopePtr result;
    if (forAutocomplete)
        result = globalsForAutocomplete.globalScope;
    else
        result = globals.globalScope;

    if (module.environmentName)
        result = getEnvironmentScope(*module.environmentName);

    if (!config.globals.empty())
    {
        result = std::make_shared<Scope>(result);

        for (const std::string& global : config.globals)
        {
            AstName name = module.names->get(global.c_str());

            if (name.value)
                result->bindings[name].typeId = builtinTypes->anyType;
        }
    }

    return result;
}

bool Frontend::allModuleDependenciesValid(const ModuleName& name, bool forAutocomplete) const
{
    auto it = sourceNodes.find(name);
    return it != sourceNodes.end() && !it->second->hasInvalidModuleDependency(forAutocomplete);
}

bool Frontend::isDirty(const ModuleName& name, bool forAutocomplete) const
{
    auto it = sourceNodes.find(name);
    return it == sourceNodes.end() || it->second->hasDirtyModule(forAutocomplete);
}

/*
 * Mark a file as requiring rechecking before its type information can be safely used again.
 *
 * I am not particularly pleased with the way each dirty() operation involves a BFS on reverse dependencies.
 * It would be nice for this function to be O(1)
 */
void Frontend::markDirty(const ModuleName& name, std::vector<ModuleName>* markedDirty)
{
    LUAU_TIMETRACE_SCOPE("Frontend::markDirty", "Frontend");
    LUAU_TIMETRACE_ARGUMENT("name", name.c_str());

    traverseDependents(
        name,
        [markedDirty](SourceNode& sourceNode)
        {
            if (markedDirty)
                markedDirty->push_back(sourceNode.name);

            if (sourceNode.dirtySourceModule && sourceNode.dirtyModule && sourceNode.dirtyModuleForAutocomplete)
                return false;

            sourceNode.dirtySourceModule = true;
            sourceNode.dirtyModule = true;
            sourceNode.dirtyModuleForAutocomplete = true;

            return true;
        }
    );
}

void Frontend::traverseDependents(const ModuleName& name, std::function<bool(SourceNode&)> processSubtree)
{
    LUAU_TIMETRACE_SCOPE("Frontend::traverseDependents", "Frontend");

    if (sourceNodes.count(name) == 0)
        return;

    std::vector<ModuleName> queue{name};

    while (!queue.empty())
    {
        ModuleName next = std::move(queue.back());
        queue.pop_back();

        LUAU_ASSERT(sourceNodes.count(next) > 0);
        SourceNode& sourceNode = *sourceNodes[next];

        if (!processSubtree(sourceNode))
            continue;

        const Set<ModuleName>& dependents = sourceNode.dependents;
        queue.insert(queue.end(), dependents.begin(), dependents.end());
    }
}

SourceModule* Frontend::getSourceModule(const ModuleName& moduleName)
{
    auto it = sourceModules.find(moduleName);
    if (it != sourceModules.end())
        return it->second.get();
    else
        return nullptr;
}

const SourceModule* Frontend::getSourceModule(const ModuleName& moduleName) const
{
    return const_cast<Frontend*>(this)->getSourceModule(moduleName); // NOLINT(cppcoreguidelines-pro-type-const-cast)
}

struct InternalTypeFinder : TypeOnceVisitor
{
    InternalTypeFinder()
        : TypeOnceVisitor("InternalTypeFinder", FFlag::LuauExplicitSkipBoundTypes)
    {
    }

    bool visit(TypeId, const ExternType&) override
    {
        return false;
    }

    bool visit(TypeId, const BlockedType&) override
    {
        LUAU_ASSERT(false);
        return false;
    }

    bool visit(TypeId, const FreeType&) override
    {
        LUAU_ASSERT(false);
        return false;
    }

    bool visit(TypeId, const PendingExpansionType&) override
    {
        LUAU_ASSERT(false);
        return false;
    }

    bool visit(TypePackId, const BlockedTypePack&) override
    {
        LUAU_ASSERT(false);
        return false;
    }

    bool visit(TypePackId, const FreeTypePack&) override
    {
        LUAU_ASSERT(false);
        return false;
    }

    bool visit(TypePackId, const TypeFunctionInstanceTypePack&) override
    {
        LUAU_ASSERT(false);
        return false;
    }
};

ModulePtr check(
    const SourceModule& sourceModule,
    Mode mode,
    const std::vector<RequireCycle>& requireCycles,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<InternalErrorReporter> iceHandler,
    NotNull<ModuleResolver> moduleResolver,
    NotNull<FileResolver> fileResolver,
    const ScopePtr& parentScope,
    const ScopePtr& typeFunctionScope,
    std::function<void(const ModuleName&, const ScopePtr&)> prepareModuleScope,
    FrontendOptions options,
    TypeCheckLimits limits,
    bool recordJsonLog,
    Frontend::Stats& stats,
    std::function<void(const ModuleName&, std::string)> writeJsonLog
)
{
    LUAU_TIMETRACE_SCOPE("Frontend::check", "Typechecking");
    LUAU_TIMETRACE_ARGUMENT("module", sourceModule.name.c_str());
    LUAU_TIMETRACE_ARGUMENT("name", sourceModule.humanReadableName.c_str());

    ModulePtr module = std::make_shared<Module>();
    module->checkedInNewSolver = true;
    module->name = sourceModule.name;
    module->humanReadableName = sourceModule.humanReadableName;
    module->mode = mode;
    module->internalTypes.owningModule = module.get();
    module->interfaceTypes.owningModule = module.get();
    module->internalTypes.collectSingletonStats = options.collectTypeAllocationStats;
    module->allocator = sourceModule.allocator;
    module->names = sourceModule.names;
    module->root = sourceModule.root;

    iceHandler->moduleName = sourceModule.name;

    std::unique_ptr<DcrLogger> logger;
    if (recordJsonLog)
    {
        logger = std::make_unique<DcrLogger>();
        std::optional<SourceCode> source = fileResolver->readSource(module->name);
        if (source)
        {
            logger->captureSource(source->source);
        }
    }

    DataFlowGraph dfg = DataFlowGraphBuilder::build(sourceModule.root, NotNull{&module->defArena}, NotNull{&module->keyArena}, iceHandler);

    UnifierSharedState unifierState{iceHandler};
    unifierState.counters.recursionLimit = FInt::LuauTypeInferRecursionLimit;
    unifierState.counters.iterationLimit = limits.unifierIterationLimit.value_or(FInt::LuauTypeInferIterationLimit);

    Normalizer normalizer{&module->internalTypes, builtinTypes, NotNull{&unifierState}, SolverMode::New};
    SimplifierPtr simplifier = newSimplifier(NotNull{&module->internalTypes}, builtinTypes);
    TypeFunctionRuntime typeFunctionRuntime{iceHandler, NotNull{&limits}};

    typeFunctionRuntime.allowEvaluation = true;

    ConstraintGenerator cg{
        module,
        NotNull{&normalizer},
        NotNull{simplifier.get()},
        NotNull{&typeFunctionRuntime},
        moduleResolver,
        builtinTypes,
        iceHandler,
        parentScope,
        typeFunctionScope,
        std::move(prepareModuleScope),
        logger.get(),
        NotNull{&dfg},
        requireCycles
    };

    ConstraintSet constraintSet = cg.run(sourceModule.root);
    module->errors = std::move(constraintSet.errors);
    if (FFlag::LuauNoConstraintGenRecursionLimitIce)
        module->constraintGenerationDidNotComplete = cg.recursionLimitMet;

    ConstraintSolver cs{
        NotNull{&normalizer},
        NotNull{simplifier.get()},
        NotNull{&typeFunctionRuntime},
        module,
        moduleResolver,
        requireCycles,
        logger.get(),
        NotNull{&dfg},
        limits,
        std::move(constraintSet)
    };

    if (options.randomizeConstraintResolutionSeed)
        cs.randomize(*options.randomizeConstraintResolutionSeed);

    try
    {
        cs.run();
    }
    catch (const TimeLimitError&)
    {
        module->timeout = true;
    }
    catch (const UserCancelError&)
    {
        module->cancelled = true;
    }

    stats.dynamicConstraintsCreated += cs.solverConstraints.size();

    if (recordJsonLog)
    {
        std::string output = logger->compileOutput();
        if (FFlag::DebugLuauLogSolverToJsonFile && writeJsonLog)
            writeJsonLog(sourceModule.name, std::move(output));
        else
            printf("%s\n", output.c_str());
    }

    for (TypeError& e : cs.errors)
        module->errors.emplace_back(std::move(e));

    module->scopes = std::move(cg.scopes);
    module->type = sourceModule.type;
    module->upperBoundContributors = std::move(cs.upperBoundContributors);

    if (module->timeout || module->cancelled)
    {
        // If solver was interrupted, skip typechecking and replace all module results with error-supressing types to avoid leaking blocked/pending
        // types
        ScopePtr moduleScope = module->getModuleScope();
        moduleScope->returnType = builtinTypes->errorTypePack;

        for (auto& [name, ty] : module->declaredGlobals)
            ty = builtinTypes->errorType;

        for (auto& [name, tf] : module->exportedTypeBindings)
            tf.type = builtinTypes->errorType;
    }
    else
    {
        try
        {
            switch (mode)
            {
            case Mode::Nonstrict:
                Luau::checkNonStrict(
                    builtinTypes,
                    NotNull{simplifier.get()},
                    NotNull{&typeFunctionRuntime},
                    iceHandler,
                    NotNull{&unifierState},
                    NotNull{&dfg},
                    NotNull{&limits},
                    sourceModule,
                    module.get()
                );
                break;
            case Mode::Definition:
                // fallthrough intentional
            case Mode::Strict:
                Luau::check(
                    builtinTypes,
                    NotNull{simplifier.get()},
                    NotNull{&typeFunctionRuntime},
                    NotNull{&unifierState},
                    NotNull{&limits},
                    logger.get(),
                    sourceModule,
                    module.get()
                );
                break;
            case Mode::NoCheck:
                break;
            };
        }
        catch (const TimeLimitError&)
        {
            module->timeout = true;
        }
        catch (const UserCancelError&)
        {
            module->cancelled = true;
        }
    }

    // if the only error we're producing is one about constraint solving being incomplete, we can silence it.
    // this means we won't give this warning if types seem totally nonsensical, but there are no other errors.
    // this is probably, on the whole, a good decision to not annoy users though.
    if (module->errors.size() == 1 && get<ConstraintSolvingIncompleteError>(module->errors[0]) &&
        !FFlag::DebugLuauAlwaysShowConstraintSolvingIncomplete)
        module->errors.clear();

    ExpectedTypeVisitor etv{
        NotNull{&module->astTypes},
        NotNull{&module->astExpectedTypes},
        NotNull{&module->astResolvedTypes},
        NotNull{&module->internalTypes},
        builtinTypes,
        NotNull{parentScope.get()}
    };
    sourceModule.root->visit(&etv);

    // NOTE: This used to be done prior to cloning the public interface, but
    // we now replace "internal" types with `*error-type*`.
    if (FFlag::LuauLimitDynamicConstraintSolving3)
    {
        if (FFlag::DebugLuauForbidInternalTypes)
        {
            InternalTypeFinder finder;

            // `result->returnType` is not filled in yet, so we
            // traverse the return type of the root module.
            finder.traverse(module->getModuleScope()->returnType);

            for (const auto& [_, binding] : module->exportedTypeBindings)
                finder.traverse(binding.type);

            for (const auto& [_, ty] : module->astTypes)
                finder.traverse(ty);

            for (const auto& [_, ty] : module->astExpectedTypes)
                finder.traverse(ty);

            for (const auto& [_, tp] : module->astTypePacks)
                finder.traverse(tp);

            for (const auto& [_, ty] : module->astResolvedTypes)
                finder.traverse(ty);

            for (const auto& [_, ty] : module->astOverloadResolvedTypes)
                finder.traverse(ty);

            for (const auto& [_, tp] : module->astResolvedTypePacks)
                finder.traverse(tp);
        }
    }


    unfreeze(module->interfaceTypes);
    if (FFlag::LuauUseWorkspacePropToChooseSolver)
        module->clonePublicInterface(builtinTypes, *iceHandler, SolverMode::New);
    else
        module->clonePublicInterface_DEPRECATED(builtinTypes, *iceHandler);

    if (!FFlag::LuauLimitDynamicConstraintSolving3)
    {
        if (FFlag::DebugLuauForbidInternalTypes)
        {
            InternalTypeFinder finder;

            finder.traverse(module->returnType);

            for (const auto& [_, binding] : module->exportedTypeBindings)
                finder.traverse(binding.type);

            for (const auto& [_, ty] : module->astTypes)
                finder.traverse(ty);

            for (const auto& [_, ty] : module->astExpectedTypes)
                finder.traverse(ty);

            for (const auto& [_, tp] : module->astTypePacks)
                finder.traverse(tp);

            for (const auto& [_, ty] : module->astResolvedTypes)
                finder.traverse(ty);

            for (const auto& [_, ty] : module->astOverloadResolvedTypes)
                finder.traverse(ty);

            for (const auto& [_, tp] : module->astResolvedTypePacks)
                finder.traverse(tp);
        }
    }

    // It would be nice if we could freeze the arenas before doing type
    // checking, but we'll have to do some work to get there.
    //
    // TypeChecker2 sometimes needs to allocate TypePacks via extendTypePack()
    // in order to do its thing.  We can rework that code to instead allocate
    // into a temporary arena as long as we can prove that the allocated types
    // and packs can never find their way into an error.
    //
    // Notably, we would first need to get to a place where TypeChecker2 is
    // never in the position of dealing with a FreeType.  They should all be
    // bound to something by the time constraints are solved.
    freeze(module->internalTypes);
    freeze(module->interfaceTypes);

    return module;
}

ModulePtr Frontend::check(
    const SourceModule& sourceModule,
    Mode mode,
    std::vector<RequireCycle> requireCycles,
    std::optional<ScopePtr> environmentScope,
    bool forAutocomplete,
    bool recordJsonLog,
    Frontend::Stats& stats,
    TypeCheckLimits typeCheckLimits
)
{
    if (getLuauSolverMode() == SolverMode::New)
    {
        auto prepareModuleScopeWrap = [this, forAutocomplete](const ModuleName& name, const ScopePtr& scope)
        {
            if (prepareModuleScope)
                prepareModuleScope(name, scope, forAutocomplete);
        };

        try
        {
            return Luau::check(
                sourceModule,
                mode,
                requireCycles,
                builtinTypes,
                NotNull{&iceHandler},
                NotNull{forAutocomplete ? &moduleResolverForAutocomplete : &moduleResolver},
                NotNull{fileResolver},
                environmentScope ? *environmentScope : globals.globalScope,
                globals.globalTypeFunctionScope,
                prepareModuleScopeWrap,
                options,
                std::move(typeCheckLimits),
                recordJsonLog,
                stats,
                writeJsonLog
            );
        }
        catch (const InternalCompilerError& err)
        {
            InternalCompilerError augmented = err.location.has_value() ? InternalCompilerError{err.message, sourceModule.name, *err.location}
                                                                       : InternalCompilerError{err.message, sourceModule.name};
            throw augmented;
        }
    }
    else
    {
        TypeChecker typeChecker(
            forAutocomplete ? globalsForAutocomplete.globalScope : globals.globalScope,
            forAutocomplete ? &moduleResolverForAutocomplete : &moduleResolver,
            builtinTypes,
            &iceHandler
        );

        if (prepareModuleScope)
        {
            typeChecker.prepareModuleScope = [this, forAutocomplete](const ModuleName& name, const ScopePtr& scope)
            {
                prepareModuleScope(name, scope, forAutocomplete);
            };
        }

        typeChecker.requireCycles = requireCycles;
        typeChecker.finishTime = typeCheckLimits.finishTime;
        typeChecker.instantiationChildLimit = typeCheckLimits.instantiationChildLimit;
        typeChecker.unifierIterationLimit = typeCheckLimits.unifierIterationLimit;
        typeChecker.cancellationToken = typeCheckLimits.cancellationToken;

        return typeChecker.check(sourceModule, mode, std::move(environmentScope));
    }
}

// Read AST into sourceModules if necessary.  Trace require()s.  Report parse errors.
std::pair<SourceNode*, SourceModule*> Frontend::getSourceNode(const ModuleName& name)
{
    auto it = sourceNodes.find(name);
    if (it != sourceNodes.end() && !it->second->hasDirtySourceModule())
    {
        auto moduleIt = sourceModules.find(name);
        if (moduleIt != sourceModules.end())
            return {it->second.get(), moduleIt->second.get()};
        else
        {
            LUAU_ASSERT(!"Everything in sourceNodes should also be in sourceModules");
            return {it->second.get(), nullptr};
        }
    }

    LUAU_TIMETRACE_SCOPE("Frontend::getSourceNode", "Frontend");
    LUAU_TIMETRACE_ARGUMENT("name", name.c_str());

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

    RequireTraceResult& require = requireTrace[name];
    require = traceRequires(fileResolver, result.root, name);

    std::shared_ptr<SourceNode>& sourceNode = sourceNodes[name];

    if (!sourceNode)
        sourceNode = std::make_shared<SourceNode>();

    std::shared_ptr<SourceModule>& sourceModule = sourceModules[name];

    if (!sourceModule)
        sourceModule = std::make_shared<SourceModule>();

    *sourceModule = std::move(result);
    sourceModule->environmentName = environmentName;

    sourceNode->name = sourceModule->name;
    sourceNode->humanReadableName = sourceModule->humanReadableName;

    // clear all prior dependents. we will re-add them after parsing the rest of the graph
    for (const auto& [moduleName, _] : sourceNode->requireLocations)
    {
        if (auto depIt = sourceNodes.find(moduleName); depIt != sourceNodes.end())
            depIt->second->dependents.erase(sourceNode->name);
    }

    sourceNode->requireSet.clear();
    sourceNode->requireLocations.clear();
    sourceNode->dirtySourceModule = false;

    if (it == sourceNodes.end())
    {
        sourceNode->dirtyModule = true;
        sourceNode->dirtyModuleForAutocomplete = true;
    }

    for (const auto& [moduleName, location] : require.requireList)
        sourceNode->requireSet.insert(moduleName);

    sourceNode->requireLocations = require.requireList;

    return {sourceNode.get(), sourceModule.get()};
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

    Luau::ParseResult parseResult = Luau::Parser::parse(src.data(), src.size(), *sourceModule.names, *sourceModule.allocator, parseOptions);

    stats.timeParse += getTimestamp() - timestamp;
    stats.files++;
    stats.lines += parseResult.lines;

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
    sourceModule.humanReadableName = fileResolver->getHumanReadableModuleName(name);

    if (parseOptions.captureComments)
    {
        sourceModule.commentLocations = std::move(parseResult.commentLocations);
        sourceModule.hotcomments = std::move(parseResult.hotcomments);
    }

    return sourceModule;
}


FrontendModuleResolver::FrontendModuleResolver(Frontend* frontend)
    : frontend(frontend)
{
}

std::optional<ModuleInfo> FrontendModuleResolver::resolveModuleInfo(const ModuleName& currentModuleName, const AstExpr& pathExpr)
{
    // FIXME I think this can be pushed into the FileResolver.
    auto it = frontend->requireTrace.find(currentModuleName);
    if (it == frontend->requireTrace.end())
    {
        // CLI-43699
        // If we can't find the current module name, that's because we bypassed the frontend's initializer
        // and called typeChecker.check directly.
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
    std::scoped_lock lock(moduleMutex);

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

bool FrontendModuleResolver::setModule(const ModuleName& moduleName, ModulePtr module)
{
    std::scoped_lock lock(moduleMutex);

    bool replaced = modules.count(moduleName) > 0;
    modules[moduleName] = std::move(module);
    return replaced;
}

void FrontendModuleResolver::clearModules()
{
    std::scoped_lock lock(moduleMutex);

    modules.clear();
}

ScopePtr Frontend::addEnvironment(const std::string& environmentName)
{
    LUAU_ASSERT(environments.count(environmentName) == 0);

    if (environments.count(environmentName) == 0)
    {
        ScopePtr scope = std::make_shared<Scope>(globals.globalScope);
        environments[environmentName] = scope;
        return scope;
    }
    else
        return environments[environmentName];
}

ScopePtr Frontend::getEnvironmentScope(const std::string& environmentName) const
{
    if (auto it = environments.find(environmentName); it != environments.end())
        return it->second;

    LUAU_ASSERT(!"environment doesn't exist");
    return {};
}

void Frontend::registerBuiltinDefinition(const std::string& name, std::function<void(Frontend&, GlobalTypes&, ScopePtr)> applicator)
{
    LUAU_ASSERT(builtinDefinitions.count(name) == 0);

    if (builtinDefinitions.count(name) == 0)
        builtinDefinitions[name] = applicator;
}

void Frontend::applyBuiltinDefinitionToEnvironment(const std::string& environmentName, const std::string& definitionName)
{
    LUAU_ASSERT(builtinDefinitions.count(definitionName) > 0);

    if (builtinDefinitions.count(definitionName) > 0)
        builtinDefinitions[definitionName](*this, globals, getEnvironmentScope(environmentName));
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
    moduleResolver.clearModules();
    moduleResolverForAutocomplete.clearModules();
    requireTrace.clear();
}

void Frontend::clearBuiltinEnvironments()
{
    environments.clear();
    builtinDefinitions.clear();
}

} // namespace Luau
