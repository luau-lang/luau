// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Config.h"
#include "Luau/ModuleResolver.h"
#include "Luau/TypeInfer.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Frontend.h"
#include "Luau/TypeAttach.h"
#include "Luau/Transpiler.h"

#include "Luau/AnalyzeRequirer.h"
#include "Luau/FileUtils.h"
#include "Luau/Flags.h"
#include "Luau/RequireNavigator.h"

#include <condition_variable>
#include <functional>
#include <mutex>
#include <queue>
#include <thread>
#include <utility>
#include <fstream>

#ifdef CALLGRIND
#include <valgrind/callgrind.h>
#endif

LUAU_FASTFLAG(DebugLuauTimeTracing)
LUAU_FASTFLAG(DebugLuauLogSolverToJsonFile)

enum class ReportFormat
{
    Default,
    Luacheck,
    Gnu,
};

static void report(ReportFormat format, const char* name, const Luau::Location& loc, const char* type, const char* message)
{
    switch (format)
    {
    case ReportFormat::Default:
        fprintf(stderr, "%s(%d,%d): %s: %s\n", name, loc.begin.line + 1, loc.begin.column + 1, type, message);
        break;

    case ReportFormat::Luacheck:
    {
        // Note: luacheck's end column is inclusive but our end column is exclusive
        // In addition, luacheck doesn't support multi-line messages, so if the error is multiline we'll fake end column as 100 and hope for the best
        int columnEnd = (loc.begin.line == loc.end.line) ? loc.end.column : 100;

        // Use stdout to match luacheck behavior
        fprintf(stdout, "%s:%d:%d-%d: (W0) %s: %s\n", name, loc.begin.line + 1, loc.begin.column + 1, columnEnd, type, message);
        break;
    }

    case ReportFormat::Gnu:
        // Note: GNU end column is inclusive but our end column is exclusive
        fprintf(stderr, "%s:%d.%d-%d.%d: %s: %s\n", name, loc.begin.line + 1, loc.begin.column + 1, loc.end.line + 1, loc.end.column, type, message);
        break;
    }
}

static void reportError(const Luau::Frontend& frontend, ReportFormat format, const Luau::TypeError& error)
{
    std::string humanReadableName = frontend.fileResolver->getHumanReadableModuleName(error.moduleName);

    if (const Luau::SyntaxError* syntaxError = Luau::get_if<Luau::SyntaxError>(&error.data))
        report(format, humanReadableName.c_str(), error.location, "SyntaxError", syntaxError->message.c_str());
    else
        report(
            format,
            humanReadableName.c_str(),
            error.location,
            "TypeError",
            Luau::toString(error, Luau::TypeErrorToStringOptions{frontend.fileResolver}).c_str()
        );
}

static void reportWarning(ReportFormat format, const char* name, const Luau::LintWarning& warning)
{
    report(format, name, warning.location, Luau::LintWarning::getName(warning.code), warning.text.c_str());
}

static bool reportModuleResult(Luau::Frontend& frontend, const Luau::ModuleName& name, ReportFormat format, bool annotate)
{
    std::optional<Luau::CheckResult> cr = frontend.getCheckResult(name, false);

    if (!cr)
    {
        fprintf(stderr, "Failed to find result for %s\n", name.c_str());
        return false;
    }

    if (!frontend.getSourceModule(name))
    {
        fprintf(stderr, "Error opening %s\n", name.c_str());
        return false;
    }

    for (auto& error : cr->errors)
        reportError(frontend, format, error);

    std::string humanReadableName = frontend.fileResolver->getHumanReadableModuleName(name);
    for (auto& error : cr->lintResult.errors)
        reportWarning(format, humanReadableName.c_str(), error);
    for (auto& warning : cr->lintResult.warnings)
        reportWarning(format, humanReadableName.c_str(), warning);

    if (annotate)
    {
        Luau::SourceModule* sm = frontend.getSourceModule(name);
        Luau::ModulePtr m = frontend.moduleResolver.getModule(name);

        Luau::attachTypeData(*sm, *m);

        std::string annotated = Luau::transpileWithTypes(*sm->root);

        printf("%s", annotated.c_str());
    }

    return cr->errors.empty() && cr->lintResult.errors.empty();
}

static void displayHelp(const char* argv0)
{
    printf("Usage: %s [--mode] [options] [file list]\n", argv0);
    printf("\n");
    printf("Available modes:\n");
    printf("  omitted: typecheck and lint input files\n");
    printf("  --annotate: typecheck input files and output source with type annotations\n");
    printf("\n");
    printf("Available options:\n");
    printf("  --formatter=plain: report analysis errors in Luacheck-compatible format\n");
    printf("  --formatter=gnu: report analysis errors in GNU-compatible format\n");
    printf("  --mode=strict: default to strict mode when typechecking\n");
    printf("  --timetrace: record compiler time tracing information into trace.json\n");
}

static int assertionHandler(const char* expr, const char* file, int line, const char* function)
{
    printf("%s(%d): ASSERTION FAILED: %s\n", file, line, expr);
    fflush(stdout);
    return 1;
}

struct CliFileResolver : Luau::FileResolver
{
    std::optional<Luau::SourceCode> readSource(const Luau::ModuleName& name) override
    {
        Luau::SourceCode::Type sourceType;
        std::optional<std::string> source = std::nullopt;

        // If the module name is "-", then read source from stdin
        if (name == "-")
        {
            source = readStdin();
            sourceType = Luau::SourceCode::Script;
        }
        else
        {
            source = readFile(name);
            sourceType = Luau::SourceCode::Module;
        }

        if (!source)
            return std::nullopt;

        return Luau::SourceCode{*source, sourceType};
    }

    std::optional<Luau::ModuleInfo> resolveModule(const Luau::ModuleInfo* context, Luau::AstExpr* node) override
    {
        if (Luau::AstExprConstantString* expr = node->as<Luau::AstExprConstantString>())
        {
            std::string path{expr->value.data, expr->value.size};

            FileNavigationContext navigationContext{context->name};
            Luau::Require::ErrorHandler nullErrorHandler{};

            Luau::Require::Navigator navigator(navigationContext, nullErrorHandler);
            if (navigator.navigate(std::move(path)) != Luau::Require::Navigator::Status::Success)
                return std::nullopt;

            if (!navigationContext.isModulePresent())
                return std::nullopt;

            if (std::optional<std::string> identifier = navigationContext.getIdentifier())
                return {{*identifier}};
        }

        return std::nullopt;
    }

    std::string getHumanReadableModuleName(const Luau::ModuleName& name) const override
    {
        if (name == "-")
            return "stdin";
        return name;
    }
};

struct CliConfigResolver : Luau::ConfigResolver
{
    Luau::Config defaultConfig;

    mutable std::unordered_map<std::string, Luau::Config> configCache;
    mutable std::vector<std::pair<std::string, std::string>> configErrors;

    CliConfigResolver(Luau::Mode mode)
    {
        defaultConfig.mode = mode;
    }

    const Luau::Config& getConfig(const Luau::ModuleName& name) const override
    {
        std::optional<std::string> path = getParentPath(name);
        if (!path)
            return defaultConfig;

        return readConfigRec(*path);
    }

    const Luau::Config& readConfigRec(const std::string& path) const
    {
        auto it = configCache.find(path);
        if (it != configCache.end())
            return it->second;

        std::optional<std::string> parent = getParentPath(path);
        Luau::Config result = parent ? readConfigRec(*parent) : defaultConfig;

        std::string configPath = joinPaths(path, Luau::kConfigName);

        if (std::optional<std::string> contents = readFile(configPath))
        {
            Luau::ConfigOptions::AliasOptions aliasOpts;
            aliasOpts.configLocation = configPath;
            aliasOpts.overwriteAliases = true;

            Luau::ConfigOptions opts;
            opts.aliasOptions = std::move(aliasOpts);

            std::optional<std::string> error = Luau::parseConfig(*contents, result, opts);
            if (error)
                configErrors.push_back({configPath, *error});
        }

        return configCache[path] = result;
    }
};

struct TaskScheduler
{
    TaskScheduler(unsigned threadCount)
        : threadCount(threadCount)
    {
        for (unsigned i = 0; i < threadCount; i++)
        {
            workers.emplace_back(
                [this]
                {
                    workerFunction();
                }
            );
        }
    }

    ~TaskScheduler()
    {
        for (unsigned i = 0; i < threadCount; i++)
            push({});

        for (std::thread& worker : workers)
            worker.join();
    }

    std::function<void()> pop()
    {
        std::unique_lock guard(mtx);

        cv.wait(
            guard,
            [this]
            {
                return !tasks.empty();
            }
        );

        std::function<void()> task = tasks.front();
        tasks.pop();
        return task;
    }

    void push(std::function<void()> task)
    {
        {
            std::unique_lock guard(mtx);
            tasks.push(std::move(task));
        }

        cv.notify_one();
    }

    static unsigned getThreadCount()
    {
        return std::max(std::thread::hardware_concurrency(), 1u);
    }

private:
    void workerFunction()
    {
        while (std::function<void()> task = pop())
            task();
    }

    unsigned threadCount = 1;
    std::mutex mtx;
    std::condition_variable cv;
    std::vector<std::thread> workers;
    std::queue<std::function<void()>> tasks;
};

int main(int argc, char** argv)
{
    Luau::assertHandler() = assertionHandler;

    setLuauFlagsDefault();

    if (argc >= 2 && strcmp(argv[1], "--help") == 0)
    {
        displayHelp(argv[0]);
        return 0;
    }

    ReportFormat format = ReportFormat::Default;
    Luau::Mode mode = Luau::Mode::Nonstrict;
    bool annotate = false;
    int threadCount = 0;
    std::string basePath = "";

    for (int i = 1; i < argc; ++i)
    {
        if (argv[i][0] != '-')
            continue;

        if (strcmp(argv[i], "--formatter=plain") == 0)
            format = ReportFormat::Luacheck;
        else if (strcmp(argv[i], "--formatter=gnu") == 0)
            format = ReportFormat::Gnu;
        else if (strcmp(argv[i], "--mode=strict") == 0)
            mode = Luau::Mode::Strict;
        else if (strcmp(argv[i], "--annotate") == 0)
            annotate = true;
        else if (strcmp(argv[i], "--timetrace") == 0)
            FFlag::DebugLuauTimeTracing.value = true;
        else if (strncmp(argv[i], "--fflags=", 9) == 0)
            setLuauFlags(argv[i] + 9);
        else if (strncmp(argv[i], "-j", 2) == 0)
            threadCount = int(strtol(argv[i] + 2, nullptr, 10));
        else if (strncmp(argv[i], "--logbase=", 10) == 0)
            basePath = std::string{argv[i] + 10};
    }

#if !defined(LUAU_ENABLE_TIME_TRACE)
    if (FFlag::DebugLuauTimeTracing)
    {
        fprintf(stderr, "To run with --timetrace, Luau has to be built with LUAU_ENABLE_TIME_TRACE enabled\n");
        return 1;
    }
#endif

    Luau::FrontendOptions frontendOptions;
    frontendOptions.retainFullTypeGraphs = annotate;
    frontendOptions.runLintChecks = true;

    CliFileResolver fileResolver;
    CliConfigResolver configResolver(mode);
    Luau::Frontend frontend(&fileResolver, &configResolver, frontendOptions);

    if (FFlag::DebugLuauLogSolverToJsonFile)
    {
        frontend.writeJsonLog = [&basePath](const Luau::ModuleName& moduleName, std::string log)
        {
            std::string path = moduleName + ".log.json";
            size_t pos = moduleName.find_last_of('/');
            if (pos != std::string::npos)
                path = moduleName.substr(pos + 1);

            if (!basePath.empty())
                path = joinPaths(basePath, path);

            std::ofstream os(path);

            os << log << std::endl;
            printf("Wrote JSON log to %s\n", path.c_str());
        };
    }

    Luau::registerBuiltinGlobals(frontend, frontend.globals);
    Luau::freeze(frontend.globals.globalTypes);

#ifdef CALLGRIND
    CALLGRIND_ZERO_STATS;
#endif

    std::vector<std::string> files = getSourceFiles(argc, argv);

    for (const std::string& path : files)
        frontend.queueModuleCheck(path);

    std::vector<Luau::ModuleName> checkedModules;

    // If thread count is not set, try to use HW thread count, but with an upper limit
    // When we improve scalability of typechecking, upper limit can be adjusted/removed
    if (threadCount <= 0)
        threadCount = std::min(TaskScheduler::getThreadCount(), 8u);

    try
    {
        TaskScheduler scheduler(threadCount);

        checkedModules = frontend.checkQueuedModules(
            std::nullopt,
            [&](std::vector<std::function<void()>> tasks)
            {
                for (auto& task : tasks)
                    scheduler.push(std::move(task));
            }
        );
    }
    catch (const Luau::InternalCompilerError& ice)
    {
        Luau::Location location = ice.location ? *ice.location : Luau::Location();

        std::string moduleName = ice.moduleName ? *ice.moduleName : "<unknown module>";
        std::string humanReadableName = frontend.fileResolver->getHumanReadableModuleName(moduleName);

        Luau::TypeError error(location, moduleName, Luau::InternalError{ice.message});

        report(
            format,
            humanReadableName.c_str(),
            location,
            "InternalCompilerError",
            Luau::toString(error, Luau::TypeErrorToStringOptions{frontend.fileResolver}).c_str()
        );
        return 1;
    }

    int failed = 0;

    for (const Luau::ModuleName& name : checkedModules)
        failed += !reportModuleResult(frontend, name, format, annotate);

    if (!configResolver.configErrors.empty())
    {
        failed += int(configResolver.configErrors.size());

        for (const auto& pair : configResolver.configErrors)
            fprintf(stderr, "%s: %s\n", pair.first.c_str(), pair.second.c_str());
    }

    if (format == ReportFormat::Luacheck)
        return 0;
    else
        return failed ? 1 : 0;
}
