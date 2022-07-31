// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lluz/ModuleResolver.h"
#include "lluz/TypeInfer.h"
#include "lluz/BuiltinDefinitions.h"
#include "lluz/Frontend.h"
#include "lluz/TypeAttach.h"
#include "lluz/Transpiler.h"

#include "FileUtils.h"

#ifdef CALLGRIND
#include <valgrind/callgrind.h>
#endif

lluz_FASTFLAG(DebugLluTimeTracing)
lluz_FASTFLAG(LluTypeMismatchModuleNameResolution)

enum class ReportFormat
{
    Default,
    Luacheck,
    Gnu,
};

static void report(ReportFormat format, const char* name, const lluz::Location& loc, const char* type, const char* message)
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

static void reportError(const lluz::Frontend& frontend, ReportFormat format, const lluz::TypeError& error)
{
    std::string humanReadableName = frontend.fileResolver->getHumanReadableModuleName(error.moduleName);

    if (const lluz::SyntaxError* syntaxError = lluz::get_if<lluz::SyntaxError>(&error.data))
        report(format, humanReadableName.c_str(), error.location, XorStr("SyntaxError"), syntaxError->message.c_str());
    else if (FFlag::LluTypeMismatchModuleNameResolution)
        report(format, humanReadableName.c_str(), error.location, XorStr("TypeError"),
            lluz::toString(error, lluz::TypeErrorToStringOptions{frontend.fileResolver}).c_str());
    else
        report(format, humanReadableName.c_str(), error.location, XorStr("TypeError"), lluz::toString(error).c_str());
}

static void reportWarning(ReportFormat format, const char* name, const lluz::LintWarning& warning)
{
    report(format, name, warning.location, lluz::LintWarning::getName(warning.code), warning.text.c_str());
}

static bool analyzeFile(lluz::Frontend& frontend, const char* name, ReportFormat format, bool annotate)
{
    lluz::CheckResult cr;

    if (frontend.isDirty(name))
        cr = frontend.check(name);

    if (!frontend.getSourceModule(name))
    {
        fprintf(stderr, XorStr("Error opening %s\n"), name);
        return false;
    }

    for (auto& error : cr.errors)
        reportError(frontend, format, error);

    lluz::LintResult lr = frontend.lint(name);

    std::string humanReadableName = frontend.fileResolver->getHumanReadableModuleName(name);
    for (auto& error : lr.errors)
        reportWarning(format, humanReadableName.c_str(), error);
    for (auto& warning : lr.warnings)
        reportWarning(format, humanReadableName.c_str(), warning);

    if (annotate)
    {
        lluz::SourceModule* sm = frontend.getSourceModule(name);
        lluz::ModulePtr m = frontend.moduleResolver.getModule(name);

        lluz::attachTypeData(*sm, *m);

        std::string annotated = lluz::transpileWithTypes(*sm->root);

        printf("%s", annotated.c_str());
    }

    return cr.errors.empty() && lr.errors.empty();
}

static void displayHelp(const char* argv0)
{
    printf(XorStr("Usage: %s [--mode] [options] [file list]\n"), argv0);
    printf(XorStr("\n"));
    printf(XorStr("Available modes:\n"));
    printf(XorStr("  omitted: typecheck and lint input files\n"));
    printf(XorStr("  --annotate: typecheck input files and output source with type annotations\n"));
    printf(XorStr("\n"));
    printf(XorStr("Available options:\n"));
    printf(XorStr("  --formatter=plain: report analysis errors in Luacheck-compatible format\n"));
    printf(XorStr("  --formatter=gnu: report analysis errors in GNU-compatible format\n"));
    printf(XorStr("  --mode=strict: default to strict mode when typechecking\n"));
    printf(XorStr("  --timetrace: record compiler time tracing information into trace.json\n"));
}

static int assertionHandler(const char* expr, const char* file, int line, const char* function)
{
    printf(XorStr("%s(%d): ASSERTION FAILED: %s\n"), file, line, expr);
    return 1;
}

struct CliFileResolver : lluz::FileResolver
{
    std::optional<lluz::SourceCode> readSource(const lluz::ModuleName& name) override
    {
        lluz::SourceCode::Type sourceType;
        std::optional<std::string> source = std::nullopt;

        // If the module name is "-", then read source from stdin
        if (name == XorStr("-"))
        {
            source = readStdin();
            sourceType = lluz::SourceCode::Script;
        }
        else
        {
            source = readFile(name);
            sourceType = lluz::SourceCode::Module;
        }

        if (!source)
            return std::nullopt;

        return lluz::SourceCode{*source, sourceType};
    }

    std::optional<lluz::ModuleInfo> resolveModule(const lluz::ModuleInfo* context, lluz::AstExpr* node) override
    {
        if (lluz::AstExprConstantString* expr = node->as<lluz::AstExprConstantString>())
        {
            lluz::ModuleName name = std::string(expr->value.data, expr->value.size) + ".lluz";
            if (!readFile(name))
            {
                // fall back to .lua if a module with .lluz doesn't exist
                name = std::string(expr->value.data, expr->value.size) + ".lua";
            }

            return {{name}};
        }

        return std::nullopt;
    }

    std::string getHumanReadableModuleName(const lluz::ModuleName& name) const override
    {
        if (name == XorStr("-"))
            return XorStr("stdin");
        return name;
    }
};

struct CliConfigResolver : lluz::ConfigResolver
{
    lluz::Config defaultConfig;

    mutable std::unordered_map<std::string, lluz::Config> configCache;
    mutable std::vector<std::pair<std::string, std::string>> configErrors;

    CliConfigResolver(lluz::Mode mode)
    {
        defaultConfig.mode = mode;
    }

    const lluz::Config& getConfig(const lluz::ModuleName& name) const override
    {
        std::optional<std::string> path = getParentPath(name);
        if (!path)
            return defaultConfig;

        return readConfigRec(*path);
    }

    const lluz::Config& readConfigRec(const std::string& path) const
    {
        auto it = configCache.find(path);
        if (it != configCache.end())
            return it->second;

        std::optional<std::string> parent = getParentPath(path);
        lluz::Config result = parent ? readConfigRec(*parent) : defaultConfig;

        std::string configPath = joinPaths(path, lluz::kConfigName);

        if (std::optional<std::string> contents = readFile(configPath))
        {
            std::optional<std::string> error = lluz::parseConfig(*contents, result);
            if (error)
                configErrors.push_back({configPath, *error});
        }

        return configCache[path] = result;
    }
};

int main(int argc, char** argv)
{
    lluz::assertHandler() = assertionHandler;

    for (lluz::FValue<bool>* flag = lluz::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, XorStr("lluz"), 4) == 0)
            flag->value = true;

    if (argc >= 2 && strcmp(argv[1], XorStr("--help")) == 0)
    {
        displayHelp(argv[0]);
        return 0;
    }

    ReportFormat format = ReportFormat::Default;
    lluz::Mode mode = lluz::Mode::Nonstrict;
    bool annotate = false;

    for (int i = 1; i < argc; ++i)
    {
        if (argv[i][0] != '-')
            continue;

        if (strcmp(argv[i], "--formatter=plain") == 0)
            format = ReportFormat::Luacheck;
        else if (strcmp(argv[i], "--formatter=gnu") == 0)
            format = ReportFormat::Gnu;
        else if (strcmp(argv[i], "--mode=strict") == 0)
            mode = lluz::Mode::Strict;
        else if (strcmp(argv[i], "--annotate") == 0)
            annotate = true;
        else if (strcmp(argv[i], "--timetrace") == 0)
            FFlag::DebugLluTimeTracing.value = true;
    }

#if !defined(lluz_ENABLE_TIME_TRACE)
    if (FFlag::DebugLluTimeTracing)
    {
        printf(XorStr("To run with --timetrace, lluz has to be built with lluz_ENABLE_TIME_TRACE enabled\n"));
        return 1;
    }
#endif

    lluz::FrontendOptions frontendOptions;
    frontendOptions.retainFullTypeGraphs = annotate;

    CliFileResolver fileResolver;
    CliConfigResolver configResolver(mode);
    lluz::Frontend frontend(&fileResolver, &configResolver, frontendOptions);

    lluz::registerBuiltinTypes(frontend.typeChecker);
    lluz::freeze(frontend.typeChecker.globalTypes);

#ifdef CALLGRIND
    CALLGRIND_ZERO_STATS;
#endif

    std::vector<std::string> files = getSourceFiles(argc, argv);

    int failed = 0;

    for (const std::string& path : files)
        failed += !analyzeFile(frontend, path.c_str(), format, annotate);

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
