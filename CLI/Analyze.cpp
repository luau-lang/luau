// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ModuleResolver.h"
#include "Luau/TypeInfer.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Frontend.h"
#include "Luau/TypeAttach.h"
#include "Luau/Transpiler.h"

#include "FileUtils.h"

enum class ReportFormat
{
    Default,
    Luacheck
};

static void report(ReportFormat format, const char* name, const Luau::Location& location, const char* type, const char* message)
{
    switch (format)
    {
    case ReportFormat::Default:
        fprintf(stderr, "%s(%d,%d): %s: %s\n", name, location.begin.line + 1, location.begin.column + 1, type, message);
        break;

    case ReportFormat::Luacheck:
    {
        // Note: luacheck's end column is inclusive but our end column is exclusive
        // In addition, luacheck doesn't support multi-line messages, so if the error is multiline we'll fake end column as 100 and hope for the best
        int columnEnd = (location.begin.line == location.end.line) ? location.end.column : 100;

        fprintf(stdout, "%s:%d:%d-%d: (W0) %s: %s\n", name, location.begin.line + 1, location.begin.column + 1, columnEnd, type, message);
        break;
    }
    }
}

static void reportError(ReportFormat format, const Luau::TypeError& error)
{
    const char* name = error.moduleName.c_str();

    if (const Luau::SyntaxError* syntaxError = Luau::get_if<Luau::SyntaxError>(&error.data))
        report(format, name, error.location, "SyntaxError", syntaxError->message.c_str());
    else
        report(format, name, error.location, "TypeError", Luau::toString(error).c_str());
}

static void reportWarning(ReportFormat format, const char* name, const Luau::LintWarning& warning)
{
    report(format, name, warning.location, Luau::LintWarning::getName(warning.code), warning.text.c_str());
}

static bool analyzeFile(Luau::Frontend& frontend, const char* name, ReportFormat format, bool annotate)
{
    Luau::CheckResult cr;

    if (frontend.isDirty(name))
        cr = frontend.check(name);

    if (!frontend.getSourceModule(name))
    {
        fprintf(stderr, "Error opening %s\n", name);
        return false;
    }

    for (auto& error : cr.errors)
        reportError(format, error);

    Luau::LintResult lr = frontend.lint(name);

    for (auto& error : lr.errors)
        reportWarning(format, name, error);
    for (auto& warning : lr.warnings)
        reportWarning(format, name, warning);

    if (annotate)
    {
        Luau::SourceModule* sm = frontend.getSourceModule(name);
        Luau::ModulePtr m = frontend.moduleResolver.getModule(name);

        Luau::attachTypeData(*sm, *m);

        std::string annotated = Luau::transpileWithTypes(*sm->root);

        printf("%s", annotated.c_str());
    }

    return cr.errors.empty() && lr.errors.empty();
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
}

static int assertionHandler(const char* expr, const char* file, int line)
{
    printf("%s(%d): ASSERTION FAILED: %s\n", file, line, expr);
    return 1;
}

struct CliFileResolver : Luau::FileResolver
{
    std::optional<Luau::SourceCode> readSource(const Luau::ModuleName& name) override
    {
        std::optional<std::string> source = readFile(name);
        if (!source)
            return std::nullopt;

        return Luau::SourceCode{*source, Luau::SourceCode::Module};
    }

    std::optional<Luau::ModuleInfo> resolveModule(const Luau::ModuleInfo* context, Luau::AstExpr* node) override
    {
        if (Luau::AstExprConstantString* expr = node->as<Luau::AstExprConstantString>())
        {
            Luau::ModuleName name = std::string(expr->value.data, expr->value.size) + ".luau";
            if (!moduleExists(name))
            {
                // fall back to .lua if a module with .luau doesn't exist
                name = std::string(expr->value.data, expr->value.size) + ".lua";
            }

            return {{name}};
        }

        return std::nullopt;
    }

    bool moduleExists(const Luau::ModuleName& name) const override
    {
        return !!readFile(name);
    }


    std::optional<Luau::ModuleName> fromAstFragment(Luau::AstExpr* expr) const override
    {
        return std::nullopt;
    }

    Luau::ModuleName concat(const Luau::ModuleName& lhs, std::string_view rhs) const override
    {
        return lhs + "/" + std::string(rhs);
    }

    std::optional<Luau::ModuleName> getParentModuleName(const Luau::ModuleName& name) const override
    {
        return std::nullopt;
    }
};

struct CliConfigResolver : Luau::ConfigResolver
{
    Luau::Config defaultConfig;

    mutable std::unordered_map<std::string, Luau::Config> configCache;
    mutable std::vector<std::pair<std::string, std::string>> configErrors;

    CliConfigResolver()
    {
        defaultConfig.mode = Luau::Mode::Nonstrict;
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
            std::optional<std::string> error = Luau::parseConfig(*contents, result);
            if (error)
                configErrors.push_back({configPath, *error});
        }

        return configCache[path] = result;
    }
};

int main(int argc, char** argv)
{
    Luau::assertHandler() = assertionHandler;

    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;

    if (argc >= 2 && strcmp(argv[1], "--help") == 0)
    {
        displayHelp(argv[0]);
        return 0;
    }

    ReportFormat format = ReportFormat::Default;
    bool annotate = false;

    for (int i = 1; i < argc; ++i)
    {
        if (argv[i][0] != '-')
            continue;

        if (strcmp(argv[i], "--formatter=plain") == 0)
            format = ReportFormat::Luacheck;
        else if (strcmp(argv[i], "--annotate") == 0)
            annotate = true;
    }

    Luau::FrontendOptions frontendOptions;
    frontendOptions.retainFullTypeGraphs = annotate;

    CliFileResolver fileResolver;
    CliConfigResolver configResolver;
    Luau::Frontend frontend(&fileResolver, &configResolver, frontendOptions);

    Luau::registerBuiltinTypes(frontend.typeChecker);
    Luau::freeze(frontend.typeChecker.globalTypes);

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

    return (format == ReportFormat::Luacheck) ? 0 : failed;
}
