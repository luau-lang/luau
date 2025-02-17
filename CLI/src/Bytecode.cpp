// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"
#include "lualib.h"

#include "Luau/CodeGen.h"
#include "Luau/Compiler.h"
#include "Luau/BytecodeBuilder.h"
#include "Luau/Parser.h"
#include "Luau/BytecodeSummary.h"
#include "Luau/FileUtils.h"
#include "Luau/Flags.h"

#include <memory>

using Luau::CodeGen::FunctionBytecodeSummary;

struct GlobalOptions
{
    int optimizationLevel = 1;
    int debugLevel = 1;
} globalOptions;

static Luau::CompileOptions copts()
{
    Luau::CompileOptions result = {};
    result.optimizationLevel = globalOptions.optimizationLevel;
    result.debugLevel = globalOptions.debugLevel;
    result.typeInfoLevel = 1;

    return result;
}

static void displayHelp(const char* argv0)
{
    printf("Usage: %s [options] [file list]\n", argv0);
    printf("\n");
    printf("Available options:\n");
    printf("  -h, --help: Display this usage message.\n");
    printf("  -O<n>: compile with optimization level n (default 1, n should be between 0 and 2).\n");
    printf("  -g<n>: compile with debug level n (default 1, n should be between 0 and 2).\n");
    printf("  --fflags=<fflags>: flags to be enabled.\n");
    printf("  --summary-file=<filename>: file in which bytecode analysis summary will be recorded (default 'bytecode-summary.json').\n");

    exit(0);
}

static bool parseArgs(int argc, char** argv, std::string& summaryFile)
{
    for (int i = 1; i < argc; i++)
    {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0)
        {
            displayHelp(argv[0]);
        }
        else if (strncmp(argv[i], "-O", 2) == 0)
        {
            int level = atoi(argv[i] + 2);
            if (level < 0 || level > 2)
            {
                fprintf(stderr, "Error: Optimization level must be between 0 and 2 inclusive.\n");
                return false;
            }
            globalOptions.optimizationLevel = level;
        }
        else if (strncmp(argv[i], "-g", 2) == 0)
        {
            int level = atoi(argv[i] + 2);
            if (level < 0 || level > 2)
            {
                fprintf(stderr, "Error: Debug level must be between 0 and 2 inclusive.\n");
                return false;
            }
            globalOptions.debugLevel = level;
        }
        else if (strncmp(argv[i], "--summary-file=", 15) == 0)
        {
            summaryFile = argv[i] + 15;

            if (summaryFile.size() == 0)
            {
                fprintf(stderr, "Error: filename missing for '--summary-file'.\n\n");
                return false;
            }
        }
        else if (strncmp(argv[i], "--fflags=", 9) == 0)
        {
            setLuauFlags(argv[i] + 9);
        }
        else if (argv[i][0] == '-')
        {
            fprintf(stderr, "Error: Unrecognized option '%s'.\n\n", argv[i]);
            displayHelp(argv[0]);
        }
    }

    return true;
}

static void report(const char* name, const Luau::Location& location, const char* type, const char* message)
{
    fprintf(stderr, "%s(%d,%d): %s: %s\n", name, location.begin.line + 1, location.begin.column + 1, type, message);
}

static void reportError(const char* name, const Luau::ParseError& error)
{
    report(name, error.getLocation(), "SyntaxError", error.what());
}

static void reportError(const char* name, const Luau::CompileError& error)
{
    report(name, error.getLocation(), "CompileError", error.what());
}

static bool analyzeFile(const char* name, const unsigned nestingLimit, std::vector<FunctionBytecodeSummary>& summaries)
{
    std::optional<std::string> source = readFile(name);

    if (!source)
    {
        fprintf(stderr, "Error opening %s\n", name);
        return false;
    }

    try
    {
        Luau::BytecodeBuilder bcb;

        compileOrThrow(bcb, *source, copts());

        const std::string& bytecode = bcb.getBytecode();

        std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
        lua_State* L = globalState.get();

        if (luau_load(L, name, bytecode.data(), bytecode.size(), 0) == 0)
        {
            summaries = Luau::CodeGen::summarizeBytecode(L, -1, nestingLimit);
            return true;
        }
        else
        {
            fprintf(stderr, "Error loading bytecode %s\n", name);
            return false;
        }
    }
    catch (Luau::ParseErrors& e)
    {
        for (auto& error : e.getErrors())
            reportError(name, error);
        return false;
    }
    catch (Luau::CompileError& e)
    {
        reportError(name, e);
        return false;
    }

    return true;
}

static std::string escapeFilename(const std::string& filename)
{
    std::string escaped;
    escaped.reserve(filename.size());

    for (const char ch : filename)
    {
        switch (ch)
        {
        case '\\':
            escaped.push_back('/');
            break;
        case '"':
            escaped.push_back('\\');
            escaped.push_back(ch);
            break;
        default:
            escaped.push_back(ch);
        }
    }

    return escaped;
}

static void serializeFunctionSummary(const FunctionBytecodeSummary& summary, FILE* fp)
{
    const unsigned nestingLimit = summary.getNestingLimit();
    const unsigned opLimit = summary.getOpLimit();

    fprintf(fp, "        {\n");
    fprintf(fp, "            \"source\": \"%s\",\n", summary.getSource().c_str());
    fprintf(fp, "            \"name\": \"%s\",\n", summary.getName().c_str());
    fprintf(fp, "            \"line\": %d,\n", summary.getLine());
    fprintf(fp, "            \"nestingLimit\": %u,\n", nestingLimit);
    fprintf(fp, "            \"counts\": [");

    for (unsigned nesting = 0; nesting <= nestingLimit; ++nesting)
    {
        fprintf(fp, "\n                [");

        for (unsigned i = 0; i < opLimit; ++i)
        {
            fprintf(fp, "%d", summary.getCount(nesting, uint8_t(i)));
            if (i < opLimit - 1)
                fprintf(fp, ", ");
        }

        fprintf(fp, "]");
        if (nesting < nestingLimit)
            fprintf(fp, ",");
    }

    fprintf(fp, "\n            ]");
    fprintf(fp, "\n        }");
}

static void serializeScriptSummary(const std::string& file, const std::vector<FunctionBytecodeSummary>& scriptSummary, FILE* fp)
{
    std::string escaped(escapeFilename(file));
    const size_t functionCount = scriptSummary.size();

    fprintf(fp, "    \"%s\": [\n", escaped.c_str());

    for (size_t i = 0; i < functionCount; ++i)
    {
        serializeFunctionSummary(scriptSummary[i], fp);
        fprintf(fp, i == (functionCount - 1) ? "\n" : ",\n");
    }

    fprintf(fp, "    ]");
}

static bool serializeSummaries(
    const std::vector<std::string>& files,
    const std::vector<std::vector<FunctionBytecodeSummary>>& scriptSummaries,
    const std::string& summaryFile
)
{

    FILE* fp = fopen(summaryFile.c_str(), "w");
    const size_t fileCount = files.size();

    if (!fp)
    {
        fprintf(stderr, "Unable to open '%s'.\n", summaryFile.c_str());
        return false;
    }

    fprintf(fp, "{\n");

    for (size_t i = 0; i < fileCount; ++i)
    {
        serializeScriptSummary(files[i], scriptSummaries[i], fp);
        fprintf(fp, i < (fileCount - 1) ? ",\n" : "\n");
    }

    fprintf(fp, "}");
    fclose(fp);

    return true;
}

static int assertionHandler(const char* expr, const char* file, int line, const char* function)
{
    printf("%s(%d): ASSERTION FAILED: %s\n", file, line, expr);
    return 1;
}

int main(int argc, char** argv)
{
    Luau::assertHandler() = assertionHandler;

    setLuauFlagsDefault();

    std::string summaryFile("bytecode-summary.json");
    unsigned nestingLimit = 0;

    if (!parseArgs(argc, argv, summaryFile))
        return 1;

    const std::vector<std::string> files = getSourceFiles(argc, argv);
    size_t fileCount = files.size();

    std::vector<std::vector<FunctionBytecodeSummary>> scriptSummaries;
    scriptSummaries.reserve(fileCount);

    for (size_t i = 0; i < fileCount; ++i)
    {
        if (!analyzeFile(files[i].c_str(), nestingLimit, scriptSummaries[i]))
            return 1;
    }

    if (!serializeSummaries(files, scriptSummaries, summaryFile))
        return 1;

    fprintf(stdout, "Bytecode summary written to '%s'\n", summaryFile.c_str());

    return 0;
}
