// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"
#include "lualib.h"

#include "Luau/CodeGen.h"
#include "Luau/Compiler.h"
#include "Luau/BytecodeBuilder.h"
#include "Luau/Parser.h"
#include "Luau/TimeTrace.h"

#include "Luau/FileUtils.h"
#include "Luau/Flags.h"

#include <memory>

#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#endif

LUAU_FASTFLAG(DebugLuauTimeTracing)

enum class CompileFormat
{
    Text,
    Binary,
    Remarks,
    Codegen,        // Prints annotated native code including IR and assembly
    CodegenAsm,     // Prints annotated native code assembly
    CodegenIr,      // Prints annotated native code IR
    CodegenVerbose, // Prints annotated native code including IR, assembly and outlined code
    CodegenNull,
    Null
};

enum class RecordStats
{
    None,
    Total,
    File,
    Function
};

struct GlobalOptions
{
    int optimizationLevel = 1;
    int debugLevel = 1;
    int typeInfoLevel = 0;

    const char* vectorLib = nullptr;
    const char* vectorCtor = nullptr;
    const char* vectorType = nullptr;
} globalOptions;

static Luau::CompileOptions copts()
{
    Luau::CompileOptions result = {};
    result.optimizationLevel = globalOptions.optimizationLevel;
    result.debugLevel = globalOptions.debugLevel;
    result.typeInfoLevel = globalOptions.typeInfoLevel;

    result.vectorLib = globalOptions.vectorLib;
    result.vectorCtor = globalOptions.vectorCtor;
    result.vectorType = globalOptions.vectorType;

    return result;
}

static std::optional<CompileFormat> getCompileFormat(const char* name)
{
    if (strcmp(name, "text") == 0)
        return CompileFormat::Text;
    else if (strcmp(name, "binary") == 0)
        return CompileFormat::Binary;
    else if (strcmp(name, "text") == 0)
        return CompileFormat::Text;
    else if (strcmp(name, "remarks") == 0)
        return CompileFormat::Remarks;
    else if (strcmp(name, "codegen") == 0)
        return CompileFormat::Codegen;
    else if (strcmp(name, "codegenasm") == 0)
        return CompileFormat::CodegenAsm;
    else if (strcmp(name, "codegenir") == 0)
        return CompileFormat::CodegenIr;
    else if (strcmp(name, "codegenverbose") == 0)
        return CompileFormat::CodegenVerbose;
    else if (strcmp(name, "codegennull") == 0)
        return CompileFormat::CodegenNull;
    else if (strcmp(name, "null") == 0)
        return CompileFormat::Null;
    else
        return std::nullopt;
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

static std::string getCodegenAssembly(
    const char* name,
    const std::string& bytecode,
    Luau::CodeGen::AssemblyOptions options,
    Luau::CodeGen::LoweringStats* stats
)
{
    std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    if (luau_load(L, name, bytecode.data(), bytecode.size(), 0) == 0)
        return Luau::CodeGen::getAssembly(L, -1, options, stats);

    fprintf(stderr, "Error loading bytecode %s\n", name);
    return "";
}

static void annotateInstruction(void* context, std::string& text, int fid, int instpos)
{
    Luau::BytecodeBuilder& bcb = *(Luau::BytecodeBuilder*)context;

    bcb.annotateInstruction(text, fid, instpos);
}

struct CompileStats
{
    size_t lines;
    size_t bytecode;
    size_t bytecodeInstructionCount;
    size_t codegen;

    double readTime;
    double miscTime;
    double parseTime;
    double compileTime;
    double codegenTime;

    Luau::CodeGen::LoweringStats lowerStats;

    CompileStats& operator+=(const CompileStats& that)
    {
        this->lines += that.lines;
        this->bytecode += that.bytecode;
        this->bytecodeInstructionCount += that.bytecodeInstructionCount;
        this->codegen += that.codegen;
        this->readTime += that.readTime;
        this->miscTime += that.miscTime;
        this->parseTime += that.parseTime;
        this->compileTime += that.compileTime;
        this->codegenTime += that.codegenTime;
        this->lowerStats += that.lowerStats;

        return *this;
    }

    CompileStats operator+(const CompileStats& other) const
    {
        CompileStats result(*this);
        result += other;
        return result;
    }
};

#define WRITE_NAME(INDENT, NAME) fprintf(fp, INDENT "\"" #NAME "\": ")
#define WRITE_PAIR(INDENT, NAME, FORMAT) fprintf(fp, INDENT "\"" #NAME "\": " FORMAT, stats.NAME)
#define WRITE_PAIR_STRING(INDENT, NAME, FORMAT) fprintf(fp, INDENT "\"" #NAME "\": " FORMAT, stats.NAME.c_str())

void serializeFunctionStats(FILE* fp, const Luau::CodeGen::FunctionStats& stats)
{
    fprintf(fp, "                {\n");
    WRITE_PAIR_STRING("                    ", name, "\"%s\",\n");
    WRITE_PAIR("                    ", line, "%d,\n");
    WRITE_PAIR("                    ", bcodeCount, "%u,\n");
    WRITE_PAIR("                    ", irCount, "%u,\n");
    WRITE_PAIR("                    ", asmCount, "%u,\n");
    WRITE_PAIR("                    ", asmSize, "%u,\n");

    WRITE_NAME("                    ", bytecodeSummary);
    const size_t nestingLimit = stats.bytecodeSummary.size();

    if (nestingLimit == 0)
        fprintf(fp, "[]");
    else
    {
        fprintf(fp, "[\n");
        for (size_t i = 0; i < nestingLimit; ++i)
        {
            const std::vector<unsigned>& counts = stats.bytecodeSummary[i];
            fprintf(fp, "                        [");
            for (size_t j = 0; j < counts.size(); ++j)
            {
                fprintf(fp, "%u", counts[j]);
                if (j < counts.size() - 1)
                    fprintf(fp, ", ");
            }
            fprintf(fp, "]");
            if (i < stats.bytecodeSummary.size() - 1)
                fprintf(fp, ",\n");
        }
        fprintf(fp, "\n                    ]");
    }

    fprintf(fp, "\n                }");
}

void serializeBlockLinearizationStats(FILE* fp, const Luau::CodeGen::BlockLinearizationStats& stats)
{
    fprintf(fp, "{\n");

    WRITE_PAIR("                ", constPropInstructionCount, "%u,\n");
    WRITE_PAIR("                ", timeSeconds, "%f\n");

    fprintf(fp, "            }");
}

void serializeLoweringStats(FILE* fp, const Luau::CodeGen::LoweringStats& stats)
{
    fprintf(fp, "{\n");

    WRITE_PAIR("            ", totalFunctions, "%u,\n");
    WRITE_PAIR("            ", skippedFunctions, "%u,\n");
    WRITE_PAIR("            ", spillsToSlot, "%d,\n");
    WRITE_PAIR("            ", spillsToRestore, "%d,\n");
    WRITE_PAIR("            ", maxSpillSlotsUsed, "%u,\n");
    WRITE_PAIR("            ", blocksPreOpt, "%u,\n");
    WRITE_PAIR("            ", blocksPostOpt, "%u,\n");
    WRITE_PAIR("            ", maxBlockInstructions, "%u,\n");
    WRITE_PAIR("            ", regAllocErrors, "%d,\n");
    WRITE_PAIR("            ", loweringErrors, "%d,\n");

    WRITE_NAME("            ", blockLinearizationStats);
    serializeBlockLinearizationStats(fp, stats.blockLinearizationStats);
    fprintf(fp, ",\n");

    WRITE_NAME("            ", functions);
    const size_t functionCount = stats.functions.size();

    if (functionCount == 0)
        fprintf(fp, "[]");
    else
    {
        fprintf(fp, "[\n");
        for (size_t i = 0; i < functionCount; ++i)
        {
            serializeFunctionStats(fp, stats.functions[i]);
            if (i < functionCount - 1)
                fprintf(fp, ",\n");
        }
        fprintf(fp, "\n            ]");
    }

    fprintf(fp, "\n        }");
}

void serializeCompileStats(FILE* fp, const CompileStats& stats)
{
    fprintf(fp, "{\n");

    WRITE_PAIR("        ", lines, "%zu,\n");
    WRITE_PAIR("        ", bytecode, "%zu,\n");
    WRITE_PAIR("        ", bytecodeInstructionCount, "%zu,\n");
    WRITE_PAIR("        ", codegen, "%zu,\n");
    WRITE_PAIR("        ", readTime, "%f,\n");
    WRITE_PAIR("        ", miscTime, "%f,\n");
    WRITE_PAIR("        ", parseTime, "%f,\n");
    WRITE_PAIR("        ", compileTime, "%f,\n");
    WRITE_PAIR("        ", codegenTime, "%f,\n");

    WRITE_NAME("        ", lowerStats);
    serializeLoweringStats(fp, stats.lowerStats);

    fprintf(fp, "\n    }");
}

#undef WRITE_NAME
#undef WRITE_PAIR
#undef WRITE_PAIR_STRING

static double recordDeltaTime(double& timer)
{
    double now = Luau::TimeTrace::getClock();
    double delta = now - timer;
    timer = now;
    return delta;
}

static bool compileFile(const char* name, CompileFormat format, Luau::CodeGen::AssemblyOptions::Target assemblyTarget, CompileStats& stats)
{
    double currts = Luau::TimeTrace::getClock();

    std::optional<std::string> source = readFile(name);
    if (!source)
    {
        fprintf(stderr, "Error opening %s\n", name);
        return false;
    }

    stats.readTime += recordDeltaTime(currts);

    // NOTE: Normally, you should use Luau::compile or luau_compile (see lua_require as an example)
    // This function is much more complicated because it supports many output human-readable formats through internal interfaces

    try
    {
        Luau::BytecodeBuilder bcb;

        Luau::CodeGen::AssemblyOptions options;
        options.target = assemblyTarget;
        options.outputBinary = format == CompileFormat::CodegenNull;

        if (!options.outputBinary)
        {
            options.includeAssembly = format != CompileFormat::CodegenIr;
            options.includeIr = format != CompileFormat::CodegenAsm;
            options.includeIrTypes = format != CompileFormat::CodegenAsm;
            options.includeOutlinedCode = format == CompileFormat::CodegenVerbose;
        }

        options.annotator = annotateInstruction;
        options.annotatorContext = &bcb;

        if (format == CompileFormat::Text)
        {
            bcb.setDumpFlags(
                Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Source | Luau::BytecodeBuilder::Dump_Locals |
                Luau::BytecodeBuilder::Dump_Remarks | Luau::BytecodeBuilder::Dump_Types
            );
            bcb.setDumpSource(*source);
        }
        else if (format == CompileFormat::Remarks)
        {
            bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Source | Luau::BytecodeBuilder::Dump_Remarks);
            bcb.setDumpSource(*source);
        }
        else if (format == CompileFormat::Codegen || format == CompileFormat::CodegenAsm || format == CompileFormat::CodegenIr ||
                 format == CompileFormat::CodegenVerbose)
        {
            bcb.setDumpFlags(
                Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Source | Luau::BytecodeBuilder::Dump_Locals |
                Luau::BytecodeBuilder::Dump_Remarks
            );
            bcb.setDumpSource(*source);
        }

        stats.miscTime += recordDeltaTime(currts);

        Luau::Allocator allocator;
        Luau::AstNameTable names(allocator);
        Luau::ParseResult result = Luau::Parser::parse(source->c_str(), source->size(), names, allocator);

        if (!result.errors.empty())
            throw Luau::ParseErrors(result.errors);

        stats.lines += result.lines;
        stats.parseTime += recordDeltaTime(currts);

        Luau::compileOrThrow(bcb, result, names, copts());
        stats.bytecode += bcb.getBytecode().size();
        stats.bytecodeInstructionCount = bcb.getTotalInstructionCount();
        stats.compileTime += recordDeltaTime(currts);

        switch (format)
        {
        case CompileFormat::Text:
            printf("%s", bcb.dumpEverything().c_str());
            break;
        case CompileFormat::Remarks:
            printf("%s", bcb.dumpSourceRemarks().c_str());
            break;
        case CompileFormat::Binary:
            fwrite(bcb.getBytecode().data(), 1, bcb.getBytecode().size(), stdout);
            break;
        case CompileFormat::Codegen:
        case CompileFormat::CodegenAsm:
        case CompileFormat::CodegenIr:
        case CompileFormat::CodegenVerbose:
            printf("%s", getCodegenAssembly(name, bcb.getBytecode(), options, &stats.lowerStats).c_str());
            break;
        case CompileFormat::CodegenNull:
            stats.codegen += getCodegenAssembly(name, bcb.getBytecode(), options, &stats.lowerStats).size();
            stats.codegenTime += recordDeltaTime(currts);
            break;
        case CompileFormat::Null:
            break;
        }

        return true;
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
}

static void displayHelp(const char* argv0)
{
    printf("Usage: %s [--mode] [options] [file list]\n", argv0);
    printf("\n");
    printf("Available modes:\n");
    printf("   binary, text, remarks, codegen\n");
    printf("\n");
    printf("Available options:\n");
    printf("  -h, --help: Display this usage message.\n");
    printf("  -O<n>: compile with optimization level n (default 1, n should be between 0 and 2).\n");
    printf("  -g<n>: compile with debug level n (default 1, n should be between 0 and 2).\n");
    printf("  --target=<target>: compile code for specific architecture (a64, x64, a64_nf, x64_ms).\n");
    printf("  --timetrace: record compiler time tracing information into trace.json\n");
    printf("  --record-stats=<granularity>: granularity of compilation stats (total, file, function).\n");
    printf("  --bytecode-summary: Compute bytecode operation distribution.\n");
    printf("  --stats-file=<filename>: file in which compilation stats will be recored (default 'stats.json').\n");
    printf("  --vector-lib=<name>: name of the library providing vector type operations.\n");
    printf("  --vector-ctor=<name>: name of the function constructing a vector value.\n");
    printf("  --vector-type=<name>: name of the vector type.\n");
}

static int assertionHandler(const char* expr, const char* file, int line, const char* function)
{
    printf("%s(%d): ASSERTION FAILED: %s\n", file, line, expr);
    return 1;
}

std::string escapeFilename(const std::string& filename)
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

int main(int argc, char** argv)
{
    Luau::assertHandler() = assertionHandler;

    setLuauFlagsDefault();

    CompileFormat compileFormat = CompileFormat::Text;
    Luau::CodeGen::AssemblyOptions::Target assemblyTarget = Luau::CodeGen::AssemblyOptions::Host;
    RecordStats recordStats = RecordStats::None;
    std::string statsFile("stats.json");
    bool bytecodeSummary = false;

    for (int i = 1; i < argc; i++)
    {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0)
        {
            displayHelp(argv[0]);
            return 0;
        }
        else if (strncmp(argv[i], "-O", 2) == 0)
        {
            int level = atoi(argv[i] + 2);
            if (level < 0 || level > 2)
            {
                fprintf(stderr, "Error: Optimization level must be between 0 and 2 inclusive.\n");
                return 1;
            }
            globalOptions.optimizationLevel = level;
        }
        else if (strncmp(argv[i], "-g", 2) == 0)
        {
            int level = atoi(argv[i] + 2);
            if (level < 0 || level > 2)
            {
                fprintf(stderr, "Error: Debug level must be between 0 and 2 inclusive.\n");
                return 1;
            }
            globalOptions.debugLevel = level;
        }
        else if (strncmp(argv[i], "-t", 2) == 0)
        {
            int level = atoi(argv[i] + 2);
            if (level < 0 || level > 1)
            {
                fprintf(stderr, "Error: Type info level must be between 0 and 1 inclusive.\n");
                return 1;
            }
            globalOptions.typeInfoLevel = level;
        }
        else if (strncmp(argv[i], "--target=", 9) == 0)
        {
            const char* value = argv[i] + 9;

            if (strcmp(value, "a64") == 0)
                assemblyTarget = Luau::CodeGen::AssemblyOptions::A64;
            else if (strcmp(value, "a64_nf") == 0)
                assemblyTarget = Luau::CodeGen::AssemblyOptions::A64_NoFeatures;
            else if (strcmp(value, "x64") == 0)
                assemblyTarget = Luau::CodeGen::AssemblyOptions::X64_SystemV;
            else if (strcmp(value, "x64_ms") == 0)
                assemblyTarget = Luau::CodeGen::AssemblyOptions::X64_Windows;
            else
            {
                fprintf(stderr, "Error: unknown target\n");
                return 1;
            }
        }
        else if (strcmp(argv[i], "--timetrace") == 0)
        {
            FFlag::DebugLuauTimeTracing.value = true;
        }
        else if (strncmp(argv[i], "--record-stats=", 15) == 0)
        {
            const char* value = argv[i] + 15;

            if (strcmp(value, "total") == 0)
                recordStats = RecordStats::Total;
            else if (strcmp(value, "file") == 0)
                recordStats = RecordStats::File;
            else if (strcmp(value, "function") == 0)
                recordStats = RecordStats::Function;
            else
            {
                fprintf(stderr, "Error: unknown 'granularity' for '--record-stats'.\n");
                return 1;
            }
        }
        else if (strncmp(argv[i], "--bytecode-summary", 18) == 0)
        {
            bytecodeSummary = true;
        }
        else if (strncmp(argv[i], "--stats-file=", 13) == 0)
        {
            statsFile = argv[i] + 13;

            if (statsFile.size() == 0)
            {
                fprintf(stderr, "Error: filename missing for '--stats-file'.\n\n");
                return 1;
            }
        }
        else if (strncmp(argv[i], "--fflags=", 9) == 0)
        {
            setLuauFlags(argv[i] + 9);
        }
        else if (strncmp(argv[i], "--vector-lib=", 13) == 0)
        {
            globalOptions.vectorLib = argv[i] + 13;
        }
        else if (strncmp(argv[i], "--vector-ctor=", 14) == 0)
        {
            globalOptions.vectorCtor = argv[i] + 14;
        }
        else if (strncmp(argv[i], "--vector-type=", 14) == 0)
        {
            globalOptions.vectorType = argv[i] + 14;
        }
        else if (argv[i][0] == '-' && argv[i][1] == '-' && getCompileFormat(argv[i] + 2))
        {
            compileFormat = *getCompileFormat(argv[i] + 2);
        }
        else if (argv[i][0] == '-')
        {
            fprintf(stderr, "Error: Unrecognized option '%s'.\n\n", argv[i]);
            displayHelp(argv[0]);
            return 1;
        }
    }

    if (bytecodeSummary && (recordStats != RecordStats::Function))
    {
        fprintf(stderr, "'Error: Required '--record-stats=function' for '--bytecode-summary'.\n");
        return 1;
    }

#if !defined(LUAU_ENABLE_TIME_TRACE)
    if (FFlag::DebugLuauTimeTracing)
    {
        fprintf(stderr, "To run with --timetrace, Luau has to be built with LUAU_ENABLE_TIME_TRACE enabled\n");
        return 1;
    }
#endif

    const std::vector<std::string> files = getSourceFiles(argc, argv);

#ifdef _WIN32
    if (compileFormat == CompileFormat::Binary)
        _setmode(_fileno(stdout), _O_BINARY);
#endif

    const size_t fileCount = files.size();
    CompileStats stats = {};

    std::vector<CompileStats> fileStats;
    if (recordStats == RecordStats::File || recordStats == RecordStats::Function)
        fileStats.reserve(fileCount);

    int failed = 0;
    unsigned functionStats = (recordStats == RecordStats::Function ? Luau::CodeGen::FunctionStats_Enable : 0) |
                             (bytecodeSummary ? Luau::CodeGen::FunctionStats_BytecodeSummary : 0);
    for (const std::string& path : files)
    {
        CompileStats fileStat = {};
        fileStat.lowerStats.functionStatsFlags = functionStats;
        failed += !compileFile(path.c_str(), compileFormat, assemblyTarget, fileStat);
        stats += fileStat;
        if (recordStats == RecordStats::File || recordStats == RecordStats::Function)
            fileStats.push_back(fileStat);
    }

    if (compileFormat == CompileFormat::Null)
    {
        printf(
            "Compiled %d KLOC into %d KB bytecode (read %.2fs, parse %.2fs, compile %.2fs)\n",
            int(stats.lines / 1000),
            int(stats.bytecode / 1024),
            stats.readTime,
            stats.parseTime,
            stats.compileTime
        );
    }
    else if (compileFormat == CompileFormat::CodegenNull)
    {
        printf(
            "Compiled %d KLOC into %d KB bytecode => %d KB native code (%.2fx) (read %.2fs, parse %.2fs, compile %.2fs, codegen %.2fs)\n",
            int(stats.lines / 1000),
            int(stats.bytecode / 1024),
            int(stats.codegen / 1024),
            stats.bytecode == 0 ? 0.0 : double(stats.codegen) / double(stats.bytecode),
            stats.readTime,
            stats.parseTime,
            stats.compileTime,
            stats.codegenTime
        );

        printf(
            "Lowering: regalloc failed: %d, lowering failed %d; spills to stack: %d, spills to restore: %d, max spill slot %u\n",
            stats.lowerStats.regAllocErrors,
            stats.lowerStats.loweringErrors,
            stats.lowerStats.spillsToSlot,
            stats.lowerStats.spillsToRestore,
            stats.lowerStats.maxSpillSlotsUsed
        );
    }

    if (recordStats != RecordStats::None)
    {
        FILE* fp = fopen(statsFile.c_str(), "w");

        if (!fp)
        {
            fprintf(stderr, "Unable to open 'stats.json'\n");
            return 1;
        }

        if (recordStats == RecordStats::Total)
        {
            serializeCompileStats(fp, stats);
        }
        else if (recordStats == RecordStats::File || recordStats == RecordStats::Function)
        {
            fprintf(fp, "{\n");
            for (size_t i = 0; i < fileCount; ++i)
            {
                std::string escaped(escapeFilename(files[i]));
                fprintf(fp, "    \"%s\": ", escaped.c_str());
                serializeCompileStats(fp, fileStats[i]);
                fprintf(fp, i == (fileCount - 1) ? "\n" : ",\n");
            }
            fprintf(fp, "}");
        }

        fclose(fp);
    }

    return failed ? 1 : 0;
}
