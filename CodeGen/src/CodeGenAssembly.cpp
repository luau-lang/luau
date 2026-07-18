// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BytecodeAnalysis.h"
#include "Luau/BytecodeSummary.h"
#include "Luau/IrDump.h"
#include "Luau/IrUtils.h"

#include "CodeGenLower.h"

#include "CodeGenA64.h"
#include "CodeGenX64.h"

#include "lapi.h"

LUAU_FASTFLAG(LuauCodegenSharedLog)

namespace Luau
{
namespace CodeGen
{

static const LocVar* tryFindLocal(const Proto* proto, int reg, int pcpos)
{
    for (int i = 0; i < proto->sizelocvars; i++)
    {
        const LocVar& local = proto->locvars[i];

        if (reg == local.reg && pcpos >= local.startpc && pcpos < local.endpc)
            return &local;
    }

    return nullptr;
}

const char* tryFindLocalName(const Proto* proto, int reg, int pcpos)
{
    const LocVar* var = tryFindLocal(proto, reg, pcpos);

    if (var && var->varname)
        return getstr(var->varname);

    return nullptr;
}

const char* tryFindUpvalueName(const Proto* proto, int upval)
{
    if (proto->upvalues)
    {
        CODEGEN_ASSERT(upval < proto->sizeupvalues);

        if (proto->upvalues[upval])
            return getstr(proto->upvalues[upval]);
    }

    return nullptr;
}

template<typename AssemblyBuilder>
static void logFunctionHeader_DEPRECATED(AssemblyBuilder& build, Proto* proto)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenSharedLog);

    if (proto->debugname)
        build.logAppend("; function %s(", getstr(proto->debugname));
    else
        build.logAppend("; function(");

    for (int i = 0; i < proto->numparams; i++)
    {
        if (const char* name = tryFindLocalName(proto, i, 0))
            build.logAppend("%s%s", i == 0 ? "" : ", ", name);
        else
            build.logAppend("%s$arg%d", i == 0 ? "" : ", ", i);
    }

    if (proto->numparams != 0 && proto->is_vararg)
        build.logAppend(", ...)");
    else
        build.logAppend(")");

    if (proto->linedefined >= 0)
        build.logAppend(" line %d\n", proto->linedefined);
    else
        build.logAppend("\n");
}

static void logFunctionHeader(LogBuilder& logger, Proto* proto)
{
    CODEGEN_ASSERT(FFlag::LuauCodegenSharedLog);

    if (proto->debugname)
        logger.formatAppend("; function %s(", getstr(proto->debugname));
    else
        logger.formatAppend("; function(");

    for (int i = 0; i < proto->numparams; i++)
    {
        if (const char* name = tryFindLocalName(proto, i, 0))
            logger.formatAppend("%s%s", i == 0 ? "" : ", ", name);
        else
            logger.formatAppend("%s$arg%d", i == 0 ? "" : ", ", i);
    }

    if (proto->numparams != 0 && proto->is_vararg)
        logger.formatAppend(", ...)");
    else
        logger.formatAppend(")");

    if (proto->linedefined >= 0)
        logger.formatAppend(" line %d\n", proto->linedefined);
    else
        logger.formatAppend("\n");
}

template<typename AssemblyBuilder>
static void logFunctionTypes_DEPRECATED(AssemblyBuilder& build, const IrFunction& function, const char* const* userdataTypes)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenSharedLog);

    const BytecodeTypeInfo& typeInfo = function.bcTypeInfo;

    for (size_t i = 0; i < typeInfo.argumentTypes.size(); i++)
    {
        uint8_t ty = typeInfo.argumentTypes[i];

        const char* type = getBytecodeTypeName(ty, userdataTypes);
        const char* optional = (ty & LBC_TYPE_OPTIONAL_BIT) != 0 ? "?" : "";

        if (ty != LBC_TYPE_ANY)
        {
            if (const char* name = tryFindLocalName(function.proto, int(i), 0))
                build.logAppend("; R%d: %s%s [argument '%s']\n", int(i), type, optional, name);
            else
                build.logAppend("; R%d: %s%s [argument]\n", int(i), type, optional);
        }
    }

    for (size_t i = 0; i < typeInfo.upvalueTypes.size(); i++)
    {
        uint8_t ty = typeInfo.upvalueTypes[i];

        const char* type = getBytecodeTypeName(ty, userdataTypes);
        const char* optional = (ty & LBC_TYPE_OPTIONAL_BIT) != 0 ? "?" : "";

        if (ty != LBC_TYPE_ANY)
        {
            if (const char* name = tryFindUpvalueName(function.proto, int(i)))
                build.logAppend("; U%d: %s%s ['%s']\n", int(i), type, optional, name);
            else
                build.logAppend("; U%d: %s%s\n", int(i), type, optional);
        }
    }

    for (const BytecodeRegTypeInfo& el : typeInfo.regTypes)
    {
        const char* type = getBytecodeTypeName(el.type, userdataTypes);
        const char* optional = (el.type & LBC_TYPE_OPTIONAL_BIT) != 0 ? "?" : "";

        // Using last active position as the PC because 'startpc' for type info is before local is initialized
        if (const char* name = tryFindLocalName(function.proto, el.reg, el.endpc - 1))
            build.logAppend("; R%d: %s%s from %d to %d [local '%s']\n", el.reg, type, optional, el.startpc, el.endpc, name);
        else
            build.logAppend("; R%d: %s%s from %d to %d\n", el.reg, type, optional, el.startpc, el.endpc);
    }
}

static void logFunctionTypes(LogBuilder& logger, const IrFunction& function, const char* const* userdataTypes)
{
    CODEGEN_ASSERT(FFlag::LuauCodegenSharedLog);

    const BytecodeTypeInfo& typeInfo = function.bcTypeInfo;

    for (size_t i = 0; i < typeInfo.argumentTypes.size(); i++)
    {
        uint8_t ty = typeInfo.argumentTypes[i];

        const char* type = getBytecodeTypeName(ty, userdataTypes);
        const char* optional = (ty & LBC_TYPE_OPTIONAL_BIT) != 0 ? "?" : "";

        if (ty != LBC_TYPE_ANY)
        {
            if (const char* name = tryFindLocalName(function.proto, int(i), 0))
                logger.formatAppend("; R%d: %s%s [argument '%s']\n", int(i), type, optional, name);
            else
                logger.formatAppend("; R%d: %s%s [argument]\n", int(i), type, optional);
        }
    }

    for (size_t i = 0; i < typeInfo.upvalueTypes.size(); i++)
    {
        uint8_t ty = typeInfo.upvalueTypes[i];

        const char* type = getBytecodeTypeName(ty, userdataTypes);
        const char* optional = (ty & LBC_TYPE_OPTIONAL_BIT) != 0 ? "?" : "";

        if (ty != LBC_TYPE_ANY)
        {
            if (const char* name = tryFindUpvalueName(function.proto, int(i)))
                logger.formatAppend("; U%d: %s%s ['%s']\n", int(i), type, optional, name);
            else
                logger.formatAppend("; U%d: %s%s\n", int(i), type, optional);
        }
    }

    for (const BytecodeRegTypeInfo& el : typeInfo.regTypes)
    {
        const char* type = getBytecodeTypeName(el.type, userdataTypes);
        const char* optional = (el.type & LBC_TYPE_OPTIONAL_BIT) != 0 ? "?" : "";

        // Using last active position as the PC because 'startpc' for type info is before local is initialized
        if (const char* name = tryFindLocalName(function.proto, el.reg, el.endpc - 1))
            logger.formatAppend("; R%d: %s%s from %d to %d [local '%s']\n", el.reg, type, optional, el.startpc, el.endpc, name);
        else
            logger.formatAppend("; R%d: %s%s from %d to %d\n", el.reg, type, optional, el.startpc, el.endpc);
    }
}

unsigned getInstructionCount(const Instruction* insns, const unsigned size)
{
    unsigned count = 0;
    for (unsigned i = 0; i < size;)
    {
        ++count;
        i += getOpLength(LuauOpcode(LUAU_INSN_OP(insns[i])));
    }
    return count;
}

template<typename AssemblyBuilder>
static std::string getAssemblyImpl(LogBuilder& logger, AssemblyBuilder& build, const TValue* func, AssemblyOptions options, LoweringStats* stats)
{
    Proto* root = clvalue(func)->l.p;

    if ((options.compilationOptions.flags & CodeGen_OnlyNativeModules) != 0 && (root->flags & LPF_NATIVE_MODULE) == 0)
    {
        build.finalize();
        return std::string();
    }

    std::vector<Proto*> protos;
    gatherFunctions(protos, root, options.compilationOptions.flags, root->flags & LPF_NATIVE_FUNCTION);

    protos.erase(
        std::remove_if(
            protos.begin(),
            protos.end(),
            [](Proto* p)
            {
                return p == nullptr;
            }
        ),
        protos.end()
    );

    if (stats)
        stats->totalFunctions += unsigned(protos.size());

    if (protos.empty())
    {
        build.finalize();
        return std::string();
    }

    ModuleHelpers helpers;
    assembleHelpers(options.includeAssembly ? &logger : nullptr, build, helpers);

    if (!options.includeOutlinedCode && options.includeAssembly)
    {
        if (FFlag::LuauCodegenSharedLog)
        {
            logger.text.clear();
            logger.formatAppend("; skipping %u bytes of outlined helpers\n", unsigned(build.getCodeSize() * sizeof(build.code[0])));
        }
        else
        {
            build.text.clear();
            build.logAppend("; skipping %u bytes of outlined helpers\n", unsigned(build.getCodeSize() * sizeof(build.code[0])));
        }
    }

    for (Proto* p : protos)
    {
        IrBuilder ir(options.compilationOptions.hooks);
        ir.buildFunctionIr(p);
        unsigned asmSize = build.getCodeSize();
        unsigned asmCount = build.getInstructionCount();

        if (options.includeAssembly || options.includeIr)
        {
            if (FFlag::LuauCodegenSharedLog)
                logFunctionHeader(logger, p);
            else
                logFunctionHeader_DEPRECATED(build, p);
        }

        if (options.includeIrTypes)
        {
            if (FFlag::LuauCodegenSharedLog)
                logFunctionTypes(logger, ir.function, options.compilationOptions.userdataTypes);
            else
                logFunctionTypes_DEPRECATED(build, ir.function, options.compilationOptions.userdataTypes);
        }

        CodeGenCompilationResult result = CodeGenCompilationResult::Success;

        if (!lowerFunction(ir, &logger, build, helpers, p, options, stats, result))
        {
            if (FFlag::LuauCodegenSharedLog)
            {
                if (options.includeAssembly)
                    logger.formatAppend("; skipping (can't lower)\n");
            }
            else
            {
                if (build.logText)
                    build.logAppend("; skipping (can't lower)\n");
            }

            asmSize = 0;
            asmCount = 0;

            if (stats)
                stats->skippedFunctions += 1;
        }
        else
        {
            asmSize = build.getCodeSize() - asmSize;
            asmCount = build.getInstructionCount() - asmCount;
        }

        if (stats && (stats->functionStatsFlags & FunctionStats_Enable))
        {
            FunctionStats functionStat;

            // function name is empty for anonymous and pseudo top-level functions
            // properly name pseudo top-level function because it will be compiled natively if it has loops
            functionStat.name = p->debugname ? getstr(p->debugname) : p->bytecodeid == root->bytecodeid ? "[top level]" : "[anonymous]";
            functionStat.line = p->linedefined;
            functionStat.bcodeCount = getInstructionCount(p->code, p->sizecode);
            functionStat.irCount = unsigned(ir.function.instructions.size());
            functionStat.asmSize = asmSize * sizeof(build.code[0]);
            functionStat.asmCount = asmCount;
            if (stats->functionStatsFlags & FunctionStats_BytecodeSummary)
            {
                FunctionBytecodeSummary summary(FunctionBytecodeSummary::fromProto(p, 0));
                functionStat.bytecodeSummary.push_back(summary.getCounts(0));
            }
            stats->functions.push_back(std::move(functionStat));
        }

        if (FFlag::LuauCodegenSharedLog)
        {
            if (options.includeAssembly)
                logger.formatAppend("\n");
        }
        else
        {
            if (build.logText)
                build.logAppend("\n");
        }
    }

    if (!build.finalize())
        return std::string();

    if (options.outputBinary)
        return std::string(reinterpret_cast<const char*>(build.code.data()), reinterpret_cast<const char*>(build.code.data() + build.code.size())) +
               std::string(build.data.begin(), build.data.end());
    else
        return FFlag::LuauCodegenSharedLog ? logger.text : build.text;
}

#if defined(CODEGEN_TARGET_A64)
unsigned int getCpuFeaturesA64();
#else
unsigned int getCpuFeaturesX64();
#endif

std::string getAssembly(lua_State* L, int idx, AssemblyOptions options, LoweringStats* stats)
{
    CODEGEN_ASSERT(lua_isLfunction(L, idx));
    const TValue* func = luaA_toobject(L, idx);

    LogBuilder logger(options);

    switch (options.target)
    {
    case AssemblyOptions::Host:
    {
#if defined(CODEGEN_TARGET_A64)
        static unsigned int cpuFeatures = getCpuFeaturesA64();
        A64::AssemblyBuilderA64 build(/* logger= */ options.includeAssembly ? &logger : nullptr, options.includeAssembly, cpuFeatures);
#else
        static unsigned int cpuFeatures = getCpuFeaturesX64();
        X64::AssemblyBuilderX64 build(/* logger= */ options.includeAssembly ? &logger : nullptr, options.includeAssembly, cpuFeatures);
#endif

        return getAssemblyImpl(logger, build, func, options, stats);
    }

    case AssemblyOptions::A64:
    {
        A64::AssemblyBuilderA64 build(/* logger= */ options.includeAssembly ? &logger : nullptr,
                                      options.includeAssembly,
                                      /* features= */ A64::Feature_JSCVT);

        return getAssemblyImpl(logger, build, func, options, stats);
    }

    case AssemblyOptions::A64_NoFeatures:
    {
        A64::AssemblyBuilderA64 build(/* logger= */ options.includeAssembly ? &logger : nullptr, options.includeAssembly, /* features= */ 0);

        return getAssemblyImpl(logger, build, func, options, stats);
    }

    case AssemblyOptions::X64_Windows:
    {
        X64::AssemblyBuilderX64 build(
            /* logger= */ options.includeAssembly ? &logger : nullptr, options.includeAssembly, X64::ABIX64::Windows, /* features= */ 0
        );

        return getAssemblyImpl(logger, build, func, options, stats);
    }

    case AssemblyOptions::X64_SystemV:
    {
        X64::AssemblyBuilderX64 build(
            /* logger= */ options.includeAssembly ? &logger : nullptr, options.includeAssembly, X64::ABIX64::SystemV, /* features= */ 0
        );

        return getAssemblyImpl(logger, build, func, options, stats);
    }

    default:
        CODEGEN_ASSERT(!"Unknown target");
        return std::string();
    }
}

template<typename AssemblyBuilder>
static std::string getAssemblyFromIrImpl(LogBuilder& logger, AssemblyBuilder& build, IrBuilder& ir, AssemblyOptions options, LoweringStats* stats)
{
    ModuleHelpers helpers;
    assembleHelpers(options.includeAssembly ? &logger : nullptr, build, helpers);

    if (!options.includeOutlinedCode && options.includeAssembly)
    {
        if (FFlag::LuauCodegenSharedLog)
        {
            logger.text.clear();
            logger.formatAppend("; skipping %u bytes of outlined helpers\n", unsigned(build.getCodeSize() * sizeof(build.code[0])));
        }
        else
        {
            build.text.clear();
            build.logAppend("; skipping %u bytes of outlined helpers\n", unsigned(build.getCodeSize() * sizeof(build.code[0])));
        }
    }

    CodeGenCompilationResult result = CodeGenCompilationResult::Success;

    if (!lowerFunction(ir, &logger, build, helpers, /* proto */ nullptr, options, stats, result))
    {
        if (FFlag::LuauCodegenSharedLog)
        {
            if (options.includeAssembly)
                logger.formatAppend("; skipping (can't lower)\n");
        }
        else
        {
            if (build.logText)
                build.logAppend("; skipping (can't lower)\n");
        }
    }

    if (FFlag::LuauCodegenSharedLog)
    {
        if (options.includeAssembly)
            logger.formatAppend("\n");
    }
    else
    {
        if (build.logText)
            build.logAppend("\n");
    }

    if (!build.finalize())
        return std::string();

    if (options.outputBinary)
        return std::string(reinterpret_cast<const char*>(build.code.data()), reinterpret_cast<const char*>(build.code.data() + build.code.size())) +
               std::string(build.data.begin(), build.data.end());
    else
        return FFlag::LuauCodegenSharedLog ? logger.text : build.text;
}

std::string getAssemblyFromIr(IrBuilder& ir, AssemblyOptions options, LoweringStats* stats)
{
    LogBuilder logger(options);

    switch (options.target)
    {
    case AssemblyOptions::Host:
    {
#if defined(CODEGEN_TARGET_A64)
        static unsigned int cpuFeatures = getCpuFeaturesA64();
        A64::AssemblyBuilderA64 build(/* logger= */ options.includeAssembly ? &logger : nullptr, options.includeAssembly, cpuFeatures);
#else
        static unsigned int cpuFeatures = getCpuFeaturesX64();
        X64::AssemblyBuilderX64 build(/* logger= */ options.includeAssembly ? &logger : nullptr, options.includeAssembly, cpuFeatures);
#endif

        return getAssemblyFromIrImpl(logger, build, ir, options, stats);
    }

    case AssemblyOptions::A64:
    {
        A64::AssemblyBuilderA64 build(/* logger= */ options.includeAssembly ? &logger : nullptr,
                                      options.includeAssembly,
                                      /* features= */ A64::Feature_JSCVT);

        return getAssemblyFromIrImpl(logger, build, ir, options, stats);
    }

    case AssemblyOptions::A64_NoFeatures:
    {
        A64::AssemblyBuilderA64 build(/* logger= */ options.includeAssembly ? &logger : nullptr, options.includeAssembly, /* features= */ 0);

        return getAssemblyFromIrImpl(logger, build, ir, options, stats);
    }

    case AssemblyOptions::X64_Windows:
    {
        X64::AssemblyBuilderX64 build(
            /* logger= */ options.includeAssembly ? &logger : nullptr, options.includeAssembly, X64::ABIX64::Windows, /* features= */ 0
        );

        return getAssemblyFromIrImpl(logger, build, ir, options, stats);
    }

    case AssemblyOptions::X64_SystemV:
    {
        X64::AssemblyBuilderX64 build(
            /* logger= */ options.includeAssembly ? &logger : nullptr, options.includeAssembly, X64::ABIX64::SystemV, /* features= */ 0
        );

        return getAssemblyFromIrImpl(logger, build, ir, options, stats);
    }

    default:
        CODEGEN_ASSERT(!"Unknown target");
        return std::string();
    }
}

} // namespace CodeGen
} // namespace Luau
