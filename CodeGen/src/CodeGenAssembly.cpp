// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/CodeGen.h"
#include "Luau/BytecodeUtils.h"
#include "Luau/BytecodeSummary.h"

#include "CodeGenLower.h"

#include "CodeGenA64.h"
#include "CodeGenX64.h"

#include "lapi.h"

namespace Luau
{
namespace CodeGen
{

template<typename AssemblyBuilder>
static void logFunctionHeader(AssemblyBuilder& build, Proto* proto)
{
    if (proto->debugname)
        build.logAppend("; function %s(", getstr(proto->debugname));
    else
        build.logAppend("; function(");

    for (int i = 0; i < proto->numparams; i++)
    {
        LocVar* var = proto->locvars ? &proto->locvars[proto->sizelocvars - proto->numparams + i] : nullptr;

        if (var && var->varname)
            build.logAppend("%s%s", i == 0 ? "" : ", ", getstr(var->varname));
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

unsigned getInstructionCount(const Instruction* insns, const unsigned size)
{
    unsigned count = 0;
    for (unsigned i = 0; i < size;)
    {
        ++count;
        i += Luau::getOpLength(LuauOpcode(LUAU_INSN_OP(insns[i])));
    }
    return count;
}

template<typename AssemblyBuilder>
static std::string getAssemblyImpl(AssemblyBuilder& build, const TValue* func, AssemblyOptions options, LoweringStats* stats)
{
    Proto* root = clvalue(func)->l.p;

    if ((options.flags & CodeGen_OnlyNativeModules) != 0 && (root->flags & LPF_NATIVE_MODULE) == 0)
        return std::string();

    std::vector<Proto*> protos;
    gatherFunctions(protos, root, options.flags);

    protos.erase(std::remove_if(protos.begin(), protos.end(),
                     [](Proto* p) {
                         return p == nullptr;
                     }),
        protos.end());

    if (stats)
        stats->totalFunctions += unsigned(protos.size());

    if (protos.empty())
    {
        build.finalize(); // to avoid assertion in AssemblyBuilder dtor
        return std::string();
    }

    ModuleHelpers helpers;
    assembleHelpers(build, helpers);

    if (!options.includeOutlinedCode && options.includeAssembly)
    {
        build.text.clear();
        build.logAppend("; skipping %u bytes of outlined helpers\n", unsigned(build.getCodeSize() * sizeof(build.code[0])));
    }

    for (Proto* p : protos)
    {
        IrBuilder ir;
        ir.buildFunctionIr(p);
        unsigned asmSize = build.getCodeSize();
        unsigned asmCount = build.getInstructionCount();

        if (options.includeAssembly || options.includeIr)
            logFunctionHeader(build, p);

        if (!lowerFunction(ir, build, helpers, p, options, stats))
        {
            if (build.logText)
                build.logAppend("; skipping (can't lower)\n");

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
            functionStat.name = p->debugname ? getstr(p->debugname) : "";
            functionStat.line = p->linedefined;
            functionStat.bcodeCount = getInstructionCount(p->code, p->sizecode);
            functionStat.irCount = unsigned(ir.function.instructions.size());
            functionStat.asmSize = asmSize;
            functionStat.asmCount = asmCount;
            if (stats->functionStatsFlags & FunctionStats_BytecodeSummary)
            {
                FunctionBytecodeSummary summary(FunctionBytecodeSummary::fromProto(p, 0));
                functionStat.bytecodeSummary.push_back(summary.getCounts(0));
            }
            stats->functions.push_back(std::move(functionStat));
        }

        if (build.logText)
            build.logAppend("\n");
    }

    if (!build.finalize())
        return std::string();

    if (options.outputBinary)
        return std::string(reinterpret_cast<const char*>(build.code.data()), reinterpret_cast<const char*>(build.code.data() + build.code.size())) +
               std::string(build.data.begin(), build.data.end());
    else
        return build.text;
}

#if defined(__aarch64__)
unsigned int getCpuFeaturesA64();
#endif

std::string getAssembly(lua_State* L, int idx, AssemblyOptions options, LoweringStats* stats)
{
    LUAU_ASSERT(lua_isLfunction(L, idx));
    const TValue* func = luaA_toobject(L, idx);

    switch (options.target)
    {
    case AssemblyOptions::Host:
    {
#if defined(__aarch64__)
        static unsigned int cpuFeatures = getCpuFeaturesA64();
        A64::AssemblyBuilderA64 build(/* logText= */ options.includeAssembly, cpuFeatures);
#else
        X64::AssemblyBuilderX64 build(/* logText= */ options.includeAssembly);
#endif

        return getAssemblyImpl(build, func, options, stats);
    }

    case AssemblyOptions::A64:
    {
        A64::AssemblyBuilderA64 build(/* logText= */ options.includeAssembly, /* features= */ A64::Feature_JSCVT);

        return getAssemblyImpl(build, func, options, stats);
    }

    case AssemblyOptions::A64_NoFeatures:
    {
        A64::AssemblyBuilderA64 build(/* logText= */ options.includeAssembly, /* features= */ 0);

        return getAssemblyImpl(build, func, options, stats);
    }

    case AssemblyOptions::X64_Windows:
    {
        X64::AssemblyBuilderX64 build(/* logText= */ options.includeAssembly, X64::ABIX64::Windows);

        return getAssemblyImpl(build, func, options, stats);
    }

    case AssemblyOptions::X64_SystemV:
    {
        X64::AssemblyBuilderX64 build(/* logText= */ options.includeAssembly, X64::ABIX64::SystemV);

        return getAssemblyImpl(build, func, options, stats);
    }

    default:
        LUAU_ASSERT(!"Unknown target");
        return std::string();
    }
}

} // namespace CodeGen
} // namespace Luau
