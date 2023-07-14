// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/CodeGen.h"

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

template<typename AssemblyBuilder>
static std::string getAssemblyImpl(AssemblyBuilder& build, const TValue* func, AssemblyOptions options)
{
    std::vector<Proto*> protos;
    gatherFunctions(protos, clvalue(func)->l.p);

    ModuleHelpers helpers;
    assembleHelpers(build, helpers);

    if (!options.includeOutlinedCode && options.includeAssembly)
    {
        build.text.clear();
        build.logAppend("; skipping %u bytes of outlined helpers\n", unsigned(build.getCodeSize() * sizeof(build.code[0])));
    }

    for (Proto* p : protos)
        if (p)
        {
            IrBuilder ir;
            ir.buildFunctionIr(p);

            if (options.includeAssembly || options.includeIr)
                logFunctionHeader(build, p);

            if (!lowerFunction(ir, build, helpers, p, options))
            {
                if (build.logText)
                    build.logAppend("; skipping (can't lower)\n");
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

std::string getAssembly(lua_State* L, int idx, AssemblyOptions options)
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

        return getAssemblyImpl(build, func, options);
    }

    case AssemblyOptions::A64:
    {
        A64::AssemblyBuilderA64 build(/* logText= */ options.includeAssembly, /* features= */ A64::Feature_JSCVT);

        return getAssemblyImpl(build, func, options);
    }

    case AssemblyOptions::A64_NoFeatures:
    {
        A64::AssemblyBuilderA64 build(/* logText= */ options.includeAssembly, /* features= */ 0);

        return getAssemblyImpl(build, func, options);
    }

    case AssemblyOptions::X64_Windows:
    {
        X64::AssemblyBuilderX64 build(/* logText= */ options.includeAssembly, X64::ABIX64::Windows);

        return getAssemblyImpl(build, func, options);
    }

    case AssemblyOptions::X64_SystemV:
    {
        X64::AssemblyBuilderX64 build(/* logText= */ options.includeAssembly, X64::ABIX64::SystemV);

        return getAssemblyImpl(build, func, options);
    }

    default:
        LUAU_ASSERT(!"Unknown target");
        return std::string();
    }
}

} // namespace CodeGen
} // namespace Luau
