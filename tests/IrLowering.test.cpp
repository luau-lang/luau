// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"
#include "lualib.h"

#include "Luau/BytecodeBuilder.h"
#include "Luau/CodeGen.h"
#include "Luau/Compiler.h"
#include "Luau/Parser.h"

#include "doctest.h"
#include "ScopedFlags.h"

#include <memory>

LUAU_FASTFLAG(LuauFixDivrkInference)
LUAU_FASTFLAG(LuauCompileRevK)

static std::string getCodegenAssembly(const char* source)
{
    Luau::CodeGen::AssemblyOptions options;

    // For IR, we don't care about assembly, but we want a stable target
    options.target = Luau::CodeGen::AssemblyOptions::Target::X64_SystemV;

    options.outputBinary = false;
    options.includeAssembly = false;
    options.includeIr = true;
    options.includeOutlinedCode = false;

    options.includeIrPrefix = false;
    options.includeUseInfo = false;
    options.includeCfgInfo = false;
    options.includeRegFlowInfo = false;

    Luau::Allocator allocator;
    Luau::AstNameTable names(allocator);
    Luau::ParseResult result = Luau::Parser::parse(source, strlen(source), names, allocator);

    if (!result.errors.empty())
        throw Luau::ParseErrors(result.errors);

    Luau::CompileOptions copts = {};

    copts.optimizationLevel = 2;
    copts.debugLevel = 1;
    copts.vectorCtor = "vector";
    copts.vectorType = "vector";

    Luau::BytecodeBuilder bcb;
    Luau::compileOrThrow(bcb, result, names, copts);

    std::string bytecode = bcb.getBytecode();
    std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    if (luau_load(L, "name", bytecode.data(), bytecode.size(), 0) == 0)
        return Luau::CodeGen::getAssembly(L, -1, options, nullptr);

    FAIL("Failed to load bytecode");
    return "";
}

TEST_SUITE_BEGIN("IrLowering");

TEST_CASE("VectorReciprocal")
{
    ScopedFastFlag luauFixDivrkInference{FFlag::LuauFixDivrkInference, true};
    ScopedFastFlag luauCompileRevK{FFlag::LuauCompileRevK, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function vecrcp(a: vector)
    return 1 / a
end
)"),
        R"(
; function vecrcp($arg0) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  JUMP bb_fallback_3
bb_4:
  INTERRUPT 1u
  RETURN R1, 1i
)");
}

TEST_SUITE_END();
