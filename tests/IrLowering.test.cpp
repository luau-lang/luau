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
LUAU_FASTFLAG(LuauCodegenVector)
LUAU_FASTFLAG(LuauCodegenMathMemArgs)

static std::string getCodegenAssembly(const char* source)
{
    Luau::CodeGen::AssemblyOptions options;

    // For IR, we don't care about assembly, but we want a stable target
    options.target = Luau::CodeGen::AssemblyOptions::Target::X64_SystemV;

    options.outputBinary = false;
    options.includeAssembly = false;
    options.includeIr = true;
    options.includeOutlinedCode = false;

    options.includeIrPrefix = Luau::CodeGen::IncludeIrPrefix::No;
    options.includeUseInfo = Luau::CodeGen::IncludeUseInfo::No;
    options.includeCfgInfo = Luau::CodeGen::IncludeCfgInfo::No;
    options.includeRegFlowInfo = Luau::CodeGen::IncludeRegFlowInfo::No;

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
    ScopedFastFlag luauCodegenVector{FFlag::LuauCodegenVector, true};

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
  %6 = NUM_TO_VECTOR 1
  %7 = LOAD_TVALUE R0
  %8 = DIV_VEC %6, %7
  STORE_TVALUE R1, %8
  INTERRUPT 1u
  RETURN R1, 1i
)");
}

TEST_CASE("VectorComponentRead")
{
    ScopedFastFlag luauCodegenVector{FFlag::LuauCodegenVector, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function compsum(a: vector)
    return a.X + a.Y + a.Z
end
)"),
        R"(
; function compsum($arg0) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %6 = LOAD_FLOAT R0, 0i
  STORE_DOUBLE R3, %6
  STORE_TAG R3, tnumber
  %11 = LOAD_FLOAT R0, 4i
  STORE_DOUBLE R4, %11
  STORE_TAG R4, tnumber
  %20 = ADD_NUM %6, %11
  STORE_DOUBLE R2, %20
  STORE_TAG R2, tnumber
  %25 = LOAD_FLOAT R0, 8i
  STORE_DOUBLE R3, %25
  %34 = ADD_NUM %20, %25
  STORE_DOUBLE R1, %34
  STORE_TAG R1, tnumber
  INTERRUPT 8u
  RETURN R1, 1i
)");
}

TEST_CASE("VectorAdd")
{
    ScopedFastFlag luauCodegenVector{FFlag::LuauCodegenVector, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function vec3add(a: vector, b: vector)
    return a + b
end
)"),
        R"(
; function vec3add($arg0, $arg1) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  CHECK_TAG R1, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %10 = LOAD_TVALUE R0
  %11 = LOAD_TVALUE R1
  %12 = ADD_VEC %10, %11
  STORE_TVALUE R2, %12
  INTERRUPT 1u
  RETURN R2, 1i
)");
}

TEST_CASE("VectorMinus")
{
    ScopedFastFlag luauCodegenVector{FFlag::LuauCodegenVector, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function vec3minus(a: vector)
    return -a
end
)"),
        R"(
; function vec3minus($arg0) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %6 = LOAD_TVALUE R0
  %7 = UNM_VEC %6
  STORE_TVALUE R1, %7
  INTERRUPT 1u
  RETURN R1, 1i
)");
}

TEST_CASE("VectorSubMulDiv")
{
    ScopedFastFlag luauCodegenVector{FFlag::LuauCodegenVector, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function vec3combo(a: vector, b: vector, c: vector, d: vector)
    return a * b - c / d
end
)"),
        R"(
; function vec3combo($arg0, $arg1, $arg2, $arg3) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  CHECK_TAG R1, tvector, exit(entry)
  CHECK_TAG R2, tvector, exit(entry)
  CHECK_TAG R3, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %14 = LOAD_TVALUE R0
  %15 = LOAD_TVALUE R1
  %16 = MUL_VEC %14, %15
  STORE_TVALUE R5, %16
  %22 = LOAD_TVALUE R2
  %23 = LOAD_TVALUE R3
  %24 = DIV_VEC %22, %23
  STORE_TVALUE R6, %24
  %32 = SUB_VEC %16, %24
  STORE_TVALUE R4, %32
  INTERRUPT 3u
  RETURN R4, 1i
)");
}

TEST_CASE("VectorMulDivMixed")
{
    ScopedFastFlag luauCodegenVector{FFlag::LuauCodegenVector, true};
    ScopedFastFlag luauFixDivrkInference{FFlag::LuauFixDivrkInference, true};
    ScopedFastFlag luauCompileRevK{FFlag::LuauCompileRevK, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function vec3combo(a: vector, b: vector, c: vector, d: vector)
    return a * 2 + b / 4 + 0.5 * c + 40 / d
end
)"),
        R"(
; function vec3combo($arg0, $arg1, $arg2, $arg3) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  CHECK_TAG R1, tvector, exit(entry)
  CHECK_TAG R2, tvector, exit(entry)
  CHECK_TAG R3, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %12 = LOAD_TVALUE R0
  %13 = NUM_TO_VECTOR 2
  %14 = MUL_VEC %12, %13
  STORE_TVALUE R7, %14
  %18 = LOAD_TVALUE R1
  %19 = NUM_TO_VECTOR 4
  %20 = DIV_VEC %18, %19
  STORE_TVALUE R8, %20
  %28 = ADD_VEC %14, %20
  STORE_TVALUE R6, %28
  STORE_DOUBLE R8, 0.5
  STORE_TAG R8, tnumber
  %37 = NUM_TO_VECTOR 0.5
  %38 = LOAD_TVALUE R2
  %39 = MUL_VEC %37, %38
  STORE_TVALUE R7, %39
  %47 = ADD_VEC %28, %39
  STORE_TVALUE R5, %47
  %51 = NUM_TO_VECTOR 40
  %52 = LOAD_TVALUE R3
  %53 = DIV_VEC %51, %52
  STORE_TVALUE R6, %53
  %61 = ADD_VEC %47, %53
  STORE_TVALUE R4, %61
  INTERRUPT 8u
  RETURN R4, 1i
)");
}

TEST_CASE("ExtraMathMemoryOperands")
{
    ScopedFastFlag luauCodegenMathMemArgs{FFlag::LuauCodegenMathMemArgs, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function foo(a: number, b: number, c: number, d: number, e: number)
    return math.floor(a) + math.ceil(b) + math.round(c) + math.sqrt(d) + math.abs(e)
end
)"),
        R"(
; function foo($arg0, $arg1, $arg2, $arg3, $arg4) line 2
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  CHECK_TAG R2, tnumber, exit(entry)
  CHECK_TAG R3, tnumber, exit(entry)
  CHECK_TAG R4, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  CHECK_SAFE_ENV exit(1)
  %16 = FLOOR_NUM R0
  STORE_DOUBLE R9, %16
  STORE_TAG R9, tnumber
  %23 = CEIL_NUM R1
  STORE_DOUBLE R10, %23
  STORE_TAG R10, tnumber
  %32 = ADD_NUM %16, %23
  STORE_DOUBLE R8, %32
  STORE_TAG R8, tnumber
  %39 = ROUND_NUM R2
  STORE_DOUBLE R9, %39
  %48 = ADD_NUM %32, %39
  STORE_DOUBLE R7, %48
  STORE_TAG R7, tnumber
  %55 = SQRT_NUM R3
  STORE_DOUBLE R8, %55
  %64 = ADD_NUM %48, %55
  STORE_DOUBLE R6, %64
  STORE_TAG R6, tnumber
  %71 = ABS_NUM R4
  STORE_DOUBLE R7, %71
  %80 = ADD_NUM %64, %71
  STORE_DOUBLE R5, %80
  STORE_TAG R5, tnumber
  INTERRUPT 29u
  RETURN R5, 1i
)");
}

TEST_SUITE_END();
