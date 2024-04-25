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

LUAU_FASTFLAG(LuauCodegenRemoveDeadStores5)
LUAU_FASTFLAG(LuauCodegenDirectUserdataFlow)
LUAU_FASTFLAG(LuauCompileTypeInfo)
LUAU_FASTFLAG(LuauLoadTypeInfo)
LUAU_FASTFLAG(LuauCodegenTypeInfo)
LUAU_FASTFLAG(LuauTypeInfoLookupImprovement)

static std::string getCodegenAssembly(const char* source, bool includeIrTypes = false)
{
    Luau::CodeGen::AssemblyOptions options;

    // For IR, we don't care about assembly, but we want a stable target
    options.target = Luau::CodeGen::AssemblyOptions::Target::X64_SystemV;

    options.outputBinary = false;
    options.includeAssembly = false;
    options.includeIr = true;
    options.includeOutlinedCode = false;
    options.includeIrTypes = includeIrTypes;

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
    copts.typeInfoLevel = 1;
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
  %6 = NUM_TO_VEC 1
  %7 = LOAD_TVALUE R0
  %8 = DIV_VEC %6, %7
  %9 = TAG_VECTOR %8
  STORE_TVALUE R1, %9
  INTERRUPT 1u
  RETURN R1, 1i
)");
}

TEST_CASE("VectorComponentRead")
{
    ScopedFastFlag luauCodegenRemoveDeadStores{FFlag::LuauCodegenRemoveDeadStores5, true};

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
  %11 = LOAD_FLOAT R0, 4i
  %20 = ADD_NUM %6, %11
  %25 = LOAD_FLOAT R0, 8i
  %34 = ADD_NUM %20, %25
  STORE_DOUBLE R1, %34
  STORE_TAG R1, tnumber
  INTERRUPT 8u
  RETURN R1, 1i
)");
}

TEST_CASE("VectorAdd")
{
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
  %13 = TAG_VECTOR %12
  STORE_TVALUE R2, %13
  INTERRUPT 1u
  RETURN R2, 1i
)");
}

TEST_CASE("VectorMinus")
{
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
  %8 = TAG_VECTOR %7
  STORE_TVALUE R1, %8
  INTERRUPT 1u
  RETURN R1, 1i
)");
}

TEST_CASE("VectorSubMulDiv")
{
    ScopedFastFlag luauCodegenRemoveDeadStores{FFlag::LuauCodegenRemoveDeadStores5, true};

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
  %23 = LOAD_TVALUE R2
  %24 = LOAD_TVALUE R3
  %25 = DIV_VEC %23, %24
  %34 = SUB_VEC %16, %25
  %35 = TAG_VECTOR %34
  STORE_TVALUE R4, %35
  INTERRUPT 3u
  RETURN R4, 1i
)");
}

TEST_CASE("VectorSubMulDiv2")
{
    ScopedFastFlag luauCodegenRemoveDeadStores{FFlag::LuauCodegenRemoveDeadStores5, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function vec3combo(a: vector)
    local tmp = a * a
    return (tmp - tmp) / (tmp + tmp)
end
)"),
        R"(
; function vec3combo($arg0) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %8 = LOAD_TVALUE R0
  %10 = MUL_VEC %8, %8
  %19 = SUB_VEC %10, %10
  %28 = ADD_VEC %10, %10
  %37 = DIV_VEC %19, %28
  %38 = TAG_VECTOR %37
  STORE_TVALUE R2, %38
  INTERRUPT 4u
  RETURN R2, 1i
)");
}

TEST_CASE("VectorMulDivMixed")
{
    ScopedFastFlag luauCodegenRemoveDeadStores{FFlag::LuauCodegenRemoveDeadStores5, true};

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
  %13 = NUM_TO_VEC 2
  %14 = MUL_VEC %12, %13
  %19 = LOAD_TVALUE R1
  %20 = NUM_TO_VEC 4
  %21 = DIV_VEC %19, %20
  %30 = ADD_VEC %14, %21
  %40 = NUM_TO_VEC 0.5
  %41 = LOAD_TVALUE R2
  %42 = MUL_VEC %40, %41
  %51 = ADD_VEC %30, %42
  %56 = NUM_TO_VEC 40
  %57 = LOAD_TVALUE R3
  %58 = DIV_VEC %56, %57
  %67 = ADD_VEC %51, %58
  %68 = TAG_VECTOR %67
  STORE_TVALUE R4, %68
  INTERRUPT 8u
  RETURN R4, 1i
)");
}

TEST_CASE("ExtraMathMemoryOperands")
{
    ScopedFastFlag luauCodegenRemoveDeadStores{FFlag::LuauCodegenRemoveDeadStores5, true};

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
  %23 = CEIL_NUM R1
  %32 = ADD_NUM %16, %23
  %39 = ROUND_NUM R2
  %48 = ADD_NUM %32, %39
  %55 = SQRT_NUM R3
  %64 = ADD_NUM %48, %55
  %71 = ABS_NUM R4
  %80 = ADD_NUM %64, %71
  STORE_DOUBLE R5, %80
  STORE_TAG R5, tnumber
  INTERRUPT 29u
  RETURN R5, 1i
)");
}

TEST_CASE("DseInitialStackState")
{
    ScopedFastFlag luauCodegenRemoveDeadStores{FFlag::LuauCodegenRemoveDeadStores5, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function foo()
    while {} do
        local _ = not _,{}
        _ = nil
    end
end
)"),
        R"(
; function foo() line 2
bb_bytecode_0:
  SET_SAVEDPC 1u
  %1 = NEW_TABLE 0u, 0u
  STORE_POINTER R0, %1
  STORE_TAG R0, ttable
  CHECK_GC
  JUMP bb_2
bb_2:
  CHECK_SAFE_ENV exit(3)
  JUMP_EQ_TAG K1, tnil, bb_fallback_4, bb_3
bb_3:
  %9 = LOAD_TVALUE K1
  STORE_TVALUE R1, %9
  JUMP bb_5
bb_5:
  SET_SAVEDPC 7u
  %21 = NEW_TABLE 0u, 0u
  STORE_POINTER R1, %21
  STORE_TAG R1, ttable
  CHECK_GC
  STORE_TAG R0, tnil
  INTERRUPT 9u
  JUMP bb_bytecode_0
)");
}

TEST_CASE("DseInitialStackState2")
{
    ScopedFastFlag luauCodegenRemoveDeadStores{FFlag::LuauCodegenRemoveDeadStores5, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function foo(a)
    math.frexp(a)
    return a
end
)"),
        R"(
; function foo($arg0) line 2
bb_bytecode_0:
  CHECK_SAFE_ENV exit(1)
  CHECK_TAG R0, tnumber, exit(1)
  FASTCALL 14u, R1, R0, undef, 1i, 2i
  INTERRUPT 5u
  RETURN R0, 1i
)");
}

TEST_CASE("DseInitialStackState3")
{
    ScopedFastFlag luauCodegenRemoveDeadStores{FFlag::LuauCodegenRemoveDeadStores5, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function foo(a)
    math.sign(a)
    return a
end
)"),
        R"(
; function foo($arg0) line 2
bb_bytecode_0:
  CHECK_SAFE_ENV exit(1)
  CHECK_TAG R0, tnumber, exit(1)
  FASTCALL 47u, R1, R0, undef, 1i, 1i
  INTERRUPT 5u
  RETURN R0, 1i
)");
}

TEST_CASE("VectorConstantTag")
{
    ScopedFastFlag luauCodegenRemoveDeadStores{FFlag::LuauCodegenRemoveDeadStores5, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function vecrcp(a: vector)
    return vector(1, 2, 3) + a
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
  %4 = LOAD_TVALUE K0, 0i, tvector
  %11 = LOAD_TVALUE R0
  %12 = ADD_VEC %4, %11
  %13 = TAG_VECTOR %12
  STORE_TVALUE R1, %13
  INTERRUPT 2u
  RETURN R1, 1i
)");
}

TEST_CASE("VectorNamecall")
{
    ScopedFastFlag luauCodegenDirectUserdataFlow{FFlag::LuauCodegenDirectUserdataFlow, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function abs(a: vector)
    return a:Abs()
end
)"),
        R"(
; function abs($arg0) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  FALLBACK_NAMECALL 0u, R1, R0, K0
  INTERRUPT 2u
  SET_SAVEDPC 3u
  CALL R1, 1i, -1i
  INTERRUPT 3u
  RETURN R1, -1i
)");
}

TEST_CASE("UserDataGetIndex")
{
    ScopedFastFlag luauCodegenDirectUserdataFlow{FFlag::LuauCodegenDirectUserdataFlow, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function getxy(a: Point)
    return a.x + a.y
end
)"),
        R"(
; function getxy($arg0) line 2
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  FALLBACK_GETTABLEKS 0u, R2, R0, K0
  FALLBACK_GETTABLEKS 2u, R3, R0, K1
  CHECK_TAG R2, tnumber, bb_fallback_3
  CHECK_TAG R3, tnumber, bb_fallback_3
  %14 = LOAD_DOUBLE R2
  %16 = ADD_NUM %14, R3
  STORE_DOUBLE R1, %16
  STORE_TAG R1, tnumber
  JUMP bb_4
bb_4:
  INTERRUPT 5u
  RETURN R1, 1i
)");
}

TEST_CASE("UserDataSetIndex")
{
    ScopedFastFlag luauCodegenDirectUserdataFlow{FFlag::LuauCodegenDirectUserdataFlow, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function setxy(a: Point)
    a.x = 3
    a.y = 4
end
)"),
        R"(
; function setxy($arg0) line 2
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  STORE_DOUBLE R1, 3
  STORE_TAG R1, tnumber
  FALLBACK_SETTABLEKS 1u, R1, R0, K0
  STORE_DOUBLE R1, 4
  FALLBACK_SETTABLEKS 4u, R1, R0, K1
  INTERRUPT 6u
  RETURN R0, 0i
)");
}

TEST_CASE("UserDataNamecall")
{
    ScopedFastFlag luauCodegenDirectUserdataFlow{FFlag::LuauCodegenDirectUserdataFlow, true};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function getxy(a: Point)
    return a:GetX() + a:GetY()
end
)"),
        R"(
; function getxy($arg0) line 2
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  FALLBACK_NAMECALL 0u, R2, R0, K0
  INTERRUPT 2u
  SET_SAVEDPC 3u
  CALL R2, 1i, 1i
  FALLBACK_NAMECALL 3u, R3, R0, K1
  INTERRUPT 5u
  SET_SAVEDPC 6u
  CALL R3, 1i, 1i
  CHECK_TAG R2, tnumber, bb_fallback_3
  CHECK_TAG R3, tnumber, bb_fallback_3
  %20 = LOAD_DOUBLE R2
  %22 = ADD_NUM %20, R3
  STORE_DOUBLE R1, %22
  STORE_TAG R1, tnumber
  JUMP bb_4
bb_4:
  INTERRUPT 7u
  RETURN R1, 1i
)");
}

TEST_CASE("ExplicitUpvalueAndLocalTypes")
{
    ScopedFastFlag sffs[]{{FFlag::LuauLoadTypeInfo, true}, {FFlag::LuauCompileTypeInfo, true}, {FFlag::LuauCodegenTypeInfo, true},
        {FFlag::LuauCodegenRemoveDeadStores5, true}};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local y: vector = ...

local function getsum(t)
    local x: vector = t
    return x.X + x.Y + y.X + y.Y
end
)",
                        /* includeIrTypes */ true),
        R"(
; function getsum($arg0) line 4
; U0: vector
; R0: vector from 0 to 14
bb_bytecode_0:
  CHECK_TAG R0, tvector, exit(0)
  %2 = LOAD_FLOAT R0, 0i
  STORE_DOUBLE R4, %2
  STORE_TAG R4, tnumber
  %7 = LOAD_FLOAT R0, 4i
  %16 = ADD_NUM %2, %7
  STORE_DOUBLE R3, %16
  STORE_TAG R3, tnumber
  GET_UPVALUE R5, U0
  CHECK_TAG R5, tvector, exit(6)
  %22 = LOAD_FLOAT R5, 0i
  %31 = ADD_NUM %16, %22
  STORE_DOUBLE R2, %31
  STORE_TAG R2, tnumber
  GET_UPVALUE R4, U0
  CHECK_TAG R4, tvector, exit(10)
  %37 = LOAD_FLOAT R4, 4i
  %46 = ADD_NUM %31, %37
  STORE_DOUBLE R1, %46
  STORE_TAG R1, tnumber
  INTERRUPT 13u
  RETURN R1, 1i
)");
}

TEST_CASE("FastcallTypeInferThroughLocal")
{
    ScopedFastFlag sffs[]{{FFlag::LuauLoadTypeInfo, true}, {FFlag::LuauCompileTypeInfo, true}, {FFlag::LuauCodegenTypeInfo, true},
        {FFlag::LuauCodegenRemoveDeadStores5, true}};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function getsum(x, c)
    local v = vector(x, 2, 3)
    if c then
        return v.X + v.Y
    else
        return v.Z
    end
end
)",
                        /* includeIrTypes */ true),
        R"(
; function getsum($arg0, $arg1) line 2
; R2: vector from 0 to 17
bb_bytecode_0:
  %0 = LOAD_TVALUE R0
  STORE_TVALUE R3, %0
  STORE_DOUBLE R4, 2
  STORE_TAG R4, tnumber
  STORE_DOUBLE R5, 3
  STORE_TAG R5, tnumber
  CHECK_SAFE_ENV exit(4)
  CHECK_TAG R3, tnumber, exit(4)
  %13 = LOAD_DOUBLE R3
  STORE_VECTOR R2, %13, 2, 3
  STORE_TAG R2, tvector
  JUMP_IF_FALSY R1, bb_bytecode_1, bb_3
bb_3:
  CHECK_TAG R2, tvector, exit(8)
  %21 = LOAD_FLOAT R2, 0i
  %26 = LOAD_FLOAT R2, 4i
  %35 = ADD_NUM %21, %26
  STORE_DOUBLE R3, %35
  STORE_TAG R3, tnumber
  INTERRUPT 13u
  RETURN R3, 1i
bb_bytecode_1:
  CHECK_TAG R2, tvector, exit(14)
  %42 = LOAD_FLOAT R2, 8i
  STORE_DOUBLE R3, %42
  STORE_TAG R3, tnumber
  INTERRUPT 16u
  RETURN R3, 1i
)");
}

TEST_CASE("FastcallTypeInferThroughUpvalue")
{
    ScopedFastFlag sffs[]{{FFlag::LuauLoadTypeInfo, true}, {FFlag::LuauCompileTypeInfo, true}, {FFlag::LuauCodegenTypeInfo, true},
        {FFlag::LuauCodegenRemoveDeadStores5, true}};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local v = ...

local function getsum(x, c)
    v = vector(x, 2, 3)
    if c then
        return v.X + v.Y
    else
        return v.Z
    end
end
)",
                        /* includeIrTypes */ true),
        R"(
; function getsum($arg0, $arg1) line 4
; U0: vector
bb_bytecode_0:
  %0 = LOAD_TVALUE R0
  STORE_TVALUE R3, %0
  STORE_DOUBLE R4, 2
  STORE_TAG R4, tnumber
  STORE_DOUBLE R5, 3
  STORE_TAG R5, tnumber
  CHECK_SAFE_ENV exit(4)
  CHECK_TAG R3, tnumber, exit(4)
  %13 = LOAD_DOUBLE R3
  STORE_VECTOR R2, %13, 2, 3
  STORE_TAG R2, tvector
  SET_UPVALUE U0, R2, tvector
  JUMP_IF_FALSY R1, bb_bytecode_1, bb_3
bb_3:
  GET_UPVALUE R4, U0
  CHECK_TAG R4, tvector, exit(10)
  %23 = LOAD_FLOAT R4, 0i
  STORE_DOUBLE R3, %23
  STORE_TAG R3, tnumber
  GET_UPVALUE R5, U0
  CHECK_TAG R5, tvector, exit(13)
  %29 = LOAD_FLOAT R5, 4i
  %38 = ADD_NUM %23, %29
  STORE_DOUBLE R2, %38
  STORE_TAG R2, tnumber
  INTERRUPT 16u
  RETURN R2, 1i
bb_bytecode_1:
  GET_UPVALUE R3, U0
  CHECK_TAG R3, tvector, exit(18)
  %46 = LOAD_FLOAT R3, 8i
  STORE_DOUBLE R2, %46
  STORE_TAG R2, tnumber
  INTERRUPT 20u
  RETURN R2, 1i
)");
}

TEST_CASE("LoadAndMoveTypePropagation")
{
    ScopedFastFlag sffs[]{{FFlag::LuauLoadTypeInfo, true}, {FFlag::LuauCompileTypeInfo, true}, {FFlag::LuauCodegenTypeInfo, true},
        {FFlag::LuauCodegenRemoveDeadStores5, true}, {FFlag::LuauTypeInfoLookupImprovement, true}};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function getsum(n)
    local seqsum = 0
    for i = 1,n do
        if i < 10 then
            seqsum += i
        else
            seqsum *= i
        end
    end

    return seqsum
end
)",
                        /* includeIrTypes */ true),
        R"(
; function getsum($arg0) line 2
; R1: number from 0 to 13
; R4: number from 1 to 11
bb_bytecode_0:
  STORE_DOUBLE R1, 0
  STORE_TAG R1, tnumber
  STORE_DOUBLE R4, 1
  STORE_TAG R4, tnumber
  %4 = LOAD_TVALUE R0
  STORE_TVALUE R2, %4
  STORE_DOUBLE R3, 1
  STORE_TAG R3, tnumber
  CHECK_TAG R2, tnumber, exit(4)
  %12 = LOAD_DOUBLE R2
  JUMP_CMP_NUM 1, %12, not_le, bb_bytecode_4, bb_bytecode_1
bb_bytecode_1:
  INTERRUPT 5u
  STORE_DOUBLE R5, 10
  STORE_TAG R5, tnumber
  CHECK_TAG R4, tnumber, bb_fallback_6
  JUMP_CMP_NUM R4, 10, not_lt, bb_bytecode_2, bb_5
bb_5:
  CHECK_TAG R1, tnumber, exit(8)
  CHECK_TAG R4, tnumber, exit(8)
  %32 = LOAD_DOUBLE R1
  %34 = ADD_NUM %32, R4
  STORE_DOUBLE R1, %34
  JUMP bb_bytecode_3
bb_bytecode_2:
  CHECK_TAG R1, tnumber, exit(10)
  CHECK_TAG R4, tnumber, exit(10)
  %41 = LOAD_DOUBLE R1
  %43 = MUL_NUM %41, R4
  STORE_DOUBLE R1, %43
  JUMP bb_bytecode_3
bb_bytecode_3:
  %46 = LOAD_DOUBLE R2
  %47 = LOAD_DOUBLE R4
  %48 = ADD_NUM %47, 1
  STORE_DOUBLE R4, %48
  JUMP_CMP_NUM %48, %46, le, bb_bytecode_1, bb_bytecode_4
bb_bytecode_4:
  INTERRUPT 12u
  RETURN R1, 1i
)");
}

TEST_CASE("ArgumentTypeRefinement")
{
    ScopedFastFlag sffs[]{{FFlag::LuauLoadTypeInfo, true}, {FFlag::LuauCompileTypeInfo, true}, {FFlag::LuauCodegenTypeInfo, true},
        {FFlag::LuauCodegenRemoveDeadStores5, true}, {FFlag::LuauTypeInfoLookupImprovement, true}};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function getsum(x, y)
    x = vector(1, y, 3)
    return x.Y + x.Z
end
)",
                        /* includeIrTypes */ true),
        R"(
; function getsum($arg0, $arg1) line 2
; R0: vector [argument]
bb_bytecode_0:
  STORE_DOUBLE R3, 1
  STORE_TAG R3, tnumber
  %2 = LOAD_TVALUE R1
  STORE_TVALUE R4, %2
  STORE_DOUBLE R5, 3
  STORE_TAG R5, tnumber
  CHECK_SAFE_ENV exit(4)
  CHECK_TAG R4, tnumber, exit(4)
  %14 = LOAD_DOUBLE R4
  STORE_VECTOR R2, 1, %14, 3
  STORE_TAG R2, tvector
  %18 = LOAD_TVALUE R2
  STORE_TVALUE R0, %18
  %22 = LOAD_FLOAT R0, 4i
  %27 = LOAD_FLOAT R0, 8i
  %36 = ADD_NUM %22, %27
  STORE_DOUBLE R2, %36
  STORE_TAG R2, tnumber
  INTERRUPT 13u
  RETURN R2, 1i
)");
}

TEST_CASE("InlineFunctionType")
{
    ScopedFastFlag sffs[]{{FFlag::LuauLoadTypeInfo, true}, {FFlag::LuauCompileTypeInfo, true}, {FFlag::LuauCodegenTypeInfo, true},
        {FFlag::LuauCodegenRemoveDeadStores5, true}, {FFlag::LuauTypeInfoLookupImprovement, true}};

    CHECK_EQ("\n" + getCodegenAssembly(R"(
local function inl(v: vector, s: number)
    return v.Y * s
end

local function getsum(x)
    return inl(x, 2) + inl(x, 5)
end
)",
                        /* includeIrTypes */ true),
        R"(
; function inl($arg0, $arg1) line 2
; R0: vector [argument]
; R1: number [argument]
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %8 = LOAD_FLOAT R0, 4i
  %17 = MUL_NUM %8, R1
  STORE_DOUBLE R2, %17
  STORE_TAG R2, tnumber
  INTERRUPT 3u
  RETURN R2, 1i
; function getsum($arg0) line 6
; R0: vector from 0 to 3
; R0: vector from 3 to 6
bb_bytecode_0:
  CHECK_TAG R0, tvector, exit(0)
  %2 = LOAD_FLOAT R0, 4i
  %8 = MUL_NUM %2, 2
  %13 = LOAD_FLOAT R0, 4i
  %19 = MUL_NUM %13, 5
  %28 = ADD_NUM %8, %19
  STORE_DOUBLE R1, %28
  STORE_TAG R1, tnumber
  INTERRUPT 7u
  RETURN R1, 1i
)");
}

TEST_SUITE_END();
