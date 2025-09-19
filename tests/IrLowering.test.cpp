// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"
#include "lualib.h"
#include "luacode.h"

#include "Luau/BytecodeBuilder.h"
#include "Luau/CodeGen.h"
#include "Luau/Compiler.h"
#include "Luau/Parser.h"
#include "Luau/IrBuilder.h"

#include "doctest.h"
#include "ScopedFlags.h"
#include "ConformanceIrHooks.h"

#include <memory>
#include <string_view>

LUAU_FASTFLAG(LuauCodeGenDirectBtest)
LUAU_FASTFLAG(LuauVectorLerp)
LUAU_FASTFLAG(LuauCompileVectorLerp)
LUAU_FASTFLAG(LuauTypeCheckerVectorLerp)
LUAU_FASTFLAG(LuauCodeGenVectorLerp)
LUAU_FASTFLAG(LuauCodeGenFMA)
LUAU_FASTFLAG(LuauCodegenDirectCompare)

static void luauLibraryConstantLookup(const char* library, const char* member, Luau::CompileConstant* constant)
{
    // While 'vector' library constants are a Luau built-in, their constant value depends on the embedder LUA_VECTOR_SIZE value
    if (strcmp(library, "vector") == 0)
    {
        if (strcmp(member, "zero") == 0)
            return Luau::setCompileConstantVector(constant, 0.0f, 0.0f, 0.0f, 0.0f);

        if (strcmp(member, "one") == 0)
            return Luau::setCompileConstantVector(constant, 1.0f, 1.0f, 1.0f, 0.0f);
    }

    if (strcmp(library, "Vector3") == 0)
    {
        if (strcmp(member, "xAxis") == 0)
            return Luau::setCompileConstantVector(constant, 1.0f, 0.0f, 0.0f, 0.0f);

        if (strcmp(member, "yAxis") == 0)
            return Luau::setCompileConstantVector(constant, 0.0f, 1.0f, 0.0f, 0.0f);
    }
}

static void luauLibraryConstantLookupC(const char* library, const char* member, lua_CompileConstant* constant)
{
    if (strcmp(library, "test") == 0)
    {
        if (strcmp(member, "some_nil") == 0)
            return luau_set_compile_constant_nil(constant);

        if (strcmp(member, "some_boolean") == 0)
            return luau_set_compile_constant_boolean(constant, 1);

        if (strcmp(member, "some_number") == 0)
            return luau_set_compile_constant_number(constant, 4.75);

        if (strcmp(member, "some_vector") == 0)
            return luau_set_compile_constant_vector(constant, 1.0f, 2.0f, 4.0f, 8.0f);

        if (strcmp(member, "some_string") == 0)
            return luau_set_compile_constant_string(constant, "test", 4);
    }
}

static int luauLibraryTypeLookup(const char* library, const char* member)
{
    if (strcmp(library, "Vector3") == 0)
    {
        if (strcmp(member, "xAxis") == 0)
            return LuauBytecodeType::LBC_TYPE_VECTOR;

        if (strcmp(member, "yAxis") == 0)
            return LuauBytecodeType::LBC_TYPE_VECTOR;
    }

    return LuauBytecodeType::LBC_TYPE_ANY;
}

static void setupAssemblyOptions(Luau::CodeGen::AssemblyOptions& options, bool includeIrTypes)
{
    options.compilationOptions.hooks.vectorAccessBytecodeType = vectorAccessBytecodeType;
    options.compilationOptions.hooks.vectorNamecallBytecodeType = vectorNamecallBytecodeType;
    options.compilationOptions.hooks.vectorAccess = vectorAccess;
    options.compilationOptions.hooks.vectorNamecall = vectorNamecall;

    options.compilationOptions.hooks.userdataAccessBytecodeType = userdataAccessBytecodeType;
    options.compilationOptions.hooks.userdataMetamethodBytecodeType = userdataMetamethodBytecodeType;
    options.compilationOptions.hooks.userdataNamecallBytecodeType = userdataNamecallBytecodeType;
    options.compilationOptions.hooks.userdataAccess = userdataAccess;
    options.compilationOptions.hooks.userdataMetamethod = userdataMetamethod;
    options.compilationOptions.hooks.userdataNamecall = userdataNamecall;

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
}

static void initializeCodegen(lua_State* L)
{
    if (Luau::CodeGen::isSupported())
    {
        // Type remapper requires the codegen runtime
        Luau::CodeGen::create(L);

        Luau::CodeGen::setUserdataRemapper(
            L,
            kUserdataRunTypes,
            [](void* context, const char* str, size_t len) -> uint8_t
            {
                const char** types = (const char**)context;

                uint8_t index = 0;

                std::string_view sv{str, len};

                for (; *types; ++types)
                {
                    if (sv == *types)
                        return index;

                    index++;
                }

                return 0xff;
            }
        );
    }
}

static std::string getCodegenAssembly(const char* source, bool includeIrTypes = false, int debugLevel = 1)
{
    Luau::Allocator allocator;
    Luau::AstNameTable names(allocator);
    Luau::ParseResult result = Luau::Parser::parse(source, strlen(source), names, allocator);

    if (!result.errors.empty())
        throw Luau::ParseErrors(result.errors);

    Luau::CompileOptions copts = {};

    copts.optimizationLevel = 2;
    copts.debugLevel = debugLevel;
    copts.typeInfoLevel = 1;
    copts.vectorCtor = "vector";
    copts.vectorType = "vector";

    static const char* kUserdataCompileTypes[] = {"vec2", "color", "mat3", nullptr};
    copts.userdataTypes = kUserdataCompileTypes;

    static const char* kLibrariesWithConstants[] = {"vector", "Vector3", nullptr};
    copts.librariesWithKnownMembers = kLibrariesWithConstants;

    copts.libraryMemberTypeCb = luauLibraryTypeLookup;
    copts.libraryMemberConstantCb = luauLibraryConstantLookup;

    Luau::BytecodeBuilder bcb;
    Luau::compileOrThrow(bcb, result, names, copts);

    std::string bytecode = bcb.getBytecode();
    std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    initializeCodegen(L);

    if (luau_load(L, "name", bytecode.data(), bytecode.size(), 0) == 0)
    {
        Luau::CodeGen::AssemblyOptions options;
        setupAssemblyOptions(options, includeIrTypes);

        // Runtime mapping is specifically created to NOT match the compilation mapping
        options.compilationOptions.userdataTypes = kUserdataRunTypes;

        return Luau::CodeGen::getAssembly(L, -1, options, nullptr);
    }

    FAIL("Failed to load bytecode");
    return "";
}

static std::string getCodegenAssemblyUsingCApi(const char* source, bool includeIrTypes = false, int debugLevel = 1)
{
    lua_CompileOptions copts = {};

    copts.optimizationLevel = 2;
    copts.debugLevel = debugLevel;
    copts.typeInfoLevel = 1;

    static const char* kLibrariesWithConstants[] = {"test", nullptr};
    copts.librariesWithKnownMembers = kLibrariesWithConstants;

    copts.libraryMemberTypeCb = luauLibraryTypeLookup;
    copts.libraryMemberConstantCb = luauLibraryConstantLookupC;

    size_t bytecodeSize = 0;
    char* bytecode = luau_compile(source, strlen(source), &copts, &bytecodeSize);
    REQUIRE(bytecode);

    std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    initializeCodegen(L);

    if (luau_load(L, "name", bytecode, bytecodeSize, 0) == 0)
    {
        free(bytecode);

        Luau::CodeGen::AssemblyOptions options;
        setupAssemblyOptions(options, includeIrTypes);

        // Runtime mapping is specifically created to NOT match the compilation mapping
        options.compilationOptions.userdataTypes = kUserdataRunTypes;

        return Luau::CodeGen::getAssembly(L, -1, options, nullptr);
    }

    free(bytecode);

    FAIL("Failed to load bytecode");
    return "";
}

static std::string getCodegenHeader(const char* source)
{
    std::string assembly = getCodegenAssembly(source, /* includeIrTypes */ true, /* debugLevel */ 2);

    auto bytecodeStart = assembly.find("bb_bytecode_0:");

    if (bytecodeStart == std::string::npos)
        bytecodeStart = assembly.find("bb_0:");

    REQUIRE(bytecodeStart != std::string::npos);

    return assembly.substr(0, bytecodeStart);
}

TEST_SUITE_BEGIN("IrLowering");

TEST_CASE("VectorReciprocal")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
)"
    );
}

TEST_CASE("VectorComponentRead")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
)"
    );
}

TEST_CASE("VectorAdd")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
)"
    );
}

TEST_CASE("VectorMinus")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
)"
    );
}

TEST_CASE("VectorSubMulDiv")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
)"
    );
}

TEST_CASE("VectorSubMulDiv2")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
)"
    );
}

TEST_CASE("VectorMulDivMixed")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
)"
    );
}

TEST_CASE("VectorLerp")
{
    ScopedFastFlag _[]{
        {FFlag::LuauCompileVectorLerp, true},
        {FFlag::LuauTypeCheckerVectorLerp, true},
        {FFlag::LuauVectorLerp, true},
        {FFlag::LuauCodeGenVectorLerp, true}
    };
    if (FFlag::LuauCodeGenFMA)
    {
        CHECK_EQ(
            "\n" + getCodegenAssembly(R"(
local function vec3lerp(a: vector, b: vector, t: number)
    return vector.lerp(a, b, t)
end
)"),
            R"(
; function vec3lerp($arg0, $arg1, $arg2) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  CHECK_TAG R1, tvector, exit(entry)
  CHECK_TAG R2, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  CHECK_SAFE_ENV exit(2)
  %15 = LOAD_TVALUE R0
  %16 = LOAD_TVALUE R1
  %17 = LOAD_DOUBLE R2
  %18 = NUM_TO_VEC %17
  %19 = NUM_TO_VEC 1
  %20 = SUB_VEC %16, %15
  MULADD_VEC %20, %18, %15
  SELECT_VEC %21, %16, %18, %19
  %23 = TAG_VECTOR %22
  STORE_TVALUE R3, %23
  INTERRUPT 8u
  RETURN R3, 1i
)"
        );
    }
    else
    {
        CHECK_EQ(
            "\n" + getCodegenAssembly(R"(
local function vec3lerp(a: vector, b: vector, t: number)
    return vector.lerp(a, b, t)
end
)"),
            R"(
; function vec3lerp($arg0, $arg1, $arg2) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  CHECK_TAG R1, tvector, exit(entry)
  CHECK_TAG R2, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  CHECK_SAFE_ENV exit(2)
  %15 = LOAD_TVALUE R0
  %16 = LOAD_TVALUE R1
  %17 = LOAD_DOUBLE R2
  %18 = NUM_TO_VEC %17
  %19 = NUM_TO_VEC 1
  %20 = SUB_VEC %16, %15
  %21 = MUL_VEC %20, %18
  %22 = ADD_VEC %15, %21
  SELECT_VEC %22, %16, %18, %19
  %24 = TAG_VECTOR %23
  STORE_TVALUE R3, %24
  INTERRUPT 8u
  RETURN R3, 1i
)"
        );
    }
}

TEST_CASE("ExtraMathMemoryOperands")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
)"
    );
}

TEST_CASE("DseInitialStackState")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
  GET_CACHED_IMPORT R1, K1 (nil), 1073741824u ('_'), 4u
  SET_SAVEDPC 7u
  %14 = NEW_TABLE 0u, 0u
  STORE_POINTER R1, %14
  STORE_TAG R1, ttable
  STORE_TAG R0, tnil
  INTERRUPT 9u
  JUMP bb_bytecode_0
)"
    );
}

TEST_CASE("DseInitialStackState2")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
  FASTCALL 14u, R1, R0, 2i
  INTERRUPT 5u
  RETURN R0, 1i
)"
    );
}

TEST_CASE("StringCompare")
{
    ScopedFastFlag luauCodegenDirectCompare{FFlag::LuauCodegenDirectCompare, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a)
    return a == "test"
end
)"
               ),
        R"(
; function foo($arg0) line 2
bb_bytecode_0:
  %0 = LOAD_TAG R0
  %1 = LOAD_POINTER R0
  %2 = LOAD_POINTER K0 ('test')
  %3 = CMP_SPLIT_TVALUE %0, tstring, %1, %2, eq
  STORE_TAG R1, tboolean
  STORE_INT R1, %3
  JUMP bb_bytecode_2
bb_bytecode_2:
  INTERRUPT 4u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("StringCompareAnnotated")
{
    ScopedFastFlag luauCodegenDirectCompare{FFlag::LuauCodegenDirectCompare, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a: string)
    return a == "test"
end
)"
               ),
        R"(
; function foo($arg0) line 2
bb_0:
  CHECK_TAG R0, tstring, exit(entry)
  JUMP bb_4
bb_4:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %5 = LOAD_POINTER R0
  %6 = LOAD_POINTER K0 ('test')
  %7 = CMP_SPLIT_TVALUE tstring, tstring, %5, %6, eq
  STORE_TAG R1, tboolean
  STORE_INT R1, %7
  JUMP bb_bytecode_3
bb_bytecode_3:
  INTERRUPT 4u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("NilCompare")
{
    ScopedFastFlag luauCodegenDirectCompare{FFlag::LuauCodegenDirectCompare, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a)
    return a == nil
end
)"
               ),
        R"(
; function foo($arg0) line 2
bb_bytecode_0:
  %0 = LOAD_TAG R0
  %1 = CMP_TAG %0, tnil, eq
  STORE_TAG R1, tboolean
  STORE_INT R1, %1
  JUMP bb_bytecode_2
bb_bytecode_2:
  INTERRUPT 4u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("BooleanCompare")
{
    ScopedFastFlag luauCodegenDirectCompare{FFlag::LuauCodegenDirectCompare, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a)
    return { a == true, a == false, a ~= true, a ~= false }
end
)"
               ),
        R"(
; function foo($arg0) line 2
bb_bytecode_0:
  SET_SAVEDPC 1u
  %1 = NEW_TABLE 4u, 0u
  STORE_POINTER R1, %1
  STORE_TAG R1, ttable
  CHECK_GC
  %5 = LOAD_TAG R0
  %6 = LOAD_INT R0
  %7 = CMP_SPLIT_TVALUE %5, tboolean, %6, 1i, eq
  STORE_TAG R2, tboolean
  STORE_INT R2, %7
  JUMP bb_bytecode_2
bb_bytecode_2:
  %14 = LOAD_TAG R0
  %15 = LOAD_INT R0
  %16 = CMP_SPLIT_TVALUE %14, tboolean, %15, 0i, eq
  STORE_TAG R3, tboolean
  STORE_INT R3, %16
  JUMP bb_bytecode_4
bb_bytecode_4:
  %23 = LOAD_TAG R0
  %24 = LOAD_INT R0
  %25 = CMP_SPLIT_TVALUE %23, tboolean, %24, 1i, not_eq
  STORE_TAG R4, tboolean
  STORE_INT R4, %25
  JUMP bb_bytecode_6
bb_bytecode_6:
  %32 = LOAD_TAG R0
  %33 = LOAD_INT R0
  %34 = CMP_SPLIT_TVALUE %32, tboolean, %33, 0i, not_eq
  STORE_TAG R5, tboolean
  STORE_INT R5, %34
  JUMP bb_bytecode_8
bb_bytecode_8:
  SETLIST 18u, R1, R2, 4i, 1u, 4u
  INTERRUPT 20u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("NumberCompare")
{
    ScopedFastFlag luauCodegenDirectCompare{FFlag::LuauCodegenDirectCompare, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a)
    return { a == 4.0, a ~= 3.0 }
end
)"
               ),
        R"(
; function foo($arg0) line 2
bb_bytecode_0:
  SET_SAVEDPC 1u
  %1 = NEW_TABLE 2u, 0u
  STORE_POINTER R1, %1
  STORE_TAG R1, ttable
  CHECK_GC
  %5 = LOAD_TAG R0
  %6 = LOAD_DOUBLE R0
  %7 = CMP_SPLIT_TVALUE %5, tnumber, %6, 4, eq
  STORE_TAG R2, tboolean
  STORE_INT R2, %7
  JUMP bb_bytecode_2
bb_bytecode_2:
  %14 = LOAD_TAG R0
  %15 = LOAD_DOUBLE R0
  %16 = CMP_SPLIT_TVALUE %14, tnumber, %15, 3, not_eq
  STORE_TAG R3, tboolean
  STORE_INT R3, %16
  JUMP bb_bytecode_4
bb_bytecode_4:
  SETLIST 10u, R1, R2, 2i, 1u, 2u
  INTERRUPT 12u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("TypeCompare")
{
    ScopedFastFlag luauCodegenDirectCompare{FFlag::LuauCodegenDirectCompare, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a)
    return type(a) == "number"
end
)"
               ),
        R"(
; function foo($arg0) line 2
bb_bytecode_0:
  CHECK_SAFE_ENV exit(1)
  %1 = LOAD_TAG R0
  %2 = GET_TYPE %1
  STORE_POINTER R2, %2
  STORE_TAG R2, tstring
  %8 = CMP_TAG %1, tnumber, eq
  STORE_TAG R1, tboolean
  STORE_INT R1, %8
  JUMP bb_bytecode_2
bb_bytecode_2:
  INTERRUPT 9u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("TypeofCompare")
{
    ScopedFastFlag luauCodegenDirectCompare{FFlag::LuauCodegenDirectCompare, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a)
    return typeof(a) == "number"
end
)"
               ),
        R"(
; function foo($arg0) line 2
bb_bytecode_0:
  CHECK_SAFE_ENV exit(1)
  %1 = GET_TYPEOF R0
  STORE_POINTER R2, %1
  STORE_TAG R2, tstring
  %7 = CMP_TAG R0, tnumber, eq
  STORE_TAG R1, tboolean
  STORE_INT R1, %7
  JUMP bb_bytecode_2
bb_bytecode_2:
  INTERRUPT 9u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("TypeofCompareCustom")
{
    ScopedFastFlag luauCodegenDirectCompare{FFlag::LuauCodegenDirectCompare, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a)
    return typeof(a) == "User"
end
)"
               ),
        R"(
; function foo($arg0) line 2
bb_bytecode_0:
  CHECK_SAFE_ENV exit(1)
  %1 = GET_TYPEOF R0
  STORE_POINTER R2, %1
  STORE_TAG R2, tstring
  %6 = LOAD_POINTER K2 ('User')
  %7 = CMP_SPLIT_TVALUE tstring, tstring, %1, %6, eq
  STORE_TAG R1, tboolean
  STORE_INT R1, %7
  JUMP bb_bytecode_2
bb_bytecode_2:
  INTERRUPT 9u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("TypeCondition")
{
    ScopedFastFlag luauCodegenDirectCompare{FFlag::LuauCodegenDirectCompare, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a, b)
    if type(a) == "number" then
        return a + b
    end
    return nil
end
)"
               ),
        R"(
; function foo($arg0, $arg1) line 2
bb_bytecode_0:
  CHECK_SAFE_ENV exit(1)
  %1 = LOAD_TAG R0
  %2 = GET_TYPE %1
  STORE_POINTER R2, %2
  STORE_TAG R2, tstring
  JUMP bb_4
bb_4:
  %7 = LOAD_POINTER R2
  %8 = LOAD_POINTER K2 ('number')
  JUMP_EQ_POINTER %7, %8, bb_3, bb_bytecode_1
bb_3:
  CHECK_TAG R0, tnumber, bb_fallback_5
  CHECK_TAG R1, tnumber, bb_fallback_5
  %14 = LOAD_DOUBLE R0
  %16 = ADD_NUM %14, R1
  STORE_DOUBLE R2, %16
  STORE_TAG R2, tnumber
  JUMP bb_6
bb_6:
  INTERRUPT 8u
  RETURN R2, 1i
bb_bytecode_1:
  STORE_TAG R2, tnil
  INTERRUPT 10u
  RETURN R2, 1i
)"
    );
}

TEST_CASE("AssertTypeGuard")
{
    ScopedFastFlag luauCodegenDirectCompare{FFlag::LuauCodegenDirectCompare, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a)
    assert(type(a) == "number")
    return a * 2
end
)"
               ),
        R"(
; function foo($arg0) line 2
bb_bytecode_0:
  CHECK_SAFE_ENV exit(1)
  %1 = LOAD_TAG R0
  %2 = GET_TYPE %1
  STORE_POINTER R3, %2
  STORE_TAG R3, tstring
  %8 = CMP_TAG %1, tnumber, eq
  STORE_TAG R2, tboolean
  STORE_INT R2, %8
  JUMP bb_bytecode_2
bb_bytecode_2:
  CHECK_TRUTHY tboolean, R2, exit(10)
  JUMP bb_5
bb_5:
  CHECK_TAG R0, tnumber, bb_fallback_6
  %28 = LOAD_DOUBLE R0
  %29 = ADD_NUM %28, %28
  STORE_DOUBLE R1, %29
  STORE_TAG R1, tnumber
  JUMP bb_7
bb_7:
  INTERRUPT 14u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("VectorConstantTag")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
  %4 = LOAD_TVALUE K0 (1, 2, 3), 0i, tvector
  %11 = LOAD_TVALUE R0
  %12 = ADD_VEC %4, %11
  %13 = TAG_VECTOR %12
  STORE_TVALUE R1, %13
  INTERRUPT 2u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("VectorNamecall")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
  FALLBACK_NAMECALL 0u, R1, R0, K0 ('Abs')
  INTERRUPT 2u
  SET_SAVEDPC 3u
  CALL R1, 1i, -1i
  INTERRUPT 3u
  RETURN R1, -1i
)"
    );
}

TEST_CASE("VectorRandomProp")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(a: vector)
    return a.XX + a.YY + a.ZZ
end
)"),
        R"(
; function foo($arg0) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  FALLBACK_GETTABLEKS 0u, R3, R0, K0 ('XX')
  FALLBACK_GETTABLEKS 2u, R4, R0, K1 ('YY')
  CHECK_TAG R3, tnumber, bb_fallback_3
  CHECK_TAG R4, tnumber, bb_fallback_3
  %14 = LOAD_DOUBLE R3
  %16 = ADD_NUM %14, R4
  STORE_DOUBLE R2, %16
  STORE_TAG R2, tnumber
  JUMP bb_4
bb_4:
  CHECK_TAG R0, tvector, exit(5)
  FALLBACK_GETTABLEKS 5u, R3, R0, K2 ('ZZ')
  CHECK_TAG R2, tnumber, bb_fallback_5
  CHECK_TAG R3, tnumber, bb_fallback_5
  %30 = LOAD_DOUBLE R2
  %32 = ADD_NUM %30, R3
  STORE_DOUBLE R1, %32
  STORE_TAG R1, tnumber
  JUMP bb_6
bb_6:
  INTERRUPT 8u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("VectorCustomAccess")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function vec3magn(a: vector)
    return a.Magnitude * 3
end
)"),
        R"(
; function vec3magn($arg0) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %6 = LOAD_FLOAT R0, 0i
  %7 = LOAD_FLOAT R0, 4i
  %8 = LOAD_FLOAT R0, 8i
  %9 = MUL_NUM %6, %6
  %10 = MUL_NUM %7, %7
  %11 = MUL_NUM %8, %8
  %12 = ADD_NUM %9, %10
  %13 = ADD_NUM %12, %11
  %14 = SQRT_NUM %13
  %20 = MUL_NUM %14, 3
  STORE_DOUBLE R1, %20
  STORE_TAG R1, tnumber
  INTERRUPT 3u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("VectorCustomNamecall")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function vec3dot(a: vector, b: vector)
    return (a:Dot(b))
end
)"),
        R"(
; function vec3dot($arg0, $arg1) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  CHECK_TAG R1, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %6 = LOAD_TVALUE R1
  STORE_TVALUE R4, %6
  %12 = LOAD_FLOAT R0, 0i
  %13 = LOAD_FLOAT R4, 0i
  %14 = MUL_NUM %12, %13
  %15 = LOAD_FLOAT R0, 4i
  %16 = LOAD_FLOAT R4, 4i
  %17 = MUL_NUM %15, %16
  %18 = LOAD_FLOAT R0, 8i
  %19 = LOAD_FLOAT R4, 8i
  %20 = MUL_NUM %18, %19
  %21 = ADD_NUM %14, %17
  %22 = ADD_NUM %21, %20
  STORE_DOUBLE R2, %22
  STORE_TAG R2, tnumber
  INTERRUPT 4u
  RETURN R2, 1i
)"
    );
}

TEST_CASE("VectorCustomAccessChain")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(a: vector, b: vector)
    return a.Unit * b.Magnitude
end
)"),
        R"(
; function foo($arg0, $arg1) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  CHECK_TAG R1, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %8 = LOAD_FLOAT R0, 0i
  %9 = LOAD_FLOAT R0, 4i
  %10 = LOAD_FLOAT R0, 8i
  %11 = MUL_NUM %8, %8
  %12 = MUL_NUM %9, %9
  %13 = MUL_NUM %10, %10
  %14 = ADD_NUM %11, %12
  %15 = ADD_NUM %14, %13
  %16 = SQRT_NUM %15
  %17 = DIV_NUM 1, %16
  %18 = MUL_NUM %8, %17
  %19 = MUL_NUM %9, %17
  %20 = MUL_NUM %10, %17
  STORE_VECTOR R3, %18, %19, %20
  STORE_TAG R3, tvector
  %25 = LOAD_FLOAT R1, 0i
  %26 = LOAD_FLOAT R1, 4i
  %27 = LOAD_FLOAT R1, 8i
  %28 = MUL_NUM %25, %25
  %29 = MUL_NUM %26, %26
  %30 = MUL_NUM %27, %27
  %31 = ADD_NUM %28, %29
  %32 = ADD_NUM %31, %30
  %33 = SQRT_NUM %32
  %40 = LOAD_TVALUE R3
  %42 = NUM_TO_VEC %33
  %43 = MUL_VEC %40, %42
  %44 = TAG_VECTOR %43
  STORE_TVALUE R2, %44
  INTERRUPT 5u
  RETURN R2, 1i
)"
    );
}

TEST_CASE("VectorCustomNamecallChain")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(n: vector, b: vector, t: vector)
    return n:Cross(t):Dot(b) + 1
end
)"),
        R"(
; function foo($arg0, $arg1, $arg2) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  CHECK_TAG R1, tvector, exit(entry)
  CHECK_TAG R2, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %8 = LOAD_TVALUE R2
  STORE_TVALUE R6, %8
  %14 = LOAD_FLOAT R0, 0i
  %15 = LOAD_FLOAT R6, 0i
  %16 = LOAD_FLOAT R0, 4i
  %17 = LOAD_FLOAT R6, 4i
  %18 = LOAD_FLOAT R0, 8i
  %19 = LOAD_FLOAT R6, 8i
  %20 = MUL_NUM %16, %19
  %21 = MUL_NUM %18, %17
  %22 = SUB_NUM %20, %21
  %23 = MUL_NUM %18, %15
  %24 = MUL_NUM %14, %19
  %25 = SUB_NUM %23, %24
  %26 = MUL_NUM %14, %17
  %27 = MUL_NUM %16, %15
  %28 = SUB_NUM %26, %27
  STORE_VECTOR R4, %22, %25, %28
  STORE_TAG R4, tvector
  %31 = LOAD_TVALUE R1
  STORE_TVALUE R6, %31
  %37 = LOAD_FLOAT R4, 0i
  %38 = LOAD_FLOAT R6, 0i
  %39 = MUL_NUM %37, %38
  %40 = LOAD_FLOAT R4, 4i
  %41 = LOAD_FLOAT R6, 4i
  %42 = MUL_NUM %40, %41
  %43 = LOAD_FLOAT R4, 8i
  %44 = LOAD_FLOAT R6, 8i
  %45 = MUL_NUM %43, %44
  %46 = ADD_NUM %39, %42
  %47 = ADD_NUM %46, %45
  %53 = ADD_NUM %47, 1
  STORE_DOUBLE R3, %53
  STORE_TAG R3, tnumber
  INTERRUPT 9u
  RETURN R3, 1i
)"
    );
}

TEST_CASE("VectorCustomNamecallChain2")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
type Vertex = {n: vector, b: vector}

local function foo(v: Vertex, t: vector)
    return v.n:Cross(t):Dot(v.b) + 1
end
)"),
        R"(
; function foo($arg0, $arg1) line 4
bb_0:
  CHECK_TAG R0, ttable, exit(entry)
  CHECK_TAG R1, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %8 = LOAD_POINTER R0
  %9 = GET_SLOT_NODE_ADDR %8, 0u, K1 ('n')
  CHECK_SLOT_MATCH %9, K1 ('n'), bb_fallback_3
  %11 = LOAD_TVALUE %9, 0i
  STORE_TVALUE R3, %11
  JUMP bb_4
bb_4:
  %16 = LOAD_TVALUE R1
  STORE_TVALUE R5, %16
  CHECK_TAG R3, tvector, exit(3)
  CHECK_TAG R5, tvector, exit(3)
  %22 = LOAD_FLOAT R3, 0i
  %23 = LOAD_FLOAT R5, 0i
  %24 = LOAD_FLOAT R3, 4i
  %25 = LOAD_FLOAT R5, 4i
  %26 = LOAD_FLOAT R3, 8i
  %27 = LOAD_FLOAT R5, 8i
  %28 = MUL_NUM %24, %27
  %29 = MUL_NUM %26, %25
  %30 = SUB_NUM %28, %29
  %31 = MUL_NUM %26, %23
  %32 = MUL_NUM %22, %27
  %33 = SUB_NUM %31, %32
  %34 = MUL_NUM %22, %25
  %35 = MUL_NUM %24, %23
  %36 = SUB_NUM %34, %35
  STORE_VECTOR R3, %30, %33, %36
  CHECK_TAG R0, ttable, exit(6)
  %41 = LOAD_POINTER R0
  %42 = GET_SLOT_NODE_ADDR %41, 6u, K3 ('b')
  CHECK_SLOT_MATCH %42, K3 ('b'), bb_fallback_5
  %44 = LOAD_TVALUE %42, 0i
  STORE_TVALUE R5, %44
  JUMP bb_6
bb_6:
  CHECK_TAG R3, tvector, exit(8)
  CHECK_TAG R5, tvector, exit(8)
  %53 = LOAD_FLOAT R3, 0i
  %54 = LOAD_FLOAT R5, 0i
  %55 = MUL_NUM %53, %54
  %56 = LOAD_FLOAT R3, 4i
  %57 = LOAD_FLOAT R5, 4i
  %58 = MUL_NUM %56, %57
  %59 = LOAD_FLOAT R3, 8i
  %60 = LOAD_FLOAT R5, 8i
  %61 = MUL_NUM %59, %60
  %62 = ADD_NUM %55, %58
  %63 = ADD_NUM %62, %61
  %69 = ADD_NUM %63, 1
  STORE_DOUBLE R2, %69
  STORE_TAG R2, tnumber
  INTERRUPT 12u
  RETURN R2, 1i
)"
    );
}

TEST_CASE("UserDataGetIndex")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
  FALLBACK_GETTABLEKS 0u, R2, R0, K0 ('x')
  FALLBACK_GETTABLEKS 2u, R3, R0, K1 ('y')
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
)"
    );
}

TEST_CASE("UserDataSetIndex")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
  FALLBACK_SETTABLEKS 1u, R1, R0, K0 ('x')
  STORE_DOUBLE R1, 4
  FALLBACK_SETTABLEKS 4u, R1, R0, K1 ('y')
  INTERRUPT 6u
  RETURN R0, 0i
)"
    );
}

TEST_CASE("UserDataNamecall")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
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
  FALLBACK_NAMECALL 0u, R2, R0, K0 ('GetX')
  INTERRUPT 2u
  SET_SAVEDPC 3u
  CALL R2, 1i, 1i
  FALLBACK_NAMECALL 3u, R3, R0, K1 ('GetY')
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
)"
    );
}

TEST_CASE("ExplicitUpvalueAndLocalTypes")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local y: vector = ...

local function getsum(t)
    local x: vector = t
    return x.X + x.Y + y.X + y.Y
end
)",
                   /* includeIrTypes */ true
               ),
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
)"
    );
}

#if LUA_VECTOR_SIZE == 3
TEST_CASE("FastcallTypeInferThroughLocal")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function getsum(x, c)
    local v = vector(x, 2, 3)
    if c then
        return v.X + v.Y
    else
        return v.Z
    end
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function getsum($arg0, $arg1) line 2
; R2: vector from 0 to 18
bb_bytecode_0:
  STORE_DOUBLE R4, 2
  STORE_TAG R4, tnumber
  STORE_DOUBLE R5, 3
  STORE_TAG R5, tnumber
  CHECK_SAFE_ENV exit(4)
  CHECK_TAG R0, tnumber, exit(4)
  %11 = LOAD_DOUBLE R0
  STORE_VECTOR R2, %11, 2, 3
  STORE_TAG R2, tvector
  JUMP_IF_FALSY R1, bb_bytecode_1, bb_3
bb_3:
  CHECK_TAG R2, tvector, exit(9)
  %19 = LOAD_FLOAT R2, 0i
  %24 = LOAD_FLOAT R2, 4i
  %33 = ADD_NUM %19, %24
  STORE_DOUBLE R3, %33
  STORE_TAG R3, tnumber
  INTERRUPT 14u
  RETURN R3, 1i
bb_bytecode_1:
  CHECK_TAG R2, tvector, exit(15)
  %40 = LOAD_FLOAT R2, 8i
  STORE_DOUBLE R3, %40
  STORE_TAG R3, tnumber
  INTERRUPT 17u
  RETURN R3, 1i
)"
    );
}

TEST_CASE("FastcallTypeInferThroughUpvalue")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
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
                   /* includeIrTypes */ true
               ),
        R"(
; function getsum($arg0, $arg1) line 4
; U0: vector
bb_bytecode_0:
  STORE_DOUBLE R4, 2
  STORE_TAG R4, tnumber
  STORE_DOUBLE R5, 3
  STORE_TAG R5, tnumber
  CHECK_SAFE_ENV exit(4)
  CHECK_TAG R0, tnumber, exit(4)
  %11 = LOAD_DOUBLE R0
  STORE_VECTOR R2, %11, 2, 3
  STORE_TAG R2, tvector
  SET_UPVALUE U0, R2, tvector
  JUMP_IF_FALSY R1, bb_bytecode_1, bb_3
bb_3:
  GET_UPVALUE R4, U0
  CHECK_TAG R4, tvector, exit(11)
  %21 = LOAD_FLOAT R4, 0i
  STORE_DOUBLE R3, %21
  STORE_TAG R3, tnumber
  GET_UPVALUE R5, U0
  CHECK_TAG R5, tvector, exit(14)
  %27 = LOAD_FLOAT R5, 4i
  %36 = ADD_NUM %21, %27
  STORE_DOUBLE R2, %36
  STORE_TAG R2, tnumber
  INTERRUPT 17u
  RETURN R2, 1i
bb_bytecode_1:
  GET_UPVALUE R3, U0
  CHECK_TAG R3, tvector, exit(19)
  %44 = LOAD_FLOAT R3, 8i
  STORE_DOUBLE R2, %44
  STORE_TAG R2, tnumber
  INTERRUPT 21u
  RETURN R2, 1i
)"
    );
}
#endif

TEST_CASE("LoadAndMoveTypePropagation")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
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
                   /* includeIrTypes */ true
               ),
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
)"
    );
}

#if LUA_VECTOR_SIZE == 3
TEST_CASE("ArgumentTypeRefinement")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function getsum(x, y)
    x = vector(1, y, 3)
    return x.Y + x.Z
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function getsum($arg0, $arg1) line 2
; R0: vector [argument]
bb_bytecode_0:
  STORE_DOUBLE R3, 1
  STORE_TAG R3, tnumber
  STORE_DOUBLE R5, 3
  STORE_TAG R5, tnumber
  CHECK_SAFE_ENV exit(4)
  CHECK_TAG R1, tnumber, exit(4)
  %12 = LOAD_DOUBLE R1
  STORE_VECTOR R2, 1, %12, 3
  STORE_TAG R2, tvector
  %16 = LOAD_TVALUE R2
  STORE_TVALUE R0, %16
  %20 = LOAD_FLOAT R0, 4i
  %25 = LOAD_FLOAT R0, 8i
  %34 = ADD_NUM %20, %25
  STORE_DOUBLE R2, %34
  STORE_TAG R2, tnumber
  INTERRUPT 14u
  RETURN R2, 1i
)"
    );
}
#endif

TEST_CASE("InlineFunctionType")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function inl(v: vector, s: number)
    return v.Y * s
end

local function getsum(x)
    return inl(x, 3) + inl(x, 5)
end
)",
                   /* includeIrTypes */ true
               ),
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
  %8 = MUL_NUM %2, 3
  %13 = LOAD_FLOAT R0, 4i
  %19 = MUL_NUM %13, 5
  %28 = ADD_NUM %8, %19
  STORE_DOUBLE R1, %28
  STORE_TAG R1, tnumber
  INTERRUPT 7u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("ResolveTablePathTypes")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
type Vertex = {pos: vector, normal: vector}

local function foo(arr: {Vertex}, i)
    local v = arr[i]

    return v.pos.Y
end
)",
                   /* includeIrTypes */ true,
                   /* debugLevel */ 2
               ),
        R"(
; function foo(arr, i) line 4
; R0: table [argument 'arr']
; R2: table from 0 to 6 [local 'v']
; R4: vector from 3 to 5
bb_0:
  CHECK_TAG R0, ttable, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  CHECK_TAG R1, tnumber, bb_fallback_3
  %8 = LOAD_POINTER R0
  %9 = LOAD_DOUBLE R1
  %10 = TRY_NUM_TO_INDEX %9, bb_fallback_3
  %11 = SUB_INT %10, 1i
  CHECK_ARRAY_SIZE %8, %11, bb_fallback_3
  CHECK_NO_METATABLE %8, bb_fallback_3
  %14 = GET_ARR_ADDR %8, %11
  %15 = LOAD_TVALUE %14
  STORE_TVALUE R2, %15
  JUMP bb_4
bb_4:
  CHECK_TAG R2, ttable, exit(1)
  %23 = LOAD_POINTER R2
  %24 = GET_SLOT_NODE_ADDR %23, 1u, K0 ('pos')
  CHECK_SLOT_MATCH %24, K0 ('pos'), bb_fallback_5
  %26 = LOAD_TVALUE %24, 0i
  STORE_TVALUE R4, %26
  JUMP bb_6
bb_6:
  CHECK_TAG R4, tvector, exit(3)
  %33 = LOAD_FLOAT R4, 4i
  STORE_DOUBLE R3, %33
  STORE_TAG R3, tnumber
  INTERRUPT 5u
  RETURN R3, 1i
)"
    );
}

TEST_CASE("ResolvableSimpleMath")
{
    CHECK_EQ(
        "\n" + getCodegenHeader(R"(
type Vertex = { p: vector, uv: vector, n: vector, t: vector, b: vector, h: number }
local mesh: { vertices: {Vertex}, indices: {number} } = ...

local function compute()
    for i = 1,#mesh.indices,3 do
        local a = mesh.vertices[mesh.indices[i]]
        local b = mesh.vertices[mesh.indices[i + 1]]
        local c = mesh.vertices[mesh.indices[i + 2]]

        local vba = b.p - a.p
        local vca = c.p - a.p

        local uvba = b.uv - a.uv
        local uvca = c.uv - a.uv

        local r = 1.0 / (uvba.X * uvca.Y - uvca.X * uvba.Y);

        local sdir = (uvca.Y * vba - uvba.Y * vca) * r

        a.t += sdir
    end
end
)"),
        R"(
; function compute() line 5
; U0: table ['mesh']
; R2: number from 0 to 78 [local 'i']
; R3: table from 7 to 78 [local 'a']
; R4: table from 15 to 78 [local 'b']
; R5: table from 24 to 78 [local 'c']
; R6: vector from 33 to 78 [local 'vba']
; R7: vector from 37 to 38
; R7: vector from 38 to 78 [local 'vca']
; R8: vector from 37 to 38
; R8: vector from 42 to 43
; R8: vector from 43 to 78 [local 'uvba']
; R9: vector from 42 to 43
; R9: vector from 47 to 48
; R9: vector from 48 to 78 [local 'uvca']
; R10: vector from 47 to 48
; R10: vector from 52 to 53
; R10: number from 53 to 78 [local 'r']
; R11: vector from 52 to 53
; R11: vector from 65 to 78 [local 'sdir']
; R12: vector from 72 to 73
; R12: vector from 75 to 76
; R13: vector from 71 to 72
; R14: vector from 71 to 72
)"
    );
}

TEST_CASE("ResolveVectorNamecalls")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
type Vertex = {pos: vector, normal: vector}

local function foo(arr: {Vertex}, i)
    return arr[i].normal:Dot(vector(0.707, 0, 0.707))
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo($arg0, $arg1) line 4
; R0: table [argument]
; R2: vector from 4 to 6
bb_0:
  CHECK_TAG R0, ttable, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  CHECK_TAG R1, tnumber, bb_fallback_3
  %8 = LOAD_POINTER R0
  %9 = LOAD_DOUBLE R1
  %10 = TRY_NUM_TO_INDEX %9, bb_fallback_3
  %11 = SUB_INT %10, 1i
  CHECK_ARRAY_SIZE %8, %11, bb_fallback_3
  CHECK_NO_METATABLE %8, bb_fallback_3
  %14 = GET_ARR_ADDR %8, %11
  %15 = LOAD_TVALUE %14
  STORE_TVALUE R3, %15
  JUMP bb_4
bb_4:
  CHECK_TAG R3, ttable, bb_fallback_5
  %23 = LOAD_POINTER R3
  %24 = GET_SLOT_NODE_ADDR %23, 1u, K0 ('normal')
  CHECK_SLOT_MATCH %24, K0 ('normal'), bb_fallback_5
  %26 = LOAD_TVALUE %24, 0i
  STORE_TVALUE R2, %26
  JUMP bb_6
bb_6:
  %31 = LOAD_TVALUE K1 (0.707000017, 0, 0.707000017), 0i, tvector
  STORE_TVALUE R4, %31
  CHECK_TAG R2, tvector, exit(4)
  %37 = LOAD_FLOAT R2, 0i
  %38 = LOAD_FLOAT R4, 0i
  %39 = MUL_NUM %37, %38
  %40 = LOAD_FLOAT R2, 4i
  %41 = LOAD_FLOAT R4, 4i
  %42 = MUL_NUM %40, %41
  %43 = LOAD_FLOAT R2, 8i
  %44 = LOAD_FLOAT R4, 8i
  %45 = MUL_NUM %43, %44
  %46 = ADD_NUM %39, %42
  %47 = ADD_NUM %46, %45
  STORE_DOUBLE R2, %47
  STORE_TAG R2, tnumber
  ADJUST_STACK_TO_REG R2, 1i
  INTERRUPT 7u
  RETURN R2, -1i
)"
    );
}

TEST_CASE("ImmediateTypeAnnotationHelp")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(arr, i)
    return (arr[i] :: vector) / 5
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo($arg0, $arg1) line 2
; R3: vector from 1 to 2
bb_bytecode_0:
  CHECK_TAG R0, ttable, bb_fallback_1
  CHECK_TAG R1, tnumber, bb_fallback_1
  %4 = LOAD_POINTER R0
  %5 = LOAD_DOUBLE R1
  %6 = TRY_NUM_TO_INDEX %5, bb_fallback_1
  %7 = SUB_INT %6, 1i
  CHECK_ARRAY_SIZE %4, %7, bb_fallback_1
  CHECK_NO_METATABLE %4, bb_fallback_1
  %10 = GET_ARR_ADDR %4, %7
  %11 = LOAD_TVALUE %10
  STORE_TVALUE R3, %11
  JUMP bb_2
bb_2:
  CHECK_TAG R3, tvector, exit(1)
  %19 = LOAD_TVALUE R3
  %20 = NUM_TO_VEC 5
  %21 = DIV_VEC %19, %20
  %22 = TAG_VECTOR %21
  STORE_TVALUE R2, %22
  INTERRUPT 2u
  RETURN R2, 1i
)"
    );
}

#if LUA_VECTOR_SIZE == 3
TEST_CASE("UnaryTypeResolve")
{
    CHECK_EQ(
        "\n" + getCodegenHeader(R"(
local function foo(a, b: vector, c)
    local d = not a
    local e = -b
    local f = #c
    return (if d then e else vector(f, 2, 3)).X
end
)"),
        R"(
; function foo(a, b, c) line 2
; R1: vector [argument 'b']
; R3: boolean from 0 to 17 [local 'd']
; R4: vector from 1 to 17 [local 'e']
; R5: number from 2 to 17 [local 'f']
; R7: vector from 14 to 16
)"
    );
}
#endif

TEST_CASE("ForInManualAnnotation")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
type Vertex = {pos: vector, normal: vector}

local function foo(a: {Vertex})
    local sum = 0
    for k, v: Vertex in ipairs(a) do
        sum += v.pos.X
    end
    return sum
end
)",
                   /* includeIrTypes */ true,
                   /* debugLevel */ 2
               ),
        R"(
; function foo(a) line 4
; R0: table [argument 'a']
; R1: number from 0 to 14 [local 'sum']
; R5: number from 5 to 11 [local 'k']
; R6: table from 5 to 11 [local 'v']
; R8: vector from 8 to 10
bb_0:
  CHECK_TAG R0, ttable, exit(entry)
  JUMP bb_4
bb_4:
  JUMP bb_bytecode_1
bb_bytecode_1:
  STORE_DOUBLE R1, 0
  STORE_TAG R1, tnumber
  CHECK_SAFE_ENV exit(1)
  GET_CACHED_IMPORT R2, K1 (nil), 1073741824u ('ipairs'), 2u
  %8 = LOAD_TVALUE R0
  STORE_TVALUE R3, %8
  INTERRUPT 4u
  SET_SAVEDPC 5u
  CALL R2, 1i, 3i
  CHECK_SAFE_ENV exit(5)
  CHECK_TAG R3, ttable, bb_fallback_5
  CHECK_TAG R4, tnumber, bb_fallback_5
  JUMP_CMP_NUM R4, 0, not_eq, bb_fallback_5, bb_6
bb_6:
  STORE_TAG R2, tnil
  STORE_POINTER R4, 0i
  STORE_EXTRA R4, 128i
  STORE_TAG R4, tlightuserdata
  JUMP bb_bytecode_3
bb_bytecode_2:
  CHECK_TAG R6, ttable, exit(6)
  %28 = LOAD_POINTER R6
  %29 = GET_SLOT_NODE_ADDR %28, 6u, K2 ('pos')
  CHECK_SLOT_MATCH %29, K2 ('pos'), bb_fallback_7
  %31 = LOAD_TVALUE %29, 0i
  STORE_TVALUE R8, %31
  JUMP bb_8
bb_8:
  CHECK_TAG R8, tvector, exit(8)
  %38 = LOAD_FLOAT R8, 0i
  STORE_DOUBLE R7, %38
  STORE_TAG R7, tnumber
  CHECK_TAG R1, tnumber, exit(10)
  %45 = LOAD_DOUBLE R1
  %47 = ADD_NUM %45, %38
  STORE_DOUBLE R1, %47
  JUMP bb_bytecode_3
bb_bytecode_3:
  INTERRUPT 11u
  CHECK_TAG R2, tnil, bb_fallback_10
  %53 = LOAD_POINTER R3
  %54 = LOAD_INT R4
  %55 = GET_ARR_ADDR %53, %54
  CHECK_ARRAY_SIZE %53, %54, bb_9
  %57 = LOAD_TAG %55
  JUMP_EQ_TAG %57, tnil, bb_9, bb_11
bb_11:
  %59 = ADD_INT %54, 1i
  STORE_INT R4, %59
  %61 = INT_TO_NUM %59
  STORE_DOUBLE R5, %61
  STORE_TAG R5, tnumber
  %64 = LOAD_TVALUE %55
  STORE_TVALUE R6, %64
  JUMP bb_bytecode_2
bb_9:
  INTERRUPT 13u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("ForInAutoAnnotationIpairs")
{
    CHECK_EQ(
        "\n" + getCodegenHeader(R"(
type Vertex = {pos: vector, normal: vector}

local function foo(a: {Vertex})
    local sum = 0
    for k, v in ipairs(a) do
        local n = v.pos.X
        sum += n
    end
    return sum
end
)"),
        R"(
; function foo(a) line 4
; R0: table [argument 'a']
; R1: number from 0 to 14 [local 'sum']
; R5: number from 5 to 11 [local 'k']
; R6: table from 5 to 11 [local 'v']
; R7: number from 6 to 11 [local 'n']
; R8: vector from 8 to 10
)"
    );
}

TEST_CASE("ForInAutoAnnotationPairs")
{
    CHECK_EQ(
        "\n" + getCodegenHeader(R"(
type Vertex = {pos: vector, normal: vector}

local function foo(a: {[string]: Vertex})
    local sum = 0
    for k, v in pairs(a) do
        local n = v.pos.X
        sum += n
    end
    return sum
end
)"),
        R"(
; function foo(a) line 4
; R0: table [argument 'a']
; R1: number from 0 to 14 [local 'sum']
; R5: string from 5 to 11 [local 'k']
; R6: table from 5 to 11 [local 'v']
; R7: number from 6 to 11 [local 'n']
; R8: vector from 8 to 10
)"
    );
}

TEST_CASE("ForInAutoAnnotationGeneric")
{
    CHECK_EQ(
        "\n" + getCodegenHeader(R"(
type Vertex = {pos: vector, normal: vector}

local function foo(a: {Vertex})
    local sum = 0
    for k, v in a do
        local n = v.pos.X
        sum += n
    end
    return sum
end
)"),
        R"(
; function foo(a) line 4
; R0: table [argument 'a']
; R1: number from 0 to 13 [local 'sum']
; R5: number from 4 to 10 [local 'k']
; R6: table from 4 to 10 [local 'v']
; R7: number from 5 to 10 [local 'n']
; R8: vector from 7 to 9
)"
    );
}

TEST_CASE("CustomUserdataTypes")
{
    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    CHECK_EQ(
        "\n" + getCodegenHeader(R"(
local function foo(v: vec2, x: mat3)
    return v.X * x
end
)"),
        R"(
; function foo(v, x) line 2
; R0: vec2 [argument 'v']
; R1: mat3 [argument 'x']
)"
    );
}

TEST_CASE("CustomUserdataPropertyAccess")
{
    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(v: vec2)
    return v.X + v.Y
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo($arg0) line 2
; R0: vec2 [argument]
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %6 = LOAD_POINTER R0
  CHECK_USERDATA_TAG %6, 12i, exit(0)
  %8 = BUFFER_READF32 %6, 0i, tuserdata
  %15 = BUFFER_READF32 %6, 4i, tuserdata
  %24 = ADD_NUM %8, %15
  STORE_DOUBLE R1, %24
  STORE_TAG R1, tnumber
  INTERRUPT 5u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("CustomUserdataPropertyAccess2")
{
    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a: mat3)
    return a.Row1 * a.Row2
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo($arg0) line 2
; R0: mat3 [argument]
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  FALLBACK_GETTABLEKS 0u, R2, R0, K0 ('Row1')
  FALLBACK_GETTABLEKS 2u, R3, R0, K1 ('Row2')
  CHECK_TAG R2, tvector, exit(4)
  CHECK_TAG R3, tvector, exit(4)
  %14 = LOAD_TVALUE R2
  %15 = LOAD_TVALUE R3
  %16 = MUL_VEC %14, %15
  %17 = TAG_VECTOR %16
  STORE_TVALUE R1, %17
  INTERRUPT 5u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("CustomUserdataNamecall1")
{
    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a: vec2, b: vec2)
    return a:Dot(b)
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo($arg0, $arg1) line 2
; R0: vec2 [argument]
; R1: vec2 [argument]
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  CHECK_TAG R1, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %6 = LOAD_TVALUE R1
  STORE_TVALUE R4, %6
  %10 = LOAD_POINTER R0
  CHECK_USERDATA_TAG %10, 12i, exit(1)
  %14 = LOAD_POINTER R4
  CHECK_USERDATA_TAG %14, 12i, exit(1)
  %16 = BUFFER_READF32 %10, 0i, tuserdata
  %17 = BUFFER_READF32 %14, 0i, tuserdata
  %18 = MUL_NUM %16, %17
  %19 = BUFFER_READF32 %10, 4i, tuserdata
  %20 = BUFFER_READF32 %14, 4i, tuserdata
  %21 = MUL_NUM %19, %20
  %22 = ADD_NUM %18, %21
  STORE_DOUBLE R2, %22
  STORE_TAG R2, tnumber
  ADJUST_STACK_TO_REG R2, 1i
  INTERRUPT 4u
  RETURN R2, -1i
)"
    );
}

TEST_CASE("CustomUserdataNamecall2")
{
    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a: vec2, b: vec2)
    return a:Min(b)
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo($arg0, $arg1) line 2
; R0: vec2 [argument]
; R1: vec2 [argument]
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  CHECK_TAG R1, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %6 = LOAD_TVALUE R1
  STORE_TVALUE R4, %6
  %10 = LOAD_POINTER R0
  CHECK_USERDATA_TAG %10, 12i, exit(1)
  %14 = LOAD_POINTER R4
  CHECK_USERDATA_TAG %14, 12i, exit(1)
  %16 = BUFFER_READF32 %10, 0i, tuserdata
  %17 = BUFFER_READF32 %14, 0i, tuserdata
  %18 = MIN_NUM %16, %17
  %19 = BUFFER_READF32 %10, 4i, tuserdata
  %20 = BUFFER_READF32 %14, 4i, tuserdata
  %21 = MIN_NUM %19, %20
  CHECK_GC
  %23 = NEW_USERDATA 8i, 12i
  BUFFER_WRITEF32 %23, 0i, %18, tuserdata
  BUFFER_WRITEF32 %23, 4i, %21, tuserdata
  STORE_POINTER R2, %23
  STORE_TAG R2, tuserdata
  ADJUST_STACK_TO_REG R2, 1i
  INTERRUPT 4u
  RETURN R2, -1i
)"
    );
}

TEST_CASE("CustomUserdataMetamethodDirectFlow")
{
    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a: mat3, b: mat3)
    return a * b
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo($arg0, $arg1) line 2
; R0: mat3 [argument]
; R1: mat3 [argument]
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  CHECK_TAG R1, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  SET_SAVEDPC 1u
  DO_ARITH R2, R0, R1, 10i
  INTERRUPT 1u
  RETURN R2, 1i
)"
    );
}

TEST_CASE("CustomUserdataMetamethodDirectFlow2")
{
    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a: mat3)
    return -a
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo($arg0) line 2
; R0: mat3 [argument]
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  SET_SAVEDPC 1u
  DO_ARITH R1, R0, R0, 15i
  INTERRUPT 1u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("CustomUserdataMetamethodDirectFlow3")
{
    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a: sequence)
    return #a
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo($arg0) line 2
; R0: userdata [argument]
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  SET_SAVEDPC 1u
  DO_LEN R1, R0
  INTERRUPT 1u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("CustomUserdataMetamethod")
{
    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a: vec2, b: vec2, c: vec2)
    return -c + a * b
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo($arg0, $arg1, $arg2) line 2
; R0: vec2 [argument]
; R1: vec2 [argument]
; R2: vec2 [argument]
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  CHECK_TAG R1, tuserdata, exit(entry)
  CHECK_TAG R2, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %10 = LOAD_POINTER R2
  CHECK_USERDATA_TAG %10, 12i, exit(0)
  %12 = BUFFER_READF32 %10, 0i, tuserdata
  %13 = BUFFER_READF32 %10, 4i, tuserdata
  %14 = UNM_NUM %12
  %15 = UNM_NUM %13
  CHECK_GC
  %17 = NEW_USERDATA 8i, 12i
  BUFFER_WRITEF32 %17, 0i, %14, tuserdata
  BUFFER_WRITEF32 %17, 4i, %15, tuserdata
  STORE_POINTER R4, %17
  STORE_TAG R4, tuserdata
  %26 = LOAD_POINTER R0
  CHECK_USERDATA_TAG %26, 12i, exit(1)
  %28 = LOAD_POINTER R1
  CHECK_USERDATA_TAG %28, 12i, exit(1)
  %30 = BUFFER_READF32 %26, 0i, tuserdata
  %31 = BUFFER_READF32 %28, 0i, tuserdata
  %32 = MUL_NUM %30, %31
  %33 = BUFFER_READF32 %26, 4i, tuserdata
  %34 = BUFFER_READF32 %28, 4i, tuserdata
  %35 = MUL_NUM %33, %34
  %37 = NEW_USERDATA 8i, 12i
  BUFFER_WRITEF32 %37, 0i, %32, tuserdata
  BUFFER_WRITEF32 %37, 4i, %35, tuserdata
  STORE_POINTER R5, %37
  STORE_TAG R5, tuserdata
  %50 = BUFFER_READF32 %17, 0i, tuserdata
  %51 = BUFFER_READF32 %37, 0i, tuserdata
  %52 = ADD_NUM %50, %51
  %53 = BUFFER_READF32 %17, 4i, tuserdata
  %54 = BUFFER_READF32 %37, 4i, tuserdata
  %55 = ADD_NUM %53, %54
  %57 = NEW_USERDATA 8i, 12i
  BUFFER_WRITEF32 %57, 0i, %52, tuserdata
  BUFFER_WRITEF32 %57, 4i, %55, tuserdata
  STORE_POINTER R3, %57
  STORE_TAG R3, tuserdata
  INTERRUPT 3u
  RETURN R3, 1i
)"
    );
}

TEST_CASE("LibraryFieldTypesAndConstants")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a: vector)
    return Vector3.xAxis * a + Vector3.yAxis
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo($arg0) line 2
; R0: vector [argument]
; R2: vector from 3 to 4
; R3: vector from 1 to 2
; R3: vector from 3 to 4
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %4 = LOAD_TVALUE K0 (1, 0, 0), 0i, tvector
  %11 = LOAD_TVALUE R0
  %12 = MUL_VEC %4, %11
  %15 = LOAD_TVALUE K1 (0, 1, 0), 0i, tvector
  %23 = ADD_VEC %12, %15
  %24 = TAG_VECTOR %23
  STORE_TVALUE R1, %24
  INTERRUPT 4u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("LibraryFieldTypesAndConstants")
{
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a: vector)
    local x = vector.zero
    x += a
    return x
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo($arg0) line 2
; R0: vector [argument]
; R1: vector from 0 to 3
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %4 = LOAD_TVALUE K0 (0, 0, 0), 0i, tvector
  %11 = LOAD_TVALUE R0
  %12 = ADD_VEC %4, %11
  %13 = TAG_VECTOR %12
  STORE_TVALUE R1, %13
  INTERRUPT 2u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("LibraryFieldTypesAndConstantsCApi")
{
    CHECK_EQ(
        "\n" + getCodegenAssemblyUsingCApi(
                   R"(
local function foo()
    return test.some_nil, test.some_boolean, test.some_number, test.some_vector, test.some_string
end
)",
                   /* includeIrTypes */ true
               ),
        R"(
; function foo() line 2
bb_bytecode_0:
  STORE_TAG R0, tnil
  STORE_INT R1, 1i
  STORE_TAG R1, tboolean
  STORE_DOUBLE R2, 4.75
  STORE_TAG R2, tnumber
  %5 = LOAD_TVALUE K1 (1, 2, 4), 0i, tvector
  STORE_TVALUE R3, %5
  %7 = LOAD_TVALUE K2 ('test'), 0i, tstring
  STORE_TVALUE R4, %7
  INTERRUPT 5u
  RETURN R0, 5i
)"
    );
}

TEST_CASE("Bit32BtestDirect")
{
    ScopedFastFlag luauCodeGenDirectBtest{FFlag::LuauCodeGenDirectBtest, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(a: number)
    return bit32.btest(a, 0x1f)
end
)"),
        R"(
; function foo($arg0) line 2
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  CHECK_SAFE_ENV exit(2)
  %7 = LOAD_DOUBLE R0
  %8 = NUM_TO_UINT %7
  %10 = BITAND_UINT %8, 31i
  %11 = CMP_INT %10, 0i, not_eq
  STORE_INT R1, %11
  STORE_TAG R1, tboolean
  INTERRUPT 7u
  RETURN R1, 1i
)"
    );
}

TEST_SUITE_END();
