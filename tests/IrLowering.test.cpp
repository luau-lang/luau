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

LUAU_FASTFLAG(LuauCompileUnusedUdataFix)
LUAU_FASTFLAG(LuauCodegenLoopStepDetectFix)
LUAU_FASTFLAG(LuauCodegenFloatLoadStoreProp)
LUAU_FASTFLAG(LuauCodegenLoadFloatSubstituteLast)
LUAU_FASTFLAG(LuauCodegenBlockSafeEnv)
LUAU_FASTFLAG(LuauCodegenChainLink)
LUAU_FASTFLAG(LuauCodegenIntegerAddSub)
LUAU_FASTFLAG(LuauCodegenSetBlockEntryState)
LUAU_FASTFLAG(LuauCodegenBetterSccRemoval)
LUAU_FASTFLAG(LuauCodegenLinearAndOr)
LUAU_FASTFLAG(LuauCodegenHydrateLoadWithTag)
LUAU_FASTFLAG(LuauCodegenUpvalueLoadProp)
LUAU_FASTFLAG(LuauCodegenGcoDse)
LUAU_FASTFLAG(LuauCodegenBufferLoadProp2)
LUAU_FASTFLAG(LuauCodegenNumIntFolds2)
LUAU_FASTFLAG(LuauCodegenNumToUintFoldRange)
LUAU_FASTFLAG(LuauCodegenSplitFloat)

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

static std::string getCodegenAssembly(const char* source, bool includeIrTypes = false, int debugLevel = 1, int optimizationLevel = 2)
{
    Luau::Allocator allocator;
    Luau::AstNameTable names(allocator);
    Luau::ParseResult result = Luau::Parser::parse(source, strlen(source), names, allocator);

    if (!result.errors.empty())
        throw Luau::ParseErrors(result.errors);

    Luau::CompileOptions copts = {};

    copts.optimizationLevel = optimizationLevel;
    copts.debugLevel = debugLevel;
    copts.typeInfoLevel = 1;
    copts.vectorCtor = "vector";
    copts.vectorType = "vector";

    static const char* kUserdataCompileTypes[] = {"vec2", "color", "mat3", "vertex", nullptr};
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

        std::string result = Luau::CodeGen::getAssembly(L, -1, options, nullptr);

        if (Luau::CodeGen::isSupported())
        {
            // Checking that other target lower correctly as well
            options.target = Luau::CodeGen::AssemblyOptions::Target::A64;
            Luau::CodeGen::getAssembly(L, -1, options, nullptr);
        }

        return result;
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
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

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
  %7 = LOAD_TVALUE R0, 0i, tvector
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
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %7 = FLOAT_TO_NUM %6
  %12 = LOAD_FLOAT R0, 4i
  %13 = FLOAT_TO_NUM %12
  %22 = ADD_NUM %7, %13
  %27 = LOAD_FLOAT R0, 8i
  %28 = FLOAT_TO_NUM %27
  %37 = ADD_NUM %22, %28
  STORE_DOUBLE R1, %37
  STORE_TAG R1, tnumber
  INTERRUPT 8u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("VectorAdd")
{
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

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
  %10 = LOAD_TVALUE R0, 0i, tvector
  %11 = LOAD_TVALUE R1, 0i, tvector
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
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

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
  %6 = LOAD_TVALUE R0, 0i, tvector
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
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

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
  %14 = LOAD_TVALUE R0, 0i, tvector
  %15 = LOAD_TVALUE R1, 0i, tvector
  %16 = MUL_VEC %14, %15
  %23 = LOAD_TVALUE R2, 0i, tvector
  %24 = LOAD_TVALUE R3, 0i, tvector
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
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

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
  %8 = LOAD_TVALUE R0, 0i, tvector
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
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

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
  %12 = LOAD_TVALUE R0, 0i, tvector
  %13 = NUM_TO_VEC 2
  %14 = MUL_VEC %12, %13
  %19 = LOAD_TVALUE R1, 0i, tvector
  %20 = NUM_TO_VEC 4
  %21 = DIV_VEC %19, %20
  %30 = ADD_VEC %14, %21
  %40 = NUM_TO_VEC 0.5
  %41 = LOAD_TVALUE R2, 0i, tvector
  %42 = MUL_VEC %40, %41
  %51 = ADD_VEC %30, %42
  %56 = NUM_TO_VEC 40
  %57 = LOAD_TVALUE R3, 0i, tvector
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
        {FFlag::LuauCodegenBlockSafeEnv, true},
        {FFlag::LuauCodegenHydrateLoadWithTag, true}
    };
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
  implicit CHECK_SAFE_ENV exit(0)
  %15 = LOAD_TVALUE R0, 0i, tvector
  %16 = LOAD_TVALUE R1, 0i, tvector
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

TEST_CASE("ExtraMathMemoryOperands")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};

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
  implicit CHECK_SAFE_ENV exit(0)
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
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};

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
  implicit CHECK_SAFE_ENV exit(0)
  CHECK_TAG R0, tnumber, exit(1)
  FASTCALL 14u, R1, R0, 2i
  INTERRUPT 5u
  RETURN R0, 1i
)"
    );
}

TEST_CASE("StringCompare")
{
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
    ScopedFastFlag luauCodegenChainLink{FFlag::LuauCodegenChainLink, true};

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
  %16 = CMP_SPLIT_TVALUE %5, tboolean, %6, 0i, eq
  STORE_TAG R3, tboolean
  STORE_INT R3, %16
  JUMP bb_bytecode_4
bb_bytecode_4:
  %25 = CMP_SPLIT_TVALUE %5, tboolean, %6, 1i, not_eq
  STORE_TAG R4, tboolean
  STORE_INT R4, %25
  JUMP bb_bytecode_6
bb_bytecode_6:
  %34 = CMP_SPLIT_TVALUE %5, tboolean, %6, 0i, not_eq
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
    ScopedFastFlag luauCodegenChainLink{FFlag::LuauCodegenChainLink, true};

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
  %16 = CMP_SPLIT_TVALUE %5, tnumber, %6, 3, not_eq
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
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenGcoDse{FFlag::LuauCodegenGcoDse, true};

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
  implicit CHECK_SAFE_ENV exit(0)
  %1 = LOAD_TAG R0
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
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenGcoDse{FFlag::LuauCodegenGcoDse, true};

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
  implicit CHECK_SAFE_ENV exit(0)
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
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenGcoDse{FFlag::LuauCodegenGcoDse, true};

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
  implicit CHECK_SAFE_ENV exit(0)
  %1 = GET_TYPEOF R0
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
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenChainLink{FFlag::LuauCodegenChainLink, true};

    // TODO: opportunity 1 - first store to R2 is dead, but dead store op doesn't go through glued chains yet
    // TODO: opportunity 2 - bb_4 already made sure %1 == R0.tag is a number, check in bb_3 can be removed
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
  implicit CHECK_SAFE_ENV exit(0)
  %1 = LOAD_TAG R0
  %2 = GET_TYPE %1
  STORE_POINTER R2, %2
  STORE_TAG R2, tstring
  JUMP bb_4
bb_4:
  JUMP_EQ_TAG %1, tnumber, bb_3, bb_bytecode_1
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

TEST_CASE("TypeCondition2")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenChainLink{FFlag::LuauCodegenChainLink, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a, b)
    if type(a) == "number" and type(b) == "number" then
        return a + b
    end
    return nil
end
)"
               ),
        R"(
; function foo($arg0, $arg1) line 2
bb_bytecode_0:
  implicit CHECK_SAFE_ENV exit(0)
  %1 = LOAD_TAG R0
  %2 = GET_TYPE %1
  STORE_POINTER R2, %2
  STORE_TAG R2, tstring
  JUMP bb_4
bb_4:
  JUMP_EQ_TAG %1, tnumber, bb_3, bb_bytecode_1
bb_3:
  CHECK_SAFE_ENV exit(8)
  %11 = LOAD_TAG R1
  %12 = GET_TYPE %11
  STORE_POINTER R2, %12
  STORE_TAG R2, tstring
  JUMP bb_7
bb_7:
  JUMP_EQ_TAG %11, tnumber, bb_6, bb_bytecode_1
bb_6:
  CHECK_TAG R0, tnumber, bb_fallback_8
  CHECK_TAG R1, tnumber, bb_fallback_8
  %24 = LOAD_DOUBLE R0
  %26 = ADD_NUM %24, R1
  STORE_DOUBLE R2, %26
  STORE_TAG R2, tnumber
  JUMP bb_9
bb_9:
  INTERRUPT 15u
  RETURN R2, 1i
bb_bytecode_1:
  STORE_TAG R2, tnil
  INTERRUPT 17u
  RETURN R2, 1i
)"
    );
}

TEST_CASE("AssertTypeGuard")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenChainLink{FFlag::LuauCodegenChainLink, true};

    // TODO: opportunity - CHECK_TRUTHY indirectly establishes that %1 is a number for CHECK_TAG in bb_5
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
  implicit CHECK_SAFE_ENV exit(0)
  %1 = LOAD_TAG R0
  %2 = GET_TYPE %1
  STORE_POINTER R3, %2
  STORE_TAG R3, tstring
  %8 = CMP_TAG %1, tnumber, eq
  STORE_TAG R2, tboolean
  STORE_INT R2, %8
  JUMP bb_bytecode_2
bb_bytecode_2:
  CHECK_TRUTHY tboolean, %8, exit(10)
  JUMP bb_5
bb_5:
  CHECK_TAG %1, tnumber, bb_fallback_6
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
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

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
  %11 = LOAD_TVALUE R0, 0i, tvector
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
    ScopedFastFlag luauCodegenSetBlockEntryState{FFlag::LuauCodegenSetBlockEntryState, true};

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
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %9 = FLOAT_TO_NUM %6
  %10 = FLOAT_TO_NUM %7
  %11 = FLOAT_TO_NUM %8
  %12 = MUL_NUM %9, %9
  %13 = MUL_NUM %10, %10
  %14 = MUL_NUM %11, %11
  %15 = ADD_NUM %12, %13
  %16 = ADD_NUM %15, %14
  %17 = SQRT_NUM %16
  %23 = MUL_NUM %17, 3
  STORE_DOUBLE R1, %23
  STORE_TAG R1, tnumber
  INTERRUPT 3u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("VectorCustomNamecall")
{
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};
    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %6 = LOAD_TVALUE R1, 0i, tvector
  %12 = LOAD_FLOAT R0, 0i
  %13 = EXTRACT_VEC %6, 0i
  %14 = FLOAT_TO_NUM %12
  %15 = FLOAT_TO_NUM %13
  %16 = MUL_NUM %14, %15
  %17 = LOAD_FLOAT R0, 4i
  %18 = EXTRACT_VEC %6, 1i
  %19 = FLOAT_TO_NUM %17
  %20 = FLOAT_TO_NUM %18
  %21 = MUL_NUM %19, %20
  %22 = LOAD_FLOAT R0, 8i
  %23 = EXTRACT_VEC %6, 2i
  %24 = FLOAT_TO_NUM %22
  %25 = FLOAT_TO_NUM %23
  %26 = MUL_NUM %24, %25
  %27 = ADD_NUM %16, %21
  %28 = ADD_NUM %27, %26
  STORE_DOUBLE R2, %28
  STORE_TAG R2, tnumber
  INTERRUPT 4u
  RETURN R2, 1i
)"
    );
}

TEST_CASE("VectorCustomNamecall2")
{
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};
    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function vec3dot(a: vector)
    return (a:Dot(vector.create(1, 2, 3)))
end
)"),
        R"(
; function vec3dot($arg0) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %10 = LOAD_FLOAT R0, 0i
  %12 = FLOAT_TO_NUM %10
  %15 = LOAD_FLOAT R0, 4i
  %17 = FLOAT_TO_NUM %15
  %19 = ADD_NUM %17, %17
  %20 = LOAD_FLOAT R0, 8i
  %22 = FLOAT_TO_NUM %20
  %24 = MUL_NUM %22, 3
  %25 = ADD_NUM %12, %19
  %26 = ADD_NUM %25, %24
  STORE_DOUBLE R1, %26
  STORE_TAG R1, tnumber
  INTERRUPT 4u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("VectorCustomAccessChain")
{
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %11 = FLOAT_TO_NUM %8
  %12 = FLOAT_TO_NUM %9
  %13 = FLOAT_TO_NUM %10
  %14 = MUL_NUM %11, %11
  %15 = MUL_NUM %12, %12
  %16 = MUL_NUM %13, %13
  %17 = ADD_NUM %14, %15
  %18 = ADD_NUM %17, %16
  %19 = SQRT_NUM %18
  %20 = DIV_NUM 1, %19
  %21 = MUL_NUM %11, %20
  %22 = MUL_NUM %12, %20
  %23 = MUL_NUM %13, %20
  %24 = NUM_TO_FLOAT %21
  %25 = NUM_TO_FLOAT %22
  %26 = NUM_TO_FLOAT %23
  STORE_VECTOR R3, %24, %25, %26
  STORE_TAG R3, tvector
  %31 = LOAD_FLOAT R1, 0i
  %32 = LOAD_FLOAT R1, 4i
  %33 = LOAD_FLOAT R1, 8i
  %34 = FLOAT_TO_NUM %31
  %35 = FLOAT_TO_NUM %32
  %36 = FLOAT_TO_NUM %33
  %37 = MUL_NUM %34, %34
  %38 = MUL_NUM %35, %35
  %39 = MUL_NUM %36, %36
  %40 = ADD_NUM %37, %38
  %41 = ADD_NUM %40, %39
  %42 = SQRT_NUM %41
  %49 = LOAD_TVALUE R3, 0i, tvector
  %51 = NUM_TO_VEC %42
  %52 = MUL_VEC %49, %51
  %53 = TAG_VECTOR %52
  STORE_TVALUE R2, %53
  INTERRUPT 5u
  RETURN R2, 1i
)"
    );
}

TEST_CASE("VectorCustomNamecallChain")
{
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};
    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %8 = LOAD_TVALUE R2, 0i, tvector
  %14 = LOAD_FLOAT R0, 0i
  %15 = EXTRACT_VEC %8, 0i
  %16 = LOAD_FLOAT R0, 4i
  %17 = EXTRACT_VEC %8, 1i
  %18 = LOAD_FLOAT R0, 8i
  %19 = EXTRACT_VEC %8, 2i
  %20 = FLOAT_TO_NUM %14
  %21 = FLOAT_TO_NUM %15
  %22 = FLOAT_TO_NUM %16
  %23 = FLOAT_TO_NUM %17
  %24 = FLOAT_TO_NUM %18
  %25 = FLOAT_TO_NUM %19
  %26 = MUL_NUM %22, %25
  %27 = MUL_NUM %24, %23
  %28 = SUB_NUM %26, %27
  %29 = MUL_NUM %24, %21
  %30 = MUL_NUM %20, %25
  %31 = SUB_NUM %29, %30
  %32 = MUL_NUM %20, %23
  %33 = MUL_NUM %22, %21
  %34 = SUB_NUM %32, %33
  %35 = NUM_TO_FLOAT %28
  %36 = NUM_TO_FLOAT %31
  %37 = NUM_TO_FLOAT %34
  %40 = LOAD_TVALUE R1, 0i, tvector
  %47 = EXTRACT_VEC %40, 0i
  %48 = FLOAT_TO_NUM %35
  %49 = FLOAT_TO_NUM %47
  %50 = MUL_NUM %48, %49
  %52 = EXTRACT_VEC %40, 1i
  %53 = FLOAT_TO_NUM %36
  %54 = FLOAT_TO_NUM %52
  %55 = MUL_NUM %53, %54
  %57 = EXTRACT_VEC %40, 2i
  %58 = FLOAT_TO_NUM %37
  %59 = FLOAT_TO_NUM %57
  %60 = MUL_NUM %58, %59
  %61 = ADD_NUM %50, %55
  %62 = ADD_NUM %61, %60
  %68 = ADD_NUM %62, 1
  STORE_DOUBLE R3, %68
  STORE_TAG R3, tnumber
  INTERRUPT 9u
  RETURN R3, 1i
)"
    );
}

TEST_CASE("VectorCustomNamecallChain2")
{
    ScopedFastFlag luauCodegenSetBlockEntryState{FFlag::LuauCodegenSetBlockEntryState, true};
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};
    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %16 = LOAD_TVALUE R1, 0i, tvector
  STORE_TVALUE R5, %16
  CHECK_TAG R3, tvector, exit(3)
  %22 = LOAD_FLOAT R3, 0i
  %23 = EXTRACT_VEC %16, 0i
  %24 = LOAD_FLOAT R3, 4i
  %25 = EXTRACT_VEC %16, 1i
  %26 = LOAD_FLOAT R3, 8i
  %27 = EXTRACT_VEC %16, 2i
  %28 = FLOAT_TO_NUM %22
  %29 = FLOAT_TO_NUM %23
  %30 = FLOAT_TO_NUM %24
  %31 = FLOAT_TO_NUM %25
  %32 = FLOAT_TO_NUM %26
  %33 = FLOAT_TO_NUM %27
  %34 = MUL_NUM %30, %33
  %35 = MUL_NUM %32, %31
  %36 = SUB_NUM %34, %35
  %37 = MUL_NUM %32, %29
  %38 = MUL_NUM %28, %33
  %39 = SUB_NUM %37, %38
  %40 = MUL_NUM %28, %31
  %41 = MUL_NUM %30, %29
  %42 = SUB_NUM %40, %41
  %43 = NUM_TO_FLOAT %36
  %44 = NUM_TO_FLOAT %39
  %45 = NUM_TO_FLOAT %42
  STORE_VECTOR R3, %43, %44, %45
  %50 = LOAD_POINTER R0
  %51 = GET_SLOT_NODE_ADDR %50, 6u, K3 ('b')
  CHECK_SLOT_MATCH %51, K3 ('b'), bb_fallback_5
  %53 = LOAD_TVALUE %51, 0i
  STORE_TVALUE R5, %53
  JUMP bb_6
bb_6:
  CHECK_TAG R3, tvector, exit(8)
  CHECK_TAG R5, tvector, exit(8)
  %62 = LOAD_FLOAT R3, 0i
  %63 = LOAD_FLOAT R5, 0i
  %64 = FLOAT_TO_NUM %62
  %65 = FLOAT_TO_NUM %63
  %66 = MUL_NUM %64, %65
  %67 = LOAD_FLOAT R3, 4i
  %68 = LOAD_FLOAT R5, 4i
  %69 = FLOAT_TO_NUM %67
  %70 = FLOAT_TO_NUM %68
  %71 = MUL_NUM %69, %70
  %72 = LOAD_FLOAT R3, 8i
  %73 = LOAD_FLOAT R5, 8i
  %74 = FLOAT_TO_NUM %72
  %75 = FLOAT_TO_NUM %73
  %76 = MUL_NUM %74, %75
  %77 = ADD_NUM %66, %71
  %78 = ADD_NUM %77, %76
  %84 = ADD_NUM %78, 1
  STORE_DOUBLE R2, %84
  STORE_TAG R2, tnumber
  INTERRUPT 12u
  RETURN R2, 1i
)"
    );
}

TEST_CASE("VectorLibraryChain")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(a: vector, b: vector)
    return vector.normalize(a) * (vector.magnitude(b) + vector.dot(a, b))
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
  implicit CHECK_SAFE_ENV exit(0)
  %9 = LOAD_TVALUE R0, 0i, tvector
  %10 = DOT_VEC %9, %9
  %11 = FLOAT_TO_NUM %10
  %12 = SQRT_NUM %11
  %13 = DIV_NUM 1, %12
  %14 = NUM_TO_VEC %13
  %15 = MUL_VEC %9, %14
  %21 = LOAD_TVALUE R1, 0i, tvector
  %22 = DOT_VEC %21, %21
  %23 = FLOAT_TO_NUM %22
  %24 = SQRT_NUM %23
  %34 = DOT_VEC %9, %21
  %35 = FLOAT_TO_NUM %34
  %44 = ADD_NUM %24, %35
  %53 = NUM_TO_VEC %44
  %54 = MUL_VEC %15, %53
  %55 = TAG_VECTOR %54
  STORE_TVALUE R2, %55
  INTERRUPT 19u
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

TEST_CASE("EntryBlockChecksAreNotInferred")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenSetBlockEntryState{FFlag::LuauCodegenSetBlockEntryState, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
function eq(a: number, b: number, limit)
  if not limit then limit = 0.125 end
  return math.abs(a - b) <= limit
end
)"
               ),
        R"(
; function eq($arg0, $arg1, $arg2) line 2
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  JUMP bb_5
bb_5:
  JUMP bb_bytecode_1
bb_bytecode_1:
  JUMP_IF_TRUTHY R2, bb_bytecode_2, bb_6
bb_6:
  STORE_DOUBLE R2, 0.125
  STORE_TAG R2, tnumber
  JUMP bb_bytecode_2
bb_bytecode_2:
  implicit CHECK_SAFE_ENV exit(2)
  %14 = LOAD_DOUBLE R0
  %16 = SUB_NUM %14, R1
  STORE_DOUBLE R5, %16
  STORE_TAG R5, tnumber
  %23 = ABS_NUM %16
  STORE_DOUBLE R4, %23
  STORE_TAG R4, tnumber
  CHECK_TAG R2, tnumber, bb_fallback_9
  %31 = LOAD_DOUBLE R2
  JUMP_CMP_NUM %23, %31, le, bb_bytecode_3, bb_8
bb_8:
  STORE_INT R3, 0i
  STORE_TAG R3, tboolean
  JUMP bb_bytecode_4
bb_bytecode_3:
  STORE_INT R3, 1i
  STORE_TAG R3, tboolean
  JUMP bb_bytecode_4
bb_bytecode_4:
  INTERRUPT 11u
  RETURN R3, 1i
)"
    );
}

TEST_CASE("ExplicitUpvalueAndLocalTypes")
{
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};
    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};
    ScopedFastFlag luauCodegenGcoDse{FFlag::LuauCodegenGcoDse, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %3 = FLOAT_TO_NUM %2
  STORE_DOUBLE R4, %3
  STORE_TAG R4, tnumber
  %8 = LOAD_FLOAT R0, 4i
  %9 = FLOAT_TO_NUM %8
  %18 = ADD_NUM %3, %9
  STORE_DOUBLE R3, %18
  STORE_TAG R3, tnumber
  %21 = GET_UPVALUE U0
  STORE_TVALUE R5, %21
  CHECK_TAG R5, tvector, exit(6)
  %25 = EXTRACT_VEC %21, 0i
  %26 = FLOAT_TO_NUM %25
  %35 = ADD_NUM %18, %26
  %42 = EXTRACT_VEC %21, 1i
  %43 = FLOAT_TO_NUM %42
  %52 = ADD_NUM %35, %43
  STORE_DOUBLE R1, %52
  STORE_TAG R1, tnumber
  INTERRUPT 13u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("DuplicateArrayLoads")
{
    ScopedFastFlag luauCodegenSetBlockEntryState{FFlag::LuauCodegenSetBlockEntryState, true};

    // TODO: opportunity - if we can track heap space version, we can remove duplicate loads
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(n: number, t: {number}, u: {number})
    return t[n] * t[n] + u[n] * u[n]
end
)"
               ),
        R"(
; function foo($arg0, $arg1, $arg2) line 2
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  CHECK_TAG R1, ttable, exit(entry)
  CHECK_TAG R2, ttable, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %12 = LOAD_POINTER R1
  %13 = LOAD_DOUBLE R0
  %14 = TRY_NUM_TO_INDEX %13, bb_fallback_3
  %15 = SUB_INT %14, 1i
  CHECK_ARRAY_SIZE %12, %15, bb_fallback_3
  CHECK_NO_METATABLE %12, bb_fallback_3
  %18 = GET_ARR_ADDR %12, %15
  %19 = LOAD_TVALUE %18
  STORE_TVALUE R5, %19
  JUMP bb_linear_17
bb_linear_17:
  %124 = LOAD_TVALUE %18
  STORE_TVALUE R6, %124
  CHECK_TAG R5, tnumber, bb_fallback_7
  CHECK_TAG R6, tnumber, bb_fallback_7
  %131 = LOAD_DOUBLE R5
  %133 = MUL_NUM %131, R6
  STORE_DOUBLE R4, %133
  STORE_TAG R4, tnumber
  %137 = LOAD_POINTER R2
  CHECK_ARRAY_SIZE %137, %15, bb_fallback_9
  CHECK_NO_METATABLE %137, bb_fallback_9
  %143 = GET_ARR_ADDR %137, %15
  %144 = LOAD_TVALUE %143
  STORE_TVALUE R6, %144
  %154 = LOAD_TVALUE %143
  STORE_TVALUE R7, %154
  CHECK_TAG R6, tnumber, bb_fallback_13
  CHECK_TAG R7, tnumber, bb_fallback_13
  %161 = LOAD_DOUBLE R6
  %163 = MUL_NUM %161, R7
  %173 = ADD_NUM %133, %163
  STORE_DOUBLE R3, %173
  STORE_TAG R3, tnumber
  INTERRUPT 7u
  RETURN R3, 1i
bb_4:
  %29 = LOAD_POINTER R1
  %30 = LOAD_DOUBLE R0
  %31 = TRY_NUM_TO_INDEX %30, bb_fallback_5
  %32 = SUB_INT %31, 1i
  CHECK_ARRAY_SIZE %29, %32, bb_fallback_5
  CHECK_NO_METATABLE %29, bb_fallback_5
  %35 = GET_ARR_ADDR %29, %32
  %36 = LOAD_TVALUE %35
  STORE_TVALUE R6, %36
  JUMP bb_6
bb_6:
  CHECK_TAG R5, tnumber, bb_fallback_7
  CHECK_TAG R6, tnumber, bb_fallback_7
  %46 = LOAD_DOUBLE R5
  %48 = MUL_NUM %46, R6
  STORE_DOUBLE R4, %48
  STORE_TAG R4, tnumber
  JUMP bb_8
bb_8:
  %59 = LOAD_POINTER R2
  %60 = LOAD_DOUBLE R0
  %61 = TRY_NUM_TO_INDEX %60, bb_fallback_9
  %62 = SUB_INT %61, 1i
  CHECK_ARRAY_SIZE %59, %62, bb_fallback_9
  CHECK_NO_METATABLE %59, bb_fallback_9
  %65 = GET_ARR_ADDR %59, %62
  %66 = LOAD_TVALUE %65
  STORE_TVALUE R6, %66
  JUMP bb_10
bb_10:
  %76 = LOAD_POINTER R2
  %77 = LOAD_DOUBLE R0
  %78 = TRY_NUM_TO_INDEX %77, bb_fallback_11
  %79 = SUB_INT %78, 1i
  CHECK_ARRAY_SIZE %76, %79, bb_fallback_11
  CHECK_NO_METATABLE %76, bb_fallback_11
  %82 = GET_ARR_ADDR %76, %79
  %83 = LOAD_TVALUE %82
  STORE_TVALUE R7, %83
  JUMP bb_12
bb_12:
  CHECK_TAG R6, tnumber, bb_fallback_13
  CHECK_TAG R7, tnumber, bb_fallback_13
  %93 = LOAD_DOUBLE R6
  %95 = MUL_NUM %93, R7
  STORE_DOUBLE R5, %95
  STORE_TAG R5, tnumber
  JUMP bb_14
bb_14:
  CHECK_TAG R4, tnumber, bb_fallback_15
  CHECK_TAG R5, tnumber, bb_fallback_15
  %106 = LOAD_DOUBLE R4
  %108 = ADD_NUM %106, R5
  STORE_DOUBLE R3, %108
  STORE_TAG R3, tnumber
  JUMP bb_16
bb_16:
  INTERRUPT 7u
  RETURN R3, 1i
)"
    );
}

#if LUA_VECTOR_SIZE == 3
TEST_CASE("FastcallTypeInferThroughLocal")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

    // TODO: opportunity - bb_3 has only one predecessor, but doesn't retain any info from it
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
  implicit CHECK_SAFE_ENV exit(0)
  STORE_DOUBLE R4, 2
  STORE_TAG R4, tnumber
  STORE_DOUBLE R5, 3
  STORE_TAG R5, tnumber
  CHECK_TAG R0, tnumber, exit(4)
  %11 = LOAD_DOUBLE R0
  %14 = NUM_TO_FLOAT %11
  STORE_VECTOR R2, %14, 2, 3
  STORE_TAG R2, tvector
  JUMP_IF_FALSY R1, bb_bytecode_1, bb_3
bb_3:
  CHECK_TAG R2, tvector, exit(9)
  %22 = LOAD_FLOAT R2, 0i
  %23 = FLOAT_TO_NUM %22
  %28 = LOAD_FLOAT R2, 4i
  %29 = FLOAT_TO_NUM %28
  %38 = ADD_NUM %23, %29
  STORE_DOUBLE R3, %38
  STORE_TAG R3, tnumber
  INTERRUPT 14u
  RETURN R3, 1i
bb_bytecode_1:
  CHECK_TAG R2, tvector, exit(15)
  %45 = LOAD_FLOAT R2, 8i
  %46 = FLOAT_TO_NUM %45
  STORE_DOUBLE R3, %46
  STORE_TAG R3, tnumber
  INTERRUPT 17u
  RETURN R3, 1i
)"
    );
}

TEST_CASE("FastcallTypeInferThroughUpvalue")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};
    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};
    ScopedFastFlag luauCodegenGcoDse{FFlag::LuauCodegenGcoDse, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  implicit CHECK_SAFE_ENV exit(0)
  STORE_DOUBLE R4, 2
  STORE_TAG R4, tnumber
  STORE_DOUBLE R5, 3
  STORE_TAG R5, tnumber
  CHECK_TAG R0, tnumber, exit(4)
  %11 = LOAD_DOUBLE R0
  %14 = NUM_TO_FLOAT %11
  STORE_VECTOR R2, %14, 2, 3
  STORE_TAG R2, tvector
  %19 = LOAD_TVALUE R2, 0i, tvector
  SET_UPVALUE U0, %19, tvector
  JUMP_IF_FALSY R1, bb_bytecode_1, bb_3
bb_3:
  %22 = GET_UPVALUE U0
  STORE_TVALUE R4, %22
  CHECK_TAG R4, tvector, exit(11)
  %26 = EXTRACT_VEC %22, 0i
  %27 = FLOAT_TO_NUM %26
  %34 = EXTRACT_VEC %22, 1i
  %35 = FLOAT_TO_NUM %34
  %44 = ADD_NUM %27, %35
  STORE_DOUBLE R2, %44
  STORE_TAG R2, tnumber
  INTERRUPT 17u
  RETURN R2, 1i
bb_bytecode_1:
  %49 = GET_UPVALUE U0
  STORE_TVALUE R3, %49
  CHECK_TAG R3, tvector, exit(19)
  %53 = EXTRACT_VEC %49, 2i
  %54 = FLOAT_TO_NUM %53
  STORE_DOUBLE R2, %54
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
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};
    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  implicit CHECK_SAFE_ENV exit(0)
  STORE_DOUBLE R3, 1
  STORE_TAG R3, tnumber
  STORE_DOUBLE R5, 3
  STORE_TAG R5, tnumber
  CHECK_TAG R1, tnumber, exit(4)
  %12 = LOAD_DOUBLE R1
  %15 = NUM_TO_FLOAT %12
  STORE_VECTOR R2, 1, %15, 3
  STORE_TAG R2, tvector
  %24 = FLOAT_TO_NUM %15
  %39 = ADD_NUM %24, 3
  STORE_DOUBLE R2, %39
  STORE_TAG R2, tnumber
  INTERRUPT 14u
  RETURN R2, 1i
)"
    );
}
#endif

TEST_CASE("InlineFunctionType")
{
    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %9 = FLOAT_TO_NUM %8
  %18 = MUL_NUM %9, R1
  STORE_DOUBLE R2, %18
  STORE_TAG R2, tnumber
  INTERRUPT 3u
  RETURN R2, 1i
; function getsum($arg0) line 6
; R0: vector from 0 to 3
; R0: vector from 3 to 6
bb_bytecode_0:
  CHECK_TAG R0, tvector, exit(0)
  %2 = LOAD_FLOAT R0, 4i
  %3 = FLOAT_TO_NUM %2
  %9 = MUL_NUM %3, 3
  %21 = MUL_NUM %3, 5
  %30 = ADD_NUM %9, %21
  STORE_DOUBLE R1, %30
  STORE_TAG R1, tnumber
  INTERRUPT 7u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("ResolveTablePathTypes")
{
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %34 = FLOAT_TO_NUM %33
  STORE_DOUBLE R3, %34
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
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};
    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};

    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %39 = FLOAT_TO_NUM %37
  %41 = MUL_NUM %39, 0.7070000171661377
  %42 = LOAD_FLOAT R2, 4i
  %44 = FLOAT_TO_NUM %42
  %46 = MUL_NUM %44, 0
  %47 = LOAD_FLOAT R2, 8i
  %49 = FLOAT_TO_NUM %47
  %51 = MUL_NUM %49, 0.7070000171661377
  %52 = ADD_NUM %41, %46
  %53 = ADD_NUM %52, %51
  STORE_DOUBLE R2, %53
  STORE_TAG R2, tnumber
  ADJUST_STACK_TO_REG R2, 1i
  INTERRUPT 7u
  RETURN R2, -1i
)"
    );
}

TEST_CASE("ImmediateTypeAnnotationHelp")
{
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

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
  %19 = LOAD_TVALUE R3, 0i, tvector
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
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  implicit CHECK_SAFE_ENV exit(0)
  STORE_DOUBLE R1, 0
  STORE_TAG R1, tnumber
  GET_CACHED_IMPORT R2, K1 (nil), 1073741824u ('ipairs'), 2u
  %8 = LOAD_TVALUE R0, 0i, ttable
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
  %39 = FLOAT_TO_NUM %38
  STORE_DOUBLE R7, %39
  STORE_TAG R7, tnumber
  CHECK_TAG R1, tnumber, exit(10)
  %46 = LOAD_DOUBLE R1
  %48 = ADD_NUM %46, %39
  STORE_DOUBLE R1, %48
  JUMP bb_bytecode_3
bb_bytecode_3:
  INTERRUPT 11u
  CHECK_TAG R2, tnil, bb_fallback_10
  %54 = LOAD_POINTER R3
  %55 = LOAD_INT R4
  %56 = GET_ARR_ADDR %54, %55
  CHECK_ARRAY_SIZE %54, %55, bb_9
  %58 = LOAD_TAG %56
  JUMP_EQ_TAG %58, tnil, bb_9, bb_11
bb_11:
  %60 = ADD_INT %55, 1i
  STORE_INT R4, %60
  %62 = INT_TO_NUM %60
  STORE_DOUBLE R5, %62
  STORE_TAG R5, tnumber
  %65 = LOAD_TVALUE %56
  STORE_TVALUE R6, %65
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

    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %9 = FLOAT_TO_NUM %8
  %16 = BUFFER_READF32 %6, 4i, tuserdata
  %17 = FLOAT_TO_NUM %16
  %26 = ADD_NUM %9, %17
  STORE_DOUBLE R1, %26
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

    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

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
  %14 = LOAD_TVALUE R2, 0i, tvector
  %15 = LOAD_TVALUE R3, 0i, tvector
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

    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %6 = LOAD_TVALUE R1, 0i, tuserdata
  STORE_TVALUE R4, %6
  %10 = LOAD_POINTER R0
  CHECK_USERDATA_TAG %10, 12i, exit(1)
  %14 = LOAD_POINTER R4
  CHECK_USERDATA_TAG %14, 12i, exit(1)
  %16 = BUFFER_READF32 %10, 0i, tuserdata
  %17 = BUFFER_READF32 %14, 0i, tuserdata
  %18 = FLOAT_TO_NUM %16
  %19 = FLOAT_TO_NUM %17
  %20 = MUL_NUM %18, %19
  %21 = BUFFER_READF32 %10, 4i, tuserdata
  %22 = BUFFER_READF32 %14, 4i, tuserdata
  %23 = FLOAT_TO_NUM %21
  %24 = FLOAT_TO_NUM %22
  %25 = MUL_NUM %23, %24
  %26 = ADD_NUM %20, %25
  STORE_DOUBLE R2, %26
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

    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

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
  %6 = LOAD_TVALUE R1, 0i, tuserdata
  STORE_TVALUE R4, %6
  %10 = LOAD_POINTER R0
  CHECK_USERDATA_TAG %10, 12i, exit(1)
  %14 = LOAD_POINTER R4
  CHECK_USERDATA_TAG %14, 12i, exit(1)
  %16 = BUFFER_READF32 %10, 0i, tuserdata
  %17 = BUFFER_READF32 %14, 0i, tuserdata
  %18 = FLOAT_TO_NUM %16
  %19 = FLOAT_TO_NUM %17
  %20 = MIN_NUM %18, %19
  %21 = BUFFER_READF32 %10, 4i, tuserdata
  %22 = BUFFER_READF32 %14, 4i, tuserdata
  %23 = FLOAT_TO_NUM %21
  %24 = FLOAT_TO_NUM %22
  %25 = MIN_NUM %23, %24
  %26 = NUM_TO_FLOAT %20
  %27 = NUM_TO_FLOAT %25
  CHECK_GC
  %29 = NEW_USERDATA 8i, 12i
  BUFFER_WRITEF32 %29, 0i, %26, tuserdata
  BUFFER_WRITEF32 %29, 4i, %27, tuserdata
  STORE_POINTER R2, %29
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
    ScopedFastFlag luauCodegenBufferLoadProp{FFlag::LuauCodegenBufferLoadProp2, true};
    ScopedFastFlag luauCodegenGcoDse{FFlag::LuauCodegenGcoDse, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    // TODO: opportunity - with float load/store separated from float/double conversion, reads at the end can be removed
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
  %14 = FLOAT_TO_NUM %12
  %15 = FLOAT_TO_NUM %13
  %16 = UNM_NUM %14
  %17 = UNM_NUM %15
  %18 = NUM_TO_FLOAT %16
  %19 = NUM_TO_FLOAT %17
  CHECK_GC
  %21 = NEW_USERDATA 8i, 12i
  BUFFER_WRITEF32 %21, 0i, %18, tuserdata
  BUFFER_WRITEF32 %21, 4i, %19, tuserdata
  STORE_POINTER R4, %21
  STORE_TAG R4, tuserdata
  %30 = LOAD_POINTER R0
  CHECK_USERDATA_TAG %30, 12i, exit(1)
  %32 = LOAD_POINTER R1
  CHECK_USERDATA_TAG %32, 12i, exit(1)
  %34 = BUFFER_READF32 %30, 0i, tuserdata
  %35 = BUFFER_READF32 %32, 0i, tuserdata
  %36 = FLOAT_TO_NUM %34
  %37 = FLOAT_TO_NUM %35
  %38 = MUL_NUM %36, %37
  %39 = BUFFER_READF32 %30, 4i, tuserdata
  %40 = BUFFER_READF32 %32, 4i, tuserdata
  %41 = FLOAT_TO_NUM %39
  %42 = FLOAT_TO_NUM %40
  %43 = MUL_NUM %41, %42
  %44 = NUM_TO_FLOAT %38
  %45 = NUM_TO_FLOAT %43
  %62 = FLOAT_TO_NUM %18
  %63 = FLOAT_TO_NUM %44
  %64 = ADD_NUM %62, %63
  %67 = FLOAT_TO_NUM %19
  %68 = FLOAT_TO_NUM %45
  %69 = ADD_NUM %67, %68
  %70 = NUM_TO_FLOAT %64
  %71 = NUM_TO_FLOAT %69
  %73 = NEW_USERDATA 8i, 12i
  BUFFER_WRITEF32 %73, 0i, %70, tuserdata
  BUFFER_WRITEF32 %73, 4i, %71, tuserdata
  STORE_POINTER R3, %73
  STORE_TAG R3, tuserdata
  INTERRUPT 3u
  RETURN R3, 1i
)"
    );
}

TEST_CASE("CustomUserdataMapping")
{
    ScopedFastFlag luauCompileUnusedUdataFix{FFlag::LuauCompileUnusedUdataFix, true};
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(a: mat3)
    print(a, vec2.create(0, 0))
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
  implicit CHECK_SAFE_ENV exit(0)
  GET_CACHED_IMPORT R1, K1 (nil), 1073741824u ('print'), 1u
  %6 = LOAD_TVALUE R0, 0i, tuserdata
  STORE_TVALUE R2, %6
  GET_CACHED_IMPORT R3, K4 (nil), 2149583872u ('vec2'.'create'), 4u
  STORE_DOUBLE R4, 0
  STORE_TAG R4, tnumber
  STORE_DOUBLE R5, 0
  STORE_TAG R5, tnumber
  INTERRUPT 7u
  SET_SAVEDPC 8u
  CALL R3, 2i, -1i
  INTERRUPT 8u
  SET_SAVEDPC 9u
  CALL R1, -1i, 0i
  INTERRUPT 9u
  RETURN R0, 0i
)"
    );
}

TEST_CASE("LibraryFieldTypesAndConstants")
{
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

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
  %11 = LOAD_TVALUE R0, 0i, tvector
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
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

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
  %11 = LOAD_TVALUE R0, 0i, tvector
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
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};

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
  implicit CHECK_SAFE_ENV exit(0)
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

TEST_CASE("VectorLoadReuse")
{
    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function shuffle(v: vector)
    return v.x * v.x + v.y * v.y
end
)"),
        R"(
; function shuffle($arg0) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %6 = LOAD_FLOAT R0, 0i
  %7 = FLOAT_TO_NUM %6
  %22 = MUL_NUM %7, %7
  %27 = LOAD_FLOAT R0, 4i
  %28 = FLOAT_TO_NUM %27
  %43 = MUL_NUM %28, %28
  %52 = ADD_NUM %22, %43
  STORE_DOUBLE R1, %52
  STORE_TAG R1, tnumber
  INTERRUPT 11u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("VectorShuffle1")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

    // TODO: opportunity - if we introduce a separate vector shuffle instruction, this can be done in a single shuffle (+/- load and store)
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function shuffle(v: vector)
    return vector.create(v.z, v.x, v.y)
end
)"),
        R"(
; function shuffle($arg0) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  implicit CHECK_SAFE_ENV exit(0)
  %6 = LOAD_FLOAT R0, 8i
  %12 = LOAD_FLOAT R0, 0i
  %18 = LOAD_FLOAT R0, 4i
  STORE_VECTOR R1, %6, %12, %18
  STORE_TAG R1, tvector
  INTERRUPT 10u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("VectorShuffle2")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};
    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function crossshuffle(v: vector, t: vector)
    local tmp1 = vector.create(v.x, v.x, v.z)
    local tmp2 = vector.create(t.y, t.z, t.x)
    return vector.create(tmp1.z, tmp2.x, tmp1.y)
end
)"),
        R"(
; function crossshuffle($arg0, $arg1) line 2
bb_0:
  CHECK_TAG R0, tvector, exit(entry)
  CHECK_TAG R1, tvector, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  implicit CHECK_SAFE_ENV exit(0)
  %8 = LOAD_FLOAT R0, 0i
  %20 = LOAD_FLOAT R0, 8i
  %41 = LOAD_FLOAT R1, 4i
  STORE_VECTOR R4, %20, %41, %8, tvector
  INTERRUPT 30u
  RETURN R4, 1i
)"
    );
}

TEST_CASE("VectorShuffleFromComposite1")
{
    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};
    ScopedFastFlag luauCodegenBufferLoadProp{FFlag::LuauCodegenBufferLoadProp2, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function test(v: vertex)
    return v.normal.X * v.normal.X + v.normal.Y * v.normal.Y
end
)"),
        R"(
; function test($arg0) line 2
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %6 = LOAD_POINTER R0
  CHECK_USERDATA_TAG %6, 13i, exit(0)
  %8 = BUFFER_READF32 %6, 12i, tuserdata
  %9 = BUFFER_READF32 %6, 16i, tuserdata
  %16 = FLOAT_TO_NUM %8
  %40 = MUL_NUM %16, %16
  %55 = FLOAT_TO_NUM %9
  %79 = MUL_NUM %55, %55
  %88 = ADD_NUM %40, %79
  STORE_DOUBLE R1, %88
  STORE_TAG R1, tnumber
  INTERRUPT 19u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("VectorShuffleFromComposite2")
{
    // This test requires runtime component to be present
    if (!Luau::CodeGen::isSupported())
        return;

    ScopedFastFlag luauCodegenBufferLoadProp{FFlag::LuauCodegenBufferLoadProp2, true};
    ScopedFastFlag luauCodegenGcoDse{FFlag::LuauCodegenGcoDse, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function test(v: vertex)
    return v.uv.X * v.uv.Y
end
)"),
        R"(
; function test($arg0) line 2
bb_0:
  CHECK_TAG R0, tuserdata, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %6 = LOAD_POINTER R0
  CHECK_USERDATA_TAG %6, 13i, exit(0)
  %8 = BUFFER_READF32 %6, 24i, tuserdata
  %9 = BUFFER_READF32 %6, 28i, tuserdata
  CHECK_GC
  %21 = FLOAT_TO_NUM %8
  %41 = FLOAT_TO_NUM %9
  %50 = MUL_NUM %21, %41
  STORE_DOUBLE R1, %50
  STORE_TAG R1, tnumber
  INTERRUPT 9u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("VectorLoadStoreOnlySamePrecision")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};
    ScopedFastFlag luauCodegenFloatLoadStoreProp{FFlag::LuauCodegenFloatLoadStoreProp, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function test(x: number, y: number)
    local vec = vector.create(x, y, 0)
    return vec.X + vec.Y + vec.Z
end
)"),
        R"(
; function test($arg0, $arg1) line 2
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  implicit CHECK_SAFE_ENV exit(0)
  %15 = LOAD_DOUBLE R0
  %16 = LOAD_DOUBLE R1
  %18 = NUM_TO_FLOAT %15
  %19 = NUM_TO_FLOAT %16
  %26 = FLOAT_TO_NUM %18
  %32 = FLOAT_TO_NUM %19
  %41 = ADD_NUM %26, %32
  %56 = ADD_NUM %41, 0
  STORE_DOUBLE R3, %56
  STORE_TAG R3, tnumber
  INTERRUPT 16u
  RETURN R3, 1i
)"
    );
}

TEST_CASE("Bit32NoDoubleTemporariesAdd")
{
    ScopedFastFlag luauCodegenIntegerAddSub{FFlag::LuauCodegenIntegerAddSub, true};
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(a: number, b: number)
    local a = bit32.band(bit32.bor(a, 0) + bit32.bor(b, 0), 0xffff)
    local b = bit32.band(bit32.bor(a, 0) + 127, 0xffff)
    local c = bit32.band(254 + bit32.bor(a, 1), 0xffff)
    return a, b, c
end
)"),
        R"(
; function foo($arg0, $arg1) line 2
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  implicit CHECK_SAFE_ENV exit(0)
  %9 = LOAD_DOUBLE R0
  %10 = NUM_TO_UINT %9
  %19 = LOAD_DOUBLE R1
  %20 = NUM_TO_UINT %19
  %39 = ADD_INT %10, %20
  %41 = BITAND_UINT %39, 65535i
  %42 = UINT_TO_NUM %41
  STORE_DOUBLE R2, %42
  STORE_TAG R2, tnumber
  %65 = ADD_INT %41, 127i
  %67 = BITAND_UINT %65, 65535i
  %68 = UINT_TO_NUM %67
  STORE_SPLIT_TVALUE R3, tnumber, %68
  %77 = BITOR_UINT %41, 1i
  %91 = ADD_INT %77, 254i
  %93 = BITAND_UINT %91, 65535i
  %94 = UINT_TO_NUM %93
  STORE_SPLIT_TVALUE R4, tnumber, %94
  INTERRUPT 49u
  RETURN R2, 3i
)"
    );
}

TEST_CASE("Bit32HasToUseDoubleTemporariesAdd")
{
    ScopedFastFlag luauCodegenIntegerAddSub{FFlag::LuauCodegenIntegerAddSub, true};
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(a: number, b: number)
    local a = bit32.band(bit32.bor(a, 0) + 0.75, 0xffff)
    local b = bit32.band(bit32.bor(a, 0) + 1e30, 0xffff)
    local c = bit32.band(1e30 + bit32.bor(a, 1), 0xffff)
    return a, b, c
end
)"),
        R"(
; function foo($arg0, $arg1) line 2
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  implicit CHECK_SAFE_ENV exit(0)
  %9 = LOAD_DOUBLE R0
  %10 = NUM_TO_UINT %9
  %13 = UINT_TO_NUM %10
  %19 = ADD_NUM %13, 0.75
  %26 = NUM_TO_UINT %19
  %28 = BITAND_UINT %26, 65535i
  %29 = UINT_TO_NUM %28
  STORE_DOUBLE R2, %29
  STORE_TAG R2, tnumber
  %45 = ADD_NUM %29, 1e+30
  %52 = NUM_TO_UINT %45
  %54 = BITAND_UINT %52, 65535i
  %55 = UINT_TO_NUM %54
  STORE_SPLIT_TVALUE R3, tnumber, %55
  %64 = BITOR_UINT %28, 1i
  %65 = UINT_TO_NUM %64
  %71 = ADD_NUM %65, 1e+30
  %78 = NUM_TO_UINT %71
  %80 = BITAND_UINT %78, 65535i
  %81 = UINT_TO_NUM %80
  STORE_SPLIT_TVALUE R4, tnumber, %81
  INTERRUPT 42u
  RETURN R2, 3i
)"
    );
}

TEST_CASE("Bit32NoDoubleTemporariesSub")
{
    ScopedFastFlag luauCodegenIntegerAddSub{FFlag::LuauCodegenIntegerAddSub, true};
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(a: number, b: number)
    local a = bit32.band(bit32.bor(a, 0) - bit32.bor(b, 0), 0xffff)
    local b = bit32.band(bit32.bor(a, 0) - 127, 0xffff)
    local c = bit32.band(254 - bit32.bor(a, 1), 0xffff)
    return a, b, c
end
)"),
        R"(
; function foo($arg0, $arg1) line 2
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  implicit CHECK_SAFE_ENV exit(0)
  %9 = LOAD_DOUBLE R0
  %10 = NUM_TO_UINT %9
  %19 = LOAD_DOUBLE R1
  %20 = NUM_TO_UINT %19
  %39 = SUB_INT %10, %20
  %41 = BITAND_UINT %39, 65535i
  %42 = UINT_TO_NUM %41
  STORE_DOUBLE R2, %42
  STORE_TAG R2, tnumber
  %65 = SUB_INT %41, 127i
  %67 = BITAND_UINT %65, 65535i
  %68 = UINT_TO_NUM %67
  STORE_SPLIT_TVALUE R3, tnumber, %68
  %77 = BITOR_UINT %41, 1i
  %91 = SUB_INT 254i, %77
  %93 = BITAND_UINT %91, 65535i
  %94 = UINT_TO_NUM %93
  STORE_SPLIT_TVALUE R4, tnumber, %94
  INTERRUPT 49u
  RETURN R2, 3i
)"
    );
}

TEST_CASE("Bit32HasToUseDoubleTemporariesSub")
{
    ScopedFastFlag luauCodegenIntegerAddSub{FFlag::LuauCodegenIntegerAddSub, true};
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(a: number, b: number)
    local a = bit32.band(bit32.bor(a, 0) - 0.75, 0xffff)
    local b = bit32.band(bit32.bor(a, 0) - 1e30, 0xffff)
    local c = bit32.band(1e30 - bit32.bor(a, 1), 0xffff)
    return a, b, c
end
)"),
        R"(
; function foo($arg0, $arg1) line 2
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  implicit CHECK_SAFE_ENV exit(0)
  %9 = LOAD_DOUBLE R0
  %10 = NUM_TO_UINT %9
  %13 = UINT_TO_NUM %10
  %19 = SUB_NUM %13, 0.75
  %26 = NUM_TO_UINT %19
  %28 = BITAND_UINT %26, 65535i
  %29 = UINT_TO_NUM %28
  STORE_DOUBLE R2, %29
  STORE_TAG R2, tnumber
  %45 = SUB_NUM %29, 1e+30
  %52 = NUM_TO_UINT %45
  %54 = BITAND_UINT %52, 65535i
  %55 = UINT_TO_NUM %54
  STORE_SPLIT_TVALUE R3, tnumber, %55
  %64 = BITOR_UINT %28, 1i
  %65 = UINT_TO_NUM %64
  %71 = SUB_NUM 1e+30, %65
  %78 = NUM_TO_UINT %71
  %80 = BITAND_UINT %78, 65535i
  %81 = UINT_TO_NUM %80
  STORE_SPLIT_TVALUE R4, tnumber, %81
  INTERRUPT 42u
  RETURN R2, 3i
)"
    );
}

TEST_CASE("OptionalOr")
{
    ScopedFastFlag luauCodegenLinearAndOr{FFlag::LuauCodegenLinearAndOr, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(a, b)
    a = a or 0
    b = b or 0
    return a + b
end
-- when a function like 'foo' is inlined, those 'default values' collapse
local function bar()
    return foo(3, 4)
end
)"),
        R"(
; function foo($arg0, $arg1) line 2
bb_bytecode_0:
  %0 = LOAD_TVALUE R0
  %1 = LOAD_TVALUE K0 (0)
  %2 = SELECT_IF_TRUTHY %0, %0, %1
  STORE_TVALUE R0, %2
  %4 = LOAD_TVALUE R1
  %5 = LOAD_TVALUE K0 (0)
  %6 = SELECT_IF_TRUTHY %4, %4, %5
  STORE_TVALUE R1, %6
  CHECK_TAG R0, tnumber, bb_fallback_1
  CHECK_TAG R1, tnumber, bb_fallback_1
  %12 = LOAD_DOUBLE R0
  %14 = ADD_NUM %12, R1
  STORE_DOUBLE R2, %14
  STORE_TAG R2, tnumber
  JUMP bb_2
bb_2:
  INTERRUPT 3u
  RETURN R2, 1i
; function bar() line 8
bb_bytecode_0:
  STORE_DOUBLE R0, 7
  STORE_TAG R0, tnumber
  INTERRUPT 5u
  RETURN R0, 1i
)"
    );
}

TEST_CASE("LinearAndOr")
{
    ScopedFastFlag luauCodegenLinearAndOr{FFlag::LuauCodegenLinearAndOr, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(a, b)
    return a and b, a or b
end
local function bar()
    local a, b = foo(3, 4)
    return a, b
end
)"),
        R"(
; function foo($arg0, $arg1) line 2
bb_bytecode_0:
  %0 = LOAD_TVALUE R0
  %1 = LOAD_TVALUE R1
  %2 = SELECT_IF_TRUTHY %0, %1, %0
  STORE_TVALUE R2, %2
  %6 = SELECT_IF_TRUTHY %0, %0, %1
  STORE_TVALUE R3, %6
  INTERRUPT 2u
  RETURN R2, 2i
; function bar() line 5
bb_bytecode_0:
  STORE_DOUBLE R0, 4
  STORE_TAG R0, tnumber
  STORE_DOUBLE R1, 3
  STORE_TAG R1, tnumber
  INTERRUPT 2u
  RETURN R0, 2i
)"
    );
}

TEST_CASE("OldStyleConditional")
{
    ScopedFastFlag luauCodegenSetBlockEntryState{FFlag::LuauCodegenSetBlockEntryState, true};
    ScopedFastFlag luauCodegenLinearAndOr{FFlag::LuauCodegenLinearAndOr, true};
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

    // TODO: opportunity - this can be done in two SELECT_IF_TRUTHY, but we cannot match such complex sequences right now
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(a: boolean, b: number, c: number)
    local x = a and b or c
    return x + 1
end
)"),
        R"(
; function foo($arg0, $arg1, $arg2) line 2
bb_0:
  CHECK_TAG R0, tboolean, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  CHECK_TAG R2, tnumber, exit(entry)
  JUMP bb_4
bb_4:
  JUMP bb_bytecode_1
bb_bytecode_1:
  JUMP_IF_FALSY R0, bb_bytecode_2, bb_5
bb_5:
  %9 = LOAD_TVALUE R1, 0i, tnumber
  STORE_TVALUE R3, %9
  JUMP bb_bytecode_3
bb_bytecode_2:
  %12 = LOAD_TVALUE R2, 0i, tnumber
  STORE_TVALUE R3, %12
  JUMP bb_bytecode_3
bb_bytecode_3:
  CHECK_TAG R3, tnumber, exit(4)
  %17 = LOAD_DOUBLE R3
  %18 = ADD_NUM %17, 1
  STORE_DOUBLE R4, %18
  STORE_TAG R4, tnumber
  INTERRUPT 5u
  RETURN R4, 1i
)"
    );
}

TEST_CASE("NewStyleConditional")
{
    ScopedFastFlag luauCodegenLinearAndOr{FFlag::LuauCodegenLinearAndOr, true};
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenSetBlockEntryState{FFlag::LuauCodegenSetBlockEntryState, true};

    // TODO: opportunity - this can be done in one SELECT_IF_TRUTHY, but this is also hard to detect in current system
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function foo(a: boolean, b: number, c: number)
    local x = if a then b else c
    return x + 1
end
)"),
        R"(
; function foo($arg0, $arg1, $arg2) line 2
bb_0:
  CHECK_TAG R0, tboolean, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  CHECK_TAG R2, tnumber, exit(entry)
  JUMP bb_4
bb_4:
  JUMP bb_bytecode_1
bb_bytecode_1:
  JUMP_IF_FALSY R0, bb_bytecode_2, bb_5
bb_5:
  %9 = LOAD_TVALUE R1, 0i, tnumber
  STORE_TVALUE R3, %9
  JUMP bb_bytecode_3
bb_bytecode_2:
  %12 = LOAD_TVALUE R2, 0i, tnumber
  STORE_TVALUE R3, %12
  JUMP bb_bytecode_3
bb_bytecode_3:
  CHECK_TAG R3, tnumber, exit(4)
  %17 = LOAD_DOUBLE R3
  %18 = ADD_NUM %17, 1
  STORE_DOUBLE R4, %18
  STORE_TAG R4, tnumber
  INTERRUPT 5u
  RETURN R4, 1i
)"
    );
}

TEST_CASE("RecursiveRemoval1")
{
    ScopedFastFlag luauCodegenBetterSccRemoval{FFlag::LuauCodegenBetterSccRemoval, true};

    // Check that this compiles with no assertions
    CHECK(
        getCodegenAssembly(R"(
repeat
local _ = # {} < # {} < _ < _ < _ ^ _ ^ ""
until ""
)")
            .size() > 0
    );
}

TEST_CASE("RecursiveRemoval2")
{
    ScopedFastFlag luauCodegenBetterSccRemoval{FFlag::LuauCodegenBetterSccRemoval, true};

    // Check that this compiles with no assertions
    CHECK(
        getCodegenAssembly(R"(
local _

for l0={[1]=(_),},_ do
    _ += _
    for l0={[1]=(_),},_ do
        break
    end
end
)")
            .size() > 0
    );
}

TEST_CASE("RecursiveRemoval3")
{
    ScopedFastFlag luauCodegenBetterSccRemoval{FFlag::LuauCodegenBetterSccRemoval, true};

    // Check that this compiles with no assertions
    CHECK(
        getCodegenAssembly(R"(
while {_,} do
    local _
    repeat
        _ = "x",_ or {}
    until "x"
end
return l0
)")
            .size() > 0
    );
}

TEST_CASE("RecursiveRemoval4")
{
    ScopedFastFlag luauCodegenBetterSccRemoval{FFlag::LuauCodegenBetterSccRemoval, true};

    // Check that this compiles with no assertions
    CHECK(
        getCodegenAssembly(R"(
local _ = 5633,5633
while "" do
    _ += bit32.replace(_,function() end,0)
    for l0=_,{_,} do
        do
            _ += bit32.replace(_,nil,0)
            local _
        end
    end
end
)")
            .size() > 0
    );
}

TEST_CASE("FuzzTest1")
{
    // Check that this compiles with no assertions
    CHECK(
        getCodegenAssembly(R"(
local _ = 5633,5633
while _ do
    _ ^= _
    for l0=_,_,{[# {}]=_,} do
        repeat
        until _
    end
end
)")
            .size() > 0
    );
}

TEST_CASE("FuzzTest2")
{
    // Check that this compiles with no assertions
    CHECK(
        getCodegenAssembly(R"(
local _
while true ~= _ do
    _ = nil
end
_ = _,{},16711935 ~= _,{["" ~= _]=16711935,},_ ~= _
while {} ~= _ do
    _ = nil
end
)")
            .size() > 0
    );
}

TEST_CASE("FuzzTest3")
{
    ScopedFastFlag luauCodegenBetterSccRemoval{FFlag::LuauCodegenBetterSccRemoval, true};

    // Check that this compiles with no assertions
    CHECK(
        getCodegenAssembly(R"(
local function f(...)
    local _ = ``,_
    _ ..= _(_()(_(_ and (...),_ .. _),_(_)),_(_(_(_(_ .. _,- _),_(_)),_()(_)),_(_,_._)))
    _(_)
end
)")
            .size() > 0
    );
}

TEST_CASE("FuzzTest4")
{
    // Check that this compiles with no assertions
    CHECK(
        getCodegenAssembly(R"(
local _ = math.exp,_(),_
local _ = math._,_(_(_),_(_ and _),_(_(_),_,_,_()),`{nil}`),_
for l41=_,_ do
end
l0 -= _(0,_(# _,_(_),_(_(_),_(_),_,_()),`{nil}`))
)")
            .size() > 0
    );
}

TEST_CASE("FuzzTest5")
{
    // Check that this compiles with no assertions
    CHECK(
        getCodegenAssembly(R"(
local _ = 32768
while "" do
    n0 ..= 0
    for l0=`{-2013233152}`,65535 do
        local l0 = vector.create(`{-2013233152}`,-2147450880,_,_)
        _ = _,_ // 0,_ // _
    end
end
)")
            .size() > 0
    );
}

TEST_CASE("FuzzTest6")
{
    // Check that this compiles with no assertions
    CHECK(
        getCodegenAssembly(R"(
local l0:any = _(393216),# 0,n0
while vector.sign(_ and true) do
_ ..= nil
do end
for l0=_,_,vector.sign(_ + - _) do
for l0=_,_,vector.sign() do
do end
end
end
end
)")
            .size() > 0
    );
}

TEST_CASE("FuzzTest7")
{
    ScopedFastFlag luauCodegenLoadFloatSubstituteLast{FFlag::LuauCodegenLoadFloatSubstituteLast, true};

    // Check that this compiles with no assertions
    CHECK(
        getCodegenAssembly(R"(
for l0=0,32768 do end

local _ = vector.create(14941,14941,14848)

local function l0() end

_,l0,_,_,_G,_ = vector.clamp(_,_ / _,_),- - _
)")
            .size() > 0
    );
}

TEST_CASE("FuzzTest8")
{
    // Check that this compiles with no assertions
    CHECK(
        getCodegenAssembly(R"(
local function f(...)
    local _ = bit32.arshift
    local _ = (_),_(_(# # true,0),30),_(l158(true,_),0),_(_(l9,8258560),_)(_),_ + _,_
end
)")
            .size() > 0
    );
}

TEST_CASE("UpvalueAccessLoadStore1")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};
    ScopedFastFlag luauCodegenGcoDse{FFlag::LuauCodegenGcoDse, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local m = 1

local function foo(a: number, b: number)
    return m * a + m * b
end

function setm(x) m = x end
)"),
        R"(
; function foo($arg0, $arg1) line 4
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %6 = GET_UPVALUE U0
  STORE_TVALUE R4, %6
  CHECK_TAG R4, tnumber, exit(1)
  %12 = LOAD_DOUBLE R4
  %14 = MUL_NUM %12, R0
  %25 = MUL_NUM %12, R1
  %34 = ADD_NUM %14, %25
  STORE_DOUBLE R2, %34
  STORE_TAG R2, tnumber
  INTERRUPT 5u
  RETURN R2, 1i
; function setm($arg0) line 8
bb_bytecode_0:
  %0 = LOAD_TVALUE R0
  SET_UPVALUE U0, %0, undef
  INTERRUPT 1u
  RETURN R0, 0i
)"
    );
}

TEST_CASE("UpvalueAccessLoadStore2")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};

    // TODO: opportunity - if the value was just stored to VM register in parts, we can use those parts to store upvalue
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local m

local function foo(a: number, b: number)
    m = a - b
    m = m * a + m * b
    return m + a
end
)"),
        R"(
; function foo($arg0, $arg1) line 4
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  %10 = LOAD_DOUBLE R0
  %11 = LOAD_DOUBLE R1
  %12 = SUB_NUM %10, %11
  STORE_DOUBLE R2, %12
  STORE_TAG R2, tnumber
  %15 = LOAD_TVALUE R2, 0i, tnumber
  SET_UPVALUE U0, %15, tnumber
  %25 = MUL_NUM %12, %10
  %36 = MUL_NUM %12, %11
  %45 = ADD_NUM %25, %36
  STORE_DOUBLE R2, %45
  %48 = LOAD_TVALUE R2, 0i, tnumber
  SET_UPVALUE U0, %48, tnumber
  %58 = ADD_NUM %45, %10
  STORE_DOUBLE R2, %58
  INTERRUPT 10u
  RETURN R2, 1i
)"
    );
}

TEST_CASE("UpvalueAccessLoadStore3")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local m = 1

local function foo()
    local a = m
    m = a
    local b = m
    m = b
    return m + a + b
end

function setm(x, y) m = x end
)"),
        R"(
; function foo() line 4
bb_bytecode_0:
  %0 = GET_UPVALUE U0
  STORE_TVALUE R0, %0
  SET_UPVALUE U0, %0, undef
  STORE_TVALUE R1, %0
  SET_UPVALUE U0, %0, undef
  STORE_TVALUE R4, %0
  CHECK_TAG R4, tnumber, exit(5)
  %14 = LOAD_DOUBLE R4
  %16 = ADD_NUM %14, %14
  %25 = ADD_NUM %16, %14
  STORE_DOUBLE R2, %25
  STORE_TAG R2, tnumber
  INTERRUPT 7u
  RETURN R2, 1i
; function setm($arg0, $arg1) line 12
bb_bytecode_0:
  %0 = LOAD_TVALUE R0
  SET_UPVALUE U0, %0, undef
  INTERRUPT 1u
  RETURN R0, 0i
)"
    );
}

TEST_CASE("UpvalueAccessLoadStore4")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenUpvalueLoadProp{FFlag::LuauCodegenUpvalueLoadProp, true};
    ScopedFastFlag luauCodegenLoopStepDetectFix{FFlag::LuauCodegenLoopStepDetectFix, true};
    ScopedFastFlag luauCodegenSetBlockEntryState{FFlag::LuauCodegenSetBlockEntryState, true};
    ScopedFastFlag luauCodegenBufferLoadProp{FFlag::LuauCodegenBufferLoadProp2, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local arr: {number}

local function foo(a: number)
    for i = 1,#arr do
        arr[i] = arr[i] + arr[i] * a
    end
end

arr = {1, 2, 3, 4}
)"),
        R"(
; function foo($arg0) line 4
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  JUMP bb_4
bb_4:
  JUMP bb_bytecode_1
bb_bytecode_1:
  STORE_DOUBLE R3, 1
  STORE_TAG R3, tnumber
  %6 = GET_UPVALUE U0
  STORE_TVALUE R4, %6
  CHECK_TAG R4, ttable, exit(2)
  %10 = LOAD_POINTER R4
  CHECK_NO_METATABLE %10, bb_fallback_5
  %12 = TABLE_LEN %10
  %13 = INT_TO_NUM %12
  STORE_DOUBLE R1, %13
  STORE_TAG R1, tnumber
  JUMP bb_6
bb_6:
  STORE_DOUBLE R2, 1
  STORE_TAG R2, tnumber
  CHECK_TAG R1, tnumber, exit(4)
  CHECK_TAG R3, tnumber, exit(4)
  %26 = LOAD_DOUBLE R1
  JUMP_CMP_NUM R3, %26, not_le, bb_bytecode_3, bb_bytecode_2
bb_bytecode_2:
  INTERRUPT 5u
  %30 = GET_UPVALUE U0
  STORE_TVALUE R4, %30
  STORE_TVALUE R7, %30
  CHECK_TAG R7, ttable, exit(7)
  CHECK_TAG R3, tnumber, exit(7)
  %38 = LOAD_POINTER R7
  %39 = LOAD_DOUBLE R3
  %40 = TRY_NUM_TO_INDEX %39, bb_fallback_7
  %41 = SUB_INT %40, 1i
  CHECK_ARRAY_SIZE %38, %41, bb_fallback_7
  CHECK_NO_METATABLE %38, bb_fallback_7
  %44 = GET_ARR_ADDR %38, %41
  %45 = LOAD_TVALUE %44
  STORE_TVALUE R6, %45
  JUMP bb_linear_17
bb_linear_17:
  STORE_TVALUE R9, %30
  %135 = LOAD_TVALUE %44
  STORE_TVALUE R8, %135
  CHECK_TAG R8, tnumber, bb_fallback_11
  %140 = LOAD_DOUBLE R8
  %142 = MUL_NUM %140, R0
  STORE_DOUBLE R7, %142
  STORE_TAG R7, tnumber
  CHECK_TAG R6, tnumber, bb_fallback_13
  %150 = LOAD_DOUBLE R6
  %152 = ADD_NUM %150, %142
  STORE_DOUBLE R5, %152
  STORE_TAG R5, tnumber
  CHECK_NO_METATABLE %38, bb_fallback_15
  CHECK_READONLY %38, bb_fallback_15
  STORE_SPLIT_TVALUE %44, tnumber, %152
  %172 = LOAD_DOUBLE R1
  %174 = ADD_NUM %39, 1
  STORE_DOUBLE R3, %174
  JUMP_CMP_NUM %174, %172, le, bb_bytecode_2, bb_bytecode_3
bb_8:
  %51 = GET_UPVALUE U0
  STORE_TVALUE R9, %51
  CHECK_TAG R9, ttable, exit(9)
  CHECK_TAG R3, tnumber, exit(9)
  %57 = LOAD_POINTER R9
  %58 = LOAD_DOUBLE R3
  %59 = TRY_NUM_TO_INDEX %58, bb_fallback_9
  %60 = SUB_INT %59, 1i
  CHECK_ARRAY_SIZE %57, %60, bb_fallback_9
  CHECK_NO_METATABLE %57, bb_fallback_9
  %63 = GET_ARR_ADDR %57, %60
  %64 = LOAD_TVALUE %63
  STORE_TVALUE R8, %64
  JUMP bb_10
bb_10:
  CHECK_TAG R8, tnumber, bb_fallback_11
  %74 = LOAD_DOUBLE R8
  %76 = MUL_NUM %74, R0
  STORE_DOUBLE R7, %76
  STORE_TAG R7, tnumber
  JUMP bb_12
bb_12:
  CHECK_TAG R6, tnumber, bb_fallback_13
  CHECK_TAG R7, tnumber, bb_fallback_13
  %87 = LOAD_DOUBLE R6
  %89 = ADD_NUM %87, R7
  STORE_DOUBLE R5, %89
  STORE_TAG R5, tnumber
  JUMP bb_14
bb_14:
  CHECK_TAG R4, ttable, exit(12)
  CHECK_TAG R3, tnumber, exit(12)
  %100 = LOAD_POINTER R4
  %101 = LOAD_DOUBLE R3
  %102 = TRY_NUM_TO_INDEX %101, bb_fallback_15
  %103 = SUB_INT %102, 1i
  CHECK_ARRAY_SIZE %100, %103, bb_fallback_15
  CHECK_NO_METATABLE %100, bb_fallback_15
  CHECK_READONLY %100, bb_fallback_15
  %107 = GET_ARR_ADDR %100, %103
  %108 = LOAD_TVALUE R5
  STORE_TVALUE %107, %108
  BARRIER_TABLE_FORWARD %100, R5, undef
  JUMP bb_16
bb_16:
  %115 = LOAD_DOUBLE R1
  %116 = LOAD_DOUBLE R3
  %117 = ADD_NUM %116, 1
  STORE_DOUBLE R3, %117
  JUMP_CMP_NUM %117, %115, le, bb_bytecode_2, bb_bytecode_3
bb_bytecode_3:
  INTERRUPT 14u
  RETURN R0, 0i
)"
    );
}

TEST_CASE("BufferLoadStoreProp1")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenBufferLoadProp{FFlag::LuauCodegenBufferLoadProp2, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function test(b: buffer)
    return buffer.readf32(b, 0) * buffer.readf32(b, 0) + buffer.readf32(b, 4) * buffer.readf32(b, 4)
end
)"),
        R"(
; function test($arg0) line 2
bb_0:
  CHECK_TAG R0, tbuffer, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  implicit CHECK_SAFE_ENV exit(0)
  %7 = LOAD_POINTER R0
  CHECK_BUFFER_LEN %7, 4i, 4i, exit(2)
  %10 = BUFFER_READF32 %7, 0i
  %11 = FLOAT_TO_NUM %10
  %30 = MUL_NUM %11, %11
  %39 = BUFFER_READF32 %7, 4i
  %40 = FLOAT_TO_NUM %39
  %59 = MUL_NUM %40, %40
  %68 = ADD_NUM %30, %59
  STORE_DOUBLE R1, %68
  STORE_TAG R1, tnumber
  INTERRUPT 31u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("BufferLoadStoreProp2")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenChainLink{FFlag::LuauCodegenChainLink, true};
    ScopedFastFlag luauCodegenBufferLoadProp{FFlag::LuauCodegenBufferLoadProp2, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function test(b: buffer)
    buffer.writei8(b, 10, 32)
    assert(buffer.readi8(b, 10) == 32)

    buffer.writei8(b, 14, 4)
    buffer.writei8(b, 13, 3)
    buffer.writei8(b, 12, 2)
    buffer.writei8(b, 11, 1)

    return buffer.readi8(b, 11) + buffer.readi8(b, 12) + buffer.readi8(b, 14) + buffer.readi8(b, 13)
end
)"),
        R"(
; function test($arg0) line 2
bb_0:
  CHECK_TAG R0, tbuffer, exit(entry)
  JUMP bb_4
bb_4:
  JUMP bb_bytecode_1
bb_bytecode_1:
  implicit CHECK_SAFE_ENV exit(0)
  STORE_DOUBLE R3, 10
  STORE_TAG R3, tnumber
  STORE_DOUBLE R4, 32
  STORE_TAG R4, tnumber
  %15 = LOAD_POINTER R0
  CHECK_BUFFER_LEN %15, 14i, 1i, exit(4)
  BUFFER_WRITEI8 %15, 10i, 32i
  JUMP bb_bytecode_3
bb_bytecode_3:
  JUMP bb_8
bb_8:
  BUFFER_WRITEI8 %15, 14i, 4i
  BUFFER_WRITEI8 %15, 13i, 3i
  BUFFER_WRITEI8 %15, 12i, 2i
  BUFFER_WRITEI8 %15, 11i, 1i
  STORE_DOUBLE R1, 10
  STORE_TAG R1, tnumber
  INTERRUPT 86u
  RETURN R1, 1i
)"
    );
}

// When dealing with constants and buffer loads/store of the same size, all assertions disappear as conditions are true
TEST_CASE("BufferLoadStoreProp3")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenChainLink{FFlag::LuauCodegenChainLink, true};
    ScopedFastFlag luauCodegenNumToUintFoldRange{FFlag::LuauCodegenNumToUintFoldRange, true};
    ScopedFastFlag luauCodegenBufferLoadProp{FFlag::LuauCodegenBufferLoadProp2, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function storeloadpreserve(b: buffer)
    buffer.writeu32(b, 0, 0xffffffff)
    assert(buffer.readi32(b, 0) == -1)
    assert(buffer.readu32(b, 0) == 4294967295)

    buffer.writei32(b, 0, -1)
    assert(buffer.readi32(b, 0) == -1)
    assert(buffer.readu32(b, 0) == 4294967295)

    buffer.writei16(b, 0, 65535)
    assert(buffer.readi16(b, 0) == -1)
    assert(buffer.readu16(b, 0) == 65535)

    buffer.writeu16(b, 0, 65535)
    assert(buffer.readi16(b, 0) == -1)
    assert(buffer.readu16(b, 0) == 65535)

    buffer.writeu8(b, 0, 0xffffffff)
    assert(buffer.readi8(b, 0) == -1)
    assert(buffer.readu8(b, 0) == 255)

    buffer.writeu16(b, 0, 0xffffffff)
    assert(buffer.readi16(b, 0) == -1)
    assert(buffer.readu16(b, 0) == 65535)
end
)"),
        R"(
; function storeloadpreserve($arg0) line 2
bb_0:
  CHECK_TAG R0, tbuffer, exit(entry)
  JUMP bb_26
bb_26:
  JUMP bb_bytecode_1
bb_bytecode_1:
  implicit CHECK_SAFE_ENV exit(0)
  STORE_DOUBLE R3, 0
  STORE_TAG R3, tnumber
  STORE_DOUBLE R4, 4294967295
  STORE_TAG R4, tnumber
  %15 = LOAD_POINTER R0
  CHECK_BUFFER_LEN %15, 0i, 4i, exit(4)
  BUFFER_WRITEI32 %15, 0i, -1i
  STORE_TAG R2, tboolean
  STORE_INT R2, 1i
  JUMP bb_bytecode_3
bb_bytecode_3:
  JUMP bb_30
bb_30:
  JUMP bb_bytecode_5
bb_bytecode_5:
  JUMP bb_33
bb_33:
  BUFFER_WRITEI32 %15, 0i, -1i
  JUMP bb_bytecode_7
bb_bytecode_7:
  JUMP bb_37
bb_37:
  JUMP bb_bytecode_9
bb_bytecode_9:
  JUMP bb_40
bb_40:
  STORE_DOUBLE R3, 0
  STORE_DOUBLE R4, 65535
  CHECK_BUFFER_LEN %15, 0i, 2i, exit(80)
  BUFFER_WRITEI16 %15, 0i, 65535i
  JUMP bb_bytecode_11
bb_bytecode_11:
  JUMP bb_44
bb_44:
  JUMP bb_bytecode_13
bb_bytecode_13:
  JUMP bb_47
bb_47:
  BUFFER_WRITEI16 %15, 0i, 65535i
  JUMP bb_bytecode_15
bb_bytecode_15:
  JUMP bb_51
bb_51:
  JUMP bb_bytecode_17
bb_bytecode_17:
  JUMP bb_54
bb_54:
  STORE_DOUBLE R3, 0
  STORE_DOUBLE R4, 4294967295
  CHECK_BUFFER_LEN %15, 0i, 1i, exit(156)
  BUFFER_WRITEI8 %15, 0i, -1i
  JUMP bb_bytecode_19
bb_bytecode_19:
  JUMP bb_58
bb_58:
  JUMP bb_bytecode_21
bb_bytecode_21:
  JUMP bb_61
bb_61:
  BUFFER_WRITEI16 %15, 0i, -1i
  JUMP bb_bytecode_23
bb_bytecode_23:
  JUMP bb_65
bb_65:
  JUMP bb_bytecode_25
bb_bytecode_25:
  JUMP bb_68
bb_68:
  INTERRUPT 228u
  RETURN R0, 0i
)"
    );
}

// When dealing with unknown numbers, stores can be propagated to loads with proper zero/signed extension
TEST_CASE("BufferLoadStoreProp4")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenNumIntFolds{FFlag::LuauCodegenNumIntFolds2, true};
    ScopedFastFlag luauCodegenIntegerAddSub{FFlag::LuauCodegenIntegerAddSub, true};
    ScopedFastFlag luauCodegenBufferLoadProp{FFlag::LuauCodegenBufferLoadProp2, true};
    ScopedFastFlag luauCodegenSplitFloat{FFlag::LuauCodegenSplitFloat, true};

    // TODO: opportunity - CHECK_BUFFER_LEN with different access sizes can be merged
    CHECK_EQ(
        "\n" + getCodegenAssembly(R"(
local function test(b: buffer, n: number, f: number)
    buffer.writei8(b, 0, n)
    buffer.writef64(b, 100, buffer.readi8(b, 0))
    buffer.writei8(b, 108, buffer.readi8(b, 0))
    buffer.writeu8(b, 109, buffer.readi8(b, 0))

    buffer.writeu8(b, 2, n)
    buffer.writef64(b, 116, buffer.readu8(b, 2))
    buffer.writeu8(b, 124, buffer.readu8(b, 2))
    buffer.writei8(b, 125, buffer.readu8(b, 2))

    buffer.writei16(b, 4, n)
    buffer.writef64(b, 132, buffer.readi16(b, 4))
    buffer.writei16(b, 140, buffer.readi16(b, 4))
    buffer.writeu16(b, 142, buffer.readi16(b, 4))

    buffer.writeu16(b, 8, n)
    buffer.writef64(b, 148, buffer.readu16(b, 8))
    buffer.writeu16(b, 156, buffer.readu16(b, 8))
    buffer.writei16(b, 158, buffer.readu16(b, 8))

    buffer.writei32(b, 12, n)
    buffer.writef64(b, 164, buffer.readi32(b, 12))
    buffer.writei32(b, 172, buffer.readi32(b, 12))
    buffer.writeu32(b, 176, buffer.readi32(b, 12))

    buffer.writeu32(b, 20, n)
    buffer.writef64(b, 180, buffer.readu32(b, 20))
    buffer.writeu32(b, 188, buffer.readu32(b, 20))
    buffer.writei32(b, 192, buffer.readu32(b, 20))

    buffer.writef32(b, 28, f)
    buffer.writef64(b, 196, buffer.readf32(b, 28))
    buffer.writef32(b, 196, buffer.readf32(b, 28))

    buffer.writef64(b, 32, f)
    buffer.writef64(b, 204, buffer.readf64(b, 32))
    buffer.writef32(b, 204, buffer.readf64(b, 32))
end
)"),
        R"(
; function test($arg0, $arg1, $arg2) line 2
bb_0:
  CHECK_TAG R0, tbuffer, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  CHECK_TAG R2, tnumber, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  implicit CHECK_SAFE_ENV exit(0)
  STORE_DOUBLE R5, 0
  STORE_TAG R5, tnumber
  %17 = LOAD_POINTER R0
  CHECK_BUFFER_LEN %17, 125i, 1i, exit(3)
  %21 = LOAD_DOUBLE R1
  %22 = NUM_TO_UINT %21
  BUFFER_WRITEI8 %17, 0i, %22
  STORE_DOUBLE R5, 100
  %32 = SEXTI8_INT %22
  %33 = INT_TO_NUM %32
  STORE_DOUBLE R6, %33
  STORE_TAG R6, tnumber
  CHECK_BUFFER_LEN %17, 204i, 8i, exit(18)
  BUFFER_WRITEF64 %17, 100i, %33
  BUFFER_WRITEI8 %17, 108i, %22
  BUFFER_WRITEI8 %17, 109i, %22
  BUFFER_WRITEI8 %17, 2i, %22
  %125 = BITAND_UINT %22, 255i
  %126 = INT_TO_NUM %125
  BUFFER_WRITEF64 %17, 116i, %126
  BUFFER_WRITEI8 %17, 124i, %22
  STORE_DOUBLE R6, %126
  BUFFER_WRITEI8 %17, 125i, %22
  STORE_DOUBLE R5, 4
  CHECK_BUFFER_LEN %17, 158i, 2i, exit(103)
  BUFFER_WRITEI16 %17, 4i, %22
  %218 = SEXTI16_INT %22
  %219 = INT_TO_NUM %218
  BUFFER_WRITEF64 %17, 132i, %219
  BUFFER_WRITEI16 %17, 140i, %22
  BUFFER_WRITEI16 %17, 142i, %22
  BUFFER_WRITEI16 %17, 8i, %22
  %311 = BITAND_UINT %22, 65535i
  %312 = INT_TO_NUM %311
  BUFFER_WRITEF64 %17, 148i, %312
  BUFFER_WRITEI16 %17, 156i, %22
  STORE_DOUBLE R6, %312
  BUFFER_WRITEI16 %17, 158i, %22
  STORE_DOUBLE R5, 12
  CHECK_BUFFER_LEN %17, 204i, 4i, exit(203)
  BUFFER_WRITEI32 %17, 12i, %22
  %404 = TRUNCATE_UINT %22
  %405 = INT_TO_NUM %404
  BUFFER_WRITEF64 %17, 164i, %405
  BUFFER_WRITEI32 %17, 172i, %22
  BUFFER_WRITEI32 %17, 176i, %22
  BUFFER_WRITEI32 %17, 20i, %22
  %498 = UINT_TO_NUM %404
  BUFFER_WRITEF64 %17, 180i, %498
  BUFFER_WRITEI32 %17, 188i, %22
  BUFFER_WRITEI32 %17, 192i, %22
  %579 = LOAD_DOUBLE R2
  %580 = NUM_TO_FLOAT %579
  BUFFER_WRITEF32 %17, 28i, %580
  %591 = FLOAT_TO_NUM %580
  BUFFER_WRITEF64 %17, 196i, %591
  BUFFER_WRITEF32 %17, 196i, %580
  BUFFER_WRITEF64 %17, 32i, %579
  BUFFER_WRITEF64 %17, 204i, %579
  BUFFER_WRITEF32 %17, 204i, %580
  INTERRUPT 372u
  RETURN R0, 0i
)"
    );
}

TEST_CASE("LoopStepDetection1")
{
    ScopedFastFlag luauCodegenLoopStepDetectFix{FFlag::LuauCodegenLoopStepDetectFix, true};
    ScopedFastFlag luauCodegenHydrateLoadWithTag{FFlag::LuauCodegenHydrateLoadWithTag, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(n: number)
    local s = 0
    for i = 1,n do
        s += i
    end
    return s
end
)"
               ),
        R"(
; function foo($arg0) line 2
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  JUMP bb_4
bb_4:
  JUMP bb_bytecode_1
bb_bytecode_1:
  STORE_DOUBLE R1, 0
  STORE_TAG R1, tnumber
  STORE_DOUBLE R4, 1
  STORE_TAG R4, tnumber
  %8 = LOAD_TVALUE R0, 0i, tnumber
  STORE_TVALUE R2, %8
  STORE_DOUBLE R3, 1
  STORE_TAG R3, tnumber
  %16 = LOAD_DOUBLE R2
  JUMP_CMP_NUM 1, %16, not_le, bb_bytecode_3, bb_bytecode_2
bb_bytecode_2:
  INTERRUPT 5u
  CHECK_TAG R1, tnumber, exit(5)
  CHECK_TAG R4, tnumber, exit(5)
  %24 = LOAD_DOUBLE R1
  %25 = LOAD_DOUBLE R4
  %26 = ADD_NUM %24, %25
  STORE_DOUBLE R1, %26
  %28 = LOAD_DOUBLE R2
  %30 = ADD_NUM %25, 1
  STORE_DOUBLE R4, %30
  JUMP_CMP_NUM %30, %28, le, bb_bytecode_2, bb_bytecode_3
bb_bytecode_3:
  INTERRUPT 7u
  RETURN R1, 1i
)"
    );
}

TEST_CASE("LoopStepDetection2")
{
    ScopedFastFlag luauCodegenLoopStepDetectFix{FFlag::LuauCodegenLoopStepDetectFix, true};
    ScopedFastFlag luauCodegenSetBlockEntryState{FFlag::LuauCodegenSetBlockEntryState, true};

    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(n: number, t: {number})
    local s = 0
    for i = 1,#t do
        s += t[i]
    end
    return s
end
)"
               ),
        R"(
; function foo($arg0, $arg1) line 2
bb_0:
  CHECK_TAG R0, tnumber, exit(entry)
  CHECK_TAG R1, ttable, exit(entry)
  JUMP bb_4
bb_4:
  JUMP bb_bytecode_1
bb_bytecode_1:
  STORE_DOUBLE R2, 0
  STORE_TAG R2, tnumber
  STORE_DOUBLE R5, 1
  STORE_TAG R5, tnumber
  %12 = LOAD_POINTER R1
  CHECK_NO_METATABLE %12, bb_fallback_5
  %14 = TABLE_LEN %12
  %15 = INT_TO_NUM %14
  STORE_DOUBLE R3, %15
  STORE_TAG R3, tnumber
  JUMP bb_6
bb_6:
  STORE_DOUBLE R4, 1
  STORE_TAG R4, tnumber
  CHECK_TAG R3, tnumber, exit(4)
  CHECK_TAG R5, tnumber, exit(4)
  %28 = LOAD_DOUBLE R3
  JUMP_CMP_NUM R5, %28, not_le, bb_bytecode_3, bb_bytecode_2
bb_bytecode_2:
  INTERRUPT 5u
  CHECK_TAG R5, tnumber, exit(5)
  %36 = LOAD_POINTER R1
  %37 = LOAD_DOUBLE R5
  %38 = TRY_NUM_TO_INDEX %37, bb_fallback_7
  %39 = SUB_INT %38, 1i
  CHECK_ARRAY_SIZE %36, %39, bb_fallback_7
  CHECK_NO_METATABLE %36, bb_fallback_7
  %42 = GET_ARR_ADDR %36, %39
  %43 = LOAD_TVALUE %42
  STORE_TVALUE R6, %43
  JUMP bb_8
bb_8:
  CHECK_TAG R2, tnumber, exit(6)
  CHECK_TAG R6, tnumber, bb_fallback_9
  %53 = LOAD_DOUBLE R2
  %55 = ADD_NUM %53, R6
  STORE_DOUBLE R2, %55
  JUMP bb_10
bb_10:
  %61 = LOAD_DOUBLE R3
  %62 = LOAD_DOUBLE R5
  %63 = ADD_NUM %62, 1
  STORE_DOUBLE R5, %63
  JUMP_CMP_NUM %63, %61, le, bb_bytecode_2, bb_bytecode_3
bb_bytecode_3:
  INTERRUPT 8u
  RETURN R2, 1i
)"
    );
}

TEST_CASE("UintSourceSanity")
{
    ScopedFastFlag luauCodegenBlockSafeEnv{FFlag::LuauCodegenBlockSafeEnv, true};
    ScopedFastFlag luauCodegenNumIntFolds{FFlag::LuauCodegenNumIntFolds2, true};

    // TODO: opportunity - many conversions and stores remain because of VM exits
    CHECK_EQ(
        "\n" + getCodegenAssembly(
                   R"(
local function foo(b: buffer, a: number, s: string)
    local r1 = buffer.readi32(b, bit32.bor(a, 0))
    local r2 = buffer.readu32(b, r1)
    local r3 = buffer.readi32(b, r2)
    local r4 = buffer.readu32(b, string.len(s))
    return r1, r2, r3, r4
end
)"
               ),
        R"(
; function foo($arg0, $arg1, $arg2) line 2
bb_0:
  CHECK_TAG R0, tbuffer, exit(entry)
  CHECK_TAG R1, tnumber, exit(entry)
  CHECK_TAG R2, tstring, exit(entry)
  JUMP bb_2
bb_2:
  JUMP bb_bytecode_1
bb_bytecode_1:
  implicit CHECK_SAFE_ENV exit(0)
  %11 = LOAD_DOUBLE R1
  %12 = NUM_TO_UINT %11
  %15 = UINT_TO_NUM %12
  STORE_DOUBLE R5, %15
  STORE_TAG R5, tnumber
  %23 = LOAD_POINTER R0
  %25 = TRUNCATE_UINT %12
  CHECK_BUFFER_LEN %23, %25, 4i, exit(9)
  %27 = BUFFER_READI32 %23, %25
  %28 = INT_TO_NUM %27
  STORE_DOUBLE R3, %28
  STORE_TAG R3, tnumber
  CHECK_BUFFER_LEN %23, %27, 4i, exit(15)
  %40 = BUFFER_READI32 %23, %27
  %41 = UINT_TO_NUM %40
  STORE_DOUBLE R4, %41
  STORE_TAG R4, tnumber
  CHECK_BUFFER_LEN %23, %40, 4i, exit(22)
  %53 = BUFFER_READI32 %23, %40
  %54 = INT_TO_NUM %53
  STORE_DOUBLE R5, %54
  %60 = LOAD_POINTER R2
  %61 = STRING_LEN %60
  %62 = INT_TO_NUM %61
  STORE_DOUBLE R8, %62
  STORE_TAG R8, tnumber
  CHECK_BUFFER_LEN %23, %61, 4i, exit(34)
  %74 = BUFFER_READI32 %23, %61
  %75 = UINT_TO_NUM %74
  STORE_DOUBLE R6, %75
  STORE_TAG R6, tnumber
  INTERRUPT 38u
  RETURN R3, 4i
)"
    );
}

TEST_SUITE_END();
