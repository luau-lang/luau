// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"
#include "Luau/BuiltinDefinitions.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauLowerBoundsCalculation);

TEST_SUITE_BEGIN("BuiltinTests");

TEST_CASE_FIXTURE(Fixture, "math_things_are_defined")
{
    CheckResult result = check(R"(
        local a00 = math.frexp
        local a01 = math.ldexp
        local a02 = math.fmod
        local a03 = math.modf
        local a04 = math.pow
        local a05 = math.exp
        local a06 = math.floor
        local a07 = math.abs
        local a08 = math.sqrt
        local a09 = math.log
        local a10 = math.log10
        local a11 = math.rad
        local a12 = math.deg
        local a13 = math.sin
        local a14 = math.cos
        local a15 = math.tan
        local a16 = math.sinh
        local a17 = math.cosh
        local a18 = math.tanh
        local a19 = math.atan
        local a20 = math.acos
        local a21 = math.asin
        local a22 = math.atan2
        local a23 = math.ceil
        local a24 = math.min
        local a25 = math.max
        local a26 = math.pi
        local a29 = math.huge
        local a30 = math.randomseed
        local a31 = math.random
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "next_iterator_should_infer_types_and_type_check")
{
    CheckResult result = check(R"(
        local a: string, b: number = next({ 1 })

        local s = "foo"
        local t = { [s] = 1 }
        local c: string, d: number = next(t)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "pairs_iterator_should_infer_types_and_type_check")
{
    CheckResult result = check(R"(
        type Map<K, V> = { [K]: V }
        local map: Map<string, number> = { ["foo"] = 1, ["bar"] = 2, ["baz"] = 3 }

        local it: (Map<string, number>, string | nil) -> (string, number), t: Map<string, number>, i: nil = pairs(map)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "ipairs_iterator_should_infer_types_and_type_check")
{
    CheckResult result = check(R"(
        type Map<K, V> = { [K]: V }
        local array: Map<number, string> = { "foo", "bar", "baz" }

        local it: (Map<number, string>, number) -> (number, string), t: Map<number, string>, i: number = ipairs(array)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "table_dot_remove_optionally_returns_generic")
{
    CheckResult result = check(R"(
        local t = { 1 }
        local n = table.remove(t, 7)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("n")), "number?");
}

TEST_CASE_FIXTURE(Fixture, "table_concat_returns_string")
{
    CheckResult result = check(R"(
        local r = table.concat({1,2,3,4}, ",", 2);
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*typeChecker.stringType, *requireType("r"));
}

TEST_CASE_FIXTURE(Fixture, "sort")
{
    CheckResult result = check(R"(
        local t = {1, 2, 3};
        table.sort(t)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "sort_with_predicate")
{
    CheckResult result = check(R"(
        --!strict
        local t = {1, 2, 3}
        local function p(a: number, b: number) return a < b end
        table.sort(t, p)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "sort_with_bad_predicate")
{
    CheckResult result = check(R"(
        --!strict
        local t = {'one', 'two', 'three'}
        local function p(a: number, b: number) return a < b end
        table.sort(t, p)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "strings_have_methods")
{
    CheckResult result = check(R"LUA(
        local s = ("RoactHostChangeEvent(%s)"):format("hello")
    )LUA");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*typeChecker.stringType, *requireType("s"));
}

TEST_CASE_FIXTURE(Fixture, "math_max_variatic")
{
    CheckResult result = check(R"(
        local n = math.max(1,2,3,4,5,6,7,8,9,0)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*typeChecker.numberType, *requireType("n"));
}

TEST_CASE_FIXTURE(Fixture, "math_max_checks_for_numbers")
{
    CheckResult result = check(R"(
        local n = math.max(1,2,"3")
    )");

    CHECK(!result.errors.empty());
}

TEST_CASE_FIXTURE(Fixture, "builtin_tables_sealed")
{
    CheckResult result = check(R"LUA(
        local b = bit32
    )LUA");
    TypeId bit32 = requireType("b");
    REQUIRE(bit32 != nullptr);
    const TableTypeVar* bit32t = get<TableTypeVar>(bit32);
    REQUIRE(bit32t != nullptr);
    CHECK_EQ(bit32t->state, TableState::Sealed);
}

TEST_CASE_FIXTURE(Fixture, "lua_51_exported_globals_all_exist")
{
    // Extracted from lua5.1
    CheckResult result = check(R"(
        local v__G = _G
        local v_string_sub = string.sub
        local v_string_upper = string.upper
        local v_string_len = string.len
        local v_string_rep = string.rep
        local v_string_find = string.find
        local v_string_match = string.match
        local v_string_char = string.char
        local v_string_gmatch = string.gmatch
        local v_string_reverse = string.reverse
        local v_string_byte = string.byte
        local v_string_format = string.format
        local v_string_gsub = string.gsub
        local v_string_lower = string.lower

        local v_xpcall = xpcall

        --local v_package_loadlib = package.loadlib
        --local v_package_loaders_1_ = package.loaders[1]
        --local v_package_loaders_2_ = package.loaders[2]
        --local v_package_loaders_3_ = package.loaders[3]
        --local v_package_loaders_4_ = package.loaders[4]

        local v_tostring = tostring
        local v_print = print

        --local v_os_exit = os.exit
        --local v_os_setlocale = os.setlocale
        local v_os_date = os.date
        --local v_os_getenv = os.getenv
        local v_os_difftime = os.difftime
        --local v_os_remove = os.remove
        local v_os_time = os.time
        --local v_os_clock = os.clock
        --local v_os_tmpname = os.tmpname
        --local v_os_rename = os.rename
        --local v_os_execute = os.execute

        local v_unpack = unpack
        local v_require = require
        local v_getfenv = getfenv
        local v_setmetatable = setmetatable
        local v_next = next
        local v_assert = assert
        local v_tonumber = tonumber

        --local v_io_lines = io.lines
        --local v_io_write = io.write
        --local v_io_close = io.close
        --local v_io_flush = io.flush
        --local v_io_open = io.open
        --local v_io_output = io.output
        --local v_io_type = io.type
        --local v_io_read = io.read
        --local v_io_stderr = io.stderr
        --local v_io_stdin = io.stdin
        --local v_io_input = io.input
        --local v_io_stdout = io.stdout
        --local v_io_popen = io.popen
        --local v_io_tmpfile = io.tmpfile

        local v_rawequal = rawequal
        --local v_collectgarbage = collectgarbage
        local v_getmetatable = getmetatable
        local v_rawset = rawset

        local v_math_log = math.log
        local v_math_max = math.max
        local v_math_acos = math.acos
        local v_math_huge = math.huge
        local v_math_ldexp = math.ldexp
        local v_math_pi = math.pi
        local v_math_cos = math.cos
        local v_math_tanh = math.tanh
        local v_math_pow = math.pow
        local v_math_deg = math.deg
        local v_math_tan = math.tan
        local v_math_cosh = math.cosh
        local v_math_sinh = math.sinh
        local v_math_random = math.random
        local v_math_randomseed = math.randomseed
        local v_math_frexp = math.frexp
        local v_math_ceil = math.ceil
        local v_math_floor = math.floor
        local v_math_rad = math.rad
        local v_math_abs = math.abs
        local v_math_sqrt = math.sqrt
        local v_math_modf = math.modf
        local v_math_asin = math.asin
        local v_math_min = math.min
        --local v_math_mod = math.mod
        local v_math_fmod = math.fmod
        local v_math_log10 = math.log10
        local v_math_atan2 = math.atan2
        local v_math_exp = math.exp
        local v_math_sin = math.sin
        local v_math_atan = math.atan

        --local v_debug_getupvalue = debug.getupvalue
        --local v_debug_debug = debug.debug
        --local v_debug_sethook = debug.sethook
        --local v_debug_getmetatable = debug.getmetatable
        --local v_debug_gethook = debug.gethook
        --local v_debug_setmetatable = debug.setmetatable
        --local v_debug_setlocal = debug.setlocal
        --local v_debug_traceback = debug.traceback
        --local v_debug_setfenv = debug.setfenv
        --local v_debug_getinfo = debug.getinfo
        --local v_debug_setupvalue = debug.setupvalue
        --local v_debug_getlocal = debug.getlocal
        --local v_debug_getregistry = debug.getregistry
        --local v_debug_getfenv = debug.getfenv

        local v_pcall = pcall

        --local v_table_setn = table.setn
        local v_table_insert = table.insert
        --local v_table_getn = table.getn
        --local v_table_foreachi = table.foreachi
        local v_table_maxn = table.maxn
        --local v_table_foreach = table.foreach
        local v_table_concat = table.concat
        local v_table_sort = table.sort
        local v_table_remove = table.remove

        local v_newproxy = newproxy
        local v_type = type

        local v_coroutine_resume = coroutine.resume
        local v_coroutine_yield = coroutine.yield
        local v_coroutine_status = coroutine.status
        local v_coroutine_wrap = coroutine.wrap
        local v_coroutine_create = coroutine.create
        local v_coroutine_running = coroutine.running

        local v_select = select
        local v_gcinfo = gcinfo
        local v_pairs = pairs
        local v_rawget = rawget
        local v_loadstring = loadstring
        local v_ipairs = ipairs
        local v__VERSION = _VERSION
        --local v_dofile = dofile
        local v_setfenv = setfenv
        --local v_load = load
        local v_error = error
        --local v_loadfile = loadfile
    )");

    dumpErrors(result);
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "setmetatable_unpacks_arg_types_correctly")
{
    CheckResult result = check(R"(
        setmetatable({}, setmetatable({}, {}))
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "table_insert_correctly_infers_type_of_array_2_args_overload")
{
    CheckResult result = check(R"(
        local t = {}
        table.insert(t, "foo")
        local s = t[1]
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(typeChecker.stringType, requireType("s"));
}

TEST_CASE_FIXTURE(Fixture, "table_insert_correctly_infers_type_of_array_3_args_overload")
{
    CheckResult result = check(R"(
        local t = {}
        table.insert(t, 1, "foo")
        local s = t[1]
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireType("s")));
}

TEST_CASE_FIXTURE(Fixture, "table_pack")
{
    CheckResult result = check(R"(
        local t = table.pack(1, "foo", true)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("{| [number]: boolean | number | string, n: number |}", toString(requireType("t")));
}

TEST_CASE_FIXTURE(Fixture, "table_pack_variadic")
{
    CheckResult result = check(R"(
--!strict
function f(): (string, ...number)
    return "str", 2, 3, 4
end

local t = table.pack(f())
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("{| [number]: number | string, n: number |}", toString(requireType("t")));
}

TEST_CASE_FIXTURE(Fixture, "table_pack_reduce")
{
    CheckResult result = check(R"(
        local t = table.pack(1, 2, true)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("{| [number]: boolean | number, n: number |}", toString(requireType("t")));

    result = check(R"(
        local t = table.pack("a", "b", "c")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("{| [number]: string, n: number |}", toString(requireType("t")));
}

TEST_CASE_FIXTURE(Fixture, "gcinfo")
{
    CheckResult result = check(R"(
        local n = gcinfo()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*typeChecker.numberType, *requireType("n"));
}

TEST_CASE_FIXTURE(Fixture, "getfenv")
{
    LUAU_REQUIRE_NO_ERRORS(check("getfenv(1)"));
}

TEST_CASE_FIXTURE(Fixture, "os_time_takes_optional_date_table")
{
    CheckResult result = check(R"(
        local n1 = os.time()
        local n2 = os.time({ year = 2020, month = 4, day = 20 })
        local n3 = os.time({ year = 2020, month = 4, day = 20, hour = 0, min = 0, sec = 0, isdst = true })
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*typeChecker.numberType, *requireType("n1"));
    CHECK_EQ(*typeChecker.numberType, *requireType("n2"));
    CHECK_EQ(*typeChecker.numberType, *requireType("n3"));
}

TEST_CASE_FIXTURE(Fixture, "thread_is_a_type")
{
    CheckResult result = check(R"(
        local co = coroutine.create(function() end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*typeChecker.threadType, *requireType("co"));
}

TEST_CASE_FIXTURE(Fixture, "coroutine_resume_anything_goes")
{
    CheckResult result = check(R"(
        local function nifty(x, y)
            print(x, y)
            local z = coroutine.yield(1, 2)
            print(z)
            return 42
        end

        local co = coroutine.create(nifty)
        local x, y = coroutine.resume(co, 1, 2)
        local answer = coroutine.resume(co, 3)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "coroutine_wrap_anything_goes")
{
    CheckResult result = check(R"(
        --!nonstrict
        local function nifty(x, y)
            print(x, y)
            local z = coroutine.yield(1, 2)
            print(z)
            return 42
        end

        local f = coroutine.wrap(nifty)
        local x, y = f(1, 2)
        local answer = f(3)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "setmetatable_should_not_mutate_persisted_types")
{
    CheckResult result = check(R"(
        local string = string

        setmetatable(string, {})
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto stringType = requireType("string");
    auto ttv = get<TableTypeVar>(stringType);
    REQUIRE(ttv);
}

TEST_CASE_FIXTURE(Fixture, "string_format_arg_types_inference")
{
    CheckResult result = check(R"(
        --!strict
        function f(a, b, c)
            return string.format("%f %d %s", a, b, c)
        end
    )");

    CHECK_EQ(0, result.errors.size());
    CHECK_EQ("(number, number, string) -> string", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "string_format_arg_count_mismatch")
{
    CheckResult result = check(R"(
        --!strict
        string.format("%f %d %s")
        string.format("%s", "hi", 42)
        string.format("%s", "hi", 42, ...)
        string.format("%s", "hi", ...)
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);
    CHECK_EQ(result.errors[0].location.begin.line, 2);
    CHECK_EQ(result.errors[1].location.begin.line, 3);
    CHECK_EQ(result.errors[2].location.begin.line, 4);
}

TEST_CASE_FIXTURE(Fixture, "string_format_correctly_ordered_types")
{
    CheckResult result = check(R"(
        --!strict
        string.format("%s", 123)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(tm->wantedType, typeChecker.stringType);
    CHECK_EQ(tm->givenType, typeChecker.numberType);
}

TEST_CASE_FIXTURE(Fixture, "xpcall")
{
    CheckResult result = check(R"(
        --!strict
        local a, b, c = xpcall(
            function() return 5, true end,
            function(e) return 0, false end
        )
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("boolean", toString(requireType("a")));
    CHECK_EQ("number", toString(requireType("b")));
    CHECK_EQ("boolean", toString(requireType("c")));
}

TEST_CASE_FIXTURE(Fixture, "see_thru_select")
{
    CheckResult result = check(R"(
        local a:number, b:boolean = select(2,"hi", 10, true)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "see_thru_select_count")
{
    CheckResult result = check(R"(
        local a = select("#","hi", 10, true)
    )");

    dumpErrors(result);
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "select_with_decimal_argument_is_rounded_down")
{
    CheckResult result = check(R"(
        local a: number, b: boolean = select(2.9, "foo", 1, true)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// Could be flaky if the fix has regressed.
TEST_CASE_FIXTURE(Fixture, "bad_select_should_not_crash")
{
    CheckResult result = check(R"(
        do end
        local _ = function(l0,...)
        end
        local _ = function()
        _(_);
        _ += select(_())
        end
    )");

    CHECK_LE(0, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "select_way_out_of_range")
{
    CheckResult result = check(R"(
        select(5432598430953240958)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    REQUIRE(get<GenericError>(result.errors[0]));
    CHECK_EQ("bad argument #1 to select (index out of range)", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "select_slightly_out_of_range")
{
    CheckResult result = check(R"(
        select(3, "a", 1)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    REQUIRE(get<GenericError>(result.errors[0]));
    CHECK_EQ("bad argument #1 to select (index out of range)", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "select_with_variadic_typepack_tail")
{
    CheckResult result = check(R"(
        --!nonstrict
        local function f(...)
            return ...
        end

        local foo, bar, baz, quux = select(1, f("foo", true))
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("any", toString(requireType("foo")));
    CHECK_EQ("any", toString(requireType("bar")));
    CHECK_EQ("any", toString(requireType("baz")));
    CHECK_EQ("any", toString(requireType("quux")));
}

TEST_CASE_FIXTURE(Fixture, "select_with_variadic_typepack_tail_and_string_head")
{
    CheckResult result = check(R"(
        --!nonstrict
        local function f(...)
            return ...
        end

        local foo, bar, baz, quux = select(1, "foo", f("bar", true))
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("any", toString(requireType("foo")));
    CHECK_EQ("any", toString(requireType("bar")));
    CHECK_EQ("any", toString(requireType("baz")));
    CHECK_EQ("any", toString(requireType("quux")));
}

TEST_CASE_FIXTURE(Fixture, "string_format_as_method")
{
    CheckResult result = check("local _ = ('%s'):format(5)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(tm->wantedType, typeChecker.stringType);
    CHECK_EQ(tm->givenType, typeChecker.numberType);
}

TEST_CASE_FIXTURE(Fixture, "string_format_use_correct_argument")
{
    CheckResult result = check(R"(
        local _ = ("%s"):format("%d", "hello")
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Argument count mismatch. Function expects 1 argument, but 2 are specified", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "string_format_use_correct_argument2")
{
    CheckResult result = check(R"(
        local _ = ("%s %d").format("%d %s", "A type error", 2)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK_EQ("Type 'string' could not be converted into 'number'", toString(result.errors[0]));
    CHECK_EQ("Type 'number' could not be converted into 'string'", toString(result.errors[1]));
}

TEST_CASE_FIXTURE(Fixture, "debug_traceback_is_crazy")
{
    CheckResult result = check(R"(
local co: thread = ...
-- debug.traceback takes thread?, message?, level? - yes, all optional!
debug.traceback()
debug.traceback(nil, 1)
debug.traceback("msg")
debug.traceback("msg", 1)
debug.traceback(co)
debug.traceback(co, "msg")
debug.traceback(co, "msg", 1)
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "debug_info_is_crazy")
{
    CheckResult result = check(R"(
local co: thread, f: ()->() = ...

-- debug.info takes thread?, level, options or function, options
debug.info(1, "n")
debug.info(co, 1, "n")
debug.info(f, "n")
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "aliased_string_format")
{
    CheckResult result = check(R"(
        local fmt = string.format
        local s = fmt("%d", "oops")
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type 'string' could not be converted into 'number'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "string_lib_self_noself")
{
    CheckResult result = check(R"(
        --!nonstrict
        local a1 = string.byte("abcdef", 2)
        local a2 = string.find("abcdef", "def")
        local a3 = string.gmatch("ab ab", "%a+")
        local a4 = string.gsub("abab", "ab", "cd")
        local a5 = string.len("abc")
        local a6 = string.match("12 ab", "%d+ %a+")
        local a7 = string.rep("a", 10)
        local a8 = string.sub("abcd", 1, 2)
        local a9 = string.split("a,b,c", ",")
        local a0 = string.packsize("ff")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "gmatch_definition")
{
    CheckResult result = check(R"_(
local a, b, c = ("hey"):gmatch("(.)(.)(.)")()

for c in ("hey"):gmatch("(.)") do
    print(c:upper())
end
)_");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "select_on_variadic")
{
    CheckResult result = check(R"(
        local function f(): (number, ...(boolean | number))
            return 100, true, 1
        end

        local a, b, c = select(f())
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("any", toString(requireType("a")));
    CHECK_EQ("any", toString(requireType("b")));
    CHECK_EQ("any", toString(requireType("c")));
}

TEST_CASE_FIXTURE(Fixture, "string_format_report_all_type_errors_at_correct_positions")
{
    CheckResult result = check(R"(
        ("%s%d%s"):format(1, "hello", true)
        string.format("%s%d%s", 1, "hello", true)
    )");

    TypeId stringType = typeChecker.stringType;
    TypeId numberType = typeChecker.numberType;
    TypeId booleanType = typeChecker.booleanType;

    LUAU_REQUIRE_ERROR_COUNT(6, result);

    CHECK_EQ(Location(Position{1, 26}, Position{1, 27}), result.errors[0].location);
    CHECK_EQ(TypeErrorData(TypeMismatch{stringType, numberType}), result.errors[0].data);

    CHECK_EQ(Location(Position{1, 29}, Position{1, 36}), result.errors[1].location);
    CHECK_EQ(TypeErrorData(TypeMismatch{numberType, stringType}), result.errors[1].data);

    CHECK_EQ(Location(Position{1, 38}, Position{1, 42}), result.errors[2].location);
    CHECK_EQ(TypeErrorData(TypeMismatch{stringType, booleanType}), result.errors[2].data);

    CHECK_EQ(Location(Position{2, 32}, Position{2, 33}), result.errors[3].location);
    CHECK_EQ(TypeErrorData(TypeMismatch{stringType, numberType}), result.errors[3].data);

    CHECK_EQ(Location(Position{2, 35}, Position{2, 42}), result.errors[4].location);
    CHECK_EQ(TypeErrorData(TypeMismatch{numberType, stringType}), result.errors[4].data);

    CHECK_EQ(Location(Position{2, 44}, Position{2, 48}), result.errors[5].location);
    CHECK_EQ(TypeErrorData(TypeMismatch{stringType, booleanType}), result.errors[5].data);
}

TEST_CASE_FIXTURE(Fixture, "tonumber_returns_optional_number_type")
{
    CheckResult result = check(R"(
        --!strict
        local b: number = tonumber('asdf')
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type 'number?' could not be converted into 'number'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "tonumber_returns_optional_number_type2")
{
    CheckResult result = check(R"(
        --!strict
        local b: number = tonumber('asdf') or 1
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "dont_add_definitions_to_persistent_types")
{
    CheckResult result = check(R"(
        local f = math.sin
        local function g(x) return math.sin(x) end
        f = g
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId fType = requireType("f");
    const FunctionTypeVar* ftv = get<FunctionTypeVar>(fType);
    REQUIRE(fType);
    REQUIRE(fType->persistent);
    REQUIRE(!ftv->definition);

    TypeId gType = requireType("g");
    const FunctionTypeVar* gtv = get<FunctionTypeVar>(gType);
    REQUIRE(gType);
    REQUIRE(!gType->persistent);
    REQUIRE(gtv->definition);
}

TEST_CASE_FIXTURE(Fixture, "assert_removes_falsy_types")
{
    ScopedFastFlag sff[]{
        {"LuauAssertStripsFalsyTypes", true},
        {"LuauDiscriminableUnions2", true},
        {"LuauWidenIfSupertypeIsFree2", true},
    };

    CheckResult result = check(R"(
        local function f(x: (number | boolean)?)
            return assert(x)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    if (FFlag::LuauLowerBoundsCalculation)
        CHECK_EQ("((boolean | number)?) -> number | true", toString(requireType("f")));
    else
        CHECK_EQ("((boolean | number)?) -> boolean | number", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "assert_removes_falsy_types2")
{
    ScopedFastFlag sff[]{
        {"LuauAssertStripsFalsyTypes", true},
        {"LuauDiscriminableUnions2", true},
        {"LuauWidenIfSupertypeIsFree2", true},
    };

    CheckResult result = check(R"(
        local function f(x: (number | boolean)?): number | true
            return assert(x)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("((boolean | number)?) -> number | true", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "assert_removes_falsy_types_even_from_type_pack_tail_but_only_for_the_first_type")
{
    ScopedFastFlag sff[]{
        {"LuauAssertStripsFalsyTypes", true},
        {"LuauDiscriminableUnions2", true},
    };

    CheckResult result = check(R"(
        local function f(...: number?)
            return assert(...)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("(...number?) -> (number, ...number?)", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "assert_returns_false_and_string_iff_it_knows_the_first_argument_cannot_be_truthy")
{
    ScopedFastFlag sff[]{
        {"LuauAssertStripsFalsyTypes", true},
        {"LuauDiscriminableUnions2", true},
    };

    CheckResult result = check(R"(
        local function f(x: nil)
            return assert(x, "hmm")
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("(nil) -> nil", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "table_freeze_is_generic")
{
    CheckResult result = check(R"(
        local t1: {a: number} = {a = 42}
        local t2: {b: string} = {b = "hello"}
        local t3: {boolean} = {false, true}

        local tf1 = table.freeze(t1)
        local tf2 = table.freeze(t2)
        local tf3 = table.freeze(t3)

        local a = tf1.a
        local b = tf2.b
        local c = tf3[2]

        local d = tf1.b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Key 'b' not found in table '{| a: number |}'", toString(result.errors[0]));

    CHECK_EQ("number", toString(requireType("a")));
    CHECK_EQ("string", toString(requireType("b")));
    CHECK_EQ("boolean", toString(requireType("c")));
    CHECK_EQ("*unknown*", toString(requireType("d")));
}

TEST_CASE_FIXTURE(Fixture, "set_metatable_needs_arguments")
{
    ScopedFastFlag sff{"LuauSetMetaTableArgsCheck", true};
    CheckResult result = check(R"(
local a = {b=setmetatable}
a.b()
a:b()
a:b({})
    )");
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ(result.errors[0], (TypeError{Location{{2, 0}, {2, 5}}, CountMismatch{2, 0}}));
    CHECK_EQ(result.errors[1], (TypeError{Location{{3, 0}, {3, 5}}, CountMismatch{2, 1}}));
}

TEST_CASE_FIXTURE(Fixture, "typeof_unresolved_function")
{
    CheckResult result = check(R"(
local function f(a: typeof(f)) end
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Unknown global 'f'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "no_persistent_typelevel_change")
{
    TypeId mathTy = requireType(typeChecker.globalScope, "math");
    REQUIRE(mathTy);
    TableTypeVar* ttv = getMutable<TableTypeVar>(mathTy);
    REQUIRE(ttv);
    const FunctionTypeVar* ftv = get<FunctionTypeVar>(ttv->props["frexp"].type);
    REQUIRE(ftv);
    auto original = ftv->level;

    CheckResult result = check("local a = math.frexp");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(ftv->level.level == original.level);
    CHECK(ftv->level.subLevel == original.subLevel);
}

TEST_CASE_FIXTURE(Fixture, "global_singleton_types_are_sealed")
{
    CheckResult result = check(R"(
local function f(x: string)
    local p = x:split('a')
    p = table.pack(table.unpack(p, 1, #p - 1))
    return p
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
