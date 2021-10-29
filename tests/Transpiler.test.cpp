// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Parser.h"
#include "Luau/TypeAttach.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"
#include "Luau/Transpiler.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("TranspilerTests");

TEST_CASE("test_1")
{
    const std::string example = R"(
local function isPortal(element)
    if type(element)~='table'then
        return false
    end

    return element.component==Core.Portal
end
)";

    CHECK_EQ(example, transpile(example).code);
}

TEST_CASE("string_literals")
{
    const std::string code = R"( local S='abcdef\n\f\a\020' )";

    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("string_literals_containing_utf8")
{
    const std::string code = R"( local S='lalala こんにちは' )"; // Konichiwa!
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("elseif_chains_indent_sensibly")
{
    const std::string code = R"(
        if This then
            Once()
        elseif That then
            Another()
        elseif SecondLast then
            Third()
        else
            IfAllElseFails()
        end
    )";

    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("strips_type_annotations")
{
    const std::string code = R"( local s: string= 'hello there' )";
    const std::string expected = R"( local s =        'hello there' )";

    CHECK_EQ(expected, transpile(code).code);
}

TEST_CASE("strips_type_assertion_expressions")
{
    const std::string code = R"( local s= some_function() :: any+ something_else() :: number )";
    const std::string expected = R"( local s= some_function() +       something_else()           )";

    CHECK_EQ(expected, transpile(code).code);
}

TEST_CASE("function_taking_ellipsis")
{
    const std::string code = R"( function F(...) end )";

    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("omit_decimal_place_for_integers")
{
    const std::string code = R"( local a=5, 6, 7, 3.141, 1.1290000000000002e+45 )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("for_loop")
{
    const std::string one = R"( for i=1,10 do end )";
    CHECK_EQ(one, transpile(one).code);

    const std::string code = R"( for i=5,6,7 do end )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("for_in_loop")
{
    const std::string code = R"( for k, v in ipairs(x)do end )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("while_loop")
{
    const std::string code = R"( while f(x)do print() end )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("repeat_until_loop")
{
    const std::string code = R"( repeat print() until f(x) )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("lambda")
{
    const std::string one = R"( local p=function(o, m, g) return 77 end )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( local p=function(o, m, g,...)  return 77 end )";
    CHECK_EQ(two, transpile(two).code);
}

TEST_CASE("local_function")
{
    const std::string one = R"( local function p(o, m, g) return 77 end )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( local function p(o, m, g,...)  return 77 end )";
    CHECK_EQ(two, transpile(two).code);
}

TEST_CASE("function")
{
    const std::string one = R"( function p(o, m, g) return 77 end )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( function p(o, m, g,...)  return 77 end )";
    CHECK_EQ(two, transpile(two).code);
}

TEST_CASE("table_literals")
{
    const std::string code = R"( local t={1, 2, 3, foo='bar', baz=99,[5.5]='five point five', 'end'} )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("more_table_literals")
{
    const std::string code = R"( local t={['Content-Type']='text/plain'} )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("table_literal_preserves_record_vs_general")
{
    const std::string code = R"( local t={['foo']='bar',quux=42} )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("table_literal_with_numeric_key")
{
    const std::string code = R"( local t={[5]='five',[6]='six'} )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("table_literal_with_keyword_key")
{
    const std::string code = R"( local t={['nil']=nil,['true']=true} )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("table_literal_closing_brace_at_correct_position")
{
    const std::string code = R"(
        local t={
            eggs='Tasty',
            avocado='more like awesomecavo amirite'
        }
    )";

    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("method_calls")
{
    const std::string code = R"( foo.bar.baz:quux() )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("method_definitions")
{
    const std::string code = R"( function foo.bar.baz:quux() end )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("spaces_between_keywords_even_if_it_pushes_the_line_estimation_off")
{
    // Luau::Parser doesn't exactly preserve the string representation of numbers in Lua, so we can find ourselves
    // falling out of sync with the original code.  We need to push keywords out so that there's at least one space between them.
    const std::string code = R"( if math.abs(raySlope) < .01 then return 0 end )";
    const std::string expected = R"( if math.abs(raySlope) < 0.01 then return 0 end)";
    CHECK_EQ(expected, transpile(code).code);
}

TEST_CASE("numbers")
{
    const std::string code = R"( local a=2510238627 )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("infinity")
{
    const std::string code = R"( local a = 1e500    local b = 1e400 )";
    const std::string expected = R"( local a = 1e500    local b = 1e500 )";
    CHECK_EQ(expected, transpile(code).code);
}

TEST_CASE("escaped_strings")
{
    const std::string code = R"( local s='\\b\\t\\n\\\\' )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("need_a_space_between_number_literals_and_dots")
{
    const std::string code = R"( return point and math.ceil(point* 100000* 100)/ 100000 .. '%'or '' )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("do_blocks")
{
    const std::string code = R"(
        foo()

        do
            local bar=baz()
            quux()
        end

        foo2()
    )";

    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("emit_a_do_block_in_cases_of_potentially_ambiguous_syntax")
{
    const std::string code = R"(
        f();
        (g or f)()
    )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("roundtrip_types")
{
    const std::string code = R"(
        local s:string='str'
        local t:{a:string,b:number,[string]:number}
        local fn:(string,string)->(number,number)
        local s2:typeof(s)='foo'
        local os:string?
        local sn:string|number
        local it:{x:number}&{y:number}
    )";
    auto allocator = Allocator{};
    auto names = AstNameTable{allocator};

    ParseOptions options;

    ParseResult parseResult = Parser::parse(code.data(), code.size(), names, allocator, options);
    REQUIRE(parseResult.errors.empty());

    CHECK_EQ(code, transpileWithTypes(*parseResult.root));
}

TEST_CASE("roundtrip_generic_types")
{
    const std::string code = R"(
        export type A<T> = {v:T, next:A<T>}
    )";
    auto allocator = Allocator{};
    auto names = AstNameTable{allocator};

    ParseOptions options;

    ParseResult parseResult = Parser::parse(code.data(), code.size(), names, allocator, options);
    REQUIRE(parseResult.errors.empty());

    CHECK_EQ(code, transpileWithTypes(*parseResult.root));
}

TEST_CASE_FIXTURE(Fixture, "attach_types")
{
    const std::string code = R"(
        local s='str'
        local t={a=1,b=false}
        local function fn()
            return 10
        end
    )";
    const std::string expected = R"(
        local s:string='str'
        local t:{a:number,b:boolean}={a=1,b=false}
        local function fn(): number
            return 10
        end
    )";

    CHECK_EQ(expected, decorateWithTypes(code));
}

TEST_CASE("a_table_key_can_be_the_empty_string")
{
    std::string code = "local T = {[''] = true}";

    CHECK_EQ(code, transpile(code).code);
}

// There's a bit of login in the transpiler that always adds a space before a dot if the previous symbol ends in a digit.
// This was surfacing an issue where we might not insert a space after the 'local' keyword.
TEST_CASE("always_emit_a_space_after_local_keyword")
{
    std::string code = "do local aZZZZ = Workspace.P1.Shape local bZZZZ = Enum.PartType.Cylinder end";
    std::string expected = "do local aZZZZ = Workspace.P1 .Shape local bZZZZ= Enum.PartType.Cylinder end";

    CHECK_EQ(expected, transpile(code).code);
}

TEST_CASE_FIXTURE(Fixture, "types_should_not_be_considered_cyclic_if_they_are_not_recursive")
{
    std::string code = R"(
        local common: {foo:string}

        local t = {}
        t.x = common
        t.y = common
    )";

    std::string expected = R"(
        local common: {foo:string}

        local t:{x:{foo:string},y:{foo:string}}={}
        t.x = common
        t.y = common
    )";

    CHECK_EQ(expected, decorateWithTypes(code));
}

TEST_CASE_FIXTURE(Fixture, "type_lists_should_be_emitted_correctly")
{
    std::string code = R"(
        local a = function(a: string, b: number, ...: string): (string, ...number)
        end

        local b = function(...: string): ...number
        end

        local c = function()
        end
    )";

    std::string expected = R"(
        local a:(string,number,...string)->(string,...number)=function(a:string,b:number,...:...string): (string,...number)
        end

        local b:(...string)->(...number)=function(...:...string): ...number
        end

        local c:()->()=function(): ()
        end
    )";

    std::string actual = decorateWithTypes(code);

    CHECK_EQ(expected, decorateWithTypes(code));
}

TEST_CASE_FIXTURE(Fixture, "function_type_location")
{
    std::string code = R"(
        local function foo(x: number): number
         return x
        end
        local g: (number)->number = foo
    )";

    std::string expected = R"(
        local function foo(x: number): number
         return x
        end
        local g: (number)->(number)=foo
    )";

    std::string actual = decorateWithTypes(code);

    CHECK_EQ(expected, actual);
}

TEST_SUITE_END();
