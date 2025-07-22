// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Parser.h"
#include "Luau/TypeAttach.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"
#include "Luau/Transpiler.h"

#include "Fixture.h"
#include "ScopedFlags.h"

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

    return element.component == Core.Portal
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

TEST_CASE("if_stmt_spaces_around_tokens")
{
    const std::string one = R"( if     This then Once() end)";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( if This     then Once() end)";
    CHECK_EQ(two, transpile(two).code);

    const std::string three = R"( if This then     Once() end)";
    CHECK_EQ(three, transpile(three).code);

    const std::string four = R"( if This then Once()     end)";
    CHECK_EQ(four, transpile(four).code);

    const std::string five = R"( if This then Once()   else Other() end)";
    CHECK_EQ(five, transpile(five).code);

    const std::string six = R"( if This then Once() else    Other() end)";
    CHECK_EQ(six, transpile(six).code);

    const std::string seven = R"( if This then Once()    elseif true then Other() end)";
    CHECK_EQ(seven, transpile(seven).code);

    const std::string eight = R"( if This then Once() elseif     true then Other() end)";
    CHECK_EQ(eight, transpile(eight).code);

    const std::string nine = R"( if This then Once() elseif true    then Other() end)";
    CHECK_EQ(nine, transpile(nine).code);
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
    const std::string expected = R"( local s        = 'hello there' )";
    CHECK_EQ(expected, transpile(code).code);
}

TEST_CASE("strips_type_assertion_expressions")
{
    const std::string code = R"( local s= some_function() :: any+ something_else() :: number )";
    const std::string expected = R"( local s= some_function()       + something_else()           )";
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

TEST_CASE("for_loop_spaces_around_tokens")
{
    const std::string one = R"( for index = 1, 10 do call(index) end )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( for index = 1  , 10 do call(index) end )";
    CHECK_EQ(two, transpile(two).code);

    const std::string three = R"( for index = 1, 10  ,  3 do call(index) end )";
    CHECK_EQ(three, transpile(three).code);

    const std::string four = R"( for index = 1, 10    do call(index) end )";
    CHECK_EQ(four, transpile(four).code);

    const std::string five = R"( for index = 1, 10 do call(index)    end )";
    CHECK_EQ(five, transpile(five).code);
}

TEST_CASE("for_in_loop")
{
    const std::string code = R"( for k, v in ipairs(x)do end )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("for_in_loop_spaces_around_tokens")
{
    const std::string one = R"( for k, v in ipairs(x)   do end )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( for k, v    in    ipairs(x) do end )";
    CHECK_EQ(two, transpile(two).code);

    const std::string three = R"( for k  ,  v in ipairs(x) do end )";
    CHECK_EQ(three, transpile(three).code);

    const std::string four = R"( for k, v in next  , t  do end )";
    CHECK_EQ(four, transpile(four).code);

    const std::string five = R"( for k, v in ipairs(x) do   end )";
    CHECK_EQ(five, transpile(five).code);
}

TEST_CASE("for_in_single_variable")
{
    const std::string one = R"( for key in pairs(x) do end )";
    CHECK_EQ(one, transpile(one).code);
}

TEST_CASE("while_loop")
{
    const std::string code = R"( while f(x)do print() end )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("while_loop_spaces_around_tokens")
{
    const std::string one = R"( while     f(x) do print() end )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( while f(x)    do print() end )";
    CHECK_EQ(two, transpile(two).code);

    const std::string three = R"( while f(x) do    print() end )";
    CHECK_EQ(three, transpile(three).code);

    const std::string four = R"( while f(x) do print()    end )";
    CHECK_EQ(four, transpile(four).code);
}

TEST_CASE("repeat_until_loop")
{
    const std::string code = R"( repeat print() until f(x) )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("repeat_until_loop_condition_on_new_line")
{
    const std::string code = R"(
    repeat
        print()
    until
        f(x) )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("lambda")
{
    const std::string one = R"( local p=function(o, m, g) return 77 end )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( local p=function(o, m, g,...)  return 77 end )";
    CHECK_EQ(two, transpile(two).code);
}

TEST_CASE("local_assignment")
{
    const std::string one = R"( local x = 1 )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( local x, y, z = 1, 2, 3 )";
    CHECK_EQ(two, transpile(two).code);

    const std::string three = R"( local x )";
    CHECK_EQ(three, transpile(three).code);
}

TEST_CASE("local_assignment_spaces_around_tokens")
{
    const std::string one = R"( local    x = 1 )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( local x    = 1 )";
    CHECK_EQ(two, transpile(two).code);

    const std::string three = R"( local x =    1 )";
    CHECK_EQ(three, transpile(three).code);

    const std::string four = R"( local x   , y = 1, 2 )";
    CHECK_EQ(four, transpile(four).code);

    const std::string five = R"( local x,    y = 1, 2 )";
    CHECK_EQ(five, transpile(five).code);

    const std::string six = R"( local x, y = 1   , 2 )";
    CHECK_EQ(six, transpile(six).code);

    const std::string seven = R"( local x, y = 1,    2 )";
    CHECK_EQ(seven, transpile(seven).code);
}

TEST_CASE("local_function")
{
    const std::string one = R"( local function p(o, m, g) return 77 end )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( local function p(o, m, g,...)  return 77 end )";
    CHECK_EQ(two, transpile(two).code);
}

TEST_CASE("local_function_spaces_around_tokens")
{
    const std::string one = R"( local     function p(o, m, ...) end )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( local function    p(o, m, ...) end )";
    CHECK_EQ(two, transpile(two).code);
}

TEST_CASE("function")
{
    const std::string one = R"( function p(o, m, g) return 77 end )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( function p(o, m, g,...)  return 77 end )";
    CHECK_EQ(two, transpile(two).code);
}

TEST_CASE("function_spaces_around_tokens")
{
    const std::string two = R"( function     p(o, m, ...) end )";
    CHECK_EQ(two, transpile(two).code);

    const std::string three = R"( function p(   o, m, ...) end )";
    CHECK_EQ(three, transpile(three).code);

    const std::string four = R"( function p(o   , m, ...) end )";
    CHECK_EQ(four, transpile(four).code);

    const std::string five = R"( function p(o,   m, ...) end )";
    CHECK_EQ(five, transpile(five).code);

    const std::string six = R"( function p(o, m   , ...) end )";
    CHECK_EQ(six, transpile(six).code);

    const std::string seven = R"( function p(o, m,   ...) end )";
    CHECK_EQ(seven, transpile(seven).code);

    const std::string eight = R"( function p(o, m, ...   ) end )";
    CHECK_EQ(eight, transpile(eight).code);

    const std::string nine = R"( function p(o, m, ...)   end )";
    CHECK_EQ(nine, transpile(nine).code);
}

TEST_CASE("function_with_types_spaces_around_tokens")
{
    std::string code = R"( function p<X, Y, Z...>(o: string, m: number, ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p   <X, Y, Z...>(o: string, m: number, ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X   , Y, Z...>(o: string, m: number, ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X,   Y, Z...>(o: string, m: number, ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y,   Z...>(o: string, m: number, ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z  ...>(o: string, m: number, ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z...  >(o: string, m: number, ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z...>  (o: string, m: number, ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z...>(o  : string, m: number, ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z...>(o:   string, m: number, ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z...>(o: string  , m: number, ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z...>(o: string,   m: number, ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z...>(o: string, m: number,   ...: any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z...>(o: string, m: number, ...  : any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z...>(o: string, m: number, ...:   any): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z...>(o: string, m: number, ...: any  ): string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z...>(o: string, m: number, ...: any)   :string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( function p<X, Y, Z...>(o: string, m: number, ...: any):    string end )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("returns_spaces_around_tokens")
{
    const std::string one = R"( return    1 )";
    CHECK_EQ(one, transpile(one).code);

    const std::string two = R"( return 1   , 2 )";
    CHECK_EQ(two, transpile(two).code);

    const std::string three = R"( return 1,  2 )";
    CHECK_EQ(three, transpile(three).code);
}

TEST_CASE_FIXTURE(Fixture, "type_alias_spaces_around_tokens")
{
    std::string code = R"( type Foo = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type    Foo = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo    = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo =    string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( export type Foo = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( export    type Foo = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo<X, Y, Z...> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo  <X, Y, Z...> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo<  X, Y, Z...> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo<X  , Y, Z...> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo<X,   Y, Z...> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo<X, Y  , Z...> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo<X, Y,   Z...> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo<X, Y, Z  ...> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo<X, Y, Z...  > = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "type_alias_with_defaults_spaces_around_tokens")
{
    std::string code = R"( type Foo<X = string, Z... = ...any> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo<X   = string, Z... = ...any> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo<X =   string, Z... = ...any> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo<X = string, Z...   = ...any> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo<X = string, Z... =   ...any> = string )";
    CHECK_EQ(code, transpile(code, {}, true).code);
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

TEST_CASE("table_literal_with_semicolon_separators")
{
    const std::string code = R"(
        local t = { x = 1; y = 2 }
    )";

    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("table_literal_with_trailing_separators")
{
    const std::string code = R"(
        local t = { x = 1, y = 2, }
    )";

    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("table_literal_with_spaces_around_separator")
{
    const std::string code = R"(
        local t = { x = 1  , y = 2 }
    )";

    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("table_literal_with_spaces_around_equals")
{
    const std::string code = R"(
        local t = { x    =   1  }
    )";

    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("table_literal_multiline_with_indexers")
{
    const std::string code = R"(
        local t = {
            ["my first value"] = "x";
            ["my second value"] = "y";
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
    const std::string code = R"( if math.abs(raySlope) < .01 then return 0 end )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("numbers")
{
    const std::string code = R"( local a=2510238627 )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("infinity")
{
    const std::string code = R"( local a = 1e500    local b = 1e400 )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("numbers_with_separators")
{
    const std::string code = R"( local a = 123_456_789 )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("hexadecimal_numbers")
{
    const std::string code = R"( local a = 0xFFFF )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("binary_numbers")
{
    const std::string code = R"( local a = 0b0101 )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("single_quoted_strings")
{
    const std::string code = R"( local a = 'hello world' )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("double_quoted_strings")
{
    const std::string code = R"( local a = "hello world" )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("simple_interp_string")
{
    const std::string code = R"( local a = `hello world` )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("raw_strings")
{
    const std::string code = R"( local a = [[ hello world ]] )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("raw_strings_with_blocks")
{
    const std::string code = R"( local a = [==[ hello world ]==] )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("escaped_strings")
{
    const std::string code = R"( local s='\\b\\t\\n\\\\' )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("escaped_strings_2")
{
    const std::string code = R"( local s="\a\b\f\n\r\t\v\'\"\\" )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("escaped_strings_newline")
{
    const std::string code = R"(
    print("foo \
        bar")
    )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("escaped_strings_raw")
{
    const std::string code = R"( local x = [=[\v<((do|load)file|require)\s*\(?['"]\zs[^'"]+\ze['"]]=] )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("position_correctly_updated_when_writing_multiline_string")
{
    const std::string code = R"(
    call([[
        testing
    ]]) )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("need_a_space_between_number_literals_and_dots")
{
    const std::string code = R"( return point and math.ceil(point* 100000* 100)/ 100000 .. '%'or '' )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("binary_keywords")
{
    const std::string code = "local c = a0 ._ or b0 ._";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("function_call_parentheses_no_args")
{
    const std::string code = R"( call() )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("function_call_parentheses_one_arg")
{
    const std::string code = R"( call(arg) )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("function_call_parentheses_multiple_args")
{
    const std::string code = R"( call(arg1, arg3, arg3) )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("function_call_parentheses_multiple_args_no_space")
{
    const std::string code = R"( call(arg1,arg3,arg3) )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("function_call_parentheses_multiple_args_space_before_commas")
{
    const std::string code = R"( call(arg1 ,arg3 ,arg3) )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("function_call_spaces_before_parentheses")
{
    const std::string code = R"( call () )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("function_call_spaces_within_parentheses")
{
    const std::string code = R"( call(  ) )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("function_call_string_double_quotes")
{
    const std::string code = R"( call "string" )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("function_call_string_single_quotes")
{
    const std::string code = R"( call 'string' )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("function_call_string_no_space")
{
    const std::string code = R"( call'string' )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("function_call_table_literal")
{
    const std::string code = R"( call { x = 1 } )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE("function_call_table_literal_no_space")
{
    const std::string code = R"( call{x=1} )";
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

TEST_CASE("nested_do_block")
{
    const std::string code = R"(
        do
            do
                local x = 1
            end
        end
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

TEST_CASE_FIXTURE(Fixture, "parentheses_multiline")
{
    std::string code = R"(
local test = (
    x
)
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "stmt_semicolon")
{
    std::string code = R"( local test = 1; )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( local test = 1  ; )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "do_block_ending_with_semicolon")
{
    std::string code = R"(
        do
            return;
        end;
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "if_stmt_semicolon")
{
    std::string code = R"(
        if init then
            x = string.sub(x, utf8.offset(x, init));
        end;
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "if_stmt_semicolon_2")
{
    std::string code = R"(
        if (t < 1) then return c/2*t*t + b end;
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "for_loop_stmt_semicolon")
{
    std::string code = R"(
        for i,v in ... do
        end;
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "while_do_semicolon")
{
    std::string code = R"(
        while true do
        end;
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "function_definition_semicolon")
{
    std::string code = R"(
        function foo()
        end;
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);
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
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE_FIXTURE(Fixture, "types_should_not_be_considered_cyclic_if_they_are_not_recursive")
{
    std::string code = R"(
        local common: {foo:string} = {foo = 'foo'}

        local t = {}
        t.x = common
        t.y = common
    )";

    std::string expected = R"(
        local common: {foo:string} = {foo = 'foo'}

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
        local a:(a:string,b:number,...string)->(string,...number)=function(a:string,b:number,...:string): (string,...number)
        end

        local b:(...string)->(...number)=function(...:string): ...number
        end

        local c:()->()=function(): ()
        end
    )";

    std::string actual = decorateWithTypes(code);

    CHECK_EQ(expected, actual);
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

TEST_CASE_FIXTURE(Fixture, "transpile_type_assertion")
{
    std::string code = "local a = 5 :: number";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "type_assertion_spaces_around_tokens")
{
    std::string code = "local a = 5   :: number";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a = 5 ::   number";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_if_then_else")
{
    std::string code = "local a = if 1 then 2 else 3";

    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_if_then_else_multiple_conditions")
{
    std::string code = "local a = if 1 then 2 elseif 3 then 4 else 5";

    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_if_then_else_multiple_conditions_2")
{
    std::string code = R"(
        local x = if yes
            then nil
            else if no
                then if this
                    then that
                    else other
                else nil
    )";

    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE_FIXTURE(Fixture, "if_then_else_spaces_around_tokens")
{
    std::string code = "local a = if   1 then 2 else 3";
    CHECK_EQ(code, transpile(code).code);

    code = "local a = if 1   then 2 else 3";
    CHECK_EQ(code, transpile(code).code);

    code = "local a = if 1 then   2 else 3";
    CHECK_EQ(code, transpile(code).code);

    code = "local a = if 1 then 2   else 3";
    CHECK_EQ(code, transpile(code).code);

    code = "local a = if 1 then 2 else   3";
    CHECK_EQ(code, transpile(code).code);

    code = "local a = if 1 then 2   elseif 3 then 4 else 5";
    CHECK_EQ(code, transpile(code).code);

    code = "local a = if 1 then 2 elseif   3 then 4 else 5";
    CHECK_EQ(code, transpile(code).code);

    code = "local a = if 1 then 2 elseif 3   then 4 else 5";
    CHECK_EQ(code, transpile(code).code);

    code = "local a = if 1 then 2 elseif 3 then   4 else 5";
    CHECK_EQ(code, transpile(code).code);

    code = "local a = if 1 then 2 elseif 3 then 4   else 5";
    CHECK_EQ(code, transpile(code).code);

    code = "local a = if 1 then 2 elseif 3 then 4 else   5";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE_FIXTURE(Fixture, "if_then_else_spaces_between_else_if")
{
    std::string code = R"(
    return
        if a then "was a" else
        if b then "was b" else
        if c then "was c" else
        "was nothing!"
    )";
    CHECK_EQ(code, transpile(code).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_type_reference_import")
{
    fileResolver.source["game/A"] = R"(
export type Type = { a: number }
return {}
    )";

    std::string code = R"(
local Import = require(game.A)
local a: Import.Type
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_type_reference_spaces_around_tokens")
{
    std::string code = R"( local _: Foo.Type )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( local _: Foo   .Type )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( local _: Foo.   Type )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( local _: Type  <> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( local _: Type<  > )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( local _: Type<  number> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( local _: Type<number  ,string> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( local _: Type<number,  string  > )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_type_annotation_spaces_around_tokens")
{
    std::string code = R"( local _: Type )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( local _  : Type )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( local _:   Type )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( local x: Type, y = 1 )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( local x  : Type, y = 1 )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_for_loop_annotation_spaces_around_tokens")
{
    std::string code = R"( for i: number = 1, 10 do end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( for i   : number = 1, 10 do end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( for i:    number = 1, 10 do end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( for x: number, y: number in ... do end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( for x   : number, y: number in ... do end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( for x:    number, y: number in ... do end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( for x: number, y   : number in ... do end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( for x: number, y:    number in ... do end )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_type_packs")
{
    std::string code = R"(
type Packed<T...> = (T...)->(T...)
local a: Packed<>
local b: Packed<(number, string)>
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "type_packs_spaces_around_tokens")
{
    std::string code = R"( type _ = Packed<  T...> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type _ = Packed<T  ...> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type _ = Packed<   ...T> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type _ = Packed<...   T> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type _ = Packed<  ()> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type _ = Packed<  (string, number)> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type _ = Packed<(  string, number)> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type _ = Packed<(string  , number)> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type _ = Packed<(string,   number)> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type _ = Packed<(string, number  )> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type _ = Packed<(string, number)  > )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type _ = Packed<(  )> )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type _ = Packed<()  > )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_union_type_nested")
{
    std::string code = "local a: ((number)->(string))|((string)->(string))";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_union_type_nested_2")
{
    std::string code = "local a: (number&string)|(string&boolean)";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_union_type_nested_3")
{
    std::string code = "local a: nil | (string & number)";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_intersection_type_nested")
{
    std::string code = "local a: ((number)->(string))&((string)->(string))";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_intersection_type_nested_2")
{
    std::string code = "local a: (number|string)&(string|boolean)";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_intersection_type_with_function")
{
    std::string code = "type FnB<U...> = () -> U... & T";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_leading_union_pipe")
{
    std::string code = "local a: | string | number";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: | string";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_union_spaces_around_tokens")
{
    std::string code = "local a: string   | number";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string |   number";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_leading_intersection_ampersand")
{
    std::string code = "local a: & string & number";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: & string";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_intersection_spaces_around_tokens")
{
    std::string code = "local a: string   & number";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string &   number";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_mixed_union_intersection")
{
    std::string code = "local a: string | (Foo & Bar)";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string |   (Foo & Bar)";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string | (  Foo & Bar)";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string | (Foo & Bar  )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string &   (Foo | Bar)";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string & (  Foo | Bar)";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string & (Foo | Bar  )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_preserve_union_optional_style")
{
    std::string code = "local a: string | nil";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string?";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string???";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string? | nil";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string | nil | number";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string | nil | number?";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = "local a: string? | number?";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_varargs")
{
    std::string code = "local function f(...) return ... end";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "index_name_spaces_around_tokens")
{
    std::string one = "local _ = a.name";
    CHECK_EQ(one, transpile(one, {}, true).code);

    std::string two = "local _ = a   .name";
    CHECK_EQ(two, transpile(two, {}, true).code);

    std::string three = "local _ = a.   name";
    CHECK_EQ(three, transpile(three, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "index_name_ends_with_digit")
{
    std::string code = "sparkles.Color = Color3.new()";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_index_expr")
{
    std::string code = "local a = {1, 2, 3} local b = a[2]";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "index_expr_spaces_around_tokens")
{
    std::string one = "local _ = a[2]";
    CHECK_EQ(one, transpile(one, {}, true).code);

    std::string two = "local _ = a   [2]";
    CHECK_EQ(two, transpile(two, {}, true).code);

    std::string three = "local _ = a[   2]";
    CHECK_EQ(three, transpile(three, {}, true).code);

    std::string four = "local _ = a[2   ]";
    CHECK_EQ(four, transpile(four, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_unary")
{
    std::string code = R"(
local a = 1
local b = -1
local c = true
local d = not c
local e = 'hello'
local d = #e
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "unary_spaces_around_tokens")
{
    std::string code = R"(
local _ =   -1
local _ = -  1
local _ =   not true
local _ = not   true
local _ =   #e
local _ = #  e
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "binary_spaces_around_tokens")
{
    std::string code = R"(
local _ =    1+1
local _ = 1   +1
local _ = 1+   1
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_break_continue")
{
    std::string code = R"(
local a, b, c
repeat
    if a then break end
    if b then continue end
until c
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_compound_assignment")
{
    std::string code = R"(
local a = 1
a += 2
a -= 3
a *= 4
a /= 5
a //= 5
a %= 6
a ^= 7
a ..= ' - result'
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "compound_assignment_spaces_around_tokens")
{
    std::string one = R"( a   += 1 )";
    CHECK_EQ(one, transpile(one, {}, true).code);

    std::string two = R"( a +=   1 )";
    CHECK_EQ(two, transpile(two, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_assign_multiple")
{
    std::string code = "a, b, c = 1, 2, 3";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_assign_spaces_around_tokens")
{
    std::string one = "a = 1";
    CHECK_EQ(one, transpile(one).code);

    std::string two = "a    = 1";
    CHECK_EQ(two, transpile(two).code);

    std::string three = "a =    1";
    CHECK_EQ(three, transpile(three).code);

    std::string four = "a   , b = 1, 2";
    CHECK_EQ(four, transpile(four).code);

    std::string five = "a,    b = 1, 2";
    CHECK_EQ(five, transpile(five).code);

    std::string six = "a, b = 1   , 2";
    CHECK_EQ(six, transpile(six).code);

    std::string seven = "a, b = 1,    2";
    CHECK_EQ(seven, transpile(seven).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_generic_function")
{
    std::string code = R"(
local function foo<T,S...>(a: T, ...: S...) return 1 end
local f: <T,S...>(T, S...)->(number) = foo
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_union_reverse")
{
    std::string code = "local a: nil | number";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_for_in_multiple")
{
    std::string code = "for k,v in next,{}do print(k,v) end";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_error_expr")
{
    std::string code = "local a = f:-";

    auto allocator = Allocator{};
    auto names = AstNameTable{allocator};
    ParseResult parseResult = Parser::parse(code.data(), code.size(), names, allocator, {});

    CHECK_EQ("local a = (error-expr: f:%error-id%)-(error-expr)", transpileWithTypes(*parseResult.root));
}

TEST_CASE_FIXTURE(Fixture, "transpile_error_stat")
{
    std::string code = "-";

    auto allocator = Allocator{};
    auto names = AstNameTable{allocator};
    ParseResult parseResult = Parser::parse(code.data(), code.size(), names, allocator, {});

    CHECK_EQ("(error-stat: (error-expr))", transpileWithTypes(*parseResult.root));
}

TEST_CASE_FIXTURE(Fixture, "transpile_error_type")
{
    std::string code = "local a: ";

    auto allocator = Allocator{};
    auto names = AstNameTable{allocator};
    ParseResult parseResult = Parser::parse(code.data(), code.size(), names, allocator, {});

    CHECK_EQ("local a:%error-type%", transpileWithTypes(*parseResult.root));
}

TEST_CASE_FIXTURE(Fixture, "transpile_parse_error")
{
    std::string code = "local a = -";

    auto result = transpile(code);
    CHECK_EQ("", result.code);
    CHECK_EQ("Expected identifier when parsing expression, got <eof>", result.parseError);
}

TEST_CASE_FIXTURE(Fixture, "transpile_declare_global_stat")
{
    std::string code = "declare _G: any";

    ParseOptions options;
    options.allowDeclarationSyntax = true;

    auto allocator = Allocator{};
    auto names = AstNameTable{allocator};
    ParseResult parseResult = Parser::parse(code.data(), code.size(), names, allocator, options);

    auto result = transpileWithTypes(*parseResult.root);

    CHECK_EQ(result, code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_to_string")
{
    std::string code = "local a: string = 'hello'";

    auto allocator = Allocator{};
    auto names = AstNameTable{allocator};
    ParseResult parseResult = Parser::parse(code.data(), code.size(), names, allocator, {});

    REQUIRE(parseResult.root);
    REQUIRE(parseResult.root->body.size == 1);
    AstStatLocal* statLocal = parseResult.root->body.data[0]->as<AstStatLocal>();
    REQUIRE(statLocal);
    CHECK_EQ("local a: string = 'hello'", toString(statLocal));
    REQUIRE(statLocal->vars.size == 1);
    AstLocal* local = statLocal->vars.data[0];
    REQUIRE(local->annotation);
    CHECK_EQ("string", toString(local->annotation));
    REQUIRE(statLocal->values.size == 1);
    AstExpr* expr = statLocal->values.data[0];
    CHECK_EQ("'hello'", toString(expr));
}

TEST_CASE_FIXTURE(Fixture, "transpile_type_alias_default_type_parameters")
{
    std::string code = R"(
type Packed<T = string, U = T, V... = ...boolean, W... = (T, U, V...)> = (T, U, V...)->(W...)
local a: Packed<number>
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_singleton_types")
{
    std::string code = R"(
type t1 = 'hello'
type t2 = true
type t3 = ''
type t4 = false
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_array_types")
{
    std::string code = R"(
type t1 = {number}
type t2 = {[string]: number}
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_for_in_multiple_types")
{
    std::string code = "for k:string,v:boolean in next,{}do end";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_string_interp")
{
    std::string code = R"( local _ = `hello {name}` )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_string_interp_multiline")
{
    std::string code = R"( local _ = `hello {
        name
    }!` )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_string_interp_on_new_line")
{
    std::string code = R"(
        error(
            `a {b} c`
        )
    )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_string_interp_multiline_escape")
{
    std::string code = R"( local _ = `hello \
        world!` )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_string_literal_escape")
{
    std::string code = R"( local _ = ` bracket = \{, backtick = \` = {'ok'} ` )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_type_functions")
{
    std::string code = R"( type function foo(arg1, arg2) if arg1 == arg2 then return arg1 end return arg2 end )";

    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_type_functions_spaces_around_tokens")
{
    std::string code = R"( type   function foo() end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type function   foo() end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type function foo  () end )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( export   type function foo() end )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE_FIXTURE(Fixture, "transpile_typeof_spaces_around_tokens")
{
    std::string code = R"( type X = typeof(x) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type X =    typeof(x) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type X = typeof   (x) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type X = typeof(   x) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type X = typeof(x   ) )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_single_quoted_string_types")
{
    const std::string code = R"( type a = 'hello world' )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_double_quoted_string_types")
{
    const std::string code = R"( type a = "hello world" )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_raw_string_types")
{
    std::string code = R"( type a = [[ hello world ]] )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type a = [==[ hello world ]==] )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_escaped_string_types")
{
    const std::string code = R"( type a = "\\b\\t\\n\\\\" )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_type_table_semicolon_separators")
{
    const std::string code = R"(
        type Foo = {
            bar: number;
            baz: number;
        }
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_type_table_access_modifiers")
{
    std::string code = R"(
        type Foo = {
            read  bar: number,
              write baz: number,
        }
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = { read string } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = {
        read [string]: number,
        read ["property"]: number
    } )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_type_table_spaces_between_tokens")
{
    std::string code = R"( type Foo = { bar: number, } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = {   bar: number, } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = { bar  : number, } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = { bar:   number, } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = { bar: number  , } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = { bar: number,   } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = { bar: number   } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = { [string]: number } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = {    [string]: number } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = { [   string]: number } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = { [string   ]: number } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = { [string]   : number } )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = { [string]:   number } )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_type_table_preserve_original_indexer_style")
{
    std::string code = R"(
        type Foo = {
            [number]: string
        }
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"(
        type Foo = { { number } }
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_type_table_preserve_indexer_location")
{
    std::string code = R"(
        type Foo = {
            [number]: string,
            property: number,
        }
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"(
        type Foo = {
            property: number,
            [number]: string,
        }
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"(
        type Foo = {
            property: number,
            [number]: string,
            property2: number,
        }
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_type_table_preserve_property_definition_style")
{
    std::string code = R"(
        type Foo = {
            ["$$typeof1"]: string,
            ['$$typeof2']: string,
        }
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_type_table_string_properties_spaces_between_tokens")
{
    std::string code = R"(
        type Foo = {
            [  "$$typeof1"]: string,
            ['$$typeof2'  ]: string,
        }
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_types_preserve_parentheses_style")
{
    std::string code = R"( type Foo = number )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (number) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = ((number)) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (  (number)  ) )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("fuzzer_transpile_with_zero_location")
{
    const std::string example = R"(
if _ then
elseif _ then
elseif l0 then
else
local function l0<t0>(...):(t0<t0...>,(any)|(<t0>((any)|(<t0>(""[[[[[[[[[[[[[[[[[[[[[[[[!*t")->()))->()))
end
end
)";

    Luau::ParseOptions parseOptions;
    parseOptions.captureComments = true;

    auto allocator = std::make_unique<Luau::Allocator>();
    auto names = std::make_unique<Luau::AstNameTable>(*allocator);
    ParseResult parseResult = Parser::parse(example.data(), example.size(), *names, *allocator, parseOptions);

    transpileWithTypes(*parseResult.root);
}

TEST_CASE("transpile_type_function_unnamed_arguments")
{
    std::string code = R"( type Foo = () -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo =   () -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (string) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (string, number) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (  string, number) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (string  , number) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (string,   number) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (string, number  ) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (string, number)   -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (string, number) ->   ()  )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_type_function_named_arguments")
{
    std::string code = R"( type Foo = (x: string) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (x: string, y: number) -> ()  )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (  x: string, y: number) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (x  : string, y: number) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (x:   string, y: number) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (x: string,   y: number) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (number, info: string) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = (first: string, second: string, ...string) -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_type_function_generics")
{
    std::string code = R"( type Foo = <X, Y, Z...>() -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo =   <X, Y, Z...>() -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = <  X, Y, Z...>() -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = <X  , Y, Z...>() -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = <X,   Y, Z...>() -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = <X, Y  , Z...>() -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = <X, Y,   Z...>() -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = <X, Y, Z  ...>() -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = <X, Y, Z...  >() -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = <X, Y, Z...>  () -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_type_function_return_types")
{
    std::string code = R"( type Foo = () ->   () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () -> (  ) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () -> string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () ->   string )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () -> (string) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () ->   (string) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () -> ...any )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () ->   ...any )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () -> ...  any )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () -> (...any) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () -> (  string, number) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () -> (string  , number) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () -> (string,   number) )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () -> (string, number  ) )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_chained_function_types")
{
    std::string code = R"( type Foo = () -> () -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () -> ()   -> () )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"( type Foo = () -> () ->   () )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("fuzzer_nil_optional")
{
    const std::string code = R"( local x: nil? )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_CASE("transpile_function_attributes")
{
    std::string code = R"(
        @native
        function foo()
        end
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"(
        @native
        local function foo()
        end
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"(
        @checked local function foo()
        end
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"(
        local foo = @native function() end
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"(
        @native
        function foo:bar()
        end
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);

    code = R"(
        @native   @checked
        function foo:bar()
        end
    )";
    CHECK_EQ(code, transpile(code, {}, true).code);
}

TEST_SUITE_END();
