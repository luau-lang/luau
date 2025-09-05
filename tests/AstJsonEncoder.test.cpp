// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Ast.h"
#include "Luau/AstJsonEncoder.h"
#include "Luau/Parser.h"
#include "ScopedFlags.h"

#include "doctest.h"

#include <math.h>
#include <ostream>

using namespace Luau;

struct JsonEncoderFixture
{
    Allocator allocator;
    AstNameTable names{allocator};

    ParseResult parse(std::string_view src)
    {
        ParseOptions opts;
        opts.allowDeclarationSyntax = true;
        return Parser::parse(src.data(), src.size(), names, allocator, opts);
    }

    AstStatBlock* expectParse(std::string_view src)
    {
        ParseResult res = parse(src);
        REQUIRE(res.errors.size() == 0);
        return res.root;
    }

    AstStat* expectParseStatement(std::string_view src)
    {
        AstStatBlock* root = expectParse(src);
        REQUIRE(1 == root->body.size);
        return root->body.data[0];
    }

    AstExpr* expectParseExpr(std::string_view src)
    {
        std::string s = "a = ";
        s.append(src);
        AstStatBlock* root = expectParse(s);

        AstStatAssign* statAssign = root->body.data[0]->as<AstStatAssign>();
        REQUIRE(statAssign != nullptr);
        REQUIRE(statAssign->values.size == 1);

        return statAssign->values.data[0];
    }
};

TEST_SUITE_BEGIN("JsonEncoderTests");

TEST_CASE("encode_constants")
{
    AstExprConstantNil nil{Location()};
    AstExprConstantBool b{Location(), true};
    AstExprConstantNumber n{Location(), 8.2};
    AstExprConstantNumber bigNum{Location(), 0.1677721600000003};
    AstExprConstantNumber positiveInfinity{Location(), INFINITY};
    AstExprConstantNumber negativeInfinity{Location(), -INFINITY};
    AstExprConstantNumber nan{Location(), NAN};

    AstArray<char> charString;
    charString.data = const_cast<char*>("a\x1d\0\\\"b");
    charString.size = 6;

    AstExprConstantString needsEscaping{Location(), charString, AstExprConstantString::QuotedSimple};

    CHECK_EQ(R"({"type":"AstExprConstantNil","location":"0,0 - 0,0"})", toJson(&nil));
    CHECK_EQ(R"({"type":"AstExprConstantBool","location":"0,0 - 0,0","value":true})", toJson(&b));
    CHECK_EQ(R"({"type":"AstExprConstantNumber","location":"0,0 - 0,0","value":8.1999999999999993})", toJson(&n));
    CHECK_EQ(R"({"type":"AstExprConstantNumber","location":"0,0 - 0,0","value":0.16777216000000031})", toJson(&bigNum));
    CHECK_EQ(R"({"type":"AstExprConstantNumber","location":"0,0 - 0,0","value":Infinity})", toJson(&positiveInfinity));
    CHECK_EQ(R"({"type":"AstExprConstantNumber","location":"0,0 - 0,0","value":-Infinity})", toJson(&negativeInfinity));
    CHECK_EQ(R"({"type":"AstExprConstantNumber","location":"0,0 - 0,0","value":NaN})", toJson(&nan));
    CHECK_EQ("{\"type\":\"AstExprConstantString\",\"location\":\"0,0 - 0,0\",\"value\":\"a\\u001d\\u0000\\\\\\\"b\"}", toJson(&needsEscaping));
}

TEST_CASE("basic_escaping")
{
    std::string s = "hello \"world\"";
    AstArray<char> theString{s.data(), s.size()};
    AstExprConstantString str{Location(), theString, AstExprConstantString::QuotedSimple};

    std::string expected = R"({"type":"AstExprConstantString","location":"0,0 - 0,0","value":"hello \"world\""})";
    CHECK_EQ(expected, toJson(&str));
}

TEST_CASE("encode_AstStatBlock")
{
    AstLocal astlocal{AstName{"a_local"}, Location(), nullptr, 0, 0, nullptr};
    AstLocal* astlocalarray[] = {&astlocal};

    AstArray<AstLocal*> vars{astlocalarray, 1};
    AstArray<AstExpr*> values{nullptr, 0};
    AstStatLocal local{Location(), vars, values, std::nullopt};
    AstStat* statArray[] = {&local};

    AstArray<AstStat*> bodyArray{statArray, 1};

    AstStatBlock block{Location(), bodyArray};

    CHECK(
        toJson(&block) ==
        (R"({"type":"AstStatBlock","location":"0,0 - 0,0","hasEnd":true,"body":[{"type":"AstStatLocal","location":"0,0 - 0,0","vars":[{"luauType":null,"name":"a_local","type":"AstLocal","location":"0,0 - 0,0"}],"values":[]}]})")
    );
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_tables")
{
    std::string src = R"(
        local x: {
            foo: number
        } = {
            foo = 123,
        }
    )";

    AstStatBlock* root = expectParse(src);
    std::string json = toJson(root);

    CHECK(
        json ==
        R"({"type":"AstStatBlock","location":"0,0 - 6,4","hasEnd":true,"body":[{"type":"AstStatLocal","location":"1,8 - 5,9","vars":[{"luauType":{"type":"AstTypeTable","location":"1,17 - 3,9","props":[{"name":"foo","type":"AstTableProp","location":"2,12 - 2,15","propType":{"type":"AstTypeReference","location":"2,17 - 2,23","name":"number","nameLocation":"2,17 - 2,23","parameters":[]}}],"indexer":null},"name":"x","type":"AstLocal","location":"1,14 - 1,15"}],"values":[{"type":"AstExprTable","location":"3,12 - 5,9","items":[{"type":"AstExprTableItem","kind":"record","key":{"type":"AstExprConstantString","location":"4,12 - 4,15","value":"foo"},"value":{"type":"AstExprConstantNumber","location":"4,18 - 4,21","value":123}}]}]}]})"
    );
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_table_array")
{
    std::string src = R"(type X = {string})";

    AstStatBlock* root = expectParse(src);
    std::string json = toJson(root);

    CHECK(
        json ==
        R"({"type":"AstStatBlock","location":"0,0 - 0,17","hasEnd":true,"body":[{"type":"AstStatTypeAlias","location":"0,0 - 0,17","name":"X","generics":[],"genericPacks":[],"value":{"type":"AstTypeTable","location":"0,9 - 0,17","props":[],"indexer":{"location":"0,10 - 0,16","indexType":{"type":"AstTypeReference","location":"0,10 - 0,16","name":"number","nameLocation":"0,10 - 0,16","parameters":[]},"resultType":{"type":"AstTypeReference","location":"0,10 - 0,16","name":"string","nameLocation":"0,10 - 0,16","parameters":[]}}},"exported":false}]})"
    );
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_table_indexer")
{
    std::string src = R"(type X = {string})";

    AstStatBlock* root = expectParse(src);
    std::string json = toJson(root);

    CHECK(
        json ==
        R"({"type":"AstStatBlock","location":"0,0 - 0,17","hasEnd":true,"body":[{"type":"AstStatTypeAlias","location":"0,0 - 0,17","name":"X","generics":[],"genericPacks":[],"value":{"type":"AstTypeTable","location":"0,9 - 0,17","props":[],"indexer":{"location":"0,10 - 0,16","indexType":{"type":"AstTypeReference","location":"0,10 - 0,16","name":"number","nameLocation":"0,10 - 0,16","parameters":[]},"resultType":{"type":"AstTypeReference","location":"0,10 - 0,16","name":"string","nameLocation":"0,10 - 0,16","parameters":[]}}},"exported":false}]})"
    );
}

TEST_CASE("encode_AstExprGroup")
{
    AstExprConstantNumber number{Location{}, 5.0};
    AstExprGroup group{Location{}, &number};

    std::string json = toJson(&group);

    const std::string expected =
        R"({"type":"AstExprGroup","location":"0,0 - 0,0","expr":{"type":"AstExprConstantNumber","location":"0,0 - 0,0","value":5}})";

    CHECK(json == expected);
}

TEST_CASE("encode_AstExprGlobal")
{
    AstExprGlobal global{Location{}, AstName{"print"}};

    std::string json = toJson(&global);
    std::string expected = R"({"type":"AstExprGlobal","location":"0,0 - 0,0","global":"print"})";

    CHECK(json == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprIfThen")
{
    AstStat* statement = expectParseStatement("local a = if x then y else z");

    std::string_view expected =
        R"({"type":"AstStatLocal","location":"0,0 - 0,28","vars":[{"luauType":null,"name":"a","type":"AstLocal","location":"0,6 - 0,7"}],"values":[{"type":"AstExprIfElse","location":"0,10 - 0,28","condition":{"type":"AstExprGlobal","location":"0,13 - 0,14","global":"x"},"hasThen":true,"trueExpr":{"type":"AstExprGlobal","location":"0,20 - 0,21","global":"y"},"hasElse":true,"falseExpr":{"type":"AstExprGlobal","location":"0,27 - 0,28","global":"z"}}]})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprInterpString")
{
    AstStat* statement = expectParseStatement("local a = `var = {x}`");

    std::string_view expected =
        R"({"type":"AstStatLocal","location":"0,0 - 0,21","vars":[{"luauType":null,"name":"a","type":"AstLocal","location":"0,6 - 0,7"}],"values":[{"type":"AstExprInterpString","location":"0,10 - 0,21","strings":["var = ",""],"expressions":[{"type":"AstExprGlobal","location":"0,18 - 0,19","global":"x"}]}]})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE("encode_AstExprLocal")
{
    AstLocal local{AstName{"foo"}, Location{}, nullptr, 0, 0, nullptr};
    AstExprLocal exprLocal{Location{}, &local, false};

    CHECK(
        toJson(&exprLocal) ==
        R"({"type":"AstExprLocal","location":"0,0 - 0,0","local":{"luauType":null,"name":"foo","type":"AstLocal","location":"0,0 - 0,0"}})"
    );
}

TEST_CASE("encode_AstExprVarargs")
{
    AstExprVarargs varargs{Location{}};

    CHECK(toJson(&varargs) == R"({"type":"AstExprVarargs","location":"0,0 - 0,0"})");
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprCall")
{
    AstExpr* expr = expectParseExpr("foo(1, 2, 3)");
    std::string_view expected =
        R"({"type":"AstExprCall","location":"0,4 - 0,16","func":{"type":"AstExprGlobal","location":"0,4 - 0,7","global":"foo"},"args":[{"type":"AstExprConstantNumber","location":"0,8 - 0,9","value":1},{"type":"AstExprConstantNumber","location":"0,11 - 0,12","value":2},{"type":"AstExprConstantNumber","location":"0,14 - 0,15","value":3}],"self":false,"argLocation":"0,8 - 0,16"})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprIndexName")
{
    AstExpr* expr = expectParseExpr("foo.bar");

    std::string_view expected =
        R"({"type":"AstExprIndexName","location":"0,4 - 0,11","expr":{"type":"AstExprGlobal","location":"0,4 - 0,7","global":"foo"},"index":"bar","indexLocation":"0,8 - 0,11","op":"."})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprIndexExpr")
{
    AstExpr* expr = expectParseExpr("foo['bar']");

    std::string_view expected =
        R"({"type":"AstExprIndexExpr","location":"0,4 - 0,14","expr":{"type":"AstExprGlobal","location":"0,4 - 0,7","global":"foo"},"index":{"type":"AstExprConstantString","location":"0,8 - 0,13","value":"bar"}})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprFunction")
{
    AstExpr* expr = expectParseExpr("function (a) return a end");

    std::string_view expected =
        R"({"type":"AstExprFunction","location":"0,4 - 0,29","attributes":[],"generics":[],"genericPacks":[],"args":[{"luauType":null,"name":"a","type":"AstLocal","location":"0,14 - 0,15"}],"vararg":false,"varargLocation":"0,0 - 0,0","body":{"type":"AstStatBlock","location":"0,16 - 0,26","hasEnd":true,"body":[{"type":"AstStatReturn","location":"0,17 - 0,25","list":[{"type":"AstExprLocal","location":"0,24 - 0,25","local":{"luauType":null,"name":"a","type":"AstLocal","location":"0,14 - 0,15"}}]}]},"functionDepth":1,"debugname":""})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprTable")
{
    AstExpr* expr = expectParseExpr("{true, key=true, [key2]=true}");

    std::string_view expected =
        R"({"type":"AstExprTable","location":"0,4 - 0,33","items":[{"type":"AstExprTableItem","kind":"item","value":{"type":"AstExprConstantBool","location":"0,5 - 0,9","value":true}},{"type":"AstExprTableItem","kind":"record","key":{"type":"AstExprConstantString","location":"0,11 - 0,14","value":"key"},"value":{"type":"AstExprConstantBool","location":"0,15 - 0,19","value":true}},{"type":"AstExprTableItem","kind":"general","key":{"type":"AstExprGlobal","location":"0,22 - 0,26","global":"key2"},"value":{"type":"AstExprConstantBool","location":"0,28 - 0,32","value":true}}]})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprUnary")
{
    AstExpr* expr = expectParseExpr("-b");

    std::string_view expected =
        R"({"type":"AstExprUnary","location":"0,4 - 0,6","op":"Minus","expr":{"type":"AstExprGlobal","location":"0,5 - 0,6","global":"b"}})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprBinary")
{
    AstExpr* expr = expectParseExpr("b + c");

    std::string_view expected =
        R"({"type":"AstExprBinary","location":"0,4 - 0,9","op":"Add","left":{"type":"AstExprGlobal","location":"0,4 - 0,5","global":"b"},"right":{"type":"AstExprGlobal","location":"0,8 - 0,9","global":"c"}})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprTypeAssertion")
{
    AstExpr* expr = expectParseExpr("b :: any");

    std::string_view expected =
        R"({"type":"AstExprTypeAssertion","location":"0,4 - 0,12","expr":{"type":"AstExprGlobal","location":"0,4 - 0,5","global":"b"},"annotation":{"type":"AstTypeReference","location":"0,9 - 0,12","name":"any","nameLocation":"0,9 - 0,12","parameters":[]}})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprError")
{
    std::string_view src = "a = ";
    ParseResult parseResult = Parser::parse(src.data(), src.size(), names, allocator);

    REQUIRE(1 == parseResult.root->body.size);

    AstStatAssign* statAssign = parseResult.root->body.data[0]->as<AstStatAssign>();
    REQUIRE(statAssign != nullptr);
    REQUIRE(1 == statAssign->values.size);

    AstExpr* expr = statAssign->values.data[0];

    std::string_view expected = R"({"type":"AstExprError","location":"0,4 - 0,4","expressions":[],"messageIndex":0})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatIf")
{
    AstStat* statement = expectParseStatement("if true then else end");

    std::string_view expected =
        R"({"type":"AstStatIf","location":"0,0 - 0,21","condition":{"type":"AstExprConstantBool","location":"0,3 - 0,7","value":true},"thenbody":{"type":"AstStatBlock","location":"0,12 - 0,13","hasEnd":true,"body":[]},"elsebody":{"type":"AstStatBlock","location":"0,17 - 0,18","hasEnd":true,"body":[]},"hasThen":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatWhile")
{
    AstStat* statement = expectParseStatement("while true do end");

    std::string_view expected =
        R"({"type":"AstStatWhile","location":"0,0 - 0,17","condition":{"type":"AstExprConstantBool","location":"0,6 - 0,10","value":true},"body":{"type":"AstStatBlock","location":"0,13 - 0,14","hasEnd":true,"body":[]},"hasDo":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatRepeat")
{
    AstStat* statement = expectParseStatement("repeat until true");

    std::string_view expected =
        R"({"type":"AstStatRepeat","location":"0,0 - 0,17","condition":{"type":"AstExprConstantBool","location":"0,13 - 0,17","value":true},"body":{"type":"AstStatBlock","location":"0,6 - 0,7","hasEnd":true,"body":[]}})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatBreak")
{
    AstStat* statement = expectParseStatement("while true do break end");

    std::string_view expected =
        R"({"type":"AstStatWhile","location":"0,0 - 0,23","condition":{"type":"AstExprConstantBool","location":"0,6 - 0,10","value":true},"body":{"type":"AstStatBlock","location":"0,13 - 0,20","hasEnd":true,"body":[{"type":"AstStatBreak","location":"0,14 - 0,19"}]},"hasDo":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatContinue")
{
    AstStat* statement = expectParseStatement("while true do continue end");

    std::string_view expected =
        R"({"type":"AstStatWhile","location":"0,0 - 0,26","condition":{"type":"AstExprConstantBool","location":"0,6 - 0,10","value":true},"body":{"type":"AstStatBlock","location":"0,13 - 0,23","hasEnd":true,"body":[{"type":"AstStatContinue","location":"0,14 - 0,22"}]},"hasDo":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatFor")
{
    AstStat* statement = expectParseStatement("for a=0,1 do end");

    std::string_view expected =
        R"({"type":"AstStatFor","location":"0,0 - 0,16","var":{"luauType":null,"name":"a","type":"AstLocal","location":"0,4 - 0,5"},"from":{"type":"AstExprConstantNumber","location":"0,6 - 0,7","value":0},"to":{"type":"AstExprConstantNumber","location":"0,8 - 0,9","value":1},"body":{"type":"AstStatBlock","location":"0,12 - 0,13","hasEnd":true,"body":[]},"hasDo":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatForIn")
{
    AstStat* statement = expectParseStatement("for a in b do end");

    std::string_view expected =
        R"({"type":"AstStatForIn","location":"0,0 - 0,17","vars":[{"luauType":null,"name":"a","type":"AstLocal","location":"0,4 - 0,5"}],"values":[{"type":"AstExprGlobal","location":"0,9 - 0,10","global":"b"}],"body":{"type":"AstStatBlock","location":"0,13 - 0,14","hasEnd":true,"body":[]},"hasIn":true,"hasDo":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatCompoundAssign")
{
    AstStat* statement = expectParseStatement("a += b");

    std::string_view expected =
        R"({"type":"AstStatCompoundAssign","location":"0,0 - 0,6","op":"Add","var":{"type":"AstExprGlobal","location":"0,0 - 0,1","global":"a"},"value":{"type":"AstExprGlobal","location":"0,5 - 0,6","global":"b"}})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatLocalFunction")
{
    AstStat* statement = expectParseStatement("local function a(b) return end");

    std::string_view expected =
        R"({"type":"AstStatLocalFunction","location":"0,0 - 0,30","name":{"luauType":null,"name":"a","type":"AstLocal","location":"0,15 - 0,16"},"func":{"type":"AstExprFunction","location":"0,0 - 0,30","attributes":[],"generics":[],"genericPacks":[],"args":[{"luauType":null,"name":"b","type":"AstLocal","location":"0,17 - 0,18"}],"vararg":false,"varargLocation":"0,0 - 0,0","body":{"type":"AstStatBlock","location":"0,19 - 0,27","hasEnd":true,"body":[{"type":"AstStatReturn","location":"0,20 - 0,26","list":[]}]},"functionDepth":1,"debugname":"a"}})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatTypeAlias")
{
    AstStat* statement = expectParseStatement("type A = B");

    std::string_view expected =
        R"({"type":"AstStatTypeAlias","location":"0,0 - 0,10","name":"A","generics":[],"genericPacks":[],"value":{"type":"AstTypeReference","location":"0,9 - 0,10","name":"B","nameLocation":"0,9 - 0,10","parameters":[]},"exported":false})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatDeclareFunction")
{
    AstStat* statement = expectParseStatement("declare function foo(x: number): string");

    std::string_view expected =
        R"({"type":"AstStatDeclareFunction","location":"0,0 - 0,39","attributes":[],"name":"foo","nameLocation":"0,17 - 0,20","params":{"type":"AstTypeList","types":[{"type":"AstTypeReference","location":"0,24 - 0,30","name":"number","nameLocation":"0,24 - 0,30","parameters":[]}]},"paramNames":[{"type":"AstArgumentName","name":"x","location":"0,21 - 0,22"}],"vararg":false,"varargLocation":"0,0 - 0,0","retTypes":{"type":"AstTypePackExplicit","location":"0,33 - 0,39","typeList":{"type":"AstTypeList","types":[{"type":"AstTypeReference","location":"0,33 - 0,39","name":"string","nameLocation":"0,33 - 0,39","parameters":[]}]}},"generics":[],"genericPacks":[]})";
    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatDeclareFunction2")
{
    AstStat* statement = expectParseStatement("declare function foo(x: number, ...: string): string");

    std::string_view expected =
        R"({"type":"AstStatDeclareFunction","location":"0,0 - 0,52","attributes":[],"name":"foo","nameLocation":"0,17 - 0,20","params":{"type":"AstTypeList","types":[{"type":"AstTypeReference","location":"0,24 - 0,30","name":"number","nameLocation":"0,24 - 0,30","parameters":[]}],"tailType":{"type":"AstTypePackVariadic","location":"0,37 - 0,43","variadicType":{"type":"AstTypeReference","location":"0,37 - 0,43","name":"string","nameLocation":"0,37 - 0,43","parameters":[]}}},"paramNames":[{"type":"AstArgumentName","name":"x","location":"0,21 - 0,22"}],"vararg":true,"varargLocation":"0,32 - 0,35","retTypes":{"type":"AstTypePackExplicit","location":"0,46 - 0,52","typeList":{"type":"AstTypeList","types":[{"type":"AstTypeReference","location":"0,46 - 0,52","name":"string","nameLocation":"0,46 - 0,52","parameters":[]}]}},"generics":[],"genericPacks":[]})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstAttr")
{
    AstStat* expr = expectParseStatement("@checked function a(b) return c end");

    std::string_view expected =
        R"({"type":"AstStatFunction","location":"0,0 - 0,35","name":{"type":"AstExprGlobal","location":"0,18 - 0,19","global":"a"},"func":{"type":"AstExprFunction","location":"0,0 - 0,35","attributes":[{"type":"AstAttr","location":"0,0 - 0,8","name":"checked"}],"generics":[],"genericPacks":[],"args":[{"luauType":null,"name":"b","type":"AstLocal","location":"0,20 - 0,21"}],"vararg":false,"varargLocation":"0,0 - 0,0","body":{"type":"AstStatBlock","location":"0,22 - 0,32","hasEnd":true,"body":[{"type":"AstStatReturn","location":"0,23 - 0,31","list":[{"type":"AstExprGlobal","location":"0,30 - 0,31","global":"c"}]}]},"functionDepth":1,"debugname":"a"}})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatDeclareClass")
{
    AstStatBlock* root = expectParse(R"(
        declare class Foo
            prop: number
            function method(self, foo: number): string
        end

        declare class Bar extends Foo
            prop2: string
        end
    )");

    REQUIRE(2 == root->body.size);

    std::string_view expected1 =
        R"({"type":"AstStatDeclareClass","location":"1,22 - 4,11","name":"Foo","props":[{"name":"prop","nameLocation":"2,12 - 2,16","type":"AstDeclaredClassProp","luauType":{"type":"AstTypeReference","location":"2,18 - 2,24","name":"number","nameLocation":"2,18 - 2,24","parameters":[]},"location":"2,12 - 2,24"},{"name":"method","nameLocation":"3,21 - 3,27","type":"AstDeclaredClassProp","luauType":{"type":"AstTypeFunction","location":"3,12 - 3,54","attributes":[],"generics":[],"genericPacks":[],"argTypes":{"type":"AstTypeList","types":[{"type":"AstTypeReference","location":"3,39 - 3,45","name":"number","nameLocation":"3,39 - 3,45","parameters":[]}]},"argNames":[{"type":"AstArgumentName","name":"foo","location":"3,34 - 3,37"}],"returnTypes":{"type":"AstTypePackExplicit","location":"3,48 - 3,54","typeList":{"type":"AstTypeList","types":[{"type":"AstTypeReference","location":"3,48 - 3,54","name":"string","nameLocation":"3,48 - 3,54","parameters":[]}]}}},"location":"3,12 - 3,54"}],"indexer":null})";
    CHECK(toJson(root->body.data[0]) == expected1);

    std::string_view expected2 =
        R"({"type":"AstStatDeclareClass","location":"6,22 - 8,11","name":"Bar","superName":"Foo","props":[{"name":"prop2","nameLocation":"7,12 - 7,17","type":"AstDeclaredClassProp","luauType":{"type":"AstTypeReference","location":"7,19 - 7,25","name":"string","nameLocation":"7,19 - 7,25","parameters":[]},"location":"7,12 - 7,25"}],"indexer":null})";
    CHECK(toJson(root->body.data[1]) == expected2);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_annotation")
{
    AstStat* statement = expectParseStatement("type T = ((number) -> (string | nil)) & ((string) -> ())");

    std::string_view expected =
        R"({"type":"AstStatTypeAlias","location":"0,0 - 0,56","name":"T","generics":[],"genericPacks":[],"value":{"type":"AstTypeIntersection","location":"0,9 - 0,56","types":[{"type":"AstTypeGroup","location":"0,9 - 0,37","inner":{"type":"AstTypeFunction","location":"0,10 - 0,36","attributes":[],"generics":[],"genericPacks":[],"argTypes":{"type":"AstTypeList","types":[{"type":"AstTypeReference","location":"0,11 - 0,17","name":"number","nameLocation":"0,11 - 0,17","parameters":[]}]},"argNames":[],"returnTypes":{"type":"AstTypePackExplicit","location":"0,22 - 0,36","typeList":{"type":"AstTypeList","types":[{"type":"AstTypeGroup","location":"0,22 - 0,36","inner":{"type":"AstTypeUnion","location":"0,23 - 0,35","types":[{"type":"AstTypeReference","location":"0,23 - 0,29","name":"string","nameLocation":"0,23 - 0,29","parameters":[]},{"type":"AstTypeReference","location":"0,32 - 0,35","name":"nil","nameLocation":"0,32 - 0,35","parameters":[]}]}}]}}}},{"type":"AstTypeGroup","location":"0,40 - 0,56","inner":{"type":"AstTypeFunction","location":"0,41 - 0,55","attributes":[],"generics":[],"genericPacks":[],"argTypes":{"type":"AstTypeList","types":[{"type":"AstTypeReference","location":"0,42 - 0,48","name":"string","nameLocation":"0,42 - 0,48","parameters":[]}]},"argNames":[],"returnTypes":{"type":"AstTypePackExplicit","location":"0,53 - 0,55","typeList":{"type":"AstTypeList","types":[]}}}}]},"exported":false})";
    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_type_literal")
{
    AstStat* statement = expectParseStatement(R"(type Action = { strings: "A" | "B" | "C", mixed: "This" | "That" | true })");

    auto json = toJson(statement);

    std::string_view expected =
        R"({"type":"AstStatTypeAlias","location":"0,0 - 0,73","name":"Action","generics":[],"genericPacks":[],"value":{"type":"AstTypeTable","location":"0,14 - 0,73","props":[{"name":"strings","type":"AstTableProp","location":"0,16 - 0,23","propType":{"type":"AstTypeUnion","location":"0,25 - 0,40","types":[{"type":"AstTypeSingletonString","location":"0,25 - 0,28","value":"A"},{"type":"AstTypeSingletonString","location":"0,31 - 0,34","value":"B"},{"type":"AstTypeSingletonString","location":"0,37 - 0,40","value":"C"}]}},{"name":"mixed","type":"AstTableProp","location":"0,42 - 0,47","propType":{"type":"AstTypeUnion","location":"0,49 - 0,71","types":[{"type":"AstTypeSingletonString","location":"0,49 - 0,55","value":"This"},{"type":"AstTypeSingletonString","location":"0,58 - 0,64","value":"That"},{"type":"AstTypeSingletonBool","location":"0,67 - 0,71","value":true}]}}],"indexer":null},"exported":false})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_indexed_type_literal")
{
    AstStat* statement = expectParseStatement(R"(type StringSet = { [string]: true })");

    std::string_view expected =
        R"({"type":"AstStatTypeAlias","location":"0,0 - 0,35","name":"StringSet","generics":[],"genericPacks":[],"value":{"type":"AstTypeTable","location":"0,17 - 0,35","props":[],"indexer":{"location":"0,19 - 0,33","indexType":{"type":"AstTypeReference","location":"0,20 - 0,26","name":"string","nameLocation":"0,20 - 0,26","parameters":[]},"resultType":{"type":"AstTypeSingletonBool","location":"0,29 - 0,33","value":true}}},"exported":false})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstTypeFunction")
{
    AstStat* statement = expectParseStatement(R"(type fun = (string, bool, named: number) -> ())");

    std::string_view expected =
        R"({"type":"AstStatTypeAlias","location":"0,0 - 0,46","name":"fun","generics":[],"genericPacks":[],"value":{"type":"AstTypeFunction","location":"0,11 - 0,46","attributes":[],"generics":[],"genericPacks":[],"argTypes":{"type":"AstTypeList","types":[{"type":"AstTypeReference","location":"0,12 - 0,18","name":"string","nameLocation":"0,12 - 0,18","parameters":[]},{"type":"AstTypeReference","location":"0,20 - 0,24","name":"bool","nameLocation":"0,20 - 0,24","parameters":[]},{"type":"AstTypeReference","location":"0,33 - 0,39","name":"number","nameLocation":"0,33 - 0,39","parameters":[]}]},"argNames":[null,null,{"type":"AstArgumentName","name":"named","location":"0,26 - 0,31"}],"returnTypes":{"type":"AstTypePackExplicit","location":"0,44 - 0,46","typeList":{"type":"AstTypeList","types":[]}}},"exported":false})";
    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstTypeError")
{
    ParseResult parseResult = parse("type T = ");
    REQUIRE(1 == parseResult.root->body.size);

    AstStat* statement = parseResult.root->body.data[0];

    std::string_view expected =
        R"({"type":"AstStatTypeAlias","location":"0,0 - 0,9","name":"T","generics":[],"genericPacks":[],"value":{"type":"AstTypeError","location":"0,8 - 0,9","types":[],"messageIndex":0},"exported":false})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstTypePackExplicit")
{
    AstStatBlock* root = expectParse(R"(
        type A<T...> = () -> T...
        local a: A<(number, string)>
    )");

    CHECK(2 == root->body.size);

    std::string_view expected =
        R"({"type":"AstStatLocal","location":"2,8 - 2,36","vars":[{"luauType":{"type":"AstTypeReference","location":"2,17 - 2,36","name":"A","nameLocation":"2,17 - 2,18","parameters":[{"type":"AstTypePackExplicit","location":"2,19 - 2,20","typeList":{"type":"AstTypeList","types":[{"type":"AstTypeReference","location":"2,20 - 2,26","name":"number","nameLocation":"2,20 - 2,26","parameters":[]},{"type":"AstTypeReference","location":"2,28 - 2,34","name":"string","nameLocation":"2,28 - 2,34","parameters":[]}]}}]},"name":"a","type":"AstLocal","location":"2,14 - 2,15"}],"values":[]})";

    CHECK(toJson(root->body.data[1]) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstGenericType")
{
    AstStatBlock* root = expectParse(R"(
        a = function<b, c>()
        end
    )");

    CHECK(1 == root->body.size);

    std::string_view expected =
        R"({"type":"AstStatAssign","location":"1,8 - 2,11","vars":[{"type":"AstExprGlobal","location":"1,8 - 1,9","global":"a"}],"values":[{"type":"AstExprFunction","location":"1,12 - 2,11","attributes":[],"generics":[{"type":"AstGenericType","name":"b"},{"type":"AstGenericType","name":"c"}],"genericPacks":[],"args":[],"vararg":false,"varargLocation":"0,0 - 0,0","body":{"type":"AstStatBlock","location":"1,28 - 2,8","hasEnd":true,"body":[]},"functionDepth":1,"debugname":""}]})";

    CHECK(toJson(root->body.data[0]) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstGenericTypeWithDefault")
{
    AstStatBlock* root = expectParse(R"(
        type Foo<X = string> = X
    )");

    CHECK(1 == root->body.size);

    std::string_view expected =
        R"({"type":"AstStatTypeAlias","location":"1,8 - 1,32","name":"Foo","generics":[{"type":"AstGenericType","name":"X","luauType":{"type":"AstTypeReference","location":"1,21 - 1,27","name":"string","nameLocation":"1,21 - 1,27","parameters":[]}}],"genericPacks":[],"value":{"type":"AstTypeReference","location":"1,31 - 1,32","name":"X","nameLocation":"1,31 - 1,32","parameters":[]},"exported":false})";

    CHECK(toJson(root->body.data[0]) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstGenericTypePack")
{
    AstStatBlock* root = expectParse(R"(
        a = function<b..., c...>()
        end
    )");

    CHECK(1 == root->body.size);

    std::string_view expected =
        R"({"type":"AstStatAssign","location":"1,8 - 2,11","vars":[{"type":"AstExprGlobal","location":"1,8 - 1,9","global":"a"}],"values":[{"type":"AstExprFunction","location":"1,12 - 2,11","attributes":[],"generics":[],"genericPacks":[{"type":"AstGenericTypePack","name":"b"},{"type":"AstGenericTypePack","name":"c"}],"args":[],"vararg":false,"varargLocation":"0,0 - 0,0","body":{"type":"AstStatBlock","location":"1,34 - 2,8","hasEnd":true,"body":[]},"functionDepth":1,"debugname":""}]})";

    CHECK(toJson(root->body.data[0]) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstGenericTypePackWithDefault")
{
    AstStatBlock* root = expectParse(R"(
        type Foo<X... = ...string> = any
    )");

    CHECK(1 == root->body.size);

    std::string_view expected =
        R"({"type":"AstStatTypeAlias","location":"1,8 - 1,40","name":"Foo","generics":[],"genericPacks":[{"type":"AstGenericTypePack","name":"X","luauType":{"type":"AstTypePackVariadic","location":"1,24 - 1,33","variadicType":{"type":"AstTypeReference","location":"1,27 - 1,33","name":"string","nameLocation":"1,27 - 1,33","parameters":[]}}}],"value":{"type":"AstTypeReference","location":"1,37 - 1,40","name":"any","nameLocation":"1,37 - 1,40","parameters":[]},"exported":false})";

    CHECK(toJson(root->body.data[0]) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstTypeOptional")
{
    AstStatBlock* root = expectParse(R"(
            type Foo = string?
        )");

    CHECK(1 == root->body.size);

    std::string_view expected =
        R"({"type":"AstStatTypeAlias","location":"1,12 - 1,30","name":"Foo","generics":[],"genericPacks":[],"value":{"type":"AstTypeUnion","location":"1,23 - 1,30","types":[{"type":"AstTypeReference","location":"1,23 - 1,29","name":"string","nameLocation":"1,23 - 1,29","parameters":[]},{"type":"AstTypeOptional","location":"1,29 - 1,30"}]},"exported":false})";

    CHECK(toJson(root->body.data[0]) == expected);
}

TEST_SUITE_END();
