// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Ast.h"
#include "Luau/JsonEncoder.h"
#include "Luau/Parser.h"

#include "doctest.h"

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

    CHECK_EQ(R"({"type":"AstExprConstantNil","location":"0,0 - 0,0"})", toJson(&nil));
    CHECK_EQ(R"({"type":"AstExprConstantBool","location":"0,0 - 0,0","value":true})", toJson(&b));
    CHECK_EQ(R"({"type":"AstExprConstantNumber","location":"0,0 - 0,0","value":8.2})", toJson(&n));
}

TEST_CASE("basic_escaping")
{
    std::string s = "hello \"world\"";
    AstArray<char> theString{s.data(), s.size()};
    AstExprConstantString str{Location(), theString};

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

    CHECK_EQ(
        (R"({"type":"AstStatBlock","location":"0,0 - 0,0","body":[{"type":"AstStatLocal","location":"0,0 - 0,0","vars":[{"type":null,"name":"a_local","location":"0,0 - 0,0"}],"values":[]}]})"),
        toJson(&block));
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
        R"({"type":"AstStatBlock","location":"0,0 - 6,4","body":[{"type":"AstStatLocal","location":"1,8 - 5,9","vars":[{"type":{"type":"AstTypeTable","location":"1,17 - 3,9","props":[{"name":"foo","location":"2,12 - 2,15","type":{"type":"AstTypeReference","location":"2,17 - 2,23","name":"number","parameters":[]}}],"indexer":false},"name":"x","location":"1,14 - 1,15"}],"values":[{"type":"AstExprTable","location":"3,12 - 5,9","items":[{"kind":"record","key":{"type":"AstExprConstantString","location":"4,12 - 4,15","value":"foo"},"value":{"type":"AstExprConstantNumber","location":"4,18 - 4,21","value":123}}]}]}]})");
}

TEST_CASE("encode_AstExprGroup")
{
    AstExprConstantNumber number{Location{}, 5.0};
    AstExprGroup group{Location{}, &number};

    std::string json = toJson(&group);

    const std::string expected = R"({"type":"AstExprGroup","location":"0,0 - 0,0","expr":{"type":"AstExprConstantNumber","location":"0,0 - 0,0","value":5}})";

    CHECK(json == expected);
}

TEST_CASE("encode_AstExprGlobal")
{
    AstExprGlobal global{Location{}, AstName{"print"}};

    std::string json = toJson(&global);
    std::string expected = R"({"type":"AstExprGlobal","location":"0,0 - 0,0","global":"print"})";

    CHECK(json == expected);
}

TEST_CASE("encode_AstExprLocal")
{
    AstLocal local{AstName{"foo"}, Location{}, nullptr, 0, 0, nullptr};
    AstExprLocal exprLocal{Location{}, &local, false};

    CHECK(toJson(&exprLocal) ==  R"({"type":"AstExprLocal","location":"0,0 - 0,0","local":{"type":null,"name":"foo","location":"0,0 - 0,0"}})");
}

TEST_CASE("encode_AstExprVarargs")
{
    AstExprVarargs varargs{Location{}};

    CHECK(toJson(&varargs) == R"({"type":"AstExprVarargs","location":"0,0 - 0,0"})");
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprCall")
{
    AstExpr* expr = expectParseExpr("foo(1, 2, 3)");
    std::string_view expected = R"({"type":"AstExprCall","location":"0,4 - 0,16","func":{"type":"AstExprGlobal","location":"0,4 - 0,7","global":"foo"},"args":[{"type":"AstExprConstantNumber","location":"0,8 - 0,9","value":1},{"type":"AstExprConstantNumber","location":"0,11 - 0,12","value":2},{"type":"AstExprConstantNumber","location":"0,14 - 0,15","value":3}],"self":false,"argLocation":"0,8 - 0,16"})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprIndexName")
{
    AstExpr* expr = expectParseExpr("foo.bar");

    std::string_view expected = R"({"type":"AstExprIndexName","location":"0,4 - 0,11","expr":{"type":"AstExprGlobal","location":"0,4 - 0,7","global":"foo"},"index":"bar","indexLocation":"0,8 - 0,11","op":"."})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprIndexExpr")
{
    AstExpr* expr = expectParseExpr("foo['bar']");

    std::string_view expected = R"({"type":"AstExprIndexExpr","location":"0,4 - 0,14","expr":{"type":"AstExprGlobal","location":"0,4 - 0,7","global":"foo"},"index":{"type":"AstExprConstantString","location":"0,8 - 0,13","value":"bar"}})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprFunction")
{
    AstExpr* expr = expectParseExpr("function (a) return a end");

    std::string_view expected = R"({"type":"AstExprFunction","location":"0,4 - 0,29","generics":[],"genericPacks":[],"args":[{"type":null,"name":"a","location":"0,14 - 0,15"}],"vararg":false,"varargLocation":"0,0 - 0,0","body":{"type":"AstStatBlock","location":"0,16 - 0,26","body":[{"type":"AstStatReturn","location":"0,17 - 0,25","list":[{"type":"AstExprLocal","location":"0,24 - 0,25","local":{"type":null,"name":"a","location":"0,14 - 0,15"}}]}]},"functionDepth":1,"debugname":"","hasEnd":true})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprTable")
{
    AstExpr* expr = expectParseExpr("{true, key=true, [key2]=true}");

    std::string_view expected = R"({"type":"AstExprTable","location":"0,4 - 0,33","items":[{"kind":"item","value":{"type":"AstExprConstantBool","location":"0,5 - 0,9","value":true}},{"kind":"record","key":{"type":"AstExprConstantString","location":"0,11 - 0,14","value":"key"},"value":{"type":"AstExprConstantBool","location":"0,15 - 0,19","value":true}},{"kind":"general","key":{"type":"AstExprGlobal","location":"0,22 - 0,26","global":"key2"},"value":{"type":"AstExprConstantBool","location":"0,28 - 0,32","value":true}}]})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprUnary")
{
    AstExpr* expr = expectParseExpr("-b");

    std::string_view expected = R"({"type":"AstExprUnary","location":"0,4 - 0,6","op":"minus","expr":{"type":"AstExprGlobal","location":"0,5 - 0,6","global":"b"}})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprBinary")
{
    AstExpr* expr = expectParseExpr("b + c");

    std::string_view expected = R"({"type":"AstExprBinary","location":"0,4 - 0,9","op":"Add","left":{"type":"AstExprGlobal","location":"0,4 - 0,5","global":"b"},"right":{"type":"AstExprGlobal","location":"0,8 - 0,9","global":"c"}})";

    CHECK(toJson(expr) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstExprTypeAssertion")
{
    AstExpr* expr = expectParseExpr("b :: any");

    std::string_view expected = R"({"type":"AstExprTypeAssertion","location":"0,4 - 0,12","expr":{"type":"AstExprGlobal","location":"0,4 - 0,5","global":"b"},"annotation":{"type":"AstTypeReference","location":"0,9 - 0,12","name":"any","parameters":[]}})";

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

    std::string_view expected = R"({"type":"AstStatIf","location":"0,0 - 0,21","condition":{"type":"AstExprConstantBool","location":"0,3 - 0,7","value":true},"thenbody":{"type":"AstStatBlock","location":"0,12 - 0,13","body":[]},"elsebody":{"type":"AstStatBlock","location":"0,17 - 0,18","body":[]},"hasThen":true,"hasEnd":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatWhile")
{
    AstStat* statement = expectParseStatement("while true do end");

    std::string_view expected = R"({"type":"AtStatWhile","location":"0,0 - 0,17","condition":{"type":"AstExprConstantBool","location":"0,6 - 0,10","value":true},"body":{"type":"AstStatBlock","location":"0,13 - 0,14","body":[]},"hasDo":true,"hasEnd":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatRepeat")
{
    AstStat* statement = expectParseStatement("repeat until true");

    std::string_view expected = R"({"type":"AstStatRepeat","location":"0,0 - 0,17","condition":{"type":"AstExprConstantBool","location":"0,13 - 0,17","value":true},"body":{"type":"AstStatBlock","location":"0,6 - 0,7","body":[]},"hasUntil":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatBreak")
{
    AstStat* statement = expectParseStatement("while true do break end");

    std::string_view expected = R"({"type":"AtStatWhile","location":"0,0 - 0,23","condition":{"type":"AstExprConstantBool","location":"0,6 - 0,10","value":true},"body":{"type":"AstStatBlock","location":"0,13 - 0,20","body":[{"type":"AstStatBreak","location":"0,14 - 0,19"}]},"hasDo":true,"hasEnd":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatContinue")
{
    AstStat* statement = expectParseStatement("while true do continue end");

    std::string_view expected = R"({"type":"AtStatWhile","location":"0,0 - 0,26","condition":{"type":"AstExprConstantBool","location":"0,6 - 0,10","value":true},"body":{"type":"AstStatBlock","location":"0,13 - 0,23","body":[{"type":"AstStatContinue","location":"0,14 - 0,22"}]},"hasDo":true,"hasEnd":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatFor")
{
    AstStat* statement = expectParseStatement("for a=0,1 do end");

    std::string_view expected = R"({"type":"AstStatFor","location":"0,0 - 0,16","var":{"type":null,"name":"a","location":"0,4 - 0,5"},"from":{"type":"AstExprConstantNumber","location":"0,6 - 0,7","value":0},"to":{"type":"AstExprConstantNumber","location":"0,8 - 0,9","value":1},"body":{"type":"AstStatBlock","location":"0,12 - 0,13","body":[]},"hasDo":true,"hasEnd":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatForIn")
{
    AstStat* statement = expectParseStatement("for a in b do end");

    std::string_view expected = R"({"type":"AstStatForIn","location":"0,0 - 0,17","vars":[{"type":null,"name":"a","location":"0,4 - 0,5"}],"values":[{"type":"AstExprGlobal","location":"0,9 - 0,10","global":"b"}],"body":{"type":"AstStatBlock","location":"0,13 - 0,14","body":[]},"hasIn":true,"hasDo":true,"hasEnd":true})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatCompoundAssign")
{
    AstStat* statement = expectParseStatement("a += b");

    std::string_view expected = R"({"type":"AstStatCompoundAssign","location":"0,0 - 0,6","op":"Add","var":{"type":"AstExprGlobal","location":"0,0 - 0,1","global":"a"},"value":{"type":"AstExprGlobal","location":"0,5 - 0,6","global":"b"}})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatLocalFunction")
{
    AstStat* statement = expectParseStatement("local function a(b) return end");

    std::string_view expected = R"({"type":"AstStatLocalFunction","location":"0,0 - 0,30","name":{"type":null,"name":"a","location":"0,15 - 0,16"},"func":{"type":"AstExprFunction","location":"0,0 - 0,30","generics":[],"genericPacks":[],"args":[{"type":null,"name":"b","location":"0,17 - 0,18"}],"vararg":false,"varargLocation":"0,0 - 0,0","body":{"type":"AstStatBlock","location":"0,19 - 0,27","body":[{"type":"AstStatReturn","location":"0,20 - 0,26","list":[]}]},"functionDepth":1,"debugname":"a","hasEnd":true}})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatTypeAlias")
{
    AstStat* statement = expectParseStatement("type A = B");

    std::string_view expected = R"({"type":"AstStatTypeAlias","location":"0,0 - 0,10","name":"A","generics":[],"genericPacks":[],"type":{"type":"AstTypeReference","location":"0,9 - 0,10","name":"B","parameters":[]},"exported":false})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstStatDeclareFunction")
{
    AstStat* statement = expectParseStatement("declare function foo(x: number): string");

    std::string_view expected = R"({"type":"AstStatDeclareFunction","location":"0,0 - 0,39","name":"foo","params":{"types":[{"type":"AstTypeReference","location":"0,24 - 0,30","name":"number","parameters":[]}]},"retTypes":{"types":[{"type":"AstTypeReference","location":"0,33 - 0,39","name":"string","parameters":[]}]},"generics":[],"genericPacks":[]})";

    CHECK(toJson(statement) == expected);
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

    std::string_view expected1 = R"({"type":"AstStatDeclareClass","location":"1,22 - 4,11","name":"Foo","props":[{"name":"prop","type":{"type":"AstTypeReference","location":"2,18 - 2,24","name":"number","parameters":[]}},{"name":"method","type":{"type":"AstTypeFunction","location":"3,21 - 4,11","generics":[],"genericPacks":[],"argTypes":{"types":[{"type":"AstTypeReference","location":"3,39 - 3,45","name":"number","parameters":[]}]},"returnTypes":{"types":[{"type":"AstTypeReference","location":"3,48 - 3,54","name":"string","parameters":[]}]}}}]})";
    CHECK(toJson(root->body.data[0]) == expected1);

    std::string_view expected2 = R"({"type":"AstStatDeclareClass","location":"6,22 - 8,11","name":"Bar","superName":"Foo","props":[{"name":"prop2","type":{"type":"AstTypeReference","location":"7,19 - 7,25","name":"string","parameters":[]}}]})";
    CHECK(toJson(root->body.data[1]) == expected2);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_annotation")
{
    AstStat* statement = expectParseStatement("type T = ((number) -> (string | nil)) & ((string) -> ())");

    std::string_view expected = R"({"type":"AstStatTypeAlias","location":"0,0 - 0,55","name":"T","generics":[],"genericPacks":[],"type":{"type":"AstTypeIntersection","location":"0,9 - 0,55","types":[{"type":"AstTypeFunction","location":"0,10 - 0,35","generics":[],"genericPacks":[],"argTypes":{"types":[{"type":"AstTypeReference","location":"0,11 - 0,17","name":"number","parameters":[]}]},"returnTypes":{"types":[{"type":"AstTypeUnion","location":"0,23 - 0,35","types":[{"type":"AstTypeReference","location":"0,23 - 0,29","name":"string","parameters":[]},{"type":"AstTypeReference","location":"0,32 - 0,35","name":"nil","parameters":[]}]}]}},{"type":"AstTypeFunction","location":"0,41 - 0,55","generics":[],"genericPacks":[],"argTypes":{"types":[{"type":"AstTypeReference","location":"0,42 - 0,48","name":"string","parameters":[]}]},"returnTypes":{"types":[]}}]},"exported":false})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstTypeError")
{
    ParseResult parseResult = parse("type T = ");
    REQUIRE(1 == parseResult.root->body.size);

    AstStat* statement = parseResult.root->body.data[0];

    std::string_view expected = R"({"type":"AstStatTypeAlias","location":"0,0 - 0,9","name":"T","generics":[],"genericPacks":[],"type":{"type":"AstTypeError","location":"0,8 - 0,9","types":[],"messageIndex":0},"exported":false})";

    CHECK(toJson(statement) == expected);
}

TEST_CASE_FIXTURE(JsonEncoderFixture, "encode_AstTypePackExplicit")
{
    AstStatBlock* root = expectParse(R"(
        type A<T...> = () -> T...
        local a: A<(number, string)>
    )");

    CHECK(2 == root->body.size);

    std::string_view expected = R"({"type":"AstStatLocal","location":"2,8 - 2,36","vars":[{"type":{"type":"AstTypeReference","location":"2,17 - 2,36","name":"A","parameters":[{"type":"AstTypePackExplicit","location":"2,19 - 2,20","typeList":{"types":[{"type":"AstTypeReference","location":"2,20 - 2,26","name":"number","parameters":[]},{"type":"AstTypeReference","location":"2,28 - 2,34","name":"string","parameters":[]}]}}]},"name":"a","location":"2,14 - 2,15"}],"values":[]})";

    CHECK(toJson(root->body.data[1]) == expected);
}

TEST_SUITE_END();
