// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Ast.h"
#include "Luau/JsonEncoder.h"

#include "doctest.h"

#include <ostream>

using namespace Luau;

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
        (R"({"type":"AstStatBlock","location":"0,0 - 0,0","body":[{"type":"AstStatLocal","location":"0,0 - 0,0","vars":["a_local"],"values":[]}]})"),
        toJson(&block));
}

TEST_SUITE_END();
