// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Parser.h"
#include "Luau/RequireTracer.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

namespace
{

struct RequireTracerFixture
{
    RequireTracerFixture()
        : allocator()
        , names(allocator)
    {
    }

    AstStatBlock* parse(std::string_view src)
    {
        ParseResult result = Parser::parse(src.data(), src.size(), names, allocator, ParseOptions{});
        if (!result.errors.empty())
        {
            std::string message;

            for (const auto& error : result.errors)
            {
                if (!message.empty())
                    message += "\n";

                message += error.what();
            }

            printf("Parse error: %s\n", message.c_str());
            return nullptr;
        }
        else
            return result.root;
    }

    Allocator allocator;
    AstNameTable names;

    Luau::TestFileResolver fileResolver;
};

const std::vector<std::string> roots = {"game", "Game", "workspace", "Workspace", "script"};

} // namespace

TEST_SUITE_BEGIN("RequireTracerTest");

TEST_CASE_FIXTURE(RequireTracerFixture, "trace_local")
{
    AstStatBlock* block = parse(R"(
        local m = workspace.Foo.Bar.Baz
    )");

    RequireTraceResult result = traceRequires(&fileResolver, block, "ModuleName");
    REQUIRE(!result.exprs.empty());

    AstStatLocal* loc = block->body.data[0]->as<AstStatLocal>();
    REQUIRE(loc);
    REQUIRE_EQ(1, loc->vars.size);
    REQUIRE_EQ(1, loc->values.size);

    AstExprIndexName* value = loc->values.data[0]->as<AstExprIndexName>();
    REQUIRE(value);
    REQUIRE(result.exprs.contains(value));
    CHECK_EQ("workspace/Foo/Bar/Baz", result.exprs[value]);

    value = value->expr->as<AstExprIndexName>();
    REQUIRE(value);
    REQUIRE(result.exprs.contains(value));
    CHECK_EQ("workspace/Foo/Bar", result.exprs[value]);

    value = value->expr->as<AstExprIndexName>();
    REQUIRE(value);
    REQUIRE(result.exprs.contains(value));
    CHECK_EQ("workspace/Foo", result.exprs[value]);

    AstExprGlobal* workspace = value->expr->as<AstExprGlobal>();
    REQUIRE(workspace);
    REQUIRE(result.exprs.contains(workspace));
    CHECK_EQ("workspace", result.exprs[workspace]);
}

TEST_CASE_FIXTURE(RequireTracerFixture, "trace_transitive_local")
{
    AstStatBlock* block = parse(R"(
        local m = workspace.Foo.Bar.Baz
        local n = m.Quux
    )");

    REQUIRE_EQ(2, block->body.size);

    RequireTraceResult result = traceRequires(&fileResolver, block, "ModuleName");

    AstStatLocal* local = block->body.data[1]->as<AstStatLocal>();
    REQUIRE(local);
    REQUIRE_EQ(1, local->vars.size);

    REQUIRE(result.exprs.contains(local->values.data[0]));
    CHECK_EQ("workspace/Foo/Bar/Baz/Quux", result.exprs[local->values.data[0]]);
}

TEST_CASE_FIXTURE(RequireTracerFixture, "trace_function_arguments")
{
    AstStatBlock* block = parse(R"(
        local M = require(workspace.Game.Thing, workspace.Something.Else)
    )");
    REQUIRE_EQ(1, block->body.size);

    RequireTraceResult result = traceRequires(&fileResolver, block, "ModuleName");

    AstStatLocal* local = block->body.data[0]->as<AstStatLocal>();
    REQUIRE(local != nullptr);
    REQUIRE_EQ(1, local->vars.size);
    REQUIRE_EQ(1, local->values.size);

    AstExprCall* call = local->values.data[0]->as<AstExprCall>();
    REQUIRE(call != nullptr);

    REQUIRE_EQ(2, call->args.size);

    CHECK_EQ("workspace/Game/Thing", result.exprs[call->args.data[0]]);
    CHECK_EQ("workspace/Something/Else", result.exprs[call->args.data[1]]);
}

TEST_CASE_FIXTURE(RequireTracerFixture, "follow_GetService_calls")
{
    AstStatBlock* block = parse(R"(
        local R = game:GetService('ReplicatedStorage').Roact
        local Roact = require(R)
    )");
    REQUIRE_EQ(2, block->body.size);

    RequireTraceResult result = traceRequires(&fileResolver, block, "ModuleName");

    AstStatLocal* local = block->body.data[0]->as<AstStatLocal>();
    REQUIRE(local != nullptr);

    CHECK_EQ("game/ReplicatedStorage/Roact", result.exprs[local->values.data[0]]);

    AstStatLocal* local2 = block->body.data[1]->as<AstStatLocal>();
    REQUIRE(local2 != nullptr);
    REQUIRE_EQ(1, local2->values.size);

    AstExprCall* call = local2->values.data[0]->as<AstExprCall>();
    REQUIRE(call != nullptr);
    REQUIRE_EQ(1, call->args.size);

    CHECK_EQ("game/ReplicatedStorage/Roact", result.exprs[call->args.data[0]]);
}

TEST_CASE_FIXTURE(RequireTracerFixture, "follow_WaitForChild_calls")
{
    ScopedFastFlag luauTraceRequireLookupChild("LuauTraceRequireLookupChild", true);

    AstStatBlock* block = parse(R"(
local A = require(workspace:WaitForChild('ReplicatedStorage').Content)
local B = require(workspace:FindFirstChild('ReplicatedFirst').Data)
    )");

    RequireTraceResult result = traceRequires(&fileResolver, block, "ModuleName");

    REQUIRE_EQ(2, result.requires.size());
    CHECK_EQ("workspace/ReplicatedStorage/Content", result.requires[0].first);
    CHECK_EQ("workspace/ReplicatedFirst/Data", result.requires[1].first);
}

TEST_CASE_FIXTURE(RequireTracerFixture, "follow_typeof")
{
    AstStatBlock* block = parse(R"(
        local R: typeof(require(workspace.CoolThing).UsefulObject)
    )");
    REQUIRE_EQ(1, block->body.size);

    RequireTraceResult result = traceRequires(&fileResolver, block, "ModuleName");

    AstStatLocal* local = block->body.data[0]->as<AstStatLocal>();
    REQUIRE(local != nullptr);

    REQUIRE_EQ(local->vars.size, 1);

    AstType* ann = local->vars.data[0]->annotation;
    REQUIRE(ann != nullptr);

    AstTypeTypeof* typeofAnnotation = ann->as<AstTypeTypeof>();
    REQUIRE(typeofAnnotation != nullptr);

    AstExprIndexName* indexName = typeofAnnotation->expr->as<AstExprIndexName>();
    REQUIRE(indexName != nullptr);
    REQUIRE_EQ(indexName->index, "UsefulObject");

    AstExprCall* call = indexName->expr->as<AstExprCall>();
    REQUIRE(call != nullptr);
    REQUIRE_EQ(1, call->args.size);

    CHECK_EQ("workspace/CoolThing", result.exprs[call->args.data[0]]);
}

TEST_CASE_FIXTURE(RequireTracerFixture, "follow_string_indexexpr")
{
    AstStatBlock* block = parse(R"(
        local R = game["Test"]
    )");
    REQUIRE_EQ(1, block->body.size);

    RequireTraceResult result = traceRequires(&fileResolver, block, "ModuleName");

    AstStatLocal* local = block->body.data[0]->as<AstStatLocal>();
    REQUIRE(local != nullptr);

    CHECK_EQ("game/Test", result.exprs[local->values.data[0]]);
}

TEST_SUITE_END();
