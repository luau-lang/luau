// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Parser.h"

#include "doctest.h"

using namespace Luau;

namespace Luau
{
namespace Compile
{

uint64_t modelCost(AstNode* root, AstLocal* const* vars, size_t varCount);
int computeCost(uint64_t model, const bool* varsConst, size_t varCount);

} // namespace Compile
} // namespace Luau

TEST_SUITE_BEGIN("CostModel");

static uint64_t modelFunction(const char* source)
{
    Allocator allocator;
    AstNameTable names(allocator);

    ParseResult result = Parser::parse(source, strlen(source), names, allocator);
    REQUIRE(result.root != nullptr);

    AstStatFunction* func = result.root->body.data[0]->as<AstStatFunction>();
    REQUIRE(func);

    return Luau::Compile::modelCost(func->func->body, func->func->args.data, func->func->args.size);
}

TEST_CASE("Expression")
{
    uint64_t model = modelFunction(R"(
function test(a, b, c)
    return a + (b + 1) * (b + 1) - c
end
)");

    const bool args1[] = {false, false, false};
    const bool args2[] = {false, true, false};

    CHECK_EQ(5, Luau::Compile::computeCost(model, args1, 3));
    CHECK_EQ(2, Luau::Compile::computeCost(model, args2, 3));
}

TEST_CASE("PropagateVariable")
{
    uint64_t model = modelFunction(R"(
function test(a)
    local b = a * a * a
    return b * b
end
)");

    const bool args1[] = {false};
    const bool args2[] = {true};

    CHECK_EQ(3, Luau::Compile::computeCost(model, args1, 1));
    CHECK_EQ(0, Luau::Compile::computeCost(model, args2, 1));
}

TEST_CASE("LoopAssign")
{
    uint64_t model = modelFunction(R"(
function test(a)
    for i=1,3 do
        a[i] = i
    end
end
)");

    const bool args1[] = {false};
    const bool args2[] = {true};

    // loop baseline cost is 2
    CHECK_EQ(3, Luau::Compile::computeCost(model, args1, 1));
    CHECK_EQ(3, Luau::Compile::computeCost(model, args2, 1));
}

TEST_CASE("MutableVariable")
{
    uint64_t model = modelFunction(R"(
function test(a, b)
    local x = a * a
    x += b
    return x * x
end
)");

    const bool args1[] = {false};
    const bool args2[] = {true};

    CHECK_EQ(3, Luau::Compile::computeCost(model, args1, 1));
    CHECK_EQ(2, Luau::Compile::computeCost(model, args2, 1));
}

TEST_CASE("ImportCall")
{
    uint64_t model = modelFunction(R"(
function test(a)
    return Instance.new(a)
end
)");

    const bool args1[] = {false};
    const bool args2[] = {true};

    CHECK_EQ(6, Luau::Compile::computeCost(model, args1, 1));
    CHECK_EQ(6, Luau::Compile::computeCost(model, args2, 1));
}

TEST_CASE("FastCall")
{
    uint64_t model = modelFunction(R"(
function test(a)
    return math.abs(a + 1)
end
)");

    const bool args1[] = {false};
    const bool args2[] = {true};

    // note: we currently don't treat fast calls differently from cost model perspective
    CHECK_EQ(6, Luau::Compile::computeCost(model, args1, 1));
    CHECK_EQ(5, Luau::Compile::computeCost(model, args2, 1));
}

TEST_CASE("ControlFlow")
{
    uint64_t model = modelFunction(R"(
function test(a)
    while a < 0 do
        a += 1
    end
    for i=1,2 do
        a += 1
    end
    for i in pairs({}) do
        a += 1
        if a % 2 == 0 then continue end
    end
    repeat
        a += 1
        if a % 2 == 0 then break end
    until a > 10
    return a
end
)");

    const bool args1[] = {false};
    const bool args2[] = {true};

    CHECK_EQ(38, Luau::Compile::computeCost(model, args1, 1));
    CHECK_EQ(37, Luau::Compile::computeCost(model, args2, 1));
}

TEST_CASE("Conditional")
{
    uint64_t model = modelFunction(R"(
function test(a)
    return if a < 0 then -a else a
end
)");

    const bool args1[] = {false};
    const bool args2[] = {true};

    CHECK_EQ(4, Luau::Compile::computeCost(model, args1, 1));
    CHECK_EQ(2, Luau::Compile::computeCost(model, args2, 1));
}

TEST_CASE("VarArgs")
{
    uint64_t model = modelFunction(R"(
function test(...)
    return select('#', ...) :: number
end
)");

    CHECK_EQ(8, Luau::Compile::computeCost(model, nullptr, 0));
}

TEST_CASE("TablesFunctions")
{
    uint64_t model = modelFunction(R"(
function test()
    return { 42, op = function() end }
end
)");

    CHECK_EQ(22, Luau::Compile::computeCost(model, nullptr, 0));
}

TEST_CASE("CostOverflow")
{
    uint64_t model = modelFunction(R"(
function test()
    return {{{{{{{{{{{{{{{}}}}}}}}}}}}}}}
end
)");

    CHECK_EQ(127, Luau::Compile::computeCost(model, nullptr, 0));
}

TEST_CASE("TableAssign")
{
    uint64_t model = modelFunction(R"(
function test(a)
    for i=1,#a do
        a[i] = i
    end
end
)");

    const bool args1[] = {false};
    const bool args2[] = {true};

    CHECK_EQ(4, Luau::Compile::computeCost(model, args1, 1));
    CHECK_EQ(3, Luau::Compile::computeCost(model, args2, 1));
}

TEST_SUITE_END();
