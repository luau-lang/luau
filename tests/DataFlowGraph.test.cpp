// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/DataFlowGraph.h"
#include "Luau/Error.h"
#include "Luau/Parser.h"

#include "AstQueryDsl.h"
#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);

struct DataFlowGraphFixture
{
    // Only needed to fix the operator== reflexivity of an empty Symbol.
    ScopedFastFlag dcr{FFlag::DebugLuauDeferredConstraintResolution, true};

    InternalErrorReporter handle;

    Allocator allocator;
    AstNameTable names{allocator};
    AstStatBlock* module;

    std::optional<DataFlowGraph> graph;

    void dfg(const std::string& code)
    {
        ParseResult parseResult = Parser::parse(code.c_str(), code.size(), names, allocator);
        if (!parseResult.errors.empty())
            throw ParseErrors(std::move(parseResult.errors));
        module = parseResult.root;
        graph = DataFlowGraphBuilder::build(module, NotNull{&handle});
    }

    template<typename T, int N>
    DefId getDef(const std::vector<Nth>& nths = {nth<T>(N)})
    {
        T* node = query<T, N>(module, nths);
        REQUIRE(node);
        return graph->getDef(node);
    }
};

TEST_SUITE_BEGIN("DataFlowGraphBuilder");

TEST_CASE_FIXTURE(DataFlowGraphFixture, "define_locals_in_local_stat")
{
    dfg(R"(
        local x = 5
        local y = x
    )");

    (void)getDef<AstExprLocal, 1>();
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "define_parameters_in_functions")
{
    dfg(R"(
        local function f(x)
            local y = x
        end
    )");

    (void)getDef<AstExprLocal, 1>();
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "find_aliases")
{
    dfg(R"(
        local x = 5
        local y = x
        local z = y
    )");

    DefId x = getDef<AstExprLocal, 1>();
    DefId y = getDef<AstExprLocal, 2>();
    REQUIRE(x != y);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "independent_locals")
{
    dfg(R"(
        local x = 5
        local y = 5

        local a = x
        local b = y
    )");

    DefId x = getDef<AstExprLocal, 1>();
    DefId y = getDef<AstExprLocal, 2>();
    REQUIRE(x != y);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "phi")
{
    dfg(R"(
        local x

        if a then
            x = true
        end

        local y = x
    )");

    DefId y = getDef<AstExprLocal, 2>();

    const Phi* phi = get<Phi>(y);
    CHECK(phi);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "mutate_local_not_owned_by_while")
{
    dfg(R"(
        local x

        while cond() do
            x = true
        end

        local y = x
    )");

    DefId x0 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId x1 = getDef<AstExprLocal, 1>(); // x = true
    DefId x2 = getDef<AstExprLocal, 2>(); // local y = x

    CHECK(x0 == x1);
    CHECK(x1 == x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "mutate_local_owned_by_while")
{
    dfg(R"(
        while cond() do
            local x
            x = true
            x = 5
        end
    )");

    DefId x0 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId x1 = getDef<AstExprLocal, 1>(); // x = true
    DefId x2 = getDef<AstExprLocal, 2>(); // x = 5

    CHECK(x0 != x1);
    CHECK(x1 != x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "mutate_local_not_owned_by_repeat")
{
    dfg(R"(
        local x

        repeat
            x = true
        until cond()

        local y = x
    )");

    DefId x0 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId x1 = getDef<AstExprLocal, 1>(); // x = true
    DefId x2 = getDef<AstExprLocal, 2>(); // local y = x

    CHECK(x0 == x1);
    CHECK(x1 == x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "mutate_local_owned_by_repeat")
{
    dfg(R"(
        repeat
            local x
            x = true
            x = 5
        until cond()
    )");

    DefId x0 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId x1 = getDef<AstExprLocal, 1>(); // x = true
    DefId x2 = getDef<AstExprLocal, 2>(); // x = 5

    CHECK(x0 != x1);
    CHECK(x1 != x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "mutate_local_not_owned_by_for")
{
    dfg(R"(
        local x

        for i = 0, 5 do
            x = true
        end

        local y = x
    )");

    DefId x0 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId x1 = getDef<AstExprLocal, 1>(); // x = true
    DefId x2 = getDef<AstExprLocal, 2>(); // local y = x

    CHECK(x0 == x1);
    CHECK(x1 == x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "mutate_local_owned_by_for")
{
    dfg(R"(
        for i = 0, 5 do
            local x
            x = true
            x = 5
        end
    )");

    DefId x0 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId x1 = getDef<AstExprLocal, 1>(); // x = true
    DefId x2 = getDef<AstExprLocal, 2>(); // x = 5

    CHECK(x0 != x1);
    CHECK(x1 != x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "mutate_local_not_owned_by_for_in")
{
    dfg(R"(
        local x

        for i, v in t do
            x = true
        end

        local y = x
    )");

    DefId x0 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId x1 = getDef<AstExprLocal, 1>(); // x = true
    DefId x2 = getDef<AstExprLocal, 2>(); // local y = x

    CHECK(x0 == x1);
    CHECK(x1 == x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "mutate_local_owned_by_for_in")
{
    dfg(R"(
        for i, v in t do
            local x
            x = true
            x = 5
        end
    )");

    DefId x0 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId x1 = getDef<AstExprLocal, 1>(); // x = true
    DefId x2 = getDef<AstExprLocal, 2>(); // x = 5

    CHECK(x0 != x1);
    CHECK(x1 != x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "mutate_preexisting_property_not_owned_by_while")
{
    dfg(R"(
        local t = {}
        t.x = 5

        while cond() do
            t.x = true
        end

        local y = t.x
    )");

    DefId x1 = getDef<AstExprIndexName, 1>(); // t.x = 5
    DefId x2 = getDef<AstExprIndexName, 2>(); // t.x = true
    DefId x3 = getDef<AstExprIndexName, 3>(); // local y = t.x

    CHECK(x1 == x2);
    CHECK(x2 == x3);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "mutate_non_preexisting_property_not_owned_by_while")
{
    dfg(R"(
        local t = {}

        while cond() do
            t.x = true
        end

        local y = t.x
    )");

    DefId x1 = getDef<AstExprIndexName, 1>(); // t.x = true
    DefId x2 = getDef<AstExprIndexName, 2>(); // local y = t.x

    CHECK(x1 == x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "mutate_property_of_table_owned_by_while")
{
    dfg(R"(
        while cond() do
            local t = {}
            t.x = true
            t.x = 5
        end
    )");

    DefId x1 = getDef<AstExprIndexName, 1>(); // t.x = true
    DefId x2 = getDef<AstExprIndexName, 2>(); // t.x = 5

    CHECK(x1 != x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "property_lookup_on_a_phi_node")
{
    dfg(R"(
        local t = {}
        t.x = 5

        if cond() then
            t.x = 7
        end

        print(t.x)
    )");

    DefId x1 = getDef<AstExprIndexName, 1>(); // t.x = 5
    DefId x2 = getDef<AstExprIndexName, 2>(); // t.x = 7
    DefId x3 = getDef<AstExprIndexName, 3>(); // print(t.x)

    CHECK(x1 != x2);
    CHECK(x2 != x3);

    const Phi* phi = get<Phi>(x3);
    REQUIRE(phi);
    CHECK(phi->operands.at(0) == x1);
    CHECK(phi->operands.at(1) == x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "property_lookup_on_a_phi_node_2")
{
    dfg(R"(
        local t = {}

        if cond() then
            t.x = 5
        else
            t.x = 7
        end

        print(t.x)
    )");

    DefId x1 = getDef<AstExprIndexName, 1>(); // t.x = 5
    DefId x2 = getDef<AstExprIndexName, 2>(); // t.x = 7
    DefId x3 = getDef<AstExprIndexName, 3>(); // print(t.x)

    CHECK(x1 != x2);
    CHECK(x2 != x3);

    const Phi* phi = get<Phi>(x3);
    REQUIRE(phi);
    CHECK(phi->operands.at(0) == x2);
    CHECK(phi->operands.at(1) == x1);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "property_lookup_on_a_phi_node_3")
{
    dfg(R"(
        local t = {}
        t.x = 3

        if cond() then
            t.x = 5
            t.y = 7
        else
            t.z = 42
        end

        print(t.x)
        print(t.y)
        print(t.z)
    )");

    DefId x1 = getDef<AstExprIndexName, 1>(); // t.x = 3
    DefId x2 = getDef<AstExprIndexName, 2>(); // t.x = 5

    DefId y1 = getDef<AstExprIndexName, 3>(); // t.y = 7

    DefId z1 = getDef<AstExprIndexName, 4>(); // t.z = 42

    DefId x3 = getDef<AstExprIndexName, 5>(); // print(t.x)
    DefId y2 = getDef<AstExprIndexName, 6>(); // print(t.y)
    DefId z2 = getDef<AstExprIndexName, 7>(); // print(t.z)

    CHECK(x1 != x2);
    CHECK(x2 != x3);
    CHECK(y1 == y2);
    CHECK(z1 == z2);

    const Phi* phi = get<Phi>(x3);
    REQUIRE(phi);
    CHECK(phi->operands.at(0) == x1);
    CHECK(phi->operands.at(1) == x2);
}

TEST_SUITE_END();
