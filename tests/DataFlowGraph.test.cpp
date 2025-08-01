// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/DataFlowGraph.h"
#include "Fixture.h"
#include "Luau/Def.h"
#include "Luau/Error.h"
#include "Luau/Parser.h"

#include "AstQueryDsl.h"
#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2);

struct DataFlowGraphFixture
{
    // Only needed to fix the operator== reflexivity of an empty Symbol.
    ScopedFastFlag dcr{FFlag::LuauSolverV2, true};

    DefArena defArena;
    RefinementKeyArena keyArena;
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
        graph = DataFlowGraphBuilder::build(module, NotNull{&defArena}, NotNull{&keyArena}, NotNull{&handle});
    }

    template<typename T, int N>
    DefId getDef(const std::vector<Nth>& nths = {nth<T>(N)})
    {
        T* node = query<T, N>(module, nths);
        REQUIRE(node);
        return graph->getDef(node);
    }

    void checkOperands(const Phi* phi, std::vector<DefId> operands)
    {
        Set<const Def*> operandSet{nullptr};
        for (auto o : operands)
            operandSet.insert(o.get());
        CHECK(phi->operands.size() == operandSet.size());
        for (auto o : phi->operands)
            CHECK(operandSet.contains(o.get()));
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

    auto phi = get<Phi>(x2);
    REQUIRE(phi);
    checkOperands(phi, {x0, x1});
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

    CHECK(x0 != x1);
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

    auto phi = get<Phi>(x2);
    REQUIRE(phi);
    checkOperands(phi, {x0, x1});
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

    auto phi = get<Phi>(x2);
    REQUIRE(phi);
    checkOperands(phi, {x0, x1});
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

    auto phi = get<Phi>(x3);
    REQUIRE(phi);
    checkOperands(phi, {x1, x2});
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
    REQUIRE(phi->operands.size() == 2);
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
    REQUIRE(phi->operands.size() == 2);
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
    REQUIRE(phi->operands.size() == 2);
    CHECK(phi->operands.at(0) == x1);
    CHECK(phi->operands.at(1) == x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "function_captures_are_phi_nodes_of_all_versions")
{
    dfg(R"(
        local x = 5

        function f()
            print(x)
            x = nil
        end

        f()
        x = "five"
    )");

    DefId x1 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId x2 = getDef<AstExprLocal, 1>(); // print(x)
    DefId x3 = getDef<AstExprLocal, 2>(); // x = nil
    DefId x4 = getDef<AstExprLocal, 3>(); // x = "five"

    CHECK(x1 != x2);
    CHECK(x2 == x3);
    CHECK(x3 != x4);

    const Phi* phi = get<Phi>(x2);
    REQUIRE(phi);
    REQUIRE(phi->operands.size() == 2);
    CHECK(phi->operands.at(0) == x1);
    CHECK(phi->operands.at(1) == x4);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "function_captures_are_phi_nodes_of_all_versions_properties")
{
    dfg(R"(
        local t = {}
        t.x = 5

        function f()
            print(t.x)
            t.x = nil
        end

        f()
        t.x = "five"
    )");

    DefId x1 = getDef<AstExprIndexName, 1>(); // t.x = 5
    DefId x2 = getDef<AstExprIndexName, 2>(); // print(t.x)
    DefId x3 = getDef<AstExprIndexName, 3>(); // t.x = nil
    DefId x4 = getDef<AstExprIndexName, 4>(); // t.x = "five"

    CHECK(x1 != x2);
    CHECK(x2 != x3);
    CHECK(x3 != x4);

    // When a local is referenced within a function, it is not pointer identical.
    // Instead, it's a phi node of all possible versions, including just one version.
    DefId t1 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId t2 = getDef<AstExprLocal, 2>(); // print(t.x)

    const Phi* phi = get<Phi>(t2);
    REQUIRE(phi);
    REQUIRE(phi->operands.size() == 1);
    CHECK(phi->operands.at(0) == t1);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "local_f_which_is_prototyped_enclosed_by_function")
{
    dfg(R"(
        local f
        function f()
            if cond() then
                f()
            end
        end
    )");

    DefId f1 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId f2 = getDef<AstExprLocal, 1>(); // function f()
    DefId f3 = getDef<AstExprLocal, 2>(); // f()

    CHECK(f1 != f2);
    CHECK(f2 != f3);

    const Phi* phi = get<Phi>(f3);
    REQUIRE(phi);
    REQUIRE(phi->operands.size() == 1);
    CHECK(phi->operands.at(0) == f2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "local_f_which_is_prototyped_enclosed_by_function_has_some_prior_versions")
{
    dfg(R"(
        local f
        f = 5
        function f()
            if cond() then
                f()
            end
        end
    )");

    DefId f1 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId f2 = getDef<AstExprLocal, 1>(); // f = 5
    DefId f3 = getDef<AstExprLocal, 2>(); // function f()
    DefId f4 = getDef<AstExprLocal, 3>(); // f()

    CHECK(f1 != f2);
    CHECK(f2 != f3);
    CHECK(f3 != f4);

    const Phi* phi = get<Phi>(f4);
    REQUIRE(phi);
    REQUIRE(phi->operands.size() == 1);
    CHECK(phi->operands.at(0) == f3);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "local_f_which_is_prototyped_enclosed_by_function_has_some_future_versions")
{
    dfg(R"(
        local f
        function f()
            if cond() then
                f()
            end
        end
        f = 5
    )");

    DefId f1 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId f2 = getDef<AstExprLocal, 1>(); // function f()
    DefId f3 = getDef<AstExprLocal, 2>(); // f()
    DefId f4 = getDef<AstExprLocal, 3>(); // f = 5

    CHECK(f1 != f2);
    CHECK(f2 != f3);
    CHECK(f3 != f4);

    const Phi* phi = get<Phi>(f3);
    REQUIRE(phi);
    REQUIRE(phi->operands.size() == 2);
    CHECK(phi->operands.at(0) == f2);
    CHECK(phi->operands.at(1) == f4);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "phi_node_if_case_binding")
{
    dfg(R"(
local x = nil
if true then
    if true then
        x = 5
    end
    print(x)
else
    print(x)
end
)");
    DefId x1 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId x2 = getDef<AstExprLocal, 1>(); // x = 5
    DefId x3 = getDef<AstExprLocal, 2>(); // print(x)

    const Phi* phi = get<Phi>(x3);
    REQUIRE(phi);
    CHECK(phi->operands.at(0) == x2);
    CHECK(phi->operands.at(1) == x1);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "phi_node_if_case_table_prop")
{
    dfg(R"(
local t = {}
t.x = true
if true then
    if true then
        t.x = 5
    end
    print(t.x)
else
    print(t.x)
end
)");

    DefId x1 = getDef<AstExprIndexName, 1>(); // t.x = true
    DefId x2 = getDef<AstExprIndexName, 2>(); // t.x = 5

    DefId x3 = getDef<AstExprIndexName, 3>(); // print(t.x)
    const Phi* phi = get<Phi>(x3);
    REQUIRE(phi);
    CHECK(phi->operands.size() == 2);
    CHECK(phi->operands.at(0) == x1);
    CHECK(phi->operands.at(1) == x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "phi_node_if_case_table_prop_literal")
{
    dfg(R"(
local t = { x = true }
if true then
    t.x = 5
end
print(t.x)

)");

    DefId x1 = getDef<AstExprConstantBool, 1>(); // {x = true <- }
    DefId x2 = getDef<AstExprIndexName, 1>();    // t.x = 5
    DefId x3 = getDef<AstExprIndexName, 2>();    // print(t.x)
    const Phi* phi = get<Phi>(x3);
    REQUIRE(phi);
    CHECK(phi->operands.size() == 2);
    CHECK(phi->operands.at(0) == x1);
    CHECK(phi->operands.at(1) == x2);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "insert_trivial_phi_nodes_inside_of_phi_nodes")
{
    dfg(R"(
        local t = {}

        local function f(k: string)
            if t[k] ~= nil then
                return
            end

            t[k] = 5
        end
    )");

    DefId t1 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]); // local t = {}
    DefId t2 = getDef<AstExprLocal, 1>();                                // t[k] ~= nil
    DefId t3 = getDef<AstExprLocal, 3>();                                // t[k] = 5

    CHECK(t1 != t2);
    CHECK(t2 == t3);

    const Phi* t2phi = get<Phi>(t2);
    REQUIRE(t2phi);
    CHECK(t2phi->operands.size() == 1);
    CHECK(t2phi->operands.at(0) == t1);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "dfg_function_definition_in_a_do_block")
{
    dfg(R"(
        local f
        do
            function f()
            end
        end
        f()
    )");

    DefId x1 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId x2 = getDef<AstExprLocal, 1>(); // x = 5
    DefId x3 = getDef<AstExprLocal, 2>(); // print(x)

    CHECK(x1 != x2);
    CHECK(x1 != x3);
    CHECK(x2 == x3);
}

TEST_CASE_FIXTURE(DataFlowGraphFixture, "dfg_captured_local_is_assigned_a_function")
{
    dfg(R"(
        local f

        local function g()
            f()
        end

        function f()
        end
    )");

    DefId f1 = graph->getDef(query<AstStatLocal>(module)->vars.data[0]);
    DefId f2 = getDef<AstExprLocal, 1>();
    DefId f3 = getDef<AstExprLocal, 2>();

    CHECK(f1 != f2);
    CHECK(f2 != f3);

    const Phi* f2phi = get<Phi>(f2);
    REQUIRE(f2phi);
    CHECK(f2phi->operands.size() == 1);
    CHECK(f2phi->operands.at(0) == f3);
}

TEST_SUITE_END();
