// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/RecursionCounter.h"

#include "Luau/Type.h"
#include "Luau/IterativeTypeVisitor.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTINT(LuauVisitRecursionLimit);
LUAU_FASTFLAG(LuauSolverV2)

TEST_SUITE_BEGIN("VisitType");

TEST_CASE_FIXTURE(Fixture, "throw_when_limit_is_exceeded")
{
    if (FFlag::LuauSolverV2)
    {
        CheckResult result = check(R"(
            local t : {a: {b: {c: {d: {e: boolean}}}}}
        )");
        ScopedFastInt sfi{FInt::LuauVisitRecursionLimit, 3};
        TypeId tType = requireType("t");

        CHECK_THROWS_AS(toString(tType), RecursionLimitException);
    }
    else
    {
        ScopedFastInt sfi{FInt::LuauVisitRecursionLimit, 3};

        CheckResult result = check(R"(
            local t : {a: {b: {c: {d: {e: boolean}}}}}
        )");

        TypeId tType = requireType("t");

        CHECK_THROWS_AS(toString(tType), RecursionLimitException);
    }
}

TEST_CASE_FIXTURE(Fixture, "dont_throw_when_limit_is_high_enough")
{
    ScopedFastInt sfi{FInt::LuauVisitRecursionLimit, 8};

    CheckResult result = check(R"(
        local t : {a: {b: {c: {d: {e: boolean}}}}}
    )");

    TypeId tType = requireType("t");

    (void)toString(tType);
}

TEST_CASE_FIXTURE(Fixture, "some_free_types_do_not_have_bounds")
{
    Type t{FreeType{TypeLevel{}, getBuiltins()->neverType, getBuiltins()->unknownType}};

    (void)toString(&t);
}

TEST_CASE_FIXTURE(Fixture, "some_free_types_have_bounds")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    Scope scope{getBuiltins()->anyTypePack};
    Type t{FreeType{&scope, getBuiltins()->neverType, getBuiltins()->numberType}};

    CHECK("('a <: number)" == toString(&t));
}

struct TracingVisitor : IterativeTypeVisitor
{
    std::vector<std::string> trace;
    std::vector<TypeId> cycles;

    TracingVisitor(bool visitOnce, bool skipBoundTypes)
        : IterativeTypeVisitor("TracingVisitor", visitOnce, skipBoundTypes)
    {}

    void cycle(TypeId ty) override
    {
        cycles.emplace_back(ty);
    }

    bool visit(TypeId ty) override
    {
        trace.emplace_back(toString(ty));
        return true;
    }
};

TEST_CASE_FIXTURE(Fixture, "trace_a_simple_function")
{
    TypeId a = parseType("(number, string) -> boolean");

    TracingVisitor vis(true, true);
    vis.run(a);

    CHECK(4 == vis.trace.size());
    CHECK(vis.trace.at(0) == "(number, string) -> boolean");
    CHECK(vis.trace.at(1) == "number");
    CHECK(vis.trace.at(2) == "string");
    CHECK(vis.trace.at(3) == "boolean");
}

struct TableSkippingVisitor : IterativeTypeVisitor
{
    TableSkippingVisitor()
        : IterativeTypeVisitor("TracingVisitor", /*visitOnce*/ true, /*skipBoundTypes*/ true)
    {}

    std::vector<std::string> trace;

    bool visit(TypeId ty) override
    {
        trace.emplace_back(toString(ty));
        return true;
    }

    bool visit(TypeId ty, const TableType& tt) override
    {
        return false;
    }
};

TEST_CASE_FIXTURE(Fixture, "skip_over_tables")
{
    TypeId a = parseType("(number, string, {x: number, y: number}) -> {x: number, y: number}");

    TableSkippingVisitor vis;
    vis.run(a);

    CHECK(3 == vis.trace.size());
    CHECK(vis.trace.at(0) == "(number, string, { x: number, y: number }) -> { x: number, y: number }");
    CHECK(vis.trace.at(1) == "number");
    CHECK(vis.trace.at(2) == "string");
}

TEST_CASE_FIXTURE(Fixture, "detects_cycles")
{
    // Alas.  parseType() can't be used to create a cyclic type.
    // F where
    //     F = (T, number) -> number
    //     T = {method: F}

    TypeId fType = arena.addType(BlockedType{});

    TypeId tType = arena.addType(TableType{
        TableType::Props{{"method", fType}},
        std::nullopt,
        TypeLevel{},
        TableState::Sealed
    });

    asMutable(fType)->ty.emplace<FunctionType>(
        arena.addTypePack({tType, getBuiltins()->numberType}),
        getBuiltins()->emptyTypePack
    );

    TracingVisitor vis(true, true);
    vis.run(fType);

    CHECK(3 == vis.trace.size());
    CHECK(vis.trace.at(0) == "t1 where t1 = ({ method: t1 }, number) -> ()");
    CHECK(vis.trace.at(1) == "t1 where t1 = { method: (t1, number) -> () }");
    CHECK(vis.trace.at(2) == "number");

    CHECK(1 == vis.cycles.size());
    CHECK("t1 where t1 = ({ method: t1 }, number) -> ()" == toString(vis.cycles.at(0)));
}

TEST_CASE_FIXTURE(Fixture, "skips_bound_types")
{
    TypeId a = arena.addType(BoundType{getBuiltins()->numberType});

    TracingVisitor vis(true, true);
    vis.run(a);

    CHECK(1 == vis.trace.size());
    CHECK("number" == vis.trace.at(0));
}

TEST_CASE_FIXTURE(Fixture, "can_be_configured_not_to_skip_bound_types")
{
    TypeId a = arena.addType(BoundType{getBuiltins()->numberType});

    TracingVisitor vis(true, false);
    vis.run(a);

    CHECK(2 == vis.trace.size());
    CHECK("number" == vis.trace.at(0));
    CHECK("number" == vis.trace.at(1));
}

TEST_CASE_FIXTURE(Fixture, "visitOnce")
{
    // An acyclic type that has redundant interior structure.
    // ({x: number}, {x: number}) -> {x: number}

    TypeId xTable = arena.addType(TableType{
        TableType::Props{{"x", getBuiltins()->numberType}},
        std::nullopt,
        TypeLevel{},
        TableState::Sealed
    });

    TypeId fnTy = arena.addType(FunctionType{
        arena.addTypePack({xTable, xTable}),
        arena.addTypePack({xTable})
    });

    SUBCASE("visitOnce_true")
    {
        TracingVisitor vis(true, true);
        vis.run(fnTy);

        CHECK(3 == vis.trace.size());
        CHECK(vis.trace.at(0) == "({ x: number }, { x: number }) -> { x: number }");
        CHECK(vis.trace.at(1) == "{ x: number }");
        CHECK(vis.trace.at(2) == "number");

        CHECK(0 == vis.cycles.size());
    }

    SUBCASE("visitOnce_false")
    {
        TracingVisitor vis(false, true);
        vis.run(fnTy);

        CHECK(7 == vis.trace.size());
        CHECK(vis.trace.at(0) == "({ x: number }, { x: number }) -> { x: number }");
        CHECK(vis.trace.at(1) == "{ x: number }");
        CHECK(vis.trace.at(2) == "{ x: number }");
        CHECK(vis.trace.at(3) == "{ x: number }");
        CHECK(vis.trace.at(4) == "number");
        CHECK(vis.trace.at(5) == "number");
        CHECK(vis.trace.at(6) == "number");

        CHECK(0 == vis.cycles.size());
    }
}

// visitOnce
// RecursionLimiter (and note that it doesn't trip for the iterative variation)

TEST_SUITE_END();
