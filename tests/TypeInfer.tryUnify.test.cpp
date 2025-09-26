// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"
#include "Luau/Scope.h"
#include "Luau/Symbol.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2);
LUAU_FASTFLAG(LuauUnifierRecursionOnRestart);

struct TryUnifyFixture : Fixture
{
    // Cannot use `TryUnifyFixture` under DCR.
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    TypeArena arena;
    ScopePtr globalScope{new Scope{arena.addTypePack({TypeId{}})}};
    InternalErrorReporter iceHandler;
    UnifierSharedState unifierState{&iceHandler};
    Normalizer normalizer{&arena, getBuiltins(), NotNull{&unifierState}, SolverMode::Old};
    Unifier state{NotNull{&normalizer}, NotNull{globalScope.get()}, Location{}, Variance::Covariant};
};

TEST_SUITE_BEGIN("TryUnifyTests");

TEST_CASE_FIXTURE(TryUnifyFixture, "primitives_unify")
{
    Type numberOne{TypeVariant{PrimitiveType{PrimitiveType::Number}}};
    Type numberTwo = numberOne.clone();

    state.tryUnify(&numberTwo, &numberOne);

    CHECK(!state.failure);
    CHECK(state.errors.empty());
}

TEST_CASE_FIXTURE(TryUnifyFixture, "compatible_functions_are_unified")
{
    Type functionOne{TypeVariant{
        FunctionType(arena.addTypePack({arena.freshType(getBuiltins(), globalScope->level)}), arena.addTypePack({getBuiltins()->numberType}))
    }};

    Type functionTwo{TypeVariant{FunctionType(
        arena.addTypePack({arena.freshType(getBuiltins(), globalScope->level)}),
        arena.addTypePack({arena.freshType(getBuiltins(), globalScope->level)})
    )}};

    state.tryUnify(&functionTwo, &functionOne);
    CHECK(!state.failure);
    CHECK(state.errors.empty());

    state.log.commit();

    CHECK_EQ(functionOne, functionTwo);
}

TEST_CASE_FIXTURE(TryUnifyFixture, "incompatible_functions_are_preserved")
{
    TypePackVar argPackOne{TypePack{{arena.freshType(getBuiltins(), globalScope->level)}, std::nullopt}};
    Type functionOne{TypeVariant{
        FunctionType(arena.addTypePack({arena.freshType(getBuiltins(), globalScope->level)}), arena.addTypePack({getBuiltins()->numberType}))
    }};

    Type functionOneSaved = functionOne.clone();

    TypePackVar argPackTwo{TypePack{{arena.freshType(getBuiltins(), globalScope->level)}, std::nullopt}};
    Type functionTwo{TypeVariant{
        FunctionType(arena.addTypePack({arena.freshType(getBuiltins(), globalScope->level)}), arena.addTypePack({getBuiltins()->stringType}))
    }};

    Type functionTwoSaved = functionTwo.clone();

    state.tryUnify(&functionTwo, &functionOne);
    CHECK(state.failure);
    CHECK(!state.errors.empty());

    CHECK_EQ(functionOne, functionOneSaved);
    CHECK_EQ(functionTwo, functionTwoSaved);
}

TEST_CASE_FIXTURE(TryUnifyFixture, "tables_can_be_unified")
{
    Type tableOne{TypeVariant{
        TableType{{{"foo", {arena.freshType(getBuiltins(), globalScope->level)}}}, std::nullopt, globalScope->level, TableState::Unsealed},
    }};

    Type tableTwo{TypeVariant{
        TableType{{{"foo", {arena.freshType(getBuiltins(), globalScope->level)}}}, std::nullopt, globalScope->level, TableState::Unsealed},
    }};

    CHECK_NE(*getMutable<TableType>(&tableOne)->props["foo"].type_DEPRECATED(), *getMutable<TableType>(&tableTwo)->props["foo"].type_DEPRECATED());

    state.tryUnify(&tableTwo, &tableOne);

    CHECK(!state.failure);
    CHECK(state.errors.empty());

    state.log.commit();

    CHECK_EQ(*getMutable<TableType>(&tableOne)->props["foo"].type_DEPRECATED(), *getMutable<TableType>(&tableTwo)->props["foo"].type_DEPRECATED());
}

TEST_CASE_FIXTURE(TryUnifyFixture, "incompatible_tables_are_preserved")
{
    Type tableOne{TypeVariant{
        TableType{
            {{"foo", {arena.freshType(getBuiltins(), globalScope->level)}}, {"bar", {getBuiltins()->numberType}}},
            std::nullopt,
            globalScope->level,
            TableState::Unsealed
        },
    }};

    Type tableTwo{TypeVariant{
        TableType{
            {{"foo", {arena.freshType(getBuiltins(), globalScope->level)}}, {"bar", {getBuiltins()->stringType}}},
            std::nullopt,
            globalScope->level,
            TableState::Unsealed
        },
    }};

    CHECK_NE(*getMutable<TableType>(&tableOne)->props["foo"].type_DEPRECATED(), *getMutable<TableType>(&tableTwo)->props["foo"].type_DEPRECATED());

    state.tryUnify(&tableTwo, &tableOne);

    CHECK(state.failure);
    CHECK_EQ(1, state.errors.size());

    CHECK_NE(*getMutable<TableType>(&tableOne)->props["foo"].type_DEPRECATED(), *getMutable<TableType>(&tableTwo)->props["foo"].type_DEPRECATED());
}

TEST_CASE_FIXTURE(Fixture, "uninhabited_intersection_sub_never")
{
    CheckResult result = check(R"(
        function f(arg : string & number) : never
          return arg
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "uninhabited_intersection_sub_anything")
{
    CheckResult result = check(R"(
        function f(arg : string & number) : boolean
          return arg
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "uninhabited_table_sub_never")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f(arg : { prop : string & number }) : never
          return arg
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "uninhabited_table_sub_anything")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f(arg : { prop : string & number }) : boolean
          return arg
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "members_of_failed_typepack_unification_are_unified_with_errorType")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f(arg: number) end
        local a
        local b
        f(a, b)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("number", toString(requireType("a")));
    CHECK_EQ("*error-type*", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "result_of_failed_typepack_unification_is_constrained")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f(arg: number) return arg end
        local a
        local b
        local c = f(a, b)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("number", toString(requireType("a")));
    CHECK_EQ("*error-type*", toString(requireType("b")));
    CHECK_EQ("number", toString(requireType("c")));
}

TEST_CASE_FIXTURE(Fixture, "typepack_unification_should_trim_free_tails")
{
    CheckResult result = check(R"(
        --!strict
        local function f(v: number)
            if v % 2 == 0 then
                return true
            end
        end

        return function()
            return (f(1))
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("(number) -> boolean", toString(requireType("f")));
}

TEST_CASE_FIXTURE(TryUnifyFixture, "variadic_type_pack_unification")
{
    TypePackVar testPack{TypePack{{getBuiltins()->numberType, getBuiltins()->stringType}, std::nullopt}};
    TypePackVar variadicPack{VariadicTypePack{getBuiltins()->numberType}};

    state.tryUnify(&testPack, &variadicPack);
    CHECK(state.failure);
    CHECK(!state.errors.empty());
}

TEST_CASE_FIXTURE(TryUnifyFixture, "variadic_tails_respect_progress")
{
    TypePackVar variadicPack{VariadicTypePack{getBuiltins()->booleanType}};
    TypePackVar a{TypePack{{getBuiltins()->numberType, getBuiltins()->stringType, getBuiltins()->booleanType, getBuiltins()->booleanType}}};
    TypePackVar b{TypePack{{getBuiltins()->numberType, getBuiltins()->stringType}, &variadicPack}};

    state.tryUnify(&b, &a);
    CHECK(!state.failure);
    CHECK(state.errors.empty());
}

TEST_CASE_FIXTURE(Fixture, "variadics_should_use_reversed_properly")
{
    CheckResult result = check(R"(
        --!strict
        local function f<T>(...: T): ...T
            return ...
        end

        local x: string = f(1)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(toString(tm->givenType), "number");
    CHECK_EQ(toString(tm->wantedType), "string");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cli_41095_concat_log_in_sealed_table_unification")
{

    CheckResult result = check(R"(
        --!strict
        table.insert()
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ(toString(result.errors[0]), "No overload for function accepts 0 arguments.");
    if (FFlag::LuauSolverV2)
        CHECK_EQ(toString(result.errors[1]), "Available overloads: <V>({V}, V) -> (); and <V>({V}, number, V) -> ()");
    else
        CHECK_EQ(toString(result.errors[1]), "Available overloads: ({'a}, 'a) -> (); and ({'a}, number, 'a) -> ()");
}

TEST_CASE_FIXTURE(TryUnifyFixture, "free_tail_is_grown_properly")
{
    TypePackId threeNumbers =
        arena.addTypePack(TypePack{{getBuiltins()->numberType, getBuiltins()->numberType, getBuiltins()->numberType}, std::nullopt});
    TypePackId numberAndFreeTail =
        arena.addTypePack(TypePack{{getBuiltins()->numberType}, arena.addTypePack(TypePackVar{FreeTypePack{TypeLevel{}}})});

    CHECK(state.canUnify(numberAndFreeTail, threeNumbers).empty());
}

TEST_CASE_FIXTURE(TryUnifyFixture, "recursive_metatable_getmatchtag")
{
    Type redirect{FreeType{TypeLevel{}, getBuiltins()->neverType, getBuiltins()->unknownType}};
    Type table{TableType{}};
    Type metatable{MetatableType{&redirect, &table}};
    redirect = BoundType{&metatable}; // Now we have a metatable that is recursive on the table type
    Type variant{UnionType{{&metatable, getBuiltins()->numberType}}};

    state.tryUnify(&metatable, &variant);
}

TEST_CASE_FIXTURE(TryUnifyFixture, "cli_50320_follow_in_any_unification")
{
    TypePackVar free{FreeTypePack{TypeLevel{}}};
    TypePackVar target{TypePack{}};

    Type func{FunctionType{&free, &free}};

    state.tryUnify(&free, &target);
    // Shouldn't assert or error.
    state.tryUnify(&func, getBuiltins()->anyType);
}

TEST_CASE_FIXTURE(TryUnifyFixture, "txnlog_preserves_type_owner")
{
    TypeId a = arena.freshType(getBuiltins(), TypeLevel{});
    TypeId b = getBuiltins()->numberType;

    state.tryUnify(a, b);
    state.log.commit();

    CHECK_EQ(a->owningArena, &arena);
}

TEST_CASE_FIXTURE(TryUnifyFixture, "txnlog_preserves_pack_owner")
{
    TypePackId a = arena.addTypePack(TypePackVar{FreeTypePack{TypeLevel{}}});
    TypePackId b = getBuiltins()->anyTypePack;

    state.tryUnify(a, b);
    state.log.commit();

    CHECK_EQ(a->owningArena, &arena);
}

TEST_CASE_FIXTURE(TryUnifyFixture, "fuzz_tail_unification_issue")
{
    TypePackVar variadicAny{VariadicTypePack{getBuiltins()->anyType}};
    TypePackVar packTmp{TypePack{{getBuiltins()->anyType}, &variadicAny}};
    TypePackVar packSub{TypePack{{getBuiltins()->anyType, getBuiltins()->anyType}, &packTmp}};

    Type freeTy{FreeType{TypeLevel{}, getBuiltins()->neverType, getBuiltins()->unknownType}};
    TypePackVar freeTp{FreeTypePack{TypeLevel{}}};
    TypePackVar packSuper{TypePack{{&freeTy}, &freeTp}};

    state.tryUnify(&packSub, &packSuper);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzz_unify_any_should_check_log")
{
    CheckResult result = check(R"(
repeat
_._,_ = nil
until _
local l0:(any)&(typeof(_)),l0:(any)|(any) = _,_
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_unification_full_restart_recursion")
{
    ScopedFastFlag luauUnifierRecursionOnRestart{FFlag::LuauUnifierRecursionOnRestart, true};

    CheckResult result = check(R"(
local A, B, C, D

E = function(a, b)
    local mt = getmetatable(b)
    if mt.tm:bar(A) == nil and mt.tm:bar(B) == nil then end
    if mt.foo == true then D(b, 3) end
    mt.foo:call(false, b)
end

A = function(a, b)
    local mt = getmetatable(b)
    if mt.foo == true then D(b, 3) end
    C(mt, 3)
end

B = function(a, b)
    local mt = getmetatable(b)
    if mt.foo == true then D(b, 3) end
    C(mt, 3)
end
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_SUITE_END();
