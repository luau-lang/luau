// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"
#include "Luau/Scope.h"
#include "Luau/Symbol.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAG(LuauAlwaysCommitInferencesOfFunctionCalls);
LUAU_FASTFLAG(LuauUnifierRecursionOnRestart);

struct TryUnifyFixture : Fixture
{
    // Cannot use `TryUnifyFixture` under DCR.
    ScopedFastFlag noDcr{FFlag::DebugLuauDeferredConstraintResolution, false};

    TypeArena arena;
    ScopePtr globalScope{new Scope{arena.addTypePack({TypeId{}})}};
    InternalErrorReporter iceHandler;
    UnifierSharedState unifierState{&iceHandler};
    Normalizer normalizer{&arena, builtinTypes, NotNull{&unifierState}};
    Unifier state{NotNull{&normalizer}, NotNull{globalScope.get()}, Location{}, Variance::Covariant};
};

TEST_SUITE_BEGIN("TryUnifyTests");

TEST_CASE_FIXTURE(TryUnifyFixture, "primitives_unify")
{
    Type numberOne{TypeVariant{PrimitiveType{PrimitiveType::Number}}};
    Type numberTwo = numberOne;

    state.tryUnify(&numberTwo, &numberOne);

    CHECK(!state.failure);
    CHECK(state.errors.empty());
}

TEST_CASE_FIXTURE(TryUnifyFixture, "compatible_functions_are_unified")
{
    Type functionOne{
        TypeVariant{FunctionType(arena.addTypePack({arena.freshType(globalScope->level)}), arena.addTypePack({builtinTypes->numberType}))}};

    Type functionTwo{TypeVariant{
        FunctionType(arena.addTypePack({arena.freshType(globalScope->level)}), arena.addTypePack({arena.freshType(globalScope->level)}))}};

    state.tryUnify(&functionTwo, &functionOne);
    CHECK(!state.failure);
    CHECK(state.errors.empty());

    state.log.commit();

    CHECK_EQ(functionOne, functionTwo);
}

TEST_CASE_FIXTURE(TryUnifyFixture, "incompatible_functions_are_preserved")
{
    TypePackVar argPackOne{TypePack{{arena.freshType(globalScope->level)}, std::nullopt}};
    Type functionOne{
        TypeVariant{FunctionType(arena.addTypePack({arena.freshType(globalScope->level)}), arena.addTypePack({builtinTypes->numberType}))}};

    Type functionOneSaved = functionOne;

    TypePackVar argPackTwo{TypePack{{arena.freshType(globalScope->level)}, std::nullopt}};
    Type functionTwo{
        TypeVariant{FunctionType(arena.addTypePack({arena.freshType(globalScope->level)}), arena.addTypePack({builtinTypes->stringType}))}};

    Type functionTwoSaved = functionTwo;

    state.tryUnify(&functionTwo, &functionOne);
    CHECK(state.failure);
    CHECK(!state.errors.empty());

    CHECK_EQ(functionOne, functionOneSaved);
    CHECK_EQ(functionTwo, functionTwoSaved);
}

TEST_CASE_FIXTURE(TryUnifyFixture, "tables_can_be_unified")
{
    Type tableOne{TypeVariant{
        TableType{{{"foo", {arena.freshType(globalScope->level)}}}, std::nullopt, globalScope->level, TableState::Unsealed},
    }};

    Type tableTwo{TypeVariant{
        TableType{{{"foo", {arena.freshType(globalScope->level)}}}, std::nullopt, globalScope->level, TableState::Unsealed},
    }};

    CHECK_NE(*getMutable<TableType>(&tableOne)->props["foo"].type(), *getMutable<TableType>(&tableTwo)->props["foo"].type());

    state.tryUnify(&tableTwo, &tableOne);

    CHECK(!state.failure);
    CHECK(state.errors.empty());

    state.log.commit();

    CHECK_EQ(*getMutable<TableType>(&tableOne)->props["foo"].type(), *getMutable<TableType>(&tableTwo)->props["foo"].type());
}

TEST_CASE_FIXTURE(TryUnifyFixture, "incompatible_tables_are_preserved")
{
    Type tableOne{TypeVariant{
        TableType{{{"foo", {arena.freshType(globalScope->level)}}, {"bar", {builtinTypes->numberType}}}, std::nullopt, globalScope->level,
            TableState::Unsealed},
    }};

    Type tableTwo{TypeVariant{
        TableType{{{"foo", {arena.freshType(globalScope->level)}}, {"bar", {builtinTypes->stringType}}}, std::nullopt, globalScope->level,
            TableState::Unsealed},
    }};

    CHECK_NE(*getMutable<TableType>(&tableOne)->props["foo"].type(), *getMutable<TableType>(&tableTwo)->props["foo"].type());

    state.tryUnify(&tableTwo, &tableOne);

    CHECK(state.failure);
    CHECK_EQ(1, state.errors.size());

    CHECK_NE(*getMutable<TableType>(&tableOne)->props["foo"].type(), *getMutable<TableType>(&tableTwo)->props["foo"].type());
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
    CheckResult result = check(R"(
        function f(arg : { prop : string & number }) : never
          return arg
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "uninhabited_table_sub_anything")
{
    CheckResult result = check(R"(
        function f(arg : { prop : string & number }) : boolean
          return arg
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "members_of_failed_typepack_unification_are_unified_with_errorType")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauAlwaysCommitInferencesOfFunctionCalls, true},
    };

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
    ScopedFastFlag sff[] = {
        {FFlag::LuauAlwaysCommitInferencesOfFunctionCalls, true},
    };

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
    TypePackVar testPack{TypePack{{builtinTypes->numberType, builtinTypes->stringType}, std::nullopt}};
    TypePackVar variadicPack{VariadicTypePack{builtinTypes->numberType}};

    state.tryUnify(&testPack, &variadicPack);
    CHECK(state.failure);
    CHECK(!state.errors.empty());
}

TEST_CASE_FIXTURE(TryUnifyFixture, "variadic_tails_respect_progress")
{
    TypePackVar variadicPack{VariadicTypePack{builtinTypes->booleanType}};
    TypePackVar a{TypePack{{builtinTypes->numberType, builtinTypes->stringType, builtinTypes->booleanType, builtinTypes->booleanType}}};
    TypePackVar b{TypePack{{builtinTypes->numberType, builtinTypes->stringType}, &variadicPack}};

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
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ(toString(result.errors[1]), "Available overloads: <V>({V}, V) -> (); and <V>({V}, number, V) -> ()");
    else
        CHECK_EQ(toString(result.errors[1]), "Available overloads: ({a}, a) -> (); and ({a}, number, a) -> ()");
}

TEST_CASE_FIXTURE(TryUnifyFixture, "free_tail_is_grown_properly")
{
    TypePackId threeNumbers =
        arena.addTypePack(TypePack{{builtinTypes->numberType, builtinTypes->numberType, builtinTypes->numberType}, std::nullopt});
    TypePackId numberAndFreeTail = arena.addTypePack(TypePack{{builtinTypes->numberType}, arena.addTypePack(TypePackVar{FreeTypePack{TypeLevel{}}})});

    CHECK(state.canUnify(numberAndFreeTail, threeNumbers).empty());
}

TEST_CASE_FIXTURE(TryUnifyFixture, "recursive_metatable_getmatchtag")
{
    Type redirect{FreeType{TypeLevel{}}};
    Type table{TableType{}};
    Type metatable{MetatableType{&redirect, &table}};
    redirect = BoundType{&metatable}; // Now we have a metatable that is recursive on the table type
    Type variant{UnionType{{&metatable, builtinTypes->numberType}}};

    state.tryUnify(&metatable, &variant);
}

TEST_CASE_FIXTURE(TryUnifyFixture, "cli_50320_follow_in_any_unification")
{
    TypePackVar free{FreeTypePack{TypeLevel{}}};
    TypePackVar target{TypePack{}};

    Type func{FunctionType{&free, &free}};

    state.tryUnify(&free, &target);
    // Shouldn't assert or error.
    state.tryUnify(&func, builtinTypes->anyType);
}

TEST_CASE_FIXTURE(TryUnifyFixture, "txnlog_preserves_type_owner")
{
    TypeId a = arena.addType(Type{FreeType{TypeLevel{}}});
    TypeId b = builtinTypes->numberType;

    state.tryUnify(a, b);
    state.log.commit();

    CHECK_EQ(a->owningArena, &arena);
}

TEST_CASE_FIXTURE(TryUnifyFixture, "txnlog_preserves_pack_owner")
{
    TypePackId a = arena.addTypePack(TypePackVar{FreeTypePack{TypeLevel{}}});
    TypePackId b = builtinTypes->anyTypePack;

    state.tryUnify(a, b);
    state.log.commit();

    CHECK_EQ(a->owningArena, &arena);
}

TEST_CASE_FIXTURE(TryUnifyFixture, "metatables_unify_against_shape_of_free_table")
{
    TableType::Props freeProps{
        {"foo", {builtinTypes->numberType}},
    };

    TypeId free = arena.addType(TableType{freeProps, std::nullopt, TypeLevel{}, TableState::Free});

    TableType::Props indexProps{
        {"foo", {builtinTypes->stringType}},
    };

    TypeId index = arena.addType(TableType{indexProps, std::nullopt, TypeLevel{}, TableState::Sealed});

    TableType::Props mtProps{
        {"__index", {index}},
    };

    TypeId mt = arena.addType(TableType{mtProps, std::nullopt, TypeLevel{}, TableState::Sealed});

    TypeId target = arena.addType(TableType{TableState::Unsealed, TypeLevel{}});
    TypeId metatable = arena.addType(MetatableType{target, mt});

    state.enableNewSolver();
    state.tryUnify(metatable, free);
    state.log.commit();

    REQUIRE_EQ(state.errors.size(), 1);
    const std::string expected = R"(Type
    '{ @metatable {| __index: {| foo: string |} |}, {  } }'
could not be converted into
    '{- foo: number -}'
caused by:
  Type 'number' could not be converted into 'string')";
    CHECK_EQ(expected, toString(state.errors[0]));
}

TEST_CASE_FIXTURE(TryUnifyFixture, "fuzz_tail_unification_issue")
{
    TypePackVar variadicAny{VariadicTypePack{builtinTypes->anyType}};
    TypePackVar packTmp{TypePack{{builtinTypes->anyType}, &variadicAny}};
    TypePackVar packSub{TypePack{{builtinTypes->anyType, builtinTypes->anyType}, &packTmp}};

    Type freeTy{FreeType{TypeLevel{}}};
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

static TypeId createTheType(TypeArena& arena, NotNull<BuiltinTypes> builtinTypes, Scope* scope, TypeId freeTy)
{
    /*
    ({|
        render: (
            (('a) -> ()) | {| current: 'a |}
        ) -> nil
    |}) -> ()
    */
    TypePackId emptyPack = arena.addTypePack({});

    return arena.addType(FunctionType{
        arena.addTypePack({arena.addType(TableType{
            TableType::Props{{{"render",
                Property(arena.addType(FunctionType{
                    arena.addTypePack({arena.addType(UnionType{{arena.addType(FunctionType{arena.addTypePack({freeTy}), emptyPack}),
                        arena.addType(TableType{TableType::Props{{"current", {freeTy}}}, std::nullopt, TypeLevel{}, scope, TableState::Sealed})}})}),
                    arena.addTypePack({builtinTypes->nilType})}))}}},
            std::nullopt, TypeLevel{}, scope, TableState::Sealed})}),
        emptyPack});
};

// See CLI-71190
TEST_CASE_FIXTURE(TryUnifyFixture, "unifying_two_unions_under_dcr_does_not_create_a_BoundType_cycle")
{
    const std::shared_ptr<Scope> scope = globalScope;
    const std::shared_ptr<Scope> nestedScope = std::make_shared<Scope>(scope);

    const TypeId outerType = arena.freshType(scope.get());
    const TypeId outerType2 = arena.freshType(scope.get());

    const TypeId innerType = arena.freshType(nestedScope.get());

    ScopedFastFlag sffs[]{
        {FFlag::LuauAlwaysCommitInferencesOfFunctionCalls, true},
    };

    state.enableNewSolver();

    SUBCASE("equal_scopes")
    {
        TypeId one = createTheType(arena, builtinTypes, scope.get(), outerType);
        TypeId two = createTheType(arena, builtinTypes, scope.get(), outerType2);

        state.tryUnify(one, two);
        state.log.commit();

        ToStringOptions opts;

        CHECK(follow(outerType) == follow(outerType2));
    }

    SUBCASE("outer_scope_is_subtype")
    {
        TypeId one = createTheType(arena, builtinTypes, scope.get(), outerType);
        TypeId two = createTheType(arena, builtinTypes, scope.get(), innerType);

        state.tryUnify(one, two);
        state.log.commit();

        ToStringOptions opts;

        CHECK(follow(outerType) == follow(innerType));

        // The scope of outerType exceeds that of innerType.  The latter should be bound to the former.
        const BoundType* bt = get_if<BoundType>(&innerType->ty);
        REQUIRE(bt);
        CHECK(bt->boundTo == outerType);
    }

    SUBCASE("outer_scope_is_supertype")
    {
        TypeId one = createTheType(arena, builtinTypes, scope.get(), innerType);
        TypeId two = createTheType(arena, builtinTypes, scope.get(), outerType);

        state.tryUnify(one, two);
        state.log.commit();

        ToStringOptions opts;

        CHECK(follow(outerType) == follow(innerType));

        // The scope of outerType exceeds that of innerType.  The latter should be bound to the former.
        const BoundType* bt = get_if<BoundType>(&innerType->ty);
        REQUIRE(bt);
        CHECK(bt->boundTo == outerType);
    }
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
