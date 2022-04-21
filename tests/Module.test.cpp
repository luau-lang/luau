// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Clone.h"
#include "Luau/Module.h"
#include "Luau/Scope.h"
#include "Luau/RecursionCounter.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauLowerBoundsCalculation);

TEST_SUITE_BEGIN("ModuleTests");

TEST_CASE_FIXTURE(Fixture, "is_within_comment")
{
    check(R"(
        --!strict
        local foo = {}
        function foo:bar() end

        --[[
            foo:
        ]] foo:bar()

        --[[]]--[[]] -- Two distinct comments that have zero characters of space between them.
    )");

    SourceModule* sm = getMainSourceModule();

    CHECK_EQ(5, sm->commentLocations.size());

    CHECK(isWithinComment(*sm, Position{1, 15}));
    CHECK(isWithinComment(*sm, Position{6, 16}));
    CHECK(isWithinComment(*sm, Position{9, 13}));
    CHECK(isWithinComment(*sm, Position{9, 14}));

    CHECK(!isWithinComment(*sm, Position{2, 15}));
    CHECK(!isWithinComment(*sm, Position{7, 10}));
    CHECK(!isWithinComment(*sm, Position{7, 11}));
}

TEST_CASE_FIXTURE(Fixture, "dont_clone_persistent_primitive")
{
    TypeArena dest;
    CloneState cloneState;

    // numberType is persistent.  We leave it as-is.
    TypeId newNumber = clone(typeChecker.numberType, dest, cloneState);
    CHECK_EQ(newNumber, typeChecker.numberType);
}

TEST_CASE_FIXTURE(Fixture, "deepClone_non_persistent_primitive")
{
    TypeArena dest;
    CloneState cloneState;

    // Create a new number type that isn't persistent
    unfreeze(typeChecker.globalTypes);
    TypeId oldNumber = typeChecker.globalTypes.addType(PrimitiveTypeVar{PrimitiveTypeVar::Number});
    freeze(typeChecker.globalTypes);
    TypeId newNumber = clone(oldNumber, dest, cloneState);

    CHECK_NE(newNumber, oldNumber);
    CHECK_EQ(*oldNumber, *newNumber);
    CHECK_EQ("number", toString(newNumber));
    CHECK_EQ(1, dest.typeVars.size());
}

TEST_CASE_FIXTURE(Fixture, "deepClone_cyclic_table")
{
    CheckResult result = check(R"(
        local Cyclic = {}
        function Cyclic.get()
            return Cyclic
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    /* The inferred type of Cyclic is {get: () -> Cyclic}
     *
     * Assert that the return type of get() is the same as the outer table.
     */

    TypeId counterType = requireType("Cyclic");

    TypeArena dest;
    CloneState cloneState;
    TypeId counterCopy = clone(counterType, dest, cloneState);

    TableTypeVar* ttv = getMutable<TableTypeVar>(counterCopy);
    REQUIRE(ttv != nullptr);

    CHECK_EQ(std::optional<std::string>{"Cyclic"}, ttv->syntheticName);

    TypeId methodType = ttv->props["get"].type;
    REQUIRE(methodType != nullptr);

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(methodType);
    REQUIRE(ftv != nullptr);

    std::optional<TypeId> methodReturnType = first(ftv->retType);
    REQUIRE(methodReturnType);

    CHECK_EQ(methodReturnType, counterCopy);
    if (FFlag::LuauLowerBoundsCalculation)
        CHECK_EQ(3, dest.typePacks.size()); // function args, its return type, and the hidden any... pack
    else
        CHECK_EQ(2, dest.typePacks.size()); // one for the function args, and another for its return type
    CHECK_EQ(2, dest.typeVars.size());      // One table and one function
}

TEST_CASE_FIXTURE(Fixture, "builtin_types_point_into_globalTypes_arena")
{
    CheckResult result = check(R"(
        return {sign=math.sign}
    )");
    dumpErrors(result);
    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr module = frontend.moduleResolver.getModule("MainModule");
    std::optional<TypeId> exports = first(module->getModuleScope()->returnType);
    REQUIRE(bool(exports));

    REQUIRE(isInArena(*exports, module->interfaceTypes));

    TableTypeVar* exportsTable = getMutable<TableTypeVar>(*exports);
    REQUIRE(exportsTable != nullptr);

    TypeId signType = exportsTable->props["sign"].type;
    REQUIRE(signType != nullptr);

    CHECK(!isInArena(signType, module->interfaceTypes));
    CHECK(isInArena(signType, typeChecker.globalTypes));
}

TEST_CASE_FIXTURE(Fixture, "deepClone_union")
{
    TypeArena dest;
    CloneState cloneState;

    unfreeze(typeChecker.globalTypes);
    TypeId oldUnion = typeChecker.globalTypes.addType(UnionTypeVar{{typeChecker.numberType, typeChecker.stringType}});
    freeze(typeChecker.globalTypes);
    TypeId newUnion = clone(oldUnion, dest, cloneState);

    CHECK_NE(newUnion, oldUnion);
    CHECK_EQ("number | string", toString(newUnion));
    CHECK_EQ(1, dest.typeVars.size());
}

TEST_CASE_FIXTURE(Fixture, "deepClone_intersection")
{
    TypeArena dest;
    CloneState cloneState;

    unfreeze(typeChecker.globalTypes);
    TypeId oldIntersection = typeChecker.globalTypes.addType(IntersectionTypeVar{{typeChecker.numberType, typeChecker.stringType}});
    freeze(typeChecker.globalTypes);
    TypeId newIntersection = clone(oldIntersection, dest, cloneState);

    CHECK_NE(newIntersection, oldIntersection);
    CHECK_EQ("number & string", toString(newIntersection));
    CHECK_EQ(1, dest.typeVars.size());
}

TEST_CASE_FIXTURE(Fixture, "clone_class")
{
    TypeVar exampleMetaClass{ClassTypeVar{"ExampleClassMeta",
        {
            {"__add", {typeChecker.anyType}},
        },
        std::nullopt, std::nullopt, {}, {}, "Test"}};
    TypeVar exampleClass{ClassTypeVar{"ExampleClass",
        {
            {"PropOne", {typeChecker.numberType}},
            {"PropTwo", {typeChecker.stringType}},
        },
        std::nullopt, &exampleMetaClass, {}, {}, "Test"}};

    TypeArena dest;
    CloneState cloneState;

    TypeId cloned = clone(&exampleClass, dest, cloneState);
    const ClassTypeVar* ctv = get<ClassTypeVar>(cloned);
    REQUIRE(ctv != nullptr);

    REQUIRE(ctv->metatable);
    const ClassTypeVar* metatable = get<ClassTypeVar>(*ctv->metatable);
    REQUIRE(metatable);

    CHECK_EQ("ExampleClass", ctv->name);
    CHECK_EQ("ExampleClassMeta", metatable->name);
}

TEST_CASE_FIXTURE(Fixture, "clone_free_types")
{
    ScopedFastFlag sff[]{
        {"LuauErrorRecoveryType", true},
        {"LuauLosslessClone", true},
    };

    TypeVar freeTy(FreeTypeVar{TypeLevel{}});
    TypePackVar freeTp(FreeTypePack{TypeLevel{}});

    TypeArena dest;
    CloneState cloneState;

    TypeId clonedTy = clone(&freeTy, dest, cloneState);
    CHECK(get<FreeTypeVar>(clonedTy));

    cloneState = {};
    TypePackId clonedTp = clone(&freeTp, dest, cloneState);
    CHECK(get<FreeTypePack>(clonedTp));
}

TEST_CASE_FIXTURE(Fixture, "clone_free_tables")
{
    ScopedFastFlag sff{"LuauLosslessClone", true};

    TypeVar tableTy{TableTypeVar{}};
    TableTypeVar* ttv = getMutable<TableTypeVar>(&tableTy);
    ttv->state = TableState::Free;

    TypeArena dest;
    CloneState cloneState;

    TypeId cloned = clone(&tableTy, dest, cloneState);
    const TableTypeVar* clonedTtv = get<TableTypeVar>(cloned);
    CHECK_EQ(clonedTtv->state, TableState::Free);
}

TEST_CASE_FIXTURE(Fixture, "clone_constrained_intersection")
{
    TypeArena src;

    TypeId constrained = src.addType(ConstrainedTypeVar{TypeLevel{}, {getSingletonTypes().numberType, getSingletonTypes().stringType}});

    TypeArena dest;
    CloneState cloneState;

    TypeId cloned = clone(constrained, dest, cloneState);
    CHECK_NE(constrained, cloned);

    const ConstrainedTypeVar* ctv = get<ConstrainedTypeVar>(cloned);
    REQUIRE_EQ(2, ctv->parts.size());
    CHECK_EQ(getSingletonTypes().numberType, ctv->parts[0]);
    CHECK_EQ(getSingletonTypes().stringType, ctv->parts[1]);
}

TEST_CASE_FIXTURE(Fixture, "clone_self_property")
{
    ScopedFastFlag sff{"LuauAnyInIsOptionalIsOptional", true};

    fileResolver.source["Module/A"] = R"(
        --!nonstrict
        local a = {}
        function a:foo(x: number)
            return -x;
        end
        return a;
    )";

    CheckResult result = frontend.check("Module/A");
    LUAU_REQUIRE_NO_ERRORS(result);

    fileResolver.source["Module/B"] = R"(
        --!nonstrict
        local a = require(script.Parent.A)
        return a.foo(5)
    )";

    result = frontend.check("Module/B");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("This function must be called with self. Did you mean to use a colon instead of a dot?", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "clone_recursion_limit")
{
#if defined(_DEBUG) || defined(_NOOPT)
    int limit = 250;
#else
    int limit = 400;
#endif
    ScopedFastInt luauTypeCloneRecursionLimit{"LuauTypeCloneRecursionLimit", limit};
    ScopedFastFlag sff{"LuauRecursionLimitException", true};

    TypeArena src;

    TypeId table = src.addType(TableTypeVar{});
    TypeId nested = table;

    for (int i = 0; i < limit + 100; i++)
    {
        TableTypeVar* ttv = getMutable<TableTypeVar>(nested);

        ttv->props["a"].type = src.addType(TableTypeVar{});
        nested = ttv->props["a"].type;
    }

    TypeArena dest;
    CloneState cloneState;

    CHECK_THROWS_AS(clone(table, dest, cloneState), RecursionLimitException);
}

TEST_SUITE_END();
