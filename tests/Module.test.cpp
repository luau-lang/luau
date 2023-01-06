// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Clone.h"
#include "Luau/Module.h"
#include "Luau/Scope.h"
#include "Luau/RecursionCounter.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);

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
    TypeId oldNumber = typeChecker.globalTypes.addType(PrimitiveType{PrimitiveType::Number});
    freeze(typeChecker.globalTypes);
    TypeId newNumber = clone(oldNumber, dest, cloneState);

    CHECK_NE(newNumber, oldNumber);
    CHECK_EQ(*oldNumber, *newNumber);
    CHECK_EQ("number", toString(newNumber));
    CHECK_EQ(1, dest.types.size());
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

    TableType* ttv = getMutable<TableType>(counterCopy);
    REQUIRE(ttv != nullptr);

    CHECK_EQ(std::optional<std::string>{"Cyclic"}, ttv->syntheticName);

    TypeId methodType = ttv->props["get"].type;
    REQUIRE(methodType != nullptr);

    const FunctionType* ftv = get<FunctionType>(methodType);
    REQUIRE(ftv != nullptr);

    std::optional<TypeId> methodReturnType = first(ftv->retTypes);
    REQUIRE(methodReturnType);

    CHECK_EQ(methodReturnType, counterCopy);
    CHECK_EQ(2, dest.typePacks.size()); // one for the function args, and another for its return type
    CHECK_EQ(2, dest.types.size());     // One table and one function
}

TEST_CASE_FIXTURE(BuiltinsFixture, "builtin_types_point_into_globalTypes_arena")
{
    ScopedFastFlag luauScopelessModule{"LuauScopelessModule", true};

    CheckResult result = check(R"(
        return {sign=math.sign}
    )");
    dumpErrors(result);
    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr module = frontend.moduleResolver.getModule("MainModule");
    std::optional<TypeId> exports = first(module->returnType);
    REQUIRE(bool(exports));

    REQUIRE(isInArena(*exports, module->interfaceTypes));

    TableType* exportsTable = getMutable<TableType>(*exports);
    REQUIRE(exportsTable != nullptr);

    TypeId signType = exportsTable->props["sign"].type;
    REQUIRE(signType != nullptr);

    CHECK(!isInArena(signType, module->interfaceTypes));
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK(isInArena(signType, frontend.globalTypes));
    else
        CHECK(isInArena(signType, typeChecker.globalTypes));
}

TEST_CASE_FIXTURE(Fixture, "deepClone_union")
{
    TypeArena dest;
    CloneState cloneState;

    unfreeze(typeChecker.globalTypes);
    TypeId oldUnion = typeChecker.globalTypes.addType(UnionType{{typeChecker.numberType, typeChecker.stringType}});
    freeze(typeChecker.globalTypes);
    TypeId newUnion = clone(oldUnion, dest, cloneState);

    CHECK_NE(newUnion, oldUnion);
    CHECK_EQ("number | string", toString(newUnion));
    CHECK_EQ(1, dest.types.size());
}

TEST_CASE_FIXTURE(Fixture, "deepClone_intersection")
{
    TypeArena dest;
    CloneState cloneState;

    unfreeze(typeChecker.globalTypes);
    TypeId oldIntersection = typeChecker.globalTypes.addType(IntersectionType{{typeChecker.numberType, typeChecker.stringType}});
    freeze(typeChecker.globalTypes);
    TypeId newIntersection = clone(oldIntersection, dest, cloneState);

    CHECK_NE(newIntersection, oldIntersection);
    CHECK_EQ("number & string", toString(newIntersection));
    CHECK_EQ(1, dest.types.size());
}

TEST_CASE_FIXTURE(Fixture, "clone_class")
{
    Type exampleMetaClass{ClassType{"ExampleClassMeta",
        {
            {"__add", {typeChecker.anyType}},
        },
        std::nullopt, std::nullopt, {}, {}, "Test"}};
    Type exampleClass{ClassType{"ExampleClass",
        {
            {"PropOne", {typeChecker.numberType}},
            {"PropTwo", {typeChecker.stringType}},
        },
        std::nullopt, &exampleMetaClass, {}, {}, "Test"}};

    TypeArena dest;
    CloneState cloneState;

    TypeId cloned = clone(&exampleClass, dest, cloneState);
    const ClassType* ctv = get<ClassType>(cloned);
    REQUIRE(ctv != nullptr);

    REQUIRE(ctv->metatable);
    const ClassType* metatable = get<ClassType>(*ctv->metatable);
    REQUIRE(metatable);

    CHECK_EQ("ExampleClass", ctv->name);
    CHECK_EQ("ExampleClassMeta", metatable->name);
}

TEST_CASE_FIXTURE(Fixture, "clone_free_types")
{
    Type freeTy(FreeType{TypeLevel{}});
    TypePackVar freeTp(FreeTypePack{TypeLevel{}});

    TypeArena dest;
    CloneState cloneState;

    TypeId clonedTy = clone(&freeTy, dest, cloneState);
    CHECK(get<FreeType>(clonedTy));

    cloneState = {};
    TypePackId clonedTp = clone(&freeTp, dest, cloneState);
    CHECK(get<FreeTypePack>(clonedTp));
}

TEST_CASE_FIXTURE(Fixture, "clone_free_tables")
{
    Type tableTy{TableType{}};
    TableType* ttv = getMutable<TableType>(&tableTy);
    ttv->state = TableState::Free;

    TypeArena dest;
    CloneState cloneState;

    TypeId cloned = clone(&tableTy, dest, cloneState);
    const TableType* clonedTtv = get<TableType>(cloned);
    CHECK_EQ(clonedTtv->state, TableState::Free);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "clone_self_property")
{
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

    TypeArena src;

    TypeId table = src.addType(TableType{});
    TypeId nested = table;

    for (int i = 0; i < limit + 100; i++)
    {
        TableType* ttv = getMutable<TableType>(nested);

        ttv->props["a"].type = src.addType(TableType{});
        nested = ttv->props["a"].type;
    }

    TypeArena dest;
    CloneState cloneState;

    CHECK_THROWS_AS(clone(table, dest, cloneState), RecursionLimitException);
}

TEST_CASE_FIXTURE(Fixture, "any_persistance_does_not_leak")
{
    ScopedFastFlag luauScopelessModule{"LuauScopelessModule", true};

    fileResolver.source["Module/A"] = R"(
export type A = B
type B = A
    )";

    FrontendOptions opts;
    opts.retainFullTypeGraphs = false;
    CheckResult result = frontend.check("Module/A", opts);
    LUAU_REQUIRE_ERRORS(result);

    auto mod = frontend.moduleResolver.getModule("Module/A");
    auto it = mod->exportedTypeBindings.find("A");
    REQUIRE(it != mod->exportedTypeBindings.end());
    CHECK(toString(it->second.type) == "any");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "do_not_clone_reexports")
{
    ScopedFastFlag flags[] = {
        {"LuauClonePublicInterfaceLess", true},
        {"LuauSubstitutionReentrant", true},
        {"LuauClassTypeVarsInSubstitution", true},
        {"LuauSubstitutionFixMissingFields", true},
        {"LuauScopelessModule", true},
    };

    fileResolver.source["Module/A"] = R"(
export type A = {p : number}
return {}
    )";

    fileResolver.source["Module/B"] = R"(
local a = require(script.Parent.A)
export type B = {q : a.A}
return {}
    )";

    CheckResult result = frontend.check("Module/B");
    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr modA = frontend.moduleResolver.getModule("Module/A");
    ModulePtr modB = frontend.moduleResolver.getModule("Module/B");
    REQUIRE(modA);
    REQUIRE(modB);
    auto modAiter = modA->exportedTypeBindings.find("A");
    auto modBiter = modB->exportedTypeBindings.find("B");
    REQUIRE(modAiter != modA->exportedTypeBindings.end());
    REQUIRE(modBiter != modB->exportedTypeBindings.end());
    TypeId typeA = modAiter->second.type;
    TypeId typeB = modBiter->second.type;
    TableType* tableB = getMutable<TableType>(typeB);
    REQUIRE(tableB);
    CHECK(typeA == tableB->props["q"].type);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "do_not_clone_types_of_reexported_values")
{
    ScopedFastFlag flags[] = {
        {"LuauClonePublicInterfaceLess", true},
        {"LuauSubstitutionReentrant", true},
        {"LuauClassTypeVarsInSubstitution", true},
        {"LuauSubstitutionFixMissingFields", true},
        {"LuauScopelessModule", true},
    };

    fileResolver.source["Module/A"] = R"(
local exports = {a={p=5}}
return exports
    )";

    fileResolver.source["Module/B"] = R"(
local a = require(script.Parent.A)
local exports = {b=a.a}
return exports
    )";

    CheckResult result = frontend.check("Module/B");
    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr modA = frontend.moduleResolver.getModule("Module/A");
    ModulePtr modB = frontend.moduleResolver.getModule("Module/B");
    REQUIRE(modA);
    REQUIRE(modB);
    std::optional<TypeId> typeA = first(modA->returnType);
    std::optional<TypeId> typeB = first(modB->returnType);
    REQUIRE(typeA);
    REQUIRE(typeB);
    TableType* tableA = getMutable<TableType>(*typeA);
    TableType* tableB = getMutable<TableType>(*typeB);
    CHECK(tableA->props["a"].type == tableB->props["b"].type);
}

TEST_SUITE_END();
