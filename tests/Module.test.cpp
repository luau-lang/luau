// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/Module.h"
#include "Luau/Parser.h"

#include "Fixture.h"

#include "ScopedFlags.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAG(DebugLuauFreezeArena);
LUAU_FASTINT(LuauTypeCloneIterationLimit);

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

TEST_CASE_FIXTURE(Fixture, "is_within_comment_parse_result")
{
    std::string src = R"(
        --!strict
        local foo = {}
        function foo:bar() end

        --[[
            foo:
        ]] foo:bar()

        --[[]]--[[]] -- Two distinct comments that have zero characters of space between them.
    )";

    Luau::Allocator alloc;
    Luau::AstNameTable names{alloc};
    Luau::ParseOptions parseOptions;
    parseOptions.captureComments = true;
    Luau::ParseResult parseResult = Luau::Parser::parse(src.data(), src.size(), names, alloc, parseOptions);

    CHECK_EQ(5, parseResult.commentLocations.size());

    CHECK(isWithinComment(parseResult, Position{1, 15}));
    CHECK(isWithinComment(parseResult, Position{6, 16}));
    CHECK(isWithinComment(parseResult, Position{9, 13}));
    CHECK(isWithinComment(parseResult, Position{9, 14}));

    CHECK(!isWithinComment(parseResult, Position{2, 15}));
    CHECK(!isWithinComment(parseResult, Position{7, 10}));
    CHECK(!isWithinComment(parseResult, Position{7, 11}));
}

TEST_CASE_FIXTURE(Fixture, "dont_clone_persistent_primitive")
{
    TypeArena dest;
    CloneState cloneState{builtinTypes};

    // numberType is persistent.  We leave it as-is.
    TypeId newNumber = clone(builtinTypes->numberType, dest, cloneState);
    CHECK_EQ(newNumber, builtinTypes->numberType);
}

TEST_CASE_FIXTURE(Fixture, "deepClone_non_persistent_primitive")
{
    TypeArena dest;
    CloneState cloneState{builtinTypes};

    // Create a new number type that isn't persistent
    unfreeze(frontend.globals.globalTypes);
    TypeId oldNumber = frontend.globals.globalTypes.addType(PrimitiveType{PrimitiveType::Number});
    freeze(frontend.globals.globalTypes);
    TypeId newNumber = clone(oldNumber, dest, cloneState);

    CHECK_NE(newNumber, oldNumber);
    CHECK_EQ(*oldNumber, *newNumber);
    CHECK_EQ("number", toString(newNumber));
    CHECK_EQ(1, dest.types.size());
}

TEST_CASE_FIXTURE(Fixture, "deepClone_cyclic_table")
{
    // Under DCR, we don't seal the outer occurrance of the table `Cyclic` which
    // breaks this test.  I'm not sure if that behaviour change is important or
    // not, but it's tangental to the core purpose of this test.

    ScopedFastFlag sff[] = {
        {FFlag::DebugLuauDeferredConstraintResolution, false},
    };

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

    TypeId ty = requireType("Cyclic");

    TypeArena dest;
    CloneState cloneState{builtinTypes};
    TypeId cloneTy = clone(ty, dest, cloneState);

    TableType* ttv = getMutable<TableType>(cloneTy);
    REQUIRE(ttv != nullptr);

    CHECK_EQ(std::optional<std::string>{"Cyclic"}, ttv->syntheticName);

    TypeId methodType = ttv->props["get"].type();
    REQUIRE(methodType != nullptr);

    const FunctionType* ftv = get<FunctionType>(methodType);
    REQUIRE(ftv != nullptr);

    std::optional<TypeId> methodReturnType = first(ftv->retTypes);
    REQUIRE(methodReturnType);

    CHECK_MESSAGE(methodReturnType == cloneTy, toString(methodType, {true}) << " should be pointer identical to " << toString(cloneTy, {true}));
    CHECK_EQ(2, dest.typePacks.size()); // one for the function args, and another for its return type
    CHECK_EQ(2, dest.types.size());     // One table and one function
}

TEST_CASE_FIXTURE(Fixture, "deepClone_cyclic_table_2")
{
    TypeArena src;

    TypeId tableTy = src.addType(TableType{});
    TableType* tt = getMutable<TableType>(tableTy);
    REQUIRE(tt);

    TypeId methodTy = src.addType(FunctionType{src.addTypePack({}), src.addTypePack({tableTy})});

    tt->props["get"].setType(methodTy);

    TypeArena dest;

    CloneState cloneState{builtinTypes};
    TypeId cloneTy = clone(tableTy, dest, cloneState);
    TableType* ctt = getMutable<TableType>(cloneTy);
    REQUIRE(ctt);

    TypeId clonedMethodType = ctt->props["get"].type();
    REQUIRE(clonedMethodType);

    const FunctionType* cmf = get<FunctionType>(clonedMethodType);
    REQUIRE(cmf);

    std::optional<TypeId> cloneMethodReturnType = first(cmf->retTypes);
    REQUIRE(bool(cloneMethodReturnType));

    CHECK(*cloneMethodReturnType == cloneTy);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "builtin_types_point_into_globalTypes_arena")
{
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

    TypeId signType = exportsTable->props["sign"].type();
    REQUIRE(signType != nullptr);

    CHECK(!isInArena(signType, module->interfaceTypes));
    CHECK(isInArena(signType, frontend.globals.globalTypes));
}

TEST_CASE_FIXTURE(Fixture, "deepClone_union")
{
    TypeArena dest;
    CloneState cloneState{builtinTypes};

    unfreeze(frontend.globals.globalTypes);
    TypeId oldUnion = frontend.globals.globalTypes.addType(UnionType{{builtinTypes->numberType, builtinTypes->stringType}});
    freeze(frontend.globals.globalTypes);
    TypeId newUnion = clone(oldUnion, dest, cloneState);

    CHECK_NE(newUnion, oldUnion);
    CHECK_EQ("number | string", toString(newUnion));
    CHECK_EQ(1, dest.types.size());
}

TEST_CASE_FIXTURE(Fixture, "deepClone_intersection")
{
    TypeArena dest;
    CloneState cloneState{builtinTypes};

    unfreeze(frontend.globals.globalTypes);
    TypeId oldIntersection = frontend.globals.globalTypes.addType(IntersectionType{{builtinTypes->numberType, builtinTypes->stringType}});
    freeze(frontend.globals.globalTypes);
    TypeId newIntersection = clone(oldIntersection, dest, cloneState);

    CHECK_NE(newIntersection, oldIntersection);
    CHECK_EQ("number & string", toString(newIntersection));
    CHECK_EQ(1, dest.types.size());
}

TEST_CASE_FIXTURE(Fixture, "clone_class")
{
    Type exampleMetaClass{ClassType{"ExampleClassMeta",
        {
            {"__add", {builtinTypes->anyType}},
        },
        std::nullopt, std::nullopt, {}, {}, "Test", {}}};
    Type exampleClass{ClassType{"ExampleClass",
        {
            {"PropOne", {builtinTypes->numberType}},
            {"PropTwo", {builtinTypes->stringType}},
        },
        std::nullopt, &exampleMetaClass, {}, {}, "Test", {}}};

    TypeArena dest;
    CloneState cloneState{builtinTypes};

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
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, false};

    TypeArena arena;
    TypeId freeTy = freshType(NotNull{&arena}, builtinTypes, nullptr);
    TypePackVar freeTp(FreeTypePack{TypeLevel{}});

    TypeArena dest;
    CloneState cloneState{builtinTypes};

    TypeId clonedTy = clone(freeTy, dest, cloneState);
    CHECK(get<FreeType>(clonedTy));

    cloneState = {builtinTypes};
    TypePackId clonedTp = clone(&freeTp, dest, cloneState);
    CHECK(get<FreeTypePack>(clonedTp));
}

TEST_CASE_FIXTURE(Fixture, "clone_free_tables")
{
    Type tableTy{TableType{}};
    TableType* ttv = getMutable<TableType>(&tableTy);
    ttv->state = TableState::Free;

    TypeArena dest;
    CloneState cloneState{builtinTypes};

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

TEST_CASE_FIXTURE(Fixture, "clone_iteration_limit")
{
    ScopedFastInt sfi{FInt::LuauTypeCloneIterationLimit, 2000};

    TypeArena src;

    TypeId table = src.addType(TableType{});
    TypeId nested = table;

    int nesting = 2500;
    for (int i = 0; i < nesting; i++)
    {
        TableType* ttv = getMutable<TableType>(nested);
        ttv->props["a"].setType(src.addType(TableType{}));
        nested = ttv->props["a"].type();
    }

    TypeArena dest;
    CloneState cloneState{builtinTypes};

    TypeId ty = clone(table, dest, cloneState);
    CHECK(get<ErrorType>(ty));

    // Cloning it again is an important test.
    TypeId ty2 = clone(table, dest, cloneState);
    CHECK(get<ErrorType>(ty2));
}

// Unions should never be cyclic, but we should clone them correctly even if
// they are.
TEST_CASE_FIXTURE(Fixture, "clone_cyclic_union")
{
    TypeArena src;

    TypeId u = src.addType(UnionType{{builtinTypes->numberType, builtinTypes->stringType}});
    UnionType* uu = getMutable<UnionType>(u);
    REQUIRE(uu);

    uu->options.push_back(u);

    TypeArena dest;
    CloneState cloneState{builtinTypes};

    TypeId cloned = clone(u, dest, cloneState);
    REQUIRE(cloned);

    const UnionType* clonedUnion = get<UnionType>(cloned);
    REQUIRE(clonedUnion);
    REQUIRE(3 == clonedUnion->options.size());

    CHECK(builtinTypes->numberType == clonedUnion->options[0]);
    CHECK(builtinTypes->stringType == clonedUnion->options[1]);
    CHECK(cloned == clonedUnion->options[2]);
}

TEST_CASE_FIXTURE(Fixture, "any_persistance_does_not_leak")
{
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

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK(toString(it->second.type) == "any");
    else
        CHECK(toString(it->second.type) == "*error-type*");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "do_not_clone_reexports")
{
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
    CHECK(typeA == tableB->props["q"].type());
}

TEST_CASE_FIXTURE(BuiltinsFixture, "do_not_clone_types_of_reexported_values")
{
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
    REQUIRE(modA);
    ModulePtr modB = frontend.moduleResolver.getModule("Module/B");
    REQUIRE(modB);

    std::optional<TypeId> typeA = first(modA->returnType);
    REQUIRE(typeA);
    std::optional<TypeId> typeB = first(modB->returnType);
    REQUIRE(typeB);

    TableType* tableA = getMutable<TableType>(*typeA);
    REQUIRE_MESSAGE(tableA, "Expected a table, but got " << toString(*typeA));
    TableType* tableB = getMutable<TableType>(*typeB);
    REQUIRE_MESSAGE(tableB, "Expected a table, but got " << toString(*typeB));

    CHECK(tableA->props["a"].type() == tableB->props["b"].type());
}

TEST_CASE_FIXTURE(BuiltinsFixture, "clone_table_bound_to_table_bound_to_table")
{
    TypeArena arena;

    TypeId a = arena.addType(TableType{TableState::Free, TypeLevel{}});
    getMutable<TableType>(a)->name = "a";

    TypeId b = arena.addType(TableType{TableState::Free, TypeLevel{}});
    getMutable<TableType>(b)->name = "b";

    TypeId c = arena.addType(TableType{TableState::Free, TypeLevel{}});
    getMutable<TableType>(c)->name = "c";

    getMutable<TableType>(a)->boundTo = b;
    getMutable<TableType>(b)->boundTo = c;

    TypeArena dest;
    CloneState state{builtinTypes};
    TypeId res = clone(a, dest, state);

    REQUIRE(dest.types.size() == 1);

    auto tableA = get<TableType>(res);
    REQUIRE_MESSAGE(tableA, "Expected table, got " << res);
    REQUIRE(tableA->name == "c");
    REQUIRE(!tableA->boundTo);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "clone_a_bound_type_to_a_persistent_type")
{
    TypeArena arena;

    TypeId boundTo = arena.addType(BoundType{builtinTypes->numberType});
    REQUIRE(builtinTypes->numberType->persistent);

    TypeArena dest;
    CloneState state{builtinTypes};
    TypeId res = clone(boundTo, dest, state);

    REQUIRE(res == follow(boundTo));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "clone_a_bound_typepack_to_a_persistent_typepack")
{
    TypeArena arena;

    TypePackId boundTo = arena.addTypePack(BoundTypePack{builtinTypes->neverTypePack});
    REQUIRE(builtinTypes->neverTypePack->persistent);

    TypeArena dest;
    CloneState state{builtinTypes};
    TypePackId res = clone(boundTo, dest, state);

    REQUIRE(res == follow(boundTo));
}

TEST_SUITE_END();
