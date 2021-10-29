// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Module.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

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

    SeenTypes seenTypes;
    SeenTypePacks seenTypePacks;

    // numberType is persistent.  We leave it as-is.
    TypeId newNumber = clone(typeChecker.numberType, dest, seenTypes, seenTypePacks);
    CHECK_EQ(newNumber, typeChecker.numberType);
}

TEST_CASE_FIXTURE(Fixture, "deepClone_non_persistent_primitive")
{
    TypeArena dest;

    SeenTypes seenTypes;
    SeenTypePacks seenTypePacks;

    // Create a new number type that isn't persistent
    unfreeze(typeChecker.globalTypes);
    TypeId oldNumber = typeChecker.globalTypes.addType(PrimitiveTypeVar{PrimitiveTypeVar::Number});
    freeze(typeChecker.globalTypes);
    TypeId newNumber = clone(oldNumber, dest, seenTypes, seenTypePacks);

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

    SeenTypes seenTypes;
    SeenTypePacks seenTypePacks;

    TypeArena dest;
    TypeId counterCopy = clone(counterType, dest, seenTypes, seenTypePacks);

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
    CHECK_EQ(2, dest.typePacks.size()); // one for the function args, and another for its return type
    CHECK_EQ(2, dest.typeVars.size());  // One table and one function
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

    SeenTypes seenTypes;
    SeenTypePacks seenTypePacks;

    unfreeze(typeChecker.globalTypes);
    TypeId oldUnion = typeChecker.globalTypes.addType(UnionTypeVar{{typeChecker.numberType, typeChecker.stringType}});
    freeze(typeChecker.globalTypes);
    TypeId newUnion = clone(oldUnion, dest, seenTypes, seenTypePacks);

    CHECK_NE(newUnion, oldUnion);
    CHECK_EQ("number | string", toString(newUnion));
    CHECK_EQ(1, dest.typeVars.size());
}

TEST_CASE_FIXTURE(Fixture, "deepClone_intersection")
{
    TypeArena dest;

    SeenTypes seenTypes;
    SeenTypePacks seenTypePacks;

    unfreeze(typeChecker.globalTypes);
    TypeId oldIntersection = typeChecker.globalTypes.addType(IntersectionTypeVar{{typeChecker.numberType, typeChecker.stringType}});
    freeze(typeChecker.globalTypes);
    TypeId newIntersection = clone(oldIntersection, dest, seenTypes, seenTypePacks);

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
        std::nullopt, std::nullopt, {}, {}}};
    TypeVar exampleClass{ClassTypeVar{"ExampleClass",
        {
            {"PropOne", {typeChecker.numberType}},
            {"PropTwo", {typeChecker.stringType}},
        },
        std::nullopt, &exampleMetaClass, {}, {}}};

    TypeArena dest;

    SeenTypes seenTypes;
    SeenTypePacks seenTypePacks;

    TypeId cloned = clone(&exampleClass, dest, seenTypes, seenTypePacks);
    const ClassTypeVar* ctv = get<ClassTypeVar>(cloned);
    REQUIRE(ctv != nullptr);

    REQUIRE(ctv->metatable);
    const ClassTypeVar* metatable = get<ClassTypeVar>(*ctv->metatable);
    REQUIRE(metatable);

    CHECK_EQ("ExampleClass", ctv->name);
    CHECK_EQ("ExampleClassMeta", metatable->name);
}

TEST_CASE_FIXTURE(Fixture, "clone_sanitize_free_types")
{
    TypeVar freeTy(FreeTypeVar{TypeLevel{}});
    TypePackVar freeTp(FreeTypePack{TypeLevel{}});

    TypeArena dest;
    SeenTypes seenTypes;
    SeenTypePacks seenTypePacks;

    bool encounteredFreeType = false;
    TypeId clonedTy = clone(&freeTy, dest, seenTypes, seenTypePacks, &encounteredFreeType);
    CHECK(Luau::get<ErrorTypeVar>(clonedTy));
    CHECK(encounteredFreeType);

    encounteredFreeType = false;
    TypePackId clonedTp = clone(&freeTp, dest, seenTypes, seenTypePacks, &encounteredFreeType);
    CHECK(Luau::get<Unifiable::Error>(clonedTp));
    CHECK(encounteredFreeType);
}

TEST_CASE_FIXTURE(Fixture, "clone_seal_free_tables")
{
    TypeVar tableTy{TableTypeVar{}};
    TableTypeVar* ttv = getMutable<TableTypeVar>(&tableTy);
    ttv->state = TableState::Free;

    TypeArena dest;
    SeenTypes seenTypes;
    SeenTypePacks seenTypePacks;

    bool encounteredFreeType = false;
    TypeId cloned = clone(&tableTy, dest, seenTypes, seenTypePacks, &encounteredFreeType);
    const TableTypeVar* clonedTtv = get<TableTypeVar>(cloned);
    CHECK_EQ(clonedTtv->state, TableState::Sealed);
    CHECK(encounteredFreeType);
}

TEST_CASE_FIXTURE(Fixture, "clone_self_property")
{
    fileResolver.source["Module/A"] = R"(
        --!nonstrict
        local a = {}
        function a:foo(x)
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
    LUAU_REQUIRE_ERRORS(result);

    CHECK_EQ(toString(result.errors[0]), "This function was declared to accept self, but you did not pass enough arguments. Use a colon instead of a "
                                         "dot or pass 1 extra nil to suppress this warning");
}

TEST_SUITE_END();
