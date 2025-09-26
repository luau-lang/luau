// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Scope.h"
#include "Luau/Type.h"
#include "Luau/TypeInfer.h"
#include "Luau/VisitType.h"

#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;


TEST_SUITE_BEGIN("TypeTests");

TEST_CASE_FIXTURE(Fixture, "primitives_are_equal")
{
    REQUIRE_EQ(getBuiltins()->booleanType, getBuiltins()->booleanType);
}

TEST_CASE_FIXTURE(Fixture, "bound_type_is_equal_to_that_which_it_is_bound")
{
    Type bound(BoundType(getBuiltins()->booleanType));
    REQUIRE_EQ(bound, *getBuiltins()->booleanType);
}

TEST_CASE_FIXTURE(Fixture, "equivalent_cyclic_tables_are_equal")
{
    Type cycleOne{TypeVariant(TableType())};
    TableType* tableOne = getMutable<TableType>(&cycleOne);
    tableOne->props["self"] = {&cycleOne};

    Type cycleTwo{TypeVariant(TableType())};
    TableType* tableTwo = getMutable<TableType>(&cycleTwo);
    tableTwo->props["self"] = {&cycleTwo};

    CHECK_EQ(cycleOne, cycleTwo);
}

TEST_CASE_FIXTURE(Fixture, "different_cyclic_tables_are_not_equal")
{
    Type cycleOne{TypeVariant(TableType())};
    TableType* tableOne = getMutable<TableType>(&cycleOne);
    tableOne->props["self"] = {&cycleOne};

    Type cycleTwo{TypeVariant(TableType())};
    TableType* tableTwo = getMutable<TableType>(&cycleTwo);
    tableTwo->props["this"] = {&cycleTwo};

    CHECK_NE(cycleOne, cycleTwo);
}

TEST_CASE_FIXTURE(Fixture, "return_type_of_function_is_not_parenthesized_if_just_one_value")
{
    auto emptyArgumentPack = TypePackVar{TypePack{}};
    auto returnPack = TypePackVar{TypePack{{getBuiltins()->numberType}}};
    auto returnsTwo = Type(FunctionType(getFrontend().globals.globalScope->level, &emptyArgumentPack, &returnPack));

    std::string res = toString(&returnsTwo);
    CHECK_EQ("() -> number", res);
}

TEST_CASE_FIXTURE(Fixture, "return_type_of_function_is_parenthesized_if_not_just_one_value")
{
    auto emptyArgumentPack = TypePackVar{TypePack{}};
    auto returnPack = TypePackVar{TypePack{{getBuiltins()->numberType, getBuiltins()->numberType}}};
    auto returnsTwo = Type(FunctionType(getFrontend().globals.globalScope->level, &emptyArgumentPack, &returnPack));

    std::string res = toString(&returnsTwo);
    CHECK_EQ("() -> (number, number)", res);
}

TEST_CASE_FIXTURE(Fixture, "return_type_of_function_is_parenthesized_if_tail_is_free")
{
    auto emptyArgumentPack = TypePackVar{TypePack{}};
    auto free = FreeTypePack(TypeLevel());
    auto freePack = TypePackVar{TypePackVariant{free}};
    auto returnPack = TypePackVar{TypePack{{getBuiltins()->numberType}, &freePack}};
    auto returnsTwo = Type(FunctionType(getFrontend().globals.globalScope->level, &emptyArgumentPack, &returnPack));

    std::string res = toString(&returnsTwo);
    CHECK_EQ(res, "() -> (number, a...)");
}

TEST_CASE_FIXTURE(Fixture, "subset_check")
{
    UnionType super, sub, notSub;
    super.options = {getBuiltins()->numberType, getBuiltins()->stringType, getBuiltins()->booleanType};
    sub.options = {getBuiltins()->numberType, getBuiltins()->stringType};
    notSub.options = {getBuiltins()->numberType, getBuiltins()->nilType};

    CHECK(isSubset(super, sub));
    CHECK(!isSubset(super, notSub));
}

TEST_CASE_FIXTURE(Fixture, "iterate_over_UnionType")
{
    UnionType utv;
    utv.options = {getBuiltins()->numberType, getBuiltins()->stringType, getBuiltins()->anyType};

    std::vector<TypeId> result;
    for (TypeId ty : &utv)
        result.push_back(ty);

    CHECK(result == utv.options);
}

TEST_CASE_FIXTURE(Fixture, "iterating_over_nested_UnionTypes")
{
    Type subunion{UnionType{}};
    UnionType* innerUtv = getMutable<UnionType>(&subunion);
    innerUtv->options = {getBuiltins()->numberType, getBuiltins()->stringType};

    UnionType utv;
    utv.options = {getBuiltins()->anyType, &subunion};

    std::vector<TypeId> result;
    for (TypeId ty : &utv)
        result.push_back(ty);

    REQUIRE_EQ(result.size(), 3);
    CHECK_EQ(result[0], getBuiltins()->anyType);
    CHECK_EQ(result[2], getBuiltins()->stringType);
    CHECK_EQ(result[1], getBuiltins()->numberType);
}

TEST_CASE_FIXTURE(Fixture, "iterating_over_nested_UnionTypes_postfix_operator_plus_plus")
{
    Type subunion{UnionType{}};
    UnionType* innerUtv = getMutable<UnionType>(&subunion);
    innerUtv->options = {getBuiltins()->numberType, getBuiltins()->stringType};

    UnionType utv;
    utv.options = {getBuiltins()->anyType, &subunion};

    std::vector<TypeId> result;
    for (auto it = begin(&utv); it != end(&utv); it++)
        result.push_back(*it);

    REQUIRE_EQ(result.size(), 3);
    CHECK_EQ(result[0], getBuiltins()->anyType);
    CHECK_EQ(result[2], getBuiltins()->stringType);
    CHECK_EQ(result[1], getBuiltins()->numberType);
}

TEST_CASE_FIXTURE(Fixture, "iterator_detects_cyclic_UnionTypes_and_skips_over_them")
{
    Type atv{UnionType{}};
    UnionType* utv1 = getMutable<UnionType>(&atv);

    Type btv{UnionType{}};
    UnionType* utv2 = getMutable<UnionType>(&btv);
    utv2->options.push_back(getBuiltins()->numberType);
    utv2->options.push_back(getBuiltins()->stringType);
    utv2->options.push_back(&atv);

    utv1->options.push_back(&btv);

    std::vector<TypeId> result;
    for (TypeId ty : utv2)
        result.push_back(ty);

    REQUIRE_EQ(result.size(), 2);
    CHECK_EQ(result[0], getBuiltins()->numberType);
    CHECK_EQ(result[1], getBuiltins()->stringType);
}

TEST_CASE_FIXTURE(Fixture, "iterator_descends_on_nested_in_first_operator*")
{
    Type tv1{UnionType{{getBuiltins()->stringType, getBuiltins()->numberType}}};
    Type tv2{UnionType{{&tv1, getBuiltins()->booleanType}}};
    auto utv = get<UnionType>(&tv2);

    std::vector<TypeId> result;
    for (TypeId ty : utv)
        result.push_back(ty);

    REQUIRE_EQ(result.size(), 3);
    CHECK_EQ(result[0], getBuiltins()->stringType);
    CHECK_EQ(result[1], getBuiltins()->numberType);
    CHECK_EQ(result[2], getBuiltins()->booleanType);
}

TEST_CASE_FIXTURE(Fixture, "UnionTypeIterator_with_vector_iter_ctor")
{
    Type tv1{UnionType{{getBuiltins()->stringType, getBuiltins()->numberType}}};
    Type tv2{UnionType{{&tv1, getBuiltins()->booleanType}}};
    auto utv = get<UnionType>(&tv2);

    std::vector<TypeId> actual(begin(utv), end(utv));
    std::vector<TypeId> expected{getBuiltins()->stringType, getBuiltins()->numberType, getBuiltins()->booleanType};
    CHECK_EQ(actual, expected);
}

TEST_CASE_FIXTURE(Fixture, "UnionTypeIterator_with_empty_union")
{
    Type tv{UnionType{}};
    auto utv = get<UnionType>(&tv);

    std::vector<TypeId> actual(begin(utv), end(utv));
    CHECK(actual.empty());
}

TEST_CASE_FIXTURE(Fixture, "UnionTypeIterator_with_only_cyclic_union")
{
    Type tv{UnionType{}};
    auto utv = getMutable<UnionType>(&tv);
    utv->options.push_back(&tv);
    utv->options.push_back(&tv);

    std::vector<TypeId> actual(begin(utv), end(utv));
    CHECK(actual.empty());
}


/* FIXME: This test is pretty weird.  It would be much nicer if we could
 * perform this operation without a TypeChecker so that we don't have to jam
 * all this state into it to make stuff work.
 */
TEST_CASE_FIXTURE(Fixture, "substitution_skip_failure")
{
    Type ftv11{FreeType{TypeLevel{}, getBuiltins()->neverType, getBuiltins()->unknownType}};

    TypePackVar tp24{TypePack{{&ftv11}}};
    TypePackVar tp17{TypePack{}};

    Type ftv23{FunctionType{&tp24, &tp17}};

    Type ttvConnection2{TableType{}};
    TableType* ttvConnection2_ = getMutable<TableType>(&ttvConnection2);
    ttvConnection2_->instantiatedTypeParams.push_back(&ftv11);
    ttvConnection2_->props["f"] = {&ftv23};

    TypePackVar tp21{TypePack{{&ftv11}}};
    TypePackVar tp20{TypePack{}};

    Type ftv19{FunctionType{&tp21, &tp20}};

    Type ttvSignal{TableType{}};
    TableType* ttvSignal_ = getMutable<TableType>(&ttvSignal);
    ttvSignal_->instantiatedTypeParams.push_back(&ftv11);
    ttvSignal_->props["f"] = {&ftv19};

    // Back edge
    ttvConnection2_->props["signal"] = {&ttvSignal};

    Type gtvK2{GenericType{}};
    Type gtvV2{GenericType{}};

    Type ttvTweenResult2{TableType{}};
    TableType* ttvTweenResult2_ = getMutable<TableType>(&ttvTweenResult2);
    ttvTweenResult2_->instantiatedTypeParams.push_back(&gtvK2);
    ttvTweenResult2_->instantiatedTypeParams.push_back(&gtvV2);

    TypePackVar tp13{TypePack{{&ttvTweenResult2}}};
    Type ftv12{FunctionType{&tp13, &tp17}};

    Type ttvConnection{TableType{}};
    TableType* ttvConnection_ = getMutable<TableType>(&ttvConnection);
    ttvConnection_->instantiatedTypeParams.push_back(&ttvTweenResult2);
    ttvConnection_->props["f"] = {&ftv12};
    ttvConnection_->props["signal"] = {&ttvSignal};

    TypePackVar tp9{TypePack{}};
    TypePackVar tp10{TypePack{{&ttvConnection}}};

    Type ftv8{FunctionType{&tp9, &tp10}};

    Type ttvTween{TableType{}};
    TableType* ttvTween_ = getMutable<TableType>(&ttvTween);
    ttvTween_->instantiatedTypeParams.push_back(&gtvK2);
    ttvTween_->instantiatedTypeParams.push_back(&gtvV2);
    ttvTween_->props["f"] = {&ftv8};

    TypePackVar tp4{TypePack{}};
    TypePackVar tp5{TypePack{{&ttvTween}}};

    Type ftv3{FunctionType{&tp4, &tp5}};

    // Back edge
    ttvTweenResult2_->props["f"] = {&ftv3};

    Type gtvK{GenericType{}};
    Type gtvV{GenericType{}};

    Type ttvTweenResult{TableType{}};
    TableType* ttvTweenResult_ = getMutable<TableType>(&ttvTweenResult);
    ttvTweenResult_->instantiatedTypeParams.push_back(&gtvK);
    ttvTweenResult_->instantiatedTypeParams.push_back(&gtvV);
    ttvTweenResult_->props["f"] = {&ftv3};

    TypeId root = &ttvTweenResult;

    ModulePtr currentModule = std::make_shared<Module>();
    Anyification anyification(
        &currentModule->internalTypes,
        getFrontend().globals.globalScope,
        getBuiltins(),
        &getFrontend().iceHandler,
        getBuiltins()->anyType,
        getBuiltins()->anyTypePack
    );
    std::optional<TypeId> any = anyification.substitute(root);

    REQUIRE(!anyification.normalizationTooComplex);
    REQUIRE(any.has_value());
    CHECK_EQ("{ f: t1 } where t1 = () -> { f: () -> { f: ({ f: t1 }) -> (), signal: { f: (any) -> () } } }", toString(*any));
}

TEST_CASE("tagging_tables")
{
    Type ttv{TableType{}};
    CHECK(!Luau::hasTag(&ttv, "foo"));
    Luau::attachTag(&ttv, "foo");
    CHECK(Luau::hasTag(&ttv, "foo"));
}

TEST_CASE("tagging_extern_types")
{
    Type base{ExternType{"Base", {}, std::nullopt, std::nullopt, {}, nullptr, "Test", {}}};
    CHECK(!Luau::hasTag(&base, "foo"));
    Luau::attachTag(&base, "foo");
    CHECK(Luau::hasTag(&base, "foo"));
}

TEST_CASE("tagging_subextern_types")
{
    Type base{ExternType{"Base", {}, std::nullopt, std::nullopt, {}, nullptr, "Test", {}}};
    Type derived{ExternType{"Derived", {}, &base, std::nullopt, {}, nullptr, "Test", {}}};

    CHECK(!Luau::hasTag(&base, "foo"));
    CHECK(!Luau::hasTag(&derived, "foo"));

    Luau::attachTag(&base, "foo");
    CHECK(Luau::hasTag(&base, "foo"));
    CHECK(Luau::hasTag(&derived, "foo"));

    Luau::attachTag(&derived, "bar");
    CHECK(!Luau::hasTag(&base, "bar"));
    CHECK(Luau::hasTag(&derived, "bar"));
}

TEST_CASE("tagging_functions")
{
    TypePackVar empty{TypePack{}};
    Type ftv{FunctionType{&empty, &empty}};
    CHECK(!Luau::hasTag(&ftv, "foo"));
    Luau::attachTag(&ftv, "foo");
    CHECK(Luau::hasTag(&ftv, "foo"));
}

TEST_CASE("tagging_props")
{
    Property prop{};
    CHECK(!Luau::hasTag(prop, "foo"));
    Luau::attachTag(prop, "foo");
    CHECK(Luau::hasTag(prop, "foo"));
}

struct VisitCountTracker final : TypeOnceVisitor
{
    std::unordered_map<TypeId, unsigned> tyVisits;
    std::unordered_map<TypePackId, unsigned> tpVisits;

    VisitCountTracker()
        : TypeOnceVisitor("VisitCountTracker", /* skipBoundTypes */ true)
    {
    }


    void cycle(TypeId) override {}
    void cycle(TypePackId) override {}

    template<typename T>
    bool operator()(TypeId ty, const T& t)
    {
        return visit(ty);
    }

    template<typename T>
    bool operator()(TypePackId tp, const T&)
    {
        return visit(tp);
    }

    bool visit(TypeId ty) override
    {
        tyVisits[ty]++;
        return true;
    }

    bool visit(TypePackId tp) override
    {
        tpVisits[tp]++;
        return true;
    }
};

TEST_CASE_FIXTURE(Fixture, "visit_once")
{
    CheckResult result = check(R"(
type T = { a: number, b: () -> () }
local b: (T, T, T) -> T
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId bType = requireType("b");

    VisitCountTracker tester;
    tester.traverse(bType);

    for (auto [_, count] : tester.tyVisits)
        CHECK_EQ(count, 1);

    for (auto [_, count] : tester.tpVisits)
        CHECK_EQ(count, 1);
}

TEST_CASE("isString_on_string_singletons")
{
    Type helloString{SingletonType{StringSingleton{"hello"}}};
    CHECK(isString(&helloString));
}

TEST_CASE("isString_on_unions_of_various_string_singletons")
{
    Type helloString{SingletonType{StringSingleton{"hello"}}};
    Type byeString{SingletonType{StringSingleton{"bye"}}};
    Type union_{UnionType{{&helloString, &byeString}}};

    CHECK(isString(&union_));
}

TEST_CASE("proof_that_isString_uses_all_of")
{
    Type helloString{SingletonType{StringSingleton{"hello"}}};
    Type byeString{SingletonType{StringSingleton{"bye"}}};
    Type booleanType{PrimitiveType{PrimitiveType::Boolean}};
    Type union_{UnionType{{&helloString, &byeString, &booleanType}}};

    CHECK(!isString(&union_));
}

TEST_CASE("isBoolean_on_boolean_singletons")
{
    Type trueBool{SingletonType{BooleanSingleton{true}}};
    CHECK(isBoolean(&trueBool));
}

TEST_CASE("isBoolean_on_unions_of_true_or_false_singletons")
{
    Type trueBool{SingletonType{BooleanSingleton{true}}};
    Type falseBool{SingletonType{BooleanSingleton{false}}};
    Type union_{UnionType{{&trueBool, &falseBool}}};

    CHECK(isBoolean(&union_));
}

TEST_CASE("proof_that_isBoolean_uses_all_of")
{
    Type trueBool{SingletonType{BooleanSingleton{true}}};
    Type falseBool{SingletonType{BooleanSingleton{false}}};
    Type stringType{PrimitiveType{PrimitiveType::String}};
    Type union_{UnionType{{&trueBool, &falseBool, &stringType}}};

    CHECK(!isBoolean(&union_));
}

TEST_CASE("content_reassignment")
{
    Type myAny{AnyType{}, /*presistent*/ true};
    myAny.documentationSymbol = "@global/any";

    TypeArena arena;
    BuiltinTypes builtinTypes;
    TypeId futureAny = arena.freshType(NotNull{&builtinTypes}, TypeLevel{});
    asMutable(futureAny)->reassign(myAny);

    CHECK(get<AnyType>(futureAny) != nullptr);
    CHECK(!futureAny->persistent);
    CHECK(futureAny->documentationSymbol == "@global/any");
    CHECK(futureAny->owningArena == &arena);
}

TEST_SUITE_END();
