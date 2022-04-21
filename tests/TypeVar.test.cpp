// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"

#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("TypeVarTests");

TEST_CASE_FIXTURE(Fixture, "primitives_are_equal")
{
    REQUIRE_EQ(typeChecker.booleanType, typeChecker.booleanType);
}

TEST_CASE_FIXTURE(Fixture, "bound_type_is_equal_to_that_which_it_is_bound")
{
    TypeVar bound(BoundTypeVar(typeChecker.booleanType));
    REQUIRE_EQ(bound, *typeChecker.booleanType);
}

TEST_CASE_FIXTURE(Fixture, "equivalent_cyclic_tables_are_equal")
{
    TypeVar cycleOne{TypeVariant(TableTypeVar())};
    TableTypeVar* tableOne = getMutable<TableTypeVar>(&cycleOne);
    tableOne->props["self"] = {&cycleOne};

    TypeVar cycleTwo{TypeVariant(TableTypeVar())};
    TableTypeVar* tableTwo = getMutable<TableTypeVar>(&cycleTwo);
    tableTwo->props["self"] = {&cycleTwo};

    CHECK_EQ(cycleOne, cycleTwo);
}

TEST_CASE_FIXTURE(Fixture, "different_cyclic_tables_are_not_equal")
{
    TypeVar cycleOne{TypeVariant(TableTypeVar())};
    TableTypeVar* tableOne = getMutable<TableTypeVar>(&cycleOne);
    tableOne->props["self"] = {&cycleOne};

    TypeVar cycleTwo{TypeVariant(TableTypeVar())};
    TableTypeVar* tableTwo = getMutable<TableTypeVar>(&cycleTwo);
    tableTwo->props["this"] = {&cycleTwo};

    CHECK_NE(cycleOne, cycleTwo);
}

TEST_CASE_FIXTURE(Fixture, "return_type_of_function_is_not_parenthesized_if_just_one_value")
{
    auto emptyArgumentPack = TypePackVar{TypePack{}};
    auto returnPack = TypePackVar{TypePack{{typeChecker.numberType}}};
    auto returnsTwo = TypeVar(FunctionTypeVar(typeChecker.globalScope->level, &emptyArgumentPack, &returnPack));

    std::string res = toString(&returnsTwo);
    CHECK_EQ("() -> number", res);
}

TEST_CASE_FIXTURE(Fixture, "return_type_of_function_is_parenthesized_if_not_just_one_value")
{
    auto emptyArgumentPack = TypePackVar{TypePack{}};
    auto returnPack = TypePackVar{TypePack{{typeChecker.numberType, typeChecker.numberType}}};
    auto returnsTwo = TypeVar(FunctionTypeVar(typeChecker.globalScope->level, &emptyArgumentPack, &returnPack));

    std::string res = toString(&returnsTwo);
    CHECK_EQ("() -> (number, number)", res);
}

TEST_CASE_FIXTURE(Fixture, "return_type_of_function_is_parenthesized_if_tail_is_free")
{
    auto emptyArgumentPack = TypePackVar{TypePack{}};
    auto free = Unifiable::Free(TypeLevel());
    auto freePack = TypePackVar{TypePackVariant{free}};
    auto returnPack = TypePackVar{TypePack{{typeChecker.numberType}, &freePack}};
    auto returnsTwo = TypeVar(FunctionTypeVar(typeChecker.globalScope->level, &emptyArgumentPack, &returnPack));

    std::string res = toString(&returnsTwo);
    CHECK_EQ(res, "() -> (number, a...)");
}

TEST_CASE_FIXTURE(Fixture, "subset_check")
{
    UnionTypeVar super, sub, notSub;
    super.options = {typeChecker.numberType, typeChecker.stringType, typeChecker.booleanType};
    sub.options = {typeChecker.numberType, typeChecker.stringType};
    notSub.options = {typeChecker.numberType, typeChecker.nilType};

    CHECK(isSubset(super, sub));
    CHECK(!isSubset(super, notSub));
}

TEST_CASE_FIXTURE(Fixture, "iterate_over_UnionTypeVar")
{
    UnionTypeVar utv;
    utv.options = {typeChecker.numberType, typeChecker.stringType, typeChecker.anyType};

    std::vector<TypeId> result;
    for (TypeId ty : &utv)
        result.push_back(ty);

    CHECK(result == utv.options);
}

TEST_CASE_FIXTURE(Fixture, "iterating_over_nested_UnionTypeVars")
{
    TypeVar subunion{UnionTypeVar{}};
    UnionTypeVar* innerUtv = getMutable<UnionTypeVar>(&subunion);
    innerUtv->options = {typeChecker.numberType, typeChecker.stringType};

    UnionTypeVar utv;
    utv.options = {typeChecker.anyType, &subunion};

    std::vector<TypeId> result;
    for (TypeId ty : &utv)
        result.push_back(ty);

    REQUIRE_EQ(result.size(), 3);
    CHECK_EQ(result[0], typeChecker.anyType);
    CHECK_EQ(result[2], typeChecker.stringType);
    CHECK_EQ(result[1], typeChecker.numberType);
}

TEST_CASE_FIXTURE(Fixture, "iterator_detects_cyclic_UnionTypeVars_and_skips_over_them")
{
    TypeVar atv{UnionTypeVar{}};
    UnionTypeVar* utv1 = getMutable<UnionTypeVar>(&atv);

    TypeVar btv{UnionTypeVar{}};
    UnionTypeVar* utv2 = getMutable<UnionTypeVar>(&btv);
    utv2->options.push_back(typeChecker.numberType);
    utv2->options.push_back(typeChecker.stringType);
    utv2->options.push_back(&atv);

    utv1->options.push_back(&btv);

    std::vector<TypeId> result;
    for (TypeId ty : utv2)
        result.push_back(ty);

    REQUIRE_EQ(result.size(), 2);
    CHECK_EQ(result[0], typeChecker.numberType);
    CHECK_EQ(result[1], typeChecker.stringType);
}

TEST_CASE_FIXTURE(Fixture, "iterator_descends_on_nested_in_first_operator*")
{
    TypeVar tv1{UnionTypeVar{{typeChecker.stringType, typeChecker.numberType}}};
    TypeVar tv2{UnionTypeVar{{&tv1, typeChecker.booleanType}}};
    auto utv = get<UnionTypeVar>(&tv2);

    std::vector<TypeId> result;
    for (TypeId ty : utv)
        result.push_back(ty);

    REQUIRE_EQ(result.size(), 3);
    CHECK_EQ(result[0], typeChecker.stringType);
    CHECK_EQ(result[1], typeChecker.numberType);
    CHECK_EQ(result[2], typeChecker.booleanType);
}

TEST_CASE_FIXTURE(Fixture, "UnionTypeVarIterator_with_vector_iter_ctor")
{
    TypeVar tv1{UnionTypeVar{{typeChecker.stringType, typeChecker.numberType}}};
    TypeVar tv2{UnionTypeVar{{&tv1, typeChecker.booleanType}}};
    auto utv = get<UnionTypeVar>(&tv2);

    std::vector<TypeId> actual(begin(utv), end(utv));
    std::vector<TypeId> expected{typeChecker.stringType, typeChecker.numberType, typeChecker.booleanType};
    CHECK_EQ(actual, expected);
}

TEST_CASE_FIXTURE(Fixture, "UnionTypeVarIterator_with_empty_union")
{
    TypeVar tv{UnionTypeVar{}};
    auto utv = get<UnionTypeVar>(&tv);

    std::vector<TypeId> actual(begin(utv), end(utv));
    CHECK(actual.empty());
}

TEST_CASE_FIXTURE(Fixture, "substitution_skip_failure")
{
    ScopedFastFlag sff{"LuauSealExports", true};

    TypeVar ftv11{FreeTypeVar{TypeLevel{}}};

    TypePackVar tp24{TypePack{{&ftv11}}};
    TypePackVar tp17{TypePack{}};

    TypeVar ftv23{FunctionTypeVar{&tp24, &tp17}};

    TypeVar ttvConnection2{TableTypeVar{}};
    TableTypeVar* ttvConnection2_ = getMutable<TableTypeVar>(&ttvConnection2);
    ttvConnection2_->instantiatedTypeParams.push_back(&ftv11);
    ttvConnection2_->props["f"] = {&ftv23};

    TypePackVar tp21{TypePack{{&ftv11}}};
    TypePackVar tp20{TypePack{}};

    TypeVar ftv19{FunctionTypeVar{&tp21, &tp20}};

    TypeVar ttvSignal{TableTypeVar{}};
    TableTypeVar* ttvSignal_ = getMutable<TableTypeVar>(&ttvSignal);
    ttvSignal_->instantiatedTypeParams.push_back(&ftv11);
    ttvSignal_->props["f"] = {&ftv19};

    // Back edge
    ttvConnection2_->props["signal"] = {&ttvSignal};

    TypeVar gtvK2{GenericTypeVar{}};
    TypeVar gtvV2{GenericTypeVar{}};

    TypeVar ttvTweenResult2{TableTypeVar{}};
    TableTypeVar* ttvTweenResult2_ = getMutable<TableTypeVar>(&ttvTweenResult2);
    ttvTweenResult2_->instantiatedTypeParams.push_back(&gtvK2);
    ttvTweenResult2_->instantiatedTypeParams.push_back(&gtvV2);

    TypePackVar tp13{TypePack{{&ttvTweenResult2}}};
    TypeVar ftv12{FunctionTypeVar{&tp13, &tp17}};

    TypeVar ttvConnection{TableTypeVar{}};
    TableTypeVar* ttvConnection_ = getMutable<TableTypeVar>(&ttvConnection);
    ttvConnection_->instantiatedTypeParams.push_back(&ttvTweenResult2);
    ttvConnection_->props["f"] = {&ftv12};
    ttvConnection_->props["signal"] = {&ttvSignal};

    TypePackVar tp9{TypePack{}};
    TypePackVar tp10{TypePack{{&ttvConnection}}};

    TypeVar ftv8{FunctionTypeVar{&tp9, &tp10}};

    TypeVar ttvTween{TableTypeVar{}};
    TableTypeVar* ttvTween_ = getMutable<TableTypeVar>(&ttvTween);
    ttvTween_->instantiatedTypeParams.push_back(&gtvK2);
    ttvTween_->instantiatedTypeParams.push_back(&gtvV2);
    ttvTween_->props["f"] = {&ftv8};

    TypePackVar tp4{TypePack{}};
    TypePackVar tp5{TypePack{{&ttvTween}}};

    TypeVar ftv3{FunctionTypeVar{&tp4, &tp5}};

    // Back edge
    ttvTweenResult2_->props["f"] = {&ftv3};

    TypeVar gtvK{GenericTypeVar{}};
    TypeVar gtvV{GenericTypeVar{}};

    TypeVar ttvTweenResult{TableTypeVar{}};
    TableTypeVar* ttvTweenResult_ = getMutable<TableTypeVar>(&ttvTweenResult);
    ttvTweenResult_->instantiatedTypeParams.push_back(&gtvK);
    ttvTweenResult_->instantiatedTypeParams.push_back(&gtvV);
    ttvTweenResult_->props["f"] = {&ftv3};

    TypeId root = &ttvTweenResult;

    typeChecker.currentModule = std::make_shared<Module>();

    TypeId result = typeChecker.anyify(typeChecker.globalScope, root, Location{});

    CHECK_EQ("{| f: t1 |} where t1 = () -> {| f: () -> {| f: ({| f: t1 |}) -> (), signal: {| f: (any) -> () |} |} |}", toString(result));
}

TEST_CASE("tagging_tables")
{
    TypeVar ttv{TableTypeVar{}};
    CHECK(!Luau::hasTag(&ttv, "foo"));
    Luau::attachTag(&ttv, "foo");
    CHECK(Luau::hasTag(&ttv, "foo"));
}

TEST_CASE("tagging_classes")
{
    TypeVar base{ClassTypeVar{"Base", {}, std::nullopt, std::nullopt, {}, nullptr, "Test"}};
    CHECK(!Luau::hasTag(&base, "foo"));
    Luau::attachTag(&base, "foo");
    CHECK(Luau::hasTag(&base, "foo"));
}

TEST_CASE("tagging_subclasses")
{
    TypeVar base{ClassTypeVar{"Base", {}, std::nullopt, std::nullopt, {}, nullptr, "Test"}};
    TypeVar derived{ClassTypeVar{"Derived", {}, &base, std::nullopt, {}, nullptr, "Test"}};

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
    TypeVar ftv{FunctionTypeVar{&empty, &empty}};
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

struct VisitCountTracker
{
    std::unordered_map<TypeId, unsigned> tyVisits;
    std::unordered_map<TypePackId, unsigned> tpVisits;

    void cycle(TypeId) {}
    void cycle(TypePackId) {}

    template<typename T>
    bool operator()(TypeId ty, const T& t)
    {
        tyVisits[ty]++;
        return true;
    }

    template<typename T>
    bool operator()(TypePackId tp, const T&)
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
    DenseHashSet<void*> seen{nullptr};
    visitTypeVarOnce(bType, tester, seen);

    for (auto [_, count] : tester.tyVisits)
        CHECK_EQ(count, 1);

    for (auto [_, count] : tester.tpVisits)
        CHECK_EQ(count, 1);
}

TEST_CASE("isString_on_string_singletons")
{
    TypeVar helloString{SingletonTypeVar{StringSingleton{"hello"}}};
    CHECK(isString(&helloString));
}

TEST_CASE("isString_on_unions_of_various_string_singletons")
{
    TypeVar helloString{SingletonTypeVar{StringSingleton{"hello"}}};
    TypeVar byeString{SingletonTypeVar{StringSingleton{"bye"}}};
    TypeVar union_{UnionTypeVar{{&helloString, &byeString}}};

    CHECK(isString(&union_));
}

TEST_CASE("proof_that_isString_uses_all_of")
{
    TypeVar helloString{SingletonTypeVar{StringSingleton{"hello"}}};
    TypeVar byeString{SingletonTypeVar{StringSingleton{"bye"}}};
    TypeVar booleanType{PrimitiveTypeVar{PrimitiveTypeVar::Boolean}};
    TypeVar union_{UnionTypeVar{{&helloString, &byeString, &booleanType}}};

    CHECK(!isString(&union_));
}

TEST_CASE("isBoolean_on_boolean_singletons")
{
    TypeVar trueBool{SingletonTypeVar{BooleanSingleton{true}}};
    CHECK(isBoolean(&trueBool));
}

TEST_CASE("isBoolean_on_unions_of_true_or_false_singletons")
{
    TypeVar trueBool{SingletonTypeVar{BooleanSingleton{true}}};
    TypeVar falseBool{SingletonTypeVar{BooleanSingleton{false}}};
    TypeVar union_{UnionTypeVar{{&trueBool, &falseBool}}};

    CHECK(isBoolean(&union_));
}

TEST_CASE("proof_that_isBoolean_uses_all_of")
{
    TypeVar trueBool{SingletonTypeVar{BooleanSingleton{true}}};
    TypeVar falseBool{SingletonTypeVar{BooleanSingleton{false}}};
    TypeVar stringType{PrimitiveTypeVar{PrimitiveTypeVar::String}};
    TypeVar union_{UnionTypeVar{{&trueBool, &falseBool, &stringType}}};

    CHECK(!isBoolean(&union_));
}

TEST_SUITE_END();
