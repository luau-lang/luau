// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "doctest.h"
#include "Fixture.h"

#include "Luau/Subtyping.h"

using namespace Luau;

struct SubtypeFixture : Fixture
{
    TypeArena arena;
    InternalErrorReporter ice;
    UnifierSharedState sharedState{&ice};
    Normalizer normalizer{&arena, builtinTypes, NotNull{&sharedState}};
    Subtyping subtyping{builtinTypes, NotNull{&normalizer}};

    TypePackId pack(std::initializer_list<TypeId> tys)
    {
        return arena.addTypePack(tys);
    }

    TypePackId pack(std::initializer_list<TypeId> tys, TypePackVariant tail)
    {
        return arena.addTypePack(tys, arena.addTypePack(std::move(tail)));
    }

    TypeId fn(std::initializer_list<TypeId> args, std::initializer_list<TypeId> rets)
    {
        return arena.addType(FunctionType{pack(args), pack(rets)});
    }

    TypeId fn(std::initializer_list<TypeId> argHead, TypePackVariant argTail, std::initializer_list<TypeId> rets)
    {
        return arena.addType(FunctionType{pack(argHead, std::move(argTail)), pack(rets)});
    }

    TypeId fn(std::initializer_list<TypeId> args, std::initializer_list<TypeId> retHead, TypePackVariant retTail)
    {
        return arena.addType(FunctionType{pack(args), pack(retHead, std::move(retTail))});
    }

    TypeId fn(std::initializer_list<TypeId> argHead, TypePackVariant argTail, std::initializer_list<TypeId> retHead, TypePackVariant retTail)
    {
        return arena.addType(FunctionType{pack(argHead, std::move(argTail)), pack(retHead, std::move(retTail))});
    }

    SubtypingGraph isSubtype(TypeId subTy, TypeId superTy)
    {
        return subtyping.isSubtype(subTy, superTy);
    }

    TypeId helloType = arena.addType(SingletonType{StringSingleton{"hello"}});
    TypeId helloType2 = arena.addType(SingletonType{StringSingleton{"hello"}});
    TypeId worldType = arena.addType(SingletonType{StringSingleton{"world"}});

    TypeId helloOrWorldType = arena.addType(UnionType{{helloType, worldType}});
    TypeId trueOrFalseType = arena.addType(UnionType{{builtinTypes->trueType, builtinTypes->falseType}});

    TypeId helloAndWorldType = arena.addType(IntersectionType{{helloType, worldType}});
    TypeId booleanAndTrueType = arena.addType(IntersectionType{{builtinTypes->booleanType, builtinTypes->trueType}});

    // (number) -> string
    const TypeId numberToStringType = fn(
        {builtinTypes->numberType},
        {builtinTypes->stringType}
    );

    // (unknown) -> string
    const TypeId unknownToStringType = fn(
        {builtinTypes->unknownType},
        {builtinTypes->stringType}
    );

    // (number) -> unknown
    const TypeId numberToUnknownType = fn(
        {builtinTypes->numberType},
        {builtinTypes->unknownType}
    );

    // (number) -> (string, string)
    const TypeId numberToTwoStringsType = fn(
        {builtinTypes->numberType},
        {builtinTypes->stringType, builtinTypes->stringType}
    );

    // (number) -> (string, unknown)
    const TypeId numberToStringAndUnknownType = fn(
        {builtinTypes->numberType},
        {builtinTypes->stringType, builtinTypes->unknownType}
    );

    // (number, number) -> string
    const TypeId numberNumberToStringType = fn(
        {builtinTypes->numberType, builtinTypes->numberType},
        {builtinTypes->stringType}
    );

    // (unknown, number) -> string
    const TypeId unknownNumberToStringType = fn(
        {builtinTypes->unknownType, builtinTypes->numberType},
        {builtinTypes->stringType}
    );

    // (number, string) -> string
    const TypeId numberAndStringToStringType = fn(
        {builtinTypes->numberType, builtinTypes->stringType},
        {builtinTypes->stringType}
    );

    // (number, ...string) -> string
    const TypeId numberAndStringsToStringType = fn(
        {builtinTypes->numberType}, VariadicTypePack{builtinTypes->stringType},
        {builtinTypes->stringType}
    );

    // (number, ...string?) -> string
    const TypeId numberAndOptionalStringsToStringType = fn(
        {builtinTypes->numberType}, VariadicTypePack{builtinTypes->optionalStringType},
        {builtinTypes->stringType}
    );

};

#define CHECK_IS_SUBTYPE(left, right) \
    do \
    { \
        const auto& leftTy = (left); \
        const auto& rightTy = (right); \
        SubtypingGraph result = isSubtype(leftTy, rightTy); \
        CHECK_MESSAGE(result.isSubtype, "Expected " << leftTy << " <: " << rightTy); \
    } while (0)

#define CHECK_IS_NOT_SUBTYPE(left, right) \
    do \
    { \
        const auto& leftTy = (left); \
        const auto& rightTy = (right); \
        SubtypingGraph result = isSubtype(leftTy, rightTy); \
        CHECK_MESSAGE(!result.isSubtype, "Expected " << leftTy << " </: " << rightTy); \
    } while (0)

#define CHECK_IS_ERROR_SUPPRESSING(left, right) \
    do \
    { \
        const auto& leftTy = (left); \
        const auto& rightTy = (right); \
        SubtypingGraph result = isSubtype(leftTy, rightTy); \
        CHECK_MESSAGE(result.isErrorSuppressing, "Expected " << leftTy << " to error-suppress " << rightTy); \
    } while (0)

#define CHECK_IS_NOT_ERROR_SUPPRESSING(left, right) \
    do \
    { \
        const auto& leftTy = (left); \
        const auto& rightTy = (right); \
        SubtypingGraph result = isSubtype(leftTy, rightTy); \
        CHECK_MESSAGE(!result.isErrorSuppressing, "Expected " << leftTy << " to error-suppress " << rightTy); \
    } while (0)

TEST_SUITE_BEGIN("Subtyping");

// We would like to write </: to mean "is not a subtype," but rotest does not like that at all, so we instead use <!:

TEST_CASE_FIXTURE(SubtypeFixture, "number <: any")
{
    CHECK_IS_SUBTYPE(builtinTypes->numberType, builtinTypes->anyType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "any <!: unknown")
{
    SubtypingGraph result = isSubtype(builtinTypes->anyType, builtinTypes->unknownType);
    CHECK(!result.isSubtype);
    CHECK(result.isErrorSuppressing);
}

TEST_CASE_FIXTURE(SubtypeFixture, "number? <: unknown")
{
    CHECK_IS_SUBTYPE(builtinTypes->optionalNumberType, builtinTypes->unknownType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "number <: unknown")
{
    CHECK_IS_SUBTYPE(builtinTypes->numberType, builtinTypes->unknownType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "number <: number")
{
    CHECK_IS_SUBTYPE(builtinTypes->numberType, builtinTypes->numberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "number <!: string")
{
    CHECK_IS_NOT_SUBTYPE(builtinTypes->numberType, builtinTypes->stringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "number <: number?")
{
    CHECK_IS_SUBTYPE(builtinTypes->numberType, builtinTypes->optionalNumberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "\"hello\" <: string")
{
    CHECK_IS_SUBTYPE(helloType, builtinTypes->stringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "string <!: \"hello\"")
{
    CHECK_IS_NOT_SUBTYPE(builtinTypes->stringType, helloType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "\"hello\" <: \"hello\"")
{
    CHECK_IS_SUBTYPE(helloType, helloType2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "true <: boolean")
{
    CHECK_IS_SUBTYPE(builtinTypes->trueType, builtinTypes->booleanType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "true <: true | false")
{
    CHECK_IS_SUBTYPE(builtinTypes->trueType, trueOrFalseType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "true | false <!: true")
{
    CHECK_IS_NOT_SUBTYPE(trueOrFalseType, builtinTypes->trueType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "true | false <: boolean")
{
    CHECK_IS_SUBTYPE(trueOrFalseType, builtinTypes->booleanType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "true | false <: true | false")
{
    CHECK_IS_SUBTYPE(trueOrFalseType, trueOrFalseType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "\"hello\" | \"world\" <: number")
{
    CHECK_IS_NOT_SUBTYPE(helloOrWorldType, builtinTypes->numberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "true <: boolean & true")
{
    CHECK_IS_SUBTYPE(builtinTypes->trueType, booleanAndTrueType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "boolean & true <: true")
{
    CHECK_IS_SUBTYPE(booleanAndTrueType, builtinTypes->trueType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "boolean & true <: boolean & true")
{
    CHECK_IS_SUBTYPE(booleanAndTrueType, booleanAndTrueType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "\"hello\" & \"world\" <: number")
{
    CHECK_IS_SUBTYPE(helloAndWorldType, builtinTypes->numberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "false <!: boolean & true")
{
    CHECK_IS_NOT_SUBTYPE(builtinTypes->falseType, booleanAndTrueType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(unknown) -> string <: (number) -> string")
{
    CHECK_IS_SUBTYPE(unknownToStringType, numberToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number) -> string <!: (unknown) -> string")
{
    CHECK_IS_NOT_SUBTYPE(numberToStringType, unknownToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number, number) -> string <!: (number) -> string")
{
    CHECK_IS_NOT_SUBTYPE(numberNumberToStringType, numberToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number) -> string <!: (number, number) -> string")
{
    CHECK_IS_NOT_SUBTYPE(numberToStringType, numberNumberToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number, number) -> string <!: (unknown, number) -> string")
{
    CHECK_IS_NOT_SUBTYPE(numberNumberToStringType, unknownNumberToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(unknown, number) -> string <: (number, number) -> string")
{
    CHECK_IS_SUBTYPE(unknownNumberToStringType, numberNumberToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number) -> (string, unknown) <!: (number) -> (string, string)")
{
    CHECK_IS_NOT_SUBTYPE(numberToStringAndUnknownType, numberToTwoStringsType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number) -> (string, string) <: (number) -> (string, unknown)")
{
    CHECK_IS_SUBTYPE(numberToTwoStringsType, numberToStringAndUnknownType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number) -> (string, string) <!: (number) -> string")
{
    CHECK_IS_NOT_SUBTYPE(numberToTwoStringsType, numberToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number) -> string <!: (number) -> (string, string)")
{
    CHECK_IS_NOT_SUBTYPE(numberToStringType, numberToTwoStringsType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number, ...string) -> string <: (number) -> string")
{
    CHECK_IS_SUBTYPE(numberAndStringsToStringType, numberToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number) -> string <!: (number, ...string) -> string")
{
    CHECK_IS_NOT_SUBTYPE(numberToStringType, numberAndStringsToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number, ...string?) -> string <: (number, ...string) -> string")
{
    CHECK_IS_SUBTYPE(numberAndOptionalStringsToStringType, numberAndStringsToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number, ...string) -> string <!: (number, ...string?) -> string")
{
    CHECK_IS_NOT_SUBTYPE(numberAndStringsToStringType, numberAndOptionalStringsToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number, ...string) -> string <: (number, string) -> string")
{
    CHECK_IS_SUBTYPE(numberAndStringsToStringType, numberAndStringToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number, string) -> string <!: (number, ...string) -> string")
{
    CHECK_IS_NOT_SUBTYPE(numberAndStringToStringType, numberAndStringsToStringType);
}

TEST_SUITE_END();
