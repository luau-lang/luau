// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "doctest.h"
#include "Fixture.h"

#include "Luau/Subtyping.h"
#include "Luau/TypePack.h"

using namespace Luau;

struct SubtypeFixture : Fixture
{
    TypeArena arena;
    InternalErrorReporter iceReporter;
    UnifierSharedState sharedState{&ice};
    Normalizer normalizer{&arena, builtinTypes, NotNull{&sharedState}};

    Subtyping subtyping{builtinTypes, NotNull{&arena}, NotNull{&normalizer}, NotNull{&iceReporter}};

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

    TypeId tbl(TableType::Props&& props)
    {
        return arena.addType(TableType{std::move(props), std::nullopt, {}, TableState::Sealed});
    }

    // `&`
    TypeId meet(TypeId a, TypeId b)
    {
        return arena.addType(IntersectionType{{a, b}});
    }

    // `|`
    TypeId join(TypeId a, TypeId b)
    {
        return arena.addType(UnionType{{a, b}});
    }

    TypeId negate(TypeId ty)
    {
        return arena.addType(NegationType{ty});
    }

    TypeId cls(const std::string& name, std::optional<TypeId> parent = std::nullopt)
    {
        return arena.addType(ClassType{name, {}, parent.value_or(builtinTypes->classType), {}, {}, nullptr, ""});
    }

    TypeId cls(const std::string& name, ClassType::Props&& props)
    {
        TypeId ty = cls(name);
        getMutable<ClassType>(ty)->props = std::move(props);
        return ty;
    }

    TypeId cyclicTable(std::function<void(TypeId, TableType*)>&& cb)
    {
        TypeId res = arena.addType(GenericType{});
        TableType tt{};
        cb(res, &tt);
        emplaceType<TableType>(asMutable(res), std::move(tt));
        return res;
    }

    TypeId meta(TableType::Props&& metaProps, TableType::Props&& tableProps = {})
    {
        return arena.addType(MetatableType{tbl(std::move(tableProps)), tbl(std::move(metaProps))});
    }

    TypeId genericT = arena.addType(GenericType{"T"});
    TypeId genericU = arena.addType(GenericType{"U"});

    TypePackId genericAs = arena.addTypePack(GenericTypePack{"A"});
    TypePackId genericBs = arena.addTypePack(GenericTypePack{"B"});
    TypePackId genericCs = arena.addTypePack(GenericTypePack{"C"});

    SubtypingResult isSubtype(TypeId subTy, TypeId superTy)
    {
        return subtyping.isSubtype(subTy, superTy);
    }

    TypeId helloType = arena.addType(SingletonType{StringSingleton{"hello"}});
    TypeId helloType2 = arena.addType(SingletonType{StringSingleton{"hello"}});
    TypeId worldType = arena.addType(SingletonType{StringSingleton{"world"}});

    TypeId helloOrWorldType = join(helloType, worldType);
    TypeId trueOrFalseType = join(builtinTypes->trueType, builtinTypes->falseType);

    TypeId helloAndWorldType = meet(helloType, worldType);
    TypeId booleanAndTrueType = meet(builtinTypes->booleanType, builtinTypes->trueType);

    /**
     * class
     * \- Root
     *    |- Child
     *    |  |-GrandchildOne
     *    |  \-GrandchildTwo
     *    \- AnotherChild
     *       |- AnotherGrandchildOne
     *       \- AnotherGrandchildTwo
    */
    TypeId rootClass = cls("Root");
    TypeId childClass = cls("Child", rootClass);
    TypeId grandchildOneClass = cls("GrandchildOne", childClass);
    TypeId grandchildTwoClass = cls("GrandchildTwo", childClass);
    TypeId anotherChildClass = cls("AnotherChild", rootClass);
    TypeId anotherGrandchildOneClass = cls("AnotherGrandchildOne", anotherChildClass);
    TypeId anotherGrandchildTwoClass = cls("AnotherGrandchildTwo", anotherChildClass);

    TypeId vec2Class = cls("Vec2", {
        {"X", builtinTypes->numberType},
        {"Y", builtinTypes->numberType},
    });

    // "hello" | "hello"
    TypeId helloOrHelloType = arena.addType(UnionType{{helloType, helloType}});

    // () -> ()
    const TypeId nothingToNothingType = fn({}, {});

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

    // (number) -> ()
    const TypeId numberToNothingType = fn(
        {builtinTypes->numberType},
        {}
    );

    // () -> number
    const TypeId nothingToNumberType = fn(
        {},
        {builtinTypes->numberType}
    );

    // (number) -> number
    const TypeId numberToNumberType = fn(
        {builtinTypes->numberType},
        {builtinTypes->numberType}
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

    // (...number) -> number
    const TypeId numbersToNumberType = arena.addType(FunctionType{
        arena.addTypePack(VariadicTypePack{builtinTypes->numberType}),
        arena.addTypePack({builtinTypes->numberType})
    });

    // <T>(T) -> ()
    const TypeId genericTToNothingType = arena.addType(FunctionType{
        {genericT},
        {},
        arena.addTypePack({genericT}),
        builtinTypes->emptyTypePack
    });

    // <T>(T) -> T
    const TypeId genericTToTType = arena.addType(FunctionType{
        {genericT},
        {},
        arena.addTypePack({genericT}),
        arena.addTypePack({genericT})
    });

    // <U>(U) -> ()
    const TypeId genericUToNothingType = arena.addType(FunctionType{
        {genericU},
        {},
        arena.addTypePack({genericU}),
        builtinTypes->emptyTypePack
    });

    // <T>() -> T
    const TypeId genericNothingToTType = arena.addType(FunctionType{
        {genericT},
        {},
        builtinTypes->emptyTypePack,
        arena.addTypePack({genericT})
    });

    // <A...>(A...) -> A...
    const TypeId genericAsToAsType = arena.addType(FunctionType{
        {},
        {genericAs},
        genericAs,
        genericAs
    });

    // <A...>(A...) -> number
    const TypeId genericAsToNumberType = arena.addType(FunctionType{
        {},
        {genericAs},
        genericAs,
        arena.addTypePack({builtinTypes->numberType})
    });

    // <B...>(B...) -> B...
    const TypeId genericBsToBsType = arena.addType(FunctionType{
        {},
        {genericBs},
        genericBs,
        genericBs
    });

    // <B..., C...>(B...) -> C...
    const TypeId genericBsToCsType = arena.addType(FunctionType{
        {},
        {genericBs, genericCs},
        genericBs,
        genericCs
    });

    // <A...>() -> A...
    const TypeId genericNothingToAsType = arena.addType(FunctionType{
        {},
        {genericAs},
        builtinTypes->emptyTypePack,
        genericAs
    });

    // { lower : string -> string }
    TypeId tableWithLower = tbl(TableType::Props{{"lower", fn({builtinTypes->stringType}, {builtinTypes->stringType})}});
    // { insaneThingNoScalarHas : () -> () }
    TypeId tableWithoutScalarProp = tbl(TableType::Props{{"insaneThingNoScalarHas", fn({}, {})}});
};

#define CHECK_IS_SUBTYPE(left, right) \
    do \
    { \
        const auto& leftTy = (left); \
        const auto& rightTy = (right); \
        SubtypingResult result = isSubtype(leftTy, rightTy); \
        CHECK_MESSAGE(result.isSubtype, "Expected " << leftTy << " <: " << rightTy); \
    } while (0)

#define CHECK_IS_NOT_SUBTYPE(left, right) \
    do \
    { \
        const auto& leftTy = (left); \
        const auto& rightTy = (right); \
        SubtypingResult result = isSubtype(leftTy, rightTy); \
        CHECK_MESSAGE(!result.isSubtype, "Expected " << leftTy << " </: " << rightTy); \
    } while (0)

#define CHECK_IS_ERROR_SUPPRESSING(left, right) \
    do \
    { \
        const auto& leftTy = (left); \
        const auto& rightTy = (right); \
        SubtypingResult result = isSubtype(leftTy, rightTy); \
        CHECK_MESSAGE(result.isErrorSuppressing, "Expected " << leftTy << " to error-suppress " << rightTy); \
    } while (0)

#define CHECK_IS_NOT_ERROR_SUPPRESSING(left, right) \
    do \
    { \
        const auto& leftTy = (left); \
        const auto& rightTy = (right); \
        SubtypingResult result = isSubtype(leftTy, rightTy); \
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
    SubtypingResult result = isSubtype(builtinTypes->anyType, builtinTypes->unknownType);
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

TEST_CASE_FIXTURE(SubtypeFixture, "string <!: ('hello' | 'hello')")
{
    CHECK_IS_NOT_SUBTYPE(builtinTypes->stringType, helloOrHelloType);
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

TEST_CASE_FIXTURE(SubtypeFixture, "<T>() -> T <: () -> number")
{
    CHECK_IS_SUBTYPE(genericNothingToTType, nothingToNumberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<T>(T) -> () <: <U>(U) -> ()")
{
    CHECK_IS_SUBTYPE(genericTToNothingType, genericUToNothingType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "() -> number <!: <T>() -> T")
{
    CHECK_IS_NOT_SUBTYPE(nothingToNumberType, genericNothingToTType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<T>(T) -> () <: (number) -> ()")
{
    CHECK_IS_SUBTYPE(genericTToNothingType, numberToNothingType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<T>(T) -> T <: (number) -> number")
{
    CHECK_IS_SUBTYPE(genericTToTType, numberToNumberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<T>(T) -> T <!: (number) -> string")
{
    CHECK_IS_NOT_SUBTYPE(genericTToTType, numberToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<T>(T) -> () <: <U>(U) -> ()")
{
    CHECK_IS_SUBTYPE(genericTToNothingType, genericUToNothingType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number) -> () <!: <T>(T) -> ()")
{
    CHECK_IS_NOT_SUBTYPE(numberToNothingType, genericTToNothingType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<A...>(A...) -> A... <: (number) -> number")
{
    CHECK_IS_SUBTYPE(genericAsToAsType, numberToNumberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number) -> number <!: <A...>(A...) -> A...")
{
    CHECK_IS_NOT_SUBTYPE(numberToNumberType, genericAsToAsType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<A...>(A...) -> A... <: <B...>(B...) -> B...")
{
    CHECK_IS_SUBTYPE(genericAsToAsType, genericBsToBsType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<B..., C...>(B...) -> C... <: <A...>(A...) -> A...")
{
    CHECK_IS_SUBTYPE(genericBsToCsType, genericAsToAsType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<A...>(A...) -> A... <!: <B..., C...>(B...) -> C...")
{
    CHECK_IS_NOT_SUBTYPE(genericAsToAsType, genericBsToCsType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<A...>(A...) -> number <: (number) -> number")
{
    CHECK_IS_SUBTYPE(genericAsToNumberType, numberToNumberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(number) -> number <!: <A...>(A...) -> number")
{
    CHECK_IS_NOT_SUBTYPE(numberToNumberType, genericAsToNumberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<A...>(A...) -> number <: (...number) -> number")
{
    CHECK_IS_SUBTYPE(genericAsToNumberType, numbersToNumberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(...number) -> number <!: <A...>(A...) -> number")
{
    CHECK_IS_NOT_SUBTYPE(numbersToNumberType, genericAsToNumberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<A...>() -> A... <: () -> ()")
{
    CHECK_IS_SUBTYPE(genericNothingToAsType, nothingToNothingType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "() -> () <!: <A...>() -> A...")
{
    CHECK_IS_NOT_SUBTYPE(nothingToNothingType, genericNothingToAsType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<A...>(A...) -> A... <: () -> ()")
{
    CHECK_IS_SUBTYPE(genericAsToAsType, nothingToNothingType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "() -> () <!: <A...>(A...) -> A...")
{
    CHECK_IS_NOT_SUBTYPE(nothingToNothingType, genericAsToAsType);
}


TEST_CASE_FIXTURE(SubtypeFixture, "{} <: {}")
{
    CHECK_IS_SUBTYPE(tbl({}), tbl({}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{x: number} <: {}")
{
    CHECK_IS_SUBTYPE(tbl({{"x", builtinTypes->numberType}}), tbl({}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{x: number} <!: {x: string}")
{
    CHECK_IS_NOT_SUBTYPE(tbl({{"x", builtinTypes->numberType}}), tbl({{"x", builtinTypes->stringType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{x: number} <!: {x: number?}")
{
    CHECK_IS_NOT_SUBTYPE(tbl({{"x", builtinTypes->numberType}}), tbl({{"x", builtinTypes->optionalNumberType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{x: number?} <!: {x: number}")
{
    CHECK_IS_NOT_SUBTYPE(tbl({{"x", builtinTypes->optionalNumberType}}), tbl({{"x", builtinTypes->numberType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{x: <T>(T) -> ()} <: {x: <U>(U) -> ()}")
{
    CHECK_IS_SUBTYPE(
        tbl({{"x", genericTToNothingType}}),
        tbl({{"x", genericUToNothingType}})
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable { x: number } } <: { @metatable {} }")
{
    CHECK_IS_SUBTYPE(
        meta({{"x", builtinTypes->numberType}}),
        meta({})
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable { x: number } } <!: { @metatable { x: boolean } }")
{
    CHECK_IS_NOT_SUBTYPE(
        meta({{"x", builtinTypes->numberType}}),
        meta({{"x", builtinTypes->booleanType}})
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable {} } <!: { @metatable { x: boolean } }")
{
    CHECK_IS_NOT_SUBTYPE(
        meta({}),
        meta({{"x", builtinTypes->booleanType}})
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable {} } <: {}")
{
    CHECK_IS_SUBTYPE(
        meta({}),
        tbl({})
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable { u: boolean }, x: number } <: { x: number }")
{
    CHECK_IS_SUBTYPE(
        meta({{"u", builtinTypes->booleanType}}, {{"x", builtinTypes->numberType}}),
        tbl({{"x", builtinTypes->numberType}})
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable { x: number } } <!: { x: number }")
{
    CHECK_IS_NOT_SUBTYPE(
        meta({{"x", builtinTypes->numberType}}),
        tbl({{"x", builtinTypes->numberType}})
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "Root <: class")
{
    CHECK_IS_SUBTYPE(rootClass, builtinTypes->classType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child | AnotherChild <: class")
{
    CHECK_IS_SUBTYPE(join(childClass, anotherChildClass), builtinTypes->classType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child | AnotherChild <: Child | AnotherChild")
{
    CHECK_IS_SUBTYPE(join(childClass, anotherChildClass), join(childClass, anotherChildClass));
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child | Root <: Root")
{
    CHECK_IS_SUBTYPE(join(childClass, rootClass), rootClass);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child & AnotherChild <: class")
{
    CHECK_IS_SUBTYPE(meet(childClass, anotherChildClass), builtinTypes->classType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child & Root <: class")
{
    CHECK_IS_SUBTYPE(meet(childClass, rootClass), builtinTypes->classType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child & ~Root <: class")
{
    CHECK_IS_SUBTYPE(meet(childClass, negate(rootClass)), builtinTypes->classType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child & AnotherChild <: number")
{
    CHECK_IS_SUBTYPE(meet(childClass, anotherChildClass), builtinTypes->numberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child & ~GrandchildOne <!: number")
{
    CHECK_IS_NOT_SUBTYPE(meet(childClass, negate(grandchildOneClass)), builtinTypes->numberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "t1 where t1 = {trim: (t1) -> string} <: t2 where t2 = {trim: (t2) -> string}")
{
    TypeId t1 = cyclicTable([&](TypeId ty, TableType* tt)
    {
        tt->props["trim"] = fn({ty}, {builtinTypes->stringType});
    });

    TypeId t2 = cyclicTable([&](TypeId ty, TableType* tt)
    {
        tt->props["trim"] = fn({ty}, {builtinTypes->stringType});
    });

    CHECK_IS_SUBTYPE(t1, t2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "t1 where t1 = {trim: (t1) -> string} <!: t2 where t2 = {trim: (t2) -> t2}")
{
    TypeId t1 = cyclicTable([&](TypeId ty, TableType* tt)
    {
        tt->props["trim"] = fn({ty}, {builtinTypes->stringType});
    });

    TypeId t2 = cyclicTable([&](TypeId ty, TableType* tt)
    {
        tt->props["trim"] = fn({ty}, {ty});
    });

    CHECK_IS_NOT_SUBTYPE(t1, t2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "t1 where t1 = {trim: (t1) -> t1} <!: t2 where t2 = {trim: (t2) -> string}")
{
    TypeId t1 = cyclicTable([&](TypeId ty, TableType* tt)
    {
        tt->props["trim"] = fn({ty}, {ty});
    });

    TypeId t2 = cyclicTable([&](TypeId ty, TableType* tt)
    {
        tt->props["trim"] = fn({ty}, {builtinTypes->stringType});
    });

    CHECK_IS_NOT_SUBTYPE(t1, t2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Vec2 <: { X: number, Y: number }")
{
    TypeId xy = tbl({
        {"X", builtinTypes->numberType},
        {"Y", builtinTypes->numberType},
    });

    CHECK_IS_SUBTYPE(vec2Class, xy);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Vec2 <: { X: number }")
{
    TypeId x = tbl({
        {"X", builtinTypes->numberType},
    });

    CHECK_IS_SUBTYPE(vec2Class, x);
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ X: number, Y: number } <!: Vec2")
{
    TypeId xy = tbl({
        {"X", builtinTypes->numberType},
        {"Y", builtinTypes->numberType},
    });

    CHECK_IS_NOT_SUBTYPE(xy, vec2Class);
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ X: number } <!: Vec2")
{
    TypeId x = tbl({
        {"X", builtinTypes->numberType},
    });

    CHECK_IS_NOT_SUBTYPE(x, vec2Class);
}

TEST_CASE_FIXTURE(SubtypeFixture, "table & { X: number, Y: number } <!: Vec2")
{
    TypeId x = tbl({
        {"X", builtinTypes->numberType},
        {"Y", builtinTypes->numberType},
    });

    CHECK_IS_NOT_SUBTYPE(meet(builtinTypes->tableType, x), vec2Class);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Vec2 <!: table & { X: number, Y: number }")
{
    TypeId xy = tbl({
        {"X", builtinTypes->numberType},
        {"Y", builtinTypes->numberType},
    });

    CHECK_IS_NOT_SUBTYPE(vec2Class, meet(builtinTypes->tableType, xy));
}

TEST_CASE_FIXTURE(SubtypeFixture, "\"hello\" <: { lower : (string) -> string }")
{
    CHECK_IS_SUBTYPE(helloType, tableWithLower);
}

TEST_CASE_FIXTURE(SubtypeFixture, "\"hello\" <!: { insaneThingNoScalarHas : () -> () }")
{
    CHECK_IS_NOT_SUBTYPE(helloType, tableWithoutScalarProp);
}

TEST_CASE_FIXTURE(SubtypeFixture, "string <: { lower : (string) -> string }")
{
    CHECK_IS_SUBTYPE(builtinTypes->stringType, tableWithLower);
}

TEST_CASE_FIXTURE(SubtypeFixture, "string <!: { insaneThingNoScalarHas : () -> () }")
{
    CHECK_IS_NOT_SUBTYPE(builtinTypes->stringType, tableWithoutScalarProp);
}

/*
 * <A>(A) -> A <: <X>(X) -> X
 *      A can be bound to X.
 *
 * <A>(A) -> A </: <X>(X) -> number
 *      A can be bound to X, but A </: number
 *
 * (number) -> number </: <A>(A) -> A
 *      Only generics on the left side can be bound.
 *      number </: A
 *
 * <A, B>(A, B) -> boolean <: <X>(X, X) -> boolean
 *      It is ok to bind both A and B to X.
 *
 * <A>(A, A) -> boolean </: <X, Y>(X, Y) -> boolean
 *      A cannot be bound to both X and Y.
 */

TEST_SUITE_END();
