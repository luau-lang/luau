// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypePath.h"

#include "Luau/Normalize.h"
#include "Luau/Subtyping.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"

#include "doctest.h"
#include "Fixture.h"
#include "RegisterCallbacks.h"

#include <initializer_list>

using namespace Luau;

namespace Luau
{

std::ostream& operator<<(std::ostream& lhs, const SubtypingVariance& variance)
{
    switch (variance)
    {
    case SubtypingVariance::Covariant:
        return lhs << "covariant";
    case SubtypingVariance::Contravariant:
        return lhs << "contravariant";
    case SubtypingVariance::Invariant:
        return lhs << "invariant";
    case SubtypingVariance::Invalid:
        return lhs << "*invalid*";
    }

    return lhs;
}

std::ostream& operator<<(std::ostream& lhs, const SubtypingReasoning& reasoning)
{
    return lhs << toString(reasoning.subPath) << " </: " << toString(reasoning.superPath) << " (" << reasoning.variance << ")";
}

bool operator==(const DenseHashSet<SubtypingReasoning, SubtypingReasoningHash>& set, const std::vector<SubtypingReasoning>& items)
{
    if (items.size() != set.size())
        return false;

    for (const SubtypingReasoning& r : items)
    {
        if (!set.contains(r))
            return false;
    }

    return true;
}

}; // namespace Luau

struct SubtypeFixture : Fixture
{
    TypeArena arena;
    InternalErrorReporter iceReporter;
    UnifierSharedState sharedState{&ice};
    Normalizer normalizer{&arena, builtinTypes, NotNull{&sharedState}};

    ScopePtr rootScope{new Scope(builtinTypes->emptyTypePack)};
    ScopePtr moduleScope{new Scope(rootScope)};

    Subtyping subtyping = mkSubtyping(rootScope);

    Subtyping mkSubtyping(const ScopePtr& scope)
    {
        return Subtyping{builtinTypes, NotNull{&arena}, NotNull{&normalizer}, NotNull{&iceReporter}, NotNull{scope.get()}};
    }

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

    TypeId idx(TypeId keyTy, TypeId valueTy)
    {
        return arena.addType(TableType{{}, TableIndexer{keyTy, valueTy}, {}, TableState::Sealed});
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

    // `~`
    TypeId negate(TypeId ty)
    {
        return arena.addType(NegationType{ty});
    }

    // "literal"
    TypeId str(const char* literal)
    {
        return arena.addType(SingletonType{StringSingleton{literal}});
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

    TypeId opt(TypeId ty)
    {
        return join(ty, builtinTypes->nilType);
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

    TypeId genericT = arena.addType(GenericType{moduleScope.get(), "T"});
    TypeId genericU = arena.addType(GenericType{moduleScope.get(), "U"});

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

    TypeId aType = arena.addType(SingletonType{StringSingleton{"a"}});
    TypeId bType = arena.addType(SingletonType{StringSingleton{"b"}});
    TypeId trueSingleton = arena.addType(SingletonType{BooleanSingleton{true}});
    TypeId falseSingleton = arena.addType(SingletonType{BooleanSingleton{false}});
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
    const TypeId numberToStringType = fn({builtinTypes->numberType}, {builtinTypes->stringType});

    // (unknown) -> string
    const TypeId unknownToStringType = fn({builtinTypes->unknownType}, {builtinTypes->stringType});

    // (number) -> ()
    const TypeId numberToNothingType = fn({builtinTypes->numberType}, {});

    // () -> number
    const TypeId nothingToNumberType = fn({}, {builtinTypes->numberType});

    // (number) -> number
    const TypeId numberToNumberType = fn({builtinTypes->numberType}, {builtinTypes->numberType});

    // (number) -> unknown
    const TypeId numberToUnknownType = fn({builtinTypes->numberType}, {builtinTypes->unknownType});

    // (number) -> (string, string)
    const TypeId numberToTwoStringsType = fn({builtinTypes->numberType}, {builtinTypes->stringType, builtinTypes->stringType});

    // (number) -> (string, unknown)
    const TypeId numberToStringAndUnknownType = fn({builtinTypes->numberType}, {builtinTypes->stringType, builtinTypes->unknownType});

    // (number, number) -> string
    const TypeId numberNumberToStringType = fn({builtinTypes->numberType, builtinTypes->numberType}, {builtinTypes->stringType});

    // (unknown, number) -> string
    const TypeId unknownNumberToStringType = fn({builtinTypes->unknownType, builtinTypes->numberType}, {builtinTypes->stringType});

    // (number, string) -> string
    const TypeId numberAndStringToStringType = fn({builtinTypes->numberType, builtinTypes->stringType}, {builtinTypes->stringType});

    // (number, ...string) -> string
    const TypeId numberAndStringsToStringType =
        fn({builtinTypes->numberType}, VariadicTypePack{builtinTypes->stringType}, {builtinTypes->stringType});

    // (number, ...string?) -> string
    const TypeId numberAndOptionalStringsToStringType =
        fn({builtinTypes->numberType}, VariadicTypePack{builtinTypes->optionalStringType}, {builtinTypes->stringType});

    // (...number) -> number
    const TypeId numbersToNumberType =
        arena.addType(FunctionType{arena.addTypePack(VariadicTypePack{builtinTypes->numberType}), arena.addTypePack({builtinTypes->numberType})});

    // <T>(T) -> ()
    const TypeId genericTToNothingType = arena.addType(FunctionType{{genericT}, {}, arena.addTypePack({genericT}), builtinTypes->emptyTypePack});

    // <T>(T) -> T
    const TypeId genericTToTType = arena.addType(FunctionType{{genericT}, {}, arena.addTypePack({genericT}), arena.addTypePack({genericT})});

    // <U>(U) -> ()
    const TypeId genericUToNothingType = arena.addType(FunctionType{{genericU}, {}, arena.addTypePack({genericU}), builtinTypes->emptyTypePack});

    // <T>() -> T
    const TypeId genericNothingToTType = arena.addType(FunctionType{{genericT}, {}, builtinTypes->emptyTypePack, arena.addTypePack({genericT})});

    // <A...>(A...) -> A...
    const TypeId genericAsToAsType = arena.addType(FunctionType{{}, {genericAs}, genericAs, genericAs});

    // <A...>(A...) -> number
    const TypeId genericAsToNumberType = arena.addType(FunctionType{{}, {genericAs}, genericAs, arena.addTypePack({builtinTypes->numberType})});

    // <B...>(B...) -> B...
    const TypeId genericBsToBsType = arena.addType(FunctionType{{}, {genericBs}, genericBs, genericBs});

    // <B..., C...>(B...) -> C...
    const TypeId genericBsToCsType = arena.addType(FunctionType{{}, {genericBs, genericCs}, genericBs, genericCs});

    // <A...>() -> A...
    const TypeId genericNothingToAsType = arena.addType(FunctionType{{}, {genericAs}, builtinTypes->emptyTypePack, genericAs});

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

/// Internal macro for registering a generated test case.
///
/// @param der the name of the derived fixture struct
/// @param reg the name of the registration callback, invoked immediately before
/// tests are ran to register the test
/// @param run the name of the run callback, invoked to actually run the test case
#define TEST_REGISTER(der, reg, run) \
    static inline DOCTEST_NOINLINE void run() \
    { \
        der fix; \
        fix.test(); \
    } \
    static inline DOCTEST_NOINLINE void reg() \
    { \
        /* we have to mark this as `static` to ensure the memory remains alive \
        for the entirety of the test process */ \
        static std::string name = der().testName; \
        doctest::detail::regTest(doctest::detail::TestCase(run, __FILE__, __LINE__, \
                                     doctest_detail_test_suite_ns::getCurrentTestSuite()) /* the test case's name, determined at runtime */ \
                                 * name.c_str() /* getCurrentTestSuite() only works at static initialization \
                                                time due to implementation details. To ensure that test cases \
                                                are grouped where they should be, manually override the suite \
                                                with the test_suite decorator. */ \
                                 * doctest::test_suite("Subtyping")); \
    } \
    DOCTEST_GLOBAL_NO_WARNINGS(DOCTEST_ANONYMOUS(DOCTEST_ANON_VAR_), addTestCallback(reg));

/// Internal macro for deriving a test case fixture. Roughly analogous to
/// DOCTEST_IMPLEMENT_FIXTURE.
///
/// @param op a function (or macro) to call that compares the subtype to
/// the supertype.
/// @param symbol the symbol to use in stringification
/// @param der the name of the derived fixture struct
/// @param left the subtype expression
/// @param right the supertype expression
#define TEST_DERIVE(op, symbol, der, left, right) \
    namespace \
    { \
    struct der : SubtypeFixture \
    { \
        const TypeId subTy = (left); \
        const TypeId superTy = (right); \
        const std::string testName = toString(subTy) + " " symbol " " + toString(superTy); \
        inline DOCTEST_NOINLINE void test() \
        { \
            op(subTy, superTy); \
        } \
    }; \
    TEST_REGISTER(der, DOCTEST_ANONYMOUS(DOCTEST_ANON_FUNC_), DOCTEST_ANONYMOUS(DOCTEST_ANON_FUNC_)); \
    }

/// Generates a test that checks if a type is a subtype of another.
#define TEST_IS_SUBTYPE(left, right) TEST_DERIVE(CHECK_IS_SUBTYPE, "<:", DOCTEST_ANONYMOUS(DOCTEST_ANON_CLASS_), left, right)

/// Generates a test that checks if a type is _not_ a subtype of another.
/// Uses <!: instead of </: to ensure that rotest doesn't explode when it splits
/// on / characters.
#define TEST_IS_NOT_SUBTYPE(left, right) TEST_DERIVE(CHECK_IS_NOT_SUBTYPE, "<!:", DOCTEST_ANONYMOUS(DOCTEST_ANON_CLASS_), left, right)

TEST_SUITE_BEGIN("Subtyping");

// We would like to write </: to mean "is not a subtype," but rotest does not like that at all, so we instead use <!:

TEST_IS_SUBTYPE(builtinTypes->numberType, builtinTypes->anyType);
TEST_IS_NOT_SUBTYPE(builtinTypes->numberType, builtinTypes->stringType);

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

TEST_CASE_FIXTURE(SubtypeFixture, "<T>() -> (T, T) <!: () -> (string, number)")
{
    TypeId nothingToTwoTs = arena.addType(FunctionType{{genericT}, {}, builtinTypes->emptyTypePack, arena.addTypePack({genericT, genericT})});

    TypeId nothingToStringAndNumber = fn({}, {builtinTypes->stringType, builtinTypes->numberType});

    CHECK_IS_NOT_SUBTYPE(nothingToTwoTs, nothingToStringAndNumber);
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

TEST_CASE_FIXTURE(SubtypeFixture, "{} <!: {x: number}")
{
    CHECK_IS_NOT_SUBTYPE(tbl({}), tbl({{"x", builtinTypes->numberType}}));
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
    CHECK_IS_SUBTYPE(tbl({{"x", genericTToNothingType}}), tbl({{"x", genericUToNothingType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable { x: number } } <: { @metatable {} }")
{
    CHECK_IS_SUBTYPE(meta({{"x", builtinTypes->numberType}}), meta({}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable { x: number } } <!: { @metatable { x: boolean } }")
{
    CHECK_IS_NOT_SUBTYPE(meta({{"x", builtinTypes->numberType}}), meta({{"x", builtinTypes->booleanType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable {} } <!: { @metatable { x: boolean } }")
{
    CHECK_IS_NOT_SUBTYPE(meta({}), meta({{"x", builtinTypes->booleanType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable {} } <: {}")
{
    CHECK_IS_SUBTYPE(meta({}), tbl({}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable { u: boolean }, x: number } <: { x: number }")
{
    CHECK_IS_SUBTYPE(meta({{"u", builtinTypes->booleanType}}, {{"x", builtinTypes->numberType}}), tbl({{"x", builtinTypes->numberType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable { x: number } } <!: { x: number }")
{
    CHECK_IS_NOT_SUBTYPE(meta({{"x", builtinTypes->numberType}}), tbl({{"x", builtinTypes->numberType}}));
}

// Negated subtypes
TEST_IS_NOT_SUBTYPE(negate(builtinTypes->neverType), builtinTypes->stringType);
TEST_IS_SUBTYPE(negate(builtinTypes->unknownType), builtinTypes->stringType);
TEST_IS_NOT_SUBTYPE(negate(builtinTypes->anyType), builtinTypes->stringType);
TEST_IS_SUBTYPE(negate(meet(builtinTypes->neverType, builtinTypes->unknownType)), builtinTypes->stringType);
TEST_IS_SUBTYPE(negate(join(builtinTypes->neverType, builtinTypes->unknownType)), builtinTypes->stringType);

// Negated supertypes: never/unknown/any/error
TEST_IS_SUBTYPE(builtinTypes->stringType, negate(builtinTypes->neverType));
TEST_IS_SUBTYPE(builtinTypes->neverType, negate(builtinTypes->unknownType));
TEST_IS_NOT_SUBTYPE(builtinTypes->stringType, negate(builtinTypes->unknownType));
TEST_IS_SUBTYPE(builtinTypes->numberType, negate(builtinTypes->anyType));
TEST_IS_SUBTYPE(builtinTypes->unknownType, negate(builtinTypes->anyType));

// Negated supertypes: unions
TEST_IS_SUBTYPE(builtinTypes->booleanType, negate(join(builtinTypes->stringType, builtinTypes->numberType)));
TEST_IS_SUBTYPE(rootClass, negate(join(childClass, builtinTypes->numberType)));
TEST_IS_SUBTYPE(str("foo"), negate(join(builtinTypes->numberType, builtinTypes->booleanType)));
TEST_IS_NOT_SUBTYPE(str("foo"), negate(join(builtinTypes->stringType, builtinTypes->numberType)));
TEST_IS_NOT_SUBTYPE(childClass, negate(join(rootClass, builtinTypes->numberType)));
TEST_IS_NOT_SUBTYPE(numbersToNumberType, negate(join(builtinTypes->functionType, rootClass)));

// Negated supertypes: intersections
TEST_IS_SUBTYPE(builtinTypes->booleanType, negate(meet(builtinTypes->stringType, str("foo"))));
TEST_IS_SUBTYPE(builtinTypes->trueType, negate(meet(builtinTypes->booleanType, builtinTypes->numberType)));
TEST_IS_SUBTYPE(rootClass, negate(meet(builtinTypes->classType, childClass)));
TEST_IS_SUBTYPE(childClass, negate(meet(builtinTypes->classType, builtinTypes->numberType)));
TEST_IS_SUBTYPE(builtinTypes->unknownType, negate(meet(builtinTypes->classType, builtinTypes->numberType)));
TEST_IS_NOT_SUBTYPE(str("foo"), negate(meet(builtinTypes->stringType, negate(str("bar")))));

// Negated supertypes: tables and metatables
TEST_IS_SUBTYPE(tbl({}), negate(builtinTypes->numberType));
TEST_IS_NOT_SUBTYPE(tbl({}), negate(builtinTypes->tableType));
TEST_IS_SUBTYPE(meta({}), negate(builtinTypes->numberType));
TEST_IS_NOT_SUBTYPE(meta({}), negate(builtinTypes->tableType));

// Negated supertypes: Functions
TEST_IS_SUBTYPE(numberToNumberType, negate(builtinTypes->classType));
TEST_IS_NOT_SUBTYPE(numberToNumberType, negate(builtinTypes->functionType));

// Negated supertypes: Primitives and singletons
TEST_IS_SUBTYPE(builtinTypes->stringType, negate(builtinTypes->numberType));
TEST_IS_SUBTYPE(str("foo"), meet(builtinTypes->stringType, negate(str("bar"))));
TEST_IS_NOT_SUBTYPE(builtinTypes->trueType, negate(builtinTypes->booleanType));
TEST_IS_NOT_SUBTYPE(str("foo"), negate(str("foo")));
TEST_IS_NOT_SUBTYPE(str("foo"), negate(builtinTypes->stringType));
TEST_IS_SUBTYPE(builtinTypes->falseType, negate(builtinTypes->trueType));
TEST_IS_SUBTYPE(builtinTypes->falseType, meet(builtinTypes->booleanType, negate(builtinTypes->trueType)));
TEST_IS_NOT_SUBTYPE(builtinTypes->stringType, meet(builtinTypes->booleanType, negate(builtinTypes->trueType)));
TEST_IS_NOT_SUBTYPE(builtinTypes->stringType, negate(str("foo")));
TEST_IS_NOT_SUBTYPE(builtinTypes->booleanType, negate(builtinTypes->falseType));

// Negated supertypes: Classes
TEST_IS_SUBTYPE(rootClass, negate(builtinTypes->tableType));
TEST_IS_NOT_SUBTYPE(rootClass, negate(builtinTypes->classType));
TEST_IS_NOT_SUBTYPE(childClass, negate(rootClass));
TEST_IS_NOT_SUBTYPE(childClass, meet(builtinTypes->classType, negate(rootClass)));
TEST_IS_SUBTYPE(anotherChildClass, meet(builtinTypes->classType, negate(childClass)));

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

TEST_CASE_FIXTURE(SubtypeFixture, "semantic_subtyping_disj")
{
    TypeId subTy = builtinTypes->unknownType;
    TypeId superTy = join(negate(builtinTypes->numberType), negate(builtinTypes->stringType));
    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(result.isSubtype);
}


TEST_CASE_FIXTURE(SubtypeFixture, "t1 where t1 = {trim: (t1) -> string} <: t2 where t2 = {trim: (t2) -> string}")
{
    TypeId t1 = cyclicTable([&](TypeId ty, TableType* tt) {
        tt->props["trim"] = fn({ty}, {builtinTypes->stringType});
    });

    TypeId t2 = cyclicTable([&](TypeId ty, TableType* tt) {
        tt->props["trim"] = fn({ty}, {builtinTypes->stringType});
    });

    CHECK_IS_SUBTYPE(t1, t2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "t1 where t1 = {trim: (t1) -> string} <!: t2 where t2 = {trim: (t2) -> t2}")
{
    TypeId t1 = cyclicTable([&](TypeId ty, TableType* tt) {
        tt->props["trim"] = fn({ty}, {builtinTypes->stringType});
    });

    TypeId t2 = cyclicTable([&](TypeId ty, TableType* tt) {
        tt->props["trim"] = fn({ty}, {ty});
    });

    CHECK_IS_NOT_SUBTYPE(t1, t2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "t1 where t1 = {trim: (t1) -> t1} <!: t2 where t2 = {trim: (t2) -> string}")
{
    TypeId t1 = cyclicTable([&](TypeId ty, TableType* tt) {
        tt->props["trim"] = fn({ty}, {ty});
    });

    TypeId t2 = cyclicTable([&](TypeId ty, TableType* tt) {
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

TEST_CASE_FIXTURE(SubtypeFixture, "~fun & (string) -> number <: (string) -> number")
{
    CHECK_IS_SUBTYPE(meet(negate(builtinTypes->functionType), numberToStringType), numberToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(string) -> number <: ~fun & (string) -> number")
{
    CHECK_IS_NOT_SUBTYPE(numberToStringType, meet(negate(builtinTypes->functionType), numberToStringType));
}

TEST_CASE_FIXTURE(SubtypeFixture, "~\"a\" & ~\"b\" & string <: { lower : (string) -> ()}")
{
    CHECK_IS_SUBTYPE(meet(meet(negate(aType), negate(bType)), builtinTypes->stringType), tableWithLower);
}

TEST_CASE_FIXTURE(SubtypeFixture, "\"a\" | (~\"b\" & string) <: { lower : (string) -> ()}")
{
    CHECK_IS_SUBTYPE(join(aType, meet(negate(bType), builtinTypes->stringType)), tableWithLower);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(string | number) & (\"a\" | true) <: { lower: (string) -> string }")
{
    auto base = meet(join(builtinTypes->stringType, builtinTypes->numberType), join(aType, trueSingleton));
    CHECK_IS_SUBTYPE(base, tableWithLower);
}

TEST_CASE_FIXTURE(SubtypeFixture, "number <: ~~number")
{
    CHECK_IS_SUBTYPE(builtinTypes->numberType, negate(negate(builtinTypes->numberType)));
}

TEST_CASE_FIXTURE(SubtypeFixture, "~~number <: number")
{
    CHECK_IS_SUBTYPE(negate(negate(builtinTypes->numberType)), builtinTypes->numberType);
}

// See https://github.com/luau-lang/luau/issues/767
TEST_CASE_FIXTURE(SubtypeFixture, "(...any) -> () <: <T>(T...) -> ()")
{
    TypeId anysToNothing = arena.addType(FunctionType{builtinTypes->anyTypePack, builtinTypes->emptyTypePack});
    TypeId genericTToAnys = arena.addType(FunctionType{genericAs, builtinTypes->emptyTypePack});

    CHECK_MESSAGE(subtyping.isSubtype(anysToNothing, genericTToAnys).isSubtype, "(...any) -> () <: <T>(T...) -> ()");
}

// See https://github.com/luau-lang/luau/issues/767
TEST_CASE_FIXTURE(SubtypeFixture, "(...unknown) -> () <: <T>(T...) -> ()")
{
    TypeId anysToNothing = arena.addType(FunctionType{arena.addTypePack(VariadicTypePack{builtinTypes->unknownType}), builtinTypes->emptyTypePack});
    TypeId genericTToAnys = arena.addType(FunctionType{genericAs, builtinTypes->emptyTypePack});

    CHECK_MESSAGE(subtyping.isSubtype(anysToNothing, genericTToAnys).isSubtype, "(...unknown) -> () <: <T>(T...) -> ()");
}

/*
 * Within the scope to which a generic belongs, that generic ought to be treated
 * as its bounds.
 *
 * We do not yet support bounded generics, so all generics are considered to be
 * bounded by unknown.
 */
TEST_CASE_FIXTURE(SubtypeFixture, "unknown <: X")
{
    ScopePtr childScope{new Scope(rootScope)};
    ScopePtr grandChildScope{new Scope(childScope)};

    TypeId genericX = arena.addType(GenericType(childScope.get(), "X"));

    SubtypingResult usingGlobalScope = subtyping.isSubtype(builtinTypes->unknownType, genericX);
    CHECK_MESSAGE(!usingGlobalScope.isSubtype, "Expected " << builtinTypes->unknownType << " </: " << genericX);

    Subtyping childSubtyping{mkSubtyping(childScope)};

    SubtypingResult usingChildScope = childSubtyping.isSubtype(builtinTypes->unknownType, genericX);
    CHECK_MESSAGE(usingChildScope.isSubtype, "Expected " << builtinTypes->unknownType << " <: " << genericX);

    Subtyping grandChildSubtyping{mkSubtyping(grandChildScope)};

    SubtypingResult usingGrandChildScope = grandChildSubtyping.isSubtype(builtinTypes->unknownType, genericX);
    CHECK_MESSAGE(usingGrandChildScope.isSubtype, "Expected " << builtinTypes->unknownType << " <: " << genericX);
}

TEST_IS_SUBTYPE(idx(builtinTypes->numberType, builtinTypes->numberType), tbl({}));
TEST_IS_NOT_SUBTYPE(tbl({}), idx(builtinTypes->numberType, builtinTypes->numberType));

TEST_IS_NOT_SUBTYPE(tbl({{"X", builtinTypes->numberType}}), idx(builtinTypes->numberType, builtinTypes->numberType));
TEST_IS_NOT_SUBTYPE(idx(builtinTypes->numberType, builtinTypes->numberType), tbl({{"X", builtinTypes->numberType}}));

TEST_IS_NOT_SUBTYPE(
    idx(join(builtinTypes->numberType, builtinTypes->stringType), builtinTypes->numberType), idx(builtinTypes->numberType, builtinTypes->numberType));
TEST_IS_NOT_SUBTYPE(
    idx(builtinTypes->numberType, builtinTypes->numberType), idx(join(builtinTypes->numberType, builtinTypes->stringType), builtinTypes->numberType));

TEST_IS_NOT_SUBTYPE(
    idx(builtinTypes->numberType, join(builtinTypes->stringType, builtinTypes->numberType)), idx(builtinTypes->numberType, builtinTypes->numberType));
TEST_IS_NOT_SUBTYPE(
    idx(builtinTypes->numberType, builtinTypes->numberType), idx(builtinTypes->numberType, join(builtinTypes->stringType, builtinTypes->numberType)));

TEST_IS_NOT_SUBTYPE(tbl({{"X", builtinTypes->numberType}}), idx(builtinTypes->stringType, builtinTypes->numberType));
TEST_IS_SUBTYPE(idx(builtinTypes->stringType, builtinTypes->numberType), tbl({{"X", builtinTypes->numberType}}));

TEST_IS_NOT_SUBTYPE(tbl({{"X", opt(builtinTypes->numberType)}}), idx(builtinTypes->stringType, builtinTypes->numberType));
TEST_IS_NOT_SUBTYPE(idx(builtinTypes->stringType, builtinTypes->numberType), tbl({{"X", opt(builtinTypes->numberType)}}));

TEST_IS_SUBTYPE(tbl({{"X", builtinTypes->numberType}, {"Y", builtinTypes->numberType}}), tbl({{"X", builtinTypes->numberType}}));
TEST_IS_NOT_SUBTYPE(tbl({{"X", builtinTypes->numberType}}), tbl({{"X", builtinTypes->numberType}, {"Y", builtinTypes->numberType}}));

TEST_CASE_FIXTURE(SubtypeFixture, "interior_tests_are_cached")
{
    TypeId tableA = tbl({{"X", builtinTypes->numberType}, {"Y", builtinTypes->numberType}});
    TypeId tableB = tbl({{"X", builtinTypes->optionalNumberType}, {"Y", builtinTypes->optionalNumberType}});

    CHECK_IS_NOT_SUBTYPE(tableA, tableB);

    const SubtypingResult* cachedResult = subtyping.peekCache().find({builtinTypes->numberType, builtinTypes->optionalNumberType});
    REQUIRE(cachedResult);

    CHECK(cachedResult->isSubtype);

    cachedResult = subtyping.peekCache().find({tableA, tableB});
    REQUIRE(cachedResult);

    CHECK(!cachedResult->isSubtype);
}

TEST_CASE_FIXTURE(SubtypeFixture, "results_that_are_contingent_on_generics_are_not_cached")
{
    // <T>(T) -> T <: (number) -> number
    CHECK_IS_SUBTYPE(genericTToTType, numberToNumberType);

    CHECK(subtyping.peekCache().empty());
}

TEST_CASE_FIXTURE(SubtypeFixture, "dont_cache_tests_involving_cycles")
{
    TypeId tableA = arena.addType(BlockedType{});
    TypeId tableA2 = tbl({{"self", tableA}});
    asMutable(tableA)->ty.emplace<BoundType>(tableA2);

    TypeId tableB = arena.addType(BlockedType{});
    TypeId tableB2 = tbl({{"self", tableB}});
    asMutable(tableB)->ty.emplace<BoundType>(tableB2);

    CHECK_IS_SUBTYPE(tableA, tableB);

    CHECK(!subtyping.peekCache().find({tableA, tableB}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "<T>({ x: T }) -> T <: ({ method: <T>({ x: T }) -> T, x: number }) -> number")
{
    // <T>({ x: T }) -> T
    TypeId tableToPropType = arena.addType(FunctionType{
        {genericT},
        {},
        arena.addTypePack({tbl({{"x", genericT}})}),
        arena.addTypePack({genericT})
    });

    // ({ method: <T>({ x: T }) -> T, x: number }) -> number
    TypeId otherType = fn(
        {tbl({{"method", tableToPropType}, {"x", builtinTypes->numberType}})},
        {builtinTypes->numberType}
    );

    CHECK_IS_SUBTYPE(tableToPropType, otherType);
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("Subtyping.Subpaths");

TEST_CASE_FIXTURE(SubtypeFixture, "table_property")
{
    TypeId subTy = tbl({{"X", builtinTypes->numberType}});
    TypeId superTy = tbl({{"X", builtinTypes->booleanType}});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(result.reasoning == std::vector{SubtypingReasoning{/* subPath */ Path(TypePath::Property("X")),
                                  /* superPath */ Path(TypePath::Property("X")),
                                  /* variance */ SubtypingVariance::Invariant}});
}

TEST_CASE_FIXTURE(SubtypeFixture, "table_indexers")
{
    TypeId subTy = idx(builtinTypes->numberType, builtinTypes->stringType);
    TypeId superTy = idx(builtinTypes->stringType, builtinTypes->numberType);

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(result.reasoning == std::vector{SubtypingReasoning{
                                              /* subPath */ Path(TypePath::TypeField::IndexLookup),
                                              /* superPath */ Path(TypePath::TypeField::IndexLookup),
                                              /* variance */ SubtypingVariance::Invariant,
                                          },
                                  SubtypingReasoning{
                                      /* subPath */ Path(TypePath::TypeField::IndexResult),
                                      /* superPath */ Path(TypePath::TypeField::IndexResult),
                                      /* variance */ SubtypingVariance::Invariant,
                                  }});
}

TEST_CASE_FIXTURE(SubtypeFixture, "fn_arguments")
{
    TypeId subTy = fn({builtinTypes->numberType}, {});
    TypeId superTy = fn({builtinTypes->stringType}, {});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(result.reasoning == std::vector{SubtypingReasoning{
                                  /* subPath */ TypePath::PathBuilder().args().index(0).build(),
                                  /* superPath */ TypePath::PathBuilder().args().index(0).build(),
                                  /* variance */ SubtypingVariance::Contravariant,
                              }});
}

TEST_CASE_FIXTURE(SubtypeFixture, "arity_mismatch")
{
    TypeId subTy = fn({builtinTypes->numberType}, {});
    TypeId superTy = fn({}, {});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(result.reasoning == std::vector{SubtypingReasoning{
                                  /* subPath */ TypePath::PathBuilder().args().build(),
                                  /* superPath */ TypePath::PathBuilder().args().build(),
                                  /* variance */ SubtypingVariance::Contravariant,
                              }});
}

TEST_CASE_FIXTURE(SubtypeFixture, "fn_arguments_tail")
{
    TypeId subTy = fn({}, VariadicTypePack{builtinTypes->numberType}, {});
    TypeId superTy = fn({}, VariadicTypePack{builtinTypes->stringType}, {});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(result.reasoning == std::vector{SubtypingReasoning{
                                  /* subPath */ TypePath::PathBuilder().args().tail().variadic().build(),
                                  /* superPath */ TypePath::PathBuilder().args().tail().variadic().build(),
                                  /* variance */ SubtypingVariance::Contravariant,
                              }});
}

TEST_CASE_FIXTURE(SubtypeFixture, "fn_rets")
{
    TypeId subTy = fn({}, {builtinTypes->numberType});
    TypeId superTy = fn({}, {builtinTypes->stringType});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(result.reasoning == std::vector{SubtypingReasoning{
                                  /* subPath */ TypePath::PathBuilder().rets().index(0).build(),
                                  /* superPath */ TypePath::PathBuilder().rets().index(0).build(),
                              }});
}

TEST_CASE_FIXTURE(SubtypeFixture, "fn_rets_tail")
{
    TypeId subTy = fn({}, {}, VariadicTypePack{builtinTypes->numberType});
    TypeId superTy = fn({}, {}, VariadicTypePack{builtinTypes->stringType});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(result.reasoning == std::vector{SubtypingReasoning{
                                  /* subPath */ TypePath::PathBuilder().rets().tail().variadic().build(),
                                  /* superPath */ TypePath::PathBuilder().rets().tail().variadic().build(),
                              }});
}

TEST_CASE_FIXTURE(SubtypeFixture, "nested_table_properties")
{
    TypeId subTy = tbl({{"X", tbl({{"Y", tbl({{"Z", builtinTypes->numberType}})}})}});
    TypeId superTy = tbl({{"X", tbl({{"Y", tbl({{"Z", builtinTypes->stringType}})}})}});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(result.reasoning == std::vector{SubtypingReasoning{
                                  /* subPath */ TypePath::PathBuilder().prop("X").prop("Y").prop("Z").build(),
                                  /* superPath */ TypePath::PathBuilder().prop("X").prop("Y").prop("Z").build(),
                                  /* variance */ SubtypingVariance::Invariant,
                              }});
}

TEST_CASE_FIXTURE(SubtypeFixture, "string_table_mt")
{
    TypeId subTy = builtinTypes->stringType;
    TypeId superTy = tbl({{"X", builtinTypes->numberType}});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    // This check is weird. Because we don't have built-in types, we don't have
    // the string metatable. That means subtyping will see that the entire
    // metatable is empty, and abort there, without looking at the metatable
    // properties (because there aren't any).
    CHECK(result.reasoning == std::vector{SubtypingReasoning{
                                  /* subPath */ TypePath::PathBuilder().mt().prop("__index").build(),
                                  /* superPath */ TypePath::kEmpty,
                              }});
}

TEST_CASE_FIXTURE(SubtypeFixture, "negation")
{
    TypeId subTy = builtinTypes->numberType;
    TypeId superTy = negate(builtinTypes->numberType);

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(result.reasoning == std::vector{SubtypingReasoning{
                                  /* subPath */ TypePath::kEmpty,
                                  /* superPath */ Path(TypePath::TypeField::Negated),
                              }});
}

TEST_CASE_FIXTURE(SubtypeFixture, "multiple_reasonings")
{
    TypeId subTy = tbl({{"X", builtinTypes->stringType}, {"Y", builtinTypes->numberType}});
    TypeId superTy = tbl({{"X", builtinTypes->numberType}, {"Y", builtinTypes->stringType}});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(result.reasoning == std::vector{
                                  SubtypingReasoning{/* subPath */ Path(TypePath::Property("X")), /* superPath */ Path(TypePath::Property("X")),
                                      /* variance */ SubtypingVariance::Invariant},
                                  SubtypingReasoning{/* subPath */ Path(TypePath::Property("Y")), /* superPath */ Path(TypePath::Property("Y")),
                                      /* variance */ SubtypingVariance::Invariant},
                              });
}

TEST_SUITE_END();
