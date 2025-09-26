// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeFwd.h"
#include "Luau/TypePath.h"

#include "Luau/Normalize.h"
#include "Luau/Subtyping.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/TypeFunction.h"

#include "doctest.h"
#include "Fixture.h"
#include "RegisterCallbacks.h"

#include <initializer_list>

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauReturnMappedGenericPacksFromSubtyping3)
LUAU_FASTFLAG(LuauSubtypingGenericsDoesntUseVariance)
LUAU_FASTFLAG(LuauVariadicAnyPackShouldBeErrorSuppressing)
LUAU_FASTFLAG(LuauSubtypingGenericPacksDoesntUseVariance)
LUAU_FASTFLAG(LuauPassBindableGenericsByReference)

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
    SimplifierPtr simplifier = newSimplifier(NotNull{&arena}, getBuiltins());
    Normalizer normalizer{&arena, getBuiltins(), NotNull{&sharedState}, FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old};
    TypeCheckLimits limits;
    TypeFunctionRuntime typeFunctionRuntime{NotNull{&iceReporter}, NotNull{&limits}};

    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
    ScopedFastFlag sff1{FFlag::LuauPassBindableGenericsByReference, true};

    ScopePtr rootScope{new Scope(getBuiltins()->emptyTypePack)};
    ScopePtr moduleScope{new Scope(rootScope)};

    Subtyping subtyping = mkSubtyping();
    BuiltinTypeFunctions builtinTypeFunctions{};

    Subtyping mkSubtyping()
    {
        return Subtyping{
            getBuiltins(), NotNull{&arena}, NotNull{simplifier.get()}, NotNull{&normalizer}, NotNull{&typeFunctionRuntime}, NotNull{&iceReporter}
        };
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
        return arena.addType(ExternType{name, {}, parent.value_or(getBuiltins()->externType), {}, {}, nullptr, "", {}});
    }

    TypeId cls(const std::string& name, ExternType::Props&& props)
    {
        TypeId ty = cls(name);
        getMutable<ExternType>(ty)->props = std::move(props);
        return ty;
    }

    TypeId opt(TypeId ty)
    {
        return join(ty, getBuiltins()->nilType);
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
        return subtyping.isSubtype(subTy, superTy, NotNull{rootScope.get()});
    }

    SubtypingResult isSubtype(TypePackId subTy, TypePackId superTy)
    {
        return subtyping.isSubtype(subTy, superTy, NotNull{rootScope.get()}, {});
    }

    TypeId helloType = arena.addType(SingletonType{StringSingleton{"hello"}});
    TypeId helloType2 = arena.addType(SingletonType{StringSingleton{"hello"}});
    TypeId worldType = arena.addType(SingletonType{StringSingleton{"world"}});

    TypeId aType = arena.addType(SingletonType{StringSingleton{"a"}});
    TypeId bType = arena.addType(SingletonType{StringSingleton{"b"}});
    TypeId trueSingleton = arena.addType(SingletonType{BooleanSingleton{true}});
    TypeId falseSingleton = arena.addType(SingletonType{BooleanSingleton{false}});
    TypeId helloOrWorldType = join(helloType, worldType);
    TypeId trueOrFalseType = join(getBuiltins()->trueType, getBuiltins()->falseType);

    TypeId helloAndWorldType = meet(helloType, worldType);
    TypeId booleanAndTrueType = meet(getBuiltins()->booleanType, getBuiltins()->trueType);

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

    TypeId vec2Class =
        cls("Vec2",
            {
                {"X", getBuiltins()->numberType},
                {"Y", getBuiltins()->numberType},
            });

    TypeId readOnlyVec2Class =
        cls("ReadOnlyVec2",
            {
                {"X", Property::readonly(getBuiltins()->numberType)},
                {"Y", Property::readonly(getBuiltins()->numberType)},
            });

    // "hello" | "hello"
    TypeId helloOrHelloType = arena.addType(UnionType{{helloType, helloType}});

    // () -> ()
    const TypeId nothingToNothingType = fn({}, {});

    // (number) -> string
    const TypeId numberToStringType = fn({getBuiltins()->numberType}, {getBuiltins()->stringType});

    // (unknown) -> string
    const TypeId unknownToStringType = fn({getBuiltins()->unknownType}, {getBuiltins()->stringType});

    // (number) -> ()
    const TypeId numberToNothingType = fn({getBuiltins()->numberType}, {});

    // () -> number
    const TypeId nothingToNumberType = fn({}, {getBuiltins()->numberType});

    // (number) -> number
    const TypeId numberToNumberType = fn({getBuiltins()->numberType}, {getBuiltins()->numberType});

    // (number) -> unknown
    const TypeId numberToUnknownType = fn({getBuiltins()->numberType}, {getBuiltins()->unknownType});

    // (number) -> (string, string)
    const TypeId numberToTwoStringsType = fn({getBuiltins()->numberType}, {getBuiltins()->stringType, getBuiltins()->stringType});

    // (number) -> (string, unknown)
    const TypeId numberToStringAndUnknownType = fn({getBuiltins()->numberType}, {getBuiltins()->stringType, getBuiltins()->unknownType});

    // (number, number) -> string
    const TypeId numberNumberToStringType = fn({getBuiltins()->numberType, getBuiltins()->numberType}, {getBuiltins()->stringType});

    // (unknown, number) -> string
    const TypeId unknownNumberToStringType = fn({getBuiltins()->unknownType, getBuiltins()->numberType}, {getBuiltins()->stringType});

    // (number, string) -> string
    const TypeId numberAndStringToStringType = fn({getBuiltins()->numberType, getBuiltins()->stringType}, {getBuiltins()->stringType});

    // (number, ...string) -> string
    const TypeId numberAndStringsToStringType =
        fn({getBuiltins()->numberType}, VariadicTypePack{getBuiltins()->stringType}, {getBuiltins()->stringType});

    // (number, ...string?) -> string
    const TypeId numberAndOptionalStringsToStringType =
        fn({getBuiltins()->numberType}, VariadicTypePack{getBuiltins()->optionalStringType}, {getBuiltins()->stringType});

    // (...number) -> number
    const TypeId numbersToNumberType =
        arena.addType(FunctionType{arena.addTypePack(VariadicTypePack{getBuiltins()->numberType}), arena.addTypePack({getBuiltins()->numberType})});

    // <T>(T) -> ()
    const TypeId genericTToNothingType = arena.addType(FunctionType{{genericT}, {}, arena.addTypePack({genericT}), getBuiltins()->emptyTypePack});

    // <T>(T) -> T
    const TypeId genericTToTType = arena.addType(FunctionType{{genericT}, {}, arena.addTypePack({genericT}), arena.addTypePack({genericT})});

    // <U>(U) -> ()
    const TypeId genericUToNothingType = arena.addType(FunctionType{{genericU}, {}, arena.addTypePack({genericU}), getBuiltins()->emptyTypePack});

    // <T>() -> T
    const TypeId genericNothingToTType = arena.addType(FunctionType{{genericT}, {}, getBuiltins()->emptyTypePack, arena.addTypePack({genericT})});

    // <A...>(A...) -> A...
    const TypeId genericAsToAsType = arena.addType(FunctionType{{}, {genericAs}, genericAs, genericAs});

    // <A...>(A...) -> number
    const TypeId genericAsToNumberType = arena.addType(FunctionType{{}, {genericAs}, genericAs, arena.addTypePack({getBuiltins()->numberType})});

    // <B...>(B...) -> B...
    const TypeId genericBsToBsType = arena.addType(FunctionType{{}, {genericBs}, genericBs, genericBs});

    // <B..., C...>(B...) -> C...
    const TypeId genericBsToCsType = arena.addType(FunctionType{{}, {genericBs, genericCs}, genericBs, genericCs});

    // <A...>() -> A...
    const TypeId genericNothingToAsType = arena.addType(FunctionType{{}, {genericAs}, getBuiltins()->emptyTypePack, genericAs});

    // { lower : string -> string }
    TypeId tableWithLower = tbl(TableType::Props{{"lower", fn({getBuiltins()->stringType}, {getBuiltins()->stringType})}});
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
        doctest::detail::regTest( \
            doctest::detail::TestCase( \
                run, __FILE__, __LINE__, doctest_detail_test_suite_ns::getCurrentTestSuite() \
            )              /* the test case's name, determined at runtime */ \
            * name.c_str() /* getCurrentTestSuite() only works at static initialization \
                           time due to implementation details. To ensure that test cases \
                           are grouped where they should be, manually override the suite \
                           with the test_suite decorator. */ \
            * doctest::test_suite("Subtyping") \
        ); \
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

TEST_IS_SUBTYPE(getBuiltins()->numberType, getBuiltins()->anyType);
TEST_IS_NOT_SUBTYPE(getBuiltins()->numberType, getBuiltins()->stringType);

TEST_CASE_FIXTURE(SubtypeFixture, "basic_reducible_sub_type_function")
{
    // add<number, number> <: number
    TypeId typeFunctionNum =
        arena.addType(TypeFunctionInstanceType{NotNull{&builtinTypeFunctions.addFunc}, {getBuiltins()->numberType, getBuiltins()->numberType}, {}});
    TypeId superTy = getBuiltins()->numberType;
    SubtypingResult result = isSubtype(typeFunctionNum, superTy);
    CHECK(result.isSubtype);
}

TEST_CASE_FIXTURE(SubtypeFixture, "basic_reducible_super_type_function")
{
    // number <: add<number, number> ~ number
    TypeId typeFunctionNum =
        arena.addType(TypeFunctionInstanceType{NotNull{&builtinTypeFunctions.addFunc}, {getBuiltins()->numberType, getBuiltins()->numberType}, {}});
    TypeId subTy = getBuiltins()->numberType;
    SubtypingResult result = isSubtype(subTy, typeFunctionNum);
    CHECK(result.isSubtype);
}

TEST_CASE_FIXTURE(SubtypeFixture, "basic_irreducible_sub_type_function")
{
    // add<string, boolean> ~ never <: number
    TypeId typeFunctionNum =
        arena.addType(TypeFunctionInstanceType{NotNull{&builtinTypeFunctions.addFunc}, {getBuiltins()->stringType, getBuiltins()->booleanType}, {}});
    TypeId superTy = getBuiltins()->numberType;
    SubtypingResult result = isSubtype(typeFunctionNum, superTy);
    CHECK(result.isSubtype);
}

TEST_CASE_FIXTURE(SubtypeFixture, "basic_irreducible_super_type_function")
{
    // number <\: add<string, boolean> ~ irreducible/never
    TypeId typeFunctionNum =
        arena.addType(TypeFunctionInstanceType{NotNull{&builtinTypeFunctions.addFunc}, {getBuiltins()->stringType, getBuiltins()->booleanType}, {}});
    TypeId subTy = getBuiltins()->numberType;
    SubtypingResult result = isSubtype(subTy, typeFunctionNum);
    CHECK(!result.isSubtype);
}

TEST_CASE_FIXTURE(SubtypeFixture, "basic_type_function_with_generics")
{
    // <T,U>(x: T, x: U) -> add<T,U> <: (number, number) -> number
    TypeId addTypeFunction = arena.addType(TypeFunctionInstanceType{NotNull{&builtinTypeFunctions.addFunc}, {genericT, genericU}, {}});
    FunctionType ft{{genericT, genericU}, {}, arena.addTypePack({genericT, genericU}), arena.addTypePack({addTypeFunction})};
    TypeId functionType = arena.addType(std::move(ft));
    FunctionType superFt{arena.addTypePack({getBuiltins()->numberType, getBuiltins()->numberType}), arena.addTypePack({getBuiltins()->numberType})};
    TypeId superFunction = arena.addType(std::move(superFt));
    SubtypingResult result = isSubtype(functionType, superFunction);
    CHECK(result.isSubtype);
}

TEST_CASE_FIXTURE(SubtypeFixture, "variadic_subpath_in_pack")
{
    TypePackId subTArgs = arena.addTypePack(TypePack{{getBuiltins()->stringType, getBuiltins()->stringType}, getBuiltins()->anyTypePack});
    TypePackId superTArgs = arena.addTypePack(TypePack{{getBuiltins()->numberType}, getBuiltins()->anyTypePack});
    // (string, string, ...any) -> number
    TypeId functionSub = arena.addType(FunctionType{subTArgs, arena.addTypePack({getBuiltins()->numberType})});
    // (number, ...any) -> string
    TypeId functionSuper = arena.addType(FunctionType{superTArgs, arena.addTypePack({getBuiltins()->stringType})});


    SubtypingResult result = isSubtype(functionSub, functionSuper);
    CHECK(
        result.reasoning ==
        std::vector{
            SubtypingReasoning{
                TypePath::PathBuilder().rets().index(0).build(), TypePath::PathBuilder().rets().index(0).build(), SubtypingVariance::Covariant
            },
            SubtypingReasoning{
                TypePath::PathBuilder().args().index(0).build(), TypePath::PathBuilder().args().index(0).build(), SubtypingVariance::Contravariant
            },
            SubtypingReasoning{
                TypePath::PathBuilder().args().index(1).build(),
                TypePath::PathBuilder().args().tail().variadic().build(),
                SubtypingVariance::Contravariant
            }
        }
    );
    CHECK(!result.isSubtype);
}

TEST_CASE_FIXTURE(SubtypeFixture, "any <: unknown")
{
    // We have added this as an exception - the set of inhabitants of any is exactly the set of inhabitants of unknown (since error has no
    // inhabitants). any = err | unknown, so under semantic subtyping, {} U unknown = unknown
    CHECK_IS_SUBTYPE(getBuiltins()->anyType, getBuiltins()->unknownType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "number? <: unknown")
{
    CHECK_IS_SUBTYPE(getBuiltins()->optionalNumberType, getBuiltins()->unknownType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "number <: unknown")
{
    CHECK_IS_SUBTYPE(getBuiltins()->numberType, getBuiltins()->unknownType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "number <: number")
{
    CHECK_IS_SUBTYPE(getBuiltins()->numberType, getBuiltins()->numberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "number <: number?")
{
    CHECK_IS_SUBTYPE(getBuiltins()->numberType, getBuiltins()->optionalNumberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "\"hello\" <: string")
{
    CHECK_IS_SUBTYPE(helloType, getBuiltins()->stringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "string <!: \"hello\"")
{
    CHECK_IS_NOT_SUBTYPE(getBuiltins()->stringType, helloType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "\"hello\" <: \"hello\"")
{
    CHECK_IS_SUBTYPE(helloType, helloType2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "true <: boolean")
{
    CHECK_IS_SUBTYPE(getBuiltins()->trueType, getBuiltins()->booleanType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "true <: true | false")
{
    CHECK_IS_SUBTYPE(getBuiltins()->trueType, trueOrFalseType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "true | false <!: true")
{
    CHECK_IS_NOT_SUBTYPE(trueOrFalseType, getBuiltins()->trueType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "true | false <: boolean")
{
    CHECK_IS_SUBTYPE(trueOrFalseType, getBuiltins()->booleanType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "true | false <: true | false")
{
    CHECK_IS_SUBTYPE(trueOrFalseType, trueOrFalseType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "\"hello\" | \"world\" <: number")
{
    CHECK_IS_NOT_SUBTYPE(helloOrWorldType, getBuiltins()->numberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "string <!: ('hello' | 'hello')")
{
    CHECK_IS_NOT_SUBTYPE(getBuiltins()->stringType, helloOrHelloType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "true <: boolean & true")
{
    CHECK_IS_SUBTYPE(getBuiltins()->trueType, booleanAndTrueType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "boolean & true <: true")
{
    CHECK_IS_SUBTYPE(booleanAndTrueType, getBuiltins()->trueType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "boolean & true <: boolean & true")
{
    CHECK_IS_SUBTYPE(booleanAndTrueType, booleanAndTrueType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "\"hello\" & \"world\" <: number")
{
    CHECK_IS_SUBTYPE(helloAndWorldType, getBuiltins()->numberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "false <!: boolean & true")
{
    CHECK_IS_NOT_SUBTYPE(getBuiltins()->falseType, booleanAndTrueType);
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

TEST_CASE_FIXTURE(SubtypeFixture, "(number) -> () <!: <T>(T) -> ()")
{
    CHECK_IS_NOT_SUBTYPE(numberToNothingType, genericTToNothingType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<T>() -> (T, T) <!: () -> (string, number)")
{
    TypeId nothingToTwoTs = arena.addType(FunctionType{{genericT}, {}, getBuiltins()->emptyTypePack, arena.addTypePack({genericT, genericT})});

    TypeId nothingToStringAndNumber = fn({}, {getBuiltins()->stringType, getBuiltins()->numberType});

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
    CHECK_IS_SUBTYPE(tbl({{"x", getBuiltins()->numberType}}), tbl({}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{} <!: {x: number}")
{
    CHECK_IS_NOT_SUBTYPE(tbl({}), tbl({{"x", getBuiltins()->numberType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{x: number} <!: {x: string}")
{
    CHECK_IS_NOT_SUBTYPE(tbl({{"x", getBuiltins()->numberType}}), tbl({{"x", getBuiltins()->stringType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{x: number} <!: {x: number?}")
{
    CHECK_IS_NOT_SUBTYPE(tbl({{"x", getBuiltins()->numberType}}), tbl({{"x", getBuiltins()->optionalNumberType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{x: number?} <!: {x: number}")
{
    CHECK_IS_NOT_SUBTYPE(tbl({{"x", getBuiltins()->optionalNumberType}}), tbl({{"x", getBuiltins()->numberType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{x: <T>(T) -> ()} <: {x: <U>(U) -> ()}")
{
    CHECK_IS_SUBTYPE(tbl({{"x", genericTToNothingType}}), tbl({{"x", genericUToNothingType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ x: number } <: { read x: number }")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    CHECK_IS_SUBTYPE(tbl({{"x", getBuiltins()->numberType}}), tbl({{"x", Property::readonly(getBuiltins()->numberType)}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ x: number } <: { write x: number }")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    CHECK_IS_SUBTYPE(tbl({{"x", getBuiltins()->numberType}}), tbl({{"x", Property::writeonly(getBuiltins()->numberType)}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ x: \"hello\" } <: { read x: string }")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    CHECK_IS_SUBTYPE(tbl({{"x", helloType}}), tbl({{"x", Property::readonly(getBuiltins()->stringType)}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ x: string } <: { write x: string }")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    CHECK_IS_SUBTYPE(tbl({{"x", getBuiltins()->stringType}}), tbl({{"x", Property::writeonly(getBuiltins()->stringType)}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable { x: number } } <: { @metatable {} }")
{
    CHECK_IS_SUBTYPE(meta({{"x", getBuiltins()->numberType}}), meta({}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable { x: number } } <!: { @metatable { x: boolean } }")
{
    CHECK_IS_NOT_SUBTYPE(meta({{"x", getBuiltins()->numberType}}), meta({{"x", getBuiltins()->booleanType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable {} } <!: { @metatable { x: boolean } }")
{
    CHECK_IS_NOT_SUBTYPE(meta({}), meta({{"x", getBuiltins()->booleanType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable {} } <: {}")
{
    CHECK_IS_SUBTYPE(meta({}), tbl({}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable { u: boolean }, x: number } <: { x: number }")
{
    CHECK_IS_SUBTYPE(meta({{"u", getBuiltins()->booleanType}}, {{"x", getBuiltins()->numberType}}), tbl({{"x", getBuiltins()->numberType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ @metatable { x: number } } <!: { x: number }")
{
    CHECK_IS_NOT_SUBTYPE(meta({{"x", getBuiltins()->numberType}}), tbl({{"x", getBuiltins()->numberType}}));
}

TEST_IS_SUBTYPE(getBuiltins()->tableType, tbl({}));
TEST_IS_SUBTYPE(tbl({}), getBuiltins()->tableType);

// Negated subtypes
TEST_IS_NOT_SUBTYPE(negate(getBuiltins()->neverType), getBuiltins()->stringType);
TEST_IS_SUBTYPE(negate(getBuiltins()->unknownType), getBuiltins()->stringType);
TEST_IS_SUBTYPE(negate(getBuiltins()->anyType), getBuiltins()->stringType);
TEST_IS_SUBTYPE(negate(meet(getBuiltins()->neverType, getBuiltins()->unknownType)), getBuiltins()->stringType);
TEST_IS_SUBTYPE(negate(join(getBuiltins()->neverType, getBuiltins()->unknownType)), getBuiltins()->stringType);

// Negated supertypes: never/unknown/any/error
TEST_IS_SUBTYPE(getBuiltins()->stringType, negate(getBuiltins()->neverType));
TEST_IS_SUBTYPE(getBuiltins()->neverType, negate(getBuiltins()->unknownType));
TEST_IS_NOT_SUBTYPE(getBuiltins()->stringType, negate(getBuiltins()->unknownType));
TEST_IS_SUBTYPE(getBuiltins()->numberType, negate(getBuiltins()->anyType));
TEST_IS_SUBTYPE(getBuiltins()->unknownType, negate(getBuiltins()->anyType));

// Negated supertypes: unions
TEST_IS_SUBTYPE(getBuiltins()->booleanType, negate(join(getBuiltins()->stringType, getBuiltins()->numberType)));
TEST_IS_SUBTYPE(rootClass, negate(join(childClass, getBuiltins()->numberType)));
TEST_IS_SUBTYPE(str("foo"), negate(join(getBuiltins()->numberType, getBuiltins()->booleanType)));
TEST_IS_NOT_SUBTYPE(str("foo"), negate(join(getBuiltins()->stringType, getBuiltins()->numberType)));
TEST_IS_NOT_SUBTYPE(childClass, negate(join(rootClass, getBuiltins()->numberType)));
TEST_IS_NOT_SUBTYPE(numbersToNumberType, negate(join(getBuiltins()->functionType, rootClass)));

// Negated supertypes: intersections
TEST_IS_SUBTYPE(getBuiltins()->booleanType, negate(meet(getBuiltins()->stringType, str("foo"))));
TEST_IS_SUBTYPE(getBuiltins()->trueType, negate(meet(getBuiltins()->booleanType, getBuiltins()->numberType)));
TEST_IS_SUBTYPE(rootClass, negate(meet(getBuiltins()->externType, childClass)));
TEST_IS_SUBTYPE(childClass, negate(meet(getBuiltins()->externType, getBuiltins()->numberType)));
TEST_IS_SUBTYPE(getBuiltins()->unknownType, negate(meet(getBuiltins()->externType, getBuiltins()->numberType)));
TEST_IS_NOT_SUBTYPE(str("foo"), negate(meet(getBuiltins()->stringType, negate(str("bar")))));

// Negated supertypes: tables and metatables
TEST_IS_SUBTYPE(tbl({}), negate(getBuiltins()->numberType));
TEST_IS_NOT_SUBTYPE(tbl({}), negate(getBuiltins()->tableType));
TEST_IS_SUBTYPE(meta({}), negate(getBuiltins()->numberType));
TEST_IS_NOT_SUBTYPE(meta({}), negate(getBuiltins()->tableType));

// Negated supertypes: Functions
TEST_IS_SUBTYPE(numberToNumberType, negate(getBuiltins()->externType));
TEST_IS_NOT_SUBTYPE(numberToNumberType, negate(getBuiltins()->functionType));

// Negated supertypes: Primitives and singletons
TEST_IS_NOT_SUBTYPE(getBuiltins()->stringType, negate(getBuiltins()->stringType));
TEST_IS_SUBTYPE(getBuiltins()->stringType, negate(getBuiltins()->numberType));
TEST_IS_SUBTYPE(str("foo"), meet(getBuiltins()->stringType, negate(str("bar"))));
TEST_IS_NOT_SUBTYPE(getBuiltins()->trueType, negate(getBuiltins()->booleanType));
TEST_IS_NOT_SUBTYPE(str("foo"), negate(str("foo")));
TEST_IS_NOT_SUBTYPE(str("foo"), negate(getBuiltins()->stringType));
TEST_IS_SUBTYPE(getBuiltins()->falseType, negate(getBuiltins()->trueType));
TEST_IS_SUBTYPE(getBuiltins()->falseType, meet(getBuiltins()->booleanType, negate(getBuiltins()->trueType)));
TEST_IS_NOT_SUBTYPE(getBuiltins()->stringType, meet(getBuiltins()->booleanType, negate(getBuiltins()->trueType)));
TEST_IS_NOT_SUBTYPE(getBuiltins()->stringType, negate(str("foo")));
TEST_IS_NOT_SUBTYPE(getBuiltins()->booleanType, negate(getBuiltins()->falseType));

// Negated supertypes: extern types
TEST_IS_SUBTYPE(rootClass, negate(getBuiltins()->tableType));
TEST_IS_NOT_SUBTYPE(rootClass, negate(getBuiltins()->externType));
TEST_IS_NOT_SUBTYPE(childClass, negate(rootClass));
TEST_IS_NOT_SUBTYPE(childClass, meet(getBuiltins()->externType, negate(rootClass)));
TEST_IS_SUBTYPE(anotherChildClass, meet(getBuiltins()->externType, negate(childClass)));

// Negated primitives against unknown
TEST_IS_NOT_SUBTYPE(getBuiltins()->unknownType, negate(getBuiltins()->booleanType));
TEST_IS_NOT_SUBTYPE(getBuiltins()->unknownType, negate(getBuiltins()->numberType));
TEST_IS_NOT_SUBTYPE(getBuiltins()->unknownType, negate(getBuiltins()->stringType));
TEST_IS_NOT_SUBTYPE(getBuiltins()->unknownType, negate(getBuiltins()->threadType));
TEST_IS_NOT_SUBTYPE(getBuiltins()->unknownType, negate(getBuiltins()->bufferType));

TEST_CASE_FIXTURE(SubtypeFixture, "Root <: class")
{
    CHECK_IS_SUBTYPE(rootClass, getBuiltins()->externType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child | AnotherChild <: class")
{
    CHECK_IS_SUBTYPE(join(childClass, anotherChildClass), getBuiltins()->externType);
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
    CHECK_IS_SUBTYPE(meet(childClass, anotherChildClass), getBuiltins()->externType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child & Root <: class")
{
    CHECK_IS_SUBTYPE(meet(childClass, rootClass), getBuiltins()->externType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child & ~Root <: class")
{
    CHECK_IS_SUBTYPE(meet(childClass, negate(rootClass)), getBuiltins()->externType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child & AnotherChild <: number")
{
    CHECK_IS_SUBTYPE(meet(childClass, anotherChildClass), getBuiltins()->numberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Child & ~GrandchildOne <!: number")
{
    CHECK_IS_NOT_SUBTYPE(meet(childClass, negate(grandchildOneClass)), getBuiltins()->numberType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "semantic_subtyping_disj")
{
    TypeId subTy = getBuiltins()->unknownType;
    TypeId superTy = join(negate(getBuiltins()->numberType), negate(getBuiltins()->stringType));
    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(result.isSubtype);
}


TEST_CASE_FIXTURE(SubtypeFixture, "t1 where t1 = {trim: (t1) -> string} <: t2 where t2 = {trim: (t2) -> string}")
{
    TypeId t1 = cyclicTable(
        [&](TypeId ty, TableType* tt)
        {
            tt->props["trim"] = fn({ty}, {getBuiltins()->stringType});
        }
    );

    TypeId t2 = cyclicTable(
        [&](TypeId ty, TableType* tt)
        {
            tt->props["trim"] = fn({ty}, {getBuiltins()->stringType});
        }
    );

    CHECK_IS_SUBTYPE(t1, t2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "t1 where t1 = {trim: (t1) -> string} <!: t2 where t2 = {trim: (t2) -> t2}")
{
    TypeId t1 = cyclicTable(
        [&](TypeId ty, TableType* tt)
        {
            tt->props["trim"] = fn({ty}, {getBuiltins()->stringType});
        }
    );

    TypeId t2 = cyclicTable(
        [&](TypeId ty, TableType* tt)
        {
            tt->props["trim"] = fn({ty}, {ty});
        }
    );

    CHECK_IS_NOT_SUBTYPE(t1, t2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "t1 where t1 = {trim: (t1) -> t1} <!: t2 where t2 = {trim: (t2) -> string}")
{
    TypeId t1 = cyclicTable(
        [&](TypeId ty, TableType* tt)
        {
            tt->props["trim"] = fn({ty}, {ty});
        }
    );

    TypeId t2 = cyclicTable(
        [&](TypeId ty, TableType* tt)
        {
            tt->props["trim"] = fn({ty}, {getBuiltins()->stringType});
        }
    );

    CHECK_IS_NOT_SUBTYPE(t1, t2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Vec2 <: { X: number, Y: number }")
{
    TypeId xy = tbl({
        {"X", getBuiltins()->numberType},
        {"Y", getBuiltins()->numberType},
    });

    CHECK_IS_SUBTYPE(vec2Class, xy);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Vec2 <: { X: number }")
{
    TypeId x = tbl({
        {"X", getBuiltins()->numberType},
    });

    CHECK_IS_SUBTYPE(vec2Class, x);
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ X: number, Y: number } <!: Vec2")
{
    TypeId xy = tbl({
        {"X", getBuiltins()->numberType},
        {"Y", getBuiltins()->numberType},
    });

    CHECK_IS_NOT_SUBTYPE(xy, vec2Class);
}

TEST_CASE_FIXTURE(SubtypeFixture, "{ X: number } <!: Vec2")
{
    TypeId x = tbl({
        {"X", getBuiltins()->numberType},
    });

    CHECK_IS_NOT_SUBTYPE(x, vec2Class);
}

TEST_CASE_FIXTURE(SubtypeFixture, "table & { X: number, Y: number } <!: Vec2")
{
    TypeId x = tbl({
        {"X", getBuiltins()->numberType},
        {"Y", getBuiltins()->numberType},
    });

    CHECK_IS_NOT_SUBTYPE(meet(getBuiltins()->tableType, x), vec2Class);
}

TEST_CASE_FIXTURE(SubtypeFixture, "Vec2 <!: table & { X: number, Y: number }")
{
    TypeId xy = tbl({
        {"X", getBuiltins()->numberType},
        {"Y", getBuiltins()->numberType},
    });

    CHECK_IS_NOT_SUBTYPE(vec2Class, meet(getBuiltins()->tableType, xy));
}

TEST_CASE_FIXTURE(SubtypeFixture, "ReadOnlyVec2 <!: { X: number, Y: number}")
{
    CHECK_IS_NOT_SUBTYPE(readOnlyVec2Class, tbl({{"X", getBuiltins()->numberType}, {"Y", getBuiltins()->numberType}}));
}

TEST_CASE_FIXTURE(SubtypeFixture, "ReadOnlyVec2 <: { read X: number, read Y: number}")
{
    CHECK_IS_SUBTYPE(
        readOnlyVec2Class, tbl({{"X", Property::readonly(getBuiltins()->numberType)}, {"Y", Property::readonly(getBuiltins()->numberType)}})
    );
}

TEST_IS_SUBTYPE(vec2Class, tbl({{"X", Property::readonly(getBuiltins()->numberType)}, {"Y", Property::readonly(getBuiltins()->numberType)}}));

TEST_IS_NOT_SUBTYPE(tbl({{"P", grandchildOneClass}}), tbl({{"P", Property::rw(rootClass)}}));
TEST_IS_SUBTYPE(tbl({{"P", grandchildOneClass}}), tbl({{"P", Property::readonly(rootClass)}}));
TEST_IS_SUBTYPE(tbl({{"P", rootClass}}), tbl({{"P", Property::writeonly(grandchildOneClass)}}));

TEST_IS_NOT_SUBTYPE(cls("HasChild", {{"P", childClass}}), tbl({{"P", rootClass}}));
TEST_IS_SUBTYPE(cls("HasChild", {{"P", childClass}}), tbl({{"P", Property::readonly(rootClass)}}));
TEST_IS_NOT_SUBTYPE(cls("HasChild", {{"P", childClass}}), tbl({{"P", grandchildOneClass}}));
TEST_IS_SUBTYPE(cls("HasChild", {{"P", childClass}}), tbl({{"P", Property::writeonly(grandchildOneClass)}}));

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
    CHECK_IS_SUBTYPE(getBuiltins()->stringType, tableWithLower);
}

TEST_CASE_FIXTURE(SubtypeFixture, "string <!: { insaneThingNoScalarHas : () -> () }")
{
    CHECK_IS_NOT_SUBTYPE(getBuiltins()->stringType, tableWithoutScalarProp);
}

TEST_CASE_FIXTURE(SubtypeFixture, "~fun & (string) -> number <: (string) -> number")
{
    CHECK_IS_SUBTYPE(meet(negate(getBuiltins()->functionType), numberToStringType), numberToStringType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(string) -> number <: ~fun & (string) -> number")
{
    CHECK_IS_NOT_SUBTYPE(numberToStringType, meet(negate(getBuiltins()->functionType), numberToStringType));
}

TEST_CASE_FIXTURE(SubtypeFixture, "~\"a\" & ~\"b\" & string <: { lower : (string) -> ()}")
{
    CHECK_IS_SUBTYPE(meet(meet(negate(aType), negate(bType)), getBuiltins()->stringType), tableWithLower);
}

TEST_CASE_FIXTURE(SubtypeFixture, "\"a\" | (~\"b\" & string) <: { lower : (string) -> ()}")
{
    CHECK_IS_SUBTYPE(join(aType, meet(negate(bType), getBuiltins()->stringType)), tableWithLower);
}

TEST_CASE_FIXTURE(SubtypeFixture, "(string | number) & (\"a\" | true) <: { lower: (string) -> string }")
{
    auto base = meet(join(getBuiltins()->stringType, getBuiltins()->numberType), join(aType, trueSingleton));
    CHECK_IS_SUBTYPE(base, tableWithLower);
}

TEST_CASE_FIXTURE(SubtypeFixture, "number <: ~~number")
{
    CHECK_IS_SUBTYPE(getBuiltins()->numberType, negate(negate(getBuiltins()->numberType)));
}

TEST_CASE_FIXTURE(SubtypeFixture, "~~number <: number")
{
    CHECK_IS_SUBTYPE(negate(negate(getBuiltins()->numberType)), getBuiltins()->numberType);
}

// See https://github.com/luau-lang/luau/issues/767
TEST_CASE_FIXTURE(SubtypeFixture, "(...any) -> () <: <T>(T...) -> ()")
{
    TypeId anysToNothing = arena.addType(FunctionType{getBuiltins()->anyTypePack, getBuiltins()->emptyTypePack});
    TypeId genericTToAnys = arena.addType(FunctionType{genericAs, getBuiltins()->emptyTypePack});

    CHECK_MESSAGE(isSubtype(anysToNothing, genericTToAnys).isSubtype, "(...any) -> () <: <T>(T...) -> ()");
}

// See https://github.com/luau-lang/luau/issues/767
TEST_CASE_FIXTURE(SubtypeFixture, "(...unknown) -> () <: <T>(T...) -> ()")
{
    TypeId unknownsToNothing =
        arena.addType(FunctionType{arena.addTypePack(VariadicTypePack{getBuiltins()->unknownType}), getBuiltins()->emptyTypePack});
    TypeId genericTToAnys = arena.addType(FunctionType{genericAs, getBuiltins()->emptyTypePack});

    CHECK_MESSAGE(isSubtype(unknownsToNothing, genericTToAnys).isSubtype, "(...unknown) -> () <: <T>(T...) -> ()");
}

TEST_CASE_FIXTURE(SubtypeFixture, "bill")
{
    TypeId a = arena.addType(
        TableType{
            {{"a", getBuiltins()->stringType}},
            TableIndexer{getBuiltins()->stringType, getBuiltins()->numberType},
            TypeLevel{},
            nullptr,
            TableState::Sealed
        }
    );

    TypeId b = arena.addType(
        TableType{
            {{"a", getBuiltins()->stringType}},
            TableIndexer{getBuiltins()->stringType, getBuiltins()->numberType},
            TypeLevel{},
            nullptr,
            TableState::Sealed
        }
    );

    CHECK(isSubtype(a, b).isSubtype);
    CHECK(isSubtype(b, a).isSubtype);
}

TEST_CASE_FIXTURE(SubtypeFixture, "({[string]: number, a: string}) -> () <: ({[string]: number, a: string}) -> ()")
{
    auto makeTheType = [&]()
    {
        TypeId argType = arena.addType(
            TableType{
                {{"a", getBuiltins()->stringType}},
                TableIndexer{getBuiltins()->stringType, getBuiltins()->numberType},
                TypeLevel{},
                nullptr,
                TableState::Sealed
            }
        );

        return arena.addType(FunctionType{arena.addTypePack({argType}), getBuiltins()->emptyTypePack});
    };

    TypeId a = makeTheType();
    TypeId b = makeTheType();

    CHECK_MESSAGE(isSubtype(a, b).isSubtype, "({[string]: number, a: string}) -> () <: ({[string]: number, a: string}) -> ()");
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

    SubtypingResult usingGlobalScope = isSubtype(getBuiltins()->unknownType, genericX);
    CHECK_MESSAGE(!usingGlobalScope.isSubtype, "Expected " << getBuiltins()->unknownType << " </: " << genericX);

    Subtyping childSubtyping{mkSubtyping()};

    SubtypingResult usingChildScope = childSubtyping.isSubtype(getBuiltins()->unknownType, genericX, NotNull{childScope.get()});
    CHECK_MESSAGE(usingChildScope.isSubtype, "Expected " << getBuiltins()->unknownType << " <: " << genericX);

    Subtyping grandChildSubtyping{mkSubtyping()};

    SubtypingResult usingGrandChildScope = grandChildSubtyping.isSubtype(getBuiltins()->unknownType, genericX, NotNull{grandChildScope.get()});
    CHECK_MESSAGE(usingGrandChildScope.isSubtype, "Expected " << getBuiltins()->unknownType << " <: " << genericX);
}

TEST_IS_SUBTYPE(idx(getBuiltins()->numberType, getBuiltins()->numberType), tbl({}));
TEST_IS_NOT_SUBTYPE(tbl({}), idx(getBuiltins()->numberType, getBuiltins()->numberType));

TEST_IS_NOT_SUBTYPE(tbl({{"X", getBuiltins()->numberType}}), idx(getBuiltins()->numberType, getBuiltins()->numberType));
TEST_IS_NOT_SUBTYPE(idx(getBuiltins()->numberType, getBuiltins()->numberType), tbl({{"X", getBuiltins()->numberType}}));

TEST_IS_NOT_SUBTYPE(
    idx(join(getBuiltins()->numberType, getBuiltins()->stringType), getBuiltins()->numberType),
    idx(getBuiltins()->numberType, getBuiltins()->numberType)
);
TEST_IS_NOT_SUBTYPE(
    idx(getBuiltins()->numberType, getBuiltins()->numberType),
    idx(join(getBuiltins()->numberType, getBuiltins()->stringType), getBuiltins()->numberType)
);

TEST_IS_NOT_SUBTYPE(
    idx(getBuiltins()->numberType, join(getBuiltins()->stringType, getBuiltins()->numberType)),
    idx(getBuiltins()->numberType, getBuiltins()->numberType)
);
TEST_IS_NOT_SUBTYPE(
    idx(getBuiltins()->numberType, getBuiltins()->numberType),
    idx(getBuiltins()->numberType, join(getBuiltins()->stringType, getBuiltins()->numberType))
);

TEST_IS_NOT_SUBTYPE(tbl({{"X", getBuiltins()->numberType}}), idx(getBuiltins()->stringType, getBuiltins()->numberType));
TEST_IS_SUBTYPE(idx(getBuiltins()->stringType, getBuiltins()->numberType), tbl({{"X", getBuiltins()->numberType}}));

TEST_IS_NOT_SUBTYPE(tbl({{"X", opt(getBuiltins()->numberType)}}), idx(getBuiltins()->stringType, getBuiltins()->numberType));
TEST_IS_NOT_SUBTYPE(idx(getBuiltins()->stringType, getBuiltins()->numberType), tbl({{"X", opt(getBuiltins()->numberType)}}));

TEST_IS_SUBTYPE(tbl({{"X", getBuiltins()->numberType}, {"Y", getBuiltins()->numberType}}), tbl({{"X", getBuiltins()->numberType}}));
TEST_IS_NOT_SUBTYPE(tbl({{"X", getBuiltins()->numberType}}), tbl({{"X", getBuiltins()->numberType}, {"Y", getBuiltins()->numberType}}));

TEST_CASE_FIXTURE(SubtypeFixture, "interior_tests_are_cached")
{
    TypeId tableA = tbl({{"X", getBuiltins()->numberType}, {"Y", getBuiltins()->numberType}});
    TypeId tableB = tbl({{"X", getBuiltins()->optionalNumberType}, {"Y", getBuiltins()->optionalNumberType}});

    CHECK_IS_NOT_SUBTYPE(tableA, tableB);

    const SubtypingResult* cachedResult = subtyping.peekCache().find({getBuiltins()->numberType, getBuiltins()->optionalNumberType});
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
    TypeId tableToPropType = arena.addType(FunctionType{{genericT}, {}, arena.addTypePack({tbl({{"x", genericT}})}), arena.addTypePack({genericT})});

    // ({ method: <T>({ x: T }) -> T, x: number }) -> number
    TypeId otherType = fn({tbl({{"method", tableToPropType}, {"x", getBuiltins()->numberType}})}, {getBuiltins()->numberType});

    CHECK_IS_SUBTYPE(tableToPropType, otherType);
}

TEST_CASE_FIXTURE(SubtypeFixture, "subtyping_reasonings_to_follow_a_reduced_type_function_instance")
{
    ScopedFastFlag sff{FFlag::LuauReturnMappedGenericPacksFromSubtyping3, true};
    ScopedFastFlag sff1{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    TypeId longTy = arena.addType(
        UnionType{
            {getBuiltins()->booleanType,
             getBuiltins()->bufferType,
             getBuiltins()->externType,
             getBuiltins()->functionType,
             getBuiltins()->numberType,
             getBuiltins()->stringType,
             getBuiltins()->tableType,
             getBuiltins()->threadType}
        }
    );
    TypeId tblTy = tbl({{"depth", getBuiltins()->unknownType}});
    TypeId combined = meet(longTy, tblTy);
    TypeId subTy = arena.addType(TypeFunctionInstanceType{NotNull{&builtinTypeFunctions.unionFunc}, {combined, getBuiltins()->neverType}, {}});
    TypeId superTy = getBuiltins()->neverType;
    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);

    for (const SubtypingReasoning& reasoning : result.reasoning)
    {
        if (reasoning.subPath.empty() && reasoning.superPath.empty())
            continue;

        std::optional<TypeOrPack> optSubLeaf = traverse(subTy, reasoning.subPath, getBuiltins(), NotNull{&arena});
        std::optional<TypeOrPack> optSuperLeaf = traverse(superTy, reasoning.superPath, getBuiltins(), NotNull{&arena});

        if (!optSubLeaf || !optSuperLeaf)
            CHECK(false);
    }
}

TEST_CASE_FIXTURE(SubtypeFixture, "(() -> number) -> () <: (<T>() -> T) -> ()")
{
    ScopedFastFlag _{FFlag::LuauSubtypingGenericsDoesntUseVariance, true};

    TypeId f1 = fn({nothingToNumberType}, {});
    TypeId f2 = fn({genericNothingToTType}, {});
    CHECK_IS_SUBTYPE(f1, f2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "((number) -> ()) -> () <: (<T>(T) -> ()) -> ()")
{
    ScopedFastFlag _{FFlag::LuauSubtypingGenericsDoesntUseVariance, true};

    TypeId f1 = fn({numberToNothingType}, {});
    TypeId f2 = fn({genericTToNothingType}, {});
    CHECK_IS_SUBTYPE(f1, f2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "((number) -> number) -> () <: (<T>(T) -> T) -> ()")
{
    ScopedFastFlag _{FFlag::LuauSubtypingGenericsDoesntUseVariance, true};

    TypeId f1 = fn({numberToNumberType}, {});
    TypeId f2 = fn({genericTToTType}, {});
    CHECK_IS_SUBTYPE(f1, f2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<T>(x: T, y: T, f: (T, T) -> T) -> T <: (number, number, <U>(U, U) -> add<U, U>) -> number")
{
    ScopedFastFlag _{FFlag::LuauSubtypingGenericsDoesntUseVariance, true};

    TypeId f1 = arena.addType(FunctionType(
        {genericT},
        {},
        arena.addTypePack({genericT, genericT, fn({genericT, genericT}, {genericT})}),
        // (T, T, (T, T) -> T)
        arena.addTypePack({genericT}),
        std::nullopt,
        false
    ));
    TypeId addUToU = arena.addType(TypeFunctionInstanceType{builtinTypeFunctions.addFunc, {genericU, genericU}});
    TypeId f2 = fn(
        {
            builtinTypes->numberType,
            builtinTypes->numberType,
            arena.addType(FunctionType({genericU}, {}, arena.addTypePack({genericU, genericU}), arena.addTypePack({addUToU})))
            // <U>(U, U) -> add<U, U>
        },
        {builtinTypes->numberType}
    );
    CHECK_IS_SUBTYPE(f1, f2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "<A...>(A...) -> (<A...>(A...) -> ()) <: (string -> ((number) -> ())")
{
    ScopedFastFlag sff{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    // <A...>(A...) -> ()
    TypeId asToNothing = arena.addType(FunctionType({}, {genericAs}, genericAs, getBuiltins()->emptyTypePack, std::nullopt, false));
    TypeId f1 = arena.addType(FunctionType({}, {genericAs}, genericAs, pack({asToNothing}), std::nullopt, false));

    TypeId f2 = fn({getBuiltins()->stringType}, {numberToNothingType});
    CHECK_IS_SUBTYPE(f1, f2);
}

TEST_CASE_FIXTURE(SubtypeFixture, "no_caching_type_function_instances_with_mapped_generics")
{
    ScopedFastFlag _{FFlag::LuauSubtypingGenericsDoesntUseVariance, true};

    // (<U>(U) -> keyof<U>, <U>(U) -> keyof<U>) </: (({"a" : number}) -> "a", ({"b" : number}) -> "a")

    TypeId keyOfU = arena.addType(TypeFunctionInstanceType{builtinTypeFunctions.keyofFunc, {genericU}});
    // <U>(U) -> keyof<U>
    TypeId uToKeyOfU = arena.addType(FunctionType({genericU}, {}, arena.addTypePack({genericU}), arena.addTypePack({keyOfU})));
    TypePackId subTypePack = arena.addTypePack({uToKeyOfU, uToKeyOfU});

    TypeId tblA = tbl({{"a", builtinTypes->numberType}});
    TypeId tblB = tbl({{"b", builtinTypes->numberType}});
    TypeId aSingleton = arena.addType(SingletonType{StringSingleton{"a"}});
    TypePackId superTypePack = arena.addTypePack({fn({tblA}, {aSingleton}), fn({tblB}, {aSingleton})});

    CHECK_IS_NOT_SUBTYPE(subTypePack, superTypePack);
}

TEST_CASE_FIXTURE(Fixture, "fuzzer_non_generics_in_function_generics")
{
    // This should not crash
    check(R"(
        local _ = _
        function _(l0)
        for _ in _(_) do
        end
        l0[_](
            _(_()) + _
        )
        end
        _(_)
    )");
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("Subtyping.Subpaths");

TEST_CASE_FIXTURE(SubtypeFixture, "table_property")
{
    TypeId subTy = tbl({{"X", getBuiltins()->numberType}});
    TypeId superTy = tbl({{"X", getBuiltins()->booleanType}});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    REQUIRE(result.reasoning.size() == 1);
    CHECK(
        *result.reasoning.begin() == SubtypingReasoning{/* subPath */ Path(TypePath::Property::read("X")),
                                                        /* superPath */ Path(TypePath::Property::read("X")),
                                                        /* variance */ SubtypingVariance::Invariant}
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "table_indexers")
{
    TypeId subTy = idx(getBuiltins()->numberType, getBuiltins()->stringType);
    TypeId superTy = idx(getBuiltins()->stringType, getBuiltins()->numberType);

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(
        result.reasoning == std::vector{
                                SubtypingReasoning{
                                    /* subPath */ Path(TypePath::TypeField::IndexLookup),
                                    /* superPath */ Path(TypePath::TypeField::IndexLookup),
                                    /* variance */ SubtypingVariance::Invariant,
                                },
                                SubtypingReasoning{
                                    /* subPath */ Path(TypePath::TypeField::IndexResult),
                                    /* superPath */ Path(TypePath::TypeField::IndexResult),
                                    /* variance */ SubtypingVariance::Invariant,
                                }
                            }
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "fn_arguments")
{
    TypeId subTy = fn({getBuiltins()->numberType}, {});
    TypeId superTy = fn({getBuiltins()->stringType}, {});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(
        result.reasoning == std::vector{SubtypingReasoning{
                                /* subPath */ TypePath::PathBuilder().args().index(0).build(),
                                /* superPath */ TypePath::PathBuilder().args().index(0).build(),
                                /* variance */ SubtypingVariance::Contravariant,
                            }}
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "arity_mismatch")
{
    TypeId subTy = fn({getBuiltins()->numberType}, {});
    TypeId superTy = fn({}, {});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(
        result.reasoning == std::vector{SubtypingReasoning{
                                /* subPath */ TypePath::PathBuilder().args().build(),
                                /* superPath */ TypePath::PathBuilder().args().build(),
                                /* variance */ SubtypingVariance::Contravariant,
                            }}
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "fn_arguments_tail")
{
    TypeId subTy = fn({}, VariadicTypePack{getBuiltins()->numberType}, {});
    TypeId superTy = fn({}, VariadicTypePack{getBuiltins()->stringType}, {});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(
        result.reasoning == std::vector{SubtypingReasoning{
                                /* subPath */ TypePath::PathBuilder().args().tail().variadic().build(),
                                /* superPath */ TypePath::PathBuilder().args().tail().variadic().build(),
                                /* variance */ SubtypingVariance::Contravariant,
                            }}
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "fn_rets")
{
    TypeId subTy = fn({}, {getBuiltins()->numberType});
    TypeId superTy = fn({}, {getBuiltins()->stringType});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    REQUIRE(result.reasoning.size() == 1);
    CHECK(
        *result.reasoning.begin() == SubtypingReasoning{
                                         /* subPath */ TypePath::PathBuilder().rets().index(0).build(),
                                         /* superPath */ TypePath::PathBuilder().rets().index(0).build(),
                                     }
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "fn_rets_tail")
{
    TypeId subTy = fn({}, {}, VariadicTypePack{getBuiltins()->numberType});
    TypeId superTy = fn({}, {}, VariadicTypePack{getBuiltins()->stringType});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    REQUIRE(result.reasoning.size() == 1);
    CHECK(
        *result.reasoning.begin() == SubtypingReasoning{
                                         /* subPath */ TypePath::PathBuilder().rets().tail().variadic().build(),
                                         /* superPath */ TypePath::PathBuilder().rets().tail().variadic().build(),
                                     }
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "nested_table_properties")
{
    TypeId subTy = tbl({{"X", tbl({{"Y", tbl({{"Z", getBuiltins()->numberType}})}})}});
    TypeId superTy = tbl({{"X", tbl({{"Y", tbl({{"Z", getBuiltins()->stringType}})}})}});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    REQUIRE(result.reasoning.size() == 1);
    CHECK(
        *result.reasoning.begin() == SubtypingReasoning{
                                         /* subPath */ TypePath::PathBuilder().readProp("X").readProp("Y").readProp("Z").build(),
                                         /* superPath */ TypePath::PathBuilder().readProp("X").readProp("Y").readProp("Z").build(),
                                         /* variance */ SubtypingVariance::Invariant,
                                     }
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "string_table_mt")
{
    TypeId subTy = getBuiltins()->stringType;
    TypeId superTy = tbl({{"X", getBuiltins()->numberType}});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    // This check is weird. Because we don't have built-in types, we don't have
    // the string metatable. That means subtyping will see that the entire
    // metatable is empty, and abort there, without looking at the metatable
    // properties (because there aren't any).
    CHECK(
        result.reasoning == std::vector{SubtypingReasoning{
                                /* subPath */ TypePath::PathBuilder().mt().readProp("__index").build(),
                                /* superPath */ TypePath::kEmpty,
                            }}
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "negation")
{
    TypeId subTy = getBuiltins()->numberType;
    TypeId superTy = negate(getBuiltins()->numberType);

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(
        result.reasoning == std::vector{SubtypingReasoning{
                                /* subPath */ TypePath::kEmpty,
                                /* superPath */ Path(TypePath::TypeField::Negated),
                            }}
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "multiple_reasonings")
{
    TypeId subTy = tbl({{"X", getBuiltins()->stringType}, {"Y", getBuiltins()->numberType}});
    TypeId superTy = tbl({{"X", getBuiltins()->numberType}, {"Y", getBuiltins()->stringType}});

    SubtypingResult result = isSubtype(subTy, superTy);
    CHECK(!result.isSubtype);
    CHECK(
        result.reasoning == std::vector{
                                SubtypingReasoning{/* subPath */ Path(TypePath::Property::read("X")),
                                                   /* superPath */ Path(TypePath::Property::read("X")),
                                                   /* variance */ SubtypingVariance::Invariant},
                                SubtypingReasoning{/* subPath */ Path(TypePath::Property::read("Y")),
                                                   /* superPath */ Path(TypePath::Property::read("Y")),
                                                   /* variance */ SubtypingVariance::Invariant},
                            }
    );
}

TEST_CASE_FIXTURE(SubtypeFixture, "substitute_a_generic_for_a_negation")
{
    // <A, B>(x: A, y: B) -> (A & ~(false?)) | B
    // (~(false?), ~(false?)) -> (~(false?) & ~(false?)) | ~(false?)

    TypeId aTy = arena.addType(GenericType{"A"});
    getMutable<GenericType>(aTy)->scope = moduleScope.get();
    TypeId bTy = arena.addType(GenericType{"B"});
    getMutable<GenericType>(bTy)->scope = moduleScope.get();

    TypeId genericFunctionTy = arena.addType(
        FunctionType{{aTy, bTy}, {}, arena.addTypePack({aTy, bTy}), arena.addTypePack({join(meet(aTy, getBuiltins()->truthyType), bTy)})}
    );

    const TypeId truthyTy = getBuiltins()->truthyType;

    TypeId actualFunctionTy = fn({truthyTy, truthyTy}, {join(meet(truthyTy, getBuiltins()->truthyType), truthyTy)});

    SubtypingResult result = isSubtype(genericFunctionTy, actualFunctionTy);

    CHECK(result.isSubtype);
}

TEST_CASE_FIXTURE(SubtypeFixture, "free_types_might_be_subtypes")
{
    TypeId argTy = arena.freshType(getBuiltins(), moduleScope.get());
    FreeType* freeArg = getMutable<FreeType>(argTy);
    REQUIRE(freeArg);
    freeArg->lowerBound = arena.addType(SingletonType{StringSingleton{"five"}});
    freeArg->upperBound = getBuiltins()->stringType;

    SubtypingResult result = isSubtype(getBuiltins()->stringType, argTy);
    CHECK(result.isSubtype);
    REQUIRE(1 == result.assumedConstraints.size());
}

TEST_CASE_FIXTURE(Fixture, "variadic_any_pack_should_suppress_errors_during_overload_resolution")
{
    ScopedFastFlag sff{FFlag::LuauVariadicAnyPackShouldBeErrorSuppressing, true};
    auto res = check(R"(
type ActionCallback = (string) -> ...any

function bindAction(callback: ActionCallback)
  local _ = function(...)
    callback(...)
  end
end
)");
    LUAU_REQUIRE_NO_ERRORS(res);
}

TEST_SUITE_END();
