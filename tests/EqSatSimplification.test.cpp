// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/EqSatSimplification.h"
#include "Luau/Type.h"

using namespace Luau;

struct ESFixture : Fixture
{
    ScopedFastFlag newSolverOnly{FFlag::LuauSolverV2, true};

    TypeArena arena_;
    const NotNull<TypeArena> arena{&arena_};

    SimplifierPtr simplifier;

    TypeId parentClass;
    TypeId childClass;
    TypeId anotherChild;
    TypeId unrelatedClass;

    TypeId genericT = arena_.addType(GenericType{"T"});
    TypeId genericU = arena_.addType(GenericType{"U"});

    TypeId numberToString =
        arena_.addType(FunctionType{arena_.addTypePack({getBuiltins()->numberType}), arena_.addTypePack({getBuiltins()->stringType})});

    TypeId stringToNumber =
        arena_.addType(FunctionType{arena_.addTypePack({getBuiltins()->stringType}), arena_.addTypePack({getBuiltins()->numberType})});

    ESFixture()
        : simplifier(newSimplifier(arena, getBuiltins()))
    {
        createSomeExternTypes(getFrontend());

        ScopePtr moduleScope = getFrontend().globals.globalScope;

        parentClass = moduleScope->linearSearchForBinding("Parent")->typeId;
        childClass = moduleScope->linearSearchForBinding("Child")->typeId;
        anotherChild = moduleScope->linearSearchForBinding("AnotherChild")->typeId;
        unrelatedClass = moduleScope->linearSearchForBinding("Unrelated")->typeId;
    }

    std::optional<std::string> simplifyStr(TypeId ty)
    {
        auto res = eqSatSimplify(NotNull{simplifier.get()}, ty);
        LUAU_ASSERT(res);
        return toString(res->result);
    }

    TypeId tbl(TableType::Props props)
    {
        return arena->addType(TableType{std::move(props), std::nullopt, TypeLevel{}, TableState::Sealed});
    }
};

TEST_SUITE_BEGIN("EqSatSimplification");

TEST_CASE_FIXTURE(ESFixture, "primitive")
{
    CHECK("number" == simplifyStr(getBuiltins()->numberType));
}

TEST_CASE_FIXTURE(ESFixture, "number | number")
{
    TypeId ty = arena->addType(UnionType{{getBuiltins()->numberType, getBuiltins()->numberType}});

    CHECK("number" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "number | string")
{
    CHECK("number | string" == simplifyStr(arena->addType(UnionType{{getBuiltins()->numberType, getBuiltins()->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "t1 where t1 = number | t1")
{
    TypeId ty = arena->freshType(getBuiltins(), nullptr);
    asMutable(ty)->ty.emplace<UnionType>(std::vector<TypeId>{getBuiltins()->numberType, ty});

    CHECK("number" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "number | string | number")
{
    TypeId ty = arena->addType(UnionType{{getBuiltins()->numberType, getBuiltins()->stringType, getBuiltins()->numberType}});

    CHECK("number | string" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "string | (number | string) | number")
{
    TypeId u1 = arena->addType(UnionType{{getBuiltins()->numberType, getBuiltins()->stringType}});
    TypeId u2 = arena->addType(UnionType{{getBuiltins()->stringType, u1, getBuiltins()->numberType}});

    CHECK("number | string" == simplifyStr(u2));
}

TEST_CASE_FIXTURE(ESFixture, "string | any")
{
    CHECK("any" == simplifyStr(arena->addType(UnionType{{getBuiltins()->stringType, getBuiltins()->anyType}})));
}

TEST_CASE_FIXTURE(ESFixture, "any | string")
{
    CHECK("any" == simplifyStr(arena->addType(UnionType{{getBuiltins()->anyType, getBuiltins()->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "any | never")
{
    CHECK("any" == simplifyStr(arena->addType(UnionType{{getBuiltins()->anyType, getBuiltins()->neverType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string | unknown")
{
    CHECK("unknown" == simplifyStr(arena->addType(UnionType{{getBuiltins()->stringType, getBuiltins()->unknownType}})));
}

TEST_CASE_FIXTURE(ESFixture, "unknown | string")
{
    CHECK("unknown" == simplifyStr(arena->addType(UnionType{{getBuiltins()->unknownType, getBuiltins()->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "unknown | never")
{
    CHECK("unknown" == simplifyStr(arena->addType(UnionType{{getBuiltins()->unknownType, getBuiltins()->neverType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string | never")
{
    CHECK("string" == simplifyStr(arena->addType(UnionType{{getBuiltins()->stringType, getBuiltins()->neverType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string | never | number")
{
    CHECK(
        "number | string" == simplifyStr(arena->addType(UnionType{{getBuiltins()->stringType, getBuiltins()->neverType, getBuiltins()->numberType}}))
    );
}

TEST_CASE_FIXTURE(ESFixture, "string & string")
{
    CHECK("string" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->stringType, getBuiltins()->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string & number")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->stringType, getBuiltins()->numberType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string & unknown")
{
    CHECK("string" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->stringType, getBuiltins()->unknownType}})));
}

TEST_CASE_FIXTURE(ESFixture, "never & string")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->neverType, getBuiltins()->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string & (unknown | never)")
{
    CHECK(
        "string" ==
        simplifyStr(arena->addType(
            IntersectionType{{getBuiltins()->stringType, arena->addType(UnionType{{getBuiltins()->unknownType, getBuiltins()->neverType}})}}
        ))
    );
}

TEST_CASE_FIXTURE(ESFixture, "true | false")
{
    CHECK("boolean" == simplifyStr(arena->addType(UnionType{{getBuiltins()->trueType, getBuiltins()->falseType}})));
}

/*
 * Intuitively, if we have a type like
 *
 * x where x = A & B & (C | D | x)
 *
 * We know that x is certainly not larger than A & B.
 * We also know that the union (C | D | x) can be rewritten `(C | D | (A & B & (C | D | x)))
 * This tells us that the union part is not smaller than A & B.
 * We can therefore discard the union entirely and simplify this type to A & B
 */
TEST_CASE_FIXTURE(ESFixture, "t1 where t1 = string & (number | t1)")
{
    TypeId intersectionTy = arena->addType(BlockedType{});
    TypeId unionTy = arena->addType(UnionType{{getBuiltins()->numberType, intersectionTy}});

    asMutable(intersectionTy)->ty.emplace<IntersectionType>(std::vector<TypeId>{getBuiltins()->stringType, unionTy});

    CHECK("string" == simplifyStr(intersectionTy));
}

TEST_CASE_FIXTURE(ESFixture, "t1 where t1 = string & (unknown | t1)")
{
    TypeId intersectionTy = arena->addType(BlockedType{});
    TypeId unionTy = arena->addType(UnionType{{getBuiltins()->unknownType, intersectionTy}});

    asMutable(intersectionTy)->ty.emplace<IntersectionType>(std::vector<TypeId>{getBuiltins()->stringType, unionTy});

    CHECK("string" == simplifyStr(intersectionTy));
}

TEST_CASE_FIXTURE(ESFixture, "error | unknown")
{
    CHECK("any" == simplifyStr(arena->addType(UnionType{{getBuiltins()->errorType, getBuiltins()->unknownType}})));
}

TEST_CASE_FIXTURE(ESFixture, "\"hello\" | string")
{
    CHECK("string" == simplifyStr(arena->addType(UnionType{{arena->addType(SingletonType{StringSingleton{"hello"}}), getBuiltins()->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "\"hello\" | \"world\" | \"hello\"")
{
    CHECK(
        "\"hello\" | \"world\"" == simplifyStr(arena->addType(
                                       UnionType{{
                                           arena->addType(SingletonType{StringSingleton{"hello"}}),
                                           arena->addType(SingletonType{StringSingleton{"world"}}),
                                           arena->addType(SingletonType{StringSingleton{"hello"}}),
                                       }}
                                   ))
    );
}

TEST_CASE_FIXTURE(ESFixture, "nil | boolean | number | string | thread | function | table | class | buffer")
{
    CHECK(
        "unknown" == simplifyStr(arena->addType(
                         UnionType{{
                             getBuiltins()->nilType,
                             getBuiltins()->booleanType,
                             getBuiltins()->numberType,
                             getBuiltins()->stringType,
                             getBuiltins()->threadType,
                             getBuiltins()->functionType,
                             getBuiltins()->tableType,
                             getBuiltins()->externType,
                             getBuiltins()->bufferType,
                         }}
                     ))
    );
}

TEST_CASE_FIXTURE(ESFixture, "Parent & number")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{parentClass, getBuiltins()->numberType}})));
}

TEST_CASE_FIXTURE(ESFixture, "Child & Parent")
{
    CHECK("Child" == simplifyStr(arena->addType(IntersectionType{{childClass, parentClass}})));
}

TEST_CASE_FIXTURE(ESFixture, "Child & Unrelated")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{childClass, unrelatedClass}})));
}

TEST_CASE_FIXTURE(ESFixture, "Child | Parent")
{
    CHECK("Parent" == simplifyStr(arena->addType(UnionType{{childClass, parentClass}})));
}

TEST_CASE_FIXTURE(ESFixture, "class | Child")
{
    CHECK("class" == simplifyStr(arena->addType(UnionType{{getBuiltins()->externType, childClass}})));
}

TEST_CASE_FIXTURE(ESFixture, "Parent | class | Child")
{
    CHECK("class" == simplifyStr(arena->addType(UnionType{{parentClass, getBuiltins()->externType, childClass}})));
}

TEST_CASE_FIXTURE(ESFixture, "Parent | Unrelated")
{
    CHECK("Parent | Unrelated" == simplifyStr(arena->addType(UnionType{{parentClass, unrelatedClass}})));
}

TEST_CASE_FIXTURE(ESFixture, "never | Parent | Unrelated")
{
    CHECK("Parent | Unrelated" == simplifyStr(arena->addType(UnionType{{getBuiltins()->neverType, parentClass, unrelatedClass}})));
}

TEST_CASE_FIXTURE(ESFixture, "never | Parent | (number & string) | Unrelated")
{
    CHECK(
        "Parent | Unrelated" == simplifyStr(arena->addType(
                                    UnionType{
                                        {getBuiltins()->neverType,
                                         parentClass,
                                         arena->addType(IntersectionType{{getBuiltins()->numberType, getBuiltins()->stringType}}),
                                         unrelatedClass}
                                    }
                                ))
    );
}

TEST_CASE_FIXTURE(ESFixture, "T & U")
{
    CHECK("T & U" == simplifyStr(arena->addType(IntersectionType{{genericT, genericU}})));
}

TEST_CASE_FIXTURE(ESFixture, "boolean & true")
{
    CHECK("true" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->booleanType, getBuiltins()->trueType}})));
}

TEST_CASE_FIXTURE(ESFixture, "boolean & (true | number | string | thread | function | table | class | buffer)")
{
    TypeId truthy = arena->addType(
        UnionType{{
            getBuiltins()->trueType,
            getBuiltins()->numberType,
            getBuiltins()->stringType,
            getBuiltins()->threadType,
            getBuiltins()->functionType,
            getBuiltins()->tableType,
            getBuiltins()->externType,
            getBuiltins()->bufferType,
        }}
    );

    CHECK("true" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->booleanType, truthy}})));
}

TEST_CASE_FIXTURE(ESFixture, "boolean & ~(false?)")
{
    CHECK("true" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->booleanType, getBuiltins()->truthyType}})));
}

TEST_CASE_FIXTURE(ESFixture, "false & ~(false?)")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->falseType, getBuiltins()->truthyType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string & (number) -> string")
{
    CHECK("(number) -> string" == simplifyStr(arena->addType(IntersectionType{{numberToString, numberToString}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string | (number) -> string")
{
    CHECK("(number) -> string" == simplifyStr(arena->addType(UnionType{{numberToString, numberToString}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string & function")
{
    CHECK("(number) -> string" == simplifyStr(arena->addType(IntersectionType{{numberToString, getBuiltins()->functionType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string & boolean")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{numberToString, getBuiltins()->booleanType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string & string")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{numberToString, getBuiltins()->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string & ~function")
{
    TypeId notFunction = arena->addType(NegationType{getBuiltins()->functionType});
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{numberToString, notFunction}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string | function")
{
    CHECK("function" == simplifyStr(arena->addType(UnionType{{numberToString, getBuiltins()->functionType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string & (string) -> number")
{
    CHECK("((number) -> string) & ((string) -> number)" == simplifyStr(arena->addType(IntersectionType{{numberToString, stringToNumber}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string | (string) -> number")
{
    CHECK("((number) -> string) | ((string) -> number)" == simplifyStr(arena->addType(UnionType{{numberToString, stringToNumber}})));
}

TEST_CASE_FIXTURE(ESFixture, "add<number, number>")
{
    CHECK(
        "number" == simplifyStr(arena->addType(
                        TypeFunctionInstanceType{getBuiltinTypeFunctions().addFunc, {getBuiltins()->numberType, getBuiltins()->numberType}}
                    ))
    );
}

TEST_CASE_FIXTURE(ESFixture, "union<number, number>")
{
    CHECK(
        "number" == simplifyStr(arena->addType(
                        TypeFunctionInstanceType{getBuiltinTypeFunctions().unionFunc, {getBuiltins()->numberType, getBuiltins()->numberType}}
                    ))
    );
}

TEST_CASE_FIXTURE(ESFixture, "never & ~string")
{
    CHECK(
        "never" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->neverType, arena->addType(NegationType{getBuiltins()->stringType})}}))
    );
}

TEST_CASE_FIXTURE(ESFixture, "blocked & never")
{
    const TypeId blocked = arena->addType(BlockedType{});

    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{blocked, getBuiltins()->neverType}})));
}

TEST_CASE_FIXTURE(ESFixture, "blocked & ~number & function")
{
    const TypeId blocked = arena->addType(BlockedType{});
    const TypeId notNumber = arena->addType(NegationType{getBuiltins()->numberType});

    const TypeId ty = arena->addType(IntersectionType{{blocked, notNumber, getBuiltins()->functionType}});

    std::string expected = toString(blocked) + " & function";

    CHECK(expected == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "(number | boolean | string | nil | table) & (false | nil)")
{
    const TypeId t1 = arena->addType(
        UnionType{
            {getBuiltins()->numberType, getBuiltins()->booleanType, getBuiltins()->stringType, getBuiltins()->nilType, getBuiltins()->tableType}
        }
    );

    CHECK("false?" == simplifyStr(arena->addType(IntersectionType{{t1, getBuiltins()->falsyType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number | boolean | nil) & (false | nil)")
{
    const TypeId t1 = arena->addType(UnionType{{getBuiltins()->numberType, getBuiltins()->booleanType, getBuiltins()->nilType}});

    CHECK("false?" == simplifyStr(arena->addType(IntersectionType{{t1, getBuiltins()->falsyType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(boolean | nil) & (false | nil)")
{
    const TypeId t1 = arena->addType(UnionType{{getBuiltins()->booleanType, getBuiltins()->nilType}});

    CHECK("false?" == simplifyStr(arena->addType(IntersectionType{{t1, getBuiltins()->falsyType}})));
}

// (('a & false) | ('a & nil)) | number

// Child & ~Parent
// ~Parent & Child
// ~Child & Parent
// Parent & ~Child
// ~Child & ~Parent
// ~Parent & ~Child

TEST_CASE_FIXTURE(ESFixture, "free & string & number")
{
    Scope scope{getBuiltins()->anyTypePack};
    const TypeId freeTy = arena->freshType(getBuiltins(), &scope);

    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{freeTy, getBuiltins()->numberType, getBuiltins()->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(blocked & number) | (blocked & number)")
{
    const TypeId blocked = arena->addType(BlockedType{});
    const TypeId u = arena->addType(IntersectionType{{blocked, getBuiltins()->numberType}});
    const TypeId ty = arena->addType(UnionType{{u, u}});

    const std::string blockedStr = toString(blocked);

    CHECK(blockedStr + " & number" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "{} & unknown")
{
    CHECK("{  }" == simplifyStr(arena->addType(IntersectionType{{tbl({}), getBuiltins()->unknownType}})));
}

TEST_CASE_FIXTURE(ESFixture, "{} & table")
{
    CHECK("{  }" == simplifyStr(arena->addType(IntersectionType{{tbl({}), getBuiltins()->tableType}})));
}

TEST_CASE_FIXTURE(ESFixture, "{} & ~(false?)")
{
    CHECK("{  }" == simplifyStr(arena->addType(IntersectionType{{tbl({}), getBuiltins()->truthyType}})));
}

TEST_CASE_FIXTURE(ESFixture, "{x: number?} & {x: number}")
{
    const TypeId hasOptionalX = tbl({{"x", getBuiltins()->optionalNumberType}});
    const TypeId hasX = tbl({{"x", getBuiltins()->numberType}});

    const TypeId ty = arena->addType(IntersectionType{{hasOptionalX, hasX}});
    auto res = eqSatSimplify(NotNull{simplifier.get()}, ty);

    CHECK("{ x: number }" == toString(res->result));

    // Also assert that we don't allocate a fresh TableType in this case.
    CHECK(follow(res->result) == hasX);
}

TEST_CASE_FIXTURE(ESFixture, "{x: number?} & {x: ~(false?)}")
{
    const TypeId hasOptionalX = tbl({{"x", getBuiltins()->optionalNumberType}});
    const TypeId hasX = tbl({{"x", getBuiltins()->truthyType}});

    const TypeId ty = arena->addType(IntersectionType{{hasOptionalX, hasX}});
    auto res = eqSatSimplify(NotNull{simplifier.get()}, ty);

    CHECK("{ x: number }" == toString(res->result));
}

TEST_CASE_FIXTURE(ESFixture, "(({ x: number? }?) & { x: ~(false?) }")
{
    // {x: number?}?
    const TypeId xWithOptionalNumber = arena->addType(UnionType{{tbl({{"x", getBuiltins()->optionalNumberType}}), getBuiltins()->nilType}});

    // {x: ~(false?)}
    const TypeId xWithTruthy = tbl({{"x", getBuiltins()->truthyType}});

    const TypeId ty = arena->addType(IntersectionType{{xWithOptionalNumber, xWithTruthy}});

    CHECK("{ x: number }" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "never | (({ x: number? }?) & { x: ~(false?) })")
{
    // {x: number?}?
    const TypeId xWithOptionalNumber = arena->addType(UnionType{{tbl({{"x", getBuiltins()->optionalNumberType}}), getBuiltins()->nilType}});

    // {x: ~(false?)}
    const TypeId xWithTruthy = tbl({{"x", getBuiltins()->truthyType}});

    // ({x: number?}?) & {x: ~(false?)}
    const TypeId intersectionTy = arena->addType(IntersectionType{{xWithOptionalNumber, xWithTruthy}});

    const TypeId ty = arena->addType(UnionType{{getBuiltins()->neverType, intersectionTy}});

    CHECK("{ x: number }" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "({ x: number? }?) & { x: ~(false?) } & ~(false?)")
{
    // {x: number?}?
    const TypeId xWithOptionalNumber = arena->addType(UnionType{{tbl({{"x", getBuiltins()->optionalNumberType}}), getBuiltins()->nilType}});

    // {x: ~(false?)}
    const TypeId xWithTruthy = tbl({{"x", getBuiltins()->truthyType}});

    // ({x: number?}?) & {x: ~(false?)} & ~(false?)
    const TypeId intersectionTy = arena->addType(IntersectionType{{xWithOptionalNumber, xWithTruthy, getBuiltins()->truthyType}});

    CHECK("{ x: number }" == simplifyStr(intersectionTy));
}

#if 0
// TODO
TEST_CASE_FIXTURE(ESFixture, "(({ x: number? }?) & { x: ~(false?) } & ~(false?)) | number")
{
    // ({ x: number? }?) & { x: ~(false?) } & ~(false?)
    const TypeId xWithOptionalNumber = tbl({{"x", getBuiltins()->optionalNumberType}});
    const TypeId xWithTruthy = tbl({{"x", getBuiltins()->truthyType}});
    const TypeId intersectionTy = arena->addType(IntersectionType{{xWithOptionalNumber, xWithTruthy, getBuiltins()->truthyType}});
    const TypeId ty = arena->addType(UnionType{{intersectionTy, getBuiltins()->numberType}});

    CHECK("{ x: number } | number" == simplifyStr(ty));
}
#endif

TEST_CASE_FIXTURE(ESFixture, "number & no-refine")
{
    CHECK("number" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->numberType, getBuiltins()->noRefineType}})));
}

TEST_CASE_FIXTURE(ESFixture, "{ x: number } & ~boolean")
{
    const TypeId tblTy = tbl(TableType::Props{{"x", getBuiltins()->numberType}});

    const TypeId ty = arena->addType(IntersectionType{{tblTy, arena->addType(NegationType{getBuiltins()->booleanType})}});

    CHECK("{ x: number }" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "(nil & string)?")
{
    const TypeId nilAndString = arena->addType(IntersectionType{{getBuiltins()->nilType, getBuiltins()->stringType}});
    const TypeId ty = arena->addType(UnionType{{nilAndString, getBuiltins()->nilType}});

    CHECK("nil" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "string & \"hi\"")
{
    const TypeId hi = arena->addType(SingletonType{StringSingleton{"hi"}});

    CHECK("\"hi\"" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->stringType, hi}})));
}

TEST_CASE_FIXTURE(ESFixture, "string & (\"hi\" | \"bye\")")
{
    const TypeId hi = arena->addType(SingletonType{StringSingleton{"hi"}});
    const TypeId bye = arena->addType(SingletonType{StringSingleton{"bye"}});

    CHECK("\"bye\" | \"hi\"" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->stringType, arena->addType(UnionType{{hi, bye}})}})));
}

TEST_CASE_FIXTURE(ESFixture, "(\"err\" | \"ok\") & ~\"ok\"")
{
    TypeId err = arena->addType(SingletonType{StringSingleton{"err"}});
    TypeId ok1 = arena->addType(SingletonType{StringSingleton{"ok"}});
    TypeId ok2 = arena->addType(SingletonType{StringSingleton{"ok"}});

    TypeId ty = arena->addType(IntersectionType{{arena->addType(UnionType{{err, ok1}}), arena->addType(NegationType{ok2})}});

    CHECK("\"err\"" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "(Child | Unrelated) & ~Child")
{
    const TypeId ty =
        arena->addType(IntersectionType{{arena->addType(UnionType{{childClass, unrelatedClass}}), arena->addType(NegationType{childClass})}});

    CHECK("Unrelated" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "string & ~Child")
{
    CHECK("string" == simplifyStr(arena->addType(IntersectionType{{getBuiltins()->stringType, arena->addType(NegationType{childClass})}})));
}

TEST_CASE_FIXTURE(ESFixture, "(Child | Unrelated) & Child")
{
    CHECK("Child" == simplifyStr(arena->addType(IntersectionType{{arena->addType(UnionType{{childClass, unrelatedClass}}), childClass}})));
}

TEST_CASE_FIXTURE(ESFixture, "(Child | AnotherChild) & ~Child")
{
    CHECK("Child" == simplifyStr(arena->addType(IntersectionType{{arena->addType(UnionType{{childClass, anotherChild}}), childClass}})));
}

TEST_CASE_FIXTURE(ESFixture, "{ tag: \"Part\", x: never }")
{
    const TypeId ty = tbl({{"tag", arena->addType(SingletonType{StringSingleton{"Part"}})}, {"x", getBuiltins()->neverType}});

    CHECK("never" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "{ tag: \"Part\", x: number? } & { x: string }")
{
    const TypeId leftTable = tbl({{"tag", arena->addType(SingletonType{StringSingleton{"Part"}})}, {"x", getBuiltins()->optionalNumberType}});
    const TypeId rightTable = tbl({{"x", getBuiltins()->stringType}});

    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{leftTable, rightTable}})));
}

TEST_CASE_FIXTURE(ESFixture, "Child & add<Child | AnotherChild | string, Parent>")
{
    const TypeId u = arena->addType(UnionType{{childClass, anotherChild, getBuiltins()->stringType}});
    const TypeId intersectTf = arena->addType(TypeFunctionInstanceType{getBuiltinTypeFunctions().addFunc, {u, parentClass}, {}});

    const TypeId intersection = arena->addType(IntersectionType{{childClass, intersectTf}});

    CHECK("Child & add<AnotherChild | Child | string, Parent>" == simplifyStr(intersection));
}

TEST_CASE_FIXTURE(ESFixture, "Child & intersect<Child | AnotherChild | string, Parent>")
{
    const TypeId u = arena->addType(UnionType{{childClass, anotherChild, getBuiltins()->stringType}});
    const TypeId intersectTf = arena->addType(TypeFunctionInstanceType{getBuiltinTypeFunctions().intersectFunc, {u, parentClass}, {}});

    const TypeId intersection = arena->addType(IntersectionType{{childClass, intersectTf}});

    CHECK("Child" == simplifyStr(intersection));
}

TEST_CASE_FIXTURE(ESFixture, "lt<number, _> == boolean")
{
    std::vector<std::pair<TypeId, TypeId>> cases{
        {getBuiltins()->numberType, arena->addType(BlockedType{})},
        {getBuiltins()->stringType, arena->addType(BlockedType{})},
        {arena->addType(BlockedType{}), getBuiltins()->numberType},
        {arena->addType(BlockedType{}), getBuiltins()->stringType},
    };

    for (const auto& [lhs, rhs] : cases)
    {
        const TypeId tfun = arena->addType(TypeFunctionInstanceType{getBuiltinTypeFunctions().ltFunc, {lhs, rhs}});
        CHECK("boolean" == simplifyStr(tfun));
    }
}

TEST_CASE_FIXTURE(ESFixture, "unknown & ~string")
{
    CHECK_EQ(
        "~string",
        simplifyStr(arena->addType(IntersectionType{{getBuiltins()->unknownType, arena->addType(NegationType{getBuiltins()->stringType})}}))
    );
}

TEST_CASE_FIXTURE(ESFixture, "string & ~\"foo\"")
{
    CHECK_EQ(
        "string & ~\"foo\"",
        simplifyStr(arena->addType(
            IntersectionType{{getBuiltins()->stringType, arena->addType(NegationType{arena->addType(SingletonType{StringSingleton{"foo"}})})}}
        ))
    );
}

// {someKey: ~any}
//
// Maybe something we could do here is to try to reduce the key, get the
// class->node mapping, and skip the extraction process if the class corresponds
// to TNever.

// t1 where t1 = add<union<number, t1>, number>

TEST_SUITE_END();
