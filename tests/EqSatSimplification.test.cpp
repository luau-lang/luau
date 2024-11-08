// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/EqSatSimplification.h"

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

    TypeId numberToString = arena_.addType(FunctionType{
        arena_.addTypePack({builtinTypes->numberType}),
        arena_.addTypePack({builtinTypes->stringType})
    });

    TypeId stringToNumber = arena_.addType(FunctionType{
        arena_.addTypePack({builtinTypes->stringType}),
        arena_.addTypePack({builtinTypes->numberType})
    });

    ESFixture()
        : simplifier(newSimplifier(arena, builtinTypes))
    {
        createSomeClasses(&frontend);

        ScopePtr moduleScope = frontend.globals.globalScope;

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
    CHECK("number" == simplifyStr(builtinTypes->numberType));
}

TEST_CASE_FIXTURE(ESFixture, "number | number")
{
    TypeId ty = arena->addType(UnionType{{builtinTypes->numberType, builtinTypes->numberType}});

    CHECK("number" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "number | string")
{
    CHECK("number | string" == simplifyStr(arena->addType(UnionType{{builtinTypes->numberType, builtinTypes->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "t1 where t1 = number | t1")
{
    TypeId ty = arena->freshType(nullptr);
    asMutable(ty)->ty.emplace<UnionType>(std::vector<TypeId>{builtinTypes->numberType, ty});

    CHECK("number" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "number | string | number")
{
    TypeId ty = arena->addType(UnionType{{builtinTypes->numberType, builtinTypes->stringType, builtinTypes->numberType}});

    CHECK("number | string" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "string | (number | string) | number")
{
    TypeId u1 = arena->addType(UnionType{{builtinTypes->numberType, builtinTypes->stringType}});
    TypeId u2 = arena->addType(UnionType{{builtinTypes->stringType, u1, builtinTypes->numberType}});

    CHECK("number | string" == simplifyStr(u2));
}

TEST_CASE_FIXTURE(ESFixture, "string | any")
{
    CHECK("any" == simplifyStr(arena->addType(UnionType{{builtinTypes->stringType, builtinTypes->anyType}})));
}

TEST_CASE_FIXTURE(ESFixture, "any | string")
{
    CHECK("any" == simplifyStr(arena->addType(UnionType{{builtinTypes->anyType, builtinTypes->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "any | never")
{
    CHECK("any" == simplifyStr(arena->addType(UnionType{{builtinTypes->anyType, builtinTypes->neverType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string | unknown")
{
    CHECK("unknown" == simplifyStr(arena->addType(UnionType{{builtinTypes->stringType, builtinTypes->unknownType}})));
}

TEST_CASE_FIXTURE(ESFixture, "unknown | string")
{
    CHECK("unknown" == simplifyStr(arena->addType(UnionType{{builtinTypes->unknownType, builtinTypes->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "unknown | never")
{
    CHECK("unknown" == simplifyStr(arena->addType(UnionType{{builtinTypes->unknownType, builtinTypes->neverType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string | never")
{
    CHECK("string" == simplifyStr(arena->addType(UnionType{{builtinTypes->stringType, builtinTypes->neverType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string | never | number")
{
    CHECK("number | string" == simplifyStr(arena->addType(UnionType{{builtinTypes->stringType, builtinTypes->neverType, builtinTypes->numberType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string & string")
{
    CHECK("string" == simplifyStr(arena->addType(IntersectionType{{builtinTypes->stringType, builtinTypes->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string & number")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{builtinTypes->stringType, builtinTypes->numberType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string & unknown")
{
    CHECK("string" == simplifyStr(arena->addType(IntersectionType{{builtinTypes->stringType, builtinTypes->unknownType}})));
}

TEST_CASE_FIXTURE(ESFixture, "never & string")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{builtinTypes->neverType, builtinTypes->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "string & (unknown | never)")
{
    CHECK("string" == simplifyStr(arena->addType(IntersectionType{{
        builtinTypes->stringType,
        arena->addType(UnionType{{builtinTypes->unknownType, builtinTypes->neverType}})
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "true | false")
{
    CHECK("boolean" == simplifyStr(arena->addType(UnionType{{builtinTypes->trueType, builtinTypes->falseType}})));
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
    TypeId unionTy = arena->addType(UnionType{{builtinTypes->numberType, intersectionTy}});

    asMutable(intersectionTy)->ty.emplace<IntersectionType>(std::vector<TypeId>{builtinTypes->stringType, unionTy});

    CHECK("string" == simplifyStr(intersectionTy));
}

TEST_CASE_FIXTURE(ESFixture, "t1 where t1 = string & (unknown | t1)")
{
    TypeId intersectionTy = arena->addType(BlockedType{});
    TypeId unionTy = arena->addType(UnionType{{builtinTypes->unknownType, intersectionTy}});

    asMutable(intersectionTy)->ty.emplace<IntersectionType>(std::vector<TypeId>{builtinTypes->stringType, unionTy});

    CHECK("string" == simplifyStr(intersectionTy));
}

TEST_CASE_FIXTURE(ESFixture, "error | unknown")
{
    CHECK("any" == simplifyStr(arena->addType(UnionType{{builtinTypes->errorType, builtinTypes->unknownType}})));
}

TEST_CASE_FIXTURE(ESFixture, "\"hello\" | string")
{
    CHECK("string" == simplifyStr(arena->addType(UnionType{{
        arena->addType(SingletonType{StringSingleton{"hello"}}), builtinTypes->stringType
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "\"hello\" | \"world\" | \"hello\"")
{
    CHECK("\"hello\" | \"world\"" == simplifyStr(arena->addType(UnionType{{
        arena->addType(SingletonType{StringSingleton{"hello"}}),
        arena->addType(SingletonType{StringSingleton{"world"}}),
        arena->addType(SingletonType{StringSingleton{"hello"}}),
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "nil | boolean | number | string | thread | function | table | class | buffer")
{
    CHECK("unknown" == simplifyStr(arena->addType(UnionType{{
        builtinTypes->nilType,
        builtinTypes->booleanType,
        builtinTypes->numberType,
        builtinTypes->stringType,
        builtinTypes->threadType,
        builtinTypes->functionType,
        builtinTypes->tableType,
        builtinTypes->classType,
        builtinTypes->bufferType,
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "Parent & number")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{
        parentClass, builtinTypes->numberType
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "Child & Parent")
{
    CHECK("Child" == simplifyStr(arena->addType(IntersectionType{{
        childClass, parentClass
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "Child & Unrelated")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{
        childClass, unrelatedClass
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "Child | Parent")
{
    CHECK("Parent" == simplifyStr(arena->addType(UnionType{{
        childClass, parentClass
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "class | Child")
{
    CHECK("class" == simplifyStr(arena->addType(UnionType{{
        builtinTypes->classType, childClass
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "Parent | class | Child")
{
    CHECK("class" == simplifyStr(arena->addType(UnionType{{
        parentClass, builtinTypes->classType, childClass
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "Parent | Unrelated")
{
    CHECK("Parent | Unrelated" == simplifyStr(arena->addType(UnionType{{
        parentClass, unrelatedClass
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "never | Parent | Unrelated")
{
    CHECK("Parent | Unrelated" == simplifyStr(arena->addType(UnionType{{
        builtinTypes->neverType, parentClass, unrelatedClass
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "never | Parent | (number & string) | Unrelated")
{
    CHECK("Parent | Unrelated" == simplifyStr(arena->addType(UnionType{{
        builtinTypes->neverType, parentClass,
        arena->addType(IntersectionType{{builtinTypes->numberType, builtinTypes->stringType}}),
        unrelatedClass
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "T & U")
{
    CHECK("T & U" == simplifyStr(arena->addType(IntersectionType{{
        genericT, genericU
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "boolean & true")
{
    CHECK("true" == simplifyStr(arena->addType(IntersectionType{{
        builtinTypes->booleanType, builtinTypes->trueType
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "boolean & (true | number | string | thread | function | table | class | buffer)")
{
    TypeId truthy = arena->addType(UnionType{{
        builtinTypes->trueType,
        builtinTypes->numberType,
        builtinTypes->stringType,
        builtinTypes->threadType,
        builtinTypes->functionType,
        builtinTypes->tableType,
        builtinTypes->classType,
        builtinTypes->bufferType,
    }});

    CHECK("true" == simplifyStr(arena->addType(IntersectionType{{
        builtinTypes->booleanType, truthy
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "boolean & ~(false?)")
{
    CHECK("true" == simplifyStr(arena->addType(IntersectionType{{
        builtinTypes->booleanType, builtinTypes->truthyType
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "false & ~(false?)")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{
        builtinTypes->falseType, builtinTypes->truthyType
    }})));
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
    CHECK("(number) -> string" == simplifyStr(arena->addType(IntersectionType{{numberToString, builtinTypes->functionType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string & boolean")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{numberToString, builtinTypes->booleanType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string & string")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{numberToString, builtinTypes->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string & ~function")
{
    TypeId notFunction = arena->addType(NegationType{builtinTypes->functionType});
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{numberToString, notFunction}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number) -> string | function")
{
    CHECK("function" == simplifyStr(arena->addType(UnionType{{numberToString, builtinTypes->functionType}})));
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
    CHECK("number" == simplifyStr(arena->addType(
        TypeFunctionInstanceType{builtinTypeFunctions().addFunc, {
            builtinTypes->numberType, builtinTypes->numberType
        }}
    )));
}

TEST_CASE_FIXTURE(ESFixture, "union<number, number>")
{
    CHECK("number" == simplifyStr(arena->addType(
        TypeFunctionInstanceType{builtinTypeFunctions().unionFunc, {
            builtinTypes->numberType, builtinTypes->numberType
        }}
    )));
}

TEST_CASE_FIXTURE(ESFixture, "never & ~string")
{
    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{
        builtinTypes->neverType,
        arena->addType(NegationType{builtinTypes->stringType})
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "blocked & never")
{
    const TypeId blocked = arena->addType(BlockedType{});

    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{blocked, builtinTypes->neverType}})));
}

TEST_CASE_FIXTURE(ESFixture, "blocked & ~number & function")
{
    const TypeId blocked = arena->addType(BlockedType{});
    const TypeId notNumber = arena->addType(NegationType{builtinTypes->numberType});

    const TypeId ty = arena->addType(IntersectionType{{blocked, notNumber, builtinTypes->functionType}});

    std::string expected = toString(blocked) + " & function";

    CHECK(expected == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "(number | boolean | string | nil | table) & (false | nil)")
{
    const TypeId t1 = arena->addType(UnionType{{builtinTypes->numberType, builtinTypes->booleanType, builtinTypes->stringType, builtinTypes->nilType, builtinTypes->tableType}});

    CHECK("false?" == simplifyStr(arena->addType(IntersectionType{{t1, builtinTypes->falsyType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(number | boolean | nil) & (false | nil)")
{
    const TypeId t1 = arena->addType(UnionType{{builtinTypes->numberType, builtinTypes->booleanType, builtinTypes->nilType}});

    CHECK("false?" == simplifyStr(arena->addType(IntersectionType{{t1, builtinTypes->falsyType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(boolean | nil) & (false | nil)")
{
    const TypeId t1 = arena->addType(UnionType{{builtinTypes->booleanType, builtinTypes->nilType}});

    CHECK("false?" == simplifyStr(arena->addType(IntersectionType{{t1, builtinTypes->falsyType}})));
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
    Scope scope{builtinTypes->anyTypePack};
    const TypeId freeTy = arena->addType(FreeType{&scope});

    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{freeTy, builtinTypes->numberType, builtinTypes->stringType}})));
}

TEST_CASE_FIXTURE(ESFixture, "(blocked & number) | (blocked & number)")
{
    const TypeId blocked = arena->addType(BlockedType{});
    const TypeId u = arena->addType(IntersectionType{{blocked, builtinTypes->numberType}});
    const TypeId ty = arena->addType(UnionType{{u, u}});

    const std::string blockedStr = toString(blocked);

    CHECK(blockedStr + " & number" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "{} & unknown")
{
    CHECK("{  }" == simplifyStr(arena->addType(IntersectionType{{
        tbl({}),
        builtinTypes->unknownType
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "{} & table")
{
    CHECK("{  }" == simplifyStr(arena->addType(IntersectionType{{
        tbl({}),
        builtinTypes->tableType
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "{} & ~(false?)")
{
    CHECK("{  }" == simplifyStr(arena->addType(IntersectionType{{
        tbl({}),
        builtinTypes->truthyType
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "{x: number?} & {x: number}")
{
    const TypeId hasOptionalX = tbl({{"x", builtinTypes->optionalNumberType}});
    const TypeId hasX = tbl({{"x", builtinTypes->numberType}});

    const TypeId ty = arena->addType(IntersectionType{{hasOptionalX, hasX}});
    auto res = eqSatSimplify(NotNull{simplifier.get()}, ty);

    CHECK("{ x: number }" == toString(res->result));

    // Also assert that we don't allocate a fresh TableType in this case.
    CHECK(follow(res->result) == hasX);
}

TEST_CASE_FIXTURE(ESFixture, "{x: number?} & {x: ~(false?)}")
{
    const TypeId hasOptionalX = tbl({{"x", builtinTypes->optionalNumberType}});
    const TypeId hasX = tbl({{"x", builtinTypes->truthyType}});

    const TypeId ty = arena->addType(IntersectionType{{hasOptionalX, hasX}});
    auto res = eqSatSimplify(NotNull{simplifier.get()}, ty);

    CHECK("{ x: number }" == toString(res->result));
}

TEST_CASE_FIXTURE(ESFixture, "(({ x: number? }?) & { x: ~(false?) }")
{
    // {x: number?}?
    const TypeId xWithOptionalNumber = arena->addType(UnionType{{tbl({{"x", builtinTypes->optionalNumberType}}), builtinTypes->nilType}});

    // {x: ~(false?)}
    const TypeId xWithTruthy = tbl({{"x", builtinTypes->truthyType}});

    const TypeId ty = arena->addType(IntersectionType{{xWithOptionalNumber, xWithTruthy}});

    CHECK("{ x: number }" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "never | (({ x: number? }?) & { x: ~(false?) })")
{
    // {x: number?}?
    const TypeId xWithOptionalNumber = arena->addType(UnionType{{tbl({{"x", builtinTypes->optionalNumberType}}), builtinTypes->nilType}});

    // {x: ~(false?)}
    const TypeId xWithTruthy = tbl({{"x", builtinTypes->truthyType}});

    // ({x: number?}?) & {x: ~(false?)}
    const TypeId intersectionTy = arena->addType(IntersectionType{{xWithOptionalNumber, xWithTruthy}});

    const TypeId ty = arena->addType(UnionType{{builtinTypes->neverType, intersectionTy}});

    CHECK("{ x: number }" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "({ x: number? }?) & { x: ~(false?) } & ~(false?)")
{
    // {x: number?}?
    const TypeId xWithOptionalNumber = arena->addType(UnionType{{tbl({{"x", builtinTypes->optionalNumberType}}), builtinTypes->nilType}});

    // {x: ~(false?)}
    const TypeId xWithTruthy = tbl({{"x", builtinTypes->truthyType}});

    // ({x: number?}?) & {x: ~(false?)} & ~(false?)
    const TypeId intersectionTy = arena->addType(IntersectionType{{xWithOptionalNumber, xWithTruthy, builtinTypes->truthyType}});

    CHECK("{ x: number }" == simplifyStr(intersectionTy));
}

#if 0
// TODO
TEST_CASE_FIXTURE(ESFixture, "(({ x: number? }?) & { x: ~(false?) } & ~(false?)) | number")
{
    // ({ x: number? }?) & { x: ~(false?) } & ~(false?)
    const TypeId xWithOptionalNumber = tbl({{"x", builtinTypes->optionalNumberType}});
    const TypeId xWithTruthy = tbl({{"x", builtinTypes->truthyType}});
    const TypeId intersectionTy = arena->addType(IntersectionType{{xWithOptionalNumber, xWithTruthy, builtinTypes->truthyType}});
    const TypeId ty = arena->addType(UnionType{{intersectionTy, builtinTypes->numberType}});

    CHECK("{ x: number } | number" == simplifyStr(ty));
}
#endif

TEST_CASE_FIXTURE(ESFixture, "number & no-refine")
{
    CHECK("number" == simplifyStr(arena->addType(IntersectionType{{builtinTypes->numberType, builtinTypes->noRefineType}})));
}

TEST_CASE_FIXTURE(ESFixture, "{ x: number } & ~boolean")
{
    const TypeId tblTy = tbl(TableType::Props{{"x", builtinTypes->numberType}});

    const TypeId ty = arena->addType(IntersectionType{{
        tblTy,
        arena->addType(NegationType{builtinTypes->booleanType})
    }});

    CHECK("{ x: number }" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "(nil & string)?")
{
    const TypeId nilAndString = arena->addType(IntersectionType{{builtinTypes->nilType, builtinTypes->stringType}});
    const TypeId ty = arena->addType(UnionType{{nilAndString, builtinTypes->nilType}});

    CHECK("nil" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "string & \"hi\"")
{
    const TypeId hi = arena->addType(SingletonType{StringSingleton{"hi"}});

    CHECK("\"hi\"" == simplifyStr(arena->addType(IntersectionType{{builtinTypes->stringType, hi}})));
}

TEST_CASE_FIXTURE(ESFixture, "string & (\"hi\" | \"bye\")")
{
    const TypeId hi = arena->addType(SingletonType{StringSingleton{"hi"}});
    const TypeId bye = arena->addType(SingletonType{StringSingleton{"bye"}});

    CHECK("\"bye\" | \"hi\"" == simplifyStr(arena->addType(IntersectionType{{
        builtinTypes->stringType,
        arena->addType(UnionType{{hi, bye}})
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "(Child | Unrelated) & ~Child")
{
    const TypeId ty = arena->addType(IntersectionType{{
        arena->addType(UnionType{{childClass, unrelatedClass}}),
        arena->addType(NegationType{childClass})
    }});

    CHECK("Unrelated" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "string & ~Child")
{
    CHECK("string" == simplifyStr(arena->addType(IntersectionType{{
        builtinTypes->stringType,
        arena->addType(NegationType{childClass})
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "(Child | Unrelated) & Child")
{
    CHECK("Child" == simplifyStr(arena->addType(IntersectionType{{
        arena->addType(UnionType{{childClass, unrelatedClass}}),
        childClass
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "(Child | AnotherChild) & ~Child")
{
    CHECK("Child" == simplifyStr(arena->addType(IntersectionType{{
        arena->addType(UnionType{{childClass, anotherChild}}),
        childClass
    }})));
}

TEST_CASE_FIXTURE(ESFixture, "{ tag: \"Part\", x: never }")
{
    const TypeId ty = tbl({{"tag", arena->addType(SingletonType{StringSingleton{"Part"}})}, {"x", builtinTypes->neverType}});

    CHECK("never" == simplifyStr(ty));
}

TEST_CASE_FIXTURE(ESFixture, "{ tag: \"Part\", x: number? } & { x: string }")
{
    const TypeId leftTable = tbl({{"tag", arena->addType(SingletonType{StringSingleton{"Part"}})}, {"x", builtinTypes->optionalNumberType}});
    const TypeId rightTable = tbl({{"x", builtinTypes->stringType}});

    CHECK("never" == simplifyStr(arena->addType(IntersectionType{{leftTable, rightTable}})));
}

TEST_CASE_FIXTURE(ESFixture, "Child & add<Child | AnotherChild | string, Parent>")
{
    const TypeId u = arena->addType(UnionType{{childClass, anotherChild, builtinTypes->stringType}});
    const TypeId intersectTf = arena->addType(TypeFunctionInstanceType{
        builtinTypeFunctions().addFunc,
        {u, parentClass},
        {}
    });

    const TypeId intersection = arena->addType(IntersectionType{{childClass, intersectTf}});

    CHECK("Child & add<AnotherChild | Child | string, Parent>" == simplifyStr(intersection));
}

TEST_CASE_FIXTURE(ESFixture, "Child & intersect<Child | AnotherChild | string, Parent>")
{
    const TypeId u = arena->addType(UnionType{{childClass, anotherChild, builtinTypes->stringType}});
    const TypeId intersectTf = arena->addType(TypeFunctionInstanceType{
        builtinTypeFunctions().intersectFunc,
        {u, parentClass},
        {}
    });

    const TypeId intersection = arena->addType(IntersectionType{{childClass, intersectTf}});

    CHECK("Child" == simplifyStr(intersection));
}

// {someKey: ~any}
//
// Maybe something we could do here is to try to reduce the key, get the
// class->node mapping, and skip the extraction process if the class corresponds
// to TNever.

// t1 where t1 = add<union<number, t1>, number>

TEST_SUITE_END();
