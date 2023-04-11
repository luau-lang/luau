// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/Frontend.h"
#include "Luau/ToString.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "doctest.h"

#include <algorithm>

using namespace Luau;

LUAU_FASTFLAG(LuauLowerBoundsCalculation);
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAG(LuauInstantiateInSubtyping)
LUAU_FASTFLAG(LuauTypeMismatchInvarianceInError)

TEST_SUITE_BEGIN("TableTests");

TEST_CASE_FIXTURE(Fixture, "basic")
{
    CheckResult result = check("local t = {foo = \"bar\", baz = 9, quux = nil}");
    LUAU_REQUIRE_NO_ERRORS(result);

    const TableType* tType = get<TableType>(requireType("t"));
    REQUIRE(tType != nullptr);

    std::optional<Property> fooProp = get(tType->props, "foo");
    REQUIRE(bool(fooProp));
    CHECK_EQ(PrimitiveType::String, getPrimitiveType(fooProp->type));

    std::optional<Property> bazProp = get(tType->props, "baz");
    REQUIRE(bool(bazProp));
    CHECK_EQ(PrimitiveType::Number, getPrimitiveType(bazProp->type));

    std::optional<Property> quuxProp = get(tType->props, "quux");
    REQUIRE(bool(quuxProp));
    CHECK_EQ(PrimitiveType::NilType, getPrimitiveType(quuxProp->type));
}

TEST_CASE_FIXTURE(Fixture, "augment_table")
{
    CheckResult result = check("local t = {}  t.foo = 'bar'");
    LUAU_REQUIRE_NO_ERRORS(result);

    const TableType* tType = get<TableType>(requireType("t"));
    REQUIRE(tType != nullptr);

    CHECK("{ foo: string }" == toString(requireType("t"), {true}));
}

TEST_CASE_FIXTURE(Fixture, "augment_nested_table")
{
    CheckResult result = check(R"(
        local t = { p = {} }
        t.p.foo = 'bar'
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    TableType* tType = getMutable<TableType>(requireType("t"));
    REQUIRE(tType != nullptr);

    REQUIRE(tType->props.find("p") != tType->props.end());
    const TableType* pType = get<TableType>(tType->props["p"].type);
    REQUIRE(pType != nullptr);

    CHECK("{ p: { foo: string } }" == toString(requireType("t"), {true}));
}

TEST_CASE_FIXTURE(Fixture, "cannot_augment_sealed_table")
{
    CheckResult result = check(R"(
        function mkt()
            return {prop=999}
        end

        local t = mkt()
        t.foo = 'bar'
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeError& err = result.errors[0];

    CHECK(err.location == Location{Position{6, 8}, Position{6, 13}});

    CannotExtendTable* error = get<CannotExtendTable>(err);
    REQUIRE_MESSAGE(error != nullptr, "Expected CannotExtendTable but got: " << toString(err));

    // TODO: better, more robust comparison of type vars
    auto s = toString(error->tableType, ToStringOptions{/*exhaustive*/ true});
    CHECK_EQ(s, "{| prop: number |}");
    CHECK_EQ(error->prop, "foo");
    CHECK_EQ(error->context, CannotExtendTable::Property);
}

TEST_CASE_FIXTURE(Fixture, "dont_seal_an_unsealed_table_by_passing_it_to_a_function_that_takes_a_sealed_table")
{
    CheckResult result = check(R"(
        type T = {[number]: number}
        function f(arg: T) end

        local B = {}
        f(B)
        function B:method() end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "updating_sealed_table_prop_is_ok")
{
    CheckResult result = check("local t = {prop=999}    t.prop = 0");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "cannot_change_type_of_unsealed_table_prop")
{
    CheckResult result = check(R"(
        local t = {}
        t.prop = 999
        t.prop = 'hello'
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "cannot_change_type_of_table_prop")
{
    CheckResult result = check("local t = {prop=999}   t.prop = 'hello'");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "function_calls_can_produce_tables")
{
    CheckResult result = check("function get_table() return {prop=999} end    get_table().prop = 0");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "function_calls_produces_sealed_table_given_unsealed_table")
{
    CheckResult result = check(R"(
        function f() return {} end
        f().foo = 'fail'
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "tc_member_function")
{
    CheckResult result = check("local T = {}  function T:foo() return 5 end");
    LUAU_REQUIRE_NO_ERRORS(result);

    const TableType* tableType = get<TableType>(requireType("T"));
    REQUIRE(tableType != nullptr);

    std::optional<Property> fooProp = get(tableType->props, "foo");
    REQUIRE(bool(fooProp));

    const FunctionType* methodType = get<FunctionType>(follow(fooProp->type));
    REQUIRE(methodType != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "tc_member_function_2")
{
    CheckResult result = check("local T = {U={}}  function T.U:foo() return 5 end");
    LUAU_REQUIRE_NO_ERRORS(result);

    const TableType* tableType = get<TableType>(requireType("T"));
    REQUIRE(tableType != nullptr);

    std::optional<Property> uProp = get(tableType->props, "U");
    REQUIRE(bool(uProp));
    TypeId uType = uProp->type;

    const TableType* uTable = get<TableType>(uType);
    REQUIRE(uTable != nullptr);

    std::optional<Property> fooProp = get(uTable->props, "foo");
    REQUIRE(bool(fooProp));

    const FunctionType* methodType = get<FunctionType>(follow(fooProp->type));
    REQUIRE(methodType != nullptr);

    std::vector<TypeId> methodArgs = flatten(methodType->argTypes).first;

    REQUIRE_EQ(methodArgs.size(), 1);

    // TODO(rblanckaert): Revist when we can bind self at function creation time
    // REQUIRE_EQ(*methodArgs[0], *uType);
}

TEST_CASE_FIXTURE(Fixture, "call_method")
{
    CheckResult result = check("local T = {}    T.x = 0    function T:method() return self.x end    local a = T:method()");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*builtinTypes->numberType, *requireType("a"));
}

TEST_CASE_FIXTURE(Fixture, "call_method_with_explicit_self_argument")
{
    CheckResult result = check("local T = {}    T.x = 0    function T:method() return self.x end    local a = T.method(T)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "used_dot_instead_of_colon")
{
    CheckResult result = check(R"(
        local T = {}
        T.x = 0
        function T:method()
            return self.x
        end
        local a = T.method()
    )");

    auto it = std::find_if(result.errors.begin(), result.errors.end(), [](const TypeError& e) {
        return nullptr != get<FunctionRequiresSelf>(e);
    });
    REQUIRE(it != result.errors.end());
}

TEST_CASE_FIXTURE(BuiltinsFixture, "used_colon_correctly")
{
    CheckResult result = check(R"(
        --!nonstrict
        local upVector = {}
        function upVector:Dot(lookVector)
            return 8
        end
        local v = math.abs(upVector:Dot(5))
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "used_dot_instead_of_colon_but_correctly")
{
    CheckResult result = check(R"(
        local T = {}
        T.x = 0
        function T:method(arg1, arg2)
            return self.x
        end
        local a = T.method(T, 6, 7)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "used_colon_instead_of_dot")
{
    CheckResult result = check(R"(
        local T = {}
        T.x = 0
        function T.method()
            return 5
        end
        local a = T:method()
    )");

    auto it = std::find_if(result.errors.begin(), result.errors.end(), [](const TypeError& e) {
        return nullptr != get<FunctionDoesNotTakeSelf>(e);
    });
    REQUIRE(it != result.errors.end());
}

#if 0
TEST_CASE_FIXTURE(Fixture, "open_table_unification")
{
    CheckResult result = check(R"(
        function foo(o)
            print(o.foo)
            print(o.bar)
        end

        local a = {}
        a.foo = 9

        local b = {}
        b.foo = 0

        if random() then
            b = a
        end

        b.bar = '99'

        foo(a)
        foo(b)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}
#endif

TEST_CASE_FIXTURE(Fixture, "open_table_unification_2")
{
    CheckResult result = check(R"(
        local a = {}
        a.x = 99

        function a:method()
            return self.y
        end
        a:method()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeError& err = result.errors[0];
    MissingProperties* error = get<MissingProperties>(err);
    REQUIRE(error != nullptr);
    REQUIRE(error->properties.size() == 1);

    CHECK_EQ("y", error->properties[0]);
    // TODO(rblanckaert): Revist when we can bind self at function creation time
    // CHECK_EQ(err.location, Location(Position{5, 19}, Position{5, 25}));

    CHECK_EQ(err.location, Location(Position{7, 8}, Position{7, 9}));
}

TEST_CASE_FIXTURE(Fixture, "open_table_unification_3")
{
    CheckResult result = check(R"(
        function id(x)
            return x
        end

        function foo(o)
            id(o.bar)
            id(o.baz)
        end
    )");

    TypeId fooType = requireType("foo");
    const FunctionType* fooFn = get<FunctionType>(fooType);
    REQUIRE(fooFn != nullptr);

    std::vector<TypeId> fooArgs = flatten(fooFn->argTypes).first;

    REQUIRE_EQ(1, fooArgs.size());

    TypeId arg0 = fooArgs[0];
    const TableType* arg0Table = get<TableType>(follow(arg0));
    REQUIRE(arg0Table != nullptr);

    CHECK(arg0Table->props.count("bar"));
    CHECK(arg0Table->props.count("baz"));
}

TEST_CASE_FIXTURE(Fixture, "table_param_row_polymorphism_1")
{
    CheckResult result = check(R"(
        function foo(o)
            local a = o.x
            local b = o.y
            return o
        end

        foo({x=55, y=nil, w=3.14159})
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "table_param_row_polymorphism_2")
{
    CheckResult result = check(R"(
        --!strict
        function foo(o)
            local a = o.bar
            local b = o.baz
        end

        foo({bar='bar'})
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    MissingProperties* error = get<MissingProperties>(result.errors[0]);
    REQUIRE(error != nullptr);
    REQUIRE(error->properties.size() == 1);

    CHECK_EQ("baz", error->properties[0]);
}

TEST_CASE_FIXTURE(Fixture, "table_param_row_polymorphism_3")
{
    CheckResult result = check(R"(
        local T = {}
        T.bar = 'hello'
        function T:method()
            local a = self.baz
        end
        T:method()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeError& err = result.errors[0];
    MissingProperties* error = get<MissingProperties>(err);
    REQUIRE(error != nullptr);
    REQUIRE(error->properties.size() == 1);

    CHECK_EQ("baz", error->properties[0]);

    // TODO(rblanckaert): Revist when we can bind self at function creation time
    /*
    CHECK_EQ(err->location,
        (Location{ Position{4, 22}, Position{4, 30} })
    );
    */

    CHECK_EQ(err.location, (Location{Position{6, 8}, Position{6, 9}}));
}

#if 0
TEST_CASE_FIXTURE(Fixture, "table_param_row_polymorphism_2")
{
    CheckResult result = check(R"(
        function id(x)
            return x
        end

        function foo(o)
            id(o.x)
            id(o.y)
            return o
        end

        local a = {x=55, y=nil, w=3.14159}
        local b = {}
        b.x = 1
        b.y = 'hello'
        b.z = 'something extra!'

        local q = foo(a) -- line 17
        local w = foo(b) -- line 18
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    for (const auto& e : result.errors)
        std::cout << "Error: " << e << std::endl;

    TypeId qType = requireType("q");
    const TableType* qTable = get<TableType>(qType);
    REQUIRE(qType != nullptr);

    CHECK(qTable->props.find("x") != qTable->props.end());
    CHECK(qTable->props.find("y") != qTable->props.end());
    CHECK(qTable->props.find("z") == qTable->props.end());
    CHECK(qTable->props.find("w") != qTable->props.end());

    TypeId wType = requireType("w");
    const TableType* wTable = get<TableType>(wType);
    REQUIRE(wTable != nullptr);

    CHECK(wTable->props.find("x") != wTable->props.end());
    CHECK(wTable->props.find("y") != wTable->props.end());
    CHECK(wTable->props.find("z") != wTable->props.end());
    CHECK(wTable->props.find("w") == wTable->props.end());
}
#endif

TEST_CASE_FIXTURE(Fixture, "table_unification_4")
{
    CheckResult result = check(R"(
        function foo(o)
            if o.prop then
                return o
            else
                return {prop=false}
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "ok_to_add_property_to_free_table")
{
    CheckResult result = check(R"(
        function fn(d)
            d:Method()
            d.prop = true
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);
}

TEST_CASE_FIXTURE(Fixture, "okay_to_add_property_to_unsealed_tables_by_assignment")
{
    CheckResult result = check(R"(
        --!strict
        local t = { u = {} }
        t = { u = { p = 37 } }
        t = { u = { q = "hi" } }
        local x = t.u.p
        local y = t.u.q
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number?", toString(requireType("x")));
    CHECK_EQ("string?", toString(requireType("y")));
}

TEST_CASE_FIXTURE(Fixture, "okay_to_add_property_to_unsealed_tables_by_function_call")
{
    CheckResult result = check(R"(
        --!strict
        function get(x) return x.opts["MYOPT"] end
        function set(x,y) x.opts["MYOPT"] = y end
        local t = { opts = {} }
        set(t,37)
        local x = get(t)
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        LUAU_REQUIRE_NO_ERRORS(result);
        CHECK_EQ("number", toString(requireType("x")));
    }
    else
    {
        LUAU_REQUIRE_ERRORS(result);
        // CHECK_EQ("number?", toString(requireType("x")));
    }
}

TEST_CASE_FIXTURE(Fixture, "width_subtyping")
{
    CheckResult result = check(R"(
        --!strict
        function f(x : { q : number })
           x.q = 8
        end
        local t : { q : number, r : string } = { q = 8, r = "hi" }
        f(t)
        local x : string = t.r
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "width_subtyping_needs_covariance")
{
    CheckResult result = check(R"(
        --!strict
        function f(x : { p : { q : number }})
           x.p = { q = 8, r = 5 }
        end
        local t : { p : { q : number, r : string } } = { p = { q = 8, r = "hi" } }
        f(t) -- Shouldn't typecheck
        local x : string = t.p.r -- x is 5
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_array")
{
    CheckResult result = check(R"(
        local t = {}
        t[1] = 'one'
        t[2] = 'two'
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const TableType* ttv = get<TableType>(requireType("t"));
    REQUIRE(ttv != nullptr);

    REQUIRE(bool(ttv->indexer));

    CHECK_EQ(*ttv->indexer->indexType, *builtinTypes->numberType);
    CHECK_EQ(*ttv->indexer->indexResultType, *builtinTypes->stringType);
}

/* This is a bit weird.
 * The type of buttonVector[i] is initially free, compared to a string with ==
 * We can't actually use this to infer that buttonVector is {string}, and we
 * also have a rule that forbids comparing unknown types with those that may have
 * metatables.
 *
 * Due to a historical quirk, strings are exempt from this rule.  Without this exemption,
 * the test code here would fail to typecheck at the use of ==.
 */
TEST_CASE_FIXTURE(Fixture, "infer_array_2")
{
    CheckResult result = check(R"(
        local buttonVector = {}

        function createButton( actionName, functionInfoTable )
            local position = nil
            for i = 1,#buttonVector do
                if buttonVector[i] == "empty" then
                    position = i
                    break
                end
            end
            return position
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "indexers_get_quantified_too")
{
    CheckResult result = check(R"(
        function swap(p)
            local temp = p[0]
            p[0] = p[1]
            p[1] = temp
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionType* ftv = get<FunctionType>(requireType("swap"));
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> argVec = flatten(ftv->argTypes).first;

    REQUIRE_EQ(1, argVec.size());

    const TableType* ttv = get<TableType>(follow(argVec[0]));
    REQUIRE(ttv != nullptr);

    REQUIRE(bool(ttv->indexer));

    const TableIndexer& indexer = *ttv->indexer;

    REQUIRE("number" == toString(indexer.indexType));

    REQUIRE(nullptr != get<GenericType>(follow(indexer.indexResultType)));
}

TEST_CASE_FIXTURE(Fixture, "indexers_quantification_2")
{
    CheckResult result = check(R"(
        function mergesort(arr)
            local p = arr[0]
            return arr
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionType* ftv = get<FunctionType>(requireType("mergesort"));
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> argVec = flatten(ftv->argTypes).first;

    REQUIRE_EQ(1, argVec.size());

    const TableType* argType = get<TableType>(follow(argVec[0]));
    REQUIRE(argType != nullptr);

    std::vector<TypeId> retVec = flatten(ftv->retTypes).first;

    const TableType* retType = get<TableType>(follow(retVec[0]));
    REQUIRE(retType != nullptr);

    CHECK_EQ(argType->state, retType->state);

    REQUIRE_EQ(*argVec[0], *retVec[0]);
}

TEST_CASE_FIXTURE(Fixture, "infer_indexer_from_array_like_table")
{
    CheckResult result = check(R"(
        local t = {"one", "two", "three"}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const TableType* ttv = get<TableType>(requireType("t"));
    REQUIRE(ttv != nullptr);

    REQUIRE(bool(ttv->indexer));
    const TableIndexer& indexer = *ttv->indexer;

    CHECK_EQ(*builtinTypes->numberType, *indexer.indexType);
    CHECK_EQ(*builtinTypes->stringType, *indexer.indexResultType);
}

TEST_CASE_FIXTURE(Fixture, "infer_indexer_from_value_property_in_literal")
{
    CheckResult result = check(R"(
        function Symbol(n)
            return { __name=n }
        end

        function f()
            return {
                [Symbol("hello")] = true,
                x = 0,
                y = 0
            }
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionType* fType = get<FunctionType>(requireType("f"));
    REQUIRE(fType != nullptr);

    auto retType_ = first(fType->retTypes);
    REQUIRE(bool(retType_));

    auto retType = get<TableType>(follow(*retType_));
    REQUIRE(retType != nullptr);

    CHECK(bool(retType->indexer));

    const TableIndexer& indexer = *retType->indexer;
    CHECK_EQ("{| __name: string |}", toString(indexer.indexType));
}

TEST_CASE_FIXTURE(Fixture, "infer_indexer_from_its_variable_type_and_unifiable")
{
    CheckResult result = check(R"(
        local t1: { [string]: string } = {}
        local t2 = { "bar" }

        t2 = t1
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm != nullptr);

    const TableType* tTy = get<TableType>(requireType("t2"));
    REQUIRE(tTy != nullptr);

    REQUIRE(tTy->indexer);
    CHECK_EQ(*builtinTypes->numberType, *tTy->indexer->indexType);
    CHECK_EQ(*builtinTypes->stringType, *tTy->indexer->indexResultType);
}

TEST_CASE_FIXTURE(Fixture, "indexer_mismatch")
{
    CheckResult result = check(R"(
        local t1: { [string]: string } = {}
        local t2: { [number]: number } = {}

        t2 = t1
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeId t1 = requireType("t1");
    TypeId t2 = requireType("t2");

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm != nullptr);
    CHECK(toString(tm->wantedType) == "{number}");
    CHECK(toString(tm->givenType) == "{| [string]: string |}");

    CHECK_NE(*t1, *t2);
}

TEST_CASE_FIXTURE(Fixture, "infer_indexer_from_its_function_return_type")
{
    CheckResult result = check(R"(
        local function f(): { [number]: string }
            return {}
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_indexer_for_left_unsealed_table_from_right_hand_table_with_indexer")
{
    CheckResult result = check(R"(
        local function f(): { [number]: string } return {} end

        local t = {}
        t = f()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "sealed_table_value_can_infer_an_indexer")
{
    CheckResult result = check(R"(
        local t: { a: string, [number]: string } = { a = "foo" }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "array_factory_function")
{
    CheckResult result = check(R"(
        function empty() return {} end
        local array: {string} = empty()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "sealed_table_indexers_must_unify")
{
    CheckResult result = check(R"(
        local A = { 5, 7, 8 }
        local B = { "one", "two", "three" }

        B = A
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_MESSAGE(nullptr != get<TypeMismatch>(result.errors[0]), "Expected a TypeMismatch but got " << result.errors[0]);
}

TEST_CASE_FIXTURE(Fixture, "indexer_on_sealed_table_must_unify_with_free_table")
{
    CheckResult result = check(R"(
        local A = { 1, 2, 3 }
        function F(t)
            t[4] = "hi"
            A = t
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "infer_type_when_indexing_from_a_table_indexer")
{
    CheckResult result = check(R"(
        local t: { [number]: string }
        local s = t[1]
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*builtinTypes->stringType, *requireType("s"));
}

TEST_CASE_FIXTURE(Fixture, "indexing_from_a_table_should_prefer_properties_when_possible")
{
    CheckResult result = check(R"(
        local t: { a: string, [string]: number }
        local a1 = t.a
        local a2 = t["a"]

        local b1 = t.b
        local b2 = t["b"]

        local some_indirection_variable = "foo"
        local c = t[some_indirection_variable]

        local d = t[1]
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(*builtinTypes->stringType, *requireType("a1"));
    CHECK_EQ(*builtinTypes->stringType, *requireType("a2"));

    CHECK_EQ(*builtinTypes->numberType, *requireType("b1"));
    CHECK_EQ(*builtinTypes->numberType, *requireType("b2"));

    CHECK_EQ(*builtinTypes->numberType, *requireType("c"));

    CHECK_MESSAGE(nullptr != get<TypeMismatch>(result.errors[0]), "Expected a TypeMismatch but got " << result.errors[0]);
}

TEST_CASE_FIXTURE(Fixture, "any_when_indexing_into_an_unsealed_table_with_no_indexer_in_nonstrict_mode")
{
    CheckResult result = check(R"(
        --!nonstrict

        local constants = {
            key1 = "value1",
            key2 = "value2"
        }

        local function getKey()
            return "key1"
        end

        local k1 = constants[getKey()]
    )");

    CHECK("any" == toString(requireType("k1")));

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "disallow_indexing_into_an_unsealed_table_with_no_indexer_in_strict_mode")
{
    CheckResult result = check(R"(
        local constants = {
            key1 = "value1",
            key2 = "value2"
        }

        function getConstant(key)
            return constants[key]
        end

        local k1 = getConstant("key1")
    )");

    CHECK("any" == toString(requireType("k1")));

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "assigning_to_an_unsealed_table_with_string_literal_should_infer_new_properties_over_indexer")
{
    CheckResult result = check(R"(
        local t = {}
        t["a"] = "foo"

        local a = t.a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("string" == toString(*builtinTypes->stringType));

    TableType* tableType = getMutable<TableType>(requireType("t"));
    REQUIRE(tableType != nullptr);
    REQUIRE(tableType->indexer == std::nullopt);
    REQUIRE(0 != tableType->props.count("a"));

    TypeId propertyA = tableType->props["a"].type;
    REQUIRE(propertyA != nullptr);
    CHECK_EQ(*builtinTypes->stringType, *propertyA);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "oop_indexer_works")
{
    CheckResult result = check(R"(
        local clazz = {}
        clazz.__index = clazz

        function clazz:speak()
            return "hi"
        end

        function clazz.new()
            return setmetatable({}, clazz)
        end

        local me = clazz.new()
        local words = me:speak()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*builtinTypes->stringType, *requireType("words"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "indexer_table")
{
    CheckResult result = check(R"(
        local clazz = {a="hello"}
        local instanace = setmetatable({}, {__index=clazz})
        local b = instanace.a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*builtinTypes->stringType, *requireType("b"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "indexer_fn")
{
    CheckResult result = check(R"(
        local instanace = setmetatable({}, {__index=function() return 10 end})
        local b = instanace.somemethodwedonthave
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*builtinTypes->numberType, *requireType("b"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "meta_add")
{
    // Note: meta_add_inferred and this unit test are currently the same exact thing.
    // We'll want to change this one in particular when we add real syntax for metatables.

    CheckResult result = check(R"(
        local a = setmetatable({}, {__add = function(l, r) return l end})
        type Vector = typeof(a)
        local b:Vector
        local c = a + b
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(follow(requireType("a")), follow(requireType("c")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "meta_add_inferred")
{
    CheckResult result = check(R"(
        local a = {}
        setmetatable(a, {__add=function(a,b) return b end} )
        local c = a + a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*requireType("a"), *requireType("c"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "meta_add_both_ways")
{
    CheckResult result = check(R"(
        type VectorMt = { __add: (Vector, number) -> Vector }
        local vectorMt: VectorMt
        type Vector = typeof(setmetatable({}, vectorMt))
        local a: Vector

        local b = a + 2
        local c = 2 + a
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Vector", toString(requireType("a")));
    CHECK_EQ(*requireType("a"), *requireType("b"));
    CHECK_EQ(*requireType("a"), *requireType("c"));
}

// This test exposed a bug where we let go of the "seen" stack while unifying table types
// As a result, type inference crashed with a stack overflow.
TEST_CASE_FIXTURE(BuiltinsFixture, "unification_of_unions_in_a_self_referential_type")
{
    CheckResult result = check(R"(
        type A = {}
        type AMT = { __mul: (A, A | number) -> A }
        local a: A
        local amt: AMT
        setmetatable(a, amt)

        type B = {}
        type BMT = { __mul: (B, A | B | number) -> A }
        local b: B
        local bmt: BMT
        setmetatable(b, bmt)

        a = b
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const MetatableType* amtv = get<MetatableType>(requireType("a"));
    REQUIRE(amtv);
    CHECK_EQ(amtv->metatable, requireType("amt"));

    const MetatableType* bmtv = get<MetatableType>(requireType("b"));
    REQUIRE(bmtv);
    CHECK_EQ(bmtv->metatable, requireType("bmt"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "oop_polymorphic")
{
    CheckResult result = check(R"(
        local animal = {}
        animal.__index = animal
        function animal:isAlive() return true end
        function animal:speed() return 10 end

        local pelican = {}
        setmetatable(pelican, animal)
        pelican.__index = pelican
        function pelican:movement() return "fly" end
        function pelican:speed() return 30 end

        function pelican.new(name)
            local s = {}
            setmetatable(s, pelican)
            s.name = name
            return s
        end

        local scoops = pelican.new("scoops")

        local alive = scoops:isAlive()
        local at = scoops.isAlive
        local movement = scoops:movement()
        local name = scoops.name
        local speed = scoops:speed()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*builtinTypes->booleanType, *requireType("alive"));
    CHECK_EQ(*builtinTypes->stringType, *requireType("movement"));
    CHECK_EQ(*builtinTypes->stringType, *requireType("name"));
    CHECK_EQ(*builtinTypes->numberType, *requireType("speed"));
}

TEST_CASE_FIXTURE(Fixture, "user_defined_table_types_are_named")
{
    CheckResult result = check(R"(
        type Vector3 = {x: number, y: number}

        local v: Vector3
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Vector3", toString(requireType("v")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "result_is_always_any_if_lhs_is_any")
{
    CheckResult result = check(R"(
        type Vector3MT = {
            __add: (Vector3MT, Vector3MT) -> Vector3MT,
            __mul: (Vector3MT, Vector3MT|number) -> Vector3MT
        }

        local Vector3: {new: (number?, number?, number?) -> Vector3MT}
        local Vector3MT: Vector3MT
        setmetatable(Vector3, Vector3MT)

        type CFrameMT = {
            __mul: (CFrameMT, Vector3MT|CFrameMT) -> Vector3MT|CFrameMT
        }

        local CFrame: {
            Angles:(number, number, number) -> CFrameMT
        }
        local CFrameMT: CFrameMT
        setmetatable(CFrame, CFrameMT)

        local n: any
        local a = (n + Vector3.new(0, 1.5, 0)) * CFrame.Angles(0, math.pi/2, 0)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("any", toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "result_is_bool_for_equality_operators_if_lhs_is_any")
{
    CheckResult result = check(R"(
        local a: any
        local b: number

        local c = a < b
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("boolean", toString(requireType("c")));
}

TEST_CASE_FIXTURE(Fixture, "inequality_operators_imply_exactly_matching_types")
{
    CheckResult result = check(R"(
        function abs(n)
            if n < 0 then
                return -n
            else
                return n
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("(number) -> number", toString(requireType("abs")));
}

TEST_CASE_FIXTURE(Fixture, "nice_error_when_trying_to_fetch_property_of_boolean")
{
    CheckResult result = check(R"(
        local a = true
        local b = a.some_prop
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Type 'boolean' does not have key 'some_prop'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "defining_a_method_for_a_builtin_sealed_table_must_fail")
{
    CheckResult result = check(R"(
        function string.m() end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "defining_a_self_method_for_a_builtin_sealed_table_must_fail")
{
    CheckResult result = check(R"(
        function string:m() end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "defining_a_method_for_a_local_sealed_table_must_fail")
{
    CheckResult result = check(R"(
        function mkt() return {x = 1} end
        local t = mkt()
        function t.m() end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "defining_a_self_method_for_a_local_sealed_table_must_fail")
{
    CheckResult result = check(R"(
        function mkt() return {x = 1} end
        local t = mkt()
        function t:m() end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "defining_a_method_for_a_local_unsealed_table_is_ok")
{
    CheckResult result = check(R"(
        local t = {x = 1}
        function t.m() end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "defining_a_self_method_for_a_local_unsealed_table_is_ok")
{
    CheckResult result = check(R"(
        local t = {x = 1}
        function t:m() end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// This unit test could be flaky if the fix has regressed.
TEST_CASE_FIXTURE(Fixture, "pass_incompatible_union_to_a_generic_table_without_crashing")
{
    CheckResult result = check(R"(
        -- must be in this specific order, and with (roughly) those exact properties!
        type A = {x: number, [any]: any} | {}
        local a: A

        function f(t)
            t.y = 1
        end

        f(a)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(get<TypeMismatch>(result.errors[0]));
}

// This unit test could be flaky if the fix has regressed.
TEST_CASE_FIXTURE(Fixture, "passing_compatible_unions_to_a_generic_table_without_crashing")
{
    CheckResult result = check(R"(
        type A = {x: number, y: number, [any]: any} | {y: number}

        function f(t)
            t.y = 1
        end

        f({y = 5} :: A)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "found_like_key_in_table_function_call")
{
    CheckResult result = check(R"(
        local t = {}
        function t.Foo() end

        t.fOo()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeError te = result.errors[0];
    UnknownPropButFoundLikeProp* error = get<UnknownPropButFoundLikeProp>(te);
    REQUIRE(error);

    TypeId t = requireType("t");
    CHECK_EQ(*t, *error->table);
    CHECK_EQ("fOo", error->key);

    auto candidates = error->candidates;
    CHECK_EQ(1, candidates.size());
    CHECK(candidates.find("Foo") != candidates.end());

    CHECK_EQ(toString(te), "Key 'fOo' not found in table 't'.  Did you mean 'Foo'?");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "found_like_key_in_table_property_access")
{
    CheckResult result = check(R"(
        local t = {X = 1}

        print(t.x)
    )");

    REQUIRE_EQ(result.errors.size(), 1);

    TypeError te = result.errors[0];
    UnknownPropButFoundLikeProp* error = get<UnknownPropButFoundLikeProp>(te);
    REQUIRE(error);

    TypeId t = requireType("t");
    CHECK_EQ(*t, *error->table);
    CHECK_EQ("x", error->key);

    auto candidates = error->candidates;
    CHECK_EQ(1, candidates.size());
    CHECK(candidates.find("X") != candidates.end());

    CHECK_EQ(toString(te), "Key 'x' not found in table 't'.  Did you mean 'X'?");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "found_multiple_like_keys")
{
    CheckResult result = check(R"(
        local t = {Foo = 1, foO = 2}

        print(t.foo)
    )");

    REQUIRE_EQ(result.errors.size(), 1);

    TypeError te = result.errors[0];
    UnknownPropButFoundLikeProp* error = get<UnknownPropButFoundLikeProp>(te);
    REQUIRE(error);

    TypeId t = requireType("t");
    CHECK_EQ(*t, *error->table);
    CHECK_EQ("foo", error->key);

    auto candidates = error->candidates;
    CHECK_EQ(2, candidates.size());
    CHECK(candidates.find("Foo") != candidates.end());
    CHECK(candidates.find("foO") != candidates.end());

    CHECK_EQ(toString(te), "Key 'foo' not found in table 't'.  Did you mean one of 'Foo', 'foO'?");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "dont_suggest_exact_match_keys")
{
    CheckResult result = check(R"(
        local t = {}
        t.foO = 1
        print(t.Foo)
        t.Foo = 2
    )");

    REQUIRE_EQ(result.errors.size(), 1);

    TypeError te = result.errors[0];
    UnknownPropButFoundLikeProp* error = get<UnknownPropButFoundLikeProp>(te);
    REQUIRE(error);

    TypeId t = requireType("t");
    CHECK_EQ(*t, *error->table);
    CHECK_EQ("Foo", error->key);

    auto candidates = error->candidates;
    CHECK_EQ(1, candidates.size());
    CHECK(candidates.find("foO") != candidates.end());
    CHECK(candidates.find("Foo") == candidates.end());

    CHECK_EQ(toString(te), "Key 'Foo' not found in table 't'.  Did you mean 'foO'?");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "getmetatable_returns_pointer_to_metatable")
{
    CheckResult result = check(R"(
        local t = {x = 1}
        local mt = {__index = {y = 2}}
        setmetatable(t, mt)

        local returnedMT = getmetatable(t)
    )");

    CHECK_EQ(*requireType("mt"), *requireType("returnedMT"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable_mismatch_should_fail")
{
    CheckResult result = check(R"(
        local t1 = {x = 1}
        local mt1 = {__index = {y = 2}}
        setmetatable(t1, mt1)

        local t2 = {x = 1}
        local mt2 = {__index = function() return nil end}
        setmetatable(t2, mt2)

        t1 = t2
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(*tm->wantedType, *requireType("t1"));
    CHECK_EQ(*tm->givenType, *requireType("t2"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "property_lookup_through_tabletypevar_metatable")
{
    CheckResult result = check(R"(
        local t = {x = 1}
        local mt = {__index = {y = 2}}
        setmetatable(t, mt)

        print(t.x)
        print(t.y)
        print(t.z)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownProperty* up = get<UnknownProperty>(result.errors[0]);
    REQUIRE_MESSAGE(up, result.errors[0].data);
    CHECK_EQ(up->key, "z");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "missing_metatable_for_sealed_tables_do_not_get_inferred")
{
    CheckResult result = check(R"(
        local t = {x = 1}

        local a = {x = 1}
        local b = {__index = {y = 2}}
        setmetatable(a, b)

        t = a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeId a = requireType("a");
    TypeId t = requireType("t");
    CHECK_NE(*a, *t);

    TypeError te = result.errors[0];
    TypeMismatch* tm = get<TypeMismatch>(te);
    REQUIRE(tm);
    CHECK_EQ(tm->wantedType, t);
    CHECK_EQ(tm->givenType, a);

    const MetatableType* aTy = get<MetatableType>(a);
    REQUIRE(aTy);

    const TableType* tTy = get<TableType>(t);
    REQUIRE(tTy);
}

// Could be flaky if the fix has regressed.
TEST_CASE_FIXTURE(Fixture, "right_table_missing_key")
{
    CheckResult result = check(R"(
        function _(...)
        end
        local l7 = not _,function(l0)
        _ += _((_) or {function(...)
        end,["z"]=_,} or {},(function(l43,...)
        end))
        _ += 0 < {}
        end
        repeat
        until _
        local l0 = n4,_((_) or {} or {[30976]=_,},({}))
    )");

    CHECK_GE(result.errors.size(), 0);
}

// Could be flaky if the fix has regressed.
TEST_CASE_FIXTURE(Fixture, "right_table_missing_key2")
{
    CheckResult result = check(R"(
        local lt: { [string]: string, a: string }
        local rt: {}

        lt = rt
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    MissingProperties* mp = get<MissingProperties>(result.errors[0]);
    REQUIRE(mp);
    CHECK_EQ(mp->context, MissingProperties::Missing);
    REQUIRE_EQ(1, mp->properties.size());
    CHECK_EQ(mp->properties[0], "a");

    CHECK_EQ("{| [string]: string, a: string |}", toString(mp->superType));
    CHECK_EQ("{|  |}", toString(mp->subType));
}

TEST_CASE_FIXTURE(Fixture, "casting_unsealed_tables_with_props_into_table_with_indexer")
{
    CheckResult result = check(R"(
        type StringToStringMap = { [string]: string }
        local rt: StringToStringMap = { ["foo"] = 1 }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    ToStringOptions o{/* exhaustive= */ true};
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("{| [string]: string |}", toString(tm->wantedType, o));
    // Should t now have an indexer?
    // It would if the assignment to rt was correctly typed.
    CHECK_EQ("{ [string]: string, foo: number }", toString(tm->givenType, o));
}

TEST_CASE_FIXTURE(Fixture, "casting_sealed_tables_with_props_into_table_with_indexer")
{
    CheckResult result = check(R"(
        type StringToStringMap = { [string]: string }
        function mkrt() return { ["foo"] = 1 } end
        local rt: StringToStringMap = mkrt()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    ToStringOptions o{/* exhaustive= */ true};
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("{| [string]: string |}", toString(tm->wantedType, o));
    CHECK_EQ("{| foo: number |}", toString(tm->givenType, o));
}

TEST_CASE_FIXTURE(Fixture, "casting_tables_with_props_into_table_with_indexer2")
{
    CheckResult result = check(R"(
        local function foo(a: {[string]: number, a: string}) end
        foo({ a = "" })
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "casting_tables_with_props_into_table_with_indexer3")
{
    CheckResult result = check(R"(
        local function foo(a: {[string]: number, a: string}) end
        foo({ a = 1 })
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    ToStringOptions o{/* exhaustive= */ true};
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("{| [string]: number, a: string |}", toString(tm->wantedType, o));
    CHECK_EQ("{ a: number }", toString(tm->givenType, o));
}

TEST_CASE_FIXTURE(Fixture, "casting_tables_with_props_into_table_with_indexer4")
{
    CheckResult result = check(R"(
        local function foo(a: {[string]: number, a: string}, i: string)
            return a[i]
        end
        local hi: number = foo({ a = "hi" }, "a") -- shouldn't typecheck since at runtime hi is "hi"
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK(toString(result.errors[0]) == "Type 'string' could not be converted into 'number'");
    }
    else
    {
        // This typechecks but shouldn't
        LUAU_REQUIRE_NO_ERRORS(result);
    }
}

TEST_CASE_FIXTURE(Fixture, "table_subtyping_with_missing_props_dont_report_multiple_errors")
{
    CheckResult result = check(R"(
        local vec3 = {x = 1, y = 2, z = 3}
        local vec1 = {x = 1}

        vec3 = vec1
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    MissingProperties* mp = get<MissingProperties>(result.errors[0]);
    REQUIRE(mp);
    CHECK_EQ(mp->context, MissingProperties::Missing);
    REQUIRE_EQ(2, mp->properties.size());
    CHECK_EQ(mp->properties[0], "y");
    CHECK_EQ(mp->properties[1], "z");
    CHECK_EQ("vec3", toString(mp->superType));
    CHECK_EQ("vec1", toString(mp->subType));
}

TEST_CASE_FIXTURE(Fixture, "table_subtyping_with_missing_props_dont_report_multiple_errors2")
{
    CheckResult result = check(R"(
        type DumbMixedTable = {[number]: number, x: number}
        local t: DumbMixedTable = {"fail"}
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    MissingProperties* mp = get<MissingProperties>(result.errors[1]);
    REQUIRE(mp);
    CHECK_EQ(mp->context, MissingProperties::Missing);
    REQUIRE_EQ(1, mp->properties.size());
    CHECK_EQ(mp->properties[0], "x");
}

TEST_CASE_FIXTURE(Fixture, "table_subtyping_with_extra_props_dont_report_multiple_errors")
{
    CheckResult result = check(R"(
        function mkvec3() return {x = 1, y = 2, z = 3} end
        function mkvec1() return {x = 1} end

        local vec3 = {mkvec3()}
        local vec1 = {mkvec1()}

        vec1 = vec3
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("vec1", toString(tm->wantedType));
    CHECK_EQ("vec3", toString(tm->givenType));
}

TEST_CASE_FIXTURE(Fixture, "table_subtyping_with_extra_props_is_ok")
{
    CheckResult result = check(R"(
        local vec3 = {x = 1, y = 2, z = 3}
        local vec1 = {x = 1}

        vec1 = vec3
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "type_mismatch_on_massive_table_is_cut_short")
{
    ScopedFastInt sfis{"LuauTableTypeMaximumStringifierLength", 40};

    CheckResult result = check(R"(
        local t
        t = {}
        t.a = 1
        t.b = 1
        t.c = 1
        t.d = 1
        t.e = 1
        t.f = 1

        t = 1
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK("{ a: number, b: number, c: number, d: number, e: number, ... 1 more ... }" == toString(requireType("t")));
    CHECK_EQ("number", toString(tm->givenType));

    CHECK_EQ("Type 'number' could not be converted into '{ a: number, b: number, c: number, d: number, e: number, ... 1 more ... }'",
        toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "ok_to_set_nil_even_on_non_lvalue_base_expr")
{
    CheckResult result = check(R"(
        local function f(): { [string]: number }
            return { ["foo"] = 1 }
        end

        f()["foo"] = nil
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "ok_to_provide_a_subtype_during_construction")
{
    CheckResult result = check(R"(
        local a: string | number = 1
        local t = {a, 1}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("{number | string}", toString(requireType("t"), {/*exhaustive*/ true}));
}

TEST_CASE_FIXTURE(Fixture, "reasonable_error_when_adding_a_nonexistent_property_to_an_array_like_table")
{
    CheckResult result = check(R"(
        --!strict
        function mkA() return {"value"} end
        local A = mkA()
        A.B = "Hello"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownProperty* up = get<UnknownProperty>(result.errors[0]);
    REQUIRE(up != nullptr);

    CHECK_EQ("B", up->key);
}

TEST_CASE_FIXTURE(Fixture, "shorter_array_types_actually_work")
{
    CheckResult result = check(R"(
        --!strict
        local A: {string | number}
    )");

    LUAU_REQUIRE_ERROR_COUNT(0, result);
    CHECK_EQ("{number | string}", toString(requireType("A")));
}

TEST_CASE_FIXTURE(Fixture, "only_ascribe_synthetic_names_at_module_scope")
{
    CheckResult result = check(R"(
        --!strict
        local TopLevel = {}
        local foo

        for i = 1, 10 do
            local SubScope = { 1, 2, 3 }
            foo = SubScope
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(0, result);

    CHECK_EQ("TopLevel", toString(requireType("TopLevel")));
    CHECK_EQ("{number}", toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "hide_table_error_properties")
{
    CheckResult result = check(R"(
        --!strict

        local function f()
        local function mkt() return { x = 1 } end
        local t = mkt()

        function t.a() end
        function t.b() end

        return t
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK_EQ("Cannot add property 'a' to table '{| x: number |}'", toString(result.errors[0]));
    CHECK_EQ("Cannot add property 'b' to table '{| x: number |}'", toString(result.errors[1]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "builtin_table_names")
{
    CheckResult result = check(R"(
        os.h = 2
        string.k = 3
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK_EQ("Cannot add property 'h' to table 'typeof(os)'", toString(result.errors[0]));
    CHECK_EQ("Cannot add property 'k' to table 'typeof(string)'", toString(result.errors[1]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "persistent_sealed_table_is_immutable")
{
    CheckResult result = check(R"(
        --!nonstrict
        function os:bad() end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Cannot add property 'bad' to table 'typeof(os)'", toString(result.errors[0]));

    const TableType* osType = get<TableType>(requireType("os"));
    REQUIRE(osType != nullptr);
    CHECK(osType->props.find("bad") == osType->props.end());
}

TEST_CASE_FIXTURE(Fixture, "common_table_element_list")
{
    CheckResult result = check(R"(
type Table = {
    a: number,
    b: number?
}

local Test: {Table} = {
    { a = 1 },
    { a = 2, b = 3 }
}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "common_table_element_general")
{
    CheckResult result = check(R"(
type Table = {
    a: number,
    b: number?
}

local Test: {Table} = {
    [2] = { a = 1 },
    [5] = { a = 2, b = 3 }
}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "common_table_element_inner_index")
{
    CheckResult result = check(R"(
type Table = {
    a: number,
    b: number?
}

local Test: {{Table}} = {{
    { a = 1 },
    { a = 2, b = 3 }
},{
    { a = 3 },
    { a = 4, b = 3 }
}}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "common_table_element_inner_prop")
{
    CheckResult result = check(R"(
type Table = {
    a: number,
    b: number?
}

local Test: {{x: Table, y: Table}} = {{
    x = { a = 1 },
    y = { a = 2, b = 3 }
},{
    x = { a = 3 },
    y = { a = 4 }
}}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "common_table_element_union_assignment")
{
    CheckResult result = check(R"(
type Foo = {x: number | string}

local foos: {Foo} = {
    {x = 1234567},
    {x = "hello"},
}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "quantifying_a_bound_var_works")
{
    CheckResult result = check(R"(
        local clazz = {}
        clazz.__index = clazz

        function clazz:speak()
            return "hi"
        end

        function clazz.new()
            return setmetatable({}, clazz)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    TypeId ty = requireType("clazz");
    TableType* ttv = getMutable<TableType>(ty);
    REQUIRE(ttv);
    REQUIRE(ttv->props.count("new"));
    Property& prop = ttv->props["new"];
    REQUIRE(prop.type);
    const FunctionType* ftv = get<FunctionType>(follow(prop.type));
    REQUIRE(ftv);
    const TypePack* res = get<TypePack>(follow(ftv->retTypes));
    REQUIRE(res);
    REQUIRE(res->head.size() == 1);
    const MetatableType* mtv = get<MetatableType>(follow(res->head[0]));
    REQUIRE(mtv);
    ttv = getMutable<TableType>(follow(mtv->table));
    REQUIRE(ttv);
    REQUIRE_EQ(ttv->state, TableState::Sealed);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "less_exponential_blowup_please")
{
    ScopedFastFlag sff{"DebugLuauSharedSelf", true};

    CheckResult result = check(R"(
        --!strict

        local Foo = setmetatable({}, {})
        Foo.__index = Foo

        function Foo.new()
            local self = setmetatable({}, Foo)
            return self:constructor() or self
        end
        function Foo:constructor() end

        function Foo:create()
            local foo = Foo.new()
            foo:First()
            foo:Second()
            foo:Third()
            return foo
        end
        function Foo:First() end
        function Foo:Second() end
        function Foo:Third() end

        local newData = Foo:create()
        newData:First()
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(Fixture, "common_table_element_union_in_call")
{
    CheckResult result = check(R"(
local function foo(l: {{x: number | string}}) end

foo({
    {x = 1234567},
    {x = "hello"},
})
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "common_table_element_union_in_call_tail")
{
    CheckResult result = check(R"(
type Foo = {x: number | string}
local function foo(l: {Foo}, ...: {Foo}) end

foo({{x = 1234567}, {x = "hello"}}, {{x = 1234567}, {x = "hello"}}, {{x = 1234567}, {x = "hello"}})
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "common_table_element_union_in_prop")
{
    CheckResult result = check(R"(
type Foo = {x: number | string}
local t: { a: {Foo}, b: number } = {
    a = {
        {x = 1234567},
        {x = "hello"},
    },
    b = 5
}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// It's unsound to instantiate tables containing generic methods,
// since mutating properties means table properties should be invariant.
TEST_CASE_FIXTURE(Fixture, "invariant_table_properties_means_instantiating_tables_in_assignment_is_unsound")
{
    CheckResult result = check(R"(
        --!strict
        local t = {}
        function t.m(x) return x end
        local a : string = t.m("hi")
        local b : number = t.m(5)
        local u : { m : (number)->number } = t -- This shouldn't typecheck
        u.m = function(x) return 1+x end
        local c : string = t.m("hi")
    )");

    // TODO: test behavior is wrong with LuauInstantiateInSubtyping until we can re-enable the covariant requirement for instantiation in subtyping
    if (FFlag::LuauInstantiateInSubtyping)
        LUAU_REQUIRE_NO_ERRORS(result);
    else
        LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_insert_should_cope_with_optional_properties_in_nonstrict")
{
    CheckResult result = check(R"(
        --!nonstrict
        local buttons = {}
        table.insert(buttons, { a = 1 })
        table.insert(buttons, { a = 2, b = true })
        table.insert(buttons, { a = 3 })
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_insert_should_cope_with_optional_properties_in_strict")
{
    CheckResult result = check(R"(
        --!strict
        local buttons = {}
        table.insert(buttons, { a = 1 })
        table.insert(buttons, { a = 2, b = true })
        table.insert(buttons, { a = 3 })
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_prop")
{
    CheckResult result = check(R"(
type A = { x: number, y: number }
type B = { x: number, y: string }

local a: A
local b: B = a
    )");

    LUAU_REQUIRE_ERRORS(result);
    if (FFlag::LuauTypeMismatchInvarianceInError)
        CHECK_EQ(toString(result.errors[0]), R"(Type 'A' could not be converted into 'B'
caused by:
  Property 'y' is not compatible. Type 'number' could not be converted into 'string' in an invariant context)");
    else
        CHECK_EQ(toString(result.errors[0]), R"(Type 'A' could not be converted into 'B'
caused by:
  Property 'y' is not compatible. Type 'number' could not be converted into 'string')");
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_prop_nested")
{
    CheckResult result = check(R"(
type AS = { x: number, y: number }
type BS = { x: number, y: string }

type A = { a: boolean, b: AS }
type B = { a: boolean, b: BS }

local a: A
local b: B = a
    )");

    LUAU_REQUIRE_ERRORS(result);
    if (FFlag::LuauTypeMismatchInvarianceInError)
        CHECK_EQ(toString(result.errors[0]), R"(Type 'A' could not be converted into 'B'
caused by:
  Property 'b' is not compatible. Type 'AS' could not be converted into 'BS'
caused by:
  Property 'y' is not compatible. Type 'number' could not be converted into 'string' in an invariant context)");
    else
        CHECK_EQ(toString(result.errors[0]), R"(Type 'A' could not be converted into 'B'
caused by:
  Property 'b' is not compatible. Type 'AS' could not be converted into 'BS'
caused by:
  Property 'y' is not compatible. Type 'number' could not be converted into 'string')");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "error_detailed_metatable_prop")
{
    CheckResult result = check(R"(
local a1 = setmetatable({ x = 2, y = 3 }, { __call = function(s) end });
local b1 = setmetatable({ x = 2, y = "hello" }, { __call = function(s) end });
local c1: typeof(a1) = b1

local a2 = setmetatable({ x = 2, y = 3 }, { __call = function(s) end });
local b2 = setmetatable({ x = 2, y = 4 }, { __call = function(s, t) end });
local c2: typeof(a2) = b2
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    if (FFlag::LuauTypeMismatchInvarianceInError)
        CHECK_EQ(toString(result.errors[0]), R"(Type 'b1' could not be converted into 'a1'
caused by:
  Type '{ x: number, y: string }' could not be converted into '{ x: number, y: number }'
caused by:
  Property 'y' is not compatible. Type 'string' could not be converted into 'number' in an invariant context)");
    else
        CHECK_EQ(toString(result.errors[0]), R"(Type 'b1' could not be converted into 'a1'
caused by:
  Type '{ x: number, y: string }' could not be converted into '{ x: number, y: number }'
caused by:
  Property 'y' is not compatible. Type 'string' could not be converted into 'number')");

    if (FFlag::LuauInstantiateInSubtyping)
    {
        CHECK_EQ(toString(result.errors[1]), R"(Type 'b2' could not be converted into 'a2'
caused by:
  Type '{ __call: <a, b>(a, b) -> () }' could not be converted into '{ __call: <a>(a) -> () }'
caused by:
  Property '__call' is not compatible. Type '<a, b>(a, b) -> ()' could not be converted into '<a>(a) -> ()'; different number of generic type parameters)");
    }
    else
    {
        CHECK_EQ(toString(result.errors[1]), R"(Type 'b2' could not be converted into 'a2'
caused by:
  Type '{ __call: (a, b) -> () }' could not be converted into '{ __call: <a>(a) -> () }'
caused by:
  Property '__call' is not compatible. Type '(a, b) -> ()' could not be converted into '<a>(a) -> ()'; different number of generic type parameters)");
    }
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_indexer_key")
{
    CheckResult result = check(R"(
        type A = { [number]: string }
        type B = { [string]: string }

        local a: A = { 'a', 'b' }
        local b: B = a
    )");

    LUAU_REQUIRE_ERRORS(result);
    if (FFlag::LuauTypeMismatchInvarianceInError)
        CHECK_EQ(toString(result.errors[0]), R"(Type 'A' could not be converted into 'B'
caused by:
  Property '[indexer key]' is not compatible. Type 'number' could not be converted into 'string' in an invariant context)");
    else
        CHECK_EQ(toString(result.errors[0]), R"(Type 'A' could not be converted into 'B'
caused by:
  Property '[indexer key]' is not compatible. Type 'number' could not be converted into 'string')");
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_indexer_value")
{
    CheckResult result = check(R"(
        type A = { [number]: number }
        type B = { [number]: string }

        local a: A = { 1, 2, 3 }
        local b: B = a
    )");

    LUAU_REQUIRE_ERRORS(result);
    if (FFlag::LuauTypeMismatchInvarianceInError)
        CHECK_EQ(toString(result.errors[0]), R"(Type 'A' could not be converted into 'B'
caused by:
  Property '[indexer value]' is not compatible. Type 'number' could not be converted into 'string' in an invariant context)");
    else
        CHECK_EQ(toString(result.errors[0]), R"(Type 'A' could not be converted into 'B'
caused by:
  Property '[indexer value]' is not compatible. Type 'number' could not be converted into 'string')");
}

TEST_CASE_FIXTURE(Fixture, "explicitly_typed_table")
{
    CheckResult result = check(R"(
--!strict
type Super = { x : number }
type Sub = { x : number, y: number }
type HasSuper = { p : Super }
type HasSub = { p : Sub }
local a: HasSuper = { p = { x = 5, y = 7 }}
a.p = { x = 9 }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "explicitly_typed_table_error")
{
    CheckResult result = check(R"(
--!strict
type Super = { x : number }
type Sub = { x : number, y: number }
type HasSuper = { p : Super }
type HasSub = { p : Sub }
local tmp = { p = { x = 5, y = 7 }}
local a: HasSuper = tmp
a.p = { x = 9 }
-- needs to be an error because
local y: number = tmp.p.y
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), R"(Type 'tmp' could not be converted into 'HasSuper'
caused by:
  Property 'p' is not compatible. Table type '{ x: number, y: number }' not compatible with type 'Super' because the former has extra field 'y')");
}

TEST_CASE_FIXTURE(Fixture, "explicitly_typed_table_with_indexer")
{
    CheckResult result = check(R"(
--!strict
type Super = { x : number }
type Sub = { x : number, y: number }
type HasSuper = { [string] : Super }
type HasSub = { [string] : Sub }
local a: HasSuper = { p = { x = 5, y = 7 }}
a.p = { x = 9 }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "recursive_metatable_type_call")
{
    CheckResult result = check(R"(
local b
b = setmetatable({}, {__call = b})
b()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), R"(Cannot call non-function t1 where t1 = { @metatable { __call: t1 }, {  } })");
}

TEST_CASE_FIXTURE(Fixture, "table_subtyping_shouldn't_add_optional_properties_to_sealed_tables")
{
    CheckResult result = check(R"(
        --!strict
        local function setNumber(t: { p: number? }, x:number) t.p = x end
        local function getString(t: { p: string? }):string return t.p or "" end
        -- This shouldn't type-check!
        local function oh(x:number): string
          local t: {} = {}
          setNumber(t, x)
          return getString(t)
        end
        local s: string = oh(37)
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "top_table_type")
{
    CheckResult result = check(R"(
        --!strict
        type Table = { [any] : any }
        type HasTable = { p: Table? }
        type HasHasTable = { p: HasTable? }
        local t : Table = { p = 5 }
        local u : HasTable = { p = { p = 5 } }
        local v : HasHasTable = { p = { p = { p = 5 } } }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "length_operator_union")
{
    CheckResult result = check(R"(
local x: {number} | {string}
local y = #x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "length_operator_intersection")
{
    CheckResult result = check(R"(
local x: {number} & {z:string} -- mixed tables are evil
local y = #x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "length_operator_non_table_union")
{
    CheckResult result = check(R"(
local x: {number} | any | string
local y = #x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "length_operator_union_errors")
{
    CheckResult result = check(R"(
local x: {number} | number | string
local y = #x
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "dont_hang_when_trying_to_look_up_in_cyclic_metatable_index")
{
    // t :: t1 where t1 = {metatable {__index: t1, __tostring: (t1) -> string}}
    CheckResult result = check(R"(
        local mt = {}
        local t = setmetatable({}, mt)
        mt.__index = t

        function mt:__tostring()
            return t.p
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type 't' does not have key 'p'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "give_up_after_one_metatable_index_look_up")
{
    CheckResult result = check(R"(
        local data = { x = 5 }
        local t1 = setmetatable({}, { __index = data })
        local t2 = setmetatable({}, t1) -- note: must be t1, not a new table

        local x1 = t1.x -- ok
        local x2 = t2.x -- nope
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type 't2' does not have key 'x'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "confusing_indexing")
{
    CheckResult result = check(R"(
        type T = {} & {p: number | string}
        local function f(t: T)
            return t.p
        end

        local foo = f({p = "string"})
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number | string", toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "pass_a_union_of_tables_to_a_function_that_requires_a_table")
{
    CheckResult result = check(R"(
        local a: {x: number, y: number, [any]: any} | {y: number}

        function f(t)
            t.y = 1
            return t
        end

        local b = f(a)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    REQUIRE_EQ("{| [any]: any, x: number, y: number |} | {| y: number |}", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "pass_a_union_of_tables_to_a_function_that_requires_a_table_2")
{
    CheckResult result = check(R"(
        local a: {y: number} | {x: number, y: number, [any]: any}

        function f(t)
            t.y = 1
            return t
        end

        local b = f(a)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    REQUIRE_EQ("{| [any]: any, x: number, y: number |} | {| y: number |}", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "unifying_tables_shouldnt_uaf1")
{
    CheckResult result = check(R"(
-- This example produced a UAF at one point, caused by pointers to table types becoming
-- invalidated by child unifiers. (Calling log.concat can cause pointers to become invalid.)
type _Entry = {
    a: number,

    middle: (self: _Entry) -> (),

    z: number
}

export type AnyEntry = _Entry

local Entry = {}
Entry.__index = Entry

function Entry:dispose()
    self:middle()
    forgetChildren(self) -- unify free with sealed AnyEntry
end

function forgetChildren(parent: AnyEntry)
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "unifying_tables_shouldnt_uaf2")
{
    CheckResult result = check(R"(
-- Another example that UAFd, this time found by fuzzing.
local _
do
_._ *= (_[{n0=_[{[{[_]=_,}]=_,}],}])[_]
_ = (_.n0)
end
_._ *= (_[false])[_]
_ = (_.cos)
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "cannot_call_tables")
{
    CheckResult result = check("local foo = {}    foo()");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK(get<CannotCallNonFunction>(result.errors[0]) != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "table_length")
{
    CheckResult result = check(R"(
        local t = {}
        local s = #t
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(nullptr != get<TableType>(requireType("t")));
    CHECK_EQ(*builtinTypes->numberType, *requireType("s"));
}

TEST_CASE_FIXTURE(Fixture, "nil_assign_doesnt_hit_indexer")
{
    CheckResult result = check("local a = {} a[0] = 7  a[0] = nil");
    LUAU_REQUIRE_ERROR_COUNT(0, result);
}

TEST_CASE_FIXTURE(Fixture, "wrong_assign_does_hit_indexer")
{
    CheckResult result = check(R"(
        local a = {}
        a[0] = 7
        a[0] = 't'
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK((Location{Position{3, 15}, Position{3, 18}}) == result.errors[0].location);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(tm->wantedType == builtinTypes->numberType);
    CHECK(tm->givenType == builtinTypes->stringType);
}

TEST_CASE_FIXTURE(Fixture, "nil_assign_doesnt_hit_no_indexer")
{
    CheckResult result = check(R"(
        local a = {a=1, b=2}
        a['a'] = nil
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(result.errors[0], (TypeError{Location{Position{2, 17}, Position{2, 20}}, TypeMismatch{
                                                                                          builtinTypes->numberType,
                                                                                          builtinTypes->nilType,
                                                                                      }}));
}

TEST_CASE_FIXTURE(Fixture, "free_rhs_table_can_also_be_bound")
{
    check(R"(
        local o
        local v = o:i()

        function g(u)
            v = u
        end

        o:f(g)
        o:h()
        o:h()
    )");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_unifies_into_map")
{
    CheckResult result = check(R"(
        local Instance: any
        local UDim2: any

        function Create(instanceType)
            return function(data)
                local obj = Instance.new(instanceType)
                for k, v in pairs(data) do
                    if type(k) == 'number' then
                        --v.Parent = obj
                    else
                        obj[k] = v
                    end
                end
                return obj
            end
        end

        local topbarShadow = Create'ImageLabel'{
            Name = "TopBarShadow";
            Size = UDim2.new(1, 0, 0, 3);
            Position = UDim2.new(0, 0, 1, 0);
            Image = "rbxasset://textures/ui/TopBar/dropshadow.png";
            BackgroundTransparency = 1;
            Active = false;
            Visible = false;
        };

    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "tables_get_names_from_their_locals")
{
    CheckResult result = check(R"(
        local T = {}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("T", toString(requireType("T")));
}

TEST_CASE_FIXTURE(Fixture, "generalize_table_argument")
{
    CheckResult result = check(R"(
        function foo(arr)
            local work = {}
            for i = 1, #arr do
                work[i] = arr[i]
            end

            return arr
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);

    const FunctionType* fooType = get<FunctionType>(requireType("foo"));
    REQUIRE(fooType);

    std::optional<TypeId> fooArg1 = first(fooType->argTypes);
    REQUIRE(fooArg1);

    const TableType* fooArg1Table = get<TableType>(*fooArg1);
    REQUIRE(fooArg1Table);

    CHECK_EQ(fooArg1Table->state, TableState::Generic);
}

/*
 * This test case exposed an oversight in the treatment of free tables.
 * Free tables, like free Types, need to record the scope depth where they were created so that
 * we do not erroneously let-generalize them when they are used in a nested lambda.
 *
 * For more information about let-generalization, see <http://okmij.org/ftp/ML/generalization.html>
 *
 * The important idea here is that the return type of Counter.new is a table with some metatable.
 * That metatable *must* be the same Type as the type of Counter.  If it is a copy (produced by
 * the generalization process), then it loses the knowledge that its metatable will have an :incr()
 * method.
 */
TEST_CASE_FIXTURE(BuiltinsFixture, "dont_quantify_table_that_belongs_to_outer_scope")
{
    CheckResult result = check(R"(
        local Counter = {}
        Counter.__index = Counter

        function Counter.new()
            local self = setmetatable({count=0}, Counter)
            return self
        end

        function Counter:incr()
            self.count = 1
            return self.count
        end

        local self = Counter.new()
        print(self:incr())
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TableType* counterType = getMutable<TableType>(requireType("Counter"));
    REQUIRE(counterType);

    REQUIRE(counterType->props.count("new"));
    const FunctionType* newType = get<FunctionType>(follow(counterType->props["new"].type));
    REQUIRE(newType);

    std::optional<TypeId> newRetType = *first(newType->retTypes);
    REQUIRE(newRetType);

    const MetatableType* newRet = get<MetatableType>(follow(*newRetType));
    REQUIRE(newRet);

    const TableType* newRetMeta = get<TableType>(follow(newRet->metatable));
    REQUIRE(newRetMeta);

    CHECK(newRetMeta->props.count("incr"));
    CHECK_EQ(follow(newRet->metatable), follow(requireType("Counter")));
}

// TODO: CLI-39624
TEST_CASE_FIXTURE(BuiltinsFixture, "instantiate_tables_at_scope_level")
{
    CheckResult result = check(R"(
        --!strict
        local Option = {}
        Option.__index = Option
        function Option.Is(obj)
                return (type(obj) == "table" and getmetatable(obj) == Option)
        end
        return Option
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "inferring_crazy_table_should_also_be_quick")
{
    CheckResult result = check(R"(
        --!strict
        function f(U)
            U(w:s(an):c()():c():U(s):c():c():U(s):c():U(s):cU()):c():U(s):c():U(s):c():c():U(s):c():U(s):cU()
        end
    )");

    ModulePtr module = getMainModule();
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_GE(500, module->internalTypes.types.size());
    else
        CHECK_GE(100, module->internalTypes.types.size());
}

TEST_CASE_FIXTURE(Fixture, "MixedPropertiesAndIndexers")
{
    CheckResult result = check(R"(
local x = {}
x.a = "a"
x[0] = true
x.b = 37
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "setmetatable_cant_be_used_to_mutate_global_types")
{
    {
        Fixture fix;

        // inherit env from parent fixture checker
        fix.frontend.globals.globalScope = frontend.globals.globalScope;

        fix.check(R"(
--!nonstrict
type MT = typeof(setmetatable)
function wtf(arg: {MT}): typeof(table)
    arg = wtf(arg)
end
)");
    }

    // validate sharedEnv post-typecheck; valuable for debugging some typeck crashes but slows fuzzing down
    // note: it's important for typeck to be destroyed at this point!
    {
        for (auto& p : frontend.globals.globalScope->bindings)
        {
            toString(p.second.typeId); // toString walks the entire type, making sure ASAN catches access to destroyed type arenas
        }
    }
}

TEST_CASE_FIXTURE(Fixture, "evil_table_unification")
{
    // this code re-infers the type of _ while processing fields of _, which can cause use-after-free
    check(R"(
--!nonstrict
_ = ...
_:table(_,string)[_:gsub(_,...,n0)],_,_:gsub(_,string)[""],_:split(_,...,table)._,n0 = nil
do end
)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "dont_crash_when_setmetatable_does_not_produce_a_metatabletypevar")
{
    CheckResult result = check("local x = setmetatable({})");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Argument count mismatch. Function 'setmetatable' expects 2 arguments, but only 1 is specified", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "instantiate_table_cloning")
{
    CheckResult result = check(R"(
--!nonstrict
local l0:any,l61:t0<t32> = _,math
while _ do
_()
end
function _():t0<t0>
end
type t0<t32> = any
)");

    std::optional<TypeId> ty = requireType("math");
    REQUIRE(ty);

    const TableType* ttv = get<TableType>(*ty);
    REQUIRE(ttv);
    CHECK(ttv->instantiatedTypeParams.empty());
}

TEST_CASE_FIXTURE(BuiltinsFixture, "instantiate_table_cloning_2")
{
    CheckResult result = check(R"(
type X<T> = T
type K = X<typeof(math)>
)");

    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> ty = requireType("math");
    REQUIRE(ty);

    const TableType* ttv = get<TableType>(*ty);
    REQUIRE(ttv);
    CHECK(ttv->instantiatedTypeParams.empty());
}

TEST_CASE_FIXTURE(Fixture, "instantiate_table_cloning_3")
{
    CheckResult result = check(R"(
type X<T> = T
local a = {}
a.x = 4
local b: X<typeof(a)>
a.y = 5
local c: X<typeof(a)>
c = b
)");

    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> ty = requireType("a");
    REQUIRE(ty);

    const TableType* ttv = get<TableType>(*ty);
    REQUIRE(ttv);
    CHECK(ttv->instantiatedTypeParams.empty());
}

TEST_CASE_FIXTURE(Fixture, "table_indexing_error_location")
{
    CheckResult result = check(R"(
local foo = {42}
local bar: number?
local baz = foo[bar]
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(result.errors[0].location, Location{Position{3, 16}, Position{3, 19}});
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_call_metamethod_basic")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local a = setmetatable({
            a = 1,
        }, {
            __call = function(self, b: number)
                return self.a * b
            end,
        })

        local foo = a(12)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(requireType("foo") == builtinTypes->numberType);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_call_metamethod_must_be_callable")
{
    CheckResult result = check(R"(
        local a = setmetatable({}, {
            __call = 123,
        })

        local foo = a()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(result.errors[0] == TypeError{
                                  Location{{5, 20}, {5, 21}},
                                  CannotCallNonFunction{builtinTypes->numberType},
                              });
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_call_metamethod_generic")
{
    CheckResult result = check(R"(
        local a = setmetatable({}, {
            __call = function<T>(self, b: T)
                return b
            end,
        })

        local foo = a(12)
        local bar = a("bar")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(requireType("foo") == builtinTypes->numberType);
    CHECK(requireType("bar") == builtinTypes->stringType);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_simple_call")
{
    CheckResult result = check(R"(
local a = setmetatable({ x = 2 }, {
    __call = function(self)
        return (self.x :: number) * 2 -- should work without annotation in the future
    end
})
local b = a()
local c = a(2) -- too many arguments
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Argument count mismatch. Function 'a' expects 1 argument, but 2 are specified", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "access_index_metamethod_that_returns_variadic")
{
    CheckResult result = check(R"(
        type Foo = {x: string}
        local t = {}
        setmetatable(t, {
            __index = function(x: string): ...Foo
                return {x = x}
            end
        })

        local foo = t.bar
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions o;
    o.exhaustive = true;
    CHECK_EQ("{| x: string |}", toString(requireType("foo"), o));
}

TEST_CASE_FIXTURE(Fixture, "dont_invalidate_the_properties_iterator_of_free_table_when_rolled_back")
{
    fileResolver.source["Module/Backend/Types"] = R"(
        export type Fiber = {
            return_: Fiber?
        }
        return {}
    )";

    fileResolver.source["Module/Backend"] = R"(
        local Types = require(script.Types)
        type Fiber = Types.Fiber
        type ReactRenderer = { findFiberByHostInstance: () -> Fiber? }

        local function attach(renderer): ()
            local function getPrimaryFiber(fiber)
                local alternate = fiber.alternate
                return fiber
            end

            local function getFiberIDForNative()
                local fiber = renderer.findFiberByHostInstance()
                fiber = fiber.return_
                return getPrimaryFiber(fiber)
            end
        end

        function culprit(renderer: ReactRenderer): ()
            attach(renderer)
        end

        return culprit
    )";

    CheckResult result = frontend.check("Module/Backend");
}

TEST_CASE_FIXTURE(Fixture, "checked_prop_too_early")
{
    CheckResult result = check(R"(
        local t: {x: number?}? = {x = nil}
        local u = t.x and t or 5
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Value of type '{| x: number? |}?' could be nil", toString(result.errors[0]));
    CHECK_EQ("number | {| x: number? |}", toString(requireType("u")));
}

TEST_CASE_FIXTURE(Fixture, "accidentally_checked_prop_in_opposite_branch")
{
    CheckResult result = check(R"(
        local t: {x: number?}? = {x = nil}
        local u = t and t.x == 5 or t.x == 31337
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Value of type '{| x: number? |}?' could be nil", toString(result.errors[0]));
    CHECK_EQ("boolean", toString(requireType("u")));
}

/*
 * We had an issue where part of the type of pairs() was an unsealed table.
 * This test depends on FFlagDebugLuauFreezeArena to trigger it.
 */
TEST_CASE_FIXTURE(Fixture, "pairs_parameters_are_not_unsealed_tables")
{
    check(R"(
        function _(l0:{n0:any})
            _ = pairs
        end
    )");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_function_check_use_after_free")
{
    CheckResult result = check(R"(
local t = {}

function t.x(value)
    for k,v in pairs(t) do end
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

/*
 * When we add new properties to an unsealed table, we should do a level check and promote the property type to be at
 * the level of the table.
 */
TEST_CASE_FIXTURE(Fixture, "inferred_properties_of_a_table_should_start_with_the_same_TypeLevel_of_that_table")
{
    CheckResult result = check(R"(
        --!strict
        local T = {}

        local function f(prop)
            T[1] = {
                prop = prop,
            }
        end

        local function g()
            local l = T[1].prop
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// The real bug here was that we weren't always uncondionally typechecking a trailing return statement last.
TEST_CASE_FIXTURE(BuiltinsFixture, "dont_leak_free_table_props")
{
    CheckResult result = check(R"(
        local function a(state)
            print(state.blah)
        end

        local function b(state) -- The bug was that we inferred state: {blah: any, gwar: any}
            print(state.gwar)
        end

        return function()
            return function(state)
                a(state)
                b(state)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("<a>({+ blah: a +}) -> ()", toString(requireType("a")));
    CHECK_EQ("<a>({+ gwar: a +}) -> ()", toString(requireType("b")));
    CHECK_EQ("() -> <a, b>({+ blah: a, gwar: b +}) -> ()", toString(getMainModule()->returnType));
}

TEST_CASE_FIXTURE(Fixture, "inferred_return_type_of_free_table")
{
    ScopedFastFlag sff[] = {
        // {"LuauLowerBoundsCalculation", true},
        {"DebugLuauSharedSelf", true},
    };

    check(R"(
        function Base64FileReader(data)
            local reader = {}
            local index: number

            function reader:PeekByte()
                return data:byte(index)
            end

            function reader:Byte()
                return data:byte(index - 1)
            end

            return reader
        end
    )");

    CHECK_EQ("<a, b...>(t1) -> {| Byte: (a) -> (b...), PeekByte: (a) -> (b...) |} where t1 = {+ byte: (t1, number) -> (b...) +}",
        toString(requireType("Base64FileReader")));
}

TEST_CASE_FIXTURE(Fixture, "mixed_tables_with_implicit_numbered_keys")
{
    CheckResult result = check(R"(
        local t: { [string]: number } = { 5, 6, 7 }
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);

    CHECK_EQ("Type 'number' could not be converted into 'string'", toString(result.errors[0]));
    CHECK_EQ("Type 'number' could not be converted into 'string'", toString(result.errors[1]));
    CHECK_EQ("Type 'number' could not be converted into 'string'", toString(result.errors[2]));
}

TEST_CASE_FIXTURE(Fixture, "shared_selfs")
{
    ScopedFastFlag sff{"DebugLuauSharedSelf", true};

    CheckResult result = check(R"(
        local t = {}
        t.x = 5

        function t:m1() return self.x end
        function t:m2() return self.y end

        return t
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions opts;
    opts.exhaustive = true;
    CHECK_EQ("{| m1: <a, b>({+ x: a, y: b +}) -> a, m2: <a, b>({+ x: a, y: b +}) -> b, x: number |}", toString(requireType("t"), opts));
}

TEST_CASE_FIXTURE(Fixture, "shared_selfs_from_free_param")
{
    ScopedFastFlag sff{"DebugLuauSharedSelf", true};

    CheckResult result = check(R"(
        local function f(t)
            function t:m1() return self.x end
            function t:m2() return self.y end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("<a, b>({+ m1: ({+ x: a, y: b +}) -> a, m2: ({+ x: a, y: b +}) -> b +}) -> ()", toString(requireType("f")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "shared_selfs_through_metatables")
{
    ScopedFastFlag sff{"DebugLuauSharedSelf", true};

    CheckResult result = check(R"(
        local t = {}
        t.__index = t
        setmetatable({}, t)

        function t:m1() return self.x end
        function t:m2() return self.y end

        return t
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions opts;
    opts.exhaustive = true;
    CHECK_EQ(
        toString(requireType("t"), opts), "t1 where t1 = {| __index: t1, m1: <a, b>({+ x: a, y: b +}) -> a, m2: <a, b>({+ x: a, y: b +}) -> b |}");
}

TEST_CASE_FIXTURE(Fixture, "expected_indexer_value_type_extra")
{
    CheckResult result = check(R"(
        type X = { { x: boolean?, y: boolean? } }

        local l1: {[string]: X} = { key = { { x = true }, { y = true } } }
        local l2: {[any]: X} = { key = { { x = true }, { y = true } } }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "expected_indexer_value_type_extra_2")
{
    CheckResult result = check(R"(
        type X = {[any]: string | boolean}

        local x: X = { key = "str" }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "expected_indexer_from_table_union")
{
    LUAU_REQUIRE_NO_ERRORS(check(R"(local a: {[string]: {number | string}} = {a = {2, 's'}})"));
    LUAU_REQUIRE_NO_ERRORS(check(R"(local a: {[string]: {number | string}}? = {a = {2, 's'}})"));
    LUAU_REQUIRE_NO_ERRORS(check(R"(local a: {[string]: {[string]: {string?}}?} = {["a"] = {["b"] = {"a", "b"}}})"));
}

TEST_CASE_FIXTURE(Fixture, "prop_access_on_key_whose_types_mismatches")
{
    CheckResult result = check(R"(
        local t: {number} = {}
        local x = t.x
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Key 'x' not found in table '{number}'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "prop_access_on_unions_of_indexers_where_key_whose_types_mismatches")
{
    CheckResult result = check(R"(
        local t: { [number]: number } | { [boolean]: number } = {}
        local u = t.x
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type '{number} | {| [boolean]: number |}' does not have key 'x'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "quantify_metatables_of_metatables_of_table")
{
    ScopedFastFlag sff[]{
        {"DebugLuauSharedSelf", true},
    };

    CheckResult result = check(R"(
        local T = {}

        function T:m()
            return self.x, self.y
        end

        function T:n()
        end

        local U = setmetatable({}, {__index = T})

        local V = setmetatable({}, {__index = U})

        return V
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions opts;
    opts.exhaustive = true;
    CHECK_EQ(toString(requireType("V"), opts), "{ @metatable { __index: { @metatable { __index: {| m: <a, b>({+ x: a, y: b +}) -> (a, b), n: <a, "
                                               "b>({+ x: a, y: b +}) -> () |} }, {  } } }, {  } }");
}

TEST_CASE_FIXTURE(Fixture, "quantify_even_that_table_was_never_exported_at_all")
{
    ScopedFastFlag sff{"DebugLuauSharedSelf", true};

    CheckResult result = check(R"(
        local T = {}

        function T:m()
            return self.x
        end

        function T:n()
            return self.y
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions opts;
    opts.exhaustive = true;
    CHECK_EQ("{| m: <a, b>({+ x: a, y: b +}) -> a, n: <a, b>({+ x: a, y: b +}) -> b |}", toString(requireType("T"), opts));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "leaking_bad_metatable_errors")
{
    CheckResult result = check(R"(
local a = setmetatable({}, 1)
local b = a.x
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ("Metatable was not a table", toString(result.errors[0]));
    CHECK_EQ("Type 'a' does not have key 'x'", toString(result.errors[1]));
}

TEST_CASE_FIXTURE(Fixture, "scalar_is_a_subtype_of_a_compatible_polymorphic_shape_type")
{
    CheckResult result = check(R"(
        local function f(s)
            return s:lower()
        end

        f("foo" :: string)
        f("bar" :: "bar")
        f("baz" :: "bar" | "baz")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "scalar_is_not_a_subtype_of_a_compatible_polymorphic_shape_type")
{
    CheckResult result = check(R"(
        local function f(s)
            return s:absolutely_no_scalar_has_this_method()
        end

        f("foo" :: string)
        f("bar" :: "bar")
        f("baz" :: "bar" | "baz")
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);

    CHECK_EQ(R"(Type 'string' could not be converted into 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}'
caused by:
  The former's metatable does not satisfy the requirements. Table type 'typeof(string)' not compatible with type 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}' because the former is missing field 'absolutely_no_scalar_has_this_method')",
        toString(result.errors[0]));
    CHECK_EQ(R"(Type '"bar"' could not be converted into 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}'
caused by:
  The former's metatable does not satisfy the requirements. Table type 'typeof(string)' not compatible with type 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}' because the former is missing field 'absolutely_no_scalar_has_this_method')",
        toString(result.errors[1]));
    CHECK_EQ(R"(Type '"bar" | "baz"' could not be converted into 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}'
caused by:
  Not all union options are compatible. Type '"bar"' could not be converted into 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}'
caused by:
  The former's metatable does not satisfy the requirements. Table type 'typeof(string)' not compatible with type 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}' because the former is missing field 'absolutely_no_scalar_has_this_method')",
        toString(result.errors[2]));
}

TEST_CASE_FIXTURE(Fixture, "a_free_shape_can_turn_into_a_scalar_if_it_is_compatible")
{
    CheckResult result = check(R"(
        local function f(s): string
            local foo = s:lower()
            return s
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("(string) -> string", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "a_free_shape_cannot_turn_into_a_scalar_if_it_is_not_compatible")
{
    CheckResult result = check(R"(
        local function f(s): string
            local foo = s:absolutely_no_scalar_has_this_method()
            return s
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(R"(Type 't1 where t1 = {+ absolutely_no_scalar_has_this_method: (t1) -> (a, b...) +}' could not be converted into 'string'
caused by:
  The former's metatable does not satisfy the requirements. Table type 'typeof(string)' not compatible with type 't1 where t1 = {+ absolutely_no_scalar_has_this_method: (t1) -> (a, b...) +}' because the former is missing field 'absolutely_no_scalar_has_this_method')",
        toString(result.errors[0]));
    CHECK_EQ("<a, b...>(t1) -> string where t1 = {+ absolutely_no_scalar_has_this_method: (t1) -> (a, b...) +}", toString(requireType("f")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "a_free_shape_can_turn_into_a_scalar_directly")
{
    CheckResult result = check(R"(
        local function stringByteList(str)
            local out = {}
            for i = 1, #str do
                table.insert(out, string.byte(str, i))
            end
            return table.concat(out, ",")
        end

        local x = stringByteList("xoo")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "invariant_table_properties_means_instantiating_tables_in_call_is_unsound")
{
    ScopedFastFlag sff[]{
        {"LuauInstantiateInSubtyping", true},
    };

    CheckResult result = check(R"(
        --!strict
        local t = {}
        function t.m(x) return x end
        local a : string = t.m("hi")
        local b : number = t.m(5)
        function f(x : { m : (number)->number })
            x.m = function(x) return 1+x end
        end
        f(t) -- This shouldn't typecheck
        local c : string = t.m("hi")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    // TODO: test behavior is wrong until we can re-enable the covariant requirement for instantiation in subtyping
    //     LUAU_REQUIRE_ERRORS(result);
    //     CHECK_EQ(toString(result.errors[0]), R"(Type 't' could not be converted into '{| m: (number) -> number |}'
    // caused by:
    //   Property 'm' is not compatible. Type '<a>(a) -> a' could not be converted into '(number) -> number'; different number of generic type
    //   parameters)");
    //     // this error message is not great since the underlying issue is that the context is invariant,
    // and `(number) -> number` cannot be a subtype of `<a>(a) -> a`.
}


TEST_CASE_FIXTURE(BuiltinsFixture, "generic_table_instantiation_potential_regression")
{
    CheckResult result = check(R"(
--!strict

function f(x)
  x.p = 5
  return x
end
local g : ({ p : number, q : string }) -> ({ p : number, r : boolean }) = f
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    MissingProperties* error = get<MissingProperties>(result.errors[0]);
    REQUIRE(error != nullptr);
    REQUIRE(error->properties.size() == 1);

    CHECK_EQ("r", error->properties[0]);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "setmetatable_has_a_side_effect")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local mt = {
            __add = function(x, y)
                return 123
            end,
        }

        local foo = {}
        setmetatable(foo, mt)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireType("foo")) == "{ @metatable mt, foo }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "tables_should_be_fully_populated")
{
    CheckResult result = check(R"(
        local t = {
            x = 5 :: NonexistingTypeWhichEndsUpReturningAnErrorType,
            y = 5
        }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    ToStringOptions opts;
    opts.exhaustive = true;
    CHECK_EQ("{ x: *error-type*, y: number }", toString(requireType("t"), opts));
}

TEST_CASE_FIXTURE(Fixture, "fuzz_table_indexer_unification_can_bound_owner_to_string")
{
    CheckResult result = check(R"(
sin,_ = nil
_ = _[_.sin][_._][_][_]._
_[_] = _
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzz_table_extra_prop_unification_can_bound_owner_to_string")
{
    CheckResult result = check(R"(
l0,_ = nil
_ = _,_[_.n5]._[_][_][_]._
_._.foreach[_],_ = _[_],_._
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzz_typelevel_promote_on_changed_table_type")
{
    CheckResult result = check(R"(
_._,_ = nil
_ = _.foreach[_]._,_[_.n5]._[_.foreach][_][_]._
_ = _._
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzz_table_unify_instantiated_table")
{
    ScopedFastFlag sff[]{
        {"LuauInstantiateInSubtyping", true},
    };

    CheckResult result = check(R"(
function _(...)
end
local function l0():typeof(_()()[_()()[_]])
end
return _[_()()[_]] <= _
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "fuzz_table_unify_instantiated_table_with_prop_realloc")
{
    ScopedFastFlag sff[]{
        {"LuauInstantiateInSubtyping", true},
    };

    CheckResult result = check(R"(
function _(l0,l0)
do
_ = _().n0
end
l0(_()._,_)
end
_(_,function(...)
end)
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzz_table_unify_prop_realloc")
{
    CheckResult result = check(R"(
n3,_ = nil
_ = _[""]._,_[l0][_._][{[_]=_,_=_,}][_G].number
_ = {_,}
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "when_augmenting_an_unsealed_table_with_an_indexer_apply_the_correct_scope_to_the_indexer_type")
{
    CheckResult result = check(R"(
        local events = {}
        local mockObserveEvent = function(_, key, callback)
            events[key] = callback
        end

        events['FriendshipNotifications']({
            EventArgs = {
                UserId2 = '2'
            },
            Type = 'FriendshipDeclined'
        })
    )");

    TypeId ty = follow(requireType("events"));
    const TableType* tt = get<TableType>(ty);
    REQUIRE_MESSAGE(tt, "Expected table but got " << toString(ty, {true}));

    CHECK(tt->props.empty());
    REQUIRE(tt->indexer);

    CHECK("string" == toString(tt->indexer->indexType));

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "dont_extend_unsealed_tables_in_rvalue_position")
{
    CheckResult result = check(R"(
        local testDictionary = {
            FruitName = "Lemon",
            FruitColor = "Yellow",
            Sour = true
        }

        local print: any

        print(testDictionary[""])
    )");

    TypeId ty = follow(requireType("testDictionary"));
    const TableType* ttv = get<TableType>(ty);
    REQUIRE(ttv);

    CHECK(0 == ttv->props.count(""));

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "extend_unsealed_table_with_metatable")
{
    CheckResult result = check(R"(
        local T = setmetatable({}, {
            __call = function(_, name: string?)
            end,
        })

        T.for_ = "for_"

        return T
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "top_table_type_is_isomorphic_to_empty_sealed_table_type")
{
    CheckResult result = check(R"(
        local None = newproxy(true)
        local mt = getmetatable(None)
        mt.__tostring = function()
            return "Object.None"
        end

        function assign(...)
            for index = 1, select("#", ...) do
                local rest = select(index, ...)

                if rest ~= nil and typeof(rest) == "table" then
                    for key, value in pairs(rest) do
                    end
                end
            end
        end
    )");
}

TEST_SUITE_END();
