// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Parser.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"

#include "Fixture.h"

#include "doctest.h"

#include <algorithm>

using namespace Luau;

TEST_SUITE_BEGIN("TableTests");

TEST_CASE_FIXTURE(Fixture, "basic")
{
    CheckResult result = check("local t = {foo = \"bar\", baz = 9, quux = nil}");
    LUAU_REQUIRE_NO_ERRORS(result);

    const TableTypeVar* tType = get<TableTypeVar>(requireType("t"));
    REQUIRE(tType != nullptr);

    std::optional<Property> fooProp = get(tType->props, "foo");
    REQUIRE(bool(fooProp));
    CHECK_EQ(PrimitiveTypeVar::String, getPrimitiveType(fooProp->type));

    std::optional<Property> bazProp = get(tType->props, "baz");
    REQUIRE(bool(bazProp));
    CHECK_EQ(PrimitiveTypeVar::Number, getPrimitiveType(bazProp->type));

    std::optional<Property> quuxProp = get(tType->props, "quux");
    REQUIRE(bool(quuxProp));
    CHECK_EQ(PrimitiveTypeVar::NilType, getPrimitiveType(quuxProp->type));
}

TEST_CASE_FIXTURE(Fixture, "augment_table")
{
    CheckResult result = check("local t = {}  t.foo = 'bar'");
    LUAU_REQUIRE_NO_ERRORS(result);

    const TableTypeVar* tType = get<TableTypeVar>(requireType("t"));
    REQUIRE(tType != nullptr);

    CHECK(tType->props.find("foo") != tType->props.end());
}

TEST_CASE_FIXTURE(Fixture, "augment_nested_table")
{
    CheckResult result = check("local t = { p = {} }  t.p.foo = 'bar'");
    LUAU_REQUIRE_NO_ERRORS(result);

    TableTypeVar* tType = getMutable<TableTypeVar>(requireType("t"));
    REQUIRE(tType != nullptr);

    REQUIRE(tType->props.find("p") != tType->props.end());
    const TableTypeVar* pType = get<TableTypeVar>(tType->props["p"].type);
    REQUIRE(pType != nullptr);

    CHECK(pType->props.find("foo") != pType->props.end());
}

TEST_CASE_FIXTURE(Fixture, "cannot_augment_sealed_table")
{
    CheckResult result = check("function mkt() return {prop=999} end    local t = mkt()    t.foo = 'bar'");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeError& err = result.errors[0];
    CannotExtendTable* error = get<CannotExtendTable>(err);
    REQUIRE(error != nullptr);

    // TODO: better, more robust comparison of type vars
    auto s = toString(error->tableType, ToStringOptions{/*exhaustive*/ true});
    CHECK_EQ(s, "{| prop: number |}");
    CHECK_EQ(error->prop, "foo");
    CHECK_EQ(error->context, CannotExtendTable::Property);
    CHECK_EQ(err.location, (Location{Position{0, 59}, Position{0, 64}}));
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
    CheckResult result = check("local t = {}    t.prop = 999    t.prop = 'hello'");
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

    const TableTypeVar* tableType = get<TableTypeVar>(requireType("T"));
    REQUIRE(tableType != nullptr);

    std::optional<Property> fooProp = get(tableType->props, "foo");
    REQUIRE(bool(fooProp));

    const FunctionTypeVar* methodType = get<FunctionTypeVar>(follow(fooProp->type));
    REQUIRE(methodType != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "tc_member_function_2")
{
    CheckResult result = check("local T = {U={}}  function T.U:foo() return 5 end");
    LUAU_REQUIRE_NO_ERRORS(result);

    const TableTypeVar* tableType = get<TableTypeVar>(requireType("T"));
    REQUIRE(tableType != nullptr);

    std::optional<Property> uProp = get(tableType->props, "U");
    REQUIRE(bool(uProp));
    TypeId uType = uProp->type;

    const TableTypeVar* uTable = get<TableTypeVar>(uType);
    REQUIRE(uTable != nullptr);

    std::optional<Property> fooProp = get(uTable->props, "foo");
    REQUIRE(bool(fooProp));

    const FunctionTypeVar* methodType = get<FunctionTypeVar>(follow(fooProp->type));
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

    CHECK_EQ(*typeChecker.numberType, *requireType("a"));
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

TEST_CASE_FIXTURE(Fixture, "used_colon_correctly")
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
    ScopedFastFlag sff{"LuauTableSubtypingVariance2", true};

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
    const FunctionTypeVar* fooFn = get<FunctionTypeVar>(fooType);
    REQUIRE(fooFn != nullptr);

    std::vector<TypeId> fooArgs = flatten(fooFn->argTypes).first;

    REQUIRE_EQ(1, fooArgs.size());

    TypeId arg0 = fooArgs[0];
    const TableTypeVar* arg0Table = get<TableTypeVar>(follow(arg0));
    REQUIRE(arg0Table != nullptr);

    REQUIRE(arg0Table->props.find("bar") != arg0Table->props.end());
    REQUIRE(arg0Table->props.find("baz") != arg0Table->props.end());
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
    ScopedFastFlag sff{"LuauTableSubtypingVariance2", true};

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
    ScopedFastFlag sff{"LuauTableSubtypingVariance2", true};

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
    const TableTypeVar* qTable = get<TableTypeVar>(qType);
    REQUIRE(qType != nullptr);

    CHECK(qTable->props.find("x") != qTable->props.end());
    CHECK(qTable->props.find("y") != qTable->props.end());
    CHECK(qTable->props.find("z") == qTable->props.end());
    CHECK(qTable->props.find("w") != qTable->props.end());

    TypeId wType = requireType("w");
    const TableTypeVar* wTable = get<TableTypeVar>(wType);
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
    ScopedFastFlag sff{"LuauTableSubtypingVariance2", true};

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

    // Currently this errors but it shouldn't, since set only needs write access
    // TODO: file a JIRA for this
    LUAU_REQUIRE_ERRORS(result);
    // CHECK_EQ("number?", toString(requireType("x")));
}

TEST_CASE_FIXTURE(Fixture, "width_subtyping")
{
    ScopedFastFlag sff{"LuauTableSubtypingVariance2", true};

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

    const TableTypeVar* ttv = get<TableTypeVar>(requireType("t"));
    REQUIRE(ttv != nullptr);

    REQUIRE(bool(ttv->indexer));

    CHECK_EQ(*ttv->indexer->indexType, *typeChecker.numberType);
    CHECK_EQ(*ttv->indexer->indexResultType, *typeChecker.stringType);
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

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(requireType("swap"));
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> argVec = flatten(ftv->argTypes).first;

    REQUIRE_EQ(1, argVec.size());

    const TableTypeVar* ttv = get<TableTypeVar>(follow(argVec[0]));
    REQUIRE(ttv != nullptr);

    REQUIRE(bool(ttv->indexer));

    const TableIndexer& indexer = *ttv->indexer;

    REQUIRE_EQ(indexer.indexType, typeChecker.numberType);

    REQUIRE(nullptr != get<GenericTypeVar>(follow(indexer.indexResultType)));
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

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(requireType("mergesort"));
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> argVec = flatten(ftv->argTypes).first;

    REQUIRE_EQ(1, argVec.size());

    const TableTypeVar* argType = get<TableTypeVar>(follow(argVec[0]));
    REQUIRE(argType != nullptr);

    std::vector<TypeId> retVec = flatten(ftv->retType).first;

    const TableTypeVar* retType = get<TableTypeVar>(follow(retVec[0]));
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

    const TableTypeVar* ttv = get<TableTypeVar>(requireType("t"));
    REQUIRE(ttv != nullptr);

    REQUIRE(bool(ttv->indexer));
    const TableIndexer& indexer = *ttv->indexer;

    CHECK_EQ(*typeChecker.numberType, *indexer.indexType);
    CHECK_EQ(*typeChecker.stringType, *indexer.indexResultType);
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

    const FunctionTypeVar* fType = get<FunctionTypeVar>(requireType("f"));
    REQUIRE(fType != nullptr);

    auto retType_ = first(fType->retType);
    REQUIRE(bool(retType_));

    auto retType = get<TableTypeVar>(follow(*retType_));
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

    const TableTypeVar* tTy = get<TableTypeVar>(requireType("t2"));
    REQUIRE(tTy != nullptr);

    REQUIRE(tTy->indexer);
    CHECK_EQ(*typeChecker.numberType, *tTy->indexer->indexType);
    CHECK_EQ(*typeChecker.stringType, *tTy->indexer->indexResultType);
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
    CHECK_EQ(tm->wantedType, t2);
    CHECK_EQ(tm->givenType, t1);

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
    ScopedFastFlag sff{"LuauTableSubtypingVariance2", true};

    CheckResult result = check(R"(
        local t: { a: string, [number]: string } = { a = "foo" }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "array_factory_function")
{
    ScopedFastFlag sff{"LuauTableSubtypingVariance2", true};

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

    CHECK_EQ(*typeChecker.stringType, *requireType("s"));
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

    CHECK_EQ(*typeChecker.stringType, *requireType("a1"));
    CHECK_EQ(*typeChecker.stringType, *requireType("a2"));

    CHECK_EQ(*typeChecker.numberType, *requireType("b1"));
    CHECK_EQ(*typeChecker.numberType, *requireType("b2"));

    CHECK_EQ(*typeChecker.numberType, *requireType("c"));

    CHECK_MESSAGE(nullptr != get<TypeMismatch>(result.errors[0]), "Expected a TypeMismatch but got " << result.errors[0]);
}

TEST_CASE_FIXTURE(Fixture, "assigning_to_an_unsealed_table_with_string_literal_should_infer_new_properties_over_indexer")
{
    CheckResult result = check(R"(
        local t = {}
        t["a"] = "foo"

        local a = t.a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.stringType, *requireType("a"));

    TableTypeVar* tableType = getMutable<TableTypeVar>(requireType("t"));
    REQUIRE(tableType != nullptr);
    REQUIRE(tableType->indexer == std::nullopt);

    TypeId propertyA = tableType->props["a"].type;
    REQUIRE(propertyA != nullptr);
    CHECK_EQ(*typeChecker.stringType, *propertyA);
}

TEST_CASE_FIXTURE(Fixture, "oop_indexer_works")
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

    CHECK_EQ(*typeChecker.stringType, *requireType("words"));
}

TEST_CASE_FIXTURE(Fixture, "indexer_table")
{
    CheckResult result = check(R"(
        local clazz = {a="hello"}
        local instanace = setmetatable({}, {__index=clazz})
        local b = instanace.a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.stringType, *requireType("b"));
}

TEST_CASE_FIXTURE(Fixture, "indexer_fn")
{
    CheckResult result = check(R"(
        local instanace = setmetatable({}, {__index=function() return 10 end})
        local b = instanace.somemethodwedonthave
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.numberType, *requireType("b"));
}

TEST_CASE_FIXTURE(Fixture, "meta_add")
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

TEST_CASE_FIXTURE(Fixture, "meta_add_inferred")
{
    CheckResult result = check(R"(
        local a = {}
        setmetatable(a, {__add=function(a,b) return b end} )
        local c = a + a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*requireType("a"), *requireType("c"));
}

TEST_CASE_FIXTURE(Fixture, "meta_add_both_ways")
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
TEST_CASE_FIXTURE(Fixture, "unification_of_unions_in_a_self_referential_type")
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

    const MetatableTypeVar* amtv = get<MetatableTypeVar>(requireType("a"));
    REQUIRE(amtv);
    CHECK_EQ(amtv->metatable, requireType("amt"));

    const MetatableTypeVar* bmtv = get<MetatableTypeVar>(requireType("b"));
    REQUIRE(bmtv);
    CHECK_EQ(bmtv->metatable, requireType("bmt"));
}

TEST_CASE_FIXTURE(Fixture, "oop_polymorphic")
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

    CHECK_EQ(*typeChecker.booleanType, *requireType("alive"));
    CHECK_EQ(*typeChecker.stringType, *requireType("movement"));
    CHECK_EQ(*typeChecker.stringType, *requireType("name"));
    CHECK_EQ(*typeChecker.numberType, *requireType("speed"));
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

TEST_CASE_FIXTURE(Fixture, "result_is_always_any_if_lhs_is_any")
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

TEST_CASE_FIXTURE(Fixture, "defining_a_method_for_a_builtin_sealed_table_must_fail")
{
    CheckResult result = check(R"(
        function string.m() end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "defining_a_self_method_for_a_builtin_sealed_table_must_fail")
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
    ScopedFastFlag sff{"LuauUnsealedTableLiteral", true};

    CheckResult result = check(R"(
        local t = {x = 1}
        function t.m() end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "defining_a_self_method_for_a_local_unsealed_table_is_ok")
{
    ScopedFastFlag sff{"LuauUnsealedTableLiteral", true};

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
        local a: A

        function f(t)
            t.y = 1
        end

        f(a)
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

TEST_CASE_FIXTURE(Fixture, "found_like_key_in_table_property_access")
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

TEST_CASE_FIXTURE(Fixture, "found_multiple_like_keys")
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

TEST_CASE_FIXTURE(Fixture, "dont_suggest_exact_match_keys")
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

TEST_CASE_FIXTURE(Fixture, "getmetatable_returns_pointer_to_metatable")
{
    CheckResult result = check(R"(
        local t = {x = 1}
        local mt = {__index = {y = 2}}
        setmetatable(t, mt)

        local returnedMT = getmetatable(t)
    )");

    CHECK_EQ(*requireType("mt"), *requireType("returnedMT"));
}

TEST_CASE_FIXTURE(Fixture, "metatable_mismatch_should_fail")
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

TEST_CASE_FIXTURE(Fixture, "property_lookup_through_tabletypevar_metatable")
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

TEST_CASE_FIXTURE(Fixture, "missing_metatable_for_sealed_tables_do_not_get_inferred")
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

    const MetatableTypeVar* aTy = get<MetatableTypeVar>(a);
    REQUIRE(aTy);

    const TableTypeVar* tTy = get<TableTypeVar>(t);
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
    ScopedFastFlag sff[]{
        {"LuauTableSubtypingVariance2", true},
        {"LuauUnsealedTableLiteral", true},
    };

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
    ScopedFastFlag sff[]{
        {"LuauTableSubtypingVariance2", true},
        {"LuauUnsealedTableLiteral", true},
    };

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

    // This typechecks but shouldn't
    LUAU_REQUIRE_NO_ERRORS(result);
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
    ScopedFastFlag sff{"LuauTableSubtypingVariance2", true};

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
    CHECK_EQ(requireType("t"), tm->wantedType);
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

TEST_CASE_FIXTURE(Fixture, "builtin_table_names")
{
    CheckResult result = check(R"(
        os.h = 2
        string.k = 3
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK_EQ("Cannot add property 'h' to table 'os'", toString(result.errors[0]));
    CHECK_EQ("Cannot add property 'k' to table 'string'", toString(result.errors[1]));
}

TEST_CASE_FIXTURE(Fixture, "persistent_sealed_table_is_immutable")
{
    CheckResult result = check(R"(
        --!nonstrict
        function os:bad() end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Cannot add property 'bad' to table 'os'", toString(result.errors[0]));

    const TableTypeVar* osType = get<TableTypeVar>(requireType("os"));
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

TEST_CASE_FIXTURE(Fixture, "quantifying_a_bound_var_works")
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
    TableTypeVar* ttv = getMutable<TableTypeVar>(ty);
    REQUIRE(ttv);
    Property& prop = ttv->props["new"];
    REQUIRE(prop.type);
    const FunctionTypeVar* ftv = get<FunctionTypeVar>(follow(prop.type));
    REQUIRE(ftv);
    const TypePack* res = get<TypePack>(follow(ftv->retType));
    REQUIRE(res);
    REQUIRE(res->head.size() == 1);
    const MetatableTypeVar* mtv = get<MetatableTypeVar>(follow(res->head[0]));
    REQUIRE(mtv);
    ttv = getMutable<TableTypeVar>(follow(mtv->table));
    REQUIRE(ttv);
    REQUIRE_EQ(ttv->state, TableState::Sealed);
}

TEST_CASE_FIXTURE(Fixture, "less_exponential_blowup_please")
{
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

    LUAU_REQUIRE_ERRORS(result);
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

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "table_insert_should_cope_with_optional_properties_in_nonstrict")
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

TEST_CASE_FIXTURE(Fixture, "table_insert_should_cope_with_optional_properties_in_strict")
{
    ScopedFastFlag sff{"LuauTableSubtypingVariance2", true};

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
    ScopedFastFlag LuauTableSubtypingVariance2{"LuauTableSubtypingVariance2", true}; // Only for new path

    CheckResult result = check(R"(
type A = { x: number, y: number }
type B = { x: number, y: string }

local a: A
local b: B = a
    )");

    LUAU_REQUIRE_ERRORS(result);
    CHECK_EQ(toString(result.errors[0]), R"(Type 'A' could not be converted into 'B'
caused by:
  Property 'y' is not compatible. Type 'number' could not be converted into 'string')");
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_prop_nested")
{
    ScopedFastFlag LuauTableSubtypingVariance2{"LuauTableSubtypingVariance2", true}; // Only for new path

    CheckResult result = check(R"(
type AS = { x: number, y: number }
type BS = { x: number, y: string }

type A = { a: boolean, b: AS }
type B = { a: boolean, b: BS }

local a: A
local b: B = a
    )");

    LUAU_REQUIRE_ERRORS(result);
    CHECK_EQ(toString(result.errors[0]), R"(Type 'A' could not be converted into 'B'
caused by:
  Property 'b' is not compatible. Type 'AS' could not be converted into 'BS'
caused by:
  Property 'y' is not compatible. Type 'number' could not be converted into 'string')");
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_metatable_prop")
{
    ScopedFastFlag sff[]{
        {"LuauTableSubtypingVariance2", true},
        {"LuauUnsealedTableLiteral", true},
    };

    CheckResult result = check(R"(
local a1 = setmetatable({ x = 2, y = 3 }, { __call = function(s) end });
local b1 = setmetatable({ x = 2, y = "hello" }, { __call = function(s) end });
local c1: typeof(a1) = b1

local a2 = setmetatable({ x = 2, y = 3 }, { __call = function(s) end });
local b2 = setmetatable({ x = 2, y = 4 }, { __call = function(s, t) end });
local c2: typeof(a2) = b2
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ(toString(result.errors[0]), R"(Type 'b1' could not be converted into 'a1'
caused by:
  Type '{ x: number, y: string }' could not be converted into '{ x: number, y: number }'
caused by:
  Property 'y' is not compatible. Type 'string' could not be converted into 'number')");

    CHECK_EQ(toString(result.errors[1]), R"(Type 'b2' could not be converted into 'a2'
caused by:
  Type '{ __call: (a, b) -> () }' could not be converted into '{ __call: <a>(a) -> () }'
caused by:
  Property '__call' is not compatible. Type '(a, b) -> ()' could not be converted into '<a>(a) -> ()'; different number of generic type parameters)");
}

TEST_CASE_FIXTURE(Fixture, "explicitly_typed_table")
{
    ScopedFastFlag sffs[] {
        {"LuauPropertiesGetExpectedType", true},
        {"LuauExpectedTypesOfProperties", true},
        {"LuauTableSubtypingVariance2", true},
    };

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
    ScopedFastFlag sffs[] {
        {"LuauPropertiesGetExpectedType", true},
        {"LuauExpectedTypesOfProperties", true},
        {"LuauTableSubtypingVariance2", true},
        {"LuauUnsealedTableLiteral", true},
    };

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
    ScopedFastFlag sffs[] {
        {"LuauPropertiesGetExpectedType", true},
        {"LuauExpectedTypesOfProperties", true},
        {"LuauTableSubtypingVariance2", true},
    };

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

TEST_CASE_FIXTURE(Fixture, "recursive_metatable_type_call")
{
    ScopedFastFlag sff[]{
        {"LuauUnsealedTableLiteral", true},
    };

    CheckResult result = check(R"(
local b
b = setmetatable({}, {__call = b})
b()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), R"(Cannot call non-function t1 where t1 = { @metatable { __call: t1 }, {  } })");
}

TEST_CASE_FIXTURE(Fixture, "length_operator_union")
{
    ScopedFastFlag luauLengthOnCompositeType{"LuauLengthOnCompositeType", true};

    CheckResult result = check(R"(
local x: {number} | {string}
local y = #x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "length_operator_intersection")
{
    ScopedFastFlag luauLengthOnCompositeType{"LuauLengthOnCompositeType", true};

    CheckResult result = check(R"(
local x: {number} & {z:string} -- mixed tables are evil
local y = #x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "length_operator_non_table_union")
{
    ScopedFastFlag luauLengthOnCompositeType{"LuauLengthOnCompositeType", true};

    CheckResult result = check(R"(
local x: {number} | any | string
local y = #x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "length_operator_union_errors")
{
    ScopedFastFlag luauLengthOnCompositeType{"LuauLengthOnCompositeType", true};

    CheckResult result = check(R"(
local x: {number} | number | string
local y = #x
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_SUITE_END();
