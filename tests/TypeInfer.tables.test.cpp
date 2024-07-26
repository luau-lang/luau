// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/Frontend.h"
#include "Luau/ToString.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "ScopedFlags.h"
#include "doctest.h"

#include <algorithm>

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAG(LuauInstantiateInSubtyping);
LUAU_FASTFLAG(LuauAlwaysCommitInferencesOfFunctionCalls);
LUAU_FASTFLAG(LuauFixIndexerSubtypingOrdering);
LUAU_FASTFLAG(DebugLuauSharedSelf);

LUAU_DYNAMIC_FASTFLAG(LuauImproveNonFunctionCallError)

TEST_SUITE_BEGIN("TableTests");

TEST_CASE_FIXTURE(BuiltinsFixture, "generalization_shouldnt_seal_table_in_len_function_fn")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;
    CheckResult result = check(R"(
local t = {}
for i = #t, 2, -1 do
    t[i] = t[i + 1]
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    const TableType* tType = get<TableType>(requireType("t"));
    REQUIRE(tType != nullptr);
    REQUIRE(tType->indexer);
    CHECK_EQ(tType->indexer->indexType, builtinTypes->numberType);
    CHECK_EQ(follow(tType->indexer->indexResultType), builtinTypes->unknownType);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "LUAU_ASSERT_arg_exprs_doesnt_trigger_assert")
{
    CheckResult result = check(R"(
local FadeValue = {}
function FadeValue.new(finalCallback)
	local self = setmetatable({}, FadeValue)
	self.finalCallback = finalCallback
	return self
end

function FadeValue:destroy()
	self.finalCallback()
	self.finalCallback = nil
end
)");
}

TEST_CASE_FIXTURE(Fixture, "basic")
{
    CheckResult result = check("local t = {foo = \"bar\", baz = 9, quux = nil}");
    LUAU_REQUIRE_NO_ERRORS(result);

    const TableType* tType = get<TableType>(requireType("t"));
    REQUIRE(tType != nullptr);

    std::optional<Property> fooProp = get(tType->props, "foo");
    REQUIRE(bool(fooProp));
    CHECK_EQ(PrimitiveType::String, getPrimitiveType(fooProp->type()));

    std::optional<Property> bazProp = get(tType->props, "baz");
    REQUIRE(bool(bazProp));
    CHECK_EQ(PrimitiveType::Number, getPrimitiveType(bazProp->type()));

    std::optional<Property> quuxProp = get(tType->props, "quux");
    REQUIRE(bool(quuxProp));
    CHECK_EQ(PrimitiveType::NilType, getPrimitiveType(quuxProp->type()));
}

TEST_CASE_FIXTURE(Fixture, "augment_table")
{
    CheckResult result = check(R"(
        local t = {}
        t.foo = 'bar'
    )");
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
    const TableType* pType = get<TableType>(tType->props["p"].type());
    REQUIRE(pType != nullptr);

    CHECK("{ p: { foo: string } }" == toString(requireType("t"), {true}));
}

TEST_CASE_FIXTURE(Fixture, "assign_key_at_index_expr")
{
    CheckResult result = check(R"(
        function f(t: {[string]: number})
            t["hello"] = 1
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // We had a bug where we forgot to record the astType of this particular node.
    CHECK("string" == toString(requireTypeAtPosition({2, 19})));
}

TEST_CASE_FIXTURE(Fixture, "index_expression_is_checked_against_the_indexer_type")
{
    CheckResult result = check(R"(
        function f(t: {[boolean]: number})
            t["hello"] = 15
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_MESSAGE(get<CannotExtendTable>(result.errors[0]), "Expected CannotExtendTable but got " << toString(result.errors[0]));
    else
        CHECK(get<TypeMismatch>(result.errors[0]));
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
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ(s, "{ prop: number }");
    else
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

TEST_CASE_FIXTURE(Fixture, "report_sensible_error_when_adding_a_value_to_a_nonexistent_prop")
{
    CheckResult result = check(R"(
        local t = {}
        t.foo[1] = 'one'
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    INFO(result.errors[0]);

    UnknownProperty* err = get<UnknownProperty>(result.errors[0]);
    REQUIRE(err);

    CHECK("t" == toString(err->table));
    CHECK("foo" == err->key);
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

    const FunctionType* methodType = get<FunctionType>(follow(fooProp->type()));
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
    TypeId uType = uProp->type();

    const TableType* uTable = get<TableType>(uType);
    REQUIRE(uTable != nullptr);

    std::optional<Property> fooProp = get(uTable->props, "foo");
    REQUIRE(bool(fooProp));

    const FunctionType* methodType = get<FunctionType>(follow(fooProp->type()));
    REQUIRE(methodType != nullptr);

    std::vector<TypeId> methodArgs = flatten(methodType->argTypes).first;

    REQUIRE_EQ(methodArgs.size(), 1);

    // TODO(rblanckaert): Revist when we can bind self at function creation time
    // REQUIRE_EQ(*methodArgs[0], *uType);
}

TEST_CASE_FIXTURE(Fixture, "call_method")
{
    CheckResult result = check(R"(
        local T = {}
        T.x = 0
        function T:method()
            return self.x
        end
        local a = T:method()
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*builtinTypes->numberType, *requireType("a"));
}

TEST_CASE_FIXTURE(Fixture, "call_method_with_explicit_self_argument")
{
    CheckResult result = check(R"(
        local T = {}
        T.x = 0

        function T:method()
            return self.x
        end

        local a = T.method(T)
    )");

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
    REQUIRE_MESSAGE(error != nullptr, "Expected MissingProperties but got " << toString(err));
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

TEST_CASE_FIXTURE(Fixture, "table_param_width_subtyping_1")
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

TEST_CASE_FIXTURE(Fixture, "table_param_width_subtyping_2")
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
    REQUIRE_MESSAGE(error != nullptr, "Expected MissingProperties but got " << toString(result.errors[0]));
    REQUIRE(error->properties.size() == 1);

    CHECK_EQ("baz", error->properties[0]);
}

TEST_CASE_FIXTURE(Fixture, "table_param_width_subtyping_3")
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
    REQUIRE_MESSAGE(error != nullptr, "Expected MissingProperties but got " << toString(err));
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

    TypeId indexResultType = follow(indexer.indexResultType);
    REQUIRE_MESSAGE(get<GenericType>(indexResultType), "Expected generic but got " << toString(indexResultType));
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
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("{ __name: string }", toString(indexer.indexType));
    else
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

    TypeId t2Ty = requireType("t2");
    const TableType* tTy = get<TableType>(t2Ty);
    REQUIRE_MESSAGE(tTy != nullptr, "Expected a table but got " << toString(t2Ty));

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
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK(toString(tm->givenType) == "{ [string]: string }");
    else
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
        function f(a: {number}): {string}
            return a
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_MESSAGE(nullptr != get<TypeMismatch>(result.errors[0]), "Expected a TypeMismatch but got " << result.errors[0]);
}

TEST_CASE_FIXTURE(Fixture, "indexer_on_sealed_table_must_unify_with_free_table")
{
    CheckResult result = check(R"(
        function F(t): {number}
            t[4] = "hi"
            return t
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "infer_type_when_indexing_from_a_table_indexer")
{
    CheckResult result = check(R"(
        function f(t: {string})
            return t[1]
        end

        local s = f({})
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*builtinTypes->stringType, *requireType("s"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "indexing_from_a_table_should_prefer_properties_when_possible")
{
    CheckResult result = check(R"(
        function f(): { a: string, [string]: number }
            error("e")
        end

        local t = f()

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

    TypeId tType = requireType("t");
    TableType* tableType = getMutable<TableType>(tType);
    REQUIRE_MESSAGE(tableType != nullptr, "Expected a table but got " << toString(tType, {true}));
    REQUIRE(tableType->indexer == std::nullopt);
    REQUIRE(0 != tableType->props.count("a"));

    TypeId propertyA = tableType->props["a"].type();
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
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, false};

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

TEST_CASE_FIXTURE(BuiltinsFixture, "meta_add_both_ways_lti")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        local vectorMt = {}

        function vectorMt.__add(self: Vector, other: number)
            return self
        end

        type Vector = typeof(setmetatable({}, vectorMt))
        local a: Vector = setmetatable({}, vectorMt)

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
    CHECK_EQ(follow(amtv->metatable), follow(requireType("amt")));

    const MetatableType* bmtv = get<MetatableType>(requireType("b"));
    REQUIRE(bmtv);
    CHECK_EQ(follow(bmtv->metatable), follow(requireType("bmt")));
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

        local v: Vector3 = {x = 5, y = 7}
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
        function f(): (any, number)
            return 5, 7
        end

        local a: any, b: number = f()

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

        function f(t)
            t.y = 1
        end

        function g(a: A)
            f(a)
        end
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

        function g(a: A)
            f(a)
        end
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
        function f(t: {}): { [string]: string, a: string }
            return t
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    MissingProperties* mp = get<MissingProperties>(result.errors[0]);
    REQUIRE_MESSAGE(mp, "Expected MissingProperties but got " << toString(result.errors[0]));
    CHECK_EQ(mp->context, MissingProperties::Missing);
    REQUIRE_EQ(1, mp->properties.size());
    CHECK_EQ(mp->properties[0], "a");

    CHECK_EQ("{| [string]: string, a: string |}", toString(mp->superType));
    CHECK_EQ("{|  |}", toString(mp->subType));
}

TEST_CASE_FIXTURE(Fixture, "casting_unsealed_tables_with_props_into_table_with_indexer")
{
    ScopedFastFlag sff{FFlag::LuauAlwaysCommitInferencesOfFunctionCalls, true};

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
    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("{ [string]: string }", toString(tm->wantedType, o));
        CHECK_EQ("{ foo: number }", toString(tm->givenType, o));
    }
    else
    {
        CHECK_EQ("{| [string]: string |}", toString(tm->wantedType, o));
        CHECK_EQ("{| foo: number |}", toString(tm->givenType, o));
    }
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
    ScopedFastFlag sff{FFlag::LuauAlwaysCommitInferencesOfFunctionCalls, true};

    CheckResult result = check(R"(
        local function foo(a: {[string]: number, a: string}) end
        foo({ a = 1 })
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    ToStringOptions o{/* exhaustive= */ true};
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("{| [string]: number, a: string |}", toString(tm->wantedType, o));
    CHECK_EQ("{ [string]: number, a: number }", toString(tm->givenType, o));
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
        CHECK(toString(result.errors[0]) == "Type 'number' could not be converted into 'string' in an invariant context");
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
        function f(vec1: {x: number}): {x: number, y: number, z: number}
            return vec1
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("Type pack '{ x: number }' could not be converted into '{ x: number, y: number, z: number }';"
                 " at [0], { x: number } is not a subtype of { x: number, y: number, z: number }",
            toString(result.errors[0]));
    }
    else
    {
        MissingProperties* mp = get<MissingProperties>(result.errors[0]);
        REQUIRE_MESSAGE(mp, result.errors[0]);
        CHECK_EQ(mp->context, MissingProperties::Missing);
        REQUIRE_EQ(2, mp->properties.size());
        CHECK_EQ(mp->properties[0], "y");
        CHECK_EQ(mp->properties[1], "z");

        CHECK_EQ("{| x: number, y: number, z: number |}", toString(mp->superType));
        CHECK_EQ("{| x: number |}", toString(mp->subType));
    }
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

        local vec3: {{x: number, y: number, z: number}} = {mkvec3()}
        local vec1: {{x: number}} = {mkvec1()}

        vec1 = vec3
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("vec1", toString(tm->wantedType));
        CHECK_EQ("vec3", toString(tm->givenType));
    }
    else
    {
        CHECK_EQ("{{| x: number |}}", toString(tm->wantedType));
        CHECK_EQ("{{| x: number, y: number, z: number |}}", toString(tm->givenType));
    }
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
    ScopedFastInt sfis{FInt::LuauTableTypeMaximumStringifierLength, 40};

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

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("Cannot add property 'a' to table '{ x: number }'", toString(result.errors[0]));
        CHECK_EQ("Cannot add property 'b' to table '{ x: number }'", toString(result.errors[1]));
    }
    else
    {
        CHECK_EQ("Cannot add property 'a' to table '{| x: number |}'", toString(result.errors[0]));
        CHECK_EQ("Cannot add property 'b' to table '{| x: number |}'", toString(result.errors[1]));
    }
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
    REQUIRE_MESSAGE(ttv, "Expected a table but got " << toString(ty, {true}));
    REQUIRE(ttv->props.count("new"));
    Property& prop = ttv->props["new"];
    REQUIRE(prop.type());
    const FunctionType* ftv = get<FunctionType>(follow(prop.type()));
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
    ScopedFastFlag sff{FFlag::DebugLuauSharedSelf, true};
    ScopedFastFlag sff2{FFlag::DebugLuauDeferredConstraintResolution, false};

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

local a: A = { x = 123, y = 456 }
local b: B = a
    )");

    LUAU_REQUIRE_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK(toString(result.errors.at(0)) == R"(Type 'A' could not be converted into 'B'; at [read "y"], number is not exactly string)");
    else
    {
        const std::string expected = R"(Type 'A' could not be converted into 'B'
caused by:
  Property 'y' is not compatible.
Type 'number' could not be converted into 'string' in an invariant context)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_prop_nested")
{
    CheckResult result = check(R"(
type AS = { x: number, y: number }
type BS = { x: number, y: string }

type A = { a: boolean, b: AS }
type B = { a: boolean, b: BS }

local a: A = { a = false, b = { x = 123, y = 456 } }
local b: B = a
    )");

    LUAU_REQUIRE_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK(toString(result.errors.at(0)) == R"(Type 'A' could not be converted into 'B'; at [read "b"][read "y"], number is not exactly string)");
    else
    {
        const std::string expected = R"(Type 'A' could not be converted into 'B'
caused by:
  Property 'b' is not compatible.
Type 'AS' could not be converted into 'BS'
caused by:
  Property 'y' is not compatible.
Type 'number' could not be converted into 'string' in an invariant context)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
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

    const std::string expected1 = R"(Type 'b1' could not be converted into 'a1'
caused by:
  Type
    '{ x: number, y: string }'
could not be converted into
    '{ x: number, y: number }'
caused by:
  Property 'y' is not compatible.
Type 'string' could not be converted into 'number' in an invariant context)";
    const std::string expected2 = R"(Type 'b2' could not be converted into 'a2'
caused by:
  Type
    '{ __call: <a, b>(a, b) -> () }'
could not be converted into
    '{ __call: <a>(a) -> () }'
caused by:
  Property '__call' is not compatible.
Type
    '<a, b>(a, b) -> ()'
could not be converted into
    '<a>(a) -> ()'; different number of generic type parameters)";
    const std::string expected3 = R"(Type 'b2' could not be converted into 'a2'
caused by:
  Type
    '{ __call: <a, b>(a, b) -> () }'
could not be converted into
    '{ __call: <a>(a) -> () }'
caused by:
  Property '__call' is not compatible.
Type
    '<a, b>(a, b) -> ()'
could not be converted into
    '<a>(a) -> ()'; different number of generic type parameters)";

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ(expected1, toString(result.errors[0]));
    if (FFlag::LuauInstantiateInSubtyping)
    {
        CHECK_EQ(expected2, toString(result.errors[1]));
    }
    else
    {
        std::string expected3 = R"(Type 'b2' could not be converted into 'a2'
caused by:
  Type
    '{ __call: (a, b) -> () }'
could not be converted into
    '{ __call: <a>(a) -> () }'
caused by:
  Property '__call' is not compatible.
Type
    '(a, b) -> ()'
could not be converted into
    '<a>(a) -> ()'; different number of generic type parameters)";
        CHECK_EQ(expected3, toString(result.errors[1]));
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
    const std::string expected = R"(Type 'A' could not be converted into 'B'
caused by:
  Property '[indexer key]' is not compatible.
Type 'number' could not be converted into 'string' in an invariant context)";
    CHECK_EQ(expected, toString(result.errors[0]));
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
    const std::string expected = R"(Type 'A' could not be converted into 'B'
caused by:
  Property '[indexer value]' is not compatible.
Type 'number' could not be converted into 'string' in an invariant context)";
    CHECK_EQ(expected, toString(result.errors[0]));
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
    const std::string expected = R"(Type 'tmp' could not be converted into 'HasSuper'
caused by:
  Property 'p' is not compatible.
Table type '{ x: number, y: number }' not compatible with type 'Super' because the former has extra field 'y')";
    CHECK_EQ(expected, toString(result.errors[0]));
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

    if (DFFlag::LuauImproveNonFunctionCallError)
        CHECK_EQ(toString(result.errors[0]), R"(Cannot call a value of type t1 where t1 = { @metatable { __call: t1 }, {  } })");
    else
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
    ScopedFastFlag sff{FFlag::LuauAlwaysCommitInferencesOfFunctionCalls, true};

    CheckResult result = check(R"(
        local a: {x: number, y: number, [any]: any} | {y: number}

        function f(t)
            t.y = 1
            return t
        end

        local b = f(a)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        REQUIRE_EQ("{| [any]: any, x: number, y: number |} | {| y: number |}", toString(requireType("b")));
    else
        REQUIRE_EQ("{- y: number -}", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "pass_a_union_of_tables_to_a_function_that_requires_a_table_2")
{
    ScopedFastFlag sff{FFlag::LuauAlwaysCommitInferencesOfFunctionCalls, true};

    CheckResult result = check(R"(
        local a: {y: number} | {x: number, y: number, [any]: any}

        function f(t)
            t.y = 1
            return t
        end

        local b = f(a)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        REQUIRE_EQ("{| [any]: any, x: number, y: number |} | {| y: number |}", toString(requireType("b")));
    else
        REQUIRE_EQ("{- y: number -}", toString(requireType("b")));
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

TEST_CASE_FIXTURE(Fixture, "should_not_unblock_table_type_twice")
{
    // don't run this when the DCR flag isn't set
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    check(R"(
        local timer = peek(timerQueue)
        while timer ~= nil do
            if timer.startTime <= currentTime then
                timer.isQueued = true
            end
            timer = peek(timerQueue)
        end
    )");

    // Just checking this is enough to satisfy the original bug.
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

    const TableType* fooArg1Table = get<TableType>(follow(*fooArg1));
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
    const FunctionType* newType = get<FunctionType>(follow(counterType->props["new"].type()));
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
    CHECK(0 == ttv->instantiatedTypeParams.size());
}

TEST_CASE_FIXTURE(Fixture, "record_location_of_inserted_table_properties")
{
    CheckResult result = check(R"(
        local a = {}
        a.foo = 1234
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const TableType* tt = get<TableType>(requireType("a"));
    REQUIRE(tt);

    REQUIRE(tt->props.count("foo"));

    const Property& prop = tt->props.find("foo")->second;
    CHECK(Location{{2, 10}, {2, 13}} == prop.location);
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

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        if (DFFlag::LuauImproveNonFunctionCallError)
            CHECK("Cannot call a value of type { @metatable { __call: number }, {  } }" == toString(result.errors[0]));
        else
            CHECK("Cannot call non-function { @metatable { __call: number }, {  } }" == toString(result.errors[0]));
    }
    else
    {
        TypeError e{
            Location{{5, 20}, {5, 21}},
            CannotCallNonFunction{builtinTypes->numberType},
        };

        CHECK(result.errors[0] == e);
    }
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
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("{ x: string }", toString(requireType("foo"), o));
    else
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
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("Type 'nil' does not have key 'x'", toString(result.errors[0]));
    else
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
        {FFlag::DebugLuauSharedSelf, true},
        {FFlag::DebugLuauDeferredConstraintResolution, false},
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

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK("Type '{number}' could not be converted into '{ [string]: number }'; at indexer(), number is not exactly string" == toString(result.errors[0]));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(3, result);

        CHECK_EQ("Type 'number' could not be converted into 'string'", toString(result.errors[0]));
        CHECK_EQ("Type 'number' could not be converted into 'string'", toString(result.errors[1]));
        CHECK_EQ("Type 'number' could not be converted into 'string'", toString(result.errors[2]));
    }
}

TEST_CASE_FIXTURE(Fixture, "shared_selfs")
{
    ScopedFastFlag sff{FFlag::DebugLuauSharedSelf, true};
    ScopedFastFlag sff2{FFlag::DebugLuauDeferredConstraintResolution, false};

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
    ScopedFastFlag sff{FFlag::DebugLuauSharedSelf, true};
    ScopedFastFlag sff2{FFlag::DebugLuauDeferredConstraintResolution, false};

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
    ScopedFastFlag sff{FFlag::DebugLuauSharedSelf, true};
    ScopedFastFlag sff2{FFlag::DebugLuauDeferredConstraintResolution, false};

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
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("Type '{ [boolean]: number } | {number}' does not have key 'x'", toString(result.errors[0]));
    else
        CHECK_EQ("Type '{number} | {| [boolean]: number |}' does not have key 'x'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "quantify_metatables_of_metatables_of_table")
{
    ScopedFastFlag sff[]{
        {FFlag::DebugLuauSharedSelf, true},
        {FFlag::DebugLuauDeferredConstraintResolution, false},
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
    ScopedFastFlag sff{FFlag::DebugLuauSharedSelf, true};
    ScopedFastFlag sff2{FFlag::DebugLuauDeferredConstraintResolution, false};

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
    ScopedFastFlag sff[] = {
        {FFlag::LuauAlwaysCommitInferencesOfFunctionCalls, true},
    };

    CheckResult result = check(R"(
        local function f(s)
            return s:absolutely_no_scalar_has_this_method()
        end

        f("foo" :: string)
        f("bar" :: "bar")
        f("baz" :: "bar" | "baz")
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);

    const std::string expected1 =
        R"(Type 'string' could not be converted into 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}'
caused by:
  The former's metatable does not satisfy the requirements.
Table type 'typeof(string)' not compatible with type 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}' because the former is missing field 'absolutely_no_scalar_has_this_method')";
    CHECK_EQ(expected1, toString(result.errors[0]));


    const std::string expected2 =
        R"(Type '"bar"' could not be converted into 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}'
caused by:
  The former's metatable does not satisfy the requirements.
Table type 'typeof(string)' not compatible with type 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}' because the former is missing field 'absolutely_no_scalar_has_this_method')";
    CHECK_EQ(expected2, toString(result.errors[1]));

    const std::string expected3 = R"(Type
    '"bar" | "baz"'
could not be converted into
    't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}'
caused by:
  Not all union options are compatible.
Type '"bar"' could not be converted into 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}'
caused by:
  The former's metatable does not satisfy the requirements.
Table type 'typeof(string)' not compatible with type 't1 where t1 = {- absolutely_no_scalar_has_this_method: (t1) -> (a...) -}' because the former is missing field 'absolutely_no_scalar_has_this_method')";
    CHECK_EQ(expected3, toString(result.errors[2]));
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

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        LUAU_REQUIRE_ERROR_COUNT(4, result);

        CHECK(toString(result.errors[0]) == "Parameter 's' has been reduced to never. This function is not callable with any possible value.");
        // FIXME: These free types should have been generalized by now.
        CHECK(toString(result.errors[1]) == "Parameter 's' is required to be a subtype of '{- read absolutely_no_scalar_has_this_method: ('a <: (never) -> ('b, c...)) -}' here.");
        CHECK(toString(result.errors[2]) == "Parameter 's' is required to be a subtype of 'string' here.");
        CHECK(get<CannotCallNonFunction>(result.errors[3]));

        CHECK_EQ("(never) -> string", toString(requireType("f")));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);

        const std::string expected =
            R"(Type 't1 where t1 = {+ absolutely_no_scalar_has_this_method: (t1) -> (a, b...) +}' could not be converted into 'string'
caused by:
  The former's metatable does not satisfy the requirements.
Table type 'typeof(string)' not compatible with type 't1 where t1 = {+ absolutely_no_scalar_has_this_method: (t1) -> (a, b...) +}' because the former is missing field 'absolutely_no_scalar_has_this_method')";
        CHECK_EQ(expected, toString(result.errors[0]));

        CHECK_EQ("<a, b...>(t1) -> string where t1 = {+ absolutely_no_scalar_has_this_method: (t1) -> (a, b...) +}", toString(requireType("f")));
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "a_free_shape_can_turn_into_a_scalar_directly")
{
    // We need egraphs to simplify the type of `out` here.  CLI-114134
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, false};

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
        {FFlag::LuauInstantiateInSubtyping, true},
    };

    CheckResult result = check(R"(
        --!strict
        local t = {}
        function t.m<T>(x: T) return x end
        local a : string = t.m("hi")
        local b : number = t.m(5)
        function f(x : { m : (number)->number })
            x.m = function(x: number) return 1+x end
        end

        f(t) -- This shouldn't typecheck

        local c : string = t.m("hi")
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        // FIXME.  We really should be reporting just one error in this case.  CLI-114509
        LUAU_REQUIRE_ERROR_COUNT(3, result);

        CHECK(get<TypePackMismatch>(result.errors[0]));
        CHECK(get<TypeMismatch>(result.errors[1]));
        CHECK(get<TypeMismatch>(result.errors[2]));
    }
    else
    {
        // TODO: test behavior is wrong until we can re-enable the covariant requirement for instantiation in subtyping
        //     LUAU_REQUIRE_ERRORS(result);
        //     CHECK_EQ(toString(result.errors[0]), R"(Type 't' could not be converted into '{| m: (number) -> number |}'
        // caused by:
        //   Property 'm' is not compatible. Type '<a>(a) -> a' could not be converted into '(number) -> number'; different number of generic type
        //   parameters)");
        //     // this error message is not great since the underlying issue is that the context is invariant,
        // and `(number) -> number` cannot be a subtype of `<a>(a) -> a`.

        LUAU_REQUIRE_NO_ERRORS(result);
    }
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

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        const TypeMismatch* error = get<TypeMismatch>(result.errors[0]);
        REQUIRE_MESSAGE(error, "Expected TypeMismatch but got " << result.errors[0]);

        CHECK("({ p: number, q: string }) -> { p: number, r: boolean }" == toString(error->wantedType));
        CHECK("({ p: number }) -> { p: number }" == toString(error->givenType));
    }
    else
    {
        const MissingProperties* error = get<MissingProperties>(result.errors[0]);
        REQUIRE_MESSAGE(error != nullptr, "Expected MissingProperties but got " << result.errors[0]);

        REQUIRE(error->properties.size() == 1);
        CHECK_EQ("r", error->properties[0]);
    }
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
        {FFlag::LuauInstantiateInSubtyping, true},
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
        {FFlag::LuauInstantiateInSubtyping, true},
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

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK("unknown" == toString(tt->indexer->indexType));
    else
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

    if (FFlag::DebugLuauDeferredConstraintResolution)
        LUAU_REQUIRE_ERROR_COUNT(1, result);
    else
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

TEST_CASE_FIXTURE(BuiltinsFixture, "luau-polyfill.Array.includes")
{

    CheckResult result = check(R"(
type Array<T> = { [number]: T }

function indexOf<T>(array: Array<T>, searchElement: any, fromIndex: number?): number
	return -1
end

return function<T>(array: Array<T>, searchElement: any, fromIndex: number?): boolean
	return -1 ~= indexOf(array, searchElement, fromIndex)
end

    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "certain_properties_of_table_literal_arguments_can_be_covariant")
{
    CheckResult result = check(R"(
        function f(a: {[string]: string | {any} | nil })
            return a
        end

        local x = f({
            title = "Feature.VirtualEvents.EnableNotificationsModalTitle",
            body = "Feature.VirtualEvents.EnableNotificationsModalBody",
            notNow = "Feature.VirtualEvents.NotNowButton",
            getNotified = "Feature.VirtualEvents.GetNotifiedButton",
        })
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "subproperties_can_also_be_covariantly_tested")
{
    CheckResult result = check(R"(
        type T = {
            [string]: {[string]: (string | number)?}
        }

        function f(t: T)
            return t
        end

        local x = f({
            subprop={x="hello"}
        })

        local y = f({
            subprop={x=41}
        })

        local z = f({
            subprop={}
        })
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "cyclic_shifted_tables")
{
    CheckResult result = check(R"(
        local function id<a>(x: a): a
          return x
        end

        -- Remove name from cyclic table
        local foo = id({})
        foo.foo = id({})
        foo.foo.foo = id({})
        foo.foo.foo.foo = id({})
        foo.foo.foo.foo.foo = foo

        local almostFoo = id({})
        almostFoo.foo = id({})
        almostFoo.foo.foo = id({})
        almostFoo.foo.foo.foo = id({})
        almostFoo.foo.foo.foo.foo = almostFoo
        -- Shift
        almostFoo = almostFoo.foo.foo
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}


TEST_CASE_FIXTURE(Fixture, "cli_84607_missing_prop_in_array_or_dict")
{
    ScopedFastFlag sff{FFlag::LuauFixIndexerSubtypingOrdering, true};

    CheckResult result = check(R"(
        type Thing = { name: string, prop: boolean }

        local arrayOfThings : {Thing} = {
            { name = "a" }
        }

        local dictOfThings : {[string]: Thing} = {
            a = { name = "a" }
        }
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        const TypeMismatch* err1 = get<TypeMismatch>(result.errors[0]);
        REQUIRE_MESSAGE(err1, "Expected TypeMismatch but got " << result.errors[0]);

        CHECK("{Thing}" == toString(err1->wantedType));
        CHECK("{{ name: string }}" == toString(err1->givenType));

        const TypeMismatch* err2 = get<TypeMismatch>(result.errors[1]);
        REQUIRE_MESSAGE(err2, "Expected TypeMismatch but got " << result.errors[1]);

        CHECK("{ [string]: Thing }" == toString(err2->wantedType));
        CHECK("{ [string]: { name: string } }" == toString(err2->givenType));
    }
    else
    {
        TypeError& err1 = result.errors[0];
        MissingProperties* error1 = get<MissingProperties>(err1);
        REQUIRE(error1);
        REQUIRE(error1->properties.size() == 1);

        CHECK_EQ("prop", error1->properties[0]);

        TypeError& err2 = result.errors[1];
        TypeMismatch* mismatch = get<TypeMismatch>(err2);
        REQUIRE(mismatch);
        MissingProperties* error2 = get<MissingProperties>(*mismatch->error);
        REQUIRE(error2);
        REQUIRE(error2->properties.size() == 1);

        CHECK_EQ("prop", error2->properties[0]);
    }
}

TEST_CASE_FIXTURE(Fixture, "simple_method_definition")
{
    CheckResult result = check(R"(
        local T = {}

        function T:m()
            return 5
        end

        return T
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("{ m: (unknown) -> number }", toString(getMainModule()->returnType, ToStringOptions{true}));
    else
        CHECK_EQ("{| m: <a>(a) -> number |}", toString(getMainModule()->returnType, ToStringOptions{true}));
}

TEST_CASE_FIXTURE(Fixture, "identify_all_problematic_table_fields")
{
    ScopedFastFlag sff_DebugLuauDeferredConstraintResolution{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        type T = {
            a: number,
            b: string,
            c: boolean,
        }

        local a: T = {
            a = "foo",
            b = false,
            c = 123,
        }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    std::string expected =
        "Type '{ a: string, b: boolean, c: number }' could not be converted into 'T'; at [read \"a\"], string is not exactly number"
        "\n\tat [read \"b\"], boolean is not exactly string"
        "\n\tat [read \"c\"], number is not exactly boolean";
    CHECK(toString(result.errors[0]) == expected);
}

TEST_CASE_FIXTURE(Fixture, "read_and_write_only_table_properties_are_unsupported")
{
    ScopedFastFlag sff[] = {
        {FFlag::DebugLuauDeferredConstraintResolution, false},
    };

    CheckResult result = check(R"(
        type W = {read x: number}
        type X = {write x: boolean}

        type Y = {read ["prop"]: boolean}
        type Z = {write ["prop"]: string}
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);

    CHECK("read keyword is illegal here" == toString(result.errors[0]));
    CHECK(Location{{1, 18}, {1, 22}} == result.errors[0].location);
    CHECK("write keyword is illegal here" == toString(result.errors[1]));
    CHECK(Location{{2, 18}, {2, 23}} == result.errors[1].location);
    CHECK("read keyword is illegal here" == toString(result.errors[2]));
    CHECK(Location{{4, 18}, {4, 22}} == result.errors[2].location);
    CHECK("write keyword is illegal here" == toString(result.errors[3]));
    CHECK(Location{{5, 18}, {5, 23}} == result.errors[3].location);
}

TEST_CASE_FIXTURE(Fixture, "read_ond_write_only_indexers_are_unsupported")
{
    CheckResult result = check(R"(
        type T = {read [string]: number}
        type U = {write [string]: boolean}
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK("read keyword is illegal here" == toString(result.errors[0]));
    CHECK(Location{{1, 18}, {1, 22}} == result.errors[0].location);
    CHECK("write keyword is illegal here" == toString(result.errors[1]));
    CHECK(Location{{2, 18}, {2, 23}} == result.errors[1].location);
}

TEST_CASE_FIXTURE(Fixture, "infer_write_property")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        function f(t)
            t.y = 1
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("({ y: number }) -> ()" == toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "table_subtyping_error_suppression")
{
    CheckResult result = check(R"(
        function one(tbl: {x: any}) end
        function two(tbl: {x: string}) one(tbl) end -- ok, string <: any and any <: string

        function three(tbl: {x: any, y: string}) end
        function four(tbl: {x: string, y: string}) three(tbl) end -- ok, string <: any, any <: string, string <: string
        function five(tbl: {x: string, y: number}) three(tbl) end -- error, string <: any, any <: string, but number </: string
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);


    // the new solver reports specifically the inner mismatch, rather than the whole table
    // honestly not sure which of these is a better developer experience.
    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ(*tm->wantedType, *builtinTypes->stringType);
        CHECK_EQ(*tm->givenType, *builtinTypes->numberType);
    }
    else
    {
        CHECK_EQ("{| x: any, y: string |}", toString(tm->wantedType));
        CHECK_EQ("{| x: string, y: number |}", toString(tm->givenType));
    }
}

TEST_CASE_FIXTURE(Fixture, "write_to_read_only_property")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        function f(t: {read x: number})
            t.x = 5
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK("Property x of table '{ read x: number }' is read-only" == toString(result.errors[0]));

    PropertyAccessViolation* pav = get<PropertyAccessViolation>(result.errors[0]);
    REQUIRE(pav);

    CHECK("{ read x: number }" == toString(pav->table, {true}));
    CHECK("x" == pav->key);
    CHECK(PropertyAccessViolation::CannotWrite == pav->context);
}

TEST_CASE_FIXTURE(Fixture, "write_to_unusually_named_read_only_property")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        function f(t: {read ["hello world"]: number})
            t["hello world"] = 5
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK("Property \"hello world\" of table '{ read [\"hello world\"]: number }' is read-only" == toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "write_annotations_are_unsupported_even_with_the_new_solver")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        function f(t: {write foo: number})
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK("write keyword is illegal here" == toString(result.errors[0]));
    CHECK(Location{{1, 23}, {1, 28}} == result.errors[0].location);
}

TEST_CASE_FIXTURE(Fixture, "read_and_write_only_table_properties_are_unsupported")
{
    ScopedFastFlag sff[] = {{FFlag::DebugLuauDeferredConstraintResolution, false}};

    CheckResult result = check(R"(
        type W = {read x: number}
        type X = {write x: boolean}

        type Y = {read ["prop"]: boolean}
        type Z = {write ["prop"]: string}
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);

    CHECK("read keyword is illegal here" == toString(result.errors[0]));
    CHECK(Location{{1, 18}, {1, 22}} == result.errors[0].location);
    CHECK("write keyword is illegal here" == toString(result.errors[1]));
    CHECK(Location{{2, 18}, {2, 23}} == result.errors[1].location);
    CHECK("read keyword is illegal here" == toString(result.errors[2]));
    CHECK(Location{{4, 18}, {4, 22}} == result.errors[2].location);
    CHECK("write keyword is illegal here" == toString(result.errors[3]));
    CHECK(Location{{5, 18}, {5, 23}} == result.errors[3].location);
}

TEST_CASE_FIXTURE(Fixture, "read_ond_write_only_indexers_are_unsupported")
{
    ScopedFastFlag sff[] = {{FFlag::DebugLuauDeferredConstraintResolution, false}};

    CheckResult result = check(R"(
        type T = {read [string]: number}
        type U = {write [string]: boolean}
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK("read keyword is illegal here" == toString(result.errors[0]));
    CHECK(Location{{1, 18}, {1, 22}} == result.errors[0].location);
    CHECK("write keyword is illegal here" == toString(result.errors[1]));
    CHECK(Location{{2, 18}, {2, 23}} == result.errors[1].location);
}

TEST_CASE_FIXTURE(Fixture, "table_writes_introduce_write_properties")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    ScopedFastFlag sff[] = {{FFlag::DebugLuauDeferredConstraintResolution, true}};

    CheckResult result = check(R"(
        function oc(player, speaker)
            local head = speaker.Character:FindFirstChild('Head')
            speaker.Character = player[1].Character
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("<a, b...>({{ read Character: t1 }}, { Character: t1 }) -> () "
          "where "
          "t1 = { read FindFirstChild: (t1, string) -> (a, b...) }" == toString(requireType("oc")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "tables_can_have_both_metatables_and_indexers")
{
    CheckResult result = check(R"(
        local a = {}
        a[1] = 5
        a[2] = 17

        local t = {}
        setmetatable(a, t)

        local c = a[1]
        print(a[1])
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("number" == toString(requireType("c")));
}

TEST_CASE_FIXTURE(Fixture, "refined_thing_can_be_an_array")
{
    CheckResult result = check(R"(
        function foo(x, y)
            if x then
                return x[1]
            else
                return y
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("<a>({a}, a) -> a" == toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "parameter_was_set_an_indexer_and_bounded_by_string")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        function f(t)
            local s: string = t
            t[5] = 7
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);

    CHECK_EQ("Parameter 't' has been reduced to never. This function is not callable with any possible value.", toString(result.errors[0]));
    CHECK_EQ("Parameter 't' is required to be a subtype of 'string' here.", toString(result.errors[1]));
    CHECK_EQ("Parameter 't' is required to be a subtype of '{number}' here.", toString(result.errors[2]));
}

TEST_CASE_FIXTURE(Fixture, "parameter_was_set_an_indexer_and_bounded_by_another_parameter")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        function f(t1, t2)
            t1[5] = 7 -- 't1 <: {number}
            t2 = t1   -- 't1 <: 't2
            t1[5] = 7 -- 't1 <: {number}
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // FIXME CLI-114134.  We need to simplify types more consistently.
    CHECK_EQ("(unknown & {number} & {number}, unknown) -> ()", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "write_to_union_property_not_all_present")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        type Animal = {tag: "Cat", meow: boolean} | {tag: "Dog", woof: boolean}
        function f(t: Animal)
            t.tag = "Dog"
        end
    )");

    // this should fail because `t` may be a `Cat` variant, and `"Dog"` is not a subtype of `"Cat"`.
    LUAU_REQUIRE_ERRORS(result);

    CannotAssignToNever* tm = get<CannotAssignToNever>(result.errors[0]);
    REQUIRE(tm);

    CHECK(builtinTypes->stringType == tm->rhsType);
    CHECK(CannotAssignToNever::Reason::PropertyNarrowed == tm->reason);
    REQUIRE(tm->cause.size() == 2);
    CHECK("\"Cat\"" == toString(tm->cause[0]));
    CHECK("\"Dog\"" == toString(tm->cause[1]));
}

TEST_CASE_FIXTURE(Fixture, "mymovie_read_write_tables_bug")
{
    CheckResult result = check(R"(
        type MockedResponseBody = string | (() -> MockedResponseBody)
        type MockedResponse = { type: 'body', body: MockedResponseBody } | { type: 'error' }

        local function mockedResponseToHttpResponse(mockedResponse: MockedResponse)
            assert(mockedResponse.type == 'body', 'Mocked response is not a body')
            if typeof(mockedResponse.body) == 'string' then
            else
                return mockedResponseToHttpResponse(mockedResponse)
            end
        end
    )");

    // we're primarily interested in knowing that this does not crash.
    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mymovie_read_write_tables_bug_2")
{
    CheckResult result = check(R"(
        type MockedResponse = { type: 'body' } | { type: 'error' }

        local function mockedResponseToHttpResponse(mockedResponse: MockedResponse)
            assert(mockedResponse.type == 'body', 'Mocked response is not a body')

            if typeof(mockedResponse.body) == 'string' then
            elseif typeof(mockedResponse.body) == 'table' then
            else
                return mockedResponseToHttpResponse(mockedResponse)
            end
        end
    )");

    // we're primarily interested in knowing that this does not crash.
    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "instantiated_metatable_frozen_table_clone_mutation")
{
    fileResolver.source["game/worker"] = R"(
type WorkerImpl<T..., R...> = {
    destroy: (self: Worker<T..., R...>) -> boolean,
}

type WorkerProps = { id: number }

export type Worker<T..., R...> = typeof(setmetatable({} :: WorkerProps, {} :: WorkerImpl<T..., R...>))

return {}
    )";

    fileResolver.source["game/library"] = R"(
local Worker = require(game.worker)

export type Worker<T..., R...> = Worker.Worker<T..., R...>

return {}
    )";

    CheckResult result = frontend.check("game/library");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "setprop_on_a_mutating_local_in_both_loops_and_functions")
{
    CheckResult result = check(R"(
        local _ = 5

        while (_) do
            _._ = nil
            function _()
                _ = nil
            end
        end
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "cant_index_this")
{
    CheckResult result = check(R"(
        local a: number = 9
        a[18] = "tomfoolery"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    NotATable* notATable = get<NotATable>(result.errors[0]);
    REQUIRE(notATable);

    CHECK("number" == toString(notATable->ty));
}

TEST_CASE_FIXTURE(Fixture, "setindexer_multiple_tables_intersection")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        local function f(t: { [string]: number } & { [thread]: boolean }, x)
            local k = "a"
            t[k] = x
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK("({ [string]: number } & { [thread]: boolean }, never) -> ()" == toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "insert_a_and_f_of_a_into_table_res_in_a_loop")
{
    CheckResult result = check(R"(
        local function f(t)
            local res = {}

            for k, a in t do
                res[k] = f(a)
                res[k] = a
            end
        end
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK(get<FunctionExitsWithoutReturning>(result.errors[0]));
    }
    else
        LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "ipairs_adds_an_unbounded_indexer")
{
    CheckResult result = check(R"(
        --!strict

        local a = {}
        ipairs(a)
    )");

    // The old solver erroneously leaves a free type dangling here.  The new
    // solver does better.
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK("{unknown}" == toString(requireType("a"), {true}));
    else
        CHECK("{a}" == toString(requireType("a"), {true}));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_results_compare_to_nil")
{
    CheckResult result = check(R"(
        --!strict

        function foo(tbl: {number})
            if tbl[2] == nil then
                print("foo")
            end

            if tbl[3] ~= nil then
                print("bar")
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzzer_normalization_preserves_tbl_scopes")
{
    CheckResult result = check(R"(
Module 'l0':
do end

Module 'l1':
local _ = {n0=nil,}
if if nil then _ then
if nil and (_)._ ~= (_)._ then
do end
while _ do
_ = _
do end
end
end
do end
end
local l0
while _ do
_ = nil
(_[_])._ %= `{# _}{bit32.extract(# _,1)}`
end

)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_literal_inference_assert")
{
    CheckResult result = check(R"(
        local buttons = {
            buttons = {};
        }

        buttons.Button = {
            call = nil;
            lightParts = nil;
            litPropertyOverrides = nil;
            model = nil;
            pivot = nil;
            unlitPropertyOverrides = nil;
        }
        buttons.Button.__index = buttons.Button

        local lightFuncs: { (self: types.Button, lit: boolean) -> nil } = {
            ['\x00'] = function(self: types.Button, lit: boolean)
        end;
        }
    )");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable_table_assertion_crash")
{
    CheckResult result = check(R"(
        local NexusInstance = {}
        function NexusInstance:__InitMetaMethods(): ()
            local Metatable = {}
            local OriginalIndexTable = getmetatable(self).__index
            setmetatable(self, Metatable)

            Metatable.__newindex = function(_, Index: string, Value: any): ()
                --Return if the new and old values are the same.
                if self[Index] == Value then
                end
            end
        end
    )");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table::insert_should_not_report_errors_when_correct_overload_is_picked")
{
    CheckResult result = check(R"(
type cs = { GetTagged : (cs, string) -> any}
local destroyQueue: {any} = {} -- pair of (time, coin)
local tick : () -> any
local CS : cs
local DESTROY_DELAY
local function SpawnCoin()
	local spawns = CS:GetTagged('CoinSpawner')
	local n : any
	local StartPos = spawns[n].CFrame
	local Coin = script.Coin:Clone()
	Coin.CFrame = StartPos
	Coin.Parent = workspace.Coins

	table.insert(destroyQueue, {tick() + DESTROY_DELAY, Coin})
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
