// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "ScopedFlags.h"
#include "doctest.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/AstQuery.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauInitializeDefaultGenericParamsAtProgramPoint)
LUAU_FASTFLAG(LuauAddErrorCaseForIncompatibleTypePacks)

TEST_SUITE_BEGIN("TypeAliases");

TEST_CASE_FIXTURE(Fixture, "basic_alias")
{
    CheckResult result = check(R"(
        type T = number
        local x: T = 1
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number", toString(requireType("x")));
}

TEST_CASE_FIXTURE(Fixture, "cyclic_function_type_in_type_alias")
{
    CheckResult result = check(R"(
        type F = () -> F?
        local function f()
            return f
        end

        local g: F = f
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("t1 where t1 = () -> t1?", toString(requireType("g")));
}

TEST_CASE_FIXTURE(Fixture, "names_are_ascribed")
{
    CheckResult result = check(R"(
        type T = { x: number }
        local x: T
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("T", toString(requireType("x")));
}

TEST_CASE_FIXTURE(Fixture, "cannot_steal_hoisted_type_alias")
{
    // This is a tricky case. In order to support recursive type aliases,
    // we first walk the block and generate free types as placeholders.
    // We then walk the AST as normal. If we declare a type alias as below,
    // we generate a free type. We then begin our normal walk, examining
    // local x: T = "foo", which establishes two constraints:
    // a <: b
    // string <: a
    // We then visit the type alias, and establish that
    // b <: number
    // Then, when solving these constraints, we dispatch them in the order
    // they appear above. This means that a ~ b, and a ~ string, thus
    // b ~ string. This means the b <: number constraint has no effect.
    // Essentially we've "stolen" the alias's type out from under it.
    // This test ensures that we don't actually do this.
    CheckResult result = check(R"(
        local x: T = "foo"
        type T = number
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::LuauSolverV2)
    {
        CHECK(
            result.errors[0] == TypeError{
                                    Location{{1, 21}, {1, 26}},
                                    getMainSourceModule()->name,
                                    TypeMismatch{
                                        getBuiltins()->numberType,
                                        getBuiltins()->stringType,
                                    },
                                }
        );
    }
    else
    {
        CHECK(
            result.errors[0] == TypeError{
                                    Location{{1, 8}, {1, 26}},
                                    getMainSourceModule()->name,
                                    TypeMismatch{
                                        getBuiltins()->numberType,
                                        getBuiltins()->stringType,
                                    },
                                }
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "mismatched_generic_type_param")
{
    // We erroneously report an extra error in this case when the new solver is enabled.
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        type T<A> = (A...) -> ()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(
        toString(result.errors[0]) ==
        "Generic type 'A' is used as a variadic type parameter; consider changing 'A' to 'A...' in the generic argument list"
    );
    CHECK(result.errors[0].location == Location{{1, 21}, {1, 25}});
}

TEST_CASE_FIXTURE(Fixture, "mismatched_generic_pack_type_param")
{
    CheckResult result = check(R"(
        type T<A...> = (A) -> ()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(
        toString(result.errors[0]) ==
        "Variadic type parameter 'A...' is used as a regular generic type; consider changing 'A...' to 'A' in the generic argument list"
    );
    CHECK(result.errors[0].location == Location{{1, 24}, {1, 25}});
}

TEST_CASE_FIXTURE(Fixture, "default_type_parameter")
{
    CheckResult result = check(R"(
        type T<A = number, B = string> = { a: A, b: B }
        local x: T<string> = { a = "foo", b = "bar" }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireType("x")) == "T<string, string>");
}

TEST_CASE_FIXTURE(Fixture, "default_pack_parameter")
{
    CheckResult result = check(R"(
        type T<A... = (number, string)> = { fn: (A...) -> () }
        local x: T
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireType("x")) == "T<number, string>");
}

TEST_CASE_FIXTURE(Fixture, "saturate_to_first_type_pack")
{
    CheckResult result = check(R"(
        type T<A, B, C...> = { fn: (A, B) -> C... }
        local x: T<string, number, string, boolean>
        local f = x.fn
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireType("x")) == "T<string, number, string, boolean>");
    CHECK(toString(requireType("f")) == "(string, number) -> (string, boolean)");
}

TEST_CASE_FIXTURE(Fixture, "cyclic_types_of_named_table_fields_do_not_expand_when_stringified")
{
    CheckResult result = check(R"(
        --!strict
        type Node = { Parent: Node?; }

        function f(node: Node)
            node.Parent = 1
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE_MESSAGE(tm, result.errors[0]);
    CHECK_EQ("Node?", toString(tm->wantedType));
    CHECK_EQ(getBuiltins()->numberType, tm->givenType);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_aliases")
{
    CheckResult result = check(R"(
        --!strict
        type T = { f: number, g: U }
        type U = { h: number, i: T? }
        local x: T = { f = 37, g = { h = 5, i = nil } }
        x.g.i = x
        local y: T = { f = 3, g = { h = 5, i = nil } }
        y.g.i = y
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_aliases")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type T<a> = { v: a }
        local x: T<number> = { v = 123 }
        local y: T<string> = { v = "foo" }
        local bad: T<number> = { v = "foo" }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(result.errors[0].location == Location{{4, 37}, {4, 42}});
    CHECK_EQ("Type 'string' could not be converted into 'number'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "dependent_generic_aliases")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type T<a> = { v: a }
        type U<a> = { t: T<a> }
        local x: U<number> = { t = { v = 123 } }
        local bad: U<number> = { t = { v = "foo" } }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(result.errors[0].location == Location{{4, 43}, {4, 48}});
    CHECK_EQ("Type 'string' could not be converted into 'number'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_generic_aliases")
{
    CheckResult result = check(R"(
        --!strict
        type T<a> = { f: a, g: U<a> }
        type U<a> = { h: a, i: T<a>? }
        local x: T<number> = { f = 37, g = { h = 5, i = nil } }
        x.g.i = x
        local y: T<string> = { f = "hi", g = { h = "lo", i = nil } }
        y.g.i = y
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_errors")
{
    CheckResult result = check(R"(
        --!strict
        type T<a> = { f: a, g: U<a> }
        type U<b> = { h: b, i: T<b>? }
        local x: T<number> = { f = 37, g = { h = 5, i = nil } }
        x.g.i = x
        local y: T<string> = { f = "hi", g = { h = 5, i = nil } }
        y.g.i = y
    )");

    LUAU_REQUIRE_ERRORS(result);

    // We had a UAF in this example caused by not cloning type function arguments
    ModulePtr module = getFrontend().moduleResolver.getModule("MainModule");
    unfreeze(module->interfaceTypes);
    copyErrors(module->errors, module->interfaceTypes, getBuiltins());
    freeze(module->interfaceTypes);
    module->internalTypes.clear();
    module->astTypes.clear();

    // Make sure the error strings don't include "VALUELESS"
    for (auto error : module->errors)
        CHECK_MESSAGE(toString(error).find("VALUELESS") == std::string::npos, toString(error));
}

TEST_CASE_FIXTURE(Fixture, "use_table_name_and_generic_params_in_errors")
{
    CheckResult result = check(R"(
        type Pair<T, U> = {first: T, second: U}
        local a: Pair<string, number>
        local b: Pair<string, string>

        a = b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);

    CHECK_EQ("Pair<string, number>", toString(tm->wantedType));
    CHECK_EQ("Pair<string, string>", toString(tm->givenType));
}

TEST_CASE_FIXTURE(Fixture, "dont_stop_typechecking_after_reporting_duplicate_type_definition")
{
    CheckResult result = check(R"(
        type A = number
        type A = string -- Redefinition of type 'A', previously defined at line 1
        local foo: string = 1 -- "Type 'number' could not be converted into 'string'"
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(Fixture, "stringify_type_alias_of_recursive_template_table_type")
{

    CheckResult result = check(R"(
        type Table<T> = { a: T }
        type Wrapped = Table<Wrapped>
        local l: Wrapped = 2
        )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("Wrapped", toString(tm->wantedType));
    CHECK_EQ(getBuiltins()->numberType, tm->givenType);
}

TEST_CASE_FIXTURE(Fixture, "stringify_type_alias_of_recursive_template_table_type2")
{

    CheckResult result = check(R"(
        type Table<T> = { a: T }
        type Wrapped = (Table<Wrapped>) -> string
        local l: Wrapped = 2
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("t1 where t1 = ({ a: t1 }) -> string", toString(tm->wantedType));
    CHECK_EQ(getBuiltins()->numberType, tm->givenType);
}

#if 0 // CLI-169898: temporarily disabled for stack overflow in unoptimized build
// Check that recursive intersection type doesn't generate an OOM
TEST_CASE_FIXTURE(Fixture, "cli_38393_recursive_intersection_oom")
{
    CheckResult result = check(R"(
        function _(l0:(t0)&((t0)&(((t0)&((t0)->()))->(typeof(_),typeof(# _)))),l39,...):any
        end
        type t0<t0> = ((typeof(_))&((t0)&(((typeof(_))&(t0))->typeof(_))),{n163:any,})->(any,typeof(_))
        _(_)
    )");
}
#endif

TEST_CASE_FIXTURE(Fixture, "type_alias_fwd_declaration_is_precise")
{
    CheckResult result = check(R"(
        local foo: Id<number> = 1
        type Id<T> = T
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "corecursive_types_generic")
{
    const std::string code = R"(
        type A<T> = {v:T, b:B<T>}
        type B<T> = {v:T, a:A<T>}

        function f(a: A<number>)
            return a
        end
    )";

    const std::string expected = R"(
        type A<T> = {v:T, b:B<T>}
        type B<T> = {v:T, a:A<T>}

        function f(a: A<number>): A<number>
            return a
        end
    )";

    CHECK_EQ(expected, decorateWithTypes(code));
    CheckResult result = check(code);

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "corecursive_function_types")
{
    CheckResult result = check(R"(
        type A = () -> (number, B)
        type B = () -> (string, A)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("t1 where t1 = () -> (number, () -> (string, t1))", toString(requireTypeAlias("A")));
    CHECK_EQ("t1 where t1 = () -> (string, () -> (number, t1))", toString(requireTypeAlias("B")));
}

TEST_CASE_FIXTURE(Fixture, "generic_param_remap")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    const std::string code = R"(
        -- An example of a forwarded use of a type that has different type arguments than parameters
        type A<T,U> = {t:T, u:U, next:A<U,T>?}
        local aa:A<number,string> = { t = 5, u = 'hi', next = { t = 'lo', u = 8 } }
        local bb = aa
    )";

    const std::string expected = R"(

        type A<T,U> = {t:T, u:U, next:A<U,T>?}
        local aa:A<number,string> = { t = 5, u = 'hi', next = { t = 'lo', u = 8 } }
        local bb:A<number,string>=aa
    )";

    CHECK_EQ(expected, decorateWithTypes(code));
    CheckResult result = check(code);

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "export_type_and_type_alias_are_duplicates")
{
    CheckResult result = check(R"(
        export type Foo = number
        type Foo = number
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto dtd = get<DuplicateTypeDefinition>(result.errors[0]);
    REQUIRE(dtd);
    CHECK_EQ(dtd->name, "Foo");
}

TEST_CASE_FIXTURE(Fixture, "reported_location_is_correct_when_type_alias_are_duplicates")
{
    CheckResult result = check(R"(
        type A = string
        type B = number
        type C = string
        type B = number
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto dtd = get<DuplicateTypeDefinition>(result.errors[0]);
    REQUIRE(dtd);
    CHECK_EQ(dtd->name, "B");
    REQUIRE(dtd->previousLocation);
    CHECK_EQ(dtd->previousLocation->begin.line + 1, 3);
}

TEST_CASE_FIXTURE(Fixture, "stringify_optional_parameterized_alias")
{
    CheckResult result = check(R"(
        type Node<T> = { value: T, child: Node<T>? }

        local function visitor<T>(node: Node<T>?)
            local a: Node<T>

            if node then
                a = node.child -- Observe the output of the error message.
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto e = get<TypeMismatch>(result.errors[0]);
    REQUIRE(e != nullptr);
    CHECK_EQ("Node<T>?", toString(e->givenType));
    CHECK_EQ("Node<T>", toString(e->wantedType));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "general_require_multi_assign")
{
    fileResolver.source["workspace/A"] = R"(
        export type myvec2 = {x: number, y: number}
        return {}
    )";

    fileResolver.source["workspace/B"] = R"(
        export type myvec3 = {x: number, y: number, z: number}
        return {}
    )";

    fileResolver.source["workspace/C"] = R"(
        local Foo, Bar = require(workspace.A), require(workspace.B)

        local a: Foo.myvec2
        local b: Bar.myvec3
    )";

    CheckResult result = getFrontend().check("workspace/C");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId aTypeId = requireType("workspace/C", "a");
    const Luau::TableType* aType = get<TableType>(follow(aTypeId));
    REQUIRE(aType);
    REQUIRE(aType->props.size() == 2);

    TypeId bTypeId = requireType("workspace/C", "b");
    const Luau::TableType* bType = get<TableType>(follow(bTypeId));
    REQUIRE(bType);
    REQUIRE(bType->props.size() == 3);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_import_mutation")
{
    CheckResult result = check("type t10<x> = typeof(table)");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId ty = getGlobalBinding(getFrontend().globals, "table");

    CHECK(toString(ty) == "typeof(table)");

    const TableType* ttv = get<TableType>(ty);
    REQUIRE(ttv);

    CHECK(ttv->instantiatedTypeParams.empty());
}

TEST_CASE_FIXTURE(Fixture, "type_alias_local_mutation")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        type Cool = { a: number, b: string }
        local c: Cool = { a = 1, b = "s" }
        type NotCool<x> = Cool
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> ty = requireType("c");
    REQUIRE(ty);
    CHECK_EQ(toString(*ty), "Cool");

    const TableType* ttv = get<TableType>(*ty);
    REQUIRE(ttv);

    CHECK(ttv->instantiatedTypeParams.empty());
}

TEST_CASE_FIXTURE(Fixture, "type_alias_local_rename")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
type Cool = { a: number, b: string }
type NotCool = Cool
local c: Cool = { a = 1, b = "s" }
local d: NotCool = { a = 1, b = "s" }
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> ty = requireType("c");
    REQUIRE(ty);
    CHECK_EQ(toString(*ty), "Cool");

    ty = requireType("d");
    REQUIRE(ty);
    CHECK_EQ(toString(*ty), "NotCool");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_local_synthetic_mutation")
{
    CheckResult result = check(R"(
local c = { a = 1, b = "s" }
type Cool = typeof(c)
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> ty = requireType("c");
    REQUIRE(ty);

    const TableType* ttv = get<TableType>(*ty);
    REQUIRE(ttv);
    CHECK_EQ(ttv->name, "Cool");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_of_an_imported_recursive_type")
{

    fileResolver.source["game/A"] = R"(
export type X = { a: number, b: X? }
return {}
    )";

    CheckResult aResult = getFrontend().check("game/A");
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CheckResult bResult = check(R"(
local Import = require(game.A)
type X = Import.X
    )");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    std::optional<TypeId> ty1 = lookupImportedType("Import", "X");
    REQUIRE(ty1);

    std::optional<TypeId> ty2 = lookupType("X");
    REQUIRE(ty2);

    CHECK_EQ(follow(*ty1), follow(*ty2));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_of_an_imported_recursive_generic_type")
{

    fileResolver.source["game/A"] = R"(
        export type X<T, U> = { a: T, b: U, C: X<T, U>? }
        return {}
    )";

    CheckResult aResult = getFrontend().check("game/A");
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CheckResult bResult = check(R"(
        local Import = require(game.A)
        type X<T, U> = Import.X<T, U>
    )");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    std::optional<TypeId> ty1 = lookupImportedType("Import", "X");
    REQUIRE(ty1);

    std::optional<TypeId> ty2 = lookupType("X");
    REQUIRE(ty2);

    CHECK_EQ(toString(*ty1, {true}), toString(*ty2, {true}));

    bResult = check(R"(
        local Import = require(game.A)
        type X<T, U> = Import.X<U, T>
    )");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    ty1 = lookupImportedType("Import", "X");
    REQUIRE(ty1);

    ty2 = lookupType("X");
    REQUIRE(ty2);

    if (FFlag::LuauSolverV2)
    {
        CHECK(toString(*ty1, {true}) == "t1 where t1 = { C: t1?, a: T, b: U }");
        CHECK(toString(*ty2, {true}) == "t1 where t1 = { C: t1?, a: U, b: T }");
    }
    else
    {
        CHECK_EQ(toString(*ty1, {true}), "t1 where t1 = { C: t1?, a: T, b: U }");
        CHECK_EQ(toString(*ty2, {true}), "{ C: t1, a: U, b: T } where t1 = { C: t1, a: U, b: T }?");
    }
}

TEST_CASE_FIXTURE(Fixture, "module_export_free_type_leak")
{
    CheckResult result = check(R"(
function get()
    return function(obj) return true end
end

export type f = typeof(get())
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "module_export_wrapped_free_type_leak")
{
    CheckResult result = check(R"(
function get()
    return {a = 1, b = function(obj) return true end}
end

export type f = typeof(get())
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}


TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_restriction_ok")
{
    CheckResult result = check(R"(
        type Tree<T> = { data: T, children: Forest<T> }
        type Forest<T> = {Tree<T>}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_restriction_not_ok_1")
{
    // CLI-116108
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        -- OK because forwarded types are used with their parameters.
        type Tree<T> = { data: T, children: Forest<T> }
        type Forest<T> = {Tree<{T}>}
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_restriction_not_ok_2")
{
    // CLI-116108
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        -- Not OK because forwarded types are used with different types than their parameters.
        type Forest<T> = {Tree<{T}>}
        type Tree<T> = { data: T, children: Forest<T> }
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_swapsies_ok")
{
    CheckResult result = check(R"(
        type Tree1<T,U> = { data: T, children: {Tree2<U,T>} }
        type Tree2<U,T> = { data: U, children: {Tree1<T,U>} }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_swapsies_not_ok")
{
    // CLI-116108
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        type Tree1<T,U> = { data: T, children: {Tree2<U,T>} }
        type Tree2<T,U> = { data: U, children: {Tree1<T,U>} }
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "free_variables_from_typeof_in_aliases")
{
    CheckResult result = check(R"(
        function f(x) return x[1] end
        -- x has type X? for a free type variable X
        local x = f ({})
        type ContainsFree<a> = { this: a, that: typeof(x) }
        type ContainsContainsFree = { that: ContainsFree<number> }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "non_recursive_aliases_that_reuse_a_generic_name")
{
    CheckResult result = check(R"(
        type Array<T> = { [number]: T }
        type Tuple<T, V> = Array<T | V>

        local p: Tuple<number, string>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("{number | string}", toString(requireType("p"), {true}));
}

/*
 * We had a problem where all type aliases would be prototyped into a child scope that happened
 * to have the same level.  This caused a problem where, if a sibling function referred to that
 * type alias in its type signature, it would erroneously be quantified away, even though it doesn't
 * actually belong to the function.
 *
 * We solved this by ascribing a unique subLevel to each prototyped alias.
 */
TEST_CASE_FIXTURE(BuiltinsFixture, "do_not_quantify_unresolved_aliases")
{
    CheckResult result = check(R"(
        --!strict

        local KeyPool = {}

        local function newkey(pool: KeyPool, index)
            return {}
        end

        function newKeyPool()
            local pool = {
                available = {} :: {Key},
            }

            return setmetatable(pool, KeyPool)
        end

        export type KeyPool = typeof(newKeyPool())
        export type Key = typeof(newkey(newKeyPool(), 1))
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

/*
 * We keep a cache of type alias onto Type to prevent infinite types from
 * being constructed via recursive or corecursive aliases.  We have to adjust
 * the TypeLevels of those generic Types so that the unifier doesn't think
 * they have improperly leaked out of their scope.
 */
TEST_CASE_FIXTURE(Fixture, "generic_typevars_are_not_considered_to_escape_their_scope_if_they_are_reused_in_multiple_aliases")
{
    CheckResult result = check(R"(
        type Array<T> = {T}
        type Exclude<T, V> = T
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

/*
 * The two-pass alias definition system starts by ascribing a free Type to each alias.  It then
 * circles back to fill in the actual type later on.
 *
 * If this free type is unified with something degenerate like `any`, we need to take extra care
 * to ensure that the alias actually binds to the type that the user expected.
 */
TEST_CASE_FIXTURE(Fixture, "forward_declared_alias_is_not_clobbered_by_prior_unification_with_any")
{

    CheckResult result = check(R"(
        local function x()
            local y: FutureType = {}::any
            return 1
        end
        type FutureType = { foo: typeof(x()) }
        local d: FutureType = { smth = true } -- missing error, 'd' is resolved to 'any'
    )");

    CHECK_EQ("{ foo: number }", toString(requireType("d"), {true}));

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "recursive_types_restriction_ok")
{
    CheckResult result = check(R"(
        type Tree<T> = { data: T, children: {Tree<T>} }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "recursive_types_restriction_not_ok")
{
    // CLI-116108
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        -- this would be an infinite type if we allowed it
        type Tree<T> = { data: T, children: {Tree<{T}>} }
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "report_shadowed_aliases")
{
    // CLI-116110
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    // We allow a previous type alias to depend on a future type alias. That exact feature enables a confusing example, like the following snippet,
    // which has the type alias FakeString point to the type alias `string` that which points to `number`.
    CheckResult result = check(R"(
        type MyString = string
        type string = number
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Redefinition of type 'string'");

    std::optional<TypeId> t1 = lookupType("MyString");
    REQUIRE(t1);
    CHECK(isPrim(*t1, PrimitiveType::String));

    std::optional<TypeId> t2 = lookupType("string");
    REQUIRE(t2);
    CHECK(isPrim(*t2, PrimitiveType::String));
}

TEST_CASE_FIXTURE(Fixture, "it_is_ok_to_shadow_user_defined_alias")
{
    CheckResult result = check(R"(
        type T = number

        do
            type T = string
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "cannot_create_cyclic_type_with_unknown_module")
{
    CheckResult result = check(R"(
        type AAA = B.AAA
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Unknown type 'B.AAA'");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_locations")
{
    check(R"(
        type T = number

        do
            type T = string
            type X = boolean
        end
    )");

    ModulePtr mod = getMainModule();
    REQUIRE(!mod->scopes.empty());

    REQUIRE(mod->scopes[0].second->typeAliasNameLocations.count("T") > 0);
    CHECK(mod->scopes[0].second->typeAliasNameLocations["T"] == Location(Position(1, 13), 1));

    ScopePtr doScope = findScopeAtPosition(*mod, Position{4, 0});
    REQUIRE(doScope);

    REQUIRE(doScope->typeAliasNameLocations.count("T") > 0);
    CHECK(doScope->typeAliasNameLocations["T"] == Location(Position(4, 17), 1));

    REQUIRE(doScope->typeAliasNameLocations.count("X") > 0);
    CHECK(doScope->typeAliasNameLocations["X"] == Location(Position(5, 17), 1));
}

/*
 * We had a bug in DCR where substitution would improperly clone a
 * PendingExpansionType.
 *
 * This cloned type did not have a matching constraint to expand it, so it was
 * left dangling and unexpanded forever.
 *
 * We must also delay the dispatch a constraint if doing so would require
 * unifying a PendingExpansionType.
 */
TEST_CASE_FIXTURE(BuiltinsFixture, "dont_lose_track_of_PendingExpansionTypes_after_substitution")
{
    fileResolver.source["game/ReactCurrentDispatcher"] = R"(
        export type BasicStateAction<S> = ((S) -> S) | S
        export type Dispatch<A> = (A) -> ()

        export type Dispatcher = {
            useState: <S>(initialState: (() -> S) | S) -> (S, Dispatch<BasicStateAction<S>>),
        }

        return {}
    )";

    // Note: This script path is actually as short as it can be.  Any shorter
    // and we somehow fail to surface the bug.
    fileResolver.source["game/React/React/ReactHooks"] = R"(
        local RCD = require(script.Parent.Parent.Parent.ReactCurrentDispatcher)

        local function resolveDispatcher(): RCD.Dispatcher
            return (nil :: any) :: RCD.Dispatcher
        end

        function useState<S>(
            initialState: (() -> S) | S
        ): (S, RCD.Dispatch<RCD.BasicStateAction<S>>)
            local dispatcher = resolveDispatcher()
            return dispatcher.useState(initialState)
        end
    )";

    CheckResult result = getFrontend().check("game/React/React/ReactHooks");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "another_thing_from_roact")
{
    CheckResult result = check(R"(
        type Map<K, V> = { [K]: V }
        type Set<T> = { [T]: boolean }

        type FiberRoot = {
            pingCache: Map<Wakeable, (Set<any> | Map<Wakeable, Set<any>>)> | nil,
        }

        type Wakeable = {
            andThen: (self: Wakeable) -> nil | Wakeable,
        }

        local function attachPingListener(root: FiberRoot, wakeable: Wakeable, lanes: number)
            local pingCache: Map<Wakeable, (Set<any> | Map<Wakeable, Set<any>>)> | nil = root.pingCache
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

/*
 * It is sometimes possible for type alias resolution to produce a TypeId that
 * belongs to a different module.
 *
 * We must not mutate any fields of the resulting type when this happens.  The
 * memory has been frozen.
 */
TEST_CASE_FIXTURE(BuiltinsFixture, "alias_expands_to_bare_reference_to_imported_type")
{
    fileResolver.source["game/A"] = R"(
        --!strict
        export type Object = {[string]: any}
        return {}
    )";

    fileResolver.source["game/B"] = R"(
        local A = require(script.Parent.A)

        type Object = A.Object
        type ReadOnly<T> = T

        local function f(): ReadOnly<Object>
            return nil :: any
        end
    )";

    CheckResult result = getFrontend().check("game/B");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "table_types_record_the_property_locations")
{
    CheckResult result = check(R"(
        type Table = {
            create: () -> ()
        }

        local x: Table
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    auto ty = requireTypeAlias("Table");

    auto ttv = Luau::get<Luau::TableType>(follow(ty));
    REQUIRE(ttv);

    auto propIt = ttv->props.find("create");
    REQUIRE(propIt != ttv->props.end());

    CHECK_EQ(propIt->second.location, std::nullopt);
    CHECK_EQ(propIt->second.typeLocation, Location({2, 12}, {2, 18}));
}

TEST_CASE_FIXTURE(Fixture, "typeof_is_not_a_valid_alias_name")
{
    CheckResult result = check(R"(
        type typeof = number
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::LuauSolverV2)
    {
        CHECK("typeof cannot be used as an identifier for a type function or alias" == toString(result.errors[0]));
    }
    else
    {
        CHECK("Type aliases cannot be named typeof" == toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "fuzzer_bug_doesnt_crash")
{
    CheckResult result = check(R"(
type t0 = (t0<t0...>)
)");
    LUAU_REQUIRE_ERRORS(result);
}


TEST_CASE_FIXTURE(Fixture, "recursive_type_alias_warns")
{
    CheckResult result = check(R"(
type Foo<T> = Foo<T>
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto occursCheckError = get<OccursCheckFailed>(result.errors[0]);
    REQUIRE(occursCheckError);
}

TEST_CASE_FIXTURE(Fixture, "recursive_type_alias_bad_pack_use_warns")
{
    ScopedFastFlag sffs[] = {{FFlag::LuauSolverV2, true}, {FFlag::LuauAddErrorCaseForIncompatibleTypePacks, true}};

    CheckResult result = check(R"(
type Foo<T> = Foo<T...>
)");

    LUAU_REQUIRE_ERROR_COUNT(5, result);
    LUAU_CHECK_ERROR(result, GenericError);
    CHECK_EQ(toString(result.errors[4]), "Generic type 'Foo<T>' expects 1 type argument, but none are specified");

    auto occursCheckFailed = get<OccursCheckFailed>(result.errors[1]);
    REQUIRE(occursCheckFailed);

    auto swappedGeneric = get<SwappedGenericTypeParameter>(result.errors[2]);
    REQUIRE(swappedGeneric);
    CHECK(swappedGeneric->name == "T");
}

TEST_CASE_FIXTURE(Fixture, "corecursive_aliases")
{
    CheckResult result = check(R"(
type Foo<T> = Bar<T>
type Bar<T> = Foo<T>
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err = get<OccursCheckFailed>(result.errors[0]);
    REQUIRE(err);
}

TEST_CASE_FIXTURE(Fixture, "should_also_occurs_check")
{
    CheckResult result = check(R"(
type Foo<T> = Foo<T> | string
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err = get<OccursCheckFailed>(result.errors[0]);
    REQUIRE(err);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_adds_reduce_constraint_for_type_function")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
    type plus<T> = add<number, T>

    local sum: plus<number> = 10
    )");

    LUAU_CHECK_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "bound_type_in_alias_segfault")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        --!nonstrict
        type Map<T, V> = {[K]: V}
        function foo:bar(): Config<any, any> end
        type Config<TSource, TContext> = Map<TSource, TContext> & { fields: FieldConfigMap<any, any>}
        export type FieldConfig<TSource, TContext, TArgs> = {[string]: any}
        export type FieldConfigMap<TSource, TContext> = Map<string, FieldConfig<TSource, TContext>>
    )");

    LUAU_CHECK_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "gh1632_no_infinite_recursion_in_normalization")
{
    ScopedFastFlag flags[] = {
        {FFlag::LuauSolverV2, true},
    };

    CheckResult result = check(R"(
        type Node<T> = {
            value: T,
            next: Node<T>?,
            -- remove `prev`, solves issue
            prev: Node<T>?,
        };

        type List<T> = {
            head: Node<T>?
        }

        local function IsFront(list: List<any>, nodeB: Node<any>)
            -- remove if statement below, solves issue
            if (list.head == nodeB) then
            end
        end
    )");

    LUAU_CHECK_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "exported_alias_location_is_accessible_on_module")
{
    CheckResult result = check(R"(
        export type Value = string
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    auto module = getMainModule();
    auto tfun = module->exportedTypeBindings.find("Value");
    REQUIRE(tfun != module->exportedTypeBindings.end());
    CHECK_EQ(tfun->second.definitionLocation, Location{{1, 8}, {1, 34}});
}

TEST_CASE_FIXTURE(Fixture, "exported_type_function_location_is_accessible_on_module")
{
    ScopedFastFlag flags[] = {
        {FFlag::LuauSolverV2, true},
    };

    CheckResult result = check(R"(
        export type function Apply()
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    auto module = getMainModule();
    auto tfun = module->exportedTypeBindings.find("Apply");
    REQUIRE(tfun != module->exportedTypeBindings.end());
    CHECK_EQ(tfun->second.definitionLocation, Location{{1, 8}, {2, 11}});
}

TEST_CASE_FIXTURE(Fixture, "fuzzer_cursed_type_aliases")
{
    // This used to crash under the new solver: we would like this to continue
    // to not crash.
    LUAU_REQUIRE_ERRORS(check(R"(
        export type t1<t0...> = t4<t0...>
        export type t4<t2, t0...> = t4<t0...>
    )"));
}

TEST_CASE_FIXTURE(Fixture, "type_alias_dont_crash_on_bad_name")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type typeof = typeof(nil :: any)
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(get<ReservedIdentifier>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "type_alias_dont_crash_on_duplicate_with_typeof")
{
    // NOTE: This pattern looks quite silly, but it's pretty common in the old
    // solver in:
    //
    //  type Foo = typeof(setmetatable({} :: SomeType, {} :: SomeMetatableType))
    //
    CheckResult result = check(R"(
        type A = typeof(nil :: any)
        type A = typeof(nil :: any)
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(get<DuplicateTypeDefinition>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "fuzzer_more_cursed_aliases")
{
    LUAU_REQUIRE_ERRORS(check(R"(
export type t138 = t0<t138>
export type t0<t0,t10,t10,t109> = t0
    )"));
}

TEST_CASE_FIXTURE(Fixture, "evaluating_generic_default_type_shouldnt_ice")
{
    ScopedFastFlag sff{FFlag::LuauInitializeDefaultGenericParamsAtProgramPoint, true};

    auto result = check(R"(
local A = {}
type B<T = typeof(A)> = unknown
)");

    if (FFlag::LuauSolverV2)
        LUAU_REQUIRE_NO_ERRORS(result);
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_CHECK_ERROR(result, UnknownSymbol);
    }
}

TEST_CASE_FIXTURE(Fixture, "evaluating_generic_default_type_pack_shouldnt_ice")
{
    ScopedFastFlag sff{FFlag::LuauInitializeDefaultGenericParamsAtProgramPoint, true};

    auto result = check(R"(
local A = {}
type B<T... = ...typeof(A)> = unknown
)");

    if (FFlag::LuauSolverV2)
        LUAU_REQUIRE_NO_ERRORS(result);
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_CHECK_ERROR(result, UnknownSymbol);
    }
}

TEST_CASE_FIXTURE(Fixture, "evaluating_generic_default_type_for_symbol_before_definition_is_an_error")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauInitializeDefaultGenericParamsAtProgramPoint, true},
    };

    auto result = check(R"(
type B<T = typeof(A)> = unknown
local A = {}
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    LUAU_CHECK_ERROR(result, UnknownSymbol);
}

TEST_CASE_FIXTURE(Fixture, "evaluating_generic_default_type_pack_for_symbol_before_definition_is_an_error")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauInitializeDefaultGenericParamsAtProgramPoint, true},
    };

    auto result = check(R"(
type B<T... = ...typeof(A)> = unknown
local A = {}
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    LUAU_CHECK_ERROR(result, UnknownSymbol);
}



TEST_SUITE_END();
