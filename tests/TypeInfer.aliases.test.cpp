// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "doctest.h"
#include "Luau/BuiltinDefinitions.h"

using namespace Luau;

TEST_SUITE_BEGIN("TypeAliases");

TEST_CASE_FIXTURE(Fixture, "cyclic_function_type_in_type_alias")
{
    ScopedFastFlag sff{"LuauOccursCheckOkWithRecursiveFunctions", true};

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

TEST_CASE_FIXTURE(Fixture, "cyclic_types_of_named_table_fields_do_not_expand_when_stringified")
{
    CheckResult result = check(R"(
        --!strict
        type Node = { Parent: Node?; }
        local node: Node;
        node.Parent = 1
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("Node?", toString(tm->wantedType));
    CHECK_EQ(typeChecker.numberType, tm->givenType);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types")
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
    ModulePtr module = frontend.moduleResolver.getModule("MainModule");
    unfreeze(module->interfaceTypes);
    copyErrors(module->errors, module->interfaceTypes);
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
    CHECK_EQ(typeChecker.numberType, tm->givenType);
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
    CHECK_EQ("t1 where t1 = ({| a: t1 |}) -> string", toString(tm->wantedType));
    CHECK_EQ(typeChecker.numberType, tm->givenType);
}

// Check that recursive intersection type doesn't generate an OOM
TEST_CASE_FIXTURE(Fixture, "cli_38393_recursive_intersection_oom")
{
    CheckResult result = check(R"(
        function _(l0:(t0)&((t0)&(((t0)&((t0)->()))->(typeof(_),typeof(# _)))),l39,...):any
        end
        type t0<t0> = ((typeof(_))&((t0)&(((typeof(_))&(t0))->typeof(_))),{n163:any,})->(any,typeof(_))
        _(_)
    )");

    LUAU_REQUIRE_ERRORS(result);
}

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
        local aa:A<number>
        local bb = aa
    )";

    const std::string expected = R"(
        type A<T> = {v:T, b:B<T>}
        type B<T> = {v:T, a:A<T>}
        local aa:A<number>
        local bb:A<number>=aa
    )";

    CHECK_EQ(expected, decorateWithTypes(code));
    CheckResult result = check(code);

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "corecursive_function_types")
{
    ScopedFastFlag sff{"LuauOccursCheckOkWithRecursiveFunctions", true};

    CheckResult result = check(R"(
        type A = () -> (number, B)
        type B = () -> (string, A)
        local a: A
        local b: B
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("t1 where t1 = () -> (number, () -> (string, t1))", toString(requireType("a")));
    CHECK_EQ("t1 where t1 = () -> (string, () -> (number, t1))", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "generic_param_remap")
{
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
    CHECK_EQ("Node<T>?", toString(e->givenType));
    CHECK_EQ("Node<T>", toString(e->wantedType));
}

TEST_CASE_FIXTURE(Fixture, "general_require_multi_assign")
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

    CheckResult result = frontend.check("workspace/C");
    LUAU_REQUIRE_NO_ERRORS(result);
    ModulePtr m = frontend.moduleResolver.modules["workspace/C"];

    REQUIRE(m != nullptr);

    std::optional<TypeId> aTypeId = lookupName(m->getModuleScope(), "a");
    REQUIRE(aTypeId);
    const Luau::TableTypeVar* aType = get<TableTypeVar>(follow(*aTypeId));
    REQUIRE(aType);
    REQUIRE(aType->props.size() == 2);

    std::optional<TypeId> bTypeId = lookupName(m->getModuleScope(), "b");
    REQUIRE(bTypeId);
    const Luau::TableTypeVar* bType = get<TableTypeVar>(follow(*bTypeId));
    REQUIRE(bType);
    REQUIRE(bType->props.size() == 3);
}

TEST_CASE_FIXTURE(Fixture, "type_alias_import_mutation")
{
    CheckResult result = check("type t10<x> = typeof(table)");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId ty = getGlobalBinding(frontend.typeChecker, "table");
    CHECK_EQ(toString(ty), "table");

    const TableTypeVar* ttv = get<TableTypeVar>(ty);
    REQUIRE(ttv);

    CHECK(ttv->instantiatedTypeParams.empty());
}

TEST_CASE_FIXTURE(Fixture, "type_alias_local_mutation")
{
    CheckResult result = check(R"(
type Cool = { a: number, b: string }
local c: Cool = { a = 1, b = "s" }
type NotCool<x> = Cool
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> ty = requireType("c");
    REQUIRE(ty);
    CHECK_EQ(toString(*ty), "Cool");

    const TableTypeVar* ttv = get<TableTypeVar>(*ty);
    REQUIRE(ttv);

    CHECK(ttv->instantiatedTypeParams.empty());
}

TEST_CASE_FIXTURE(Fixture, "type_alias_local_rename")
{
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

    const TableTypeVar* ttv = get<TableTypeVar>(*ty);
    REQUIRE(ttv);
    CHECK_EQ(ttv->name, "Cool");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_of_an_imported_recursive_type")
{
    fileResolver.source["game/A"] = R"(
export type X = { a: number, b: X? }
return {}
    )";

    CheckResult aResult = frontend.check("game/A");
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

TEST_CASE_FIXTURE(Fixture, "type_alias_of_an_imported_recursive_generic_type")
{
    fileResolver.source["game/A"] = R"(
export type X<T, U> = { a: T, b: U, C: X<T, U>? }
return {}
    )";

    CheckResult aResult = frontend.check("game/A");
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

    CHECK_EQ(toString(*ty1, {true}), "t1 where t1 = {| C: t1?, a: T, b: U |}");
    CHECK_EQ(toString(*ty2, {true}), "{| C: t1, a: U, b: T |} where t1 = {| C: t1, a: U, b: T |}?");
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
    ScopedFastFlag sff{"LuauRecursiveTypeParameterRestriction", true};

    CheckResult result = check(R"(
        -- OK because forwarded types are used with their parameters.
        type Tree<T> = { data: T, children: Forest<T> }
        type Forest<T> = {Tree<{T}>}
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_restriction_not_ok_2")
{
    ScopedFastFlag sff{"LuauRecursiveTypeParameterRestriction", true};

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
    ScopedFastFlag sff{"LuauRecursiveTypeParameterRestriction", true};

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
    ScopedFastFlag sff1{"LuauSubstitutionDontReplaceIgnoredTypes", true};

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
TEST_CASE_FIXTURE(Fixture, "do_not_quantify_unresolved_aliases")
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
 * We keep a cache of type alias onto TypeVar to prevent infinite types from
 * being constructed via recursive or corecursive aliases.  We have to adjust
 * the TypeLevels of those generic TypeVars so that the unifier doesn't think
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

TEST_SUITE_END();
