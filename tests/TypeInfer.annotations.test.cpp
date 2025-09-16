// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "doctest.h"

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(DebugLuauMagicTypes)

using namespace Luau;

TEST_SUITE_BEGIN("AnnotationTests");

TEST_CASE_FIXTURE(Fixture, "initializers_are_checked_against_annotations")
{
    CheckResult result = check("local a: number = \"Hello Types!\"");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "check_multi_initialize")
{
    CheckResult result = check(R"(
        local a: number, b: string = "one", 2
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK(get<TypeMismatch>(result.errors[0]));
    CHECK(get<TypeMismatch>(result.errors[1]));
}

TEST_CASE_FIXTURE(Fixture, "successful_check")
{
    CheckResult result = check(R"(
        local a: number, b: string = 1, "two"
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "assignments_are_checked_against_annotations")
{
    CheckResult result = check(R"(
        local x: number = 1
        x = "two"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "multi_assign_checks_against_annotations")
{
    CheckResult result = check(R"(
        local a: number, b: string = 1, "two"
        a, b = "one", 2
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK(Location{{2, 15}, {2, 20}} == result.errors[0].location);
    CHECK(Location{{2, 22}, {2, 23}} == result.errors[1].location);
}

TEST_CASE_FIXTURE(Fixture, "assignment_cannot_transform_a_table_property_type")
{
    CheckResult result = check(R"(
        local a = {x=0}
        a.x = "one"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK(Location{{2, 14}, {2, 19}} == result.errors[0].location);
}

TEST_CASE_FIXTURE(Fixture, "assignments_to_unannotated_parameters_can_transform_the_type")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        function f(x)
            x = 0
            return x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("(unknown) -> number" == toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "assignments_to_annotated_parameters_are_checked")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        function f(x: string)
            x = 0
            return x
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(Location{{2, 16}, {2, 17}} == result.errors[0].location);

    CHECK("(string) -> number" == toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "variable_type_is_supertype")
{
    CheckResult result = check(R"(
        local x: number = 1
        local y: number? = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "assignment_also_checks_subtyping")
{
    CheckResult result = check(R"(
        function f(): number?
            return nil
        end
        local x: number = 1
        local y: number? = f()
        x = y
        y = x
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(Location{{6, 12}, {6, 13}} == result.errors[0].location);
}

TEST_CASE_FIXTURE(Fixture, "function_parameters_can_have_annotations")
{
    CheckResult result = check(R"(
        function double(x: number)
            return 2
        end

        local four = double(2)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "function_parameter_annotations_are_checked")
{
    CheckResult result = check(R"(
        function double(x: number)
            return 2
        end

        local four = double("two")
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "function_return_annotations_are_checked")
{
    CheckResult result = check(R"(
        function fifty(): any
            return 55
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId fiftyType = requireType("fifty");
    const FunctionType* ftv = get<FunctionType>(fiftyType);
    REQUIRE(ftv != nullptr);

    TypePackId retPack = follow(ftv->retTypes);
    const TypePack* tp = get<TypePack>(retPack);
    REQUIRE(tp != nullptr);

    REQUIRE_EQ(1, tp->head.size());

    REQUIRE_EQ(getBuiltins()->anyType, follow(tp->head[0]));
}

TEST_CASE_FIXTURE(Fixture, "function_return_multret_annotations_are_checked")
{
    CheckResult result = check(R"(
        function foo(): (number, string)
            return 1, 2
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "function_return_annotation_should_disambiguate_into_function_type_return_and_checked")
{
    CheckResult result = check(R"(
        function foo(): (number, string) -> nil
            return function(a: number, b: string): number return 1 end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "function_return_annotation_should_continuously_parse_return_annotation_and_checked")
{
    CheckResult result = check(R"(
        function foo(): (number, string) -> (number) -> nil
            return function(a: number, b: string): (number) -> nil
                return function(a: number): nil
                    return 1
                end
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "unknown_type_reference_generates_error")
{
    CheckResult result = check(R"(
        local x: IDoNotExist
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(
        result.errors[0] == TypeError{
                                Location{{1, 17}, {1, 28}},
                                getMainSourceModule()->name,
                                UnknownSymbol{
                                    "IDoNotExist",
                                    UnknownSymbol::Context::Type,
                                },
                            }
    );
}

TEST_CASE_FIXTURE(Fixture, "typeof_variable_type_annotation_should_return_its_type")
{
    CheckResult result = check(R"(
        local foo = { bar = "baz" }

        type Foo = typeof(foo)

        local foo2: Foo
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(requireType("foo"), requireType("foo2"));
}

TEST_CASE_FIXTURE(Fixture, "infer_type_of_value_a_via_typeof_with_assignment")
{
    CheckResult result = check(R"(
        local a
        local b: typeof(a) = 1

        a = "foo"
    )");

    if (FFlag::LuauSolverV2)
    {
        CHECK("string?" == toString(requireType("a")));
        CHECK("nil" == toString(requireType("b")));

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK(
            result.errors[0] ==
            (TypeError{Location{Position{2, 29}, Position{2, 30}}, TypeMismatch{getBuiltins()->nilType, getBuiltins()->numberType}})
        );
    }
    else
    {
        CHECK_EQ(*getBuiltins()->numberType, *requireType("a"));
        CHECK_EQ(*getBuiltins()->numberType, *requireType("b"));

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK_EQ(
            result.errors[0],
            (TypeError{Location{Position{4, 12}, Position{4, 17}}, TypeMismatch{getBuiltins()->numberType, getBuiltins()->stringType}})
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "table_annotation")
{
    CheckResult result = check(R"(
        local x: {a: number, b: string} = {a=2, b="three"}
        local y = x.a
        local z = x.b
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(PrimitiveType::Number, getPrimitiveType(follow(requireType("y"))));
    CHECK_EQ(PrimitiveType::String, getPrimitiveType(follow(requireType("z"))));
}

TEST_CASE_FIXTURE(Fixture, "function_annotation")
{
    CheckResult result = check(R"(
        local f: (number, string) -> number
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    dumpErrors(result);

    TypeId fType = requireType("f");
    const FunctionType* ftv = get<FunctionType>(follow(fType));

    REQUIRE(ftv != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "function_annotation_with_a_defined_function")
{
    CheckResult result = check(R"(
        local f: (number, number) -> string = function(a: number, b: number) return "" end
    )");

    TypeId fType = requireType("f");
    const FunctionType* ftv = get<FunctionType>(follow(fType));

    REQUIRE(ftv != nullptr);
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "type_assertion_expr")
{
    CheckResult result = check("local a = 55 :: any");
    REQUIRE_EQ("any", toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "as_expr_does_not_propagate_type_info")
{
    CheckResult result = check(R"(
        local a = 55 :: any
        local b = a :: number
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("any", toString(requireType("a")));
    CHECK_EQ("number", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "as_expr_is_bidirectional")
{
    CheckResult result = check(R"(
        local a = 55 :: number?
        local b = a :: number
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number?", toString(requireType("a")));
    CHECK_EQ("number", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "as_expr_warns_on_unrelated_cast")
{
    CheckResult result = check(R"(
        local a = 55 :: string
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Cannot cast 'number' into 'string' because the types are unrelated", toString(result.errors[0]));
    CHECK_EQ("string", toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "type_annotations_inside_function_bodies")
{
    CheckResult result = check(R"(
        function get_message()
            local message = 'That smarts!' :: string
            return message
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);
}

TEST_CASE_FIXTURE(Fixture, "for_loop_counter_annotation")
{
    CheckResult result1 = check(R"( for i: number = 0, 50 do end )");
    LUAU_REQUIRE_NO_ERRORS(result1);
}

TEST_CASE_FIXTURE(Fixture, "for_loop_counter_annotation_is_checked")
{
    CheckResult result2 = check(R"( for i: string = 0, 10 do end )");
    CHECK_EQ(1, result2.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "type_alias_should_alias_to_number")
{
    CheckResult result = check(R"(
        type A = number
        local a: A = 10
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "type_alias_B_should_check_with_another_aliases_until_a_non_aliased_type")
{
    CheckResult result = check(R"(
        type A = number
        type B = A
        local b: B = 10
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "type_aliasing_to_number_should_not_check_given_a_string")
{
    CheckResult result = check(R"(
        type A = number
        local a: A = "fail"
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "self_referential_type_alias")
{
    CheckResult result = check(R"(
        type O = { x: number, incr: (O) -> number }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> res = lookupType("O");
    REQUIRE(res);

    TypeId oType = follow(*res);
    const TableType* oTable = get<TableType>(oType);
    REQUIRE(oTable);

    std::optional<Property> incr = get(oTable->props, "incr");
    REQUIRE(incr);

    REQUIRE(incr->readTy);

    const FunctionType* incrFunc = get<FunctionType>(*incr->readTy);
    REQUIRE(incrFunc);

    std::optional<TypeId> firstArg = first(incrFunc->argTypes);
    REQUIRE(firstArg);

    REQUIRE_EQ(follow(*firstArg), oType);
}

TEST_CASE_FIXTURE(Fixture, "define_generic_type_alias")
{
    CheckResult result = check(R"(
        type Array<T> = {[number]: T}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr mainModule = getMainModule();
    REQUIRE(mainModule);
    REQUIRE(mainModule->hasModuleScope());

    auto it = mainModule->getModuleScope()->privateTypeBindings.find("Array");
    REQUIRE(it != mainModule->getModuleScope()->privateTypeBindings.end());

    TypeFun& tf = it->second;
    CHECK_EQ(1, tf.typeParams.size());
}

TEST_CASE_FIXTURE(Fixture, "use_generic_type_alias")
{
    CheckResult result = check(R"(
        type Array<T> = {[number]: T}   -- 1
        local p: Array<number> = {}     -- 2
        p[1] = 5                        -- 3 OK
        p[2] = 'hello'                  -- 4 Error.
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(4, result.errors[0].location.begin.line);
    CHECK(nullptr != get<TypeMismatch>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "two_type_params")
{
    CheckResult result = check(R"(
        type Map<K, V> = {[K]: V}
        local m: Map<string, number> = {}
        local a = m['foo']
        local b = m[9]                  -- error here
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(4, result.errors[0].location.begin.line);

    CHECK_EQ(toString(requireType("a")), "number");
}

TEST_CASE_FIXTURE(Fixture, "too_many_type_params")
{
    CheckResult result = check(R"(
        type Callback<A, R> = (A) -> (boolean, R)
        local a: Callback<number, number, string> = function(i) return true, 4 end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(2, result.errors[0].location.begin.line);

    IncorrectGenericParameterCount* igpc = get<IncorrectGenericParameterCount>(result.errors[0]);
    CHECK(nullptr != igpc);

    CHECK_EQ(3, igpc->actualParameters);
    CHECK_EQ(2, igpc->typeFun.typeParams.size());
    CHECK_EQ("Callback", igpc->name);

    CHECK_EQ("Generic type 'Callback<A, R>' expects 2 type arguments, but 3 are specified", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "duplicate_type_param_name")
{
    CheckResult result = check(R"(
        type Oopsies<T, T> = {a: T, b: T}
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto dgp = get<DuplicateGenericParameter>(result.errors[0]);
    REQUIRE(dgp);
    CHECK_EQ(dgp->parameterName, "T");
}

TEST_CASE_FIXTURE(Fixture, "typeof_expr")
{
    CheckResult result = check(R"(
        function id(i) return i end

        local m: typeof(id(77))
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number", toString(requireType("m")));
}

TEST_CASE_FIXTURE(Fixture, "corecursive_types_error_on_tight_loop")
{
    CheckResult result = check(R"(
        type A = B
        type B = A

        local aa:A
        local bb:B
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    OccursCheckFailed* ocf = get<OccursCheckFailed>(result.errors[0]);
    REQUIRE(ocf);
}

TEST_CASE_FIXTURE(Fixture, "type_alias_always_resolve_to_a_real_type")
{
    CheckResult result = check(R"(
        type A = B
        type B = C
        type C = number

        local aa:A
    )");

    TypeId fType = requireType("aa");
    REQUIRE(follow(fType) == getBuiltins()->numberType);
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "interface_types_belong_to_interface_arena")
{
    CheckResult result = check(R"(
        export type A = {field: number}

        local n: A = {field = 551}

        return {n=n}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    Module& mod = *getMainModule();

    const TypeFun& a = mod.exportedTypeBindings["A"];

    CHECK(isInArena(a.type, mod.interfaceTypes));
    CHECK(!isInArena(a.type, getFrontend().globals.globalTypes));

    std::optional<TypeId> exportsType = first(mod.returnType);
    REQUIRE(exportsType);

    TableType* exportsTable = getMutable<TableType>(*exportsType);
    REQUIRE(exportsTable != nullptr);

    REQUIRE(exportsTable->props["n"].readTy);
    TypeId n = *exportsTable->props["n"].readTy;
    REQUIRE(n != nullptr);

    CHECK(isInArena(n, mod.interfaceTypes));
}

TEST_CASE_FIXTURE(Fixture, "generic_aliases_are_cloned_properly")
{
    CheckResult result = check(R"(
        export type Array<T> = { [number]: T }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);

    Module& mod = *getMainModule();
    const auto& typeBindings = mod.exportedTypeBindings;

    auto it = typeBindings.find("Array");
    REQUIRE(typeBindings.end() != it);
    const TypeFun& array = it->second;

    REQUIRE_EQ(1, array.typeParams.size());

    const TableType* arrayTable = get<TableType>(array.type);
    REQUIRE(arrayTable != nullptr);

    CHECK_EQ(0, arrayTable->props.size());
    CHECK(arrayTable->indexer);

    CHECK(isInArena(array.type, mod.interfaceTypes));
    CHECK_EQ(array.typeParams[0].ty, arrayTable->indexer->indexResultType);
}

TEST_CASE_FIXTURE(Fixture, "cloned_interface_maintains_pointers_between_definitions")
{
    CheckResult result = check(R"(
        export type Record = { name: string, location: string }
        local a: Record = { name="Waldo", location="?????" }
        local b: Record = { name="Santa Claus", location="Maui" } -- FIXME

        return {a=a, b=b}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    Module& mod = *getMainModule();

    TypeId recordType = mod.exportedTypeBindings["Record"].type;

    std::optional<TypeId> exportsType = first(mod.returnType);
    REQUIRE(exportsType);

    TableType* exportsTable = getMutable<TableType>(*exportsType);
    REQUIRE(exportsTable != nullptr);

    REQUIRE(exportsTable->props["a"].readTy);
    TypeId aType = *exportsTable->props["a"].readTy;
    REQUIRE(aType);

    REQUIRE(exportsTable->props["b"].readTy);
    TypeId bType = *exportsTable->props["b"].readTy;
    REQUIRE(bType);

    CHECK(isInArena(recordType, mod.interfaceTypes));
    CHECK(isInArena(aType, mod.interfaceTypes));
    CHECK(isInArena(bType, mod.interfaceTypes));

    CHECK(toString(recordType, {true}) == toString(aType, {true}));
    CHECK(toString(recordType, {true}) == toString(bType, {true}));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "use_type_required_from_another_file")
{
    addGlobalBinding(getFrontend().globals, "script", getBuiltins()->anyType, "@test");

    fileResolver.source["Modules/Main"] = R"(
        --!strict
        local Test = require(script.Parent.Thing)

        export type Foo = { [any]: Test.TestType }

        return Test
    )";

    fileResolver.source["Modules/Thing"] = R"(
        --!strict

        export type TestType = {bar: boolean}

        return {}
    )";

    CheckResult result = getFrontend().check("Modules/Main");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cannot_use_nonexported_type")
{
    addGlobalBinding(getFrontend().globals, "script", getBuiltins()->anyType, "@test");

    fileResolver.source["Modules/Main"] = R"(
        --!strict
        local Test = require(script.Parent.Thing)

        export type Foo = { [any]: Test.TestType }

        return Test
    )";

    fileResolver.source["Modules/Thing"] = R"(
        --!strict

        type TestType = {bar: boolean}

        return {}
    )";

    CheckResult result = getFrontend().check("Modules/Main");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "builtin_types_are_not_exported")
{
    addGlobalBinding(getFrontend().globals, "script", getBuiltins()->anyType, "@test");

    fileResolver.source["Modules/Main"] = R"(
        --!strict
        local Test = require(script.Parent.Thing)

        export type Foo = { [any]: Test.number }

        return Test
    )";

    fileResolver.source["Modules/Thing"] = R"(
        --!strict

        return {}
    )";

    CheckResult result = getFrontend().check("Modules/Main");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

namespace
{
struct AssertionCatcher
{
    AssertionCatcher()
    {
        tripped = 0;
        oldhook = Luau::assertHandler();
        Luau::assertHandler() = [](const char* expr, const char* file, int line, const char* function) -> int
        {
            ++tripped;
            return 0;
        };
    }

    ~AssertionCatcher()
    {
        Luau::assertHandler() = oldhook;
    }

    static int tripped;
    Luau::AssertHandler oldhook;
};

int AssertionCatcher::tripped;
} // namespace

TEST_CASE_FIXTURE(Fixture, "luau_ice_triggers_an_ice_exception_with_flag")
{
    ScopedFastFlag sffs{FFlag::DebugLuauMagicTypes, true};

    AssertionCatcher ac;

    CHECK_THROWS_AS(
        check(R"(
        local a: _luau_ice = 55
    )"),
        InternalCompilerError
    );

    LUAU_ASSERT(1 == AssertionCatcher::tripped);
}

TEST_CASE_FIXTURE(Fixture, "luau_ice_triggers_an_ice_exception_with_flag_handler")
{
    ScopedFastFlag sffs{FFlag::DebugLuauMagicTypes, true};

    bool caught = false;

    getFrontend().iceHandler.onInternalError = [&](const char*)
    {
        caught = true;
    };

    CHECK_THROWS_AS(
        check(R"(
        local a: _luau_ice = 55
    )"),
        InternalCompilerError
    );

    CHECK_EQ(true, caught);
}

TEST_CASE_FIXTURE(Fixture, "luau_ice_is_not_special_without_the_flag")
{
    ScopedFastFlag sffs{FFlag::DebugLuauMagicTypes, false};

    // We only care that this does not throw
    check(R"(
        local a: _luau_ice = 55
    )");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "luau_print_is_magic_if_the_flag_is_set")
{
    // Force the frontend on here
    // The Fixture constructor (now lazy)
    // initializes printline. If we do something after that, we'll override it with something empty
    getFrontend();
    static std::vector<std::string> output;
    output.clear();
    Luau::setPrintLine(
        [](const std::string& s)
        {
            output.push_back(s);
        }
    );

    ScopedFastFlag sffs{FFlag::DebugLuauMagicTypes, true};

    CheckResult result = check(R"(
        local a: _luau_print<typeof(math.abs)>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    REQUIRE(1 == output.size());
}

TEST_CASE_FIXTURE(Fixture, "luau_print_is_not_special_without_the_flag")
{
    ScopedFastFlag sffs{FFlag::DebugLuauMagicTypes, false};

    CheckResult result = check(R"(
        local a: _luau_print<number>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "luau_print_incomplete")
{
    ScopedFastFlag sffs{FFlag::DebugLuauMagicTypes, true};

    CheckResult result = check(R"(
        local a: _luau_print
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("_luau_print requires one generic parameter", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "instantiate_type_fun_should_not_trip_rbxassert")
{
    CheckResult result = check(R"(
        type Foo<T> = typeof(function(x) return x end)
        local foo: Foo<number>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

#if 0
// This is because, after visiting all nodes in a block, we check if each type alias still points to a FreeType.
// Doing it that way is wrong, but I also tried to make typeof(x) return a BoundType, with no luck.
// Not important enough to fix today.
TEST_CASE_FIXTURE(Fixture, "pulling_a_type_from_value_dont_falsely_create_occurs_check_failed")
{
    CheckResult result = check(R"(
        function f(x)
            type T = typeof(x)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}
#endif

TEST_CASE_FIXTURE(Fixture, "occurs_check_on_cyclic_union_type")
{
    CheckResult result = check(R"(
        type T = T | T
        local x : T
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    OccursCheckFailed* ocf = get<OccursCheckFailed>(result.errors[0]);
    REQUIRE(ocf);
}

TEST_CASE_FIXTURE(Fixture, "occurs_check_on_cyclic_intersection_type")
{
    CheckResult result = check(R"(
        type T = T & T
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    OccursCheckFailed* ocf = get<OccursCheckFailed>(result.errors[0]);
    REQUIRE(ocf);
}

TEST_CASE_FIXTURE(Fixture, "instantiation_clone_has_to_follow")
{
    CheckResult result = check(R"(
        export type t8<t8> = (t0)&(<t0...>((true)|(any))->"")
        export type t0<t0> = ({})&({_:{[any]:number},})
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_SUITE_END();
