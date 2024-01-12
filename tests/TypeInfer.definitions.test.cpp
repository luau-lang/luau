// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("DefinitionTests");

TEST_CASE_FIXTURE(Fixture, "definition_file_simple")
{
    loadDefinition(R"(
        declare foo: number
        declare function bar(x: number): string
        declare foo2: typeof(foo)
    )");

    TypeId globalFooTy = getGlobalBinding(frontend.globals, "foo");
    CHECK_EQ(toString(globalFooTy), "number");

    TypeId globalBarTy = getGlobalBinding(frontend.globals, "bar");
    CHECK_EQ(toString(globalBarTy), "(number) -> string");

    TypeId globalFoo2Ty = getGlobalBinding(frontend.globals, "foo2");
    CHECK_EQ(toString(globalFoo2Ty), "number");

    CheckResult result = check(R"(
        local x: number = foo - 1
        local y: string = bar(x)
        local z: number | string = x
        z = y
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "definition_file_loading")
{
    loadDefinition(R"(
        declare foo: number
        export type Asdf = number | string
        declare function bar(x: number): string
        declare foo2: typeof(foo)
        declare function var(...: any): string
    )");

    TypeId globalFooTy = getGlobalBinding(frontend.globals, "foo");
    CHECK_EQ(toString(globalFooTy), "number");

    std::optional<TypeFun> globalAsdfTy = frontend.globals.globalScope->lookupType("Asdf");
    REQUIRE(bool(globalAsdfTy));
    CHECK_EQ(toString(globalAsdfTy->type), "number | string");

    TypeId globalBarTy = getGlobalBinding(frontend.globals, "bar");
    CHECK_EQ(toString(globalBarTy), "(number) -> string");

    TypeId globalFoo2Ty = getGlobalBinding(frontend.globals, "foo2");
    CHECK_EQ(toString(globalFoo2Ty), "number");

    TypeId globalVarTy = getGlobalBinding(frontend.globals, "var");

    CHECK_EQ(toString(globalVarTy), "(...any) -> string");

    CheckResult result = check(R"(
        local x: number = foo + 1
        local y: string = bar(x)
        local z: Asdf = x
        z = y
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "load_definition_file_errors_do_not_pollute_global_scope")
{
    unfreeze(frontend.globals.globalTypes);
    LoadDefinitionFileResult parseFailResult = frontend.loadDefinitionFile(frontend.globals, frontend.globals.globalScope, R"(
        declare foo
    )",
        "@test", /* captureComments */ false);
    freeze(frontend.globals.globalTypes);

    REQUIRE(!parseFailResult.success);
    std::optional<Binding> fooTy = tryGetGlobalBinding(frontend.globals, "foo");
    CHECK(!fooTy.has_value());

    LoadDefinitionFileResult checkFailResult = frontend.loadDefinitionFile(frontend.globals, frontend.globals.globalScope, R"(
        local foo: string = 123
        declare bar: typeof(foo)
    )",
        "@test", /* captureComments */ false);

    REQUIRE(!checkFailResult.success);
    std::optional<Binding> barTy = tryGetGlobalBinding(frontend.globals, "bar");
    CHECK(!barTy.has_value());
}

TEST_CASE_FIXTURE(Fixture, "definition_file_classes")
{
    loadDefinition(R"(
        declare class Foo
            X: number

            function inheritance(self): number
        end

        declare class Bar extends Foo
            Y: number

            function foo(self, x: number): number
            function foo(self, x: string): string

            function __add(self, other: Bar): Bar
        end
    )");

    CheckResult result = check(R"(
        local x: Bar
        local prop: number = x.Y
        local inheritedProp: number = x.X
        local method: number = x:foo(1)
        local method2: string = x:foo("string")
        local metamethod: Bar = x + x
        local inheritedMethod: number = x:inheritance()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("prop")), "number");
    CHECK_EQ(toString(requireType("inheritedProp")), "number");
    CHECK_EQ(toString(requireType("method")), "number");
    CHECK_EQ(toString(requireType("method2")), "string");
    CHECK_EQ(toString(requireType("metamethod")), "Bar");
    CHECK_EQ(toString(requireType("inheritedMethod")), "number");
}

TEST_CASE_FIXTURE(Fixture, "class_definitions_cannot_overload_non_function")
{
    unfreeze(frontend.globals.globalTypes);
    LoadDefinitionFileResult result = frontend.loadDefinitionFile(frontend.globals, frontend.globals.globalScope, R"(
        declare class A
            X: number
            X: string
        end
    )",
        "@test", /* captureComments */ false);
    freeze(frontend.globals.globalTypes);

    REQUIRE(!result.success);
    CHECK_EQ(result.parseResult.errors.size(), 0);
    REQUIRE(bool(result.module));
    REQUIRE_EQ(result.module->errors.size(), 1);
    GenericError* ge = get<GenericError>(result.module->errors[0]);
    REQUIRE(ge);
    CHECK_EQ("Cannot overload non-function class member 'X'", ge->message);
}

TEST_CASE_FIXTURE(Fixture, "class_definitions_cannot_extend_non_class")
{
    unfreeze(frontend.globals.globalTypes);
    LoadDefinitionFileResult result = frontend.loadDefinitionFile(frontend.globals, frontend.globals.globalScope, R"(
        type NotAClass = {}

        declare class Foo extends NotAClass
        end
    )",
        "@test", /* captureComments */ false);
    freeze(frontend.globals.globalTypes);

    REQUIRE(!result.success);
    CHECK_EQ(result.parseResult.errors.size(), 0);
    REQUIRE(bool(result.module));
    REQUIRE_EQ(result.module->errors.size(), 1);
    GenericError* ge = get<GenericError>(result.module->errors[0]);
    REQUIRE(ge);
    CHECK_EQ("Cannot use non-class type 'NotAClass' as a superclass of class 'Foo'", ge->message);
}

TEST_CASE_FIXTURE(Fixture, "no_cyclic_defined_classes")
{
    unfreeze(frontend.globals.globalTypes);
    LoadDefinitionFileResult result = frontend.loadDefinitionFile(frontend.globals, frontend.globals.globalScope, R"(
        declare class Foo extends Bar
        end

        declare class Bar extends Foo
        end
    )",
        "@test", /* captureComments */ false);
    freeze(frontend.globals.globalTypes);

    REQUIRE(!result.success);
}

TEST_CASE_FIXTURE(Fixture, "declaring_generic_functions")
{
    loadDefinition(R"(
        declare function f<a, b>(a: a, b: b): string
        declare function g<a..., b...>(...: a...): b...
        declare function h<a, b>(a: a, b: b): (b, a)
    )");

    CheckResult result = check(R"(
        local x = f(1, true)
        local y: number, z: string = g("foo", 123)
        local w, u = h(1, true)

        local f = f
        local g = g
        local h = h
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("x")), "string");
    CHECK_EQ(toString(requireType("w")), "boolean");
    CHECK_EQ(toString(requireType("u")), "number");
    CHECK_EQ(toString(requireType("f")), "<a, b>(a, b) -> string");
    CHECK_EQ(toString(requireType("g")), "<a..., b...>(a...) -> (b...)");
    CHECK_EQ(toString(requireType("h")), "<a, b>(a, b) -> (b, a)");
}

TEST_CASE_FIXTURE(Fixture, "class_definition_function_prop")
{
    loadDefinition(R"(
        declare class Foo
            X: (number) -> string
        end

        declare Foo: {
            new: () -> Foo
        }
    )");

    CheckResult result = check(R"(
        local x: Foo = Foo.new()
        local prop = x.X
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("prop")), "(number) -> string");
}

TEST_CASE_FIXTURE(Fixture, "definition_file_class_function_args")
{
    loadDefinition(R"(
        declare class Foo
            function foo1(self, x: number): number
            function foo2(self, x: number, y: string): number

            y: (a: number, b: string) -> string
        end

        declare Foo: {
            new: () -> Foo
        }
    )");

    CheckResult result = check(R"(
        local x: Foo = Foo.new()
        local methodRef1 = x.foo1
        local methodRef2 = x.foo2
        local prop = x.y
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    ToStringOptions opts;
    opts.functionTypeArguments = true;
    CHECK_EQ(toString(requireType("methodRef1"), opts), "(self: Foo, x: number) -> number");
    CHECK_EQ(toString(requireType("methodRef2"), opts), "(self: Foo, x: number, y: string) -> number");
    CHECK_EQ(toString(requireType("prop"), opts), "(a: number, b: string) -> string");
}

TEST_CASE_FIXTURE(Fixture, "definitions_documentation_symbols")
{
    loadDefinition(R"(
        declare x: string

        export type Foo = string | number

        declare class Bar
            prop: string
        end

        declare y: {
            x: number,
        }
    )");

    std::optional<Binding> xBinding = frontend.globals.globalScope->linearSearchForBinding("x");
    REQUIRE(bool(xBinding));
    // note: loadDefinition uses the @test package name.
    CHECK_EQ(xBinding->documentationSymbol, "@test/global/x");

    std::optional<TypeFun> fooTy = frontend.globals.globalScope->lookupType("Foo");
    REQUIRE(bool(fooTy));
    CHECK_EQ(fooTy->type->documentationSymbol, "@test/globaltype/Foo");

    std::optional<TypeFun> barTy = frontend.globals.globalScope->lookupType("Bar");
    REQUIRE(bool(barTy));
    CHECK_EQ(barTy->type->documentationSymbol, "@test/globaltype/Bar");

    ClassType* barClass = getMutable<ClassType>(barTy->type);
    REQUIRE(bool(barClass));
    REQUIRE_EQ(barClass->props.count("prop"), 1);
    CHECK_EQ(barClass->props["prop"].documentationSymbol, "@test/globaltype/Bar.prop");

    std::optional<Binding> yBinding = frontend.globals.globalScope->linearSearchForBinding("y");
    REQUIRE(bool(yBinding));
    CHECK_EQ(yBinding->documentationSymbol, "@test/global/y");

    TableType* yTtv = getMutable<TableType>(yBinding->typeId);
    REQUIRE(bool(yTtv));
    REQUIRE_EQ(yTtv->props.count("x"), 1);
    CHECK_EQ(yTtv->props["x"].documentationSymbol, "@test/global/y.x");
}

TEST_CASE_FIXTURE(Fixture, "definitions_symbols_are_generated_for_recursively_referenced_types")
{
    loadDefinition(R"(
        declare class MyClass
            function myMethod(self)
        end

        declare function myFunc(): MyClass
    )");

    std::optional<TypeFun> myClassTy = frontend.globals.globalScope->lookupType("MyClass");
    REQUIRE(bool(myClassTy));
    CHECK_EQ(myClassTy->type->documentationSymbol, "@test/globaltype/MyClass");
}

TEST_CASE_FIXTURE(Fixture, "documentation_symbols_dont_attach_to_persistent_types")
{
    loadDefinition(R"(
        export type Evil = string
    )");

    std::optional<TypeFun> ty = frontend.globals.globalScope->lookupType("Evil");
    REQUIRE(bool(ty));
    CHECK_EQ(ty->type->documentationSymbol, std::nullopt);
}

TEST_CASE_FIXTURE(Fixture, "single_class_type_identity_in_global_types")
{
    loadDefinition(R"(
declare class Cls
end

declare GetCls: () -> (Cls)
    )");

    CheckResult result = check(R"(
local s : Cls = GetCls()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "class_definition_overload_metamethods")
{
    loadDefinition(R"(
        declare class Vector3
        end

        declare class CFrame
            function __mul(self, other: CFrame): CFrame
            function __mul(self, other: Vector3): Vector3
        end

        declare function newVector3(): Vector3
        declare function newCFrame(): CFrame
    )");

    CheckResult result = check(R"(
        local base = newCFrame()
        local shouldBeCFrame = base * newCFrame()
        local shouldBeVector = base * newVector3()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("shouldBeCFrame")), "CFrame");
    CHECK_EQ(toString(requireType("shouldBeVector")), "Vector3");
}

TEST_CASE_FIXTURE(Fixture, "class_definition_string_props")
{
    loadDefinition(R"(
        declare class Foo
            ["a property"]: string
        end
    )");

    CheckResult result = check(R"(
        local x: Foo
        local y = x["a property"]
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("y")), "string");
}


TEST_CASE_FIXTURE(Fixture, "class_definition_indexer")
{
    loadDefinition(R"(
        declare class Foo
            [number]: string
        end
    )");

    CheckResult result = check(R"(
        local x: Foo
        local y = x[1]
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const ClassType* ctv = get<ClassType>(requireType("x"));
    REQUIRE(ctv != nullptr);

    REQUIRE(bool(ctv->indexer));

    CHECK_EQ(*ctv->indexer->indexType, *builtinTypes->numberType);
    CHECK_EQ(*ctv->indexer->indexResultType, *builtinTypes->stringType);

    CHECK_EQ(toString(requireType("y")), "string");
}

TEST_CASE_FIXTURE(Fixture, "class_definitions_reference_other_classes")
{
    unfreeze(frontend.globals.globalTypes);
    LoadDefinitionFileResult result = frontend.loadDefinitionFile(frontend.globals, frontend.globals.globalScope, R"(
        declare class Channel
            Messages: { Message }
            OnMessage: (message: Message) -> ()
        end

        declare class Message
            Text: string
            Channel: Channel
        end
    )",
        "@test", /* captureComments */ false);
    freeze(frontend.globals.globalTypes);

    REQUIRE(result.success);
}

TEST_CASE_FIXTURE(Fixture, "definition_file_has_source_module_name_set")
{
    LoadDefinitionFileResult result = loadDefinition(R"(
        declare class Foo
        end
    )");

    REQUIRE(result.success);

    CHECK_EQ(result.sourceModule.name, "@test");
    CHECK_EQ(result.sourceModule.humanReadableName, "@test");

    std::optional<TypeFun> fooTy = frontend.globals.globalScope->lookupType("Foo");
    REQUIRE(fooTy);

    const ClassType* ctv = get<ClassType>(fooTy->type);

    REQUIRE(ctv);
    CHECK_EQ(ctv->definitionModuleName, "@test");
}

TEST_SUITE_END();
