// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauNoMoreComparisonTypeFunctions)

LUAU_FASTINT(LuauTypeInferRecursionLimit)
LUAU_FASTFLAG(LuauNoOrderingTypeFunctions)

TEST_SUITE_BEGIN("DefinitionTests");

TEST_CASE_FIXTURE(Fixture, "definition_file_simple")
{
    loadDefinition(R"(
        declare foo: number
        declare function bar(x: number): string
        declare foo2: typeof(foo)
    )");

    TypeId globalFooTy = getGlobalBinding(getFrontend().globals, "foo");
    CHECK_EQ(toString(globalFooTy), "number");

    TypeId globalBarTy = getGlobalBinding(getFrontend().globals, "bar");
    CHECK_EQ(toString(globalBarTy), "(number) -> string");

    TypeId globalFoo2Ty = getGlobalBinding(getFrontend().globals, "foo2");
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

    TypeId globalFooTy = getGlobalBinding(getFrontend().globals, "foo");
    CHECK_EQ(toString(globalFooTy), "number");

    std::optional<TypeFun> globalAsdfTy = getFrontend().globals.globalScope->lookupType("Asdf");
    REQUIRE(bool(globalAsdfTy));
    CHECK_EQ(toString(globalAsdfTy->type), "number | string");

    TypeId globalBarTy = getGlobalBinding(getFrontend().globals, "bar");
    CHECK_EQ(toString(globalBarTy), "(number) -> string");

    TypeId globalFoo2Ty = getGlobalBinding(getFrontend().globals, "foo2");
    CHECK_EQ(toString(globalFoo2Ty), "number");

    TypeId globalVarTy = getGlobalBinding(getFrontend().globals, "var");

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
    unfreeze(getFrontend().globals.globalTypes);
    LoadDefinitionFileResult parseFailResult = getFrontend().loadDefinitionFile(
        getFrontend().globals,
        getFrontend().globals.globalScope,
        R"(
        declare foo
    )",
        "@test",
        /* captureComments */ false
    );
    freeze(getFrontend().globals.globalTypes);

    REQUIRE(!parseFailResult.success);
    std::optional<Binding> fooTy = tryGetGlobalBinding(getFrontend().globals, "foo");
    CHECK(!fooTy.has_value());

    LoadDefinitionFileResult checkFailResult = getFrontend().loadDefinitionFile(
        getFrontend().globals,
        getFrontend().globals.globalScope,
        R"(
        local foo: string = 123
        declare bar: typeof(foo)
    )",
        "@test",
        /* captureComments */ false
    );

    REQUIRE(!checkFailResult.success);
    std::optional<Binding> barTy = tryGetGlobalBinding(getFrontend().globals, "bar");
    CHECK(!barTy.has_value());
}

TEST_CASE_FIXTURE(Fixture, "definition_file_extern_types")
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
    unfreeze(getFrontend().globals.globalTypes);
    LoadDefinitionFileResult result = getFrontend().loadDefinitionFile(
        getFrontend().globals,
        getFrontend().globals.globalScope,
        R"(
        declare class A
            X: number
            X: string
        end
    )",
        "@test",
        /* captureComments */ false
    );
    freeze(getFrontend().globals.globalTypes);

    REQUIRE(!result.success);
    CHECK_EQ(result.parseResult.errors.size(), 0);
    REQUIRE(bool(result.module));
    if (FFlag::LuauSolverV2)
        REQUIRE_EQ(result.module->errors.size(), 2);
    else
        REQUIRE_EQ(result.module->errors.size(), 1);

    GenericError* ge = get<GenericError>(result.module->errors[0]);
    REQUIRE(ge);
    if (FFlag::LuauSolverV2)
        CHECK_EQ("Cannot overload read type of non-function class member 'X'", ge->message);
    else
        CHECK_EQ("Cannot overload non-function class member 'X'", ge->message);

    if (FFlag::LuauSolverV2)
    {
        GenericError* ge2 = get<GenericError>(result.module->errors[1]);
        REQUIRE(ge2);
        CHECK_EQ("Cannot overload write type of non-function class member 'X'", ge2->message);
    }
}

TEST_CASE_FIXTURE(Fixture, "class_definitions_cannot_extend_non_class")
{
    unfreeze(getFrontend().globals.globalTypes);
    LoadDefinitionFileResult result = getFrontend().loadDefinitionFile(
        getFrontend().globals,
        getFrontend().globals.globalScope,
        R"(
        type NotAClass = {}

        declare class Foo extends NotAClass
        end
    )",
        "@test",
        /* captureComments */ false
    );
    freeze(getFrontend().globals.globalTypes);

    REQUIRE(!result.success);
    CHECK_EQ(result.parseResult.errors.size(), 0);
    REQUIRE(bool(result.module));
    REQUIRE_EQ(result.module->errors.size(), 1);
    GenericError* ge = get<GenericError>(result.module->errors[0]);
    REQUIRE(ge);
    CHECK_EQ("Cannot use non-class type 'NotAClass' as a superclass of class 'Foo'", ge->message);
}

TEST_CASE_FIXTURE(Fixture, "no_cyclic_defined_extern_types")
{
    unfreeze(getFrontend().globals.globalTypes);
    LoadDefinitionFileResult result = getFrontend().loadDefinitionFile(
        getFrontend().globals,
        getFrontend().globals.globalScope,
        R"(
        declare class Foo extends Bar
        end

        declare class Bar extends Foo
        end
    )",
        "@test",
        /* captureComments */ false
    );
    freeze(getFrontend().globals.globalTypes);

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

    std::optional<Binding> xBinding = getFrontend().globals.globalScope->linearSearchForBinding("x");
    REQUIRE(bool(xBinding));
    // note: loadDefinition uses the @test package name.
    CHECK_EQ(xBinding->documentationSymbol, "@test/global/x");

    std::optional<TypeFun> fooTy = getFrontend().globals.globalScope->lookupType("Foo");
    REQUIRE(bool(fooTy));
    CHECK_EQ(fooTy->type->documentationSymbol, "@test/globaltype/Foo");

    std::optional<TypeFun> barTy = getFrontend().globals.globalScope->lookupType("Bar");
    REQUIRE(bool(barTy));
    CHECK_EQ(barTy->type->documentationSymbol, "@test/globaltype/Bar");

    ExternType* barClass = getMutable<ExternType>(barTy->type);
    REQUIRE(bool(barClass));
    REQUIRE_EQ(barClass->props.count("prop"), 1);
    CHECK_EQ(barClass->props["prop"].documentationSymbol, "@test/globaltype/Bar.prop");

    std::optional<Binding> yBinding = getFrontend().globals.globalScope->linearSearchForBinding("y");
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

    std::optional<TypeFun> myClassTy = getFrontend().globals.globalScope->lookupType("MyClass");
    REQUIRE(bool(myClassTy));
    CHECK_EQ(myClassTy->type->documentationSymbol, "@test/globaltype/MyClass");

    ExternType* cls = getMutable<ExternType>(myClassTy->type);
    REQUIRE(bool(cls));
    REQUIRE_EQ(cls->props.count("myMethod"), 1);

    const auto& method = cls->props["myMethod"];
    CHECK_EQ(method.documentationSymbol, "@test/globaltype/MyClass.myMethod");

    REQUIRE(method.readTy);
    FunctionType* function = getMutable<FunctionType>(*method.readTy);
    REQUIRE(function);

    REQUIRE(function->definition.has_value());
    CHECK(function->definition->definitionModuleName == "@test");
    CHECK(function->definition->definitionLocation == Location({2, 12}, {2, 35}));
    CHECK(!function->definition->varargLocation.has_value());
    CHECK(function->definition->originalNameLocation == Location({2, 21}, {2, 29}));
}

TEST_CASE_FIXTURE(Fixture, "documentation_symbols_dont_attach_to_persistent_types")
{
    loadDefinition(R"(
        export type Evil = string
    )");

    std::optional<TypeFun> ty = getFrontend().globals.globalScope->lookupType("Evil");
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

TEST_CASE_FIXTURE(Fixture, "class_definition_malformed_string")
{
    unfreeze(getFrontend().globals.globalTypes);
    LoadDefinitionFileResult result = getFrontend().loadDefinitionFile(
        getFrontend().globals,
        getFrontend().globals.globalScope,
        R"(
        declare class Foo
            ["a\0property"]: string
        end
    )",
        "@test",
        /* captureComments */ false
    );
    freeze(getFrontend().globals.globalTypes);

    REQUIRE(!result.success);
    REQUIRE_EQ(result.parseResult.errors.size(), 1);
    CHECK_EQ(result.parseResult.errors[0].getMessage(), "String literal contains malformed escape sequence or \\0");
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

    const ExternType* etv = get<ExternType>(requireType("x"));
    REQUIRE(etv != nullptr);

    REQUIRE(bool(etv->indexer));

    CHECK_EQ(*etv->indexer->indexType, *getBuiltins()->numberType);
    CHECK_EQ(*etv->indexer->indexResultType, *getBuiltins()->stringType);

    CHECK_EQ(toString(requireType("y")), "string");
}

TEST_CASE_FIXTURE(Fixture, "class_definitions_reference_other_extern_types")
{
    loadDefinition(R"(
        declare class Channel
            Messages: { Message }
            OnMessage: (message: Message) -> ()
        end

        declare class Message
            Text: string
            Channel: Channel
        end
    )");

    CheckResult result = check(R"(
        local a: Channel
        local b = a.Messages[1]
        local c = b.Channel
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "Channel");
    CHECK_EQ(toString(requireType("b")), "Message");
    CHECK_EQ(toString(requireType("c")), "Channel");
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

    std::optional<TypeFun> fooTy = getFrontend().globals.globalScope->lookupType("Foo");
    REQUIRE(fooTy);

    const ExternType* etv = get<ExternType>(fooTy->type);

    REQUIRE(etv);
    CHECK_EQ(etv->definitionModuleName, "@test");
}

TEST_CASE_FIXTURE(Fixture, "recursive_redefinition_reduces_rightfully")
{
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local t: {[string]: string} = {}

        local function f()
            t = t
        end

        t = t
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cli_142285_reduce_minted_union_func")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauNoMoreComparisonTypeFunctions, true},
        {FFlag::LuauNoOrderingTypeFunctions, true},
    };

    CheckResult result = check(R"(
        local function middle(a: number, b: number): number
            return math.ceil((a + b) / 2 - 0.5)
        end

        local function find<T>(array: {T}, item: T): number?
            local l, m, r = 1, middle(1, #array), #array
            while l <= r do
                if item <= array[m] then
                    if item == array[m] then return m end
                    m, r = middle(l, m-1), m-1
                else
                    l, m = middle(m+1, r), m+1
                end
            end
        return nil
        end
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err1 = get<CannotInferBinaryOperation>(result.errors[0]);
    REQUIRE(err1);
    CHECK_EQ(err1->suggestedToAnnotate, "item");
    CHECK_EQ(err1->op, AstExprBinary::Op::CompareLe);
}

TEST_CASE_FIXTURE(Fixture, "vector3_overflow")
{
    // We set this to zero to ensure that we either run to completion or stack overflow here.
    ScopedFastInt sfi{FInt::LuauTypeInferRecursionLimit, 0};

    loadDefinition(R"(
        declare class Vector3
            function __add(self, other: Vector3): Vector3
        end
    )");

    CheckResult result = check(R"(
--!strict
local function graphPoint(t : number, points : { Vector3 }) : Vector3
    local n : number = #points - 1
    local p : Vector3 = (nil :: any)
    for i = 0, n do
        local x = points[i + 1]
        p = p and p + x or x
    end
    return p
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
