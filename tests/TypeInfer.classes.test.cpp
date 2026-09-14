// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/BuiltinDefinitions.h"
#include "Luau/Error.h"
#include "ScopedFlags.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauUserDefinedClasses)
LUAU_FASTFLAG(LuauAllowGlobalDeclarationToBeCalledClass);
LUAU_FASTFLAG(LuauIntegerType2)
LUAU_FASTFLAG(LuauExportValueSyntax)
LUAU_FASTFLAG(LuauExportValueTypecheck)

namespace
{

struct ClassesFixture : Fixture
{
    const std::string definitions = R"LUAU_SRC(
@checked declare function require(target: any): any
declare function sqrt(n: number): number
declare function tostring<T>(value: T): string

declare class: {
    isinstance: @checked (o: unknown, c: class) -> boolean,
    classof: @checked (o: unknown) -> class?
}
)LUAU_SRC";
    Frontend& getFrontend() override
    {
        if (frontend)
            return *frontend;

        Frontend& f = Fixture::getFrontend();
        Luau::unfreeze(f.globals.globalTypes);

        f.loadDefinitionFile(f.globals, f.globals.globalScope, definitions, "@test", false);
        AstName reqName = f.globals.globalNames.names->getOrAdd("require");
        auto it = f.globals.globalScope->bindings.find(reqName);
        LUAU_ASSERT(it != f.globals.globalScope->bindings.end());
        attachTag(it->second.typeId, kRequireTagName);
        attachMagicFunction(it->second.typeId, std::make_shared<MagicRequire>());
        registerTestTypes();
        Luau::freeze(f.globals.globalTypes);


        return *frontend;
    }
    ScopedFastFlag sff_DebugLuauUserDefinedClasses{FFlag::DebugLuauUserDefinedClasses, true};
    ScopedFastFlag sff_LuauAllowGlobalDeclarationToBeCalledClass{FFlag::LuauAllowGlobalDeclarationToBeCalledClass, true};
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
};

} // namespace

TEST_SUITE_BEGIN("Classes");

TEST_CASE_FIXTURE(Fixture, "classes_arent_in_old_solver")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauUserDefinedClasses, true},
        {FFlag::DebugLuauForceOldSolver, true},
    };

    CheckResult result = check(R"( class Point end )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err = get<GenericError>(result.errors[0]);
    REQUIRE(err);
    CHECK_EQ("class keyword is illegal here", err->message);
}

TEST_CASE_FIXTURE(Fixture, "export_class_isnt_in_old_solver")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauUserDefinedClasses, true},
        {FFlag::DebugLuauForceOldSolver, true},
    };

    CheckResult result = check(R"( export class Point end )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err = get<GenericError>(result.errors[0]);
    REQUIRE(err);
    CHECK_EQ("class keyword is illegal here", err->message);
}

TEST_CASE_FIXTURE(ClassesFixture, "empty_class")
{
    CheckResult result = check(R"( class Point end )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "class_decl")
{
    CheckResult result = check(R"(
        class Point
            public x: number
            public y: number
        end

        local p = Point.new { x = 2, y = 3 }

        local x = p.x
        local y = p.y
    )");

    LUAU_CHECK_NO_ERRORS(result);

    TypeId t = requireTypeAlias("Point");
    CHECK("Point" == toString(t));

    const ExternType* point = get<ExternType>(t);
    REQUIRE(point);

    CHECK("Point" == toString(requireType("p")));
    CHECK("number" == toString(requireType("x")));
    CHECK("number" == toString(requireType("y")));
}

TEST_CASE_FIXTURE(ClassesFixture, "point_class")
{
    CheckResult result = check(R"(
        class Point
            public x: number
            public y: number

            function length(self): number
                return 100
            end

            function __init(self, x: number, y: number)
                self.x = x
                self.y = y
            end
        end

        local p = Point.new(2, 3)
        local len = p:length()
    )");

    LUAU_CHECK_NO_ERRORS(result);

    TypeId p = requireType("p");
    const ExternType* et = get<ExternType>(p);
    REQUIRE(et);

    CHECK("Point" == toString(requireType("p")));
    CHECK("number" == toString(requireType("len")));
}

TEST_CASE_FIXTURE(ClassesFixture, "self_argument_has_self_type")
{
    CheckResult result = check(R"(
        class I
            function m(self): I
                return self
            end
        end

        local i = I.new{}
        local i2 = i:m()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("I" == toString(requireType("i2")));
}

TEST_CASE_FIXTURE(ClassesFixture, "Point_tostring")
{
    ScopedFastFlag sff_DebugLuauUserDefinedClasses{FFlag::DebugLuauUserDefinedClasses, true};
    auto result = check(R"(
class Point
    public x
    public y
    function __tostring(self): string
        return `Point(x={self.x}, y={self.y})`
    end
end

local p = Point.new { x = 1, y = 2 }
local _ = tostring(p)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}


TEST_CASE_FIXTURE(ClassesFixture, "Point_eq_mm")
{
    auto result = check(R"(
class Point
    public x
    public y

    function __eq(self, other: Point): boolean
        return self.x == other.x and self.y == other.y
    end
    function zero(): Point
        return Point.new { x = 0, y = 0 }
    end
end

local p1 = Point.new { x = 1, y = 2 }
local p2 = Point.new { x = 1, y = 2 }
local _ = p1 == p2
local _ = p1 ~= Point.zero()
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "Box_Point_no_eq")
{
    auto result = check(R"(
class Point
    public x
    public y
end


class Box
    public x
end

local p1 = Point.new { x = 1, y = 2 }
local p2 = Box.new { x = 1 }
local _ = p1 == p1
-- This one too
local _ = p1 ~= p2
local _ = Box == Box
-- This line should error...
local _ = Point ~= Box
)");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    auto e1 = get<CannotCompareUnrelatedTypes>(result.errors[0]);
    auto e2 = get<CannotCompareUnrelatedTypes>(result.errors[1]);
    REQUIRE(e1);
    REQUIRE(e2);

    CHECK(result.errors[0].location.begin.line == 15);
    CHECK(result.errors[1].location.begin.line == 18);
}

TEST_CASE_FIXTURE(ClassesFixture, "class_mm")
{
    auto result = check(R"(
class Point
    function __add(self, other: unknown)
    end
end

local p = Point.new {}
p:__add()
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "class_structure")
{
    auto result = check(R"(
class Point
    public x
    public y

    function magnitude(self): number
        return sqrt(self.x * self.x + self.y * self.y)
    end

    function zero(): Point
        return Point.new { x = 0, y = 0 }
    end

    function __tostring(self): string
        return `Point(x={self.x}, y={self.y})`
    end

end

local p = Point
)");

    LUAU_REQUIRE_NO_ERRORS(result);
    auto t = requireType("p");
    auto et = get<ExternType>(t);
    REQUIRE(et);
    CHECK(et->parent == builtinTypes->classType);

    CHECK(et->props.count("zero") == 1);
    CHECK(et->props.count("new") == 1);
}

TEST_CASE_FIXTURE(ClassesFixture, "isinstance_refines_unknown_value")
{
    ScopedFastFlag sff{FFlag::LuauIntegerType2, true};
    CheckResult result = check(R"(
class Point
    public x
end

local function f(v: unknown)
    if class.isinstance(v, Point) then
        local s = v
    else
        local s = v
    end
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("Point", toString(requireTypeAtPosition({7, 18})));
    CHECK_EQ(
        "((userdata & ~Point) | boolean | buffer | function | integer | number | string | table | thread)?", toString(requireTypeAtPosition({9, 18}))
    );
}

TEST_CASE_FIXTURE(ClassesFixture, "isinstance_refines_union_value")
{
    CheckResult result = check(R"(
class Point
    public x
end

local function f(v: Point | string)
    if class.isinstance(v, Point) then
        local s = v
    else
        local s = v
    end
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("Point", toString(requireTypeAtPosition({7, 18})));
    CHECK_EQ("string", toString(requireTypeAtPosition({9, 18})));
}

TEST_CASE_FIXTURE(ClassesFixture, "not_isinstance_refines_union")
{
    CheckResult result = check(R"(
class Point
    public x
end

local function f(v: Point | string)
    if not class.isinstance(v, Point) then
        local s = v
    else
        local s = v
    end
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({7, 18})));
    CHECK_EQ("Point", toString(requireTypeAtPosition({9, 18})));
}

TEST_CASE_FIXTURE(ClassesFixture, "not_isinstance_refines_unknown")
{
    CheckResult result = check(R"(
class Point
    public x
end

local function f(v: unknown)
    if not class.isinstance(v, Point) then
        local s = v
    else
        local s = v
    end
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("Point", toString(requireTypeAtPosition({9, 18})));
}

TEST_CASE_FIXTURE(ClassesFixture, "isinstance_refines_optional_property")
{
    CheckResult result = check(R"(
class Point
    public x
end

local function f(t: { x: Point? })
    if t.x and class.isinstance(t.x, Point) then
        local s = t.x
    end
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("Point", toString(requireTypeAtPosition({7, 20})));
}

TEST_CASE_FIXTURE(ClassesFixture, "isinstance_refines_property_already_typed")
{
    CheckResult result = check(R"(
class Point
    public x
end

local function f(t: { x: Point })
    if class.isinstance(t.x, Point) then
        local s = t.x
    end
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("Point", toString(requireTypeAtPosition({7, 20})));
}

TEST_CASE_FIXTURE(ClassesFixture, "isinstance_refines_imported_class")
{
    ScopedFastFlag _[2]{{FFlag::LuauExportValueSyntax, true}, {FFlag::LuauExportValueTypecheck, true}};

    fileResolver.source["game/A"] = R"(
        export class Point
            public x: number
        end
    )";

    fileResolver.source["game/B"] = R"(
        local A = require(game.A)

        local x : unknown = (A.Point.new { x = 0 } ) :: any
        if class.isinstance(x, A.Point) then
            local y = x
        end
    )";
    CheckResult modB = getFrontend().check("game/B");
    LUAU_REQUIRE_NO_ERRORS(modB);
    CHECK_EQ("Point", toString(requireTypeAtPosition("game/B", {5, 22})));
}

TEST_CASE_FIXTURE(ClassesFixture, "isinstance_refines_imported_class_but_not_a_class")
{
    ScopedFastFlag _[2]{{FFlag::LuauExportValueSyntax, true}, {FFlag::LuauExportValueTypecheck, true}};

    fileResolver.source["game/A"] = R"(
        export class Point
            public x: number
        end

        export const notAPoint = nil
    )";

    fileResolver.source["game/B"] = R"(
        local A = require(game.A)

        local x : unknown = (A.Point.new { x = 0 } ) :: any
        if class.isinstance(x, A.notAPoint) then
            local y = x
        end
    )";
    CheckResult modA = getFrontend().check("game/A");
    CheckResult modB = getFrontend().check("game/B");
    LUAU_REQUIRE_ERROR_COUNT(1, modB);
    // There's an unknown property on A.foo, but
    LUAU_REQUIRE_ERROR(modB, TypeMismatch);
    auto err = get<TypeMismatch>(modB.errors[0]);
    CHECK_EQ("class", toString(err->wantedType));
    CHECK_EQ("nil", toString(err->givenType));
}

TEST_CASE_FIXTURE(ClassesFixture, "typed_self_parameter_after_class_declaration")
{
    // Annotations on the self parameter are forbidden, but we still have to
    // parse this without crashing.
    CheckResult result = check(R"(
        class Q
            function f(self: number) end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    auto e0 = get<SyntaxError>(result.errors[0]);
    REQUIRE(e0);
    CHECK("The 'self' parameter cannot have a type annotation" == e0->message);

    auto e1 = get<TypeMismatch>(result.errors[1]);
    REQUIRE(e1);
    CHECK("number" == toString(e1->wantedType));
    CHECK("Q" == toString(e1->givenType));
}

TEST_CASE_FIXTURE(ClassesFixture, "typeof_class_prop_ice")
{
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local x = 1
        class Foo
            public bar: typeof(x)
        end
    )"));
}

TEST_CASE_FIXTURE(ClassesFixture, "typeof_indexing_ice_in_class_prop_typeof")
{
    CheckResult results = check(R"(
local A = ""
class B
    public C: { _: typeof(A.D) }
end
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, results);
    auto err = get<UnknownProperty>(results.errors[0]);
    REQUIRE(err);
    CHECK_EQ("D", err->key);
}

TEST_CASE_FIXTURE(ClassesFixture, "class_refers_to_later_type_alias")
{
    CheckResult result = check(R"(
        class Foo
            public bar: BarType
        end

        type BarType = number | string

        local function getbar(f: Foo)
            return f.bar
        end
    )");
    ignoreMissingAnnotations(result);
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("(Foo) -> number | string", toString(requireType("getbar")));
}

TEST_CASE_FIXTURE(ClassesFixture, "accept_read_only_tables")
{
    CheckResult result = check(R"(
        class Foo
            public bar: number | string
        end

        local function ofnumbertbl(tbl: { bar: number })
            return Foo.new(tbl)
        end

        local function inference(tbl)
            return Foo.new(tbl)
        end
    )");
    ignoreMissingAnnotations(result);
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("({ bar: number }) -> Foo", toString(requireType("ofnumbertbl")));
    CHECK_EQ("({ read bar: number | string }) -> Foo", toString(requireType("inference")));
}

TEST_CASE_FIXTURE(ClassesFixture, "fuzzy_classes_crash")
{
    // TODO CLI-201171: This should be an error, but at least it doesn't crash.
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        class sqrt extends sqrt
        end
    )"));
}

TEST_CASE_FIXTURE(ClassesFixture, "constructors_must_accept_self")
{
    CheckResult res = check(R"(
        class Point2
            public x: number
            public y: number

            function __init(x: number, y: number) end
        end

        class Point3
            function __init() end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, res);
    auto e1 = get<SyntaxError>(res.errors[0]);
    REQUIRE(e1);
    CHECK_EQ(e1->message, R"(__init's first parameter must be named 'self'.)");
    auto e2 = get<SyntaxError>(res.errors[1]);
    REQUIRE(e2);
    CHECK_EQ(e2->message, R"(__init must have at least one parameter.)");
}

TEST_CASE_FIXTURE(ClassesFixture, "refer_to_uninitialized_field")
{
    CheckResult result = check(R"(
        local something

        class Foo
            public x: number
            function __init(self)
                something = self.x
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto e = get<UninitializedFieldAccess>(result.errors[0]);
    REQUIRE(e);
    REQUIRE(e->fieldName);
    CHECK("x" == *e->fieldName);
}

TEST_CASE_FIXTURE(ClassesFixture, "refer_to_uninitialized_field_index_string_expr")
{
    CheckResult result = check(R"(
        local something

        class Foo
            public x: number
            function __init(self)
                something = self["x"]
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto e = get<UninitializedFieldAccess>(result.errors[0]);
    REQUIRE(e);
    REQUIRE(e->fieldName);
    CHECK("x" == *e->fieldName);
}

TEST_CASE_FIXTURE(ClassesFixture, "refer_to_uninitialized_field_index_computed_index")
{
    CheckResult result = check(R"(
        local something

        class Foo
            public xy: number
            function __init(self)
                something = self["x" .. "y"]
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    auto e1 = get<DynamicPropertyLookupOnExternTypesUnsafe>(result.errors[0]);
    REQUIRE(e1);
    CHECK_EQ("Foo", toString(e1->ty));
    auto e2 = get<UninitializedFieldAccess>(result.errors[1]);
    REQUIRE(e2);
    // The type checker only reports specific field errors for constant strings, so we just report the error on self in this case
    REQUIRE(!e2->fieldName.has_value());
}

TEST_CASE_FIXTURE(ClassesFixture, "reference_to_shadowed_self_is_absurd_but_ok")
{
    CheckResult result = check(R"(
        local something

        class Foo
            function __init(self)
                local self = {}
                something = self
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "self_referential_assign")
{
    CheckResult result = check(R"(
        class Foo
            public x: number
            public y: number
            function __init(self)
                self.x, self.y = self.y, self.x
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    auto e0 = get<UninitializedFieldAccess>(result.errors[0]);
    REQUIRE(e0);
    REQUIRE(e0->fieldName);
    auto e1 = get<UninitializedFieldAccess>(result.errors[1]);
    REQUIRE(e1);
    REQUIRE(e1->fieldName);
    // Both `self.x` and `self.y` are read before either is initialized; the
    // two errors are collected from a hash map, so their order isn't fixed.
    CHECK(std::set<std::string>{*e0->fieldName, *e1->fieldName} == std::set<std::string>{"x", "y"});
}

// It would be nice to afford this someday.
TEST_CASE_FIXTURE(ClassesFixture, "conditional_assignment_is_not_yet_allowed")
{
    CheckResult result = check(R"(
        class Foo
            public x: number
            public y: number
            function __init(self, b: boolean)
                if b then
                    self.x = 0
                else
                    self.x = 2
                end
                self.y = self.x
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto e = get<UninitializedFieldAccess>(result.errors[0]);
    REQUIRE(e);
    REQUIRE(e->fieldName);
    CHECK("x" == *e->fieldName);
}

// It would be nice to afford this someday.
TEST_CASE_FIXTURE(ClassesFixture, "ok_conditional_assignment")
{
    CheckResult result = check(R"(
        class Foo
            public x: number
            public y: number
            function __init(self, b: boolean)
                self.x = if b then 0 else 2
                self.y = self.x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "all_fields_initialized_before_use")
{
    CheckResult result = check(R"(
        local something

        class Foo
            public x: number
            function __init(self)
                self.x = 5
                something = self.x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "pass_self_before_initialization")
{
    CheckResult result = check(R"(
        local function doSomething(x: unknown) end

        class Foo
            public x: number
            function __init(self)
                doSomething(self)
                self.x = 0
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto e = get<UninitializedFieldAccess>(result.errors[0]);
    REQUIRE(e);
    CHECK(!e->fieldName);
}

TEST_CASE_FIXTURE(ClassesFixture, "pass_self_after_initialization")
{
    CheckResult result = check(R"(
        local function doSomething(x: unknown) end

        class Foo
            public x: number
            function __init(self)
                self.x = 0
                doSomething(self)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "read_nested_field_of_uninitialized")
{
    CheckResult result = check(R"(
        local something

        class Foo
            public x: {y: number}
            function __init(self)
                something = self.x.y
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto e = get<UninitializedFieldAccess>(result.errors[0]);
    REQUIRE(e);
    REQUIRE(e->fieldName);
    CHECK("x" == *e->fieldName);
}

TEST_CASE_FIXTURE(ClassesFixture, "partial_initialization_order")
{
    CheckResult result = check(R"(
        local something

        class Foo
            public x: number
            public y: number
            function __init(self)
                self.x = 0
                something = self.x
                something = self.y
                self.y = 1
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto e = get<UninitializedFieldAccess>(result.errors[0]);
    REQUIRE(e);
    REQUIRE(e->fieldName);
    CHECK("y" == *e->fieldName);
}

TEST_CASE_FIXTURE(ClassesFixture, "field_read_inside_closure")
{
    // This is technically safe, maybe in the future we have more sophisticated logic to allow this
    CheckResult result = check(R"(
        class Foo
            public x: number
            function __init(self)
                local f = function() return self.x end
                self.x = 0
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto e = get<UninitializedFieldAccess>(result.errors[0]);
    REQUIRE(e);
    REQUIRE(e->fieldName);
    CHECK("x" == *e->fieldName);
}

TEST_CASE_FIXTURE(ClassesFixture, "shadowing_self_via_closure")
{
    CheckResult result = check(R"(
        class Foo
            public x: number
            function __init(self)
                local f = function(self) return self.x end
                self.x = 0
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "no_fields_no_errors")
{
    CheckResult result = check(R"(
        local function doSomething(x: any) end

        class Foo
            function __init(self)
                doSomething(self)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "nilable_fields_dont_need_initialization")
{
    CheckResult result = check(R"(
        local function doSomething(...: any) end

        class Foo
            public x: number
            public y: number?
            public z: any
            public w: unknown
            function __init(self)
                doSomething(self.x, self.y, self.z, self.w)
            end
        end
    )");

    // Access to x is bad.  y, z, and w are all fine.
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto e = get<UninitializedFieldAccess>(result.errors[0]);
    REQUIRE(e);
    REQUIRE(e->fieldName);
    CHECK("x" == *e->fieldName);
}

TEST_CASE_FIXTURE(ClassesFixture, "unannotated_field_doesnt_need_initialization")
{
    CheckResult result = check(R"(
        class Foo
            public x
            public y: number
            function __init(self)
                self.y = 0
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "pass_self_with_nilable_fields_unassigned")
{
    CheckResult result = check(R"(
        local function doSomething(x: unknown) end

        class Foo
            public x: number
            public y: string?
            function __init(self)
                self.x = 0
                doSomething(self)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "read_nilable_field_before_assign")
{
    CheckResult result = check(R"(
        local something

        class Foo
            public x: number?
            function __init(self)
                something = self.x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "read_error_suppressing_field_before_assign")
{
    // TODO: CLI-222651: This shouldn't error because the annotation on x is error suppressing
    CheckResult result = check(R"(
        local something

        class Foo
            public x: string & any
            function __init(self)
                something = self.x
            end
        end
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "type_assertion_loophole")
{
    CheckResult result = check(R"(
        local something: any

        class Foo
            public x: number
            function __init(self)
                something = self :: Foo
                something = (self :: Foo).x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "method_calls_require_full_initialization")
{
    CheckResult result = check(R"(
        class Foo
            public x: number
            function __init(self)
                self:increment()
            end

            function increment(self)
                self.x += 1
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto e = get<UninitializedFieldAccess>(result.errors[0]);
    REQUIRE(e);
    CHECK(!e->fieldName);
}

TEST_CASE_FIXTURE(ClassesFixture, "method_calls_on_fully_initialized_instances_are_ok")
{
    CheckResult result = check(R"(
        class Foo
            public x: number
            function __init(self)
                self.x = 0
                self:increment()
            end

            function increment(self)
                self.x += 1
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassesFixture, "variadic_constructor")
{
    CheckResult result = check(R"(
        class Foo
            public values: {number}
            function __init(self, ...: number)
                self.values = {...}
            end
        end

        local f = Foo.new(3, 4, 5) -- OK
        local g = Foo.new(3, 4, 5, "six") -- Error
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(9 == result.errors[0].location.begin.line);
}

TEST_CASE_FIXTURE(ClassesFixture, "variadic_constructor_with_leading_positional_arguments")
{
    CheckResult result = check(R"(
        class Foo
            public x: number
            public y: string
            public values: {number}
            function __init(self, x: number, y: string, ...: number)
                self.x = x
                self.y = y
                self.values = {...}
            end
        end

        local f = Foo.new(3, "four", 5) -- OK
        local g = Foo.new(3, "four", 5, "six") -- Error
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(13 == result.errors[0].location.begin.line);
}

TEST_CASE_FIXTURE(ClassesFixture, "class_methods_should_be_annotated_except_for_self")
{
    CheckResult result = check(R"(
        class Foo
            public x: number

            function __init(self, x)
                self.x = x
            end

            function double(this)
                return Foo.new(this.x * 2)
            end

            function double2(self): Foo
                return Foo.new(self.x * 2)
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);
    CHECK_ERROR_IS(result.errors.at(0), TypeAnnotationRequired);
    // We can't figure out the multiplication
    CHECK_ERROR_IS(result.errors.at(1), UninhabitedTypeFunction);
    CHECK_ERROR_IS(result.errors.at(2), TypeAnnotationRequired);

    CHECK(4 == result.errors.at(0).location.begin.line);
    CHECK(9 == result.errors.at(1).location.begin.line);
    CHECK(8 == result.errors.at(2).location.begin.line);
}

TEST_CASE_FIXTURE(ClassesFixture, "fuzzer_duplicate_class_definition")
{
    CheckResult result = check(R"(
        class l0
        end
        class l0
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err = get<SyntaxError>(result.errors[0]);
    REQUIRE(err);
    CHECK_EQ("A class named 'l0' has already been declared in this module", err->message);
}

TEST_CASE_FIXTURE(ClassesFixture, "repeat_props")
{
    CheckResult result = check(
        R"(
class l0
    public foo
    public foo
end
)"
    );
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err = get<SyntaxError>(result.errors[0]);
    REQUIRE(err);
    CHECK_EQ("Duplicate class member 'foo'", err->message);
}

TEST_CASE_FIXTURE(ClassesFixture, "repeat_class_methods")
{
    CheckResult result = check(
        R"(
class l0
    function foo()
    end
    function foo()
    end
end
)"
    );

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err = get<SyntaxError>(result.errors[0]);
    REQUIRE(err);
    CHECK_EQ("Duplicate class member 'foo'", err->message);
}

TEST_CASE_FIXTURE(ClassesFixture, "repeat_nameless_class_methods")
{
    CheckResult result = check(
        R"(
class l0
    function  ()
    end
    function ()
    end
end
)"
    );

    LUAU_REQUIRE_ERROR_COUNT(3, result);
    auto err1 = get<SyntaxError>(result.errors[0]);
    REQUIRE(err1);
    CHECK_EQ("Expected identifier when parsing method name, got '('", err1->message);
    auto err2 = get<SyntaxError>(result.errors[1]);
    REQUIRE(err2);
    CHECK_EQ("Expected identifier when parsing method name, got '('", err2->message);
    auto err3 = get<SyntaxError>(result.errors[2]);
    REQUIRE(err3);
    CHECK_EQ(R"(Duplicate class member '%error-id%')", err3->message);
}

TEST_CASE_FIXTURE(ClassesFixture, "fuzzer_self_referential_class_definition")
{
    CheckResult result = check(R"(
        class l0
            public _:typeof(l0)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    TypeId l0 = requireType("l0");
    CHECK(is<ExternType>(l0));
}

TEST_CASE_FIXTURE(ClassesFixture, "instantiate_duplicate_class")
{
    CheckResult result = check(
        R"(
class l0
end
class l0
end
_ = l0 {  }
)"
    );

    LUAU_REQUIRE_ERROR_COUNT(3, result);
    auto err = get<SyntaxError>(result.errors[0]);
    REQUIRE(err);
    CHECK_EQ("A class named 'l0' has already been declared in this module", err->message);
    REQUIRE(get<UnknownSymbol>(result.errors[1]));
    REQUIRE(get<CannotCallNonFunction>(result.errors[2]));
}

TEST_CASE_FIXTURE(ClassesFixture, "prop_with_typeof_reassigned_class")
{
    ScopedFastFlag sff{FFlag::LuauExportValueSyntax, true};

    // This should not assert or crash
    CheckResult result = check(
        R"(
class Animal end
Animal = nil
class l0
public _:typeof(Animal)
end
)"
    );

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err = get<SyntaxError>(result.errors[0]);
    REQUIRE(err);
    CHECK_EQ("'Animal' refers to a class and cannot be used as a variable name (defined on line 2)", err->message);
}

TEST_CASE_FIXTURE(ClassesFixture, "class_that_shadows_a_type_alias")
{
    CheckResult result = check(R"(
        type AAA = { x: number }
        class AAA end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err = get<DuplicateTypeDefinition>(result.errors[0]);
    REQUIRE(err);
    CHECK(err->name == "AAA");
    CHECK(err->previousLocation.has_value());
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typecheck_class_method_field_access")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauForceOldSolver, false},
        {FFlag::DebugLuauUserDefinedClasses, true},
    };

    CheckResult result = check(R"(
        class Point
            public x: number?
            public y: number?
            function magnitude(self): number
                return math.sqrt(self.x * self.x + self.y * self.y)
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);

    for (const auto& err : result.errors)
    {
        auto* utf = get<UninhabitedTypeFunction>(err);
        REQUIRE(utf);
        CHECK_EQ(toString(utf->ty), "mul<number?, number?>");
    }
}


TEST_CASE_FIXTURE(BuiltinsFixture, "typecheck_class_annotations")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauForceOldSolver, false},
        {FFlag::DebugLuauUserDefinedClasses, true},
    };

    CheckResult result = check(R"(
        class Point
            public x: number
            public y: number
            public name: string
            function magnitude(self): string
                -- self.name is not a number
                self.name = self.x

                -- This function is declared to return string.
                return math.sqrt(self.x * self.x + self.y * self.y)
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    LUAU_REQUIRE_ERROR(result, TypeMismatch);
    LUAU_REQUIRE_ERROR(result, TypePackMismatch);
}


TEST_CASE_FIXTURE(ClassesFixture, "read_unknown_property_from_class_object_or_instance")
{
    CheckResult result = check(R"(
        class Point
            public x: number
            public y: number

            function zero(): Point
                return Point.new {x=0, y=0}
            end
        end

        local p = Point.zero()
        local a = p.z
        local b = Point.z
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    auto* up0 = get<UnknownProperty>(result.errors[0]);
    REQUIRE(up0);
    CHECK(up0->key == "z");

    auto* up1 = get<UnknownProperty>(result.errors[1]);
    REQUIRE(up1);
    CHECK(up1->key == "z");
}

TEST_CASE_FIXTURE(ClassesFixture, "writes_to_class_object_properties_are_forbidden")
{
    CheckResult result = check(R"(
        class Point
            public x: number
            public y: number

            function zero(): Point
                return Point.new {x=0, y=0}
            end

            function magnitude(self): number
                return 5 -- stochastic approximation for performance
            end
        end

        Point.magnitude = function(p: Point) return 3 end
        Point.zero = function() return Point.new { x = 1, y = 1 } end
        Point.one = function() return Point.new { x = 1, y = 1 } end
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);

    auto* pav0 = get<PropertyAccessViolation>(result.errors[0]);
    REQUIRE(pav0);
    CHECK(pav0->key == "magnitude");
    CHECK(pav0->context == PropertyAccessViolation::CannotWrite);

    auto* pav1 = get<PropertyAccessViolation>(result.errors[1]);
    REQUIRE(pav1);
    CHECK(pav1->key == "zero");
    CHECK(pav1->context == PropertyAccessViolation::CannotWrite);

    auto* pav2 = get<PropertyAccessViolation>(result.errors[2]);
    REQUIRE(pav2);
    CHECK(pav2->key == "one");
    CHECK(pav2->context == PropertyAccessViolation::CannotWrite);
}


TEST_CASE_FIXTURE(ClassesFixture, "writes_to_unknown_class_instance_properties_are_forbidden")
{
    CheckResult result = check(R"(
        class Point
            public x: number
            public y: number

            function zero(): Point
                return Point.new {x=0, y=0}
            end

            function magnitude(self): number
                return 5 -- stochastic approximation for performance
            end
        end

        local p = Point.zero()

        p.magnitude = function(p: Point) return 3 end
        p.zero = function() return Point.new { x = 1, y = 1 } end
        p.one = function() return Point.new { x = 1, y = 1 } end

        p.__index = {}
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);

    auto* pav0 = get<PropertyAccessViolation>(result.errors[0]);
    REQUIRE(pav0);
    CHECK(pav0->key == "magnitude");
    CHECK(pav0->context == PropertyAccessViolation::CannotWrite);

    auto* pav1 = get<PropertyAccessViolation>(result.errors[1]);
    REQUIRE(pav1);
    CHECK(pav1->key == "zero");
    CHECK(pav1->context == PropertyAccessViolation::CannotWrite);

    auto* pav2 = get<PropertyAccessViolation>(result.errors[2]);
    REQUIRE(pav2);
    CHECK(pav2->key == "one");
    CHECK(pav2->context == PropertyAccessViolation::CannotWrite);

    auto* pav3 = get<PropertyAccessViolation>(result.errors[3]);
    REQUIRE(pav3);
    CHECK(pav3->key == "__index");
    CHECK(pav3->context == PropertyAccessViolation::CannotWrite);
}

TEST_SUITE_END();
