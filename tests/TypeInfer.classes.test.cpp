// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/Error.h"
#include "ScopedFlags.h"
#include "doctest.h"
#include <cfloat>

using namespace Luau;

LUAU_FASTFLAG(DebugLuauUserDefinedClasses)

namespace
{

struct ClassesFixture : Fixture
{
    const std::string definitions = R"LUAU_SRC(
    declare function tostring<T>(value: T): string
)LUAU_SRC";
    Frontend& getFrontend() override
    {
        if (frontend)
            return *frontend;

        Frontend& f = Fixture::getFrontend();
        Luau::unfreeze(f.globals.globalTypes);
        // Can register additional classes here
        f.loadDefinitionFile(f.globals, f.globals.globalScope, definitions, "@test", false);
        Luau::freeze(f.globals.globalTypes);


        return *frontend;
    }
    ScopedFastFlag sff_DebugLuauUserDefinedClasses{FFlag::DebugLuauUserDefinedClasses, true};
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
};

} // namespace

TEST_SUITE_BEGIN("ClassesConformance");

TEST_CASE_FIXTURE(ClassesFixture, "Point_tostring")
{
    ScopedFastFlag sff_DebugLuauUserDefinedClasses{FFlag::DebugLuauUserDefinedClasses, true};
    auto result = check(R"(
class Point
    public x
    public y
    function __tostring(self)
        return `Point(x={self.x}, y={self.y})`
    end
end

local p = Point { x = 1, y = 2 }
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

    function __eq(self, other)
        return self.x == other.x and self.y == other.y
    end
    function zero()
        return Point { x = 0, y = 0 }
    end
end        

local p1 = Point { x = 1, y = 2 }
local p2 = Point { x = 1, y = 2 }
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

local p1 = Point { x = 1, y = 2 }
local p2 = Box { x = 1 }
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
    function __add(self, other)
    end
end

local p = Point {}
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

    function magnitude(self)
        return math.sqrt(self.x * self.x + self.y * self.y)
    end

    function zero()
        return Point { x = 0, y = 0 }
    end

    function __tostring(self)
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
    REQUIRE(et->metatable);

    CHECK(et->props.find("zero") != et->props.end());

    auto cobjmeta = get<TableType>(*et->metatable);
    REQUIRE(cobjmeta);
    auto& cobjMetaProps = cobjmeta->props;
    CHECK(cobjMetaProps.find("__call") != cobjmeta->props.end());
}

TEST_SUITE_END();
