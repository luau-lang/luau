// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Fixture.h"

#include "doctest.h"

LUAU_FASTFLAG(DebugLuauReadWriteProperties)
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);

using namespace Luau;

namespace
{

struct ReadWriteFixture : Fixture
{
    ScopedFastFlag dcr{FFlag::DebugLuauDeferredConstraintResolution, true};

    ReadWriteFixture()
        : Fixture()
    {
        if (!FFlag::DebugLuauReadWriteProperties)
            return;

        TypeArena* arena = &frontend.globals.globalTypes;
        NotNull<Scope> globalScope{frontend.globals.globalScope.get()};

        unfreeze(*arena);

        TypeId genericT = arena->addType(GenericType{"T"});

        TypeId readonlyX = arena->addType(TableType{TableState::Sealed, TypeLevel{}, globalScope});
        getMutable<TableType>(readonlyX)->props["x"] = Property::readonly(genericT);
        globalScope->addBuiltinTypeBinding("ReadonlyX", TypeFun{{{genericT}}, readonlyX});

        freeze(*arena);
    }
};

} // namespace

TEST_SUITE_BEGIN("ReadWriteProperties");

TEST_CASE_FIXTURE(ReadWriteFixture, "read_from_a_readonly_prop")
{
    if (!FFlag::DebugLuauReadWriteProperties)
        return;

    CheckResult result = check(R"(
        function f(rx: ReadonlyX<string>)
            local x = rx.x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ReadWriteFixture, "write_to_a_readonly_prop")
{
    if (!FFlag::DebugLuauReadWriteProperties)
        return;

    CheckResult result = check(R"(
        function f(rx: ReadonlyX<string>)
            rx.x = "hello!" -- error
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_SUITE_END();
