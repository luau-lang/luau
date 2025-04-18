// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Fixture.h"

#include "Luau/InferPolarity.h"
#include "Luau/Polarity.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"

using namespace Luau;

LUAU_FASTFLAG(LuauNonReentrantGeneralization2);

TEST_SUITE_BEGIN("InferPolarity");

TEST_CASE_FIXTURE(Fixture, "T where T = { m: <a>(a) -> T }")
{
    ScopedFastFlag sff{FFlag::LuauNonReentrantGeneralization2, true};

    TypeArena arena;
    ScopePtr globalScope = std::make_shared<Scope>(builtinTypes->anyTypePack);

    TypeId tType = arena.addType(BlockedType{});
    TypeId aType = arena.addType(GenericType{globalScope.get(), "a"});

    TypeId mType = arena.addType(FunctionType{
        TypeLevel{},
        /* generics */ {aType},
        /* genericPacks */ {},
        /* argPack */ arena.addTypePack({aType}),
        /* retPack */ arena.addTypePack({tType})
    });

    emplaceType<TableType>(
        asMutable(tType),
        TableType{
            TableType::Props{{"m", Property::rw(mType)}},
            /* indexer */ std::nullopt,
            TypeLevel{},
            globalScope.get(),
            TableState::Sealed
        }
    );

    inferGenericPolarities(NotNull{&arena}, NotNull{globalScope.get()}, tType);

    const GenericType* aGeneric = get<GenericType>(aType);
    REQUIRE(aGeneric);
    CHECK(aGeneric->polarity == Polarity::Negative);
}

TEST_SUITE_END();
