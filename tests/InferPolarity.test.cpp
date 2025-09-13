// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Fixture.h"

#include "Luau/InferPolarity.h"
#include "Luau/Polarity.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"

using namespace Luau;


TEST_SUITE_BEGIN("InferPolarity");

TEST_CASE_FIXTURE(Fixture, "T where T = { m: <a>(a) -> T }")
{
    TypeArena arena;
    ScopePtr globalScope = std::make_shared<Scope>(getBuiltins()->anyTypePack);

    TypeId tType = arena.addType(BlockedType{});
    TypeId aType = arena.addType(GenericType{globalScope.get(), "a"});

    TypeId mType = arena.addType(
        FunctionType{
            TypeLevel{},
            /* generics */ {aType},
            /* genericPacks */ {},
            /* argPack */ arena.addTypePack({aType}),
            /* retPack */ arena.addTypePack({tType})
        }
    );

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

TEST_CASE_FIXTURE(Fixture, "<a, b>({ read x: a, write x: b }) -> ()")
{
    TypeArena arena;
    ScopePtr globalScope = std::make_shared<Scope>(getBuiltins()->anyTypePack);

    TypeId aType = arena.addType(GenericType{globalScope.get(), "a"});
    TypeId bType = arena.addType(GenericType{globalScope.get(), "b"});

    TableType ttv;
    ttv.state = TableState::Sealed;
    ttv.props["x"] = Property::create({aType}, {bType});

    TypeId mType = arena.addType(
        FunctionType{
            TypeLevel{},
            /* generics */ {aType, bType},
            /* genericPacks */ {},
            /* argPack */ arena.addTypePack({arena.addType(std::move(ttv))}),
            /* retPack */ builtinTypes->emptyTypePack,
        }
    );

    inferGenericPolarities(NotNull{&arena}, NotNull{globalScope.get()}, mType);

    const GenericType* aGeneric = get<GenericType>(aType);
    REQUIRE(aGeneric);
    CHECK(aGeneric->polarity == Polarity::Negative);

    const GenericType* bGeneric = get<GenericType>(bType);
    REQUIRE(bGeneric);
    CHECK(bGeneric->polarity == Polarity::Positive);
}

TEST_SUITE_END();
