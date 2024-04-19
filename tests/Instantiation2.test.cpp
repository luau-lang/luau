// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Instantiation2.h"

#include "Fixture.h"
#include "ClassFixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("Instantiation2Test");

TEST_CASE_FIXTURE(Fixture, "weird_cyclic_instantiation")
{
    TypeArena arena;
    Scope scope(builtinTypes->anyTypePack);

    TypeId genericT = arena.addType(GenericType{"T"});

    TypeId idTy = arena.addType(FunctionType{/* generics */ {genericT},
        /* genericPacks */ {},
        /* argTypes */ arena.addTypePack({genericT}),
        /* retTypes */ arena.addTypePack({genericT})});

    DenseHashMap<TypeId, TypeId> genericSubstitutions{nullptr};
    DenseHashMap<TypePackId, TypePackId> genericPackSubstitutions{nullptr};

    TypeId freeTy = arena.freshType(&scope);
    FreeType* ft = getMutable<FreeType>(freeTy);
    REQUIRE(ft);
    ft->lowerBound = idTy;
    ft->upperBound = builtinTypes->unknownType;

    genericSubstitutions[genericT] = freeTy;

    CHECK("<T>(T) -> T" == toString(idTy));

    std::optional<TypeId> res = instantiate2(&arena, std::move(genericSubstitutions), std::move(genericPackSubstitutions), idTy);

    // Substitutions should not mutate the original type!
    CHECK("<T>(T) -> T" == toString(idTy));

    // Weird looking because we haven't properly clipped the generic from the
    // function type, but this is what we asked for.
    REQUIRE(res);
    CHECK("<<T>(T) -> T>(<T>(T) -> T) -> <T>(T) -> T" == toString(*res));
}

TEST_SUITE_END();
