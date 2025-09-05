// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Instantiation2.h"

#include "Fixture.h"
#include "ClassFixture.h"
#include "Luau/Type.h"
#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("Instantiation2Test");

TEST_CASE_FIXTURE(Fixture, "weird_cyclic_instantiation")
{
    TypeArena arena;
    Scope scope(getBuiltins()->anyTypePack);

    TypeId genericT = arena.addType(GenericType{"T"});

    TypeId idTy = arena.addType(
        FunctionType{/* generics */ {genericT},
                     /* genericPacks */ {},
                     /* argTypes */ arena.addTypePack({genericT}),
                     /* retTypes */ arena.addTypePack({genericT})}
    );

    DenseHashMap<TypeId, TypeId> genericSubstitutions{nullptr};
    DenseHashMap<TypePackId, TypePackId> genericPackSubstitutions{nullptr};

    TypeId freeTy = arena.freshType(getBuiltins(), &scope);
    FreeType* ft = getMutable<FreeType>(freeTy);
    REQUIRE(ft);
    ft->lowerBound = idTy;
    ft->upperBound = getBuiltins()->unknownType;

    genericSubstitutions[genericT] = freeTy;

    CHECK("<T>(T) -> T" == toString(idTy));

    std::optional<TypeId> res = instantiate2(&arena, std::move(genericSubstitutions), std::move(genericPackSubstitutions), idTy);

    // Substitutions should not mutate the original type!
    CHECK("<T>(T) -> T" == toString(idTy));

    REQUIRE(res);
    CHECK("<T>(T) -> T" == toString(*res));
}

TEST_SUITE_END();
