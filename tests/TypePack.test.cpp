// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

namespace
{
struct TypePackFixture
{
    TypePackFixture()
    {
        typeVars.emplace_back(new Type(PrimitiveType(PrimitiveType::NilType)));
        typeVars.emplace_back(new Type(PrimitiveType(PrimitiveType::Boolean)));
        typeVars.emplace_back(new Type(PrimitiveType(PrimitiveType::Number)));
        typeVars.emplace_back(new Type(PrimitiveType(PrimitiveType::String)));

        for (const auto& ptr : typeVars)
            types.push_back(ptr.get());
    }

    TypePackId freshTypePack()
    {
        typePacks.emplace_back(new TypePackVar{FreeTypePack{TypeLevel{}}});
        return typePacks.back().get();
    }

    TypePackId newTypePack(std::initializer_list<TypeId> types, std::optional<TypePackId> tail)
    {
        typePacks.emplace_back(new TypePackVar{TypePack{types, tail}});
        return typePacks.back().get();
    }

    std::vector<std::unique_ptr<TypePackVar>> typePacks;

    std::vector<std::unique_ptr<Type>> typeVars;
    std::vector<TypeId> types;
};

} // namespace

TEST_SUITE_BEGIN("TypePackTests");

TEST_CASE_FIXTURE(TypePackFixture, "type_pack_hello")
{
    auto tp = TypePackVar{TypePack{{types[0], types[1]}, std::nullopt}};

    CHECK(tp == tp);
}

TEST_CASE_FIXTURE(TypePackFixture, "first_chases_Bound_TypePackVars")
{
    Type nilType{PrimitiveType{PrimitiveType::NilType}};

    auto tp1 = TypePackVar{TypePack{{&nilType}, std::nullopt}};

    auto tp2 = TypePackVar{BoundTypePack{&tp1}};

    auto tp3 = TypePackVar{TypePack{{}, &tp2}};

    CHECK_EQ(first(&tp3), &nilType);
}

TEST_CASE_FIXTURE(TypePackFixture, "iterate_over_TypePack")
{
    TypePackId typePack = newTypePack({types[0], types[1]}, std::nullopt);

    std::vector<TypeId> res;
    for (TypeId t : typePack)
        res.push_back(t);

    REQUIRE_EQ(2, res.size());
}

TEST_CASE_FIXTURE(TypePackFixture, "iterate_over_TypePack_with_2_links")
{
    auto typePack1 = newTypePack({types[0], types[1]}, std::nullopt);
    auto typePack2 = newTypePack({types[0], types[3]}, typePack1);

    std::vector<TypeId> result;
    for (TypeId ty : typePack2)
        result.push_back(ty);

    REQUIRE_EQ(4, result.size());

    CHECK_EQ(types[0], result[0]);
    CHECK_EQ(types[3], result[1]);
    CHECK_EQ(types[0], result[2]);
    CHECK_EQ(types[1], result[3]);
}

TEST_CASE_FIXTURE(TypePackFixture, "get_the_tail")
{
    TypePackId freeTail = freshTypePack();
    TypePackId typePack = newTypePack({types[0]}, freeTail);

    auto it = begin(typePack);
    auto endIt = end(typePack);
    int count = 0;
    while (it != endIt)
        ++count, ++it;
    REQUIRE_EQ(1, count);

    CHECK(it == end(typePack));

    REQUIRE_EQ(it.tail(), freeTail);
}

TEST_CASE_FIXTURE(TypePackFixture, "tail_can_be_nullopt")
{
    TypePackId typePack = newTypePack({types[0], types[0]}, std::nullopt);

    auto it = end(typePack);
    REQUIRE_EQ(std::nullopt, it.tail());
}

TEST_CASE_FIXTURE(TypePackFixture, "tail_is_end_for_free_TypePack")
{
    TypePackId typePack = freshTypePack();

    auto it = begin(typePack);
    auto endIt = end(typePack);
    while (it != endIt)
        ++it;
    REQUIRE_EQ(typePack, it.tail());
}

TEST_CASE_FIXTURE(TypePackFixture, "skip_over_empty_head_typepack_with_tail")
{
    TypePackId tailTP = newTypePack({types[2], types[3]}, std::nullopt);
    TypePackId headTP = newTypePack({}, tailTP);

    int count = 0;
    for (TypeId ty : headTP)
    {
        (void)ty;
        ++count;
    }

    CHECK_EQ(2, count);
}

TEST_CASE_FIXTURE(TypePackFixture, "skip_over_empty_middle_link")
{
    TypePackId tailTP = newTypePack({types[2], types[3]}, std::nullopt);
    TypePackId middleTP = newTypePack({}, tailTP);
    TypePackId headTP = newTypePack({types[0], types[1]}, middleTP);

    int count = 0;
    for (TypeId ty : headTP)
    {
        (void)ty;
        ++count;
    }

    CHECK_EQ(4, count);
}

TEST_CASE_FIXTURE(TypePackFixture, "follows_Bound_TypePacks")
{
    TypePackId tailTP = newTypePack({types[2], types[3]}, std::nullopt);
    TypePackId middleTP = freshTypePack();
    *asMutable(middleTP) = Unifiable::Bound(tailTP);
    TypePackId headTP = newTypePack({}, middleTP);

    int count = 0;
    for (TypeId ty : headTP)
    {
        (void)ty;
        ++count;
    }

    CHECK_EQ(2, count);
}

TEST_CASE_FIXTURE(TypePackFixture, "post_and_pre_increment")
{
    TypePackId typePack = newTypePack({types[0], types[1], types[2], types[3]}, std::nullopt);

    auto it1 = begin(typePack);
    auto it2 = it1++;
    auto it3 = ++it2;

    CHECK_EQ(*it2, *it3);
}

TEST_CASE_FIXTURE(TypePackFixture, "std_distance")
{
    TypePackId typePack = newTypePack({types[0], types[1], types[2], types[3]}, std::nullopt);

    auto b = begin(typePack);
    auto e = end(typePack);
    CHECK_EQ(4, std::distance(b, e));
}

TEST_CASE("content_reassignment")
{
    TypePackVar myError{ErrorTypePack{}, /*presistent*/ true};

    TypeArena arena;

    TypePackId futureError = arena.addTypePack(TypePackVar{FreeTypePack{TypeLevel{}}});
    asMutable(futureError)->reassign(myError);

    CHECK(get<ErrorTypePack>(futureError) != nullptr);
    CHECK(!futureError->persistent);
    CHECK(futureError->owningArena == &arena);
}

TEST_SUITE_END();
