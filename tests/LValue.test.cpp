// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"

#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

static void merge(TypeArena& arena, RefinementMap& l, const RefinementMap& r)
{
    Luau::merge(l, r, [&arena](TypeId a, TypeId b) -> TypeId {
        // TODO: normalize here also.
        std::unordered_set<TypeId> s;

        if (auto utv = get<UnionTypeVar>(follow(a)))
            s.insert(begin(utv), end(utv));
        else
            s.insert(a);

        if (auto utv = get<UnionTypeVar>(follow(b)))
            s.insert(begin(utv), end(utv));
        else
            s.insert(b);

        std::vector<TypeId> options(s.begin(), s.end());
        return options.size() == 1 ? options[0] : arena.addType(UnionTypeVar{std::move(options)});
    });
}

static LValue mkSymbol(const std::string& s)
{
    return Symbol{AstName{s.data()}};
}

TEST_SUITE_BEGIN("LValue");

TEST_CASE("Luau_merge_hashmap_order")
{
    std::string a = "a";
    std::string b = "b";
    std::string c = "c";

    RefinementMap m{{
        {mkSymbol(b), getSingletonTypes().stringType},
        {mkSymbol(c), getSingletonTypes().numberType},
    }};

    RefinementMap other{{
        {mkSymbol(a), getSingletonTypes().stringType},
        {mkSymbol(b), getSingletonTypes().stringType},
        {mkSymbol(c), getSingletonTypes().booleanType},
    }};

    TypeArena arena;
    merge(arena, m, other);

    REQUIRE_EQ(3, m.size());
    REQUIRE(m.count(mkSymbol(a)));
    REQUIRE(m.count(mkSymbol(b)));
    REQUIRE(m.count(mkSymbol(c)));

    CHECK_EQ("string", toString(m[mkSymbol(a)]));
    CHECK_EQ("string", toString(m[mkSymbol(b)]));
    CHECK_EQ("boolean | number", toString(m[mkSymbol(c)]));
}

TEST_CASE("Luau_merge_hashmap_order2")
{
    std::string a = "a";
    std::string b = "b";
    std::string c = "c";

    RefinementMap m{{
        {mkSymbol(a), getSingletonTypes().stringType},
        {mkSymbol(b), getSingletonTypes().stringType},
        {mkSymbol(c), getSingletonTypes().numberType},
    }};

    RefinementMap other{{
        {mkSymbol(b), getSingletonTypes().stringType},
        {mkSymbol(c), getSingletonTypes().booleanType},
    }};

    TypeArena arena;
    merge(arena, m, other);

    REQUIRE_EQ(3, m.size());
    REQUIRE(m.count(mkSymbol(a)));
    REQUIRE(m.count(mkSymbol(b)));
    REQUIRE(m.count(mkSymbol(c)));

    CHECK_EQ("string", toString(m[mkSymbol(a)]));
    CHECK_EQ("string", toString(m[mkSymbol(b)]));
    CHECK_EQ("boolean | number", toString(m[mkSymbol(c)]));
}

TEST_CASE("one_map_has_overlap_at_end_whereas_other_has_it_in_start")
{
    std::string a = "a";
    std::string b = "b";
    std::string c = "c";
    std::string d = "d";
    std::string e = "e";

    RefinementMap m{{
        {mkSymbol(a), getSingletonTypes().stringType},
        {mkSymbol(b), getSingletonTypes().numberType},
        {mkSymbol(c), getSingletonTypes().booleanType},
    }};

    RefinementMap other{{
        {mkSymbol(c), getSingletonTypes().stringType},
        {mkSymbol(d), getSingletonTypes().numberType},
        {mkSymbol(e), getSingletonTypes().booleanType},
    }};

    TypeArena arena;
    merge(arena, m, other);

    REQUIRE_EQ(5, m.size());
    REQUIRE(m.count(mkSymbol(a)));
    REQUIRE(m.count(mkSymbol(b)));
    REQUIRE(m.count(mkSymbol(c)));
    REQUIRE(m.count(mkSymbol(d)));
    REQUIRE(m.count(mkSymbol(e)));

    CHECK_EQ("string", toString(m[mkSymbol(a)]));
    CHECK_EQ("number", toString(m[mkSymbol(b)]));
    CHECK_EQ("boolean | string", toString(m[mkSymbol(c)]));
    CHECK_EQ("number", toString(m[mkSymbol(d)]));
    CHECK_EQ("boolean", toString(m[mkSymbol(e)]));
}

TEST_CASE("hashing_lvalue_global_prop_access")
{
    std::string t1 = "t";
    std::string x1 = "x";

    LValue t_x1{Field{std::make_shared<LValue>(Symbol{AstName{t1.data()}}), x1}};

    std::string t2 = "t";
    std::string x2 = "x";

    LValue t_x2{Field{std::make_shared<LValue>(Symbol{AstName{t2.data()}}), x2}};

    CHECK_EQ(t_x1, t_x1);
    CHECK_EQ(t_x1, t_x2);
    CHECK_EQ(t_x2, t_x2);

    CHECK_EQ(LValueHasher{}(t_x1), LValueHasher{}(t_x1));
    CHECK_EQ(LValueHasher{}(t_x1), LValueHasher{}(t_x2));
    CHECK_EQ(LValueHasher{}(t_x2), LValueHasher{}(t_x2));

    RefinementMap m;
    m[t_x1] = getSingletonTypes().stringType;
    m[t_x2] = getSingletonTypes().numberType;

    CHECK_EQ(1, m.size());
}

TEST_CASE("hashing_lvalue_local_prop_access")
{
    std::string t1 = "t";
    std::string x1 = "x";

    AstLocal localt1{AstName{t1.data()}, Location(), nullptr, 0, 0, nullptr};
    LValue t_x1{Field{std::make_shared<LValue>(Symbol{&localt1}), x1}};

    std::string t2 = "t";
    std::string x2 = "x";

    AstLocal localt2{AstName{t2.data()}, Location(), &localt1, 0, 0, nullptr};
    LValue t_x2{Field{std::make_shared<LValue>(Symbol{&localt2}), x2}};

    CHECK_EQ(t_x1, t_x1);
    CHECK_NE(t_x1, t_x2);
    CHECK_EQ(t_x2, t_x2);

    CHECK_EQ(LValueHasher{}(t_x1), LValueHasher{}(t_x1));
    CHECK_NE(LValueHasher{}(t_x1), LValueHasher{}(t_x2));
    CHECK_EQ(LValueHasher{}(t_x2), LValueHasher{}(t_x2));

    RefinementMap m;
    m[t_x1] = getSingletonTypes().stringType;
    m[t_x2] = getSingletonTypes().numberType;

    CHECK_EQ(2, m.size());
}

TEST_SUITE_END();
