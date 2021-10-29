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

TEST_SUITE_BEGIN("Predicate");

TEST_CASE_FIXTURE(Fixture, "Luau_merge_hashmap_order")
{
    ScopedFastFlag sff{"LuauOrPredicate", true};

    RefinementMap m{
        {"b", typeChecker.stringType},
        {"c", typeChecker.numberType},
    };

    RefinementMap other{
        {"a", typeChecker.stringType},
        {"b", typeChecker.stringType},
        {"c", typeChecker.booleanType},
    };

    TypeArena arena;
    merge(arena, m, other);

    REQUIRE_EQ(3, m.size());
    REQUIRE(m.count("a"));
    REQUIRE(m.count("b"));
    REQUIRE(m.count("c"));

    CHECK_EQ("string", toString(m["a"]));
    CHECK_EQ("string", toString(m["b"]));
    CHECK_EQ("boolean | number", toString(m["c"]));
}

TEST_CASE_FIXTURE(Fixture, "Luau_merge_hashmap_order2")
{
    ScopedFastFlag sff{"LuauOrPredicate", true};

    RefinementMap m{
        {"a", typeChecker.stringType},
        {"b", typeChecker.stringType},
        {"c", typeChecker.numberType},
    };

    RefinementMap other{
        {"b", typeChecker.stringType},
        {"c", typeChecker.booleanType},
    };

    TypeArena arena;
    merge(arena, m, other);

    REQUIRE_EQ(3, m.size());
    REQUIRE(m.count("a"));
    REQUIRE(m.count("b"));
    REQUIRE(m.count("c"));

    CHECK_EQ("string", toString(m["a"]));
    CHECK_EQ("string", toString(m["b"]));
    CHECK_EQ("boolean | number", toString(m["c"]));
}

TEST_CASE_FIXTURE(Fixture, "one_map_has_overlap_at_end_whereas_other_has_it_in_start")
{
    ScopedFastFlag sff{"LuauOrPredicate", true};

    RefinementMap m{
        {"a", typeChecker.stringType},
        {"b", typeChecker.numberType},
        {"c", typeChecker.booleanType},
    };

    RefinementMap other{
        {"c", typeChecker.stringType},
        {"d", typeChecker.numberType},
        {"e", typeChecker.booleanType},
    };

    TypeArena arena;
    merge(arena, m, other);

    REQUIRE_EQ(5, m.size());
    REQUIRE(m.count("a"));
    REQUIRE(m.count("b"));
    REQUIRE(m.count("c"));
    REQUIRE(m.count("d"));
    REQUIRE(m.count("e"));

    CHECK_EQ("string", toString(m["a"]));
    CHECK_EQ("number", toString(m["b"]));
    CHECK_EQ("boolean | string", toString(m["c"]));
    CHECK_EQ("number", toString(m["d"]));
    CHECK_EQ("boolean", toString(m["e"]));
}

TEST_SUITE_END();
