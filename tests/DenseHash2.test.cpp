// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/DenseHash2.h"
#include "Luau/NotNull.h"

#include "doctest.h"

#include <memory>
#include <string>

TEST_SUITE_BEGIN("DenseHash2Tests");

TEST_CASE("map_insert_and_find")
{
    Luau::DenseHashMap2<int, int> m;

    m[1] = 10;
    m[2] = 20;
    m[3] = 30;

    REQUIRE(m.size() == 3);

    int* v1 = m.find(1);
    int* v2 = m.find(2);
    int* v3 = m.find(3);
    int* v4 = m.find(4);

    REQUIRE(v1 != nullptr);
    REQUIRE(v2 != nullptr);
    REQUIRE(v3 != nullptr);
    CHECK(v4 == nullptr);

    CHECK(*v1 == 10);
    CHECK(*v2 == 20);
    CHECK(*v3 == 30);
}

TEST_CASE("map_overwrite")
{
    Luau::DenseHashMap2<int, int> m;

    m[1] = 10;
    m[1] = 42;

    REQUIRE(m.size() == 1);
    int* v = m.find(1);
    REQUIRE(v != nullptr);
    CHECK(*v == 42);
}

TEST_CASE("map_contains")
{
    Luau::DenseHashMap2<int, int> m;

    m[5] = 50;

    CHECK(m.contains(5));
    CHECK(!m.contains(6));
}

TEST_CASE("map_try_insert")
{
    Luau::DenseHashMap2<int, int> m;

    auto [ref1, fresh1] = m.try_insert(1, 10);
    CHECK(fresh1);
    CHECK(ref1 == 10);

    auto [ref2, fresh2] = m.try_insert(1, 99);
    CHECK(!fresh2);
    CHECK(ref2 == 10);
}

TEST_CASE("map_try_insert_move")
{
    Luau::DenseHashMap2<int, std::string> m;

    auto [ref1, fresh1] = m.try_insert(1, std::string("hello"));
    CHECK(fresh1);
    CHECK(ref1 == "hello");

    auto [ref2, fresh2] = m.try_insert(1, std::string("world"));
    CHECK(!fresh2);
    CHECK(ref2 == "hello");
}

TEST_CASE("map_clear")
{
    Luau::DenseHashMap2<int, int> m;

    for (int i = 0; i < 10; ++i)
        m[i] = i * 10;

    REQUIRE(m.size() == 10);
    m.clear();
    CHECK(m.size() == 0);
    CHECK(m.empty());
    CHECK(m.find(0) == nullptr);
}

TEST_CASE("map_iteration")
{
    Luau::DenseHashMap2<int, int> m;

    for (int i = 0; i < 5; ++i)
        m[i] = i * 2;

    int sum_keys = 0;
    int sum_values = 0;
    size_t count = 0;
    for (auto [k, v] : m)
    {
        sum_keys += k;
        sum_values += v;
        ++count;
    }

    CHECK(count == 5);
    CHECK(sum_keys == 0 + 1 + 2 + 3 + 4);
    CHECK(sum_values == 0 + 2 + 4 + 6 + 8);
}

TEST_CASE("map_rehash")
{
    Luau::DenseHashMap2<int, int> m;

    for (int i = 0; i < 100; ++i)
        m[i] = i;

    REQUIRE(m.size() == 100);

    for (int i = 0; i < 100; ++i)
    {
        int* v = m.find(i);
        REQUIRE(v != nullptr);
        CHECK(*v == i);
    }
}

TEST_CASE("map_copy")
{
    Luau::DenseHashMap2<int, int> m;

    for (int i = 0; i < 10; ++i)
        m[i] = i * 3;

    Luau::DenseHashMap2<int, int> copy(m);

    REQUIRE(copy.size() == 10);
    for (int i = 0; i < 10; ++i)
    {
        int* v = copy.find(i);
        REQUIRE(v != nullptr);
        CHECK(*v == i * 3);
    }

    // Mutating the copy doesn't affect the original
    copy[0] = 999;
    CHECK(*m.find(0) == 0);
}

TEST_CASE("map_move")
{
    Luau::DenseHashMap2<int, int> m;

    for (int i = 0; i < 10; ++i)
        m[i] = i;

    Luau::DenseHashMap2<int, int> moved(std::move(m));

    REQUIRE(moved.size() == 10);
    for (int i = 0; i < 10; ++i)
    {
        int* v = moved.find(i);
        REQUIRE(v != nullptr);
        CHECK(*v == i);
    }

    CHECK(m.size() == 0);
}

TEST_CASE("map_overwrite_when_full_shouldnt_rehash")
{
    Luau::DenseHashMap2<int, int> m;
    for (int i = 0; i < 12; ++i)
        m[i] = i;

    REQUIRE(m.size() == 12);

    for (auto [k, a] : m)
        m[k] = a + 1;

    for (size_t i = 0; i < m.size(); ++i)
    {
        int* a = m.find(int(i));
        REQUIRE(a);
        CHECK(i + 1 == *a);
    }
}

TEST_CASE("map_merge_with_rehash_while_iterating")
{
    Luau::DenseHashMap2<int, int> m1;
    for (int i = 0; i < 12; ++i)
        m1[i] = i;

    Luau::DenseHashMap2<int, int> m2;
    for (int i = 8; i < 24; ++i)
        m2[i] = i;

    REQUIRE(m1.size() == 12);
    REQUIRE(m2.size() == 16);

    for (auto [k, a] : m1)
    {
        if (int* b = m2.find(k))
            m1[k] = a + *b;
    }

    for (auto [k, a] : m2)
    {
        if (!m1.find(k))
            m1[k] = a;
    }

    REQUIRE(m1.size() == 24);
    for (size_t i = 0; i < m1.size(); ++i)
    {
        int* a = m1.find(int(i));
        REQUIRE(a);
        if (i < 8 || i >= 12)
            CHECK(i == *a);
        else
            CHECK(i + i == *a);
    }
}

TEST_CASE("map_string_keys")
{
    Luau::DenseHashMap2<std::string, int> m;

    m["hello"] = 1;
    m["world"] = 2;
    m["test"] = 3;

    CHECK(m.size() == 3);
    CHECK(*m.find("hello") == 1);
    CHECK(*m.find("world") == 2);
    CHECK(*m.find("test") == 3);
    CHECK(m.find("missing") == nullptr);
}

TEST_CASE("set_insert_and_find")
{
    Luau::DenseHashSet2<int> s;

    s.insert(1);
    s.insert(2);
    s.insert(3);

    REQUIRE(s.size() == 3);
    CHECK(s.contains(1));
    CHECK(s.contains(2));
    CHECK(s.contains(3));
    CHECK(!s.contains(4));
}

TEST_CASE("set_duplicate_insert")
{
    Luau::DenseHashSet2<int> s;

    s.insert(1);
    s.insert(1);
    s.insert(1);

    CHECK(s.size() == 1);
    CHECK(s.contains(1));
}

TEST_CASE("set_clear")
{
    Luau::DenseHashSet2<int> s;

    for (int i = 0; i < 10; ++i)
        s.insert(i);

    REQUIRE(s.size() == 10);
    s.clear();
    CHECK(s.size() == 0);
    CHECK(s.empty());
    CHECK(!s.contains(0));
}

TEST_CASE("set_iteration")
{
    Luau::DenseHashSet2<int> s;

    for (int i = 0; i < 5; ++i)
        s.insert(i);

    int sum = 0;
    size_t count = 0;
    for (const int& k : s)
    {
        sum += k;
        ++count;
    }

    CHECK(count == 5);
    CHECK(sum == 0 + 1 + 2 + 3 + 4);
}

TEST_CASE("set_rehash")
{
    Luau::DenseHashSet2<int> s;

    for (int i = 0; i < 100; ++i)
        s.insert(i);

    REQUIRE(s.size() == 100);

    for (int i = 0; i < 100; ++i)
        CHECK(s.contains(i));

    CHECK(!s.contains(100));
}

TEST_CASE("set_copy")
{
    Luau::DenseHashSet2<int> s;

    for (int i = 0; i < 10; ++i)
        s.insert(i);

    Luau::DenseHashSet2<int> copy(s);

    REQUIRE(copy.size() == 10);
    for (int i = 0; i < 10; ++i)
        CHECK(copy.contains(i));
}

TEST_CASE("set_move")
{
    Luau::DenseHashSet2<int> s;

    for (int i = 0; i < 10; ++i)
        s.insert(i);

    Luau::DenseHashSet2<int> moved(std::move(s));

    REQUIRE(moved.size() == 10);
    for (int i = 0; i < 10; ++i)
        CHECK(moved.contains(i));

    CHECK(s.size() == 0);
}

TEST_CASE("set_equality")
{
    Luau::DenseHashSet2<int> a;
    Luau::DenseHashSet2<int> b;

    for (int i = 0; i < 5; ++i)
    {
        a.insert(i);
        b.insert(4 - i);
    }

    CHECK(a == b);

    b.insert(99);
    CHECK(a != b);
}

TEST_CASE("set_pointer_keys")
{
    struct Test
    {
        explicit Test(int a)
            : a(a)
        {
        }
        int a;
    };

    std::shared_ptr<Test> ta = std::make_shared<Test>(1);
    std::shared_ptr<Test> tb = std::make_shared<Test>(2);

    Luau::DenseHashSet2<Test*> s;
    s.insert(ta.get());
    s.insert(tb.get());

    CHECK(s.size() == 2);
    CHECK(s.contains(ta.get()));
    CHECK(s.contains(tb.get()));
    CHECK(!s.contains(nullptr));
}

TEST_CASE("map_pointer_keys")
{
    struct Test
    {
        explicit Test(int a)
            : a(a)
        {
        }
        int a;
    };

    std::shared_ptr<Test> ta = std::make_shared<Test>(1);
    std::shared_ptr<Test> tb = std::make_shared<Test>(2);

    Luau::DenseHashMap2<Test*, int> m;
    m[ta.get()] = 1;
    m[tb.get()] = 2;

    CHECK(m.size() == 2);
    CHECK(*m.find(ta.get()) == 1);
    CHECK(*m.find(tb.get()) == 2);
    CHECK(m.find(nullptr) == nullptr);
}

TEST_CASE("map_erase")
{
    Luau::DenseHashMap2<int, int> m;

    for (int i = 0; i < 10; ++i)
        m[i] = i * 10;

    REQUIRE(m.size() == 10);

    m.erase(5);

    CHECK(m.size() == 9);
    CHECK(m.find(5) == nullptr);

    // Other elements are still accessible
    for (int i = 0; i < 10; ++i)
    {
        if (i == 5)
            continue;
        int* v = m.find(i);
        REQUIRE(v != nullptr);
        CHECK(*v == i * 10);
    }
}

TEST_CASE("map_erase_nonexistent")
{
    Luau::DenseHashMap2<int, int> m;

    m[1] = 10;
    m[2] = 20;

    m.erase(99);

    CHECK(m.size() == 2);
    CHECK(*m.find(1) == 10);
    CHECK(*m.find(2) == 20);
}

TEST_CASE("map_erase_and_reinsert")
{
    Luau::DenseHashMap2<int, int> m;

    m[1] = 10;
    m[2] = 20;
    m[3] = 30;

    m.erase(2);
    CHECK(m.find(2) == nullptr);

    m[2] = 99;
    CHECK(*m.find(2) == 99);
    CHECK(m.size() == 3);
}

TEST_CASE("map_erase_chain")
{
    // Insert elements that will form a probe chain, then erase from the middle
    Luau::DenseHashMap2<int, int> m;

    for (int i = 0; i < 50; ++i)
        m[i] = i;

    // Erase every other element
    for (int i = 0; i < 50; i += 2)
        m.erase(i);

    CHECK(m.size() == 25);

    // Odd elements should still be findable
    for (int i = 1; i < 50; i += 2)
    {
        int* v = m.find(i);
        REQUIRE(v != nullptr);
        CHECK(*v == i);
    }

    // Even elements should be gone
    for (int i = 0; i < 50; i += 2)
        CHECK(m.find(i) == nullptr);
}

TEST_CASE("set_erase_destroys_element_destructor")
{
    // A single element in the set will be at the end of its probe chain,
    // so erase won't backward-shift anything — it just marks the bit as empty.
    // If the erased element's destructor isn't called, the shared_ptr refcount stays elevated.
    auto p = std::make_shared<int>(42);
    CHECK(p.use_count() == 1);

    {
        Luau::DenseHashSet2<std::shared_ptr<int>> s;
        s.insert(p);
        CHECK(p.use_count() == 2);

        s.erase(p);
        CHECK(p.use_count() == 1);
    }
}

TEST_CASE("map_erase_destroys_value_destructor")
{
    auto p = std::make_shared<int>(42);
    CHECK(p.use_count() == 1);

    {
        Luau::DenseHashMap2<int, std::shared_ptr<int>> m;
        m[1] = p;
        CHECK(p.use_count() == 2);

        m.erase(1);
        CHECK(p.use_count() == 1);
    }
}

TEST_CASE("map_usable_with_Luau_notnull")
{
    Luau::DenseHashMap2<Luau::NotNull<int>, int> map;
    int x = 1;
    Luau::NotNull<int> i{&x};
    map[i] = x;
    CHECK(map.contains(i));
}

TEST_SUITE_END();
