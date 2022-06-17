#include "Luau/NotNull.h"

#include "doctest.h"

#include <stdio.h>
#include <unordered_map>
#include <string>

using Luau::NotNull;

namespace
{

struct Test
{
    int x;
    float y;

    static int count;
    Test()
    {
        ++count;
    }

    ~Test()
    {
        --count;
    }
};

int Test::count = 0;

}

int foo(NotNull<int> p)
{
    return *p;
}

void bar(int* q)
{}

TEST_SUITE_BEGIN("NotNull");

TEST_CASE("basic_stuff")
{
    NotNull<int> a = NotNull{new int(55)};    // Does runtime test
    NotNull<int> b{new int(55)};             // As above
    // NotNull<int> c = new int(55);         // Nope. Mildly regrettable, but implicit conversion from T* to NotNull<T> in the general case is not good.

    // a = nullptr; // nope

    NotNull<int> d = a; // No runtime test. a is known not to be null.

    int e = *d;
    *d = 1;
    CHECK(e == 55);

    const NotNull<int> f = d;
    *f = 5; // valid: there is a difference between const NotNull<T> and NotNull<const T>
    // f = a; // nope

    CHECK_EQ(a, d);
    CHECK(a != b);

    NotNull<const int> g(a);
    CHECK(g == a);

    // *g = 123; // nope

    (void)f;

    NotNull<Test> t{new Test};
    t->x = 5;
    t->y = 3.14f;

    const NotNull<Test> u = t;
    u->x = 44;
    int v = u->x;
    CHECK(v == 44);

    bar(a);

    // a++; // nope
    // a[41]; // nope
    // a + 41; // nope
    // a - 41; // nope

    delete a;
    delete b;
    delete t;

    CHECK_EQ(0, Test::count);
}

TEST_CASE("hashable")
{
    std::unordered_map<NotNull<int>, const char*> map;
    int a_ = 8;
    int b_ = 10;

    NotNull<int> a{&a_};
    NotNull<int> b{&b_};

    std::string hello = "hello";
    std::string world = "world";

    map[a] = hello.c_str();
    map[b] = world.c_str();

    CHECK_EQ(2, map.size());
    CHECK_EQ(hello.c_str(), map[a]);
    CHECK_EQ(world.c_str(), map[b]);
}

TEST_CASE("const")
{
    int p = 0;
    int q = 0;

    NotNull<int> n{&p};

    *n = 123;

    NotNull<const int> m = n; // Conversion from NotNull<T> to NotNull<const T> is allowed

    CHECK(123 == *m); // readonly access of m is ok

    // *m = 321; // nope.  m points at const data.

    // NotNull<int> o = m; // nope.  Conversion from NotNull<const T> to NotNull<T> is forbidden

    NotNull<int> n2{&q};
    m = n2; // ok.  m points to const data, but is not itself const

    const NotNull<int> m2 = n;
    // m2 = n2; // nope.  m2 is const.
    *m2 = 321; // ok.  m2 is const, but points to mutable data

    CHECK(321 == *n);
}

TEST_CASE("const_compatibility")
{
    int* raw = new int(8);

    NotNull<int> a(raw);
    NotNull<const int> b(raw);
    NotNull<const int> c = a;
    // NotNull<int> d = c; // nope - no conversion from const to non-const

    CHECK_EQ(*c, 8);

    delete raw;
}

TEST_SUITE_END();
