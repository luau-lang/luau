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
    // u->x = 44; // nope
    int v = u->x;
    CHECK(v == 5);

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
    NotNull<int> a{new int(8)};
    NotNull<int> b{new int(10)};

    std::string hello = "hello";
    std::string world = "world";

    map[a] = hello.c_str();
    map[b] = world.c_str();

    CHECK_EQ(2, map.size());
    CHECK_EQ(hello.c_str(), map[a]);
    CHECK_EQ(world.c_str(), map[b]);

    delete a;
    delete b;
}

TEST_SUITE_END();
