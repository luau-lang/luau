// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Variant.h"

#include <string>
#include <ostream>

#include "doctest.h"

using namespace Luau;

struct Foo
{
    int x = 42;
};

struct Bar
{
    explicit Bar(int x)
        : prop(x * 2)
    {
        ++count;
    }

    ~Bar()
    {
        --count;
    }

    int prop;
    static int count;
};

int Bar::count = 0;

TEST_SUITE_BEGIN("Variant");

TEST_CASE("DefaultCtor")
{
    Variant<int, Foo> v1;
    Variant<Foo, int> v2;

    REQUIRE(get_if<int>(&v1));
    CHECK(*get_if<int>(&v1) == 0);
    CHECK(!get_if<Foo>(&v1));

    REQUIRE(get_if<Foo>(&v2));
    CHECK(get_if<Foo>(&v2)->x == 42);
}

TEST_CASE("Create")
{
    Variant<int, Foo> v1 = 1;
    Variant<Foo, int> v2 = Foo{2};

    Foo f = {3};
    Variant<Foo, int> v3 = f;

    REQUIRE(get_if<int>(&v1));
    CHECK(*get_if<int>(&v1) == 1);

    REQUIRE(get_if<Foo>(&v2));
    CHECK(get_if<Foo>(&v2)->x == 2);

    REQUIRE(get_if<Foo>(&v3));
    CHECK(get_if<Foo>(&v3)->x == 3);
}

TEST_CASE("Emplace")
{
    {
        Variant<int, Bar> v1;

        CHECK(0 == Bar::count);
        int& i = v1.emplace<int>(5);
        CHECK(5 == i);

        CHECK(0 == Bar::count);

        CHECK(get_if<int>(&v1) == &i);

        Bar& bar = v1.emplace<Bar>(11);
        CHECK(22 == bar.prop);
        CHECK(1 == Bar::count);

        CHECK(get_if<Bar>(&v1) == &bar);
    }

    CHECK(0 == Bar::count);
}

TEST_CASE("NonPOD")
{
    // initialize (copy)
    std::string s1 = "hello";
    Variant<std::string, int> v1 = s1;

    CHECK(*get_if<std::string>(&v1) == "hello");

    // initialize (move)
    Variant<std::string, int> v2 = std::string("hello");

    CHECK(*get_if<std::string>(&v2) == "hello");

    // move-assign
    v2 = std::string("this is a long string that doesn't fit into the small buffer");

    CHECK(*get_if<std::string>(&v2) == "this is a long string that doesn't fit into the small buffer");

    // copy-assign
    std::string s2("this is another long string, and this time we're copying it");
    v2 = s2;

    CHECK(*get_if<std::string>(&v2) == "this is another long string, and this time we're copying it");

    // copy ctor
    Variant<std::string, int> v3 = v2;

    CHECK(*get_if<std::string>(&v2) == "this is another long string, and this time we're copying it");
    CHECK(*get_if<std::string>(&v3) == "this is another long string, and this time we're copying it");

    // move ctor
    Variant<std::string, int> v4 = std::move(v3);

    CHECK(*get_if<std::string>(&v2) == "this is another long string, and this time we're copying it");
    CHECK(*get_if<std::string>(&v3) == ""); // moved-from variant has an empty string now
    CHECK(*get_if<std::string>(&v4) == "this is another long string, and this time we're copying it");
}

TEST_CASE("Equality")
{
    Variant<int, std::string> v1 = std::string("hi");
    Variant<int, std::string> v2 = std::string("me");
    Variant<int, std::string> v3 = 1;
    Variant<int, std::string> v4 = 0;
    Variant<int, std::string> v5;

    CHECK(v1 == v1);
    CHECK(v1 != v2);
    CHECK(v1 != v3);
    CHECK(v3 != v4);
    CHECK(v4 == v5);
}

struct ToStringVisitor
{
    std::string operator()(const std::string& v)
    {
        return v;
    }

    std::string operator()(int v)
    {
        return std::to_string(v);
    }
};

struct IncrementVisitor
{
    void operator()(std::string& v)
    {
        v += "1";
    }

    void operator()(int& v)
    {
        v += 1;
    }
};

TEST_CASE("Visit")
{
    Variant<std::string, int> v1 = std::string("123");
    Variant<std::string, int> v2 = 45;
    const Variant<std::string, int>& v1c = v1;
    const Variant<std::string, int>& v2c = v2;

    // void-returning visitor, const variants
    std::string r1;
    visit(
        [&](const auto& v) {
            r1 += ToStringVisitor()(v);
        },
        v1c);
    visit(
        [&](const auto& v) {
            r1 += ToStringVisitor()(v);
        },
        v2c);
    CHECK(r1 == "12345");

    // value-returning visitor, const variants
    std::string r2;
    r2 += visit(ToStringVisitor(), v1c);
    r2 += visit(ToStringVisitor(), v2c);
    CHECK(r2 == "12345");

    // void-returning visitor, mutable variant
    visit(IncrementVisitor(), v1);
    visit(IncrementVisitor(), v2);
    CHECK(visit(ToStringVisitor(), v1) == "1231");
    CHECK(visit(ToStringVisitor(), v2) == "46");

    // value-returning visitor, mutable variant
    std::string r3;
    r3 += visit(
        [&](auto& v) {
            IncrementVisitor()(v);
            return ToStringVisitor()(v);
        },
        v1);
    r3 += visit(
        [&](auto& v) {
            IncrementVisitor()(v);
            return ToStringVisitor()(v);
        },
        v2);
    CHECK(r3 == "1231147");
}

TEST_SUITE_END();
