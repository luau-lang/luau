// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"

#include "TableFormatExamples.h"

#include "doctest.h"

#include <cmath>
#include <cstring>
#include <string>

using namespace Luau;
using namespace Luau::TableFormat;

static Table readOk(const std::string& source)
{
    ReadResult result = read(source);
    INFO(source);
    REQUIRE_MESSAGE(result.errors.empty(), result.errors[0].getMessage());
    REQUIRE(result.table);
    return std::move(*result.table);
}

static void checkMalformed(const std::string& source)
{
    ReadResult result = read(source);
    INFO(source);
    CHECK(!result.errors.empty());
    CHECK(!result.table);
}

template<typename T>
static const T& get(const Table& table, const Key& key)
{
    const Value* value = table.find(key);
    REQUIRE(value);
    REQUIRE(value->get_if<T>());
    return *value->get_if<T>();
}

TEST_SUITE_BEGIN("TableFormat");

TEST_CASE("read_rfc_example")
{
    Table table = readOk(kTableFormatRfcExample);

    CHECK(table.getArray().empty());
    CHECK(table.getMap().size() == 11);
    CHECK(get<std::string>(table, "name") == "example");
    CHECK(get<bool>(table, "enabled") == true);
    CHECK(get<double>(table, "retries") == 3);
    CHECK(get<double>(table, "timeout") == 2500);
    CHECK(get<double>(table, "flags") == 0xa1);
    CHECK(get<double>(table, "threshold") == -HUGE_VAL);
    CHECK(get<std::string>(table, 1.0) == "first numeric key");
    CHECK(get<std::string>(table, "display name") == "Example Service");

    const Vector& origin = get<Vector>(table, "origin");
    CHECK(origin.x == 0.0f);
    CHECK(origin.y == 10.0f);
    CHECK(origin.z == -5.0f);

    const Table& tags = get<Table>(table, "tags");
    REQUIRE(tags.getArray().size() == 2);
    CHECK(get<std::string>(tags, 2.0) == "beta");

    const Table& limits = get<Table>(table, "limits");
    CHECK(get<double>(limits, true) == HUGE_VAL);
    CHECK(get<double>(limits, false) == 0);

    // The map part keeps the order the keys were written in.
    CHECK(*table.getMap()[0].key.get_if<std::string>() == "name");
    CHECK(*table.getMap()[10].key.get_if<std::string>() == "limits");
}

TEST_CASE("read_literals")
{
    Table table = readOk(R"({
        'single', "double", [[raw]], [==[raw ]] level]==], `backtick`,
        "\n\t\\\"\x41\u{48}\65\z
             skipped",
        1_000, 0xf_f, 0B11, 1e-3, .5, 5., 0.1, -0, 1e999, 0xffffffffffffffffff,
    })");

    REQUIRE(table.getArray().size() == 16);
    CHECK(get<std::string>(table, 1.0) == "single");
    CHECK(get<std::string>(table, 3.0) == "raw");
    CHECK(get<std::string>(table, 4.0) == "raw ]] level");
    CHECK(get<std::string>(table, 5.0) == "backtick");
    CHECK(get<std::string>(table, 6.0) == "\n\t\\\"AHAskipped");
    CHECK(get<double>(table, 7.0) == 1000);
    CHECK(get<double>(table, 8.0) == 255);
    CHECK(get<double>(table, 9.0) == 3);
    CHECK(get<double>(table, 10.0) == 0.001);
    CHECK(get<double>(table, 11.0) == 0.5);
    CHECK(get<double>(table, 12.0) == 5);
    CHECK(get<double>(table, 13.0) == 0.1);
    CHECK(std::signbit(get<double>(table, 14.0)));
    CHECK(get<double>(table, 15.0) == HUGE_VAL);
}

TEST_CASE("read_blessed_names")
{
    Table table = readOk("{ math.nan, math.huge, -math.huge, vector.create(1, 2), vector.create(-1, math.huge, math.nan) }");

    CHECK(std::isnan(get<double>(table, 1.0)));
    CHECK(get<double>(table, 2.0) == HUGE_VAL);
    CHECK(get<double>(table, 3.0) == -HUGE_VAL);

    CHECK(get<Vector>(table, 4.0).z == 0.0f);

    const Vector& three = get<Vector>(table, 5.0);
    CHECK(three.x == -1.0f);
    CHECK(three.y == HUGE_VALF);
    CHECK(std::isnan(three.z));
}

TEST_CASE("read_syntax_variations")
{
    readOk("{}");
    readOk("return {}");
    readOk("--[[ before ]] return --[==[ between ]==] {} -- after");
    readOk("{ 1; 2, a = 3; }");
    readOk("{ [2] = 2, 1 }");
    readOk("{};");
    readOk("return {};");
    readOk("return {} -- a\n; -- b");
    readOk("{ a = {} }\n;\n");
    readOk("{ [vector.create(1, 2, 3)] = 1, [vector.create(1, 2, 4)] = 2 }");
}

TEST_CASE("read_rejects_malformed")
{
    // Evaluation of any kind.
    checkMalformed("{ a = 1 + 2 }");
    checkMalformed("{ a = x }");
    checkMalformed("{ a = not true }");
    checkMalformed("{ a = (1) }");
    checkMalformed("{ a = - -1 }");
    checkMalformed("{ a = #\"a\" }");
    checkMalformed("{ a = function() end }");
    checkMalformed("{ a = `{1}` }");
    checkMalformed("{ a = math.pi }");
    checkMalformed("{ a = string.nan }");
    checkMalformed("{ a = math[\"huge\"] }");
    checkMalformed("{ a = -math.nan }");
    checkMalformed("{ a = -vector.create(1, 2) }");
    checkMalformed("{ a = vector.create(1) }");
    checkMalformed("{ a = vector.create(1, 2, 3, 4) }");
    checkMalformed("{ a = vector.create(\"1\", 2) }");
    checkMalformed("{ a = vector.create{1, 2} }");
    checkMalformed("{ a = vector.create(vector.create(1, 2), 2) }");
    checkMalformed("{ a = if true then 1 else 2 }");

    // Nil.
    checkMalformed("{ a = nil }");
    checkMalformed("{ 1, nil, 3 }");

    // Type syntax.
    checkMalformed("{ a = 1 :: number }");

    // Invalid keys.
    checkMalformed("{ [{}] = 1 }");
    checkMalformed("{ [math.nan] = 1 }");
    checkMalformed("{ [vector.create(math.nan, 0, 0)] = 1 }");

    // Duplicate keys.
    checkMalformed("{ a = 1, a = 2 }");
    checkMalformed("{ a = 1, [\"a\"] = 2 }");
    checkMalformed("{ [1] = 1, [1.0] = 2 }");
    checkMalformed("{ [0] = 1, [-0] = 2 }");
    checkMalformed("{ \"a\", \"b\", [2] = \"c\" }");
    checkMalformed("{ [2] = \"c\", \"a\", \"b\" }");
    checkMalformed("{ [vector.create(0, 1)] = 1, [vector.create(-0, 1, 0)] = 2 }");
    checkMalformed("{ [math.huge] = 1, [1e999] = 2 }");

    // Anything other than a single table, optionally returned.
    checkMalformed("");
    checkMalformed("return");
    checkMalformed("1");
    checkMalformed("\"a\"");
    checkMalformed("{} {}");
    checkMalformed("{} 5");
    checkMalformed(";");
    checkMalformed("; {}");
    checkMalformed("{};;");
    checkMalformed("{}; ;");
    checkMalformed("return {};;");
    checkMalformed("return; {}");
    checkMalformed("return {}, {}");
    checkMalformed("return ({})");
    checkMalformed("local a = 1 return {}");
    checkMalformed("return {} return {}");
    checkMalformed("{ a = 1");

    // The lexer stops at a NUL byte, so whatever follows must not be ignored.
    checkMalformed(std::string("{}\0garbage", 10));
    checkMalformed(std::string("{}--\0garbage", 12));
    checkMalformed(std::string("{ a = \"\0\" }", 12));
}

TEST_CASE("read_allows_unknown_escapes")
{
    // Luau reads an unknown escape as the character after the backslash.
    CHECK(get<std::string>(readOk(R"({ "\q\w\X41", '\%', `\!` })"), 1.0) == "qwX41");
}

TEST_CASE("read_allows_escaped_nul")
{
    CHECK(get<std::string>(readOk("{ \"a\\0b\" }"), 1.0) == std::string("a\0b", 3));
}

static Table nestedTable(size_t depth)
{
    Table table;
    bool appended = true;
    for (size_t i = 1; i < depth; i++)
    {
        Table outer;
        appended &= outer.append(std::move(table));
        table = std::move(outer);
    }
    REQUIRE(appended);
    return table;
}

TEST_CASE("depth_limit")
{
    readOk(tableFormatNested(kMaxDepth));
    checkMalformed(tableFormatNested(kMaxDepth + 1));
    checkMalformed(tableFormatNested(100000));

    ReadResult tooDeep = read(tableFormatNested(kMaxDepth + 1));
    REQUIRE(tooDeep.errors.size() == 1);
    CHECK(tooDeep.errors[0].getMessage() == "Exceeded the maximum table nesting depth of 255");

    WriteResult deepest = write(nestedTable(kMaxDepth));
    REQUIRE(deepest.source);
    CHECK(deepest.error.empty());
    readOk(*deepest.source);

    WriteResult deeper = write(nestedTable(kMaxDepth + 1));
    CHECK(!deeper.source);
    CHECK(deeper.error == tooDeep.errors[0].getMessage());
}

TEST_CASE("read_reports_locations")
{
    ReadResult result = read("{\n    a = 1,\n    b = x,\n}");
    REQUIRE(result.errors.size() == 1);
    CHECK(result.errors[0].getLocation().begin == Position{2, 8});
}

TEST_CASE("read_reports_duplicate_keys")
{
    ReadResult result = read("{ \"a\", [1] = 1, b = 1, [\"b\"] = 2, [vector.create(1, 2)] = 1, [vector.create(1, 2, 0)] = 2 }");
    REQUIRE(result.errors.size() == 3);
    CHECK(result.errors[0].getMessage() == "Duplicate key 1");
    CHECK(result.errors[1].getMessage() == "Duplicate key \"b\"");
    CHECK(result.errors[2].getMessage() == "Duplicate key vector.create(1, 2, 0)");

    ReadResult implicit = read("{ [2] = 1, \"a\", \"b\" }");
    REQUIRE(implicit.errors.size() == 1);
    CHECK(implicit.errors[0].getMessage() == "Duplicate key 2");
}

TEST_CASE("read_limits_errors")
{
    std::string source = "{";
    for (int i = 0; i < 1000; i++)
        source += " x,";
    source += " }";

    ReadResult result = read(source);
    CHECK(result.errors.size() == 101);
    CHECK(result.errors.back().getMessage() == "Reached error limit (100)");
}

TEST_CASE("table_keys")
{
    Table table;

    CHECK(table.append("a"));
    CHECK(table.insert(3.0, "c"));
    CHECK(table.append("b"));
    CHECK(!table.append("x")); // 3 is already in the map part

    CHECK(!table.insert(1.0, true));
    CHECK(!table.insert(3.0, true));
    CHECK(!table.insert(std::nan(""), true));
    CHECK(!table.insert(Vector{0.0f, std::nanf(""), 0.0f}, true));

    CHECK(table.insert(0.0, true));
    CHECK(!table.insert(-0.0, true));
    CHECK(table.insert(Vector{-0.0f, 1.0f, 0.0f}, true));
    CHECK(table.find(Vector{0.0f, 1.0f, -0.0f}));
    CHECK(table.insert("1", true));
    CHECK(table.find("1"));
    CHECK(table.insert(false, true));

    CHECK(get<std::string>(table, 2.0) == "b");
    CHECK(get<std::string>(table, 3.0) == "c");
    CHECK(!table.find(4.0));
    CHECK(!table.find(1.5));
    CHECK(!table.find(std::nan("")));
}

TEST_CASE("table_erase")
{
    Table table = readOk("{ 1, 2, a = 1, b = 2, c = 3 }");

    CHECK(!table.erase(1.0)); // would leave a hole
    CHECK(table.erase(2.0));
    CHECK(table.erase(1.0));
    CHECK(table.getArray().empty());

    CHECK(table.erase("b"));
    CHECK(!table.erase("b"));
    CHECK(get<double>(table, "a") == 1);
    CHECK(get<double>(table, "c") == 3);
    CHECK(table.getMap().size() == 2);

    // Erasing the last map entry keeps the rest findable.
    CHECK(table.erase("c"));
    CHECK(get<double>(table, "a") == 1);
    CHECK(table.insert("d", 4.0));
    CHECK(get<double>(table, "d") == 4);

    // The array part can grow back into keys that were erased.
    CHECK(table.append(true));
}

TEST_CASE("table_edit_in_place")
{
    Table table = readOk("{ nested = { count = 1 } }");

    Table* nested = table.find("nested")->get_if<Table>();
    REQUIRE(nested);
    *nested->find("count") = 2.0;

    CHECK(write(table).source == "{\n    nested = {\n        count = 2,\n    },\n}\n");
}

TEST_CASE("table_copy_and_move")
{
    Table table = readOk("{ a = { b = { 1, 2 } }, c = 3 }");

    Table copy = table;
    *copy.find("a")->get_if<Table>()->find("b")->get_if<Table>()->find(1.0) = 10.0;
    CHECK(get<double>(get<Table>(get<Table>(table, "a"), "b"), 1.0) == 1);
    CHECK(get<double>(get<Table>(get<Table>(copy, "a"), "b"), 1.0) == 10);
    CHECK(get<double>(copy, "c") == 3);

    // A table can be replaced by one of its own nested tables.
    table = std::move(*table.find("a")->get_if<Table>());
    CHECK(table.getMap().size() == 1);
    REQUIRE(table.find("b"));

    copy = get<Table>(copy, "a");
    CHECK(get<double>(get<Table>(copy, "b"), 1.0) == 10);
}

TEST_CASE("table_deep_nesting_doesnt_overflow_stack")
{
    // Far deeper than can be read or written, to check that copying and destroying don't recurse.
    Table table = nestedTable(1000000);
    Table copy = table;
    CHECK(copy.getArray().size() == 1);
    CHECK(!write(copy).source);
}

TEST_CASE("write_format")
{
    Table table = readOk(R"({
        "a", true, {},
        name = "x", ["not a name"] = 1, ["end"] = 2, ["continue"] = 3, ["1a"] = 4, _1 = 5,
        [2.5] = -0, [-1] = math.huge, [false] = math.nan, [vector.create(1, 0.5, -2)] = { 1 },
    })");

    CHECK(write(table).source == R"({
    "a",
    true,
    {},
    name = "x",
    ["not a name"] = 1,
    ["end"] = 2,
    continue = 3,
    ["1a"] = 4,
    _1 = 5,
    [2.5] = -0,
    [-1] = math.huge,
    [false] = math.nan,
    [vector.create(1, 0.5, -2)] = {
        1,
    },
}
)");

    readOk(*write(table).source);
    CHECK(write(Table{}).source == "{}\n");
}

TEST_CASE("write_names_are_ascii")
{
    Table table;
    CHECK(table.insert("caf\xc3\xa9", 1.0));
    CHECK(table.insert("_x9", 2.0));

    CHECK(write(table).source == "{\n    [\"caf\xc3\xa9\"] = 1,\n    _x9 = 2,\n}\n");
}

TEST_CASE("write_whole_numbers")
{
    Table table = readOk("{ 10, 100, 1e6, -1e14, 99999999999999.5, 1e15, vector.create(10, 20, 300) }");

    CHECK(write(table).source == R"({
    10,
    100,
    1000000,
    -100000000000000,
    99999999999999.5,
    1000000000000000,
    vector.create(10, 20, 300),
}
)");
}

TEST_CASE("write_vector_components_read_back_through_doubles")
{
    // The shortest float representation of this component, 7.038531e-26, reads back as a different float when it's read
    // as a double first, as Luau does, so it's written with one more digit.
    uint32_t bits = 0x15ae43fd;
    float component;
    memcpy(&component, &bits, sizeof(component));

    Table table;
    CHECK(table.append(Vector{component, 0.1f, 0.0f}));

    std::string source = *write(table).source;
    INFO(source);
    CHECK(source.find("vector.create(7.0385307e-26, 0.1, 0)") != std::string::npos);

    Table copy = readOk(source);
    const Vector& read = get<Vector>(copy, 1.0);
    CHECK(memcmp(&read.x, &component, sizeof(component)) == 0);
}

TEST_CASE("write_numbers_and_strings_round_trip")
{
    Table table;
    for (double n : kTableFormatRoundTripNumbers)
        CHECK(table.append(n));
    for (float f : kTableFormatRoundTripFloats)
        CHECK(table.append(Vector{f, -f, 0.0f}));
    for (const std::string& s : kTableFormatRoundTripStrings)
        CHECK(table.insert(s, s));

    std::string source = *write(table).source;
    INFO(source);
    Table copy = readOk(source);

    CHECK(write(copy).source == source);

    double index = 1;
    for (double n : kTableFormatRoundTripNumbers)
        CHECK(get<double>(copy, index++) == n);
    for (float f : kTableFormatRoundTripFloats)
        CHECK(get<Vector>(copy, index++).x == f);
    for (const std::string& s : kTableFormatRoundTripStrings)
        CHECK(get<std::string>(copy, s) == s);
}

TEST_SUITE_END();
