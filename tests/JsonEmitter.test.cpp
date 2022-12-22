// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/JsonEmitter.h"

#include "doctest.h"

using namespace Luau::Json;

TEST_SUITE_BEGIN("JsonEmitter");

TEST_CASE("write_array")
{
    JsonEmitter emitter;
    ArrayEmitter a = emitter.writeArray();
    a.writeValue(123);
    a.writeValue("foo");
    a.finish();

    std::string result = emitter.str();
    CHECK(result == "[123,\"foo\"]");
}

TEST_CASE("write_object")
{
    JsonEmitter emitter;
    ObjectEmitter o = emitter.writeObject();
    o.writePair("foo", "bar");
    o.writePair("bar", "baz");
    o.finish();

    std::string result = emitter.str();
    CHECK(result == "{\"foo\":\"bar\",\"bar\":\"baz\"}");
}

TEST_CASE("write_bool")
{
    JsonEmitter emitter;
    write(emitter, false);
    CHECK(emitter.str() == "false");

    emitter = JsonEmitter{};
    write(emitter, true);
    CHECK(emitter.str() == "true");
}

TEST_CASE("write_null")
{
    JsonEmitter emitter;
    write(emitter, nullptr);
    CHECK(emitter.str() == "null");
}

TEST_CASE("write_string")
{
    JsonEmitter emitter;
    write(emitter, R"(foo,bar,baz,
"this should be escaped")");
    CHECK(emitter.str() == "\"foo,bar,baz,\\n\\\"this should be escaped\\\"\"");
}

TEST_CASE("write_comma")
{
    JsonEmitter emitter;
    emitter.writeComma();
    write(emitter, true);
    emitter.writeComma();
    write(emitter, false);
    CHECK(emitter.str() == "true,false");
}

TEST_CASE("push_and_pop_comma")
{
    JsonEmitter emitter;
    emitter.writeComma();
    write(emitter, true);
    emitter.writeComma();
    emitter.writeRaw('[');
    bool comma = emitter.pushComma();
    emitter.writeComma();
    write(emitter, true);
    emitter.writeComma();
    write(emitter, false);
    emitter.writeRaw(']');
    emitter.popComma(comma);
    emitter.writeComma();
    write(emitter, false);

    CHECK(emitter.str() == "true,[true,false],false");
}

TEST_CASE("write_optional")
{
    JsonEmitter emitter;
    emitter.writeComma();
    write(emitter, std::optional<bool>{true});
    emitter.writeComma();
    write(emitter, std::nullopt);

    CHECK(emitter.str() == "true,null");
}

TEST_CASE("write_vector")
{
    std::vector<int> values{1, 2, 3, 4};
    JsonEmitter emitter;
    write(emitter, values);
    CHECK(emitter.str() == "[1,2,3,4]");
}

TEST_CASE("prevent_multiple_object_finish")
{
    JsonEmitter emitter;
    ObjectEmitter o = emitter.writeObject();
    o.writePair("a", "b");
    o.finish();
    o.finish();

    CHECK(emitter.str() == "{\"a\":\"b\"}");
}

TEST_CASE("prevent_multiple_array_finish")
{
    JsonEmitter emitter;
    ArrayEmitter a = emitter.writeArray();
    a.writeValue(1);
    a.finish();
    a.finish();

    CHECK(emitter.str() == "[1]");
}

TEST_CASE("cannot_write_pair_after_finished")
{
    JsonEmitter emitter;
    ObjectEmitter o = emitter.writeObject();
    o.finish();
    o.writePair("a", "b");

    CHECK(emitter.str() == "{}");
}

TEST_CASE("cannot_write_value_after_finished")
{
    JsonEmitter emitter;
    ArrayEmitter a = emitter.writeArray();
    a.finish();
    a.writeValue(1);

    CHECK(emitter.str() == "[]");
}

TEST_CASE("finish_when_destructing_object")
{
    JsonEmitter emitter;
    emitter.writeObject();

    CHECK(emitter.str() == "{}");
}

TEST_CASE("finish_when_destructing_array")
{
    JsonEmitter emitter;
    emitter.writeArray();

    CHECK(emitter.str() == "[]");
}

namespace Luau::Json
{

struct Special
{
    int foo;
    int bar;
};

void write(JsonEmitter& emitter, const Special& value)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("foo", value.foo);
    o.writePair("bar", value.bar);
}

} // namespace Luau::Json

TEST_CASE("afford_extensibility")
{
    std::vector<Special> vec{Special{1, 2}, Special{3, 4}};
    JsonEmitter e;
    write(e, vec);

    std::string result = e.str();
    CHECK(result == R"([{"foo":1,"bar":2},{"foo":3,"bar":4}])");
}

TEST_SUITE_END();
