// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/JsonEmitter.h"

#include "Luau/StringUtils.h"

#include <string.h>

namespace Luau::Json
{

static constexpr int CHUNK_SIZE = 1024;

ObjectEmitter::ObjectEmitter(NotNull<JsonEmitter> emitter)
    : emitter(emitter)
    , finished(false)
{
    comma = emitter->pushComma();
    emitter->writeRaw('{');
}

ObjectEmitter::~ObjectEmitter()
{
    finish();
}

void ObjectEmitter::finish()
{
    if (finished)
        return;

    emitter->writeRaw('}');
    emitter->popComma(comma);
    finished = true;
}

ArrayEmitter::ArrayEmitter(NotNull<JsonEmitter> emitter)
    : emitter(emitter)
    , finished(false)
{
    comma = emitter->pushComma();
    emitter->writeRaw('[');
}

ArrayEmitter::~ArrayEmitter()
{
    finish();
}

void ArrayEmitter::finish()
{
    if (finished)
        return;

    emitter->writeRaw(']');
    emitter->popComma(comma);
    finished = true;
}

JsonEmitter::JsonEmitter()
{
    newChunk();
}

std::string JsonEmitter::str()
{
    return join(chunks, "");
}

bool JsonEmitter::pushComma()
{
    bool current = comma;
    comma = false;
    return current;
}

void JsonEmitter::popComma(bool c)
{
    comma = c;
}

void JsonEmitter::writeRaw(std::string_view sv)
{
    if (sv.size() > CHUNK_SIZE)
    {
        chunks.emplace_back(sv);
        newChunk();
        return;
    }

    auto& chunk = chunks.back();
    if (chunk.size() + sv.size() < CHUNK_SIZE)
    {
        chunk.append(sv.data(), sv.size());
        return;
    }

    size_t prefix = CHUNK_SIZE - chunk.size();
    chunk.append(sv.data(), prefix);
    newChunk();

    chunks.back().append(sv.data() + prefix, sv.size() - prefix);
}

void JsonEmitter::writeRaw(char c)
{
    writeRaw(std::string_view{&c, 1});
}

void write(JsonEmitter& emitter, bool b)
{
    if (b)
        emitter.writeRaw("true");
    else
        emitter.writeRaw("false");
}

void write(JsonEmitter& emitter, double d)
{
    emitter.writeRaw(std::to_string(d));
}

void write(JsonEmitter& emitter, int i)
{
    emitter.writeRaw(std::to_string(i));
}

void write(JsonEmitter& emitter, long i)
{
    emitter.writeRaw(std::to_string(i));
}

void write(JsonEmitter& emitter, long long i)
{
    emitter.writeRaw(std::to_string(i));
}

void write(JsonEmitter& emitter, unsigned int i)
{
    emitter.writeRaw(std::to_string(i));
}

void write(JsonEmitter& emitter, unsigned long i)
{
    emitter.writeRaw(std::to_string(i));
}

void write(JsonEmitter& emitter, unsigned long long i)
{
    emitter.writeRaw(std::to_string(i));
}

void write(JsonEmitter& emitter, std::string_view sv)
{
    emitter.writeRaw('\"');

    for (char c : sv)
    {
        if (c == '"')
            emitter.writeRaw("\\\"");
        else if (c == '\\')
            emitter.writeRaw("\\\\");
        else if (c == '\n')
            emitter.writeRaw("\\n");
        else if (c < ' ')
            emitter.writeRaw(format("\\u%04x", c));
        else
            emitter.writeRaw(c);
    }

    emitter.writeRaw('\"');
}

void write(JsonEmitter& emitter, char c)
{
    write(emitter, std::string_view{&c, 1});
}

void write(JsonEmitter& emitter, const char* str)
{
    write(emitter, std::string_view{str, strlen(str)});
}

void write(JsonEmitter& emitter, const std::string& str)
{
    write(emitter, std::string_view{str});
}

void write(JsonEmitter& emitter, std::nullptr_t)
{
    emitter.writeRaw("null");
}

void write(JsonEmitter& emitter, std::nullopt_t)
{
    emitter.writeRaw("null");
}

void JsonEmitter::writeComma()
{
    if (comma)
        writeRaw(',');
    else
        comma = true;
}

ObjectEmitter JsonEmitter::writeObject()
{
    return ObjectEmitter{NotNull(this)};
}

ArrayEmitter JsonEmitter::writeArray()
{
    return ArrayEmitter{NotNull(this)};
}

void JsonEmitter::newChunk()
{
    chunks.emplace_back();
    chunks.back().reserve(CHUNK_SIZE);
}

} // namespace Luau::Json
