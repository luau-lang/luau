// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <type_traits>
#include <string>
#include <optional>
#include <unordered_map>
#include <vector>

#include "Luau/NotNull.h"

namespace Luau::Json
{

struct JsonEmitter;

/// Writes a value to the JsonEmitter. Note that this can produce invalid JSON
/// if you do not insert commas or appropriate object / array syntax.
template<typename T>
void write(JsonEmitter&, T) = delete;

/// Writes a boolean to a JsonEmitter.
/// @param emitter the emitter to write to.
/// @param b the boolean to write.
void write(JsonEmitter& emitter, bool b);

/// Writes an integer to a JsonEmitter.
/// @param emitter the emitter to write to.
/// @param i the integer to write.
void write(JsonEmitter& emitter, int i);

/// Writes an integer to a JsonEmitter.
/// @param emitter the emitter to write to.
/// @param i the integer to write.
void write(JsonEmitter& emitter, long i);

/// Writes an integer to a JsonEmitter.
/// @param emitter the emitter to write to.
/// @param i the integer to write.
void write(JsonEmitter& emitter, long long i);

/// Writes an integer to a JsonEmitter.
/// @param emitter the emitter to write to.
/// @param i the integer to write.
void write(JsonEmitter& emitter, unsigned int i);

/// Writes an integer to a JsonEmitter.
/// @param emitter the emitter to write to.
/// @param i the integer to write.
void write(JsonEmitter& emitter, unsigned long i);

/// Writes an integer to a JsonEmitter.
/// @param emitter the emitter to write to.
/// @param i the integer to write.
void write(JsonEmitter& emitter, unsigned long long i);

/// Writes a double to a JsonEmitter.
/// @param emitter the emitter to write to.
/// @param d the double to write.
void write(JsonEmitter& emitter, double d);

/// Writes a string to a JsonEmitter. The string will be escaped.
/// @param emitter the emitter to write to.
/// @param sv the string to write.
void write(JsonEmitter& emitter, std::string_view sv);

/// Writes a character to a JsonEmitter as a single-character string. The
/// character will be escaped.
/// @param emitter the emitter to write to.
/// @param c the string to write.
void write(JsonEmitter& emitter, char c);

/// Writes a string to a JsonEmitter. The string will be escaped.
/// @param emitter the emitter to write to.
/// @param str the string to write.
void write(JsonEmitter& emitter, const char* str);

/// Writes a string to a JsonEmitter. The string will be escaped.
/// @param emitter the emitter to write to.
/// @param str the string to write.
void write(JsonEmitter& emitter, const std::string& str);

/// Writes null to a JsonEmitter.
/// @param emitter the emitter to write to.
void write(JsonEmitter& emitter, std::nullptr_t);

/// Writes null to a JsonEmitter.
/// @param emitter the emitter to write to.
void write(JsonEmitter& emitter, std::nullopt_t);

struct ObjectEmitter;
struct ArrayEmitter;

struct JsonEmitter
{
    JsonEmitter();

    /// Converts the current contents of the JsonEmitter to a string value. This
    /// does not invalidate the emitter, but it does not clear it either.
    std::string str();

    /// Returns the current comma state and resets it to false. Use popComma to
    /// restore the old state.
    /// @returns the previous comma state.
    bool pushComma();

    /// Restores a previous comma state.
    /// @param c the comma state to restore.
    void popComma(bool c);

    /// Writes a raw sequence of characters to the buffer, without escaping or
    /// other processing.
    /// @param sv the character sequence to write.
    void writeRaw(std::string_view sv);

    /// Writes a character to the buffer, without escaping or other processing.
    /// @param c the character to write.
    void writeRaw(char c);

    /// Writes a comma if this wasn't the first time writeComma has been
    /// invoked. Otherwise, sets the comma state to true.
    /// @see pushComma
    /// @see popComma
    void writeComma();

    /// Begins writing an object to the emitter.
    /// @returns an ObjectEmitter that can be used to write key-value pairs.
    ObjectEmitter writeObject();

    /// Begins writing an array to the emitter.
    /// @returns an ArrayEmitter that can be used to write values.
    ArrayEmitter writeArray();

private:
    bool comma = false;
    std::vector<std::string> chunks;

    void newChunk();
};

/// An interface for writing an object into a JsonEmitter instance.
/// @see JsonEmitter::writeObject
struct ObjectEmitter
{
    ObjectEmitter(NotNull<JsonEmitter> emitter);
    ~ObjectEmitter();

    NotNull<JsonEmitter> emitter;
    bool comma;
    bool finished;

    /// Writes a key-value pair to the associated JsonEmitter. Keys will be escaped.
    /// @param name the name of the key-value pair.
    /// @param value the value to write.
    template<typename T>
    void writePair(std::string_view name, T value)
    {
        if (finished)
        {
            return;
        }

        emitter->writeComma();
        write(*emitter, name);
        emitter->writeRaw(':');
        write(*emitter, value);
    }

    /// Finishes writing the object, appending a closing `}` character and
    /// resetting the comma state of the associated emitter. This can only be
    /// called once, and once called will render the emitter unusable. This
    /// method is also called when the ObjectEmitter is destructed.
    void finish();
};

/// An interface for writing an array into a JsonEmitter instance. Array values
/// do not need to be the same type.
/// @see JsonEmitter::writeArray
struct ArrayEmitter
{
    ArrayEmitter(NotNull<JsonEmitter> emitter);
    ~ArrayEmitter();

    NotNull<JsonEmitter> emitter;
    bool comma;
    bool finished;

    /// Writes a value to the array.
    /// @param value the value to write.
    template<typename T>
    void writeValue(T value)
    {
        if (finished)
        {
            return;
        }

        emitter->writeComma();
        write(*emitter, value);
    }

    /// Finishes writing the object, appending a closing `]` character and
    /// resetting the comma state of the associated emitter. This can only be
    /// called once, and once called will render the emitter unusable. This
    /// method is also called when the ArrayEmitter is destructed.
    void finish();
};

/// Writes a vector as an array to a JsonEmitter.
/// @param emitter the emitter to write to.
/// @param vec the vector to write.
template<typename T>
void write(JsonEmitter& emitter, const std::vector<T>& vec)
{
    ArrayEmitter a = emitter.writeArray();

    for (const T& value : vec)
        a.writeValue(value);

    a.finish();
}

/// Writes an optional to a JsonEmitter. Will write the contained value, if
/// present, or null, if no value is present.
/// @param emitter the emitter to write to.
/// @param v the value to write.
template<typename T>
void write(JsonEmitter& emitter, const std::optional<T>& v)
{
    if (v.has_value())
        write(emitter, *v);
    else
        emitter.writeRaw("null");
}

template<typename T>
void write(JsonEmitter& emitter, const std::unordered_map<std::string, T>& map)
{
    ObjectEmitter o = emitter.writeObject();

    for (const auto& [k, v] : map)
        o.writePair(k, v);

    o.finish();
}

} // namespace Luau::Json
