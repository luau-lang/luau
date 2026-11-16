// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>

// Shared by the TableFormat tests in Luau.UnitTest and Luau.Conformance.

inline const char* const kTableFormatRfcExample = R"(-- Settings for the example service.
return {
    name = "example",
    enabled = true,
    retries = 3,
    timeout = 2.5e3,
    flags = 0b1010_0001,
    threshold = -math.huge,
    origin = vector.create(0, 10, -5),
    tags = { "fast", "beta", },
    [1] = "first numeric key";
    ["display name"] = `Example Service`,
    limits = {
        [true] = math.huge,
        [false] = 0,
    },
}
)";

// Values that are easy to write in a way that doesn't read back exactly.
inline const double kTableFormatRoundTripNumbers[] =
    {0.1, 1.0 / 3.0, 1e300, 5e-324, 2.2250738585072014e-308, 9007199254740993.0, 123456789012345680000.0, -1e-7, 100, -0.0};
inline const float kTableFormatRoundTripFloats[] = {0.1f, 1.0f / 3.0f, 3.4028235e38f, 1e-45f, -16777217.0f};
inline const std::string kTableFormatRoundTripStrings[] =
    {std::string("\0\0001", 3), "\a\b\f\v\x7f", "\"'`{}[[]]", "\xff\xfe", "multi\r\nline", "end"};

// A source with tables nested the given number of levels deep.
inline std::string tableFormatNested(size_t depth)
{
    return std::string(depth, '{') + std::string(depth, '}');
}
