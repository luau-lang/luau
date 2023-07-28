// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/StringUtils.h"

#include "doctest.h"

#include <iostream>

namespace
{
using LevenshteinMatrix = std::vector<std::vector<size_t>>;

std::string format(std::string_view a, std::string_view b, size_t expected, size_t actual)
{
    return "Distance of '" + std::string(a) + "' and '" + std::string(b) + "': expected " + std::to_string(expected) + ", got " +
           std::to_string(actual);
}

// Each call to this function is not one test, but instead actually runs tests (A.size() * B.size()) + 2 times.
void compareLevenshtein(LevenshteinMatrix distances, std::string_view a, std::string_view b)
{
    for (size_t x = 0; x <= a.size(); ++x)
    {
        for (size_t y = 0; y <= b.size(); ++y)
        {
            std::string_view currentA = a.substr(0, x);
            std::string_view currentB = b.substr(0, y);

            size_t actual = Luau::editDistance(currentA, currentB);
            size_t expected = distances[x][y];
            CHECK_MESSAGE(actual == expected, format(currentA, currentB, expected, actual));
        }
    }
}
} // namespace

TEST_SUITE_BEGIN("StringUtilsTest");

#if 0
// This unit test is only used to measure how performant the current levenshtein distance algorithm is.
// It is entirely ok to submit this, but keep #if 0.
TEST_CASE("BenchmarkLevenshteinDistance")
{
    // For reference: running this benchmark on a Macbook Pro 16 takes ~250ms.

    int count = 1'000'000;

    // specifically chosen because they:
    // - are real words,
    // - have common prefix and suffix, and
    // - are sufficiently long enough to stress test with
    std::string_view a("Intercalate");
    std::string_view b("Interchangeable");

    auto start = std::chrono::steady_clock::now();

    for (int i = 0; i < count; ++i)
        Luau::editDistance(a, b);

    auto end = std::chrono::steady_clock::now();
    auto time = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);

    MESSAGE("Running levenshtein distance ", count, " times took ", time.count(), "ms");
}
#endif

TEST_CASE("LevenshteinDistanceKittenSitting")
{
    LevenshteinMatrix distances{
        {0, 1, 2, 3, 4, 5, 6, 7}, //  S I T T I N G
        {1, 1, 2, 3, 4, 5, 6, 7}, // K
        {2, 2, 1, 2, 3, 4, 5, 6}, // I
        {3, 3, 2, 1, 2, 3, 4, 5}, // T
        {4, 4, 3, 2, 1, 2, 3, 4}, // T
        {5, 5, 4, 3, 2, 2, 3, 4}, // E
        {6, 6, 5, 4, 3, 3, 2, 3}, // N
    };

    compareLevenshtein(distances, "kitten", "sitting");
}

TEST_CASE("LevenshteinDistanceSaturdaySunday")
{
    LevenshteinMatrix distances{
        {0, 1, 2, 3, 4, 5, 6}, //  S U N D A Y
        {1, 0, 1, 2, 3, 4, 5}, // S
        {2, 1, 1, 2, 3, 3, 4}, // A
        {3, 2, 2, 2, 3, 4, 4}, // T
        {4, 3, 2, 3, 3, 4, 5}, // U
        {5, 4, 3, 3, 4, 4, 5}, // R
        {6, 5, 4, 4, 3, 4, 5}, // D
        {7, 6, 5, 5, 4, 3, 4}, // A
        {8, 7, 6, 6, 5, 4, 3}, // Y
    };

    compareLevenshtein(distances, "saturday", "sunday");
}

TEST_CASE("EditDistanceIsAgnosticOfArgumentOrdering")
{
    CHECK_EQ(Luau::editDistance("blox", "block"), Luau::editDistance("block", "blox"));
}

TEST_CASE("AreWeUsingDistanceWithAdjacentTranspositionsAndNotOptimalStringAlignment")
{
    size_t distance = Luau::editDistance("CA", "ABC");
    CHECK_EQ(distance, 2);
}

TEST_CASE("EditDistanceSupportsUnicode")
{
    // ASCII character
    CHECK_EQ(Luau::editDistance("A block", "X block"), 1);

    // UTF-8 2 byte character
    CHECK_EQ(Luau::editDistance("A block", "À block"), 2);

    // UTF-8 3 byte character
    CHECK_EQ(Luau::editDistance("A block", "⪻ block"), 3);

    // UTF-8 4 byte character
    CHECK_EQ(Luau::editDistance("A block", "𒋄 block"), 4);

    // UTF-8 extreme characters
    CHECK_EQ(Luau::editDistance("A block", "R̴̨̢̟̚ŏ̶̳̳͚́ͅb̶̡̻̞̐̿ͅl̸̼͝ợ̷̜͓̒̏͜͝ẍ̴̝̦̟̰́̒́̌ block"), 85);
}

TEST_SUITE_END();
