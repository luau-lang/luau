// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/NumPrint.h"

#include "doctest.h"

#include <algorithm>
#include <atomic>
#include <cmath>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <thread>
#include <vector>

static std::string num2str(double n)
{
    char buf[LUAI_MAXNUM2STR];
    return std::string(buf, luai_num2str(buf, n));
}

static std::string num2strf(float n)
{
    char buf[LUAI_MAXNUM2STR];
    return std::string(buf, luai_num2strf(buf, n));
}

static float floatFromBits(uint32_t bits)
{
    float f;
    memcpy(&f, &bits, sizeof(f));
    return f;
}

static bool sameFloat(float a, float b)
{
    return memcmp(&a, &b, sizeof(float)) == 0;
}

static int significantDigits(const std::string& s)
{
    std::string digits;
    for (char c : s.substr(0, s.find('e')))
        if (c >= '0' && c <= '9')
            digits += c;

    size_t first = digits.find_first_not_of('0');
    return first == std::string::npos ? 1 : int(digits.find_last_not_of('0') - first + 1);
}

// Whether a representation reads back as the same float both directly and through a double
static bool readsBack(const char* s, float f)
{
    return sameFloat(strtof(s, nullptr), f) && sameFloat(float(strtod(s, nullptr)), f);
}

// Checks that a finite float's representation reads back, and that the closest one with fewer significant digits doesn't
static bool checkFloat(float f)
{
    std::string s = num2strf(f);
    if (!readsBack(s.c_str(), f))
        return false;

    int digits = significantDigits(s);
    if (digits == 1)
        return true;

    char shorter[64];
    snprintf(shorter, sizeof(shorter), "%.*g", digits - 1, double(f));
    return !readsBack(shorter, f);
}

TEST_SUITE_BEGIN("NumPrint");

TEST_CASE("num2str")
{
    CHECK(num2str(0.0) == "0");
    CHECK(num2str(-0.0) == "-0");
    CHECK(num2str(0.1) == "0.1");
    CHECK(num2str(100) == "100");
    CHECK(num2str(1e21) == "1e+21");
    CHECK(num2str(5e-324) == "5e-324");
    CHECK(num2str(HUGE_VAL) == "inf");
    CHECK(num2str(-HUGE_VAL) == "-inf");
    CHECK(num2str(NAN) == "nan");
}

TEST_CASE("num2strf")
{
    CHECK(num2strf(0.0f) == "0");
    CHECK(num2strf(-0.0f) == "-0");
    CHECK(num2strf(0.1f) == "0.1");
    CHECK(num2strf(1.0f / 3.0f) == "0.33333334");
    CHECK(num2strf(16777216.0f) == "16777216");
    CHECK(num2strf(1e-45f) == "1e-45");
    CHECK(num2strf(3.4028235e38f) == "3.4028235e+38");
    CHECK(num2strf(112591100000000.0f) == "112591100000000");
    CHECK(num2strf(HUGE_VALF) == "inf");
    CHECK(num2strf(NAN) == "nan");
}

TEST_CASE("num2strf_reads_back_through_double")
{
    // The only float whose shortest representation reads back differently through a double; see luai_num2strf.
    float f = floatFromBits(0x15ae43fd);
    CHECK(num2strf(f) == "7.0385307e-26");
    CHECK(num2strf(-f) == "-7.0385307e-26");
    CHECK(checkFloat(f));
    CHECK(checkFloat(-f));

    // Its shortest representation, 7.038531e-26, reads back as its neighbour through a double.
    CHECK(sameFloat(strtof("7.038531e-26", nullptr), f));
    CHECK(!sameFloat(float(strtod("7.038531e-26", nullptr)), f));
}

TEST_CASE("num2strf_sampled_floats")
{
    size_t failures = 0;
    for (uint64_t bits = 0; bits < 0x7f800000; bits += 4093)
        failures += !checkFloat(floatFromBits(uint32_t(bits)));

    CHECK(failures == 0);
}

// Checks every finite float, which takes around 20 minutes on 16 cores, so it only runs when asked for:
// Luau.UnitTest -tc=num2strf_all_floats --no-skip
TEST_CASE("num2strf_all_floats" * doctest::skip())
{
    std::atomic<size_t> failures{0};
    std::vector<std::thread> threads;
    unsigned count = std::max(1u, std::thread::hardware_concurrency());

    for (unsigned t = 0; t < count; t++)
    {
        threads.emplace_back(
            [&, t]
            {
                size_t local = 0;
                for (uint64_t bits = t; bits < 0x7f800000; bits += count)
                {
                    float f = floatFromBits(uint32_t(bits));
                    local += !checkFloat(f) + !checkFloat(-f);
                }
                failures += local;
            }
        );
    }

    for (std::thread& thread : threads)
        thread.join();

    CHECK(failures == 0);
}

TEST_SUITE_END();
