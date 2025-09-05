// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"

#include "Luau/CodeGenCommon.h"

#define DOCTEST_CONFIG_IMPLEMENT
// Our calls to parseOption/parseFlag don't provide a prefix so set the prefix to the empty string.
#define DOCTEST_CONFIG_OPTIONS_PREFIX ""
#include "doctest.h"

#include "RegisterCallbacks.h"

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h> // IsDebuggerPresent
#endif

#if defined(CODEGEN_TARGET_X64)
#include <immintrin.h>
#endif

#ifdef __APPLE__
#include <sys/types.h>
#include <sys/sysctl.h>
#endif

#include <fstream>
#include <iostream>
#include <optional>

#include <stdio.h>

// Indicates if verbose output is enabled; can be overridden via --verbose
// Currently, this enables output from 'print', but other verbose output could be enabled eventually.
bool verbose = false;

// Default optimization level for conformance test; can be overridden via -On
int optimizationLevel = 1;

// Run conformance tests with native code generation
bool codegen = false;

// Something to seed a pseudorandom number generator with
std::optional<unsigned> randomSeed;

static bool skipFastFlag(const char* flagName)
{
    if (strncmp(flagName, "Test", 4) == 0)
        return true;

    if (strncmp(flagName, "Debug", 5) == 0)
        return true;

    if (strcmp(flagName, "StudioReportLuauAny2") == 0)
        return true;

    return false;
}

static bool debuggerPresent()
{
#if defined(_WIN32)
    return 0 != IsDebuggerPresent();
#elif defined(__APPLE__)
    // ask sysctl information about a specific process ID
    int mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PID, getpid()};
    kinfo_proc info = {};
    size_t size = sizeof(info);
    int ret = sysctl(mib, sizeof(mib) / sizeof(*mib), &info, &size, nullptr, 0);
    // debugger is attached if the P_TRACED flag is set
    return ret == 0 && (info.kp_proc.p_flag & P_TRACED) != 0;
#elif defined(__linux__)
    FILE* st = fopen("/proc/self/status", "r");
    if (!st)
        return false; // assume no debugger is attached.

    int tpid = 0;
    char buf[256];

    while (fgets(buf, sizeof(buf), st))
    {
        if (strncmp(buf, "TracerPid:\t", 11) == 0)
        {
            tpid = atoi(buf + 11);
            break;
        }
    }

    fclose(st);
    return tpid != 0;
#else
    return false; // assume no debugger is attached.
#endif
}

static int testAssertionHandler(const char* expr, const char* file, int line, const char* function)
{
    if (debuggerPresent())
        return 1; // LUAU_ASSERT will trigger LUAU_DEBUGBREAK for a more convenient debugging experience

    ADD_FAIL_AT(file, line, "Assertion failed: ", std::string(expr));
    return 1;
}

struct BoostLikeReporter : doctest::IReporter
{
    const doctest::TestCaseData* currentTest = nullptr;

    BoostLikeReporter(const doctest::ContextOptions& in) {}

    // called when a query should be reported (listing test cases, printing the version, etc.)
    void report_query(const doctest::QueryData& qd) override
    {
        for (unsigned int i = 0; i < qd.num_data; ++i)
        {
            const doctest::TestCaseData& tc = *qd.data[i];

            fprintf(stderr, "%s/%s\n", tc.m_test_suite, tc.m_name);
        }

        fprintf(stderr, "Found %d tests.\n", int(qd.num_data));
    }

    // called when the whole test run starts/ends
    void test_run_start() override {}

    void test_run_end(const doctest::TestRunStats& ts) override {}

    // called when a test case is started (safe to cache a pointer to the input)
    void test_case_start(const doctest::TestCaseData& tc) override
    {
        currentTest = &tc;

        printf("Entering test suite \"%s\"\n", tc.m_test_suite);
        printf("Entering test case \"%s\"\n", tc.m_name);
    }

    // called when a test case has ended
    void test_case_end(const doctest::CurrentTestCaseStats& tc) override
    {
        LUAU_ASSERT(currentTest);

        printf("Leaving test case \"%s\"\n", currentTest->m_name);
        printf("Leaving test suite \"%s\"\n", currentTest->m_test_suite);

        currentTest = nullptr;
    }

    // called when an exception is thrown from the test case (or it crashes)
    void test_case_exception(const doctest::TestCaseException& e) override
    {
        LUAU_ASSERT(currentTest);

        printf("%s(%d): FATAL: Unhandled exception %s\n", currentTest->m_file.c_str(), currentTest->m_line, e.error_string.c_str());
    }

    // called whenever a subcase is entered/exited (noop)
    void test_case_reenter(const doctest::TestCaseData&) override {}
    void subcase_start(const doctest::SubcaseSignature&) override {}
    void subcase_end() override {}

    void log_assert(const doctest::AssertData& ad) override
    {
        if (!ad.m_failed)
            return;

        if (ad.m_decomp.size())
            printf("%s(%d): ERROR: %s (%s)\n", ad.m_file, ad.m_line, ad.m_expr, ad.m_decomp.c_str());
        else
            printf("%s(%d): ERROR: %s\n", ad.m_file, ad.m_line, ad.m_expr);
    }

    void log_message(const doctest::MessageData& md) override
    {
        const char* severity = (md.m_severity & doctest::assertType::is_warn) ? "WARNING" : "ERROR";

        printf("%s(%d): %s: %s\n", md.m_file, md.m_line, severity, md.m_string.c_str());
    }

    // called when a test case is skipped either because it doesn't pass the filters, has a skip decorator
    // or isn't in the execution range (between first and last) (safe to cache a pointer to the input)
    void test_case_skipped(const doctest::TestCaseData&) override {}
};

struct TeamCityReporter : doctest::IReporter
{
    const doctest::TestCaseData* currentTest = nullptr;

    TeamCityReporter(const doctest::ContextOptions& in) {}

    void report_query(const doctest::QueryData&) override {}

    void test_run_start() override {}

    void test_run_end(const doctest::TestRunStats& /*in*/) override {}

    void test_case_start(const doctest::TestCaseData& in) override
    {
        currentTest = &in;
        printf("##teamcity[testStarted name='%s: %s' captureStandardOutput='true']\n", in.m_test_suite, in.m_name);
    }

    // called when a test case is reentered because of unfinished subcases
    void test_case_reenter(const doctest::TestCaseData& /*in*/) override {}

    void test_case_end(const doctest::CurrentTestCaseStats& in) override
    {
        printf(
            "##teamcity[testMetadata testName='%s: %s' name='total_asserts' type='number' value='%d']\n",
            currentTest->m_test_suite,
            currentTest->m_name,
            in.numAssertsCurrentTest
        );
        printf(
            "##teamcity[testMetadata testName='%s: %s' name='failed_asserts' type='number' value='%d']\n",
            currentTest->m_test_suite,
            currentTest->m_name,
            in.numAssertsFailedCurrentTest
        );
        printf(
            "##teamcity[testMetadata testName='%s: %s' name='runtime' type='number' value='%f']\n",
            currentTest->m_test_suite,
            currentTest->m_name,
            in.seconds
        );

        if (!in.testCaseSuccess)
            printf("##teamcity[testFailed name='%s: %s']\n", currentTest->m_test_suite, currentTest->m_name);

        printf("##teamcity[testFinished name='%s: %s']\n", currentTest->m_test_suite, currentTest->m_name);
    }

    void test_case_exception(const doctest::TestCaseException& in) override
    {
        printf(
            "##teamcity[testFailed name='%s: %s' message='Unhandled exception' details='%s']\n",
            currentTest->m_test_suite,
            currentTest->m_name,
            in.error_string.c_str()
        );
    }

    void subcase_start(const doctest::SubcaseSignature& /*in*/) override {}
    void subcase_end() override {}

    void log_assert(const doctest::AssertData& ad) override
    {
        if (!ad.m_failed)
            return;

        if (ad.m_decomp.size())
            fprintf(stderr, "%s(%d): ERROR: %s (%s)\n", ad.m_file, ad.m_line, ad.m_expr, ad.m_decomp.c_str());
        else
            fprintf(stderr, "%s(%d): ERROR: %s\n", ad.m_file, ad.m_line, ad.m_expr);
    }

    void log_message(const doctest::MessageData& md) override
    {
        const char* severity = (md.m_severity & doctest::assertType::is_warn) ? "WARNING" : "ERROR";
        bool isError = md.m_severity & (doctest::assertType::is_require | doctest::assertType::is_check);
        fprintf(isError ? stderr : stdout, "%s(%d): %s: %s\n", md.m_file, md.m_line, severity, md.m_string.c_str());
    }

    void test_case_skipped(const doctest::TestCaseData& in) override
    {
        printf("##teamcity[testIgnored name='%s: %s' captureStandardOutput='false']\n", in.m_test_suite, in.m_name);
    }
};

REGISTER_REPORTER("teamcity", 1, TeamCityReporter);

template<typename T>
using FValueResult = std::pair<std::string, T>;

static FValueResult<std::optional<std::string>> parseFValueHelper(std::string_view view)
{
    size_t equalpos = view.find('=');
    if (equalpos == std::string_view::npos)
        return {std::string{view}, std::nullopt};

    std::string name{view.substr(0, equalpos)};
    view.remove_prefix(equalpos + 1);
    return {name, std::string{view}};
}

static FValueResult<int> parseFInt(std::string_view view)
{
    // If this was an FInt but there were no provided value, we should be noisy about that.
    // std::stoi already throws an exception on invalid conversion.
    auto [name, value] = parseFValueHelper(view);
    if (!value)
        throw std::runtime_error("Expected a value associated with " + name);

    return {name, std::stoi(*value)};
}

static FValueResult<bool> parseFFlag(std::string_view view)
{
    // If we have a flag name but there's no provided value, we default to true.
    auto [name, value] = parseFValueHelper(view);
    bool state = value ? *value == "true" : true;
    if (value && value != "true" && value != "false")
        fprintf(stderr, "Ignored '%s' because '%s' is not a valid flag state\n", name.c_str(), value->c_str());

    return {name, state};
}

template<typename T>
static void setFastValue(const std::string& name, T value)
{
    for (Luau::FValue<T>* fvalue = Luau::FValue<T>::list; fvalue; fvalue = fvalue->next)
        if (fvalue->name == name)
            fvalue->value = value;
}

static void setFastFlags(const std::vector<doctest::String>& flags)
{
    for (const doctest::String& flag : flags)
    {
        std::string_view view = flag.c_str();
        if (view == "true" || view == "false")
        {
            for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
            {
                if (!skipFastFlag(flag->name))
                    flag->value = view == "true";
            }

            continue;
        }

        if (view.size() >= 2 && view[0] == 'D' && view[1] == 'F')
            view.remove_prefix(1);

        if (view.substr(0, 4) == "FInt")
        {
            auto [name, value] = parseFInt(view.substr(4));
            setFastValue(name, value);
        }
        else
        {
            // We want to prevent the footgun where '--fflags=LuauSomeFlag' is ignored. We'll assume that this was declared as FFlag.
            auto [name, value] = parseFFlag(view.substr(0, 5) == "FFlag" ? view.substr(5) : view);
            setFastValue(name, value);
        }
    }
}

// This function performs system/architecture specific initialization prior to running tests.
static void initSystem()
{
#if defined(CODEGEN_TARGET_X64)
    // Some unit tests make use of denormalized numbers.  So flags to flush to zero or treat denormals as zero
    // must be disabled for expected behavior.
    _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_OFF);
    _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_OFF);
#endif
}

int main(int argc, char** argv)
{
    initSystem();

    Luau::assertHandler() = testAssertionHandler;

    doctest::registerReporter<BoostLikeReporter>("boost", 0, true);

    doctest::Context context;
    context.setOption("no-version", true);
    context.applyCommandLine(argc, argv);

    if (doctest::parseFlag(argc, argv, "--list-fflags"))
    {
        for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
        {
            if (skipFastFlag(flag->name))
                continue;

            printf("%sFFlag%s\n", flag->dynamic ? "D" : "", flag->name);
        }

        return 0;
    }

    if (doctest::parseFlag(argc, argv, "--verbose"))
    {
        verbose = true;
    }

    if (doctest::parseFlag(argc, argv, "--codegen"))
    {
        codegen = true;
    }

    int level = -1;
    if (doctest::parseIntOption(argc, argv, "-O", doctest::option_int, level))
    {
        if (level < 0 || level > 2)
            fprintf(stderr, "Optimization level must be between 0 and 2 inclusive\n");
        else
            optimizationLevel = level;
    }

    int rseed = -1;
    if (doctest::parseIntOption(argc, argv, "--random-seed=", doctest::option_int, rseed))
        randomSeed = unsigned(rseed);

    if (doctest::parseOption(argc, argv, "--randomize") && !randomSeed)
    {
        randomSeed = unsigned(time(nullptr));
        printf("Using RNG seed %u\n", *randomSeed);
    }

    if (std::vector<doctest::String> flags; doctest::parseCommaSepArgs(argc, argv, "--fflags=", flags))
        setFastFlags(flags);

    if (doctest::parseFlag(argc, argv, "--list_content"))
    {
        const char* ltc[] = {argv[0], "--list-test-cases"};
        context.applyCommandLine(2, ltc);
    }

    doctest::String filter;
    if (doctest::parseOption(argc, argv, "--run_test", &filter) && filter[0] == '=')
    {
        if (doctest::parseOption(argc, argv, "--run_suites_in_file"))
        {
            fprintf(stderr, "ERROR: Cannot pass both --run_test and --run_suites_in_file\n");
            return 1;
        }
        if (doctest::parseOption(argc, argv, "--run_cases_in_file"))
        {
            fprintf(stderr, "ERROR: Cannot pass both --run_test and --run_cases_in_file\n");
            return 1;
        }
        const char* f = filter.c_str() + 1;
        const char* s = strchr(f, '/');

        if (s)
        {
            context.addFilter("test-suite", std::string(f, s).c_str());
            context.addFilter("test-case", s + 1);
        }
        else
        {
            context.addFilter("test-suite", f);
        }
    }

    doctest::String suite_filter_path;
    if (doctest::parseOption(argc, argv, "--run_suites_in_file", &suite_filter_path) && suite_filter_path[0] == '=')
    {
        const char* filter_file = suite_filter_path.c_str() + 1;
        std::ifstream filter_stream(filter_file);
        std::stringstream buffer;
        buffer << filter_stream.rdbuf();
        std::string suite_list = buffer.str();
        context.addFilter("test-suite", suite_list.c_str());
    }

    doctest::String case_filter_path;
    if (doctest::parseOption(argc, argv, "--run_cases_in_file", &case_filter_path) && case_filter_path[0] == '=')
    {
        const char* filter_file = case_filter_path.c_str() + 1;
        std::ifstream filter_stream(filter_file);
        std::stringstream buffer;
        buffer << filter_stream.rdbuf();
        std::string case_list = buffer.str();
        context.addFilter("test-path", case_list.c_str());
    }

    // These callbacks register unit tests that need runtime support to be
    // correctly set up. Running them here means that all command line flags
    // have been parsed, fast flags have been set, and we've potentially already
    // exited. Once doctest::Context::run is invoked, the test list will be
    // picked up from global state.
    for (Luau::RegisterCallback cb : Luau::getRegisterCallbacks())
        cb();

    int result = context.run();
    if (doctest::parseFlag(argc, argv, "--help") || doctest::parseFlag(argc, argv, "-h"))
    {
        printf("Additional command line options:\n");
        printf(" -O[n]                                 Changes default optimization level (1) for conformance runs\n");
        printf(" --verbose                             Enables verbose output (e.g. lua 'print' statements)\n");
        printf(" --fflags=                             Sets specified fast flags\n");
        printf(" --list-fflags                         List all fast flags\n");
        printf(" --randomize                           Use a random RNG seed\n");
        printf(" --random-seed=n                       Use a particular RNG seed\n");
    }
    return result;
}
