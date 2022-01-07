// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"

#define DOCTEST_CONFIG_IMPLEMENT
// Our calls to parseOption/parseFlag don't provide a prefix so set the prefix to the empty string.
#define DOCTEST_CONFIG_OPTIONS_PREFIX ""
#include "doctest.h"

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <Windows.h> // IsDebuggerPresent
#endif

#ifdef __APPLE__
#include <sys/types.h>
#include <sys/sysctl.h>
#endif

#include <optional>

// Indicates if verbose output is enabled.
// Currently, this enables  output from lua's 'print', but other verbose output could be enabled eventually.
bool verbose = false;

static bool skipFastFlag(const char* flagName)
{
    if (strncmp(flagName, "Test", 4) == 0)
        return true;

    if (strncmp(flagName, "Debug", 5) == 0)
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
#else
    return false; // assume no debugger is attached.
#endif
}

static int assertionHandler(const char* expr, const char* file, int line, const char* function)
{
    if (debuggerPresent())
        LUAU_DEBUGBREAK();

    ADD_FAIL_AT(file, line, "Assertion failed: ", expr);
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
    { //
        printf("%s(%d): ERROR: %s\n", md.m_file, md.m_line, md.m_string.c_str());
    }

    // called when a test case is skipped either because it doesn't pass the filters, has a skip decorator
    // or isn't in the execution range (between first and last) (safe to cache a pointer to the input)
    void test_case_skipped(const doctest::TestCaseData&) override {}
};

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
        std::cerr << "Ignored '" << name << "' because '" << *value << "' is not a valid FFlag state." << std::endl;

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

int main(int argc, char** argv)
{
    Luau::assertHandler() = assertionHandler;

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

            if (flag->dynamic)
                std::cout << 'D';
            std::cout << "FFlag" << flag->name << std::endl;
        }

        return 0;
    }

    if (doctest::parseFlag(argc, argv, "--verbose"))
    {
        verbose = true;
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

    int result = context.run();
    if (doctest::parseFlag(argc, argv, "--help") || doctest::parseFlag(argc, argv, "-h"))
    {
        printf("Additional command line options:\n");
        printf(" --verbose                             Enables verbose output (e.g. lua 'print' statements)\n");
        printf(" --fflags=                             Sets specified fast flags\n");
        printf(" --list-fflags                         List all fast flags\n");
    }
    return result;
}


