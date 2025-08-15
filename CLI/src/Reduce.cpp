// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Ast.h"
#include "Luau/Common.h"
#include "Luau/Parser.h"
#include "Luau/Transpiler.h"

#include "Luau/FileUtils.h"

#include <algorithm>
#include <stdio.h>
#include <string>
#include <string_view>
#include <queue>

#define VERBOSE 0 // 1 - print out commandline invocations.  2 - print out stdout

#if defined(_WIN32) && !defined(__MINGW32__)

const auto popen = &_popen;
const auto pclose = &_pclose;

#endif

using namespace Luau;

enum class TestResult
{
    BugFound, // We encountered the bug we are trying to isolate
    NoBug,    // We did not encounter the bug we are trying to isolate
};

struct Enqueuer : public AstVisitor
{
    std::queue<AstStatBlock*>* queue;

    explicit Enqueuer(std::queue<AstStatBlock*>* queue)
        : queue(queue)
    {
        LUAU_ASSERT(queue);
    }

    bool visit(AstStatBlock* block) override
    {
        queue->push(block);
        return false;
    }
};

struct Reducer
{
    Allocator allocator;
    AstNameTable nameTable{allocator};
    ParseOptions parseOptions;

    ParseResult parseResult;
    CstNodeMap cstNodeMap{nullptr};
    AstStatBlock* root;

    std::string scriptName;

    std::string command;
    std::string_view searchText;

    Reducer()
    {
        parseOptions.captureComments = true;
        parseOptions.storeCstData = true;
    }

    std::string readLine(FILE* f)
    {
        std::string line = "";
        char buffer[256];
        while (fgets(buffer, sizeof(buffer), f))
        {
            auto len = strlen(buffer);
            line += std::string(buffer, len);
            if (buffer[len - 1] == '\n')
                break;
        }

        return line;
    }

    void writeTempScript(bool minify = false)
    {
        std::string source = transpileWithTypes(*root, cstNodeMap);

        if (minify)
        {
            size_t pos = 0;
            do
            {
                pos = source.find("\n\n", pos);
                if (pos == std::string::npos)
                    break;

                source.erase(pos, 1);
            } while (true);
        }

        FILE* f = fopen(scriptName.c_str(), "w");
        if (!f)
        {
            printf("Unable to open temp script to %s\n", scriptName.c_str());
            exit(2);
        }

        for (const HotComment& comment : parseResult.hotcomments)
            fprintf(f, "--!%s\n", comment.content.c_str());

        auto written = fwrite(source.data(), 1, source.size(), f);
        if (written != source.size())
        {
            printf("??? %zu %zu\n", written, source.size());
            printf("Unable to write to temp script %s\n", scriptName.c_str());
            exit(3);
        }

        fclose(f);
    }

    int step = 0;

    std::string escape(const std::string& s)
    {
        std::string result;
        result.reserve(s.size() + 20); // guess
        result += '"';
        for (char c : s)
        {
            if (c == '"')
                result += '\\';
            result += c;
        }
        result += '"';

        return result;
    }

    TestResult run()
    {
        writeTempScript();

        std::string cmd = command;
        while (true)
        {
            auto pos = cmd.find("{}");
            if (std::string::npos == pos)
                break;

            cmd = cmd.substr(0, pos) + escape(scriptName) + cmd.substr(pos + 2);
        }

#if VERBOSE >= 1
        printf("running %s\n", cmd.c_str());
#endif

        TestResult result = TestResult::NoBug;

        ++step;
        printf("Step %4d...\n", step);

        FILE* p = popen(cmd.c_str(), "r");

        while (!feof(p))
        {
            std::string s = readLine(p);
#if VERBOSE >= 2
            printf("%s", s.c_str());
#endif
            if (std::string::npos != s.find(searchText))
            {
                result = TestResult::BugFound;
                break;
            }
        }

        pclose(p);

        return result;
    }

    std::vector<AstStat*> getNestedStats(AstStat* stat)
    {
        std::vector<AstStat*> result;

        auto append = [&](AstStatBlock* block)
        {
            if (block)
                result.insert(result.end(), block->body.data, block->body.data + block->body.size);
        };

        if (auto block = stat->as<AstStatBlock>())
            append(block);
        else if (auto ifs = stat->as<AstStatIf>())
        {
            append(ifs->thenbody);
            if (ifs->elsebody)
            {
                if (AstStatBlock* elseBlock = ifs->elsebody->as<AstStatBlock>())
                    append(elseBlock);
                else if (AstStatIf* elseIf = ifs->elsebody->as<AstStatIf>())
                {
                    auto innerStats = getNestedStats(elseIf);
                    result.insert(end(result), begin(innerStats), end(innerStats));
                }
                else
                {
                    printf("AstStatIf's else clause can have more statement types than I thought\n");
                    LUAU_ASSERT(0);
                }
            }
        }
        else if (auto w = stat->as<AstStatWhile>())
            append(w->body);
        else if (auto r = stat->as<AstStatRepeat>())
            append(r->body);
        else if (auto f = stat->as<AstStatFor>())
            append(f->body);
        else if (auto f = stat->as<AstStatForIn>())
            append(f->body);
        else if (auto f = stat->as<AstStatFunction>())
            append(f->func->body);
        else if (auto f = stat->as<AstStatLocalFunction>())
            append(f->func->body);

        return result;
    }

    // Move new body data into allocator-managed storage so that it's safe to keep around longterm.
    AstStat** reallocateStatements(const std::vector<AstStat*>& statements)
    {
        AstStat** newData = static_cast<AstStat**>(allocator.allocate(sizeof(AstStat*) * statements.size()));
        std::copy(statements.data(), statements.data() + statements.size(), newData);

        return newData;
    }

    // Semiopen interval
    using Span = std::pair<size_t, size_t>;

    // Generates 'chunks' semiopen spans of equal-ish size to span the indeces running from 0 to 'size'
    // Also inverses.
    std::vector<std::pair<Span, Span>> generateSpans(size_t size, size_t chunks)
    {
        if (size <= 1)
            return {};

        LUAU_ASSERT(chunks > 0);
        size_t chunkLength = std::max<size_t>(1, size / chunks);

        std::vector<std::pair<Span, Span>> result;

        auto append = [&result](Span a, Span b)
        {
            if (a.first == a.second && b.first == b.second)
                return;
            else
                result.emplace_back(a, b);
        };

        size_t i = 0;
        while (i < size)
        {
            size_t end = std::min(i + chunkLength, size);
            append(Span{0, i}, Span{end, size});

            i = end;
        }

        i = 0;
        while (i < size)
        {
            size_t end = std::min(i + chunkLength, size);
            append(Span{i, end}, Span{size, size});

            i = end;
        }

        return result;
    }

    // Returns the statements of block within span1 and span2
    // Also has the hokey restriction that span1 must come before span2
    std::vector<AstStat*> prunedSpan(AstStatBlock* block, Span span1, Span span2)
    {
        std::vector<AstStat*> result;

        for (size_t i = span1.first; i < span1.second; ++i)
            result.push_back(block->body.data[i]);

        for (size_t i = span2.first; i < span2.second; ++i)
            result.push_back(block->body.data[i]);

        return result;
    }

    // returns true if anything was culled plus the chunk count
    std::pair<bool, size_t> deleteChildStatements(AstStatBlock* block, size_t chunkCount)
    {
        if (block->body.size == 0)
            return {false, chunkCount};

        do
        {
            auto permutations = generateSpans(block->body.size, chunkCount);
            for (const auto& [span1, span2] : permutations)
            {
                auto tempStatements = prunedSpan(block, span1, span2);
                AstArray<AstStat*> backupBody{tempStatements.data(), tempStatements.size()};

                std::swap(block->body, backupBody);
                TestResult result = run();
                if (result == TestResult::BugFound)
                {
                    // The bug still reproduces without the statements we've culled.  Commit.
                    block->body.data = reallocateStatements(tempStatements);
                    return {true, std::max<size_t>(2, chunkCount - 1)};
                }
                else
                {
                    // The statements we've culled are critical for the reproduction of the bug.
                    // TODO try promoting its contents into this scope
                    std::swap(block->body, backupBody);
                }
            }

            chunkCount *= 2;
        } while (chunkCount <= block->body.size);

        return {false, block->body.size};
    }

    bool deleteChildStatements(AstStatBlock* b)
    {
        bool result = false;

        size_t chunkCount = 2;
        while (true)
        {
            auto [workDone, newChunkCount] = deleteChildStatements(b, chunkCount);
            if (workDone)
            {
                result = true;
                chunkCount = newChunkCount;
                continue;
            }
            else
                break;
        }

        return result;
    }

    bool tryPromotingChildStatements(AstStatBlock* b, size_t index)
    {
        std::vector<AstStat*> tempStats(b->body.data, b->body.data + b->body.size);
        AstStat* removed = tempStats.at(index);
        tempStats.erase(begin(tempStats) + index);

        std::vector<AstStat*> nestedStats = getNestedStats(removed);
        tempStats.insert(begin(tempStats) + index, begin(nestedStats), end(nestedStats));

        AstArray<AstStat*> tempArray{tempStats.data(), tempStats.size()};
        std::swap(b->body, tempArray);

        TestResult result = run();

        if (result == TestResult::BugFound)
        {
            b->body.data = reallocateStatements(tempStats);
            return true;
        }
        else
        {
            std::swap(b->body, tempArray);
            return false;
        }
    }

    // We live with some weirdness because I'm kind of lazy: If a statement's
    // contents are promoted, we try promoting those prometed statements right
    // away. I don't think it matters: If we can delete a statement and still
    // exhibit the bug, we should do so.  The order isn't so important.
    bool tryPromotingChildStatements(AstStatBlock* b)
    {
        size_t i = 0;
        while (i < b->body.size)
        {
            bool promoted = tryPromotingChildStatements(b, i);
            if (!promoted)
                ++i;
        }

        return false;
    }

    void walk(AstStatBlock* block)
    {
        std::queue<AstStatBlock*> queue;
        Enqueuer enqueuer{&queue};

        queue.push(block);

        while (!queue.empty())
        {
            AstStatBlock* b = queue.front();
            queue.pop();

            bool result = false;
            do
            {
                result = deleteChildStatements(b);

                /* Try other reductions here before we walk into child statements
                 * Other reductions to try someday:
                 *
                 * Promoting a statement's children to the enclosing block.
                 * Deleting type annotations
                 * Deleting parts of type annotations
                 * Replacing subexpressions with ({} :: any)
                 * Inlining type aliases
                 * Inlining constants
                 * Inlining functions
                 */
                result |= tryPromotingChildStatements(b);
            } while (result);

            for (AstStat* stat : b->body)
                stat->visit(&enqueuer);
        }
    }

    void run(const std::string scriptName, const std::string command, std::string_view source, std::string_view searchText)
    {
        this->scriptName = scriptName;

#if 0
        // Handy debugging trick: VS Code will update its view of the file in realtime as it is edited.
        std::string wheee = "code " + scriptName;
        system(wheee.c_str());
#endif

        printf("Script: %s\n", scriptName.c_str());

        this->command = command;
        this->searchText = searchText;

        parseResult = Parser::parse(source.data(), source.size(), nameTable, allocator, parseOptions);
        if (!parseResult.errors.empty())
        {
            printf("Parse errors\n");
            exit(1);
        }

        root = parseResult.root;
        cstNodeMap = std::move(parseResult.cstNodeMap);

        const TestResult initialResult = run();
        if (initialResult == TestResult::NoBug)
        {
            printf("Could not find failure string in the unmodified script!  Check your commandline arguments\n");
            exit(2);
        }

        walk(root);

        writeTempScript(/* minify */ true);

        printf("Done!  Check %s\n", scriptName.c_str());
    }
};

[[noreturn]] void help(const std::vector<std::string_view>& args)
{
    printf("Syntax: %s script command \"search text\"\n", args[0].data());
    printf("    Within command, use {} as a stand-in for the script being reduced\n");
    exit(1);
}

int main(int argc, char** argv)
{
    const std::vector<std::string_view> args(argv, argv + argc);

    if (args.size() != 4)
        help(args);

    for (size_t i = 1; i < args.size(); ++i)
    {
        if (args[i] == "--help")
            help(args);
    }

    std::string scriptName = argv[1];
    std::string appName = argv[2];
    const std::string searchText = argv[3];

    std::optional<std::string> source = readFile(scriptName);

    if (!source)
    {
        printf("Could not read source %s\n", argv[1]);
        exit(1);
    }

    Reducer reducer;
    reducer.run(std::move(scriptName), std::move(appName), *source, searchText);
}
