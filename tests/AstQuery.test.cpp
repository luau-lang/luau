// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Fixture.h"

#include "Luau/AstQuery.h"

#include "doctest.h"

using namespace Luau;

struct DocumentationSymbolFixture : Fixture
{
    std::optional<DocumentationSymbol> getDocSymbol(const std::string& source, Position position)
    {
        check(source);

        SourceModule* sourceModule = getMainSourceModule();
        ModulePtr module = getMainModule();

        return getDocumentationSymbolAtPosition(*sourceModule, *module, position);
    }
};

TEST_SUITE_BEGIN("AstQuery::getDocumentationSymbolAtPosition");

TEST_CASE_FIXTURE(DocumentationSymbolFixture, "binding")
{
    std::optional<DocumentationSymbol> global = getDocSymbol(R"(
        local a = string.sub()
    )",
        Position(1, 21));

    CHECK_EQ(global, "@luau/global/string");
}

TEST_CASE_FIXTURE(DocumentationSymbolFixture, "prop")
{
    std::optional<DocumentationSymbol> substring = getDocSymbol(R"(
        local a = string.sub()
    )",
        Position(1, 27));

    CHECK_EQ(substring, "@luau/global/string.sub");
}

TEST_CASE_FIXTURE(DocumentationSymbolFixture, "event_callback_arg")
{
    ScopedFastFlag sffs[] = {
        {"LuauPersistDefinitionFileTypes", true},
    };

    loadDefinition(R"(
        declare function Connect(fn: (string) -> ())
    )");

    std::optional<DocumentationSymbol> substring = getDocSymbol(R"(
        Connect(function(abc)
        end)
    )",
        Position(1, 27));

    CHECK_EQ(substring, "@test/global/Connect/param/0/param/0");
}

TEST_CASE_FIXTURE(DocumentationSymbolFixture, "overloaded_fn")
{
    ScopedFastFlag sffs{"LuauStoreMatchingOverloadFnType", true};

    loadDefinition(R"(
        declare foo: ((string) -> number) & ((number) -> string)
    )");

    std::optional<DocumentationSymbol> symbol = getDocSymbol(R"(
        foo("asdf")
    )",
        Position(1, 10));

    CHECK_EQ(symbol, "@test/global/foo/overload/(string) -> number");
}

TEST_SUITE_END();
