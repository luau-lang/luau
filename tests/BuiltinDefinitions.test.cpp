// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("BuiltinDefinitionsTest");

TEST_CASE_FIXTURE(BuiltinsFixture, "lib_documentation_symbols")
{
    CHECK(!getFrontend().globals.globalScope->bindings.empty());

    for (const auto& [name, binding] : getFrontend().globals.globalScope->bindings)
    {
        std::string nameString(name.c_str());
        std::string expectedRootSymbol = "@luau/global/" + nameString;
        std::optional<std::string> actualRootSymbol = binding.documentationSymbol;
        CHECK_MESSAGE(
            actualRootSymbol == expectedRootSymbol, "expected symbol ", expectedRootSymbol, " for global ", nameString, ", got ", actualRootSymbol
        );

        const TableType::Props* props = nullptr;
        if (const TableType* ttv = get<TableType>(binding.typeId))
        {
            props = &ttv->props;
        }
        else if (const ExternType* etv = get<ExternType>(binding.typeId))
        {
            props = &etv->props;
        }

        if (props)
        {
            for (const auto& [propName, prop] : *props)
            {
                std::string fullPropName = nameString + "." + propName;
                std::string expectedPropSymbol = expectedRootSymbol + "." + propName;
                std::optional<std::string> actualPropSymbol = prop.documentationSymbol;
                CHECK_MESSAGE(
                    actualPropSymbol == expectedPropSymbol, "expected symbol ", expectedPropSymbol, " for ", fullPropName, ", got ", actualPropSymbol
                );
            }
        }
    }
}
