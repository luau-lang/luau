// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Config.h"
#include "Luau/FileResolver.h"
#include "Luau/Frontend.h"
#include "Luau/IostreamHelpers.h"
#include "Luau/Linter.h"
#include "Luau/Location.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Parser.h"
#include "Luau/ToString.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"

#include "IostreamOptional.h"
#include "ScopedFlags.h"

#include <iostream>
#include <string>
#include <unordered_map>

#include <optional>

namespace Luau
{

struct TestFileResolver
    : FileResolver
    , ModuleResolver
{
    std::optional<ModuleInfo> resolveModuleInfo(const ModuleName& currentModuleName, const AstExpr& pathExpr) override
    {
        if (auto name = pathExprToModuleName(currentModuleName, pathExpr))
            return {{*name, false}};

        return std::nullopt;
    }

    const ModulePtr getModule(const ModuleName& moduleName) const override
    {
        LUAU_ASSERT(false);
        return nullptr;
    }

    bool moduleExists(const ModuleName& moduleName) const override
    {
        auto it = source.find(moduleName);
        return (it != source.end());
    }

    std::optional<SourceCode> readSource(const ModuleName& name) override
    {
        auto it = source.find(name);
        if (it == source.end())
            return std::nullopt;

        SourceCode::Type sourceType = SourceCode::Module;

        auto it2 = sourceTypes.find(name);
        if (it2 != sourceTypes.end())
            sourceType = it2->second;

        return SourceCode{it->second, sourceType};
    }

    std::optional<ModuleName> fromAstFragment(AstExpr* expr) const override;
    ModuleName concat(const ModuleName& lhs, std::string_view rhs) const override;
    std::optional<ModuleName> getParentModuleName(const ModuleName& name) const override;

    std::string getHumanReadableModuleName(const ModuleName& name) const override;

    std::optional<std::string> getEnvironmentForModule(const ModuleName& name) const override;

    std::unordered_map<ModuleName, std::string> source;
    std::unordered_map<ModuleName, SourceCode::Type> sourceTypes;
    std::unordered_map<ModuleName, std::string> environments;
};

struct TestConfigResolver : ConfigResolver
{
    Config defaultConfig;
    std::unordered_map<ModuleName, Config> configFiles;

    const Config& getConfig(const ModuleName& name) const override
    {
        auto it = configFiles.find(name);
        if (it != configFiles.end())
            return it->second;

        return defaultConfig;
    }
};

struct Fixture
{
    explicit Fixture(bool freeze = true);
    ~Fixture();

    // Throws Luau::ParseErrors if the parse fails.
    AstStatBlock* parse(const std::string& source, const ParseOptions& parseOptions = {});
    CheckResult check(Mode mode, std::string source);
    CheckResult check(const std::string& source);

    LintResult lint(const std::string& source, const std::optional<LintOptions>& lintOptions = {});
    LintResult lintTyped(const std::string& source, const std::optional<LintOptions>& lintOptions = {});

    /// Parse with all language extensions enabled
    ParseResult parseEx(const std::string& source, const ParseOptions& parseOptions = {});
    ParseResult tryParse(const std::string& source, const ParseOptions& parseOptions = {});
    ParseResult matchParseError(const std::string& source, const std::string& message);
    // Verify a parse error occurs and the parse error message has the specified prefix
    ParseResult matchParseErrorPrefix(const std::string& source, const std::string& prefix);

    ModulePtr getMainModule();
    SourceModule* getMainSourceModule();

    std::optional<PrimitiveTypeVar::Type> getPrimitiveType(TypeId ty);
    std::optional<TypeId> getType(const std::string& name);
    TypeId requireType(const std::string& name);
    TypeId requireType(const ModuleName& moduleName, const std::string& name);
    TypeId requireType(const ModulePtr& module, const std::string& name);
    TypeId requireType(const ScopePtr& scope, const std::string& name);

    std::optional<TypeId> findTypeAtPosition(Position position);
    TypeId requireTypeAtPosition(Position position);

    std::optional<TypeId> lookupType(const std::string& name);
    std::optional<TypeId> lookupImportedType(const std::string& moduleAlias, const std::string& name);

    ScopedFastFlag sff_DebugLuauFreezeArena;

    TestFileResolver fileResolver;
    TestConfigResolver configResolver;
    std::unique_ptr<SourceModule> sourceModule;
    Frontend frontend;
    TypeChecker& typeChecker;

    std::string decorateWithTypes(const std::string& code);

    void dumpErrors(std::ostream& os, const std::vector<TypeError>& errors);

    void dumpErrors(const CheckResult& cr);
    void dumpErrors(const ModulePtr& module);
    void dumpErrors(const Module& module);

    void validateErrors(const std::vector<TypeError>& errors);

    std::string getErrors(const CheckResult& cr);

    void registerTestTypes();

    LoadDefinitionFileResult loadDefinition(const std::string& source);
};

// Disables arena freezing for a given test case.
// Do not use this in new tests. If you are running into access violations, you
// are violating Luau's memory model - the fix is not to use UnfrozenFixture.
// Related: CLI-45692
struct UnfrozenFixture : Fixture
{
    UnfrozenFixture();
};

ModuleName fromString(std::string_view name);

template<typename T>
std::optional<T> get(const std::map<Name, T>& map, const Name& name)
{
    auto it = map.find(name);
    if (it != map.end())
        return std::optional<T>(it->second);
    else
        return std::nullopt;
}

std::string rep(const std::string& s, size_t n);

bool isInArena(TypeId t, const TypeArena& arena);

void dumpErrors(const ModulePtr& module);
void dumpErrors(const Module& module);
void dump(const std::string& name, TypeId ty);

std::optional<TypeId> lookupName(ScopePtr scope, const std::string& name); // Warning: This function runs in O(n**2)

} // namespace Luau

#define LUAU_REQUIRE_ERRORS(result) \
    do \
    { \
        auto&& r = (result); \
        validateErrors(r.errors); \
        REQUIRE(!r.errors.empty()); \
    } while (false)

#define LUAU_REQUIRE_ERROR_COUNT(count, result) \
    do \
    { \
        auto&& r = (result); \
        validateErrors(r.errors); \
        REQUIRE_MESSAGE(count == r.errors.size(), getErrors(r)); \
    } while (false)

#define LUAU_REQUIRE_NO_ERRORS(result) LUAU_REQUIRE_ERROR_COUNT(0, result)
