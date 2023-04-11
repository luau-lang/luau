// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Config.h"
#include "Luau/FileResolver.h"
#include "Luau/Frontend.h"
#include "Luau/IostreamHelpers.h"
#include "Luau/Linter.h"
#include "Luau/Location.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"

#include "IostreamOptional.h"
#include "ScopedFlags.h"

#include <string>
#include <unordered_map>
#include <optional>

namespace Luau
{

struct TypeChecker;

struct TestFileResolver
    : FileResolver
    , ModuleResolver
{
    std::optional<ModuleInfo> resolveModuleInfo(const ModuleName& currentModuleName, const AstExpr& pathExpr) override;

    const ModulePtr getModule(const ModuleName& moduleName) const override;

    bool moduleExists(const ModuleName& moduleName) const override;

    std::optional<SourceCode> readSource(const ModuleName& name) override;

    std::optional<ModuleInfo> resolveModule(const ModuleInfo* context, AstExpr* expr) override;

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

    const Config& getConfig(const ModuleName& name) const override;
};

struct Fixture
{
    explicit Fixture(bool freeze = true, bool prepareAutocomplete = false);
    ~Fixture();

    // Throws Luau::ParseErrors if the parse fails.
    AstStatBlock* parse(const std::string& source, const ParseOptions& parseOptions = {});
    CheckResult check(Mode mode, std::string source);
    CheckResult check(const std::string& source);

    LintResult lint(const std::string& source, const std::optional<LintOptions>& lintOptions = {});
    LintResult lintModule(const ModuleName& moduleName, const std::optional<LintOptions>& lintOptions = {});

    /// Parse with all language extensions enabled
    ParseResult parseEx(const std::string& source, const ParseOptions& parseOptions = {});
    ParseResult tryParse(const std::string& source, const ParseOptions& parseOptions = {});
    ParseResult matchParseError(const std::string& source, const std::string& message, std::optional<Location> location = std::nullopt);
    // Verify a parse error occurs and the parse error message has the specified prefix
    ParseResult matchParseErrorPrefix(const std::string& source, const std::string& prefix);

    ModulePtr getMainModule();
    SourceModule* getMainSourceModule();

    std::optional<PrimitiveType::Type> getPrimitiveType(TypeId ty);
    std::optional<TypeId> getType(const std::string& name);
    TypeId requireType(const std::string& name);
    TypeId requireType(const ModuleName& moduleName, const std::string& name);
    TypeId requireType(const ModulePtr& module, const std::string& name);
    TypeId requireType(const ScopePtr& scope, const std::string& name);

    std::optional<TypeId> findTypeAtPosition(Position position);
    TypeId requireTypeAtPosition(Position position);
    std::optional<TypeId> findExpectedTypeAtPosition(Position position);

    std::optional<TypeId> lookupType(const std::string& name);
    std::optional<TypeId> lookupImportedType(const std::string& moduleAlias, const std::string& name);
    TypeId requireTypeAlias(const std::string& name);

    ScopedFastFlag sff_DebugLuauFreezeArena;

    TestFileResolver fileResolver;
    TestConfigResolver configResolver;
    NullModuleResolver moduleResolver;
    std::unique_ptr<SourceModule> sourceModule;
    Frontend frontend;
    InternalErrorReporter ice;
    NotNull<BuiltinTypes> builtinTypes;

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

struct BuiltinsFixture : Fixture
{
    BuiltinsFixture(bool freeze = true, bool prepareAutocomplete = false);
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
void dump(const std::vector<Constraint>& constraints);

std::optional<TypeId> lookupName(ScopePtr scope, const std::string& name); // Warning: This function runs in O(n**2)

std::optional<TypeId> linearSearchForBinding(Scope* scope, const char* name);

void registerHiddenTypes(Frontend* frontend);
void createSomeClasses(Frontend* frontend);

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
