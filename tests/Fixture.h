// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/BuiltinTypeFunctions.h"
#include "Luau/Config.h"
#include "Luau/EqSatSimplification.h"
#include "Luau/Error.h"
#include "Luau/FileResolver.h"
#include "Luau/Frontend.h"
#include "Luau/IostreamHelpers.h"
#include "Luau/Linter.h"
#include "Luau/Location.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeFunction.h"

#include "IostreamOptional.h"
#include "ScopedFlags.h"

#include "doctest.h"
#include <string>
#include <string_view>
#include <unordered_map>
#include <optional>
#include <vector>

LUAU_FASTFLAG(DebugLuauFreezeArena)
LUAU_FASTFLAG(DebugLuauForceAllNewSolverTests)

LUAU_FASTFLAG(LuauTidyTypeUtils)
LUAU_FASTFLAG(DebugLuauAlwaysShowConstraintSolvingIncomplete);

#define DOES_NOT_PASS_NEW_SOLVER_GUARD_IMPL(line) ScopedFastFlag sff_##line{FFlag::LuauSolverV2, FFlag::DebugLuauForceAllNewSolverTests};

#define DOES_NOT_PASS_NEW_SOLVER_GUARD() DOES_NOT_PASS_NEW_SOLVER_GUARD_IMPL(__LINE__)

namespace Luau
{

struct TypeChecker;

struct TestRequireNode : RequireNode
{
    TestRequireNode(ModuleName moduleName, std::unordered_map<ModuleName, std::string>* allSources)
        : moduleName(std::move(moduleName))
        , allSources(allSources)
    {
    }

    std::string getLabel() const override;
    std::string getPathComponent() const override;
    std::unique_ptr<RequireNode> resolvePathToNode(const std::string& path) const override;
    std::vector<std::unique_ptr<RequireNode>> getChildren() const override;
    std::vector<RequireAlias> getAvailableAliases() const override;

    ModuleName moduleName;
    std::unordered_map<ModuleName, std::string>* allSources;
};

struct TestFileResolver;
struct TestRequireSuggester : RequireSuggester
{
    TestRequireSuggester(TestFileResolver* resolver)
        : resolver(resolver)
    {
    }

    std::unique_ptr<RequireNode> getNode(const ModuleName& name) const override;
    TestFileResolver* resolver;
};

struct TestFileResolver
    : FileResolver
    , ModuleResolver
{
    TestFileResolver()
        : FileResolver(std::make_shared<TestRequireSuggester>(this))
    {
    }

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
    explicit Fixture(bool prepareAutocomplete = false);
    ~Fixture();

    // Throws Luau::ParseErrors if the parse fails.
    AstStatBlock* parse(const std::string& source, const ParseOptions& parseOptions = {});
    CheckResult check(Mode mode, const std::string& source, std::optional<FrontendOptions> = std::nullopt);
    CheckResult check(const std::string& source, std::optional<FrontendOptions> = std::nullopt);

    LintResult lint(const std::string& source, const std::optional<LintOptions>& lintOptions = {});
    LintResult lintModule(const ModuleName& moduleName, const std::optional<LintOptions>& lintOptions = {});

    /// Parse with all language extensions enabled
    ParseResult parseEx(const std::string& source, const ParseOptions& parseOptions = {});
    ParseResult tryParse(const std::string& source, const ParseOptions& parseOptions = {});
    ParseResult matchParseError(const std::string& source, const std::string& message, std::optional<Location> location = std::nullopt);
    // Verify a parse error occurs and the parse error message has the specified prefix
    ParseResult matchParseErrorPrefix(const std::string& source, const std::string& prefix);

    ModulePtr getMainModule(bool forAutocomplete = false);
    SourceModule* getMainSourceModule();

    std::optional<PrimitiveType::Type> getPrimitiveType(TypeId ty);
    std::optional<TypeId> getType(const std::string& name, bool forAutocomplete = false);
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
    TypeId requireExportedType(const ModuleName& moduleName, const std::string& name);

    std::string canonicalize(TypeId ty);

    // While most flags can be flipped inside the unit test, some code changes affect the state that is part of Fixture initialization
    // Most often those are changes related to builtin type definitions.
    // In that case, flag can be forced to 'true' using the example below:
    // ScopedFastFlag sff_LuauExampleFlagDefinition{FFlag::LuauExampleFlagDefinition, true};

    ScopedFastFlag sff_TypeUtilTidy{FFlag::LuauTidyTypeUtils, true};

    // Arena freezing marks the `TypeArena`'s underlying memory as read-only, raising an access violation whenever you mutate it.
    // This is useful for tracking down violations of Luau's memory model.
    ScopedFastFlag sff_DebugLuauFreezeArena{FFlag::DebugLuauFreezeArena, true};

    // This makes sure that errant cases of constraint solving failing to complete still pop up in tests.
    ScopedFastFlag sff_DebugLuauAlwaysShowConstraintSolvingIncomplete{FFlag::DebugLuauAlwaysShowConstraintSolvingIncomplete, true};

    TestFileResolver fileResolver;
    TestConfigResolver configResolver;
    NullModuleResolver moduleResolver;
    std::unique_ptr<SourceModule> sourceModule;
    InternalErrorReporter ice;


    std::string decorateWithTypes(const std::string& code);

    void dumpErrors(std::ostream& os, const std::vector<TypeError>& errors);

    void dumpErrors(const CheckResult& cr);
    void dumpErrors(const ModulePtr& module);
    void dumpErrors(const Module& module);

    void validateErrors(const std::vector<TypeError>& errors);

    std::string getErrors(const CheckResult& cr);

    void registerTestTypes();

    LoadDefinitionFileResult loadDefinition(const std::string& source, bool forAutocomplete = false);
    // TODO: test theory about dynamic dispatch
    NotNull<BuiltinTypes> getBuiltins();
    const BuiltinTypeFunctions& getBuiltinTypeFunctions();
    virtual Frontend& getFrontend();

private:
    bool hasDumpedErrors = false;

protected:
    bool forAutocomplete = false;
    std::optional<Frontend> frontend;
    BuiltinTypes* builtinTypes = nullptr;

    TypeArena simplifierArena;
    SimplifierPtr simplifier{nullptr, nullptr};
};

struct BuiltinsFixture : Fixture
{
    explicit BuiltinsFixture(bool prepareAutocomplete = false);

    // For the purpose of our tests, we're always the latest version of type functions.
    Frontend& getFrontend() override;
};

std::optional<std::string> pathExprToModuleName(const ModuleName& currentModuleName, const std::vector<std::string_view>& segments);
std::optional<std::string> pathExprToModuleName(const ModuleName& currentModuleName, const AstExpr& pathExpr);

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

void registerHiddenTypes(Frontend& frontend);
void createSomeExternTypes(Frontend& frontend);

template<typename E>
const E* findError(const CheckResult& result)
{
    for (const auto& e : result.errors)
    {
        if (auto p = get<E>(e))
            return p;
    }

    return nullptr;
}

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

#define LUAU_CHECK_ERRORS(result) \
    do \
    { \
        auto&& r = (result); \
        validateErrors(r.errors); \
        CHECK(!r.errors.empty()); \
    } while (false)

#define LUAU_CHECK_ERROR_COUNT(count, result) \
    do \
    { \
        auto&& r = (result); \
        validateErrors(r.errors); \
        CHECK_MESSAGE(count == r.errors.size(), getErrors(r)); \
    } while (false)

#define LUAU_CHECK_NO_ERRORS(result) LUAU_CHECK_ERROR_COUNT(0, result)

#define LUAU_CHECK_HAS_KEY(map, key) \
    do \
    { \
        auto&& _m = (map); \
        auto&& _k = (key); \
        const size_t count = _m.count(_k); \
        CHECK_MESSAGE(count, "Map should have key \"" << _k << "\""); \
        if (!count) \
        { \
            MESSAGE("Keys: (count " << _m.size() << ")"); \
            for (const auto& [k, v] : _m) \
            { \
                MESSAGE("\tkey: " << k); \
            } \
        } \
    } while (false)

#define LUAU_CHECK_HAS_NO_KEY(map, key) \
    do \
    { \
        auto&& _m = (map); \
        auto&& _k = (key); \
        const size_t count = _m.count(_k); \
        CHECK_MESSAGE(!count, "Map should not have key \"" << _k << "\""); \
        if (count) \
        { \
            MESSAGE("Keys: (count " << _m.size() << ")"); \
            for (const auto& [k, v] : _m) \
            { \
                MESSAGE("\tkey: " << k); \
            } \
        } \
    } while (false)

#define LUAU_REQUIRE_ERROR(result, Type) \
    do \
    { \
        using T = Type; \
        const auto& res = (result); \
        if (!findError<T>(res)) \
        { \
            dumpErrors(res); \
            REQUIRE_MESSAGE(false, "Expected to find " #Type " error"); \
        } \
    } while (false)

#define LUAU_CHECK_ERROR(result, Type) \
    do \
    { \
        using T = Type; \
        const auto& res = (result); \
        if (!findError<T>(res)) \
        { \
            dumpErrors(res); \
            CHECK_MESSAGE(false, "Expected to find " #Type " error"); \
        } \
    } while (false)

#define LUAU_REQUIRE_NO_ERROR(result, Type) \
    do \
    { \
        using T = Type; \
        const auto& res = (result); \
        if (findError<T>(res)) \
        { \
            dumpErrors(res); \
            REQUIRE_MESSAGE(false, "Expected to find no " #Type " error"); \
        } \
    } while (false)

#define LUAU_CHECK_NO_ERROR(result, Type) \
    do \
    { \
        using T = Type; \
        const auto& res = (result); \
        if (findError<T>(res)) \
        { \
            dumpErrors(res); \
            CHECK_MESSAGE(false, "Expected to find no " #Type " error"); \
        } \
    } while (false)
