// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Fixture.h"

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Constraint.h"
#include "Luau/ModuleResolver.h"
#include "Luau/NotNull.h"
#include "Luau/Parser.h"
#include "Luau/Type.h"
#include "Luau/TypeAttach.h"
#include "Luau/TypeInfer.h"
#include "Luau/Transpiler.h"

#include "doctest.h"

#include <algorithm>
#include <sstream>
#include <string_view>
#include <iostream>
#include <fstream>

static const char* mainModuleName = "MainModule";

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAG(DebugLuauFreezeArena);
LUAU_FASTFLAG(DebugLuauLogSolverToJsonFile)

extern std::optional<unsigned> randomSeed; // tests/main.cpp

namespace Luau
{

std::optional<ModuleInfo> TestFileResolver::resolveModuleInfo(const ModuleName& currentModuleName, const AstExpr& pathExpr)
{
    if (auto name = pathExprToModuleName(currentModuleName, pathExpr))
        return {{*name, false}};

    return std::nullopt;
}

const ModulePtr TestFileResolver::getModule(const ModuleName& moduleName) const
{
    LUAU_ASSERT(false);
    return nullptr;
}

bool TestFileResolver::moduleExists(const ModuleName& moduleName) const
{
    auto it = source.find(moduleName);
    return (it != source.end());
}

std::optional<SourceCode> TestFileResolver::readSource(const ModuleName& name)
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

std::optional<ModuleInfo> TestFileResolver::resolveModule(const ModuleInfo* context, AstExpr* expr)
{
    if (AstExprGlobal* g = expr->as<AstExprGlobal>())
    {
        if (g->name == "game")
            return ModuleInfo{"game"};
        if (g->name == "workspace")
            return ModuleInfo{"workspace"};
        if (g->name == "script")
            return context ? std::optional<ModuleInfo>(*context) : std::nullopt;
    }
    else if (AstExprIndexName* i = expr->as<AstExprIndexName>(); i && context)
    {
        if (i->index == "Parent")
        {
            std::string_view view = context->name;
            size_t lastSeparatorIndex = view.find_last_of('/');

            if (lastSeparatorIndex == std::string_view::npos)
                return std::nullopt;

            return ModuleInfo{ModuleName(view.substr(0, lastSeparatorIndex)), context->optional};
        }
        else
        {
            return ModuleInfo{context->name + '/' + i->index.value, context->optional};
        }
    }
    else if (AstExprIndexExpr* i = expr->as<AstExprIndexExpr>(); i && context)
    {
        if (AstExprConstantString* index = i->index->as<AstExprConstantString>())
        {
            return ModuleInfo{context->name + '/' + std::string(index->value.data, index->value.size), context->optional};
        }
    }
    else if (AstExprCall* call = expr->as<AstExprCall>(); call && call->self && call->args.size >= 1 && context)
    {
        if (AstExprConstantString* index = call->args.data[0]->as<AstExprConstantString>())
        {
            AstName func = call->func->as<AstExprIndexName>()->index;

            if (func == "GetService" && context->name == "game")
                return ModuleInfo{"game/" + std::string(index->value.data, index->value.size)};
        }
    }

    return std::nullopt;
}

std::string TestFileResolver::getHumanReadableModuleName(const ModuleName& name) const
{
    // We have a handful of tests that need to distinguish between a canonical
    // ModuleName and the human-readable version so we apply a simple transform
    // here:  We replace all slashes with dots.
    std::string result = name;
    for (size_t i = 0; i < result.size(); ++i)
    {
        if (result[i] == '/')
            result[i] = '.';
    }

    return result;
}

std::optional<std::string> TestFileResolver::getEnvironmentForModule(const ModuleName& name) const
{
    auto it = environments.find(name);
    if (it != environments.end())
        return it->second;

    return std::nullopt;
}

const Config& TestConfigResolver::getConfig(const ModuleName& name) const
{
    auto it = configFiles.find(name);
    if (it != configFiles.end())
        return it->second;

    return defaultConfig;
}

Fixture::Fixture(bool freeze, bool prepareAutocomplete)
    : sff_DebugLuauFreezeArena(FFlag::DebugLuauFreezeArena, freeze)
    , frontend(&fileResolver, &configResolver,
          {/* retainFullTypeGraphs= */ true, /* forAutocomplete */ false, /* runLintChecks */ false, /* randomConstraintResolutionSeed */ randomSeed})
    , builtinTypes(frontend.builtinTypes)
{
    configResolver.defaultConfig.mode = Mode::Strict;
    configResolver.defaultConfig.enabledLint.warningMask = ~0ull;
    configResolver.defaultConfig.parseOptions.captureComments = true;

    Luau::freeze(frontend.globals.globalTypes);
    Luau::freeze(frontend.globalsForAutocomplete.globalTypes);

    Luau::setPrintLine([](auto s) {});

    if (FFlag::DebugLuauLogSolverToJsonFile)
    {
        frontend.writeJsonLog = [&](const Luau::ModuleName& moduleName, std::string log) {
            std::string path = moduleName + ".log.json";
            size_t pos = moduleName.find_last_of('/');
            if (pos != std::string::npos)
                path = moduleName.substr(pos + 1);

            std::ofstream os(path);

            os << log << std::endl;
            MESSAGE("Wrote JSON log to ", path);
        };
    }
}

Fixture::~Fixture()
{
    Luau::resetPrintLine();
}

AstStatBlock* Fixture::parse(const std::string& source, const ParseOptions& parseOptions)
{
    sourceModule.reset(new SourceModule);

    ParseResult result = Parser::parse(source.c_str(), source.length(), *sourceModule->names, *sourceModule->allocator, parseOptions);

    sourceModule->name = fromString(mainModuleName);
    sourceModule->root = result.root;
    sourceModule->mode = parseMode(result.hotcomments);
    sourceModule->hotcomments = std::move(result.hotcomments);

    if (!result.errors.empty())
    {
        // if AST is available, check how lint and typecheck handle error nodes
        if (result.root)
        {
            if (FFlag::DebugLuauDeferredConstraintResolution)
            {
                Mode mode = sourceModule->mode ? *sourceModule->mode : Mode::Strict;
                ModulePtr module = Luau::check(*sourceModule, mode, {}, builtinTypes, NotNull{&ice}, NotNull{&moduleResolver}, NotNull{&fileResolver},
                    frontend.globals.globalScope, /*prepareModuleScope*/ nullptr, frontend.options, {}, false, {});

                Luau::lint(sourceModule->root, *sourceModule->names, frontend.globals.globalScope, module.get(), sourceModule->hotcomments, {});
            }
            else
            {
                TypeChecker typeChecker(frontend.globals.globalScope, &moduleResolver, builtinTypes, &frontend.iceHandler);
                ModulePtr module = typeChecker.check(*sourceModule, sourceModule->mode.value_or(Luau::Mode::Nonstrict), std::nullopt);

                Luau::lint(sourceModule->root, *sourceModule->names, frontend.globals.globalScope, module.get(), sourceModule->hotcomments, {});
            }
        }

        throw ParseErrors(result.errors);
    }

    return result.root;
}

CheckResult Fixture::check(Mode mode, const std::string& source)
{
    ModuleName mm = fromString(mainModuleName);
    configResolver.defaultConfig.mode = mode;
    fileResolver.source[mm] = std::move(source);
    frontend.markDirty(mm);

    CheckResult result = frontend.check(mm);

    return result;
}

CheckResult Fixture::check(const std::string& source)
{
    return check(Mode::Strict, source);
}

LintResult Fixture::lint(const std::string& source, const std::optional<LintOptions>& lintOptions)
{
    ModuleName mm = fromString(mainModuleName);
    configResolver.defaultConfig.mode = Mode::Strict;
    fileResolver.source[mm] = std::move(source);
    frontend.markDirty(mm);

    return lintModule(mm);
}

LintResult Fixture::lintModule(const ModuleName& moduleName, const std::optional<LintOptions>& lintOptions)
{
    FrontendOptions options = frontend.options;
    options.runLintChecks = true;
    options.enabledLintWarnings = lintOptions;

    CheckResult result = frontend.check(moduleName, options);

    return result.lintResult;
}

ParseResult Fixture::parseEx(const std::string& source, const ParseOptions& options)
{
    ParseResult result = tryParse(source, options);
    if (!result.errors.empty())
        throw ParseErrors(result.errors);

    return result;
}

ParseResult Fixture::tryParse(const std::string& source, const ParseOptions& parseOptions)
{
    ParseOptions options = parseOptions;
    options.allowDeclarationSyntax = true;

    sourceModule.reset(new SourceModule);
    ParseResult result = Parser::parse(source.c_str(), source.length(), *sourceModule->names, *sourceModule->allocator, options);
    sourceModule->root = result.root;
    return result;
}

ParseResult Fixture::matchParseError(const std::string& source, const std::string& message, std::optional<Location> location)
{
    ParseOptions options;
    options.allowDeclarationSyntax = true;

    sourceModule.reset(new SourceModule);
    ParseResult result = Parser::parse(source.c_str(), source.length(), *sourceModule->names, *sourceModule->allocator, options);

    CHECK_MESSAGE(!result.errors.empty(), "Expected a parse error in '" << source << "'");

    if (!result.errors.empty())
    {
        CHECK_EQ(result.errors.front().getMessage(), message);

        if (location)
            CHECK_EQ(result.errors.front().getLocation(), *location);
    }

    return result;
}

ParseResult Fixture::matchParseErrorPrefix(const std::string& source, const std::string& prefix)
{
    ParseOptions options;
    options.allowDeclarationSyntax = true;

    sourceModule.reset(new SourceModule);
    ParseResult result = Parser::parse(source.c_str(), source.length(), *sourceModule->names, *sourceModule->allocator, options);

    CHECK_MESSAGE(!result.errors.empty(), "Expected a parse error in '" << source << "'");

    if (!result.errors.empty())
    {
        const std::string& message = result.errors.front().getMessage();
        CHECK_GE(message.length(), prefix.length());
        CHECK_EQ(prefix, message.substr(0, prefix.size()));
    }

    return result;
}

ModulePtr Fixture::getMainModule()
{
    return frontend.moduleResolver.getModule(fromString(mainModuleName));
}

SourceModule* Fixture::getMainSourceModule()
{
    return frontend.getSourceModule(fromString(mainModuleName));
}

std::optional<PrimitiveType::Type> Fixture::getPrimitiveType(TypeId ty)
{
    REQUIRE(ty != nullptr);

    TypeId aType = follow(ty);
    REQUIRE(aType != nullptr);

    const PrimitiveType* pt = get<PrimitiveType>(aType);
    if (pt != nullptr)
        return pt->type;
    else
        return std::nullopt;
}

std::optional<TypeId> Fixture::getType(const std::string& name)
{
    ModulePtr module = getMainModule();
    REQUIRE(module);

    if (!module->hasModuleScope())
        return std::nullopt;

    if (FFlag::DebugLuauDeferredConstraintResolution)
        return linearSearchForBinding(module->getModuleScope().get(), name.c_str());
    else
        return lookupName(module->getModuleScope(), name);
}

TypeId Fixture::requireType(const std::string& name)
{
    std::optional<TypeId> ty = getType(name);
    REQUIRE_MESSAGE(bool(ty), "Unable to requireType \"" << name << "\"");
    return follow(*ty);
}

TypeId Fixture::requireType(const ModuleName& moduleName, const std::string& name)
{
    ModulePtr module = frontend.moduleResolver.getModule(moduleName);
    REQUIRE(module);
    return requireType(module, name);
}

TypeId Fixture::requireType(const ModulePtr& module, const std::string& name)
{
    if (!module->hasModuleScope())
        FAIL("requireType: module scope data is not available");

    return requireType(module->getModuleScope(), name);
}

TypeId Fixture::requireType(const ScopePtr& scope, const std::string& name)
{
    std::optional<TypeId> ty = lookupName(scope, name);
    REQUIRE_MESSAGE(ty, "requireType: No type \"" << name << "\"");
    return *ty;
}

std::optional<TypeId> Fixture::findTypeAtPosition(Position position)
{
    ModulePtr module = getMainModule();
    SourceModule* sourceModule = getMainSourceModule();
    return Luau::findTypeAtPosition(*module, *sourceModule, position);
}

std::optional<TypeId> Fixture::findExpectedTypeAtPosition(Position position)
{
    ModulePtr module = getMainModule();
    SourceModule* sourceModule = getMainSourceModule();
    return Luau::findExpectedTypeAtPosition(*module, *sourceModule, position);
}

TypeId Fixture::requireTypeAtPosition(Position position)
{
    auto ty = findTypeAtPosition(position);
    REQUIRE_MESSAGE(ty, "requireTypeAtPosition: No type at position " << position);
    return *ty;
}

std::optional<TypeId> Fixture::lookupType(const std::string& name)
{
    ModulePtr module = getMainModule();

    if (!module->hasModuleScope())
        return std::nullopt;

    if (auto typeFun = module->getModuleScope()->lookupType(name))
        return typeFun->type;

    return std::nullopt;
}

std::optional<TypeId> Fixture::lookupImportedType(const std::string& moduleAlias, const std::string& name)
{
    ModulePtr module = getMainModule();

    if (!module->hasModuleScope())
        FAIL("lookupImportedType: module scope data is not available");

    if (auto typeFun = module->getModuleScope()->lookupImportedType(moduleAlias, name))
        return typeFun->type;

    return std::nullopt;
}

TypeId Fixture::requireTypeAlias(const std::string& name)
{
    std::optional<TypeId> ty = lookupType(name);
    REQUIRE(ty);
    return follow(*ty);
}

TypeId Fixture::requireExportedType(const ModuleName& moduleName, const std::string& name)
{
    ModulePtr module = frontend.moduleResolver.getModule(moduleName);
    REQUIRE(module);

    auto it = module->exportedTypeBindings.find(name);
    REQUIRE(it != module->exportedTypeBindings.end());

    return it->second.type;
}

std::string Fixture::decorateWithTypes(const std::string& code)
{
    fileResolver.source[mainModuleName] = code;

    Luau::CheckResult typeInfo = frontend.check(mainModuleName);

    SourceModule* sourceModule = frontend.getSourceModule(mainModuleName);
    attachTypeData(*sourceModule, *frontend.moduleResolver.getModule(mainModuleName));

    return transpileWithTypes(*sourceModule->root);
}

void Fixture::dumpErrors(std::ostream& os, const std::vector<TypeError>& errors)
{
    for (const auto& error : errors)
    {
        os << std::endl;
        os << "Error: " << error << std::endl;

        std::string_view source = fileResolver.source[error.moduleName];
        std::vector<std::string_view> lines = Luau::split(source, '\n');

        if (error.location.begin.line >= lines.size())
        {
            os << "\tSource not available?" << std::endl;
            continue;
        }

        std::string_view theLine = lines[error.location.begin.line];
        os << "Line:\t" << theLine << std::endl;
        int startCol = error.location.begin.column;
        int endCol = error.location.end.line == error.location.begin.line ? error.location.end.column : int(theLine.size());

        os << '\t' << std::string(startCol, ' ') << std::string(std::max(1, endCol - startCol), '-') << std::endl;
    }
}

void Fixture::registerTestTypes()
{
    addGlobalBinding(frontend.globals, "game", builtinTypes->anyType, "@luau");
    addGlobalBinding(frontend.globals, "workspace", builtinTypes->anyType, "@luau");
    addGlobalBinding(frontend.globals, "script", builtinTypes->anyType, "@luau");
}

void Fixture::dumpErrors(const CheckResult& cr)
{
    std::string error = getErrors(cr);
    if (!error.empty())
        MESSAGE(error);
}

void Fixture::dumpErrors(const ModulePtr& module)
{
    std::stringstream ss;
    dumpErrors(ss, module->errors);
    if (!ss.str().empty())
        MESSAGE(ss.str());
}

void Fixture::dumpErrors(const Module& module)
{
    std::stringstream ss;
    dumpErrors(ss, module.errors);
    if (!ss.str().empty())
        MESSAGE(ss.str());
}

std::string Fixture::getErrors(const CheckResult& cr)
{
    std::stringstream ss;
    dumpErrors(ss, cr.errors);
    return ss.str();
}

void Fixture::validateErrors(const std::vector<Luau::TypeError>& errors)
{
    std::ostringstream oss;

    // This helps us validate that error stringification doesn't crash, using both user-facing and internal test-only representation
    // Also we exercise error comparison to make sure it's at least able to compare the error equal to itself
    for (const Luau::TypeError& e : errors)
    {
        oss.clear();
        oss << e;
        toString(e);
        // CHECK(e == e); TODO: this doesn't work due to union/intersection type vars
    }
}

LoadDefinitionFileResult Fixture::loadDefinition(const std::string& source)
{
    unfreeze(frontend.globals.globalTypes);
    LoadDefinitionFileResult result =
        frontend.loadDefinitionFile(frontend.globals, frontend.globals.globalScope, source, "@test", /* captureComments */ false);
    freeze(frontend.globals.globalTypes);

    if (result.module)
        dumpErrors(result.module);
    REQUIRE_MESSAGE(result.success, "loadDefinition: unable to load definition file");
    return result;
}

BuiltinsFixture::BuiltinsFixture(bool freeze, bool prepareAutocomplete)
    : Fixture(freeze, prepareAutocomplete)
{
    Luau::unfreeze(frontend.globals.globalTypes);
    Luau::unfreeze(frontend.globalsForAutocomplete.globalTypes);

    registerBuiltinGlobals(frontend, frontend.globals);
    if (prepareAutocomplete)
        registerBuiltinGlobals(frontend, frontend.globalsForAutocomplete, /*typeCheckForAutocomplete*/ true);
    registerTestTypes();

    Luau::freeze(frontend.globals.globalTypes);
    Luau::freeze(frontend.globalsForAutocomplete.globalTypes);
}

ModuleName fromString(std::string_view name)
{
    return ModuleName(name);
}

std::string rep(const std::string& s, size_t n)
{
    std::string r;
    r.reserve(s.length() * n);
    for (size_t i = 0; i < n; ++i)
        r += s;
    return r;
}

bool isInArena(TypeId t, const TypeArena& arena)
{
    return arena.types.contains(t);
}

void dumpErrors(const ModulePtr& module)
{
    for (const auto& error : module->errors)
        std::cout << "Error: " << error << std::endl;
}

void dump(const std::string& name, TypeId ty)
{
    std::cout << name << '\t' << toString(ty, {true}) << std::endl;
}

std::optional<TypeId> lookupName(ScopePtr scope, const std::string& name)
{
    auto binding = scope->linearSearchForBinding(name);
    if (binding)
        return binding->typeId;
    else
        return std::nullopt;
}

std::optional<TypeId> linearSearchForBinding(Scope* scope, const char* name)
{
    while (scope)
    {
        for (const auto& [n, ty] : scope->bindings)
        {
            if (n.astName() == name)
                return ty.typeId;
        }

        scope = scope->parent.get();
    }

    return std::nullopt;
}

void registerHiddenTypes(Frontend* frontend)
{
    GlobalTypes& globals = frontend->globals;

    unfreeze(globals.globalTypes);

    TypeId t = globals.globalTypes.addType(GenericType{"T"});
    GenericTypeDefinition genericT{t};

    TypeId u = globals.globalTypes.addType(GenericType{"U"});
    GenericTypeDefinition genericU{u};

    ScopePtr globalScope = globals.globalScope;
    globalScope->exportedTypeBindings["Not"] = TypeFun{{genericT}, globals.globalTypes.addType(NegationType{t})};
    globalScope->exportedTypeBindings["Mt"] = TypeFun{{genericT, genericU}, globals.globalTypes.addType(MetatableType{t, u})};
    globalScope->exportedTypeBindings["fun"] = TypeFun{{}, frontend->builtinTypes->functionType};
    globalScope->exportedTypeBindings["cls"] = TypeFun{{}, frontend->builtinTypes->classType};
    globalScope->exportedTypeBindings["err"] = TypeFun{{}, frontend->builtinTypes->errorType};
    globalScope->exportedTypeBindings["tbl"] = TypeFun{{}, frontend->builtinTypes->tableType};

    freeze(globals.globalTypes);
}

void createSomeClasses(Frontend* frontend)
{
    GlobalTypes& globals = frontend->globals;

    TypeArena& arena = globals.globalTypes;
    unfreeze(arena);

    ScopePtr moduleScope = globals.globalScope;

    TypeId parentType = arena.addType(ClassType{"Parent", {}, frontend->builtinTypes->classType, std::nullopt, {}, nullptr, "Test", {}});

    ClassType* parentClass = getMutable<ClassType>(parentType);
    parentClass->props["method"] = {makeFunction(arena, parentType, {}, {})};

    parentClass->props["virtual_method"] = {makeFunction(arena, parentType, {}, {})};

    addGlobalBinding(globals, "Parent", {parentType});
    moduleScope->exportedTypeBindings["Parent"] = TypeFun{{}, parentType};

    TypeId childType = arena.addType(ClassType{"Child", {}, parentType, std::nullopt, {}, nullptr, "Test", {}});

    addGlobalBinding(globals, "Child", {childType});
    moduleScope->exportedTypeBindings["Child"] = TypeFun{{}, childType};

    TypeId anotherChildType = arena.addType(ClassType{"AnotherChild", {}, parentType, std::nullopt, {}, nullptr, "Test", {}});

    addGlobalBinding(globals, "AnotherChild", {anotherChildType});
    moduleScope->exportedTypeBindings["AnotherChild"] = TypeFun{{}, anotherChildType};

    TypeId unrelatedType = arena.addType(ClassType{"Unrelated", {}, frontend->builtinTypes->classType, std::nullopt, {}, nullptr, "Test", {}});

    addGlobalBinding(globals, "Unrelated", {unrelatedType});
    moduleScope->exportedTypeBindings["Unrelated"] = TypeFun{{}, unrelatedType};

    for (const auto& [name, ty] : moduleScope->exportedTypeBindings)
        persist(ty.type);

    freeze(arena);
}

void dump(const std::vector<Constraint>& constraints)
{
    ToStringOptions opts;
    for (const auto& c : constraints)
        printf("%s\n", toString(c, opts).c_str());
}

} // namespace Luau
