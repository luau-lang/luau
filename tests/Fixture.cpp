// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Fixture.h"

#include "Luau/AstQuery.h"
#include "Luau/TypeVar.h"
#include "Luau/TypeAttach.h"
#include "Luau/Transpiler.h"

#include "Luau/BuiltinDefinitions.h"

#include "doctest.h"

#include <algorithm>
#include <sstream>
#include <string_view>

static const char* mainModuleName = "MainModule";

namespace Luau
{

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
    return name;
}

std::optional<std::string> TestFileResolver::getEnvironmentForModule(const ModuleName& name) const
{
    auto it = environments.find(name);
    if (it != environments.end())
        return it->second;

    return std::nullopt;
}

Fixture::Fixture(bool freeze)
    : sff_DebugLuauFreezeArena("DebugLuauFreezeArena", freeze)
    , frontend(&fileResolver, &configResolver, {/* retainFullTypeGraphs= */ true})
    , typeChecker(frontend.typeChecker)
{
    configResolver.defaultConfig.mode = Mode::Strict;
    configResolver.defaultConfig.enabledLint.warningMask = ~0ull;
    configResolver.defaultConfig.parseOptions.captureComments = true;

    registerBuiltinTypes(frontend.typeChecker);
    registerTestTypes();
    Luau::freeze(frontend.typeChecker.globalTypes);

    Luau::setPrintLine([](auto s) {});
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
    sourceModule->ignoreLints = LintWarning::parseMask(result.hotcomments);

    if (!result.errors.empty())
    {
        // if AST is available, check how lint and typecheck handle error nodes
        if (result.root)
        {
            frontend.lint(*sourceModule);

            typeChecker.check(*sourceModule, sourceModule->mode.value_or(Luau::Mode::Nonstrict));
        }

        throw ParseErrors(result.errors);
    }

    return result.root;
}

CheckResult Fixture::check(Mode mode, std::string source)
{
    configResolver.defaultConfig.mode = mode;
    fileResolver.source[mainModuleName] = std::move(source);

    CheckResult result = frontend.check(fromString(mainModuleName));

    configResolver.defaultConfig.mode = Mode::Strict;

    return result;
}

CheckResult Fixture::check(const std::string& source)
{
    ModuleName mm = fromString(mainModuleName);
    configResolver.defaultConfig.mode = Mode::Strict;
    fileResolver.source[mm] = std::move(source);
    frontend.markDirty(mm);

    CheckResult result = frontend.check(mm);

    return result;
}

LintResult Fixture::lint(const std::string& source, const std::optional<LintOptions>& lintOptions)
{
    ParseOptions parseOptions;
    configResolver.defaultConfig.mode = Mode::Nonstrict;
    parse(source, parseOptions);

    return frontend.lint(*sourceModule, lintOptions);
}

LintResult Fixture::lintTyped(const std::string& source, const std::optional<LintOptions>& lintOptions)
{
    check(source);
    ModuleName mm = fromString(mainModuleName);

    return frontend.lint(mm, lintOptions);
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

    REQUIRE_MESSAGE(!result.errors.empty(), "Expected a parse error in '" << source << "'");

    CHECK_EQ(result.errors.front().getMessage(), message);

    if (location)
        CHECK_EQ(result.errors.front().getLocation(), *location);

    return result;
}

ParseResult Fixture::matchParseErrorPrefix(const std::string& source, const std::string& prefix)
{
    ParseOptions options;
    options.allowDeclarationSyntax = true;

    sourceModule.reset(new SourceModule);
    ParseResult result = Parser::parse(source.c_str(), source.length(), *sourceModule->names, *sourceModule->allocator, options);

    REQUIRE_MESSAGE(!result.errors.empty(), "Expected a parse error in '" << source << "'");

    const std::string& message = result.errors.front().getMessage();
    CHECK_GE(message.length(), prefix.length());
    CHECK_EQ(prefix, message.substr(0, prefix.size()));

    return result;
}

ModulePtr Fixture::getMainModule()
{
    return frontend.moduleResolver.getModule(fromString(mainModuleName));
}

SourceModule* Fixture::getMainSourceModule()
{
    return frontend.getSourceModule(fromString("MainModule"));
}

std::optional<PrimitiveTypeVar::Type> Fixture::getPrimitiveType(TypeId ty)
{
    REQUIRE(ty != nullptr);

    TypeId aType = follow(ty);
    REQUIRE(aType != nullptr);

    const PrimitiveTypeVar* pt = get<PrimitiveTypeVar>(aType);
    if (pt != nullptr)
        return pt->type;
    else
        return std::nullopt;
}

std::optional<TypeId> Fixture::getType(const std::string& name)
{
    ModulePtr module = getMainModule();
    REQUIRE(module);

    return lookupName(module->getModuleScope(), name);
}

TypeId Fixture::requireType(const std::string& name)
{
    std::optional<TypeId> ty = getType(name);
    REQUIRE(bool(ty));
    return follow(*ty);
}

TypeId Fixture::requireType(const ModuleName& moduleName, const std::string& name)
{
    ModulePtr module = frontend.moduleResolver.getModule(moduleName);
    REQUIRE(module);
    return requireType(module->getModuleScope(), name);
}

TypeId Fixture::requireType(const ModulePtr& module, const std::string& name)
{
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
    if (auto typeFun = getMainModule()->getModuleScope()->lookupType(name))
        return typeFun->type;

    return std::nullopt;
}

std::optional<TypeId> Fixture::lookupImportedType(const std::string& moduleAlias, const std::string& name)
{
    if (auto typeFun = getMainModule()->getModuleScope()->lookupImportedType(moduleAlias, name))
        return typeFun->type;

    return std::nullopt;
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
            return;
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
    addGlobalBinding(typeChecker, "game", typeChecker.anyType, "@luau");
    addGlobalBinding(typeChecker, "workspace", typeChecker.anyType, "@luau");
    addGlobalBinding(typeChecker, "script", typeChecker.anyType, "@luau");
}

void Fixture::dumpErrors(const CheckResult& cr)
{
    dumpErrors(std::cout, cr.errors);
}

void Fixture::dumpErrors(const ModulePtr& module)
{
    dumpErrors(std::cout, module->errors);
}

void Fixture::dumpErrors(const Module& module)
{
    dumpErrors(std::cout, module.errors);
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
    unfreeze(typeChecker.globalTypes);
    LoadDefinitionFileResult result = loadDefinitionFile(typeChecker, typeChecker.globalScope, source, "@test");
    freeze(typeChecker.globalTypes);

    REQUIRE_MESSAGE(result.success, "loadDefinition: unable to load definition file");
    return result;
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
    return arena.typeVars.contains(t);
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

} // namespace Luau
