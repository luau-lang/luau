// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Config.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Scope.h"
#include "Luau/Variant.h"
#include "Luau/Normalize.h"
#include "Luau/TypePack.h"
#include "Luau/TypeArena.h"

#include <mutex>
#include <string>
#include <vector>
#include <optional>

namespace Luau
{

class AstStat;
class ParseError;
struct TypeError;
struct LintWarning;
struct GlobalTypes;
struct ModuleResolver;
struct ParseResult;
struct DcrLogger;

struct TelemetryTypePair
{
    std::string annotatedType;
    std::string inferredType;
};

struct AnyTypeSummary
{
    TypeArena arena;

    DenseHashSet<TypeId> seenTypeFamilyInstances{nullptr};

    int recursionCount = 0;

    std::string root;
    int strictCount = 0;

    DenseHashMap<const void*, bool> seen{nullptr};

    AnyTypeSummary();

    void traverse(Module* module, AstStat* src, NotNull<BuiltinTypes> builtinTypes);

    std::pair<bool, TypeId> checkForAnyCast(Scope* scope, AstExprTypeAssertion* expr);

    // Todo: errors resolved by anys
    void reportError(Location location, TypeErrorData err);

    bool containsAny(TypePackId typ);
    bool containsAny(TypeId typ);

    bool isAnyCast(Scope* scope, AstExpr* expr, Module* module, NotNull<BuiltinTypes> builtinTypes);
    bool isAnyCall(Scope* scope, AstExpr* expr, Module* module, NotNull<BuiltinTypes> builtinTypes);

    bool hasVariadicAnys(Scope* scope, AstExprFunction* expr, Module* module, NotNull<BuiltinTypes> builtinTypes);
    bool hasArgAnys(Scope* scope, AstExprFunction* expr, Module* module, NotNull<BuiltinTypes> builtinTypes);
    bool hasAnyReturns(Scope* scope, AstExprFunction* expr, Module* module, NotNull<BuiltinTypes> builtinTypes);

    TypeId checkForFamilyInhabitance(TypeId instance, Location location);
    TypeId lookupType(AstExpr* expr, Module* module, NotNull<BuiltinTypes> builtinTypes);
    TypePackId reconstructTypePack(AstArray<AstExpr*> exprs, Module* module, NotNull<BuiltinTypes> builtinTypes);

    DenseHashSet<TypeId> seenTypeFunctionInstances{nullptr};
    TypeId lookupAnnotation(AstType* annotation, Module* module, NotNull<BuiltinTypes> builtintypes);
    std::optional<TypePackId> lookupPackAnnotation(AstTypePack* annotation, Module* module);
    TypeId checkForTypeFunctionInhabitance(TypeId instance, Location location);

    enum Pattern : uint64_t
    {
        Casts,
        FuncArg,
        FuncRet,
        FuncApp,
        VarAnnot,
        VarAny,
        TableProp,
        Alias,
        Assign
    };

    struct TypeInfo
    {
        Pattern code;
        std::string node;
        TelemetryTypePair type;
        std::string debug;

        explicit TypeInfo(Pattern code, std::string node, TelemetryTypePair type);
    };

    std::vector<TypeInfo> typeInfo;

    /**
     * Fabricates a scope that is a child of another scope.
     * @param node the lexical node that the scope belongs to.
     * @param parent the parent scope of the new scope. Must not be null.
     */
    Scope* childScope(AstNode* node, const Scope* parent);

    Scope* findInnerMostScope(Location location, Module* module);

    void visit(Scope* scope, AstStat* stat, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatBlock* block, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatIf* ifStatement, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatWhile* while_, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatRepeat* repeat, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatReturn* ret, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatLocal* local, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatFor* for_, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatForIn* forIn, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatAssign* assign, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatCompoundAssign* assign, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatFunction* function, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatLocalFunction* function, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatTypeAlias* alias, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatExpr* expr, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatDeclareGlobal* declareGlobal, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatDeclareClass* declareClass, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatDeclareFunction* declareFunction, Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(Scope* scope, AstStatError* error, Module* module, NotNull<BuiltinTypes> builtinTypes);
};

} // namespace Luau