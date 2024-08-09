// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/AstQuery.h"
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

    AstStatBlock* rootSrc = nullptr;
    DenseHashSet<TypeId> seenTypeFamilyInstances{nullptr};

    int recursionCount = 0;

    std::string root;
    int strictCount = 0;

    DenseHashMap<const void*, bool> seen{nullptr};

    AnyTypeSummary();

    void traverse(const Module* module, AstStat* src, NotNull<BuiltinTypes> builtinTypes);

    std::pair<bool, TypeId> checkForAnyCast(const Scope* scope, AstExprTypeAssertion* expr);

    bool containsAny(TypePackId typ);
    bool containsAny(TypeId typ);

    bool isAnyCast(const Scope* scope, AstExpr* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    bool isAnyCall(const Scope* scope, AstExpr* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes);

    bool hasVariadicAnys(const Scope* scope, AstExprFunction* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    bool hasArgAnys(const Scope* scope, AstExprFunction* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    bool hasAnyReturns(const Scope* scope, AstExprFunction* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes);

    TypeId checkForFamilyInhabitance(const TypeId instance, Location location);
    TypeId lookupType(const AstExpr* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    TypePackId reconstructTypePack(const AstArray<AstExpr*> exprs, const Module* module, NotNull<BuiltinTypes> builtinTypes);

    DenseHashSet<TypeId> seenTypeFunctionInstances{nullptr};
    TypeId lookupAnnotation(AstType* annotation, const Module* module, NotNull<BuiltinTypes> builtintypes);
    std::optional<TypePackId> lookupPackAnnotation(AstTypePack* annotation, const Module* module);
    TypeId checkForTypeFunctionInhabitance(const TypeId instance, const Location location);

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
        Assign,
        TypePk
    };

    struct TypeInfo
    {
        Pattern code;
        std::string node;
        TelemetryTypePair type;

        explicit TypeInfo(Pattern code, std::string node, TelemetryTypePair type);
    };

    struct FindReturnAncestry final : public AstVisitor
    {
        AstNode* currNode{nullptr};
        AstNode* stat{nullptr};
        Position rootEnd;
        bool found = false;

        explicit FindReturnAncestry(AstNode* stat, Position rootEnd);

        bool visit(AstType* node) override;
        bool visit(AstNode* node) override;
        bool visit(AstStatFunction* node) override;
        bool visit(AstStatLocalFunction* node) override;
    };

    std::vector<TypeInfo> typeInfo;

    /**
     * Fabricates a scope that is a child of another scope.
     * @param node the lexical node that the scope belongs to.
     * @param parent the parent scope of the new scope. Must not be null.
     */
    const Scope* childScope(const AstNode* node, const Scope* parent);

    std::optional<AstExpr*> matchRequire(const AstExprCall& call);
    AstNode* getNode(AstStatBlock* root, AstNode* node);
    const Scope* findInnerMostScope(const Location location, const Module* module);
    const AstNode* findAstAncestryAtLocation(const AstStatBlock* root, AstNode* node);

    void visit(const Scope* scope, AstStat* stat, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatBlock* block, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatIf* ifStatement, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatWhile* while_, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatRepeat* repeat, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatReturn* ret, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatLocal* local, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatFor* for_, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatForIn* forIn, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatAssign* assign, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatCompoundAssign* assign, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatFunction* function, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatLocalFunction* function, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatTypeAlias* alias, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatExpr* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatDeclareGlobal* declareGlobal, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatDeclareClass* declareClass, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatDeclareFunction* declareFunction, const Module* module, NotNull<BuiltinTypes> builtinTypes);
    void visit(const Scope* scope, AstStatError* error, const Module* module, NotNull<BuiltinTypes> builtinTypes);
};

} // namespace Luau