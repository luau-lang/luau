// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

// Do not include LValue. It should never be used here.
#include "Luau/Ast.h"
#include "Luau/ControlFlow.h"
#include "Luau/DenseHash.h"
#include "Luau/Def.h"
#include "Luau/Symbol.h"
#include "Luau/TypedAllocator.h"

#include <unordered_map>

namespace Luau
{

struct RefinementKey
{
    const RefinementKey* parent = nullptr;
    DefId def;
    std::optional<std::string> propName;
};

struct RefinementKeyArena
{
    TypedAllocator<RefinementKey> allocator;

    const RefinementKey* leaf(DefId def);
    const RefinementKey* node(const RefinementKey* parent, DefId def, const std::string& propName);
};

struct DataFlowGraph
{
    DataFlowGraph(DataFlowGraph&&) = default;
    DataFlowGraph& operator=(DataFlowGraph&&) = default;

    DefId getDef(const AstExpr* expr) const;
    // Look up for the rvalue def for a compound assignment.
    std::optional<DefId> getRValueDefForCompoundAssign(const AstExpr* expr) const;

    DefId getDef(const AstLocal* local) const;

    DefId getDef(const AstStatDeclareGlobal* global) const;
    DefId getDef(const AstStatDeclareFunction* func) const;

    const RefinementKey* getRefinementKey(const AstExpr* expr) const;

private:
    DataFlowGraph() = default;

    DataFlowGraph(const DataFlowGraph&) = delete;
    DataFlowGraph& operator=(const DataFlowGraph&) = delete;

    DefArena defArena;
    RefinementKeyArena keyArena;

    DenseHashMap<const AstExpr*, const Def*> astDefs{nullptr};

    // Sometimes we don't have the AstExprLocal* but we have AstLocal*, and sometimes we need to extract that DefId.
    DenseHashMap<const AstLocal*, const Def*> localDefs{nullptr};

    // There's no AstStatDeclaration, and it feels useless to introduce it just to enforce an invariant in one place.
    // All keys in this maps are really only statements that ambiently declares a symbol.
    DenseHashMap<const AstStat*, const Def*> declaredDefs{nullptr};

    // Compound assignments are in a weird situation where the local being assigned to is also being used at its
    // previous type implicitly in an rvalue position. This map provides the previous binding.
    DenseHashMap<const AstExpr*, const Def*> compoundAssignDefs{nullptr};

    DenseHashMap<const AstExpr*, const RefinementKey*> astRefinementKeys{nullptr};

    friend struct DataFlowGraphBuilder;
};

struct DfgScope
{
    enum ScopeType
    {
        Linear,
        Loop,
        Function,
    };

    DfgScope* parent;
    ScopeType scopeType;

    using Bindings = DenseHashMap<Symbol, const Def*>;
    using Props = DenseHashMap<const Def*, std::unordered_map<std::string, const Def*>>;

    Bindings bindings{Symbol{}};
    Props props{nullptr};

    std::optional<DefId> lookup(Symbol symbol) const;
    std::optional<DefId> lookup(DefId def, const std::string& key) const;

    void inherit(const DfgScope* childScope);

    bool canUpdateDefinition(Symbol symbol) const;
    bool canUpdateDefinition(DefId def, const std::string& key) const;
};

struct DataFlowResult
{
    DefId def;
    const RefinementKey* parent = nullptr;
};

struct DataFlowGraphBuilder
{
    static DataFlowGraph build(AstStatBlock* root, NotNull<struct InternalErrorReporter> handle);

private:
    DataFlowGraphBuilder() = default;

    DataFlowGraphBuilder(const DataFlowGraphBuilder&) = delete;
    DataFlowGraphBuilder& operator=(const DataFlowGraphBuilder&) = delete;

    DataFlowGraph graph;
    NotNull<DefArena> defArena{&graph.defArena};
    NotNull<RefinementKeyArena> keyArena{&graph.keyArena};

    struct InternalErrorReporter* handle = nullptr;
    DfgScope* moduleScope = nullptr;

    std::vector<std::unique_ptr<DfgScope>> scopes;

    struct FunctionCapture
    {
        std::vector<DefId> captureDefs;
        std::vector<DefId> allVersions;
        size_t versionOffset = 0;
    };

    DenseHashMap<Symbol, FunctionCapture> captures{Symbol{}};
    void resolveCaptures();

    DfgScope* childScope(DfgScope* scope, DfgScope::ScopeType scopeType = DfgScope::Linear);

    void join(DfgScope* p, DfgScope* a, DfgScope* b);
    void joinBindings(DfgScope* p, const DfgScope& a, const DfgScope& b);
    void joinProps(DfgScope* p, const DfgScope& a, const DfgScope& b);

    DefId lookup(DfgScope* scope, Symbol symbol);
    DefId lookup(DfgScope* scope, DefId def, const std::string& key);

    ControlFlow visit(DfgScope* scope, AstStatBlock* b);
    ControlFlow visitBlockWithoutChildScope(DfgScope* scope, AstStatBlock* b);

    ControlFlow visit(DfgScope* scope, AstStat* s);
    ControlFlow visit(DfgScope* scope, AstStatIf* i);
    ControlFlow visit(DfgScope* scope, AstStatWhile* w);
    ControlFlow visit(DfgScope* scope, AstStatRepeat* r);
    ControlFlow visit(DfgScope* scope, AstStatBreak* b);
    ControlFlow visit(DfgScope* scope, AstStatContinue* c);
    ControlFlow visit(DfgScope* scope, AstStatReturn* r);
    ControlFlow visit(DfgScope* scope, AstStatExpr* e);
    ControlFlow visit(DfgScope* scope, AstStatLocal* l);
    ControlFlow visit(DfgScope* scope, AstStatFor* f);
    ControlFlow visit(DfgScope* scope, AstStatForIn* f);
    ControlFlow visit(DfgScope* scope, AstStatAssign* a);
    ControlFlow visit(DfgScope* scope, AstStatCompoundAssign* c);
    ControlFlow visit(DfgScope* scope, AstStatFunction* f);
    ControlFlow visit(DfgScope* scope, AstStatLocalFunction* l);
    ControlFlow visit(DfgScope* scope, AstStatTypeAlias* t);
    ControlFlow visit(DfgScope* scope, AstStatDeclareGlobal* d);
    ControlFlow visit(DfgScope* scope, AstStatDeclareFunction* d);
    ControlFlow visit(DfgScope* scope, AstStatDeclareClass* d);
    ControlFlow visit(DfgScope* scope, AstStatError* error);

    DataFlowResult visitExpr(DfgScope* scope, AstExpr* e);
    DataFlowResult visitExpr(DfgScope* scope, AstExprGroup* group);
    DataFlowResult visitExpr(DfgScope* scope, AstExprLocal* l);
    DataFlowResult visitExpr(DfgScope* scope, AstExprGlobal* g);
    DataFlowResult visitExpr(DfgScope* scope, AstExprCall* c);
    DataFlowResult visitExpr(DfgScope* scope, AstExprIndexName* i);
    DataFlowResult visitExpr(DfgScope* scope, AstExprIndexExpr* i);
    DataFlowResult visitExpr(DfgScope* scope, AstExprFunction* f);
    DataFlowResult visitExpr(DfgScope* scope, AstExprTable* t);
    DataFlowResult visitExpr(DfgScope* scope, AstExprUnary* u);
    DataFlowResult visitExpr(DfgScope* scope, AstExprBinary* b);
    DataFlowResult visitExpr(DfgScope* scope, AstExprTypeAssertion* t);
    DataFlowResult visitExpr(DfgScope* scope, AstExprIfElse* i);
    DataFlowResult visitExpr(DfgScope* scope, AstExprInterpString* i);
    DataFlowResult visitExpr(DfgScope* scope, AstExprError* error);

    void visitLValue(DfgScope* scope, AstExpr* e, DefId incomingDef, bool isCompoundAssignment = false);
    DefId visitLValue(DfgScope* scope, AstExprLocal* l, DefId incomingDef, bool isCompoundAssignment);
    DefId visitLValue(DfgScope* scope, AstExprGlobal* g, DefId incomingDef, bool isCompoundAssignment);
    DefId visitLValue(DfgScope* scope, AstExprIndexName* i, DefId incomingDef);
    DefId visitLValue(DfgScope* scope, AstExprIndexExpr* i, DefId incomingDef);
    DefId visitLValue(DfgScope* scope, AstExprError* e, DefId incomingDef);

    void visitType(DfgScope* scope, AstType* t);
    void visitType(DfgScope* scope, AstTypeReference* r);
    void visitType(DfgScope* scope, AstTypeTable* t);
    void visitType(DfgScope* scope, AstTypeFunction* f);
    void visitType(DfgScope* scope, AstTypeTypeof* t);
    void visitType(DfgScope* scope, AstTypeUnion* u);
    void visitType(DfgScope* scope, AstTypeIntersection* i);
    void visitType(DfgScope* scope, AstTypeError* error);

    void visitTypePack(DfgScope* scope, AstTypePack* p);
    void visitTypePack(DfgScope* scope, AstTypePackExplicit* e);
    void visitTypePack(DfgScope* scope, AstTypePackVariadic* v);
    void visitTypePack(DfgScope* scope, AstTypePackGeneric* g);

    void visitTypeList(DfgScope* scope, AstTypeList l);

    void visitGenerics(DfgScope* scope, AstArray<AstGenericType> g);
    void visitGenericPacks(DfgScope* scope, AstArray<AstGenericTypePack> g);
};

} // namespace Luau
