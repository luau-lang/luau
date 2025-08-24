// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

// Do not include LValue. It should never be used here.
#include "Luau/Ast.h"
#include "Luau/ControlFlow.h"
#include "Luau/DenseHash.h"
#include "Luau/Def.h"
#include "Luau/NotNull.h"
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
    // Look up the definition optionally, knowing it may not be present.
    std::optional<DefId> getDefOptional(const AstExpr* expr) const;

    DefId getDef(const AstLocal* local) const;

    DefId getDef(const AstStatDeclareGlobal* global) const;
    DefId getDef(const AstStatDeclareFunction* func) const;

    const RefinementKey* getRefinementKey(const AstExpr* expr) const;

    std::optional<Symbol> getSymbolFromDef(const Def* def) const;

private:
    DataFlowGraph(NotNull<DefArena> defArena, NotNull<RefinementKeyArena> keyArena);

    DataFlowGraph(const DataFlowGraph&) = delete;
    DataFlowGraph& operator=(const DataFlowGraph&) = delete;

    NotNull<DefArena> defArena;
    NotNull<RefinementKeyArena> keyArena;

    DenseHashMap<const AstExpr*, const Def*> astDefs{nullptr};

    // Sometimes we don't have the AstExprLocal* but we have AstLocal*, and sometimes we need to extract that DefId.
    DenseHashMap<const AstLocal*, const Def*> localDefs{nullptr};

    // There's no AstStatDeclaration, and it feels useless to introduce it just to enforce an invariant in one place.
    // All keys in this maps are really only statements that ambiently declares a symbol.
    DenseHashMap<const AstStat*, const Def*> declaredDefs{nullptr};
    DenseHashMap<const Def*, Symbol> defToSymbol{nullptr};

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
};

struct DataFlowResult
{
    DefId def;
    const RefinementKey* parent = nullptr;
};

using ScopeStack = std::vector<DfgScope*>;

struct DataFlowGraphBuilder
{
    static DataFlowGraph build(
        AstStatBlock* block,
        NotNull<DefArena> defArena,
        NotNull<RefinementKeyArena> keyArena,
        NotNull<struct InternalErrorReporter> handle
    );

private:
    DataFlowGraphBuilder(NotNull<DefArena> defArena, NotNull<RefinementKeyArena> keyArena);

    DataFlowGraphBuilder(const DataFlowGraphBuilder&) = delete;
    DataFlowGraphBuilder& operator=(const DataFlowGraphBuilder&) = delete;

    DataFlowGraph graph;
    NotNull<DefArena> defArena;
    NotNull<RefinementKeyArena> keyArena;

    struct InternalErrorReporter* handle = nullptr;

    /// The arena owning all of the scope allocations for the dataflow graph being built.
    std::vector<std::unique_ptr<DfgScope>> scopes;

    /// A stack of scopes used by the visitor to see where we are.
    ScopeStack scopeStack;
    NotNull<DfgScope> currentScope();

    struct FunctionCapture
    {
        std::vector<DefId> captureDefs;
        std::vector<DefId> allVersions;
        size_t versionOffset = 0;
    };

    DenseHashMap<Symbol, FunctionCapture> captures{Symbol{}};
    void resolveCaptures();

    DfgScope* makeChildScope(DfgScope::ScopeType scopeType = DfgScope::Linear);

    void join(DfgScope* p, DfgScope* a, DfgScope* b);
    void joinBindings(DfgScope* p, const DfgScope& a, const DfgScope& b);
    void joinProps(DfgScope* p, const DfgScope& a, const DfgScope& b);

    DefId lookup(Symbol symbol, Location location);
    DefId lookup(DefId def, const std::string& key, Location location);

    ControlFlow visit(AstStatBlock* b);
    ControlFlow visitBlockWithoutChildScope(AstStatBlock* b);

    ControlFlow visit(AstStat* s);
    ControlFlow visit(AstStatIf* i);
    ControlFlow visit(AstStatWhile* w);
    ControlFlow visit(AstStatRepeat* r);
    ControlFlow visit(AstStatBreak* b);
    ControlFlow visit(AstStatContinue* c);
    ControlFlow visit(AstStatReturn* r);
    ControlFlow visit(AstStatExpr* e);
    ControlFlow visit(AstStatLocal* l);
    ControlFlow visit(AstStatFor* f);
    ControlFlow visit(AstStatForIn* f);
    ControlFlow visit(AstStatAssign* a);
    ControlFlow visit(AstStatCompoundAssign* c);
    ControlFlow visit(AstStatFunction* f);
    ControlFlow visit(AstStatLocalFunction* l);
    ControlFlow visit(AstStatTypeAlias* t);
    ControlFlow visit(AstStatTypeFunction* f);
    ControlFlow visit(AstStatDeclareGlobal* d);
    ControlFlow visit(AstStatDeclareFunction* d);
    ControlFlow visit(AstStatDeclareExternType* d);
    ControlFlow visit(AstStatError* error);

    DataFlowResult visitExpr(AstExpr* e);
    DataFlowResult visitExpr(AstExprGroup* group);
    DataFlowResult visitExpr(AstExprLocal* l);
    DataFlowResult visitExpr(AstExprGlobal* g);
    DataFlowResult visitExpr(AstExprCall* c);
    DataFlowResult visitExpr(AstExprIndexName* i);
    DataFlowResult visitExpr(AstExprIndexExpr* i);
    DataFlowResult visitExpr(AstExprFunction* f);
    DataFlowResult visitExpr(AstExprTable* t);
    DataFlowResult visitExpr(AstExprUnary* u);
    DataFlowResult visitExpr(AstExprBinary* b);
    DataFlowResult visitExpr(AstExprTypeAssertion* t);
    DataFlowResult visitExpr(AstExprIfElse* i);
    DataFlowResult visitExpr(AstExprInterpString* i);
    DataFlowResult visitExpr(AstExprError* error);

    void visitLValue(AstExpr* e, DefId incomingDef);
    DefId visitLValue(AstExprLocal* l, DefId incomingDef);
    DefId visitLValue(AstExprGlobal* g, DefId incomingDef);
    DefId visitLValue(AstExprIndexName* i, DefId incomingDef);
    DefId visitLValue(AstExprIndexExpr* i, DefId incomingDef);
    DefId visitLValue(AstExprError* e, DefId incomingDef);

    void visitType(AstType* t);
    void visitType(AstTypeReference* r);
    void visitType(AstTypeTable* t);
    void visitType(AstTypeFunction* f);
    void visitType(AstTypeTypeof* t);
    void visitType(AstTypeUnion* u);
    void visitType(AstTypeIntersection* i);
    void visitType(AstTypeError* error);

    void visitTypePack(AstTypePack* p);
    void visitTypePack(AstTypePackExplicit* e);
    void visitTypePack(AstTypePackVariadic* v);
    void visitTypePack(AstTypePackGeneric* g);

    void visitTypeList(AstTypeList l);

    void visitGenerics(AstArray<AstGenericType*> g);
    void visitGenericPacks(AstArray<AstGenericTypePack*> g);
};

} // namespace Luau
