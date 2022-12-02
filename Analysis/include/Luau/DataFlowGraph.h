// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

// Do not include LValue. It should never be used here.
#include "Luau/Ast.h"
#include "Luau/DenseHash.h"
#include "Luau/Def.h"
#include "Luau/Symbol.h"

#include <unordered_map>

namespace Luau
{

struct DataFlowGraph
{
    DataFlowGraph(DataFlowGraph&&) = default;
    DataFlowGraph& operator=(DataFlowGraph&&) = default;

    // TODO: AstExprLocal, AstExprGlobal, and AstLocal* are guaranteed never to return nullopt.
    // We leave them to return an optional as we build it out, but the end state is for them to return a non-optional DefId.
    std::optional<DefId> getDef(const AstExpr* expr) const;
    std::optional<DefId> getDef(const AstLocal* local) const;

    /// Retrieve the Def that corresponds to the given Symbol.
    ///
    /// We do not perform dataflow analysis on globals, so this function always
    /// yields nullopt when passed a global Symbol.
    std::optional<DefId> getDef(const Symbol& symbol) const;

private:
    DataFlowGraph() = default;

    DataFlowGraph(const DataFlowGraph&) = delete;
    DataFlowGraph& operator=(const DataFlowGraph&) = delete;

    DefArena arena;
    DenseHashMap<const AstExpr*, const Def*> astDefs{nullptr};
    DenseHashMap<const AstLocal*, const Def*> localDefs{nullptr};

    friend struct DataFlowGraphBuilder;
};

struct DfgScope
{
    DfgScope* parent;
    DenseHashMap<Symbol, const Def*> bindings{Symbol{}};
};

struct ExpressionFlowGraph
{
    std::optional<DefId> def;
};

// Currently unsound. We do not presently track the control flow of the program.
// Additionally, we do not presently track assignments.
struct DataFlowGraphBuilder
{
    static DataFlowGraph build(AstStatBlock* root, NotNull<struct InternalErrorReporter> handle);

private:
    DataFlowGraphBuilder() = default;

    DataFlowGraphBuilder(const DataFlowGraphBuilder&) = delete;
    DataFlowGraphBuilder& operator=(const DataFlowGraphBuilder&) = delete;

    DataFlowGraph graph;
    NotNull<DefArena> arena{&graph.arena};
    struct InternalErrorReporter* handle;
    std::vector<std::unique_ptr<DfgScope>> scopes;

    // Does not belong in DataFlowGraphBuilder, but the old solver allows properties to escape the scope they were defined in,
    // so we will need to be able to emulate this same behavior here too. We can kill this once we have better flow sensitivity.
    DenseHashMap<const Def*, std::unordered_map<std::string, const Def*>> props{nullptr};

    DfgScope* childScope(DfgScope* scope);

    std::optional<DefId> use(DfgScope* scope, Symbol symbol, AstExpr* e);
    DefId use(DefId def, AstExprIndexName* e);

    void visit(DfgScope* scope, AstStatBlock* b);
    void visitBlockWithoutChildScope(DfgScope* scope, AstStatBlock* b);

    // TODO: visit type aliases
    void visit(DfgScope* scope, AstStat* s);
    void visit(DfgScope* scope, AstStatIf* i);
    void visit(DfgScope* scope, AstStatWhile* w);
    void visit(DfgScope* scope, AstStatRepeat* r);
    void visit(DfgScope* scope, AstStatBreak* b);
    void visit(DfgScope* scope, AstStatContinue* c);
    void visit(DfgScope* scope, AstStatReturn* r);
    void visit(DfgScope* scope, AstStatExpr* e);
    void visit(DfgScope* scope, AstStatLocal* l);
    void visit(DfgScope* scope, AstStatFor* f);
    void visit(DfgScope* scope, AstStatForIn* f);
    void visit(DfgScope* scope, AstStatAssign* a);
    void visit(DfgScope* scope, AstStatCompoundAssign* c);
    void visit(DfgScope* scope, AstStatFunction* f);
    void visit(DfgScope* scope, AstStatLocalFunction* l);

    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExpr* e);
    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExprLocal* l);
    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExprGlobal* g);
    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExprCall* c);
    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExprIndexName* i);
    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExprIndexExpr* i);
    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExprFunction* f);
    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExprTable* t);
    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExprUnary* u);
    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExprBinary* b);
    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExprTypeAssertion* t);
    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExprIfElse* i);
    ExpressionFlowGraph visitExpr(DfgScope* scope, AstExprInterpString* i);

    // TODO: visitLValue
    // TODO: visitTypes (because of typeof which has access to values namespace, needs unreachable scope)
    // TODO: visitTypePacks (because of typeof which has access to values namespace, needs unreachable scope)
};

} // namespace Luau
