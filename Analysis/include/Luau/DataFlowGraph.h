// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

// Do not include LValue. It should never be used here.
#include "Luau/Ast.h"
#include "Luau/Breadcrumb.h"
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

    NullableBreadcrumbId getBreadcrumb(const AstExpr* expr) const;

    BreadcrumbId getBreadcrumb(const AstLocal* local) const;
    BreadcrumbId getBreadcrumb(const AstExprLocal* local) const;
    BreadcrumbId getBreadcrumb(const AstExprGlobal* global) const;

    BreadcrumbId getBreadcrumb(const AstStatDeclareGlobal* global) const;
    BreadcrumbId getBreadcrumb(const AstStatDeclareFunction* func) const;

private:
    DataFlowGraph() = default;

    DataFlowGraph(const DataFlowGraph&) = delete;
    DataFlowGraph& operator=(const DataFlowGraph&) = delete;

    DefArena defs;
    BreadcrumbArena breadcrumbs;

    DenseHashMap<const AstExpr*, NullableBreadcrumbId> astBreadcrumbs{nullptr};

    // Sometimes we don't have the AstExprLocal* but we have AstLocal*, and sometimes we need to extract that DefId.
    DenseHashMap<const AstLocal*, NullableBreadcrumbId> localBreadcrumbs{nullptr};

    // There's no AstStatDeclaration, and it feels useless to introduce it just to enforce an invariant in one place.
    // All keys in this maps are really only statements that ambiently declares a symbol.
    DenseHashMap<const AstStat*, NullableBreadcrumbId> declaredBreadcrumbs{nullptr};

    friend struct DataFlowGraphBuilder;
};

struct DfgScope
{
    DfgScope* parent;
    DenseHashMap<Symbol, NullableBreadcrumbId> bindings{Symbol{}};
    DenseHashMap<const Def*, std::unordered_map<std::string, NullableBreadcrumbId>> props{nullptr};

    NullableBreadcrumbId lookup(Symbol symbol) const;
    NullableBreadcrumbId lookup(DefId def, const std::string& key) const;
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
    NotNull<DefArena> defs{&graph.defs};
    NotNull<BreadcrumbArena> breadcrumbs{&graph.breadcrumbs};

    struct InternalErrorReporter* handle = nullptr;
    DfgScope* moduleScope = nullptr;

    std::vector<std::unique_ptr<DfgScope>> scopes;

    DfgScope* childScope(DfgScope* scope);

    void visit(DfgScope* scope, AstStatBlock* b);
    void visitBlockWithoutChildScope(DfgScope* scope, AstStatBlock* b);

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
    void visit(DfgScope* scope, AstStatTypeAlias* t);
    void visit(DfgScope* scope, AstStatDeclareGlobal* d);
    void visit(DfgScope* scope, AstStatDeclareFunction* d);
    void visit(DfgScope* scope, AstStatDeclareClass* d);
    void visit(DfgScope* scope, AstStatError* error);

    BreadcrumbId visitExpr(DfgScope* scope, AstExpr* e);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprLocal* l);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprGlobal* g);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprCall* c);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprIndexName* i);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprIndexExpr* i);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprFunction* f);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprTable* t);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprUnary* u);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprBinary* b);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprTypeAssertion* t);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprIfElse* i);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprInterpString* i);
    BreadcrumbId visitExpr(DfgScope* scope, AstExprError* error);

    void visitLValue(DfgScope* scope, AstExpr* e);
    void visitLValue(DfgScope* scope, AstExprLocal* l);
    void visitLValue(DfgScope* scope, AstExprGlobal* g);
    void visitLValue(DfgScope* scope, AstExprIndexName* i);
    void visitLValue(DfgScope* scope, AstExprIndexExpr* i);
    void visitLValue(DfgScope* scope, AstExprError* e);

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
