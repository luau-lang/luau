// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Documentation.h"

#include <memory>

namespace Luau
{

struct Binding;
struct SourceModule;
struct Module;

struct TypeVar;
using TypeId = const TypeVar*;

using ScopePtr = std::shared_ptr<struct Scope>;

struct ExprOrLocal
{
    AstExpr* getExpr()
    {
        return expr;
    }
    AstLocal* getLocal()
    {
        return local;
    }
    void setExpr(AstExpr* newExpr)
    {
        expr = newExpr;
        local = nullptr;
    }
    void setLocal(AstLocal* newLocal)
    {
        local = newLocal;
        expr = nullptr;
    }
    std::optional<Location> getLocation()
    {
        return expr ? expr->location : (local ? local->location : std::optional<Location>{});
    }

private:
    AstExpr* expr = nullptr;
    AstLocal* local = nullptr;
};

std::vector<AstNode*> findAstAncestryOfPosition(const SourceModule& source, Position pos);
AstNode* findNodeAtPosition(const SourceModule& source, Position pos);
AstExpr* findExprAtPosition(const SourceModule& source, Position pos);
ScopePtr findScopeAtPosition(const Module& module, Position pos);
std::optional<Binding> findBindingAtPosition(const Module& module, const SourceModule& source, Position pos);
ExprOrLocal findExprOrLocalAtPosition(const SourceModule& source, Position pos);

std::optional<TypeId> findTypeAtPosition(const Module& module, const SourceModule& sourceModule, Position pos);
std::optional<TypeId> findExpectedTypeAtPosition(const Module& module, const SourceModule& sourceModule, Position pos);

std::optional<DocumentationSymbol> getDocumentationSymbolAtPosition(const SourceModule& source, const Module& module, Position position);

} // namespace Luau
