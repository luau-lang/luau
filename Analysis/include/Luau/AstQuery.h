// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Documentation.h"
#include "Luau/TypeFwd.h"

#include <memory>

namespace Luau
{

struct Binding;
struct SourceModule;
struct Module;

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
    std::optional<AstName> getName()
    {
        if (expr)
        {
            if (AstName name = getIdentifier(expr); name.value)
            {
                return name;
            }
        }
        else if (local)
        {
            return local->name;
        }
        return std::nullopt;
    }

private:
    AstExpr* expr = nullptr;
    AstLocal* local = nullptr;
};

struct FindFullAncestry final : public AstVisitor
{
    std::vector<AstNode*> nodes;
    Position pos;
    Position documentEnd;
    bool includeTypes = false;

    explicit FindFullAncestry(Position pos, Position documentEnd, bool includeTypes = false);

    bool visit(AstType* type) override;

    bool visit(AstStatFunction* node) override;

    bool visit(AstNode* node) override;
};

std::vector<AstNode*> findAncestryAtPositionForAutocomplete(const SourceModule& source, Position pos);
std::vector<AstNode*> findAncestryAtPositionForAutocomplete(AstStatBlock* root, Position pos);
std::vector<AstNode*> findAstAncestryOfPosition(const SourceModule& source, Position pos, bool includeTypes = false);
std::vector<AstNode*> findAstAncestryOfPosition(AstStatBlock* root, Position pos, bool includeTypes = false);
AstNode* findNodeAtPosition(const SourceModule& source, Position pos);
AstNode* findNodeAtPosition(AstStatBlock* root, Position pos);
AstExpr* findExprAtPosition(const SourceModule& source, Position pos);
ScopePtr findScopeAtPosition(const Module& module, Position pos);
std::optional<Binding> findBindingAtPosition(const Module& module, const SourceModule& source, Position pos);
ExprOrLocal findExprOrLocalAtPosition(const SourceModule& source, Position pos);

std::optional<TypeId> findTypeAtPosition(const Module& module, const SourceModule& sourceModule, Position pos);
std::optional<TypeId> findExpectedTypeAtPosition(const Module& module, const SourceModule& sourceModule, Position pos);

std::optional<DocumentationSymbol> getDocumentationSymbolAtPosition(const SourceModule& source, const Module& module, Position position);

} // namespace Luau
