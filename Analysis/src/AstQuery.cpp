// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AstQuery.h"

#include "Luau/Module.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"
#include "Luau/ToString.h"

#include "Luau/Common.h"

#include <algorithm>

namespace Luau
{

namespace
{

struct FindNode : public AstVisitor
{
    const Position pos;
    const Position documentEnd;
    AstNode* best = nullptr;

    explicit FindNode(Position pos, Position documentEnd)
        : pos(pos)
        , documentEnd(documentEnd)
    {
    }

    bool visit(AstNode* node) override
    {
        if (node->location.contains(pos))
        {
            best = node;
            return true;
        }

        // Edge case: If we ask for the node at the position that is the very end of the document
        // return the innermost AST element that ends at that position.

        if (node->location.end == documentEnd && pos >= documentEnd)
        {
            best = node;
            return true;
        }

        return false;
    }

    bool visit(AstStatBlock* block) override
    {
        visit(static_cast<AstNode*>(block));

        for (AstStat* stat : block->body)
        {
            if (stat->location.end < pos)
                continue;
            if (stat->location.begin > pos)
                break;

            stat->visit(this);
        }

        return false;
    }
};

struct FindFullAncestry final : public AstVisitor
{
    std::vector<AstNode*> nodes;
    Position pos;

    explicit FindFullAncestry(Position pos)
        : pos(pos)
    {
    }

    bool visit(AstNode* node)
    {
        if (node->location.contains(pos))
        {
            nodes.push_back(node);
            return true;
        }
        return false;
    }
};

} // namespace

std::vector<AstNode*> findAstAncestryOfPosition(const SourceModule& source, Position pos)
{
    FindFullAncestry finder(pos);
    source.root->visit(&finder);
    return std::move(finder.nodes);
}

AstNode* findNodeAtPosition(const SourceModule& source, Position pos)
{
    const Position end = source.root->location.end;
    if (pos < source.root->location.begin)
        return source.root;

    if (pos > end)
        pos = end;

    FindNode findNode{pos, end};
    findNode.visit(source.root);
    return findNode.best;
}

AstExpr* findExprAtPosition(const SourceModule& source, Position pos)
{
    AstNode* node = findNodeAtPosition(source, pos);
    if (node)
        return node->asExpr();
    else
        return nullptr;
}

ScopePtr findScopeAtPosition(const Module& module, Position pos)
{
    LUAU_ASSERT(!module.scopes.empty());

    Location scopeLocation = module.scopes.front().first;
    ScopePtr scope = module.scopes.front().second;
    for (const auto& s : module.scopes)
    {
        if (s.first.contains(pos))
        {
            if (!scope || scopeLocation.encloses(s.first))
            {
                scopeLocation = s.first;
                scope = s.second;
            }
        }
    }
    return scope;
}

std::optional<TypeId> findTypeAtPosition(const Module& module, const SourceModule& sourceModule, Position pos)
{
    if (auto expr = findExprAtPosition(sourceModule, pos))
    {
        if (auto it = module.astTypes.find(expr); it != module.astTypes.end())
            return it->second;
    }

    return std::nullopt;
}

std::optional<TypeId> findExpectedTypeAtPosition(const Module& module, const SourceModule& sourceModule, Position pos)
{
    if (auto expr = findExprAtPosition(sourceModule, pos))
    {
        if (auto it = module.astExpectedTypes.find(expr); it != module.astExpectedTypes.end())
            return it->second;
    }

    return std::nullopt;
}

static std::optional<AstStatLocal*> findBindingLocalStatement(const SourceModule& source, const Binding& binding)
{
    std::vector<AstNode*> nodes = findAstAncestryOfPosition(source, binding.location.begin);
    auto iter = std::find_if(nodes.rbegin(), nodes.rend(), [](AstNode* node) {
        return node->is<AstStatLocal>();
    });
    return iter != nodes.rend() ? std::make_optional((*iter)->as<AstStatLocal>()) : std::nullopt;
}

std::optional<Binding> findBindingAtPosition(const Module& module, const SourceModule& source, Position pos)
{
    AstExpr* expr = findExprAtPosition(source, pos);
    if (!expr)
        return std::nullopt;

    Symbol name;
    if (auto g = expr->as<AstExprGlobal>())
        name = g->name;
    else if (auto l = expr->as<AstExprLocal>())
        name = l->local;
    else
        return std::nullopt;

    ScopePtr currentScope = findScopeAtPosition(module, pos);
    LUAU_ASSERT(currentScope);

    while (currentScope)
    {
        auto iter = currentScope->bindings.find(name);
        if (iter != currentScope->bindings.end() && iter->second.location.begin <= pos)
        {
            /* Ignore this binding if we're inside its definition. e.g. local abc = abc -- Will take the definition of abc from outer scope */
            std::optional<AstStatLocal*> bindingStatement = findBindingLocalStatement(source, iter->second);
            if (!bindingStatement || !(*bindingStatement)->location.contains(pos))
                return iter->second;
        }
        currentScope = currentScope->parent;
    }

    return std::nullopt;
}

namespace
{
struct FindExprOrLocal : public AstVisitor
{
    const Position pos;
    ExprOrLocal result;

    explicit FindExprOrLocal(Position pos)
        : pos(pos)
    {
    }

    // We want to find the result with the smallest location range.
    bool isCloserMatch(Location newLocation)
    {
        auto current = result.getLocation();
        return newLocation.contains(pos) && (!current || current->encloses(newLocation));
    }

    bool visit(AstStatBlock* block) override
    {
        for (AstStat* stat : block->body)
        {
            if (stat->location.end <= pos)
                continue;
            if (stat->location.begin > pos)
                break;

            stat->visit(this);
        }

        return false;
    }

    bool visit(AstExpr* expr) override
    {
        if (isCloserMatch(expr->location))
        {
            result.setExpr(expr);
            return true;
        }
        return false;
    }

    bool visitLocal(AstLocal* local)
    {
        if (isCloserMatch(local->location))
        {
            result.setLocal(local);
            return true;
        }
        return false;
    }

    bool visit(AstStatLocalFunction* function) override
    {
        visitLocal(function->name);
        return true;
    }

    bool visit(AstStatLocal* al) override
    {
        for (size_t i = 0; i < al->vars.size; ++i)
        {
            visitLocal(al->vars.data[i]);
        }
        return true;
    }

    virtual bool visit(AstExprFunction* fn) override
    {
        for (size_t i = 0; i < fn->args.size; ++i)
        {
            visitLocal(fn->args.data[i]);
        }
        return visit((class AstExpr*)fn);
    }

    virtual bool visit(AstStatFor* forStat) override
    {
        visitLocal(forStat->var);
        return true;
    }

    virtual bool visit(AstStatForIn* forIn) override
    {
        for (AstLocal* var : forIn->vars)
        {
            visitLocal(var);
        }
        return true;
    }
};
}; // namespace

ExprOrLocal findExprOrLocalAtPosition(const SourceModule& source, Position pos)
{
    FindExprOrLocal findVisitor{pos};
    findVisitor.visit(source.root);
    return findVisitor.result;
}

std::optional<DocumentationSymbol> getDocumentationSymbolAtPosition(const SourceModule& source, const Module& module, Position position)
{
    std::vector<AstNode*> ancestry = findAstAncestryOfPosition(source, position);

    AstExpr* targetExpr = ancestry.size() >= 1 ? ancestry[ancestry.size() - 1]->asExpr() : nullptr;
    AstExpr* parentExpr = ancestry.size() >= 2 ? ancestry[ancestry.size() - 2]->asExpr() : nullptr;

    if (std::optional<Binding> binding = findBindingAtPosition(module, source, position))
    {
        if (binding->documentationSymbol)
        {
            // This might be an overloaded function binding.
            if (get<IntersectionTypeVar>(follow(binding->typeId)))
            {
                TypeId matchingOverload = nullptr;
                if (parentExpr && parentExpr->is<AstExprCall>())
                {
                    if (auto it = module.astOverloadResolvedTypes.find(parentExpr); it != module.astOverloadResolvedTypes.end())
                    {
                        matchingOverload = it->second;
                    }
                }

                if (matchingOverload)
                {
                    std::string overloadSymbol = *binding->documentationSymbol + "/overload/";
                    // Default toString options are fine for this purpose.
                    overloadSymbol += toString(matchingOverload);
                    return overloadSymbol;
                }
            }
        }

        return binding->documentationSymbol;
    }

    if (targetExpr)
    {
        if (AstExprIndexName* indexName = targetExpr->as<AstExprIndexName>())
        {
            if (auto it = module.astTypes.find(indexName->expr); it != module.astTypes.end())
            {
                TypeId parentTy = follow(it->second);
                if (const TableTypeVar* ttv = get<TableTypeVar>(parentTy))
                {
                    if (auto propIt = ttv->props.find(indexName->index.value); propIt != ttv->props.end())
                    {
                        return propIt->second.documentationSymbol;
                    }
                }
                else if (const ClassTypeVar* ctv = get<ClassTypeVar>(parentTy))
                {
                    if (auto propIt = ctv->props.find(indexName->index.value); propIt != ctv->props.end())
                    {
                        return propIt->second.documentationSymbol;
                    }
                }
            }
        }
        else if (AstExprFunction* fn = targetExpr->as<AstExprFunction>())
        {
            // Handle event connection-like structures where we have
            // something:Connect(function(a, b, c) end)
            // In this case, we want to ascribe a documentation symbol to 'a'
            // based on the documentation symbol of Connect.
            if (parentExpr && parentExpr->is<AstExprCall>())
            {
                AstExprCall* call = parentExpr->as<AstExprCall>();
                if (std::optional<DocumentationSymbol> parentSymbol = getDocumentationSymbolAtPosition(source, module, call->func->location.begin))
                {
                    for (size_t i = 0; i < call->args.size; ++i)
                    {
                        AstExpr* callArg = call->args.data[i];
                        if (callArg == targetExpr)
                        {
                            std::string fnSymbol = *parentSymbol + "/param/" + std::to_string(i);
                            for (size_t j = 0; j < fn->args.size; ++j)
                            {
                                AstLocal* fnArg = fn->args.data[j];

                                if (fnArg->location.contains(position))
                                {
                                    return fnSymbol + "/param/" + std::to_string(j);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if (std::optional<TypeId> ty = findTypeAtPosition(module, source, position))
    {
        if ((*ty)->documentationSymbol)
        {
            return (*ty)->documentationSymbol;
        }
    }

    return std::nullopt;
}

} // namespace Luau
