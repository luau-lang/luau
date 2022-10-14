// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Common.h"

#include <vector>
#include <type_traits>

namespace Luau
{

struct Nth
{
    int classIndex;
    int nth;
};

template<typename T>
Nth nth(int nth = 1)
{
    static_assert(std::is_base_of_v<AstNode, T>, "T must be a derived class of AstNode");
    LUAU_ASSERT(nth > 0); // Did you mean to use `nth<T>(1)`?

    return Nth{T::ClassIndex(), nth};
}

struct FindNthOccurenceOf : public AstVisitor
{
    Nth requestedNth;
    int currentOccurrence = 0;
    AstNode* theNode = nullptr;

    FindNthOccurenceOf(Nth nth);

    bool checkIt(AstNode* n);

    bool visit(AstNode* n) override;
    bool visit(AstType* n) override;
    bool visit(AstTypePack* n) override;
};

/** DSL querying of the AST.
 *
 * Given an AST, one can query for a particular node directly without having to manually unwrap the tree, for example:
 *
 * ```
 * if a and b then
 *   print(a + b)
 * end
 *
 * function f(x, y)
 *   return x + y
 * end
 * ```
 *
 * There are numerous ways to access the second AstExprBinary.
 * 1. Luau::query<AstExprBinary>(block, {nth<AstStatFunction>(), nth<AstExprBinary>()})
 * 2. Luau::query<AstExprBinary>(Luau::query<AstStatFunction>(block))
 * 3. Luau::query<AstExprBinary>(block, {nth<AstExprBinary>(2)})
 */
template<typename T, int N = 1>
T* query(AstNode* node, const std::vector<Nth>& nths = {nth<T>(N)})
{
    static_assert(std::is_base_of_v<AstNode, T>, "T must be a derived class of AstNode");

    // If a nested query call fails to find the node in question, subsequent calls can propagate rather than trying to do more.
    // This supports `query(query(...))`

    for (Nth nth : nths)
    {
        if (!node)
            return nullptr;

        FindNthOccurenceOf finder{nth};
        node->visit(&finder);
        node = finder.theNode;
    }

    return node ? node->as<T>() : nullptr;
}

} // namespace Luau
