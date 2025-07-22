// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/DenseHash.h"
#include "Luau/NotNull.h"
#include "Luau/Type.h"

namespace Luau
{

struct ExpectedTypeVisitor : public AstVisitor
{

    explicit ExpectedTypeVisitor(
        NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes,
        NotNull<DenseHashMap<const AstExpr*, TypeId>> astExpectedTypes,
        NotNull<DenseHashMap<const AstType*, TypeId>> astResolvedTypes,
        NotNull<TypeArena> arena,
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<Scope> rootScope
    );

    // When we have an assignment, we grab the type of the left-hand-side
    // and we use it to inform what the type of the right-hand-side ought
    // to be. This is important for something like:
    //
    //
    //    local function foobar(tbl: { prop: boolean })
    //        tbl.prop = [autocomplete here]
    //    end
    //
    // ... where the right hand side _must_ be a subtype of `boolean`
    bool visit(AstStatAssign* stat) override;

    // Similar to an assignment, we can apply expected types to the
    // right-hand-side of a local based on the annotated type of the
    // left-hand-side.
    bool visit(AstStatLocal* stat) override;

    // Compound assignments have the curious property that they do not change
    // type state, so we can use the left-hand-side to inform the
    // right-hand-side.
    bool visit(AstStatCompoundAssign* stat) override;

    // When we are returning something, and we've inferred a return type (or have
    // a written return type), then we need to apply the expected types to the
    // return type expression.
    bool visit(AstStatReturn* stat) override;

    // When we have a function call, we can apply expected types to all the
    // parameters.
    bool visit(AstExprCall* expr) override;

    // If we have an expression like `A[B]`, then the expected type of B is
    // clearly the properties and indexer of `A`.
    bool visit(AstExprIndexExpr* expr) override;

    // If we have an expression of type:
    //
    //   return X :: Y
    //
    // Then surely the expected type of `X` is `Y`
    bool visit(AstExprTypeAssertion* expr) override;

private:
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes;
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astExpectedTypes;
    NotNull<DenseHashMap<const AstType*, TypeId>> astResolvedTypes;
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<Scope> rootScope;

    void applyExpectedType(const TypeId expectedType, const AstExpr* expr);
};


} // namespace Luau