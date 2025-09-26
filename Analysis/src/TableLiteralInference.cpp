// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TableLiteralInference.h"

#include "Luau/Ast.h"
#include "Luau/Common.h"
#include "Luau/HashUtil.h"
#include "Luau/Simplify.h"
#include "Luau/Subtyping.h"
#include "Luau/Type.h"
#include "Luau/ToString.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"

LUAU_FASTFLAGVARIABLE(LuauPushTypeConstraintIntersection)

namespace Luau
{

namespace
{

struct BidirectionalTypePusher
{

    NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes;
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astExpectedTypes;

    NotNull<ConstraintSolver> solver;
    NotNull<const Constraint> constraint;
    NotNull<Unifier2> unifier;
    NotNull<Subtyping> subtyping;

    std::vector<IncompleteInference> incompleteInferences;

    DenseHashSet<std::pair<TypeId, const AstExpr*>, PairHash<TypeId, const AstExpr*>> seen{{nullptr, nullptr}};

    BidirectionalTypePusher(
        NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes,
        NotNull<DenseHashMap<const AstExpr*, TypeId>> astExpectedTypes,
        NotNull<ConstraintSolver> solver,
        NotNull<const Constraint> constraint,
        NotNull<Unifier2> unifier,
        NotNull<Subtyping> subtyping
    )
        : astTypes{astTypes}
        , astExpectedTypes{astExpectedTypes}
        , solver{solver}
        , constraint{constraint}
        , unifier{unifier}
        , subtyping{subtyping}
    {
    }

    TypeId pushType(TypeId expectedType, const AstExpr* expr)
    {
        if (!astTypes->contains(expr))
        {
            LUAU_ASSERT(false);
            return solver->builtinTypes->errorType;
        }

        TypeId exprType = *astTypes->find(expr);

        if (FFlag::LuauPushTypeConstraintIntersection)
        {
            if (seen.contains({expectedType, expr}))
                return exprType;
            seen.insert({expectedType, expr});
        }

        expectedType = follow(expectedType);
        exprType = follow(exprType);

        // NOTE: We cannot block on free types here, as that trivially means
        // any recursive function would have a cycle, consider:
        //
        //  local function fact(n)
        //      return if n < 2 then 1 else n * fact(n - 1)
        //  end
        //
        // We'll have a cycle between trying to push `fact`'s type into its
        // arguments and generalizing `fact`.

        if (auto tfit = get<TypeFunctionInstanceType>(expectedType); tfit && tfit->state == TypeFunctionInstanceState::Unsolved)
        {
            incompleteInferences.push_back(IncompleteInference{expectedType, exprType, expr});
            return exprType;
        }

        if (is<BlockedType, PendingExpansionType>(expectedType))
        {
            incompleteInferences.push_back(IncompleteInference{expectedType, exprType, expr});
            return exprType;
        }

        if (is<AnyType, UnknownType>(expectedType))
            return exprType;

        (*astExpectedTypes)[expr] = expectedType;

        if (auto group = expr->as<AstExprGroup>())
        {
            pushType(expectedType, group->expr);
            return exprType;
        }

        if (auto ternary = expr->as<AstExprIfElse>())
        {
            pushType(expectedType, ternary->trueExpr);
            pushType(expectedType, ternary->falseExpr);
            return exprType;
        }

        if (!isLiteral(expr))
            // NOTE: For now we aren't using the result of this function, so
            // just return the original expression type.
            return exprType;

        if (expr->is<AstExprConstantString>())
        {
            auto ft = get<FreeType>(exprType);
            if (ft && get<SingletonType>(ft->lowerBound) && fastIsSubtype(solver->builtinTypes->stringType, ft->upperBound) &&
                fastIsSubtype(ft->lowerBound, solver->builtinTypes->stringType))
            {
                // if the upper bound is a subtype of the expected type, we can push the expected type in
                Relation upperBoundRelation = relate(ft->upperBound, expectedType);
                if (upperBoundRelation == Relation::Subset || upperBoundRelation == Relation::Coincident)
                {
                    emplaceType<BoundType>(asMutable(exprType), expectedType);
                    solver->unblock(exprType, expr->location);
                    return exprType;
                }

                // likewise, if the lower bound is a subtype, we can force the expected type in
                // if this is the case and the previous relation failed, it means that the primitive type
                // constraint was going to have to select the lower bound for this type anyway.
                Relation lowerBoundRelation = relate(ft->lowerBound, expectedType);
                if (lowerBoundRelation == Relation::Subset || lowerBoundRelation == Relation::Coincident)
                {
                    emplaceType<BoundType>(asMutable(exprType), expectedType);
                    solver->unblock(exprType, expr->location);
                    return exprType;
                }
            }
        }
        else if (expr->is<AstExprConstantBool>())
        {
            auto ft = get<FreeType>(exprType);
            if (ft && get<SingletonType>(ft->lowerBound) && fastIsSubtype(solver->builtinTypes->booleanType, ft->upperBound) &&
                fastIsSubtype(ft->lowerBound, solver->builtinTypes->booleanType))
            {
                // if the upper bound is a subtype of the expected type, we can push the expected type in
                Relation upperBoundRelation = relate(ft->upperBound, expectedType);
                if (upperBoundRelation == Relation::Subset || upperBoundRelation == Relation::Coincident)
                {
                    emplaceType<BoundType>(asMutable(exprType), expectedType);
                    solver->unblock(exprType, expr->location);
                    return exprType;
                }

                // likewise, if the lower bound is a subtype, we can force the expected type in
                // if this is the case and the previous relation failed, it means that the primitive type
                // constraint was going to have to select the lower bound for this type anyway.
                Relation lowerBoundRelation = relate(ft->lowerBound, expectedType);
                if (lowerBoundRelation == Relation::Subset || lowerBoundRelation == Relation::Coincident)
                {
                    emplaceType<BoundType>(asMutable(exprType), expectedType);
                    solver->unblock(exprType, expr->location);
                    return exprType;
                }
            }
        }

        if (expr->is<AstExprConstantString>() || expr->is<AstExprConstantNumber>() || expr->is<AstExprConstantBool>() ||
            expr->is<AstExprConstantNil>())
        {
            if (auto ft = get<FreeType>(exprType); ft && fastIsSubtype(ft->upperBound, expectedType))
            {
                emplaceType<BoundType>(asMutable(exprType), expectedType);
                solver->unblock(exprType, expr->location);
                return exprType;
            }

            Relation r = relate(exprType, expectedType);
            if (r == Relation::Coincident || r == Relation::Subset)
                return expectedType;

            return exprType;
        }


        if (expr->is<AstExprFunction>())
            // TODO: Push argument / return types into the lambda.
            return exprType;

        // TODO: CLI-169235: This probably ought to use the same logic as
        // `index` to determine what the type of a given member is.
        if (auto exprTable = expr->as<AstExprTable>())
        {
            const TableType* expectedTableTy = get<TableType>(expectedType);

            if (!expectedTableTy)
            {
                if (auto utv = get<UnionType>(expectedType))
                {
                    std::vector<TypeId> parts{begin(utv), end(utv)};

                    std::optional<TypeId> tt = extractMatchingTableType(parts, exprType, solver->builtinTypes);

                    if (tt)
                        (void)pushType(*tt, expr);
                }
                else if (auto itv = get<IntersectionType>(expectedType); FFlag::LuauPushTypeConstraintIntersection && itv)
                {
                    for (const auto part : itv)
                        (void)pushType(part, expr);

                    // Reset the expected type for this expression prior,
                    // otherwise the expected type will be the last part
                    // of the intersection, which does not seem ideal.
                    (*astExpectedTypes)[expr] = expectedType;
                }

                return exprType;
            }

            for (const AstExprTable::Item& item : exprTable->items)
            {
                if (isRecord(item))
                {
                    const AstArray<char>& s = item.key->as<AstExprConstantString>()->value;
                    std::string keyStr{s.data, s.data + s.size};
                    auto it = expectedTableTy->props.find(keyStr);

                    if (it == expectedTableTy->props.end())
                    {
                        // If we have some type:
                        //
                        //  { [string]: T }
                        //
                        // ... that we're trying to push into ...
                        //
                        //  { foo = bar }
                        //
                        // Then the intent is probably to push `T` into `bar`.
                        if (expectedTableTy->indexer && fastIsSubtype(solver->builtinTypes->stringType, expectedTableTy->indexer->indexType))
                            (void)pushType(expectedTableTy->indexer->indexResultType, item.value);

                        // If it's just an extra property and the expected type
                        // has no indexer, there's no work to do here.
                        continue;
                    }

                    LUAU_ASSERT(it != expectedTableTy->props.end());

                    const Property& expectedProp = it->second;

                    if (expectedProp.readTy)
                        (void)pushType(*expectedProp.readTy, item.value);

                    // NOTE: We do *not* add to the potential indexer types here.
                    // I think this is correct to support something like:
                    //
                    //  { [string]: number, foo: boolean }
                    //
                    // NOTE: We also do nothing for write properties.
                }
                else if (item.kind == AstExprTable::Item::List)
                {
                    if (expectedTableTy->indexer)
                    {
                        unifier->unify(expectedTableTy->indexer->indexType, solver->builtinTypes->numberType);
                        (void)pushType(expectedTableTy->indexer->indexResultType, item.value);
                    }
                }
                else if (item.kind == AstExprTable::Item::General)
                {

                    // We have { ..., [blocked] : somePropExpr, ...}
                    // If blocked resolves to a string, we will then take care of this above
                    // If it resolves to some other kind of expression, we don't have a way of folding this information into indexer
                    // because there is no named prop to remove
                    // We should just block here
                    if (expectedTableTy->indexer)
                    {
                        (void)pushType(expectedTableTy->indexer->indexType, item.key);
                        (void)pushType(expectedTableTy->indexer->indexResultType, item.value);
                    }
                }
                else
                    LUAU_ASSERT(!"Unexpected");
            }
        }

        return exprType;
    }
};
} // namespace

PushTypeResult pushTypeInto(
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes,
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astExpectedTypes,
    NotNull<ConstraintSolver> solver,
    NotNull<const Constraint> constraint,
    NotNull<Unifier2> unifier,
    NotNull<Subtyping> subtyping,
    TypeId expectedType,
    const AstExpr* expr
)
{
    BidirectionalTypePusher btp{astTypes, astExpectedTypes, solver, constraint, unifier, subtyping};
    (void)btp.pushType(expectedType, expr);
    return {std::move(btp.incompleteInferences)};
}

} // namespace Luau
