// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TableLiteralInference.h"

#include "Luau/Ast.h"
#include "Luau/Common.h"
#include "Luau/Normalize.h"
#include "Luau/Simplify.h"
#include "Luau/Subtyping.h"
#include "Luau/Type.h"
#include "Luau/ToString.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"

namespace Luau
{

TypeId matchLiteralType(
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes,
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astExpectedTypes,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    NotNull<Unifier2> unifier,
    NotNull<Subtyping> subtyping,
    TypeId expectedType,
    TypeId exprType,
    const AstExpr* expr,
    std::vector<TypeId>& toBlock
)
{
    /*
     * Table types that arise from literal table expressions have some
     * properties that make this algorithm much simpler.
     *
     * Most importantly, the parts of the type that arise directly from the
     * table expression are guaranteed to be acyclic.  This means we can do all
     * kinds of naive depth first traversal shenanigans and not worry about
     * nasty details like aliasing or reentrancy.
     *
     * We are therefore completely free to mutate these portions of the
     * TableType however we choose!  We'll take advantage of this property to do
     * things like replace explicit named properties with indexers as required
     * by the expected type.
     */

    if (!isLiteral(expr))
    {
        auto result = subtyping->isSubtype(/*subTy=*/exprType, /*superTy=*/expectedType, unifier->scope);
        return result.isSubtype ? expectedType : exprType;
    }

    expectedType = follow(expectedType);
    exprType = follow(exprType);


    // The intent of `matchLiteralType` is to upcast values when it's safe
    // to do so. it's always safe to upcast to `any` or `unknown`, so we
    // can unconditionally do so here.
    if (is<AnyType, UnknownType>(expectedType))
        return expectedType;



    if (expr->is<AstExprConstantString>())
    {
        auto ft = get<FreeType>(exprType);
        if (ft && get<SingletonType>(ft->lowerBound) && fastIsSubtype(builtinTypes->stringType, ft->upperBound) &&
            fastIsSubtype(ft->lowerBound, builtinTypes->stringType))
        {
            // if the upper bound is a subtype of the expected type, we can push the expected type in
            Relation upperBoundRelation = relate(ft->upperBound, expectedType);
            if (upperBoundRelation == Relation::Subset || upperBoundRelation == Relation::Coincident)
            {
                emplaceType<BoundType>(asMutable(exprType), expectedType);
                return exprType;
            }

            // likewise, if the lower bound is a subtype, we can force the expected type in
            // if this is the case and the previous relation failed, it means that the primitive type
            // constraint was going to have to select the lower bound for this type anyway.
            Relation lowerBoundRelation = relate(ft->lowerBound, expectedType);
            if (lowerBoundRelation == Relation::Subset || lowerBoundRelation == Relation::Coincident)
            {
                emplaceType<BoundType>(asMutable(exprType), expectedType);
                return exprType;
            }
        }
    }
    else if (expr->is<AstExprConstantBool>())
    {
        auto ft = get<FreeType>(exprType);
        if (ft && get<SingletonType>(ft->lowerBound) && fastIsSubtype(builtinTypes->booleanType, ft->upperBound) &&
            fastIsSubtype(ft->lowerBound, builtinTypes->booleanType))
        {
            // if the upper bound is a subtype of the expected type, we can push the expected type in
            Relation upperBoundRelation = relate(ft->upperBound, expectedType);
            if (upperBoundRelation == Relation::Subset || upperBoundRelation == Relation::Coincident)
            {
                emplaceType<BoundType>(asMutable(exprType), expectedType);
                return exprType;
            }

            // likewise, if the lower bound is a subtype, we can force the expected type in
            // if this is the case and the previous relation failed, it means that the primitive type
            // constraint was going to have to select the lower bound for this type anyway.
            Relation lowerBoundRelation = relate(ft->lowerBound, expectedType);
            if (lowerBoundRelation == Relation::Subset || lowerBoundRelation == Relation::Coincident)
            {
                emplaceType<BoundType>(asMutable(exprType), expectedType);
                return exprType;
            }
        }
    }

    if (expr->is<AstExprConstantString>() || expr->is<AstExprConstantNumber>() || expr->is<AstExprConstantBool>() || expr->is<AstExprConstantNil>())
    {
        if (auto ft = get<FreeType>(exprType); ft && fastIsSubtype(ft->upperBound, expectedType))
        {
            emplaceType<BoundType>(asMutable(exprType), expectedType);
            return exprType;
        }

        Relation r = relate(exprType, expectedType);
        if (r == Relation::Coincident || r == Relation::Subset)
            return expectedType;

        return exprType;
    }


    if (expr->is<AstExprFunction>())
    {
        // TODO: Push argument / return types into the lambda. For now, just do
        // the non-literal thing: check for a subtype and upcast if valid.
        auto result = subtyping->isSubtype(/*subTy=*/exprType, /*superTy=*/expectedType, unifier->scope);
        return result.isSubtype ? expectedType : exprType;
    }

    if (auto exprTable = expr->as<AstExprTable>())
    {
        TableType* const tableTy = getMutable<TableType>(exprType);

        // This can occur if we have an expression like:
        //
        //  { x = {}, x = 42 }
        //
        // The type of this will be `{ x: number }`
        if (!tableTy)
            return exprType;

        LUAU_ASSERT(tableTy);

        const TableType* expectedTableTy = get<TableType>(expectedType);

        if (!expectedTableTy)
        {
            if (auto utv = get<UnionType>(expectedType))
            {
                std::vector<TypeId> parts{begin(utv), end(utv)};

                std::optional<TypeId> tt = extractMatchingTableType(parts, exprType, builtinTypes);

                if (tt)
                {
                    TypeId res = matchLiteralType(astTypes, astExpectedTypes, builtinTypes, arena, unifier, subtyping, *tt, exprType, expr, toBlock);
                    parts.push_back(res);
                    return arena->addType(UnionType{std::move(parts)});
                }
            }

            return exprType;
        }

        DenseHashSet<AstExprConstantString*> keysToDelete{nullptr};

        DenseHashSet<TypeId> indexerKeyTypes{nullptr};
        DenseHashSet<TypeId> indexerValueTypes{nullptr};

        for (const AstExprTable::Item& item : exprTable->items)
        {
            if (isRecord(item))
            {
                const AstArray<char>& s = item.key->as<AstExprConstantString>()->value;
                std::string keyStr{s.data, s.data + s.size};
                auto it = tableTy->props.find(keyStr);

                // This can occur, potentially, if we are re-entrant.
                if (it == tableTy->props.end())
                    continue;

                LUAU_ASSERT(it != tableTy->props.end());

                Property& prop = it->second;

                // If the property is write-only, do nothing.
                if (prop.isWriteOnly())
                    continue;

                TypeId propTy = *prop.readTy;

                auto it2 = expectedTableTy->props.find(keyStr);

                if (it2 == expectedTableTy->props.end())
                {
                    // expectedType may instead have an indexer.  This is
                    // kind of interesting because it means we clip the prop
                    // from the exprType and fold it into the indexer.
                    if (expectedTableTy->indexer && isString(expectedTableTy->indexer->indexType))
                    {
                        (*astExpectedTypes)[item.key] = expectedTableTy->indexer->indexType;
                        (*astExpectedTypes)[item.value] = expectedTableTy->indexer->indexResultType;

                        TypeId matchedType = matchLiteralType(
                            astTypes,
                            astExpectedTypes,
                            builtinTypes,
                            arena,
                            unifier,
                            subtyping,
                            expectedTableTy->indexer->indexResultType,
                            propTy,
                            item.value,
                            toBlock
                        );

                        indexerKeyTypes.insert(arena->addType(SingletonType{StringSingleton{keyStr}}));
                        indexerValueTypes.insert(matchedType);

                        keysToDelete.insert(item.key->as<AstExprConstantString>());
                    }

                    // If it's just an extra property and the expected type
                    // has no indexer, there's no work to do here.

                    continue;
                }

                LUAU_ASSERT(it2 != expectedTableTy->props.end());

                const Property& expectedProp = it2->second;

                std::optional<TypeId> expectedReadTy = expectedProp.readTy;
                std::optional<TypeId> expectedWriteTy = expectedProp.writeTy;

                TypeId matchedType = nullptr;

                // Important optimization: If we traverse into the read and
                // write types separately even when they are shared, we go
                // quadratic in a hurry.
                if (expectedProp.isShared())
                {
                    matchedType = matchLiteralType(
                        astTypes, astExpectedTypes, builtinTypes, arena, unifier, subtyping, *expectedReadTy, propTy, item.value, toBlock
                    );
                    prop.readTy = matchedType;
                    prop.writeTy = matchedType;
                }
                else if (expectedReadTy)
                {
                    matchedType = matchLiteralType(
                        astTypes, astExpectedTypes, builtinTypes, arena, unifier, subtyping, *expectedReadTy, propTy, item.value, toBlock
                    );
                    prop.readTy = matchedType;
                    prop.writeTy.reset();
                }
                else if (expectedWriteTy)
                {
                    matchedType = matchLiteralType(
                        astTypes, astExpectedTypes, builtinTypes, arena, unifier, subtyping, *expectedWriteTy, propTy, item.value, toBlock
                    );
                    prop.readTy.reset();
                    prop.writeTy = matchedType;
                }
                else
                {
                    // Also important: It is presently the case that all
                    // table properties are either read-only, or have the
                    // same read and write types.
                    LUAU_ASSERT(!"Should be unreachable");
                }

                LUAU_ASSERT(prop.readTy || prop.writeTy);

                LUAU_ASSERT(matchedType);

                (*astExpectedTypes)[item.value] = matchedType;
                // NOTE: We do *not* add to the potential indexer types here.
                // I think this is correct to support something like:
                //
                //  { [string]: number, foo: boolean }
                //
            }
            else if (item.kind == AstExprTable::Item::List)
            {
                if (expectedTableTy->indexer)
                {
                    const TypeId* propTy = astTypes->find(item.value);
                    LUAU_ASSERT(propTy);

                    unifier->unify(expectedTableTy->indexer->indexType, builtinTypes->numberType);
                    TypeId matchedType = matchLiteralType(
                        astTypes,
                        astExpectedTypes,
                        builtinTypes,
                        arena,
                        unifier,
                        subtyping,
                        expectedTableTy->indexer->indexResultType,
                        *propTy,
                        item.value,
                        toBlock
                    );

                    indexerKeyTypes.insert(builtinTypes->numberType);
                    indexerValueTypes.insert(matchedType);
                }
            }
            else if (item.kind == AstExprTable::Item::General)
            {

                // We have { ..., [blocked] : somePropExpr, ...}
                // If blocked resolves to a string, we will then take care of this above
                // If it resolves to some other kind of expression, we don't have a way of folding this information into indexer
                // because there is no named prop to remove
                // We should just block here
                const TypeId* keyTy = astTypes->find(item.key);
                LUAU_ASSERT(keyTy);
                TypeId tKey = follow(*keyTy);
                LUAU_ASSERT(!is<BlockedType>(tKey));
                const TypeId* propTy = astTypes->find(item.value);
                LUAU_ASSERT(propTy);
                TypeId tProp = follow(*propTy);
                LUAU_ASSERT(!is<BlockedType>(tProp));
                // Populate expected types for non-string keys declared with [] (the code below will handle the case where they are strings)
                if (!item.key->as<AstExprConstantString>() && expectedTableTy->indexer)
                    (*astExpectedTypes)[item.key] = expectedTableTy->indexer->indexType;

                indexerKeyTypes.insert(tKey);
                indexerValueTypes.insert(tProp);
            }
            else
                LUAU_ASSERT(!"Unexpected");
        }

        for (const auto& key : keysToDelete)
        {
            const AstArray<char>& s = key->value;
            std::string keyStr{s.data, s.data + s.size};
            tableTy->props.erase(keyStr);
        }

        // Keys that the expectedType says we should have, but that aren't
        // specified by the AST fragment.
        //
        // If any such keys are options, then we'll add them to the expression
        // type.
        //
        // We use std::optional<std::string> here because the empty string is a
        // perfectly reasonable value to insert into the set.  We'll use
        // std::nullopt as our sentinel value.
        Set<std::optional<std::string>> missingKeys{{}};
        for (const auto& [name, _] : expectedTableTy->props)
            missingKeys.insert(name);

        for (const AstExprTable::Item& item : exprTable->items)
        {
            if (item.key)
            {
                if (const auto str = item.key->as<AstExprConstantString>())
                {
                    missingKeys.erase(std::string(str->value.data, str->value.size));
                }
            }
        }

        for (const auto& key : missingKeys)
        {
            LUAU_ASSERT(key.has_value());

            auto it = expectedTableTy->props.find(*key);
            LUAU_ASSERT(it != expectedTableTy->props.end());

            const Property& expectedProp = it->second;

            Property exprProp;

            if (expectedProp.readTy && isOptional(*expectedProp.readTy))
                exprProp.readTy = *expectedProp.readTy;
            if (expectedProp.writeTy && isOptional(*expectedProp.writeTy))
                exprProp.writeTy = *expectedProp.writeTy;

            // If the property isn't actually optional, do nothing.
            if (exprProp.readTy || exprProp.writeTy)
                tableTy->props[*key] = std::move(exprProp);
        }

        // If the expected table has an indexer, then the provided table can
        // have one too.
        // TODO: If the expected table also has an indexer, we might want to
        // push the expected indexer's types into it.
        if (expectedTableTy->indexer)
        {
            if (indexerValueTypes.size() > 0 && indexerKeyTypes.size() > 0)
            {
                TypeId inferredKeyType = builtinTypes->neverType;
                TypeId inferredValueType = builtinTypes->neverType;
                for (auto kt : indexerKeyTypes)
                {
                    auto simplified = simplifyUnion(builtinTypes, arena, inferredKeyType, kt);
                    inferredKeyType = simplified.result;
                }
                for (auto vt : indexerValueTypes)
                {
                    auto simplified = simplifyUnion(builtinTypes, arena, inferredValueType, vt);
                    inferredValueType = simplified.result;
                }
                tableTy->indexer = TableIndexer{inferredKeyType, inferredValueType};
                auto keyCheck = subtyping->isSubtype(inferredKeyType, expectedTableTy->indexer->indexType, unifier->scope);
                if (keyCheck.isSubtype)
                    tableTy->indexer->indexType = expectedTableTy->indexer->indexType;
                auto valueCheck = subtyping->isSubtype(inferredValueType, expectedTableTy->indexer->indexResultType, unifier->scope);
                if (valueCheck.isSubtype)
                    tableTy->indexer->indexResultType = expectedTableTy->indexer->indexResultType;
            }
            else
                LUAU_ASSERT(indexerKeyTypes.empty() && indexerValueTypes.empty());
        }
        else
        {
            if (expectedTableTy->indexer && !tableTy->indexer)
            {
                tableTy->indexer = expectedTableTy->indexer;
            }
        }
    }

    return exprType;
}

} // namespace Luau
