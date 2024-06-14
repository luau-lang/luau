// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Ast.h"
#include "Luau/Normalize.h"
#include "Luau/Simplify.h"
#include "Luau/Type.h"
#include "Luau/ToString.h"
#include "Luau/TypeArena.h"
#include "Luau/Unifier2.h"

namespace Luau
{

static bool isLiteral(const AstExpr* expr)
{
    return (expr->is<AstExprTable>() || expr->is<AstExprFunction>() || expr->is<AstExprConstantNumber>() || expr->is<AstExprConstantString>() ||
            expr->is<AstExprConstantBool>() || expr->is<AstExprConstantNil>());
}

// A fast approximation of subTy <: superTy
static bool fastIsSubtype(TypeId subTy, TypeId superTy)
{
    Relation r = relate(superTy, subTy);
    return r == Relation::Coincident || r == Relation::Superset;
}

static bool isRecord(const AstExprTable::Item& item)
{
    if (item.kind == AstExprTable::Item::Record)
        return true;
    else if (item.kind == AstExprTable::Item::General && item.key->is<AstExprConstantString>())
        return true;
    else
        return false;
}

static std::optional<TypeId> extractMatchingTableType(std::vector<TypeId>& tables, TypeId exprType, NotNull<BuiltinTypes> builtinTypes)
{
    if (tables.empty())
        return std::nullopt;

    const TableType* exprTable = get<TableType>(follow(exprType));
    if (!exprTable)
        return std::nullopt;

    size_t tableCount = 0;
    std::optional<TypeId> firstTable;

    for (TypeId ty : tables)
    {
        ty = follow(ty);
        if (auto tt = get<TableType>(ty))
        {
            // If the expected table has a key whose type is a string or boolean
            // singleton and the corresponding exprType property does not match,
            // then skip this table.

            if (!firstTable)
                firstTable = ty;
            ++tableCount;

            for (const auto& [name, expectedProp] : tt->props)
            {
                if (!expectedProp.readTy)
                    continue;

                const TypeId expectedType = follow(*expectedProp.readTy);

                auto st = get<SingletonType>(expectedType);
                if (!st)
                    continue;

                auto it = exprTable->props.find(name);
                if (it == exprTable->props.end())
                    continue;

                const auto& [_name, exprProp] = *it;

                if (!exprProp.readTy)
                    continue;

                const TypeId propType = follow(*exprProp.readTy);

                const FreeType* ft = get<FreeType>(propType);

                if (ft && get<SingletonType>(ft->lowerBound))
                {
                    if (fastIsSubtype(builtinTypes->booleanType, ft->upperBound) && fastIsSubtype(expectedType, builtinTypes->booleanType))
                    {
                        return ty;
                    }

                    if (fastIsSubtype(builtinTypes->stringType, ft->upperBound) && fastIsSubtype(expectedType, ft->lowerBound))
                    {
                        return ty;
                    }
                }
            }
        }
    }

    if (tableCount == 1)
    {
        LUAU_ASSERT(firstTable);
        return firstTable;
    }

    return std::nullopt;
}

TypeId matchLiteralType(NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes, NotNull<DenseHashMap<const AstExpr*, TypeId>> astExpectedTypes,
    NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, NotNull<Unifier2> unifier, TypeId expectedType, TypeId exprType,
    const AstExpr* expr, std::vector<TypeId>& toBlock)
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
        return exprType;

    expectedType = follow(expectedType);
    exprType = follow(exprType);

    if (get<AnyType>(expectedType) || get<UnknownType>(expectedType))
    {
        // "Narrowing" to unknown or any is not going to do anything useful.
        return exprType;
    }

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

    // TODO: lambdas

    if (auto exprTable = expr->as<AstExprTable>())
    {
        TableType* tableTy = getMutable<TableType>(exprType);
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
                    TypeId res = matchLiteralType(astTypes, astExpectedTypes, builtinTypes, arena, unifier, *tt, exprType, expr, toBlock);

                    parts.push_back(res);
                    return arena->addType(UnionType{std::move(parts)});
                }
            }

            return exprType;
        }

        for (const AstExprTable::Item& item : exprTable->items)
        {
            if (isRecord(item))
            {
                const AstArray<char>& s = item.key->as<AstExprConstantString>()->value;
                std::string keyStr{s.data, s.data + s.size};
                auto it = tableTy->props.find(keyStr);
                LUAU_ASSERT(it != tableTy->props.end());

                Property& prop = it->second;

                // Table literals always initially result in shared read-write types
                LUAU_ASSERT(prop.isShared());
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

                        TypeId matchedType = matchLiteralType(astTypes, astExpectedTypes, builtinTypes, arena, unifier,
                            expectedTableTy->indexer->indexResultType, propTy, item.value, toBlock);

                        if (tableTy->indexer)
                            unifier->unify(matchedType, tableTy->indexer->indexResultType);
                        else
                            tableTy->indexer = TableIndexer{expectedTableTy->indexer->indexType, matchedType};

                        tableTy->props.erase(keyStr);
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
                    matchedType =
                        matchLiteralType(astTypes, astExpectedTypes, builtinTypes, arena, unifier, *expectedReadTy, propTy, item.value, toBlock);
                    prop.readTy = matchedType;
                    prop.writeTy = matchedType;
                }
                else if (expectedReadTy)
                {
                    matchedType =
                        matchLiteralType(astTypes, astExpectedTypes, builtinTypes, arena, unifier, *expectedReadTy, propTy, item.value, toBlock);
                    prop.readTy = matchedType;
                    prop.writeTy.reset();
                }
                else if (expectedWriteTy)
                {
                    matchedType =
                        matchLiteralType(astTypes, astExpectedTypes, builtinTypes, arena, unifier, *expectedWriteTy, propTy, item.value, toBlock);
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
            }
            else if (item.kind == AstExprTable::Item::List)
            {
                LUAU_ASSERT(tableTy->indexer);

                if (expectedTableTy->indexer)
                {
                    const TypeId* propTy = astTypes->find(item.value);
                    LUAU_ASSERT(propTy);

                    unifier->unify(expectedTableTy->indexer->indexType, builtinTypes->numberType);
                    TypeId matchedType = matchLiteralType(astTypes, astExpectedTypes, builtinTypes, arena, unifier,
                        expectedTableTy->indexer->indexResultType, *propTy, item.value, toBlock);

                    // if the index result type is the prop type, we can replace it with the matched type here.
                    if (tableTy->indexer->indexResultType == *propTy)
                        tableTy->indexer->indexResultType = matchedType;
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
                if (get<BlockedType>(tKey))
                    toBlock.push_back(tKey);

                const TypeId* propTy = astTypes->find(item.value);
                LUAU_ASSERT(propTy);
                TypeId tProp = follow(*propTy);
                if (get<BlockedType>(tProp))
                    toBlock.push_back(tProp);
            }
            else
                LUAU_ASSERT(!"Unexpected");
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
    }

    return exprType;
}

} // namespace Luau
