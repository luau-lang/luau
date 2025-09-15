// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ExpectedTypeVisitor.h"

#include "Luau/Scope.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeIds.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/VisitType.h"

namespace Luau
{

ExpectedTypeVisitor::ExpectedTypeVisitor(
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes,
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astExpectedTypes,
    NotNull<DenseHashMap<const AstType*, TypeId>> astResolvedTypes,
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> rootScope
)
    : astTypes(astTypes)
    , astExpectedTypes(astExpectedTypes)
    , astResolvedTypes(astResolvedTypes)
    , arena(arena)
    , builtinTypes(builtinTypes)
    , rootScope(rootScope)
{
}

bool ExpectedTypeVisitor::visit(AstStatAssign* stat)
{
    for (size_t idx = 0; idx < std::min(stat->vars.size, stat->values.size); idx++)
    {
        if (auto lhsType = astTypes->find(stat->vars.data[idx]))
            applyExpectedType(*lhsType, stat->values.data[idx]);
    }
    return true;
}

bool ExpectedTypeVisitor::visit(AstStatLocal* stat)
{
    for (size_t idx = 0; idx < std::min(stat->vars.size, stat->values.size); idx++)
    {
        if (auto annot = astResolvedTypes->find(stat->vars.data[idx]->annotation))
            applyExpectedType(*annot, stat->values.data[idx]);
    }
    return true;
}

bool ExpectedTypeVisitor::visit(AstStatCompoundAssign* stat)
{
    if (auto lhsType = astTypes->find(stat->var))
        applyExpectedType(*lhsType, stat->value);
    return true;
}

bool ExpectedTypeVisitor::visit(AstStatReturn* stat)
{
    auto scope = rootScope->findNarrowestScopeContaining(stat->location);

    auto it = begin(scope->returnType);
    size_t idx = 0;

    while (idx < stat->list.size && it != end(scope->returnType))
    {
        applyExpectedType(*it, stat->list.data[idx]);
        it++;
        idx++;
    }

    return true;
}

namespace
{

struct IndexerIndexCollector : public TypeOnceVisitor
{
    NotNull<TypeIds> indexes;

    explicit IndexerIndexCollector(NotNull<TypeIds> indexes)
        : TypeOnceVisitor("IndexerIndexCollector", /* skipBoundTypes */ true)
        , indexes(indexes)
    {
    }

    bool visit(TypeId ty) override
    {
        indexes->insert(ty);
        return false;
    }

    bool visit(TypeId, const UnionType&) override
    {
        return true;
    }

    bool visit(TypeId, const IntersectionType&) override
    {
        return true;
    }
};

struct IndexCollector : public TypeOnceVisitor
{
    NotNull<TypeArena> arena;
    TypeIds indexes;

    explicit IndexCollector(NotNull<TypeArena> arena)
        : TypeOnceVisitor("IndexCollector", /* skipBoundTypes */ true)
        , arena(arena)
    {
    }

    bool visit(TypeId ty) override
    {
        return false;
    }

    bool visit(TypeId, const UnionType&) override
    {
        return true;
    }

    bool visit(TypeId, const IntersectionType&) override
    {
        return true;
    }

    bool visit(TypeId, const TableType& ttv) override
    {
        for (const auto& [name, _] : ttv.props)
            indexes.insert(arena->addType(SingletonType{StringSingleton{name}}));

        if (ttv.indexer)
        {
            IndexerIndexCollector iic{NotNull{&indexes}};
            iic.traverse(ttv.indexer->indexType);
        }

        return false;
    }
};

} // namespace

bool ExpectedTypeVisitor::visit(AstExprIndexExpr* expr)
{
    if (auto ty = astTypes->find(expr->expr))
    {
        IndexCollector ic{arena};
        ic.traverse(*ty);
        if (ic.indexes.size() > 1)
        {
            applyExpectedType(arena->addType(UnionType{ic.indexes.take()}), expr->index);
        }
        else if (ic.indexes.size() == 1)
        {
            applyExpectedType(*ic.indexes.begin(), expr->index);
        }
    }

    return true;
}


bool ExpectedTypeVisitor::visit(AstExprCall* expr)
{
    auto ty = astTypes->find(expr->func);
    if (!ty)
        return true;

    const FunctionType* ftv = get<FunctionType>(follow(*ty));

    // FIXME: Bidirectional type checking of overloaded functions is not yet
    // supported, which means we *also* do not provide autocomplete for
    // the arguments of overloaded functions.
    if (!ftv)
        return true;

    auto it = begin(ftv->argTypes);
    size_t idx = 0;

    if (expr->self && it != end(ftv->argTypes))
    {
        // If we have a `foo:bar(...)` call, then the first type in the arg
        // pack will be the type of `self`, so we just skip that.
        it++;
    }

    while (idx < expr->args.size && it != end(ftv->argTypes))
    {
        applyExpectedType(*it, expr->args.data[idx]);
        it++;
        idx++;
    }

    return true;
}

bool ExpectedTypeVisitor::visit(AstExprTypeAssertion* expr)
{
    if (auto annot = astResolvedTypes->find(expr->annotation))
        applyExpectedType(*annot, expr->expr);

    return true;
}

void ExpectedTypeVisitor::applyExpectedType(TypeId expectedType, const AstExpr* expr)
{
    expectedType = follow(expectedType);

    // No matter what, we set the expected type of the current expression to
    // whatever was just passed in. We may traverse the type and do more.
    (*astExpectedTypes)[expr] = expectedType;

    if (const auto exprTable = expr->as<AstExprTable>())
    {
        const auto expectedTableType = get<TableType>(expectedType);
        if (!expectedTableType)
        {
            if (auto utv = get<UnionType>(expectedType))
            {
                if (auto exprType = astTypes->find(expr))
                {
                    std::vector<TypeId> parts{begin(utv), end(utv)};
                    if (auto tt = extractMatchingTableType(parts, *exprType, builtinTypes))
                    {
                        applyExpectedType(*tt, expr);
                        return;
                    }
                }
            }
            return;
        }

        // If we have a table, then the expected type for any given key is a
        // union between all the possible keys and an indexer type (if it exists).
        std::vector<TypeId> possibleKeyTypes;
        possibleKeyTypes.reserve(expectedTableType->props.size() + (expectedTableType->indexer ? 1 : 0));
        for (const auto& [name, _] : expectedTableType->props)
        {
            possibleKeyTypes.push_back(arena->addType(SingletonType{StringSingleton{name}}));
        }

        if (expectedTableType->indexer)
            possibleKeyTypes.push_back(expectedTableType->indexer->indexType);

        TypeId expectedKeyType = nullptr;
        if (possibleKeyTypes.size() == 0)
            expectedKeyType = builtinTypes->neverType;
        else if (possibleKeyTypes.size() == 1)
            expectedKeyType = possibleKeyTypes[0];
        else
            expectedKeyType = arena->addType(UnionType{std::move(possibleKeyTypes)});

        for (const AstExprTable::Item& item : exprTable->items)
        {
            if (isRecord(item))
            {
                const AstArray<char>& s = item.key->as<AstExprConstantString>()->value;
                std::string keyStr{s.data, s.data + s.size};

                // No mater what, we can claim that the expected key type is the
                // union of all possible props plus the indexer.
                applyExpectedType(expectedKeyType, item.key);

                // - If the property is defined and has a read type, apply it
                //   as an expected type. e.g.:
                //
                //      -- _ will have expected type `number`
                //      local t: { [string]: number, write foo: boolean } = { foo = _ }
                //
                // - Otherwise if the property has an indexer, apply the result type.
                // - Otherwise do nothing.
                if (auto it = expectedTableType->props.find(keyStr); it != expectedTableType->props.end() && it->second.readTy)
                {
                    applyExpectedType(*it->second.readTy, item.value);
                }
                else if (expectedTableType->indexer)
                {
                    applyExpectedType(expectedTableType->indexer->indexResultType, item.value);
                }
            }
            else if (item.kind == AstExprTable::Item::List && expectedTableType->indexer)
            {
                applyExpectedType(expectedTableType->indexer->indexResultType, item.value);
            }
            else if (item.kind == AstExprTable::Item::General && expectedTableType->indexer)
            {
                applyExpectedType(expectedTableType->indexer->indexResultType, item.value);
                applyExpectedType(expectedKeyType, item.key);
            }
        }
    }
    else if (auto group = expr->as<AstExprGroup>())
    {
        applyExpectedType(expectedType, group->expr);
    }
    else if (auto ternary = expr->as<AstExprIfElse>())
    {
        applyExpectedType(expectedType, ternary->trueExpr);
        applyExpectedType(expectedType, ternary->falseExpr);
    }
}

} // namespace Luau
