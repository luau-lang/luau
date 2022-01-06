// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/LValue.h"

#include "Luau/Ast.h"

#include <vector>

LUAU_FASTFLAG(LuauLValueAsKey)

namespace Luau
{

bool Field::operator==(const Field& rhs) const
{
    LUAU_ASSERT(parent && rhs.parent);
    return key == rhs.key && (parent == rhs.parent || *parent == *rhs.parent);
}

bool Field::operator!=(const Field& rhs) const
{
    return !(*this == rhs);
}

size_t LValueHasher::operator()(const LValue& lvalue) const
{
    // Most likely doesn't produce high quality hashes, but we're probably ok enough with it.
    // When an evidence is shown that operator==(LValue) is used more often than it should, we can have a look at improving the hash quality.
    size_t acc = 0;
    size_t offset = 0;

    const LValue* current = &lvalue;
    while (current)
    {
        if (auto field = get<Field>(*current))
            acc ^= (std::hash<std::string>{}(field->key) << 1) >> ++offset;
        else if (auto symbol = get<Symbol>(*current))
            acc ^= std::hash<Symbol>{}(*symbol) << 1;
        else
            LUAU_ASSERT(!"Hash not accumulated for this new LValue alternative.");

        current = baseof(*current);
    }

    return acc;
}

const LValue* baseof(const LValue& lvalue)
{
    if (auto field = get<Field>(lvalue))
        return field->parent.get();

    auto symbol = get<Symbol>(lvalue);
    LUAU_ASSERT(symbol);
    return nullptr; // Base of root is null.
}

std::optional<LValue> tryGetLValue(const AstExpr& node)
{
    const AstExpr* expr = &node;
    while (auto e = expr->as<AstExprGroup>())
        expr = e->expr;

    if (auto local = expr->as<AstExprLocal>())
        return Symbol{local->local};
    else if (auto global = expr->as<AstExprGlobal>())
        return Symbol{global->name};
    else if (auto indexname = expr->as<AstExprIndexName>())
    {
        if (auto lvalue = tryGetLValue(*indexname->expr))
            return Field{std::make_shared<LValue>(*lvalue), indexname->index.value};
    }
    else if (auto indexexpr = expr->as<AstExprIndexExpr>())
    {
        if (auto lvalue = tryGetLValue(*indexexpr->expr))
            if (auto string = indexexpr->index->as<AstExprConstantString>())
                return Field{std::make_shared<LValue>(*lvalue), std::string(string->value.data, string->value.size)};
    }

    return std::nullopt;
}

std::pair<Symbol, std::vector<std::string>> getFullName(const LValue& lvalue)
{
    const LValue* current = &lvalue;
    std::vector<std::string> keys;
    while (auto field = get<Field>(*current))
    {
        keys.push_back(field->key);
        current = baseof(*current);
    }

    const Symbol* symbol = get<Symbol>(*current);
    LUAU_ASSERT(symbol);
    return {*symbol, std::vector<std::string>(keys.rbegin(), keys.rend())};
}

// Kill with LuauLValueAsKey.
std::string toString(const LValue& lvalue)
{
    auto [symbol, keys] = getFullName(lvalue);
    std::string s = toString(symbol);
    for (std::string key : keys)
        s += "." + key;
    return s;
}

static void merge(NEW_RefinementMap& l, const NEW_RefinementMap& r, std::function<TypeId(TypeId, TypeId)> f)
{
    for (const auto& [k, a] : r)
    {
        if (auto it = l.find(k); it != l.end())
            l[k] = f(it->second, a);
        else
            l[k] = a;
    }
}

static void merge(DEPRECATED_RefinementMap& l, const DEPRECATED_RefinementMap& r, std::function<TypeId(TypeId, TypeId)> f)
{
    auto itL = l.begin();
    auto itR = r.begin();
    while (itL != l.end() && itR != r.end())
    {
        const auto& [k, a] = *itR;
        if (itL->first == k)
        {
            l[k] = f(itL->second, a);
            ++itL;
            ++itR;
        }
        else if (itL->first < k)
            ++itL;
        else
        {
            l[k] = a;
            ++itR;
        }
    }

    l.insert(itR, r.end());
}

void merge(RefinementMap& l, const RefinementMap& r, std::function<TypeId(TypeId, TypeId)> f)
{
    if (FFlag::LuauLValueAsKey)
        return merge(l.NEW_refinements, r.NEW_refinements, f);
    else
        return merge(l.DEPRECATED_refinements, r.DEPRECATED_refinements, f);
}

void addRefinement(RefinementMap& refis, const LValue& lvalue, TypeId ty)
{
    if (FFlag::LuauLValueAsKey)
        refis.NEW_refinements[lvalue] = ty;
    else
        refis.DEPRECATED_refinements[toString(lvalue)] = ty;
}

} // namespace Luau
