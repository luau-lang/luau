// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Predicate.h"

#include "Luau/Ast.h"

LUAU_FASTFLAG(LuauOrPredicate)

namespace Luau
{

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
            if (auto string = indexexpr->expr->as<AstExprConstantString>())
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
        current = field->parent.get();
        if (!current)
            LUAU_ASSERT(!"LValue root is a Field?");
    }

    const Symbol* symbol = get<Symbol>(*current);
    return {*symbol, std::vector<std::string>(keys.rbegin(), keys.rend())};
}

std::string toString(const LValue& lvalue)
{
    auto [symbol, keys] = getFullName(lvalue);
    std::string s = toString(symbol);
    for (std::string key : keys)
        s += "." + key;
    return s;
}

void merge(RefinementMap& l, const RefinementMap& r, std::function<TypeId(TypeId, TypeId)> f)
{
    LUAU_ASSERT(FFlag::LuauOrPredicate);

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
        else if (itL->first > k)
        {
            l[k] = a;
            ++itR;
        }
        else
            ++itL;
    }

    l.insert(itR, r.end());
}

void addRefinement(RefinementMap& refis, const LValue& lvalue, TypeId ty)
{
    refis[toString(lvalue)] = ty;
}

} // namespace Luau
