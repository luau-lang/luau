// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Variant.h"
#include "Luau/Symbol.h"
#include "Luau/TypeFwd.h"

#include <memory>
#include <unordered_map>

namespace Luau
{

struct Field;

// Deprecated. Do not use in new work.
using LValue = Variant<Symbol, Field>;

struct Field
{
    std::shared_ptr<LValue> parent;
    std::string key;

    bool operator==(const Field& rhs) const;
    bool operator!=(const Field& rhs) const;
};

struct LValueHasher
{
    size_t operator()(const LValue& lvalue) const;
};

const LValue* baseof(const LValue& lvalue);

std::optional<LValue> tryGetLValue(const class AstExpr& expr);

// Utility function: breaks down an LValue to get at the Symbol
Symbol getBaseSymbol(const LValue& lvalue);

template<typename T>
const T* get(const LValue& lvalue)
{
    return get_if<T>(&lvalue);
}

using RefinementMap = std::unordered_map<LValue, TypeId, LValueHasher>;

void merge(RefinementMap& l, const RefinementMap& r, std::function<TypeId(TypeId, TypeId)> f);
void addRefinement(RefinementMap& refis, const LValue& lvalue, TypeId ty);

} // namespace Luau
