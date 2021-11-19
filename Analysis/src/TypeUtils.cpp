// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeUtils.h"

#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/TypeInfer.h"

namespace Luau
{

std::optional<TypeId> findMetatableEntry(ErrorVec& errors, const ScopePtr& globalScope, TypeId type, std::string entry, Location location)
{
    type = follow(type);

    std::optional<TypeId> metatable = getMetatable(type);
    if (!metatable)
        return std::nullopt;

    TypeId unwrapped = follow(*metatable);

    if (get<AnyTypeVar>(unwrapped))
        return singletonTypes.anyType;

    const TableTypeVar* mtt = getTableType(unwrapped);
    if (!mtt)
    {
        errors.push_back(TypeError{location, GenericError{"Metatable was not a table."}});
        return std::nullopt;
    }

    auto it = mtt->props.find(entry);
    if (it != mtt->props.end())
        return it->second.type;
    else
        return std::nullopt;
}

std::optional<TypeId> findTablePropertyRespectingMeta(ErrorVec& errors, const ScopePtr& globalScope, TypeId ty, Name name, Location location)
{
    if (get<AnyTypeVar>(ty))
        return ty;

    if (const TableTypeVar* tableType = getTableType(ty))
    {
        const auto& it = tableType->props.find(name);
        if (it != tableType->props.end())
            return it->second.type;
    }

    std::optional<TypeId> mtIndex = findMetatableEntry(errors, globalScope, ty, "__index", location);
    while (mtIndex)
    {
        TypeId index = follow(*mtIndex);
        if (const auto& itt = getTableType(index))
        {
            const auto& fit = itt->props.find(name);
            if (fit != itt->props.end())
                return fit->second.type;
        }
        else if (const auto& itf = get<FunctionTypeVar>(index))
        {
            std::optional<TypeId> r = first(follow(itf->retType));
            if (!r)
                return singletonTypes.nilType;
            else
                return *r;
        }
        else if (get<AnyTypeVar>(index))
            return singletonTypes.anyType;
        else
            errors.push_back(TypeError{location, GenericError{"__index should either be a function or table. Got " + toString(index)}});

        mtIndex = findMetatableEntry(errors, globalScope, *mtIndex, "__index", location);
    }

    return std::nullopt;
}

} // namespace Luau
