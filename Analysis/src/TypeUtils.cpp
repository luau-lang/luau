// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeUtils.h"

#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/TypeInfer.h"

LUAU_FASTFLAGVARIABLE(LuauTerminateCyclicMetatableIndexLookup, false)

namespace Luau
{

std::optional<TypeId> findMetatableEntry(ErrorVec& errors, TypeId type, std::string entry, Location location)
{
    type = follow(type);

    std::optional<TypeId> metatable = getMetatable(type);
    if (!metatable)
        return std::nullopt;

    TypeId unwrapped = follow(*metatable);

    if (get<AnyTypeVar>(unwrapped))
        return getSingletonTypes().anyType;

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

std::optional<TypeId> findTablePropertyRespectingMeta(ErrorVec& errors, TypeId ty, Name name, Location location)
{
    if (get<AnyTypeVar>(ty))
        return ty;

    if (const TableTypeVar* tableType = getTableType(ty))
    {
        const auto& it = tableType->props.find(name);
        if (it != tableType->props.end())
            return it->second.type;
    }

    std::optional<TypeId> mtIndex = findMetatableEntry(errors, ty, "__index", location);
    int count = 0;
    while (mtIndex)
    {
        TypeId index = follow(*mtIndex);

        if (FFlag::LuauTerminateCyclicMetatableIndexLookup)
        {
            if (count >= 100)
                return std::nullopt;

            ++count;
        }

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
                return getSingletonTypes().nilType;
            else
                return *r;
        }
        else if (get<AnyTypeVar>(index))
            return getSingletonTypes().anyType;
        else
            errors.push_back(TypeError{location, GenericError{"__index should either be a function or table. Got " + toString(index)}});

        mtIndex = findMetatableEntry(errors, *mtIndex, "__index", location);
    }

    return std::nullopt;
}

} // namespace Luau
