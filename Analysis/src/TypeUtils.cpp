// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeUtils.h"

#include "Luau/Normalize.h"
#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/TypeInfer.h"

namespace Luau
{

std::optional<TypeId> findMetatableEntry(ErrorVec& errors, TypeId type, const std::string& entry, Location location)
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
        errors.push_back(TypeError{location, GenericError{"Metatable was not a table"}});
        return std::nullopt;
    }

    auto it = mtt->props.find(entry);
    if (it != mtt->props.end())
        return it->second.type;
    else
        return std::nullopt;
}

std::optional<TypeId> findTablePropertyRespectingMeta(ErrorVec& errors, TypeId ty, const std::string& name, Location location)
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

        if (count >= 100)
            return std::nullopt;

        ++count;

        if (const auto& itt = getTableType(index))
        {
            const auto& fit = itt->props.find(name);
            if (fit != itt->props.end())
                return fit->second.type;
        }
        else if (const auto& itf = get<FunctionTypeVar>(index))
        {
            std::optional<TypeId> r = first(follow(itf->retTypes));
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

std::optional<TypeId> getIndexTypeFromType(const ScopePtr& scope, ErrorVec& errors, TypeArena* arena, TypeId type, const std::string& prop,
    const Location& location, bool addErrors, InternalErrorReporter& handle)
{
    type = follow(type);

    if (get<ErrorTypeVar>(type) || get<AnyTypeVar>(type) || get<NeverTypeVar>(type))
        return type;

    if (auto f = get<FreeTypeVar>(type))
        *asMutable(type) = TableTypeVar{TableState::Free, f->level};

    if (isString(type))
    {
        std::optional<TypeId> mtIndex = Luau::findMetatableEntry(errors, getSingletonTypes().stringType, "__index", location);
        LUAU_ASSERT(mtIndex);
        type = *mtIndex;
    }

    if (getTableType(type))
    {
        return findTablePropertyRespectingMeta(errors, type, prop, location);
    }
    else if (const ClassTypeVar* cls = get<ClassTypeVar>(type))
    {
        if (const Property* p = lookupClassProp(cls, prop))
            return p->type;
    }
    else if (const UnionTypeVar* utv = get<UnionTypeVar>(type))
    {
        std::vector<TypeId> goodOptions;
        std::vector<TypeId> badOptions;

        for (TypeId t : utv)
        {
            // TODO: we should probably limit recursion here?
            // RecursionLimiter _rl(&recursionCount, FInt::LuauTypeInferRecursionLimit);

            // Not needed when we normalize types.
            if (get<AnyTypeVar>(follow(t)))
                return t;

            if (std::optional<TypeId> ty = getIndexTypeFromType(scope, errors, arena, t, prop, location, /* addErrors= */ false, handle))
                goodOptions.push_back(*ty);
            else
                badOptions.push_back(t);
        }

        if (!badOptions.empty())
        {
            if (addErrors)
            {
                if (goodOptions.empty())
                    errors.push_back(TypeError{location, UnknownProperty{type, prop}});
                else
                    errors.push_back(TypeError{location, MissingUnionProperty{type, badOptions, prop}});
            }
            return std::nullopt;
        }

        if (goodOptions.empty())
            return getSingletonTypes().neverType;

        if (goodOptions.size() == 1)
            return goodOptions[0];

        // TODO: inefficient.
        TypeId result = arena->addType(UnionTypeVar{std::move(goodOptions)});
        auto [ty, ok] = normalize(result, NotNull{scope.get()}, *arena, handle);
        if (!ok && addErrors)
            errors.push_back(TypeError{location, NormalizationTooComplex{}});
        return ok ? ty : getSingletonTypes().anyType;
    }
    else if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(type))
    {
        std::vector<TypeId> parts;

        for (TypeId t : itv->parts)
        {
            // TODO: we should probably limit recursion here?
            // RecursionLimiter _rl(&recursionCount, FInt::LuauTypeInferRecursionLimit);

            if (std::optional<TypeId> ty = getIndexTypeFromType(scope, errors, arena, t, prop, location, /* addErrors= */ false, handle))
                parts.push_back(*ty);
        }

        // If no parts of the intersection had the property we looked up for, it never existed at all.
        if (parts.empty())
        {
            if (addErrors)
                errors.push_back(TypeError{location, UnknownProperty{type, prop}});
            return std::nullopt;
        }

        if (parts.size() == 1)
            return parts[0];

        return arena->addType(IntersectionTypeVar{std::move(parts)}); // Not at all correct.
    }

    if (addErrors)
        errors.push_back(TypeError{location, UnknownProperty{type, prop}});

    return std::nullopt;
}

std::pair<size_t, std::optional<size_t>> getParameterExtents(const TxnLog* log, TypePackId tp)
{
    size_t minCount = 0;
    size_t optionalCount = 0;

    auto it = begin(tp, log);
    auto endIter = end(tp);

    while (it != endIter)
    {
        TypeId ty = *it;
        if (isOptional(ty))
            ++optionalCount;
        else
        {
            minCount += optionalCount;
            optionalCount = 0;
            minCount++;
        }

        ++it;
    }

    if (it.tail())
        return {minCount, std::nullopt};
    else
        return {minCount, minCount + optionalCount};
}

} // namespace Luau
