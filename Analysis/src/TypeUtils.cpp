// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeUtils.h"

#include "Luau/Normalize.h"
#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/TypeInfer.h"

#include <algorithm>

namespace Luau
{

std::optional<TypeId> findMetatableEntry(
    NotNull<SingletonTypes> singletonTypes, ErrorVec& errors, TypeId type, const std::string& entry, Location location)
{
    type = follow(type);

    std::optional<TypeId> metatable = getMetatable(type, singletonTypes);
    if (!metatable)
        return std::nullopt;

    TypeId unwrapped = follow(*metatable);

    if (get<AnyTypeVar>(unwrapped))
        return singletonTypes->anyType;

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

std::optional<TypeId> findTablePropertyRespectingMeta(
    NotNull<SingletonTypes> singletonTypes, ErrorVec& errors, TypeId ty, const std::string& name, Location location)
{
    if (get<AnyTypeVar>(ty))
        return ty;

    if (const TableTypeVar* tableType = getTableType(ty))
    {
        const auto& it = tableType->props.find(name);
        if (it != tableType->props.end())
            return it->second.type;
    }

    std::optional<TypeId> mtIndex = findMetatableEntry(singletonTypes, errors, ty, "__index", location);
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
                return singletonTypes->nilType;
            else
                return *r;
        }
        else if (get<AnyTypeVar>(index))
            return singletonTypes->anyType;
        else
            errors.push_back(TypeError{location, GenericError{"__index should either be a function or table. Got " + toString(index)}});

        mtIndex = findMetatableEntry(singletonTypes, errors, *mtIndex, "__index", location);
    }

    return std::nullopt;
}

std::optional<TypeId> getIndexTypeFromType(const ScopePtr& scope, ErrorVec& errors, TypeArena* arena, NotNull<SingletonTypes> singletonTypes,
    TypeId type, const std::string& prop, const Location& location, bool addErrors, InternalErrorReporter& handle)
{
    type = follow(type);

    if (get<ErrorTypeVar>(type) || get<AnyTypeVar>(type) || get<NeverTypeVar>(type))
        return type;

    if (auto f = get<FreeTypeVar>(type))
        *asMutable(type) = TableTypeVar{TableState::Free, f->level};

    if (isString(type))
    {
        std::optional<TypeId> mtIndex = Luau::findMetatableEntry(singletonTypes, errors, singletonTypes->stringType, "__index", location);
        LUAU_ASSERT(mtIndex);
        type = *mtIndex;
    }

    if (getTableType(type))
    {
        return findTablePropertyRespectingMeta(singletonTypes, errors, type, prop, location);
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
            if (get<AnyTypeVar>(follow(t)))
                return t;

            if (std::optional<TypeId> ty =
                    getIndexTypeFromType(scope, errors, arena, singletonTypes, t, prop, location, /* addErrors= */ false, handle))
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

        goodOptions = reduceUnion(goodOptions);

        if (goodOptions.empty())
            return singletonTypes->neverType;

        if (goodOptions.size() == 1)
            return goodOptions[0];

        return arena->addType(UnionTypeVar{std::move(goodOptions)});
    }
    else if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(type))
    {
        std::vector<TypeId> parts;

        for (TypeId t : itv->parts)
        {
            if (std::optional<TypeId> ty =
                    getIndexTypeFromType(scope, errors, arena, singletonTypes, t, prop, location, /* addErrors= */ false, handle))
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

        return arena->addType(IntersectionTypeVar{std::move(parts)});
    }

    if (addErrors)
        errors.push_back(TypeError{location, UnknownProperty{type, prop}});

    return std::nullopt;
}

std::pair<size_t, std::optional<size_t>> getParameterExtents(const TxnLog* log, TypePackId tp, bool includeHiddenVariadics)
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

    if (it.tail() && isVariadicTail(*it.tail(), *log, includeHiddenVariadics))
        return {minCount, std::nullopt};
    else
        return {minCount, minCount + optionalCount};
}

TypePack extendTypePack(TypeArena& arena, NotNull<SingletonTypes> singletonTypes, TypePackId pack, size_t length)
{
    TypePack result;

    while (true)
    {
        pack = follow(pack);

        if (const TypePack* p = get<TypePack>(pack))
        {
            size_t i = 0;
            while (i < p->head.size() && result.head.size() < length)
            {
                result.head.push_back(p->head[i]);
                ++i;
            }

            if (result.head.size() == length)
            {
                if (i == p->head.size())
                    result.tail = p->tail;
                else
                {
                    TypePackId newTail = arena.addTypePack(TypePack{});
                    TypePack* newTailPack = getMutable<TypePack>(newTail);

                    newTailPack->head.insert(newTailPack->head.begin(), p->head.begin() + i, p->head.end());
                    newTailPack->tail = p->tail;

                    result.tail = newTail;
                }

                return result;
            }
            else if (p->tail)
            {
                pack = *p->tail;
                continue;
            }
            else
            {
                // There just aren't enough types in this pack to satisfy the request.
                return result;
            }
        }
        else if (const VariadicTypePack* vtp = get<VariadicTypePack>(pack))
        {
            while (result.head.size() < length)
                result.head.push_back(vtp->ty);
            result.tail = pack;
            return result;
        }
        else if (FreeTypePack* ftp = getMutable<FreeTypePack>(pack))
        {
            // If we need to get concrete types out of a free pack, we choose to
            // interpret this as proof that the pack must have at least 'length'
            // elements.  We mint fresh types for each element we're extracting
            // and rebind the free pack to be a TypePack containing them. We
            // also have to create a new tail.

            TypePack newPack;
            newPack.tail = arena.freshTypePack(ftp->scope);

            while (result.head.size() < length)
            {
                newPack.head.push_back(arena.freshType(ftp->scope));
                result.head.push_back(newPack.head.back());
            }

            asMutable(pack)->ty.emplace<TypePack>(std::move(newPack));

            return result;
        }
        else if (const Unifiable::Error* etp = getMutable<Unifiable::Error>(pack))
        {
            while (result.head.size() < length)
                result.head.push_back(singletonTypes->errorRecoveryType());

            result.tail = pack;
            return result;
        }
        else
        {
            // If the pack is blocked or generic, we can't extract.
            // Return whatever we've got with this pack as the tail.
            result.tail = pack;
            return result;
        }
    }
}

std::vector<TypeId> reduceUnion(const std::vector<TypeId>& types)
{
    std::vector<TypeId> result;
    for (TypeId t : types)
    {
        t = follow(t);
        if (get<NeverTypeVar>(t))
            continue;

        if (get<ErrorTypeVar>(t) || get<AnyTypeVar>(t))
            return {t};

        if (const UnionTypeVar* utv = get<UnionTypeVar>(t))
        {
            for (TypeId ty : utv)
            {
                ty = follow(ty);
                if (get<NeverTypeVar>(ty))
                    continue;
                if (get<ErrorTypeVar>(ty) || get<AnyTypeVar>(ty))
                    return {ty};

                if (result.end() == std::find(result.begin(), result.end(), ty))
                    result.push_back(ty);
            }
        }
        else if (std::find(result.begin(), result.end(), t) == result.end())
            result.push_back(t);
    }

    return result;
}

static std::optional<TypeId> tryStripUnionFromNil(TypeArena& arena, TypeId ty)
{
    if (const UnionTypeVar* utv = get<UnionTypeVar>(ty))
    {
        if (!std::any_of(begin(utv), end(utv), isNil))
            return ty;

        std::vector<TypeId> result;

        for (TypeId option : utv)
        {
            if (!isNil(option))
                result.push_back(option);
        }

        if (result.empty())
            return std::nullopt;

        return result.size() == 1 ? result[0] : arena.addType(UnionTypeVar{std::move(result)});
    }

    return std::nullopt;
}

TypeId stripNil(NotNull<SingletonTypes> singletonTypes, TypeArena& arena, TypeId ty)
{
    ty = follow(ty);

    if (get<UnionTypeVar>(ty))
    {
        std::optional<TypeId> cleaned = tryStripUnionFromNil(arena, ty);

        // If there is no union option without 'nil'
        if (!cleaned)
            return singletonTypes->nilType;

        return follow(*cleaned);
    }

    return follow(ty);
}

} // namespace Luau
