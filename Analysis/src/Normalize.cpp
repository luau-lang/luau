// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Normalize.h"
#include "Luau/ToString.h"

#include <algorithm>

#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/RecursionCounter.h"
#include "Luau/TypeVar.h"
#include "Luau/Unifier.h"

LUAU_FASTFLAGVARIABLE(DebugLuauCopyBeforeNormalizing, false)
LUAU_FASTFLAGVARIABLE(DebugLuauCheckNormalizeInvariant, false)

// This could theoretically be 2000 on amd64, but x86 requires this.
LUAU_FASTINTVARIABLE(LuauNormalizeIterationLimit, 1200);
LUAU_FASTINTVARIABLE(LuauNormalizeCacheLimit, 100000);
LUAU_FASTFLAGVARIABLE(LuauNormalizeCombineTableFix, false);
LUAU_FASTFLAGVARIABLE(LuauTypeNormalization2, false);
LUAU_FASTFLAGVARIABLE(LuauNegatedStringSingletons, false);
LUAU_FASTFLAGVARIABLE(LuauNegatedFunctionTypes, false);
LUAU_FASTFLAG(LuauUnknownAndNeverType)
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)
LUAU_FASTFLAG(LuauOverloadedFunctionSubtypingPerf);

namespace Luau
{
void TypeIds::insert(TypeId ty)
{
    ty = follow(ty);
    auto [_, fresh] = types.insert(ty);
    if (fresh)
    {
        order.push_back(ty);
        hash ^= std::hash<TypeId>{}(ty);
    }
}

void TypeIds::clear()
{
    order.clear();
    types.clear();
    hash = 0;
}

TypeIds::iterator TypeIds::begin()
{
    return order.begin();
}

TypeIds::iterator TypeIds::end()
{
    return order.end();
}

TypeIds::const_iterator TypeIds::begin() const
{
    return order.begin();
}

TypeIds::const_iterator TypeIds::end() const
{
    return order.end();
}

TypeIds::iterator TypeIds::erase(TypeIds::const_iterator it)
{
    TypeId ty = *it;
    types.erase(ty);
    hash ^= std::hash<TypeId>{}(ty);
    return order.erase(it);
}

size_t TypeIds::size() const
{
    return types.size();
}

bool TypeIds::empty() const
{
    return types.empty();
}

size_t TypeIds::count(TypeId ty) const
{
    ty = follow(ty);
    return types.count(ty);
}

void TypeIds::retain(const TypeIds& there)
{
    for (auto it = begin(); it != end();)
    {
        if (there.count(*it))
            it++;
        else
            it = erase(it);
    }
}

size_t TypeIds::getHash() const
{
    return hash;
}

bool TypeIds::operator==(const TypeIds& there) const
{
    return hash == there.hash && types == there.types;
}

NormalizedStringType::NormalizedStringType(bool isCofinite, std::optional<std::map<std::string, TypeId>> singletons)
    : isCofinite(isCofinite)
    , singletons(std::move(singletons))
{
    if (!FFlag::LuauNegatedStringSingletons)
        LUAU_ASSERT(!isCofinite);
}

void NormalizedStringType::resetToString()
{
    if (FFlag::LuauNegatedStringSingletons)
    {
        isCofinite = true;
        singletons->clear();
    }
    else
        singletons.reset();
}

void NormalizedStringType::resetToNever()
{
    if (FFlag::LuauNegatedStringSingletons)
    {
        isCofinite = false;
        singletons.emplace();
    }
    else
    {
        if (singletons)
            singletons->clear();
        else
            singletons.emplace();
    }
}

bool NormalizedStringType::isNever() const
{
    if (FFlag::LuauNegatedStringSingletons)
        return !isCofinite && singletons->empty();
    else
        return singletons && singletons->empty();
}

bool NormalizedStringType::isString() const
{
    if (FFlag::LuauNegatedStringSingletons)
        return isCofinite && singletons->empty();
    else
        return !singletons;
}

bool NormalizedStringType::isUnion() const
{
    if (FFlag::LuauNegatedStringSingletons)
        return !isCofinite;
    else
        return singletons.has_value();
}

bool NormalizedStringType::isIntersection() const
{
    if (FFlag::LuauNegatedStringSingletons)
        return isCofinite;
    else
        return false;
}

bool NormalizedStringType::includes(const std::string& str) const
{
    if (isString())
        return true;
    else if (isUnion() && singletons->count(str))
        return true;
    else if (isIntersection() && !singletons->count(str))
        return true;
    else
        return false;
}

const NormalizedStringType NormalizedStringType::never{false, {{}}};

bool isSubtype(const NormalizedStringType& subStr, const NormalizedStringType& superStr)
{
    if (subStr.isUnion() && superStr.isUnion())
    {
        for (auto [name, ty] : *subStr.singletons)
        {
            if (!superStr.singletons->count(name))
                return false;
        }
    }
    else if (subStr.isString() && superStr.isUnion())
        return false;

    return true;
}

NormalizedFunctionType::NormalizedFunctionType()
    : parts(FFlag::LuauNegatedFunctionTypes ? std::optional<TypeIds>{TypeIds{}} : std::nullopt)
{
}

void NormalizedFunctionType::resetToTop()
{
    isTop = true;
    parts.emplace();
}

void NormalizedFunctionType::resetToNever()
{
    isTop = false;
    parts.emplace();
}

bool NormalizedFunctionType::isNever() const
{
    return !isTop && (!parts || parts->empty());
}

NormalizedType::NormalizedType(NotNull<SingletonTypes> singletonTypes)
    : tops(singletonTypes->neverType)
    , booleans(singletonTypes->neverType)
    , errors(singletonTypes->neverType)
    , nils(singletonTypes->neverType)
    , numbers(singletonTypes->neverType)
    , strings{NormalizedStringType::never}
    , threads(singletonTypes->neverType)
{
}

static bool isInhabited(const NormalizedType& norm)
{
    return !get<NeverTypeVar>(norm.tops) || !get<NeverTypeVar>(norm.booleans) || !norm.classes.empty() || !get<NeverTypeVar>(norm.errors) ||
           !get<NeverTypeVar>(norm.nils) || !get<NeverTypeVar>(norm.numbers) || !norm.strings.isNever() || !get<NeverTypeVar>(norm.threads) ||
           !norm.functions.isNever() || !norm.tables.empty() || !norm.tyvars.empty();
}

static int tyvarIndex(TypeId ty)
{
    if (const GenericTypeVar* gtv = get<GenericTypeVar>(ty))
        return gtv->index;
    else if (const FreeTypeVar* ftv = get<FreeTypeVar>(ty))
        return ftv->index;
    else
        return 0;
}

#ifdef LUAU_ASSERTENABLED

static bool isNormalizedTop(TypeId ty)
{
    return get<NeverTypeVar>(ty) || get<AnyTypeVar>(ty) || get<UnknownTypeVar>(ty);
}

static bool isNormalizedBoolean(TypeId ty)
{
    if (get<NeverTypeVar>(ty))
        return true;
    else if (const PrimitiveTypeVar* ptv = get<PrimitiveTypeVar>(ty))
        return ptv->type == PrimitiveTypeVar::Boolean;
    else if (const SingletonTypeVar* stv = get<SingletonTypeVar>(ty))
        return get<BooleanSingleton>(stv);
    else
        return false;
}

static bool isNormalizedError(TypeId ty)
{
    if (get<NeverTypeVar>(ty) || get<ErrorTypeVar>(ty))
        return true;
    else
        return false;
}

static bool isNormalizedNil(TypeId ty)
{
    if (get<NeverTypeVar>(ty))
        return true;
    else if (const PrimitiveTypeVar* ptv = get<PrimitiveTypeVar>(ty))
        return ptv->type == PrimitiveTypeVar::NilType;
    else
        return false;
}

static bool isNormalizedNumber(TypeId ty)
{
    if (get<NeverTypeVar>(ty))
        return true;
    else if (const PrimitiveTypeVar* ptv = get<PrimitiveTypeVar>(ty))
        return ptv->type == PrimitiveTypeVar::Number;
    else
        return false;
}

static bool isNormalizedString(const NormalizedStringType& ty)
{
    if (ty.isString())
        return true;

    for (auto& [str, ty] : *ty.singletons)
    {
        if (const SingletonTypeVar* stv = get<SingletonTypeVar>(ty))
        {
            if (const StringSingleton* sstv = get<StringSingleton>(stv))
            {
                if (sstv->value != str)
                    return false;
            }
            else
                return false;
        }
        else
            return false;
    }

    return true;
}

static bool isNormalizedThread(TypeId ty)
{
    if (get<NeverTypeVar>(ty))
        return true;
    else if (const PrimitiveTypeVar* ptv = get<PrimitiveTypeVar>(ty))
        return ptv->type == PrimitiveTypeVar::Thread;
    else
        return false;
}

static bool areNormalizedFunctions(const NormalizedFunctionType& tys)
{
    if (tys.parts)
    {
        for (TypeId ty : *tys.parts)
        {
            if (!get<FunctionTypeVar>(ty) && !get<ErrorTypeVar>(ty))
                return false;
        }
    }
    return true;
}

static bool areNormalizedTables(const TypeIds& tys)
{
    for (TypeId ty : tys)
        if (!get<TableTypeVar>(ty) && !get<MetatableTypeVar>(ty))
            return false;
    return true;
}

static bool areNormalizedClasses(const TypeIds& tys)
{
    for (TypeId ty : tys)
        if (!get<ClassTypeVar>(ty))
            return false;
    return true;
}

static bool isPlainTyvar(TypeId ty)
{
    return (get<FreeTypeVar>(ty) || get<GenericTypeVar>(ty));
}

static bool isNormalizedTyvar(const NormalizedTyvars& tyvars)
{
    for (auto& [tyvar, intersect] : tyvars)
    {
        if (!isPlainTyvar(tyvar))
            return false;
        if (!isInhabited(*intersect))
            return false;
        for (auto& [other, _] : intersect->tyvars)
            if (tyvarIndex(other) <= tyvarIndex(tyvar))
                return false;
    }
    return true;
}

#endif // LUAU_ASSERTENABLED

static void assertInvariant(const NormalizedType& norm)
{
#ifdef LUAU_ASSERTENABLED
    if (!FFlag::DebugLuauCheckNormalizeInvariant)
        return;

    LUAU_ASSERT(isNormalizedTop(norm.tops));
    LUAU_ASSERT(isNormalizedBoolean(norm.booleans));
    LUAU_ASSERT(areNormalizedClasses(norm.classes));
    LUAU_ASSERT(isNormalizedError(norm.errors));
    LUAU_ASSERT(isNormalizedNil(norm.nils));
    LUAU_ASSERT(isNormalizedNumber(norm.numbers));
    LUAU_ASSERT(isNormalizedString(norm.strings));
    LUAU_ASSERT(isNormalizedThread(norm.threads));
    LUAU_ASSERT(areNormalizedFunctions(norm.functions));
    LUAU_ASSERT(areNormalizedTables(norm.tables));
    LUAU_ASSERT(isNormalizedTyvar(norm.tyvars));
    for (auto& [_, child] : norm.tyvars)
        assertInvariant(*child);
#endif
}

Normalizer::Normalizer(TypeArena* arena, NotNull<SingletonTypes> singletonTypes, NotNull<UnifierSharedState> sharedState)
    : arena(arena)
    , singletonTypes(singletonTypes)
    , sharedState(sharedState)
{
}

const NormalizedType* Normalizer::normalize(TypeId ty)
{
    if (!arena)
        sharedState->iceHandler->ice("Normalizing types outside a module");

    auto found = cachedNormals.find(ty);
    if (found != cachedNormals.end())
        return found->second.get();

    NormalizedType norm{singletonTypes};
    if (!unionNormalWithTy(norm, ty))
        return nullptr;
    std::unique_ptr<NormalizedType> uniq = std::make_unique<NormalizedType>(std::move(norm));
    const NormalizedType* result = uniq.get();
    cachedNormals[ty] = std::move(uniq);
    return result;
}

void Normalizer::clearNormal(NormalizedType& norm)
{
    norm.tops = singletonTypes->neverType;
    norm.booleans = singletonTypes->neverType;
    norm.classes.clear();
    norm.errors = singletonTypes->neverType;
    norm.nils = singletonTypes->neverType;
    norm.numbers = singletonTypes->neverType;
    norm.strings.resetToNever();
    norm.threads = singletonTypes->neverType;
    norm.tables.clear();
    norm.functions.resetToNever();
    norm.tyvars.clear();
}

// ------- Cached TypeIds
const TypeIds* Normalizer::cacheTypeIds(TypeIds tys)
{
    auto found = cachedTypeIds.find(&tys);
    if (found != cachedTypeIds.end())
        return found->first;

    std::unique_ptr<TypeIds> uniq = std::make_unique<TypeIds>(std::move(tys));
    const TypeIds* result = uniq.get();
    cachedTypeIds[result] = std::move(uniq);
    return result;
}

TypeId Normalizer::unionType(TypeId here, TypeId there)
{
    here = follow(here);
    there = follow(there);

    if (here == there)
        return here;
    if (get<NeverTypeVar>(here) || get<AnyTypeVar>(there))
        return there;
    if (get<NeverTypeVar>(there) || get<AnyTypeVar>(here))
        return here;

    TypeIds tmps;

    if (const UnionTypeVar* utv = get<UnionTypeVar>(here))
    {
        TypeIds heres;
        heres.insert(begin(utv), end(utv));
        tmps.insert(heres.begin(), heres.end());
        cachedUnions[cacheTypeIds(std::move(heres))] = here;
    }
    else
        tmps.insert(here);

    if (const UnionTypeVar* utv = get<UnionTypeVar>(there))
    {
        TypeIds theres;
        theres.insert(begin(utv), end(utv));
        tmps.insert(theres.begin(), theres.end());
        cachedUnions[cacheTypeIds(std::move(theres))] = there;
    }
    else
        tmps.insert(there);

    auto cacheHit = cachedUnions.find(&tmps);
    if (cacheHit != cachedUnions.end())
        return cacheHit->second;

    std::vector<TypeId> parts;
    parts.insert(parts.end(), tmps.begin(), tmps.end());
    TypeId result = arena->addType(UnionTypeVar{std::move(parts)});
    cachedUnions[cacheTypeIds(std::move(tmps))] = result;

    return result;
}

TypeId Normalizer::intersectionType(TypeId here, TypeId there)
{
    here = follow(here);
    there = follow(there);

    if (here == there)
        return here;
    if (get<NeverTypeVar>(here) || get<AnyTypeVar>(there))
        return here;
    if (get<NeverTypeVar>(there) || get<AnyTypeVar>(here))
        return there;

    TypeIds tmps;

    if (const IntersectionTypeVar* utv = get<IntersectionTypeVar>(here))
    {
        TypeIds heres;
        heres.insert(begin(utv), end(utv));
        tmps.insert(heres.begin(), heres.end());
        cachedIntersections[cacheTypeIds(std::move(heres))] = here;
    }
    else
        tmps.insert(here);

    if (const IntersectionTypeVar* utv = get<IntersectionTypeVar>(there))
    {
        TypeIds theres;
        theres.insert(begin(utv), end(utv));
        tmps.insert(theres.begin(), theres.end());
        cachedIntersections[cacheTypeIds(std::move(theres))] = there;
    }
    else
        tmps.insert(there);

    if (tmps.size() == 1)
        return *tmps.begin();

    auto cacheHit = cachedIntersections.find(&tmps);
    if (cacheHit != cachedIntersections.end())
        return cacheHit->second;

    std::vector<TypeId> parts;
    parts.insert(parts.end(), tmps.begin(), tmps.end());
    TypeId result = arena->addType(IntersectionTypeVar{std::move(parts)});
    cachedIntersections[cacheTypeIds(std::move(tmps))] = result;

    return result;
}

void Normalizer::clearCaches()
{
    cachedNormals.clear();
    cachedIntersections.clear();
    cachedUnions.clear();
    cachedTypeIds.clear();
}

// ------- Normalizing unions
TypeId Normalizer::unionOfTops(TypeId here, TypeId there)
{
    if (get<NeverTypeVar>(here) || get<AnyTypeVar>(there))
        return there;
    else
        return here;
}

TypeId Normalizer::unionOfBools(TypeId here, TypeId there)
{
    if (get<NeverTypeVar>(here))
        return there;
    if (get<NeverTypeVar>(there))
        return here;
    if (const BooleanSingleton* hbool = get<BooleanSingleton>(get<SingletonTypeVar>(here)))
        if (const BooleanSingleton* tbool = get<BooleanSingleton>(get<SingletonTypeVar>(there)))
            if (hbool->value == tbool->value)
                return here;
    return singletonTypes->booleanType;
}

void Normalizer::unionClassesWithClass(TypeIds& heres, TypeId there)
{
    if (heres.count(there))
        return;

    const ClassTypeVar* tctv = get<ClassTypeVar>(there);

    for (auto it = heres.begin(); it != heres.end();)
    {
        TypeId here = *it;
        const ClassTypeVar* hctv = get<ClassTypeVar>(here);
        if (isSubclass(tctv, hctv))
            return;
        else if (isSubclass(hctv, tctv))
            it = heres.erase(it);
        else
            it++;
    }

    heres.insert(there);
}

void Normalizer::unionClasses(TypeIds& heres, const TypeIds& theres)
{
    for (TypeId there : theres)
        unionClassesWithClass(heres, there);
}

void Normalizer::unionStrings(NormalizedStringType& here, const NormalizedStringType& there)
{
    if (FFlag::LuauNegatedStringSingletons)
    {
        if (there.isString())
            here.resetToString();
        else if (here.isUnion() && there.isUnion())
            here.singletons->insert(there.singletons->begin(), there.singletons->end());
        else if (here.isUnion() && there.isIntersection())
        {
            here.isCofinite = true;
            for (const auto& pair : *there.singletons)
            {
                auto it = here.singletons->find(pair.first);
                if (it != end(*here.singletons))
                    here.singletons->erase(it);
                else
                    here.singletons->insert(pair);
            }
        }
        else if (here.isIntersection() && there.isUnion())
        {
            for (const auto& [name, ty] : *there.singletons)
                here.singletons->erase(name);
        }
        else if (here.isIntersection() && there.isIntersection())
        {
            auto iter = begin(*here.singletons);
            auto endIter = end(*here.singletons);

            while (iter != endIter)
            {
                if (!there.singletons->count(iter->first))
                {
                    auto eraseIt = iter;
                    ++iter;
                    here.singletons->erase(eraseIt);
                }
                else
                    ++iter;
            }
        }
        else
            LUAU_ASSERT(!"Unreachable");
    }
    else
    {
        if (there.isString())
            here.resetToString();
        else if (here.isUnion())
            here.singletons->insert(there.singletons->begin(), there.singletons->end());
    }
}

std::optional<TypePackId> Normalizer::unionOfTypePacks(TypePackId here, TypePackId there)
{
    if (here == there)
        return here;

    std::vector<TypeId> head;
    std::optional<TypePackId> tail;

    bool hereSubThere = true;
    bool thereSubHere = true;

    TypePackIterator ith = begin(here);
    TypePackIterator itt = begin(there);

    while (ith != end(here) && itt != end(there))
    {
        TypeId hty = *ith;
        TypeId tty = *itt;
        TypeId ty = unionType(hty, tty);
        if (ty != hty)
            thereSubHere = false;
        if (ty != tty)
            hereSubThere = false;
        head.push_back(ty);
        ith++;
        itt++;
    }

    auto dealWithDifferentArities = [&](TypePackIterator& ith, TypePackIterator itt, TypePackId here, TypePackId there, bool& hereSubThere,
                                        bool& thereSubHere) {
        if (ith != end(here))
        {
            TypeId tty = singletonTypes->nilType;
            if (std::optional<TypePackId> ttail = itt.tail())
            {
                if (const VariadicTypePack* tvtp = get<VariadicTypePack>(*ttail))
                    tty = tvtp->ty;
                else
                    // Luau doesn't have unions of type pack variables
                    return false;
            }
            else
                // Type packs of different arities are incomparable
                return false;

            while (ith != end(here))
            {
                TypeId hty = *ith;
                TypeId ty = unionType(hty, tty);
                if (ty != hty)
                    thereSubHere = false;
                if (ty != tty)
                    hereSubThere = false;
                head.push_back(ty);
                ith++;
            }
        }
        return true;
    };

    if (!dealWithDifferentArities(ith, itt, here, there, hereSubThere, thereSubHere))
        return std::nullopt;

    if (!dealWithDifferentArities(itt, ith, there, here, thereSubHere, hereSubThere))
        return std::nullopt;

    if (std::optional<TypePackId> htail = ith.tail())
    {
        if (std::optional<TypePackId> ttail = itt.tail())
        {
            if (*htail == *ttail)
                tail = htail;
            else if (const VariadicTypePack* hvtp = get<VariadicTypePack>(*htail))
            {
                if (const VariadicTypePack* tvtp = get<VariadicTypePack>(*ttail))
                {
                    TypeId ty = unionType(hvtp->ty, tvtp->ty);
                    if (ty != hvtp->ty)
                        thereSubHere = false;
                    if (ty != tvtp->ty)
                        hereSubThere = false;
                    bool hidden = hvtp->hidden & tvtp->hidden;
                    tail = arena->addTypePack(VariadicTypePack{ty, hidden});
                }
                else
                    // Luau doesn't have unions of type pack variables
                    return std::nullopt;
            }
            else
                // Luau doesn't have unions of type pack variables
                return std::nullopt;
        }
        else if (get<VariadicTypePack>(*htail))
        {
            hereSubThere = false;
            tail = htail;
        }
        else
            // Luau doesn't have unions of type pack variables
            return std::nullopt;
    }
    else if (std::optional<TypePackId> ttail = itt.tail())
    {
        if (get<VariadicTypePack>(*ttail))
        {
            thereSubHere = false;
            tail = htail;
        }
        else
            // Luau doesn't have unions of type pack variables
            return std::nullopt;
    }

    if (hereSubThere)
        return there;
    else if (thereSubHere)
        return here;
    if (!head.empty())
        return arena->addTypePack(TypePack{head, tail});
    else if (tail)
        return *tail;
    else
        // TODO: Add an emptyPack to singleton types
        return arena->addTypePack({});
}

std::optional<TypeId> Normalizer::unionOfFunctions(TypeId here, TypeId there)
{
    if (get<ErrorTypeVar>(here))
        return here;

    if (get<ErrorTypeVar>(there))
        return there;

    const FunctionTypeVar* hftv = get<FunctionTypeVar>(here);
    LUAU_ASSERT(hftv);
    const FunctionTypeVar* tftv = get<FunctionTypeVar>(there);
    LUAU_ASSERT(tftv);

    if (hftv->generics != tftv->generics)
        return std::nullopt;
    if (hftv->genericPacks != tftv->genericPacks)
        return std::nullopt;

    std::optional<TypePackId> argTypes = intersectionOfTypePacks(hftv->argTypes, tftv->argTypes);
    if (!argTypes)
        return std::nullopt;

    std::optional<TypePackId> retTypes = unionOfTypePacks(hftv->retTypes, tftv->retTypes);
    if (!retTypes)
        return std::nullopt;

    if (*argTypes == hftv->argTypes && *retTypes == hftv->retTypes)
        return here;
    if (*argTypes == tftv->argTypes && *retTypes == tftv->retTypes)
        return there;

    FunctionTypeVar result{*argTypes, *retTypes};
    result.generics = hftv->generics;
    result.genericPacks = hftv->genericPacks;
    return arena->addType(std::move(result));
}

void Normalizer::unionFunctions(NormalizedFunctionType& heres, const NormalizedFunctionType& theres)
{
    if (FFlag::LuauNegatedFunctionTypes)
    {
        if (heres.isTop)
            return;
        if (theres.isTop)
            heres.resetToTop();
    }

    if (theres.isNever())
        return;

    TypeIds tmps;

    if (heres.isNever())
    {
        tmps.insert(theres.parts->begin(), theres.parts->end());
        heres.parts = std::move(tmps);
        return;
    }

    for (TypeId here : *heres.parts)
        for (TypeId there : *theres.parts)
        {
            if (std::optional<TypeId> fun = unionOfFunctions(here, there))
                tmps.insert(*fun);
            else
                tmps.insert(singletonTypes->errorRecoveryType(there));
        }

    heres.parts = std::move(tmps);
}

void Normalizer::unionFunctionsWithFunction(NormalizedFunctionType& heres, TypeId there)
{
    if (heres.isNever())
    {
        TypeIds tmps;
        tmps.insert(there);
        heres.parts = std::move(tmps);
        return;
    }

    TypeIds tmps;
    for (TypeId here : *heres.parts)
    {
        if (std::optional<TypeId> fun = unionOfFunctions(here, there))
            tmps.insert(*fun);
        else
            tmps.insert(singletonTypes->errorRecoveryType(there));
    }
    heres.parts = std::move(tmps);
}

void Normalizer::unionTablesWithTable(TypeIds& heres, TypeId there)
{
    // TODO: remove unions of tables where possible
    heres.insert(there);
}

void Normalizer::unionTables(TypeIds& heres, const TypeIds& theres)
{
    for (TypeId there : theres)
        unionTablesWithTable(heres, there);
}

// So why `ignoreSmallerTyvars`?
//
// First up, what it does... Every tyvar has an index, and this parameter says to ignore
// any tyvars in `there` if their index is less than or equal to the parameter.
// The parameter is always greater than any tyvars mentioned in here, so the result is
// a lower bound on any tyvars in `here.tyvars`.
//
// This is used to maintain in invariant, which is that in any tyvar `X&T`, any any tyvar
// `Y&U` in `T`, the index of `X` is less than the index of `Y`. This is an implementation
// of *ordered decision diagrams* (https://en.wikipedia.org/wiki/Binary_decision_diagram#Variable_ordering)
// which are a compression technique used to save memory usage when representing boolean formulae.
//
// The idea is that if you have an out-of-order decision diagram
// like `Z&(X|Y)`, to re-order it in this case to `(X&Z)|(Y&Z)`.
// The hope is that by imposing a global order, there's a higher chance of sharing opportunities,
// and hence reduced memory.
//
// And yes, this is essentially a SAT solver hidden inside a typechecker.
// That's what you get for having a type system with generics, intersection and union types.
bool Normalizer::unionNormals(NormalizedType& here, const NormalizedType& there, int ignoreSmallerTyvars)
{
    TypeId tops = unionOfTops(here.tops, there.tops);
    if (!get<NeverTypeVar>(tops))
    {
        clearNormal(here);
        here.tops = tops;
        return true;
    }

    for (auto it = there.tyvars.begin(); it != there.tyvars.end(); it++)
    {
        TypeId tyvar = it->first;
        const NormalizedType& inter = *it->second;
        int index = tyvarIndex(tyvar);
        if (index <= ignoreSmallerTyvars)
            continue;
        auto [emplaced, fresh] = here.tyvars.emplace(tyvar, std::make_unique<NormalizedType>(NormalizedType{singletonTypes}));
        if (fresh)
            if (!unionNormals(*emplaced->second, here, index))
                return false;
        if (!unionNormals(*emplaced->second, inter, index))
            return false;
    }

    here.booleans = unionOfBools(here.booleans, there.booleans);
    unionClasses(here.classes, there.classes);
    here.errors = (get<NeverTypeVar>(there.errors) ? here.errors : there.errors);
    here.nils = (get<NeverTypeVar>(there.nils) ? here.nils : there.nils);
    here.numbers = (get<NeverTypeVar>(there.numbers) ? here.numbers : there.numbers);
    unionStrings(here.strings, there.strings);
    here.threads = (get<NeverTypeVar>(there.threads) ? here.threads : there.threads);
    unionFunctions(here.functions, there.functions);
    unionTables(here.tables, there.tables);
    return true;
}

bool Normalizer::withinResourceLimits()
{
    // If cache is too large, clear it
    if (FInt::LuauNormalizeCacheLimit > 0)
    {
        size_t cacheUsage = cachedNormals.size() + cachedIntersections.size() + cachedUnions.size() + cachedTypeIds.size();
        if (cacheUsage > size_t(FInt::LuauNormalizeCacheLimit))
        {
            clearCaches();
            return false;
        }
    }

    // Check the recursion count
    if (sharedState->counters.recursionLimit > 0)
        if (sharedState->counters.recursionLimit < sharedState->counters.recursionCount)
            return false;

    return true;
}

// See above for an explaination of `ignoreSmallerTyvars`.
bool Normalizer::unionNormalWithTy(NormalizedType& here, TypeId there, int ignoreSmallerTyvars)
{
    RecursionCounter _rc(&sharedState->counters.recursionCount);
    if (!withinResourceLimits())
        return false;

    there = follow(there);
    if (get<AnyTypeVar>(there) || get<UnknownTypeVar>(there))
    {
        TypeId tops = unionOfTops(here.tops, there);
        clearNormal(here);
        here.tops = tops;
        return true;
    }
    else if (get<NeverTypeVar>(there) || !get<NeverTypeVar>(here.tops))
        return true;
    else if (const UnionTypeVar* utv = get<UnionTypeVar>(there))
    {
        for (UnionTypeVarIterator it = begin(utv); it != end(utv); ++it)
            if (!unionNormalWithTy(here, *it))
                return false;
        return true;
    }
    else if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(there))
    {
        NormalizedType norm{singletonTypes};
        norm.tops = singletonTypes->anyType;
        for (IntersectionTypeVarIterator it = begin(itv); it != end(itv); ++it)
            if (!intersectNormalWithTy(norm, *it))
                return false;
        return unionNormals(here, norm);
    }
    else if (get<GenericTypeVar>(there) || get<FreeTypeVar>(there))
    {
        if (tyvarIndex(there) <= ignoreSmallerTyvars)
            return true;
        NormalizedType inter{singletonTypes};
        inter.tops = singletonTypes->unknownType;
        here.tyvars.insert_or_assign(there, std::make_unique<NormalizedType>(std::move(inter)));
    }
    else if (get<FunctionTypeVar>(there))
        unionFunctionsWithFunction(here.functions, there);
    else if (get<TableTypeVar>(there) || get<MetatableTypeVar>(there))
        unionTablesWithTable(here.tables, there);
    else if (get<ClassTypeVar>(there))
        unionClassesWithClass(here.classes, there);
    else if (get<ErrorTypeVar>(there))
        here.errors = there;
    else if (const PrimitiveTypeVar* ptv = get<PrimitiveTypeVar>(there))
    {
        if (ptv->type == PrimitiveTypeVar::Boolean)
            here.booleans = there;
        else if (ptv->type == PrimitiveTypeVar::NilType)
            here.nils = there;
        else if (ptv->type == PrimitiveTypeVar::Number)
            here.numbers = there;
        else if (ptv->type == PrimitiveTypeVar::String)
            here.strings.resetToString();
        else if (ptv->type == PrimitiveTypeVar::Thread)
            here.threads = there;
        else if (ptv->type == PrimitiveTypeVar::Function)
        {
            LUAU_ASSERT(FFlag::LuauNegatedFunctionTypes);
            here.functions.resetToTop();
        }
        else
            LUAU_ASSERT(!"Unreachable");
    }
    else if (const SingletonTypeVar* stv = get<SingletonTypeVar>(there))
    {
        if (get<BooleanSingleton>(stv))
            here.booleans = unionOfBools(here.booleans, there);
        else if (const StringSingleton* sstv = get<StringSingleton>(stv))
        {
            if (FFlag::LuauNegatedStringSingletons)
            {
                if (here.strings.isCofinite)
                {
                    auto it = here.strings.singletons->find(sstv->value);
                    if (it != here.strings.singletons->end())
                        here.strings.singletons->erase(it);
                }
                else
                    here.strings.singletons->insert({sstv->value, there});
            }
            else
            {
                if (here.strings.isUnion())
                    here.strings.singletons->insert({sstv->value, there});
            }
        }
        else
            LUAU_ASSERT(!"Unreachable");
    }
    else if (const NegationTypeVar* ntv = get<NegationTypeVar>(there))
    {
        const NormalizedType* thereNormal = normalize(ntv->ty);
        std::optional<NormalizedType> tn = negateNormal(*thereNormal);
        if (!tn)
            return false;

        if (!unionNormals(here, *tn))
            return false;
    }
    else
        LUAU_ASSERT(!"Unreachable");

    for (auto& [tyvar, intersect] : here.tyvars)
        if (!unionNormalWithTy(*intersect, there, tyvarIndex(tyvar)))
            return false;

    assertInvariant(here);
    return true;
}

// ------- Negations

std::optional<NormalizedType> Normalizer::negateNormal(const NormalizedType& here)
{
    NormalizedType result{singletonTypes};
    if (!get<NeverTypeVar>(here.tops))
    {
        // The negation of unknown or any is never.  Easy.
        return result;
    }

    if (!get<NeverTypeVar>(here.errors))
    {
        // Negating an error yields the same error.
        result.errors = here.errors;
        return result;
    }

    if (get<NeverTypeVar>(here.booleans))
        result.booleans = singletonTypes->booleanType;
    else if (get<PrimitiveTypeVar>(here.booleans))
        result.booleans = singletonTypes->neverType;
    else if (auto stv = get<SingletonTypeVar>(here.booleans))
    {
        auto boolean = get<BooleanSingleton>(stv);
        LUAU_ASSERT(boolean != nullptr);
        if (boolean->value)
            result.booleans = singletonTypes->falseType;
        else
            result.booleans = singletonTypes->trueType;
    }

    result.classes = negateAll(here.classes);
    result.nils = get<NeverTypeVar>(here.nils) ? singletonTypes->nilType : singletonTypes->neverType;
    result.numbers = get<NeverTypeVar>(here.numbers) ? singletonTypes->numberType : singletonTypes->neverType;

    result.strings = here.strings;
    result.strings.isCofinite = !result.strings.isCofinite;

    result.threads = get<NeverTypeVar>(here.threads) ? singletonTypes->threadType : singletonTypes->neverType;

    /*
     * Things get weird and so, so complicated if we allow negations of
     * arbitrary function types.  Ordinary code can never form these kinds of
     * types, so we decline to negate them.
     */
    if (FFlag::LuauNegatedFunctionTypes)
    {
        if (here.functions.isNever())
            result.functions.resetToTop();
        else if (here.functions.isTop)
            result.functions.resetToNever();
        else
            return std::nullopt;
    }

    // TODO: negating tables
    // TODO: negating tyvars?

    return result;
}

TypeIds Normalizer::negateAll(const TypeIds& theres)
{
    TypeIds tys;
    for (TypeId there : theres)
        tys.insert(negate(there));
    return tys;
}

TypeId Normalizer::negate(TypeId there)
{
    there = follow(there);
    if (get<AnyTypeVar>(there))
        return there;
    else if (get<UnknownTypeVar>(there))
        return singletonTypes->neverType;
    else if (get<NeverTypeVar>(there))
        return singletonTypes->unknownType;
    else if (auto ntv = get<NegationTypeVar>(there))
        return ntv->ty; // TODO: do we want to normalize this?
    else if (auto utv = get<UnionTypeVar>(there))
    {
        std::vector<TypeId> parts;
        for (TypeId option : utv)
            parts.push_back(negate(option));
        return arena->addType(IntersectionTypeVar{std::move(parts)});
    }
    else if (auto itv = get<IntersectionTypeVar>(there))
    {
        std::vector<TypeId> options;
        for (TypeId part : itv)
            options.push_back(negate(part));
        return arena->addType(UnionTypeVar{std::move(options)});
    }
    else
        return there;
}

void Normalizer::subtractPrimitive(NormalizedType& here, TypeId ty)
{
    const PrimitiveTypeVar* ptv = get<PrimitiveTypeVar>(follow(ty));
    LUAU_ASSERT(ptv);
    switch (ptv->type)
    {
    case PrimitiveTypeVar::NilType:
        here.nils = singletonTypes->neverType;
        break;
    case PrimitiveTypeVar::Boolean:
        here.booleans = singletonTypes->neverType;
        break;
    case PrimitiveTypeVar::Number:
        here.numbers = singletonTypes->neverType;
        break;
    case PrimitiveTypeVar::String:
        here.strings.resetToNever();
        break;
    case PrimitiveTypeVar::Thread:
        here.threads = singletonTypes->neverType;
        break;
    case PrimitiveTypeVar::Function:
        LUAU_ASSERT(FFlag::LuauNegatedStringSingletons);
        here.functions.resetToNever();
        break;
    }
}

void Normalizer::subtractSingleton(NormalizedType& here, TypeId ty)
{
    LUAU_ASSERT(FFlag::LuauNegatedStringSingletons);

    const SingletonTypeVar* stv = get<SingletonTypeVar>(ty);
    LUAU_ASSERT(stv);

    if (const StringSingleton* ss = get<StringSingleton>(stv))
    {
        if (here.strings.isCofinite)
            here.strings.singletons->insert({ss->value, ty});
        else
        {
            auto it = here.strings.singletons->find(ss->value);
            if (it != here.strings.singletons->end())
                here.strings.singletons->erase(it);
        }
    }
    else if (const BooleanSingleton* bs = get<BooleanSingleton>(stv))
    {
        if (get<NeverTypeVar>(here.booleans))
        {
            // Nothing
        }
        else if (get<PrimitiveTypeVar>(here.booleans))
            here.booleans = bs->value ? singletonTypes->falseType : singletonTypes->trueType;
        else if (auto hereSingleton = get<SingletonTypeVar>(here.booleans))
        {
            const BooleanSingleton* hereBooleanSingleton = get<BooleanSingleton>(hereSingleton);
            LUAU_ASSERT(hereBooleanSingleton);

            // Crucial subtlety: ty (and thus bs) are the value that is being
            // negated out. We therefore reduce to never when the values match,
            // rather than when they differ.
            if (bs->value == hereBooleanSingleton->value)
                here.booleans = singletonTypes->neverType;
        }
        else
            LUAU_ASSERT(!"Unreachable");
    }
    else
        LUAU_ASSERT(!"Unreachable");
}

// ------- Normalizing intersections
TypeId Normalizer::intersectionOfTops(TypeId here, TypeId there)
{
    if (get<NeverTypeVar>(here) || get<AnyTypeVar>(there))
        return here;
    else
        return there;
}

TypeId Normalizer::intersectionOfBools(TypeId here, TypeId there)
{
    if (get<NeverTypeVar>(here))
        return here;
    if (get<NeverTypeVar>(there))
        return there;
    if (const BooleanSingleton* hbool = get<BooleanSingleton>(get<SingletonTypeVar>(here)))
        if (const BooleanSingleton* tbool = get<BooleanSingleton>(get<SingletonTypeVar>(there)))
            return (hbool->value == tbool->value ? here : singletonTypes->neverType);
        else
            return here;
    else
        return there;
}

void Normalizer::intersectClasses(TypeIds& heres, const TypeIds& theres)
{
    TypeIds tmp;
    for (auto it = heres.begin(); it != heres.end();)
    {
        const ClassTypeVar* hctv = get<ClassTypeVar>(*it);
        LUAU_ASSERT(hctv);
        bool keep = false;
        for (TypeId there : theres)
        {
            const ClassTypeVar* tctv = get<ClassTypeVar>(there);
            LUAU_ASSERT(tctv);
            if (isSubclass(hctv, tctv))
            {
                keep = true;
                break;
            }
            else if (isSubclass(tctv, hctv))
            {
                keep = false;
                tmp.insert(there);
                break;
            }
        }
        if (keep)
            it++;
        else
            it = heres.erase(it);
    }
    heres.insert(tmp.begin(), tmp.end());
}

void Normalizer::intersectClassesWithClass(TypeIds& heres, TypeId there)
{
    bool foundSuper = false;
    const ClassTypeVar* tctv = get<ClassTypeVar>(there);
    LUAU_ASSERT(tctv);
    for (auto it = heres.begin(); it != heres.end();)
    {
        const ClassTypeVar* hctv = get<ClassTypeVar>(*it);
        LUAU_ASSERT(hctv);
        if (isSubclass(hctv, tctv))
            it++;
        else if (isSubclass(tctv, hctv))
        {
            foundSuper = true;
            break;
        }
        else
            it = heres.erase(it);
    }
    if (foundSuper)
    {
        heres.clear();
        heres.insert(there);
    }
}

void Normalizer::intersectStrings(NormalizedStringType& here, const NormalizedStringType& there)
{
    if (there.isString())
        return;
    if (here.isString())
        here.resetToNever();

    for (auto it = here.singletons->begin(); it != here.singletons->end();)
    {
        if (there.singletons->count(it->first))
            it++;
        else
            it = here.singletons->erase(it);
    }
}

std::optional<TypePackId> Normalizer::intersectionOfTypePacks(TypePackId here, TypePackId there)
{
    if (here == there)
        return here;

    std::vector<TypeId> head;
    std::optional<TypePackId> tail;

    bool hereSubThere = true;
    bool thereSubHere = true;

    TypePackIterator ith = begin(here);
    TypePackIterator itt = begin(there);

    while (ith != end(here) && itt != end(there))
    {
        TypeId hty = *ith;
        TypeId tty = *itt;
        TypeId ty = intersectionType(hty, tty);
        if (ty != hty)
            hereSubThere = false;
        if (ty != tty)
            thereSubHere = false;
        head.push_back(ty);
        ith++;
        itt++;
    }

    auto dealWithDifferentArities = [&](TypePackIterator& ith, TypePackIterator itt, TypePackId here, TypePackId there, bool& hereSubThere,
                                        bool& thereSubHere) {
        if (ith != end(here))
        {
            TypeId tty = singletonTypes->nilType;
            if (std::optional<TypePackId> ttail = itt.tail())
            {
                if (const VariadicTypePack* tvtp = get<VariadicTypePack>(*ttail))
                    tty = tvtp->ty;
                else
                    // Luau doesn't have intersections of type pack variables
                    return false;
            }
            else
                // Type packs of different arities are incomparable
                return false;

            while (ith != end(here))
            {
                TypeId hty = *ith;
                TypeId ty = intersectionType(hty, tty);
                if (ty != hty)
                    hereSubThere = false;
                if (ty != tty)
                    thereSubHere = false;
                head.push_back(ty);
                ith++;
            }
        }
        return true;
    };

    if (!dealWithDifferentArities(ith, itt, here, there, hereSubThere, thereSubHere))
        return std::nullopt;

    if (!dealWithDifferentArities(itt, ith, there, here, thereSubHere, hereSubThere))
        return std::nullopt;

    if (std::optional<TypePackId> htail = ith.tail())
    {
        if (std::optional<TypePackId> ttail = itt.tail())
        {
            if (*htail == *ttail)
                tail = htail;
            else if (const VariadicTypePack* hvtp = get<VariadicTypePack>(*htail))
            {
                if (const VariadicTypePack* tvtp = get<VariadicTypePack>(*ttail))
                {
                    TypeId ty = intersectionType(hvtp->ty, tvtp->ty);
                    if (ty != hvtp->ty)
                        thereSubHere = false;
                    if (ty != tvtp->ty)
                        hereSubThere = false;
                    bool hidden = hvtp->hidden & tvtp->hidden;
                    tail = arena->addTypePack(VariadicTypePack{ty, hidden});
                }
                else
                    // Luau doesn't have unions of type pack variables
                    return std::nullopt;
            }
            else
                // Luau doesn't have unions of type pack variables
                return std::nullopt;
        }
        else if (get<VariadicTypePack>(*htail))
            hereSubThere = false;
        else
            // Luau doesn't have unions of type pack variables
            return std::nullopt;
    }
    else if (std::optional<TypePackId> ttail = itt.tail())
    {
        if (get<VariadicTypePack>(*ttail))
            thereSubHere = false;
        else
            // Luau doesn't have unions of type pack variables
            return std::nullopt;
    }

    if (hereSubThere)
        return here;
    else if (thereSubHere)
        return there;
    if (!head.empty())
        return arena->addTypePack(TypePack{head, tail});
    else if (tail)
        return *tail;
    else
        // TODO: Add an emptyPack to singleton types
        return arena->addTypePack({});
}

std::optional<TypeId> Normalizer::intersectionOfTables(TypeId here, TypeId there)
{
    if (here == there)
        return here;

    RecursionCounter _rc(&sharedState->counters.recursionCount);
    if (sharedState->counters.recursionLimit > 0 && sharedState->counters.recursionLimit < sharedState->counters.recursionCount)
        return std::nullopt;

    TypeId htable = here;
    TypeId hmtable = nullptr;
    if (const MetatableTypeVar* hmtv = get<MetatableTypeVar>(here))
    {
        htable = hmtv->table;
        hmtable = hmtv->metatable;
    }
    TypeId ttable = there;
    TypeId tmtable = nullptr;
    if (const MetatableTypeVar* tmtv = get<MetatableTypeVar>(there))
    {
        ttable = tmtv->table;
        tmtable = tmtv->metatable;
    }

    const TableTypeVar* httv = get<TableTypeVar>(htable);
    LUAU_ASSERT(httv);
    const TableTypeVar* tttv = get<TableTypeVar>(ttable);
    LUAU_ASSERT(tttv);

    if (httv->state == TableState::Free || tttv->state == TableState::Free)
        return std::nullopt;
    if (httv->state == TableState::Generic || tttv->state == TableState::Generic)
        return std::nullopt;

    TableState state = httv->state;
    if (tttv->state == TableState::Unsealed)
        state = tttv->state;

    TypeLevel level = max(httv->level, tttv->level);
    TableTypeVar result{state, level};

    bool hereSubThere = true;
    bool thereSubHere = true;

    for (const auto& [name, hprop] : httv->props)
    {
        Property prop = hprop;
        auto tfound = tttv->props.find(name);
        if (tfound == tttv->props.end())
            thereSubHere = false;
        else
        {
            const auto& [_name, tprop] = *tfound;
            // TODO: variance issues here, which can't be fixed until we have read/write property types
            prop.type = intersectionType(hprop.type, tprop.type);
            hereSubThere &= (prop.type == hprop.type);
            thereSubHere &= (prop.type == tprop.type);
        }
        // TODO: string indexers
        result.props[name] = prop;
    }

    for (const auto& [name, tprop] : tttv->props)
    {
        if (httv->props.count(name) == 0)
        {
            result.props[name] = tprop;
            hereSubThere = false;
        }
    }

    if (httv->indexer && tttv->indexer)
    {
        // TODO: What should intersection of indexes be?
        TypeId index = unionType(httv->indexer->indexType, tttv->indexer->indexType);
        TypeId indexResult = intersectionType(httv->indexer->indexResultType, tttv->indexer->indexResultType);
        result.indexer = {index, indexResult};
        hereSubThere &= (httv->indexer->indexType == index) && (httv->indexer->indexResultType == indexResult);
        thereSubHere &= (tttv->indexer->indexType == index) && (tttv->indexer->indexResultType == indexResult);
    }
    else if (httv->indexer)
    {
        result.indexer = httv->indexer;
        thereSubHere = false;
    }
    else if (tttv->indexer)
    {
        result.indexer = tttv->indexer;
        hereSubThere = false;
    }

    TypeId table;
    if (hereSubThere)
        table = htable;
    else if (thereSubHere)
        table = ttable;
    else
        table = arena->addType(std::move(result));

    if (tmtable && hmtable)
    {
        // NOTE: this assumes metatables are ivariant
        if (std::optional<TypeId> mtable = intersectionOfTables(hmtable, tmtable))
        {
            if (table == htable && *mtable == hmtable)
                return here;
            else if (table == ttable && *mtable == tmtable)
                return there;
            else
                return arena->addType(MetatableTypeVar{table, *mtable});
        }
        else
            return std::nullopt;
    }
    else if (hmtable)
    {
        if (table == htable)
            return here;
        else
            return arena->addType(MetatableTypeVar{table, hmtable});
    }
    else if (tmtable)
    {
        if (table == ttable)
            return there;
        else
            return arena->addType(MetatableTypeVar{table, tmtable});
    }
    else
        return table;
}

void Normalizer::intersectTablesWithTable(TypeIds& heres, TypeId there)
{
    TypeIds tmp;
    for (TypeId here : heres)
        if (std::optional<TypeId> inter = intersectionOfTables(here, there))
            tmp.insert(*inter);
    heres.retain(tmp);
    heres.insert(tmp.begin(), tmp.end());
}

void Normalizer::intersectTables(TypeIds& heres, const TypeIds& theres)
{
    TypeIds tmp;
    for (TypeId here : heres)
        for (TypeId there : theres)
            if (std::optional<TypeId> inter = intersectionOfTables(here, there))
                tmp.insert(*inter);
    heres.retain(tmp);
    heres.insert(tmp.begin(), tmp.end());
}

std::optional<TypeId> Normalizer::intersectionOfFunctions(TypeId here, TypeId there)
{
    const FunctionTypeVar* hftv = get<FunctionTypeVar>(here);
    LUAU_ASSERT(hftv);
    const FunctionTypeVar* tftv = get<FunctionTypeVar>(there);
    LUAU_ASSERT(tftv);

    if (hftv->generics != tftv->generics)
        return std::nullopt;
    if (hftv->genericPacks != tftv->genericPacks)
        return std::nullopt;

    TypePackId argTypes;
    TypePackId retTypes;

    if (hftv->retTypes == tftv->retTypes)
    {
        std::optional<TypePackId> argTypesOpt = unionOfTypePacks(hftv->argTypes, tftv->argTypes);
        if (!argTypesOpt)
            return std::nullopt;
        argTypes = *argTypesOpt;
        retTypes = hftv->retTypes;
    }
    else if (FFlag::LuauOverloadedFunctionSubtypingPerf && hftv->argTypes == tftv->argTypes)
    {
        std::optional<TypePackId> retTypesOpt = intersectionOfTypePacks(hftv->argTypes, tftv->argTypes);
        if (!retTypesOpt)
            return std::nullopt;
        argTypes = hftv->argTypes;
        retTypes = *retTypesOpt;
    }
    else
        return std::nullopt;

    if (argTypes == hftv->argTypes && retTypes == hftv->retTypes)
        return here;
    if (argTypes == tftv->argTypes && retTypes == tftv->retTypes)
        return there;

    FunctionTypeVar result{argTypes, retTypes};
    result.generics = hftv->generics;
    result.genericPacks = hftv->genericPacks;
    return arena->addType(std::move(result));
}

std::optional<TypeId> Normalizer::unionSaturatedFunctions(TypeId here, TypeId there)
{
    // Deep breath...
    //
    // When we come to check overloaded functions for subtyping,
    // we have to compare (F1 & ... & FM) <: (G1 & ... G GN)
    // where each Fi or Gj is a function type. Now that intersection on the right is no
    // problem, since that's true if and only if (F1 & ... & FM) <: Gj for every j.
    // But the intersection on the left is annoying, since we might have
    // (F1 & ... & FM) <: G but no Fi <: G.  For example
    //
    //   ((number? -> number?) & (string? -> string?)) <: (nil -> nil)
    //
    // So in this case, what we do is define Apply<F, T> for the result of applying
    // a function of type F to an argument of type T, and then F <: (T -> U)
    // if and only if Apply<F, T> <: U. For example:
    //
    //   if f : ((number? -> number?) & (string? -> string?))
    //   then f(nil) must be nil, so
    //   Apply<((number? -> number?) & (string? -> string?)), nil> is nil
    //
    // So subtyping on overloaded functions "just" boils down to defining Apply<F, T>.
    //
    // Now for non-overloaded functions, this is easy!
    // Apply<(R -> S), T> is S if T <: R, and an error type otherwise.
    //
    // But for overloaded functions it's not so simple. We'd like Apply<F1 & ... & FM, T>
    // to just be Apply<F1, T> & ... & Apply<FM, T> but oh dear
    //
    //   if f : ((number -> number) & (string -> string))
    //   and x : (number | string)
    //   then f(x) : (number | string)
    //
    // so we want
    //
    //   Apply<((number -> number) & (string -> string)), (number | string)> is (number | string)
    //
    // but
    //
    //   Apply<(number -> number), (number | string)> is an error
    //   Apply<(string -> string), (number | string)> is an error
    //
    // that is Apply<F, T> should consider all possible combinations of overloads of F,
    // not just individual overloads.
    //
    // For this reason, when we're normalizing function types (in order to check subtyping
    // or perform overload resolution) we should first *union-saturate* them. An overloaded
    // function is union-saturated whenever:
    //
    //    if (R -> S) is an overload of F
    //    and (T -> U) is an overload of F
    //    then ((R | T) -> (S | U)) is a subtype of an overload of F
    //
    // Any overloaded function can be normalized to a union-saturated one by adding enough extra overloads.
    // For example, union-saturating
    //
    //   ((number -> number) & (string -> string))
    //
    // is
    //
    //   ((number -> number) & (string -> string) & ((number | string) -> (number | string)))
    //
    // For union-saturated overloaded functions, the "obvious" algorithm works:
    //
    //   Apply<F1 & ... & FM, T>  is   Apply<F1, T> & ... & Apply<FM, T>
    //
    // so we can define Apply, so we can perform overloaded function resolution
    // and check subtyping on overloaded function types, yay!
    //
    // This is yet another potential source of exponential blow-up, sigh, since
    // the union-saturation of a function with N overloads may have 2^N overloads
    // (one for every subset). In practice, that hopefully won't happen that often,
    // in particular we only union-saturate overloads with different return types,
    // and there are hopefully not very many cases of that.
    //
    // All of this is mechanically verified in Agda, at https://github.com/luau-lang/agda-typeck
    //
    // It is essentially the algorithm defined in https://pnwamk.github.io/sst-tutorial/
    // except that we're precomputing the union-saturation rather than converting
    // to disjunctive normal form on the fly.
    //
    // This is all built on semantic subtyping:
    //
    //   Covariance and Contravariance, Giuseppe Castagna,
    //   Logical Methods in Computer Science 16(1), 2022
    //   https://arxiv.org/abs/1809.01427
    //
    //   A gentle introduction to semantic subtyping, Giuseppe Castagna and Alain Frisch,
    //   Proc. Principles and practice of declarative programming 2005, pp 198–208
    //   https://doi.org/10.1145/1069774.1069793

    const FunctionTypeVar* hftv = get<FunctionTypeVar>(here);
    if (!hftv)
        return std::nullopt;
    const FunctionTypeVar* tftv = get<FunctionTypeVar>(there);
    if (!tftv)
        return std::nullopt;

    if (hftv->generics != tftv->generics)
        return std::nullopt;
    if (hftv->genericPacks != tftv->genericPacks)
        return std::nullopt;

    std::optional<TypePackId> argTypes = unionOfTypePacks(hftv->argTypes, tftv->argTypes);
    if (!argTypes)
        return std::nullopt;
    std::optional<TypePackId> retTypes = unionOfTypePacks(hftv->retTypes, tftv->retTypes);
    if (!retTypes)
        return std::nullopt;

    FunctionTypeVar result{*argTypes, *retTypes};
    result.generics = hftv->generics;
    result.genericPacks = hftv->genericPacks;
    return arena->addType(std::move(result));
}

void Normalizer::intersectFunctionsWithFunction(NormalizedFunctionType& heres, TypeId there)
{
    if (heres.isNever())
        return;

    heres.isTop = false;

    for (auto it = heres.parts->begin(); it != heres.parts->end();)
    {
        TypeId here = *it;
        if (get<ErrorTypeVar>(here))
            it++;
        else if (std::optional<TypeId> tmp = intersectionOfFunctions(here, there))
        {
            heres.parts->erase(it);
            heres.parts->insert(*tmp);
            return;
        }
        else
            it++;
    }

    TypeIds tmps;
    for (TypeId here : *heres.parts)
    {
        if (std::optional<TypeId> tmp = unionSaturatedFunctions(here, there))
            tmps.insert(*tmp);
    }
    heres.parts->insert(there);
    heres.parts->insert(tmps.begin(), tmps.end());
}

void Normalizer::intersectFunctions(NormalizedFunctionType& heres, const NormalizedFunctionType& theres)
{
    if (heres.isNever())
        return;
    else if (theres.isNever())
    {
        heres.resetToNever();
        return;
    }
    else
    {
        for (TypeId there : *theres.parts)
            intersectFunctionsWithFunction(heres, there);
    }
}

bool Normalizer::intersectTyvarsWithTy(NormalizedTyvars& here, TypeId there)
{
    for (auto it = here.begin(); it != here.end();)
    {
        NormalizedType& inter = *it->second;
        if (!intersectNormalWithTy(inter, there))
            return false;
        if (isInhabited(inter))
            ++it;
        else
            it = here.erase(it);
    }
    return true;
}

// See above for an explaination of `ignoreSmallerTyvars`.
bool Normalizer::intersectNormals(NormalizedType& here, const NormalizedType& there, int ignoreSmallerTyvars)
{
    if (!get<NeverTypeVar>(there.tops))
    {
        here.tops = intersectionOfTops(here.tops, there.tops);
        return true;
    }
    else if (!get<NeverTypeVar>(here.tops))
    {
        clearNormal(here);
        return unionNormals(here, there, ignoreSmallerTyvars);
    }

    here.booleans = intersectionOfBools(here.booleans, there.booleans);
    intersectClasses(here.classes, there.classes);
    here.errors = (get<NeverTypeVar>(there.errors) ? there.errors : here.errors);
    here.nils = (get<NeverTypeVar>(there.nils) ? there.nils : here.nils);
    here.numbers = (get<NeverTypeVar>(there.numbers) ? there.numbers : here.numbers);
    intersectStrings(here.strings, there.strings);
    here.threads = (get<NeverTypeVar>(there.threads) ? there.threads : here.threads);
    intersectFunctions(here.functions, there.functions);
    intersectTables(here.tables, there.tables);

    for (auto& [tyvar, inter] : there.tyvars)
    {
        int index = tyvarIndex(tyvar);
        if (ignoreSmallerTyvars < index)
        {
            auto [found, fresh] = here.tyvars.emplace(tyvar, std::make_unique<NormalizedType>(NormalizedType{singletonTypes}));
            if (fresh)
            {
                if (!unionNormals(*found->second, here, index))
                    return false;
            }
        }
    }
    for (auto it = here.tyvars.begin(); it != here.tyvars.end();)
    {
        TypeId tyvar = it->first;
        NormalizedType& inter = *it->second;
        int index = tyvarIndex(tyvar);
        LUAU_ASSERT(ignoreSmallerTyvars < index);
        auto found = there.tyvars.find(tyvar);
        if (found == there.tyvars.end())
        {
            if (!intersectNormals(inter, there, index))
                return false;
        }
        else
        {
            if (!intersectNormals(inter, *found->second, index))
                return false;
        }
        if (isInhabited(inter))
            it++;
        else
            it = here.tyvars.erase(it);
    }
    return true;
}

bool Normalizer::intersectNormalWithTy(NormalizedType& here, TypeId there)
{
    RecursionCounter _rc(&sharedState->counters.recursionCount);
    if (!withinResourceLimits())
        return false;

    there = follow(there);
    if (get<AnyTypeVar>(there) || get<UnknownTypeVar>(there))
    {
        here.tops = intersectionOfTops(here.tops, there);
        return true;
    }
    else if (!get<NeverTypeVar>(here.tops))
    {
        clearNormal(here);
        return unionNormalWithTy(here, there);
    }
    else if (const UnionTypeVar* utv = get<UnionTypeVar>(there))
    {
        NormalizedType norm{singletonTypes};
        for (UnionTypeVarIterator it = begin(utv); it != end(utv); ++it)
            if (!unionNormalWithTy(norm, *it))
                return false;
        return intersectNormals(here, norm);
    }
    else if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(there))
    {
        for (IntersectionTypeVarIterator it = begin(itv); it != end(itv); ++it)
            if (!intersectNormalWithTy(here, *it))
                return false;
        return true;
    }
    else if (get<GenericTypeVar>(there) || get<FreeTypeVar>(there))
    {
        NormalizedType thereNorm{singletonTypes};
        NormalizedType topNorm{singletonTypes};
        topNorm.tops = singletonTypes->unknownType;
        thereNorm.tyvars.insert_or_assign(there, std::make_unique<NormalizedType>(std::move(topNorm)));
        return intersectNormals(here, thereNorm);
    }

    NormalizedTyvars tyvars = std::move(here.tyvars);

    if (const FunctionTypeVar* utv = get<FunctionTypeVar>(there))
    {
        NormalizedFunctionType functions = std::move(here.functions);
        clearNormal(here);
        intersectFunctionsWithFunction(functions, there);
        here.functions = std::move(functions);
    }
    else if (get<TableTypeVar>(there) || get<MetatableTypeVar>(there))
    {
        TypeIds tables = std::move(here.tables);
        clearNormal(here);
        intersectTablesWithTable(tables, there);
        here.tables = std::move(tables);
    }
    else if (get<ClassTypeVar>(there))
    {
        TypeIds classes = std::move(here.classes);
        clearNormal(here);
        intersectClassesWithClass(classes, there);
        here.classes = std::move(classes);
    }
    else if (get<ErrorTypeVar>(there))
    {
        TypeId errors = here.errors;
        clearNormal(here);
        here.errors = errors;
    }
    else if (const PrimitiveTypeVar* ptv = get<PrimitiveTypeVar>(there))
    {
        TypeId booleans = here.booleans;
        TypeId nils = here.nils;
        TypeId numbers = here.numbers;
        NormalizedStringType strings = std::move(here.strings);
        NormalizedFunctionType functions = std::move(here.functions);
        TypeId threads = here.threads;

        clearNormal(here);

        if (ptv->type == PrimitiveTypeVar::Boolean)
            here.booleans = booleans;
        else if (ptv->type == PrimitiveTypeVar::NilType)
            here.nils = nils;
        else if (ptv->type == PrimitiveTypeVar::Number)
            here.numbers = numbers;
        else if (ptv->type == PrimitiveTypeVar::String)
            here.strings = std::move(strings);
        else if (ptv->type == PrimitiveTypeVar::Thread)
            here.threads = threads;
        else if (ptv->type == PrimitiveTypeVar::Function)
        {
            LUAU_ASSERT(FFlag::LuauNegatedFunctionTypes);
            here.functions = std::move(functions);
        }
        else
            LUAU_ASSERT(!"Unreachable");
    }
    else if (const SingletonTypeVar* stv = get<SingletonTypeVar>(there))
    {
        TypeId booleans = here.booleans;
        NormalizedStringType strings = std::move(here.strings);

        clearNormal(here);

        if (get<BooleanSingleton>(stv))
            here.booleans = intersectionOfBools(booleans, there);
        else if (const StringSingleton* sstv = get<StringSingleton>(stv))
        {
            if (strings.includes(sstv->value))
                here.strings.singletons->insert({sstv->value, there});
        }
        else
            LUAU_ASSERT(!"Unreachable");
    }
    else if (const NegationTypeVar* ntv = get<NegationTypeVar>(there); FFlag::LuauNegatedStringSingletons && ntv)
    {
        TypeId t = follow(ntv->ty);
        if (const PrimitiveTypeVar* ptv = get<PrimitiveTypeVar>(t))
            subtractPrimitive(here, ntv->ty);
        else if (const SingletonTypeVar* stv = get<SingletonTypeVar>(t))
            subtractSingleton(here, follow(ntv->ty));
        else if (const UnionTypeVar* itv = get<UnionTypeVar>(t))
        {
            for (TypeId part : itv->options)
            {
                const NormalizedType* normalPart = normalize(part);
                std::optional<NormalizedType> negated = negateNormal(*normalPart);
                if (!negated)
                    return false;
                intersectNormals(here, *negated);
            }
        }
        else
        {
            // TODO negated unions, intersections, table, and function.
            // Report a TypeError for other types.
            LUAU_ASSERT(!"Unimplemented");
        }
    }
    else
        LUAU_ASSERT(!"Unreachable");

    if (!intersectTyvarsWithTy(tyvars, there))
        return false;
    here.tyvars = std::move(tyvars);

    return true;
}

// -------- Convert back from a normalized type to a type
TypeId Normalizer::typeFromNormal(const NormalizedType& norm)
{
    assertInvariant(norm);
    if (!get<NeverTypeVar>(norm.tops))
        return norm.tops;

    std::vector<TypeId> result;

    if (!get<NeverTypeVar>(norm.booleans))
        result.push_back(norm.booleans);
    result.insert(result.end(), norm.classes.begin(), norm.classes.end());
    if (!get<NeverTypeVar>(norm.errors))
        result.push_back(norm.errors);
    if (FFlag::LuauNegatedFunctionTypes && norm.functions.isTop)
        result.push_back(singletonTypes->functionType);
    else if (!norm.functions.isNever())
    {
        if (norm.functions.parts->size() == 1)
            result.push_back(*norm.functions.parts->begin());
        else
        {
            std::vector<TypeId> parts;
            parts.insert(parts.end(), norm.functions.parts->begin(), norm.functions.parts->end());
            result.push_back(arena->addType(IntersectionTypeVar{std::move(parts)}));
        }
    }
    if (!get<NeverTypeVar>(norm.nils))
        result.push_back(norm.nils);
    if (!get<NeverTypeVar>(norm.numbers))
        result.push_back(norm.numbers);
    if (norm.strings.isString())
        result.push_back(singletonTypes->stringType);
    else if (norm.strings.isUnion())
    {
        for (auto& [_, ty] : *norm.strings.singletons)
            result.push_back(ty);
    }
    else if (FFlag::LuauNegatedStringSingletons && norm.strings.isIntersection())
    {
        std::vector<TypeId> parts;
        parts.push_back(singletonTypes->stringType);
        for (const auto& [name, ty] : *norm.strings.singletons)
            parts.push_back(arena->addType(NegationTypeVar{ty}));

        result.push_back(arena->addType(IntersectionTypeVar{std::move(parts)}));
    }
    if (!get<NeverTypeVar>(norm.threads))
        result.push_back(singletonTypes->threadType);

    result.insert(result.end(), norm.tables.begin(), norm.tables.end());
    for (auto& [tyvar, intersect] : norm.tyvars)
    {
        if (get<NeverTypeVar>(intersect->tops))
        {
            TypeId ty = typeFromNormal(*intersect);
            result.push_back(arena->addType(IntersectionTypeVar{{tyvar, ty}}));
        }
        else
            result.push_back(tyvar);
    }

    if (result.size() == 0)
        return singletonTypes->neverType;
    else if (result.size() == 1)
        return result[0];
    else
        return arena->addType(UnionTypeVar{std::move(result)});
}

bool isSubtype(TypeId subTy, TypeId superTy, NotNull<Scope> scope, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice)
{
    UnifierSharedState sharedState{&ice};
    TypeArena arena;
    Normalizer normalizer{&arena, singletonTypes, NotNull{&sharedState}};
    Unifier u{NotNull{&normalizer}, Mode::Strict, scope, Location{}, Covariant};

    u.tryUnify(subTy, superTy);
    const bool ok = u.errors.empty() && u.log.empty();
    return ok;
}

bool isSubtype(TypePackId subPack, TypePackId superPack, NotNull<Scope> scope, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice)
{
    UnifierSharedState sharedState{&ice};
    TypeArena arena;
    Normalizer normalizer{&arena, singletonTypes, NotNull{&sharedState}};
    Unifier u{NotNull{&normalizer}, Mode::Strict, scope, Location{}, Covariant};

    u.tryUnify(subPack, superPack);
    const bool ok = u.errors.empty() && u.log.empty();
    return ok;
}

} // namespace Luau
