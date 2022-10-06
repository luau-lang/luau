// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Normalize.h"
#include "Luau/ToString.h"

#include <algorithm>

#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/Unifier.h"
#include "Luau/VisitTypeVar.h"

LUAU_FASTFLAGVARIABLE(DebugLuauCopyBeforeNormalizing, false)
LUAU_FASTFLAGVARIABLE(DebugLuauCheckNormalizeInvariant, false)

// This could theoretically be 2000 on amd64, but x86 requires this.
LUAU_FASTINTVARIABLE(LuauNormalizeIterationLimit, 1200);
LUAU_FASTINTVARIABLE(LuauNormalizeCacheLimit, 100000);
LUAU_FASTINT(LuauTypeInferRecursionLimit);
LUAU_FASTFLAGVARIABLE(LuauNormalizeCombineTableFix, false);
LUAU_FASTFLAGVARIABLE(LuauTypeNormalization2, false);
LUAU_FASTFLAG(LuauUnknownAndNeverType)
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

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

NormalizedType::NormalizedType(NotNull<SingletonTypes> singletonTypes)
    : tops(singletonTypes->neverType)
    , booleans(singletonTypes->neverType)
    , errors(singletonTypes->neverType)
    , nils(singletonTypes->neverType)
    , numbers(singletonTypes->neverType)
    , threads(singletonTypes->neverType)
{
}

static bool isInhabited(const NormalizedType& norm)
{
    return !get<NeverTypeVar>(norm.tops)
        || !get<NeverTypeVar>(norm.booleans)
        || !norm.classes.empty()
        || !get<NeverTypeVar>(norm.errors)
        || !get<NeverTypeVar>(norm.nils)
        || !get<NeverTypeVar>(norm.numbers)
        || !norm.strings || !norm.strings->empty()
        || !get<NeverTypeVar>(norm.threads)
        || norm.functions
        || !norm.tables.empty()
        || !norm.tyvars.empty();
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
    if (!ty)
        return true;
    
    for (auto& [str, ty] : *ty)
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
    if (tys)
        for (TypeId ty : *tys)
            if (!get<FunctionTypeVar>(ty) && !get<ErrorTypeVar>(ty))
                return false;
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
    if (norm.strings)
        norm.strings->clear();
    else
        norm.strings.emplace();
    norm.threads = singletonTypes->neverType;
    norm.tables.clear();
    norm.functions = std::nullopt;
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
    if (!there)
        here.reset();
    else if (here)
        here->insert(there->begin(), there->end());
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

    auto dealWithDifferentArities = [&](TypePackIterator& ith, TypePackIterator itt, TypePackId here, TypePackId there, bool& hereSubThere, bool& thereSubHere)
    {
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
                    tail = arena->addTypePack(VariadicTypePack{ty,hidden});
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
        return arena->addTypePack(TypePack{head,tail});
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
    if (!theres)
        return;

    TypeIds tmps;

    if (!heres)
    {
        tmps.insert(theres->begin(), theres->end());
        heres = std::move(tmps);
        return;
    }

    for (TypeId here : *heres)
        for (TypeId there : *theres)
        {
            if (std::optional<TypeId> fun = unionOfFunctions(here, there))
                tmps.insert(*fun);
            else
                tmps.insert(singletonTypes->errorRecoveryType(there));
        }

    heres = std::move(tmps);
}

void Normalizer::unionFunctionsWithFunction(NormalizedFunctionType& heres, TypeId there)
{
    if (!heres)
    {
        TypeIds tmps;
        tmps.insert(there);
        heres = std::move(tmps);
        return;
    }

    TypeIds tmps;
    for (TypeId here : *heres)
    {
        if (std::optional<TypeId> fun = unionOfFunctions(here, there))
            tmps.insert(*fun);
        else
            tmps.insert(singletonTypes->errorRecoveryType(there));
    }
    heres = std::move(tmps);
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
            here.strings = std::nullopt;
        else if (ptv->type == PrimitiveTypeVar::Thread)
            here.threads = there;
        else
            LUAU_ASSERT(!"Unreachable");
    }
    else if (const SingletonTypeVar* stv = get<SingletonTypeVar>(there))
    {
        if (get<BooleanSingleton>(stv))
            here.booleans = unionOfBools(here.booleans, there);
        else if (const StringSingleton* sstv = get<StringSingleton>(stv))
        {
            if (here.strings)
                here.strings->insert({sstv->value, there});
        }
        else
            LUAU_ASSERT(!"Unreachable");
    }
    else
        LUAU_ASSERT(!"Unreachable");

    for (auto& [tyvar, intersect] : here.tyvars)
        if (!unionNormalWithTy(*intersect, there, tyvarIndex(tyvar)))
            return false;

    assertInvariant(here);
    return true;
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
    if (!there)
        return;
    if (!here)
        here.emplace();

    for (auto it = here->begin(); it != here->end();)
    {
        if (there->count(it->first))
            it++;
        else
            it = here->erase(it);
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

    auto dealWithDifferentArities = [&](TypePackIterator& ith, TypePackIterator itt, TypePackId here, TypePackId there, bool& hereSubThere, bool& thereSubHere)
    {
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
                    tail = arena->addTypePack(VariadicTypePack{ty,hidden});
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
        return arena->addTypePack(TypePack{head,tail});
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
    if (hftv->retTypes != tftv->retTypes)
        return std::nullopt;
    
    std::optional<TypePackId> argTypes = unionOfTypePacks(hftv->argTypes, tftv->argTypes);
    if (!argTypes)
        return std::nullopt;

    if (*argTypes == hftv->argTypes)
        return here;
    if (*argTypes == tftv->argTypes)
        return there;
        
    FunctionTypeVar result{*argTypes, hftv->retTypes};
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
    if (!heres)
        return;
    
    for (auto it = heres->begin(); it != heres->end();)
    {
        TypeId here = *it;
        if (get<ErrorTypeVar>(here))
            it++;
        else if (std::optional<TypeId> tmp = intersectionOfFunctions(here, there))
        {
            heres->erase(it);
            heres->insert(*tmp);
            return;
        }
        else
            it++;
    }

    TypeIds tmps;
    for (TypeId here : *heres)
    {
        if (std::optional<TypeId> tmp = unionSaturatedFunctions(here, there))
            tmps.insert(*tmp);
    }
    heres->insert(there);
    heres->insert(tmps.begin(), tmps.end());
}

void Normalizer::intersectFunctions(NormalizedFunctionType& heres, const NormalizedFunctionType& theres)
{
    if (!heres)
        return;
    else if (!theres)
    {
        heres = std::nullopt;
        return;
    }   
    else
    {
        for (TypeId there : *theres)
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
            if (!strings || strings->count(sstv->value))
                here.strings->insert({sstv->value, there});
        }
        else
            LUAU_ASSERT(!"Unreachable");
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
    if (norm.functions)
    {
        if (norm.functions->size() == 1)
            result.push_back(*norm.functions->begin());
        else
        {
            std::vector<TypeId> parts;
            parts.insert(parts.end(), norm.functions->begin(), norm.functions->end());
            result.push_back(arena->addType(IntersectionTypeVar{std::move(parts)}));
        }
    }
    if (!get<NeverTypeVar>(norm.nils))
        result.push_back(norm.nils);
    if (!get<NeverTypeVar>(norm.numbers))
        result.push_back(norm.numbers);
    if (norm.strings)
        for (auto& [_, ty] : *norm.strings)
            result.push_back(ty);
    else
        result.push_back(singletonTypes->stringType);
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

namespace
{

struct Replacer
{
    TypeArena* arena;
    TypeId sourceType;
    TypeId replacedType;
    DenseHashMap<TypeId, TypeId> newTypes;

    Replacer(TypeArena* arena, TypeId sourceType, TypeId replacedType)
        : arena(arena)
        , sourceType(sourceType)
        , replacedType(replacedType)
        , newTypes(nullptr)
    {
    }

    TypeId smartClone(TypeId t)
    {
        t = follow(t);
        TypeId* res = newTypes.find(t);
        if (res)
            return *res;

        TypeId result = shallowClone(t, *arena, TxnLog::empty());
        newTypes[t] = result;
        newTypes[result] = result;

        return result;
    }
};

} // anonymous namespace

bool isSubtype(TypeId subTy, TypeId superTy, NotNull<Scope> scope, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice, bool anyIsTop)
{
    UnifierSharedState sharedState{&ice};
    TypeArena arena;
    Normalizer normalizer{&arena, singletonTypes, NotNull{&sharedState}};
    Unifier u{NotNull{&normalizer}, Mode::Strict, scope, Location{}, Covariant};
    u.anyIsTop = anyIsTop;

    u.tryUnify(subTy, superTy);
    const bool ok = u.errors.empty() && u.log.empty();
    return ok;
}

bool isSubtype(TypePackId subPack, TypePackId superPack, NotNull<Scope> scope, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice, bool anyIsTop)
{
    UnifierSharedState sharedState{&ice};
    TypeArena arena;
    Normalizer normalizer{&arena, singletonTypes, NotNull{&sharedState}};
    Unifier u{NotNull{&normalizer}, Mode::Strict, scope, Location{}, Covariant};
    u.anyIsTop = anyIsTop;

    u.tryUnify(subPack, superPack);
    const bool ok = u.errors.empty() && u.log.empty();
    return ok;
}

template<typename T>
static bool areNormal_(const T& t, const std::unordered_set<void*>& seen, InternalErrorReporter& ice)
{
    int count = 0;
    auto isNormal = [&](TypeId ty) {
        ++count;
        if (count >= FInt::LuauNormalizeIterationLimit)
            ice.ice("Luau::areNormal hit iteration limit");

        return ty->normal;
    };

    return std::all_of(begin(t), end(t), isNormal);
}

static bool areNormal(const std::vector<TypeId>& types, const std::unordered_set<void*>& seen, InternalErrorReporter& ice)
{
    return areNormal_(types, seen, ice);
}

static bool areNormal(TypePackId tp, const std::unordered_set<void*>& seen, InternalErrorReporter& ice)
{
    tp = follow(tp);
    if (get<FreeTypePack>(tp))
        return false;

    auto [head, tail] = flatten(tp);

    if (!areNormal_(head, seen, ice))
        return false;

    if (!tail)
        return true;

    if (auto vtp = get<VariadicTypePack>(*tail))
        return vtp->ty->normal || follow(vtp->ty)->normal || seen.find(asMutable(vtp->ty)) != seen.end();

    return true;
}

#define CHECK_ITERATION_LIMIT(...) \
    do \
    { \
        if (iterationLimit > FInt::LuauNormalizeIterationLimit) \
        { \
            limitExceeded = true; \
            return __VA_ARGS__; \
        } \
        ++iterationLimit; \
    } while (false)

struct Normalize final : TypeVarVisitor
{
    using TypeVarVisitor::Set;

    Normalize(TypeArena& arena, NotNull<Scope> scope, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice)
        : arena(arena)
        , scope(scope)
        , singletonTypes(singletonTypes)
        , ice(ice)
    {
    }

    TypeArena& arena;
    NotNull<Scope> scope;
    NotNull<SingletonTypes> singletonTypes;
    InternalErrorReporter& ice;

    int iterationLimit = 0;
    bool limitExceeded = false;

    bool visit(TypeId ty, const FreeTypeVar&) override
    {
        LUAU_ASSERT(!ty->normal);
        return false;
    }

    bool visit(TypeId ty, const BoundTypeVar& btv) override
    {
        // A type could be considered normal when it is in the stack, but we will eventually find out it is not normal as normalization progresses.
        // So we need to avoid eagerly saying that this bound type is normal if the thing it is bound to is in the stack.
        if (seen.find(asMutable(btv.boundTo)) != seen.end())
            return false;

        // It should never be the case that this TypeVar is normal, but is bound to a non-normal type, except in nontrivial cases.
        LUAU_ASSERT(!ty->normal || ty->normal == btv.boundTo->normal);

        if (!ty->normal)
            asMutable(ty)->normal = btv.boundTo->normal;
        return !ty->normal;
    }

    bool visit(TypeId ty, const PrimitiveTypeVar&) override
    {
        LUAU_ASSERT(ty->normal);
        return false;
    }

    bool visit(TypeId ty, const GenericTypeVar&) override
    {
        if (!ty->normal)
            asMutable(ty)->normal = true;
        return false;
    }

    bool visit(TypeId ty, const ErrorTypeVar&) override
    {
        if (!ty->normal)
            asMutable(ty)->normal = true;
        return false;
    }

    bool visit(TypeId ty, const UnknownTypeVar&) override
    {
        if (!ty->normal)
            asMutable(ty)->normal = true;
        return false;
    }

    bool visit(TypeId ty, const NeverTypeVar&) override
    {
        if (!ty->normal)
            asMutable(ty)->normal = true;
        return false;
    }

    bool visit(TypeId ty, const ConstrainedTypeVar& ctvRef) override
    {
        CHECK_ITERATION_LIMIT(false);
        LUAU_ASSERT(!ty->normal);

        ConstrainedTypeVar* ctv = const_cast<ConstrainedTypeVar*>(&ctvRef);

        std::vector<TypeId> parts = std::move(ctv->parts);

        // We might transmute, so it's not safe to rely on the builtin traversal logic of visitTypeVar
        for (TypeId part : parts)
            traverse(part);

        std::vector<TypeId> newParts = normalizeUnion(parts);
        ctv->parts = std::move(newParts);

        return false;
    }

    bool visit(TypeId ty, const FunctionTypeVar& ftv) override
    {
        CHECK_ITERATION_LIMIT(false);

        if (ty->normal)
            return false;

        traverse(ftv.argTypes);
        traverse(ftv.retTypes);

        asMutable(ty)->normal = areNormal(ftv.argTypes, seen, ice) && areNormal(ftv.retTypes, seen, ice);

        return false;
    }

    bool visit(TypeId ty, const TableTypeVar& ttv) override
    {
        CHECK_ITERATION_LIMIT(false);

        if (ty->normal)
            return false;

        bool normal = true;

        auto checkNormal = [&](TypeId t) {
            // if t is on the stack, it is possible that this type is normal.
            // If t is not normal and it is not on the stack, this type is definitely not normal.
            if (!t->normal && seen.find(asMutable(t)) == seen.end())
                normal = false;
        };

        if (ttv.boundTo)
        {
            traverse(*ttv.boundTo);
            asMutable(ty)->normal = (*ttv.boundTo)->normal;
            return false;
        }

        for (const auto& [_name, prop] : ttv.props)
        {
            traverse(prop.type);
            checkNormal(prop.type);
        }

        if (ttv.indexer)
        {
            traverse(ttv.indexer->indexType);
            checkNormal(ttv.indexer->indexType);
            traverse(ttv.indexer->indexResultType);
            checkNormal(ttv.indexer->indexResultType);
        }

        // An unsealed table can never be normal, ditto for free tables iff the type it is bound to is also not normal.
        if (ttv.state == TableState::Generic || ttv.state == TableState::Sealed || (ttv.state == TableState::Free && follow(ty)->normal))
            asMutable(ty)->normal = normal;

        return false;
    }

    bool visit(TypeId ty, const MetatableTypeVar& mtv) override
    {
        CHECK_ITERATION_LIMIT(false);

        if (ty->normal)
            return false;

        traverse(mtv.table);
        traverse(mtv.metatable);

        asMutable(ty)->normal = mtv.table->normal && mtv.metatable->normal;

        return false;
    }

    bool visit(TypeId ty, const ClassTypeVar& ctv) override
    {
        if (!ty->normal)
            asMutable(ty)->normal = true;
        return false;
    }

    bool visit(TypeId ty, const AnyTypeVar&) override
    {
        LUAU_ASSERT(ty->normal);
        return false;
    }

    bool visit(TypeId ty, const UnionTypeVar& utvRef) override
    {
        CHECK_ITERATION_LIMIT(false);

        if (ty->normal)
            return false;

        UnionTypeVar* utv = &const_cast<UnionTypeVar&>(utvRef);

        // We might transmute, so it's not safe to rely on the builtin traversal logic of visitTypeVar
        for (TypeId option : utv->options)
            traverse(option);

        std::vector<TypeId> newOptions = normalizeUnion(utv->options);

        const bool normal = areNormal(newOptions, seen, ice);

        LUAU_ASSERT(!newOptions.empty());

        if (newOptions.size() == 1)
            *asMutable(ty) = BoundTypeVar{newOptions[0]};
        else
            utv->options = std::move(newOptions);

        asMutable(ty)->normal = normal;

        return false;
    }

    bool visit(TypeId ty, const IntersectionTypeVar& itvRef) override
    {
        CHECK_ITERATION_LIMIT(false);

        if (ty->normal)
            return false;

        IntersectionTypeVar* itv = &const_cast<IntersectionTypeVar&>(itvRef);

        std::vector<TypeId> oldParts = itv->parts;
        IntersectionTypeVar newIntersection;

        for (TypeId part : oldParts)
            traverse(part);

        std::vector<TypeId> tables;
        for (TypeId part : oldParts)
        {
            part = follow(part);
            if (get<TableTypeVar>(part))
                tables.push_back(part);
            else
            {
                Replacer replacer{&arena, nullptr, nullptr}; // FIXME this is super super WEIRD
                combineIntoIntersection(replacer, &newIntersection, part);
            }
        }

        // Don't allocate a new table if there's just one in the intersection.
        if (tables.size() == 1)
            newIntersection.parts.push_back(tables[0]);
        else if (!tables.empty())
        {
            const TableTypeVar* first = get<TableTypeVar>(tables[0]);
            LUAU_ASSERT(first);

            TypeId newTable = arena.addType(TableTypeVar{first->state, first->level});
            TableTypeVar* ttv = getMutable<TableTypeVar>(newTable);
            for (TypeId part : tables)
            {
                // Intuition: If combineIntoTable() needs to clone a table, any references to 'part' are cyclic and need
                // to be rewritten to point at 'newTable' in the clone.
                Replacer replacer{&arena, part, newTable};
                combineIntoTable(replacer, ttv, part);
            }

            newIntersection.parts.push_back(newTable);
        }

        itv->parts = std::move(newIntersection.parts);

        asMutable(ty)->normal = areNormal(itv->parts, seen, ice);

        if (itv->parts.size() == 1)
        {
            TypeId part = itv->parts[0];
            *asMutable(ty) = BoundTypeVar{part};
        }

        return false;
    }

    std::vector<TypeId> normalizeUnion(const std::vector<TypeId>& options)
    {
        if (options.size() == 1)
            return options;

        std::vector<TypeId> result;

        for (TypeId part : options)
        {
            // AnyTypeVar always win the battle no matter what we do, so we're done.
            if (FFlag::LuauUnknownAndNeverType && get<AnyTypeVar>(follow(part)))
                return {part};

            combineIntoUnion(result, part);
        }

        return result;
    }

    void combineIntoUnion(std::vector<TypeId>& result, TypeId ty)
    {
        ty = follow(ty);
        if (auto utv = get<UnionTypeVar>(ty))
        {
            for (TypeId t : utv)
            {
                // AnyTypeVar always win the battle no matter what we do, so we're done.
                if (FFlag::LuauUnknownAndNeverType && get<AnyTypeVar>(t))
                {
                    result = {t};
                    return;
                }

                combineIntoUnion(result, t);
            }

            return;
        }

        for (TypeId& part : result)
        {
            if (isSubtype(ty, part, scope, singletonTypes, ice))
                return; // no need to do anything
            else if (isSubtype(part, ty, scope, singletonTypes, ice))
            {
                part = ty; // replace the less general type by the more general one
                return;
            }
        }

        result.push_back(ty);
    }

    /**
     * @param replacer knows how to clone a type such that any recursive references point at the new containing type.
     * @param result is an intersection that is safe for us to mutate in-place.
     */
    void combineIntoIntersection(Replacer& replacer, IntersectionTypeVar* result, TypeId ty)
    {
        // Note: this check guards against running out of stack space
        // so if you increase the size of a stack frame, you'll need to decrease the limit.
        CHECK_ITERATION_LIMIT();

        ty = follow(ty);
        if (auto itv = get<IntersectionTypeVar>(ty))
        {
            for (TypeId part : itv->parts)
                combineIntoIntersection(replacer, result, part);
            return;
        }

        // Let's say that the last part of our result intersection is always a table, if any table is part of this intersection
        if (get<TableTypeVar>(ty))
        {
            if (result->parts.empty())
                result->parts.push_back(arena.addType(TableTypeVar{TableState::Sealed, TypeLevel{}}));

            TypeId theTable = result->parts.back();

            if (!get<TableTypeVar>(follow(theTable)))
            {
                result->parts.push_back(arena.addType(TableTypeVar{TableState::Sealed, TypeLevel{}}));
                theTable = result->parts.back();
            }

            TypeId newTable = replacer.smartClone(theTable);
            result->parts.back() = newTable;

            combineIntoTable(replacer, getMutable<TableTypeVar>(newTable), ty);
        }
        else if (auto ftv = get<FunctionTypeVar>(ty))
        {
            bool merged = false;
            for (TypeId& part : result->parts)
            {
                if (isSubtype(part, ty, scope, singletonTypes, ice))
                {
                    merged = true;
                    break; // no need to do anything
                }
                else if (isSubtype(ty, part, scope, singletonTypes, ice))
                {
                    merged = true;
                    part = ty; // replace the less general type by the more general one
                    break;
                }
            }

            if (!merged)
                result->parts.push_back(ty);
        }
        else
            result->parts.push_back(ty);
    }

    TableState combineTableStates(TableState lhs, TableState rhs)
    {
        if (lhs == rhs)
            return lhs;

        if (lhs == TableState::Free || rhs == TableState::Free)
            return TableState::Free;

        if (lhs == TableState::Unsealed || rhs == TableState::Unsealed)
            return TableState::Unsealed;

        return lhs;
    }

    /**
     * @param replacer gives us a way to clone a type such that recursive references are rewritten to the new
     * "containing" type.
     * @param table always points into a table that is safe for us to mutate.
     */
    void combineIntoTable(Replacer& replacer, TableTypeVar* table, TypeId ty)
    {
        // Note: this check guards against running out of stack space
        // so if you increase the size of a stack frame, you'll need to decrease the limit.
        CHECK_ITERATION_LIMIT();

        LUAU_ASSERT(table);

        ty = follow(ty);

        TableTypeVar* tyTable = getMutable<TableTypeVar>(ty);
        LUAU_ASSERT(tyTable);

        for (const auto& [propName, prop] : tyTable->props)
        {
            if (auto it = table->props.find(propName); it != table->props.end())
            {
                /**
                 * If we are going to recursively merge intersections of tables, we need to ensure that we never mutate
                 * a table that comes from somewhere else in the type graph.
                 *
                 * smarClone() does some nice things for us: It will perform a clone that is as shallow as possible
                 * while still rewriting any cyclic references back to the new 'root' table.
                 *
                 * replacer also keeps a mapping of types that have previously been copied, so we have the added
                 * advantage here of knowing that, whether or not a new copy was actually made, the resulting TypeVar is
                 * safe for us to mutate in-place.
                 */
                TypeId clone = replacer.smartClone(it->second.type);
                it->second.type = combine(replacer, clone, prop.type);
            }
            else
                table->props.insert({propName, prop});
        }

        if (tyTable->indexer)
        {
            if (table->indexer)
            {
                table->indexer->indexType = combine(replacer, replacer.smartClone(tyTable->indexer->indexType), table->indexer->indexType);
                table->indexer->indexResultType =
                    combine(replacer, replacer.smartClone(tyTable->indexer->indexResultType), table->indexer->indexResultType);
            }
            else
            {
                table->indexer =
                    TableIndexer{replacer.smartClone(tyTable->indexer->indexType), replacer.smartClone(tyTable->indexer->indexResultType)};
            }
        }

        table->state = combineTableStates(table->state, tyTable->state);
        table->level = max(table->level, tyTable->level);
    }

    /**
     * @param a is always cloned by the caller.  It is safe to mutate in-place.
     * @param b will never be mutated.
     */
    TypeId combine(Replacer& replacer, TypeId a, TypeId b)
    {
        b = follow(b);

        if (FFlag::LuauNormalizeCombineTableFix && a == b)
            return a;

        if (!get<IntersectionTypeVar>(a) && !get<TableTypeVar>(a))
        {
            if (!FFlag::LuauNormalizeCombineTableFix && a == b)
                return a;
            else
                return arena.addType(IntersectionTypeVar{{a, b}});
        }

        if (auto itv = getMutable<IntersectionTypeVar>(a))
        {
            combineIntoIntersection(replacer, itv, b);
            return a;
        }
        else if (auto ttv = getMutable<TableTypeVar>(a))
        {
            if (FFlag::LuauNormalizeCombineTableFix && !get<TableTypeVar>(b))
                return arena.addType(IntersectionTypeVar{{a, b}});
            combineIntoTable(replacer, ttv, b);
            return a;
        }

        LUAU_ASSERT(!"Impossible");
        LUAU_UNREACHABLE();
    }
};

#undef CHECK_ITERATION_LIMIT

/**
 * @returns A tuple of TypeId and a success indicator. (true indicates that the normalization completed successfully)
 */
std::pair<TypeId, bool> normalize(
    TypeId ty, NotNull<Scope> scope, TypeArena& arena, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice)
{
    CloneState state;
    if (FFlag::DebugLuauCopyBeforeNormalizing)
        (void)clone(ty, arena, state);

    Normalize n{arena, scope, singletonTypes, ice};
    n.traverse(ty);

    return {ty, !n.limitExceeded};
}

// TODO: Think about using a temporary arena and cloning types out of it so that we
// reclaim memory used by wantonly allocated intermediate types here.
// The main wrinkle here is that we don't want clone() to copy a type if the source and dest
// arena are the same.
std::pair<TypeId, bool> normalize(TypeId ty, NotNull<Module> module, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice)
{
    return normalize(ty, NotNull{module->getModuleScope().get()}, module->internalTypes, singletonTypes, ice);
}

std::pair<TypeId, bool> normalize(TypeId ty, const ModulePtr& module, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice)
{
    return normalize(ty, NotNull{module.get()}, singletonTypes, ice);
}

/**
 * @returns A tuple of TypeId and a success indicator. (true indicates that the normalization completed successfully)
 */
std::pair<TypePackId, bool> normalize(
    TypePackId tp, NotNull<Scope> scope, TypeArena& arena, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice)
{
    CloneState state;
    if (FFlag::DebugLuauCopyBeforeNormalizing)
        (void)clone(tp, arena, state);

    Normalize n{arena, scope, singletonTypes, ice};
    n.traverse(tp);

    return {tp, !n.limitExceeded};
}

std::pair<TypePackId, bool> normalize(TypePackId tp, NotNull<Module> module, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice)
{
    return normalize(tp, NotNull{module->getModuleScope().get()}, module->internalTypes, singletonTypes, ice);
}

std::pair<TypePackId, bool> normalize(TypePackId tp, const ModulePtr& module, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice)
{
    return normalize(tp, NotNull{module.get()}, singletonTypes, ice);
}

} // namespace Luau

