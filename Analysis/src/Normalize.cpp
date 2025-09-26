// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Normalize.h"
#include "Luau/ToString.h"

#include <algorithm>

#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Set.h"
#include "Luau/Simplify.h"
#include "Luau/Subtyping.h"
#include "Luau/Type.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier.h"

LUAU_FASTFLAGVARIABLE(DebugLuauCheckNormalizeInvariant)

LUAU_FASTINTVARIABLE(LuauNormalizeCacheLimit, 100000)
LUAU_FASTINTVARIABLE(LuauNormalizeIntersectionLimit, 200)
LUAU_FASTINTVARIABLE(LuauNormalizeUnionLimit, 100)
LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauUseWorkspacePropToChooseSolver)
LUAU_FASTFLAG(LuauReduceSetTypeStackPressure)
LUAU_FASTFLAG(LuauPassBindableGenericsByReference)

namespace Luau
{

static bool shouldEarlyExit(NormalizationResult res)
{
    // if res is hit limits, return control flow
    if (res == NormalizationResult::HitLimits || res == NormalizationResult::False)
        return true;
    return false;
}

NormalizedStringType::NormalizedStringType() {}

NormalizedStringType::NormalizedStringType(bool isCofinite, std::map<std::string, TypeId> singletons)
    : isCofinite(isCofinite)
    , singletons(std::move(singletons))
{
}

void NormalizedStringType::resetToString()
{
    isCofinite = true;
    singletons.clear();
}

void NormalizedStringType::resetToNever()
{
    isCofinite = false;
    singletons.clear();
}

bool NormalizedStringType::isNever() const
{
    return !isCofinite && singletons.empty();
}

bool NormalizedStringType::isString() const
{
    return isCofinite && singletons.empty();
}

bool NormalizedStringType::isUnion() const
{
    return !isCofinite;
}

bool NormalizedStringType::isIntersection() const
{
    return isCofinite;
}

bool NormalizedStringType::includes(const std::string& str) const
{
    if (isString())
        return true;
    else if (isUnion() && singletons.count(str))
        return true;
    else if (isIntersection() && !singletons.count(str))
        return true;
    else
        return false;
}

const NormalizedStringType NormalizedStringType::never;

bool isSubtype(const NormalizedStringType& subStr, const NormalizedStringType& superStr)
{
    if (subStr.isUnion() && (superStr.isUnion() && !superStr.isNever()))
    {
        for (auto [name, ty] : subStr.singletons)
        {
            if (!superStr.singletons.count(name))
                return false;
        }
    }
    else if (subStr.isString() && superStr.isUnion())
        return false;

    return true;
}

void NormalizedExternType::pushPair(TypeId ty, TypeIds negations)
{
    auto result = externTypes.insert(std::make_pair(ty, std::move(negations)));
    if (result.second)
        ordering.push_back(ty);
    LUAU_ASSERT(ordering.size() == externTypes.size());
}

void NormalizedExternType::resetToNever()
{
    ordering.clear();
    externTypes.clear();
}

bool NormalizedExternType::isNever() const
{
    return externTypes.empty();
}

void NormalizedFunctionType::resetToTop()
{
    isTop = true;
    parts.clear();
}

void NormalizedFunctionType::resetToNever()
{
    isTop = false;
    parts.clear();
}

bool NormalizedFunctionType::isNever() const
{
    return !isTop && parts.empty();
}

NormalizedType::NormalizedType(NotNull<BuiltinTypes> builtinTypes)
    : tops(builtinTypes->neverType)
    , booleans(builtinTypes->neverType)
    , errors(builtinTypes->neverType)
    , nils(builtinTypes->neverType)
    , numbers(builtinTypes->neverType)
    , strings{NormalizedStringType::never}
    , threads(builtinTypes->neverType)
    , buffers(builtinTypes->neverType)
{
}

bool NormalizedType::isUnknown() const
{
    if (get<UnknownType>(tops))
        return true;

    // Otherwise, we can still be unknown!
    bool hasAllPrimitives = isPrim(booleans, PrimitiveType::Boolean) && isPrim(nils, PrimitiveType::NilType) && isNumber(numbers) &&
                            strings.isString() && isThread(threads) && isBuffer(buffers);

    // Check is class
    bool isTopExternType = false;
    for (const auto& [t, disj] : externTypes.externTypes)
    {
        if (auto ct = get<ExternType>(t))
        {
            if (ct->name == "class" && disj.empty())
            {
                isTopExternType = true;
                break;
            }
        }
    }
    // Check is table
    bool isTopTable = false;
    for (auto t : tables)
    {
        if (isPrim(t, PrimitiveType::Table))
        {
            isTopTable = true;
            break;
        }
    }
    // any = unknown or error ==> we need to make sure we have all the unknown components, but not errors
    return get<NeverType>(errors) && hasAllPrimitives && isTopExternType && isTopTable && functions.isTop;
}

bool NormalizedType::isExactlyNumber() const
{
    return hasNumbers() && !hasTops() && !hasBooleans() && !hasExternTypes() && !hasErrors() && !hasNils() && !hasStrings() && !hasThreads() &&
           !hasBuffers() && !hasTables() && !hasFunctions() && !hasTyvars();
}

bool NormalizedType::isSubtypeOfString() const
{
    return hasStrings() && !hasTops() && !hasBooleans() && !hasExternTypes() && !hasErrors() && !hasNils() && !hasNumbers() && !hasThreads() &&
           !hasBuffers() && !hasTables() && !hasFunctions() && !hasTyvars();
}

bool NormalizedType::isSubtypeOfBooleans() const
{
    return hasBooleans() && !hasTops() && !hasExternTypes() && !hasErrors() && !hasNils() && !hasNumbers() && !hasStrings() && !hasThreads() &&
           !hasBuffers() && !hasTables() && !hasFunctions() && !hasTyvars();
}

bool NormalizedType::shouldSuppressErrors() const
{
    return hasErrors() || get<AnyType>(tops);
}

bool NormalizedType::hasTopTable() const
{
    return hasTables() && std::any_of(
                              tables.begin(),
                              tables.end(),
                              [&](TypeId ty)
                              {
                                  auto primTy = get<PrimitiveType>(ty);
                                  return primTy && primTy->type == PrimitiveType::Type::Table;
                              }
                          );
}

bool NormalizedType::hasTops() const
{
    return !get<NeverType>(tops);
}


bool NormalizedType::hasBooleans() const
{
    return !get<NeverType>(booleans);
}

bool NormalizedType::hasExternTypes() const
{
    return !externTypes.isNever();
}

bool NormalizedType::hasErrors() const
{
    return !get<NeverType>(errors);
}

bool NormalizedType::hasNils() const
{
    return !get<NeverType>(nils);
}

bool NormalizedType::hasNumbers() const
{
    return !get<NeverType>(numbers);
}

bool NormalizedType::hasStrings() const
{
    return !strings.isNever();
}

bool NormalizedType::hasThreads() const
{
    return !get<NeverType>(threads);
}

bool NormalizedType::hasBuffers() const
{
    return !get<NeverType>(buffers);
}

bool NormalizedType::hasTables() const
{
    return !tables.isNever();
}

bool NormalizedType::hasFunctions() const
{
    return !functions.isNever();
}

bool NormalizedType::hasTyvars() const
{
    return !tyvars.empty();
}

bool NormalizedType::isFalsy() const
{

    bool hasAFalse = false;
    if (auto singleton = get<SingletonType>(booleans))
    {
        if (auto bs = singleton->variant.get_if<BooleanSingleton>())
            hasAFalse = !bs->value;
    }

    return (hasAFalse || hasNils()) && (!hasTops() && !hasExternTypes() && !hasErrors() && !hasNumbers() && !hasStrings() && !hasThreads() &&
                                        !hasBuffers() && !hasTables() && !hasFunctions() && !hasTyvars());
}

bool NormalizedType::isTruthy() const
{
    return !isFalsy();
}

bool NormalizedType::isNil() const
{
    if (!hasNils())
        return false;

    return !hasTops() && !hasBooleans() && !hasExternTypes() && !hasNumbers() && !hasStrings() && !hasThreads() && !hasBuffers() && !hasTables() &&
           !hasFunctions() && !hasTyvars();
}

static bool isShallowInhabited(const NormalizedType& norm)
{
    // This test is just a shallow check, for example it returns `true` for `{ p : never }`
    return !get<NeverType>(norm.tops) || !get<NeverType>(norm.booleans) || !norm.externTypes.isNever() || !get<NeverType>(norm.errors) ||
           !get<NeverType>(norm.nils) || !get<NeverType>(norm.numbers) || !norm.strings.isNever() || !get<NeverType>(norm.threads) ||
           !get<NeverType>(norm.buffers) || !norm.functions.isNever() || !norm.tables.empty() || !norm.tyvars.empty();
}

NormalizationResult Normalizer::isInhabited(const NormalizedType* norm)
{
    Set<TypeId> seen{nullptr};

    return isInhabited(norm, seen);
}

NormalizationResult Normalizer::isInhabited(const NormalizedType* norm, Set<TypeId>& seen)
{
    RecursionCounter _rc(&sharedState->counters.recursionCount);
    if (!withinResourceLimits() || !norm)
        return NormalizationResult::HitLimits;

    if (!get<NeverType>(norm->tops) || !get<NeverType>(norm->booleans) || !get<NeverType>(norm->errors) || !get<NeverType>(norm->nils) ||
        !get<NeverType>(norm->numbers) || !get<NeverType>(norm->threads) || !get<NeverType>(norm->buffers) || !norm->externTypes.isNever() ||
        !norm->strings.isNever() || !norm->functions.isNever())
        return NormalizationResult::True;

    for (const auto& [_, intersect] : norm->tyvars)
    {
        NormalizationResult res = isInhabited(intersect.get(), seen);
        if (res != NormalizationResult::False)
            return res;
    }

    for (TypeId table : norm->tables)
    {
        NormalizationResult res = isInhabited(table, seen);
        if (res != NormalizationResult::False)
            return res;
    }

    return NormalizationResult::False;
}

NormalizationResult Normalizer::isInhabited(TypeId ty)
{
    if (cacheInhabitance)
    {
        if (bool* result = cachedIsInhabited.find(ty))
            return *result ? NormalizationResult::True : NormalizationResult::False;
    }

    Set<TypeId> seen{nullptr};
    NormalizationResult result = isInhabited(ty, seen);

    if (cacheInhabitance && result == NormalizationResult::True)
        cachedIsInhabited[ty] = true;
    else if (cacheInhabitance && result == NormalizationResult::False)
        cachedIsInhabited[ty] = false;

    return result;
}

NormalizationResult Normalizer::isInhabited(TypeId ty, Set<TypeId>& seen)
{
    RecursionCounter _rc(&sharedState->counters.recursionCount);
    if (!withinResourceLimits())
        return NormalizationResult::HitLimits;

    // TODO: use log.follow(ty), CLI-64291
    ty = follow(ty);

    if (get<NeverType>(ty))
        return NormalizationResult::False;

    if (!get<IntersectionType>(ty) && !get<UnionType>(ty) && !get<TableType>(ty) && !get<MetatableType>(ty))
        return NormalizationResult::True;

    if (seen.count(ty))
        return NormalizationResult::True;

    seen.insert(ty);

    if (const TableType* ttv = get<TableType>(ty))
    {
        for (const auto& [_, prop] : ttv->props)
        {
            if (useNewLuauSolver())
            {
                // A table enclosing a read property whose type is uninhabitable is also itself uninhabitable,
                // but not its write property. That just means the write property doesn't exist, and so is readonly.
                if (auto ty = prop.readTy)
                {
                    NormalizationResult res = isInhabited(*ty, seen);
                    if (res != NormalizationResult::True)
                        return res;
                }
            }
            else
            {
                NormalizationResult res = isInhabited(prop.type_DEPRECATED(), seen);
                if (res != NormalizationResult::True)
                    return res;
            }
        }
        return NormalizationResult::True;
    }

    if (const MetatableType* mtv = get<MetatableType>(ty))
    {
        NormalizationResult res = isInhabited(mtv->table, seen);
        if (res != NormalizationResult::True)
            return res;
        return isInhabited(mtv->metatable, seen);
    }

    std::shared_ptr<const NormalizedType> norm = normalize(ty);
    return isInhabited(norm.get(), seen);
}

NormalizationResult Normalizer::isIntersectionInhabited(TypeId left, TypeId right)
{
    Set<TypeId> seen{nullptr};
    SeenTablePropPairs seenTablePropPairs{{nullptr, nullptr}};
    return isIntersectionInhabited(left, right, seenTablePropPairs, seen);
}

NormalizationResult Normalizer::isIntersectionInhabited(TypeId left, TypeId right, SeenTablePropPairs& seenTablePropPairs, Set<TypeId>& seenSet)
{
    left = follow(left);
    right = follow(right);
    // We're asking if intersection is inhabited between left and right but we've already seen them ....

    if (cacheInhabitance)
    {
        if (bool* result = cachedIsInhabitedIntersection.find({left, right}))
            return *result ? NormalizationResult::True : NormalizationResult::False;
    }

    NormalizedType norm{builtinTypes};
    NormalizationResult res = normalizeIntersections({left, right}, norm, seenTablePropPairs, seenSet);
    if (res != NormalizationResult::True)
    {
        if (cacheInhabitance && res == NormalizationResult::False)
            cachedIsInhabitedIntersection[{left, right}] = false;

        return res;
    }

    NormalizationResult result = isInhabited(&norm, seenSet);

    if (cacheInhabitance && result == NormalizationResult::True)
        cachedIsInhabitedIntersection[{left, right}] = true;
    else if (cacheInhabitance && result == NormalizationResult::False)
        cachedIsInhabitedIntersection[{left, right}] = false;

    return result;
}

static int tyvarIndex(TypeId ty)
{
    if (const GenericType* gtv = get<GenericType>(ty))
        return gtv->index;
    else if (const FreeType* ftv = get<FreeType>(ty))
        return ftv->index;
    else if (const BlockedType* btv = get<BlockedType>(ty))
        return btv->index;
    else
        return 0;
}

static bool isTop(NotNull<BuiltinTypes> builtinTypes, const NormalizedExternType& externTypes)
{
    if (externTypes.externTypes.size() != 1)
        return false;

    auto first = externTypes.externTypes.begin();
    if (first->first != builtinTypes->externType)
        return false;

    if (!first->second.empty())
        return false;

    return true;
}

static void resetToTop(NotNull<BuiltinTypes> builtinTypes, NormalizedExternType& externTypes)
{
    externTypes.ordering.clear();
    externTypes.externTypes.clear();
    externTypes.pushPair(builtinTypes->externType, TypeIds{});
}

#ifdef LUAU_ASSERTENABLED

static bool isNormalizedTop(TypeId ty)
{
    return get<NeverType>(ty) || get<AnyType>(ty) || get<UnknownType>(ty);
}

static bool isNormalizedBoolean(TypeId ty)
{
    if (get<NeverType>(ty))
        return true;
    else if (const PrimitiveType* ptv = get<PrimitiveType>(ty))
        return ptv->type == PrimitiveType::Boolean;
    else if (const SingletonType* stv = get<SingletonType>(ty))
        return get<BooleanSingleton>(stv);
    else
        return false;
}

static bool isNormalizedError(TypeId ty)
{
    if (get<NeverType>(ty) || get<ErrorType>(ty))
        return true;
    else
        return false;
}

static bool isNormalizedNil(TypeId ty)
{
    if (get<NeverType>(ty))
        return true;
    else if (const PrimitiveType* ptv = get<PrimitiveType>(ty))
        return ptv->type == PrimitiveType::NilType;
    else
        return false;
}

static bool isNormalizedNumber(TypeId ty)
{
    if (get<NeverType>(ty))
        return true;
    else if (const PrimitiveType* ptv = get<PrimitiveType>(ty))
        return ptv->type == PrimitiveType::Number;
    else
        return false;
}

static bool isNormalizedString(const NormalizedStringType& ty)
{
    if (ty.isString())
        return true;

    for (auto& [str, ty] : ty.singletons)
    {
        if (const SingletonType* stv = get<SingletonType>(ty))
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
    if (get<NeverType>(ty))
        return true;
    else if (const PrimitiveType* ptv = get<PrimitiveType>(ty))
        return ptv->type == PrimitiveType::Thread;
    else
        return false;
}

static bool isNormalizedBuffer(TypeId ty)
{
    if (get<NeverType>(ty))
        return true;
    else if (const PrimitiveType* ptv = get<PrimitiveType>(ty))
        return ptv->type == PrimitiveType::Buffer;
    else
        return false;
}

static bool areNormalizedFunctions(const NormalizedFunctionType& tys)
{
    for (TypeId ty : tys.parts)
    {
        if (!get<FunctionType>(ty) && !get<ErrorType>(ty))
            return false;
    }
    return true;
}

static bool areNormalizedTables(const TypeIds& tys)
{
    for (TypeId ty : tys)
    {
        if (get<TableType>(ty) || get<MetatableType>(ty))
            continue;

        const PrimitiveType* pt = get<PrimitiveType>(ty);
        if (!pt)
            return false;

        if (pt->type == PrimitiveType::Table)
            continue;

        return false;
    }

    return true;
}

static bool areNormalizedExternTypes(const NormalizedExternType& tys)
{
    for (const auto& [ty, negations] : tys.externTypes)
    {
        const ExternType* etv = get<ExternType>(ty);
        if (!etv)
        {
            return false;
        }

        for (TypeId negation : negations)
        {
            const ExternType* nctv = get<ExternType>(negation);
            if (!nctv)
            {
                return false;
            }

            if (!isSubclass(nctv, etv))
            {
                return false;
            }
        }

        for (const auto& [otherTy, otherNegations] : tys.externTypes)
        {
            if (otherTy == ty)
                continue;

            const ExternType* octv = get<ExternType>(otherTy);
            if (!octv)
            {
                return false;
            }

            if (isSubclass(etv, octv))
            {
                auto iss = [etv](TypeId t)
                {
                    const ExternType* c = get<ExternType>(t);
                    if (!c)
                        return false;

                    return isSubclass(etv, c);
                };

                if (!std::any_of(otherNegations.begin(), otherNegations.end(), iss))
                    return false;
            }
        }
    }

    return true;
}

static bool isPlainTyvar(TypeId ty)
{
    return (get<FreeType>(ty) || get<GenericType>(ty) || get<BlockedType>(ty) || get<PendingExpansionType>(ty) || get<TypeFunctionInstanceType>(ty));
}

static bool isNormalizedTyvar(const NormalizedTyvars& tyvars)
{
    for (auto& [tyvar, intersect] : tyvars)
    {
        if (!isPlainTyvar(tyvar))
            return false;
        if (!isShallowInhabited(*intersect))
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
    LUAU_ASSERT(areNormalizedExternTypes(norm.externTypes));
    LUAU_ASSERT(isNormalizedError(norm.errors));
    LUAU_ASSERT(isNormalizedNil(norm.nils));
    LUAU_ASSERT(isNormalizedNumber(norm.numbers));
    LUAU_ASSERT(isNormalizedString(norm.strings));
    LUAU_ASSERT(isNormalizedThread(norm.threads));
    LUAU_ASSERT(isNormalizedBuffer(norm.buffers));
    LUAU_ASSERT(areNormalizedFunctions(norm.functions));
    LUAU_ASSERT(areNormalizedTables(norm.tables));
    LUAU_ASSERT(isNormalizedTyvar(norm.tyvars));
    for (auto& [_, child] : norm.tyvars)
        assertInvariant(*child);
#endif
}

Normalizer::Normalizer(
    TypeArena* arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<UnifierSharedState> sharedState,
    SolverMode solverMode,
    bool cacheInhabitance
)
    : arena(arena)
    , builtinTypes(builtinTypes)
    , sharedState(sharedState)
    , cacheInhabitance(cacheInhabitance)
    , solverMode(solverMode)
{
}

static bool isCacheable(TypeId ty, Set<TypeId>& seen);

static bool isCacheable(TypePackId tp, Set<TypeId>& seen)
{
    tp = follow(tp);

    auto it = begin(tp);
    auto endIt = end(tp);
    for (; it != endIt; ++it)
    {
        if (!isCacheable(*it, seen))
            return false;
    }

    if (auto tail = it.tail())
    {
        if (get<FreeTypePack>(*tail) || get<BlockedTypePack>(*tail) || get<TypeFunctionInstanceTypePack>(*tail))
            return false;
    }

    return true;
}

static bool isCacheable(TypeId ty, Set<TypeId>& seen)
{
    if (seen.contains(ty))
        return true;
    seen.insert(ty);

    ty = follow(ty);

    if (get<FreeType>(ty) || get<BlockedType>(ty) || get<PendingExpansionType>(ty))
        return false;

    if (auto tfi = get<TypeFunctionInstanceType>(ty))
    {
        for (TypeId t : tfi->typeArguments)
        {
            if (!isCacheable(t, seen))
                return false;
        }

        for (TypePackId tp : tfi->packArguments)
        {
            if (!isCacheable(tp, seen))
                return false;
        }
    }

    return true;
}

static bool isCacheable(TypeId ty)
{
    Set<TypeId> seen{nullptr};
    return isCacheable(ty, seen);
}

std::shared_ptr<const NormalizedType> Normalizer::normalize(TypeId ty)
{
    if (!arena)
        sharedState->iceHandler->ice("Normalizing types outside a module");

    auto found = cachedNormals.find(ty);
    if (found != cachedNormals.end())
        return found->second;

    NormalizedType norm{builtinTypes};
    Set<TypeId> seenSetTypes{nullptr};
    SeenTablePropPairs seenTablePropPairs{{nullptr, nullptr}};
    NormalizationResult res = unionNormalWithTy(norm, ty, seenTablePropPairs, seenSetTypes);
    if (res != NormalizationResult::True)
        return nullptr;

    if (norm.isUnknown())
    {
        clearNormal(norm);
        norm.tops = builtinTypes->unknownType;
    }

    std::shared_ptr<NormalizedType> shared = std::make_shared<NormalizedType>(std::move(norm));

    if (shared->isCacheable)
        cachedNormals[ty] = shared;

    return shared;
}

NormalizationResult Normalizer::normalizeIntersections(
    const std::vector<TypeId>& intersections,
    NormalizedType& outType,
    SeenTablePropPairs& seenTablePropPairs,
    Set<TypeId>& seenSet
)
{
    if (!arena)
        sharedState->iceHandler->ice("Normalizing types outside a module");
    NormalizedType norm{builtinTypes};
    norm.tops = builtinTypes->anyType;
    // Now we need to intersect the two types
    for (auto ty : intersections)
    {
        NormalizationResult res = intersectNormalWithTy(norm, ty, seenTablePropPairs, seenSet);
        if (res != NormalizationResult::True)
            return res;
    }

    NormalizationResult res = unionNormals(outType, norm);
    if (res != NormalizationResult::True)
        return res;

    return NormalizationResult::True;
}

void Normalizer::clearNormal(NormalizedType& norm)
{
    norm.tops = builtinTypes->neverType;
    norm.booleans = builtinTypes->neverType;
    norm.externTypes.resetToNever();
    norm.errors = builtinTypes->neverType;
    norm.nils = builtinTypes->neverType;
    norm.numbers = builtinTypes->neverType;
    norm.strings.resetToNever();
    norm.threads = builtinTypes->neverType;
    norm.buffers = builtinTypes->neverType;
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
    if (get<NeverType>(here) || get<AnyType>(there))
        return there;
    if (get<NeverType>(there) || get<AnyType>(here))
        return here;

    TypeIds tmps;

    if (const UnionType* utv = get<UnionType>(here))
    {
        TypeIds heres;
        heres.insert(begin(utv), end(utv));
        tmps.insert(heres.begin(), heres.end());
        cachedUnions[cacheTypeIds(std::move(heres))] = here;
    }
    else
        tmps.insert(here);

    if (const UnionType* utv = get<UnionType>(there))
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
    TypeId result = arena->addType(UnionType{std::move(parts)});
    cachedUnions[cacheTypeIds(std::move(tmps))] = result;

    return result;
}

TypeId Normalizer::intersectionType(TypeId here, TypeId there)
{
    here = follow(here);
    there = follow(there);

    if (here == there)
        return here;
    if (get<NeverType>(here) || get<AnyType>(there))
        return here;
    if (get<NeverType>(there) || get<AnyType>(here))
        return there;

    TypeIds tmps;

    if (const IntersectionType* utv = get<IntersectionType>(here))
    {
        TypeIds heres;
        heres.insert(begin(utv), end(utv));
        tmps.insert(heres.begin(), heres.end());
        cachedIntersections[cacheTypeIds(std::move(heres))] = here;
    }
    else
        tmps.insert(here);

    if (const IntersectionType* utv = get<IntersectionType>(there))
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
    TypeId result = arena->addType(IntersectionType{std::move(parts)});
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
    if (get<NeverType>(here) || get<AnyType>(there))
        return there;
    else
        return here;
}

TypeId Normalizer::unionOfBools(TypeId here, TypeId there)
{
    if (get<NeverType>(here))
        return there;
    if (get<NeverType>(there))
        return here;
    if (const BooleanSingleton* hbool = get<BooleanSingleton>(get<SingletonType>(here)))
        if (const BooleanSingleton* tbool = get<BooleanSingleton>(get<SingletonType>(there)))
            if (hbool->value == tbool->value)
                return here;
    return builtinTypes->booleanType;
}

void Normalizer::unionExternTypesWithExternType(TypeIds& heres, TypeId there)
{
    if (heres.count(there))
        return;

    const ExternType* tctv = get<ExternType>(there);

    for (auto it = heres.begin(); it != heres.end();)
    {
        TypeId here = *it;
        const ExternType* hctv = get<ExternType>(here);
        if (isSubclass(tctv, hctv))
            return;
        else if (isSubclass(hctv, tctv))
            it = heres.erase(it);
        else
            it++;
    }

    heres.insert(there);
}

void Normalizer::unionExternTypes(TypeIds& heres, const TypeIds& theres)
{
    for (TypeId there : theres)
        unionExternTypesWithExternType(heres, there);
}

static bool isSubclass(TypeId test, TypeId parent)
{
    const ExternType* testCtv = get<ExternType>(test);
    const ExternType* parentCtv = get<ExternType>(parent);

    LUAU_ASSERT(testCtv);
    LUAU_ASSERT(parentCtv);

    return isSubclass(testCtv, parentCtv);
}

void Normalizer::unionExternTypesWithExternType(NormalizedExternType& heres, TypeId there)
{
    for (auto it = heres.ordering.begin(); it != heres.ordering.end();)
    {
        TypeId hereTy = *it;
        TypeIds& hereNegations = heres.externTypes.at(hereTy);

        // If the incoming class is a subclass of another class in the map, we
        // must ensure that it is negated by one of the negations in the same
        // cluster. If it isn't, we do not need to insert it - the subtyping
        // relationship is already handled by this entry. If it is, we must
        // insert it, to capture the presence of this particular subtype.
        if (isSubclass(there, hereTy))
        {
            for (auto nIt = hereNegations.begin(); nIt != hereNegations.end();)
            {
                TypeId hereNegation = *nIt;

                // If the incoming class is a subclass of one of the negations,
                // we must insert it into the class map.
                if (isSubclass(there, hereNegation))
                {
                    heres.pushPair(there, TypeIds{});
                    return;
                }
                // If the incoming class is a superclass of one of the
                // negations, then the negation no longer applies and must be
                // removed. This is also true if they are equal. Since extern types
                // are, at this time, entirely persistent (we do not clone
                // them), a pointer identity check is sufficient.
                else if (isSubclass(hereNegation, there))
                {
                    nIt = hereNegations.erase(nIt);
                }
                // If the incoming class is unrelated to the negation, we move
                // on to the next item.
                else
                {
                    ++nIt;
                }
            }

            // If, at the end of the above loop, we haven't returned, that means
            // that the class is not a subclass of one of the negations, and is
            // covered by the existing subtype relationship. We can return now.
            return;
        }
        // If the incoming class is a superclass of another class in the map, we
        // need to replace the existing class with the incoming class,
        // preserving the relevant negations.
        else if (isSubclass(hereTy, there))
        {
            TypeIds negations = std::move(hereNegations);
            it = heres.ordering.erase(it);
            heres.externTypes.erase(hereTy);

            heres.pushPair(there, std::move(negations));
            return;
        }

        // If the incoming class is unrelated to the class in the map, we move
        // on. If we do not otherwise exit from this method body, we will
        // eventually fall out of this loop and insert the incoming class, which
        // we have proven to be completely unrelated to any class in the map,
        // into the map itself.
        ++it;
    }

    heres.pushPair(there, TypeIds{});
}

void Normalizer::unionExternTypes(NormalizedExternType& heres, const NormalizedExternType& theres)
{
    // This method bears much similarity with unionExternTypesWithExternType, but is
    // solving a more general problem. In unionExternTypesWithExternType, we are dealing
    // with a singular positive type. Since it's one type, we can use early
    // returns as control flow. Since it's guaranteed to be positive, we do not
    // have negations to worry about combining. The two aspects combine to make
    // the tasks this method must perform different enough to warrant a separate
    // implementation.

    for (const TypeId thereTy : theres.ordering)
    {
        const TypeIds& thereNegations = theres.externTypes.at(thereTy);

        // If it happens that there are _no_ extern types in the current map, or the
        // incoming class is completely unrelated to any class in the current
        // map, we must insert the incoming pair as-is.
        bool insert = true;

        for (auto it = heres.ordering.begin(); it != heres.ordering.end();)
        {
            TypeId hereTy = *it;
            TypeIds& hereNegations = heres.externTypes.at(hereTy);

            if (isSubclass(thereTy, hereTy))
            {
                bool inserted = false;
                for (auto nIt = hereNegations.begin(); nIt != hereNegations.end();)
                {
                    TypeId hereNegateTy = *nIt;

                    // If the incoming class is a subclass of one of the negations,
                    // we must insert it into the class map.
                    if (isSubclass(thereTy, hereNegateTy))
                    {
                        // We do not concern ourselves with iterator
                        // invalidation here because we will break out of the
                        // loop over `heres` when `inserted` is set, and we do
                        // not read from the iterator after this point.
                        inserted = true;
                        heres.pushPair(thereTy, thereNegations);
                        break;
                    }
                    // If the incoming class is a superclass of one of the
                    // negations, then the negation no longer applies and must
                    // be removed. This is also true if they are equal. Since
                    // extern types are, at this time, entirely persistent (we do not
                    // clone them), a pointer identity check is sufficient.
                    else if (isSubclass(hereNegateTy, thereTy))
                    {
                        inserted = true;
                        nIt = hereNegations.erase(nIt);
                        break;
                    }
                    // If the incoming class is unrelated to the negation, we
                    // move on to the next item.
                    else
                    {
                        ++nIt;
                    }
                }

                if (inserted)
                {
                    insert = false;
                    break;
                }
            }
            else if (isSubclass(hereTy, thereTy))
            {
                TypeIds negations = std::move(hereNegations);
                unionExternTypes(negations, thereNegations);

                it = heres.ordering.erase(it);
                heres.externTypes.erase(hereTy);
                heres.pushPair(thereTy, std::move(negations));
                insert = false;
                break;
            }
            else if (hereTy == thereTy)
            {
                unionExternTypes(hereNegations, thereNegations);
                insert = false;
                break;
            }

            ++it;
        }

        if (insert)
        {
            heres.pushPair(thereTy, thereNegations);
        }
    }
}

void Normalizer::unionStrings(NormalizedStringType& here, const NormalizedStringType& there)
{
    if (there.isString())
        here.resetToString();
    else if (here.isUnion() && there.isUnion())
        here.singletons.insert(there.singletons.begin(), there.singletons.end());
    else if (here.isUnion() && there.isIntersection())
    {
        here.isCofinite = true;
        for (const auto& pair : there.singletons)
        {
            auto it = here.singletons.find(pair.first);
            if (it != end(here.singletons))
                here.singletons.erase(it);
            else
                here.singletons.insert(pair);
        }
    }
    else if (here.isIntersection() && there.isUnion())
    {
        for (const auto& [name, ty] : there.singletons)
            here.singletons.erase(name);
    }
    else if (here.isIntersection() && there.isIntersection())
    {
        auto iter = begin(here.singletons);
        auto endIter = end(here.singletons);

        while (iter != endIter)
        {
            if (!there.singletons.count(iter->first))
            {
                auto eraseIt = iter;
                ++iter;
                here.singletons.erase(eraseIt);
            }
            else
                ++iter;
        }
    }
    else
        LUAU_ASSERT(!"Unreachable");
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

    auto dealWithDifferentArities =
        [&](TypePackIterator& ith, TypePackIterator itt, TypePackId here, TypePackId there, bool& hereSubThere, bool& thereSubHere)
    {
        if (ith != end(here))
        {
            TypeId tty = builtinTypes->nilType;
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
        return arena->addTypePack(TypePack{std::move(head), tail});
    else if (tail)
        return *tail;
    else
        // TODO: Add an emptyPack to singleton types
        return arena->addTypePack({});
}

std::optional<TypeId> Normalizer::unionOfFunctions(TypeId here, TypeId there)
{
    if (get<ErrorType>(here))
        return here;

    if (get<ErrorType>(there))
        return there;

    const FunctionType* hftv = get<FunctionType>(here);
    LUAU_ASSERT(hftv);
    const FunctionType* tftv = get<FunctionType>(there);
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

    FunctionType result{*argTypes, *retTypes};
    result.generics = hftv->generics;
    result.genericPacks = hftv->genericPacks;
    return arena->addType(std::move(result));
}

void Normalizer::unionFunctions(NormalizedFunctionType& heres, const NormalizedFunctionType& theres)
{
    if (heres.isTop)
        return;
    if (theres.isTop)
        heres.resetToTop();

    if (theres.isNever())
        return;

    TypeIds tmps;

    if (heres.isNever())
    {
        tmps.insert(theres.parts.begin(), theres.parts.end());
        heres.parts = std::move(tmps);
        return;
    }

    for (TypeId here : heres.parts)
        for (TypeId there : theres.parts)
        {
            if (std::optional<TypeId> fun = unionOfFunctions(here, there))
                tmps.insert(*fun);
            else
                tmps.insert(builtinTypes->errorRecoveryType(there));
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
    for (TypeId here : heres.parts)
    {
        if (std::optional<TypeId> fun = unionOfFunctions(here, there))
            tmps.insert(*fun);
        else
            tmps.insert(builtinTypes->errorRecoveryType(there));
    }
    heres.parts = std::move(tmps);
}

void Normalizer::unionTablesWithTable(TypeIds& heres, TypeId there)
{
    // TODO: remove unions of tables where possible

    // we can always skip `never`
    if (get<NeverType>(there))
        return;

    heres.insert(there);
}

void Normalizer::unionTables(TypeIds& heres, const TypeIds& theres)
{
    for (TypeId there : theres)
    {
        if (there == builtinTypes->tableType)
        {
            heres.clear();
            heres.insert(there);
            return;
        }
        else
        {
            unionTablesWithTable(heres, there);
        }
    }
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
NormalizationResult Normalizer::unionNormals(NormalizedType& here, const NormalizedType& there, int ignoreSmallerTyvars)
{
    here.isCacheable &= there.isCacheable;

    TypeId tops = unionOfTops(here.tops, there.tops);
    if (get<UnknownType>(tops) && (get<ErrorType>(here.errors) || get<ErrorType>(there.errors)))
        tops = builtinTypes->anyType;
    if (!get<NeverType>(tops))
    {
        clearNormal(here);
        here.tops = tops;
        return NormalizationResult::True;
    }

    if (here.tyvars.size() * there.tyvars.size() >= size_t(FInt::LuauNormalizeUnionLimit))
        return NormalizationResult::HitLimits;

    for (auto it = there.tyvars.begin(); it != there.tyvars.end(); it++)
    {
        TypeId tyvar = it->first;
        const NormalizedType& inter = *it->second;
        int index = tyvarIndex(tyvar);
        if (index <= ignoreSmallerTyvars)
            continue;
        auto [emplaced, fresh] = here.tyvars.emplace(tyvar, std::make_unique<NormalizedType>(NormalizedType{builtinTypes}));
        if (fresh)
        {
            NormalizationResult res = unionNormals(*emplaced->second, here, index);
            if (res != NormalizationResult::True)
                return res;
        }

        NormalizationResult res = unionNormals(*emplaced->second, inter, index);
        if (res != NormalizationResult::True)
            return res;
    }

    // Limit based on worst-case expansion of the function unions
    if (here.functions.parts.size() * there.functions.parts.size() >= size_t(FInt::LuauNormalizeUnionLimit))
        return NormalizationResult::HitLimits;

    here.booleans = unionOfBools(here.booleans, there.booleans);
    unionExternTypes(here.externTypes, there.externTypes);

    here.errors = (get<NeverType>(there.errors) ? here.errors : there.errors);
    here.nils = (get<NeverType>(there.nils) ? here.nils : there.nils);
    here.numbers = (get<NeverType>(there.numbers) ? here.numbers : there.numbers);
    unionStrings(here.strings, there.strings);
    here.threads = (get<NeverType>(there.threads) ? here.threads : there.threads);
    here.buffers = (get<NeverType>(there.buffers) ? here.buffers : there.buffers);
    unionFunctions(here.functions, there.functions);
    unionTables(here.tables, there.tables);

    return NormalizationResult::True;
}

bool Normalizer::withinResourceLimits()
{
    // If cache is too large, clear it
    if (FInt::LuauNormalizeCacheLimit > 0)
    {
        size_t cacheUsage = cachedNormals.size() + cachedIntersections.size() + cachedUnions.size() + cachedTypeIds.size() +
                            cachedIsInhabited.size() + cachedIsInhabitedIntersection.size();
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

bool Normalizer::useNewLuauSolver() const
{
    return FFlag::LuauUseWorkspacePropToChooseSolver ? (solverMode == SolverMode::New) : FFlag::LuauSolverV2;
}

NormalizationResult Normalizer::intersectNormalWithNegationTy(TypeId toNegate, NormalizedType& intersect)
{

    std::optional<NormalizedType> negated;

    std::shared_ptr<const NormalizedType> normal = normalize(toNegate);
    negated = negateNormal(*normal);

    if (!negated)
        return NormalizationResult::False;
    intersectNormals(intersect, *negated);
    return NormalizationResult::True;
}

// See above for an explanation of `ignoreSmallerTyvars`.
NormalizationResult Normalizer::unionNormalWithTy(
    NormalizedType& here,
    TypeId there,
    SeenTablePropPairs& seenTablePropPairs,
    Set<TypeId>& seenSetTypes,
    int ignoreSmallerTyvars
)
{
    RecursionCounter _rc(&sharedState->counters.recursionCount);
    if (!withinResourceLimits())
        return NormalizationResult::HitLimits;

    there = follow(there);

    if (get<AnyType>(there) || get<UnknownType>(there))
    {
        TypeId tops = unionOfTops(here.tops, there);
        if (get<UnknownType>(tops) && get<ErrorType>(here.errors))
            tops = builtinTypes->anyType;
        clearNormal(here);
        here.tops = tops;
        return NormalizationResult::True;
    }
    else if (get<NeverType>(there) || get<AnyType>(here.tops))
        return NormalizationResult::True;
    else if (get<ErrorType>(there) && get<UnknownType>(here.tops))
    {
        here.tops = builtinTypes->anyType;
        return NormalizationResult::True;
    }
    else if (const UnionType* utv = get<UnionType>(there))
    {
        if (seenSetTypes.count(there))
            return NormalizationResult::True;
        seenSetTypes.insert(there);

        for (UnionTypeIterator it = begin(utv); it != end(utv); ++it)
        {
            NormalizationResult res = unionNormalWithTy(here, *it, seenTablePropPairs, seenSetTypes);
            if (res != NormalizationResult::True)
            {
                seenSetTypes.erase(there);
                return res;
            }
        }

        seenSetTypes.erase(there);
        return NormalizationResult::True;
    }
    else if (const IntersectionType* itv = get<IntersectionType>(there))
    {
        if (seenSetTypes.count(there))
            return NormalizationResult::True;
        seenSetTypes.insert(there);

        NormalizedType norm{builtinTypes};
        norm.tops = builtinTypes->anyType;
        for (IntersectionTypeIterator it = begin(itv); it != end(itv); ++it)
        {
            NormalizationResult res = intersectNormalWithTy(norm, *it, seenTablePropPairs, seenSetTypes);
            if (res != NormalizationResult::True)
            {
                seenSetTypes.erase(there);
                return res;
            }
        }

        seenSetTypes.erase(there);

        return unionNormals(here, norm);
    }
    else if (get<UnknownType>(here.tops))
        return NormalizationResult::True;
    else if (get<GenericType>(there) || get<FreeType>(there) || get<BlockedType>(there) || get<PendingExpansionType>(there) ||
             get<TypeFunctionInstanceType>(there))
    {
        if (tyvarIndex(there) <= ignoreSmallerTyvars)
            return NormalizationResult::True;
        NormalizedType inter{builtinTypes};
        inter.tops = builtinTypes->unknownType;
        here.tyvars.insert_or_assign(there, std::make_unique<NormalizedType>(std::move(inter)));

        if (!isCacheable(there))
            here.isCacheable = false;
    }
    else if (get<FunctionType>(there))
        unionFunctionsWithFunction(here.functions, there);
    else if (get<TableType>(there) || get<MetatableType>(there))
        unionTablesWithTable(here.tables, there);
    else if (get<ExternType>(there))
        unionExternTypesWithExternType(here.externTypes, there);
    else if (get<ErrorType>(there))
        here.errors = there;
    else if (const PrimitiveType* ptv = get<PrimitiveType>(there))
    {
        if (ptv->type == PrimitiveType::Boolean)
            here.booleans = there;
        else if (ptv->type == PrimitiveType::NilType)
            here.nils = there;
        else if (ptv->type == PrimitiveType::Number)
            here.numbers = there;
        else if (ptv->type == PrimitiveType::String)
            here.strings.resetToString();
        else if (ptv->type == PrimitiveType::Thread)
            here.threads = there;
        else if (ptv->type == PrimitiveType::Buffer)
            here.buffers = there;
        else if (ptv->type == PrimitiveType::Function)
        {
            here.functions.resetToTop();
        }
        else if (ptv->type == PrimitiveType::Table)
        {
            here.tables.clear();
            here.tables.insert(there);
        }
        else
            LUAU_ASSERT(!"Unreachable");
    }
    else if (const SingletonType* stv = get<SingletonType>(there))
    {
        if (get<BooleanSingleton>(stv))
            here.booleans = unionOfBools(here.booleans, there);
        else if (const StringSingleton* sstv = get<StringSingleton>(stv))
        {
            if (here.strings.isCofinite)
            {
                auto it = here.strings.singletons.find(sstv->value);
                if (it != here.strings.singletons.end())
                    here.strings.singletons.erase(it);
            }
            else
                here.strings.singletons.insert({sstv->value, there});
        }
        else
            LUAU_ASSERT(!"Unreachable");
    }
    else if (const NegationType* ntv = get<NegationType>(there))
    {
        std::optional<NormalizedType> tn;

        std::shared_ptr<const NormalizedType> thereNormal = normalize(ntv->ty);
        tn = negateNormal(*thereNormal);

        if (!tn)
            return NormalizationResult::False;

        NormalizationResult res = unionNormals(here, *tn);
        if (res != NormalizationResult::True)
            return res;
    }
    else if (get<PendingExpansionType>(there) || get<TypeFunctionInstanceType>(there) || get<NoRefineType>(there))
    {
        // nothing
    }
    else
        LUAU_ASSERT(!"Unreachable");

    for (auto& [tyvar, intersect] : here.tyvars)
    {
        NormalizationResult res = unionNormalWithTy(*intersect, there, seenTablePropPairs, seenSetTypes, tyvarIndex(tyvar));
        if (res != NormalizationResult::True)
            return res;
    }

    assertInvariant(here);
    return NormalizationResult::True;
}

// ------- Negations

std::optional<NormalizedType> Normalizer::negateNormal(const NormalizedType& here)
{
    NormalizedType result{builtinTypes};
    result.isCacheable = here.isCacheable;

    if (!get<NeverType>(here.tops))
    {
        // The negation of unknown or any is never.  Easy.
        return result;
    }

    if (!get<NeverType>(here.errors))
    {
        // Negating an error yields the same error.
        result.errors = here.errors;
        return result;
    }

    if (get<NeverType>(here.booleans))
        result.booleans = builtinTypes->booleanType;
    else if (get<PrimitiveType>(here.booleans))
        result.booleans = builtinTypes->neverType;
    else if (auto stv = get<SingletonType>(here.booleans))
    {
        auto boolean = get<BooleanSingleton>(stv);
        LUAU_ASSERT(boolean != nullptr);
        if (boolean->value)
            result.booleans = builtinTypes->falseType;
        else
            result.booleans = builtinTypes->trueType;
    }

    if (here.externTypes.isNever())
    {
        resetToTop(builtinTypes, result.externTypes);
    }
    else if (isTop(builtinTypes, result.externTypes))
    {
        result.externTypes.resetToNever();
    }
    else
    {
        TypeIds rootNegations{};

        for (const auto& [hereParent, hereNegations] : here.externTypes.externTypes)
        {
            if (hereParent != builtinTypes->externType)
                rootNegations.insert(hereParent);

            for (TypeId hereNegation : hereNegations)
                unionExternTypesWithExternType(result.externTypes, hereNegation);
        }

        if (!rootNegations.empty())
            result.externTypes.pushPair(builtinTypes->externType, std::move(rootNegations));
    }

    result.nils = get<NeverType>(here.nils) ? builtinTypes->nilType : builtinTypes->neverType;
    result.numbers = get<NeverType>(here.numbers) ? builtinTypes->numberType : builtinTypes->neverType;

    result.strings = here.strings;
    result.strings.isCofinite = !result.strings.isCofinite;

    result.threads = get<NeverType>(here.threads) ? builtinTypes->threadType : builtinTypes->neverType;
    result.buffers = get<NeverType>(here.buffers) ? builtinTypes->bufferType : builtinTypes->neverType;

    /*
     * Things get weird and so, so complicated if we allow negations of
     * arbitrary function types.  Ordinary code can never form these kinds of
     * types, so we decline to negate them.
     */
    if (here.functions.isNever())
        result.functions.resetToTop();
    else if (here.functions.isTop)
        result.functions.resetToNever();
    else
        return std::nullopt;

    /*
     * It is not possible to negate an arbitrary table type, because function
     * types are not runtime-testable. Thus, we prohibit negation of anything
     * other than `table` and `never`.
     */
    if (here.tables.empty())
        result.tables.insert(builtinTypes->tableType);
    else if (here.tables.size() == 1 && here.tables.front() == builtinTypes->tableType)
        result.tables.clear();
    else
        return std::nullopt;

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
    if (get<AnyType>(there))
        return there;
    else if (get<UnknownType>(there))
        return builtinTypes->neverType;
    else if (get<NeverType>(there))
        return builtinTypes->unknownType;
    else if (auto ntv = get<NegationType>(there))
        return ntv->ty; // TODO: do we want to normalize this?
    else if (auto utv = get<UnionType>(there))
    {
        std::vector<TypeId> parts;
        for (TypeId option : utv)
            parts.push_back(negate(option));
        return arena->addType(IntersectionType{std::move(parts)});
    }
    else if (auto itv = get<IntersectionType>(there))
    {
        std::vector<TypeId> options;
        for (TypeId part : itv)
            options.push_back(negate(part));
        return arena->addType(UnionType{std::move(options)});
    }
    else
        return there;
}

void Normalizer::subtractPrimitive(NormalizedType& here, TypeId ty)
{
    const PrimitiveType* ptv = get<PrimitiveType>(follow(ty));
    LUAU_ASSERT(ptv);
    switch (ptv->type)
    {
    case PrimitiveType::NilType:
        here.nils = builtinTypes->neverType;
        break;
    case PrimitiveType::Boolean:
        here.booleans = builtinTypes->neverType;
        break;
    case PrimitiveType::Number:
        here.numbers = builtinTypes->neverType;
        break;
    case PrimitiveType::String:
        here.strings.resetToNever();
        break;
    case PrimitiveType::Thread:
        here.threads = builtinTypes->neverType;
        break;
    case PrimitiveType::Buffer:
        here.buffers = builtinTypes->neverType;
        break;
    case PrimitiveType::Function:
        here.functions.resetToNever();
        break;
    case PrimitiveType::Table:
        here.tables.clear();
        break;
    }
}

void Normalizer::subtractSingleton(NormalizedType& here, TypeId ty)
{
    const SingletonType* stv = get<SingletonType>(ty);
    LUAU_ASSERT(stv);

    if (const StringSingleton* ss = get<StringSingleton>(stv))
    {
        if (here.strings.isCofinite)
            here.strings.singletons.insert({ss->value, ty});
        else
        {
            auto it = here.strings.singletons.find(ss->value);
            if (it != here.strings.singletons.end())
                here.strings.singletons.erase(it);
        }
    }
    else if (const BooleanSingleton* bs = get<BooleanSingleton>(stv))
    {
        if (get<NeverType>(here.booleans))
        {
            // Nothing
        }
        else if (get<PrimitiveType>(here.booleans))
            here.booleans = bs->value ? builtinTypes->falseType : builtinTypes->trueType;
        else if (auto hereSingleton = get<SingletonType>(here.booleans))
        {
            const BooleanSingleton* hereBooleanSingleton = get<BooleanSingleton>(hereSingleton);
            LUAU_ASSERT(hereBooleanSingleton);

            // Crucial subtlety: ty (and thus bs) are the value that is being
            // negated out. We therefore reduce to never when the values match,
            // rather than when they differ.
            if (bs->value == hereBooleanSingleton->value)
                here.booleans = builtinTypes->neverType;
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
    if (get<NeverType>(here) || get<AnyType>(there))
        return here;
    else
        return there;
}

TypeId Normalizer::intersectionOfBools(TypeId here, TypeId there)
{
    if (get<NeverType>(here))
        return here;
    if (get<NeverType>(there))
        return there;
    if (const BooleanSingleton* hbool = get<BooleanSingleton>(get<SingletonType>(here)))
        if (const BooleanSingleton* tbool = get<BooleanSingleton>(get<SingletonType>(there)))
            return (hbool->value == tbool->value ? here : builtinTypes->neverType);
        else
            return here;
    else
        return there;
}

void Normalizer::intersectExternTypes(NormalizedExternType& heres, const NormalizedExternType& theres)
{
    if (theres.isNever())
    {
        heres.resetToNever();
        return;
    }
    else if (isTop(builtinTypes, theres))
    {
        return;
    }

    // For intersections of two distinct class sets, we must normalize to a map
    // where, for each entry, one of the following is true:
    // - The class is the superclass of all other classes in the map
    // - The class is a subclass of another class B in the map _and_ a subclass
    //   of one of B's negations.
    //
    // Once we have identified the common superclass, we proceed down the list
    // of class types. For each class and negation pair in the incoming set, we
    // check each entry in the current set.
    // - If the incoming class is exactly identical to a class in the current
    //   set, we union the negations together and move on.
    // - If the incoming class is a subclass of a class in the current set, we
    //   replace the current class with the incoming class. We keep negations
    //   that are a subclass of the incoming class, and discard ones that
    //   aren't.
    // - If the incoming class is a superclass of a class in the current set, we
    //   take the negations that are a subclass of the current class and union
    //   them with the negations for the current class.
    // - If the incoming class is unrelated to any class in the current set, we
    //   declare the result of the intersection operation to be never.
    for (const TypeId thereTy : theres.ordering)
    {
        const TypeIds& thereNegations = theres.externTypes.at(thereTy);

        for (auto it = heres.ordering.begin(); it != heres.ordering.end();)
        {
            TypeId hereTy = *it;
            TypeIds& hereNegations = heres.externTypes.at(hereTy);

            if (isSubclass(thereTy, hereTy))
            {
                // If thereTy is a subtype of hereTy, we need to replace hereTy
                // by thereTy and combine their negation lists.
                //
                // If any types in the negation list are not subtypes of
                // thereTy, they need to be removed from the negation list.
                TypeIds negations = std::move(hereNegations);

                for (auto nIt = negations.begin(); nIt != negations.end();)
                {
                    if (!isSubclass(*nIt, thereTy))
                    {
                        nIt = negations.erase(nIt);
                    }
                    else
                    {
                        ++nIt;
                    }
                }

                unionExternTypes(negations, thereNegations);

                it = heres.ordering.erase(it);
                heres.externTypes.erase(hereTy);
                heres.pushPair(thereTy, std::move(negations));
                break;
            }
            else if (isSubclass(hereTy, thereTy))
            {
                // If thereTy is a supertype of hereTy, we need to extend the
                // negation list of hereTy by that of thereTy.
                //
                // If any of the types of thereTy's negations are not subtypes
                // of hereTy, they must not be added to hereTy's negation list.
                //
                // If any of the types of thereTy's negations are supertypes of
                // hereTy, then hereTy must be removed entirely.
                //
                // If any of the types of thereTy's negations are supertypes of
                // the negations of herety, the former must supplant the latter.
                TypeIds negations = thereNegations;

                bool erasedHere = false;

                for (auto nIt = negations.begin(); nIt != negations.end();)
                {
                    if (isSubclass(hereTy, *nIt))
                    {
                        // eg SomeExternType & (class & ~SomeExternType)
                        // or SomeExternType & (class & ~ParentExternType)
                        heres.externTypes.erase(hereTy);
                        it = heres.ordering.erase(it);
                        erasedHere = true;
                        break;
                    }

                    // eg SomeExternType & (class & ~Unrelated)
                    if (!isSubclass(*nIt, hereTy))
                        nIt = negations.erase(nIt);
                    else
                        ++nIt;
                }

                if (!erasedHere)
                {
                    unionExternTypes(hereNegations, negations);
                    ++it;
                }
            }
            else if (hereTy == thereTy)
            {
                unionExternTypes(hereNegations, thereNegations);
                break;
            }
            else
            {
                it = heres.ordering.erase(it);
                heres.externTypes.erase(hereTy);
            }
        }
    }
}

void Normalizer::intersectExternTypesWithExternType(NormalizedExternType& heres, TypeId there)
{
    for (auto it = heres.ordering.begin(); it != heres.ordering.end();)
    {
        TypeId hereTy = *it;
        const TypeIds& hereNegations = heres.externTypes.at(hereTy);

        // If the incoming class _is_ the current class, we skip it. Maybe
        // another entry will have a different story. We check for this first
        // because isSubclass will be true if the types are equal, and entering
        // either of those branches below will trigger wrong behaviors.
        if (hereTy == there)
        {
            ++it;
        }
        // If the incoming class is a subclass of this type, we replace the
        // current class with the incoming class. We preserve negations that are
        // a subclass of the incoming class, and discard ones that aren't.
        else if (isSubclass(there, hereTy))
        {
            TypeIds negations = std::move(hereNegations);
            bool emptyIntersectWithNegation = false;

            for (auto nIt = negations.begin(); nIt != negations.end();)
            {
                if (isSubclass(there, *nIt))
                {
                    // Hitting this block means that the incoming class is a
                    // subclass of this type, _and_ one of its negations is a
                    // superclass of this type, e.g.:
                    //
                    //  Dog & ~Animal
                    //
                    // Clearly this intersects to never, so we mark this class as
                    // being removed from the normalized class type.
                    emptyIntersectWithNegation = true;
                    break;
                }

                if (!isSubclass(*nIt, there))
                {
                    nIt = negations.erase(nIt);
                }
                else
                {
                    ++nIt;
                }
            }

            it = heres.ordering.erase(it);
            heres.externTypes.erase(hereTy);
            if (!emptyIntersectWithNegation)
                heres.pushPair(there, std::move(negations));
            break;
        }
        // If the incoming class is a superclass of the current class, we don't
        // insert it into the map.
        else if (isSubclass(hereTy, there))
        {
            return;
        }
        // If the incoming class is completely unrelated to the current class,
        // we drop the current class from the map.
        else
        {
            it = heres.ordering.erase(it);
            heres.externTypes.erase(hereTy);
        }
    }
}

void Normalizer::intersectStrings(NormalizedStringType& here, const NormalizedStringType& there)
{
    /* There are 9 cases to worry about here
         Normalized Left    | Normalized Right
       C1 string            | string              ===> trivial
       C2 string - {u_1,..} | string              ===> trivial
       C3 {u_1, ..}         | string              ===> trivial
       C4 string            | string - {v_1, ..}  ===> string - {v_1, ..}
       C5 string - {u_1,..} | string - {v_1, ..}  ===> string - ({u_s} U {v_s})
       C6 {u_1, ..}         | string - {v_1, ..}  ===> {u_s} - {v_s}
       C7 string            | {v_1, ..}           ===> {v_s}
       C8 string - {u_1,..} | {v_1, ..}           ===> {v_s} - {u_s}
       C9 {u_1, ..}         | {v_1, ..}           ===> {u_s} ∩ {v_s}
    */
    // Case 1,2,3
    if (there.isString())
        return;
    // Case 4, Case 7
    else if (here.isString())
    {
        here.singletons.clear();
        for (const auto& [key, type] : there.singletons)
            here.singletons[key] = type;
        here.isCofinite = here.isCofinite && there.isCofinite;
    }
    // Case 5
    else if (here.isIntersection() && there.isIntersection())
    {
        here.isCofinite = true;
        for (const auto& [key, type] : there.singletons)
            here.singletons[key] = type;
    }
    // Case 6
    else if (here.isUnion() && there.isIntersection())
    {
        here.isCofinite = false;
        for (const auto& [key, _] : there.singletons)
            here.singletons.erase(key);
    }
    // Case 8
    else if (here.isIntersection() && there.isUnion())
    {
        here.isCofinite = false;
        std::map<std::string, TypeId> result(there.singletons);
        for (const auto& [key, _] : here.singletons)
            result.erase(key);
        here.singletons = result;
    }
    // Case 9
    else if (here.isUnion() && there.isUnion())
    {
        here.isCofinite = false;
        std::map<std::string, TypeId> result;
        result.insert(here.singletons.begin(), here.singletons.end());
        result.insert(there.singletons.begin(), there.singletons.end());
        for (auto it = result.begin(); it != result.end();)
            if (!here.singletons.count(it->first) || !there.singletons.count(it->first))
                it = result.erase(it);
            else
                ++it;
        here.singletons = result;
    }
    else
        LUAU_ASSERT(0 && "Internal Error - unrecognized case");
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

    auto dealWithDifferentArities =
        [&](TypePackIterator& ith, TypePackIterator itt, TypePackId here, TypePackId there, bool& hereSubThere, bool& thereSubHere)
    {
        if (ith != end(here))
        {
            TypeId tty = builtinTypes->nilType;
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
        return arena->addTypePack(TypePack{std::move(head), tail});
    else if (tail)
        return *tail;
    else
        // TODO: Add an emptyPack to singleton types
        return arena->addTypePack({});
}

std::optional<TypeId> Normalizer::intersectionOfTables(TypeId here, TypeId there, SeenTablePropPairs& seenTablePropPairs, Set<TypeId>& seenSet)
{
    if (here == there)
        return here;

    RecursionCounter _rc(&sharedState->counters.recursionCount);
    if (sharedState->counters.recursionLimit > 0 && sharedState->counters.recursionLimit < sharedState->counters.recursionCount)
        return std::nullopt;

    if (isPrim(here, PrimitiveType::Table))
        return there;
    else if (isPrim(there, PrimitiveType::Table))
        return here;

    if (get<NeverType>(here))
        return there;
    else if (get<NeverType>(there))
        return here;
    else if (get<AnyType>(here))
        return there;
    else if (get<AnyType>(there))
        return here;

    TypeId htable = here;
    TypeId hmtable = nullptr;
    if (const MetatableType* hmtv = get<MetatableType>(here))
    {
        htable = follow(hmtv->table);
        hmtable = follow(hmtv->metatable);
    }
    TypeId ttable = there;
    TypeId tmtable = nullptr;
    if (const MetatableType* tmtv = get<MetatableType>(there))
    {
        ttable = follow(tmtv->table);
        tmtable = follow(tmtv->metatable);
    }

    const TableType* httv = get<TableType>(htable);
    if (!httv)
        return std::nullopt;

    const TableType* tttv = get<TableType>(ttable);
    if (!tttv)
        return std::nullopt;


    if (httv->state == TableState::Free || tttv->state == TableState::Free)
        return std::nullopt;
    if (httv->state == TableState::Generic || tttv->state == TableState::Generic)
        return std::nullopt;

    TableState state = httv->state;
    if (tttv->state == TableState::Unsealed)
        state = tttv->state;

    TypeLevel level = max(httv->level, tttv->level);
    Scope* scope = max(httv->scope, tttv->scope);

    std::unique_ptr<TableType> result = nullptr;
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
            if (useNewLuauSolver())
            {
                if (hprop.readTy.has_value())
                {
                    if (tprop.readTy.has_value())
                    {
                        TypeId ty = simplifyIntersection(builtinTypes, NotNull{arena}, *hprop.readTy, *tprop.readTy).result;

                        // If any property is going to get mapped to `never`, we can just call the entire table `never`.
                        // Since this check is syntactic, we may sometimes miss simplifying tables with complex uninhabited properties.
                        // Prior versions of this code attempted to do this semantically using the normalization machinery, but this
                        // mistakenly causes infinite loops when giving more complex recursive table types. As it stands, this approach
                        // will continue to scale as simplification is improved, but we may wish to reintroduce the semantic approach
                        // once we have revisited the usage of seen sets systematically (and possibly with some additional guarding to recognize
                        // when types are infinitely-recursive with non-pointer identical instances of them, or some guard to prevent that
                        // construction altogether). See also: `gh1632_no_infinite_recursion_in_normalization`
                        if (get<NeverType>(ty))
                            return {builtinTypes->neverType};

                        prop.readTy = ty;
                        hereSubThere &= (ty == hprop.readTy);
                        thereSubHere &= (ty == tprop.readTy);
                    }
                    else
                    {
                        prop.readTy = *hprop.readTy;
                        thereSubHere = false;
                    }
                }
                else if (tprop.readTy.has_value())
                {
                    prop.readTy = *tprop.readTy;
                    hereSubThere = false;
                }

                if (hprop.writeTy.has_value())
                {
                    if (tprop.writeTy.has_value())
                    {
                        prop.writeTy = simplifyIntersection(builtinTypes, NotNull{arena}, *hprop.writeTy, *tprop.writeTy).result;
                        hereSubThere &= (prop.writeTy == hprop.writeTy);
                        thereSubHere &= (prop.writeTy == tprop.writeTy);
                    }
                    else
                    {
                        prop.writeTy = *hprop.writeTy;
                        thereSubHere = false;
                    }
                }
                else if (tprop.writeTy.has_value())
                {
                    prop.writeTy = *tprop.writeTy;
                    hereSubThere = false;
                }
            }
            else
            {
                prop.setType(intersectionType(hprop.type_DEPRECATED(), tprop.type_DEPRECATED()));
                hereSubThere &= (prop.type_DEPRECATED() == hprop.type_DEPRECATED());
                thereSubHere &= (prop.type_DEPRECATED() == tprop.type_DEPRECATED());
            }
        }

        // TODO: string indexers

        if (prop.readTy || prop.writeTy)
        {
            if (!result.get())
                result = std::make_unique<TableType>(TableType{state, level, scope});
            result->props[name] = prop;
        }
    }

    for (const auto& [name, tprop] : tttv->props)
    {
        if (httv->props.count(name) == 0)
        {
            if (!result.get())
                result = std::make_unique<TableType>(TableType{state, level, scope});

            result->props[name] = tprop;
            hereSubThere = false;
        }
    }

    if (httv->indexer && tttv->indexer)
    {
        // TODO: What should intersection of indexes be?
        TypeId index = unionType(httv->indexer->indexType, tttv->indexer->indexType);
        TypeId indexResult = intersectionType(httv->indexer->indexResultType, tttv->indexer->indexResultType);
        if (!result.get())
            result = std::make_unique<TableType>(TableType{state, level, scope});
        result->indexer = {index, indexResult};
        hereSubThere &= (httv->indexer->indexType == index) && (httv->indexer->indexResultType == indexResult);
        thereSubHere &= (tttv->indexer->indexType == index) && (tttv->indexer->indexResultType == indexResult);
    }
    else if (httv->indexer)
    {
        if (!result.get())
            result = std::make_unique<TableType>(TableType{state, level, scope});
        result->indexer = httv->indexer;
        thereSubHere = false;
    }
    else if (tttv->indexer)
    {
        if (!result.get())
            result = std::make_unique<TableType>(TableType{state, level, scope});
        result->indexer = tttv->indexer;
        hereSubThere = false;
    }

    TypeId table;
    if (hereSubThere)
        table = htable;
    else if (thereSubHere)
        table = ttable;
    else
    {
        if (result.get())
            table = arena->addType(std::move(*result));
        else
            table = arena->addType(TableType{state, level, scope});
    }

    if (tmtable && hmtable)
    {
        // NOTE: this assumes metatables are ivariant
        if (std::optional<TypeId> mtable = intersectionOfTables(hmtable, tmtable, seenTablePropPairs, seenSet))
        {
            if (table == htable && *mtable == hmtable)
                return here;
            else if (table == ttable && *mtable == tmtable)
                return there;
            else
                return arena->addType(MetatableType{table, *mtable});
        }
        else
            return std::nullopt;
    }
    else if (hmtable)
    {
        if (table == htable)
            return here;
        else
            return arena->addType(MetatableType{table, hmtable});
    }
    else if (tmtable)
    {
        if (table == ttable)
            return there;
        else
            return arena->addType(MetatableType{table, tmtable});
    }
    else
        return table;
}

void Normalizer::intersectTablesWithTable(TypeIds& heres, TypeId there, SeenTablePropPairs& seenTablePropPairs, Set<TypeId>& seenSetTypes)
{
    TypeIds tmp;
    for (TypeId here : heres)
    {
        if (std::optional<TypeId> inter = intersectionOfTables(here, there, seenTablePropPairs, seenSetTypes))
            tmp.insert(*inter);
    }
    heres.retain(tmp);
    heres.insert(tmp.begin(), tmp.end());
}

void Normalizer::intersectTables(TypeIds& heres, const TypeIds& theres)
{
    TypeIds tmp;
    for (TypeId here : heres)
    {
        for (TypeId there : theres)
        {
            Set<TypeId> seenSetTypes{nullptr};
            SeenTablePropPairs seenTablePropPairs{{nullptr, nullptr}};
            if (std::optional<TypeId> inter = intersectionOfTables(here, there, seenTablePropPairs, seenSetTypes))
                tmp.insert(*inter);
        }
    }

    heres.retain(tmp);
    heres.insert(tmp.begin(), tmp.end());
}

std::optional<TypeId> Normalizer::intersectionOfFunctions(TypeId here, TypeId there)
{
    const FunctionType* hftv = get<FunctionType>(here);
    LUAU_ASSERT(hftv);
    const FunctionType* tftv = get<FunctionType>(there);
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
    else if (hftv->argTypes == tftv->argTypes)
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

    FunctionType result{argTypes, retTypes};
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

    const FunctionType* hftv = get<FunctionType>(here);
    if (!hftv)
        return std::nullopt;
    const FunctionType* tftv = get<FunctionType>(there);
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

    FunctionType result{*argTypes, *retTypes};
    result.generics = hftv->generics;
    result.genericPacks = hftv->genericPacks;
    return arena->addType(std::move(result));
}

void Normalizer::intersectFunctionsWithFunction(NormalizedFunctionType& heres, TypeId there)
{
    if (heres.isNever())
        return;

    heres.isTop = false;

    for (auto it = heres.parts.begin(); it != heres.parts.end();)
    {
        TypeId here = *it;
        if (get<ErrorType>(here))
            it++;
        else if (std::optional<TypeId> tmp = intersectionOfFunctions(here, there))
        {
            heres.parts.erase(it);
            heres.parts.insert(*tmp);
            return;
        }
        else
            it++;
    }

    TypeIds tmps;
    for (TypeId here : heres.parts)
    {
        if (std::optional<TypeId> tmp = unionSaturatedFunctions(here, there))
            tmps.insert(*tmp);
    }
    heres.parts.insert(there);
    heres.parts.insert(tmps.begin(), tmps.end());
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
        for (TypeId there : theres.parts)
            intersectFunctionsWithFunction(heres, there);
    }
}

NormalizationResult Normalizer::intersectTyvarsWithTy(
    NormalizedTyvars& here,
    TypeId there,
    SeenTablePropPairs& seenTablePropPairs,
    Set<TypeId>& seenSetTypes
)
{
    for (auto it = here.begin(); it != here.end();)
    {
        NormalizedType& inter = *it->second;
        NormalizationResult res = intersectNormalWithTy(inter, there, seenTablePropPairs, seenSetTypes);
        if (res != NormalizationResult::True)
            return res;
        if (isShallowInhabited(inter))
            ++it;
        else
            it = here.erase(it);
    }
    return NormalizationResult::True;
}

// See above for an explanation of `ignoreSmallerTyvars`.
NormalizationResult Normalizer::intersectNormals(NormalizedType& here, const NormalizedType& there, int ignoreSmallerTyvars)
{
    RecursionCounter _rc(&sharedState->counters.recursionCount);
    if (!withinResourceLimits())
        return NormalizationResult::HitLimits;

    if (!get<NeverType>(there.tops))
    {
        here.tops = intersectionOfTops(here.tops, there.tops);
        return NormalizationResult::True;
    }
    else if (!get<NeverType>(here.tops))
    {
        clearNormal(here);
        return unionNormals(here, there, ignoreSmallerTyvars);
    }

    // Limit based on worst-case expansion of the table/function intersections
    // This restriction can be relaxed when table intersection simplification is improved
    if (here.tables.size() * there.tables.size() >= size_t(FInt::LuauNormalizeIntersectionLimit))
        return NormalizationResult::HitLimits;

    if (here.functions.parts.size() * there.functions.parts.size() >= size_t(FInt::LuauNormalizeIntersectionLimit))
        return NormalizationResult::HitLimits;

    for (auto& [tyvar, inter] : there.tyvars)
    {
        int index = tyvarIndex(tyvar);
        if (ignoreSmallerTyvars < index)
        {
            auto [found, fresh] = here.tyvars.emplace(tyvar, std::make_unique<NormalizedType>(NormalizedType{builtinTypes}));
            if (fresh)
            {
                NormalizationResult res = unionNormals(*found->second, here, index);
                if (res != NormalizationResult::True)
                    return res;
            }
        }
    }

    here.booleans = intersectionOfBools(here.booleans, there.booleans);

    intersectExternTypes(here.externTypes, there.externTypes);
    here.errors = (get<NeverType>(there.errors) ? there.errors : here.errors);
    here.nils = (get<NeverType>(there.nils) ? there.nils : here.nils);
    here.numbers = (get<NeverType>(there.numbers) ? there.numbers : here.numbers);
    intersectStrings(here.strings, there.strings);
    here.threads = (get<NeverType>(there.threads) ? there.threads : here.threads);
    here.buffers = (get<NeverType>(there.buffers) ? there.buffers : here.buffers);
    intersectFunctions(here.functions, there.functions);
    intersectTables(here.tables, there.tables);

    for (auto it = here.tyvars.begin(); it != here.tyvars.end();)
    {
        TypeId tyvar = it->first;
        NormalizedType& inter = *it->second;
        int index = tyvarIndex(tyvar);
        LUAU_ASSERT(ignoreSmallerTyvars < index);
        auto found = there.tyvars.find(tyvar);
        if (found == there.tyvars.end())
        {
            NormalizationResult res = intersectNormals(inter, there, index);
            if (res != NormalizationResult::True)
                return res;
        }
        else
        {
            NormalizationResult res = intersectNormals(inter, *found->second, index);
            if (res != NormalizationResult::True)
                return res;
        }
        if (isShallowInhabited(inter))
            it++;
        else
            it = here.tyvars.erase(it);
    }
    return NormalizationResult::True;
}

NormalizationResult Normalizer::intersectNormalWithTy(
    NormalizedType& here,
    TypeId there,
    SeenTablePropPairs& seenTablePropPairs,
    Set<TypeId>& seenSetTypes
)
{
    RecursionCounter _rc(&sharedState->counters.recursionCount);
    if (!withinResourceLimits())
        return NormalizationResult::HitLimits;

    there = follow(there);

    if (get<AnyType>(there) || get<UnknownType>(there))
    {
        here.tops = intersectionOfTops(here.tops, there);
        return NormalizationResult::True;
    }
    else if (!get<NeverType>(here.tops))
    {
        clearNormal(here);
        return unionNormalWithTy(here, there, seenTablePropPairs, seenSetTypes);
    }
    else if (const UnionType* utv = get<UnionType>(there))
    {
        NormalizedType norm{builtinTypes};
        for (UnionTypeIterator it = begin(utv); it != end(utv); ++it)
        {
            NormalizationResult res = unionNormalWithTy(norm, *it, seenTablePropPairs, seenSetTypes);
            if (res != NormalizationResult::True)
                return res;
        }
        return intersectNormals(here, norm);
    }
    else if (const IntersectionType* itv = get<IntersectionType>(there))
    {
        for (IntersectionTypeIterator it = begin(itv); it != end(itv); ++it)
        {
            NormalizationResult res = intersectNormalWithTy(here, *it, seenTablePropPairs, seenSetTypes);
            if (res != NormalizationResult::True)
                return res;
        }
        return NormalizationResult::True;
    }
    else if (get<GenericType>(there) || get<FreeType>(there) || get<BlockedType>(there) || get<PendingExpansionType>(there) ||
             get<TypeFunctionInstanceType>(there))
    {
        NormalizedType thereNorm{builtinTypes};
        NormalizedType topNorm{builtinTypes};
        topNorm.tops = builtinTypes->unknownType;
        thereNorm.tyvars.insert_or_assign(there, std::make_unique<NormalizedType>(std::move(topNorm)));
        here.isCacheable = false;
        return intersectNormals(here, thereNorm);
    }

    NormalizedTyvars tyvars = std::move(here.tyvars);

    if (const FunctionType* utv = get<FunctionType>(there))
    {
        NormalizedFunctionType functions = std::move(here.functions);
        clearNormal(here);
        intersectFunctionsWithFunction(functions, there);
        here.functions = std::move(functions);
    }
    else if (get<TableType>(there) || get<MetatableType>(there))
    {
        if (useNewLuauSolver())
        {
            NormalizedExternType externTypes = std::move(here.externTypes);
            TypeIds tables = std::move(here.tables);
            clearNormal(here);
            intersectTablesWithTable(tables, there, seenTablePropPairs, seenSetTypes);
            here.tables = std::move(tables);
            here.externTypes = std::move(externTypes);
        }
        else
        {
            TypeIds tables = std::move(here.tables);
            clearNormal(here);
            intersectTablesWithTable(tables, there, seenTablePropPairs, seenSetTypes);
            here.tables = std::move(tables);
        }
    }
    else if (get<ExternType>(there))
    {
        NormalizedExternType nct = std::move(here.externTypes);
        clearNormal(here);
        intersectExternTypesWithExternType(nct, there);
        here.externTypes = std::move(nct);
    }
    else if (get<ErrorType>(there))
    {
        TypeId errors = here.errors;
        clearNormal(here);
        here.errors = get<ErrorType>(errors) ? errors : there;
    }
    else if (const PrimitiveType* ptv = get<PrimitiveType>(there))
    {
        TypeId booleans = here.booleans;
        TypeId nils = here.nils;
        TypeId numbers = here.numbers;
        NormalizedStringType strings = std::move(here.strings);
        NormalizedFunctionType functions = std::move(here.functions);
        TypeId threads = here.threads;
        TypeId buffers = here.buffers;
        TypeIds tables = std::move(here.tables);

        clearNormal(here);

        if (ptv->type == PrimitiveType::Boolean)
            here.booleans = booleans;
        else if (ptv->type == PrimitiveType::NilType)
            here.nils = nils;
        else if (ptv->type == PrimitiveType::Number)
            here.numbers = numbers;
        else if (ptv->type == PrimitiveType::String)
            here.strings = std::move(strings);
        else if (ptv->type == PrimitiveType::Thread)
            here.threads = threads;
        else if (ptv->type == PrimitiveType::Buffer)
            here.buffers = buffers;
        else if (ptv->type == PrimitiveType::Function)
            here.functions = std::move(functions);
        else if (ptv->type == PrimitiveType::Table)
            here.tables = std::move(tables);
        else
            LUAU_ASSERT(!"Unreachable");
    }
    else if (const SingletonType* stv = get<SingletonType>(there))
    {
        TypeId booleans = here.booleans;
        NormalizedStringType strings = std::move(here.strings);

        clearNormal(here);

        if (get<BooleanSingleton>(stv))
            here.booleans = intersectionOfBools(booleans, there);
        else if (const StringSingleton* sstv = get<StringSingleton>(stv))
        {
            if (strings.includes(sstv->value))
                here.strings.singletons.insert({sstv->value, there});
        }
        else
            LUAU_ASSERT(!"Unreachable");
    }
    else if (const NegationType* ntv = get<NegationType>(there))
    {
        TypeId t = follow(ntv->ty);
        if (const PrimitiveType* ptv = get<PrimitiveType>(t))
            subtractPrimitive(here, ntv->ty);
        else if (const SingletonType* stv = get<SingletonType>(t))
            subtractSingleton(here, follow(ntv->ty));
        else if (get<ExternType>(t))
        {
            NormalizationResult res = intersectNormalWithNegationTy(t, here);
            if (shouldEarlyExit(res))
                return res;
        }
        else if (const UnionType* itv = get<UnionType>(t))
        {
            for (TypeId part : itv->options)
            {
                NormalizationResult res = intersectNormalWithNegationTy(part, here);
                if (shouldEarlyExit(res))
                    return res;
            }
        }
        else if (get<AnyType>(t))
        {
            // HACK: Refinements sometimes intersect with ~any under the
            // assumption that it is the same as any.
            return NormalizationResult::True;
        }
        else if (get<NoRefineType>(t))
        {
            // `*no-refine*` means we will never do anything to affect the intersection.
            return NormalizationResult::True;
        }
        else if (get<NeverType>(t))
        {
            // if we're intersecting with `~never`, this is equivalent to intersecting with `unknown`
            // this is a noop since an intersection with `unknown` is trivial.
            return NormalizationResult::True;
        }
        else if (get<UnknownType>(t))
        {
            // if we're intersecting with `~unknown`, this is equivalent to intersecting with `never`
            // this means we should clear the type entirely.
            clearNormal(here);
            return NormalizationResult::True;
        }
        else if (get<ErrorType>(t))
        {
            // ~error is still an error, so intersecting with the negation is the same as intersecting with a type
            TypeId errors = here.errors;
            clearNormal(here);
            here.errors = get<ErrorType>(errors) ? errors : t;
        }
        else if (auto nt = get<NegationType>(t))
        {
            here.tyvars = std::move(tyvars);
            return intersectNormalWithTy(here, nt->ty, seenTablePropPairs, seenSetTypes);
        }
        else
        {
            // TODO negated unions, intersections, table, and function.
            // Report a TypeError for other types.
            LUAU_ASSERT(!"Unimplemented");
        }
    }
    else if (get<NeverType>(there))
    {
        here.externTypes.resetToNever();
    }
    else if (get<NoRefineType>(there))
    {
        // `*no-refine*` means we will never do anything to affect the intersection.
        return NormalizationResult::True;
    }
    else
        LUAU_ASSERT(!"Unreachable");

    NormalizationResult res = intersectTyvarsWithTy(tyvars, there, seenTablePropPairs, seenSetTypes);
    if (res != NormalizationResult::True)
        return res;
    here.tyvars = std::move(tyvars);

    return NormalizationResult::True;
}

void makeTableShared(TypeId ty, DenseHashSet<TypeId>& seen)
{
    ty = follow(ty);
    if (seen.contains(ty))
        return;
    seen.insert(ty);
    if (auto tableTy = getMutable<TableType>(ty))
    {
        for (auto& [_, prop] : tableTy->props)
            prop.makeShared();
    }
    else if (auto metatableTy = get<MetatableType>(ty))
    {
        makeTableShared(metatableTy->metatable, seen);
        makeTableShared(metatableTy->table, seen);
    }
}

void makeTableShared(TypeId ty)
{
    DenseHashSet<TypeId> seen{nullptr};
    makeTableShared(ty, seen);
}

// -------- Convert back from a normalized type to a type
TypeId Normalizer::typeFromNormal(const NormalizedType& norm)
{
    assertInvariant(norm);
    if (!get<NeverType>(norm.tops))
        return norm.tops;

    std::vector<TypeId> result;

    if (!get<NeverType>(norm.booleans))
        result.push_back(norm.booleans);

    if (isTop(builtinTypes, norm.externTypes))
    {
        result.push_back(builtinTypes->externType);
    }
    else if (!norm.externTypes.isNever())
    {
        std::vector<TypeId> parts;
        parts.reserve(norm.externTypes.externTypes.size());

        for (const TypeId normTy : norm.externTypes.ordering)
        {
            const TypeIds& normNegations = norm.externTypes.externTypes.at(normTy);

            if (normNegations.empty())
            {
                parts.push_back(normTy);
            }
            else
            {
                std::vector<TypeId> intersection;
                intersection.reserve(normNegations.size() + 1);

                intersection.push_back(normTy);
                for (TypeId negation : normNegations)
                {
                    intersection.push_back(arena->addType(NegationType{negation}));
                }

                parts.push_back(arena->addType(IntersectionType{std::move(intersection)}));
            }
        }

        if (parts.size() == 1)
        {
            result.push_back(parts.at(0));
        }
        else if (parts.size() > 1)
        {
            result.push_back(arena->addType(UnionType{std::move(parts)}));
        }
    }

    if (!get<NeverType>(norm.errors))
        result.push_back(norm.errors);
    if (norm.functions.isTop)
        result.push_back(builtinTypes->functionType);
    else if (!norm.functions.isNever())
    {
        if (norm.functions.parts.size() == 1)
            result.push_back(*norm.functions.parts.begin());
        else
        {
            std::vector<TypeId> parts;
            parts.insert(parts.end(), norm.functions.parts.begin(), norm.functions.parts.end());
            result.push_back(arena->addType(IntersectionType{std::move(parts)}));
        }
    }
    if (!get<NeverType>(norm.nils))
        result.push_back(norm.nils);
    if (!get<NeverType>(norm.numbers))
        result.push_back(norm.numbers);
    if (norm.strings.isString())
        result.push_back(builtinTypes->stringType);
    else if (norm.strings.isUnion())
    {
        for (auto& [_, ty] : norm.strings.singletons)
            result.push_back(ty);
    }
    else if (norm.strings.isIntersection())
    {
        std::vector<TypeId> parts;
        parts.push_back(builtinTypes->stringType);
        for (const auto& [name, ty] : norm.strings.singletons)
            parts.push_back(arena->addType(NegationType{ty}));

        result.push_back(arena->addType(IntersectionType{std::move(parts)}));
    }
    if (!get<NeverType>(norm.threads))
        result.push_back(builtinTypes->threadType);
    if (!get<NeverType>(norm.buffers))
        result.push_back(builtinTypes->bufferType);

    if (useNewLuauSolver())
    {
        result.reserve(result.size() + norm.tables.size());
        for (auto table : norm.tables)
            result.push_back(table);
    }
    else
        result.insert(result.end(), norm.tables.begin(), norm.tables.end());

    for (auto& [tyvar, intersect] : norm.tyvars)
    {
        if (get<NeverType>(intersect->tops))
        {
            TypeId ty = typeFromNormal(*intersect);
            if (FFlag::LuauReduceSetTypeStackPressure)
                result.push_back(addIntersection(NotNull{arena}, builtinTypes, {tyvar, ty}));
            else
                result.push_back(arena->addType(IntersectionType{{tyvar, ty}}));
        }
        else
            result.push_back(tyvar);
    }

    if (result.size() == 0)
        return builtinTypes->neverType;
    else if (result.size() == 1)
        return result[0];
    else
        return arena->addType(UnionType{std::move(result)});
}

bool isSubtype(
    TypeId subTy,
    TypeId superTy,
    NotNull<Scope> scope,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Simplifier> simplifier,
    InternalErrorReporter& ice,
    SolverMode solverMode
)
{
    UnifierSharedState sharedState{&ice};
    TypeArena arena;
    TypeCheckLimits limits;
    TypeFunctionRuntime typeFunctionRuntime{
        NotNull{&ice}, NotNull{&limits}
    }; // TODO: maybe subtyping checks should not invoke user-defined type function runtime

    if (FFlag::LuauUseWorkspacePropToChooseSolver)
    {
        Normalizer normalizer{&arena, builtinTypes, NotNull{&sharedState}, solverMode};
        if (solverMode == SolverMode::New)
        {
            Subtyping subtyping{builtinTypes, NotNull{&arena}, simplifier, NotNull{&normalizer}, NotNull{&typeFunctionRuntime}, NotNull{&ice}};

            return subtyping.isSubtype(subTy, superTy, scope).isSubtype;
        }
        else
        {
            Unifier u{NotNull{&normalizer}, scope, Location{}, Covariant};

            u.tryUnify(subTy, superTy);
            return !u.failure;
        }
    }
    else
    {
        Normalizer normalizer{&arena, builtinTypes, NotNull{&sharedState}, FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old};
        if (FFlag::LuauSolverV2)
        {
            Subtyping subtyping{builtinTypes, NotNull{&arena}, simplifier, NotNull{&normalizer}, NotNull{&typeFunctionRuntime}, NotNull{&ice}};

            return subtyping.isSubtype(subTy, superTy, scope).isSubtype;
        }
        else
        {
            Unifier u{NotNull{&normalizer}, scope, Location{}, Covariant};

            u.tryUnify(subTy, superTy);
            return !u.failure;
        }
    }
}

bool isSubtype(
    TypePackId subPack,
    TypePackId superPack,
    NotNull<Scope> scope,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Simplifier> simplifier,
    InternalErrorReporter& ice,
    SolverMode solverMode
)
{
    UnifierSharedState sharedState{&ice};
    TypeArena arena;
    TypeCheckLimits limits;
    TypeFunctionRuntime typeFunctionRuntime{
        NotNull{&ice}, NotNull{&limits}
    }; // TODO: maybe subtyping checks should not invoke user-defined type function runtime

    if (FFlag::LuauUseWorkspacePropToChooseSolver)
    {
        Normalizer normalizer{&arena, builtinTypes, NotNull{&sharedState}, solverMode};
        if (solverMode == SolverMode::New)
        {
            Subtyping subtyping{builtinTypes, NotNull{&arena}, simplifier, NotNull{&normalizer}, NotNull{&typeFunctionRuntime}, NotNull{&ice}};

            return FFlag::LuauPassBindableGenericsByReference ? subtyping.isSubtype(subPack, superPack, scope, {}).isSubtype
                                                              : subtyping.isSubtype_DEPRECATED(subPack, superPack, scope).isSubtype;
        }
        else
        {
            Unifier u{NotNull{&normalizer}, scope, Location{}, Covariant};

            u.tryUnify(subPack, superPack);
            return !u.failure;
        }
    }
    else
    {
        Normalizer normalizer{&arena, builtinTypes, NotNull{&sharedState}, FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old};
        if (FFlag::LuauSolverV2)
        {
            Subtyping subtyping{builtinTypes, NotNull{&arena}, simplifier, NotNull{&normalizer}, NotNull{&typeFunctionRuntime}, NotNull{&ice}};

            return FFlag::LuauPassBindableGenericsByReference ? subtyping.isSubtype(subPack, superPack, scope, {}).isSubtype
                                                              : subtyping.isSubtype_DEPRECATED(subPack, superPack, scope).isSubtype;
        }
        else
        {
            Unifier u{NotNull{&normalizer}, scope, Location{}, Covariant};

            u.tryUnify(subPack, superPack);
            return !u.failure;
        }
    }
}

} // namespace Luau
