// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Subtyping.h"

#include "iostream"
#include "Luau/Common.h"
#include "Luau/Error.h"
#include "Luau/Normalize.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/Substitution.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeIds.h"
#include "Luau/TypePack.h"
#include "Luau/TypePath.h"
#include "Luau/TypeUtils.h"

LUAU_FASTFLAGVARIABLE(LuauIndividualRecursionLimits)
LUAU_DYNAMIC_FASTINTVARIABLE(LuauSubtypingRecursionLimit, 100)

LUAU_FASTFLAGVARIABLE(DebugLuauSubtypingCheckPathValidity)
LUAU_FASTINTVARIABLE(LuauSubtypingReasoningLimit, 100)
LUAU_FASTFLAGVARIABLE(LuauReturnMappedGenericPacksFromSubtyping3)
LUAU_FASTFLAGVARIABLE(LuauSubtypingNegationsChecksNormalizationComplexity)
LUAU_FASTFLAGVARIABLE(LuauSubtypingGenericsDoesntUseVariance)
LUAU_FASTFLAG(LuauEmplaceNotPushBack)
LUAU_FASTFLAGVARIABLE(LuauSubtypingReportGenericBoundMismatches2)
LUAU_FASTFLAGVARIABLE(LuauTrackUniqueness)
LUAU_FASTFLAGVARIABLE(LuauSubtypingGenericPacksDoesntUseVariance)
LUAU_FASTFLAGVARIABLE(LuauSubtypingUnionsAndIntersectionsInGenericBounds)
LUAU_FASTFLAGVARIABLE(LuauIndexInMetatableSubtyping)
LUAU_FASTFLAGVARIABLE(LuauSubtypingPackRecursionLimits)
LUAU_FASTFLAGVARIABLE(LuauSubtypingPrimitiveAndGenericTableTypes)
LUAU_FASTFLAGVARIABLE(LuauPassBindableGenericsByReference)

namespace Luau
{

struct VarianceFlipper
{
    Subtyping::Variance* variance;
    Subtyping::Variance oldValue;

    explicit VarianceFlipper(Subtyping::Variance* v)
        : variance(v)
        , oldValue(*v)
    {
        switch (oldValue)
        {
        case Subtyping::Variance::Covariant:
            *variance = Subtyping::Variance::Contravariant;
            break;
        case Subtyping::Variance::Contravariant:
            *variance = Subtyping::Variance::Covariant;
            break;
        }
    }

    ~VarianceFlipper()
    {
        *variance = oldValue;
    }
};

bool SubtypingReasoning::operator==(const SubtypingReasoning& other) const
{
    return subPath == other.subPath && superPath == other.superPath && variance == other.variance;
}

size_t SubtypingReasoningHash::operator()(const SubtypingReasoning& r) const
{
    return TypePath::PathHash()(r.subPath) ^ (TypePath::PathHash()(r.superPath) << 1) ^ (static_cast<size_t>(r.variance) << 1);
}

MappedGenericEnvironment::MappedGenericFrame::MappedGenericFrame(
    DenseHashMap<TypePackId, std::optional<TypePackId>> mappings,
    const std::optional<size_t> parentScopeIndex
)
    : mappings(std::move(mappings))
    , parentScopeIndex(parentScopeIndex)
{
    LUAU_ASSERT(FFlag::LuauSubtypingGenericPacksDoesntUseVariance);
}

MappedGenericEnvironment::LookupResult MappedGenericEnvironment::lookupGenericPack(TypePackId genericTp) const
{
    LUAU_ASSERT(FFlag::LuauSubtypingGenericPacksDoesntUseVariance);

    genericTp = follow(genericTp);

    std::optional<size_t> currentFrameIndex = currentScopeIndex;

    while (currentFrameIndex)
    {
        const MappedGenericFrame& currentFrame = frames[*currentFrameIndex];
        if (const auto mappedPack = currentFrame.mappings.find(genericTp))
        {
            if (*mappedPack)
                return mappedPack->value();
            else
                return Unmapped{*currentFrameIndex};
        }

        currentFrameIndex = currentFrame.parentScopeIndex;
    }

    // Do a DFS of children to see if any enclosed scope mentions this generic pack
    if (currentScopeIndex)
    {
        const MappedGenericFrame& baseFrame = frames[*currentScopeIndex];
        std::vector<size_t> toCheck = std::vector(baseFrame.children.begin(), baseFrame.children.end());

        while (!toCheck.empty())
        {
            const size_t currIndex = toCheck.back();
            toCheck.pop_back();

            const MappedGenericFrame& currentFrame = frames[currIndex];
            if (const auto mappedPack = currentFrame.mappings.find(genericTp))
            {
                if (*mappedPack)
                    return mappedPack->value();
                else
                    return Unmapped{currIndex};
            }

            toCheck.insert(toCheck.end(), currentFrame.children.begin(), currentFrame.children.end());
        }
    }

    return NotBindable{};
}

void MappedGenericEnvironment::pushFrame(const std::vector<TypePackId>& genericTps)
{
    LUAU_ASSERT(FFlag::LuauSubtypingGenericPacksDoesntUseVariance);

    DenseHashMap<TypePackId, std::optional<TypePackId>> mappings{nullptr};

    for (TypePackId tp : genericTps)
        mappings[tp] = std::nullopt;

    frames.emplace_back(std::move(mappings), currentScopeIndex);

    const size_t newFrameIndex = frames.size() - 1;

    if (currentScopeIndex)
        frames[*currentScopeIndex].children.insert(newFrameIndex);

    currentScopeIndex = newFrameIndex;
}

void MappedGenericEnvironment::popFrame()
{
    LUAU_ASSERT(FFlag::LuauSubtypingGenericPacksDoesntUseVariance);
    LUAU_ASSERT(currentScopeIndex);
    if (currentScopeIndex)
    {
        const std::optional<size_t> newFrameIndex = frames[*currentScopeIndex].parentScopeIndex;
        currentScopeIndex = newFrameIndex.value_or(0);
    }
}

bool MappedGenericEnvironment::bindGeneric(TypePackId genericTp, TypePackId bindeeTp)
{
    LUAU_ASSERT(FFlag::LuauSubtypingGenericPacksDoesntUseVariance);
    // We shouldn't bind generic type packs to themselves
    if (genericTp == bindeeTp)
        return true;

    if (!get<GenericTypePack>(genericTp))
    {
        LUAU_ASSERT(!"bindGeneric should not be called with a non-generic type pack");
        return false;
    }

    const LookupResult lookupResult = lookupGenericPack(genericTp);
    if (const Unmapped* unmapped = get_if<Unmapped>(&lookupResult))
    {
        frames[unmapped->scopeIndex].mappings[genericTp] = bindeeTp;
        return true;
    }
    else
    {
        LUAU_ASSERT(!"bindGeneric called on a non-bindable generic type pack");
        return false;
    }
}

template<typename TID>
static void assertReasoningValid_DEPRECATED(TID subTy, TID superTy, const SubtypingResult& result, NotNull<BuiltinTypes> builtinTypes)
{
    if (!FFlag::DebugLuauSubtypingCheckPathValidity)
        return;

    for (const SubtypingReasoning& reasoning : result.reasoning)
    {
        LUAU_ASSERT(traverse_DEPRECATED(subTy, reasoning.subPath, builtinTypes));
        LUAU_ASSERT(traverse_DEPRECATED(superTy, reasoning.superPath, builtinTypes));
    }
}

template<typename TID>
static void assertReasoningValid(TID subTy, TID superTy, const SubtypingResult& result, NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena)
{
    LUAU_ASSERT(FFlag::LuauReturnMappedGenericPacksFromSubtyping3);

    if (!FFlag::DebugLuauSubtypingCheckPathValidity)
        return;

    for (const SubtypingReasoning& reasoning : result.reasoning)
    {
        if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
        {
            LUAU_ASSERT(traverse(subTy, reasoning.subPath, builtinTypes, arena));
            LUAU_ASSERT(traverse(superTy, reasoning.superPath, builtinTypes, arena));
        }
        else
        {
            LUAU_ASSERT(traverse_DEPRECATED(subTy, reasoning.subPath, builtinTypes, NotNull{&result.mappedGenericPacks_DEPRECATED}, arena));
            LUAU_ASSERT(traverse_DEPRECATED(superTy, reasoning.superPath, builtinTypes, NotNull{&result.mappedGenericPacks_DEPRECATED}, arena));
        }
    }
}

template<>
void assertReasoningValid_DEPRECATED<TableIndexer>(
    TableIndexer subIdx,
    TableIndexer superIdx,
    const SubtypingResult& result,
    NotNull<BuiltinTypes> builtinTypes
)
{
    // Empty method to satisfy the compiler.
}

template<>
void assertReasoningValid<TableIndexer>(
    TableIndexer subIdx,
    TableIndexer superIdx,
    const SubtypingResult& result,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena
)
{
    // Empty method to satisfy the compiler.
}

static SubtypingReasonings mergeReasonings(const SubtypingReasonings& a, const SubtypingReasonings& b)
{
    SubtypingReasonings result{kEmptyReasoning};

    for (const SubtypingReasoning& r : a)
    {
        if (r.variance == SubtypingVariance::Invariant)
            result.insert(r);
        else if (r.variance == SubtypingVariance::Covariant || r.variance == SubtypingVariance::Contravariant)
        {
            SubtypingReasoning inverseReasoning = SubtypingReasoning{
                r.subPath, r.superPath, r.variance == SubtypingVariance::Covariant ? SubtypingVariance::Contravariant : SubtypingVariance::Covariant
            };
            if (b.contains(inverseReasoning))
                result.insert(SubtypingReasoning{r.subPath, r.superPath, SubtypingVariance::Invariant});
            else
                result.insert(r);
        }

        if (result.size() >= size_t(FInt::LuauSubtypingReasoningLimit))
            return result;
    }

    for (const SubtypingReasoning& r : b)
    {
        if (r.variance == SubtypingVariance::Invariant)
            result.insert(r);
        else if (r.variance == SubtypingVariance::Covariant || r.variance == SubtypingVariance::Contravariant)
        {
            SubtypingReasoning inverseReasoning = SubtypingReasoning{
                r.subPath, r.superPath, r.variance == SubtypingVariance::Covariant ? SubtypingVariance::Contravariant : SubtypingVariance::Covariant
            };
            if (a.contains(inverseReasoning))
                result.insert(SubtypingReasoning{r.subPath, r.superPath, SubtypingVariance::Invariant});
            else
                result.insert(r);
        }

        if (result.size() >= size_t(FInt::LuauSubtypingReasoningLimit))
            return result;
    }

    return result;
}

SubtypingResult& SubtypingResult::andAlso(const SubtypingResult& other)
{
    // If the other result is not a subtype, we want to join all of its
    // reasonings to this one. If this result already has reasonings of its own,
    // those need to be attributed here whenever this _also_ failed.
    if (!other.isSubtype)
        reasoning = isSubtype ? std::move(other.reasoning) : mergeReasonings(reasoning, other.reasoning);

    isSubtype &= other.isSubtype;
    normalizationTooComplex |= other.normalizationTooComplex;
    isCacheable &= other.isCacheable;
    errors.insert(errors.end(), other.errors.begin(), other.errors.end());
    if (FFlag::LuauSubtypingReportGenericBoundMismatches2)
        genericBoundsMismatches.insert(genericBoundsMismatches.end(), other.genericBoundsMismatches.begin(), other.genericBoundsMismatches.end());

    return *this;
}

SubtypingResult& SubtypingResult::orElse(const SubtypingResult& other)
{
    // If this result is a subtype, we do not join the reasoning lists. If this
    // result is not a subtype, but the other is a subtype, we want to _clear_
    // our reasoning list. If both results are not subtypes, we join the
    // reasoning lists.
    if (!isSubtype)
    {
        if (other.isSubtype)
            reasoning.clear();
        else
            reasoning = mergeReasonings(reasoning, other.reasoning);
    }

    isSubtype |= other.isSubtype;
    normalizationTooComplex |= other.normalizationTooComplex;
    isCacheable &= other.isCacheable;
    errors.insert(errors.end(), other.errors.begin(), other.errors.end());
    if (FFlag::LuauSubtypingReportGenericBoundMismatches2)
        genericBoundsMismatches.insert(genericBoundsMismatches.end(), other.genericBoundsMismatches.begin(), other.genericBoundsMismatches.end());

    return *this;
}

SubtypingResult& SubtypingResult::withBothComponent(TypePath::Component component)
{
    return withSubComponent(component).withSuperComponent(std::move(component));
}

SubtypingResult& SubtypingResult::withSubComponent(TypePath::Component component)
{
    if (reasoning.empty())
        reasoning.insert(SubtypingReasoning{Path(std::move(component)), TypePath::kEmpty});
    else
    {
        for (auto& r : reasoning)
            r.subPath = r.subPath.push_front(component);
    }

    return *this;
}

SubtypingResult& SubtypingResult::withSuperComponent(TypePath::Component component)
{
    if (reasoning.empty())
        reasoning.insert(SubtypingReasoning{TypePath::kEmpty, Path(std::move(component))});
    else
    {
        for (auto& r : reasoning)
            r.superPath = r.superPath.push_front(component);
    }

    return *this;
}

SubtypingResult& SubtypingResult::withBothPath(TypePath::Path path)
{
    return withSubPath(path).withSuperPath(std::move(path));
}

SubtypingResult& SubtypingResult::withSubPath(TypePath::Path path)
{
    if (reasoning.empty())
        reasoning.insert(SubtypingReasoning{std::move(path), TypePath::kEmpty});
    else
    {
        for (auto& r : reasoning)
            r.subPath = path.append(r.subPath);
    }

    return *this;
}

SubtypingResult& SubtypingResult::withSuperPath(TypePath::Path path)
{
    if (reasoning.empty())
        reasoning.insert(SubtypingReasoning{TypePath::kEmpty, std::move(path)});
    else
    {
        for (auto& r : reasoning)
            r.superPath = path.append(r.superPath);
    }

    return *this;
}

SubtypingResult& SubtypingResult::withErrors(ErrorVec& err)
{
    for (TypeError& e : err)
        errors.emplace_back(e);
    return *this;
}

SubtypingResult& SubtypingResult::withError(TypeError err)
{
    errors.push_back(std::move(err));
    return *this;
}

SubtypingResult SubtypingResult::negate(const SubtypingResult& result)
{
    return SubtypingResult{
        !result.isSubtype,
        result.normalizationTooComplex,
    };
}

SubtypingResult SubtypingResult::all(const std::vector<SubtypingResult>& results)
{
    SubtypingResult acc{true};
    for (const SubtypingResult& current : results)
        acc.andAlso(current);
    return acc;
}

SubtypingResult SubtypingResult::any(const std::vector<SubtypingResult>& results)
{
    SubtypingResult acc{false};
    for (const SubtypingResult& current : results)
        acc.orElse(current);
    return acc;
}

struct ApplyMappedGenerics : Substitution
{
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<TypeArena> arena;
    // TODO: make this NotNull when LuauSubtypingGenericsDoesntUseVariance is clipped
    InternalErrorReporter* iceReporter;

    SubtypingEnvironment& env;

    // TODO: Clip with LuauSubtypingGenericsDoesntUseVariance
    ApplyMappedGenerics(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, SubtypingEnvironment& env)
        : Substitution(TxnLog::empty(), arena)
        , builtinTypes(builtinTypes)
        , arena(arena)
        , env(env)
    {
    }

    ApplyMappedGenerics(
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<TypeArena> arena,
        SubtypingEnvironment& env,
        NotNull<InternalErrorReporter> iceReporter
    )
        : Substitution(TxnLog::empty(), arena)
        , builtinTypes(builtinTypes)
        , arena(arena)
        , iceReporter(iceReporter.get())
        , env(env)
    {
    }

    bool isDirty(TypeId ty) override
    {
        return env.containsMappedType(ty);
    }

    bool isDirty(TypePackId tp) override
    {
        return env.containsMappedPack(tp);
    }

    TypeId clean(TypeId ty) override
    {
        if (FFlag::LuauSubtypingGenericsDoesntUseVariance)
        {
            const auto& [lowerBound, upperBound] = env.getMappedTypeBounds(ty, NotNull{iceReporter});

            if (upperBound.empty() && lowerBound.empty())
            {
                // No bounds for the generic we're mapping.
                // In this case, unknown vs never is an arbitrary choice:
                // ie, does it matter if we map add<A, A> to add<unknown, unknown> or add<never, never> in the context of subtyping?
                // We choose unknown here, since it's closest to the original behavior.
                return builtinTypes->unknownType;
            }
            else if (!upperBound.empty())
            {
                TypeIds boundsToUse;

                for (TypeId ub : upperBound)
                {
                    // quick and dirty check to avoid adding generic types
                    if (!get<GenericType>(ub))
                        boundsToUse.insert(ub);
                }

                if (boundsToUse.empty())
                {
                    // This case happens when we've collected no bounds for the generic we're mapping.
                    // In this case, unknown vs never is an arbitrary choice:
                    // ie, does it matter if we map add<A, A> to add<unknown, unknown> or add<never, never> in the context of subtyping?
                    // We choose unknown here, since it's closest to the original behavior.
                    return builtinTypes->unknownType;
                }
                if (boundsToUse.size() == 1)
                    return *boundsToUse.begin();

                return arena->addType(IntersectionType{boundsToUse.take()});
            }
            else if (!lowerBound.empty())
            {
                TypeIds boundsToUse;

                for (TypeId lb : lowerBound)
                {
                    // quick and dirty check to avoid adding generic types
                    if (!get<GenericType>(lb))
                        boundsToUse.insert(lb);
                }

                if (boundsToUse.empty())
                {
                    // This case happens when we've collected no bounds for the generic we're mapping.
                    // In this case, unknown vs never is an arbitrary choice:
                    // ie, does it matter if we map add<A, A> to add<unknown, unknown> or add<never, never> in the context of subtyping?
                    // We choose unknown here, since it's closest to the original behavior.
                    return builtinTypes->unknownType;
                }
                else if (lowerBound.size() == 1)
                    return *boundsToUse.begin();
                else
                    return arena->addType(UnionType{boundsToUse.take()});
            }
            else
            {
                LUAU_ASSERT(!"Unreachable path");
                return builtinTypes->unknownType;
            }
        }
        else
        {
            const auto& bounds = env.getMappedTypeBounds_DEPRECATED(ty);

            if (bounds.upperBound.empty())
                return builtinTypes->unknownType;

            if (bounds.upperBound.size() == 1)
                return *begin(bounds.upperBound);

            return arena->addType(IntersectionType{std::vector<TypeId>(begin(bounds.upperBound), end(bounds.upperBound))});
        }
    }

    TypePackId clean(TypePackId tp) override
    {
        if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
        {
            const MappedGenericEnvironment::LookupResult result = env.mappedGenericPacks.lookupGenericPack(tp);
            if (const TypePackId* mappedGen = get_if<TypePackId>(&result))
                return *mappedGen;
        }
        else if (auto it = env.getMappedPackBounds_DEPRECATED(tp))
            return *it;

        // Clean is only called when isDirty found a pack bound
        LUAU_ASSERT(!"Unreachable");
        return nullptr;
    }

    bool ignoreChildren(TypeId ty) override
    {
        if (get<ExternType>(ty))
            return true;

        if (FFlag::LuauSubtypingGenericsDoesntUseVariance)
        {
            if (const FunctionType* f = get<FunctionType>(ty))
            {
                for (TypeId g : f->generics)
                {
                    if (const std::vector<SubtypingEnvironment::GenericBounds>* bounds = env.mappedGenerics.find(g); bounds && !bounds->empty())
                        // We don't want to mutate the generics of a function that's being subtyped
                        return true;
                }
            }
        }

        return ty->persistent;
    }
    bool ignoreChildren(TypePackId ty) override
    {
        return ty->persistent;
    }
};

std::optional<TypeId> SubtypingEnvironment::applyMappedGenerics(
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    TypeId ty,
    NotNull<InternalErrorReporter> iceReporter
)
{
    LUAU_ASSERT(FFlag::LuauSubtypingGenericsDoesntUseVariance);
    ApplyMappedGenerics amg{builtinTypes, arena, *this, iceReporter};
    return amg.substitute(ty);
}

std::optional<TypeId> SubtypingEnvironment::applyMappedGenerics_DEPRECATED(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, TypeId ty)
{
    ApplyMappedGenerics amg{builtinTypes, arena, *this};
    return amg.substitute(ty);
}

const TypeId* SubtypingEnvironment::tryFindSubstitution(TypeId ty) const
{
    if (auto it = substitutions.find(ty))
        return it;

    if (parent)
        return parent->tryFindSubstitution(ty);

    return nullptr;
}

const SubtypingResult* SubtypingEnvironment::tryFindSubtypingResult(std::pair<TypeId, TypeId> subAndSuper) const
{
    if (FFlag::LuauSubtypingGenericsDoesntUseVariance)
    {
        if (const auto it = seenSetCache.find(subAndSuper))
            return it;
    }
    else if (auto it = ephemeralCache.find(subAndSuper))
        return it;

    if (parent)
        return parent->tryFindSubtypingResult(subAndSuper);

    return nullptr;
}

bool SubtypingEnvironment::containsMappedType(TypeId ty) const
{
    if (FFlag::LuauSubtypingGenericsDoesntUseVariance)
    {
        ty = follow(ty);
        if (const auto bounds = mappedGenerics.find(ty); bounds && !bounds->empty())
            return true;

        if (parent)
            return parent->containsMappedType(ty);

        return false;
    }
    else
    {
        if (mappedGenerics_DEPRECATED.contains(ty))
            return true;

        if (parent)
            return parent->containsMappedType(ty);

        return false;
    }
}

bool SubtypingEnvironment::containsMappedPack(TypePackId tp) const
{
    if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
    {
        if (const MappedGenericEnvironment::LookupResult lookupResult = mappedGenericPacks.lookupGenericPack(tp); get_if<TypePackId>(&lookupResult))
            return true;
    }
    else if (mappedGenericPacks_DEPRECATED.contains(tp))
        return true;

    if (parent)
        return parent->containsMappedPack(tp);

    return false;
}

SubtypingEnvironment::GenericBounds& SubtypingEnvironment::getMappedTypeBounds(TypeId ty, NotNull<InternalErrorReporter> iceReporter)
{
    LUAU_ASSERT(FFlag::LuauSubtypingGenericsDoesntUseVariance);
    ty = follow(ty);
    std::vector<GenericBounds>* bounds = mappedGenerics.find(ty);
    if (bounds && !bounds->empty())
        return bounds->back();

    if (parent)
        return parent->getMappedTypeBounds(ty, iceReporter);

    LUAU_ASSERT(!"Use containsMappedType before asking for bounds!");
    iceReporter->ice("Trying to access bounds for a type with no in-scope bounds");
}

SubtypingEnvironment::GenericBounds_DEPRECATED& SubtypingEnvironment::getMappedTypeBounds_DEPRECATED(TypeId ty)
{
    LUAU_ASSERT(!FFlag::LuauSubtypingGenericsDoesntUseVariance);
    if (auto it = mappedGenerics_DEPRECATED.find(ty))
        return *it;

    if (parent)
        return parent->getMappedTypeBounds_DEPRECATED(ty);

    LUAU_ASSERT(!"Use containsMappedType before asking for bounds!");
    return mappedGenerics_DEPRECATED[ty];
}

TypePackId* SubtypingEnvironment::getMappedPackBounds_DEPRECATED(TypePackId tp)
{
    LUAU_ASSERT(!FFlag::LuauSubtypingGenericPacksDoesntUseVariance);

    if (auto it = mappedGenericPacks_DEPRECATED.find(tp))
        return it;

    if (parent)
        return parent->getMappedPackBounds_DEPRECATED(tp);

    // This fallback is reachable in valid cases, unlike the final part of getMappedTypeBounds
    return nullptr;
}

Subtyping::Subtyping(
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> typeArena,
    NotNull<Simplifier> simplifier,
    NotNull<Normalizer> normalizer,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    NotNull<InternalErrorReporter> iceReporter
)
    : builtinTypes(builtinTypes)
    , arena(typeArena)
    , simplifier(simplifier)
    , normalizer(normalizer)
    , typeFunctionRuntime(typeFunctionRuntime)
    , iceReporter(iceReporter)
{
}

SubtypingResult Subtyping::isSubtype(TypeId subTy, TypeId superTy, NotNull<Scope> scope)
{
    SubtypingEnvironment env;

    SubtypingResult result = isCovariantWith(env, subTy, superTy, scope);

    if (result.normalizationTooComplex)
    {
        if (result.isCacheable)
            resultCache[{subTy, superTy}] = result;

        return result;
    }

    if (FFlag::LuauSubtypingGenericsDoesntUseVariance)
    {
        for (const auto& [_, bounds] : env.mappedGenerics)
            LUAU_ASSERT(bounds.empty());
    }
    else
    {
        for (const auto& [subTy, bounds] : env.mappedGenerics_DEPRECATED)
        {
            const auto& lb = bounds.lowerBound;
            const auto& ub = bounds.upperBound;
            TypeId lowerBound = makeAggregateType<UnionType>(lb, builtinTypes->neverType);
            TypeId upperBound = makeAggregateType<IntersectionType>(ub, builtinTypes->unknownType);

            std::shared_ptr<const NormalizedType> nt = normalizer->normalize(upperBound);
            // we say that the result is true if normalization failed because complex types are likely to be inhabited.
            NormalizationResult res = nt ? normalizer->isInhabited(nt.get()) : NormalizationResult::True;

            if (!nt || res == NormalizationResult::HitLimits)
                result.normalizationTooComplex = true;
            else if (res == NormalizationResult::False)
            {
                /* If the normalized upper bound we're mapping to a generic is
                 * uninhabited, then we must consider the subtyping relation not to
                 * hold.
                 *
                 * This happens eg in <T>() -> (T, T) <: () -> (string, number)
                 *
                 * T appears in covariant position and would have to be both string
                 * and number at once.
                 *
                 * No actual value is both a string and a number, so the test fails.
                 *
                 * TODO: We'll need to add explanitory context here.
                 */
                result.isSubtype = false;
            }

            SubtypingEnvironment boundsEnv;
            boundsEnv.parent = &env;
            SubtypingResult boundsResult = isCovariantWith(boundsEnv, lowerBound, upperBound, scope);
            boundsResult.reasoning.clear();

            result.andAlso(boundsResult);
        }
    }

    /* TODO: We presently don't store subtype test results in the persistent
     * cache if the left-side type is a generic function.
     *
     * The implementation would be a bit tricky and we haven't seen any material
     * impact on benchmarks.
     *
     * What we would want to do is to remember points within the type where
     * mapped generics are introduced.  When all the contingent generics are
     * introduced at which we're doing the test, we can mark the result as
     * cacheable.
     */

    if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3 && !FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
        result.mappedGenericPacks_DEPRECATED = std::move(env.mappedGenericPacks_DEPRECATED);

    if (result.isCacheable)
        resultCache[{subTy, superTy}] = result;

    return result;
}
SubtypingResult Subtyping::isSubtype(TypePackId subTp, TypePackId superTp, NotNull<Scope> scope, const std::vector<TypeId>& bindableGenerics)
{
    LUAU_ASSERT(FFlag::LuauPassBindableGenericsByReference);

    SubtypingEnvironment env;
    if (FFlag::LuauSubtypingGenericsDoesntUseVariance)
    {
        for (TypeId g : bindableGenerics)
            env.mappedGenerics[follow(g)] = {SubtypingEnvironment::GenericBounds{}};
    }

    SubtypingResult result = isCovariantWith(env, subTp, superTp, scope);

    if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3 && !FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
    {
        if (!env.mappedGenericPacks_DEPRECATED.empty())
            result.mappedGenericPacks_DEPRECATED = std::move(env.mappedGenericPacks_DEPRECATED);
    }

    if (FFlag::LuauSubtypingGenericsDoesntUseVariance)
    {
        for (TypeId bg : bindableGenerics)
        {
            bg = follow(bg);

            LUAU_ASSERT(env.mappedGenerics.contains(bg));

            if (const std::vector<SubtypingEnvironment::GenericBounds>* bounds = env.mappedGenerics.find(bg))
            {
                // Bounds should have exactly one entry
                LUAU_ASSERT(bounds->size() == 1);
                if (FFlag::LuauSubtypingReportGenericBoundMismatches2)
                {
                    if (bounds->empty())
                        continue;
                    if (const GenericType* gen = get<GenericType>(bg))
                        result.andAlso(checkGenericBounds(bounds->back(), env, scope, gen->name));
                }
                else if (!bounds->empty())
                    result.andAlso(checkGenericBounds_DEPRECATED(bounds->back(), env, scope));
            }
        }
    }

    return result;
}

SubtypingResult Subtyping::isSubtype_DEPRECATED(
    TypePackId subTp,
    TypePackId superTp,
    NotNull<Scope> scope,
    std::optional<std::vector<TypeId>> bindableGenerics
)
{
    LUAU_ASSERT(!FFlag::LuauPassBindableGenericsByReference);

    SubtypingEnvironment env;
    if (FFlag::LuauSubtypingGenericsDoesntUseVariance && bindableGenerics)
    {
        for (TypeId g : *bindableGenerics)
            env.mappedGenerics[follow(g)] = {SubtypingEnvironment::GenericBounds{}};
    }

    SubtypingResult result = isCovariantWith(env, subTp, superTp, scope);

    if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3 && !FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
    {
        if (!env.mappedGenericPacks_DEPRECATED.empty())
            result.mappedGenericPacks_DEPRECATED = std::move(env.mappedGenericPacks_DEPRECATED);
    }

    if (FFlag::LuauSubtypingGenericsDoesntUseVariance && bindableGenerics)
    {
        for (TypeId bg : *bindableGenerics)
        {
            bg = follow(bg);

            LUAU_ASSERT(env.mappedGenerics.contains(bg));

            if (const std::vector<SubtypingEnvironment::GenericBounds>* bounds = env.mappedGenerics.find(bg))
            {
                // Bounds should have exactly one entry
                LUAU_ASSERT(bounds->size() == 1);
                if (FFlag::LuauSubtypingReportGenericBoundMismatches2)
                {
                    if (bounds->empty())
                        continue;
                    if (const GenericType* gen = get<GenericType>(bg))
                        result.andAlso(checkGenericBounds(bounds->back(), env, scope, gen->name));
                }
                else if (!bounds->empty())
                    result.andAlso(checkGenericBounds_DEPRECATED(bounds->back(), env, scope));
            }
        }
    }

    return result;
}

SubtypingResult Subtyping::cache(SubtypingEnvironment& env, SubtypingResult result, TypeId subTy, TypeId superTy)
{
    const std::pair<TypeId, TypeId> p{subTy, superTy};

    if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3 && !FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
        result.mappedGenericPacks_DEPRECATED = env.mappedGenericPacks_DEPRECATED;

    if (result.isCacheable)
        resultCache[p] = result;
    else if (!FFlag::LuauSubtypingGenericsDoesntUseVariance)
        env.ephemeralCache[p] = result;

    return result;
}

namespace
{
struct SeenSetPopper
{
    Subtyping::SeenSet* seenTypes;
    std::pair<TypeId, TypeId> pair;

    SeenSetPopper(Subtyping::SeenSet* seenTypes, std::pair<TypeId, TypeId> pair)
        : seenTypes(seenTypes)
        , pair(pair)
    {
    }

    ~SeenSetPopper()
    {
        seenTypes->erase(pair);
    }
};

struct SeenTypePackSetPopper
{
    Subtyping::SeenTypePackSet* seenTypes;
    std::pair<TypePackId, TypePackId> pair;

    SeenTypePackSetPopper(Subtyping::SeenTypePackSet* seenTypes, std::pair<TypePackId, TypePackId> pair)
        : seenTypes(seenTypes)
        , pair(std::move(pair))
    {
        LUAU_ASSERT(FFlag::LuauReturnMappedGenericPacksFromSubtyping3);
    }

    SeenTypePackSetPopper(const SeenTypePackSetPopper&) = delete;
    SeenTypePackSetPopper& operator=(const SeenTypePackSetPopper&) = delete;
    SeenTypePackSetPopper(SeenTypePackSetPopper&&) = delete;
    SeenTypePackSetPopper& operator=(SeenTypePackSetPopper&&) = delete;

    ~SeenTypePackSetPopper()
    {
        LUAU_ASSERT(FFlag::LuauReturnMappedGenericPacksFromSubtyping3);
        seenTypes->erase(pair);
    }
};
} // namespace

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, TypeId subTy, TypeId superTy, NotNull<Scope> scope)
{
    UnifierCounters& counters = normalizer->sharedState->counters;
    RecursionCounter rc(&counters.recursionCount);
    if (FFlag::LuauIndividualRecursionLimits)
    {
        if (DFInt::LuauSubtypingRecursionLimit > 0 && DFInt::LuauSubtypingRecursionLimit < counters.recursionCount)
            return SubtypingResult{false, true};
    }
    else
    {
        if (counters.recursionLimit > 0 && counters.recursionLimit < counters.recursionCount)
            return SubtypingResult{false, true};
    }

    subTy = follow(subTy);
    superTy = follow(superTy);

    if (const TypeId* subIt = env.tryFindSubstitution(subTy); subIt && *subIt)
        subTy = *subIt;

    if (const TypeId* superIt = env.tryFindSubstitution(superTy); superIt && *superIt)
        superTy = *superIt;

    const SubtypingResult* cachedResult = resultCache.find({subTy, superTy});
    if (cachedResult)
    {
        if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3 && !FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
        {
            for (const auto& [genericTp, boundTp] : cachedResult->mappedGenericPacks_DEPRECATED)
                env.mappedGenericPacks_DEPRECATED.try_insert(genericTp, boundTp);
        }

        return *cachedResult;
    }

    cachedResult = env.tryFindSubtypingResult({subTy, superTy});
    if (cachedResult)
    {
        if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3 && !FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
        {
            for (const auto& [genericTp, boundTp] : cachedResult->mappedGenericPacks_DEPRECATED)
                env.mappedGenericPacks_DEPRECATED.try_insert(genericTp, boundTp);
        }

        return *cachedResult;
    }

    // TODO: Do we care about returning a proof that this is error-suppressing?
    // e.g. given `a | error <: a | error` where both operands are pointer equal,
    // then should it also carry the information that it's error-suppressing?
    // If it should, then `error <: error` should also do the same.
    if (subTy == superTy)
        return {true};

    std::pair<TypeId, TypeId> typePair{subTy, superTy};
    if (!seenTypes.insert(typePair))
    {
        /* TODO: Caching results for recursive types is really tricky to think
         * about.
         *
         * We'd like to cache at the outermost level where we encounter the
         * recursive type, but we do not want to cache interior results that
         * involve the cycle.
         *
         * Presently, we stop at cycles and assume that the subtype check will
         * succeed because we'll eventually get there if it won't. However, if
         * that cyclic type turns out not to have the asked-for subtyping
         * relation, then all the intermediate cached results that were
         * contingent on that assumption need to be evicted from the cache, or
         * not entered into the cache, or something.
         *
         * For now, we do the conservative thing and refuse to cache anything
         * that touches a cycle.
         */
        SubtypingResult res;
        res.isSubtype = true;
        res.isCacheable = false;

        if (FFlag::LuauSubtypingGenericsDoesntUseVariance)
            env.seenSetCache[typePair] = res;

        return res;
    }

    SeenSetPopper ssp{&seenTypes, typePair};

    // Within the scope to which a generic belongs, that generic should be
    // tested as though it were its upper bounds.  We do not yet support bounded
    // generics, so the upper bound is always unknown.
    if (auto subGeneric = get<GenericType>(subTy); subGeneric && subsumes(subGeneric->scope, scope))
        return isCovariantWith(env, builtinTypes->neverType, superTy, scope);
    if (auto superGeneric = get<GenericType>(superTy); superGeneric && subsumes(superGeneric->scope, scope))
        return isCovariantWith(env, subTy, builtinTypes->unknownType, scope);

    SubtypingResult result;

    if (auto subUnion = get<UnionType>(subTy); subUnion && !FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds)
        result = isCovariantWith(env, subUnion, superTy, scope);
    else if (auto superUnion = get<UnionType>(superTy); superUnion && !FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds)
    {
        result = isCovariantWith(env, subTy, superUnion, scope);
        if (!result.isSubtype && !result.normalizationTooComplex)
            result = trySemanticSubtyping(env, subTy, superTy, scope, result);
    }
    else if (auto superIntersection = get<IntersectionType>(superTy); superIntersection && !FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds)
        result = isCovariantWith(env, subTy, superIntersection, scope);
    else if (auto subIntersection = get<IntersectionType>(subTy); subIntersection && !FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds)
    {
        result = isCovariantWith(env, subIntersection, superTy, scope);
        if (!result.isSubtype && !result.normalizationTooComplex)
            result = trySemanticSubtyping(env, subTy, superTy, scope, result);
    }
    else if (get<AnyType>(superTy))
        result = {true};

    // We have added this as an exception - the set of inhabitants of any is exactly the set of inhabitants of unknown (since error has no
    // inhabitants). any = err | unknown, so under semantic subtyping, {} U unknown = unknown
    else if (get<AnyType>(subTy) && get<UnknownType>(superTy))
        result = {true};
    else if (get<AnyType>(subTy))
    {
        // any = unknown | error, so we rewrite this to match.
        // As per TAPL: A | B <: T iff A <: T && B <: T
        result =
            isCovariantWith(env, builtinTypes->unknownType, superTy, scope).andAlso(isCovariantWith(env, builtinTypes->errorType, superTy, scope));
    }
    else if (get<UnknownType>(superTy) && // flag delays recursing into unions and inters, so only handle this case if subTy isn't a union or inter
             (FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds ? !get<UnionType>(subTy) && !get<IntersectionType>(subTy) : true))
    {
        LUAU_ASSERT(!get<AnyType>(subTy));          // TODO: replace with ice.
        LUAU_ASSERT(!get<UnionType>(subTy));        // TODO: replace with ice.
        LUAU_ASSERT(!get<IntersectionType>(subTy)); // TODO: replace with ice.

        bool errorSuppressing = get<ErrorType>(subTy);
        result = {!errorSuppressing};
    }
    else if (get<NeverType>(subTy))
        result = {true};
    else if (get<ErrorType>(superTy))
        result = {false};
    else if (get<ErrorType>(subTy))
        result = {true};
    else if (auto subTypeFunctionInstance = get<TypeFunctionInstanceType>(subTy);
             subTypeFunctionInstance && FFlag::LuauSubtypingGenericsDoesntUseVariance)
    {
        bool mappedGenericsApplied = false;
        if (auto substSubTy = env.applyMappedGenerics(builtinTypes, arena, subTy, iceReporter))
        {
            mappedGenericsApplied = *substSubTy != subTy;
            subTypeFunctionInstance = get<TypeFunctionInstanceType>(*substSubTy);
        }

        result = isCovariantWith(env, subTypeFunctionInstance, superTy, scope);
        result.isCacheable = !mappedGenericsApplied;
    }
    else if (auto superTypeFunctionInstance = get<TypeFunctionInstanceType>(superTy);
             superTypeFunctionInstance && FFlag::LuauSubtypingGenericsDoesntUseVariance)
    {
        bool mappedGenericsApplied = false;
        if (auto substSuperTy = env.applyMappedGenerics(builtinTypes, arena, superTy, iceReporter))
        {
            mappedGenericsApplied = *substSuperTy != superTy;
            superTypeFunctionInstance = get<TypeFunctionInstanceType>(*substSuperTy);
        }

        result = isCovariantWith(env, subTy, superTypeFunctionInstance, scope);
        result.isCacheable = !mappedGenericsApplied;
    }
    else if (FFlag::LuauSubtypingGenericsDoesntUseVariance && (get<GenericType>(subTy) || get<GenericType>(superTy)))
    {
        if (const auto subBounds = env.mappedGenerics.find(subTy); subBounds && !subBounds->empty())
        {
            bool ok = bindGeneric(env, subTy, superTy);
            result.isSubtype = ok;
            result.isCacheable = false;
        }
        else if (const auto superBounds = env.mappedGenerics.find(superTy); superBounds && !superBounds->empty())
        {
            bool ok = bindGeneric(env, subTy, superTy);
            result.isSubtype = ok;
            result.isCacheable = false;
        }
    }
    else if (auto subUnion = get<UnionType>(subTy))
    {
        LUAU_ASSERT(FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds);
        result = isCovariantWith(env, subUnion, superTy, scope);
    }
    else if (auto superUnion = get<UnionType>(superTy))
    {
        LUAU_ASSERT(FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds);
        result = isCovariantWith(env, subTy, superUnion, scope);
        if (!result.isSubtype && !result.normalizationTooComplex)
            result = trySemanticSubtyping(env, subTy, superTy, scope, result);
    }
    else if (auto superIntersection = get<IntersectionType>(superTy))
    {
        LUAU_ASSERT(FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds);
        result = isCovariantWith(env, subTy, superIntersection, scope);
    }
    else if (auto subIntersection = get<IntersectionType>(subTy))
    {
        LUAU_ASSERT(FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds);
        result = isCovariantWith(env, subIntersection, superTy, scope);
        if (!result.isSubtype && !result.normalizationTooComplex)
            result = trySemanticSubtyping(env, subTy, superTy, scope, result);
    }
    else if (!FFlag::LuauSubtypingGenericsDoesntUseVariance && get<GenericType>(subTy) && variance == Variance::Covariant)
    {
        bool ok = bindGeneric(env, subTy, superTy);
        result.isSubtype = ok;
        result.isCacheable = false;
    }
    else if (!FFlag::LuauSubtypingGenericsDoesntUseVariance && get<GenericType>(superTy) && variance == Variance::Contravariant)
    {
        bool ok = bindGeneric(env, subTy, superTy);
        result.isSubtype = ok;
        result.isCacheable = false;
    }
    else if (auto pair = get2<FreeType, FreeType>(subTy, superTy))
    {
        // Any two free types are potentially subtypes of one another because
        // both of them could be narrowed to never.
        result = {true};
        result.assumedConstraints.emplace_back(SubtypeConstraint{subTy, superTy});
    }
    else if (auto superFree = get<FreeType>(superTy))
    {
        // Given SubTy <: (LB <: SuperTy <: UB)
        //
        // If SubTy <: UB, then it is possible that SubTy <: SuperTy.
        // If SubTy </: UB, then it is definitely the case that SubTy </: SuperTy.
        //
        // It's always possible for SuperTy's upper bound to later be
        // constrained, so this relation may not actually hold.

        result = isCovariantWith(env, subTy, superFree->upperBound, scope);

        if (result.isSubtype)
            result.assumedConstraints.emplace_back(SubtypeConstraint{subTy, superTy});
    }
    else if (auto subFree = get<FreeType>(subTy))
    {
        // Given (LB <: SubTy <: UB) <: SuperTy
        //
        // If UB <: SuperTy, then it is certainly the case that SubTy <: SuperTy.
        // If SuperTy <: UB and LB <: SuperTy, then it is possible that UB will later be narrowed such that SubTy <: SuperTy.
        // If LB </: SuperTy, then SubTy </: SuperTy

        if (isCovariantWith(env, subFree->lowerBound, superTy, scope).isSubtype)
        {
            result = {true};
            result.assumedConstraints.emplace_back(SubtypeConstraint{subTy, superTy});
        }
        else
            result = {false};
    }
    else if (auto p = get2<NegationType, NegationType>(subTy, superTy))
        result = isCovariantWith(env, p.first->ty, p.second->ty, scope).withBothComponent(TypePath::TypeField::Negated);
    else if (auto subNegation = get<NegationType>(subTy))
    {
        result = isCovariantWith(env, subNegation, superTy, scope);
        if (!result.isSubtype && !result.normalizationTooComplex)
        {
            if (FFlag::LuauSubtypingNegationsChecksNormalizationComplexity)
                result = trySemanticSubtyping(env, subTy, superTy, scope, result);
            else
            {
                SubtypingResult semantic = isCovariantWith(env, normalizer->normalize(subTy), normalizer->normalize(superTy), scope);
                if (semantic.isSubtype)
                {
                    semantic.reasoning.clear();
                    result = semantic;
                }
            }
        }
    }
    else if (auto superNegation = get<NegationType>(superTy))
    {
        result = isCovariantWith(env, subTy, superNegation, scope);
        if (!result.isSubtype && !result.normalizationTooComplex)
        {
            if (FFlag::LuauSubtypingNegationsChecksNormalizationComplexity)
                result = trySemanticSubtyping(env, subTy, superTy, scope, result);
            else
            {
                SubtypingResult semantic = isCovariantWith(env, normalizer->normalize(subTy), normalizer->normalize(superTy), scope);
                if (semantic.isSubtype)
                {
                    semantic.reasoning.clear();
                    result = semantic;
                }
            }
        }
    }
    else if (auto subTypeFunctionInstance = get<TypeFunctionInstanceType>(subTy))
    {
        LUAU_ASSERT(!FFlag::LuauSubtypingGenericsDoesntUseVariance);
        if (auto substSubTy = env.applyMappedGenerics_DEPRECATED(builtinTypes, arena, subTy))
            subTypeFunctionInstance = get<TypeFunctionInstanceType>(*substSubTy);

        result = isCovariantWith(env, subTypeFunctionInstance, superTy, scope);
    }
    else if (auto superTypeFunctionInstance = get<TypeFunctionInstanceType>(superTy))
    {
        LUAU_ASSERT(!FFlag::LuauSubtypingGenericsDoesntUseVariance);
        if (auto substSuperTy = env.applyMappedGenerics_DEPRECATED(builtinTypes, arena, superTy))
            superTypeFunctionInstance = get<TypeFunctionInstanceType>(*substSuperTy);

        result = isCovariantWith(env, subTy, superTypeFunctionInstance, scope);
    }
    else if (auto p = get2<PrimitiveType, PrimitiveType>(subTy, superTy))
        result = isCovariantWith(env, p, scope);
    else if (auto p = get2<SingletonType, PrimitiveType>(subTy, superTy))
        result = isCovariantWith(env, p, scope);
    else if (auto p = get2<SingletonType, SingletonType>(subTy, superTy))
        result = isCovariantWith(env, p, scope);
    else if (auto p = get2<FunctionType, PrimitiveType>(subTy, superTy))
    {
        auto [subFunction, superPrimitive] = p;
        result.isSubtype = superPrimitive->type == PrimitiveType::Function;
    }
    else if (auto p = get2<FunctionType, FunctionType>(subTy, superTy))
        result = isCovariantWith(env, p, scope);
    else if (auto p = get2<TableType, TableType>(subTy, superTy))
    {
        if (FFlag::LuauTrackUniqueness)
        {
            const bool forceCovariantTest = uniqueTypes != nullptr && uniqueTypes->contains(subTy);
            result = isCovariantWith(env, p.first, p.second, forceCovariantTest, scope);
        }
        else
            result = isCovariantWith(env, p, scope);
    }
    else if (auto p = get2<MetatableType, MetatableType>(subTy, superTy))
        result = isCovariantWith(env, p, scope);
    else if (auto p = get2<MetatableType, TableType>(subTy, superTy))
        result = isCovariantWith(env, p, scope);
    else if (auto p = get2<ExternType, ExternType>(subTy, superTy))
        result = isCovariantWith(env, p, scope);
    else if (auto p = get2<ExternType, TableType>(subTy, superTy))
        result = isCovariantWith(env, subTy, p.first, superTy, p.second, scope);
    else if (auto p = get2<TableType, PrimitiveType>(subTy, superTy))
        result = isCovariantWith(env, p, scope);
    else if (auto p = get2<PrimitiveType, TableType>(subTy, superTy))
        result = isCovariantWith(env, p, scope);
    else if (auto p = get2<SingletonType, TableType>(subTy, superTy))
        result = isCovariantWith(env, p, scope);

    if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
        assertReasoningValid(subTy, superTy, result, builtinTypes, arena);
    else
        assertReasoningValid_DEPRECATED(subTy, superTy, result, builtinTypes);

    return cache(env, std::move(result), subTy, superTy);
}

/*
 * Subtyping of packs is fairly involved. There are three parts to the test.
 *
 * 1. If both packs have types at their heads, we do a pairwise test for each
 *    pair of types.
 * 2. If the finite parts of the packs are of inequal length and the pack on the
 *    opposite side has a tail, we test that. (eg
 *    testing concrete types against variadics or a generic pack)
 * 3. Lastly, do a subtype test on non-finite tails. (eg between two generic
 *    packs or variadics)
 */
SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, TypePackId subTp, TypePackId superTp, NotNull<Scope> scope)
{
    UnifierCounters& counters = normalizer->sharedState->counters;
    std::optional<RecursionCounter> rc;

    if (FFlag::LuauSubtypingPackRecursionLimits)
    {
        rc.emplace(&counters.recursionCount);

        if (FFlag::LuauIndividualRecursionLimits)
        {
            if (DFInt::LuauSubtypingRecursionLimit > 0 && counters.recursionCount > DFInt::LuauSubtypingRecursionLimit)
                return SubtypingResult{false, true};
        }
        else
        {
            if (counters.recursionLimit > 0 && counters.recursionLimit < counters.recursionCount)
                return SubtypingResult{false, true};
        }
    }

    subTp = follow(subTp);
    superTp = follow(superTp);

    std::optional<SeenTypePackSetPopper> popper = std::nullopt;
    if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
    {
        std::pair<TypePackId, TypePackId> typePair = {subTp, superTp};
        if (!seenPacks.insert(typePair))
            return SubtypingResult{true, false, false};
        popper.emplace(&seenPacks, std::move(typePair));
    }

    auto [subHead, subTail] = flatten(subTp);
    auto [superHead, superTail] = flatten(superTp);

    const size_t headSize = std::min(subHead.size(), superHead.size());

    std::vector<SubtypingResult> results;
    results.reserve(std::max(subHead.size(), superHead.size()) + 1);

    if (subTp == superTp)
        return {true};

    // Match head types pairwise

    for (size_t i = 0; i < headSize; ++i)
        results.push_back(
            isCovariantWith(env, subHead[i], superHead[i], scope).withBothComponent(TypePath::Index{i, TypePath::Index::Variant::Pack})
        );

    // Handle mismatched head sizes

    if (subHead.size() < superHead.size())
    {
        if (subTail)
        {
            std::optional<SubtypingResult> sr = isSubTailCovariantWith(env, results, subTp, *subTail, superTp, headSize, superHead, superTail, scope);
            if (sr)
                return *sr;
        }
        else
        {
            results.push_back({false});
            return SubtypingResult::all(results);
        }
    }
    else if (subHead.size() > superHead.size())
    {
        if (superTail)
        {
            std::optional<SubtypingResult> sr = isCovariantWithSuperTail(env, results, subTp, headSize, subHead, subTail, superTp, *superTail, scope);
            if (sr)
                return *sr;
        }
        else
            return {false};
    }

    // Handle tails

    if (subTail && superTail)
    {
        if (auto p = get2<VariadicTypePack, VariadicTypePack>(*subTail, *superTail))
        {
            // Variadic component is added by the isCovariantWith
            // implementation; no need to add it here.
            results.push_back(isCovariantWith(env, p, scope).withBothComponent(TypePath::PackField::Tail));
        }
        else if (get2<GenericTypePack, GenericTypePack>(*subTail, *superTail))
        {
            if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
            {
                MappedGenericEnvironment::LookupResult subLookupResult = env.mappedGenericPacks.lookupGenericPack(*subTail);
                MappedGenericEnvironment::LookupResult superLookupResult = env.mappedGenericPacks.lookupGenericPack(*superTail);

                // match (subLookup, superLookupResult) {
                //     (TypePackId, _) => do covariant test
                //     (Unmapped, _) => bind the generic
                //     (_, TypePackId) => do covariant test
                //     (_, Unmapped) => bind the generic
                //     (_, _) => subtyping succeeds if the two generics are pointer-identical
                // }
                if (const TypePackId* currMapping = get_if<TypePackId>(&subLookupResult))
                {
                    results.push_back(isCovariantWith(env, *currMapping, *superTail, scope)
                                          .withSubPath(Path({TypePath::PackField::Tail, TypePath::GenericPackMapping{*currMapping}}))
                                          .withSuperComponent(TypePath::PackField::Tail));
                }
                else if (get_if<MappedGenericEnvironment::Unmapped>(&subLookupResult))
                {
                    bool ok = env.mappedGenericPacks.bindGeneric(*subTail, *superTail);
                    results.push_back(
                        SubtypingResult{ok, /* normalizationTooComplex */ false, /* isCacheable */ false}.withBothComponent(TypePath::PackField::Tail)
                    );
                }
                else if (const TypePackId* currMapping = get_if<TypePackId>(&superLookupResult))
                {
                    results.push_back(isCovariantWith(env, *subTail, *currMapping, scope)
                                          .withSubComponent(TypePath::PackField::Tail)
                                          .withSuperPath(Path({TypePath::PackField::Tail, TypePath::GenericPackMapping{*currMapping}})));
                }
                else if (get_if<MappedGenericEnvironment::Unmapped>(&superLookupResult))
                {
                    bool ok = env.mappedGenericPacks.bindGeneric(*superTail, *subTail);
                    results.push_back(
                        SubtypingResult{ok, /* normalizationTooComplex */ false, /* isCacheable */ false}.withBothComponent(TypePath::PackField::Tail)
                    );
                }
                else
                {
                    // Sometimes, we compare generic packs inside the functions which are quantifying them. They're not bindable, but should still
                    // subtype against themselves.
                    results.push_back(
                        SubtypingResult{*subTail == *superTail, /* normalizationTooComplex */ false, /* isCacheable */ false}.withBothComponent(
                            TypePath::PackField::Tail
                        )
                    );
                }
            }
            else
            {
                bool ok = bindGeneric_DEPRECATED(env, *subTail, *superTail);
                results.push_back(SubtypingResult{ok}.withBothComponent(TypePath::PackField::Tail));
            }
        }
        else if (get2<VariadicTypePack, GenericTypePack>(*subTail, *superTail))
        {
            if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
            {
                MappedGenericEnvironment::LookupResult lookupResult = env.mappedGenericPacks.lookupGenericPack(*superTail);
                if (const TypePackId* currMapping = get_if<TypePackId>(&lookupResult))
                {
                    results.push_back(isCovariantWith(env, *subTail, *currMapping, scope)
                                          .withSubComponent(TypePath::PackField::Tail)
                                          .withSuperPath(Path({TypePath::PackField::Tail, TypePath::GenericPackMapping{*currMapping}})));
                }
                else if (get_if<MappedGenericEnvironment::Unmapped>(&lookupResult))
                {
                    bool ok = env.mappedGenericPacks.bindGeneric(*superTail, *subTail);
                    results.push_back(
                        SubtypingResult{ok, /* normalizationTooComplex */ false, /* isCacheable */ false}.withBothComponent(TypePath::PackField::Tail)
                    );
                }
                else
                {
                    LUAU_ASSERT(get_if<MappedGenericEnvironment::NotBindable>(&lookupResult));
                    results.push_back(
                        SubtypingResult{false, /* normalizationTooComplex */ false, /* isCacheable */ false}.withBothComponent(
                            TypePath::PackField::Tail
                        )
                    );
                }
            }
            else if (variance == Variance::Contravariant)
            {
                // <A...>(A...) -> number <: (...number) -> number
                bool ok = bindGeneric_DEPRECATED(env, *subTail, *superTail);

                results.push_back(SubtypingResult{ok}.withBothComponent(TypePath::PackField::Tail));
            }
            else
            {
                // (number) -> ...number </: <A...>(number) -> A...
                results.push_back(SubtypingResult{false}.withBothComponent(TypePath::PackField::Tail));
            }
        }
        else if (auto p = get2<GenericTypePack, VariadicTypePack>(*subTail, *superTail))
        {
            if (TypeId t = follow(p.second->ty); get<AnyType>(t) || get<UnknownType>(t))
            {
                // Extra magic rule:
                // T... <: ...any
                // T... <: ...unknown
                //
                // See https://github.com/luau-lang/luau/issues/767
            }
            else if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
            {
                MappedGenericEnvironment::LookupResult lookupResult = env.mappedGenericPacks.lookupGenericPack(*subTail);
                if (const TypePackId* currMapping = get_if<TypePackId>(&lookupResult))
                {
                    results.push_back(isCovariantWith(env, *currMapping, *superTail, scope)
                                          .withSubPath(Path({TypePath::PackField::Tail, TypePath::GenericPackMapping{*currMapping}}))
                                          .withSuperComponent(TypePath::PackField::Tail));
                }
                else if (get_if<MappedGenericEnvironment::Unmapped>(&lookupResult))
                {
                    bool ok = env.mappedGenericPacks.bindGeneric(*subTail, *superTail);
                    results.push_back(
                        SubtypingResult{ok, /* normalizationTooComplex */ false, /* isCacheable */ false}.withBothComponent(TypePath::PackField::Tail)
                    );
                }
                else
                {
                    LUAU_ASSERT(get_if<MappedGenericEnvironment::NotBindable>(&lookupResult));
                    results.push_back(
                        SubtypingResult{false, /* normalizationTooComplex */ false, /* isCacheable */ false}.withBothComponent(
                            TypePath::PackField::Tail
                        )
                    );
                }
            }
            else if (variance == Variance::Contravariant)
            {
                // (...number) -> number </: <A...>(A...) -> number
                results.push_back(SubtypingResult{false}.withBothComponent(TypePath::PackField::Tail));
            }
            else
            {
                // <A...>() -> A... <: () -> ...number
                bool ok = bindGeneric_DEPRECATED(env, *subTail, *superTail);
                results.push_back(SubtypingResult{ok}.withBothComponent(TypePath::PackField::Tail));
            }
        }
        else if (get<ErrorTypePack>(*subTail) || get<ErrorTypePack>(*superTail))
            // error type is fine on either side
            results.push_back(SubtypingResult{true}.withBothComponent(TypePath::PackField::Tail));
        else
            return SubtypingResult{false}
                .withBothComponent(TypePath::PackField::Tail)
                .withError({scope->location, UnexpectedTypePackInSubtyping{*subTail}})
                .withError({scope->location, UnexpectedTypePackInSubtyping{*superTail}});
    }
    else if (subTail)
    {
        if (get<VariadicTypePack>(*subTail))
        {
            return SubtypingResult{false}.withSubComponent(TypePath::PackField::Tail);
        }
        else if (get<GenericTypePack>(*subTail))
        {
            if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
            {
                MappedGenericEnvironment::LookupResult lookupResult = env.mappedGenericPacks.lookupGenericPack(*subTail);
                if (const TypePackId* currMapping = get_if<TypePackId>(&lookupResult))
                    results.push_back(isCovariantWith(env, *currMapping, builtinTypes->emptyTypePack, scope)
                                          .withSubPath(Path({TypePath::PackField::Tail, TypePath::GenericPackMapping{*currMapping}})));
                else if (get_if<MappedGenericEnvironment::Unmapped>(&lookupResult))
                {
                    bool ok = env.mappedGenericPacks.bindGeneric(*subTail, builtinTypes->emptyTypePack);
                    results.push_back(
                        SubtypingResult{ok, /* normalizationTooComplex */ false, /* isCacheable */ false}.withSubComponent(TypePath::PackField::Tail)
                    );
                }
                else
                {
                    LUAU_ASSERT(get_if<MappedGenericEnvironment::NotBindable>(&lookupResult));
                    results.push_back(
                        SubtypingResult{false, /* normalizationTooComplex */ false, /* isCacheable */ false}.withSubComponent(
                            TypePath::PackField::Tail
                        )
                    );
                }
            }
            else
            {
                bool ok = bindGeneric_DEPRECATED(env, *subTail, builtinTypes->emptyTypePack);
                return SubtypingResult{ok}.withSubComponent(TypePath::PackField::Tail);
            }
        }
        else
            return SubtypingResult{false}
                .withSubComponent(TypePath::PackField::Tail)
                .withError({scope->location, UnexpectedTypePackInSubtyping{*subTail}});
    }
    else if (superTail)
    {
        if (get<VariadicTypePack>(*superTail))
        {
            /*
             * A variadic type pack ...T can be thought of as an infinite union of finite type packs.
             *     () | (T) | (T, T) | (T, T, T) | ...
             *
             * And, per TAPL:
             *     T <: A | B iff T <: A or T <: B
             *
             * All variadic type packs are therefore supertypes of the empty type pack.
             */
        }
        else if (get<GenericTypePack>(*superTail))
        {
            if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
            {
                MappedGenericEnvironment::LookupResult lookupResult = env.mappedGenericPacks.lookupGenericPack(*superTail);
                if (const TypePackId* currMapping = get_if<TypePackId>(&lookupResult))
                    results.push_back(isCovariantWith(env, builtinTypes->emptyTypePack, *currMapping, scope)
                                          .withSuperPath(Path({TypePath::PackField::Tail, TypePath::GenericPackMapping{*currMapping}})));
                else if (get_if<MappedGenericEnvironment::Unmapped>(&lookupResult))
                {
                    bool ok = env.mappedGenericPacks.bindGeneric(*superTail, builtinTypes->emptyTypePack);
                    results.push_back(
                        SubtypingResult{ok, /* normalizationTooComplex */ false, /* isCacheable */ false}.withSuperComponent(
                            TypePath::PackField::Tail
                        )
                    );
                }
                else
                {
                    LUAU_ASSERT(get_if<MappedGenericEnvironment::NotBindable>(&lookupResult));
                    results.push_back(
                        SubtypingResult{false, /* normalizationTooComplex */ false, /* isCacheable */ false}.withSuperComponent(
                            TypePath::PackField::Tail
                        )
                    );
                }
            }
            else if (variance == Variance::Contravariant)
            {
                bool ok = bindGeneric_DEPRECATED(env, builtinTypes->emptyTypePack, *superTail);
                results.push_back(SubtypingResult{ok}.withSuperComponent(TypePath::PackField::Tail));
            }
            else
                results.push_back(SubtypingResult{false}.withSuperComponent(TypePath::PackField::Tail));
        }
        else
            return SubtypingResult{false}
                .withSuperComponent(TypePath::PackField::Tail)
                .withError({scope->location, UnexpectedTypePackInSubtyping{*superTail}});
    }

    SubtypingResult result = SubtypingResult::all(results);

    if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
        assertReasoningValid(subTp, superTp, result, builtinTypes, arena);
    else
        assertReasoningValid_DEPRECATED(subTp, superTp, result, builtinTypes);

    return result;
}

/* Check the tail of the subtype pack against a slice of the finite part of the
 * pack in supertype position. For example, in the following type, the head of
 * the pack in supertype position is longer than that of the subtype head:
 *
 *     (number, string, any...) <: (number, string, boolean, thread, any...),
 *
 * This function handles the mismatched heads: any... <: (boolean, thread)
 *
 * Notably, this function does _not_ handle the test between the actual tail
 * packs.
 *
 * The contract on this function is a bit strange. If the function returns a
 * SubtypingResult, it should be considered to be the result for the entire pack
 * subtyping relation.  It is not necessary to further check the tails.
 */
std::optional<SubtypingResult> Subtyping::isSubTailCovariantWith(
    SubtypingEnvironment& env,
    std::vector<SubtypingResult>& outputResults,
    TypePackId subTp,
    TypePackId subTail,
    TypePackId superTp,
    size_t superHeadStartIndex,
    const std::vector<TypeId>& superHead,
    std::optional<TypePackId> superTail,
    NotNull<Scope> scope
)
{
    if (auto vt = get<VariadicTypePack>(subTail))
    {
        for (size_t i = superHeadStartIndex; i < superHead.size(); ++i)
            outputResults.push_back(isCovariantWith(env, vt->ty, superHead[i], scope)
                                    .withSubPath(TypePath::PathBuilder().tail().variadic().build())
                                    .withSuperComponent(TypePath::Index{i, TypePath::Index::Variant::Pack}));
        return std::nullopt;
    }
    else if (get<GenericTypePack>(subTail))
    {
        if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
        {
            MappedGenericEnvironment::LookupResult lookupResult = env.mappedGenericPacks.lookupGenericPack(subTail);
            SubtypingResult result;
            if (get_if<MappedGenericEnvironment::NotBindable>(&lookupResult))
                result = SubtypingResult{false, /* normalizationTooComplex */ false, /* isCacheable */ false}
                                .withSubComponent(TypePath::PackField::Tail)
                                .withSuperComponent(TypePath::PackSlice{superHeadStartIndex});
            else
            {
                TypePackId superTailPack = sliceTypePack(superHeadStartIndex, superTp, superHead, superTail, builtinTypes, arena);

                if (const TypePackId* mappedGen = get_if<TypePackId>(&lookupResult))
                {
                    // Subtype against the mapped generic pack.
                    TypePackId subTpToCompare = *mappedGen;

                    // If mappedGen has a hidden variadic tail, we clip it for better arity mismatch reporting.
                    const TypePack* tp = get<TypePack>(*mappedGen);
                    if (const VariadicTypePack* vtp = tp ? get<VariadicTypePack>(follow(tp->tail)) : nullptr; vtp && vtp->hidden)
                        subTpToCompare = arena->addTypePack(tp->head);

                    result = isCovariantWith(env, subTpToCompare, superTailPack, scope)
                                    .withSubPath(Path({TypePath::PackField::Tail, TypePath::GenericPackMapping{*mappedGen}}))
                                    .withSuperComponent(TypePath::PackSlice{superHeadStartIndex});
                }
                else
                {
                    LUAU_ASSERT(get_if<MappedGenericEnvironment::Unmapped>(&lookupResult));
                    bool ok = env.mappedGenericPacks.bindGeneric(subTail, superTailPack);
                    result = SubtypingResult{ok, /* normalizationTooComplex */ false, /* isCacheable */ false}
                                    .withSubComponent(TypePath::PackField::Tail)
                                    .withSuperComponent(TypePath::PackSlice{superHeadStartIndex});
                }
            }

            outputResults.push_back(result);
            return SubtypingResult::all(outputResults);
        }
        else if (variance == Variance::Covariant)
        {
            // For any non-generic type T:
            //
            // <X>(X) -> () <: (T) -> ()

            TypePackId superTailPack;
            if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
            {
                if (superHeadStartIndex == 0)
                    superTailPack = superTp;
                else if (superHeadStartIndex == superHead.size())
                    superTailPack = superTail ? *superTail : builtinTypes->emptyTypePack;
                else
                {
                    auto superHeadIter = begin(superHead);
                    for (size_t i = 0; i < superHeadStartIndex; ++i)
                        ++superHeadIter;
                    std::vector<TypeId> headSlice(std::move(superHeadIter), end(superHead));
                    superTailPack = arena->addTypePack(std::move(headSlice), superTail);
                }
            }
            else
            {
                // Possible optimization: If headSize == 0 then we can just use subTp as-is.
                std::vector<TypeId> headSlice = std::vector<TypeId>(begin(superHead), begin(superHead) + int(superHeadStartIndex));
                superTailPack = arena->addTypePack(std::move(headSlice), superTail);
            }

            if (TypePackId* other = env.getMappedPackBounds_DEPRECATED(subTail))
            {
                if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
                {
                    const TypePack* tp = get<TypePack>(*other);
                    if (const VariadicTypePack* vtp = tp ? get<VariadicTypePack>(follow(tp->tail)) : nullptr; vtp && vtp->hidden)
                    {
                        TypePackId taillessTp = arena->addTypePack(tp->head);
                        outputResults.push_back(isCovariantWith(env, taillessTp, superTailPack, scope)
                                                .withSubComponent(TypePath::PackField::Tail)
                                                .withSuperComponent(TypePath::PackSlice{superHeadStartIndex}));
                    }
                    else
                        outputResults.push_back(isCovariantWith(env, *other, superTailPack, scope)
                                                .withSubComponent(TypePath::PackField::Tail)
                                                .withSuperComponent(TypePath::PackSlice{superHeadStartIndex}));
                }
                else
                    outputResults.push_back(isCovariantWith(env, *other, superTailPack, scope).withSubComponent(TypePath::PackField::Tail));
            }
            else
                env.mappedGenericPacks_DEPRECATED.try_insert(subTail, superTailPack);

            // FIXME? Not a fan of the early return here.  It makes the
            // control flow harder to reason about.
            return SubtypingResult::all(outputResults);
        }
        else
        {
            // For any non-generic type T:
            //
            // (T) -> () </: <X>(X) -> ()
            //
            return SubtypingResult{false}.withSubComponent(TypePath::PackField::Tail);
        }
    }
    else if (get<ErrorTypePack>(subTail))
        return SubtypingResult{true}.withSubComponent(TypePath::PackField::Tail);
    else
        return SubtypingResult{false}
            .withSubComponent(TypePath::PackField::Tail)
            .withError({scope->location, UnexpectedTypePackInSubtyping{subTail}});
}

std::optional<SubtypingResult> Subtyping::isCovariantWithSuperTail(
    SubtypingEnvironment& env,
    std::vector<SubtypingResult>& results,
    TypePackId subTp,
    size_t subHeadStartIndex,
    const std::vector<TypeId>& subHead,
    std::optional<TypePackId> subTail,
    TypePackId superTp,
    TypePackId superTail,
    NotNull<Scope> scope
)
{
    if (auto vt = get<VariadicTypePack>(superTail))
    {
        for (size_t i = subHeadStartIndex; i < subHead.size(); ++i)
            results.push_back(isCovariantWith(env, subHead[i], vt->ty, scope)
                                    .withSubComponent(TypePath::Index{i, TypePath::Index::Variant::Pack})
                                    .withSuperPath(TypePath::PathBuilder().tail().variadic().build()));
        return std::nullopt;
    }
    else if (get<GenericTypePack>(superTail))
    {
        if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
        {
            MappedGenericEnvironment::LookupResult lookupResult = env.mappedGenericPacks.lookupGenericPack(superTail);
            SubtypingResult result;
            if (get_if<MappedGenericEnvironment::NotBindable>(&lookupResult))
                result = SubtypingResult{false, /* normalizationTooComplex */ false, /* isCacheable */ false}
                                .withSubComponent(TypePath::PackSlice{subHeadStartIndex})
                                .withSuperComponent(TypePath::PackField::Tail);
            else
            {
                TypePackId subTailPack = sliceTypePack(subHeadStartIndex, subTp, subHead, subTail, builtinTypes, arena);

                if (const TypePackId* mappedGen = get_if<TypePackId>(&lookupResult))
                {
                    TypePackId superTpToCompare = *mappedGen;

                    // Subtype against the mapped generic pack.
                    const TypePack* tp = get<TypePack>(*mappedGen);
                    if (const VariadicTypePack* vtp = tp ? get<VariadicTypePack>(follow(tp->tail)) : nullptr; vtp && vtp->hidden)
                        superTpToCompare = arena->addTypePack(tp->head);

                    result = isCovariantWith(env, subTailPack, superTpToCompare, scope)
                                    .withSubComponent(TypePath::PackSlice{subHeadStartIndex})
                                    .withSuperPath(Path({TypePath::PackField::Tail, TypePath::GenericPackMapping{*mappedGen}}));
                }
                else
                {
                    LUAU_ASSERT(get_if<MappedGenericEnvironment::Unmapped>(&lookupResult));
                    bool ok = env.mappedGenericPacks.bindGeneric(superTail, subTailPack);
                    result = SubtypingResult{ok, /* normalizationTooComplex */ false, /* isCacheable */ false}
                                    .withSubComponent(TypePath::PackSlice{subHeadStartIndex})
                                    .withSuperComponent(TypePath::PackField::Tail);
                }
            }

            results.push_back(result);
            return SubtypingResult::all(results);
        }
        else if (variance == Variance::Contravariant)
        {
            // For any non-generic type T:
            //
            // <X...>(X...) -> () <: (T) -> ()

            TypePackId subTailPack;
            if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
            {
                if (subHeadStartIndex == 0)
                    subTailPack = subTp;
                else if (subHeadStartIndex == subHead.size())
                    subTailPack = subTail ? *subTail : builtinTypes->emptyTypePack;
                else
                {
                    auto subHeadIter = begin(subHead);
                    for (size_t i = 0; i < subHeadStartIndex; ++i)
                        ++subHeadIter;
                    std::vector<TypeId> headSlice(std::move(subHeadIter), end(subHead));
                    subTailPack = arena->addTypePack(std::move(headSlice), subTail);
                }
            }
            else
            {
                // Possible optimization: If headSize == 0 then we can just use subTp as-is.
                std::vector<TypeId> headSlice = std::vector<TypeId>(begin(subHead), begin(subHead) + int(subHeadStartIndex));
                subTailPack = arena->addTypePack(std::move(headSlice), subTail);
            }

            if (TypePackId* other = env.getMappedPackBounds_DEPRECATED(superTail))
            {
                if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
                {
                    const TypePack* tp = get<TypePack>(*other);
                    if (const VariadicTypePack* vtp = tp ? get<VariadicTypePack>(follow(tp->tail)) : nullptr; vtp && vtp->hidden)
                    {
                        TypePackId taillessTp = arena->addTypePack(tp->head);
                        results.push_back(isCovariantWith(env, subTailPack, taillessTp, scope)
                                                .withSubComponent(TypePath::PackSlice{subHeadStartIndex})
                                                .withSuperComponent(TypePath::PackField::Tail));
                    }
                    else
                        results.push_back(isCovariantWith(env, subTailPack, *other, scope)
                                                .withSubComponent(TypePath::PackSlice{subHeadStartIndex})
                                                .withSuperComponent(TypePath::PackField::Tail));
                }
                else
                    results.push_back(isContravariantWith(env, subTailPack, *other, scope).withSuperComponent(TypePath::PackField::Tail));
            }
            else
                env.mappedGenericPacks_DEPRECATED.try_insert(superTail, subTailPack);

            // FIXME? Not a fan of the early return here.  It makes the
            // control flow harder to reason about.
            return SubtypingResult::all(results);
        }
        else
        {
            // For any non-generic type T:
            //
            // () -> T </: <X...>() -> X...
            return SubtypingResult{false}.withSuperComponent(TypePath::PackField::Tail);
        }
    }
    else if (get<ErrorTypePack>(superTail))
        return SubtypingResult{true}.withSuperComponent(TypePath::PackField::Tail);
    else
        return SubtypingResult{false}
            .withSuperComponent(TypePath::PackField::Tail)
            .withError({scope->location, UnexpectedTypePackInSubtyping{superTail}});
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isContravariantWith(SubtypingEnvironment& env, SubTy&& subTy, SuperTy&& superTy, NotNull<Scope> scope)
{
    VarianceFlipper vf{&variance};

    SubtypingResult result = isCovariantWith(env, superTy, subTy, scope);
    if (result.reasoning.empty())
        result.reasoning.insert(SubtypingReasoning{TypePath::kEmpty, TypePath::kEmpty, SubtypingVariance::Contravariant});
    else
    {
        // If we don't swap the paths here, we will end up producing an invalid path
        // whenever we involve contravariance. We'll end up appending path
        // components that should belong to the supertype to the subtype, and vice
        // versa.
        for (auto& reasoning : result.reasoning)
        {
            std::swap(reasoning.subPath, reasoning.superPath);

            // Also swap covariant/contravariant, since those are also the other way
            // around.
            if (reasoning.variance == SubtypingVariance::Covariant)
                reasoning.variance = SubtypingVariance::Contravariant;
            else if (reasoning.variance == SubtypingVariance::Contravariant)
                reasoning.variance = SubtypingVariance::Covariant;
        }
    }

    if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
        assertReasoningValid(subTy, superTy, result, builtinTypes, arena);
    else
        assertReasoningValid_DEPRECATED(subTy, superTy, result, builtinTypes);

    return result;
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isInvariantWith(SubtypingEnvironment& env, SubTy&& subTy, SuperTy&& superTy, NotNull<Scope> scope)
{
    SubtypingResult result = isCovariantWith(env, subTy, superTy, scope);
    result.andAlso(isContravariantWith(env, subTy, superTy, scope));

    if (result.reasoning.empty())
        result.reasoning.insert(SubtypingReasoning{TypePath::kEmpty, TypePath::kEmpty, SubtypingVariance::Invariant});
    else
    {
        for (auto& reasoning : result.reasoning)
            reasoning.variance = SubtypingVariance::Invariant;
    }

    if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
        assertReasoningValid(subTy, superTy, result, builtinTypes, arena);
    else
        assertReasoningValid_DEPRECATED(subTy, superTy, result, builtinTypes);

    return result;
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair, NotNull<Scope> scope)
{
    return isCovariantWith(env, pair.first, pair.second, scope);
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isContravariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair, NotNull<Scope> scope)
{
    return isContravariantWith(env, pair.first, pair.second, scope);
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isInvariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair, NotNull<Scope> scope)
{
    return isInvariantWith(env, pair.first, pair.second);
}

/*
 * This is much simpler than the Unifier implementation because we don't
 * actually care about potential "cross-talk" between union parts that match the
 * left side.
 *
 * In fact, we're very limited in what we can do: If multiple choices match, but
 * all of them have non-overlapping constraints, then we're stuck with an "or"
 * conjunction of constraints.  Solving this in the general case is quite
 * difficult.
 *
 * For example, we cannot dispatch anything from this constraint:
 *
 * {x: number, y: string} <: {x: number, y: 'a} | {x: 'b, y: string}
 *
 * From this constraint, we can know that either string <: 'a or number <: 'b,
 * but we don't know which!
 *
 * However:
 *
 * {x: number, y: string} <: {x: number, y: 'a} | {x: number, y: string}
 *
 * We can dispatch this constraint because there is no 'or' conjunction.  One of
 * the arms requires 0 matches.
 *
 * {x: number, y: string, z: boolean} | {x: number, y: 'a, z: 'b} | {x: number,
 * y: string, z: 'b}
 *
 * Here, we have two matches.  One asks for string ~ 'a and boolean ~ 'b.  The
 * other just asks for boolean ~ 'b. We can dispatch this and only commit
 * boolean ~ 'b.  This constraint does not teach us anything about 'a.
 */
SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, TypeId subTy, const UnionType* superUnion, NotNull<Scope> scope)
{
    // As per TAPL: T <: A | B iff T <: A || T <: B

    for (TypeId ty : superUnion)
    {
        SubtypingResult next = isCovariantWith(env, subTy, ty, scope);

        if (next.normalizationTooComplex)
            return SubtypingResult{false, /* normalizationTooComplex */ true};

        if (next.isSubtype)
            return SubtypingResult{true};
    }

    /*
     * TODO: Is it possible here to use the context produced by the above
     * isCovariantWith() calls to produce a richer, more helpful result in the
     * case that the subtyping relation does not hold?
     */
    return SubtypingResult{false};
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const UnionType* subUnion, TypeId superTy, NotNull<Scope> scope)
{
    // As per TAPL: A | B <: T iff A <: T && B <: T
    std::vector<SubtypingResult> subtypings;
    size_t i = 0;
    for (TypeId ty : subUnion)
    {
        subtypings.push_back(isCovariantWith(env, ty, superTy, scope).withSubComponent(TypePath::Index{i++, TypePath::Index::Variant::Union}));

        if (subtypings.back().normalizationTooComplex)
            return SubtypingResult{false, /* normalizationTooComplex */ true};
    }

    return SubtypingResult::all(subtypings);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, TypeId subTy, const IntersectionType* superIntersection, NotNull<Scope> scope)
{
    // As per TAPL: T <: A & B iff T <: A && T <: B
    std::vector<SubtypingResult> subtypings;
    size_t i = 0;
    for (TypeId ty : superIntersection)
    {
        subtypings.push_back(isCovariantWith(env, subTy, ty, scope).withSuperComponent(TypePath::Index{i++, TypePath::Index::Variant::Intersection}));

        if (subtypings.back().normalizationTooComplex)
            return SubtypingResult{false, /* normalizationTooComplex */ true};
    }

    return SubtypingResult::all(subtypings);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const IntersectionType* subIntersection, TypeId superTy, NotNull<Scope> scope)
{
    // As per TAPL: A & B <: T iff A <: T || B <: T
    std::vector<SubtypingResult> subtypings;
    size_t i = 0;
    for (TypeId ty : subIntersection)
    {
        subtypings.push_back(isCovariantWith(env, ty, superTy, scope).withSubComponent(TypePath::Index{i++, TypePath::Index::Variant::Intersection}));

        if (subtypings.back().normalizationTooComplex)
            return SubtypingResult{false, /* normalizationTooComplex */ true};
    }

    return SubtypingResult::any(subtypings);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const NegationType* subNegation, TypeId superTy, NotNull<Scope> scope)
{
    TypeId negatedTy = follow(subNegation->ty);

    SubtypingResult result;

    // In order to follow a consistent codepath, rather than folding the
    // isCovariantWith test down to its conclusion here, we test the subtyping test
    // of the result of negating the type for never, unknown, any, and error.
    if (is<NeverType>(negatedTy))
    {
        // ¬never ~ unknown
        result = isCovariantWith(env, builtinTypes->unknownType, superTy, scope).withSubComponent(TypePath::TypeField::Negated);
    }
    else if (is<UnknownType>(negatedTy))
    {
        // ¬unknown ~ never
        result = isCovariantWith(env, builtinTypes->neverType, superTy, scope).withSubComponent(TypePath::TypeField::Negated);
    }
    else if (is<AnyType>(negatedTy))
    {
        // ¬any ~ any
        result = isCovariantWith(env, negatedTy, superTy, scope).withSubComponent(TypePath::TypeField::Negated);
    }
    else if (auto u = get<UnionType>(negatedTy))
    {
        // ¬(A ∪ B) ~ ¬A ∩ ¬B
        // follow intersection rules: A & B <: T iff A <: T && B <: T
        std::vector<SubtypingResult> subtypings;

        for (TypeId ty : u)
        {
            if (auto negatedPart = get<NegationType>(follow(ty)))
                subtypings.push_back(isCovariantWith(env, negatedPart->ty, superTy, scope).withSubComponent(TypePath::TypeField::Negated));
            else
            {
                NegationType negatedTmp{ty};
                subtypings.push_back(isCovariantWith(env, &negatedTmp, superTy, scope));
            }
        }

        result = SubtypingResult::all(subtypings);
    }
    else if (auto i = get<IntersectionType>(negatedTy))
    {
        // ¬(A ∩ B) ~ ¬A ∪ ¬B
        // follow union rules: A | B <: T iff A <: T || B <: T
        std::vector<SubtypingResult> subtypings;

        for (TypeId ty : i)
        {
            if (auto negatedPart = get<NegationType>(follow(ty)))
                subtypings.push_back(isCovariantWith(env, negatedPart->ty, superTy, scope).withSubComponent(TypePath::TypeField::Negated));
            else
            {
                NegationType negatedTmp{ty};
                subtypings.push_back(isCovariantWith(env, &negatedTmp, superTy, scope));
            }
        }

        result = SubtypingResult::any(subtypings);
    }
    else if (is<ErrorType, FunctionType, TableType, MetatableType>(negatedTy))
    {
        iceReporter->ice("attempting to negate a non-testable type");
    }
    // negating a different subtype will get you a very wide type that's not a
    // subtype of other stuff.
    else
    {
        result = SubtypingResult{false}.withSubComponent(TypePath::TypeField::Negated);
    }

    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TypeId subTy, const NegationType* superNegation, NotNull<Scope> scope)
{
    TypeId negatedTy = follow(superNegation->ty);

    SubtypingResult result;

    if (is<NeverType>(negatedTy))
    {
        // ¬never ~ unknown
        result = isCovariantWith(env, subTy, builtinTypes->unknownType, scope);
    }
    else if (is<UnknownType>(negatedTy))
    {
        // ¬unknown ~ never
        result = isCovariantWith(env, subTy, builtinTypes->neverType, scope);
    }
    else if (is<AnyType>(negatedTy))
    {
        // ¬any ~ any
        result = isSubtype(subTy, negatedTy, scope);
    }
    else if (auto u = get<UnionType>(negatedTy))
    {
        // ¬(A ∪ B) ~ ¬A ∩ ¬B
        // follow intersection rules: A & B <: T iff A <: T && B <: T
        std::vector<SubtypingResult> subtypings;

        for (TypeId ty : u)
        {
            if (auto negatedPart = get<NegationType>(follow(ty)))
                subtypings.push_back(isCovariantWith(env, subTy, negatedPart->ty, scope));
            else
            {
                NegationType negatedTmp{ty};
                subtypings.push_back(isCovariantWith(env, subTy, &negatedTmp, scope));
            }
        }

        return SubtypingResult::all(subtypings);
    }
    else if (auto i = get<IntersectionType>(negatedTy))
    {
        // ¬(A ∩ B) ~ ¬A ∪ ¬B
        // follow union rules: A | B <: T iff A <: T || B <: T
        std::vector<SubtypingResult> subtypings;

        for (TypeId ty : i)
        {
            if (auto negatedPart = get<NegationType>(follow(ty)))
                subtypings.push_back(isCovariantWith(env, subTy, negatedPart->ty, scope));
            else
            {
                NegationType negatedTmp{ty};
                subtypings.push_back(isCovariantWith(env, subTy, &negatedTmp, scope));
            }
        }

        return SubtypingResult::any(subtypings);
    }
    else if (auto p = get2<PrimitiveType, PrimitiveType>(subTy, negatedTy))
    {
        // number <: ¬boolean
        // number </: ¬number
        result = {p.first->type != p.second->type};
    }
    else if (auto p = get2<SingletonType, PrimitiveType>(subTy, negatedTy))
    {
        // "foo" </: ¬string
        if (get<StringSingleton>(p.first) && p.second->type == PrimitiveType::String)
            result = {false};
        // false </: ¬boolean
        else if (get<BooleanSingleton>(p.first) && p.second->type == PrimitiveType::Boolean)
            result = {false};
        // other cases are true
        else
            result = {true};
    }
    else if (auto p = get2<PrimitiveType, SingletonType>(subTy, negatedTy))
    {
        if (p.first->type == PrimitiveType::String && get<StringSingleton>(p.second))
            result = {false};
        else if (p.first->type == PrimitiveType::Boolean && get<BooleanSingleton>(p.second))
            result = {false};
        else
            result = {true};
    }
    // the top class type is not actually a primitive type, so the negation of
    // any one of them includes the top class type.
    else if (auto p = get2<ExternType, PrimitiveType>(subTy, negatedTy))
        result = {true};
    else if (auto p = get<PrimitiveType>(negatedTy); p && is<TableType, MetatableType>(subTy))
        result = {p->type != PrimitiveType::Table};
    else if (auto p = get2<FunctionType, PrimitiveType>(subTy, negatedTy))
        result = {p.second->type != PrimitiveType::Function};
    else if (auto p = get2<SingletonType, SingletonType>(subTy, negatedTy))
        result = {*p.first != *p.second};
    else if (auto p = get2<ExternType, ExternType>(subTy, negatedTy))
        result = SubtypingResult::negate(isCovariantWith(env, p.first, p.second, scope));
    else if (get2<FunctionType, ExternType>(subTy, negatedTy))
        result = {true};
    else if (is<ErrorType, FunctionType, TableType, MetatableType>(negatedTy))
        iceReporter->ice("attempting to negate a non-testable type");
    else
        result = {false};

    return result.withSuperComponent(TypePath::TypeField::Negated);
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const PrimitiveType* subPrim,
    const PrimitiveType* superPrim,
    NotNull<Scope> scope
)
{
    return {subPrim->type == superPrim->type};
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const SingletonType* subSingleton,
    const PrimitiveType* superPrim,
    NotNull<Scope> scope
)
{
    if (get<StringSingleton>(subSingleton) && superPrim->type == PrimitiveType::String)
        return {true};
    else if (get<BooleanSingleton>(subSingleton) && superPrim->type == PrimitiveType::Boolean)
        return {true};
    else
        return {false};
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const SingletonType* subSingleton,
    const SingletonType* superSingleton,
    NotNull<Scope> scope
)
{
    return {*subSingleton == *superSingleton};
}

// Compatibility shim for the unflagged codepath of FFlag::LuauTrackUniqueness
// TODO: Delete this when clipping that flag.
SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TableType* subTable, const TableType* superTable, NotNull<Scope> scope)
{
    return isCovariantWith(env, subTable, superTable, /*forceCovariantTest*/ false, scope);
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const TableType* subTable,
    const TableType* superTable,
    bool forceCovariantTest,
    NotNull<Scope> scope
)
{
    SubtypingResult result{true};

    if (subTable->props.empty() && !subTable->indexer && subTable->state == TableState::Sealed && superTable->indexer)
    {
        // While it is certainly the case that {} </: {T}, the story is a little bit different for {| |} <: {T}
        // The shape of an unsealed tabel is still in flux, so it is probably the case that the unsealed table
        // will later gain the necessary indexer as type inference proceeds.
        //
        // Unsealed tables are always sealed by the time inference completes, so this should never affect the
        // type checking phase.
        return {false};
    }

    for (const auto& [name, superProp] : superTable->props)
    {
        std::vector<SubtypingResult> results;
        if (auto subIter = subTable->props.find(name); subIter != subTable->props.end())
            results.push_back(isCovariantWith(env, subIter->second, superProp, name, forceCovariantTest, scope));
        else if (subTable->indexer)
        {
            if (isCovariantWith(env, builtinTypes->stringType, subTable->indexer->indexType, scope).isSubtype)
            {
                if (superProp.isShared())
                {
                    results.push_back(isInvariantWith(env, subTable->indexer->indexResultType, *superProp.readTy, scope)
                                          .withSubComponent(TypePath::TypeField::IndexResult)
                                          .withSuperComponent(TypePath::Property::read(name)));
                }
                else
                {
                    if (superProp.readTy)
                        results.push_back(isCovariantWith(env, subTable->indexer->indexResultType, *superProp.readTy, scope)
                                              .withSubComponent(TypePath::TypeField::IndexResult)
                                              .withSuperComponent(TypePath::Property::read(name)));
                    if (superProp.writeTy)
                        results.push_back(isContravariantWith(env, subTable->indexer->indexResultType, *superProp.writeTy, scope)
                                              .withSubComponent(TypePath::TypeField::IndexResult)
                                              .withSuperComponent(TypePath::Property::write(name)));
                }
            }
        }

        if (results.empty())
            return SubtypingResult{false};

        result.andAlso(SubtypingResult::all(results));
    }

    if (superTable->indexer)
    {
        if (subTable->indexer)
            result.andAlso(isInvariantWith(env, *subTable->indexer, *superTable->indexer, scope));
        else if (subTable->state != TableState::Sealed)
        {
            // As above, we assume that {| |} <: {T} because the unsealed table
            // on the left will eventually gain the necessary indexer.
            return {true};
        }
        else
            return {false};
    }

    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const MetatableType* subMt, const MetatableType* superMt, NotNull<Scope> scope)
{
    return isCovariantWith(env, subMt->table, superMt->table, scope)
        .withBothComponent(TypePath::TypeField::Table)
        .andAlso(isCovariantWith(env, subMt->metatable, superMt->metatable, scope).withBothComponent(TypePath::TypeField::Metatable));
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const MetatableType* subMt, const TableType* superTable, NotNull<Scope> scope)
{
    if (auto subTable = get<TableType>(follow(subMt->table)))
    {
        if (FFlag::LuauIndexInMetatableSubtyping)
        {
            auto doDefault = [&]()
            {
                return isCovariantWith(env, subTable, superTable, /* forceCovariantTest */ false, scope);
            };

            // My kingdom for `do` notation.

            // TODO CLI-169235: This logic is very mechanical and is,
            // effectively a repeat of the logic for the `index<_, _>`
            // type function. These should use similar logic. Otherwise
            // this all constantly falls over for the same reasons
            // structural subtypying falls over.
            //
            // Notably, this does not support `__index` as a function.

            auto subMTTable = get<TableType>(follow(subMt->metatable));
            if (!subMTTable)
                return doDefault();

            auto __index = subMTTable->props.find("__index");
            if (__index == subMTTable->props.end())
                return doDefault();

            // `read`-only __index sounds reasonable, but write-only
            // or non-shared sounds weird.
            if (!__index->second.readTy)
                return doDefault();

            auto __indexAsTable = get<TableType>(follow(*__index->second.readTy));
            if (!__indexAsTable)
                return doDefault();

            // Consider the snippet:
            //
            //  local ItemContainer = {}
            //  ItemContainer.__index = ItemContainer
            //
            //  function ItemContainer.new()
            //      local self = {}
            //      setmetatable(self, ItemContainer)
            //      return self
            //  end
            //
            //  function ItemContainer:removeItem(itemId, itemType)
            //      self:getItem(itemId, itemType)
            //  end
            //
            //  function ItemContainer:getItem(itemId, itemType) end
            //
            //  local container = ItemContainer.new()
            //  container:removeItem(0, "magic")
            //
            // When we go to check this, we're effectively asking whether
            // `container` is a subtype of the first argument of
            // `container.removeItem`. `container` has a metatable with the
            // `__index` metamethod, so we need to include those fields in the
            // subtype check.
            //
            // However, we need to include a read only view of those fields.
            // Consider:
            //
            //  local Foobar = {}
            //  Foobar.__index = Foobar
            //  Foobar.const = 42
            //
            //  local foobar = setmetatable({}, Foobar)
            //
            //  local _: { const: number } = foobar
            //
            // This should error, as we cannot write to `const`.

            TableType fauxSubTable{*subTable};
            for (auto& [name, prop] : __indexAsTable->props)
            {
                if (prop.readTy && fauxSubTable.props.find(name) == fauxSubTable.props.end())
                    fauxSubTable.props[name] = Property::readonly(*prop.readTy);
            }

            return isCovariantWith(env, &fauxSubTable, superTable, /* forceCovariantTest */ false, scope);

        }
        else
        {
            // Metatables cannot erase properties from the table they're attached to, so
            // the subtyping rule for this is just if the table component is a subtype
            // of the supertype table.
            //
            // There's a flaw here in that if the __index metamethod contributes a new
            // field that would satisfy the subtyping relationship, we'll erroneously say
            // that the metatable isn't a subtype of the table, even though they have
            // compatible properties/shapes. We'll revisit this later when we have a
            // better understanding of how important this is.
            return isCovariantWith(env, subTable, superTable, /* forceCovariantTest */ false, scope);
        }
    }
    else
    {
        // TODO: This may be a case we actually hit?
        return {false};
    }
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const ExternType* subExternType,
    const ExternType* superExternType,
    NotNull<Scope> scope
)
{
    return {isSubclass(subExternType, superExternType)};
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    TypeId subTy,
    const ExternType* subExternType,
    TypeId superTy,
    const TableType* superTable,
    NotNull<Scope> scope
)
{
    SubtypingResult result{true};

    env.substitutions[superTy] = subTy;

    for (const auto& [name, prop] : superTable->props)
    {
        if (auto classProp = lookupExternTypeProp(subExternType, name))
        {
            result.andAlso(isCovariantWith(env, *classProp, prop, name, /*forceCovariantTest*/ false, scope));
        }
        else
        {
            result = {false};
            break;
        }
    }

    env.substitutions[superTy] = nullptr;

    return result;
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const FunctionType* subFunction,
    const FunctionType* superFunction,
    NotNull<Scope> scope
)
{
    SubtypingResult result;

    if (FFlag::LuauSubtypingGenericsDoesntUseVariance && !subFunction->generics.empty())
    {
        for (TypeId g : subFunction->generics)
        {
            g = follow(g);
            if (get<GenericType>(g))
            {
                if (auto bounds = env.mappedGenerics.find(g))
                    // g may shadow an existing generic, so push a fresh set of bounds
                    bounds->emplace_back();
                else
                    env.mappedGenerics[g] = {SubtypingEnvironment::GenericBounds{}};
            }
        }
    }

    if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance && !subFunction->genericPacks.empty())
    {
        std::vector<TypePackId> packs;
        packs.reserve(subFunction->genericPacks.size());

        for (TypePackId g : subFunction->genericPacks)
        {
            g = follow(g);
            if (get<GenericTypePack>(g))
                packs.emplace_back(g);
        }

        env.mappedGenericPacks.pushFrame(packs);
    }

    {
        result.orElse(
            isContravariantWith(env, subFunction->argTypes, superFunction->argTypes, scope).withBothComponent(TypePath::PackField::Arguments)
        );

        // If subtyping failed in the argument packs, we should check if there's a hidden variadic tail and try ignoring it.
        // This might cause subtyping correctly because the sub type here may not have a hidden variadic tail or equivalent.
        if (!result.isSubtype)
        {
            auto [arguments, tail] = flatten(superFunction->argTypes);

            if (auto variadic = get<VariadicTypePack>(tail); variadic && variadic->hidden)
            {
                result.orElse(isContravariantWith(env, subFunction->argTypes, arena->addTypePack(TypePack{arguments}), scope)
                                  .withBothComponent(TypePath::PackField::Arguments));
            }
        }
    }

    result.andAlso(isCovariantWith(env, subFunction->retTypes, superFunction->retTypes, scope).withBothComponent(TypePath::PackField::Returns));

    if (*subFunction->argTypes == *superFunction->argTypes && *subFunction->retTypes == *superFunction->retTypes)
    {
        if (superFunction->generics.size() != subFunction->generics.size())
            result.andAlso({false}).withError(
                TypeError{scope->location, GenericTypeCountMismatch{superFunction->generics.size(), subFunction->generics.size()}}
            );
        if (superFunction->genericPacks.size() != subFunction->genericPacks.size())
            result.andAlso({false}).withError(
                TypeError{scope->location, GenericTypePackCountMismatch{superFunction->genericPacks.size(), subFunction->genericPacks.size()}}
            );
    }

    if (FFlag::LuauSubtypingGenericsDoesntUseVariance && !subFunction->generics.empty())
    {
        for (TypeId g : subFunction->generics)
        {
            g = follow(g);
            if (const GenericType* gen = get<GenericType>(g))
            {
                auto bounds = env.mappedGenerics.find(g);
                LUAU_ASSERT(bounds && !bounds->empty());
                // Check the bounds are valid
                if (FFlag::LuauSubtypingReportGenericBoundMismatches2)
                    result.andAlso(checkGenericBounds(bounds->back(), env, scope, gen->name));
                else
                    result.andAlso(checkGenericBounds_DEPRECATED(bounds->back(), env, scope));

                bounds->pop_back();
            }
        }
    }

    if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance && !subFunction->genericPacks.empty())
    {
        env.mappedGenericPacks.popFrame();
        // This result isn't cacheable, because we may need it to populate the generic pack mapping environment again later
        result.isCacheable = false;
    }

    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TableType* subTable, const PrimitiveType* superPrim, NotNull<Scope> scope)
{
    SubtypingResult result{false};
    if (superPrim->type == PrimitiveType::Table)
        result.isSubtype = true;

    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const PrimitiveType* subPrim, const TableType* superTable, NotNull<Scope> scope)
{
    SubtypingResult result{false};
    if (subPrim->type == PrimitiveType::String)
    {
        if (auto metatable = getMetatable(builtinTypes->stringType, builtinTypes))
        {
            if (auto mttv = get<TableType>(follow(metatable)))
            {
                if (auto it = mttv->props.find("__index"); it != mttv->props.end())
                {
                    // the `string` metatable should not have any write-only types.
                    LUAU_ASSERT(*it->second.readTy);

                    if (auto stringTable = get<TableType>(*it->second.readTy))
                        result.orElse(isCovariantWith(env, stringTable, superTable, /*forceCovariantTest*/ false, scope)
                                          .withSubPath(TypePath::PathBuilder().mt().readProp("__index").build()));
                }
            }
        }
    }
    else if (subPrim->type == PrimitiveType::Table)
    {
        const bool isSubtype = FFlag::LuauSubtypingPrimitiveAndGenericTableTypes
                                   ? superTable->props.empty() && (!superTable->indexer.has_value() || superTable->state == TableState::Generic)
                                   : superTable->props.empty() && !superTable->indexer.has_value();
        return {isSubtype};
    }

    return result;
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const SingletonType* subSingleton,
    const TableType* superTable,
    NotNull<Scope> scope
)
{
    SubtypingResult result{false};
    if (auto stringleton = get<StringSingleton>(subSingleton))
    {
        if (auto metatable = getMetatable(builtinTypes->stringType, builtinTypes))
        {
            if (auto mttv = get<TableType>(follow(metatable)))
            {
                if (auto it = mttv->props.find("__index"); it != mttv->props.end())
                {
                    // the `string` metatable should not have any write-only types.
                    LUAU_ASSERT(*it->second.readTy);

                    if (auto stringTable = get<TableType>(*it->second.readTy))
                        result.orElse(isCovariantWith(env, stringTable, superTable, /*forceCovariantTest*/ false, scope)
                                          .withSubPath(TypePath::PathBuilder().mt().readProp("__index").build()));
                }
            }
        }
    }
    return result;
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const TableIndexer& subIndexer,
    const TableIndexer& superIndexer,
    NotNull<Scope> scope
)
{
    return isInvariantWith(env, subIndexer.indexType, superIndexer.indexType, scope)
        .withBothComponent(TypePath::TypeField::IndexLookup)
        .andAlso(
            isInvariantWith(env, subIndexer.indexResultType, superIndexer.indexResultType, scope).withBothComponent(TypePath::TypeField::IndexResult)
        );
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const Property& subProp,
    const Property& superProp,
    const std::string& name,
    bool forceCovariantTest,
    NotNull<Scope> scope
)
{
    SubtypingResult res{true};

    if (superProp.isShared() && subProp.isShared())
    {
        if (FFlag::LuauTrackUniqueness && forceCovariantTest)
            res.andAlso(isCovariantWith(env, *subProp.readTy, *superProp.readTy, scope).withBothComponent(TypePath::Property::read(name)));
        else
            res.andAlso(isInvariantWith(env, *subProp.readTy, *superProp.readTy, scope).withBothComponent(TypePath::Property::read(name)));
    }
    else
    {
        if (superProp.readTy.has_value() && subProp.readTy.has_value())
            res.andAlso(isCovariantWith(env, *subProp.readTy, *superProp.readTy, scope).withBothComponent(TypePath::Property::read(name)));
        if (FFlag::LuauTrackUniqueness)
        {
            if (superProp.writeTy.has_value() && subProp.writeTy.has_value() && !forceCovariantTest)
                res.andAlso(isContravariantWith(env, *subProp.writeTy, *superProp.writeTy, scope).withBothComponent(TypePath::Property::write(name)));
        }
        else
        {
            if (superProp.writeTy.has_value() && subProp.writeTy.has_value())
                res.andAlso(isContravariantWith(env, *subProp.writeTy, *superProp.writeTy, scope).withBothComponent(TypePath::Property::write(name)));
        }

        if (superProp.isReadWrite())
        {
            if (subProp.isReadOnly())
                res.andAlso(SubtypingResult{false}.withBothComponent(TypePath::Property::read(name)));
            else if (subProp.isWriteOnly())
                res.andAlso(SubtypingResult{false}.withBothComponent(TypePath::Property::write(name)));
        }
    }

    return res;
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const std::shared_ptr<const NormalizedType>& subNorm,
    const std::shared_ptr<const NormalizedType>& superNorm,
    NotNull<Scope> scope
)
{
    if (!subNorm || !superNorm)
        return {false, true};

    SubtypingResult result = isCovariantWith(env, subNorm->tops, superNorm->tops, scope);
    result.andAlso(isCovariantWith(env, subNorm->booleans, superNorm->booleans, scope));
    result.andAlso(isCovariantWith(env, subNorm->externTypes, superNorm->externTypes, scope)
                       .orElse(isCovariantWith(env, subNorm->externTypes, superNorm->tables, scope)));
    result.andAlso(isCovariantWith(env, subNorm->errors, superNorm->errors, scope));
    result.andAlso(isCovariantWith(env, subNorm->nils, superNorm->nils, scope));
    result.andAlso(isCovariantWith(env, subNorm->numbers, superNorm->numbers, scope));
    result.andAlso(isCovariantWith(env, subNorm->strings, superNorm->strings, scope));
    result.andAlso(isCovariantWith(env, subNorm->strings, superNorm->tables, scope));
    result.andAlso(isCovariantWith(env, subNorm->threads, superNorm->threads, scope));
    result.andAlso(isCovariantWith(env, subNorm->buffers, superNorm->buffers, scope));
    result.andAlso(isCovariantWith(env, subNorm->tables, superNorm->tables, scope));
    result.andAlso(isCovariantWith(env, subNorm->functions, superNorm->functions, scope));
    // isCovariantWith(subNorm->tyvars, superNorm->tyvars);
    return result;
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const NormalizedExternType& subExternType,
    const NormalizedExternType& superExternType,
    NotNull<Scope> scope
)
{
    for (const auto& [subExternTypeTy, _] : subExternType.externTypes)
    {
        SubtypingResult result;

        for (const auto& [superExternTypeTy, superNegations] : superExternType.externTypes)
        {
            result.orElse(isCovariantWith(env, subExternTypeTy, superExternTypeTy, scope));
            if (!result.isSubtype)
                continue;

            for (TypeId negation : superNegations)
            {
                result.andAlso(SubtypingResult::negate(isCovariantWith(env, subExternTypeTy, negation, scope)));
                if (result.isSubtype)
                    break;
            }
        }

        if (!result.isSubtype)
            return result;
    }

    return {true};
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const NormalizedExternType& subExternType,
    const TypeIds& superTables,
    NotNull<Scope> scope
)
{
    for (const auto& [subExternTypeTy, _] : subExternType.externTypes)
    {
        SubtypingResult result;

        for (TypeId superTableTy : superTables)
            result.orElse(isCovariantWith(env, subExternTypeTy, superTableTy, scope));

        if (!result.isSubtype)
            return result;
    }

    return {true};
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const NormalizedStringType& subString,
    const NormalizedStringType& superString,
    NotNull<Scope> scope
)
{
    bool isSubtype = Luau::isSubtype(subString, superString);
    return {isSubtype};
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const NormalizedStringType& subString,
    const TypeIds& superTables,
    NotNull<Scope> scope
)
{
    if (subString.isNever())
        return {true};

    if (subString.isCofinite)
    {
        SubtypingResult result;
        for (const auto& superTable : superTables)
        {
            result.orElse(isCovariantWith(env, builtinTypes->stringType, superTable, scope));
            if (result.isSubtype)
                return result;
        }
        return result;
    }

    // Finite case
    // S = s1 | s2 | s3 ... sn <: t1 | t2 | ... | tn
    // iff for some ti, S <: ti
    // iff for all sj, sj <: ti
    for (const auto& superTable : superTables)
    {
        SubtypingResult result{true};
        for (const auto& [_, subString] : subString.singletons)
        {
            result.andAlso(isCovariantWith(env, subString, superTable, scope));
            if (!result.isSubtype)
                break;
        }

        if (!result.isSubtype)
            continue;
        else
            return result;
    }

    return {false};
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const NormalizedFunctionType& subFunction,
    const NormalizedFunctionType& superFunction,
    NotNull<Scope> scope
)
{
    if (subFunction.isNever())
        return {true};
    else if (superFunction.isTop)
        return {true};
    else
        return isCovariantWith(env, subFunction.parts, superFunction.parts, scope);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TypeIds& subTypes, const TypeIds& superTypes, NotNull<Scope> scope)
{
    std::vector<SubtypingResult> results;

    for (TypeId subTy : subTypes)
    {
        results.emplace_back();
        for (TypeId superTy : superTypes)
        {
            results.back().orElse(isCovariantWith(env, subTy, superTy, scope));

            if (results.back().normalizationTooComplex)
                return SubtypingResult{false, /* normalizationTooComplex */ true};
        }
    }

    return SubtypingResult::all(results);
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const VariadicTypePack* subVariadic,
    const VariadicTypePack* superVariadic,
    NotNull<Scope> scope
)
{
    return isCovariantWith(env, subVariadic->ty, superVariadic->ty, scope).withBothComponent(TypePath::TypeField::Variadic);
}

bool Subtyping::bindGeneric(SubtypingEnvironment& env, TypeId subTy, TypeId superTy)
{
    if (FFlag::LuauSubtypingGenericsDoesntUseVariance)
    {
        subTy = follow(subTy);
        superTy = follow(superTy);
        std::optional<SubtypingEnvironment::GenericBounds> originalSubTyBounds = std::nullopt;

        if (const auto subBounds = env.mappedGenerics.find(subTy); subBounds && !subBounds->empty())
        {
            LUAU_ASSERT(get<GenericType>(subTy));

            originalSubTyBounds = SubtypingEnvironment::GenericBounds{subBounds->back()};

            auto& [lowerSubBounds, upperSubBounds] = subBounds->back();

            if (const auto superBounds = env.mappedGenerics.find(superTy); superBounds && !superBounds->empty())
            {
                LUAU_ASSERT(get<GenericType>(superTy));

                const auto& [lowerSuperBounds, upperSuperBounds] = superBounds->back();

                maybeUpdateBounds(subTy, superTy, upperSubBounds, lowerSuperBounds, upperSuperBounds);
            }
            else
                upperSubBounds.insert(superTy);
        }
        else if (env.containsMappedType(subTy))
            iceReporter->ice("attempting to modify bounds of a potentially visited generic");

        if (const auto superBounds = env.mappedGenerics.find(superTy); superBounds && !superBounds->empty())
        {
            LUAU_ASSERT(get<GenericType>(superTy));

            auto& [lowerSuperBounds, upperSuperBounds] = superBounds->back();

            if (originalSubTyBounds)
            {
                LUAU_ASSERT(get<GenericType>(subTy));

                const auto& [originalLowerSubBound, originalUpperSubBound] = *originalSubTyBounds;

                maybeUpdateBounds(superTy, subTy, lowerSuperBounds, originalUpperSubBound, originalLowerSubBound);
            }
            else
                lowerSuperBounds.insert(subTy);
        }
        else if (env.containsMappedType(superTy))
            iceReporter->ice("attempting to modify bounds of a potentially visited generic");
    }
    else
    {
        if (variance == Variance::Covariant)
        {
            if (!get<GenericType>(subTy))
                return false;

            if (!env.mappedGenerics_DEPRECATED.find(subTy) && env.containsMappedType(subTy))
                iceReporter->ice("attempting to modify bounds of a potentially visited generic");

            env.mappedGenerics_DEPRECATED[subTy].upperBound.insert(superTy);
        }
        else
        {
            if (!get<GenericType>(superTy))
                return false;

            if (!env.mappedGenerics_DEPRECATED.find(superTy) && env.containsMappedType(superTy))
                iceReporter->ice("attempting to modify bounds of a potentially visited generic");

            env.mappedGenerics_DEPRECATED[superTy].lowerBound.insert(subTy);
        }
    }

    return true;
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const TypeFunctionInstanceType* subFunctionInstance,
    const TypeId superTy,
    NotNull<Scope> scope
)
{
    // Reduce the type function instance
    auto [ty, errors] = handleTypeFunctionReductionResult(subFunctionInstance, scope);

    // If we return optional, that means the type function was irreducible - we can reduce that to never
    return isCovariantWith(env, ty, superTy, scope).withErrors(errors).withSubComponent(TypePath::Reduction{ty});
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env,
    const TypeId subTy,
    const TypeFunctionInstanceType* superFunctionInstance,
    NotNull<Scope> scope
)
{
    // Reduce the type function instance
    auto [ty, errors] = handleTypeFunctionReductionResult(superFunctionInstance, scope);
    return isCovariantWith(env, subTy, ty, scope).withErrors(errors).withSuperComponent(TypePath::Reduction{ty});
}

/*
 * If, when performing a subtyping test, we encounter a generic on the left
 * side, it is permissible to tentatively bind that generic to the right side
 * type.
 */
bool Subtyping::bindGeneric_DEPRECATED(SubtypingEnvironment& env, TypePackId subTp, TypePackId superTp) const
{
    LUAU_ASSERT(!FFlag::LuauSubtypingGenericPacksDoesntUseVariance);
    if (variance == Variance::Contravariant)
        std::swap(superTp, subTp);

    if (!get<GenericTypePack>(subTp))
        return false;

    if (TypePackId* m = env.getMappedPackBounds_DEPRECATED(subTp))
        return *m == superTp;

    if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
    {
        // We shouldn't bind generic type packs to themselves
        if (subTp == superTp)
            return true;
    }

    env.mappedGenericPacks_DEPRECATED[subTp] = superTp;

    return true;
}

template<typename T, typename Container>
TypeId Subtyping::makeAggregateType(const Container& container, TypeId orElse)
{
    if (container.empty())
        return orElse;
    else if (container.size() == 1)
        return *begin(container);
    else
        return arena->addType(T{std::vector<TypeId>(begin(container), end(container))});
}

std::pair<TypeId, ErrorVec> Subtyping::handleTypeFunctionReductionResult(const TypeFunctionInstanceType* functionInstance, NotNull<Scope> scope)
{
    TypeFunctionContext context{arena, builtinTypes, scope, simplifier, normalizer, typeFunctionRuntime, iceReporter, NotNull{&limits}};
    TypeId function = arena->addType(*functionInstance);
    FunctionGraphReductionResult result = reduceTypeFunctions(function, {}, NotNull{&context}, true);
    ErrorVec errors;
    if (result.blockedTypes.size() != 0 || result.blockedPacks.size() != 0)
    {
        if (FFlag::LuauEmplaceNotPushBack)
            errors.emplace_back(Location{}, UninhabitedTypeFunction{function});
        else
            errors.push_back(TypeError{{}, UninhabitedTypeFunction{function}});
        return {builtinTypes->neverType, errors};
    }
    if (result.reducedTypes.contains(function))
        return {function, errors};
    return {builtinTypes->neverType, errors};
}

SubtypingResult Subtyping::trySemanticSubtyping(
    SubtypingEnvironment& env,
    TypeId subTy,
    TypeId superTy,
    NotNull<Scope> scope,
    SubtypingResult& original
)
{
    SubtypingResult semantic = isCovariantWith(env, normalizer->normalize(subTy), normalizer->normalize(superTy), scope);

    if (semantic.normalizationTooComplex)
    {
        return semantic;
    }
    else if (semantic.isSubtype)
    {
        semantic.reasoning.clear();
        return semantic;
    }

    return original;
}


SubtypingResult Subtyping::checkGenericBounds(
    const SubtypingEnvironment::GenericBounds& bounds,
    SubtypingEnvironment& env,
    NotNull<Scope> scope,
    std::string_view genericName
)
{
    LUAU_ASSERT(FFlag::LuauSubtypingGenericsDoesntUseVariance);
    LUAU_ASSERT(FFlag::LuauSubtypingReportGenericBoundMismatches2);

    SubtypingResult result{true};

    const auto& [lb, ub] = bounds;

    TypeIds lbTypes;
    for (TypeId t : lb)
    {
        t = follow(t);
        if (const auto mappedBounds = env.mappedGenerics.find(t))
        {
            if (mappedBounds->empty()) // If the generic is no longer in scope, we don't have any info about it
                continue;

            auto& [lowerBound, upperBound] = mappedBounds->back();
            // We're populating the lower bounds, so we prioritize the upper bounds of a mapped generic
            if (!upperBound.empty())
                lbTypes.insert(upperBound.begin(), upperBound.end());
            else if (!lowerBound.empty())
                lbTypes.insert(lowerBound.begin(), lowerBound.end());
            else
                lbTypes.insert(builtinTypes->unknownType);
        }
        else
            lbTypes.insert(t);
    }

    TypeIds ubTypes;
    for (TypeId t : ub)
    {
        t = follow(t);
        if (const auto mappedBounds = env.mappedGenerics.find(t))
        {
            if (mappedBounds->empty()) // If the generic is no longer in scope, we don't have any info about it
                continue;

            auto& [lowerBound, upperBound] = mappedBounds->back();
            // We're populating the upper bounds, so we prioritize the lower bounds of a mapped generic
            if (!lowerBound.empty())
                ubTypes.insert(lowerBound.begin(), lowerBound.end());
            else if (!upperBound.empty())
                ubTypes.insert(upperBound.begin(), upperBound.end());
            else
                ubTypes.insert(builtinTypes->unknownType);
        }
        else
            ubTypes.insert(t);
    }
    TypeId lowerBound = makeAggregateType<UnionType>(lbTypes.take(), builtinTypes->neverType);
    TypeId upperBound = makeAggregateType<IntersectionType>(ubTypes.take(), builtinTypes->unknownType);

    std::shared_ptr<const NormalizedType> nt = normalizer->normalize(upperBound);
    // we say that the result is true if normalization failed because complex types are likely to be inhabited.
    NormalizationResult res = nt ? normalizer->isInhabited(nt.get()) : NormalizationResult::True;

    if (!nt || res == NormalizationResult::HitLimits)
        result.normalizationTooComplex = true;
    else if (res == NormalizationResult::False)
    {
        /* If the normalized upper bound we're mapping to a generic is
         * uninhabited, then we must consider the subtyping relation not to
         * hold.
         *
         * This happens eg in <T>() -> (T, T) <: () -> (string, number)
         *
         * T appears in covariant position and would have to be both string
         * and number at once.
         *
         * No actual value is both a string and a number, so the test fails.
         *
         * TODO: We'll need to add explanitory context here.
         */
        result.isSubtype = false;
    }

    SubtypingEnvironment boundsEnv;
    boundsEnv.parent = &env;
    SubtypingResult boundsResult = isCovariantWith(boundsEnv, lowerBound, upperBound, scope);
    boundsResult.reasoning.clear();

    if (res == NormalizationResult::False)
        result.genericBoundsMismatches.emplace_back(genericName, bounds.lowerBound, bounds.upperBound);
    else if (!boundsResult.isSubtype)
    {
        if (FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds)
        {
            // Check if the bounds are error suppressing before reporting a mismatch
            switch (shouldSuppressErrors(normalizer, lowerBound).orElse(shouldSuppressErrors(normalizer, upperBound)))
            {
            case ErrorSuppression::Suppress:
                break;
            case ErrorSuppression::NormalizationFailed:
                // intentionally fallthrough here since we couldn't prove this was error-suppressing
                [[fallthrough]];
            case ErrorSuppression::DoNotSuppress:
                result.genericBoundsMismatches.emplace_back(genericName, bounds.lowerBound, bounds.upperBound);
                break;
            default:
                LUAU_ASSERT(0);
                break;
            }
        }
        else
            result.genericBoundsMismatches.emplace_back(genericName, bounds.lowerBound, bounds.upperBound);
    }

    result.andAlso(boundsResult);

    return result;
}

SubtypingResult Subtyping::checkGenericBounds_DEPRECATED(
    const SubtypingEnvironment::GenericBounds& bounds,
    SubtypingEnvironment& env,
    NotNull<Scope> scope
)
{
    LUAU_ASSERT(FFlag::LuauSubtypingGenericsDoesntUseVariance);
    LUAU_ASSERT(!FFlag::LuauSubtypingReportGenericBoundMismatches2);

    SubtypingResult result{true};

    const auto& [lb, ub] = bounds;

    TypeIds lbTypes;
    for (TypeId t : lb)
    {
        t = follow(t);
        if (const auto mappedBounds = env.mappedGenerics.find(t))
        {
            if (mappedBounds->empty()) // If the generic is no longer in scope, we don't have any info about it
                continue;

            auto& [lowerBound, upperBound] = mappedBounds->back();
            // We're populating the lower bounds, so we prioritize the upper bounds of a mapped generic
            if (!upperBound.empty())
                lbTypes.insert(upperBound.begin(), upperBound.end());
            else if (!lowerBound.empty())
                lbTypes.insert(lowerBound.begin(), lowerBound.end());
            else
                lbTypes.insert(builtinTypes->unknownType);
        }
        else
            lbTypes.insert(t);
    }

    TypeIds ubTypes;
    for (TypeId t : ub)
    {
        t = follow(t);
        if (const auto mappedBounds = env.mappedGenerics.find(t))
        {
            if (mappedBounds->empty()) // If the generic is no longer in scope, we don't have any info about it
                continue;

            auto& [lowerBound, upperBound] = mappedBounds->back();
            // We're populating the upper bounds, so we prioritize the lower bounds of a mapped generic
            if (!lowerBound.empty())
                ubTypes.insert(lowerBound.begin(), lowerBound.end());
            else if (!upperBound.empty())
                ubTypes.insert(upperBound.begin(), upperBound.end());
            else
                ubTypes.insert(builtinTypes->unknownType);
        }
        else
            ubTypes.insert(t);
    }
    TypeId lowerBound = makeAggregateType<UnionType>(lbTypes.take(), builtinTypes->neverType);
    TypeId upperBound = makeAggregateType<IntersectionType>(ubTypes.take(), builtinTypes->unknownType);

    std::shared_ptr<const NormalizedType> nt = normalizer->normalize(upperBound);
    // we say that the result is true if normalization failed because complex types are likely to be inhabited.
    NormalizationResult res = nt ? normalizer->isInhabited(nt.get()) : NormalizationResult::True;

    if (!nt || res == NormalizationResult::HitLimits)
        result.normalizationTooComplex = true;
    else if (res == NormalizationResult::False)
    {
        /* If the normalized upper bound we're mapping to a generic is
         * uninhabited, then we must consider the subtyping relation not to
         * hold.
         *
         * This happens eg in <T>() -> (T, T) <: () -> (string, number)
         *
         * T appears in covariant position and would have to be both string
         * and number at once.
         *
         * No actual value is both a string and a number, so the test fails.
         *
         * TODO: We'll need to add explanitory context here.
         */
        result.isSubtype = false;
    }

    SubtypingEnvironment boundsEnv;
    boundsEnv.parent = &env;
    SubtypingResult boundsResult = isCovariantWith(boundsEnv, lowerBound, upperBound, scope);
    boundsResult.reasoning.clear();
    result.andAlso(boundsResult);

    return result;
}

void Subtyping::maybeUpdateBounds(
    TypeId here,
    TypeId there,
    TypeIds& boundsToUpdate,
    const TypeIds& firstBoundsToCheck,
    const TypeIds& secondBoundsToCheck
)
{
    bool boundsChanged = false;

    if (!firstBoundsToCheck.empty())
    {
        for (const TypeId t : firstBoundsToCheck)
        {
            if (t != here) // We don't want to bound a generic by itself, ie A <: A
            {
                boundsToUpdate.insert(t);
                boundsChanged = true;
            }
        }
    }
    if (!boundsChanged && !secondBoundsToCheck.empty())
    {
        for (const TypeId t : secondBoundsToCheck)
        {
            if (t != here)
            {
                boundsToUpdate.insert(t);
                boundsChanged = true;
            }
        }
    }
    if (!boundsChanged && here != there)
        boundsToUpdate.insert(there);
}

} // namespace Luau
