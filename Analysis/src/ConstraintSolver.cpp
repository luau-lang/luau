// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ConstraintSolver.h"

#include "Luau/Anyification.h"
#include "Luau/ApplyTypeFunction.h"
#include "Luau/Common.h"
#include "Luau/DcrLogger.h"
#include "Luau/Generalization.h"
#include "Luau/Instantiation.h"
#include "Luau/Instantiation2.h"
#include "Luau/Location.h"
#include "Luau/ModuleResolver.h"
#include "Luau/OverloadResolution.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Simplify.h"
#include "Luau/TableLiteralInference.h"
#include "Luau/TimeTrace.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"
#include "Luau/VisitType.h"

#include <algorithm>
#include <utility>

LUAU_FASTFLAGVARIABLE(DebugLuauAssertOnForcedConstraint)
LUAU_FASTFLAGVARIABLE(DebugLuauLogSolver)
LUAU_FASTFLAGVARIABLE(DebugLuauLogSolverIncludeDependencies)
LUAU_FASTFLAGVARIABLE(DebugLuauLogBindings)
LUAU_FASTINTVARIABLE(LuauSolverRecursionLimit, 500)
LUAU_FASTFLAGVARIABLE(DebugLuauEqSatSimplification)
LUAU_FASTFLAG(LuauEagerGeneralization3)
LUAU_FASTFLAG(LuauDeprecatedAttribute)
LUAU_FASTFLAGVARIABLE(LuauAddCallConstraintForIterableFunctions)
LUAU_FASTFLAGVARIABLE(LuauGuardAgainstMalformedTypeAliasExpansion2)
LUAU_FASTFLAGVARIABLE(LuauInsertErrorTypesIntoIndexerResult)
LUAU_FASTFLAGVARIABLE(LuauClipVariadicAnysFromArgsToGenericFuncs2)
LUAU_FASTFLAG(LuauTableLiteralSubtypeSpecificCheck)
LUAU_FASTFLAGVARIABLE(LuauAvoidGenericsLeakingDuringFunctionCallCheck)
LUAU_FASTFLAGVARIABLE(LuauMissingFollowInAssignIndexConstraint)

namespace Luau
{

static void dump(ConstraintSolver* cs, ToStringOptions& opts);

size_t HashBlockedConstraintId::operator()(const BlockedConstraintId& bci) const
{
    size_t result = 0;

    if (const TypeId* ty = get_if<TypeId>(&bci))
        result = std::hash<TypeId>()(*ty);
    else if (const TypePackId* tp = get_if<TypePackId>(&bci))
        result = std::hash<TypePackId>()(*tp);
    else if (Constraint const* const* c = get_if<const Constraint*>(&bci))
        result = std::hash<const Constraint*>()(*c);
    else
        LUAU_ASSERT(!"Should be unreachable");

    return result;
}

[[maybe_unused]] static void dumpBindings(NotNull<Scope> scope, ToStringOptions& opts)
{
    for (const auto& [k, v] : scope->bindings)
    {
        auto d = toString(v.typeId, opts);
        printf("\t%s : %s\n", k.c_str(), d.c_str());
    }

    for (NotNull<Scope> child : scope->children)
        dumpBindings(child, opts);
}

// used only in asserts
[[maybe_unused]] static bool canMutate(TypeId ty, NotNull<const Constraint> constraint)
{
    if (auto blocked = get<BlockedType>(ty))
    {
        const Constraint* owner = blocked->getOwner();
        LUAU_ASSERT(owner);
        return owner == constraint;
    }

    return true;
}

// used only in asserts
[[maybe_unused]] static bool canMutate(TypePackId tp, NotNull<const Constraint> constraint)
{
    if (auto blocked = get<BlockedTypePack>(tp))
    {
        Constraint* owner = blocked->owner;
        LUAU_ASSERT(owner);
        return owner == constraint;
    }

    return true;
}

std::pair<std::vector<TypeId>, std::vector<TypePackId>> saturateArguments(
    TypeArena* arena,
    NotNull<BuiltinTypes> builtinTypes,
    const TypeFun& fn,
    const std::vector<TypeId>& rawTypeArguments,
    const std::vector<TypePackId>& rawPackArguments
)
{
    std::vector<TypeId> saturatedTypeArguments;
    std::vector<TypeId> extraTypes;
    std::vector<TypePackId> saturatedPackArguments;

    for (size_t i = 0; i < rawTypeArguments.size(); ++i)
    {
        TypeId ty = rawTypeArguments[i];

        if (i < fn.typeParams.size())
            saturatedTypeArguments.push_back(ty);
        else
            extraTypes.push_back(ty);
    }

    // If we collected extra types, put them in a type pack now. This case is
    // mutually exclusive with the type pack -> type conversion we do below:
    // extraTypes will only have elements in it if we have more types than we
    // have parameter slots for them to go into.
    if (!extraTypes.empty() && !fn.typePackParams.empty())
    {
        saturatedPackArguments.push_back(arena->addTypePack(extraTypes));
    }

    for (size_t i = 0; i < rawPackArguments.size(); ++i)
    {
        TypePackId tp = rawPackArguments[i];

        // If we are short on regular type saturatedTypeArguments and we have a single
        // element type pack, we can decompose that to the type it contains and
        // use that as a type parameter.
        if (saturatedTypeArguments.size() < fn.typeParams.size() && size(tp) == 1 && finite(tp) && first(tp) && saturatedPackArguments.empty())
        {
            saturatedTypeArguments.push_back(*first(tp));
        }
        else if (saturatedPackArguments.size() < fn.typePackParams.size())
        {
            saturatedPackArguments.push_back(tp);
        }
    }

    size_t typesProvided = saturatedTypeArguments.size();
    size_t typesRequired = fn.typeParams.size();

    size_t packsProvided = saturatedPackArguments.size();
    size_t packsRequired = fn.typePackParams.size();

    // Extra types should be accumulated in extraTypes, not saturatedTypeArguments. Extra
    // packs will be accumulated in saturatedPackArguments, so we don't have an
    // assertion for that.
    LUAU_ASSERT(typesProvided <= typesRequired);

    // If we didn't provide enough types, but we did provide a type pack, we
    // don't want to use defaults. The rationale for this is that if the user
    // provides a pack but doesn't provide enough types, we want to report an
    // error, rather than simply using the default saturatedTypeArguments, if they exist. If
    // they did provide enough types, but not enough packs, we of course want to
    // use the default packs.
    bool needsDefaults = (typesProvided < typesRequired && packsProvided == 0) || (typesProvided == typesRequired && packsProvided < packsRequired);

    if (needsDefaults)
    {
        // Default types can reference earlier types. It's legal to write
        // something like
        // type T<A, B = A> = (A, B) -> number
        // and we need to respect that. We use an ApplyTypeFunction for this.
        ApplyTypeFunction atf{arena};

        for (size_t i = 0; i < typesProvided; ++i)
            atf.typeArguments[fn.typeParams[i].ty] = saturatedTypeArguments[i];

        for (size_t i = typesProvided; i < typesRequired; ++i)
        {
            TypeId defaultTy = fn.typeParams[i].defaultValue.value_or(nullptr);

            // We will fill this in with the error type later.
            if (!defaultTy)
                break;

            TypeId instantiatedDefault = atf.substitute(defaultTy).value_or(builtinTypes->errorRecoveryType());
            atf.typeArguments[fn.typeParams[i].ty] = instantiatedDefault;
            saturatedTypeArguments.push_back(instantiatedDefault);
        }

        for (size_t i = 0; i < packsProvided; ++i)
        {
            atf.typePackArguments[fn.typePackParams[i].tp] = saturatedPackArguments[i];
        }

        for (size_t i = packsProvided; i < packsRequired; ++i)
        {
            TypePackId defaultTp = fn.typePackParams[i].defaultValue.value_or(nullptr);

            // We will fill this in with the error type pack later.
            if (!defaultTp)
                break;

            TypePackId instantiatedDefault = atf.substitute(defaultTp).value_or(builtinTypes->errorRecoveryTypePack());
            atf.typePackArguments[fn.typePackParams[i].tp] = instantiatedDefault;
            saturatedPackArguments.push_back(instantiatedDefault);
        }
    }

    // If we didn't create an extra type pack from overflowing parameter packs,
    // and we're still missing a type pack, plug in an empty type pack as the
    // value of the empty packs.
    if (extraTypes.empty() && saturatedPackArguments.size() + 1 == fn.typePackParams.size())
    {
        saturatedPackArguments.push_back(arena->addTypePack({}));
    }

    // We need to have _something_ when we substitute the generic saturatedTypeArguments,
    // even if they're missing, so we use the error type as a filler.
    for (size_t i = saturatedTypeArguments.size(); i < typesRequired; ++i)
    {
        saturatedTypeArguments.push_back(builtinTypes->errorRecoveryType());
    }

    for (size_t i = saturatedPackArguments.size(); i < packsRequired; ++i)
    {
        saturatedPackArguments.push_back(builtinTypes->errorRecoveryTypePack());
    }

    for (TypeId& arg : saturatedTypeArguments)
        arg = follow(arg);

    for (TypePackId& pack : saturatedPackArguments)
        pack = follow(pack);

    // At this point, these two conditions should be true. If they aren't we
    // will run into access violations.
    LUAU_ASSERT(saturatedTypeArguments.size() == fn.typeParams.size());
    LUAU_ASSERT(saturatedPackArguments.size() == fn.typePackParams.size());

    return {saturatedTypeArguments, saturatedPackArguments};
}

bool InstantiationSignature::operator==(const InstantiationSignature& rhs) const
{
    return fn == rhs.fn && arguments == rhs.arguments && packArguments == rhs.packArguments;
}

size_t HashInstantiationSignature::operator()(const InstantiationSignature& signature) const
{
    size_t hash = std::hash<TypeId>{}(signature.fn.type);
    for (const GenericTypeDefinition& p : signature.fn.typeParams)
    {
        hash ^= (std::hash<TypeId>{}(p.ty) << 1);
    }

    for (const GenericTypePackDefinition& p : signature.fn.typePackParams)
    {
        hash ^= (std::hash<TypePackId>{}(p.tp) << 1);
    }

    for (const TypeId a : signature.arguments)
    {
        hash ^= (std::hash<TypeId>{}(a) << 1);
    }

    for (const TypePackId a : signature.packArguments)
    {
        hash ^= (std::hash<TypePackId>{}(a) << 1);
    }

    return hash;
}

struct InstantiationQueuer : TypeOnceVisitor
{
    ConstraintSolver* solver;
    NotNull<Scope> scope;
    Location location;

    explicit InstantiationQueuer(NotNull<Scope> scope, const Location& location, ConstraintSolver* solver)
        : solver(solver)
        , scope(scope)
        , location(location)
    {
    }

    bool visit(TypeId ty, const PendingExpansionType& petv) override
    {
        solver->pushConstraint(scope, location, TypeAliasExpansionConstraint{ty});
        return false;
    }

    bool visit(TypeId ty, const TypeFunctionInstanceType&) override
    {
        solver->pushConstraint(scope, location, ReduceConstraint{ty});
        return true;
    }

    bool visit(TypeId ty, const ExternType& etv) override
    {
        return false;
    }
};

ConstraintSolver::ConstraintSolver(
    NotNull<Normalizer> normalizer,
    NotNull<Simplifier> simplifier,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    ModuleName moduleName,
    NotNull<ModuleResolver> moduleResolver,
    std::vector<RequireCycle> requireCycles,
    DcrLogger* logger,
    NotNull<const DataFlowGraph> dfg,
    TypeCheckLimits limits,
    ConstraintSet constraintSet_
)
    : arena(normalizer->arena)
    , builtinTypes(normalizer->builtinTypes)
    , normalizer(normalizer)
    , simplifier(simplifier)
    , typeFunctionRuntime(typeFunctionRuntime)
    , constraintSet(std::move(constraintSet_))
    , constraints(borrowConstraints(constraintSet.constraints))
    , scopeToFunction(&constraintSet.scopeToFunction)
    , rootScope(constraintSet.rootScope)
    , currentModuleName(std::move(moduleName))
    , dfg(dfg)
    , moduleResolver(moduleResolver)
    , requireCycles(std::move(requireCycles))
    , logger(logger)
    , limits(std::move(limits))
    , opts{/*exhaustive*/ true}
{
    initFreeTypeTracking();
}

ConstraintSolver::ConstraintSolver(
    NotNull<Normalizer> normalizer,
    NotNull<Simplifier> simplifier,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    NotNull<Scope> rootScope,
    std::vector<NotNull<Constraint>> constraints,
    NotNull<DenseHashMap<Scope*, TypeId>> scopeToFunction,
    ModuleName moduleName,
    NotNull<ModuleResolver> moduleResolver,
    std::vector<RequireCycle> requireCycles,
    DcrLogger* logger,
    NotNull<const DataFlowGraph> dfg,
    TypeCheckLimits limits
)
    : arena(normalizer->arena)
    , builtinTypes(normalizer->builtinTypes)
    , normalizer(normalizer)
    , simplifier(simplifier)
    , typeFunctionRuntime(typeFunctionRuntime)
    , constraintSet{rootScope}
    , constraints(std::move(constraints))
    , scopeToFunction(scopeToFunction)
    , rootScope(rootScope)
    , currentModuleName(std::move(moduleName))
    , dfg(dfg)
    , moduleResolver(moduleResolver)
    , requireCycles(std::move(requireCycles))
    , logger(logger)
    , limits(std::move(limits))
    , opts{/*exhaustive*/ true}
{
    initFreeTypeTracking();
}

void ConstraintSolver::randomize(unsigned seed)
{
    if (unsolvedConstraints.empty())
        return;

    unsigned int rng = seed;

    for (size_t i = unsolvedConstraints.size() - 1; i > 0; --i)
    {
        // Fisher-Yates shuffle
        size_t j = rng % (i + 1);

        std::swap(unsolvedConstraints[i], unsolvedConstraints[j]);

        // LCG RNG, constants from Numerical Recipes
        // This may occasionally result in skewed shuffles due to distribution properties, but this is a debugging tool so it should be good enough
        rng = rng * 1664525 + 1013904223;
    }
}

void ConstraintSolver::run()
{
    LUAU_TIMETRACE_SCOPE("ConstraintSolver::run", "Typechecking");

    if (isDone())
        return;

    if (FFlag::DebugLuauLogSolver)
    {
        printf(
            "Starting solver for module %s (%s)\n", moduleResolver->getHumanReadableModuleName(currentModuleName).c_str(), currentModuleName.c_str()
        );
        dump(this, opts);
        printf("Bindings:\n");
        dumpBindings(rootScope, opts);
    }

    if (logger)
    {
        logger->captureInitialSolverState(rootScope, unsolvedConstraints);
    }

    // Free types that have no constraints at all can be generalized right away.
    if (FFlag::LuauEagerGeneralization3)
    {
        for (TypeId ty : constraintSet.freeTypes)
        {
            if (auto it = mutatedFreeTypeToConstraint.find(ty); it == mutatedFreeTypeToConstraint.end() || it->second.empty())
                generalizeOneType(ty);
        }
    }

    constraintSet.freeTypes.clear();

    auto runSolverPass = [&](bool force)
    {
        bool progress = false;

        size_t i = 0;
        while (i < unsolvedConstraints.size())
        {
            NotNull<const Constraint> c = unsolvedConstraints[i];
            if (!force && isBlocked(c))
            {
                ++i;
                continue;
            }

            if (limits.finishTime && TimeTrace::getClock() > *limits.finishTime)
                throwTimeLimitError();
            if (limits.cancellationToken && limits.cancellationToken->requested())
                throwUserCancelError();

            std::string saveMe = FFlag::DebugLuauLogSolver ? toString(*c, opts) : std::string{};
            StepSnapshot snapshot;

            if (logger)
            {
                snapshot = logger->prepareStepSnapshot(rootScope, c, force, unsolvedConstraints);
            }

            if (FFlag::DebugLuauAssertOnForcedConstraint)
                LUAU_ASSERT(!force);

            bool success = tryDispatch(c, force);

            progress |= success;

            if (success)
            {
                unblock(c);
                unsolvedConstraints.erase(unsolvedConstraints.begin() + ptrdiff_t(i));

                const auto maybeMutated = maybeMutatedFreeTypes.find(c);
                if (maybeMutated != maybeMutatedFreeTypes.end())
                {
                    DenseHashSet<TypeId> seen{nullptr};
                    for (auto ty : maybeMutated->second)
                    {
                        // There is a high chance that this type has been rebound
                        // across blocked types, rebound free types, pending
                        // expansion types, etc, so we need to follow it.
                        ty = follow(ty);

                        if (FFlag::LuauEagerGeneralization3)
                        {
                            if (seen.contains(ty))
                                continue;
                            seen.insert(ty);
                        }

                        size_t& refCount = unresolvedConstraints[ty];
                        if (refCount > 0)
                            refCount -= 1;

                        // We have two constraints that are designed to wait for the
                        // refCount on a free type to be equal to 1: the
                        // PrimitiveTypeConstraint and ReduceConstraint. We
                        // therefore wake any constraint waiting for a free type's
                        // refcount to be 1 or 0.
                        if (refCount <= 1)
                            unblock(ty, Location{});

                        if (FFlag::LuauEagerGeneralization3 && refCount == 0)
                            generalizeOneType(ty);
                    }
                }

                if (logger)
                {
                    logger->commitStepSnapshot(snapshot);
                }

                if (FFlag::DebugLuauLogSolver)
                {
                    if (force)
                        printf("Force ");
                    printf("Dispatched\n\t%s\n", saveMe.c_str());

                    if (force)
                    {
                        printf("Blocked on:\n");

                        for (const auto& [bci, cv] : blocked)
                        {
                            if (end(cv) == std::find(begin(cv), end(cv), c))
                                continue;

                            if (auto bty = get_if<TypeId>(&bci))
                                printf("\tType %s\n", toString(*bty, opts).c_str());
                            else if (auto btp = get_if<TypePackId>(&bci))
                                printf("\tPack %s\n", toString(*btp, opts).c_str());
                            else if (auto cc = get_if<const Constraint*>(&bci))
                                printf("\tCons %s\n", toString(**cc, opts).c_str());
                            else
                                LUAU_ASSERT(!"Unreachable??");
                        }
                    }

                    dump(this, opts);
                }
            }
            else
                ++i;

            if (force && success)
                return true;
        }

        return progress;
    };

    bool progress = false;
    do
    {
        progress = runSolverPass(false);
        if (!progress)
            progress |= runSolverPass(true);
    } while (progress);

    if (!unsolvedConstraints.empty())
        reportError(ConstraintSolvingIncompleteError{}, Location{});

    // After we have run all the constraints, type functions should be generalized
    // At this point, we can try to perform one final simplification to suss out
    // whether type functions are truly uninhabited or if they can reduce

    finalizeTypeFunctions();

    if (FFlag::DebugLuauLogSolver || FFlag::DebugLuauLogBindings)
        dumpBindings(rootScope, opts);

    if (logger)
    {
        logger->captureFinalSolverState(rootScope, unsolvedConstraints);
    }
}

void ConstraintSolver::finalizeTypeFunctions()
{
    // At this point, we've generalized. Let's try to finish reducing as much as we can, we'll leave warning to the typechecker
    for (auto [t, constraint] : typeFunctionsToFinalize)
    {
        TypeId ty = follow(t);
        if (get<TypeFunctionInstanceType>(ty))
        {
            FunctionGraphReductionResult result =
                reduceTypeFunctions(t, constraint->location, TypeFunctionContext{NotNull{this}, constraint->scope, NotNull{constraint}}, true);

            for (TypeId r : result.reducedTypes)
                unblock(r, constraint->location);
            for (TypePackId r : result.reducedPacks)
                unblock(r, constraint->location);
        }
    }
}

bool ConstraintSolver::isDone() const
{
    return unsolvedConstraints.empty();
}

struct TypeSearcher : TypeVisitor
{
    TypeId needle;
    Polarity current = Polarity::Positive;

    size_t count = 0;
    Polarity result = Polarity::None;

    explicit TypeSearcher(TypeId needle)
        : TypeSearcher(needle, Polarity::Positive)
    {
    }

    explicit TypeSearcher(TypeId needle, Polarity initialPolarity)
        : needle(needle)
        , current(initialPolarity)
    {
    }

    bool visit(TypeId ty) override
    {
        if (ty == needle)
        {
            ++count;
            result = Polarity(size_t(result) | size_t(current));
        }

        return true;
    }

    void flip()
    {
        switch (current)
        {
        case Polarity::Positive:
            current = Polarity::Negative;
            break;
        case Polarity::Negative:
            current = Polarity::Positive;
            break;
        default:
            break;
        }
    }

    bool visit(TypeId ty, const FunctionType& ft) override
    {
        flip();
        traverse(ft.argTypes);

        flip();
        traverse(ft.retTypes);

        return false;
    }

    // bool visit(TypeId ty, const TableType& tt) override
    // {

    // }

    bool visit(TypeId ty, const ExternType&) override
    {
        return false;
    }
};

void ConstraintSolver::initFreeTypeTracking()
{
    for (NotNull<Constraint> c : this->constraints)
    {
        unsolvedConstraints.emplace_back(c);

        auto maybeMutatedTypesPerConstraint = c->getMaybeMutatedFreeTypes();
        for (auto ty : maybeMutatedTypesPerConstraint)
        {
            auto [refCount, _] = unresolvedConstraints.try_insert(ty, 0);
            refCount += 1;

            if (FFlag::LuauEagerGeneralization3)
            {
                auto [it, fresh] = mutatedFreeTypeToConstraint.try_emplace(ty, nullptr);
                it->second.insert(c.get());
            }
        }
        maybeMutatedFreeTypes.emplace(c, maybeMutatedTypesPerConstraint);

        for (NotNull<const Constraint> dep : c->dependencies)
        {
            block(dep, c);
        }
    }
}

void ConstraintSolver::generalizeOneType(TypeId ty)
{
    ty = follow(ty);
    const FreeType* freeTy = get<FreeType>(ty);

    std::string saveme = toString(ty, opts);

    // Some constraints (like prim) will also replace a free type with something
    // concrete. If so, our work is already done.
    if (!freeTy)
        return;

    TypeId* functionType = scopeToFunction->find(freeTy->scope);
    if (!functionType)
        return;

    std::optional<TypeId> resultTy = generalize(arena, builtinTypes, NotNull{freeTy->scope}, generalizedTypes, *functionType, ty);

    if (FFlag::DebugLuauLogSolver)
    {
        printf(
            "Eagerly generalized %s (now %s)\n\tin function %s\n",
            saveme.c_str(),
            toString(ty, opts).c_str(),
            toString(resultTy.value_or(*functionType), opts).c_str()
        );
    }
}

void ConstraintSolver::bind(NotNull<const Constraint> constraint, TypeId ty, TypeId boundTo)
{
    LUAU_ASSERT(get<BlockedType>(ty) || get<FreeType>(ty) || get<PendingExpansionType>(ty));
    LUAU_ASSERT(canMutate(ty, constraint));

    boundTo = follow(boundTo);
    if (get<BlockedType>(ty) && ty == boundTo)
    {
        emplace<FreeType>(
            constraint, ty, constraint->scope, builtinTypes->neverType, builtinTypes->unknownType, Polarity::Mixed
        ); // FIXME?  Is this the right polarity?

        if (FFlag::LuauEagerGeneralization3)
            trackInteriorFreeType(constraint->scope, ty);

        return;
    }

    shiftReferences(ty, boundTo);
    emplaceType<BoundType>(asMutable(ty), boundTo);
    unblock(ty, constraint->location);
}

void ConstraintSolver::bind(NotNull<const Constraint> constraint, TypePackId tp, TypePackId boundTo)
{
    LUAU_ASSERT(get<BlockedTypePack>(tp) || get<FreeTypePack>(tp));
    LUAU_ASSERT(canMutate(tp, constraint));

    boundTo = follow(boundTo);
    LUAU_ASSERT(tp != boundTo);

    emplaceTypePack<BoundTypePack>(asMutable(tp), boundTo);
    unblock(tp, constraint->location);
}

template<typename T, typename... Args>
void ConstraintSolver::emplace(NotNull<const Constraint> constraint, TypeId ty, Args&&... args)
{
    static_assert(!std::is_same_v<T, BoundType>, "cannot use `emplace<BoundType>`! use `bind`");

    LUAU_ASSERT(get<BlockedType>(ty) || get<FreeType>(ty) || get<PendingExpansionType>(ty));
    LUAU_ASSERT(canMutate(ty, constraint));

    emplaceType<T>(asMutable(ty), std::forward<Args>(args)...);
    unblock(ty, constraint->location);
}

template<typename T, typename... Args>
void ConstraintSolver::emplace(NotNull<const Constraint> constraint, TypePackId tp, Args&&... args)
{
    static_assert(!std::is_same_v<T, BoundTypePack>, "cannot use `emplace<BoundTypePack>`! use `bind`");

    LUAU_ASSERT(get<BlockedTypePack>(tp) || get<FreeTypePack>(tp));
    LUAU_ASSERT(canMutate(tp, constraint));

    emplaceTypePack<T>(asMutable(tp), std::forward<Args>(args)...);
    unblock(tp, constraint->location);
}

bool ConstraintSolver::tryDispatch(NotNull<const Constraint> constraint, bool force)
{
    if (!force && isBlocked(constraint))
        return false;

    bool success = false;

    if (auto sc = get<SubtypeConstraint>(*constraint))
        success = tryDispatch(*sc, constraint);
    else if (auto psc = get<PackSubtypeConstraint>(*constraint))
        success = tryDispatch(*psc, constraint);
    else if (auto gc = get<GeneralizationConstraint>(*constraint))
        success = tryDispatch(*gc, constraint);
    else if (auto ic = get<IterableConstraint>(*constraint))
        success = tryDispatch(*ic, constraint, force);
    else if (auto nc = get<NameConstraint>(*constraint))
        success = tryDispatch(*nc, constraint);
    else if (auto taec = get<TypeAliasExpansionConstraint>(*constraint))
        success = tryDispatch(*taec, constraint);
    else if (auto fcc = get<FunctionCallConstraint>(*constraint))
        success = tryDispatch(*fcc, constraint, force);
    else if (auto fcc = get<FunctionCheckConstraint>(*constraint))
        success = tryDispatch(*fcc, constraint);
    else if (auto tcc = get<TableCheckConstraint>(*constraint))
        success = tryDispatch(*tcc, constraint);
    else if (auto fcc = get<PrimitiveTypeConstraint>(*constraint))
        success = tryDispatch(*fcc, constraint);
    else if (auto hpc = get<HasPropConstraint>(*constraint))
        success = tryDispatch(*hpc, constraint);
    else if (auto spc = get<HasIndexerConstraint>(*constraint))
        success = tryDispatch(*spc, constraint);
    else if (auto uc = get<AssignPropConstraint>(*constraint))
        success = tryDispatch(*uc, constraint);
    else if (auto uc = get<AssignIndexConstraint>(*constraint))
        success = tryDispatch(*uc, constraint);
    else if (auto uc = get<UnpackConstraint>(*constraint))
        success = tryDispatch(*uc, constraint);
    else if (auto rc = get<ReduceConstraint>(*constraint))
        success = tryDispatch(*rc, constraint, force);
    else if (auto rpc = get<ReducePackConstraint>(*constraint))
        success = tryDispatch(*rpc, constraint, force);
    else if (auto eqc = get<EqualityConstraint>(*constraint))
        success = tryDispatch(*eqc, constraint);
    else if (auto sc = get<SimplifyConstraint>(*constraint))
        success = tryDispatch(*sc, constraint);
    else
        LUAU_ASSERT(false);

    return success;
}

bool ConstraintSolver::tryDispatch(const SubtypeConstraint& c, NotNull<const Constraint> constraint)
{
    if (isBlocked(c.subType))
        return block(c.subType, constraint);
    else if (isBlocked(c.superType))
        return block(c.superType, constraint);

    unify(constraint, c.subType, c.superType);

    return true;
}

bool ConstraintSolver::tryDispatch(const PackSubtypeConstraint& c, NotNull<const Constraint> constraint)
{
    if (isBlocked(c.subPack))
        return block(c.subPack, constraint);
    else if (isBlocked(c.superPack))
        return block(c.superPack, constraint);

    unify(constraint, c.subPack, c.superPack);

    return true;
}

bool ConstraintSolver::tryDispatch(const GeneralizationConstraint& c, NotNull<const Constraint> constraint)
{
    TypeId generalizedType = follow(c.generalizedType);

    if (isBlocked(c.sourceType))
        return block(c.sourceType, constraint);
    else if (get<PendingExpansionType>(generalizedType))
        return block(generalizedType, constraint);

    std::optional<TypeId> generalizedTy = generalize(NotNull{arena}, builtinTypes, constraint->scope, generalizedTypes, c.sourceType);
    if (!generalizedTy)
        reportError(CodeTooComplex{}, constraint->location);

    if (generalizedTy)
    {
        pruneUnnecessaryGenerics(arena, builtinTypes, constraint->scope, generalizedTypes, *generalizedTy);
        if (get<BlockedType>(generalizedType))
            bind(constraint, generalizedType, *generalizedTy);
        else
            unify(constraint, generalizedType, *generalizedTy);

        if (FunctionType* fty = getMutable<FunctionType>(follow(generalizedType)))
        {
            if (c.hasDeprecatedAttribute)
                fty->isDeprecatedFunction = true;
        }
    }
    else
    {
        reportError(CodeTooComplex{}, constraint->location);
        bind(constraint, c.generalizedType, builtinTypes->errorRecoveryType());
    }

    // We check if this member is initialized and then access it, but
    // clang-tidy doesn't understand this is safe.
    if (constraint->scope->interiorFreeTypes)
    {
        for (TypeId ty : *constraint->scope->interiorFreeTypes) // NOLINT(bugprone-unchecked-optional-access)
        {
            if (FFlag::LuauEagerGeneralization3)
            {
                ty = follow(ty);
                if (auto freeTy = get<FreeType>(ty))
                {
                    GeneralizationParams<TypeId> params;
                    params.foundOutsideFunctions = true;
                    params.useCount = 1;
                    params.polarity = freeTy->polarity;
                    GeneralizationResult<TypeId> res = generalizeType(arena, builtinTypes, constraint->scope, ty, params);
                    if (res.resourceLimitsExceeded)
                        reportError(CodeTooComplex{}, constraint->scope->location); // FIXME: We don't have a very good location for this.
                }
                else if (get<TableType>(ty))
                    sealTable(constraint->scope, ty);
            }
            else
                generalize(NotNull{arena}, builtinTypes, constraint->scope, generalizedTypes, ty);
        }
    }

    if (FFlag::LuauEagerGeneralization3)
    {
        if (constraint->scope->interiorFreeTypePacks)
        {
            for (TypePackId tp : *constraint->scope->interiorFreeTypePacks) // NOLINT(bugprone-unchecked-optional-access)
            {
                tp = follow(tp);
                if (auto freeTp = get<FreeTypePack>(tp))
                {
                    GeneralizationParams<TypePackId> params;
                    params.foundOutsideFunctions = true;
                    params.useCount = 1;
                    params.polarity = freeTp->polarity;
                    LUAU_ASSERT(isKnown(params.polarity));
                    generalizeTypePack(arena, builtinTypes, constraint->scope, tp, params);
                }
            }
        }
    }

    if (FFlag::LuauEagerGeneralization3)
    {
        if (c.noGenerics)
        {
            if (auto ft = getMutable<FunctionType>(c.sourceType))
            {
                for (TypeId gen : ft->generics)
                    asMutable(gen)->ty.emplace<BoundType>(builtinTypes->unknownType);
                ft->generics.clear();

                for (TypePackId gen : ft->genericPacks)
                    asMutable(gen)->ty.emplace<BoundTypePack>(builtinTypes->unknownTypePack);
                ft->genericPacks.clear();
            }
        }
    }

    return true;
}

bool ConstraintSolver::tryDispatch(const IterableConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    /*
     * for .. in loops can play out in a bunch of different ways depending on
     * the shape of iteratee.
     *
     * iteratee might be:
     *  * (nextFn)
     *  * (nextFn, table)
     *  * (nextFn, table, firstIndex)
     *  * table with a metatable and __index
     *  * table with a metatable and __call but no __index (if the metatable has
     *    both, __index takes precedence)
     *  * table with an indexer but no __index or __call (or no metatable)
     *
     * To dispatch this constraint, we need first to know enough about iteratee
     * to figure out which of the above shapes we are actually working with.
     *
     * If `force` is true and we still do not know, we must flag a warning. Type
     * functions are the fix for this.
     *
     * Since we need to know all of this stuff about the types of the iteratee,
     * we have no choice but for ConstraintSolver to also be the thing that
     * applies constraints to the types of the iterators.
     */

    auto block_ = [&](auto&& t)
    {
        if (force)
        {
            // If we haven't figured out the type of the iteratee by now,
            // there's nothing we can do.
            return true;
        }

        block(t, constraint);
        return false;
    };

    TypePack iterator = extendTypePack(*arena, builtinTypes, c.iterator, 3);
    if (iterator.head.size() < 3 && iterator.tail && isBlocked(*iterator.tail))
        return block_(*iterator.tail);

    {
        bool blocked = false;
        for (TypeId t : iterator.head)
        {
            if (isBlocked(t))
            {
                block(t, constraint);
                blocked = true;
            }
        }

        if (blocked)
            return false;
    }

    if (0 == iterator.head.size())
    {
        for (TypeId ty : c.variables)
            unify(constraint, builtinTypes->errorRecoveryType(), ty);

        return true;
    }

    TypeId nextTy = follow(iterator.head[0]);
    if (get<FreeType>(nextTy))
    {
        TypeId keyTy = freshType(arena, builtinTypes, constraint->scope, Polarity::Mixed);
        TypeId valueTy = freshType(arena, builtinTypes, constraint->scope, Polarity::Mixed);
        trackInteriorFreeType(constraint->scope, keyTy);
        trackInteriorFreeType(constraint->scope, valueTy);
        TypeId tableTy =
            arena->addType(TableType{TableType::Props{}, TableIndexer{keyTy, valueTy}, TypeLevel{}, constraint->scope, TableState::Free});

        trackInteriorFreeType(constraint->scope, tableTy);

        unify(constraint, nextTy, tableTy);

        auto it = begin(c.variables);
        auto endIt = end(c.variables);

        if (it != endIt)
        {
            bind(constraint, *it, keyTy);
            ++it;
        }
        if (it != endIt)
        {
            bind(constraint, *it, valueTy);
            ++it;
        }

        while (it != endIt)
        {
            bind(constraint, *it, builtinTypes->nilType);
            ++it;
        }

        return true;
    }

    if (get<FunctionType>(nextTy))
    {
        TypeId tableTy = builtinTypes->nilType;
        if (iterator.head.size() >= 2)
            tableTy = iterator.head[1];

        return tryDispatchIterableFunction(nextTy, tableTy, c, constraint);
    }

    else
        return tryDispatchIterableTable(iterator.head[0], c, constraint, force);

    return true;
}

bool ConstraintSolver::tryDispatch(const NameConstraint& c, NotNull<const Constraint> constraint)
{
    if (isBlocked(c.namedType))
        return block(c.namedType, constraint);

    TypeId target = follow(c.namedType);

    if (target->persistent || target->owningArena != arena)
        return true;

    if (TableType* ttv = getMutable<TableType>(target))
    {
        if (c.synthetic && !ttv->name)
            ttv->syntheticName = c.name;
        else
        {
            ttv->name = c.name;
            ttv->instantiatedTypeParams = c.typeParameters;
            ttv->instantiatedTypePackParams = c.typePackParameters;
        }
    }
    else if (MetatableType* mtv = getMutable<MetatableType>(target))
        mtv->syntheticName = c.name;
    else if (get<IntersectionType>(target) || get<UnionType>(target))
    {
        // nothing (yet)
    }

    return true;
}

struct InfiniteTypeFinder : TypeOnceVisitor
{
    ConstraintSolver* solver;
    const InstantiationSignature& signature;
    NotNull<Scope> scope;
    bool foundInfiniteType = false;

    explicit InfiniteTypeFinder(ConstraintSolver* solver, const InstantiationSignature& signature, NotNull<Scope> scope)
        : solver(solver)
        , signature(signature)
        , scope(scope)
    {
    }

    bool visit(TypeId ty, const PendingExpansionType& petv) override
    {
        std::optional<TypeFun> tf =
            (petv.prefix) ? scope->lookupImportedType(petv.prefix->value, petv.name.value) : scope->lookupType(petv.name.value);

        if (!tf.has_value())
            return true;

        auto [typeArguments, packArguments] = saturateArguments(solver->arena, solver->builtinTypes, *tf, petv.typeArguments, petv.packArguments);

        if (follow(tf->type) == follow(signature.fn.type) && (signature.arguments != typeArguments || signature.packArguments != packArguments))
        {
            foundInfiniteType = true;
            return false;
        }

        return true;
    }
};

bool ConstraintSolver::tryDispatch(const TypeAliasExpansionConstraint& c, NotNull<const Constraint> constraint)
{
    const PendingExpansionType* petv = get<PendingExpansionType>(follow(c.target));
    if (!petv)
    {
        unblock(c.target, constraint->location); // TODO: do we need this? any re-entrancy?
        return true;
    }

    auto bindResult = [this, &c, constraint](TypeId result)
    {
        auto cTarget = follow(c.target);
        LUAU_ASSERT(get<PendingExpansionType>(cTarget));
        // We do this check here to ensure that we don't bind an alias to itself
        if (FFlag::LuauGuardAgainstMalformedTypeAliasExpansion2 && occursCheck(cTarget, result))
        {
            reportError(OccursCheckFailed{}, constraint->location);
            bind(constraint, cTarget, builtinTypes->errorRecoveryType());
        }
        else
        {
            shiftReferences(cTarget, result);
            bind(constraint, cTarget, result);
        }
    };

    std::optional<TypeFun> tf = (petv->prefix) ? constraint->scope->lookupImportedType(petv->prefix->value, petv->name.value)
                                               : constraint->scope->lookupType(petv->name.value);

    if (!tf.has_value())
    {
        reportError(UnknownSymbol{petv->name.value, UnknownSymbol::Context::Type}, constraint->location);
        bindResult(errorRecoveryType());
        return true;
    }

    // Adding ReduceConstraint on type function for the constraint solver
    if (auto typeFn = get<TypeFunctionInstanceType>(follow(tf->type)))
        pushConstraint(NotNull(constraint->scope.get()), constraint->location, ReduceConstraint{tf->type});

    // Due to how pending expansion types and TypeFun's are created
    // If this check passes, we have created a cyclic / corecursive type alias
    // of size 0
    TypeId lhs = follow(c.target);
    TypeId rhs = tf->type;
    if (occursCheck(lhs, rhs))
    {
        reportError(OccursCheckFailed{}, constraint->location);
        bindResult(errorRecoveryType());
        return true;
    }

    // If there are no parameters to the type function we can just use the type directly
    if (tf->typeParams.empty() && tf->typePackParams.empty())
    {
        bindResult(tf->type);
        return true;
    }

    auto [typeArguments, packArguments] = saturateArguments(arena, builtinTypes, *tf, petv->typeArguments, petv->packArguments);

    bool sameTypes = std::equal(
        typeArguments.begin(),
        typeArguments.end(),
        tf->typeParams.begin(),
        tf->typeParams.end(),
        [](auto&& itp, auto&& p)
        {
            return itp == p.ty;
        }
    );

    bool samePacks = std::equal(
        packArguments.begin(),
        packArguments.end(),
        tf->typePackParams.begin(),
        tf->typePackParams.end(),
        [](auto&& itp, auto&& p)
        {
            return itp == p.tp;
        }
    );

    // If we're instantiating the type with its generic saturatedTypeArguments we are
    // performing the identity substitution. We can just short-circuit and bind
    // to the TypeFun's type.
    if (sameTypes && samePacks)
    {
        bindResult(tf->type);
        return true;
    }

    InstantiationSignature signature{
        *tf,
        typeArguments,
        packArguments,
    };

    // If we use the same signature, we don't need to bother trying to
    // instantiate the alias again, since the instantiation should be
    // deterministic.
    if (TypeId* cached = instantiatedAliases.find(signature))
    {
        bindResult(*cached);
        return true;
    }

    // In order to prevent infinite types from being expanded and causing us to
    // cycle infinitely, we need to scan the type function for cases where we
    // expand the same alias with different type saturatedTypeArguments. See
    // https://github.com/luau-lang/luau/pull/68 for the RFC responsible for
    // this. This is a little nicer than using a recursion limit because we can
    // catch the infinite expansion before actually trying to expand it.
    InfiniteTypeFinder itf{this, signature, constraint->scope};
    itf.traverse(tf->type);

    if (itf.foundInfiniteType)
    {
        // TODO (CLI-56761): Report an error.
        bindResult(errorRecoveryType());
        reportError(GenericError{"Recursive type being used with different parameters"}, constraint->location);
        return true;
    }

    ApplyTypeFunction applyTypeFunction{arena};
    for (size_t i = 0; i < typeArguments.size(); ++i)
    {
        applyTypeFunction.typeArguments[tf->typeParams[i].ty] = typeArguments[i];
    }

    for (size_t i = 0; i < packArguments.size(); ++i)
    {
        applyTypeFunction.typePackArguments[tf->typePackParams[i].tp] = packArguments[i];
    }

    std::optional<TypeId> maybeInstantiated = applyTypeFunction.substitute(tf->type);
    // Note that ApplyTypeFunction::encounteredForwardedType is never set in
    // DCR, because we do not use free types for forward-declared generic
    // aliases.

    if (!maybeInstantiated.has_value())
    {
        // TODO (CLI-56761): Report an error.
        bindResult(errorRecoveryType());
        return true;
    }

    TypeId instantiated = *maybeInstantiated;
    TypeId target = follow(instantiated);

    // The application is not recursive, so we need to queue up application of
    // any child type function instantiations within the result in order for it
    // to be complete.
    InstantiationQueuer queuer{constraint->scope, constraint->location, this};
    queuer.traverse(target);

    if (target->persistent || target->owningArena != arena)
    {
        bindResult(target);
        return true;
    }

    // Type function application will happily give us the exact same type if
    // there are e.g. generic saturatedTypeArguments that go unused.
    const TableType* tfTable = getTableType(tf->type);

    bool needsClone = follow(tf->type) == target || (tfTable != nullptr && tfTable == getTableType(target)) ||
                      std::any_of(
                          typeArguments.begin(),
                          typeArguments.end(),
                          [&](const auto& other)
                          {
                              return other == target;
                          }
                      );

    // Only tables have the properties we're trying to set.
    TableType* ttv = getMutableTableType(target);

    if (ttv)
    {
        if (needsClone)
        {
            // Substitution::clone is a shallow clone. If this is a
            // metatable type, we want to mutate its table, so we need to
            // explicitly clone that table as well. If we don't, we will
            // mutate another module's type surface and cause a
            // use-after-free.
            if (get<MetatableType>(target))
            {
                instantiated = applyTypeFunction.clone(target);
                MetatableType* mtv = getMutable<MetatableType>(instantiated);
                mtv->table = applyTypeFunction.clone(mtv->table);
                ttv = getMutable<TableType>(mtv->table);
            }
            else if (get<TableType>(target))
            {
                instantiated = applyTypeFunction.clone(target);
                ttv = getMutable<TableType>(instantiated);
            }

            target = follow(instantiated);
        }

        // This is a new type - redefine the location.
        ttv->definitionLocation = constraint->location;
        ttv->definitionModuleName = currentModuleName;

        ttv->instantiatedTypeParams = typeArguments;
        ttv->instantiatedTypePackParams = packArguments;
    }

    bindResult(target);

    instantiatedAliases[signature] = target;

    return true;
}

void ConstraintSolver::fillInDiscriminantTypes(NotNull<const Constraint> constraint, const std::vector<std::optional<TypeId>>& discriminantTypes)
{
    for (std::optional<TypeId> ty : discriminantTypes)
    {
        if (!ty)
            continue;

        if (isBlocked(*ty))
            // We bind any unused discriminants to the `*no-refine*` type indicating that it can be safely ignored.
            emplaceType<BoundType>(asMutable(follow(*ty)), builtinTypes->noRefineType);

        // We also need to unconditionally unblock these types, otherwise
        // you end up with funky looking "Blocked on *no-refine*."
        unblock(*ty, constraint->location);
    }
}

bool ConstraintSolver::tryDispatch(const FunctionCallConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    TypeId fn = follow(c.fn);
    TypePackId argsPack = follow(c.argsPack);
    TypePackId result = follow(c.result);

    if (FFlag::LuauEagerGeneralization3)
    {
        if (isBlocked(fn))
            return block(c.fn, constraint);
    }
    else
    {
        if (isBlocked(fn) || hasUnresolvedConstraints(fn))
        {
            return block(c.fn, constraint);
        }
    }

    if (get<AnyType>(fn))
    {
        emplaceTypePack<BoundTypePack>(asMutable(c.result), builtinTypes->anyTypePack);
        unblock(c.result, constraint->location);
        fillInDiscriminantTypes(constraint, c.discriminantTypes);
        return true;
    }

    // if we're calling an error type, the result is an error type, and that's that.
    if (get<ErrorType>(fn))
    {
        bind(constraint, c.result, builtinTypes->errorRecoveryTypePack());
        fillInDiscriminantTypes(constraint, c.discriminantTypes);
        return true;
    }

    if (get<NeverType>(fn))
    {
        bind(constraint, c.result, builtinTypes->neverTypePack);
        fillInDiscriminantTypes(constraint, c.discriminantTypes);
        return true;
    }

    auto [argsHead, argsTail] = flatten(argsPack);

    bool blocked = false;
    for (TypeId t : argsHead)
    {
        if (isBlocked(t))
        {
            block(t, constraint);
            blocked = true;
        }
    }

    if (argsTail && isBlocked(*argsTail))
    {
        block(*argsTail, constraint);
        blocked = true;
    }

    if (blocked)
        return false;

    auto collapse = [](const auto* t) -> std::optional<TypeId>
    {
        auto it = begin(t);
        auto endIt = end(t);

        LUAU_ASSERT(it != endIt);
        TypeId fst = follow(*it);
        while (it != endIt)
        {
            if (follow(*it) != fst)
                return std::nullopt;
            ++it;
        }

        return fst;
    };

    // Sometimes the `fn` type is a union/intersection, but whose constituents are all the same pointer.
    if (auto ut = get<UnionType>(fn))
        fn = collapse(ut).value_or(fn);
    else if (auto it = get<IntersectionType>(fn))
        fn = collapse(it).value_or(fn);

    // We don't support magic __call metamethods.
    if (std::optional<TypeId> callMm = findMetatableEntry(builtinTypes, errors, fn, "__call", constraint->location))
    {
        if (isBlocked(*callMm))
            return block(*callMm, constraint);

        argsHead.insert(argsHead.begin(), fn);

        if (argsTail && isBlocked(*argsTail))
            return block(*argsTail, constraint);

        argsPack = arena->addTypePack(TypePack{std::move(argsHead), argsTail});
        fn = follow(*callMm);
        emplace<FreeTypePack>(constraint, c.result, constraint->scope, Polarity::Positive);
        trackInteriorFreeTypePack(constraint->scope, c.result);
    }
    else
    {
        const FunctionType* ftv = get<FunctionType>(fn);
        bool usedMagic = false;

        if (ftv)
        {
            if (ftv->magic && c.callSite)
            {
                usedMagic = ftv->magic->infer(MagicFunctionCallContext{NotNull{this}, constraint, NotNull{c.callSite}, c.argsPack, result});
                ftv->magic->refine(MagicRefinementContext{constraint->scope, c.callSite, c.discriminantTypes});
            }
        }

        if (!usedMagic)
        {
            emplace<FreeTypePack>(constraint, c.result, constraint->scope, Polarity::Positive);
            trackInteriorFreeTypePack(constraint->scope, c.result);
        }
    }

    fillInDiscriminantTypes(constraint, c.discriminantTypes);

    OverloadResolver resolver{
        builtinTypes,
        NotNull{arena},
        simplifier,
        normalizer,
        typeFunctionRuntime,
        constraint->scope,
        NotNull{&iceReporter},
        NotNull{&limits},
        constraint->location
    };
    auto [status, overload] = resolver.selectOverload(fn, argsPack, /*useFreeTypeBounds*/ force);
    TypeId overloadToUse = fn;
    if (status == OverloadResolver::Analysis::Ok)
        overloadToUse = overload;

    TypeId inferredTy = arena->addType(FunctionType{TypeLevel{}, argsPack, c.result});
    Unifier2 u2{NotNull{arena}, builtinTypes, constraint->scope, NotNull{&iceReporter}};

    const bool occursCheckPassed = u2.unify(overloadToUse, inferredTy);

    if (FFlag::LuauEagerGeneralization3)
    {
        for (TypeId freeTy : u2.newFreshTypes)
            trackInteriorFreeType(constraint->scope, freeTy);
        for (TypePackId freeTp : u2.newFreshTypePacks)
            trackInteriorFreeTypePack(constraint->scope, freeTp);
    }

    if (!u2.genericSubstitutions.empty() || !u2.genericPackSubstitutions.empty())
    {
        std::optional<TypePackId> subst = instantiate2(arena, std::move(u2.genericSubstitutions), std::move(u2.genericPackSubstitutions), result);
        if (!subst)
        {
            reportError(CodeTooComplex{}, constraint->location);
            result = builtinTypes->errorTypePack;
        }
        else
            result = *subst;

        if (c.result != result)
            emplaceTypePack<BoundTypePack>(asMutable(c.result), result);

        if (FFlag::LuauClipVariadicAnysFromArgsToGenericFuncs2)
        {
            FunctionType* inferredFuncTy = getMutable<FunctionType>(inferredTy);
            LUAU_ASSERT(inferredFuncTy);

            // Strip variadic anys from the argTypes of any functionType arguments
            const auto [argsHead, argsTail] = flatten(inferredFuncTy->argTypes);
            TypePack clippedArgs = {{}, argsTail};
            bool clippedAny = false;

            for (TypeId t : argsHead)
            {
                const FunctionType* f = get<FunctionType>(follow(t));
                if (!f || !f->argTypes)
                {
                    clippedArgs.head.push_back(t);
                    continue;
                }

                const TypePack* argTp = get<TypePack>(follow(f->argTypes));
                if (!argTp || !argTp->tail)
                {
                    clippedArgs.head.push_back(t);
                    continue;
                }

                if (const VariadicTypePack* argTpTail = get<VariadicTypePack>(follow(argTp->tail));
                    argTpTail && argTpTail->hidden && argTpTail->ty == builtinTypes->anyType)
                {
                    const TypePackId anyLessArgTp = arena->addTypePack(TypePack{argTp->head});
                    // Mint a new FunctionType in case the original came from another module
                    const TypeId newFuncTypeId = arena->addType(FunctionType{anyLessArgTp, f->retTypes});
                    FunctionType* newFunc = getMutable<FunctionType>(newFuncTypeId);
                    newFunc->argNames = f->argNames;
                    clippedArgs.head.push_back(newFuncTypeId);
                    clippedAny = true;
                }
                else
                    clippedArgs.head.push_back(t);
            }

            if (clippedAny)
                inferredFuncTy->argTypes = arena->addTypePack(std::move(clippedArgs));
        }
    }

    for (const auto& [expanded, additions] : u2.expandedFreeTypes)
    {
        for (TypeId addition : additions)
            upperBoundContributors[expanded].emplace_back(constraint->location, addition);
    }

    if (occursCheckPassed && c.callSite)
        (*c.astOverloadResolvedTypes)[c.callSite] = inferredTy;
    else if (!occursCheckPassed)
        reportError(OccursCheckFailed{}, constraint->location);

    InstantiationQueuer queuer{constraint->scope, constraint->location, this};
    queuer.traverse(overloadToUse);
    queuer.traverse(inferredTy);

    // This can potentially contain free types if the return type of
    // `inferredTy` is never unified elsewhere.
    trackInteriorFreeType(constraint->scope, inferredTy);

    unblock(c.result, constraint->location);

    return true;
}

struct ContainsGenerics_DEPRECATED : public TypeOnceVisitor
{
    DenseHashSet<const void*> generics{nullptr};

    bool found = false;

    bool visit(TypeId ty) override
    {
        return !found;
    }

    bool visit(TypeId ty, const GenericType&) override
    {
        found |= generics.contains(ty);
        return true;
    }

    bool visit(TypeId ty, const TypeFunctionInstanceType&) override
    {
        return !found;
    }

    bool visit(TypePackId tp, const GenericTypePack&) override
    {
        found |= generics.contains(tp);
        return !found;
    }

    bool hasGeneric(TypeId ty)
    {
        traverse(ty);
        auto ret = found;
        found = false;
        return ret;
    }
};

namespace
{
struct ReferentialReplacer : Substitution
{
    NotNull<DenseHashMap<TypeId, TypeId>> replacements;
    NotNull<DenseHashMap<TypePackId, TypePackId>> replacementPacks;

    ReferentialReplacer(
        NotNull<TypeArena> arena,
        NotNull<DenseHashMap<TypeId, TypeId>> replacements,
        NotNull<DenseHashMap<TypePackId, TypePackId>> replacementPacks
    )
        : Substitution(TxnLog::empty(), arena)
        , replacements(std::move(replacements))
        , replacementPacks(std::move(replacementPacks))
    {
    }

    bool isDirty(TypeId ty) override
    {
        return replacements->find(ty) != nullptr;
    }

    bool isDirty(TypePackId tp) override
    {
        return replacementPacks->find(tp) != nullptr;
    }

    TypeId clean(TypeId ty) override
    {
        TypeId res = (*replacements)[ty];
        LUAU_ASSERT(res);
        dontTraverseInto(res);
        return res;
    }

    TypePackId clean(TypePackId tp) override
    {
        TypePackId res = (*replacementPacks)[tp];
        LUAU_ASSERT(res);
        dontTraverseInto(res);
        return res;
    }
};
struct ContainsGenerics : public TypeOnceVisitor
{
    NotNull<DenseHashSet<const void*>> generics;

    explicit ContainsGenerics(NotNull<DenseHashSet<const void*>> generics)
        : generics{generics}
    {
    }

    bool found = false;

    bool visit(TypeId ty) override
    {
        return !found;
    }

    bool visit(TypeId ty, const GenericType&) override
    {
        found |= generics->contains(ty);
        return true;
    }

    bool visit(TypeId ty, const TypeFunctionInstanceType&) override
    {
        return !found;
    }

    bool visit(TypePackId tp, const GenericTypePack&) override
    {
        found |= generics->contains(tp);
        return !found;
    }

    static bool hasGeneric(TypeId ty, NotNull<DenseHashSet<const void*>> generics)
    {
        ContainsGenerics cg{generics};
        cg.traverse(ty);
        return cg.found;
    }
};

} // namespace

bool ConstraintSolver::tryDispatch(const FunctionCheckConstraint& c, NotNull<const Constraint> constraint)
{
    TypeId fn = follow(c.fn);
    const TypePackId argsPack = follow(c.argsPack);

    if (isBlocked(fn))
        return block(fn, constraint);

    if (isBlocked(argsPack))
        return true;

    // This is expensive as we need to traverse a (potentially large)
    // literal up front in order to determine if there are any blocked
    // types, otherwise we may run `matchTypeLiteral` multiple times,
    // which right now may fail due to being non-idempotent (it
    // destructively updates the underlying literal type).
    auto blockedTypes = findBlockedArgTypesIn(c.callSite, c.astTypes);
    for (const auto ty : blockedTypes)
    {
        block(ty, constraint);
    }
    if (!blockedTypes.empty())
        return false;

    // We know the type of the function and the arguments it expects to receive.
    // We also know the TypeIds of the actual arguments that will be passed.
    //
    // Bidirectional type checking: Force those TypeIds to be the expected
    // arguments. If something is incoherent, we'll spot it in type checking.
    //
    // Most important detail: If a function argument is a lambda, we also want
    // to force unannotated argument types of that lambda to be the expected
    // types.

    // FIXME: Bidirectional type checking of overloaded functions is not yet supported.
    const FunctionType* ftv = get<FunctionType>(fn);
    if (!ftv)
        return true;

    DenseHashMap<TypeId, TypeId> replacements{nullptr};
    DenseHashMap<TypePackId, TypePackId> replacementPacks{nullptr};

    if (FFlag::LuauAvoidGenericsLeakingDuringFunctionCallCheck)
    {

        DenseHashSet<const void*> genericTypesAndPacks{nullptr};

        Unifier2 u2{arena, builtinTypes, constraint->scope, NotNull{&iceReporter}};

        for (auto generic : ftv->generics)
        {
            // We may see non-generic types here, for example when evaluating a
            // recursive function call.
            if (auto gty = get<GenericType>(follow(generic)))
            {
                replacements[generic] = gty->polarity == Polarity::Negative ? builtinTypes->neverType : builtinTypes->unknownType;
                genericTypesAndPacks.insert(generic);
            }
        }

        for (auto genericPack : ftv->genericPacks)
        {
            replacementPacks[genericPack] = builtinTypes->unknownTypePack;
            genericTypesAndPacks.insert(genericPack);
        }

        const std::vector<TypeId> expectedArgs = flatten(ftv->argTypes).first;
        const std::vector<TypeId> argPackHead = flatten(argsPack).first;

        // If this is a self call, the types will have more elements than the AST call.
        // We don't attempt to perform bidirectional inference on the self type.
        const size_t typeOffset = c.callSite->self ? 1 : 0;

        for (size_t i = 0; i < c.callSite->args.size && i + typeOffset < expectedArgs.size() && i + typeOffset < argPackHead.size(); ++i)
        {
            const TypeId expectedArgTy = follow(expectedArgs[i + typeOffset]);
            const TypeId actualArgTy = follow(argPackHead[i + typeOffset]);
            AstExpr* expr = unwrapGroup(c.callSite->args.data[i]);

            (*c.astExpectedTypes)[expr] = expectedArgTy;

            const auto lambdaTy = get<FunctionType>(actualArgTy);
            const auto expectedLambdaTy = get<FunctionType>(expectedArgTy);
            const auto lambdaExpr = expr->as<AstExprFunction>();

            if (expectedLambdaTy && lambdaTy && lambdaExpr)
            {
                if (ContainsGenerics::hasGeneric(expectedArgTy, NotNull{&genericTypesAndPacks}))
                    continue;

                const std::vector<TypeId> expectedLambdaArgTys = flatten(expectedLambdaTy->argTypes).first;
                const std::vector<TypeId> lambdaArgTys = flatten(lambdaTy->argTypes).first;

                for (size_t j = 0; j < expectedLambdaArgTys.size() && j < lambdaArgTys.size() && j < lambdaExpr->args.size; ++j)
                {
                    if (!lambdaExpr->args.data[j]->annotation && get<FreeType>(follow(lambdaArgTys[j])))
                    {
                        shiftReferences(lambdaArgTys[j], expectedLambdaArgTys[j]);
                        bind(constraint, lambdaArgTys[j], expectedLambdaArgTys[j]);
                    }
                }
            }
            else if (expr->is<AstExprConstantBool>() || expr->is<AstExprConstantString>() || expr->is<AstExprConstantNumber>() ||
                     expr->is<AstExprConstantNil>())
            {
                ReferentialReplacer replacer{arena, NotNull{&replacements}, NotNull{&replacementPacks}};
                if (auto res = replacer.substitute(expectedArgTy))
                    u2.unify(actualArgTy, *res);
                else
                    u2.unify(actualArgTy, expectedArgTy);
            }
            else if (expr->is<AstExprTable>() && !ContainsGenerics::hasGeneric(expectedArgTy, NotNull{&genericTypesAndPacks}))
            {
                Subtyping sp{builtinTypes, arena, simplifier, normalizer, typeFunctionRuntime, NotNull{&iceReporter}};
                std::vector<TypeId> toBlock;
                (void)matchLiteralType(
                    c.astTypes, c.astExpectedTypes, builtinTypes, arena, NotNull{&u2}, NotNull{&sp}, expectedArgTy, actualArgTy, expr, toBlock
                );
                LUAU_ASSERT(toBlock.empty());
            }
        }
    }
    else
    {
        ContainsGenerics_DEPRECATED containsGenerics;

        for (auto generic : ftv->generics)
        {
            replacements[generic] = builtinTypes->unknownType;
            containsGenerics.generics.insert(generic);
        }

        for (auto genericPack : ftv->genericPacks)
        {
            replacementPacks[genericPack] = builtinTypes->unknownTypePack;
            containsGenerics.generics.insert(genericPack);
        }

        const std::vector<TypeId> expectedArgs = flatten(ftv->argTypes).first;
        const std::vector<TypeId> argPackHead = flatten(argsPack).first;

        // If this is a self call, the types will have more elements than the AST call.
        // We don't attempt to perform bidirectional inference on the self type.
        const size_t typeOffset = c.callSite->self ? 1 : 0;

        for (size_t i = 0; i < c.callSite->args.size && i + typeOffset < expectedArgs.size() && i + typeOffset < argPackHead.size(); ++i)
        {
            const TypeId expectedArgTy = follow(expectedArgs[i + typeOffset]);
            const TypeId actualArgTy = follow(argPackHead[i + typeOffset]);
            AstExpr* expr = unwrapGroup(c.callSite->args.data[i]);

            (*c.astExpectedTypes)[expr] = expectedArgTy;

            const FunctionType* lambdaTy = get<FunctionType>(actualArgTy);
            // Generic types are skipped over entirely, for now.
            if (containsGenerics.hasGeneric(expectedArgTy))
            {
                if (!FFlag::LuauClipVariadicAnysFromArgsToGenericFuncs2 || !lambdaTy || !lambdaTy->argTypes)
                    continue;

                const TypePack* argTp = get<TypePack>(follow(lambdaTy->argTypes));
                if (!argTp || !argTp->tail)
                    continue;

                if (const VariadicTypePack* argTpTail = get<VariadicTypePack>(follow(argTp->tail));
                    argTpTail && argTpTail->hidden && argTpTail->ty == builtinTypes->anyType)
                {
                    // Strip variadic any
                    const TypePackId anyLessArgTp = arena->addTypePack(TypePack{argTp->head});
                    const TypeId newFuncTypeId = arena->addType(FunctionType{anyLessArgTp, lambdaTy->retTypes});
                    FunctionType* newFunc = getMutable<FunctionType>(newFuncTypeId);
                    newFunc->argNames = lambdaTy->argNames;
                    (*c.astTypes)[expr] = newFuncTypeId;
                }

                continue;
            }

            const FunctionType* expectedLambdaTy = get<FunctionType>(expectedArgTy);
            const AstExprFunction* lambdaExpr = expr->as<AstExprFunction>();

            if (expectedLambdaTy && lambdaTy && lambdaExpr)
            {
                const std::vector<TypeId> expectedLambdaArgTys = flatten(expectedLambdaTy->argTypes).first;
                const std::vector<TypeId> lambdaArgTys = flatten(lambdaTy->argTypes).first;

                for (size_t j = 0; j < expectedLambdaArgTys.size() && j < lambdaArgTys.size() && j < lambdaExpr->args.size; ++j)
                {
                    if (!lambdaExpr->args.data[j]->annotation && get<FreeType>(follow(lambdaArgTys[j])))
                    {
                        shiftReferences(lambdaArgTys[j], expectedLambdaArgTys[j]);
                        bind(constraint, lambdaArgTys[j], expectedLambdaArgTys[j]);
                    }
                }
            }
            else if (expr->is<AstExprConstantBool>() || expr->is<AstExprConstantString>() || expr->is<AstExprConstantNumber>() ||
                     expr->is<AstExprConstantNil>())
            {
                Unifier2 u2{arena, builtinTypes, constraint->scope, NotNull{&iceReporter}};
                u2.unify(actualArgTy, expectedArgTy);
            }
            else if (expr->is<AstExprTable>())
            {
                Unifier2 u2{arena, builtinTypes, constraint->scope, NotNull{&iceReporter}};
                Subtyping sp{builtinTypes, arena, simplifier, normalizer, typeFunctionRuntime, NotNull{&iceReporter}};
                std::vector<TypeId> toBlock;
                (void)matchLiteralType(
                    c.astTypes, c.astExpectedTypes, builtinTypes, arena, NotNull{&u2}, NotNull{&sp}, expectedArgTy, actualArgTy, expr, toBlock
                );
                LUAU_ASSERT(toBlock.empty());
            }
        }
    }

    return true;
}

bool ConstraintSolver::tryDispatch(const TableCheckConstraint& c, NotNull<const Constraint> constraint)
{
    // This is expensive as we need to traverse a (potentially large)
    // literal up front in order to determine if there are any blocked
    // types, otherwise we may run `matchTypeLiteral` multiple times,
    // which right now may fail due to being non-idempotent (it
    // destructively updates the underlying literal type).
    auto blockedTypes = findBlockedTypesIn(c.table, c.astTypes);
    for (const auto ty : blockedTypes)
    {
        block(ty, constraint);
    }
    if (!blockedTypes.empty())
        return false;

    Unifier2 u2{arena, builtinTypes, constraint->scope, NotNull{&iceReporter}};
    Subtyping sp{builtinTypes, arena, simplifier, normalizer, typeFunctionRuntime, NotNull{&iceReporter}};
    std::vector<TypeId> toBlock;
    (void)matchLiteralType(c.astTypes, c.astExpectedTypes, builtinTypes, arena, NotNull{&u2}, NotNull{&sp}, c.expectedType, c.exprType, c.table, toBlock);
    LUAU_ASSERT(toBlock.empty());
    return true;
}

bool ConstraintSolver::tryDispatch(const PrimitiveTypeConstraint& c, NotNull<const Constraint> constraint)
{
    std::optional<TypeId> expectedType = c.expectedType ? std::make_optional<TypeId>(follow(*c.expectedType)) : std::nullopt;
    if (expectedType && (isBlocked(*expectedType) || get<PendingExpansionType>(*expectedType)))
        return block(*expectedType, constraint);

    const FreeType* freeType = get<FreeType>(follow(c.freeType));

    // if this is no longer a free type, then we're done.
    if (!freeType)
        return true;

    // We will wait if there are any other references to the free type mentioned here.
    // This is probably the only thing that makes this not insane to do.
    if (auto refCount = unresolvedConstraints.find(c.freeType); refCount && *refCount > 1)
    {
        block(c.freeType, constraint);
        return false;
    }

    TypeId bindTo = c.primitiveType;

    if (freeType->upperBound != c.primitiveType && maybeSingleton(freeType->upperBound))
        bindTo = freeType->lowerBound;
    else if (expectedType && maybeSingleton(*expectedType))
        bindTo = freeType->lowerBound;

    auto ty = follow(c.freeType);
    shiftReferences(ty, bindTo);
    bind(constraint, ty, bindTo);

    return true;
}

bool ConstraintSolver::tryDispatch(const HasPropConstraint& c, NotNull<const Constraint> constraint)
{
    const TypeId subjectType = follow(c.subjectType);
    const TypeId resultType = follow(c.resultType);

    LUAU_ASSERT(get<BlockedType>(resultType));
    LUAU_ASSERT(canMutate(resultType, constraint));

    if (isBlocked(subjectType))
        return block(subjectType, constraint);

    if (const TableType* subjectTable = getTableType(subjectType))
    {
        if (subjectTable->state == TableState::Unsealed && subjectTable->remainingProps > 0 && subjectTable->props.count(c.prop) == 0)
        {
            return block(subjectType, constraint);
        }
    }

    // It doesn't matter whether this type came from an indexer or not.
    auto [blocked, result, _isIndex] = lookupTableProp(constraint, subjectType, c.prop, c.context, c.inConditional, c.suppressSimplification);
    if (!blocked.empty())
    {
        for (TypeId blocked : blocked)
            block(blocked, constraint);

        return false;
    }

    bind(constraint, resultType, result.value_or(builtinTypes->anyType));
    return true;
}

bool ConstraintSolver::tryDispatchHasIndexer(
    int& recursionDepth,
    NotNull<const Constraint> constraint,
    TypeId subjectType,
    TypeId indexType,
    TypeId resultType,
    Set<TypeId>& seen
)
{
    RecursionLimiter _rl{&recursionDepth, FInt::LuauSolverRecursionLimit};

    subjectType = follow(subjectType);
    indexType = follow(indexType);

    if (seen.contains(subjectType))
        return false;
    seen.insert(subjectType);

    LUAU_ASSERT(get<BlockedType>(resultType));
    LUAU_ASSERT(canMutate(resultType, constraint));

    if (get<AnyType>(subjectType))
    {
        bind(constraint, resultType, builtinTypes->anyType);
        return true;
    }

    if (auto ft = getMutable<FreeType>(subjectType))
    {
        if (auto tbl = get<TableType>(follow(ft->upperBound)); tbl && tbl->indexer)
        {
            unify(constraint, indexType, tbl->indexer->indexType);
            bind(constraint, resultType, tbl->indexer->indexResultType);
            return true;
        }
        else if (auto mt = get<MetatableType>(follow(ft->upperBound)))
            return tryDispatchHasIndexer(recursionDepth, constraint, mt->table, indexType, resultType, seen);

        FreeType freeResult{ft->scope, builtinTypes->neverType, builtinTypes->unknownType, Polarity::Mixed};
        emplace<FreeType>(constraint, resultType, freeResult);

        TypeId upperBound =
            arena->addType(TableType{/* props */ {}, TableIndexer{indexType, resultType}, TypeLevel{}, ft->scope, TableState::Unsealed});

        if (FFlag::LuauEagerGeneralization3)
        {
            TypeId sr = follow(simplifyIntersection(constraint->scope, constraint->location, ft->upperBound, upperBound));

            if (get<NeverType>(sr))
                bind(constraint, resultType, builtinTypes->errorType);
            else
                ft->upperBound = sr;
        }
        else
        {
            unify(constraint, subjectType, upperBound);
        }

        return true;
    }
    else if (auto tt = getMutable<TableType>(subjectType))
    {
        if (auto indexer = tt->indexer)
        {
            unify(constraint, indexType, indexer->indexType);
            bind(constraint, resultType, indexer->indexResultType);
            return true;
        }

        if (tt->state == TableState::Unsealed)
        {
            // FIXME this is greedy.

            FreeType freeResult{tt->scope, builtinTypes->neverType, builtinTypes->unknownType, Polarity::Mixed};
            emplace<FreeType>(constraint, resultType, freeResult);
            if (FFlag::LuauEagerGeneralization3)
                trackInteriorFreeType(constraint->scope, resultType);

            tt->indexer = TableIndexer{indexType, resultType};
            return true;
        }
    }
    else if (auto mt = get<MetatableType>(subjectType))
        return tryDispatchHasIndexer(recursionDepth, constraint, mt->table, indexType, resultType, seen);
    else if (auto ct = get<ExternType>(subjectType))
    {
        if (auto indexer = ct->indexer)
        {
            unify(constraint, indexType, indexer->indexType);
            bind(constraint, resultType, indexer->indexResultType);
            return true;
        }
        else if (isString(indexType))
        {
            bind(constraint, resultType, builtinTypes->unknownType);
            return true;
        }
    }
    else if (auto it = get<IntersectionType>(subjectType))
    {
        // subjectType <: {[indexType]: resultType}
        //
        // 'a & ~(false | nil) <: {[indexType]: resultType}
        //
        // 'a <: {[indexType]: resultType}
        // ~(false | nil) <: {[indexType]: resultType}

        Set<TypeId> parts{nullptr};
        for (TypeId part : it)
            parts.insert(follow(part));

        Set<TypeId> results{nullptr};

        for (TypeId part : parts)
        {
            TypeId r = arena->addType(BlockedType{});
            getMutable<BlockedType>(r)->setOwner(constraint.get());

            bool ok = tryDispatchHasIndexer(recursionDepth, constraint, part, indexType, r, seen);
            // If we've cut a recursive loop short, skip it.
            if (!ok)
                continue;

            r = follow(r);
            if (!get<ErrorType>(r))
                results.insert(r);
        }

        if (0 == results.size())
            bind(constraint, resultType, builtinTypes->errorType);
        else if (1 == results.size())
            bind(constraint, resultType, *results.begin());
        else
            emplace<IntersectionType>(constraint, resultType, std::vector(results.begin(), results.end()));

        return true;
    }
    else if (auto ut = get<UnionType>(subjectType))
    {
        Set<TypeId> parts{nullptr};
        for (TypeId part : ut)
            parts.insert(follow(part));

        Set<TypeId> results{nullptr};

        for (TypeId part : parts)
        {
            TypeId r = arena->addType(BlockedType{});
            getMutable<BlockedType>(r)->setOwner(constraint.get());

            bool ok = tryDispatchHasIndexer(recursionDepth, constraint, part, indexType, r, seen);
            // If we've cut a recursive loop short, skip it.
            if (!ok)
                continue;

            r = follow(r);
            if (FFlag::LuauInsertErrorTypesIntoIndexerResult || !get<ErrorType>(r))
                results.insert(r);
        }

        if (0 == results.size())
            bind(constraint, resultType, builtinTypes->errorType);
        else if (1 == results.size())
        {
            TypeId firstResult = *results.begin();
            shiftReferences(resultType, firstResult);
            bind(constraint, resultType, firstResult);
        }
        else
            emplace<UnionType>(constraint, resultType, std::vector(results.begin(), results.end()));

        return true;
    }

    bind(constraint, resultType, builtinTypes->errorType);

    return true;
}

namespace
{

struct BlockedTypeFinder : TypeOnceVisitor
{
    std::optional<TypeId> blocked;

    bool visit(TypeId ty) override
    {
        // If we've already found one, stop traversing.
        return !blocked.has_value();
    }

    bool visit(TypeId ty, const BlockedType&) override
    {
        blocked = ty;
        return false;
    }
};

} // namespace

bool ConstraintSolver::tryDispatch(const HasIndexerConstraint& c, NotNull<const Constraint> constraint)
{
    const TypeId subjectType = follow(c.subjectType);
    const TypeId indexType = follow(c.indexType);

    if (isBlocked(subjectType))
        return block(subjectType, constraint);

    if (isBlocked(indexType))
        return block(indexType, constraint);

    BlockedTypeFinder btf;

    btf.visit(subjectType);

    if (btf.blocked)
        return block(*btf.blocked, constraint);
    int recursionDepth = 0;

    Set<TypeId> seen{nullptr};

    return tryDispatchHasIndexer(recursionDepth, constraint, subjectType, indexType, c.resultType, seen);
}

bool ConstraintSolver::tryDispatch(const AssignPropConstraint& c, NotNull<const Constraint> constraint)
{
    TypeId lhsType = follow(c.lhsType);
    const std::string& propName = c.propName;
    const TypeId rhsType = follow(c.rhsType);

    if (isBlocked(lhsType))
        return block(lhsType, constraint);

    // 1. lhsType is a class that already has the prop
    // 2. lhsType is a table that already has the prop (or a union or
    //    intersection that has the prop in aggregate)
    // 3. lhsType has a metatable that already has the prop
    // 4. lhsType is an unsealed table that does not have the prop, but has a
    //    string indexer
    // 5. lhsType is an unsealed table that does not have the prop or a string
    //    indexer

    // Important: In every codepath through this function, the type `c.propType`
    // must be bound to something, even if it's just the errorType.

    if (auto lhsExternType = get<ExternType>(lhsType))
    {
        const Property* prop = lookupExternTypeProp(lhsExternType, propName);
        if (!prop || !prop->writeTy.has_value())
        {
            bind(constraint, c.propType, builtinTypes->anyType);
            return true;
        }

        bind(constraint, c.propType, *prop->writeTy);
        unify(constraint, rhsType, *prop->writeTy);
        return true;
    }

    if (auto lhsFree = getMutable<FreeType>(lhsType))
    {
        auto lhsFreeUpperBound = follow(lhsFree->upperBound);

        if (FFlag::LuauEagerGeneralization3)
        {
            const auto [blocked, maybeTy, isIndex] = lookupTableProp(constraint, lhsType, propName, ValueContext::LValue);
            if (!blocked.empty())
            {
                for (TypeId t : blocked)
                    block(t, constraint);
                return false;
            }
            else if (maybeTy)
            {
                bind(constraint, c.propType, isIndex ? arena->addType(UnionType{{*maybeTy, builtinTypes->nilType}}) : *maybeTy);
                unify(constraint, rhsType, *maybeTy);
                return true;
            }
            else
            {
                TypeId newUpperBound = arena->addType(TableType{TableState::Free, TypeLevel{}, constraint->scope});

                trackInteriorFreeType(constraint->scope, newUpperBound);

                TableType* upperTable = getMutable<TableType>(newUpperBound);
                LUAU_ASSERT(upperTable);

                upperTable->props[c.propName] = rhsType;

                // Food for thought: Could we block if simplification encounters a blocked type?
                lhsFree->upperBound = simplifyIntersection(constraint->scope, constraint->location, lhsFreeUpperBound, newUpperBound);

                bind(constraint, c.propType, rhsType);
                return true;
            }
        }
        else
        {
            if (get<TableType>(lhsFreeUpperBound) || get<MetatableType>(lhsFreeUpperBound))
                lhsType = lhsFreeUpperBound;
            else
            {
                TypeId newUpperBound = arena->addType(TableType{TableState::Free, TypeLevel{}, constraint->scope});

                trackInteriorFreeType(constraint->scope, newUpperBound);

                TableType* upperTable = getMutable<TableType>(newUpperBound);
                LUAU_ASSERT(upperTable);

                upperTable->props[c.propName] = rhsType;

                // Food for thought: Could we block if simplification encounters a blocked type?
                lhsFree->upperBound = simplifyIntersection(constraint->scope, constraint->location, lhsFreeUpperBound, newUpperBound);

                bind(constraint, c.propType, rhsType);
                return true;
            }
        }
    }

    // Handle the case that lhsType is a table that already has the property or
    // a matching indexer. This also handles unions and intersections.
    const auto [blocked, maybeTy, isIndex] = lookupTableProp(constraint, lhsType, propName, ValueContext::LValue);
    if (!blocked.empty())
    {
        for (TypeId t : blocked)
            block(t, constraint);
        return false;
    }

    if (maybeTy)
    {
        TypeId propTy = *maybeTy;
        bind(constraint, c.propType, isIndex ? arena->addType(UnionType{{propTy, builtinTypes->nilType}}) : propTy);
        unify(constraint, rhsType, propTy);
        return true;
    }

    if (auto lhsMeta = get<MetatableType>(lhsType))
        lhsType = follow(lhsMeta->table);

    // Handle the case where the lhs type is a table that does not have the
    // named property. It could be a table with a string indexer, or an unsealed
    // or free table that can grow.
    if (auto lhsTable = getMutable<TableType>(lhsType))
    {
        if (auto it = lhsTable->props.find(propName); it != lhsTable->props.end())
        {
            Property& prop = it->second;

            if (prop.writeTy.has_value())
            {
                bind(constraint, c.propType, *prop.writeTy);
                unify(constraint, rhsType, *prop.writeTy);
                return true;
            }
            else
            {
                LUAU_ASSERT(prop.isReadOnly());
                if (lhsTable->state == TableState::Unsealed || lhsTable->state == TableState::Free)
                {
                    prop.writeTy = prop.readTy;
                    bind(constraint, c.propType, *prop.writeTy);
                    unify(constraint, rhsType, *prop.writeTy);
                    return true;
                }
                else
                {
                    bind(constraint, c.propType, builtinTypes->errorType);
                    return true;
                }
            }
        }

        if (lhsTable->indexer && maybeString(lhsTable->indexer->indexType))
        {
            bind(constraint, c.propType, rhsType);
            unify(constraint, rhsType, lhsTable->indexer->indexResultType);
            return true;
        }

        if (lhsTable->state == TableState::Unsealed || lhsTable->state == TableState::Free)
        {
            bind(constraint, c.propType, rhsType);
            Property& newProp = lhsTable->props[propName];
            newProp.readTy = rhsType;
            newProp.writeTy = rhsType;
            newProp.location = c.propLocation;

            if (lhsTable->state == TableState::Unsealed && c.decrementPropCount)
            {
                LUAU_ASSERT(lhsTable->remainingProps > 0);
                lhsTable->remainingProps -= 1;
            }

            return true;
        }
    }

    bind(constraint, c.propType, builtinTypes->errorType);

    return true;
}

bool ConstraintSolver::tryDispatch(const AssignIndexConstraint& c, NotNull<const Constraint> constraint)
{
    const TypeId lhsType = follow(c.lhsType);
    const TypeId indexType = follow(c.indexType);
    const TypeId rhsType = follow(c.rhsType);

    if (isBlocked(lhsType))
        return block(lhsType, constraint);

    // 0. lhsType could be an intersection or union.
    // 1. lhsType is a class with an indexer
    // 2. lhsType is a table with an indexer, or it has a metatable that has an indexer
    // 3. lhsType is a free or unsealed table and can grow an indexer

    // Important: In every codepath through this function, the type `c.propType`
    // must be bound to something, even if it's just the errorType.

    auto tableStuff = [&](TableType* lhsTable) -> std::optional<bool>
    {
        if (lhsTable->indexer)
        {
            unify(constraint, indexType, lhsTable->indexer->indexType);
            unify(constraint, rhsType, lhsTable->indexer->indexResultType);
            bind(constraint, c.propType, arena->addType(UnionType{{lhsTable->indexer->indexResultType, builtinTypes->nilType}}));
            return true;
        }

        if (lhsTable->state == TableState::Unsealed || lhsTable->state == TableState::Free)
        {
            lhsTable->indexer = TableIndexer{indexType, rhsType};
            bind(constraint, c.propType, rhsType);
            return true;
        }

        return {};
    };

    if (auto lhsFree = getMutable<FreeType>(lhsType))
    {
        if (FFlag::LuauMissingFollowInAssignIndexConstraint)
        {
            if (auto lhsTable = getMutable<TableType>(follow(lhsFree->upperBound)))
            {
                if (auto res = tableStuff(lhsTable))
                    return *res;
            }
        }
        else
        {
            if (auto lhsTable = getMutable<TableType>(lhsFree->upperBound))
            {
                if (auto res = tableStuff(lhsTable))
                    return *res;
            }
        }

        TypeId newUpperBound =
            arena->addType(TableType{/*props*/ {}, TableIndexer{indexType, rhsType}, TypeLevel{}, constraint->scope, TableState::Free});
        const TableType* newTable = get<TableType>(newUpperBound);
        LUAU_ASSERT(newTable);

        unify(constraint, lhsType, newUpperBound);

        LUAU_ASSERT(newTable->indexer);
        bind(constraint, c.propType, newTable->indexer->indexResultType);
        return true;
    }

    if (auto lhsTable = getMutable<TableType>(lhsType))
    {
        std::optional<bool> res = tableStuff(lhsTable);
        if (res.has_value())
            return *res;
    }

    if (auto lhsExternType = get<ExternType>(lhsType))
    {
        while (true)
        {
            if (lhsExternType->indexer)
            {
                unify(constraint, indexType, lhsExternType->indexer->indexType);
                unify(constraint, rhsType, lhsExternType->indexer->indexResultType);
                bind(constraint, c.propType, arena->addType(UnionType{{lhsExternType->indexer->indexResultType, builtinTypes->nilType}}));
                return true;
            }

            if (lhsExternType->parent)
                lhsExternType = get<ExternType>(lhsExternType->parent);
            else
                break;
        }
        return true;
    }

    if (auto lhsIntersection = getMutable<IntersectionType>(lhsType))
    {
        std::set<TypeId> parts;

        for (TypeId t : lhsIntersection)
        {
            if (auto tbl = getMutable<TableType>(follow(t)))
            {
                if (tbl->indexer)
                {
                    unify(constraint, indexType, tbl->indexer->indexType);
                    parts.insert(tbl->indexer->indexResultType);
                }

                if (tbl->state == TableState::Unsealed || tbl->state == TableState::Free)
                {
                    tbl->indexer = TableIndexer{indexType, rhsType};
                    parts.insert(rhsType);
                }
            }
            else if (auto cls = get<ExternType>(follow(t)))
            {
                while (true)
                {
                    if (cls->indexer)
                    {
                        unify(constraint, indexType, cls->indexer->indexType);
                        parts.insert(cls->indexer->indexResultType);
                        break;
                    }

                    if (cls->parent)
                        cls = get<ExternType>(cls->parent);
                    else
                        break;
                }
            }
        }

        TypeId res = simplifyIntersection(constraint->scope, constraint->location, std::move(parts));

        unify(constraint, rhsType, res);
    }

    // Other types do not support index assignment.
    bind(constraint, c.propType, builtinTypes->errorType);

    return true;
}

bool ConstraintSolver::tryDispatch(const UnpackConstraint& c, NotNull<const Constraint> constraint)
{
    TypePackId sourcePack = follow(c.sourcePack);

    if (isBlocked(sourcePack))
        return block(sourcePack, constraint);

    TypePack srcPack = extendTypePack(*arena, builtinTypes, sourcePack, c.resultPack.size());

    auto resultIter = begin(c.resultPack);
    auto resultEnd = end(c.resultPack);

    size_t i = 0;
    while (resultIter != resultEnd)
    {
        if (i >= srcPack.head.size())
            break;

        TypeId srcTy = follow(srcPack.head[i]);
        TypeId resultTy = follow(*resultIter);

        LUAU_ASSERT(get<BlockedType>(resultTy));
        LUAU_ASSERT(canMutate(resultTy, constraint));

        if (get<BlockedType>(resultTy))
        {
            if (follow(srcTy) == resultTy)
            {
                // It is sometimes the case that we find that a blocked type
                // is only blocked on itself. This doesn't actually
                // constitute any meaningful constraint, so we replace it
                // with a free type.
                TypeId f = freshType(arena, builtinTypes, constraint->scope, Polarity::Positive); // FIXME?  Is this the right polarity?
                trackInteriorFreeType(constraint->scope, f);
                shiftReferences(resultTy, f);
                emplaceType<BoundType>(asMutable(resultTy), f);
            }
            else
                bind(constraint, resultTy, srcTy);
        }
        else
            unify(constraint, srcTy, resultTy);

        unblock(resultTy, constraint->location);

        ++resultIter;
        ++i;
    }

    // We know that resultPack does not have a tail, but we don't know if
    // sourcePack is long enough to fill every value.  Replace every remaining
    // result TypeId with `nil`.

    while (resultIter != resultEnd)
    {
        TypeId resultTy = follow(*resultIter);
        LUAU_ASSERT(canMutate(resultTy, constraint));
        if (get<BlockedType>(resultTy) || get<PendingExpansionType>(resultTy))
        {
            bind(constraint, resultTy, builtinTypes->nilType);
        }

        ++resultIter;
    }

    return true;
}

bool ConstraintSolver::tryDispatch(const ReduceConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    TypeId ty = follow(c.ty);
    FunctionGraphReductionResult result =
        reduceTypeFunctions(ty, constraint->location, TypeFunctionContext{NotNull{this}, constraint->scope, constraint}, force);

    for (TypeId r : result.reducedTypes)
        unblock(r, constraint->location);

    for (TypePackId r : result.reducedPacks)
        unblock(r, constraint->location);

    for (TypeId ity : result.irreducibleTypes)
        uninhabitedTypeFunctions.insert(ity);

    bool reductionFinished = result.blockedTypes.empty() && result.blockedPacks.empty();

    ty = follow(ty);

    // If we couldn't reduce this type function, stick it in the set!
    if (get<TypeFunctionInstanceType>(ty) && !result.irreducibleTypes.find(ty))
        typeFunctionsToFinalize[ty] = constraint;

    if (force || reductionFinished)
    {
        for (auto& message : result.messages)
        {
            reportError(std::move(message));
        }

        // if we're completely dispatching this constraint, we want to record any uninhabited type functions to unblock.
        for (auto error : result.errors)
        {
            if (auto utf = get<UninhabitedTypeFunction>(error))
                uninhabitedTypeFunctions.insert(utf->ty);
            else if (auto utpf = get<UninhabitedTypePackFunction>(error))
                uninhabitedTypeFunctions.insert(utpf->tp);
        }
    }

    if (force)
        return true;

    for (TypeId b : result.blockedTypes)
        block(b, constraint);

    for (TypePackId b : result.blockedPacks)
        block(b, constraint);

    return reductionFinished;
}

bool ConstraintSolver::tryDispatch(const ReducePackConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    TypePackId tp = follow(c.tp);
    FunctionGraphReductionResult result =
        reduceTypeFunctions(tp, constraint->location, TypeFunctionContext{NotNull{this}, constraint->scope, constraint}, force);

    for (TypeId r : result.reducedTypes)
        unblock(r, constraint->location);

    for (TypePackId r : result.reducedPacks)
        unblock(r, constraint->location);

    bool reductionFinished = result.blockedTypes.empty() && result.blockedPacks.empty();

    if (force || reductionFinished)
    {
        // if we're completely dispatching this constraint, we want to record any uninhabited type functions to unblock.
        for (auto error : result.errors)
        {
            if (auto utf = get<UninhabitedTypeFunction>(error))
                uninhabitedTypeFunctions.insert(utf->ty);
            else if (auto utpf = get<UninhabitedTypePackFunction>(error))
                uninhabitedTypeFunctions.insert(utpf->tp);
        }
    }

    if (force)
        return true;

    for (TypeId b : result.blockedTypes)
        block(b, constraint);

    for (TypePackId b : result.blockedPacks)
        block(b, constraint);

    return reductionFinished;
}

bool ConstraintSolver::tryDispatch(const EqualityConstraint& c, NotNull<const Constraint> constraint)
{
    unify(constraint, c.resultType, c.assignmentType);
    unify(constraint, c.assignmentType, c.resultType);
    return true;
}

struct FindAllUnionMembers : TypeOnceVisitor
{
    TypeIds recordedTys;
    TypeIds blockedTys;

    FindAllUnionMembers()
        : TypeOnceVisitor(/* skipBoundTypes */ true)
    {
    }

    bool visit(TypeId ty) override
    {
        recordedTys.insert(ty);
        return false;
    }

    bool visit(TypeId ty, const BlockedType&) override
    {
        blockedTys.insert(ty);
        return false;
    }
    bool visit(TypeId ty, const PendingExpansionType&) override
    {
        blockedTys.insert(ty);
        return false;
    }
    bool visit(TypeId ty, const FreeType&) override
    {
        blockedTys.insert(ty);
        return false;
    }
    bool visit(TypeId ty, const TypeFunctionInstanceType&) override
    {
        blockedTys.insert(ty);
        return false;
    }

    bool visit(TypeId, const UnionType&) override
    {
        return true;
    }
};

bool ConstraintSolver::tryDispatch(const SimplifyConstraint& c, NotNull<const Constraint> constraint)
{
    TypeId target = follow(c.ty);

    if (target->persistent || target->owningArena != arena || !is<UnionType>(target))
    {
        // If our target ends up being:
        // - A persistent union like `false?`
        // - A union from another arena
        // - Something other than a union type
        // Then it's either harmful or useless to fire this constraint, so we exit early.
        return true;
    }

    FindAllUnionMembers finder;
    finder.traverse(target);
    if (!finder.blockedTys.empty())
    {
        for (TypeId ty : finder.blockedTys)
            block(ty, constraint);
        return false;
    }
    TypeId result = builtinTypes->neverType;
    for (TypeId ty : finder.recordedTys)
    {
        ty = follow(ty);
        if (ty == target)
            continue;
        result = simplifyUnion(constraint->scope, constraint->location, result, ty);
    }
    emplaceType<BoundType>(asMutable(target), result);
    return true;
}

bool ConstraintSolver::tryDispatchIterableTable(TypeId iteratorTy, const IterableConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    iteratorTy = follow(iteratorTy);

    if (get<FreeType>(iteratorTy))
    {
        TypeId keyTy = freshType(arena, builtinTypes, constraint->scope, Polarity::Mixed);
        TypeId valueTy = freshType(arena, builtinTypes, constraint->scope, Polarity::Mixed);
        trackInteriorFreeType(constraint->scope, keyTy);
        trackInteriorFreeType(constraint->scope, valueTy);
        TypeId tableTy = arena->addType(TableType{TableState::Sealed, {}, constraint->scope});
        getMutable<TableType>(tableTy)->indexer = TableIndexer{keyTy, valueTy};

        pushConstraint(constraint->scope, constraint->location, SubtypeConstraint{iteratorTy, tableTy});

        auto it = begin(c.variables);
        auto endIt = end(c.variables);
        if (it != endIt)
        {
            bind(constraint, *it, keyTy);
            ++it;
        }
        if (it != endIt)
            bind(constraint, *it, valueTy);

        return true;
    }

    auto unpack = [&](TypeId ty)
    {
        for (TypeId varTy : c.variables)
        {
            LUAU_ASSERT(get<BlockedType>(varTy));
            LUAU_ASSERT(varTy != ty);
            bind(constraint, varTy, ty);
        }
    };

    if (get<AnyType>(iteratorTy))
    {
        unpack(builtinTypes->anyType);
        return true;
    }

    if (get<ErrorType>(iteratorTy))
    {
        unpack(builtinTypes->errorType);
        return true;
    }

    if (get<NeverType>(iteratorTy))
    {
        unpack(builtinTypes->neverType);
        return true;
    }

    // Irksome: I don't think we have any way to guarantee that this table
    // type never has a metatable.

    if (auto iteratorTable = get<TableType>(iteratorTy))
    {
        /*
         * We try not to dispatch IterableConstraints over free tables because
         * it's possible that there are other constraints on the table that will
         * clarify what we should do.
         *
         * We should eventually introduce a type function to talk about iteration.
         */
        if (iteratorTable->state == TableState::Free && !force)
            return block(iteratorTy, constraint);

        if (iteratorTable->indexer)
        {
            std::vector<TypeId> expectedVariables{iteratorTable->indexer->indexType, iteratorTable->indexer->indexResultType};
            while (c.variables.size() >= expectedVariables.size())
                expectedVariables.push_back(builtinTypes->errorRecoveryType());

            for (size_t i = 0; i < c.variables.size(); ++i)
            {
                LUAU_ASSERT(c.variables[i] != expectedVariables[i]);

                unify(constraint, c.variables[i], expectedVariables[i]);

                bind(constraint, c.variables[i], expectedVariables[i]);
            }
        }
        else
            unpack(builtinTypes->errorType);
    }
    else if (std::optional<TypeId> iterFn = findMetatableEntry(builtinTypes, errors, iteratorTy, "__iter", Location{}))
    {
        if (isBlocked(*iterFn))
        {
            return block(*iterFn, constraint);
        }

        if (std::optional<TypeId> instantiatedIterFn = instantiate(builtinTypes, arena, NotNull{&limits}, constraint->scope, *iterFn))
        {
            if (auto iterFtv = get<FunctionType>(*instantiatedIterFn))
            {
                TypePackId expectedIterArgs = arena->addTypePack({iteratorTy});
                unify(constraint, iterFtv->argTypes, expectedIterArgs);

                TypePack iterRets = extendTypePack(*arena, builtinTypes, iterFtv->retTypes, 2);

                if (iterRets.head.size() < 1)
                {
                    // We've done what we can; this will get reported as an
                    // error by the type checker.
                    return true;
                }

                TypeId nextFn = iterRets.head[0];

                if (std::optional<TypeId> instantiatedNextFn = instantiate(builtinTypes, arena, NotNull{&limits}, constraint->scope, nextFn))
                {
                    const FunctionType* nextFn = get<FunctionType>(*instantiatedNextFn);

                    // If nextFn is nullptr, then the iterator function has an improper signature.
                    if (nextFn)
                        unpackAndAssign(c.variables, nextFn->retTypes, constraint);

                    return true;
                }
                else
                {
                    reportError(UnificationTooComplex{}, constraint->location);
                }
            }
            else
            {
                // TODO: Support __call and function overloads (what does an overload even mean for this?)
            }
        }
        else
        {
            reportError(UnificationTooComplex{}, constraint->location);
        }
    }
    else if (auto iteratorMetatable = get<MetatableType>(iteratorTy))
    {
        // If the metatable does not contain a `__iter` metamethod, then we iterate over the table part of the metatable.
        return tryDispatchIterableTable(iteratorMetatable->table, c, constraint, force);
    }
    else if (auto primitiveTy = get<PrimitiveType>(iteratorTy); primitiveTy && primitiveTy->type == PrimitiveType::Type::Table)
        unpack(builtinTypes->unknownType);
    else
    {
        unpack(builtinTypes->errorType);
    }

    return true;
}

bool ConstraintSolver::tryDispatchIterableFunction(TypeId nextTy, TypeId tableTy, const IterableConstraint& c, NotNull<const Constraint> constraint)
{
    const FunctionType* nextFn = get<FunctionType>(nextTy);
    // If this does not hold, we should've never called `tryDispatchIterableFunction` in the first place.
    LUAU_ASSERT(nextFn);

    // the type of the `nextAstFragment` is the `nextTy`.
    (*c.astForInNextTypes)[c.nextAstFragment] = nextTy;

    if (FFlag::LuauAddCallConstraintForIterableFunctions)
    {
        // Construct a FunctionCallConstraint, to help us learn about the type of the loop variables being assigned to in this iterable
        TypePackId tableTyPack = arena->addTypePack({tableTy});

        TypePackId variablesPack = arena->addTypePack(BlockedTypePack{});

        auto callConstraint = pushConstraint(constraint->scope, constraint->location, FunctionCallConstraint{nextTy, tableTyPack, variablesPack});

        getMutable<BlockedTypePack>(variablesPack)->owner = callConstraint.get();

        auto unpackConstraint = unpackAndAssign(c.variables, variablesPack, constraint);

        inheritBlocks(constraint, callConstraint);

        inheritBlocks(unpackConstraint, callConstraint);
    }
    else
    {
        const TypePackId nextRetPack = nextFn->retTypes;
        TypePackIterator it = begin(nextRetPack);
        std::vector<TypeId> modifiedNextRetHead;

        // The first value is never nil in the context of the loop, even if it's nil
        // in the next function's return type, because the loop will not advance if
        // it's nil.
        if (it != end(nextRetPack))
        {
            TypeId firstRet = *it;
            TypeId modifiedFirstRet = stripNil(builtinTypes, *arena, firstRet);
            modifiedNextRetHead.push_back(modifiedFirstRet);
            ++it;
        }

        for (; it != end(nextRetPack); ++it)
            modifiedNextRetHead.push_back(*it);

        TypePackId modifiedNextRetPack = arena->addTypePack(std::move(modifiedNextRetHead), it.tail());

        auto unpackConstraint = unpackAndAssign(c.variables, modifiedNextRetPack, constraint);

        inheritBlocks(constraint, unpackConstraint);
    }

    return true;
}

NotNull<const Constraint> ConstraintSolver::unpackAndAssign(
    const std::vector<TypeId> destTypes,
    TypePackId srcTypes,
    NotNull<const Constraint> constraint
)
{
    auto c = pushConstraint(constraint->scope, constraint->location, UnpackConstraint{destTypes, srcTypes});

    for (TypeId t : destTypes)
    {
        BlockedType* bt = getMutable<BlockedType>(t);
        LUAU_ASSERT(bt);
        bt->replaceOwner(c);
    }

    return c;
}

TablePropLookupResult ConstraintSolver::lookupTableProp(
    NotNull<const Constraint> constraint,
    TypeId subjectType,
    const std::string& propName,
    ValueContext context,
    bool inConditional,
    bool suppressSimplification
)
{
    DenseHashSet<TypeId> seen{nullptr};
    return lookupTableProp(constraint, subjectType, propName, context, inConditional, suppressSimplification, seen);
}

TablePropLookupResult ConstraintSolver::lookupTableProp(
    NotNull<const Constraint> constraint,
    TypeId subjectType,
    const std::string& propName,
    ValueContext context,
    bool inConditional,
    bool suppressSimplification,
    DenseHashSet<TypeId>& seen
)
{
    if (seen.contains(subjectType))
        return {};
    seen.insert(subjectType);

    subjectType = follow(subjectType);

    if (isBlocked(subjectType))
        return {{subjectType}, std::nullopt};
    else if (get<AnyType>(subjectType) || get<NeverType>(subjectType))
    {
        return {{}, subjectType};
    }
    else if (auto ttv = getMutable<TableType>(subjectType))
    {
        if (auto prop = ttv->props.find(propName); prop != ttv->props.end())
        {
            switch (context)
            {
            case ValueContext::RValue:
                if (auto rt = prop->second.readTy)
                    return {{}, rt};
                break;
            case ValueContext::LValue:
                if (auto wt = prop->second.writeTy)
                    return {{}, wt};
                break;
            }
        }

        if (ttv->indexer && maybeString(ttv->indexer->indexType))
            return {{}, ttv->indexer->indexResultType, /* isIndex = */ true};

        if (ttv->state == TableState::Free)
        {
            TypeId result = freshType(arena, builtinTypes, ttv->scope, Polarity::Mixed);
            trackInteriorFreeType(ttv->scope, result);
            switch (context)
            {
            case ValueContext::RValue:
                ttv->props[propName].readTy = result;
                break;
            case ValueContext::LValue:
                if (auto it = ttv->props.find(propName); it != ttv->props.end() && it->second.isReadOnly())
                {
                    // We do infer read-only properties, but we do not infer
                    // separate read and write types.
                    //
                    // If we encounter a case where a free table has a read-only
                    // property that we subsequently sense a write to, we make
                    // the judgement that the property is read-write and that
                    // both the read and write types are the same.

                    Property& prop = it->second;

                    prop.writeTy = prop.readTy;
                    return {{}, *prop.readTy};
                }
                else
                    ttv->props[propName] = Property::rw(result);

                break;
            }
            return {{}, result};
        }

        // if we are in a conditional context, we treat the property as present and `unknown` because
        // we may be _refining_ a table to include that property. we will want to revisit this a bit
        // in the future once luau has support for exact tables since this only applies when inexact.
        if (inConditional)
            return {{}, builtinTypes->unknownType};
    }
    else if (auto mt = get<MetatableType>(subjectType); mt && context == ValueContext::RValue)
    {
        auto result = lookupTableProp(constraint, mt->table, propName, context, inConditional, suppressSimplification, seen);
        if (!result.blockedTypes.empty() || result.propType)
            return result;

        TypeId mtt = follow(mt->metatable);

        if (get<BlockedType>(mtt))
            return {{mtt}, std::nullopt};
        else if (auto metatable = get<TableType>(mtt))
        {
            auto indexProp = metatable->props.find("__index");
            if (indexProp == metatable->props.end())
                return {{}, result.propType};

            // TODO: __index can be an overloaded function.

            TypeId indexType = follow(indexProp->second.type());

            if (auto ft = get<FunctionType>(indexType))
            {
                TypePack rets = extendTypePack(*arena, builtinTypes, ft->retTypes, 1);
                if (1 == rets.head.size())
                    return {{}, rets.head[0]};
                else
                {
                    // This should probably be an error: We need the first result of the MT.__index method,
                    // but it returns 0 values.  See CLI-68672
                    return {{}, builtinTypes->nilType};
                }
            }
            else
                return lookupTableProp(constraint, indexType, propName, context, inConditional, suppressSimplification, seen);
        }
        else if (get<MetatableType>(mtt))
            return lookupTableProp(constraint, mtt, propName, context, inConditional, suppressSimplification, seen);
    }
    else if (auto ct = get<ExternType>(subjectType))
    {
        if (auto p = lookupExternTypeProp(ct, propName))
            return {{}, context == ValueContext::RValue ? p->readTy : p->writeTy};
        if (ct->indexer)
        {
            return {{}, ct->indexer->indexResultType, /* isIndex = */ true};
        }
    }
    else if (auto pt = get<PrimitiveType>(subjectType); pt && pt->metatable)
    {
        const TableType* metatable = get<TableType>(follow(*pt->metatable));
        LUAU_ASSERT(metatable);

        auto indexProp = metatable->props.find("__index");
        if (indexProp == metatable->props.end())
            return {{}, std::nullopt};

        return lookupTableProp(constraint, indexProp->second.type(), propName, context, inConditional, suppressSimplification, seen);
    }
    else if (auto ft = get<FreeType>(subjectType))
    {
        const TypeId upperBound = follow(ft->upperBound);

        if (FFlag::LuauEagerGeneralization3)
        {
            if (get<TableType>(upperBound) || get<PrimitiveType>(upperBound))
            {
                TablePropLookupResult res = lookupTableProp(constraint, upperBound, propName, context, inConditional, suppressSimplification, seen);
                // If the upper bound is a table that already has the property, we don't need to extend its bounds.
                if (res.propType || get<PrimitiveType>(upperBound))
                    return res;
            }
        }
        else
        {
            if (get<TableType>(upperBound) || get<PrimitiveType>(upperBound))
                return lookupTableProp(constraint, upperBound, propName, context, inConditional, suppressSimplification, seen);
        }

        // TODO: The upper bound could be an intersection that contains suitable tables or extern types.

        NotNull<Scope> scope{ft->scope};

        const TypeId newUpperBound = arena->addType(TableType{TableState::Free, TypeLevel{}, scope});

        trackInteriorFreeType(constraint->scope, newUpperBound);

        TableType* tt = getMutable<TableType>(newUpperBound);
        LUAU_ASSERT(tt);
        TypeId propType = freshType(arena, builtinTypes, scope, Polarity::Mixed);
        trackInteriorFreeType(scope, propType);

        switch (context)
        {
        case ValueContext::RValue:
            tt->props[propName] = Property::readonly(propType);
            break;
        case ValueContext::LValue:
            tt->props[propName] = Property::rw(propType);
            break;
        }

        unify(constraint, subjectType, newUpperBound);

        return {{}, propType};
    }
    else if (auto utv = get<UnionType>(subjectType))
    {
        std::vector<TypeId> blocked;
        std::set<TypeId> options;

        for (TypeId ty : utv)
        {
            auto result = lookupTableProp(constraint, ty, propName, context, inConditional, suppressSimplification, seen);
            blocked.insert(blocked.end(), result.blockedTypes.begin(), result.blockedTypes.end());
            if (result.propType)
                options.insert(*result.propType);
        }

        if (!blocked.empty())
            return {blocked, std::nullopt};

        if (options.empty())
            return {{}, std::nullopt};
        else if (options.size() == 1)
            return {{}, *begin(options)};
        else if (options.size() == 2 && !suppressSimplification)
        {
            TypeId one = *begin(options);
            TypeId two = *(++begin(options));

            // if we're in an lvalue context, we need the _common_ type here.
            if (context == ValueContext::LValue)
                return {{}, simplifyIntersection(constraint->scope, constraint->location, one, two)};

            return {{}, simplifyUnion(constraint->scope, constraint->location, one, two)};
        }
        // if we're in an lvalue context, we need the _common_ type here.
        else if (context == ValueContext::LValue)
            return {{}, arena->addType(IntersectionType{std::vector<TypeId>(begin(options), end(options))})};
        else
            return {{}, arena->addType(UnionType{std::vector<TypeId>(begin(options), end(options))})};
    }
    else if (auto itv = get<IntersectionType>(subjectType))
    {
        std::vector<TypeId> blocked;
        std::set<TypeId> options;

        for (TypeId ty : itv)
        {
            auto result = lookupTableProp(constraint, ty, propName, context, inConditional, suppressSimplification, seen);
            blocked.insert(blocked.end(), result.blockedTypes.begin(), result.blockedTypes.end());
            if (result.propType)
                options.insert(*result.propType);
        }

        if (!blocked.empty())
            return {blocked, std::nullopt};

        if (options.empty())
            return {{}, std::nullopt};
        else if (options.size() == 1)
            return {{}, *begin(options)};
        else if (options.size() == 2 && !suppressSimplification)
        {
            TypeId one = *begin(options);
            TypeId two = *(++begin(options));
            return {{}, simplifyIntersection(constraint->scope, constraint->location, one, two)};
        }
        else
            return {{}, arena->addType(IntersectionType{std::vector<TypeId>(begin(options), end(options))})};
    }
    else if (auto pt = get<PrimitiveType>(subjectType))
    {
        // if we are in a conditional context, we treat the property as present and `unknown` because
        // we may be _refining_ a table to include that property. we will want to revisit this a bit
        // in the future once luau has support for exact tables since this only applies when inexact.
        if (inConditional && pt->type == PrimitiveType::Table)
            return {{}, builtinTypes->unknownType};
    }

    return {{}, std::nullopt};
}

template<typename TID>
bool ConstraintSolver::unify(NotNull<const Constraint> constraint, TID subTy, TID superTy)
{
    Unifier2 u2{NotNull{arena}, builtinTypes, constraint->scope, NotNull{&iceReporter}, &uninhabitedTypeFunctions};

    const bool ok = u2.unify(subTy, superTy);

    for (ConstraintV& c : u2.incompleteSubtypes)
    {
        NotNull<Constraint> addition = pushConstraint(constraint->scope, constraint->location, std::move(c));
        inheritBlocks(constraint, addition);
    }

    if (ok)
    {
        for (const auto& [expanded, additions] : u2.expandedFreeTypes)
        {
            for (TypeId addition : additions)
                upperBoundContributors[expanded].emplace_back(constraint->location, addition);
        }
    }
    else
    {
        reportError(OccursCheckFailed{}, constraint->location);
        return false;
    }

    return true;
}

bool ConstraintSolver::block_(BlockedConstraintId target, NotNull<const Constraint> constraint)
{
    // If a set is not present for the target, construct a new DenseHashSet for it,
    // else grab the address of the existing set.
    auto [iter, inserted] = blocked.try_emplace(target, nullptr);
    auto& [key, blockVec] = *iter;

    if (blockVec.find(constraint))
        return false;

    blockVec.insert(constraint);

    size_t& count = blockedConstraints[constraint];
    count += 1;

    return true;
}

void ConstraintSolver::block(NotNull<const Constraint> target, NotNull<const Constraint> constraint)
{
    const bool newBlock = block_(target.get(), constraint);
    if (newBlock)
    {
        if (logger)
            logger->pushBlock(constraint, target);

        if (FFlag::DebugLuauLogSolver)
            printf("%s depends on constraint %s\n", toString(*constraint, opts).c_str(), toString(*target, opts).c_str());
    }
}

bool ConstraintSolver::block(TypeId target, NotNull<const Constraint> constraint)
{
    const bool newBlock = block_(follow(target), constraint);
    if (newBlock)
    {
        if (logger)
            logger->pushBlock(constraint, target);

        if (FFlag::DebugLuauLogSolver)
            printf("%s depends on TypeId %s\n", toString(*constraint, opts).c_str(), toString(target, opts).c_str());
    }

    return false;
}

bool ConstraintSolver::block(TypePackId target, NotNull<const Constraint> constraint)
{
    const bool newBlock = block_(target, constraint);
    if (newBlock)
    {
        if (logger)
            logger->pushBlock(constraint, target);

        if (FFlag::DebugLuauLogSolver)
            printf("%s depends on TypePackId %s\n", toString(*constraint, opts).c_str(), toString(target, opts).c_str());
    }

    return false;
}

void ConstraintSolver::inheritBlocks(NotNull<const Constraint> source, NotNull<const Constraint> addition)
{
    // Anything that is blocked on this constraint must also be blocked on our
    // synthesized constraints.
    auto blockedIt = blocked.find(source.get());
    if (blockedIt != blocked.end())
    {
        for (const Constraint* blockedConstraint : blockedIt->second)
        {
            block(addition, NotNull{blockedConstraint});
        }
    }
}

struct Blocker : TypeOnceVisitor
{
    NotNull<ConstraintSolver> solver;
    NotNull<const Constraint> constraint;

    bool blocked = false;

    explicit Blocker(NotNull<ConstraintSolver> solver, NotNull<const Constraint> constraint)
        : solver(solver)
        , constraint(constraint)
    {
    }

    bool visit(TypeId ty, const PendingExpansionType&) override
    {
        blocked = true;
        solver->block(ty, constraint);
        return false;
    }

    bool visit(TypeId ty, const ExternType&) override
    {
        return false;
    }
};

bool ConstraintSolver::blockOnPendingTypes(TypeId target, NotNull<const Constraint> constraint)
{
    Blocker blocker{NotNull{this}, constraint};
    blocker.traverse(target);
    return !blocker.blocked;
}

bool ConstraintSolver::blockOnPendingTypes(TypePackId targetPack, NotNull<const Constraint> constraint)
{
    Blocker blocker{NotNull{this}, constraint};
    blocker.traverse(targetPack);
    return !blocker.blocked;
}

void ConstraintSolver::unblock_(BlockedConstraintId progressed)
{
    auto it = blocked.find(progressed);
    if (it == blocked.end())
        return;

    // unblocked should contain a value always, because of the above check
    for (const Constraint* unblockedConstraint : it->second)
    {
        auto& count = blockedConstraints[NotNull{unblockedConstraint}];
        if (FFlag::DebugLuauLogSolver)
            printf("Unblocking count=%d\t%s\n", int(count), toString(*unblockedConstraint, opts).c_str());

        // This assertion being hit indicates that `blocked` and
        // `blockedConstraints` desynchronized at some point. This is problematic
        // because we rely on this count being correct to skip over blocked
        // constraints.
        LUAU_ASSERT(count > 0);
        count -= 1;
    }

    blocked.erase(it);
}

void ConstraintSolver::unblock(NotNull<const Constraint> progressed)
{
    if (logger)
        logger->popBlock(progressed);

    return unblock_(progressed.get());
}

void ConstraintSolver::unblock(TypeId ty, Location location)
{
    DenseHashSet<TypeId> seen{nullptr};

    TypeId progressed = ty;
    while (true)
    {
        if (seen.find(progressed))
            iceReporter.ice("ConstraintSolver::unblock encountered a self-bound type!", location);
        seen.insert(progressed);

        if (logger)
            logger->popBlock(progressed);

        unblock_(progressed);

        if (auto bt = get<BoundType>(progressed))
            progressed = bt->boundTo;
        else
            break;
    }
}

void ConstraintSolver::unblock(TypePackId progressed, Location)
{
    if (logger)
        logger->popBlock(progressed);

    return unblock_(progressed);
}

void ConstraintSolver::unblock(const std::vector<TypeId>& types, Location location)
{
    for (TypeId t : types)
        unblock(t, location);
}

void ConstraintSolver::unblock(const std::vector<TypePackId>& packs, Location location)
{
    for (TypePackId t : packs)
        unblock(t, location);
}

void ConstraintSolver::reproduceConstraints(NotNull<Scope> scope, const Location& location, const Substitution& subst)
{
    for (auto [_, newTy] : subst.newTypes)
    {
        if (get<TypeFunctionInstanceType>(newTy))
            pushConstraint(scope, location, ReduceConstraint{newTy});
    }

    for (auto [_, newPack] : subst.newPacks)
    {
        if (get<TypeFunctionInstanceTypePack>(newPack))
            pushConstraint(scope, location, ReducePackConstraint{newPack});
    }
}

bool ConstraintSolver::isBlocked(TypeId ty) const
{
    ty = follow(ty);

    if (auto tfit = get<TypeFunctionInstanceType>(ty))
        return uninhabitedTypeFunctions.contains(ty) == false;

    return nullptr != get<BlockedType>(ty) || nullptr != get<PendingExpansionType>(ty);
}

bool ConstraintSolver::isBlocked(TypePackId tp) const
{
    tp = follow(tp);

    if (auto tfitp = get<TypeFunctionInstanceTypePack>(tp))
        return uninhabitedTypeFunctions.contains(tp) == false;

    return nullptr != get<BlockedTypePack>(tp);
}

bool ConstraintSolver::isBlocked(NotNull<const Constraint> constraint) const
{
    auto blockedIt = blockedConstraints.find(constraint);
    return blockedIt != blockedConstraints.end() && blockedIt->second > 0;
}

NotNull<Constraint> ConstraintSolver::pushConstraint(NotNull<Scope> scope, const Location& location, ConstraintV cv)
{
    std::unique_ptr<Constraint> c = std::make_unique<Constraint>(scope, location, std::move(cv));
    NotNull<Constraint> borrow = NotNull(c.get());
    solverConstraints.push_back(std::move(c));
    unsolvedConstraints.emplace_back(borrow);

    return borrow;
}

TypeId ConstraintSolver::resolveModule(const ModuleInfo& info, const Location& location)
{
    if (info.name.empty())
    {
        reportError(UnknownRequire{}, location);
        return errorRecoveryType();
    }

    for (const auto& [location, path] : requireCycles)
    {
        if (!path.empty() && path.front() == info.name)
            return builtinTypes->anyType;
    }

    ModulePtr module = moduleResolver->getModule(info.name);
    if (!module)
    {
        if (!moduleResolver->moduleExists(info.name) && !info.optional)
            reportError(UnknownRequire{moduleResolver->getHumanReadableModuleName(info.name)}, location);

        return errorRecoveryType();
    }

    if (module->type != SourceCode::Type::Module)
    {
        reportError(IllegalRequire{module->humanReadableName, "Module is not a ModuleScript. It cannot be required."}, location);
        return errorRecoveryType();
    }

    TypePackId modulePack = module->returnType;
    if (get<ErrorTypePack>(modulePack))
        return errorRecoveryType();

    std::optional<TypeId> moduleType = first(modulePack);
    if (!moduleType)
    {
        reportError(IllegalRequire{module->humanReadableName, "Module does not return exactly 1 value. It cannot be required."}, location);
        return errorRecoveryType();
    }

    return *moduleType;
}

void ConstraintSolver::reportError(TypeErrorData&& data, const Location& location)
{
    errors.emplace_back(location, std::move(data));
    errors.back().moduleName = currentModuleName;
}

void ConstraintSolver::reportError(TypeError e)
{
    errors.emplace_back(std::move(e));
    errors.back().moduleName = currentModuleName;
}

void ConstraintSolver::shiftReferences(TypeId source, TypeId target)
{
    target = follow(target);

    // if the target isn't a reference counted type, there's nothing to do.
    // this stops us from keeping unnecessary counts for e.g. primitive types.
    if (!isReferenceCountedType(target))
        return;

    auto sourceRefs = unresolvedConstraints.find(source);
    if (!sourceRefs)
        return;

    // we read out the count before proceeding to avoid hash invalidation issues.
    size_t count = *sourceRefs;

    auto [targetRefs, _] = unresolvedConstraints.try_insert(target, 0);
    targetRefs += count;

    // Any constraint that might have mutated source may now mutate target

    if (FFlag::LuauEagerGeneralization3)
    {
        auto it = mutatedFreeTypeToConstraint.find(source);
        if (it != mutatedFreeTypeToConstraint.end())
        {
            auto [it2, fresh] = mutatedFreeTypeToConstraint.try_emplace(target, DenseHashSet<const Constraint*>{nullptr});
            for (const Constraint* constraint : it->second)
            {
                it2->second.insert(constraint);

                auto [it3, fresh2] = maybeMutatedFreeTypes.try_emplace(NotNull{constraint}, DenseHashSet<TypeId>{nullptr});
                it3->second.insert(target);
            }
        }
    }
}

std::optional<TypeId> ConstraintSolver::generalizeFreeType(NotNull<Scope> scope, TypeId type)
{
    TypeId t = follow(type);
    if (get<FreeType>(t))
    {
        auto refCount = unresolvedConstraints.find(t);
        if (refCount && *refCount > 0)
            return {};

        // if no reference count is present, then that means the only constraints referring to
        // this free type need only for it to be generalized. in principle, this means we could
        // have actually never generated the free type in the first place, but we couldn't know
        // that until all constraint generation is complete.
    }

    return generalize(NotNull{arena}, builtinTypes, scope, generalizedTypes, type);
}

bool ConstraintSolver::hasUnresolvedConstraints(TypeId ty)
{
    if (auto refCount = unresolvedConstraints.find(ty))
        return *refCount > 0;

    return false;
}

TypeId ConstraintSolver::simplifyIntersection(NotNull<Scope> scope, Location location, TypeId left, TypeId right)
{
    if (FFlag::DebugLuauEqSatSimplification)
    {
        TypeId ty = arena->addType(IntersectionType{{left, right}});

        std::optional<EqSatSimplificationResult> res = eqSatSimplify(simplifier, ty);
        if (!res)
            return ty;

        for (TypeId ty : res->newTypeFunctions)
            pushConstraint(scope, location, ReduceConstraint{ty});

        return res->result;
    }
    else
        return ::Luau::simplifyIntersection(builtinTypes, arena, left, right).result;
}

TypeId ConstraintSolver::simplifyIntersection(NotNull<Scope> scope, Location location, std::set<TypeId> parts)
{
    if (FFlag::DebugLuauEqSatSimplification)
    {
        TypeId ty = arena->addType(IntersectionType{std::vector(parts.begin(), parts.end())});

        std::optional<EqSatSimplificationResult> res = eqSatSimplify(simplifier, ty);
        if (!res)
            return ty;

        for (TypeId ty : res->newTypeFunctions)
            pushConstraint(scope, location, ReduceConstraint{ty});

        return res->result;
    }
    else
        return ::Luau::simplifyIntersection(builtinTypes, arena, std::move(parts)).result;
}

TypeId ConstraintSolver::simplifyUnion(NotNull<Scope> scope, Location location, TypeId left, TypeId right)
{
    if (FFlag::DebugLuauEqSatSimplification)
    {
        TypeId ty = arena->addType(UnionType{{left, right}});

        std::optional<EqSatSimplificationResult> res = eqSatSimplify(simplifier, ty);
        if (!res)
            return ty;

        for (TypeId ty : res->newTypeFunctions)
            pushConstraint(scope, location, ReduceConstraint{ty});

        return res->result;
    }
    else
        return ::Luau::simplifyUnion(builtinTypes, arena, left, right).result;
}

TypeId ConstraintSolver::errorRecoveryType() const
{
    return builtinTypes->errorRecoveryType();
}

TypePackId ConstraintSolver::errorRecoveryTypePack() const
{
    return builtinTypes->errorRecoveryTypePack();
}

TypePackId ConstraintSolver::anyifyModuleReturnTypePackGenerics(TypePackId tp)
{
    tp = follow(tp);

    if (const VariadicTypePack* vtp = get<VariadicTypePack>(tp))
    {
        TypeId ty = follow(vtp->ty);
        return get<GenericType>(ty) ? builtinTypes->anyTypePack : tp;
    }

    if (!get<TypePack>(follow(tp)))
        return tp;

    std::vector<TypeId> resultTypes;
    std::optional<TypePackId> resultTail;

    TypePackIterator it = begin(tp);

    for (TypePackIterator e = end(tp); it != e; ++it)
    {
        TypeId ty = follow(*it);
        resultTypes.push_back(get<GenericType>(ty) ? builtinTypes->anyType : ty);
    }

    if (std::optional<TypePackId> tail = it.tail())
        resultTail = anyifyModuleReturnTypePackGenerics(*tail);

    return arena->addTypePack(resultTypes, resultTail);
}

LUAU_NOINLINE void ConstraintSolver::throwTimeLimitError() const
{
    throw TimeLimitError(currentModuleName);
}

LUAU_NOINLINE void ConstraintSolver::throwUserCancelError() const
{
    throw UserCancelError(currentModuleName);
}

// Instantiate private template implementations for external callers
template bool ConstraintSolver::unify(NotNull<const Constraint> constraint, TypeId subTy, TypeId superTy);
template bool ConstraintSolver::unify(NotNull<const Constraint> constraint, TypePackId subTy, TypePackId superTy);

std::vector<NotNull<Constraint>> borrowConstraints(const std::vector<ConstraintPtr>& constraints)
{
    std::vector<NotNull<Constraint>> result;
    result.reserve(constraints.size());

    for (const auto& c : constraints)
        result.emplace_back(c.get());

    return result;
}

void dump(ConstraintSolver* cs, ToStringOptions& opts)
{
    printf("constraints:\n");
    for (NotNull<const Constraint> c : cs->unsolvedConstraints)
    {
        auto it = cs->blockedConstraints.find(c);
        int blockCount = it == cs->blockedConstraints.end() ? 0 : int(it->second);
        printf("\t%d\t%s\n", blockCount, toString(*c, opts).c_str());

        if (FFlag::DebugLuauLogSolverIncludeDependencies)
        {
            for (NotNull<Constraint> dep : c->dependencies)
            {
                if (std::find(cs->unsolvedConstraints.begin(), cs->unsolvedConstraints.end(), dep) != cs->unsolvedConstraints.end())
                    printf("\t\t|\t%s\n", toString(*dep, opts).c_str());
            }
        }
    }
}

} // namespace Luau
