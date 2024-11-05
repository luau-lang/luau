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
#include "Luau/Quantify.h"
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

LUAU_FASTFLAGVARIABLE(DebugLuauLogSolver)
LUAU_FASTFLAGVARIABLE(DebugLuauLogSolverIncludeDependencies)
LUAU_FASTFLAGVARIABLE(DebugLuauLogBindings)
LUAU_FASTINTVARIABLE(LuauSolverRecursionLimit, 500)
LUAU_DYNAMIC_FASTINT(LuauTypeSolverRelease)
LUAU_FASTFLAGVARIABLE(LuauRemoveNotAnyHack)
LUAU_FASTFLAG(LuauNewSolverPopulateTableLocations)

namespace Luau
{

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
        Constraint* owner = blocked->getOwner();
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

static std::pair<std::vector<TypeId>, std::vector<TypePackId>> saturateArguments(
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

    bool visit(TypeId ty, const ClassType& ctv) override
    {
        return false;
    }
};

ConstraintSolver::ConstraintSolver(
    NotNull<Normalizer> normalizer,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    NotNull<Scope> rootScope,
    std::vector<NotNull<Constraint>> constraints,
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
    , typeFunctionRuntime(typeFunctionRuntime)
    , constraints(std::move(constraints))
    , rootScope(rootScope)
    , currentModuleName(std::move(moduleName))
    , dfg(dfg)
    , moduleResolver(moduleResolver)
    , requireCycles(std::move(requireCycles))
    , logger(logger)
    , limits(std::move(limits))
{
    opts.exhaustive = true;

    for (NotNull<Constraint> c : this->constraints)
    {
        unsolvedConstraints.emplace_back(c);

        // initialize the reference counts for the free types in this constraint.
        for (auto ty : c->getMaybeMutatedFreeTypes())
        {
            // increment the reference count for `ty`
            auto [refCount, _] = unresolvedConstraints.try_insert(ty, 0);
            refCount += 1;
        }

        for (NotNull<const Constraint> dep : c->dependencies)
        {
            block(dep, c);
        }
    }
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

            bool success = tryDispatch(c, force);

            progress |= success;

            if (success)
            {
                unblock(c);
                unsolvedConstraints.erase(unsolvedConstraints.begin() + i);

                // decrement the referenced free types for this constraint if we dispatched successfully!
                for (auto ty : c->getMaybeMutatedFreeTypes())
                {
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

bool ConstraintSolver::isDone()
{
    return unsolvedConstraints.empty();
}

namespace
{

struct TypeAndLocation
{
    TypeId typeId;
    Location location;
};

} // namespace

void ConstraintSolver::bind(NotNull<const Constraint> constraint, TypeId ty, TypeId boundTo)
{
    LUAU_ASSERT(get<BlockedType>(ty) || get<FreeType>(ty) || get<PendingExpansionType>(ty));
    LUAU_ASSERT(canMutate(ty, constraint));

    boundTo = follow(boundTo);
    if (get<BlockedType>(ty) && ty == boundTo)
        return emplace<FreeType>(constraint, ty, constraint->scope, builtinTypes->neverType, builtinTypes->unknownType);

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
        success = tryDispatch(*fcc, constraint);
    else if (auto fcc = get<FunctionCheckConstraint>(*constraint))
        success = tryDispatch(*fcc, constraint);
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

    std::optional<QuantifierResult> generalized;

    std::optional<TypeId> generalizedTy = generalize(NotNull{arena}, builtinTypes, constraint->scope, generalizedTypes, c.sourceType);
    if (generalizedTy)
        generalized = QuantifierResult{*generalizedTy}; // FIXME insertedGenerics and insertedGenericPacks
    else
        reportError(CodeTooComplex{}, constraint->location);

    if (generalized)
    {
        if (get<BlockedType>(generalizedType))
            bind(constraint, generalizedType, generalized->result);
        else
            unify(constraint, generalizedType, generalized->result);

        for (auto [free, gen] : generalized->insertedGenerics.pairings)
            unify(constraint, free, gen);

        for (auto [free, gen] : generalized->insertedGenericPacks.pairings)
            unify(constraint, free, gen);
    }
    else
    {
        reportError(CodeTooComplex{}, constraint->location);
        bind(constraint, c.generalizedType, builtinTypes->errorRecoveryType());
    }

    for (TypeId ty : c.interiorTypes)
        generalize(NotNull{arena}, builtinTypes, constraint->scope, generalizedTypes, ty, /* avoidSealingTables */ false);

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
        TypeId keyTy = freshType(arena, builtinTypes, constraint->scope);
        TypeId valueTy = freshType(arena, builtinTypes, constraint->scope);
        TypeId tableTy =
            arena->addType(TableType{TableType::Props{}, TableIndexer{keyTy, valueTy}, TypeLevel{}, constraint->scope, TableState::Free});

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
        if (DFInt::LuauTypeSolverRelease >= 646)
        {
            auto cTarget = follow(c.target);
            LUAU_ASSERT(get<PendingExpansionType>(cTarget));
            shiftReferences(cTarget, result);
            bind(constraint, cTarget, result);
        }
        else
        {
            LUAU_ASSERT(get<PendingExpansionType>(c.target));
            shiftReferences(c.target, result);
            bind(constraint, c.target, result);
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

    // If there are no parameters to the type function we can just use the type
    // directly.
    if (tf->typeParams.empty() && tf->typePackParams.empty())
    {
        bindResult(tf->type);
        return true;
    }

    // Due to how pending expansion types and TypeFun's are created
    // If this check passes, we have created a cyclic / corecursive type alias
    // of size 0
    TypeId lhs = DFInt::LuauTypeSolverRelease >= 646 ? follow(c.target) : c.target;
    TypeId rhs = tf->type;
    if (occursCheck(lhs, rhs))
    {
        reportError(OccursCheckFailed{}, constraint->location);
        bindResult(errorRecoveryType());
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

        if (FFlag::LuauNewSolverPopulateTableLocations)
        {
            // This is a new type - redefine the location.
            ttv->definitionLocation = constraint->location;
            ttv->definitionModuleName = currentModuleName;
        }

        ttv->instantiatedTypeParams = typeArguments;
        ttv->instantiatedTypePackParams = packArguments;
    }

    bindResult(target);

    instantiatedAliases[signature] = target;

    return true;
}

bool ConstraintSolver::tryDispatch(const FunctionCallConstraint& c, NotNull<const Constraint> constraint)
{
    TypeId fn = follow(c.fn);
    TypePackId argsPack = follow(c.argsPack);
    TypePackId result = follow(c.result);

    if (isBlocked(fn) || hasUnresolvedConstraints(fn))
    {
        return block(c.fn, constraint);
    }

    if (get<AnyType>(fn))
    {
        emplaceTypePack<BoundTypePack>(asMutable(c.result), builtinTypes->anyTypePack);
        unblock(c.result, constraint->location);
        return true;
    }

    // if we're calling an error type, the result is an error type, and that's that.
    if (get<ErrorType>(fn))
    {
        bind(constraint, c.result, builtinTypes->errorRecoveryTypePack());
        return true;
    }

    if (get<NeverType>(fn))
    {
        bind(constraint, c.result, builtinTypes->neverTypePack);
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
        emplace<FreeTypePack>(constraint, c.result, constraint->scope);
    }
    else
    {
        const FunctionType* ftv = get<FunctionType>(fn);
        bool usedMagic = false;

        if (ftv)
        {
            if (ftv->dcrMagicFunction)
                usedMagic = ftv->dcrMagicFunction(MagicFunctionCallContext{NotNull{this}, constraint, c.callSite, c.argsPack, result});

            if (ftv->dcrMagicRefinement)
                ftv->dcrMagicRefinement(MagicRefinementContext{constraint->scope, c.callSite, c.discriminantTypes});
        }

        if (!usedMagic)
            emplace<FreeTypePack>(constraint, c.result, constraint->scope);
    }

    for (std::optional<TypeId> ty : c.discriminantTypes)
    {
        if (!ty)
            continue;

        // If the discriminant type has been transmuted, we need to unblock them.
        if (!isBlocked(*ty))
        {
            unblock(*ty, constraint->location);
            continue;
        }

        if (FFlag::LuauRemoveNotAnyHack)
        {
            // We bind any unused discriminants to the `*no-refine*` type indicating that it can be safely ignored.
            emplaceType<BoundType>(asMutable(follow(*ty)), builtinTypes->noRefineType);
        }
        else
        {
            // We use `any` here because the discriminant type may be pointed at by both branches,
            // where the discriminant type is not negated, and the other where it is negated, i.e.
            // `unknown ~ unknown` and `~unknown ~ never`, so `T & unknown ~ T` and `T & ~unknown ~ never`
            // v.s.
            // `any ~ any` and `~any ~ any`, so `T & any ~ T` and `T & ~any ~ T`
            //
            // In practice, users cannot negate `any`, so this is an implementation detail we can always change.
            emplaceType<BoundType>(asMutable(follow(*ty)), builtinTypes->anyType);
        }
    }

    OverloadResolver resolver{
        builtinTypes,
        NotNull{arena},
        normalizer,
        typeFunctionRuntime,
        constraint->scope,
        NotNull{&iceReporter},
        NotNull{&limits},
        constraint->location
    };
    auto [status, overload] = resolver.selectOverload(fn, argsPack);
    TypeId overloadToUse = fn;
    if (status == OverloadResolver::Analysis::Ok)
        overloadToUse = overload;

    TypeId inferredTy = arena->addType(FunctionType{TypeLevel{}, constraint->scope.get(), argsPack, c.result});
    Unifier2 u2{NotNull{arena}, builtinTypes, constraint->scope, NotNull{&iceReporter}};

    const bool occursCheckPassed = u2.unify(overloadToUse, inferredTy);

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

    unblock(c.result, constraint->location);

    return true;
}

static AstExpr* unwrapGroup(AstExpr* expr)
{
    while (auto group = expr->as<AstExprGroup>())
        expr = group->expr;

    return expr;
}

bool ConstraintSolver::tryDispatch(const FunctionCheckConstraint& c, NotNull<const Constraint> constraint)
{
    TypeId fn = follow(c.fn);
    const TypePackId argsPack = follow(c.argsPack);

    if (isBlocked(fn))
        return block(fn, constraint);

    if (isBlocked(argsPack))
        return true;

    if (DFInt::LuauTypeSolverRelease >= 648)
    {
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
    }

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

    for (auto generic : ftv->generics)
        replacements[generic] = builtinTypes->unknownType;

    for (auto genericPack : ftv->genericPacks)
        replacementPacks[genericPack] = builtinTypes->unknownTypePack;

    // If the type of the function has generics, we don't actually want to push any of the generics themselves
    // into the argument types as expected types because this creates an unnecessary loop. Instead, we want to
    // replace these types with `unknown` (and `...unknown`) to keep any structure but not create the cycle.
    if (!replacements.empty() || !replacementPacks.empty())
    {
        Replacer replacer{arena, std::move(replacements), std::move(replacementPacks)};

        std::optional<TypeId> res = replacer.substitute(fn);
        if (res)
        {
            if (*res != fn)
            {
                FunctionType* ftvMut = getMutable<FunctionType>(*res);
                LUAU_ASSERT(ftvMut);
                ftvMut->generics.clear();
                ftvMut->genericPacks.clear();
            }

            fn = *res;
            ftv = get<FunctionType>(*res);
            LUAU_ASSERT(ftv);

            // we've potentially copied type functions here, so we need to reproduce their reduce constraint.
            reproduceConstraints(constraint->scope, constraint->location, replacer);
        }
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

        const FunctionType* expectedLambdaTy = get<FunctionType>(expectedArgTy);
        const FunctionType* lambdaTy = get<FunctionType>(actualArgTy);
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
            std::vector<TypeId> toBlock;
            (void)matchLiteralType(c.astTypes, c.astExpectedTypes, builtinTypes, arena, NotNull{&u2}, expectedArgTy, actualArgTy, expr, toBlock);
            if (DFInt::LuauTypeSolverRelease >= 648)
            {
                LUAU_ASSERT(toBlock.empty());
            }
            else
            {
                for (auto t : toBlock)
                    block(t, constraint);
                if (!toBlock.empty())
                    return false;
            }
        }
    }

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

    if (DFInt::LuauTypeSolverRelease >= 645)
    {
        auto ty = follow(c.freeType);
        shiftReferences(ty, bindTo);
        bind(constraint, ty, bindTo);
    }
    else
    {
        shiftReferences(c.freeType, bindTo);
        bind(constraint, c.freeType, bindTo);
    }

    return true;
}

bool ConstraintSolver::tryDispatch(const HasPropConstraint& c, NotNull<const Constraint> constraint)
{
    const TypeId subjectType = follow(c.subjectType);
    const TypeId resultType = follow(c.resultType);

    LUAU_ASSERT(get<BlockedType>(resultType));
    LUAU_ASSERT(canMutate(resultType, constraint));

    if (isBlocked(subjectType) || get<PendingExpansionType>(subjectType) || get<TypeFunctionInstanceType>(subjectType))
        return block(subjectType, constraint);

    if (const TableType* subjectTable = getTableType(subjectType))
    {
        if (subjectTable->state == TableState::Unsealed && subjectTable->remainingProps > 0 && subjectTable->props.count(c.prop) == 0)
        {
            return block(subjectType, constraint);
        }
    }

    auto [blocked, result] = lookupTableProp(constraint, subjectType, c.prop, c.context, c.inConditional, c.suppressSimplification);
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

    if (auto ft = get<FreeType>(subjectType))
    {
        if (auto tbl = get<TableType>(follow(ft->upperBound)); tbl && tbl->indexer)
        {
            unify(constraint, indexType, tbl->indexer->indexType);
            bind(constraint, resultType, tbl->indexer->indexResultType);
            return true;
        }
        else if (auto mt = get<MetatableType>(follow(ft->upperBound)))
            return tryDispatchHasIndexer(recursionDepth, constraint, mt->table, indexType, resultType, seen);

        FreeType freeResult{ft->scope, builtinTypes->neverType, builtinTypes->unknownType};
        emplace<FreeType>(constraint, resultType, freeResult);

        TypeId upperBound =
            arena->addType(TableType{/* props */ {}, TableIndexer{indexType, resultType}, TypeLevel{}, ft->scope, TableState::Unsealed});

        unify(constraint, subjectType, upperBound);

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

            FreeType freeResult{tt->scope, builtinTypes->neverType, builtinTypes->unknownType};
            emplace<FreeType>(constraint, resultType, freeResult);

            tt->indexer = TableIndexer{indexType, resultType};
            return true;
        }
    }
    else if (auto mt = get<MetatableType>(subjectType))
        return tryDispatchHasIndexer(recursionDepth, constraint, mt->table, indexType, resultType, seen);
    else if (auto ct = get<ClassType>(subjectType))
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
            getMutable<BlockedType>(r)->setOwner(const_cast<Constraint*>(constraint.get()));

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
            getMutable<BlockedType>(r)->setOwner(const_cast<Constraint*>(constraint.get()));

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

    if (auto lhsClass = get<ClassType>(lhsType))
    {
        const Property* prop = lookupClassProp(lhsClass, propName);
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
        auto lhsFreeUpperBound = DFInt::LuauTypeSolverRelease >= 648 ? follow(lhsFree->upperBound) : lhsFree->upperBound;
        if (get<TableType>(lhsFreeUpperBound) || get<MetatableType>(lhsFreeUpperBound))
            lhsType = lhsFreeUpperBound;
        else
        {
            TypeId newUpperBound = arena->addType(TableType{TableState::Free, TypeLevel{}, constraint->scope});
            TableType* upperTable = getMutable<TableType>(newUpperBound);
            LUAU_ASSERT(upperTable);

            upperTable->props[c.propName] = rhsType;

            // Food for thought: Could we block if simplification encounters a blocked type?
            lhsFree->upperBound = simplifyIntersection(builtinTypes, arena, lhsFreeUpperBound, newUpperBound).result;

            bind(constraint, c.propType, rhsType);
            return true;
        }
    }

    // Handle the case that lhsType is a table that already has the property or
    // a matching indexer. This also handles unions and intersections.
    const auto [blocked, maybeTy] = lookupTableProp(constraint, lhsType, propName, ValueContext::LValue);
    if (!blocked.empty())
    {
        for (TypeId t : blocked)
            block(t, constraint);
        return false;
    }

    if (maybeTy)
    {
        const TypeId propTy = *maybeTy;
        bind(constraint, c.propType, propTy);
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
            bind(constraint, c.propType, lhsTable->indexer->indexResultType);
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
        if (auto lhsTable = getMutable<TableType>(lhsFree->upperBound))
        {
            if (auto res = tableStuff(lhsTable))
                return *res;
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

    if (auto lhsClass = get<ClassType>(lhsType))
    {
        while (true)
        {
            if (lhsClass->indexer)
            {
                unify(constraint, indexType, lhsClass->indexer->indexType);
                unify(constraint, rhsType, lhsClass->indexer->indexResultType);
                bind(constraint, c.propType, lhsClass->indexer->indexResultType);
                return true;
            }

            if (lhsClass->parent)
                lhsClass = get<ClassType>(lhsClass->parent);
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
            else if (auto cls = get<ClassType>(follow(t)))
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
                        cls = get<ClassType>(cls->parent);
                    else
                        break;
                }
            }
        }

        TypeId res = simplifyIntersection(builtinTypes, arena, std::move(parts)).result;

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
                TypeId f = freshType(arena, builtinTypes, constraint->scope);
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

    bool reductionFinished = result.blockedTypes.empty() && result.blockedPacks.empty();

    ty = follow(ty);
    // If we couldn't reduce this type function, stick it in the set!
    if (get<TypeFunctionInstanceType>(ty))
        typeFunctionsToFinalize[ty] = constraint;

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

bool ConstraintSolver::tryDispatchIterableTable(TypeId iteratorTy, const IterableConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    iteratorTy = follow(iteratorTy);

    if (get<FreeType>(iteratorTy))
    {
        TypeId keyTy = freshType(arena, builtinTypes, constraint->scope);
        TypeId valueTy = freshType(arena, builtinTypes, constraint->scope);
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
    const TypePackId nextRetPack = nextFn->retTypes;

    // the type of the `nextAstFragment` is the `nextTy`.
    (*c.astForInNextTypes)[c.nextAstFragment] = nextTy;

    auto it = begin(nextRetPack);
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

std::pair<std::vector<TypeId>, std::optional<TypeId>> ConstraintSolver::lookupTableProp(
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

std::pair<std::vector<TypeId>, std::optional<TypeId>> ConstraintSolver::lookupTableProp(
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
            return {{}, ttv->indexer->indexResultType};

        if (ttv->state == TableState::Free)
        {
            TypeId result = freshType(arena, builtinTypes, ttv->scope);
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
        auto [blocked, result] = lookupTableProp(constraint, mt->table, propName, context, inConditional, suppressSimplification, seen);
        if (!blocked.empty() || result)
            return {blocked, result};

        TypeId mtt = follow(mt->metatable);

        if (get<BlockedType>(mtt))
            return {{mtt}, std::nullopt};
        else if (auto metatable = get<TableType>(mtt))
        {
            auto indexProp = metatable->props.find("__index");
            if (indexProp == metatable->props.end())
                return {{}, result};

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
    else if (auto ct = get<ClassType>(subjectType))
    {
        if (auto p = lookupClassProp(ct, propName))
            return {{}, context == ValueContext::RValue ? p->readTy : p->writeTy};
        if (ct->indexer)
        {
            return {{}, ct->indexer->indexResultType};
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

        if (get<TableType>(upperBound) || get<PrimitiveType>(upperBound))
            return lookupTableProp(constraint, upperBound, propName, context, inConditional, suppressSimplification, seen);

        // TODO: The upper bound could be an intersection that contains suitable tables or classes.

        NotNull<Scope> scope{ft->scope};

        const TypeId newUpperBound = arena->addType(TableType{TableState::Free, TypeLevel{}, scope});
        TableType* tt = getMutable<TableType>(newUpperBound);
        LUAU_ASSERT(tt);
        TypeId propType = freshType(arena, builtinTypes, scope);

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
            auto [innerBlocked, innerResult] = lookupTableProp(constraint, ty, propName, context, inConditional, suppressSimplification, seen);
            blocked.insert(blocked.end(), innerBlocked.begin(), innerBlocked.end());
            if (innerResult)
                options.insert(*innerResult);
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
                return {{}, simplifyIntersection(builtinTypes, arena, one, two).result};

            return {{}, simplifyUnion(builtinTypes, arena, one, two).result};
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
            auto [innerBlocked, innerResult] = lookupTableProp(constraint, ty, propName, context, inConditional, suppressSimplification, seen);
            blocked.insert(blocked.end(), innerBlocked.begin(), innerBlocked.end());
            if (innerResult)
                options.insert(*innerResult);
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
            return {{}, simplifyIntersection(builtinTypes, arena, one, two).result};
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

    bool visit(TypeId ty, const ClassType&) override
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

bool ConstraintSolver::blockOnPendingTypes(TypePackId pack, NotNull<const Constraint> constraint)
{
    Blocker blocker{NotNull{this}, constraint};
    blocker.traverse(pack);
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
    if (get<Unifiable::Error>(modulePack))
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
}

std::optional<TypeId> ConstraintSolver::generalizeFreeType(NotNull<Scope> scope, TypeId type, bool avoidSealingTables)
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

    return generalize(NotNull{arena}, builtinTypes, scope, generalizedTypes, type, avoidSealingTables);
}

bool ConstraintSolver::hasUnresolvedConstraints(TypeId ty)
{
    if (auto refCount = unresolvedConstraints.find(ty))
        return *refCount > 0;

    return false;
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

} // namespace Luau
