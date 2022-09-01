// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Anyification.h"
#include "Luau/ApplyTypeFunction.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/Instantiation.h"
#include "Luau/Location.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Quantify.h"
#include "Luau/ToString.h"
#include "Luau/Unifier.h"
#include "Luau/VisitTypeVar.h"

LUAU_FASTFLAGVARIABLE(DebugLuauLogSolver, false);
LUAU_FASTFLAGVARIABLE(DebugLuauLogSolverToJson, false);
LUAU_FASTFLAG(LuauFixNameMaps)

namespace Luau
{

[[maybe_unused]] static void dumpBindings(NotNull<Scope> scope, ToStringOptions& opts)
{
    for (const auto& [k, v] : scope->bindings)
    {
        if (FFlag::LuauFixNameMaps)
        {
            auto d = toString(v.typeId, opts);
            printf("\t%s : %s\n", k.c_str(), d.c_str());
        }
        else
        {
            auto d = toStringDetailed(v.typeId, opts);
            opts.DEPRECATED_nameMap = d.DEPRECATED_nameMap;
            printf("\t%s : %s\n", k.c_str(), d.name.c_str());
        }
    }

    for (NotNull<Scope> child : scope->children)
        dumpBindings(child, opts);
}

static void dumpConstraints(NotNull<Scope> scope, ToStringOptions& opts)
{
    for (const ConstraintPtr& c : scope->constraints)
    {
        printf("\t%s\n", toString(*c, opts).c_str());
    }

    for (NotNull<Scope> child : scope->children)
        dumpConstraints(child, opts);
}

static std::pair<std::vector<TypeId>, std::vector<TypePackId>> saturateArguments(
    const TypeFun& fn, const std::vector<TypeId>& rawTypeArguments, const std::vector<TypePackId>& rawPackArguments, TypeArena* arena)
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
    if (!extraTypes.empty())
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
        else
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

            TypeId instantiatedDefault = atf.substitute(defaultTy).value_or(getSingletonTypes().errorRecoveryType());
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

            TypePackId instantiatedDefault = atf.substitute(defaultTp).value_or(getSingletonTypes().errorRecoveryTypePack());
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
        saturatedTypeArguments.push_back(getSingletonTypes().errorRecoveryType());
    }

    for (size_t i = saturatedPackArguments.size(); i < packsRequired; ++i)
    {
        saturatedPackArguments.push_back(getSingletonTypes().errorRecoveryTypePack());
    }

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

void dump(NotNull<Scope> rootScope, ToStringOptions& opts)
{
    printf("constraints:\n");
    dumpConstraints(rootScope, opts);
}

void dump(ConstraintSolver* cs, ToStringOptions& opts)
{
    printf("constraints:\n");
    for (NotNull<const Constraint> c : cs->unsolvedConstraints)
    {
        auto it = cs->blockedConstraints.find(c);
        int blockCount = it == cs->blockedConstraints.end() ? 0 : int(it->second);
        printf("\t%d\t%s\n", blockCount, toString(*c, opts).c_str());

        for (NotNull<Constraint> dep : c->dependencies)
        {
            auto unsolvedIter = std::find(begin(cs->unsolvedConstraints), end(cs->unsolvedConstraints), dep);
            if (unsolvedIter == cs->unsolvedConstraints.end())
                continue;

            auto it = cs->blockedConstraints.find(dep);
            int blockCount = it == cs->blockedConstraints.end() ? 0 : int(it->second);
            printf("\t%d\t\t%s\n", blockCount, toString(*dep, opts).c_str());
        }
    }
}

ConstraintSolver::ConstraintSolver(TypeArena* arena, NotNull<Scope> rootScope, ModuleName moduleName, NotNull<ModuleResolver> moduleResolver,
    std::vector<RequireCycle> requireCycles)
    : arena(arena)
    , constraints(collectConstraints(rootScope))
    , rootScope(rootScope)
    , currentModuleName(std::move(moduleName))
    , moduleResolver(moduleResolver)
    , requireCycles(requireCycles)
{
    opts.exhaustive = true;

    for (NotNull<Constraint> c : constraints)
    {
        unsolvedConstraints.push_back(c);

        for (NotNull<const Constraint> dep : c->dependencies)
        {
            block(dep, c);
        }
    }
}

void ConstraintSolver::run()
{
    if (done())
        return;

    if (FFlag::DebugLuauLogSolver)
    {
        printf("Starting solver\n");
        dump(this, opts);
    }

    if (FFlag::DebugLuauLogSolverToJson)
    {
        logger.captureBoundarySnapshot(rootScope, unsolvedConstraints);
    }

    auto runSolverPass = [&](bool force) {
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

            std::string saveMe = FFlag::DebugLuauLogSolver ? toString(*c, opts) : std::string{};

            if (FFlag::DebugLuauLogSolverToJson)
            {
                logger.prepareStepSnapshot(rootScope, c, unsolvedConstraints, force);
            }

            bool success = tryDispatch(c, force);

            progress |= success;

            if (success)
            {
                unblock(c);
                unsolvedConstraints.erase(unsolvedConstraints.begin() + i);

                if (FFlag::DebugLuauLogSolverToJson)
                {
                    logger.commitPreparedStepSnapshot();
                }

                if (FFlag::DebugLuauLogSolver)
                {
                    if (force)
                        printf("Force ");
                    printf("Dispatched\n\t%s\n", saveMe.c_str());
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

    if (FFlag::DebugLuauLogSolver)
    {
        dumpBindings(rootScope, opts);
    }

    if (FFlag::DebugLuauLogSolverToJson)
    {
        logger.captureBoundarySnapshot(rootScope, unsolvedConstraints);
        printf("Logger output:\n%s\n", logger.compileOutput().c_str());
    }
}

bool ConstraintSolver::done()
{
    return unsolvedConstraints.empty();
}

bool ConstraintSolver::tryDispatch(NotNull<const Constraint> constraint, bool force)
{
    if (!force && isBlocked(constraint))
        return false;

    bool success = false;

    if (auto sc = get<SubtypeConstraint>(*constraint))
        success = tryDispatch(*sc, constraint, force);
    else if (auto psc = get<PackSubtypeConstraint>(*constraint))
        success = tryDispatch(*psc, constraint, force);
    else if (auto gc = get<GeneralizationConstraint>(*constraint))
        success = tryDispatch(*gc, constraint, force);
    else if (auto ic = get<InstantiationConstraint>(*constraint))
        success = tryDispatch(*ic, constraint, force);
    else if (auto uc = get<UnaryConstraint>(*constraint))
        success = tryDispatch(*uc, constraint, force);
    else if (auto bc = get<BinaryConstraint>(*constraint))
        success = tryDispatch(*bc, constraint, force);
    else if (auto ic = get<IterableConstraint>(*constraint))
        success = tryDispatch(*ic, constraint, force);
    else if (auto nc = get<NameConstraint>(*constraint))
        success = tryDispatch(*nc, constraint);
    else if (auto taec = get<TypeAliasExpansionConstraint>(*constraint))
        success = tryDispatch(*taec, constraint);
    else if (auto fcc = get<FunctionCallConstraint>(*constraint))
        success = tryDispatch(*fcc, constraint);
    else
        LUAU_ASSERT(0);

    if (success)
    {
        unblock(constraint);
    }

    return success;
}

bool ConstraintSolver::tryDispatch(const SubtypeConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    if (isBlocked(c.subType))
        return block(c.subType, constraint);
    else if (isBlocked(c.superType))
        return block(c.superType, constraint);

    unify(c.subType, c.superType, constraint->scope);

    return true;
}

bool ConstraintSolver::tryDispatch(const PackSubtypeConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    if (isBlocked(c.subPack))
        return block(c.subPack, constraint);
    else if (isBlocked(c.superPack))
        return block(c.superPack, constraint);

    unify(c.subPack, c.superPack, constraint->scope);
    return true;
}

bool ConstraintSolver::tryDispatch(const GeneralizationConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    if (isBlocked(c.sourceType))
        return block(c.sourceType, constraint);

    TypeId generalized = quantify(arena, c.sourceType, constraint->scope);

    if (isBlocked(c.generalizedType))
        asMutable(c.generalizedType)->ty.emplace<BoundTypeVar>(generalized);
    else
        unify(c.generalizedType, generalized, constraint->scope);

    unblock(c.generalizedType);
    unblock(c.sourceType);

    return true;
}

bool ConstraintSolver::tryDispatch(const InstantiationConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    if (isBlocked(c.superType))
        return block(c.superType, constraint);

    Instantiation inst(TxnLog::empty(), arena, TypeLevel{});

    std::optional<TypeId> instantiated = inst.substitute(c.superType);
    LUAU_ASSERT(instantiated); // TODO FIXME HANDLE THIS

    if (isBlocked(c.subType))
        asMutable(c.subType)->ty.emplace<BoundTypeVar>(*instantiated);
    else
        unify(c.subType, *instantiated, constraint->scope);

    unblock(c.subType);

    return true;
}

bool ConstraintSolver::tryDispatch(const UnaryConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    TypeId operandType = follow(c.operandType);

    if (isBlocked(operandType))
        return block(operandType, constraint);

    if (get<FreeTypeVar>(operandType))
        return block(operandType, constraint);

    LUAU_ASSERT(get<BlockedTypeVar>(c.resultType));

    if (isNumber(operandType) || get<AnyTypeVar>(operandType) || get<ErrorTypeVar>(operandType))
    {
        asMutable(c.resultType)->ty.emplace<BoundTypeVar>(c.operandType);
        return true;
    }

    LUAU_ASSERT(0); // TODO metatable handling
    return false;
}

bool ConstraintSolver::tryDispatch(const BinaryConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    TypeId leftType = follow(c.leftType);
    TypeId rightType = follow(c.rightType);
    TypeId resultType = follow(c.resultType);

    if (isBlocked(leftType) || isBlocked(rightType))
    {
        /* Compound assignments create constraints of the form
         *
         *     A <: Binary<op, A, B>
         *
         * This constraint is the one that is meant to unblock A, so it doesn't
         * make any sense to stop and wait for someone else to do it.
         */
        if (leftType != resultType && rightType != resultType)
        {
            block(c.leftType, constraint);
            block(c.rightType, constraint);
            return false;
        }
    }

    if (isNumber(leftType))
    {
        unify(leftType, rightType, constraint->scope);
        asMutable(resultType)->ty.emplace<BoundTypeVar>(leftType);
        return true;
    }

    if (!force)
    {
        if (get<FreeTypeVar>(leftType))
            return block(leftType, constraint);
    }

    if (isBlocked(leftType))
    {
        asMutable(resultType)->ty.emplace<BoundTypeVar>(getSingletonTypes().errorRecoveryType());
        // reportError(constraint->location, CannotInferBinaryOperation{c.op, std::nullopt, CannotInferBinaryOperation::Operation});
        return true;
    }

    // TODO metatables, classes

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
     * families are the fix for this.
     *
     * Since we need to know all of this stuff about the types of the iteratee,
     * we have no choice but for ConstraintSolver to also be the thing that
     * applies constraints to the types of the iterators.
     */

    auto block_ = [&](auto&& t) {
        if (force)
        {
            // If we haven't figured out the type of the iteratee by now,
            // there's nothing we can do.
            return true;
        }

        block(t, constraint);
        return false;
    };

    auto [iteratorTypes, iteratorTail] = flatten(c.iterator);
    if (iteratorTail)
        return block_(*iteratorTail);

    if (0 == iteratorTypes.size())
    {
        Anyification anyify{
            arena, constraint->scope, &iceReporter, getSingletonTypes().errorRecoveryType(), getSingletonTypes().errorRecoveryTypePack()};
        std::optional<TypePackId> anyified = anyify.substitute(c.variables);
        LUAU_ASSERT(anyified);
        unify(*anyified, c.variables, constraint->scope);

        return true;
    }

    TypeId nextTy = follow(iteratorTypes[0]);
    if (get<FreeTypeVar>(nextTy))
        return block_(nextTy);

    if (get<FunctionTypeVar>(nextTy))
    {
        TypeId tableTy = getSingletonTypes().nilType;
        if (iteratorTypes.size() >= 2)
            tableTy = iteratorTypes[1];

        TypeId firstIndexTy = getSingletonTypes().nilType;
        if (iteratorTypes.size() >= 3)
            firstIndexTy = iteratorTypes[2];

        return tryDispatchIterableFunction(nextTy, tableTy, firstIndexTy, c, constraint, force);
    }

    else
        return tryDispatchIterableTable(iteratorTypes[0], c, constraint, force);

    return true;
}

bool ConstraintSolver::tryDispatch(const NameConstraint& c, NotNull<const Constraint> constraint)
{
    if (isBlocked(c.namedType))
        return block(c.namedType, constraint);

    TypeId target = follow(c.namedType);

    if (target->persistent || target->owningArena != arena)
        return true;

    if (TableTypeVar* ttv = getMutable<TableTypeVar>(target))
        ttv->name = c.name;
    else if (MetatableTypeVar* mtv = getMutable<MetatableTypeVar>(target))
        mtv->syntheticName = c.name;
    else
        return block(c.namedType, constraint);

    return true;
}

struct InfiniteTypeFinder : TypeVarOnceVisitor
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

    bool visit(TypeId ty, const PendingExpansionTypeVar& petv) override
    {
        std::optional<TypeFun> tf =
            (petv.prefix) ? scope->lookupImportedType(petv.prefix->value, petv.name.value) : scope->lookupType(petv.name.value);

        if (!tf.has_value())
            return true;

        auto [typeArguments, packArguments] = saturateArguments(*tf, petv.typeArguments, petv.packArguments, solver->arena);

        if (follow(tf->type) == follow(signature.fn.type) && (signature.arguments != typeArguments || signature.packArguments != packArguments))
        {
            foundInfiniteType = true;
            return false;
        }

        return true;
    }
};

struct InstantiationQueuer : TypeVarOnceVisitor
{
    ConstraintSolver* solver;
    const InstantiationSignature& signature;
    NotNull<Scope> scope;
    Location location;

    explicit InstantiationQueuer(NotNull<Scope> scope, const Location& location, ConstraintSolver* solver, const InstantiationSignature& signature)
        : solver(solver)
        , signature(signature)
        , scope(scope)
        , location(location)
    {
    }

    bool visit(TypeId ty, const PendingExpansionTypeVar& petv) override
    {
        solver->pushConstraint(scope, location, TypeAliasExpansionConstraint{ty});
        return false;
    }
};

bool ConstraintSolver::tryDispatch(const TypeAliasExpansionConstraint& c, NotNull<const Constraint> constraint)
{
    const PendingExpansionTypeVar* petv = get<PendingExpansionTypeVar>(follow(c.target));
    if (!petv)
    {
        unblock(c.target);
        return true;
    }

    auto bindResult = [this, &c](TypeId result) {
        asMutable(c.target)->ty.emplace<BoundTypeVar>(result);
        unblock(c.target);
    };

    std::optional<TypeFun> tf = (petv->prefix) ? constraint->scope->lookupImportedType(petv->prefix->value, petv->name.value)
                                               : constraint->scope->lookupType(petv->name.value);

    if (!tf.has_value())
    {
        reportError(UnknownSymbol{petv->name.value, UnknownSymbol::Context::Type}, constraint->location);
        bindResult(getSingletonTypes().errorRecoveryType());
        return true;
    }

    // If there are no parameters to the type function we can just use the type
    // directly.
    if (tf->typeParams.empty() && tf->typePackParams.empty())
    {
        bindResult(tf->type);
        return true;
    }

    auto [typeArguments, packArguments] = saturateArguments(*tf, petv->typeArguments, petv->packArguments, arena);

    bool sameTypes = std::equal(typeArguments.begin(), typeArguments.end(), tf->typeParams.begin(), tf->typeParams.end(), [](auto&& itp, auto&& p) {
        return itp == p.ty;
    });

    bool samePacks =
        std::equal(packArguments.begin(), packArguments.end(), tf->typePackParams.begin(), tf->typePackParams.end(), [](auto&& itp, auto&& p) {
            return itp == p.tp;
        });

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
    // https://github.com/Roblox/luau/pull/68 for the RFC responsible for this.
    // This is a little nicer than using a recursion limit because we can catch
    // the infinite expansion before actually trying to expand it.
    InfiniteTypeFinder itf{this, signature, constraint->scope};
    itf.traverse(tf->type);

    if (itf.foundInfiniteType)
    {
        // TODO (CLI-56761): Report an error.
        bindResult(getSingletonTypes().errorRecoveryType());
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
        bindResult(getSingletonTypes().errorRecoveryType());
        return true;
    }

    TypeId instantiated = *maybeInstantiated;
    TypeId target = follow(instantiated);

    if (target->persistent)
        return true;

    // Type function application will happily give us the exact same type if
    // there are e.g. generic saturatedTypeArguments that go unused.
    bool needsClone = follow(tf->type) == target;
    // Only tables have the properties we're trying to set.
    TableTypeVar* ttv = getMutableTableType(target);

    if (ttv)
    {
        if (needsClone)
        {
            // Substitution::clone is a shallow clone. If this is a
            // metatable type, we want to mutate its table, so we need to
            // explicitly clone that table as well. If we don't, we will
            // mutate another module's type surface and cause a
            // use-after-free.
            if (get<MetatableTypeVar>(target))
            {
                instantiated = applyTypeFunction.clone(target);
                MetatableTypeVar* mtv = getMutable<MetatableTypeVar>(instantiated);
                mtv->table = applyTypeFunction.clone(mtv->table);
                ttv = getMutable<TableTypeVar>(mtv->table);
            }
            else if (get<TableTypeVar>(target))
            {
                instantiated = applyTypeFunction.clone(target);
                ttv = getMutable<TableTypeVar>(instantiated);
            }

            target = follow(instantiated);
        }

        ttv->instantiatedTypeParams = typeArguments;
        ttv->instantiatedTypePackParams = packArguments;
        // TODO: Fill in definitionModuleName.
    }

    bindResult(target);

    // The application is not recursive, so we need to queue up application of
    // any child type function instantiations within the result in order for it
    // to be complete.
    InstantiationQueuer queuer{constraint->scope, constraint->location, this, signature};
    queuer.traverse(target);

    instantiatedAliases[signature] = target;

    return true;
}

bool ConstraintSolver::tryDispatch(const FunctionCallConstraint& c, NotNull<const Constraint> constraint)
{
    TypeId fn = follow(c.fn);
    TypePackId result = follow(c.result);

    if (isBlocked(c.fn))
    {
        return block(c.fn, constraint);
    }

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(fn);
    bool usedMagic = false;

    if (ftv && ftv->dcrMagicFunction != nullptr)
    {
        usedMagic = ftv->dcrMagicFunction(NotNull(this), result, c.astFragment);
    }

    if (!usedMagic)
    {
        for (const auto& inner : c.innerConstraints)
        {
            unsolvedConstraints.push_back(inner);
        }

        asMutable(c.result)->ty.emplace<FreeTypePack>(constraint->scope);
    }

    unblock(c.result);

    return true;
}

bool ConstraintSolver::tryDispatchIterableTable(TypeId iteratorTy, const IterableConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    auto block_ = [&](auto&& t) {
        if (force)
        {
            // TODO: I believe it is the case that, if we are asked to force
            // this constraint, then we can do nothing but fail.  I'd like to
            // find a code sample that gets here.
            LUAU_ASSERT(0);
        }
        else
            block(t, constraint);
        return false;
    };

    // We may have to block here if we don't know what the iteratee type is,
    // if it's a free table, if we don't know it has a metatable, and so on.
    iteratorTy = follow(iteratorTy);
    if (get<FreeTypeVar>(iteratorTy))
        return block_(iteratorTy);

    auto anyify = [&](auto ty) {
        Anyification anyify{arena, constraint->scope, &iceReporter, getSingletonTypes().anyType, getSingletonTypes().anyTypePack};
        std::optional anyified = anyify.substitute(ty);
        if (!anyified)
            reportError(CodeTooComplex{}, constraint->location);
        else
            unify(*anyified, ty, constraint->scope);
    };

    auto errorify = [&](auto ty) {
        Anyification anyify{
            arena, constraint->scope, &iceReporter, getSingletonTypes().errorRecoveryType(), getSingletonTypes().errorRecoveryTypePack()};
        std::optional errorified = anyify.substitute(ty);
        if (!errorified)
            reportError(CodeTooComplex{}, constraint->location);
        else
            unify(*errorified, ty, constraint->scope);
    };

    if (get<AnyTypeVar>(iteratorTy))
    {
        anyify(c.variables);
        return true;
    }

    if (get<ErrorTypeVar>(iteratorTy))
    {
        errorify(c.variables);
        return true;
    }

    // Irksome: I don't think we have any way to guarantee that this table
    // type never has a metatable.

    if (auto iteratorTable = get<TableTypeVar>(iteratorTy))
    {
        if (iteratorTable->state == TableState::Free)
            return block_(iteratorTy);

        if (iteratorTable->indexer)
        {
            TypePackId expectedVariablePack = arena->addTypePack({iteratorTable->indexer->indexType, iteratorTable->indexer->indexResultType});
            unify(c.variables, expectedVariablePack, constraint->scope);
        }
        else
            errorify(c.variables);
    }
    else if (auto iteratorMetatable = get<MetatableTypeVar>(iteratorTy))
    {
        TypeId metaTy = follow(iteratorMetatable->metatable);
        if (get<FreeTypeVar>(metaTy))
            return block_(metaTy);

        LUAU_ASSERT(0);
    }
    else
        errorify(c.variables);

    return true;
}

bool ConstraintSolver::tryDispatchIterableFunction(
    TypeId nextTy, TypeId tableTy, TypeId firstIndexTy, const IterableConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    // We need to know whether or not this type is nil or not.
    // If we don't know, block and reschedule ourselves.
    firstIndexTy = follow(firstIndexTy);
    if (get<FreeTypeVar>(firstIndexTy))
    {
        if (force)
            LUAU_ASSERT(0);
        else
            block(firstIndexTy, constraint);
        return false;
    }

    const TypeId firstIndex = isNil(firstIndexTy) ? arena->freshType(constraint->scope) // FIXME: Surely this should be a union (free | nil)
                                                  : firstIndexTy;

    // nextTy : (tableTy, indexTy?) -> (indexTy, valueTailTy...)
    const TypePackId nextArgPack = arena->addTypePack({tableTy, arena->addType(UnionTypeVar{{firstIndex, getSingletonTypes().nilType}})});
    const TypePackId valueTailTy = arena->addTypePack(FreeTypePack{constraint->scope});
    const TypePackId nextRetPack = arena->addTypePack(TypePack{{firstIndex}, valueTailTy});

    const TypeId expectedNextTy = arena->addType(FunctionTypeVar{nextArgPack, nextRetPack});
    unify(nextTy, expectedNextTy, constraint->scope);

    pushConstraint(constraint->scope, constraint->location, PackSubtypeConstraint{c.variables, nextRetPack});

    return true;
}

void ConstraintSolver::block_(BlockedConstraintId target, NotNull<const Constraint> constraint)
{
    blocked[target].push_back(constraint);

    auto& count = blockedConstraints[constraint];
    count += 1;
}

void ConstraintSolver::block(NotNull<const Constraint> target, NotNull<const Constraint> constraint)
{
    if (FFlag::DebugLuauLogSolver)
        printf("block Constraint %s on\t%s\n", toString(*target, opts).c_str(), toString(*constraint, opts).c_str());
    block_(target, constraint);
}

bool ConstraintSolver::block(TypeId target, NotNull<const Constraint> constraint)
{
    if (FFlag::DebugLuauLogSolver)
        printf("block TypeId %s on\t%s\n", toString(target, opts).c_str(), toString(*constraint, opts).c_str());
    block_(target, constraint);
    return false;
}

bool ConstraintSolver::block(TypePackId target, NotNull<const Constraint> constraint)
{
    if (FFlag::DebugLuauLogSolver)
        printf("block TypeId %s on\t%s\n", toString(target, opts).c_str(), toString(*constraint, opts).c_str());
    block_(target, constraint);
    return false;
}

void ConstraintSolver::unblock_(BlockedConstraintId progressed)
{
    auto it = blocked.find(progressed);
    if (it == blocked.end())
        return;

    // unblocked should contain a value always, because of the above check
    for (NotNull<const Constraint> unblockedConstraint : it->second)
    {
        auto& count = blockedConstraints[unblockedConstraint];
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
    return unblock_(progressed);
}

void ConstraintSolver::unblock(TypeId progressed)
{
    return unblock_(progressed);
}

void ConstraintSolver::unblock(TypePackId progressed)
{
    return unblock_(progressed);
}

void ConstraintSolver::unblock(const std::vector<TypeId>& types)
{
    for (TypeId t : types)
        unblock(t);
}

void ConstraintSolver::unblock(const std::vector<TypePackId>& packs)
{
    for (TypePackId t : packs)
        unblock(t);
}

bool ConstraintSolver::isBlocked(TypeId ty)
{
    return nullptr != get<BlockedTypeVar>(follow(ty)) || nullptr != get<PendingExpansionTypeVar>(follow(ty));
}

bool ConstraintSolver::isBlocked(TypePackId tp)
{
    return nullptr != get<BlockedTypePack>(follow(tp));
}

bool ConstraintSolver::isBlocked(NotNull<const Constraint> constraint)
{
    auto blockedIt = blockedConstraints.find(constraint);
    return blockedIt != blockedConstraints.end() && blockedIt->second > 0;
}

void ConstraintSolver::unify(TypeId subType, TypeId superType, NotNull<Scope> scope)
{
    UnifierSharedState sharedState{&iceReporter};
    Unifier u{arena, Mode::Strict, scope, Location{}, Covariant, sharedState};

    u.tryUnify(subType, superType);

    if (!u.errors.empty())
    {
        TypeId errorType = getSingletonTypes().errorRecoveryType();
        u.tryUnify(subType, errorType);
        u.tryUnify(superType, errorType);
    }

    const auto [changedTypes, changedPacks] = u.log.getChanges();

    u.log.commit();

    unblock(changedTypes);
    unblock(changedPacks);
}

void ConstraintSolver::unify(TypePackId subPack, TypePackId superPack, NotNull<Scope> scope)
{
    UnifierSharedState sharedState{&iceReporter};
    Unifier u{arena, Mode::Strict, scope, Location{}, Covariant, sharedState};

    u.tryUnify(subPack, superPack);

    const auto [changedTypes, changedPacks] = u.log.getChanges();

    u.log.commit();

    unblock(changedTypes);
    unblock(changedPacks);
}

void ConstraintSolver::pushConstraint(NotNull<Scope> scope, const Location& location, ConstraintV cv)
{
    std::unique_ptr<Constraint> c = std::make_unique<Constraint>(scope, location, std::move(cv));
    NotNull<Constraint> borrow = NotNull(c.get());
    solverConstraints.push_back(std::move(c));
    unsolvedConstraints.push_back(borrow);
}

TypeId ConstraintSolver::resolveModule(const ModuleInfo& info, const Location& location)
{
    if (info.name.empty())
    {
        reportError(UnknownRequire{}, location);
        return getSingletonTypes().errorRecoveryType();
    }

    std::string humanReadableName = moduleResolver->getHumanReadableModuleName(info.name);

    for (const auto& [location, path] : requireCycles)
    {
        if (!path.empty() && path.front() == humanReadableName)
            return getSingletonTypes().anyType;
    }

    ModulePtr module = moduleResolver->getModule(info.name);
    if (!module)
    {
        if (!moduleResolver->moduleExists(info.name) && !info.optional)
            reportError(UnknownRequire{humanReadableName}, location);

        return getSingletonTypes().errorRecoveryType();
    }

    if (module->type != SourceCode::Type::Module)
    {
        reportError(IllegalRequire{humanReadableName, "Module is not a ModuleScript. It cannot be required."}, location);
        return getSingletonTypes().errorRecoveryType();
    }

    TypePackId modulePack = module->getModuleScope()->returnType;
    if (get<Unifiable::Error>(modulePack))
        return getSingletonTypes().errorRecoveryType();

    std::optional<TypeId> moduleType = first(modulePack);
    if (!moduleType)
    {
        reportError(IllegalRequire{humanReadableName, "Module does not return exactly 1 value. It cannot be required."}, location);
        return getSingletonTypes().errorRecoveryType();
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

} // namespace Luau
