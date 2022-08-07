// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/ApplyTypeFunction.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/Instantiation.h"
#include "Luau/Location.h"
#include "Luau/Quantify.h"
#include "Luau/ToString.h"
#include "Luau/Unifier.h"
#include "Luau/VisitTypeVar.h"

LUAU_FASTFLAGVARIABLE(DebugLuauLogSolver, false);
LUAU_FASTFLAGVARIABLE(DebugLuauLogSolverToJson, false);

namespace Luau
{

[[maybe_unused]] static void dumpBindings(NotNull<Scope> scope, ToStringOptions& opts)
{
    for (const auto& [k, v] : scope->bindings)
    {
        auto d = toStringDetailed(v.typeId, opts);
        opts.nameMap = d.nameMap;
        printf("\t%s : %s\n", k.c_str(), d.name.c_str());
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
    for (const Constraint* c : cs->unsolvedConstraints)
    {
        printf("\t%s\n", toString(*c, opts).c_str());

        for (const Constraint* dep : c->dependencies)
            printf("\t\t%s\n", toString(*dep, opts).c_str());
    }
}

ConstraintSolver::ConstraintSolver(TypeArena* arena, NotNull<Scope> rootScope)
    : arena(arena)
    , constraints(collectConstraints(rootScope))
    , rootScope(rootScope)
{
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

    ToStringOptions opts;
    opts.exhaustive = true;

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
                logger.prepareStepSnapshot(rootScope, c, unsolvedConstraints);
            }

            bool success = tryDispatch(c, force);

            progress |= success;

            if (success)
            {
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
    else if (auto nc = get<NameConstraint>(*constraint))
        success = tryDispatch(*nc, constraint);
    else if (auto taec = get<TypeAliasExpansionConstraint>(*constraint))
        success = tryDispatch(*taec, constraint);
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

    unify(c.subType, c.superType);

    unblock(c.subType);
    unblock(c.superType);

    return true;
}

bool ConstraintSolver::tryDispatch(const PackSubtypeConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    unify(c.subPack, c.superPack);
    unblock(c.subPack);
    unblock(c.superPack);

    return true;
}

bool ConstraintSolver::tryDispatch(const GeneralizationConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    if (isBlocked(c.sourceType))
        return block(c.sourceType, constraint);

    if (isBlocked(c.generalizedType))
        asMutable(c.generalizedType)->ty.emplace<BoundTypeVar>(c.sourceType);
    else
        unify(c.generalizedType, c.sourceType);

    TypeId generalized = quantify(arena, c.sourceType, c.scope);
    *asMutable(c.sourceType) = *generalized;

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
        unify(c.subType, *instantiated);

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

    if (isBlocked(leftType) || isBlocked(rightType))
    {
        block(leftType, constraint);
        block(rightType, constraint);
        return false;
    }

    if (isNumber(leftType))
    {
        unify(leftType, rightType);
        asMutable(c.resultType)->ty.emplace<BoundTypeVar>(leftType);
        return true;
    }

    if (get<FreeTypeVar>(leftType) && !force)
        return block(leftType, constraint);

    // TODO metatables, classes

    return true;
}

bool ConstraintSolver::tryDispatch(const NameConstraint& c, NotNull<const Constraint> constraint)
{
    if (isBlocked(c.namedType))
        return block(c.namedType, constraint);

    TypeId target = follow(c.namedType);
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
    bool foundInfiniteType = false;

    explicit InfiniteTypeFinder(ConstraintSolver* solver, const InstantiationSignature& signature)
        : solver(solver)
        , signature(signature)
    {
    }

    bool visit(TypeId ty, const PendingExpansionTypeVar& petv) override
    {
        auto [typeArguments, packArguments] = saturateArguments(petv.fn, petv.typeArguments, petv.packArguments, solver->arena);

        if (follow(petv.fn.type) == follow(signature.fn.type) && (signature.arguments != typeArguments || signature.packArguments != packArguments))
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

    explicit InstantiationQueuer(ConstraintSolver* solver, const InstantiationSignature& signature)
        : solver(solver)
        , signature(signature)
    {
    }

    bool visit(TypeId ty, const PendingExpansionTypeVar& petv) override
    {
        solver->pushConstraint(TypeAliasExpansionConstraint{ty});
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

    // If there are no parameters to the type function we can just use the type
    // directly.
    if (petv->fn.typeParams.empty() && petv->fn.typePackParams.empty())
    {
        bindResult(petv->fn.type);
        return true;
    }

    auto [typeArguments, packArguments] = saturateArguments(petv->fn, petv->typeArguments, petv->packArguments, arena);

    bool sameTypes =
        std::equal(typeArguments.begin(), typeArguments.end(), petv->fn.typeParams.begin(), petv->fn.typeParams.end(), [](auto&& itp, auto&& p) {
            return itp == p.ty;
        });

    bool samePacks = std::equal(
        packArguments.begin(), packArguments.end(), petv->fn.typePackParams.begin(), petv->fn.typePackParams.end(), [](auto&& itp, auto&& p) {
            return itp == p.tp;
        });

    // If we're instantiating the type with its generic saturatedTypeArguments we are
    // performing the identity substitution. We can just short-circuit and bind
    // to the TypeFun's type.
    if (sameTypes && samePacks)
    {
        bindResult(petv->fn.type);
        return true;
    }

    InstantiationSignature signature{
        petv->fn,
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
    InfiniteTypeFinder itf{this, signature};
    itf.traverse(petv->fn.type);

    if (itf.foundInfiniteType)
    {
        // TODO (CLI-56761): Report an error.
        bindResult(getSingletonTypes().errorRecoveryType());
        return true;
    }

    ApplyTypeFunction applyTypeFunction{arena};
    for (size_t i = 0; i < typeArguments.size(); ++i)
    {
        applyTypeFunction.typeArguments[petv->fn.typeParams[i].ty] = typeArguments[i];
    }

    for (size_t i = 0; i < packArguments.size(); ++i)
    {
        applyTypeFunction.typePackArguments[petv->fn.typePackParams[i].tp] = packArguments[i];
    }

    std::optional<TypeId> maybeInstantiated = applyTypeFunction.substitute(petv->fn.type);
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
    // Type function application will happily give us the exact same type if
    // there are e.g. generic saturatedTypeArguments that go unused.
    bool needsClone = follow(petv->fn.type) == target;
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
    InstantiationQueuer queuer{this, signature};
    queuer.traverse(target);

    instantiatedAliases[signature] = target;

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
    block_(target, constraint);
}

bool ConstraintSolver::block(TypeId target, NotNull<const Constraint> constraint)
{
    block_(target, constraint);
    return false;
}

bool ConstraintSolver::block(TypePackId target, NotNull<const Constraint> constraint)
{
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

bool ConstraintSolver::isBlocked(TypeId ty)
{
    return nullptr != get<BlockedTypeVar>(follow(ty)) || nullptr != get<PendingExpansionTypeVar>(follow(ty));
}

bool ConstraintSolver::isBlocked(NotNull<const Constraint> constraint)
{
    auto blockedIt = blockedConstraints.find(constraint);
    return blockedIt != blockedConstraints.end() && blockedIt->second > 0;
}

void ConstraintSolver::unify(TypeId subType, TypeId superType)
{
    UnifierSharedState sharedState{&iceReporter};
    Unifier u{arena, Mode::Strict, Location{}, Covariant, sharedState};

    u.tryUnify(subType, superType);
    u.log.commit();
}

void ConstraintSolver::unify(TypePackId subPack, TypePackId superPack)
{
    UnifierSharedState sharedState{&iceReporter};
    Unifier u{arena, Mode::Strict, Location{}, Covariant, sharedState};

    u.tryUnify(subPack, superPack);
    u.log.commit();
}

void ConstraintSolver::pushConstraint(ConstraintV cv)
{
    std::unique_ptr<Constraint> c = std::make_unique<Constraint>(std::move(cv));
    NotNull<Constraint> borrow = NotNull(c.get());
    solverConstraints.push_back(std::move(c));
    unsolvedConstraints.push_back(borrow);
}

} // namespace Luau
