// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details

#include "lluz/ConstraintSolver.h"
#include "lluz/Instantiation.h"
#include "lluz/Location.h"
#include "lluz/Quantify.h"
#include "lluz/ToString.h"
#include "lluz/Unifier.h"

lluz_FASTFLAGVARIABLE(DebugLluLogSolver, false);
lluz_FASTFLAGVARIABLE(DebugLluLogSolverToJson, false);

namespace lluz
{

[[maybe_unused]] static void dumpBindings(NotNull<Scope2> scope, ToStringOptions& opts)
{
    for (const auto& [k, v] : scope->bindings)
    {
        auto d = toStringDetailed(v, opts);
        opts.nameMap = d.nameMap;
        printf("\t%s : %s\n", k.c_str(), d.name.c_str());
    }

    for (NotNull<Scope2> child : scope->children)
        dumpBindings(child, opts);
}

static void dumpConstraints(NotNull<Scope2> scope, ToStringOptions& opts)
{
    for (const ConstraintPtr& c : scope->constraints)
    {
        printf("\t%s\n", toString(*c, opts).c_str());
    }

    for (NotNull<Scope2> child : scope->children)
        dumpConstraints(child, opts);
}

void dump(NotNull<Scope2> rootScope, ToStringOptions& opts)
{
    printf(XorStr("constraints:\n"));
    dumpConstraints(rootScope, opts);
}

void dump(ConstraintSolver* cs, ToStringOptions& opts)
{
    printf(XorStr("constraints:\n"));
    for (const Constraint* c : cs->unsolvedConstraints)
    {
        printf("\t%s\n", toString(*c, opts).c_str());

        for (const Constraint* dep : c->dependencies)
            printf("\t\t%s\n", toString(*dep, opts).c_str());
    }
}

ConstraintSolver::ConstraintSolver(TypeArena* arena, NotNull<Scope2> rootScope)
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

    if (FFlag::DebugLluLogSolver)
    {
        printf(XorStr("Starting solver\n"));
        dump(this, opts);
    }

    if (FFlag::DebugLluLogSolverToJson)
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

            std::string saveMe = FFlag::DebugLluLogSolver ? toString(*c, opts) : std::string{};

            if (FFlag::DebugLluLogSolverToJson)
            {
                logger.prepareStepSnapshot(rootScope, c, unsolvedConstraints);
            }

            bool success = tryDispatch(c, force);

            progress |= success;

            if (success)
            {
                unsolvedConstraints.erase(unsolvedConstraints.begin() + i);

                if (FFlag::DebugLluLogSolverToJson)
                {
                    logger.commitPreparedStepSnapshot();
                }

                if (FFlag::DebugLluLogSolver)
                {
                    if (force)
                        printf(XorStr("Force "));
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

    if (FFlag::DebugLluLogSolver)
    {
        dumpBindings(rootScope, opts);
    }

    if (FFlag::DebugLluLogSolverToJson)
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
    else
        lluz_ASSERT(0);

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
    lluz_ASSERT(instantiated); // TODO FIXME HANDLE THIS

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

    lluz_ASSERT(get<BlockedTypeVar>(c.resultType));

    if (isNumber(operandType) || get<AnyTypeVar>(operandType) || get<ErrorTypeVar>(operandType))
    {
        asMutable(c.resultType)->ty.emplace<BoundTypeVar>(c.operandType);
        return true;
    }

    lluz_ASSERT(0); // TODO metatable handling
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
        lluz_ASSERT(count > 0);
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
    return nullptr != get<BlockedTypeVar>(follow(ty));
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

} // namespace lluz
