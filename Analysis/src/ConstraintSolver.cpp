// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/ConstraintSolver.h"
#include "Luau/Instantiation.h"
#include "Luau/Quantify.h"
#include "Luau/ToString.h"
#include "Luau/Unifier.h"

LUAU_FASTFLAGVARIABLE(DebugLuauLogSolver, false);

namespace Luau
{

[[maybe_unused]] static void dumpBindings(Scope2* scope, ToStringOptions& opts)
{
    for (const auto& [k, v] : scope->bindings)
    {
        auto d = toStringDetailed(v, opts);
        opts.nameMap = d.nameMap;
        printf("\t%s : %s\n", k.c_str(), d.name.c_str());
    }

    for (Scope2* child : scope->children)
        dumpBindings(child, opts);
}

static void dumpConstraints(Scope2* scope, ToStringOptions& opts)
{
    for (const ConstraintPtr& c : scope->constraints)
    {
        printf("\t%s\n", toString(*c, opts).c_str());
    }

    for (Scope2* child : scope->children)
        dumpConstraints(child, opts);
}

void dump(Scope2* rootScope, ToStringOptions& opts)
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

ConstraintSolver::ConstraintSolver(TypeArena* arena, Scope2* rootScope)
    : arena(arena)
    , constraints(collectConstraints(rootScope))
    , rootScope(rootScope)
{
    for (const Constraint* c : constraints)
    {
        unsolvedConstraints.insert(c);

        for (const Constraint* dep : c->dependencies)
        {
            block(dep, c);
        }
    }
}

void ConstraintSolver::run()
{
    if (done())
        return;

    bool progress = false;

    ToStringOptions opts;

    if (FFlag::DebugLuauLogSolver)
    {
        printf("Starting solver\n");
        dump(this, opts);
    }

    do
    {
        progress = false;

        auto it = begin(unsolvedConstraints);
        auto endIt = end(unsolvedConstraints);

        while (it != endIt)
        {
            if (isBlocked(*it))
            {
                ++it;
                continue;
            }

            std::string saveMe = FFlag::DebugLuauLogSolver ? toString(**it, opts) : std::string{};

            bool success = tryDispatch(*it);
            progress = progress || success;

            auto saveIt = it;
            ++it;
            if (success)
            {
                unsolvedConstraints.erase(saveIt);
                if (FFlag::DebugLuauLogSolver)
                {
                    printf("Dispatched\n\t%s\n", saveMe.c_str());
                    dump(this, opts);
                }
            }
        }
    } while (progress);

    if (FFlag::DebugLuauLogSolver)
        dumpBindings(rootScope, opts);

    LUAU_ASSERT(done());
}

bool ConstraintSolver::done()
{
    return unsolvedConstraints.empty();
}

bool ConstraintSolver::tryDispatch(const Constraint* constraint)
{
    if (isBlocked(constraint))
        return false;

    bool success = false;

    if (auto sc = get<SubtypeConstraint>(*constraint))
        success = tryDispatch(*sc);
    else if (auto psc = get<PackSubtypeConstraint>(*constraint))
        success = tryDispatch(*psc);
    else if (auto gc = get<GeneralizationConstraint>(*constraint))
        success = tryDispatch(*gc);
    else if (auto ic = get<InstantiationConstraint>(*constraint))
        success = tryDispatch(*ic, constraint);
    else
        LUAU_ASSERT(0);

    if (success)
    {
        unblock(constraint);
    }

    return success;
}

bool ConstraintSolver::tryDispatch(const SubtypeConstraint& c)
{
    unify(c.subType, c.superType);
    unblock(c.subType);
    unblock(c.superType);

    return true;
}

bool ConstraintSolver::tryDispatch(const PackSubtypeConstraint& c)
{
    unify(c.subPack, c.superPack);
    unblock(c.subPack);
    unblock(c.superPack);

    return true;
}

bool ConstraintSolver::tryDispatch(const GeneralizationConstraint& constraint)
{
    unify(constraint.subType, constraint.superType);

    quantify(constraint.superType, constraint.scope);
    unblock(constraint.subType);
    unblock(constraint.superType);

    return true;
}

bool ConstraintSolver::tryDispatch(const InstantiationConstraint& c, const Constraint* constraint)
{
    TypeId superType = follow(c.superType);
    if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(superType))
    {
        if (!ftv->generalized)
        {
            block(superType, constraint);
            return false;
        }
    }
    else if (get<FreeTypeVar>(superType))
    {
        block(superType, constraint);
        return false;
    }
    // TODO: Error if it's a primitive or something

    Instantiation inst(TxnLog::empty(), arena, TypeLevel{});

    std::optional<TypeId> instantiated = inst.substitute(c.superType);
    LUAU_ASSERT(instantiated); // TODO FIXME HANDLE THIS

    unify(c.subType, *instantiated);
    unblock(c.subType);

    return true;
}

void ConstraintSolver::block_(BlockedConstraintId target, const Constraint* constraint)
{
    blocked[target].push_back(constraint);

    auto& count = blockedConstraints[constraint];
    count += 1;
}

void ConstraintSolver::block(const Constraint* target, const Constraint* constraint)
{
    block_(target, constraint);
}

void ConstraintSolver::block(TypeId target, const Constraint* constraint)
{
    block_(target, constraint);
}

void ConstraintSolver::block(TypePackId target, const Constraint* constraint)
{
    block_(target, constraint);
}

void ConstraintSolver::unblock_(BlockedConstraintId progressed)
{
    auto it = blocked.find(progressed);
    if (it == blocked.end())
        return;

    // unblocked should contain a value always, because of the above check
    for (const Constraint* unblockedConstraint : it->second)
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

void ConstraintSolver::unblock(const Constraint* progressed)
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

bool ConstraintSolver::isBlocked(const Constraint* constraint)
{
    auto blockedIt = blockedConstraints.find(constraint);
    return blockedIt != blockedConstraints.end() && blockedIt->second > 0;
}

void ConstraintSolver::reportErrors(const std::vector<TypeError>& errors)
{
    this->errors.insert(end(this->errors), begin(errors), end(errors));
}

void ConstraintSolver::unify(TypeId subType, TypeId superType)
{
    UnifierSharedState sharedState{&iceReporter};
    Unifier u{arena, Mode::Strict, Location{}, Covariant, sharedState};

    u.tryUnify(subType, superType);
    u.log.commit();
    reportErrors(u.errors);
}

void ConstraintSolver::unify(TypePackId subPack, TypePackId superPack)
{
    UnifierSharedState sharedState{&iceReporter};
    Unifier u{arena, Mode::Strict, Location{}, Covariant, sharedState};

    u.tryUnify(subPack, superPack);
    u.log.commit();
    reportErrors(u.errors);
}

} // namespace Luau
