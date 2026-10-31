// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeStateMap.h"

#include "Luau/Common.h"
#include "Luau/Constraint.h"
#include "Luau/ControlFlowGraph.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/BuiltinTypeFunctions.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeUtils.h"
#include "Luau/ToString.h"
#include "Luau/Variant.h"
#include "Luau/DumpCFG.h"
#include <utility>


LUAU_FASTINTVARIABLE(LuauMaxCFGDataflowIterations, 2);
LUAU_FASTFLAG(DebugLuauLogCFG)
namespace Luau
{

TypeStateMap::TypeStateMap(NotNull<TypeArena> arena, NotNull<Scope> globalScope, NotNull<BuiltinTypes> builtinTypes, NotNull<ControlFlowGraph> g)
    : arena(arena)
    , builtinTypes(builtinTypes)
    , g(g)
    , globalScope(globalScope)
{
}

std::vector<ConstraintV> TypeStateMap::flushConstraintsRequiringFinalization()
{
    std::vector<ConstraintV> allConstraints;
    for (auto& [ty, constraints] : typesRequiringConstraints)
    {
        for (auto& cv : constraints)
            allConstraints.emplace_back(std::move(cv));
    }
    typesRequiringConstraints.clear();
    return allConstraints;
}

TypeId TypeStateMap::getRHSType(AstExpr* expr)
{
    if (Definition* def = g->getUseDef(expr))
        return getType(NotNull{def});
    return builtinTypes->errorType;
}

TypeId TypeStateMap::getDomain(Symbol sym) const
{
    if (auto domain = lvalueDomain.find(sym))
        return *domain;
    return builtinTypes->anyType;
}

void TypeStateMap::expandDomain(const LHS& lv, TypeId rhs)
{
    Definition* def = g->getLhsDef(lv);
    if (!def)
        return;

    SSAId ssa = NotNull{def};
    TypeId* stateTy = defTypes.find(ssa);
    if (!stateTy)
    {
        LUAU_ASSERT(!"Couldn't find type for LHS definition - TypeStateMap was not fully populated");
        return;
    }

    TypeId followedStateTy = follow(*stateTy);
    if (auto bt = get<BlockedType>(followedStateTy))
    {
        if (bt->getOwner() != nullptr)
        {
            LUAU_ASSERT(!"Expected the blocked type having it's domain expanded to have no owner - this means it came from somewhere outside the "
                         "typestate map");
            return;
        }


        emplaceType<BoundType>(asMutable(followedStateTy), rhs);
    }
}

std::vector<ConstraintV> TypeStateMap::getConstraints(TypeId ty) const
{
    if (auto constraints = typesRequiringConstraints.find(ty))
        return *constraints;
    return {};
}

TypeId TypeStateMap::getType(SSAId def) const
{
    LUAU_ASSERT(def->materialize() == def);
    if (auto ty = defTypes.find(def))
        return *ty;
    else
    {
        LUAU_ASSERT(!"Couldn't find type for definition - you probably forgot to allocate a type during CFG traversal");
        return builtinTypes->errorType;
    }
}

void TypeStateMap::addConstraintForType(TypeId ty, ConstraintV&& constraint)
{
    std::vector<ConstraintV>& constraints = typesRequiringConstraints[ty];
    constraints.emplace_back(std::move(constraint));
}

TypeId TypeStateMap::newBlockedType(SSAId d)
{
    TypeId t = arena->addType(BlockedType{});
    if (FFlag::DebugLuauLogCFG)
        printf("%s -> %s\n", d->versionedName().c_str(), toString(t).c_str());
    return t;
}

void TypeStateMap::computeTypes()
{
    for (int iter = 0; iter < FInt::LuauMaxCFGDataflowIterations; iter++)
    {
        for (Block* blk : g->rpo())
        {
            for (auto inst : blk->getInstructions())
                handleInstruction(inst);
        }
    }

    if (FFlag::DebugLuauLogCFG)
    {
        fprintf(stderr, "=== TypeStateMap: def -> type assignments ===\n");
        for (const auto& [def, ty] : defTypes)
        {
            TypeId followed = follow(ty);
            fprintf(stderr, "  %s -> %s\n", def->versionedName().c_str(), toString(followed).c_str());
        }
        fprintf(stderr, "=== end TypeStateMap ===\n");
    }
}

void TypeStateMap::finalizeDomains()
{
    for (const auto& [domain, states] : domainStates)
    {
        UnionBuilder ub{arena, builtinTypes};
        ub.reserve(states.size());

        for (const auto& state : states)
            ub.add(state.second);

        TypeId domainTy = follow(domain);
        if (get<BlockedType>(domainTy))
            emplaceType<BoundType>(asMutable(domainTy), ub.build());

        addConstraintForType(domain, SimplifyConstraint{domain});
    }
}

void TypeStateMap::handleInstruction(InstructionId id)
{
    visit(
        overloaded{
            [&](const Dead&)
            {
                LUAU_ASSERT(!"Impossible - dead instructions cannot exist in a well formed CFG");
            },
            [&](const Declare& decl)
            {
                if (defTypes.find(decl.def))
                    return;
                TypeId rvalue = newBlockedType(decl.def);
                defTypes[decl.def] = rvalue;

                TypeId domainPlaceholder = arena->addType(BlockedType{});
                lvalueDomain[decl.def->getSym()] = domainPlaceholder;
                domainStates[domainPlaceholder][decl.def] = rvalue;
            },
            [&](const GlobalFunction& fn)
            {
                return;
            },
            [&](const LocalFunction& fn)
            {
                if (defTypes.find(fn.def))
                    return;

                // A local function introduces the domain of its name but no rhs type:
                // the function's type is produced by the ConstraintGenerator, which owns
                // and binds this domain placeholder. Because there is no rhs state to
                // union over, we skip domainStates so finalizeDomains leaves the
                // placeholder blocked for the ConstraintGenerator to resolve.
                TypeId domainPlaceholder = arena->addType(BlockedType{});
                lvalueDomain[fn.def->getSym()] = domainPlaceholder;
                defTypes[fn.def] = domainPlaceholder;
            },
            [&](const Arg& arg)
            {
                if (defTypes.find(arg.def))
                    return;
                // A parameter behaves like a Declare (mutable local with a domain), but
                // its initial value has no rhs expression - the ConstraintGenerator binds
                // this blocked type to the signature's argument type.
                TypeId argValue = newBlockedType(arg.def);
                defTypes[arg.def] = argValue;

                TypeId domainPlaceholder = arena->addType(BlockedType{});
                lvalueDomain[arg.def->getSym()] = domainPlaceholder;
                domainStates[domainPlaceholder][arg.def] = argValue;
            },
            [&](const Upvalue& upval)
            {
                // When a def is sourced as a capture, it does not participate in type stating.
                // Instead, it obtains the same type as its domain.
                // Future reads/writes that kill this def will cause the typestate to change
                if (defTypes.find(upval.def))
                    return;
                if (auto t = lvalueDomain.find(upval.s))
                {
                    defTypes[upval.def] = *t;
                }
            },
            [&](const Assign& assign)
            {
                if (defTypes.find(assign.def))
                    return;
                TypeId rvalue = newBlockedType(assign.def);
                defTypes[assign.def] = rvalue;

                // Assignment instructions extend the domain we observe
                if (auto* domainPlaceholder = lvalueDomain.find(assign.def->getSym()))
                    domainStates[*domainPlaceholder][assign.def] = rvalue;
            },
            [&](const Refine& refine)
            {
                if (defTypes.find(refine.definition))
                    return;
                if (auto ty = defTypes.find(refine.target))
                {
                    auto dt = getDiscriminantOf(refine.constraint);

                    TypeId result = arena->addTypeFunction(builtinTypes->typeFunctions->refineFunc, {*ty, dt}, {});
                    defTypes[refine.definition] = result;
                    addConstraintForType(result, ReduceConstraint{result});
                }
                else
                {
                    fprintf(stderr, "Refine: could not find type for def '%s'\n", refine.target->versionedName().c_str());
                    LUAU_ASSERT(false);
                }
            },

            [&](const Join& join)
            {
                auto existingTy = defTypes.find(join.definition);
                // We resolved a type for this join instruction on the first pass so there is nothing to do here
                if (existingTy && !get<BlockedType>(*existingTy))
                    return;

                // At this point, we know either existingType is nil (first pass) or existing type is a blocked type
                std::vector<TypeId> operands;
                bool missingOp = false;
                for (const auto& op : join.operands)
                {
                    if (auto opTy = defTypes.find(NotNull{op}))
                        operands.emplace_back(*opTy);
                    else
                    {
                        missingOp = true;
                        break;
                    }
                }

                if (missingOp)
                {
                    defTypes[join.definition] = newBlockedType(join.definition);
                    return;
                }

                auto finalizeType = [&]()
                {
                    auto ub = UnionBuilder{arena, builtinTypes};
                    ub.reserve(operands.size());
                    for (auto& opTy : operands)
                        ub.add(opTy);
                    return ub.build();
                };

                auto result = finalizeType();

                if (existingTy)
                    emplaceType<BoundType>(asMutable(*existingTy), result);
                else
                    defTypes[join.definition] = result;

                auto typeToConstrain = existingTy ? *existingTy : result;

                addConstraintForType(typeToConstrain, SimplifyConstraint{typeToConstrain});
            },
        } // namespace Luau
        ,
        *id.get()
    );
}

TypeId TypeStateMap::getDiscriminantOf(const NarrowingConstraint& constraint)
{
    auto getDiscriminant = [&](std::string_view name, bool isTypeOf, bool sense) -> TypeId
    {
        TypeId discriminantTy = builtinTypes->neverType;
        if (name == "nil")
            discriminantTy = builtinTypes->nilType;
        else if (name == "string")
            discriminantTy = builtinTypes->stringType;
        else if (name == "number")
            discriminantTy = builtinTypes->numberType;
        else if (name == "integer")
            discriminantTy = builtinTypes->integerType;
        else if (name == "boolean")
            discriminantTy = builtinTypes->booleanType;
        else if (name == "thread")
            discriminantTy = builtinTypes->threadType;
        else if (name == "buffer")
            discriminantTy = builtinTypes->bufferType;
        else if (name == "table")
            discriminantTy = builtinTypes->tableType;
        else if (name == "function")
            discriminantTy = builtinTypes->functionType;
        else if (name == "userdata")
        {
            discriminantTy = builtinTypes->externType;
        }
        else if (name == "vector" && !isTypeOf)
        {
            // `vector` is defined in EmbeddedBuiltinDefinitions, not as an actual built-in type
            if (auto typeFun = globalScope->lookupType("vector"))
                discriminantTy = follow(typeFun->type);
        }
        else if (!isTypeOf)
            discriminantTy = builtinTypes->neverType;
        else if (auto typeFun = globalScope->lookupType(std::string(name)); typeFun && typeFun->typeParams.empty() && typeFun->typePackParams.empty())
        {
            TypeId ty = follow(typeFun->type);

            // Only accept the root of an extern-type chain (or anything tagged as a
            // typeof root). Anything else stays `never` and produces an empty
            // refinement.
            if (auto etv = get<ExternType>(ty); etv && (etv->parent == builtinTypes->externType || hasTag(ty, kTypeofRootTag)))
                discriminantTy = ty;
        }

        // sense=false flips the proposition: the branch is taken when the def is
        // *not* of `discriminantTy`, so the discriminant we feed to refine<...> is
        // the negation.
        if (!sense)
        {
            if (auto nt = get<NegationType>(discriminantTy))
                discriminantTy = nt->ty;
            // Avoids allocating a new negation
            else if (discriminantTy == builtinTypes->falsyType)
                discriminantTy = builtinTypes->truthyType;
            else
                discriminantTy = arena->addType(NegationType{discriminantTy});
        }

        return discriminantTy;
    };

    auto getSingletonDiscriminant = [&](const SingletonValue& value, bool sense) -> TypeId
    {
        TypeId discriminantTy = Luau::visit(
            overloaded{
                [&](const StringSingletonValue& stringValue) -> TypeId
                {
                    return arena->addType(SingletonType{StringSingleton{stringValue.value}});
                },
                [&](const BooleanSingletonValue& booleanValue) -> TypeId
                {
                    return arena->addType(SingletonType{BooleanSingleton{booleanValue.value}});
                },
                [&](const NilSingletonValue& nilValue) -> TypeId
                {
                    return builtinTypes->nilType;
                },
            },
            value
        );

        if (!sense)
            discriminantTy = arena->addType(NegationType{discriminantTy});

        return discriminantTy;
    };

    TypeId disc = Luau::visit(
        overloaded{

            [&](const Truthy&) -> TypeId
            {
                return builtinTypes->truthyType;
            },
            [&](const Falsy&) -> TypeId
            {
                return builtinTypes->falsyType;
            },
            [&](const TypeEq& eq) -> TypeId
            {
                return getDiscriminant(eq.type, eq.isTypeOf, true);
            },
            [&](const TypeNeq& neq) -> TypeId
            {
                return getDiscriminant(neq.type, neq.isTypeOf, false);
            },
            [&](const SingletonEq& eq) -> TypeId
            {
                return getSingletonDiscriminant(eq.v, true);
            },
            [&](const SingletonNeq& neq) -> TypeId
            {
                return getSingletonDiscriminant(neq.v, false);
            },
            [&](const HasShape& hs) -> TypeId
            {
                // Resolve the leaf constraint to a discriminant type
                TypeId leafDisc = Luau::visit(
                    overloaded{
                        [&](const Truthy&) -> TypeId
                        {
                            return builtinTypes->truthyType;
                        },
                        [&](const Falsy&) -> TypeId
                        {
                            return builtinTypes->falsyType;
                        },
                        [&](const TypeEq& eq) -> TypeId
                        {
                            return getDiscriminant(eq.type, eq.isTypeOf, true);
                        },
                        [&](const TypeNeq& neq) -> TypeId
                        {
                            return getDiscriminant(neq.type, neq.isTypeOf, false);
                        },
                        [&](const SingletonEq& eq) -> TypeId
                        {
                            return getSingletonDiscriminant(eq.v, true);
                        },
                        [&](const SingletonNeq& neq) -> TypeId
                        {
                            return getSingletonDiscriminant(neq.v, false);
                        },
                    },
                    hs.constraint
                );

                // Build nested table type inside-out: {path[n-1]: {... {path[0]: leafDisc}}}
                TypeId inner = leafDisc;
                for (auto it = hs.path.rbegin(); it != hs.path.rend(); ++it)
                {
                    TypeId tab = arena->addType(TableType{});
                    TableType* tt = getMutable<TableType>(tab);
                    tt->state = TableState::Sealed;
                    tt->props[*it] = Property::readonly(inner);
                    inner = tab;
                }
                return inner;
            },
        },
        constraint
    );

    return disc;
}

} // namespace Luau
