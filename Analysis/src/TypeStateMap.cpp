// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeStateMap.h"

#include "Luau/Common.h"
#include "Luau/Constraint.h"
#include "Luau/ControlFlowGraph.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/BuiltinTypeFunctions.h"
#include "Luau/TypeUtils.h"
#include <algorithm>


LUAU_FASTINTVARIABLE(LuauMaxCFGDataflowIterations, 2);

namespace Luau::CFG
{

TypeStateMap::TypeStateMap(NotNull<TypeArena> arena, NotNull<Scope> globalScope, NotNull<BuiltinTypes> builtinTypes, NotNull<ControlFlowGraph> g)
    : arena(arena)
    , builtinTypes(builtinTypes)
    , g(g)
    , globalScope(globalScope)
{
}

TypeId TypeStateMap::getRHSType(AstExpr* expr)
{
    if (Definition* def = g->getUseDef(expr))
        return getType(def);
    return builtinTypes->errorType;
}

TypeId TypeStateMap::getLHSType(const LValue& lv) const
{
    if (Definition* def = g->getLhsDef(lv))
        return getType(def);
    return builtinTypes->anyType;
}

std::optional<ConstraintV> TypeStateMap::getOptionalConstraint(TypeId ty) const
{
    if (auto cv = typesRequiringConstraint.find(ty))
        return {*cv};
    return std::nullopt;
}

TypeId TypeStateMap::getType(Definition* def) const
{
    if (auto ty = defTypes.find(def))
        return *ty;
    else
    {
        LUAU_ASSERT(!"Couldn't find type for definition - you probably forgot to allocate a type during CFG traversal");
        return builtinTypes->errorType;
    }
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
}

void TypeStateMap::handleInstruction(InstrId id)
{
    visit(
        overloaded{
            [&](const Dead&) {},
            [&](const Declare& decl)
            {
                if (defTypes.find(decl.def.get()))
                    return;
                defTypes[decl.def.get()] = arena->addType(BlockedType{});
            },
            [&](const Assign& assign)
            {
                if (defTypes.find(assign.def.get()))
                    return;
                defTypes[assign.def.get()] = arena->addType(BlockedType{});
            },
            [&](const Refine& refine)
            {
                if (defTypes.find(refine.definition))
                    return;
                if (auto ty = defTypes.find(refine.toRefine))
                {
                    auto dt = getDiscriminantOf(refine);

                    TypeId result = arena->addTypeFunction(builtinTypes->typeFunctions->refineFunc, {*ty, dt}, {});
                    defTypes[refine.definition] = result;
                    typesRequiringConstraint[result] = ReduceConstraint{result};
                }
                else
                {
                    fprintf(stderr, "Refine: could not find type for def '%s'\n", refine.toRefine->versionedName().c_str());
                    LUAU_ASSERT(false);
                }
            },
            [&](const Join& join)
            {
                auto existingTy = defTypes.find(join.definition.get());
                // We resolved a type for this join instruction on the first pass so there is nothing to do here
                if (existingTy && !get<BlockedType>(*existingTy))
                    return;

                // At this point, we know either existingType is nil (first pass) or existing type is a blocked type
                std::vector<TypeId> operands;
                bool missingOp = false;
                for (const auto& op : join.operands)
                {
                    Definition* resolved = g->resolve(op);
                    if (auto opTy = defTypes.find(resolved))
                        operands.emplace_back(*opTy);
                    else
                    {
                        missingOp = true;
                        break;
                    }
                }

                if (missingOp)
                {
                    defTypes[join.definition.get()] = arena->addType(BlockedType{});
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
                    defTypes[join.definition.get()] = result;

                auto typeToConstrain = existingTy ? *existingTy : result;
                typesRequiringConstraint[typeToConstrain] = SimplifyConstraint{typeToConstrain};
            }
        },
        *id.get()
    );
}

TypeId TypeStateMap::getDiscriminantOf(const Refine& refine)
{
    if (!refine.type.has_value())
        return refine.sense ? builtinTypes->truthyType : builtinTypes->falsyType;

    LUAU_ASSERT(refine.type.has_value());
    const std::string& name = *refine.type;

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
        // typeof("userdata") collapses to the extern-type root; the precise
        // class hierarchy is irrelevant for the purposes of refinement.
        discriminantTy = builtinTypes->externType;
    }
    else if (auto typeFun = globalScope->lookupType(name); typeFun && typeFun->typeParams.empty() && typeFun->typePackParams.empty())
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
    if (!refine.sense)
    {
        if (auto nt = get<NegationType>(discriminantTy))
            discriminantTy = nt->ty;
        else
            discriminantTy = arena->addType(NegationType{discriminantTy});
    }


    return discriminantTy;
}

} // namespace Luau::CFG
