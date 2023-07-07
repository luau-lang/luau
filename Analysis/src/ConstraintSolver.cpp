// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Anyification.h"
#include "Luau/ApplyTypeFunction.h"
#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/DcrLogger.h"
#include "Luau/Instantiation.h"
#include "Luau/Location.h"
#include "Luau/Metamethods.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Quantify.h"
#include "Luau/Simplify.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeFamily.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier.h"
#include "Luau/VisitType.h"

LUAU_FASTFLAGVARIABLE(DebugLuauLogSolver, false);

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

static std::pair<std::vector<TypeId>, std::vector<TypePackId>> saturateArguments(TypeArena* arena, NotNull<BuiltinTypes> builtinTypes,
    const TypeFun& fn, const std::vector<TypeId>& rawTypeArguments, const std::vector<TypePackId>& rawPackArguments)
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

    bool visit(TypeId ty, const TypeFamilyInstanceType& tfit) override
    {
        solver->pushConstraint(scope, location, ReduceConstraint{ty});
        return true;
    }

    bool visit(TypeId ty, const ClassType& ctv) override
    {
        return false;
    }
};

ConstraintSolver::ConstraintSolver(NotNull<Normalizer> normalizer, NotNull<Scope> rootScope, std::vector<NotNull<Constraint>> constraints,
    ModuleName moduleName, NotNull<ModuleResolver> moduleResolver, std::vector<RequireCycle> requireCycles, DcrLogger* logger)
    : arena(normalizer->arena)
    , builtinTypes(normalizer->builtinTypes)
    , normalizer(normalizer)
    , constraints(std::move(constraints))
    , rootScope(rootScope)
    , currentModuleName(std::move(moduleName))
    , moduleResolver(moduleResolver)
    , requireCycles(requireCycles)
    , logger(logger)
{
    opts.exhaustive = true;

    for (NotNull<Constraint> c : this->constraints)
    {
        unsolvedConstraints.push_back(c);

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
    if (isDone())
        return;

    if (FFlag::DebugLuauLogSolver)
    {
        printf("Starting solver\n");
        dump(this, opts);
        printf("Bindings:\n");
        dumpBindings(rootScope, opts);
    }

    if (logger)
    {
        logger->captureInitialSolverState(rootScope, unsolvedConstraints);
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

    finalizeModule();

    if (FFlag::DebugLuauLogSolver)
    {
        dumpBindings(rootScope, opts);
    }

    if (logger)
    {
        logger->captureFinalSolverState(rootScope, unsolvedConstraints);
    }
}

bool ConstraintSolver::isDone()
{
    return unsolvedConstraints.empty();
}

void ConstraintSolver::finalizeModule()
{
    Anyification a{arena, rootScope, builtinTypes, &iceReporter, builtinTypes->anyType, builtinTypes->anyTypePack};
    std::optional<TypePackId> returnType = a.substitute(rootScope->returnType);
    if (!returnType)
    {
        reportError(CodeTooComplex{}, Location{});
        rootScope->returnType = builtinTypes->errorTypePack;
    }
    else
    {
        rootScope->returnType = anyifyModuleReturnTypePackGenerics(*returnType);
    }
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
    else if (auto fcc = get<PrimitiveTypeConstraint>(*constraint))
        success = tryDispatch(*fcc, constraint);
    else if (auto hpc = get<HasPropConstraint>(*constraint))
        success = tryDispatch(*hpc, constraint);
    else if (auto spc = get<SetPropConstraint>(*constraint))
        success = tryDispatch(*spc, constraint, force);
    else if (auto spc = get<SetIndexerConstraint>(*constraint))
        success = tryDispatch(*spc, constraint, force);
    else if (auto sottc = get<SingletonOrTopTypeConstraint>(*constraint))
        success = tryDispatch(*sottc, constraint);
    else if (auto uc = get<UnpackConstraint>(*constraint))
        success = tryDispatch(*uc, constraint);
    else if (auto rc = get<RefineConstraint>(*constraint))
        success = tryDispatch(*rc, constraint, force);
    else if (auto rc = get<ReduceConstraint>(*constraint))
        success = tryDispatch(*rc, constraint, force);
    else if (auto rpc = get<ReducePackConstraint>(*constraint))
        success = tryDispatch(*rpc, constraint, force);
    else
        LUAU_ASSERT(false);

    if (success)
        unblock(constraint);

    return success;
}

bool ConstraintSolver::tryDispatch(const SubtypeConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    if (isBlocked(c.subType))
        return block(c.subType, constraint);
    else if (isBlocked(c.superType))
        return block(c.superType, constraint);

    return tryUnify(constraint, c.subType, c.superType);
}

bool ConstraintSolver::tryDispatch(const PackSubtypeConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    if (isBlocked(c.subPack))
        return block(c.subPack, constraint);
    else if (isBlocked(c.superPack))
        return block(c.superPack, constraint);

    return tryUnify(constraint, c.subPack, c.superPack);
}

bool ConstraintSolver::tryDispatch(const GeneralizationConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    TypeId generalizedType = follow(c.generalizedType);

    if (isBlocked(c.sourceType))
        return block(c.sourceType, constraint);
    else if (get<PendingExpansionType>(generalizedType))
        return block(generalizedType, constraint);

    std::optional<QuantifierResult> generalized = quantify(arena, c.sourceType, constraint->scope);
    if (generalized)
    {
        if (get<BlockedType>(generalizedType))
            asMutable(generalizedType)->ty.emplace<BoundType>(generalized->result);
        else
            unify(generalizedType, generalized->result, constraint->scope);

        for (auto [free, gen] : generalized->insertedGenerics.pairings)
            unify(free, gen, constraint->scope);

        for (auto [free, gen] : generalized->insertedGenericPacks.pairings)
            unify(free, gen, constraint->scope);
    }
    else
    {
        reportError(CodeTooComplex{}, constraint->location);
        asMutable(c.generalizedType)->ty.emplace<BoundType>(builtinTypes->errorRecoveryType());
    }

    unblock(c.generalizedType, constraint->location);
    unblock(c.sourceType, constraint->location);

    return true;
}

bool ConstraintSolver::tryDispatch(const InstantiationConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    if (isBlocked(c.superType))
        return block(c.superType, constraint);

    if (!blockOnPendingTypes(c.superType, constraint))
        return false;

    Instantiation inst(TxnLog::empty(), arena, TypeLevel{}, constraint->scope);

    std::optional<TypeId> instantiated = inst.substitute(c.superType);

    LUAU_ASSERT(get<BlockedType>(c.subType));

    if (!instantiated.has_value())
    {
        reportError(UnificationTooComplex{}, constraint->location);

        asMutable(c.subType)->ty.emplace<BoundType>(errorRecoveryType());
        unblock(c.subType, constraint->location);

        return true;
    }

    asMutable(c.subType)->ty.emplace<BoundType>(*instantiated);

    InstantiationQueuer queuer{constraint->scope, constraint->location, this};
    queuer.traverse(c.subType);

    unblock(c.subType, constraint->location);

    return true;
}

bool ConstraintSolver::tryDispatch(const UnaryConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    TypeId operandType = follow(c.operandType);

    if (isBlocked(operandType))
        return block(operandType, constraint);

    if (get<FreeType>(operandType))
        return block(operandType, constraint);

    LUAU_ASSERT(get<BlockedType>(c.resultType));

    switch (c.op)
    {
    case AstExprUnary::Not:
    {
        asMutable(c.resultType)->ty.emplace<BoundType>(builtinTypes->booleanType);

        unblock(c.resultType, constraint->location);
        return true;
    }
    case AstExprUnary::Len:
    {
        // __len must return a number.
        asMutable(c.resultType)->ty.emplace<BoundType>(builtinTypes->numberType);

        unblock(c.resultType, constraint->location);
        return true;
    }
    case AstExprUnary::Minus:
    {
        if (isNumber(operandType) || get<AnyType>(operandType) || get<ErrorType>(operandType) || get<NeverType>(operandType))
        {
            asMutable(c.resultType)->ty.emplace<BoundType>(c.operandType);
        }
        else if (std::optional<TypeId> mm = findMetatableEntry(builtinTypes, errors, operandType, "__unm", constraint->location))
        {
            TypeId mmTy = follow(*mm);

            if (get<FreeType>(mmTy) && !force)
                return block(mmTy, constraint);

            TypePackId argPack = arena->addTypePack(TypePack{{operandType}, {}});
            TypePackId retPack = arena->addTypePack(BlockedTypePack{});

            asMutable(c.resultType)->ty.emplace<FreeType>(constraint->scope);

            pushConstraint(constraint->scope, constraint->location, PackSubtypeConstraint{retPack, arena->addTypePack(TypePack{{c.resultType}})});

            pushConstraint(constraint->scope, constraint->location, FunctionCallConstraint{mmTy, argPack, retPack, nullptr});
        }
        else
        {
            asMutable(c.resultType)->ty.emplace<BoundType>(builtinTypes->errorRecoveryType());
        }

        unblock(c.resultType, constraint->location);
        return true;
    }
    }

    LUAU_ASSERT(false);
    return false;
}

bool ConstraintSolver::tryDispatch(const BinaryConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    TypeId leftType = follow(c.leftType);
    TypeId rightType = follow(c.rightType);
    TypeId resultType = follow(c.resultType);

    LUAU_ASSERT(get<BlockedType>(resultType));

    bool isLogical = c.op == AstExprBinary::Op::And || c.op == AstExprBinary::Op::Or;

    /* Compound assignments create constraints of the form
     *
     *     A <: Binary<op, A, B>
     *
     * This constraint is the one that is meant to unblock A, so it doesn't
     * make any sense to stop and wait for someone else to do it.
     */

    // If any is present, the expression must evaluate to any as well.
    bool leftAny = get<AnyType>(leftType) || get<ErrorType>(leftType);
    bool rightAny = get<AnyType>(rightType) || get<ErrorType>(rightType);
    bool anyPresent = leftAny || rightAny;

    if (isBlocked(leftType) && leftType != resultType)
        return block(c.leftType, constraint);

    if (isBlocked(rightType) && rightType != resultType)
        return block(c.rightType, constraint);

    if (!force)
    {
        // Logical expressions may proceed if the LHS is free.
        if (hasTypeInIntersection<FreeType>(leftType) && !isLogical)
            return block(leftType, constraint);
    }

    // Logical expressions may proceed if the LHS is free.
    if (isBlocked(leftType) || (hasTypeInIntersection<FreeType>(leftType) && !isLogical))
    {
        asMutable(resultType)->ty.emplace<BoundType>(errorRecoveryType());
        unblock(resultType, constraint->location);
        return true;
    }

    // Metatables go first, even if there is primitive behavior.
    if (auto it = kBinaryOpMetamethods.find(c.op); it != kBinaryOpMetamethods.end())
    {
        // Metatables are not the same. The metamethod will not be invoked.
        if ((c.op == AstExprBinary::Op::CompareEq || c.op == AstExprBinary::Op::CompareNe) &&
            getMetatable(leftType, builtinTypes) != getMetatable(rightType, builtinTypes))
        {
            // TODO: Boolean singleton false? The result is _always_ boolean false.
            asMutable(resultType)->ty.emplace<BoundType>(builtinTypes->booleanType);
            unblock(resultType, constraint->location);
            return true;
        }

        std::optional<TypeId> mm;

        // The LHS metatable takes priority over the RHS metatable, where
        // present.
        if (std::optional<TypeId> leftMm = findMetatableEntry(builtinTypes, errors, leftType, it->second, constraint->location))
            mm = leftMm;
        else if (std::optional<TypeId> rightMm = findMetatableEntry(builtinTypes, errors, rightType, it->second, constraint->location))
            mm = rightMm;

        if (mm)
        {
            Instantiation instantiation{TxnLog::empty(), arena, TypeLevel{}, constraint->scope};
            std::optional<TypeId> instantiatedMm = instantiation.substitute(*mm);
            if (!instantiatedMm)
            {
                reportError(CodeTooComplex{}, constraint->location);
                return true;
            }

            // TODO: Is a table with __call legal here?
            // TODO: Overloads
            if (const FunctionType* ftv = get<FunctionType>(follow(*instantiatedMm)))
            {
                TypePackId inferredArgs;
                // For >= and > we invoke __lt and __le respectively with
                // swapped argument ordering.
                if (c.op == AstExprBinary::Op::CompareGe || c.op == AstExprBinary::Op::CompareGt)
                {
                    inferredArgs = arena->addTypePack({rightType, leftType});
                }
                else
                {
                    inferredArgs = arena->addTypePack({leftType, rightType});
                }

                unify(inferredArgs, ftv->argTypes, constraint->scope);

                TypeId mmResult;

                // Comparison operations always evaluate to a boolean,
                // regardless of what the metamethod returns.
                switch (c.op)
                {
                case AstExprBinary::Op::CompareEq:
                case AstExprBinary::Op::CompareNe:
                case AstExprBinary::Op::CompareGe:
                case AstExprBinary::Op::CompareGt:
                case AstExprBinary::Op::CompareLe:
                case AstExprBinary::Op::CompareLt:
                    mmResult = builtinTypes->booleanType;
                    break;
                default:
                    if (get<NeverType>(leftType) || get<NeverType>(rightType))
                        mmResult = builtinTypes->neverType;
                    else
                        mmResult = first(ftv->retTypes).value_or(errorRecoveryType());
                }

                asMutable(resultType)->ty.emplace<BoundType>(mmResult);
                unblock(resultType, constraint->location);

                (*c.astOriginalCallTypes)[c.astFragment] = *mm;
                (*c.astOverloadResolvedTypes)[c.astFragment] = *instantiatedMm;
                return true;
            }
        }

        // If there's no metamethod available, fall back to primitive behavior.
    }

    switch (c.op)
    {
    // For arithmetic operators, if the LHS is a number, the RHS must be a
    // number as well. The result will also be a number.
    case AstExprBinary::Op::Add:
    case AstExprBinary::Op::Sub:
    case AstExprBinary::Op::Mul:
    case AstExprBinary::Op::Div:
    case AstExprBinary::Op::Pow:
    case AstExprBinary::Op::Mod:
    {
        const NormalizedType* normLeftTy = normalizer->normalize(leftType);
        if (hasTypeInIntersection<FreeType>(leftType) && force)
            asMutable(leftType)->ty.emplace<BoundType>(anyPresent ? builtinTypes->anyType : builtinTypes->numberType);
        // We want to check if the left type has tops because `any` is a valid type for the lhs
        if (normLeftTy && (normLeftTy->isExactlyNumber() || get<AnyType>(normLeftTy->tops)))
        {
            unify(leftType, rightType, constraint->scope);
            asMutable(resultType)->ty.emplace<BoundType>(anyPresent ? builtinTypes->anyType : leftType);
            unblock(resultType, constraint->location);
            return true;
        }
        else if (get<NeverType>(leftType) || get<NeverType>(rightType))
        {
            unify(leftType, rightType, constraint->scope);
            asMutable(resultType)->ty.emplace<BoundType>(builtinTypes->neverType);
            unblock(resultType, constraint->location);
            return true;
        }

        break;
    }
    // For concatenation, if the LHS is a string, the RHS must be a string as
    // well. The result will also be a string.
    case AstExprBinary::Op::Concat:
    {
        if (hasTypeInIntersection<FreeType>(leftType) && force)
            asMutable(leftType)->ty.emplace<BoundType>(anyPresent ? builtinTypes->anyType : builtinTypes->stringType);
        const NormalizedType* leftNormTy = normalizer->normalize(leftType);
        if (leftNormTy && leftNormTy->isSubtypeOfString())
        {
            unify(leftType, rightType, constraint->scope);
            asMutable(resultType)->ty.emplace<BoundType>(anyPresent ? builtinTypes->anyType : leftType);
            unblock(resultType, constraint->location);
            return true;
        }
        else if (get<NeverType>(leftType) || get<NeverType>(rightType))
        {
            unify(leftType, rightType, constraint->scope);
            asMutable(resultType)->ty.emplace<BoundType>(builtinTypes->neverType);
            unblock(resultType, constraint->location);
            return true;
        }

        break;
    }
    // Inexact comparisons require that the types be both numbers or both
    // strings, and evaluate to a boolean.
    case AstExprBinary::Op::CompareGe:
    case AstExprBinary::Op::CompareGt:
    case AstExprBinary::Op::CompareLe:
    case AstExprBinary::Op::CompareLt:
    {
        const NormalizedType* lt = normalizer->normalize(leftType);
        const NormalizedType* rt = normalizer->normalize(rightType);
        // If the lhs is any, comparisons should be valid.
        if (lt && rt && (lt->isExactlyNumber() || get<AnyType>(lt->tops)) && rt->isExactlyNumber())
        {
            asMutable(resultType)->ty.emplace<BoundType>(builtinTypes->booleanType);
            unblock(resultType, constraint->location);
            return true;
        }

        if (lt && rt && (lt->isSubtypeOfString() || get<AnyType>(lt->tops)) && rt->isSubtypeOfString())
        {
            asMutable(resultType)->ty.emplace<BoundType>(builtinTypes->booleanType);
            unblock(resultType, constraint->location);
            return true;
        }


        if (get<NeverType>(leftType) || get<NeverType>(rightType))
        {
            asMutable(resultType)->ty.emplace<BoundType>(builtinTypes->booleanType);
            unblock(resultType, constraint->location);
            return true;
        }

        break;
    }

    // == and ~= always evaluate to a boolean, and impose no other constraints
    // on their parameters.
    case AstExprBinary::Op::CompareEq:
    case AstExprBinary::Op::CompareNe:
        asMutable(resultType)->ty.emplace<BoundType>(builtinTypes->booleanType);
        unblock(resultType, constraint->location);
        return true;
    // And evalutes to a boolean if the LHS is falsey, and the RHS type if LHS is
    // truthy.
    case AstExprBinary::Op::And:
    {
        TypeId leftFilteredTy = simplifyIntersection(builtinTypes, arena, leftType, builtinTypes->falsyType).result;

        asMutable(resultType)->ty.emplace<BoundType>(simplifyUnion(builtinTypes, arena, rightType, leftFilteredTy).result);
        unblock(resultType, constraint->location);
        return true;
    }
    // Or evaluates to the LHS type if the LHS is truthy, and the RHS type if
    // LHS is falsey.
    case AstExprBinary::Op::Or:
    {
        TypeId leftFilteredTy = simplifyIntersection(builtinTypes, arena, leftType, builtinTypes->truthyType).result;

        asMutable(resultType)->ty.emplace<BoundType>(simplifyUnion(builtinTypes, arena, rightType, leftFilteredTy).result);
        unblock(resultType, constraint->location);
        return true;
    }
    default:
        iceReporter.ice("Unhandled AstExprBinary::Op for binary operation", constraint->location);
        break;
    }

    // We failed to either evaluate a metamethod or invoke primitive behavior.
    unify(leftType, errorRecoveryType(), constraint->scope);
    unify(rightType, errorRecoveryType(), constraint->scope);
    asMutable(resultType)->ty.emplace<BoundType>(errorRecoveryType());
    unblock(resultType, constraint->location);

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
    if (iteratorTail && isBlocked(*iteratorTail))
        return block_(*iteratorTail);

    {
        bool blocked = false;
        for (TypeId t : iteratorTypes)
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

    if (0 == iteratorTypes.size())
    {
        Anyification anyify{arena, constraint->scope, builtinTypes, &iceReporter, errorRecoveryType(), errorRecoveryTypePack()};
        std::optional<TypePackId> anyified = anyify.substitute(c.variables);
        LUAU_ASSERT(anyified);
        unify(*anyified, c.variables, constraint->scope);

        return true;
    }

    TypeId nextTy = follow(iteratorTypes[0]);
    if (get<FreeType>(nextTy))
        return block_(nextTy);

    if (get<FunctionType>(nextTy))
    {
        TypeId tableTy = builtinTypes->nilType;
        if (iteratorTypes.size() >= 2)
            tableTy = iteratorTypes[1];

        TypeId firstIndexTy = builtinTypes->nilType;
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
    else
        return block(c.namedType, constraint);

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
        unblock(c.target, constraint->location);
        return true;
    }

    auto bindResult = [this, &c, constraint](TypeId result) {
        LUAU_ASSERT(get<PendingExpansionType>(c.target));
        asMutable(c.target)->ty.emplace<BoundType>(result);
        unblock(c.target, constraint->location);
    };

    std::optional<TypeFun> tf = (petv->prefix) ? constraint->scope->lookupImportedType(petv->prefix->value, petv->name.value)
                                               : constraint->scope->lookupType(petv->name.value);

    if (!tf.has_value())
    {
        reportError(UnknownSymbol{petv->name.value, UnknownSymbol::Context::Type}, constraint->location);
        bindResult(errorRecoveryType());
        return true;
    }

    // If there are no parameters to the type function we can just use the type
    // directly.
    if (tf->typeParams.empty() && tf->typePackParams.empty())
    {
        bindResult(tf->type);
        return true;
    }

    auto [typeArguments, packArguments] = saturateArguments(arena, builtinTypes, *tf, petv->typeArguments, petv->packArguments);

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
    bool needsClone = follow(tf->type) == target;
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

        ttv->instantiatedTypeParams = typeArguments;
        ttv->instantiatedTypePackParams = packArguments;
        // TODO: Fill in definitionModuleName.
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

    if (isBlocked(fn))
    {
        return block(c.fn, constraint);
    }

    auto collapse = [](const auto* t) -> std::optional<TypeId> {
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
        auto [head, tail] = flatten(c.argsPack);
        head.insert(head.begin(), fn);

        argsPack = arena->addTypePack(TypePack{std::move(head), tail});
        fn = *callMm;
        asMutable(c.result)->ty.emplace<FreeTypePack>(constraint->scope);
    }
    else
    {
        const FunctionType* ftv = get<FunctionType>(fn);
        bool usedMagic = false;

        if (ftv)
        {
            if (ftv->dcrMagicFunction)
                usedMagic = ftv->dcrMagicFunction(MagicFunctionCallContext{NotNull(this), c.callSite, c.argsPack, result});

            if (ftv->dcrMagicRefinement)
                ftv->dcrMagicRefinement(MagicRefinementContext{constraint->scope, c.callSite, c.discriminantTypes});
        }

        if (!usedMagic)
            asMutable(c.result)->ty.emplace<FreeTypePack>(constraint->scope);
    }

    for (std::optional<TypeId> ty : c.discriminantTypes)
    {
        if (!ty || !isBlocked(*ty))
            continue;

        // We use `any` here because the discriminant type may be pointed at by both branches,
        // where the discriminant type is not negated, and the other where it is negated, i.e.
        // `unknown ~ unknown` and `~unknown ~ never`, so `T & unknown ~ T` and `T & ~unknown ~ never`
        // v.s.
        // `any ~ any` and `~any ~ any`, so `T & any ~ T` and `T & ~any ~ T`
        //
        // In practice, users cannot negate `any`, so this is an implementation detail we can always change.
        *asMutable(follow(*ty)) = BoundType{builtinTypes->anyType};
    }

    TypeId inferredTy = arena->addType(FunctionType{TypeLevel{}, constraint->scope.get(), argsPack, c.result});

    const NormalizedType* normFn = normalizer->normalize(fn);
    if (!normFn)
    {
        reportError(UnificationTooComplex{}, constraint->location);
        return true;
    }

    // TODO: It would be nice to not need to convert the normalized type back to
    // an intersection and flatten it.
    TypeId normFnTy = normalizer->typeFromNormal(*normFn);
    std::vector<TypeId> overloads = flattenIntersection(normFnTy);

    Instantiation inst(TxnLog::empty(), arena, TypeLevel{}, constraint->scope);

    std::vector<TypeId> arityMatchingOverloads;
    std::optional<TxnLog> bestOverloadLog;

    for (TypeId overload : overloads)
    {
        overload = follow(overload);

        std::optional<TypeId> instantiated = inst.substitute(overload);

        if (!instantiated.has_value())
        {
            reportError(UnificationTooComplex{}, constraint->location);
            return true;
        }

        Unifier u{normalizer, constraint->scope, Location{}, Covariant};
        u.enableScopeTests();

        u.tryUnify(*instantiated, inferredTy, /* isFunctionCall */ true);

        if (!u.blockedTypes.empty() || !u.blockedTypePacks.empty())
        {
            for (TypeId bt : u.blockedTypes)
                block(bt, constraint);
            for (TypePackId btp : u.blockedTypePacks)
                block(btp, constraint);
            return false;
        }

        if (const auto& e = hasUnificationTooComplex(u.errors))
            reportError(*e);

        const auto& e = hasCountMismatch(u.errors);
        bool areArgumentsCompatible = (!e || get<CountMismatch>(*e)->context != CountMismatch::Context::Arg) && get<FunctionType>(*instantiated);
        if (areArgumentsCompatible)
            arityMatchingOverloads.push_back(*instantiated);

        if (u.errors.empty())
        {
            if (c.callSite)
                (*c.astOverloadResolvedTypes)[c.callSite] = *instantiated;

            // This overload has no errors, so override the bestOverloadLog and use this one.
            bestOverloadLog = std::move(u.log);
            break;
        }
        else if (areArgumentsCompatible && !bestOverloadLog)
        {
            // This overload is erroneous. Replace its inferences with `any` iff there isn't already a TxnLog.
            bestOverloadLog = std::move(u.log);
        }
    }

    if (arityMatchingOverloads.size() == 1 && c.callSite)
    {
        // In the name of better error messages in the type checker, we provide
        // it with an instantiated function signature that matched arity, but
        // not the requisite subtyping requirements. This makes errors better in
        // cases where only one overload fit from an arity perspective.
        (*c.astOverloadResolvedTypes)[c.callSite] = arityMatchingOverloads.at(0);
    }

    // We didn't find any overload that were a viable candidate, so replace the inferences with `any`.
    if (!bestOverloadLog)
    {
        Unifier u{normalizer, constraint->scope, Location{}, Covariant};
        u.enableScopeTests();

        u.tryUnify(inferredTy, builtinTypes->anyType);
        u.tryUnify(fn, builtinTypes->anyType);

        bestOverloadLog = std::move(u.log);
    }

    const auto [changedTypes, changedPacks] = bestOverloadLog->getChanges();
    bestOverloadLog->commit();

    unblock(changedTypes, constraint->location);
    unblock(changedPacks, constraint->location);
    unblock(c.result, constraint->location);

    InstantiationQueuer queuer{constraint->scope, constraint->location, this};
    queuer.traverse(fn);
    queuer.traverse(inferredTy);

    return true;
}

bool ConstraintSolver::tryDispatch(const PrimitiveTypeConstraint& c, NotNull<const Constraint> constraint)
{
    TypeId expectedType = follow(c.expectedType);
    if (isBlocked(expectedType) || get<PendingExpansionType>(expectedType))
        return block(expectedType, constraint);

    LUAU_ASSERT(get<BlockedType>(c.resultType));

    TypeId bindTo = maybeSingleton(expectedType) ? c.singletonType : c.multitonType;
    asMutable(c.resultType)->ty.emplace<BoundType>(bindTo);
    unblock(c.resultType, constraint->location);

    return true;
}

bool ConstraintSolver::tryDispatch(const HasPropConstraint& c, NotNull<const Constraint> constraint)
{
    TypeId subjectType = follow(c.subjectType);

    LUAU_ASSERT(get<BlockedType>(c.resultType));

    if (isBlocked(subjectType) || get<PendingExpansionType>(subjectType))
        return block(subjectType, constraint);

    if (get<FreeType>(subjectType))
    {
        TableType& ttv = asMutable(subjectType)->ty.emplace<TableType>(TableState::Free, TypeLevel{}, constraint->scope);
        ttv.props[c.prop] = Property{c.resultType};
        asMutable(c.resultType)->ty.emplace<FreeType>(constraint->scope);
        unblock(c.resultType, constraint->location);
        return true;
    }

    auto [blocked, result] = lookupTableProp(subjectType, c.prop, c.suppressSimplification);
    if (!blocked.empty())
    {
        for (TypeId blocked : blocked)
            block(blocked, constraint);

        return false;
    }

    bindBlockedType(c.resultType, result.value_or(builtinTypes->anyType), c.subjectType, constraint->location);
    unblock(c.resultType, constraint->location);
    return true;
}

static bool isUnsealedTable(TypeId ty)
{
    ty = follow(ty);
    const TableType* ttv = get<TableType>(ty);
    return ttv && ttv->state == TableState::Unsealed;
}

/**
 * Given a path into a set of nested unsealed tables `ty`, insert a new property `replaceTy` as the leaf-most property.
 *
 * Fails and does nothing if every table along the way is not unsealed.
 *
 * Mutates the innermost table type in-place.
 */
static void updateTheTableType(
    NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, TypeId ty, const std::vector<std::string>& path, TypeId replaceTy)
{
    if (path.empty())
        return;

    // First walk the path and ensure that it's unsealed tables all the way
    // to the end.
    {
        TypeId t = ty;
        for (size_t i = 0; i < path.size() - 1; ++i)
        {
            if (!isUnsealedTable(t))
                return;

            const TableType* tbl = get<TableType>(t);
            auto it = tbl->props.find(path[i]);
            if (it == tbl->props.end())
                return;

            t = follow(it->second.type());
        }

        // The last path segment should not be a property of the table at all.
        // We are not changing property types.  We are only admitting this one
        // new property to be appended.
        if (!isUnsealedTable(t))
            return;
        const TableType* tbl = get<TableType>(t);
        if (0 != tbl->props.count(path.back()))
            return;
    }

    TypeId t = ty;
    ErrorVec dummy;

    for (size_t i = 0; i < path.size() - 1; ++i)
    {
        auto propTy = findTablePropertyRespectingMeta(builtinTypes, dummy, t, path[i], Location{});
        dummy.clear();

        if (!propTy)
            return;

        t = *propTy;
    }

    const std::string& lastSegment = path.back();

    t = follow(t);
    TableType* tt = getMutable<TableType>(t);
    if (auto mt = get<MetatableType>(t))
        tt = getMutable<TableType>(mt->table);

    if (!tt)
        return;

    tt->props[lastSegment].setType(replaceTy);
}

bool ConstraintSolver::tryDispatch(const SetPropConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    TypeId subjectType = follow(c.subjectType);

    if (isBlocked(subjectType))
        return block(subjectType, constraint);

    if (!force && get<FreeType>(subjectType))
        return block(subjectType, constraint);

    std::optional<TypeId> existingPropType = subjectType;
    for (const std::string& segment : c.path)
    {
        if (!existingPropType)
            break;

        auto [blocked, result] = lookupTableProp(*existingPropType, segment);
        if (!blocked.empty())
        {
            for (TypeId blocked : blocked)
                block(blocked, constraint);
            return false;
        }

        existingPropType = result;
    }

    auto bind = [&](TypeId a, TypeId b) {
        bindBlockedType(a, b, c.subjectType, constraint->location);
    };

    if (existingPropType)
    {
        if (!isBlocked(c.propType))
            unify(c.propType, *existingPropType, constraint->scope);
        bind(c.resultType, c.subjectType);
        unblock(c.resultType, constraint->location);
        return true;
    }

    if (auto mt = get<MetatableType>(subjectType))
        subjectType = follow(mt->table);

    if (get<FreeType>(subjectType))
    {
        TypeId ty = arena->freshType(constraint->scope);

        // Mint a chain of free tables per c.path
        for (auto it = rbegin(c.path); it != rend(c.path); ++it)
        {
            TableType t{TableState::Free, TypeLevel{}, constraint->scope};
            t.props[*it] = {ty};

            ty = arena->addType(std::move(t));
        }

        LUAU_ASSERT(ty);

        bind(subjectType, ty);
        if (follow(c.resultType) != follow(ty))
            bind(c.resultType, ty);
        unblock(subjectType, constraint->location);
        unblock(c.resultType, constraint->location);
        return true;
    }
    else if (auto ttv = getMutable<TableType>(subjectType))
    {
        if (ttv->state == TableState::Free)
        {
            LUAU_ASSERT(!subjectType->persistent);

            ttv->props[c.path[0]] = Property{c.propType};
            bind(c.resultType, c.subjectType);
            unblock(c.resultType, constraint->location);
            return true;
        }
        else if (ttv->state == TableState::Unsealed)
        {
            LUAU_ASSERT(!subjectType->persistent);

            updateTheTableType(builtinTypes, NotNull{arena}, subjectType, c.path, c.propType);
            bind(c.resultType, c.subjectType);
            unblock(subjectType, constraint->location);
            unblock(c.resultType, constraint->location);
            return true;
        }
        else
        {
            bind(c.resultType, subjectType);
            unblock(c.resultType, constraint->location);
            return true;
        }
    }
    else
    {
        // Other kinds of types don't change shape when properties are assigned
        // to them. (if they allow properties at all!)
        bind(c.resultType, subjectType);
        unblock(c.resultType, constraint->location);
        return true;
    }
}

bool ConstraintSolver::tryDispatch(const SetIndexerConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    TypeId subjectType = follow(c.subjectType);
    if (isBlocked(subjectType))
        return block(subjectType, constraint);

    if (auto ft = get<FreeType>(subjectType))
    {
        Scope* scope = ft->scope;
        TableType* tt = &asMutable(subjectType)->ty.emplace<TableType>(TableState::Free, TypeLevel{}, scope);
        tt->indexer = TableIndexer{c.indexType, c.propType};

        asMutable(c.resultType)->ty.emplace<BoundType>(subjectType);
        asMutable(c.propType)->ty.emplace<FreeType>(scope);
        unblock(c.propType, constraint->location);
        unblock(c.resultType, constraint->location);

        return true;
    }
    else if (auto tt = get<TableType>(subjectType))
    {
        if (tt->indexer)
        {
            // TODO This probably has to be invariant.
            unify(c.indexType, tt->indexer->indexType, constraint->scope);
            asMutable(c.propType)->ty.emplace<BoundType>(tt->indexer->indexResultType);
            asMutable(c.resultType)->ty.emplace<BoundType>(subjectType);
            unblock(c.propType, constraint->location);
            unblock(c.resultType, constraint->location);
            return true;
        }
        else if (tt->state == TableState::Free || tt->state == TableState::Unsealed)
        {
            TypeId promotedIndexTy = arena->freshType(tt->scope);
            unify(c.indexType, promotedIndexTy, constraint->scope);

            auto mtt = getMutable<TableType>(subjectType);
            mtt->indexer = TableIndexer{promotedIndexTy, c.propType};
            asMutable(c.propType)->ty.emplace<FreeType>(tt->scope);
            asMutable(c.resultType)->ty.emplace<BoundType>(subjectType);
            unblock(c.propType, constraint->location);
            unblock(c.resultType, constraint->location);
            return true;
        }
        // Do not augment sealed or generic tables that lack indexers
    }

    asMutable(c.propType)->ty.emplace<BoundType>(builtinTypes->errorRecoveryType());
    asMutable(c.resultType)->ty.emplace<BoundType>(builtinTypes->errorRecoveryType());
    unblock(c.propType, constraint->location);
    unblock(c.resultType, constraint->location);
    return true;
}

bool ConstraintSolver::tryDispatch(const SingletonOrTopTypeConstraint& c, NotNull<const Constraint> constraint)
{
    if (isBlocked(c.discriminantType))
        return false;

    TypeId followed = follow(c.discriminantType);

    // `nil` is a singleton type too! There's only one value of type `nil`.
    if (c.negated && (get<SingletonType>(followed) || isNil(followed)))
        *asMutable(c.resultType) = NegationType{c.discriminantType};
    else if (!c.negated && get<SingletonType>(followed))
        *asMutable(c.resultType) = BoundType{c.discriminantType};
    else
        *asMutable(c.resultType) = BoundType{builtinTypes->anyType};

    unblock(c.resultType, constraint->location);

    return true;
}

bool ConstraintSolver::tryDispatch(const UnpackConstraint& c, NotNull<const Constraint> constraint)
{
    TypePackId sourcePack = follow(c.sourcePack);
    TypePackId resultPack = follow(c.resultPack);

    if (isBlocked(sourcePack))
        return block(sourcePack, constraint);

    if (isBlocked(resultPack))
    {
        asMutable(resultPack)->ty.emplace<BoundTypePack>(sourcePack);
        unblock(resultPack, constraint->location);
        return true;
    }

    TypePack srcPack = extendTypePack(*arena, builtinTypes, sourcePack, size(resultPack));

    auto destIter = begin(resultPack);
    auto destEnd = end(resultPack);

    size_t i = 0;
    while (destIter != destEnd)
    {
        if (i >= srcPack.head.size())
            break;
        TypeId srcTy = follow(srcPack.head[i]);

        if (isBlocked(*destIter))
        {
            if (follow(srcTy) == *destIter)
            {
                // Cyclic type dependency. (????)
                asMutable(*destIter)->ty.emplace<FreeType>(constraint->scope);
            }
            else
                asMutable(*destIter)->ty.emplace<BoundType>(srcTy);
            unblock(*destIter, constraint->location);
        }
        else
            unify(*destIter, srcTy, constraint->scope);

        ++destIter;
        ++i;
    }

    // We know that resultPack does not have a tail, but we don't know if
    // sourcePack is long enough to fill every value.  Replace every remaining
    // result TypeId with the error recovery type.

    while (destIter != destEnd)
    {
        if (isBlocked(*destIter))
        {
            asMutable(*destIter)->ty.emplace<BoundType>(builtinTypes->errorRecoveryType());
            unblock(*destIter, constraint->location);
        }

        ++destIter;
    }

    return true;
}

namespace
{

/*
 * Search for types that prevent us from being ready to dispatch a particular
 * RefineConstraint.
 */
struct FindRefineConstraintBlockers : TypeOnceVisitor
{
    std::unordered_set<TypeId> found;
    bool visit(TypeId ty, const BlockedType&) override
    {
        found.insert(ty);
        return false;
    }

    bool visit(TypeId ty, const PendingExpansionType&) override
    {
        found.insert(ty);
        return false;
    }

    bool visit(TypeId ty, const ClassType&) override
    {
        return false;
    }
};

} // namespace

static bool isNegatedAny(TypeId ty)
{
    ty = follow(ty);
    const NegationType* nt = get<NegationType>(ty);
    if (!nt)
        return false;
    TypeId negatedTy = follow(nt->ty);
    return bool(get<AnyType>(negatedTy));
}

bool ConstraintSolver::tryDispatch(const RefineConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    if (isBlocked(c.discriminant))
        return block(c.discriminant, constraint);

    FindRefineConstraintBlockers fbt;
    fbt.traverse(c.discriminant);

    if (!fbt.found.empty())
    {
        bool foundOne = false;

        for (TypeId blocked : fbt.found)
        {
            if (blocked == c.type)
                continue;

            block(blocked, constraint);
            foundOne = true;
        }

        if (foundOne)
            return false;
    }

    /* HACK: Refinements sometimes produce a type T & ~any under the assumption
     * that ~any is the same as any.  This is so so weird, but refinements needs
     * some way to say "I may refine this, but I'm not sure."
     *
     * It does this by refining on a blocked type and deferring the decision
     * until it is unblocked.
     *
     * Refinements also get negated, so we wind up with types like T & ~*blocked*
     *
     * We need to treat T & ~any as T in this case.
     */

    if (c.mode == RefineConstraint::Intersection && isNegatedAny(c.discriminant))
    {
        asMutable(c.resultType)->ty.emplace<BoundType>(c.type);
        unblock(c.resultType, constraint->location);
        return true;
    }

    const TypeId type = follow(c.type);

    LUAU_ASSERT(get<BlockedType>(c.resultType));

    if (type == c.resultType)
    {
        /*
         * Sometimes, we get a constraint of the form
         *
         * *blocked-N* ~ refine *blocked-N* & U
         *
         * The constraint essentially states that a particular type is a
         * refinement of itself. This is weird and I think vacuous.
         *
         * I *believe* it is safe to replace the result with a fresh type that
         * is constrained by U.  We effect this by minting a fresh type for the
         * result when U = any, else we bind the result to whatever discriminant
         * was offered.
         */
        if (get<AnyType>(follow(c.discriminant)))
            asMutable(c.resultType)->ty.emplace<FreeType>(constraint->scope);
        else
            asMutable(c.resultType)->ty.emplace<BoundType>(c.discriminant);

        unblock(c.resultType, constraint->location);
        return true;
    }

    auto [result, blockedTypes] = c.mode == RefineConstraint::Intersection ? simplifyIntersection(builtinTypes, NotNull{arena}, type, c.discriminant)
                                                                           : simplifyUnion(builtinTypes, NotNull{arena}, type, c.discriminant);

    if (!force && !blockedTypes.empty())
        return block(blockedTypes, constraint);

    const NormalizedType* normType = normalizer->normalize(c.type);

    if (!normType)
        reportError(NormalizationTooComplex{}, constraint->location);

    if (normType && normType->shouldSuppressErrors())
    {
        auto resultOrError = simplifyUnion(builtinTypes, arena, result, builtinTypes->errorType).result;
        asMutable(c.resultType)->ty.emplace<BoundType>(resultOrError);
    }
    else
    {
        asMutable(c.resultType)->ty.emplace<BoundType>(result);
    }

    unblock(c.resultType, constraint->location);

    return true;
}

bool ConstraintSolver::tryDispatch(const ReduceConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    TypeId ty = follow(c.ty);
    FamilyGraphReductionResult result =
        reduceFamilies(ty, constraint->location, NotNull{arena}, builtinTypes, constraint->scope, normalizer, nullptr, force);

    for (TypeId r : result.reducedTypes)
        unblock(r, constraint->location);

    for (TypePackId r : result.reducedPacks)
        unblock(r, constraint->location);

    if (force)
        return true;

    for (TypeId b : result.blockedTypes)
        block(b, constraint);

    for (TypePackId b : result.blockedPacks)
        block(b, constraint);

    return result.blockedTypes.empty() && result.blockedPacks.empty();
}

bool ConstraintSolver::tryDispatch(const ReducePackConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    TypePackId tp = follow(c.tp);
    FamilyGraphReductionResult result =
        reduceFamilies(tp, constraint->location, NotNull{arena}, builtinTypes, constraint->scope, normalizer, nullptr, force);

    for (TypeId r : result.reducedTypes)
        unblock(r, constraint->location);

    for (TypePackId r : result.reducedPacks)
        unblock(r, constraint->location);

    if (force)
        return true;

    for (TypeId b : result.blockedTypes)
        block(b, constraint);

    for (TypePackId b : result.blockedPacks)
        block(b, constraint);

    return result.blockedTypes.empty() && result.blockedPacks.empty();
}

bool ConstraintSolver::tryDispatchIterableTable(TypeId iteratorTy, const IterableConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    auto block_ = [&](auto&& t) {
        if (force)
        {
            // TODO: I believe it is the case that, if we are asked to force
            // this constraint, then we can do nothing but fail.  I'd like to
            // find a code sample that gets here.
            LUAU_ASSERT(false);
        }
        else
            block(t, constraint);
        return false;
    };

    // We may have to block here if we don't know what the iteratee type is,
    // if it's a free table, if we don't know it has a metatable, and so on.
    iteratorTy = follow(iteratorTy);
    if (get<FreeType>(iteratorTy))
        return block_(iteratorTy);

    auto anyify = [&](auto ty) {
        Anyification anyify{arena, constraint->scope, builtinTypes, &iceReporter, builtinTypes->anyType, builtinTypes->anyTypePack};
        std::optional anyified = anyify.substitute(ty);
        if (!anyified)
            reportError(CodeTooComplex{}, constraint->location);
        else
            unify(*anyified, ty, constraint->scope);
    };

    auto errorify = [&](auto ty) {
        Anyification anyify{arena, constraint->scope, builtinTypes, &iceReporter, errorRecoveryType(), errorRecoveryTypePack()};
        std::optional errorified = anyify.substitute(ty);
        if (!errorified)
            reportError(CodeTooComplex{}, constraint->location);
        else
            unify(*errorified, ty, constraint->scope);
    };

    auto neverify = [&](auto ty) {
        Anyification anyify{arena, constraint->scope, builtinTypes, &iceReporter, builtinTypes->neverType, builtinTypes->neverTypePack};
        std::optional neverified = anyify.substitute(ty);
        if (!neverified)
            reportError(CodeTooComplex{}, constraint->location);
        else
            unify(*neverified, ty, constraint->scope);
    };

    if (get<AnyType>(iteratorTy))
    {
        anyify(c.variables);
        return true;
    }

    if (get<ErrorType>(iteratorTy))
    {
        errorify(c.variables);
        return true;
    }

    if (get<NeverType>(iteratorTy))
    {
        neverify(c.variables);
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
         * We should eventually introduce a type family to talk about iteration.
         */
        if (iteratorTable->state == TableState::Free && !force)
            return block(iteratorTy, constraint);

        if (iteratorTable->indexer)
        {
            TypePackId expectedVariablePack = arena->addTypePack({iteratorTable->indexer->indexType, iteratorTable->indexer->indexResultType});
            unify(c.variables, expectedVariablePack, constraint->scope);
        }
        else
            errorify(c.variables);
    }
    else if (std::optional<TypeId> iterFn = findMetatableEntry(builtinTypes, errors, iteratorTy, "__iter", Location{}))
    {
        if (isBlocked(*iterFn))
        {
            return block(*iterFn, constraint);
        }

        Instantiation instantiation(TxnLog::empty(), arena, TypeLevel{}, constraint->scope);

        if (std::optional<TypeId> instantiatedIterFn = instantiation.substitute(*iterFn))
        {
            if (auto iterFtv = get<FunctionType>(*instantiatedIterFn))
            {
                TypePackId expectedIterArgs = arena->addTypePack({iteratorTy});
                unify(iterFtv->argTypes, expectedIterArgs, constraint->scope);

                TypePack iterRets = extendTypePack(*arena, builtinTypes, iterFtv->retTypes, 2);

                if (iterRets.head.size() < 1)
                {
                    // We've done what we can; this will get reported as an
                    // error by the type checker.
                    return true;
                }

                TypeId nextFn = iterRets.head[0];
                TypeId table = iterRets.head.size() == 2 ? iterRets.head[1] : arena->freshType(constraint->scope);

                if (std::optional<TypeId> instantiatedNextFn = instantiation.substitute(nextFn))
                {
                    const TypeId firstIndex = arena->freshType(constraint->scope);

                    // nextTy : (iteratorTy, indexTy?) -> (indexTy, valueTailTy...)
                    const TypePackId nextArgPack = arena->addTypePack({table, arena->addType(UnionType{{firstIndex, builtinTypes->nilType}})});
                    const TypePackId valueTailTy = arena->addTypePack(FreeTypePack{constraint->scope});
                    const TypePackId nextRetPack = arena->addTypePack(TypePack{{firstIndex}, valueTailTy});

                    const TypeId expectedNextTy = arena->addType(FunctionType{nextArgPack, nextRetPack});
                    unify(*instantiatedNextFn, expectedNextTy, constraint->scope);

                    pushConstraint(constraint->scope, constraint->location, PackSubtypeConstraint{c.variables, nextRetPack});
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
        TypeId metaTy = follow(iteratorMetatable->metatable);
        if (get<FreeType>(metaTy))
            return block_(metaTy);

        LUAU_ASSERT(false);
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
    if (get<FreeType>(firstIndexTy))
    {
        if (force)
            LUAU_ASSERT(false);
        else
            block(firstIndexTy, constraint);
        return false;
    }

    TypeId firstIndex;
    TypeId retIndex;
    if (isNil(firstIndexTy) || isOptional(firstIndexTy))
    {
        firstIndex = arena->addType(UnionType{{arena->freshType(constraint->scope), builtinTypes->nilType}});
        retIndex = firstIndex;
    }
    else
    {
        firstIndex = firstIndexTy;
        retIndex = arena->addType(UnionType{{firstIndexTy, builtinTypes->nilType}});
    }

    // nextTy : (tableTy, indexTy?) -> (indexTy?, valueTailTy...)
    const TypePackId nextArgPack = arena->addTypePack({tableTy, firstIndex});
    const TypePackId valueTailTy = arena->addTypePack(FreeTypePack{constraint->scope});
    const TypePackId nextRetPack = arena->addTypePack(TypePack{{retIndex}, valueTailTy});

    const TypeId expectedNextTy = arena->addType(FunctionType{TypeLevel{}, constraint->scope, nextArgPack, nextRetPack});
    ErrorVec errors = unify(nextTy, expectedNextTy, constraint->scope);

    // if there are no errors from unifying the two, we can pass forward the expected type as our selected resolution.
    if (errors.empty())
    {
        (*c.astForInNextTypes)[c.nextAstFragment] = expectedNextTy;
    }

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
    auto psc = pushConstraint(constraint->scope, constraint->location, PackSubtypeConstraint{c.variables, modifiedNextRetPack});
    inheritBlocks(constraint, psc);

    return true;
}

std::pair<std::vector<TypeId>, std::optional<TypeId>> ConstraintSolver::lookupTableProp(
    TypeId subjectType, const std::string& propName, bool suppressSimplification)
{
    std::unordered_set<TypeId> seen;
    return lookupTableProp(subjectType, propName, suppressSimplification, seen);
}

std::pair<std::vector<TypeId>, std::optional<TypeId>> ConstraintSolver::lookupTableProp(
    TypeId subjectType, const std::string& propName, bool suppressSimplification, std::unordered_set<TypeId>& seen)
{
    if (!seen.insert(subjectType).second)
        return {};

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
            return {{}, FFlag::DebugLuauReadWriteProperties ? prop->second.readType() : prop->second.type()};
        else if (ttv->indexer && maybeString(ttv->indexer->indexType))
            return {{}, ttv->indexer->indexResultType};
        else if (ttv->state == TableState::Free)
        {
            TypeId result = arena->freshType(ttv->scope);
            ttv->props[propName] = Property{result};
            return {{}, result};
        }
    }
    else if (auto mt = get<MetatableType>(subjectType))
    {
        auto [blocked, result] = lookupTableProp(mt->table, propName, suppressSimplification, seen);
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
                return lookupTableProp(indexType, propName, suppressSimplification, seen);
        }
    }
    else if (auto ct = get<ClassType>(subjectType))
    {
        if (auto p = lookupClassProp(ct, propName))
            return {{}, p->type()};
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

        return lookupTableProp(indexProp->second.type(), propName, suppressSimplification, seen);
    }
    else if (auto ft = get<FreeType>(subjectType))
    {
        Scope* scope = ft->scope;

        TableType* tt = &asMutable(subjectType)->ty.emplace<TableType>();
        tt->state = TableState::Free;
        tt->scope = scope;
        TypeId propType = arena->freshType(scope);
        tt->props[propName] = Property{propType};

        return {{}, propType};
    }
    else if (auto utv = get<UnionType>(subjectType))
    {
        std::vector<TypeId> blocked;
        std::set<TypeId> options;

        for (TypeId ty : utv)
        {
            auto [innerBlocked, innerResult] = lookupTableProp(ty, propName, suppressSimplification, seen);
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
            return {{}, simplifyUnion(builtinTypes, arena, one, two).result};
        }
        else
            return {{}, arena->addType(UnionType{std::vector<TypeId>(begin(options), end(options))})};
    }
    else if (auto itv = get<IntersectionType>(subjectType))
    {
        std::vector<TypeId> blocked;
        std::set<TypeId> options;

        for (TypeId ty : itv)
        {
            auto [innerBlocked, innerResult] = lookupTableProp(ty, propName, suppressSimplification, seen);
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

    return {{}, std::nullopt};
}

static TypeId getErrorType(NotNull<BuiltinTypes> builtinTypes, TypeId)
{
    return builtinTypes->errorRecoveryType();
}

static TypePackId getErrorType(NotNull<BuiltinTypes> builtinTypes, TypePackId)
{
    return builtinTypes->errorRecoveryTypePack();
}

template<typename TID>
bool ConstraintSolver::tryUnify(NotNull<const Constraint> constraint, TID subTy, TID superTy)
{
    Unifier u{normalizer, constraint->scope, constraint->location, Covariant};
    u.enableScopeTests();

    u.tryUnify(subTy, superTy);

    if (!u.blockedTypes.empty() || !u.blockedTypePacks.empty())
    {
        for (TypeId bt : u.blockedTypes)
            block(bt, constraint);
        for (TypePackId btp : u.blockedTypePacks)
            block(btp, constraint);
        return false;
    }

    if (const auto& e = hasUnificationTooComplex(u.errors))
        reportError(*e);

    if (!u.errors.empty())
    {
        TID errorType = getErrorType(builtinTypes, TID{});
        u.tryUnify(subTy, errorType);
        u.tryUnify(superTy, errorType);
    }

    const auto [changedTypes, changedPacks] = u.log.getChanges();

    u.log.commit();

    unblock(changedTypes, constraint->location);
    unblock(changedPacks, constraint->location);

    return true;
}

void ConstraintSolver::bindBlockedType(TypeId blockedTy, TypeId resultTy, TypeId rootTy, Location location)
{
    resultTy = follow(resultTy);

    LUAU_ASSERT(get<BlockedType>(blockedTy));

    if (blockedTy == resultTy)
    {
        rootTy = follow(rootTy);
        Scope* freeScope = nullptr;
        if (auto ft = get<FreeType>(rootTy))
            freeScope = ft->scope;
        else if (auto tt = get<TableType>(rootTy); tt && tt->state == TableState::Free)
            freeScope = tt->scope;
        else
            iceReporter.ice("bindBlockedType couldn't find an appropriate scope for a fresh type!", location);

        LUAU_ASSERT(freeScope);

        asMutable(blockedTy)->ty.emplace<BoundType>(arena->freshType(freeScope));
    }
    else
        asMutable(blockedTy)->ty.emplace<BoundType>(resultTy);
}

void ConstraintSolver::block_(BlockedConstraintId target, NotNull<const Constraint> constraint)
{
    blocked[target].push_back(constraint);

    auto& count = blockedConstraints[constraint];
    count += 1;
}

void ConstraintSolver::block(NotNull<const Constraint> target, NotNull<const Constraint> constraint)
{
    if (logger)
        logger->pushBlock(constraint, target);

    if (FFlag::DebugLuauLogSolver)
        printf("block Constraint %s on\t%s\n", toString(*target, opts).c_str(), toString(*constraint, opts).c_str());

    block_(target.get(), constraint);
}

bool ConstraintSolver::block(TypeId target, NotNull<const Constraint> constraint)
{
    if (logger)
        logger->pushBlock(constraint, target);

    if (FFlag::DebugLuauLogSolver)
        printf("block TypeId %s on\t%s\n", toString(target, opts).c_str(), toString(*constraint, opts).c_str());

    block_(follow(target), constraint);
    return false;
}

bool ConstraintSolver::block(TypePackId target, NotNull<const Constraint> constraint)
{
    if (logger)
        logger->pushBlock(constraint, target);

    if (FFlag::DebugLuauLogSolver)
        printf("block TypeId %s on\t%s\n", toString(target, opts).c_str(), toString(*constraint, opts).c_str());

    block_(target, constraint);
    return false;
}

void ConstraintSolver::inheritBlocks(NotNull<const Constraint> source, NotNull<const Constraint> addition)
{
    // Anything that is blocked on this constraint must also be blocked on our
    // synthesized constraints.
    auto blockedIt = blocked.find(source.get());
    if (blockedIt != blocked.end())
    {
        for (const auto& blockedConstraint : blockedIt->second)
        {
            block(addition, blockedConstraint);
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

bool ConstraintSolver::isBlocked(TypeId ty)
{
    return nullptr != get<BlockedType>(follow(ty)) || nullptr != get<PendingExpansionType>(follow(ty));
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

ErrorVec ConstraintSolver::unify(TypeId subType, TypeId superType, NotNull<Scope> scope)
{
    Unifier u{normalizer, scope, Location{}, Covariant};
    u.enableScopeTests();

    u.tryUnify(subType, superType);

    if (!u.errors.empty())
    {
        TypeId errorType = errorRecoveryType();
        u.tryUnify(subType, errorType);
        u.tryUnify(superType, errorType);
    }

    const auto [changedTypes, changedPacks] = u.log.getChanges();

    u.log.commit();

    unblock(changedTypes, Location{});
    unblock(changedPacks, Location{});

    return std::move(u.errors);
}

ErrorVec ConstraintSolver::unify(TypePackId subPack, TypePackId superPack, NotNull<Scope> scope)
{
    UnifierSharedState sharedState{&iceReporter};
    Unifier u{normalizer, scope, Location{}, Covariant};
    u.enableScopeTests();

    u.tryUnify(subPack, superPack);

    const auto [changedTypes, changedPacks] = u.log.getChanges();

    u.log.commit();

    unblock(changedTypes, Location{});
    unblock(changedPacks, Location{});

    return std::move(u.errors);
}

NotNull<Constraint> ConstraintSolver::pushConstraint(NotNull<Scope> scope, const Location& location, ConstraintV cv)
{
    std::unique_ptr<Constraint> c = std::make_unique<Constraint>(scope, location, std::move(cv));
    NotNull<Constraint> borrow = NotNull(c.get());
    solverConstraints.push_back(std::move(c));
    unsolvedConstraints.push_back(borrow);

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

TypeId ConstraintSolver::errorRecoveryType() const
{
    return builtinTypes->errorRecoveryType();
}

TypePackId ConstraintSolver::errorRecoveryTypePack() const
{
    return builtinTypes->errorRecoveryTypePack();
}

TypeId ConstraintSolver::unionOfTypes(TypeId a, TypeId b, NotNull<Scope> scope, bool unifyFreeTypes)
{
    a = follow(a);
    b = follow(b);

    if (unifyFreeTypes && (get<FreeType>(a) || get<FreeType>(b)))
    {
        Unifier u{normalizer, scope, Location{}, Covariant};
        u.enableScopeTests();
        u.tryUnify(b, a);

        if (u.errors.empty())
        {
            u.log.commit();
            return a;
        }
        else
        {
            return builtinTypes->errorRecoveryType(builtinTypes->anyType);
        }
    }

    if (*a == *b)
        return a;

    std::vector<TypeId> types = reduceUnion({a, b});
    if (types.empty())
        return builtinTypes->neverType;

    if (types.size() == 1)
        return types[0];

    return arena->addType(UnionType{types});
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

} // namespace Luau
