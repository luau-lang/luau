// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Anyification.h"
#include "Luau/ApplyTypeFunction.h"
#include "Luau/Clone.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/DcrLogger.h"
#include "Luau/Instantiation.h"
#include "Luau/Location.h"
#include "Luau/Metamethods.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Quantify.h"
#include "Luau/ToString.h"
#include "Luau/TypeUtils.h"
#include "Luau/Type.h"
#include "Luau/Unifier.h"
#include "Luau/VisitType.h"

LUAU_FASTFLAGVARIABLE(DebugLuauLogSolver, false);
LUAU_FASTFLAGVARIABLE(DebugLuauLogSolverToJson, false);
LUAU_FASTFLAG(LuauScopelessModule);

namespace Luau
{

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

        for (NotNull<Constraint> dep : c->dependencies)
        {
            auto unsolvedIter = std::find(begin(cs->unsolvedConstraints), end(cs->unsolvedConstraints), dep);
            if (unsolvedIter == cs->unsolvedConstraints.end())
                continue;

            auto it = cs->blockedConstraints.find(dep);
            int blockCount = it == cs->blockedConstraints.end() ? 0 : int(it->second);
            printf("\t%d\t\t%s\n", blockCount, toString(*dep, opts).c_str());
        }

        if (auto fcc = get<FunctionCallConstraint>(*c))
        {
            for (NotNull<const Constraint> inner : fcc->innerConstraints)
                printf("\t ->\t\t%s\n", toString(*inner, opts).c_str());
        }
    }
}

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

    if (FFlag::DebugLuauLogSolverToJson)
        LUAU_ASSERT(logger);
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

    if (FFlag::DebugLuauLogSolverToJson)
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

            if (FFlag::DebugLuauLogSolverToJson)
            {
                snapshot = logger->prepareStepSnapshot(rootScope, c, force, unsolvedConstraints);
            }

            bool success = tryDispatch(c, force);

            progress |= success;

            if (success)
            {
                unblock(c);
                unsolvedConstraints.erase(unsolvedConstraints.begin() + i);

                if (FFlag::DebugLuauLogSolverToJson)
                {
                    logger->commitStepSnapshot(snapshot);
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

    finalizeModule();

    if (FFlag::DebugLuauLogSolver)
    {
        dumpBindings(rootScope, opts);
    }

    if (FFlag::DebugLuauLogSolverToJson)
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
        rootScope->returnType = *returnType;
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
    else if (auto sottc = get<SingletonOrTopTypeConstraint>(*constraint))
        success = tryDispatch(*sottc, constraint);
    else
        LUAU_ASSERT(false);

    if (success)
    {
        unblock(constraint);
    }

    return success;
}

bool ConstraintSolver::tryDispatch(const SubtypeConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    if (!recursiveBlock(c.subType, constraint))
        return false;
    if (!recursiveBlock(c.superType, constraint))
        return false;

    if (isBlocked(c.subType))
        return block(c.subType, constraint);
    else if (isBlocked(c.superType))
        return block(c.superType, constraint);

    unify(c.subType, c.superType, constraint->scope);

    return true;
}

bool ConstraintSolver::tryDispatch(const PackSubtypeConstraint& c, NotNull<const Constraint> constraint, bool force)
{
    if (!recursiveBlock(c.subPack, constraint) || !recursiveBlock(c.superPack, constraint))
        return false;

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
        asMutable(c.generalizedType)->ty.emplace<BoundType>(generalized);
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

    Instantiation inst(TxnLog::empty(), arena, TypeLevel{}, constraint->scope);

    std::optional<TypeId> instantiated = inst.substitute(c.superType);
    LUAU_ASSERT(instantiated); // TODO FIXME HANDLE THIS

    if (isBlocked(c.subType))
        asMutable(c.subType)->ty.emplace<BoundType>(*instantiated);
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

    if (get<FreeType>(operandType))
        return block(operandType, constraint);

    LUAU_ASSERT(get<BlockedType>(c.resultType));

    switch (c.op)
    {
    case AstExprUnary::Not:
    {
        asMutable(c.resultType)->ty.emplace<BoundType>(builtinTypes->booleanType);
        return true;
    }
    case AstExprUnary::Len:
    {
        // __len must return a number.
        asMutable(c.resultType)->ty.emplace<BoundType>(builtinTypes->numberType);
        return true;
    }
    case AstExprUnary::Minus:
    {
        if (isNumber(operandType) || get<AnyType>(operandType) || get<ErrorType>(operandType))
        {
            asMutable(c.resultType)->ty.emplace<BoundType>(c.operandType);
        }
        else if (std::optional<TypeId> mm = findMetatableEntry(builtinTypes, errors, operandType, "__unm", constraint->location))
        {
            const FunctionType* ftv = get<FunctionType>(follow(*mm));

            if (!ftv)
            {
                if (std::optional<TypeId> callMm = findMetatableEntry(builtinTypes, errors, follow(*mm), "__call", constraint->location))
                {
                    ftv = get<FunctionType>(follow(*callMm));
                }
            }

            if (!ftv)
            {
                asMutable(c.resultType)->ty.emplace<BoundType>(builtinTypes->errorRecoveryType());
                return true;
            }

            TypePackId argsPack = arena->addTypePack({operandType});
            unify(ftv->argTypes, argsPack, constraint->scope);

            TypeId result = builtinTypes->errorRecoveryType();
            if (ftv)
            {
                result = first(ftv->retTypes).value_or(builtinTypes->errorRecoveryType());
            }

            asMutable(c.resultType)->ty.emplace<BoundType>(result);
        }
        else
        {
            asMutable(c.resultType)->ty.emplace<BoundType>(builtinTypes->errorRecoveryType());
        }

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

    bool isLogical = c.op == AstExprBinary::Op::And || c.op == AstExprBinary::Op::Or;

    /* Compound assignments create constraints of the form
     *
     *     A <: Binary<op, A, B>
     *
     * This constraint is the one that is meant to unblock A, so it doesn't
     * make any sense to stop and wait for someone else to do it.
     */

    if (isBlocked(leftType) && leftType != resultType)
        return block(c.leftType, constraint);

    if (isBlocked(rightType) && rightType != resultType)
        return block(c.rightType, constraint);

    if (!force)
    {
        // Logical expressions may proceed if the LHS is free.
        if (get<FreeType>(leftType) && !isLogical)
            return block(leftType, constraint);
    }

    // Logical expressions may proceed if the LHS is free.
    if (isBlocked(leftType) || (get<FreeType>(leftType) && !isLogical))
    {
        asMutable(resultType)->ty.emplace<BoundType>(errorRecoveryType());
        unblock(resultType);
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
            unblock(resultType);
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
                    mmResult = first(ftv->retTypes).value_or(errorRecoveryType());
                }

                asMutable(resultType)->ty.emplace<BoundType>(mmResult);
                unblock(resultType);

                (*c.astOriginalCallTypes)[c.astFragment] = *mm;
                (*c.astOverloadResolvedTypes)[c.astFragment] = *instantiatedMm;
                return true;
            }
        }

        // If there's no metamethod available, fall back to primitive behavior.
    }

    // If any is present, the expression must evaluate to any as well.
    bool leftAny = get<AnyType>(leftType) || get<ErrorType>(leftType);
    bool rightAny = get<AnyType>(rightType) || get<ErrorType>(rightType);
    bool anyPresent = leftAny || rightAny;

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
        if (isNumber(leftType))
        {
            unify(leftType, rightType, constraint->scope);
            asMutable(resultType)->ty.emplace<BoundType>(anyPresent ? builtinTypes->anyType : leftType);
            unblock(resultType);
            return true;
        }

        break;
    // For concatenation, if the LHS is a string, the RHS must be a string as
    // well. The result will also be a string.
    case AstExprBinary::Op::Concat:
        if (isString(leftType))
        {
            unify(leftType, rightType, constraint->scope);
            asMutable(resultType)->ty.emplace<BoundType>(anyPresent ? builtinTypes->anyType : leftType);
            unblock(resultType);
            return true;
        }

        break;
    // Inexact comparisons require that the types be both numbers or both
    // strings, and evaluate to a boolean.
    case AstExprBinary::Op::CompareGe:
    case AstExprBinary::Op::CompareGt:
    case AstExprBinary::Op::CompareLe:
    case AstExprBinary::Op::CompareLt:
        if ((isNumber(leftType) && isNumber(rightType)) || (isString(leftType) && isString(rightType)))
        {
            asMutable(resultType)->ty.emplace<BoundType>(builtinTypes->booleanType);
            unblock(resultType);
            return true;
        }

        break;
    // == and ~= always evaluate to a boolean, and impose no other constraints
    // on their parameters.
    case AstExprBinary::Op::CompareEq:
    case AstExprBinary::Op::CompareNe:
        asMutable(resultType)->ty.emplace<BoundType>(builtinTypes->booleanType);
        unblock(resultType);
        return true;
    // And evalutes to a boolean if the LHS is falsey, and the RHS type if LHS is
    // truthy.
    case AstExprBinary::Op::And:
    {
        TypeId leftFilteredTy = arena->addType(IntersectionType{{builtinTypes->falsyType, leftType}});

        asMutable(resultType)->ty.emplace<BoundType>(arena->addType(UnionType{{leftFilteredTy, rightType}}));
        unblock(resultType);
        return true;
    }
    // Or evaluates to the LHS type if the LHS is truthy, and the RHS type if
    // LHS is falsey.
    case AstExprBinary::Op::Or:
    {
        TypeId leftFilteredTy = arena->addType(IntersectionType{{builtinTypes->truthyType, leftType}});

        asMutable(resultType)->ty.emplace<BoundType>(arena->addType(UnionType{{leftFilteredTy, rightType}}));
        unblock(resultType);
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
    unblock(resultType);

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
        ttv->name = c.name;
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
};

bool ConstraintSolver::tryDispatch(const TypeAliasExpansionConstraint& c, NotNull<const Constraint> constraint)
{
    const PendingExpansionType* petv = get<PendingExpansionType>(follow(c.target));
    if (!petv)
    {
        unblock(c.target);
        return true;
    }

    auto bindResult = [this, &c](TypeId result) {
        asMutable(c.target)->ty.emplace<BoundType>(result);
        unblock(c.target);
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

    if (target->persistent)
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
    TypePackId result = follow(c.result);

    if (isBlocked(c.fn))
    {
        return block(c.fn, constraint);
    }

    // We don't support magic __call metamethods.
    if (std::optional<TypeId> callMm = findMetatableEntry(builtinTypes, errors, fn, "__call", constraint->location))
    {
        std::vector<TypeId> args{fn};

        for (TypeId arg : c.argsPack)
            args.push_back(arg);

        TypeId instantiatedType = arena->addType(BlockedType{});
        TypeId inferredFnType = arena->addType(FunctionType(TypeLevel{}, constraint->scope.get(), arena->addTypePack(TypePack{args, {}}), c.result));

        // Alter the inner constraints.
        LUAU_ASSERT(c.innerConstraints.size() == 2);

        // Anything that is blocked on this constraint must also be blocked on our inner constraints
        auto blockedIt = blocked.find(constraint.get());
        if (blockedIt != blocked.end())
        {
            for (const auto& ic : c.innerConstraints)
            {
                for (const auto& blockedConstraint : blockedIt->second)
                    block(ic, blockedConstraint);
            }
        }

        asMutable(*c.innerConstraints.at(0)).c = InstantiationConstraint{instantiatedType, *callMm};
        asMutable(*c.innerConstraints.at(1)).c = SubtypeConstraint{inferredFnType, instantiatedType};

        unsolvedConstraints.insert(end(unsolvedConstraints), begin(c.innerConstraints), end(c.innerConstraints));

        asMutable(c.result)->ty.emplace<FreeTypePack>(constraint->scope);
        unblock(c.result);
        return true;
    }

    const FunctionType* ftv = get<FunctionType>(fn);
    bool usedMagic = false;

    if (ftv && ftv->dcrMagicFunction != nullptr)
    {
        usedMagic = ftv->dcrMagicFunction(MagicFunctionCallContext{NotNull(this), c.callSite, c.argsPack, result});
    }

    if (usedMagic)
    {
        // There are constraints that are blocked on these constraints.  If we
        // are never going to even examine them, then we should not block
        // anything else on them.
        //
        // TODO CLI-58842
#if 0
        for (auto& c: c.innerConstraints)
            unblock(c);
#endif
    }
    else
    {
        // Anything that is blocked on this constraint must also be blocked on our inner constraints
        auto blockedIt = blocked.find(constraint.get());
        if (blockedIt != blocked.end())
        {
            for (const auto& ic : c.innerConstraints)
            {
                for (const auto& blockedConstraint : blockedIt->second)
                    block(ic, blockedConstraint);
            }
        }

        unsolvedConstraints.insert(end(unsolvedConstraints), begin(c.innerConstraints), end(c.innerConstraints));
        asMutable(c.result)->ty.emplace<FreeTypePack>(constraint->scope);
    }

    unblock(c.result);

    return true;
}

bool ConstraintSolver::tryDispatch(const PrimitiveTypeConstraint& c, NotNull<const Constraint> constraint)
{
    TypeId expectedType = follow(c.expectedType);
    if (isBlocked(expectedType) || get<PendingExpansionType>(expectedType))
        return block(expectedType, constraint);

    TypeId bindTo = maybeSingleton(expectedType) ? c.singletonType : c.multitonType;
    asMutable(c.resultType)->ty.emplace<BoundType>(bindTo);

    return true;
}

bool ConstraintSolver::tryDispatch(const HasPropConstraint& c, NotNull<const Constraint> constraint)
{
    TypeId subjectType = follow(c.subjectType);

    if (isBlocked(subjectType) || get<PendingExpansionType>(subjectType))
        return block(subjectType, constraint);

    if (get<FreeType>(subjectType))
    {
        TableType& ttv = asMutable(subjectType)->ty.emplace<TableType>(TableState::Free, TypeLevel{}, constraint->scope);
        ttv.props[c.prop] = Property{c.resultType};
        asMutable(c.resultType)->ty.emplace<FreeType>(constraint->scope);
        unblock(c.resultType);
        return true;
    }

    std::optional<TypeId> resultType = lookupTableProp(subjectType, c.prop);
    if (!resultType)
    {
        asMutable(c.resultType)->ty.emplace<BoundType>(builtinTypes->errorRecoveryType());
        unblock(c.resultType);
        return true;
    }

    if (isBlocked(*resultType))
    {
        block(*resultType, constraint);
        return false;
    }

    asMutable(c.resultType)->ty.emplace<BoundType>(*resultType);
    return true;
}

static bool isUnsealedTable(TypeId ty)
{
    ty = follow(ty);
    const TableType* ttv = get<TableType>(ty);
    return ttv && ttv->state == TableState::Unsealed;
}

/**
 * Create a shallow copy of `ty` and its properties along `path`.  Insert a new
 * property (the last segment of `path`) into the tail table with the value `t`.
 *
 * On success, returns the new outermost table type.  If the root table or any
 * of its subkeys are not unsealed tables, the function fails and returns
 * std::nullopt.
 *
 * TODO: Prove that we completely give up in the face of indexers and
 * metatables.
 */
static std::optional<TypeId> updateTheTableType(NotNull<TypeArena> arena, TypeId ty, const std::vector<std::string>& path, TypeId replaceTy)
{
    if (path.empty())
        return std::nullopt;

    // First walk the path and ensure that it's unsealed tables all the way
    // to the end.
    {
        TypeId t = ty;
        for (size_t i = 0; i < path.size() - 1; ++i)
        {
            if (!isUnsealedTable(t))
                return std::nullopt;

            const TableType* tbl = get<TableType>(t);
            auto it = tbl->props.find(path[i]);
            if (it == tbl->props.end())
                return std::nullopt;

            t = it->second.type;
        }

        // The last path segment should not be a property of the table at all.
        // We are not changing property types.  We are only admitting this one
        // new property to be appended.
        if (!isUnsealedTable(t))
            return std::nullopt;
        const TableType* tbl = get<TableType>(t);
        if (0 != tbl->props.count(path.back()))
            return std::nullopt;
    }

    const TypeId res = shallowClone(ty, arena);
    TypeId t = res;

    for (size_t i = 0; i < path.size() - 1; ++i)
    {
        const std::string segment = path[i];

        TableType* ttv = getMutable<TableType>(t);
        LUAU_ASSERT(ttv);

        auto propIt = ttv->props.find(segment);
        if (propIt != ttv->props.end())
        {
            LUAU_ASSERT(isUnsealedTable(propIt->second.type));
            t = shallowClone(follow(propIt->second.type), arena);
            ttv->props[segment].type = t;
        }
        else
            return std::nullopt;
    }

    TableType* ttv = getMutable<TableType>(t);
    LUAU_ASSERT(ttv);

    const std::string lastSegment = path.back();
    LUAU_ASSERT(0 == ttv->props.count(lastSegment));
    ttv->props[lastSegment] = Property{replaceTy};
    return res;
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
        ErrorVec e;
        std::optional<TypeId> propTy = lookupTableProp(*existingPropType, segment);
        if (!propTy)
        {
            existingPropType = std::nullopt;
            break;
        }
        else if (isBlocked(*propTy))
            return block(*propTy, constraint);
        else
            existingPropType = follow(*propTy);
    }

    auto bind = [](TypeId a, TypeId b) {
        asMutable(a)->ty.emplace<BoundType>(b);
    };

    if (existingPropType)
    {
        unify(c.propType, *existingPropType, constraint->scope);
        bind(c.resultType, c.subjectType);
        return true;
    }

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
        bind(c.resultType, ty);
        return true;
    }
    else if (auto ttv = getMutable<TableType>(subjectType))
    {
        if (ttv->state == TableState::Free)
        {
            ttv->props[c.path[0]] = Property{c.propType};
            bind(c.resultType, c.subjectType);
            return true;
        }
        else if (ttv->state == TableState::Unsealed)
        {
            std::optional<TypeId> augmented = updateTheTableType(NotNull{arena}, subjectType, c.path, c.propType);
            bind(c.resultType, augmented.value_or(subjectType));
            return true;
        }
        else
        {
            bind(c.resultType, subjectType);
            return true;
        }
    }
    else if (get<ClassType>(subjectType))
    {
        // Classes never change shape as a result of property assignments.
        // The result is always the subject.
        bind(c.resultType, subjectType);
        return true;
    }
    else if (get<AnyType>(subjectType) || get<ErrorType>(subjectType))
    {
        bind(c.resultType, subjectType);
        return true;
    }

    LUAU_ASSERT(0);
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
        *asMutable(c.resultType) = BoundType{builtinTypes->unknownType};

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

    // Irksome: I don't think we have any way to guarantee that this table
    // type never has a metatable.

    if (auto iteratorTable = get<TableType>(iteratorTy))
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

    const TypeId firstIndex = isNil(firstIndexTy) ? arena->freshType(constraint->scope) // FIXME: Surely this should be a union (free | nil)
                                                  : firstIndexTy;

    // nextTy : (tableTy, indexTy?) -> (indexTy, valueTailTy...)
    const TypePackId nextArgPack = arena->addTypePack({tableTy, arena->addType(UnionType{{firstIndex, builtinTypes->nilType}})});
    const TypePackId valueTailTy = arena->addTypePack(FreeTypePack{constraint->scope});
    const TypePackId nextRetPack = arena->addTypePack(TypePack{{firstIndex}, valueTailTy});

    const TypeId expectedNextTy = arena->addType(FunctionType{TypeLevel{}, constraint->scope, nextArgPack, nextRetPack});
    unify(nextTy, expectedNextTy, constraint->scope);

    pushConstraint(constraint->scope, constraint->location, PackSubtypeConstraint{c.variables, nextRetPack});

    return true;
}

std::optional<TypeId> ConstraintSolver::lookupTableProp(TypeId subjectType, const std::string& propName)
{
    auto collectParts = [&](auto&& unionOrIntersection) -> std::pair<std::optional<TypeId>, std::vector<TypeId>> {
        std::optional<TypeId> blocked;

        std::vector<TypeId> parts;
        for (TypeId expectedPart : unionOrIntersection)
        {
            expectedPart = follow(expectedPart);
            if (isBlocked(expectedPart) || get<PendingExpansionType>(expectedPart))
                blocked = expectedPart;
            else if (const TableType* ttv = get<TableType>(follow(expectedPart)))
            {
                if (auto prop = ttv->props.find(propName); prop != ttv->props.end())
                    parts.push_back(prop->second.type);
                else if (ttv->indexer && maybeString(ttv->indexer->indexType))
                    parts.push_back(ttv->indexer->indexResultType);
            }
        }

        return {blocked, parts};
    };

    std::optional<TypeId> resultType;

    if (auto ttv = get<TableType>(subjectType))
    {
        if (auto prop = ttv->props.find(propName); prop != ttv->props.end())
            resultType = prop->second.type;
        else if (ttv->indexer && maybeString(ttv->indexer->indexType))
            resultType = ttv->indexer->indexResultType;
    }
    else if (auto utv = get<UnionType>(subjectType))
    {
        auto [blocked, parts] = collectParts(utv);

        if (blocked)
            resultType = *blocked;
        else if (parts.size() == 1)
            resultType = parts[0];
        else if (parts.size() > 1)
            resultType = arena->addType(UnionType{std::move(parts)});
        else
            LUAU_ASSERT(false); // parts.size() == 0
    }
    else if (auto itv = get<IntersectionType>(subjectType))
    {
        auto [blocked, parts] = collectParts(itv);

        if (blocked)
            resultType = *blocked;
        else if (parts.size() == 1)
            resultType = parts[0];
        else if (parts.size() > 1)
            resultType = arena->addType(IntersectionType{std::move(parts)});
        else
            LUAU_ASSERT(false); // parts.size() == 0
    }

    return resultType;
}

void ConstraintSolver::block_(BlockedConstraintId target, NotNull<const Constraint> constraint)
{
    blocked[target].push_back(constraint);

    auto& count = blockedConstraints[constraint];
    count += 1;
}

void ConstraintSolver::block(NotNull<const Constraint> target, NotNull<const Constraint> constraint)
{
    if (FFlag::DebugLuauLogSolverToJson)
        logger->pushBlock(constraint, target);

    if (FFlag::DebugLuauLogSolver)
        printf("block Constraint %s on\t%s\n", toString(*target, opts).c_str(), toString(*constraint, opts).c_str());

    block_(target, constraint);
}

bool ConstraintSolver::block(TypeId target, NotNull<const Constraint> constraint)
{
    if (FFlag::DebugLuauLogSolverToJson)
        logger->pushBlock(constraint, target);

    if (FFlag::DebugLuauLogSolver)
        printf("block TypeId %s on\t%s\n", toString(target, opts).c_str(), toString(*constraint, opts).c_str());

    block_(target, constraint);
    return false;
}

bool ConstraintSolver::block(TypePackId target, NotNull<const Constraint> constraint)
{
    if (FFlag::DebugLuauLogSolverToJson)
        logger->pushBlock(constraint, target);

    if (FFlag::DebugLuauLogSolver)
        printf("block TypeId %s on\t%s\n", toString(target, opts).c_str(), toString(*constraint, opts).c_str());

    block_(target, constraint);
    return false;
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

    bool visit(TypeId ty, const BlockedType&)
    {
        blocked = true;
        solver->block(ty, constraint);
        return false;
    }

    bool visit(TypeId ty, const PendingExpansionType&)
    {
        blocked = true;
        solver->block(ty, constraint);
        return false;
    }
};

bool ConstraintSolver::recursiveBlock(TypeId target, NotNull<const Constraint> constraint)
{
    Blocker blocker{NotNull{this}, constraint};
    blocker.traverse(target);
    return !blocker.blocked;
}

bool ConstraintSolver::recursiveBlock(TypePackId pack, NotNull<const Constraint> constraint)
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
    if (FFlag::DebugLuauLogSolverToJson)
        logger->popBlock(progressed);

    return unblock_(progressed);
}

void ConstraintSolver::unblock(TypeId progressed)
{
    if (FFlag::DebugLuauLogSolverToJson)
        logger->popBlock(progressed);

    return unblock_(progressed);
}

void ConstraintSolver::unblock(TypePackId progressed)
{
    if (FFlag::DebugLuauLogSolverToJson)
        logger->popBlock(progressed);

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

void ConstraintSolver::unify(TypeId subType, TypeId superType, NotNull<Scope> scope)
{
    Unifier u{normalizer, Mode::Strict, scope, Location{}, Covariant};
    u.useScopes = true;

    u.tryUnify(subType, superType);

    if (!u.errors.empty())
    {
        TypeId errorType = errorRecoveryType();
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
    Unifier u{normalizer, Mode::Strict, scope, Location{}, Covariant};
    u.useScopes = true;

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
        return errorRecoveryType();
    }

    std::string humanReadableName = moduleResolver->getHumanReadableModuleName(info.name);

    for (const auto& [location, path] : requireCycles)
    {
        if (!path.empty() && path.front() == humanReadableName)
            return builtinTypes->anyType;
    }

    ModulePtr module = moduleResolver->getModule(info.name);
    if (!module)
    {
        if (!moduleResolver->moduleExists(info.name) && !info.optional)
            reportError(UnknownRequire{humanReadableName}, location);

        return errorRecoveryType();
    }

    if (module->type != SourceCode::Type::Module)
    {
        reportError(IllegalRequire{humanReadableName, "Module is not a ModuleScript. It cannot be required."}, location);
        return errorRecoveryType();
    }

    TypePackId modulePack = FFlag::LuauScopelessModule ? module->returnType : module->getModuleScope()->returnType;
    if (get<Unifiable::Error>(modulePack))
        return errorRecoveryType();

    std::optional<TypeId> moduleType = first(modulePack);
    if (!moduleType)
    {
        reportError(IllegalRequire{humanReadableName, "Module does not return exactly 1 value. It cannot be required."}, location);
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
        Unifier u{normalizer, Mode::Strict, scope, Location{}, Covariant};
        u.useScopes = true;
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

} // namespace Luau
