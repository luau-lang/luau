// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeFunction.h"

#include "Luau/BytecodeBuilder.h"
#include "Luau/Common.h"
#include "Luau/Compiler.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/DenseHash.h"
#include "Luau/Instantiation.h"
#include "Luau/Normalize.h"
#include "Luau/NotNull.h"
#include "Luau/OverloadResolution.h"
#include "Luau/Set.h"
#include "Luau/Simplify.h"
#include "Luau/Subtyping.h"
#include "Luau/TimeTrace.h"
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/TypeFunctionReductionGuesser.h"
#include "Luau/TypeFunctionRuntime.h"
#include "Luau/TypeFunctionRuntimeBuilder.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"
#include "Luau/VecDeque.h"
#include "Luau/VisitType.h"

#include "lua.h"
#include "lualib.h"

#include <iterator>
#include <memory>
#include <unordered_map>

// used to control emitting CodeTooComplex warnings on type function reduction
LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypeFamilyGraphReductionMaximumSteps, 1'000'000);

// used to control the limits of type function application over union type arguments
// e.g. `mul<a | b, c | d>` blows up into `mul<a, c> | mul<a, d> | mul<b, c> | mul<b, d>`
LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypeFamilyApplicationCartesianProductLimit, 5'000);

// used to control falling back to a more conservative reduction based on guessing
// when this value is set to a negative value, guessing will be totally disabled.
LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypeFamilyUseGuesserDepth, -1);

LUAU_FASTFLAGVARIABLE(DebugLuauLogTypeFamilies)
LUAU_FASTFLAG(LuauUserTypeFunFixRegister)
LUAU_FASTFLAG(LuauRemoveNotAnyHack)
LUAU_FASTFLAGVARIABLE(LuauUserDefinedTypeFunctionResetState)
LUAU_FASTFLAG(LuauUserTypeFunExportedAndLocal)

namespace Luau
{

using TypeOrTypePackIdSet = DenseHashSet<const void*>;

struct InstanceCollector : TypeOnceVisitor
{
    VecDeque<TypeId> tys;
    VecDeque<TypePackId> tps;
    TypeOrTypePackIdSet shouldGuess{nullptr};
    std::vector<TypeId> cyclicInstance;

    bool visit(TypeId ty, const TypeFunctionInstanceType&) override
    {
        // TypeOnceVisitor performs a depth-first traversal in the absence of
        // cycles. This means that by pushing to the front of the queue, we will
        // try to reduce deeper instances first if we start with the first thing
        // in the queue. Consider Add<Add<Add<number, number>, number>, number>:
        // we want to reduce the innermost Add<number, number> instantiation
        // first.

        if (DFInt::LuauTypeFamilyUseGuesserDepth >= 0 && typeFunctionDepth > DFInt::LuauTypeFamilyUseGuesserDepth)
            shouldGuess.insert(ty);

        tys.push_front(ty);

        return true;
    }

    void cycle(TypeId ty) override
    {
        /// Detected cyclic type pack
        TypeId t = follow(ty);
        if (get<TypeFunctionInstanceType>(t))
            cyclicInstance.push_back(t);
    }

    bool visit(TypeId ty, const ClassType&) override
    {
        return false;
    }

    bool visit(TypePackId tp, const TypeFunctionInstanceTypePack&) override
    {
        // TypeOnceVisitor performs a depth-first traversal in the absence of
        // cycles. This means that by pushing to the front of the queue, we will
        // try to reduce deeper instances first if we start with the first thing
        // in the queue. Consider Add<Add<Add<number, number>, number>, number>:
        // we want to reduce the innermost Add<number, number> instantiation
        // first.

        if (DFInt::LuauTypeFamilyUseGuesserDepth >= 0 && typeFunctionDepth > DFInt::LuauTypeFamilyUseGuesserDepth)
            shouldGuess.insert(tp);

        tps.push_front(tp);

        return true;
    }
};

struct TypeFunctionReducer
{
    TypeFunctionContext ctx;

    VecDeque<TypeId> queuedTys;
    VecDeque<TypePackId> queuedTps;
    TypeOrTypePackIdSet shouldGuess;
    std::vector<TypeId> cyclicTypeFunctions;
    TypeOrTypePackIdSet irreducible{nullptr};
    FunctionGraphReductionResult result;
    bool force = false;

    // Local to the constraint being reduced.
    Location location;

    TypeFunctionReducer(
        VecDeque<TypeId> queuedTys,
        VecDeque<TypePackId> queuedTps,
        TypeOrTypePackIdSet shouldGuess,
        std::vector<TypeId> cyclicTypes,
        Location location,
        TypeFunctionContext ctx,
        bool force = false
    )
        : ctx(ctx)
        , queuedTys(std::move(queuedTys))
        , queuedTps(std::move(queuedTps))
        , shouldGuess(std::move(shouldGuess))
        , cyclicTypeFunctions(std::move(cyclicTypes))
        , force(force)
        , location(location)
    {
    }

    enum class SkipTestResult
    {
        CyclicTypeFunction,
        Irreducible,
        Defer,
        Okay,
    };

    SkipTestResult testForSkippability(TypeId ty)
    {
        ty = follow(ty);

        if (is<TypeFunctionInstanceType>(ty))
        {
            for (auto t : cyclicTypeFunctions)
            {
                if (ty == t)
                    return SkipTestResult::CyclicTypeFunction;
            }

            if (!irreducible.contains(ty))
                return SkipTestResult::Defer;

            return SkipTestResult::Irreducible;
        }
        else if (is<GenericType>(ty))
        {
            return SkipTestResult::Irreducible;
        }

        return SkipTestResult::Okay;
    }

    SkipTestResult testForSkippability(TypePackId ty) const
    {
        ty = follow(ty);

        if (is<TypeFunctionInstanceTypePack>(ty))
        {
            if (!irreducible.contains(ty))
                return SkipTestResult::Defer;
            else
                return SkipTestResult::Irreducible;
        }
        else if (is<GenericTypePack>(ty))
        {
            return SkipTestResult::Irreducible;
        }

        return SkipTestResult::Okay;
    }

    template<typename T>
    void replace(T subject, T replacement)
    {
        if (subject->owningArena != ctx.arena.get())
        {
            result.errors.emplace_back(location, InternalError{"Attempting to modify a type function instance from another arena"});
            return;
        }

        if (FFlag::DebugLuauLogTypeFamilies)
            printf("%s -> %s\n", toString(subject, {true}).c_str(), toString(replacement, {true}).c_str());

        asMutable(subject)->ty.template emplace<Unifiable::Bound<T>>(replacement);

        if constexpr (std::is_same_v<T, TypeId>)
            result.reducedTypes.insert(subject);
        else if constexpr (std::is_same_v<T, TypePackId>)
            result.reducedPacks.insert(subject);
    }

    template<typename T>
    void handleTypeFunctionReduction(T subject, TypeFunctionReductionResult<T> reduction)
    {
        if (reduction.result)
            replace(subject, *reduction.result);
        else
        {
            irreducible.insert(subject);

            if (reduction.error.has_value())
                result.errors.emplace_back(location, UserDefinedTypeFunctionError{*reduction.error});

            if (reduction.uninhabited || force)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("%s is uninhabited\n", toString(subject, {true}).c_str());

                if constexpr (std::is_same_v<T, TypeId>)
                    result.errors.emplace_back(location, UninhabitedTypeFunction{subject});
                else if constexpr (std::is_same_v<T, TypePackId>)
                    result.errors.emplace_back(location, UninhabitedTypePackFunction{subject});
            }
            else if (!reduction.uninhabited && !force)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf(
                        "%s is irreducible; blocked on %zu types, %zu packs\n",
                        toString(subject, {true}).c_str(),
                        reduction.blockedTypes.size(),
                        reduction.blockedPacks.size()
                    );

                for (TypeId b : reduction.blockedTypes)
                    result.blockedTypes.insert(b);

                for (TypePackId b : reduction.blockedPacks)
                    result.blockedPacks.insert(b);
            }
        }
    }

    bool done() const
    {
        return queuedTys.empty() && queuedTps.empty();
    }

    template<typename T, typename I>
    bool testParameters(T subject, const I* tfit)
    {
        for (TypeId p : tfit->typeArguments)
        {
            SkipTestResult skip = testForSkippability(p);

            if (skip == SkipTestResult::Irreducible)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("%s is irreducible due to a dependency on %s\n", toString(subject, {true}).c_str(), toString(p, {true}).c_str());

                irreducible.insert(subject);
                return false;
            }
            else if (skip == SkipTestResult::Defer)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("Deferring %s until %s is solved\n", toString(subject, {true}).c_str(), toString(p, {true}).c_str());

                if constexpr (std::is_same_v<T, TypeId>)
                    queuedTys.push_back(subject);
                else if constexpr (std::is_same_v<T, TypePackId>)
                    queuedTps.push_back(subject);

                return false;
            }
        }

        for (TypePackId p : tfit->packArguments)
        {
            SkipTestResult skip = testForSkippability(p);

            if (skip == SkipTestResult::Irreducible)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("%s is irreducible due to a dependency on %s\n", toString(subject, {true}).c_str(), toString(p, {true}).c_str());

                irreducible.insert(subject);
                return false;
            }
            else if (skip == SkipTestResult::Defer)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("Deferring %s until %s is solved\n", toString(subject, {true}).c_str(), toString(p, {true}).c_str());

                if constexpr (std::is_same_v<T, TypeId>)
                    queuedTys.push_back(subject);
                else if constexpr (std::is_same_v<T, TypePackId>)
                    queuedTps.push_back(subject);

                return false;
            }
        }

        return true;
    }

    template<typename TID>
    inline bool tryGuessing(TID subject)
    {
        if (shouldGuess.contains(subject))
        {
            if (FFlag::DebugLuauLogTypeFamilies)
                printf("Flagged %s for reduction with guesser.\n", toString(subject, {true}).c_str());

            TypeFunctionReductionGuesser guesser{ctx.arena, ctx.builtins, ctx.normalizer};
            auto guessed = guesser.guess(subject);

            if (guessed)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("Selected %s as the guessed result type.\n", toString(*guessed, {true}).c_str());

                replace(subject, *guessed);
                return true;
            }

            if (FFlag::DebugLuauLogTypeFamilies)
                printf("Failed to produce a guess for the result of %s.\n", toString(subject, {true}).c_str());
        }

        return false;
    }


    void stepType()
    {
        TypeId subject = follow(queuedTys.front());
        queuedTys.pop_front();

        if (irreducible.contains(subject))
            return;

        if (FFlag::DebugLuauLogTypeFamilies)
            printf("Trying to reduce %s\n", toString(subject, {true}).c_str());

        if (const TypeFunctionInstanceType* tfit = get<TypeFunctionInstanceType>(subject))
        {
            SkipTestResult testCyclic = testForSkippability(subject);

            if (!testParameters(subject, tfit) && testCyclic != SkipTestResult::CyclicTypeFunction)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("Irreducible due to irreducible/pending and a non-cyclic function\n");

                return;
            }

            if (tryGuessing(subject))
                return;

            ctx.userFuncName = tfit->userFuncName;

            TypeFunctionReductionResult<TypeId> result = tfit->function->reducer(subject, tfit->typeArguments, tfit->packArguments, NotNull{&ctx});
            handleTypeFunctionReduction(subject, result);
        }
    }

    void stepPack()
    {
        TypePackId subject = follow(queuedTps.front());
        queuedTps.pop_front();

        if (irreducible.contains(subject))
            return;

        if (FFlag::DebugLuauLogTypeFamilies)
            printf("Trying to reduce %s\n", toString(subject, {true}).c_str());

        if (const TypeFunctionInstanceTypePack* tfit = get<TypeFunctionInstanceTypePack>(subject))
        {
            if (!testParameters(subject, tfit))
                return;

            if (tryGuessing(subject))
                return;

            TypeFunctionReductionResult<TypePackId> result =
                tfit->function->reducer(subject, tfit->typeArguments, tfit->packArguments, NotNull{&ctx});
            handleTypeFunctionReduction(subject, result);
        }
    }

    void step()
    {
        if (!queuedTys.empty())
            stepType();
        else if (!queuedTps.empty())
            stepPack();
    }
};

struct LuauTempThreadPopper
{
    explicit LuauTempThreadPopper(lua_State* L)
        : L(L)
    {
    }
    ~LuauTempThreadPopper()
    {
        lua_pop(L, 1);
    }

    lua_State* L = nullptr;
};

static FunctionGraphReductionResult reduceFunctionsInternal(
    VecDeque<TypeId> queuedTys,
    VecDeque<TypePackId> queuedTps,
    TypeOrTypePackIdSet shouldGuess,
    std::vector<TypeId> cyclics,
    Location location,
    TypeFunctionContext ctx,
    bool force
)
{
    TypeFunctionReducer reducer{std::move(queuedTys), std::move(queuedTps), std::move(shouldGuess), std::move(cyclics), location, ctx, force};
    int iterationCount = 0;

    while (!reducer.done())
    {
        reducer.step();

        ++iterationCount;
        if (iterationCount > DFInt::LuauTypeFamilyGraphReductionMaximumSteps)
        {
            reducer.result.errors.emplace_back(location, CodeTooComplex{});
            break;
        }
    }

    return std::move(reducer.result);
}

FunctionGraphReductionResult reduceTypeFunctions(TypeId entrypoint, Location location, TypeFunctionContext ctx, bool force)
{
    InstanceCollector collector;

    try
    {
        collector.traverse(entrypoint);
    }
    catch (RecursionLimitException&)
    {
        return FunctionGraphReductionResult{};
    }

    if (collector.tys.empty() && collector.tps.empty())
        return {};

    return reduceFunctionsInternal(
        std::move(collector.tys),
        std::move(collector.tps),
        std::move(collector.shouldGuess),
        std::move(collector.cyclicInstance),
        location,
        ctx,
        force
    );
}

FunctionGraphReductionResult reduceTypeFunctions(TypePackId entrypoint, Location location, TypeFunctionContext ctx, bool force)
{
    InstanceCollector collector;

    try
    {
        collector.traverse(entrypoint);
    }
    catch (RecursionLimitException&)
    {
        return FunctionGraphReductionResult{};
    }

    if (collector.tys.empty() && collector.tps.empty())
        return {};

    return reduceFunctionsInternal(
        std::move(collector.tys),
        std::move(collector.tps),
        std::move(collector.shouldGuess),
        std::move(collector.cyclicInstance),
        location,
        ctx,
        force
    );
}

bool isPending(TypeId ty, ConstraintSolver* solver)
{
    return is<BlockedType, PendingExpansionType, TypeFunctionInstanceType>(ty) || (solver && solver->hasUnresolvedConstraints(ty));
}

template<typename F, typename... Args>
static std::optional<TypeFunctionReductionResult<TypeId>> tryDistributeTypeFunctionApp(
    F f,
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx,
    Args&&... args
)
{
    // op (a | b) (c | d) ~ (op a (c | d)) | (op b (c | d)) ~ (op a c) | (op a d) | (op b c) | (op b d)
    bool uninhabited = false;
    std::vector<TypeId> blockedTypes;
    std::vector<TypeId> results;
    size_t cartesianProductSize = 1;

    const UnionType* firstUnion = nullptr;
    size_t unionIndex = 0;

    std::vector<TypeId> arguments = typeParams;
    for (size_t i = 0; i < arguments.size(); ++i)
    {
        const UnionType* ut = get<UnionType>(follow(arguments[i]));
        if (!ut)
            continue;

        // We want to find the first union type in the set of arguments to distribute that one and only that one union.
        // The function `f` we have is recursive, so `arguments[unionIndex]` will be updated in-place for each option in
        // the union we've found in this context, so that index will no longer be a union type. Any other arguments at
        // index + 1 or after will instead be distributed, if those are a union, which will be subjected to the same rules.
        if (!firstUnion && ut)
        {
            firstUnion = ut;
            unionIndex = i;
        }

        cartesianProductSize *= std::distance(begin(ut), end(ut));

        // TODO: We'd like to report that the type function application is too complex here.
        if (size_t(DFInt::LuauTypeFamilyApplicationCartesianProductLimit) <= cartesianProductSize)
            return {{std::nullopt, true, {}, {}}};
    }

    if (!firstUnion)
    {
        // If we couldn't find any union type argument, we're not distributing.
        return std::nullopt;
    }

    for (TypeId option : firstUnion)
    {
        arguments[unionIndex] = option;

        TypeFunctionReductionResult<TypeId> result = f(instance, arguments, packParams, ctx, args...);
        blockedTypes.insert(blockedTypes.end(), result.blockedTypes.begin(), result.blockedTypes.end());
        uninhabited |= result.uninhabited;

        if (result.uninhabited || !result.result)
            break;
        else
            results.push_back(*result.result);
    }

    if (uninhabited || !blockedTypes.empty())
        return {{std::nullopt, uninhabited, blockedTypes, {}}};

    if (!results.empty())
    {
        if (results.size() == 1)
            return {{results[0], false, {}, {}}};

        TypeId resultTy = ctx->arena->addType(TypeFunctionInstanceType{
            NotNull{&builtinTypeFunctions().unionFunc},
            std::move(results),
            {},
        });

        return {{resultTy, false, {}, {}}};
    }

    return std::nullopt;
}

TypeFunctionReductionResult<TypeId> userDefinedTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    auto typeFunction = getMutable<TypeFunctionInstanceType>(instance);

    if (FFlag::LuauUserTypeFunExportedAndLocal)
    {
        if (typeFunction->userFuncData.owner.expired())
        {
            ctx->ice->ice("user-defined type function module has expired");
            return {std::nullopt, true, {}, {}};
        }

        if (!typeFunction->userFuncName || !typeFunction->userFuncData.definition)
        {
            ctx->ice->ice("all user-defined type functions must have an associated function definition");
            return {std::nullopt, true, {}, {}};
        }
    }
    else
    {
        if (!ctx->userFuncName)
        {
            ctx->ice->ice("all user-defined type functions must have an associated function definition");
            return {std::nullopt, true, {}, {}};
        }
    }

    // If type functions cannot be evaluated because of errors in the code, we do not generate any additional ones
    if (!ctx->typeFunctionRuntime->allowEvaluation)
        return {ctx->builtins->errorRecoveryType(), false, {}, {}};

    for (auto typeParam : typeParams)
    {
        TypeId ty = follow(typeParam);

        // block if we need to
        if (isPending(ty, ctx->solver))
            return {std::nullopt, false, {ty}, {}};
    }

    if (FFlag::LuauUserTypeFunExportedAndLocal)
    {
        // Ensure that whole type function environment is registered
        for (auto& [name, definition] : typeFunction->userFuncData.environment)
        {
            if (std::optional<std::string> error = ctx->typeFunctionRuntime->registerFunction(definition))
            {
                // Failure to register at this point means that original definition had to error out and should not have been present in the
                // environment
                ctx->ice->ice("user-defined type function reference cannot be registered");
                return {std::nullopt, true, {}, {}};
            }
        }
    }

    AstName name = FFlag::LuauUserTypeFunExportedAndLocal ? typeFunction->userFuncData.definition->name : *ctx->userFuncName;

    lua_State* global = ctx->typeFunctionRuntime->state.get();

    if (global == nullptr)
        return {std::nullopt, true, {}, {}, format("'%s' type function: cannot be evaluated in this context", name.value)};

    // Separate sandboxed thread for individual execution and private globals
    lua_State* L = lua_newthread(global);
    LuauTempThreadPopper popper(global);

    if (FFlag::LuauUserTypeFunExportedAndLocal)
    {
        // Fetch the function we want to evaluate
        lua_pushlightuserdata(L, typeFunction->userFuncData.definition);
        lua_gettable(L, LUA_REGISTRYINDEX);

        if (!lua_isfunction(L, -1))
        {
            ctx->ice->ice("user-defined type function reference cannot be found in the registry");
            return {std::nullopt, true, {}, {}};
        }

        // Build up the environment
        lua_getfenv(L, -1);
        lua_setreadonly(L, -1, false);

        for (auto& [name, definition] : typeFunction->userFuncData.environment)
        {
            lua_pushlightuserdata(L, definition);
            lua_gettable(L, LUA_REGISTRYINDEX);

            if (!lua_isfunction(L, -1))
            {
                ctx->ice->ice("user-defined type function reference cannot be found in the registry");
                return {std::nullopt, true, {}, {}};
            }

            lua_setfield(L, -2, name.c_str());
        }

        lua_setreadonly(L, -1, true);
        lua_pop(L, 1);
    }
    else
    {
        lua_getglobal(global, name.value);
        lua_xmove(global, L, 1);
    }

    if (FFlag::LuauUserDefinedTypeFunctionResetState)
        resetTypeFunctionState(L);

    // Push serialized arguments onto the stack

    // Since there aren't any new class types being created in type functions, there isn't a deserialization function
    // class types. Instead, we can keep this map and return the mapping as the "deserialized value"
    std::unique_ptr<TypeFunctionRuntimeBuilderState> runtimeBuilder = std::make_unique<TypeFunctionRuntimeBuilderState>(ctx);
    for (auto typeParam : typeParams)
    {
        TypeId ty = follow(typeParam);
        // This is checked at the top of the function, and should still be true.
        LUAU_ASSERT(!isPending(ty, ctx->solver));

        TypeFunctionTypeId serializedTy = serialize(ty, runtimeBuilder.get());
        // Check if there were any errors while serializing
        if (runtimeBuilder->errors.size() != 0)
            return {std::nullopt, true, {}, {}, runtimeBuilder->errors.front()};

        allocTypeUserData(L, serializedTy->type);
    }

    // Set up an interrupt handler for type functions to respect type checking limits and LSP cancellation requests.
    lua_callbacks(L)->interrupt = [](lua_State* L, int gc)
    {
        auto ctx = static_cast<const TypeFunctionRuntime*>(lua_getthreaddata(lua_mainthread(L)));
        if (ctx->limits->finishTime && TimeTrace::getClock() > *ctx->limits->finishTime)
            throw TimeLimitError(ctx->ice->moduleName);

        if (ctx->limits->cancellationToken && ctx->limits->cancellationToken->requested())
            throw UserCancelError(ctx->ice->moduleName);
    };

    if (auto error = checkResultForError(L, name.value, lua_pcall(L, int(typeParams.size()), 1, 0)))
        return {std::nullopt, true, {}, {}, error};

    // If the return value is not a type userdata, return with error message
    if (!isTypeUserData(L, 1))
        return {std::nullopt, true, {}, {}, format("'%s' type function: returned a non-type value", name.value)};

    TypeFunctionTypeId retTypeFunctionTypeId = getTypeUserData(L, 1);

    // No errors should be present here since we should've returned already if any were raised during serialization.
    LUAU_ASSERT(runtimeBuilder->errors.size() == 0);

    TypeId retTypeId = deserialize(retTypeFunctionTypeId, runtimeBuilder.get());

    // At least 1 error occurred while deserializing
    if (runtimeBuilder->errors.size() > 0)
        return {std::nullopt, true, {}, {}, runtimeBuilder->errors.front()};

    return {retTypeId, false, {}, {}};
}

TypeFunctionReductionResult<TypeId> notTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("not type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId ty = follow(typeParams.at(0));

    if (ty == instance)
        return {ctx->builtins->neverType, false, {}, {}};

    if (isPending(ty, ctx->solver))
        return {std::nullopt, false, {ty}, {}};

    if (auto result = tryDistributeTypeFunctionApp(notTypeFunction, instance, typeParams, packParams, ctx))
        return *result;

    // `not` operates on anything and returns a `boolean` always.
    return {ctx->builtins->booleanType, false, {}, {}};
}

TypeFunctionReductionResult<TypeId> lenTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("len type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId operandTy = follow(typeParams.at(0));

    if (operandTy == instance)
        return {ctx->builtins->neverType, false, {}, {}};

    // check to see if the operand type is resolved enough, and wait to reduce if not
    // the use of `typeFromNormal` later necessitates blocking on local types.
    if (isPending(operandTy, ctx->solver))
        return {std::nullopt, false, {operandTy}, {}};

    // if the type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> maybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, operandTy, /* avoidSealingTables */ true);
        if (!maybeGeneralized)
            return {std::nullopt, false, {operandTy}, {}};
        operandTy = *maybeGeneralized;
    }

    std::shared_ptr<const NormalizedType> normTy = ctx->normalizer->normalize(operandTy);
    NormalizationResult inhabited = ctx->normalizer->isInhabited(normTy.get());

    // if the type failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normTy || inhabited == NormalizationResult::HitLimits)
        return {std::nullopt, false, {}, {}};

    // if the operand type is error suppressing, we can immediately reduce to `number`.
    if (normTy->shouldSuppressErrors())
        return {ctx->builtins->numberType, false, {}, {}};

    // # always returns a number, even if its operand is never.
    // if we're checking the length of a string, that works!
    if (inhabited == NormalizationResult::False || normTy->isSubtypeOfString())
        return {ctx->builtins->numberType, false, {}, {}};

    // we use the normalized operand here in case there was an intersection or union.
    TypeId normalizedOperand = follow(ctx->normalizer->typeFromNormal(*normTy));
    if (normTy->hasTopTable() || get<TableType>(normalizedOperand))
        return {ctx->builtins->numberType, false, {}, {}};

    if (auto result = tryDistributeTypeFunctionApp(lenTypeFunction, instance, typeParams, packParams, ctx))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, operandTy, "__len", Location{});
    if (!mmType)
        return {std::nullopt, true, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, false, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, true, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, true, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorRecoveryType(), false, {}, {}};

    TypePackId inferredArgPack = ctx->arena->addTypePack({operandTy});
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (!u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, true, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->normalizer, ctx->typeFunctionRuntime, ctx->ice};
    if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, true, {}, {}};

    // `len` must return a `number`.
    return {ctx->builtins->numberType, false, {}, {}};
}

TypeFunctionReductionResult<TypeId> unmTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("unm type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId operandTy = follow(typeParams.at(0));

    if (operandTy == instance)
        return {ctx->builtins->neverType, false, {}, {}};

    // check to see if the operand type is resolved enough, and wait to reduce if not
    if (isPending(operandTy, ctx->solver))
        return {std::nullopt, false, {operandTy}, {}};

    // if the type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> maybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, operandTy);
        if (!maybeGeneralized)
            return {std::nullopt, false, {operandTy}, {}};
        operandTy = *maybeGeneralized;
    }

    std::shared_ptr<const NormalizedType> normTy = ctx->normalizer->normalize(operandTy);

    // if the operand failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normTy)
        return {std::nullopt, false, {}, {}};

    // if the operand is error suppressing, we can just go ahead and reduce.
    if (normTy->shouldSuppressErrors())
        return {operandTy, false, {}, {}};

    // if we have a `never`, we can never observe that the operation didn't work.
    if (is<NeverType>(operandTy))
        return {ctx->builtins->neverType, false, {}, {}};

    // If the type is exactly `number`, we can reduce now.
    if (normTy->isExactlyNumber())
        return {ctx->builtins->numberType, false, {}, {}};

    if (auto result = tryDistributeTypeFunctionApp(unmTypeFunction, instance, typeParams, packParams, ctx))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, operandTy, "__unm", Location{});
    if (!mmType)
        return {std::nullopt, true, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, false, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, true, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, true, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorRecoveryType(), false, {}, {}};

    TypePackId inferredArgPack = ctx->arena->addTypePack({operandTy});
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (!u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, true, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->normalizer, ctx->typeFunctionRuntime, ctx->ice};
    if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, true, {}, {}};

    if (std::optional<TypeId> ret = first(instantiatedMmFtv->retTypes))
        return {*ret, false, {}, {}};
    else
        return {std::nullopt, true, {}, {}};
}

void dummyStateClose(lua_State*) {}

TypeFunctionRuntime::TypeFunctionRuntime(NotNull<InternalErrorReporter> ice, NotNull<TypeCheckLimits> limits)
    : ice(ice)
    , limits(limits)
    , state(nullptr, dummyStateClose)
{
}

TypeFunctionRuntime::~TypeFunctionRuntime() {}

std::optional<std::string> TypeFunctionRuntime::registerFunction(AstStatTypeFunction* function)
{
    // If evaluation is disabled, we do not generate additional error messages
    if (!allowEvaluation)
        return std::nullopt;

    prepareState();

    lua_State* global = state.get();

    if (FFlag::LuauUserTypeFunExportedAndLocal)
    {
        // Fetch to check if function is already registered
        lua_pushlightuserdata(global, function);
        lua_gettable(global, LUA_REGISTRYINDEX);

        if (!lua_isnil(global, -1))
        {
            lua_pop(global, 1);
            return std::nullopt;
        }

        lua_pop(global, 1);
    }

    AstName name = function->name;

    // Construct ParseResult containing the type function
    Allocator allocator;
    AstNameTable names(allocator);

    AstExpr* exprFunction = function->body;
    AstArray<AstExpr*> exprReturns{&exprFunction, 1};
    AstStatReturn stmtReturn{Location{}, exprReturns};
    AstStat* stmtArray[] = {&stmtReturn};
    AstArray<AstStat*> stmts{stmtArray, 1};
    AstStatBlock exec{Location{}, stmts};
    ParseResult parseResult{&exec, 1};

    BytecodeBuilder builder;
    try
    {
        compileOrThrow(builder, parseResult, names);
    }
    catch (CompileError& e)
    {
        return format("'%s' type function failed to compile with error message: %s", name.value, e.what());
    }

    std::string bytecode = builder.getBytecode();


    // Separate sandboxed thread for individual execution and private globals
    lua_State* L = lua_newthread(global);
    LuauTempThreadPopper popper(global);

    // Create individual environment for the type function
    luaL_sandboxthread(L);

    // Do not allow global writes to that environment
    lua_pushvalue(L, LUA_GLOBALSINDEX);
    lua_setreadonly(L, -1, true);
    lua_pop(L, 1);

    // Load bytecode into Luau state
    if (auto error = checkResultForError(L, name.value, luau_load(L, name.value, bytecode.data(), bytecode.size(), 0)))
        return error;

    // Execute the global function which should return our user-defined type function
    if (auto error = checkResultForError(L, name.value, lua_resume(L, nullptr, 0)))
        return error;

    if (!lua_isfunction(L, -1))
    {
        lua_pop(L, 1);
        return format("Could not find '%s' type function in the global scope", name.value);
    }

    if (FFlag::LuauUserTypeFunExportedAndLocal)
    {
        // Store resulting function in the registry
        lua_pushlightuserdata(global, function);
        lua_xmove(L, global, 1);
        lua_settable(global, LUA_REGISTRYINDEX);
    }
    else
    {
        // Store resulting function in the global environment
        lua_xmove(L, global, 1);
        lua_setglobal(global, name.value);
    }

    return std::nullopt;
}

void TypeFunctionRuntime::prepareState()
{
    if (state)
        return;

    state = StateRef(lua_newstate(typeFunctionAlloc, nullptr), lua_close);
    lua_State* L = state.get();

    lua_setthreaddata(L, this);

    setTypeFunctionEnvironment(L);

    registerTypeUserData(L);

    if (FFlag::LuauUserTypeFunFixRegister)
        registerTypesLibrary(L);

    luaL_sandbox(L);
    luaL_sandboxthread(L);
}

TypeFunctionContext::TypeFunctionContext(NotNull<ConstraintSolver> cs, NotNull<Scope> scope, NotNull<const Constraint> constraint)
    : arena(cs->arena)
    , builtins(cs->builtinTypes)
    , scope(scope)
    , normalizer(cs->normalizer)
    , typeFunctionRuntime(cs->typeFunctionRuntime)
    , ice(NotNull{&cs->iceReporter})
    , limits(NotNull{&cs->limits})
    , solver(cs.get())
    , constraint(constraint.get())
{
}

NotNull<Constraint> TypeFunctionContext::pushConstraint(ConstraintV&& c) const
{
    LUAU_ASSERT(solver);
    NotNull<Constraint> newConstraint = solver->pushConstraint(scope, constraint ? constraint->location : Location{}, std::move(c));

    // Every constraint that is blocked on the current constraint must also be
    // blocked on this new one.
    if (constraint)
        solver->inheritBlocks(NotNull{constraint}, newConstraint);

    return newConstraint;
}

TypeFunctionReductionResult<TypeId> numericBinopTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx,
    const std::string metamethod
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // isPending of `lhsTy` or `rhsTy` would return true, even if it cycles. We want a different answer for that.
    if (lhsTy == instance || rhsTy == instance)
        return {ctx->builtins->neverType, false, {}, {}};

    // if we have a `never`, we can never observe that the math operator is unreachable.
    if (is<NeverType>(lhsTy) || is<NeverType>(rhsTy))
        return {ctx->builtins->neverType, false, {}, {}};

    const Location location = ctx->constraint ? ctx->constraint->location : Location{};

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> lhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, lhsTy);
        std::optional<TypeId> rhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, rhsTy);

        if (!lhsMaybeGeneralized)
            return {std::nullopt, false, {lhsTy}, {}};
        else if (!rhsMaybeGeneralized)
            return {std::nullopt, false, {rhsTy}, {}};

        lhsTy = *lhsMaybeGeneralized;
        rhsTy = *rhsMaybeGeneralized;
    }

    // TODO: Normalization needs to remove cyclic type functions from a `NormalizedType`.
    std::shared_ptr<const NormalizedType> normLhsTy = ctx->normalizer->normalize(lhsTy);
    std::shared_ptr<const NormalizedType> normRhsTy = ctx->normalizer->normalize(rhsTy);

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy)
        return {std::nullopt, false, {}, {}};

    // if one of the types is error suppressing, we can reduce to `any` since we should suppress errors in the result of the usage.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->anyType, false, {}, {}};

    // if we're adding two `number` types, the result is `number`.
    if (normLhsTy->isExactlyNumber() && normRhsTy->isExactlyNumber())
        return {ctx->builtins->numberType, false, {}, {}};

    if (auto result = tryDistributeTypeFunctionApp(numericBinopTypeFunction, instance, typeParams, packParams, ctx, metamethod))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, lhsTy, metamethod, location);
    bool reversed = false;
    if (!mmType)
    {
        mmType = findMetatableEntry(ctx->builtins, dummy, rhsTy, metamethod, location);
        reversed = true;
    }

    if (!mmType)
        return {std::nullopt, true, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, false, {*mmType}, {}};

    TypePackId argPack = ctx->arena->addTypePack({lhsTy, rhsTy});
    SolveResult solveResult;

    if (!reversed)
        solveResult = solveFunctionCall(
            ctx->arena, ctx->builtins, ctx->normalizer, ctx->typeFunctionRuntime, ctx->ice, ctx->limits, ctx->scope, location, *mmType, argPack
        );
    else
    {
        TypePack* p = getMutable<TypePack>(argPack);
        std::swap(p->head.front(), p->head.back());
        solveResult = solveFunctionCall(
            ctx->arena, ctx->builtins, ctx->normalizer, ctx->typeFunctionRuntime, ctx->ice, ctx->limits, ctx->scope, location, *mmType, argPack
        );
    }

    if (!solveResult.typePackId.has_value())
        return {std::nullopt, true, {}, {}};

    TypePack extracted = extendTypePack(*ctx->arena, ctx->builtins, *solveResult.typePackId, 1);
    if (extracted.head.empty())
        return {std::nullopt, true, {}, {}};

    return {extracted.head.front(), false, {}, {}};
}

TypeFunctionReductionResult<TypeId> addTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("add type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__add");
}

TypeFunctionReductionResult<TypeId> subTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("sub type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__sub");
}

TypeFunctionReductionResult<TypeId> mulTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("mul type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__mul");
}

TypeFunctionReductionResult<TypeId> divTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("div type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__div");
}

TypeFunctionReductionResult<TypeId> idivTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("integer div type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__idiv");
}

TypeFunctionReductionResult<TypeId> powTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("pow type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__pow");
}

TypeFunctionReductionResult<TypeId> modTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("modulo type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__mod");
}

TypeFunctionReductionResult<TypeId> concatTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("concat type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // isPending of `lhsTy` or `rhsTy` would return true, even if it cycles. We want a different answer for that.
    if (lhsTy == instance || rhsTy == instance)
        return {ctx->builtins->neverType, false, {}, {}};

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> lhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, lhsTy);
        std::optional<TypeId> rhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, rhsTy);

        if (!lhsMaybeGeneralized)
            return {std::nullopt, false, {lhsTy}, {}};
        else if (!rhsMaybeGeneralized)
            return {std::nullopt, false, {rhsTy}, {}};

        lhsTy = *lhsMaybeGeneralized;
        rhsTy = *rhsMaybeGeneralized;
    }

    std::shared_ptr<const NormalizedType> normLhsTy = ctx->normalizer->normalize(lhsTy);
    std::shared_ptr<const NormalizedType> normRhsTy = ctx->normalizer->normalize(rhsTy);

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy)
        return {std::nullopt, false, {}, {}};

    // if one of the types is error suppressing, we can reduce to `any` since we should suppress errors in the result of the usage.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->anyType, false, {}, {}};

    // if we have a `never`, we can never observe that the numeric operator didn't work.
    if (is<NeverType>(lhsTy) || is<NeverType>(rhsTy))
        return {ctx->builtins->neverType, false, {}, {}};

    // if we're concatenating two elements that are either strings or numbers, the result is `string`.
    if ((normLhsTy->isSubtypeOfString() || normLhsTy->isExactlyNumber()) && (normRhsTy->isSubtypeOfString() || normRhsTy->isExactlyNumber()))
        return {ctx->builtins->stringType, false, {}, {}};

    if (auto result = tryDistributeTypeFunctionApp(concatTypeFunction, instance, typeParams, packParams, ctx))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, lhsTy, "__concat", Location{});
    bool reversed = false;
    if (!mmType)
    {
        mmType = findMetatableEntry(ctx->builtins, dummy, rhsTy, "__concat", Location{});
        reversed = true;
    }

    if (!mmType)
        return {std::nullopt, true, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, false, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, true, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, true, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorRecoveryType(), false, {}, {}};

    std::vector<TypeId> inferredArgs;
    if (!reversed)
        inferredArgs = {lhsTy, rhsTy};
    else
        inferredArgs = {rhsTy, lhsTy};

    TypePackId inferredArgPack = ctx->arena->addTypePack(std::move(inferredArgs));
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (!u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, true, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->normalizer, ctx->typeFunctionRuntime, ctx->ice};
    if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, true, {}, {}};

    return {ctx->builtins->stringType, false, {}, {}};
}

TypeFunctionReductionResult<TypeId> andTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("and type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // t1 = and<lhs, t1> ~> lhs
    if (follow(rhsTy) == instance && lhsTy != rhsTy)
        return {lhsTy, false, {}, {}};
    // t1 = and<t1, rhs> ~> rhs
    if (follow(lhsTy) == instance && lhsTy != rhsTy)
        return {rhsTy, false, {}, {}};

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> lhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, lhsTy);
        std::optional<TypeId> rhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, rhsTy);

        if (!lhsMaybeGeneralized)
            return {std::nullopt, false, {lhsTy}, {}};
        else if (!rhsMaybeGeneralized)
            return {std::nullopt, false, {rhsTy}, {}};

        lhsTy = *lhsMaybeGeneralized;
        rhsTy = *rhsMaybeGeneralized;
    }

    // And evalutes to a boolean if the LHS is falsey, and the RHS type if LHS is truthy.
    SimplifyResult filteredLhs = simplifyIntersection(ctx->builtins, ctx->arena, lhsTy, ctx->builtins->falsyType);
    SimplifyResult overallResult = simplifyUnion(ctx->builtins, ctx->arena, rhsTy, filteredLhs.result);
    std::vector<TypeId> blockedTypes{};
    for (auto ty : filteredLhs.blockedTypes)
        blockedTypes.push_back(ty);
    for (auto ty : overallResult.blockedTypes)
        blockedTypes.push_back(ty);
    return {overallResult.result, false, std::move(blockedTypes), {}};
}

TypeFunctionReductionResult<TypeId> orTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("or type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // t1 = or<lhs, t1> ~> lhs
    if (follow(rhsTy) == instance && lhsTy != rhsTy)
        return {lhsTy, false, {}, {}};
    // t1 = or<t1, rhs> ~> rhs
    if (follow(lhsTy) == instance && lhsTy != rhsTy)
        return {rhsTy, false, {}, {}};

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> lhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, lhsTy);
        std::optional<TypeId> rhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, rhsTy);

        if (!lhsMaybeGeneralized)
            return {std::nullopt, false, {lhsTy}, {}};
        else if (!rhsMaybeGeneralized)
            return {std::nullopt, false, {rhsTy}, {}};

        lhsTy = *lhsMaybeGeneralized;
        rhsTy = *rhsMaybeGeneralized;
    }

    // Or evalutes to the LHS type if the LHS is truthy, and the RHS type if LHS is falsy.
    SimplifyResult filteredLhs = simplifyIntersection(ctx->builtins, ctx->arena, lhsTy, ctx->builtins->truthyType);
    SimplifyResult overallResult = simplifyUnion(ctx->builtins, ctx->arena, rhsTy, filteredLhs.result);
    std::vector<TypeId> blockedTypes{};
    for (auto ty : filteredLhs.blockedTypes)
        blockedTypes.push_back(ty);
    for (auto ty : overallResult.blockedTypes)
        blockedTypes.push_back(ty);
    return {overallResult.result, false, std::move(blockedTypes), {}};
}

static TypeFunctionReductionResult<TypeId> comparisonTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx,
    const std::string metamethod
)
{

    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    if (lhsTy == instance || rhsTy == instance)
        return {ctx->builtins->neverType, false, {}, {}};

    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    // Algebra Reduction Rules for comparison type functions
    // Note that comparing to never tells you nothing about the other operand
    // lt< 'a , never> -> continue
    // lt< never, 'a>  -> continue
    // lt< 'a, t>      -> 'a is t - we'll solve the constraint, return and solve lt<t, t> -> bool
    // lt< t, 'a>      -> same as above
    bool canSubmitConstraint = ctx->solver && ctx->constraint;
    bool lhsFree = get<FreeType>(lhsTy) != nullptr;
    bool rhsFree = get<FreeType>(rhsTy) != nullptr;
    if (canSubmitConstraint)
    {
        // Implement injective type functions for comparison type functions
        // lt <number, t> implies t is number
        // lt <t, number> implies t is number
        if (lhsFree && isNumber(rhsTy))
            emplaceType<BoundType>(asMutable(lhsTy), ctx->builtins->numberType);
        else if (rhsFree && isNumber(lhsTy))
            emplaceType<BoundType>(asMutable(rhsTy), ctx->builtins->numberType);
        else if (lhsFree && ctx->normalizer->isInhabited(rhsTy) != NormalizationResult::False)
        {
            auto c1 = ctx->pushConstraint(EqualityConstraint{lhsTy, rhsTy});
            const_cast<Constraint*>(ctx->constraint)->dependencies.emplace_back(c1);
        }
        else if (rhsFree && ctx->normalizer->isInhabited(lhsTy) != NormalizationResult::False)
        {
            auto c1 = ctx->pushConstraint(EqualityConstraint{rhsTy, lhsTy});
            const_cast<Constraint*>(ctx->constraint)->dependencies.emplace_back(c1);
        }
    }

    // The above might have caused the operand types to be rebound, we need to follow them again
    lhsTy = follow(lhsTy);
    rhsTy = follow(rhsTy);

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> lhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, lhsTy);
        std::optional<TypeId> rhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, rhsTy);

        if (!lhsMaybeGeneralized)
            return {std::nullopt, false, {lhsTy}, {}};
        else if (!rhsMaybeGeneralized)
            return {std::nullopt, false, {rhsTy}, {}};

        lhsTy = *lhsMaybeGeneralized;
        rhsTy = *rhsMaybeGeneralized;
    }

    // check to see if both operand types are resolved enough, and wait to reduce if not

    std::shared_ptr<const NormalizedType> normLhsTy = ctx->normalizer->normalize(lhsTy);
    std::shared_ptr<const NormalizedType> normRhsTy = ctx->normalizer->normalize(rhsTy);
    NormalizationResult lhsInhabited = ctx->normalizer->isInhabited(normLhsTy.get());
    NormalizationResult rhsInhabited = ctx->normalizer->isInhabited(normRhsTy.get());

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy || lhsInhabited == NormalizationResult::HitLimits || rhsInhabited == NormalizationResult::HitLimits)
        return {std::nullopt, false, {}, {}};

    // if one of the types is error suppressing, we can just go ahead and reduce.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->booleanType, false, {}, {}};

    // if we have an uninhabited type (e.g. `never`), we can never observe that the comparison didn't work.
    if (lhsInhabited == NormalizationResult::False || rhsInhabited == NormalizationResult::False)
        return {ctx->builtins->booleanType, false, {}, {}};

    // If both types are some strict subset of `string`, we can reduce now.
    if (normLhsTy->isSubtypeOfString() && normRhsTy->isSubtypeOfString())
        return {ctx->builtins->booleanType, false, {}, {}};

    // If both types are exactly `number`, we can reduce now.
    if (normLhsTy->isExactlyNumber() && normRhsTy->isExactlyNumber())
        return {ctx->builtins->booleanType, false, {}, {}};

    if (auto result = tryDistributeTypeFunctionApp(comparisonTypeFunction, instance, typeParams, packParams, ctx, metamethod))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, lhsTy, metamethod, Location{});
    if (!mmType)
        mmType = findMetatableEntry(ctx->builtins, dummy, rhsTy, metamethod, Location{});

    if (!mmType)
        return {std::nullopt, true, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, false, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, true, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, true, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorRecoveryType(), false, {}, {}};

    TypePackId inferredArgPack = ctx->arena->addTypePack({lhsTy, rhsTy});
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (!u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, true, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->normalizer, ctx->typeFunctionRuntime, ctx->ice};
    if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, true, {}, {}};

    return {ctx->builtins->booleanType, false, {}, {}};
}

TypeFunctionReductionResult<TypeId> ltTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("lt type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return comparisonTypeFunction(instance, typeParams, packParams, ctx, "__lt");
}

TypeFunctionReductionResult<TypeId> leTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("le type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return comparisonTypeFunction(instance, typeParams, packParams, ctx, "__le");
}

TypeFunctionReductionResult<TypeId> eqTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("eq type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> lhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, lhsTy);
        std::optional<TypeId> rhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, rhsTy);

        if (!lhsMaybeGeneralized)
            return {std::nullopt, false, {lhsTy}, {}};
        else if (!rhsMaybeGeneralized)
            return {std::nullopt, false, {rhsTy}, {}};

        lhsTy = *lhsMaybeGeneralized;
        rhsTy = *rhsMaybeGeneralized;
    }

    std::shared_ptr<const NormalizedType> normLhsTy = ctx->normalizer->normalize(lhsTy);
    std::shared_ptr<const NormalizedType> normRhsTy = ctx->normalizer->normalize(rhsTy);
    NormalizationResult lhsInhabited = ctx->normalizer->isInhabited(normLhsTy.get());
    NormalizationResult rhsInhabited = ctx->normalizer->isInhabited(normRhsTy.get());

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy || lhsInhabited == NormalizationResult::HitLimits || rhsInhabited == NormalizationResult::HitLimits)
        return {std::nullopt, false, {}, {}};

    // if one of the types is error suppressing, we can just go ahead and reduce.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->booleanType, false, {}, {}};

    // if we have a `never`, we can never observe that the comparison didn't work.
    if (lhsInhabited == NormalizationResult::False || rhsInhabited == NormalizationResult::False)
        return {ctx->builtins->booleanType, false, {}, {}};

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, lhsTy, "__eq", Location{});
    if (!mmType)
        mmType = findMetatableEntry(ctx->builtins, dummy, rhsTy, "__eq", Location{});

    // if neither type has a metatable entry for `__eq`, then we'll check for inhabitance of the intersection!
    NormalizationResult intersectInhabited = ctx->normalizer->isIntersectionInhabited(lhsTy, rhsTy);
    if (!mmType)
    {
        if (intersectInhabited == NormalizationResult::True)
            return {ctx->builtins->booleanType, false, {}, {}}; // if it's inhabited, everything is okay!

        // we might be in a case where we still want to accept the comparison...
        if (intersectInhabited == NormalizationResult::False)
        {
            // if they're both subtypes of `string` but have no common intersection, the comparison is allowed but always `false`.
            if (normLhsTy->isSubtypeOfString() && normRhsTy->isSubtypeOfString())
                return {ctx->builtins->falseType, false, {}, {}};

            // if they're both subtypes of `boolean` but have no common intersection, the comparison is allowed but always `false`.
            if (normLhsTy->isSubtypeOfBooleans() && normRhsTy->isSubtypeOfBooleans())
                return {ctx->builtins->falseType, false, {}, {}};
        }

        return {std::nullopt, true, {}, {}}; // if it's not, then this type function is irreducible!
    }

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, false, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, true, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, true, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorRecoveryType(), false, {}, {}};

    TypePackId inferredArgPack = ctx->arena->addTypePack({lhsTy, rhsTy});
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (!u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, true, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->normalizer, ctx->typeFunctionRuntime, ctx->ice};
    if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, true, {}, {}};

    return {ctx->builtins->booleanType, false, {}, {}};
}

// Collect types that prevent us from reducing a particular refinement.
struct FindRefinementBlockers : TypeOnceVisitor
{
    DenseHashSet<TypeId> found{nullptr};
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

TypeFunctionReductionResult<TypeId> refineTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() < 2 || !packParams.empty())
    {
        ctx->ice->ice("refine type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId targetTy = follow(typeParams.at(0));
    std::vector<TypeId> discriminantTypes;
    for (size_t i = 1; i < typeParams.size(); i++)
        discriminantTypes.push_back(follow(typeParams.at(i)));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(targetTy, ctx->solver))
        return {std::nullopt, false, {targetTy}, {}};
    else
    {
        for (auto t : discriminantTypes)
        {
            if (isPending(t, ctx->solver))
                return {std::nullopt, false, {t}, {}};
        }
    }
    // Refine a target type and a discriminant one at a time.
    // Returns result : TypeId, toBlockOn : vector<TypeId>
    auto stepRefine = [&ctx](TypeId target, TypeId discriminant) -> std::pair<TypeId, std::vector<TypeId>>
    {
        std::vector<TypeId> toBlock;
        if (ctx->solver)
        {
            std::optional<TypeId> targetMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, target);
            std::optional<TypeId> discriminantMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, discriminant);

            if (!targetMaybeGeneralized)
                return std::pair<TypeId, std::vector<TypeId>>{nullptr, {target}};
            else if (!discriminantMaybeGeneralized)
                return std::pair<TypeId, std::vector<TypeId>>{nullptr, {discriminant}};

            target = *targetMaybeGeneralized;
            discriminant = *discriminantMaybeGeneralized;
        }

        // we need a more complex check for blocking on the discriminant in particular
        FindRefinementBlockers frb;
        frb.traverse(discriminant);

        if (!frb.found.empty())
            return {nullptr, {frb.found.begin(), frb.found.end()}};

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
        if (auto nt = get<NegationType>(discriminant))
        {
            if (FFlag::LuauRemoveNotAnyHack)
            {
                if (get<NoRefineType>(follow(nt->ty)))
                    return {target, {}};
            }
            else
            {
                if (get<AnyType>(follow(nt->ty)))
                    return {target, {}};
            }
        }

        // If the target type is a table, then simplification already implements the logic to deal with refinements properly since the
        // type of the discriminant is guaranteed to only ever be an (arbitrarily-nested) table of a single property type.
        if (get<TableType>(target))
        {
            SimplifyResult result = simplifyIntersection(ctx->builtins, ctx->arena, target, discriminant);
            if (!result.blockedTypes.empty())
                return {nullptr, {result.blockedTypes.begin(), result.blockedTypes.end()}};

            return {result.result, {}};
        }

        // In the general case, we'll still use normalization though.
        TypeId intersection = ctx->arena->addType(IntersectionType{{target, discriminant}});
        std::shared_ptr<const NormalizedType> normIntersection = ctx->normalizer->normalize(intersection);
        std::shared_ptr<const NormalizedType> normType = ctx->normalizer->normalize(target);

        // if the intersection failed to normalize, we can't reduce, but know nothing about inhabitance.
        if (!normIntersection || !normType)
            return {nullptr, {}};

        TypeId resultTy = ctx->normalizer->typeFromNormal(*normIntersection);
        // include the error type if the target type is error-suppressing and the intersection we computed is not
        if (normType->shouldSuppressErrors() && !normIntersection->shouldSuppressErrors())
            resultTy = ctx->arena->addType(UnionType{{resultTy, ctx->builtins->errorType}});

        return {resultTy, {}};
    };

    // refine target with each discriminant type in sequence (reverse of insertion order)
    // If we cannot proceed, block. If all discriminant types refine successfully, return
    // the result
    TypeId target = targetTy;
    while (!discriminantTypes.empty())
    {
        TypeId discriminant = discriminantTypes.back();
        auto [refined, blocked] = stepRefine(target, discriminant);

        if (blocked.empty() && refined == nullptr)
            return {std::nullopt, false, {}, {}};

        if (!blocked.empty())
            return {std::nullopt, false, blocked, {}};

        target = refined;
        discriminantTypes.pop_back();
    }
    return {target, false, {}, {}};
}

TypeFunctionReductionResult<TypeId> singletonTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("singleton type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId type = follow(typeParams.at(0));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(type, ctx->solver))
        return {std::nullopt, false, {type}, {}};

    // if the type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> maybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, type);
        if (!maybeGeneralized)
            return {std::nullopt, false, {type}, {}};
        type = *maybeGeneralized;
    }

    TypeId followed = type;
    // we want to follow through a negation here as well.
    if (auto negation = get<NegationType>(followed))
        followed = follow(negation->ty);

    // if we have a singleton type or `nil`, which is its own singleton type...
    if (get<SingletonType>(followed) || isNil(followed))
        return {type, false, {}, {}};

    // otherwise, we'll return the top type, `unknown`.
    return {ctx->builtins->unknownType, false, {}, {}};
}

TypeFunctionReductionResult<TypeId> unionTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (!packParams.empty())
    {
        ctx->ice->ice("union type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    // if we only have one parameter, there's nothing to do.
    if (typeParams.size() == 1)
        return {follow(typeParams[0]), false, {}, {}};

    // we need to follow all of the type parameters.
    std::vector<TypeId> types;
    types.reserve(typeParams.size());
    for (auto ty : typeParams)
        types.emplace_back(follow(ty));

    // unfortunately, we need this short-circuit: if all but one type is `never`, we will return that one type.
    // this also will early return if _everything_ is `never`, since we already have to check that.
    std::optional<TypeId> lastType = std::nullopt;
    for (auto ty : types)
    {
        // if we have a previous type and it's not `never` and the current type isn't `never`...
        if (lastType && !get<NeverType>(lastType) && !get<NeverType>(ty))
        {
            // we know we are not taking the short-circuited path.
            lastType = std::nullopt;
            break;
        }

        if (get<NeverType>(ty))
            continue;
        lastType = ty;
    }

    // if we still have a `lastType` at the end, we're taking the short-circuit and reducing early.
    if (lastType)
        return {lastType, false, {}, {}};

    // check to see if the operand types are resolved enough, and wait to reduce if not
    for (auto ty : types)
        if (isPending(ty, ctx->solver))
            return {std::nullopt, false, {ty}, {}};

    // fold over the types with `simplifyUnion`
    TypeId resultTy = ctx->builtins->neverType;
    for (auto ty : types)
    {
        SimplifyResult result = simplifyUnion(ctx->builtins, ctx->arena, resultTy, ty);
        if (!result.blockedTypes.empty())
            return {std::nullopt, false, {result.blockedTypes.begin(), result.blockedTypes.end()}, {}};

        resultTy = result.result;
    }

    return {resultTy, false, {}, {}};
}


TypeFunctionReductionResult<TypeId> intersectTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (!packParams.empty())
    {
        ctx->ice->ice("intersect type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    // if we only have one parameter, there's nothing to do.
    if (typeParams.size() == 1)
        return {follow(typeParams[0]), false, {}, {}};

    // we need to follow all of the type parameters.
    std::vector<TypeId> types;
    types.reserve(typeParams.size());
    for (auto ty : typeParams)
        types.emplace_back(follow(ty));

    if (FFlag::LuauRemoveNotAnyHack)
    {
        // if we only have two parameters and one is `*no-refine*`, we're all done.
        if (types.size() == 2 && get<NoRefineType>(types[1]))
            return {types[0], false, {}, {}};
        else if (types.size() == 2 && get<NoRefineType>(types[0]))
            return {types[1], false, {}, {}};
    }

    // check to see if the operand types are resolved enough, and wait to reduce if not
    // if any of them are `never`, the intersection will always be `never`, so we can reduce directly.
    for (auto ty : types)
    {
        if (isPending(ty, ctx->solver))
            return {std::nullopt, false, {ty}, {}};
        else if (get<NeverType>(ty))
            return {ctx->builtins->neverType, false, {}, {}};
    }

    // fold over the types with `simplifyIntersection`
    TypeId resultTy = ctx->builtins->unknownType;
    for (auto ty : types)
    {
        // skip any `*no-refine*` types.
        if (FFlag::LuauRemoveNotAnyHack && get<NoRefineType>(ty))
            continue;

        SimplifyResult result = simplifyIntersection(ctx->builtins, ctx->arena, resultTy, ty);
        if (!result.blockedTypes.empty())
            return {std::nullopt, false, {result.blockedTypes.begin(), result.blockedTypes.end()}, {}};

        resultTy = result.result;
    }

    // if the intersection simplifies to `never`, this gives us bad autocomplete.
    // we'll just produce the intersection plainly instead, but this might be revisitable
    // if we ever give `never` some kind of "explanation" trail.
    if (get<NeverType>(resultTy))
    {
        TypeId intersection = ctx->arena->addType(IntersectionType{typeParams});
        return {intersection, false, {}, {}};
    }

    return {resultTy, false, {}, {}};
}

// computes the keys of `ty` into `result`
// `isRaw` parameter indicates whether or not we should follow __index metamethods
// returns `false` if `result` should be ignored because the answer is "all strings"
bool computeKeysOf(TypeId ty, Set<std::string>& result, DenseHashSet<TypeId>& seen, bool isRaw, NotNull<TypeFunctionContext> ctx)
{
    // if the type is the top table type, the answer is just "all strings"
    if (get<PrimitiveType>(ty))
        return false;

    // if we've already seen this type, we can do nothing
    if (seen.contains(ty))
        return true;
    seen.insert(ty);

    // if we have a particular table type, we can insert the keys
    if (auto tableTy = get<TableType>(ty))
    {
        if (tableTy->indexer)
        {
            // if we have a string indexer, the answer is, again, "all strings"
            if (isString(tableTy->indexer->indexType))
                return false;
        }

        for (auto [key, _] : tableTy->props)
            result.insert(key);
        return true;
    }

    // otherwise, we have a metatable to deal with
    if (auto metatableTy = get<MetatableType>(ty))
    {
        bool res = true;

        if (!isRaw)
        {
            // findMetatableEntry demands the ability to emit errors, so we must give it
            // the necessary state to do that, even if we intend to just eat the errors.
            ErrorVec dummy;

            std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, ty, "__index", Location{});
            if (mmType)
                res = res && computeKeysOf(*mmType, result, seen, isRaw, ctx);
        }

        res = res && computeKeysOf(metatableTy->table, result, seen, isRaw, ctx);

        return res;
    }

    if (auto classTy = get<ClassType>(ty))
    {
        for (auto [key, _] : classTy->props)
            result.insert(key);

        bool res = true;
        if (classTy->metatable && !isRaw)
        {
            // findMetatableEntry demands the ability to emit errors, so we must give it
            // the necessary state to do that, even if we intend to just eat the errors.
            ErrorVec dummy;

            std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, ty, "__index", Location{});
            if (mmType)
                res = res && computeKeysOf(*mmType, result, seen, isRaw, ctx);
        }

        if (classTy->parent)
            res = res && computeKeysOf(follow(*classTy->parent), result, seen, isRaw, ctx);

        return res;
    }

    // this should not be reachable since the type should be a valid tables or classes part from normalization.
    LUAU_ASSERT(false);
    return false;
}

TypeFunctionReductionResult<TypeId> keyofFunctionImpl(
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx,
    bool isRaw
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("keyof type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId operandTy = follow(typeParams.at(0));

    std::shared_ptr<const NormalizedType> normTy = ctx->normalizer->normalize(operandTy);

    // if the operand failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normTy)
        return {std::nullopt, false, {}, {}};

    // if we don't have either just tables or just classes, we've got nothing to get keys of (at least until a future version perhaps adds classes
    // as well)
    if (normTy->hasTables() == normTy->hasClasses())
        return {std::nullopt, true, {}, {}};

    // this is sort of atrocious, but we're trying to reject any type that has not normalized to a table or a union of tables.
    if (normTy->hasTops() || normTy->hasBooleans() || normTy->hasErrors() || normTy->hasNils() || normTy->hasNumbers() || normTy->hasStrings() ||
        normTy->hasThreads() || normTy->hasBuffers() || normTy->hasFunctions() || normTy->hasTyvars())
        return {std::nullopt, true, {}, {}};

    // we're going to collect the keys in here
    Set<std::string> keys{{}};

    // computing the keys for classes
    if (normTy->hasClasses())
    {
        LUAU_ASSERT(!normTy->hasTables());

        // seen set for key computation for classes
        DenseHashSet<TypeId> seen{{}};

        auto classesIter = normTy->classes.ordering.begin();
        auto classesIterEnd = normTy->classes.ordering.end();
        LUAU_ASSERT(classesIter != classesIterEnd); // should be guaranteed by the `hasClasses` check earlier

        // collect all the properties from the first class type
        if (!computeKeysOf(*classesIter, keys, seen, isRaw, ctx))
            return {ctx->builtins->stringType, false, {}, {}}; // if it failed, we have a top type!

        // we need to look at each class to remove any keys that are not common amongst them all
        while (++classesIter != classesIterEnd)
        {
            seen.clear(); // we'll reuse the same seen set

            Set<std::string> localKeys{{}};

            // we can skip to the next class if this one is a top type
            if (!computeKeysOf(*classesIter, localKeys, seen, isRaw, ctx))
                continue;

            for (auto& key : keys)
            {
                // remove any keys that are not present in each class
                if (!localKeys.contains(key))
                    keys.erase(key);
            }
        }
    }

    // computing the keys for tables
    if (normTy->hasTables())
    {
        LUAU_ASSERT(!normTy->hasClasses());

        // seen set for key computation for tables
        DenseHashSet<TypeId> seen{{}};

        auto tablesIter = normTy->tables.begin();
        LUAU_ASSERT(tablesIter != normTy->tables.end()); // should be guaranteed by the `hasTables` check earlier

        // collect all the properties from the first table type
        if (!computeKeysOf(*tablesIter, keys, seen, isRaw, ctx))
            return {ctx->builtins->stringType, false, {}, {}}; // if it failed, we have the top table type!

        // we need to look at each tables to remove any keys that are not common amongst them all
        while (++tablesIter != normTy->tables.end())
        {
            seen.clear(); // we'll reuse the same seen set

            Set<std::string> localKeys{{}};

            // we can skip to the next table if this one is the top table type
            if (!computeKeysOf(*tablesIter, localKeys, seen, isRaw, ctx))
                continue;

            for (auto& key : keys)
            {
                // remove any keys that are not present in each table
                if (!localKeys.contains(key))
                    keys.erase(key);
            }
        }
    }

    // if the set of keys is empty, `keyof<T>` is `never`
    if (keys.empty())
        return {ctx->builtins->neverType, false, {}, {}};

    // everything is validated, we need only construct our big union of singletons now!
    std::vector<TypeId> singletons;
    singletons.reserve(keys.size());

    for (std::string key : keys)
        singletons.push_back(ctx->arena->addType(SingletonType{StringSingleton{key}}));

    // If there's only one entry, we don't need a UnionType.
    // We can take straight take it from the first entry
    // because it was added into the type arena already.
    if (singletons.size() == 1)
        return {singletons.front(), false, {}, {}};

    return {ctx->arena->addType(UnionType{singletons}), false, {}, {}};
}

TypeFunctionReductionResult<TypeId> keyofTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("keyof type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return keyofFunctionImpl(typeParams, packParams, ctx, /* isRaw */ false);
}

TypeFunctionReductionResult<TypeId> rawkeyofTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("rawkeyof type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return keyofFunctionImpl(typeParams, packParams, ctx, /* isRaw */ true);
}

/* Searches through table's or class's props/indexer to find the property of `ty`
   If found, appends that property to `result` and returns true
   Else, returns false */
bool searchPropsAndIndexer(
    TypeId ty,
    TableType::Props tblProps,
    std::optional<TableIndexer> tblIndexer,
    DenseHashSet<TypeId>& result,
    NotNull<TypeFunctionContext> ctx
)
{
    ty = follow(ty);

    // index into tbl's properties
    if (auto stringSingleton = get<StringSingleton>(get<SingletonType>(ty)))
    {
        if (tblProps.find(stringSingleton->value) != tblProps.end())
        {
            TypeId propTy = follow(tblProps.at(stringSingleton->value).type());

            // property is a union type -> we need to extend our reduction type
            if (auto propUnionTy = get<UnionType>(propTy))
            {
                for (TypeId option : propUnionTy->options)
                    result.insert(option);
            }
            else // property is a singular type or intersection type -> we can simply append
                result.insert(propTy);

            return true;
        }
    }

    // index into tbl's indexer
    if (tblIndexer)
    {
        if (isSubtype(ty, tblIndexer->indexType, ctx->scope, ctx->builtins, *ctx->ice))
        {
            TypeId idxResultTy = follow(tblIndexer->indexResultType);

            // indexResultType is a union type -> we need to extend our reduction type
            if (auto idxResUnionTy = get<UnionType>(idxResultTy))
            {
                for (TypeId option : idxResUnionTy->options)
                    result.insert(option);
            }
            else // indexResultType is a singular type or intersection type -> we can simply append
                result.insert(idxResultTy);

            return true;
        }
    }

    return false;
}

/* Handles recursion / metamethods of tables/classes
   `isRaw` parameter indicates whether or not we should follow __index metamethods
   returns false if property of `ty` could not be found */
bool tblIndexInto(TypeId indexer, TypeId indexee, DenseHashSet<TypeId>& result, NotNull<TypeFunctionContext> ctx, bool isRaw)
{
    indexer = follow(indexer);
    indexee = follow(indexee);

    // we have a table type to try indexing
    if (auto tableTy = get<TableType>(indexee))
    {
        return searchPropsAndIndexer(indexer, tableTy->props, tableTy->indexer, result, ctx);
    }

    // we have a metatable type to try indexing
    if (auto metatableTy = get<MetatableType>(indexee))
    {
        if (auto tableTy = get<TableType>(metatableTy->table))
        {

            // try finding all properties within the current scope of the table
            if (searchPropsAndIndexer(indexer, tableTy->props, tableTy->indexer, result, ctx))
                return true;
        }

        // if the code reached here, it means we weren't able to find all properties -> look into __index metamethod
        if (!isRaw)
        {
            // findMetatableEntry demands the ability to emit errors, so we must give it
            // the necessary state to do that, even if we intend to just eat the errors.
            ErrorVec dummy;
            std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, indexee, "__index", Location{});
            if (mmType)
                return tblIndexInto(indexer, *mmType, result, ctx, isRaw);
        }
    }

    return false;
}

/* Vocabulary note: indexee refers to the type that contains the properties,
                    indexer refers to the type that is used to access indexee
   Example:         index<Person, "name"> => `Person` is the indexee and `"name"` is the indexer */
TypeFunctionReductionResult<TypeId> indexFunctionImpl(
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx,
    bool isRaw
)
{
    TypeId indexeeTy = follow(typeParams.at(0));
    std::shared_ptr<const NormalizedType> indexeeNormTy = ctx->normalizer->normalize(indexeeTy);

    // if the indexee failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!indexeeNormTy)
        return {std::nullopt, false, {}, {}};

    // if we don't have either just tables or just classes, we've got nothing to index into
    if (indexeeNormTy->hasTables() == indexeeNormTy->hasClasses())
        return {std::nullopt, true, {}, {}};

    // we're trying to reject any type that has not normalized to a table/class or a union of tables/classes.
    if (indexeeNormTy->hasTops() || indexeeNormTy->hasBooleans() || indexeeNormTy->hasErrors() || indexeeNormTy->hasNils() ||
        indexeeNormTy->hasNumbers() || indexeeNormTy->hasStrings() || indexeeNormTy->hasThreads() || indexeeNormTy->hasBuffers() ||
        indexeeNormTy->hasFunctions() || indexeeNormTy->hasTyvars())
        return {std::nullopt, true, {}, {}};

    TypeId indexerTy = follow(typeParams.at(1));

    if (isPending(indexerTy, ctx->solver))
        return {std::nullopt, false, {indexerTy}, {}};

    std::shared_ptr<const NormalizedType> indexerNormTy = ctx->normalizer->normalize(indexerTy);

    // if the indexer failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!indexerNormTy)
        return {std::nullopt, false, {}, {}};

    // we're trying to reject any type that is not a string singleton or primitive (string, number, boolean, thread, nil, function, table, or buffer)
    if (indexerNormTy->hasTops() || indexerNormTy->hasErrors())
        return {std::nullopt, true, {}, {}};

    // indexer can be a union —> break them down into a vector
    const std::vector<TypeId>* typesToFind = nullptr;
    const std::vector<TypeId> singleType{indexerTy};
    if (auto unionTy = get<UnionType>(indexerTy))
        typesToFind = &unionTy->options;
    else
        typesToFind = &singleType;

    DenseHashSet<TypeId> properties{{}}; // vector of types that will be returned

    if (indexeeNormTy->hasClasses())
    {
        LUAU_ASSERT(!indexeeNormTy->hasTables());

        if (isRaw) // rawget should never reduce for classes (to match the behavior of the rawget global function)
            return {std::nullopt, true, {}, {}};

        // at least one class is guaranteed to be in the iterator by .hasClasses()
        for (auto classesIter = indexeeNormTy->classes.ordering.begin(); classesIter != indexeeNormTy->classes.ordering.end(); ++classesIter)
        {
            auto classTy = get<ClassType>(*classesIter);
            if (!classTy)
            {
                LUAU_ASSERT(false); // this should not be possible according to normalization's spec
                return {std::nullopt, true, {}, {}};
            }

            for (TypeId ty : *typesToFind)
            {
                // Search for all instances of indexer in class->props and class->indexer
                if (searchPropsAndIndexer(ty, classTy->props, classTy->indexer, properties, ctx))
                    continue; // Indexer was found in this class, so we can move on to the next

                auto parent = classTy->parent;
                bool foundInParent = false;
                while (parent && !foundInParent)
                {
                    auto parentClass = get<ClassType>(follow(*parent));
                    foundInParent = searchPropsAndIndexer(ty, parentClass->props, parentClass->indexer, properties, ctx);
                    parent = parentClass->parent;
                }

                // we move on to the next type if any of the parents we went through had the property.
                if (foundInParent)
                    continue;

                // If code reaches here,that means the property not found -> check in the metatable's __index

                // findMetatableEntry demands the ability to emit errors, so we must give it
                // the necessary state to do that, even if we intend to just eat the errors.
                ErrorVec dummy;
                std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, *classesIter, "__index", Location{});
                if (!mmType) // if a metatable does not exist, there is no where else to look
                    return {std::nullopt, true, {}, {}};

                if (!tblIndexInto(ty, *mmType, properties, ctx, isRaw)) // if indexer is not in the metatable, we fail to reduce
                    return {std::nullopt, true, {}, {}};
            }
        }
    }

    if (indexeeNormTy->hasTables())
    {
        LUAU_ASSERT(!indexeeNormTy->hasClasses());

        // at least one table is guaranteed to be in the iterator by .hasTables()
        for (auto tablesIter = indexeeNormTy->tables.begin(); tablesIter != indexeeNormTy->tables.end(); ++tablesIter)
        {
            for (TypeId ty : *typesToFind)
                if (!tblIndexInto(ty, *tablesIter, properties, ctx, isRaw))
                    return {std::nullopt, true, {}, {}};
        }
    }

    // Call `follow()` on each element to resolve all Bound types before returning
    std::transform(
        properties.begin(),
        properties.end(),
        properties.begin(),
        [](TypeId ty)
        {
            return follow(ty);
        }
    );

    // If the type being reduced to is a single type, no need to union
    if (properties.size() == 1)
        return {*properties.begin(), false, {}, {}};

    return {ctx->arena->addType(UnionType{std::vector<TypeId>(properties.begin(), properties.end())}), false, {}, {}};
}

TypeFunctionReductionResult<TypeId> indexTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("index type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return indexFunctionImpl(typeParams, packParams, ctx, /* isRaw */ false);
}

TypeFunctionReductionResult<TypeId> rawgetTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("rawget type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return indexFunctionImpl(typeParams, packParams, ctx, /* isRaw */ true);
}

BuiltinTypeFunctions::BuiltinTypeFunctions()
    : userFunc{"user", userDefinedTypeFunction}
    , notFunc{"not", notTypeFunction}
    , lenFunc{"len", lenTypeFunction}
    , unmFunc{"unm", unmTypeFunction}
    , addFunc{"add", addTypeFunction}
    , subFunc{"sub", subTypeFunction}
    , mulFunc{"mul", mulTypeFunction}
    , divFunc{"div", divTypeFunction}
    , idivFunc{"idiv", idivTypeFunction}
    , powFunc{"pow", powTypeFunction}
    , modFunc{"mod", modTypeFunction}
    , concatFunc{"concat", concatTypeFunction}
    , andFunc{"and", andTypeFunction}
    , orFunc{"or", orTypeFunction}
    , ltFunc{"lt", ltTypeFunction}
    , leFunc{"le", leTypeFunction}
    , eqFunc{"eq", eqTypeFunction}
    , refineFunc{"refine", refineTypeFunction}
    , singletonFunc{"singleton", singletonTypeFunction}
    , unionFunc{"union", unionTypeFunction}
    , intersectFunc{"intersect", intersectTypeFunction}
    , keyofFunc{"keyof", keyofTypeFunction}
    , rawkeyofFunc{"rawkeyof", rawkeyofTypeFunction}
    , indexFunc{"index", indexTypeFunction}
    , rawgetFunc{"rawget", rawgetTypeFunction}
{
}

void BuiltinTypeFunctions::addToScope(NotNull<TypeArena> arena, NotNull<Scope> scope) const
{
    // make a type function for a one-argument type function
    auto mkUnaryTypeFunction = [&](const TypeFunction* tf)
    {
        TypeId t = arena->addType(GenericType{"T"});
        GenericTypeDefinition genericT{t};

        return TypeFun{{genericT}, arena->addType(TypeFunctionInstanceType{NotNull{tf}, {t}, {}})};
    };

    // make a type function for a two-argument type function
    auto mkBinaryTypeFunction = [&](const TypeFunction* tf)
    {
        TypeId t = arena->addType(GenericType{"T"});
        TypeId u = arena->addType(GenericType{"U"});
        GenericTypeDefinition genericT{t};
        GenericTypeDefinition genericU{u, {t}};

        return TypeFun{{genericT, genericU}, arena->addType(TypeFunctionInstanceType{NotNull{tf}, {t, u}, {}})};
    };

    scope->exportedTypeBindings[lenFunc.name] = mkUnaryTypeFunction(&lenFunc);
    scope->exportedTypeBindings[unmFunc.name] = mkUnaryTypeFunction(&unmFunc);

    scope->exportedTypeBindings[addFunc.name] = mkBinaryTypeFunction(&addFunc);
    scope->exportedTypeBindings[subFunc.name] = mkBinaryTypeFunction(&subFunc);
    scope->exportedTypeBindings[mulFunc.name] = mkBinaryTypeFunction(&mulFunc);
    scope->exportedTypeBindings[divFunc.name] = mkBinaryTypeFunction(&divFunc);
    scope->exportedTypeBindings[idivFunc.name] = mkBinaryTypeFunction(&idivFunc);
    scope->exportedTypeBindings[powFunc.name] = mkBinaryTypeFunction(&powFunc);
    scope->exportedTypeBindings[modFunc.name] = mkBinaryTypeFunction(&modFunc);
    scope->exportedTypeBindings[concatFunc.name] = mkBinaryTypeFunction(&concatFunc);

    scope->exportedTypeBindings[ltFunc.name] = mkBinaryTypeFunction(&ltFunc);
    scope->exportedTypeBindings[leFunc.name] = mkBinaryTypeFunction(&leFunc);
    scope->exportedTypeBindings[eqFunc.name] = mkBinaryTypeFunction(&eqFunc);

    scope->exportedTypeBindings[keyofFunc.name] = mkUnaryTypeFunction(&keyofFunc);
    scope->exportedTypeBindings[rawkeyofFunc.name] = mkUnaryTypeFunction(&rawkeyofFunc);

    scope->exportedTypeBindings[indexFunc.name] = mkBinaryTypeFunction(&indexFunc);
    scope->exportedTypeBindings[rawgetFunc.name] = mkBinaryTypeFunction(&rawgetFunc);
}

const BuiltinTypeFunctions& builtinTypeFunctions()
{
    static std::unique_ptr<const BuiltinTypeFunctions> result = std::make_unique<BuiltinTypeFunctions>();

    return *result;
}

} // namespace Luau
