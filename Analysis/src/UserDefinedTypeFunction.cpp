// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/ApplyTypeFunction.h"
#include "Luau/BuiltinTypeFunctions.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/Normalize.h"
#include "Luau/StringUtils.h"
#include "Luau/TimeTrace.h"
#include "Luau/UserDefinedTypeFunction.h"
#include "Luau/VisitType.h"

#include "lua.h"
#include "lualib.h"

namespace Luau
{

namespace
{

template<typename T>
class ScopedAssign
{
public:
    ScopedAssign(T& target, const T& value)
        : target(&target)
        , oldValue(target)
    {
        target = value;
    }

    ~ScopedAssign()
    {
        *target = oldValue;
    }

private:
    T* target = nullptr;
    T oldValue;
};

} // namespace

struct FindUserTypeFunctionBlockers : TypeOnceVisitor
{
    NotNull<TypeFunctionContext> ctx;
    DenseHashSet<TypeId> blockingTypeMap{nullptr};
    std::vector<TypeId> blockingTypes;

    explicit FindUserTypeFunctionBlockers(NotNull<TypeFunctionContext> ctx)
        : TypeOnceVisitor("FindUserTypeFunctionBlockers", /* skipBoundTypes */ true)
        , ctx(ctx)
    {
    }

    bool visit(TypeId ty) override
    {
        if (isPending(ty, ctx->solver))
        {
            if (!blockingTypeMap.contains(ty))
            {
                blockingTypeMap.insert(ty);
                blockingTypes.push_back(ty);
            }
        }
        return true;
    }

    bool visit(TypePackId tp) override
    {
        return true;
    }

    bool visit(TypeId ty, const ExternType&) override
    {
        return false;
    }
};

static int evaluateTypeAliasCall(lua_State* L)
{
    TypeFun* tf = static_cast<TypeFun*>(lua_tolightuserdata(L, lua_upvalueindex(1)));

    TypeFunctionRuntime* runtime = getTypeFunctionRuntime(L);
    TypeFunctionRuntimeBuilderState* runtimeBuilder = runtime->runtimeBuilder;

    ApplyTypeFunction applyTypeFunction{runtimeBuilder->ctx->arena};

    int argumentCount = lua_gettop(L);
    std::vector<TypeId> rawTypeArguments;

    for (int i = 0; i < argumentCount; i++)
    {
        TypeFunctionTypeId tfty = getTypeUserData(L, i + 1);
        TypeId ty = deserialize(tfty, runtimeBuilder);

        if (!runtimeBuilder->errors.empty())
            luaL_error(L, "failed to deserialize type at argument %d", i + 1);

        rawTypeArguments.push_back(ty);
    }

    // Check if we have enough arguments, by typical typechecking rules
    size_t typesRequired = tf->typeParams.size();
    size_t packsRequired = tf->typePackParams.size();

    size_t typesProvided = rawTypeArguments.size() > typesRequired ? typesRequired : rawTypeArguments.size();
    size_t extraTypes = rawTypeArguments.size() > typesRequired ? rawTypeArguments.size() - typesRequired : 0;
    size_t packsProvided = 0;

    if (extraTypes != 0 && packsProvided == 0)
    {
        // Extra types are only collected into a pack if a pack is expected
        if (packsRequired != 0)
            packsProvided += 1;
        else
            typesProvided += extraTypes;
    }

    for (size_t i = typesProvided; i < typesRequired; ++i)
    {
        if (tf->typeParams[i].defaultValue)
            typesProvided += 1;
    }

    for (size_t i = packsProvided; i < packsRequired; ++i)
    {
        if (tf->typePackParams[i].defaultValue)
            packsProvided += 1;
    }

    if (extraTypes == 0 && packsProvided + 1 == packsRequired)
        packsProvided += 1;

    if (typesProvided != typesRequired || packsProvided != packsRequired)
        luaL_error(L, "not enough arguments to call");

    // Prepare final types and packs
    auto [types, packs] = saturateArguments(runtimeBuilder->ctx->arena, runtimeBuilder->ctx->builtins, *tf, rawTypeArguments, {});

    for (size_t i = 0; i < types.size(); ++i)
        applyTypeFunction.typeArguments[tf->typeParams[i].ty] = types[i];

    for (size_t i = 0; i < packs.size(); ++i)
        applyTypeFunction.typePackArguments[tf->typePackParams[i].tp] = packs[i];

    std::optional<TypeId> maybeInstantiated = applyTypeFunction.substitute(tf->type);

    if (!maybeInstantiated.has_value())
    {
        luaL_error(L, "failed to instantiate type alias");
        return 1;
    }

    TypeId target = follow(*maybeInstantiated);

    FunctionGraphReductionResult result = reduceTypeFunctions(target, Location{}, runtimeBuilder->ctx);

    if (!result.errors.empty())
        luaL_error(L, "failed to reduce type function with: %s", toString(result.errors.front()).c_str());

    TypeFunctionTypeId serializedTy = serialize(follow(target), runtimeBuilder);

    if (!runtimeBuilder->errors.empty())
        luaL_error(L, "%s", runtimeBuilder->errors.front().c_str());

    allocTypeUserData(L, serializedTy->type);
    return 1;
}

TypeFunctionReductionResult<TypeId> userDefinedTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    auto typeFunction = getMutable<TypeFunctionInstanceType>(instance);

    if (typeFunction->userFuncData.owner.expired())
    {
        ctx->ice->ice("user-defined type function module has expired");
        return {std::nullopt, Reduction::Erroneous, {}, {}};
    }

    if (!typeFunction->userFuncName || !typeFunction->userFuncData.definition)
    {
        ctx->ice->ice("all user-defined type functions must have an associated function definition");
        return {std::nullopt, Reduction::Erroneous, {}, {}};
    }

    // If type functions cannot be evaluated because of errors in the code, we do not generate any additional ones
    if (!ctx->typeFunctionRuntime->allowEvaluation || typeFunction->userFuncData.definition->hasErrors)
        return {ctx->builtins->errorType, Reduction::MaybeOk, {}, {}};

    FindUserTypeFunctionBlockers check{ctx};

    for (auto typeParam : typeParams)
        check.traverse(follow(typeParam));

    // Check that our environment doesn't depend on any type aliases that are blocked
    for (auto& [name, definition] : typeFunction->userFuncData.environmentAlias)
    {
        if (definition.first->typeParams.empty() && definition.first->typePackParams.empty())
            check.traverse(follow(definition.first->type));
    }

    if (!check.blockingTypes.empty())
        return {std::nullopt, Reduction::MaybeOk, check.blockingTypes, {}};

    // Ensure that whole type function environment is registered
    for (auto& [name, definition] : typeFunction->userFuncData.environmentFunction)
    {
        // Cannot evaluate if a potential dependency couldn't be parsed
        if (definition.first->hasErrors)
            return {ctx->builtins->errorType, Reduction::MaybeOk, {}, {}};

        if (std::optional<std::string> error = ctx->typeFunctionRuntime->registerFunction(definition.first))
        {
            // Failure to register at this point means that original definition had to error out and should not have been present in the
            // environment
            ctx->ice->ice("user-defined type function reference cannot be registered");
            return {std::nullopt, Reduction::Erroneous, {}, {}};
        }
    }

    AstName name = typeFunction->userFuncData.definition->name;

    lua_State* global = ctx->typeFunctionRuntime->state.get();

    if (global == nullptr)
        return {std::nullopt, Reduction::Erroneous, {}, {}, format("'%s' type function: cannot be evaluated in this context", name.value)};

    // Separate sandboxed thread for individual execution and private globals
    lua_State* L = lua_newthread(global);
    LuauTempThreadPopper popper(global);

    std::unique_ptr<TypeFunctionRuntimeBuilderState> runtimeBuilder = std::make_unique<TypeFunctionRuntimeBuilderState>(ctx);

    ScopedAssign setRuntimeBuilder(ctx->typeFunctionRuntime->runtimeBuilder, runtimeBuilder.get());
    ScopedAssign enableReduction(ctx->normalizer->sharedState->reentrantTypeReduction, false);

    // Build up the environment table of each function we have visible
    for (auto& [_, curr] : typeFunction->userFuncData.environmentFunction)
    {
        // Environment table has to be filled only once in the current execution context
        if (ctx->typeFunctionRuntime->initialized.find(curr.first))
            continue;
        ctx->typeFunctionRuntime->initialized.insert(curr.first);

        lua_pushlightuserdata(L, curr.first);
        lua_gettable(L, LUA_REGISTRYINDEX);

        if (!lua_isfunction(L, -1))
        {
            ctx->ice->ice("user-defined type function reference cannot be found in the registry");
            return {std::nullopt, Reduction::Erroneous, {}, {}};
        }

        // Build up the environment of the current function, where some might not be visible
        lua_getfenv(L, -1);
        lua_setreadonly(L, -1, false);

        for (auto& [name, definition] : typeFunction->userFuncData.environmentFunction)
        {
            // Filter visibility based on original scope depth
            if (definition.second >= curr.second)
            {
                lua_pushlightuserdata(L, definition.first);
                lua_gettable(L, LUA_REGISTRYINDEX);

                if (!lua_isfunction(L, -1))
                    break; // Don't have to report an error here, we will visit each function in outer loop

                lua_setfield(L, -2, name.c_str());
            }
        }

        for (auto& [name, definition] : typeFunction->userFuncData.environmentAlias)
        {
            // Filter visibility based on original scope depth
            if (definition.second >= curr.second)
            {
                if (definition.first->typeParams.empty() && definition.first->typePackParams.empty())
                {
                    TypeId ty = follow(definition.first->type);

                    // This is checked at the top of the function, and should still be true.
                    LUAU_ASSERT(!isPending(ty, ctx->solver));

                    TypeFunctionTypeId serializedTy = serialize(ty, runtimeBuilder.get());

                    // Only register aliases that are representable in type environment
                    if (runtimeBuilder->errors.empty())
                    {
                        allocTypeUserData(L, serializedTy->type);
                        lua_setfield(L, -2, name.c_str());
                    }
                }
                else
                {
                    lua_pushlightuserdata(L, definition.first);
                    lua_pushcclosure(L, evaluateTypeAliasCall, name.c_str(), 1);
                    lua_setfield(L, -2, name.c_str());
                }
            }
        }

        lua_setreadonly(L, -1, true);
        lua_pop(L, 2);
    }

    // Fetch the function we want to evaluate
    lua_pushlightuserdata(L, typeFunction->userFuncData.definition);
    lua_gettable(L, LUA_REGISTRYINDEX);

    if (!lua_isfunction(L, -1))
    {
        ctx->ice->ice("user-defined type function reference cannot be found in the registry");
        return {std::nullopt, Reduction::Erroneous, {}, {}};
    }

    resetTypeFunctionState(L);

    // Push serialized arguments onto the stack
    for (auto typeParam : typeParams)
    {
        TypeId ty = follow(typeParam);
        // This is checked at the top of the function, and should still be true.
        LUAU_ASSERT(!isPending(ty, ctx->solver));

        TypeFunctionTypeId serializedTy = serialize(ty, runtimeBuilder.get());
        // Check if there were any errors while serializing
        if (runtimeBuilder->errors.size() != 0)
            return {std::nullopt, Reduction::Erroneous, {}, {}, runtimeBuilder->errors.front()};

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

    ctx->typeFunctionRuntime->messages.clear();

    if (auto error = checkResultForError(L, name.value, lua_pcall(L, int(typeParams.size()), 1, 0)))
        return {std::nullopt, Reduction::Erroneous, {}, {}, std::move(error), ctx->typeFunctionRuntime->messages};

    // If the return value is not a type userdata, return with error message
    if (!isTypeUserData(L, 1))
    {
        return {
            std::nullopt,
            Reduction::Erroneous,
            {},
            {},
            format("'%s' type function: returned a non-type value", name.value),
            ctx->typeFunctionRuntime->messages
        };
    }

    TypeFunctionTypeId retTypeFunctionTypeId = getTypeUserData(L, 1);

    // No errors should be present here since we should've returned already if any were raised during serialization.
    LUAU_ASSERT(runtimeBuilder->errors.size() == 0);

    TypeId retTypeId = deserialize(retTypeFunctionTypeId, runtimeBuilder.get());

    // At least 1 error occurred while deserializing
    if (runtimeBuilder->errors.size() > 0)
        return {std::nullopt, Reduction::Erroneous, {}, {}, runtimeBuilder->errors.front(), ctx->typeFunctionRuntime->messages};

    return {retTypeId, Reduction::MaybeOk, {}, {}, std::nullopt, ctx->typeFunctionRuntime->messages};
}

} // namespace Luau
