// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/NonStrictTypeChecker.h"

#include "Luau/Ast.h"
#include "Luau/Common.h"
#include "Luau/Simplify.h"
#include "Luau/Type.h"
#include "Luau/Simplify.h"
#include "Luau/Subtyping.h"
#include "Luau/Normalize.h"
#include "Luau/Error.h"
#include "Luau/TimeTrace.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeFunction.h"
#include "Luau/Def.h"
#include "Luau/ToString.h"
#include "Luau/TypeFwd.h"

#include <iostream>
#include <iterator>

LUAU_FASTFLAGVARIABLE(LuauUserTypeFunNonstrict)
LUAU_FASTFLAGVARIABLE(LuauCountSelfCallsNonstrict)

namespace Luau
{

/* Push a scope onto the end of a stack for the lifetime of the StackPusher instance.
 * NonStrictTypeChecker uses this to maintain knowledge about which scope encloses every
 * given AstNode.
 */
struct StackPusher
{
    std::vector<NotNull<Scope>>* stack;
    NotNull<Scope> scope;

    explicit StackPusher(std::vector<NotNull<Scope>>& stack, Scope* scope)
        : stack(&stack)
        , scope(scope)
    {
        stack.push_back(NotNull{scope});
    }

    ~StackPusher()
    {
        if (stack)
        {
            LUAU_ASSERT(stack->back() == scope);
            stack->pop_back();
        }
    }

    StackPusher(const StackPusher&) = delete;
    StackPusher&& operator=(const StackPusher&) = delete;

    StackPusher(StackPusher&& other)
        : stack(std::exchange(other.stack, nullptr))
        , scope(other.scope)
    {
    }
};


struct NonStrictContext
{
    NonStrictContext() = default;

    NonStrictContext(const NonStrictContext&) = delete;
    NonStrictContext& operator=(const NonStrictContext&) = delete;

    NonStrictContext(NonStrictContext&&) = default;
    NonStrictContext& operator=(NonStrictContext&&) = default;

    static NonStrictContext disjunction(
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<TypeArena> arena,
        const NonStrictContext& left,
        const NonStrictContext& right
    )
    {
        // disjunction implements union over the domain of keys
        // if the default value for a defId not in the map is `never`
        // then never | T is T
        NonStrictContext disj{};

        for (auto [def, leftTy] : left.context)
        {
            if (std::optional<TypeId> rightTy = right.find(def))
                disj.context[def] = simplifyUnion(builtinTypes, arena, leftTy, *rightTy).result;
            else
                disj.context[def] = leftTy;
        }

        for (auto [def, rightTy] : right.context)
        {
            if (!left.find(def).has_value())
                disj.context[def] = rightTy;
        }

        return disj;
    }

    static NonStrictContext conjunction(
        NotNull<BuiltinTypes> builtins,
        NotNull<TypeArena> arena,
        const NonStrictContext& left,
        const NonStrictContext& right
    )
    {
        NonStrictContext conj{};

        for (auto [def, leftTy] : left.context)
        {
            if (std::optional<TypeId> rightTy = right.find(def))
                conj.context[def] = simplifyIntersection(builtins, arena, leftTy, *rightTy).result;
        }

        return conj;
    }

    // Returns true if the removal was successful
    bool remove(const DefId& def)
    {
        std::vector<DefId> defs;
        collectOperands(def, &defs);
        bool result = true;
        for (DefId def : defs)
            result = result && context.erase(def.get()) == 1;
        return result;
    }

    std::optional<TypeId> find(const DefId& def) const
    {
        const Def* d = def.get();
        return find(d);
    }

    void addContext(const DefId& def, TypeId ty)
    {
        std::vector<DefId> defs;
        collectOperands(def, &defs);
        for (DefId def : defs)
            context[def.get()] = ty;
    }

private:
    std::optional<TypeId> find(const Def* d) const
    {
        auto it = context.find(d);
        if (it != context.end())
            return {it->second};
        return {};
    }

    std::unordered_map<const Def*, TypeId> context;
};

struct NonStrictTypeChecker
{
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<TypeFunctionRuntime> typeFunctionRuntime;
    const NotNull<InternalErrorReporter> ice;
    NotNull<TypeArena> arena;
    Module* module;
    Normalizer normalizer;
    Subtyping subtyping;
    NotNull<const DataFlowGraph> dfg;
    DenseHashSet<TypeId> noTypeFunctionErrors{nullptr};
    std::vector<NotNull<Scope>> stack;
    DenseHashMap<TypeId, TypeId> cachedNegations{nullptr};

    const NotNull<TypeCheckLimits> limits;

    NonStrictTypeChecker(
        NotNull<TypeArena> arena,
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<TypeFunctionRuntime> typeFunctionRuntime,
        const NotNull<InternalErrorReporter> ice,
        NotNull<UnifierSharedState> unifierState,
        NotNull<const DataFlowGraph> dfg,
        NotNull<TypeCheckLimits> limits,
        Module* module
    )
        : builtinTypes(builtinTypes)
        , typeFunctionRuntime(typeFunctionRuntime)
        , ice(ice)
        , arena(arena)
        , module(module)
        , normalizer{arena, builtinTypes, unifierState, /* cache inhabitance */ true}
        , subtyping{builtinTypes, arena, NotNull(&normalizer), typeFunctionRuntime, ice}
        , dfg(dfg)
        , limits(limits)
    {
    }

    std::optional<StackPusher> pushStack(AstNode* node)
    {
        if (Scope** scope = module->astScopes.find(node))
            return StackPusher{stack, *scope};
        else
            return std::nullopt;
    }

    TypeId flattenPack(TypePackId pack)
    {
        pack = follow(pack);

        if (auto fst = first(pack, /*ignoreHiddenVariadics*/ false))
            return *fst;
        else if (auto ftp = get<FreeTypePack>(pack))
        {
            TypeId result = arena->addType(FreeType{ftp->scope});
            TypePackId freeTail = arena->addTypePack(FreeTypePack{ftp->scope});

            TypePack* resultPack = emplaceTypePack<TypePack>(asMutable(pack));
            resultPack->head.assign(1, result);
            resultPack->tail = freeTail;

            return result;
        }
        else if (get<ErrorTypePack>(pack))
            return builtinTypes->errorRecoveryType();
        else if (finite(pack) && size(pack) == 0)
            return builtinTypes->nilType; // `(f())` where `f()` returns no values is coerced into `nil`
        else
            ice->ice("flattenPack got a weird pack!");
    }


    TypeId checkForTypeFunctionInhabitance(TypeId instance, Location location)
    {
        if (noTypeFunctionErrors.find(instance))
            return instance;

        ErrorVec errors = reduceTypeFunctions(
                              instance,
                              location,
                              TypeFunctionContext{arena, builtinTypes, stack.back(), NotNull{&normalizer}, typeFunctionRuntime, ice, limits},
                              true
        )
                              .errors;

        if (errors.empty())
            noTypeFunctionErrors.insert(instance);
        // TODO??
        // if (!isErrorSuppressing(location, instance))
        //     reportErrors(std::move(errors));
        return instance;
    }


    TypeId lookupType(AstExpr* expr)
    {
        TypeId* ty = module->astTypes.find(expr);
        if (ty)
            return checkForTypeFunctionInhabitance(follow(*ty), expr->location);

        TypePackId* tp = module->astTypePacks.find(expr);
        if (tp)
            return checkForTypeFunctionInhabitance(flattenPack(*tp), expr->location);
        return builtinTypes->anyType;
    }

    NonStrictContext visit(AstStat* stat)
    {
        auto pusher = pushStack(stat);
        if (auto s = stat->as<AstStatBlock>())
            return visit(s);
        else if (auto s = stat->as<AstStatIf>())
            return visit(s);
        else if (auto s = stat->as<AstStatWhile>())
            return visit(s);
        else if (auto s = stat->as<AstStatRepeat>())
            return visit(s);
        else if (auto s = stat->as<AstStatBreak>())
            return visit(s);
        else if (auto s = stat->as<AstStatContinue>())
            return visit(s);
        else if (auto s = stat->as<AstStatReturn>())
            return visit(s);
        else if (auto s = stat->as<AstStatExpr>())
            return visit(s);
        else if (auto s = stat->as<AstStatLocal>())
            return visit(s);
        else if (auto s = stat->as<AstStatFor>())
            return visit(s);
        else if (auto s = stat->as<AstStatForIn>())
            return visit(s);
        else if (auto s = stat->as<AstStatAssign>())
            return visit(s);
        else if (auto s = stat->as<AstStatCompoundAssign>())
            return visit(s);
        else if (auto s = stat->as<AstStatFunction>())
            return visit(s);
        else if (auto s = stat->as<AstStatLocalFunction>())
            return visit(s);
        else if (auto s = stat->as<AstStatTypeAlias>())
            return visit(s);
        else if (auto f = stat->as<AstStatTypeFunction>())
            return visit(f);
        else if (auto s = stat->as<AstStatDeclareFunction>())
            return visit(s);
        else if (auto s = stat->as<AstStatDeclareGlobal>())
            return visit(s);
        else if (auto s = stat->as<AstStatDeclareClass>())
            return visit(s);
        else if (auto s = stat->as<AstStatError>())
            return visit(s);
        else
        {
            LUAU_ASSERT(!"NonStrictTypeChecker encountered an unknown statement type");
            ice->ice("NonStrictTypeChecker encountered an unknown statement type");
        }
    }

    NonStrictContext visit(AstStatBlock* block)
    {
        auto StackPusher = pushStack(block);
        NonStrictContext ctx;


        for (auto it = block->body.rbegin(); it != block->body.rend(); it++)
        {
            AstStat* stat = *it;
            if (AstStatLocal* local = stat->as<AstStatLocal>())
            {
                // Iterating in reverse order
                // local x ; B generates the context of B without x
                visit(local);
                for (auto local : local->vars)
                    ctx.remove(dfg->getDef(local));
            }
            else
                ctx = NonStrictContext::disjunction(builtinTypes, arena, visit(stat), ctx);
        }
        return ctx;
    }

    NonStrictContext visit(AstStatIf* ifStatement)
    {
        NonStrictContext condB = visit(ifStatement->condition);
        NonStrictContext branchContext;
        // If there is no else branch, don't bother generating warnings for the then branch - we can't prove there is an error
        if (ifStatement->elsebody)
        {
            NonStrictContext thenBody = visit(ifStatement->thenbody);
            NonStrictContext elseBody = visit(ifStatement->elsebody);
            branchContext = NonStrictContext::conjunction(builtinTypes, arena, thenBody, elseBody);
        }
        return NonStrictContext::disjunction(builtinTypes, arena, condB, branchContext);
    }

    NonStrictContext visit(AstStatWhile* whileStatement)
    {
        return {};
    }

    NonStrictContext visit(AstStatRepeat* repeatStatement)
    {
        return {};
    }

    NonStrictContext visit(AstStatBreak* breakStatement)
    {
        return {};
    }

    NonStrictContext visit(AstStatContinue* continueStatement)
    {
        return {};
    }

    NonStrictContext visit(AstStatReturn* returnStatement)
    {
        return {};
    }

    NonStrictContext visit(AstStatExpr* expr)
    {
        return visit(expr->expr);
    }

    NonStrictContext visit(AstStatLocal* local)
    {
        for (AstExpr* rhs : local->values)
            visit(rhs);
        return {};
    }

    NonStrictContext visit(AstStatFor* forStatement)
    {
        return {};
    }

    NonStrictContext visit(AstStatForIn* forInStatement)
    {
        return {};
    }

    NonStrictContext visit(AstStatAssign* assign)
    {
        return {};
    }

    NonStrictContext visit(AstStatCompoundAssign* compoundAssign)
    {
        return {};
    }

    NonStrictContext visit(AstStatFunction* statFn)
    {
        return visit(statFn->func);
    }

    NonStrictContext visit(AstStatLocalFunction* localFn)
    {
        return visit(localFn->func);
    }

    NonStrictContext visit(AstStatTypeAlias* typeAlias)
    {
        return {};
    }

    NonStrictContext visit(AstStatTypeFunction* typeFunc)
    {
        if (!FFlag::LuauUserTypeFunNonstrict)
            reportError(GenericError{"This syntax is not supported"}, typeFunc->location);

        return {};
    }

    NonStrictContext visit(AstStatDeclareFunction* declFn)
    {
        return {};
    }

    NonStrictContext visit(AstStatDeclareGlobal* declGlobal)
    {
        return {};
    }

    NonStrictContext visit(AstStatDeclareClass* declClass)
    {
        return {};
    }

    NonStrictContext visit(AstStatError* error)
    {
        return {};
    }

    NonStrictContext visit(AstExpr* expr)
    {
        auto pusher = pushStack(expr);
        if (auto e = expr->as<AstExprGroup>())
            return visit(e);
        else if (auto e = expr->as<AstExprConstantNil>())
            return visit(e);
        else if (auto e = expr->as<AstExprConstantBool>())
            return visit(e);
        else if (auto e = expr->as<AstExprConstantNumber>())
            return visit(e);
        else if (auto e = expr->as<AstExprConstantString>())
            return visit(e);
        else if (auto e = expr->as<AstExprLocal>())
            return visit(e);
        else if (auto e = expr->as<AstExprGlobal>())
            return visit(e);
        else if (auto e = expr->as<AstExprVarargs>())
            return visit(e);
        else if (auto e = expr->as<AstExprCall>())
            return visit(e);
        else if (auto e = expr->as<AstExprIndexName>())
            return visit(e);
        else if (auto e = expr->as<AstExprIndexExpr>())
            return visit(e);
        else if (auto e = expr->as<AstExprFunction>())
            return visit(e);
        else if (auto e = expr->as<AstExprTable>())
            return visit(e);
        else if (auto e = expr->as<AstExprUnary>())
            return visit(e);
        else if (auto e = expr->as<AstExprBinary>())
            return visit(e);
        else if (auto e = expr->as<AstExprTypeAssertion>())
            return visit(e);
        else if (auto e = expr->as<AstExprIfElse>())
            return visit(e);
        else if (auto e = expr->as<AstExprInterpString>())
            return visit(e);
        else if (auto e = expr->as<AstExprError>())
            return visit(e);
        else
        {
            LUAU_ASSERT(!"NonStrictTypeChecker encountered an unknown expression type");
            ice->ice("NonStrictTypeChecker encountered an unknown expression type");
        }
    }

    NonStrictContext visit(AstExprGroup* group)
    {
        return {};
    }

    NonStrictContext visit(AstExprConstantNil* expr)
    {
        return {};
    }

    NonStrictContext visit(AstExprConstantBool* expr)
    {
        return {};
    }

    NonStrictContext visit(AstExprConstantNumber* expr)
    {
        return {};
    }

    NonStrictContext visit(AstExprConstantString* expr)
    {
        return {};
    }

    NonStrictContext visit(AstExprLocal* local)
    {
        return {};
    }

    NonStrictContext visit(AstExprGlobal* global)
    {
        return {};
    }

    NonStrictContext visit(AstExprVarargs* global)
    {
        return {};
    }

    NonStrictContext visit(AstExprCall* call)
    {
        if (FFlag::LuauCountSelfCallsNonstrict)
            return visitCall(call);
        else
            return visitCall_DEPRECATED(call);
    }

    // rename this to `visit` when `FFlag::LuauCountSelfCallsNonstrict` is removed, and clean up above `visit`.
    NonStrictContext visitCall(AstExprCall* call)
    {
        LUAU_ASSERT(FFlag::LuauCountSelfCallsNonstrict);

        NonStrictContext fresh{};
        TypeId* originalCallTy = module->astOriginalCallTypes.find(call->func);
        if (!originalCallTy)
            return fresh;

        TypeId fnTy = *originalCallTy;
        if (auto fn = get<FunctionType>(follow(fnTy)); fn && fn->isCheckedFunction)
        {
            // We know fn is a checked function, which means it looks like:
            // (S1, ... SN) -> T &
            // (~S1, unknown^N-1) -> error &
            // (unknown, ~S2, unknown^N-2) -> error
            // ...
            // ...
            // (unknown^N-1, ~S_N) -> error

            std::vector<AstExpr*> arguments;
            arguments.reserve(call->args.size + (call->self ? 1 : 0));
            if (call->self)
            {
                if (auto indexExpr = call->func->as<AstExprIndexName>())
                    arguments.push_back(indexExpr->expr);
                else
                    ice->ice("method call expression has no 'self'");
            }
            arguments.insert(arguments.end(), call->args.begin(), call->args.end());

            std::vector<TypeId> argTypes;
            argTypes.reserve(arguments.size());

            // Move all the types over from the argument typepack for `fn`
            TypePackIterator curr = begin(fn->argTypes);
            TypePackIterator fin = end(fn->argTypes);
            for (; curr != fin; curr++)
                argTypes.push_back(*curr);

            // Pad out the rest with the variadic as needed.
            if (auto argTail = curr.tail())
            {
                if (const VariadicTypePack* vtp = get<VariadicTypePack>(follow(*argTail)))
                {
                    while (argTypes.size() < arguments.size())
                    {
                        argTypes.push_back(vtp->ty);
                    }
                }
            }

            std::string functionName = getFunctionNameAsString(*call->func).value_or("");
            if (arguments.size() > argTypes.size())
            {
                // We are passing more arguments than we expect, so we should error
                reportError(CheckedFunctionIncorrectArgs{functionName, argTypes.size(), arguments.size()}, call->location);
                return fresh;
            }

            for (size_t i = 0; i < arguments.size(); i++)
            {
                // For example, if the arg is "hi"
                // The actual arg type is string
                // The expected arg type is number
                // The type of the argument in the overload is ~number
                // We will compare arg and ~number
                AstExpr* arg = arguments[i];
                TypeId expectedArgType = argTypes[i];
                std::shared_ptr<const NormalizedType> norm = normalizer.normalize(expectedArgType);
                DefId def = dfg->getDef(arg);
                TypeId runTimeErrorTy;
                // If we're dealing with any, negating any will cause all subtype tests to fail
                // However, when someone calls this function, they're going to want to be able to pass it anything,
                // for that reason, we manually inject never into the context so that the runtime test will always pass.
                if (!norm)
                    reportError(NormalizationTooComplex{}, arg->location);

                if (norm && get<AnyType>(norm->tops))
                    runTimeErrorTy = builtinTypes->neverType;
                else
                    runTimeErrorTy = getOrCreateNegation(expectedArgType);
                fresh.addContext(def, runTimeErrorTy);
            }

            // Populate the context and now iterate through each of the arguments to the call to find out if we satisfy the types
            for (size_t i = 0; i < arguments.size(); i++)
            {
                AstExpr* arg = arguments[i];
                if (auto runTimeFailureType = willRunTimeError(arg, fresh))
                    reportError(CheckedFunctionCallError{argTypes[i], *runTimeFailureType, functionName, i}, arg->location);
            }

            if (arguments.size() < argTypes.size())
            {
                // We are passing fewer arguments than we expect
                // so we need to ensure that the rest of the args are optional.
                bool remainingArgsOptional = true;
                for (size_t i = arguments.size(); i < argTypes.size(); i++)
                    remainingArgsOptional = remainingArgsOptional && isOptional(argTypes[i]);

                if (!remainingArgsOptional)
                {
                    reportError(CheckedFunctionIncorrectArgs{functionName, argTypes.size(), arguments.size()}, call->location);
                    return fresh;
                }
            }
        }

        return fresh;
    }

    // Remove with `FFlag::LuauCountSelfCallsNonstrict` clean up.
    NonStrictContext visitCall_DEPRECATED(AstExprCall* call)
    {
        LUAU_ASSERT(!FFlag::LuauCountSelfCallsNonstrict);

        NonStrictContext fresh{};
        TypeId* originalCallTy = module->astOriginalCallTypes.find(call->func);
        if (!originalCallTy)
            return fresh;

        TypeId fnTy = *originalCallTy;
        if (auto fn = get<FunctionType>(follow(fnTy)))
        {
            if (fn->isCheckedFunction)
            {
                // We know fn is a checked function, which means it looks like:
                // (S1, ... SN) -> T &
                // (~S1, unknown^N-1) -> error &
                // (unknown, ~S2, unknown^N-2) -> error
                // ...
                // ...
                // (unknown^N-1, ~S_N) -> error
                std::vector<TypeId> argTypes;
                argTypes.reserve(call->args.size);
                // Pad out the arg types array with the types you would expect to see
                TypePackIterator curr = begin(fn->argTypes);
                TypePackIterator fin = end(fn->argTypes);
                while (curr != fin)
                {
                    argTypes.push_back(*curr);
                    ++curr;
                }
                if (auto argTail = curr.tail())
                {
                    if (const VariadicTypePack* vtp = get<VariadicTypePack>(follow(*argTail)))
                    {
                        while (argTypes.size() < call->args.size)
                        {
                            argTypes.push_back(vtp->ty);
                        }
                    }
                }

                std::string functionName = getFunctionNameAsString(*call->func).value_or("");
                if (call->args.size > argTypes.size())
                {
                    // We are passing more arguments than we expect, so we should error
                    reportError(CheckedFunctionIncorrectArgs{functionName, argTypes.size(), call->args.size}, call->location);
                    return fresh;
                }

                for (size_t i = 0; i < call->args.size; i++)
                {
                    // For example, if the arg is "hi"
                    // The actual arg type is string
                    // The expected arg type is number
                    // The type of the argument in the overload is ~number
                    // We will compare arg and ~number
                    AstExpr* arg = call->args.data[i];
                    TypeId expectedArgType = argTypes[i];
                    std::shared_ptr<const NormalizedType> norm = normalizer.normalize(expectedArgType);
                    DefId def = dfg->getDef(arg);
                    TypeId runTimeErrorTy;
                    // If we're dealing with any, negating any will cause all subtype tests to fail
                    // However, when someone calls this function, they're going to want to be able to pass it anything,
                    // for that reason, we manually inject never into the context so that the runtime test will always pass.
                    if (!norm)
                        reportError(NormalizationTooComplex{}, arg->location);

                    if (norm && get<AnyType>(norm->tops))
                        runTimeErrorTy = builtinTypes->neverType;
                    else
                        runTimeErrorTy = getOrCreateNegation(expectedArgType);
                    fresh.addContext(def, runTimeErrorTy);
                }

                // Populate the context and now iterate through each of the arguments to the call to find out if we satisfy the types
                for (size_t i = 0; i < call->args.size; i++)
                {
                    AstExpr* arg = call->args.data[i];
                    if (auto runTimeFailureType = willRunTimeError(arg, fresh))
                        reportError(CheckedFunctionCallError{argTypes[i], *runTimeFailureType, functionName, i}, arg->location);
                }

                if (call->args.size < argTypes.size())
                {
                    // We are passing fewer arguments than we expect
                    // so we need to ensure that the rest of the args are optional.
                    bool remainingArgsOptional = true;
                    for (size_t i = call->args.size; i < argTypes.size(); i++)
                        remainingArgsOptional = remainingArgsOptional && isOptional(argTypes[i]);
                    if (!remainingArgsOptional)
                    {
                        reportError(CheckedFunctionIncorrectArgs{functionName, argTypes.size(), call->args.size}, call->location);
                        return fresh;
                    }
                }
            }
        }

        return fresh;
    }

    NonStrictContext visit(AstExprIndexName* indexName)
    {
        return {};
    }

    NonStrictContext visit(AstExprIndexExpr* indexExpr)
    {
        return {};
    }

    NonStrictContext visit(AstExprFunction* exprFn)
    {
        // TODO: should a function being used as an expression generate a context without the arguments?
        auto pusher = pushStack(exprFn);
        NonStrictContext remainder = visit(exprFn->body);
        for (AstLocal* local : exprFn->args)
        {
            if (std::optional<TypeId> ty = willRunTimeErrorFunctionDefinition(local, remainder))
                reportError(NonStrictFunctionDefinitionError{exprFn->debugname.value, local->name.value, *ty}, local->location);
            remainder.remove(dfg->getDef(local));
        }
        return remainder;
    }

    NonStrictContext visit(AstExprTable* table)
    {
        return {};
    }

    NonStrictContext visit(AstExprUnary* unary)
    {
        return {};
    }

    NonStrictContext visit(AstExprBinary* binary)
    {
        return {};
    }

    NonStrictContext visit(AstExprTypeAssertion* typeAssertion)
    {
        return {};
    }

    NonStrictContext visit(AstExprIfElse* ifElse)
    {
        NonStrictContext condB = visit(ifElse->condition);
        NonStrictContext thenB = visit(ifElse->trueExpr);
        NonStrictContext elseB = visit(ifElse->falseExpr);
        return NonStrictContext::disjunction(builtinTypes, arena, condB, NonStrictContext::conjunction(builtinTypes, arena, thenB, elseB));
    }

    NonStrictContext visit(AstExprInterpString* interpString)
    {
        return {};
    }

    NonStrictContext visit(AstExprError* error)
    {
        return {};
    }

    void reportError(TypeErrorData data, const Location& location)
    {
        module->errors.emplace_back(location, module->name, std::move(data));
        // TODO: weave in logger here?
    }

    // If this fragment of the ast will run time error, return the type that causes this
    std::optional<TypeId> willRunTimeError(AstExpr* fragment, const NonStrictContext& context)
    {
        NotNull<Scope> scope{Luau::findScopeAtPosition(*module, fragment->location.end).get()};
        DefId def = dfg->getDef(fragment);
        std::vector<DefId> defs;
        collectOperands(def, &defs);
        for (DefId def : defs)
        {
            if (std::optional<TypeId> contextTy = context.find(def))
            {

                TypeId actualType = lookupType(fragment);
                SubtypingResult r = subtyping.isSubtype(actualType, *contextTy, scope);
                if (r.normalizationTooComplex)
                    reportError(NormalizationTooComplex{}, fragment->location);
                if (r.isSubtype)
                    return {actualType};
            }
        }

        return {};
    }

    std::optional<TypeId> willRunTimeErrorFunctionDefinition(AstLocal* fragment, const NonStrictContext& context)
    {
        NotNull<Scope> scope{Luau::findScopeAtPosition(*module, fragment->location.end).get()};
        DefId def = dfg->getDef(fragment);
        std::vector<DefId> defs;
        collectOperands(def, &defs);
        for (DefId def : defs)
        {
            if (std::optional<TypeId> contextTy = context.find(def))
            {
                SubtypingResult r1 = subtyping.isSubtype(builtinTypes->unknownType, *contextTy, scope);
                SubtypingResult r2 = subtyping.isSubtype(*contextTy, builtinTypes->unknownType, scope);
                if (r1.normalizationTooComplex || r2.normalizationTooComplex)
                    reportError(NormalizationTooComplex{}, fragment->location);
                bool isUnknown = r1.isSubtype && r2.isSubtype;
                if (isUnknown)
                    return {builtinTypes->unknownType};
            }
        }
        return {};
    }

private:
    TypeId getOrCreateNegation(TypeId baseType)
    {
        TypeId& cachedResult = cachedNegations[baseType];
        if (!cachedResult)
            cachedResult = arena->addType(NegationType{baseType});
        return cachedResult;
    }
};

void checkNonStrict(
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    NotNull<InternalErrorReporter> ice,
    NotNull<UnifierSharedState> unifierState,
    NotNull<const DataFlowGraph> dfg,
    NotNull<TypeCheckLimits> limits,
    const SourceModule& sourceModule,
    Module* module
)
{
    LUAU_TIMETRACE_SCOPE("checkNonStrict", "Typechecking");

    NonStrictTypeChecker typeChecker{NotNull{&module->internalTypes}, builtinTypes, typeFunctionRuntime, ice, unifierState, dfg, limits, module};
    typeChecker.visit(sourceModule.root);
    unfreeze(module->interfaceTypes);
    copyErrors(module->errors, module->interfaceTypes, builtinTypes);
    freeze(module->interfaceTypes);
}

} // namespace Luau
