// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/NonStrictTypeChecker.h"

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Common.h"
#include "Luau/Simplify.h"
#include "Luau/Type.h"
#include "Luau/Subtyping.h"
#include "Luau/Normalize.h"
#include "Luau/Error.h"
#include "Luau/TimeTrace.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeFunction.h"
#include "Luau/Def.h"
#include "Luau/ToString.h"
#include "Luau/TypeUtils.h"

#include <iterator>

LUAU_FASTFLAG(DebugLuauMagicTypes)

LUAU_FASTFLAGVARIABLE(LuauNewNonStrictMoreUnknownSymbols)
LUAU_FASTFLAGVARIABLE(LuauNewNonStrictNoErrorsPassingNever)
LUAU_FASTFLAGVARIABLE(LuauNewNonStrictSuppressesDynamicRequireErrors)
LUAU_FASTFLAG(LuauEmplaceNotPushBack)
LUAU_FASTFLAGVARIABLE(LuauUnreducedTypeFunctionsDontTriggerWarnings)

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
        if (FFlag::LuauEmplaceNotPushBack)
            stack.emplace_back(scope);
        else
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
    NotNull<Simplifier> simplifier;
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
        NotNull<Simplifier> simplifier,
        NotNull<TypeFunctionRuntime> typeFunctionRuntime,
        const NotNull<InternalErrorReporter> ice,
        NotNull<UnifierSharedState> unifierState,
        NotNull<const DataFlowGraph> dfg,
        NotNull<TypeCheckLimits> limits,
        Module* module
    )
        : builtinTypes(builtinTypes)
        , simplifier(simplifier)
        , typeFunctionRuntime(typeFunctionRuntime)
        , ice(ice)
        , arena(arena)
        , module(module)
        , normalizer{arena, builtinTypes, unifierState, SolverMode::New, /* cache inhabitance */ true}
        , subtyping{builtinTypes, arena, simplifier, NotNull(&normalizer), typeFunctionRuntime, ice}
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
            TypeId result = arena->freshType(builtinTypes, ftp->scope);
            TypePackId freeTail = arena->addTypePack(FreeTypePack{ftp->scope});

            TypePack* resultPack = emplaceTypePack<TypePack>(asMutable(pack));
            resultPack->head.assign(1, result);
            resultPack->tail = freeTail;

            return result;
        }
        else if (get<ErrorTypePack>(pack))
            return builtinTypes->errorType;
        else if (finite(pack) && size(pack) == 0)
            return builtinTypes->nilType; // `(f())` where `f()` returns no values is coerced into `nil`
        else
            ice->ice("flattenPack got a weird pack!");
    }


    TypeId checkForTypeFunctionInhabitance(TypeId instance, Location location)
    {
        if (noTypeFunctionErrors.find(instance))
            return instance;

        TypeFunctionContext context{arena, builtinTypes, stack.back(), simplifier, NotNull{&normalizer}, typeFunctionRuntime, ice, limits};
        ErrorVec errors = reduceTypeFunctions(instance, location, NotNull{&context}, true).errors;

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
        else if (auto s = stat->as<AstStatDeclareExternType>())
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
                {
                    ctx.remove(dfg->getDef(local));

                    visit(local->annotation);
                }
            }
            else
                ctx = NonStrictContext::disjunction(builtinTypes, arena, visit(stat), ctx);
        }
        return ctx;
    }

    NonStrictContext visit(AstStatIf* ifStatement)
    {
        NonStrictContext condB = visit(ifStatement->condition, ValueContext::RValue);
        NonStrictContext branchContext;

        if (FFlag::LuauNewNonStrictMoreUnknownSymbols)
        {
            NonStrictContext thenBody = visit(ifStatement->thenbody);
            if (ifStatement->elsebody)
            {
                NonStrictContext elseBody = visit(ifStatement->elsebody);
                branchContext = NonStrictContext::conjunction(builtinTypes, arena, thenBody, elseBody);
            }
        }
        else
        {
            // If there is no else branch, don't bother generating warnings for the then branch - we can't prove there is an error
            if (ifStatement->elsebody)
            {
                NonStrictContext thenBody = visit(ifStatement->thenbody);
                NonStrictContext elseBody = visit(ifStatement->elsebody);
                branchContext = NonStrictContext::conjunction(builtinTypes, arena, thenBody, elseBody);
            }
        }

        return NonStrictContext::disjunction(builtinTypes, arena, condB, branchContext);
    }

    NonStrictContext visit(AstStatWhile* whileStatement)
    {
        NonStrictContext condition = visit(whileStatement->condition, ValueContext::RValue);
        NonStrictContext body = visit(whileStatement->body);
        return NonStrictContext::disjunction(builtinTypes, arena, condition, body);
    }

    NonStrictContext visit(AstStatRepeat* repeatStatement)
    {
        NonStrictContext body = visit(repeatStatement->body);
        NonStrictContext condition = visit(repeatStatement->condition, ValueContext::RValue);
        return NonStrictContext::disjunction(builtinTypes, arena, body, condition);
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
        // TODO: this is believing existing code, but i'm not sure if this makes sense
        // for how the contexts are handled
        for (AstExpr* expr : returnStatement->list)
            visit(expr, ValueContext::RValue);

        return {};
    }

    NonStrictContext visit(AstStatExpr* expr)
    {
        return visit(expr->expr, ValueContext::RValue);
    }

    NonStrictContext visit(AstStatLocal* local)
    {
        for (AstExpr* rhs : local->values)
            visit(rhs, ValueContext::RValue);
        return {};
    }

    NonStrictContext visit(AstStatFor* forStatement)
    {
        visit(forStatement->var->annotation);

        // TODO: throwing out context based on same principle as existing code?
        if (forStatement->from)
            visit(forStatement->from, ValueContext::RValue);
        if (forStatement->to)
            visit(forStatement->to, ValueContext::RValue);
        if (forStatement->step)
            visit(forStatement->step, ValueContext::RValue);
        return visit(forStatement->body);
    }

    NonStrictContext visit(AstStatForIn* forInStatement)
    {
        for (auto var : forInStatement->vars)
            visit(var->annotation);

        for (AstExpr* rhs : forInStatement->values)
            visit(rhs, ValueContext::RValue);
        return visit(forInStatement->body);
    }

    NonStrictContext visit(AstStatAssign* assign)
    {
        for (AstExpr* lhs : assign->vars)
            visit(lhs, ValueContext::LValue);
        for (AstExpr* rhs : assign->values)
            visit(rhs, ValueContext::RValue);

        return {};
    }

    NonStrictContext visit(AstStatCompoundAssign* compoundAssign)
    {
        visit(compoundAssign->var, ValueContext::LValue);
        visit(compoundAssign->value, ValueContext::RValue);

        return {};
    }

    NonStrictContext visit(AstStatFunction* statFn)
    {
        return visit(statFn->func, ValueContext::RValue);
    }

    NonStrictContext visit(AstStatLocalFunction* localFn)
    {
        return visit(localFn->func, ValueContext::RValue);
    }

    NonStrictContext visit(AstStatTypeAlias* typeAlias)
    {
        visitGenerics(typeAlias->generics, typeAlias->genericPacks);
        visit(typeAlias->type);

        return {};
    }

    NonStrictContext visit(AstStatTypeFunction* typeFunc)
    {
        return {};
    }

    NonStrictContext visit(AstStatDeclareFunction* declFn)
    {
        visitGenerics(declFn->generics, declFn->genericPacks);
        visit(declFn->params);
        visit(declFn->retTypes);

        return {};
    }

    NonStrictContext visit(AstStatDeclareGlobal* declGlobal)
    {
        visit(declGlobal->type);

        return {};
    }

    NonStrictContext visit(AstStatDeclareExternType* declClass)
    {
        if (declClass->indexer)
        {
            visit(declClass->indexer->indexType);
            visit(declClass->indexer->resultType);
        }

        for (auto prop : declClass->props)
            visit(prop.ty);

        return {};
    }

    NonStrictContext visit(AstStatError* error)
    {
        for (AstStat* stat : error->statements)
            visit(stat);
        for (AstExpr* expr : error->expressions)
            visit(expr, ValueContext::RValue);

        return {};
    }

    NonStrictContext visit(AstExpr* expr, ValueContext context)
    {
        auto pusher = pushStack(expr);
        if (auto e = expr->as<AstExprGroup>())
            return visit(e, context);
        else if (auto e = expr->as<AstExprConstantNil>())
            return visit(e);
        else if (auto e = expr->as<AstExprConstantBool>())
            return visit(e);
        else if (auto e = expr->as<AstExprConstantNumber>())
            return visit(e);
        else if (auto e = expr->as<AstExprConstantString>())
            return visit(e);
        else if (auto e = expr->as<AstExprLocal>())
            return visit(e, context);
        else if (auto e = expr->as<AstExprGlobal>())
            return visit(e, context);
        else if (auto e = expr->as<AstExprVarargs>())
            return visit(e);
        else if (auto e = expr->as<AstExprCall>())
            return visit(e);
        else if (auto e = expr->as<AstExprIndexName>())
            return visit(e, context);
        else if (auto e = expr->as<AstExprIndexExpr>())
            return visit(e, context);
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

    NonStrictContext visit(AstExprGroup* group, ValueContext context)
    {
        return visit(group->expr, context);
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

    NonStrictContext visit(AstExprLocal* local, ValueContext context)
    {
        return {};
    }

    NonStrictContext visit(AstExprGlobal* global, ValueContext context)
    {
        // We don't file unknown symbols for LValues.
        if (context == ValueContext::LValue)
            return {};

        NotNull<Scope> scope = stack.back();
        if (!scope->lookup(global->name))
        {
            reportError(UnknownSymbol{global->name.value, UnknownSymbol::Binding}, global->location);
        }

        return {};
    }

    NonStrictContext visit(AstExprVarargs* varargs)
    {
        return {};
    }

    NonStrictContext visit(AstExprCall* call)
    {
        if (FFlag::LuauNewNonStrictMoreUnknownSymbols)
        {
            visit(call->func, ValueContext::RValue);
            for (auto arg : call->args)
                visit(arg, ValueContext::RValue);
        }

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
                reportError(CheckedFunctionIncorrectArgs{std::move(functionName), argTypes.size(), arguments.size()}, call->location);
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
                {
                    if (FFlag::LuauUnreducedTypeFunctionsDontTriggerWarnings)
                        reportError(CheckedFunctionCallError{argTypes[i], *runTimeFailureType, functionName, i}, arg->location);
                    else
                    {
                        if (FFlag::LuauNewNonStrictNoErrorsPassingNever)
                        {
                            if (!get<NeverType>(follow(*runTimeFailureType)))
                                reportError(CheckedFunctionCallError{argTypes[i], *runTimeFailureType, functionName, i}, arg->location);
                        }
                        else
                            reportError(CheckedFunctionCallError{argTypes[i], *runTimeFailureType, functionName, i}, arg->location);
                    }
                }
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
                    reportError(CheckedFunctionIncorrectArgs{std::move(functionName), argTypes.size(), arguments.size()}, call->location);
                    return fresh;
                }
            }
        }

        return fresh;
    }

    NonStrictContext visit(AstExprIndexName* indexName, ValueContext context)
    {
        return visit(indexName->expr, context);
    }

    NonStrictContext visit(AstExprIndexExpr* indexExpr, ValueContext context)
    {
        NonStrictContext expr = visit(indexExpr->expr, context);
        NonStrictContext index = visit(indexExpr->index, ValueContext::RValue);
        return NonStrictContext::disjunction(builtinTypes, arena, expr, index);
    }


    NonStrictContext visit(AstExprFunction* exprFn)
    {
        // TODO: should a function being used as an expression generate a context without the arguments?
        auto pusher = pushStack(exprFn);
        NonStrictContext remainder = visit(exprFn->body);
        for (AstLocal* local : exprFn->args)
        {
            if (std::optional<TypeId> ty = willRunTimeErrorFunctionDefinition(local, remainder))
            {
                const char* debugname = exprFn->debugname.value;
                reportError(NonStrictFunctionDefinitionError{debugname ? debugname : "", local->name.value, *ty}, local->location);
            }
            remainder.remove(dfg->getDef(local));

            visit(local->annotation);
        }

        visitGenerics(exprFn->generics, exprFn->genericPacks);

        visit(exprFn->returnAnnotation);

        if (exprFn->varargAnnotation)
            visit(exprFn->varargAnnotation);

        return remainder;
    }

    NonStrictContext visit(AstExprTable* table)
    {
        for (auto [_, key, value] : table->items)
        {
            if (key)
                visit(key, ValueContext::RValue);
            visit(value, ValueContext::RValue);
        }

        return {};
    }

    NonStrictContext visit(AstExprUnary* unary)
    {
        return visit(unary->expr, ValueContext::RValue);
    }

    NonStrictContext visit(AstExprBinary* binary)
    {
        NonStrictContext lhs = visit(binary->left, ValueContext::RValue);
        NonStrictContext rhs = visit(binary->right, ValueContext::RValue);
        return NonStrictContext::disjunction(builtinTypes, arena, lhs, rhs);
    }

    NonStrictContext visit(AstExprTypeAssertion* typeAssertion)
    {
        visit(typeAssertion->annotation);

        return visit(typeAssertion->expr, ValueContext::RValue);
    }

    NonStrictContext visit(AstExprIfElse* ifElse)
    {
        NonStrictContext condB = visit(ifElse->condition, ValueContext::RValue);
        NonStrictContext thenB = visit(ifElse->trueExpr, ValueContext::RValue);
        NonStrictContext elseB = visit(ifElse->falseExpr, ValueContext::RValue);
        return NonStrictContext::disjunction(builtinTypes, arena, condB, NonStrictContext::conjunction(builtinTypes, arena, thenB, elseB));
    }

    NonStrictContext visit(AstExprInterpString* interpString)
    {
        for (AstExpr* expr : interpString->expressions)
            visit(expr, ValueContext::RValue);

        return {};
    }

    NonStrictContext visit(AstExprError* error)
    {
        for (AstExpr* expr : error->expressions)
            visit(expr, ValueContext::RValue);

        return {};
    }

    void visit(AstType* ty)
    {
        // If this node is `nullptr`, early exit.
        if (!ty)
            return;

        if (auto t = ty->as<AstTypeReference>())
            return visit(t);
        else if (auto t = ty->as<AstTypeTable>())
            return visit(t);
        else if (auto t = ty->as<AstTypeFunction>())
            return visit(t);
        else if (auto t = ty->as<AstTypeTypeof>())
            return visit(t);
        else if (auto t = ty->as<AstTypeUnion>())
            return visit(t);
        else if (auto t = ty->as<AstTypeIntersection>())
            return visit(t);
        else if (auto t = ty->as<AstTypeGroup>())
            return visit(t->type);
    }

    void visit(AstTypeReference* ty)
    {
        if (FFlag::DebugLuauMagicTypes)
        {
            // No further validation is necessary in this case.
            if (ty->name == kLuauPrint)
                return;

            if (ty->name == kLuauForceConstraintSolvingIncomplete)
            {
                reportError(ConstraintSolvingIncompleteError{}, ty->location);
                return;
            }
        }

        if (FFlag::DebugLuauMagicTypes && (ty->name == kLuauPrint || ty->name == kLuauForceConstraintSolvingIncomplete))
            return;

        for (const AstTypeOrPack& param : ty->parameters)
        {
            if (param.type)
                visit(param.type);
            else
                visit(param.typePack);
        }

        Scope* scope = findInnermostScope(ty->location);
        LUAU_ASSERT(scope);

        std::optional<TypeFun> alias = ty->prefix ? scope->lookupImportedType(ty->prefix->value, ty->name.value) : scope->lookupType(ty->name.value);

        if (alias.has_value())
        {
            size_t typesRequired = alias->typeParams.size();
            size_t packsRequired = alias->typePackParams.size();

            bool hasDefaultTypes = std::any_of(
                alias->typeParams.begin(),
                alias->typeParams.end(),
                [](auto&& el)
                {
                    return el.defaultValue.has_value();
                }
            );

            bool hasDefaultPacks = std::any_of(
                alias->typePackParams.begin(),
                alias->typePackParams.end(),
                [](auto&& el)
                {
                    return el.defaultValue.has_value();
                }
            );

            if (!ty->hasParameterList)
            {
                if ((!alias->typeParams.empty() && !hasDefaultTypes) || (!alias->typePackParams.empty() && !hasDefaultPacks))
                    reportError(GenericError{"Type parameter list is required"}, ty->location);
            }

            size_t typesProvided = 0;
            size_t extraTypes = 0;
            size_t packsProvided = 0;

            for (const AstTypeOrPack& p : ty->parameters)
            {
                if (p.type)
                {
                    if (packsProvided != 0)
                    {
                        reportError(GenericError{"Type parameters must come before type pack parameters"}, ty->location);
                        continue;
                    }

                    if (typesProvided < typesRequired)
                        typesProvided += 1;
                    else
                        extraTypes += 1;
                }
                else if (p.typePack)
                {
                    std::optional<TypePackId> tp = lookupPackAnnotation(p.typePack);
                    if (!tp.has_value())
                        continue;

                    if (typesProvided < typesRequired && size(*tp) == 1 && finite(*tp) && first(*tp))
                        typesProvided += 1;
                    else
                        packsProvided += 1;
                }
            }

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
                if (alias->typeParams[i].defaultValue)
                    typesProvided += 1;
            }

            for (size_t i = packsProvided; i < packsRequired; ++i)
            {
                if (alias->typePackParams[i].defaultValue)
                    packsProvided += 1;
            }

            if (extraTypes == 0 && packsProvided + 1 == packsRequired)
                packsProvided += 1;


            if (typesProvided != typesRequired || packsProvided != packsRequired)
            {
                reportError(
                    IncorrectGenericParameterCount{
                        /* name */ ty->name.value,
                        /* typeFun */ *alias,
                        /* actualParameters */ typesProvided,
                        /* actualPackParameters */ packsProvided,
                    },
                    ty->location
                );
            }
        }
        else
        {
            if (scope->lookupPack(ty->name.value))
            {
                reportError(
                    SwappedGenericTypeParameter{
                        ty->name.value,
                        SwappedGenericTypeParameter::Kind::Type,
                    },
                    ty->location
                );
            }
            else
            {
                std::string symbol = "";
                if (ty->prefix)
                {
                    symbol += (*(ty->prefix)).value;
                    symbol += ".";
                }
                symbol += ty->name.value;

                reportError(UnknownSymbol{std::move(symbol), UnknownSymbol::Context::Type}, ty->location);
            }
        }
    }

    void visit(AstTypeTable* table)
    {
        if (table->indexer)
        {
            visit(table->indexer->indexType);
            visit(table->indexer->resultType);
        }

        for (auto prop : table->props)
            visit(prop.type);
    }

    void visit(AstTypeFunction* function)
    {
        visit(function->argTypes);
        visit(function->returnTypes);
    }

    void visit(AstTypeTypeof* typeOf)
    {
        visit(typeOf->expr, ValueContext::RValue);
    }

    void visit(AstTypeUnion* unionType)
    {
        for (auto typ : unionType->types)
            visit(typ);
    }

    void visit(AstTypeIntersection* intersectionType)
    {
        for (auto typ : intersectionType->types)
            visit(typ);
    }

    void visit(AstTypeList& list)
    {
        for (auto typ : list.types)
            visit(typ);
        if (list.tailType)
            visit(list.tailType);
    }

    void visit(AstTypePack* pack)
    {
        // If there is no pack node, early exit.
        if (!pack)
            return;

        if (auto p = pack->as<AstTypePackExplicit>())
            return visit(p);
        else if (auto p = pack->as<AstTypePackVariadic>())
            return visit(p);
        else if (auto p = pack->as<AstTypePackGeneric>())
            return visit(p);
    }

    void visit(AstTypePackExplicit* tp)
    {
        for (AstType* type : tp->typeList.types)
            visit(type);

        if (tp->typeList.tailType)
            visit(tp->typeList.tailType);
    }

    void visit(AstTypePackVariadic* tp)
    {
        visit(tp->variadicType);
    }

    void visit(AstTypePackGeneric* tp)
    {
        Scope* scope = findInnermostScope(tp->location);
        LUAU_ASSERT(scope);

        if (std::optional<TypePackId> alias = scope->lookupPack(tp->genericName.value))
            return;

        if (scope->lookupType(tp->genericName.value))
            return reportError(
                SwappedGenericTypeParameter{
                    tp->genericName.value,
                    SwappedGenericTypeParameter::Kind::Pack,
                },
                tp->location
            );

        reportError(UnknownSymbol{tp->genericName.value, UnknownSymbol::Context::Type}, tp->location);
    }

    void visitGenerics(AstArray<AstGenericType*> generics, AstArray<AstGenericTypePack*> genericPacks)
    {
        DenseHashSet<AstName> seen{AstName{}};

        for (const auto* g : generics)
        {
            if (seen.contains(g->name))
                reportError(DuplicateGenericParameter{g->name.value}, g->location);
            else
                seen.insert(g->name);

            if (g->defaultValue)
                visit(g->defaultValue);
        }

        for (const auto* g : genericPacks)
        {
            if (seen.contains(g->name))
                reportError(DuplicateGenericParameter{g->name.value}, g->location);
            else
                seen.insert(g->name);

            if (g->defaultValue)
                visit(g->defaultValue);
        }
    }

    Scope* findInnermostScope(Location location) const
    {
        Scope* bestScope = module->getModuleScope().get();

        bool didNarrow;
        do
        {
            didNarrow = false;
            for (auto scope : bestScope->children)
            {
                if (scope->location.encloses(location))
                {
                    bestScope = scope.get();
                    didNarrow = true;
                    break;
                }
            }
        } while (didNarrow && bestScope->children.size() > 0);

        return bestScope;
    }

    std::optional<TypePackId> lookupPackAnnotation(AstTypePack* annotation) const
    {
        TypePackId* tp = module->astResolvedTypePacks.find(annotation);
        if (tp != nullptr)
            return {follow(*tp)};
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
                if (FFlag::LuauUnreducedTypeFunctionsDontTriggerWarnings && shouldSkipRuntimeErrorTesting(actualType))
                    continue;
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

    bool shouldSkipRuntimeErrorTesting(TypeId test)
    {
        TypeId t = follow(test);
        return is<NeverType, TypeFunctionInstanceType>(t);
    }
};

void checkNonStrict(
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Simplifier> simplifier,
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

    NonStrictTypeChecker typeChecker{
        NotNull{&module->internalTypes}, builtinTypes, simplifier, typeFunctionRuntime, ice, unifierState, dfg, limits, module
    };
    typeChecker.visit(sourceModule.root);
    unfreeze(module->interfaceTypes);
    copyErrors(module->errors, module->interfaceTypes, builtinTypes);

    if (FFlag::LuauNewNonStrictSuppressesDynamicRequireErrors)
    {
        module->errors.erase(
            std::remove_if(
                module->errors.begin(),
                module->errors.end(),
                [](auto err)
                {
                    return get<UnknownRequire>(err) != nullptr;
                }
            ),
            module->errors.end()
        );
    }

    freeze(module->interfaceTypes);
}

} // namespace Luau
