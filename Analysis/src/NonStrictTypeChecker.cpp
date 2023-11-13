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
#include "Luau/TypeArena.h"
#include "Luau/TypeFamily.h"
#include "Luau/Def.h"

#include <iostream>

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
    std::unordered_map<const Def*, TypeId> context;

    NonStrictContext() = default;

    NonStrictContext(const NonStrictContext&) = delete;
    NonStrictContext& operator=(const NonStrictContext&) = delete;

    NonStrictContext(NonStrictContext&&) = default;
    NonStrictContext& operator=(NonStrictContext&&) = default;

    static NonStrictContext disjunction(
        NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, const NonStrictContext& left, const NonStrictContext& right)
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
            if (!right.find(def).has_value())
                disj.context[def] = rightTy;
        }

        return disj;
    }

    static NonStrictContext conjunction(
        NotNull<BuiltinTypes> builtins, NotNull<TypeArena> arena, const NonStrictContext& left, const NonStrictContext& right)
    {
        NonStrictContext conj{};

        for (auto [def, leftTy] : left.context)
        {
            if (std::optional<TypeId> rightTy = right.find(def))
                conj.context[def] = simplifyIntersection(builtins, arena, leftTy, *rightTy).result;
        }

        return conj;
    }

    void removeFromContext(const std::vector<DefId>& defs)
    {
        // TODO: unimplemented
    }

    std::optional<TypeId> find(const DefId& def) const
    {
        const Def* d = def.get();
        return find(d);
    }

private:
    std::optional<TypeId> find(const Def* d) const
    {
        auto it = context.find(d);
        if (it != context.end())
            return {it->second};
        return {};
    }
};

struct NonStrictTypeChecker
{

    NotNull<BuiltinTypes> builtinTypes;
    const NotNull<InternalErrorReporter> ice;
    TypeArena arena;
    Module* module;
    Normalizer normalizer;
    Subtyping subtyping;
    NotNull<const DataFlowGraph> dfg;
    DenseHashSet<TypeId> noTypeFamilyErrors{nullptr};
    std::vector<NotNull<Scope>> stack;

    const NotNull<TypeCheckLimits> limits;

    NonStrictTypeChecker(NotNull<BuiltinTypes> builtinTypes, const NotNull<InternalErrorReporter> ice, NotNull<UnifierSharedState> unifierState,
        NotNull<const DataFlowGraph> dfg, NotNull<TypeCheckLimits> limits, Module* module)
        : builtinTypes(builtinTypes)
        , ice(ice)
        , module(module)
        , normalizer{&arena, builtinTypes, unifierState, /* cache inhabitance */ true}
        , subtyping{builtinTypes, NotNull{&arena}, NotNull(&normalizer), ice, NotNull{module->getModuleScope().get()}}
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
            TypeId result = arena.addType(FreeType{ftp->scope});
            TypePackId freeTail = arena.addTypePack(FreeTypePack{ftp->scope});

            TypePack& resultPack = asMutable(pack)->ty.emplace<TypePack>();
            resultPack.head.assign(1, result);
            resultPack.tail = freeTail;

            return result;
        }
        else if (get<Unifiable::Error>(pack))
            return builtinTypes->errorRecoveryType();
        else if (finite(pack) && size(pack) == 0)
            return builtinTypes->nilType; // `(f())` where `f()` returns no values is coerced into `nil`
        else
            ice->ice("flattenPack got a weird pack!");
    }


    TypeId checkForFamilyInhabitance(TypeId instance, Location location)
    {
        if (noTypeFamilyErrors.find(instance))
            return instance;

        ErrorVec errors = reduceFamilies(
            instance, location, TypeFamilyContext{NotNull{&arena}, builtinTypes, stack.back(), NotNull{&normalizer}, ice, limits}, true)
                              .errors;

        if (errors.empty())
            noTypeFamilyErrors.insert(instance);
        // TODO??
        // if (!isErrorSuppressing(location, instance))
        //     reportErrors(std::move(errors));
        return instance;
    }


    TypeId lookupType(AstExpr* expr)
    {
        TypeId* ty = module->astTypes.find(expr);
        if (ty)
            return checkForFamilyInhabitance(follow(*ty), expr->location);

        TypePackId* tp = module->astTypePacks.find(expr);
        if (tp)
            return checkForFamilyInhabitance(flattenPack(*tp), expr->location);
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
        for (AstStat* statement : block->body)
            visit(statement);
        return {};
    }

    NonStrictContext visit(AstStatIf* ifStatement)
    {
        NonStrictContext condB = visit(ifStatement->condition);
        NonStrictContext thenB = visit(ifStatement->thenbody);
        NonStrictContext elseB = visit(ifStatement->elsebody);
        return NonStrictContext::disjunction(
            builtinTypes, NotNull{&arena}, condB, NonStrictContext::conjunction(builtinTypes, NotNull{&arena}, thenB, elseB));
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
        return {};
    }

    NonStrictContext visit(AstStatLocalFunction* localFn)
    {
        return {};
    }

    NonStrictContext visit(AstStatTypeAlias* typeAlias)
    {
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
        NonStrictContext fresh{};
        TypeId* originalCallTy = module->astOriginalCallTypes.find(call);
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
                for (TypeId ty : fn->argTypes)
                    argTypes.push_back(ty);
                // For a checked function, these gotta be the same size
                LUAU_ASSERT(call->args.size == argTypes.size());
                for (size_t i = 0; i < call->args.size; i++)
                {
                    // For example, if the arg is "hi"
                    // The actual arg type is string
                    // The expected arg type is number
                    // The type of the argument in the overload is ~number
                    // We will compare arg and ~number
                    AstExpr* arg = call->args.data[i];
                    TypeId expectedArgType = argTypes[i];
                    DefId def = dfg->getDef(arg);
                    // TODO: Cache negations created here!!!
                    // See Jira Ticket: https://roblox.atlassian.net/browse/CLI-87539
                    TypeId runTimeErrorTy = arena.addType(NegationType{expectedArgType});
                    fresh.context[def.get()] = runTimeErrorTy;
                }

                // Populate the context and now iterate through each of the arguments to the call to find out if we satisfy the types
                AstName name = getIdentifier(call->func);
                for (size_t i = 0; i < call->args.size; i++)
                {
                    AstExpr* arg = call->args.data[i];
                    if (auto runTimeFailureType = willRunTimeError(arg, fresh))
                        reportError(CheckedFunctionCallError{argTypes[i], *runTimeFailureType, name.value, i}, arg->location);
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
        auto pusher = pushStack(exprFn);
        return {};
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
        return NonStrictContext::disjunction(
            builtinTypes, NotNull{&arena}, condB, NonStrictContext::conjunction(builtinTypes, NotNull{&arena}, thenB, elseB));
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
        DefId def = dfg->getDef(fragment);
        if (std::optional<TypeId> contextTy = context.find(def))
        {

            TypeId actualType = lookupType(fragment);
            SubtypingResult r = subtyping.isSubtype(actualType, *contextTy);
            if (r.normalizationTooComplex)
                reportError(NormalizationTooComplex{}, fragment->location);

            if (!r.isSubtype && !r.isErrorSuppressing)
                reportError(TypeMismatch{actualType, *contextTy}, fragment->location);

            if (r.isSubtype)
                return {actualType};
        }

        return {};
    }
};

void checkNonStrict(NotNull<BuiltinTypes> builtinTypes, NotNull<InternalErrorReporter> ice, NotNull<UnifierSharedState> unifierState,
    NotNull<const DataFlowGraph> dfg, NotNull<TypeCheckLimits> limits, const SourceModule& sourceModule, Module* module)
{
    // TODO: unimplemented
    NonStrictTypeChecker typeChecker{builtinTypes, ice, unifierState, dfg, limits, module};
    typeChecker.visit(sourceModule.root);
    unfreeze(module->interfaceTypes);
    copyErrors(module->errors, module->interfaceTypes, builtinTypes);
    freeze(module->interfaceTypes);
}

} // namespace Luau
