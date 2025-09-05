// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"

#include "Luau/ApplyTypeFunction.h"
#include "Luau/Cancellation.h"
#include "Luau/Common.h"
#include "Luau/Instantiation.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Normalize.h"
#include "Luau/Quantify.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/Substitution.h"
#include "Luau/TimeTrace.h"
#include "Luau/TopoSortStatements.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/VisitType.h"

#include <algorithm>
#include <iterator>

LUAU_FASTFLAGVARIABLE(DebugLuauMagicTypes)
LUAU_FASTINTVARIABLE(LuauTypeInferRecursionLimit, 165)
LUAU_FASTINTVARIABLE(LuauTypeInferIterationLimit, 20000)
LUAU_FASTINTVARIABLE(LuauTypeInferTypePackLoopLimit, 5000)
LUAU_FASTINTVARIABLE(LuauCheckRecursionLimit, 300)
LUAU_FASTINTVARIABLE(LuauVisitRecursionLimit, 500)
LUAU_FASTFLAG(LuauKnowsTheDataModel3)
LUAU_FASTFLAGVARIABLE(DebugLuauFreezeDuringUnification)
LUAU_FASTFLAG(LuauInstantiateInSubtyping)
LUAU_FASTFLAG(LuauUseWorkspacePropToChooseSolver)
LUAU_FASTFLAG(LuauParametrizedAttributeSyntax)
LUAU_FASTFLAG(LuauNameConstraintRestrictRecursiveTypes)

namespace Luau
{

static bool typeCouldHaveMetatable(TypeId ty)
{
    return get<TableType>(follow(ty)) || get<ExternType>(follow(ty)) || get<MetatableType>(follow(ty));
}

static void defaultLuauPrintLine(const std::string& s)
{
    printf("%s\n", s.c_str());
}

PrintLineProc luauPrintLine = &defaultLuauPrintLine;

void setPrintLine(PrintLineProc pl)
{
    luauPrintLine = pl;
}

void resetPrintLine()
{
    luauPrintLine = &defaultLuauPrintLine;
}

bool doesCallError(const AstExprCall* call)
{
    const AstExprGlobal* global = call->func->as<AstExprGlobal>();
    if (!global)
        return false;

    if (global->name == "error")
        return true;
    else if (global->name == "assert")
    {
        // assert() will error because it is missing the first argument
        if (call->args.size == 0)
            return true;

        if (AstExprConstantBool* expr = call->args.data[0]->as<AstExprConstantBool>())
            if (!expr->value)
                return true;
    }

    return false;
}

bool hasBreak(AstStat* node)
{
    if (AstStatBlock* stat = node->as<AstStatBlock>())
    {
        for (size_t i = 0; i < stat->body.size; ++i)
        {
            if (hasBreak(stat->body.data[i]))
                return true;
        }

        return false;
    }
    else if (node->is<AstStatBreak>())
    {
        return true;
    }
    else if (AstStatIf* stat = node->as<AstStatIf>())
    {
        if (hasBreak(stat->thenbody))
            return true;

        if (stat->elsebody && hasBreak(stat->elsebody))
            return true;

        return false;
    }
    else
    {
        return false;
    }
}

// returns the last statement before the block exits, or nullptr if the block never exits
const AstStat* getFallthrough(const AstStat* node)
{
    if (const AstStatBlock* stat = node->as<AstStatBlock>())
    {
        if (stat->body.size == 0)
            return stat;

        for (size_t i = 0; i < stat->body.size - 1; ++i)
        {
            if (getFallthrough(stat->body.data[i]) == nullptr)
                return nullptr;
        }

        return getFallthrough(stat->body.data[stat->body.size - 1]);
    }
    else if (const AstStatIf* stat = node->as<AstStatIf>())
    {
        if (const AstStat* thenf = getFallthrough(stat->thenbody))
            return thenf;

        if (stat->elsebody)
        {
            if (const AstStat* elsef = getFallthrough(stat->elsebody))
                return elsef;

            return nullptr;
        }
        else
        {
            return stat;
        }
    }
    else if (node->is<AstStatReturn>())
    {
        return nullptr;
    }
    else if (const AstStatExpr* stat = node->as<AstStatExpr>())
    {
        if (AstExprCall* call = stat->expr->as<AstExprCall>())
        {
            if (doesCallError(call))
                return nullptr;
        }

        return stat;
    }
    else if (const AstStatWhile* stat = node->as<AstStatWhile>())
    {
        if (AstExprConstantBool* expr = stat->condition->as<AstExprConstantBool>())
        {
            if (expr->value && !hasBreak(stat->body))
                return nullptr;
        }

        return node;
    }
    else if (const AstStatRepeat* stat = node->as<AstStatRepeat>())
    {
        if (AstExprConstantBool* expr = stat->condition->as<AstExprConstantBool>())
        {
            if (!expr->value && !hasBreak(stat->body))
                return nullptr;
        }

        if (getFallthrough(stat->body) == nullptr)
            return nullptr;

        return node;
    }
    else
    {
        return node;
    }
}

static bool isMetamethod(const Name& name)
{
    return name == "__index" || name == "__newindex" || name == "__call" || name == "__concat" || name == "__unm" || name == "__add" ||
           name == "__sub" || name == "__mul" || name == "__div" || name == "__mod" || name == "__pow" || name == "__tostring" ||
           name == "__metatable" || name == "__eq" || name == "__lt" || name == "__le" || name == "__mode" || name == "__iter" || name == "__len" ||
           name == "__idiv";
}

size_t HashBoolNamePair::operator()(const std::pair<bool, Name>& pair) const
{
    return std::hash<bool>()(pair.first) ^ std::hash<Name>()(pair.second);
}

TypeChecker::TypeChecker(const ScopePtr& globalScope, ModuleResolver* resolver, NotNull<BuiltinTypes> builtinTypes, InternalErrorReporter* iceHandler)
    : globalScope(globalScope)
    , resolver(resolver)
    , builtinTypes(builtinTypes)
    , iceHandler(iceHandler)
    , unifierState(iceHandler)
    , normalizer(nullptr, builtinTypes, NotNull{&unifierState}, SolverMode::Old)
    , reusableInstantiation(TxnLog::empty(), nullptr, builtinTypes, {}, nullptr)
    , nilType(builtinTypes->nilType)
    , numberType(builtinTypes->numberType)
    , stringType(builtinTypes->stringType)
    , booleanType(builtinTypes->booleanType)
    , threadType(builtinTypes->threadType)
    , bufferType(builtinTypes->bufferType)
    , anyType(builtinTypes->anyType)
    , unknownType(builtinTypes->unknownType)
    , neverType(builtinTypes->neverType)
    , anyTypePack(builtinTypes->anyTypePack)
    , neverTypePack(builtinTypes->neverTypePack)
    , uninhabitableTypePack(builtinTypes->uninhabitableTypePack)
    , duplicateTypeAliases{{false, {}}}
{
}

ModulePtr TypeChecker::check(const SourceModule& module, Mode mode, std::optional<ScopePtr> environmentScope)
{
    try
    {
        return checkWithoutRecursionCheck(module, mode, std::move(environmentScope));
    }
    catch (const RecursionLimitException&)
    {
        reportErrorCodeTooComplex(module.root->location);
        return std::move(currentModule);
    }
}

ModulePtr TypeChecker::checkWithoutRecursionCheck(const SourceModule& module, Mode mode, std::optional<ScopePtr> environmentScope)
{
    LUAU_TIMETRACE_SCOPE("TypeChecker::check", "TypeChecker");
    LUAU_TIMETRACE_ARGUMENT("module", module.name.c_str());
    LUAU_TIMETRACE_ARGUMENT("name", module.humanReadableName.c_str());

    currentModule.reset(new Module);
    currentModule->name = module.name;
    currentModule->humanReadableName = module.humanReadableName;
    currentModule->internalTypes.owningModule = currentModule.get();
    currentModule->interfaceTypes.owningModule = currentModule.get();
    currentModule->type = module.type;
    currentModule->allocator = module.allocator;
    currentModule->names = module.names;
    currentModule->root = module.root;

    iceHandler->moduleName = module.name;
    normalizer.arena = &currentModule->internalTypes;

    unifierState.counters.recursionLimit = FInt::LuauTypeInferRecursionLimit;
    unifierState.counters.iterationLimit = unifierIterationLimit ? *unifierIterationLimit : FInt::LuauTypeInferIterationLimit;

    ScopePtr parentScope = environmentScope.value_or(globalScope);
    ScopePtr moduleScope = std::make_shared<Scope>(parentScope);

    moduleScope->returnType = freshTypePack(moduleScope);
    moduleScope->varargPack = anyTypePack;

    currentModule->scopes.push_back(std::make_pair(module.root->location, moduleScope));
    currentModule->mode = mode;

    if (prepareModuleScope)
        prepareModuleScope(currentModule->name, currentModule->getModuleScope());

    try
    {
        checkBlock(moduleScope, *module.root);
    }
    catch (const TimeLimitError&)
    {
        currentModule->timeout = true;
    }
    catch (const UserCancelError&)
    {
        currentModule->cancelled = true;
    }

    if (get<FreeTypePack>(follow(moduleScope->returnType)))
        moduleScope->returnType = addTypePack(TypePack{{}, std::nullopt});
    else
        moduleScope->returnType = anyify(moduleScope, moduleScope->returnType, Location{});

    moduleScope->returnType = anyifyModuleReturnTypePackGenerics(moduleScope->returnType);

    for (auto& [_, typeFun] : moduleScope->exportedTypeBindings)
        typeFun.type = anyify(moduleScope, typeFun.type, Location{});

    prepareErrorsForDisplay(currentModule->errors);

    // Clear the normalizer caches, since they contain types from the internal type surface
    normalizer.clearCaches();
    normalizer.arena = nullptr;

    if (FFlag::LuauUseWorkspacePropToChooseSolver)
        currentModule->clonePublicInterface(builtinTypes, *iceHandler, SolverMode::Old);
    else
        currentModule->clonePublicInterface_DEPRECATED(builtinTypes, *iceHandler);

    freeze(currentModule->internalTypes);
    freeze(currentModule->interfaceTypes);

    // Clear unifier cache since it's keyed off internal types that get deallocated
    // This avoids fake cross-module cache hits and keeps cache size at bay when typechecking large module graphs.
    unifierState.cachedUnify.clear();
    unifierState.cachedUnifyError.clear();
    unifierState.skipCacheForType.clear();

    duplicateTypeAliases.clear();
    incorrectExternTypeDefinitions.clear();

    return std::move(currentModule);
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStat& program)
{
    if (finishTime && TimeTrace::getClock() > *finishTime)
        throwTimeLimitError();
    if (cancellationToken && cancellationToken->requested())
        throwUserCancelError();

    if (auto block = program.as<AstStatBlock>())
        return check(scope, *block);
    else if (auto if_ = program.as<AstStatIf>())
        return check(scope, *if_);
    else if (auto while_ = program.as<AstStatWhile>())
        return check(scope, *while_);
    else if (auto repeat = program.as<AstStatRepeat>())
        return check(scope, *repeat);
    else if (program.is<AstStatBreak>())
        return ControlFlow::Breaks;
    else if (program.is<AstStatContinue>())
        return ControlFlow::Continues;
    else if (auto return_ = program.as<AstStatReturn>())
        return check(scope, *return_);
    else if (auto expr = program.as<AstStatExpr>())
    {
        checkExprPack(scope, *expr->expr);

        if (auto call = expr->expr->as<AstExprCall>(); call && doesCallError(call))
            return ControlFlow::Throws;

        return ControlFlow::None;
    }
    else if (auto local = program.as<AstStatLocal>())
        return check(scope, *local);
    else if (auto for_ = program.as<AstStatFor>())
        return check(scope, *for_);
    else if (auto forIn = program.as<AstStatForIn>())
        return check(scope, *forIn);
    else if (auto assign = program.as<AstStatAssign>())
        return check(scope, *assign);
    else if (auto assign = program.as<AstStatCompoundAssign>())
        return check(scope, *assign);
    else if (program.is<AstStatFunction>())
        ice("Should not be calling two-argument check() on a function statement", program.location);
    else if (program.is<AstStatLocalFunction>())
        ice("Should not be calling two-argument check() on a function statement", program.location);
    else if (auto typealias = program.as<AstStatTypeAlias>())
        return check(scope, *typealias);
    else if (auto typefunction = program.as<AstStatTypeFunction>())
        return check(scope, *typefunction);
    else if (auto global = program.as<AstStatDeclareGlobal>())
    {
        TypeId globalType = resolveType(scope, *global->type);
        Name globalName(global->name.value);

        currentModule->declaredGlobals[globalName] = globalType;
        currentModule->getModuleScope()->bindings[global->name] = Binding{globalType, global->location};

        return ControlFlow::None;
    }
    else if (auto global = program.as<AstStatDeclareFunction>())
        return check(scope, *global);
    else if (auto global = program.as<AstStatDeclareExternType>())
        return check(scope, *global);
    else if (auto errorStatement = program.as<AstStatError>())
    {
        const size_t oldSize = currentModule->errors.size();

        for (AstStat* s : errorStatement->statements)
            check(scope, *s);

        for (AstExpr* expr : errorStatement->expressions)
            checkExpr(scope, *expr);

        // HACK: We want to run typechecking on the contents of the AstStatError, but
        // we don't think the type errors will be useful most of the time.
        currentModule->errors.resize(oldSize);

        return ControlFlow::None;
    }
    else
        ice("Unknown AstStat");
}

// This particular overload is for do...end. If you need to not increase the scope level, use checkBlock directly.
ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatBlock& block)
{
    ScopePtr child = childScope(scope, block.location);

    ControlFlow flow = checkBlock(child, block);
    scope->inheritRefinements(child);

    return flow;
}

ControlFlow TypeChecker::checkBlock(const ScopePtr& scope, const AstStatBlock& block)
{
    RecursionCounter _rc(&checkRecursionCount);
    if (FInt::LuauCheckRecursionLimit > 0 && checkRecursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportErrorCodeTooComplex(block.location);
        return ControlFlow::None;
    }
    try
    {
        return checkBlockWithoutRecursionCheck(scope, block);
    }
    catch (const RecursionLimitException&)
    {
        reportErrorCodeTooComplex(block.location);
        return ControlFlow::None;
    }
}

struct InplaceDemoter : TypeOnceVisitor
{
    TypeLevel newLevel;
    TypeArena* arena;

    InplaceDemoter(TypeLevel level, TypeArena* arena)
        : TypeOnceVisitor("InplaceDemoter", /* skipBoundTypes= */ true)
        , newLevel(level)
        , arena(arena)
    {
    }

    bool demote(TypeId ty)
    {
        if (auto level = getMutableLevel(ty))
        {
            if (level->subsumesStrict(newLevel))
            {
                *level = newLevel;
                return true;
            }
        }

        return false;
    }

    bool visit(TypeId ty) override
    {
        if (ty->owningArena != arena)
            return false;
        return demote(ty);
    }

    bool visit(TypePackId tp, const FreeTypePack& ftpRef) override
    {
        if (tp->owningArena != arena)
            return false;

        FreeTypePack* ftp = &const_cast<FreeTypePack&>(ftpRef);
        if (ftp->level.subsumesStrict(newLevel))
        {
            ftp->level = newLevel;
            return true;
        }

        return false;
    }
};

ControlFlow TypeChecker::checkBlockWithoutRecursionCheck(const ScopePtr& scope, const AstStatBlock& block)
{
    int subLevel = 0;

    std::vector<AstStat*> sorted(block.body.data, block.body.data + block.body.size);
    toposort(sorted);

    for (const auto& stat : sorted)
    {
        if (const auto& typealias = stat->as<AstStatTypeAlias>())
        {
            prototype(scope, *typealias, subLevel);
            ++subLevel;
        }
        else if (const auto& declaredExternType = stat->as<AstStatDeclareExternType>())
        {
            prototype(scope, *declaredExternType);
        }
    }

    auto protoIter = sorted.begin();
    auto checkIter = sorted.begin();

    std::unordered_map<AstStat*, std::pair<TypeId, ScopePtr>> functionDecls;

    auto checkBody = [&](AstStat* stat)
    {
        if (auto fun = stat->as<AstStatFunction>())
        {
            LUAU_ASSERT(functionDecls.count(stat));
            auto [funTy, funScope] = functionDecls[stat];
            check(scope, funTy, funScope, *fun);
        }
        else if (auto fun = stat->as<AstStatLocalFunction>())
        {
            LUAU_ASSERT(functionDecls.count(stat));
            auto [funTy, funScope] = functionDecls[stat];
            check(scope, funTy, funScope, *fun);
        }
    };

    std::optional<ControlFlow> firstFlow;
    while (protoIter != sorted.end())
    {
        // protoIter walks forward
        //      If it contains a function call (function bodies don't count), walk checkIter forward until it catches up with protoIter
        //          For each element checkIter sees, check function bodies and unify the computed type with the prototype
        //      If it is a function definition, add its prototype to the environment
        //      If it is anything else, check it.

        // A subtlety is caused by mutually recursive functions, e.g.
        // ```
        // function f(x) return g(x) end
        // function g(x) return f(x) end
        // ```
        // These both call each other, so `f` will be ordered before `g`, so the call to `g`
        // is typechecked before `g` has had its body checked. For this reason, there's three
        // types for each function: before its body is checked, during checking its body,
        // and after its body is checked.
        //
        // We currently treat the before-type and the during-type as the same,
        // which can result in some oddness, as the before-type is usually a monotype,
        // and the after-type is often a polytype. For example:
        //
        // ```
        // function f(x) local x: number = g(37) return x end
        // function g(x) return f(x) end
        // ```
        // The before-type of g is `(X)->Y...` but during type-checking of `f` we will
        // unify that with `(number)->number`. The types end up being
        // ```
        // function f<a>(x:a):a local x: number = g(37) return x end
        // function g(x:number):number return f(x) end
        // ```
        if (containsFunctionCallOrReturn(**protoIter))
        {
            while (checkIter != protoIter)
            {
                checkBody(*checkIter);
                ++checkIter;
            }

            // We do check the current element, so advance checkIter beyond it.
            ++checkIter;
            ControlFlow flow = check(scope, **protoIter);
            if (flow != ControlFlow::None && !firstFlow)
                firstFlow = flow;
        }
        else if (auto fun = (*protoIter)->as<AstStatFunction>())
        {
            std::optional<TypeId> selfType; // TODO clip
            std::optional<TypeId> expectedType;

            if (!fun->func->self)
            {
                if (auto name = fun->name->as<AstExprIndexName>())
                {
                    TypeId exprTy = checkExpr(scope, *name->expr).type;
                    expectedType = getIndexTypeFromType(scope, exprTy, name->index.value, name->indexLocation, /* addErrors= */ false);
                }
            }

            auto pair = checkFunctionSignature(scope, subLevel, *fun->func, fun->name->location, selfType, expectedType);
            auto [funTy, funScope] = pair;

            functionDecls[*protoIter] = pair;
            ++subLevel;

            TypeId leftType = follow(checkFunctionName(scope, *fun->name, funScope->level));

            unify(funTy, leftType, scope, fun->location);
        }
        else if (auto fun = (*protoIter)->as<AstStatLocalFunction>())
        {
            auto pair = checkFunctionSignature(scope, subLevel, *fun->func, fun->name->location, std::nullopt, std::nullopt);
            auto [funTy, funScope] = pair;

            functionDecls[*protoIter] = pair;
            ++subLevel;

            scope->bindings[fun->name] = {funTy, fun->name->location};
        }
        else
        {
            ControlFlow flow = check(scope, **protoIter);
            if (flow != ControlFlow::None && !firstFlow)
                firstFlow = flow;
        }

        ++protoIter;
    }

    while (checkIter != sorted.end())
    {
        checkBody(*checkIter);
        ++checkIter;
    }

    checkBlockTypeAliases(scope, sorted);

    return firstFlow.value_or(ControlFlow::None);
}

LUAU_NOINLINE void TypeChecker::checkBlockTypeAliases(const ScopePtr& scope, std::vector<AstStat*>& sorted)
{
    for (const auto& stat : sorted)
    {
        if (const auto& typealias = stat->as<AstStatTypeAlias>())
        {
            if (typealias->name == kParseNameError || typealias->name == "typeof")
                continue;

            auto& bindings = typealias->exported ? scope->exportedTypeBindings : scope->privateTypeBindings;

            Name name = typealias->name.value;

            if (duplicateTypeAliases.contains({typealias->exported, name}))
                continue;

            TypeId type = follow(bindings[name].type);
            if (get<FreeType>(type))
            {
                asMutable(type)->ty.emplace<BoundType>(errorRecoveryType(anyType));

                reportError(TypeError{typealias->location, OccursCheckFailed{}});
            }
        }
    }
}

static std::optional<Predicate> tryGetTypeGuardPredicate(const AstExprBinary& expr)
{
    if (expr.op != AstExprBinary::Op::CompareEq && expr.op != AstExprBinary::Op::CompareNe)
        return std::nullopt;

    AstExpr* left = expr.left;
    AstExpr* right = expr.right;

    if (left->as<AstExprConstantString>())
        std::swap(left, right);

    AstExprConstantString* str = right->as<AstExprConstantString>();
    if (!str)
        return std::nullopt;

    AstExprCall* call = left->as<AstExprCall>();
    if (!call)
        return std::nullopt;

    AstExprGlobal* callee = call->func->as<AstExprGlobal>();
    if (!callee)
        return std::nullopt;

    if (callee->name != "type" && callee->name != "typeof")
        return std::nullopt;

    if (call->args.size != 1)
        return std::nullopt;

    // If ssval is not a valid constant string, we'll find out later when resolving predicate.
    Name ssval(str->value.data, str->value.size);
    bool isTypeof = callee->name == "typeof";

    std::optional<LValue> lvalue = tryGetLValue(*call->args.data[0]);
    if (!lvalue)
        return std::nullopt;

    Predicate predicate{TypeGuardPredicate{std::move(*lvalue), expr.location, std::move(ssval), isTypeof}};
    if (expr.op == AstExprBinary::Op::CompareNe)
        return NotPredicate{{std::move(predicate)}};

    return predicate;
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatIf& statement)
{
    WithPredicate<TypeId> result = checkExpr(scope, *statement.condition);

    ScopePtr thenScope = childScope(scope, statement.thenbody->location);
    resolve(result.predicates, thenScope, true);

    ScopePtr elseScope = childScope(scope, statement.elsebody ? statement.elsebody->location : statement.location);
    resolve(result.predicates, elseScope, false);

    ControlFlow thencf = check(thenScope, *statement.thenbody);
    ControlFlow elsecf = ControlFlow::None;
    if (statement.elsebody)
        elsecf = check(elseScope, *statement.elsebody);

    if (thencf != ControlFlow::None && elsecf == ControlFlow::None)
        scope->inheritRefinements(elseScope);
    else if (thencf == ControlFlow::None && elsecf != ControlFlow::None)
        scope->inheritRefinements(thenScope);

    if (thencf == elsecf)
        return thencf;
    else if (matches(thencf, ControlFlow::Returns | ControlFlow::Throws) && matches(elsecf, ControlFlow::Returns | ControlFlow::Throws))
        return ControlFlow::Returns;
    else
        return ControlFlow::None;
}

template<typename Id>
ErrorVec TypeChecker::canUnify_(Id subTy, Id superTy, const ScopePtr& scope, const Location& location)
{
    Unifier state = mkUnifier(scope, location);
    return state.canUnify(subTy, superTy);
}

ErrorVec TypeChecker::canUnify(TypeId subTy, TypeId superTy, const ScopePtr& scope, const Location& location)
{
    return canUnify_(subTy, superTy, scope, location);
}

ErrorVec TypeChecker::canUnify(TypePackId subTy, TypePackId superTy, const ScopePtr& scope, const Location& location)
{
    return canUnify_(subTy, superTy, scope, location);
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatWhile& statement)
{
    WithPredicate<TypeId> result = checkExpr(scope, *statement.condition);

    ScopePtr whileScope = childScope(scope, statement.body->location);
    resolve(result.predicates, whileScope, true);
    check(whileScope, *statement.body);

    return ControlFlow::None;
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatRepeat& statement)
{
    ScopePtr repScope = childScope(scope, statement.location);

    checkBlock(repScope, *statement.body);

    checkExpr(repScope, *statement.condition);

    return ControlFlow::None;
}

struct Demoter : Substitution
{
    TypeArena* arena = nullptr;
    NotNull<BuiltinTypes> builtins;
    Demoter(TypeArena* arena, NotNull<BuiltinTypes> builtins)
        : Substitution(TxnLog::empty(), arena)
        , arena(arena)
        , builtins(builtins)
    {
    }

    bool isDirty(TypeId ty) override
    {
        return get<FreeType>(ty);
    }

    bool isDirty(TypePackId tp) override
    {
        return get<FreeTypePack>(tp);
    }

    bool ignoreChildren(TypeId ty) override
    {
        if (get<ExternType>(ty))
            return true;

        return false;
    }

    TypeId clean(TypeId ty) override
    {
        auto ftv = get<FreeType>(ty);
        LUAU_ASSERT(ftv);
        return arena->freshType(builtins, demotedLevel(ftv->level));
    }

    TypePackId clean(TypePackId tp) override
    {
        auto ftp = get<FreeTypePack>(tp);
        LUAU_ASSERT(ftp);
        return addTypePack(TypePackVar{FreeTypePack{demotedLevel(ftp->level)}});
    }

    TypeLevel demotedLevel(TypeLevel level)
    {
        return TypeLevel{level.level + 5000, level.subLevel};
    }

    void demote(std::vector<std::optional<TypeId>>& expectedTypes)
    {
        for (std::optional<TypeId>& ty : expectedTypes)
        {
            if (ty)
                ty = substitute(*ty);
        }
    }
};

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatReturn& return_)
{
    std::vector<std::optional<TypeId>> expectedTypes;
    expectedTypes.reserve(return_.list.size);

    TypePackIterator expectedRetCurr = begin(scope->returnType);
    TypePackIterator expectedRetEnd = end(scope->returnType);

    for (size_t i = 0; i < return_.list.size; ++i)
    {
        if (expectedRetCurr != expectedRetEnd)
        {
            expectedTypes.push_back(*expectedRetCurr);
            ++expectedRetCurr;
        }
        else if (auto expectedArgsTail = expectedRetCurr.tail())
        {
            if (const VariadicTypePack* vtp = get<VariadicTypePack>(follow(*expectedArgsTail)))
                expectedTypes.push_back(vtp->ty);
        }
    }

    Demoter demoter{&currentModule->internalTypes, builtinTypes};
    demoter.demote(expectedTypes);

    TypePackId retPack = checkExprList(scope, return_.location, return_.list, false, {}, expectedTypes).type;

    // HACK: Nonstrict mode gets a bit too smart and strict for us when we
    // start typechecking everything across module boundaries.
    if (isNonstrictMode() && follow(scope->returnType) == follow(currentModule->getModuleScope()->returnType))
    {
        ErrorVec errors = tryUnify(retPack, scope->returnType, scope, return_.location);

        if (!errors.empty())
            currentModule->getModuleScope()->returnType = addTypePack({anyType});

        return ControlFlow::Returns;
    }

    unify(retPack, scope->returnType, scope, return_.location, CountMismatch::Context::Return);

    return ControlFlow::Returns;
}

template<typename Id>
ErrorVec TypeChecker::tryUnify_(Id subTy, Id superTy, const ScopePtr& scope, const Location& location)
{
    Unifier state = mkUnifier(scope, location);

    if (FFlag::DebugLuauFreezeDuringUnification)
        freeze(currentModule->internalTypes);

    state.tryUnify(subTy, superTy);

    if (FFlag::DebugLuauFreezeDuringUnification)
        unfreeze(currentModule->internalTypes);

    if (state.errors.empty())
        state.log.commit();

    return state.errors;
}

ErrorVec TypeChecker::tryUnify(TypeId subTy, TypeId superTy, const ScopePtr& scope, const Location& location)
{
    return tryUnify_(subTy, superTy, scope, location);
}

ErrorVec TypeChecker::tryUnify(TypePackId subTy, TypePackId superTy, const ScopePtr& scope, const Location& location)
{
    return tryUnify_(subTy, superTy, scope, location);
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatAssign& assign)
{
    std::vector<std::optional<TypeId>> expectedTypes;
    expectedTypes.reserve(assign.vars.size);

    ScopePtr moduleScope = currentModule->getModuleScope();

    for (size_t i = 0; i < assign.vars.size; ++i)
    {
        AstExpr* dest = assign.vars.data[i];

        if (auto a = dest->as<AstExprLocal>())
        {
            // AstExprLocal l-values will have to be checked again because their type might have been mutated during checkExprList later
            expectedTypes.push_back(scope->lookup(a->local));
        }
        else if (auto a = dest->as<AstExprGlobal>())
        {
            // AstExprGlobal l-values lookup is inlined here to avoid creating a global binding before checkExprList
            if (auto it = moduleScope->bindings.find(a->name); it != moduleScope->bindings.end())
                expectedTypes.push_back(it->second.typeId);
            else
                expectedTypes.push_back(std::nullopt);
        }
        else
        {
            expectedTypes.push_back(checkLValue(scope, *dest, ValueContext::LValue));
        }
    }

    TypePackId valuePack = checkExprList(scope, assign.location, assign.values, false, {}, expectedTypes).type;

    auto valueIter = begin(valuePack);
    auto valueEnd = end(valuePack);

    TypePack* growingPack = nullptr;

    for (size_t i = 0; i < assign.vars.size; ++i)
    {
        AstExpr* dest = assign.vars.data[i];
        TypeId left = nullptr;

        if (dest->is<AstExprLocal>() || dest->is<AstExprGlobal>())
            left = checkLValue(scope, *dest, ValueContext::LValue);
        else
            left = *expectedTypes[i];

        TypeId right = nullptr;

        Location loc = 0 == assign.values.size  ? assign.location
                       : i < assign.values.size ? assign.values.data[i]->location
                                                : assign.values.data[assign.values.size - 1]->location;

        if (valueIter != valueEnd)
        {
            right = follow(*valueIter);
            ++valueIter;
        }
        else if (growingPack)
        {
            growingPack->head.push_back(left);
            continue;
        }
        else if (auto tail = valueIter.tail())
        {
            TypePackId tailPack = follow(*tail);
            if (get<ErrorTypePack>(tailPack))
                right = errorRecoveryType(scope);
            else if (auto vtp = get<VariadicTypePack>(tailPack))
                right = vtp->ty;
            else if (get<FreeTypePack>(tailPack))
            {
                *asMutable(tailPack) = TypePack{{left}};
                growingPack = getMutable<TypePack>(tailPack);
            }
        }

        if (right)
        {
            if (!FFlag::LuauInstantiateInSubtyping)
            {
                if (!maybeGeneric(left) && isGeneric(right))
                    right = instantiate(scope, right, loc);
            }

            // Setting a table entry to nil doesn't mean nil is the type of the indexer, it is just deleting the entry
            const TableType* destTableTypeReceivingNil = nullptr;
            if (auto indexExpr = dest->as<AstExprIndexExpr>(); isNil(right) && indexExpr)
                destTableTypeReceivingNil = getTableType(checkExpr(scope, *indexExpr->expr).type);

            if (!destTableTypeReceivingNil || !destTableTypeReceivingNil->indexer)
            {
                // In nonstrict mode, any assignments where the lhs is free and rhs isn't a function, we give it any type.
                if (isNonstrictMode() && get<FreeType>(follow(left)) && !get<FunctionType>(follow(right)))
                    unify(anyType, left, scope, loc);
                else
                    unify(right, left, scope, loc);
            }
        }
    }

    return ControlFlow::None;
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatCompoundAssign& assign)
{
    AstExprBinary expr(assign.location, assign.op, assign.var, assign.value);

    TypeId left = checkExpr(scope, *expr.left).type;
    TypeId right = checkExpr(scope, *expr.right).type;

    TypeId result = checkBinaryOperation(scope, expr, left, right);

    unify(result, left, scope, assign.location);

    return ControlFlow::None;
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatLocal& local)
{
    // Important subtlety: A local variable is not in scope while its initializer is being evaluated.
    // For instance, you cannot do this:
    //     local a = function() return a end

    AstLocal** vars = local.vars.data;

    std::vector<std::pair<AstLocal*, Binding>> varBindings;
    varBindings.reserve(local.vars.size);

    std::vector<TypeId> variableTypes;
    variableTypes.reserve(local.vars.size);

    std::vector<std::optional<TypeId>> expectedTypes;
    expectedTypes.reserve(local.vars.size);

    std::vector<bool> instantiateGenerics;

    for (size_t i = 0; i < local.vars.size; ++i)
    {
        const AstType* annotation = vars[i]->annotation;
        const bool rhsIsTable = local.values.size > i && local.values.data[i]->as<AstExprTable>();

        TypeId ty = nullptr;

        if (annotation)
        {
            ty = resolveType(scope, *annotation);

            // If the annotation type has an error, treat it as if there was no annotation
            if (get<ErrorType>(follow(ty)))
                ty = nullptr;
        }

        if (!ty)
            ty = rhsIsTable ? freshType(scope) : isNonstrictMode() ? anyType : freshType(scope);

        varBindings.emplace_back(vars[i], Binding{ty, vars[i]->location});

        variableTypes.push_back(ty);
        expectedTypes.push_back(ty);

        // with FFlag::LuauInstantiateInSubtyping enabled, we shouldn't need to produce instantiateGenerics at all.
        if (!FFlag::LuauInstantiateInSubtyping)
            instantiateGenerics.push_back(annotation != nullptr && !maybeGeneric(ty));
    }

    if (local.values.size > 0)
    {
        TypePackId variablePack = addTypePack(variableTypes, freshTypePack(scope));
        TypePackId valuePack =
            checkExprList(scope, local.location, local.values, /* substituteFreeForNil= */ true, instantiateGenerics, expectedTypes).type;

        // If the expression list only contains one expression and it's a function call or is otherwise within parentheses, use FunctionResult.
        // Otherwise, we'll want to use ExprListResult to make the error messaging more general.
        CountMismatch::Context ctx = CountMismatch::ExprListResult;
        if (local.values.size == 1)
        {
            AstExpr* e = local.values.data[0];
            while (auto group = e->as<AstExprGroup>())
                e = group->expr;
            if (e->is<AstExprCall>())
                ctx = CountMismatch::FunctionResult;
        }

        Unifier state = mkUnifier(scope, local.location);
        state.ctx = ctx;
        state.tryUnify(valuePack, variablePack);
        reportErrors(state.errors);

        state.log.commit();

        // In the code 'local T = {}', we wish to ascribe the name 'T' to the type of the table for error-reporting purposes.
        // We also want to do this for 'local T = setmetatable(...)'.
        if (local.vars.size == 1 && local.values.size == 1)
        {
            const AstExpr* rhs = local.values.data[0];
            std::optional<TypeId> ty = first(valuePack);

            if (ty)
            {
                if (rhs->is<AstExprTable>())
                {
                    TableType* ttv = getMutable<TableType>(follow(*ty));
                    if (ttv && !ttv->name && scope == currentModule->getModuleScope())
                        ttv->syntheticName = vars[0]->name.value;
                }
                else if (const AstExprCall* call = rhs->as<AstExprCall>())
                {
                    if (const AstExprGlobal* global = call->func->as<AstExprGlobal>(); global && global->name == "setmetatable")
                    {
                        MetatableType* mtv = getMutable<MetatableType>(follow(*ty));
                        if (mtv)
                            mtv->syntheticName = vars[0]->name.value;
                    }
                }
            }
        }

        // Handle 'require' calls, we need to import exported type bindings into the variable 'namespace' and to update binding type in non-strict
        // mode
        for (size_t i = 0; i < local.values.size && i < local.vars.size; ++i)
        {
            const AstExprCall* call = local.values.data[i]->as<AstExprCall>();
            if (!call)
                continue;

            if (auto maybeRequire = matchRequire(*call))
            {
                AstExpr* require = *maybeRequire;

                if (auto moduleInfo = resolver->resolveModuleInfo(currentModule->name, *require))
                {
                    const Name name{local.vars.data[i]->name.value};

                    if (ModulePtr module = resolver->getModule(moduleInfo->name))
                    {
                        scope->importedTypeBindings[name] = module->exportedTypeBindings;
                        scope->importedModules[name] = moduleInfo->name;

                        // Imported types of requires that transitively refer to current module have to be replaced with 'any'
                        for (const auto& [location, path] : requireCycles)
                        {
                            if (!path.empty() && path.front() == moduleInfo->name)
                            {
                                for (auto& [name, tf] : scope->importedTypeBindings[name])
                                    tf = TypeFun{{}, {}, anyType};
                            }
                        }
                    }

                    // In non-strict mode we force the module type on the variable, in strict mode it is already unified
                    if (isNonstrictMode())
                    {
                        auto [types, tail] = flatten(valuePack);

                        if (i < types.size())
                            varBindings[i].second.typeId = types[i];
                    }
                }
            }
        }
    }

    for (const auto& [local, binding] : varBindings)
        scope->bindings[local] = binding;

    return ControlFlow::None;
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatFor& expr)
{
    ScopePtr loopScope = childScope(scope, expr.location);

    TypeId loopVarType = numberType;
    if (expr.var->annotation)
        unify(loopVarType, resolveType(scope, *expr.var->annotation), scope, expr.location);

    loopScope->bindings[expr.var] = {loopVarType, expr.var->location};

    if (!expr.from)
        ice("Bad AstStatFor has no from expr");

    if (!expr.to)
        ice("Bad AstStatFor has no to expr");

    unify(checkExpr(loopScope, *expr.from).type, loopVarType, scope, expr.from->location);
    unify(checkExpr(loopScope, *expr.to).type, loopVarType, scope, expr.to->location);

    if (expr.step)
        unify(checkExpr(loopScope, *expr.step).type, loopVarType, scope, expr.step->location);

    check(loopScope, *expr.body);

    return ControlFlow::None;
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatForIn& forin)
{
    ScopePtr loopScope = childScope(scope, forin.location);

    AstLocal** vars = forin.vars.data;

    std::vector<TypeId> varTypes;
    varTypes.reserve(forin.vars.size);

    for (size_t i = 0; i < forin.vars.size; ++i)
    {
        AstType* ann = vars[i]->annotation;
        TypeId ty = ann ? resolveType(scope, *ann) : anyIfNonstrict(freshType(loopScope));

        loopScope->bindings[vars[i]] = {ty, vars[i]->location};
        varTypes.push_back(ty);
    }

    AstExpr** values = forin.values.data;
    AstExpr* firstValue = forin.values.data[0];

    // next is a function that takes Table<K, V> and an optional index of type K
    //      next<K, V>(t: Table<K, V>, index: K | nil) -> (K?, V)
    // however, pairs and ipairs are quite messy, but they both share the same types
    // pairs returns 'next, t, nil', thus the type would be
    //      pairs<K, V>(t: Table<K, V>) -> ((Table<K, V>, K | nil) -> (K?, V), Table<K, V>, K | nil)
    // ipairs returns 'next, t, 0', thus ipairs will also share the same type as pairs, except K = number
    //
    // we can also define our own custom iterators by by returning a wrapped coroutine that calls coroutine.yield
    // and most custom iterators does not return a table state, or returns a function that takes no additional arguments, making it optional
    // so we up with this catch-all type constraint that works for all use cases
    //      <K, V, R>(free) -> ((free) -> R, Table<K, V> | nil, K | nil)

    if (!firstValue)
        ice("expected at least an iterator function value, but we parsed nothing");

    TypeId iterTy = nullptr;
    TypePackId callRetPack = nullptr;

    if (forin.values.size == 1 && firstValue->is<AstExprCall>())
    {
        AstExprCall* exprCall = firstValue->as<AstExprCall>();
        callRetPack = checkExprPack(scope, *exprCall).type;
        callRetPack = follow(callRetPack);

        if (get<FreeTypePack>(callRetPack))
        {
            iterTy = freshType(scope);
            unify(callRetPack, addTypePack({{iterTy}, freshTypePack(scope)}), scope, forin.location);
        }
        else if (get<ErrorTypePack>(callRetPack) || !first(callRetPack))
        {
            for (TypeId var : varTypes)
                unify(errorRecoveryType(scope), var, scope, forin.location);

            return check(loopScope, *forin.body);
        }
        else
        {
            iterTy = *first(callRetPack);
            iterTy = instantiate(scope, iterTy, exprCall->location);
        }
    }
    else
    {
        iterTy = instantiate(scope, checkExpr(scope, *firstValue).type, firstValue->location);
    }

    iterTy = stripFromNilAndReport(iterTy, firstValue->location);

    if (std::optional<TypeId> iterMM = findMetatableEntry(iterTy, "__iter", firstValue->location, /* addErrors= */ true))
    {
        // if __iter metamethod is present, it will be called and the results are going to be called as if they are functions
        // TODO: this needs to typecheck all returned values by __iter as if they were for loop arguments
        // the structure of the function makes it difficult to do this especially since we don't have actual expressions, only types
        for (TypeId var : varTypes)
            unify(anyType, var, scope, forin.location);

        return check(loopScope, *forin.body);
    }

    if (const TableType* iterTable = get<TableType>(iterTy))
    {
        // TODO: note that this doesn't cleanly handle iteration over mixed tables and tables without an indexer
        // this behavior is more or less consistent with what we do for pairs(), but really both are pretty wrong and need revisiting
        if (iterTable->indexer)
        {
            if (varTypes.size() > 0)
                unify(iterTable->indexer->indexType, varTypes[0], scope, forin.location);

            if (varTypes.size() > 1)
                unify(iterTable->indexer->indexResultType, varTypes[1], scope, forin.location);

            for (size_t i = 2; i < varTypes.size(); ++i)
                unify(nilType, varTypes[i], scope, forin.location);
        }
        else
        {
            for (TypeId var : varTypes)
                unify(unknownType, var, scope, forin.location);
        }

        return check(loopScope, *forin.body);
    }

    const FunctionType* iterFunc = get<FunctionType>(iterTy);
    if (!iterFunc)
    {
        TypeId varTy = get<AnyType>(iterTy) ? anyType : errorRecoveryType(loopScope);

        for (TypeId var : varTypes)
            unify(varTy, var, scope, forin.location);

        if (!get<ErrorType>(iterTy) && !get<AnyType>(iterTy) && !get<FreeType>(iterTy) && !get<NeverType>(iterTy))
            reportError(firstValue->location, CannotCallNonFunction{iterTy});

        return check(loopScope, *forin.body);
    }

    if (forin.values.size == 1)
    {
        TypePackId argPack = nullptr;
        if (firstValue->is<AstExprCall>())
        {
            // Extract the remaining return values of the call
            // and check them against the parameter types of the iterator function.
            auto [types, tail] = flatten(callRetPack);

            if (!types.empty())
            {
                std::vector<TypeId> argTypes = std::vector<TypeId>(types.begin() + 1, types.end());
                argPack = addTypePack(TypePackVar{TypePack{std::move(argTypes), tail}});
            }
            else
            {
                argPack = addTypePack(TypePack{});
            }
        }
        else
        {
            // Check if iterator function accepts 0 arguments
            argPack = addTypePack(TypePack{});
        }

        Unifier state = mkUnifier(loopScope, firstValue->location);
        checkArgumentList(loopScope, *firstValue, state, argPack, iterFunc->argTypes, /*argLocations*/ {});

        state.log.commit();

        reportErrors(state.errors);
    }

    TypePackId retPack = iterFunc->retTypes;

    if (forin.values.size >= 2)
    {
        AstArray<AstExpr*> arguments{forin.values.data + 1, forin.values.size - 1};

        Position start = firstValue->location.begin;
        Position end = values[forin.values.size - 1]->location.end;
        AstExprCall exprCall{Location(start, end), firstValue, arguments, /* self= */ false, Location()};

        retPack = checkExprPack(scope, exprCall).type;
    }

    // We need to remove 'nil' from the set of options of the first return value
    // Because for loop stops when it gets 'nil', this result is never actually assigned to the first variable
    if (std::optional<TypeId> fty = first(retPack); fty && !varTypes.empty())
    {
        TypeId keyTy = follow(*fty);

        if (get<UnionType>(keyTy))
        {
            if (std::optional<TypeId> ty = tryStripUnionFromNil(keyTy))
                keyTy = *ty;
        }

        unify(keyTy, varTypes.front(), scope, forin.location);

        // We have already handled the first variable type, make it match in the pack check
        varTypes.front() = *fty;
    }

    TypePackId varPack = addTypePack(TypePackVar{TypePack{std::move(varTypes), freshTypePack(scope)}});

    unify(retPack, varPack, scope, forin.location);

    check(loopScope, *forin.body);

    return ControlFlow::None;
}

ControlFlow TypeChecker::check(const ScopePtr& scope, TypeId ty, const ScopePtr& funScope, const AstStatFunction& function)
{
    if (auto exprName = function.name->as<AstExprGlobal>())
    {
        auto& globalBindings = currentModule->getModuleScope()->bindings;
        Symbol name = exprName->name;
        Name globalName = exprName->name.value;

        Binding oldBinding;
        bool previouslyDefined = isNonstrictMode() && globalBindings.count(name);

        if (previouslyDefined)
        {
            oldBinding = globalBindings[name];
        }

        globalBindings[name] = {ty, exprName->location};
        checkFunctionBody(funScope, ty, *function.func);

        // If in nonstrict mode and allowing redefinition of global function, restore the previous definition type
        // in case this function has a differing signature. The signature discrepancy will be caught in checkBlock.
        if (previouslyDefined)
            globalBindings[name] = oldBinding;
        else
            globalBindings[name] = {quantify(funScope, ty, exprName->location), exprName->location};
    }
    else if (auto name = function.name->as<AstExprLocal>())
    {
        scope->bindings[name->local] = {ty, name->local->location};

        checkFunctionBody(funScope, ty, *function.func);

        scope->bindings[name->local] = {anyIfNonstrict(quantify(funScope, ty, name->local->location)), name->local->location};
    }
    else if (auto name = function.name->as<AstExprIndexName>())
    {
        TypeId exprTy = checkExpr(scope, *name->expr).type;
        TableType* ttv = getMutableTableType(exprTy);

        if (!getIndexTypeFromType(scope, exprTy, name->index.value, name->indexLocation, /* addErrors= */ false))
        {
            if (ttv || isTableIntersection(exprTy))
                reportError(TypeError{function.location, CannotExtendTable{exprTy, CannotExtendTable::Property, name->index.value}});
            else
                reportError(TypeError{function.location, OnlyTablesCanHaveMethods{exprTy}});
        }

        ty = follow(ty);

        if (ttv && ttv->state != TableState::Sealed)
            ttv->props[name->index.value] = {ty, /* deprecated */ false, {}, name->indexLocation};

        if (function.func->self)
        {
            const FunctionType* funTy = get<FunctionType>(ty);
            if (!funTy)
                ice("Methods should be functions");

            std::optional<TypeId> arg0 = first(funTy->argTypes);
            if (!arg0)
                ice("Methods should always have at least 1 argument (self)");
        }

        checkFunctionBody(funScope, ty, *function.func);

        InplaceDemoter demoter{funScope->level, &currentModule->internalTypes};
        demoter.traverse(ty);

        if (ttv && ttv->state != TableState::Sealed)
            ttv->props[name->index.value] = {follow(quantify(funScope, ty, name->indexLocation)), /* deprecated */ false, {}, name->indexLocation};
    }
    else
    {
        LUAU_ASSERT(function.name->is<AstExprError>());

        ty = follow(ty);

        checkFunctionBody(funScope, ty, *function.func);
    }

    return ControlFlow::None;
}

ControlFlow TypeChecker::check(const ScopePtr& scope, TypeId ty, const ScopePtr& funScope, const AstStatLocalFunction& function)
{
    Name name = function.name->name.value;

    scope->bindings[function.name] = {ty, function.location};

    checkFunctionBody(funScope, ty, *function.func);

    scope->bindings[function.name] = {quantify(funScope, ty, function.name->location), function.name->location};

    return ControlFlow::None;
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatTypeAlias& typealias)
{
    Name name = typealias.name.value;

    // If the alias is missing a name, we can't do anything with it.  Ignore it.
    if (name == kParseNameError)
        return ControlFlow::None;

    if (name == "typeof")
    {
        reportError(typealias.location, GenericError{"Type aliases cannot be named typeof"});
        return ControlFlow::None;
    }

    std::optional<TypeFun> binding;
    if (auto it = scope->exportedTypeBindings.find(name); it != scope->exportedTypeBindings.end())
        binding = it->second;
    else if (auto it = scope->privateTypeBindings.find(name); it != scope->privateTypeBindings.end())
        binding = it->second;

    auto& bindingsMap = typealias.exported ? scope->exportedTypeBindings : scope->privateTypeBindings;

    // If the first pass failed (this should mean a duplicate definition), the second pass isn't going to be
    // interesting.
    if (duplicateTypeAliases.find({typealias.exported, name}))
        return ControlFlow::None;

    // By now this alias must have been `prototype()`d first.
    if (!binding)
        ice("Not predeclared");

    ScopePtr aliasScope = childScope(scope, typealias.location);
    aliasScope->level = scope->level.incr();

    for (auto param : binding->typeParams)
    {
        auto generic = get<GenericType>(param.ty);
        LUAU_ASSERT(generic);
        aliasScope->privateTypeBindings[generic->name] = TypeFun{{}, param.ty};
    }

    for (auto param : binding->typePackParams)
    {
        auto generic = get<GenericTypePack>(param.tp);
        LUAU_ASSERT(generic);
        aliasScope->privateTypePackBindings[generic->name] = param.tp;
    }

    TypeId ty = resolveType(aliasScope, *typealias.type);
    if (auto ttv = getMutable<TableType>(follow(ty)))
    {
        // If the table is already named and we want to rename the type function, we have to bind new alias to a copy
        // Additionally, we can't modify types that come from other modules
        if (ttv->name || follow(ty)->owningArena != &currentModule->internalTypes)
        {
            bool sameTys = std::equal(
                ttv->instantiatedTypeParams.begin(),
                ttv->instantiatedTypeParams.end(),
                binding->typeParams.begin(),
                binding->typeParams.end(),
                [](auto&& itp, auto&& tp)
                {
                    return itp == tp.ty;
                }
            );
            bool sameTps = std::equal(
                ttv->instantiatedTypePackParams.begin(),
                ttv->instantiatedTypePackParams.end(),
                binding->typePackParams.begin(),
                binding->typePackParams.end(),
                [](auto&& itpp, auto&& tpp)
                {
                    return itpp == tpp.tp;
                }
            );

            // Copy can be skipped if this is an identical alias
            if (!ttv->name || ttv->name != name || !sameTys || !sameTps)
            {
                // This is a shallow clone, original recursive links to self are not updated
                TableType clone = TableType{ttv->props, ttv->indexer, ttv->level, ttv->state};
                clone.definitionModuleName = ttv->definitionModuleName;
                clone.definitionLocation = ttv->definitionLocation;
                clone.name = name;

                for (auto param : binding->typeParams)
                    clone.instantiatedTypeParams.push_back(param.ty);

                for (auto param : binding->typePackParams)
                    clone.instantiatedTypePackParams.push_back(param.tp);

                ty = addType(std::move(clone));
            }
        }
        else
        {
            ttv->name = name;

            ttv->instantiatedTypeParams.clear();
            for (auto param : binding->typeParams)
                ttv->instantiatedTypeParams.push_back(param.ty);

            ttv->instantiatedTypePackParams.clear();
            for (auto param : binding->typePackParams)
                ttv->instantiatedTypePackParams.push_back(param.tp);
        }
    }
    else if (auto mtv = getMutable<MetatableType>(follow(ty)))
    {
        // We can't modify types that come from other modules
        if (follow(ty)->owningArena == &currentModule->internalTypes)
            mtv->syntheticName = name;
    }

    TypeId& bindingType = bindingsMap[name].type;

    unify(ty, bindingType, aliasScope, typealias.location);

    // It is possible for this unification to succeed but for
    // `bindingType` still to be free For example, in
    // `type T = T|T`, we generate a fresh free type `X`, and then
    // unify `X` with `X|X`, which succeeds without binding `X` to
    // anything, since `X <: X|X`
    if (bindingType->ty.get_if<FreeType>())
    {
        ty = errorRecoveryType(aliasScope);
        unify(ty, bindingType, aliasScope, typealias.location);
        reportError(TypeError{typealias.location, OccursCheckFailed{}});
    }

    bindingType = ty;
    return ControlFlow::None;
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatTypeFunction& typefunction)
{
    reportError(TypeError{typefunction.location, GenericError{"This syntax is not supported"}});

    return ControlFlow::None;
}

void TypeChecker::prototype(const ScopePtr& scope, const AstStatTypeAlias& typealias, int subLevel)
{
    Name name = typealias.name.value;

    // If the alias is missing a name, we can't do anything with it.  Ignore it.
    // Also, typeof is not a valid type alias name.  We will report an error for
    // this in check()
    if (name == kParseNameError || name == "typeof")
        return;

    std::optional<TypeFun> binding;
    if (auto it = scope->exportedTypeBindings.find(name); it != scope->exportedTypeBindings.end())
        binding = it->second;
    else if (auto it = scope->privateTypeBindings.find(name); it != scope->privateTypeBindings.end())
        binding = it->second;

    auto& bindingsMap = typealias.exported ? scope->exportedTypeBindings : scope->privateTypeBindings;

    if (binding)
    {
        Location location = scope->typeAliasLocations[name];
        reportError(TypeError{typealias.location, DuplicateTypeDefinition{name, location}});

        duplicateTypeAliases.insert({typealias.exported, name});
    }
    else
    {
        if (globalScope->builtinTypeNames.contains(name))
        {
            reportError(typealias.location, DuplicateTypeDefinition{name});
            duplicateTypeAliases.insert({typealias.exported, name});
        }
        else
        {
            ScopePtr aliasScope = childScope(scope, typealias.location);
            aliasScope->level = scope->level.incr();
            aliasScope->level.subLevel = subLevel;

            auto [generics, genericPacks] =
                createGenericTypes(aliasScope, scope->level, typealias, typealias.generics, typealias.genericPacks, /* useCache = */ true);

            TypeId ty = freshType(aliasScope);
            FreeType* ftv = getMutable<FreeType>(ty);
            LUAU_ASSERT(ftv);
            ftv->forwardedTypeAlias = true;
            bindingsMap[name] = {std::move(generics), std::move(genericPacks), ty, typealias.location};

            scope->typeAliasLocations[name] = typealias.location;
            scope->typeAliasNameLocations[name] = typealias.nameLocation;
        }
    }
}

void TypeChecker::prototype(const ScopePtr& scope, const AstStatDeclareExternType& declaredExternType)
{
    std::optional<TypeId> superTy = std::make_optional(builtinTypes->externType);
    if (declaredExternType.superName)
    {
        Name superName = Name(declaredExternType.superName->value);
        std::optional<TypeFun> lookupType = scope->lookupType(superName);

        if (!lookupType)
        {
            reportError(declaredExternType.location, UnknownSymbol{std::move(superName), UnknownSymbol::Type});
            incorrectExternTypeDefinitions.insert(&declaredExternType);
            return;
        }

        // We don't have generic extern types, so this assertion _should_ never be hit.
        LUAU_ASSERT(lookupType->typeParams.size() == 0 && lookupType->typePackParams.size() == 0);
        superTy = lookupType->type;

        if (!get<ExternType>(follow(*superTy)))
        {
            reportError(
                declaredExternType.location,
                GenericError{format("Cannot use non-class type '%s' as a superclass of class '%s'", superName.c_str(), declaredExternType.name.value)}
            );
            incorrectExternTypeDefinitions.insert(&declaredExternType);
            return;
        }
    }

    Name className(declaredExternType.name.value);

    TypeId classTy = addType(ExternType(className, {}, superTy, std::nullopt, {}, {}, currentModule->name, declaredExternType.location));
    ExternType* etv = getMutable<ExternType>(classTy);
    TypeId metaTy = addType(TableType{TableState::Sealed, scope->level});

    etv->metatable = metaTy;
    scope->exportedTypeBindings[className] = TypeFun{{}, classTy, declaredExternType.location};
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatDeclareExternType& declaredExternType)
{
    Name className(declaredExternType.name.value);

    // Don't bother checking if the class definition was incorrect
    if (incorrectExternTypeDefinitions.find(&declaredExternType))
        return ControlFlow::None;

    std::optional<TypeFun> binding;
    if (auto it = scope->exportedTypeBindings.find(className); it != scope->exportedTypeBindings.end())
        binding = it->second;

    // This extern type definition must have been `prototype()`d first.
    if (!binding)
        ice("Extern type not predeclared");

    TypeId externTy = binding->type;
    ExternType* etv = getMutable<ExternType>(externTy);

    if (!etv->metatable)
        ice("No metatable for declared extern type");

    if (const auto& indexer = declaredExternType.indexer)
        etv->indexer = TableIndexer(resolveType(scope, *indexer->indexType), resolveType(scope, *indexer->resultType));

    TableType* metatable = getMutable<TableType>(*etv->metatable);
    for (const AstDeclaredExternTypeProperty& prop : declaredExternType.props)
    {
        Name propName(prop.name.value);
        TypeId propTy = resolveType(scope, *prop.ty);

        bool assignToMetatable = isMetamethod(propName);
        Luau::ExternType::Props& assignTo = assignToMetatable ? metatable->props : etv->props;

        // Function types always take 'self', but this isn't reflected in the
        // parsed annotation. Add it here.
        if (prop.isMethod)
        {
            if (FunctionType* ftv = getMutable<FunctionType>(propTy))
            {
                ftv->argNames.insert(ftv->argNames.begin(), FunctionArgument{"self", {}});
                ftv->argTypes = addTypePack(TypePack{{externTy}, ftv->argTypes});
                ftv->hasSelf = true;

                FunctionDefinition defn;

                defn.definitionModuleName = currentModule->name;
                defn.definitionLocation = prop.location;
                // No data is preserved for varargLocation
                defn.originalNameLocation = prop.nameLocation;

                ftv->definition = defn;
            }
        }

        if (assignTo.count(propName) == 0)
        {
            assignTo[propName] = {propTy, /*deprecated*/ false, /*deprecatedSuggestion*/ "", prop.location};
        }
        else
        {
            Luau::Property& prop = assignTo[propName];
            TypeId currentTy = prop.type_DEPRECATED();

            // We special-case this logic to keep the intersection flat; otherwise we
            // would create a ton of nested intersection types.
            if (const IntersectionType* itv = get<IntersectionType>(currentTy))
            {
                std::vector<TypeId> options = itv->parts;
                options.push_back(propTy);
                TypeId newItv = addType(IntersectionType{std::move(options)});

                prop.readTy = newItv;
                prop.writeTy = newItv;
            }
            else if (get<FunctionType>(currentTy))
            {
                TypeId intersection = addType(IntersectionType{{currentTy, propTy}});

                prop.readTy = intersection;
                prop.writeTy = intersection;
            }
            else
            {
                reportError(declaredExternType.location, GenericError{format("Cannot overload non-function class member '%s'", propName.c_str())});
            }
        }
    }

    return ControlFlow::None;
}

ControlFlow TypeChecker::check(const ScopePtr& scope, const AstStatDeclareFunction& global)
{
    ScopePtr funScope = childFunctionScope(scope, global.location);

    auto [generics, genericPacks] = createGenericTypes(funScope, std::nullopt, global, global.generics, global.genericPacks);

    std::vector<TypeId> genericTys;
    genericTys.reserve(generics.size());
    std::transform(
        generics.begin(),
        generics.end(),
        std::back_inserter(genericTys),
        [](auto&& el)
        {
            return el.ty;
        }
    );

    std::vector<TypePackId> genericTps;
    genericTps.reserve(genericPacks.size());
    std::transform(
        genericPacks.begin(),
        genericPacks.end(),
        std::back_inserter(genericTps),
        [](auto&& el)
        {
            return el.tp;
        }
    );

    TypePackId argPack = resolveTypePack(funScope, global.params);
    TypePackId retPack = resolveTypePack(funScope, *global.retTypes);

    FunctionDefinition defn;

    defn.definitionModuleName = currentModule->name;
    defn.definitionLocation = global.location;
    defn.varargLocation = global.vararg ? std::make_optional(global.varargLocation) : std::nullopt;
    defn.originalNameLocation = global.nameLocation;

    TypeId fnType = addType(FunctionType{funScope->level, std::move(genericTys), std::move(genericTps), argPack, retPack, defn});
    FunctionType* ftv = getMutable<FunctionType>(fnType);

    ftv->argNames.reserve(global.paramNames.size);
    for (const auto& el : global.paramNames)
        ftv->argNames.push_back(FunctionArgument{el.first.value, el.second});

    if (FFlag::LuauParametrizedAttributeSyntax)
    {
        AstAttr* deprecatedAttr = global.getAttribute(AstAttr::Type::Deprecated);
        ftv->isDeprecatedFunction = deprecatedAttr != nullptr;
        if (deprecatedAttr)
        {
            ftv->deprecatedInfo = std::make_shared<AstAttr::DeprecatedInfo>(deprecatedAttr->deprecatedInfo());
        }
    }

    Name fnName(global.name.value);

    currentModule->declaredGlobals[fnName] = fnType;
    currentModule->getModuleScope()->bindings[global.name] = Binding{fnType, global.location};

    return ControlFlow::None;
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExpr& expr, std::optional<TypeId> expectedType, bool forceSingleton)
{
    RecursionCounter _rc(&checkRecursionCount);
    if (FInt::LuauCheckRecursionLimit > 0 && checkRecursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportErrorCodeTooComplex(expr.location);
        return WithPredicate{errorRecoveryType(scope)};
    }

    WithPredicate<TypeId> result;

    if (auto a = expr.as<AstExprGroup>())
        result = checkExpr(scope, *a->expr, expectedType);
    else if (expr.is<AstExprConstantNil>())
        result = WithPredicate{nilType};
    else if (const AstExprConstantBool* bexpr = expr.as<AstExprConstantBool>())
    {
        if (forceSingleton || (expectedType && maybeSingleton(*expectedType)))
            result = WithPredicate{singletonType(bexpr->value)};
        else
            result = WithPredicate{booleanType};
    }
    else if (const AstExprConstantString* sexpr = expr.as<AstExprConstantString>())
    {
        if (forceSingleton || (expectedType && maybeSingleton(*expectedType)))
            result = WithPredicate{singletonType(std::string(sexpr->value.data, sexpr->value.size))};
        else
            result = WithPredicate{stringType};
    }
    else if (expr.is<AstExprConstantNumber>())
        result = WithPredicate{numberType};
    else if (auto a = expr.as<AstExprLocal>())
        result = checkExpr(scope, *a);
    else if (auto a = expr.as<AstExprGlobal>())
        result = checkExpr(scope, *a);
    else if (auto a = expr.as<AstExprVarargs>())
        result = checkExpr(scope, *a);
    else if (auto a = expr.as<AstExprCall>())
        result = checkExpr(scope, *a);
    else if (auto a = expr.as<AstExprIndexName>())
        result = checkExpr(scope, *a);
    else if (auto a = expr.as<AstExprIndexExpr>())
        result = checkExpr(scope, *a);
    else if (auto a = expr.as<AstExprFunction>())
        result = checkExpr(scope, *a, expectedType);
    else if (auto a = expr.as<AstExprTable>())
        result = checkExpr(scope, *a, expectedType);
    else if (auto a = expr.as<AstExprUnary>())
        result = checkExpr(scope, *a);
    else if (auto a = expr.as<AstExprBinary>())
        result = checkExpr(scope, *a, expectedType);
    else if (auto a = expr.as<AstExprTypeAssertion>())
        result = checkExpr(scope, *a);
    else if (auto a = expr.as<AstExprError>())
        result = checkExpr(scope, *a);
    else if (auto a = expr.as<AstExprIfElse>())
        result = checkExpr(scope, *a, expectedType);
    else if (auto a = expr.as<AstExprInterpString>())
        result = checkExpr(scope, *a);
    else
        ice("Unhandled AstExpr?");

    result.type = follow(result.type);

    if (!currentModule->astTypes.find(&expr))
        currentModule->astTypes[&expr] = result.type;

    if (expectedType)
        currentModule->astExpectedTypes[&expr] = *expectedType;

    return result;
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprLocal& expr)
{
    std::optional<LValue> lvalue = tryGetLValue(expr);
    LUAU_ASSERT(lvalue); // Guaranteed to not be nullopt - AstExprLocal is an LValue.

    if (std::optional<TypeId> ty = resolveLValue(scope, *lvalue))
        return {*ty, {TruthyPredicate{std::move(*lvalue), expr.location}}};

    // TODO: tempting to ice here, but this breaks very often because our toposort doesn't enforce this constraint
    // ice("AstExprLocal exists but no binding definition for it?", expr.location);
    reportError(TypeError{expr.location, UnknownSymbol{expr.local->name.value, UnknownSymbol::Binding}});
    return WithPredicate{errorRecoveryType(scope)};
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprGlobal& expr)
{
    std::optional<LValue> lvalue = tryGetLValue(expr);
    LUAU_ASSERT(lvalue); // Guaranteed to not be nullopt - AstExprGlobal is an LValue.

    if (std::optional<TypeId> ty = resolveLValue(scope, *lvalue))
        return {*ty, {TruthyPredicate{std::move(*lvalue), expr.location}}};

    reportError(TypeError{expr.location, UnknownSymbol{expr.name.value, UnknownSymbol::Binding}});
    return WithPredicate{errorRecoveryType(scope)};
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprVarargs& expr)
{
    TypePackId varargPack = checkExprPack(scope, expr).type;

    if (get<TypePack>(varargPack))
    {
        if (std::optional<TypeId> ty = first(varargPack))
            return WithPredicate{*ty};

        return WithPredicate{nilType};
    }
    else if (get<FreeTypePack>(varargPack))
    {
        TypeId head = freshType(scope);
        TypePackId tail = freshTypePack(scope);
        *asMutable(varargPack) = TypePack{{head}, tail};
        return WithPredicate{head};
    }
    if (get<ErrorTypePack>(varargPack))
        return WithPredicate{errorRecoveryType(scope)};
    else if (auto vtp = get<VariadicTypePack>(varargPack))
        return WithPredicate{vtp->ty};
    else if (get<GenericTypePack>(varargPack))
    {
        // TODO: Better error?
        reportError(expr.location, GenericError{"Trying to get a type from a variadic type parameter"});
        return WithPredicate{errorRecoveryType(scope)};
    }
    else
        ice("Unknown TypePack type in checkExpr(AstExprVarargs)!");
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprCall& expr)
{
    WithPredicate<TypePackId> result = checkExprPack(scope, expr);
    TypePackId retPack = follow(result.type);

    if (auto pack = get<TypePack>(retPack))
    {
        return {pack->head.empty() ? nilType : pack->head[0], std::move(result.predicates)};
    }
    else if (const FreeTypePack* ftp = get<FreeTypePack>(retPack))
    {
        TypeId head = freshType(scope->level);
        TypePackId pack = addTypePack(TypePackVar{TypePack{{head}, freshTypePack(scope->level)}});
        unify(pack, retPack, scope, expr.location);
        return {head, std::move(result.predicates)};
    }
    if (get<ErrorTypePack>(retPack))
        return {errorRecoveryType(scope), std::move(result.predicates)};
    else if (auto vtp = get<VariadicTypePack>(retPack))
        return {vtp->ty, std::move(result.predicates)};
    else if (get<GenericTypePack>(retPack))
        return {anyType, std::move(result.predicates)};
    else
        ice("Unknown TypePack type!", expr.location);
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprIndexName& expr)
{
    Name name = expr.index.value;

    // Redundant call if we find a refined lvalue, but this function must be called in order to recursively populate astTypes.
    TypeId lhsType = checkExpr(scope, *expr.expr).type;

    if (std::optional<LValue> lvalue = tryGetLValue(expr))
        if (std::optional<TypeId> ty = resolveLValue(scope, *lvalue))
            return {*ty, {TruthyPredicate{std::move(*lvalue), expr.location}}};

    lhsType = stripFromNilAndReport(lhsType, expr.expr->location);

    if (std::optional<TypeId> ty = getIndexTypeFromType(scope, lhsType, name, expr.location, /* addErrors= */ true))
        return WithPredicate{*ty};

    return WithPredicate{errorRecoveryType(scope)};
}

std::optional<TypeId> TypeChecker::findTablePropertyRespectingMeta(TypeId lhsType, Name name, const Location& location, bool addErrors)
{
    ErrorVec errors;
    auto result = Luau::findTablePropertyRespectingMeta(builtinTypes, errors, lhsType, name, location);
    if (addErrors)
        reportErrors(errors);
    return result;
}

std::optional<TypeId> TypeChecker::findMetatableEntry(TypeId type, std::string entry, const Location& location, bool addErrors)
{
    ErrorVec errors;
    auto result = Luau::findMetatableEntry(builtinTypes, errors, type, entry, location);
    if (addErrors)
        reportErrors(errors);
    return result;
}

std::optional<TypeId> TypeChecker::getIndexTypeFromType(
    const ScopePtr& scope,
    TypeId type,
    const Name& name,
    const Location& location,
    bool addErrors
)
{
    size_t errorCount = currentModule->errors.size();

    std::optional<TypeId> result = getIndexTypeFromTypeImpl(scope, type, name, location, addErrors);

    if (!addErrors)
        LUAU_ASSERT(errorCount == currentModule->errors.size());

    return result;
}

std::optional<TypeId> TypeChecker::getIndexTypeFromTypeImpl(
    const ScopePtr& scope,
    TypeId type,
    const Name& name,
    const Location& location,
    bool addErrors
)
{
    type = follow(type);

    if (get<ErrorType>(type) || get<AnyType>(type) || get<NeverType>(type))
        return type;

    tablify(type);

    if (isString(type))
    {
        std::optional<TypeId> mtIndex = findMetatableEntry(stringType, "__index", location, addErrors);
        LUAU_ASSERT(mtIndex);
        type = *mtIndex;
    }

    if (TableType* tableType = getMutableTableType(type))
    {
        if (auto it = tableType->props.find(name); it != tableType->props.end())
            return it->second.type_DEPRECATED();
        else if (auto indexer = tableType->indexer)
        {
            // TODO: Property lookup should work with string singletons or unions thereof as the indexer key type.
            ErrorVec errors = tryUnify(stringType, indexer->indexType, scope, location);

            if (errors.empty())
                return indexer->indexResultType;

            if (addErrors)
                reportError(location, UnknownProperty{type, name});

            return std::nullopt;
        }
        else if (tableType->state == TableState::Free)
        {
            TypeId result = freshType(tableType->level);
            tableType->props[name] = {result};
            return result;
        }

        if (auto found = findTablePropertyRespectingMeta(type, name, location, addErrors))
            return *found;
    }
    else if (const ExternType* cls = get<ExternType>(type))
    {
        const Property* prop = lookupExternTypeProp(cls, name);
        if (prop)
            return prop->type_DEPRECATED();

        if (auto indexer = cls->indexer)
        {
            // TODO: Property lookup should work with string singletons or unions thereof as the indexer key type.
            ErrorVec errors = tryUnify(stringType, indexer->indexType, scope, location);

            if (errors.empty())
                return indexer->indexResultType;

            if (addErrors)
                reportError(location, UnknownProperty{type, name});

            return std::nullopt;
        }
    }
    else if (const UnionType* utv = get<UnionType>(type))
    {
        std::vector<TypeId> goodOptions;
        std::vector<TypeId> badOptions;

        for (TypeId t : utv)
        {
            RecursionLimiter _rl("TypeInfer::UnionType", &recursionCount, FInt::LuauTypeInferRecursionLimit);

            // Not needed when we normalize types.
            if (get<AnyType>(follow(t)))
                return t;

            if (std::optional<TypeId> ty = getIndexTypeFromType(scope, t, name, location, /* addErrors= */ false))
                goodOptions.push_back(*ty);
            else
                badOptions.push_back(t);
        }

        if (!badOptions.empty())
        {
            if (addErrors)
            {
                if (goodOptions.empty())
                    reportError(location, UnknownProperty{type, name});
                else
                    reportError(location, MissingUnionProperty{type, std::move(badOptions), name});
            }
            return std::nullopt;
        }

        std::vector<TypeId> result = reduceUnion(goodOptions);
        if (result.empty())
            return neverType;

        if (result.size() == 1)
            return result[0];

        return addType(UnionType{std::move(result)});
    }
    else if (const IntersectionType* itv = get<IntersectionType>(type))
    {
        std::vector<TypeId> parts;

        for (TypeId t : itv->parts)
        {
            RecursionLimiter _rl("TypeInfer::IntersectionType", &recursionCount, FInt::LuauTypeInferRecursionLimit);

            if (std::optional<TypeId> ty = getIndexTypeFromType(scope, t, name, location, /* addErrors= */ false))
                parts.push_back(*ty);
        }

        // If no parts of the intersection had the property we looked up for, it never existed at all.
        if (parts.empty())
        {
            if (addErrors)
                reportError(location, UnknownProperty{type, name});
            return std::nullopt;
        }

        if (parts.size() == 1)
            return parts[0];

        return addType(IntersectionType{std::move(parts)}); // Not at all correct.
    }

    if (addErrors)
        reportError(location, UnknownProperty{type, name});

    return std::nullopt;
}

std::optional<TypeId> TypeChecker::tryStripUnionFromNil(TypeId ty)
{
    if (const UnionType* utv = get<UnionType>(ty))
    {
        if (!std::any_of(begin(utv), end(utv), isNil))
            return ty;

        std::vector<TypeId> result;

        for (TypeId option : utv)
        {
            if (!isNil(option))
                result.push_back(option);
        }

        if (result.empty())
            return std::nullopt;

        return result.size() == 1 ? result[0] : addType(UnionType{std::move(result)});
    }

    return std::nullopt;
}

TypeId TypeChecker::stripFromNilAndReport(TypeId ty, const Location& location)
{
    ty = follow(ty);

    if (auto utv = get<UnionType>(ty))
    {
        if (!std::any_of(begin(utv), end(utv), isNil))
            return ty;
    }

    if (std::optional<TypeId> strippedUnion = tryStripUnionFromNil(ty))
    {
        reportError(location, OptionalValueAccess{ty});
        return follow(*strippedUnion);
    }

    return ty;
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprIndexExpr& expr)
{
    TypeId ty = checkLValue(scope, expr, ValueContext::RValue);

    if (std::optional<LValue> lvalue = tryGetLValue(expr))
        if (std::optional<TypeId> refiTy = resolveLValue(scope, *lvalue))
            return {*refiTy, {TruthyPredicate{std::move(*lvalue), expr.location}}};

    return WithPredicate{ty};
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprFunction& expr, std::optional<TypeId> expectedType)
{
    auto [funTy, funScope] = checkFunctionSignature(scope, 0, expr, std::nullopt, std::nullopt, expectedType);

    checkFunctionBody(funScope, funTy, expr);

    return WithPredicate{quantify(funScope, funTy, expr.location)};
}

TypeId TypeChecker::checkExprTable(
    const ScopePtr& scope,
    const AstExprTable& expr,
    const std::vector<std::pair<TypeId, TypeId>>& fieldTypes,
    std::optional<TypeId> expectedType
)
{
    TableType::Props props;
    std::optional<TableIndexer> indexer;

    const TableType* expectedTable = nullptr;

    if (expectedType)
    {
        if (auto ttv = get<TableType>(follow(*expectedType)))
        {
            if (ttv->state == TableState::Sealed)
                expectedTable = ttv;
        }
    }

    for (size_t i = 0; i < expr.items.size; ++i)
    {
        const AstExprTable::Item& item = expr.items.data[i];

        AstExpr* k = item.key;
        AstExpr* value = item.value;

        auto [keyType, valueType] = fieldTypes[i];

        if (item.kind == AstExprTable::Item::List)
        {
            if (expectedTable && !indexer)
                indexer = expectedTable->indexer;

            if (indexer)
            {
                unify(numberType, indexer->indexType, scope, value->location);
                unify(valueType, indexer->indexResultType, scope, value->location);
            }
            else
                indexer = TableIndexer{numberType, anyIfNonstrict(valueType)};
        }
        else if (item.kind == AstExprTable::Item::Record || item.kind == AstExprTable::Item::General)
        {
            if (auto key = k->as<AstExprConstantString>())
            {
                TypeId exprType = follow(valueType);
                if (isNonstrictMode() && !getTableType(exprType) && !get<FunctionType>(exprType))
                    exprType = anyType;

                if (expectedTable)
                {
                    auto it = expectedTable->props.find(key->value.data);
                    if (it != expectedTable->props.end())
                    {
                        Property expectedProp = it->second;
                        ErrorVec errors = tryUnify(exprType, expectedProp.type_DEPRECATED(), scope, k->location);
                        if (errors.empty())
                            exprType = expectedProp.type_DEPRECATED();
                    }
                    else if (expectedTable->indexer && maybeString(expectedTable->indexer->indexType))
                    {
                        ErrorVec errors = tryUnify(exprType, expectedTable->indexer->indexResultType, scope, k->location);
                        if (errors.empty())
                            exprType = expectedTable->indexer->indexResultType;
                    }
                }

                props[key->value.data] = {exprType, /* deprecated */ false, {}, k->location};
            }
            else
            {
                if (expectedTable && !indexer)
                    indexer = expectedTable->indexer;

                if (indexer)
                {
                    unify(keyType, indexer->indexType, scope, k->location);
                    unify(valueType, indexer->indexResultType, scope, value->location);
                }
                else if (isNonstrictMode())
                {
                    indexer = TableIndexer{anyType, anyType};
                }
                else
                {
                    indexer = TableIndexer{keyType, valueType};
                }
            }
        }
    }

    TableState state = TableState::Unsealed;
    TableType table = TableType{std::move(props), indexer, scope->level, state};
    table.definitionModuleName = currentModule->name;
    table.definitionLocation = expr.location;
    return addType(table);
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprTable& expr, std::optional<TypeId> expectedType)
{
    RecursionCounter _rc(&checkRecursionCount);
    if (FInt::LuauCheckRecursionLimit > 0 && checkRecursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportErrorCodeTooComplex(expr.location);
        return WithPredicate{errorRecoveryType(scope)};
    }

    std::vector<std::pair<TypeId, TypeId>> fieldTypes(expr.items.size);

    const TableType* expectedTable = nullptr;
    const UnionType* expectedUnion = nullptr;
    std::optional<TypeId> expectedIndexType;
    std::optional<TypeId> expectedIndexResultType;

    if (expectedType)
    {
        if (auto ttv = get<TableType>(follow(*expectedType)))
        {
            if (ttv->state == TableState::Sealed)
            {
                expectedTable = ttv;

                if (ttv->indexer)
                {
                    expectedIndexType = ttv->indexer->indexType;
                    expectedIndexResultType = ttv->indexer->indexResultType;
                }
            }
        }
        else if (const UnionType* utv = get<UnionType>(follow(*expectedType)))
            expectedUnion = utv;
    }

    for (size_t i = 0; i < expr.items.size; ++i)
    {
        AstExprTable::Item& item = expr.items.data[i];
        std::optional<TypeId> expectedResultType;
        bool isIndexedItem = false;

        if (item.kind == AstExprTable::Item::List)
        {
            expectedResultType = expectedIndexResultType;
            isIndexedItem = true;
        }
        else if (item.kind == AstExprTable::Item::Record || item.kind == AstExprTable::Item::General)
        {
            if (auto key = item.key->as<AstExprConstantString>())
            {
                if (expectedTable)
                {
                    if (auto prop = expectedTable->props.find(key->value.data); prop != expectedTable->props.end())
                        expectedResultType = prop->second.type_DEPRECATED();
                    else if (expectedIndexType && maybeString(*expectedIndexType))
                        expectedResultType = expectedIndexResultType;
                }
                else if (expectedUnion)
                {
                    std::vector<TypeId> expectedResultTypes;
                    for (TypeId expectedOption : expectedUnion)
                    {
                        if (const TableType* ttv = get<TableType>(follow(expectedOption)))
                        {
                            if (auto prop = ttv->props.find(key->value.data); prop != ttv->props.end())
                                expectedResultTypes.push_back(prop->second.type_DEPRECATED());
                            else if (ttv->indexer && maybeString(ttv->indexer->indexType))
                                expectedResultTypes.push_back(ttv->indexer->indexResultType);
                        }
                    }

                    if (expectedResultTypes.size() == 1)
                        expectedResultType = expectedResultTypes[0];
                    else if (expectedResultTypes.size() > 1)
                        expectedResultType = addType(UnionType{expectedResultTypes});
                }
            }
            else
            {
                expectedResultType = expectedIndexResultType;
                isIndexedItem = true;
            }
        }

        fieldTypes[i].first = item.key ? checkExpr(scope, *item.key, expectedIndexType).type : nullptr;
        fieldTypes[i].second = checkExpr(scope, *item.value, expectedResultType).type;

        // Indexer keys after the first are unified with the first one
        // If we don't have an expected indexer type yet, take this first item type
        if (isIndexedItem && !expectedIndexResultType)
            expectedIndexResultType = fieldTypes[i].second;
    }

    return WithPredicate{checkExprTable(scope, expr, fieldTypes, expectedType)};
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprUnary& expr)
{
    WithPredicate<TypeId> result = checkExpr(scope, *expr.expr);
    TypeId operandType = follow(result.type);

    switch (expr.op)
    {
    case AstExprUnary::Not:
        return {booleanType, {NotPredicate{std::move(result.predicates)}}};
    case AstExprUnary::Minus:
    {
        const bool operandIsAny = get<AnyType>(operandType) || get<ErrorType>(operandType) || get<NeverType>(operandType);

        if (operandIsAny)
            return WithPredicate{operandType};

        if (typeCouldHaveMetatable(operandType))
        {
            if (auto fnt = findMetatableEntry(operandType, "__unm", expr.location, /* addErrors= */ true))
            {
                TypeId actualFunctionType = instantiate(scope, *fnt, expr.location);
                TypePackId arguments = addTypePack({operandType});
                TypePackId retTypePack = freshTypePack(scope);
                TypeId expectedFunctionType = addType(FunctionType(scope->level, arguments, retTypePack));

                Unifier state = mkUnifier(scope, expr.location);
                state.tryUnify(actualFunctionType, expectedFunctionType, /*isFunctionCall*/ true);
                state.log.commit();

                reportErrors(state.errors);

                TypeId retType = first(retTypePack).value_or(nilType);
                if (!state.errors.empty())
                    retType = errorRecoveryType(retType);

                return WithPredicate{retType};
            }

            reportError(
                expr.location,
                GenericError{format("Unary operator '%s' not supported by type '%s'", toString(expr.op).c_str(), toString(operandType).c_str())}
            );
            return WithPredicate{errorRecoveryType(scope)};
        }

        reportErrors(tryUnify(operandType, numberType, scope, expr.location));
        return WithPredicate{numberType};
    }
    case AstExprUnary::Len:
    {
        tablify(operandType);

        operandType = stripFromNilAndReport(operandType, expr.location);

        // # operator is guaranteed to return number
        if (get<AnyType>(operandType) || get<ErrorType>(operandType) || get<NeverType>(operandType))
            return WithPredicate{numberType};

        DenseHashSet<TypeId> seen{nullptr};

        if (typeCouldHaveMetatable(operandType))
        {
            if (auto fnt = findMetatableEntry(operandType, "__len", expr.location, /* addErrors= */ true))
            {
                TypeId actualFunctionType = instantiate(scope, *fnt, expr.location);
                TypePackId arguments = addTypePack({operandType});
                TypePackId retTypePack = addTypePack({numberType});
                TypeId expectedFunctionType = addType(FunctionType(scope->level, arguments, retTypePack));

                Unifier state = mkUnifier(scope, expr.location);
                state.tryUnify(actualFunctionType, expectedFunctionType, /*isFunctionCall*/ true);
                state.log.commit();

                reportErrors(state.errors);
            }
        }

        if (!hasLength(operandType, seen, &recursionCount))
            reportError(TypeError{expr.location, NotATable{operandType}});

        return WithPredicate{numberType};
    }
    default:
        ice("Unknown AstExprUnary " + std::to_string(int(expr.op)));
    }
}

std::string opToMetaTableEntry(const AstExprBinary::Op& op)
{
    switch (op)
    {
    case AstExprBinary::CompareNe:
    case AstExprBinary::CompareEq:
        return "__eq";
    case AstExprBinary::CompareLt:
    case AstExprBinary::CompareGe:
        return "__lt";
    case AstExprBinary::CompareLe:
    case AstExprBinary::CompareGt:
        return "__le";
    case AstExprBinary::Add:
        return "__add";
    case AstExprBinary::Sub:
        return "__sub";
    case AstExprBinary::Mul:
        return "__mul";
    case AstExprBinary::Div:
        return "__div";
    case AstExprBinary::FloorDiv:
        return "__idiv";
    case AstExprBinary::Mod:
        return "__mod";
    case AstExprBinary::Pow:
        return "__pow";
    case AstExprBinary::Concat:
        return "__concat";
    default:
        return "";
    }
}

TypeId TypeChecker::unionOfTypes(TypeId a, TypeId b, const ScopePtr& scope, const Location& location, bool unifyFreeTypes)
{
    a = follow(a);
    b = follow(b);

    if (unifyFreeTypes && (get<FreeType>(a) || get<FreeType>(b)))
    {
        if (unify(b, a, scope, location))
            return a;

        return errorRecoveryType(anyType);
    }

    if (*a == *b)
        return a;

    std::vector<TypeId> types = reduceUnion({a, b});
    if (types.empty())
        return neverType;

    if (types.size() == 1)
        return types[0];

    return addType(UnionType{std::move(types)});
}

static std::optional<std::string> getIdentifierOfBaseVar(AstExpr* node)
{
    if (AstExprGlobal* expr = node->as<AstExprGlobal>())
        return expr->name.value;

    if (AstExprLocal* expr = node->as<AstExprLocal>())
        return expr->local->name.value;

    if (AstExprIndexExpr* expr = node->as<AstExprIndexExpr>())
        return getIdentifierOfBaseVar(expr->expr);

    if (AstExprIndexName* expr = node->as<AstExprIndexName>())
        return getIdentifierOfBaseVar(expr->expr);

    return std::nullopt;
}

/** Return true if comparison between the types a and b should be permitted with
 * the == or ~= operators.
 *
 * Two types are considered eligible for equality testing if it is possible for
 * the test to ever succeed.  In other words, we test to see whether the two
 * types have any overlap at all.
 *
 * In order to make things work smoothly with the greedy solver, this function
 * exempts any and FreeTypes from this requirement.
 *
 * This function does not (yet?) take into account extra Lua restrictions like
 * that two tables can only be compared if they have the same metatable.  That
 * is presently handled by the caller.
 *
 * @return True if the types are comparable.  False if they are not.
 *
 * If an internal recursion limit is reached while performing this test, the
 * function returns std::nullopt.
 */
static std::optional<bool> areEqComparable(NotNull<TypeArena> arena, NotNull<Normalizer> normalizer, TypeId a, TypeId b)
{
    a = follow(a);
    b = follow(b);

    auto isExempt = [](TypeId t)
    {
        return isNil(t) || get<FreeType>(t);
    };

    if (isExempt(a) || isExempt(b))
        return true;

    NormalizationResult nr;

    TypeId c = arena->addType(IntersectionType{{a, b}});
    std::shared_ptr<const NormalizedType> n = normalizer->normalize(c);
    if (!n)
        return std::nullopt;

    nr = normalizer->isInhabited(n.get());

    switch (nr)
    {
    case NormalizationResult::HitLimits:
        return std::nullopt;
    case NormalizationResult::False:
        return false;
    case NormalizationResult::True:
        return true;
    }

    // n.b. msvc can never figure this stuff out.
    LUAU_UNREACHABLE();
}

TypeId TypeChecker::checkRelationalOperation(
    const ScopePtr& scope,
    const AstExprBinary& expr,
    TypeId lhsType,
    TypeId rhsType,
    const PredicateVec& predicates
)
{
    auto stripNil = [this](TypeId ty, bool isOrOp = false)
    {
        ty = follow(ty);
        if (!isNonstrictMode() && !isOrOp)
            return ty;

        if (get<UnionType>(ty))
        {
            std::optional<TypeId> cleaned = tryStripUnionFromNil(ty);

            // If there is no union option without 'nil'
            if (!cleaned)
                return nilType;

            return follow(*cleaned);
        }

        return follow(ty);
    };

    bool isEquality = expr.op == AstExprBinary::CompareEq || expr.op == AstExprBinary::CompareNe;

    lhsType = stripNil(lhsType, expr.op == AstExprBinary::Or);
    rhsType = stripNil(rhsType);

    // If we know nothing at all about the lhs type, we can usually say nothing about the result.
    // The notable exception to this is the equality and inequality operators, which always produce a boolean.
    const bool lhsIsAny = get<AnyType>(lhsType) || get<ErrorType>(lhsType) || get<NeverType>(lhsType);

    // Peephole check for `cond and a or b -> type(a)|type(b)`
    // TODO: Kill this when singleton types arrive. :(
    if (AstExprBinary* subexp = expr.left->as<AstExprBinary>())
    {
        if (expr.op == AstExprBinary::Or && subexp->op == AstExprBinary::And)
        {
            ScopePtr subScope = childScope(scope, subexp->location);
            resolve(predicates, subScope, true);
            return unionOfTypes(rhsType, stripNil(checkExpr(subScope, *subexp->right).type, true), subScope, expr.location);
        }
    }

    // Lua casts the results of these to boolean
    switch (expr.op)
    {
    case AstExprBinary::CompareNe:
    case AstExprBinary::CompareEq:
    {
        if (isNonstrictMode() && (isNil(lhsType) || isNil(rhsType)))
            return booleanType;

        const bool rhsIsAny = get<AnyType>(rhsType) || get<ErrorType>(rhsType) || get<NeverType>(rhsType);
        if (lhsIsAny || rhsIsAny)
            return booleanType;

        [[fallthrough]];
    }
    case AstExprBinary::CompareLt:
    case AstExprBinary::CompareGt:
    case AstExprBinary::CompareGe:
    case AstExprBinary::CompareLe:
    {
        // If one of the operand is never, it doesn't make sense to unify these.
        if (get<NeverType>(lhsType) || get<NeverType>(rhsType))
            return booleanType;

        if (isEquality)
        {
            // Unless either type is free or any, an equality comparison is only
            // valid when the intersection of the two operands is non-empty.
            //
            // eg it is okay to compare string? == number? because the two types
            // have nil in common, but string == number is not allowed.
            std::optional<bool> eqTestResult = areEqComparable(NotNull{&currentModule->internalTypes}, NotNull{&normalizer}, lhsType, rhsType);
            if (!eqTestResult)
            {
                reportErrorCodeTooComplex(expr.location);
                return errorRecoveryType(booleanType);
            }

            if (!*eqTestResult)
            {
                reportError(
                    expr.location, GenericError{format("Type %s cannot be compared with %s", toString(lhsType).c_str(), toString(rhsType).c_str())}
                );
                return errorRecoveryType(booleanType);
            }
        }

        /* Subtlety here:
         * We need to do this unification first, but there are situations where we don't actually want to
         * report any problems that might have been surfaced as a result of this step because we might already
         * have a better, more descriptive error teed up.
         */
        Unifier state = mkUnifier(scope, expr.location);
        if (!isEquality)
        {
            state.tryUnify(rhsType, lhsType);
            state.log.commit();
        }

        const bool needsMetamethod = !isEquality;

        TypeId leftType = follow(lhsType);
        if (get<PrimitiveType>(leftType) || get<AnyType>(leftType) || get<ErrorType>(leftType) || get<UnionType>(leftType))
        {
            reportErrors(state.errors);

            // The original version of this check also produced this error when we had a union type.
            // However, the old solver does not readily have the ability to discern if the union is comparable.
            // This is the case when the lhs is e.g. a union of singletons and the rhs is the combined type.
            // The new solver has much more powerful logic for resolving relational operators, but for now,
            // we need to be conservative in the old solver to deliver a reasonable developer experience.
            if (!isEquality && state.errors.empty() && isBoolean(leftType))
            {
                reportError(
                    expr.location,
                    GenericError{
                        format("Type '%s' cannot be compared with relational operator %s", toString(leftType).c_str(), toString(expr.op).c_str())
                    }
                );
            }

            return booleanType;
        }

        std::string metamethodName = opToMetaTableEntry(expr.op);

        std::optional<TypeId> stringNoMT = std::nullopt; // works around gcc false positive "maybe uninitialized" warnings
        std::optional<TypeId> leftMetatable = isString(lhsType) ? stringNoMT : getMetatable(follow(lhsType), builtinTypes);
        std::optional<TypeId> rightMetatable = isString(rhsType) ? stringNoMT : getMetatable(follow(rhsType), builtinTypes);

        if (leftMetatable != rightMetatable)
        {
            bool matches = false;
            if (isEquality)
            {
                if (const UnionType* utv = get<UnionType>(leftType); utv && rightMetatable)
                {
                    for (TypeId leftOption : utv)
                    {
                        if (getMetatable(follow(leftOption), builtinTypes) == rightMetatable)
                        {
                            matches = true;
                            break;
                        }
                    }
                }

                if (!matches)
                {
                    if (const UnionType* utv = get<UnionType>(rhsType); utv && leftMetatable)
                    {
                        for (TypeId rightOption : utv)
                        {
                            if (getMetatable(follow(rightOption), builtinTypes) == leftMetatable)
                            {
                                matches = true;
                                break;
                            }
                        }
                    }
                }
            }

            if (!matches)
            {
                reportError(
                    expr.location,
                    GenericError{format(
                        "Types %s and %s cannot be compared with %s because they do not have the same metatable",
                        toString(lhsType).c_str(),
                        toString(rhsType).c_str(),
                        toString(expr.op).c_str()
                    )}
                );
                return errorRecoveryType(booleanType);
            }
        }

        if (leftMetatable)
        {
            std::optional<TypeId> metamethod = findMetatableEntry(lhsType, metamethodName, expr.location, /* addErrors= */ true);
            if (metamethod)
            {
                if (const FunctionType* ftv = get<FunctionType>(follow(*metamethod)))
                {
                    if (isEquality)
                    {
                        Unifier state = mkUnifier(scope, expr.location);
                        state.tryUnify(addTypePack({booleanType}), ftv->retTypes);

                        if (!state.errors.empty())
                        {
                            reportError(expr.location, GenericError{format("Metamethod '%s' must return type 'boolean'", metamethodName.c_str())});
                            return errorRecoveryType(booleanType);
                        }

                        state.log.commit();
                    }
                }

                reportErrors(state.errors);

                TypeId actualFunctionType = addType(FunctionType(scope->level, addTypePack({lhsType, rhsType}), addTypePack({booleanType})));
                state.tryUnify(
                    instantiate(scope, actualFunctionType, expr.location), instantiate(scope, *metamethod, expr.location), /*isFunctionCall*/ true
                );

                state.log.commit();

                reportErrors(state.errors);
                return booleanType;
            }
            else if (needsMetamethod)
            {
                reportError(
                    expr.location, GenericError{format("Table %s does not offer metamethod %s", toString(lhsType).c_str(), metamethodName.c_str())}
                );
                return errorRecoveryType(booleanType);
            }
        }

        if (get<FreeType>(follow(lhsType)) && !isEquality)
        {
            auto name = getIdentifierOfBaseVar(expr.left);
            reportError(expr.location, CannotInferBinaryOperation{expr.op, std::move(name), CannotInferBinaryOperation::Comparison});
            return errorRecoveryType(booleanType);
        }

        if (needsMetamethod)
        {
            reportError(
                expr.location,
                GenericError{
                    format("Type %s cannot be compared with %s because it has no metatable", toString(lhsType).c_str(), toString(expr.op).c_str())
                }
            );
            return errorRecoveryType(booleanType);
        }

        return booleanType;
    }

    case AstExprBinary::And:
        if (lhsIsAny)
        {
            return lhsType;
        }
        else
        {
            // If lhs is free, we can't tell which 'falsy' components it has, if any
            if (get<FreeType>(lhsType))
                return unionOfTypes(addType(UnionType{{nilType, singletonType(false)}}), rhsType, scope, expr.location, false);

            auto [oty, notNever] = pickTypesFromSense(lhsType, false, neverType); // Filter out falsy types

            if (notNever)
            {
                LUAU_ASSERT(oty);

                // Perform a limited form of type reduction for booleans
                if (isPrim(*oty, PrimitiveType::Boolean) && get<BooleanSingleton>(get<SingletonType>(follow(rhsType))))
                    return booleanType;
                if (isPrim(rhsType, PrimitiveType::Boolean) && get<BooleanSingleton>(get<SingletonType>(follow(*oty))))
                    return booleanType;

                return unionOfTypes(*oty, rhsType, scope, expr.location, false);
            }
            else
            {
                return rhsType;
            }
        }
    case AstExprBinary::Or:
        if (lhsIsAny)
        {
            return lhsType;
        }
        else
        {
            auto [oty, notNever] = pickTypesFromSense(lhsType, true, neverType); // Filter out truthy types

            if (notNever)
            {
                LUAU_ASSERT(oty);

                // Perform a limited form of type reduction for booleans
                if (isPrim(*oty, PrimitiveType::Boolean) && get<BooleanSingleton>(get<SingletonType>(follow(rhsType))))
                    return booleanType;
                if (isPrim(rhsType, PrimitiveType::Boolean) && get<BooleanSingleton>(get<SingletonType>(follow(*oty))))
                    return booleanType;

                return unionOfTypes(*oty, rhsType, scope, expr.location);
            }
            else
            {
                return rhsType;
            }
        }
    default:
        LUAU_ASSERT(0);
        ice(format("checkRelationalOperation called with incorrect binary expression '%s'", toString(expr.op).c_str()), expr.location);
    }
}

TypeId TypeChecker::checkBinaryOperation(
    const ScopePtr& scope,
    const AstExprBinary& expr,
    TypeId lhsType,
    TypeId rhsType,
    const PredicateVec& predicates
)
{
    switch (expr.op)
    {
    case AstExprBinary::CompareNe:
    case AstExprBinary::CompareEq:
    case AstExprBinary::CompareLt:
    case AstExprBinary::CompareGt:
    case AstExprBinary::CompareGe:
    case AstExprBinary::CompareLe:
    case AstExprBinary::And:
    case AstExprBinary::Or:
        return checkRelationalOperation(scope, expr, lhsType, rhsType, predicates);
    default:
        break;
    }

    lhsType = follow(lhsType);
    rhsType = follow(rhsType);

    if (!isNonstrictMode() && get<FreeType>(lhsType))
    {
        auto name = getIdentifierOfBaseVar(expr.left);
        reportError(expr.location, CannotInferBinaryOperation{expr.op, std::move(name), CannotInferBinaryOperation::Operation});
        // We will fall-through to the `return anyType` check below.
    }

    // If we know nothing at all about the lhs type, we can usually say nothing about the result.
    // The notable exception to this is the equality and inequality operators, which always produce a boolean.
    const bool lhsIsAny = get<AnyType>(lhsType) || get<ErrorType>(lhsType) || get<NeverType>(lhsType);
    const bool rhsIsAny = get<AnyType>(rhsType) || get<ErrorType>(rhsType) || get<NeverType>(rhsType);

    if (lhsIsAny)
        return lhsType;
    if (rhsIsAny)
        return rhsType;

    if (get<FreeType>(lhsType))
    {
        // Inferring this accurately will get a bit weird.
        // If the lhs type is not known, it could be assumed that it is a table or class that has a metatable
        // that defines the required method, but we don't know which.
        // For now, we'll give up and hope for the best.
        return anyType;
    }

    if (get<FreeType>(rhsType))
        unify(rhsType, lhsType, scope, expr.location);

    if (typeCouldHaveMetatable(lhsType) || typeCouldHaveMetatable(rhsType))
    {
        auto checkMetatableCall = [this, &scope, &expr](TypeId fnt, TypeId lhst, TypeId rhst) -> TypeId
        {
            TypeId actualFunctionType = instantiate(scope, fnt, expr.location);
            TypePackId arguments = addTypePack({lhst, rhst});
            TypePackId retTypePack = freshTypePack(scope);
            TypeId expectedFunctionType = addType(FunctionType(scope->level, arguments, retTypePack));

            Unifier state = mkUnifier(scope, expr.location);
            state.tryUnify(actualFunctionType, expectedFunctionType, /*isFunctionCall*/ true);

            reportErrors(state.errors);
            bool hasErrors = !state.errors.empty();

            if (hasErrors)
            {
                // If there are unification errors, the return type may still be unknown
                // so we loosen the argument types to see if that helps.
                TypePackId fallbackArguments = freshTypePack(scope);
                TypeId fallbackFunctionType = addType(FunctionType(scope->level, fallbackArguments, retTypePack));
                state.errors.clear();
                state.log.clear();

                state.tryUnify(actualFunctionType, fallbackFunctionType, /*isFunctionCall*/ true);

                if (state.errors.empty())
                    state.log.commit();
            }
            else
            {
                state.log.commit();
            }

            TypeId retType = first(retTypePack).value_or(nilType);
            if (hasErrors)
                retType = errorRecoveryType(retType);

            return retType;
        };

        std::string op = opToMetaTableEntry(expr.op);
        if (auto fnt = findMetatableEntry(lhsType, op, expr.location, /* addErrors= */ true))
            return checkMetatableCall(*fnt, lhsType, rhsType);
        if (auto fnt = findMetatableEntry(rhsType, std::move(op), expr.location, /* addErrors= */ true))
        {
            // Note the intentionally reversed arguments here.
            return checkMetatableCall(*fnt, rhsType, lhsType);
        }

        reportError(
            expr.location,
            GenericError{format(
                "Binary operator '%s' not supported by types '%s' and '%s'",
                toString(expr.op).c_str(),
                toString(lhsType).c_str(),
                toString(rhsType).c_str()
            )}
        );

        return errorRecoveryType(scope);
    }

    switch (expr.op)
    {
    case AstExprBinary::Concat:
        reportErrors(tryUnify(lhsType, addType(UnionType{{stringType, numberType}}), scope, expr.left->location));
        reportErrors(tryUnify(rhsType, addType(UnionType{{stringType, numberType}}), scope, expr.right->location));
        return stringType;
    case AstExprBinary::Add:
    case AstExprBinary::Sub:
    case AstExprBinary::Mul:
    case AstExprBinary::Div:
    case AstExprBinary::FloorDiv:
    case AstExprBinary::Mod:
    case AstExprBinary::Pow:
        reportErrors(tryUnify(lhsType, numberType, scope, expr.left->location));
        reportErrors(tryUnify(rhsType, numberType, scope, expr.right->location));
        return numberType;
    default:
        // These should have been handled with checkRelationalOperation
        LUAU_ASSERT(0);
        return anyType;
    }
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprBinary& expr, std::optional<TypeId> expectedType)
{
    if (expr.op == AstExprBinary::And)
    {
        auto [lhsTy, lhsPredicates] = checkExpr(scope, *expr.left, expectedType);

        ScopePtr innerScope = childScope(scope, expr.location);
        resolve(lhsPredicates, innerScope, true);

        auto [rhsTy, rhsPredicates] = checkExpr(innerScope, *expr.right, expectedType);

        return {checkBinaryOperation(scope, expr, lhsTy, rhsTy), {AndPredicate{std::move(lhsPredicates), std::move(rhsPredicates)}}};
    }
    else if (expr.op == AstExprBinary::Or)
    {
        auto [lhsTy, lhsPredicates] = checkExpr(scope, *expr.left, expectedType);

        ScopePtr innerScope = childScope(scope, expr.location);
        resolve(lhsPredicates, innerScope, false);

        auto [rhsTy, rhsPredicates] = checkExpr(innerScope, *expr.right, expectedType);

        // Because of C++, I'm not sure if lhsPredicates was not moved out by the time we call checkBinaryOperation.
        TypeId result = checkBinaryOperation(scope, expr, lhsTy, rhsTy, lhsPredicates);
        return {result, {OrPredicate{std::move(lhsPredicates), std::move(rhsPredicates)}}};
    }
    else if (expr.op == AstExprBinary::CompareEq || expr.op == AstExprBinary::CompareNe)
    {
        // Defer the stack allocation of lhs, predicate etc until this lambda is called.
        auto checkExprOr = [&]() -> WithPredicate<TypeId>
        {
            // For these, passing expectedType is worse than simply forcing them, because their implementation
            // may inadvertently check if expectedTypes exist first and use it, instead of forceSingleton first.
            WithPredicate<TypeId> lhs = checkExpr(scope, *expr.left, std::nullopt, /*forceSingleton=*/true);
            WithPredicate<TypeId> rhs = checkExpr(scope, *expr.right, std::nullopt, /*forceSingleton=*/true);

            if (auto predicate = tryGetTypeGuardPredicate(expr))
                return {booleanType, {std::move(*predicate)}};

            PredicateVec predicates;

            if (auto lvalue = tryGetLValue(*expr.left))
                predicates.emplace_back(EqPredicate{std::move(*lvalue), rhs.type, expr.location});

            if (auto lvalue = tryGetLValue(*expr.right))
                predicates.emplace_back(EqPredicate{std::move(*lvalue), lhs.type, expr.location});

            if (!predicates.empty() && expr.op == AstExprBinary::CompareNe)
                predicates = {NotPredicate{std::move(predicates)}};

            return {checkBinaryOperation(scope, expr, lhs.type, rhs.type), std::move(predicates)};
        };
        return checkExprOr();
    }
    else
    {
        // Expected types are not useful for other binary operators.
        WithPredicate<TypeId> lhs = checkExpr(scope, *expr.left);
        WithPredicate<TypeId> rhs = checkExpr(scope, *expr.right);

        // Intentionally discarding predicates with other operators.
        return WithPredicate{checkBinaryOperation(scope, expr, lhs.type, rhs.type, lhs.predicates)};
    }
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprTypeAssertion& expr)
{
    TypeId annotationType = resolveType(scope, *expr.annotation);
    WithPredicate<TypeId> result = checkExpr(scope, *expr.expr, annotationType);

    // Note: As an optimization, we try 'number <: number | string' first, as that is the more likely case.
    if (canUnify(annotationType, result.type, scope, expr.location).empty())
        return {annotationType, std::move(result.predicates)};

    if (canUnify(result.type, annotationType, scope, expr.location).empty())
        return {annotationType, std::move(result.predicates)};

    reportError(expr.location, TypesAreUnrelated{result.type, annotationType});
    return {errorRecoveryType(annotationType), std::move(result.predicates)};
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprError& expr)
{
    const size_t oldSize = currentModule->errors.size();

    for (AstExpr* expr : expr.expressions)
        checkExpr(scope, *expr);

    // HACK: We want to check the contents of the AstExprError, but
    // any type errors that may arise from it are going to be useless.
    currentModule->errors.resize(oldSize);

    return WithPredicate{errorRecoveryType(scope)};
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprIfElse& expr, std::optional<TypeId> expectedType)
{
    WithPredicate<TypeId> result = checkExpr(scope, *expr.condition);

    ScopePtr trueScope = childScope(scope, expr.trueExpr->location);
    resolve(result.predicates, trueScope, true);
    WithPredicate<TypeId> trueType = checkExpr(trueScope, *expr.trueExpr, expectedType);

    ScopePtr falseScope = childScope(scope, expr.falseExpr->location);
    resolve(result.predicates, falseScope, false);
    WithPredicate<TypeId> falseType = checkExpr(falseScope, *expr.falseExpr, expectedType);

    if (falseType.type == trueType.type)
        return WithPredicate{trueType.type};

    std::vector<TypeId> types = reduceUnion({trueType.type, falseType.type});
    if (types.empty())
        return WithPredicate{neverType};
    return WithPredicate{types.size() == 1 ? types[0] : addType(UnionType{std::move(types)})};
}

WithPredicate<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprInterpString& expr)
{
    for (AstExpr* expr : expr.expressions)
        checkExpr(scope, *expr);

    return WithPredicate{stringType};
}

TypeId TypeChecker::checkLValue(const ScopePtr& scope, const AstExpr& expr, ValueContext ctx)
{
    return checkLValueBinding(scope, expr, ctx);
}

TypeId TypeChecker::checkLValueBinding(const ScopePtr& scope, const AstExpr& expr, ValueContext ctx)
{
    if (auto a = expr.as<AstExprLocal>())
        return checkLValueBinding(scope, *a);
    else if (auto a = expr.as<AstExprGlobal>())
        return checkLValueBinding(scope, *a);
    else if (auto a = expr.as<AstExprIndexName>())
        return checkLValueBinding(scope, *a, ctx);
    else if (auto a = expr.as<AstExprIndexExpr>())
        return checkLValueBinding(scope, *a, ctx);
    else if (auto a = expr.as<AstExprError>())
    {
        for (AstExpr* expr : a->expressions)
            checkExpr(scope, *expr);
        return errorRecoveryType(scope);
    }
    else
        ice("Unexpected AST node in checkLValue", expr.location);
}

TypeId TypeChecker::checkLValueBinding(const ScopePtr& scope, const AstExprLocal& expr)
{
    if (std::optional<TypeId> ty = scope->lookup(expr.local))
    {
        ty = follow(*ty);
        return get<NeverType>(*ty) ? unknownType : *ty;
    }

    reportError(expr.location, UnknownSymbol{expr.local->name.value, UnknownSymbol::Binding});
    return errorRecoveryType(scope);
}

TypeId TypeChecker::checkLValueBinding(const ScopePtr& scope, const AstExprGlobal& expr)
{
    Name name = expr.name.value;
    ScopePtr moduleScope = currentModule->getModuleScope();

    const auto it = moduleScope->bindings.find(expr.name);

    if (it != moduleScope->bindings.end())
    {
        TypeId ty = follow(it->second.typeId);
        return get<NeverType>(ty) ? unknownType : ty;
    }

    TypeId result = freshType(scope);
    Binding& binding = moduleScope->bindings[expr.name];
    binding = {result, expr.location};

    // If we're in strict mode, we want to report defining a global as an error,
    // but still add it to the bindings, so that autocomplete includes it in completions.
    if (!isNonstrictMode())
        reportError(TypeError{expr.location, UnknownSymbol{std::move(name), UnknownSymbol::Binding}});

    return result;
}

TypeId TypeChecker::checkLValueBinding(const ScopePtr& scope, const AstExprIndexName& expr, ValueContext ctx)
{
    TypeId lhs = checkExpr(scope, *expr.expr).type;

    if (get<ErrorType>(lhs) || get<AnyType>(lhs))
        return lhs;

    if (get<NeverType>(lhs))
        return unknownType;

    tablify(lhs);

    Name name = expr.index.value;

    lhs = stripFromNilAndReport(lhs, expr.expr->location);

    if (TableType* lhsTable = getMutableTableType(lhs))
    {
        const auto& it = lhsTable->props.find(name);
        if (it != lhsTable->props.end())
        {
            return it->second.type_DEPRECATED();
        }
        else if ((ctx == ValueContext::LValue && lhsTable->state == TableState::Unsealed) || lhsTable->state == TableState::Free)
        {
            TypeId theType = freshType(scope);
            Property& property = lhsTable->props[name];
            property.setType(theType);
            property.location = expr.indexLocation;
            return theType;
        }
        else if (auto indexer = lhsTable->indexer)
        {
            Unifier state = mkUnifier(scope, expr.location);
            state.tryUnify(stringType, indexer->indexType);
            TypeId retType = indexer->indexResultType;
            if (!state.errors.empty())
            {

                reportError(expr.location, UnknownProperty{lhs, std::move(name)});
                retType = errorRecoveryType(retType);
            }
            else
                state.log.commit();

            return retType;
        }
        else if (lhsTable->state == TableState::Sealed)
        {
            reportError(TypeError{expr.location, CannotExtendTable{lhs, CannotExtendTable::Property, std::move(name)}});
            return errorRecoveryType(scope);
        }
        else
        {
            reportError(TypeError{expr.location, GenericError{"Internal error: generic tables are not lvalues"}});
            return errorRecoveryType(scope);
        }
    }
    else if (const ExternType* lhsExternType = get<ExternType>(lhs))
    {
        if (const Property* prop = lookupExternTypeProp(lhsExternType, name))
        {
            return prop->type_DEPRECATED();
        }

        if (auto indexer = lhsExternType->indexer)
        {
            Unifier state = mkUnifier(scope, expr.location);
            state.tryUnify(stringType, indexer->indexType);
            if (state.errors.empty())
            {
                state.log.commit();
                return indexer->indexResultType;
            }
        }

        reportError(TypeError{expr.location, UnknownProperty{lhs, std::move(name)}});
        return errorRecoveryType(scope);
    }
    else if (get<IntersectionType>(lhs))
    {
        if (std::optional<TypeId> ty = getIndexTypeFromType(scope, lhs, name, expr.location, /* addErrors= */ false))
            return *ty;

        // If intersection has a table part, report that it cannot be extended just as a sealed table
        if (isTableIntersection(lhs))
        {
            reportError(TypeError{expr.location, CannotExtendTable{lhs, CannotExtendTable::Property, std::move(name)}});
            return errorRecoveryType(scope);
        }
    }

    reportError(TypeError{expr.location, NotATable{lhs}});
    return errorRecoveryType(scope);
}

TypeId TypeChecker::checkLValueBinding(const ScopePtr& scope, const AstExprIndexExpr& expr, ValueContext ctx)
{
    TypeId exprType = checkExpr(scope, *expr.expr).type;
    tablify(exprType);

    exprType = stripFromNilAndReport(exprType, expr.expr->location);

    TypeId indexType = checkExpr(scope, *expr.index).type;

    exprType = follow(exprType);

    if (get<AnyType>(exprType) || get<ErrorType>(exprType))
        return exprType;

    if (get<NeverType>(exprType))
        return unknownType;

    AstExprConstantString* value = expr.index->as<AstExprConstantString>();

    if (value)
    {
        if (const ExternType* exprExternType = get<ExternType>(exprType))
        {
            if (const Property* prop = lookupExternTypeProp(exprExternType, value->value.data))
            {
                return prop->type_DEPRECATED();
            }

            if (auto indexer = exprExternType->indexer)
            {
                unify(stringType, indexer->indexType, scope, expr.index->location);
                return indexer->indexResultType;
            }

            reportError(TypeError{expr.location, UnknownProperty{exprType, value->value.data}});
            return errorRecoveryType(scope);
        }
        else if (get<IntersectionType>(exprType))
        {
            Name name = std::string(value->value.data, value->value.size);

            if (std::optional<TypeId> ty = getIndexTypeFromType(scope, exprType, name, expr.location, /* addErrors= */ false))
                return *ty;

            // If intersection has a table part, report that it cannot be extended just as a sealed table
            if (isTableIntersection(exprType))
            {
                reportError(TypeError{expr.location, CannotExtendTable{exprType, CannotExtendTable::Property, std::move(name)}});
                return errorRecoveryType(scope);
            }
        }
    }
    else
    {
        if (const ExternType* exprExternType = get<ExternType>(exprType))
        {
            if (auto indexer = exprExternType->indexer)
            {
                unify(indexType, indexer->indexType, scope, expr.index->location);
                return indexer->indexResultType;
            }
        }

        if (const ExternType* exprExternType = get<ExternType>(exprType))
        {
            if (isNonstrictMode())
                return unknownType;
            reportError(TypeError{expr.location, DynamicPropertyLookupOnExternTypesUnsafe{exprType}});
            return errorRecoveryType(scope);
        }
    }

    {
        // We're going to have a whole vector.
        std::vector<TableType*> tableTypes{};
        bool isUnion = true;

        // We'd like for normalization eventually to deal with this sort of thing, but as a tactical affordance, we will
        // attempt to deal with _one_ level of unions or intersections.
        if (auto exprUnion = get<UnionType>(exprType))
        {
            tableTypes.reserve(exprUnion->options.size());

            for (auto option : exprUnion)
            {
                TableType* optionTable = getMutableTableType(option);

                if (!optionTable)
                {
                    // TODO: we could do better here and report `option` is not a table as reasoning for the error
                    reportError(TypeError{expr.expr->location, NotATable{exprType}});
                    return errorRecoveryType(scope);
                }

                tableTypes.push_back(optionTable);
            }
        }
        else if (auto exprIntersection = get<IntersectionType>(exprType))
        {
            tableTypes.reserve(exprIntersection->parts.size());
            isUnion = false;

            for (auto part : exprIntersection)
            {
                TableType* partTable = getMutableTableType(part);

                if (!partTable)
                {
                    // TODO: we could do better here and report `part` is not a table as reasoning for the error
                    reportError(TypeError{expr.expr->location, NotATable{exprType}});
                    return errorRecoveryType(scope);
                }

                tableTypes.push_back(partTable);
            }
        }
        else if (auto exprTable = getMutableTableType(exprType))
        {
            tableTypes.push_back(exprTable);
        }
        else
        {
            reportError(TypeError{expr.expr->location, NotATable{exprType}});
            return errorRecoveryType(scope);
        }

        if (value)
        {
            DenseHashSet<TypeId> propTypes{{}};

            for (auto table : tableTypes)
            {
                const auto& it = table->props.find(value->value.data);
                if (it != table->props.end())
                {
                    propTypes.insert(it->second.type_DEPRECATED());
                }
                else if ((ctx == ValueContext::LValue && table->state == TableState::Unsealed) || table->state == TableState::Free)
                {
                    TypeId resultType = freshType(scope);
                    Property& property = table->props[value->value.data];
                    property.setType(resultType);
                    property.location = expr.index->location;
                    propTypes.insert(resultType);
                }
            }

            if (propTypes.size() == 1)
                return *propTypes.begin();

            if (!propTypes.empty())
            {
                if (isUnion)
                {
                    std::vector<TypeId> options = reduceUnion({propTypes.begin(), propTypes.end()});

                    if (options.empty())
                        return neverType;

                    if (options.size() == 1)
                        return options[0];

                    return addType(UnionType{std::move(options)});
                }

                return addType(IntersectionType{{propTypes.begin(), propTypes.end()}});
            }
        }

        DenseHashSet<TypeId> resultTypes{{}};

        for (auto table : tableTypes)
        {
            if (table->indexer)
            {
                const TableIndexer& indexer = *table->indexer;
                unify(indexType, indexer.indexType, scope, expr.index->location);
                resultTypes.insert(indexer.indexResultType);
            }
            else if ((ctx == ValueContext::LValue && table->state == TableState::Unsealed) || table->state == TableState::Free)
            {
                TypeId indexerType = freshType(table->level);
                unify(indexType, indexerType, scope, expr.location);
                TypeId indexResultType = freshType(table->level);

                table->indexer = TableIndexer{anyIfNonstrict(indexerType), anyIfNonstrict(indexResultType)};
                resultTypes.insert(indexResultType);
            }
            else
            {
                /*
                 * If we use [] indexing to fetch a property from a sealed table that
                 * has no indexer, we have no idea if it will work so we just return any
                 * and hope for the best.
                 */

                // if this is a union, it's going to be equivalent to `any` no matter what at this point, so we'll just call it done.
                if (isUnion)
                    return anyType;

                resultTypes.insert(anyType);
            }
        }

        if (resultTypes.size() == 1)
            return *resultTypes.begin();

        if (isUnion)
        {
            std::vector<TypeId> options = reduceUnion({resultTypes.begin(), resultTypes.end()});

            if (options.empty())
                return neverType;

            if (options.size() == 1)
                return options[0];

            return addType(UnionType{std::move(options)});
        }

        return addType(IntersectionType{{resultTypes.begin(), resultTypes.end()}});
    }
}

// Answers the question: "Can I define another function with this name?"
// Primarily about detecting duplicates.
TypeId TypeChecker::checkFunctionName(const ScopePtr& scope, AstExpr& funName, TypeLevel level)
{
    auto freshTy = [&]()
    {
        return freshType(level);
    };

    if (auto globalName = funName.as<AstExprGlobal>())
    {
        const ScopePtr& moduleScope = currentModule->getModuleScope();
        Symbol name = globalName->name;
        if (moduleScope->bindings.count(name))
        {
            if (isNonstrictMode())
                return moduleScope->bindings[name].typeId;

            return errorRecoveryType(scope);
        }
        else
        {
            TypeId ty = freshTy();
            moduleScope->bindings[name] = {ty, funName.location};
            return ty;
        }
    }
    else if (auto localName = funName.as<AstExprLocal>())
    {
        Symbol name = localName->local;
        Binding& binding = scope->bindings[name];
        if (binding.typeId == nullptr)
            binding = {freshTy(), funName.location};

        return binding.typeId;
    }
    else if (auto indexName = funName.as<AstExprIndexName>())
    {
        TypeId lhsType = checkExpr(scope, *indexName->expr).type;
        TableType* ttv = getMutableTableType(lhsType);

        if (!ttv || ttv->state == TableState::Sealed)
        {
            if (auto ty = getIndexTypeFromType(scope, lhsType, indexName->index.value, indexName->indexLocation, /* addErrors= */ false))
                return *ty;

            return errorRecoveryType(scope);
        }

        Name name = indexName->index.value;

        if (ttv->props.count(name))
            return ttv->props[name].type_DEPRECATED();

        Property& property = ttv->props[name];
        property.setType(freshTy());
        property.location = indexName->indexLocation;
        return property.type_DEPRECATED();
    }
    else if (funName.is<AstExprError>())
        return errorRecoveryType(scope);
    else
    {
        ice("Unexpected AST node type", funName.location);
    }
}

// This returns a pair `[funType, funScope]` where
// - funType is the prototype type of the function
// - funScope is the scope for the function, which is a child scope with bindings added for
//   parameters (and generic types if there were explicit generic annotations).
//
// The function type is a prototype, in that it may be missing some generic types which
// can only be inferred from type inference after typechecking the function body.
// For example the function `function id(x) return x end` has prototype
// `(X) -> Y...`, but after typechecking the body, we cam unify `Y...` with `X`
// to get type `(X) -> X`, then we quantify the free types to get the final
// generic type `<a>(a) -> a`.
std::pair<TypeId, ScopePtr> TypeChecker::checkFunctionSignature(
    const ScopePtr& scope,
    int subLevel,
    const AstExprFunction& expr,
    std::optional<Location> originalName,
    std::optional<TypeId> selfType,
    std::optional<TypeId> expectedType
)
{
    ScopePtr funScope = childFunctionScope(scope, expr.location, subLevel);

    const FunctionType* expectedFunctionType = nullptr;

    if (expectedType)
    {
        LUAU_ASSERT(!expr.self);

        if (auto ftv = get<FunctionType>(follow(*expectedType)))
        {
            expectedFunctionType = ftv;
        }
        else if (auto utv = get<UnionType>(follow(*expectedType)))
        {
            // Look for function type in a union. Other types can be ignored since current expression is a function
            for (auto option : utv)
            {
                if (auto ftv = get<FunctionType>(follow(option)))
                {
                    if (!expectedFunctionType)
                    {
                        expectedFunctionType = ftv;
                    }
                    else
                    {
                        // Do not infer argument types when multiple overloads are expected
                        expectedFunctionType = nullptr;
                        break;
                    }
                }
            }
        }
    }

    auto [generics, genericPacks] = createGenericTypes(funScope, std::nullopt, expr, expr.generics, expr.genericPacks);

    TypePackId retPack;
    if (expr.returnAnnotation)
        retPack = resolveTypePack(funScope, *expr.returnAnnotation);
    else if (isNonstrictMode())
        retPack = anyTypePack;
    else if (expectedFunctionType && expectedFunctionType->generics.empty() && expectedFunctionType->genericPacks.empty())
    {
        auto [head, tail] = flatten(expectedFunctionType->retTypes);

        // Do not infer 'nil' as function return type
        if (!tail && head.size() == 1 && isNil(head[0]))
            retPack = freshTypePack(funScope);
        else
            retPack = addTypePack(head, tail);
    }
    else
        retPack = freshTypePack(funScope);

    if (expr.vararg)
    {
        if (expr.varargAnnotation)
            funScope->varargPack = resolveTypePack(funScope, *expr.varargAnnotation);
        else
        {
            if (expectedFunctionType && !isNonstrictMode())
            {
                auto [head, tail] = flatten(expectedFunctionType->argTypes);

                if (expr.args.size <= head.size())
                {
                    head.erase(head.begin(), head.begin() + expr.args.size);

                    funScope->varargPack = addTypePack(head, tail);
                }
                else if (tail)
                {
                    if (get<VariadicTypePack>(follow(*tail)))
                        funScope->varargPack = addTypePack({}, tail);
                }
                else
                {
                    funScope->varargPack = addTypePack({});
                }
            }

            // TODO: should this be a free type pack? CLI-39910
            if (!funScope->varargPack)
                funScope->varargPack = anyTypePack;
        }
    }

    std::vector<TypeId> argTypes;

    funScope->returnType = retPack;

    if (expr.self)
    {
        TypeId selfType = anyIfNonstrict(freshType(funScope));
        funScope->bindings[expr.self] = {selfType, expr.self->location};
        argTypes.push_back(selfType);
    }

    // Prepare expected argument type iterators if we have an expected function type
    TypePackIterator expectedArgsCurr, expectedArgsEnd;

    if (expectedFunctionType && !isNonstrictMode())
    {
        expectedArgsCurr = begin(expectedFunctionType->argTypes);
        expectedArgsEnd = end(expectedFunctionType->argTypes);
    }

    for (AstLocal* local : expr.args)
    {
        TypeId argType = nullptr;

        if (local->annotation)
        {
            argType = resolveType(funScope, *local->annotation);

            // If the annotation type has an error, treat it as if there was no annotation
            if (get<ErrorType>(follow(argType)))
                argType = anyIfNonstrict(freshType(funScope));
        }
        else
        {
            if (expectedFunctionType && !isNonstrictMode())
            {
                if (expectedArgsCurr != expectedArgsEnd)
                {
                    argType = *expectedArgsCurr;
                }
                else if (auto expectedArgsTail = expectedArgsCurr.tail())
                {
                    if (const VariadicTypePack* vtp = get<VariadicTypePack>(follow(*expectedArgsTail)))
                        argType = vtp->ty;
                }
            }

            if (!argType)
                argType = anyIfNonstrict(freshType(funScope));
        }

        funScope->bindings[local] = {argType, local->location};
        argTypes.push_back(argType);

        if (expectedArgsCurr != expectedArgsEnd)
            ++expectedArgsCurr;
    }

    TypePackId argPack = addTypePack(TypePackVar(TypePack{std::move(argTypes), funScope->varargPack}));

    FunctionDefinition defn;
    defn.definitionModuleName = currentModule->name;
    defn.definitionLocation = expr.location;
    defn.varargLocation = expr.vararg ? std::make_optional(expr.varargLocation) : std::nullopt;
    defn.originalNameLocation = originalName.value_or(Location(expr.location.begin, 0));

    std::vector<TypeId> genericTys;
    // if we have a generic expected function type and no generics, we should use the expected ones.
    if (expectedFunctionType && generics.empty())
    {
        genericTys = expectedFunctionType->generics;
    }
    else
    {
        genericTys.reserve(generics.size());
        for (const GenericTypeDefinition& generic : generics)
            genericTys.push_back(generic.ty);
    }

    std::vector<TypePackId> genericTps;
    // if we have a generic expected function type and no generic typepacks, we should use the expected ones.
    if (expectedFunctionType && genericPacks.empty())
    {
        genericTps = expectedFunctionType->genericPacks;
    }
    else
    {
        genericTps.reserve(genericPacks.size());
        for (const GenericTypePackDefinition& generic : genericPacks)
            genericTps.push_back(generic.tp);
    }

    TypeId funTy =
        addType(FunctionType(funScope->level, std::move(genericTys), std::move(genericTps), argPack, retPack, std::move(defn), bool(expr.self)));

    FunctionType* ftv = getMutable<FunctionType>(funTy);

    ftv->argNames.reserve(expr.args.size + (expr.self ? 1 : 0));

    if (expr.self)
        ftv->argNames.push_back(FunctionArgument{"self", {}});

    for (AstLocal* local : expr.args)
        ftv->argNames.push_back(FunctionArgument{local->name.value, local->location});

    if (FFlag::LuauParametrizedAttributeSyntax)
    {
        AstAttr* deprecatedAttr = expr.getAttribute(AstAttr::Type::Deprecated);
        ftv->isDeprecatedFunction = deprecatedAttr != nullptr;
        if (deprecatedAttr)
        {
            ftv->deprecatedInfo = std::make_shared<AstAttr::DeprecatedInfo>(deprecatedAttr->deprecatedInfo());
        }
    }

    return std::make_pair(funTy, funScope);
}

static bool allowsNoReturnValues(const TypePackId tp)
{
    for (TypeId ty : tp)
    {
        if (!get<ErrorType>(follow(ty)))
        {
            return false;
        }
    }

    return true;
}

static Location getEndLocation(const AstExprFunction& function)
{
    Location loc = function.location;
    if (loc.begin.line != loc.end.line)
    {
        Position begin = loc.end;
        begin.column = std::max(0u, begin.column - 3);
        loc = Location(begin, 3);
    }

    return loc;
}

void TypeChecker::checkFunctionBody(const ScopePtr& scope, TypeId ty, const AstExprFunction& function)
{
    LUAU_TIMETRACE_SCOPE("TypeChecker::checkFunctionBody", "TypeChecker");

    if (function.debugname.value)
        LUAU_TIMETRACE_ARGUMENT("name", function.debugname.value);
    else
        LUAU_TIMETRACE_ARGUMENT("line", std::to_string(function.location.begin.line).c_str());

    if (FunctionType* funTy = getMutable<FunctionType>(ty))
    {
        check(scope, *function.body);

        // We explicitly don't follow here to check if we have a 'true' free type instead of bound one
        if (get_if<FreeTypePack>(&funTy->retTypes->ty))
            *asMutable(funTy->retTypes) = TypePack{{}, std::nullopt};

        bool reachesImplicitReturn = getFallthrough(function.body) != nullptr;

        if (reachesImplicitReturn && !allowsNoReturnValues(follow(funTy->retTypes)))
        {
            // If we're in nonstrict mode we want to only report this missing return
            // statement if there are type annotations on the function. In strict mode
            // we report it regardless.
            if (!isNonstrictMode() || function.returnAnnotation != nullptr)
            {
                reportError(getEndLocation(function), FunctionExitsWithoutReturning{funTy->retTypes});
            }
        }

        if (!currentModule->astTypes.find(&function))
            currentModule->astTypes[&function] = ty;
    }
    else
        ice("Checking non functional type");
}

WithPredicate<TypePackId> TypeChecker::checkExprPack(const ScopePtr& scope, const AstExpr& expr)
{
    WithPredicate<TypePackId> result = checkExprPackHelper(scope, expr);
    if (containsNever(result.type))
        return WithPredicate{uninhabitableTypePack};
    return result;
}

WithPredicate<TypePackId> TypeChecker::checkExprPackHelper(const ScopePtr& scope, const AstExpr& expr)
{
    if (auto a = expr.as<AstExprCall>())
        return checkExprPackHelper(scope, *a);
    else if (expr.is<AstExprVarargs>())
    {
        if (!scope->varargPack)
            return WithPredicate{errorRecoveryTypePack(scope)};

        return WithPredicate{*scope->varargPack};
    }
    else
    {
        TypeId type = checkExpr(scope, expr).type;
        return WithPredicate{addTypePack({type})};
    }
}

void TypeChecker::checkArgumentList(
    const ScopePtr& scope,
    const AstExpr& funName,
    Unifier& state,
    TypePackId argPack,
    TypePackId paramPack,
    const std::vector<Location>& argLocations
)
{
    /* Important terminology refresher:
     * A function requires parameters.
     * To call a function, you supply arguments.
     */
    TypePackIterator argIter = begin(argPack, &state.log);
    TypePackIterator paramIter = begin(paramPack, &state.log);
    TypePackIterator endIter = end(argPack); // Important subtlety: All end TypePackIterators are equivalent

    size_t paramIndex = 0;

    int loopCount = 0;
    auto exceedsLoopCount = [&]()
    {
        ++loopCount;
        if (loopCount > FInt::LuauTypeInferTypePackLoopLimit)
        {
            state.reportError(TypeError{state.location, CodeTooComplex{}});
            reportErrorCodeTooComplex(state.location);
            return true;
        }

        return false;
    };

    auto reportCountMismatchError = [&state, &argLocations, paramPack, argPack, &funName]()
    {
        // For this case, we want the error span to cover every errant extra parameter
        Location location = state.location;
        if (!argLocations.empty())
            location = {state.location.begin, argLocations.back().end};

        std::string namePath;

        if (std::optional<std::string> path = getFunctionNameAsString(funName))
            namePath = *path;

        auto [minParams, optMaxParams] = getParameterExtents(&state.log, paramPack);
        state.reportError(
            TypeError{
                location,
                CountMismatch{
                    minParams, optMaxParams, std::distance(begin(argPack), end(argPack)), CountMismatch::Context::Arg, false, std::move(namePath)
                }
            }
        );
    };

    while (true)
    {
        state.location = paramIndex < argLocations.size() ? argLocations[paramIndex] : state.location;

        if (argIter == endIter && paramIter == endIter)
        {
            std::optional<TypePackId> argTail = argIter.tail();
            std::optional<TypePackId> paramTail = paramIter.tail();

            // If we hit the end of both type packs simultaneously, we have to unify them.
            // But if one side has a free tail and the other has none at all, we create an empty pack and bind the free tail to that.

            if (argTail)
            {
                if (state.log.getMutable<FreeTypePack>(state.log.follow(*argTail)))
                {
                    if (paramTail)
                        state.tryUnify(*paramTail, *argTail);
                    else
                        state.log.replace(*argTail, TypePackVar(TypePack{{}}));
                }
                else if (paramTail)
                {
                    state.tryUnify(*argTail, *paramTail);
                }
            }
            else if (paramTail)
            {
                // argTail is definitely empty
                if (state.log.getMutable<FreeTypePack>(state.log.follow(*paramTail)))
                    state.log.replace(*paramTail, TypePackVar(TypePack{{}}));
            }

            return;
        }
        else if (argIter == endIter)
        {
            // Not enough arguments.

            // Might be ok if we are forwarding a vararg along.  This is a common thing to occur in nonstrict mode.
            if (argIter.tail())
            {
                TypePackId tail = *argIter.tail();
                if (state.log.getMutable<ErrorTypePack>(tail))
                {
                    // Unify remaining parameters so we don't leave any free-types hanging around.
                    while (paramIter != endIter)
                    {
                        state.tryUnify(errorRecoveryType(anyType), *paramIter);
                        ++paramIter;
                    }
                    return;
                }
                else if (auto vtp = state.log.getMutable<VariadicTypePack>(tail))
                {
                    loopCount = 0;

                    // Function is variadic and requires that all subsequent parameters
                    // be compatible with a type.
                    while (paramIter != endIter)
                    {
                        state.tryUnify(vtp->ty, *paramIter);
                        ++paramIter;

                        if (exceedsLoopCount())
                            return;
                    }

                    return;
                }
                else if (state.log.getMutable<FreeTypePack>(tail))
                {
                    std::vector<TypeId> rest;
                    rest.reserve(std::distance(paramIter, endIter));

                    loopCount = 0;

                    while (paramIter != endIter)
                    {
                        rest.push_back(*paramIter);
                        ++paramIter;

                        if (exceedsLoopCount())
                            return;
                    }

                    TypePackId varPack = addTypePack(TypePackVar{TypePack{std::move(rest), paramIter.tail()}});
                    state.tryUnify(tail, varPack);
                    return;
                }
            }

            // If any remaining unfulfilled parameters are nonoptional, this is a problem.
            while (paramIter != endIter)
            {
                TypeId t = state.log.follow(*paramIter);
                if (isOptional(t))
                {
                } // ok
                else if (state.log.getMutable<ErrorType>(t))
                {
                } // ok
                else
                {
                    auto [minParams, optMaxParams] = getParameterExtents(&state.log, paramPack);

                    std::optional<TypePackId> tail = flatten(paramPack, state.log).second;
                    bool isVariadic = tail && Luau::isVariadic(*tail);

                    std::string namePath;

                    if (std::optional<std::string> path = getFunctionNameAsString(funName))
                        namePath = *path;

                    state.reportError(
                        TypeError{
                            funName.location,
                            CountMismatch{minParams, optMaxParams, paramIndex, CountMismatch::Context::Arg, isVariadic, std::move(namePath)}
                        }
                    );
                    return;
                }
                ++paramIter;
            }
        }
        else if (paramIter == endIter)
        {
            // too many parameters passed
            if (!paramIter.tail())
            {
                loopCount = 0;

                while (argIter != endIter)
                {
                    // The use of unify here is deliberate. We don't want this unification
                    // to be undoable.
                    unify(errorRecoveryType(scope), *argIter, scope, state.location);
                    ++argIter;

                    if (exceedsLoopCount())
                        return;
                }
                reportCountMismatchError();
                return;
            }
            TypePackId tail = state.log.follow(*paramIter.tail());

            if (state.log.getMutable<ErrorTypePack>(tail))
            {
                // Function is variadic.  Ok.
                return;
            }
            else if (auto vtp = state.log.getMutable<VariadicTypePack>(tail))
            {
                loopCount = 0;

                // Function is variadic and requires that all subsequent parameters
                // be compatible with a type.
                size_t argIndex = paramIndex;
                while (argIter != endIter)
                {
                    Location location = state.location;

                    if (argIndex < argLocations.size())
                        location = argLocations[argIndex];

                    state.location = location;
                    state.tryUnify(*argIter, vtp->ty);

                    ++argIter;
                    ++argIndex;

                    if (exceedsLoopCount())
                        return;
                }

                return;
            }
            else if (state.log.getMutable<FreeTypePack>(tail))
            {
                loopCount = 0;

                // Create a type pack out of the remaining argument types
                // and unify it with the tail.
                std::vector<TypeId> rest;
                rest.reserve(std::distance(argIter, endIter));
                while (argIter != endIter)
                {
                    rest.push_back(*argIter);
                    ++argIter;

                    if (exceedsLoopCount())
                        return;
                }

                TypePackId varPack = addTypePack(TypePackVar{TypePack{std::move(rest), argIter.tail()}});
                state.tryUnify(varPack, tail);

                return;
            }
            else if (state.log.getMutable<FreeTypePack>(tail))
            {
                state.log.replace(tail, TypePackVar(TypePack{{}}));
                return;
            }
            else if (state.log.getMutable<GenericTypePack>(tail))
            {
                reportCountMismatchError();
                return;
            }
        }
        else
        {
            if (FFlag::LuauInstantiateInSubtyping)
                state.tryUnify(*argIter, *paramIter, /*isFunctionCall*/ false);
            else
                unifyWithInstantiationIfNeeded(*argIter, *paramIter, scope, state);
            ++argIter;
            ++paramIter;
        }

        ++paramIndex;
    }
}

WithPredicate<TypePackId> TypeChecker::checkExprPackHelper(const ScopePtr& scope, const AstExprCall& expr)
{
    // evaluate type of function
    // decompose an intersection into its component overloads
    // Compute types of parameters
    // For each overload
    //     Compare parameter and argument types
    //     Report any errors (also speculate dot vs colon warnings!)
    //     Return the resulting return type (even if there are errors)
    // If there are no matching overloads, unify with (a...) -> (b...) and return b...

    TypeId selfType = nullptr;
    TypeId functionType = nullptr;
    TypeId actualFunctionType = nullptr;

    if (expr.self)
    {
        AstExprIndexName* indexExpr = expr.func->as<AstExprIndexName>();
        if (!indexExpr)
            ice("method call expression has no 'self'");

        selfType = checkExpr(scope, *indexExpr->expr).type;
        selfType = stripFromNilAndReport(selfType, expr.func->location);

        if (std::optional<TypeId> propTy = getIndexTypeFromType(scope, selfType, indexExpr->index.value, expr.location, /* addErrors= */ true))
        {
            functionType = *propTy;
            actualFunctionType = instantiate(scope, functionType, expr.func->location);
        }
        else
        {
            functionType = errorRecoveryType(scope);
            actualFunctionType = functionType;
        }
    }
    else
    {
        functionType = checkExpr(scope, *expr.func).type;
        actualFunctionType = instantiate(scope, functionType, expr.func->location);
    }

    TypePackId retPack;
    if (auto free = get<FreeType>(actualFunctionType))
    {
        retPack = freshTypePack(free->level);
        TypePackId freshArgPack = freshTypePack(free->level);
        emplaceType<FunctionType>(asMutable(actualFunctionType), free->level, freshArgPack, retPack);
    }
    else
        retPack = freshTypePack(scope->level);

    // We break this function up into a lambda here to limit our stack footprint.
    // The vectors used by this function aren't allocated until the lambda is actually called.
    auto the_rest = [&]() -> WithPredicate<TypePackId>
    {
        // checkExpr will log the pre-instantiated type of the function.
        // That's not nearly as interesting as the instantiated type, which will include details about how
        // generic functions are being instantiated for this particular callsite.
        currentModule->astOriginalCallTypes[expr.func] = follow(functionType);
        currentModule->astTypes[expr.func] = actualFunctionType;

        std::vector<TypeId> overloads = flattenIntersection(actualFunctionType);

        std::vector<std::optional<TypeId>> expectedTypes = getExpectedTypesForCall(overloads, expr.args.size, expr.self);

        WithPredicate<TypePackId> argListResult = checkExprList(scope, expr.location, expr.args, false, {}, expectedTypes);
        TypePackId argPack = argListResult.type;

        if (get<ErrorTypePack>(argPack))
            return WithPredicate{errorRecoveryTypePack(scope)};

        TypePack* args = nullptr;
        if (expr.self)
        {
            argPack = addTypePack(TypePack{{selfType}, argPack});
            argListResult.type = argPack;
        }
        args = getMutable<TypePack>(argPack);
        LUAU_ASSERT(args);

        std::vector<Location> argLocations;
        argLocations.reserve(expr.args.size + 1);
        if (expr.self)
            argLocations.push_back(expr.func->as<AstExprIndexName>()->expr->location);
        for (AstExpr* arg : expr.args)
            argLocations.push_back(arg->location);

        std::vector<OverloadErrorEntry> errors; // errors encountered for each overload

        std::vector<TypeId> overloadsThatMatchArgCount;
        std::vector<TypeId> overloadsThatDont;

        for (TypeId fn : overloads)
        {
            fn = follow(fn);

            if (auto ret = checkCallOverload(
                    scope, expr, fn, retPack, argPack, args, &argLocations, argListResult, overloadsThatMatchArgCount, overloadsThatDont, errors
                ))
                return *ret;
        }

        if (handleSelfCallMismatch(scope, expr, args, argLocations, errors))
            return WithPredicate{retPack};

        reportOverloadResolutionError(scope, expr, retPack, argPack, argLocations, overloads, overloadsThatMatchArgCount, errors);

        const FunctionType* overload = nullptr;
        if (!overloadsThatMatchArgCount.empty())
            overload = get<FunctionType>(overloadsThatMatchArgCount[0]);
        if (!overload && !overloadsThatDont.empty())
            overload = get<FunctionType>(overloadsThatDont[0]);
        if (overload)
            return WithPredicate{errorRecoveryTypePack(overload->retTypes)};

        return WithPredicate{errorRecoveryTypePack(retPack)};
    };

    return the_rest();
}

std::vector<std::optional<TypeId>> TypeChecker::getExpectedTypesForCall(const std::vector<TypeId>& overloads, size_t argumentCount, bool selfCall)
{
    std::vector<std::optional<TypeId>> expectedTypes;

    auto assignOption = [this, &expectedTypes](size_t index, TypeId ty)
    {
        if (index == expectedTypes.size())
        {
            expectedTypes.push_back(ty);
        }
        else if (ty)
        {
            auto& el = expectedTypes[index];

            if (!el)
            {
                el = ty;
            }
            else
            {
                std::vector<TypeId> result = reduceUnion({*el, ty});
                if (result.empty())
                    el = neverType;
                else
                    el = result.size() == 1 ? result[0] : addType(UnionType{std::move(result)});
            }
        }
    };

    for (const TypeId overload : overloads)
    {
        if (const FunctionType* ftv = get<FunctionType>(overload))
        {
            auto [argsHead, argsTail] = flatten(ftv->argTypes);

            size_t start = selfCall ? 1 : 0;
            size_t index = 0;

            for (size_t i = start; i < argsHead.size(); ++i)
                assignOption(index++, argsHead[i]);

            if (argsTail)
            {
                argsTail = follow(*argsTail);
                if (const VariadicTypePack* vtp = get<VariadicTypePack>(*argsTail))
                {
                    while (index < argumentCount)
                        assignOption(index++, vtp->ty);
                }
            }
        }
    }

    Demoter demoter{&currentModule->internalTypes, builtinTypes};
    demoter.demote(expectedTypes);

    return expectedTypes;
}

/*
 * Note: We return a std::unique_ptr here rather than an optional to manage our stack consumption.
 * If this was an optional, callers would have to pay the stack cost for the result.  This is problematic
 * for functions that need to support recursion up to 600 levels deep.
 */
std::unique_ptr<WithPredicate<TypePackId>> TypeChecker::checkCallOverload(
    const ScopePtr& scope,
    const AstExprCall& expr,
    TypeId fn,
    TypePackId retPack,
    TypePackId argPack,
    TypePack* args,
    const std::vector<Location>* argLocations,
    const WithPredicate<TypePackId>& argListResult,
    std::vector<TypeId>& overloadsThatMatchArgCount,
    std::vector<TypeId>& overloadsThatDont,
    std::vector<OverloadErrorEntry>& errors
)
{
    LUAU_ASSERT(argLocations);

    fn = stripFromNilAndReport(fn, expr.func->location);

    if (get<AnyType>(fn))
    {
        unify(anyTypePack, argPack, scope, expr.location);
        return std::make_unique<WithPredicate<TypePackId>>(anyTypePack);
    }

    if (get<ErrorType>(fn))
    {
        return std::make_unique<WithPredicate<TypePackId>>(errorRecoveryTypePack(scope));
    }

    if (get<NeverType>(fn))
        return std::make_unique<WithPredicate<TypePackId>>(uninhabitableTypePack);

    if (auto ftv = get<FreeType>(fn))
    {
        // fn is one of the overloads of actualFunctionType, which
        // has been instantiated, so is a monotype. We can therefore
        // unify it with a monomorphic function.
        TypeId r = addType(FunctionType(scope->level, argPack, retPack));

        UnifierOptions options;
        options.isFunctionCall = true;
        unify(r, fn, scope, expr.location, options);

        return std::make_unique<WithPredicate<TypePackId>>(retPack);
    }

    std::vector<Location> metaArgLocations;

    // Might be a callable table or class
    std::optional<TypeId> callTy = std::nullopt;
    if (const MetatableType* mttv = get<MetatableType>(fn))
    {
        callTy = getIndexTypeFromType(scope, mttv->metatable, "__call", expr.func->location, /* addErrors= */ false);
    }
    else if (const ExternType* etv = get<ExternType>(fn); etv && etv->metatable)
    {
        callTy = getIndexTypeFromType(scope, *etv->metatable, "__call", expr.func->location, /* addErrors= */ false);
    }

    if (callTy)
    {
        // Construct arguments with 'self' added in front
        TypePackId metaCallArgPack = addTypePack(TypePackVar(TypePack{args->head, args->tail}));

        TypePack* metaCallArgs = getMutable<TypePack>(metaCallArgPack);
        metaCallArgs->head.insert(metaCallArgs->head.begin(), fn);

        metaArgLocations = *argLocations;
        metaArgLocations.insert(metaArgLocations.begin(), expr.func->location);

        fn = instantiate(scope, *callTy, expr.func->location);

        argPack = metaCallArgPack;
        args = metaCallArgs;
        argLocations = &metaArgLocations;
    }

    const FunctionType* ftv = get<FunctionType>(fn);
    if (!ftv)
    {
        reportError(TypeError{expr.func->location, CannotCallNonFunction{fn}});
        unify(errorRecoveryTypePack(scope), retPack, scope, expr.func->location);
        return std::make_unique<WithPredicate<TypePackId>>(errorRecoveryTypePack(retPack));
    }

    // When this function type has magic functions and did return something, we select that overload instead.
    // TODO: pass in a Unifier object to the magic functions? This will allow the magic functions to cooperate with overload resolution.
    if (ftv->magic)
    {
        // TODO: We're passing in the wrong TypePackId. Should be argPack, but a unit test fails otherwise. CLI-40458
        if (std::optional<WithPredicate<TypePackId>> ret = ftv->magic->handleOldSolver(*this, scope, expr, argListResult))
            return std::make_unique<WithPredicate<TypePackId>>(std::move(*ret));
    }

    Unifier state = mkUnifier(scope, expr.location);

    // Unify return types
    checkArgumentList(scope, *expr.func, state, retPack, ftv->retTypes, /*argLocations*/ {});
    if (!state.errors.empty())
    {
        return nullptr;
    }

    checkArgumentList(scope, *expr.func, state, argPack, ftv->argTypes, *argLocations);

    if (!state.errors.empty())
    {
        bool argMismatch = false;
        for (auto error : state.errors)
        {
            CountMismatch* cm = get<CountMismatch>(error);
            if (!cm)
                continue;

            if (cm->context == CountMismatch::Arg)
            {
                argMismatch = true;
                break;
            }
        }

        if (!argMismatch)
            overloadsThatMatchArgCount.push_back(fn);
        else
            overloadsThatDont.push_back(fn);

        errors.push_back(
            OverloadErrorEntry{
                std::move(state.log),
                std::move(state.errors),
                args->head,
                ftv,
            }
        );
    }
    else
    {
        state.log.commit();

        currentModule->astOverloadResolvedTypes[&expr] = fn;

        // We select this overload
        return std::make_unique<WithPredicate<TypePackId>>(retPack);
    }

    return nullptr;
}

bool TypeChecker::handleSelfCallMismatch(
    const ScopePtr& scope,
    const AstExprCall& expr,
    TypePack* args,
    const std::vector<Location>& argLocations,
    const std::vector<OverloadErrorEntry>& errors
)
{
    // No overloads succeeded: Scan for one that would have worked had the user
    // used a.b() rather than a:b() or vice versa.
    for (const auto& e : errors)
    {
        // Did you write foo:bar() when you should have written foo.bar()?
        if (expr.self)
        {
            std::vector<Location> editedArgLocations(argLocations.begin() + 1, argLocations.end());

            std::vector<TypeId> editedParamList(args->head.begin() + 1, args->head.end());
            TypePackId editedArgPack = addTypePack(TypePack{editedParamList});

            Unifier editedState = mkUnifier(scope, expr.location);
            checkArgumentList(scope, *expr.func, editedState, editedArgPack, e.fnTy->argTypes, editedArgLocations);

            if (editedState.errors.empty())
            {
                editedState.log.commit();

                reportError(TypeError{expr.location, FunctionDoesNotTakeSelf{}});
                // This is a little bit suspect: If this overload would work with a . replaced by a :
                // we eagerly assume that that's what you actually meant and we commit to it.
                // This could be incorrect if the function has an additional overload that
                // actually works.
                // checkArgumentList(scope, editedState, retPack, ftv->retTypes, retLocations, CountMismatch::Return);
                return true;
            }
        }
        else if (e.fnTy->hasSelf)
        {
            // Did you write foo.bar() when you should have written foo:bar()?
            if (AstExprIndexName* indexName = expr.func->as<AstExprIndexName>())
            {
                std::vector<Location> editedArgLocations;
                editedArgLocations.reserve(argLocations.size() + 1);
                editedArgLocations.push_back(indexName->expr->location);
                editedArgLocations.insert(editedArgLocations.end(), argLocations.begin(), argLocations.end());

                std::vector<TypeId> editedArgList(args->head);
                editedArgList.insert(editedArgList.begin(), checkExpr(scope, *indexName->expr).type);
                TypePackId editedArgPack = addTypePack(TypePack{editedArgList});

                Unifier editedState = mkUnifier(scope, expr.location);

                checkArgumentList(scope, *expr.func, editedState, editedArgPack, e.fnTy->argTypes, editedArgLocations);

                if (editedState.errors.empty())
                {
                    editedState.log.commit();

                    reportError(TypeError{expr.location, FunctionRequiresSelf{}});
                    // This is a little bit suspect: If this overload would work with a : replaced by a .
                    // we eagerly assume that that's what you actually meant and we commit to it.
                    // This could be incorrect if the function has an additional overload that
                    // actually works.
                    // checkArgumentList(scope, editedState, retPack, ftv->retTypes, retLocations, CountMismatch::Return);
                    return true;
                }
            }
        }
    }

    return false;
}

void TypeChecker::reportOverloadResolutionError(
    const ScopePtr& scope,
    const AstExprCall& expr,
    TypePackId retPack,
    TypePackId argPack,
    const std::vector<Location>& argLocations,
    const std::vector<TypeId>& overloads,
    const std::vector<TypeId>& overloadsThatMatchArgCount,
    std::vector<OverloadErrorEntry>& errors
)
{
    if (overloads.size() == 1)
    {
        errors.front().log.commit();

        reportErrors(errors.front().errors);
        return;
    }

    std::vector<TypeId> overloadTypes = overloadsThatMatchArgCount;
    if (overloadsThatMatchArgCount.size() == 0)
    {
        reportError(TypeError{expr.location, GenericError{"No overload for function accepts " + std::to_string(size(argPack)) + " arguments."}});
        // If no overloads match argument count, just list all overloads.
        overloadTypes = overloads;
    }
    else
    {
        // Report errors of the first argument-count-matching, but failing overload
        TypeId overload = overloadsThatMatchArgCount[0];

        // Remove the overload we are reporting errors about, from the list of alternative
        overloadTypes.erase(std::remove(overloadTypes.begin(), overloadTypes.end(), overload), overloadTypes.end());

        const FunctionType* ftv = get<FunctionType>(overload);

        auto error = std::find_if(
            errors.begin(),
            errors.end(),
            [ftv](const OverloadErrorEntry& e)
            {
                return ftv == e.fnTy;
            }
        );

        LUAU_ASSERT(error != errors.end());

        error->log.commit();

        reportErrors(error->errors);

        // If only one overload matched, we don't need this error because we provided the previous errors.
        if (overloadsThatMatchArgCount.size() == 1)
            return;
    }

    std::string s;
    for (size_t i = 0; i < overloadTypes.size(); ++i)
    {
        TypeId overload = follow(overloadTypes[i]);
        Unifier state = mkUnifier(scope, expr.location);

        // Unify return types
        if (const FunctionType* ftv = get<FunctionType>(overload))
        {
            checkArgumentList(scope, *expr.func, state, retPack, ftv->retTypes, {});
            checkArgumentList(scope, *expr.func, state, argPack, ftv->argTypes, argLocations);
        }

        if (state.errors.empty())
            state.log.commit();

        if (i > 0)
            s += "; ";

        if (i > 0 && i == overloadTypes.size() - 1)
            s += "and ";

        s += toString(overload);
    }

    if (overloadsThatMatchArgCount.size() == 0)
        reportError(expr.func->location, ExtraInformation{"Available overloads: " + s});
    else
        reportError(expr.func->location, ExtraInformation{"Other overloads are also not viable: " + s});

    // No viable overload
    return;
}

WithPredicate<TypePackId> TypeChecker::checkExprList(
    const ScopePtr& scope,
    const Location& location,
    const AstArray<AstExpr*>& exprs,
    bool substituteFreeForNil,
    const std::vector<bool>& instantiateGenerics,
    const std::vector<std::optional<TypeId>>& expectedTypes
)
{
    bool uninhabitable = false;
    TypePackId pack = addTypePack(TypePack{});
    PredicateVec predicates; // At the moment we will be pushing all predicate sets into this. Do we need some way to split them up?

    auto insert = [&predicates](PredicateVec& vec)
    {
        for (Predicate& c : vec)
            predicates.push_back(std::move(c));
    };

    if (exprs.size == 0)
        return WithPredicate{pack};

    TypePack* tp = getMutable<TypePack>(pack);

    size_t lastIndex = exprs.size - 1;
    tp->head.reserve(lastIndex);

    Unifier state = mkUnifier(scope, location);

    std::vector<TxnLog> inverseLogs;

    for (size_t i = 0; i < exprs.size; ++i)
    {
        AstExpr* expr = exprs.data[i];
        std::optional<TypeId> expectedType = i < expectedTypes.size() ? expectedTypes[i] : std::nullopt;

        if (i == lastIndex && (expr->is<AstExprCall>() || expr->is<AstExprVarargs>()))
        {
            auto [typePack, exprPredicates] = checkExprPack(scope, *expr);
            insert(exprPredicates);

            if (containsNever(typePack))
            {
                // f(), g() where f() returns (never, string) or (string, never) means this whole TypePackId is uninhabitable, so return (never,
                // ...never)
                uninhabitable = true;
                continue;
            }
            else if (std::optional<TypeId> firstTy = first(typePack))
            {
                if (!currentModule->astTypes.find(expr))
                    currentModule->astTypes[expr] = follow(*firstTy);
            }

            if (expectedType)
                currentModule->astExpectedTypes[expr] = *expectedType;

            tp->tail = typePack;
        }
        else
        {
            auto [type, exprPredicates] = checkExpr(scope, *expr, expectedType);
            insert(exprPredicates);

            if (get<NeverType>(type))
            {
                // f(), g() where f() returns (never, string) or (string, never) means this whole TypePackId is uninhabitable, so return (never,
                // ...never)
                uninhabitable = true;
                continue;
            }

            TypeId actualType = substituteFreeForNil && expr->is<AstExprConstantNil>() ? freshType(scope) : type;

            if (!FFlag::LuauInstantiateInSubtyping)
            {
                if (instantiateGenerics.size() > i && instantiateGenerics[i])
                    actualType = instantiate(scope, actualType, expr->location);
            }

            if (expectedType)
            {
                state.tryUnify(actualType, *expectedType);

                // Ugly: In future iterations of the loop, we might need the state of the unification we
                // just performed. There's not a great way to pass that into checkExpr. Instead, we store
                // the inverse of the current log, and commit it. When we're done, we'll commit all the
                // inverses. This isn't optimal, and a better solution is welcome here.
                inverseLogs.push_back(state.log.inverse());
                state.log.commit();
            }

            tp->head.push_back(actualType);
        }
    }

    for (TxnLog& log : inverseLogs)
        log.commit();

    if (uninhabitable)
        return WithPredicate{uninhabitableTypePack};
    return {pack, std::move(predicates)};
}

std::optional<AstExpr*> TypeChecker::matchRequire(const AstExprCall& call)
{
    const char* require = "require";

    if (call.args.size != 1)
        return std::nullopt;

    const AstExprGlobal* funcAsGlobal = call.func->as<AstExprGlobal>();
    if (!funcAsGlobal || funcAsGlobal->name != require)
        return std::nullopt;

    if (call.args.size != 1)
        return std::nullopt;

    return call.args.data[0];
}

TypeId TypeChecker::checkRequire(const ScopePtr& scope, const ModuleInfo& moduleInfo, const Location& location)
{
    LUAU_TIMETRACE_SCOPE("TypeChecker::checkRequire", "TypeChecker");
    LUAU_TIMETRACE_ARGUMENT("moduleInfo", moduleInfo.name.c_str());

    if (moduleInfo.name.empty())
    {
        if (currentModule->mode == Mode::Strict)
        {
            reportError(TypeError{location, UnknownRequire{}});
            return errorRecoveryType(anyType);
        }

        return anyType;
    }

    // Types of requires that transitively refer to current module have to be replaced with 'any'
    for (const auto& [location, path] : requireCycles)
    {
        if (!path.empty() && path.front() == moduleInfo.name)
            return anyType;
    }

    ModulePtr module = resolver->getModule(moduleInfo.name);
    if (!module)
    {
        // There are two reasons why we might fail to find the module:
        // either the file does not exist or there's a cycle. If there's a cycle
        // we will already have reported the error.
        if (!resolver->moduleExists(moduleInfo.name) && !moduleInfo.optional)
            reportError(TypeError{location, UnknownRequire{resolver->getHumanReadableModuleName(moduleInfo.name)}});

        return errorRecoveryType(scope);
    }

    if (module->type != SourceCode::Module)
    {
        reportError(location, IllegalRequire{module->humanReadableName, "Module is not a ModuleScript.  It cannot be required."});
        return errorRecoveryType(scope);
    }

    TypePackId modulePack = module->returnType;

    if (get<ErrorTypePack>(modulePack))
        return errorRecoveryType(scope);

    std::optional<TypeId> moduleType = first(modulePack);
    if (!moduleType)
    {
        reportError(location, IllegalRequire{module->humanReadableName, "Module does not return exactly 1 value.  It cannot be required."});
        return errorRecoveryType(scope);
    }

    return *moduleType;
}

void TypeChecker::tablify(TypeId type)
{
    type = follow(type);

    if (auto f = get<FreeType>(type))
        *asMutable(type) = TableType{TableState::Free, f->level};
}

TypeId TypeChecker::anyIfNonstrict(TypeId ty) const
{
    if (isNonstrictMode())
        return anyType;
    else
        return ty;
}

bool TypeChecker::unify(TypeId subTy, TypeId superTy, const ScopePtr& scope, const Location& location)
{
    UnifierOptions options;
    return unify(subTy, superTy, scope, location, options);
}

bool TypeChecker::unify(TypeId subTy, TypeId superTy, const ScopePtr& scope, const Location& location, const UnifierOptions& options)
{
    Unifier state = mkUnifier(scope, location);
    state.tryUnify(subTy, superTy, options.isFunctionCall);

    state.log.commit();

    reportErrors(state.errors);

    return state.errors.empty();
}

bool TypeChecker::unify(TypePackId subTy, TypePackId superTy, const ScopePtr& scope, const Location& location, CountMismatch::Context ctx)
{
    Unifier state = mkUnifier(scope, location);
    state.ctx = ctx;
    state.tryUnify(subTy, superTy);

    state.log.commit();

    reportErrors(state.errors);

    return state.errors.empty();
}

bool TypeChecker::unifyWithInstantiationIfNeeded(TypeId subTy, TypeId superTy, const ScopePtr& scope, const Location& location)
{
    Unifier state = mkUnifier(scope, location);
    unifyWithInstantiationIfNeeded(subTy, superTy, scope, state);

    state.log.commit();

    reportErrors(state.errors);

    return state.errors.empty();
}

void TypeChecker::unifyWithInstantiationIfNeeded(TypeId subTy, TypeId superTy, const ScopePtr& scope, Unifier& state)
{
    LUAU_ASSERT(!FFlag::LuauInstantiateInSubtyping);

    if (!maybeGeneric(subTy))
        // Quick check to see if we definitely can't instantiate
        state.tryUnify(subTy, superTy, /*isFunctionCall*/ false);
    else if (!maybeGeneric(superTy) && isGeneric(subTy))
    {
        // Quick check to see if we definitely have to instantiate
        TypeId instantiated = instantiate(scope, subTy, state.location);
        state.tryUnify(instantiated, superTy, /*isFunctionCall*/ false);
    }
    else
    {
        // First try unifying with the original uninstantiated type
        // but if that fails, try the instantiated one.
        std::unique_ptr<Unifier> child = state.makeChildUnifier();
        child->tryUnify(subTy, superTy, /*isFunctionCall*/ false);
        if (!child->errors.empty())
        {
            TypeId instantiated = instantiate(scope, subTy, state.location, &child->log);
            if (subTy == instantiated)
            {
                // Instantiating the argument made no difference, so just report any child errors
                state.log.concat(std::move(child->log));

                state.errors.insert(state.errors.end(), child->errors.begin(), child->errors.end());
            }
            else
            {
                state.tryUnify(instantiated, superTy, /*isFunctionCall*/ false);
            }
        }
        else
        {
            state.log.concat(std::move(child->log));
        }
    }
}

TypeId TypeChecker::quantify(const ScopePtr& scope, TypeId ty, Location location)
{
    ty = follow(ty);

    const FunctionType* ftv = get<FunctionType>(ty);

    if (ftv)
        Luau::quantify(ty, scope->level);

    return ty;
}

TypeId TypeChecker::instantiate(const ScopePtr& scope, TypeId ty, Location location, const TxnLog* log)
{
    ty = follow(ty);

    const FunctionType* ftv = get<FunctionType>(ty);
    if (ftv && ftv->hasNoFreeOrGenericTypes)
        return ty;

    std::optional<TypeId> instantiated;

    reusableInstantiation.resetState(log, &currentModule->internalTypes, builtinTypes, scope->level, /*scope*/ nullptr);

    if (instantiationChildLimit)
        reusableInstantiation.childLimit = *instantiationChildLimit;

    instantiated = reusableInstantiation.substitute(ty);

    if (instantiated.has_value())
        return *instantiated;
    else
    {
        reportError(location, UnificationTooComplex{});
        return errorRecoveryType(scope);
    }
}

TypeId TypeChecker::anyify(const ScopePtr& scope, TypeId ty, Location location)
{
    Anyification anyification{&currentModule->internalTypes, scope, builtinTypes, iceHandler, anyType, anyTypePack};
    std::optional<TypeId> any = anyification.substitute(ty);
    if (anyification.normalizationTooComplex)
        reportError(location, NormalizationTooComplex{});
    if (any.has_value())
        return *any;
    else
    {
        reportError(location, UnificationTooComplex{});
        return errorRecoveryType(anyType);
    }
}

TypePackId TypeChecker::anyify(const ScopePtr& scope, TypePackId ty, Location location)
{
    Anyification anyification{&currentModule->internalTypes, scope, builtinTypes, iceHandler, anyType, anyTypePack};
    std::optional<TypePackId> any = anyification.substitute(ty);
    if (any.has_value())
        return *any;
    else
    {
        reportError(location, UnificationTooComplex{});
        return errorRecoveryTypePack(anyTypePack);
    }
}

TypePackId TypeChecker::anyifyModuleReturnTypePackGenerics(TypePackId tp)
{
    tp = follow(tp);

    if (const VariadicTypePack* vtp = get<VariadicTypePack>(tp))
    {
        TypeId ty = follow(vtp->ty);
        return get<GenericType>(ty) ? anyTypePack : tp;
    }

    if (!get<TypePack>(follow(tp)))
        return tp;

    std::vector<TypeId> resultTypes;
    std::optional<TypePackId> resultTail;

    TypePackIterator it = begin(tp);

    for (TypePackIterator e = end(tp); it != e; ++it)
    {
        TypeId ty = follow(*it);
        resultTypes.push_back(get<GenericType>(ty) ? anyType : ty);
    }

    if (std::optional<TypePackId> tail = it.tail())
        resultTail = anyifyModuleReturnTypePackGenerics(*tail);

    return addTypePack(resultTypes, resultTail);
}

void TypeChecker::reportError(const TypeError& error)
{
    if (currentModule->mode == Mode::NoCheck)
        return;
    currentModule->errors.push_back(error);
    currentModule->errors.back().moduleName = currentModule->name;
}

void TypeChecker::reportError(const Location& location, TypeErrorData errorData)
{
    return reportError(TypeError{location, std::move(errorData)});
}

void TypeChecker::reportErrors(const ErrorVec& errors)
{
    for (const auto& err : errors)
        reportError(err);
}

LUAU_NOINLINE void TypeChecker::ice(const std::string& message, const Location& location)
{
    iceHandler->ice(message, location);
}

LUAU_NOINLINE void TypeChecker::ice(const std::string& message)
{
    iceHandler->ice(message);
}

LUAU_NOINLINE void TypeChecker::throwTimeLimitError()
{
    throw TimeLimitError(iceHandler->moduleName);
}

LUAU_NOINLINE void TypeChecker::throwUserCancelError()
{
    throw UserCancelError(iceHandler->moduleName);
}

void TypeChecker::prepareErrorsForDisplay(ErrorVec& errVec)
{
    // Remove errors with names that were generated by recovery from a parse error
    errVec.erase(
        std::remove_if(
            errVec.begin(),
            errVec.end(),
            [](auto& err)
            {
                return containsParseErrorName(err);
            }
        ),
        errVec.end()
    );

    for (auto& err : errVec)
    {
        if (auto utk = get<UnknownProperty>(err))
            diagnoseMissingTableKey(utk, err.data);
    }
}

void TypeChecker::diagnoseMissingTableKey(UnknownProperty* utk, TypeErrorData& data)
{
    std::string_view sv(utk->key);
    std::set<Name> candidates;

    auto accumulate = [&](const TableType::Props& props)
    {
        for (const auto& [name, ty] : props)
        {
            if (sv != name && equalsLower(sv, name))
                candidates.insert(name);
        }
    };

    if (auto ttv = getTableType(utk->table))
        accumulate(ttv->props);
    else if (auto etv = get<ExternType>(follow(utk->table)))
    {
        while (etv)
        {
            accumulate(etv->props);

            if (!etv->parent)
                break;

            etv = get<ExternType>(*etv->parent);
            LUAU_ASSERT(etv);
        }
    }

    if (!candidates.empty())
        data = TypeErrorData(UnknownPropButFoundLikeProp{utk->table, utk->key, std::move(candidates)});
}

LUAU_NOINLINE void TypeChecker::reportErrorCodeTooComplex(const Location& location)
{
    reportError(TypeError{location, CodeTooComplex{}});
}

// Creates a new Scope but without carrying forward the varargs from the parent.
ScopePtr TypeChecker::childFunctionScope(const ScopePtr& parent, const Location& location, int subLevel)
{
    ScopePtr scope = std::make_shared<Scope>(parent, subLevel);
    scope->location = location;
    scope->returnType = parent->returnType;
    parent->children.emplace_back(scope.get());

    currentModule->scopes.push_back(std::make_pair(location, scope));
    return scope;
}

// Creates a new Scope and carries forward the varargs from the parent.
ScopePtr TypeChecker::childScope(const ScopePtr& parent, const Location& location)
{
    ScopePtr scope = std::make_shared<Scope>(parent);
    scope->level = parent->level;
    scope->varargPack = parent->varargPack;
    scope->location = location;
    scope->returnType = parent->returnType;
    parent->children.emplace_back(scope.get());

    currentModule->scopes.push_back(std::make_pair(location, scope));
    return scope;
}

void TypeChecker::merge(RefinementMap& l, const RefinementMap& r)
{
    Luau::merge(
        l,
        r,
        [this](TypeId a, TypeId b)
        {
            // TODO: normalize(UnionType{{a, b}})
            std::unordered_set<TypeId> set;

            if (auto utv = get<UnionType>(follow(a)))
                set.insert(begin(utv), end(utv));
            else
                set.insert(a);

            if (auto utv = get<UnionType>(follow(b)))
                set.insert(begin(utv), end(utv));
            else
                set.insert(b);

            std::vector<TypeId> options(set.begin(), set.end());
            if (set.size() == 1)
                return options[0];
            return addType(UnionType{std::move(options)});
        }
    );
}

Unifier TypeChecker::mkUnifier(const ScopePtr& scope, const Location& location)
{
    return Unifier{NotNull{&normalizer}, NotNull{scope.get()}, location, Variance::Covariant};
}

TypeId TypeChecker::freshType(const ScopePtr& scope)
{
    return freshType(scope->level);
}

TypeId TypeChecker::freshType(TypeLevel level)
{
    return currentModule->internalTypes.freshType(builtinTypes, level);
}

TypeId TypeChecker::singletonType(bool value)
{
    return value ? builtinTypes->trueType : builtinTypes->falseType;
}

TypeId TypeChecker::singletonType(std::string value)
{
    // TODO: cache singleton types
    return currentModule->internalTypes.addType(Type(SingletonType(StringSingleton{std::move(value)})));
}

TypeId TypeChecker::errorRecoveryType(const ScopePtr& scope)
{
    return builtinTypes->errorType;
}

TypeId TypeChecker::errorRecoveryType(TypeId guess)
{
    return builtinTypes->errorRecoveryType(guess);
}

TypePackId TypeChecker::errorRecoveryTypePack(const ScopePtr& scope)
{
    return builtinTypes->errorTypePack;
}

TypePackId TypeChecker::errorRecoveryTypePack(TypePackId guess)
{
    return builtinTypes->errorRecoveryTypePack(guess);
}

TypeIdPredicate TypeChecker::mkTruthyPredicate(bool sense, TypeId emptySetTy)
{
    return [this, sense, emptySetTy](TypeId ty) -> std::optional<TypeId>
    {
        // any/error/free gets a special pass unconditionally because they can't be decided.
        if (get<AnyType>(ty) || get<ErrorType>(ty) || get<FreeType>(ty))
            return ty;

        // maps boolean primitive to the corresponding singleton equal to sense
        if (isPrim(ty, PrimitiveType::Boolean))
            return singletonType(sense);

        // if we have boolean singleton, eliminate it if the sense doesn't match with that singleton
        if (auto boolean = get<BooleanSingleton>(get<SingletonType>(ty)))
            return boolean->value == sense ? std::optional<TypeId>(ty) : std::nullopt;

        // if we have nil, eliminate it if sense is true, otherwise take it
        if (isNil(ty))
            return sense ? std::nullopt : std::optional<TypeId>(ty);

        // at this point, anything else is kept if sense is true, or replaced by nil
        return sense ? ty : emptySetTy;
    };
}

std::optional<TypeId> TypeChecker::filterMapImpl(TypeId type, TypeIdPredicate predicate)
{
    std::vector<TypeId> types = Luau::filterMap(type, std::move(predicate));
    if (!types.empty())
        return types.size() == 1 ? types[0] : addType(UnionType{std::move(types)});
    return std::nullopt;
}

std::pair<std::optional<TypeId>, bool> TypeChecker::filterMap(TypeId type, TypeIdPredicate predicate)
{
    TypeId ty = filterMapImpl(type, std::move(predicate)).value_or(neverType);
    return {ty, !bool(get<NeverType>(ty))};
}

std::pair<std::optional<TypeId>, bool> TypeChecker::pickTypesFromSense(TypeId type, bool sense, TypeId emptySetTy)
{
    return filterMap(type, mkTruthyPredicate(sense, emptySetTy));
}

TypeId TypeChecker::addTV(Type&& tv)
{
    return currentModule->internalTypes.addType(std::move(tv));
}

TypePackId TypeChecker::addTypePack(TypePackVar&& tv)
{
    return currentModule->internalTypes.addTypePack(std::move(tv));
}

TypePackId TypeChecker::addTypePack(TypePack&& tp)
{
    return addTypePack(TypePackVar(std::move(tp)));
}

TypePackId TypeChecker::addTypePack(const std::vector<TypeId>& ty)
{
    return addTypePack(ty, std::nullopt);
}

TypePackId TypeChecker::addTypePack(const std::vector<TypeId>& ty, std::optional<TypePackId> tail)
{
    return addTypePack(TypePackVar(TypePack{ty, tail}));
}

TypePackId TypeChecker::addTypePack(std::initializer_list<TypeId>&& ty)
{
    return addTypePack(TypePackVar(TypePack{std::vector<TypeId>(begin(ty), end(ty)), std::nullopt}));
}

TypePackId TypeChecker::freshTypePack(const ScopePtr& scope)
{
    return freshTypePack(scope->level);
}

TypePackId TypeChecker::freshTypePack(TypeLevel level)
{
    return addTypePack(TypePackVar(FreeTypePack(level)));
}

TypeId TypeChecker::resolveType(const ScopePtr& scope, const AstType& annotation)
{
    TypeId ty = resolveTypeWorker(scope, annotation);
    currentModule->astResolvedTypes[&annotation] = ty;
    return ty;
}

TypeId TypeChecker::resolveTypeWorker(const ScopePtr& scope, const AstType& annotation)
{
    if (const auto& lit = annotation.as<AstTypeReference>())
    {
        std::optional<TypeFun> tf;
        if (lit->prefix)
            tf = scope->lookupImportedType(lit->prefix->value, lit->name.value);

        else if (FFlag::DebugLuauMagicTypes && lit->name == "_luau_ice")
            ice("_luau_ice encountered", lit->location);

        else if (FFlag::DebugLuauMagicTypes && lit->name == "_luau_print")
        {
            if (lit->parameters.size != 1 || !lit->parameters.data[0].type)
            {
                reportError(TypeError{annotation.location, GenericError{"_luau_print requires one generic parameter"}});
                return errorRecoveryType(anyType);
            }

            ToStringOptions opts;
            opts.exhaustive = true;
            opts.maxTableLength = 0;
            opts.useLineBreaks = true;

            TypeId param = resolveType(scope, *lit->parameters.data[0].type);
            luauPrintLine(format("_luau_print\t%s\t|\t%s", toString(param, opts).c_str(), toString(lit->location).c_str()));
            return param;
        }

        else
            tf = scope->lookupType(lit->name.value);

        if (!tf)
        {
            if (lit->name == kParseNameError)
                return errorRecoveryType(scope);

            std::string typeName;
            if (lit->prefix)
                typeName = std::string(lit->prefix->value) + ".";
            typeName += lit->name.value;

            if (scope->lookupPack(typeName))
                reportError(TypeError{annotation.location, SwappedGenericTypeParameter{std::move(typeName), SwappedGenericTypeParameter::Type}});
            else
                reportError(TypeError{annotation.location, UnknownSymbol{std::move(typeName), UnknownSymbol::Type}});

            return errorRecoveryType(scope);
        }

        if (lit->parameters.size == 0 && tf->typeParams.empty() && tf->typePackParams.empty())
            return tf->type;

        bool parameterCountErrorReported = false;
        bool hasDefaultTypes = std::any_of(
            tf->typeParams.begin(),
            tf->typeParams.end(),
            [](auto&& el)
            {
                return el.defaultValue.has_value();
            }
        );
        bool hasDefaultPacks = std::any_of(
            tf->typePackParams.begin(),
            tf->typePackParams.end(),
            [](auto&& el)
            {
                return el.defaultValue.has_value();
            }
        );

        if (!lit->hasParameterList)
        {
            if ((!tf->typeParams.empty() && !hasDefaultTypes) || (!tf->typePackParams.empty() && !hasDefaultPacks))
            {
                reportError(TypeError{annotation.location, GenericError{"Type parameter list is required"}});
                parameterCountErrorReported = true;
            }
        }

        std::vector<TypeId> typeParams;
        std::vector<TypeId> extraTypes;
        std::vector<TypePackId> typePackParams;

        for (size_t i = 0; i < lit->parameters.size; ++i)
        {
            if (AstType* type = lit->parameters.data[i].type)
            {
                TypeId ty = resolveType(scope, *type);

                if (typeParams.size() < tf->typeParams.size() || tf->typePackParams.empty())
                    typeParams.push_back(ty);
                else if (typePackParams.empty())
                    extraTypes.push_back(ty);
                else
                    reportError(TypeError{annotation.location, GenericError{"Type parameters must come before type pack parameters"}});
            }
            else if (AstTypePack* typePack = lit->parameters.data[i].typePack)
            {
                TypePackId tp = resolveTypePack(scope, *typePack);

                // If we have collected an implicit type pack, materialize it
                if (typePackParams.empty() && !extraTypes.empty())
                    typePackParams.push_back(addTypePack(extraTypes));

                // If we need more regular types, we can use single element type packs to fill those in
                if (typeParams.size() < tf->typeParams.size() && size(tp) == 1 && finite(tp) && first(tp))
                    typeParams.push_back(*first(tp));
                else
                    typePackParams.push_back(tp);
            }
        }

        // If we still haven't meterialized an implicit type pack, do it now
        if (typePackParams.empty() && !extraTypes.empty())
            typePackParams.push_back(addTypePack(extraTypes));

        size_t typesProvided = typeParams.size();
        size_t typesRequired = tf->typeParams.size();

        size_t packsProvided = typePackParams.size();
        size_t packsRequired = tf->typePackParams.size();

        bool notEnoughParameters =
            (typesProvided < typesRequired && packsProvided == 0) || (typesProvided == typesRequired && packsProvided < packsRequired);
        bool hasDefaultParameters = hasDefaultTypes || hasDefaultPacks;

        // Add default type and type pack parameters if that's required and it's possible
        if (notEnoughParameters && hasDefaultParameters)
        {
            // 'applyTypeFunction' is used to substitute default types that reference previous generic types
            ApplyTypeFunction applyTypeFunction{&currentModule->internalTypes};

            for (size_t i = 0; i < typesProvided; ++i)
                applyTypeFunction.typeArguments[tf->typeParams[i].ty] = typeParams[i];

            if (typesProvided < typesRequired)
            {
                for (size_t i = typesProvided; i < typesRequired; ++i)
                {
                    TypeId defaultTy = tf->typeParams[i].defaultValue.value_or(nullptr);

                    if (!defaultTy)
                        break;

                    std::optional<TypeId> maybeInstantiated = applyTypeFunction.substitute(defaultTy);

                    if (!maybeInstantiated.has_value())
                    {
                        reportError(annotation.location, UnificationTooComplex{});
                        maybeInstantiated = errorRecoveryType(scope);
                    }

                    applyTypeFunction.typeArguments[tf->typeParams[i].ty] = *maybeInstantiated;
                    typeParams.push_back(*maybeInstantiated);
                }
            }

            for (size_t i = 0; i < packsProvided; ++i)
                applyTypeFunction.typePackArguments[tf->typePackParams[i].tp] = typePackParams[i];

            if (packsProvided < packsRequired)
            {
                for (size_t i = packsProvided; i < packsRequired; ++i)
                {
                    TypePackId defaultTp = tf->typePackParams[i].defaultValue.value_or(nullptr);

                    if (!defaultTp)
                        break;

                    std::optional<TypePackId> maybeInstantiated = applyTypeFunction.substitute(defaultTp);

                    if (!maybeInstantiated.has_value())
                    {
                        reportError(annotation.location, UnificationTooComplex{});
                        maybeInstantiated = errorRecoveryTypePack(scope);
                    }

                    applyTypeFunction.typePackArguments[tf->typePackParams[i].tp] = *maybeInstantiated;
                    typePackParams.push_back(*maybeInstantiated);
                }
            }
        }

        // If we didn't combine regular types into a type pack and we're still one type pack short, provide an empty type pack
        if (extraTypes.empty() && typePackParams.size() + 1 == tf->typePackParams.size())
            typePackParams.push_back(addTypePack({}));

        if (typeParams.size() != tf->typeParams.size() || typePackParams.size() != tf->typePackParams.size())
        {
            if (!parameterCountErrorReported)
                reportError(
                    TypeError{annotation.location, IncorrectGenericParameterCount{lit->name.value, *tf, typeParams.size(), typePackParams.size()}}
                );

            // Pad the types out with error recovery types
            while (typeParams.size() < tf->typeParams.size())
                typeParams.push_back(errorRecoveryType(scope));
            while (typePackParams.size() < tf->typePackParams.size())
                typePackParams.push_back(errorRecoveryTypePack(scope));
        }

        bool sameTys = std::equal(
            typeParams.begin(),
            typeParams.end(),
            tf->typeParams.begin(),
            tf->typeParams.end(),
            [](auto&& itp, auto&& tp)
            {
                return itp == tp.ty;
            }
        );
        bool sameTps = std::equal(
            typePackParams.begin(),
            typePackParams.end(),
            tf->typePackParams.begin(),
            tf->typePackParams.end(),
            [](auto&& itpp, auto&& tpp)
            {
                return itpp == tpp.tp;
            }
        );

        // If the generic parameters and the type arguments are the same, we are about to
        // perform an identity substitution, which we can just short-circuit.
        if (sameTys && sameTps)
            return tf->type;

        return instantiateTypeFun(scope, *tf, typeParams, typePackParams, annotation.location);
    }
    else if (const auto& table = annotation.as<AstTypeTable>())
    {
        TableType::Props props;
        std::optional<TableIndexer> tableIndexer;

        for (const auto& prop : table->props)
        {
            if (prop.access == AstTableAccess::Read)
                reportError(prop.accessLocation.value_or(Location{}), GenericError{"read keyword is illegal here"});
            else if (prop.access == AstTableAccess::Write)
                reportError(prop.accessLocation.value_or(Location{}), GenericError{"write keyword is illegal here"});
            else if (prop.access == AstTableAccess::ReadWrite)
                props[prop.name.value] = {resolveType(scope, *prop.type), /* deprecated: */ false, {}, std::nullopt, {}, std::nullopt, prop.location};
            else
                ice("Unexpected property access " + std::to_string(int(prop.access)));
        }

        if (const auto& indexer = table->indexer)
        {
            if (indexer->access == AstTableAccess::Read)
                reportError(indexer->accessLocation.value_or(Location{}), GenericError{"read keyword is illegal here"});
            else if (indexer->access == AstTableAccess::Write)
                reportError(indexer->accessLocation.value_or(Location{}), GenericError{"write keyword is illegal here"});
            else if (indexer->access == AstTableAccess::ReadWrite)
                tableIndexer = TableIndexer(resolveType(scope, *indexer->indexType), resolveType(scope, *indexer->resultType));
            else
                ice("Unexpected property access " + std::to_string(int(indexer->access)));
        }

        TableType ttv{props, tableIndexer, scope->level, TableState::Sealed};
        ttv.definitionModuleName = currentModule->name;
        ttv.definitionLocation = annotation.location;
        return addType(std::move(ttv));
    }
    else if (const auto& func = annotation.as<AstTypeFunction>())
    {
        ScopePtr funcScope = childScope(scope, func->location);
        funcScope->level = scope->level.incr();

        auto [generics, genericPacks] = createGenericTypes(funcScope, std::nullopt, annotation, func->generics, func->genericPacks);

        TypePackId argTypes = resolveTypePack(funcScope, func->argTypes);
        TypePackId retTypes = resolveTypePack(funcScope, *func->returnTypes);

        std::vector<TypeId> genericTys;
        genericTys.reserve(generics.size());
        std::transform(
            generics.begin(),
            generics.end(),
            std::back_inserter(genericTys),
            [](auto&& el)
            {
                return el.ty;
            }
        );

        std::vector<TypePackId> genericTps;
        genericTps.reserve(genericPacks.size());
        std::transform(
            genericPacks.begin(),
            genericPacks.end(),
            std::back_inserter(genericTps),
            [](auto&& el)
            {
                return el.tp;
            }
        );

        TypeId fnType = addType(FunctionType{funcScope->level, std::move(genericTys), std::move(genericTps), argTypes, retTypes});

        FunctionType* ftv = getMutable<FunctionType>(fnType);

        ftv->argNames.reserve(func->argNames.size);
        for (const auto& el : func->argNames)
        {
            if (el)
                ftv->argNames.push_back(FunctionArgument{el->first.value, el->second});
            else
                ftv->argNames.push_back(std::nullopt);
        }

        if (FFlag::LuauParametrizedAttributeSyntax)
        {
            AstAttr* deprecatedAttr = func->getAttribute(AstAttr::Type::Deprecated);
            ftv->isDeprecatedFunction = deprecatedAttr != nullptr;
            if (deprecatedAttr)
            {
                ftv->deprecatedInfo = std::make_shared<AstAttr::DeprecatedInfo>(deprecatedAttr->deprecatedInfo());
            }
        }

        return fnType;
    }
    else if (auto typeOf = annotation.as<AstTypeTypeof>())
    {
        TypeId ty = checkExpr(scope, *typeOf->expr).type;
        return ty;
    }
    else if (annotation.is<AstTypeOptional>())
    {
        return builtinTypes->nilType;
    }
    else if (const auto& un = annotation.as<AstTypeUnion>())
    {
        if (un->types.size == 1)
            return resolveType(scope, *un->types.data[0]);
        std::vector<TypeId> types;
        for (AstType* ann : un->types)
            types.push_back(resolveType(scope, *ann));

        return addType(UnionType{std::move(types)});
    }
    else if (const auto& un = annotation.as<AstTypeIntersection>())
    {
        if (un->types.size == 1)
            return resolveType(scope, *un->types.data[0]);
        std::vector<TypeId> types;
        for (AstType* ann : un->types)
            types.push_back(resolveType(scope, *ann));

        return addType(IntersectionType{std::move(types)});
    }
    else if (const auto& g = annotation.as<AstTypeGroup>())
    {
        return resolveType(scope, *g->type);
    }
    else if (const auto& tsb = annotation.as<AstTypeSingletonBool>())
    {
        return singletonType(tsb->value);
    }
    else if (const auto& tss = annotation.as<AstTypeSingletonString>())
    {
        return singletonType(std::string(tss->value.data, tss->value.size));
    }
    else if (annotation.is<AstTypeError>())
        return errorRecoveryType(scope);
    else
    {
        reportError(TypeError{annotation.location, GenericError{"Unknown type annotation?"}});
        return errorRecoveryType(scope);
    }
}

TypePackId TypeChecker::resolveTypePack(const ScopePtr& scope, const AstTypeList& types)
{
    if (types.types.size == 0 && types.tailType)
    {
        return resolveTypePack(scope, *types.tailType);
    }
    else if (types.types.size > 0)
    {
        std::vector<TypeId> head;
        for (AstType* ann : types.types)
            head.push_back(resolveType(scope, *ann));

        std::optional<TypePackId> tail = types.tailType ? std::optional<TypePackId>(resolveTypePack(scope, *types.tailType)) : std::nullopt;
        return addTypePack(TypePack{std::move(head), tail});
    }

    return addTypePack(TypePack{});
}

TypePackId TypeChecker::resolveTypePack(const ScopePtr& scope, const AstTypePack& annotation)
{
    TypePackId result;
    if (const AstTypePackVariadic* variadic = annotation.as<AstTypePackVariadic>())
    {
        result = addTypePack(TypePackVar{VariadicTypePack{resolveType(scope, *variadic->variadicType)}});
    }
    else if (const AstTypePackGeneric* generic = annotation.as<AstTypePackGeneric>())
    {
        Name genericName = Name(generic->genericName.value);
        std::optional<TypePackId> genericTy = scope->lookupPack(genericName);

        if (!genericTy)
        {
            if (scope->lookupType(genericName))
                reportError(TypeError{generic->location, SwappedGenericTypeParameter{std::move(genericName), SwappedGenericTypeParameter::Pack}});
            else
                reportError(TypeError{generic->location, UnknownSymbol{std::move(genericName), UnknownSymbol::Type}});

            result = errorRecoveryTypePack(scope);
        }
        else
        {
            result = *genericTy;
        }
    }
    else if (const AstTypePackExplicit* explicitTp = annotation.as<AstTypePackExplicit>())
    {
        std::vector<TypeId> types;

        for (auto type : explicitTp->typeList.types)
            types.push_back(resolveType(scope, *type));

        if (auto tailType = explicitTp->typeList.tailType)
            result = addTypePack(types, resolveTypePack(scope, *tailType));
        else
            result = addTypePack(types);
    }
    else
    {
        ice("Unknown AstTypePack kind");
    }

    currentModule->astResolvedTypePacks[&annotation] = result;
    return result;
}

TypeId TypeChecker::instantiateTypeFun(
    const ScopePtr& scope,
    const TypeFun& tf,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& typePackParams,
    const Location& location
)
{
    if (tf.typeParams.empty() && tf.typePackParams.empty())
        return tf.type;

    ApplyTypeFunction applyTypeFunction{&currentModule->internalTypes};

    for (size_t i = 0; i < tf.typeParams.size(); ++i)
        applyTypeFunction.typeArguments[tf.typeParams[i].ty] = typeParams[i];

    for (size_t i = 0; i < tf.typePackParams.size(); ++i)
        applyTypeFunction.typePackArguments[tf.typePackParams[i].tp] = typePackParams[i];

    std::optional<TypeId> maybeInstantiated = applyTypeFunction.substitute(tf.type);
    if (!maybeInstantiated.has_value())
    {
        reportError(location, UnificationTooComplex{});
        return errorRecoveryType(scope);
    }
    if (applyTypeFunction.encounteredForwardedType)
    {
        if (FFlag::LuauNameConstraintRestrictRecursiveTypes)
            reportError(TypeError{location, RecursiveRestraintViolation{}});
        else
            reportError(TypeError{location, GenericError{"Recursive type being used with different parameters"}});
        return errorRecoveryType(scope);
    }

    TypeId instantiated = *maybeInstantiated;

    TypeId target = follow(instantiated);
    const TableType* tfTable = getTableType(tf.type);
    bool needsClone = follow(tf.type) == target || (tfTable != nullptr && tfTable == getTableType(target));
    bool shouldMutate = getTableType(tf.type);
    TableType* ttv = getMutableTableType(target);

    if (shouldMutate && ttv && needsClone)
    {
        // Substitution::clone is a shallow clone. If this is a metatable type, we
        // want to mutate its table, so we need to explicitly clone that table as
        // well. If we don't, we will mutate another module's type surface and cause
        // a use-after-free.
        if (get<MetatableType>(target))
        {
            instantiated = applyTypeFunction.clone(tf.type);
            MetatableType* mtv = getMutable<MetatableType>(instantiated);
            mtv->table = applyTypeFunction.clone(mtv->table);
            ttv = getMutable<TableType>(mtv->table);
        }
        if (get<TableType>(target))
        {
            instantiated = applyTypeFunction.clone(tf.type);
            ttv = getMutable<TableType>(instantiated);
        }
    }

    if (shouldMutate && ttv)
    {
        ttv->instantiatedTypeParams = typeParams;
        ttv->instantiatedTypePackParams = typePackParams;
        ttv->definitionModuleName = currentModule->name;
        ttv->definitionLocation = location;
    }

    return instantiated;
}

GenericTypeDefinitions TypeChecker::createGenericTypes(
    const ScopePtr& scope,
    std::optional<TypeLevel> levelOpt,
    const AstNode& node,
    const AstArray<AstGenericType*>& genericNames,
    const AstArray<AstGenericTypePack*>& genericPackNames,
    bool useCache
)
{
    LUAU_ASSERT(scope->parent);

    const TypeLevel level = levelOpt.value_or(scope->level);

    std::vector<GenericTypeDefinition> generics;

    for (const AstGenericType* generic : genericNames)
    {
        std::optional<TypeId> defaultValue;

        if (generic->defaultValue)
            defaultValue = resolveType(scope, *generic->defaultValue);

        Name n = generic->name.value;

        // These generics are the only thing that will ever be added to scope, so we can be certain that
        // a collision can only occur when two generic types have the same name.
        if (scope->privateTypeBindings.count(n) || scope->privateTypePackBindings.count(n))
        {
            // TODO(jhuelsman): report the exact span of the generic type parameter whose name is a duplicate.
            reportError(TypeError{node.location, DuplicateGenericParameter{n}});
        }

        TypeId g;
        if (useCache)
        {
            TypeId& cached = scope->parent->typeAliasTypeParameters[n];
            if (!cached)
                cached = addType(GenericType{level, n});
            g = cached;
        }
        else
        {
            g = addType(GenericType{level, n});
        }

        generics.push_back({g, defaultValue});
        scope->privateTypeBindings[n] = TypeFun{{}, g};
    }

    std::vector<GenericTypePackDefinition> genericPacks;

    for (const AstGenericTypePack* genericPack : genericPackNames)
    {
        std::optional<TypePackId> defaultValue;

        if (genericPack->defaultValue)
            defaultValue = resolveTypePack(scope, *genericPack->defaultValue);

        Name n = genericPack->name.value;

        // These generics are the only thing that will ever be added to scope, so we can be certain that
        // a collision can only occur when two generic types have the same name.
        if (scope->privateTypePackBindings.count(n) || scope->privateTypeBindings.count(n))
        {
            // TODO(jhuelsman): report the exact span of the generic type parameter whose name is a duplicate.
            reportError(TypeError{node.location, DuplicateGenericParameter{n}});
        }

        TypePackId& cached = scope->parent->typeAliasTypePackParameters[n];
        if (!cached)
            cached = addTypePack(TypePackVar{GenericTypePack{level, n}});

        genericPacks.push_back({cached, defaultValue});
        scope->privateTypePackBindings[n] = cached;
    }

    return {std::move(generics), std::move(genericPacks)};
}

void TypeChecker::refineLValue(const LValue& lvalue, RefinementMap& refis, const ScopePtr& scope, TypeIdPredicate predicate)
{
    const LValue* target = &lvalue;
    std::optional<LValue> key; // If set, we know we took the base of the lvalue path and should be walking down each option of the base's type.

    auto ty = resolveLValue(scope, *target);
    if (!ty)
        return; // Do nothing. An error was already reported.

    // If the provided lvalue is a local or global, then that's without a doubt the target.
    // However, if there is a base lvalue, then we'll want that to be the target iff the base is a union type.
    if (auto base = baseof(lvalue))
    {
        std::optional<TypeId> baseTy = resolveLValue(scope, *base);
        if (baseTy && get<UnionType>(follow(*baseTy)))
        {
            ty = baseTy;
            target = base;
            key = lvalue;
        }
    }

    // If we do not have a key, it means we're not trying to discriminate anything, so it's a simple matter of just filtering for a subset.
    if (!key)
    {
        auto [result, ok] = filterMap(*ty, std::move(predicate));
        addRefinement(refis, *target, *result);
        return;
    }

    // Otherwise, we'll want to walk each option of ty, get its index type, and filter that.
    auto utv = get<UnionType>(follow(*ty));
    LUAU_ASSERT(utv);

    std::unordered_set<TypeId> viableTargetOptions;
    std::unordered_set<TypeId> viableChildOptions; // There may be additional refinements that apply. We add those here too.

    for (TypeId option : utv)
    {
        std::optional<TypeId> discriminantTy;
        if (auto field = Luau::get<Field>(*key)) // need to fully qualify Luau::get because of ADL.
            discriminantTy = getIndexTypeFromType(scope, option, field->key, Location(), /* addErrors= */ false);
        else
            LUAU_ASSERT(!"Unhandled LValue alternative?");

        if (!discriminantTy)
            return; // Do nothing. An error was already reported, as per usual.

        auto [result, ok] = filterMap(*discriminantTy, predicate);
        if (!get<NeverType>(*result))
        {
            viableTargetOptions.insert(option);
            viableChildOptions.insert(*result);
        }
    }

    auto intoType = [this](const std::unordered_set<TypeId>& s) -> std::optional<TypeId>
    {
        if (s.empty())
            return std::nullopt;

        // TODO: allocate UnionType and just normalize.
        std::vector<TypeId> options(s.begin(), s.end());
        if (options.size() == 1)
            return options[0];

        return addType(UnionType{std::move(options)});
    };

    if (std::optional<TypeId> viableTargetType = intoType(viableTargetOptions))
        addRefinement(refis, *target, *viableTargetType);

    if (std::optional<TypeId> viableChildType = intoType(viableChildOptions))
        addRefinement(refis, lvalue, *viableChildType);
}

std::optional<TypeId> TypeChecker::resolveLValue(const ScopePtr& scope, const LValue& lvalue)
{
    // We want to be walking the Scope parents.
    // We'll also want to walk up the LValue path. As we do this, we need to save each LValue because we must walk back.
    // For example:
    //  There exists an entry t.x.
    //  We are asked to look for t.x.y.
    //  We need to search in the provided Scope. Find t.x.y first.
    //  We fail to find t.x.y. Try t.x. We found it. Now we must return the type of the property y from the mapped-to type of t.x.
    //  If we completely fail to find the Symbol t but the Scope has that entry, then we should walk that all the way through and terminate.
    const Symbol symbol = getBaseSymbol(lvalue);

    ScopePtr currentScope = scope;
    while (currentScope)
    {
        std::optional<TypeId> found;

        const LValue* topLValue = nullptr;

        for (topLValue = &lvalue; topLValue; topLValue = baseof(*topLValue))
        {
            if (auto it = currentScope->refinements.find(*topLValue); it != currentScope->refinements.end())
            {
                found = it->second;
                break;
            }
        }

        if (!found)
        {
            // Should not be using scope->lookup. This is already recursive.
            if (auto it = currentScope->bindings.find(symbol); it != currentScope->bindings.end())
                found = it->second.typeId;
            else
            {
                // Nothing exists in this Scope. Just skip and try the parent one.
                currentScope = currentScope->parent;
                continue;
            }
        }

        // We need to walk the l-value path in reverse, so we collect components into a vector
        std::vector<const LValue*> childKeys;

        for (const LValue* curr = &lvalue; curr != topLValue; curr = baseof(*curr))
            childKeys.push_back(curr);

        for (auto it = childKeys.rbegin(); it != childKeys.rend(); ++it)
        {
            const LValue& key = **it;

            // Symbol can happen. Skip.
            if (get<Symbol>(key))
                continue;
            else if (auto field = get<Field>(key))
            {
                found = getIndexTypeFromType(scope, *found, field->key, Location(), /* addErrors= */ false);
                if (!found)
                    return std::nullopt; // Turns out this type doesn't have the property at all. We're done.
            }
            else
                LUAU_ASSERT(!"New LValue alternative not handled here.");
        }

        return found;
    }

    // No entry for it at all. Can happen when LValue root is a global.
    return std::nullopt;
}

std::optional<TypeId> TypeChecker::resolveLValue(const RefinementMap& refis, const ScopePtr& scope, const LValue& lvalue)
{
    if (auto it = refis.find(lvalue); it != refis.end())
        return it->second;
    else
        return resolveLValue(scope, lvalue);
}

// Only should be used for refinements!
// This can probably go away once we have something that can limit a free type's type domain.
static bool isUndecidable(TypeId ty)
{
    ty = follow(ty);
    return get<AnyType>(ty) || get<ErrorType>(ty) || get<FreeType>(ty);
}

void TypeChecker::resolve(const PredicateVec& predicates, const ScopePtr& scope, bool sense)
{
    resolve(predicates, scope->refinements, scope, sense);
}

void TypeChecker::resolve(const PredicateVec& predicates, RefinementMap& refis, const ScopePtr& scope, bool sense, bool fromOr)
{
    for (const Predicate& c : predicates)
        resolve(c, refis, scope, sense, fromOr);
}

void TypeChecker::resolve(const Predicate& predicate, RefinementMap& refis, const ScopePtr& scope, bool sense, bool fromOr)
{
    if (auto truthyP = get<TruthyPredicate>(predicate))
        resolve(*truthyP, refis, scope, sense, fromOr);
    else if (auto andP = get<AndPredicate>(predicate))
        resolve(*andP, refis, scope, sense);
    else if (auto orP = get<OrPredicate>(predicate))
        resolve(*orP, refis, scope, sense);
    else if (auto notP = get<NotPredicate>(predicate))
        resolve(notP->predicates, refis, scope, !sense, fromOr);
    else if (auto isaP = get<IsAPredicate>(predicate))
        resolve(*isaP, refis, scope, sense);
    else if (auto typeguardP = get<TypeGuardPredicate>(predicate))
        resolve(*typeguardP, refis, scope, sense);
    else if (auto eqP = get<EqPredicate>(predicate))
        resolve(*eqP, refis, scope, sense);
    else
        ice("Unhandled predicate kind");
}

void TypeChecker::resolve(const TruthyPredicate& truthyP, RefinementMap& refis, const ScopePtr& scope, bool sense, bool fromOr)
{
    std::optional<TypeId> ty = resolveLValue(refis, scope, truthyP.lvalue);
    if (ty && fromOr)
        return addRefinement(refis, truthyP.lvalue, *ty);

    refineLValue(truthyP.lvalue, refis, scope, mkTruthyPredicate(sense, nilType));
}

void TypeChecker::resolve(const AndPredicate& andP, RefinementMap& refis, const ScopePtr& scope, bool sense)
{
    if (!sense)
    {
        OrPredicate orP{
            {NotPredicate{std::move(andP.lhs)}},
            {NotPredicate{std::move(andP.rhs)}},
        };

        return resolve(orP, refis, scope, !sense);
    }

    resolve(andP.lhs, refis, scope, sense);
    resolve(andP.rhs, refis, scope, sense);
}

void TypeChecker::resolve(const OrPredicate& orP, RefinementMap& refis, const ScopePtr& scope, bool sense)
{
    if (!sense)
    {
        AndPredicate andP{
            {NotPredicate{std::move(orP.lhs)}},
            {NotPredicate{std::move(orP.rhs)}},
        };

        return resolve(andP, refis, scope, !sense);
    }

    RefinementMap leftRefis;
    resolve(orP.lhs, leftRefis, scope, sense);

    RefinementMap rightRefis;
    resolve(orP.lhs, rightRefis, scope, !sense);
    resolve(orP.rhs, rightRefis, scope, sense, true); // :(

    merge(refis, leftRefis);
    merge(refis, rightRefis);
}

void TypeChecker::resolve(const IsAPredicate& isaP, RefinementMap& refis, const ScopePtr& scope, bool sense)
{
    auto predicate = [&](TypeId option) -> std::optional<TypeId>
    {
        // This by itself is not truly enough to determine that A is stronger than B or vice versa.
        bool optionIsSubtype = canUnify(option, isaP.ty, scope, isaP.location).empty();
        bool targetIsSubtype = canUnify(isaP.ty, option, scope, isaP.location).empty();

        // If A is a superset of B, then if sense is true, we promote A to B, otherwise we keep A.
        if (!optionIsSubtype && targetIsSubtype)
            return sense ? isaP.ty : option;

        // If A is a subset of B, then if sense is true we pick A, otherwise we eliminate A.
        if (optionIsSubtype && !targetIsSubtype)
            return sense ? std::optional<TypeId>(option) : std::nullopt;

        // If neither has any relationship, we only return A if sense is false.
        if (!optionIsSubtype && !targetIsSubtype)
            return sense ? std::nullopt : std::optional<TypeId>(option);

        // If both are subtypes, then we're in one of the two situations:
        //   1. Instance₁ <: Instance₂ ∧ Instance₂ <: Instance₁
        //   2. any <: Instance ∧ Instance <: any
        // Right now, we have to look at the types to see if they were undecidables.
        // By this point, we also know free tables are also subtypes and supertypes.
        if (optionIsSubtype && targetIsSubtype)
        {
            // We can only have (any, Instance) because the rhs is never undecidable right now.
            // So we can just return the right hand side immediately.

            // typeof(x) == "Instance" where x : any
            auto ttv = get<TableType>(option);
            if (isUndecidable(option) || (ttv && ttv->state == TableState::Free))
                return sense ? isaP.ty : option;

            // typeof(x) == "Instance" where x : Instance
            if (sense)
                return isaP.ty;
        }

        // local variable works around an odd gcc 9.3 warning: <anonymous> may be used uninitialized
        std::optional<TypeId> res = std::nullopt;
        return res;
    };

    refineLValue(isaP.lvalue, refis, scope, predicate);
}

void TypeChecker::resolve(const TypeGuardPredicate& typeguardP, RefinementMap& refis, const ScopePtr& scope, bool sense)
{
    // Rewrite the predicate 'type(foo) == "vector"' to be 'typeof(foo) == "Vector3"'. They're exactly identical.
    // This allows us to avoid writing in edge cases.
    if (!typeguardP.isTypeof && typeguardP.kind == "vector")
        return resolve(TypeGuardPredicate{std::move(typeguardP.lvalue), typeguardP.location, "Vector3", true}, refis, scope, sense);

    std::optional<TypeId> ty = resolveLValue(refis, scope, typeguardP.lvalue);
    if (!ty)
        return;

    // In certain cases, the value may actually be nil, but Luau doesn't know about it. So we whitelist this.
    if (sense && typeguardP.kind == "nil")
    {
        addRefinement(refis, typeguardP.lvalue, nilType);
        return;
    }

    auto refine = [this, &lvalue = typeguardP.lvalue, &refis, &scope, sense](bool(f)(TypeId), std::optional<TypeId> mapsTo = std::nullopt)
    {
        TypeIdPredicate predicate = [f, mapsTo, sense](TypeId ty) -> std::optional<TypeId>
        {
            if (sense && get<UnknownType>(ty))
                return mapsTo.value_or(ty);

            if (f(ty) == sense)
                return ty;

            if (isUndecidable(ty))
                return mapsTo.value_or(ty);

            return std::nullopt;
        };

        refineLValue(lvalue, refis, scope, std::move(predicate));
    };

    // Note: "vector" never happens here at this point, so we don't have to write something for it.
    if (typeguardP.kind == "nil")
        return refine(isNil, nilType); // This can still happen when sense is false!
    else if (typeguardP.kind == "string")
        return refine(isString, stringType);
    else if (typeguardP.kind == "number")
        return refine(isNumber, numberType);
    else if (typeguardP.kind == "boolean")
        return refine(isBoolean, booleanType);
    else if (typeguardP.kind == "thread")
        return refine(isThread, threadType);
    else if (typeguardP.kind == "buffer")
        return refine(isBuffer, bufferType);
    else if (typeguardP.kind == "table")
    {
        return refine(
            [](TypeId ty) -> bool
            {
                return isTableIntersection(ty) || get<TableType>(ty) || get<MetatableType>(ty);
            }
        );
    }
    else if (typeguardP.kind == "function")
    {
        return refine(
            [](TypeId ty) -> bool
            {
                return isOverloadedFunction(ty) || get<FunctionType>(ty);
            }
        );
    }
    else if (typeguardP.kind == "userdata")
    {
        // For now, we don't really care about being accurate with userdata if the typeguard was using typeof.
        return refine(
            [](TypeId ty) -> bool
            {
                return get<ExternType>(ty);
            }
        );
    }

    if (!typeguardP.isTypeof)
        return addRefinement(refis, typeguardP.lvalue, errorRecoveryType(scope));

    auto typeFun = globalScope->lookupType(typeguardP.kind);
    if (!typeFun || !typeFun->typeParams.empty() || !typeFun->typePackParams.empty())
        return addRefinement(refis, typeguardP.lvalue, errorRecoveryType(scope));

    TypeId type = follow(typeFun->type);

    // You cannot refine to the top class type.
    if (type == builtinTypes->externType)
    {
        return addRefinement(refis, typeguardP.lvalue, errorRecoveryType(scope));
    }

    // We're only interested in the root type of any extern type.
    if (auto etv = get<ExternType>(type); !etv || (etv->parent != builtinTypes->externType && !hasTag(type, kTypeofRootTag)))
        return addRefinement(refis, typeguardP.lvalue, errorRecoveryType(scope));

    // This probably hints at breaking out type filtering functions from the predicate solver so that typeof is not tightly coupled with IsA.
    // Until then, we rewrite this to be the same as using IsA.
    return resolve(IsAPredicate{std::move(typeguardP.lvalue), typeguardP.location, type}, refis, scope, sense);
}

void TypeChecker::resolve(const EqPredicate& eqP, RefinementMap& refis, const ScopePtr& scope, bool sense)
{
    // This refinement will require success typing to do everything correctly. For now, we can get most of the way there.
    auto options = [](TypeId ty) -> std::vector<TypeId>
    {
        if (auto utv = get<UnionType>(follow(ty)))
            return std::vector<TypeId>(begin(utv), end(utv));
        return {ty};
    };

    std::vector<TypeId> rhs = options(eqP.type);

    if (sense && std::any_of(rhs.begin(), rhs.end(), isUndecidable))
        return; // Optimization: the other side has unknown types, so there's probably an overlap. Refining is no-op here.

    auto predicate = [&](TypeId option) -> std::optional<TypeId>
    {
        if (!sense && isNil(eqP.type))
            return (isUndecidable(option) || !isNil(option)) ? std::optional<TypeId>(option) : std::nullopt;

        if (maybeSingleton(eqP.type))
        {
            bool optionIsSubtype = canUnify(option, eqP.type, scope, eqP.location).empty();
            bool targetIsSubtype = canUnify(eqP.type, option, scope, eqP.location).empty();

            // terminology refresher:
            // - option is the type of the expression `x`, and
            // - eqP.type is the type of the expression `"hello"`
            //
            // "hello" == x where
            // x : "hello" | "world" -> x : "hello"
            // x : number | string   -> x : "hello"
            // x : number            -> x : never
            //
            // "hello" ~= x where
            // x : "hello" | "world" -> x : "world"
            // x : number | string   -> x : number | string
            // x : number            -> x : number

            // local variable works around an odd gcc 9.3 warning: <anonymous> may be used uninitialized
            std::optional<TypeId> nope = std::nullopt;

            if (sense)
            {
                if (optionIsSubtype && !targetIsSubtype)
                    return option;
                else if (!optionIsSubtype && targetIsSubtype)
                    return follow(eqP.type);
                else if (!optionIsSubtype && !targetIsSubtype)
                    return nope;
                else if (optionIsSubtype && targetIsSubtype)
                    return follow(eqP.type);
            }
            else
            {
                bool isOptionSingleton = get<SingletonType>(option);
                if (!isOptionSingleton)
                    return option;
                else if (optionIsSubtype && targetIsSubtype)
                    return nope;
            }
        }

        return option;
    };

    refineLValue(eqP.lvalue, refis, scope, predicate);
}

bool TypeChecker::isNonstrictMode() const
{
    return (currentModule->mode == Mode::Nonstrict) || (currentModule->mode == Mode::NoCheck);
}

std::vector<TypeId> TypeChecker::unTypePack(const ScopePtr& scope, TypePackId tp, size_t expectedLength, const Location& location)
{
    TypePackId expectedTypePack = addTypePack({});
    TypePack* expectedPack = getMutable<TypePack>(expectedTypePack);
    LUAU_ASSERT(expectedPack);
    for (size_t i = 0; i < expectedLength; ++i)
        expectedPack->head.push_back(freshType(scope));

    size_t oldErrorsSize = currentModule->errors.size();

    unify(tp, expectedTypePack, scope, location);

    // HACK: tryUnify would undo the changes to the expectedTypePack if the length mismatches, but
    // we want to tie up free types to be error types, so we do this instead.
    currentModule->errors.resize(oldErrorsSize);

    for (TypeId& tp : expectedPack->head)
        tp = follow(tp);

    return expectedPack->head;
}

std::vector<std::pair<Location, ScopePtr>> TypeChecker::getScopes() const
{
    return currentModule->scopes;
}

} // namespace Luau
