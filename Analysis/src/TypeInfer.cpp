// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"

#include "Luau/Common.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Parser.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Substitution.h"
#include "Luau/TopoSortStatements.h"
#include "Luau/ToString.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/TypeVar.h"

#include <deque>
#include <algorithm>

LUAU_FASTFLAGVARIABLE(DebugLuauMagicTypes, false)
LUAU_FASTINTVARIABLE(LuauTypeInferRecursionLimit, 0)
LUAU_FASTINTVARIABLE(LuauTypeInferTypePackLoopLimit, 0)
LUAU_FASTINTVARIABLE(LuauCheckRecursionLimit, 500)
LUAU_FASTFLAGVARIABLE(LuauIndexTablesWithIndexers, false)
LUAU_FASTFLAGVARIABLE(LuauGenericFunctions, false)
LUAU_FASTFLAGVARIABLE(LuauGenericVariadicsUnification, false)
LUAU_FASTFLAG(LuauKnowsTheDataModel3)
LUAU_FASTFLAG(LuauSecondTypecheckKnowsTheDataModel)
LUAU_FASTFLAGVARIABLE(LuauClassPropertyAccessAsString, false)
LUAU_FASTFLAGVARIABLE(LuauEqConstraint, false)
LUAU_FASTFLAGVARIABLE(LuauWeakEqConstraint, false) // Eventually removed as false.
LUAU_FASTFLAGVARIABLE(LuauImprovedTypeGuardPredicate2, false)
LUAU_FASTFLAG(LuauTraceRequireLookupChild)
LUAU_FASTFLAG(DebugLuauTrackOwningArena)
LUAU_FASTFLAGVARIABLE(LuauCloneCorrectlyBeforeMutatingTableType, false)
LUAU_FASTFLAGVARIABLE(LuauStoreMatchingOverloadFnType, false)
LUAU_FASTFLAGVARIABLE(LuauRankNTypes, false)
LUAU_FASTFLAGVARIABLE(LuauOrPredicate, false)
LUAU_FASTFLAGVARIABLE(LuauFixTableTypeAliasClone, false)
LUAU_FASTFLAGVARIABLE(LuauExtraNilRecovery, false)
LUAU_FASTFLAGVARIABLE(LuauMissingUnionPropertyError, false)
LUAU_FASTFLAGVARIABLE(LuauInferReturnAssertAssign, false)
LUAU_FASTFLAGVARIABLE(LuauRecursiveTypeParameterRestriction, false)
LUAU_FASTFLAGVARIABLE(LuauAddMissingFollow, false)
LUAU_FASTFLAGVARIABLE(LuauTypeGuardPeelsAwaySubclasses, false)
LUAU_FASTFLAGVARIABLE(LuauSlightlyMoreFlexibleBinaryPredicates, false)
LUAU_FASTFLAGVARIABLE(LuauInferFunctionArgsFix, false)
LUAU_FASTFLAGVARIABLE(LuauFollowInTypeFunApply, false)
LUAU_FASTFLAGVARIABLE(LuauIfElseExpressionAnalysisSupport, false)

namespace Luau
{

static bool typeCouldHaveMetatable(TypeId ty)
{
    return get<TableTypeVar>(follow(ty)) || get<ClassTypeVar>(follow(ty)) || get<MetatableTypeVar>(follow(ty));
}

static void defaultLuauPrintLine(const std::string& s)
{
    printf("%s\n", s.c_str());
}

using PrintLineProc = decltype(&defaultLuauPrintLine);

static PrintLineProc luauPrintLine = &defaultLuauPrintLine;

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
           name == "__metatable" || name == "__eq" || name == "__lt" || name == "__le" || name == "__mode";
}

TypeChecker::TypeChecker(ModuleResolver* resolver, InternalErrorReporter* iceHandler)
    : resolver(resolver)
    , iceHandler(iceHandler)
    , nilType(singletonTypes.nilType)
    , numberType(singletonTypes.numberType)
    , stringType(singletonTypes.stringType)
    , booleanType(
          FFlag::LuauImprovedTypeGuardPredicate2 ? singletonTypes.booleanType : globalTypes.addType(PrimitiveTypeVar(PrimitiveTypeVar::Boolean)))
    , threadType(FFlag::LuauImprovedTypeGuardPredicate2 ? singletonTypes.threadType : globalTypes.addType(PrimitiveTypeVar(PrimitiveTypeVar::Thread)))
    , anyType(singletonTypes.anyType)
    , errorType(singletonTypes.errorType)
    , optionalNumberType(globalTypes.addType(UnionTypeVar{{numberType, nilType}}))
    , anyTypePack(globalTypes.addTypePack(TypePackVar{VariadicTypePack{singletonTypes.anyType}, true}))
    , errorTypePack(globalTypes.addTypePack(TypePackVar{Unifiable::Error{}}))
{
    globalScope = std::make_shared<Scope>(globalTypes.addTypePack(TypePackVar{FreeTypePack{TypeLevel{}}}));

    globalScope->exportedTypeBindings["any"] = TypeFun{{}, anyType};
    globalScope->exportedTypeBindings["nil"] = TypeFun{{}, nilType};
    globalScope->exportedTypeBindings["number"] = TypeFun{{}, numberType};
    globalScope->exportedTypeBindings["string"] = TypeFun{{}, stringType};
    globalScope->exportedTypeBindings["boolean"] = TypeFun{{}, booleanType};
    globalScope->exportedTypeBindings["thread"] = TypeFun{{}, threadType};
}

ModulePtr TypeChecker::check(const SourceModule& module, Mode mode, std::optional<ScopePtr> environmentScope)
{
    currentModule.reset(new Module());
    currentModule->type = module.type;

    iceHandler->moduleName = module.name;

    ScopePtr parentScope = environmentScope.value_or(globalScope);
    ScopePtr moduleScope = std::make_shared<Scope>(parentScope);

    if (module.cyclic)
        moduleScope->returnType = addTypePack(TypePack{{anyType}, std::nullopt});
    else if (FFlag::LuauRankNTypes)
        moduleScope->returnType = freshTypePack(moduleScope);
    else
        moduleScope->returnType = DEPRECATED_freshTypePack(moduleScope, true);

    moduleScope->varargPack = anyTypePack;

    currentModule->scopes.push_back(std::make_pair(module.root->location, moduleScope));
    currentModule->mode = mode;

    currentModuleName = module.name;

    if (prepareModuleScope)
        prepareModuleScope(module.name, currentModule->getModuleScope());

    checkBlock(moduleScope, *module.root);

    if (get<FreeTypePack>(FFlag::LuauAddMissingFollow ? follow(moduleScope->returnType) : moduleScope->returnType))
        moduleScope->returnType = addTypePack(TypePack{{}, std::nullopt});
    else
        moduleScope->returnType = anyify(moduleScope, moduleScope->returnType, Location{});

    for (auto& [_, typeFun] : moduleScope->exportedTypeBindings)
        typeFun.type = anyify(moduleScope, typeFun.type, Location{});

    prepareErrorsForDisplay(currentModule->errors);

    bool encounteredFreeType = currentModule->clonePublicInterface();
    if (encounteredFreeType)
    {
        reportError(TypeError{module.root->location,
            GenericError{"Free types leaked into this module's public interface. This is an internal Luau error; please report it."}});
    }

    return std::move(currentModule);
}

void TypeChecker::check(const ScopePtr& scope, const AstStat& program)
{
    if (auto block = program.as<AstStatBlock>())
        check(scope, *block);
    else if (auto if_ = program.as<AstStatIf>())
        check(scope, *if_);
    else if (auto while_ = program.as<AstStatWhile>())
        check(scope, *while_);
    else if (auto repeat = program.as<AstStatRepeat>())
        check(scope, *repeat);
    else if (program.is<AstStatBreak>())
    {
    } // Nothing to do
    else if (program.is<AstStatContinue>())
    {
    } // Nothing to do
    else if (auto return_ = program.as<AstStatReturn>())
        check(scope, *return_);
    else if (auto expr = program.as<AstStatExpr>())
        checkExprPack(scope, *expr->expr);
    else if (auto local = program.as<AstStatLocal>())
        check(scope, *local);
    else if (auto for_ = program.as<AstStatFor>())
        check(scope, *for_);
    else if (auto forIn = program.as<AstStatForIn>())
        check(scope, *forIn);
    else if (auto assign = program.as<AstStatAssign>())
        check(scope, *assign);
    else if (auto assign = program.as<AstStatCompoundAssign>())
        check(scope, *assign);
    else if (program.is<AstStatFunction>())
        ice("Should not be calling two-argument check() on a function statement", program.location);
    else if (program.is<AstStatLocalFunction>())
        ice("Should not be calling two-argument check() on a function statement", program.location);
    else if (auto typealias = program.as<AstStatTypeAlias>())
        check(scope, *typealias);
    else if (auto global = program.as<AstStatDeclareGlobal>())
    {
        TypeId globalType = (FFlag::LuauRankNTypes ? resolveType(scope, *global->type) : resolveType(scope, *global->type, true));
        Name globalName(global->name.value);

        currentModule->declaredGlobals[globalName] = globalType;
        currentModule->getModuleScope()->bindings[global->name] = Binding{globalType, global->location};
    }
    else if (auto global = program.as<AstStatDeclareFunction>())
        check(scope, *global);
    else if (auto global = program.as<AstStatDeclareClass>())
        check(scope, *global);
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
    }
    else
        ice("Unknown AstStat");
}

// This particular overload is for do...end. If you need to not increase the scope level, use checkBlock directly.
void TypeChecker::check(const ScopePtr& scope, const AstStatBlock& block)
{
    ScopePtr child = childScope(scope, block.location);
    checkBlock(child, block);
}

void TypeChecker::checkBlock(const ScopePtr& scope, const AstStatBlock& block)
{
    RecursionCounter _rc(&checkRecursionCount);
    if (FInt::LuauCheckRecursionLimit > 0 && checkRecursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportErrorCodeTooComplex(block.location);
        return;
    }

    std::vector<AstStat*> sorted(block.body.data, block.body.data + block.body.size);
    toposort(sorted);

    for (const auto& stat : sorted)
    {
        if (const auto& typealias = stat->as<AstStatTypeAlias>())
            check(scope, *typealias, true);
    }

    auto protoIter = sorted.begin();
    auto checkIter = sorted.begin();

    std::unordered_map<AstStat*, std::pair<TypeId, ScopePtr>> functionDecls;

    auto checkBody = [&](AstStat* stat) {
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

    int subLevel = 0;

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
        // types for each functuion: before its body is checked, during checking its body,
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
        if (containsFunctionCall(**protoIter))
        {
            while (checkIter != protoIter)
            {
                checkBody(*checkIter);
                ++checkIter;
            }

            // We do check the current element, so advance checkIter beyond it.
            ++checkIter;
            check(scope, **protoIter);
        }
        else if (auto fun = (*protoIter)->as<AstStatFunction>())
        {
            auto pair = checkFunctionSignature(scope, subLevel, *fun->func, fun->name->location, std::nullopt);
            auto [funTy, funScope] = pair;

            functionDecls[*protoIter] = pair;
            ++subLevel;

            TypeId leftType = checkFunctionName(scope, *fun->name);
            unify(leftType, funTy, fun->location);
        }
        else if (auto fun = (*protoIter)->as<AstStatLocalFunction>())
        {
            auto pair = checkFunctionSignature(scope, subLevel, *fun->func, fun->name->location, std::nullopt);
            auto [funTy, funScope] = pair;

            functionDecls[*protoIter] = pair;
            ++subLevel;

            scope->bindings[fun->name] = {funTy, fun->name->location};
        }
        else
            check(scope, **protoIter);

        ++protoIter;
    }

    while (checkIter != sorted.end())
    {
        checkBody(*checkIter);
        ++checkIter;
    }

    checkBlockTypeAliases(scope, sorted);
}

LUAU_NOINLINE void TypeChecker::checkBlockTypeAliases(const ScopePtr& scope, std::vector<AstStat*>& sorted)
{
    for (const auto& stat : sorted)
    {
        if (const auto& typealias = stat->as<AstStatTypeAlias>())
        {
            auto& bindings = typealias->exported ? scope->exportedTypeBindings : scope->privateTypeBindings;

            Name name = typealias->name.value;
            TypeId type = bindings[name].type;
            if (get<FreeTypeVar>(FFlag::LuauAddMissingFollow ? follow(type) : type))
            {
                *asMutable(type) = ErrorTypeVar{};
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

    Predicate predicate{TypeGuardPredicate{std::move(*lvalue), expr.location, ssval, isTypeof}};
    if (expr.op == AstExprBinary::Op::CompareNe)
        return NotPredicate{{std::move(predicate)}};

    return predicate;
}

void TypeChecker::check(const ScopePtr& scope, const AstStatIf& statement)
{
    ExprResult<TypeId> result = checkExpr(scope, *statement.condition);

    ScopePtr ifScope = childScope(scope, statement.thenbody->location);
    reportErrors(resolve(result.predicates, ifScope, true));
    check(ifScope, *statement.thenbody);

    if (statement.elsebody)
    {
        ScopePtr elseScope = childScope(scope, statement.elsebody->location);
        resolve(result.predicates, elseScope, false);
        check(elseScope, *statement.elsebody);
    }
}

ErrorVec TypeChecker::canUnify(TypeId left, TypeId right, const Location& location)
{
    return canUnify_(left, right, location);
}

ErrorVec TypeChecker::canUnify(TypePackId left, TypePackId right, const Location& location)
{
    return canUnify_(left, right, location);
}

ErrorVec TypeChecker::canUnify(const std::vector<std::pair<TypeId, TypeId>>& seen, TypeId superTy, TypeId subTy, const Location& location)
{
    Unifier state = mkUnifier(seen, location);
    return state.canUnify(superTy, subTy);
}

template<typename Id>
ErrorVec TypeChecker::canUnify_(Id superTy, Id subTy, const Location& location)
{
    Unifier state = mkUnifier(location);
    return state.canUnify(superTy, subTy);
}

void TypeChecker::check(const ScopePtr& scope, const AstStatWhile& statement)
{
    ExprResult<TypeId> result = checkExpr(scope, *statement.condition);

    ScopePtr whileScope = childScope(scope, statement.body->location);
    reportErrors(resolve(result.predicates, whileScope, true));
    check(whileScope, *statement.body);
}

void TypeChecker::check(const ScopePtr& scope, const AstStatRepeat& statement)
{
    ScopePtr repScope = childScope(scope, statement.location);

    checkBlock(repScope, *statement.body);

    checkExpr(repScope, *statement.condition);
}

void TypeChecker::check(const ScopePtr& scope, const AstStatReturn& return_)
{
    std::vector<std::optional<TypeId>> expectedTypes;

    if (FFlag::LuauInferReturnAssertAssign)
    {
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
    }

    TypePackId retPack = checkExprList(scope, return_.location, return_.list, false, {}, expectedTypes).type;

    // HACK: Nonstrict mode gets a bit too smart and strict for us when we
    // start typechecking everything across module boundaries.
    if (isNonstrictMode() && follow(scope->returnType) == follow(currentModule->getModuleScope()->returnType))
    {
        ErrorVec errors = tryUnify(scope->returnType, retPack, return_.location);

        if (!errors.empty())
            currentModule->getModuleScope()->returnType = addTypePack({anyType});

        return;
    }

    unify(scope->returnType, retPack, return_.location, CountMismatch::Context::Return);
}

ErrorVec TypeChecker::tryUnify(TypeId left, TypeId right, const Location& location)
{
    return tryUnify_(left, right, location);
}

ErrorVec TypeChecker::tryUnify(TypePackId left, TypePackId right, const Location& location)
{
    return tryUnify_(left, right, location);
}

template<typename Id>
ErrorVec TypeChecker::tryUnify_(Id left, Id right, const Location& location)
{
    Unifier state = mkUnifier(location);
    state.tryUnify(left, right);

    if (!state.errors.empty())
        state.log.rollback();

    return state.errors;
}

void TypeChecker::check(const ScopePtr& scope, const AstStatAssign& assign)
{
    std::vector<std::optional<TypeId>> expectedTypes;

    if (FFlag::LuauInferReturnAssertAssign)
    {
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
                expectedTypes.push_back(checkLValue(scope, *dest));
            }
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

        if (!FFlag::LuauInferReturnAssertAssign || dest->is<AstExprLocal>() || dest->is<AstExprGlobal>())
            left = checkLValue(scope, *dest);
        else
            left = *expectedTypes[i];

        TypeId right = nullptr;

        Location loc = 0 == assign.values.size
                           ? assign.location
                           : i < assign.values.size ? assign.values.data[i]->location : assign.values.data[assign.values.size - 1]->location;

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
            if (get<Unifiable::Error>(*tail))
                right = errorType;
            else if (auto vtp = get<VariadicTypePack>(*tail))
                right = vtp->ty;
            else if (get<Unifiable::Free>(*tail))
            {
                *asMutable(*tail) = TypePack{{left}};
                growingPack = getMutable<TypePack>(*tail);
            }
        }

        if (right)
        {
            if (FFlag::LuauGenericFunctions && !maybeGeneric(left) && isGeneric(right))
                right = instantiate(scope, right, loc);

            if (!FFlag::LuauGenericFunctions && get<FunctionTypeVar>(FFlag::LuauAddMissingFollow ? follow(left) : left) &&
                get<FunctionTypeVar>(FFlag::LuauAddMissingFollow ? follow(right) : right))
                right = instantiate(scope, right, loc);

            // Setting a table entry to nil doesn't mean nil is the type of the indexer, it is just deleting the entry
            const TableTypeVar* destTableTypeReceivingNil = nullptr;
            if (auto indexExpr = dest->as<AstExprIndexExpr>(); isNil(right) && indexExpr)
                destTableTypeReceivingNil = getTableType(checkExpr(scope, *indexExpr->expr).type);

            if (!destTableTypeReceivingNil || !destTableTypeReceivingNil->indexer)
            {
                // In nonstrict mode, any assignments where the lhs is free and rhs isn't a function, we give it any typevar.
                if (isNonstrictMode() && get<FreeTypeVar>(FFlag::LuauAddMissingFollow ? follow(left) : left) && !get<FunctionTypeVar>(follow(right)))
                    unify(left, anyType, loc);
                else
                    unify(left, right, loc);
            }
        }
    }
}

void TypeChecker::check(const ScopePtr& scope, const AstStatCompoundAssign& assign)
{
    AstExprBinary expr(assign.location, assign.op, assign.var, assign.value);

    TypeId left = checkExpr(scope, *expr.left).type;
    TypeId right = checkExpr(scope, *expr.right).type;

    TypeId result = checkBinaryOperation(scope, expr, left, right);

    unify(left, result, assign.location);
}

void TypeChecker::check(const ScopePtr& scope, const AstStatLocal& local)
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
            ty = (FFlag::LuauRankNTypes ? resolveType(scope, *annotation) : resolveType(scope, *annotation, true));

            // If the annotation type has an error, treat it as if there was no annotation
            if (get<ErrorTypeVar>(follow(ty)))
                ty = nullptr;
        }

        if (!ty)
            ty = rhsIsTable ? (FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(scope, true))
                            : isNonstrictMode() ? anyType : (FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(scope, true));

        varBindings.emplace_back(vars[i], Binding{ty, vars[i]->location});

        variableTypes.push_back(ty);
        expectedTypes.push_back(ty);

        if (FFlag::LuauGenericFunctions)
            instantiateGenerics.push_back(annotation != nullptr && !maybeGeneric(ty));
        else
            instantiateGenerics.push_back(annotation != nullptr && get<FunctionTypeVar>(FFlag::LuauAddMissingFollow ? follow(ty) : ty));
    }

    if (local.values.size > 0)
    {
        TypePackId variablePack = addTypePack(variableTypes, FFlag::LuauRankNTypes ? freshTypePack(scope) : DEPRECATED_freshTypePack(scope, true));
        TypePackId valuePack =
            checkExprList(scope, local.location, local.values, /* substituteFreeForNil= */ true, instantiateGenerics, expectedTypes).type;

        Unifier state = mkUnifier(local.location);
        state.ctx = CountMismatch::Result;
        state.tryUnify(variablePack, valuePack);
        reportErrors(state.errors);

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
                    TableTypeVar* ttv = getMutable<TableTypeVar>(follow(*ty));
                    if (ttv && !ttv->name && scope == currentModule->getModuleScope())
                        ttv->syntheticName = vars[0]->name.value;
                }
                else if (const AstExprCall* call = rhs->as<AstExprCall>())
                {
                    if (const AstExprGlobal* global = call->func->as<AstExprGlobal>(); global && global->name == "setmetatable")
                    {
                        MetatableTypeVar* mtv = getMutable<MetatableTypeVar>(follow(*ty));
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

                if (auto moduleInfo = resolver->resolveModuleInfo(currentModuleName, *require))
                {
                    const Name name{local.vars.data[i]->name.value};

                    if (ModulePtr module = resolver->getModule(moduleInfo->name))
                        scope->importedTypeBindings[name] = module->getModuleScope()->exportedTypeBindings;

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
}

void TypeChecker::check(const ScopePtr& scope, const AstStatFor& expr)
{
    ScopePtr loopScope = childScope(scope, expr.location);

    TypeId loopVarType = numberType;
    if (expr.var->annotation)
        unify(resolveType(scope, *expr.var->annotation), loopVarType, expr.location);

    loopScope->bindings[expr.var] = {loopVarType, expr.var->location};

    if (!expr.from)
        ice("Bad AstStatFor has no from expr");

    if (!expr.to)
        ice("Bad AstStatFor has no to expr");

    unify(loopVarType, checkExpr(loopScope, *expr.from).type, expr.from->location);
    unify(loopVarType, checkExpr(loopScope, *expr.to).type, expr.to->location);

    if (expr.step)
        unify(loopVarType, checkExpr(loopScope, *expr.step).type, expr.step->location);

    check(loopScope, *expr.body);
}

void TypeChecker::check(const ScopePtr& scope, const AstStatForIn& forin)
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
    //      next<K, V>(t: Table<K, V>, index: K | nil) -> (K, V)
    // however, pairs and ipairs are quite messy, but they both share the same types
    // pairs returns 'next, t, nil', thus the type would be
    //      pairs<K, V>(t: Table<K, V>) -> ((Table<K, V>, K | nil) -> (K, V), Table<K, V>, K | nil)
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
        if (!FFlag::LuauRankNTypes)
            callRetPack = DEPRECATED_instantiate(scope, callRetPack, exprCall->location);
        callRetPack = follow(callRetPack);

        if (get<Unifiable::Free>(callRetPack))
        {
            iterTy = freshType(scope);
            unify(addTypePack({{iterTy}, freshTypePack(scope)}), callRetPack, forin.location);
        }
        else if (get<Unifiable::Error>(callRetPack) || !first(callRetPack))
        {
            for (TypeId var : varTypes)
                unify(var, errorType, forin.location);

            return check(loopScope, *forin.body);
        }
        else
        {
            iterTy = *first(callRetPack);
            if (FFlag::LuauRankNTypes)
                iterTy = instantiate(scope, iterTy, exprCall->location);
        }
    }
    else
    {
        iterTy = follow(instantiate(scope, checkExpr(scope, *firstValue).type, firstValue->location));
    }

    const FunctionTypeVar* iterFunc = get<FunctionTypeVar>(iterTy);
    if (!iterFunc)
    {
        TypeId varTy = get<AnyTypeVar>(iterTy) ? anyType : errorType;

        for (TypeId var : varTypes)
            unify(var, varTy, forin.location);

        if (!get<ErrorTypeVar>(iterTy) && !get<AnyTypeVar>(iterTy) && !get<FreeTypeVar>(iterTy))
            reportError(TypeError{firstValue->location, TypeMismatch{globalScope->bindings[AstName{"next"}].typeId, iterTy}});

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
            std::vector<TypeId> argTypes = std::vector<TypeId>(types.begin() + 1, types.end());
            argPack = addTypePack(TypePackVar{TypePack{std::move(argTypes), tail}});
        }
        else
        {
            // Check if iterator function accepts 0 arguments
            argPack = addTypePack(TypePack{});
        }

        Unifier state = mkUnifier(firstValue->location);
        checkArgumentList(loopScope, state, argPack, iterFunc->argTypes, /*argLocations*/ {});

        reportErrors(state.errors);
    }

    TypePackId varPack = addTypePack(TypePackVar{TypePack{varTypes, freshTypePack(scope)}});

    if (forin.values.size >= 2)
    {
        AstArray<AstExpr*> arguments{forin.values.data + 1, forin.values.size - 1};

        Position start = firstValue->location.begin;
        Position end = values[forin.values.size - 1]->location.end;
        AstExprCall exprCall{Location(start, end), firstValue, arguments, /* self= */ false, Location()};

        TypePackId retPack = checkExprPack(scope, exprCall).type;
        unify(varPack, retPack, forin.location);
    }
    else
        unify(varPack, iterFunc->retType, forin.location);

    check(loopScope, *forin.body);
}

void TypeChecker::check(const ScopePtr& scope, TypeId ty, const ScopePtr& funScope, const AstStatFunction& function)
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
        // in case this function has a differing signature. The signature discrepency will be caught in checkBlock.
        if (previouslyDefined)
            globalBindings[name] = oldBinding;
        else
            globalBindings[name] = {quantify(funScope, ty, exprName->location), exprName->location};

        return;
    }
    else if (auto name = function.name->as<AstExprLocal>())
    {
        scope->bindings[name->local] = {ty, name->local->location};

        checkFunctionBody(funScope, ty, *function.func);

        scope->bindings[name->local] = {anyIfNonstrict(quantify(funScope, ty, name->local->location)), name->local->location};
        return;
    }
    else if (function.func->self)
    {
        AstExprIndexName* indexName = function.name->as<AstExprIndexName>();
        if (!indexName)
            ice("member function declaration has malformed name expression");

        TypeId selfTy = checkExpr(scope, *indexName->expr).type;
        TableTypeVar* tableSelf = getMutableTableType(selfTy);
        if (!tableSelf)
        {
            if (isTableIntersection(selfTy))
                reportError(TypeError{function.location, CannotExtendTable{selfTy, CannotExtendTable::Property, indexName->index.value}});
            else if (!get<ErrorTypeVar>(selfTy) && !get<AnyTypeVar>(selfTy))
                reportError(TypeError{function.location, OnlyTablesCanHaveMethods{selfTy}});
        }
        else if (tableSelf->state == TableState::Sealed)
            reportError(TypeError{function.location, CannotExtendTable{selfTy, CannotExtendTable::Property, indexName->index.value}});

        ty = follow(ty);

        if (tableSelf && !selfTy->persistent)
            tableSelf->props[indexName->index.value] = {ty, /* deprecated */ false, {}, indexName->indexLocation};

        const FunctionTypeVar* funTy = get<FunctionTypeVar>(ty);
        if (!funTy)
            ice("Methods should be functions");

        std::optional<TypeId> arg0 = first(funTy->argTypes);
        if (!arg0)
            ice("Methods should always have at least 1 argument (self)");

        checkFunctionBody(funScope, ty, *function.func);

        if (tableSelf && !selfTy->persistent)
            tableSelf->props[indexName->index.value] = {
                follow(quantify(funScope, ty, indexName->indexLocation)), /* deprecated */ false, {}, indexName->indexLocation};
    }
    else
    {
        auto [leftType, leftTypeBinding] = checkLValueBinding(scope, *function.name);

        checkFunctionBody(funScope, ty, *function.func);

        unify(leftType, ty, function.location);

        if (leftTypeBinding)
            *leftTypeBinding = follow(quantify(funScope, leftType, function.name->location));
    }
}

void TypeChecker::check(const ScopePtr& scope, TypeId ty, const ScopePtr& funScope, const AstStatLocalFunction& function)
{
    Name name = function.name->name.value;

    scope->bindings[function.name] = {ty, function.location};

    checkFunctionBody(funScope, ty, *function.func);

    if (FFlag::LuauGenericFunctions)
        scope->bindings[function.name] = {quantify(funScope, ty, function.name->location), function.name->location};
    else
        scope->bindings[function.name] = {quantify(scope, ty, function.name->location), function.name->location};
}

void TypeChecker::check(const ScopePtr& scope, const AstStatTypeAlias& typealias, bool forwardDeclare)
{
    // This function should be called at most twice for each type alias.
    // Once with forwardDeclare, and once without.
    Name name = typealias.name.value;

    std::optional<TypeFun> binding;
    if (auto it = scope->exportedTypeBindings.find(name); it != scope->exportedTypeBindings.end())
        binding = it->second;
    else if (auto it = scope->privateTypeBindings.find(name); it != scope->privateTypeBindings.end())
        binding = it->second;

    auto& bindingsMap = typealias.exported ? scope->exportedTypeBindings : scope->privateTypeBindings;

    if (forwardDeclare)
    {
        if (binding)
        {
            Location location = scope->typeAliasLocations[name];
            reportError(TypeError{typealias.location, DuplicateTypeDefinition{name, location}});
            bindingsMap[name] = TypeFun{binding->typeParams, errorType};
        }
        else
        {
            ScopePtr aliasScope = childScope(scope, typealias.location);

            std::vector<TypeId> generics;
            for (AstName generic : typealias.generics)
            {
                Name n = generic.value;

                // These generics are the only thing that will ever be added to aliasScope, so we can be certain that
                // a collision can only occur when two generic typevars have the same name.
                if (aliasScope->privateTypeBindings.end() != aliasScope->privateTypeBindings.find(n))
                {
                    // TODO(jhuelsman): report the exact span of the generic type parameter whose name is a duplicate.
                    reportError(TypeError{typealias.location, DuplicateGenericParameter{n}});
                }

                TypeId g;
                if (FFlag::LuauRecursiveTypeParameterRestriction)
                {
                    TypeId& cached = scope->typeAliasParameters[n];
                    if (!cached)
                        cached = addType(GenericTypeVar{aliasScope->level, n});
                    g = cached;
                }
                else
                    g = addType(GenericTypeVar{aliasScope->level, n});
                generics.push_back(g);
                aliasScope->privateTypeBindings[n] = TypeFun{{}, g};
            }

            TypeId ty = (FFlag::LuauRankNTypes ? freshType(aliasScope) : DEPRECATED_freshType(scope, true));
            FreeTypeVar* ftv = getMutable<FreeTypeVar>(ty);
            LUAU_ASSERT(ftv);
            ftv->forwardedTypeAlias = true;
            bindingsMap[name] = {std::move(generics), ty};
        }
    }
    else
    {
        if (!binding)
            ice("Not predeclared");

        ScopePtr aliasScope = childScope(scope, typealias.location);

        for (TypeId ty : binding->typeParams)
        {
            auto generic = get<GenericTypeVar>(ty);
            LUAU_ASSERT(generic);
            aliasScope->privateTypeBindings[generic->name] = TypeFun{{}, ty};
        }

        TypeId ty = (FFlag::LuauRankNTypes ? resolveType(aliasScope, *typealias.type) : resolveType(aliasScope, *typealias.type, true));
        if (auto ttv = getMutable<TableTypeVar>(follow(ty)))
        {
            // If the table is already named and we want to rename the type function, we have to bind new alias to a copy
            if (ttv->name)
            {
                // Copy can be skipped if this is an identical alias
                if (!FFlag::LuauFixTableTypeAliasClone || ttv->name != name || ttv->instantiatedTypeParams != binding->typeParams)
                {
                    // This is a shallow clone, original recursive links to self are not updated
                    TableTypeVar clone = TableTypeVar{ttv->props, ttv->indexer, ttv->level, ttv->state};

                    clone.methodDefinitionLocations = ttv->methodDefinitionLocations;
                    clone.definitionModuleName = ttv->definitionModuleName;

                    clone.name = name;
                    clone.instantiatedTypeParams = binding->typeParams;

                    ty = addType(std::move(clone));
                }
            }
            else
            {
                ttv->name = name;
                ttv->instantiatedTypeParams = binding->typeParams;
            }
        }
        else if (auto mtv = getMutable<MetatableTypeVar>(follow(ty)))
            mtv->syntheticName = name;

        unify(bindingsMap[name].type, ty, typealias.location);
    }
}

void TypeChecker::check(const ScopePtr& scope, const AstStatDeclareClass& declaredClass)
{
    std::optional<TypeId> superTy = std::nullopt;
    if (declaredClass.superName)
    {
        Name superName = Name(declaredClass.superName->value);
        std::optional<TypeFun> lookupType = scope->lookupType(superName);

        if (!lookupType)
        {
            reportError(declaredClass.location, UnknownSymbol{superName, UnknownSymbol::Type});
            return;
        }

        // We don't have generic classes, so this assertion _should_ never be hit.
        LUAU_ASSERT(lookupType->typeParams.size() == 0);
        superTy = lookupType->type;

        if (FFlag::LuauAddMissingFollow)
        {
            if (!get<ClassTypeVar>(follow(*superTy)))
            {
                reportError(declaredClass.location, GenericError{format("Cannot use non-class type '%s' as a superclass of class '%s'",
                                                        superName.c_str(), declaredClass.name.value)});

                return;
            }
        }
        else
        {
            if (const ClassTypeVar* superCtv = get<ClassTypeVar>(*superTy); !superCtv)
            {
                reportError(declaredClass.location, GenericError{format("Cannot use non-class type '%s' as a superclass of class '%s'",
                                                        superName.c_str(), declaredClass.name.value)});

                return;
            }
        }
    }

    Name className(declaredClass.name.value);

    TypeId classTy = addType(ClassTypeVar(className, {}, superTy, std::nullopt, {}, {}));
    ClassTypeVar* ctv = getMutable<ClassTypeVar>(classTy);

    TypeId metaTy = addType(TableTypeVar{TableState::Sealed, scope->level});
    TableTypeVar* metatable = getMutable<TableTypeVar>(metaTy);

    ctv->metatable = metaTy;

    scope->exportedTypeBindings[className] = TypeFun{{}, classTy};

    for (const AstDeclaredClassProp& prop : declaredClass.props)
    {
        Name propName(prop.name.value);
        TypeId propTy = resolveType(scope, *prop.ty);

        bool assignToMetatable = isMetamethod(propName);

        // Function types always take 'self', but this isn't reflected in the
        // parsed annotation. Add it here.
        if (prop.isMethod)
        {
            if (FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(propTy))
            {
                ftv->argNames.insert(ftv->argNames.begin(), FunctionArgument{"self", {}});
                ftv->argTypes = addTypePack(TypePack{{classTy}, ftv->argTypes});
            }
        }

        if (ctv->props.count(propName) == 0)
        {
            if (assignToMetatable)
                metatable->props[propName] = {propTy};
            else
                ctv->props[propName] = {propTy};
        }
        else
        {
            TypeId currentTy = assignToMetatable ? metatable->props[propName].type : ctv->props[propName].type;

            // We special-case this logic to keep the intersection flat; otherwise we
            // would create a ton of nested intersection types.
            if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(currentTy))
            {
                std::vector<TypeId> options = itv->parts;
                options.push_back(propTy);
                TypeId newItv = addType(IntersectionTypeVar{std::move(options)});

                if (assignToMetatable)
                    metatable->props[propName] = {newItv};
                else
                    ctv->props[propName] = {newItv};
            }
            else if (get<FunctionTypeVar>(currentTy))
            {
                TypeId intersection = addType(IntersectionTypeVar{{currentTy, propTy}});

                if (assignToMetatable)
                    metatable->props[propName] = {intersection};
                else
                    ctv->props[propName] = {intersection};
            }
            else
            {
                reportError(declaredClass.location, GenericError{format("Cannot overload non-function class member '%s'", propName.c_str())});
            }
        }
    }
}

void TypeChecker::check(const ScopePtr& scope, const AstStatDeclareFunction& global)
{
    ScopePtr funScope = childFunctionScope(scope, global.location);

    auto [generics, genericPacks] = createGenericTypes(funScope, global, global.generics, global.genericPacks);

    TypePackId argPack = resolveTypePack(funScope, global.params);
    TypePackId retPack = resolveTypePack(funScope, global.retTypes);
    TypeId fnType = addType(FunctionTypeVar{funScope->level, generics, genericPacks, argPack, retPack});
    FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(fnType);

    ftv->argNames.reserve(global.paramNames.size);
    for (const auto& el : global.paramNames)
        ftv->argNames.push_back(FunctionArgument{el.first.value, el.second});

    Name fnName(global.name.value);

    currentModule->declaredGlobals[fnName] = fnType;
    currentModule->getModuleScope()->bindings[global.name] = Binding{fnType, global.location};
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExpr& expr, std::optional<TypeId> expectedType)
{
    RecursionCounter _rc(&checkRecursionCount);
    if (FInt::LuauCheckRecursionLimit > 0 && checkRecursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportErrorCodeTooComplex(expr.location);
        return {errorType};
    }

    ExprResult<TypeId> result;

    if (auto a = expr.as<AstExprGroup>())
        result = checkExpr(scope, *a->expr);
    else if (expr.is<AstExprConstantNil>())
        result = {nilType};
    else if (expr.is<AstExprConstantBool>())
        result = {booleanType};
    else if (expr.is<AstExprConstantNumber>())
        result = {numberType};
    else if (expr.is<AstExprConstantString>())
        result = {stringType};
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
        result = checkExpr(scope, *a);
    else if (auto a = expr.as<AstExprTypeAssertion>())
        result = checkExpr(scope, *a);
    else if (auto a = expr.as<AstExprError>())
        result = checkExpr(scope, *a);
    else if (auto a = expr.as<AstExprIfElse>())
    {
        if (FFlag::LuauIfElseExpressionAnalysisSupport)
        {
            result = checkExpr(scope, *a);
        }
        else
        {
            // Note: When the fast flag is disabled we can't skip the handling of AstExprIfElse
            // because we would generate an ICE.  We also can't use the default value
            // of result, because it will lead to a compiler crash.
            // Note: LuauIfElseExpressionBaseSupport can be used to disable parser support
            // for if-else expressions which will mean this node type is never created.
            result = {anyType};
        }
    }
    else
        ice("Unhandled AstExpr?");

    result.type = follow(result.type);

    if (FFlag::LuauStoreMatchingOverloadFnType)
    {
        currentModule->astTypes.try_emplace(&expr, result.type);
    }
    else
    {
        currentModule->astTypes[&expr] = result.type;
    }

    if (expectedType)
        currentModule->astExpectedTypes[&expr] = *expectedType;

    return result;
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprLocal& expr)
{
    std::optional<LValue> lvalue = tryGetLValue(expr);
    LUAU_ASSERT(lvalue); // Guaranteed to not be nullopt - AstExprLocal is an LValue.

    if (std::optional<TypeId> ty = resolveLValue(scope, *lvalue))
        return {*ty, {TruthyPredicate{std::move(*lvalue), expr.location}}};

    // TODO: tempting to ice here, but this breaks very often because our toposort doesn't enforce this constraint
    // ice("AstExprLocal exists but no binding definition for it?", expr.location);
    reportError(TypeError{expr.location, UnknownSymbol{expr.local->name.value, UnknownSymbol::Binding}});
    return {errorType};
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprGlobal& expr)
{
    std::optional<LValue> lvalue = tryGetLValue(expr);
    LUAU_ASSERT(lvalue); // Guaranteed to not be nullopt - AstExprGlobal is an LValue.

    if (std::optional<TypeId> ty = resolveLValue(scope, *lvalue))
        return {*ty, {TruthyPredicate{std::move(*lvalue), expr.location}}};

    reportError(TypeError{expr.location, UnknownSymbol{expr.name.value, UnknownSymbol::Binding}});
    return {errorType};
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprVarargs& expr)
{
    TypePackId varargPack = checkExprPack(scope, expr).type;

    if (get<TypePack>(varargPack))
    {
        std::vector<TypeId> types = flatten(varargPack).first;
        return {!types.empty() ? types[0] : nilType};
    }
    else if (auto ftp = get<FreeTypePack>(varargPack))
    {
        TypeId head = (FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(scope, ftp->DEPRECATED_canBeGeneric));
        TypePackId tail = (FFlag::LuauRankNTypes ? freshTypePack(scope) : DEPRECATED_freshTypePack(scope, ftp->DEPRECATED_canBeGeneric));
        *asMutable(varargPack) = TypePack{{head}, tail};
        return {head};
    }
    if (get<ErrorTypeVar>(varargPack))
        return {errorType};
    else if (auto vtp = get<VariadicTypePack>(varargPack))
        return {vtp->ty};
    else if (FFlag::LuauGenericVariadicsUnification && get<Unifiable::Generic>(varargPack))
    {
        // TODO: Better error?
        reportError(expr.location, GenericError{"Trying to get a type from a variadic type parameter"});
        return {errorType};
    }
    else
        ice("Unknown TypePack type in checkExpr(AstExprVarargs)!");
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprCall& expr)
{
    ExprResult<TypePackId> result = checkExprPack(scope, expr);
    TypePackId retPack = follow(result.type);

    if (auto pack = get<TypePack>(retPack))
    {
        return {pack->head.empty() ? nilType : pack->head[0], std::move(result.predicates)};
    }
    else if (auto ftp = get<Unifiable::Free>(retPack))
    {
        TypeId head = (FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(scope, ftp->DEPRECATED_canBeGeneric));
        TypePackId pack = addTypePack(TypePackVar{TypePack{{head}, freshTypePack(scope)}});
        unify(retPack, pack, expr.location);
        return {head, std::move(result.predicates)};
    }
    if (get<Unifiable::Error>(retPack))
        return {errorType, std::move(result.predicates)};
    else if (auto vtp = get<VariadicTypePack>(retPack))
        return {vtp->ty, std::move(result.predicates)};
    else if (get<Unifiable::Generic>(retPack))
        ice("Unexpected abstract type pack!");
    else
        ice("Unknown TypePack type!");
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprIndexName& expr)
{
    Name name = expr.index.value;

    // Redundant call if we find a refined lvalue, but this function must be called in order to recursively populate astTypes.
    TypeId lhsType = checkExpr(scope, *expr.expr).type;

    if (std::optional<LValue> lvalue = tryGetLValue(expr))
        if (std::optional<TypeId> ty = resolveLValue(scope, *lvalue))
            return {*ty, {TruthyPredicate{std::move(*lvalue), expr.location}}};

    if (FFlag::LuauExtraNilRecovery)
        lhsType = stripFromNilAndReport(lhsType, expr.expr->location);

    if (std::optional<TypeId> ty = getIndexTypeFromType(scope, lhsType, name, expr.location, true))
        return {*ty};

    if (!FFlag::LuauMissingUnionPropertyError)
        reportError(expr.indexLocation, UnknownProperty{lhsType, expr.index.value});

    if (!FFlag::LuauExtraNilRecovery)
    {
        // Try to recover using a union without 'nil' options
        if (std::optional<TypeId> strippedUnion = tryStripUnionFromNil(lhsType))
        {
            if (std::optional<TypeId> ty = getIndexTypeFromType(scope, *strippedUnion, name, expr.location, false))
                return {*ty};
        }
    }

    return {errorType};
}

std::optional<TypeId> TypeChecker::findTablePropertyRespectingMeta(TypeId lhsType, Name name, const Location& location)
{
    ErrorVec errors;
    auto result = Luau::findTablePropertyRespectingMeta(errors, globalScope, lhsType, name, location);
    reportErrors(errors);
    return result;
}

std::optional<TypeId> TypeChecker::findMetatableEntry(TypeId type, std::string entry, const Location& location)
{
    ErrorVec errors;
    auto result = Luau::findMetatableEntry(errors, globalScope, type, entry, location);
    reportErrors(errors);
    return result;
}

std::optional<TypeId> TypeChecker::getIndexTypeFromType(
    const ScopePtr& scope, TypeId type, const Name& name, const Location& location, bool addErrors)
{
    type = follow(type);

    if (get<ErrorTypeVar>(type) || get<AnyTypeVar>(type))
        return type;

    tablify(type);

    const PrimitiveTypeVar* primitiveType = get<PrimitiveTypeVar>(type);
    if (primitiveType && primitiveType->type == PrimitiveTypeVar::String)
    {
        if (std::optional<TypeId> mtIndex = findMetatableEntry(type, "__index", location))
            type = *mtIndex;
    }

    if (TableTypeVar* tableType = getMutableTableType(type))
    {
        const auto& it = tableType->props.find(name);
        if (it != tableType->props.end())
            return it->second.type;
        else if (auto indexer = tableType->indexer)
        {
            tryUnify(indexer->indexType, stringType, location);
            return indexer->indexResultType;
        }
        else if (tableType->state == TableState::Free)
        {
            TypeId result = (FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(scope, true));
            tableType->props[name] = {result};
            return result;
        }

        auto found = findTablePropertyRespectingMeta(type, name, location);
        if (found)
            return *found;
    }
    else if (const ClassTypeVar* cls = get<ClassTypeVar>(type))
    {
        const Property* prop = lookupClassProp(cls, name);
        if (prop)
            return prop->type;
    }
    else if (const UnionTypeVar* utv = get<UnionTypeVar>(type))
    {
        if (FFlag::LuauMissingUnionPropertyError)
        {
            std::vector<TypeId> goodOptions;
            std::vector<TypeId> badOptions;

            for (TypeId t : utv)
            {
                RecursionLimiter _rl(&recursionCount, FInt::LuauTypeInferRecursionLimit);

                if (std::optional<TypeId> ty = getIndexTypeFromType(scope, t, name, location, false))
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
                        reportError(location, MissingUnionProperty{type, badOptions, name});
                }
                return std::nullopt;
            }

            std::vector<TypeId> result = reduceUnion(goodOptions);

            if (result.size() == 1)
                return result[0];

            return addType(UnionTypeVar{std::move(result)});
        }
        else
        {
            std::vector<TypeId> options;

            for (TypeId t : utv->options)
            {
                RecursionLimiter _rl(&recursionCount, FInt::LuauTypeInferRecursionLimit);

                if (std::optional<TypeId> ty = getIndexTypeFromType(scope, t, name, location, false))
                    options.push_back(*ty);
                else
                    return std::nullopt;
            }

            std::vector<TypeId> result = reduceUnion(options);

            if (result.size() == 1)
                return result[0];

            return addType(UnionTypeVar{std::move(result)});
        }
    }
    else if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(type))
    {
        std::vector<TypeId> parts;

        for (TypeId t : itv->parts)
        {
            RecursionLimiter _rl(&recursionCount, FInt::LuauTypeInferRecursionLimit);

            if (std::optional<TypeId> ty = getIndexTypeFromType(scope, t, name, location, false))
                parts.push_back(*ty);
        }

        // If no parts of the intersection had the property we looked up for, it never existed at all.
        if (parts.empty())
        {
            if (FFlag::LuauMissingUnionPropertyError && addErrors)
                reportError(location, UnknownProperty{type, name});
            return std::nullopt;
        }

        // TODO(amccord): Write some logic to correctly handle intersections. CLI-34659
        std::vector<TypeId> result = reduceUnion(parts);

        if (result.size() == 1)
            return result[0];

        return addType(IntersectionTypeVar{result});
    }

    if (FFlag::LuauMissingUnionPropertyError && addErrors)
        reportError(location, UnknownProperty{type, name});

    return std::nullopt;
}

std::vector<TypeId> TypeChecker::reduceUnion(const std::vector<TypeId>& types)
{
    std::set<TypeId> s;

    for (TypeId t : types)
    {
        if (const UnionTypeVar* utv = get<UnionTypeVar>(follow(t)))
        {
            std::vector<TypeId> r = reduceUnion(utv->options);
            for (TypeId ty : r)
                s.insert(ty);
        }
        else
            s.insert(t);
    }

    // If any of them are ErrorTypeVars/AnyTypeVars, decay into them.
    for (TypeId t : s)
    {
        t = follow(t);
        if (get<ErrorTypeVar>(t) || get<AnyTypeVar>(t))
            return {t};
    }

    std::vector<TypeId> r(s.begin(), s.end());
    std::sort(r.begin(), r.end());
    return r;
}

std::optional<TypeId> TypeChecker::tryStripUnionFromNil(TypeId ty)
{
    if (const UnionTypeVar* utv = get<UnionTypeVar>(ty))
    {
        bool hasNil = false;

        for (TypeId option : utv)
        {
            if (isNil(option))
            {
                hasNil = true;
                break;
            }
        }

        if (!hasNil)
            return ty;

        std::vector<TypeId> result;

        for (TypeId option : utv)
        {
            if (!isNil(option))
                result.push_back(option);
        }

        if (result.empty())
            return std::nullopt;

        return result.size() == 1 ? result[0] : addType(UnionTypeVar{std::move(result)});
    }

    return std::nullopt;
}

TypeId TypeChecker::stripFromNilAndReport(TypeId ty, const Location& location)
{
    if (isOptional(ty))
    {
        if (std::optional<TypeId> strippedUnion = tryStripUnionFromNil(follow(ty)))
        {
            reportError(location, OptionalValueAccess{ty});
            return follow(*strippedUnion);
        }
    }

    return ty;
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprIndexExpr& expr)
{
    return {checkLValue(scope, expr)};
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprFunction& expr, std::optional<TypeId> expectedType)
{
    auto [funTy, funScope] = checkFunctionSignature(scope, 0, expr, std::nullopt, expectedType);

    checkFunctionBody(funScope, funTy, expr);

    return {quantify(funScope, funTy, expr.location)};
}

TypeId TypeChecker::checkExprTable(
    const ScopePtr& scope, const AstExprTable& expr, const std::vector<std::pair<TypeId, TypeId>>& fieldTypes, std::optional<TypeId> expectedType)
{
    TableTypeVar::Props props;
    std::optional<TableIndexer> indexer;

    const TableTypeVar* expectedTable = nullptr;

    if (expectedType)
    {
        if (auto ttv = get<TableTypeVar>(follow(*expectedType)))
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
                unify(indexer->indexResultType, valueType, value->location);
            else
                indexer = TableIndexer{numberType, anyIfNonstrict(valueType)};
        }
        else if (item.kind == AstExprTable::Item::Record || item.kind == AstExprTable::Item::General)
        {
            if (auto key = k->as<AstExprConstantString>())
            {
                TypeId exprType = follow(valueType);
                if (isNonstrictMode() && !getTableType(exprType) && !get<FunctionTypeVar>(exprType))
                    exprType = anyType;

                props[key->value.data] = {exprType, /* deprecated */ false, {}, k->location};
            }
            else
            {
                if (expectedTable && !indexer)
                    indexer = expectedTable->indexer;

                if (indexer)
                {
                    unify(indexer->indexType, keyType, k->location);
                    unify(indexer->indexResultType, valueType, value->location);
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

    TableState state = (expr.items.size == 0 || isNonstrictMode()) ? TableState::Unsealed : TableState::Sealed;
    TableTypeVar table = TableTypeVar{std::move(props), indexer, scope->level, state};
    table.definitionModuleName = currentModuleName;
    return addType(table);
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprTable& expr, std::optional<TypeId> expectedType)
{
    RecursionLimiter _rl(&recursionCount, FInt::LuauTypeInferRecursionLimit);
    std::vector<std::pair<TypeId, TypeId>> fieldTypes(expr.items.size);

    const TableTypeVar* expectedTable = nullptr;
    std::optional<TypeId> expectedIndexType;
    std::optional<TypeId> expectedIndexResultType;

    if (expectedType)
    {
        if (auto ttv = get<TableTypeVar>(follow(*expectedType)))
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
                        expectedResultType = prop->second.type;
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

    return {checkExprTable(scope, expr, fieldTypes, expectedType)};
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprUnary& expr)
{
    ExprResult<TypeId> result = checkExpr(scope, *expr.expr);
    TypeId operandType = follow(result.type);

    switch (expr.op)
    {
    case AstExprUnary::Not:
        return {booleanType, {NotPredicate{std::move(result.predicates)}}};
    case AstExprUnary::Minus:
    {
        const bool operandIsAny = get<AnyTypeVar>(operandType) || get<ErrorTypeVar>(operandType);

        if (operandIsAny)
            return {operandType};

        if (typeCouldHaveMetatable(operandType))
        {
            if (auto fnt = findMetatableEntry(operandType, "__unm", expr.location))
            {
                TypeId actualFunctionType = instantiate(scope, *fnt, expr.location);
                TypePackId arguments = addTypePack({operandType});
                TypePackId retType = freshTypePack(scope);
                TypeId expectedFunctionType = addType(FunctionTypeVar(scope->level, arguments, retType));

                Unifier state = mkUnifier(expr.location);
                state.tryUnify(expectedFunctionType, actualFunctionType, /*isFunctionCall*/ true);

                if (!state.errors.empty())
                    return {errorType};

                return {first(retType).value_or(nilType)};
            }

            reportError(expr.location,
                GenericError{format("Unary operator '%s' not supported by type '%s'", toString(expr.op).c_str(), toString(operandType).c_str())});
            return {errorType};
        }

        reportErrors(tryUnify(numberType, operandType, expr.location));
        return {numberType};
    }
    case AstExprUnary::Len:
        tablify(operandType);

        if (FFlag::LuauExtraNilRecovery)
            operandType = stripFromNilAndReport(operandType, expr.location);

        if (get<ErrorTypeVar>(operandType))
            return {errorType};

        if (get<AnyTypeVar>(operandType))
            return {numberType}; // Not strictly correct: metatables permit overriding this

        if (auto p = get<PrimitiveTypeVar>(operandType))
        {
            if (p->type == PrimitiveTypeVar::String)
                return {numberType};
        }

        if (!getTableType(operandType))
            reportError(TypeError{expr.location, NotATable{operandType}});

        return {numberType};

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

TypeId TypeChecker::unionOfTypes(TypeId a, TypeId b, const Location& location, bool unifyFreeTypes)
{
    if (unifyFreeTypes && (get<FreeTypeVar>(a) || get<FreeTypeVar>(b)))
    {
        if (unify(a, b, location))
            return a;

        return errorType;
    }

    if (*a == *b)
        return a;

    std::vector<TypeId> types = reduceUnion({a, b});
    if (types.size() == 1)
        return types[0];

    return addType(UnionTypeVar{types});
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

TypeId TypeChecker::checkRelationalOperation(
    const ScopePtr& scope, const AstExprBinary& expr, TypeId lhsType, TypeId rhsType, const PredicateVec& predicates)
{
    auto stripNil = [this](TypeId ty, bool isOrOp = false) {
        ty = follow(ty);
        if (!isNonstrictMode() && !isOrOp)
            return ty;

        if (auto i = get<UnionTypeVar>(ty))
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
    const bool lhsIsAny = get<AnyTypeVar>(lhsType) || get<ErrorTypeVar>(lhsType);

    // Peephole check for `cond and a or b -> type(a)|type(b)`
    // TODO: Kill this when singleton types arrive. :(
    if (AstExprBinary* subexp = expr.left->as<AstExprBinary>())
    {
        if (expr.op == AstExprBinary::Or && subexp->op == AstExprBinary::And)
        {
            if (FFlag::LuauSlightlyMoreFlexibleBinaryPredicates)
            {
                ScopePtr subScope = childScope(scope, subexp->location);
                reportErrors(resolve(predicates, subScope, true));
                return unionOfTypes(rhsType, stripNil(checkExpr(subScope, *subexp->right).type, true), expr.location);
            }
            else
            {
                return unionOfTypes(rhsType, checkExpr(scope, *subexp->right).type, expr.location);
            }
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

        const bool rhsIsAny = get<AnyTypeVar>(rhsType) || get<ErrorTypeVar>(rhsType);
        if (lhsIsAny || rhsIsAny)
            return booleanType;

        // Fallthrough here is intentional
    }
    case AstExprBinary::CompareLt:
    case AstExprBinary::CompareGt:
    case AstExprBinary::CompareGe:
    case AstExprBinary::CompareLe:
    {
        /* Subtlety here:
         * We need to do this unification first, but there are situations where we don't actually want to
         * report any problems that might have been surfaced as a result of this step because we might already
         * have a better, more descriptive error teed up.
         */
        Unifier state = mkUnifier(expr.location);
        if (!FFlag::LuauEqConstraint || !isEquality)
            state.tryUnify(lhsType, rhsType);

        bool needsMetamethod = !isEquality;

        TypeId leftType = follow(lhsType);
        if (get<PrimitiveTypeVar>(leftType) || get<AnyTypeVar>(leftType) || get<ErrorTypeVar>(leftType) || get<UnionTypeVar>(leftType))
        {
            reportErrors(state.errors);

            const PrimitiveTypeVar* ptv = get<PrimitiveTypeVar>(leftType);
            if (!isEquality && state.errors.empty() && (get<UnionTypeVar>(leftType) || (ptv && ptv->type == PrimitiveTypeVar::Boolean)))
                reportError(expr.location, GenericError{format("Type '%s' cannot be compared with relational operator %s", toString(leftType).c_str(),
                                               toString(expr.op).c_str())});

            return booleanType;
        }

        std::string metamethodName = opToMetaTableEntry(expr.op);

        std::optional<TypeId> leftMetatable =
            isString(lhsType) ? std::nullopt : getMetatable(FFlag::LuauAddMissingFollow ? follow(lhsType) : lhsType);
        std::optional<TypeId> rightMetatable =
            isString(rhsType) ? std::nullopt : getMetatable(FFlag::LuauAddMissingFollow ? follow(rhsType) : rhsType);

        if (bool(leftMetatable) != bool(rightMetatable) && leftMetatable != rightMetatable)
        {
            reportError(expr.location, GenericError{format("Types %s and %s cannot be compared with %s because they do not have the same metatable",
                                           toString(lhsType).c_str(), toString(rhsType).c_str(), toString(expr.op).c_str())});
            return errorType;
        }

        if (leftMetatable)
        {
            std::optional<TypeId> metamethod = findMetatableEntry(lhsType, metamethodName, expr.location);
            if (metamethod)
            {
                if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(*metamethod))
                {
                    if (isEquality)
                    {
                        Unifier state = mkUnifier(expr.location);
                        state.tryUnify(ftv->retType, addTypePack({booleanType}));

                        if (!state.errors.empty())
                        {
                            reportError(expr.location, GenericError{format("Metamethod '%s' must return type 'boolean'", metamethodName.c_str())});
                            return errorType;
                        }
                    }
                }

                reportErrors(state.errors);

                TypeId actualFunctionType = addType(FunctionTypeVar(scope->level, addTypePack({lhsType, rhsType}), addTypePack({booleanType})));
                state.tryUnify(
                    instantiate(scope, *metamethod, expr.location), instantiate(scope, actualFunctionType, expr.location), /*isFunctionCall*/ true);

                reportErrors(state.errors);
                return booleanType;
            }
            else if (needsMetamethod)
            {
                reportError(
                    expr.location, GenericError{format("Table %s does not offer metamethod %s", toString(lhsType).c_str(), metamethodName.c_str())});
                return errorType;
            }
        }

        if (get<FreeTypeVar>(FFlag::LuauAddMissingFollow ? follow(lhsType) : lhsType) && (!FFlag::LuauEqConstraint || !isEquality))
        {
            auto name = getIdentifierOfBaseVar(expr.left);
            reportError(expr.location, CannotInferBinaryOperation{expr.op, name, CannotInferBinaryOperation::Comparison});
            return errorType;
        }

        if (needsMetamethod)
        {
            reportError(expr.location, GenericError{format("Type %s cannot be compared with %s because it has no metatable",
                                           toString(lhsType).c_str(), toString(expr.op).c_str())});
            return errorType;
        }

        if (!FFlag::LuauEqConstraint)
        {
            if (isEquality)
            {
                ErrorVec errVec = tryUnify(rhsType, lhsType, expr.location);
                if (!state.errors.empty() && !errVec.empty())
                    reportError(expr.location, TypeMismatch{lhsType, rhsType});
            }
            else
                reportErrors(state.errors);
        }

        return booleanType;
    }

    case AstExprBinary::And:
        if (lhsIsAny)
            return lhsType;
        return unionOfTypes(rhsType, booleanType, expr.location, false);
    case AstExprBinary::Or:
        if (lhsIsAny)
            return lhsType;
        return unionOfTypes(lhsType, rhsType, expr.location);
    default:
        LUAU_ASSERT(0);
        ice(format("checkRelationalOperation called with incorrect binary expression '%s'", toString(expr.op).c_str()), expr.location);
    }
}

TypeId TypeChecker::checkBinaryOperation(
    const ScopePtr& scope, const AstExprBinary& expr, TypeId lhsType, TypeId rhsType, const PredicateVec& predicates)
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

    if (!isNonstrictMode() && get<FreeTypeVar>(lhsType))
    {
        auto name = getIdentifierOfBaseVar(expr.left);
        reportError(expr.location, CannotInferBinaryOperation{expr.op, name, CannotInferBinaryOperation::Operation});
        return errorType;
    }

    // If we know nothing at all about the lhs type, we can usually say nothing about the result.
    // The notable exception to this is the equality and inequality operators, which always produce a boolean.
    const bool lhsIsAny = get<AnyTypeVar>(lhsType) || get<ErrorTypeVar>(lhsType);
    const bool rhsIsAny = get<AnyTypeVar>(rhsType) || get<ErrorTypeVar>(rhsType);

    if (lhsIsAny)
        return lhsType;
    if (rhsIsAny)
        return rhsType;

    if (get<FreeTypeVar>(lhsType))
    {
        // Inferring this accurately will get a bit weird.
        // If the lhs type is not known, it could be assumed that it is a table or class that has a metatable
        // that defines the required method, but we don't know which.
        // For now, we'll give up and hope for the best.
        return anyType;
    }

    if (get<FreeTypeVar>(rhsType))
        unify(lhsType, rhsType, expr.location);

    if (typeCouldHaveMetatable(lhsType) || typeCouldHaveMetatable(rhsType))
    {
        auto checkMetatableCall = [this, &scope, &expr](TypeId fnt, TypeId lhst, TypeId rhst) -> TypeId {
            TypeId actualFunctionType = instantiate(scope, fnt, expr.location);
            TypePackId arguments = addTypePack({lhst, rhst});
            TypePackId retType = freshTypePack(scope);
            TypeId expectedFunctionType = addType(FunctionTypeVar(scope->level, arguments, retType));

            Unifier state = mkUnifier(expr.location);
            state.tryUnify(expectedFunctionType, actualFunctionType, /*isFunctionCall*/ true);

            reportErrors(state.errors);

            if (!state.errors.empty())
                return errorType;

            return first(retType).value_or(nilType);
        };

        std::string op = opToMetaTableEntry(expr.op);
        if (auto fnt = findMetatableEntry(lhsType, op, expr.location))
            return checkMetatableCall(*fnt, lhsType, rhsType);
        if (auto fnt = findMetatableEntry(rhsType, op, expr.location))
        {
            // Note the intentionally reversed arguments here.
            return checkMetatableCall(*fnt, rhsType, lhsType);
        }

        reportError(expr.location, GenericError{format("Binary operator '%s' not supported by types '%s' and '%s'", toString(expr.op).c_str(),
                                       toString(lhsType).c_str(), toString(rhsType).c_str())});
        return errorType;
    }

    switch (expr.op)
    {
    case AstExprBinary::Concat:
        reportErrors(tryUnify(addType(UnionTypeVar{{stringType, numberType}}), lhsType, expr.left->location));
        reportErrors(tryUnify(addType(UnionTypeVar{{stringType, numberType}}), rhsType, expr.right->location));
        return stringType;
    case AstExprBinary::Add:
    case AstExprBinary::Sub:
    case AstExprBinary::Mul:
    case AstExprBinary::Div:
    case AstExprBinary::Mod:
    case AstExprBinary::Pow:
        reportErrors(tryUnify(numberType, lhsType, expr.left->location));
        reportErrors(tryUnify(numberType, rhsType, expr.right->location));
        return numberType;
    default:
        // These should have been handled with checkRelationalOperation
        LUAU_ASSERT(0);
        return anyType;
    }
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprBinary& expr)
{
    if (expr.op == AstExprBinary::And)
    {
        ExprResult<TypeId> lhs = checkExpr(scope, *expr.left);

        // We can't just report errors here.
        // This function can be called from AstStatLocal or from AstStatIf, or even from AstExprBinary (and others).
        // For now, ignore the errors returned by the predicate resolver.
        // We may need an extra property for each predicate set that indicates it has been resolved.
        // Requires a slight modification to the data structure.
        ScopePtr innerScope = childScope(scope, expr.location);
        resolve(lhs.predicates, innerScope, true);

        ExprResult<TypeId> rhs = checkExpr(innerScope, *expr.right);
        if (!FFlag::LuauSlightlyMoreFlexibleBinaryPredicates)
            resolve(rhs.predicates, innerScope, true);

        return {checkBinaryOperation(innerScope, expr, lhs.type, rhs.type), {AndPredicate{std::move(lhs.predicates), std::move(rhs.predicates)}}};
    }
    else if (FFlag::LuauOrPredicate && expr.op == AstExprBinary::Or)
    {
        ExprResult<TypeId> lhs = checkExpr(scope, *expr.left);

        ScopePtr innerScope = childScope(scope, expr.location);
        resolve(lhs.predicates, innerScope, false);

        ExprResult<TypeId> rhs = checkExpr(innerScope, *expr.right);

        // Because of C++, I'm not sure if lhs.predicates was not moved out by the time we call checkBinaryOperation.
        TypeId result = checkBinaryOperation(innerScope, expr, lhs.type, rhs.type, lhs.predicates);
        return {result, {OrPredicate{std::move(lhs.predicates), std::move(rhs.predicates)}}};
    }
    else if (FFlag::LuauEqConstraint && (expr.op == AstExprBinary::CompareEq || expr.op == AstExprBinary::CompareNe))
    {
        if (auto predicate = tryGetTypeGuardPredicate(expr))
            return {booleanType, {std::move(*predicate)}};

        ExprResult<TypeId> lhs = checkExpr(scope, *expr.left);
        ExprResult<TypeId> rhs = checkExpr(scope, *expr.right);

        PredicateVec predicates;

        if (auto lvalue = tryGetLValue(*expr.left))
            predicates.push_back(EqPredicate{std::move(*lvalue), rhs.type, expr.location});

        if (auto lvalue = tryGetLValue(*expr.right))
            predicates.push_back(EqPredicate{std::move(*lvalue), lhs.type, expr.location});

        if (!predicates.empty() && expr.op == AstExprBinary::CompareNe)
            predicates = {NotPredicate{std::move(predicates)}};

        return {checkBinaryOperation(scope, expr, lhs.type, rhs.type), std::move(predicates)};
    }
    else
    {
        // Once we have EqPredicate, we should break this else branch into its' own branch.
        // For now, fall through is intentional.
        if (expr.op == AstExprBinary::CompareEq || expr.op == AstExprBinary::CompareNe)
        {
            if (auto predicate = tryGetTypeGuardPredicate(expr))
                return {booleanType, {std::move(*predicate)}};
        }

        ExprResult<TypeId> lhs = checkExpr(scope, *expr.left);
        ExprResult<TypeId> rhs = checkExpr(scope, *expr.right);

        // Intentionally discarding predicates with other operators.
        return {checkBinaryOperation(scope, expr, lhs.type, rhs.type, lhs.predicates)};
    }
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprTypeAssertion& expr)
{
    ExprResult<TypeId> result;
    TypeId annotationType;

    if (FFlag::LuauInferReturnAssertAssign)
    {
        annotationType = (FFlag::LuauRankNTypes ? resolveType(scope, *expr.annotation) : resolveType(scope, *expr.annotation, true));
        result = checkExpr(scope, *expr.expr, annotationType);
    }
    else
    {
        result = checkExpr(scope, *expr.expr);
        annotationType = (FFlag::LuauRankNTypes ? resolveType(scope, *expr.annotation) : resolveType(scope, *expr.annotation, true));
    }

    ErrorVec errorVec = canUnify(result.type, annotationType, expr.location);
    if (!errorVec.empty())
    {
        reportErrors(errorVec);
        return {errorType, std::move(result.predicates)};
    }

    return {annotationType, std::move(result.predicates)};
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprError& expr)
{
    const size_t oldSize = currentModule->errors.size();

    for (AstExpr* expr : expr.expressions)
        checkExpr(scope, *expr);

    // HACK: We want to check the contents of the AstExprError, but
    // any type errors that may arise from it are going to be useless.
    currentModule->errors.resize(oldSize);

    return {errorType};
}

ExprResult<TypeId> TypeChecker::checkExpr(const ScopePtr& scope, const AstExprIfElse& expr)
{
    ExprResult<TypeId> result = checkExpr(scope, *expr.condition);
    ScopePtr trueScope = childScope(scope, expr.trueExpr->location);
    reportErrors(resolve(result.predicates, trueScope, true));
    ExprResult<TypeId> trueType = checkExpr(trueScope, *expr.trueExpr);

    ScopePtr falseScope = childScope(scope, expr.falseExpr->location);
    // Don't report errors for this scope to avoid potentially duplicating errors reported for the first scope.
    resolve(result.predicates, falseScope, false);
    ExprResult<TypeId> falseType = checkExpr(falseScope, *expr.falseExpr);

    unify(trueType.type, falseType.type, expr.location);

    // TODO: normalize(UnionTypeVar{{trueType, falseType}})
    // For now both trueType and falseType must be the same type.
    return {trueType.type};
}

TypeId TypeChecker::checkLValue(const ScopePtr& scope, const AstExpr& expr)
{
    auto [ty, binding] = checkLValueBinding(scope, expr);
    return ty;
}

std::pair<TypeId, TypeId*> TypeChecker::checkLValueBinding(const ScopePtr& scope, const AstExpr& expr)
{
    if (auto a = expr.as<AstExprLocal>())
        return checkLValueBinding(scope, *a);
    else if (auto a = expr.as<AstExprGlobal>())
        return checkLValueBinding(scope, *a);
    else if (auto a = expr.as<AstExprIndexName>())
        return checkLValueBinding(scope, *a);
    else if (auto a = expr.as<AstExprIndexExpr>())
        return checkLValueBinding(scope, *a);
    else if (auto a = expr.as<AstExprError>())
    {
        for (AstExpr* expr : a->expressions)
            checkExpr(scope, *expr);
        return std::pair(errorType, nullptr);
    }
    else
        ice("Unexpected AST node in checkLValue", expr.location);
}

std::pair<TypeId, TypeId*> TypeChecker::checkLValueBinding(const ScopePtr& scope, const AstExprLocal& expr)
{
    if (std::optional<TypeId> ty = scope->lookup(expr.local))
        return {*ty, nullptr};

    reportError(expr.location, UnknownSymbol{expr.local->name.value, UnknownSymbol::Binding});
    return {errorType, nullptr};
}

std::pair<TypeId, TypeId*> TypeChecker::checkLValueBinding(const ScopePtr& scope, const AstExprGlobal& expr)
{
    Name name = expr.name.value;
    ScopePtr moduleScope = currentModule->getModuleScope();

    const auto it = moduleScope->bindings.find(expr.name);

    if (it != moduleScope->bindings.end())
        return std::pair(it->second.typeId, &it->second.typeId);

    if (isNonstrictMode() || FFlag::LuauSecondTypecheckKnowsTheDataModel)
    {
        TypeId result = (FFlag::LuauGenericFunctions && FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(moduleScope, true));

        Binding& binding = moduleScope->bindings[expr.name];
        binding = {result, expr.location};

        // If we're in strict mode, we want to report defining a global as an error,
        // but still add it to the bindings, so that autocomplete includes it in completions.
        if (!isNonstrictMode())
            reportError(TypeError{expr.location, UnknownSymbol{name, UnknownSymbol::Binding}});

        return std::pair(result, &binding.typeId);
    }

    reportError(TypeError{expr.location, UnknownSymbol{name, UnknownSymbol::Binding}});
    return std::pair(errorType, nullptr);
}

std::pair<TypeId, TypeId*> TypeChecker::checkLValueBinding(const ScopePtr& scope, const AstExprIndexName& expr)
{
    TypeId lhs = checkExpr(scope, *expr.expr).type;

    if (get<ErrorTypeVar>(lhs) || get<AnyTypeVar>(lhs))
        return std::pair(lhs, nullptr);

    tablify(lhs);

    Name name = expr.index.value;

    if (FFlag::LuauExtraNilRecovery)
        lhs = stripFromNilAndReport(lhs, expr.expr->location);

    if (TableTypeVar* lhsTable = getMutableTableType(lhs))
    {
        const auto& it = lhsTable->props.find(name);
        if (it != lhsTable->props.end())
        {
            return std::pair(it->second.type, &it->second.type);
        }
        else if (lhsTable->state == TableState::Unsealed || lhsTable->state == TableState::Free)
        {
            TypeId theType = (FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(scope, true));
            Property& property = lhsTable->props[name];
            property.type = theType;
            property.location = expr.indexLocation;
            return std::pair(theType, &property.type);
        }
        else if (auto indexer = lhsTable->indexer)
        {
            Unifier state = mkUnifier(expr.location);
            state.tryUnify(indexer->indexType, stringType);
            if (!state.errors.empty())
            {
                state.log.rollback();
                reportError(expr.location, UnknownProperty{lhs, name});
                return std::pair(errorType, nullptr);
            }

            return std::pair(indexer->indexResultType, nullptr);
        }
        else if (lhsTable->state == TableState::Sealed)
        {
            reportError(TypeError{expr.location, CannotExtendTable{lhs, CannotExtendTable::Property, name}});
            return std::pair(errorType, nullptr);
        }
        else
        {
            reportError(TypeError{expr.location, GenericError{"Internal error: generic tables are not lvalues"}});
            return std::pair(errorType, nullptr);
        }
    }
    else if (const ClassTypeVar* lhsClass = get<ClassTypeVar>(lhs))
    {
        const Property* prop = lookupClassProp(lhsClass, name);
        if (!prop)
        {
            reportError(TypeError{expr.location, UnknownProperty{lhs, name}});
            return std::pair(errorType, nullptr);
        }

        return std::pair(prop->type, nullptr);
    }
    else if (get<IntersectionTypeVar>(lhs))
    {
        if (std::optional<TypeId> ty = getIndexTypeFromType(scope, lhs, name, expr.location, false))
            return std::pair(*ty, nullptr);

        // If intersection has a table part, report that it cannot be extended just as a sealed table
        if (isTableIntersection(lhs))
        {
            reportError(TypeError{expr.location, CannotExtendTable{lhs, CannotExtendTable::Property, name}});
            return std::pair(errorType, nullptr);
        }
    }

    reportError(TypeError{expr.location, NotATable{lhs}});
    return std::pair(errorType, nullptr);
}

std::pair<TypeId, TypeId*> TypeChecker::checkLValueBinding(const ScopePtr& scope, const AstExprIndexExpr& expr)
{
    TypeId exprType = checkExpr(scope, *expr.expr).type;
    tablify(exprType);

    if (FFlag::LuauExtraNilRecovery)
        exprType = stripFromNilAndReport(exprType, expr.expr->location);

    TypeId indexType = checkExpr(scope, *expr.index).type;

    if (get<AnyTypeVar>(exprType) || get<ErrorTypeVar>(exprType))
        return std::pair(exprType, nullptr);

    AstExprConstantString* value = expr.index->as<AstExprConstantString>();

    if (value && FFlag::LuauClassPropertyAccessAsString)
    {
        if (const ClassTypeVar* exprClass = get<ClassTypeVar>(exprType))
        {
            const Property* prop = lookupClassProp(exprClass, value->value.data);
            if (!prop)
            {
                reportError(TypeError{expr.location, UnknownProperty{exprType, value->value.data}});
                return std::pair(errorType, nullptr);
            }
            return std::pair(prop->type, nullptr);
        }
    }

    TableTypeVar* exprTable = getMutableTableType(exprType);

    if (!exprTable)
    {
        if (FFlag::LuauExtraNilRecovery)
            reportError(TypeError{expr.expr->location, NotATable{exprType}});
        else
            reportError(TypeError{expr.location, NotATable{exprType}});
        return std::pair(errorType, nullptr);
    }

    if (value)
    {
        const auto& it = exprTable->props.find(value->value.data);
        if (it != exprTable->props.end())
        {
            return std::pair(it->second.type, &it->second.type);
        }
        else if (exprTable->state == TableState::Unsealed || exprTable->state == TableState::Free)
        {
            TypeId resultType = (FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(scope, true));
            Property& property = exprTable->props[value->value.data];
            property.type = resultType;
            property.location = expr.index->location;
            return std::pair(resultType, &property.type);
        }
    }

    if (exprTable->indexer)
    {
        const TableIndexer& indexer = *exprTable->indexer;
        unify(indexer.indexType, indexType, expr.index->location);
        return std::pair(indexer.indexResultType, nullptr);
    }
    else if (exprTable->state == TableState::Unsealed || exprTable->state == TableState::Free)
    {
        TypeId resultType = (FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(scope, true));
        exprTable->indexer = TableIndexer{anyIfNonstrict(indexType), anyIfNonstrict(resultType)};
        return std::pair(resultType, nullptr);
    }
    else if (FFlag::LuauIndexTablesWithIndexers)
    {
        // We allow t[x] where x:string for tables without an indexer
        unify(indexType, stringType, expr.location);
        return std::pair(anyType, nullptr);
    }
    else
    {
        TypeId resultType = freshType(scope);
        return std::pair(resultType, nullptr);
    }
}

// Answers the question: "Can I define another function with this name?"
// Primarily about detecting duplicates.
TypeId TypeChecker::checkFunctionName(const ScopePtr& scope, AstExpr& funName)
{
    if (auto globalName = funName.as<AstExprGlobal>())
    {
        const ScopePtr& globalScope = currentModule->getModuleScope();
        Symbol name = globalName->name;
        if (globalScope->bindings.count(name))
        {
            if (isNonstrictMode())
                return globalScope->bindings[name].typeId;

            return errorType;
        }
        else
        {
            TypeId ty = (FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(scope, true));
            globalScope->bindings[name] = {ty, funName.location};
            return ty;
        }
    }
    else if (auto localName = funName.as<AstExprLocal>())
    {
        Symbol name = localName->local;
        Binding& binding = scope->bindings[name];
        if (binding.typeId == nullptr)
            binding = {(FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(scope, true)), funName.location};

        return binding.typeId;
    }
    else if (auto indexName = funName.as<AstExprIndexName>())
    {
        TypeId lhsType = checkExpr(scope, *indexName->expr).type;
        if (get<ErrorTypeVar>(lhsType) || get<AnyTypeVar>(lhsType))
            return lhsType;

        TableTypeVar* ttv = getMutableTableType(lhsType);
        if (!ttv)
        {
            if (!isTableIntersection(lhsType))
                reportError(TypeError{funName.location, OnlyTablesCanHaveMethods{lhsType}});

            return errorType;
        }

        // Cannot extend sealed table, but we dont report an error here because it will be reported during AstStatFunction check
        if (lhsType->persistent || ttv->state == TableState::Sealed)
            return errorType;

        Name name = indexName->index.value;

        if (ttv->props.count(name))
            return errorType;

        Property& property = ttv->props[name];

        property.type = (FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(scope, true));
        property.location = indexName->indexLocation;
        ttv->methodDefinitionLocations[name] = funName.location;
        return property.type;
    }
    else if (funName.is<AstExprError>())
    {
        return errorType;
    }
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
    const ScopePtr& scope, int subLevel, const AstExprFunction& expr, std::optional<Location> originalName, std::optional<TypeId> expectedType)
{
    ScopePtr funScope = childFunctionScope(scope, expr.location, subLevel);

    const FunctionTypeVar* expectedFunctionType = nullptr;

    if (expectedType)
    {
        LUAU_ASSERT(!expr.self);

        if (auto ftv = get<FunctionTypeVar>(follow(*expectedType)))
        {
            expectedFunctionType = ftv;
        }
        else if (auto utv = get<UnionTypeVar>(follow(*expectedType)))
        {
            // Look for function type in a union. Other types can be ignored since current expression is a function
            for (auto option : utv)
            {
                if (auto ftv = get<FunctionTypeVar>(follow(option)))
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

        // We do not infer type binders, so if a generic function is required we do not propagate
        if (expectedFunctionType && !(expectedFunctionType->generics.empty() && expectedFunctionType->genericPacks.empty()))
            expectedFunctionType = nullptr;
    }

    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;

    if (FFlag::LuauGenericFunctions)
    {
        std::tie(generics, genericPacks) = createGenericTypes(funScope, expr, expr.generics, expr.genericPacks);
    }

    TypePackId retPack;
    if (expr.hasReturnAnnotation)
    {
        if (FFlag::LuauGenericFunctions)
            retPack = resolveTypePack(funScope, expr.returnAnnotation);
        else
            retPack = resolveTypePack(scope, expr.returnAnnotation);
    }
    else if (isNonstrictMode())
        retPack = anyTypePack;
    else if (expectedFunctionType)
    {
        auto [head, tail] = flatten(expectedFunctionType->retType);

        // Do not infer 'nil' as function return type
        if (!tail && head.size() == 1 && isNil(head[0]))
            retPack = FFlag::LuauGenericFunctions ? freshTypePack(funScope) : freshTypePack(scope);
        else
            retPack = addTypePack(head, tail);
    }
    else if (FFlag::LuauGenericFunctions)
        retPack = freshTypePack(funScope);
    else
        retPack = freshTypePack(scope);

    if (expr.vararg)
    {
        if (expr.varargAnnotation)
        {
            if (FFlag::LuauGenericFunctions)
                funScope->varargPack = resolveTypePack(funScope, *expr.varargAnnotation);
            else
                funScope->varargPack = resolveTypePack(scope, *expr.varargAnnotation);
        }
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
        // TODO: generic self types: CLI-39906
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
            argType = resolveType((FFlag::LuauGenericFunctions ? funScope : scope), *local->annotation);

            // If the annotation type has an error, treat it as if there was no annotation
            if (get<ErrorTypeVar>(follow(argType)))
                argType = anyIfNonstrict(freshType(funScope));
        }
        else
        {
            if (expectedFunctionType && !isNonstrictMode())
            {
                if (expectedArgsCurr != expectedArgsEnd)
                {
                    argType = *expectedArgsCurr;

                    if (!FFlag::LuauInferFunctionArgsFix)
                        ++expectedArgsCurr;
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

        if (FFlag::LuauInferFunctionArgsFix && expectedArgsCurr != expectedArgsEnd)
            ++expectedArgsCurr;
    }

    TypePackId argPack = addTypePack(TypePackVar(TypePack{argTypes, funScope->varargPack}));

    FunctionDefinition defn;
    defn.definitionModuleName = currentModuleName;
    defn.definitionLocation = expr.location;
    defn.varargLocation = expr.vararg ? std::make_optional(expr.varargLocation) : std::nullopt;
    defn.originalNameLocation = originalName.value_or(Location(expr.location.begin, 0));

    TypeId funTy = addType(FunctionTypeVar(funScope->level, generics, genericPacks, argPack, retPack, std::move(defn), bool(expr.self)));

    FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(funTy);

    ftv->argNames.reserve(expr.args.size + (expr.self ? 1 : 0));

    if (expr.self)
        ftv->argNames.push_back(FunctionArgument{"self", {}});

    for (AstLocal* local : expr.args)
        ftv->argNames.push_back(FunctionArgument{local->name.value, local->location});

    return std::make_pair(funTy, funScope);
}

static bool allowsNoReturnValues(const TypePackId tp)
{
    for (TypeId ty : tp)
    {
        if (!get<ErrorTypeVar>(FFlag::LuauAddMissingFollow ? follow(ty) : ty))
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
    if (FunctionTypeVar* funTy = getMutable<FunctionTypeVar>(ty))
    {
        check(scope, *function.body);

        // We explicitly don't follow here to check if we have a 'true' free type instead of bound one
        if (FFlag::LuauAddMissingFollow ? get_if<FreeTypePack>(&funTy->retType->ty) : get<FreeTypePack>(funTy->retType))
            *asMutable(funTy->retType) = TypePack{{}, std::nullopt};

        bool reachesImplicitReturn = getFallthrough(function.body) != nullptr;

        if (reachesImplicitReturn && !allowsNoReturnValues(follow(funTy->retType)))
        {
            // If we're in nonstrict mode we want to only report this missing return
            // statement if there are type annotations on the function. In strict mode
            // we report it regardless.
            if (!isNonstrictMode() || function.hasReturnAnnotation)
            {
                reportError(getEndLocation(function), FunctionExitsWithoutReturning{funTy->retType});
            }
        }
    }
    else
        ice("Checking non functional type");
}

ExprResult<TypePackId> TypeChecker::checkExprPack(const ScopePtr& scope, const AstExpr& expr)
{
    if (auto a = expr.as<AstExprCall>())
        return checkExprPack(scope, *a);
    else if (expr.is<AstExprVarargs>())
    {
        if (!scope->varargPack)
            return {addTypePack({addType(ErrorTypeVar())})};

        return {*scope->varargPack};
    }
    else
    {
        TypeId type = checkExpr(scope, expr).type;
        return {addTypePack({type})};
    }
}

// Returns the minimum number of arguments the argument list can accept.
static size_t getMinParameterCount(TypePackId tp)
{
    size_t minCount = 0;
    size_t optionalCount = 0;

    auto it = begin(tp);
    auto endIter = end(tp);

    while (it != endIter)
    {
        TypeId ty = *it;
        if (isOptional(ty))
            ++optionalCount;
        else
        {
            minCount += optionalCount;
            optionalCount = 0;
            minCount++;
        }

        ++it;
    }

    return minCount;
}

void TypeChecker::checkArgumentList(
    const ScopePtr& scope, Unifier& state, TypePackId argPack, TypePackId paramPack, const std::vector<Location>& argLocations)
{
    /* Important terminology refresher:
     * A function requires paramaters.
     * To call a function, you supply arguments.
     */
    TypePackIterator argIter = begin(argPack);
    TypePackIterator paramIter = begin(paramPack);
    TypePackIterator endIter = end(argPack); // Important subtlety: All end TypePackIterators are equivalent

    size_t paramIndex = 0;

    size_t minParams = getMinParameterCount(paramPack);

    while (true)
    {
        state.location = paramIndex < argLocations.size() ? argLocations[paramIndex] : state.location;

        if (argIter == endIter && paramIter == endIter)
        {
            std::optional<TypePackId> argTail = argIter.tail();
            std::optional<TypePackId> paramTail = paramIter.tail();

            // If we hit the end of both type packs simultaneously, then there are definitely no further type
            // errors to report.  All we need to do is tie up any free tails.
            //
            // If one side has a free tail and the other has none at all, we create an empty pack and bind the
            // free tail to that.

            if (argTail)
            {
                if (get<Unifiable::Free>(*argTail))
                {
                    if (paramTail)
                        state.tryUnify(*argTail, *paramTail);
                    else
                    {
                        state.log(*argTail);
                        *asMutable(*argTail) = TypePack{{}};
                    }
                }
            }
            else if (paramTail)
            {
                // argTail is definitely empty
                if (get<Unifiable::Free>(*paramTail))
                {
                    state.log(*paramTail);
                    *asMutable(*paramTail) = TypePack{{}};
                }
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
                if (get<Unifiable::Error>(tail))
                {
                    // Unify remaining parameters so we don't leave any free-types hanging around.
                    TypeId argTy = errorType;
                    while (paramIter != endIter)
                    {
                        state.tryUnify(*paramIter, argTy);
                        ++paramIter;
                    }
                    return;
                }
                else if (auto vtp = get<VariadicTypePack>(tail))
                {
                    while (paramIter != endIter)
                    {
                        state.tryUnify(*paramIter, vtp->ty);
                        ++paramIter;
                    }

                    return;
                }
                else if (get<FreeTypePack>(tail))
                {
                    std::vector<TypeId> rest;
                    rest.reserve(std::distance(paramIter, endIter));
                    while (paramIter != endIter)
                    {
                        rest.push_back(*paramIter);
                        ++paramIter;
                    }

                    TypePackId varPack = addTypePack(TypePackVar{TypePack{rest, paramIter.tail()}});
                    state.tryUnify(tail, varPack);
                    return;
                }
            }

            // If any remaining unfulfilled parameters are nonoptional, this is a problem.
            while (paramIter != endIter)
            {
                TypeId t = follow(*paramIter);
                if (isOptional(t))
                {
                } // ok
                else if (get<ErrorTypeVar>(t))
                {
                } // ok
                else if (isNonstrictMode() && get<AnyTypeVar>(t))
                {
                } // ok
                else
                {
                    state.errors.push_back(TypeError{state.location, CountMismatch{minParams, paramIndex}});
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
                while (argIter != endIter)
                {
                    unify(*argIter, errorType, state.location);
                    ++argIter;
                }
                // For this case, we want the error span to cover every errant extra parameter
                Location location = state.location;
                if (!argLocations.empty())
                    location = {state.location.begin, argLocations.back().end};
                state.errors.push_back(TypeError{location, CountMismatch{minParams, std::distance(begin(argPack), end(argPack))}});
                return;
            }
            TypePackId tail = *paramIter.tail();

            if (get<Unifiable::Error>(tail))
            {
                // Function is variadic.  Ok.
                return;
            }
            else if (auto vtp = get<VariadicTypePack>(tail))
            {
                // Function is variadic and requires that all subsequent parameters
                // be compatible with a type.
                size_t argIndex = paramIndex;
                while (argIter != endIter)
                {
                    Location location = state.location;

                    if (argIndex < argLocations.size())
                        location = argLocations[argIndex];

                    unify(vtp->ty, *argIter, location);
                    ++argIter;
                    ++argIndex;
                }

                return;
            }
            else if (FFlag::LuauGenericVariadicsUnification && get<FreeTypePack>(tail))
            {
                // Create a type pack out of the remaining argument types
                // and unify it with the tail.
                std::vector<TypeId> rest;
                rest.reserve(std::distance(argIter, endIter));
                while (argIter != endIter)
                {
                    rest.push_back(*argIter);
                    ++argIter;
                }

                TypePackId varPack = addTypePack(TypePackVar{TypePack{rest, argIter.tail()}});
                state.tryUnify(tail, varPack);
                return;
            }
            else if (get<FreeTypePack>(tail))
            {
                state.log(tail);
                *asMutable(tail) = TypePack{};

                return;
            }
            else if (FFlag::LuauRankNTypes && get<GenericTypePack>(tail))
            {
                // For this case, we want the error span to cover every errant extra parameter
                Location location = state.location;
                if (!argLocations.empty())
                    location = {state.location.begin, argLocations.back().end};
                // TODO: Better error message?
                state.errors.push_back(TypeError{location, CountMismatch{minParams, std::distance(begin(argPack), end(argPack))}});
                return;
            }
        }
        else
        {
            if (FFlag::LuauRankNTypes)
                unifyWithInstantiationIfNeeded(scope, *paramIter, *argIter, state);
            else
                state.tryUnify(*paramIter, *argIter, /*isFunctionCall*/ false);
            ++argIter;
            ++paramIter;
        }

        ++paramIndex;
    }
}

ExprResult<TypePackId> TypeChecker::checkExprPack(const ScopePtr& scope, const AstExprCall& expr)
{
    // evaluate type of function
    // decompose an intersection into its component overloads
    // Compute types of parameters
    // For each overload
    //     Compare parameter and argument types
    //     Report any errors (also speculate dot vs colon warnings!)
    //     If there are no errors, return the resulting return type

    TypeId selfType = nullptr;
    TypeId functionType = nullptr;
    TypeId actualFunctionType = nullptr;

    if (expr.self)
    {
        AstExprIndexName* indexExpr = expr.func->as<AstExprIndexName>();
        if (!indexExpr)
            ice("method call expression has no 'self'");

        selfType = checkExpr(scope, *indexExpr->expr).type;
        if (!FFlag::LuauRankNTypes)
            instantiate(scope, selfType, expr.func->location);

        if (FFlag::LuauExtraNilRecovery)
            selfType = stripFromNilAndReport(selfType, expr.func->location);

        if (std::optional<TypeId> propTy = getIndexTypeFromType(scope, selfType, indexExpr->index.value, expr.location, true))
        {
            functionType = *propTy;
            actualFunctionType = instantiate(scope, functionType, expr.func->location);
        }
        else
        {
            if (!FFlag::LuauMissingUnionPropertyError)
                reportError(indexExpr->indexLocation, UnknownProperty{selfType, indexExpr->index.value});

            if (!FFlag::LuauExtraNilRecovery)
            {
                // Try to recover using a union without 'nil' options
                if (std::optional<TypeId> strippedUnion = tryStripUnionFromNil(selfType))
                {
                    if (std::optional<TypeId> propTy = getIndexTypeFromType(scope, *strippedUnion, indexExpr->index.value, expr.location, false))
                    {
                        selfType = *strippedUnion;

                        functionType = *propTy;
                        actualFunctionType = instantiate(scope, functionType, expr.func->location);
                    }
                }

                if (!actualFunctionType)
                {
                    functionType = errorType;
                    actualFunctionType = errorType;
                }
            }
            else
            {
                functionType = errorType;
                actualFunctionType = errorType;
            }
        }
    }
    else
    {
        functionType = checkExpr(scope, *expr.func).type;
        actualFunctionType = instantiate(scope, functionType, expr.func->location);
    }

    actualFunctionType = follow(actualFunctionType);

    // checkExpr will log the pre-instantiated type of the function.
    // That's not nearly as interesting as the instantiated type, which will include details about how
    // generic functions are being instantiated for this particular callsite.
    currentModule->astOriginalCallTypes[expr.func] = follow(functionType);
    currentModule->astTypes[expr.func] = actualFunctionType;

    std::vector<TypeId> overloads = flattenIntersection(actualFunctionType);

    TypePackId retPack = freshTypePack(scope->level);

    std::vector<std::optional<TypeId>> expectedTypes = getExpectedTypesForCall(overloads, expr.args.size, expr.self);

    ExprResult<TypePackId> argListResult = checkExprList(scope, expr.location, expr.args, false, {}, expectedTypes);
    TypePackId argList = argListResult.type;
    TypePackId argPack = (FFlag::LuauRankNTypes ? argList : DEPRECATED_instantiate(scope, argList, expr.location));

    if (get<Unifiable::Error>(argPack))
        return ExprResult<TypePackId>{errorTypePack};

    TypePack* args = getMutable<TypePack>(argPack);
    LUAU_ASSERT(args != nullptr);

    if (expr.self)
        args->head.insert(args->head.begin(), selfType);

    std::vector<Location> argLocations;
    argLocations.reserve(expr.args.size + 1);
    if (expr.self)
        argLocations.push_back(expr.func->as<AstExprIndexName>()->expr->location);
    for (AstExpr* arg : expr.args)
        argLocations.push_back(arg->location);

    std::vector<OverloadErrorEntry> errors; // errors encountered for each overload

    std::vector<TypeId> overloadsThatMatchArgCount;

    for (TypeId fn : overloads)
    {
        fn = follow(fn);

        if (auto ret = checkCallOverload(scope, expr, fn, retPack, argPack, args, argLocations, argListResult, overloadsThatMatchArgCount, errors))
            return *ret;
    }

    if (handleSelfCallMismatch(scope, expr, args, argLocations, errors))
        return {retPack};

    return reportOverloadResolutionError(scope, expr, retPack, argPack, argLocations, overloads, overloadsThatMatchArgCount, errors);
}

std::vector<std::optional<TypeId>> TypeChecker::getExpectedTypesForCall(const std::vector<TypeId>& overloads, size_t argumentCount, bool selfCall)
{
    std::vector<std::optional<TypeId>> expectedTypes;

    auto assignOption = [this, &expectedTypes](size_t index, std::optional<TypeId> ty) {
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
                std::vector<TypeId> result = reduceUnion({*el, *ty});
                el = result.size() == 1 ? result[0] : addType(UnionTypeVar{std::move(result)});
            }
        }
    };

    for (const TypeId overload : overloads)
    {
        if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(overload))
        {
            auto [argsHead, argsTail] = flatten(ftv->argTypes);

            size_t start = selfCall ? 1 : 0;
            size_t index = 0;

            for (size_t i = start; i < argsHead.size(); ++i)
                assignOption(index++, argsHead[i]);

            if (argsTail)
            {
                if (const VariadicTypePack* vtp = get<VariadicTypePack>(follow(*argsTail)))
                {
                    while (index < argumentCount)
                        assignOption(index++, vtp->ty);
                }
            }
        }
    }

    return expectedTypes;
}

std::optional<ExprResult<TypePackId>> TypeChecker::checkCallOverload(const ScopePtr& scope, const AstExprCall& expr, TypeId fn, TypePackId retPack,
    TypePackId argPack, TypePack* args, const std::vector<Location>& argLocations, const ExprResult<TypePackId>& argListResult,
    std::vector<TypeId>& overloadsThatMatchArgCount, std::vector<OverloadErrorEntry>& errors)
{
    if (FFlag::LuauExtraNilRecovery)
        fn = stripFromNilAndReport(fn, expr.func->location);

    if (get<AnyTypeVar>(fn))
    {
        unify(argPack, anyTypePack, expr.location);
        return {{anyTypePack}};
    }

    if (get<ErrorTypeVar>(fn))
    {
        return {{addTypePack(TypePackVar{Unifiable::Error{}})}};
    }

    if (get<FreeTypeVar>(fn))
    {
        // fn is one of the overloads of actualFunctionType, which
        // has been instantiated, so is a monotype. We can therefore
        // unify it with a monomorphic function.
        TypeId r = addType(FunctionTypeVar(scope->level, argPack, retPack));
        unify(r, fn, expr.location);
        return {{retPack}};
    }

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(fn);
    if (!ftv)
    {
        // Might be a callable table
        if (const MetatableTypeVar* mttv = get<MetatableTypeVar>(fn))
        {
            if (std::optional<TypeId> ty = getIndexTypeFromType(scope, mttv->metatable, "__call", expr.func->location, false))
            {
                // Construct arguments with 'self' added in front
                TypePackId metaCallArgPack = addTypePack(TypePackVar(TypePack{args->head, args->tail}));

                TypePack* metaCallArgs = getMutable<TypePack>(metaCallArgPack);
                metaCallArgs->head.insert(metaCallArgs->head.begin(), fn);

                std::vector<Location> metaArgLocations = argLocations;
                metaArgLocations.insert(metaArgLocations.begin(), expr.func->location);

                TypeId fn = *ty;
                if (FFlag::LuauRankNTypes)
                    fn = instantiate(scope, fn, expr.func->location);

                return checkCallOverload(
                    scope, expr, fn, retPack, metaCallArgPack, metaCallArgs, metaArgLocations, argListResult, overloadsThatMatchArgCount, errors);
            }
        }

        reportError(TypeError{expr.func->location, CannotCallNonFunction{fn}});
        unify(retPack, errorTypePack, expr.func->location);
        return {{errorTypePack}};
    }

    // When this function type has magic functions and did return something, we select that overload instead.
    // TODO: pass in a Unifier object to the magic functions? This will allow the magic functions to cooperate with overload resolution.
    if (ftv->magicFunction)
    {
        // TODO: We're passing in the wrong TypePackId. Should be argPack, but a unit test fails otherwise. CLI-40458
        if (std::optional<ExprResult<TypePackId>> ret = ftv->magicFunction(*this, scope, expr, argListResult))
            return *ret;
    }

    Unifier state = mkUnifier(expr.location);

    // Unify return types
    checkArgumentList(scope, state, retPack, ftv->retType, /*argLocations*/ {});
    if (!state.errors.empty())
    {
        state.log.rollback();
        return {};
    }

    checkArgumentList(scope, state, argPack, ftv->argTypes, argLocations);

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

        errors.emplace_back(std::move(state.errors), args->head, ftv);
        state.log.rollback();
    }
    else
    {
        if (isNonstrictMode() && !expr.self && expr.func->is<AstExprIndexName>() && ftv->hasSelf)
        {
            // If we are running in nonstrict mode, passing fewer arguments than the function is declared to take AND
            // the function is declared with colon notation AND we use dot notation, warn.
            auto [providedArgs, providedTail] = flatten(argPack);

            // If we have a variadic tail, we can't say how many arguments were actually provided
            if (!providedTail)
            {
                std::vector<TypeId> actualArgs = flatten(ftv->argTypes).first;

                size_t providedCount = providedArgs.size();
                size_t requiredCount = actualArgs.size();

                // Ignore optional arguments
                while (providedCount < requiredCount && requiredCount != 0 && isOptional(actualArgs[requiredCount - 1]))
                    requiredCount--;

                if (providedCount < requiredCount)
                {
                    int requiredExtraNils = int(requiredCount - providedCount);
                    reportError(TypeError{expr.func->location, FunctionRequiresSelf{requiredExtraNils}});
                }
            }
        }

        if (FFlag::LuauStoreMatchingOverloadFnType)
            currentModule->astOverloadResolvedTypes[&expr] = fn;

        // We select this overload
        return {{retPack}};
    }

    return {};
}

bool TypeChecker::handleSelfCallMismatch(const ScopePtr& scope, const AstExprCall& expr, TypePack* args, const std::vector<Location>& argLocations,
    const std::vector<OverloadErrorEntry>& errors)
{
    // No overloads succeeded: Scan for one that would have worked had the user
    // used a.b() rather than a:b() or vice versa.
    for (const auto& [_, argVec, ftv] : errors)
    {
        // Did you write foo:bar() when you should have written foo.bar()?
        if (expr.self)
        {
            std::vector<Location> editedArgLocations(argLocations.begin() + 1, argLocations.end());

            std::vector<TypeId> editedParamList(args->head.begin() + 1, args->head.end());
            TypePackId editedArgPack = addTypePack(TypePack{editedParamList});

            Unifier editedState = mkUnifier(expr.location);
            checkArgumentList(scope, editedState, editedArgPack, ftv->argTypes, editedArgLocations);

            if (editedState.errors.empty())
            {
                reportError(TypeError{expr.location, FunctionDoesNotTakeSelf{}});
                // This is a little bit suspect: If this overload would work with a . replaced by a :
                // we eagerly assume that that's what you actually meant and we commit to it.
                // This could be incorrect if the function has an additional overload that
                // actually works.
                // checkArgumentList(scope, editedState, retPack, ftv->retType, retLocations, CountMismatch::Return);
                return true;
            }
            else
                editedState.log.rollback();
        }
        else if (ftv->hasSelf)
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

                Unifier editedState = mkUnifier(expr.location);

                checkArgumentList(scope, editedState, editedArgPack, ftv->argTypes, editedArgLocations);

                if (editedState.errors.empty())
                {
                    reportError(TypeError{expr.location, FunctionRequiresSelf{}});
                    // This is a little bit suspect: If this overload would work with a : replaced by a .
                    // we eagerly assume that that's what you actually meant and we commit to it.
                    // This could be incorrect if the function has an additional overload that
                    // actually works.
                    // checkArgumentList(scope, editedState, retPack, ftv->retType, retLocations, CountMismatch::Return);
                    return true;
                }
                else
                    editedState.log.rollback();
            }
        }
    }

    return false;
}

ExprResult<TypePackId> TypeChecker::reportOverloadResolutionError(const ScopePtr& scope, const AstExprCall& expr, TypePackId retPack,
    TypePackId argPack, const std::vector<Location>& argLocations, const std::vector<TypeId>& overloads,
    const std::vector<TypeId>& overloadsThatMatchArgCount, const std::vector<OverloadErrorEntry>& errors)
{
    if (overloads.size() == 1)
    {
        reportErrors(std::get<0>(errors.front()));
        return {errorTypePack};
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

        const FunctionTypeVar* ftv = get<FunctionTypeVar>(overload);

        auto error = std::find_if(errors.begin(), errors.end(), [ftv](const OverloadErrorEntry& e) {
            return ftv == std::get<2>(e);
        });

        LUAU_ASSERT(error != errors.end());
        reportErrors(std::get<0>(*error));

        // If only one overload matched, we don't need this error because we provided the previous errors.
        if (overloadsThatMatchArgCount.size() == 1)
            return {errorTypePack};
    }

    std::string s;
    for (size_t i = 0; i < overloadTypes.size(); ++i)
    {
        TypeId overload = overloadTypes[i];
        Unifier state = mkUnifier(expr.location);

        // Unify return types
        if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(overload))
        {
            checkArgumentList(scope, state, retPack, ftv->retType, {});
            checkArgumentList(scope, state, argPack, ftv->argTypes, argLocations);
        }

        if (i > 0)
            s += "; ";

        if (i > 0 && i == overloadTypes.size() - 1)
            s += "and ";

        s += toString(overload);

        state.log.rollback();
    }

    if (overloadsThatMatchArgCount.size() == 0)
        reportError(expr.func->location, ExtraInformation{"Available overloads: " + s});
    else
        reportError(expr.func->location, ExtraInformation{"Other overloads are also not viable: " + s});

    // No viable overload
    return {errorTypePack};
}

ExprResult<TypePackId> TypeChecker::checkExprList(const ScopePtr& scope, const Location& location, const AstArray<AstExpr*>& exprs,
    bool substituteFreeForNil, const std::vector<bool>& instantiateGenerics, const std::vector<std::optional<TypeId>>& expectedTypes)
{
    TypePackId pack = addTypePack(TypePack{});
    PredicateVec predicates; // At the moment we will be pushing all predicate sets into this. Do we need some way to split them up?

    auto insert = [&predicates](PredicateVec& vec) {
        for (Predicate& c : vec)
            predicates.push_back(std::move(c));
    };

    if (exprs.size == 0)
        return {pack};

    TypePack* tp = getMutable<TypePack>(pack);

    size_t lastIndex = exprs.size - 1;
    tp->head.reserve(lastIndex);

    Unifier state = mkUnifier(location);

    for (size_t i = 0; i < exprs.size; ++i)
    {
        AstExpr* expr = exprs.data[i];

        if (i == lastIndex && (expr->is<AstExprCall>() || expr->is<AstExprVarargs>()))
        {
            auto [typePack, exprPredicates] = checkExprPack(scope, *expr);
            insert(exprPredicates);

            tp->tail = typePack;
        }
        else
        {
            std::optional<TypeId> expectedType = i < expectedTypes.size() ? expectedTypes[i] : std::nullopt;
            auto [type, exprPredicates] = checkExpr(scope, *expr, expectedType);
            insert(exprPredicates);

            TypeId actualType = substituteFreeForNil && expr->is<AstExprConstantNil>() ? freshType(scope) : type;

            if (instantiateGenerics.size() > i && instantiateGenerics[i] && (FFlag::LuauGenericFunctions || get<FunctionTypeVar>(actualType)))
                actualType = instantiate(scope, actualType, expr->location);

            if (expectedType)
                state.tryUnify(*expectedType, actualType);

            tp->head.push_back(actualType);
        }
    }

    state.log.rollback();

    return {pack, predicates};
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
    ModulePtr module = resolver->getModule(moduleInfo.name);
    if (!module)
    {
        // There are two reasons why we might fail to find the module:
        // either the file does not exist or there's a cycle. If there's a cycle
        // we will already have reported the error.
        if (!resolver->moduleExists(moduleInfo.name) && (FFlag::LuauTraceRequireLookupChild ? !moduleInfo.optional : true))
        {
            std::string reportedModulePath = resolver->getHumanReadableModuleName(moduleInfo.name);
            reportError(TypeError{location, UnknownRequire{reportedModulePath}});
        }

        return errorType;
    }

    if (module->type != SourceCode::Module)
    {
        std::string humanReadableName = resolver->getHumanReadableModuleName(moduleInfo.name);
        reportError(location, IllegalRequire{humanReadableName, "Module is not a ModuleScript.  It cannot be required."});
        return errorType;
    }

    std::optional<TypeId> moduleType = first(module->getModuleScope()->returnType);
    if (!moduleType)
    {
        std::string humanReadableName = resolver->getHumanReadableModuleName(moduleInfo.name);
        reportError(location, IllegalRequire{humanReadableName, "Module does not return exactly 1 value.  It cannot be required."});
        return errorType;
    }

    SeenTypes seenTypes;
    SeenTypePacks seenTypePacks;
    return clone(*moduleType, currentModule->internalTypes, seenTypes, seenTypePacks);
}

void TypeChecker::tablify(TypeId type)
{
    type = follow(type);

    if (auto f = get<FreeTypeVar>(type))
        *asMutable(type) = TableTypeVar{TableState::Free, f->level};
}

TypeId TypeChecker::anyIfNonstrict(TypeId ty) const
{
    if (isNonstrictMode())
        return anyType;
    else
        return ty;
}

bool TypeChecker::unify(TypeId left, TypeId right, const Location& location)
{
    Unifier state = mkUnifier(location);
    state.tryUnify(left, right);

    reportErrors(state.errors);

    return state.errors.empty();
}

bool TypeChecker::unify(TypePackId left, TypePackId right, const Location& location, CountMismatch::Context ctx)
{
    Unifier state = mkUnifier(location);
    state.ctx = ctx;
    state.tryUnify(left, right);

    reportErrors(state.errors);

    return state.errors.empty();
}

bool TypeChecker::unifyWithInstantiationIfNeeded(const ScopePtr& scope, TypeId left, TypeId right, const Location& location)
{
    LUAU_ASSERT(FFlag::LuauRankNTypes);
    Unifier state = mkUnifier(location);
    unifyWithInstantiationIfNeeded(scope, left, right, state);

    reportErrors(state.errors);

    return state.errors.empty();
}

void TypeChecker::unifyWithInstantiationIfNeeded(const ScopePtr& scope, TypeId left, TypeId right, Unifier& state)
{
    LUAU_ASSERT(FFlag::LuauRankNTypes);
    if (!maybeGeneric(right))
        // Quick check to see if we definitely can't instantiate
        state.tryUnify(left, right, /*isFunctionCall*/ false);
    else if (!maybeGeneric(left) && isGeneric(right))
    {
        // Quick check to see if we definitely have to instantiate
        TypeId instantiated = instantiate(scope, right, state.location);
        state.tryUnify(left, instantiated, /*isFunctionCall*/ false);
    }
    else
    {
        // First try unifying with the original uninstantiated type
        // but if that fails, try the instantiated one.
        Unifier child = state.makeChildUnifier();
        child.tryUnify(left, right, /*isFunctionCall*/ false);
        if (!child.errors.empty())
        {
            TypeId instantiated = instantiate(scope, right, state.location);
            if (right == instantiated)
            {
                // Instantiating the argument made no difference, so just report any child errors
                state.log.concat(std::move(child.log));
                state.errors.insert(state.errors.end(), child.errors.begin(), child.errors.end());
            }
            else
            {
                child.log.rollback();
                state.tryUnify(left, instantiated, /*isFunctionCall*/ false);
            }
        }
        else
        {
            state.log.concat(std::move(child.log));
        }
    }
}

bool Instantiation::isDirty(TypeId ty)
{
    if (FFlag::LuauRankNTypes)
    {
        if (get<FunctionTypeVar>(ty))
            return true;
        else
            return false;
    }

    if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty))
        return !ftv->generics.empty() || !ftv->genericPacks.empty();
    else if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
        return ttv->state == TableState::Generic;
    else if (get<GenericTypeVar>(ty))
        return true;
    else
        return false;
}

bool Instantiation::isDirty(TypePackId tp)
{
    if (FFlag::LuauRankNTypes)
        return false;

    if (get<GenericTypePack>(tp))
        return true;
    else
        return false;
}

bool Instantiation::ignoreChildren(TypeId ty)
{
    LUAU_ASSERT(FFlag::LuauRankNTypes);
    if (get<FunctionTypeVar>(ty))
        return true;
    else
        return false;
}

TypeId Instantiation::clean(TypeId ty)
{
    LUAU_ASSERT(isDirty(ty));

    if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty))
    {
        FunctionTypeVar clone = FunctionTypeVar{level, ftv->argTypes, ftv->retType, ftv->definition, ftv->hasSelf};
        clone.magicFunction = ftv->magicFunction;
        clone.tags = ftv->tags;
        clone.argNames = ftv->argNames;
        TypeId result = addType(std::move(clone));

        if (FFlag::LuauRankNTypes)
        {
            // Annoyingly, we have to do this even if there are no generics,
            // to replace any generic tables.
            replaceGenerics.level = level;
            replaceGenerics.currentModule = currentModule;
            replaceGenerics.generics.assign(ftv->generics.begin(), ftv->generics.end());
            replaceGenerics.genericPacks.assign(ftv->genericPacks.begin(), ftv->genericPacks.end());

            // TODO: What to do if this returns nullopt?
            // We don't have access to the error-reporting machinery
            result = replaceGenerics.substitute(result).value_or(result);
        }

        asMutable(result)->documentationSymbol = ty->documentationSymbol;
        return result;
    }
    else if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
    {
        LUAU_ASSERT(!FFlag::LuauRankNTypes);
        TableTypeVar clone = TableTypeVar{ttv->props, ttv->indexer, level, TableState::Free};
        clone.methodDefinitionLocations = ttv->methodDefinitionLocations;
        clone.definitionModuleName = ttv->definitionModuleName;
        TypeId result = addType(std::move(clone));

        asMutable(result)->documentationSymbol = ty->documentationSymbol;
        return result;
    }
    else
    {
        LUAU_ASSERT(!FFlag::LuauRankNTypes);
        TypeId result = addType(FreeTypeVar{level});

        asMutable(result)->documentationSymbol = ty->documentationSymbol;
        return result;
    }
}

TypePackId Instantiation::clean(TypePackId tp)
{
    LUAU_ASSERT(!FFlag::LuauRankNTypes);
    return addTypePack(TypePackVar(FreeTypePack{level}));
}

bool ReplaceGenerics::ignoreChildren(TypeId ty)
{
    LUAU_ASSERT(FFlag::LuauRankNTypes);
    if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty))
        // We aren't recursing in the case of a generic function which
        // binds the same generics. This can happen if, for example, there's recursive types.
        // If T = <a>(a,T)->T then instantiating T should produce T' = (X,T)->T not T' = (X,T')->T'.
        // It's OK to use vector equality here, since we always generate fresh generics
        // whenever we quantify, so the vectors overlap if and only if they are equal.
        return (!generics.empty() || !genericPacks.empty()) && (ftv->generics == generics) && (ftv->genericPacks == genericPacks);
    else
        return false;
}

bool ReplaceGenerics::isDirty(TypeId ty)
{
    LUAU_ASSERT(FFlag::LuauRankNTypes);
    if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
        return ttv->state == TableState::Generic;
    else if (get<GenericTypeVar>(ty))
        return std::find(generics.begin(), generics.end(), ty) != generics.end();
    else
        return false;
}

bool ReplaceGenerics::isDirty(TypePackId tp)
{
    LUAU_ASSERT(FFlag::LuauRankNTypes);
    if (get<GenericTypePack>(tp))
        return std::find(genericPacks.begin(), genericPacks.end(), tp) != genericPacks.end();
    else
        return false;
}

TypeId ReplaceGenerics::clean(TypeId ty)
{
    LUAU_ASSERT(isDirty(ty));
    if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
    {
        TableTypeVar clone = TableTypeVar{ttv->props, ttv->indexer, level, TableState::Free};
        clone.methodDefinitionLocations = ttv->methodDefinitionLocations;
        clone.definitionModuleName = ttv->definitionModuleName;
        return addType(std::move(clone));
    }
    else
        return addType(FreeTypeVar{level});
}

TypePackId ReplaceGenerics::clean(TypePackId tp)
{
    LUAU_ASSERT(isDirty(tp));
    return addTypePack(TypePackVar(FreeTypePack{level}));
}

bool Quantification::isDirty(TypeId ty)
{
    if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
        return level.subsumes(ttv->level) && ((ttv->state == TableState::Free) || (ttv->state == TableState::Unsealed));
    else if (const FreeTypeVar* ftv = get<FreeTypeVar>(ty))
        return level.subsumes(ftv->level);
    else
        return false;
}

bool Quantification::isDirty(TypePackId tp)
{
    if (const FreeTypePack* ftv = get<FreeTypePack>(tp))
        return level.subsumes(ftv->level);
    else
        return false;
}

TypeId Quantification::clean(TypeId ty)
{
    LUAU_ASSERT(isDirty(ty));
    if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
    {
        TableState state = (ttv->state == TableState::Unsealed ? TableState::Sealed : TableState::Generic);
        TableTypeVar clone = TableTypeVar{ttv->props, ttv->indexer, level, state};
        clone.methodDefinitionLocations = ttv->methodDefinitionLocations;
        clone.definitionModuleName = ttv->definitionModuleName;
        return addType(std::move(clone));
    }
    else
    {
        TypeId generic = addType(GenericTypeVar{level});
        generics.push_back(generic);
        return generic;
    }
}

TypePackId Quantification::clean(TypePackId tp)
{
    LUAU_ASSERT(isDirty(tp));
    TypePackId genericPack = addTypePack(TypePackVar(GenericTypePack{level}));
    genericPacks.push_back(genericPack);
    return genericPack;
}

bool Anyification::isDirty(TypeId ty)
{
    if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
        return (ttv->state == TableState::Free);
    else if (get<FreeTypeVar>(ty))
        return true;
    else
        return false;
}

bool Anyification::isDirty(TypePackId tp)
{
    if (get<FreeTypePack>(tp))
        return true;
    else
        return false;
}

TypeId Anyification::clean(TypeId ty)
{
    LUAU_ASSERT(isDirty(ty));
    if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
    {
        TableTypeVar clone = TableTypeVar{ttv->props, ttv->indexer, ttv->level, TableState::Sealed};
        clone.methodDefinitionLocations = ttv->methodDefinitionLocations;
        clone.definitionModuleName = ttv->definitionModuleName;
        return addType(std::move(clone));
    }
    else
        return anyType;
}

TypePackId Anyification::clean(TypePackId tp)
{
    LUAU_ASSERT(isDirty(tp));
    return anyTypePack;
}

TypeId TypeChecker::quantify(const ScopePtr& scope, TypeId ty, Location location)
{
    ty = follow(ty);

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty);
    if (!ftv || !ftv->generics.empty() || !ftv->genericPacks.empty())
        return ty;

    quantification.level = scope->level;
    quantification.generics.clear();
    quantification.genericPacks.clear();
    quantification.currentModule = currentModule;

    std::optional<TypeId> qty = quantification.substitute(ty);

    if (!qty.has_value())
    {
        reportError(location, UnificationTooComplex{});
        return errorType;
    }

    if (ty == *qty)
        return ty;

    FunctionTypeVar* qftv = getMutable<FunctionTypeVar>(*qty);
    LUAU_ASSERT(qftv);
    qftv->generics = quantification.generics;
    qftv->genericPacks = quantification.genericPacks;
    return *qty;
}

TypeId TypeChecker::instantiate(const ScopePtr& scope, TypeId ty, Location location)
{
    instantiation.level = scope->level;
    instantiation.currentModule = currentModule;
    std::optional<TypeId> instantiated = instantiation.substitute(ty);
    if (instantiated.has_value())
        return *instantiated;
    else
    {
        reportError(location, UnificationTooComplex{});
        return errorType;
    }
}

TypePackId TypeChecker::DEPRECATED_instantiate(const ScopePtr& scope, TypePackId ty, Location location)
{
    LUAU_ASSERT(!FFlag::LuauRankNTypes);
    instantiation.level = scope->level;
    instantiation.currentModule = currentModule;
    std::optional<TypePackId> instantiated = instantiation.substitute(ty);
    if (instantiated.has_value())
        return *instantiated;
    else
    {
        reportError(location, UnificationTooComplex{});
        return errorTypePack;
    }
}

TypeId TypeChecker::anyify(const ScopePtr& scope, TypeId ty, Location location)
{
    anyification.anyType = anyType;
    anyification.anyTypePack = anyTypePack;
    anyification.currentModule = currentModule;
    std::optional<TypeId> any = anyification.substitute(ty);
    if (any.has_value())
        return *any;
    else
    {
        reportError(location, UnificationTooComplex{});
        return errorType;
    }
}

TypePackId TypeChecker::anyify(const ScopePtr& scope, TypePackId ty, Location location)
{
    anyification.anyType = anyType;
    anyification.anyTypePack = anyTypePack;
    anyification.currentModule = currentModule;
    std::optional<TypePackId> any = anyification.substitute(ty);
    if (any.has_value())
        return *any;
    else
    {
        reportError(location, UnificationTooComplex{});
        return errorTypePack;
    }
}

void TypeChecker::reportError(const TypeError& error)
{
    if (currentModule->mode == Mode::NoCheck)
        return;
    currentModule->errors.push_back(error);
    currentModule->errors.back().moduleName = currentModuleName;
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

void TypeChecker::ice(const std::string& message, const Location& location)
{
    iceHandler->ice(message, location);
}

void TypeChecker::ice(const std::string& message)
{
    iceHandler->ice(message);
}

void TypeChecker::prepareErrorsForDisplay(ErrorVec& errVec)
{
    // Remove errors with names that were generated by recovery from a parse error
    errVec.erase(std::remove_if(errVec.begin(), errVec.end(),
                     [](auto& err) {
                         return containsParseErrorName(err);
                     }),
        errVec.end());

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

    auto accumulate = [&](const TableTypeVar::Props& props) {
        for (const auto& [name, ty] : props)
        {
            if (sv != name && equalsLower(sv, name))
                candidates.insert(name);
        }
    };

    if (auto ttv = getTableType(follow(utk->table)))
        accumulate(ttv->props);
    else if (auto ctv = get<ClassTypeVar>(follow(utk->table)))
    {
        while (ctv)
        {
            accumulate(ctv->props);

            if (!ctv->parent)
                break;

            ctv = get<ClassTypeVar>(*ctv->parent);
            LUAU_ASSERT(ctv);
        }
    }

    if (!candidates.empty())
        data = TypeErrorData(UnknownPropButFoundLikeProp{utk->table, utk->key, candidates});
}

LUAU_NOINLINE void TypeChecker::reportErrorCodeTooComplex(const Location& location)
{
    reportError(TypeError{location, CodeTooComplex{}});
}

// Creates a new Scope but without carrying forward the varargs from the parent.
ScopePtr TypeChecker::childFunctionScope(const ScopePtr& parent, const Location& location, int subLevel)
{
    ScopePtr scope = std::make_shared<Scope>(parent, subLevel);
    currentModule->scopes.push_back(std::make_pair(location, scope));
    return scope;
}

// Creates a new Scope and carries forward the varargs from the parent.
ScopePtr TypeChecker::childScope(const ScopePtr& parent, const Location& location, int subLevel)
{
    ScopePtr scope = std::make_shared<Scope>(parent, subLevel);
    scope->varargPack = parent->varargPack;

    currentModule->scopes.push_back(std::make_pair(location, scope));
    return scope;
}

void TypeChecker::merge(RefinementMap& l, const RefinementMap& r)
{
    Luau::merge(l, r, [this](TypeId a, TypeId b) {
        // TODO: normalize(UnionTypeVar{{a, b}})
        std::unordered_set<TypeId> set;

        if (auto utv = get<UnionTypeVar>(follow(a)))
            set.insert(begin(utv), end(utv));
        else
            set.insert(a);

        if (auto utv = get<UnionTypeVar>(follow(b)))
            set.insert(begin(utv), end(utv));
        else
            set.insert(b);

        std::vector<TypeId> options(set.begin(), set.end());
        if (set.size() == 1)
            return options[0];
        return addType(UnionTypeVar{std::move(options)});
    });
}

Unifier TypeChecker::mkUnifier(const Location& location)
{
    return Unifier{&currentModule->internalTypes, currentModule->mode, globalScope, location, Variance::Covariant, iceHandler};
}

Unifier TypeChecker::mkUnifier(const std::vector<std::pair<TypeId, TypeId>>& seen, const Location& location)
{
    return Unifier{&currentModule->internalTypes, currentModule->mode, globalScope, seen, location, Variance::Covariant, iceHandler};
}

TypeId TypeChecker::freshType(const ScopePtr& scope)
{
    return freshType(scope->level);
}

TypeId TypeChecker::freshType(TypeLevel level)
{
    return currentModule->internalTypes.typeVars.allocate(TypeVar(FreeTypeVar(level)));
}

TypeId TypeChecker::DEPRECATED_freshType(const ScopePtr& scope, bool canBeGeneric)
{
    return DEPRECATED_freshType(scope->level, canBeGeneric);
}

TypeId TypeChecker::DEPRECATED_freshType(TypeLevel level, bool canBeGeneric)
{
    TypeId allocated = currentModule->internalTypes.typeVars.allocate(TypeVar(FreeTypeVar(level, canBeGeneric)));
    if (FFlag::DebugLuauTrackOwningArena)
        asMutable(allocated)->owningArena = &currentModule->internalTypes;

    return allocated;
}

std::optional<TypeId> TypeChecker::filterMap(TypeId type, TypeIdPredicate predicate)
{
    std::vector<TypeId> types = Luau::filterMap(type, predicate);
    if (!types.empty())
        return types.size() == 1 ? types[0] : addType(UnionTypeVar{std::move(types)});
    return std::nullopt;
}

TypeId TypeChecker::addType(const UnionTypeVar& utv)
{
    LUAU_ASSERT(utv.options.size() > 1);

    return addTV(TypeVar(utv));
}

TypeId TypeChecker::addTV(TypeVar&& tv)
{
    TypeId allocated = currentModule->internalTypes.typeVars.allocate(std::move(tv));
    if (FFlag::DebugLuauTrackOwningArena)
        asMutable(allocated)->owningArena = &currentModule->internalTypes;

    return allocated;
}

TypePackId TypeChecker::addTypePack(TypePackVar&& tv)
{
    TypePackId allocated = currentModule->internalTypes.typePacks.allocate(std::move(tv));
    if (FFlag::DebugLuauTrackOwningArena)
        asMutable(allocated)->owningArena = &currentModule->internalTypes;

    return allocated;
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

TypePackId TypeChecker::DEPRECATED_freshTypePack(const ScopePtr& scope, bool canBeGeneric)
{
    return DEPRECATED_freshTypePack(scope->level, canBeGeneric);
}

TypePackId TypeChecker::DEPRECATED_freshTypePack(TypeLevel level, bool canBeGeneric)
{
    return addTypePack(TypePackVar(FreeTypePack(level, canBeGeneric)));
}

TypeId TypeChecker::resolveType(const ScopePtr& scope, const AstType& annotation, bool DEPRECATED_canBeGeneric)
{
    if (DEPRECATED_canBeGeneric)
        LUAU_ASSERT(!FFlag::LuauRankNTypes);

    if (const auto& lit = annotation.as<AstTypeReference>())
    {
        std::optional<TypeFun> tf;
        if (lit->hasPrefix)
            tf = scope->lookupImportedType(lit->prefix.value, lit->name.value);

        else if (FFlag::DebugLuauMagicTypes && lit->name == "_luau_ice")
            ice("_luau_ice encountered", lit->location);

        else if (FFlag::DebugLuauMagicTypes && lit->name == "_luau_print")
        {
            if (lit->generics.size != 1)
            {
                reportError(TypeError{annotation.location, GenericError{"_luau_print requires one generic parameter"}});
                return addType(ErrorTypeVar{});
            }

            ToStringOptions opts;
            opts.exhaustive = true;
            opts.maxTableLength = 0;

            TypeId param = resolveType(scope, *lit->generics.data[0]);
            luauPrintLine(format("_luau_print\t%s\t|\t%s", toString(param, opts).c_str(), toString(lit->location).c_str()));
            return param;
        }

        else
            tf = scope->lookupType(lit->name.value);

        if (!tf)
        {
            if (lit->name == Parser::errorName)
                return addType(ErrorTypeVar{});

            std::string typeName;
            if (lit->hasPrefix)
                typeName = std::string(lit->prefix.value) + ".";
            typeName += lit->name.value;

            if (scope->lookupPack(typeName))
                reportError(TypeError{annotation.location, SwappedGenericTypeParameter{typeName, SwappedGenericTypeParameter::Type}});
            else
                reportError(TypeError{annotation.location, UnknownSymbol{typeName, UnknownSymbol::Type}});

            return addType(ErrorTypeVar{});
        }

        if (lit->generics.size == 0 && tf->typeParams.empty())
            return tf->type;
        else if (lit->generics.size != tf->typeParams.size())
        {
            reportError(TypeError{annotation.location, IncorrectGenericParameterCount{lit->name.value, *tf, lit->generics.size}});
            return addType(ErrorTypeVar{});
        }
        else
        {
            std::vector<TypeId> typeParams;
            for (AstType* paramAnnot : lit->generics)
                typeParams.push_back(resolveType(scope, *paramAnnot));

            if (FFlag::LuauRecursiveTypeParameterRestriction && typeParams == tf->typeParams)
            {
                // If the generic parameters and the type arguments are the same, we are about to
                // perform an identity substitution, which we can just short-circuit.
                return tf->type;
            }

            return instantiateTypeFun(scope, *tf, typeParams, annotation.location);
        }
    }
    else if (const auto& table = annotation.as<AstTypeTable>())
    {
        TableTypeVar::Props props;
        std::optional<TableIndexer> tableIndexer;

        for (const auto& prop : table->props)
            props[prop.name.value] = {resolveType(scope, *prop.type, DEPRECATED_canBeGeneric)};

        if (const auto& indexer = table->indexer)
            tableIndexer = TableIndexer(
                resolveType(scope, *indexer->indexType, DEPRECATED_canBeGeneric), resolveType(scope, *indexer->resultType, DEPRECATED_canBeGeneric));

        return addType(TableTypeVar{
            props, tableIndexer, scope->level,
            TableState::Sealed // FIXME: probably want a way to annotate other kinds of tables maybe
        });
    }
    else if (const auto& func = annotation.as<AstTypeFunction>())
    {
        ScopePtr funcScope = childScope(scope, func->location);

        std::vector<TypeId> generics;
        std::vector<TypePackId> genericPacks;

        if (FFlag::LuauGenericFunctions)
        {
            std::tie(generics, genericPacks) = createGenericTypes(funcScope, annotation, func->generics, func->genericPacks);
        }

        // TODO: better error message CLI-39912
        if (FFlag::LuauGenericFunctions && !FFlag::LuauRankNTypes && !DEPRECATED_canBeGeneric && (generics.size() > 0 || genericPacks.size() > 0))
            reportError(TypeError{annotation.location, GenericError{"generic function where only monotypes are allowed"}});

        TypePackId argTypes = resolveTypePack(funcScope, func->argTypes);
        TypePackId retTypes = resolveTypePack(funcScope, func->returnTypes);

        TypeId fnType = addType(FunctionTypeVar{funcScope->level, std::move(generics), std::move(genericPacks), argTypes, retTypes});

        FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(fnType);

        ftv->argNames.reserve(func->argNames.size);
        for (const auto& el : func->argNames)
        {
            if (el)
                ftv->argNames.push_back(FunctionArgument{el->first.value, el->second});
            else
                ftv->argNames.push_back(std::nullopt);
        }

        return fnType;
    }
    else if (auto typeOf = annotation.as<AstTypeTypeof>())
    {
        TypeId ty = checkExpr(scope, *typeOf->expr).type;
        // TODO: better error message CLI-39912
        if (FFlag::LuauGenericFunctions && !FFlag::LuauRankNTypes && !DEPRECATED_canBeGeneric && isGeneric(ty))
            reportError(TypeError{annotation.location, GenericError{"typeof produced a polytype where only monotypes are allowed"}});
        return ty;
    }
    else if (const auto& un = annotation.as<AstTypeUnion>())
    {
        std::vector<TypeId> types;
        for (AstType* ann : un->types)
            types.push_back(resolveType(scope, *ann, DEPRECATED_canBeGeneric));

        return addType(UnionTypeVar{types});
    }
    else if (const auto& un = annotation.as<AstTypeIntersection>())
    {
        std::vector<TypeId> types;
        for (AstType* ann : un->types)
            types.push_back(resolveType(scope, *ann, DEPRECATED_canBeGeneric));

        return addType(IntersectionTypeVar{types});
    }
    else if (annotation.is<AstTypeError>())
    {
        return addType(ErrorTypeVar{});
    }
    else
    {
        reportError(TypeError{annotation.location, GenericError{"Unknown type annotation?"}});
        return addType(ErrorTypeVar{});
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
        return addTypePack(TypePack{head, tail});
    }

    return addTypePack(TypePack{});
}

TypePackId TypeChecker::resolveTypePack(const ScopePtr& scope, const AstTypePack& annotation)
{
    if (const AstTypePackVariadic* variadic = annotation.as<AstTypePackVariadic>())
    {
        return addTypePack(TypePackVar{VariadicTypePack{resolveType(scope, *variadic->variadicType)}});
    }
    else if (const AstTypePackGeneric* generic = annotation.as<AstTypePackGeneric>())
    {
        Name genericName = Name(generic->genericName.value);
        std::optional<TypePackId> genericTy = scope->lookupPack(genericName);

        if (!genericTy)
        {
            if (scope->lookupType(genericName))
                reportError(TypeError{generic->location, SwappedGenericTypeParameter{genericName, SwappedGenericTypeParameter::Pack}});
            else
                reportError(TypeError{generic->location, UnknownSymbol{genericName, UnknownSymbol::Type}});

            return addTypePack(TypePackVar{Unifiable::Error{}});
        }

        return *genericTy;
    }
    else
    {
        ice("Unknown AstTypePack kind");
    }
}

bool ApplyTypeFunction::isDirty(TypeId ty)
{
    // Really this should just replace the arguments,
    // but for bug-compatibility with existing code, we replace
    // all generics.
    if (get<GenericTypeVar>(ty))
        return true;
    else if (const FreeTypeVar* ftv = get<FreeTypeVar>(ty))
    {
        if (FFlag::LuauRecursiveTypeParameterRestriction && ftv->forwardedTypeAlias)
            encounteredForwardedType = true;
        return false;
    }
    else
        return false;
}

bool ApplyTypeFunction::isDirty(TypePackId tp)
{
    // Really this should just replace the arguments,
    // but for bug-compatibility with existing code, we replace
    // all generics.
    if (get<GenericTypePack>(tp))
        return true;
    else
        return false;
}

TypeId ApplyTypeFunction::clean(TypeId ty)
{
    // Really this should just replace the arguments,
    // but for bug-compatibility with existing code, we replace
    // all generics by free type variables.
    TypeId& arg = arguments[ty];
    if (arg)
        return arg;
    else
        return addType(FreeTypeVar{level});
}

TypePackId ApplyTypeFunction::clean(TypePackId tp)
{
    // Really this should just replace the arguments,
    // but for bug-compatibility with existing code, we replace
    // all generics by free type variables.
    return addTypePack(FreeTypePack{level});
}

TypeId TypeChecker::instantiateTypeFun(const ScopePtr& scope, const TypeFun& tf, const std::vector<TypeId>& typeParams, const Location& location)
{
    if (tf.typeParams.empty())
        return tf.type;

    applyTypeFunction.arguments.clear();
    for (size_t i = 0; i < tf.typeParams.size(); ++i)
        applyTypeFunction.arguments[tf.typeParams[i]] = typeParams[i];
    applyTypeFunction.currentModule = currentModule;
    applyTypeFunction.level = scope->level;
    applyTypeFunction.encounteredForwardedType = false;
    std::optional<TypeId> maybeInstantiated = applyTypeFunction.substitute(tf.type);
    if (!maybeInstantiated.has_value())
    {
        reportError(location, UnificationTooComplex{});
        return errorType;
    }
    if (FFlag::LuauRecursiveTypeParameterRestriction && applyTypeFunction.encounteredForwardedType)
    {
        reportError(TypeError{location, GenericError{"Recursive type being used with different parameters"}});
        return errorType;
    }

    TypeId instantiated = *maybeInstantiated;

    if (FFlag::LuauCloneCorrectlyBeforeMutatingTableType)
    {
        // TODO: CLI-46926 it's a bad idea to rename the type whether we follow through the BoundTypeVar or not
        TypeId target = FFlag::LuauFollowInTypeFunApply ? follow(instantiated) : instantiated;

        bool needsClone = follow(tf.type) == target;
        TableTypeVar* ttv = getMutableTableType(target);

        if (ttv && needsClone)
        {
            // Substitution::clone is a shallow clone. If this is a metatable type, we
            // want to mutate its table, so we need to explicitly clone that table as
            // well. If we don't, we will mutate another module's type surface and cause
            // a use-after-free.
            if (get<MetatableTypeVar>(target))
            {
                instantiated = applyTypeFunction.clone(tf.type);
                MetatableTypeVar* mtv = getMutable<MetatableTypeVar>(instantiated);
                mtv->table = applyTypeFunction.clone(mtv->table);
                ttv = getMutable<TableTypeVar>(mtv->table);
            }
            if (get<TableTypeVar>(target))
            {
                instantiated = applyTypeFunction.clone(tf.type);
                ttv = getMutable<TableTypeVar>(instantiated);
            }
        }

        if (ttv)
        {
            ttv->instantiatedTypeParams = typeParams;
        }
    }
    else
    {
        if (TableTypeVar* ttv = getMutableTableType(instantiated))
        {
            if (follow(tf.type) == instantiated)
            {
                // This can happen if a type alias has generics that it does not use at all.
                // ex type FooBar<T> = { a: number }
                instantiated = applyTypeFunction.clone(tf.type);
                ttv = getMutableTableType(instantiated);
            }

            ttv->instantiatedTypeParams = typeParams;
        }
    }

    return instantiated;
}

std::pair<std::vector<TypeId>, std::vector<TypePackId>> TypeChecker::createGenericTypes(
    const ScopePtr& scope, const AstNode& node, const AstArray<AstName>& genericNames, const AstArray<AstName>& genericPackNames)
{
    std::vector<TypeId> generics;
    for (const AstName& generic : genericNames)
    {
        Name n = generic.value;

        // These generics are the only thing that will ever be added to scope, so we can be certain that
        // a collision can only occur when two generic typevars have the same name.
        if (scope->privateTypeBindings.count(n) || scope->privateTypePackBindings.count(n))
        {
            // TODO(jhuelsman): report the exact span of the generic type parameter whose name is a duplicate.
            reportError(TypeError{node.location, DuplicateGenericParameter{n}});
        }

        TypeId g = addType(Unifiable::Generic{scope->level, n});
        generics.push_back(g);
        scope->privateTypeBindings[n] = TypeFun{{}, g};
    }

    std::vector<TypePackId> genericPacks;
    for (const AstName& genericPack : genericPackNames)
    {
        Name n = genericPack.value;

        // These generics are the only thing that will ever be added to scope, so we can be certain that
        // a collision can only occur when two generic typevars have the same name.
        if (scope->privateTypePackBindings.count(n) || scope->privateTypeBindings.count(n))
        {
            // TODO(jhuelsman): report the exact span of the generic type parameter whose name is a duplicate.
            reportError(TypeError{node.location, DuplicateGenericParameter{n}});
        }

        TypePackId g = addTypePack(TypePackVar{Unifiable::Generic{scope->level, n}});
        genericPacks.push_back(g);
        scope->privateTypePackBindings[n] = g;
    }

    return {generics, genericPacks};
}

std::optional<TypeId> TypeChecker::resolveLValue(const ScopePtr& scope, const LValue& lvalue)
{
    std::string path = toString(lvalue);
    auto [symbol, keys] = getFullName(lvalue);

    ScopePtr currentScope = scope;
    while (currentScope)
    {
        if (auto it = currentScope->refinements.find(path); it != currentScope->refinements.end())
            return it->second;

        // Should not be using scope->lookup. This is already recursive.
        if (auto it = currentScope->bindings.find(symbol); it != currentScope->bindings.end())
        {
            std::optional<TypeId> currentTy = it->second.typeId;

            for (std::string key : keys)
            {
                // TODO: This function probably doesn't need Location at all, or at least should hide the argument.
                currentTy = getIndexTypeFromType(scope, *currentTy, key, Location(), false);
                if (!currentTy)
                    break;
            }

            return currentTy;
        }

        currentScope = currentScope->parent;
    }

    return std::nullopt;
}

std::optional<TypeId> TypeChecker::resolveLValue(const RefinementMap& refis, const ScopePtr& scope, const LValue& lvalue)
{
    if (auto it = refis.find(toString(lvalue)); it != refis.end())
        return it->second;
    else
        return resolveLValue(scope, lvalue);
}

// Only should be used for refinements!
// This can probably go away once we have something that can limit a free type's type domain.
static bool isUndecidable(TypeId ty)
{
    ty = follow(ty);
    return get<AnyTypeVar>(ty) || get<ErrorTypeVar>(ty) || get<FreeTypeVar>(ty);
}

ErrorVec TypeChecker::resolve(const PredicateVec& predicates, const ScopePtr& scope, bool sense)
{
    ErrorVec errVec;
    resolve(predicates, errVec, scope->refinements, scope, sense);
    return errVec;
}

void TypeChecker::resolve(const PredicateVec& predicates, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense, bool fromOr)
{
    for (const Predicate& c : predicates)
        resolve(c, errVec, refis, scope, sense, fromOr);
}

void TypeChecker::resolve(const Predicate& predicate, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense, bool fromOr)
{
    if (auto truthyP = get<TruthyPredicate>(predicate))
        resolve(*truthyP, errVec, refis, scope, sense, fromOr);
    else if (auto andP = get<AndPredicate>(predicate))
        resolve(*andP, errVec, refis, scope, sense);
    else if (auto orP = get<OrPredicate>(predicate))
        resolve(*orP, errVec, refis, scope, sense);
    else if (auto notP = get<NotPredicate>(predicate))
        resolve(notP->predicates, errVec, refis, scope, !sense, fromOr);
    else if (auto isaP = get<IsAPredicate>(predicate))
        resolve(*isaP, errVec, refis, scope, sense);
    else if (auto typeguardP = get<TypeGuardPredicate>(predicate))
    {
        if (FFlag::LuauImprovedTypeGuardPredicate2)
            resolve(*typeguardP, errVec, refis, scope, sense);
        else
            DEPRECATED_resolve(*typeguardP, errVec, refis, scope, sense);
    }
    else if (auto eqP = get<EqPredicate>(predicate); eqP && FFlag::LuauEqConstraint)
        resolve(*eqP, errVec, refis, scope, sense);
    else
        ice("Unhandled predicate kind");
}

void TypeChecker::resolve(const TruthyPredicate& truthyP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense, bool fromOr)
{
    auto predicate = [sense](TypeId option) -> std::optional<TypeId> {
        if (isUndecidable(option) || isBoolean(option) || isNil(option) != sense)
            return option;

        return std::nullopt;
    };

    std::optional<TypeId> ty = resolveLValue(refis, scope, truthyP.lvalue);
    if (!ty)
        return;

    // This is a hack. :(
    // Without this, the expression 'a or b' might refine 'b' to be falsy.
    // I'm not yet sure how else to get this to do the right thing without this hack, so we'll do this for now in the meantime.
    if (fromOr)
        return addRefinement(refis, truthyP.lvalue, *ty);

    if (std::optional<TypeId> result = filterMap(*ty, predicate))
        addRefinement(refis, truthyP.lvalue, *result);
}

void TypeChecker::resolve(const AndPredicate& andP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense)
{
    if (FFlag::LuauOrPredicate)
    {
        if (!sense)
        {
            OrPredicate orP{
                {NotPredicate{std::move(andP.lhs)}},
                {NotPredicate{std::move(andP.rhs)}},
            };

            return resolve(orP, errVec, refis, scope, !sense);
        }

        resolve(andP.lhs, errVec, refis, scope, sense);
        resolve(andP.rhs, errVec, refis, scope, sense);
    }
    else
    {
        // And predicate is currently not resolvable when sense is false. 'not (a and b)' is synonymous with '(not a) or (not b)'.
        // TODO: implement environment merging to permit this case.
        if (!sense)
            return;

        resolve(andP.lhs, errVec, refis, scope, sense);
        resolve(andP.rhs, errVec, refis, scope, sense);
    }
}

void TypeChecker::resolve(const OrPredicate& orP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense)
{
    if (!sense)
    {
        AndPredicate andP{
            {NotPredicate{std::move(orP.lhs)}},
            {NotPredicate{std::move(orP.rhs)}},
        };

        return resolve(andP, errVec, refis, scope, !sense);
    }

    ErrorVec discarded;

    RefinementMap leftRefis;
    resolve(orP.lhs, errVec, leftRefis, scope, sense);

    RefinementMap rightRefis;
    resolve(orP.lhs, discarded, rightRefis, scope, !sense);
    resolve(orP.rhs, errVec, rightRefis, scope, sense, true); // :(

    merge(refis, leftRefis);
    merge(refis, rightRefis);
}

void TypeChecker::resolve(const IsAPredicate& isaP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense)
{
    auto predicate = [&](TypeId option) -> std::optional<TypeId> {
        if (FFlag::LuauTypeGuardPeelsAwaySubclasses)
        {
            // This by itself is not truly enough to determine that A is stronger than B or vice versa.
            // The best unambiguous way about this would be to have a function that returns the relationship ordering of a pair.
            // i.e. TypeRelationship relationshipOf(TypeId superTy, TypeId subTy)
            bool optionIsSubtype = canUnify(isaP.ty, option, isaP.location).empty();
            bool targetIsSubtype = canUnify(option, isaP.ty, isaP.location).empty();

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
                auto ttv = get<TableTypeVar>(option);
                if (isUndecidable(option) || (ttv && ttv->state == TableState::Free))
                    return sense ? isaP.ty : option;

                // typeof(x) == "Instance" where x : Instance
                if (sense)
                    return isaP.ty;
            }
        }
        else if (FFlag::LuauImprovedTypeGuardPredicate2)
        {
            auto lctv = get<ClassTypeVar>(option);
            auto rctv = get<ClassTypeVar>(isaP.ty);

            if (isSubclass(lctv, rctv) == sense)
                return option;

            if (isSubclass(rctv, lctv) == sense)
                return isaP.ty;

            if (canUnify(option, isaP.ty, isaP.location).empty() == sense)
                return isaP.ty;
        }
        else
        {
            auto lctv = get<ClassTypeVar>(option);
            auto rctv = get<ClassTypeVar>(isaP.ty);

            if (lctv && rctv)
            {
                if (isSubclass(lctv, rctv) == sense)
                    return option;
                else if (isSubclass(rctv, lctv) == sense)
                    return isaP.ty;
            }
        }

        return std::nullopt;
    };

    std::optional<TypeId> ty = resolveLValue(refis, scope, isaP.lvalue);
    if (!ty)
        return;

    if (std::optional<TypeId> result = filterMap(*ty, predicate))
        addRefinement(refis, isaP.lvalue, *result);
    else
    {
        addRefinement(refis, isaP.lvalue, errorType);
        errVec.push_back(TypeError{isaP.location, TypeMismatch{isaP.ty, *ty}});
    }
}

void TypeChecker::resolve(const TypeGuardPredicate& typeguardP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense)
{
    // Rewrite the predicate 'type(foo) == "vector"' to be 'typeof(foo) == "Vector3"'. They're exactly identical.
    // This allows us to avoid writing in edge cases.
    if (!typeguardP.isTypeof && typeguardP.kind == "vector")
        return resolve(TypeGuardPredicate{std::move(typeguardP.lvalue), typeguardP.location, "Vector3", true}, errVec, refis, scope, sense);

    std::optional<TypeId> ty = resolveLValue(refis, scope, typeguardP.lvalue);
    if (!ty)
        return;

    // In certain cases, the value may actually be nil, but Luau doesn't know about it. So we whitelist this.
    if (sense && typeguardP.kind == "nil")
    {
        addRefinement(refis, typeguardP.lvalue, nilType);
        return;
    }

    using ConditionFunc = bool(TypeId);
    using SenseToTypeIdPredicate = std::function<TypeIdPredicate(bool)>;
    auto mkFilter = [](ConditionFunc f, std::optional<TypeId> other = std::nullopt) -> SenseToTypeIdPredicate {
        return [f, other](bool sense) -> TypeIdPredicate {
            return [f, other, sense](TypeId ty) -> std::optional<TypeId> {
                if (f(ty) == sense)
                    return ty;

                if (isUndecidable(ty))
                    return other.value_or(ty);

                return std::nullopt;
            };
        };
    };

    // Note: "vector" never happens here at this point, so we don't have to write something for it.
    // clang-format off
    static const std::unordered_map<std::string, SenseToTypeIdPredicate> primitives{
        // Trivial primitives.
        {"nil", mkFilter(isNil, nilType)}, // This can still happen when sense is false!
        {"string", mkFilter(isString, stringType)},
        {"number", mkFilter(isNumber, numberType)},
        {"boolean", mkFilter(isBoolean, booleanType)},
        {"thread", mkFilter(isThread, threadType)},

        // Non-trivial primitives.
        {"table", mkFilter([](TypeId ty) -> bool { return isTableIntersection(ty) || get<TableTypeVar>(ty) || get<MetatableTypeVar>(ty); })},
        {"function", mkFilter([](TypeId ty) -> bool { return isOverloadedFunction(ty) || get<FunctionTypeVar>(ty); })},

        // For now, we don't really care about being accurate with userdata if the typeguard was using typeof.
        {"userdata", mkFilter([](TypeId ty) -> bool { return get<ClassTypeVar>(ty); })},
    };
    // clang-format on

    if (auto it = primitives.find(typeguardP.kind); it != primitives.end())
    {
        if (std::optional<TypeId> result = filterMap(*ty, it->second(sense)))
            addRefinement(refis, typeguardP.lvalue, *result);
        else
        {
            addRefinement(refis, typeguardP.lvalue, errorType);
            if (sense)
                errVec.push_back(
                    TypeError{typeguardP.location, GenericError{"Type '" + toString(*ty) + "' has no overlap with '" + typeguardP.kind + "'"}});
        }

        return;
    }

    auto fail = [&](const TypeErrorData& err) {
        errVec.push_back(TypeError{typeguardP.location, err});
        addRefinement(refis, typeguardP.lvalue, errorType);
    };

    if (!typeguardP.isTypeof)
        return fail(UnknownSymbol{typeguardP.kind, UnknownSymbol::Type});

    auto typeFun = globalScope->lookupType(typeguardP.kind);
    if (!typeFun || !typeFun->typeParams.empty())
        return fail(UnknownSymbol{typeguardP.kind, UnknownSymbol::Type});

    TypeId type = follow(typeFun->type);

    // We're only interested in the root class of any classes.
    if (auto ctv = get<ClassTypeVar>(type); !ctv || ctv->parent)
        return fail(UnknownSymbol{typeguardP.kind, UnknownSymbol::Type});

    // This probably hints at breaking out type filtering functions from the predicate solver so that typeof is not tightly coupled with IsA.
    // Until then, we rewrite this to be the same as using IsA.
    return resolve(IsAPredicate{std::move(typeguardP.lvalue), typeguardP.location, type}, errVec, refis, scope, sense);
}

void TypeChecker::DEPRECATED_resolve(const TypeGuardPredicate& typeguardP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense)
{
    if (!sense)
        return;

    static std::vector<std::string> primitives{
        "string", "number", "boolean", "nil", "thread",
        "table",    // no op. Requires special handling.
        "function", // no op. Requires special handling.
        "userdata", // no op. Requires special handling.
    };

    if (auto typeFun = globalScope->lookupType(typeguardP.kind); typeFun && typeFun->typeParams.empty())
    {
        if (auto it = std::find(primitives.begin(), primitives.end(), typeguardP.kind); it != primitives.end())
            addRefinement(refis, typeguardP.lvalue, typeFun->type);
        else if (typeguardP.isTypeof)
            addRefinement(refis, typeguardP.lvalue, typeFun->type);
    }
}

void TypeChecker::resolve(const EqPredicate& eqP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense)
{
    // This refinement will require success typing to do everything correctly. For now, we can get most of the way there.

    auto options = [](TypeId ty) -> std::vector<TypeId> {
        if (auto utv = get<UnionTypeVar>(follow(ty)))
            return std::vector<TypeId>(begin(utv), end(utv));
        return {ty};
    };

    if (FFlag::LuauWeakEqConstraint)
    {
        if (!sense && isNil(eqP.type))
            resolve(TruthyPredicate{std::move(eqP.lvalue), eqP.location}, errVec, refis, scope, true, /* fromOr= */ false);

        return;
    }

    std::optional<TypeId> ty = resolveLValue(refis, scope, eqP.lvalue);
    if (!ty)
        return;

    std::vector<TypeId> lhs = options(*ty);
    std::vector<TypeId> rhs = options(eqP.type);

    if (sense && std::any_of(lhs.begin(), lhs.end(), isUndecidable))
    {
        addRefinement(refis, eqP.lvalue, eqP.type);
        return;
    }
    else if (sense && std::any_of(rhs.begin(), rhs.end(), isUndecidable))
        return; // Optimization: the other side has unknown types, so there's probably an overlap. Refining is no-op here.

    std::unordered_set<TypeId> set;
    for (TypeId left : lhs)
    {
        for (TypeId right : rhs)
        {
            // When singleton types arrive, `isNil` here probably should be replaced with `isLiteral`.
            if (canUnify(left, right, eqP.location).empty() == sense || (!sense && !isNil(left)))
                set.insert(left);
        }
    }

    if (set.empty())
        return;

    std::vector<TypeId> viable(set.begin(), set.end());
    TypeId result = viable.size() == 1 ? viable[0] : addType(UnionTypeVar{std::move(viable)});
    addRefinement(refis, eqP.lvalue, result);
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
        expectedPack->head.push_back(FFlag::LuauRankNTypes ? freshType(scope) : DEPRECATED_freshType(scope, true));

    unify(expectedTypePack, tp, location);

    for (TypeId& tp : expectedPack->head)
        tp = follow(tp);

    return expectedPack->head;
}

std::vector<std::pair<Location, ScopePtr>> TypeChecker::getScopes() const
{
    return currentModule->scopes;
}

Scope::Scope(TypePackId returnType)
    : parent(nullptr)
    , returnType(returnType)
    , level(TypeLevel())
{
}

Scope::Scope(const ScopePtr& parent, int subLevel)
    : parent(parent)
    , returnType(parent->returnType)
    , level(parent->level.incr())
{
    level.subLevel = subLevel;
}

std::optional<TypeId> Scope::lookup(const Symbol& name)
{
    Scope* scope = this;

    while (scope)
    {
        auto it = scope->bindings.find(name);
        if (it != scope->bindings.end())
            return it->second.typeId;

        scope = scope->parent.get();
    }

    return std::nullopt;
}

std::optional<TypeFun> Scope::lookupType(const Name& name)
{
    const Scope* scope = this;
    while (true)
    {
        auto it = scope->exportedTypeBindings.find(name);
        if (it != scope->exportedTypeBindings.end())
            return it->second;

        it = scope->privateTypeBindings.find(name);
        if (it != scope->privateTypeBindings.end())
            return it->second;

        if (scope->parent)
            scope = scope->parent.get();
        else
            return std::nullopt;
    }
}

std::optional<TypeFun> Scope::lookupImportedType(const Name& moduleAlias, const Name& name)
{
    const Scope* scope = this;
    while (scope)
    {
        auto it = scope->importedTypeBindings.find(moduleAlias);
        if (it == scope->importedTypeBindings.end())
        {
            scope = scope->parent.get();
            continue;
        }

        auto it2 = it->second.find(name);
        if (it2 == it->second.end())
        {
            scope = scope->parent.get();
            continue;
        }

        return it2->second;
    }

    return std::nullopt;
}

std::optional<TypePackId> Scope::lookupPack(const Name& name)
{
    const Scope* scope = this;
    while (true)
    {
        auto it = scope->privateTypePackBindings.find(name);
        if (it != scope->privateTypePackBindings.end())
            return it->second;

        if (scope->parent)
            scope = scope->parent.get();
        else
            return std::nullopt;
    }
}

std::optional<Binding> Scope::linearSearchForBinding(const std::string& name, bool traverseScopeChain)
{
    Scope* scope = this;

    while (scope)
    {
        for (const auto& [n, binding] : scope->bindings)
        {
            if (n.local && n.local->name == name.c_str())
                return binding;
            else if (n.global.value && n.global == name.c_str())
                return binding;
        }

        scope = scope->parent.get();

        if (!traverseScopeChain)
            break;
    }

    return std::nullopt;
}

} // namespace Luau
