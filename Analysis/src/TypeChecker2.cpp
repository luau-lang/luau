// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeChecker2.h"

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Common.h"
#include "Luau/DcrLogger.h"
#include "Luau/DenseHash.h"
#include "Luau/Error.h"
#include "Luau/InsertionOrderedMap.h"
#include "Luau/Instantiation.h"
#include "Luau/Metamethods.h"
#include "Luau/Normalize.h"
#include "Luau/OverloadResolution.h"
#include "Luau/Subtyping.h"
#include "Luau/TimeTrace.h"
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeFunctionReductionGuesser.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypePack.h"
#include "Luau/TypePath.h"
#include "Luau/TypeUtils.h"
#include "Luau/TypeOrPack.h"
#include "Luau/VisitType.h"

#include <algorithm>
#include <iostream>
#include <ostream>

LUAU_FASTFLAG(DebugLuauMagicTypes)

namespace Luau
{

// TypeInfer.h
// TODO move these
using PrintLineProc = void (*)(const std::string&);
extern PrintLineProc luauPrintLine;


/* Push a scope onto the end of a stack for the lifetime of the StackPusher instance.
 * TypeChecker2 uses this to maintain knowledge about which scope encloses every
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

template<typename T>
bool areEquivalent(const T& a, const T& b)
{
    if (a.function != b.function)
        return false;

    if (a.typeArguments.size() != b.typeArguments.size() || a.packArguments.size() != b.packArguments.size())
        return false;

    for (size_t i = 0; i < a.typeArguments.size(); ++i)
    {
        if (follow(a.typeArguments[i]) != follow(b.typeArguments[i]))
            return false;
    }

    for (size_t i = 0; i < a.packArguments.size(); ++i)
    {
        if (follow(a.packArguments[i]) != follow(b.packArguments[i]))
            return false;
    }

    return true;
}

struct TypeFunctionFinder : TypeOnceVisitor
{
    DenseHashSet<TypeId> mentionedFunctions{nullptr};
    DenseHashSet<TypePackId> mentionedFunctionPacks{nullptr};

    bool visit(TypeId ty, const TypeFunctionInstanceType&) override
    {
        mentionedFunctions.insert(ty);
        return true;
    }

    bool visit(TypePackId tp, const TypeFunctionInstanceTypePack&) override
    {
        mentionedFunctionPacks.insert(tp);
        return true;
    }
};

struct InternalTypeFunctionFinder : TypeOnceVisitor
{
    DenseHashSet<TypeId> internalFunctions{nullptr};
    DenseHashSet<TypePackId> internalPackFunctions{nullptr};
    DenseHashSet<TypeId> mentionedFunctions{nullptr};
    DenseHashSet<TypePackId> mentionedFunctionPacks{nullptr};

    InternalTypeFunctionFinder(std::vector<TypeId>& declStack)
    {
        TypeFunctionFinder f;
        for (TypeId fn : declStack)
            f.traverse(fn);

        mentionedFunctions = std::move(f.mentionedFunctions);
        mentionedFunctionPacks = std::move(f.mentionedFunctionPacks);
    }

    bool visit(TypeId ty, const TypeFunctionInstanceType& tfit) override
    {
        bool hasGeneric = false;

        for (TypeId p : tfit.typeArguments)
        {
            if (get<GenericType>(follow(p)))
            {
                hasGeneric = true;
                break;
            }
        }

        for (TypePackId p : tfit.packArguments)
        {
            if (get<GenericTypePack>(follow(p)))
            {
                hasGeneric = true;
                break;
            }
        }

        if (hasGeneric)
        {
            for (TypeId mentioned : mentionedFunctions)
            {
                const TypeFunctionInstanceType* mentionedTfit = get<TypeFunctionInstanceType>(mentioned);
                LUAU_ASSERT(mentionedTfit);
                if (areEquivalent(tfit, *mentionedTfit))
                {
                    return true;
                }
            }

            internalFunctions.insert(ty);
        }

        return true;
    }

    bool visit(TypePackId tp, const TypeFunctionInstanceTypePack& tfitp) override
    {
        bool hasGeneric = false;

        for (TypeId p : tfitp.typeArguments)
        {
            if (get<GenericType>(follow(p)))
            {
                hasGeneric = true;
                break;
            }
        }

        for (TypePackId p : tfitp.packArguments)
        {
            if (get<GenericTypePack>(follow(p)))
            {
                hasGeneric = true;
                break;
            }
        }

        if (hasGeneric)
        {
            for (TypePackId mentioned : mentionedFunctionPacks)
            {
                const TypeFunctionInstanceTypePack* mentionedTfitp = get<TypeFunctionInstanceTypePack>(mentioned);
                LUAU_ASSERT(mentionedTfitp);
                if (areEquivalent(tfitp, *mentionedTfitp))
                {
                    return true;
                }
            }

            internalPackFunctions.insert(tp);
        }

        return true;
    }
};

struct TypeChecker2
{
    NotNull<BuiltinTypes> builtinTypes;
    DcrLogger* logger;
    const NotNull<TypeCheckLimits> limits;
    const NotNull<InternalErrorReporter> ice;
    const SourceModule* sourceModule;
    Module* module;

    TypeContext typeContext = TypeContext::Default;
    std::vector<NotNull<Scope>> stack;
    std::vector<TypeId> functionDeclStack;

    DenseHashSet<TypeId> seenTypeFunctionInstances{nullptr};

    Normalizer normalizer;
    Subtyping _subtyping;
    NotNull<Subtyping> subtyping;

    TypeChecker2(NotNull<BuiltinTypes> builtinTypes, NotNull<UnifierSharedState> unifierState, NotNull<TypeCheckLimits> limits, DcrLogger* logger,
        const SourceModule* sourceModule, Module* module)
        : builtinTypes(builtinTypes)
        , logger(logger)
        , limits(limits)
        , ice(unifierState->iceHandler)
        , sourceModule(sourceModule)
        , module(module)
        , normalizer{&module->internalTypes, builtinTypes, unifierState, /* cacheInhabitance */ true}
        , _subtyping{builtinTypes, NotNull{&module->internalTypes}, NotNull{&normalizer}, NotNull{unifierState->iceHandler},
              NotNull{module->getModuleScope().get()}}
        , subtyping(&_subtyping)
    {
    }

    static bool allowsNoReturnValues(const TypePackId tp)
    {
        for (TypeId ty : tp)
        {
            if (!get<ErrorType>(follow(ty)))
                return false;
        }

        return true;
    }

    static Location getEndLocation(const AstExprFunction* function)
    {
        Location loc = function->location;
        if (loc.begin.line != loc.end.line)
        {
            Position begin = loc.end;
            begin.column = std::max(0u, begin.column - 3);
            loc = Location(begin, 3);
        }

        return loc;
    }

    bool isErrorCall(const AstExprCall* call)
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

        if (node->is<AstStatBreak>())
            return true;

        if (AstStatIf* stat = node->as<AstStatIf>())
        {
            if (hasBreak(stat->thenbody))
                return true;

            if (stat->elsebody && hasBreak(stat->elsebody))
                return true;

            return false;
        }

        return false;
    }

    // returns the last statement before the block implicitly exits, or nullptr if the block does not implicitly exit
    // i.e. returns nullptr if the block returns properly or never returns
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

        if (const AstStatIf* stat = node->as<AstStatIf>())
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
                return stat;
        }

        if (node->is<AstStatReturn>())
            return nullptr;

        if (const AstStatExpr* stat = node->as<AstStatExpr>())
        {
            if (AstExprCall* call = stat->expr->as<AstExprCall>(); call && isErrorCall(call))
                return nullptr;

            return stat;
        }

        if (const AstStatWhile* stat = node->as<AstStatWhile>())
        {
            if (AstExprConstantBool* expr = stat->condition->as<AstExprConstantBool>())
            {
                if (expr->value && !hasBreak(stat->body))
                    return nullptr;
            }

            return node;
        }

        if (const AstStatRepeat* stat = node->as<AstStatRepeat>())
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

        return node;
    }

    std::optional<StackPusher> pushStack(AstNode* node)
    {
        if (Scope** scope = module->astScopes.find(node))
            return StackPusher{stack, *scope};
        else
            return std::nullopt;
    }

    void checkForInternalTypeFunction(TypeId ty, Location location)
    {
        InternalTypeFunctionFinder finder(functionDeclStack);
        finder.traverse(ty);

        for (TypeId internal : finder.internalFunctions)
            reportError(WhereClauseNeeded{internal}, location);

        for (TypePackId internal : finder.internalPackFunctions)
            reportError(PackWhereClauseNeeded{internal}, location);
    }

    TypeId checkForTypeFunctionInhabitance(TypeId instance, Location location)
    {
        if (seenTypeFunctionInstances.find(instance))
            return instance;
        seenTypeFunctionInstances.insert(instance);

        ErrorVec errors = reduceTypeFunctions(instance, location,
            TypeFunctionContext{NotNull{&module->internalTypes}, builtinTypes, stack.back(), NotNull{&normalizer}, ice, limits}, true)
                              .errors;
        if (!isErrorSuppressing(location, instance))
            reportErrors(std::move(errors));
        return instance;
    }

    TypePackId lookupPack(AstExpr* expr)
    {
        // If a type isn't in the type graph, it probably means that a recursion limit was exceeded.
        // We'll just return anyType in these cases.  Typechecking against any is very fast and this
        // allows us not to think about this very much in the actual typechecking logic.
        TypePackId* tp = module->astTypePacks.find(expr);
        if (tp)
            return follow(*tp);
        else
            return builtinTypes->anyTypePack;
    }

    TypeId lookupType(AstExpr* expr)
    {
        // If a type isn't in the type graph, it probably means that a recursion limit was exceeded.
        // We'll just return anyType in these cases.  Typechecking against any is very fast and this
        // allows us not to think about this very much in the actual typechecking logic.
        TypeId* ty = module->astTypes.find(expr);
        if (ty)
            return checkForTypeFunctionInhabitance(follow(*ty), expr->location);

        TypePackId* tp = module->astTypePacks.find(expr);
        if (tp)
            return checkForTypeFunctionInhabitance(flattenPack(*tp), expr->location);

        return builtinTypes->anyType;
    }

    TypeId lookupAnnotation(AstType* annotation)
    {
        if (FFlag::DebugLuauMagicTypes)
        {
            if (auto ref = annotation->as<AstTypeReference>(); ref && ref->name == "_luau_print" && ref->parameters.size > 0)
            {
                if (auto ann = ref->parameters.data[0].type)
                {
                    TypeId argTy = lookupAnnotation(ref->parameters.data[0].type);
                    luauPrintLine(format(
                        "_luau_print (%d, %d): %s\n", annotation->location.begin.line, annotation->location.begin.column, toString(argTy).c_str()));
                    return follow(argTy);
                }
            }
        }

        TypeId* ty = module->astResolvedTypes.find(annotation);
        LUAU_ASSERT(ty);
        return checkForTypeFunctionInhabitance(follow(*ty), annotation->location);
    }

    std::optional<TypePackId> lookupPackAnnotation(AstTypePack* annotation)
    {
        TypePackId* tp = module->astResolvedTypePacks.find(annotation);
        if (tp != nullptr)
            return {follow(*tp)};
        return {};
    }

    TypeId lookupExpectedType(AstExpr* expr)
    {
        if (TypeId* ty = module->astExpectedTypes.find(expr))
            return follow(*ty);

        return builtinTypes->anyType;
    }

    TypePackId lookupExpectedPack(AstExpr* expr, TypeArena& arena)
    {
        if (TypeId* ty = module->astExpectedTypes.find(expr))
            return arena.addTypePack(TypePack{{follow(*ty)}, std::nullopt});

        return builtinTypes->anyTypePack;
    }

    TypePackId reconstructPack(AstArray<AstExpr*> exprs, TypeArena& arena)
    {
        if (exprs.size == 0)
            return arena.addTypePack(TypePack{{}, std::nullopt});

        std::vector<TypeId> head;

        for (size_t i = 0; i < exprs.size - 1; ++i)
        {
            head.push_back(lookupType(exprs.data[i]));
        }

        TypePackId tail = lookupPack(exprs.data[exprs.size - 1]);
        return arena.addTypePack(TypePack{head, tail});
    }

    Scope* findInnermostScope(Location location)
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

    void visit(AstStat* stat)
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
            LUAU_ASSERT(!"TypeChecker2 encountered an unknown node type");
    }

    void visit(AstStatBlock* block)
    {
        auto StackPusher = pushStack(block);

        for (AstStat* statement : block->body)
            visit(statement);
    }

    void visit(AstStatIf* ifStatement)
    {
        {
            InConditionalContext flipper{&typeContext};
            visit(ifStatement->condition, ValueContext::RValue);
        }

        visit(ifStatement->thenbody);
        if (ifStatement->elsebody)
            visit(ifStatement->elsebody);
    }

    void visit(AstStatWhile* whileStatement)
    {
        visit(whileStatement->condition, ValueContext::RValue);
        visit(whileStatement->body);
    }

    void visit(AstStatRepeat* repeatStatement)
    {
        visit(repeatStatement->body);
        visit(repeatStatement->condition, ValueContext::RValue);
    }

    void visit(AstStatBreak*) {}

    void visit(AstStatContinue*) {}

    void visit(AstStatReturn* ret)
    {
        Scope* scope = findInnermostScope(ret->location);
        TypePackId expectedRetType = scope->returnType;

        TypeArena* arena = &module->internalTypes;
        TypePackId actualRetType = reconstructPack(ret->list, *arena);

        testIsSubtype(actualRetType, expectedRetType, ret->location);

        for (AstExpr* expr : ret->list)
            visit(expr, ValueContext::RValue);
    }

    void visit(AstStatExpr* expr)
    {
        visit(expr->expr, ValueContext::RValue);
    }

    void visit(AstStatLocal* local)
    {
        size_t count = std::max(local->values.size, local->vars.size);
        for (size_t i = 0; i < count; ++i)
        {
            AstExpr* value = i < local->values.size ? local->values.data[i] : nullptr;
            const bool isPack = value && (value->is<AstExprCall>() || value->is<AstExprVarargs>());

            if (value)
                visit(value, ValueContext::RValue);

            if (i != local->values.size - 1 || !isPack)
            {
                AstLocal* var = i < local->vars.size ? local->vars.data[i] : nullptr;

                if (var && var->annotation)
                {
                    TypeId annotationType = lookupAnnotation(var->annotation);
                    TypeId valueType = value ? lookupType(value) : nullptr;
                    if (valueType)
                        testIsSubtype(valueType, annotationType, value->location);

                    visit(var->annotation);
                }
            }
            else if (value)
            {
                TypePackId valuePack = lookupPack(value);
                TypePack valueTypes;
                if (i < local->vars.size)
                    valueTypes = extendTypePack(module->internalTypes, builtinTypes, valuePack, local->vars.size - i);

                Location errorLocation;
                for (size_t j = i; j < local->vars.size; ++j)
                {
                    if (j - i >= valueTypes.head.size())
                    {
                        errorLocation = local->vars.data[j]->location;
                        break;
                    }

                    AstLocal* var = local->vars.data[j];
                    if (var->annotation)
                    {
                        TypeId varType = lookupAnnotation(var->annotation);
                        testIsSubtype(valueTypes.head[j - i], varType, value->location);

                        visit(var->annotation);
                    }
                }

                if (valueTypes.head.size() < local->vars.size - i)
                {
                    reportError(
                        CountMismatch{
                            // We subtract 1 here because the final AST
                            // expression is not worth one value.  It is worth 0
                            // or more depending on valueTypes.head
                            local->values.size - 1 + valueTypes.head.size(),
                            std::nullopt,
                            local->vars.size,
                            local->values.data[local->values.size - 1]->is<AstExprCall>() ? CountMismatch::FunctionResult
                                                                                          : CountMismatch::ExprListResult,
                        },
                        errorLocation);
                }
            }
        }
    }

    void visit(AstStatFor* forStatement)
    {
        if (forStatement->var->annotation)
        {
            visit(forStatement->var->annotation);

            TypeId annotatedType = lookupAnnotation(forStatement->var->annotation);
            testIsSubtype(builtinTypes->numberType, annotatedType, forStatement->var->location);
        }

        auto checkNumber = [this](AstExpr* expr) {
            if (!expr)
                return;

            visit(expr, ValueContext::RValue);
            testIsSubtype(lookupType(expr), builtinTypes->numberType, expr->location);
        };

        checkNumber(forStatement->from);
        checkNumber(forStatement->to);
        checkNumber(forStatement->step);

        visit(forStatement->body);
    }

    void visit(AstStatForIn* forInStatement)
    {
        for (AstLocal* local : forInStatement->vars)
        {
            if (local->annotation)
                visit(local->annotation);
        }

        for (AstExpr* expr : forInStatement->values)
            visit(expr, ValueContext::RValue);

        visit(forInStatement->body);

        // Rule out crazy stuff.  Maybe possible if the file is not syntactically valid.
        if (!forInStatement->vars.size || !forInStatement->values.size)
            return;

        NotNull<Scope> scope = stack.back();
        TypeArena& arena = module->internalTypes;

        std::vector<TypeId> variableTypes;
        for (AstLocal* var : forInStatement->vars)
        {
            std::optional<TypeId> ty = scope->lookup(var);
            LUAU_ASSERT(ty);
            variableTypes.emplace_back(*ty);
        }

        AstExpr* firstValue = forInStatement->values.data[0];

        // we need to build up a typepack for the iterators/values portion of the for-in statement.
        std::vector<TypeId> valueTypes;
        std::optional<TypePackId> iteratorTail;

        // since the first value may be the only iterator (e.g. if it is a call), we want to
        // look to see if it has a resulting typepack as our iterators.
        TypePackId* retPack = module->astTypePacks.find(firstValue);
        if (retPack)
        {
            auto [head, tail] = flatten(*retPack);
            valueTypes = head;
            iteratorTail = tail;
        }
        else
        {
            valueTypes.emplace_back(lookupType(firstValue));
        }

        // if the initial and expected types from the iterator unified during constraint solving,
        // we'll have a resolved type to use here, but we'll only use it if either the iterator is
        // directly present in the for-in statement or if we have an iterator state constraining us
        TypeId* resolvedTy = module->astForInNextTypes.find(firstValue);
        if (resolvedTy && (!retPack || valueTypes.size() > 1))
            valueTypes[0] = *resolvedTy;

        for (size_t i = 1; i < forInStatement->values.size - 1; ++i)
        {
            valueTypes.emplace_back(lookupType(forInStatement->values.data[i]));
        }

        // if we had more than one value, the tail from the first value is no longer appropriate to use.
        if (forInStatement->values.size > 1)
        {
            auto [head, tail] = flatten(lookupPack(forInStatement->values.data[forInStatement->values.size - 1]));
            valueTypes.insert(valueTypes.end(), head.begin(), head.end());
            iteratorTail = tail;
        }

        // and now we can put everything together to get the actual typepack of the iterators.
        TypePackId iteratorPack = arena.addTypePack(valueTypes, iteratorTail);

        // ... and then expand it out to 3 values (if possible)
        TypePack iteratorTypes = extendTypePack(arena, builtinTypes, iteratorPack, 3);
        if (iteratorTypes.head.empty())
        {
            reportError(GenericError{"for..in loops require at least one value to iterate over.  Got zero"}, getLocation(forInStatement->values));
            return;
        }
        TypeId iteratorTy = follow(iteratorTypes.head[0]);

        auto checkFunction = [this, &arena, &forInStatement, &variableTypes](const FunctionType* iterFtv, std::vector<TypeId> iterTys, bool isMm) {
            if (iterTys.size() < 1 || iterTys.size() > 3)
            {
                if (isMm)
                    reportError(GenericError{"__iter metamethod must return (next[, table[, state]])"}, getLocation(forInStatement->values));
                else
                    reportError(GenericError{"for..in loops must be passed (next[, table[, state]])"}, getLocation(forInStatement->values));

                return;
            }

            // It is okay if there aren't enough iterators, but the iteratee must provide enough.
            TypePack expectedVariableTypes = extendTypePack(arena, builtinTypes, iterFtv->retTypes, variableTypes.size());
            if (expectedVariableTypes.head.size() < variableTypes.size())
            {
                if (isMm)
                    reportError(
                        GenericError{"__iter metamethod's next() function does not return enough values"}, getLocation(forInStatement->values));
                else
                    reportError(GenericError{"next() does not return enough values"}, forInStatement->values.data[0]->location);
            }

            for (size_t i = 0; i < std::min(expectedVariableTypes.head.size(), variableTypes.size()); ++i)
                testIsSubtype(variableTypes[i], expectedVariableTypes.head[i], forInStatement->vars.data[i]->location);

            // nextFn is going to be invoked with (arrayTy, startIndexTy)

            // It will be passed two arguments on every iteration save the
            // first.

            // It may be invoked with 0 or 1 argument on the first iteration.
            // This depends on the types in iterateePack and therefore
            // iteratorTypes.

            // If the iteratee is an error type, then we can't really say anything else about iteration over it.
            // After all, it _could've_ been a table.
            if (get<ErrorType>(follow(flattenPack(iterFtv->argTypes))))
                return;

            // If iteratorTypes is too short to be a valid call to nextFn, we have to report a count mismatch error.
            // If 2 is too short to be a valid call to nextFn, we have to report a count mismatch error.
            // If 2 is too long to be a valid call to nextFn, we have to report a count mismatch error.
            auto [minCount, maxCount] = getParameterExtents(TxnLog::empty(), iterFtv->argTypes, /*includeHiddenVariadics*/ true);

            TypePack flattenedArgTypes = extendTypePack(arena, builtinTypes, iterFtv->argTypes, 2);
            size_t firstIterationArgCount = iterTys.empty() ? 0 : iterTys.size() - 1;
            size_t actualArgCount = expectedVariableTypes.head.size();
            if (firstIterationArgCount < minCount)
            {
                if (isMm)
                    reportError(GenericError{"__iter metamethod must return (next[, table[, state]])"}, getLocation(forInStatement->values));
                else
                    reportError(CountMismatch{2, std::nullopt, firstIterationArgCount, CountMismatch::Arg}, forInStatement->values.data[0]->location);
            }

            else if (actualArgCount < minCount)
            {
                if (isMm)
                    reportError(GenericError{"__iter metamethod must return (next[, table[, state]])"}, getLocation(forInStatement->values));
                else
                    reportError(CountMismatch{2, std::nullopt, firstIterationArgCount, CountMismatch::Arg}, forInStatement->values.data[0]->location);
            }


            if (iterTys.size() >= 2 && flattenedArgTypes.head.size() > 0)
            {
                size_t valueIndex = forInStatement->values.size > 1 ? 1 : 0;
                testIsSubtype(iterTys[1], flattenedArgTypes.head[0], forInStatement->values.data[valueIndex]->location);
            }

            if (iterTys.size() == 3 && flattenedArgTypes.head.size() > 1)
            {
                size_t valueIndex = forInStatement->values.size > 2 ? 2 : 0;
                testIsSubtype(iterTys[2], flattenedArgTypes.head[1], forInStatement->values.data[valueIndex]->location);
            }
        };

        std::shared_ptr<const NormalizedType> iteratorNorm = normalizer.normalize(iteratorTy);

        if (!iteratorNorm)
            reportError(NormalizationTooComplex{}, firstValue->location);

        /*
         * If the first iterator argument is a function
         *  * There must be 1 to 3 iterator arguments.  Name them (nextTy,
         *    arrayTy, startIndexTy)
         *  * The return type of nextTy() must correspond to the variables'
         *    types and counts.  HOWEVER the first iterator will never be nil.
         *  * The first return value of nextTy must be compatible with
         *    startIndexTy.
         *  * The first argument to nextTy() must be compatible with arrayTy if
         *    present.  nil if not.
         *  * The second argument to nextTy() must be compatible with
         *    startIndexTy if it is present.  Else, it must be compatible with
         *    nil.
         *  * nextTy() must be callable with only 2 arguments.
         */
        if (const FunctionType* nextFn = get<FunctionType>(iteratorTy))
        {
            checkFunction(nextFn, iteratorTypes.head, false);
        }
        else if (const TableType* ttv = get<TableType>(iteratorTy))
        {
            if ((forInStatement->vars.size == 1 || forInStatement->vars.size == 2) && ttv->indexer)
            {
                testIsSubtype(variableTypes[0], ttv->indexer->indexType, forInStatement->vars.data[0]->location);
                if (variableTypes.size() == 2)
                    testIsSubtype(variableTypes[1], ttv->indexer->indexResultType, forInStatement->vars.data[1]->location);
            }
            else
                reportError(GenericError{"Cannot iterate over a table without indexer"}, forInStatement->values.data[0]->location);
        }
        else if (get<AnyType>(iteratorTy) || get<ErrorType>(iteratorTy) || get<NeverType>(iteratorTy))
        {
            // nothing
        }
        else if (isOptional(iteratorTy) && !(iteratorNorm && iteratorNorm->shouldSuppressErrors()))
        {
            reportError(OptionalValueAccess{iteratorTy}, forInStatement->values.data[0]->location);
        }
        else if (std::optional<TypeId> iterMmTy =
                     findMetatableEntry(builtinTypes, module->errors, iteratorTy, "__iter", forInStatement->values.data[0]->location))
        {
            Instantiation instantiation{TxnLog::empty(), &arena, builtinTypes, TypeLevel{}, scope};

            if (std::optional<TypeId> instantiatedIterMmTy = instantiate(builtinTypes, NotNull{&arena}, limits, scope, *iterMmTy))
            {
                if (const FunctionType* iterMmFtv = get<FunctionType>(*instantiatedIterMmTy))
                {
                    TypePackId argPack = arena.addTypePack({iteratorTy});
                    testIsSubtype(argPack, iterMmFtv->argTypes, forInStatement->values.data[0]->location);

                    TypePack mmIteratorTypes = extendTypePack(arena, builtinTypes, iterMmFtv->retTypes, 3);

                    if (mmIteratorTypes.head.size() == 0)
                    {
                        reportError(GenericError{"__iter must return at least one value"}, forInStatement->values.data[0]->location);
                        return;
                    }

                    TypeId nextFn = follow(mmIteratorTypes.head[0]);

                    if (std::optional<TypeId> instantiatedNextFn = instantiation.substitute(nextFn))
                    {
                        std::vector<TypeId> instantiatedIteratorTypes = mmIteratorTypes.head;
                        instantiatedIteratorTypes[0] = *instantiatedNextFn;

                        if (const FunctionType* nextFtv = get<FunctionType>(*instantiatedNextFn))
                        {
                            checkFunction(nextFtv, instantiatedIteratorTypes, true);
                        }
                        else if (!isErrorSuppressing(forInStatement->values.data[0]->location, *instantiatedNextFn))
                        {
                            reportError(CannotCallNonFunction{*instantiatedNextFn}, forInStatement->values.data[0]->location);
                        }
                    }
                    else
                    {
                        reportError(UnificationTooComplex{}, forInStatement->values.data[0]->location);
                    }
                }
                else if (!isErrorSuppressing(forInStatement->values.data[0]->location, *iterMmTy))
                {
                    // TODO: This will not tell the user that this is because the
                    // metamethod isn't callable. This is not ideal, and we should
                    // improve this error message.

                    // TODO: This will also not handle intersections of functions or
                    // callable tables (which are supported by the runtime).
                    reportError(CannotCallNonFunction{*iterMmTy}, forInStatement->values.data[0]->location);
                }
            }
            else
            {
                reportError(UnificationTooComplex{}, forInStatement->values.data[0]->location);
            }
        }
        else if (iteratorNorm && iteratorNorm->hasTables())
        {
            // Ok. All tables can be iterated.
        }
        else if (!iteratorNorm || !iteratorNorm->shouldSuppressErrors())
        {
            reportError(CannotCallNonFunction{iteratorTy}, forInStatement->values.data[0]->location);
        }
    }

    std::optional<TypeId> getBindingType(AstExpr* expr)
    {
        if (auto localExpr = expr->as<AstExprLocal>())
        {
            Scope* s = stack.back();
            return s->lookup(localExpr->local);
        }
        else if (auto globalExpr = expr->as<AstExprGlobal>())
        {
            Scope* s = stack.back();
            return s->lookup(globalExpr->name);
        }
        else
            return std::nullopt;
    }

    // this should only be called if the type of `lhs` is `never`.
    void reportErrorsFromAssigningToNever(AstExpr* lhs, TypeId rhsType)
    {

        if (auto indexName = lhs->as<AstExprIndexName>())
        {
            TypeId indexedType = lookupType(indexName->expr);

            // if it's already never, I don't think we have anything to do here.
            if (get<NeverType>(indexedType))
                return;

            std::string prop = indexName->index.value;

            std::shared_ptr<const NormalizedType> norm = normalizer.normalize(indexedType);
            if (!norm)
            {
                reportError(NormalizationTooComplex{}, lhs->location);
                return;
            }

            // if the type is error suppressing, we don't actually have any work left to do.
            if (norm->shouldSuppressErrors())
                return;

            const auto propTypes = lookupProp(norm.get(), prop, ValueContext::LValue, lhs->location, builtinTypes->stringType, module->errors);

            reportError(CannotAssignToNever{rhsType, propTypes.typesOfProp, CannotAssignToNever::Reason::PropertyNarrowed}, lhs->location);
        }
    }

    void visit(AstStatAssign* assign)
    {
        size_t count = std::min(assign->vars.size, assign->values.size);

        for (size_t i = 0; i < count; ++i)
        {
            AstExpr* lhs = assign->vars.data[i];
            visit(lhs, ValueContext::LValue);
            TypeId lhsType = lookupType(lhs);

            AstExpr* rhs = assign->values.data[i];
            visit(rhs, ValueContext::RValue);
            TypeId rhsType = lookupType(rhs);

            if (get<NeverType>(lhsType))
            {
                reportErrorsFromAssigningToNever(lhs, rhsType);
                continue;
            }

            bool ok = testIsSubtype(rhsType, lhsType, rhs->location);

            // If rhsType </: lhsType, then it's not useful to also report that rhsType </: bindingType
            if (ok)
            {
                std::optional<TypeId> bindingType = getBindingType(lhs);
                if (bindingType)
                    testIsSubtype(rhsType, *bindingType, rhs->location);
            }
        }
    }

    void visit(AstStatCompoundAssign* stat)
    {
        AstExprBinary fake{stat->location, stat->op, stat->var, stat->value};
        visit(&fake, stat);

        TypeId* resultTy = module->astCompoundAssignResultTypes.find(stat);
        LUAU_ASSERT(resultTy);
        TypeId varTy = lookupType(stat->var);

        testIsSubtype(*resultTy, varTy, stat->location);
    }

    void visit(AstStatFunction* stat)
    {
        visit(stat->name, ValueContext::LValue);
        visit(stat->func);
    }

    void visit(AstStatLocalFunction* stat)
    {
        visit(stat->func);
    }

    void visit(const AstTypeList* typeList)
    {
        for (AstType* ty : typeList->types)
            visit(ty);

        if (typeList->tailType)
            visit(typeList->tailType);
    }

    void visit(AstStatTypeAlias* stat)
    {
        visitGenerics(stat->generics, stat->genericPacks);
        visit(stat->type);
    }

    void visit(AstTypeList types)
    {
        for (AstType* type : types.types)
            visit(type);
        if (types.tailType)
            visit(types.tailType);
    }

    void visit(AstStatDeclareFunction* stat)
    {
        visitGenerics(stat->generics, stat->genericPacks);
        visit(stat->params);
        visit(stat->retTypes);
    }

    void visit(AstStatDeclareGlobal* stat)
    {
        visit(stat->type);
    }

    void visit(AstStatDeclareClass* stat)
    {
        for (const AstDeclaredClassProp& prop : stat->props)
            visit(prop.ty);
    }

    void visit(AstStatError* stat)
    {
        for (AstExpr* expr : stat->expressions)
            visit(expr, ValueContext::RValue);

        for (AstStat* s : stat->statements)
            visit(s);
    }

    void visit(AstExpr* expr, ValueContext context)
    {
        auto StackPusher = pushStack(expr);

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
            return visit(e);
        else if (auto e = expr->as<AstExprGlobal>())
            return visit(e);
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
        {
            visit(e);
            return;
        }
        else if (auto e = expr->as<AstExprTypeAssertion>())
            return visit(e);
        else if (auto e = expr->as<AstExprIfElse>())
            return visit(e);
        else if (auto e = expr->as<AstExprInterpString>())
            return visit(e);
        else if (auto e = expr->as<AstExprError>())
            return visit(e);
        else
            LUAU_ASSERT(!"TypeChecker2 encountered an unknown expression type");
    }

    void visit(AstExprGroup* expr, ValueContext context)
    {
        visit(expr->expr, context);
    }

    void visit(AstExprConstantNil* expr)
    {
#if defined(LUAU_ENABLE_ASSERT)
        TypeId actualType = lookupType(expr);
        TypeId expectedType = builtinTypes->nilType;

        SubtypingResult r = subtyping->isSubtype(actualType, expectedType);
        LUAU_ASSERT(r.isSubtype || isErrorSuppressing(expr->location, actualType));
#endif
    }

    void visit(AstExprConstantBool* expr)
    {
        // booleans use specialized inference logic for singleton types, which can lead to real type errors here.

        const TypeId bestType = expr->value ? builtinTypes->trueType : builtinTypes->falseType;
        const TypeId inferredType = lookupType(expr);

        const SubtypingResult r = subtyping->isSubtype(bestType, inferredType);
        if (!r.isSubtype && !isErrorSuppressing(expr->location, inferredType))
            reportError(TypeMismatch{inferredType, bestType}, expr->location);
    }

    void visit(AstExprConstantNumber* expr)
    {
#if defined(LUAU_ENABLE_ASSERT)
        const TypeId bestType = builtinTypes->numberType;
        const TypeId inferredType = lookupType(expr);

        const SubtypingResult r = subtyping->isSubtype(bestType, inferredType);
        LUAU_ASSERT(r.isSubtype || isErrorSuppressing(expr->location, inferredType));
#endif
    }

    void visit(AstExprConstantString* expr)
    {
        // strings use specialized inference logic for singleton types, which can lead to real type errors here.

        const TypeId bestType = module->internalTypes.addType(SingletonType{StringSingleton{std::string{expr->value.data, expr->value.size}}});
        const TypeId inferredType = lookupType(expr);

        const SubtypingResult r = subtyping->isSubtype(bestType, inferredType);
        if (!r.isSubtype && !isErrorSuppressing(expr->location, inferredType))
            reportError(TypeMismatch{inferredType, bestType}, expr->location);
    }

    void visit(AstExprLocal* expr)
    {
        // TODO!
    }

    void visit(AstExprGlobal* expr)
    {
        NotNull<Scope> scope = stack.back();
        if (!scope->lookup(expr->name))
            reportError(UnknownSymbol{expr->name.value, UnknownSymbol::Binding}, expr->location);
    }

    void visit(AstExprVarargs* expr)
    {
        // TODO!
    }

    // Note: this is intentionally separated from `visit(AstExprCall*)` for stack allocation purposes.
    void visitCall(AstExprCall* call)
    {
        TypePack args;
        std::vector<AstExpr*> argExprs;
        argExprs.reserve(call->args.size + 1);

        TypeId* originalCallTy = module->astOriginalCallTypes.find(call);
        TypeId* selectedOverloadTy = module->astOverloadResolvedTypes.find(call);
        if (!originalCallTy)
            return;

        TypeId fnTy = follow(*originalCallTy);


        if (get<AnyType>(fnTy) || get<ErrorType>(fnTy) || get<NeverType>(fnTy))
            return;
        else if (isOptional(fnTy))
        {
            switch (shouldSuppressErrors(NotNull{&normalizer}, fnTy))
            {
            case ErrorSuppression::Suppress:
                break;
            case ErrorSuppression::NormalizationFailed:
                reportError(NormalizationTooComplex{}, call->func->location);
                // fallthrough intentional
            case ErrorSuppression::DoNotSuppress:
                reportError(OptionalValueAccess{fnTy}, call->func->location);
            }
            return;
        }

        if (selectedOverloadTy)
        {
            SubtypingResult result = subtyping->isSubtype(*originalCallTy, *selectedOverloadTy);
            if (result.isSubtype)
                fnTy = follow(*selectedOverloadTy);

            if (result.normalizationTooComplex)
            {
                reportError(NormalizationTooComplex{}, call->func->location);
                return;
            }
        }

        if (call->self)
        {
            AstExprIndexName* indexExpr = call->func->as<AstExprIndexName>();
            if (!indexExpr)
                ice->ice("method call expression has no 'self'");

            args.head.push_back(lookupType(indexExpr->expr));
            argExprs.push_back(indexExpr->expr);
        }
        else if (findMetatableEntry(builtinTypes, module->errors, *originalCallTy, "__call", call->func->location))
        {
            args.head.insert(args.head.begin(), lookupType(call->func));
            argExprs.push_back(call->func);
        }

        for (size_t i = 0; i < call->args.size; ++i)
        {
            AstExpr* arg = call->args.data[i];
            argExprs.push_back(arg);
            TypeId* argTy = module->astTypes.find(arg);
            if (argTy)
                args.head.push_back(*argTy);
            else if (i == call->args.size - 1)
            {
                if (auto argTail = module->astTypePacks.find(arg))
                {
                    auto [head, tail] = flatten(*argTail);
                    args.head.insert(args.head.end(), head.begin(), head.end());
                    args.tail = tail;
                }
                else
                    args.tail = builtinTypes->anyTypePack;
            }
            else
                args.head.push_back(builtinTypes->anyType);
        }



        OverloadResolver resolver{
            builtinTypes,
            NotNull{&module->internalTypes},
            NotNull{&normalizer},
            NotNull{stack.back()},
            ice,
            limits,
            call->location,
        };
        resolver.resolve(fnTy, &args, call->func, &argExprs);

        auto norm = normalizer.normalize(fnTy);
        if (!norm)
            reportError(NormalizationTooComplex{}, call->func->location);
        auto isInhabited = normalizer.isInhabited(norm.get());
        if (isInhabited == NormalizationResult::HitLimits)
            reportError(NormalizationTooComplex{}, call->func->location);

        if (norm && norm->shouldSuppressErrors())
            return; // error suppressing function type!
        else if (!resolver.ok.empty())
            return; // We found a call that works, so this is ok.
        else if (!norm || isInhabited == NormalizationResult::False)
            return; // Ok. Calling an uninhabited type is no-op.
        else if (!resolver.nonviableOverloads.empty())
        {
            if (resolver.nonviableOverloads.size() == 1 && !isErrorSuppressing(call->func->location, resolver.nonviableOverloads.front().first))
                reportErrors(resolver.nonviableOverloads.front().second);
            else
            {
                std::string s = "None of the overloads for function that accept ";
                s += std::to_string(args.head.size());
                s += " arguments are compatible.";
                reportError(GenericError{std::move(s)}, call->location);
            }
        }
        else if (!resolver.arityMismatches.empty())
        {
            if (resolver.arityMismatches.size() == 1)
                reportErrors(resolver.arityMismatches.front().second);
            else
            {
                std::string s = "No overload for function accepts ";
                s += std::to_string(args.head.size());
                s += " arguments.";
                reportError(GenericError{std::move(s)}, call->location);
            }
        }
        else if (!resolver.nonFunctions.empty())
            reportError(CannotCallNonFunction{fnTy}, call->func->location);
        else
            LUAU_ASSERT(!"Generating the best possible error from this function call resolution was inexhaustive?");

        if (resolver.arityMismatches.size() > 1 || resolver.nonviableOverloads.size() > 1)
        {
            std::string s = "Available overloads: ";

            std::vector<TypeId> overloads;
            if (resolver.nonviableOverloads.empty())
            {
                for (const auto& [ty, p] : resolver.resolution)
                {
                    if (p.first == OverloadResolver::TypeIsNotAFunction)
                        continue;

                    overloads.push_back(ty);
                }
            }
            else
            {
                for (const auto& [ty, _] : resolver.nonviableOverloads)
                    overloads.push_back(ty);
            }

            for (size_t i = 0; i < overloads.size(); ++i)
            {
                if (i > 0)
                    s += (i == overloads.size() - 1) ? "; and " : "; ";

                s += toString(overloads[i]);
            }

            reportError(ExtraInformation{std::move(s)}, call->func->location);
        }
    }

    void visit(AstExprCall* call)
    {
        visit(call->func, ValueContext::RValue);

        for (AstExpr* arg : call->args)
            visit(arg, ValueContext::RValue);

        visitCall(call);
    }

    std::optional<TypeId> tryStripUnionFromNil(TypeId ty)
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

            return result.size() == 1 ? result[0] : module->internalTypes.addType(UnionType{std::move(result)});
        }

        return std::nullopt;
    }

    TypeId stripFromNilAndReport(TypeId ty, const Location& location)
    {
        ty = follow(ty);

        if (auto utv = get<UnionType>(ty))
        {
            if (!std::any_of(begin(utv), end(utv), isNil))
                return ty;
        }

        if (std::optional<TypeId> strippedUnion = tryStripUnionFromNil(ty))
        {
            switch (shouldSuppressErrors(NotNull{&normalizer}, ty))
            {
            case ErrorSuppression::Suppress:
                break;
            case ErrorSuppression::NormalizationFailed:
                reportError(NormalizationTooComplex{}, location);
                // fallthrough intentional
            case ErrorSuppression::DoNotSuppress:
                reportError(OptionalValueAccess{ty}, location);
            }

            return follow(*strippedUnion);
        }

        return ty;
    }

    void visitExprName(AstExpr* expr, Location location, const std::string& propName, ValueContext context, TypeId astIndexExprTy)
    {
        visit(expr, ValueContext::RValue);
        TypeId leftType = stripFromNilAndReport(lookupType(expr), location);
        checkIndexTypeFromType(leftType, propName, context, location, astIndexExprTy);
    }

    void visit(AstExprIndexName* indexName, ValueContext context)
    {
        // If we're indexing like _.foo - foo could either be a prop or a string.
        visitExprName(indexName->expr, indexName->location, indexName->index.value, context, builtinTypes->stringType);
    }

    void indexExprMetatableHelper(AstExprIndexExpr* indexExpr, const MetatableType* metaTable, TypeId exprType, TypeId indexType)
    {
        if (auto tt = get<TableType>(follow(metaTable->table)); tt && tt->indexer)
            testIsSubtype(indexType, tt->indexer->indexType, indexExpr->index->location);
        else if (auto mt = get<MetatableType>(follow(metaTable->table)))
            indexExprMetatableHelper(indexExpr, mt, exprType, indexType);
        else if (auto tmt = get<TableType>(follow(metaTable->metatable)); tmt && tmt->indexer)
            testIsSubtype(indexType, tmt->indexer->indexType, indexExpr->index->location);
        else if (auto mtmt = get<MetatableType>(follow(metaTable->metatable)))
            indexExprMetatableHelper(indexExpr, mtmt, exprType, indexType);
        else
        {
            LUAU_ASSERT(tt || get<PrimitiveType>(follow(metaTable->table)));

            reportError(CannotExtendTable{exprType, CannotExtendTable::Indexer, "indexer??"}, indexExpr->location);
        }
    }

    void visit(AstExprIndexExpr* indexExpr, ValueContext context)
    {
        if (auto str = indexExpr->index->as<AstExprConstantString>())
        {
            TypeId astIndexExprType = lookupType(indexExpr->index);
            const std::string stringValue(str->value.data, str->value.size);
            visitExprName(indexExpr->expr, indexExpr->location, stringValue, context, astIndexExprType);
            return;
        }

        visit(indexExpr->expr, ValueContext::RValue);
        visit(indexExpr->index, ValueContext::RValue);

        TypeId exprType = follow(lookupType(indexExpr->expr));
        TypeId indexType = follow(lookupType(indexExpr->index));

        if (auto tt = get<TableType>(exprType))
        {
            if (tt->indexer)
                testIsSubtype(indexType, tt->indexer->indexType, indexExpr->index->location);
            else
                reportError(CannotExtendTable{exprType, CannotExtendTable::Indexer, "indexer??"}, indexExpr->location);
        }
        else if (auto mt = get<MetatableType>(exprType))
        {
            return indexExprMetatableHelper(indexExpr, mt, exprType, indexType);
        }
        else if (auto cls = get<ClassType>(exprType))
        {
            if (cls->indexer)
                testIsSubtype(indexType, cls->indexer->indexType, indexExpr->index->location);
            else
                reportError(DynamicPropertyLookupOnClassesUnsafe{exprType}, indexExpr->location);
        }
        else if (get<UnionType>(exprType) && isOptional(exprType))
        {
            switch (shouldSuppressErrors(NotNull{&normalizer}, exprType))
            {
            case ErrorSuppression::Suppress:
                break;
            case ErrorSuppression::NormalizationFailed:
                reportError(NormalizationTooComplex{}, indexExpr->location);
                // fallthrough intentional
            case ErrorSuppression::DoNotSuppress:
                reportError(OptionalValueAccess{exprType}, indexExpr->location);
            }
        }
        else if (auto exprIntersection = get<IntersectionType>(exprType))
        {
            for (TypeId part : exprIntersection)
            {
                (void)part;
            }
        }
        else if (get<NeverType>(exprType) || isErrorSuppressing(indexExpr->location, exprType))
        {
            // Nothing
        }
        else
            reportError(NotATable{exprType}, indexExpr->location);
    }

    void visit(AstExprFunction* fn)
    {
        auto StackPusher = pushStack(fn);

        visitGenerics(fn->generics, fn->genericPacks);

        TypeId inferredFnTy = lookupType(fn);
        functionDeclStack.push_back(inferredFnTy);

        std::shared_ptr<const NormalizedType> normalizedFnTy = normalizer.normalize(inferredFnTy);
        if (!normalizedFnTy)
        {
            reportError(CodeTooComplex{}, fn->location);
        }
        else if (get<ErrorType>(normalizedFnTy->errors))
        {
            // Nothing
        }
        else if (!normalizedFnTy->hasFunctions())
        {
            ice->ice("Internal error: Lambda has non-function type " + toString(inferredFnTy), fn->location);
        }
        else
        {
            if (1 != normalizedFnTy->functions.parts.size())
                ice->ice("Unexpected: Lambda has unexpected type " + toString(inferredFnTy), fn->location);

            const FunctionType* inferredFtv = get<FunctionType>(normalizedFnTy->functions.parts.front());
            LUAU_ASSERT(inferredFtv);

            // There is no way to write an annotation for the self argument, so we
            // cannot do anything to check it.
            auto argIt = begin(inferredFtv->argTypes);
            if (fn->self)
                ++argIt;

            for (const auto& arg : fn->args)
            {
                if (argIt == end(inferredFtv->argTypes))
                    break;

                TypeId inferredArgTy = *argIt;

                if (arg->annotation)
                {
                    // we need to typecheck any argument annotations themselves.
                    visit(arg->annotation);

                    TypeId annotatedArgTy = lookupAnnotation(arg->annotation);

                    testIsSubtype(inferredArgTy, annotatedArgTy, arg->location);
                }

                // Some Luau constructs can result in an argument type being
                // reduced to never by inference. In this case, we want to
                // report an error at the function, instead of reporting an
                // error at every callsite.
                if (is<NeverType>(follow(inferredArgTy)))
                {
                    // If the annotation simplified to never, we don't want to
                    // even look at contributors.
                    bool explicitlyNever = false;
                    if (arg->annotation)
                    {
                        TypeId annotatedArgTy = lookupAnnotation(arg->annotation);
                        explicitlyNever = is<NeverType>(annotatedArgTy);
                    }

                    // Not following here is deliberate: the contribution map is
                    // keyed by type pointer, but that type pointer has, at some
                    // point, been transmuted to a bound type pointing to never.
                    if (const auto contributors = module->upperBoundContributors.find(inferredArgTy); contributors && !explicitlyNever)
                    {
                        // It's unfortunate that we can't link error messages
                        // together. For now, this will work.
                        reportError(
                            GenericError{format(
                                "Parameter '%s' has been reduced to never. This function is not callable with any possible value.", arg->name.value)},
                            arg->location);
                        for (const auto& [site, component] : *contributors)
                            reportError(ExtraInformation{format("Parameter '%s' is required to be a subtype of '%s' here.", arg->name.value,
                                            toString(component).c_str())},
                                site);
                    }
                }

                ++argIt;
            }

            // we need to typecheck the vararg annotation, if it exists.
            if (fn->vararg && fn->varargAnnotation)
                visit(fn->varargAnnotation);

            bool reachesImplicitReturn = getFallthrough(fn->body) != nullptr;
            if (reachesImplicitReturn && !allowsNoReturnValues(follow(inferredFtv->retTypes)))
                reportError(FunctionExitsWithoutReturning{inferredFtv->retTypes}, getEndLocation(fn));
        }

        visit(fn->body);

        // we need to typecheck the return annotation itself, if it exists.
        if (fn->returnAnnotation)
            visit(*fn->returnAnnotation);


        // If the function type has a function annotation, we need to see if we can suggest an annotation
        if (normalizedFnTy)
        {
            const FunctionType* inferredFtv = get<FunctionType>(normalizedFnTy->functions.parts.front());
            LUAU_ASSERT(inferredFtv);

            TypeFunctionReductionGuesser guesser{NotNull{&module->internalTypes}, builtinTypes, NotNull{&normalizer}};
            for (TypeId retTy : inferredFtv->retTypes)
            {
                if (get<TypeFunctionInstanceType>(follow(retTy)))
                {
                    TypeFunctionReductionGuessResult result = guesser.guessTypeFunctionReductionForFunctionExpr(*fn, inferredFtv, retTy);
                    if (result.shouldRecommendAnnotation)
                        reportError(ExplicitFunctionAnnotationRecommended{std::move(result.guessedFunctionAnnotations), result.guessedReturnType},
                            fn->location);
                }
            }
        }

        functionDeclStack.pop_back();
    }

    void visit(AstExprTable* expr)
    {
        // TODO!
        for (const AstExprTable::Item& item : expr->items)
        {
            if (item.key)
                visit(item.key, ValueContext::LValue);
            visit(item.value, ValueContext::RValue);
        }
    }

    void visit(AstExprUnary* expr)
    {
        visit(expr->expr, ValueContext::RValue);

        TypeId operandType = lookupType(expr->expr);
        TypeId resultType = lookupType(expr);

        if (isErrorSuppressing(expr->expr->location, operandType))
            return;

        if (auto it = kUnaryOpMetamethods.find(expr->op); it != kUnaryOpMetamethods.end())
        {
            std::optional<TypeId> mm = findMetatableEntry(builtinTypes, module->errors, operandType, it->second, expr->location);
            if (mm)
            {
                if (const FunctionType* ftv = get<FunctionType>(follow(*mm)))
                {
                    if (std::optional<TypeId> ret = first(ftv->retTypes))
                    {
                        if (expr->op == AstExprUnary::Op::Len)
                        {
                            testIsSubtype(follow(*ret), builtinTypes->numberType, expr->location);
                        }
                    }
                    else
                    {
                        reportError(GenericError{format("Metamethod '%s' must return a value", it->second)}, expr->location);
                    }

                    std::optional<TypeId> firstArg = first(ftv->argTypes);
                    if (!firstArg)
                    {
                        reportError(GenericError{"__unm metamethod must accept one argument"}, expr->location);
                        return;
                    }

                    TypePackId expectedArgs = module->internalTypes.addTypePack({operandType});
                    TypePackId expectedRet = module->internalTypes.addTypePack({resultType});

                    TypeId expectedFunction = module->internalTypes.addType(FunctionType{expectedArgs, expectedRet});

                    bool success = testIsSubtype(*mm, expectedFunction, expr->location);
                    if (!success)
                        return;
                }

                return;
            }
        }

        if (expr->op == AstExprUnary::Op::Len)
        {
            DenseHashSet<TypeId> seen{nullptr};
            int recursionCount = 0;
            std::shared_ptr<const NormalizedType> nty = normalizer.normalize(operandType);

            if (nty && nty->shouldSuppressErrors())
                return;

            if (!hasLength(operandType, seen, &recursionCount))
            {
                if (isOptional(operandType))
                    reportError(OptionalValueAccess{operandType}, expr->location);
                else
                    reportError(NotATable{operandType}, expr->location);
            }
        }
        else if (expr->op == AstExprUnary::Op::Minus)
        {
            testIsSubtype(operandType, builtinTypes->numberType, expr->location);
        }
        else if (expr->op == AstExprUnary::Op::Not)
        {
        }
        else
        {
            LUAU_ASSERT(!"Unhandled unary operator");
        }
    }

    TypeId visit(AstExprBinary* expr, AstNode* overrideKey = nullptr)
    {
        visit(expr->left, ValueContext::RValue);
        visit(expr->right, ValueContext::RValue);

        NotNull<Scope> scope = stack.back();

        bool isEquality = expr->op == AstExprBinary::Op::CompareEq || expr->op == AstExprBinary::Op::CompareNe;
        bool isComparison = expr->op >= AstExprBinary::Op::CompareEq && expr->op <= AstExprBinary::Op::CompareGe;
        bool isLogical = expr->op == AstExprBinary::Op::And || expr->op == AstExprBinary::Op::Or;

        TypeId leftType = follow(lookupType(expr->left));
        TypeId rightType = follow(lookupType(expr->right));
        TypeId expectedResult = follow(lookupType(expr));

        if (get<TypeFunctionInstanceType>(expectedResult))
        {
            checkForInternalTypeFunction(expectedResult, expr->location);
            return expectedResult;
        }

        if (expr->op == AstExprBinary::Op::Or)
        {
            leftType = stripNil(builtinTypes, module->internalTypes, leftType);
        }

        std::shared_ptr<const NormalizedType> normLeft = normalizer.normalize(leftType);
        std::shared_ptr<const NormalizedType> normRight = normalizer.normalize(rightType);

        bool isStringOperation =
            (normLeft ? normLeft->isSubtypeOfString() : isString(leftType)) && (normRight ? normRight->isSubtypeOfString() : isString(rightType));
        leftType = follow(leftType);
        if (get<AnyType>(leftType) || get<ErrorType>(leftType) || get<NeverType>(leftType))
            return leftType;
        else if (get<AnyType>(rightType) || get<ErrorType>(rightType) || get<NeverType>(rightType))
            return rightType;
        else if ((normLeft && normLeft->shouldSuppressErrors()) || (normRight && normRight->shouldSuppressErrors()))
            return builtinTypes->anyType; // we can't say anything better if it's error suppressing but not any or error alone.

        if ((get<BlockedType>(leftType) || get<FreeType>(leftType) || get<GenericType>(leftType)) && !isEquality && !isLogical)
        {
            auto name = getIdentifierOfBaseVar(expr->left);
            reportError(CannotInferBinaryOperation{expr->op, name,
                            isComparison ? CannotInferBinaryOperation::OpKind::Comparison : CannotInferBinaryOperation::OpKind::Operation},
                expr->location);
            return leftType;
        }

        NormalizationResult typesHaveIntersection = normalizer.isIntersectionInhabited(leftType, rightType);
        if (auto it = kBinaryOpMetamethods.find(expr->op); it != kBinaryOpMetamethods.end())
        {
            std::optional<TypeId> leftMt = getMetatable(leftType, builtinTypes);
            std::optional<TypeId> rightMt = getMetatable(rightType, builtinTypes);
            bool matches = leftMt == rightMt;


            if (isEquality && !matches)
            {
                auto testUnion = [&matches, builtinTypes = this->builtinTypes](const UnionType* utv, std::optional<TypeId> otherMt) {
                    for (TypeId option : utv)
                    {
                        if (getMetatable(follow(option), builtinTypes) == otherMt)
                        {
                            matches = true;
                            break;
                        }
                    }
                };

                if (const UnionType* utv = get<UnionType>(leftType); utv && rightMt)
                {
                    testUnion(utv, rightMt);
                }

                if (const UnionType* utv = get<UnionType>(rightType); utv && leftMt && !matches)
                {
                    testUnion(utv, leftMt);
                }
            }

            // If we're working with things that are not tables, the metatable comparisons above are a little excessive
            // It's ok for one type to have a meta table and the other to not. In that case, we should fall back on
            // checking if the intersection of the types is inhabited. If `typesHaveIntersection` failed due to limits,
            // TODO: Maybe add more checks here (e.g. for functions, classes, etc)
            if (!(get<TableType>(leftType) || get<TableType>(rightType)))
                if (!leftMt.has_value() || !rightMt.has_value())
                    matches = matches || typesHaveIntersection != NormalizationResult::False;

            if (!matches && isComparison)
            {
                reportError(GenericError{format("Types %s and %s cannot be compared with %s because they do not have the same metatable",
                                toString(leftType).c_str(), toString(rightType).c_str(), toString(expr->op).c_str())},
                    expr->location);

                return builtinTypes->errorRecoveryType();
            }

            std::optional<TypeId> mm;
            if (std::optional<TypeId> leftMm = findMetatableEntry(builtinTypes, module->errors, leftType, it->second, expr->left->location))
                mm = leftMm;
            else if (std::optional<TypeId> rightMm = findMetatableEntry(builtinTypes, module->errors, rightType, it->second, expr->right->location))
            {
                mm = rightMm;
                std::swap(leftType, rightType);
            }

            if (mm)
            {
                AstNode* key = expr;
                if (overrideKey != nullptr)
                    key = overrideKey;

                TypeId* selectedOverloadTy = module->astOverloadResolvedTypes.find(key);
                if (!selectedOverloadTy)
                {
                    // reportError(CodeTooComplex{}, expr->location);
                    // was handled by a type function
                    return expectedResult;
                }

                else if (const FunctionType* ftv = get<FunctionType>(follow(*selectedOverloadTy)))
                {
                    TypePackId expectedArgs;
                    // For >= and > we invoke __lt and __le respectively with
                    // swapped argument ordering.
                    if (expr->op == AstExprBinary::Op::CompareGe || expr->op == AstExprBinary::Op::CompareGt)
                    {
                        expectedArgs = module->internalTypes.addTypePack({rightType, leftType});
                    }
                    else
                    {
                        expectedArgs = module->internalTypes.addTypePack({leftType, rightType});
                    }

                    TypePackId expectedRets;
                    if (expr->op == AstExprBinary::CompareEq || expr->op == AstExprBinary::CompareNe || expr->op == AstExprBinary::CompareGe ||
                        expr->op == AstExprBinary::CompareGt || expr->op == AstExprBinary::Op::CompareLe || expr->op == AstExprBinary::Op::CompareLt)
                    {
                        expectedRets = module->internalTypes.addTypePack({builtinTypes->booleanType});
                    }
                    else
                    {
                        expectedRets = module->internalTypes.addTypePack({module->internalTypes.freshType(scope, TypeLevel{})});
                    }

                    TypeId expectedTy = module->internalTypes.addType(FunctionType(expectedArgs, expectedRets));

                    testIsSubtype(follow(*mm), expectedTy, expr->location);

                    std::optional<TypeId> ret = first(ftv->retTypes);
                    if (ret)
                    {
                        if (isComparison)
                        {
                            if (!isBoolean(follow(*ret)))
                            {
                                reportError(GenericError{format("Metamethod '%s' must return a boolean", it->second)}, expr->location);
                            }

                            return builtinTypes->booleanType;
                        }
                        else
                        {
                            return follow(*ret);
                        }
                    }
                    else
                    {
                        if (isComparison)
                        {
                            reportError(GenericError{format("Metamethod '%s' must return a boolean", it->second)}, expr->location);
                        }
                        else
                        {
                            reportError(GenericError{format("Metamethod '%s' must return a value", it->second)}, expr->location);
                        }

                        return builtinTypes->errorRecoveryType();
                    }
                }
                else
                {
                    reportError(CannotCallNonFunction{*mm}, expr->location);
                }

                return builtinTypes->errorRecoveryType();
            }
            // If this is a string comparison, or a concatenation of strings, we
            // want to fall through to primitive behavior.
            else if (!isEquality && !(isStringOperation && (expr->op == AstExprBinary::Op::Concat || isComparison)))
            {
                if ((leftMt && !isString(leftType)) || (rightMt && !isString(rightType)))
                {
                    if (isComparison)
                    {
                        reportError(GenericError{format(
                                        "Types '%s' and '%s' cannot be compared with %s because neither type's metatable has a '%s' metamethod",
                                        toString(leftType).c_str(), toString(rightType).c_str(), toString(expr->op).c_str(), it->second)},
                            expr->location);
                    }
                    else
                    {
                        reportError(GenericError{format(
                                        "Operator %s is not applicable for '%s' and '%s' because neither type's metatable has a '%s' metamethod",
                                        toString(expr->op).c_str(), toString(leftType).c_str(), toString(rightType).c_str(), it->second)},
                            expr->location);
                    }

                    return builtinTypes->errorRecoveryType();
                }
                else if (!leftMt && !rightMt && (get<TableType>(leftType) || get<TableType>(rightType)))
                {
                    if (isComparison)
                    {
                        reportError(GenericError{format("Types '%s' and '%s' cannot be compared with %s because neither type has a metatable",
                                        toString(leftType).c_str(), toString(rightType).c_str(), toString(expr->op).c_str())},
                            expr->location);
                    }
                    else
                    {
                        reportError(GenericError{format("Operator %s is not applicable for '%s' and '%s' because neither type has a metatable",
                                        toString(expr->op).c_str(), toString(leftType).c_str(), toString(rightType).c_str())},
                            expr->location);
                    }

                    return builtinTypes->errorRecoveryType();
                }
            }
        }

        switch (expr->op)
        {
        case AstExprBinary::Op::Add:
        case AstExprBinary::Op::Sub:
        case AstExprBinary::Op::Mul:
        case AstExprBinary::Op::Div:
        case AstExprBinary::Op::FloorDiv:
        case AstExprBinary::Op::Pow:
        case AstExprBinary::Op::Mod:
            testIsSubtype(leftType, builtinTypes->numberType, expr->left->location);
            testIsSubtype(rightType, builtinTypes->numberType, expr->right->location);

            return builtinTypes->numberType;
        case AstExprBinary::Op::Concat:
            testIsSubtype(leftType, builtinTypes->stringType, expr->left->location);
            testIsSubtype(rightType, builtinTypes->stringType, expr->right->location);

            return builtinTypes->stringType;
        case AstExprBinary::Op::CompareGe:
        case AstExprBinary::Op::CompareGt:
        case AstExprBinary::Op::CompareLe:
        case AstExprBinary::Op::CompareLt:
        {
            if (normLeft && normLeft->shouldSuppressErrors())
                return builtinTypes->booleanType;

            // if we're comparing against an uninhabited type, it's unobservable that the comparison did not run
            if (normLeft && normalizer.isInhabited(normLeft.get()) == NormalizationResult::False)
                return builtinTypes->booleanType;

            if (normLeft && normLeft->isExactlyNumber())
            {
                testIsSubtype(rightType, builtinTypes->numberType, expr->right->location);
                return builtinTypes->booleanType;
            }

            if (normLeft && normLeft->isSubtypeOfString())
            {
                testIsSubtype(rightType, builtinTypes->stringType, expr->right->location);
                return builtinTypes->booleanType;
            }

            reportError(GenericError{format("Types '%s' and '%s' cannot be compared with relational operator %s", toString(leftType).c_str(),
                            toString(rightType).c_str(), toString(expr->op).c_str())},
                expr->location);
            return builtinTypes->errorRecoveryType();
        }

        case AstExprBinary::Op::And:
        case AstExprBinary::Op::Or:
        case AstExprBinary::Op::CompareEq:
        case AstExprBinary::Op::CompareNe:
            // Ugly case: we don't care about this possibility, because a
            // compound assignment will never exist with one of these operators.
            return builtinTypes->anyType;
        default:
            // Unhandled AstExprBinary::Op possibility.
            LUAU_ASSERT(false);
            return builtinTypes->errorRecoveryType();
        }
    }

    void visit(AstExprTypeAssertion* expr)
    {
        visit(expr->expr, ValueContext::RValue);
        visit(expr->annotation);

        TypeId annotationType = lookupAnnotation(expr->annotation);
        TypeId computedType = lookupType(expr->expr);

        switch (shouldSuppressErrors(NotNull{&normalizer}, computedType).orElse(shouldSuppressErrors(NotNull{&normalizer}, annotationType)))
        {
        case ErrorSuppression::Suppress:
            return;
        case ErrorSuppression::NormalizationFailed:
            reportError(NormalizationTooComplex{}, expr->location);
            return;
        case ErrorSuppression::DoNotSuppress:
            break;
        }

        switch (normalizer.isInhabited(computedType))
        {
        case NormalizationResult::True:
            break;
        case NormalizationResult::False:
            return;
        case NormalizationResult::HitLimits:
            reportError(NormalizationTooComplex{}, expr->location);
            return;
        }

        switch (normalizer.isIntersectionInhabited(computedType, annotationType))
        {
        case NormalizationResult::True:
            return;
        case NormalizationResult::False:
            reportError(TypesAreUnrelated{computedType, annotationType}, expr->location);
            break;
        case NormalizationResult::HitLimits:
            reportError(NormalizationTooComplex{}, expr->location);
            break;
        }
    }

    void visit(AstExprIfElse* expr)
    {
        // TODO!
        visit(expr->condition, ValueContext::RValue);
        visit(expr->trueExpr, ValueContext::RValue);
        visit(expr->falseExpr, ValueContext::RValue);
    }

    void visit(AstExprInterpString* interpString)
    {
        for (AstExpr* expr : interpString->expressions)
            visit(expr, ValueContext::RValue);
    }

    void visit(AstExprError* expr)
    {
        // TODO!
        for (AstExpr* e : expr->expressions)
            visit(e, ValueContext::RValue);
    }

    /** Extract a TypeId for the first type of the provided pack.
     *
     * Note that this may require modifying some types.  I hope this doesn't cause problems!
     */
    TypeId flattenPack(TypePackId pack)
    {
        pack = follow(pack);

        if (auto fst = first(pack, /*ignoreHiddenVariadics*/ false))
            return *fst;
        else if (auto ftp = get<FreeTypePack>(pack))
        {
            TypeId result = module->internalTypes.addType(FreeType{ftp->scope});
            TypePackId freeTail = module->internalTypes.addTypePack(FreeTypePack{ftp->scope});

            TypePack* resultPack = emplaceTypePack<TypePack>(asMutable(pack));
            resultPack->head.assign(1, result);
            resultPack->tail = freeTail;

            return result;
        }
        else if (get<Unifiable::Error>(pack))
            return builtinTypes->errorRecoveryType();
        else if (finite(pack) && size(pack) == 0)
            return builtinTypes->nilType; // `(f())` where `f()` returns no values is coerced into `nil`
        else
            ice->ice("flattenPack got a weird pack!");
    }

    void visitGenerics(AstArray<AstGenericType> generics, AstArray<AstGenericTypePack> genericPacks)
    {
        DenseHashSet<AstName> seen{AstName{}};

        for (const auto& g : generics)
        {
            if (seen.contains(g.name))
                reportError(DuplicateGenericParameter{g.name.value}, g.location);
            else
                seen.insert(g.name);

            if (g.defaultValue)
                visit(g.defaultValue);
        }

        for (const auto& g : genericPacks)
        {
            if (seen.contains(g.name))
                reportError(DuplicateGenericParameter{g.name.value}, g.location);
            else
                seen.insert(g.name);

            if (g.defaultValue)
                visit(g.defaultValue);
        }
    }

    void visit(AstType* ty)
    {
        TypeId* resolvedTy = module->astResolvedTypes.find(ty);
        if (resolvedTy)
            checkForTypeFunctionInhabitance(follow(*resolvedTy), ty->location);

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
    }

    void visit(AstTypeReference* ty)
    {
        // No further validation is necessary in this case. The main logic for
        // _luau_print is contained in lookupAnnotation.
        if (FFlag::DebugLuauMagicTypes && ty->name == "_luau_print")
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

        std::optional<TypeFun> alias =
            (ty->prefix) ? scope->lookupImportedType(ty->prefix->value, ty->name.value) : scope->lookupType(ty->name.value);

        if (alias.has_value())
        {
            size_t typesRequired = alias->typeParams.size();
            size_t packsRequired = alias->typePackParams.size();

            bool hasDefaultTypes = std::any_of(alias->typeParams.begin(), alias->typeParams.end(), [](auto&& el) {
                return el.defaultValue.has_value();
            });

            bool hasDefaultPacks = std::any_of(alias->typePackParams.begin(), alias->typePackParams.end(), [](auto&& el) {
                return el.defaultValue.has_value();
            });

            if (!ty->hasParameterList)
            {
                if ((!alias->typeParams.empty() && !hasDefaultTypes) || (!alias->typePackParams.empty() && !hasDefaultPacks))
                {
                    reportError(GenericError{"Type parameter list is required"}, ty->location);
                }
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
                    {
                        typesProvided += 1;
                    }
                    else
                    {
                        extraTypes += 1;
                    }
                }
                else if (p.typePack)
                {
                    std::optional<TypePackId> tp = lookupPackAnnotation(p.typePack);
                    if (!tp.has_value())
                        continue;

                    if (typesProvided < typesRequired && size(*tp) == 1 && finite(*tp) && first(*tp))
                    {
                        typesProvided += 1;
                    }
                    else
                    {
                        packsProvided += 1;
                    }
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
                {
                    typesProvided += 1;
                }
            }

            for (size_t i = packsProvided; i < packsRequired; ++i)
            {
                if (alias->typePackParams[i].defaultValue)
                {
                    packsProvided += 1;
                }
            }

            if (extraTypes == 0 && packsProvided + 1 == packsRequired)
            {
                packsProvided += 1;
            }

            if (typesProvided != typesRequired || packsProvided != packsRequired)
            {
                reportError(IncorrectGenericParameterCount{
                                /* name */ ty->name.value,
                                /* typeFun */ *alias,
                                /* actualParameters */ typesProvided,
                                /* actualPackParameters */ packsProvided,
                            },
                    ty->location);
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
                    ty->location);
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

                reportError(UnknownSymbol{symbol, UnknownSymbol::Context::Type}, ty->location);
            }
        }
    }

    void visit(AstTypeTable* table)
    {
        // TODO!

        for (const AstTableProp& prop : table->props)
            visit(prop.type);

        if (table->indexer)
        {
            visit(table->indexer->indexType);
            visit(table->indexer->resultType);
        }
    }

    void visit(AstTypeFunction* ty)
    {
        visitGenerics(ty->generics, ty->genericPacks);
        visit(ty->argTypes);
        visit(ty->returnTypes);
    }

    void visit(AstTypeTypeof* ty)
    {
        visit(ty->expr, ValueContext::RValue);
    }

    void visit(AstTypeUnion* ty)
    {
        // TODO!
        for (AstType* type : ty->types)
            visit(type);
    }

    void visit(AstTypeIntersection* ty)
    {
        // TODO!
        for (AstType* type : ty->types)
            visit(type);
    }

    void visit(AstTypePack* pack)
    {
        if (auto p = pack->as<AstTypePackExplicit>())
            return visit(p);
        else if (auto p = pack->as<AstTypePackVariadic>())
            return visit(p);
        else if (auto p = pack->as<AstTypePackGeneric>())
            return visit(p);
    }

    void visit(AstTypePackExplicit* tp)
    {
        // TODO!
        for (AstType* type : tp->typeList.types)
            visit(type);

        if (tp->typeList.tailType)
            visit(tp->typeList.tailType);
    }

    void visit(AstTypePackVariadic* tp)
    {
        // TODO!
        visit(tp->variadicType);
    }

    void visit(AstTypePackGeneric* tp)
    {
        Scope* scope = findInnermostScope(tp->location);
        LUAU_ASSERT(scope);

        std::optional<TypePackId> alias = scope->lookupPack(tp->genericName.value);
        if (!alias.has_value())
        {
            if (scope->lookupType(tp->genericName.value))
            {
                reportError(
                    SwappedGenericTypeParameter{
                        tp->genericName.value,
                        SwappedGenericTypeParameter::Kind::Pack,
                    },
                    tp->location);
            }
            else
            {
                reportError(UnknownSymbol{tp->genericName.value, UnknownSymbol::Context::Type}, tp->location);
            }
        }
    }

    struct Reasonings
    {
        // the list of reasons
        std::vector<std::string> reasons;

        // this should be true if _all_ of the reasons have an error suppressing type, and false otherwise.
        bool suppressed;

        std::string toString()
        {
            // DenseHashSet ordering is entirely undefined, so we want to
            // sort the reasons here to achieve a stable error
            // stringification.
            std::sort(reasons.begin(), reasons.end());
            std::string allReasons;
            bool first = true;
            for (const std::string& reason : reasons)
            {
                if (first)
                    first = false;
                else
                    allReasons += "\n\t";

                allReasons += reason;
            }

            return allReasons;
        }
    };

    template<typename TID>
    Reasonings explainReasonings(TID subTy, TID superTy, Location location, const SubtypingResult& r)
    {
        if (r.reasoning.empty())
            return {};

        std::vector<std::string> reasons;
        bool suppressed = true;
        for (const SubtypingReasoning& reasoning : r.reasoning)
        {
            if (reasoning.subPath.empty() && reasoning.superPath.empty())
                continue;

            std::optional<TypeOrPack> optSubLeaf = traverse(subTy, reasoning.subPath, builtinTypes);
            std::optional<TypeOrPack> optSuperLeaf = traverse(superTy, reasoning.superPath, builtinTypes);

            if (!optSubLeaf || !optSuperLeaf)
                ice->ice("Subtyping test returned a reasoning with an invalid path", location);

            const TypeOrPack& subLeaf = *optSubLeaf;
            const TypeOrPack& superLeaf = *optSuperLeaf;

            auto subLeafTy = get<TypeId>(subLeaf);
            auto superLeafTy = get<TypeId>(superLeaf);

            auto subLeafTp = get<TypePackId>(subLeaf);
            auto superLeafTp = get<TypePackId>(superLeaf);

            if (!subLeafTy && !superLeafTy && !subLeafTp && !superLeafTp)
                ice->ice("Subtyping test returned a reasoning where one path ends at a type and the other ends at a pack.", location);

            std::string relation = "a subtype of";
            if (reasoning.variance == SubtypingVariance::Invariant)
                relation = "exactly";
            else if (reasoning.variance == SubtypingVariance::Contravariant)
                relation = "a supertype of";

            std::string reason;
            if (reasoning.subPath == reasoning.superPath)
                reason = "at " + toString(reasoning.subPath) + ", " + toString(subLeaf) + " is not " + relation + " " + toString(superLeaf);
            else
                reason = "type " + toString(subTy) + toString(reasoning.subPath, /* prefixDot */ true) + " (" + toString(subLeaf) + ") is not " +
                         relation + " " + toString(superTy) + toString(reasoning.superPath, /* prefixDot */ true) + " (" + toString(superLeaf) + ")";

            reasons.push_back(reason);

            // if we haven't already proved this isn't suppressing, we have to keep checking.
            if (suppressed)
            {
                if (subLeafTy && superLeafTy)
                    suppressed &= isErrorSuppressing(location, *subLeafTy) || isErrorSuppressing(location, *superLeafTy);
                else
                    suppressed &= isErrorSuppressing(location, *subLeafTp) || isErrorSuppressing(location, *superLeafTp);
            }
        }

        return {std::move(reasons), suppressed};
    }


    void explainError(TypeId subTy, TypeId superTy, Location location, const SubtypingResult& result)
    {
        switch (shouldSuppressErrors(NotNull{&normalizer}, subTy).orElse(shouldSuppressErrors(NotNull{&normalizer}, superTy)))
        {
        case ErrorSuppression::Suppress:
            return;
        case ErrorSuppression::NormalizationFailed:
            reportError(NormalizationTooComplex{}, location);
        case ErrorSuppression::DoNotSuppress:
            break;
        }

        Reasonings reasonings = explainReasonings(subTy, superTy, location, result);

        if (!reasonings.suppressed)
            reportError(TypeMismatch{superTy, subTy, reasonings.toString()}, location);
    }

    void explainError(TypePackId subTy, TypePackId superTy, Location location, const SubtypingResult& result)
    {
        switch (shouldSuppressErrors(NotNull{&normalizer}, subTy).orElse(shouldSuppressErrors(NotNull{&normalizer}, superTy)))
        {
        case ErrorSuppression::Suppress:
            return;
        case ErrorSuppression::NormalizationFailed:
            reportError(NormalizationTooComplex{}, location);
        case ErrorSuppression::DoNotSuppress:
            break;
        }

        Reasonings reasonings = explainReasonings(subTy, superTy, location, result);

        if (!reasonings.suppressed)
            reportError(TypePackMismatch{superTy, subTy, reasonings.toString()}, location);
    }

    bool testIsSubtype(TypeId subTy, TypeId superTy, Location location)
    {
        SubtypingResult r = subtyping->isSubtype(subTy, superTy);

        if (r.normalizationTooComplex)
            reportError(NormalizationTooComplex{}, location);

        if (!r.isSubtype)
            explainError(subTy, superTy, location, r);

        return r.isSubtype;
    }

    bool testIsSubtype(TypePackId subTy, TypePackId superTy, Location location)
    {
        SubtypingResult r = subtyping->isSubtype(subTy, superTy);

        if (r.normalizationTooComplex)
            reportError(NormalizationTooComplex{}, location);

        if (!r.isSubtype)
            explainError(subTy, superTy, location, r);

        return r.isSubtype;
    }

    void reportError(TypeErrorData data, const Location& location)
    {
        if (auto utk = get_if<UnknownProperty>(&data))
            diagnoseMissingTableKey(utk, data);

        module->errors.emplace_back(location, module->name, std::move(data));

        if (logger)
            logger->captureTypeCheckError(module->errors.back());
    }

    void reportError(TypeError e)
    {
        reportError(std::move(e.data), e.location);
    }

    void reportErrors(ErrorVec errors)
    {
        for (TypeError e : errors)
            reportError(std::move(e));
    }

    struct PropertyTypes
    {
        // a vector of all the types assigned to the given property.
        std::vector<TypeId> typesOfProp;

        // a vector of all the types that are missing the given property.
        std::vector<TypeId> missingProp;

        bool foundOneProp() const
        {
            return !typesOfProp.empty();
        }

        bool noneMissingProp() const
        {
            return missingProp.empty();
        }

        bool foundMissingProp() const
        {
            return !missingProp.empty();
        }
    };

    /* A helper for checkIndexTypeFromType.
     *
     * Returns a pair:
     * * A boolean indicating that at least one of the constituent types
     *     contains the prop, and
     * * A vector of types that do not contain the prop.
     */
    PropertyTypes lookupProp(const NormalizedType* norm, const std::string& prop, ValueContext context, const Location& location,
        TypeId astIndexExprType, std::vector<TypeError>& errors)
    {
        std::vector<TypeId> typesOfProp;
        std::vector<TypeId> typesMissingTheProp;

        // this is `false` if we ever hit the resource limits during any of our uses of `fetch`.
        bool normValid = true;

        auto fetch = [&](TypeId ty) {
            NormalizationResult result = normalizer.isInhabited(ty);
            if (result == NormalizationResult::HitLimits)
                normValid = false;
            if (result != NormalizationResult::True)
                return;

            DenseHashSet<TypeId> seen{nullptr};
            PropertyType res = hasIndexTypeFromType(ty, prop, context, location, seen, astIndexExprType, errors);

            if (res.present == NormalizationResult::HitLimits)
            {
                normValid = false;
                return;
            }

            if (res.present == NormalizationResult::True && res.result)
                typesOfProp.emplace_back(*res.result);

            if (res.present == NormalizationResult::False)
                typesMissingTheProp.push_back(ty);
        };

        if (normValid)
            fetch(norm->tops);
        if (normValid)
            fetch(norm->booleans);

        if (normValid)
        {
            for (const auto& [ty, _negations] : norm->classes.classes)
            {
                fetch(ty);

                if (!normValid)
                    break;
            }
        }

        if (normValid)
            fetch(norm->errors);
        if (normValid)
            fetch(norm->nils);
        if (normValid)
            fetch(norm->numbers);
        if (normValid && !norm->strings.isNever())
            fetch(builtinTypes->stringType);
        if (normValid)
            fetch(norm->threads);
        if (normValid)
            fetch(norm->buffers);

        if (normValid)
        {
            for (TypeId ty : norm->tables)
            {
                fetch(ty);

                if (!normValid)
                    break;
            }
        }

        if (normValid && norm->functions.isTop)
            fetch(builtinTypes->functionType);
        else if (normValid && !norm->functions.isNever())
        {
            if (norm->functions.parts.size() == 1)
                fetch(norm->functions.parts.front());
            else
            {
                std::vector<TypeId> parts;
                parts.insert(parts.end(), norm->functions.parts.begin(), norm->functions.parts.end());
                fetch(module->internalTypes.addType(IntersectionType{std::move(parts)}));
            }
        }

        if (normValid)
        {
            for (const auto& [tyvar, intersect] : norm->tyvars)
            {
                if (get<NeverType>(intersect->tops))
                {
                    TypeId ty = normalizer.typeFromNormal(*intersect);
                    fetch(module->internalTypes.addType(IntersectionType{{tyvar, ty}}));
                }
                else
                    fetch(follow(tyvar));

                if (!normValid)
                    break;
            }
        }

        return {typesOfProp, typesMissingTheProp};
    }

    // If the provided type does not have the named property, report an error.
    void checkIndexTypeFromType(TypeId tableTy, const std::string& prop, ValueContext context, const Location& location, TypeId astIndexExprType)
    {
        std::shared_ptr<const NormalizedType> norm = normalizer.normalize(tableTy);
        if (!norm)
        {
            reportError(NormalizationTooComplex{}, location);
            return;
        }

        // if the type is error suppressing, we don't actually have any work left to do.
        if (norm->shouldSuppressErrors())
            return;

        std::vector<TypeError> dummy;
        const auto propTypes = lookupProp(norm.get(), prop, context, location, astIndexExprType, module->errors);

        if (propTypes.foundMissingProp())
        {
            if (propTypes.foundOneProp())
                reportError(MissingUnionProperty{tableTy, propTypes.missingProp, prop}, location);
            // For class LValues, we don't want to report an extension error,
            // because classes come into being with full knowledge of their
            // shape. We instead want to report the unknown property error of
            // the `else` branch.
            else if (context == ValueContext::LValue && !get<ClassType>(tableTy))
            {
                const auto lvPropTypes = lookupProp(norm.get(), prop, ValueContext::RValue, location, astIndexExprType, dummy);
                if (lvPropTypes.foundOneProp() && lvPropTypes.noneMissingProp())
                    reportError(PropertyAccessViolation{tableTy, prop, PropertyAccessViolation::CannotWrite}, location);
                else if (get<PrimitiveType>(tableTy) || get<FunctionType>(tableTy))
                    reportError(NotATable{tableTy}, location);
                else
                    reportError(CannotExtendTable{tableTy, CannotExtendTable::Property, prop}, location);
            }
            else if (context == ValueContext::RValue && !get<ClassType>(tableTy))
            {
                const auto rvPropTypes = lookupProp(norm.get(), prop, ValueContext::LValue, location, astIndexExprType, dummy);
                if (rvPropTypes.foundOneProp() && rvPropTypes.noneMissingProp())
                    reportError(PropertyAccessViolation{tableTy, prop, PropertyAccessViolation::CannotRead}, location);
                else
                    reportError(UnknownProperty{tableTy, prop}, location);
            }
            else
                reportError(UnknownProperty{tableTy, prop}, location);
        }
    }

    struct PropertyType
    {
        NormalizationResult present;
        std::optional<TypeId> result;
    };

    PropertyType hasIndexTypeFromType(TypeId ty, const std::string& prop, ValueContext context, const Location& location, DenseHashSet<TypeId>& seen,
        TypeId astIndexExprType, std::vector<TypeError>& errors)
    {
        // If we have already encountered this type, we must assume that some
        // other codepath will do the right thing and signal false if the
        // property is not present.
        if (seen.contains(ty))
            return {NormalizationResult::True, {}};
        seen.insert(ty);

        if (get<ErrorType>(ty) || get<AnyType>(ty) || get<NeverType>(ty))
            return {NormalizationResult::True, {ty}};

        if (isString(ty))
        {
            std::optional<TypeId> mtIndex = Luau::findMetatableEntry(builtinTypes, errors, builtinTypes->stringType, "__index", location);
            LUAU_ASSERT(mtIndex);
            ty = *mtIndex;
        }

        if (auto tt = getTableType(ty))
        {
            if (auto resTy = findTablePropertyRespectingMeta(builtinTypes, errors, ty, prop, context, location))
                return {NormalizationResult::True, resTy};

            if (tt->indexer)
            {
                TypeId indexType = follow(tt->indexer->indexType);
                if (isPrim(indexType, PrimitiveType::String))
                    return {NormalizationResult::True, {tt->indexer->indexResultType}};
                // If the indexer looks like { [any] : _} - the prop lookup should be allowed!
                else if (get<AnyType>(indexType) || get<UnknownType>(indexType))
                    return {NormalizationResult::True, {tt->indexer->indexResultType}};
            }


            // if we are in a conditional context, we treat the property as present and `unknown` because
            // we may be _refining_ `tableTy` to include that property. we will want to revisit this a bit
            // in the future once luau has support for exact tables since this only applies when inexact.
            return {inConditional(typeContext) ? NormalizationResult::True : NormalizationResult::False, {builtinTypes->unknownType}};
        }
        else if (const ClassType* cls = get<ClassType>(ty))
        {
            // If the property doesn't exist on the class, we consult the indexer
            // We need to check if the type of the index expression foo (x[foo])
            // is compatible with the indexer's indexType
            // Construct the intersection and test inhabitedness!
            if (auto property = lookupClassProp(cls, prop))
                return {NormalizationResult::True, context == ValueContext::LValue ? property->writeTy : property->readTy};
            if (cls->indexer)
            {
                TypeId inhabitatedTestType = module->internalTypes.addType(IntersectionType{{cls->indexer->indexType, astIndexExprType}});
                return {normalizer.isInhabited(inhabitatedTestType), {cls->indexer->indexResultType}};
            }
            return {NormalizationResult::False, {}};
        }
        else if (const UnionType* utv = get<UnionType>(ty))
        {
            std::vector<TypeId> parts;
            parts.reserve(utv->options.size());

            for (TypeId part : utv)
            {
                PropertyType result = hasIndexTypeFromType(part, prop, context, location, seen, astIndexExprType, errors);

                if (result.present != NormalizationResult::True)
                    return {result.present, {}};
                if (result.result)
                    parts.emplace_back(*result.result);
            }

            if (parts.size() == 0)
                return {NormalizationResult::False, {}};

            if (parts.size() == 1)
                return {NormalizationResult::True, {parts[0]}};

            TypeId propTy;
            if (context == ValueContext::LValue)
                propTy = module->internalTypes.addType(IntersectionType{parts});
            else
                propTy = module->internalTypes.addType(UnionType{parts});

            return {NormalizationResult::True, propTy};
        }
        else if (const IntersectionType* itv = get<IntersectionType>(ty))
        {
            for (TypeId part : itv)
            {
                PropertyType result = hasIndexTypeFromType(part, prop, context, location, seen, astIndexExprType, errors);
                if (result.present != NormalizationResult::False)
                    return result;
            }

            return {NormalizationResult::False, {}};
        }
        else if (const PrimitiveType* pt = get<PrimitiveType>(ty))
            return {(inConditional(typeContext) && pt->type == PrimitiveType::Table) ? NormalizationResult::True : NormalizationResult::False, {ty}};
        else
            return {NormalizationResult::False, {}};
    }

    void diagnoseMissingTableKey(UnknownProperty* utk, TypeErrorData& data) const
    {
        std::string_view sv(utk->key);
        std::set<Name> candidates;

        auto accumulate = [&](const TableType::Props& props) {
            for (const auto& [name, ty] : props)
            {
                if (sv != name && equalsLower(sv, name))
                    candidates.insert(name);
            }
        };

        if (auto ttv = getTableType(utk->table))
            accumulate(ttv->props);
        else if (auto ctv = get<ClassType>(follow(utk->table)))
        {
            while (ctv)
            {
                accumulate(ctv->props);

                if (!ctv->parent)
                    break;

                ctv = get<ClassType>(*ctv->parent);
                LUAU_ASSERT(ctv);
            }
        }

        if (!candidates.empty())
            data = TypeErrorData(UnknownPropButFoundLikeProp{utk->table, utk->key, candidates});
    }

    bool isErrorSuppressing(Location loc, TypeId ty)
    {
        switch (shouldSuppressErrors(NotNull{&normalizer}, ty))
        {
        case ErrorSuppression::DoNotSuppress:
            return false;
        case ErrorSuppression::Suppress:
            return true;
        case ErrorSuppression::NormalizationFailed:
            reportError(NormalizationTooComplex{}, loc);
            return false;
        };

        LUAU_ASSERT(false);
        return false; // UNREACHABLE
    }

    bool isErrorSuppressing(Location loc1, TypeId ty1, Location loc2, TypeId ty2)
    {
        return isErrorSuppressing(loc1, ty1) || isErrorSuppressing(loc2, ty2);
    }

    bool isErrorSuppressing(Location loc, TypePackId tp)
    {
        switch (shouldSuppressErrors(NotNull{&normalizer}, tp))
        {
        case ErrorSuppression::DoNotSuppress:
            return false;
        case ErrorSuppression::Suppress:
            return true;
        case ErrorSuppression::NormalizationFailed:
            reportError(NormalizationTooComplex{}, loc);
            return false;
        };

        LUAU_ASSERT(false);
        return false; // UNREACHABLE
    }

    bool isErrorSuppressing(Location loc1, TypePackId tp1, Location loc2, TypePackId tp2)
    {
        return isErrorSuppressing(loc1, tp1) || isErrorSuppressing(loc2, tp2);
    }
};

void check(NotNull<BuiltinTypes> builtinTypes, NotNull<UnifierSharedState> unifierState, NotNull<TypeCheckLimits> limits, DcrLogger* logger,
    const SourceModule& sourceModule, Module* module)
{
    LUAU_TIMETRACE_SCOPE("check", "Typechecking");

    TypeChecker2 typeChecker{builtinTypes, unifierState, limits, logger, &sourceModule, module};

    typeChecker.visit(sourceModule.root);

    unfreeze(module->interfaceTypes);
    copyErrors(module->errors, module->interfaceTypes, builtinTypes);
    freeze(module->interfaceTypes);
}

} // namespace Luau
