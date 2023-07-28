// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeChecker2.h"

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/DcrLogger.h"
#include "Luau/Error.h"
#include "Luau/InsertionOrderedMap.h"
#include "Luau/Instantiation.h"
#include "Luau/Metamethods.h"
#include "Luau/Normalize.h"
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier.h"
#include "Luau/TypeFamily.h"
#include "Luau/VisitType.h"

#include <algorithm>

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
    if (a.family != b.family)
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

struct FamilyFinder : TypeOnceVisitor
{
    DenseHashSet<TypeId> mentionedFamilies{nullptr};
    DenseHashSet<TypePackId> mentionedFamilyPacks{nullptr};

    bool visit(TypeId ty, const TypeFamilyInstanceType&) override
    {
        mentionedFamilies.insert(ty);
        return true;
    }

    bool visit(TypePackId tp, const TypeFamilyInstanceTypePack&) override
    {
        mentionedFamilyPacks.insert(tp);
        return true;
    }
};

struct InternalFamilyFinder : TypeOnceVisitor
{
    DenseHashSet<TypeId> internalFamilies{nullptr};
    DenseHashSet<TypePackId> internalPackFamilies{nullptr};
    DenseHashSet<TypeId> mentionedFamilies{nullptr};
    DenseHashSet<TypePackId> mentionedFamilyPacks{nullptr};

    InternalFamilyFinder(std::vector<TypeId>& declStack)
    {
        FamilyFinder f;
        for (TypeId fn : declStack)
            f.traverse(fn);

        mentionedFamilies = std::move(f.mentionedFamilies);
        mentionedFamilyPacks = std::move(f.mentionedFamilyPacks);
    }

    bool visit(TypeId ty, const TypeFamilyInstanceType& tfit) override
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
            for (TypeId mentioned : mentionedFamilies)
            {
                const TypeFamilyInstanceType* mentionedTfit = get<TypeFamilyInstanceType>(mentioned);
                LUAU_ASSERT(mentionedTfit);
                if (areEquivalent(tfit, *mentionedTfit))
                {
                    return true;
                }
            }

            internalFamilies.insert(ty);
        }

        return true;
    }

    bool visit(TypePackId tp, const TypeFamilyInstanceTypePack& tfitp) override
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
            for (TypePackId mentioned : mentionedFamilyPacks)
            {
                const TypeFamilyInstanceTypePack* mentionedTfitp = get<TypeFamilyInstanceTypePack>(mentioned);
                LUAU_ASSERT(mentionedTfitp);
                if (areEquivalent(tfitp, *mentionedTfitp))
                {
                    return true;
                }
            }

            internalPackFamilies.insert(tp);
        }

        return true;
    }
};

struct TypeChecker2
{
    NotNull<BuiltinTypes> builtinTypes;
    DcrLogger* logger;
    NotNull<InternalErrorReporter> ice;
    const SourceModule* sourceModule;
    Module* module;
    TypeArena testArena;

    std::vector<NotNull<Scope>> stack;
    std::vector<TypeId> functionDeclStack;

    DenseHashSet<TypeId> noTypeFamilyErrors{nullptr};

    Normalizer normalizer;

    TypeChecker2(NotNull<BuiltinTypes> builtinTypes, NotNull<UnifierSharedState> unifierState, DcrLogger* logger, const SourceModule* sourceModule,
        Module* module)
        : builtinTypes(builtinTypes)
        , logger(logger)
        , ice(unifierState->iceHandler)
        , sourceModule(sourceModule)
        , module(module)
        , normalizer{&testArena, builtinTypes, unifierState, /* cacheInhabitance */ true}
    {
    }

    std::optional<StackPusher> pushStack(AstNode* node)
    {
        if (Scope** scope = module->astScopes.find(node))
            return StackPusher{stack, *scope};
        else
            return std::nullopt;
    }

    void checkForInternalFamily(TypeId ty, Location location)
    {
        InternalFamilyFinder finder(functionDeclStack);
        finder.traverse(ty);

        for (TypeId internal : finder.internalFamilies)
            reportError(WhereClauseNeeded{internal}, location);

        for (TypePackId internal : finder.internalPackFamilies)
            reportError(PackWhereClauseNeeded{internal}, location);
    }

    TypeId checkForFamilyInhabitance(TypeId instance, Location location)
    {
        if (noTypeFamilyErrors.find(instance))
            return instance;

        TxnLog fake{};
        ErrorVec errors =
            reduceFamilies(instance, location, NotNull{&testArena}, builtinTypes, stack.back(), NotNull{&normalizer}, &fake, true).errors;

        if (errors.empty())
            noTypeFamilyErrors.insert(instance);

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
            return checkForFamilyInhabitance(follow(*ty), expr->location);

        TypePackId* tp = module->astTypePacks.find(expr);
        if (tp)
            return checkForFamilyInhabitance(flattenPack(*tp), expr->location);

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
        return checkForFamilyInhabitance(follow(*ty), annotation->location);
    }

    TypePackId lookupPackAnnotation(AstTypePack* annotation)
    {
        TypePackId* tp = module->astResolvedTypePacks.find(annotation);
        LUAU_ASSERT(tp);
        return follow(*tp);
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
        Location bestLocation = module->scopes[0].first;

        for (size_t i = 0; i < module->scopes.size(); ++i)
        {
            auto& [scopeBounds, scope] = module->scopes[i];
            if (scopeBounds.encloses(location))
            {
                if (scopeBounds.begin > bestLocation.begin || scopeBounds.end < bestLocation.end)
                {
                    bestScope = scope.get();
                    bestLocation = scopeBounds;
                }
            }
        }

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
        visit(ifStatement->condition, ValueContext::RValue);
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

        TypeArena* arena = &testArena;
        TypePackId actualRetType = reconstructPack(ret->list, *arena);

        Unifier u{NotNull{&normalizer}, stack.back(), ret->location, Covariant};
        u.hideousFixMeGenericsAreActuallyFree = true;

        u.tryUnify(actualRetType, expectedRetType);
        const bool ok = (u.errors.empty() && u.log.empty()) || isErrorSuppressing(ret->location, actualRetType, ret->location, expectedRetType);

        if (!ok)
        {
            for (const TypeError& e : u.errors)
                reportError(e);
        }

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
                    {
                        reportErrors(tryUnify(stack.back(), value->location, valueType, annotationType));
                    }

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
                        reportErrors(tryUnify(stack.back(), value->location, valueTypes.head[j - i], varType));

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
        NotNull<Scope> scope = stack.back();

        if (forStatement->var->annotation)
        {
            visit(forStatement->var->annotation);
            reportErrors(tryUnify(scope, forStatement->var->location, builtinTypes->numberType, lookupAnnotation(forStatement->var->annotation)));
        }

        auto checkNumber = [this, scope](AstExpr* expr) {
            if (!expr)
                return;

            visit(expr, ValueContext::RValue);
            reportErrors(tryUnify(scope, expr->location, lookupType(expr), builtinTypes->numberType));
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
        TypeArena& arena = testArena;

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

        auto checkFunction = [this, &arena, &scope, &forInStatement, &variableTypes](
                                 const FunctionType* iterFtv, std::vector<TypeId> iterTys, bool isMm) {
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
                reportErrors(tryUnify(scope, forInStatement->vars.data[i]->location, variableTypes[i], expectedVariableTypes.head[i]));

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
                reportErrors(tryUnify(scope, forInStatement->values.data[valueIndex]->location, iterTys[1], flattenedArgTypes.head[0]));
            }

            if (iterTys.size() == 3 && flattenedArgTypes.head.size() > 1)
            {
                size_t valueIndex = forInStatement->values.size > 2 ? 2 : 0;
                reportErrors(tryUnify(scope, forInStatement->values.data[valueIndex]->location, iterTys[2], flattenedArgTypes.head[1]));
            }
        };

        const NormalizedType* iteratorNorm = normalizer.normalize(iteratorTy);

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
                reportErrors(tryUnify(scope, forInStatement->vars.data[0]->location, variableTypes[0], ttv->indexer->indexType));
                if (variableTypes.size() == 2)
                    reportErrors(tryUnify(scope, forInStatement->vars.data[1]->location, variableTypes[1], ttv->indexer->indexResultType));
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
            Instantiation instantiation{TxnLog::empty(), &arena, TypeLevel{}, scope};

            if (std::optional<TypeId> instantiatedIterMmTy = instantiation.substitute(*iterMmTy))
            {
                if (const FunctionType* iterMmFtv = get<FunctionType>(*instantiatedIterMmTy))
                {
                    TypePackId argPack = arena.addTypePack({iteratorTy});
                    reportErrors(tryUnify(scope, forInStatement->values.data[0]->location, argPack, iterMmFtv->argTypes));

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
        else if (iteratorNorm && iteratorNorm->hasTopTable())
        {
            // nothing
        }
        else if (!iteratorNorm || !iteratorNorm->shouldSuppressErrors())
        {
            reportError(CannotCallNonFunction{iteratorTy}, forInStatement->values.data[0]->location);
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
                continue;


            if (!isSubtype(rhsType, lhsType, stack.back()) &&
                !isErrorSuppressing(assign->vars.data[i]->location, lhsType, assign->values.data[i]->location, rhsType))
            {
                reportError(TypeMismatch{lhsType, rhsType}, rhs->location);
            }
        }
    }

    void visit(AstStatCompoundAssign* stat)
    {
        AstExprBinary fake{stat->location, stat->op, stat->var, stat->value};
        TypeId resultTy = visit(&fake, stat);
        TypeId varTy = lookupType(stat->var);

        reportErrors(tryUnify(stack.back(), stat->location, resultTy, varTy));
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
        NotNull<Scope> scope = stack.back();
        TypeId actualType = lookupType(expr);
        TypeId expectedType = builtinTypes->nilType;
        LUAU_ASSERT(isSubtype(actualType, expectedType, scope));
    }

    void visit(AstExprConstantBool* expr)
    {
        NotNull<Scope> scope = stack.back();
        TypeId actualType = lookupType(expr);
        TypeId expectedType = builtinTypes->booleanType;
        LUAU_ASSERT(isSubtype(actualType, expectedType, scope));
    }

    void visit(AstExprConstantNumber* expr)
    {
        NotNull<Scope> scope = stack.back();
        TypeId actualType = lookupType(expr);
        TypeId expectedType = builtinTypes->numberType;
        LUAU_ASSERT(isSubtype(actualType, expectedType, scope));
    }

    void visit(AstExprConstantString* expr)
    {
        NotNull<Scope> scope = stack.back();
        TypeId actualType = lookupType(expr);
        TypeId expectedType = builtinTypes->stringType;
        LUAU_ASSERT(isSubtype(actualType, expectedType, scope));
    }

    void visit(AstExprLocal* expr)
    {
        // TODO!
    }

    void visit(AstExprGlobal* expr)
    {
        // TODO!
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
        if (!originalCallTy && !selectedOverloadTy)
            return;

        TypeId fnTy = follow(selectedOverloadTy ? *selectedOverloadTy : *originalCallTy);
        if (get<AnyType>(fnTy) || get<ErrorType>(fnTy) || get<NeverType>(fnTy))
            return;
        else if (isOptional(fnTy))
        {
            reportError(OptionalValueAccess{fnTy}, call->func->location);
            return;
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

        FunctionCallResolver resolver{
            builtinTypes,
            NotNull{&testArena},
            NotNull{&normalizer},
            NotNull{stack.back()},
            ice,
            call->location,
        };

        resolver.resolve(fnTy, &args, call->func, &argExprs);
        auto norm = normalizer.normalize(fnTy);
        if (!norm)
            reportError(NormalizationTooComplex{}, call->func->location);

        if (norm && norm->shouldSuppressErrors())
            return; // error suppressing function type!
        else if (!resolver.ok.empty())
            return; // We found a call that works, so this is ok.
        else if (!norm || !normalizer.isInhabited(norm))
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
                    if (p.first == FunctionCallResolver::TypeIsNotAFunction)
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

    struct FunctionCallResolver
    {
        enum Analysis
        {
            Ok,
            TypeIsNotAFunction,
            ArityMismatch,
            OverloadIsNonviable, // Arguments were incompatible with the overload's parameters, but were otherwise compatible by arity.
        };

        NotNull<BuiltinTypes> builtinTypes;
        NotNull<TypeArena> arena;
        NotNull<Normalizer> normalizer;
        NotNull<Scope> scope;
        NotNull<InternalErrorReporter> ice;
        Location callLoc;

        std::vector<TypeId> ok;
        std::vector<TypeId> nonFunctions;
        std::vector<std::pair<TypeId, ErrorVec>> arityMismatches;
        std::vector<std::pair<TypeId, ErrorVec>> nonviableOverloads;
        InsertionOrderedMap<TypeId, std::pair<Analysis, size_t>> resolution;

    private:
        std::optional<ErrorVec> tryUnify(const Location& location, TypeId subTy, TypeId superTy, const LiteralProperties* literalProperties = nullptr)
        {
            Unifier u{normalizer, scope, location, Covariant};
            u.ctx = CountMismatch::Arg;
            u.hideousFixMeGenericsAreActuallyFree = true;
            u.enableNewSolver();
            u.tryUnify(subTy, superTy, /*isFunctionCall*/ false, /*isIntersection*/ false, literalProperties);

            if (u.errors.empty())
                return std::nullopt;

            return std::move(u.errors);
        }

        std::optional<ErrorVec> tryUnify(const Location& location, TypePackId subTy, TypePackId superTy)
        {
            Unifier u{normalizer, scope, location, Covariant};
            u.ctx = CountMismatch::Arg;
            u.hideousFixMeGenericsAreActuallyFree = true;
            u.enableNewSolver();
            u.tryUnify(subTy, superTy);

            if (u.errors.empty())
                return std::nullopt;

            return std::move(u.errors);
        }

        std::pair<Analysis, ErrorVec> checkOverload(
            TypeId fnTy, const TypePack* args, AstExpr* fnLoc, const std::vector<AstExpr*>* argExprs, bool callMetamethodOk = true)
        {
            fnTy = follow(fnTy);

            ErrorVec discard;
            if (get<AnyType>(fnTy) || get<ErrorType>(fnTy) || get<NeverType>(fnTy))
                return {Ok, {}};
            else if (auto fn = get<FunctionType>(fnTy))
                return checkOverload_(fnTy, fn, args, fnLoc, argExprs); // Intentionally split to reduce the stack pressure of this function.
            else if (auto callMm = findMetatableEntry(builtinTypes, discard, fnTy, "__call", callLoc); callMm && callMetamethodOk)
            {
                // Calling a metamethod forwards the `fnTy` as self.
                TypePack withSelf = *args;
                withSelf.head.insert(withSelf.head.begin(), fnTy);

                std::vector<AstExpr*> withSelfExprs = *argExprs;
                withSelfExprs.insert(withSelfExprs.begin(), fnLoc);

                return checkOverload(*callMm, &withSelf, fnLoc, &withSelfExprs, /*callMetamethodOk=*/false);
            }
            else
                return {TypeIsNotAFunction, {}}; // Intentionally empty. We can just fabricate the type error later on.
        }

        static bool isLiteral(AstExpr* expr)
        {
            if (auto group = expr->as<AstExprGroup>())
                return isLiteral(group->expr);
            else if (auto assertion = expr->as<AstExprTypeAssertion>())
                return isLiteral(assertion->expr);

            return
                expr->is<AstExprConstantNil>() ||
                expr->is<AstExprConstantBool>() ||
                expr->is<AstExprConstantNumber>() ||
                expr->is<AstExprConstantString>() ||
                expr->is<AstExprFunction>() ||
                expr->is<AstExprTable>();
        }

        static std::unique_ptr<LiteralProperties> buildLiteralPropertiesSet(AstExpr* expr)
        {
            const AstExprTable* table = expr->as<AstExprTable>();
            if (!table)
                return nullptr;

            std::unique_ptr<LiteralProperties> result = std::make_unique<LiteralProperties>(Name{});

            for (const AstExprTable::Item& item : table->items)
            {
                if (item.kind != AstExprTable::Item::Record)
                    continue;

                AstExprConstantString* keyExpr = item.key->as<AstExprConstantString>();
                LUAU_ASSERT(keyExpr);

                if (isLiteral(item.value))
                    result->insert(Name{keyExpr->value.begin(), keyExpr->value.end()});
            }

            return result;
        }

        LUAU_NOINLINE
        std::pair<Analysis, ErrorVec> checkOverload_(
            TypeId fnTy, const FunctionType* fn, const TypePack* args, AstExpr* fnExpr, const std::vector<AstExpr*>* argExprs)
        {
            TxnLog fake;
            FamilyGraphReductionResult result = reduceFamilies(fnTy, callLoc, arena, builtinTypes, scope, normalizer, &fake, /*force=*/true);
            if (!result.errors.empty())
                return {OverloadIsNonviable, result.errors};

            ErrorVec argumentErrors;

            // Reminder: Functions have parameters. You provide arguments.
            auto paramIter = begin(fn->argTypes);
            size_t argOffset = 0;

            while (paramIter != end(fn->argTypes))
            {
                if (argOffset >= args->head.size())
                    break;

                TypeId paramTy = *paramIter;
                TypeId argTy = args->head[argOffset];
                AstExpr* argLoc = argExprs->at(argOffset >= argExprs->size() ? argExprs->size() - 1 : argOffset);

                std::unique_ptr<LiteralProperties> literalProperties{buildLiteralPropertiesSet(argLoc)};

                if (auto errors = tryUnify(argLoc->location, argTy, paramTy, literalProperties.get()))
                {
                    // Since we're stopping right here, we need to decide if this is a nonviable overload or if there is an arity mismatch.
                    // If it's a nonviable overload, then we need to keep going to get all type errors.
                    auto [minParams, optMaxParams] = getParameterExtents(TxnLog::empty(), fn->argTypes);
                    if (args->head.size() < minParams)
                        return {ArityMismatch, *errors};
                    else
                        argumentErrors.insert(argumentErrors.end(), errors->begin(), errors->end());
                }

                ++paramIter;
                ++argOffset;
            }

            while (argOffset < args->head.size())
            {
                // If we can iterate over the head of arguments, then we have exhausted the head of the parameters.
                LUAU_ASSERT(paramIter == end(fn->argTypes));

                AstExpr* argExpr = argExprs->at(argOffset >= argExprs->size() ? argExprs->size() - 1 : argOffset);

                if (!paramIter.tail())
                {
                    auto [minParams, optMaxParams] = getParameterExtents(TxnLog::empty(), fn->argTypes);
                    TypeError error{argExpr->location, CountMismatch{minParams, optMaxParams, args->head.size(), CountMismatch::Arg, false}};
                    return {ArityMismatch, {error}};
                }
                else if (auto vtp = get<VariadicTypePack>(follow(paramIter.tail())))
                {
                    if (auto errors = tryUnify(argExpr->location, args->head[argOffset], vtp->ty))
                        argumentErrors.insert(argumentErrors.end(), errors->begin(), errors->end());
                }
                else if (get<GenericTypePack>(follow(paramIter.tail())))
                    argumentErrors.push_back(TypeError{argExpr->location, TypePackMismatch{fn->argTypes, arena->addTypePack(*args)}});

                ++argOffset;
            }

            while (paramIter != end(fn->argTypes))
            {
                // If we can iterate over parameters, then we have exhausted the head of the arguments.
                LUAU_ASSERT(argOffset == args->head.size());

                // It may have a tail, however, so check that.
                if (auto vtp = get<VariadicTypePack>(follow(args->tail)))
                {
                    AstExpr* argExpr = argExprs->at(argExprs->size() - 1);

                    if (auto errors = tryUnify(argExpr->location, vtp->ty, *paramIter))
                        argumentErrors.insert(argumentErrors.end(), errors->begin(), errors->end());
                }
                else if (!isOptional(*paramIter))
                {
                    AstExpr* argExpr = argExprs->empty() ? fnExpr : argExprs->at(argExprs->size() - 1);

                    // It is ok to have excess parameters as long as they are all optional.
                    auto [minParams, optMaxParams] = getParameterExtents(TxnLog::empty(), fn->argTypes);
                    TypeError error{argExpr->location, CountMismatch{minParams, optMaxParams, args->head.size(), CountMismatch::Arg, false}};
                    return {ArityMismatch, {error}};
                }

                ++paramIter;
            }

            // We hit the end of the heads for both parameters and arguments, so check their tails.
            LUAU_ASSERT(paramIter == end(fn->argTypes));
            LUAU_ASSERT(argOffset == args->head.size());

            const Location argLoc = argExprs->empty() ? Location{} // TODO
                : argExprs->at(argExprs->size() - 1)->location;

            if (paramIter.tail() && args->tail)
            {
                if (auto errors = tryUnify(argLoc, *args->tail, *paramIter.tail()))
                    argumentErrors.insert(argumentErrors.end(), errors->begin(), errors->end());
            }
            else if (paramIter.tail())
            {
                const TypePackId paramTail = follow(*paramIter.tail());

                if (get<GenericTypePack>(paramTail))
                {
                    argumentErrors.push_back(TypeError{argLoc, TypePackMismatch{fn->argTypes, arena->addTypePack(*args)}});
                }
                else if (get<VariadicTypePack>(paramTail))
                {
                    // Nothing.  This is ok.
                }
            }

            return {argumentErrors.empty() ? Ok : OverloadIsNonviable, argumentErrors};
        }

        size_t indexof(Analysis analysis)
        {
            switch (analysis)
            {
            case Ok:
                return ok.size();
            case TypeIsNotAFunction:
                return nonFunctions.size();
            case ArityMismatch:
                return arityMismatches.size();
            case OverloadIsNonviable:
                return nonviableOverloads.size();
            }

            ice->ice("Inexhaustive switch in FunctionCallResolver::indexof");
        }

        void add(Analysis analysis, TypeId ty, ErrorVec&& errors)
        {
            resolution.insert(ty, {analysis, indexof(analysis)});

            switch (analysis)
            {
            case Ok:
                LUAU_ASSERT(errors.empty());
                ok.push_back(ty);
                break;
            case TypeIsNotAFunction:
                LUAU_ASSERT(errors.empty());
                nonFunctions.push_back(ty);
                break;
            case ArityMismatch:
                LUAU_ASSERT(!errors.empty());
                arityMismatches.emplace_back(ty, std::move(errors));
                break;
            case OverloadIsNonviable:
                LUAU_ASSERT(!errors.empty());
                nonviableOverloads.emplace_back(ty, std::move(errors));
                break;
            }
        }

    public:
        void resolve(TypeId fnTy, const TypePack* args, AstExpr* selfExpr, const std::vector<AstExpr*>* argExprs)
        {
            fnTy = follow(fnTy);

            auto it = get<IntersectionType>(fnTy);
            if (!it)
            {
                auto [analysis, errors] = checkOverload(fnTy, args, selfExpr, argExprs);
                add(analysis, fnTy, std::move(errors));
                return;
            }

            for (TypeId ty : it)
            {
                if (resolution.find(ty) != resolution.end())
                    continue;

                auto [analysis, errors] = checkOverload(ty, args, selfExpr, argExprs);
                add(analysis, ty, std::move(errors));
            }
        }
    };

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
            reportError(OptionalValueAccess{ty}, location);
            return follow(*strippedUnion);
        }

        return ty;
    }

    void visitExprName(AstExpr* expr, Location location, const std::string& propName, ValueContext context, TypeId astIndexExprTy)
    {
        visit(expr, ValueContext::RValue);
        TypeId leftType = stripFromNilAndReport(lookupType(expr), location);
        checkIndexTypeFromType(leftType, propName, location, context, astIndexExprTy);
    }

    void visit(AstExprIndexName* indexName, ValueContext context)
    {
        // If we're indexing like _.foo - foo could either be a prop or a string.
        visitExprName(indexName->expr, indexName->location, indexName->index.value, context, builtinTypes->stringType);
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

        NotNull<Scope> scope = stack.back();

        TypeId exprType = lookupType(indexExpr->expr);
        TypeId indexType = lookupType(indexExpr->index);

        if (auto tt = get<TableType>(exprType))
        {
            if (tt->indexer)
                reportErrors(tryUnify(scope, indexExpr->index->location, indexType, tt->indexer->indexType));
            else
                reportError(CannotExtendTable{exprType, CannotExtendTable::Indexer, "indexer??"}, indexExpr->location);
        }
        else if (auto cls = get<ClassType>(exprType); cls && cls->indexer)
            reportErrors(tryUnify(scope, indexExpr->index->location, indexType, cls->indexer->indexType));
        else if (get<UnionType>(exprType) && isOptional(exprType))
            reportError(OptionalValueAccess{exprType}, indexExpr->location);
    }

    void visit(AstExprFunction* fn)
    {
        auto StackPusher = pushStack(fn);

        visitGenerics(fn->generics, fn->genericPacks);

        TypeId inferredFnTy = lookupType(fn);
        functionDeclStack.push_back(inferredFnTy);

        const NormalizedType* normalizedFnTy = normalizer.normalize(inferredFnTy);
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

                if (arg->annotation)
                {
                    TypeId inferredArgTy = *argIt;
                    TypeId annotatedArgTy = lookupAnnotation(arg->annotation);

                    if (!isSubtype(inferredArgTy, annotatedArgTy, stack.back()) &&
                        !isErrorSuppressing(arg->location, inferredArgTy, arg->annotation->location, annotatedArgTy))
                    {
                        reportError(TypeMismatch{inferredArgTy, annotatedArgTy}, arg->location);
                    }
                }

                ++argIt;
            }
        }

        visit(fn->body);

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

        NotNull<Scope> scope = stack.back();
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
                            reportErrors(tryUnify(scope, expr->location, follow(*ret), builtinTypes->numberType));
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

                    TypePackId expectedArgs = testArena.addTypePack({operandType});
                    TypePackId expectedRet = testArena.addTypePack({resultType});

                    TypeId expectedFunction = testArena.addType(FunctionType{expectedArgs, expectedRet});

                    ErrorVec errors = tryUnify(scope, expr->location, *mm, expectedFunction);
                    if (!errors.empty() && !isErrorSuppressing(expr->expr->location, *firstArg, expr->expr->location, operandType))
                    {
                        reportError(TypeMismatch{*firstArg, operandType}, expr->location);
                        return;
                    }
                }

                return;
            }
        }

        if (expr->op == AstExprUnary::Op::Len)
        {
            DenseHashSet<TypeId> seen{nullptr};
            int recursionCount = 0;
            const NormalizedType* nty = normalizer.normalize(operandType);

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
            reportErrors(tryUnify(scope, expr->location, operandType, builtinTypes->numberType));
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
        visit(expr->left, ValueContext::LValue);
        visit(expr->right, ValueContext::LValue);

        NotNull<Scope> scope = stack.back();

        bool isEquality = expr->op == AstExprBinary::Op::CompareEq || expr->op == AstExprBinary::Op::CompareNe;
        bool isComparison = expr->op >= AstExprBinary::Op::CompareEq && expr->op <= AstExprBinary::Op::CompareGe;
        bool isLogical = expr->op == AstExprBinary::Op::And || expr->op == AstExprBinary::Op::Or;

        TypeId leftType = lookupType(expr->left);
        TypeId rightType = lookupType(expr->right);
        TypeId expectedResult = lookupType(expr);

        if (get<TypeFamilyInstanceType>(expectedResult))
        {
            checkForInternalFamily(expectedResult, expr->location);
            return expectedResult;
        }

        if (expr->op == AstExprBinary::Op::Or)
        {
            leftType = stripNil(builtinTypes, testArena, leftType);
        }

        const NormalizedType* normLeft = normalizer.normalize(leftType);
        const NormalizedType* normRight = normalizer.normalize(rightType);

        bool isStringOperation =
            (normLeft ? normLeft->isSubtypeOfString() : isString(leftType)) && (normRight ? normRight->isSubtypeOfString() : isString(rightType));

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

        bool typesHaveIntersection = normalizer.isIntersectionInhabited(leftType, rightType);
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
            // checking if the intersection of the types is inhabited.
            // TODO: Maybe add more checks here (e.g. for functions, classes, etc)
            if (!(get<TableType>(leftType) || get<TableType>(rightType)))
                if (!leftMt.has_value() || !rightMt.has_value())
                    matches = matches || typesHaveIntersection;

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
                    // was handled by a type family
                    return expectedResult;
                }

                else if (const FunctionType* ftv = get<FunctionType>(follow(*selectedOverloadTy)))
                {
                    TypePackId expectedArgs;
                    // For >= and > we invoke __lt and __le respectively with
                    // swapped argument ordering.
                    if (expr->op == AstExprBinary::Op::CompareGe || expr->op == AstExprBinary::Op::CompareGt)
                    {
                        expectedArgs = testArena.addTypePack({rightType, leftType});
                    }
                    else
                    {
                        expectedArgs = testArena.addTypePack({leftType, rightType});
                    }

                    TypePackId expectedRets;
                    if (expr->op == AstExprBinary::CompareEq || expr->op == AstExprBinary::CompareNe || expr->op == AstExprBinary::CompareGe ||
                        expr->op == AstExprBinary::CompareGt || expr->op == AstExprBinary::Op::CompareLe || expr->op == AstExprBinary::Op::CompareLt)
                    {
                        expectedRets = testArena.addTypePack({builtinTypes->booleanType});
                    }
                    else
                    {
                        expectedRets = testArena.addTypePack({testArena.freshType(scope, TypeLevel{})});
                    }

                    TypeId expectedTy = testArena.addType(FunctionType(expectedArgs, expectedRets));

                    reportErrors(tryUnify(scope, expr->location, follow(*mm), expectedTy));

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
        case AstExprBinary::Op::Pow:
        case AstExprBinary::Op::Mod:
            reportErrors(tryUnify(scope, expr->left->location, leftType, builtinTypes->numberType));
            reportErrors(tryUnify(scope, expr->right->location, rightType, builtinTypes->numberType));

            return builtinTypes->numberType;
        case AstExprBinary::Op::Concat:
            reportErrors(tryUnify(scope, expr->left->location, leftType, builtinTypes->stringType));
            reportErrors(tryUnify(scope, expr->right->location, rightType, builtinTypes->stringType));

            return builtinTypes->stringType;
        case AstExprBinary::Op::CompareGe:
        case AstExprBinary::Op::CompareGt:
        case AstExprBinary::Op::CompareLe:
        case AstExprBinary::Op::CompareLt:
        {
            if (normLeft && normLeft->shouldSuppressErrors())
                return builtinTypes->numberType;

            if (normLeft && normLeft->isExactlyNumber())
            {
                reportErrors(tryUnify(scope, expr->right->location, rightType, builtinTypes->numberType));
                return builtinTypes->numberType;
            }
            else if (normLeft && normLeft->isSubtypeOfString())
            {
                reportErrors(tryUnify(scope, expr->right->location, rightType, builtinTypes->stringType));
                return builtinTypes->stringType;
            }
            else
            {
                reportError(GenericError{format("Types '%s' and '%s' cannot be compared with relational operator %s", toString(leftType).c_str(),
                                toString(rightType).c_str(), toString(expr->op).c_str())},
                    expr->location);
                return builtinTypes->errorRecoveryType();
            }
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

        // Note: As an optimization, we try 'number <: number | string' first, as that is the more likely case.
        if (isSubtype(annotationType, computedType, stack.back(), true))
            return;

        if (isSubtype(computedType, annotationType, stack.back(), true))
            return;

        reportError(TypesAreUnrelated{computedType, annotationType}, expr->location);
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
            TypeId result = testArena.addType(FreeType{ftp->scope});
            TypePackId freeTail = testArena.addTypePack(FreeTypePack{ftp->scope});

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
            checkForFamilyInhabitance(follow(*resolvedTy), ty->location);

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
                    TypePackId tp = lookupPackAnnotation(p.typePack);

                    if (typesProvided < typesRequired && size(tp) == 1 && finite(tp) && first(tp))
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

    template<typename TID>
    bool isSubtype(TID subTy, TID superTy, NotNull<Scope> scope, bool genericsOkay = false)
    {
        TypeArena arena;
        Unifier u{NotNull{&normalizer}, scope, Location{}, Covariant};
        u.hideousFixMeGenericsAreActuallyFree = genericsOkay;
        u.enableNewSolver();

        u.tryUnify(subTy, superTy);
        const bool ok = u.errors.empty() && u.log.empty();
        return ok;
    }

    template<typename TID>
    ErrorVec tryUnify(NotNull<Scope> scope, const Location& location, TID subTy, TID superTy, CountMismatch::Context context = CountMismatch::Arg,
        bool genericsOkay = false)
    {
        Unifier u{NotNull{&normalizer}, scope, location, Covariant};
        u.ctx = context;
        u.hideousFixMeGenericsAreActuallyFree = genericsOkay;
        u.enableNewSolver();
        u.tryUnify(subTy, superTy);

        if (isErrorSuppressing(location, subTy, location, superTy))
            return {};

        return std::move(u.errors);
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

    // If the provided type does not have the named property, report an error.
    void checkIndexTypeFromType(TypeId tableTy, const std::string& prop, const Location& location, ValueContext context, TypeId astIndexExprType)
    {
        const NormalizedType* norm = normalizer.normalize(tableTy);
        if (!norm)
        {
            reportError(NormalizationTooComplex{}, location);
            return;
        }

        // if the type is error suppressing, we don't actually have any work left to do.
        if (norm->shouldSuppressErrors())
            return;

        bool foundOneProp = false;
        std::vector<TypeId> typesMissingTheProp;

        auto fetch = [&](TypeId ty) {
            if (!normalizer.isInhabited(ty))
                return;

            std::unordered_set<TypeId> seen;
            bool found = hasIndexTypeFromType(ty, prop, location, seen, astIndexExprType);
            foundOneProp |= found;
            if (!found)
                typesMissingTheProp.push_back(ty);
        };

        fetch(norm->tops);
        fetch(norm->booleans);

        for (const auto& [ty, _negations] : norm->classes.classes)
        {
            fetch(ty);
        }
        fetch(norm->errors);
        fetch(norm->nils);
        fetch(norm->numbers);
        if (!norm->strings.isNever())
            fetch(builtinTypes->stringType);
        fetch(norm->threads);
        for (TypeId ty : norm->tables)
            fetch(ty);
        if (norm->functions.isTop)
            fetch(builtinTypes->functionType);
        else if (!norm->functions.isNever())
        {
            if (norm->functions.parts.size() == 1)
                fetch(norm->functions.parts.front());
            else
            {
                std::vector<TypeId> parts;
                parts.insert(parts.end(), norm->functions.parts.begin(), norm->functions.parts.end());
                fetch(testArena.addType(IntersectionType{std::move(parts)}));
            }
        }
        for (const auto& [tyvar, intersect] : norm->tyvars)
        {
            if (get<NeverType>(intersect->tops))
            {
                TypeId ty = normalizer.typeFromNormal(*intersect);
                fetch(testArena.addType(IntersectionType{{tyvar, ty}}));
            }
            else
                fetch(tyvar);
        }

        if (!typesMissingTheProp.empty())
        {
            if (foundOneProp)
                reportError(MissingUnionProperty{tableTy, typesMissingTheProp, prop}, location);
            // For class LValues, we don't want to report an extension error,
            // because classes come into being with full knowledge of their
            // shape. We instead want to report the unknown property error of
            // the `else` branch.
            else if (context == ValueContext::LValue && !get<ClassType>(tableTy))
                reportError(CannotExtendTable{tableTy, CannotExtendTable::Property, prop}, location);
            else
                reportError(UnknownProperty{tableTy, prop}, location);
        }
    }

    bool hasIndexTypeFromType(TypeId ty, const std::string& prop, const Location& location, std::unordered_set<TypeId>& seen, TypeId astIndexExprType)
    {
        // If we have already encountered this type, we must assume that some
        // other codepath will do the right thing and signal false if the
        // property is not present.
        const bool isUnseen = seen.insert(ty).second;
        if (!isUnseen)
            return true;

        if (get<ErrorType>(ty) || get<AnyType>(ty) || get<NeverType>(ty))
            return true;

        if (isString(ty))
        {
            std::optional<TypeId> mtIndex = Luau::findMetatableEntry(builtinTypes, module->errors, builtinTypes->stringType, "__index", location);
            LUAU_ASSERT(mtIndex);
            ty = *mtIndex;
        }

        if (auto tt = getTableType(ty))
        {
            if (findTablePropertyRespectingMeta(builtinTypes, module->errors, ty, prop, location))
                return true;

            if (tt->indexer)
            {
                TypeId indexType = follow(tt->indexer->indexType);
                if (isPrim(indexType, PrimitiveType::String))
                    return true;
                // If the indexer looks like { [any] : _} - the prop lookup should be allowed!
                else if (get<AnyType>(indexType) || get<UnknownType>(indexType))
                    return true;
            }

            return false;
        }
        else if (const ClassType* cls = get<ClassType>(ty))
        {
            // If the property doesn't exist on the class, we consult the indexer
            // We need to check if the type of the index expression foo (x[foo])
            // is compatible with the indexer's indexType
            // Construct the intersection and test inhabitedness!
            if (auto property = lookupClassProp(cls, prop))
                return true;
            if (cls->indexer)
            {
                TypeId inhabitatedTestType = testArena.addType(IntersectionType{{cls->indexer->indexType, astIndexExprType}});
                return normalizer.isInhabited(inhabitatedTestType);
            }
            return false;
        }
        else if (const UnionType* utv = get<UnionType>(ty))
            return std::all_of(begin(utv), end(utv), [&](TypeId part) {
                return hasIndexTypeFromType(part, prop, location, seen, astIndexExprType);
            });
        else if (const IntersectionType* itv = get<IntersectionType>(ty))
            return std::any_of(begin(itv), end(itv), [&](TypeId part) {
                return hasIndexTypeFromType(part, prop, location, seen, astIndexExprType);
            });
        else
            return false;
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

void check(
    NotNull<BuiltinTypes> builtinTypes, NotNull<UnifierSharedState> unifierState, DcrLogger* logger, const SourceModule& sourceModule, Module* module)
{
    TypeChecker2 typeChecker{builtinTypes, unifierState, logger, &sourceModule, module};

    typeChecker.visit(sourceModule.root);

    unfreeze(module->interfaceTypes);
    copyErrors(module->errors, module->interfaceTypes);
    freeze(module->interfaceTypes);
}

} // namespace Luau
