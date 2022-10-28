// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeChecker2.h"

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Clone.h"
#include "Luau/Instantiation.h"
#include "Luau/Metamethods.h"
#include "Luau/Normalize.h"
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeUtils.h"
#include "Luau/TypeVar.h"
#include "Luau/Unifier.h"
#include "Luau/ToString.h"
#include "Luau/DcrLogger.h"

#include <algorithm>

LUAU_FASTFLAG(DebugLuauLogSolverToJson);
LUAU_FASTFLAG(DebugLuauMagicTypes);

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

struct TypeChecker2
{
    NotNull<SingletonTypes> singletonTypes;
    DcrLogger* logger;
    InternalErrorReporter ice; // FIXME accept a pointer from Frontend
    const SourceModule* sourceModule;
    Module* module;

    std::vector<NotNull<Scope>> stack;

    TypeChecker2(NotNull<SingletonTypes> singletonTypes, DcrLogger* logger, const SourceModule* sourceModule, Module* module)
        : singletonTypes(singletonTypes)
        , logger(logger)
        , sourceModule(sourceModule)
        , module(module)
    {
        if (FFlag::DebugLuauLogSolverToJson)
            LUAU_ASSERT(logger);
    }

    std::optional<StackPusher> pushStack(AstNode* node)
    {
        if (Scope** scope = module->astScopes.find(node))
            return StackPusher{stack, *scope};
        else
            return std::nullopt;
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
            return singletonTypes->anyTypePack;
    }

    TypeId lookupType(AstExpr* expr)
    {
        // If a type isn't in the type graph, it probably means that a recursion limit was exceeded.
        // We'll just return anyType in these cases.  Typechecking against any is very fast and this
        // allows us not to think about this very much in the actual typechecking logic.
        TypeId* ty = module->astTypes.find(expr);
        if (ty)
            return follow(*ty);

        TypePackId* tp = module->astTypePacks.find(expr);
        if (tp)
            return flattenPack(*tp);

        return singletonTypes->anyType;
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
        return follow(*ty);
    }

    TypePackId lookupPackAnnotation(AstTypePack* annotation)
    {
        TypePackId* tp = module->astResolvedTypePacks.find(annotation);
        LUAU_ASSERT(tp);
        return follow(*tp);
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
            else if (scopeBounds.begin > location.end)
            {
                // TODO: Is this sound? This relies on the fact that scopes are inserted
                // into the scope list in the order that they appear in the AST.
                break;
            }
        }

        return bestScope;
    }

    void visit(AstStat* stat)
    {
        auto pusher = pushStack(stat);

        if (0)
        {
        }
        else if (auto s = stat->as<AstStatBlock>())
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
        visit(ifStatement->condition);
        visit(ifStatement->thenbody);
        if (ifStatement->elsebody)
            visit(ifStatement->elsebody);
    }

    void visit(AstStatWhile* whileStatement)
    {
        visit(whileStatement->condition);
        visit(whileStatement->body);
    }

    void visit(AstStatRepeat* repeatStatement)
    {
        visit(repeatStatement->body);
        visit(repeatStatement->condition);
    }

    void visit(AstStatBreak*) {}

    void visit(AstStatContinue*) {}

    void visit(AstStatReturn* ret)
    {
        Scope* scope = findInnermostScope(ret->location);
        TypePackId expectedRetType = scope->returnType;

        TypeArena arena;
        TypePackId actualRetType = reconstructPack(ret->list, arena);

        UnifierSharedState sharedState{&ice};
        Normalizer normalizer{&arena, singletonTypes, NotNull{&sharedState}};
        Unifier u{NotNull{&normalizer}, Mode::Strict, stack.back(), ret->location, Covariant};
        u.anyIsTop = true;

        u.tryUnify(actualRetType, expectedRetType);
        const bool ok = u.errors.empty() && u.log.empty();

        if (!ok)
        {
            for (const TypeError& e : u.errors)
                reportError(e);
        }

        for (AstExpr* expr : ret->list)
            visit(expr);
    }

    void visit(AstStatExpr* expr)
    {
        visit(expr->expr);
    }

    void visit(AstStatLocal* local)
    {
        size_t count = std::max(local->values.size, local->vars.size);
        for (size_t i = 0; i < count; ++i)
        {
            AstExpr* value = i < local->values.size ? local->values.data[i] : nullptr;

            if (value)
                visit(value);

            if (i != local->values.size - 1)
            {
                AstLocal* var = i < local->vars.size ? local->vars.data[i] : nullptr;

                if (var && var->annotation)
                {
                    TypeId varType = lookupAnnotation(var->annotation);
                    TypeId valueType = value ? lookupType(value) : nullptr;
                    if (valueType && !isSubtype(varType, valueType, stack.back(), singletonTypes, ice, /* anyIsTop */ false))
                        reportError(TypeMismatch{varType, valueType}, value->location);
                }
            }
            else
            {
                LUAU_ASSERT(value);

                TypePackId valueTypes = lookupPack(value);
                auto it = begin(valueTypes);
                for (size_t j = i; j < local->vars.size; ++j)
                {
                    if (it == end(valueTypes))
                    {
                        break;
                    }

                    AstLocal* var = local->vars.data[i];
                    if (var->annotation)
                    {
                        TypeId varType = lookupAnnotation(var->annotation);
                        ErrorVec errors = tryUnify(stack.back(), value->location, *it, varType);
                        if (!errors.empty())
                            reportErrors(std::move(errors));
                    }

                    ++it;
                }
            }
        }
    }

    void visit(AstStatFor* forStatement)
    {
        if (forStatement->var->annotation)
            visit(forStatement->var->annotation);

        visit(forStatement->from);
        visit(forStatement->to);
        if (forStatement->step)
            visit(forStatement->step);
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
            visit(expr);

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

        // ugh.  There's nothing in the AST to hang a whole type pack on for the
        // set of iteratees, so we have to piece it back together by hand.
        std::vector<TypeId> valueTypes;
        for (size_t i = 0; i < forInStatement->values.size - 1; ++i)
            valueTypes.emplace_back(lookupType(forInStatement->values.data[i]));
        TypePackId iteratorTail = lookupPack(forInStatement->values.data[forInStatement->values.size - 1]);
        TypePackId iteratorPack = arena.addTypePack(valueTypes, iteratorTail);

        // ... and then expand it out to 3 values (if possible)
        const std::vector<TypeId> iteratorTypes = flatten(arena, singletonTypes, iteratorPack, 3);
        if (iteratorTypes.empty())
        {
            reportError(GenericError{"for..in loops require at least one value to iterate over.  Got zero"}, getLocation(forInStatement->values));
            return;
        }
        TypeId iteratorTy = follow(iteratorTypes[0]);

        auto checkFunction = [this, &arena, &scope, &forInStatement, &variableTypes](
                                 const FunctionTypeVar* iterFtv, std::vector<TypeId> iterTys, bool isMm) {
            if (iterTys.size() < 1 || iterTys.size() > 3)
            {
                if (isMm)
                    reportError(GenericError{"__iter metamethod must return (next[, table[, state]])"}, getLocation(forInStatement->values));
                else
                    reportError(GenericError{"for..in loops must be passed (next[, table[, state]])"}, getLocation(forInStatement->values));

                return;
            }

            // It is okay if there aren't enough iterators, but the iteratee must provide enough.
            std::vector<TypeId> expectedVariableTypes = flatten(arena, singletonTypes, iterFtv->retTypes, variableTypes.size());
            if (expectedVariableTypes.size() < variableTypes.size())
            {
                if (isMm)
                    reportError(
                        GenericError{"__iter metamethod's next() function does not return enough values"}, getLocation(forInStatement->values));
                else
                    reportError(GenericError{"next() does not return enough values"}, forInStatement->values.data[0]->location);
            }

            for (size_t i = 0; i < std::min(expectedVariableTypes.size(), variableTypes.size()); ++i)
                reportErrors(tryUnify(scope, forInStatement->vars.data[i]->location, variableTypes[i], expectedVariableTypes[i]));

            // nextFn is going to be invoked with (arrayTy, startIndexTy)

            // It will be passed two arguments on every iteration save the
            // first.

            // It may be invoked with 0 or 1 argument on the first iteration.
            // This depends on the types in iterateePack and therefore
            // iteratorTypes.

            // If iteratorTypes is too short to be a valid call to nextFn, we have to report a count mismatch error.
            // If 2 is too short to be a valid call to nextFn, we have to report a count mismatch error.
            // If 2 is too long to be a valid call to nextFn, we have to report a count mismatch error.
            auto [minCount, maxCount] = getParameterExtents(TxnLog::empty(), iterFtv->argTypes, /*includeHiddenVariadics*/ true);

            if (minCount > 2)
                reportError(CountMismatch{2, std::nullopt, minCount, CountMismatch::Arg}, forInStatement->vars.data[0]->location);
            if (maxCount && *maxCount < 2)
                reportError(CountMismatch{2, std::nullopt, *maxCount, CountMismatch::Arg}, forInStatement->vars.data[0]->location);

            const std::vector<TypeId> flattenedArgTypes = flatten(arena, singletonTypes, iterFtv->argTypes, 2);
            size_t firstIterationArgCount = iterTys.empty() ? 0 : iterTys.size() - 1;
            size_t actualArgCount = expectedVariableTypes.size();

            if (firstIterationArgCount < minCount)
                reportError(CountMismatch{2, std::nullopt, firstIterationArgCount, CountMismatch::Arg}, forInStatement->vars.data[0]->location);
            else if (actualArgCount < minCount)
                reportError(CountMismatch{2, std::nullopt, actualArgCount, CountMismatch::Arg}, forInStatement->vars.data[0]->location);

            if (iterTys.size() >= 2 && flattenedArgTypes.size() > 0)
            {
                size_t valueIndex = forInStatement->values.size > 1 ? 1 : 0;
                reportErrors(tryUnify(scope, forInStatement->values.data[valueIndex]->location, iterTys[1], flattenedArgTypes[0]));
            }

            if (iterTys.size() == 3 && flattenedArgTypes.size() > 1)
            {
                size_t valueIndex = forInStatement->values.size > 2 ? 2 : 0;
                reportErrors(tryUnify(scope, forInStatement->values.data[valueIndex]->location, iterTys[2], flattenedArgTypes[1]));
            }
        };

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
        if (const FunctionTypeVar* nextFn = get<FunctionTypeVar>(iteratorTy))
        {
            checkFunction(nextFn, iteratorTypes, false);
        }
        else if (const TableTypeVar* ttv = get<TableTypeVar>(iteratorTy))
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
        else if (get<AnyTypeVar>(iteratorTy) || get<ErrorTypeVar>(iteratorTy))
        {
            // nothing
        }
        else if (std::optional<TypeId> iterMmTy =
                     findMetatableEntry(singletonTypes, module->errors, iteratorTy, "__iter", forInStatement->values.data[0]->location))
        {
            Instantiation instantiation{TxnLog::empty(), &arena, TypeLevel{}, scope};

            if (std::optional<TypeId> instantiatedIterMmTy = instantiation.substitute(*iterMmTy))
            {
                if (const FunctionTypeVar* iterMmFtv = get<FunctionTypeVar>(*instantiatedIterMmTy))
                {
                    TypePackId argPack = arena.addTypePack({iteratorTy});
                    reportErrors(tryUnify(scope, forInStatement->values.data[0]->location, argPack, iterMmFtv->argTypes));

                    std::vector<TypeId> mmIteratorTypes = flatten(arena, singletonTypes, iterMmFtv->retTypes, 3);

                    if (mmIteratorTypes.size() == 0)
                    {
                        reportError(GenericError{"__iter must return at least one value"}, forInStatement->values.data[0]->location);
                        return;
                    }

                    TypeId nextFn = follow(mmIteratorTypes[0]);

                    if (std::optional<TypeId> instantiatedNextFn = instantiation.substitute(nextFn))
                    {
                        std::vector<TypeId> instantiatedIteratorTypes = mmIteratorTypes;
                        instantiatedIteratorTypes[0] = *instantiatedNextFn;

                        if (const FunctionTypeVar* nextFtv = get<FunctionTypeVar>(*instantiatedNextFn))
                        {
                            checkFunction(nextFtv, instantiatedIteratorTypes, true);
                        }
                        else
                        {
                            reportError(CannotCallNonFunction{*instantiatedNextFn}, forInStatement->values.data[0]->location);
                        }
                    }
                    else
                    {
                        reportError(UnificationTooComplex{}, forInStatement->values.data[0]->location);
                    }
                }
                else
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
        else
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
            visit(lhs);
            TypeId lhsType = lookupType(lhs);

            AstExpr* rhs = assign->values.data[i];
            visit(rhs);
            TypeId rhsType = lookupType(rhs);

            if (!isSubtype(rhsType, lhsType, stack.back(), singletonTypes, ice, /* anyIsTop */ false))
            {
                reportError(TypeMismatch{lhsType, rhsType}, rhs->location);
            }
        }
    }

    void visit(AstStatCompoundAssign* stat)
    {
        visit(stat->var);
        visit(stat->value);
    }

    void visit(AstStatFunction* stat)
    {
        visit(stat->name);
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
        for (const AstGenericType& el : stat->generics)
        {
            if (el.defaultValue)
                visit(el.defaultValue);
        }

        for (const AstGenericTypePack& el : stat->genericPacks)
        {
            if (el.defaultValue)
                visit(el.defaultValue);
        }

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
            visit(expr);

        for (AstStat* s : stat->statements)
            visit(s);
    }

    void visit(AstExpr* expr)
    {
        auto StackPusher = pushStack(expr);

        if (0)
        {
        }
        else if (auto e = expr->as<AstExprGroup>())
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
        else if (auto e = expr->as<AstExprError>())
            return visit(e);
        else
            LUAU_ASSERT(!"TypeChecker2 encountered an unknown expression type");
    }

    void visit(AstExprGroup* expr)
    {
        visit(expr->expr);
    }

    void visit(AstExprConstantNil* expr)
    {
        // TODO!
    }

    void visit(AstExprConstantBool* expr)
    {
        // TODO!
    }

    void visit(AstExprConstantNumber* number)
    {
        TypeId actualType = lookupType(number);
        TypeId numberType = singletonTypes->numberType;

        if (!isSubtype(numberType, actualType, stack.back(), singletonTypes, ice, /* anyIsTop */ false))
        {
            reportError(TypeMismatch{actualType, numberType}, number->location);
        }
    }

    void visit(AstExprConstantString* string)
    {
        TypeId actualType = lookupType(string);
        TypeId stringType = singletonTypes->stringType;

        if (!isSubtype(actualType, stringType, stack.back(), singletonTypes, ice, /* anyIsTop */ false))
        {
            reportError(TypeMismatch{actualType, stringType}, string->location);
        }
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

    void visit(AstExprCall* call)
    {
        visit(call->func);

        for (AstExpr* arg : call->args)
            visit(arg);

        TypeArena arena;
        Instantiation instantiation{TxnLog::empty(), &arena, TypeLevel{}, stack.back()};

        TypePackId expectedRetType = lookupPack(call);
        TypeId functionType = lookupType(call->func);
        TypeId testFunctionType = functionType;
        TypePack args;

        if (get<AnyTypeVar>(functionType) || get<ErrorTypeVar>(functionType))
            return;
        else if (std::optional<TypeId> callMm = findMetatableEntry(singletonTypes, module->errors, functionType, "__call", call->func->location))
        {
            if (get<FunctionTypeVar>(follow(*callMm)))
            {
                if (std::optional<TypeId> instantiatedCallMm = instantiation.substitute(*callMm))
                {
                    args.head.push_back(functionType);
                    testFunctionType = follow(*instantiatedCallMm);
                }
                else
                {
                    reportError(UnificationTooComplex{}, call->func->location);
                    return;
                }
            }
            else
            {
                // TODO: This doesn't flag the __call metamethod as the problem
                // very clearly.
                reportError(CannotCallNonFunction{*callMm}, call->func->location);
                return;
            }
        }
        else if (get<FunctionTypeVar>(functionType))
        {
            if (std::optional<TypeId> instantiatedFunctionType = instantiation.substitute(functionType))
            {
                testFunctionType = *instantiatedFunctionType;
            }
            else
            {
                reportError(UnificationTooComplex{}, call->func->location);
                return;
            }
        }
        else
        {
            reportError(CannotCallNonFunction{functionType}, call->func->location);
            return;
        }

        for (AstExpr* arg : call->args)
        {
            TypeId argTy = lookupType(arg);
            args.head.push_back(argTy);
        }

        TypePackId argsTp = arena.addTypePack(args);
        FunctionTypeVar ftv{argsTp, expectedRetType};
        TypeId expectedType = arena.addType(ftv);

        if (!isSubtype(testFunctionType, expectedType, stack.back(), singletonTypes, ice, /* anyIsTop */ false))
        {
            CloneState cloneState;
            expectedType = clone(expectedType, module->internalTypes, cloneState);
            reportError(TypeMismatch{expectedType, functionType}, call->location);
        }
    }

    void visit(AstExprIndexName* indexName)
    {
        TypeId leftType = lookupType(indexName->expr);
        TypeId resultType = lookupType(indexName);

        // leftType must have a property called indexName->index

        std::optional<TypeId> ty =
            getIndexTypeFromType(module->getModuleScope(), leftType, indexName->index.value, indexName->location, /* addErrors */ true);
        if (ty)
        {
            if (!isSubtype(resultType, *ty, stack.back(), singletonTypes, ice, /* anyIsTop */ false))
            {
                reportError(TypeMismatch{resultType, *ty}, indexName->location);
            }
        }
    }

    void visit(AstExprIndexExpr* indexExpr)
    {
        // TODO!
        visit(indexExpr->expr);
        visit(indexExpr->index);
    }

    void visit(AstExprFunction* fn)
    {
        auto StackPusher = pushStack(fn);

        TypeId inferredFnTy = lookupType(fn);
        const FunctionTypeVar* inferredFtv = get<FunctionTypeVar>(inferredFnTy);
        LUAU_ASSERT(inferredFtv);

        auto argIt = begin(inferredFtv->argTypes);
        for (const auto& arg : fn->args)
        {
            if (argIt == end(inferredFtv->argTypes))
                break;

            if (arg->annotation)
            {
                TypeId inferredArgTy = *argIt;
                TypeId annotatedArgTy = lookupAnnotation(arg->annotation);

                if (!isSubtype(annotatedArgTy, inferredArgTy, stack.back(), singletonTypes, ice, /* anyIsTop */ false))
                {
                    reportError(TypeMismatch{annotatedArgTy, inferredArgTy}, arg->location);
                }
            }

            ++argIt;
        }

        visit(fn->body);
    }

    void visit(AstExprTable* expr)
    {
        // TODO!
        for (const AstExprTable::Item& item : expr->items)
        {
            if (item.key)
                visit(item.key);
            visit(item.value);
        }
    }

    void visit(AstExprUnary* expr)
    {
        visit(expr->expr);

        NotNull<Scope> scope = stack.back();
        TypeId operandType = lookupType(expr->expr);

        if (get<AnyTypeVar>(operandType) || get<ErrorTypeVar>(operandType) || get<NeverTypeVar>(operandType))
            return;

        if (auto it = kUnaryOpMetamethods.find(expr->op); it != kUnaryOpMetamethods.end())
        {
            std::optional<TypeId> mm = findMetatableEntry(singletonTypes, module->errors, operandType, it->second, expr->location);
            if (mm)
            {
                if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(follow(*mm)))
                {
                    TypePackId expectedArgs = module->internalTypes.addTypePack({operandType});
                    reportErrors(tryUnify(scope, expr->location, ftv->argTypes, expectedArgs));

                    if (std::optional<TypeId> ret = first(ftv->retTypes))
                    {
                        if (expr->op == AstExprUnary::Op::Len)
                        {
                            reportErrors(tryUnify(scope, expr->location, follow(*ret), singletonTypes->numberType));
                        }
                    }
                    else
                    {
                        reportError(GenericError{format("Metamethod '%s' must return a value", it->second)}, expr->location);
                    }
                }

                return;
            }
        }

        if (expr->op == AstExprUnary::Op::Len)
        {
            DenseHashSet<TypeId> seen{nullptr};
            int recursionCount = 0;

            if (!hasLength(operandType, seen, &recursionCount))
            {
                reportError(NotATable{operandType}, expr->location);
            }
        }
        else if (expr->op == AstExprUnary::Op::Minus)
        {
            reportErrors(tryUnify(scope, expr->location, operandType, singletonTypes->numberType));
        }
        else if (expr->op == AstExprUnary::Op::Not)
        {
        }
        else
        {
            LUAU_ASSERT(!"Unhandled unary operator");
        }
    }

    void visit(AstExprBinary* expr)
    {
        visit(expr->left);
        visit(expr->right);

        NotNull<Scope> scope = stack.back();

        bool isEquality = expr->op == AstExprBinary::Op::CompareEq || expr->op == AstExprBinary::Op::CompareNe;
        bool isComparison = expr->op >= AstExprBinary::Op::CompareEq && expr->op <= AstExprBinary::Op::CompareGe;
        bool isLogical = expr->op == AstExprBinary::Op::And || expr->op == AstExprBinary::Op::Or;

        TypeId leftType = lookupType(expr->left);
        TypeId rightType = lookupType(expr->right);

        if (expr->op == AstExprBinary::Op::Or)
        {
            leftType = stripNil(singletonTypes, module->internalTypes, leftType);
        }

        bool isStringOperation = isString(leftType) && isString(rightType);

        if (get<AnyTypeVar>(leftType) || get<ErrorTypeVar>(leftType) || get<AnyTypeVar>(rightType) || get<ErrorTypeVar>(rightType))
            return;

        if ((get<BlockedTypeVar>(leftType) || get<FreeTypeVar>(leftType)) && !isEquality && !isLogical)
        {
            auto name = getIdentifierOfBaseVar(expr->left);
            reportError(CannotInferBinaryOperation{expr->op, name,
                            isComparison ? CannotInferBinaryOperation::OpKind::Comparison : CannotInferBinaryOperation::OpKind::Operation},
                expr->location);
            return;
        }

        if (auto it = kBinaryOpMetamethods.find(expr->op); it != kBinaryOpMetamethods.end())
        {
            std::optional<TypeId> leftMt = getMetatable(leftType, singletonTypes);
            std::optional<TypeId> rightMt = getMetatable(rightType, singletonTypes);

            bool matches = leftMt == rightMt;
            if (isEquality && !matches)
            {
                auto testUnion = [&matches, singletonTypes = this->singletonTypes](const UnionTypeVar* utv, std::optional<TypeId> otherMt) {
                    for (TypeId option : utv)
                    {
                        if (getMetatable(follow(option), singletonTypes) == otherMt)
                        {
                            matches = true;
                            break;
                        }
                    }
                };

                if (const UnionTypeVar* utv = get<UnionTypeVar>(leftType); utv && rightMt)
                {
                    testUnion(utv, rightMt);
                }

                if (const UnionTypeVar* utv = get<UnionTypeVar>(rightType); utv && leftMt && !matches)
                {
                    testUnion(utv, leftMt);
                }
            }

            if (!matches && isComparison)
            {
                reportError(GenericError{format("Types %s and %s cannot be compared with %s because they do not have the same metatable",
                                toString(leftType).c_str(), toString(rightType).c_str(), toString(expr->op).c_str())},
                    expr->location);

                return;
            }

            std::optional<TypeId> mm;
            if (std::optional<TypeId> leftMm = findMetatableEntry(singletonTypes, module->errors, leftType, it->second, expr->left->location))
                mm = leftMm;
            else if (std::optional<TypeId> rightMm = findMetatableEntry(singletonTypes, module->errors, rightType, it->second, expr->right->location))
                mm = rightMm;

            if (mm)
            {
                if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(*mm))
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

                    reportErrors(tryUnify(scope, expr->location, ftv->argTypes, expectedArgs));

                    if (expr->op == AstExprBinary::CompareEq || expr->op == AstExprBinary::CompareNe || expr->op == AstExprBinary::CompareGe ||
                        expr->op == AstExprBinary::CompareGt || expr->op == AstExprBinary::Op::CompareLe || expr->op == AstExprBinary::Op::CompareLt)
                    {
                        TypePackId expectedRets = module->internalTypes.addTypePack({singletonTypes->booleanType});
                        if (!isSubtype(ftv->retTypes, expectedRets, scope, singletonTypes, ice))
                        {
                            reportError(GenericError{format("Metamethod '%s' must return type 'boolean'", it->second)}, expr->location);
                        }
                    }
                    else if (!first(ftv->retTypes))
                    {
                        reportError(GenericError{format("Metamethod '%s' must return a value", it->second)}, expr->location);
                    }
                }
                else
                {
                    reportError(CannotCallNonFunction{*mm}, expr->location);
                }

                return;
            }
            // If this is a string comparison, or a concatenation of strings, we
            // want to fall through to primitive behavior.
            else if (!isEquality && !(isStringOperation && (expr->op == AstExprBinary::Op::Concat || isComparison)))
            {
                if (leftMt || rightMt)
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

                    return;
                }
                else if (!leftMt && !rightMt && (get<TableTypeVar>(leftType) || get<TableTypeVar>(rightType)))
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

                    return;
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
            reportErrors(tryUnify(scope, expr->left->location, leftType, singletonTypes->numberType));
            reportErrors(tryUnify(scope, expr->right->location, rightType, singletonTypes->numberType));

            break;
        case AstExprBinary::Op::Concat:
            reportErrors(tryUnify(scope, expr->left->location, leftType, singletonTypes->stringType));
            reportErrors(tryUnify(scope, expr->right->location, rightType, singletonTypes->stringType));

            break;
        case AstExprBinary::Op::CompareGe:
        case AstExprBinary::Op::CompareGt:
        case AstExprBinary::Op::CompareLe:
        case AstExprBinary::Op::CompareLt:
            if (isNumber(leftType))
                reportErrors(tryUnify(scope, expr->right->location, rightType, singletonTypes->numberType));
            else if (isString(leftType))
                reportErrors(tryUnify(scope, expr->right->location, rightType, singletonTypes->stringType));
            else
                reportError(GenericError{format("Types '%s' and '%s' cannot be compared with relational operator %s", toString(leftType).c_str(),
                                toString(rightType).c_str(), toString(expr->op).c_str())},
                    expr->location);

            break;
        case AstExprBinary::Op::And:
        case AstExprBinary::Op::Or:
        case AstExprBinary::Op::CompareEq:
        case AstExprBinary::Op::CompareNe:
            break;
        default:
            // Unhandled AstExprBinary::Op possibility.
            LUAU_ASSERT(false);
        }
    }

    void visit(AstExprTypeAssertion* expr)
    {
        visit(expr->expr);
        visit(expr->annotation);

        TypeId annotationType = lookupAnnotation(expr->annotation);
        TypeId computedType = lookupType(expr->expr);

        // Note: As an optimization, we try 'number <: number | string' first, as that is the more likely case.
        if (isSubtype(annotationType, computedType, stack.back(), singletonTypes, ice, /* anyIsTop */ false))
            return;

        if (isSubtype(computedType, annotationType, stack.back(), singletonTypes, ice, /* anyIsTop */ false))
            return;

        reportError(TypesAreUnrelated{computedType, annotationType}, expr->location);
    }

    void visit(AstExprIfElse* expr)
    {
        // TODO!
        visit(expr->condition);
        visit(expr->trueExpr);
        visit(expr->falseExpr);
    }

    void visit(AstExprError* expr)
    {
        // TODO!
        for (AstExpr* e : expr->expressions)
            visit(e);
    }

    /** Extract a TypeId for the first type of the provided pack.
     *
     * Note that this may require modifying some types.  I hope this doesn't cause problems!
     */
    TypeId flattenPack(TypePackId pack)
    {
        pack = follow(pack);

        while (true)
        {
            auto tp = get<TypePack>(pack);
            if (tp && tp->head.empty() && tp->tail)
                pack = *tp->tail;
            else
                break;
        }

        if (auto ty = first(pack))
            return *ty;
        else if (auto vtp = get<VariadicTypePack>(pack))
            return vtp->ty;
        else if (auto ftp = get<FreeTypePack>(pack))
        {
            TypeId result = module->internalTypes.addType(FreeTypeVar{ftp->scope});
            TypePackId freeTail = module->internalTypes.addTypePack(FreeTypePack{ftp->scope});

            TypePack& resultPack = asMutable(pack)->ty.emplace<TypePack>();
            resultPack.head.assign(1, result);
            resultPack.tail = freeTail;

            return result;
        }
        else if (get<Unifiable::Error>(pack))
            return singletonTypes->errorRecoveryType();
        else
            ice.ice("flattenPack got a weird pack!");
    }

    void visit(AstType* ty)
    {
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
                packsProvided += 1;
            }

            for (size_t i = typesProvided; i < typesRequired; ++i)
            {
                if (alias->typeParams[i].defaultValue)
                {
                    typesProvided += 1;
                }
            }

            for (size_t i = packsProvided; i < packsProvided; ++i)
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
                reportError(UnknownSymbol{ty->name.value, UnknownSymbol::Context::Type}, ty->location);
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
        // TODO!

        visit(ty->argTypes);
        visit(ty->returnTypes);
    }

    void visit(AstTypeTypeof* ty)
    {
        visit(ty->expr);
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
    ErrorVec tryUnify(NotNull<Scope> scope, const Location& location, TID subTy, TID superTy)
    {
        UnifierSharedState sharedState{&ice};
        Normalizer normalizer{&module->internalTypes, singletonTypes, NotNull{&sharedState}};
        Unifier u{NotNull{&normalizer}, Mode::Strict, scope, location, Covariant};
        u.anyIsTop = true;
        u.tryUnify(subTy, superTy);

        return std::move(u.errors);
    }

    void reportError(TypeErrorData data, const Location& location)
    {
        module->errors.emplace_back(location, sourceModule->name, std::move(data));

        if (FFlag::DebugLuauLogSolverToJson)
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

    std::optional<TypeId> getIndexTypeFromType(const ScopePtr& scope, TypeId type, const std::string& prop, const Location& location, bool addErrors)
    {
        return Luau::getIndexTypeFromType(scope, module->errors, &module->internalTypes, singletonTypes, type, prop, location, addErrors, ice);
    }
};

void check(NotNull<SingletonTypes> singletonTypes, DcrLogger* logger, const SourceModule& sourceModule, Module* module)
{
    TypeChecker2 typeChecker{singletonTypes, logger, &sourceModule, module};

    typeChecker.visit(sourceModule.root);
}

} // namespace Luau
