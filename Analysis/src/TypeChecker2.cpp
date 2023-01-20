// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeChecker2.h"

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Clone.h"
#include "Luau/Error.h"
#include "Luau/Instantiation.h"
#include "Luau/Metamethods.h"
#include "Luau/Normalize.h"
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeUtils.h"
#include "Luau/Type.h"
#include "Luau/Unifier.h"
#include "Luau/ToString.h"
#include "Luau/DcrLogger.h"

#include <algorithm>

LUAU_FASTFLAG(DebugLuauLogSolverToJson);
LUAU_FASTFLAG(DebugLuauMagicTypes);
LUAU_FASTFLAG(LuauNegatedClassTypes)

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
    NotNull<BuiltinTypes> builtinTypes;
    DcrLogger* logger;
    InternalErrorReporter ice; // FIXME accept a pointer from Frontend
    const SourceModule* sourceModule;
    Module* module;
    TypeArena testArena;

    std::vector<NotNull<Scope>> stack;

    UnifierSharedState sharedState{&ice};
    Normalizer normalizer{&testArena, builtinTypes, NotNull{&sharedState}};

    TypeChecker2(NotNull<BuiltinTypes> builtinTypes, DcrLogger* logger, const SourceModule* sourceModule, Module* module)
        : builtinTypes(builtinTypes)
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
            return builtinTypes->anyTypePack;
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

    enum ValueContext
    {
        LValue,
        RValue
    };

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
        visit(ifStatement->condition, RValue);
        visit(ifStatement->thenbody);
        if (ifStatement->elsebody)
            visit(ifStatement->elsebody);
    }

    void visit(AstStatWhile* whileStatement)
    {
        visit(whileStatement->condition, RValue);
        visit(whileStatement->body);
    }

    void visit(AstStatRepeat* repeatStatement)
    {
        visit(repeatStatement->body);
        visit(repeatStatement->condition, RValue);
    }

    void visit(AstStatBreak*) {}

    void visit(AstStatContinue*) {}

    void visit(AstStatReturn* ret)
    {
        Scope* scope = findInnermostScope(ret->location);
        TypePackId expectedRetType = scope->returnType;

        TypeArena* arena = &testArena;
        TypePackId actualRetType = reconstructPack(ret->list, *arena);

        Unifier u{NotNull{&normalizer}, Mode::Strict, stack.back(), ret->location, Covariant};

        u.tryUnify(actualRetType, expectedRetType);
        const bool ok = u.errors.empty() && u.log.empty();

        if (!ok)
        {
            for (const TypeError& e : u.errors)
                reportError(e);
        }

        for (AstExpr* expr : ret->list)
            visit(expr, RValue);
    }

    void visit(AstStatExpr* expr)
    {
        visit(expr->expr, RValue);
    }

    void visit(AstStatLocal* local)
    {
        size_t count = std::max(local->values.size, local->vars.size);
        for (size_t i = 0; i < count; ++i)
        {
            AstExpr* value = i < local->values.size ? local->values.data[i] : nullptr;

            if (value)
                visit(value, RValue);

            TypeId* maybeValueType = value ? module->astTypes.find(value) : nullptr;
            if (i != local->values.size - 1 || maybeValueType)
            {
                AstLocal* var = i < local->vars.size ? local->vars.data[i] : nullptr;

                if (var && var->annotation)
                {
                    TypeId annotationType = lookupAnnotation(var->annotation);
                    TypeId valueType = value ? lookupType(value) : nullptr;
                    if (valueType)
                    {
                        ErrorVec errors = tryUnify(stack.back(), value->location, valueType, annotationType);
                        if (!errors.empty())
                            reportErrors(std::move(errors));
                    }

                    visit(var->annotation);
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

                        visit(var->annotation);
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

        visit(forStatement->from, RValue);
        visit(forStatement->to, RValue);
        if (forStatement->step)
            visit(forStatement->step, RValue);
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
            visit(expr, RValue);

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

        // ugh.  There's nothing in the AST to hang a whole type pack on for the
        // set of iteratees, so we have to piece it back together by hand.
        std::vector<TypeId> valueTypes;
        for (size_t i = 0; i < forInStatement->values.size - 1; ++i)
            valueTypes.emplace_back(lookupType(forInStatement->values.data[i]));
        TypePackId iteratorTail = lookupPack(forInStatement->values.data[forInStatement->values.size - 1]);
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

            // If iteratorTypes is too short to be a valid call to nextFn, we have to report a count mismatch error.
            // If 2 is too short to be a valid call to nextFn, we have to report a count mismatch error.
            // If 2 is too long to be a valid call to nextFn, we have to report a count mismatch error.
            auto [minCount, maxCount] = getParameterExtents(TxnLog::empty(), iterFtv->argTypes, /*includeHiddenVariadics*/ true);

            if (minCount > 2)
                reportError(CountMismatch{2, std::nullopt, minCount, CountMismatch::Arg}, forInStatement->vars.data[0]->location);
            if (maxCount && *maxCount < 2)
                reportError(CountMismatch{2, std::nullopt, *maxCount, CountMismatch::Arg}, forInStatement->vars.data[0]->location);

            TypePack flattenedArgTypes = extendTypePack(arena, builtinTypes, iterFtv->argTypes, 2);
            size_t firstIterationArgCount = iterTys.empty() ? 0 : iterTys.size() - 1;
            size_t actualArgCount = expectedVariableTypes.head.size();

            if (firstIterationArgCount < minCount)
                reportError(CountMismatch{2, std::nullopt, firstIterationArgCount, CountMismatch::Arg}, forInStatement->vars.data[0]->location);
            else if (actualArgCount < minCount)
                reportError(CountMismatch{2, std::nullopt, actualArgCount, CountMismatch::Arg}, forInStatement->vars.data[0]->location);

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
        else if (get<AnyType>(iteratorTy) || get<ErrorType>(iteratorTy))
        {
            // nothing
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
            visit(lhs, LValue);
            TypeId lhsType = lookupType(lhs);

            AstExpr* rhs = assign->values.data[i];
            visit(rhs, RValue);
            TypeId rhsType = lookupType(rhs);

            if (!isSubtype(rhsType, lhsType, stack.back()))
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
        visit(stat->name, LValue);
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
            visit(expr, RValue);

        for (AstStat* s : stat->statements)
            visit(s);
    }

    void visit(AstExpr* expr, ValueContext context)
    {
        auto StackPusher = pushStack(expr);

        if (0)
        {
        }
        else if (auto e = expr->as<AstExprGroup>())
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
        // TODO!
    }

    void visit(AstExprConstantBool* expr)
    {
        // TODO!
    }

    void visit(AstExprConstantNumber* number)
    {
        TypeId actualType = lookupType(number);
        TypeId numberType = builtinTypes->numberType;

        if (!isSubtype(numberType, actualType, stack.back()))
        {
            reportError(TypeMismatch{actualType, numberType}, number->location);
        }
    }

    void visit(AstExprConstantString* string)
    {
        TypeId actualType = lookupType(string);
        TypeId stringType = builtinTypes->stringType;

        if (!isSubtype(actualType, stringType, stack.back()))
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
        visit(call->func, RValue);

        for (AstExpr* arg : call->args)
            visit(arg, RValue);

        TypeArena* arena = &testArena;
        Instantiation instantiation{TxnLog::empty(), arena, TypeLevel{}, stack.back()};

        TypePackId expectedRetType = lookupPack(call);
        TypeId functionType = lookupType(call->func);
        TypeId testFunctionType = functionType;
        TypePack args;
        std::vector<Location> argLocs;
        argLocs.reserve(call->args.size + 1);

        if (get<AnyType>(functionType) || get<ErrorType>(functionType))
            return;
        else if (std::optional<TypeId> callMm = findMetatableEntry(builtinTypes, module->errors, functionType, "__call", call->func->location))
        {
            if (get<FunctionType>(follow(*callMm)))
            {
                if (std::optional<TypeId> instantiatedCallMm = instantiation.substitute(*callMm))
                {
                    args.head.push_back(functionType);
                    argLocs.push_back(call->func->location);
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
        else if (get<FunctionType>(functionType))
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
        else if (auto utv = get<UnionType>(functionType))
        {
            // Sometimes it's okay to call a union of functions, but only if all of the functions are the same.
            std::optional<TypeId> fst;
            for (TypeId ty : utv)
            {
                if (!fst)
                    fst = follow(ty);
                else if (fst != follow(ty))
                {
                    reportError(CannotCallNonFunction{functionType}, call->func->location);
                    return;
                }
            }

            if (!fst)
                ice.ice("UnionType had no elements, so fst is nullopt?");

            if (std::optional<TypeId> instantiatedFunctionType = instantiation.substitute(*fst))
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

        if (call->self)
        {
            AstExprIndexName* indexExpr = call->func->as<AstExprIndexName>();
            if (!indexExpr)
                ice.ice("method call expression has no 'self'");

            args.head.push_back(lookupType(indexExpr->expr));
            argLocs.push_back(indexExpr->expr->location);
        }

        for (size_t i = 0; i < call->args.size; ++i)
        {
            AstExpr* arg = call->args.data[i];
            argLocs.push_back(arg->location);
            TypeId* argTy = module->astTypes.find(arg);
            if (argTy)
                args.head.push_back(*argTy);
            else if (i == call->args.size - 1)
            {
                TypePackId* argTail = module->astTypePacks.find(arg);
                if (argTail)
                    args.tail = *argTail;
                else
                    args.tail = builtinTypes->anyTypePack;
            }
            else
                args.head.push_back(builtinTypes->anyType);
        }

        TypePackId expectedArgTypes = arena->addTypePack(args);

        const FunctionType* inferredFunctionType = get<FunctionType>(testFunctionType);
        LUAU_ASSERT(inferredFunctionType); // testFunctionType should always be a FunctionType here

        size_t argIndex = 0;
        auto inferredArgIt = begin(inferredFunctionType->argTypes);
        auto expectedArgIt = begin(expectedArgTypes);
        while (inferredArgIt != end(inferredFunctionType->argTypes) && expectedArgIt != end(expectedArgTypes))
        {
            Location argLoc = (argIndex >= argLocs.size()) ? argLocs.back() : argLocs[argIndex];
            reportErrors(tryUnify(stack.back(), argLoc, *expectedArgIt, *inferredArgIt));

            ++argIndex;
            ++inferredArgIt;
            ++expectedArgIt;
        }

        // piggyback on the unifier for arity checking, but we can't do this for checking the actual arguments since the locations would be bad
        ErrorVec errors = tryUnify(stack.back(), call->location, expectedArgTypes, inferredFunctionType->argTypes);
        for (TypeError e : errors)
            if (get<CountMismatch>(e) != nullptr)
                reportError(std::move(e));

        reportErrors(tryUnify(stack.back(), call->location, inferredFunctionType->retTypes, expectedRetType, CountMismatch::FunctionResult));
    }

    void visit(AstExprIndexName* indexName, ValueContext context)
    {
        TypeId leftType = lookupType(indexName->expr);

        const NormalizedType* norm = normalizer.normalize(leftType);
        if (!norm)
            reportError(NormalizationTooComplex{}, indexName->indexLocation);

        checkIndexTypeFromType(leftType, *norm, indexName->index.value, indexName->location, context);
    }

    void visit(AstExprIndexExpr* indexExpr, ValueContext context)
    {
        // TODO!
        visit(indexExpr->expr, LValue);
        visit(indexExpr->index, RValue);
    }

    void visit(AstExprFunction* fn)
    {
        auto StackPusher = pushStack(fn);

        TypeId inferredFnTy = lookupType(fn);
        const FunctionType* inferredFtv = get<FunctionType>(inferredFnTy);
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

                if (!isSubtype(annotatedArgTy, inferredArgTy, stack.back()))
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
                visit(item.key, LValue);
            visit(item.value, RValue);
        }
    }

    void visit(AstExprUnary* expr)
    {
        visit(expr->expr, RValue);

        NotNull<Scope> scope = stack.back();
        TypeId operandType = lookupType(expr->expr);

        if (get<AnyType>(operandType) || get<ErrorType>(operandType) || get<NeverType>(operandType))
            return;

        if (auto it = kUnaryOpMetamethods.find(expr->op); it != kUnaryOpMetamethods.end())
        {
            std::optional<TypeId> mm = findMetatableEntry(builtinTypes, module->errors, operandType, it->second, expr->location);
            if (mm)
            {
                if (const FunctionType* ftv = get<FunctionType>(follow(*mm)))
                {
                    TypePackId expectedArgs = testArena.addTypePack({operandType});
                    reportErrors(tryUnify(scope, expr->location, expectedArgs, ftv->argTypes));

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

    TypeId visit(AstExprBinary* expr, void* overrideKey = nullptr)
    {
        visit(expr->left, LValue);
        visit(expr->right, LValue);

        NotNull<Scope> scope = stack.back();

        bool isEquality = expr->op == AstExprBinary::Op::CompareEq || expr->op == AstExprBinary::Op::CompareNe;
        bool isComparison = expr->op >= AstExprBinary::Op::CompareEq && expr->op <= AstExprBinary::Op::CompareGe;
        bool isLogical = expr->op == AstExprBinary::Op::And || expr->op == AstExprBinary::Op::Or;

        TypeId leftType = lookupType(expr->left);
        TypeId rightType = lookupType(expr->right);

        if (expr->op == AstExprBinary::Op::Or)
        {
            leftType = stripNil(builtinTypes, testArena, leftType);
        }

        bool isStringOperation = isString(leftType) && isString(rightType);

        if (get<AnyType>(leftType) || get<ErrorType>(leftType))
            return leftType;
        else if (get<AnyType>(rightType) || get<ErrorType>(rightType))
            return rightType;

        if ((get<BlockedType>(leftType) || get<FreeType>(leftType)) && !isEquality && !isLogical)
        {
            auto name = getIdentifierOfBaseVar(expr->left);
            reportError(CannotInferBinaryOperation{expr->op, name,
                            isComparison ? CannotInferBinaryOperation::OpKind::Comparison : CannotInferBinaryOperation::OpKind::Operation},
                expr->location);
            return leftType;
        }

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
                void* key = expr;
                if (overrideKey != nullptr)
                    key = overrideKey;

                TypeId instantiatedMm = module->astOverloadResolvedTypes[key];
                if (!instantiatedMm)
                    reportError(CodeTooComplex{}, expr->location);

                else if (const FunctionType* ftv = get<FunctionType>(follow(instantiatedMm)))
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
            if (isNumber(leftType))
            {
                reportErrors(tryUnify(scope, expr->right->location, rightType, builtinTypes->numberType));
                return builtinTypes->numberType;
            }
            else if (isString(leftType))
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
        visit(expr->expr, RValue);
        visit(expr->annotation);

        TypeId annotationType = lookupAnnotation(expr->annotation);
        TypeId computedType = lookupType(expr->expr);

        // Note: As an optimization, we try 'number <: number | string' first, as that is the more likely case.
        if (isSubtype(annotationType, computedType, stack.back()))
            return;

        if (isSubtype(computedType, annotationType, stack.back()))
            return;

        reportError(TypesAreUnrelated{computedType, annotationType}, expr->location);
    }

    void visit(AstExprIfElse* expr)
    {
        // TODO!
        visit(expr->condition, RValue);
        visit(expr->trueExpr, RValue);
        visit(expr->falseExpr, RValue);
    }

    void visit(AstExprError* expr)
    {
        // TODO!
        for (AstExpr* e : expr->expressions)
            visit(e, RValue);
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
            TypeId result = testArena.addType(FreeType{ftp->scope});
            TypePackId freeTail = testArena.addTypePack(FreeTypePack{ftp->scope});

            TypePack& resultPack = asMutable(pack)->ty.emplace<TypePack>();
            resultPack.head.assign(1, result);
            resultPack.tail = freeTail;

            return result;
        }
        else if (get<Unifiable::Error>(pack))
            return builtinTypes->errorRecoveryType();
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
        // No further validation is necessary in this case. The main logic for
        // _luau_print is contained in lookupAnnotation.
        if (FFlag::DebugLuauMagicTypes && ty->name == "_luau_print" && ty->parameters.size > 0)
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
        visit(ty->expr, RValue);
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
    bool isSubtype(TID subTy, TID superTy, NotNull<Scope> scope)
    {
        TypeArena arena;
        Unifier u{NotNull{&normalizer}, Mode::Strict, scope, Location{}, Covariant};
        u.useScopes = true;

        u.tryUnify(subTy, superTy);
        const bool ok = u.errors.empty() && u.log.empty();
        return ok;
    }

    template<typename TID>
    ErrorVec tryUnify(NotNull<Scope> scope, const Location& location, TID subTy, TID superTy, CountMismatch::Context context = CountMismatch::Arg)
    {
        Unifier u{NotNull{&normalizer}, Mode::Strict, scope, location, Covariant};
        u.ctx = context;
        u.useScopes = true;
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

    void checkIndexTypeFromType(TypeId tableTy, const NormalizedType& norm, const std::string& prop, const Location& location, ValueContext context)
    {
        bool foundOneProp = false;
        std::vector<TypeId> typesMissingTheProp;

        auto fetch = [&](TypeId ty) {
            if (!normalizer.isInhabited(ty))
                return;

            bool found = hasIndexTypeFromType(ty, prop, location);
            foundOneProp |= found;
            if (!found)
                typesMissingTheProp.push_back(ty);
        };

        fetch(norm.tops);
        fetch(norm.booleans);

        if (FFlag::LuauNegatedClassTypes)
        {
            for (const auto& [ty, _negations] : norm.classes.classes)
            {
                fetch(ty);
            }
        }
        else
        {
            for (TypeId ty : norm.DEPRECATED_classes)
                fetch(ty);
        }
        fetch(norm.errors);
        fetch(norm.nils);
        fetch(norm.numbers);
        if (!norm.strings.isNever())
            fetch(builtinTypes->stringType);
        fetch(norm.threads);
        for (TypeId ty : norm.tables)
            fetch(ty);
        if (norm.functions.isTop)
            fetch(builtinTypes->functionType);
        else if (!norm.functions.isNever())
        {
            if (norm.functions.parts->size() == 1)
                fetch(norm.functions.parts->front());
            else
            {
                std::vector<TypeId> parts;
                parts.insert(parts.end(), norm.functions.parts->begin(), norm.functions.parts->end());
                fetch(testArena.addType(IntersectionType{std::move(parts)}));
            }
        }
        for (const auto& [tyvar, intersect] : norm.tyvars)
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
            else if (context == LValue)
                reportError(CannotExtendTable{tableTy, CannotExtendTable::Property, prop}, location);
            else
                reportError(UnknownProperty{tableTy, prop}, location);
        }
    }

    bool hasIndexTypeFromType(TypeId ty, const std::string& prop, const Location& location)
    {
        if (get<ErrorType>(ty) || get<AnyType>(ty) || get<NeverType>(ty))
            return true;

        if (isString(ty))
        {
            std::optional<TypeId> mtIndex = Luau::findMetatableEntry(builtinTypes, module->errors, builtinTypes->stringType, "__index", location);
            LUAU_ASSERT(mtIndex);
            ty = *mtIndex;
        }

        if (getTableType(ty))
            return bool(findTablePropertyRespectingMeta(builtinTypes, module->errors, ty, prop, location));
        else if (const ClassType* cls = get<ClassType>(ty))
            return bool(lookupClassProp(cls, prop));
        else if (const UnionType* utv = get<UnionType>(ty))
            ice.ice("getIndexTypeFromTypeHelper cannot take a UnionType");
        else if (const IntersectionType* itv = get<IntersectionType>(ty))
            return std::any_of(begin(itv), end(itv), [&](TypeId part) {
                return hasIndexTypeFromType(part, prop, location);
            });
        else
            return false;
    }
};

void check(NotNull<BuiltinTypes> builtinTypes, DcrLogger* logger, const SourceModule& sourceModule, Module* module)
{
    TypeChecker2 typeChecker{builtinTypes, logger, &sourceModule, module};

    typeChecker.visit(sourceModule.root);

    unfreeze(module->interfaceTypes);
    copyErrors(module->errors, module->interfaceTypes);
    freeze(module->interfaceTypes);
}

} // namespace Luau
