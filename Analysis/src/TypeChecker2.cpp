
#include "lluz/TypeChecker2.h"

#include <algorithm>

#include "lluz/Ast.h"
#include "lluz/AstQuery.h"
#include "lluz/Clone.h"
#include "lluz/Instantiation.h"
#include "lluz/Normalize.h"
#include "lluz/TxnLog.h"
#include "lluz/TypeUtils.h"
#include "lluz/Unifier.h"
#include "lluz/ToString.h"

namespace lluz
{

struct TypeChecker2 : public AstVisitor
{
    const SourceModule* sourceModule;
    Module* module;
    InternalErrorReporter ice; // FIXME accept a pointer from Frontend
    SingletonTypes& singletonTypes;

    TypeChecker2(const SourceModule* sourceModule, Module* module)
        : sourceModule(sourceModule)
        , module(module)
        , singletonTypes(getSingletonTypes())
    {
    }

    using AstVisitor::visit;

    TypePackId lookupPack(AstExpr* expr)
    {
        // If a type isn't in the type graph, it probably means that a recursion limit was exceeded.
        // We'll just return anyType in these cases.  Typechecking against any is very fast and this
        // allows us not to think about this very much in the actual typechecking logic.
        TypePackId* tp = module->astTypePacks.find(expr);
        if (tp)
            return follow(*tp);
        else
            return singletonTypes.anyTypePack;
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

        return singletonTypes.anyType;
    }

    TypeId lookupAnnotation(AstType* annotation)
    {
        TypeId* ty = module->astResolvedTypes.find(annotation);
        lluz_ASSERT(ty);
        return follow(*ty);
    }

    TypePackId reconstructPack(AstArray<AstExpr*> exprs, TypeArena& arena)
    {
        std::vector<TypeId> head;

        for (size_t i = 0; i < exprs.size - 1; ++i)
        {
            head.push_back(lookupType(exprs.data[i]));
        }

        TypePackId tail = lookupPack(exprs.data[exprs.size - 1]);
        return arena.addTypePack(TypePack{head, tail});
    }

    Scope2* findInnermostScope(Location location)
    {
        Scope2* bestScope = module->getModuleScope2();
        Location bestLocation = module->scope2s[0].first;

        for (size_t i = 0; i < module->scope2s.size(); ++i)
        {
            auto& [scopeBounds, scope] = module->scope2s[i];
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

    bool visit(AstStatLocal* local) override
    {
        for (size_t i = 0; i < local->values.size; ++i)
        {
            AstExpr* value = local->values.data[i];
            if (i == local->values.size - 1)
            {
                if (i < local->values.size)
                {
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
                            if (!isSubtype(*it, varType, ice))
                            {
                                reportError(TypeMismatch{varType, *it}, value->location);
                            }
                        }

                        ++it;
                    }
                }
            }
            else
            {
                TypeId valueType = lookupType(value);
                AstLocal* var = local->vars.data[i];

                if (var->annotation)
                {
                    TypeId varType = lookupAnnotation(var->annotation);
                    if (!isSubtype(varType, valueType, ice))
                    {
                        reportError(TypeMismatch{varType, valueType}, value->location);
                    }
                }
            }
        }

        return true;
    }

    bool visit(AstStatAssign* assign) override
    {
        size_t count = std::min(assign->vars.size, assign->values.size);

        for (size_t i = 0; i < count; ++i)
        {
            AstExpr* lhs = assign->vars.data[i];
            TypeId lhsType = lookupType(lhs);

            AstExpr* rhs = assign->values.data[i];
            TypeId rhsType = lookupType(rhs);

            if (!isSubtype(rhsType, lhsType, ice))
            {
                reportError(TypeMismatch{lhsType, rhsType}, rhs->location);
            }
        }

        return true;
    }

    bool visit(AstStatReturn* ret) override
    {
        Scope2* scope = findInnermostScope(ret->location);
        TypePackId expectedRetType = scope->returnType;

        TypeArena arena;
        TypePackId actualRetType = reconstructPack(ret->list, arena);

        UnifierSharedState sharedState{&ice};
        Unifier u{&arena, Mode::Strict, ret->location, Covariant, sharedState};
        u.anyIsTop = true;

        u.tryUnify(actualRetType, expectedRetType);
        const bool ok = u.errors.empty() && u.log.empty();

        if (!ok)
        {
            for (const TypeError& e : u.errors)
                reportError(e);
        }

        return true;
    }

    bool visit(AstExprCall* call) override
    {
        TypeArena arena;
        Instantiation instantiation{TxnLog::empty(), &arena, TypeLevel{}};

        TypePackId expectedRetType = lookupPack(call);
        TypeId functionType = lookupType(call->func);
        TypeId instantiatedFunctionType = instantiation.substitute(functionType).value_or(nullptr);
        lluz_ASSERT(functionType);

        TypePack args;
        for (const auto& arg : call->args)
        {
            TypeId argTy = module->astTypes[arg];
            lluz_ASSERT(argTy);
            args.head.push_back(argTy);
        }

        TypePackId argsTp = arena.addTypePack(args);
        FunctionTypeVar ftv{argsTp, expectedRetType};
        TypeId expectedType = arena.addType(ftv);
        if (!isSubtype(expectedType, instantiatedFunctionType, ice))
        {
            unfreeze(module->interfaceTypes);
            CloneState cloneState;
            expectedType = clone(expectedType, module->interfaceTypes, cloneState);
            freeze(module->interfaceTypes);
            reportError(TypeMismatch{expectedType, functionType}, call->location);
        }

        return true;
    }

    bool visit(AstExprFunction* fn) override
    {
        TypeId inferredFnTy = lookupType(fn);
        const FunctionTypeVar* inferredFtv = get<FunctionTypeVar>(inferredFnTy);
        lluz_ASSERT(inferredFtv);

        auto argIt = begin(inferredFtv->argTypes);
        for (const auto& arg : fn->args)
        {
            if (argIt == end(inferredFtv->argTypes))
                break;

            if (arg->annotation)
            {
                TypeId inferredArgTy = *argIt;
                TypeId annotatedArgTy = lookupAnnotation(arg->annotation);

                if (!isSubtype(annotatedArgTy, inferredArgTy, ice))
                {
                    reportError(TypeMismatch{annotatedArgTy, inferredArgTy}, arg->location);
                }
            }

            ++argIt;
        }

        return true;
    }

    bool visit(AstExprIndexName* indexName) override
    {
        TypeId leftType = lookupType(indexName->expr);
        TypeId resultType = lookupType(indexName);

        // leftType must have a property called indexName->index

        std::optional<TypeId> t = findTablePropertyRespectingMeta(module->errors, leftType, indexName->index.value, indexName->location);
        if (t)
        {
            if (!isSubtype(resultType, *t, ice))
            {
                reportError(TypeMismatch{resultType, *t}, indexName->location);
            }
        }
        else
        {
            reportError(UnknownProperty{leftType, indexName->index.value}, indexName->location);
        }

        return true;
    }

    bool visit(AstExprConstantNumber* number) override
    {
        TypeId actualType = lookupType(number);
        TypeId numberType = getSingletonTypes().numberType;

        if (!isSubtype(numberType, actualType, ice))
        {
            reportError(TypeMismatch{actualType, numberType}, number->location);
        }

        return true;
    }

    bool visit(AstExprConstantString* string) override
    {
        TypeId actualType = lookupType(string);
        TypeId stringType = getSingletonTypes().stringType;

        if (!isSubtype(stringType, actualType, ice))
        {
            reportError(TypeMismatch{actualType, stringType}, string->location);
        }

        return true;
    }

    /** Extract a TypeId for the first type of the provided pack.
     *
     * Note that this may require modifying some types.  I hope this doesn't cause problems!
     */
    TypeId flattenPack(TypePackId pack)
    {
        pack = follow(pack);

        while (auto tp = get<TypePack>(pack))
        {
            if (tp->head.empty() && tp->tail)
                pack = *tp->tail;
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
            return singletonTypes.errorRecoveryType();
        else
            ice.ice(XorStr("flattenPack got a weird pack!"));
    }

    bool visit(AstType* ty) override
    {
        return true;
    }

    bool visit(AstTypeReference* ty) override
    {
        Scope2* scope = findInnermostScope(ty->location);

        // TODO: Imported types
        // TODO: Generic types
        if (!scope->lookupTypeBinding(ty->name.value))
        {
            reportError(UnknownSymbol{ty->name.value, UnknownSymbol::Context::Type}, ty->location);
        }

        return true;
    }

    void reportError(TypeErrorData&& data, const Location& location)
    {
        module->errors.emplace_back(location, sourceModule->name, std::move(data));
    }

    void reportError(TypeError e)
    {
        module->errors.emplace_back(std::move(e));
    }
};

void check(const SourceModule& sourceModule, Module* module)
{
    TypeChecker2 typeChecker{&sourceModule, module};

    sourceModule.root->visit(&typeChecker);
}

} // namespace lluz
