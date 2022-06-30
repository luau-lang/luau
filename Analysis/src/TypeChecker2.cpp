
#include "Luau/TypeChecker2.h"

#include <algorithm>

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Clone.h"
#include "Luau/Normalize.h"
#include "Luau/ConstraintGraphBuilder.h" // FIXME move Scope2 into its own header
#include "Luau/Unifier.h"
#include "Luau/ToString.h"

namespace Luau
{

struct TypeChecker2 : public AstVisitor
{
    const SourceModule* sourceModule;
    Module* module;
    InternalErrorReporter ice; // FIXME accept a pointer from Frontend

    TypeChecker2(const SourceModule* sourceModule, Module* module)
        : sourceModule(sourceModule)
        , module(module)
    {
    }

    using AstVisitor::visit;

    TypePackId lookupPack(AstExpr* expr)
    {
        TypePackId* tp = module->astTypePacks.find(expr);
        LUAU_ASSERT(tp);
        return follow(*tp);
    }

    TypeId lookupType(AstExpr* expr)
    {
        TypeId* ty = module->astTypes.find(expr);
        LUAU_ASSERT(ty);
        return follow(*ty);
    }

    TypeId lookupAnnotation(AstType* annotation)
    {
        TypeId* ty = module->astResolvedTypes.find(annotation);
        LUAU_ASSERT(ty);
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
            else
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
            TypeId* lhsType = module->astTypes.find(lhs);
            LUAU_ASSERT(lhsType);

            AstExpr* rhs = assign->values.data[i];
            TypeId* rhsType = module->astTypes.find(rhs);
            LUAU_ASSERT(rhsType);

            if (!isSubtype(*rhsType, *lhsType, ice))
            {
                reportError(TypeMismatch{*lhsType, *rhsType}, rhs->location);
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
                module->errors.push_back(e);
        }

        return true;
    }

    bool visit(AstExprCall* call) override
    {
        TypePackId expectedRetType = lookupPack(call);
        TypeId functionType = lookupType(call->func);

        TypeArena arena;
        TypePack args;
        for (const auto& arg : call->args)
        {
            TypeId argTy = module->astTypes[arg];
            LUAU_ASSERT(argTy);
            args.head.push_back(argTy);
        }

        TypePackId argsTp = arena.addTypePack(args);
        FunctionTypeVar ftv{argsTp, expectedRetType};
        TypeId expectedType = arena.addType(ftv);
        if (!isSubtype(expectedType, functionType, ice))
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

        if (auto ttv = get<TableTypeVar>(leftType))
        {
            auto it = ttv->props.find(indexName->index.value);
            if (it == ttv->props.end())
            {
                reportError(UnknownProperty{leftType, indexName->index.value}, indexName->location);
            }
            else if (!isSubtype(resultType, it->second.type, ice))
            {
                reportError(TypeMismatch{resultType, it->second.type}, indexName->location);
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

        if (!isSubtype(actualType, numberType, ice))
        {
            reportError(TypeMismatch{actualType, numberType}, number->location);
        }

        return true;
    }

    bool visit(AstExprConstantString* string) override
    {
        TypeId actualType = lookupType(string);
        TypeId stringType = getSingletonTypes().stringType;

        if (!isSubtype(actualType, stringType, ice))
        {
            reportError(TypeMismatch{actualType, stringType}, string->location);
        }

        return true;
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
};

void check(const SourceModule& sourceModule, Module* module)
{
    TypeChecker2 typeChecker{&sourceModule, module};

    sourceModule.root->visit(&typeChecker);
}

} // namespace Luau
