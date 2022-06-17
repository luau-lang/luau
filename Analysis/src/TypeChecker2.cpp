
#include "Luau/TypeChecker2.h"

#include <algorithm>

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Clone.h"
#include "Luau/Normalize.h"

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
