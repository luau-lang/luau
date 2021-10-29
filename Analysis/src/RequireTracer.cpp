// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/RequireTracer.h"

#include "Luau/Ast.h"
#include "Luau/Module.h"

LUAU_FASTFLAGVARIABLE(LuauTraceRequireLookupChild, false)

namespace Luau
{

namespace
{

struct RequireTracer : AstVisitor
{
    explicit RequireTracer(FileResolver* fileResolver, ModuleName currentModuleName)
        : fileResolver(fileResolver)
        , currentModuleName(std::move(currentModuleName))
    {
    }

    FileResolver* const fileResolver;
    ModuleName currentModuleName;
    DenseHashMap<AstLocal*, ModuleName> locals{0};
    RequireTraceResult result;

    std::optional<ModuleName> fromAstFragment(AstExpr* expr)
    {
        if (auto g = expr->as<AstExprGlobal>(); g && g->name == "script")
            return currentModuleName;

        return fileResolver->fromAstFragment(expr);
    }

    bool visit(AstStatLocal* stat) override
    {
        for (size_t i = 0; i < stat->vars.size; ++i)
        {
            AstLocal* local = stat->vars.data[i];

            if (local->annotation)
            {
                if (AstTypeTypeof* ann = local->annotation->as<AstTypeTypeof>())
                    ann->expr->visit(this);
            }

            if (i < stat->values.size)
            {
                AstExpr* expr = stat->values.data[i];
                expr->visit(this);

                const ModuleName* name = result.exprs.find(expr);
                if (name)
                    locals[local] = *name;
            }
        }

        return false;
    }

    bool visit(AstExprGlobal* global) override
    {
        std::optional<ModuleName> name = fromAstFragment(global);
        if (name)
            result.exprs[global] = *name;

        return false;
    }

    bool visit(AstExprLocal* local) override
    {
        const ModuleName* name = locals.find(local->local);
        if (name)
            result.exprs[local] = *name;

        return false;
    }

    bool visit(AstExprIndexName* indexName) override
    {
        indexName->expr->visit(this);

        const ModuleName* name = result.exprs.find(indexName->expr);
        if (name)
        {
            if (indexName->index == "parent" || indexName->index == "Parent")
            {
                if (auto parent = fileResolver->getParentModuleName(*name))
                    result.exprs[indexName] = *parent;
            }
            else
                result.exprs[indexName] = fileResolver->concat(*name, indexName->index.value);
        }

        return false;
    }

    bool visit(AstExprIndexExpr* indexExpr) override
    {
        indexExpr->expr->visit(this);

        const ModuleName* name = result.exprs.find(indexExpr->expr);
        const AstExprConstantString* str = indexExpr->index->as<AstExprConstantString>();
        if (name && str)
        {
            result.exprs[indexExpr] = fileResolver->concat(*name, std::string_view(str->value.data, str->value.size));
        }

        indexExpr->index->visit(this);

        return false;
    }

    bool visit(AstExprTypeAssertion* expr) override
    {
        return false;
    }

    // If we see game:GetService("StringLiteral") or Game:GetService("StringLiteral"), then rewrite to game.StringLiteral.
    // Else traverse arguments and trace requires to them.
    bool visit(AstExprCall* call) override
    {
        for (AstExpr* arg : call->args)
            arg->visit(this);

        call->func->visit(this);

        AstExprGlobal* globalName = call->func->as<AstExprGlobal>();
        if (globalName && globalName->name == "require" && call->args.size >= 1)
        {
            if (const ModuleName* moduleName = result.exprs.find(call->args.data[0]))
                result.requires.push_back({*moduleName, call->location});

            return false;
        }

        AstExprIndexName* indexName = call->func->as<AstExprIndexName>();
        if (!indexName)
            return false;

        std::optional<ModuleName> rootName = fromAstFragment(indexName->expr);

        if (FFlag::LuauTraceRequireLookupChild && !rootName)
        {
            if (const ModuleName* moduleName = result.exprs.find(indexName->expr))
                rootName = *moduleName;
        }

        if (!rootName)
            return false;

        bool supportedLookup = indexName->index == "GetService" ||
                               (FFlag::LuauTraceRequireLookupChild && (indexName->index == "FindFirstChild" || indexName->index == "WaitForChild"));

        if (!supportedLookup)
            return false;

        if (call->args.size != 1)
            return false;

        AstExprConstantString* name = call->args.data[0]->as<AstExprConstantString>();
        if (!name)
            return false;

        std::string_view v{name->value.data, name->value.size};
        if (v.end() != std::find(v.begin(), v.end(), '/'))
            return false;

        result.exprs[call] = fileResolver->concat(*rootName, v);

        // 'WaitForChild' can be used on modules that are not awailable at the typecheck time, but will be awailable at runtime
        // If we fail to find such module, we will not report an UnknownRequire error
        if (FFlag::LuauTraceRequireLookupChild && indexName->index == "WaitForChild")
            result.optional[call] = true;

        return false;
    }
};

} // anonymous namespace

RequireTraceResult traceRequires(FileResolver* fileResolver, AstStatBlock* root, ModuleName currentModuleName)
{
    RequireTracer tracer{fileResolver, std::move(currentModuleName)};
    root->visit(&tracer);
    return tracer.result;
}

} // namespace Luau
