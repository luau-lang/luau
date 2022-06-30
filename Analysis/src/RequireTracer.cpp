// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/RequireTracer.h"

#include "Luau/Ast.h"
#include "Luau/Module.h"

namespace Luau
{

struct RequireTracer : AstVisitor
{
    RequireTracer(RequireTraceResult& result, FileResolver* fileResolver, const ModuleName& currentModuleName)
        : result(result)
        , fileResolver(fileResolver)
        , currentModuleName(currentModuleName)
        , locals(nullptr)
    {
    }

    bool visit(AstExprTypeAssertion* expr) override
    {
        // suppress `require() :: any`
        return false;
    }

    bool visit(AstExprCall* expr) override
    {
        AstExprGlobal* global = expr->func->as<AstExprGlobal>();

        if (global && global->name == "require" && expr->args.size >= 1)
            requireCalls.push_back(expr);

        return true;
    }

    bool visit(AstStatLocal* stat) override
    {
        for (size_t i = 0; i < stat->vars.size && i < stat->values.size; ++i)
        {
            AstLocal* local = stat->vars.data[i];
            AstExpr* expr = stat->values.data[i];

            // track initializing expression to be able to trace modules through locals
            locals[local] = expr;
        }

        return true;
    }

    bool visit(AstStatAssign* stat) override
    {
        for (size_t i = 0; i < stat->vars.size; ++i)
        {
            // locals that are assigned don't have a known expression
            if (AstExprLocal* expr = stat->vars.data[i]->as<AstExprLocal>())
                locals[expr->local] = nullptr;
        }

        return true;
    }

    bool visit(AstType* node) override
    {
        // allow resolving require inside `typeof` annotations
        return true;
    }

    AstExpr* getDependent(AstExpr* node)
    {
        if (AstExprLocal* expr = node->as<AstExprLocal>())
            return locals[expr->local];
        else if (AstExprIndexName* expr = node->as<AstExprIndexName>())
            return expr->expr;
        else if (AstExprIndexExpr* expr = node->as<AstExprIndexExpr>())
            return expr->expr;
        else if (AstExprCall* expr = node->as<AstExprCall>(); expr && expr->self)
            return expr->func->as<AstExprIndexName>()->expr;
        else
            return nullptr;
    }

    void process()
    {
        ModuleInfo moduleContext{currentModuleName};

        // seed worklist with require arguments
        work.reserve(requireCalls.size());

        for (AstExprCall* require : requireCalls)
            work.push_back(require->args.data[0]);

        // push all dependent expressions to the work stack; note that the vector is modified during traversal
        for (size_t i = 0; i < work.size(); ++i)
            if (AstExpr* dep = getDependent(work[i]))
                work.push_back(dep);

        // resolve all expressions to a module info
        for (size_t i = work.size(); i > 0; --i)
        {
            AstExpr* expr = work[i - 1];

            // when multiple expressions depend on the same one we push it to work queue multiple times
            if (result.exprs.contains(expr))
                continue;

            std::optional<ModuleInfo> info;

            if (AstExpr* dep = getDependent(expr))
            {
                const ModuleInfo* context = result.exprs.find(dep);

                // locals just inherit their dependent context, no resolution required
                if (expr->is<AstExprLocal>())
                    info = context ? std::optional<ModuleInfo>(*context) : std::nullopt;
                else
                    info = fileResolver->resolveModule(context, expr);
            }
            else
            {
                info = fileResolver->resolveModule(&moduleContext, expr);
            }

            if (info)
                result.exprs[expr] = std::move(*info);
        }

        // resolve all requires according to their argument
        result.requireList.reserve(requireCalls.size());

        for (AstExprCall* require : requireCalls)
        {
            AstExpr* arg = require->args.data[0];

            if (const ModuleInfo* info = result.exprs.find(arg))
            {
                result.requireList.push_back({info->name, require->location});

                ModuleInfo infoCopy = *info; // copy *info out since next line invalidates info!
                result.exprs[require] = std::move(infoCopy);
            }
            else
            {
                result.exprs[require] = {}; // mark require as unresolved
            }
        }
    }

    RequireTraceResult& result;
    FileResolver* fileResolver;
    ModuleName currentModuleName;

    DenseHashMap<AstLocal*, AstExpr*> locals;
    std::vector<AstExpr*> work;
    std::vector<AstExprCall*> requireCalls;
};

RequireTraceResult traceRequires(FileResolver* fileResolver, AstStatBlock* root, const ModuleName& currentModuleName)
{
    RequireTraceResult result;
    RequireTracer tracer{result, fileResolver, currentModuleName};
    root->visit(&tracer);
    tracer.process();
    return result;
}

} // namespace Luau
