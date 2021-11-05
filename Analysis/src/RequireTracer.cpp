// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/RequireTracer.h"

#include "Luau/Ast.h"
#include "Luau/Module.h"

LUAU_FASTFLAGVARIABLE(LuauTraceRequireLookupChild, false)
LUAU_FASTFLAGVARIABLE(LuauNewRequireTrace, false)

namespace Luau
{

namespace
{

struct RequireTracerOld : AstVisitor
{
    explicit RequireTracerOld(FileResolver* fileResolver, const ModuleName& currentModuleName)
        : fileResolver(fileResolver)
        , currentModuleName(currentModuleName)
    {
        LUAU_ASSERT(!FFlag::LuauNewRequireTrace);
    }

    FileResolver* const fileResolver;
    ModuleName currentModuleName;
    DenseHashMap<AstLocal*, ModuleName> locals{nullptr};
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

                const ModuleInfo* info = result.exprs.find(expr);
                if (info)
                    locals[local] = info->name;
            }
        }

        return false;
    }

    bool visit(AstExprGlobal* global) override
    {
        std::optional<ModuleName> name = fromAstFragment(global);
        if (name)
            result.exprs[global] = {*name};

        return false;
    }

    bool visit(AstExprLocal* local) override
    {
        const ModuleName* name = locals.find(local->local);
        if (name)
            result.exprs[local] = {*name};

        return false;
    }

    bool visit(AstExprIndexName* indexName) override
    {
        indexName->expr->visit(this);

        const ModuleInfo* info = result.exprs.find(indexName->expr);
        if (info)
        {
            if (indexName->index == "parent" || indexName->index == "Parent")
            {
                if (auto parent = fileResolver->getParentModuleName(info->name))
                    result.exprs[indexName] = {*parent};
            }
            else
                result.exprs[indexName] = {fileResolver->concat(info->name, indexName->index.value)};
        }

        return false;
    }

    bool visit(AstExprIndexExpr* indexExpr) override
    {
        indexExpr->expr->visit(this);

        const ModuleInfo* info = result.exprs.find(indexExpr->expr);
        const AstExprConstantString* str = indexExpr->index->as<AstExprConstantString>();
        if (info && str)
        {
            result.exprs[indexExpr] = {fileResolver->concat(info->name, std::string_view(str->value.data, str->value.size))};
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
            if (const ModuleInfo* moduleInfo = result.exprs.find(call->args.data[0]))
                result.requires.push_back({moduleInfo->name, call->location});

            return false;
        }

        AstExprIndexName* indexName = call->func->as<AstExprIndexName>();
        if (!indexName)
            return false;

        std::optional<ModuleName> rootName = fromAstFragment(indexName->expr);

        if (FFlag::LuauTraceRequireLookupChild && !rootName)
        {
            if (const ModuleInfo* moduleInfo = result.exprs.find(indexName->expr))
                rootName = moduleInfo->name;
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

        result.exprs[call] = {fileResolver->concat(*rootName, v)};

        // 'WaitForChild' can be used on modules that are not available at the typecheck time, but will be available at runtime
        // If we fail to find such module, we will not report an UnknownRequire error
        if (FFlag::LuauTraceRequireLookupChild && indexName->index == "WaitForChild")
            result.exprs[call].optional = true;

        return false;
    }
};

struct RequireTracer : AstVisitor
{
    RequireTracer(RequireTraceResult& result, FileResolver * fileResolver, const ModuleName& currentModuleName)
        : result(result)
        , fileResolver(fileResolver)
        , currentModuleName(currentModuleName)
        , locals(nullptr)
    {
        LUAU_ASSERT(FFlag::LuauNewRequireTrace);
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
            requires.push_back(expr);

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
        work.reserve(requires.size());

        for (AstExprCall* require: requires)
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
        result.requires.reserve(requires.size());

        for (AstExprCall* require : requires)
        {
            AstExpr* arg = require->args.data[0];

            if (const ModuleInfo* info = result.exprs.find(arg))
            {
                result.requires.push_back({info->name, require->location});

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
    std::vector<AstExprCall*> requires;
};

} // anonymous namespace

RequireTraceResult traceRequires(FileResolver* fileResolver, AstStatBlock* root, const ModuleName& currentModuleName)
{
    if (FFlag::LuauNewRequireTrace)
    {
        RequireTraceResult result;
        RequireTracer tracer{result, fileResolver, currentModuleName};
        root->visit(&tracer);
        tracer.process();
        return result;
    }
    else
    {
        RequireTracerOld tracer{fileResolver, currentModuleName};
        root->visit(&tracer);
        return tracer.result;
    }
}

} // namespace Luau
