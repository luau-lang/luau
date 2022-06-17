// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Linter.h"

#include "Luau/AstQuery.h"
#include "Luau/Module.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/StringUtils.h"
#include "Luau/Common.h"

#include <algorithm>
#include <math.h>
#include <limits.h>

LUAU_FASTINTVARIABLE(LuauSuggestionDistance, 4)
LUAU_FASTFLAGVARIABLE(LuauLintGlobalNeverReadBeforeWritten, false)

namespace Luau
{

// clang-format off
static const char* kWarningNames[] = {
    "Unknown",

    "UnknownGlobal",
    "DeprecatedGlobal",
    "GlobalUsedAsLocal",
    "LocalShadow",
    "SameLineStatement",
    "MultiLineStatement",
    "LocalUnused",
    "FunctionUnused",
    "ImportUnused",
    "BuiltinGlobalWrite",
    "PlaceholderRead",
    "UnreachableCode",
    "UnknownType",
    "ForRange",
    "UnbalancedAssignment",
    "ImplicitReturn",
    "DuplicateLocal",
    "FormatString",
    "TableLiteral",
    "UninitializedLocal",
    "DuplicateFunction",
    "DeprecatedApi",
    "TableOperations",
    "DuplicateCondition",
    "MisleadingAndOr",
    "CommentDirective",
};
// clang-format on

static_assert(std::size(kWarningNames) == unsigned(LintWarning::Code__Count), "did you forget to add warning to the list?");

struct LintContext
{
    struct Global
    {
        TypeId type = nullptr;
        std::optional<const char*> deprecated;
    };

    std::vector<LintWarning> result;
    LintOptions options;

    AstStat* root;

    AstName placeholder;
    DenseHashMap<AstName, Global> builtinGlobals;
    ScopePtr scope;
    const Module* module;

    LintContext()
        : root(nullptr)
        , builtinGlobals(AstName())
        , module(nullptr)
    {
    }

    bool warningEnabled(LintWarning::Code code)
    {
        return (options.warningMask & (1ull << code)) != 0;
    }

    std::optional<TypeId> getType(AstExpr* expr)
    {
        if (!module)
            return std::nullopt;

        auto it = module->astTypes.find(expr);
        if (!it)
            return std::nullopt;

        return *it;
    }
};

struct WarningComparator
{
    int compare(const Position& lhs, const Position& rhs) const
    {
        if (lhs.line != rhs.line)
            return lhs.line < rhs.line ? -1 : 1;
        if (lhs.column != rhs.column)
            return lhs.column < rhs.column ? -1 : 1;
        return 0;
    }

    int compare(const Location& lhs, const Location& rhs) const
    {
        if (int c = compare(lhs.begin, rhs.begin))
            return c;
        if (int c = compare(lhs.end, rhs.end))
            return c;
        return 0;
    }

    bool operator()(const LintWarning& lhs, const LintWarning& rhs) const
    {
        if (int c = compare(lhs.location, rhs.location))
            return c < 0;

        return lhs.code < rhs.code;
    }
};

LUAU_PRINTF_ATTR(4, 5)
static void emitWarning(LintContext& context, LintWarning::Code code, const Location& location, const char* format, ...)
{
    if (!context.warningEnabled(code))
        return;

    va_list args;
    va_start(args, format);
    std::string message = vformat(format, args);
    va_end(args);

    LintWarning warning = {code, location, message};
    context.result.push_back(warning);
}

static bool similar(AstExpr* lhs, AstExpr* rhs)
{
    if (lhs->classIndex != rhs->classIndex)
        return false;

#define CASE(T) else if (T* le = lhs->as<T>(), *re = rhs->as<T>(); le && re)

    if (false)
        return false;
    CASE(AstExprGroup) return similar(le->expr, re->expr);
    CASE(AstExprConstantNil) return true;
    CASE(AstExprConstantBool) return le->value == re->value;
    CASE(AstExprConstantNumber) return le->value == re->value;
    CASE(AstExprConstantString) return le->value.size == re->value.size && memcmp(le->value.data, re->value.data, le->value.size) == 0;
    CASE(AstExprLocal) return le->local == re->local;
    CASE(AstExprGlobal) return le->name == re->name;
    CASE(AstExprVarargs) return true;
    CASE(AstExprIndexName) return le->index == re->index && similar(le->expr, re->expr);
    CASE(AstExprIndexExpr) return similar(le->expr, re->expr) && similar(le->index, re->index);
    CASE(AstExprFunction) return false; // rarely meaningful in context of this pass, avoids having to process statement nodes
    CASE(AstExprUnary) return le->op == re->op && similar(le->expr, re->expr);
    CASE(AstExprBinary) return le->op == re->op && similar(le->left, re->left) && similar(le->right, re->right);
    CASE(AstExprTypeAssertion) return le->expr == re->expr; // the type doesn't affect execution semantics, avoids having to process type nodes
    CASE(AstExprError) return false;
    CASE(AstExprCall)
    {
        if (le->args.size != re->args.size || le->self != re->self)
            return false;

        if (!similar(le->func, re->func))
            return false;

        for (size_t i = 0; i < le->args.size; ++i)
            if (!similar(le->args.data[i], re->args.data[i]))
                return false;

        return true;
    }
    CASE(AstExprTable)
    {
        if (le->items.size != re->items.size)
            return false;

        for (size_t i = 0; i < le->items.size; ++i)
        {
            const AstExprTable::Item& li = le->items.data[i];
            const AstExprTable::Item& ri = re->items.data[i];

            if (li.kind != ri.kind)
                return false;

            if (bool(li.key) != bool(ri.key))
                return false;
            else if (li.key && !similar(li.key, ri.key))
                return false;

            if (!similar(li.value, ri.value))
                return false;
        }

        return true;
    }
    CASE(AstExprIfElse) return similar(le->condition, re->condition) && similar(le->trueExpr, re->trueExpr) && similar(le->falseExpr, re->falseExpr);
    else
    {
        LUAU_ASSERT(!"Unknown expression type");
        return false;
    }

#undef CASE
}

class LintGlobalLocal : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintGlobalLocal pass;
        pass.context = &context;

        for (auto& global : context.builtinGlobals)
        {
            Global& g = pass.globals[global.first];

            g.builtin = true;
            g.deprecated = global.second.deprecated;
        }

        context.root->visit(&pass);

        pass.report();
    }

private:
    struct FunctionInfo
    {
        explicit FunctionInfo(AstExprFunction* ast)
            : ast(ast)
            , dominatedGlobals({})
            , conditionalExecution(false)
        {
        }

        AstExprFunction* ast;
        DenseHashSet<AstName> dominatedGlobals;
        bool conditionalExecution;
    };

    struct Global
    {
        AstExprGlobal* firstRef = nullptr;

        std::vector<AstExprFunction*> functionRef;

        bool assigned = false;
        bool builtin = false;
        bool definedInModuleScope = false;
        bool definedAsFunction = false;
        bool readBeforeWritten = false;
        std::optional<const char*> deprecated;
    };

    LintContext* context;

    DenseHashMap<AstName, Global> globals;
    std::vector<AstExprGlobal*> globalRefs;
    std::vector<FunctionInfo> functionStack;


    LintGlobalLocal()
        : globals(AstName())
    {
    }

    void report()
    {
        for (size_t i = 0; i < globalRefs.size(); ++i)
        {
            AstExprGlobal* gv = globalRefs[i];
            Global* g = globals.find(gv->name);

            if (!g || (!g->assigned && !g->builtin))
                emitWarning(*context, LintWarning::Code_UnknownGlobal, gv->location, "Unknown global '%s'", gv->name.value);
            else if (g->deprecated)
            {
                if (*g->deprecated)
                    emitWarning(*context, LintWarning::Code_DeprecatedGlobal, gv->location, "Global '%s' is deprecated, use '%s' instead",
                        gv->name.value, *g->deprecated);
                else
                    emitWarning(*context, LintWarning::Code_DeprecatedGlobal, gv->location, "Global '%s' is deprecated", gv->name.value);
            }
        }

        for (auto& global : globals)
        {
            const Global& g = global.second;

            if (g.functionRef.size() && g.assigned && g.firstRef->name != context->placeholder)
            {
                AstExprFunction* top = g.functionRef.back();

                if (top->debugname.value)
                    emitWarning(*context, LintWarning::Code_GlobalUsedAsLocal, g.firstRef->location,
                        "Global '%s' is only used in the enclosing function '%s'; consider changing it to local", g.firstRef->name.value,
                        top->debugname.value);
                else
                    emitWarning(*context, LintWarning::Code_GlobalUsedAsLocal, g.firstRef->location,
                        "Global '%s' is only used in the enclosing function defined at line %d; consider changing it to local",
                        g.firstRef->name.value, top->location.begin.line + 1);
            }
            else if (FFlag::LuauLintGlobalNeverReadBeforeWritten && g.assigned && !g.readBeforeWritten && !g.definedInModuleScope &&
                     g.firstRef->name != context->placeholder)
            {
                emitWarning(*context, LintWarning::Code_GlobalUsedAsLocal, g.firstRef->location,
                    "Global '%s' is never read before being written. Consider changing it to local", g.firstRef->name.value);
            }
        }
    }

    bool visit(AstExprFunction* node) override
    {
        functionStack.emplace_back(node);

        node->body->visit(this);

        functionStack.pop_back();

        return false;
    }

    bool visit(AstExprGlobal* node) override
    {
        if (FFlag::LuauLintGlobalNeverReadBeforeWritten && !functionStack.empty() && !functionStack.back().dominatedGlobals.contains(node->name))
        {
            Global& g = globals[node->name];
            g.readBeforeWritten = true;
        }
        trackGlobalRef(node);

        if (node->name == context->placeholder)
            emitWarning(
                *context, LintWarning::Code_PlaceholderRead, node->location, "Placeholder value '_' is read here; consider using a named variable");

        return true;
    }

    bool visit(AstExprLocal* node) override
    {
        if (node->local->name == context->placeholder)
            emitWarning(
                *context, LintWarning::Code_PlaceholderRead, node->location, "Placeholder value '_' is read here; consider using a named variable");

        return true;
    }

    bool visit(AstStatAssign* node) override
    {
        for (size_t i = 0; i < node->vars.size; ++i)
        {
            AstExpr* var = node->vars.data[i];

            if (AstExprGlobal* gv = var->as<AstExprGlobal>())
            {
                Global& g = globals[gv->name];

                if (FFlag::LuauLintGlobalNeverReadBeforeWritten)
                {
                    if (functionStack.empty())
                    {
                        g.definedInModuleScope = true;
                    }
                    else
                    {
                        if (!functionStack.back().conditionalExecution)
                        {
                            functionStack.back().dominatedGlobals.insert(gv->name);
                        }
                    }
                }

                if (g.builtin)
                    emitWarning(*context, LintWarning::Code_BuiltinGlobalWrite, gv->location,
                        "Built-in global '%s' is overwritten here; consider using a local or changing the name", gv->name.value);
                else
                    g.assigned = true;

                trackGlobalRef(gv);
            }
            else if (var->is<AstExprLocal>())
            {
                // We don't visit locals here because it's a local *write*, and visit(AstExprLocal*) assumes it's a local *read*
            }
            else
            {
                var->visit(this);
            }
        }

        for (size_t i = 0; i < node->values.size; ++i)
            node->values.data[i]->visit(this);

        return false;
    }

    bool visit(AstStatFunction* node) override
    {
        if (AstExprGlobal* gv = node->name->as<AstExprGlobal>())
        {
            Global& g = globals[gv->name];

            if (g.builtin)
                emitWarning(*context, LintWarning::Code_BuiltinGlobalWrite, gv->location,
                    "Built-in global '%s' is overwritten here; consider using a local or changing the name", gv->name.value);
            else
            {
                g.assigned = true;
                if (FFlag::LuauLintGlobalNeverReadBeforeWritten)
                {
                    g.definedAsFunction = true;
                    g.definedInModuleScope = functionStack.empty();
                }
            }

            trackGlobalRef(gv);
        }

        return true;
    }

    class HoldConditionalExecution
    {
    public:
        HoldConditionalExecution(LintGlobalLocal& p)
            : p(p)
        {
            if (!p.functionStack.empty() && !p.functionStack.back().conditionalExecution)
            {
                resetToFalse = true;
                p.functionStack.back().conditionalExecution = true;
            }
        }
        ~HoldConditionalExecution()
        {
            if (resetToFalse)
                p.functionStack.back().conditionalExecution = false;
        }

    private:
        bool resetToFalse = false;
        LintGlobalLocal& p;
    };

    bool visit(AstStatIf* node) override
    {
        if (!FFlag::LuauLintGlobalNeverReadBeforeWritten)
            return true;

        HoldConditionalExecution ce(*this);
        node->condition->visit(this);
        node->thenbody->visit(this);
        if (node->elsebody)
            node->elsebody->visit(this);

        return false;
    }

    bool visit(AstStatWhile* node) override
    {
        if (!FFlag::LuauLintGlobalNeverReadBeforeWritten)
            return true;

        HoldConditionalExecution ce(*this);
        node->condition->visit(this);
        node->body->visit(this);

        return false;
    }

    bool visit(AstStatRepeat* node) override
    {
        if (!FFlag::LuauLintGlobalNeverReadBeforeWritten)
            return true;

        HoldConditionalExecution ce(*this);
        node->condition->visit(this);
        node->body->visit(this);

        return false;
    }

    bool visit(AstStatFor* node) override
    {
        if (!FFlag::LuauLintGlobalNeverReadBeforeWritten)
            return true;

        HoldConditionalExecution ce(*this);
        node->from->visit(this);
        node->to->visit(this);

        if (node->step)
            node->step->visit(this);

        node->body->visit(this);

        return false;
    }

    bool visit(AstStatForIn* node) override
    {
        if (!FFlag::LuauLintGlobalNeverReadBeforeWritten)
            return true;

        HoldConditionalExecution ce(*this);
        for (AstExpr* expr : node->values)
            expr->visit(this);

        node->body->visit(this);

        return false;
    }

    void trackGlobalRef(AstExprGlobal* node)
    {
        Global& g = globals[node->name];

        globalRefs.push_back(node);

        if (!g.firstRef)
        {
            g.firstRef = node;

            // to reduce the cost of tracking we only track this for user globals
            if (!g.builtin)
            {
                g.functionRef.clear();
                g.functionRef.reserve(functionStack.size());
                for (const FunctionInfo& entry : functionStack)
                {
                    g.functionRef.push_back(entry.ast);
                }
            }
        }
        else
        {
            // to reduce the cost of tracking we only track this for user globals
            if (!g.builtin)
            {
                // we need to find a common prefix between all uses of a global
                size_t prefix = 0;

                while (prefix < g.functionRef.size() && prefix < functionStack.size() && g.functionRef[prefix] == functionStack[prefix].ast)
                    prefix++;

                g.functionRef.resize(prefix);
            }
        }
    }
};

class LintSameLineStatement : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintSameLineStatement pass;

        pass.context = &context;
        pass.lastLine = ~0u;

        context.root->visit(&pass);
    }

private:
    LintContext* context;
    unsigned int lastLine;

    bool visit(AstStatBlock* node) override
    {
        for (size_t i = 1; i < node->body.size; ++i)
        {
            const Location& last = node->body.data[i - 1]->location;
            const Location& location = node->body.data[i]->location;

            if (location.begin.line != last.end.line)
                continue;

            // We warn once per line with multiple statements
            if (location.begin.line == lastLine)
                continue;

            // There's a common pattern where local variables are computed inside a do block that starts on the same line; we white-list this pattern
            if (node->body.data[i - 1]->is<AstStatLocal>() && node->body.data[i]->is<AstStatBlock>())
                continue;

            // Another common pattern is using multiple statements on the same line with semi-colons on each of them. White-list this pattern too.
            if (node->body.data[i - 1]->hasSemicolon)
                continue;

            emitWarning(*context, LintWarning::Code_SameLineStatement, location,
                "A new statement is on the same line; add semi-colon on previous statement to silence");

            lastLine = location.begin.line;
        }

        return true;
    }
};

class LintMultiLineStatement : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintMultiLineStatement pass;
        pass.context = &context;

        context.root->visit(&pass);
    }

private:
    LintContext* context;

    struct Statement
    {
        Location start;
        unsigned int lastLine;
        bool flagged;
    };

    std::vector<Statement> stack;

    bool visit(AstExpr* node) override
    {
        Statement& top = stack.back();

        if (!top.flagged)
        {
            Location location = node->location;

            if (location.begin.line > top.lastLine)
            {
                top.lastLine = location.begin.line;

                if (location.begin.column <= top.start.begin.column)
                {
                    emitWarning(
                        *context, LintWarning::Code_MultiLineStatement, location, "Statement spans multiple lines; use indentation to silence");

                    top.flagged = true;
                }
            }
        }

        return true;
    }

    bool visit(AstExprTable* node) override
    {
        (void)node;

        return false;
    }

    bool visit(AstStatRepeat* node) override
    {
        node->body->visit(this);

        return false;
    }

    bool visit(AstStatBlock* node) override
    {
        for (size_t i = 0; i < node->body.size; ++i)
        {
            AstStat* stmt = node->body.data[i];

            Statement s = {stmt->location, stmt->location.begin.line, false};
            stack.push_back(s);

            stmt->visit(this);

            stack.pop_back();
        }

        return false;
    }
};

class LintLocalHygiene : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintLocalHygiene pass;
        pass.context = &context;

        for (auto& global : context.builtinGlobals)
            pass.globals[global.first].builtin = true;

        context.root->visit(&pass);

        pass.report();
    }

private:
    LintContext* context;

    struct Local
    {
        AstNode* defined = nullptr;
        bool function;
        bool import;
        bool used;
        bool arg;
    };

    struct Global
    {
        bool used;
        bool builtin;
        AstExprGlobal* firstRef;
    };

    DenseHashMap<AstLocal*, Local> locals;
    DenseHashMap<AstName, AstLocal*> imports;
    DenseHashMap<AstName, Global> globals;

    LintLocalHygiene()
        : locals(NULL)
        , imports(AstName())
        , globals(AstName())
    {
    }

    void report()
    {
        for (auto& l : locals)
        {
            if (l.second.used)
                reportUsedLocal(l.first, l.second);
            else if (l.second.defined)
                reportUnusedLocal(l.first, l.second);
        }
    }

    void reportUsedLocal(AstLocal* local, const Local& info)
    {
        if (AstLocal* shadow = local->shadow)
        {
            // LintDuplicateFunctions will catch this.
            Local* shadowLocal = locals.find(shadow);
            if (context->options.isEnabled(LintWarning::Code_DuplicateFunction) && info.function && shadowLocal && shadowLocal->function)
                return;

            // LintDuplicateLocal will catch this.
            if (context->options.isEnabled(LintWarning::Code_DuplicateLocal) && shadowLocal && shadowLocal->defined == info.defined)
                return;

            // don't warn on inter-function shadowing since it is much more fragile wrt refactoring
            if (shadow->functionDepth == local->functionDepth)
                emitWarning(*context, LintWarning::Code_LocalShadow, local->location, "Variable '%s' shadows previous declaration at line %d",
                    local->name.value, shadow->location.begin.line + 1);
        }
        else if (Global* global = globals.find(local->name))
        {
            if (global->builtin)
                ; // there are many builtins with common names like 'table'; some of them are deprecated as well
            else if (global->firstRef)
            {
                emitWarning(*context, LintWarning::Code_LocalShadow, local->location, "Variable '%s' shadows a global variable used at line %d",
                    local->name.value, global->firstRef->location.begin.line + 1);
            }
            else
            {
                emitWarning(*context, LintWarning::Code_LocalShadow, local->location, "Variable '%s' shadows a global variable", local->name.value);
            }
        }
    }

    void reportUnusedLocal(AstLocal* local, const Local& info)
    {
        if (local->name.value[0] == '_')
            return;

        if (info.function)
            emitWarning(*context, LintWarning::Code_FunctionUnused, local->location, "Function '%s' is never used; prefix with '_' to silence",
                local->name.value);
        else if (info.import)
            emitWarning(*context, LintWarning::Code_ImportUnused, local->location, "Import '%s' is never used; prefix with '_' to silence",
                local->name.value);
        else
            emitWarning(*context, LintWarning::Code_LocalUnused, local->location, "Variable '%s' is never used; prefix with '_' to silence",
                local->name.value);
    }

    bool isRequireCall(AstExpr* expr)
    {
        AstExprCall* call = expr->as<AstExprCall>();
        if (!call)
            return false;

        AstExprGlobal* glob = call->func->as<AstExprGlobal>();
        if (!glob)
            return false;

        return glob->name == "require";
    }

    bool visit(AstStatAssign* node) override
    {
        for (AstExpr* var : node->vars)
        {
            // We don't visit locals here because it's a local *write*, and visit(AstExprLocal*) assumes it's a local *read*
            if (!var->is<AstExprLocal>())
                var->visit(this);
        }

        for (AstExpr* value : node->values)
            value->visit(this);

        return false;
    }

    bool visit(AstStatLocal* node) override
    {
        if (node->vars.size == 1 && node->values.size == 1)
        {
            Local& l = locals[node->vars.data[0]];

            l.defined = node;
            l.import = isRequireCall(node->values.data[0]);

            if (l.import)
                imports[node->vars.data[0]->name] = node->vars.data[0];
        }
        else
        {
            for (size_t i = 0; i < node->vars.size; ++i)
            {
                Local& l = locals[node->vars.data[i]];

                l.defined = node;
            }
        }

        return true;
    }

    bool visit(AstStatLocalFunction* node) override
    {
        Local& l = locals[node->name];

        l.defined = node;
        l.function = true;

        return true;
    }

    bool visit(AstExprLocal* node) override
    {
        Local& l = locals[node->local];

        l.used = true;

        return true;
    }

    bool visit(AstExprGlobal* node) override
    {
        Global& global = globals[node->name];

        global.used = true;
        if (!global.firstRef)
            global.firstRef = node;

        return true;
    }

    bool visit(AstType* node) override
    {
        return true;
    }

    bool visit(AstTypeReference* node) override
    {
        if (!node->prefix)
            return true;

        if (!imports.contains(*node->prefix))
            return true;

        AstLocal* astLocal = imports[*node->prefix];
        Local& local = locals[astLocal];
        LUAU_ASSERT(local.import);
        local.used = true;

        return true;
    }

    bool visit(AstExprFunction* node) override
    {
        if (node->self)
            locals[node->self].arg = true;

        for (size_t i = 0; i < node->args.size; ++i)
            locals[node->args.data[i]].arg = true;

        return true;
    }
};

class LintUnusedFunction : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintUnusedFunction pass;
        pass.context = &context;

        context.root->visit(&pass);

        pass.report();
    }

private:
    LintContext* context;

    struct Global
    {
        Location location;
        bool function;
        bool used;
    };

    DenseHashMap<AstName, Global> globals;

    LintUnusedFunction()
        : globals(AstName())
    {
    }

    void report()
    {
        for (auto& g : globals)
        {
            if (g.second.function && !g.second.used && g.first.value[0] != '_')
                emitWarning(*context, LintWarning::Code_FunctionUnused, g.second.location, "Function '%s' is never used; prefix with '_' to silence",
                    g.first.value);
        }
    }

    bool visit(AstStatFunction* node) override
    {
        if (AstExprGlobal* expr = node->name->as<AstExprGlobal>())
        {
            Global& g = globals[expr->name];

            g.function = true;
            g.location = expr->location;

            node->func->visit(this);

            return false;
        }

        return true;
    }

    bool visit(AstExprGlobal* node) override
    {
        Global& g = globals[node->name];

        g.used = true;

        return true;
    }
};

class LintUnreachableCode : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintUnreachableCode pass;
        pass.context = &context;

        pass.analyze(context.root);
        context.root->visit(&pass);
    }

private:
    LintContext* context;

    // Note: this enum is order-sensitive!
    // The order is in the "severity" of the termination and affects merging of status codes from different branches
    // For example, if one branch breaks and one returns, the merged result is "break"
    enum Status
    {
        Unknown,
        Continue,
        Break,
        Return,
        Error,
    };

    const char* getReason(Status status)
    {
        switch (status)
        {
        case Continue:
            return "continue";

        case Break:
            return "break";

        case Return:
            return "return";

        case Error:
            return "error";

        default:
            return "unknown";
        }
    }

    Status analyze(AstStat* node)
    {
        if (AstStatBlock* stat = node->as<AstStatBlock>())
        {
            for (size_t i = 0; i < stat->body.size; ++i)
            {
                AstStat* si = stat->body.data[i];
                Status step = analyze(si);

                if (step != Unknown)
                {
                    if (i + 1 == stat->body.size)
                        return step;

                    AstStat* next = stat->body.data[i + 1];

                    // silence the warning for common pattern of Error (coming from error()) + Return
                    if (step == Error && si->is<AstStatExpr>() && next->is<AstStatReturn>() && i + 2 == stat->body.size)
                        return Error;

                    emitWarning(*context, LintWarning::Code_UnreachableCode, next->location, "Unreachable code (previous statement always %ss)",
                        getReason(step));
                    return step;
                }
            }

            return Unknown;
        }
        else if (AstStatIf* stat = node->as<AstStatIf>())
        {
            Status ifs = analyze(stat->thenbody);
            Status elses = stat->elsebody ? analyze(stat->elsebody) : Unknown;

            return std::min(ifs, elses);
        }
        else if (AstStatWhile* stat = node->as<AstStatWhile>())
        {
            analyze(stat->body);

            return Unknown;
        }
        else if (AstStatRepeat* stat = node->as<AstStatRepeat>())
        {
            analyze(stat->body);

            return Unknown;
        }
        else if (node->is<AstStatBreak>())
        {
            return Break;
        }
        else if (node->is<AstStatContinue>())
        {
            return Continue;
        }
        else if (node->is<AstStatReturn>())
        {
            return Return;
        }
        else if (AstStatExpr* stat = node->as<AstStatExpr>())
        {
            if (AstExprCall* call = stat->expr->as<AstExprCall>())
                if (doesCallError(call))
                    return Error;

            return Unknown;
        }
        else if (AstStatFor* stat = node->as<AstStatFor>())
        {
            analyze(stat->body);

            return Unknown;
        }
        else if (AstStatForIn* stat = node->as<AstStatForIn>())
        {
            analyze(stat->body);

            return Unknown;
        }
        else
        {
            return Unknown;
        }
    }

    bool visit(AstExprFunction* node) override
    {
        analyze(node->body);

        return true;
    }
};

class LintUnknownType : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintUnknownType pass;
        pass.context = &context;

        context.root->visit(&pass);
    }

private:
    LintContext* context;

    enum TypeKind
    {
        Kind_Unknown,
        Kind_Primitive, // primitive type supported by VM - boolean/userdata/etc. No differentiation between types of userdata.
        Kind_Vector,    // 'vector' but only used when type is used
        Kind_Userdata,  // custom userdata type
    };

    TypeKind getTypeKind(const std::string& name)
    {
        if (name == "nil" || name == "boolean" || name == "userdata" || name == "number" || name == "string" || name == "table" ||
            name == "function" || name == "thread")
            return Kind_Primitive;

        if (name == "vector")
            return Kind_Vector;

        if (std::optional<TypeFun> maybeTy = context->scope->lookupType(name))
            return Kind_Userdata;

        return Kind_Unknown;
    }

    void validateType(AstExprConstantString* expr, std::initializer_list<TypeKind> expected, const char* expectedString)
    {
        std::string name(expr->value.data, expr->value.size);
        TypeKind kind = getTypeKind(name);

        if (kind == Kind_Unknown)
        {
            emitWarning(*context, LintWarning::Code_UnknownType, expr->location, "Unknown type '%s'", name.c_str());
            return;
        }

        for (TypeKind ek : expected)
        {
            if (kind == ek)
                return;
        }

        emitWarning(*context, LintWarning::Code_UnknownType, expr->location, "Unknown type '%s' (expected %s)", name.c_str(), expectedString);
    }

    bool visit(AstExprBinary* node) override
    {
        if (node->op == AstExprBinary::CompareNe || node->op == AstExprBinary::CompareEq)
        {
            AstExpr* lhs = node->left;
            AstExpr* rhs = node->right;

            if (!rhs->is<AstExprConstantString>())
                std::swap(lhs, rhs);

            AstExprCall* call = lhs->as<AstExprCall>();
            AstExprConstantString* arg = rhs->as<AstExprConstantString>();

            if (call && arg)
            {
                AstExprGlobal* g = call->func->as<AstExprGlobal>();

                if (g && g->name == "type")
                {
                    validateType(arg, {Kind_Primitive, Kind_Vector}, "primitive type");
                }
                else if (g && g->name == "typeof")
                {
                    validateType(arg, {Kind_Primitive, Kind_Userdata}, "primitive or userdata type");
                }
            }
        }

        return true;
    }
};

class LintForRange : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintForRange pass;
        pass.context = &context;

        context.root->visit(&pass);
    }

private:
    LintContext* context;

    double getLoopEnd(double from, double to)
    {
        return from + floor(to - from);
    }

    bool visit(AstStatFor* node) override
    {
        // note: we silence all warnings below if *any* step is specified, assuming that the user knows best
        if (!node->step)
        {
            AstExprConstantNumber* fc = node->from->as<AstExprConstantNumber>();
            AstExprUnary* fu = node->from->as<AstExprUnary>();
            AstExprConstantNumber* tc = node->to->as<AstExprConstantNumber>();
            AstExprUnary* tu = node->to->as<AstExprUnary>();

            Location rangeLocation(node->from->location, node->to->location);

            // for i=#t,1 do
            if (fu && fu->op == AstExprUnary::Len && tc && tc->value == 1.0)
                emitWarning(
                    *context, LintWarning::Code_ForRange, rangeLocation, "For loop should iterate backwards; did you forget to specify -1 as step?");
            // for i=8,1 do
            else if (fc && tc && fc->value > tc->value)
                emitWarning(
                    *context, LintWarning::Code_ForRange, rangeLocation, "For loop should iterate backwards; did you forget to specify -1 as step?");
            // for i=1,8.75 do
            else if (fc && tc && getLoopEnd(fc->value, tc->value) != tc->value)
                emitWarning(*context, LintWarning::Code_ForRange, rangeLocation, "For loop ends at %g instead of %g; did you forget to specify step?",
                    getLoopEnd(fc->value, tc->value), tc->value);
            // for i=0,#t do
            else if (fc && tu && fc->value == 0.0 && tu->op == AstExprUnary::Len)
                emitWarning(*context, LintWarning::Code_ForRange, rangeLocation, "For loop starts at 0, but arrays start at 1");
            // for i=#t,0 do
            else if (fu && fu->op == AstExprUnary::Len && tc && tc->value == 0.0)
                emitWarning(*context, LintWarning::Code_ForRange, rangeLocation,
                    "For loop should iterate backwards; did you forget to specify -1 as step? Also consider changing 0 to 1 since arrays start at 1");
        }

        return true;
    }
};

class LintUnbalancedAssignment : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintUnbalancedAssignment pass;
        pass.context = &context;

        context.root->visit(&pass);
    }

private:
    LintContext* context;

    void assign(size_t vars, const AstArray<AstExpr*>& values, const Location& location)
    {
        if (vars != values.size && values.size > 0)
        {
            AstExpr* last = values.data[values.size - 1];

            if (vars < values.size)
                emitWarning(*context, LintWarning::Code_UnbalancedAssignment, location,
                    "Assigning %d values to %d variables leaves some values unused", int(values.size), int(vars));
            else if (last->is<AstExprCall>() || last->is<AstExprVarargs>())
                ; // we don't know how many values the last expression returns
            else if (last->is<AstExprConstantNil>())
                ; // last expression is nil which explicitly silences the nil-init warning
            else
                emitWarning(*context, LintWarning::Code_UnbalancedAssignment, location,
                    "Assigning %d values to %d variables initializes extra variables with nil; add 'nil' to value list to silence", int(values.size),
                    int(vars));
        }
    }

    bool visit(AstStatLocal* node) override
    {
        assign(node->vars.size, node->values, node->location);

        return true;
    }

    bool visit(AstStatAssign* node) override
    {
        assign(node->vars.size, node->values, node->location);

        return true;
    }
};

class LintImplicitReturn : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintImplicitReturn pass;
        pass.context = &context;

        context.root->visit(&pass);
    }

private:
    LintContext* context;

    Location getEndLocation(const AstStat* node)
    {
        Location loc = node->location;

        if (node->is<AstStatExpr>() || node->is<AstStatAssign>() || node->is<AstStatLocal>())
            return loc;

        if (loc.begin.line == loc.end.line)
            return loc;

        // assume that we're in context of a statement that has an "end" block
        return Location(Position(loc.end.line, std::max(0, int(loc.end.column) - 3)), loc.end);
    }

    AstStatReturn* getValueReturn(AstStat* node)
    {
        struct Visitor : AstVisitor
        {
            AstStatReturn* result = nullptr;

            bool visit(AstExpr* node) override
            {
                (void)node;
                return false;
            }

            bool visit(AstStatReturn* node) override
            {
                if (!result && node->list.size > 0)
                    result = node;

                return false;
            }
        };

        Visitor visitor;
        node->visit(&visitor);
        return visitor.result;
    }

    bool visit(AstExprFunction* node) override
    {
        const AstStat* bodyf = getFallthrough(node->body);
        AstStat* vret = getValueReturn(node->body);

        if (bodyf && vret)
        {
            Location location = getEndLocation(bodyf);

            if (node->debugname.value)
                emitWarning(*context, LintWarning::Code_ImplicitReturn, location,
                    "Function '%s' can implicitly return no values even though there's an explicit return at line %d; add explicit return to silence",
                    node->debugname.value, vret->location.begin.line + 1);
            else
                emitWarning(*context, LintWarning::Code_ImplicitReturn, location,
                    "Function can implicitly return no values even though there's an explicit return at line %d; add explicit return to silence",
                    vret->location.begin.line + 1);
        }

        return true;
    }
};

class LintFormatString : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintFormatString pass;
        pass.context = &context;

        context.root->visit(&pass);
    }

    static void fuzz(const char* data, size_t size)
    {
        LintContext context;

        LintFormatString pass;
        pass.context = &context;

        pass.checkStringFormat(data, size);
        pass.checkStringPack(data, size, false);
        pass.checkStringMatch(data, size);
        pass.checkStringReplace(data, size, -1);
        pass.checkDateFormat(data, size);
    }

private:
    LintContext* context;

    static inline bool isAlpha(char ch)
    {
        // use or trick to convert to lower case and unsigned comparison to do range check
        return unsigned((ch | ' ') - 'a') < 26;
    }

    static inline bool isDigit(char ch)
    {
        // use unsigned comparison to do range check for performance
        return unsigned(ch - '0') < 10;
    }

    const char* checkStringFormat(const char* data, size_t size)
    {
        const char* flags = "-+ #0";
        const char* options = "cdiouxXeEfgGqs";

        for (size_t i = 0; i < size; ++i)
        {
            if (data[i] == '%')
            {
                i++;

                // escaped % doesn't allow for flags/etc.
                if (i < size && data[i] == '%')
                    continue;

                // skip flags
                while (i < size && strchr(flags, data[i]))
                    i++;

                // skip width (up to two digits)
                if (i < size && isDigit(data[i]))
                    i++;
                if (i < size && isDigit(data[i]))
                    i++;

                // skip precision
                if (i < size && data[i] == '.')
                {
                    i++;

                    // up to two digits
                    if (i < size && isDigit(data[i]))
                        i++;
                    if (i < size && isDigit(data[i]))
                        i++;
                }

                if (i == size)
                    return "unfinished format specifier";

                if (!strchr(options, data[i]))
                    return "invalid format specifier: must be a string format specifier or %";
            }
        }

        return nullptr;
    }

    const char* checkStringPack(const char* data, size_t size, bool fixed)
    {
        const char* options = "<>=!bBhHlLjJTiIfdnczsxX ";
        const char* unsized = "<>=!zX ";

        for (size_t i = 0; i < size; ++i)
        {
            if (!strchr(options, data[i]))
                return "unexpected character; must be a pack specifier or space";

            if (data[i] == 'c' && (i + 1 == size || !isDigit(data[i + 1])))
                return "fixed-sized string format must specify the size";

            if (data[i] == 'X' && (i + 1 == size || strchr(unsized, data[i + 1])))
                return "X must be followed by a size specifier";

            if (fixed && (data[i] == 'z' || data[i] == 's'))
                return "pack specifier must be fixed-size";

            if ((data[i] == '!' || data[i] == 'i' || data[i] == 'I' || data[i] == 'c' || data[i] == 's') && i + 1 < size && isDigit(data[i + 1]))
            {
                bool isc = data[i] == 'c';

                unsigned int v = 0;
                while (i + 1 < size && isDigit(data[i + 1]) && v <= (INT_MAX - 9) / 10)
                {
                    v = v * 10 + (data[i + 1] - '0');
                    i++;
                }

                if (i + 1 < size && isDigit(data[i + 1]))
                    return "size specifier is too large";

                if (!isc && (v == 0 || v > 16))
                    return "integer size must be in range [1,16]";
            }
        }

        return nullptr;
    }

    const char* checkStringMatchSet(const char* data, size_t size, const char* magic, const char* classes)
    {
        for (size_t i = 0; i < size; ++i)
        {
            if (data[i] == '%')
            {
                i++;

                if (i == size)
                    return "unfinished character class";

                if (isDigit(data[i]))
                {
                    return "sets can not contain capture references";
                }
                else if (isAlpha(data[i]))
                {
                    // lower case lookup - upper case for every character class is defined as its inverse
                    if (!strchr(classes, data[i] | ' '))
                        return "invalid character class, must refer to a defined class or its inverse";
                }
                else
                {
                    // technically % can escape any non-alphanumeric character but this is error-prone
                    if (!strchr(magic, data[i]))
                        return "expected a magic character after %";
                }

                if (i + 1 < size && data[i + 1] == '-')
                    return "character range can't include character sets";
            }
            else if (data[i] == '-')
            {
                if (i + 1 < size && data[i + 1] == '%')
                    return "character range can't include character sets";
            }
        }

        return nullptr;
    }

    const char* checkStringMatch(const char* data, size_t size, int* outCaptures = nullptr)
    {
        const char* magic = "^$()%.[]*+-?)";
        const char* classes = "acdglpsuwxz";

        std::vector<int> openCaptures;
        int totalCaptures = 0;

        for (size_t i = 0; i < size; ++i)
        {
            if (data[i] == '%')
            {
                i++;

                if (i == size)
                    return "unfinished character class";

                if (isDigit(data[i]))
                {
                    if (data[i] == '0')
                        return "invalid capture reference, must be 1-9";

                    int captureIndex = data[i] - '0';

                    if (captureIndex > totalCaptures)
                        return "invalid capture reference, must refer to a valid capture";

                    for (int open : openCaptures)
                        if (open == captureIndex)
                            return "invalid capture reference, must refer to a closed capture";
                }
                else if (isAlpha(data[i]))
                {
                    if (data[i] == 'b')
                    {
                        if (i + 2 >= size)
                            return "missing brace characters for balanced match";

                        i += 2;
                    }
                    else if (data[i] == 'f')
                    {
                        if (i + 1 >= size || data[i + 1] != '[')
                            return "missing set after a frontier pattern";

                        // we can parse the set with the regular logic
                    }
                    else
                    {
                        // lower case lookup - upper case for every character class is defined as its inverse
                        if (!strchr(classes, data[i] | ' '))
                            return "invalid character class, must refer to a defined class or its inverse";
                    }
                }
                else
                {
                    // technically % can escape any non-alphanumeric character but this is error-prone
                    if (!strchr(magic, data[i]))
                        return "expected a magic character after %";
                }
            }
            else if (data[i] == '[')
            {
                size_t j = i + 1;

                // empty patterns don't exist as per grammar rules, so we skip leading ^ and ]
                if (j < size && data[j] == '^')
                    j++;

                if (j < size && data[j] == ']')
                    j++;

                // scan for the end of the pattern
                while (j < size && data[j] != ']')
                {
                    // % escapes the next character
                    if (j + 1 < size && data[j] == '%')
                        j++;

                    j++;
                }

                if (j == size)
                    return "expected ] at the end of the string to close a set";

                if (const char* error = checkStringMatchSet(data + i + 1, j - i - 1, magic, classes))
                    return error;

                LUAU_ASSERT(data[j] == ']');
                i = j;
            }
            else if (data[i] == '(')
            {
                totalCaptures++;
                openCaptures.push_back(totalCaptures);
            }
            else if (data[i] == ')')
            {
                if (openCaptures.empty())
                    return "unexpected ) without a matching (";
                openCaptures.pop_back();
            }
        }

        if (!openCaptures.empty())
            return "expected ) at the end of the string to close a capture";

        if (outCaptures)
            *outCaptures = totalCaptures;

        return nullptr;
    }

    const char* checkStringReplace(const char* data, size_t size, int captures)
    {
        for (size_t i = 0; i < size; ++i)
        {
            if (data[i] == '%')
            {
                i++;

                if (i == size)
                    return "unfinished replacement";

                if (data[i] != '%' && !isDigit(data[i]))
                    return "unexpected replacement character; must be a digit or %";

                if (isDigit(data[i]) && captures >= 0 && data[i] - '0' > captures)
                    return "invalid capture index, must refer to pattern capture";
            }
        }

        return nullptr;
    }

    const char* checkDateFormat(const char* data, size_t size)
    {
        const char* options = "aAbBcdHIjmMpSUwWxXyYzZ";

        for (size_t i = 0; i < size; ++i)
        {
            if (data[i] == '%')
            {
                i++;

                if (i == size)
                    return "unfinished replacement";

                if (data[i] != '%' && !strchr(options, data[i]))
                    return "unexpected replacement character; must be a date format specifier or %";
            }

            if (data[i] == 0)
                return "date format can not contain null characters";
        }

        return nullptr;
    }

    void matchStringCall(AstName name, AstExpr* self, AstArray<AstExpr*> args)
    {
        if (name == "format")
        {
            if (AstExprConstantString* fmt = self->as<AstExprConstantString>())
                if (const char* error = checkStringFormat(fmt->value.data, fmt->value.size))
                    emitWarning(*context, LintWarning::Code_FormatString, fmt->location, "Invalid format string: %s", error);
        }
        else if (name == "pack" || name == "packsize" || name == "unpack")
        {
            if (AstExprConstantString* fmt = self->as<AstExprConstantString>())
                if (const char* error = checkStringPack(fmt->value.data, fmt->value.size, name == "packsize"))
                    emitWarning(*context, LintWarning::Code_FormatString, fmt->location, "Invalid pack format: %s", error);
        }
        else if ((name == "match" || name == "gmatch") && args.size > 0)
        {
            if (AstExprConstantString* pat = args.data[0]->as<AstExprConstantString>())
                if (const char* error = checkStringMatch(pat->value.data, pat->value.size))
                    emitWarning(*context, LintWarning::Code_FormatString, pat->location, "Invalid match pattern: %s", error);
        }
        else if (name == "find" && args.size > 0 && args.size <= 2)
        {
            if (AstExprConstantString* pat = args.data[0]->as<AstExprConstantString>())
                if (const char* error = checkStringMatch(pat->value.data, pat->value.size))
                    emitWarning(*context, LintWarning::Code_FormatString, pat->location, "Invalid match pattern: %s", error);
        }
        else if (name == "find" && args.size >= 3)
        {
            AstExprConstantBool* mode = args.data[2]->as<AstExprConstantBool>();

            // find(_, _, _, true) is a raw string find, not a pattern match
            if (mode && !mode->value)
                if (AstExprConstantString* pat = args.data[0]->as<AstExprConstantString>())
                    if (const char* error = checkStringMatch(pat->value.data, pat->value.size))
                        emitWarning(*context, LintWarning::Code_FormatString, pat->location, "Invalid match pattern: %s", error);
        }
        else if (name == "gsub" && args.size > 1)
        {
            int captures = -1;

            if (AstExprConstantString* pat = args.data[0]->as<AstExprConstantString>())
                if (const char* error = checkStringMatch(pat->value.data, pat->value.size, &captures))
                    emitWarning(*context, LintWarning::Code_FormatString, pat->location, "Invalid match pattern: %s", error);

            if (AstExprConstantString* rep = args.data[1]->as<AstExprConstantString>())
                if (const char* error = checkStringReplace(rep->value.data, rep->value.size, captures))
                    emitWarning(*context, LintWarning::Code_FormatString, rep->location, "Invalid match replacement: %s", error);
        }
    }

    void matchCall(AstExprCall* node)
    {
        AstExprIndexName* func = node->func->as<AstExprIndexName>();
        if (!func)
            return;

        if (node->self)
        {
            AstExprGroup* group = func->expr->as<AstExprGroup>();
            AstExpr* self = group ? group->expr : func->expr;

            if (self->is<AstExprConstantString>())
                matchStringCall(func->index, self, node->args);
            else if (std::optional<TypeId> type = context->getType(self))
                if (isString(*type))
                    matchStringCall(func->index, self, node->args);
            return;
        }

        AstExprGlobal* lib = func->expr->as<AstExprGlobal>();
        if (!lib)
            return;

        if (lib->name == "string")
        {
            if (node->args.size > 0)
            {
                AstArray<AstExpr*> rest = {node->args.data + 1, node->args.size - 1};

                matchStringCall(func->index, node->args.data[0], rest);
            }
        }
        else if (lib->name == "os")
        {
            if (func->index == "date" && node->args.size > 0)
            {
                if (AstExprConstantString* fmt = node->args.data[0]->as<AstExprConstantString>())
                    if (const char* error = checkDateFormat(fmt->value.data, fmt->value.size))
                        emitWarning(*context, LintWarning::Code_FormatString, fmt->location, "Invalid date format: %s", error);
            }
        }
    }

    bool visit(AstExprCall* node) override
    {
        matchCall(node);
        return true;
    }
};

class LintTableLiteral : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintTableLiteral pass;
        pass.context = &context;

        context.root->visit(&pass);
    }

private:
    LintContext* context;

    bool visit(AstExprTable* node) override
    {
        int count = 0;

        for (const AstExprTable::Item& item : node->items)
            if (item.kind == AstExprTable::Item::List)
                count++;

        DenseHashMap<AstArray<char>*, int, AstArrayPredicate, AstArrayPredicate> names(nullptr);
        DenseHashMap<int, int> indices(-1);

        for (const AstExprTable::Item& item : node->items)
        {
            if (!item.key)
                continue;

            if (AstExprConstantString* expr = item.key->as<AstExprConstantString>())
            {
                int& line = names[&expr->value];

                if (line)
                    emitWarning(*context, LintWarning::Code_TableLiteral, expr->location,
                        "Table field '%.*s' is a duplicate; previously defined at line %d", int(expr->value.size), expr->value.data, line);
                else
                    line = expr->location.begin.line + 1;
            }
            else if (AstExprConstantNumber* expr = item.key->as<AstExprConstantNumber>())
            {
                if (expr->value >= 1 && expr->value <= double(count) && double(int(expr->value)) == expr->value)
                    emitWarning(*context, LintWarning::Code_TableLiteral, expr->location,
                        "Table index %d is a duplicate; previously defined as a list entry", int(expr->value));
                else if (expr->value >= 0 && expr->value <= double(INT_MAX) && double(int(expr->value)) == expr->value)
                {
                    int& line = indices[int(expr->value)];

                    if (line)
                        emitWarning(*context, LintWarning::Code_TableLiteral, expr->location,
                            "Table index %d is a duplicate; previously defined at line %d", int(expr->value), line);
                    else
                        line = expr->location.begin.line + 1;
                }
            }
        }

        return true;
    }

    bool visit(AstType* node) override
    {
        return true;
    }

    bool visit(AstTypeTable* node) override
    {
        DenseHashMap<AstName, int> names(AstName{});

        for (const AstTableProp& item : node->props)
        {
            int& line = names[item.name];

            if (line)
                emitWarning(*context, LintWarning::Code_TableLiteral, item.location,
                    "Table type field '%s' is a duplicate; previously defined at line %d", item.name.value, line);
            else
                line = item.location.begin.line + 1;
        }

        return true;
    }

    struct AstArrayPredicate
    {
        size_t operator()(const AstArray<char>* value) const
        {
            return hashRange(value->data, value->size);
        }

        bool operator()(const AstArray<char>* lhs, const AstArray<char>* rhs) const
        {
            return (lhs && rhs) ? lhs->size == rhs->size && memcmp(lhs->data, rhs->data, lhs->size) == 0 : lhs == rhs;
        }
    };
};

class LintUninitializedLocal : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintUninitializedLocal pass;
        pass.context = &context;

        context.root->visit(&pass);

        pass.report();
    }

private:
    struct Local
    {
        bool defined;
        bool initialized;
        bool assigned;
        AstExprLocal* firstUse;
    };

    LintContext* context;
    DenseHashMap<AstLocal*, Local> locals;

    LintUninitializedLocal()
        : locals(NULL)
    {
    }

    void report()
    {
        for (auto& lp : locals)
        {
            AstLocal* local = lp.first;
            const Local& l = lp.second;

            if (l.defined && !l.initialized && !l.assigned && l.firstUse)
            {
                emitWarning(*context, LintWarning::Code_UninitializedLocal, l.firstUse->location,
                    "Variable '%s' defined at line %d is never initialized or assigned; initialize with 'nil' to silence", local->name.value,
                    local->location.begin.line + 1);
            }
        }
    }

    bool visit(AstStatLocal* node) override
    {
        AstExpr* last = node->values.size ? node->values.data[node->values.size - 1] : nullptr;
        bool vararg = last && (last->is<AstExprVarargs>() || last->is<AstExprCall>());

        for (size_t i = 0; i < node->vars.size; ++i)
        {
            Local& l = locals[node->vars.data[i]];

            l.defined = true;
            l.initialized = vararg || i < node->values.size;
        }

        return true;
    }

    bool visit(AstStatAssign* node) override
    {
        for (size_t i = 0; i < node->vars.size; ++i)
            visitAssign(node->vars.data[i]);

        for (size_t i = 0; i < node->values.size; ++i)
            node->values.data[i]->visit(this);

        return false;
    }

    bool visit(AstStatFunction* node) override
    {
        visitAssign(node->name);
        node->func->visit(this);

        return false;
    }

    bool visit(AstExprLocal* node) override
    {
        Local& l = locals[node->local];

        if (!l.firstUse)
            l.firstUse = node;

        return false;
    }

    void visitAssign(AstExpr* var)
    {
        if (AstExprLocal* lv = var->as<AstExprLocal>())
        {
            Local& l = locals[lv->local];

            l.assigned = true;
        }
        else
        {
            var->visit(this);
        }
    }
};

class LintDuplicateFunction : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintDuplicateFunction pass{&context};
        context.root->visit(&pass);
    }

private:
    LintContext* context;
    DenseHashMap<std::string, Location> defns;

    LintDuplicateFunction(LintContext* context)
        : context(context)
        , defns("")
    {
    }

    bool visit(AstStatBlock* block) override
    {
        defns.clear();

        for (AstStat* stat : block->body)
        {
            if (AstStatFunction* func = stat->as<AstStatFunction>())
                trackFunction(func->name->location, buildName(func->name));
            else if (AstStatLocalFunction* func = stat->as<AstStatLocalFunction>())
                trackFunction(func->name->location, func->name->name.value);
        }

        return true;
    }

    void trackFunction(Location location, const std::string& name)
    {
        if (name.empty())
            return;

        Location& defn = defns[name];

        if (defn.end.line == 0 && defn.end.column == 0)
            defn = location;
        else
            report(name, location, defn);
    }

    std::string buildName(AstExpr* expr)
    {
        if (AstExprLocal* local = expr->as<AstExprLocal>())
            return local->local->name.value;
        else if (AstExprGlobal* global = expr->as<AstExprGlobal>())
            return global->name.value;
        else if (AstExprIndexName* indexName = expr->as<AstExprIndexName>())
        {
            std::string lhs = buildName(indexName->expr);
            if (lhs.empty())
                return lhs;

            lhs += '.';
            lhs += indexName->index.value;
            return lhs;
        }
        else
            return std::string();
    }

    void report(const std::string& name, Location location, Location otherLocation)
    {
        emitWarning(*context, LintWarning::Code_DuplicateFunction, location, "Duplicate function definition: '%s' also defined on line %d",
            name.c_str(), otherLocation.begin.line + 1);
    }
};

class LintDeprecatedApi : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        if (!context.module)
            return;

        LintDeprecatedApi pass{&context};
        context.root->visit(&pass);
    }

private:
    LintContext* context;

    LintDeprecatedApi(LintContext* context)
        : context(context)
    {
    }

    bool visit(AstExprIndexName* node) override
    {
        std::optional<TypeId> ty = context->getType(node->expr);
        if (!ty)
            return true;

        if (const ClassTypeVar* cty = get<ClassTypeVar>(follow(*ty)))
        {
            const Property* prop = lookupClassProp(cty, node->index.value);

            if (prop && prop->deprecated)
                report(node->location, *prop, cty->name.c_str(), node->index.value);
        }
        else if (const TableTypeVar* tty = get<TableTypeVar>(follow(*ty)))
        {
            auto prop = tty->props.find(node->index.value);

            if (prop != tty->props.end() && prop->second.deprecated)
                report(node->location, prop->second, tty->name ? tty->name->c_str() : nullptr, node->index.value);
        }

        return true;
    }

    void report(const Location& location, const Property& prop, const char* container, const char* field)
    {
        std::string suggestion = prop.deprecatedSuggestion.empty() ? "" : format(", use '%s' instead", prop.deprecatedSuggestion.c_str());

        if (container)
            emitWarning(*context, LintWarning::Code_DeprecatedApi, location, "Member '%s.%s' is deprecated%s", container, field, suggestion.c_str());
        else
            emitWarning(*context, LintWarning::Code_DeprecatedApi, location, "Member '%s' is deprecated%s", field, suggestion.c_str());
    }
};

class LintTableOperations : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        if (!context.module)
            return;

        LintTableOperations pass{&context};
        context.root->visit(&pass);
    }

private:
    LintContext* context;

    LintTableOperations(LintContext* context)
        : context(context)
    {
    }

    bool visit(AstExprCall* node) override
    {
        AstExprIndexName* func = node->func->as<AstExprIndexName>();
        if (!func)
            return true;

        AstExprGlobal* tablib = func->expr->as<AstExprGlobal>();
        if (!tablib || tablib->name != "table")
            return true;

        AstExpr** args = node->args.data;

        if (func->index == "insert" && node->args.size == 2)
        {
            if (AstExprCall* tail = args[1]->as<AstExprCall>())
            {
                if (std::optional<TypeId> funty = context->getType(tail->func))
                {
                    size_t ret = getReturnCount(follow(*funty));

                    if (ret > 1)
                        emitWarning(*context, LintWarning::Code_TableOperations, tail->location,
                            "table.insert may change behavior if the call returns more than one result; consider adding parentheses around second "
                            "argument");
                }
            }
        }

        if (func->index == "insert" && node->args.size >= 3)
        {
            // table.insert(t, 0, ?)
            if (isConstant(args[1], 0.0))
                emitWarning(*context, LintWarning::Code_TableOperations, args[1]->location,
                    "table.insert uses index 0 but arrays are 1-based; did you mean 1 instead?");

            // table.insert(t, #t, ?)
            if (isLength(args[1], args[0]))
                emitWarning(*context, LintWarning::Code_TableOperations, args[1]->location,
                    "table.insert will insert the value before the last element, which is likely a bug; consider removing the second argument or "
                    "wrap it in parentheses to silence");

            // table.insert(t, #t+1, ?)
            if (AstExprBinary* add = args[1]->as<AstExprBinary>();
                add && add->op == AstExprBinary::Add && isLength(add->left, args[0]) && isConstant(add->right, 1.0))
                emitWarning(*context, LintWarning::Code_TableOperations, args[1]->location,
                    "table.insert will append the value to the table; consider removing the second argument for efficiency");
        }

        if (func->index == "remove" && node->args.size >= 2)
        {
            // table.remove(t, 0)
            if (isConstant(args[1], 0.0))
                emitWarning(*context, LintWarning::Code_TableOperations, args[1]->location,
                    "table.remove uses index 0 but arrays are 1-based; did you mean 1 instead?");

            // note: it's tempting to check for table.remove(t, #t), which is equivalent to table.remove(t), but it's correct, occurs frequently,
            // and also reads better.

            // table.remove(t, #t-1)
            if (AstExprBinary* sub = args[1]->as<AstExprBinary>();
                sub && sub->op == AstExprBinary::Sub && isLength(sub->left, args[0]) && isConstant(sub->right, 1.0))
                emitWarning(*context, LintWarning::Code_TableOperations, args[1]->location,
                    "table.remove will remove the value before the last element, which is likely a bug; consider removing the second argument or "
                    "wrap it in parentheses to silence");
        }

        if (func->index == "move" && node->args.size >= 4)
        {
            // table.move(t, 0, _, _)
            if (isConstant(args[1], 0.0))
                emitWarning(*context, LintWarning::Code_TableOperations, args[1]->location,
                    "table.move uses index 0 but arrays are 1-based; did you mean 1 instead?");

            // table.move(t, _, _, 0)
            else if (isConstant(args[3], 0.0))
                emitWarning(*context, LintWarning::Code_TableOperations, args[3]->location,
                    "table.move uses index 0 but arrays are 1-based; did you mean 1 instead?");
        }

        if (func->index == "create" && node->args.size == 2)
        {
            // table.create(n, {...})
            if (args[1]->is<AstExprTable>())
                emitWarning(*context, LintWarning::Code_TableOperations, args[1]->location,
                    "table.create with a table literal will reuse the same object for all elements; consider using a for loop instead");

            // table.create(n, {...} :: ?)
            if (AstExprTypeAssertion* as = args[1]->as<AstExprTypeAssertion>(); as && as->expr->is<AstExprTable>())
                emitWarning(*context, LintWarning::Code_TableOperations, as->expr->location,
                    "table.create with a table literal will reuse the same object for all elements; consider using a for loop instead");
        }

        return true;
    }

    bool isConstant(AstExpr* expr, double value)
    {
        AstExprConstantNumber* n = expr->as<AstExprConstantNumber>();
        return n && n->value == value;
    }

    bool isLength(AstExpr* expr, AstExpr* table)
    {
        AstExprUnary* n = expr->as<AstExprUnary>();
        return n && n->op == AstExprUnary::Len && similar(n->expr, table);
    }

    size_t getReturnCount(TypeId ty)
    {
        if (auto ftv = get<FunctionTypeVar>(ty))
            return size(ftv->retTypes);

        if (auto itv = get<IntersectionTypeVar>(ty))
        {
            // We don't process the type recursively to avoid having to deal with self-recursive intersection types
            size_t result = 0;

            for (TypeId part : itv->parts)
                if (auto ftv = get<FunctionTypeVar>(follow(part)))
                    result = std::max(result, size(ftv->retTypes));

            return result;
        }

        return 0;
    }
};

class LintDuplicateCondition : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintDuplicateCondition pass{&context};
        context.root->visit(&pass);
    }

private:
    LintContext* context;

    LintDuplicateCondition(LintContext* context)
        : context(context)
    {
    }

    bool visit(AstStatIf* stat) override
    {
        if (!stat->elsebody)
            return true;

        if (!stat->elsebody->is<AstStatIf>())
            return true;

        // if..elseif chain detected, we need to unroll it
        std::vector<AstExpr*> conditions;
        conditions.reserve(2);

        AstStatIf* head = stat;
        while (head)
        {
            head->condition->visit(this);
            head->thenbody->visit(this);

            conditions.push_back(head->condition);

            if (head->elsebody && head->elsebody->is<AstStatIf>())
            {
                head = head->elsebody->as<AstStatIf>();
                continue;
            }

            if (head->elsebody)
                head->elsebody->visit(this);

            break;
        }

        detectDuplicates(conditions);

        // block recursive visits so that we only analyze each chain once
        return false;
    }

    bool visit(AstExprIfElse* expr) override
    {
        if (!expr->falseExpr->is<AstExprIfElse>())
            return true;

        // if..elseif chain detected, we need to unroll it
        std::vector<AstExpr*> conditions;
        conditions.reserve(2);

        AstExprIfElse* head = expr;
        while (head)
        {
            head->condition->visit(this);
            head->trueExpr->visit(this);

            conditions.push_back(head->condition);

            if (head->falseExpr->is<AstExprIfElse>())
            {
                head = head->falseExpr->as<AstExprIfElse>();
                continue;
            }

            head->falseExpr->visit(this);
            break;
        }

        detectDuplicates(conditions);

        // block recursive visits so that we only analyze each chain once
        return false;
    }

    bool visit(AstExprBinary* expr) override
    {
        if (expr->op != AstExprBinary::And && expr->op != AstExprBinary::Or)
            return true;

        // for And expressions, it's idiomatic to use "a and a or b" as a ternary replacement, so we detect this pattern
        if (expr->op == AstExprBinary::Or)
        {
            AstExprBinary* la = expr->left->as<AstExprBinary>();

            if (la && la->op == AstExprBinary::And)
            {
                AstExprBinary* lb = la->left->as<AstExprBinary>();
                AstExprBinary* rb = la->right->as<AstExprBinary>();

                // check that the length of and-chain is exactly 2
                if (!(lb && lb->op == AstExprBinary::And) && !(rb && rb->op == AstExprBinary::And))
                {
                    la->left->visit(this);
                    la->right->visit(this);
                    expr->right->visit(this);
                    return false;
                }
            }
        }

        // unroll condition chain
        std::vector<AstExpr*> conditions;
        conditions.reserve(2);

        extractOpChain(conditions, expr, expr->op);

        detectDuplicates(conditions);

        // block recursive visits so that we only analyze each chain once
        return false;
    }

    void extractOpChain(std::vector<AstExpr*>& conditions, AstExpr* expr, AstExprBinary::Op op)
    {
        if (AstExprBinary* bin = expr->as<AstExprBinary>(); bin && bin->op == op)
        {
            extractOpChain(conditions, bin->left, op);
            extractOpChain(conditions, bin->right, op);
        }
        else if (AstExprGroup* group = expr->as<AstExprGroup>())
        {
            extractOpChain(conditions, group->expr, op);
        }
        else
        {
            conditions.push_back(expr);
        }
    }

    void detectDuplicates(const std::vector<AstExpr*>& conditions)
    {
        // Limit the distance at which we consider duplicates to reduce N^2 complexity to KN
        const size_t kMaxDistance = 5;

        for (size_t i = 0; i < conditions.size(); ++i)
        {
            for (size_t j = std::max(i, kMaxDistance) - kMaxDistance; j < i; ++j)
            {
                if (similar(conditions[j], conditions[i]))
                {
                    if (conditions[i]->location.begin.line == conditions[j]->location.begin.line)
                        emitWarning(*context, LintWarning::Code_DuplicateCondition, conditions[i]->location,
                            "Condition has already been checked on column %d", conditions[j]->location.begin.column + 1);
                    else
                        emitWarning(*context, LintWarning::Code_DuplicateCondition, conditions[i]->location,
                            "Condition has already been checked on line %d", conditions[j]->location.begin.line + 1);
                    break;
                }
            }
        }
    }
};

class LintDuplicateLocal : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintDuplicateLocal pass;
        pass.context = &context;

        context.root->visit(&pass);
    }

private:
    LintContext* context;

    DenseHashMap<AstLocal*, AstNode*> locals;

    LintDuplicateLocal()
        : locals(nullptr)
    {
    }

    bool visit(AstStatLocal* node) override
    {
        // early out for performance
        if (node->vars.size == 1)
            return true;

        for (size_t i = 0; i < node->vars.size; ++i)
            locals[node->vars.data[i]] = node;

        for (size_t i = 0; i < node->vars.size; ++i)
        {
            AstLocal* local = node->vars.data[i];

            if (local->shadow && locals[local->shadow] == node && !ignoreDuplicate(local))
            {
                if (local->shadow->location.begin.line == local->location.begin.line)
                    emitWarning(*context, LintWarning::Code_DuplicateLocal, local->location, "Variable '%s' already defined on column %d",
                        local->name.value, local->shadow->location.begin.column + 1);
                else
                    emitWarning(*context, LintWarning::Code_DuplicateLocal, local->location, "Variable '%s' already defined on line %d",
                        local->name.value, local->shadow->location.begin.line + 1);
            }
        }

        return true;
    }

    bool visit(AstExprFunction* node) override
    {
        if (node->self)
            locals[node->self] = node;

        for (size_t i = 0; i < node->args.size; ++i)
            locals[node->args.data[i]] = node;

        for (size_t i = 0; i < node->args.size; ++i)
        {
            AstLocal* local = node->args.data[i];

            if (local->shadow && locals[local->shadow] == node && !ignoreDuplicate(local))
            {
                if (local->shadow == node->self)
                    emitWarning(*context, LintWarning::Code_DuplicateLocal, local->location, "Function parameter 'self' already defined implicitly");
                else if (local->shadow->location.begin.line == local->location.begin.line)
                    emitWarning(*context, LintWarning::Code_DuplicateLocal, local->location, "Function parameter '%s' already defined on column %d",
                        local->name.value, local->shadow->location.begin.column + 1);
                else
                    emitWarning(*context, LintWarning::Code_DuplicateLocal, local->location, "Function parameter '%s' already defined on line %d",
                        local->name.value, local->shadow->location.begin.line + 1);
            }
        }

        return true;
    }

    bool ignoreDuplicate(AstLocal* local)
    {
        return local->name == "_";
    }
};

class LintMisleadingAndOr : AstVisitor
{
public:
    LUAU_NOINLINE static void process(LintContext& context)
    {
        LintMisleadingAndOr pass;
        pass.context = &context;

        context.root->visit(&pass);
    }

private:
    LintContext* context;

    bool visit(AstExprBinary* node) override
    {
        if (node->op != AstExprBinary::Or)
            return true;

        AstExprBinary* and_ = node->left->as<AstExprBinary>();
        if (!and_ || and_->op != AstExprBinary::And)
            return true;

        const char* alt = nullptr;

        if (and_->right->is<AstExprConstantNil>())
            alt = "nil";
        else if (AstExprConstantBool* c = and_->right->as<AstExprConstantBool>(); c && c->value == false)
            alt = "false";

        if (alt)
            emitWarning(*context, LintWarning::Code_MisleadingAndOr, node->location,
                "The and-or expression always evaluates to the second alternative because the first alternative is %s; consider using if-then-else "
                "expression instead",
                alt);

        return true;
    }
};

static void fillBuiltinGlobals(LintContext& context, const AstNameTable& names, const ScopePtr& env)
{
    ScopePtr current = env;
    while (true)
    {
        for (auto& [global, binding] : current->bindings)
        {
            AstName name = names.get(global.c_str());

            if (name.value)
            {
                auto& g = context.builtinGlobals[name];
                g.type = binding.typeId;
                if (binding.deprecated)
                    g.deprecated = binding.deprecatedSuggestion.c_str();
            }
        }

        if (current->parent)
            current = current->parent;
        else
            break;
    }
}

static const char* fuzzyMatch(std::string_view str, const char** array, size_t size)
{
    if (FInt::LuauSuggestionDistance == 0)
        return nullptr;

    size_t bestDistance = FInt::LuauSuggestionDistance;
    size_t bestMatch = size;

    for (size_t i = 0; i < size; ++i)
    {
        size_t ed = editDistance(str, array[i]);

        if (ed <= bestDistance)
        {
            bestDistance = ed;
            bestMatch = i;
        }
    }

    return bestMatch < size ? array[bestMatch] : nullptr;
}

static void lintComments(LintContext& context, const std::vector<HotComment>& hotcomments)
{
    bool seenMode = false;

    for (const HotComment& hc : hotcomments)
    {
        // We reserve --!<space> for various informational (non-directive) comments
        if (hc.content.empty() || hc.content[0] == ' ' || hc.content[0] == '\t')
            continue;

        if (!hc.header)
        {
            emitWarning(context, LintWarning::Code_CommentDirective, hc.location,
                "Comment directive is ignored because it is placed after the first non-comment token");
        }
        else
        {
            size_t space = hc.content.find_first_of(" \t");
            std::string_view first = std::string_view(hc.content).substr(0, space);

            if (first == "nolint")
            {
                size_t notspace = hc.content.find_first_not_of(" \t", space);

                if (space == std::string::npos || notspace == std::string::npos)
                {
                    // disables all lints
                }
                else if (LintWarning::parseName(hc.content.c_str() + notspace) == LintWarning::Code_Unknown)
                {
                    const char* rule = hc.content.c_str() + notspace;

                    // skip Unknown
                    if (const char* suggestion = fuzzyMatch(rule, kWarningNames + 1, LintWarning::Code__Count - 1))
                        emitWarning(context, LintWarning::Code_CommentDirective, hc.location,
                            "nolint directive refers to unknown lint rule '%s'; did you mean '%s'?", rule, suggestion);
                    else
                        emitWarning(
                            context, LintWarning::Code_CommentDirective, hc.location, "nolint directive refers to unknown lint rule '%s'", rule);
                }
            }
            else if (first == "nocheck" || first == "nonstrict" || first == "strict")
            {
                if (space != std::string::npos)
                    emitWarning(context, LintWarning::Code_CommentDirective, hc.location,
                        "Comment directive with the type checking mode has extra symbols at the end of the line");
                else if (seenMode)
                    emitWarning(context, LintWarning::Code_CommentDirective, hc.location,
                        "Comment directive with the type checking mode has already been used");
                else
                    seenMode = true;
            }
            else
            {
                static const char* kHotComments[] = {
                    "nolint",
                    "nocheck",
                    "nonstrict",
                    "strict",
                };

                if (const char* suggestion = fuzzyMatch(first, kHotComments, std::size(kHotComments)))
                    emitWarning(context, LintWarning::Code_CommentDirective, hc.location, "Unknown comment directive '%.*s'; did you mean '%s'?",
                        int(first.size()), first.data(), suggestion);
                else
                    emitWarning(context, LintWarning::Code_CommentDirective, hc.location, "Unknown comment directive '%.*s'", int(first.size()),
                        first.data());
            }
        }
    }
}

void LintOptions::setDefaults()
{
    // By default, we enable all warnings
    warningMask = ~0ull;
}

std::vector<LintWarning> lint(AstStat* root, const AstNameTable& names, const ScopePtr& env, const Module* module,
    const std::vector<HotComment>& hotcomments, const LintOptions& options)
{
    LintContext context;

    context.options = options;
    context.root = root;
    context.placeholder = names.get("_");
    context.scope = env;
    context.module = module;

    fillBuiltinGlobals(context, names, env);

    if (context.warningEnabled(LintWarning::Code_UnknownGlobal) || context.warningEnabled(LintWarning::Code_DeprecatedGlobal) ||
        context.warningEnabled(LintWarning::Code_GlobalUsedAsLocal) || context.warningEnabled(LintWarning::Code_PlaceholderRead) ||
        context.warningEnabled(LintWarning::Code_BuiltinGlobalWrite))
    {
        LintGlobalLocal::process(context);
    }

    if (context.warningEnabled(LintWarning::Code_MultiLineStatement))
        LintMultiLineStatement::process(context);

    if (context.warningEnabled(LintWarning::Code_SameLineStatement))
        LintSameLineStatement::process(context);

    if (context.warningEnabled(LintWarning::Code_LocalShadow) || context.warningEnabled(LintWarning::Code_FunctionUnused) ||
        context.warningEnabled(LintWarning::Code_ImportUnused) || context.warningEnabled(LintWarning::Code_LocalUnused))
    {
        LintLocalHygiene::process(context);
    }

    if (context.warningEnabled(LintWarning::Code_FunctionUnused))
        LintUnusedFunction::process(context);

    if (context.warningEnabled(LintWarning::Code_UnreachableCode))
        LintUnreachableCode::process(context);

    if (context.warningEnabled(LintWarning::Code_UnknownType))
        LintUnknownType::process(context);

    if (context.warningEnabled(LintWarning::Code_ForRange))
        LintForRange::process(context);

    if (context.warningEnabled(LintWarning::Code_UnbalancedAssignment))
        LintUnbalancedAssignment::process(context);

    if (context.warningEnabled(LintWarning::Code_ImplicitReturn))
        LintImplicitReturn::process(context);

    if (context.warningEnabled(LintWarning::Code_FormatString))
        LintFormatString::process(context);

    if (context.warningEnabled(LintWarning::Code_TableLiteral))
        LintTableLiteral::process(context);

    if (context.warningEnabled(LintWarning::Code_UninitializedLocal))
        LintUninitializedLocal::process(context);

    if (context.warningEnabled(LintWarning::Code_DuplicateFunction))
        LintDuplicateFunction::process(context);

    if (context.warningEnabled(LintWarning::Code_DeprecatedApi))
        LintDeprecatedApi::process(context);

    if (context.warningEnabled(LintWarning::Code_TableOperations))
        LintTableOperations::process(context);

    if (context.warningEnabled(LintWarning::Code_DuplicateCondition))
        LintDuplicateCondition::process(context);

    if (context.warningEnabled(LintWarning::Code_DuplicateLocal))
        LintDuplicateLocal::process(context);

    if (context.warningEnabled(LintWarning::Code_MisleadingAndOr))
        LintMisleadingAndOr::process(context);

    if (context.warningEnabled(LintWarning::Code_CommentDirective))
        lintComments(context, hotcomments);

    std::sort(context.result.begin(), context.result.end(), WarningComparator());

    return context.result;
}

const char* LintWarning::getName(Code code)
{
    LUAU_ASSERT(unsigned(code) < Code__Count);

    return kWarningNames[code];
}

LintWarning::Code LintWarning::parseName(const char* name)
{
    for (int code = Code_Unknown; code < Code__Count; ++code)
        if (strcmp(name, getName(Code(code))) == 0)
            return Code(code);

    return Code_Unknown;
}

uint64_t LintWarning::parseMask(const std::vector<HotComment>& hotcomments)
{
    uint64_t result = 0;

    for (const HotComment& hc : hotcomments)
    {
        if (!hc.header)
            continue;

        if (hc.content.compare(0, 6, "nolint") != 0)
            continue;

        size_t name = hc.content.find_first_not_of(" \t", 6);

        // --!nolint disables everything
        if (name == std::string::npos)
            return ~0ull;

        // --!nolint needs to be followed by a whitespace character
        if (name == 6)
            continue;

        // --!nolint name disables the specific lint
        LintWarning::Code code = LintWarning::parseName(hc.content.c_str() + name);

        if (code != LintWarning::Code_Unknown)
            result |= 1ull << int(code);
    }

    return result;
}

std::vector<AstName> getDeprecatedGlobals(const AstNameTable& names)
{
    LintContext context;

    std::vector<AstName> result;
    result.reserve(context.builtinGlobals.size());

    for (auto& p : context.builtinGlobals)
        if (p.second.deprecated)
            result.push_back(p.first);

    return result;
}

void fuzzFormatString(const char* data, size_t size)
{
    LintFormatString::fuzz(data, size);
}

} // namespace Luau
