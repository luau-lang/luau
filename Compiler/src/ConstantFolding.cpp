// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "ConstantFolding.h"

#include <math.h>

namespace Luau
{
namespace Compile
{

static bool constantsEqual(const Constant& la, const Constant& ra)
{
    LUAU_ASSERT(la.type != Constant::Type_Unknown && ra.type != Constant::Type_Unknown);

    switch (la.type)
    {
    case Constant::Type_Nil:
        return ra.type == Constant::Type_Nil;

    case Constant::Type_Boolean:
        return ra.type == Constant::Type_Boolean && la.valueBoolean == ra.valueBoolean;

    case Constant::Type_Number:
        return ra.type == Constant::Type_Number && la.valueNumber == ra.valueNumber;

    case Constant::Type_String:
        return ra.type == Constant::Type_String && la.stringLength == ra.stringLength && memcmp(la.valueString, ra.valueString, la.stringLength) == 0;

    default:
        LUAU_ASSERT(!"Unexpected constant type in comparison");
        return false;
    }
}

static void foldUnary(Constant& result, AstExprUnary::Op op, const Constant& arg)
{
    switch (op)
    {
    case AstExprUnary::Not:
        if (arg.type != Constant::Type_Unknown)
        {
            result.type = Constant::Type_Boolean;
            result.valueBoolean = !arg.isTruthful();
        }
        break;

    case AstExprUnary::Minus:
        if (arg.type == Constant::Type_Number)
        {
            result.type = Constant::Type_Number;
            result.valueNumber = -arg.valueNumber;
        }
        break;

    case AstExprUnary::Len:
        if (arg.type == Constant::Type_String)
        {
            result.type = Constant::Type_Number;
            result.valueNumber = double(arg.stringLength);
        }
        break;

    default:
        LUAU_ASSERT(!"Unexpected unary operation");
    }
}

static void foldBinary(Constant& result, AstExprBinary::Op op, const Constant& la, const Constant& ra)
{
    switch (op)
    {
    case AstExprBinary::Add:
        if (la.type == Constant::Type_Number && ra.type == Constant::Type_Number)
        {
            result.type = Constant::Type_Number;
            result.valueNumber = la.valueNumber + ra.valueNumber;
        }
        break;

    case AstExprBinary::Sub:
        if (la.type == Constant::Type_Number && ra.type == Constant::Type_Number)
        {
            result.type = Constant::Type_Number;
            result.valueNumber = la.valueNumber - ra.valueNumber;
        }
        break;

    case AstExprBinary::Mul:
        if (la.type == Constant::Type_Number && ra.type == Constant::Type_Number)
        {
            result.type = Constant::Type_Number;
            result.valueNumber = la.valueNumber * ra.valueNumber;
        }
        break;

    case AstExprBinary::Div:
        if (la.type == Constant::Type_Number && ra.type == Constant::Type_Number)
        {
            result.type = Constant::Type_Number;
            result.valueNumber = la.valueNumber / ra.valueNumber;
        }
        break;

    case AstExprBinary::Mod:
        if (la.type == Constant::Type_Number && ra.type == Constant::Type_Number)
        {
            result.type = Constant::Type_Number;
            result.valueNumber = la.valueNumber - floor(la.valueNumber / ra.valueNumber) * ra.valueNumber;
        }
        break;

    case AstExprBinary::Pow:
        if (la.type == Constant::Type_Number && ra.type == Constant::Type_Number)
        {
            result.type = Constant::Type_Number;
            result.valueNumber = pow(la.valueNumber, ra.valueNumber);
        }
        break;

    case AstExprBinary::Concat:
        break;

    case AstExprBinary::CompareNe:
        if (la.type != Constant::Type_Unknown && ra.type != Constant::Type_Unknown)
        {
            result.type = Constant::Type_Boolean;
            result.valueBoolean = !constantsEqual(la, ra);
        }
        break;

    case AstExprBinary::CompareEq:
        if (la.type != Constant::Type_Unknown && ra.type != Constant::Type_Unknown)
        {
            result.type = Constant::Type_Boolean;
            result.valueBoolean = constantsEqual(la, ra);
        }
        break;

    case AstExprBinary::CompareLt:
        if (la.type == Constant::Type_Number && ra.type == Constant::Type_Number)
        {
            result.type = Constant::Type_Boolean;
            result.valueBoolean = la.valueNumber < ra.valueNumber;
        }
        break;

    case AstExprBinary::CompareLe:
        if (la.type == Constant::Type_Number && ra.type == Constant::Type_Number)
        {
            result.type = Constant::Type_Boolean;
            result.valueBoolean = la.valueNumber <= ra.valueNumber;
        }
        break;

    case AstExprBinary::CompareGt:
        if (la.type == Constant::Type_Number && ra.type == Constant::Type_Number)
        {
            result.type = Constant::Type_Boolean;
            result.valueBoolean = la.valueNumber > ra.valueNumber;
        }
        break;

    case AstExprBinary::CompareGe:
        if (la.type == Constant::Type_Number && ra.type == Constant::Type_Number)
        {
            result.type = Constant::Type_Boolean;
            result.valueBoolean = la.valueNumber >= ra.valueNumber;
        }
        break;

    case AstExprBinary::And:
        if (la.type != Constant::Type_Unknown)
        {
            result = la.isTruthful() ? ra : la;
        }
        break;

    case AstExprBinary::Or:
        if (la.type != Constant::Type_Unknown)
        {
            result = la.isTruthful() ? la : ra;
        }
        break;

    default:
        LUAU_ASSERT(!"Unexpected binary operation");
    }
}

struct ConstantVisitor : AstVisitor
{
    DenseHashMap<AstExpr*, Constant>& constants;
    DenseHashMap<AstLocal*, Variable>& variables;
    DenseHashMap<AstLocal*, Constant>& locals;

    ConstantVisitor(
        DenseHashMap<AstExpr*, Constant>& constants, DenseHashMap<AstLocal*, Variable>& variables, DenseHashMap<AstLocal*, Constant>& locals)
        : constants(constants)
        , variables(variables)
        , locals(locals)
    {
    }

    Constant analyze(AstExpr* node)
    {
        Constant result;
        result.type = Constant::Type_Unknown;

        if (AstExprGroup* expr = node->as<AstExprGroup>())
        {
            result = analyze(expr->expr);
        }
        else if (node->is<AstExprConstantNil>())
        {
            result.type = Constant::Type_Nil;
        }
        else if (AstExprConstantBool* expr = node->as<AstExprConstantBool>())
        {
            result.type = Constant::Type_Boolean;
            result.valueBoolean = expr->value;
        }
        else if (AstExprConstantNumber* expr = node->as<AstExprConstantNumber>())
        {
            result.type = Constant::Type_Number;
            result.valueNumber = expr->value;
        }
        else if (AstExprConstantString* expr = node->as<AstExprConstantString>())
        {
            result.type = Constant::Type_String;
            result.valueString = expr->value.data;
            result.stringLength = unsigned(expr->value.size);
        }
        else if (AstExprLocal* expr = node->as<AstExprLocal>())
        {
            const Constant* l = locals.find(expr->local);

            if (l)
                result = *l;
        }
        else if (node->is<AstExprGlobal>())
        {
            // nope
        }
        else if (node->is<AstExprVarargs>())
        {
            // nope
        }
        else if (AstExprCall* expr = node->as<AstExprCall>())
        {
            analyze(expr->func);

            for (size_t i = 0; i < expr->args.size; ++i)
                analyze(expr->args.data[i]);
        }
        else if (AstExprIndexName* expr = node->as<AstExprIndexName>())
        {
            analyze(expr->expr);
        }
        else if (AstExprIndexExpr* expr = node->as<AstExprIndexExpr>())
        {
            analyze(expr->expr);
            analyze(expr->index);
        }
        else if (AstExprFunction* expr = node->as<AstExprFunction>())
        {
            // this is necessary to propagate constant information in all child functions
            expr->body->visit(this);
        }
        else if (AstExprTable* expr = node->as<AstExprTable>())
        {
            for (size_t i = 0; i < expr->items.size; ++i)
            {
                const AstExprTable::Item& item = expr->items.data[i];

                if (item.key)
                    analyze(item.key);

                analyze(item.value);
            }
        }
        else if (AstExprUnary* expr = node->as<AstExprUnary>())
        {
            Constant arg = analyze(expr->expr);

            if (arg.type != Constant::Type_Unknown)
                foldUnary(result, expr->op, arg);
        }
        else if (AstExprBinary* expr = node->as<AstExprBinary>())
        {
            Constant la = analyze(expr->left);
            Constant ra = analyze(expr->right);

            // note: ra doesn't need to be constant to fold and/or
            if (la.type != Constant::Type_Unknown)
                foldBinary(result, expr->op, la, ra);
        }
        else if (AstExprTypeAssertion* expr = node->as<AstExprTypeAssertion>())
        {
            Constant arg = analyze(expr->expr);

            result = arg;
        }
        else if (AstExprIfElse* expr = node->as<AstExprIfElse>())
        {
            Constant cond = analyze(expr->condition);
            Constant trueExpr = analyze(expr->trueExpr);
            Constant falseExpr = analyze(expr->falseExpr);

            if (cond.type != Constant::Type_Unknown)
                result = cond.isTruthful() ? trueExpr : falseExpr;
        }
        else
        {
            LUAU_ASSERT(!"Unknown expression type");
        }

        if (result.type != Constant::Type_Unknown)
            constants[node] = result;

        return result;
    }

    bool visit(AstExpr* node) override
    {
        // note: we short-circuit the visitor traversal through any expression trees by returning false
        // recursive traversal is happening inside analyze() which makes it easier to get the resulting value of the subexpression
        analyze(node);

        return false;
    }

    bool visit(AstStatLocal* node) override
    {
        // all values that align wrt indexing are simple - we just match them 1-1
        for (size_t i = 0; i < node->vars.size && i < node->values.size; ++i)
        {
            Constant arg = analyze(node->values.data[i]);

            if (arg.type != Constant::Type_Unknown)
            {
                // note: we rely on trackValues to have been run before us
                Variable* v = variables.find(node->vars.data[i]);
                LUAU_ASSERT(v);

                if (!v->written)
                {
                    locals[node->vars.data[i]] = arg;
                    v->constant = true;
                }
            }
        }

        if (node->vars.size > node->values.size)
        {
            // if we have trailing variables, then depending on whether the last value is capable of returning multiple values
            // (aka call or varargs), we either don't know anything about these vars, or we know they're nil
            AstExpr* last = node->values.size ? node->values.data[node->values.size - 1] : nullptr;
            bool multRet = last && (last->is<AstExprCall>() || last->is<AstExprVarargs>());

            if (!multRet)
            {
                for (size_t i = node->values.size; i < node->vars.size; ++i)
                {
                    // note: we rely on trackValues to have been run before us
                    Variable* v = variables.find(node->vars.data[i]);
                    LUAU_ASSERT(v);

                    if (!v->written)
                    {
                        locals[node->vars.data[i]].type = Constant::Type_Nil;
                        v->constant = true;
                    }
                }
            }
        }
        else
        {
            // we can have more values than variables; in this case we still need to analyze them to make sure we do constant propagation inside
            // them
            for (size_t i = node->vars.size; i < node->values.size; ++i)
                analyze(node->values.data[i]);
        }

        return false;
    }
};

void foldConstants(DenseHashMap<AstExpr*, Constant>& constants, DenseHashMap<AstLocal*, Variable>& variables,
    DenseHashMap<AstLocal*, Constant>& locals, AstNode* root)
{
    ConstantVisitor visitor{constants, variables, locals};
    root->visit(&visitor);
}

} // namespace Compile
} // namespace Luau
