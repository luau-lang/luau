// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "CostModel.h"

#include "Luau/Common.h"
#include "Luau/DenseHash.h"

namespace Luau
{
namespace Compile
{

inline uint64_t parallelAddSat(uint64_t x, uint64_t y)
{
    uint64_t s = x + y;
    uint64_t m = s & 0x8080808080808080ull; // saturation mask

    return (s ^ m) | (m - (m >> 7));
}

struct Cost
{
    static const uint64_t kLiteral = ~0ull;

    // cost model: 8 bytes, where first byte is the baseline cost, and the next 7 bytes are discounts for when variable #i is constant
    uint64_t model;
    // constant mask: 8-byte 0xff mask; equal to all ff's for literals, for variables only byte #i (1+) is set to align with model
    uint64_t constant;

    Cost(int cost = 0, uint64_t constant = 0)
        : model(cost < 0x7f ? cost : 0x7f)
        , constant(constant)
    {
    }

    Cost operator+(const Cost& other) const
    {
        Cost result;
        result.model = parallelAddSat(model, other.model);
        return result;
    }

    Cost& operator+=(const Cost& other)
    {
        model = parallelAddSat(model, other.model);
        constant = 0;
        return *this;
    }

    static Cost fold(const Cost& x, const Cost& y)
    {
        uint64_t newmodel = parallelAddSat(x.model, y.model);
        uint64_t newconstant = x.constant & y.constant;

        // the extra cost for folding is 1; the discount is 1 for the variable that is shared by x&y (or whichever one is used in x/y if the other is
        // literal)
        uint64_t extra = (newconstant == kLiteral) ? 0 : (1 | (0x0101010101010101ull & newconstant));

        Cost result;
        result.model = parallelAddSat(newmodel, extra);
        result.constant = newconstant;

        return result;
    }
};

struct CostVisitor : AstVisitor
{
    DenseHashMap<AstLocal*, uint64_t> vars;
    Cost result;

    CostVisitor()
        : vars(nullptr)
    {
    }

    Cost model(AstExpr* node)
    {
        if (AstExprGroup* expr = node->as<AstExprGroup>())
        {
            return model(expr->expr);
        }
        else if (node->is<AstExprConstantNil>() || node->is<AstExprConstantBool>() || node->is<AstExprConstantNumber>() ||
                 node->is<AstExprConstantString>())
        {
            return Cost(0, Cost::kLiteral);
        }
        else if (AstExprLocal* expr = node->as<AstExprLocal>())
        {
            const uint64_t* i = vars.find(expr->local);

            return Cost(0, i ? *i : 0); // locals typically don't require extra instructions to compute
        }
        else if (node->is<AstExprGlobal>())
        {
            return 1;
        }
        else if (node->is<AstExprVarargs>())
        {
            return 3;
        }
        else if (AstExprCall* expr = node->as<AstExprCall>())
        {
            Cost cost = 3;
            cost += model(expr->func);

            for (size_t i = 0; i < expr->args.size; ++i)
            {
                Cost ac = model(expr->args.data[i]);
                // for constants/locals we still need to copy them to the argument list
                cost += ac.model == 0 ? Cost(1) : ac;
            }

            return cost;
        }
        else if (AstExprIndexName* expr = node->as<AstExprIndexName>())
        {
            return model(expr->expr) + 1;
        }
        else if (AstExprIndexExpr* expr = node->as<AstExprIndexExpr>())
        {
            return model(expr->expr) + model(expr->index) + 1;
        }
        else if (AstExprFunction* expr = node->as<AstExprFunction>())
        {
            return 10; // high baseline cost due to allocation
        }
        else if (AstExprTable* expr = node->as<AstExprTable>())
        {
            Cost cost = 10; // high baseline cost due to allocation

            for (size_t i = 0; i < expr->items.size; ++i)
            {
                const AstExprTable::Item& item = expr->items.data[i];

                if (item.key)
                    cost += model(item.key);

                cost += model(item.value);
                cost += 1;
            }

            return cost;
        }
        else if (AstExprUnary* expr = node->as<AstExprUnary>())
        {
            return Cost::fold(model(expr->expr), Cost(0, Cost::kLiteral));
        }
        else if (AstExprBinary* expr = node->as<AstExprBinary>())
        {
            return Cost::fold(model(expr->left), model(expr->right));
        }
        else if (AstExprTypeAssertion* expr = node->as<AstExprTypeAssertion>())
        {
            return model(expr->expr);
        }
        else if (AstExprIfElse* expr = node->as<AstExprIfElse>())
        {
            return model(expr->condition) + model(expr->trueExpr) + model(expr->falseExpr) + 2;
        }
        else
        {
            LUAU_ASSERT(!"Unknown expression type");
            return {};
        }
    }

    void assign(AstExpr* expr)
    {
        // variable assignments reset variable mask, so that further uses of this variable aren't discounted
        // this doesn't work perfectly with backwards control flow like loops, but is good enough for a single pass
        if (AstExprLocal* lv = expr->as<AstExprLocal>())
            if (uint64_t* i = vars.find(lv->local))
                *i = 0;
    }

    bool visit(AstExpr* node) override
    {
        // note: we short-circuit the visitor traversal through any expression trees by returning false
        // recursive traversal is happening inside model() which makes it easier to get the resulting value of the subexpression
        result += model(node);

        return false;
    }

    bool visit(AstStat* node) override
    {
        if (node->is<AstStatIf>())
            result += 2;
        else if (node->is<AstStatWhile>() || node->is<AstStatRepeat>() || node->is<AstStatFor>() || node->is<AstStatForIn>())
            result += 2;
        else if (node->is<AstStatBreak>() || node->is<AstStatContinue>())
            result += 1;

        return true;
    }

    bool visit(AstStatLocal* node) override
    {
        for (size_t i = 0; i < node->values.size; ++i)
        {
            Cost arg = model(node->values.data[i]);

            // propagate constant mask from expression through variables
            if (arg.constant && i < node->vars.size)
                vars[node->vars.data[i]] = arg.constant;

            result += arg;
        }

        return false;
    }

    bool visit(AstStatAssign* node) override
    {
        for (size_t i = 0; i < node->vars.size; ++i)
            assign(node->vars.data[i]);

        return true;
    }

    bool visit(AstStatCompoundAssign* node) override
    {
        assign(node->var);

        // if lhs is not a local, setting it requires an extra table operation
        result += node->var->is<AstExprLocal>() ? 1 : 2;

        return true;
    }
};

uint64_t modelCost(AstNode* root, AstLocal* const* vars, size_t varCount)
{
    CostVisitor visitor;
    for (size_t i = 0; i < varCount && i < 7; ++i)
        visitor.vars[vars[i]] = 0xffull << (i * 8 + 8);

    root->visit(&visitor);

    return visitor.result.model;
}

int computeCost(uint64_t model, const bool* varsConst, size_t varCount)
{
    int cost = int(model & 0x7f);

    // don't apply discounts to what is likely a saturated sum
    if (cost == 0x7f)
        return cost;

    for (size_t i = 0; i < varCount && i < 7; ++i)
        cost -= int((model >> (8 * i + 8)) & 0x7f) * varsConst[i];

    return cost;
}

} // namespace Compile
} // namespace Luau
