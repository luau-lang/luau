// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstUtils.h"
#include "Luau/Ast.h"
#include "Luau/Type.h"

namespace Luau
{

std::optional<TypeGuard> matchTypeGuard(AstExprBinary::Op op, AstExpr* left, AstExpr* right)
{
    if (op != AstExprBinary::CompareEq && op != AstExprBinary::CompareNe)
        return std::nullopt;

    if (right->is<AstExprCall>())
        std::swap(left, right);

    if (!right->is<AstExprConstantString>())
        return std::nullopt;

    AstExprCall* call = left->as<AstExprCall>();
    AstExprConstantString* string = right->as<AstExprConstantString>();
    if (!call || !string)
        return std::nullopt;

    AstExprGlobal* callee = call->func->as<AstExprGlobal>();
    if (!callee)
        return std::nullopt;

    if (callee->name != "type" && callee->name != "typeof")
        return std::nullopt;

    if (call->args.size != 1)
        return std::nullopt;

    return TypeGuard{
        /*isTypeof*/ callee->name == "typeof",
        /*target*/ call->args.data[0],
        /*type*/ std::string(string->value.data, string->value.size),
    };
}

struct AstExprTableFinder : AstVisitor
{
    NotNull<DenseHashSet<TypeId>> result;
    NotNull<const DenseHashMap<const AstExpr*, TypeId>> astTypes;

    explicit AstExprTableFinder(NotNull<DenseHashSet<TypeId>> result, NotNull<const DenseHashMap<const AstExpr*, TypeId>> astTypes)
        : result(result)
        , astTypes(astTypes)
    {
    }

    bool visit(AstExpr* expr) override
    {
        return false;
    }

    bool visit(AstExprTable* tbl) override
    {
        const TypeId* ty = astTypes->find(tbl);
        LUAU_ASSERT(ty);
        if (ty)
            result->insert(*ty);

        return true;
    }
};

void findUniqueTypes(NotNull<DenseHashSet<TypeId>> uniqueTypes, AstExpr* expr, NotNull<const DenseHashMap<const AstExpr*, TypeId>> astTypes)
{
    AstExprTableFinder finder{uniqueTypes, astTypes};
    expr->visit(&finder);
}

template<typename Iter>
void findUniqueTypes(
    NotNull<DenseHashSet<TypeId>> uniqueTypes,
    Iter startIt,
    Iter endIt,
    NotNull<const DenseHashMap<const AstExpr*, TypeId>> astTypes
)
{
    while (startIt != endIt)
    {
        AstExpr* expr = *startIt;
        if (expr->is<AstExprTable>())
            findUniqueTypes(uniqueTypes, expr, astTypes);
        ++startIt;
    }
}


void findUniqueTypes(
    NotNull<DenseHashSet<TypeId>> uniqueTypes,
    AstArray<AstExpr*> exprs,
    NotNull<const DenseHashMap<const AstExpr*, TypeId>> astTypes
)
{
    findUniqueTypes(uniqueTypes, exprs.begin(), exprs.end(), astTypes);
}

void findUniqueTypes(
    NotNull<DenseHashSet<TypeId>> uniqueTypes,
    const std::vector<AstExpr*>& exprs,
    NotNull<const DenseHashMap<const AstExpr*, TypeId>> astTypes
)
{
    findUniqueTypes(uniqueTypes, exprs.begin(), exprs.end(), astTypes);
}

} // namespace Luau
