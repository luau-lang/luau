// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Ast.h"
#include "Luau/Type.h"

namespace Luau
{

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
