// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Ast.h"

#include "Luau/Common.h"

namespace Luau
{

static void visitTypeList(AstVisitor* visitor, const AstTypeList& list)
{
    for (AstType* ty : list.types)
        ty->visit(visitor);

    if (list.tailType)
        list.tailType->visit(visitor);
}

int gAstRttiIndex = 0;

AstExprGroup::AstExprGroup(const Location& location, AstExpr* expr)
    : AstExpr(ClassIndex(), location)
    , expr(expr)
{
}

void AstExprGroup::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
        expr->visit(visitor);
}

AstExprConstantNil::AstExprConstantNil(const Location& location)
    : AstExpr(ClassIndex(), location)
{
}

void AstExprConstantNil::visit(AstVisitor* visitor)
{
    visitor->visit(this);
}

AstExprConstantBool::AstExprConstantBool(const Location& location, bool value)
    : AstExpr(ClassIndex(), location)
    , value(value)
{
}

void AstExprConstantBool::visit(AstVisitor* visitor)
{
    visitor->visit(this);
}

AstExprConstantNumber::AstExprConstantNumber(const Location& location, double value)
    : AstExpr(ClassIndex(), location)
    , value(value)
{
}

void AstExprConstantNumber::visit(AstVisitor* visitor)
{
    visitor->visit(this);
}

AstExprConstantString::AstExprConstantString(const Location& location, const AstArray<char>& value)
    : AstExpr(ClassIndex(), location)
    , value(value)
{
}

void AstExprConstantString::visit(AstVisitor* visitor)
{
    visitor->visit(this);
}

AstExprLocal::AstExprLocal(const Location& location, AstLocal* local, bool upvalue)
    : AstExpr(ClassIndex(), location)
    , local(local)
    , upvalue(upvalue)
{
}

void AstExprLocal::visit(AstVisitor* visitor)
{
    visitor->visit(this);
}

AstExprGlobal::AstExprGlobal(const Location& location, const AstName& name)
    : AstExpr(ClassIndex(), location)
    , name(name)
{
}

void AstExprGlobal::visit(AstVisitor* visitor)
{
    visitor->visit(this);
}

AstExprVarargs::AstExprVarargs(const Location& location)
    : AstExpr(ClassIndex(), location)
{
}

void AstExprVarargs::visit(AstVisitor* visitor)
{
    visitor->visit(this);
}

AstExprCall::AstExprCall(const Location& location, AstExpr* func, const AstArray<AstExpr*>& args, bool self, const Location& argLocation)
    : AstExpr(ClassIndex(), location)
    , func(func)
    , args(args)
    , self(self)
    , argLocation(argLocation)
{
}

void AstExprCall::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        func->visit(visitor);

        for (AstExpr* arg : args)
            arg->visit(visitor);
    }
}

AstExprIndexName::AstExprIndexName(
    const Location& location, AstExpr* expr, const AstName& index, const Location& indexLocation, const Position& opPosition, char op)
    : AstExpr(ClassIndex(), location)
    , expr(expr)
    , index(index)
    , indexLocation(indexLocation)
    , opPosition(opPosition)
    , op(op)
{
}

void AstExprIndexName::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
        expr->visit(visitor);
}

AstExprIndexExpr::AstExprIndexExpr(const Location& location, AstExpr* expr, AstExpr* index)
    : AstExpr(ClassIndex(), location)
    , expr(expr)
    , index(index)
{
}

void AstExprIndexExpr::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        expr->visit(visitor);
        index->visit(visitor);
    }
}

AstExprFunction::AstExprFunction(const Location& location, const AstArray<AstName>& generics, const AstArray<AstName>& genericPacks, AstLocal* self,
    const AstArray<AstLocal*>& args, std::optional<Location> vararg, AstStatBlock* body, size_t functionDepth, const AstName& debugname,
    std::optional<AstTypeList> returnAnnotation, AstTypePack* varargAnnotation, bool hasEnd, std::optional<Location> argLocation)
    : AstExpr(ClassIndex(), location)
    , generics(generics)
    , genericPacks(genericPacks)
    , self(self)
    , args(args)
    , hasReturnAnnotation(returnAnnotation.has_value())
    , returnAnnotation()
    , vararg(vararg.has_value())
    , varargLocation(vararg.value_or(Location()))
    , varargAnnotation(varargAnnotation)
    , body(body)
    , functionDepth(functionDepth)
    , debugname(debugname)
    , hasEnd(hasEnd)
    , argLocation(argLocation)
{
    if (returnAnnotation.has_value())
        this->returnAnnotation = *returnAnnotation;
}

void AstExprFunction::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (AstLocal* arg : args)
        {
            if (arg->annotation)
                arg->annotation->visit(visitor);
        }

        if (varargAnnotation)
            varargAnnotation->visit(visitor);

        if (hasReturnAnnotation)
            visitTypeList(visitor, returnAnnotation);

        body->visit(visitor);
    }
}

AstExprTable::AstExprTable(const Location& location, const AstArray<Item>& items)
    : AstExpr(ClassIndex(), location)
    , items(items)
{
}

void AstExprTable::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (const Item& item : items)
        {
            if (item.key)
                item.key->visit(visitor);

            item.value->visit(visitor);
        }
    }
}

AstExprUnary::AstExprUnary(const Location& location, Op op, AstExpr* expr)
    : AstExpr(ClassIndex(), location)
    , op(op)
    , expr(expr)
{
}

void AstExprUnary::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
        expr->visit(visitor);
}

std::string toString(AstExprUnary::Op op)
{
    switch (op)
    {
    case AstExprUnary::Minus:
        return "-";
    case AstExprUnary::Not:
        return "not";
    case AstExprUnary::Len:
        return "#";
    default:
        LUAU_ASSERT(false);
        return ""; // MSVC requires this even though the switch/case is exhaustive
    }
}

AstExprBinary::AstExprBinary(const Location& location, Op op, AstExpr* left, AstExpr* right)
    : AstExpr(ClassIndex(), location)
    , op(op)
    , left(left)
    , right(right)
{
}

void AstExprBinary::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        left->visit(visitor);
        right->visit(visitor);
    }
}

std::string toString(AstExprBinary::Op op)
{
    switch (op)
    {
    case AstExprBinary::Add:
        return "+";
    case AstExprBinary::Sub:
        return "-";
    case AstExprBinary::Mul:
        return "*";
    case AstExprBinary::Div:
        return "/";
    case AstExprBinary::Mod:
        return "%";
    case AstExprBinary::Pow:
        return "^";
    case AstExprBinary::Concat:
        return "..";
    case AstExprBinary::CompareNe:
        return "~=";
    case AstExprBinary::CompareEq:
        return "==";
    case AstExprBinary::CompareLt:
        return "<";
    case AstExprBinary::CompareLe:
        return "<=";
    case AstExprBinary::CompareGt:
        return ">";
    case AstExprBinary::CompareGe:
        return ">=";
    case AstExprBinary::And:
        return "and";
    case AstExprBinary::Or:
        return "or";
    default:
        LUAU_ASSERT(false);
        return ""; // MSVC requires this even though the switch/case is exhaustive
    }
}

AstExprTypeAssertion::AstExprTypeAssertion(const Location& location, AstExpr* expr, AstType* annotation)
    : AstExpr(ClassIndex(), location)
    , expr(expr)
    , annotation(annotation)
{
}

void AstExprTypeAssertion::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        expr->visit(visitor);
        annotation->visit(visitor);
    }
}

AstExprIfElse::AstExprIfElse(const Location& location, AstExpr* condition, bool hasThen, AstExpr* trueExpr, bool hasElse, AstExpr* falseExpr)
    : AstExpr(ClassIndex(), location)
    , condition(condition)
    , hasThen(hasThen)
    , trueExpr(trueExpr)
    , hasElse(hasElse)
    , falseExpr(falseExpr)
{
}

void AstExprIfElse::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        condition->visit(visitor);
        trueExpr->visit(visitor);
        falseExpr->visit(visitor);
    }
}

AstExprError::AstExprError(const Location& location, const AstArray<AstExpr*>& expressions, unsigned messageIndex)
    : AstExpr(ClassIndex(), location)
    , expressions(expressions)
    , messageIndex(messageIndex)
{
}

void AstExprError::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (AstExpr* expression : expressions)
            expression->visit(visitor);
    }
}

AstStatBlock::AstStatBlock(const Location& location, const AstArray<AstStat*>& body)
    : AstStat(ClassIndex(), location)
    , body(body)
{
}

void AstStatBlock::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (AstStat* stat : body)
            stat->visit(visitor);
    }
}

AstStatIf::AstStatIf(const Location& location, AstExpr* condition, AstStatBlock* thenbody, AstStat* elsebody, bool hasThen,
    const Location& thenLocation, const std::optional<Location>& elseLocation, bool hasEnd)
    : AstStat(ClassIndex(), location)
    , condition(condition)
    , thenbody(thenbody)
    , elsebody(elsebody)
    , hasThen(hasThen)
    , thenLocation(thenLocation)
    , hasEnd(hasEnd)
{
    if (bool(elseLocation))
    {
        hasElse = true;
        this->elseLocation = *elseLocation;
    }
}

void AstStatIf::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        condition->visit(visitor);
        thenbody->visit(visitor);

        if (elsebody)
            elsebody->visit(visitor);
    }
}

AstStatWhile::AstStatWhile(const Location& location, AstExpr* condition, AstStatBlock* body, bool hasDo, const Location& doLocation, bool hasEnd)
    : AstStat(ClassIndex(), location)
    , condition(condition)
    , body(body)
    , hasDo(hasDo)
    , doLocation(doLocation)
    , hasEnd(hasEnd)
{
}

void AstStatWhile::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        condition->visit(visitor);
        body->visit(visitor);
    }
}

AstStatRepeat::AstStatRepeat(const Location& location, AstExpr* condition, AstStatBlock* body, bool hasUntil)
    : AstStat(ClassIndex(), location)
    , condition(condition)
    , body(body)
    , hasUntil(hasUntil)
{
}

void AstStatRepeat::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        body->visit(visitor);
        condition->visit(visitor);
    }
}

AstStatBreak::AstStatBreak(const Location& location)
    : AstStat(ClassIndex(), location)
{
}

void AstStatBreak::visit(AstVisitor* visitor)
{
    visitor->visit(this);
}

AstStatContinue::AstStatContinue(const Location& location)
    : AstStat(ClassIndex(), location)
{
}

void AstStatContinue::visit(AstVisitor* visitor)
{
    visitor->visit(this);
}

AstStatReturn::AstStatReturn(const Location& location, const AstArray<AstExpr*>& list)
    : AstStat(ClassIndex(), location)
    , list(list)
{
}

void AstStatReturn::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (AstExpr* expr : list)
            expr->visit(visitor);
    }
}

AstStatExpr::AstStatExpr(const Location& location, AstExpr* expr)
    : AstStat(ClassIndex(), location)
    , expr(expr)
{
}

void AstStatExpr::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
        expr->visit(visitor);
}

AstStatLocal::AstStatLocal(
    const Location& location, const AstArray<AstLocal*>& vars, const AstArray<AstExpr*>& values, const std::optional<Location>& equalsSignLocation)
    : AstStat(ClassIndex(), location)
    , vars(vars)
    , values(values)
{
    if (bool(equalsSignLocation))
    {
        hasEqualsSign = true;
        this->equalsSignLocation = *equalsSignLocation;
    }
}

void AstStatLocal::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (AstLocal* var : vars)
        {
            if (var->annotation)
                var->annotation->visit(visitor);
        }

        for (AstExpr* expr : values)
            expr->visit(visitor);
    }
}

AstStatFor::AstStatFor(const Location& location, AstLocal* var, AstExpr* from, AstExpr* to, AstExpr* step, AstStatBlock* body, bool hasDo,
    const Location& doLocation, bool hasEnd)
    : AstStat(ClassIndex(), location)
    , var(var)
    , from(from)
    , to(to)
    , step(step)
    , body(body)
    , hasDo(hasDo)
    , doLocation(doLocation)
    , hasEnd(hasEnd)
{
}

void AstStatFor::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        if (var->annotation)
            var->annotation->visit(visitor);

        from->visit(visitor);
        to->visit(visitor);

        if (step)
            step->visit(visitor);

        body->visit(visitor);
    }
}

AstStatForIn::AstStatForIn(const Location& location, const AstArray<AstLocal*>& vars, const AstArray<AstExpr*>& values, AstStatBlock* body,
    bool hasIn, const Location& inLocation, bool hasDo, const Location& doLocation, bool hasEnd)
    : AstStat(ClassIndex(), location)
    , vars(vars)
    , values(values)
    , body(body)
    , hasIn(hasIn)
    , inLocation(inLocation)
    , hasDo(hasDo)
    , doLocation(doLocation)
    , hasEnd(hasEnd)
{
}

void AstStatForIn::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (AstLocal* var : vars)
        {
            if (var->annotation)
                var->annotation->visit(visitor);
        }

        for (AstExpr* expr : values)
            expr->visit(visitor);

        body->visit(visitor);
    }
}

AstStatAssign::AstStatAssign(const Location& location, const AstArray<AstExpr*>& vars, const AstArray<AstExpr*>& values)
    : AstStat(ClassIndex(), location)
    , vars(vars)
    , values(values)
{
}

void AstStatAssign::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (AstExpr* lvalue : vars)
            lvalue->visit(visitor);

        for (AstExpr* expr : values)
            expr->visit(visitor);
    }
}

AstStatCompoundAssign::AstStatCompoundAssign(const Location& location, AstExprBinary::Op op, AstExpr* var, AstExpr* value)
    : AstStat(ClassIndex(), location)
    , op(op)
    , var(var)
    , value(value)
{
}

void AstStatCompoundAssign::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        var->visit(visitor);
        value->visit(visitor);
    }
}

AstStatFunction::AstStatFunction(const Location& location, AstExpr* name, AstExprFunction* func)
    : AstStat(ClassIndex(), location)
    , name(name)
    , func(func)
{
}

void AstStatFunction::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        name->visit(visitor);
        func->visit(visitor);
    }
}

AstStatLocalFunction::AstStatLocalFunction(const Location& location, AstLocal* name, AstExprFunction* func)
    : AstStat(ClassIndex(), location)
    , name(name)
    , func(func)
{
}

void AstStatLocalFunction::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
        func->visit(visitor);
}

AstStatTypeAlias::AstStatTypeAlias(const Location& location, const AstName& name, const AstArray<AstName>& generics, AstType* type, bool exported)
    : AstStat(ClassIndex(), location)
    , name(name)
    , generics(generics)
    , type(type)
    , exported(exported)
{
}

void AstStatTypeAlias::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
        type->visit(visitor);
}

AstStatDeclareGlobal::AstStatDeclareGlobal(const Location& location, const AstName& name, AstType* type)
    : AstStat(ClassIndex(), location)
    , name(name)
    , type(type)
{
}

void AstStatDeclareGlobal::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
        type->visit(visitor);
}

AstStatDeclareFunction::AstStatDeclareFunction(const Location& location, const AstName& name, const AstArray<AstName>& generics,
    const AstArray<AstName>& genericPacks, const AstTypeList& params, const AstArray<AstArgumentName>& paramNames, const AstTypeList& retTypes)
    : AstStat(ClassIndex(), location)
    , name(name)
    , generics(generics)
    , genericPacks(genericPacks)
    , params(params)
    , paramNames(paramNames)
    , retTypes(retTypes)
{
}

void AstStatDeclareFunction::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        visitTypeList(visitor, params);
        visitTypeList(visitor, retTypes);
    }
}

AstStatDeclareClass::AstStatDeclareClass(
    const Location& location, const AstName& name, std::optional<AstName> superName, const AstArray<AstDeclaredClassProp>& props)
    : AstStat(ClassIndex(), location)
    , name(name)
    , superName(superName)
    , props(props)
{
}

void AstStatDeclareClass::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (const AstDeclaredClassProp& prop : props)
            prop.ty->visit(visitor);
    }
}

AstStatError::AstStatError(
    const Location& location, const AstArray<AstExpr*>& expressions, const AstArray<AstStat*>& statements, unsigned messageIndex)
    : AstStat(ClassIndex(), location)
    , expressions(expressions)
    , statements(statements)
    , messageIndex(messageIndex)
{
}

void AstStatError::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (AstNode* expression : expressions)
            expression->visit(visitor);

        for (AstNode* statement : statements)
            statement->visit(visitor);
    }
}

AstTypeReference::AstTypeReference(const Location& location, std::optional<AstName> prefix, AstName name, const AstArray<AstType*>& generics)
    : AstType(ClassIndex(), location)
    , hasPrefix(bool(prefix))
    , prefix(prefix ? *prefix : AstName())
    , name(name)
    , generics(generics)
{
}

void AstTypeReference::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (AstType* generic : generics)
            generic->visit(visitor);
    }
}

AstTypeTable::AstTypeTable(const Location& location, const AstArray<AstTableProp>& props, AstTableIndexer* indexer)
    : AstType(ClassIndex(), location)
    , props(props)
    , indexer(indexer)
{
}

void AstTypeTable::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (const AstTableProp& prop : props)
            prop.type->visit(visitor);

        if (indexer)
        {
            indexer->indexType->visit(visitor);
            indexer->resultType->visit(visitor);
        }
    }
}

AstTypeFunction::AstTypeFunction(const Location& location, const AstArray<AstName>& generics, const AstArray<AstName>& genericPacks,
    const AstTypeList& argTypes, const AstArray<std::optional<AstArgumentName>>& argNames, const AstTypeList& returnTypes)
    : AstType(ClassIndex(), location)
    , generics(generics)
    , genericPacks(genericPacks)
    , argTypes(argTypes)
    , argNames(argNames)
    , returnTypes(returnTypes)
{
    LUAU_ASSERT(argNames.size == 0 || argNames.size == argTypes.types.size);
}

void AstTypeFunction::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        visitTypeList(visitor, argTypes);
        visitTypeList(visitor, returnTypes);
    }
}

AstTypeTypeof::AstTypeTypeof(const Location& location, AstExpr* expr)
    : AstType(ClassIndex(), location)
    , expr(expr)
{
}

void AstTypeTypeof::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
        expr->visit(visitor);
}

AstTypeUnion::AstTypeUnion(const Location& location, const AstArray<AstType*>& types)
    : AstType(ClassIndex(), location)
    , types(types)
{
}

void AstTypeUnion::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (AstType* type : types)
            type->visit(visitor);
    }
}

AstTypeIntersection::AstTypeIntersection(const Location& location, const AstArray<AstType*>& types)
    : AstType(ClassIndex(), location)
    , types(types)
{
}

void AstTypeIntersection::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (AstType* type : types)
            type->visit(visitor);
    }
}

AstTypeError::AstTypeError(const Location& location, const AstArray<AstType*>& types, bool isMissing, unsigned messageIndex)
    : AstType(ClassIndex(), location)
    , types(types)
    , isMissing(isMissing)
    , messageIndex(messageIndex)
{
}

void AstTypeError::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
    {
        for (AstType* type : types)
            type->visit(visitor);
    }
}

AstTypePackVariadic::AstTypePackVariadic(const Location& location, AstType* variadicType)
    : AstTypePack(ClassIndex(), location)
    , variadicType(variadicType)
{
}

void AstTypePackVariadic::visit(AstVisitor* visitor)
{
    if (visitor->visit(this))
        variadicType->visit(visitor);
}

AstTypePackGeneric::AstTypePackGeneric(const Location& location, AstName name)
    : AstTypePack(ClassIndex(), location)
    , genericName(name)
{
}

void AstTypePackGeneric::visit(AstVisitor* visitor)
{
    visitor->visit(this);
}

AstName getIdentifier(AstExpr* node)
{
    if (AstExprGlobal* expr = node->as<AstExprGlobal>())
        return expr->name;

    if (AstExprLocal* expr = node->as<AstExprLocal>())
        return expr->local->name;

    return AstName();
}

} // namespace Luau
