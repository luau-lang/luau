// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Ast.h"
#include "Luau/Cst.h"
#include "Luau/Common.h"

namespace Luau
{

int gCstRttiIndex = 0;

CstExprConstantNumber::CstExprConstantNumber(const AstArray<char>& value)
    : CstNode(CstClassIndex())
    , value(value)
{
}

CstExprConstantString::CstExprConstantString(AstArray<char> sourceString, QuoteStyle quoteStyle, unsigned int blockDepth)
    : CstNode(CstClassIndex())
    , sourceString(sourceString)
    , quoteStyle(quoteStyle)
    , blockDepth(blockDepth)
{
    LUAU_ASSERT(blockDepth == 0 || quoteStyle == QuoteStyle::QuotedRaw);
}

CstExprCall::CstExprCall(std::optional<Position> openParens, std::optional<Position> closeParens, AstArray<Position> commaPositions)
    : CstNode(CstClassIndex())
    , openParens(openParens)
    , closeParens(closeParens)
    , commaPositions(commaPositions)
{
}

CstExprIndexExpr::CstExprIndexExpr(Position openBracketPosition, Position closeBracketPosition)
    : CstNode(CstClassIndex())
    , openBracketPosition(openBracketPosition)
    , closeBracketPosition(closeBracketPosition)
{
}

CstExprTable::CstExprTable(const AstArray<Item>& items)
    : CstNode(CstClassIndex())
    , items(items)
{
}

CstExprOp::CstExprOp(Position opPosition)
    : CstNode(CstClassIndex())
    , opPosition(opPosition)
{
}

CstExprIfElse::CstExprIfElse(Position thenPosition, Position elsePosition, bool isElseIf)
    : CstNode(CstClassIndex())
    , thenPosition(thenPosition)
    , elsePosition(elsePosition)
    , isElseIf(isElseIf)
{
}

CstExprInterpString::CstExprInterpString(AstArray<AstArray<char>> sourceStrings, AstArray<Position> stringPositions)
    : CstNode(CstClassIndex())
    , sourceStrings(sourceStrings)
    , stringPositions(stringPositions)
{
}

CstStatDo::CstStatDo(Position endPosition)
    : CstNode(CstClassIndex())
    , endPosition(endPosition)
{
}

CstStatRepeat::CstStatRepeat(Position untilPosition)
    : CstNode(CstClassIndex())
    , untilPosition(untilPosition)
{
}

CstStatReturn::CstStatReturn(AstArray<Position> commaPositions)
    : CstNode(CstClassIndex())
    , commaPositions(commaPositions)
{
}

CstStatLocal::CstStatLocal(AstArray<Position> varsCommaPositions, AstArray<Position> valuesCommaPositions)
    : CstNode(CstClassIndex())
    , varsCommaPositions(varsCommaPositions)
    , valuesCommaPositions(valuesCommaPositions)
{
}

CstStatFor::CstStatFor(Position equalsPosition, Position endCommaPosition, std::optional<Position> stepCommaPosition)
    : CstNode(CstClassIndex())
    , equalsPosition(equalsPosition)
    , endCommaPosition(endCommaPosition)
    , stepCommaPosition(stepCommaPosition)
{
}

CstStatForIn::CstStatForIn(AstArray<Position> varsCommaPositions, AstArray<Position> valuesCommaPositions)
    : CstNode(CstClassIndex())
    , varsCommaPositions(varsCommaPositions)
    , valuesCommaPositions(valuesCommaPositions)
{
}

CstStatAssign::CstStatAssign(
    AstArray<Position> varsCommaPositions,
    Position equalsPosition,
    AstArray<Position> valuesCommaPositions
)
    : CstNode(CstClassIndex())
    , varsCommaPositions(varsCommaPositions)
    , equalsPosition(equalsPosition)
    , valuesCommaPositions(valuesCommaPositions)
{
}

CstStatCompoundAssign::CstStatCompoundAssign(Position opPosition)
    : CstNode(CstClassIndex())
    , opPosition(opPosition)
{
}

CstStatLocalFunction::CstStatLocalFunction(Position functionKeywordPosition)
    : CstNode(CstClassIndex())
    , functionKeywordPosition(functionKeywordPosition)
{
}

CstTypeReference::CstTypeReference(
    std::optional<Position> prefixPointPosition,
    Position openParametersPosition,
    AstArray<Position> parametersCommaPositions,
    Position closeParametersPosition
)
    : CstNode(CstClassIndex())
    , prefixPointPosition(prefixPointPosition)
    , openParametersPosition(openParametersPosition)
    , parametersCommaPositions(parametersCommaPositions)
    , closeParametersPosition(closeParametersPosition)
{
}

CstTypeTable::CstTypeTable(AstArray<Item> items, bool isArray)
    : CstNode(CstClassIndex())
    , items(items)
    , isArray(isArray)
{
}

CstTypeTypeof::CstTypeTypeof(Position openPosition, Position closePosition)
    : CstNode(CstClassIndex())
    , openPosition(openPosition)
    , closePosition(closePosition)
{
}

CstTypeSingletonString::CstTypeSingletonString(AstArray<char> sourceString, CstExprConstantString::QuoteStyle quoteStyle, unsigned int blockDepth)
    : CstNode(CstClassIndex())
    , sourceString(sourceString)
    , quoteStyle(quoteStyle)
    , blockDepth(blockDepth)
{
    LUAU_ASSERT(quoteStyle != CstExprConstantString::QuotedInterp);
}

} // namespace Luau
