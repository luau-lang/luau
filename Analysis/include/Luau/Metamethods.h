// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"

#include <unordered_map>

namespace Luau
{

inline const std::unordered_map<AstExprBinary::Op, const char*> kBinaryOpMetamethods{
    {AstExprBinary::Op::CompareEq, "__eq"},
    {AstExprBinary::Op::CompareNe, "__eq"},
    {AstExprBinary::Op::CompareGe, "__lt"},
    {AstExprBinary::Op::CompareGt, "__le"},
    {AstExprBinary::Op::CompareLe, "__le"},
    {AstExprBinary::Op::CompareLt, "__lt"},
    {AstExprBinary::Op::Add, "__add"},
    {AstExprBinary::Op::Sub, "__sub"},
    {AstExprBinary::Op::Mul, "__mul"},
    {AstExprBinary::Op::Div, "__div"},
    {AstExprBinary::Op::FloorDiv, "__idiv"},
    {AstExprBinary::Op::Pow, "__pow"},
    {AstExprBinary::Op::Mod, "__mod"},
    {AstExprBinary::Op::Concat, "__concat"},
};

inline const std::unordered_map<AstExprUnary::Op, const char*> kUnaryOpMetamethods{
    {AstExprUnary::Op::Minus, "__unm"},
    {AstExprUnary::Op::Len, "__len"},
};

} // namespace Luau
