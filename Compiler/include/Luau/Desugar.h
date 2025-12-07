// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Allocator.h"
#include "Luau/Ast.h"
#include "Luau/Lexer.h"

namespace Luau
{

struct DesugarResult
{
    Allocator allocator;
    DenseHashMap<AstStatDataDeclaration*, std::vector<AstStat*>> stats{nullptr};
};

DesugarResult desugar(AstStatBlock* program, AstNameTable& names);

}
