// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Bytecode.h"
#include "Luau/DenseHash.h"
#include "ValueTracking.h"

#include <string>

namespace Luau
{
class BytecodeBuilder;

struct BuiltinAstTypes
{
    BuiltinAstTypes(const char* vectorType)
        : vectorType{{}, std::nullopt, AstName{vectorType}, std::nullopt, {}}
    {
    }

    // AstName use here will not match the AstNameTable, but the was we use them here always force a full string compare
    AstTypeReference booleanType{{}, std::nullopt, AstName{"boolean"}, std::nullopt, {}};
    AstTypeReference numberType{{}, std::nullopt, AstName{"number"}, std::nullopt, {}};
    AstTypeReference stringType{{}, std::nullopt, AstName{"string"}, std::nullopt, {}};
    AstTypeReference vectorType;
};

void buildTypeMap(
    DenseHashMap<AstExprFunction*, std::string>& functionTypes,
    DenseHashMap<AstLocal*, LuauBytecodeType>& localTypes,
    DenseHashMap<AstExpr*, LuauBytecodeType>& exprTypes,
    AstNode* root,
    const char* vectorType,
    const DenseHashMap<AstName, uint8_t>& userdataTypes,
    const BuiltinAstTypes& builtinTypes,
    const DenseHashMap<AstExprCall*, int>& builtinCalls,
    const DenseHashMap<AstName, Compile::Global>& globals,
    BytecodeBuilder& bytecode
);

} // namespace Luau
