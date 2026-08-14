// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Bytecode.h"
#include "Luau/Compiler.h"
#include "Luau/DenseHash2.h"
#include "ValueTracking.h"

#include <string>

namespace Luau
{
class BytecodeBuilder;

struct BuiltinAstTypes
{
    BuiltinAstTypes(const char* hostVectorType)
        : hostVectorType{{}, std::nullopt, AstName{hostVectorType}, std::nullopt, {}}
    {
    }

    // AstName use here will not match the AstNameTable, but the way we use them here always forces a full string compare
    AstTypeReference booleanType{{}, std::nullopt, AstName{"boolean"}, std::nullopt, {}};
    AstTypeReference numberType{{}, std::nullopt, AstName{"number"}, std::nullopt, {}};
    AstTypeReference integerType{{}, std::nullopt, AstName{"integer"}, std::nullopt, {}};
    AstTypeReference stringType{{}, std::nullopt, AstName{"string"}, std::nullopt, {}};
    AstTypeReference vectorType{{}, std::nullopt, AstName{"vector"}, std::nullopt, {}};

    AstTypeReference hostVectorType;
};

void buildTypeMap(
    DenseHashMap2<AstExprFunction*, std::string>& functionTypes,
    DenseHashMap2<AstLocal*, LuauBytecodeType>& localTypes,
    DenseHashMap2<AstExpr*, LuauBytecodeType>& exprTypes,
    AstNode* root,
    const char* hostVectorType,
    const DenseHashMap2<AstName, uint8_t>& userdataTypes,
    const BuiltinAstTypes& builtinTypes,
    const DenseHashMap2<AstExprCall*, int>& builtinCalls,
    const DenseHashMap2<AstName, Compile::Global>& globals,
    LibraryMemberTypeCallback libraryMemberTypeCb,
    BytecodeBuilder& bytecode
);

} // namespace Luau
