// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "ValueTracking.h"

namespace Luau
{
namespace Compile
{

struct Constant
{
    enum Type
    {
        Type_Unknown,
        Type_Nil,
        Type_Boolean,
        Type_Number,
        Type_String,
    };

    Type type = Type_Unknown;
    unsigned int stringLength = 0;

    union
    {
        bool valueBoolean;
        double valueNumber;
        char* valueString = nullptr; // length stored in stringLength
    };

    bool isTruthful() const
    {
        LUAU_ASSERT(type != Type_Unknown);
        return type != Type_Nil && !(type == Type_Boolean && valueBoolean == false);
    }

    AstArray<char> getString() const
    {
        LUAU_ASSERT(type == Type_String);
        return {valueString, stringLength};
    }
};

void foldConstants(DenseHashMap<AstExpr*, Constant>& constants, DenseHashMap<AstLocal*, Variable>& variables,
    DenseHashMap<AstLocal*, Constant>& locals, AstNode* root);

} // namespace Compile
} // namespace Luau
