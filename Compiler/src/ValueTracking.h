// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/DenseHash2.h"

#include <vector>

namespace Luau
{
class AstNameTable;
}

namespace Luau
{
namespace Compile
{

enum class Global
{
    Default = 0,
    Mutable, // builtin that has contents unknown at compile time, blocks GETIMPORT for chains
    Written, // written in the code which means we can't reason about the value
};

struct Variable
{
    AstExpr* init = nullptr; // initial value of the variable; filled by trackValues
    bool written = false;    // is the variable ever assigned to? filled by trackValues
    bool constant = false;   // is the variable's value a compile-time constant? filled by constantFold
};

void assignMutable(DenseHashMap2<AstName, Global>& globals, const AstNameTable& names, const char* const* mutableGlobals);
void trackValues(
    DenseHashMap2<AstName, Global>& globals,
    DenseHashMap2<AstLocal*, Variable>& variables,
    DenseHashMap2<AstName, AstLocal*>& classLocals,
    DenseHashSet2<AstLocal*>& exportedFunctions,
    std::vector<AstLocal*>& exportedVariables,
    AstNode* root
);
void trackValues_DEPRECATED(
    DenseHashMap2<AstName, Global>& globals,
    DenseHashMap2<AstLocal*, Variable>& variables,
    DenseHashMap2<AstName, AstLocal*>& classLocals,
    AstNode* root
);

inline Global getGlobalState(const DenseHashMap2<AstName, Global>& globals, AstName name)
{
    const Global* it = globals.find(name);

    return it ? *it : Global::Default;
}

} // namespace Compile
} // namespace Luau
