// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

namespace Luau
{
namespace CodeGen
{

class AssemblyBuilderX64;
struct Label;
struct OperandX64;
struct BuiltinImplResult;

BuiltinImplResult emitBuiltin(AssemblyBuilderX64& build, int bfid, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback);

} // namespace CodeGen
} // namespace Luau
