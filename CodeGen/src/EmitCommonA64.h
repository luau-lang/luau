// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/AssemblyBuilderA64.h"

#include "EmitCommon.h"

#include "lobject.h"
#include "ltm.h"

// AArch64 ABI reminder:
// Arguments: x0-x7, v0-v7
// Return: x0, v0 (or x8 that points to the address of the resulting structure)
// Volatile: x9-x15, v16-v31 ("caller-saved", any call may change them)
// Non-volatile: x19-x28, v8-v15 ("callee-saved", preserved after calls, only bottom half of SIMD registers is preserved!)
// Reserved: x16-x18: reserved for linker/platform use; x29: frame pointer (unless omitted); x30: link register; x31: stack pointer

namespace Luau
{
namespace CodeGen
{

struct NativeState;

namespace A64
{

// Data that is very common to access is placed in non-volatile registers:
// 1. Constant registers (only loaded during codegen entry)
constexpr RegisterA64 rState = x19;         // lua_State* L
constexpr RegisterA64 rNativeContext = x20; // NativeContext* context

// 2. Frame registers (reloaded when call frame changes; rBase is also reloaded after all calls that may reallocate stack)
constexpr RegisterA64 rConstants = x21; // TValue* k
constexpr RegisterA64 rClosure = x22;   // Closure* cl
constexpr RegisterA64 rCode = x23;      // Instruction* code
constexpr RegisterA64 rBase = x24;      // StkId base

// Native code is as stackless as the interpreter, so we can place some data on the stack once and have it accessible at any point
// See CodeGenA64.cpp for layout
constexpr unsigned kStackSize = 64; // 8 stashed registers

void emitUpdateBase(AssemblyBuilderA64& build);

// TODO: Move these to CodeGenA64 so that they can't be accidentally called during lowering
void emitExit(AssemblyBuilderA64& build, bool continueInVm);
void emitInterrupt(AssemblyBuilderA64& build);
void emitReentry(AssemblyBuilderA64& build, ModuleHelpers& helpers);
void emitFallback(AssemblyBuilderA64& build, int op, int pcpos);

} // namespace A64
} // namespace CodeGen
} // namespace Luau
