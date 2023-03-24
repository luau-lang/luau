// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/AssemblyBuilderA64.h"

#include "EmitCommon.h"

#include "lobject.h"
#include "ltm.h"

// AArch64 ABI reminder:
// Arguments: x0-x7, v0-v7
// Return: x0, v0 (or x8 that points to the address of the resulting structure)
// Volatile: x9-x14, v16-v31 ("caller-saved", any call may change them)
// Non-volatile: x19-x28, v8-v15 ("callee-saved", preserved after calls, only bottom half of SIMD registers is preserved!)
// Reserved: x16-x18: reserved for linker/platform use; x29: frame pointer (unless omitted); x30: link register; x31: stack pointer

namespace Luau
{
namespace CodeGen
{

struct NativeState;

namespace A64
{

// Data that is very common to access is placed in non-volatile registers
constexpr RegisterA64 rState = x19;         // lua_State* L
constexpr RegisterA64 rBase = x20;          // StkId base
constexpr RegisterA64 rNativeContext = x21; // NativeContext* context
constexpr RegisterA64 rConstants = x22;     // TValue* k
constexpr RegisterA64 rClosure = x23;       // Closure* cl
constexpr RegisterA64 rCode = x24;          // Instruction* code

// Native code is as stackless as the interpreter, so we can place some data on the stack once and have it accessible at any point
// See CodeGenA64.cpp for layout
constexpr unsigned kStackSize = 64; // 8 stashed registers

inline AddressA64 luauReg(int ri)
{
    return mem(rBase, ri * sizeof(TValue));
}

inline AddressA64 luauRegValue(int ri)
{
    return mem(rBase, ri * sizeof(TValue) + offsetof(TValue, value));
}

inline AddressA64 luauRegTag(int ri)
{
    return mem(rBase, ri * sizeof(TValue) + offsetof(TValue, tt));
}

inline AddressA64 luauConstant(int ki)
{
    return mem(rConstants, ki * sizeof(TValue));
}

inline AddressA64 luauConstantTag(int ki)
{
    return mem(rConstants, ki * sizeof(TValue) + offsetof(TValue, tt));
}

inline AddressA64 luauConstantValue(int ki)
{
    return mem(rConstants, ki * sizeof(TValue) + offsetof(TValue, value));
}

void emitExit(AssemblyBuilderA64& build, bool continueInVm);
void emitUpdateBase(AssemblyBuilderA64& build);
void emitSetSavedPc(AssemblyBuilderA64& build, int pcpos); // invalidates x0/x1
void emitInterrupt(AssemblyBuilderA64& build, int pcpos);

} // namespace A64
} // namespace CodeGen
} // namespace Luau
