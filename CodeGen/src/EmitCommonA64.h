// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/AssemblyBuilderA64.h"

#include "EmitCommon.h"

#include "lobject.h"
#include "ltm.h"
#include "lstate.h"

// AArch64 ABI reminder:
// Arguments: x0-x7, v0-v7
// Return: x0, v0 (or x8 that points to the address of the resulting structure)
// Volatile: x9-x15, v16-v31 ("caller-saved", any call may change them)
// Intra-procedure-call temporary: x16-x17 (any call or relocated jump may change them, as linker may point branches to veneers to perform long jumps)
// Non-volatile: x19-x28, v8-v15 ("callee-saved", preserved after calls, only bottom half of SIMD registers is preserved!)
// Reserved: x18: reserved for platform use; x29: frame pointer (unless omitted); x30: link register; x31: stack pointer

namespace Luau
{
namespace CodeGen
{

namespace A64
{

// Data that is very common to access is placed in non-volatile registers:
// 1. Constant registers (only loaded during codegen entry)
inline constexpr RegisterA64 rState = x19;         // lua_State* L
inline constexpr RegisterA64 rNativeContext = x20; // NativeContext* context
inline constexpr RegisterA64 rGlobalState = x21;   // global_State* L->global

// 2. Frame registers (reloaded when call frame changes; rBase is also reloaded after all calls that may reallocate stack)
inline constexpr RegisterA64 rConstants = x22; // TValue* k
inline constexpr RegisterA64 rClosure = x23;   // Closure* cl
inline constexpr RegisterA64 rCode = x24;      // Instruction* code
inline constexpr RegisterA64 rBase = x25;      // StkId base

// Native code is as stackless as the interpreter, so we can place some data on the stack once and have it accessible at any point
// See CodeGenA64.cpp for layout
inline constexpr unsigned kStashSlots = 9;  // stashed non-volatile registers
inline constexpr unsigned kTempSlots = 1;   // 8 bytes of temporary space, such luxury!
inline constexpr unsigned kSpillSlots = 22; // slots for spilling temporary registers

inline constexpr unsigned kStackSize = (kStashSlots + kTempSlots + kSpillSlots) * 8;

inline constexpr AddressA64 sSpillArea = mem(sp, (kStashSlots + kTempSlots) * 8);
inline constexpr AddressA64 sTemporary = mem(sp, kStashSlots * 8);

inline void emitUpdateBase(AssemblyBuilderA64& build)
{
    build.ldr(rBase, mem(rState, offsetof(lua_State, base)));
}

// Double to integer conversions that produce the same results as x64 cvttsd2si and the VM for every input, see NUM_TO_INT/NUM_TO_INT64/NUM_TO_UINT
// fcvtzs saturates out-of-range values and converts NaN to 0, so these fix up such results; all of them clobber the condition flags

// Truncates 'src' to int32; values outside of [-2^31, 2^31) and NaN produce INT_MIN
// This doesn't need a temporary register, so register restore can use it
inline void emitNumToInt(AssemblyBuilderA64& build, RegisterA64 dst, RegisterA64 src)
{
    CODEGEN_ASSERT(dst.kind == KindA64::w && src.kind == KindA64::d);

    RegisterA64 dstx = castReg(KindA64::x, dst);

    // 64-bit conversion is exact for every value that truncates to an int32, so the result fits in 32 bits exactly when 'src' is in range
    // INT_MAX can't be used to detect 32-bit saturation because values in [2^31 - 1, 2^31) truncate to it
    build.fcvtzs(dstx, src);
    build.cmp_sxtw(dstx, dst);
    build.fccmp(src, src, ConditionA64::Equal, 0); // when the result fits, check that 'src' is not NaN; otherwise make 'eq' false
    // INT_MIN is produced by zeroing the value with its sign bit flipped
    build.eor(dst, dst, 0x80000000);
    build.csel(dst, dst, wzr, ConditionA64::Equal);
    build.eor(dst, dst, 0x80000000);
}

// Truncates 'src' to int64; values outside of [-2^63, 2^63) and NaN produce INT64_MIN
inline void emitNumToInt64(AssemblyBuilderA64& build, RegisterA64 dst, RegisterA64 src, RegisterA64 temp)
{
    CODEGEN_ASSERT(dst.kind == KindA64::x && temp.kind == KindA64::x && src.kind == KindA64::d && dst != temp);

    // values below -2^63 saturate to INT64_MIN already; values at or above 2^63 saturate to INT64_MAX, which in-range values never truncate to
    build.fcvtzs(dst, src);
    build.cmn(dst, 1);                                       // 'vs' if the result is INT64_MAX
    build.fccmp(src, src, ConditionA64::NoOverflow, 0b0001); // if it's not, 'vs' if 'src' is NaN; otherwise keep 'vs' set
    build.movz(temp, 0x8000, 48);                            // INT64_MIN
    build.csel(dst, temp, dst, ConditionA64::Overflow);
}

// Truncates 'src' to int64 and keeps the low 32 bits (matches luai_num2unsigned); values outside of (-2^63, 2^63) and NaN produce 0
// note: we don't use fcvtzu for consistency with C++ code
inline void emitNumToUint(AssemblyBuilderA64& build, RegisterA64 dst, RegisterA64 src)
{
    CODEGEN_ASSERT(dst.kind == KindA64::w && src.kind == KindA64::d);

    RegisterA64 dstx = castReg(KindA64::x, dst);

    // NaN converts to 0 and values below -2^63 to INT64_MIN, whose low 32 bits are 0 already
    // values at or above 2^63 saturate to INT64_MAX, which in-range values never truncate to and is the only value where +1 overflows
    build.fcvtzs(dstx, src);
    build.cmn(dstx, 1);
    build.csel(dst, wzr, dst, ConditionA64::Overflow);
}

} // namespace A64
} // namespace CodeGen
} // namespace Luau
