// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/AssemblyBuilderX64.h"

#include "lobject.h"
#include "ltm.h"

// MS x64 ABI reminder:
// Arguments: rcx, rdx, r8, r9 ('overlapped' with xmm0-xmm3)
// Return: rax, xmm0
// Nonvolatile: r12-r15, rdi, rsi, rbx, rbp
// SIMD: only xmm6-xmm15 are non-volatile, all ymm upper parts are volatile

// AMD64 ABI reminder:
// Arguments: rdi, rsi, rdx, rcx, r8, r9 (xmm0-xmm7)
// Return: rax, rdx, xmm0, xmm1
// Nonvolatile: r12-r15, rbx, rbp
// SIMD: all volatile

namespace Luau
{
namespace CodeGen
{

struct NativeState;

// Data that is very common to access is placed in non-volatile registers
constexpr RegisterX64 rState = r15;         // lua_State* L
constexpr RegisterX64 rBase = r14;          // StkId base
constexpr RegisterX64 rNativeContext = r13; // NativeContext* context
constexpr RegisterX64 rConstants = r12;     // TValue* k

// Native code is as stackless as the interpreter, so we can place some data on the stack once and have it accessible at any point
// See CodeGenX64.cpp for layout
constexpr unsigned kStackSize = 32 + 16; // 4 home locations for registers, 16 bytes for additional function call arguments
constexpr unsigned kLocalsSize = 24;     // 3 extra slots for our custom locals (also aligns the stack to 16 byte boundary)

constexpr OperandX64 sClosure = qword[rsp + kStackSize + 0]; // Closure* cl
constexpr OperandX64 sCode = qword[rsp + kStackSize + 8];    // Instruction* code

// TODO: These should be replaced with a portable call function that checks the ABI at runtime and reorders moves accordingly to avoid conflicts
#if defined(_WIN32)

constexpr RegisterX64 rArg1 = rcx;
constexpr RegisterX64 rArg2 = rdx;
constexpr RegisterX64 rArg3 = r8;
constexpr RegisterX64 rArg4 = r9;
constexpr RegisterX64 rArg5 = noreg;
constexpr RegisterX64 rArg6 = noreg;
constexpr OperandX64 sArg5 = qword[rsp + 32];
constexpr OperandX64 sArg6 = qword[rsp + 40];

#else

constexpr RegisterX64 rArg1 = rdi;
constexpr RegisterX64 rArg2 = rsi;
constexpr RegisterX64 rArg3 = rdx;
constexpr RegisterX64 rArg4 = rcx;
constexpr RegisterX64 rArg5 = r8;
constexpr RegisterX64 rArg6 = r9;
constexpr OperandX64 sArg5 = noreg;
constexpr OperandX64 sArg6 = noreg;

#endif

constexpr unsigned kTValueSizeLog2 = 4;
constexpr unsigned kLuaNodeSizeLog2 = 5;
constexpr unsigned kLuaNodeTagMask = 0xf;
constexpr unsigned kNextBitOffset = 4;

constexpr unsigned kOffsetOfLuaNodeTag = 12; // offsetof cannot be used on a bit field
constexpr unsigned kOffsetOfLuaNodeNext = 12; // offsetof cannot be used on a bit field
constexpr unsigned kOffsetOfInstructionC = 3;

// Leaf functions that are placed in every module to perform common instruction sequences
struct ModuleHelpers
{
    Label exitContinueVm;
    Label exitNoContinueVm;
    Label continueCallInVm;
};

inline OperandX64 luauReg(int ri)
{
    return xmmword[rBase + ri * sizeof(TValue)];
}

inline OperandX64 luauRegAddress(int ri)
{
    return addr[rBase + ri * sizeof(TValue)];
}

inline OperandX64 luauRegValue(int ri)
{
    return qword[rBase + ri * sizeof(TValue) + offsetof(TValue, value)];
}

inline OperandX64 luauRegTag(int ri)
{
    return dword[rBase + ri * sizeof(TValue) + offsetof(TValue, tt)];
}

inline OperandX64 luauRegValueInt(int ri)
{
    return dword[rBase + ri * sizeof(TValue) + offsetof(TValue, value)];
}

inline OperandX64 luauConstant(int ki)
{
    return xmmword[rConstants + ki * sizeof(TValue)];
}

inline OperandX64 luauConstantAddress(int ki)
{
    return addr[rConstants + ki * sizeof(TValue)];
}

inline OperandX64 luauConstantTag(int ki)
{
    return dword[rConstants + ki * sizeof(TValue) + offsetof(TValue, tt)];
}

inline OperandX64 luauConstantValue(int ki)
{
    return qword[rConstants + ki * sizeof(TValue) + offsetof(TValue, value)];
}

inline OperandX64 luauNodeKeyValue(RegisterX64 node)
{
    return qword[node + offsetof(LuaNode, key) + offsetof(TKey, value)];
}

// Note: tag has dirty upper bits
inline OperandX64 luauNodeKeyTag(RegisterX64 node)
{
    return dword[node + offsetof(LuaNode, key) + kOffsetOfLuaNodeTag];
}

inline OperandX64 luauNodeValue(RegisterX64 node)
{
    return xmmword[node + offsetof(LuaNode, val)];
}

inline void setLuauReg(AssemblyBuilderX64& build, RegisterX64 tmp, int ri, OperandX64 op)
{
    LUAU_ASSERT(op.cat == CategoryX64::mem);

    build.vmovups(tmp, op);
    build.vmovups(luauReg(ri), tmp);
}

inline void setNodeValue(AssemblyBuilderX64& build, RegisterX64 tmp, OperandX64 op, int ri)
{
    LUAU_ASSERT(op.cat == CategoryX64::mem);

    build.vmovups(tmp, luauReg(ri));
    build.vmovups(op, tmp);
}

inline void jumpIfTagIs(AssemblyBuilderX64& build, int ri, lua_Type tag, Label& label)
{
    build.cmp(luauRegTag(ri), tag);
    build.jcc(ConditionX64::Equal, label);
}

inline void jumpIfTagIsNot(AssemblyBuilderX64& build, int ri, lua_Type tag, Label& label)
{
    build.cmp(luauRegTag(ri), tag);
    build.jcc(ConditionX64::NotEqual, label);
}

inline void jumpIfTagIsNot(AssemblyBuilderX64& build, RegisterX64 reg, lua_Type tag, Label& label)
{
    build.cmp(dword[reg + offsetof(TValue, tt)], tag);
    build.jcc(ConditionX64::NotEqual, label);
}

// Note: fallthrough label should be placed after this condition
inline void jumpIfFalsy(AssemblyBuilderX64& build, int ri, Label& target, Label& fallthrough)
{
    jumpIfTagIs(build, ri, LUA_TNIL, target);             // false if nil
    jumpIfTagIsNot(build, ri, LUA_TBOOLEAN, fallthrough); // true if not nil or boolean

    build.cmp(luauRegValueInt(ri), 0);
    build.jcc(ConditionX64::Equal, target); // true if boolean value is 'true'
}

// Note: fallthrough label should be placed after this condition
inline void jumpIfTruthy(AssemblyBuilderX64& build, int ri, Label& target, Label& fallthrough)
{
    jumpIfTagIs(build, ri, LUA_TNIL, fallthrough);   // false if nil
    jumpIfTagIsNot(build, ri, LUA_TBOOLEAN, target); // true if not nil or boolean

    build.cmp(luauRegValueInt(ri), 0);
    build.jcc(ConditionX64::NotEqual, target); // true if boolean value is 'true'
}

inline void jumpIfMetatablePresent(AssemblyBuilderX64& build, RegisterX64 table, Label& target)
{
    build.cmp(qword[table + offsetof(Table, metatable)], 0);
    build.jcc(ConditionX64::NotEqual, target);
}

inline void jumpIfUnsafeEnv(AssemblyBuilderX64& build, RegisterX64 tmp, Label& label)
{
    build.mov(tmp, sClosure);
    build.mov(tmp, qword[tmp + offsetof(Closure, env)]);
    build.test(byte[tmp + offsetof(Table, safeenv)], 1);
    build.jcc(ConditionX64::Zero, label); // Not a safe environment
}

inline void jumpIfTableIsReadOnly(AssemblyBuilderX64& build, RegisterX64 table, Label& label)
{
    build.cmp(byte[table + offsetof(Table, readonly)], 0);
    build.jcc(ConditionX64::NotEqual, label);
}

inline void jumpIfNodeKeyTagIsNot(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 node, lua_Type tag, Label& label)
{
    tmp.size = SizeX64::dword;

    build.mov(tmp, luauNodeKeyTag(node));
    build.and_(tmp, kLuaNodeTagMask);
    build.cmp(tmp, tag);
    build.jcc(ConditionX64::NotEqual, label);
}

inline void jumpIfNodeValueTagIs(AssemblyBuilderX64& build, RegisterX64 node, lua_Type tag, Label& label)
{
    build.cmp(dword[node + offsetof(LuaNode, val) + offsetof(TValue, tt)], tag);
    build.jcc(ConditionX64::Equal, label);
}

inline void jumpIfNodeHasNext(AssemblyBuilderX64& build, RegisterX64 node, Label& label)
{
    build.mov(ecx, dword[node + offsetof(LuaNode, key) + kOffsetOfLuaNodeNext]);
    build.shr(ecx, kNextBitOffset);
    build.jcc(ConditionX64::NotZero, label);
}

inline void jumpIfNodeKeyNotInExpectedSlot(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 node, OperandX64 expectedKey, Label& label)
{
    jumpIfNodeKeyTagIsNot(build, tmp, node, LUA_TSTRING, label);

    build.mov(tmp, expectedKey);
    build.cmp(tmp, luauNodeKeyValue(node));
    build.jcc(ConditionX64::NotEqual, label);

    jumpIfNodeValueTagIs(build, node, LUA_TNIL, label);
}

void jumpOnNumberCmp(AssemblyBuilderX64& build, RegisterX64 tmp, OperandX64 lhs, OperandX64 rhs, ConditionX64 cond, Label& label);
void jumpOnAnyCmpFallback(AssemblyBuilderX64& build, int ra, int rb, ConditionX64 cond, Label& label);

void getTableNodeAtCachedSlot(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 node, RegisterX64 table, int pcpos);
void convertNumberToIndexOrJump(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 numd, RegisterX64 numi, Label& label);

void callArithHelper(AssemblyBuilderX64& build, int ra, int rb, OperandX64 c, TMS tm);
void callLengthHelper(AssemblyBuilderX64& build, int ra, int rb);
void callPrepareForN(AssemblyBuilderX64& build, int limit, int step, int init);
void callGetTable(AssemblyBuilderX64& build, int rb, OperandX64 c, int ra);
void callSetTable(AssemblyBuilderX64& build, int rb, OperandX64 c, int ra);
void callBarrierTable(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 table, int ra, Label& skip);
void callBarrierObject(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 object, int ra, Label& skip);
void callBarrierTableFast(AssemblyBuilderX64& build, RegisterX64 table, Label& skip);
void callCheckGc(AssemblyBuilderX64& build, int pcpos, bool savepc, Label& skip);
void callGetFastTmOrFallback(AssemblyBuilderX64& build, RegisterX64 table, TMS tm, Label& fallback);

void emitExit(AssemblyBuilderX64& build, bool continueInVm);
void emitUpdateBase(AssemblyBuilderX64& build);
void emitSetSavedPc(AssemblyBuilderX64& build, int pcpos); // Note: only uses rax/rdx, the caller may use other registers
void emitInterrupt(AssemblyBuilderX64& build, int pcpos);
void emitFallback(AssemblyBuilderX64& build, NativeState& data, int op, int pcpos);

void emitContinueCallInVm(AssemblyBuilderX64& build);

} // namespace CodeGen
} // namespace Luau
