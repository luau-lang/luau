// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Label.h"
#include "Luau/RegisterX64.h"
#include "Luau/RegisterA64.h"

#include <vector>

#include <stdint.h>

struct Proto;

namespace Luau
{
namespace CodeGen
{

enum class IrCmd : uint8_t
{
    NOP,

    LOAD_TAG,
    LOAD_POINTER,
    LOAD_DOUBLE,
    LOAD_INT,
    LOAD_TVALUE,
    LOAD_NODE_VALUE_TV, // TODO: we should find a way to generalize LOAD_TVALUE
    LOAD_ENV,

    GET_ARR_ADDR,
    GET_SLOT_NODE_ADDR,

    STORE_TAG,
    STORE_POINTER,
    STORE_DOUBLE,
    STORE_INT,
    STORE_TVALUE,
    STORE_NODE_VALUE_TV, // TODO: we should find a way to generalize STORE_TVALUE

    ADD_INT,
    SUB_INT,

    ADD_NUM,
    SUB_NUM,
    MUL_NUM,
    DIV_NUM,
    MOD_NUM,
    POW_NUM,

    UNM_NUM,

    NOT_ANY, // TODO: boolean specialization will be useful

    JUMP,
    JUMP_IF_TRUTHY,
    JUMP_IF_FALSY,
    JUMP_EQ_TAG,
    JUMP_EQ_BOOLEAN,
    JUMP_EQ_POINTER,

    JUMP_CMP_NUM,
    JUMP_CMP_STR,
    JUMP_CMP_ANY,

    TABLE_LEN,
    NEW_TABLE,
    DUP_TABLE,

    NUM_TO_INDEX,

    // Fallback functions
    DO_ARITH,
    DO_LEN,
    GET_TABLE,
    SET_TABLE,
    GET_IMPORT,
    CONCAT,
    GET_UPVALUE,
    SET_UPVALUE,

    // Guards and checks
    CHECK_TAG,
    CHECK_READONLY,
    CHECK_NO_METATABLE,
    CHECK_SAFE_ENV,
    CHECK_ARRAY_SIZE,
    CHECK_SLOT_MATCH,

    // Special operations
    INTERRUPT,
    CHECK_GC,
    BARRIER_OBJ,
    BARRIER_TABLE_BACK,
    BARRIER_TABLE_FORWARD,
    SET_SAVEDPC,
    CLOSE_UPVALS,

    // While capture is a no-op right now, it might be useful to track register/upvalue lifetimes
    CAPTURE,

    // Operations that don't have an IR representation yet
    LOP_SETLIST,
    LOP_NAMECALL,
    LOP_CALL,
    LOP_RETURN,
    LOP_FASTCALL,
    LOP_FASTCALL1,
    LOP_FASTCALL2,
    LOP_FASTCALL2K,
    LOP_FORNPREP,
    LOP_FORNLOOP,
    LOP_FORGLOOP,
    LOP_FORGLOOP_FALLBACK,
    LOP_FORGPREP_NEXT,
    LOP_FORGPREP_INEXT,
    LOP_FORGPREP_XNEXT_FALLBACK,
    LOP_AND,
    LOP_ANDK,
    LOP_OR,
    LOP_ORK,
    LOP_COVERAGE,

    // Operations that have a translation, but use a full instruction fallback
    FALLBACK_GETGLOBAL,
    FALLBACK_SETGLOBAL,
    FALLBACK_GETTABLEKS,
    FALLBACK_SETTABLEKS,
    FALLBACK_NAMECALL,

    // Operations that don't have assembly lowering at all
    FALLBACK_PREPVARARGS,
    FALLBACK_GETVARARGS,
    FALLBACK_NEWCLOSURE,
    FALLBACK_DUPCLOSURE,
    FALLBACK_FORGPREP,
};

enum class IrConstKind : uint8_t
{
    Bool,
    Int,
    Uint,
    Double,
    Tag,
};

struct IrConst
{
    IrConstKind kind;

    union
    {
        bool valueBool;
        int valueInt;
        unsigned valueUint;
        double valueDouble;
        uint8_t valueTag;
    };
};

enum class IrCondition : uint8_t
{
    Equal,
    NotEqual,
    Less,
    NotLess,
    LessEqual,
    NotLessEqual,
    Greater,
    NotGreater,
    GreaterEqual,
    NotGreaterEqual,

    UnsignedLess,
    UnsignedLessEqual,
    UnsignedGreater,
    UnsignedGreaterEqual,

    Count
};

enum class IrOpKind : uint32_t
{
    None,

    // To reference a constant value
    Constant,

    // To specify a condition code
    Condition,

    // To reference a result of a previous instruction
    Inst,

    // To reference a basic block in control flow
    Block,

    // To reference a VM register
    VmReg,

    // To reference a VM constant
    VmConst,

    // To reference a VM upvalue
    VmUpvalue,
};

struct IrOp
{
    IrOpKind kind : 4;
    uint32_t index : 28;

    IrOp()
        : kind(IrOpKind::None)
        , index(0)
    {
    }

    IrOp(IrOpKind kind, uint32_t index)
        : kind(kind)
        , index(index)
    {
    }
};

static_assert(sizeof(IrOp) == 4);

struct IrInst
{
    IrCmd cmd;

    // Operands
    IrOp a;
    IrOp b;
    IrOp c;
    IrOp d;
    IrOp e;

    uint32_t lastUse = 0;
    uint16_t useCount = 0;

    // Location of the result (optional)
    RegisterX64 regX64 = noreg;
    RegisterA64 regA64{KindA64::none, 0};
    bool reusedReg = false;
};

enum class IrBlockKind : uint8_t
{
    Bytecode,
    Fallback,
    Internal,
};

struct IrBlock
{
    IrBlockKind kind;

    // Start points to an instruction index in a stream
    // End is implicit
    uint32_t start;

    Label label;
};

struct BytecodeMapping
{
    uint32_t irLocation;
    uint32_t asmLocation;
};

struct IrFunction
{
    std::vector<IrBlock> blocks;
    std::vector<IrInst> instructions;
    std::vector<IrConst> constants;

    std::vector<BytecodeMapping> bcMapping;

    Proto* proto = nullptr;
};

} // namespace CodeGen
} // namespace Luau
