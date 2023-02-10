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

// IR instruction command.
// In the command description, following abbreviations are used:
// * Rn - VM stack register slot, n in 0..254
// * Kn - VM proto constant slot, n in 0..2^23-1
// * UPn - VM function upvalue slot, n in 0..254
// * A, B, C, D, E are instruction arguments
enum class IrCmd : uint8_t
{
    NOP,

    // Load a tag from TValue
    // A: Rn or Kn
    LOAD_TAG,

    // Load a pointer (*) from TValue
    // A: Rn or Kn
    LOAD_POINTER,

    // Load a double number from TValue
    // A: Rn or Kn
    LOAD_DOUBLE,

    // Load an int from TValue
    // A: Rn
    LOAD_INT,

    // Load a TValue from memory
    // A: Rn or Kn or pointer (TValue)
    LOAD_TVALUE,

    // Load a TValue from table node value
    // A: pointer (LuaNode)
    LOAD_NODE_VALUE_TV, // TODO: we should find a way to generalize LOAD_TVALUE

    // Load current environment table
    LOAD_ENV,

    // Get pointer (TValue) to table array at index
    // A: pointer (Table)
    // B: unsigned int
    GET_ARR_ADDR,

    // Get pointer (LuaNode) to table node element at the active cached slot index
    // A: pointer (Table)
    GET_SLOT_NODE_ADDR,

    // Store a tag into TValue
    // A: Rn
    // B: tag
    STORE_TAG,

    // Store a pointer (*) into TValue
    // A: Rn
    // B: pointer
    STORE_POINTER,

    // Store a double number into TValue
    // A: Rn
    // B: double
    STORE_DOUBLE,

    // Store an int into TValue
    // A: Rn
    // B: int
    STORE_INT,

    // Store a TValue into memory
    // A: Rn or pointer (TValue)
    // B: TValue
    STORE_TVALUE,

    // Store a TValue into table node value
    // A: pointer (LuaNode)
    // B: TValue
    STORE_NODE_VALUE_TV, // TODO: we should find a way to generalize STORE_TVALUE

    // Add/Sub two integers together
    // A, B: int
    ADD_INT,
    SUB_INT,

    // Add/Sub/Mul/Div/Mod/Pow two double numbers
    // A, B: double
    // In final x64 lowering, B can also be Rn or Kn
    ADD_NUM,
    SUB_NUM,
    MUL_NUM,
    DIV_NUM,
    MOD_NUM,
    POW_NUM,

    // Negate a double number
    // A: double
    UNM_NUM,

    // Compute Luau 'not' operation on destructured TValue
    // A: tag
    // B: double
    NOT_ANY, // TODO: boolean specialization will be useful

    // Unconditional jump
    // A: block
    JUMP,

    // Jump if TValue is truthy
    // A: Rn
    // B: block (if true)
    // C: block (if false)
    JUMP_IF_TRUTHY,

    // Jump if TValue is falsy
    // A: Rn
    // B: block (if true)
    // C: block (if false)
    JUMP_IF_FALSY,

    // Jump if tags are equal
    // A, B: tag
    // C: block (if true)
    // D: block (if false)
    JUMP_EQ_TAG,

    // Jump if two int numbers are equal
    // A, B: int
    // C: block (if true)
    // D: block (if false)
    JUMP_EQ_INT,

    // Jump if pointers are equal
    // A, B: pointer (*)
    // C: block (if true)
    // D: block (if false)
    JUMP_EQ_POINTER,

    // Perform a conditional jump based on the result of double comparison
    // A, B: double
    // C: condition
    // D: block (if true)
    // E: block (if false)
    JUMP_CMP_NUM,

    // Perform a conditional jump based on the result of TValue comparison
    // A, B: Rn
    // C: condition
    // D: block (if true)
    // E: block (if false)
    JUMP_CMP_ANY,

    // Get table length
    // A: pointer (Table)
    TABLE_LEN,

    // Allocate new table
    // A: int (array element count)
    // B: int (node element count)
    NEW_TABLE,

    // Duplicate a table
    // A: pointer (Table)
    DUP_TABLE,

    // Try to convert a double number into a table index or jump if it's not an integer
    // A: double
    // B: block
    NUM_TO_INDEX,

    // Convert integer into a double number
    // A: int
    INT_TO_NUM,

    // Fallback functions

    // Perform an arithmetic operation on TValues of any type
    // A: Rn (where to store the result)
    // B: Rn (lhs)
    // C: Rn or Kn (rhs)
    DO_ARITH,

    // Get length of a TValue of any type
    // A: Rn (where to store the result)
    // B: Rn
    DO_LEN,

    // Lookup a value in TValue of any type using a key of any type
    // A: Rn (where to store the result)
    // B: Rn
    // C: Rn or unsigned int (key)
    GET_TABLE,

    // Store a value into TValue of any type using a key of any type
    // A: Rn (value to store)
    // B: Rn
    // C: Rn or unsigned int (key)
    SET_TABLE,

    // Lookup a value in the environment
    // A: Rn (where to store the result)
    // B: unsigned int (import path)
    GET_IMPORT,

    // Concatenate multiple TValues
    // A: Rn (where to store the result)
    // B: unsigned int (index of the first VM stack slot)
    // C: unsigned int (number of stack slots to go over)
    CONCAT,

    // Load function upvalue into stack slot
    // A: Rn
    // B: UPn
    GET_UPVALUE,

    // Store TValue from stack slot into a function upvalue
    // A: UPn
    // B: Rn
    SET_UPVALUE,

    // Convert TValues into numbers for a numerical for loop
    // A: Rn (start)
    // B: Rn (end)
    // C: Rn (step)
    PREPARE_FORN,

    // Guards and checks (these instructions are not block terminators even though they jump to fallback)

    // Guard against tag mismatch
    // A, B: tag
    // C: block
    // In final x64 lowering, A can also be Rn
    CHECK_TAG,

    // Guard against readonly table
    // A: pointer (Table)
    // B: block
    CHECK_READONLY,

    // Guard against table having a metatable
    // A: pointer (Table)
    // B: block
    CHECK_NO_METATABLE,

    // Guard against executing in unsafe environment
    // A: block
    CHECK_SAFE_ENV,

    // Guard against index overflowing the table array size
    // A: pointer (Table)
    // B: block
    CHECK_ARRAY_SIZE,

    // Guard against cached table node slot not matching the actual table node slot for a key
    // A: pointer (LuaNode)
    // B: Kn
    // C: block
    CHECK_SLOT_MATCH,

    // Special operations

    // Check interrupt handler
    // A: unsigned int (pcpos)
    INTERRUPT,

    // Check and run GC assist if necessary
    CHECK_GC,

    // Handle GC write barrier (forward)
    // A: pointer (GCObject)
    // B: Rn (TValue that was written to the object)
    BARRIER_OBJ,

    // Handle GC write barrier (backwards) for a write into a table
    // A: pointer (Table)
    BARRIER_TABLE_BACK,

    // Handle GC write barrier (forward) for a write into a table
    // A: pointer (Table)
    // B: Rn (TValue that was written to the object)
    BARRIER_TABLE_FORWARD,

    // Update savedpc value
    // A: unsigned int (pcpos)
    SET_SAVEDPC,

    // Close open upvalues for registers at specified index or higher
    // A: Rn (starting register index)
    CLOSE_UPVALS,

    // While capture is a no-op right now, it might be useful to track register/upvalue lifetimes
    // A: Rn or UPn
    // B: boolean (true for reference capture, false for value capture)
    CAPTURE,

    // Operations that don't have an IR representation yet

    // Set a list of values to table in target register
    // A: unsigned int (bytecode instruction index)
    // B: Rn (target)
    // C: Rn (source start)
    // D: int (count or -1 to assign values up to stack top)
    // E: unsigned int (table index to start from)
    LOP_SETLIST,

    // Load function from source register using name into target register and copying source register into target register + 1
    // A: unsigned int (bytecode instruction index)
    // B: Rn (target)
    // C: Rn (source)
    // D: block (next)
    // E: block (fallback)
    LOP_NAMECALL,

    // Call specified function
    // A: unsigned int (bytecode instruction index)
    // B: Rn (function, followed by arguments)
    // C: int (argument count or -1 to preserve all arguments up to stack top)
    // D: int (result count or -1 to preserve all results and adjust stack top)
    // Note: return values are placed starting from Rn specified in 'B'
    LOP_CALL,

    // Return specified values from the function
    // A: unsigned int (bytecode instruction index)
    // B: Rn (value start)
    // B: int (result count or -1 to return all values up to stack top)
    LOP_RETURN,

    // Perform a fast call of a built-in function
    // A: unsigned int (bytecode instruction index)
    // B: Rn (argument start)
    // C: int (argument count or -1 preserve all arguments up to stack top)
    // D: block (fallback)
    // Note: return values are placed starting from Rn specified in 'B'
    LOP_FASTCALL,

    // Perform a fast call of a built-in function using 1 register argument
    // A: unsigned int (bytecode instruction index)
    // B: Rn (result start)
    // C: Rn (arg1)
    // D: block (fallback)
    LOP_FASTCALL1,

    // Perform a fast call of a built-in function using 2 register arguments
    // A: unsigned int (bytecode instruction index)
    // B: Rn (result start)
    // C: Rn (arg1)
    // D: Rn (arg2)
    // E: block (fallback)
    LOP_FASTCALL2,

    // Perform a fast call of a built-in function using 1 register argument and 1 constant argument
    // A: unsigned int (bytecode instruction index)
    // B: Rn (result start)
    // C: Rn (arg1)
    // D: Kn (arg2)
    // E: block (fallback)
    LOP_FASTCALL2K,

    LOP_FORGLOOP,
    LOP_FORGLOOP_FALLBACK,
    LOP_FORGPREP_XNEXT_FALLBACK,

    // Perform `and` or `or` operation (selecting lhs or rhs based on whether the lhs is truthy) and put the result into target register
    // A: unsigned int (bytecode instruction index)
    // B: Rn (target)
    // C: Rn (lhs)
    // D: Rn or Kn (rhs)
    LOP_AND,
    LOP_ANDK,
    LOP_OR,
    LOP_ORK,

    // Increment coverage data (saturating 24 bit add)
    // A: unsigned int (bytecode instruction index)
    LOP_COVERAGE,

    // Operations that have a translation, but use a full instruction fallback

    // Load a value from global table at specified key
    // A: unsigned int (bytecode instruction index)
    // B: Rn (dest)
    // C: Kn (key)
    FALLBACK_GETGLOBAL,

    // Store a value into global table at specified key
    // A: unsigned int (bytecode instruction index)
    // B: Rn (value)
    // C: Kn (key)
    FALLBACK_SETGLOBAL,

    // Load a value from table at specified key
    // A: unsigned int (bytecode instruction index)
    // B: Rn (dest)
    // C: Rn (table)
    // D: Kn (key)
    FALLBACK_GETTABLEKS,

    // Store a value into a table at specified key
    // A: unsigned int (bytecode instruction index)
    // B: Rn (value)
    // C: Rn (table)
    // D: Kn (key)
    FALLBACK_SETTABLEKS,

    // Load function from source register using name into target register and copying source register into target register + 1
    // A: unsigned int (bytecode instruction index)
    // B: Rn (target)
    // C: Rn (source)
    // D: Kn (name)
    FALLBACK_NAMECALL,

    // Operations that don't have assembly lowering at all

    // Prepare stack for variadic functions so that GETVARARGS works correctly
    // A: unsigned int (bytecode instruction index)
    // B: int (numparams)
    FALLBACK_PREPVARARGS,

    // Copy variables into the target registers from vararg storage for current function
    // A: unsigned int (bytecode instruction index)
    // B: Rn (dest start)
    // C: int (count)
    FALLBACK_GETVARARGS,

    // Create closure from a child proto
    // A: unsigned int (bytecode instruction index)
    // B: Rn (dest)
    // C: unsigned int (protoid)
    FALLBACK_NEWCLOSURE,

    // Create closure from a pre-created function object (reusing it unless environments diverge)
    // A: unsigned int (bytecode instruction index)
    // B: Rn (dest)
    // C: Kn (prototype)
    FALLBACK_DUPCLOSURE,

    // Prepare loop variables for a generic for loop, jump to the loop backedge unconditionally
    // A: unsigned int (bytecode instruction index)
    // B: Rn (loop state, updates Rn Rn+1 Rn+2)
    // B: block
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
    Dead,
};

struct IrBlock
{
    IrBlockKind kind;

    uint16_t useCount = 0;

    // Start points to an instruction index in a stream
    // End is implicit
    uint32_t start = ~0u;

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

    IrBlock& blockOp(IrOp op)
    {
        LUAU_ASSERT(op.kind == IrOpKind::Block);
        return blocks[op.index];
    }

    IrInst& instOp(IrOp op)
    {
        LUAU_ASSERT(op.kind == IrOpKind::Inst);
        return instructions[op.index];
    }

    IrConst& constOp(IrOp op)
    {
        LUAU_ASSERT(op.kind == IrOpKind::Constant);
        return constants[op.index];
    }

    uint8_t tagOp(IrOp op)
    {
        IrConst& value = constOp(op);

        LUAU_ASSERT(value.kind == IrConstKind::Tag);
        return value.valueTag;
    }

    bool boolOp(IrOp op)
    {
        IrConst& value = constOp(op);

        LUAU_ASSERT(value.kind == IrConstKind::Bool);
        return value.valueBool;
    }

    int intOp(IrOp op)
    {
        IrConst& value = constOp(op);

        LUAU_ASSERT(value.kind == IrConstKind::Int);
        return value.valueInt;
    }

    unsigned uintOp(IrOp op)
    {
        IrConst& value = constOp(op);

        LUAU_ASSERT(value.kind == IrConstKind::Uint);
        return value.valueUint;
    }

    double doubleOp(IrOp op)
    {
        IrConst& value = constOp(op);

        LUAU_ASSERT(value.kind == IrConstKind::Double);
        return value.valueDouble;
    }
};

} // namespace CodeGen
} // namespace Luau
