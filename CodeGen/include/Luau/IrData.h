// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/IrAnalysis.h"
#include "Luau/Label.h"
#include "Luau/RegisterX64.h"
#include "Luau/RegisterA64.h"

#include <optional>
#include <vector>

#include <stdint.h>
#include <string.h>

struct Proto;

namespace Luau
{
namespace CodeGen
{

// IR extensions to LuauBuiltinFunction enum (these only exist inside IR, and start from 256 to avoid collisions)
enum
{
    LBF_IR_MATH_LOG2 = 256,
};

// IR instruction command.
// In the command description, following abbreviations are used:
// * Rn - VM stack register slot, n in 0..254
// * Kn - VM proto constant slot, n in 0..2^23-1
// * UPn - VM function upvalue slot, n in 0..199
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
    // B: int (optional 'A' pointer offset)
    LOAD_TVALUE,

    // Load current environment table
    LOAD_ENV,

    // Get pointer (TValue) to table array at index
    // A: pointer (Table)
    // B: int
    GET_ARR_ADDR,

    // Get pointer (LuaNode) to table node element at the active cached slot index
    // A: pointer (Table)
    // B: unsigned int (pcpos)
    // C: Kn
    GET_SLOT_NODE_ADDR,

    // Get pointer (LuaNode) to table node element at the main position of the specified key hash
    // A: pointer (Table)
    // B: unsigned int (hash)
    GET_HASH_NODE_ADDR,

    // Get pointer (TValue) to Closure upvalue.
    // A: pointer or undef (Closure)
    // B: UPn
    // When undef is specified, uses current function Closure.
    GET_CLOSURE_UPVAL_ADDR,

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

    // Store a vector into TValue
    // A: Rn
    // B: double (x)
    // C: double (y)
    // D: double (z)
    STORE_VECTOR,

    // Store a TValue into memory
    // A: Rn or pointer (TValue)
    // B: TValue
    // C: int (optional 'A' pointer offset)
    STORE_TVALUE,

    // Store a pair of tag and value into memory
    // A: Rn or pointer (TValue)
    // B: tag (must be a constant)
    // C: int/double/pointer
    // D: int (optional 'A' pointer offset)
    STORE_SPLIT_TVALUE,

    // Add/Sub two integers together
    // A, B: int
    ADD_INT,
    SUB_INT,

    // Add/Sub/Mul/Div/Mod two double numbers
    // A, B: double
    // In final x64 lowering, B can also be Rn or Kn
    ADD_NUM,
    SUB_NUM,
    MUL_NUM,
    DIV_NUM,
    IDIV_NUM,
    MOD_NUM,

    // Get the minimum/maximum of two numbers
    // If one of the values is NaN, 'B' is returned as the result
    // A, B: double
    // In final x64 lowering, B can also be Rn or Kn
    MIN_NUM,
    MAX_NUM,

    // Negate a double number
    // A: double
    UNM_NUM,

    // Round number to negative infinity (math.floor)
    // A: double
    FLOOR_NUM,

    // Round number to positive infinity (math.ceil)
    // A: double
    CEIL_NUM,

    // Round number to nearest integer number, rounding half-way cases away from zero (math.round)
    // A: double
    ROUND_NUM,

    // Get square root of the argument (math.sqrt)
    // A: double
    SQRT_NUM,

    // Get absolute value of the argument (math.abs)
    // A: double
    ABS_NUM,

    // Compute Luau 'not' operation on destructured TValue
    // A: tag
    // B: int (value)
    NOT_ANY,

    // Perform a TValue comparison, supported conditions are LessEqual, Less and Equal
    // A, B: Rn
    // C: condition
    CMP_ANY,

    // Unconditional jump
    // A: block/vmexit/undef
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

    // Perform a conditional jump based on the result of integer comparison
    // A, B: int
    // C: condition
    // D: block (if true)
    // E: block (if false)
    JUMP_CMP_INT,

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

    // Perform jump based on a numerical loop condition (step > 0 ? idx <= limit : limit <= idx)
    // A: double (index)
    // B: double (limit)
    // C: double (step)
    // D: block (if true)
    // E: block (if false)
    JUMP_FORN_LOOP_COND,

    // Perform a conditional jump based on cached table node slot matching the actual table node slot for a key
    // A: pointer (LuaNode)
    // B: Kn
    // C: block (if matches)
    // D: block (if it doesn't)
    JUMP_SLOT_MATCH,

    // Get table length
    // A: pointer (Table)
    TABLE_LEN,

    // Get string length
    // A: pointer (string)
    STRING_LEN,

    // Allocate new table
    // A: unsigned int (array element count)
    // B: unsigned int (node element count)
    NEW_TABLE,

    // Duplicate a table
    // A: pointer (Table)
    DUP_TABLE,

    // Insert an integer key into a table and return the pointer to inserted value (TValue)
    // A: pointer (Table)
    // B: int (key)
    TABLE_SETNUM,

    // Try to convert a double number into a table index (int) or jump if it's not an integer
    // A: double
    // B: block
    TRY_NUM_TO_INDEX,

    // Try to get pointer to tag method TValue inside the table's metatable or jump if there is no such value or metatable
    // A: table
    // B: int (TMS enum)
    // C: block
    TRY_CALL_FASTGETTM,

    // Convert integer into a double number
    // A: int
    INT_TO_NUM,
    UINT_TO_NUM,

    // Converts a double number to an integer. 'A' may be any representable integer in a double.
    // A: double
    NUM_TO_INT,

    // Converts a double number to an unsigned integer. For out-of-range values of 'A', the result is arch-specific.
    // A: double
    NUM_TO_UINT,

    // Adjust stack top (L->top) to point at 'B' TValues *after* the specified register
    // This is used to return multiple values
    // A: Rn
    // B: int (offset)
    ADJUST_STACK_TO_REG,

    // Restore stack top (L->top) to point to the function stack top (L->ci->top)
    // This is used to recover after calling a variadic function
    ADJUST_STACK_TO_TOP,

    // Execute fastcall builtin function in-place
    // A: builtin
    // B: Rn (result start)
    // C: Rn (argument start)
    // D: Rn or Kn or undef (optional second argument)
    // E: int (argument count)
    // F: int (result count)
    FASTCALL,

    // Call the fastcall builtin function
    // A: builtin
    // B: Rn (result start)
    // C: Rn (argument start)
    // D: Rn or Kn or undef (optional second argument)
    // E: int (argument count or -1 to use all arguments up to stack top)
    // F: int (result count or -1 to preserve all results and adjust stack top)
    INVOKE_FASTCALL,

    // Check that fastcall builtin function invocation was successful (negative result count jumps to fallback)
    // A: int (result count)
    // B: block (fallback)
    CHECK_FASTCALL_RES,

    // Fallback functions

    // Perform an arithmetic operation on TValues of any type
    // A: Rn (where to store the result)
    // B: Rn (lhs)
    // C: Rn or Kn (rhs)
    // D: int (TMS enum with arithmetic type)
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

    // Concatenate multiple TValues into a string
    // A: Rn (value start)
    // B: unsigned int (number of registers to go over)
    // Note: result is stored in the register specified in 'A'
    // Note: all referenced registers might be modified in the operation
    CONCAT,

    // Load function upvalue into stack slot
    // A: Rn
    // B: UPn
    GET_UPVALUE,

    // Store TValue from stack slot into a function upvalue
    // A: UPn
    // B: Rn
    // C: tag/undef (tag of the value that was written)
    SET_UPVALUE,

    // Guards and checks (these instructions are not block terminators even though they jump to fallback)

    // Guard against tag mismatch
    // A, B: tag
    // C: block/vmexit/undef
    // In final x64 lowering, A can also be Rn
    // When undef is specified instead of a block, execution is aborted on check failure
    CHECK_TAG,

    // Guard against a falsy tag+value
    // A: tag
    // B: value
    // C: block/vmexit/undef
    CHECK_TRUTHY,

    // Guard against readonly table
    // A: pointer (Table)
    // B: block/vmexit/undef
    // When undef is specified instead of a block, execution is aborted on check failure
    CHECK_READONLY,

    // Guard against table having a metatable
    // A: pointer (Table)
    // B: block/vmexit/undef
    // When undef is specified instead of a block, execution is aborted on check failure
    CHECK_NO_METATABLE,

    // Guard against executing in unsafe environment, exits to VM on check failure
    // A: vmexit/vmexit/undef
    // When undef is specified, execution is aborted on check failure
    CHECK_SAFE_ENV,

    // Guard against index overflowing the table array size
    // A: pointer (Table)
    // B: int (index)
    // C: block/vmexit/undef
    // When undef is specified instead of a block, execution is aborted on check failure
    CHECK_ARRAY_SIZE,

    // Guard against cached table node slot not matching the actual table node slot for a key
    // A: pointer (LuaNode)
    // B: Kn
    // C: block/undef
    // When undef is specified instead of a block, execution is aborted on check failure
    CHECK_SLOT_MATCH,

    // Guard against table node with a linked next node to ensure that our lookup hits the main position of the key
    // A: pointer (LuaNode)
    // B: block/vmexit/undef
    // When undef is specified instead of a block, execution is aborted on check failure
    CHECK_NODE_NO_NEXT,

    // Guard against table node with 'nil' value
    // A: pointer (LuaNode)
    // B: block/vmexit/undef
    // When undef is specified instead of a block, execution is aborted on check failure
    CHECK_NODE_VALUE,

    // Guard against access at specified offset/size overflowing the buffer length
    // A: pointer (buffer)
    // B: int (offset)
    // C: int (size)
    // D: block/vmexit/undef
    // When undef is specified instead of a block, execution is aborted on check failure
    CHECK_BUFFER_LEN,

    // Special operations

    // Check interrupt handler
    // A: unsigned int (pcpos)
    INTERRUPT,

    // Check and run GC assist if necessary
    CHECK_GC,

    // Handle GC write barrier (forward)
    // A: pointer (GCObject)
    // B: Rn (TValue that was written to the object)
    // C: tag/undef (tag of the value that was written)
    BARRIER_OBJ,

    // Handle GC write barrier (backwards) for a write into a table
    // A: pointer (Table)
    BARRIER_TABLE_BACK,

    // Handle GC write barrier (forward) for a write into a table
    // A: pointer (Table)
    // B: Rn (TValue that was written to the object)
    // C: tag/undef (tag of the value that was written)
    BARRIER_TABLE_FORWARD,

    // Update savedpc value
    // A: unsigned int (pcpos)
    SET_SAVEDPC,

    // Close open upvalues for registers at specified index or higher
    // A: Rn (starting register index)
    CLOSE_UPVALS,

    // While capture is a no-op right now, it might be useful to track register/upvalue lifetimes
    // A: Rn or UPn
    // B: unsigned int (1 for reference capture, 0 for value capture)
    CAPTURE,

    // Operations that don't have an IR representation yet

    // Set a list of values to table in target register
    // A: unsigned int (bytecode instruction index)
    // B: Rn (target)
    // C: Rn (source start)
    // D: int (count or -1 to assign values up to stack top)
    // E: unsigned int (table index to start from)
    // F: undef/unsigned int (target table known size)
    SETLIST,

    // Call specified function
    // A: Rn (function, followed by arguments)
    // B: int (argument count or -1 to use all arguments up to stack top)
    // C: int (result count or -1 to preserve all results and adjust stack top)
    // Note: return values are placed starting from Rn specified in 'A'
    CALL,

    // Return specified values from the function
    // A: Rn (value start)
    // B: int (result count or -1 to return all values up to stack top)
    RETURN,

    // Adjust loop variables for one iteration of a generic for loop, jump back to the loop header if loop needs to continue
    // A: Rn (loop variable start, updates Rn+2 and 'B' number of registers starting from Rn+3)
    // B: int (loop variable count, if more than 2, registers starting from Rn+5 are set to nil)
    // C: block (repeat)
    // D: block (exit)
    FORGLOOP,

    // Handle LOP_FORGLOOP fallback when variable being iterated is not a table
    // A: Rn (loop state start, updates Rn+2 and 'B' number of registers starting from Rn+3)
    // B: int (loop variable count and a MSB set when it's an ipairs-like iteration loop)
    // C: block (repeat)
    // D: block (exit)
    FORGLOOP_FALLBACK,

    // Fallback for generic for loop preparation when iterating over builtin pairs/ipairs
    // It raises an error if 'B' register is not a function
    // A: unsigned int (bytecode instruction index)
    // B: Rn
    // C: block (forgloop location)
    FORGPREP_XNEXT_FALLBACK,

    // Increment coverage data (saturating 24 bit add)
    // A: unsigned int (bytecode instruction index)
    COVERAGE,

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
    // A: unsigned int (nups)
    // B: pointer (table)
    // C: unsigned int (protoid)
    NEWCLOSURE,

    // Create closure from a pre-created function object (reusing it unless environments diverge)
    // A: unsigned int (bytecode instruction index)
    // B: Rn (dest)
    // C: Kn (prototype)
    FALLBACK_DUPCLOSURE,

    // Prepare loop variables for a generic for loop, jump to the loop backedge unconditionally
    // A: unsigned int (bytecode instruction index)
    // B: Rn (loop state start, updates Rn Rn+1 Rn+2)
    // C: block
    FALLBACK_FORGPREP,

    // Instruction that passes value through, it is produced by constant folding and users substitute it with the value
    SUBSTITUTE,
    // A: operand of any type

    // Performs bitwise and/xor/or on two unsigned integers
    // A, B: int
    BITAND_UINT,
    BITXOR_UINT,
    BITOR_UINT,

    // Performs bitwise not on an unsigned integer
    // A: int
    BITNOT_UINT,

    // Performs bitwise shift/rotate on an unsigned integer
    // A: int (source)
    // B: int (shift amount)
    BITLSHIFT_UINT,
    BITRSHIFT_UINT,
    BITARSHIFT_UINT,
    BITLROTATE_UINT,
    BITRROTATE_UINT,

    // Returns the number of consecutive zero bits in A starting from the left-most (most significant) bit.
    // A: int
    BITCOUNTLZ_UINT,
    BITCOUNTRZ_UINT,

    // Swap byte order in A
    // A: int
    BYTESWAP_UINT,

    // Calls native libm function with 1 or 2 arguments
    // A: builtin function ID
    // B: double
    // C: double/int (optional, 2nd argument)
    INVOKE_LIBM,

    // Returns the string name of a type based on tag, alternative for type(x)
    // A: tag
    GET_TYPE,

    // Returns the string name of a type either from a __type metatable field or just based on the tag, alternative for typeof(x)
    // A: Rn
    GET_TYPEOF,

    // Find or create an upval at the given level
    // A: Rn (level)
    FINDUPVAL,

    // Read i8 (sign-extended to int) from buffer storage at specified offset
    // A: pointer (buffer)
    // B: int (offset)
    BUFFER_READI8,

    // Read u8 (zero-extended to int) from buffer storage at specified offset
    // A: pointer (buffer)
    // B: int (offset)
    BUFFER_READU8,

    // Write i8/u8 value (int argument is truncated) to buffer storage at specified offset
    // A: pointer (buffer)
    // B: int (offset)
    // C: int (value)
    BUFFER_WRITEI8,

    // Read i16 (sign-extended to int) from buffer storage at specified offset
    // A: pointer (buffer)
    // B: int (offset)
    BUFFER_READI16,

    // Read u16 (zero-extended to int) from buffer storage at specified offset
    // A: pointer (buffer)
    // B: int (offset)
    BUFFER_READU16,

    // Write i16/u16 value (int argument is truncated) to buffer storage at specified offset
    // A: pointer (buffer)
    // B: int (offset)
    // C: int (value)
    BUFFER_WRITEI16,

    // Read i32 value from buffer storage at specified offset
    // A: pointer (buffer)
    // B: int (offset)
    BUFFER_READI32,

    // Write i32/u32 value to buffer storage at specified offset
    // A: pointer (buffer)
    // B: int (offset)
    // C: int (value)
    BUFFER_WRITEI32,

    // Read float value (converted to double) from buffer storage at specified offset
    // A: pointer (buffer)
    // B: int (offset)
    BUFFER_READF32,

    // Write float value (converted from double) to buffer storage at specified offset
    // A: pointer (buffer)
    // B: int (offset)
    // C: double (value)
    BUFFER_WRITEF32,

    // Read double value from buffer storage at specified offset
    // A: pointer (buffer)
    // B: int (offset)
    BUFFER_READF64,

    // Write double value to buffer storage at specified offset
    // A: pointer (buffer)
    // B: int (offset)
    // C: double (value)
    BUFFER_WRITEF64,
};

enum class IrConstKind : uint8_t
{
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

    Undef,

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

    // To reference an exit to VM at specific PC pos
    VmExit,
};

// VmExit uses a special value to indicate that pcpos update should be skipped
// This is only used during type checking at function entry
constexpr uint32_t kVmExitEntryGuardPc = (1u << 28) - 1;

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

    bool operator==(const IrOp& rhs) const
    {
        return kind == rhs.kind && index == rhs.index;
    }

    bool operator!=(const IrOp& rhs) const
    {
        return !(*this == rhs);
    }
};

static_assert(sizeof(IrOp) == 4);

enum class IrValueKind : uint8_t
{
    Unknown, // Used by SUBSTITUTE, argument has to be checked to get type
    None,
    Tag,
    Int,
    Pointer,
    Double,
    Tvalue,
};

struct IrInst
{
    IrCmd cmd;

    // Operands
    IrOp a;
    IrOp b;
    IrOp c;
    IrOp d;
    IrOp e;
    IrOp f;

    uint32_t lastUse = 0;
    uint16_t useCount = 0;

    // Location of the result (optional)
    X64::RegisterX64 regX64 = X64::noreg;
    A64::RegisterA64 regA64 = A64::noreg;
    bool reusedReg = false;
    bool spilled = false;
    bool needsReload = false;
};

// When IrInst operands are used, current instruction index is often required to track lifetime
constexpr uint32_t kInvalidInstIdx = ~0u;

struct IrInstHash
{
    static const uint32_t m = 0x5bd1e995;
    static const int r = 24;

    static uint32_t mix(uint32_t h, uint32_t k)
    {
        // MurmurHash2 step
        k *= m;
        k ^= k >> r;
        k *= m;

        h *= m;
        h ^= k;

        return h;
    }

    static uint32_t mix(uint32_t h, IrOp op)
    {
        static_assert(sizeof(op) == sizeof(uint32_t));
        uint32_t k;
        memcpy(&k, &op, sizeof(op));

        return mix(h, k);
    }

    size_t operator()(const IrInst& key) const
    {
        // MurmurHash2 unrolled
        uint32_t h = 25;

        h = mix(h, uint32_t(key.cmd));
        h = mix(h, key.a);
        h = mix(h, key.b);
        h = mix(h, key.c);
        h = mix(h, key.d);
        h = mix(h, key.e);
        h = mix(h, key.f);

        // MurmurHash2 tail
        h ^= h >> 13;
        h *= m;
        h ^= h >> 15;

        return h;
    }
};

struct IrInstEq
{
    bool operator()(const IrInst& a, const IrInst& b) const
    {
        return a.cmd == b.cmd && a.a == b.a && a.b == b.b && a.c == b.c && a.d == b.d && a.e == b.e && a.f == b.f;
    }
};

enum class IrBlockKind : uint8_t
{
    Bytecode,
    Fallback,
    Internal,
    Linearized,
    Dead,
};

struct IrBlock
{
    IrBlockKind kind;

    uint16_t useCount = 0;

    // 'start' and 'finish' define an inclusive range of instructions which belong to this block inside the function
    // When block has been constructed, 'finish' always points to the first and only terminating instruction
    uint32_t start = ~0u;
    uint32_t finish = ~0u;

    uint32_t sortkey = ~0u;
    uint32_t chainkey = 0;
    uint32_t expectedNextBlock = ~0u;

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
    uint32_t entryBlock = 0;
    uint32_t entryLocation = 0;

    // For each instruction, an operand that can be used to recompute the value
    std::vector<IrOp> valueRestoreOps;
    uint32_t validRestoreOpBlockIdx = 0;

    Proto* proto = nullptr;
    bool variadic = false;

    CfgInfo cfg;

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

    IrInst* asInstOp(IrOp op)
    {
        if (op.kind == IrOpKind::Inst)
            return &instructions[op.index];

        return nullptr;
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

    std::optional<uint8_t> asTagOp(IrOp op)
    {
        if (op.kind != IrOpKind::Constant)
            return std::nullopt;

        IrConst& value = constOp(op);

        if (value.kind != IrConstKind::Tag)
            return std::nullopt;

        return value.valueTag;
    }

    int intOp(IrOp op)
    {
        IrConst& value = constOp(op);

        LUAU_ASSERT(value.kind == IrConstKind::Int);
        return value.valueInt;
    }

    std::optional<int> asIntOp(IrOp op)
    {
        if (op.kind != IrOpKind::Constant)
            return std::nullopt;

        IrConst& value = constOp(op);

        if (value.kind != IrConstKind::Int)
            return std::nullopt;

        return value.valueInt;
    }

    unsigned uintOp(IrOp op)
    {
        IrConst& value = constOp(op);

        LUAU_ASSERT(value.kind == IrConstKind::Uint);
        return value.valueUint;
    }

    std::optional<unsigned> asUintOp(IrOp op)
    {
        if (op.kind != IrOpKind::Constant)
            return std::nullopt;

        IrConst& value = constOp(op);

        if (value.kind != IrConstKind::Uint)
            return std::nullopt;

        return value.valueUint;
    }

    double doubleOp(IrOp op)
    {
        IrConst& value = constOp(op);

        LUAU_ASSERT(value.kind == IrConstKind::Double);
        return value.valueDouble;
    }

    std::optional<double> asDoubleOp(IrOp op)
    {
        if (op.kind != IrOpKind::Constant)
            return std::nullopt;

        IrConst& value = constOp(op);

        if (value.kind != IrConstKind::Double)
            return std::nullopt;

        return value.valueDouble;
    }

    uint32_t getBlockIndex(const IrBlock& block) const
    {
        // Can only be called with blocks from our vector
        LUAU_ASSERT(&block >= blocks.data() && &block <= blocks.data() + blocks.size());
        return uint32_t(&block - blocks.data());
    }

    uint32_t getInstIndex(const IrInst& inst) const
    {
        // Can only be called with instructions from our vector
        LUAU_ASSERT(&inst >= instructions.data() && &inst <= instructions.data() + instructions.size());
        return uint32_t(&inst - instructions.data());
    }

    void recordRestoreOp(uint32_t instIdx, IrOp location)
    {
        if (instIdx >= valueRestoreOps.size())
            valueRestoreOps.resize(instIdx + 1);

        valueRestoreOps[instIdx] = location;
    }

    IrOp findRestoreOp(uint32_t instIdx, bool limitToCurrentBlock) const
    {
        if (instIdx >= valueRestoreOps.size())
            return {};

        const IrBlock& block = blocks[validRestoreOpBlockIdx];

        // When spilled, values can only reference restore operands in the current block
        if (limitToCurrentBlock)
        {
            if (instIdx < block.start || instIdx > block.finish)
                return {};
        }

        return valueRestoreOps[instIdx];
    }

    IrOp findRestoreOp(const IrInst& inst, bool limitToCurrentBlock) const
    {
        return findRestoreOp(getInstIndex(inst), limitToCurrentBlock);
    }
};

inline IrCondition conditionOp(IrOp op)
{
    LUAU_ASSERT(op.kind == IrOpKind::Condition);
    return IrCondition(op.index);
}

inline int vmRegOp(IrOp op)
{
    LUAU_ASSERT(op.kind == IrOpKind::VmReg);
    return op.index;
}

inline int vmConstOp(IrOp op)
{
    LUAU_ASSERT(op.kind == IrOpKind::VmConst);
    return op.index;
}

inline int vmUpvalueOp(IrOp op)
{
    LUAU_ASSERT(op.kind == IrOpKind::VmUpvalue);
    return op.index;
}

inline uint32_t vmExitOp(IrOp op)
{
    LUAU_ASSERT(op.kind == IrOpKind::VmExit);
    return op.index;
}

} // namespace CodeGen
} // namespace Luau
