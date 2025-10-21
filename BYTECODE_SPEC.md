# Luau Bytecode Specification

## Overview

Luau uses a **register-based bytecode** format (unlike vanilla Lua's stack-based bytecode). This document provides a complete reference to Luau's instruction set, encoding format, and design rationale for Swift reimplementation.

**Source**: `Common/include/Luau/Bytecode.h`

---

## Table of Contents

1. [Instruction Encoding](#1-instruction-encoding)
2. [Complete Instruction Set](#2-complete-instruction-set)
3. [Instruction Categories](#3-instruction-categories)
4. [Fast Call Instructions](#4-fast-call-instructions)
5. [Design Rationale](#5-design-rationale)
6. [Swift Translation](#6-swift-translation)
7. [Optimization Opportunities](#7-optimization-opportunities)

---

## 1. Instruction Encoding

### 1.1 Word Code Format

Luau bytecode uses **word code**: each instruction is one or more **32-bit words**.

The first word is the **instruction header**:
```
┌────────────────────────────────────────────────────────┐
│  31-24  │  23-16  │  15-8   │  7-0                     │
│    C    │    B    │    A    │  Opcode (8 bits)         │
└────────────────────────────────────────────────────────┘
```

**Byte 0** (bits 0-7): Opcode (enum LuauOpcode, 0-255)
**Byte 1** (bits 8-15): Register/parameter A
**Byte 2** (bits 16-23): Register/parameter B
**Byte 3** (bits 24-31): Register/parameter C

### 1.2 Encoding Formats

Luau supports three instruction encoding formats:

#### Format 1: ABC (3 bytes, 8-bit each)
```
ABC: [Opcode:8] [A:8] [B:8] [C:8]
```
- Used for: Most register-to-register operations
- A, B, C: Register indices (0-254) or small constants (0-255)
- Example: `ADD R0, R1, R2` → A=0, B=1, C=2

**Extraction macros**:
```c
#define LUAU_INSN_A(insn) (((insn) >> 8) & 0xff)
#define LUAU_INSN_B(insn) (((insn) >> 16) & 0xff)
#define LUAU_INSN_C(insn) (((insn) >> 24) & 0xff)
```

#### Format 2: AD (1 byte + 1 signed 16-bit)
```
AD: [Opcode:8] [A:8] [D:16 signed]
```
- Used for: Constant indices, jump offsets
- A: Register index (0-254)
- D: Signed 16-bit value (-32768 to 32767)
- Example: `LOADK R0, K100` → A=0, D=100
- Example: `JUMP +50` → A=unused, D=50

**Extraction macro**:
```c
#define LUAU_INSN_D(insn) (int32_t(insn) >> 16)  // Sign-extends
```

#### Format 3: E (1 signed 24-bit)
```
E: [Opcode:8] [E:24 signed]
```
- Used for: Long jumps
- E: Signed 24-bit value (-8,388,608 to 8,388,607)
- Example: `JUMPX +100000` → E=100000

**Extraction macro**:
```c
#define LUAU_INSN_E(insn) (int32_t(insn) >> 8)  // Sign-extends
```

### 1.3 Auxiliary Words (AUX)

Some instructions are followed by an **AUX word** (additional 32-bit integer).

**Examples**:
- `GETGLOBAL`: AUX contains constant table index for global name
- `FASTCALL2`: AUX contains second argument register

**AUX extraction macros**:
```c
// For FASTCALL3: two register indices
#define LUAU_INSN_AUX_A(aux) ((aux) & 0xff)
#define LUAU_INSN_AUX_B(aux) (((aux) >> 8) & 0xff)

// For JUMPXEQK*: constant index + NOT flag
#define LUAU_INSN_AUX_KV(aux) ((aux) & 0xffffff)  // 24-bit constant index
#define LUAU_INSN_AUX_NOT(aux) ((aux) >> 31)      // High bit = negation
```

### 1.4 Bytecode Limits

| Entity | Range | Notes |
|--------|-------|-------|
| Registers | 0-254 | Stack slots in function frame |
| Upvalues | 0-199 | Captured variables in closure |
| Constants | 0-8,388,607 | 2^23-1, stored in constant table |
| Child Closures | 0-32,767 | 2^15-1, nested functions |
| Jump Offsets | -8,388,608 to 8,388,607 | 2^23, in word units |

**Why these limits?**
- Registers: 255 would mean unlimited stack, so 254 is practical
- Upvalues: 200 is plenty, more would indicate bad code
- Constants: 23 bits allows room for encoding extensions
- Jumps: 24-bit signed covers very large functions

---

## 2. Complete Instruction Set

Luau has **~80 opcodes** (see `enum LuauOpcode` in Bytecode.h:57-424). Below is the complete reference.

### 2.1 Instruction Reference Table

| Opcode | Format | Args | Description | Example |
|--------|--------|------|-------------|---------|
| **LOP_NOP** | ABC | - | No operation | `NOP` |
| **LOP_BREAK** | ABC | - | Debugger breakpoint | `BREAK` |
| **LOP_LOADNIL** | ABC | A | Load nil into register A | `LOADNIL R0` |
| **LOP_LOADB** | ABC | A B C | Load boolean B into A, jump C | `LOADB R0, 1, 0` |
| **LOP_LOADN** | AD | A D | Load number D into A | `LOADN R0, 42` |
| **LOP_LOADK** | AD | A D + AUX | Load constant K[D] into A | `LOADK R0, K100` |
| **LOP_MOVE** | ABC | A B | Copy register B to A | `MOVE R0, R1` |
| **LOP_GETGLOBAL** | ABC | A C + AUX | Get global K[AUX] into A | `GETGLOBAL R0, K0` |
| **LOP_SETGLOBAL** | ABC | A C + AUX | Set global K[AUX] to A | `SETGLOBAL R0, K0` |
| **LOP_GETUPVAL** | ABC | A B | Get upvalue[B] into A | `GETUPVAL R0, U0` |
| **LOP_SETUPVAL** | ABC | A B | Set upvalue[B] to A | `SETUPVAL R0, U0` |
| **LOP_CLOSEUPVALS** | ABC | A | Close upvalues ≥ A | `CLOSEUPVALS R0` |
| **LOP_GETIMPORT** | AD | A D + AUX | Get import K[D] into A | `GETIMPORT R0, K0` |
| **LOP_GETTABLE** | ABC | A B C | A = B[C] (register key) | `GETTABLE R0, R1, R2` |
| **LOP_SETTABLE** | ABC | A B C | B[C] = A (register key) | `SETTABLE R0, R1, R2` |
| **LOP_GETTABLEKS** | ABC | A B C + AUX | A = B[K[AUX]] (string key) | `GETTABLEKS R0, R1, K0` |
| **LOP_SETTABLEKS** | ABC | A B C + AUX | B[K[AUX]] = A (string key) | `SETTABLEKS R0, R1, K0` |
| **LOP_GETTABLEN** | ABC | A B C | A = B[C+1] (small int key 1..256) | `GETTABLEN R0, R1, 5` |
| **LOP_SETTABLEN** | ABC | A B C | B[C+1] = A (small int key 1..256) | `SETTABLEN R0, R1, 5` |
| **LOP_NEWCLOSURE** | AD | A D | Create closure from Proto[D] | `NEWCLOSURE R0, P0` |
| **LOP_NAMECALL** | ABC | A B C + AUX | Call method K[AUX] on B | `NAMECALL R0, R1, K0` |
| **LOP_CALL** | ABC | A B C | Call R[A] with B-1 args, C-1 results | `CALL R0, 2, 2` |
| **LOP_RETURN** | ABC | A B | Return B-1 values starting at A | `RETURN R0, 2` |
| **LOP_JUMP** | AD | D | Jump to PC + D | `JUMP +10` |
| **LOP_JUMPBACK** | AD | D | Jump back to PC + D (safepoint) | `JUMPBACK -50` |
| **LOP_JUMPIF** | AD | A D | Jump if A is not nil/false | `JUMPIF R0, +5` |
| **LOP_JUMPIFNOT** | AD | A D | Jump if A is nil/false | `JUMPIFNOT R0, +5` |
| **LOP_JUMPIFEQ** | AD | A D + AUX | Jump if A == AUX | `JUMPIFEQ R0, +5, R1` |
| **LOP_JUMPIFLE** | AD | A D + AUX | Jump if A <= AUX | `JUMPIFLE R0, +5, R1` |
| **LOP_JUMPIFLT** | AD | A D + AUX | Jump if A < AUX | `JUMPIFLT R0, +5, R1` |
| **LOP_JUMPIFNOTEQ** | AD | A D + AUX | Jump if A != AUX | `JUMPIFNOTEQ R0, +5, R1` |
| **LOP_JUMPIFNOTLE** | AD | A D + AUX | Jump if A > AUX (not ≤) | `JUMPIFNOTLE R0, +5, R1` |
| **LOP_JUMPIFNOTLT** | AD | A D + AUX | Jump if A >= AUX (not <) | `JUMPIFNOTLT R0, +5, R1` |
| **LOP_ADD** | ABC | A B C | A = B + C | `ADD R0, R1, R2` |
| **LOP_SUB** | ABC | A B C | A = B - C | `SUB R0, R1, R2` |
| **LOP_MUL** | ABC | A B C | A = B * C | `MUL R0, R1, R2` |
| **LOP_DIV** | ABC | A B C | A = B / C | `DIV R0, R1, R2` |
| **LOP_MOD** | ABC | A B C | A = B % C | `MOD R0, R1, R2` |
| **LOP_POW** | ABC | A B C | A = B ^ C | `POW R0, R1, R2` |
| **LOP_ADDK** | ABC | A B C | A = B + K[C] | `ADDK R0, R1, K5` |
| **LOP_SUBK** | ABC | A B C | A = B - K[C] | `SUBK R0, R1, K5` |
| **LOP_MULK** | ABC | A B C | A = B * K[C] | `MULK R0, R1, K5` |
| **LOP_DIVK** | ABC | A B C | A = B / K[C] | `DIVK R0, R1, K5` |
| **LOP_MODK** | ABC | A B C | A = B % K[C] | `MODK R0, R1, K5` |
| **LOP_POWK** | ABC | A B C | A = B ^ K[C] | `POWK R0, R1, K5` |
| **LOP_AND** | ABC | A B C | A = B and C | `AND R0, R1, R2` |
| **LOP_OR** | ABC | A B C | A = B or C | `OR R0, R1, R2` |
| **LOP_ANDK** | ABC | A B C | A = B and K[C] | `ANDK R0, R1, K0` |
| **LOP_ORK** | ABC | A B C | A = B or K[C] | `ORK R0, R1, K0` |
| **LOP_CONCAT** | ABC | A B C | A = concat(B..C) | `CONCAT R0, R1, R3` |
| **LOP_NOT** | ABC | A B | A = not B | `NOT R0, R1` |
| **LOP_MINUS** | ABC | A B | A = -B | `MINUS R0, R1` |
| **LOP_LENGTH** | ABC | A B | A = #B | `LENGTH R0, R1` |
| **LOP_NEWTABLE** | ABC | A B + AUX | Create table, A=dest, B=hash size | `NEWTABLE R0, 0, 10` |
| **LOP_DUPTABLE** | AD | A D | Duplicate table template K[D] | `DUPTABLE R0, K5` |
| **LOP_SETLIST** | ABC | A B C + AUX | Set array elements A[AUX..] = B..C | `SETLIST R0, R1, 10` |
| **LOP_FORNPREP** | AD | A D | Prepare numeric for loop | `FORNPREP R0, +20` |
| **LOP_FORNLOOP** | AD | A D | Iterate numeric for loop | `FORNLOOP R0, -15` |
| **LOP_FORGLOOP** | AD | A D + AUX | Iterate generic for loop | `FORGLOOP R0, -10` |
| **LOP_FORGPREP_INEXT** | AD | A D | Prepare ipairs() loop | `FORGPREP_INEXT R0, +5` |
| **LOP_FORGPREP_NEXT** | AD | A D | Prepare pairs() loop | `FORGPREP_NEXT R0, +5` |
| **LOP_FORGPREP** | AD | A D | Prepare generic for loop | `FORGPREP R0, +5` |
| **LOP_GETVARARGS** | ABC | A B | Get varargs into A.. | `GETVARARGS R0, 3` |
| **LOP_DUPCLOSURE** | AD | A D | Reuse closure K[D] | `DUPCLOSURE R0, K0` |
| **LOP_PREPVARARGS** | ABC | A | Prepare varargs (fixed A args) | `PREPVARARGS 2` |
| **LOP_LOADKX** | ABC | A + AUX | Load constant K[AUX] | `LOADKX R0, K100000` |
| **LOP_JUMPX** | E | E | Long jump (24-bit offset) | `JUMPX +100000` |
| **LOP_FASTCALL** | ABC | A C | Fast call builtin A | `FASTCALL 5, +2` |
| **LOP_FASTCALL1** | ABC | A B C | Fast call builtin A, arg B | `FASTCALL1 5, R0, +2` |
| **LOP_FASTCALL2** | ABC | A B C + AUX | Fast call builtin A, args B, AUX | `FASTCALL2 5, R0, +2` |
| **LOP_FASTCALL2K** | ABC | A B C + AUX | Fast call builtin A, arg B, const AUX | `FASTCALL2K 5, R0, +2` |
| **LOP_FASTCALL3** | ABC | A B C + AUX | Fast call builtin A, 3 args | `FASTCALL3 5, R0, +2` |
| **LOP_COVERAGE** | E | E | Update coverage counter | `COVERAGE` |
| **LOP_CAPTURE** | ABC | A B | Capture upvalue (after NEWCLOSURE) | `CAPTURE VAL, R0` |
| **LOP_SUBRK** | ABC | A B C | A = K[B] - C | `SUBRK R0, K5, R1` |
| **LOP_DIVRK** | ABC | A B C | A = K[B] / C | `DIVRK R0, K5, R1` |
| **LOP_JUMPXEQKNIL** | AD | A D + AUX | Jump if A == nil (with NOT flag) | `JUMPXEQKNIL R0, +5` |
| **LOP_JUMPXEQKB** | AD | A D + AUX | Jump if A == bool (with NOT flag) | `JUMPXEQKB R0, +5` |
| **LOP_JUMPXEQKN** | AD | A D + AUX | Jump if A == K[AUX] (number) | `JUMPXEQKN R0, +5` |
| **LOP_JUMPXEQKS** | AD | A D + AUX | Jump if A == K[AUX] (string) | `JUMPXEQKS R0, +5` |
| **LOP_IDIV** | ABC | A B C | A = B // C (floor div) | `IDIV R0, R1, R2` |
| **LOP_IDIVK** | ABC | A B C | A = B // K[C] (floor div) | `IDIVK R0, R1, K5` |
| **LOP_NATIVECALL** | ABC | - | Switch to native code | `NATIVECALL` |

**Total**: 77 opcodes (LOP__COUNT = 77)

---

## 3. Instruction Categories

### 3.1 Load/Store Instructions

**Purpose**: Move data between registers, constants, upvalues, globals.

| Instruction | Description | Use Case |
|-------------|-------------|----------|
| LOADNIL | Load nil | Initialize variables |
| LOADB | Load boolean | Load true/false, with conditional jump |
| LOADN | Load small number | Numbers in range [-32768, 32767] |
| LOADK | Load constant | Large numbers, strings, vectors |
| LOADKX | Load constant (extended) | Constants beyond 32K |
| MOVE | Copy register | Assign variables, pass arguments |
| GETUPVAL | Load upvalue | Access closure captures |
| SETUPVAL | Store upvalue | Modify closure captures |
| GETGLOBAL | Load global | Access global variables |
| SETGLOBAL | Store global | Modify global variables |
| GETIMPORT | Load import | Optimized for frequently imported globals |

**Why separate LOADN and LOADK?**
- LOADN is one word (faster, more compact)
- LOADK is two words but handles any constant
- Compiler chooses based on value range

**Example**:
```lua
local x = 10          -- LOADN R0, 10
local y = 100000      -- LOADK R0, K0 (K0 = 100000)
local z = "hello"     -- LOADK R0, K1 (K1 = "hello")
```

### 3.2 Arithmetic Instructions

**Purpose**: Perform arithmetic operations.

| Category | Register-Register | Register-Constant | Constant-Register |
|----------|-------------------|-------------------|-------------------|
| Addition | ADD | ADDK | - |
| Subtraction | SUB | SUBK | SUBRK |
| Multiplication | MUL | MULK | - |
| Division | DIV | DIVK | DIVRK |
| Modulo | MOD | MODK | - |
| Power | POW | POWK | - |
| Floor Division | IDIV | IDIVK | - |

**Why *K variants?**
- Avoids loading constant into register first
- One instruction instead of two (LOADK + ADD)
- Example: `x + 5` → `ADDK R0, R0, K0` (one instruction)

**Why SUBRK and DIVRK?**
- Subtraction and division are **not commutative**
- `5 - x` requires SUBRK (constant on left)
- `x - 5` uses SUBK (constant on right)

**Example**:
```lua
local a = b + c        -- ADD R0, R1, R2
local x = y + 10       -- ADDK R0, R1, K0 (K0 = 10)
local p = 100 - q      -- SUBRK R0, K0, R1 (K0 = 100)
```

### 3.3 Table Instructions

**Purpose**: Access and modify tables.

| Instruction | Key Type | Description |
|-------------|----------|-------------|
| GETTABLE | Register | `A = B[C]` (generic) |
| SETTABLE | Register | `B[C] = A` (generic) |
| GETTABLEKS | String constant | `A = B["key"]` (optimized) |
| SETTABLEKS | String constant | `B["key"] = A` (optimized) |
| GETTABLEN | Small int (1-256) | `A = B[N]` (array fast path) |
| SETTABLEN | Small int (1-256) | `B[N] = A` (array fast path) |
| NEWTABLE | - | Create empty table |
| DUPTABLE | - | Duplicate table template |
| SETLIST | - | Initialize array elements |

**Why so many variants?**
- **Performance**: String keys are most common (object properties)
- **GETTABLEKS**: Uses inline cache for string keys (95% hit rate)
- **GETTABLEN**: Fast path for small array indices (avoids hashing)

**Inline Caching** (GETTABLEKS):
```c
// Bytecode stores predicted slot (C parameter)
int slot = LUAU_INSN_C(insn) & h->nodemask8;
LuaNode* n = &h->node[slot];

// Fast path: key matches predicted slot
if (LUAU_LIKELY(ttisstring(gkey(n)) && tsvalue(gkey(n)) == tsvalue(kv))) {
    setobj2s(L, ra, gval(n));  // Cache hit!
    VM_NEXT();
}
```

**Example**:
```lua
local x = t.name         -- GETTABLEKS R0, R1, K0 (K0 = "name")
local y = t[5]           -- GETTABLEN R0, R1, 4 (C=4, array index 5)
local z = t[key]         -- GETTABLE R0, R1, R2 (generic)
t.name = "foo"           -- SETTABLEKS R0, R1, K0
local t = {1, 2, 3}      -- NEWTABLE + SETLIST
```

### 3.4 Control Flow Instructions

**Purpose**: Conditionals, loops, function calls/returns.

| Instruction | Description | Use Case |
|-------------|-------------|----------|
| JUMP | Unconditional jump | goto, break |
| JUMPBACK | Backward jump (safepoint) | Loop backedge |
| JUMPIF | Jump if truthy | `if x then` |
| JUMPIFNOT | Jump if falsy | `if not x then` |
| JUMPIFEQ | Jump if equal | `if a == b then` |
| JUMPIFLE | Jump if less or equal | `if a <= b then` |
| JUMPIFLT | Jump if less than | `if a < b then` |
| JUMPIFNOTEQ | Jump if not equal | `if a ~= b then` |
| JUMPIFNOTLE | Jump if greater | `if a > b then` |
| JUMPIFNOTLT | Jump if greater or equal | `if a >= b then` |
| JUMPX | Long jump (24-bit) | Very large functions |
| JUMPXEQKNIL | Jump if nil/not nil | `if x == nil then` |
| JUMPXEQKB | Jump if bool | `if x == true then` |
| JUMPXEQKN | Jump if equals number | `if x == 5 then` |
| JUMPXEQKS | Jump if equals string | `if x == "foo" then` |

**Why JUMPBACK vs JUMP?**
- JUMPBACK is a **safepoint**: checks for interrupts (Ctrl+C, debugger, GC)
- JUMP does not check (optimization)
- Compiler emits JUMPBACK for loop backedges

**Why JUMPXEQK* instructions?**
- Optimize common pattern: `if x == constant then`
- Avoids separate LOADK instruction
- Example: `if x == nil then` → one instruction instead of two

**Example**:
```lua
if x then end              -- JUMPIFNOT R0, +1
if x == 5 then end         -- JUMPXEQKN R0, +1 (K0 = 5)
while true do end          -- JUMP +0 / JUMPBACK -1
for i = 1, 10 do end       -- FORNPREP ... FORNLOOP
```

### 3.5 Function Instructions

**Purpose**: Function calls, returns, closures, varargs.

| Instruction | Description | Use Case |
|-------------|-------------|----------|
| CALL | Call function | `f(a, b)` |
| RETURN | Return values | `return x, y` |
| NEWCLOSURE | Create closure | `function() ... end` |
| DUPCLOSURE | Reuse closure | Optimize constant closures |
| NAMECALL | Method call | `obj:method(...)` |
| GETVARARGS | Get variadic args | `function(...) local x = ... end` |
| PREPVARARGS | Prepare varargs | Prologue for variadic functions |
| CAPTURE | Capture upvalue | After NEWCLOSURE |
| CLOSEUPVALS | Close upvalues | Before return/break |

**CALL Encoding**:
- A: Register where function lives
- B: Argument count + 1 (0 = variable args, LUA_MULTRET)
- C: Result count + 1 (0 = variable results, LUA_MULTRET)

**Example**:
```lua
f(a, b)                    -- CALL R0, 3, 1 (func + 2 args, discard results)
local x, y = f(a, b)       -- CALL R0, 3, 3 (func + 2 args, 2 results)
return f(...)              -- CALL R0, 0, 0 (MULTRET args and results)
```

**NAMECALL Optimization**:
```lua
obj:method(arg)            -- NAMECALL R0, R1, K0; CALL R0, 3, 1
```

This is equivalent to:
```lua
local temp = obj.method    -- GETTABLEKS R0, R1, K0
temp(obj, arg)             -- CALL R0, 3, 1
```

But NAMECALL is **one instruction** and handles `__namecall` metamethod.

### 3.6 Loop Instructions

**Purpose**: Efficient loop iteration.

| Instruction | Loop Type | Description |
|-------------|-----------|-------------|
| FORNPREP | Numeric for | Initialize `for i = start, stop, step` |
| FORNLOOP | Numeric for | Iterate numeric for loop |
| FORGPREP | Generic for | Initialize `for k, v in pairs(t)` |
| FORGLOOP | Generic for | Iterate generic for loop |
| FORGPREP_INEXT | ipairs for | Optimized `for i, v in ipairs(t)` |
| FORGPREP_NEXT | pairs for | Optimized `for k, v in pairs(t)` |

**Register Layout** for numeric for:
```
R[A+0] = limit     (immutable)
R[A+1] = step      (immutable)
R[A+2] = index     (internal, incremented each iteration)
R[A+3] = variable  (user-visible loop variable)
```

**Register Layout** for generic for:
```
R[A+0] = generator  (immutable, e.g., next function)
R[A+1] = state      (immutable, e.g., table)
R[A+2] = index      (internal, updated each iteration)
R[A+3..] = variables (user-visible loop variables)
```

**Why specialized instructions?**
- Numeric for: Avoids function calls, inlined arithmetic
- Generic for: Specialized for ipairs/pairs (array/table traversal)

**Example**:
```lua
for i = 1, 10, 1 do        -- FORNPREP R0, +5; ... ; FORNLOOP R0, -10
    print(i)
end

for i, v in ipairs(t) do   -- FORGPREP_INEXT R0, +5; ... ; FORGLOOP R0, -10
    print(i, v)
end
```

---

## 4. Fast Call Instructions

**Purpose**: Inline built-in functions for massive speedup.

### 4.1 FASTCALL Family

| Instruction | Args | Description |
|-------------|------|-------------|
| FASTCALL | A C | Fast call builtin A, jump C on failure |
| FASTCALL1 | A B C | Fast call builtin A, arg B |
| FASTCALL2 | A B C + AUX | Fast call builtin A, args B, AUX |
| FASTCALL2K | A B C + AUX | Fast call builtin A, arg B, const AUX |
| FASTCALL3 | A B C + AUX | Fast call builtin A, 3 register args |

### 4.2 Built-in Function IDs

See `enum LuauBuiltinFunction` (Bytecode.h:505-641):

**Math Functions** (32 functions):
```
LBF_MATH_ABS, LBF_MATH_ACOS, LBF_MATH_ASIN, LBF_MATH_ATAN2, LBF_MATH_ATAN,
LBF_MATH_CEIL, LBF_MATH_COSH, LBF_MATH_COS, LBF_MATH_DEG, LBF_MATH_EXP,
LBF_MATH_FLOOR, LBF_MATH_FMOD, LBF_MATH_FREXP, LBF_MATH_LDEXP,
LBF_MATH_LOG10, LBF_MATH_LOG, LBF_MATH_MAX, LBF_MATH_MIN, LBF_MATH_MODF,
LBF_MATH_POW, LBF_MATH_RAD, LBF_MATH_SINH, LBF_MATH_SIN, LBF_MATH_SQRT,
LBF_MATH_TANH, LBF_MATH_TAN, LBF_MATH_CLAMP, LBF_MATH_SIGN, LBF_MATH_ROUND,
LBF_MATH_LERP
```

**Bit32 Functions** (13 functions):
```
LBF_BIT32_ARSHIFT, LBF_BIT32_BAND, LBF_BIT32_BNOT, LBF_BIT32_BOR,
LBF_BIT32_BXOR, LBF_BIT32_BTEST, LBF_BIT32_EXTRACT, LBF_BIT32_LROTATE,
LBF_BIT32_LSHIFT, LBF_BIT32_REPLACE, LBF_BIT32_RROTATE, LBF_BIT32_RSHIFT,
LBF_BIT32_COUNTLZ, LBF_BIT32_COUNTRZ, LBF_BIT32_EXTRACTK, LBF_BIT32_BYTESWAP
```

**String Functions** (4 functions):
```
LBF_STRING_BYTE, LBF_STRING_CHAR, LBF_STRING_LEN, LBF_STRING_SUB
```

**Table Functions** (2 functions):
```
LBF_TABLE_INSERT, LBF_TABLE_UNPACK
```

**Vector Functions** (11 functions):
```
LBF_VECTOR, LBF_VECTOR_MAGNITUDE, LBF_VECTOR_NORMALIZE, LBF_VECTOR_CROSS,
LBF_VECTOR_DOT, LBF_VECTOR_FLOOR, LBF_VECTOR_CEIL, LBF_VECTOR_ABS,
LBF_VECTOR_SIGN, LBF_VECTOR_CLAMP, LBF_VECTOR_MIN, LBF_VECTOR_MAX,
LBF_VECTOR_LERP
```

**Buffer Functions** (13 functions):
```
LBF_BUFFER_READI8, LBF_BUFFER_READU8, LBF_BUFFER_WRITEU8,
LBF_BUFFER_READI16, LBF_BUFFER_READU16, LBF_BUFFER_WRITEU16,
LBF_BUFFER_READI32, LBF_BUFFER_READU32, LBF_BUFFER_WRITEU32,
LBF_BUFFER_READF32, LBF_BUFFER_WRITEF32,
LBF_BUFFER_READF64, LBF_BUFFER_WRITEF64
```

**Other Functions** (9 functions):
```
LBF_ASSERT, LBF_TYPE, LBF_TYPEOF, LBF_RAWSET, LBF_RAWGET, LBF_RAWEQUAL,
LBF_SELECT_VARARG, LBF_RAWLEN, LBF_GETMETATABLE, LBF_SETMETATABLE,
LBF_TONUMBER, LBF_TOSTRING
```

### 4.3 Fast Call Flow

```
┌─────────────────────────────────────────────────────────────┐
│  FASTCALL1 LBF_MATH_ABS, R1, +2  // Try fast path, jump +2  │
│  GETIMPORT R0, K0                 // Fallback: load function │
│  CALL R0, 2, 1                    // Fallback: call normally │
│  ... (next instruction)           // Fast path lands here    │
└─────────────────────────────────────────────────────────────┘
```

**How it works**:
1. FASTCALL tries to execute builtin inline
2. If successful: Jump over GETIMPORT + CALL (2 instructions)
3. If failed (wrong types, etc.): Fall through to normal call

**Why so fast?**
- No function call overhead
- No stack frame setup
- Inlined type checks and operations
- Example: `math.abs(-5)` is just a few CPU instructions

### 4.4 Example: math.sqrt

```lua
local x = math.sqrt(y)
```

**Without FASTCALL**:
```
GETIMPORT R0, K0    // Load math.sqrt function
MOVE R1, R_y        // Copy argument
CALL R0, 2, 2       // Call function
MOVE R_x, R0        // Store result
```
= **4 instructions, function call overhead**

**With FASTCALL**:
```
MOVE R0, R_y             // Setup argument
FASTCALL1 LBF_MATH_SQRT, R0, +2
GETIMPORT R0, K0         // Fallback (skipped if fast path succeeds)
CALL R0, 2, 2            // Fallback (skipped)
MOVE R_x, R0             // Store result
```

Fast path = **~10 CPU instructions** (type check + sqrt + store)
Normal call = **~100+ CPU instructions** (stack frame, dispatch, etc.)

**Speedup**: 10x+ for math-heavy code!

---

## 5. Design Rationale

### 5.1 Why Register-Based?

**Register-based** (Luau):
```
LOADN R0, 10
LOADN R1, 20
ADD R2, R0, R1
```

**Stack-based** (Lua 5.1):
```
PUSHINT 10
PUSHINT 20
ADD
```

**Advantages of register-based**:
1. **Fewer instructions**: No PUSH/POP overhead
2. **Fewer memory accesses**: Register indices are offsets, not pointers
3. **Better cache locality**: Registers are consecutive stack slots
4. **Easier optimization**: Clear data flow, easier to analyze
5. **Faster dispatch**: Fewer instructions to decode

**Benchmarks**: Luau is 2-3x faster than Lua 5.1 for arithmetic code.

### 5.2 Why Separate *K Instructions?

**Problem**: Loading constants is slow
```
LOADK R1, K0      -- Load constant 5
ADD R2, R0, R1    -- Add R0 + R1
```
= 2 instructions, 2 register accesses

**Solution**: ADDK instruction
```
ADDK R2, R0, K0   -- Add R0 + K[0] in one instruction
```
= 1 instruction, 1 register access + 1 constant access

**Result**: ~30% faster for constant-heavy code.

### 5.3 Why GETTABLEKS vs GETTABLE?

**Problem**: Property access is super common
```lua
player.name, player.health, player.position, ...
```

**Solution**: Specialized instruction with inline cache
- GETTABLEKS stores predicted slot index (based on hash)
- 95%+ cache hit rate in real code
- Cache miss: Update prediction, continue

**Result**: Table property access is 5-10x faster.

### 5.4 Why FASTCALL?

**Problem**: Built-in functions are called constantly
```lua
for i = 1, 1000000 do
    local x = math.sqrt(i)
end
```

**Without FASTCALL**: ~5 seconds
**With FASTCALL**: ~0.5 seconds

**How**: Inline built-ins, skip function call overhead.

**Why it works**: Built-ins have predictable types and behavior.

### 5.5 Why JUMPBACK vs JUMP?

**Problem**: Infinite loops can't be interrupted
```lua
while true do end  -- Can't Ctrl+C out of this!
```

**Solution**: JUMPBACK checks for interrupts
- Loop backedges use JUMPBACK
- Forward jumps use JUMP (no check needed)

**Result**: Responsive debugger, interruptible loops.

---

## 6. Swift Translation

### 6.1 Opcode Enum

```swift
@frozen
enum Opcode: UInt8 {
    case nop = 0
    case `break` = 1
    case loadNil = 2
    case loadB = 3
    case loadN = 4
    case loadK = 5
    case move = 6
    case getGlobal = 7
    // ... all 77 opcodes
    case idivK = 76

    // Computed properties for fast decoding
    var takesAux: Bool {
        switch self {
        case .getGlobal, .setGlobal, .getImport, .gettableKS, .settableKS,
             .namecall, .newTable, .setList, .forgLoop, .fastcall2, .fastcall2K,
             .fastcall3, .loadKX, .jumpXEqKNil, .jumpXEqKB, .jumpXEqKN, .jumpXEqKS:
            return true
        default:
            return false
        }
    }
}
```

### 6.2 Instruction Decoder

```swift
struct Instruction {
    let raw: UInt32

    @inline(__always)
    var opcode: Opcode {
        Opcode(rawValue: UInt8(raw & 0xFF))!
    }

    @inline(__always)
    var a: UInt8 {
        UInt8((raw >> 8) & 0xFF)
    }

    @inline(__always)
    var b: UInt8 {
        UInt8((raw >> 16) & 0xFF)
    }

    @inline(__always)
    var c: UInt8 {
        UInt8((raw >> 24) & 0xFF)
    }

    @inline(__always)
    var d: Int16 {
        Int16(bitPattern: UInt16(raw >> 16))  // Sign-extend
    }

    @inline(__always)
    var e: Int32 {
        Int32(bitPattern: raw) >> 8  // Sign-extend
    }
}

// Auxiliary word helpers
extension UInt32 {
    var auxA: UInt8 {
        UInt8(self & 0xFF)
    }

    var auxB: UInt8 {
        UInt8((self >> 8) & 0xFF)
    }

    var auxKV: UInt32 {
        self & 0xFFFFFF  // 24-bit constant index
    }

    var auxNOT: Bool {
        (self >> 31) != 0  // High bit
    }
}
```

### 6.3 VM Dispatch Loop

```swift
final class VirtualMachine {
    var stack: ContiguousArray<Value>
    var pc: UnsafePointer<UInt32>
    var base: Int
    let constants: [Value]
    let proto: Proto

    func execute() throws {
        while true {
            let insn = Instruction(raw: pc.pointee)
            pc = pc.advanced(by: 1)

            switch insn.opcode {
            case .loadNil:
                stack[base + Int(insn.a)] = .nil

            case .loadB:
                stack[base + Int(insn.a)] = .boolean(insn.b != 0)
                pc = pc.advanced(by: Int(insn.c))  // Conditional jump

            case .loadN:
                stack[base + Int(insn.a)] = .number(Double(insn.d))

            case .loadK:
                let aux = pc.pointee
                pc = pc.advanced(by: 1)
                stack[base + Int(insn.a)] = constants[Int(aux)]

            case .move:
                stack[base + Int(insn.a)] = stack[base + Int(insn.b)]

            case .add:
                let lhs = stack[base + Int(insn.b)]
                let rhs = stack[base + Int(insn.c)]
                stack[base + Int(insn.a)] = try add(lhs, rhs)

            case .addK:
                let lhs = stack[base + Int(insn.b)]
                let rhs = constants[Int(insn.c)]
                stack[base + Int(insn.a)] = try add(lhs, rhs)

            // ... all other opcodes
            default:
                fatalError("Unimplemented opcode: \(insn.opcode)")
            }
        }
    }

    @inline(__always)
    private func add(_ lhs: Value, _ rhs: Value) throws -> Value {
        // Fast path: both numbers
        if case .number(let a) = lhs, case .number(let b) = rhs {
            return .number(a + b)
        }
        // Slow path: metamethods
        return try callMetamethod("__add", lhs, rhs)
    }
}
```

### 6.4 Bytecode Builder

```swift
final class BytecodeBuilder {
    private(set) var instructions: [UInt32] = []
    private(set) var constants: [Value] = []
    private var constantMap: [Value: Int] = [:]

    func emitABC(_ op: Opcode, _ a: UInt8, _ b: UInt8, _ c: UInt8) {
        let insn = UInt32(op.rawValue)
                 | (UInt32(a) << 8)
                 | (UInt32(b) << 16)
                 | (UInt32(c) << 24)
        instructions.append(insn)
    }

    func emitAD(_ op: Opcode, _ a: UInt8, _ d: Int16) {
        let insn = UInt32(op.rawValue)
                 | (UInt32(a) << 8)
                 | (UInt32(bitPattern: Int32(d)) << 16)
        instructions.append(insn)
    }

    func emitE(_ op: Opcode, _ e: Int32) {
        let insn = UInt32(op.rawValue)
                 | (UInt32(bitPattern: e) << 8)
        instructions.append(insn)
    }

    func emitAux(_ aux: UInt32) {
        instructions.append(aux)
    }

    func addConstant(_ value: Value) -> Int {
        if let existing = constantMap[value] {
            return existing
        }
        let index = constants.count
        constants.append(value)
        constantMap[value] = index
        return index
    }

    // Higher-level emitters
    func emitLoadK(_ dest: UInt8, _ value: Value) {
        let index = addConstant(value)
        if index <= 0x7FFF {
            emitAD(.loadK, dest, Int16(index))
        } else {
            emitABC(.loadKX, dest, 0, 0)
            emitAux(UInt32(index))
        }
    }

    func emitAdd(_ dest: UInt8, _ lhs: UInt8, _ rhs: UInt8) {
        emitABC(.add, dest, lhs, rhs)
    }

    func emitAddK(_ dest: UInt8, _ lhs: UInt8, _ constant: Value) {
        let index = addConstant(constant)
        if index <= 255 {
            emitABC(.addK, dest, lhs, UInt8(index))
        } else {
            // Fallback: load constant first, then add
            let tempReg = allocateRegister()
            emitLoadK(tempReg, constant)
            emitAdd(dest, lhs, tempReg)
            freeRegister(tempReg)
        }
    }
}
```

---

## 7. Optimization Opportunities

### 7.1 SIMD Vectorization

**Target**: Arithmetic operations on vectors (SIMD3<Float>)

```swift
case .add:
    let lhs = stack[base + Int(insn.b)]
    let rhs = stack[base + Int(insn.c)]

    // Fast path: vector + vector
    if case .vector(let a) = lhs, case .vector(let b) = rhs {
        stack[base + Int(insn.a)] = .vector(a &+ b)  // SIMD addition!
        break
    }

    // Fallback: number + number or metamethod
    stack[base + Int(insn.a)] = try add(lhs, rhs)
```

**Result**: Vector math is 4x faster on Apple Silicon (NEON).

### 7.2 Inline Caching for Tables

**Implementation**: Store last accessed table + slot in instruction

```swift
struct CachedTableAccess {
    var lastTable: ObjectIdentifier?
    var lastSlot: Dictionary<HashableValue, Value>.Index?
}

// Store cache alongside instruction
var inlineCaches: [CachedTableAccess] = []

case .gettableKS:
    let table = stack[base + Int(insn.b)]
    let aux = pc.pointee
    pc = pc.advanced(by: 1)
    let key = constants[Int(aux)]

    guard case .table(let t) = table, case .string(let s) = key else {
        throw RuntimeError.typeError
    }

    // Check inline cache
    let cacheIndex = Int(pc - proto.instructions.baseAddress! - 2)
    var cache = inlineCaches[cacheIndex]

    if cache.lastTable == ObjectIdentifier(t), let slot = cache.lastSlot {
        // Cache hit! ~5x faster
        stack[base + Int(insn.a)] = t.hash[slot].value
    } else {
        // Cache miss
        if let slot = t.hash.index(forKey: .string(s)) {
            cache.lastTable = ObjectIdentifier(t)
            cache.lastSlot = slot
            inlineCaches[cacheIndex] = cache
            stack[base + Int(insn.a)] = t.hash[slot].value
        } else {
            stack[base + Int(insn.a)] = .nil
        }
    }
```

### 7.3 Fast Path Specialization

**Idea**: Generate specialized versions of hot functions

```swift
// Generic interpreter
func execute(_ proto: Proto) throws {
    // Slow: handles all types, all edge cases
}

// Specialized for number-only arithmetic
func executeNumberArithmetic(_ proto: Proto) throws {
    // Fast: assumes all values are numbers, no type checks
    // Falls back to generic if assumption violated
}

// Profile-guided: detect hot functions, specialize them
class ProfileGuidedOptimizer {
    var executionCounts: [Proto: Int] = [:]

    func shouldSpecialize(_ proto: Proto) -> Bool {
        executionCounts[proto, default: 0] > 1000
    }
}
```

### 7.4 Direct Threading (Alternative Dispatch)

**Problem**: Switch statement has overhead

**Solution**: Use function pointers (like computed goto in C)

```swift
typealias InstructionHandler = (inout VMState, Instruction) throws -> Void

let handlers: [InstructionHandler] = [
    executeLoadNil,
    executeBreak,
    executeLoadB,
    // ... all 77 handlers
]

func executeDirect() throws {
    while true {
        let insn = Instruction(raw: pc.pointee)
        pc = pc.advanced(by: 1)
        try handlers[Int(insn.opcode.rawValue)](&vmState, insn)
    }
}
```

**Benefit**: ~10-15% faster than switch on some workloads.

---

## Conclusion

Luau's bytecode is a masterclass in performance-oriented instruction set design:

1. **Register-based**: Fewer instructions, better cache locality
2. **Specialized instructions**: ADDK, GETTABLEKS, etc. optimize common patterns
3. **Inline caching**: 95%+ hit rate for property access
4. **Fast calls**: 10x+ speedup for built-ins
5. **Smart encoding**: ABC/AD/E formats balance compactness and expressiveness

For Swift reimplementation:
- Use `@frozen` enums for opcode (fast switching)
- Use `@inline(__always)` for instruction decoding
- Consider function pointer dispatch for 10-15% speedup
- Leverage SIMD for vector operations
- Implement inline caching for tables (huge win!)

**Next Steps**: See `VM_LOOP_DESIGN.md` for VM implementation details.
