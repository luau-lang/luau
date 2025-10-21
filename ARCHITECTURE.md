# Luau Architecture Analysis for Swift 6.2 Reimplementation

## Executive Summary

Luau is a high-performance scripting language derived from Lua 5.1, featuring a sophisticated multi-stage execution pipeline: **Source → AST → Bytecode → Interpretation/Native Execution**. This document maps Luau's architecture for reimplementation in Swift 6.2 with Apple Silicon optimizations.

**Key Design Philosophy**: Luau prioritizes performance through:
- Register-based bytecode VM (not stack-based)
- Incremental garbage collection with write barriers
- Aggressive inline caching and fast paths
- Optional native code generation (JIT)
- Gradual type system for static analysis

---

## 1. System Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                     Luau Architecture                        │
└─────────────────────────────────────────────────────────────┘

Source Code (.lua)
      │
      ▼
┌─────────────┐
│   LEXER     │──→ Token Stream
└─────────────┘
      │
      ▼
┌─────────────┐
│   PARSER    │──→ Abstract Syntax Tree (AST)
└─────────────┘
      │
      ▼
┌─────────────────────────────────────────────────────────────┐
│              TYPE ANALYSIS (Optional)                        │
│  • Type inference (constraint-based)                         │
│  • Type checking                                             │
│  • Linting                                                   │
└─────────────────────────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────────────────────────┐
│              BYTECODE COMPILER                               │
│  • AST → Bytecode translation                                │
│  • Register allocation                                       │
│  • Constant folding                                          │
│  • Dead code elimination                                     │
└─────────────────────────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────────────────────────┐
│              BYTECODE (Proto objects)                        │
│  • Instruction stream                                        │
│  • Constant table                                            │
│  • Debug info                                                │
└─────────────────────────────────────────────────────────────┘
      │
      ├──────────────────┬──────────────────┐
      ▼                  ▼                  ▼
┌──────────┐      ┌──────────┐      ┌──────────────┐
│    VM    │      │ CODEGEN  │      │  NATIVE JIT  │
│ (Interp) │      │ (ARM64)  │      │  (Optional)  │
└──────────┘      └──────────┘      └──────────────┘
      │                  │                  │
      └──────────────────┴──────────────────┘
                         │
                         ▼
              ┌──────────────────────┐
              │   RUNTIME SYSTEM     │
              │  • Garbage Collector │
              │  • Tables            │
              │  • Strings           │
              │  • Closures          │
              └──────────────────────┘
```

---

## 2. Major Subsystems

### 2.1 Frontend: Lexer & Parser

**Location**: `Ast/src/`

**Primary Files**:
- `Lexer.cpp` (32KB) - Tokenization with confusable detection
- `Parser.cpp` (153KB) - Recursive descent parser, produces AST
- `Ast.cpp` (32KB) - AST node definitions

**Responsibilities**:
- Tokenize source code (including string literals, numbers, keywords)
- Parse into AST with position tracking for error reporting
- Handle UTF-8 and detect confusable characters (security feature)
- Support type annotations (if enabled)

**Key Design Decisions**:
- **Single-pass parsing**: No separate preprocessing
- **Recursive descent**: Easy to understand and maintain
- **Position tracking**: Every node knows source location for errors
- **Confusables detection**: Prevents homoglyph attacks (e.g., Cyrillic 'а' vs Latin 'a')

**Performance Considerations**:
- Parser is not on the hot path (compilation happens once)
- Focus on error quality over parsing speed
- Memory allocations use arena allocator

**Swift Translation Strategy**:
```swift
// Use Swift's strong enum types for AST
enum AstExpr {
    case literal(Literal)
    case variable(Name, location: SourceLocation)
    case binOp(op: BinaryOp, lhs: AstExpr, rhs: AstExpr, location: SourceLocation)
    case call(func: AstExpr, args: [AstExpr], location: SourceLocation)
    // ... more cases
}

// Leverage Swift's error handling
enum ParseError: Error {
    case unexpectedToken(expected: Token, got: Token, location: SourceLocation)
    case syntaxError(message: String, location: SourceLocation)
}
```

---

### 2.2 Analysis: Type System

**Location**: `Analysis/src/`

**Primary Files** (Large and Complex):
- `TypeInfer.cpp` (233KB) - Main type inference engine
- `ConstraintGenerator.cpp` (171KB) - Generates type constraints
- `ConstraintSolver.cpp` (134KB) - Solves constraint system
- `Subtyping.cpp` (133KB) - Subtype checking
- `Normalize.cpp` (114KB) - Type normalization
- `TypeChecker2.cpp` (137KB) - Second pass type checker

**Responsibilities**:
- Infer types from untyped code (Hindley-Milner-style with extensions)
- Check type annotations
- Provide autocomplete suggestions
- Catch type errors before runtime
- Support union/intersection types, generics, type functions

**Key Design Decisions**:
- **Gradual typing**: Types are optional, code works without them
- **Constraint-based inference**: Generates constraints, then solves
- **Two-pass checking**: First pass (TypeInfer), second pass (TypeChecker2) for validation
- **Subtyping with structural types**: Tables are structurally typed

**Why This Exists**:
- Roblox needs better IDE support (autocomplete, error detection)
- Catch bugs before runtime in large codebases
- Enable better optimization hints for compiler

**Swift Translation Strategy**:
```swift
// Swift's type system is different, but we can model Luau's runtime types
enum LuauType {
    case nil
    case boolean
    case number
    case string
    case function(params: [LuauType], returns: [LuauType])
    case table(shape: TableShape)
    case union(types: Set<LuauType>)
    case intersection(types: Set<LuauType>)
    case generic(name: String)
    case any
}

// For MVP, we can skip type inference entirely
// Focus on runtime execution first, add types later
```

**Recommendation for MVP**: Skip this subsystem initially. Type inference is complex and not needed for execution.

---

### 2.3 Compiler: Bytecode Generation

**Location**: `Compiler/src/`

**Primary Files**:
- `Compiler.cpp` (161KB) - Main compiler, AST → Bytecode
- `BytecodeBuilder.cpp` (76KB) - Bytecode emission and serialization
- `ConstantFolding.cpp` (26KB) - Compile-time constant evaluation
- `Builtins.cpp` (16KB) - Built-in function handling
- `CostModel.cpp` (13KB) - Optimization cost heuristics

**Responsibilities**:
- Translate AST to register-based bytecode
- Allocate registers (locals, temporaries)
- Emit efficient bytecode sequences
- Constant folding (e.g., `2 + 3` → `5`)
- Dead code elimination
- Generate constant table and debug info

**Key Design Decisions**:
- **Register-based VM**: Not stack-based like vanilla Lua
  - Why? Fewer instructions, better cache locality, easier to optimize
  - Registers map to stack slots in the VM
- **Single-pass compilation**: AST → Bytecode in one pass (mostly)
- **Unlimited registers**: Uses stack slots, not fixed register count
- **Constant table**: Shared constants (strings, numbers) stored separately

**Compilation Pipeline**:
1. **AST Traversal**: Walk tree, emit bytecode for each node
2. **Register Allocation**: Assign registers to locals/temps (linear scan)
3. **Constant Emission**: Build constant table
4. **Jump Patching**: Fix forward jump targets
5. **Debug Info Generation**: Line numbers, variable names

**Example Compilation**:
```lua
local x = 10
local y = x + 5
return y
```

Compiles to:
```
LOADN R0, 10      -- R0 = x
ADDK R1, R0, K0   -- R1 = R0 + K0 (K0 = 5)
RETURN R1, 1      -- return R1
```

**Swift Translation Strategy**:
```swift
// Bytecode builder with register allocation
class BytecodeCompiler {
    var instructions: [UInt32] = []
    var constants: [Value] = []
    var registerStack: RegisterStack

    func compile(_ ast: AstExpr) throws {
        switch ast {
        case .literal(let lit):
            let reg = registerStack.allocate()
            emitLoadConstant(reg, lit)
        case .binOp(let op, let lhs, let rhs, _):
            let lhsReg = try compile(lhs)
            let rhsReg = try compile(rhs)
            let destReg = registerStack.allocate()
            emitArithmetic(op, destReg, lhsReg, rhsReg)
        // ...
        }
    }
}
```

---

### 2.4 Virtual Machine: Bytecode Interpreter

**Location**: `VM/src/`

**Primary Files**:
- `lvmexecute.cpp` (125KB) - **Main VM loop** ⭐
- `ldo.cpp` (25KB) - Execution stack management, function calls
- `lvm.h` (2KB) - VM interface
- `lobject.h` (12KB) - Value representation (TValue)
- `lstate.h` (11KB) - VM state structures

**Responsibilities**:
- Execute bytecode instructions
- Manage execution stack (function call frames)
- Handle function calls (Lua → Lua, Lua → C, C → Lua)
- Perform arithmetic, table operations, etc.
- Integrate with GC (write barriers)

**Key Design Decisions**:
- **Computed goto dispatch** (GCC/Clang): Uses `goto *label` for fast dispatch
  - Why? Eliminates switch statement overhead, better branch prediction
  - Fallback: Traditional switch statement for other compilers
- **Register-based**: Instructions reference registers (stack slots) directly
- **Inline caching**: Cache table slot positions for fast property access
- **Fast paths**: Optimized code paths for common cases (e.g., table[string])

**VM Loop Structure** (simplified):
```c++
static void luau_execute(lua_State* L) {
    #if VM_USE_CGOTO
        static const void* kDispatchTable[256] = { /* labels */ };
    #endif

    Closure* cl = clvalue(L->ci->func);
    StkId base = L->base;
    TValue* k = cl->l.p->k;  // constant table
    const Instruction* pc = L->ci->savedpc;

    VM_NEXT();  // Start dispatch

    VM_CASE(LOP_LOADNIL) {
        Instruction insn = *pc++;
        StkId ra = &base[LUAU_INSN_A(insn)];
        setnilvalue(ra);
        VM_NEXT();
    }

    VM_CASE(LOP_ADD) {
        Instruction insn = *pc++;
        StkId ra = &base[LUAU_INSN_A(insn)];
        StkId rb = &base[LUAU_INSN_B(insn)];
        StkId rc = &base[LUAU_INSN_C(insn)];

        if (ttisnumber(rb) && ttisnumber(rc)) {
            setnvalue(ra, nvalue(rb) + nvalue(rc));
            VM_NEXT();
        }
        // Slow path: metamethods, type coercion...
    }

    // ... 80+ more instruction handlers
}
```

**Performance Techniques**:
1. **Inline caching**: Cache hash table slot indices
2. **Predicted slot indices**: Store likely slot in instruction
3. **Fast paths**: Separate code for common types (number + number)
4. **Write barriers**: Only when needed, integrated into setobj macros
5. **Hot variables in registers**: pc, base, cl, k kept in CPU registers

**Swift Translation Strategy**:
```swift
// Swift doesn't have computed goto, but we can optimize differently
final class VirtualMachine {
    var stack: ContiguousArray<Value>  // Use ContiguousArray for cache locality
    var pc: UnsafePointer<UInt32>
    var base: Int

    @inline(__always)
    func execute(_ proto: Proto) throws {
        while true {
            let insn = pc.pointee
            pc = pc.advanced(by: 1)

            let op = Opcode(rawValue: UInt8(insn & 0xFF))!

            // Swift's switch is pretty fast with frozen enums
            switch op {
            case .loadNil:
                let a = Int((insn >> 8) & 0xFF)
                stack[base + a] = .nil

            case .add:
                let a = Int((insn >> 8) & 0xFF)
                let b = Int((insn >> 16) & 0xFF)
                let c = Int((insn >> 24) & 0xFF)
                // Fast path with inline check
                if case .number(let lhs) = stack[base + b],
                   case .number(let rhs) = stack[base + c] {
                    stack[base + a] = .number(lhs + rhs)
                } else {
                    try slowPathArithmetic(.add, a, b, c)
                }
            // ... more cases
            }
        }
    }
}

// Alternative: Use function pointer table (like C computed goto)
typealias InstructionHandler = (inout VMState) -> Void
let handlers: [InstructionHandler] = [
    executeLoadNil,
    executeLoadB,
    // ...
]
```

---

### 2.5 Runtime: Tables (Hash Maps)

**Location**: `VM/src/ltable.cpp` (26KB)

**Responsibilities**:
- Implement Lua tables (the only data structure in Lua)
- Hybrid array + hash map
- Support metamethods
- Maintain boundary invariant for `#` operator

**Key Design Decisions**:
- **Hybrid structure**: Array part (indices 1..n) + hash part (everything else)
  - Why? Most tables are either pure arrays or pure objects, hybrid handles both
- **Array part sizing**: Largest n where ≥50% of slots [1..n] are filled
- **Hash function**: MurmurHash for numbers, cached hash for strings
- **Open addressing with chaining**: Uses Brent's variation for fast lookup
- **Boundary tracking**: Maintains boundary (last non-nil index) for `#` operator

**Table Structure** (from lobject.h:451):
```c++
typedef struct LuaTable {
    CommonHeader;  // GC header

    uint8_t tmcache;      // 1<<p means tagmethod(p) is not present
    uint8_t readonly;     // sandboxing feature
    uint8_t safeenv;      // environment doesn't share globals
    uint8_t lsizenode;    // log2(hash size)
    uint8_t nodemask8;    // (1<<lsizenode)-1, truncated to 8 bits

    int sizearray;        // size of array part
    union {
        int lastfree;     // any free position is before this
        int aboundary;    // negated boundary of array part
    };

    LuaTable* metatable;
    TValue* array;        // array part (contiguous)
    LuaNode* node;        // hash part (chained scatter table)
    GCObject* gclist;
} LuaTable;
```

**Hash Node Structure** (lobject.h:422):
```c++
typedef struct LuaNode {
    TValue val;    // value
    TKey key;      // key (with type tag and next pointer)
} LuaNode;

typedef struct TKey {
    Value value;
    int extra[LUA_EXTRA_SIZE];
    unsigned tt : 4;      // type tag (4 bits)
    int next : 28;        // next node in chain (28 bits)
} TKey;
```

**Why This Design?**:
- **Memory efficiency**: Array part is dense, hash part is sparse
- **Performance**: Array access is O(1), hash access is O(1) average
- **Flexibility**: Can efficiently represent arrays, objects, sparse arrays, etc.

**Key Algorithms**:
1. **Lookup** (ltable.cpp:133):
   - Check array part first if key is integer in range
   - Compute main position in hash part
   - Follow collision chain
2. **Resize** (triggered when load factor changes):
   - Recompute optimal array size (≥50% full)
   - Rehash all elements
3. **Insertion**:
   - If array index and array has space, insert directly
   - Otherwise, insert in hash part, resize if needed

**Swift Translation Strategy**:
```swift
// Use Swift's copy-on-write semantics to our advantage
final class LuaTable {
    // Array part: use ContiguousArray for cache locality
    private var array: ContiguousArray<Value> = []

    // Hash part: use Dictionary, but consider custom hash map
    private var hash: Dictionary<HashableValue, Value> = [:]

    // Metadata
    private(set) var metatable: LuaTable?
    private var boundary: Int = 0

    subscript(index: Int) -> Value {
        get {
            if index >= 1 && index <= array.count {
                return array[index - 1]
            }
            return hash[.number(Double(index))] ?? .nil
        }
        set {
            if index >= 1 && index <= array.count + 1 {
                // Extend array if needed
                if index == array.count + 1 {
                    array.append(newValue)
                } else {
                    array[index - 1] = newValue
                }
                updateBoundary()
            } else {
                hash[.number(Double(index))] = newValue
            }
        }
    }

    subscript(key: HashableValue) -> Value {
        get { hash[key] ?? .nil }
        set { hash[key] = newValue }
    }
}

// For better performance, consider implementing custom hash table
// with open addressing like Luau does, but Swift's Dictionary
// is surprisingly fast for MVP
```

**Optimization Opportunities for Swift**:
- Use SIMD for boundary search
- Custom hash table with open addressing (avoid Dictionary overhead)
- Pool table objects to reduce allocations

---

### 2.6 Runtime: Value Representation

**Location**: `VM/src/lobject.h` (lines 32-50)

**The TValue Structure**:
```c++
typedef union {
    GCObject* gc;      // pointer to GC'd object
    void* p;           // light userdata
    double n;          // number
    int b;             // boolean
    float v[2];        // vector (x, y, z in extra field)
} Value;

typedef struct lua_TValue {
    Value value;                   // 8 bytes
    int extra[LUA_EXTRA_SIZE];     // 4 bytes (used for vector.z)
    int tt;                        // 4 bytes (type tag)
} TValue;  // Total: 16 bytes
```

**Type Tags** (lua.h:70):
```c++
enum lua_Type {
    LUA_TNIL = 0,          // must be 0 for isnoneornil
    LUA_TBOOLEAN = 1,      // must be 1 for l_isfalse
    LUA_TLIGHTUSERDATA,    // unmanaged pointer
    LUA_TNUMBER,           // double
    LUA_TVECTOR,           // float3 or float4

    // Below this: GC'd types
    LUA_TSTRING,           // interned string
    LUA_TTABLE,            // table (array+hash)
    LUA_TFUNCTION,         // closure
    LUA_TUSERDATA,         // managed userdata
    LUA_TTHREAD,           // coroutine
    LUA_TBUFFER,           // byte buffer
    // ...
};
```

**Key Design Decisions**:
- **NO NaN boxing**: Uses explicit type tag (simpler, debuggable)
  - Why not NaN boxing? Easier to debug, supports vectors natively, type tag is fast
- **16-byte structure**: Fits in 2 cache lines, alignment-friendly
- **Vector support**: Native float3/float4 for 3D math (Roblox games)
- **Separate GC bit**: Types ≥ LUA_TSTRING are GC'd

**Why NOT use NaN boxing?**
- Luau prioritizes clarity and vector support over space
- 16 bytes per value is acceptable for game scripting
- Debugging is easier with explicit tags
- Native vector type doesn't fit in NaN-boxed double

**Swift Translation Strategy**:
```swift
// Swift enums with associated values are perfect for this
enum Value {
    case `nil`
    case boolean(Bool)
    case number(Double)
    case vector(SIMD3<Float>)  // or SIMD4 for 4-component
    case string(LuaString)     // interned, reference-counted
    case table(LuaTable)       // reference-counted
    case function(Closure)
    case userdata(Userdata)
    // ... more cases
}

// Memory layout: Swift enum with associated value
// - 1 byte for discriminator (type tag)
// - 8 bytes for largest associated value
// - Padding for alignment
// Total: 16 bytes (similar to Luau!)

// For performance-critical code, we can use unsafe pointer magic:
struct UnsafeValue {
    var payload: UInt64  // holds number/pointer/boolean
    var tag: UInt8       // type tag
    var extra: [UInt8]   // padding or extra data
}
```

**Optimization Opportunities**:
- Use `@frozen` enum for faster switching
- Consider NaN boxing for number-heavy code (but measure first!)
- Use `indirect` case for large types to keep enum size small

---

### 2.7 Runtime: Garbage Collection

**Location**: `VM/src/lgc.cpp` (44KB)

**Algorithm**: Incremental Mark-Sweep with Tri-Color Invariant

**Responsibilities**:
- Collect unused objects (strings, tables, closures, threads, userdata)
- Minimize pause times (incremental collection)
- Maintain tri-color invariant (white, gray, black)
- Write barriers for mutations

**Key Design Decisions**:
- **Incremental**: Not stop-the-world, spreads work over allocations
  - Why? Games can't tolerate long pauses (frame drops)
- **Mark-sweep**: Not copying GC (simpler, no pointer updates)
- **Write barriers**: Track cross-generational pointers
- **Proportional-integral controller**: Adjusts GC pace based on allocation rate

**GC Phases**:
1. **Propagate** (Mark): Trace reachable objects, mark gray → black
2. **Atomic**: Finish marking in one atomic step
3. **Sweep**: Free white (unreachable) objects
4. **Pause**: Wait until allocation threshold reached

**GC State** (lstate.h:79):
```c++
struct GCStats {
    int32_t triggerterms[32];     // PI controller state
    uint32_t triggertermpos;
    int32_t triggerintegral;

    size_t atomicstarttotalsizebytes;
    size_t endtotalsizebytes;
    size_t heapgoalsizebytes;

    double starttimestamp;
    double atomicstarttimestamp;
    double endtimestamp;
};
```

**Write Barriers** (integrated into setobj macros):
```c++
// When writing to a GC'd object, may need barrier
#define luaC_barrier(L, p, v) \
    { if (iscollectable(v) && isblack(obj2gco(p)) && iswhite(gcvalue(v))) \
        luaC_barrierf(L, obj2gco(p), gcvalue(v)); }
```

**Why Incremental GC?**:
- Predictable frame times in games
- Distributes GC work across frames
- Tunable (goal = heap size multiplier, step = work per allocation)

**Swift Translation Strategy**:
```swift
// Option 1: Use Swift's ARC (Automatic Reference Counting)
// Pros: Built-in, fast, deterministic
// Cons: Can't handle cycles (need weak references or cycle detection)

class LuaTable {
    var array: [Value]
    var hash: [HashableValue: Value]
    var metatable: LuaTable?  // Strong reference, potential cycle!
}

// Option 2: Hybrid ARC + Cycle Detector
// - Use ARC for most objects
// - Periodic cycle detection for tables/closures with cycles
// - Much simpler than full GC

final class CycleDetector {
    private var potentialCycles: Set<ObjectIdentifier> = []

    func markPotentialCycle(_ object: AnyObject) {
        potentialCycles.insert(ObjectIdentifier(object))
    }

    func detectCycles() {
        // DFS to find cycles, break them with weak references
        // Run periodically or when memory pressure occurs
    }
}

// Option 3: Full GC (like Luau)
// Only if we need Lua compatibility for mixed object graphs
// Probably overkill for internal use
```

**Recommendation for Swift**:
- **Use ARC for MVP**. It's fast, built-in, and handles 99% of cases.
- Add cycle detector if needed (measure first!).
- Luau's GC is optimized for C++ allocations, Swift's ARC is optimized for Swift.

---

### 2.8 Runtime: String Interning

**Location**: `VM/src/lstring.cpp` (5KB)

**Responsibilities**:
- Intern all strings (deduplicate)
- Cache hash values
- Integrate with GC

**Key Design Decisions**:
- **All strings interned**: No duplicate strings in memory
  - Why? Fast equality checks (pointer comparison), memory savings
- **Hash cached**: Stored in TString header
- **Global string table**: Hash table in global_State

**TString Structure** (lobject.h:237):
```c++
typedef struct TString {
    CommonHeader;       // GC header (tt, marked, memcat)
    int16_t atom;       // optional: atom ID for fast comparison
    TString* next;      // next in hash bucket
    unsigned int hash;  // cached hash value
    unsigned int len;   // string length
    char data[1];       // string data (flexible array member)
} TString;
```

**String Table** (lstate.h:19):
```c++
typedef struct stringtable {
    TString** hash;     // array of hash buckets
    uint32_t nuse;      // number of interned strings
    int size;           // size of hash array
} stringtable;
```

**Why Interning?**:
- **Fast equality**: Just compare pointers (`tsvalue(a) == tsvalue(b)`)
- **Memory savings**: "hello" appears once, even if used 1000 times
- **Fast hashing**: Hash computed once, cached forever

**Swift Translation Strategy**:
```swift
// Swift's String is already optimized, but we can do better
final class InternedString: Hashable {
    let value: String
    let hash: Int

    private static var internTable: [String: InternedString] = [:]

    static func intern(_ str: String) -> InternedString {
        if let existing = internTable[str] {
            return existing
        }
        let interned = InternedString(value: str)
        internTable[str] = interned
        return interned
    }

    private init(value: String) {
        self.value = value
        self.hash = value.hashValue
    }

    // Pointer equality for fast comparison
    static func == (lhs: InternedString, rhs: InternedString) -> Bool {
        return lhs === rhs  // Pointer comparison!
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(ObjectIdentifier(self))  // Fast hash
    }
}

// Alternative: Use StaticString for compile-time strings
// or leverage Swift's String small string optimization
```

---

### 2.9 CodeGen: Native Code Generation (Optional)

**Location**: `CodeGen/src/`

**Responsibilities**:
- JIT-compile bytecode to ARM64/x64 native code
- Optimize hot functions
- Integrate with VM for seamless fallback

**Key Design Decisions**:
- **Selective compilation**: Only compile hot functions (profiling-guided)
- **IR-based**: Bytecode → IR → Native (not direct translation)
- **Fallback to interpreter**: If JIT fails or for debugging
- **Inline caching**: Native code uses same inline caching as interpreter

**Recommendation**: Skip for MVP. Interpreter performance is excellent, native compilation adds significant complexity. Revisit if profiling shows bottlenecks.

---

## 3. Component Dependencies

```
┌──────────────────────────────────────────────────────────────┐
│                        Dependency Graph                       │
└──────────────────────────────────────────────────────────────┘

               VM (lvmexecute.cpp)
                      │
        ┌─────────────┼─────────────┐
        ▼             ▼             ▼
     Tables       Strings          GC
   (ltable.cpp)  (lstring.cpp)  (lgc.cpp)
        │             │             │
        └─────────────┴─────────────┘
                      │
                      ▼
                 Objects (lobject.h)
                 State (lstate.h)

               Compiler (Compiler.cpp)
                      │
        ┌─────────────┼─────────────┐
        ▼             ▼             ▼
   BytecodeBuilder   AST        ConstantFolding
                      │
                      ▼
                   Parser (Parser.cpp)
                      │
                      ▼
                   Lexer (Lexer.cpp)
```

**Key Insight**: The VM is relatively independent! We can implement it first, then add the compiler later.

---

## 4. Performance-Critical Paths

### 4.1 Hot Path 1: Table Property Access

**Flow**: `table.property` → GETTABLEKS instruction

1. Check if value is table
2. Hash string key (already cached in TString)
3. Check predicted slot (inline cache)
4. If hit: return value ✓
5. If miss: search hash table, update inline cache

**Why Fast**:
- Inline cache hits ~95% of the time
- Hash pre-computed and cached
- String pointer comparison (no strcmp)

**Swift Optimization**:
```swift
// Use Swift's Dictionary, it's already optimized
// For even faster access, consider storing cached slot in instruction
struct CachedTableAccess {
    var lastTable: ObjectIdentifier?
    var lastSlot: Dictionary<HashableValue, Value>.Index?

    @inline(__always)
    mutating func get(_ table: LuaTable, _ key: InternedString) -> Value {
        if let cachedTable = lastTable,
           cachedTable == ObjectIdentifier(table),
           let slot = lastSlot {
            // Cache hit! O(1) access
            return table.hash[slot].value
        }
        // Cache miss, update cache
        // ...
    }
}
```

### 4.2 Hot Path 2: Arithmetic Operations

**Flow**: `a + b` → ADD instruction

1. Check if both operands are numbers (type check)
2. If yes: perform addition, store result ✓
3. If no: check for __add metamethod, call it

**Why Fast**:
- Fast path is just type check + addition
- No function call overhead
- Branch predictor learns fast path

**Swift Optimization**:
```swift
@inline(__always)
func add(_ a: Value, _ b: Value) -> Value {
    // Fast path: both numbers
    if case .number(let lhs) = a, case .number(let rhs) = b {
        return .number(lhs + rhs)
    }
    // Slow path: metamethods
    return callMetamethod("__add", a, b)
}
```

### 4.3 Hot Path 3: Function Calls

**Flow**: `function(arg1, arg2)` → CALL instruction

1. Set up new call frame (CallInfo)
2. Copy arguments to new frame
3. If Lua function: Jump to bytecode
4. If C function: Call directly

**Why Needs Optimization**:
- Function calls are frequent
- Stack frame setup has overhead

**Optimization**: Inline small functions, tail call optimization

---

## 5. Key Data Structures

### 5.1 Proto (Function Prototype)

**Location**: lobject.h:286

Represents a compiled function:
```c++
typedef struct Proto {
    CommonHeader;

    uint8_t nups;           // number of upvalues
    uint8_t numparams;      // number of parameters
    uint8_t is_vararg;
    uint8_t maxstacksize;
    uint8_t flags;

    TValue* k;              // constants
    Instruction* code;      // bytecode
    struct Proto** p;       // nested functions

    uint8_t* lineinfo;      // debug: line numbers
    struct LocVar* locvars; // debug: local variables
    TString** upvalues;     // upvalue names
    TString* source;        // source file name
    TString* debugname;     // function name

    // sizes
    int sizecode;
    int sizep;
    int sizek;
    // ...
} Proto;
```

**Swift Translation**:
```swift
final class Proto {
    let instructions: [UInt32]
    let constants: [Value]
    let nestedProtos: [Proto]

    let upvalueCount: UInt8
    let paramCount: UInt8
    let isVararg: Bool
    let maxStackSize: UInt8

    // Debug info (optional)
    let lineInfo: [UInt8]?
    let localVars: [LocalVar]?
    let upvalueNames: [InternedString]?
    let source: InternedString?
    let debugName: InternedString?
}
```

### 5.2 Closure

**Location**: lobject.h:377

Function + upvalues:
```c++
typedef struct Closure {
    CommonHeader;

    uint8_t isC;         // C function or Lua function?
    uint8_nupvalues;
    uint8_t stacksize;
    uint8_t preload;

    GCObject* gclist;
    LuaTable* env;       // environment (globals)

    union {
        struct {         // C closure
            lua_CFunction f;
            lua_Continuation cont;
            const char* debugname;
            TValue upvals[1];
        } c;

        struct {         // Lua closure
            Proto* p;
            TValue uprefs[1];  // upvalue references
        } l;
    };
} Closure;
```

**Swift Translation**:
```swift
enum Closure {
    case lua(proto: Proto, upvalues: [UpvalueRef], env: LuaTable)
    case c(function: CFunction, upvalues: [Value], env: LuaTable, name: String)
}

typealias CFunction = (VirtualMachine) throws -> Int  // returns result count
```

---

## 6. Memory Layout Considerations for Apple Silicon

### 6.1 Cache Line Awareness

Apple Silicon (M1/M2/M3) has:
- L1 cache: 128KB data + 128KB instruction per core
- L2 cache: 4-12MB per core cluster
- Cache line: 64 bytes

**Optimization Strategies**:
1. **Keep hot fields together**: In TValue, Value union + tt are adjacent
2. **Pad structs to cache line**: Avoid false sharing
3. **Contiguous arrays**: Use ContiguousArray in Swift for better cache locality

### 6.2 SIMD Opportunities

Apple Silicon has excellent SIMD support (ARM Neon):
- Use `SIMD3<Float>` for vectors (already in Value enum)
- Vectorize table boundary search
- Vectorize string operations

---

## 7. Swift-Specific Optimizations

### 7.1 Value Types vs Reference Types

**Use Value Types (struct) for**:
- Instruction (UInt32)
- Small enums (Opcode, TypeTag)
- Small aggregates (SourceLocation)

**Use Reference Types (class) for**:
- Proto (shared across closures)
- LuaTable (mutated in place)
- Closures (captured by other closures)
- InternedString (needs pointer equality)

### 7.2 Copy-on-Write

Tables can leverage Swift's COW:
```swift
final class LuaTable {
    private var storage: TableStorage  // value type with COW

    mutating func insert(_ key: Value, _ value: Value) {
        // COW happens automatically if storage is shared
        storage.hash[key] = value
    }
}
```

### 7.3 Unsafe Optimizations

For hot paths, consider:
```swift
// Use UnsafePointer for bytecode iteration
func execute(_ instructions: UnsafePointer<UInt32>, count: Int) {
    var pc = instructions
    let end = pc.advanced(by: count)

    while pc < end {
        let insn = pc.pointee
        pc = pc.advanced(by: 1)
        // Execute instruction...
    }
}
```

---

## 8. Key Takeaways for Swift Reimplementation

### 8.1 What to Keep

1. **Register-based bytecode**: Proven faster than stack-based
2. **Hybrid tables**: Array + hash map is optimal for Lua semantics
3. **String interning**: Critical for performance
4. **Inline caching**: 95%+ hit rate, huge speedup
5. **Incremental design**: Separate compilation from execution

### 8.2 What to Change

1. **Memory management**: Use ARC instead of GC (simpler, faster on Swift)
2. **Value representation**: Use Swift enum (safer, similar performance)
3. **Dispatch mechanism**: Use switch statement (no computed goto in Swift)
4. **Type system**: Skip for MVP (add later if needed)
5. **Native codegen**: Skip for MVP (interpreter is fast enough)

### 8.3 What to Leverage

1. **SIMD types**: `SIMD3<Float>` for vectors
2. **Accelerate framework**: For math operations
3. **Metal**: For GPU compute (if needed)
4. **Unified memory**: Share data between CPU and GPU
5. **Swift's type system**: Catch more errors at compile time

---

## 9. Implementation Priority

### Phase 1: Core VM (4-6 weeks)
1. Value representation (Value enum)
2. Bytecode format (instruction encoding)
3. Basic VM loop (switch-based dispatch)
4. Stack management
5. Primitive operations (arithmetic, comparisons)

### Phase 2: Tables & Functions (3-4 weeks)
1. Table implementation (array + hash)
2. String interning
3. Function calls (Lua → Lua)
4. Closures and upvalues
5. C function interface

### Phase 3: Compiler (3-4 weeks)
1. Lexer
2. Parser (AST)
3. Bytecode compiler
4. Register allocation
5. Constant table

### Phase 4: Standard Library (2-3 weeks)
1. Base functions (print, type, etc.)
2. Table functions (insert, remove, etc.)
3. String functions
4. Math functions (leverage Accelerate)
5. I/O functions (if needed)

### Phase 5: Optimizations (Ongoing)
1. Inline caching
2. Fast paths
3. JIT compilation (optional)
4. SIMD vectorization
5. Metal integration (optional)

---

## 10. Performance Expectations

Based on Luau benchmarks vs Lua 5.1:
- **Interpreter**: 2-3x faster than Lua 5.1 (confirmed by benchmarks)
- **With native codegen**: 4-10x faster than Lua 5.1

**With Swift optimizations** (estimated):
- **ARC vs GC**: 10-20% faster (no GC pauses)
- **Enum dispatch**: Similar to switch (fast on modern CPUs)
- **Apple Silicon SIMD**: 2-4x faster for vector math
- **Metal integration**: 10-100x for parallel workloads

**Realistic target**: 3-5x faster than Lua 5.1 with pure Swift, 10-20x with SIMD/Metal for numerical code.

---

## Conclusion

Luau's architecture is well-designed for performance and embeddability. The key insights are:
1. **Register-based bytecode** is faster than stack-based
2. **Hybrid tables** efficiently handle both arrays and objects
3. **Incremental GC** provides predictable latency
4. **Inline caching** is critical for property access performance

For Swift reimplementation:
- Use Swift's strengths: ARC, value types, SIMD, Accelerate
- Start simple: Interpreter first, optimize later
- Measure everything: Profile before optimizing
- Don't over-engineer: Skip type system and JIT for MVP

This architecture provides a solid foundation for a Swift-native high-performance scripting engine optimized for Apple Silicon.
