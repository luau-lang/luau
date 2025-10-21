# Luau Swift 6.2 Implementation Roadmap

## Executive Summary

This roadmap outlines a **phased approach** to reimplementing Luau's core functionality in Swift 6.2, optimized for Apple Silicon. The goal is to build a **high-performance scripting engine** for internal use, without the constraint of maintaining Lua language compatibility.

**Timeline**: 12-16 weeks (3-4 months) for MVP, additional 8-12 weeks for optimizations

**Philosophy**: **Start simple, measure everything, optimize what matters**

---

## Guiding Principles

1. **MVP First**: Get something working quickly, optimize later
2. **Measure, Don't Guess**: Profile before optimizing
3. **Leverage Swift**: Use Swift's strengths (ARC, value types, SIMD, safety)
4. **Internal Use Only**: No Lua compatibility required, design for your needs
5. **Apple Silicon First**: Optimize for ARM64, x86_64 is secondary

---

## Phase 0: Preparation (Week 0, 3-5 days)

### Goals
- Set up development environment
- Create project structure
- Define success criteria

### Tasks

#### 1. Project Setup
```bash
# Create Swift package
swift package init --type library --name LuauSwift

# Directory structure
LuauSwift/
├── Sources/
│   ├── LuauCore/          # Core VM
│   ├── LuauCompiler/      # Compiler
│   ├── LuauRuntime/       # Standard library
│   └── LuauCLI/           # REPL
├── Tests/
│   ├── LuauCoreTests/
│   ├── LuauCompilerTests/
│   └── BenchmarkTests/
├── Benchmarks/            # Performance tests
└── Documentation/
```

#### 2. Define Success Criteria
**MVP Success**:
- ✅ Execute simple scripts (arithmetic, control flow, functions)
- ✅ Table operations (array and hash parts)
- ✅ Function calls (Lua → Lua, Lua → Swift, Swift → Lua)
- ✅ String operations
- ✅ Pass 80%+ of Luau conformance tests

**Performance Target**:
- ≥ 2x faster than Lua 5.1 interpreter (Luau is 2-3x, we should match)
- ≥ 5x faster than Lua 5.1 for vector math (leveraging SIMD)

#### 3. Set Up Benchmarks
Port Luau's benchmark suite:
```swift
// Benchmarks/Sources/BenchmarkRunner.swift
struct Benchmark {
    let name: String
    let script: String
    let iterations: Int
}

let benchmarks: [Benchmark] = [
    Benchmark(name: "fibonacci", script: fibScript, iterations: 1000),
    Benchmark(name: "mandelbrot", script: mandelbrotScript, iterations: 100),
    Benchmark(name: "table-insert", script: tableInsertScript, iterations: 1000),
    // ... port from luau/bench
]
```

**Deliverables**:
- [ ] Swift package structure
- [ ] Benchmark harness
- [ ] CI/CD pipeline (GitHub Actions)

---

## Phase 1: Core Value System & VM Foundation (Weeks 1-2, 10-14 days)

### Goals
- Implement value representation
- Build basic VM loop (interpreter)
- Support primitive operations (arithmetic, comparisons)

### Tasks

#### 1.1 Value Representation (3 days)

**File**: `Sources/LuauCore/Value.swift`

```swift
@frozen
public enum Value {
    case `nil`
    case boolean(Bool)
    case number(Double)
    case vector(SIMD3<Float>)
    // Add more types later (string, table, function)
}

extension Value {
    public var isNil: Bool {
        if case .nil = self { return true }
        return false
    }

    public var isFalsy: Bool {
        switch self {
        case .nil: return true
        case .boolean(let b): return !b
        default: return false
        }
    }

    // Arithmetic operations (inline for speed)
    @inline(__always)
    public static func add(_ lhs: Value, _ rhs: Value) throws -> Value {
        switch (lhs, rhs) {
        case (.number(let a), .number(let b)):
            return .number(a + b)
        case (.vector(let a), .vector(let b)):
            return .vector(a &+ b)  // SIMD!
        default:
            throw RuntimeError.typeError("attempt to add \(lhs) and \(rhs)")
        }
    }

    // ... sub, mul, div, mod, pow, eq, lt, le
}
```

**Tests**:
```swift
func testValueArithmetic() {
    let a = Value.number(10)
    let b = Value.number(20)
    let result = try Value.add(a, b)
    XCTAssertEqual(result, .number(30))
}

func testVectorSIMD() {
    let a = Value.vector(SIMD3(1, 2, 3))
    let b = Value.vector(SIMD3(4, 5, 6))
    let result = try Value.add(a, b)
    XCTAssertEqual(result, .vector(SIMD3(5, 7, 9)))
}
```

#### 1.2 Bytecode Format (2 days)

**File**: `Sources/LuauCore/Bytecode.swift`

```swift
@frozen
public enum Opcode: UInt8, CaseIterable {
    case nop = 0
    case loadNil = 2
    case loadB = 3
    case loadN = 4
    case move = 6
    case add = 26
    case sub = 27
    // ... essential opcodes first (~20 opcodes for MVP)
}

public struct Instruction {
    let raw: UInt32

    @inline(__always)
    public var opcode: Opcode {
        Opcode(rawValue: UInt8(raw & 0xFF))!
    }

    @inline(__always)
    public var a: UInt8 { UInt8((raw >> 8) & 0xFF) }
    @inline(__always)
    public var b: UInt8 { UInt8((raw >> 16) & 0xFF) }
    @inline(__always)
    public var c: UInt8 { UInt8((raw >> 24) & 0xFF) }
    @inline(__always)
    public var d: Int16 { Int16(bitPattern: UInt16(raw >> 16)) }
}

public struct Proto {
    public let instructions: [UInt32]
    public let constants: [Value]
    public let maxStackSize: Int

    // Add more fields later (nested protos, debug info)
}
```

#### 1.3 VM Interpreter (5 days)

**File**: `Sources/LuauCore/VirtualMachine.swift`

```swift
public final class VirtualMachine {
    // Execution state
    private var stack: ContiguousArray<Value>
    private var base: Int = 0
    private var top: Int = 0

    // Current function state
    private var proto: Proto!
    private var pc: UnsafePointer<UInt32>!
    private var constants: [Value] = []

    public init(stackSize: Int = 1024) {
        self.stack = ContiguousArray(repeating: .nil, count: stackSize)
    }

    public func execute(_ proto: Proto) throws {
        self.proto = proto
        self.constants = proto.constants

        // Use unsafe pointer for speed
        try proto.instructions.withUnsafeBufferPointer { buffer in
            self.pc = buffer.baseAddress!
            let end = pc.advanced(by: buffer.count)

            while pc < end {
                let insn = Instruction(raw: pc.pointee)
                pc = pc.advanced(by: 1)

                try executeInstruction(insn)
            }
        }
    }

    @inline(__always)
    private func executeInstruction(_ insn: Instruction) throws {
        switch insn.opcode {
        case .loadNil:
            stack[base + Int(insn.a)] = .nil

        case .loadN:
            stack[base + Int(insn.a)] = .number(Double(insn.d))

        case .move:
            stack[base + Int(insn.a)] = stack[base + Int(insn.b)]

        case .add:
            let lhs = stack[base + Int(insn.b)]
            let rhs = stack[base + Int(insn.c)]
            stack[base + Int(insn.a)] = try Value.add(lhs, rhs)

        // ... implement ~20 opcodes
        default:
            throw RuntimeError.unimplementedOpcode(insn.opcode)
        }
    }
}
```

**Tests**:
```swift
func testSimpleArithmetic() {
    // Bytecode for: local x = 10 + 20; return x
    let proto = Proto(
        instructions: [
            encodeAD(.loadN, 0, 10),    // R0 = 10
            encodeAD(.loadN, 1, 20),    // R1 = 20
            encodeABC(.add, 0, 0, 1),   // R0 = R0 + R1
            // RETURN would go here
        ],
        constants: [],
        maxStackSize: 2
    )

    let vm = VirtualMachine()
    try vm.execute(proto)
    XCTAssertEqual(vm.stack[0], .number(30))
}
```

#### 1.4 Jump Instructions (2 days)

Implement control flow: JUMP, JUMPIF, JUMPIFNOT

**Test**:
```swift
func testConditionalJump() {
    // Bytecode for: if x then y = 1 else y = 2 end
    let proto = Proto(
        instructions: [
            encodeAD(.loadB, 0, 1),           // R0 = true
            encodeAD(.jumpIfNot, 0, 2),       // if not R0 then jump +2
            encodeAD(.loadN, 1, 1),           // R1 = 1
            encodeAD(.jump, 0, 1),            // jump +1 (skip else)
            encodeAD(.loadN, 1, 2),           // R1 = 2 (else branch)
        ],
        constants: [],
        maxStackSize: 2
    )

    let vm = VirtualMachine()
    try vm.execute(proto)
    XCTAssertEqual(vm.stack[1], .number(1))  // true branch taken
}
```

### Milestone 1: Arithmetic Interpreter

**Deliverables**:
- [ ] Value system with numbers, booleans, nil
- [ ] Bytecode format (20-30 essential opcodes)
- [ ] VM interpreter (execute hardcoded bytecode)
- [ ] Pass 10+ unit tests

**Demo**: Execute hardcoded bytecode for fibonacci(10)

---

## Phase 2: Tables (Weeks 3-4, 10-14 days)

### Goals
- Implement hybrid array + hash table
- Support table operations (get, set, length)
- String interning

### Tasks

#### 2.1 String Interning (3 days)

**File**: `Sources/LuauCore/InternedString.swift`

```swift
public final class InternedString: Hashable {
    public let value: String
    public let hash: Int

    private static var internTable: [String: InternedString] = [:]
    private static var lock = OSAllocatedUnfairLock()

    public static func intern(_ str: String) -> InternedString {
        lock.lock()
        defer { lock.unlock() }

        if let existing = internTable[str] {
            return existing
        }

        let interned = InternedString(value: str, hash: str.hashValue)
        internTable[str] = interned
        return interned
    }

    private init(value: String, hash: Int) {
        self.value = value
        self.hash = hash
    }

    // Pointer equality (fast!)
    public static func == (lhs: InternedString, rhs: InternedString) -> Bool {
        return lhs === rhs
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(ObjectIdentifier(self))
    }
}

// Add to Value enum
extension Value {
    case string(InternedString)
}
```

#### 2.2 Table Implementation (7 days)

**File**: `Sources/LuauCore/LuaTable.swift`

```swift
public final class LuaTable {
    // Array part: 1-indexed, contiguous
    private var array: ContiguousArray<Value>

    // Hash part: any key
    private var hash: [HashableValue: Value]

    // Metadata
    private var boundary: Int = 0

    public init(arrayCapacity: Int = 0, hashCapacity: Int = 0) {
        self.array = ContiguousArray()
        self.array.reserveCapacity(arrayCapacity)
        self.hash = Dictionary(minimumCapacity: hashCapacity)
    }

    // Array-like access (1-indexed!)
    public subscript(index: Int) -> Value {
        get {
            if index >= 1 && index <= array.count {
                return array[index - 1]
            }
            return hash[.number(Double(index))] ?? .nil
        }
        set {
            if index >= 1 {
                // Extend array if needed
                while array.count < index {
                    array.append(.nil)
                }
                array[index - 1] = newValue
                updateBoundary()
            } else {
                hash[.number(Double(index))] = newValue
            }
        }
    }

    // Hash-like access
    public subscript(key: HashableValue) -> Value {
        get {
            // Check array part first if integer key
            if case .number(let n) = key,
               let i = Int(exactly: n),
               i >= 1 && i <= array.count {
                return array[i - 1]
            }
            return hash[key] ?? .nil
        }
        set {
            if case .number(let n) = key,
               let i = Int(exactly: n),
               i >= 1 {
                self[i] = newValue
            } else {
                hash[key] = newValue
            }
        }
    }

    // Length operator (#)
    public var length: Int {
        boundary
    }

    private func updateBoundary() {
        // Binary search for boundary (last non-nil index)
        var left = 0
        var right = array.count

        while left < right {
            let mid = (left + right + 1) / 2
            if !array[mid - 1].isNil {
                left = mid
            } else {
                right = mid - 1
            }
        }
        boundary = left
    }
}

// HashableValue for dictionary keys
public enum HashableValue: Hashable {
    case `nil`
    case boolean(Bool)
    case number(Double)
    case string(InternedString)
}
```

#### 2.3 Table Instructions (2 days)

Implement: NEWTABLE, GETTABLE, SETTABLE, GETTABLEKS, SETTABLEKS, LENGTH

**Test**:
```swift
func testTableOperations() {
    let t = LuaTable()
    t[1] = .number(10)
    t[2] = .number(20)
    t[.string(InternedString.intern("name"))] = .string(InternedString.intern("test"))

    XCTAssertEqual(t[1], .number(10))
    XCTAssertEqual(t[.string(InternedString.intern("name"))],
                   .string(InternedString.intern("test")))
    XCTAssertEqual(t.length, 2)
}
```

### Milestone 2: Tables & Strings

**Deliverables**:
- [ ] String interning system
- [ ] Hybrid array + hash table
- [ ] Table instructions (GETTABLE, SETTABLE, etc.)
- [ ] Pass 20+ table tests

**Demo**: Execute bytecode for table operations (insert, lookup, length)

---

## Phase 3: Functions & Closures (Weeks 5-6, 10-14 days)

### Goals
- Function calls (Lua → Lua)
- Closures with upvalue capture
- Return values (single and multiple)

### Tasks

#### 3.1 Call Frames (3 days)

**File**: `Sources/LuauCore/CallFrame.swift`

```swift
public struct CallFrame {
    let proto: Proto
    let base: Int           // Stack base for this frame
    let returnPC: Int       // PC to return to
    let resultCount: Int    // Expected result count (-1 = multret)
}

// Update VirtualMachine
extension VirtualMachine {
    private var callStack: [CallFrame] = []

    func call(_ closure: Closure, argCount: Int, resultCount: Int) throws {
        // Push new call frame
        let frame = CallFrame(
            proto: closure.proto,
            base: top - argCount,
            returnPC: currentPC,
            resultCount: resultCount
        )
        callStack.append(frame)

        // Setup new execution context
        base = frame.base
        proto = closure.proto
        constants = closure.proto.constants
        // Reset PC to start of function
    }

    func returnFromCall(_ values: [Value]) throws {
        guard let frame = callStack.popLast() else {
            throw RuntimeError.callStackUnderflow
        }

        // Copy return values to caller's stack
        let resultCount = frame.resultCount == -1 ? values.count : frame.resultCount
        for i in 0..<resultCount {
            stack[frame.base + i] = i < values.count ? values[i] : .nil
        }

        // Restore caller's context
        if let parentFrame = callStack.last {
            base = parentFrame.base
            proto = parentFrame.proto
            pc = // restore from returnPC
        }
    }
}
```

#### 3.2 Closures & Upvalues (5 days)

**File**: `Sources/LuauCore/Closure.swift`

```swift
public final class Closure {
    public let proto: Proto
    public var upvalues: [UpvalueRef]

    init(proto: Proto) {
        self.proto = proto
        self.upvalues = []
    }
}

// Upvalue reference (open or closed)
public enum UpvalueRef {
    case open(stackIndex: Int)     // Points to stack
    case closed(value: Value)      // Captured on heap
}

// Update Value enum
extension Value {
    case function(Closure)
}
```

#### 3.3 Function Instructions (4 days)

Implement: NEWCLOSURE, CALL, RETURN, CAPTURE, CLOSEUPVALS

**Test**:
```swift
func testFunctionCall() {
    // Bytecode for:
    // function add(a, b) return a + b end
    // local result = add(10, 20)

    let addProto = Proto(
        instructions: [
            encodeABC(.add, 2, 0, 1),    // R2 = R0 + R1
            encodeABC(.return, 2, 2),    // return R2
        ],
        constants: [],
        maxStackSize: 3
    )

    let mainProto = Proto(
        instructions: [
            encodeAD(.newClosure, 0, 0),      // R0 = closure(addProto)
            encodeAD(.loadN, 1, 10),          // R1 = 10
            encodeAD(.loadN, 2, 20),          // R2 = 20
            encodeABC(.call, 0, 3, 2),        // R0 = R0(R1, R2), 2 results
            // R0 now contains 30
        ],
        constants: [.proto(addProto)],
        maxStackSize: 10
    )

    let vm = VirtualMachine()
    try vm.execute(mainProto)
    XCTAssertEqual(vm.stack[0], .number(30))
}
```

### Milestone 3: Functions Work

**Deliverables**:
- [ ] Call frames & call stack
- [ ] Closures with upvalue capture
- [ ] Function call instructions
- [ ] Pass 30+ function tests

**Demo**: Execute bytecode for recursive fibonacci

---

## Phase 4: Basic Compiler (Weeks 7-9, 15-21 days)

### Goals
- Lexer (tokenization)
- Parser (AST generation)
- Compiler (AST → Bytecode)

### Tasks

#### 4.1 Lexer (4 days)

**File**: `Sources/LuauCompiler/Lexer.swift`

```swift
public enum TokenKind {
    case eof
    case identifier(String)
    case number(Double)
    case string(String)
    case keyword(Keyword)
    case symbol(Symbol)
}

public enum Keyword: String {
    case `if`, `then`, `else`, `elseif`, `end`
    case `while`, `do`, `for`, `in`, `repeat`, `until`
    case `function`, `local`, `return`, `break`
    case `nil`, `true`, `false`
    case `and`, `or`, `not`
}

public struct Token {
    public let kind: TokenKind
    public let location: SourceLocation
}

public final class Lexer {
    private let source: String
    private var index: String.Index
    private var line: Int = 1
    private var column: Int = 1

    public init(source: String) {
        self.source = source
        self.index = source.startIndex
    }

    public func nextToken() throws -> Token {
        skipWhitespace()

        guard index < source.endIndex else {
            return Token(kind: .eof, location: currentLocation)
        }

        let char = source[index]

        // Identifiers and keywords
        if char.isLetter || char == "_" {
            return scanIdentifierOrKeyword()
        }

        // Numbers
        if char.isNumber {
            return try scanNumber()
        }

        // Strings
        if char == "\"" || char == "'" {
            return try scanString()
        }

        // Symbols
        return try scanSymbol()
    }

    // ... implement scanning methods
}
```

#### 4.2 Parser (7 days)

**File**: `Sources/LuauCompiler/Parser.swift`

```swift
public enum AstNode {
    case block([AstStat])
    case stat(AstStat)
    case expr(AstExpr)
}

public enum AstStat {
    case local([String], [AstExpr])      // local x, y = 1, 2
    case assignment([AstExpr], [AstExpr]) // x, y = 1, 2
    case functionCall(AstExpr)            // f()
    case ifStat(AstExpr, AstBlock, [(AstExpr, AstBlock)], AstBlock?) // if-then-else
    case whileStat(AstExpr, AstBlock)     // while-do
    case repeatStat(AstBlock, AstExpr)    // repeat-until
    case returnStat([AstExpr])            // return
}

public enum AstExpr {
    case nil
    case boolean(Bool)
    case number(Double)
    case string(String)
    case variable(String)
    case index(AstExpr, AstExpr)          // t[k]
    case dot(AstExpr, String)             // t.k
    case call(AstExpr, [AstExpr])         // f(args)
    case function([String], AstBlock)     // function(params) body end
    case table([TableEntry])              // { ... }
    case binOp(BinaryOp, AstExpr, AstExpr)
    case unOp(UnaryOp, AstExpr)
}

public final class Parser {
    private let lexer: Lexer
    private var current: Token

    public init(source: String) {
        self.lexer = Lexer(source: source)
        self.current = try! lexer.nextToken()
    }

    public func parse() throws -> AstBlock {
        var statements: [AstStat] = []
        while current.kind != .eof {
            statements.append(try parseStatement())
        }
        return AstBlock(statements)
    }

    private func parseStatement() throws -> AstStat {
        switch current.kind {
        case .keyword(.local):
            return try parseLocal()
        case .keyword(.if):
            return try parseIf()
        case .keyword(.while):
            return try parseWhile()
        case .keyword(.return):
            return try parseReturn()
        default:
            // Assignment or function call
            return try parseAssignmentOrCall()
        }
    }

    // ... implement parsing methods (recursive descent)
}
```

#### 4.3 Compiler (7 days)

**File**: `Sources/LuauCompiler/Compiler.swift`

```swift
public final class Compiler {
    private var builder: BytecodeBuilder
    private var locals: [String] = []
    private var registerStack: RegisterStack

    public init() {
        self.builder = BytecodeBuilder()
        self.registerStack = RegisterStack()
    }

    public func compile(_ ast: AstBlock) throws -> Proto {
        for stat in ast.statements {
            try compileStatement(stat)
        }
        return builder.build()
    }

    private func compileStatement(_ stat: AstStat) throws {
        switch stat {
        case .local(let names, let exprs):
            // Compile expressions
            for expr in exprs {
                let reg = try compileExpression(expr)
                // Store in local
            }

        case .assignment(let targets, let exprs):
            // Compile and assign

        case .returnStat(let exprs):
            // Compile return values
            builder.emitReturn(...)
        }
    }

    private func compileExpression(_ expr: AstExpr) throws -> UInt8 {
        switch expr {
        case .number(let n):
            let reg = registerStack.allocate()
            if let smallN = Int16(exactly: n), smallN >= -32768 && smallN <= 32767 {
                builder.emitLoadN(reg, smallN)
            } else {
                builder.emitLoadK(reg, .number(n))
            }
            return reg

        case .binOp(.add, let lhs, let rhs):
            let lhsReg = try compileExpression(lhs)
            let rhsReg = try compileExpression(rhs)
            let destReg = registerStack.allocate()
            builder.emitAdd(destReg, lhsReg, rhsReg)
            registerStack.free(lhsReg)
            registerStack.free(rhsReg)
            return destReg

        // ... more cases
        }
    }
}
```

### Milestone 4: End-to-End Compiler

**Deliverables**:
- [ ] Lexer (tokenization)
- [ ] Parser (AST generation)
- [ ] Compiler (AST → Bytecode)
- [ ] Compile and execute "hello world"

**Demo**: Compile source code → bytecode → execute

---

## Phase 5: Standard Library (Weeks 10-11, 10-14 days)

### Goals
- Essential built-in functions
- Table library (insert, remove, sort, etc.)
- String library (sub, find, format, etc.)
- Math library (leveraging Accelerate framework)

### Tasks

#### 5.1 Base Functions (4 days)

```swift
// Sources/LuauRuntime/BaseLib.swift
public func luau_print(_ vm: VirtualMachine, _ args: [Value]) -> [Value] {
    for arg in args {
        print(arg, terminator: " ")
    }
    print()
    return []
}

public func luau_type(_ vm: VirtualMachine, _ args: [Value]) -> [Value] {
    guard let value = args.first else {
        return [.nil]
    }

    let typeName: String
    switch value {
    case .nil: typeName = "nil"
    case .boolean: typeName = "boolean"
    case .number: typeName = "number"
    case .string: typeName = "string"
    case .table: typeName = "table"
    case .function: typeName = "function"
    // ...
    }

    return [.string(InternedString.intern(typeName))]
}

// Register built-ins
public func registerBaseLib(_ vm: VirtualMachine) {
    vm.setGlobal("print", .cfunction(luau_print))
    vm.setGlobal("type", .cfunction(luau_type))
    vm.setGlobal("tonumber", .cfunction(luau_tonumber))
    vm.setGlobal("tostring", .cfunction(luau_tostring))
    // ...
}
```

#### 5.2 Table Library (3 days)

```swift
public func luau_table_insert(_ vm: VirtualMachine, _ args: [Value]) throws -> [Value] {
    guard case .table(let t) = args[0] else {
        throw RuntimeError.typeError("table.insert expects table")
    }

    if args.count == 2 {
        // table.insert(t, value) - append
        let index = t.length + 1
        t[index] = args[1]
    } else if args.count == 3 {
        // table.insert(t, pos, value)
        guard case .number(let pos) = args[1], let idx = Int(exactly: pos) else {
            throw RuntimeError.typeError("bad argument #2 to 'insert'")
        }
        // Shift elements and insert
        // ...
    }

    return []
}
```

#### 5.3 Math Library (3 days)

```swift
// Leverage Accelerate framework for SIMD
import Accelerate

public func luau_math_sqrt(_ vm: VirtualMachine, _ args: [Value]) throws -> [Value] {
    guard case .number(let x) = args.first else {
        throw RuntimeError.typeError("math.sqrt expects number")
    }
    return [.number(sqrt(x))]
}

public func luau_math_sin(_ vm: VirtualMachine, _ args: [Value]) throws -> [Value] {
    guard case .number(let x) = args.first else {
        throw RuntimeError.typeError("math.sin expects number")
    }
    return [.number(sin(x))]
}

// Vector operations (SIMD!)
public func luau_vector_magnitude(_ vm: VirtualMachine, _ args: [Value]) throws -> [Value] {
    guard case .vector(let v) = args.first else {
        throw RuntimeError.typeError("vector.magnitude expects vector")
    }
    let magnitude = sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
    return [.number(Double(magnitude))]
}
```

### Milestone 5: Usable Scripting Language

**Deliverables**:
- [ ] Base functions (print, type, etc.)
- [ ] Table library
- [ ] String library
- [ ] Math library (with SIMD)
- [ ] Pass 100+ conformance tests

**Demo**: Run real scripts (game logic, data processing, etc.)

---

## Phase 6: Optimization (Weeks 12-16, Ongoing)

### Goals
- Profile and identify bottlenecks
- Implement fast paths
- Inline caching
- JIT compilation (optional, future)

### Tasks

#### 6.1 Profiling (2 days)

```swift
// Use Instruments (Time Profiler)
// Identify hot functions
```

#### 6.2 Inline Caching (5 days)

```swift
// Cache table property lookups
struct InlineCache {
    var lastTable: ObjectIdentifier?
    var lastKey: InternedString?
    var lastValue: Value?
}

// Store cache in instruction metadata
```

#### 6.3 Fast Calls (5 days)

```swift
// Implement FASTCALL instructions for common built-ins
case .fastCall1:
    let builtinID = insn.a
    let arg = stack[base + Int(insn.b)]

    switch builtinID {
    case LBF_MATH_ABS:
        if case .number(let n) = arg {
            stack[base + Int(insn.a)] = .number(abs(n))
            pc = pc.advanced(by: Int(insn.c))  // Skip fallback
            break
        }
    // ... more fast calls
    }
```

#### 6.4 SIMD Optimizations (3 days)

```swift
// Use SIMD for batch operations
func vectorAdd(_ a: [Float], _ b: [Float]) -> [Float] {
    var result = [Float](repeating: 0, count: a.count)
    vDSP_vadd(a, 1, b, 1, &result, 1, vDSP_Length(a.count))
    return result
}
```

### Milestone 6: Performance Parity

**Deliverables**:
- [ ] 2-3x faster than Lua 5.1 (match Luau)
- [ ] 5-10x faster for vector math (SIMD)
- [ ] Inline caching (95%+ hit rate)
- [ ] Fast calls for common built-ins

**Demo**: Benchmark results vs Lua 5.1 and Luau

---

## Success Metrics

### Performance Benchmarks

| Benchmark | Lua 5.1 | Luau | Our Target | Goal |
|-----------|---------|------|------------|------|
| Fibonacci | 1.0x | 2.5x | 2.0x+ | Pass ✅ |
| Mandelbrot | 1.0x | 3.0x | 2.5x+ | Pass ✅ |
| Table Insert | 1.0x | 2.0x | 2.0x+ | Pass ✅ |
| String Ops | 1.0x | 2.5x | 2.0x+ | Pass ✅ |
| Vector Math | 1.0x | 3.0x | 5.0x+ | Stretch 🎯 |

### Conformance Tests

- Pass ≥ 80% of Luau conformance suite
- 100% pass rate on MVP feature subset

### Code Quality

- 90%+ test coverage
- Zero compiler warnings
- Passes SwiftLint checks
- Documentation for all public APIs

---

## Risk Mitigation

### Technical Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| Swift ARC slower than GC | Medium | Profile early, optimize hot paths, consider custom allocator |
| Switch dispatch slower than computed goto | Low | Measure first, consider function pointer dispatch |
| Table performance | Medium | Implement inline caching early |
| Parser complexity | Medium | Start with simple grammar, add features incrementally |

### Timeline Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| Underestimated complexity | High | Build MVP first, defer optimizations |
| Scope creep | Medium | Stick to roadmap, resist feature additions |
| Debugging challenges | Medium | Invest in good error messages and debugging tools |

---

## Post-MVP Enhancements

### Phase 7+: Advanced Features (Optional, 8-12 weeks)

**Type System** (4-6 weeks):
- Type inference
- Type checking
- IDE integration (LSP)

**JIT Compilation** (4-6 weeks):
- Profile-guided JIT
- ARM64 code generation
- Inline caching with code patching

**Debugger** (2-3 weeks):
- Breakpoints
- Single-step execution
- Variable inspection

**Metal Integration** (3-4 weeks):
- GPU compute shaders
- Parallel execution
- Shared memory optimization

---

## Conclusion

This roadmap provides a **pragmatic, phased approach** to reimplementing Luau in Swift 6.2:

1. **Phase 1-2** (4 weeks): Core VM + Tables
2. **Phase 3** (2 weeks): Functions & Closures
3. **Phase 4** (3 weeks): Compiler
4. **Phase 5** (2 weeks): Standard Library
5. **Phase 6** (4 weeks): Optimization

**Total MVP**: 12-16 weeks (3-4 months)

**Key Success Factors**:
- ✅ Start simple, iterate quickly
- ✅ Measure everything (profile before optimizing)
- ✅ Leverage Swift's strengths (ARC, SIMD, value types)
- ✅ Test continuously (unit tests + conformance suite)

**Result**: A high-performance, Swift-native scripting engine optimized for Apple Silicon that matches or exceeds Luau's performance while being safer and easier to maintain.

---

## Quick Start

```bash
# Clone and set up
git clone https://github.com/your-org/luau-swift
cd luau-swift
swift build

# Run tests
swift test

# Run benchmarks
swift run LuauBenchmarks

# Try REPL
swift run luau-repl
> print("Hello from Luau Swift!")
Hello from Luau Swift!
```

Let's build it! 🚀
