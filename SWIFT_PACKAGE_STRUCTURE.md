# Swift 6.2 Package Structure - Recommended Format

## Overview

This document outlines the **recommended Swift Package Manager structure** for the Luau Swift 6.2 reimplementation, following Apple's official guidelines and 2025 best practices.

---

## Directory Structure

```
LuauSwift/
├── Package.swift                          # Package manifest
├── README.md                              # Package documentation
├── LICENSE                                # MIT License
├── .gitignore                            # Git ignore rules
├── .swiftpm/                             # SPM working directory (git-ignored)
│   └── xcode/
├── Sources/                              # All source code
│   ├── LuauCore/                         # Core VM library (target)
│   │   ├── Value.swift
│   │   ├── Bytecode.swift
│   │   ├── VirtualMachine.swift
│   │   ├── Proto.swift
│   │   ├── Closure.swift
│   │   ├── Stack.swift
│   │   └── RuntimeError.swift
│   ├── LuauRuntime/                      # Runtime library (target)
│   │   ├── LuaTable.swift
│   │   ├── InternedString.swift
│   │   ├── Userdata.swift
│   │   └── Buffer.swift
│   ├── LuauCompiler/                     # Compiler library (target)
│   │   ├── Lexer.swift
│   │   ├── Parser.swift
│   │   ├── AST.swift
│   │   ├── Compiler.swift
│   │   ├── BytecodeBuilder.swift
│   │   └── RegisterAllocator.swift
│   ├── LuauStdlib/                       # Standard library (target)
│   │   ├── BaseLib.swift
│   │   ├── TableLib.swift
│   │   ├── StringLib.swift
│   │   ├── MathLib.swift
│   │   └── VectorLib.swift
│   └── luau/                             # CLI executable (target)
│       └── main.swift
├── Tests/                                # All tests
│   ├── LuauCoreTests/                    # Unit tests for Core
│   │   ├── ValueTests.swift
│   │   ├── VMTests.swift
│   │   ├── BytecodeTests.swift
│   │   └── StackTests.swift
│   ├── LuauRuntimeTests/                 # Unit tests for Runtime
│   │   ├── TableTests.swift
│   │   └── StringTests.swift
│   ├── LuauCompilerTests/                # Unit tests for Compiler
│   │   ├── LexerTests.swift
│   │   ├── ParserTests.swift
│   │   └── CompilerTests.swift
│   ├── LuauStdlibTests/                  # Unit tests for Stdlib
│   │   └── StdlibTests.swift
│   └── ConformanceTests/                 # Integration tests
│       ├── ConformanceRunner.swift
│       └── Scripts/                      # Test Lua scripts
│           ├── arithmetic.lua
│           ├── tables.lua
│           └── functions.lua
├── Benchmarks/                           # Performance benchmarks
│   ├── Package.swift                     # Separate benchmark package
│   └── Sources/
│       └── LuauBenchmarks/
│           ├── main.swift
│           ├── FibonacciBench.swift
│           ├── MandelbrotBench.swift
│           └── TableBench.swift
└── Documentation/                        # Additional documentation
    ├── ARCHITECTURE.md
    ├── BYTECODE_SPEC.md
    ├── VALUE_SYSTEM.md
    └── IMPLEMENTATION_ROADMAP.md
```

---

## Package.swift Manifest

### Recommended Format for Swift 6.2

```swift
// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "LuauSwift",

    // Supported platforms
    platforms: [
        .macOS(.v14),      // macOS Sonoma and later
        .iOS(.v17),        // iOS 17 and later
        .visionOS(.v1)     // visionOS support
    ],

    // Products define the executables and libraries a package produces
    products: [
        // Core VM library - the heart of the system
        .library(
            name: "LuauCore",
            targets: ["LuauCore"]
        ),

        // Runtime library - tables, strings, etc.
        .library(
            name: "LuauRuntime",
            targets: ["LuauRuntime"]
        ),

        // Compiler library - lexer, parser, bytecode generation
        .library(
            name: "LuauCompiler",
            targets: ["LuauCompiler"]
        ),

        // Standard library - built-in functions
        .library(
            name: "LuauStdlib",
            targets: ["LuauStdlib"]
        ),

        // CLI executable - REPL and script runner
        .executable(
            name: "luau",
            targets: ["luau"]
        ),
    ],

    // Dependencies - external packages this package depends on
    dependencies: [
        // Example: SwiftArgumentParser for CLI
        // .package(url: "https://github.com/apple/swift-argument-parser", from: "1.3.0"),
    ],

    // Targets are the basic building blocks of a package
    targets: [
        // MARK: - Core VM
        .target(
            name: "LuauCore",
            dependencies: [],
            path: "Sources/LuauCore",
            swiftSettings: [
                // Enable complete concurrency checking for Swift 6
                .enableUpcomingFeature("StrictConcurrency"),

                // Enable compiler optimizations for release builds
                .unsafeFlags(["-O"], .when(configuration: .release)),

                // Suppress specific warnings if needed
                // .unsafeFlags(["-suppress-warnings"], .when(configuration: .debug)),
            ]
        ),

        // MARK: - Runtime
        .target(
            name: "LuauRuntime",
            dependencies: ["LuauCore"],
            path: "Sources/LuauRuntime"
        ),

        // MARK: - Compiler
        .target(
            name: "LuauCompiler",
            dependencies: ["LuauCore", "LuauRuntime"],
            path: "Sources/LuauCompiler"
        ),

        // MARK: - Standard Library
        .target(
            name: "LuauStdlib",
            dependencies: ["LuauCore", "LuauRuntime"],
            path: "Sources/LuauStdlib"
        ),

        // MARK: - CLI Executable
        .executableTarget(
            name: "luau",
            dependencies: [
                "LuauCore",
                "LuauRuntime",
                "LuauCompiler",
                "LuauStdlib",
                // .product(name: "ArgumentParser", package: "swift-argument-parser"),
            ],
            path: "Sources/luau"
        ),

        // MARK: - Tests
        .testTarget(
            name: "LuauCoreTests",
            dependencies: ["LuauCore"],
            path: "Tests/LuauCoreTests"
        ),

        .testTarget(
            name: "LuauRuntimeTests",
            dependencies: ["LuauCore", "LuauRuntime"],
            path: "Tests/LuauRuntimeTests"
        ),

        .testTarget(
            name: "LuauCompilerTests",
            dependencies: ["LuauCore", "LuauCompiler"],
            path: "Tests/LuauCompilerTests"
        ),

        .testTarget(
            name: "LuauStdlibTests",
            dependencies: ["LuauCore", "LuauRuntime", "LuauStdlib"],
            path: "Tests/LuauStdlibTests"
        ),

        .testTarget(
            name: "ConformanceTests",
            dependencies: ["LuauCore", "LuauRuntime", "LuauCompiler", "LuauStdlib"],
            path: "Tests/ConformanceTests",
            resources: [
                .copy("Scripts")  // Include Lua test scripts as resources
            ]
        ),
    ],

    // Swift language version
    swiftLanguageModes: [.v6]
)
```

---

## Key Features Explained

### 1. Swift Tools Version

```swift
// swift-tools-version: 6.0
```

**Why 6.0?**
- Latest package manifest API
- Support for Swift 6.2 language features
- Enhanced diagnostic control
- Platform-specific settings

### 2. Platform Support

```swift
platforms: [
    .macOS(.v14),      // Sonoma+
    .iOS(.v17),        // iOS 17+
    .visionOS(.v1)     // visionOS support
]
```

**Why these versions?**
- macOS 14 = Apple Silicon optimizations
- iOS 17 = Latest Swift runtime
- visionOS = Future-proofing for spatial computing

### 3. Products vs Targets

**Products** = What you expose to package consumers
**Targets** = Internal building blocks

```swift
products: [
    .library(name: "LuauCore", targets: ["LuauCore"]),  // Public library
]

targets: [
    .target(name: "LuauCore", dependencies: []),         // Implementation
]
```

### 4. Swift Settings

```swift
swiftSettings: [
    // Enable strict concurrency checking
    .enableUpcomingFeature("StrictConcurrency"),

    // Optimization flags for release
    .unsafeFlags(["-O"], .when(configuration: .release)),

    // Suppress specific warnings
    .unsafeFlags(["-suppress-warnings"], .when(configuration: .debug)),
]
```

**Best Practices**:
- Use `.enableUpcomingFeature()` for gradual Swift 6 migration
- Use `.unsafeFlags()` sparingly (breaks package ecosystem)
- Prefer `.define()` for conditional compilation

### 5. Dependencies

**Internal dependencies** (within package):
```swift
.target(
    name: "LuauCompiler",
    dependencies: ["LuauCore", "LuauRuntime"]  // Depends on other targets
)
```

**External dependencies** (other packages):
```swift
dependencies: [
    .package(url: "https://github.com/apple/swift-argument-parser", from: "1.3.0"),
]

targets: [
    .executableTarget(
        name: "luau",
        dependencies: [
            .product(name: "ArgumentParser", package: "swift-argument-parser")
        ]
    )
]
```

**Version specifiers**:
- `from: "1.0.0"` = 1.0.0 up to (but not including) 2.0.0
- `.upToNextMinor(from: "1.2.0")` = 1.2.0 up to 1.3.0
- `.exact("1.2.3")` = Exactly 1.2.3 (avoid this)
- `branch: "main"` = Latest from branch (development only)

### 6. Resources

```swift
.testTarget(
    name: "ConformanceTests",
    resources: [
        .copy("Scripts"),           // Copy directory as-is
        .process("Assets")          // Process and optimize assets
    ]
)
```

**Resource types**:
- `.copy()` = Copy files verbatim (Lua scripts, JSON, etc.)
- `.process()` = Let SPM process (images, plists, etc.)

---

## Target Organization Best Practices

### 1. Naming Conventions

- **Targets**: PascalCase, descriptive (`LuauCore`, `LuauCompiler`)
- **Executables**: lowercase, short (`luau`, `luau-analyze`)
- **Test targets**: Suffix with `Tests` (`LuauCoreTests`)

### 2. Dependency Hierarchy

```
┌─────────────────────────────────────────┐
│              luau (exe)                  │
└──────────────────┬──────────────────────┘
                   │
         ┌─────────┴─────────┐
         ▼                   ▼
   LuauCompiler          LuauStdlib
         │                   │
         └─────────┬─────────┘
                   ▼
            LuauRuntime
                   │
                   ▼
              LuauCore
```

**Rules**:
- Core has no dependencies (pure value system, VM)
- Runtime depends on Core (tables, strings use Value)
- Compiler depends on Core + Runtime (generates bytecode)
- Stdlib depends on Core + Runtime (built-in functions)
- Executable depends on everything

### 3. Module Boundaries

**LuauCore** (pure VM):
- `Value`, `Bytecode`, `VirtualMachine`
- No I/O, no dependencies
- Platform-agnostic

**LuauRuntime** (data structures):
- `LuaTable`, `InternedString`, `Userdata`
- Depends on Core for Value
- Still platform-agnostic

**LuauCompiler** (source → bytecode):
- `Lexer`, `Parser`, `Compiler`
- Depends on Core + Runtime
- No VM execution

**LuauStdlib** (built-ins):
- `BaseLib`, `TableLib`, `MathLib`
- Depends on Core + Runtime
- May use platform APIs (Accelerate, etc.)

---

## Swift 6.2 Specific Features

### 1. Language Modes

```swift
swiftLanguageModes: [.v6]
```

**Options**:
- `.v5` = Swift 5 language mode (gradual migration)
- `.v6` = Swift 6 language mode (strict concurrency, etc.)
- Can specify per-target if needed

### 2. Diagnostic Settings

```swift
swiftSettings: [
    // Treat specific warnings as errors
    .unsafeFlags(["-Werror=deprecated"], .when(configuration: .release)),

    // Suppress specific warnings
    .unsafeFlags(["-Wno-unused-parameter"], .when(configuration: .debug)),
]
```

**Swift 6.2 enhancement**: Can now control at diagnostic group level

### 3. Upcoming Features

```swift
swiftSettings: [
    .enableUpcomingFeature("StrictConcurrency"),       // Strict concurrency checking
    .enableUpcomingFeature("ExistentialAny"),          // Require 'any' keyword
    .enableUpcomingFeature("BareSlashRegexLiterals"),  // Bare slash regex
]
```

**Available features**:
- `StrictConcurrency` = Required for Swift 6 (helps find data races)
- `ExistentialAny` = Makes protocols explicit
- `BareSlashRegexLiterals` = Regex syntax improvements

---

## Testing Structure

### Unit Tests

```swift
import XCTest
@testable import LuauCore  // @testable exposes internal APIs

final class ValueTests: XCTestCase {
    func testNumberAddition() throws {
        let a = Value.number(10)
        let b = Value.number(20)
        let result = try Value.add(a, b)
        XCTAssertEqual(result, .number(30))
    }

    func testVectorSIMD() throws {
        let a = Value.vector(SIMD3(1, 2, 3))
        let b = Value.vector(SIMD3(4, 5, 6))
        let result = try Value.add(a, b)
        XCTAssertEqual(result, .vector(SIMD3(5, 7, 9)))
    }
}
```

### Integration Tests

```swift
final class ConformanceTests: XCTestCase {
    func testArithmeticScript() throws {
        let source = """
        local x = 10 + 20
        local y = x * 2
        return y
        """

        let vm = VirtualMachine()
        let compiler = Compiler()
        let proto = try compiler.compile(source)
        try vm.execute(proto)

        XCTAssertEqual(vm.stack[0], .number(60))
    }
}
```

### Resource Loading

```swift
func loadScript(_ name: String) throws -> String {
    let bundle = Bundle.module  // Auto-generated for SPM
    guard let url = bundle.url(forResource: name, withExtension: "lua", subdirectory: "Scripts"),
          let contents = try? String(contentsOf: url) else {
        throw TestError.scriptNotFound(name)
    }
    return contents
}
```

---

## Benchmark Package

### Separate Package for Benchmarks

```swift
// Benchmarks/Package.swift
// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "LuauBenchmarks",
    platforms: [.macOS(.v14)],
    dependencies: [
        // Depend on main package
        .package(path: ".."),

        // Benchmarking framework
        .package(url: "https://github.com/ordo-one/package-benchmark", from: "1.4.0"),
    ],
    targets: [
        .executableTarget(
            name: "LuauBenchmarks",
            dependencies: [
                .product(name: "LuauCore", package: "LuauSwift"),
                .product(name: "LuauCompiler", package: "LuauSwift"),
                .product(name: "Benchmark", package: "package-benchmark"),
            ]
        )
    ]
)
```

**Why separate package?**
- Benchmarks have different dependencies
- Don't bloat main package
- Can use `.package(path: "..")` for local development

---

## .gitignore

```gitignore
# Swift Package Manager
.build/
.swiftpm/
*.xcodeproj
*.xcworkspace

# Xcode
xcuserdata/
*.xcscmblueprint
*.xccheckout

# macOS
.DS_Store

# Build artifacts
*.o
*.a
*.dylib
*.dSYM/

# Benchmarking results
.benchmarkBaselines/
```

---

## Building and Running

### Initialize Package

```bash
# Create package structure
mkdir LuauSwift
cd LuauSwift
swift package init --type library --name LuauSwift

# Or create from scratch
# (Use the Package.swift manifest above)
```

### Build

```bash
# Build all targets
swift build

# Build in release mode (optimizations enabled)
swift build -c release

# Build specific target
swift build --target LuauCore
```

### Test

```bash
# Run all tests
swift test

# Run specific test suite
swift test --filter LuauCoreTests

# Run with code coverage
swift test --enable-code-coverage

# Parallel testing
swift test --parallel
```

### Run Executable

```bash
# Run CLI
swift run luau

# Run with arguments
swift run luau script.lua

# Run release build
swift run -c release luau
```

### Generate Xcode Project

```bash
# Open in Xcode
swift package generate-xcodeproj
open LuauSwift.xcodeproj

# Or just open in Xcode directly (Xcode 11+)
open Package.swift
```

---

## Best Practices Summary

### DO ✅

1. **Use semantic versioning** for dependencies
2. **Specify platform versions** explicitly
3. **Organize targets** by dependency hierarchy
4. **Use `.testable` imports** for unit tests
5. **Enable strict concurrency** for Swift 6
6. **Document public APIs** with `///` comments
7. **Use `.copy()` for resources** that shouldn't be processed
8. **Keep executables simple** (thin layer over libraries)

### DON'T ❌

1. **Don't use `.exact()` versions** (breaks ecosystem)
2. **Don't overuse `.unsafeFlags()`** (breaks package ecosystem)
3. **Don't mix test code with source** (use Tests/ directory)
4. **Don't hardcode paths** (use Bundle.module for resources)
5. **Don't depend on executables** (only libraries)
6. **Don't commit `.build/` or `.swiftpm/`** (add to .gitignore)

---

## Migration from Roadmap Structure

The IMPLEMENTATION_ROADMAP.md suggested this structure:
```
LuauSwift/
├── Sources/
│   ├── LuauCore/
│   ├── LuauCompiler/
│   ├── LuauRuntime/
│   └── LuauCLI/
```

**Updated to recommended SPM structure**:
```
LuauSwift/
├── Sources/
│   ├── LuauCore/          # ✅ Keep (library target)
│   ├── LuauCompiler/      # ✅ Keep (library target)
│   ├── LuauRuntime/       # ✅ Keep (library target)
│   ├── LuauStdlib/        # ✅ Add (stdlib should be separate)
│   └── luau/              # ✅ Rename from LuauCLI (executable target)
│       └── main.swift
```

**Changes**:
- ✅ Renamed `LuauCLI` → `luau` (lowercase for executables)
- ✅ Added `LuauStdlib` (separate stdlib from runtime)
- ✅ Executable is directory with `main.swift`, not `main.swift` in root

---

## Next Steps

1. **Create package structure** using this format
2. **Set up Package.swift** with all targets
3. **Create placeholder files** in each target
4. **Verify builds** with `swift build`
5. **Set up CI/CD** (GitHub Actions for testing)
6. **Start implementing** Phase 1 from roadmap

---

## References

- [Swift Package Manager Documentation](https://swift.org/package-manager/)
- [Package.swift API](https://docs.swift.org/package-manager/PackageDescription/)
- [Swift 6.2 Release Notes](https://www.swift.org/blog/swift-6.2-released/)
- [Swift Package Index](https://swiftpackageindex.com/) (for discovering packages)

---

**Ready to create the package!** This structure follows all Apple recommendations and 2025 best practices. 🚀
