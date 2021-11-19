// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/ParseOptions.h"
#include "Luau/Location.h"
#include "Luau/StringUtils.h"
#include "Luau/Common.h"

namespace Luau
{
class AstStatBlock;
class AstNameTable;
class BytecodeBuilder;
class BytecodeEncoder;

// Note: this structure is duplicated in luacode.h, don't forget to change these in sync!
struct CompileOptions
{
    // 0 - no optimization
    // 1 - baseline optimization level that doesn't prevent debuggability
    // 2 - includes optimizations that harm debuggability such as inlining
    int optimizationLevel = 1;

    // 0 - no debugging support
    // 1 - line info & function names only; sufficient for backtraces
    // 2 - full debug info with local & upvalue names; necessary for debugger
    int debugLevel = 1;

    // 0 - no code coverage support
    // 1 - statement coverage
    // 2 - statement and expression coverage (verbose)
    int coverageLevel = 0;

    // global builtin to construct vectors; disabled by default
    const char* vectorLib = nullptr;
    const char* vectorCtor = nullptr;

    // null-terminated array of globals that are mutable; disables the import optimization for fields accessed through these
    const char** mutableGlobals = nullptr;
};

class CompileError : public std::exception
{
public:
    CompileError(const Location& location, const std::string& message);

    virtual ~CompileError() throw();

    virtual const char* what() const throw();

    const Location& getLocation() const;

    static LUAU_NORETURN void raise(const Location& location, const char* format, ...) LUAU_PRINTF_ATTR(2, 3);

private:
    Location location;
    std::string message;
};

// compiles bytecode into bytecode builder using either a pre-parsed AST or parsing it from source; throws on errors
void compileOrThrow(BytecodeBuilder& bytecode, AstStatBlock* root, const AstNameTable& names, const CompileOptions& options = {});
void compileOrThrow(BytecodeBuilder& bytecode, const std::string& source, const CompileOptions& options = {}, const ParseOptions& parseOptions = {});

// compiles bytecode into a bytecode blob, that either contains the valid bytecode or an encoded error that luau_load can decode
std::string compile(
    const std::string& source, const CompileOptions& options = {}, const ParseOptions& parseOptions = {}, BytecodeEncoder* encoder = nullptr);

} // namespace Luau
