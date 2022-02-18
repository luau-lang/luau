// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Location.h"

#include <memory>
#include <vector>

namespace Luau
{

struct AstName;
class AstStat;
class AstNameTable;
struct TypeChecker;
struct Module;
struct HotComment;

using ScopePtr = std::shared_ptr<struct Scope>;

struct LintWarning
{
    // Make sure any new lint codes are documented here: https://luau-lang.org/lint
    // Note that in Studio, the active set of lint warnings is determined by FStringStudioLuauLints
    enum Code
    {
        Code_Unknown = 0,

        Code_UnknownGlobal = 1, // superseded by type checker
        Code_DeprecatedGlobal = 2,
        Code_GlobalUsedAsLocal = 3,
        Code_LocalShadow = 4,       // disabled in Studio
        Code_SameLineStatement = 5, // disabled in Studio
        Code_MultiLineStatement = 6,
        Code_LocalUnused = 7,    // disabled in Studio
        Code_FunctionUnused = 8, // disabled in Studio
        Code_ImportUnused = 9,   // disabled in Studio
        Code_BuiltinGlobalWrite = 10,
        Code_PlaceholderRead = 11,
        Code_UnreachableCode = 12,
        Code_UnknownType = 13,
        Code_ForRange = 14,
        Code_UnbalancedAssignment = 15,
        Code_ImplicitReturn = 16, // disabled in Studio, superseded by type checker in strict mode
        Code_DuplicateLocal = 17,
        Code_FormatString = 18,
        Code_TableLiteral = 19,
        Code_UninitializedLocal = 20,
        Code_DuplicateFunction = 21,
        Code_DeprecatedApi = 22,
        Code_TableOperations = 23,
        Code_DuplicateCondition = 24,
        Code_MisleadingAndOr = 25,
        Code_CommentDirective = 26,

        Code__Count
    };

    Code code;
    Location location;
    std::string text;

    static const char* getName(Code code);
    static Code parseName(const char* name);
    static uint64_t parseMask(const std::vector<HotComment>& hotcomments);
};

struct LintResult
{
    std::vector<LintWarning> errors;
    std::vector<LintWarning> warnings;
};

struct LintOptions
{
    uint64_t warningMask = 0;

    void enableWarning(LintWarning::Code code)
    {
        warningMask |= 1ull << code;
    }
    void disableWarning(LintWarning::Code code)
    {
        warningMask &= ~(1ull << code);
    }

    bool isEnabled(LintWarning::Code code) const
    {
        return 0 != (warningMask & (1ull << code));
    }

    void setDefaults();
};

std::vector<LintWarning> lint(AstStat* root, const AstNameTable& names, const ScopePtr& env, const Module* module,
    const std::vector<HotComment>& hotcomments, const LintOptions& options);

std::vector<AstName> getDeprecatedGlobals(const AstNameTable& names);

} // namespace Luau
