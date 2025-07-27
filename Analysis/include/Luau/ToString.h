// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/DenseHash.h"
#include "Luau/TypeFwd.h"

#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

LUAU_FASTINT(LuauTableTypeMaximumStringifierLength)
LUAU_FASTINT(LuauTypeMaximumStringifierLength)

namespace Luau
{

class AstExpr;

struct Scope;

struct Constraint;

struct Position;
struct Location;

struct ToStringNameMap
{
    std::unordered_map<TypeId, std::string> types;
    std::unordered_map<TypePackId, std::string> typePacks;
};

struct ToStringOptions
{
    ToStringOptions(bool exhaustive = false)
        : exhaustive(exhaustive)
    {
    }

    bool exhaustive = false;                      // If true, we produce complete output rather than comprehensible output
    bool useLineBreaks = false;                   // If true, we insert new lines to separate long results such as table entries/metatable.
    bool functionTypeArguments = false;           // If true, output function type argument names when they are available
    bool hideTableKind = false;                   // If true, all tables will be surrounded with plain '{}'
    bool hideNamedFunctionTypeParameters = false; // If true, type parameters of functions will be hidden at top-level.
    bool hideFunctionSelfArgument = false;        // If true, `self: X` will be omitted from the function signature if the function has self
    bool hideTableAliasExpansions = false;        // If true, all table aliases will not be expanded
    bool useQuestionMarks = true;                 // If true, use a postfix ? for options, else write them out as unions that include nil.
    size_t maxTableLength = size_t(FInt::LuauTableTypeMaximumStringifierLength); // Only applied to TableTypes
    size_t maxTypeLength = size_t(FInt::LuauTypeMaximumStringifierLength);
    size_t compositeTypesSingleLineLimit = 5; // The number of type elements permitted on a single line when printing type unions/intersections
    ToStringNameMap nameMap;
    std::shared_ptr<Scope> scope; // If present, module names will be added and types that are not available in scope will be marked as 'invalid'
    std::vector<std::string> namedFunctionOverrideArgNames; // If present, named function argument names will be overridden
};

struct ToStringResult
{
    std::string name;

    bool invalid = false;
    bool error = false;
    bool cycle = false;
    bool truncated = false;
};

ToStringResult toStringDetailed(TypeId ty, ToStringOptions& opts);
ToStringResult toStringDetailed(TypePackId ty, ToStringOptions& opts);

std::string toString(TypeId ty, ToStringOptions& opts);
std::string toString(TypePackId ty, ToStringOptions& opts);

// These overloads are selected when a temporary ToStringOptions is passed. (eg
// via an initializer list)
inline std::string toString(TypePackId ty, ToStringOptions&& opts)
{
    // Delegate to the overload (TypePackId, ToStringOptions&)
    return toString(ty, opts);
}
inline std::string toString(TypeId ty, ToStringOptions&& opts)
{
    // Delegate to the overload (TypeId, ToStringOptions&)
    return toString(ty, opts);
}

// These are offered as overloads rather than a default parameter so that they can be easily invoked from within the MSVC debugger.
// You can use them in watch expressions!
inline std::string toString(TypeId ty)
{
    return toString(ty, ToStringOptions{});
}
inline std::string toString(TypePackId ty)
{
    return toString(ty, ToStringOptions{});
}

std::string toString(const Constraint& c, ToStringOptions& opts);

inline std::string toString(const Constraint& c, ToStringOptions&& opts)
{
    return toString(c, opts);
}

std::string toString(const Constraint& c);

std::string toString(const Type& tv, ToStringOptions& opts);
std::string toString(const TypePackVar& tp, ToStringOptions& opts);

inline std::string toString(const Type& tv)
{
    ToStringOptions opts;
    return toString(tv, opts);
}

inline std::string toString(const TypePackVar& tp)
{
    ToStringOptions opts;
    return toString(tp, opts);
}

std::string toStringNamedFunction(const std::string& funcName, const FunctionType& ftv, ToStringOptions& opts);

inline std::string toStringNamedFunction(const std::string& funcName, const FunctionType& ftv)
{
    ToStringOptions opts;
    return toStringNamedFunction(funcName, ftv, opts);
}

std::optional<std::string> getFunctionNameAsString(const AstExpr& expr);

// It could be useful to see the text representation of a type during a debugging session instead of exploring the content of the class
// These functions will dump the type to stdout and can be evaluated in Watch/Immediate windows or as gdb/lldb expression
std::string dump(TypeId ty);
std::string dump(const std::optional<TypeId>& ty);
std::string dump(TypePackId ty);
std::string dump(const std::optional<TypePackId>& ty);
std::string dump(const std::vector<TypeId>& types);
std::string dump(DenseHashMap<TypeId, TypeId>& types);
std::string dump(DenseHashMap<TypePackId, TypePackId>& types);

std::string dump(const Constraint& c);

std::string dump(const std::shared_ptr<Scope>& scope, const char* name);

std::string generateName(size_t n);

std::string toString(const Position& position);
std::string toString(const Location& location, int offset = 0, bool useBegin = true);

std::string toString(const TypeOrPack& tyOrTp, ToStringOptions& opts);

inline std::string toString(const TypeOrPack& tyOrTp)
{
    ToStringOptions opts{};
    return toString(tyOrTp, opts);
}

std::string dump(const TypeOrPack& tyOrTp);

std::string toStringVector(const std::vector<TypeId>& types, ToStringOptions& opts);
} // namespace Luau
