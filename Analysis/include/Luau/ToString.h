// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/TypeVar.h"
#include "Luau/ConstraintGraphBuilder.h"

#include <unordered_map>
#include <optional>
#include <memory>
#include <string>

LUAU_FASTINT(LuauTableTypeMaximumStringifierLength)
LUAU_FASTINT(LuauTypeMaximumStringifierLength)

namespace Luau
{

struct ToStringNameMap
{
    std::unordered_map<TypeId, std::string> typeVars;
    std::unordered_map<TypePackId, std::string> typePacks;
};

struct ToStringOptions
{
    bool exhaustive = false;                      // If true, we produce complete output rather than comprehensible output
    bool useLineBreaks = false;                   // If true, we insert new lines to separate long results such as table entries/metatable.
    bool functionTypeArguments = false;           // If true, output function type argument names when they are available
    bool hideTableKind = false;                   // If true, all tables will be surrounded with plain '{}'
    bool hideNamedFunctionTypeParameters = false; // If true, type parameters of functions will be hidden at top-level.
    bool hideFunctionSelfArgument = false;        // If true, `self: X` will be omitted from the function signature if the function has self
    bool indent = false;
    size_t maxTableLength = size_t(FInt::LuauTableTypeMaximumStringifierLength); // Only applied to TableTypeVars
    size_t maxTypeLength = size_t(FInt::LuauTypeMaximumStringifierLength);
    std::optional<ToStringNameMap> nameMap;
    std::shared_ptr<Scope> scope; // If present, module names will be added and types that are not available in scope will be marked as 'invalid'
    std::vector<std::string> namedFunctionOverrideArgNames; // If present, named function argument names will be overridden
};

struct ToStringResult
{
    std::string name;
    ToStringNameMap nameMap;

    bool invalid = false;
    bool error = false;
    bool cycle = false;
    bool truncated = false;
};

ToStringResult toStringDetailed(TypeId ty, const ToStringOptions& opts = {});
ToStringResult toStringDetailed(TypePackId ty, const ToStringOptions& opts = {});

std::string toString(TypeId ty, const ToStringOptions& opts);
std::string toString(TypePackId ty, const ToStringOptions& opts);
std::string toString(const Constraint& c, ToStringOptions& opts);

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
inline std::string toString(const Constraint& c)
{
    ToStringOptions opts;
    return toString(c, opts);
}

std::string toString(const TypeVar& tv, const ToStringOptions& opts = {});
std::string toString(const TypePackVar& tp, const ToStringOptions& opts = {});

std::string toStringNamedFunction(const std::string& funcName, const FunctionTypeVar& ftv, const ToStringOptions& opts = {});

// It could be useful to see the text representation of a type during a debugging session instead of exploring the content of the class
// These functions will dump the type to stdout and can be evaluated in Watch/Immediate windows or as gdb/lldb expression
std::string dump(TypeId ty);
std::string dump(TypePackId ty);
std::string dump(const Constraint& c);

std::string dump(const std::shared_ptr<Scope>& scope, const char* name);

std::string generateName(size_t n);

} // namespace Luau
