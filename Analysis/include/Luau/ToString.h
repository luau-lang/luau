// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/TypeVar.h"

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
    bool exhaustive = false;            // If true, we produce complete output rather than comprehensible output
    bool useLineBreaks = false;         // If true, we insert new lines to separate long results such as table entries/metatable.
    bool functionTypeArguments = false; // If true, output function type argument names when they are available
    bool hideTableKind = false;         // If true, all tables will be surrounded with plain '{}'
    size_t maxTableLength = size_t(FInt::LuauTableTypeMaximumStringifierLength); // Only applied to TableTypeVars
    size_t maxTypeLength = size_t(FInt::LuauTypeMaximumStringifierLength);
    std::optional<ToStringNameMap> nameMap;
    std::shared_ptr<Scope> scope; // If present, module names will be added and types that are not available in scope will be marked as 'invalid'
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

std::string toString(const TypeVar& tv, const ToStringOptions& opts = {});
std::string toString(const TypePackVar& tp, const ToStringOptions& opts = {});

// It could be useful to see the text representation of a type during a debugging session instead of exploring the content of the class
// These functions will dump the type to stdout and can be evaluated in Watch/Immediate windows or as gdb/lldb expression
void dump(TypeId ty);
void dump(TypePackId ty);

} // namespace Luau
