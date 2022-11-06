// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Common.h"

#include <string>

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

namespace Luau
{

struct Symbol
{
    Symbol()
        : local(nullptr)
        , global()
    {
    }

    Symbol(AstLocal* local)
        : local(local)
        , global()
    {
    }

    Symbol(const AstName& global)
        : local(nullptr)
        , global(global)
    {
    }

    template<typename T>
    Symbol(const T&) = delete;

    AstLocal* local;
    AstName global;

    bool operator==(const Symbol& rhs) const
    {
        if (local)
            return local == rhs.local;
        else if (global.value)
            return rhs.global.value && global == rhs.global.value; // Subtlety: AstName::operator==(const char*) uses strcmp, not pointer identity.
        else if (FFlag::DebugLuauDeferredConstraintResolution)
            return !rhs.local && !rhs.global.value; // Reflexivity: we already know `this` Symbol is empty, so check that rhs is.
        else
            return false;
    }

    bool operator!=(const Symbol& rhs) const
    {
        return !(*this == rhs);
    }

    bool operator<(const Symbol& rhs) const
    {
        if (local && rhs.local)
            return local < rhs.local;
        else if (global.value && rhs.global.value)
            return global < rhs.global;
        else if (local)
            return true;

        return false;
    }

    AstName astName() const
    {
        if (local)
            return local->name;

        LUAU_ASSERT(global.value);
        return global;
    }

    const char* c_str() const
    {
        if (local)
            return local->name.value;

        LUAU_ASSERT(global.value);
        return global.value;
    }
};

std::string toString(const Symbol& name);

} // namespace Luau

namespace std
{
template<>
struct hash<Luau::Symbol>
{
    std::size_t operator()(const Luau::Symbol& s) const noexcept
    {
        return std::hash<const Luau::AstLocal*>()(s.local) ^ (s.global.value ? std::hash<std::string_view>()(s.global.value) : 0);
    }
};
} // namespace std
