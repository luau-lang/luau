// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/Ast.h"
#include "lluz/Common.h"

#include <string>

namespace lluz
{

// TODO Rename this to Name once the old type alias is gone.
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
        if (global.value)
            return rhs.global.value && global == rhs.global.value; // Subtlety: AstName::operator==(const char*) uses strcmp, not pointer identity.
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
        else
            return false;
    }

    AstName astName() const
    {
        if (local)
            return local->name;

        lluz_ASSERT(global.value);
        return global;
    }

    const char* c_str() const
    {
        if (local)
            return local->name.value;

        lluz_ASSERT(global.value);
        return global.value;
    }
};

std::string toString(const Symbol& name);

} // namespace lluz

namespace std
{
template<>
struct hash<lluz::Symbol>
{
    std::size_t operator()(const lluz::Symbol& s) const noexcept
    {
        return std::hash<const lluz::AstLocal*>()(s.local) ^ (s.global.value ? std::hash<std::string_view>()(s.global.value) : 0);
    }
};
} // namespace std
