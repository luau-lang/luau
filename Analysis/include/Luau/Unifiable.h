// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Variant.h"

#include <string>

namespace Luau
{

/**
 * The 'level' of a TypeVar is an indirect way to talk about the scope that it 'belongs' too.
 * To start, read http://okmij.org/ftp/ML/generalization.html
 *
 * We extend the idea by adding a "sub-level" which helps us to differentiate sibling scopes
 * within a single larger scope.
 *
 * We need this because we try to prototype functions and add them to the type environment before
 * we check the function bodies.  This allows us to properly typecheck many scenarios where there
 * is no single good order in which to typecheck a program.
 */
struct TypeLevel
{
    int level = 0;
    int subLevel = 0;

    // Returns true if the level of "this" belongs to an equal or larger scope than that of rhs
    bool subsumes(const TypeLevel& rhs) const
    {
        if (level < rhs.level)
            return true;
        if (level > rhs.level)
            return false;
        if (subLevel == rhs.subLevel)
            return true; // if level == rhs.level and subLevel == rhs.subLevel, then they are the exact same TypeLevel

        // Sibling TypeLevels (that is, TypeLevels that share a level but have a different subLevel) are not considered to subsume one another
        return false;
    }

    // Returns true if the level of "this" belongs to a larger (not equal) scope than that of rhs
    bool subsumesStrict(const TypeLevel& rhs) const
    {
        if (level == rhs.level && subLevel == rhs.subLevel)
            return false;
        else
            return subsumes(rhs);
    }

    TypeLevel incr() const
    {
        TypeLevel result;
        result.level = level + 1;
        result.subLevel = 0;
        return result;
    }
};

inline TypeLevel min(const TypeLevel& a, const TypeLevel& b)
{
    if (a.subsumes(b))
        return a;
    else
        return b;
}

namespace Unifiable
{

using Name = std::string;

struct Free
{
    explicit Free(TypeLevel level);

    int index;
    TypeLevel level;
    // True if this free type variable is part of a mutually
    // recursive type alias whose definitions haven't been
    // resolved yet.
    bool forwardedTypeAlias = false;

private:
    static int nextIndex;
};

template<typename Id>
struct Bound
{
    explicit Bound(Id boundTo)
        : boundTo(boundTo)
    {
    }

    Id boundTo;
};

struct Generic
{
    // By default, generics are global, with a synthetic name
    Generic();
    explicit Generic(TypeLevel level);
    explicit Generic(const Name& name);
    Generic(TypeLevel level, const Name& name);

    int index;
    TypeLevel level;
    Name name;
    bool explicitName;

private:
    static int nextIndex;
};

struct Error
{
    // This constructor has to be public, since it's used in TypeVar and TypePack,
    // but shouldn't be called directly. Please use errorRecoveryType() instead.
    Error();

    int index;

private:
    static int nextIndex;
};

template<typename Id, typename... Value>
using Variant = Variant<Free, Bound<Id>, Generic, Error, Value...>;

} // namespace Unifiable
} // namespace Luau
