// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeVar.h"
#include "Luau/Unifiable.h"
#include "Luau/Variant.h"

#include <optional>
#include <set>

namespace Luau
{

struct TypeArena;

struct TypePack;
struct VariadicTypePack;

struct TypePackVar;

struct TxnLog;

using TypePackId = const TypePackVar*;
using FreeTypePack = Unifiable::Free;
using BoundTypePack = Unifiable::Bound<TypePackId>;
using GenericTypePack = Unifiable::Generic;
using TypePackVariant = Unifiable::Variant<TypePackId, TypePack, VariadicTypePack>;

/* A TypePack is a rope-like string of TypeIds.  We use this structure to encode
 * notions like packs of unknown length and packs of any length, as well as more
 * nuanced compositions like "a pack which is a number prepended to this other pack,"
 * or "a pack that is 2 numbers followed by any number of any other types."
 */
struct TypePack
{
    std::vector<TypeId> head;
    std::optional<TypePackId> tail;
};

struct VariadicTypePack
{
    TypeId ty;
};

struct TypePackVar
{
    explicit TypePackVar(const TypePackVariant& ty);
    explicit TypePackVar(TypePackVariant&& ty);
    TypePackVar(TypePackVariant&& ty, bool persistent);
    bool operator==(const TypePackVar& rhs) const;
    TypePackVar& operator=(TypePackVariant&& tp);

    TypePackVariant ty;
    bool persistent = false;

    // Pointer to the type arena that allocated this type.
    // Do not depend on the value of this under any circumstances. This is for
    // debugging purposes only. This is only set in debug builds; it is nullptr
    // in all other environments.
    TypeArena* owningArena = nullptr;
};

/* Walk the set of TypeIds in a TypePack.
 *
 * Like TypeVars, individual TypePacks can be free, generic, or any.
 *
 * We afford the ability to work with these kinds of packs by giving the
 * iterator a .tail() property that yields the tail-most TypePack in the
 * rope.
 *
 * It is very commonplace to want to walk each type in a pack, then handle
 * the tail specially.  eg when checking parameters, it might be the case
 * that the parameter pack ends with a VariadicTypePack.  In this case, we
 * want to allow any number of extra arguments.
 *
 * The iterator obtained by calling end(tp) does not have a .tail(), but is
 * equivalent with end(tp2) for any two type packs.
 */
struct TypePackIterator
{
    using value_type = Luau::TypeId;
    using pointer = value_type*;
    using reference = value_type&;
    using difference_type = size_t;
    using iterator_category = std::input_iterator_tag;

    TypePackIterator() = default;
    explicit TypePackIterator(TypePackId tp);
    TypePackIterator(TypePackId tp, const TxnLog* log);

    TypePackIterator& operator++();
    TypePackIterator operator++(int);
    bool operator!=(const TypePackIterator& rhs);
    bool operator==(const TypePackIterator& rhs);

    const TypeId& operator*();

    /** Return the tail of a TypePack.
     * This may *only* be called on an iterator that has been incremented to the end.
     * Returns nullopt if the pack has fixed length.
     */
    std::optional<TypePackId> tail();

    friend TypePackIterator end(TypePackId tp);

private:
    TypePackId currentTypePack = nullptr;
    const TypePack* tp = nullptr;
    size_t currentIndex = 0;

    // Only used if LuauUseCommittingTxnLog is true.
    const TxnLog* log;
};

TypePackIterator begin(TypePackId tp);
TypePackIterator begin(TypePackId tp, TxnLog* log);
TypePackIterator end(TypePackId tp);

using SeenSet = std::set<std::pair<void*, void*>>;

bool areEqual(SeenSet& seen, const TypePackVar& lhs, const TypePackVar& rhs);

TypePackId follow(TypePackId tp);
TypePackId follow(TypePackId tp, std::function<TypePackId(TypePackId)> mapper);

size_t size(TypePackId tp);
bool finite(TypePackId tp);
size_t size(const TypePack& tp);
std::optional<TypeId> first(TypePackId tp);

TypePackVar* asMutable(TypePackId tp);
TypePack* asMutable(const TypePack* tp);

template<typename T>
const T* get(TypePackId tp)
{
    LUAU_ASSERT(tp);

    if constexpr (!std::is_same_v<T, BoundTypePack>)
        LUAU_ASSERT(get_if<BoundTypePack>(&tp->ty) == nullptr);

    return get_if<T>(&(tp->ty));
}

template<typename T>
T* getMutable(TypePackId tp)
{
    LUAU_ASSERT(tp);

    if constexpr (!std::is_same_v<T, BoundTypePack>)
        LUAU_ASSERT(get_if<BoundTypePack>(&tp->ty) == nullptr);

    return get_if<T>(&(asMutable(tp)->ty));
}

/// Returns true if the type pack is known to be empty (no types in the head and no/an empty tail).
bool isEmpty(TypePackId tp);

/// Flattens out a type pack.  Also returns a valid TypePackId tail if the type pack's full size is not known
std::pair<std::vector<TypeId>, std::optional<TypePackId>> flatten(TypePackId tp);

} // namespace Luau
