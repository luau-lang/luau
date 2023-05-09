// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Type.h"
#include "Luau/Unifiable.h"
#include "Luau/Variant.h"

#include <optional>
#include <set>

namespace Luau
{

struct TypeArena;
struct TxnLog;

struct TypePack;
struct VariadicTypePack;
struct BlockedTypePack;

struct TypePackVar;
using TypePackId = const TypePackVar*;

struct FreeTypePack
{
    explicit FreeTypePack(TypeLevel level);
    explicit FreeTypePack(Scope* scope);
    FreeTypePack(Scope* scope, TypeLevel level);

    int index;
    TypeLevel level;
    Scope* scope = nullptr;
};

struct GenericTypePack
{
    // By default, generics are global, with a synthetic name
    GenericTypePack();
    explicit GenericTypePack(TypeLevel level);
    explicit GenericTypePack(const Name& name);
    explicit GenericTypePack(Scope* scope);
    GenericTypePack(TypeLevel level, const Name& name);
    GenericTypePack(Scope* scope, const Name& name);

    int index;
    TypeLevel level;
    Scope* scope = nullptr;
    Name name;
    bool explicitName = false;
};

using BoundTypePack = Unifiable::Bound<TypePackId>;

using ErrorTypePack = Unifiable::Error;

using TypePackVariant = Unifiable::Variant<TypePackId, FreeTypePack, GenericTypePack, TypePack, VariadicTypePack, BlockedTypePack>;

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
    bool hidden = false; // if true, we don't display this when toString()ing a pack with this variadic as its tail.
};

/**
 * Analogous to a BlockedType.
 */
struct BlockedTypePack
{
    BlockedTypePack();
    size_t index;

    static size_t nextIndex;
};

struct TypePackVar
{
    explicit TypePackVar(const TypePackVariant& ty);
    explicit TypePackVar(TypePackVariant&& ty);
    TypePackVar(TypePackVariant&& ty, bool persistent);

    bool operator==(const TypePackVar& rhs) const;

    TypePackVar& operator=(TypePackVariant&& tp);

    TypePackVar& operator=(const TypePackVar& rhs);

    // Re-assignes the content of the pack, but doesn't change the owning arena and can't make pack persistent.
    void reassign(const TypePackVar& rhs)
    {
        ty = rhs.ty;
    }

    TypePackVariant ty;

    bool persistent = false;

    // Pointer to the type arena that allocated this pack.
    TypeArena* owningArena = nullptr;
};

/* Walk the set of TypeIds in a TypePack.
 *
 * Like Types, individual TypePacks can be free, generic, or any.
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

    const TxnLog* log;
};

TypePackIterator begin(TypePackId tp);
TypePackIterator begin(TypePackId tp, const TxnLog* log);
TypePackIterator end(TypePackId tp);

using SeenSet = std::set<std::pair<const void*, const void*>>;

bool areEqual(SeenSet& seen, const TypePackVar& lhs, const TypePackVar& rhs);

TypePackId follow(TypePackId tp);
TypePackId follow(TypePackId t, const void* context, TypePackId (*mapper)(const void*, TypePackId));

size_t size(TypePackId tp, TxnLog* log = nullptr);
bool finite(TypePackId tp, TxnLog* log = nullptr);
size_t size(const TypePack& tp, TxnLog* log = nullptr);
std::optional<TypeId> first(TypePackId tp, bool ignoreHiddenVariadics = true);

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
std::pair<std::vector<TypeId>, std::optional<TypePackId>> flatten(TypePackId tp, const TxnLog& log);

/// Returs true if the type pack arose from a function that is declared to be variadic.
/// Returns *false* for function argument packs that are inferred to be safe to oversaturate!
bool isVariadic(TypePackId tp);
bool isVariadic(TypePackId tp, const TxnLog& log);

// Returns true if the TypePack is Generic or Variadic.  Does not walk TypePacks!!
bool isVariadicTail(TypePackId tp, const TxnLog& log, bool includeHiddenVariadics = false);

bool containsNever(TypePackId tp);

} // namespace Luau
