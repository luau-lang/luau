// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeFwd.h"
#include "Luau/Variant.h"
#include "Luau/NotNull.h"

#include <optional>
#include <string>
#include <vector>

namespace Luau
{

namespace TypePath
{

/// Represents a property of a class, table, or anything else with a concept of
/// a named property.
struct Property
{
    /// The name of the property.
    std::string name;
    /// Whether to look at the read or the write type.
    bool isRead = true;

    explicit Property(std::string name);
    Property(std::string name, bool read)
        : name(std::move(name))
        , isRead(read)
    {
    }

    static Property read(std::string name);
    static Property write(std::string name);

    bool operator==(const Property& other) const;
};

/// Represents an index into a type or a pack. For a type, this indexes into a
/// union or intersection's list. For a pack, this indexes into the pack's nth
/// element.
struct Index
{
    /// The 0-based index to use for the lookup.
    size_t index;

    bool operator==(const Index& other) const;
};

/// Represents fields of a type or pack that contain a type.
enum class TypeField
{
    /// The metatable of a type. This could be a metatable type, a primitive
    /// type, a class type, or perhaps even a string singleton type.
    Metatable,
    /// The lower bound of this type, if one is present.
    LowerBound,
    /// The upper bound of this type, if present.
    UpperBound,
    /// The index type.
    IndexLookup,
    /// The indexer result type.
    IndexResult,
    /// The negated type, for negations.
    Negated,
    /// The variadic type for a type pack.
    Variadic,
};

/// Represents fields of a type or type pack that contain a type pack.
enum class PackField
{
    /// What arguments this type accepts.
    Arguments,
    /// What this type returns when called.
    Returns,
    /// The tail of a type pack.
    Tail,
};

/// A single component of a path, representing one inner type or type pack to
/// traverse into.
using Component = Luau::Variant<Property, Index, TypeField, PackField>;

/// A path through a type or type pack accessing a particular type or type pack
/// contained within.
///
/// Paths are always relative; to make use of a Path, you need to specify an
/// entry point. They are not canonicalized; two Paths may not compare equal but
/// may point to the same result, depending on the layout of the entry point.
///
/// Paths always descend through an entry point. This doesn't mean that they
/// cannot reach "upwards" in the actual type hierarchy in some cases, but it
/// does mean that there is no equivalent to `../` in file system paths. This is
/// intentional and unavoidable, because types and type packs don't have a
/// concept of a parent - they are a directed cyclic graph, with no hierarchy
/// that actually holds in all cases.
struct Path
{
    /// The Components of this Path.
    std::vector<Component> components;

    /// Creates a new empty Path.
    Path()
    {
    }

    /// Creates a new Path from a list of components.
    explicit Path(std::vector<Component> components)
        : components(std::move(components))
    {
    }

    /// Creates a new single-component Path.
    explicit Path(Component component)
        : components({component})
    {
    }

    /// Creates a new Path by appending another Path to this one.
    /// @param suffix the Path to append
    /// @return a new Path representing `this + suffix`
    Path append(const Path& suffix) const;

    /// Creates a new Path by appending a Component to this Path.
    /// @param component the Component to append
    /// @return a new Path with `component` appended to it.
    Path push(Component component) const;

    /// Creates a new Path by prepending a Component to this Path.
    /// @param component the Component to prepend
    /// @return a new Path with `component` prepended to it.
    Path push_front(Component component) const;

    /// Creates a new Path by removing the last Component of this Path.
    /// If the Path is empty, this is a no-op.
    /// @return a Path with the last component removed.
    Path pop() const;

    /// Returns the last Component of this Path, if present.
    std::optional<Component> last() const;

    /// Returns whether this Path is empty, meaning it has no components at all.
    /// Traversing an empty Path results in the type you started with.
    bool empty() const;

    bool operator==(const Path& other) const;
    bool operator!=(const Path& other) const
    {
        return !(*this == other);
    }
};

struct PathHash
{
    size_t operator()(const Property& prop) const;
    size_t operator()(const Index& idx) const;
    size_t operator()(const TypeField& field) const;
    size_t operator()(const PackField& field) const;
    size_t operator()(const Component& component) const;
    size_t operator()(const Path& path) const;
};

/// The canonical "empty" Path, meaning a Path with no components.
static const Path kEmpty{};

struct PathBuilder
{
    std::vector<Component> components;

    Path build();

    PathBuilder& readProp(std::string name);
    PathBuilder& writeProp(std::string name);
    PathBuilder& prop(std::string name);
    PathBuilder& index(size_t i);
    PathBuilder& mt();
    PathBuilder& lb();
    PathBuilder& ub();
    PathBuilder& indexKey();
    PathBuilder& indexValue();
    PathBuilder& negated();
    PathBuilder& variadic();
    PathBuilder& args();
    PathBuilder& rets();
    PathBuilder& tail();
};

} // namespace TypePath

using Path = TypePath::Path;

/// Converts a Path to a string for debugging purposes. This output may not be
/// terribly clear to end users of the Luau type system.
std::string toString(const TypePath::Path& path, bool prefixDot = false);

std::optional<TypeOrPack> traverse(TypeId root, const Path& path, NotNull<BuiltinTypes> builtinTypes);
std::optional<TypeOrPack> traverse(TypePackId root, const Path& path, NotNull<BuiltinTypes> builtinTypes);

/// Traverses a path from a type to its end point, which must be a type.
/// @param root the entry point of the traversal
/// @param path the path to traverse
/// @param builtinTypes the built-in types in use (used to acquire the string metatable)
/// @returns the TypeId at the end of the path, or nullopt if the traversal failed.
std::optional<TypeId> traverseForType(TypeId root, const Path& path, NotNull<BuiltinTypes> builtinTypes);

/// Traverses a path from a type pack to its end point, which must be a type.
/// @param root the entry point of the traversal
/// @param path the path to traverse
/// @param builtinTypes the built-in types in use (used to acquire the string metatable)
/// @returns the TypeId at the end of the path, or nullopt if the traversal failed.
std::optional<TypeId> traverseForType(TypePackId root, const Path& path, NotNull<BuiltinTypes> builtinTypes);

/// Traverses a path from a type to its end point, which must be a type pack.
/// @param root the entry point of the traversal
/// @param path the path to traverse
/// @param builtinTypes the built-in types in use (used to acquire the string metatable)
/// @returns the TypePackId at the end of the path, or nullopt if the traversal failed.
std::optional<TypePackId> traverseForPack(TypeId root, const Path& path, NotNull<BuiltinTypes> builtinTypes);

/// Traverses a path from a type pack to its end point, which must be a type pack.
/// @param root the entry point of the traversal
/// @param path the path to traverse
/// @param builtinTypes the built-in types in use (used to acquire the string metatable)
/// @returns the TypePackId at the end of the path, or nullopt if the traversal failed.
std::optional<TypePackId> traverseForPack(TypePackId root, const Path& path, NotNull<BuiltinTypes> builtinTypes);

} // namespace Luau
