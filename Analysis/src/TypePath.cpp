// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypePath.h"
#include "Luau/Common.h"
#include "Luau/DenseHash.h"
#include "Luau/Type.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypePack.h"
#include "Luau/TypeOrPack.h"

#include <functional>
#include <optional>
#include <sstream>
#include <type_traits>

LUAU_FASTFLAG(DebugLuauReadWriteProperties);

// Maximum number of steps to follow when traversing a path. May not always
// equate to the number of components in a path, depending on the traversal
// logic.
LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypePathMaximumTraverseSteps, 100);

namespace Luau
{

namespace TypePath
{

Property::Property(std::string name)
    : name(std::move(name))
{
    LUAU_ASSERT(!FFlag::DebugLuauReadWriteProperties);
}

Property Property::read(std::string name)
{
    return Property(std::move(name), true);
}

Property Property::write(std::string name)
{
    return Property(std::move(name), false);
}

bool Property::operator==(const Property& other) const
{
    return name == other.name && isRead == other.isRead;
}

bool Index::operator==(const Index& other) const
{
    return index == other.index;
}

Path Path::append(const Path& suffix) const
{
    std::vector<Component> joined(components);
    joined.reserve(suffix.components.size());
    joined.insert(joined.end(), suffix.components.begin(), suffix.components.end());
    return Path(std::move(joined));
}

Path Path::push(Component component) const
{
    std::vector<Component> joined(components);
    joined.push_back(component);
    return Path(std::move(joined));
}

Path Path::push_front(Component component) const
{
    std::vector<Component> joined{};
    joined.reserve(components.size() + 1);
    joined.push_back(std::move(component));
    joined.insert(joined.end(), components.begin(), components.end());
    return Path(std::move(joined));
}

Path Path::pop() const
{
    if (empty())
        return kEmpty;

    std::vector<Component> popped(components);
    popped.pop_back();
    return Path(std::move(popped));
}

std::optional<Component> Path::last() const
{
    if (empty())
        return std::nullopt;

    return components.back();
}

bool Path::empty() const
{
    return components.empty();
}

bool Path::operator==(const Path& other) const
{
    return components == other.components;
}

size_t PathHash::operator()(const Property& prop) const
{
    return std::hash<std::string>()(prop.name) ^ static_cast<size_t>(prop.isRead);
}

size_t PathHash::operator()(const Index& idx) const
{
    return idx.index;
}

size_t PathHash::operator()(const TypeField& field) const
{
    return static_cast<size_t>(field);
}

size_t PathHash::operator()(const PackField& field) const
{
    return static_cast<size_t>(field);
}

size_t PathHash::operator()(const Component& component) const
{
    return visit(*this, component);
}

size_t PathHash::operator()(const Path& path) const
{
    size_t hash = 0;

    for (const Component& component : path.components)
        hash ^= (*this)(component);

    return hash;
}

Path PathBuilder::build()
{
    return Path(std::move(components));
}

PathBuilder& PathBuilder::readProp(std::string name)
{
    LUAU_ASSERT(FFlag::DebugLuauReadWriteProperties);
    components.push_back(Property{std::move(name), true});
    return *this;
}

PathBuilder& PathBuilder::writeProp(std::string name)
{
    LUAU_ASSERT(FFlag::DebugLuauReadWriteProperties);
    components.push_back(Property{std::move(name), false});
    return *this;
}

PathBuilder& PathBuilder::prop(std::string name)
{
    LUAU_ASSERT(!FFlag::DebugLuauReadWriteProperties);
    components.push_back(Property{std::move(name)});
    return *this;
}

PathBuilder& PathBuilder::index(size_t i)
{
    components.push_back(Index{i});
    return *this;
}

PathBuilder& PathBuilder::mt()
{
    components.push_back(TypeField::Metatable);
    return *this;
}

PathBuilder& PathBuilder::lb()
{
    components.push_back(TypeField::LowerBound);
    return *this;
}

PathBuilder& PathBuilder::ub()
{
    components.push_back(TypeField::UpperBound);
    return *this;
}

PathBuilder& PathBuilder::indexKey()
{
    components.push_back(TypeField::IndexLookup);
    return *this;
}

PathBuilder& PathBuilder::indexValue()
{
    components.push_back(TypeField::IndexResult);
    return *this;
}

PathBuilder& PathBuilder::negated()
{
    components.push_back(TypeField::Negated);
    return *this;
}

PathBuilder& PathBuilder::variadic()
{
    components.push_back(TypeField::Variadic);
    return *this;
}

PathBuilder& PathBuilder::args()
{
    components.push_back(PackField::Arguments);
    return *this;
}

PathBuilder& PathBuilder::rets()
{
    components.push_back(PackField::Returns);
    return *this;
}

PathBuilder& PathBuilder::tail()
{
    components.push_back(PackField::Tail);
    return *this;
}

} // namespace TypePath

namespace
{

struct TraversalState
{
    TraversalState(TypeId root, NotNull<BuiltinTypes> builtinTypes)
        : current(root)
        , builtinTypes(builtinTypes)
    {
    }
    TraversalState(TypePackId root, NotNull<BuiltinTypes> builtinTypes)
        : current(root)
        , builtinTypes(builtinTypes)
    {
    }

    TypeOrPack current;
    NotNull<BuiltinTypes> builtinTypes;
    int steps = 0;

    void updateCurrent(TypeId ty)
    {
        LUAU_ASSERT(ty);
        current = follow(ty);
    }

    void updateCurrent(TypePackId tp)
    {
        LUAU_ASSERT(tp);
        current = follow(tp);
    }

    bool tooLong()
    {
        return ++steps > DFInt::LuauTypePathMaximumTraverseSteps;
    }

    bool checkInvariants()
    {
        return tooLong();
    }

    bool traverse(const TypePath::Property& property)
    {
        auto currentType = get<TypeId>(current);
        if (!currentType)
            return false;

        if (checkInvariants())
            return false;

        const Property* prop = nullptr;

        if (auto t = get<TableType>(*currentType))
        {
            auto it = t->props.find(property.name);
            if (it != t->props.end())
            {
                prop = &it->second;
            }
        }
        else if (auto c = get<ClassType>(*currentType))
        {
            prop = lookupClassProp(c, property.name);
        }
        // For a metatable type, the table takes priority; check that before
        // falling through to the metatable entry below.
        else if (auto m = get<MetatableType>(*currentType))
        {
            TypeOrPack pinned = current;
            updateCurrent(m->table);

            if (traverse(property))
                return true;

            // Restore the old current type if we didn't traverse the metatable
            // successfully; we'll use the next branch to address this.
            current = pinned;
        }

        if (!prop)
        {
            if (auto m = getMetatable(*currentType, builtinTypes))
            {
                // Weird: rather than use findMetatableEntry, which requires a lot
                // of stuff that we don't have and don't want to pull in, we use the
                // path traversal logic to grab __index and then re-enter the lookup
                // logic there.
                updateCurrent(*m);

                if (!traverse(TypePath::Property{"__index"}))
                    return false;

                return traverse(property);
            }
        }

        if (prop)
        {
            std::optional<TypeId> maybeType;
            if (FFlag::DebugLuauReadWriteProperties)
                maybeType = property.isRead ? prop->readType() : prop->writeType();
            else
                maybeType = prop->type();

            if (maybeType)
            {
                updateCurrent(*maybeType);
                return true;
            }
        }

        return false;
    }

    bool traverse(const TypePath::Index& index)
    {
        if (checkInvariants())
            return false;

        if (auto currentType = get<TypeId>(current))
        {
            if (auto u = get<UnionType>(*currentType))
            {
                auto it = begin(u);
                std::advance(it, index.index);
                if (it != end(u))
                {
                    updateCurrent(*it);
                    return true;
                }
            }
            else if (auto i = get<IntersectionType>(*currentType))
            {
                auto it = begin(i);
                std::advance(it, index.index);
                if (it != end(i))
                {
                    updateCurrent(*it);
                    return true;
                }
            }
        }
        else
        {
            auto currentPack = get<TypePackId>(current);
            LUAU_ASSERT(currentPack);
            if (get<TypePack>(*currentPack))
            {
                auto it = begin(*currentPack);

                for (size_t i = 0; i < index.index && it != end(*currentPack); ++i)
                    ++it;

                if (it != end(*currentPack))
                {
                    updateCurrent(*it);
                    return true;
                }
            }
        }

        return false;
    }

    bool traverse(TypePath::TypeField field)
    {
        if (checkInvariants())
            return false;

        switch (field)
        {
        case TypePath::TypeField::Metatable:
            if (auto currentType = get<TypeId>(current))
            {
                if (std::optional<TypeId> mt = getMetatable(*currentType, builtinTypes))
                {
                    updateCurrent(*mt);
                    return true;
                }
            }

            return false;
        case TypePath::TypeField::LowerBound:
        case TypePath::TypeField::UpperBound:
            if (auto ft = get<FreeType>(current))
            {
                updateCurrent(field == TypePath::TypeField::LowerBound ? ft->lowerBound : ft->upperBound);
                return true;
            }

            return false;
        case TypePath::TypeField::IndexLookup:
        case TypePath::TypeField::IndexResult:
        {
            const TableIndexer* indexer = nullptr;

            if (auto tt = get<TableType>(current); tt && tt->indexer)
                indexer = &(*tt->indexer);
            // Note: we don't appear to walk the class hierarchy for indexers
            else if (auto ct = get<ClassType>(current); ct && ct->indexer)
                indexer = &(*ct->indexer);

            if (indexer)
            {
                updateCurrent(field == TypePath::TypeField::IndexLookup ? indexer->indexType : indexer->indexResultType);
                return true;
            }

            return false;
        }
        case TypePath::TypeField::Negated:
            if (auto nt = get<NegationType>(current))
            {
                updateCurrent(nt->ty);
                return true;
            }

            return false;
        case TypePath::TypeField::Variadic:
            if (auto vtp = get<VariadicTypePack>(current))
            {
                updateCurrent(vtp->ty);
                return true;
            }

            return false;
        }

        return false;
    }

    bool traverse(TypePath::PackField field)
    {
        if (checkInvariants())
            return false;

        switch (field)
        {
        case TypePath::PackField::Arguments:
        case TypePath::PackField::Returns:
            if (auto ft = get<FunctionType>(current))
            {
                updateCurrent(field == TypePath::PackField::Arguments ? ft->argTypes : ft->retTypes);
                return true;
            }

            return false;
        case TypePath::PackField::Tail:
            if (auto currentPack = get<TypePackId>(current))
            {
                auto it = begin(*currentPack);
                while (it != end(*currentPack))
                    ++it;

                if (auto tail = it.tail())
                {
                    updateCurrent(*tail);
                    return true;
                }
            }

            return false;
        }

        return false;
    }
};

} // namespace

std::string toString(const TypePath::Path& path, bool prefixDot)
{
    std::stringstream result;
    bool first = true;

    auto strComponent = [&](auto&& c) {
        using T = std::decay_t<decltype(c)>;
        if constexpr (std::is_same_v<T, TypePath::Property>)
        {
            result << '[';
            if (FFlag::DebugLuauReadWriteProperties)
            {
                if (c.isRead)
                    result << "read ";
                else
                    result << "write ";
            }

            result << '"' << c.name << '"' << ']';
        }
        else if constexpr (std::is_same_v<T, TypePath::Index>)
        {
            result << '[' << std::to_string(c.index) << ']';
        }
        else if constexpr (std::is_same_v<T, TypePath::TypeField>)
        {
            if (!first || prefixDot)
                result << '.';

            switch (c)
            {
            case TypePath::TypeField::Metatable:
                result << "metatable";
                break;
            case TypePath::TypeField::LowerBound:
                result << "lowerBound";
                break;
            case TypePath::TypeField::UpperBound:
                result << "upperBound";
                break;
            case TypePath::TypeField::IndexLookup:
                result << "indexer";
                break;
            case TypePath::TypeField::IndexResult:
                result << "indexResult";
                break;
            case TypePath::TypeField::Negated:
                result << "negated";
                break;
            case TypePath::TypeField::Variadic:
                result << "variadic";
                break;
            }

            result << "()";
        }
        else if constexpr (std::is_same_v<T, TypePath::PackField>)
        {
            if (!first || prefixDot)
                result << '.';

            switch (c)
            {
            case TypePath::PackField::Arguments:
                result << "arguments";
                break;
            case TypePath::PackField::Returns:
                result << "returns";
                break;
            case TypePath::PackField::Tail:
                result << "tail";
                break;
            }

            result << "()";
        }
        else
        {
            static_assert(always_false_v<T>, "Unhandled Component variant");
        }

        first = false;
    };

    for (const TypePath::Component& component : path.components)
        Luau::visit(strComponent, component);

    return result.str();
}

static bool traverse(TraversalState& state, const Path& path)
{
    auto step = [&state](auto&& c) {
        return state.traverse(c);
    };

    for (const TypePath::Component& component : path.components)
    {
        bool stepSuccess = visit(step, component);
        if (!stepSuccess)
            return false;
    }

    return true;
}

std::optional<TypeOrPack> traverse(TypeId root, const Path& path, NotNull<BuiltinTypes> builtinTypes)
{
    TraversalState state(follow(root), builtinTypes);
    if (traverse(state, path))
        return state.current;
    else
        return std::nullopt;
}

std::optional<TypeOrPack> traverse(TypePackId root, const Path& path, NotNull<BuiltinTypes> builtinTypes)
{
    TraversalState state(follow(root), builtinTypes);
    if (traverse(state, path))
        return state.current;
    else
        return std::nullopt;
}

std::optional<TypeId> traverseForType(TypeId root, const Path& path, NotNull<BuiltinTypes> builtinTypes)
{
    TraversalState state(follow(root), builtinTypes);
    if (traverse(state, path))
    {
        auto ty = get<TypeId>(state.current);
        return ty ? std::make_optional(*ty) : std::nullopt;
    }
    else
        return std::nullopt;
}

std::optional<TypeId> traverseForType(TypePackId root, const Path& path, NotNull<BuiltinTypes> builtinTypes)
{
    TraversalState state(follow(root), builtinTypes);
    if (traverse(state, path))
    {
        auto ty = get<TypeId>(state.current);
        return ty ? std::make_optional(*ty) : std::nullopt;
    }
    else
        return std::nullopt;
}

std::optional<TypePackId> traverseForPack(TypeId root, const Path& path, NotNull<BuiltinTypes> builtinTypes)
{
    TraversalState state(follow(root), builtinTypes);
    if (traverse(state, path))
    {
        auto ty = get<TypePackId>(state.current);
        return ty ? std::make_optional(*ty) : std::nullopt;
    }
    else
        return std::nullopt;
}

std::optional<TypePackId> traverseForPack(TypePackId root, const Path& path, NotNull<BuiltinTypes> builtinTypes)
{
    TraversalState state(follow(root), builtinTypes);
    if (traverse(state, path))
    {
        auto ty = get<TypePackId>(state.current);
        return ty ? std::make_optional(*ty) : std::nullopt;
    }
    else
        return std::nullopt;
}

} // namespace Luau
