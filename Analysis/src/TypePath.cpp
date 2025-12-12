// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypePath.h"

#include "Luau/Anyification.h"
#include "Luau/Common.h"
#include "Luau/DenseHash.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypePack.h"
#include "Luau/TypeOrPack.h"

#include <functional>
#include <optional>
#include <sstream>

LUAU_FASTFLAG(LuauSolverV2);
LUAU_FASTFLAG(LuauNewOverloadResolver2)
LUAU_FASTFLAG(LuauNewNonStrictBetterCheckedFunctionErrorMessage)

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

bool PackSlice::operator==(const PackSlice& other) const
{
    return start_index == other.start_index;
}

bool Reduction::operator==(const Reduction& other) const
{
    return resultType == other.resultType;
}

bool GenericPackMapping::operator==(const GenericPackMapping& other) const
{
    return mappedType == other.mappedType;
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

size_t PathHash::operator()(const PackSlice& slice) const
{
    return slice.start_index;
}

size_t PathHash::operator()(const Reduction& reduction) const
{
    return std::hash<TypeId>()(reduction.resultType);
}

size_t PathHash::operator()(const GenericPackMapping& mapping) const
{
    return std::hash<TypePackId>()(mapping.mappedType);
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
    components.emplace_back(Property{std::move(name), true});
    return *this;
}

PathBuilder& PathBuilder::writeProp(std::string name)
{
    components.emplace_back(Property{std::move(name), false});
    return *this;
}

PathBuilder& PathBuilder::prop(std::string name)
{
    components.emplace_back(Property{std::move(name)});
    return *this;
}

PathBuilder& PathBuilder::index(size_t i)
{
    components.emplace_back(Index{i});
    return *this;
}

PathBuilder& PathBuilder::mt()
{
    components.emplace_back(TypeField::Metatable);
    return *this;
}

PathBuilder& PathBuilder::lb()
{
    components.emplace_back(TypeField::LowerBound);
    return *this;
}

PathBuilder& PathBuilder::ub()
{
    components.emplace_back(TypeField::UpperBound);
    return *this;
}

PathBuilder& PathBuilder::indexKey()
{
    components.emplace_back(TypeField::IndexLookup);
    return *this;
}

PathBuilder& PathBuilder::indexValue()
{
    components.emplace_back(TypeField::IndexResult);
    return *this;
}

PathBuilder& PathBuilder::negated()
{
    components.emplace_back(TypeField::Negated);
    return *this;
}

PathBuilder& PathBuilder::variadic()
{
    components.emplace_back(TypeField::Variadic);
    return *this;
}

PathBuilder& PathBuilder::args()
{
    components.emplace_back(PackField::Arguments);
    return *this;
}

PathBuilder& PathBuilder::rets()
{
    components.emplace_back(PackField::Returns);
    return *this;
}

PathBuilder& PathBuilder::tail()
{
    components.emplace_back(PackField::Tail);
    return *this;
}

PathBuilder& PathBuilder::packSlice(size_t start_index)
{
    components.emplace_back(PackSlice{start_index});
    return *this;
}

PathBuilder& PathBuilder::mappedGenericPack(TypePackId mappedType)
{
    components.emplace_back(GenericPackMapping{mappedType});
    return *this;
}

} // namespace TypePath

namespace
{

struct TraversalState
{
    TraversalState(TypeId root, const NotNull<BuiltinTypes> builtinTypes, TypeArena* arena)
        : current(root)
        , builtinTypes(builtinTypes)
        , arena(arena)
    {
    }

    TraversalState(TypePackId root, NotNull<BuiltinTypes> builtinTypes, TypeArena* arena)
        : current(root)
        , builtinTypes(builtinTypes)
        , arena(arena)
    {
    }

    TypeOrPack current;
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<TypeArena> arena;
    int steps = 0;
    bool encounteredErrorSuppression = false;

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
        else if (auto c = get<ExternType>(*currentType))
        {
            prop = lookupExternTypeProp(c, property.name);
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

                if (!traverse(TypePath::Property::read("__index")))
                    return false;

                return traverse(property);
            }
        }

        if (prop)
        {
            std::optional<TypeId> maybeType;
            if (FFlag::LuauSolverV2)
                maybeType = property.isRead ? prop->readTy : prop->writeTy;
            else
                maybeType = prop->type_DEPRECATED();

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
            bool updatedCurrent = false;

            if (get<ErrorType>(*currentType))
            {
                encounteredErrorSuppression = true;
                return false;
            }

            if (auto u = get<UnionType>(*currentType))
            {
                auto it = begin(u);
                // We want to track the index that updates the current type with `idx` while still iterating through the entire union to check for error types with `it`.
                size_t idx = 0;
                for (auto it = begin(u); it != end(u); ++it)
                {
                    if (get<ErrorType>(*it))
                        encounteredErrorSuppression = true;
                    if (idx == index.index)
                    {
                        updateCurrent(*it);
                        updatedCurrent = true;
                    }
                    ++idx;
                }
            }
            else if (auto i = get<IntersectionType>(*currentType))
            {
                auto it = begin(i);
                // We want to track the index that updates the current type with `idx` while still iterating through the entire intersection to check for error types with `it`.
                size_t idx = 0;
                for (auto it = begin(i); it != end(i); ++it)
                {
                    if (get<ErrorType>(*it))
                        encounteredErrorSuppression = true;
                    if (idx == index.index)
                    {
                        updateCurrent(*it);
                        updatedCurrent = true;
                    }
                    ++idx;
                }
            }

            return updatedCurrent;
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
        case TypePath::TypeField::Table:
            if (auto mt = get<MetatableType>(current))
            {
                updateCurrent(mt->table);
                return true;
            }

            return false;
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
            else if (auto mt = get<MetatableType>(current))
            {
                if (auto mtTab = get<TableType>(follow(mt->table)); mtTab && mtTab->indexer)
                    indexer = &(*mtTab->indexer);
                else if (auto mtMt = get<TableType>(follow(mt->metatable)); mtMt && mtMt->indexer)
                    indexer = &(*mtMt->indexer);
            }
            // Note: we don't appear to walk the class hierarchy for indexers
            else if (auto ct = get<ExternType>(current); ct && ct->indexer)
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

    bool traverse(TypePath::Reduction reduction)
    {
        if (checkInvariants())
            return false;
        updateCurrent(reduction.resultType);
        return true;
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

    bool traverse(const TypePath::PackSlice slice)
    {
        if (checkInvariants())
            return false;

        const TypePackId* currentPack = get<TypePackId>(current);
        if (!currentPack)
            return false;

        auto [flatHead, flatTail] = flatten(*currentPack);

        if (flatHead.size() <= slice.start_index)
            return false;

        std::vector<TypeId> headSlice;
        headSlice.reserve(flatHead.size() - slice.start_index);

        auto headIter = begin(flatHead);
        for (size_t i = 0; i < slice.start_index && headIter != end(flatHead); ++i)
            ++headIter;

        while (headIter != end(flatHead))
        {
            headSlice.push_back(*headIter);
            ++headIter;
        }

        TypePackId packSlice = arena->addTypePack(headSlice, flatTail);

        updateCurrent(packSlice);

        return true;
    }

    bool traverse(const TypePath::GenericPackMapping mapping)
    {
        if (checkInvariants())
            return false;

        updateCurrent(mapping.mappedType);

        return true;
    }
};

} // namespace

std::string toString(const TypePath::Path& path, bool prefixDot)
{
    std::stringstream result;
    bool first = true;

    auto strComponent = [&](auto&& c)
    {
        using T = std::decay_t<decltype(c)>;
        if constexpr (std::is_same_v<T, TypePath::Property>)
        {
            result << '[';
            if (FFlag::LuauSolverV2)
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
            case TypePath::TypeField::Table:
                result << "table";
                break;
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
        else if constexpr (std::is_same_v<T, TypePath::PackSlice>)
            result << "[" << std::to_string(c.start_index) << ":]";
        else if constexpr (std::is_same_v<T, TypePath::Reduction>)
        {
            // We need to rework the TypePath system to make subtyping failures easier to understand
            // https://roblox.atlassian.net/browse/CLI-104422
            result << "~~>";
        }
        else if constexpr (std::is_same_v<T, TypePath::GenericPackMapping>)
            result << "~";
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

std::string toStringHuman(const TypePath::Path& path)
{
    enum class State
    {
        Initial,
        Normal,
        Property,
        PendingIs,
        PendingAs,
        PendingWhich,
    };

    std::stringstream result;
    State state = State::Initial;
    bool last = false;

    auto strComponent = [&](auto&& c)
    {
        using T = std::decay_t<decltype(c)>;
        if constexpr (std::is_same_v<T, TypePath::Property>)
        {
            if (state == State::PendingIs)
                result << ", ";

            switch (state)
            {
            case State::Initial:
            case State::PendingIs:
                if (c.isRead)
                    result << "accessing `";
                else
                    result << "writing to `";
                break;
            case State::Property:
                // if the previous state was a property, then we're doing a sequence of indexing
                result << '.';
                break;
            default:
                break;
            }

            result << c.name;

            state = State::Property;
        }
        else if constexpr (std::is_same_v<T, TypePath::Index>)
        {
            if (FFlag::LuauNewNonStrictBetterCheckedFunctionErrorMessage)
            {
                if (state == State::Initial && !last)
                    result << "in" << ' ';
                else if (state == State::PendingIs)
                    result << ' ' << "has" << ' ';
                else if (state == State::Property)
                    result << '`' << ' ' << "has" << ' ';

                result << "the " << toHumanReadableIndex(c.index);
            }
            else
            {
                size_t humanIndex = c.index + 1;

                if (state == State::Initial && !last)
                    result << "in" << ' ';
                else if (state == State::PendingIs)
                    result << ' ' << "has" << ' ';
                else if (state == State::Property)
                    result << '`' << ' ' << "has" << ' ';

                result << "the " << humanIndex;
                switch (humanIndex)
                {
                case 1:
                    result << "st";
                    break;
                case 2:
                    result << "nd";
                    break;
                case 3:
                    result << "rd";
                    break;
                default:
                    result << "th";
                }
            }

            switch (c.variant)
            {
            case TypePath::Index::Variant::Pack:
                result << ' ' << "entry in the type pack";
                break;
            case TypePath::Index::Variant::Union:
                result << ' ' << "component of the union";
                break;
            case TypePath::Index::Variant::Intersection:
                result << ' ' << "component of the intersection";
                break;
            }

            if (state == State::PendingWhich)
                result << ' ' << "which";

            if (state == State::PendingIs || state == State::Property)
                state = State::PendingAs;
            else
                state = State::PendingIs;
        }
        else if constexpr (std::is_same_v<T, TypePath::TypeField>)
        {
            if (state == State::Initial && !last)
                result << "in" << ' ';
            else if (state == State::PendingIs)
                result << ", ";
            else if (state == State::Property)
                result << '`' << ' ' << "has" << ' ';

            switch (c)
            {
            case TypePath::TypeField::Table:
                result << "the table portion";
                if (state == State::Property)
                    state = State::PendingAs;
                else
                    state = State::PendingIs;
                break;
            case TypePath::TypeField::Metatable:
                result << "the metatable portion";
                if (state == State::Property)
                    state = State::PendingAs;
                else
                    state = State::PendingIs;
                break;
            case TypePath::TypeField::LowerBound:
                result << "the lower bound of" << ' ';
                state = State::Normal;
                break;
            case TypePath::TypeField::UpperBound:
                result << "the upper bound of" << ' ';
                state = State::Normal;
                break;
            case TypePath::TypeField::IndexLookup:
                result << "the index type";
                if (state == State::Property)
                    state = State::PendingAs;
                else
                    state = State::PendingIs;
                break;
            case TypePath::TypeField::IndexResult:
                result << "the result of indexing";
                if (state == State::Property)
                    state = State::PendingAs;
                else
                    state = State::PendingIs;
                break;
            case TypePath::TypeField::Negated:
                result << "the negation" << ' ';
                state = State::Normal;
                break;
            case TypePath::TypeField::Variadic:
                result << "the variadic" << ' ';
                state = State::Normal;
                break;
            }
        }
        else if constexpr (std::is_same_v<T, TypePath::PackField>)
        {
            if (state == State::PendingIs)
                result << ", ";
            else if (state == State::Property)
                result << "`, ";

            switch (c)
            {
            case TypePath::PackField::Arguments:
                if (state == State::Initial)
                    result << "it" << ' ';
                else if (state == State::PendingIs)
                    result << "the function" << ' ';

                result << "takes";
                break;
            case TypePath::PackField::Returns:
                if (state == State::Initial)
                    result << "it" << ' ';
                else if (state == State::PendingIs)
                    result << "the function" << ' ';

                result << "returns";
                break;
            case TypePath::PackField::Tail:
                if (state == State::Initial)
                    result << "it has" << ' ';
                result << "a tail of";
                break;
            }

            if (state == State::PendingIs)
            {
                result << ' ';
                state = State::PendingWhich;
            }
            else
            {
                result << ' ';
                state = State::Normal;
            }
        }
        else if constexpr (std::is_same_v<T, TypePath::PackSlice>)
            result << "the portion of the type pack starting at index " << c.start_index << " to the end";
        else if constexpr (std::is_same_v<T, TypePath::Reduction>)
        {
            if (state == State::Initial)
                result << "it" << ' ';
            result << "reduces to" << ' ';
            state = State::Normal;
        }
        else if constexpr (std::is_same_v<T, TypePath::GenericPackMapping>)
            result << "is a generic pack mapped to ";
        else
        {
            static_assert(always_false_v<T>, "Unhandled Component variant");
        }
    };

    size_t count = 0;

    for (const TypePath::Component& component : path.components)
    {
        count++;
        if (count == path.components.size())
            last = true;

        Luau::visit(strComponent, component);
    }

    switch (state)
    {
    case State::Property:
        result << "` results in ";
        break;
    case State::PendingWhich:
        // pending `which` becomes `is` if it's at the end
        result << "is" << ' ';
        break;
    case State::PendingIs:
        result << ' ' << "is" << ' ';
        break;
    case State::PendingAs:
        result << ' ' << "as" << ' ';
        break;
    default:
        break;
    }

    return result.str();
}

static bool traverse(TraversalState& state, const Path& path)
{
    auto step = [&state](auto&& c)
    {
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

std::optional<TypeOrPack> traverse(const TypePackId root, const Path& path, const NotNull<BuiltinTypes> builtinTypes, const NotNull<TypeArena> arena)
{
    TraversalState state(follow(root), builtinTypes, arena);
    if (traverse(state, path))
    {
        if (FFlag::LuauNewOverloadResolver2 && state.encounteredErrorSuppression)
            return builtinTypes->errorType;
        return state.current;
    }
    else
        return std::nullopt;
}

std::optional<TypeOrPack> traverse(const TypeId root, const Path& path, const NotNull<BuiltinTypes> builtinTypes, const NotNull<TypeArena> arena)
{
    TraversalState state(follow(root), builtinTypes, arena);
    if (traverse(state, path))
        return state.current;
    else
        return std::nullopt;
}

std::optional<TypeId> traverseForType(const TypeId root, const Path& path, const NotNull<BuiltinTypes> builtinTypes, const NotNull<TypeArena> arena)
{
    TraversalState state(follow(root), builtinTypes, arena);
    if (traverse(state, path))
    {
        if (state.encounteredErrorSuppression)
            return builtinTypes->errorType;

        const TypeId* ty = get<TypeId>(state.current);
        return ty ? std::make_optional(*ty) : std::nullopt;
    }
    else
        return std::nullopt;
}

std::optional<TypeId> traverseForType(
    const TypePackId root,
    const Path& path,
    const NotNull<BuiltinTypes> builtinTypes,
    const NotNull<TypeArena> arena
)
{
    TraversalState state(follow(root), builtinTypes, arena);
    if (traverse(state, path))
    {
        if (state.encounteredErrorSuppression)
            return builtinTypes->errorType;
        const TypeId* ty = get<TypeId>(state.current);
        return ty ? std::make_optional(*ty) : std::nullopt;
    }
    else
        return std::nullopt;
}

std::optional<TypePackId> traverseForPack(
    const TypeId root,
    const Path& path,
    const NotNull<BuiltinTypes> builtinTypes,
    const NotNull<TypeArena> arena
)
{
    TraversalState state(follow(root), builtinTypes, arena);
    if (traverse(state, path))
    {
        if (state.encounteredErrorSuppression)
            return builtinTypes->errorTypePack;
        const TypePackId* ty = get<TypePackId>(state.current);
        return ty ? std::make_optional(*ty) : std::nullopt;
    }
    else
        return std::nullopt;
}

std::optional<TypePackId> traverseForPack(
    const TypePackId root,
    const Path& path,
    const NotNull<BuiltinTypes> builtinTypes,
    const NotNull<TypeArena> arena
)
{
    TraversalState state(follow(root), builtinTypes, arena);
    if (traverse(state, path))
    {
        if (state.encounteredErrorSuppression)
            return builtinTypes->errorTypePack;
        const TypePackId* ty = get<TypePackId>(state.current);
        return ty ? std::make_optional(*ty) : std::nullopt;
    }
    else
        return std::nullopt;
}

std::optional<size_t> traverseForIndex(const Path& path)
{
    auto componentIter = begin(path.components);
    size_t index = 0;
    const auto lastComponent = end(path.components) - 1;

    while (componentIter != lastComponent)
    {
        if (const auto packSlice = get_if<Luau::TypePath::PackSlice>(&*componentIter))
        {
            index += packSlice->start_index;
        }
        else
        {
            return std::nullopt;
        }
        ++componentIter;
    }

    if (const auto indexComponent = get_if<TypePath::Index>(&*componentIter))
    {
        index += indexComponent->index;
        return index;
    }
    return std::nullopt;
}

TypePack flattenPackWithPath(TypePackId root, const Path& path)
{
    std::vector<TypeId> flattened;

    std::optional<TypePackId> curr = root;
    auto pathIter = begin(path.components);
    const auto pathEnd = end(path.components);

    while (curr)
    {
        TypePackIterator it(*curr);
        // Push back curr's head
        for (; it != end(*curr); ++it)
            flattened.emplace_back(*it);

        // Check if curr has a tail, and if the next bit of path is Tail + GenericPackMapping
        curr = it.tail();
        if (!curr || !get<GenericTypePack>(*curr) || pathIter == pathEnd)
            break;

        if (const TypePath::PackField* pf = get_if<TypePath::PackField>(&*pathIter); !pf || *pf != TypePath::PackField::Tail)
            break;

        ++pathIter;

        const TypePath::GenericPackMapping* gpm = get_if<TypePath::GenericPackMapping>(&*pathIter);
        if (!gpm)
            break;

        ++pathIter;
        curr = gpm->mappedType;
    }

    return {flattened, curr};
}

TypePack traverseForFlattenedPack(const TypeId root, const Path& path, const NotNull<BuiltinTypes> builtinTypes, const NotNull<TypeArena> arena)
{
    // Iterate over path's components, and figure out when it turns into Tails and GenericPackMappings
    // We want to split out the part of the path that contains the generic pack mappings we're interested in, so that we can flatten it
    // path[splitIndex:] will contain only Tails and GenericPackMappings
    size_t splitIndex = 0;
    for (size_t i = path.components.size(); i > 0; --i)
    {
        const TypePath::Component& c = path.components[i - 1];

        const TypePath::PackField* pf = get_if<TypePath::PackField>(&c);
        const bool isNotTail = pf == nullptr || *pf != TypePath::PackField::Tail;
        const bool isNotGPM = get_if<TypePath::GenericPackMapping>(&c) == nullptr;

        if (isNotTail && isNotGPM)
        {
            splitIndex = i;
            break;
        }
    }

    // Root is a TypeId, not a TypePackId, so splitIndex should be > 0
    LUAU_ASSERT(splitIndex > 0);
    if (splitIndex == path.components.size() || splitIndex == 0)
        return {{}, std::nullopt};

    auto basePathIter = begin(path.components);
    std::advance(basePathIter, splitIndex);
    const std::optional<TypePackId> basePack = traverseForPack(root, Path(std::vector(begin(path.components), basePathIter)), builtinTypes, arena);

    if (!basePack)
    {
        LUAU_ASSERT(!"Expected to be able to traverse to a TypePackId");
        return {{}, std::nullopt};
    }

    return flattenPackWithPath(*basePack, Path(std::vector(basePathIter, end(path.components))));
}

bool matchesPrefix(const Path& prefix, const Path& full)
{
    auto prefixIter = begin(prefix.components);
    auto fullIter = begin(full.components);
    const auto prefixEnd = end(prefix.components);
    const auto fullEnd = end(full.components);

    while (prefixIter != prefixEnd)
    {
        if (fullIter == fullEnd)
            return false;

        if (*prefixIter++ != *fullIter++)
            return false;
    }

    return true;
}

} // namespace Luau
