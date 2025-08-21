// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Type.h"
#include "Luau/TypeIds.h"

namespace Luau
{

TypeIds::TypeIds(std::initializer_list<TypeId> tys)
{
    for (TypeId ty : tys)
        insert(ty);
}

void TypeIds::insert(TypeId ty)
{
    ty = follow(ty);

    // get a reference to the slot for `ty` in `types`
    bool& entry = types[ty];

    // if `ty` is fresh, we can set it to `true`, add it to the order and hash and be done.
    if (!entry)
    {
        entry = true;
        order.push_back(ty);
        hash ^= std::hash<TypeId>{}(ty);
    }
}

void TypeIds::clear()
{
    order.clear();
    types.clear();
    hash = 0;
}

TypeId TypeIds::front() const
{
    return order.at(0);
}

TypeIds::iterator TypeIds::begin()
{
    return order.begin();
}

TypeIds::iterator TypeIds::end()
{
    return order.end();
}

TypeIds::const_iterator TypeIds::begin() const
{
    return order.begin();
}

TypeIds::const_iterator TypeIds::end() const
{
    return order.end();
}

TypeIds::iterator TypeIds::erase(TypeIds::const_iterator it)
{
    TypeId ty = *it;
    types[ty] = false;
    hash ^= std::hash<TypeId>{}(ty);
    return order.erase(it);
}

void TypeIds::erase(TypeId ty)
{
    const_iterator it = std::find(order.begin(), order.end(), ty);
    if (it == order.end())
        return;

    erase(it);
}

size_t TypeIds::size() const
{
    return order.size();
}

bool TypeIds::empty() const
{
    return order.empty();
}

size_t TypeIds::count(TypeId ty) const
{
    ty = follow(ty);
    const bool* val = types.find(ty);
    return (val && *val) ? 1 : 0;
}

void TypeIds::retain(const TypeIds& tys)
{
    for (auto it = begin(); it != end();)
    {
        if (tys.count(*it))
            it++;
        else
            it = erase(it);
    }
}

size_t TypeIds::getHash() const
{
    return hash;
}

bool TypeIds::isNever() const
{
    return std::all_of(
        begin(),
        end(),
        [&](TypeId i)
        {
            // If each typeid is never, then I guess typeid's is also never?
            return get<NeverType>(i) != nullptr;
        }
    );
}

bool TypeIds::operator==(const TypeIds& there) const
{
    // we can early return if the hashes don't match.
    if (hash != there.hash)
        return false;

    // we have to check equality of the sets themselves if not.

    // if the sets are unequal sizes, then they cannot possibly be equal.
    // it is important to use `order` here and not `types` since the mappings
    // may have different sizes since removal is not possible, and so erase
    // simply writes `false` into the map.
    if (order.size() != there.order.size())
        return false;

    // otherwise, we'll need to check that every element we have here is in `there`.
    for (auto ty : order)
    {
        // if it's not, we'll return `false`
        if (there.count(ty) == 0)
            return false;
    }

    // otherwise, we've proven the two equal!
    return true;
}

std::vector<TypeId> TypeIds::take()
{
    hash = 0;
    types.clear();
    return std::move(order);
}

void TypeIds::reserve(size_t n)
{
    order.reserve(n);
}


} // namespace Luau
