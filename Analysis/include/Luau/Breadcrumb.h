// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Def.h"
#include "Luau/NotNull.h"
#include "Luau/Variant.h"

#include <string>
#include <optional>

namespace Luau
{

using NullableBreadcrumbId = const struct Breadcrumb*;
using BreadcrumbId = NotNull<const struct Breadcrumb>;

struct FieldMetadata
{
    std::string prop;
};

struct SubscriptMetadata
{
    BreadcrumbId key;
};

using Metadata = Variant<FieldMetadata, SubscriptMetadata>;

struct Breadcrumb
{
    NullableBreadcrumbId previous;
    DefId def;
    std::optional<Metadata> metadata;
    std::vector<BreadcrumbId> children;
};

inline Breadcrumb* asMutable(NullableBreadcrumbId breadcrumb)
{
    LUAU_ASSERT(breadcrumb);
    return const_cast<Breadcrumb*>(breadcrumb);
}

template<typename T>
const T* getMetadata(NullableBreadcrumbId breadcrumb)
{
    if (!breadcrumb || !breadcrumb->metadata)
        return nullptr;

    return get_if<T>(&*breadcrumb->metadata);
}

struct BreadcrumbArena
{
    TypedAllocator<Breadcrumb> allocator;

    template<typename... Args>
    BreadcrumbId add(NullableBreadcrumbId previous, DefId def, Args&&... args)
    {
        Breadcrumb* bc = allocator.allocate(Breadcrumb{previous, def, std::forward<Args>(args)...});
        if (previous)
            asMutable(previous)->children.push_back(NotNull{bc});
        return NotNull{bc};
    }

    template<typename T, typename... Args>
    BreadcrumbId emplace(NullableBreadcrumbId previous, DefId def, Args&&... args)
    {
        Breadcrumb* bc = allocator.allocate(Breadcrumb{previous, def, Metadata{T{std::forward<Args>(args)...}}});
        if (previous)
            asMutable(previous)->children.push_back(NotNull{bc});
        return NotNull{bc};
    }
};

} // namespace Luau
