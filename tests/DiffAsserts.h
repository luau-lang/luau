// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Differ.h"
#include "Luau/TypeFwd.h"

#include "doctest.h"

#include <string>
#include <type_traits>

namespace Luau
{

std::string toString(const DifferResult& result);

template<typename L, typename R>
std::string diff(L, R)
{
    return "<undiffable>";
}

template<>
std::string diff<TypeId, TypeId>(TypeId l, TypeId r);

template<>
std::string diff<const Type&, const Type&>(const Type& l, const Type& r);

} // namespace Luau

// Note: the do-while blocks in the macros below is to scope the INFO block to
// only that assertion.

#define CHECK_EQ_DIFF(l, r) \
    do \
    { \
        INFO("Left and right values were not equal: ", diff(l, r)); \
        CHECK_EQ(l, r); \
    } while (false);

#define REQUIRE_EQ_DIFF(l, r) \
    do \
    { \
        INFO("Left and right values were not equal: ", diff(l, r)); \
        REQUIRE_EQ(l, r); \
    } while (false);
