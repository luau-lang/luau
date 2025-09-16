// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/HashUtil.h"

#include <cstddef>
#include <functional>
#include <unordered_set>
#include <vector>

namespace Luau::EqSat
{

template<typename T>
struct LanguageHash
{
    size_t operator()(const T& t, decltype(std::hash<T>{}(std::declval<T>()))* = 0) const
    {
        return std::hash<T>{}(t);
    }
};

template<typename T>
size_t languageHash(const T& lang)
{
    return LanguageHash<T>{}(lang);
}

template<typename T, size_t I>
struct LanguageHash<std::array<T, I>>
{
    size_t operator()(const std::array<T, I>& array) const
    {
        size_t seed = 0;
        for (const T& t : array)
            hashCombine(seed, languageHash(t));
        return seed;
    }
};

template<typename T>
struct LanguageHash<std::vector<T>>
{
    size_t operator()(const std::vector<T>& vector) const
    {
        size_t seed = 0;
        for (const T& t : vector)
            hashCombine(seed, languageHash(t));
        return seed;
    }
};

} // namespace Luau::EqSat
