// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "DiffAsserts.h"

#include <string>

namespace Luau
{


std::string toString(const DifferResult& result)
{
    if (result.diffError)
        return result.diffError->toString();
    else
        return "<no diff>";
}

template<>
std::string diff<TypeId, TypeId>(TypeId l, TypeId r)
{
    return toString(diff(l, r));
}

template<>
std::string diff<const Type&, const Type&>(const Type& l, const Type& r)
{
    return toString(diff(&l, &r));
}

} // namespace Luau
