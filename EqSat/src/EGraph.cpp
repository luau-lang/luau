// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/EGraph.h"

namespace Luau::EqSat {

template<typename L, typename N>
Id EGraph<L, N>::find(Id id) const {
    return unionfind.find(id);
}

}
