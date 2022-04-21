// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Substitution.h"
#include "Luau/TypeVar.h"
#include "Luau/Module.h"

namespace Luau
{

struct InternalErrorReporter;

bool isSubtype(TypeId superTy, TypeId subTy, InternalErrorReporter& ice);

std::pair<TypeId, bool> normalize(TypeId ty, TypeArena& arena, InternalErrorReporter& ice);
std::pair<TypeId, bool> normalize(TypeId ty, const ModulePtr& module, InternalErrorReporter& ice);
std::pair<TypePackId, bool> normalize(TypePackId ty, TypeArena& arena, InternalErrorReporter& ice);
std::pair<TypePackId, bool> normalize(TypePackId ty, const ModulePtr& module, InternalErrorReporter& ice);

} // namespace Luau
