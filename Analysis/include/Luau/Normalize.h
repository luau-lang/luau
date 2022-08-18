// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Module.h"
#include "Luau/NotNull.h"
#include "Luau/TypeVar.h"

#include <memory>

namespace Luau
{

struct InternalErrorReporter;
struct Module;
struct Scope;

using ModulePtr = std::shared_ptr<Module>;

bool isSubtype(TypeId subTy, TypeId superTy, NotNull<Scope> scope, InternalErrorReporter& ice);
bool isSubtype(TypePackId subTy, TypePackId superTy, NotNull<Scope> scope, InternalErrorReporter& ice);

std::pair<TypeId, bool> normalize(TypeId ty, NotNull<Scope> scope, TypeArena& arena, InternalErrorReporter& ice);
std::pair<TypeId, bool> normalize(TypeId ty, NotNull<Module> module, InternalErrorReporter& ice);
std::pair<TypeId, bool> normalize(TypeId ty, const ModulePtr& module, InternalErrorReporter& ice);
std::pair<TypePackId, bool> normalize(TypePackId ty, NotNull<Scope> scope, TypeArena& arena, InternalErrorReporter& ice);
std::pair<TypePackId, bool> normalize(TypePackId ty, NotNull<Module> module, InternalErrorReporter& ice);
std::pair<TypePackId, bool> normalize(TypePackId ty, const ModulePtr& module, InternalErrorReporter& ice);

} // namespace Luau
