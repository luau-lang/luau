// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details

#include "lluz/Substitution.h"
#include "lluz/TypeVar.h"
#include "lluz/Module.h"

namespace lluz
{

struct InternalErrorReporter;

bool isSubtype(TypeId subTy, TypeId superTy, InternalErrorReporter& ice);
bool isSubtype(TypePackId subTy, TypePackId superTy, InternalErrorReporter& ice);

std::pair<TypeId, bool> normalize(TypeId ty, TypeArena& arena, InternalErrorReporter& ice);
std::pair<TypeId, bool> normalize(TypeId ty, const ModulePtr& module, InternalErrorReporter& ice);
std::pair<TypePackId, bool> normalize(TypePackId ty, TypeArena& arena, InternalErrorReporter& ice);
std::pair<TypePackId, bool> normalize(TypePackId ty, const ModulePtr& module, InternalErrorReporter& ice);

} // namespace lluz
