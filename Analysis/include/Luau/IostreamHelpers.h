// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Error.h"
#include "Luau/Location.h"
#include "Luau/Type.h"
#include "Luau/Ast.h"
#include "Luau/TypePath.h"

#include <ostream>

namespace Luau
{

std::ostream& operator<<(std::ostream& lhs, const Position& position);
std::ostream& operator<<(std::ostream& lhs, const Location& location);
std::ostream& operator<<(std::ostream& lhs, const AstName& name);

std::ostream& operator<<(std::ostream& lhs, const TypeError& error);
std::ostream& operator<<(std::ostream& lhs, const TypeMismatch& error);
std::ostream& operator<<(std::ostream& lhs, const UnknownSymbol& error);
std::ostream& operator<<(std::ostream& lhs, const UnknownProperty& error);
std::ostream& operator<<(std::ostream& lhs, const NotATable& error);
std::ostream& operator<<(std::ostream& lhs, const CannotExtendTable& error);
std::ostream& operator<<(std::ostream& lhs, const CannotCompareUnrelatedTypes& error);
std::ostream& operator<<(std::ostream& lhs, const OnlyTablesCanHaveMethods& error);
std::ostream& operator<<(std::ostream& lhs, const DuplicateTypeDefinition& error);
std::ostream& operator<<(std::ostream& lhs, const CountMismatch& error);
std::ostream& operator<<(std::ostream& lhs, const FunctionDoesNotTakeSelf& error);
std::ostream& operator<<(std::ostream& lhs, const FunctionRequiresSelf& error);
std::ostream& operator<<(std::ostream& lhs, const OccursCheckFailed& error);
std::ostream& operator<<(std::ostream& lhs, const UnknownRequire& error);
std::ostream& operator<<(std::ostream& lhs, const UnknownPropButFoundLikeProp& e);
std::ostream& operator<<(std::ostream& lhs, const GenericError& error);
std::ostream& operator<<(std::ostream& lhs, const InternalError& error);
std::ostream& operator<<(std::ostream& lhs, const FunctionExitsWithoutReturning& error);
std::ostream& operator<<(std::ostream& lhs, const MissingProperties& error);
std::ostream& operator<<(std::ostream& lhs, const IllegalRequire& error);
std::ostream& operator<<(std::ostream& lhs, const ModuleHasCyclicDependency& error);
std::ostream& operator<<(std::ostream& lhs, const DuplicateGenericParameter& error);
std::ostream& operator<<(std::ostream& lhs, const CannotInferBinaryOperation& error);
std::ostream& operator<<(std::ostream& lhs, const SwappedGenericTypeParameter& error);
std::ostream& operator<<(std::ostream& lhs, const OptionalValueAccess& error);
std::ostream& operator<<(std::ostream& lhs, const MissingUnionProperty& error);
std::ostream& operator<<(std::ostream& lhs, const TypesAreUnrelated& error);

std::ostream& operator<<(std::ostream& lhs, const TableState& tv);
std::ostream& operator<<(std::ostream& lhs, const Type& tv);
std::ostream& operator<<(std::ostream& lhs, const TypePackVar& tv);

std::ostream& operator<<(std::ostream& lhs, const TypeErrorData& ted);

std::ostream& operator<<(std::ostream& lhs, TypeId ty);
std::ostream& operator<<(std::ostream& lhs, TypePackId tp);

namespace TypePath
{

std::ostream& operator<<(std::ostream& lhs, const Path& path);

}; // namespace TypePath

} // namespace Luau
