// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IostreamHelpers.h"
#include "Luau/ToString.h"

namespace Luau
{

std::ostream& operator<<(std::ostream& stream, const Position& position)
{
    return stream << "{ line = " << position.line << ", col = " << position.column << " }";
}

std::ostream& operator<<(std::ostream& stream, const Location& location)
{
    return stream << "Location { " << location.begin << ", " << location.end << " }";
}

std::ostream& operator<<(std::ostream& stream, const AstName& name)
{
    if (name.value)
        return stream << name.value;
    else
        return stream << "<empty>";
}

std::ostream& operator<<(std::ostream& stream, const TypeMismatch& tm)
{
    return stream << "TypeMismatch { " << toString(tm.wantedType) << ", " << toString(tm.givenType) << " }";
}

std::ostream& operator<<(std::ostream& stream, const TypeError& error)
{
    return stream << "TypeError { \"" << error.moduleName << "\", " << error.location << ", " << error.data << " }";
}

std::ostream& operator<<(std::ostream& stream, const UnknownSymbol& error)
{
    return stream << "UnknownSymbol { " << error.name << " , context " << error.context << " }";
}

std::ostream& operator<<(std::ostream& stream, const UnknownProperty& error)
{
    return stream << "UnknownProperty { " << toString(error.table) << ", key = " << error.key << " }";
}

std::ostream& operator<<(std::ostream& stream, const NotATable& ge)
{
    return stream << "NotATable { " << toString(ge.ty) << " }";
}

std::ostream& operator<<(std::ostream& stream, const CannotExtendTable& error)
{
    return stream << "CannotExtendTable { " << toString(error.tableType) << ", context " << error.context << ", prop \"" << error.prop << "\" }";
}

std::ostream& operator<<(std::ostream& stream, const OnlyTablesCanHaveMethods& error)
{
    return stream << "OnlyTablesCanHaveMethods { " << toString(error.tableType) << " }";
}

std::ostream& operator<<(std::ostream& stream, const DuplicateTypeDefinition& error)
{
    return stream << "DuplicateTypeDefinition { " << error.name << " }";
}

std::ostream& operator<<(std::ostream& stream, const CountMismatch& error)
{
    return stream << "CountMismatch { expected " << error.expected << ", got " << error.actual << ", context " << error.context << " }";
}

std::ostream& operator<<(std::ostream& stream, const FunctionDoesNotTakeSelf&)
{
    return stream << "FunctionDoesNotTakeSelf { }";
}

std::ostream& operator<<(std::ostream& stream, const FunctionRequiresSelf& error)
{
    return stream << "FunctionRequiresSelf { extraNils " << error.requiredExtraNils << " }";
}

std::ostream& operator<<(std::ostream& stream, const OccursCheckFailed&)
{
    return stream << "OccursCheckFailed { }";
}

std::ostream& operator<<(std::ostream& stream, const UnknownRequire& error)
{
    return stream << "UnknownRequire { " << error.modulePath << " }";
}

std::ostream& operator<<(std::ostream& stream, const IncorrectGenericParameterCount& error)
{
    stream << "IncorrectGenericParameterCount { name = " << error.name;

    if (!error.typeFun.typeParams.empty())
    {
        stream << "<";
        bool first = true;
        for (TypeId t : error.typeFun.typeParams)
        {
            if (first)
                first = false;
            else
                stream << ", ";

            stream << toString(t);
        }
        stream << ">";
    }

    stream << ", typeFun = " << toString(error.typeFun.type) << ", actualCount = " << error.actualParameters << " }";
    return stream;
}

std::ostream& operator<<(std::ostream& stream, const SyntaxError& ge)
{
    return stream << "SyntaxError { " << ge.message << " }";
}

std::ostream& operator<<(std::ostream& stream, const CodeTooComplex&)
{
    return stream << "CodeTooComplex {}";
}

std::ostream& operator<<(std::ostream& stream, const UnificationTooComplex&)
{
    return stream << "UnificationTooComplex {}";
}

std::ostream& operator<<(std::ostream& stream, const UnknownPropButFoundLikeProp& e)
{
    stream << "UnknownPropButFoundLikeProp { key = '" << e.key << "', suggested = { ";

    bool first = true;
    for (Name name : e.candidates)
    {
        if (first)
            first = false;
        else
            stream << ", ";

        stream << "'" << name << "'";
    }

    return stream << " }, table = " << toString(e.table) << " } ";
}

std::ostream& operator<<(std::ostream& stream, const GenericError& ge)
{
    return stream << "GenericError { " << ge.message << " }";
}

std::ostream& operator<<(std::ostream& stream, const CannotCallNonFunction& e)
{
    return stream << "CannotCallNonFunction { " << toString(e.ty) << " }";
}

std::ostream& operator<<(std::ostream& stream, const FunctionExitsWithoutReturning& error)
{
    return stream << "FunctionExitsWithoutReturning {" << toString(error.expectedReturnType) << "}";
}

std::ostream& operator<<(std::ostream& stream, const ExtraInformation& e)
{
    return stream << "ExtraInformation { " << e.message << " }";
}

std::ostream& operator<<(std::ostream& stream, const DeprecatedApiUsed& e)
{
    return stream << "DeprecatedApiUsed { " << e.symbol << ", useInstead = " << e.useInstead << " }";
}

std::ostream& operator<<(std::ostream& stream, const ModuleHasCyclicDependency& e)
{
    stream << "ModuleHasCyclicDependency {";

    bool first = true;
    for (const ModuleName& name : e.cycle)
    {
        if (first)
            first = false;
        else
            stream << ", ";

        stream << name;
    }

    return stream << "}";
}

std::ostream& operator<<(std::ostream& stream, const IllegalRequire& e)
{
    return stream << "IllegalRequire { " << e.moduleName << ", reason = " << e.reason << " }";
}

std::ostream& operator<<(std::ostream& stream, const MissingProperties& e)
{
    stream << "MissingProperties { superType = '" << toString(e.superType) << "', subType = '" << toString(e.subType) << "', properties = { ";

    bool first = true;
    for (Name name : e.properties)
    {
        if (first)
            first = false;
        else
            stream << ", ";

        stream << "'" << name << "'";
    }

    return stream << " }, context " << e.context << " } ";
}

std::ostream& operator<<(std::ostream& stream, const DuplicateGenericParameter& error)
{
    return stream << "DuplicateGenericParameter { " + error.parameterName + " }";
}

std::ostream& operator<<(std::ostream& stream, const CannotInferBinaryOperation& error)
{
    return stream << "CannotInferBinaryOperation { op = " + toString(error.op) + ", suggested = '" +
                         (error.suggestedToAnnotate ? *error.suggestedToAnnotate : "") + "', kind "
                  << error.kind << "}";
}

std::ostream& operator<<(std::ostream& stream, const SwappedGenericTypeParameter& error)
{
    return stream << "SwappedGenericTypeParameter { name = '" + error.name + "', kind = " + std::to_string(error.kind) + " }";
}

std::ostream& operator<<(std::ostream& stream, const OptionalValueAccess& error)
{
    return stream << "OptionalValueAccess { optional = '" + toString(error.optional) + "' }";
}

std::ostream& operator<<(std::ostream& stream, const MissingUnionProperty& error)
{
    stream << "MissingUnionProperty { type = '" + toString(error.type) + "', missing = { ";

    bool first = true;
    for (auto ty : error.missing)
    {
        if (first)
            first = false;
        else
            stream << ", ";

        stream << "'" << toString(ty) << "'";
    }

    return stream << " }, key = '" + error.key + "' }";
}

std::ostream& operator<<(std::ostream& stream, const TableState& tv)
{
    return stream << static_cast<std::underlying_type<TableState>::type>(tv);
}

std::ostream& operator<<(std::ostream& stream, const TypeVar& tv)
{
    return stream << toString(tv);
}

std::ostream& operator<<(std::ostream& stream, const TypePackVar& tv)
{
    return stream << toString(tv);
}

std::ostream& operator<<(std::ostream& lhs, const TypeErrorData& ted)
{
    Luau::visit(
        [&](const auto& a) {
            lhs << a;
        },
        ted);

    return lhs;
}

} // namespace Luau
