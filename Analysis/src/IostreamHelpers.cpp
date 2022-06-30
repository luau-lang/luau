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

template<typename T>
static void errorToString(std::ostream& stream, const T& err)
{
    if constexpr (false)
    {
    }
    else if constexpr (std::is_same_v<T, TypeMismatch>)
        stream << "TypeMismatch { " << toString(err.wantedType) << ", " << toString(err.givenType) << " }";
    else if constexpr (std::is_same_v<T, UnknownSymbol>)
        stream << "UnknownSymbol { " << err.name << " , context " << err.context << " }";
    else if constexpr (std::is_same_v<T, UnknownProperty>)
        stream << "UnknownProperty { " << toString(err.table) << ", key = " << err.key << " }";
    else if constexpr (std::is_same_v<T, NotATable>)
        stream << "NotATable { " << toString(err.ty) << " }";
    else if constexpr (std::is_same_v<T, CannotExtendTable>)
        stream << "CannotExtendTable { " << toString(err.tableType) << ", context " << err.context << ", prop \"" << err.prop << "\" }";
    else if constexpr (std::is_same_v<T, OnlyTablesCanHaveMethods>)
        stream << "OnlyTablesCanHaveMethods { " << toString(err.tableType) << " }";
    else if constexpr (std::is_same_v<T, DuplicateTypeDefinition>)
        stream << "DuplicateTypeDefinition { " << err.name << " }";
    else if constexpr (std::is_same_v<T, CountMismatch>)
        stream << "CountMismatch { expected " << err.expected << ", got " << err.actual << ", context " << err.context << " }";
    else if constexpr (std::is_same_v<T, FunctionDoesNotTakeSelf>)
        stream << "FunctionDoesNotTakeSelf { }";
    else if constexpr (std::is_same_v<T, FunctionRequiresSelf>)
        stream << "FunctionRequiresSelf { }";
    else if constexpr (std::is_same_v<T, OccursCheckFailed>)
        stream << "OccursCheckFailed { }";
    else if constexpr (std::is_same_v<T, UnknownRequire>)
        stream << "UnknownRequire { " << err.modulePath << " }";
    else if constexpr (std::is_same_v<T, IncorrectGenericParameterCount>)
    {
        stream << "IncorrectGenericParameterCount { name = " << err.name;

        if (!err.typeFun.typeParams.empty() || !err.typeFun.typePackParams.empty())
        {
            stream << "<";
            bool first = true;
            for (auto param : err.typeFun.typeParams)
            {
                if (first)
                    first = false;
                else
                    stream << ", ";

                stream << toString(param.ty);
            }

            for (auto param : err.typeFun.typePackParams)
            {
                if (first)
                    first = false;
                else
                    stream << ", ";

                stream << toString(param.tp);
            }

            stream << ">";
        }

        stream << ", typeFun = " << toString(err.typeFun.type) << ", actualCount = " << err.actualParameters << " }";
    }
    else if constexpr (std::is_same_v<T, SyntaxError>)
        stream << "SyntaxError { " << err.message << " }";
    else if constexpr (std::is_same_v<T, CodeTooComplex>)
        stream << "CodeTooComplex {}";
    else if constexpr (std::is_same_v<T, UnificationTooComplex>)
        stream << "UnificationTooComplex {}";
    else if constexpr (std::is_same_v<T, UnknownPropButFoundLikeProp>)
    {
        stream << "UnknownPropButFoundLikeProp { key = '" << err.key << "', suggested = { ";

        bool first = true;
        for (Name name : err.candidates)
        {
            if (first)
                first = false;
            else
                stream << ", ";

            stream << "'" << name << "'";
        }

        stream << " }, table = " << toString(err.table) << " } ";
    }
    else if constexpr (std::is_same_v<T, GenericError>)
        stream << "GenericError { " << err.message << " }";
    else if constexpr (std::is_same_v<T, InternalError>)
        stream << "InternalError { " << err.message << " }";
    else if constexpr (std::is_same_v<T, CannotCallNonFunction>)
        stream << "CannotCallNonFunction { " << toString(err.ty) << " }";
    else if constexpr (std::is_same_v<T, ExtraInformation>)
        stream << "ExtraInformation { " << err.message << " }";
    else if constexpr (std::is_same_v<T, DeprecatedApiUsed>)
        stream << "DeprecatedApiUsed { " << err.symbol << ", useInstead = " << err.useInstead << " }";
    else if constexpr (std::is_same_v<T, ModuleHasCyclicDependency>)
    {
        stream << "ModuleHasCyclicDependency {";

        bool first = true;
        for (const ModuleName& name : err.cycle)
        {
            if (first)
                first = false;
            else
                stream << ", ";

            stream << name;
        }

        stream << "}";
    }
    else if constexpr (std::is_same_v<T, IllegalRequire>)
        stream << "IllegalRequire { " << err.moduleName << ", reason = " << err.reason << " }";
    else if constexpr (std::is_same_v<T, FunctionExitsWithoutReturning>)
        stream << "FunctionExitsWithoutReturning {" << toString(err.expectedReturnType) << "}";
    else if constexpr (std::is_same_v<T, DuplicateGenericParameter>)
        stream << "DuplicateGenericParameter { " + err.parameterName + " }";
    else if constexpr (std::is_same_v<T, CannotInferBinaryOperation>)
        stream << "CannotInferBinaryOperation { op = " + toString(err.op) + ", suggested = '" +
                      (err.suggestedToAnnotate ? *err.suggestedToAnnotate : "") + "', kind "
               << err.kind << "}";
    else if constexpr (std::is_same_v<T, MissingProperties>)
    {
        stream << "MissingProperties { superType = '" << toString(err.superType) << "', subType = '" << toString(err.subType) << "', properties = { ";

        bool first = true;
        for (Name name : err.properties)
        {
            if (first)
                first = false;
            else
                stream << ", ";

            stream << "'" << name << "'";
        }

        stream << " }, context " << err.context << " } ";
    }
    else if constexpr (std::is_same_v<T, SwappedGenericTypeParameter>)
        stream << "SwappedGenericTypeParameter { name = '" + err.name + "', kind = " + std::to_string(err.kind) + " }";
    else if constexpr (std::is_same_v<T, OptionalValueAccess>)
        stream << "OptionalValueAccess { optional = '" + toString(err.optional) + "' }";
    else if constexpr (std::is_same_v<T, MissingUnionProperty>)
    {
        stream << "MissingUnionProperty { type = '" + toString(err.type) + "', missing = { ";

        bool first = true;
        for (auto ty : err.missing)
        {
            if (first)
                first = false;
            else
                stream << ", ";

            stream << "'" << toString(ty) << "'";
        }

        stream << " }, key = '" + err.key + "' }";
    }
    else if constexpr (std::is_same_v<T, TypesAreUnrelated>)
        stream << "TypesAreUnrelated { left = '" + toString(err.left) + "', right = '" + toString(err.right) + "' }";
    else if constexpr (std::is_same_v<T, NormalizationTooComplex>)
        stream << "NormalizationTooComplex { }";
    else
        static_assert(always_false_v<T>, "Non-exhaustive type switch");
}

std::ostream& operator<<(std::ostream& stream, const TypeErrorData& data)
{
    auto cb = [&](const auto& e) {
        return errorToString(stream, e);
    };
    visit(cb, data);
    return stream;
}

std::ostream& operator<<(std::ostream& stream, const TypeError& error)
{
    return stream << "TypeError { \"" << error.moduleName << "\", " << error.location << ", " << error.data << " }";
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

} // namespace Luau
