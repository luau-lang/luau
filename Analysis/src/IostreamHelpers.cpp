// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IostreamHelpers.h"
#include "Luau/Error.h"
#include "Luau/ToString.h"
#include "Luau/TypePath.h"

#include <type_traits>

LUAU_FASTFLAG(LuauSubtypingReportGenericBoundMismatches2);

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
    else if constexpr (std::is_same_v<T, CannotCompareUnrelatedTypes>)
        stream << "CannotCompareUnrelatedTypes { " << toString(err.left) << ", " << toString(err.right) << ", op '" << toString(err.op) << "' }";
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
    else if constexpr (std::is_same_v<T, ConstraintSolvingIncompleteError>)
        stream << "ConstraintSolvingIncompleteError {}";
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
    else if constexpr (std::is_same_v<T, TypePackMismatch>)
        stream << "TypePackMismatch { wanted = '" + toString(err.wantedTp) + "', given = '" + toString(err.givenTp) + "' }";
    else if constexpr (std::is_same_v<T, DynamicPropertyLookupOnExternTypesUnsafe>)
        stream << "DynamicPropertyLookupOnExternTypesUnsafe { " << toString(err.ty) << " }";
    else if constexpr (std::is_same_v<T, UninhabitedTypeFunction>)
        stream << "UninhabitedTypeFunction { " << toString(err.ty) << " }";
    else if constexpr (std::is_same_v<T, ExplicitFunctionAnnotationRecommended>)
    {
        std::string recArgs = "[";
        for (auto [s, t] : err.recommendedArgs)
            recArgs += " " + s + ": " + toString(t);
        recArgs += " ]";
        stream << "ExplicitFunctionAnnotationRecommended { recommmendedReturn = '" + toString(err.recommendedReturn) +
                      "', recommmendedArgs = " + recArgs + "}";
    }
    else if constexpr (std::is_same_v<T, UninhabitedTypePackFunction>)
        stream << "UninhabitedTypePackFunction { " << toString(err.tp) << " }";
    else if constexpr (std::is_same_v<T, WhereClauseNeeded>)
        stream << "WhereClauseNeeded { " << toString(err.ty) << " }";
    else if constexpr (std::is_same_v<T, PackWhereClauseNeeded>)
        stream << "PackWhereClauseNeeded { " << toString(err.tp) << " }";
    else if constexpr (std::is_same_v<T, CheckedFunctionCallError>)
        stream << "CheckedFunctionCallError { expected = '" << toString(err.expected) << "', passed = '" << toString(err.passed)
               << "', checkedFunctionName = " << err.checkedFunctionName << ", argumentIndex = " << std::to_string(err.argumentIndex) << " }";
    else if constexpr (std::is_same_v<T, NonStrictFunctionDefinitionError>)
        stream << "NonStrictFunctionDefinitionError { functionName = '" + err.functionName + "', argument = '" + err.argument +
                      "', argumentType = '" + toString(err.argumentType) + "' }";
    else if constexpr (std::is_same_v<T, PropertyAccessViolation>)
        stream << "PropertyAccessViolation { table = " << toString(err.table) << ", prop = '" << err.key << "', context = " << err.context << " }";
    else if constexpr (std::is_same_v<T, CheckedFunctionIncorrectArgs>)
        stream << "CheckedFunction {  functionName = '" + err.functionName + ", expected = " + std::to_string(err.expected) +
                      ", actual = " + std::to_string(err.actual) + "}";
    else if constexpr (std::is_same_v<T, UnexpectedTypeInSubtyping>)
        stream << "UnexpectedTypeInSubtyping {  ty = '" + toString(err.ty) + "' }";
    else if constexpr (std::is_same_v<T, UnexpectedTypePackInSubtyping>)
        stream << "UnexpectedTypePackInSubtyping {  tp = '" + toString(err.tp) + "' }";
    else if constexpr (std::is_same_v<T, UserDefinedTypeFunctionError>)
        stream << "UserDefinedTypeFunctionError { " << err.message << " }";
    else if constexpr (std::is_same_v<T, ReservedIdentifier>)
        stream << "ReservedIdentifier { " << err.name << " }";
    else if constexpr (std::is_same_v<T, CannotAssignToNever>)
    {
        stream << "CannotAssignToNever { rvalueType = '" << toString(err.rhsType) << "', reason = '" << err.reason << "', cause = { ";

        bool first = true;
        for (TypeId ty : err.cause)
        {
            if (first)
                first = false;
            else
                stream << ", ";

            stream << "'" << toString(ty) << "'";
        }

        stream << " } } ";
    }
    else if constexpr (std::is_same_v<T, UnexpectedArrayLikeTableItem>)
        stream << "UnexpectedArrayLikeTableItem {}";
    else if constexpr (std::is_same_v<T, CannotCheckDynamicStringFormatCalls>)
        stream << "CannotCheckDynamicStringFormatCalls {}";
    else if constexpr (std::is_same_v<T, GenericTypeCountMismatch>)
    {
        stream << "GenericTypeCountMismatch { subTyGenericCount = " << err.subTyGenericCount << ", superTyGenericCount = " << err.superTyGenericCount
               << " }";
    }
    else if constexpr (std::is_same_v<T, GenericTypePackCountMismatch>)
    {
        stream << "GenericTypePackCountMismatch { subTyGenericPackCount = " << err.subTyGenericPackCount
               << ", superTyGenericPackCount = " << err.superTyGenericPackCount << " }";
    }
    else if constexpr (std::is_same_v<T, MultipleNonviableOverloads>)
        stream << "MultipleNonviableOverloads { attemptedArgCount = " << err.attemptedArgCount << " }";
    else if constexpr (std::is_same_v<T, RecursiveRestraintViolation>)
        stream << "RecursiveRestraintViolation";
    else if constexpr (std::is_same_v<T, GenericBoundsMismatch>)
    {
        LUAU_ASSERT(FFlag::LuauSubtypingReportGenericBoundMismatches2);
        stream << "GenericBoundsMismatch { genericName = " << std::string{err.genericName} << ", lowerBounds = [";
        for (size_t i = 0; i < err.lowerBounds.size(); ++i)
        {
            if (i > 0)
                stream << ", ";
            stream << toString(err.lowerBounds[i]);
        }
        stream << "], upperBounds = [";
        for (size_t i = 0; i < err.upperBounds.size(); ++i)
        {
            if (i > 0)
                stream << ", ";
            stream << toString(err.upperBounds[i]);
        }
        stream << "] }";
    }
    else if constexpr (std::is_same_v<T, UnappliedTypeFunction>)
        stream << "UnappliedTypeFunction {}";
    else
        static_assert(always_false_v<T>, "Non-exhaustive type switch");
}

std::ostream& operator<<(std::ostream& stream, const CannotAssignToNever::Reason& reason)
{
    switch (reason)
    {
    case CannotAssignToNever::Reason::PropertyNarrowed:
        return stream << "PropertyNarrowed";
    default:
        return stream << "UnknownReason";
    }
}

std::ostream& operator<<(std::ostream& stream, const TypeErrorData& data)
{
    auto cb = [&](const auto& e)
    {
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

std::ostream& operator<<(std::ostream& stream, const Type& tv)
{
    return stream << toString(tv);
}

std::ostream& operator<<(std::ostream& stream, const TypePackVar& tv)
{
    return stream << toString(tv);
}

std::ostream& operator<<(std::ostream& stream, TypeId ty)
{
    // we commonly use a null pointer when a type may not be present; we need to
    // account for that here.
    if (!ty)
        return stream << "<nullptr>";

    return stream << toString(ty);
}

std::ostream& operator<<(std::ostream& stream, TypePackId tp)
{
    // we commonly use a null pointer when a type may not be present; we need to
    // account for that here.
    if (!tp)
        return stream << "<nullptr>";

    return stream << toString(tp);
}

namespace TypePath
{

std::ostream& operator<<(std::ostream& stream, const Path& path)
{
    return stream << toString(path);
}

} // namespace TypePath

} // namespace Luau
