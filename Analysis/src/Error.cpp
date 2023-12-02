// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Error.h"

#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/FileResolver.h"
#include "Luau/NotNull.h"
#include "Luau/StringUtils.h"
#include "Luau/ToString.h"

#include <optional>
#include <stdexcept>
#include <type_traits>

LUAU_FASTINTVARIABLE(LuauIndentTypeMismatchMaxTypeLength, 10)

static std::string wrongNumberOfArgsString(
    size_t expectedCount, std::optional<size_t> maximumCount, size_t actualCount, const char* argPrefix = nullptr, bool isVariadic = false)
{
    std::string s = "expects ";

    if (isVariadic)
        s += "at least ";

    s += std::to_string(expectedCount) + " ";

    if (maximumCount && expectedCount != *maximumCount)
        s += "to " + std::to_string(*maximumCount) + " ";

    if (argPrefix)
        s += std::string(argPrefix) + " ";

    s += "argument";
    if ((maximumCount ? *maximumCount : expectedCount) != 1)
        s += "s";

    s += ", but ";

    if (actualCount == 0)
    {
        s += "none";
    }
    else
    {
        if (actualCount < expectedCount)
            s += "only ";

        s += std::to_string(actualCount);
    }

    s += (actualCount == 1) ? " is" : " are";

    s += " specified";

    return s;
}

namespace Luau
{

struct ErrorConverter
{
    FileResolver* fileResolver = nullptr;

    std::string operator()(const Luau::TypeMismatch& tm) const
    {
        std::string givenTypeName = Luau::toString(tm.givenType);
        std::string wantedTypeName = Luau::toString(tm.wantedType);

        std::string result;

        auto quote = [&](std::string s) {
            return "'" + s + "'";
        };

        auto constructErrorMessage = [&](std::string givenType, std::string wantedType, std::optional<std::string> givenModule,
                                         std::optional<std::string> wantedModule) -> std::string {
            std::string given = givenModule ? quote(givenType) + " from " + quote(*givenModule) : quote(givenType);
            std::string wanted = wantedModule ? quote(wantedType) + " from " + quote(*wantedModule) : quote(wantedType);
            size_t luauIndentTypeMismatchMaxTypeLength = size_t(FInt::LuauIndentTypeMismatchMaxTypeLength);
            if (givenType.length() <= luauIndentTypeMismatchMaxTypeLength || wantedType.length() <= luauIndentTypeMismatchMaxTypeLength)
                return "Type " + given + " could not be converted into " + wanted;
            return "Type\n    " + given + "\ncould not be converted into\n    " + wanted;
        };

        if (givenTypeName == wantedTypeName)
        {
            if (auto givenDefinitionModule = getDefinitionModuleName(tm.givenType))
            {
                if (auto wantedDefinitionModule = getDefinitionModuleName(tm.wantedType))
                {
                    if (fileResolver != nullptr)
                    {
                        std::string givenModuleName = fileResolver->getHumanReadableModuleName(*givenDefinitionModule);
                        std::string wantedModuleName = fileResolver->getHumanReadableModuleName(*wantedDefinitionModule);
                        result = constructErrorMessage(givenTypeName, wantedTypeName, givenModuleName, wantedModuleName);
                    }
                    else
                    {
                        result = constructErrorMessage(givenTypeName, wantedTypeName, *givenDefinitionModule, *wantedDefinitionModule);
                    }
                }
            }
        }

        if (result.empty())
            result = constructErrorMessage(givenTypeName, wantedTypeName, std::nullopt, std::nullopt);


        if (tm.error)
        {
            result += "\ncaused by:\n  ";

            if (!tm.reason.empty())
                result += tm.reason + "\n";

            result += Luau::toString(*tm.error, TypeErrorToStringOptions{fileResolver});
        }
        else if (!tm.reason.empty())
        {
            result += "; " + tm.reason;
        }
        else if (tm.context == TypeMismatch::InvariantContext)
        {
            result += " in an invariant context";
        }

        return result;
    }

    std::string operator()(const Luau::UnknownSymbol& e) const
    {
        switch (e.context)
        {
        case UnknownSymbol::Binding:
            return "Unknown global '" + e.name + "'";
        case UnknownSymbol::Type:
            return "Unknown type '" + e.name + "'";
        }

        LUAU_ASSERT(!"Unexpected context for UnknownSymbol");
        return "";
    }

    std::string operator()(const Luau::UnknownProperty& e) const
    {
        TypeId t = follow(e.table);
        if (get<TableType>(t))
            return "Key '" + e.key + "' not found in table '" + Luau::toString(t) + "'";
        else if (get<ClassType>(t))
            return "Key '" + e.key + "' not found in class '" + Luau::toString(t) + "'";
        else
            return "Type '" + Luau::toString(e.table) + "' does not have key '" + e.key + "'";
    }

    std::string operator()(const Luau::NotATable& e) const
    {
        return "Expected type table, got '" + Luau::toString(e.ty) + "' instead";
    }

    std::string operator()(const Luau::CannotExtendTable& e) const
    {
        switch (e.context)
        {
        case Luau::CannotExtendTable::Property:
            return "Cannot add property '" + e.prop + "' to table '" + Luau::toString(e.tableType) + "'";
        case Luau::CannotExtendTable::Metatable:
            return "Cannot add metatable to table '" + Luau::toString(e.tableType) + "'";
        case Luau::CannotExtendTable::Indexer:
            return "Cannot add indexer to table '" + Luau::toString(e.tableType) + "'";
        }

        LUAU_ASSERT(!"Unknown context");
        return "";
    }

    std::string operator()(const Luau::OnlyTablesCanHaveMethods& e) const
    {
        return "Cannot add method to non-table type '" + Luau::toString(e.tableType) + "'";
    }

    std::string operator()(const Luau::DuplicateTypeDefinition& e) const
    {
        std::string s = "Redefinition of type '" + e.name + "'";
        if (e.previousLocation)
            s += ", previously defined at line " + std::to_string(e.previousLocation->begin.line + 1);
        return s;
    }

    std::string operator()(const Luau::CountMismatch& e) const
    {
        const std::string expectedS = e.expected == 1 ? "" : "s";
        const std::string actualS = e.actual == 1 ? "" : "s";
        const std::string actualVerb = e.actual == 1 ? "is" : "are";

        switch (e.context)
        {
        case CountMismatch::Return:
            return "Expected to return " + std::to_string(e.expected) + " value" + expectedS + ", but " + std::to_string(e.actual) + " " +
                   actualVerb + " returned here";
        case CountMismatch::FunctionResult:
            // It is alright if right hand side produces more values than the
            // left hand side accepts. In this context consider only the opposite case.
            return "Function only returns " + std::to_string(e.expected) + " value" + expectedS + ", but " + std::to_string(e.actual) + " " +
                   actualVerb + " required here";
        case CountMismatch::ExprListResult:
            return "Expression list has " + std::to_string(e.expected) + " value" + expectedS + ", but " + std::to_string(e.actual) + " " +
                   actualVerb + " required here";
        case CountMismatch::Arg:
            if (!e.function.empty())
                return "Argument count mismatch. Function '" + e.function + "' " +
                       wrongNumberOfArgsString(e.expected, e.maximum, e.actual, /*argPrefix*/ nullptr, e.isVariadic);
            else
                return "Argument count mismatch. Function " +
                       wrongNumberOfArgsString(e.expected, e.maximum, e.actual, /*argPrefix*/ nullptr, e.isVariadic);
        }

        LUAU_ASSERT(!"Unknown context");
        return "";
    }

    std::string operator()(const Luau::FunctionDoesNotTakeSelf&) const
    {
        return std::string("This function does not take self. Did you mean to use a dot instead of a colon?");
    }

    std::string operator()(const Luau::FunctionRequiresSelf& e) const
    {
        return "This function must be called with self. Did you mean to use a colon instead of a dot?";
    }

    std::string operator()(const Luau::OccursCheckFailed&) const
    {
        return "Type contains a self-recursive construct that cannot be resolved";
    }

    std::string operator()(const Luau::UnknownRequire& e) const
    {
        if (e.modulePath.empty())
            return "Unknown require: unsupported path";
        else
            return "Unknown require: " + e.modulePath;
    }

    std::string operator()(const Luau::IncorrectGenericParameterCount& e) const
    {
        std::string name = e.name;
        if (!e.typeFun.typeParams.empty() || !e.typeFun.typePackParams.empty())
        {
            name += "<";
            bool first = true;
            for (auto param : e.typeFun.typeParams)
            {
                if (first)
                    first = false;
                else
                    name += ", ";

                name += toString(param.ty);
            }

            for (auto param : e.typeFun.typePackParams)
            {
                if (first)
                    first = false;
                else
                    name += ", ";

                name += toString(param.tp);
            }

            name += ">";
        }

        if (e.typeFun.typeParams.size() != e.actualParameters)
            return "Generic type '" + name + "' " +
                   wrongNumberOfArgsString(e.typeFun.typeParams.size(), std::nullopt, e.actualParameters, "type", !e.typeFun.typePackParams.empty());

        return "Generic type '" + name + "' " +
               wrongNumberOfArgsString(e.typeFun.typePackParams.size(), std::nullopt, e.actualPackParameters, "type pack", /*isVariadic*/ false);
    }

    std::string operator()(const Luau::SyntaxError& e) const
    {
        return e.message;
    }

    std::string operator()(const Luau::CodeTooComplex&) const
    {
        return "Code is too complex to typecheck! Consider simplifying the code around this area";
    }

    std::string operator()(const Luau::UnificationTooComplex&) const
    {
        return "Internal error: Code is too complex to typecheck! Consider adding type annotations around this area";
    }

    std::string operator()(const Luau::UnknownPropButFoundLikeProp& e) const
    {
        std::string candidatesSuggestion = "Did you mean ";
        if (e.candidates.size() != 1)
            candidatesSuggestion += "one of ";

        bool first = true;
        for (Name name : e.candidates)
        {
            if (first)
                first = false;
            else
                candidatesSuggestion += ", ";

            candidatesSuggestion += "'" + name + "'";
        }

        std::string s = "Key '" + e.key + "' not found in ";

        TypeId t = follow(e.table);
        if (get<ClassType>(t))
            s += "class";
        else
            s += "table";

        s += " '" + toString(e.table) + "'.  " + candidatesSuggestion + "?";
        return s;
    }

    std::string operator()(const Luau::GenericError& e) const
    {
        return e.message;
    }

    std::string operator()(const Luau::InternalError& e) const
    {
        return e.message;
    }

    std::string operator()(const Luau::CannotCallNonFunction& e) const
    {
        return "Cannot call non-function " + toString(e.ty);
    }
    std::string operator()(const Luau::ExtraInformation& e) const
    {
        return e.message;
    }

    std::string operator()(const Luau::DeprecatedApiUsed& e) const
    {
        return "The property ." + e.symbol + " is deprecated.  Use ." + e.useInstead + " instead.";
    }

    std::string operator()(const Luau::ModuleHasCyclicDependency& e) const
    {
        if (e.cycle.empty())
            return "Cyclic module dependency detected";

        std::string s = "Cyclic module dependency: ";

        bool first = true;
        for (const ModuleName& name : e.cycle)
        {
            if (first)
                first = false;
            else
                s += " -> ";

            if (fileResolver != nullptr)
                s += fileResolver->getHumanReadableModuleName(name);
            else
                s += name;
        }

        return s;
    }

    std::string operator()(const Luau::FunctionExitsWithoutReturning& e) const
    {
        return "Not all codepaths in this function return '" + toString(e.expectedReturnType) + "'.";
    }

    std::string operator()(const Luau::IllegalRequire& e) const
    {
        return "Cannot require module " + e.moduleName + ": " + e.reason;
    }

    std::string operator()(const Luau::MissingProperties& e) const
    {
        std::string s = "Table type '" + toString(e.subType) + "' not compatible with type '" + toString(e.superType) + "' because the former";

        switch (e.context)
        {
        case MissingProperties::Missing:
            s += " is missing field";
            break;
        case MissingProperties::Extra:
            s += " has extra field";
            break;
        }

        if (e.properties.size() > 1)
            s += "s";

        s += " ";

        for (size_t i = 0; i < e.properties.size(); ++i)
        {
            if (i > 0)
                s += ", ";

            if (i > 0 && i == e.properties.size() - 1)
                s += "and ";

            s += "'" + e.properties[i] + "'";
        }

        return s;
    }

    std::string operator()(const Luau::DuplicateGenericParameter& e) const
    {
        return "Duplicate type parameter '" + e.parameterName + "'";
    }

    std::string operator()(const Luau::CannotInferBinaryOperation& e) const
    {
        std::string ss = "Unknown type used in " + toString(e.op);

        switch (e.kind)
        {
        case Luau::CannotInferBinaryOperation::Comparison:
            ss += " comparison";
            break;
        case Luau::CannotInferBinaryOperation::Operation:
            ss += " operation";
        }

        if (e.suggestedToAnnotate)
            ss += "; consider adding a type annotation to '" + *e.suggestedToAnnotate + "'";

        return ss;
    }

    std::string operator()(const Luau::SwappedGenericTypeParameter& e) const
    {
        switch (e.kind)
        {
        case Luau::SwappedGenericTypeParameter::Type:
            return "Variadic type parameter '" + e.name + "...' is used as a regular generic type; consider changing '" + e.name + "...' to '" +
                   e.name + "' in the generic argument list";
        case Luau::SwappedGenericTypeParameter::Pack:
            return "Generic type '" + e.name + "' is used as a variadic type parameter; consider changing '" + e.name + "' to '" + e.name +
                   "...' in the generic argument list";
        default:
            LUAU_ASSERT(!"Unknown kind");
            return "";
        }
    }

    std::string operator()(const Luau::OptionalValueAccess& e) const
    {
        return "Value of type '" + toString(e.optional) + "' could be nil";
    }

    std::string operator()(const Luau::MissingUnionProperty& e) const
    {
        std::string ss = "Key '" + e.key + "' is missing from ";

        bool first = true;
        for (auto ty : e.missing)
        {
            if (first)
                first = false;
            else
                ss += ", ";

            ss += "'" + toString(ty) + "'";
        }

        return ss + " in the type '" + toString(e.type) + "'";
    }

    std::string operator()(const TypesAreUnrelated& e) const
    {
        return "Cannot cast '" + toString(e.left) + "' into '" + toString(e.right) + "' because the types are unrelated";
    }

    std::string operator()(const NormalizationTooComplex&) const
    {
        return "Code is too complex to typecheck! Consider simplifying the code around this area";
    }

    std::string operator()(const TypePackMismatch& e) const
    {
        std::string ss = "Type pack '" + toString(e.givenTp) + "' could not be converted into '" + toString(e.wantedTp) + "'";

        if (!e.reason.empty())
            ss += "; " + e.reason;

        return ss;
    }

    std::string operator()(const DynamicPropertyLookupOnClassesUnsafe& e) const
    {
        return "Attempting a dynamic property access on type '" + Luau::toString(e.ty) + "' is unsafe and may cause exceptions at runtime";
    }

    std::string operator()(const UninhabitedTypeFamily& e) const
    {
        return "Type family instance " + Luau::toString(e.ty) + " is uninhabited";
    }

    std::string operator()(const UninhabitedTypePackFamily& e) const
    {
        return "Type pack family instance " + Luau::toString(e.tp) + " is uninhabited";
    }

    std::string operator()(const WhereClauseNeeded& e) const
    {
        return "Type family instance " + Luau::toString(e.ty) +
               " depends on generic function parameters but does not appear in the function signature; this construct cannot be type-checked at this "
               "time";
    }

    std::string operator()(const PackWhereClauseNeeded& e) const
    {
        return "Type pack family instance " + Luau::toString(e.tp) +
               " depends on generic function parameters but does not appear in the function signature; this construct cannot be type-checked at this "
               "time";
    }

    std::string operator()(const CheckedFunctionCallError& e) const
    {
        // TODO: What happens if checkedFunctionName cannot be found??
        return "Function '" + e.checkedFunctionName + "' expects '" + toString(e.expected) + "' at argument #" + std::to_string(e.argumentIndex) +
               ", but got '" + Luau::toString(e.passed) + "'";
    }

    std::string operator()(const NonStrictFunctionDefinitionError& e) const
    {
        return "Argument " + e.argument + " with type '" + toString(e.argumentType) + "' in function '" + e.functionName +
               "' is used in a way that will run time error";
    }
};

struct InvalidNameChecker
{
    std::string invalidName = "%error-id%";

    bool operator()(const Luau::UnknownProperty& e) const
    {
        return e.key == invalidName;
    }
    bool operator()(const Luau::CannotExtendTable& e) const
    {
        return e.prop == invalidName;
    }
    bool operator()(const Luau::DuplicateTypeDefinition& e) const
    {
        return e.name == invalidName;
    }

    template<typename T>
    bool operator()(const T& other) const
    {
        return false;
    }
};

TypeMismatch::TypeMismatch(TypeId wantedType, TypeId givenType)
    : wantedType(wantedType)
    , givenType(givenType)
{
}

TypeMismatch::TypeMismatch(TypeId wantedType, TypeId givenType, std::string reason)
    : wantedType(wantedType)
    , givenType(givenType)
    , reason(reason)
{
}

TypeMismatch::TypeMismatch(TypeId wantedType, TypeId givenType, std::string reason, std::optional<TypeError> error)
    : wantedType(wantedType)
    , givenType(givenType)
    , reason(reason)
    , error(error ? std::make_shared<TypeError>(std::move(*error)) : nullptr)
{
}

TypeMismatch::TypeMismatch(TypeId wantedType, TypeId givenType, TypeMismatch::Context context)
    : wantedType(wantedType)
    , givenType(givenType)
    , context(context)
{
}

TypeMismatch::TypeMismatch(TypeId wantedType, TypeId givenType, std::string reason, TypeMismatch::Context context)
    : wantedType(wantedType)
    , givenType(givenType)
    , context(context)
    , reason(reason)
{
}

TypeMismatch::TypeMismatch(TypeId wantedType, TypeId givenType, std::string reason, std::optional<TypeError> error, TypeMismatch::Context context)
    : wantedType(wantedType)
    , givenType(givenType)
    , context(context)
    , reason(reason)
    , error(error ? std::make_shared<TypeError>(std::move(*error)) : nullptr)
{
}

bool TypeMismatch::operator==(const TypeMismatch& rhs) const
{
    if (!!error != !!rhs.error)
        return false;

    if (error && !(*error == *rhs.error))
        return false;

    return *wantedType == *rhs.wantedType && *givenType == *rhs.givenType && reason == rhs.reason && context == rhs.context;
}

bool UnknownSymbol::operator==(const UnknownSymbol& rhs) const
{
    return name == rhs.name;
}

bool UnknownProperty::operator==(const UnknownProperty& rhs) const
{
    return *table == *rhs.table && key == rhs.key;
}

bool NotATable::operator==(const NotATable& rhs) const
{
    return ty == rhs.ty;
}

bool CannotExtendTable::operator==(const CannotExtendTable& rhs) const
{
    return *tableType == *rhs.tableType && prop == rhs.prop && context == rhs.context;
}

bool OnlyTablesCanHaveMethods::operator==(const OnlyTablesCanHaveMethods& rhs) const
{
    return *tableType == *rhs.tableType;
}

bool DuplicateTypeDefinition::operator==(const DuplicateTypeDefinition& rhs) const
{
    return name == rhs.name && previousLocation == rhs.previousLocation;
}

bool CountMismatch::operator==(const CountMismatch& rhs) const
{
    return expected == rhs.expected && maximum == rhs.maximum && actual == rhs.actual && context == rhs.context && function == rhs.function;
}

bool FunctionDoesNotTakeSelf::operator==(const FunctionDoesNotTakeSelf&) const
{
    return true;
}

bool FunctionRequiresSelf::operator==(const FunctionRequiresSelf& e) const
{
    return true;
}

bool OccursCheckFailed::operator==(const OccursCheckFailed&) const
{
    return true;
}

bool UnknownRequire::operator==(const UnknownRequire& rhs) const
{
    return modulePath == rhs.modulePath;
}

bool IncorrectGenericParameterCount::operator==(const IncorrectGenericParameterCount& rhs) const
{
    if (name != rhs.name)
        return false;

    if (typeFun.type != rhs.typeFun.type)
        return false;

    if (typeFun.typeParams.size() != rhs.typeFun.typeParams.size())
        return false;

    if (typeFun.typePackParams.size() != rhs.typeFun.typePackParams.size())
        return false;

    for (size_t i = 0; i < typeFun.typeParams.size(); ++i)
    {
        if (typeFun.typeParams[i].ty != rhs.typeFun.typeParams[i].ty)
            return false;
    }

    for (size_t i = 0; i < typeFun.typePackParams.size(); ++i)
    {
        if (typeFun.typePackParams[i].tp != rhs.typeFun.typePackParams[i].tp)
            return false;
    }

    return true;
}

bool SyntaxError::operator==(const SyntaxError& rhs) const
{
    return message == rhs.message;
}

bool CodeTooComplex::operator==(const CodeTooComplex&) const
{
    return true;
}

bool UnificationTooComplex::operator==(const UnificationTooComplex&) const
{
    return true;
}

bool UnknownPropButFoundLikeProp::operator==(const UnknownPropButFoundLikeProp& rhs) const
{
    return *table == *rhs.table && key == rhs.key && candidates.size() == rhs.candidates.size() &&
           std::equal(candidates.begin(), candidates.end(), rhs.candidates.begin());
}

bool GenericError::operator==(const GenericError& rhs) const
{
    return message == rhs.message;
}

bool InternalError::operator==(const InternalError& rhs) const
{
    return message == rhs.message;
}

bool CannotCallNonFunction::operator==(const CannotCallNonFunction& rhs) const
{
    return ty == rhs.ty;
}

bool ExtraInformation::operator==(const ExtraInformation& rhs) const
{
    return message == rhs.message;
}

bool DeprecatedApiUsed::operator==(const DeprecatedApiUsed& rhs) const
{
    return symbol == rhs.symbol && useInstead == rhs.useInstead;
}

bool FunctionExitsWithoutReturning::operator==(const FunctionExitsWithoutReturning& rhs) const
{
    return expectedReturnType == rhs.expectedReturnType;
}

int TypeError::code() const
{
    return minCode() + int(data.index());
}

int TypeError::minCode()
{
    return 1000;
}

TypeErrorSummary TypeError::summary() const
{
    return TypeErrorSummary{location, moduleName, code()};
}

bool TypeError::operator==(const TypeError& rhs) const
{
    return location == rhs.location && data == rhs.data;
}

bool ModuleHasCyclicDependency::operator==(const ModuleHasCyclicDependency& rhs) const
{
    return cycle.size() == rhs.cycle.size() && std::equal(cycle.begin(), cycle.end(), rhs.cycle.begin());
}

bool IllegalRequire::operator==(const IllegalRequire& rhs) const
{
    return moduleName == rhs.moduleName && reason == rhs.reason;
}

bool MissingProperties::operator==(const MissingProperties& rhs) const
{
    return *superType == *rhs.superType && *subType == *rhs.subType && properties.size() == rhs.properties.size() &&
           std::equal(properties.begin(), properties.end(), rhs.properties.begin()) && context == rhs.context;
}

bool DuplicateGenericParameter::operator==(const DuplicateGenericParameter& rhs) const
{
    return parameterName == rhs.parameterName;
}

bool CannotInferBinaryOperation::operator==(const CannotInferBinaryOperation& rhs) const
{
    return op == rhs.op && suggestedToAnnotate == rhs.suggestedToAnnotate && kind == rhs.kind;
}

bool SwappedGenericTypeParameter::operator==(const SwappedGenericTypeParameter& rhs) const
{
    return name == rhs.name && kind == rhs.kind;
}

bool OptionalValueAccess::operator==(const OptionalValueAccess& rhs) const
{
    return *optional == *rhs.optional;
}

bool MissingUnionProperty::operator==(const MissingUnionProperty& rhs) const
{
    if (missing.size() != rhs.missing.size())
        return false;

    for (size_t i = 0; i < missing.size(); ++i)
    {
        if (*missing[i] != *rhs.missing[i])
            return false;
    }

    return *type == *rhs.type && key == rhs.key;
}

bool TypesAreUnrelated::operator==(const TypesAreUnrelated& rhs) const
{
    return left == rhs.left && right == rhs.right;
}

bool TypePackMismatch::operator==(const TypePackMismatch& rhs) const
{
    return *wantedTp == *rhs.wantedTp && *givenTp == *rhs.givenTp;
}

bool DynamicPropertyLookupOnClassesUnsafe::operator==(const DynamicPropertyLookupOnClassesUnsafe& rhs) const
{
    return ty == rhs.ty;
}

bool UninhabitedTypeFamily::operator==(const UninhabitedTypeFamily& rhs) const
{
    return ty == rhs.ty;
}

bool UninhabitedTypePackFamily::operator==(const UninhabitedTypePackFamily& rhs) const
{
    return tp == rhs.tp;
}

bool WhereClauseNeeded::operator==(const WhereClauseNeeded& rhs) const
{
    return ty == rhs.ty;
}

bool PackWhereClauseNeeded::operator==(const PackWhereClauseNeeded& rhs) const
{
    return tp == rhs.tp;
}

bool CheckedFunctionCallError::operator==(const CheckedFunctionCallError& rhs) const
{
    return *expected == *rhs.expected && *passed == *rhs.passed && checkedFunctionName == rhs.checkedFunctionName &&
           argumentIndex == rhs.argumentIndex;
}

bool NonStrictFunctionDefinitionError::operator==(const NonStrictFunctionDefinitionError& rhs) const
{
    return functionName == rhs.functionName && argument == rhs.argument && argumentType == rhs.argumentType;
}

std::string toString(const TypeError& error)
{
    return toString(error, TypeErrorToStringOptions{});
}

std::string toString(const TypeError& error, TypeErrorToStringOptions options)
{
    ErrorConverter converter{options.fileResolver};
    return Luau::visit(converter, error.data);
}

bool containsParseErrorName(const TypeError& error)
{
    return Luau::visit(InvalidNameChecker{}, error.data);
}

template<typename T>
void copyError(T& e, TypeArena& destArena, CloneState& cloneState)
{
    auto clone = [&](auto&& ty) {
        return ::Luau::clone(ty, destArena, cloneState);
    };

    auto visitErrorData = [&](auto&& e) {
        copyError(e, destArena, cloneState);
    };

    if constexpr (false)
    {
    }
    else if constexpr (std::is_same_v<T, TypeMismatch>)
    {
        e.wantedType = clone(e.wantedType);
        e.givenType = clone(e.givenType);

        if (e.error)
            visit(visitErrorData, e.error->data);
    }
    else if constexpr (std::is_same_v<T, UnknownSymbol>)
    {
    }
    else if constexpr (std::is_same_v<T, UnknownProperty>)
    {
        e.table = clone(e.table);
    }
    else if constexpr (std::is_same_v<T, NotATable>)
    {
        e.ty = clone(e.ty);
    }
    else if constexpr (std::is_same_v<T, CannotExtendTable>)
    {
        e.tableType = clone(e.tableType);
    }
    else if constexpr (std::is_same_v<T, OnlyTablesCanHaveMethods>)
    {
        e.tableType = clone(e.tableType);
    }
    else if constexpr (std::is_same_v<T, DuplicateTypeDefinition>)
    {
    }
    else if constexpr (std::is_same_v<T, CountMismatch>)
    {
    }
    else if constexpr (std::is_same_v<T, FunctionDoesNotTakeSelf>)
    {
    }
    else if constexpr (std::is_same_v<T, FunctionRequiresSelf>)
    {
    }
    else if constexpr (std::is_same_v<T, OccursCheckFailed>)
    {
    }
    else if constexpr (std::is_same_v<T, UnknownRequire>)
    {
    }
    else if constexpr (std::is_same_v<T, IncorrectGenericParameterCount>)
    {
        e.typeFun = clone(e.typeFun);
    }
    else if constexpr (std::is_same_v<T, SyntaxError>)
    {
    }
    else if constexpr (std::is_same_v<T, CodeTooComplex>)
    {
    }
    else if constexpr (std::is_same_v<T, UnificationTooComplex>)
    {
    }
    else if constexpr (std::is_same_v<T, UnknownPropButFoundLikeProp>)
    {
        e.table = clone(e.table);
    }
    else if constexpr (std::is_same_v<T, GenericError>)
    {
    }
    else if constexpr (std::is_same_v<T, InternalError>)
    {
    }
    else if constexpr (std::is_same_v<T, CannotCallNonFunction>)
    {
        e.ty = clone(e.ty);
    }
    else if constexpr (std::is_same_v<T, ExtraInformation>)
    {
    }
    else if constexpr (std::is_same_v<T, DeprecatedApiUsed>)
    {
    }
    else if constexpr (std::is_same_v<T, ModuleHasCyclicDependency>)
    {
    }
    else if constexpr (std::is_same_v<T, IllegalRequire>)
    {
    }
    else if constexpr (std::is_same_v<T, FunctionExitsWithoutReturning>)
    {
        e.expectedReturnType = clone(e.expectedReturnType);
    }
    else if constexpr (std::is_same_v<T, DuplicateGenericParameter>)
    {
    }
    else if constexpr (std::is_same_v<T, CannotInferBinaryOperation>)
    {
    }
    else if constexpr (std::is_same_v<T, MissingProperties>)
    {
        e.superType = clone(e.superType);
        e.subType = clone(e.subType);
    }
    else if constexpr (std::is_same_v<T, SwappedGenericTypeParameter>)
    {
    }
    else if constexpr (std::is_same_v<T, OptionalValueAccess>)
    {
        e.optional = clone(e.optional);
    }
    else if constexpr (std::is_same_v<T, MissingUnionProperty>)
    {
        e.type = clone(e.type);

        for (auto& ty : e.missing)
            ty = clone(ty);
    }
    else if constexpr (std::is_same_v<T, TypesAreUnrelated>)
    {
        e.left = clone(e.left);
        e.right = clone(e.right);
    }
    else if constexpr (std::is_same_v<T, NormalizationTooComplex>)
    {
    }
    else if constexpr (std::is_same_v<T, TypePackMismatch>)
    {
        e.wantedTp = clone(e.wantedTp);
        e.givenTp = clone(e.givenTp);
    }
    else if constexpr (std::is_same_v<T, DynamicPropertyLookupOnClassesUnsafe>)
        e.ty = clone(e.ty);
    else if constexpr (std::is_same_v<T, UninhabitedTypeFamily>)
        e.ty = clone(e.ty);
    else if constexpr (std::is_same_v<T, UninhabitedTypePackFamily>)
        e.tp = clone(e.tp);
    else if constexpr (std::is_same_v<T, WhereClauseNeeded>)
        e.ty = clone(e.ty);
    else if constexpr (std::is_same_v<T, PackWhereClauseNeeded>)
        e.tp = clone(e.tp);
    else if constexpr (std::is_same_v<T, CheckedFunctionCallError>)
    {
        e.expected = clone(e.expected);
        e.passed = clone(e.passed);
    }
    else if constexpr (std::is_same_v<T, NonStrictFunctionDefinitionError>)
    {
        e.argumentType = clone(e.argumentType);
    }
    else
        static_assert(always_false_v<T>, "Non-exhaustive type switch");
}

void copyErrors(ErrorVec& errors, TypeArena& destArena, NotNull<BuiltinTypes> builtinTypes)
{
    CloneState cloneState{builtinTypes};

    auto visitErrorData = [&](auto&& e) {
        copyError(e, destArena, cloneState);
    };

    LUAU_ASSERT(!destArena.types.isFrozen());
    LUAU_ASSERT(!destArena.typePacks.isFrozen());

    for (TypeError& error : errors)
        visit(visitErrorData, error.data);
}

void InternalErrorReporter::ice(const std::string& message, const Location& location) const
{
    InternalCompilerError error(message, moduleName, location);

    if (onInternalError)
        onInternalError(error.what());

    throw error;
}

void InternalErrorReporter::ice(const std::string& message) const
{
    InternalCompilerError error(message, moduleName);

    if (onInternalError)
        onInternalError(error.what());

    throw error;
}

const char* InternalCompilerError::what() const throw()
{
    return this->message.data();
}

} // namespace Luau
