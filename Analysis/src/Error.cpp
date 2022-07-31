// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lluz/Error.h"

#include "lluz/Clone.h"
#include "lluz/StringUtils.h"
#include "lluz/ToString.h"

#include <stdexcept>

lluz_FASTFLAGVARIABLE(LluTypeMismatchModuleNameResolution, false)
lluz_FASTFLAGVARIABLE(LluUseInternalCompilerErrorException, false)

static std::string wrongNumberOfArgsString(size_t expectedCount, size_t actualCount, const char* argPrefix = nullptr, bool isVariadic = false)
{
    std::string s = "expects ";

    if (isVariadic)
        s += "at least ";

    s += std::to_string(expectedCount) + " ";

    if (argPrefix)
        s += std::string(argPrefix) + " ";

    s += "argument";
    if (expectedCount != 1)
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

namespace lluz
{

struct ErrorConverter
{
    FileResolver* fileResolver = nullptr;

    std::string operator()(const lluz::TypeMismatch& tm) const
    {
        std::string givenTypeName = lluz::toString(tm.givenType);
        std::string wantedTypeName = lluz::toString(tm.wantedType);

        std::string result;

        if (givenTypeName == wantedTypeName)
        {
            if (auto givenDefinitionModule = getDefinitionModuleName(tm.givenType))
            {
                if (auto wantedDefinitionModule = getDefinitionModuleName(tm.wantedType))
                {
                    if (FFlag::LluTypeMismatchModuleNameResolution && fileResolver != nullptr)
                    {
                        std::string givenModuleName = fileResolver->getHumanReadableModuleName(*givenDefinitionModule);
                        std::string wantedModuleName = fileResolver->getHumanReadableModuleName(*wantedDefinitionModule);
                        result = "Type '" + givenTypeName + "' from '" + givenModuleName + "' could not be converted into '" + wantedTypeName +
                                 "' from '" + wantedModuleName + "'";
                    }
                    else
                    {
                        result = "Type '" + givenTypeName + "' from '" + *givenDefinitionModule + "' could not be converted into '" + wantedTypeName +
                                 "' from '" + *wantedDefinitionModule + "'";
                    }
                }
            }
        }

        if (result.empty())
            result = "Type '" + givenTypeName + "' could not be converted into '" + wantedTypeName + "'";

        if (tm.error)
        {
            result += "\ncaused by:\n  ";

            if (!tm.reason.empty())
                result += tm.reason + " ";

            if (FFlag::LluTypeMismatchModuleNameResolution)
            {
                result += lluz::toString(*tm.error, TypeErrorToStringOptions{fileResolver});
            }
            else
            {
                result += lluz::toString(*tm.error);
            }
        }
        else if (!tm.reason.empty())
        {
            result += "; " + tm.reason;
        }

        return result;
    }

    std::string operator()(const lluz::UnknownSymbol& e) const
    {
        switch (e.context)
        {
        case UnknownSymbol::Binding:
            return XorStr("Unknown global '") + e.name + "'";
        case UnknownSymbol::Type:
            return XorStr("Unknown type '") + e.name + "'";
        case UnknownSymbol::Generic:
            return XorStr("Unknown generic '") + e.name + "'";
        }

        lluz_ASSERT(!XorStr("Unexpected context for UnknownSymbol"));
        return XorStr("");
    }

    std::string operator()(const lluz::UnknownProperty& e) const
    {
        TypeId t = follow(e.table);
        if (get<TableTypeVar>(t))
            return XorStr("Key '") + e.key + "' not found in table '" + lluz::toString(t) + "'";
        else if (get<ClassTypeVar>(t))
            return XorStr("Key '") + e.key + "' not found in class '" + lluz::toString(t) + "'";
        else
            return XorStr("Type '") + lluz::toString(e.table) + "' does not have key '" + e.key + "'";
    }

    std::string operator()(const lluz::NotATable& e) const
    {
        return XorStr("Expected type table, got '") + lluz::toString(e.ty) + "' instead";
    }

    std::string operator()(const lluz::CannotExtendTable& e) const
    {
        switch (e.context)
        {
        case lluz::CannotExtendTable::Property:
            return XorStr("Cannot add property '") + e.prop + "' to table '" + lluz::toString(e.tableType) + "'";
        case lluz::CannotExtendTable::Metatable:
            return XorStr("Cannot add metatable to table '") + lluz::toString(e.tableType) + "'";
        case lluz::CannotExtendTable::Indexer:
            return XorStr("Cannot add indexer to table '") + lluz::toString(e.tableType) + "'";
        }

        lluz_ASSERT(!XorStr("Unknown context"));
        return XorStr("");
    }

    std::string operator()(const lluz::OnlyTablesCanHaveMethods& e) const
    {
        return XorStr("Cannot add method to non-table type '") + lluz::toString(e.tableType) + "'";
    }

    std::string operator()(const lluz::DuplicateTypeDefinition& e) const
    {
        return XorStr("Redefinition of type '") + e.name + "', previously defined at line " + std::to_string(e.previousLocation.begin.line + 1);
    }

    std::string operator()(const lluz::CountMismatch& e) const
    {
        const std::string expectedS = e.expected == 1 ? "" : "s";
        const std::string actualS = e.actual == 1 ? "" : "s";
        const std::string actualVerb = e.actual == 1 ? "is" : "are";

        switch (e.context)
        {
        case CountMismatch::Return:
            return XorStr("Expected to return ") + std::to_string(e.expected) + " value" + expectedS + ", but " + std::to_string(e.actual) + " " +
                   actualVerb + " returned here";
        case CountMismatch::Result:
            // It is alright if right hand side produces more values than the
            // left hand side accepts. In this context consider only the opposite case.
            return XorStr("Function only returns ") + std::to_string(e.expected) + " value" + expectedS + ". " + std::to_string(e.actual) +
                   " are required here";
        case CountMismatch::Arg:
            return XorStr("Argument count mismatch. Function ") + wrongNumberOfArgsString(e.expected, e.actual, /*argPrefix*/ nullptr, e.isVariadic);
        }

        lluz_ASSERT(!XorStr("Unknown context"));
        return XorStr("");
    }

    std::string operator()(const lluz::FunctionDoesNotTakeSelf&) const
    {
        return std::string(XorStr("This function does not take self. Did you mean to use a dot instead of a colon?"));
    }

    std::string operator()(const lluz::FunctionRequiresSelf& e) const
    {
        return XorStr("This function must be called with self. Did you mean to use a colon instead of a dot?");
    }

    std::string operator()(const lluz::OccursCheckFailed&) const
    {
        return XorStr("Type contains a self-recursive construct that cannot be resolved");
    }

    std::string operator()(const lluz::UnknownRequire& e) const
    {
        if (e.modulePath.empty())
            return XorStr("Unknown require: unsupported path");
        else
            return XorStr("Unknown require: ") + e.modulePath;
    }

    std::string operator()(const lluz::IncorrectGenericParameterCount& e) const
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
            return XorStr("Generic type '") + name + "' " +
                   wrongNumberOfArgsString(e.typeFun.typeParams.size(), e.actualParameters, "type", !e.typeFun.typePackParams.empty());

        return XorStr("Generic type '") + name + "' " +
               wrongNumberOfArgsString(e.typeFun.typePackParams.size(), e.actualPackParameters, "type pack", /*isVariadic*/ false);
    }

    std::string operator()(const lluz::SyntaxError& e) const
    {
        return e.message;
    }

    std::string operator()(const lluz::CodeTooComplex&) const
    {
        return XorStr("Code is too complex to typecheck! Consider simplifying the code around this area");
    }

    std::string operator()(const lluz::UnificationTooComplex&) const
    {
        return XorStr("Internal error: Code is too complex to typecheck! Consider adding type annotations around this area");
    }

    std::string operator()(const lluz::UnknownPropButFoundLikeProp& e) const
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
        if (get<ClassTypeVar>(t))
            s += "class";
        else
            s += "table";

        s += " '" + toString(e.table) + "'.  " + candidatesSuggestion + "?";
        return s;
    }

    std::string operator()(const lluz::GenericError& e) const
    {
        return e.message;
    }

    std::string operator()(const lluz::InternalError& e) const
    {
        return e.message;
    }

    std::string operator()(const lluz::CannotCallNonFunction& e) const
    {
        return XorStr("Cannot call non-function ") + toString(e.ty);
    }
    std::string operator()(const lluz::ExtraInformation& e) const
    {
        return e.message;
    }

    std::string operator()(const lluz::DeprecatedApiUsed& e) const
    {
        return XorStr("The property .") + e.symbol + " is deprecated.  Use ." + e.useInstead + " instead.";
    }

    std::string operator()(const lluz::ModuleHasCyclicDependency& e) const
    {
        if (e.cycle.empty())
            return XorStr("Cyclic module dependency detected");

        std::string s = "Cyclic module dependency: ";

        bool first = true;
        for (const ModuleName& name : e.cycle)
        {
            if (first)
                first = false;
            else
                s += " -> ";

            s += name;
        }

        return s;
    }

    std::string operator()(const lluz::FunctionExitsWithoutReturning& e) const
    {
        return XorStr("Not all codepaths in this function return '") + toString(e.expectedReturnType) + "'.";
    }

    std::string operator()(const lluz::IllegalRequire& e) const
    {
        return XorStr("Cannot require module ") + e.moduleName + ": " + e.reason;
    }

    std::string operator()(const lluz::MissingProperties& e) const
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

    std::string operator()(const lluz::DuplicateGenericParameter& e) const
    {
        return XorStr("Duplicate type parameter '") + e.parameterName + "'";
    }

    std::string operator()(const lluz::CannotInferBinaryOperation& e) const
    {
        std::string ss = "Unknown type used in " + toString(e.op);

        switch (e.kind)
        {
        case lluz::CannotInferBinaryOperation::Comparison:
            ss += " comparison";
            break;
        case lluz::CannotInferBinaryOperation::Operation:
            ss += " operation";
        }

        if (e.suggestedToAnnotate)
            ss += "; consider adding a type annotation to '" + *e.suggestedToAnnotate + "'";

        return ss;
    }

    std::string operator()(const lluz::SwappedGenericTypeParameter& e) const
    {
        switch (e.kind)
        {
        case lluz::SwappedGenericTypeParameter::Type:
            return XorStr("Variadic type parameter '") + e.name + "...' is used as a regular generic type; consider changing '" + e.name + "...' to '" +
                   e.name + "' in the generic argument list";
        case lluz::SwappedGenericTypeParameter::Pack:
            return XorStr("Generic type '") + e.name + "' is used as a variadic type parameter; consider changing '" + e.name + "' to '" + e.name +
                   "...' in the generic argument list";
        default:
            lluz_ASSERT(!XorStr("Unknown kind"));
            return XorStr("");
        }
    }

    std::string operator()(const lluz::OptionalValueAccess& e) const
    {
        return XorStr("Value of type '") + toString(e.optional) + "' could be nil";
    }

    std::string operator()(const lluz::MissingUnionProperty& e) const
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
        return XorStr("Cannot cast '") + toString(e.left) + "' into '" + toString(e.right) + "' because the types are unrelated";
    }

    std::string operator()(const NormalizationTooComplex&) const
    {
        return XorStr("Code is too complex to typecheck! Consider simplifying the code around this area");
    }
};

struct InvalidNameChecker
{
    std::string invalidName = "%error-id%";

    bool operator()(const lluz::UnknownProperty& e) const
    {
        return e.key == invalidName;
    }
    bool operator()(const lluz::CannotExtendTable& e) const
    {
        return e.prop == invalidName;
    }
    bool operator()(const lluz::DuplicateTypeDefinition& e) const
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

TypeMismatch::TypeMismatch(TypeId wantedType, TypeId givenType, std::string reason, TypeError error)
    : wantedType(wantedType)
    , givenType(givenType)
    , reason(reason)
    , error(std::make_shared<TypeError>(std::move(error)))
{
}

bool TypeMismatch::operator==(const TypeMismatch& rhs) const
{
    if (!!error != !!rhs.error)
        return false;

    if (error && !(*error == *rhs.error))
        return false;

    return *wantedType == *rhs.wantedType && *givenType == *rhs.givenType && reason == rhs.reason;
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
    return expected == rhs.expected && actual == rhs.actual && context == rhs.context;
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
    return 1000 + int(data.index());
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

std::string toString(const TypeError& error)
{
    return toString(error, TypeErrorToStringOptions{});
}

std::string toString(const TypeError& error, TypeErrorToStringOptions options)
{
    ErrorConverter converter{options.fileResolver};
    return lluz::visit(converter, error.data);
}

bool containsParseErrorName(const TypeError& error)
{
    return lluz::visit(InvalidNameChecker{}, error.data);
}

template<typename T>
void copyError(T& e, TypeArena& destArena, CloneState cloneState)
{
    auto clone = [&](auto&& ty) {
        return ::lluz::clone(ty, destArena, cloneState);
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
    else
        static_assert(always_false_v<T>, XorStr("Non-exhaustive type switch"));
}

void copyErrors(ErrorVec& errors, TypeArena& destArena)
{
    CloneState cloneState;

    auto visitErrorData = [&](auto&& e) {
        copyError(e, destArena, cloneState);
    };

    lluz_ASSERT(!destArena.typeVars.isFrozen());
    lluz_ASSERT(!destArena.typePacks.isFrozen());

    for (TypeError& error : errors)
        visit(visitErrorData, error.data);
}

void InternalErrorReporter::ice(const std::string& message, const Location& location)
{
    if (FFlag::LluUseInternalCompilerErrorException)
    {
        InternalCompilerError error(message, moduleName, location);

        if (onInternalError)
            onInternalError(error.what());

        throw error;
    }
    else
    {
        std::runtime_error error("Internal error in " + moduleName + " at " + toString(location) + ": " + message);

        if (onInternalError)
            onInternalError(error.what());

        throw error;
    }
}

void InternalErrorReporter::ice(const std::string& message)
{
    if (FFlag::LluUseInternalCompilerErrorException)
    {
        InternalCompilerError error(message, moduleName);

        if (onInternalError)
            onInternalError(error.what());

        throw error;
    }
    else
    {
        std::runtime_error error("Internal error in " + moduleName + ": " + message);

        if (onInternalError)
            onInternalError(error.what());

        throw error;
    }
}

const char* InternalCompilerError::what() const throw()
{
    return this->message.data();
}

} // namespace lluz
