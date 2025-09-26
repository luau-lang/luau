// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Error.h"

#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/FileResolver.h"
#include "Luau/NotNull.h"
#include "Luau/StringUtils.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeChecker2.h"
#include "Luau/TypeFunction.h"

#include <optional>
#include <string>
#include <type_traits>
#include <unordered_set>

LUAU_FASTINTVARIABLE(LuauIndentTypeMismatchMaxTypeLength, 10)

LUAU_FASTFLAGVARIABLE(LuauNewNonStrictReportsOneIndexedErrors)
LUAU_FASTFLAG(LuauSubtypingReportGenericBoundMismatches2)

static std::string wrongNumberOfArgsString(
    size_t expectedCount,
    std::optional<size_t> maximumCount,
    size_t actualCount,
    const char* argPrefix = nullptr,
    bool isVariadic = false
)
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

// this list of binary operator type functions is used for better stringification of type functions errors
static const std::unordered_map<std::string, const char*> kBinaryOps{
    {"add", "+"},
    {"sub", "-"},
    {"mul", "*"},
    {"div", "/"},
    {"idiv", "//"},
    {"pow", "^"},
    {"mod", "%"},
    {"concat", ".."},
    {"lt", "< or >="},
    {"le", "<= or >"},
    {"eq", "== or ~="}
};

// this list of unary operator type functions is used for better stringification of type functions errors
static const std::unordered_map<std::string, const char*> kUnaryOps{{"unm", "-"}, {"len", "#"}, {"not", "not"}};

// this list of type functions will receive a special error indicating that the user should file a bug on the GitHub repository
// putting a type function in this list indicates that it is expected to _always_ reduce
static const std::unordered_set<std::string> kUnreachableTypeFunctions{"refine", "singleton", "union", "intersect", "and", "or"};

struct ErrorConverter
{
    FileResolver* fileResolver = nullptr;

    std::string operator()(const Luau::TypeMismatch& tm) const
    {
        std::string givenTypeName = Luau::toString(tm.givenType);
        std::string wantedTypeName = Luau::toString(tm.wantedType);

        std::string result;

        auto quote = [&](std::string s)
        {
            return "'" + s + "'";
        };

        auto constructErrorMessage = [&](std::string givenType,
                                         std::string wantedType,
                                         std::optional<std::string> givenModule,
                                         std::optional<std::string> wantedModule) -> std::string
        {
            std::string given = givenModule ? quote(givenType) + " from " + quote(*givenModule) : quote(givenType);
            std::string wanted = wantedModule ? quote(wantedType) + " from " + quote(*wantedModule) : quote(wantedType);
            size_t luauIndentTypeMismatchMaxTypeLength = size_t(FInt::LuauIndentTypeMismatchMaxTypeLength);
            if (givenType.length() <= luauIndentTypeMismatchMaxTypeLength || wantedType.length() <= luauIndentTypeMismatchMaxTypeLength)
                return "Type " + given + " could not be converted into " + wanted;
            return "Type\n\t" + given + "\ncould not be converted into\n\t" + wanted;
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
            result = constructErrorMessage(std::move(givenTypeName), std::move(wantedTypeName), std::nullopt, std::nullopt);


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
        else if (get<ExternType>(t))
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

    std::string operator()(const Luau::CannotCompareUnrelatedTypes& e) const
    {
        return "Cannot compare unrelated types '" + toString(e.left) + "' and '" + toString(e.right) + "' with '" + toString(e.op) + "'";
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
        if (get<ExternType>(t))
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

    std::string operator()(const Luau::ConstraintSolvingIncompleteError& e) const
    {
        return "Type inference failed to complete, you may see some confusing types and type errors.";
    }

    std::optional<TypeId> findCallMetamethod(TypeId type) const
    {
        type = follow(type);

        std::optional<TypeId> metatable;
        if (const MetatableType* mtType = get<MetatableType>(type))
            metatable = mtType->metatable;
        else if (const ExternType* externType = get<ExternType>(type))
            metatable = externType->metatable;

        if (!metatable)
            return std::nullopt;

        TypeId unwrapped = follow(*metatable);

        if (get<AnyType>(unwrapped))
            return unwrapped;

        const TableType* mtt = getTableType(unwrapped);
        if (!mtt)
            return std::nullopt;

        auto it = mtt->props.find("__call");
        if (it != mtt->props.end())
        {
            return it->second.readTy;
        }
        else
            return std::nullopt;
    }

    std::string operator()(const Luau::CannotCallNonFunction& e) const
    {
        if (auto unionTy = get<UnionType>(follow(e.ty)))
        {
            std::string err = "Cannot call a value of the union type:";

            for (auto option : unionTy)
            {
                option = follow(option);

                if (get<FunctionType>(option) || findCallMetamethod(option))
                {
                    err += "\n  | " + toString(option);
                    continue;
                }

                // early-exit if we find something that isn't callable in the union.
                return "Cannot call a value of type " + toString(option) + " in union:\n  " + toString(e.ty);
            }

            err += "\nWe are unable to determine the appropriate result type for such a call.";

            return err;
        }

        if (auto primitiveTy = get<PrimitiveType>(follow(e.ty)); primitiveTy && primitiveTy->type == PrimitiveType::Function)
            return "The type " + toString(e.ty) + " is not precise enough for us to determine the appropriate result type of this call.";

        return "Cannot call a value of type " + toString(e.ty);
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

    std::string operator()(const DynamicPropertyLookupOnExternTypesUnsafe& e) const
    {
        return "Attempting a dynamic property access on type '" + Luau::toString(e.ty) + "' is unsafe and may cause exceptions at runtime";
    }

    std::string operator()(const UninhabitedTypeFunction& e) const
    {
        auto tfit = get<TypeFunctionInstanceType>(e.ty);
        LUAU_ASSERT(tfit); // Luau analysis has actually done something wrong if this type is not a type function.
        if (!tfit)
            return "Internal error: Unexpected type " + Luau::toString(e.ty) + " flagged as an uninhabited type function.";

        // unary operators
        if (auto unaryString = kUnaryOps.find(tfit->function->name); unaryString != kUnaryOps.end())
        {
            std::string result = "Operator '" + std::string(unaryString->second) + "' could not be applied to ";

            if (tfit->typeArguments.size() == 1 && tfit->packArguments.empty())
            {
                result += "operand of type " + Luau::toString(tfit->typeArguments[0]);

                if (tfit->function->name != "not")
                    result += "; there is no corresponding overload for __" + tfit->function->name;
            }
            else
            {
                // if it's not the expected case, we ought to add a specialization later, but this is a sane default.
                result += "operands of types ";

                bool isFirst = true;
                for (auto arg : tfit->typeArguments)
                {
                    if (!isFirst)
                        result += ", ";

                    result += Luau::toString(arg);
                    isFirst = false;
                }

                for (auto packArg : tfit->packArguments)
                    result += ", " + Luau::toString(packArg);
            }

            return result;
        }

        // binary operators
        if (auto binaryString = kBinaryOps.find(tfit->function->name); binaryString != kBinaryOps.end())
        {
            std::string result = "Operator '" + std::string(binaryString->second) + "' could not be applied to operands of types ";

            if (tfit->typeArguments.size() == 2 && tfit->packArguments.empty())
            {
                // this is the expected case.
                result += Luau::toString(tfit->typeArguments[0]) + " and " + Luau::toString(tfit->typeArguments[1]);
            }
            else
            {
                // if it's not the expected case, we ought to add a specialization later, but this is a sane default.

                bool isFirst = true;
                for (auto arg : tfit->typeArguments)
                {
                    if (!isFirst)
                        result += ", ";

                    result += Luau::toString(arg);
                    isFirst = false;
                }

                for (auto packArg : tfit->packArguments)
                    result += ", " + Luau::toString(packArg);
            }

            result += "; there is no corresponding overload for __" + tfit->function->name;

            return result;
        }

        // miscellaneous

        if ("keyof" == tfit->function->name || "rawkeyof" == tfit->function->name)
        {
            if (tfit->typeArguments.size() == 1 && tfit->packArguments.empty())
                return "Type '" + toString(tfit->typeArguments[0]) + "' does not have keys, so '" + Luau::toString(e.ty) + "' is invalid";
            else
                return "Type function instance " + Luau::toString(e.ty) + " is ill-formed, and thus invalid";
        }

        if ("index" == tfit->function->name || "rawget" == tfit->function->name)
        {
            if (tfit->typeArguments.size() != 2)
                return "Type function instance " + Luau::toString(e.ty) + " is ill-formed, and thus invalid";

            if (auto errType = get<ErrorType>(tfit->typeArguments[1])) // Second argument to (index | rawget)<_,_> is not a type
                return "Second argument to " + tfit->function->name + "<" + Luau::toString(tfit->typeArguments[0]) + ", _> is not a valid index type";
            else // Property `indexer` does not exist on type `indexee`
                return "Property '" + Luau::toString(tfit->typeArguments[1]) + "' does not exist on type '" + Luau::toString(tfit->typeArguments[0]) +
                       "'";
        }

        if (kUnreachableTypeFunctions.count(tfit->function->name))
        {
            return "Type function instance " + Luau::toString(e.ty) + " is uninhabited\n" +
                   "This is likely to be a bug, please report it at https://github.com/luau-lang/luau/issues";
        }

        // Everything should be specialized above to report a more descriptive error that hopefully does not mention "type functions" explicitly.
        // If we produce this message, it's an indication that we've missed a specialization and it should be fixed!
        return "Type function instance " + Luau::toString(e.ty) + " is uninhabited";
    }

    std::string operator()(const ExplicitFunctionAnnotationRecommended& r) const
    {
        std::string toReturn = toString(r.recommendedReturn);
        std::string argAnnotations;
        for (auto [arg, type] : r.recommendedArgs)
        {
            argAnnotations += arg + ": " + toString(type) + ", ";
        }
        if (argAnnotations.length() >= 2)
        {
            argAnnotations.pop_back();
            argAnnotations.pop_back();
        }

        if (argAnnotations.empty())
            return "Consider annotating the return with " + toReturn;

        return "Consider placing the following annotations on the arguments: " + argAnnotations + " or instead annotating the return as " + toReturn;
    }

    std::string operator()(const UninhabitedTypePackFunction& e) const
    {
        return "Type pack function instance " + Luau::toString(e.tp) + " is uninhabited";
    }

    std::string operator()(const WhereClauseNeeded& e) const
    {
        return "Type function instance " + Luau::toString(e.ty) +
               " depends on generic function parameters but does not appear in the function signature; this construct cannot be type-checked at this "
               "time";
    }

    std::string operator()(const PackWhereClauseNeeded& e) const
    {
        return "Type pack function instance " + Luau::toString(e.tp) +
               " depends on generic function parameters but does not appear in the function signature; this construct cannot be type-checked at this "
               "time";
    }

    std::string operator()(const CheckedFunctionCallError& e) const
    {
        // TODO: What happens if checkedFunctionName cannot be found??
        if (FFlag::LuauNewNonStrictReportsOneIndexedErrors)
            return "Function '" + e.checkedFunctionName + "' expects '" + toString(e.expected) + "' at argument #" +
                   std::to_string(e.argumentIndex + 1) + ", but got '" + Luau::toString(e.passed) + "'";
        else
            return "Function '" + e.checkedFunctionName + "' expects '" + toString(e.expected) + "' at argument #" + std::to_string(e.argumentIndex) +
                   ", but got '" + Luau::toString(e.passed) + "'";
    }

    std::string operator()(const NonStrictFunctionDefinitionError& e) const
    {
        if (e.functionName.empty())
        {
            return "Argument " + e.argument + " with type '" + toString(e.argumentType) + "' is used in a way that will run time error";
        }
        else
        {
            return "Argument " + e.argument + " with type '" + toString(e.argumentType) + "' in function '" + e.functionName +
                   "' is used in a way that will run time error";
        }
    }

    std::string operator()(const PropertyAccessViolation& e) const
    {
        const std::string stringKey = isIdentifier(e.key) ? e.key : "\"" + e.key + "\"";
        switch (e.context)
        {
        case PropertyAccessViolation::CannotRead:
            return "Property " + stringKey + " of table '" + toString(e.table) + "' is write-only";
        case PropertyAccessViolation::CannotWrite:
            return "Property " + stringKey + " of table '" + toString(e.table) + "' is read-only";
        }

        LUAU_UNREACHABLE();
        return "<Invalid PropertyAccessViolation>";
    }

    std::string operator()(const CheckedFunctionIncorrectArgs& e) const
    {
        return "Checked Function " + e.functionName + " expects " + std::to_string(e.expected) + " arguments, but received " +
               std::to_string(e.actual);
    }

    std::string operator()(const UnexpectedTypeInSubtyping& e) const
    {
        return "Encountered an unexpected type in subtyping: " + toString(e.ty);
    }

    std::string operator()(const UnexpectedTypePackInSubtyping& e) const
    {
        return "Encountered an unexpected type pack in subtyping: " + toString(e.tp);
    }

    std::string operator()(const UserDefinedTypeFunctionError& e) const
    {
        return e.message;
    }

    std::string operator()(const ReservedIdentifier& e) const
    {
        return e.name + " cannot be used as an identifier for a type function or alias";
    }

    std::string operator()(const CannotAssignToNever& e) const
    {
        std::string result = "Cannot assign a value of type " + toString(e.rhsType) + " to a field of type never";

        switch (e.reason)
        {
        case CannotAssignToNever::Reason::PropertyNarrowed:
            if (!e.cause.empty())
            {
                result += "\ncaused by the property being given the following incompatible types:\n";
                for (auto ty : e.cause)
                    result += "    " + toString(ty) + "\n";
                result += "There are no values that could safely satisfy all of these types at once.";
            }
        }

        return result;
    }

    std::string operator()(const UnexpectedArrayLikeTableItem&) const
    {
        return "Unexpected array-like table item: the indexer key type of this table is not `number`.";
    }

    std::string operator()(const CannotCheckDynamicStringFormatCalls& e) const
    {
        return "We cannot statically check the type of `string.format` when called with a format string that is not statically known.\n"
               "If you'd like to use an unchecked `string.format` call, you can cast the format string to `any` using `:: any`.";
    }


    std::string operator()(const GenericTypeCountMismatch& e) const
    {
        return "Different number of generic type parameters: subtype had " + std::to_string(e.subTyGenericCount) + ", supertype had " +
               std::to_string(e.superTyGenericCount) + ".";
    }

    std::string operator()(const GenericTypePackCountMismatch& e) const
    {
        return "Different number of generic type pack parameters: subtype had " + std::to_string(e.subTyGenericPackCount) + ", supertype had " +
               std::to_string(e.superTyGenericPackCount) + ".";
    }

    std::string operator()(const MultipleNonviableOverloads& e) const
    {
        return "None of the overloads for function that accept " + std::to_string(e.attemptedArgCount) + " arguments are compatible.";
    }

    std::string operator()(const RecursiveRestraintViolation& e) const
    {
        return "Recursive type being used with different parameters.";
    }

    std::string operator()(const GenericBoundsMismatch& e) const
    {
        LUAU_ASSERT(FFlag::LuauSubtypingReportGenericBoundMismatches2);
        std::string lowerBounds;
        for (size_t i = 0; i < e.lowerBounds.size(); ++i)
        {
            if (i > 0)
                lowerBounds += " | ";
            lowerBounds += Luau::toString(e.lowerBounds[i]);
        }
        std::string upperBounds;
        for (size_t i = 0; i < e.upperBounds.size(); ++i)
        {
            if (i > 0)
                upperBounds += " & ";
            upperBounds += Luau::toString(e.upperBounds[i]);
        }

        return "No valid instantiation could be inferred for generic type parameter " + std::string{e.genericName} +
               ". It was expected to be at least:\n\t" + lowerBounds + "\nand at most:\n\t" + upperBounds +
               "\nbut these types are not compatible with one another.";
    }

    std::string operator()(const UnappliedTypeFunction&) const
    {
        return "Type functions always require `<>` when referenced.";
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
    , reason(std::move(reason))
{
}

TypeMismatch::TypeMismatch(TypeId wantedType, TypeId givenType, std::string reason, std::optional<TypeError> error)
    : wantedType(wantedType)
    , givenType(givenType)
    , reason(std::move(reason))
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
    , reason(std::move(reason))
{
}

TypeMismatch::TypeMismatch(TypeId wantedType, TypeId givenType, std::string reason, std::optional<TypeError> error, TypeMismatch::Context context)
    : wantedType(wantedType)
    , givenType(givenType)
    , context(context)
    , reason(std::move(reason))
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

bool PropertyAccessViolation::operator==(const PropertyAccessViolation& rhs) const
{
    return *table == *rhs.table && key == rhs.key && context == rhs.context;
}

bool NotATable::operator==(const NotATable& rhs) const
{
    return ty == rhs.ty;
}

bool CannotExtendTable::operator==(const CannotExtendTable& rhs) const
{
    return *tableType == *rhs.tableType && prop == rhs.prop && context == rhs.context;
}

bool CannotCompareUnrelatedTypes::operator==(const CannotCompareUnrelatedTypes& rhs) const
{
    return *left == *rhs.left && right == rhs.right && op == rhs.op;
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

bool ConstraintSolvingIncompleteError::operator==(const ConstraintSolvingIncompleteError& rhs) const
{
    return true;
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

bool DynamicPropertyLookupOnExternTypesUnsafe::operator==(const DynamicPropertyLookupOnExternTypesUnsafe& rhs) const
{
    return ty == rhs.ty;
}

bool UninhabitedTypeFunction::operator==(const UninhabitedTypeFunction& rhs) const
{
    return ty == rhs.ty;
}


bool ExplicitFunctionAnnotationRecommended::operator==(const ExplicitFunctionAnnotationRecommended& rhs) const
{
    return recommendedReturn == rhs.recommendedReturn && recommendedArgs == rhs.recommendedArgs;
}

bool UninhabitedTypePackFunction::operator==(const UninhabitedTypePackFunction& rhs) const
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

bool CheckedFunctionIncorrectArgs::operator==(const CheckedFunctionIncorrectArgs& rhs) const
{
    return functionName == rhs.functionName && expected == rhs.expected && actual == rhs.actual;
}

bool UnexpectedTypeInSubtyping::operator==(const UnexpectedTypeInSubtyping& rhs) const
{
    return ty == rhs.ty;
}

bool UnexpectedTypePackInSubtyping::operator==(const UnexpectedTypePackInSubtyping& rhs) const
{
    return tp == rhs.tp;
}

bool UserDefinedTypeFunctionError::operator==(const UserDefinedTypeFunctionError& rhs) const
{
    return message == rhs.message;
}

bool ReservedIdentifier::operator==(const ReservedIdentifier& rhs) const
{
    return name == rhs.name;
}

bool CannotAssignToNever::operator==(const CannotAssignToNever& rhs) const
{
    if (cause.size() != rhs.cause.size())
        return false;

    for (size_t i = 0; i < cause.size(); ++i)
    {
        if (*cause[i] != *rhs.cause[i])
            return false;
    }

    return *rhsType == *rhs.rhsType && reason == rhs.reason;
}

bool GenericTypeCountMismatch::operator==(const GenericTypeCountMismatch& rhs) const
{
    return subTyGenericCount == rhs.subTyGenericCount && superTyGenericCount == rhs.superTyGenericCount;
}

bool GenericTypePackCountMismatch::operator==(const GenericTypePackCountMismatch& rhs) const
{
    return subTyGenericPackCount == rhs.subTyGenericPackCount && superTyGenericPackCount == rhs.superTyGenericPackCount;
}

bool MultipleNonviableOverloads::operator==(const MultipleNonviableOverloads& rhs) const
{
    return attemptedArgCount == rhs.attemptedArgCount;
}

GenericBoundsMismatch::GenericBoundsMismatch(const std::string_view genericName, TypeIds lowerBoundSet, TypeIds upperBoundSet)
    : genericName(genericName)
    , lowerBounds(lowerBoundSet.take())
    , upperBounds(upperBoundSet.take())
{
    LUAU_ASSERT(FFlag::LuauSubtypingReportGenericBoundMismatches2);
}

bool GenericBoundsMismatch::operator==(const GenericBoundsMismatch& rhs) const
{
    LUAU_ASSERT(FFlag::LuauSubtypingReportGenericBoundMismatches2);
    return genericName == rhs.genericName && lowerBounds == rhs.lowerBounds && upperBounds == rhs.upperBounds;
}

bool UnappliedTypeFunction::operator==(const UnappliedTypeFunction& rhs) const
{
    return true;
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
    auto clone = [&](auto&& ty)
    {
        return ::Luau::clone(ty, destArena, cloneState);
    };

    auto visitErrorData = [&](auto&& e)
    {
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
    else if constexpr (std::is_same_v<T, CannotCompareUnrelatedTypes>)
    {
        e.left = clone(e.left);
        e.right = clone(e.right);
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
    else if constexpr (std::is_same_v<T, ConstraintSolvingIncompleteError>)
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
    else if constexpr (std::is_same_v<T, DynamicPropertyLookupOnExternTypesUnsafe>)
        e.ty = clone(e.ty);
    else if constexpr (std::is_same_v<T, UninhabitedTypeFunction>)
        e.ty = clone(e.ty);
    else if constexpr (std::is_same_v<T, ExplicitFunctionAnnotationRecommended>)
    {
        e.recommendedReturn = clone(e.recommendedReturn);
        for (auto& [_, t] : e.recommendedArgs)
            t = clone(t);
    }
    else if constexpr (std::is_same_v<T, UninhabitedTypePackFunction>)
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
    else if constexpr (std::is_same_v<T, PropertyAccessViolation>)
        e.table = clone(e.table);
    else if constexpr (std::is_same_v<T, CheckedFunctionIncorrectArgs>)
    {
    }
    else if constexpr (std::is_same_v<T, UnexpectedTypeInSubtyping>)
        e.ty = clone(e.ty);
    else if constexpr (std::is_same_v<T, UnexpectedTypePackInSubtyping>)
        e.tp = clone(e.tp);
    else if constexpr (std::is_same_v<T, UserDefinedTypeFunctionError>)
    {
    }
    else if constexpr (std::is_same_v<T, CannotAssignToNever>)
    {
        e.rhsType = clone(e.rhsType);

        for (auto& ty : e.cause)
            ty = clone(ty);
    }
    else if constexpr (std::is_same_v<T, UnexpectedArrayLikeTableItem>)
    {
    }
    else if constexpr (std::is_same_v<T, ReservedIdentifier>)
    {
    }
    else if constexpr (std::is_same_v<T, CannotCheckDynamicStringFormatCalls>)
    {
    }
    else if constexpr (std::is_same_v<T, GenericTypeCountMismatch>)
    {
    }
    else if constexpr (std::is_same_v<T, GenericTypePackCountMismatch>)
    {
    }
    else if constexpr (std::is_same_v<T, MultipleNonviableOverloads>)
    {
    }
    else if constexpr (std::is_same_v<T, RecursiveRestraintViolation>)
    {
    }
    else if constexpr (std::is_same_v<T, GenericBoundsMismatch>)
    {
        LUAU_ASSERT(FFlag::LuauSubtypingReportGenericBoundMismatches2);
        for (auto& lowerBound : e.lowerBounds)
            lowerBound = clone(lowerBound);
        for (auto& upperBound : e.upperBounds)
            upperBound = clone(upperBound);
    }
    else if constexpr (std::is_same_v<T, UnappliedTypeFunction>)
    {
    }
    else
        static_assert(always_false_v<T>, "Non-exhaustive type switch");
}

void copyErrors(ErrorVec& errors, TypeArena& destArena, NotNull<BuiltinTypes> builtinTypes)
{
    CloneState cloneState{builtinTypes};

    auto visitErrorData = [&](auto&& e)
    {
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
