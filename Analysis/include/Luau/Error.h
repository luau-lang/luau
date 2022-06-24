// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/FileResolver.h"
#include "Luau/Location.h"
#include "Luau/TypeVar.h"
#include "Luau/Variant.h"
#include "Luau/TypeArena.h"

namespace Luau
{
struct TypeError;

struct TypeMismatch
{
    TypeMismatch() = default;
    TypeMismatch(TypeId wantedType, TypeId givenType);
    TypeMismatch(TypeId wantedType, TypeId givenType, std::string reason);
    TypeMismatch(TypeId wantedType, TypeId givenType, std::string reason, TypeError error);

    TypeId wantedType = nullptr;
    TypeId givenType = nullptr;

    std::string reason;
    std::shared_ptr<TypeError> error;

    bool operator==(const TypeMismatch& rhs) const;
};

struct UnknownSymbol
{
    enum Context
    {
        Binding,
        Type,
        Generic
    };
    Name name;
    Context context;

    bool operator==(const UnknownSymbol& rhs) const;
};

struct UnknownProperty
{
    TypeId table;
    Name key;

    bool operator==(const UnknownProperty& rhs) const;
};

struct NotATable
{
    TypeId ty;

    bool operator==(const NotATable& rhs) const;
};

struct CannotExtendTable
{
    enum Context
    {
        Property,
        Indexer,
        Metatable
    };
    TypeId tableType;
    Context context;
    Name prop;

    bool operator==(const CannotExtendTable& rhs) const;
};

struct OnlyTablesCanHaveMethods
{
    TypeId tableType;

    bool operator==(const OnlyTablesCanHaveMethods& rhs) const;
};

struct DuplicateTypeDefinition
{
    Name name;
    Location previousLocation;

    bool operator==(const DuplicateTypeDefinition& rhs) const;
};

struct CountMismatch
{
    enum Context
    {
        Arg,
        Result,
        Return,
    };
    size_t expected;
    size_t actual;
    Context context = Arg;
    bool isVariadic = false;

    bool operator==(const CountMismatch& rhs) const;
};

struct FunctionDoesNotTakeSelf
{
    bool operator==(const FunctionDoesNotTakeSelf& rhs) const;
};

struct FunctionRequiresSelf
{
    bool operator==(const FunctionRequiresSelf& rhs) const;
};

struct OccursCheckFailed
{
    bool operator==(const OccursCheckFailed& rhs) const;
};

struct UnknownRequire
{
    std::string modulePath;

    bool operator==(const UnknownRequire& rhs) const;
};

struct IncorrectGenericParameterCount
{
    Name name;
    TypeFun typeFun;
    size_t actualParameters;
    size_t actualPackParameters;

    bool operator==(const IncorrectGenericParameterCount& rhs) const;
};

struct SyntaxError
{
    std::string message;

    bool operator==(const SyntaxError& rhs) const;
};

struct CodeTooComplex
{
    bool operator==(const CodeTooComplex&) const;
};

struct UnificationTooComplex
{
    bool operator==(const UnificationTooComplex&) const;
};

// Could easily be folded into UnknownProperty with an extra field, std::set<Name> candidates.
// But for telemetry purposes, we want to have this be a distinct variant.
struct UnknownPropButFoundLikeProp
{
    TypeId table;
    Name key;
    std::set<Name> candidates;

    bool operator==(const UnknownPropButFoundLikeProp& rhs) const;
};

struct GenericError
{
    std::string message;

    bool operator==(const GenericError& rhs) const;
};

struct InternalError
{
    std::string message;

    bool operator==(const InternalError& rhs) const;
};

struct CannotCallNonFunction
{
    TypeId ty;

    bool operator==(const CannotCallNonFunction& rhs) const;
};

struct ExtraInformation
{
    std::string message;
    bool operator==(const ExtraInformation& rhs) const;
};

struct DeprecatedApiUsed
{
    std::string symbol;
    std::string useInstead;
    bool operator==(const DeprecatedApiUsed& rhs) const;
};

struct ModuleHasCyclicDependency
{
    std::vector<ModuleName> cycle;
    bool operator==(const ModuleHasCyclicDependency& rhs) const;
};

struct FunctionExitsWithoutReturning
{
    TypePackId expectedReturnType;
    bool operator==(const FunctionExitsWithoutReturning& rhs) const;
};

struct IllegalRequire
{
    std::string moduleName;
    std::string reason;

    bool operator==(const IllegalRequire& rhs) const;
};

struct MissingProperties
{
    enum Context
    {
        Missing,
        Extra
    };
    TypeId superType;
    TypeId subType;
    std::vector<Name> properties;
    Context context = Missing;

    bool operator==(const MissingProperties& rhs) const;
};

struct DuplicateGenericParameter
{
    std::string parameterName;

    bool operator==(const DuplicateGenericParameter& rhs) const;
};

struct CannotInferBinaryOperation
{
    enum OpKind
    {
        Operation,
        Comparison,
    };

    AstExprBinary::Op op;
    std::optional<std::string> suggestedToAnnotate;
    OpKind kind;

    bool operator==(const CannotInferBinaryOperation& rhs) const;
};

struct SwappedGenericTypeParameter
{
    enum Kind
    {
        Type,
        Pack,
    };

    std::string name;
    // What was `name` being used as?
    Kind kind;

    bool operator==(const SwappedGenericTypeParameter& rhs) const;
};

struct OptionalValueAccess
{
    TypeId optional;

    bool operator==(const OptionalValueAccess& rhs) const;
};

struct MissingUnionProperty
{
    TypeId type;
    std::vector<TypeId> missing;
    Name key;

    bool operator==(const MissingUnionProperty& rhs) const;
};

struct TypesAreUnrelated
{
    TypeId left;
    TypeId right;

    bool operator==(const TypesAreUnrelated& rhs) const;
};

struct NormalizationTooComplex
{
    bool operator==(const NormalizationTooComplex&) const
    {
        return true;
    }
};

using TypeErrorData = Variant<TypeMismatch, UnknownSymbol, UnknownProperty, NotATable, CannotExtendTable, OnlyTablesCanHaveMethods,
    DuplicateTypeDefinition, CountMismatch, FunctionDoesNotTakeSelf, FunctionRequiresSelf, OccursCheckFailed, UnknownRequire,
    IncorrectGenericParameterCount, SyntaxError, CodeTooComplex, UnificationTooComplex, UnknownPropButFoundLikeProp, GenericError, InternalError,
    CannotCallNonFunction, ExtraInformation, DeprecatedApiUsed, ModuleHasCyclicDependency, IllegalRequire, FunctionExitsWithoutReturning,
    DuplicateGenericParameter, CannotInferBinaryOperation, MissingProperties, SwappedGenericTypeParameter, OptionalValueAccess, MissingUnionProperty,
    TypesAreUnrelated, NormalizationTooComplex>;

struct TypeError
{
    Location location;
    ModuleName moduleName;
    TypeErrorData data;

    int code() const;

    TypeError() = default;

    TypeError(const Location& location, const ModuleName& moduleName, const TypeErrorData& data)
        : location(location)
        , moduleName(moduleName)
        , data(data)
    {
    }

    TypeError(const Location& location, const TypeErrorData& data)
        : TypeError(location, {}, data)
    {
    }

    bool operator==(const TypeError& rhs) const;
};

template<typename T>
const T* get(const TypeError& e)
{
    return get_if<T>(&e.data);
}

template<typename T>
T* get(TypeError& e)
{
    return get_if<T>(&e.data);
}

using ErrorVec = std::vector<TypeError>;

struct TypeErrorToStringOptions
{
    FileResolver* fileResolver = nullptr;
};

std::string toString(const TypeError& error);
std::string toString(const TypeError& error, TypeErrorToStringOptions options);

bool containsParseErrorName(const TypeError& error);

// Copy any types named in the error into destArena.
void copyErrors(ErrorVec& errors, struct TypeArena& destArena);

// Internal Compiler Error
struct InternalErrorReporter
{
    std::function<void(const char*)> onInternalError;
    std::string moduleName;

    [[noreturn]] void ice(const std::string& message, const Location& location);
    [[noreturn]] void ice(const std::string& message);
};

class InternalCompilerError : public std::exception {
public:
  explicit InternalCompilerError(const std::string& message, const std::string& moduleName)
  : message(message)
  , moduleName(moduleName)
  {
  }
  explicit InternalCompilerError(const std::string& message, const std::string& moduleName, const Location& location)
  : message(message)
  , moduleName(moduleName)
  , location(location)
  {
  }
  virtual const char* what() const throw();

  const std::string message;
  const std::string moduleName;
  const std::optional<Location> location;
};

} // namespace Luau
