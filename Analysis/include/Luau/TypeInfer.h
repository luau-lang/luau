// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Anyification.h"
#include "Luau/ControlFlow.h"
#include "Luau/Error.h"
#include "Luau/Instantiation.h"
#include "Luau/Module.h"
#include "Luau/Predicate.h"
#include "Luau/Substitution.h"
#include "Luau/Symbol.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier.h"
#include "Luau/UnifierSharedState.h"

#include <memory>
#include <unordered_map>
#include <unordered_set>

namespace Luau
{

struct Scope;
struct TypeChecker;
struct ModuleResolver;
struct FrontendCancellationToken;

using Name = std::string;
using ScopePtr = std::shared_ptr<Scope>;

struct OverloadErrorEntry
{
    TxnLog log;
    ErrorVec errors;
    std::vector<TypeId> arguments;
    const FunctionType* fnTy;
};

bool doesCallError(const AstExprCall* call);
bool hasBreak(AstStat* node);
const AstStat* getFallthrough(const AstStat* node);

struct UnifierOptions;
struct Unifier;

struct GenericTypeDefinitions
{
    std::vector<GenericTypeDefinition> genericTypes;
    std::vector<GenericTypePackDefinition> genericPacks;
};

struct HashBoolNamePair
{
    size_t operator()(const std::pair<bool, Name>& pair) const;
};

// All Types are retained via Environment::types.  All TypeIds
// within a program are borrowed pointers into this set.
struct TypeChecker
{
    explicit TypeChecker(
        const ScopePtr& globalScope,
        ModuleResolver* resolver,
        NotNull<BuiltinTypes> builtinTypes,
        InternalErrorReporter* iceHandler
    );
    TypeChecker(const TypeChecker&) = delete;
    TypeChecker& operator=(const TypeChecker&) = delete;

    ModulePtr check(const SourceModule& module, Mode mode, std::optional<ScopePtr> environmentScope = std::nullopt);
    ModulePtr checkWithoutRecursionCheck(const SourceModule& module, Mode mode, std::optional<ScopePtr> environmentScope = std::nullopt);

    std::vector<std::pair<Location, ScopePtr>> getScopes() const;

    ControlFlow check(const ScopePtr& scope, const AstStat& statement);
    ControlFlow check(const ScopePtr& scope, const AstStatBlock& statement);
    ControlFlow check(const ScopePtr& scope, const AstStatIf& statement);
    ControlFlow check(const ScopePtr& scope, const AstStatWhile& statement);
    ControlFlow check(const ScopePtr& scope, const AstStatRepeat& statement);
    ControlFlow check(const ScopePtr& scope, const AstStatReturn& return_);
    ControlFlow check(const ScopePtr& scope, const AstStatAssign& assign);
    ControlFlow check(const ScopePtr& scope, const AstStatCompoundAssign& assign);
    ControlFlow check(const ScopePtr& scope, const AstStatLocal& local);
    ControlFlow check(const ScopePtr& scope, const AstStatFor& local);
    ControlFlow check(const ScopePtr& scope, const AstStatForIn& forin);
    ControlFlow check(const ScopePtr& scope, TypeId ty, const ScopePtr& funScope, const AstStatFunction& function);
    ControlFlow check(const ScopePtr& scope, TypeId ty, const ScopePtr& funScope, const AstStatLocalFunction& function);
    ControlFlow check(const ScopePtr& scope, const AstStatTypeAlias& typealias);
    ControlFlow check(const ScopePtr& scope, const AstStatTypeFunction& typefunction);
    ControlFlow check(const ScopePtr& scope, const AstStatDeclareExternType& declaredExternType);
    ControlFlow check(const ScopePtr& scope, const AstStatDeclareFunction& declaredFunction);

    void prototype(const ScopePtr& scope, const AstStatTypeAlias& typealias, int subLevel = 0);
    void prototype(const ScopePtr& scope, const AstStatDeclareExternType& declaredExternType);

    ControlFlow checkBlock(const ScopePtr& scope, const AstStatBlock& statement);
    ControlFlow checkBlockWithoutRecursionCheck(const ScopePtr& scope, const AstStatBlock& statement);
    void checkBlockTypeAliases(const ScopePtr& scope, std::vector<AstStat*>& sorted);

    WithPredicate<TypeId> checkExpr(
        const ScopePtr& scope,
        const AstExpr& expr,
        std::optional<TypeId> expectedType = std::nullopt,
        bool forceSingleton = false
    );
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprLocal& expr);
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprGlobal& expr);
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprVarargs& expr);
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprCall& expr);
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprIndexName& expr);
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprIndexExpr& expr);
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprFunction& expr, std::optional<TypeId> expectedType = std::nullopt);
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprTable& expr, std::optional<TypeId> expectedType = std::nullopt);
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprUnary& expr);
    TypeId checkRelationalOperation(
        const ScopePtr& scope,
        const AstExprBinary& expr,
        TypeId lhsType,
        TypeId rhsType,
        const PredicateVec& predicates = {}
    );
    TypeId checkBinaryOperation(
        const ScopePtr& scope,
        const AstExprBinary& expr,
        TypeId lhsType,
        TypeId rhsType,
        const PredicateVec& predicates = {}
    );
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprBinary& expr, std::optional<TypeId> expectedType = std::nullopt);
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprTypeAssertion& expr);
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprError& expr);
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprIfElse& expr, std::optional<TypeId> expectedType = std::nullopt);
    WithPredicate<TypeId> checkExpr(const ScopePtr& scope, const AstExprInterpString& expr);

    TypeId checkExprTable(
        const ScopePtr& scope,
        const AstExprTable& expr,
        const std::vector<std::pair<TypeId, TypeId>>& fieldTypes,
        std::optional<TypeId> expectedType
    );

    // Returns the type of the lvalue.
    TypeId checkLValue(const ScopePtr& scope, const AstExpr& expr, ValueContext ctx);

    // Returns the type of the lvalue.
    TypeId checkLValueBinding(const ScopePtr& scope, const AstExpr& expr, ValueContext ctx);
    TypeId checkLValueBinding(const ScopePtr& scope, const AstExprLocal& expr);
    TypeId checkLValueBinding(const ScopePtr& scope, const AstExprGlobal& expr);
    TypeId checkLValueBinding(const ScopePtr& scope, const AstExprIndexName& expr, ValueContext ctx);
    TypeId checkLValueBinding(const ScopePtr& scope, const AstExprIndexExpr& expr, ValueContext ctx);

    TypeId checkFunctionName(const ScopePtr& scope, AstExpr& funName, TypeLevel level);
    std::pair<TypeId, ScopePtr> checkFunctionSignature(
        const ScopePtr& scope,
        int subLevel,
        const AstExprFunction& expr,
        std::optional<Location> originalNameLoc,
        std::optional<TypeId> selfType,
        std::optional<TypeId> expectedType
    );
    void checkFunctionBody(const ScopePtr& scope, TypeId type, const AstExprFunction& function);

    void checkArgumentList(
        const ScopePtr& scope,
        const AstExpr& funName,
        Unifier& state,
        TypePackId paramPack,
        TypePackId argPack,
        const std::vector<Location>& argLocations
    );

    WithPredicate<TypePackId> checkExprPack(const ScopePtr& scope, const AstExpr& expr);

    WithPredicate<TypePackId> checkExprPackHelper(const ScopePtr& scope, const AstExpr& expr);
    WithPredicate<TypePackId> checkExprPackHelper(const ScopePtr& scope, const AstExprCall& expr);
    WithPredicate<TypePackId> checkExprPackHelper2(
        const ScopePtr& scope,
        const AstExprCall& expr,
        TypeId selfType,
        TypeId actualFunctionType,
        TypeId functionType,
        TypePackId retPack
    );

    std::vector<std::optional<TypeId>> getExpectedTypesForCall(const std::vector<TypeId>& overloads, size_t argumentCount, bool selfCall);

    std::unique_ptr<WithPredicate<TypePackId>> checkCallOverload(
        const ScopePtr& scope,
        const AstExprCall& expr,
        TypeId fn,
        TypePackId retPack,
        TypePackId argPack,
        TypePack* args,
        const std::vector<Location>* argLocations,
        const WithPredicate<TypePackId>& argListResult,
        std::vector<TypeId>& overloadsThatMatchArgCount,
        std::vector<TypeId>& overloadsThatDont,
        std::vector<OverloadErrorEntry>& errors
    );
    bool handleSelfCallMismatch(
        const ScopePtr& scope,
        const AstExprCall& expr,
        TypePack* args,
        const std::vector<Location>& argLocations,
        const std::vector<OverloadErrorEntry>& errors
    );
    void reportOverloadResolutionError(
        const ScopePtr& scope,
        const AstExprCall& expr,
        TypePackId retPack,
        TypePackId argPack,
        const std::vector<Location>& argLocations,
        const std::vector<TypeId>& overloads,
        const std::vector<TypeId>& overloadsThatMatchArgCount,
        std::vector<OverloadErrorEntry>& errors
    );

    WithPredicate<TypePackId> checkExprList(
        const ScopePtr& scope,
        const Location& location,
        const AstArray<AstExpr*>& exprs,
        bool substituteFreeForNil = false,
        const std::vector<bool>& lhsAnnotations = {},
        const std::vector<std::optional<TypeId>>& expectedTypes = {}
    );

    static std::optional<AstExpr*> matchRequire(const AstExprCall& call);
    TypeId checkRequire(const ScopePtr& scope, const ModuleInfo& moduleInfo, const Location& location);

    // Try to infer that the provided type is a table of some sort.
    // Reports an error if the type is already some kind of non-table.
    void tablify(TypeId type);

    /** In nonstrict mode, many types need to be replaced by any.
     */
    TypeId anyIfNonstrict(TypeId ty) const;

    /** Attempt to unify the types.
     * Treat any failures as type errors in the final typecheck report.
     */
    bool unify(TypeId subTy, TypeId superTy, const ScopePtr& scope, const Location& location);
    bool unify(TypeId subTy, TypeId superTy, const ScopePtr& scope, const Location& location, const UnifierOptions& options);
    bool unify(
        TypePackId subTy,
        TypePackId superTy,
        const ScopePtr& scope,
        const Location& location,
        CountMismatch::Context ctx = CountMismatch::Context::Arg
    );

    /** Attempt to unify the types.
     * If this fails, and the subTy type can be instantiated, do so and try unification again.
     */
    bool unifyWithInstantiationIfNeeded(TypeId subTy, TypeId superTy, const ScopePtr& scope, const Location& location);
    void unifyWithInstantiationIfNeeded(TypeId subTy, TypeId superTy, const ScopePtr& scope, Unifier& state);

    /** Attempt to unify.
     * If there are errors, undo everything and return the errors.
     * If there are no errors, commit and return an empty error vector.
     */
    template<typename Id>
    ErrorVec tryUnify_(Id subTy, Id superTy, const ScopePtr& scope, const Location& location);
    ErrorVec tryUnify(TypeId subTy, TypeId superTy, const ScopePtr& scope, const Location& location);
    ErrorVec tryUnify(TypePackId subTy, TypePackId superTy, const ScopePtr& scope, const Location& location);

    // Test whether the two type vars unify.  Never commits the result.
    template<typename Id>
    ErrorVec canUnify_(Id subTy, Id superTy, const ScopePtr& scope, const Location& location);
    ErrorVec canUnify(TypeId subTy, TypeId superTy, const ScopePtr& scope, const Location& location);
    ErrorVec canUnify(TypePackId subTy, TypePackId superTy, const ScopePtr& scope, const Location& location);

    std::optional<TypeId> findMetatableEntry(TypeId type, std::string entry, const Location& location, bool addErrors);
    std::optional<TypeId> findTablePropertyRespectingMeta(TypeId lhsType, Name name, const Location& location, bool addErrors);

    std::optional<TypeId> getIndexTypeFromType(const ScopePtr& scope, TypeId type, const Name& name, const Location& location, bool addErrors);
    std::optional<TypeId> getIndexTypeFromTypeImpl(const ScopePtr& scope, TypeId type, const Name& name, const Location& location, bool addErrors);

    std::optional<TypeId> tryStripUnionFromNil(TypeId ty);
    TypeId stripFromNilAndReport(TypeId ty, const Location& location);

public:
    /*
     * Convert monotype into a a polytype, by replacing any metavariables in descendant scopes
     * by bound generic type variables. This is used to infer that a function is generic.
     */
    TypeId quantify(const ScopePtr& scope, TypeId ty, Location location);

    /*
     * Convert a polytype into a monotype, by replacing any bound generic types by type metavariables.
     * This is used to typecheck particular calls to generic functions, and when generic functions
     * are passed as arguments.
     *
     * The "changed" boolean is used to permit us to return the same TypeId in the case that the instantiated type is unchanged.
     * This is important in certain cases, such as methods on objects, where a table contains a function whose first argument is the table.
     * Without this property, we can wind up in a situation where a new TypeId is allocated for the outer table.  This can cause us to produce
     * unfortunate types like
     *
     *     {method: ({method: (<CYCLE>) -> a}) -> a}
     *
     */
    TypeId instantiate(const ScopePtr& scope, TypeId ty, Location location, const TxnLog* log = TxnLog::empty());

    // Replace any free types or type packs by `any`.
    // This is used when exporting types from modules, to make sure free types don't leak.
    TypeId anyify(const ScopePtr& scope, TypeId ty, Location location);
    TypePackId anyify(const ScopePtr& scope, TypePackId ty, Location location);

    TypePackId anyifyModuleReturnTypePackGenerics(TypePackId ty);

    void reportError(const TypeError& error);
    void reportError(const Location& location, TypeErrorData error);
    void reportErrors(const ErrorVec& errors);

    [[noreturn]] void ice(const std::string& message, const Location& location);
    [[noreturn]] void ice(const std::string& message);
    [[noreturn]] void throwTimeLimitError();
    [[noreturn]] void throwUserCancelError();

    ScopePtr childFunctionScope(const ScopePtr& parent, const Location& location, int subLevel = 0);
    ScopePtr childScope(const ScopePtr& parent, const Location& location);

    // Wrapper for merge(l, r, toUnion) but without the lambda junk.
    void merge(RefinementMap& l, const RefinementMap& r);

    // Produce an "emergency backup type" for recovery from type errors.
    // This comes in two flavours, depening on whether or not we can make a good guess
    // for an error recovery type.
    TypeId errorRecoveryType(TypeId guess);
    TypePackId errorRecoveryTypePack(TypePackId guess);
    TypeId errorRecoveryType(const ScopePtr& scope);
    TypePackId errorRecoveryTypePack(const ScopePtr& scope);

private:
    void prepareErrorsForDisplay(ErrorVec& errVec);
    void diagnoseMissingTableKey(UnknownProperty* utk, TypeErrorData& data);
    void reportErrorCodeTooComplex(const Location& location);

private:
    Unifier mkUnifier(const ScopePtr& scope, const Location& location);

    // These functions are only safe to call when we are in the process of typechecking a module.

    // Produce a new free type var.
    TypeId freshType(const ScopePtr& scope);
    TypeId freshType(TypeLevel level);

    // Produce a new singleton type var.
    TypeId singletonType(bool value);
    TypeId singletonType(std::string value);

    TypeIdPredicate mkTruthyPredicate(bool sense, TypeId emptySetTy);

    // TODO: Return TypeId only.
    std::optional<TypeId> filterMapImpl(TypeId type, TypeIdPredicate predicate);
    std::pair<std::optional<TypeId>, bool> filterMap(TypeId type, TypeIdPredicate predicate);

public:
    std::pair<std::optional<TypeId>, bool> pickTypesFromSense(TypeId type, bool sense, TypeId emptySetTy);

private:
    TypeId unionOfTypes(TypeId a, TypeId b, const ScopePtr& scope, const Location& location, bool unifyFreeTypes = true);

    // ex
    //      TypeId id = addType(FreeType());
    template<typename T>
    TypeId addType(const T& tv)
    {
        return addTV(Type(tv));
    }

    TypeId addTV(Type&& tv);

    TypePackId addTypePack(TypePackVar&& tp);
    TypePackId addTypePack(TypePack&& tp);

    TypePackId addTypePack(const std::vector<TypeId>& ty);
    TypePackId addTypePack(const std::vector<TypeId>& ty, std::optional<TypePackId> tail);
    TypePackId addTypePack(std::initializer_list<TypeId>&& ty);
    TypePackId freshTypePack(const ScopePtr& scope);
    TypePackId freshTypePack(TypeLevel level);

    TypeId resolveType(const ScopePtr& scope, const AstType& annotation);
    TypeId resolveTypeWorker(const ScopePtr& scope, const AstType& annotation);
    TypePackId resolveTypePack(const ScopePtr& scope, const AstTypeList& types);
    TypePackId resolveTypePack(const ScopePtr& scope, const AstTypePack& annotation);
    TypeId instantiateTypeFun(
        const ScopePtr& scope,
        const TypeFun& tf,
        const std::vector<TypeId>& typeParams,
        const std::vector<TypePackId>& typePackParams,
        const Location& location
    );

    // Note: `scope` must be a fresh scope.
    GenericTypeDefinitions createGenericTypes(
        const ScopePtr& scope,
        std::optional<TypeLevel> levelOpt,
        const AstNode& node,
        const AstArray<AstGenericType*>& genericNames,
        const AstArray<AstGenericTypePack*>& genericPackNames,
        bool useCache = false
    );

public:
    void resolve(const PredicateVec& predicates, const ScopePtr& scope, bool sense);

private:
    void refineLValue(const LValue& lvalue, RefinementMap& refis, const ScopePtr& scope, TypeIdPredicate predicate);

    std::optional<TypeId> resolveLValue(const ScopePtr& scope, const LValue& lvalue);
    std::optional<TypeId> resolveLValue(const RefinementMap& refis, const ScopePtr& scope, const LValue& lvalue);

    void resolve(const PredicateVec& predicates, RefinementMap& refis, const ScopePtr& scope, bool sense, bool fromOr = false);
    void resolve(const Predicate& predicate, RefinementMap& refis, const ScopePtr& scope, bool sense, bool fromOr);
    void resolve(const TruthyPredicate& truthyP, RefinementMap& refis, const ScopePtr& scope, bool sense, bool fromOr);
    void resolve(const AndPredicate& andP, RefinementMap& refis, const ScopePtr& scope, bool sense);
    void resolve(const OrPredicate& orP, RefinementMap& refis, const ScopePtr& scope, bool sense);
    void resolve(const IsAPredicate& isaP, RefinementMap& refis, const ScopePtr& scope, bool sense);
    void resolve(const TypeGuardPredicate& typeguardP, RefinementMap& refis, const ScopePtr& scope, bool sense);
    void resolve(const EqPredicate& eqP, RefinementMap& refis, const ScopePtr& scope, bool sense);

    bool isNonstrictMode() const;
    bool useConstrainedIntersections() const;

public:
    /** Extract the types in a type pack, given the assumption that the pack must have some exact length.
     * TypePacks can have free tails, which means that inference has not yet determined the length of the pack.
     * Calling this function means submitting evidence that the pack must have the length provided.
     * If the pack is known not to have the correct length, an error will be reported.
     * The return vector is always of the exact requested length.  In the event that the pack's length does
     * not match up, excess TypeIds will be ErrorTypes.
     */
    std::vector<TypeId> unTypePack(const ScopePtr& scope, TypePackId pack, size_t expectedLength, const Location& location);

    const ScopePtr& globalScope;

    ModuleResolver* resolver;
    ModulePtr currentModule;

    std::function<void(const ModuleName&, const ScopePtr&)> prepareModuleScope;
    NotNull<BuiltinTypes> builtinTypes;
    InternalErrorReporter* iceHandler;

    UnifierSharedState unifierState;
    Normalizer normalizer;

    Instantiation reusableInstantiation;

    std::vector<RequireCycle> requireCycles;

    // Type inference limits
    std::optional<double> finishTime;
    std::optional<int> instantiationChildLimit;
    std::optional<int> unifierIterationLimit;

    std::shared_ptr<FrontendCancellationToken> cancellationToken;

public:
    const TypeId nilType;
    const TypeId numberType;
    const TypeId stringType;
    const TypeId booleanType;
    const TypeId threadType;
    const TypeId bufferType;
    const TypeId anyType;
    const TypeId unknownType;
    const TypeId neverType;

    const TypePackId anyTypePack;
    const TypePackId neverTypePack;
    const TypePackId uninhabitableTypePack;

private:
    int checkRecursionCount = 0;
    int recursionCount = 0;

    /**
     * We use this to avoid doing second-pass analysis of type aliases that are duplicates. We record a pair
     * (exported, name) to properly deal with the case where the two duplicates do not have the same export status.
     */
    DenseHashSet<std::pair<bool, Name>, HashBoolNamePair> duplicateTypeAliases;

    /**
     * A set of incorrect class definitions which is used to avoid a second-pass analysis.
     */
    DenseHashSet<const AstStatDeclareExternType*> incorrectExternTypeDefinitions{nullptr};

    std::vector<std::pair<TypeId, ScopePtr>> deferredQuantification;
};

using PrintLineProc = void (*)(const std::string&);

extern PrintLineProc luauPrintLine;

// Unit test hook
void setPrintLine(PrintLineProc pl);
void resetPrintLine();

} // namespace Luau
