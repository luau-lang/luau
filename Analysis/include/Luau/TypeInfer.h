// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Predicate.h"
#include "Luau/Error.h"
#include "Luau/Module.h"
#include "Luau/Symbol.h"
#include "Luau/Parser.h"
#include "Luau/Substitution.h"
#include "Luau/TxnLog.h"
#include "Luau/TypePack.h"
#include "Luau/TypeVar.h"
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

using Name = std::string;
using ScopePtr = std::shared_ptr<Scope>;
using OverloadErrorEntry = std::tuple<std::vector<TypeError>, std::vector<TypeId>, const FunctionTypeVar*>;

bool doesCallError(const AstExprCall* call);
bool hasBreak(AstStat* node);
const AstStat* getFallthrough(const AstStat* node);

struct Unifier;

// A substitution which replaces generic types in a given set by free types.
struct ReplaceGenerics : Substitution
{
    TypeLevel level;
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;
    bool ignoreChildren(TypeId ty) override;
    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId tp) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId tp) override;
};

// A substitution which replaces generic functions by monomorphic functions
struct Instantiation : Substitution
{
    TypeLevel level;
    ReplaceGenerics replaceGenerics;
    bool ignoreChildren(TypeId ty) override;
    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId tp) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId tp) override;
};

// A substitution which replaces free types by generic types.
struct Quantification : Substitution
{
    TypeLevel level;
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;
    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId tp) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId tp) override;
};

// A substitution which replaces free types by any
struct Anyification : Substitution
{
    TypeId anyType;
    TypePackId anyTypePack;
    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId tp) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId tp) override;
};

// A substitution which replaces the type parameters of a type function by arguments
struct ApplyTypeFunction : Substitution
{
    TypeLevel level;
    bool encounteredForwardedType;
    std::unordered_map<TypeId, TypeId> typeArguments;
    std::unordered_map<TypePackId, TypePackId> typePackArguments;
    bool ignoreChildren(TypeId ty) override;
    bool ignoreChildren(TypePackId tp) override;
    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId tp) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId tp) override;
};

// All TypeVars are retained via Environment::typeVars.  All TypeIds
// within a program are borrowed pointers into this set.
struct TypeChecker
{
    explicit TypeChecker(ModuleResolver* resolver, InternalErrorReporter* iceHandler);
    TypeChecker(const TypeChecker&) = delete;
    TypeChecker& operator=(const TypeChecker&) = delete;

    ModulePtr check(const SourceModule& module, Mode mode, std::optional<ScopePtr> environmentScope = std::nullopt);

    std::vector<std::pair<Location, ScopePtr>> getScopes() const;

    void check(const ScopePtr& scope, const AstStat& statement);
    void check(const ScopePtr& scope, const AstStatBlock& statement);
    void check(const ScopePtr& scope, const AstStatIf& statement);
    void check(const ScopePtr& scope, const AstStatWhile& statement);
    void check(const ScopePtr& scope, const AstStatRepeat& statement);
    void check(const ScopePtr& scope, const AstStatReturn& return_);
    void check(const ScopePtr& scope, const AstStatAssign& assign);
    void check(const ScopePtr& scope, const AstStatCompoundAssign& assign);
    void check(const ScopePtr& scope, const AstStatLocal& local);
    void check(const ScopePtr& scope, const AstStatFor& local);
    void check(const ScopePtr& scope, const AstStatForIn& forin);
    void check(const ScopePtr& scope, TypeId ty, const ScopePtr& funScope, const AstStatFunction& function);
    void check(const ScopePtr& scope, TypeId ty, const ScopePtr& funScope, const AstStatLocalFunction& function);
    void check(const ScopePtr& scope, const AstStatTypeAlias& typealias, int subLevel = 0, bool forwardDeclare = false);
    void check(const ScopePtr& scope, const AstStatDeclareClass& declaredClass);
    void check(const ScopePtr& scope, const AstStatDeclareFunction& declaredFunction);

    void checkBlock(const ScopePtr& scope, const AstStatBlock& statement);
    void checkBlockTypeAliases(const ScopePtr& scope, std::vector<AstStat*>& sorted);

    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExpr& expr, std::optional<TypeId> expectedType = std::nullopt);
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprLocal& expr);
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprGlobal& expr);
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprVarargs& expr);
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprCall& expr);
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprIndexName& expr);
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprIndexExpr& expr);
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprFunction& expr, std::optional<TypeId> expectedType = std::nullopt);
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprTable& expr, std::optional<TypeId> expectedType = std::nullopt);
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprUnary& expr);
    TypeId checkRelationalOperation(
        const ScopePtr& scope, const AstExprBinary& expr, TypeId lhsType, TypeId rhsType, const PredicateVec& predicates = {});
    TypeId checkBinaryOperation(
        const ScopePtr& scope, const AstExprBinary& expr, TypeId lhsType, TypeId rhsType, const PredicateVec& predicates = {});
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprBinary& expr);
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprTypeAssertion& expr);
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprError& expr);
    ExprResult<TypeId> checkExpr(const ScopePtr& scope, const AstExprIfElse& expr);

    TypeId checkExprTable(const ScopePtr& scope, const AstExprTable& expr, const std::vector<std::pair<TypeId, TypeId>>& fieldTypes,
        std::optional<TypeId> expectedType);

    // Returns the type of the lvalue.
    TypeId checkLValue(const ScopePtr& scope, const AstExpr& expr);

    // Returns both the type of the lvalue and its binding (if the caller wants to mutate the binding).
    // Note: the binding may be null.
    std::pair<TypeId, TypeId*> checkLValueBinding(const ScopePtr& scope, const AstExpr& expr);
    std::pair<TypeId, TypeId*> checkLValueBinding(const ScopePtr& scope, const AstExprLocal& expr);
    std::pair<TypeId, TypeId*> checkLValueBinding(const ScopePtr& scope, const AstExprGlobal& expr);
    std::pair<TypeId, TypeId*> checkLValueBinding(const ScopePtr& scope, const AstExprIndexName& expr);
    std::pair<TypeId, TypeId*> checkLValueBinding(const ScopePtr& scope, const AstExprIndexExpr& expr);

    TypeId checkFunctionName(const ScopePtr& scope, AstExpr& funName);
    std::pair<TypeId, ScopePtr> checkFunctionSignature(const ScopePtr& scope, int subLevel, const AstExprFunction& expr,
        std::optional<Location> originalNameLoc, std::optional<TypeId> expectedType);
    void checkFunctionBody(const ScopePtr& scope, TypeId type, const AstExprFunction& function);

    void checkArgumentList(
        const ScopePtr& scope, Unifier& state, TypePackId paramPack, TypePackId argPack, const std::vector<Location>& argLocations);

    ExprResult<TypePackId> checkExprPack(const ScopePtr& scope, const AstExpr& expr);
    ExprResult<TypePackId> checkExprPack(const ScopePtr& scope, const AstExprCall& expr);
    std::vector<std::optional<TypeId>> getExpectedTypesForCall(const std::vector<TypeId>& overloads, size_t argumentCount, bool selfCall);
    std::optional<ExprResult<TypePackId>> checkCallOverload(const ScopePtr& scope, const AstExprCall& expr, TypeId fn, TypePackId retPack,
        TypePackId argPack, TypePack* args, const std::vector<Location>& argLocations, const ExprResult<TypePackId>& argListResult,
        std::vector<TypeId>& overloadsThatMatchArgCount, std::vector<OverloadErrorEntry>& errors);
    bool handleSelfCallMismatch(const ScopePtr& scope, const AstExprCall& expr, TypePack* args, const std::vector<Location>& argLocations,
        const std::vector<OverloadErrorEntry>& errors);
    ExprResult<TypePackId> reportOverloadResolutionError(const ScopePtr& scope, const AstExprCall& expr, TypePackId retPack, TypePackId argPack,
        const std::vector<Location>& argLocations, const std::vector<TypeId>& overloads, const std::vector<TypeId>& overloadsThatMatchArgCount,
        const std::vector<OverloadErrorEntry>& errors);

    ExprResult<TypePackId> checkExprList(const ScopePtr& scope, const Location& location, const AstArray<AstExpr*>& exprs,
        bool substituteFreeForNil = false, const std::vector<bool>& lhsAnnotations = {},
        const std::vector<std::optional<TypeId>>& expectedTypes = {});

    static std::optional<AstExpr*> matchRequire(const AstExprCall& call);
    TypeId checkRequire(const ScopePtr& scope, const ModuleInfo& moduleInfo, const Location& location);

    // Try to infer that the provided type is a table of some sort.
    // Reports an error if the type is already some kind of non-table.
    void tablify(TypeId type);

    /** In nonstrict mode, many typevars need to be replaced by any.
     */
    TypeId anyIfNonstrict(TypeId ty) const;

    /** Attempt to unify the types left and right.  Treat any failures as type errors
     * in the final typecheck report.
     */
    bool unify(TypeId left, TypeId right, const Location& location);
    bool unify(TypePackId left, TypePackId right, const Location& location, CountMismatch::Context ctx = CountMismatch::Context::Arg);

    /** Attempt to unify the types left and right.
     * If this fails, and the right type can be instantiated, do so and try unification again.
     */
    bool unifyWithInstantiationIfNeeded(const ScopePtr& scope, TypeId left, TypeId right, const Location& location);
    void unifyWithInstantiationIfNeeded(const ScopePtr& scope, TypeId left, TypeId right, Unifier& state);

    /** Attempt to unify left with right.
     * If there are errors, undo everything and return the errors.
     * If there are no errors, commit and return an empty error vector.
     */
    ErrorVec tryUnify(TypeId left, TypeId right, const Location& location);
    ErrorVec tryUnify(TypePackId left, TypePackId right, const Location& location);

    // Test whether the two type vars unify.  Never commits the result.
    ErrorVec canUnify(TypeId superTy, TypeId subTy, const Location& location);
    ErrorVec canUnify(TypePackId superTy, TypePackId subTy, const Location& location);

    // Variant that takes a preexisting 'seen' set.  We need this in certain cases to avoid infinitely recursing
    // into cyclic types.
    ErrorVec canUnify(const std::vector<std::pair<TypeId, TypeId>>& seen, TypeId left, TypeId right, const Location& location);

    std::optional<TypeId> findMetatableEntry(TypeId type, std::string entry, const Location& location);
    std::optional<TypeId> findTablePropertyRespectingMeta(TypeId lhsType, Name name, const Location& location);

    std::optional<TypeId> getIndexTypeFromType(const ScopePtr& scope, TypeId type, const Name& name, const Location& location, bool addErrors);

    // Reduces the union to its simplest possible shape.
    // (A | B) | B | C yields A | B | C
    std::vector<TypeId> reduceUnion(const std::vector<TypeId>& types);

    std::optional<TypeId> tryStripUnionFromNil(TypeId ty);
    TypeId stripFromNilAndReport(TypeId ty, const Location& location);

    template<typename Id>
    ErrorVec tryUnify_(Id left, Id right, const Location& location);

    template<typename Id>
    ErrorVec canUnify_(Id left, Id right, const Location& location);

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
    TypeId instantiate(const ScopePtr& scope, TypeId ty, Location location);
    // Removed by FFlag::LuauRankNTypes
    TypePackId DEPRECATED_instantiate(const ScopePtr& scope, TypePackId ty, Location location);

    // Replace any free types or type packs by `any`.
    // This is used when exporting types from modules, to make sure free types don't leak.
    TypeId anyify(const ScopePtr& scope, TypeId ty, Location location);
    TypePackId anyify(const ScopePtr& scope, TypePackId ty, Location location);

    void reportError(const TypeError& error);
    void reportError(const Location& location, TypeErrorData error);
    void reportErrors(const ErrorVec& errors);

    [[noreturn]] void ice(const std::string& message, const Location& location);
    [[noreturn]] void ice(const std::string& message);

    ScopePtr childFunctionScope(const ScopePtr& parent, const Location& location, int subLevel = 0);
    ScopePtr childScope(const ScopePtr& parent, const Location& location, int subLevel = 0);

    // Wrapper for merge(l, r, toUnion) but without the lambda junk.
    void merge(RefinementMap& l, const RefinementMap& r);

private:
    void prepareErrorsForDisplay(ErrorVec& errVec);
    void diagnoseMissingTableKey(UnknownProperty* utk, TypeErrorData& data);
    void reportErrorCodeTooComplex(const Location& location);

private:
    Unifier mkUnifier(const Location& location);
    Unifier mkUnifier(const std::vector<std::pair<TypeId, TypeId>>& seen, const Location& location);

    // These functions are only safe to call when we are in the process of typechecking a module.

    // Produce a new free type var.
    TypeId freshType(const ScopePtr& scope);
    TypeId freshType(TypeLevel level);
    TypeId DEPRECATED_freshType(const ScopePtr& scope, bool canBeGeneric = false);
    TypeId DEPRECATED_freshType(TypeLevel level, bool canBeGeneric = false);

    // Returns nullopt if the predicate filters down the TypeId to 0 options.
    std::optional<TypeId> filterMap(TypeId type, TypeIdPredicate predicate);

    TypeId unionOfTypes(TypeId a, TypeId b, const Location& location, bool unifyFreeTypes = true);

    // ex
    //      TypeId id = addType(FreeTypeVar());
    template<typename T>
    TypeId addType(const T& tv)
    {
        return addTV(TypeVar(tv));
    }

    TypeId addType(const UnionTypeVar& utv);

    TypeId addTV(TypeVar&& tv);

    TypePackId addTypePack(TypePackVar&& tp);
    TypePackId addTypePack(TypePack&& tp);

    TypePackId addTypePack(const std::vector<TypeId>& ty);
    TypePackId addTypePack(const std::vector<TypeId>& ty, std::optional<TypePackId> tail);
    TypePackId addTypePack(std::initializer_list<TypeId>&& ty);
    TypePackId freshTypePack(const ScopePtr& scope);
    TypePackId freshTypePack(TypeLevel level);
    TypePackId DEPRECATED_freshTypePack(const ScopePtr& scope, bool canBeGeneric = false);
    TypePackId DEPRECATED_freshTypePack(TypeLevel level, bool canBeGeneric = false);

    TypeId resolveType(const ScopePtr& scope, const AstType& annotation, bool canBeGeneric = false);
    TypePackId resolveTypePack(const ScopePtr& scope, const AstTypeList& types);
    TypePackId resolveTypePack(const ScopePtr& scope, const AstTypePack& annotation);
    TypeId instantiateTypeFun(const ScopePtr& scope, const TypeFun& tf, const std::vector<TypeId>& typeParams,
        const std::vector<TypePackId>& typePackParams, const Location& location);

    // Note: `scope` must be a fresh scope.
    std::pair<std::vector<TypeId>, std::vector<TypePackId>> createGenericTypes(
        const ScopePtr& scope, std::optional<TypeLevel> levelOpt, const AstNode& node, const AstArray<AstName>& genericNames, const AstArray<AstName>& genericPackNames);

public:
    ErrorVec resolve(const PredicateVec& predicates, const ScopePtr& scope, bool sense);

private:
    std::optional<TypeId> resolveLValue(const ScopePtr& scope, const LValue& lvalue);
    std::optional<TypeId> resolveLValue(const RefinementMap& refis, const ScopePtr& scope, const LValue& lvalue);

    void resolve(const PredicateVec& predicates, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense, bool fromOr = false);
    void resolve(const Predicate& predicate, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense, bool fromOr);
    void resolve(const TruthyPredicate& truthyP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense, bool fromOr);
    void resolve(const AndPredicate& andP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense);
    void resolve(const OrPredicate& orP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense);
    void resolve(const IsAPredicate& isaP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense);
    void resolve(const TypeGuardPredicate& typeguardP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense);
    void DEPRECATED_resolve(const TypeGuardPredicate& typeguardP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense);
    void resolve(const EqPredicate& eqP, ErrorVec& errVec, RefinementMap& refis, const ScopePtr& scope, bool sense);

    bool isNonstrictMode() const;

public:
    /** Extract the types in a type pack, given the assumption that the pack must have some exact length.
     * TypePacks can have free tails, which means that inference has not yet determined the length of the pack.
     * Calling this function means submitting evidence that the pack must have the length provided.
     * If the pack is known not to have the correct length, an error will be reported.
     * The return vector is always of the exact requested length.  In the event that the pack's length does
     * not match up, excess TypeIds will be ErrorTypeVars.
     */
    std::vector<TypeId> unTypePack(const ScopePtr& scope, TypePackId pack, size_t expectedLength, const Location& location);

    TypeArena globalTypes;

    ModuleResolver* resolver;
    SourceModule globalNames; // names for symbols entered into globalScope
    ScopePtr globalScope;     // shared by all modules
    ModulePtr currentModule;
    ModuleName currentModuleName;

    Instantiation instantiation;
    Quantification quantification;
    Anyification anyification;
    ApplyTypeFunction applyTypeFunction;

    std::function<void(const ModuleName&, const ScopePtr&)> prepareModuleScope;
    InternalErrorReporter* iceHandler;

    UnifierSharedState unifierState;

public:
    const TypeId nilType;
    const TypeId numberType;
    const TypeId stringType;
    const TypeId booleanType;
    const TypeId threadType;
    const TypeId anyType;

    const TypeId errorType;
    const TypeId optionalNumberType;

    const TypePackId anyTypePack;
    const TypePackId errorTypePack;

private:
    int checkRecursionCount = 0;
    int recursionCount = 0;
};

// Unit test hook
void setPrintLine(void (*pl)(const std::string& s));
void resetPrintLine();

} // namespace Luau
