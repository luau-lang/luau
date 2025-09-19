// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Common.h"
#include "Luau/EqSatSimplification.h"
#include "Luau/Error.h"
#include "Luau/Normalize.h"
#include "Luau/NotNull.h"
#include "Luau/Subtyping.h"
#include "Luau/Type.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeOrPack.h"
#include "Luau/TypeUtils.h"

namespace Luau
{

struct BuiltinTypes;
struct DcrLogger;
struct TypeCheckLimits;
struct UnifierSharedState;
struct SourceModule;
struct Module;
struct InternalErrorReporter;
struct Scope;
struct PropertyType;
struct PropertyTypes;
struct StackPusher;

struct Reasonings
{
    // the list of reasons
    std::vector<std::string> reasons;

    // this should be true if _all_ of the reasons have an error suppressing type, and false otherwise.
    bool suppressed;

    std::string toString()
    {
        if (reasons.empty())
            return "";

        // DenseHashSet ordering is entirely undefined, so we want to
        // sort the reasons here to achieve a stable error
        // stringification.
        std::sort(reasons.begin(), reasons.end());
        std::string allReasons = "\nthis is because ";
        for (const std::string& reason : reasons)
        {
            if (reasons.size() > 1)
                allReasons += "\n\t * ";

            allReasons += reason;
        }

        return allReasons;
    }
};


void check(
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Simplifier> simplifier,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    NotNull<UnifierSharedState> unifierState,
    NotNull<TypeCheckLimits> limits,
    DcrLogger* logger,
    const SourceModule& sourceModule,
    Module* module
);

struct TypeChecker2
{
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<Simplifier> simplifier;
    NotNull<TypeFunctionRuntime> typeFunctionRuntime;
    DcrLogger* logger;
    const NotNull<TypeCheckLimits> limits;
    const NotNull<InternalErrorReporter> ice;
    const SourceModule* sourceModule;
    Module* module;

    TypeContext typeContext = TypeContext::Default;
    std::vector<NotNull<Scope>> stack;
    std::vector<TypeId> functionDeclStack;

    DenseHashSet<TypeId> seenTypeFunctionInstances{nullptr};

    Normalizer normalizer;
    Subtyping _subtyping;
    NotNull<Subtyping> subtyping;

    TypeChecker2(
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<Simplifier> simplifier,
        NotNull<TypeFunctionRuntime> typeFunctionRuntime,
        NotNull<UnifierSharedState> unifierState,
        NotNull<TypeCheckLimits> limits,
        DcrLogger* logger,
        const SourceModule* sourceModule,
        Module* module
    );

    void visit(AstStatBlock* block);
    void reportError(TypeErrorData data, const Location& location);
    Reasonings explainReasonings(TypeId subTy, TypeId superTy, Location location, const SubtypingResult& r);
    Reasonings explainReasonings(TypePackId subTp, TypePackId superTp, Location location, const SubtypingResult& r);

private:
    static bool allowsNoReturnValues(const TypePackId tp);
    static Location getEndLocation(const AstExprFunction* function);
    bool isErrorCall(const AstExprCall* call);
    bool hasBreak(AstStat* node);
    const AstStat* getFallthrough(const AstStat* node);
    std::optional<StackPusher> pushStack(AstNode* node);
    void checkForInternalTypeFunction(TypeId ty, Location location);
    TypeId checkForTypeFunctionInhabitance(TypeId instance, Location location);
    TypePackId lookupPack(AstExpr* expr) const;
    TypeId lookupType(AstExpr* expr);
    TypeId lookupAnnotation(AstType* annotation);
    std::optional<TypePackId> lookupPackAnnotation(AstTypePack* annotation) const;
    TypeId lookupExpectedType(AstExpr* expr) const;
    TypePackId lookupExpectedPack(AstExpr* expr, TypeArena& arena) const;
    TypePackId reconstructPack(AstArray<AstExpr*> exprs, TypeArena& arena);
    Scope* findInnermostScope(Location location) const;
    void visit(AstStat* stat);
    void visit(AstStatIf* ifStatement);
    void visit(AstStatWhile* whileStatement);
    void visit(AstStatRepeat* repeatStatement);
    void visit(AstStatBreak*);
    void visit(AstStatContinue*);
    void visit(AstStatReturn* ret);
    void visit(AstStatExpr* expr);
    void visit(AstStatLocal* local);
    void visit(AstStatFor* forStatement);
    void visit(AstStatForIn* forInStatement);
    std::optional<TypeId> getBindingType(AstExpr* expr);
    void reportErrorsFromAssigningToNever(AstExpr* lhs, TypeId rhsType);
    void visit(AstStatAssign* assign);
    void visit(AstStatCompoundAssign* stat);
    void visit(AstStatFunction* stat);
    void visit(AstStatLocalFunction* stat);
    void visit(const AstTypeList* typeList);
    void visit(AstStatTypeAlias* stat);
    void visit(AstStatTypeFunction* stat);
    void visit(AstTypeList types);
    void visit(AstStatDeclareFunction* stat);
    void visit(AstStatDeclareGlobal* stat);
    void visit(AstStatDeclareExternType* stat);
    void visit(AstStatError* stat);
    void visit(AstExpr* expr, ValueContext context);
    void visit(AstExprGroup* expr, ValueContext context);
    void visit(AstExprConstantNil* expr);
    void visit(AstExprConstantBool* expr);
    void visit(AstExprConstantNumber* expr);
    void visit(AstExprConstantString* expr);
    void visit(AstExprLocal* expr);
    void visit(AstExprGlobal* expr);
    void visit(AstExprVarargs* expr);
    void visitCall(AstExprCall* call);
    void visit(AstExprCall* call);
    std::optional<TypeId> tryStripUnionFromNil(TypeId ty) const;
    TypeId stripFromNilAndReport(TypeId ty, const Location& location);
    void visitExprName(AstExpr* expr, Location location, const std::string& propName, ValueContext context, TypeId astIndexExprTy);
    void visit(AstExprIndexName* indexName, ValueContext context);
    void indexExprMetatableHelper(AstExprIndexExpr* indexExpr, const MetatableType* metaTable, TypeId exprType, TypeId indexType);
    void visit(AstExprIndexExpr* indexExpr, ValueContext context);
    void visit(AstExprFunction* fn);
    void visit(AstExprTable* expr);
    void visit(AstExprUnary* expr);
    TypeId visit(AstExprBinary* expr, AstNode* overrideKey = nullptr);
    void visit(AstExprTypeAssertion* expr);
    void visit(AstExprIfElse* expr);
    void visit(AstExprInterpString* interpString);
    void visit(AstExprError* expr);
    TypeId flattenPack(TypePackId pack);
    void visitGenerics(AstArray<AstGenericType*> generics, AstArray<AstGenericTypePack*> genericPacks);
    void visit(AstType* ty);
    void visit(AstTypeReference* ty);
    void visit(AstTypeTable* table);
    void visit(AstTypeFunction* ty);
    void visit(AstTypeTypeof* ty);
    void visit(AstTypeUnion* ty);
    void visit(AstTypeIntersection* ty);
    void visit(AstTypePack* pack);
    void visit(AstTypePackExplicit* tp);
    void visit(AstTypePackVariadic* tp);
    void visit(AstTypePackGeneric* tp);

    template<typename TID>
    Reasonings explainReasonings_(TID subTy, TID superTy, Location location, const SubtypingResult& r);

    void explainError(TypeId subTy, TypeId superTy, Location location, const SubtypingResult& result);
    void explainError(TypePackId subTy, TypePackId superTy, Location location, const SubtypingResult& result);

    bool testLiteralOrAstTypeIsSubtype(AstExpr* expr, TypeId expectedType);

    bool testPotentialLiteralIsSubtype(AstExpr* expr, TypeId expectedType);

    bool testIsSubtype(TypeId subTy, TypeId superTy, Location location);
    bool testIsSubtype(TypePackId subTy, TypePackId superTy, Location location);

    void maybeReportSubtypingError(TypeId subTy, TypeId superTy, const Location& location);
    // Tests whether subTy is a subtype of superTy in the context of a function iterator for a for-in statement.
    // Includes some extra logic to help locate errors to the values and variables of the for-in statement.
    void testIsSubtypeForInStat(TypeId iterFunc, TypeId prospectiveFunc, const AstStatForIn& forInStat);

    void reportError(TypeError e);
    void reportErrors(ErrorVec errors);
    PropertyTypes lookupProp(
        const NormalizedType* norm,
        const std::string& prop,
        ValueContext context,
        const Location& location,
        TypeId astIndexExprType,
        std::vector<TypeError>& errors
    );
    // If the provided type does not have the named property, report an error.
    void checkIndexTypeFromType(TypeId tableTy, const std::string& prop, ValueContext context, const Location& location, TypeId astIndexExprType);
    PropertyType hasIndexTypeFromType(
        TypeId ty,
        const std::string& prop,
        ValueContext context,
        const Location& location,
        DenseHashSet<TypeId>& seen,
        TypeId astIndexExprType,
        std::vector<TypeError>& errors
    );

    // Avoid duplicate warnings being emitted for the same global variable.
    DenseHashSet<std::string> warnedGlobals{""};

    void suggestAnnotations(AstExprFunction* expr, TypeId ty);

    void diagnoseMissingTableKey(UnknownProperty* utk, TypeErrorData& data) const;
    bool isErrorSuppressing(Location loc, TypeId ty);
    bool isErrorSuppressing(Location loc1, TypeId ty1, Location loc2, TypeId ty2);
    bool isErrorSuppressing(Location loc, TypePackId tp);
    bool isErrorSuppressing(Location loc1, TypePackId tp1, Location loc2, TypePackId tp2);

    // Returns whether we reported any errors
    bool reportNonviableOverloadErrors(
        std::vector<std::pair<TypeId, ErrorVec>> nonviableOverloads,
        Location callFuncLocation,
        size_t argHeadSize,
        Location callLocation
    );
};

} // namespace Luau
