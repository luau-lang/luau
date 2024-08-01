// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Constraint.h"
#include "Luau/NotNull.h"
#include "Luau/Scope.h"
#include "Luau/Module.h"
#include "Luau/ToString.h"
#include "Luau/Error.h"
#include "Luau/Variant.h"

#include <optional>
#include <string>
#include <vector>

namespace Luau
{

struct ErrorSnapshot
{
    std::string message;
    Location location;
};

struct BindingSnapshot
{
    std::string typeId;
    std::string typeString;
    Location location;
};

struct TypeBindingSnapshot
{
    std::string typeId;
    std::string typeString;
};

struct ExprTypesAtLocation
{
    Location location;
    TypeId ty;
    std::optional<TypeId> expectedTy;
};

struct AnnotationTypesAtLocation
{
    Location location;
    TypeId resolvedTy;
};

struct ConstraintGenerationLog
{
    std::string source;
    std::vector<ErrorSnapshot> errors;

    std::vector<ExprTypesAtLocation> exprTypeLocations;
    std::vector<AnnotationTypesAtLocation> annotationTypeLocations;
};

struct ScopeSnapshot
{
    std::unordered_map<Name, BindingSnapshot> bindings;
    std::unordered_map<Name, TypeBindingSnapshot> typeBindings;
    std::unordered_map<Name, TypeBindingSnapshot> typePackBindings;
    std::vector<ScopeSnapshot> children;
};

using ConstraintBlockTarget = Variant<TypeId, TypePackId, NotNull<const Constraint>>;

struct ConstraintBlock
{
    ConstraintBlockTarget target;
    std::string stringification;
};

struct ConstraintSnapshot
{
    std::string stringification;
    Location location;
    std::vector<ConstraintBlock> blocks;
};

struct BoundarySnapshot
{
    DenseHashMap<const Constraint*, ConstraintSnapshot> unsolvedConstraints{nullptr};
    ScopeSnapshot rootScope;
    DenseHashMap<const void*, std::string> typeStrings{nullptr};
};

struct StepSnapshot
{
    const Constraint* currentConstraint;
    bool forced;
    DenseHashMap<const Constraint*, ConstraintSnapshot> unsolvedConstraints{nullptr};
    ScopeSnapshot rootScope;
    DenseHashMap<const void*, std::string> typeStrings{nullptr};
};

struct TypeSolveLog
{
    BoundarySnapshot initialState;
    std::vector<StepSnapshot> stepStates;
    BoundarySnapshot finalState;
};

struct TypeCheckLog
{
    std::vector<ErrorSnapshot> errors;
};

struct DcrLogger
{
    std::string compileOutput();

    void captureSource(std::string source);
    void captureGenerationError(const TypeError& error);
    void captureConstraintLocation(NotNull<const Constraint> constraint, Location location);
    void captureGenerationModule(const ModulePtr& module);

    void pushBlock(NotNull<const Constraint> constraint, TypeId block);
    void pushBlock(NotNull<const Constraint> constraint, TypePackId block);
    void pushBlock(NotNull<const Constraint> constraint, NotNull<const Constraint> block);
    void popBlock(TypeId block);
    void popBlock(TypePackId block);
    void popBlock(NotNull<const Constraint> block);

    void captureInitialSolverState(const Scope* rootScope, const std::vector<NotNull<const Constraint>>& unsolvedConstraints);
    StepSnapshot prepareStepSnapshot(
        const Scope* rootScope,
        NotNull<const Constraint> current,
        bool force,
        const std::vector<NotNull<const Constraint>>& unsolvedConstraints
    );
    void commitStepSnapshot(StepSnapshot snapshot);
    void captureFinalSolverState(const Scope* rootScope, const std::vector<NotNull<const Constraint>>& unsolvedConstraints);

    void captureTypeCheckError(const TypeError& error);

private:
    ConstraintGenerationLog generationLog;
    std::unordered_map<NotNull<const Constraint>, std::vector<ConstraintBlockTarget>> constraintBlocks;
    TypeSolveLog solveLog;
    TypeCheckLog checkLog;

    ToStringOptions opts{true};

    std::vector<ConstraintBlock> snapshotBlocks(NotNull<const Constraint> constraint);
    void captureBoundaryState(BoundarySnapshot& target, const Scope* rootScope, const std::vector<NotNull<const Constraint>>& unsolvedConstraints);
};

} // namespace Luau
