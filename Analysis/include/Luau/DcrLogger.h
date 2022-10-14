// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Constraint.h"
#include "Luau/NotNull.h"
#include "Luau/Scope.h"
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

struct ConstraintGenerationLog
{
    std::string source;
    std::unordered_map<std::string, Location> constraintLocations;
    std::vector<ErrorSnapshot> errors;
};

struct ScopeSnapshot
{
    std::unordered_map<Name, BindingSnapshot> bindings;
    std::unordered_map<Name, TypeBindingSnapshot> typeBindings;
    std::unordered_map<Name, TypeBindingSnapshot> typePackBindings;
    std::vector<ScopeSnapshot> children;
};

enum class ConstraintBlockKind
{
    TypeId,
    TypePackId,
    ConstraintId,
};

struct ConstraintBlock
{
    ConstraintBlockKind kind;
    std::string stringification;
};

struct ConstraintSnapshot
{
    std::string stringification;
    std::vector<ConstraintBlock> blocks;
};

struct BoundarySnapshot
{
    std::unordered_map<std::string, ConstraintSnapshot> constraints;
    ScopeSnapshot rootScope;
};

struct StepSnapshot
{
    std::string currentConstraint;
    bool forced;
    std::unordered_map<std::string, ConstraintSnapshot> unsolvedConstraints;
    ScopeSnapshot rootScope;
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

using ConstraintBlockTarget = Variant<TypeId, TypePackId, NotNull<const Constraint>>;

struct DcrLogger
{
    std::string compileOutput();

    void captureSource(std::string source);
    void captureGenerationError(const TypeError& error);
    void captureConstraintLocation(NotNull<const Constraint> constraint, Location location);

    void pushBlock(NotNull<const Constraint> constraint, TypeId block);
    void pushBlock(NotNull<const Constraint> constraint, TypePackId block);
    void pushBlock(NotNull<const Constraint> constraint, NotNull<const Constraint> block);
    void popBlock(TypeId block);
    void popBlock(TypePackId block);
    void popBlock(NotNull<const Constraint> block);

    void captureInitialSolverState(const Scope* rootScope, const std::vector<NotNull<const Constraint>>& unsolvedConstraints);
    StepSnapshot prepareStepSnapshot(
        const Scope* rootScope, NotNull<const Constraint> current, bool force, const std::vector<NotNull<const Constraint>>& unsolvedConstraints);
    void commitStepSnapshot(StepSnapshot snapshot);
    void captureFinalSolverState(const Scope* rootScope, const std::vector<NotNull<const Constraint>>& unsolvedConstraints);

    void captureTypeCheckError(const TypeError& error);

private:
    ConstraintGenerationLog generationLog;
    std::unordered_map<NotNull<const Constraint>, std::vector<ConstraintBlockTarget>> constraintBlocks;
    TypeSolveLog solveLog;
    TypeCheckLog checkLog;

    ToStringOptions opts;

    std::vector<ConstraintBlock> snapshotBlocks(NotNull<const Constraint> constraint);
};

} // namespace Luau
