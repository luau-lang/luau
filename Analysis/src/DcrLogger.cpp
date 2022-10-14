// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/DcrLogger.h"

#include <algorithm>

#include "Luau/JsonEmitter.h"

namespace Luau
{

namespace Json
{

void write(JsonEmitter& emitter, const Location& location)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("beginLine", location.begin.line);
    o.writePair("beginColumn", location.begin.column);
    o.writePair("endLine", location.end.line);
    o.writePair("endColumn", location.end.column);
    o.finish();
}

void write(JsonEmitter& emitter, const ErrorSnapshot& snapshot)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("message", snapshot.message);
    o.writePair("location", snapshot.location);
    o.finish();
}

void write(JsonEmitter& emitter, const BindingSnapshot& snapshot)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("typeId", snapshot.typeId);
    o.writePair("typeString", snapshot.typeString);
    o.writePair("location", snapshot.location);
    o.finish();
}

void write(JsonEmitter& emitter, const TypeBindingSnapshot& snapshot)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("typeId", snapshot.typeId);
    o.writePair("typeString", snapshot.typeString);
    o.finish();
}

void write(JsonEmitter& emitter, const ConstraintGenerationLog& log)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("source", log.source);

    emitter.writeComma();
    write(emitter, "constraintLocations");
    emitter.writeRaw(":");

    ObjectEmitter locationEmitter = emitter.writeObject();

    for (const auto& [id, location] : log.constraintLocations)
    {
        locationEmitter.writePair(id, location);
    }

    locationEmitter.finish();
    o.writePair("errors", log.errors);
    o.finish();
}

void write(JsonEmitter& emitter, const ScopeSnapshot& snapshot)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("bindings", snapshot.bindings);
    o.writePair("typeBindings", snapshot.typeBindings);
    o.writePair("typePackBindings", snapshot.typePackBindings);
    o.writePair("children", snapshot.children);
    o.finish();
}

void write(JsonEmitter& emitter, const ConstraintBlockKind& kind)
{
    switch (kind)
    {
    case ConstraintBlockKind::TypeId:
        return write(emitter, "type");
    case ConstraintBlockKind::TypePackId:
        return write(emitter, "typePack");
    case ConstraintBlockKind::ConstraintId:
        return write(emitter, "constraint");
    default:
        LUAU_ASSERT(0);
    }
}

void write(JsonEmitter& emitter, const ConstraintBlock& block)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("kind", block.kind);
    o.writePair("stringification", block.stringification);
    o.finish();
}

void write(JsonEmitter& emitter, const ConstraintSnapshot& snapshot)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("stringification", snapshot.stringification);
    o.writePair("blocks", snapshot.blocks);
    o.finish();
}

void write(JsonEmitter& emitter, const BoundarySnapshot& snapshot)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("rootScope", snapshot.rootScope);
    o.writePair("constraints", snapshot.constraints);
    o.finish();
}

void write(JsonEmitter& emitter, const StepSnapshot& snapshot)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("currentConstraint", snapshot.currentConstraint);
    o.writePair("forced", snapshot.forced);
    o.writePair("unsolvedConstraints", snapshot.unsolvedConstraints);
    o.writePair("rootScope", snapshot.rootScope);
    o.finish();
}

void write(JsonEmitter& emitter, const TypeSolveLog& log)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("initialState", log.initialState);
    o.writePair("stepStates", log.stepStates);
    o.writePair("finalState", log.finalState);
    o.finish();
}

void write(JsonEmitter& emitter, const TypeCheckLog& log)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("errors", log.errors);
    o.finish();
}

} // namespace Json

static std::string toPointerId(NotNull<const Constraint> ptr)
{
    return std::to_string(reinterpret_cast<size_t>(ptr.get()));
}

static ScopeSnapshot snapshotScope(const Scope* scope, ToStringOptions& opts)
{
    std::unordered_map<Name, BindingSnapshot> bindings;
    std::unordered_map<Name, TypeBindingSnapshot> typeBindings;
    std::unordered_map<Name, TypeBindingSnapshot> typePackBindings;
    std::vector<ScopeSnapshot> children;

    for (const auto& [name, binding] : scope->bindings)
    {
        std::string id = std::to_string(reinterpret_cast<size_t>(binding.typeId));
        ToStringResult result = toStringDetailed(binding.typeId, opts);

        bindings[name.c_str()] = BindingSnapshot{
            id,
            result.name,
            binding.location,
        };
    }

    for (const auto& [name, tf] : scope->exportedTypeBindings)
    {
        std::string id = std::to_string(reinterpret_cast<size_t>(tf.type));

        typeBindings[name] = TypeBindingSnapshot{
            id,
            toString(tf.type, opts),
        };
    }

    for (const auto& [name, tf] : scope->privateTypeBindings)
    {
        std::string id = std::to_string(reinterpret_cast<size_t>(tf.type));

        typeBindings[name] = TypeBindingSnapshot{
            id,
            toString(tf.type, opts),
        };
    }

    for (const auto& [name, tp] : scope->privateTypePackBindings)
    {
        std::string id = std::to_string(reinterpret_cast<size_t>(tp));

        typePackBindings[name] = TypeBindingSnapshot{
            id,
            toString(tp, opts),
        };
    }

    for (const auto& child : scope->children)
    {
        children.push_back(snapshotScope(child.get(), opts));
    }

    return ScopeSnapshot{
        bindings,
        typeBindings,
        typePackBindings,
        children,
    };
}

std::string DcrLogger::compileOutput()
{
    Json::JsonEmitter emitter;
    Json::ObjectEmitter o = emitter.writeObject();
    o.writePair("generation", generationLog);
    o.writePair("solve", solveLog);
    o.writePair("check", checkLog);
    o.finish();

    return emitter.str();
}

void DcrLogger::captureSource(std::string source)
{
    generationLog.source = std::move(source);
}

void DcrLogger::captureGenerationError(const TypeError& error)
{
    std::string stringifiedError = toString(error);
    generationLog.errors.push_back(ErrorSnapshot{
        /* message */ stringifiedError,
        /* location */ error.location,
    });
}

void DcrLogger::captureConstraintLocation(NotNull<const Constraint> constraint, Location location)
{
    std::string id = toPointerId(constraint);
    generationLog.constraintLocations[id] = location;
}

void DcrLogger::pushBlock(NotNull<const Constraint> constraint, TypeId block)
{
    constraintBlocks[constraint].push_back(block);
}

void DcrLogger::pushBlock(NotNull<const Constraint> constraint, TypePackId block)
{
    constraintBlocks[constraint].push_back(block);
}

void DcrLogger::pushBlock(NotNull<const Constraint> constraint, NotNull<const Constraint> block)
{
    constraintBlocks[constraint].push_back(block);
}

void DcrLogger::popBlock(TypeId block)
{
    for (auto& [_, list] : constraintBlocks)
    {
        list.erase(std::remove(list.begin(), list.end(), block), list.end());
    }
}

void DcrLogger::popBlock(TypePackId block)
{
    for (auto& [_, list] : constraintBlocks)
    {
        list.erase(std::remove(list.begin(), list.end(), block), list.end());
    }
}

void DcrLogger::popBlock(NotNull<const Constraint> block)
{
    for (auto& [_, list] : constraintBlocks)
    {
        list.erase(std::remove(list.begin(), list.end(), block), list.end());
    }
}

void DcrLogger::captureInitialSolverState(const Scope* rootScope, const std::vector<NotNull<const Constraint>>& unsolvedConstraints)
{
    solveLog.initialState.rootScope = snapshotScope(rootScope, opts);
    solveLog.initialState.constraints.clear();

    for (NotNull<const Constraint> c : unsolvedConstraints)
    {
        std::string id = toPointerId(c);
        solveLog.initialState.constraints[id] = {
            toString(*c.get(), opts),
            snapshotBlocks(c),
        };
    }
}

StepSnapshot DcrLogger::prepareStepSnapshot(
    const Scope* rootScope, NotNull<const Constraint> current, bool force, const std::vector<NotNull<const Constraint>>& unsolvedConstraints)
{
    ScopeSnapshot scopeSnapshot = snapshotScope(rootScope, opts);
    std::string currentId = toPointerId(current);
    std::unordered_map<std::string, ConstraintSnapshot> constraints;

    for (NotNull<const Constraint> c : unsolvedConstraints)
    {
        std::string id = toPointerId(c);
        constraints[id] = {
            toString(*c.get(), opts),
            snapshotBlocks(c),
        };
    }

    return StepSnapshot{
        currentId,
        force,
        constraints,
        scopeSnapshot,
    };
}

void DcrLogger::commitStepSnapshot(StepSnapshot snapshot)
{
    solveLog.stepStates.push_back(std::move(snapshot));
}

void DcrLogger::captureFinalSolverState(const Scope* rootScope, const std::vector<NotNull<const Constraint>>& unsolvedConstraints)
{
    solveLog.finalState.rootScope = snapshotScope(rootScope, opts);
    solveLog.finalState.constraints.clear();

    for (NotNull<const Constraint> c : unsolvedConstraints)
    {
        std::string id = toPointerId(c);
        solveLog.finalState.constraints[id] = {
            toString(*c.get(), opts),
            snapshotBlocks(c),
        };
    }
}

void DcrLogger::captureTypeCheckError(const TypeError& error)
{
    std::string stringifiedError = toString(error);
    checkLog.errors.push_back(ErrorSnapshot{
        /* message */ stringifiedError,
        /* location */ error.location,
    });
}

std::vector<ConstraintBlock> DcrLogger::snapshotBlocks(NotNull<const Constraint> c)
{
    auto it = constraintBlocks.find(c);
    if (it == constraintBlocks.end())
    {
        return {};
    }

    std::vector<ConstraintBlock> snapshot;

    for (const ConstraintBlockTarget& target : it->second)
    {
        if (const TypeId* ty = get_if<TypeId>(&target))
        {
            snapshot.push_back({
                ConstraintBlockKind::TypeId,
                toString(*ty, opts),
            });
        }
        else if (const TypePackId* tp = get_if<TypePackId>(&target))
        {
            snapshot.push_back({
                ConstraintBlockKind::TypePackId,
                toString(*tp, opts),
            });
        }
        else if (const NotNull<const Constraint>* c = get_if<NotNull<const Constraint>>(&target))
        {
            snapshot.push_back({
                ConstraintBlockKind::ConstraintId,
                toString(*(c->get()), opts),
            });
        }
        else
        {
            LUAU_ASSERT(0);
        }
    }

    return snapshot;
}

} // namespace Luau
