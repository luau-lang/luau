// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/DcrLogger.h"

#include <algorithm>

#include "Luau/JsonEmitter.h"

namespace Luau
{

template<typename T>
static std::string toPointerId(const T* ptr)
{
    return std::to_string(reinterpret_cast<size_t>(ptr));
}

static std::string toPointerId(NotNull<const Constraint> ptr)
{
    return std::to_string(reinterpret_cast<size_t>(ptr.get()));
}

namespace Json
{

template<typename T>
void write(JsonEmitter& emitter, const T* ptr)
{
    write(emitter, toPointerId(ptr));
}

void write(JsonEmitter& emitter, NotNull<const Constraint> ptr)
{
    write(emitter, toPointerId(ptr));
}

void write(JsonEmitter& emitter, const Location& location)
{
    ArrayEmitter a = emitter.writeArray();
    a.writeValue(location.begin.line);
    a.writeValue(location.begin.column);
    a.writeValue(location.end.line);
    a.writeValue(location.end.column);
    a.finish();
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

template<typename K, typename V>
void write(JsonEmitter& emitter, const DenseHashMap<const K*, V>& map)
{
    ObjectEmitter o = emitter.writeObject();
    for (const auto& [k, v] : map)
        o.writePair(toPointerId(k), v);
    o.finish();
}

void write(JsonEmitter& emitter, const ExprTypesAtLocation& tys)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("location", tys.location);
    o.writePair("ty", toPointerId(tys.ty));

    if (tys.expectedTy)
        o.writePair("expectedTy", toPointerId(*tys.expectedTy));

    o.finish();
}

void write(JsonEmitter& emitter, const AnnotationTypesAtLocation& tys)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("location", tys.location);
    o.writePair("resolvedTy", toPointerId(tys.resolvedTy));
    o.finish();
}

void write(JsonEmitter& emitter, const ConstraintGenerationLog& log)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("source", log.source);
    o.writePair("errors", log.errors);
    o.writePair("exprTypeLocations", log.exprTypeLocations);
    o.writePair("annotationTypeLocations", log.annotationTypeLocations);

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

void write(JsonEmitter& emitter, const ConstraintBlock& block)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("stringification", block.stringification);

    auto go = [&o](auto&& t)
    {
        using T = std::decay_t<decltype(t)>;

        o.writePair("id", toPointerId(t));

        if constexpr (std::is_same_v<T, TypeId>)
        {
            o.writePair("kind", "type");
        }
        else if constexpr (std::is_same_v<T, TypePackId>)
        {
            o.writePair("kind", "typePack");
        }
        else if constexpr (std::is_same_v<T, NotNull<const Constraint>>)
        {
            o.writePair("kind", "constraint");
        }
        else
            static_assert(always_false_v<T>, "non-exhaustive possibility switch");
    };

    visit(go, block.target);

    o.finish();
}

void write(JsonEmitter& emitter, const ConstraintSnapshot& snapshot)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("stringification", snapshot.stringification);
    o.writePair("location", snapshot.location);
    o.writePair("blocks", snapshot.blocks);
    o.finish();
}

void write(JsonEmitter& emitter, const BoundarySnapshot& snapshot)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("rootScope", snapshot.rootScope);
    o.writePair("unsolvedConstraints", snapshot.unsolvedConstraints);
    o.writePair("typeStrings", snapshot.typeStrings);
    o.finish();
}

void write(JsonEmitter& emitter, const StepSnapshot& snapshot)
{
    ObjectEmitter o = emitter.writeObject();
    o.writePair("currentConstraint", snapshot.currentConstraint);
    o.writePair("forced", snapshot.forced);
    o.writePair("unsolvedConstraints", snapshot.unsolvedConstraints);
    o.writePair("rootScope", snapshot.rootScope);
    o.writePair("typeStrings", snapshot.typeStrings);
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
        std::move(bindings),
        std::move(typeBindings),
        std::move(typePackBindings),
        std::move(children),
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

void DcrLogger::captureGenerationModule(const ModulePtr& module)
{
    generationLog.exprTypeLocations.reserve(module->astTypes.size());
    for (const auto& [expr, ty] : module->astTypes)
    {
        ExprTypesAtLocation tys;
        tys.location = expr->location;
        tys.ty = ty;

        if (auto expectedTy = module->astExpectedTypes.find(expr))
            tys.expectedTy = *expectedTy;

        generationLog.exprTypeLocations.push_back(tys);
    }

    generationLog.annotationTypeLocations.reserve(module->astResolvedTypes.size());
    for (const auto& [annot, ty] : module->astResolvedTypes)
    {
        AnnotationTypesAtLocation tys;
        tys.location = annot->location;
        tys.resolvedTy = ty;

        generationLog.annotationTypeLocations.push_back(tys);
    }
}

void DcrLogger::captureGenerationError(const TypeError& error)
{
    std::string stringifiedError = toString(error);
    generationLog.errors.push_back(
        ErrorSnapshot{
            /* message */ std::move(stringifiedError),
            /* location */ error.location,
        }
    );
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

static void snapshotTypeStrings(
    const std::vector<ExprTypesAtLocation>& interestedExprs,
    const std::vector<AnnotationTypesAtLocation>& interestedAnnots,
    DenseHashMap<const void*, std::string>& map,
    ToStringOptions& opts
)
{
    for (const ExprTypesAtLocation& tys : interestedExprs)
    {
        map[tys.ty] = toString(tys.ty, opts);

        if (tys.expectedTy)
            map[*tys.expectedTy] = toString(*tys.expectedTy, opts);
    }

    for (const AnnotationTypesAtLocation& tys : interestedAnnots)
    {
        map[tys.resolvedTy] = toString(tys.resolvedTy, opts);
    }
}

void DcrLogger::captureBoundaryState(
    BoundarySnapshot& target,
    const Scope* rootScope,
    const std::vector<NotNull<const Constraint>>& unsolvedConstraints
)
{
    target.rootScope = snapshotScope(rootScope, opts);
    target.unsolvedConstraints.clear();

    for (NotNull<const Constraint> c : unsolvedConstraints)
    {
        target.unsolvedConstraints[c.get()] = {
            toString(*c.get(), opts),
            c->location,
            snapshotBlocks(c),
        };
    }

    snapshotTypeStrings(generationLog.exprTypeLocations, generationLog.annotationTypeLocations, target.typeStrings, opts);
}

void DcrLogger::captureInitialSolverState(const Scope* rootScope, const std::vector<NotNull<const Constraint>>& unsolvedConstraints)
{
    captureBoundaryState(solveLog.initialState, rootScope, unsolvedConstraints);
}

StepSnapshot DcrLogger::prepareStepSnapshot(
    const Scope* rootScope,
    NotNull<const Constraint> current,
    bool force,
    const std::vector<NotNull<const Constraint>>& unsolvedConstraints
)
{
    ScopeSnapshot scopeSnapshot = snapshotScope(rootScope, opts);
    DenseHashMap<const Constraint*, ConstraintSnapshot> constraints{nullptr};

    for (NotNull<const Constraint> c : unsolvedConstraints)
    {
        constraints[c.get()] = {
            toString(*c.get(), opts),
            c->location,
            snapshotBlocks(c),
        };
    }

    DenseHashMap<const void*, std::string> typeStrings{nullptr};
    snapshotTypeStrings(generationLog.exprTypeLocations, generationLog.annotationTypeLocations, typeStrings, opts);

    return StepSnapshot{
        current,
        force,
        std::move(constraints),
        std::move(scopeSnapshot),
        std::move(typeStrings),
    };
}

void DcrLogger::commitStepSnapshot(StepSnapshot snapshot)
{
    solveLog.stepStates.push_back(std::move(snapshot));
}

void DcrLogger::captureFinalSolverState(const Scope* rootScope, const std::vector<NotNull<const Constraint>>& unsolvedConstraints)
{
    captureBoundaryState(solveLog.finalState, rootScope, unsolvedConstraints);
}

void DcrLogger::captureTypeCheckError(const TypeError& error)
{
    std::string stringifiedError = toString(error);
    checkLog.errors.push_back(
        ErrorSnapshot{
            /* message */ std::move(stringifiedError),
            /* location */ error.location,
        }
    );
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
                *ty,
                toString(*ty, opts),
            });
        }
        else if (const TypePackId* tp = get_if<TypePackId>(&target))
        {
            snapshot.push_back({
                *tp,
                toString(*tp, opts),
            });
        }
        else if (const NotNull<const Constraint>* c = get_if<NotNull<const Constraint>>(&target))
        {
            snapshot.push_back({
                *c,
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
