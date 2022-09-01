// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/ConstraintSolverLogger.h"

#include "Luau/JsonEmitter.h"
#include "Luau/ToString.h"

LUAU_FASTFLAG(LuauFixNameMaps);

namespace Luau
{

static void dumpScopeAndChildren(const Scope* scope, Json::JsonEmitter& emitter, ToStringOptions& opts)
{
    emitter.writeRaw("{");
    Json::write(emitter, "bindings");
    emitter.writeRaw(":");

    Json::ObjectEmitter o = emitter.writeObject();

    for (const auto& [name, binding] : scope->bindings)
    {
        if (FFlag::LuauFixNameMaps)
            o.writePair(name.c_str(), toString(binding.typeId, opts));
        else
        {
            ToStringResult result = toStringDetailed(binding.typeId, opts);
            opts.DEPRECATED_nameMap = std::move(result.DEPRECATED_nameMap);
            o.writePair(name.c_str(), result.name);
        }
    }

    o.finish();
    emitter.writeRaw(",");
    Json::write(emitter, "children");
    emitter.writeRaw(":");

    Json::ArrayEmitter a = emitter.writeArray();
    for (const Scope* child : scope->children)
    {
        emitter.writeComma();
        dumpScopeAndChildren(child, emitter, opts);
    }

    a.finish();
    emitter.writeRaw("}");
}

static std::string dumpConstraintsToDot(std::vector<NotNull<const Constraint>>& constraints, ToStringOptions& opts)
{
    std::string result = "digraph Constraints {\n";
    result += "rankdir=LR\n";

    std::unordered_set<NotNull<const Constraint>> contained;
    for (NotNull<const Constraint> c : constraints)
    {
        contained.insert(c);
    }

    for (NotNull<const Constraint> c : constraints)
    {
        std::string shape;
        if (get<SubtypeConstraint>(*c))
            shape = "box";
        else if (get<PackSubtypeConstraint>(*c))
            shape = "box3d";
        else
            shape = "oval";

        std::string id = std::to_string(reinterpret_cast<size_t>(c.get()));
        result += id;
        result += " [label=\"";
        result += toString(*c, opts);
        result += "\" shape=" + shape + "];\n";

        for (NotNull<const Constraint> dep : c->dependencies)
        {
            if (contained.count(dep) == 0)
                continue;

            result += std::to_string(reinterpret_cast<size_t>(dep.get()));
            result += " -> ";
            result += id;
            result += ";\n";
        }
    }

    result += "}";

    return result;
}

std::string ConstraintSolverLogger::compileOutput()
{
    Json::JsonEmitter emitter;
    emitter.writeRaw("[");
    for (const std::string& snapshot : snapshots)
    {
        emitter.writeComma();
        emitter.writeRaw(snapshot);
    }

    emitter.writeRaw("]");
    return emitter.str();
}

void ConstraintSolverLogger::captureBoundarySnapshot(const Scope* rootScope, std::vector<NotNull<const Constraint>>& unsolvedConstraints)
{
    Json::JsonEmitter emitter;
    Json::ObjectEmitter o = emitter.writeObject();
    o.writePair("type", "boundary");
    o.writePair("constraintGraph", dumpConstraintsToDot(unsolvedConstraints, opts));
    emitter.writeComma();
    Json::write(emitter, "rootScope");
    emitter.writeRaw(":");
    dumpScopeAndChildren(rootScope, emitter, opts);
    o.finish();

    snapshots.push_back(emitter.str());
}

void ConstraintSolverLogger::prepareStepSnapshot(
    const Scope* rootScope, NotNull<const Constraint> current, std::vector<NotNull<const Constraint>>& unsolvedConstraints, bool force)
{
    Json::JsonEmitter emitter;
    Json::ObjectEmitter o = emitter.writeObject();
    o.writePair("type", "step");
    o.writePair("constraintGraph", dumpConstraintsToDot(unsolvedConstraints, opts));
    o.writePair("currentId", std::to_string(reinterpret_cast<size_t>(current.get())));
    o.writePair("current", toString(*current, opts));
    o.writePair("force", force);
    emitter.writeComma();
    Json::write(emitter, "rootScope");
    emitter.writeRaw(":");
    dumpScopeAndChildren(rootScope, emitter, opts);
    o.finish();

    preparedSnapshot = emitter.str();
}

void ConstraintSolverLogger::commitPreparedStepSnapshot()
{
    if (preparedSnapshot)
    {
        snapshots.push_back(std::move(*preparedSnapshot));
        preparedSnapshot = std::nullopt;
    }
}

} // namespace Luau
