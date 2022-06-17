// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/ConstraintSolverLogger.h"

namespace Luau
{

static std::string dumpScopeAndChildren(const Scope2* scope, ToStringOptions& opts)
{
    std::string output = "{\"bindings\":{";

    bool comma = false;
    for (const auto& [name, type] : scope->bindings)
    {
        if (comma)
            output += ",";

        output += "\"";
        output += name.c_str();
        output += "\": \"";

        ToStringResult result = toStringDetailed(type, opts);
        opts.nameMap = std::move(result.nameMap);
        output += result.name;
        output += "\"";

        comma = true;
    }

    output += "},\"children\":[";
    comma = false;

    for (const Scope2* child : scope->children)
    {
        if (comma)
            output += ",";

        output += dumpScopeAndChildren(child, opts);
        comma = true;
    }

    output += "]}";
    return output;
}

static std::string dumpConstraintsToDot(std::vector<NotNull<const Constraint>>& constraints, ToStringOptions& opts)
{
    std::string result = "digraph Constraints {\\n";

    std::unordered_set<NotNull<const Constraint>> contained;
    for (NotNull<const Constraint> c : constraints)
    {
        contained.insert(c);
    }

    for (NotNull<const Constraint> c : constraints)
    {
        std::string id = std::to_string(reinterpret_cast<size_t>(c.get()));
        result += id;
        result += " [label=\\\"";
        result += toString(*c, opts).c_str();
        result += "\\\"];\\n";

        for (NotNull<const Constraint> dep : c->dependencies)
        {
            if (contained.count(dep) == 0)
                continue;

            result += std::to_string(reinterpret_cast<size_t>(dep.get()));
            result += " -> ";
            result += id;
            result += ";\\n";
        }
    }

    result += "}";

    return result;
}

std::string ConstraintSolverLogger::compileOutput()
{
    std::string output = "[";
    bool comma = false;

    for (const std::string& snapshot : snapshots)
    {
        if (comma)
            output += ",";
        output += snapshot;

        comma = true;
    }

    output += "]";
    return output;
}

void ConstraintSolverLogger::captureBoundarySnapshot(const Scope2* rootScope, std::vector<NotNull<const Constraint>>& unsolvedConstraints)
{
    std::string snapshot = "{\"type\":\"boundary\",\"rootScope\":";

    snapshot += dumpScopeAndChildren(rootScope, opts);
    snapshot += ",\"constraintGraph\":\"";
    snapshot += dumpConstraintsToDot(unsolvedConstraints, opts);
    snapshot += "\"}";

    snapshots.push_back(std::move(snapshot));
}

void ConstraintSolverLogger::prepareStepSnapshot(
    const Scope2* rootScope, NotNull<const Constraint> current, std::vector<NotNull<const Constraint>>& unsolvedConstraints)
{
    // LUAU_ASSERT(!preparedSnapshot);

    std::string snapshot = "{\"type\":\"step\",\"rootScope\":";

    snapshot += dumpScopeAndChildren(rootScope, opts);
    snapshot += ",\"constraintGraph\":\"";
    snapshot += dumpConstraintsToDot(unsolvedConstraints, opts);
    snapshot += "\",\"currentId\":\"";
    snapshot += std::to_string(reinterpret_cast<size_t>(current.get()));
    snapshot += "\",\"current\":\"";
    snapshot += toString(*current, opts);
    snapshot += "\"}";

    preparedSnapshot = std::move(snapshot);
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
