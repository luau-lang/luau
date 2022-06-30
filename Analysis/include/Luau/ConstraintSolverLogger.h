// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Constraint.h"
#include "Luau/NotNull.h"
#include "Luau/Scope.h"
#include "Luau/ToString.h"

#include <optional>
#include <string>
#include <vector>

namespace Luau
{

struct ConstraintSolverLogger
{
    std::string compileOutput();
    void captureBoundarySnapshot(const Scope2* rootScope, std::vector<NotNull<const Constraint>>& unsolvedConstraints);
    void prepareStepSnapshot(const Scope2* rootScope, NotNull<const Constraint> current, std::vector<NotNull<const Constraint>>& unsolvedConstraints);
    void commitPreparedStepSnapshot();

private:
    std::vector<std::string> snapshots;
    std::optional<std::string> preparedSnapshot;
    ToStringOptions opts;
};

} // namespace Luau
