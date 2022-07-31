// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details

#include "lluz/Constraint.h"
#include "lluz/NotNull.h"
#include "lluz/Scope.h"
#include "lluz/ToString.h"

#include <optional>
#include <string>
#include <vector>

namespace lluz
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

} // namespace lluz
