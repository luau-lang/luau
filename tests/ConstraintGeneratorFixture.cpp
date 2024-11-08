// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "ConstraintGeneratorFixture.h"
#include "ScopedFlags.h"

LUAU_FASTFLAG(LuauSolverV2);

namespace Luau
{

ConstraintGeneratorFixture::ConstraintGeneratorFixture()
    : Fixture()
    , mainModule(new Module)
    , simplifier(newSimplifier(NotNull{&arena}, builtinTypes))
    , forceTheFlag{FFlag::LuauSolverV2, true}
{
    mainModule->name = "MainModule";
    mainModule->humanReadableName = "MainModule";

    BlockedTypePack::nextIndex = 0;
}

void ConstraintGeneratorFixture::generateConstraints(const std::string& code)
{
    AstStatBlock* root = parse(code);
    dfg = std::make_unique<DataFlowGraph>(DataFlowGraphBuilder::build(root, NotNull{&ice}));
    cg = std::make_unique<ConstraintGenerator>(
        mainModule,
        NotNull{&normalizer},
        NotNull{simplifier.get()},
        NotNull{&typeFunctionRuntime},
        NotNull(&moduleResolver),
        builtinTypes,
        NotNull(&ice),
        frontend.globals.globalScope,
        /*prepareModuleScope*/ nullptr,
        &logger,
        NotNull{dfg.get()},
        std::vector<RequireCycle>()
    );
    cg->visitModuleRoot(root);
    rootScope = cg->rootScope;
    constraints = Luau::borrowConstraints(cg->constraints);
}

void ConstraintGeneratorFixture::solve(const std::string& code)
{
    generateConstraints(code);
    ConstraintSolver cs{
        NotNull{&normalizer},
        NotNull{simplifier.get()},
        NotNull{&typeFunctionRuntime},
        NotNull{rootScope},
        constraints,
        "MainModule",
        NotNull(&moduleResolver),
        {},
        &logger,
        NotNull{dfg.get()},
        {}
    };

    cs.run();
}

} // namespace Luau
