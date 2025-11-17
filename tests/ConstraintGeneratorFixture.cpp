// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "ConstraintGeneratorFixture.h"
#include "ScopedFlags.h"

LUAU_FASTFLAG(LuauSolverV2);

namespace Luau
{

ConstraintGeneratorFixture::ConstraintGeneratorFixture()
    : Fixture()
    , mainModule(new Module)
    , simplifier(newSimplifier(NotNull{&arena}, getBuiltins()))
    , forceTheFlag{FFlag::LuauSolverV2, true}
{
    getFrontend(); // Force the frontend to exist in the constructor.
    mainModule->name = "MainModule";
    mainModule->humanReadableName = "MainModule";

    BlockedTypePack::nextIndex = 0;
}

void ConstraintGeneratorFixture::generateConstraints(const std::string& code)
{
    AstStatBlock* root = parse(code);
    dfg = std::make_unique<DataFlowGraph>(
        DataFlowGraphBuilder::build(root, NotNull{&mainModule->defArena}, NotNull{&mainModule->keyArena}, NotNull{&ice})
    );
    cg = std::make_unique<ConstraintGenerator>(
        mainModule,
        NotNull{&normalizer},
        NotNull{simplifier.get()},
        NotNull{&typeFunctionRuntime},
        NotNull(&moduleResolver),
        getBuiltins(),
        NotNull(&ice),
        getFrontend().globals.globalScope,
        getFrontend().globals.globalTypeFunctionScope,
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
        NotNull{&cg->scopeToFunction},
        mainModule,
        NotNull(&moduleResolver),
        {},
        &logger,
        NotNull{dfg.get()},
        {}
    };

    cs.run();
}

} // namespace Luau
