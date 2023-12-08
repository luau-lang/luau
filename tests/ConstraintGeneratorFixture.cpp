// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "ConstraintGeneratorFixture.h"
#include "ScopedFlags.h"

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);

namespace Luau
{

ConstraintGeneratorFixture::ConstraintGeneratorFixture()
    : Fixture()
    , mainModule(new Module)
    , forceTheFlag{FFlag::DebugLuauDeferredConstraintResolution, true}
{
    mainModule->name = "MainModule";
    mainModule->humanReadableName = "MainModule";

    BlockedTypePack::nextIndex = 0;
}

void ConstraintGeneratorFixture::generateConstraints(const std::string& code)
{
    AstStatBlock* root = parse(code);
    dfg = std::make_unique<DataFlowGraph>(DataFlowGraphBuilder::build(root, NotNull{&ice}));
    cg = std::make_unique<ConstraintGenerator>(mainModule, NotNull{&normalizer}, NotNull(&moduleResolver), builtinTypes, NotNull(&ice),
        frontend.globals.globalScope, /*prepareModuleScope*/ nullptr, &logger, NotNull{dfg.get()}, std::vector<RequireCycle>());
    cg->visitModuleRoot(root);
    rootScope = cg->rootScope;
    constraints = Luau::borrowConstraints(cg->constraints);
}

void ConstraintGeneratorFixture::solve(const std::string& code)
{
    generateConstraints(code);
    ConstraintSolver cs{NotNull{&normalizer}, NotNull{rootScope}, constraints, "MainModule", NotNull(&moduleResolver), {}, &logger, {}};
    cs.run();
}

} // namespace Luau
