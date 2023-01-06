// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "ConstraintGraphBuilderFixture.h"

#include "Luau/TypeReduction.h"

namespace Luau
{

ConstraintGraphBuilderFixture::ConstraintGraphBuilderFixture()
    : Fixture()
    , mainModule(new Module)
    , forceTheFlag{"DebugLuauDeferredConstraintResolution", true}
{
    mainModule->reduction = std::make_unique<TypeReduction>(NotNull{&mainModule->internalTypes}, builtinTypes, NotNull{&ice});

    BlockedType::nextIndex = 0;
    BlockedTypePack::nextIndex = 0;
}

void ConstraintGraphBuilderFixture::generateConstraints(const std::string& code)
{
    AstStatBlock* root = parse(code);
    dfg = std::make_unique<DataFlowGraph>(DataFlowGraphBuilder::build(root, NotNull{&ice}));
    cgb = std::make_unique<ConstraintGraphBuilder>("MainModule", mainModule, &arena, NotNull(&moduleResolver), builtinTypes, NotNull(&ice),
        frontend.getGlobalScope(), &logger, NotNull{dfg.get()});
    cgb->visit(root);
    rootScope = cgb->rootScope;
    constraints = Luau::borrowConstraints(cgb->constraints);
}

void ConstraintGraphBuilderFixture::solve(const std::string& code)
{
    generateConstraints(code);
    ConstraintSolver cs{NotNull{&normalizer}, NotNull{rootScope}, constraints, "MainModule", NotNull(&moduleResolver), {}, &logger};
    cs.run();
}

} // namespace Luau
