// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ConstraintGraphBuilder.h"

#include "Luau/Ast.h"
#include "Luau/Breadcrumb.h"
#include "Luau/Common.h"
#include "Luau/Constraint.h"
#include "Luau/ControlFlow.h"
#include "Luau/DcrLogger.h"
#include "Luau/ModuleResolver.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Refinement.h"
#include "Luau/Scope.h"
#include "Luau/TypeUtils.h"
#include "Luau/Type.h"
#include "Luau/TypeFamily.h"
#include "Luau/Simplify.h"
#include "Luau/VisitType.h"
#include "Luau/InsertionOrderedMap.h"

#include <algorithm>

LUAU_FASTINT(LuauCheckRecursionLimit);
LUAU_FASTFLAG(DebugLuauLogSolverToJson);
LUAU_FASTFLAG(DebugLuauMagicTypes);
LUAU_FASTFLAG(LuauParseDeclareClassIndexer);

namespace Luau
{

bool doesCallError(const AstExprCall* call);        // TypeInfer.cpp
const AstStat* getFallthrough(const AstStat* node); // TypeInfer.cpp

static std::optional<AstExpr*> matchRequire(const AstExprCall& call)
{
    const char* require = "require";

    if (call.args.size != 1)
        return std::nullopt;

    const AstExprGlobal* funcAsGlobal = call.func->as<AstExprGlobal>();
    if (!funcAsGlobal || funcAsGlobal->name != require)
        return std::nullopt;

    if (call.args.size != 1)
        return std::nullopt;

    return call.args.data[0];
}

static bool matchSetmetatable(const AstExprCall& call)
{
    const char* smt = "setmetatable";

    if (call.args.size != 2)
        return false;

    const AstExprGlobal* funcAsGlobal = call.func->as<AstExprGlobal>();
    if (!funcAsGlobal || funcAsGlobal->name != smt)
        return false;

    return true;
}

struct TypeGuard
{
    bool isTypeof;
    AstExpr* target;
    std::string type;
};

static std::optional<TypeGuard> matchTypeGuard(const AstExprBinary* binary)
{
    if (binary->op != AstExprBinary::CompareEq && binary->op != AstExprBinary::CompareNe)
        return std::nullopt;

    AstExpr* left = binary->left;
    AstExpr* right = binary->right;
    if (right->is<AstExprCall>())
        std::swap(left, right);

    if (!right->is<AstExprConstantString>())
        return std::nullopt;

    AstExprCall* call = left->as<AstExprCall>();
    AstExprConstantString* string = right->as<AstExprConstantString>();
    if (!call || !string)
        return std::nullopt;

    AstExprGlobal* callee = call->func->as<AstExprGlobal>();
    if (!callee)
        return std::nullopt;

    if (callee->name != "type" && callee->name != "typeof")
        return std::nullopt;

    if (call->args.size != 1)
        return std::nullopt;

    return TypeGuard{
        /*isTypeof*/ callee->name == "typeof",
        /*target*/ call->args.data[0],
        /*type*/ std::string(string->value.data, string->value.size),
    };
}

static bool matchAssert(const AstExprCall& call)
{
    if (call.args.size < 1)
        return false;

    const AstExprGlobal* funcAsGlobal = call.func->as<AstExprGlobal>();
    if (!funcAsGlobal || funcAsGlobal->name != "assert")
        return false;

    return true;
}

namespace
{

struct Checkpoint
{
    size_t offset;
};

Checkpoint checkpoint(const ConstraintGraphBuilder* cgb)
{
    return Checkpoint{cgb->constraints.size()};
}

template<typename F>
void forEachConstraint(const Checkpoint& start, const Checkpoint& end, const ConstraintGraphBuilder* cgb, F f)
{
    for (size_t i = start.offset; i < end.offset; ++i)
        f(cgb->constraints[i]);
}

} // namespace

ConstraintGraphBuilder::ConstraintGraphBuilder(ModulePtr module, NotNull<Normalizer> normalizer, NotNull<ModuleResolver> moduleResolver,
    NotNull<BuiltinTypes> builtinTypes, NotNull<InternalErrorReporter> ice, const ScopePtr& globalScope,
    std::function<void(const ModuleName&, const ScopePtr&)> prepareModuleScope, DcrLogger* logger, NotNull<DataFlowGraph> dfg,
    std::vector<RequireCycle> requireCycles)
    : module(module)
    , builtinTypes(builtinTypes)
    , arena(normalizer->arena)
    , rootScope(nullptr)
    , dfg(dfg)
    , normalizer(normalizer)
    , moduleResolver(moduleResolver)
    , ice(ice)
    , globalScope(globalScope)
    , prepareModuleScope(std::move(prepareModuleScope))
    , requireCycles(std::move(requireCycles))
    , logger(logger)
{
    LUAU_ASSERT(module);
}

TypeId ConstraintGraphBuilder::freshType(const ScopePtr& scope)
{
    return Luau::freshType(arena, builtinTypes, scope.get());
}

TypePackId ConstraintGraphBuilder::freshTypePack(const ScopePtr& scope)
{
    FreeTypePack f{scope.get()};
    return arena->addTypePack(TypePackVar{std::move(f)});
}

ScopePtr ConstraintGraphBuilder::childScope(AstNode* node, const ScopePtr& parent)
{
    auto scope = std::make_shared<Scope>(parent);
    scopes.emplace_back(node->location, scope);

    scope->returnType = parent->returnType;
    scope->varargPack = parent->varargPack;

    parent->children.push_back(NotNull{scope.get()});
    module->astScopes[node] = scope.get();

    return scope;
}

NotNull<Constraint> ConstraintGraphBuilder::addConstraint(const ScopePtr& scope, const Location& location, ConstraintV cv)
{
    return NotNull{constraints.emplace_back(new Constraint{NotNull{scope.get()}, location, std::move(cv)}).get()};
}

NotNull<Constraint> ConstraintGraphBuilder::addConstraint(const ScopePtr& scope, std::unique_ptr<Constraint> c)
{
    return NotNull{constraints.emplace_back(std::move(c)).get()};
}

struct RefinementPartition
{
    // Types that we want to intersect against the type of the expression.
    std::vector<TypeId> discriminantTypes;

    // Sometimes the type we're discriminating against is implicitly nil.
    bool shouldAppendNilType = false;
};

using RefinementContext = InsertionOrderedMap<DefId, RefinementPartition>;

static void unionRefinements(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, const RefinementContext& lhs, const RefinementContext& rhs,
    RefinementContext& dest, std::vector<ConstraintV>* constraints)
{
    const auto intersect = [&](const std::vector<TypeId>& types) {
        if (1 == types.size())
            return types[0];
        else if (2 == types.size())
        {
            // TODO: It may be advantageous to create a RefineConstraint here when there are blockedTypes.
            SimplifyResult sr = simplifyIntersection(builtinTypes, arena, types[0], types[1]);
            if (sr.blockedTypes.empty())
                return sr.result;
        }

        return arena->addType(IntersectionType{types});
    };

    for (auto& [def, partition] : lhs)
    {
        auto rhsIt = rhs.find(def);
        if (rhsIt == rhs.end())
            continue;

        LUAU_ASSERT(!partition.discriminantTypes.empty());
        LUAU_ASSERT(!rhsIt->second.discriminantTypes.empty());

        TypeId leftDiscriminantTy = partition.discriminantTypes.size() == 1 ? partition.discriminantTypes[0] : intersect(partition.discriminantTypes);

        TypeId rightDiscriminantTy =
            rhsIt->second.discriminantTypes.size() == 1 ? rhsIt->second.discriminantTypes[0] : intersect(rhsIt->second.discriminantTypes);

        dest.insert(def, {});
        dest.get(def)->discriminantTypes.push_back(simplifyUnion(builtinTypes, arena, leftDiscriminantTy, rightDiscriminantTy).result);
        dest.get(def)->shouldAppendNilType |= partition.shouldAppendNilType || rhsIt->second.shouldAppendNilType;
    }
}

static void computeRefinement(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, const ScopePtr& scope, RefinementId refinement,
    RefinementContext* refis, bool sense, bool eq, std::vector<ConstraintV>* constraints)
{
    if (!refinement)
        return;
    else if (auto variadic = get<Variadic>(refinement))
    {
        for (RefinementId refi : variadic->refinements)
            computeRefinement(builtinTypes, arena, scope, refi, refis, sense, eq, constraints);
    }
    else if (auto negation = get<Negation>(refinement))
        return computeRefinement(builtinTypes, arena, scope, negation->refinement, refis, !sense, eq, constraints);
    else if (auto conjunction = get<Conjunction>(refinement))
    {
        RefinementContext lhsRefis;
        RefinementContext rhsRefis;

        computeRefinement(builtinTypes, arena, scope, conjunction->lhs, sense ? refis : &lhsRefis, sense, eq, constraints);
        computeRefinement(builtinTypes, arena, scope, conjunction->rhs, sense ? refis : &rhsRefis, sense, eq, constraints);

        if (!sense)
            unionRefinements(builtinTypes, arena, lhsRefis, rhsRefis, *refis, constraints);
    }
    else if (auto disjunction = get<Disjunction>(refinement))
    {
        RefinementContext lhsRefis;
        RefinementContext rhsRefis;

        computeRefinement(builtinTypes, arena, scope, disjunction->lhs, sense ? &lhsRefis : refis, sense, eq, constraints);
        computeRefinement(builtinTypes, arena, scope, disjunction->rhs, sense ? &rhsRefis : refis, sense, eq, constraints);

        if (sense)
            unionRefinements(builtinTypes, arena, lhsRefis, rhsRefis, *refis, constraints);
    }
    else if (auto equivalence = get<Equivalence>(refinement))
    {
        computeRefinement(builtinTypes, arena, scope, equivalence->lhs, refis, sense, true, constraints);
        computeRefinement(builtinTypes, arena, scope, equivalence->rhs, refis, sense, true, constraints);
    }
    else if (auto proposition = get<Proposition>(refinement))
    {
        TypeId discriminantTy = proposition->discriminantTy;
        if (!sense && !eq)
            discriminantTy = arena->addType(NegationType{proposition->discriminantTy});
        else if (eq)
        {
            discriminantTy = arena->addType(BlockedType{});
            constraints->push_back(SingletonOrTopTypeConstraint{discriminantTy, proposition->discriminantTy, !sense});
        }

        RefinementContext uncommittedRefis;
        uncommittedRefis.insert(proposition->breadcrumb->def, {});
        uncommittedRefis.get(proposition->breadcrumb->def)->discriminantTypes.push_back(discriminantTy);

        // When the top-level expression is `t[x]`, we want to refine it into `nil`, not `never`.
        if ((sense || !eq) && getMetadata<SubscriptMetadata>(proposition->breadcrumb))
            uncommittedRefis.get(proposition->breadcrumb->def)->shouldAppendNilType = true;

        for (NullableBreadcrumbId current = proposition->breadcrumb; current && current->previous; current = current->previous)
        {
            LUAU_ASSERT(get<Cell>(current->def));

            // If this current breadcrumb has no metadata, it's no-op for the purpose of building a discriminant type.
            if (!current->metadata)
                continue;
            else if (auto field = getMetadata<FieldMetadata>(current))
            {
                TableType::Props props{{field->prop, Property{discriminantTy}}};
                discriminantTy = arena->addType(TableType{std::move(props), std::nullopt, TypeLevel{}, scope.get(), TableState::Sealed});
                uncommittedRefis.insert(current->previous->def, {});
                uncommittedRefis.get(current->previous->def)->discriminantTypes.push_back(discriminantTy);
            }
        }

        // And now it's time to commit it.
        for (auto& [def, partition] : uncommittedRefis)
        {
            (*refis).insert(def, {});

            for (TypeId discriminantTy : partition.discriminantTypes)
                (*refis).get(def)->discriminantTypes.push_back(discriminantTy);

            (*refis).get(def)->shouldAppendNilType |= partition.shouldAppendNilType;
        }
    }
}

namespace
{

/*
 * Constraint generation may be called upon to simplify an intersection or union
 * of types that are not sufficiently solved yet.  We use
 * FindSimplificationBlockers to recognize these types and defer the
 * simplification until constraint solution.
 */
struct FindSimplificationBlockers : TypeOnceVisitor
{
    bool found = false;

    bool visit(TypeId) override
    {
        return !found;
    }

    bool visit(TypeId, const BlockedType&) override
    {
        found = true;
        return false;
    }

    bool visit(TypeId, const FreeType&) override
    {
        found = true;
        return false;
    }

    bool visit(TypeId, const PendingExpansionType&) override
    {
        found = true;
        return false;
    }

    // We do not need to know anything at all about a function's argument or
    // return types in order to simplify it in an intersection or union.
    bool visit(TypeId, const FunctionType&) override
    {
        return false;
    }

    bool visit(TypeId, const ClassType&) override
    {
        return false;
    }
};

bool mustDeferIntersection(TypeId ty)
{
    FindSimplificationBlockers bts;
    bts.traverse(ty);
    return bts.found;
}
} // namespace

void ConstraintGraphBuilder::applyRefinements(const ScopePtr& scope, Location location, RefinementId refinement)
{
    if (!refinement)
        return;

    RefinementContext refinements;
    std::vector<ConstraintV> constraints;
    computeRefinement(builtinTypes, arena, scope, refinement, &refinements, /*sense*/ true, /*eq*/ false, &constraints);

    for (auto& [def, partition] : refinements)
    {
        if (std::optional<TypeId> defTy = scope->lookup(def))
        {
            TypeId ty = *defTy;
            if (partition.shouldAppendNilType)
                ty = arena->addType(UnionType{{ty, builtinTypes->nilType}});

            // Intersect ty with every discriminant type. If either type is not
            // sufficiently solved, we queue the intersection up via an
            // IntersectConstraint.

            for (TypeId dt : partition.discriminantTypes)
            {
                if (mustDeferIntersection(ty) || mustDeferIntersection(dt))
                {
                    TypeId r = arena->addType(BlockedType{});
                    addConstraint(scope, location, RefineConstraint{RefineConstraint::Intersection, r, ty, dt});

                    ty = r;
                }
                else
                {
                    switch (shouldSuppressErrors(normalizer, ty))
                    {
                        case ErrorSuppression::DoNotSuppress:
                            ty = simplifyIntersection(builtinTypes, arena, ty, dt).result;
                            break;
                        case ErrorSuppression::Suppress:
                            ty = simplifyIntersection(builtinTypes, arena, ty, dt).result;
                            ty = simplifyUnion(builtinTypes, arena, ty, builtinTypes->errorType).result;
                            break;
                        case ErrorSuppression::NormalizationFailed:
                            reportError(location, NormalizationTooComplex{});
                            ty = simplifyIntersection(builtinTypes, arena, ty, dt).result;
                            break;
                    }
                }
            }

            scope->dcrRefinements[def] = ty;
        }
    }

    for (auto& c : constraints)
        addConstraint(scope, location, c);
}

void ConstraintGraphBuilder::visit(AstStatBlock* block)
{
    LUAU_ASSERT(scopes.empty());
    LUAU_ASSERT(rootScope == nullptr);
    ScopePtr scope = std::make_shared<Scope>(globalScope);
    rootScope = scope.get();
    scopes.emplace_back(block->location, scope);
    module->astScopes[block] = NotNull{scope.get()};

    rootScope->returnType = freshTypePack(scope);

    prepopulateGlobalScope(scope, block);

    visitBlockWithoutChildScope(scope, block);

    if (logger)
        logger->captureGenerationModule(module);
}

ControlFlow ConstraintGraphBuilder::visitBlockWithoutChildScope(const ScopePtr& scope, AstStatBlock* block)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportCodeTooComplex(block->location);
        return ControlFlow::None;
    }

    std::unordered_map<Name, Location> aliasDefinitionLocations;

    // In order to enable mutually-recursive type aliases, we need to
    // populate the type bindings before we actually check any of the
    // alias statements.
    for (AstStat* stat : block->body)
    {
        if (auto alias = stat->as<AstStatTypeAlias>())
        {
            if (scope->exportedTypeBindings.count(alias->name.value) || scope->privateTypeBindings.count(alias->name.value))
            {
                auto it = aliasDefinitionLocations.find(alias->name.value);
                LUAU_ASSERT(it != aliasDefinitionLocations.end());
                reportError(alias->location, DuplicateTypeDefinition{alias->name.value, it->second});
                continue;
            }

            ScopePtr defnScope = childScope(alias, scope);

            TypeId initialType = arena->addType(BlockedType{});
            TypeFun initialFun{initialType};

            for (const auto& [name, gen] : createGenerics(defnScope, alias->generics, /* useCache */ true))
            {
                initialFun.typeParams.push_back(gen);
            }

            for (const auto& [name, genPack] : createGenericPacks(defnScope, alias->genericPacks, /* useCache */ true))
            {
                initialFun.typePackParams.push_back(genPack);
            }

            if (alias->exported)
                scope->exportedTypeBindings[alias->name.value] = std::move(initialFun);
            else
                scope->privateTypeBindings[alias->name.value] = std::move(initialFun);

            astTypeAliasDefiningScopes[alias] = defnScope;
            aliasDefinitionLocations[alias->name.value] = alias->location;
        }
    }

    std::optional<ControlFlow> firstControlFlow;
    for (AstStat* stat : block->body)
    {
        ControlFlow cf = visit(scope, stat);
        if (cf != ControlFlow::None && !firstControlFlow)
            firstControlFlow = cf;
    }

    return firstControlFlow.value_or(ControlFlow::None);
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStat* stat)
{
    RecursionLimiter limiter{&recursionCount, FInt::LuauCheckRecursionLimit};

    if (auto s = stat->as<AstStatBlock>())
        return visit(scope, s);
    else if (auto i = stat->as<AstStatIf>())
        return visit(scope, i);
    else if (auto s = stat->as<AstStatWhile>())
        return visit(scope, s);
    else if (auto s = stat->as<AstStatRepeat>())
        return visit(scope, s);
    else if (stat->is<AstStatBreak>() || stat->is<AstStatContinue>())
    {
        // Nothing
        return ControlFlow::None; // TODO: ControlFlow::Break/Continue
    }
    else if (auto r = stat->as<AstStatReturn>())
        return visit(scope, r);
    else if (auto e = stat->as<AstStatExpr>())
    {
        checkPack(scope, e->expr);

        if (auto call = e->expr->as<AstExprCall>(); call && doesCallError(call))
            return ControlFlow::Throws;

        return ControlFlow::None;
    }
    else if (auto s = stat->as<AstStatLocal>())
        return visit(scope, s);
    else if (auto s = stat->as<AstStatFor>())
        return visit(scope, s);
    else if (auto s = stat->as<AstStatForIn>())
        return visit(scope, s);
    else if (auto a = stat->as<AstStatAssign>())
        return visit(scope, a);
    else if (auto a = stat->as<AstStatCompoundAssign>())
        return visit(scope, a);
    else if (auto f = stat->as<AstStatFunction>())
        return visit(scope, f);
    else if (auto f = stat->as<AstStatLocalFunction>())
        return visit(scope, f);
    else if (auto a = stat->as<AstStatTypeAlias>())
        return visit(scope, a);
    else if (auto s = stat->as<AstStatDeclareGlobal>())
        return visit(scope, s);
    else if (auto s = stat->as<AstStatDeclareFunction>())
        return visit(scope, s);
    else if (auto s = stat->as<AstStatDeclareClass>())
        return visit(scope, s);
    else if (auto s = stat->as<AstStatError>())
        return visit(scope, s);
    else
    {
        LUAU_ASSERT(0 && "Internal error: Unknown AstStat type");
        return ControlFlow::None;
    }
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatLocal* local)
{
    std::vector<TypeId> varTypes;
    varTypes.reserve(local->vars.size);

    // Used to name the first value type, even if it's not placed in varTypes,
    // for the purpose of synthetic name attribution.
    std::optional<TypeId> firstValueType;

    for (AstLocal* local : local->vars)
    {
        TypeId ty = nullptr;

        if (local->annotation)
            ty = resolveType(scope, local->annotation, /* inTypeArguments */ false);

        varTypes.push_back(ty);
    }

    for (size_t i = 0; i < local->values.size; ++i)
    {
        AstExpr* value = local->values.data[i];
        const bool hasAnnotation = i < local->vars.size && nullptr != local->vars.data[i]->annotation;

        if (value->is<AstExprConstantNil>())
        {
            // HACK: we leave nil-initialized things floating under the
            // assumption that they will later be populated.
            //
            // See the test TypeInfer/infer_locals_with_nil_value. Better flow
            // awareness should make this obsolete.

            if (!varTypes[i])
                varTypes[i] = freshType(scope);
        }
        // Only function calls and vararg expressions can produce packs.  All
        // other expressions produce exactly one value.
        else if (i != local->values.size - 1 || (!value->is<AstExprCall>() && !value->is<AstExprVarargs>()))
        {
            std::optional<TypeId> expectedType;
            if (hasAnnotation)
                expectedType = varTypes.at(i);

            TypeId exprType = check(scope, value, ValueContext::RValue, expectedType).ty;
            if (i < varTypes.size())
            {
                if (varTypes[i])
                    addConstraint(scope, local->location, SubtypeConstraint{exprType, varTypes[i]});
                else
                    varTypes[i] = exprType;
            }

            if (i == 0)
                firstValueType = exprType;
        }
        else
        {
            std::vector<std::optional<TypeId>> expectedTypes;
            if (hasAnnotation)
                expectedTypes.insert(begin(expectedTypes), begin(varTypes) + i, end(varTypes));

            TypePackId exprPack = checkPack(scope, value, expectedTypes).tp;

            if (i < local->vars.size)
            {
                TypePack packTypes = extendTypePack(*arena, builtinTypes, exprPack, varTypes.size() - i);

                // fill out missing values in varTypes with values from exprPack
                for (size_t j = i; j < varTypes.size(); ++j)
                {
                    if (!varTypes[j])
                    {
                        if (j - i < packTypes.head.size())
                            varTypes[j] = packTypes.head[j - i];
                        else
                            varTypes[j] = arena->addType(BlockedType{});
                    }
                }

                std::vector<TypeId> tailValues{varTypes.begin() + i, varTypes.end()};
                TypePackId tailPack = arena->addTypePack(std::move(tailValues));
                addConstraint(scope, local->location, UnpackConstraint{tailPack, exprPack});
            }
        }
    }

    if (local->vars.size == 1 && local->values.size == 1 && firstValueType && scope.get() == rootScope)
    {
        AstLocal* var = local->vars.data[0];
        AstExpr* value = local->values.data[0];

        if (value->is<AstExprTable>())
            addConstraint(scope, value->location, NameConstraint{*firstValueType, var->name.value, /*synthetic*/ true});
        else if (const AstExprCall* call = value->as<AstExprCall>())
        {
            if (const AstExprGlobal* global = call->func->as<AstExprGlobal>(); global && global->name == "setmetatable")
            {
                addConstraint(scope, value->location, NameConstraint{*firstValueType, var->name.value, /*synthetic*/ true});
            }
        }
    }

    for (size_t i = 0; i < local->vars.size; ++i)
    {
        AstLocal* l = local->vars.data[i];
        Location location = l->location;

        if (!varTypes[i])
            varTypes[i] = freshType(scope);

        scope->bindings[l] = Binding{varTypes[i], location};

        // HACK: In the greedy solver, we say the type state of a variable is the type annotation itself, but
        // the actual type state is the corresponding initializer expression (if it exists) or nil otherwise.
        BreadcrumbId bc = dfg->getBreadcrumb(l);
        scope->dcrRefinements[bc->def] = varTypes[i];
    }

    if (local->values.size > 0)
    {
        // To correctly handle 'require', we need to import the exported type bindings into the variable 'namespace'.
        for (size_t i = 0; i < local->values.size && i < local->vars.size; ++i)
        {
            const AstExprCall* call = local->values.data[i]->as<AstExprCall>();
            if (!call)
                continue;

            if (auto maybeRequire = matchRequire(*call))
            {
                AstExpr* require = *maybeRequire;

                if (auto moduleInfo = moduleResolver->resolveModuleInfo(module->name, *require))
                {
                    const Name name{local->vars.data[i]->name.value};

                    if (ModulePtr module = moduleResolver->getModule(moduleInfo->name))
                    {
                        scope->importedTypeBindings[name] = module->exportedTypeBindings;
                        scope->importedModules[name] = moduleInfo->name;

                        // Imported types of requires that transitively refer to current module have to be replaced with 'any'
                        for (const auto& [location, path] : requireCycles)
                        {
                            if (!path.empty() && path.front() == moduleInfo->name)
                            {
                                for (auto& [name, tf] : scope->importedTypeBindings[name])
                                    tf = TypeFun{{}, {}, builtinTypes->anyType};
                            }
                        }
                    }
                }
            }
        }
    }

    return ControlFlow::None;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatFor* for_)
{
    TypeId annotationTy = builtinTypes->numberType;
    if (for_->var->annotation)
        annotationTy = resolveType(scope, for_->var->annotation, /* inTypeArguments */ false);

    auto inferNumber = [&](AstExpr* expr) {
        if (!expr)
            return;

        TypeId t = check(scope, expr).ty;
        addConstraint(scope, expr->location, SubtypeConstraint{t, builtinTypes->numberType});
    };

    inferNumber(for_->from);
    inferNumber(for_->to);
    inferNumber(for_->step);

    ScopePtr forScope = childScope(for_, scope);
    forScope->bindings[for_->var] = Binding{annotationTy, for_->var->location};

    BreadcrumbId bc = dfg->getBreadcrumb(for_->var);
    forScope->dcrRefinements[bc->def] = annotationTy;

    visit(forScope, for_->body);

    return ControlFlow::None;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatForIn* forIn)
{
    ScopePtr loopScope = childScope(forIn, scope);

    TypePackId iterator = checkPack(scope, forIn->values).tp;

    std::vector<TypeId> variableTypes;
    variableTypes.reserve(forIn->vars.size);
    for (AstLocal* var : forIn->vars)
    {
        TypeId ty = nullptr;
        if (var->annotation)
            ty = resolveType(loopScope, var->annotation, /*inTypeArguments*/ false);
        else
            ty = freshType(loopScope);

        loopScope->bindings[var] = Binding{ty, var->location};
        variableTypes.push_back(ty);

        BreadcrumbId bc = dfg->getBreadcrumb(var);
        loopScope->dcrRefinements[bc->def] = ty;
    }

    // It is always ok to provide too few variables, so we give this pack a free tail.
    TypePackId variablePack = arena->addTypePack(std::move(variableTypes), freshTypePack(loopScope));

    addConstraint(
        loopScope, getLocation(forIn->values), IterableConstraint{iterator, variablePack, forIn->values.data[0], &module->astForInNextTypes});
    visit(loopScope, forIn->body);

    return ControlFlow::None;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatWhile* while_)
{
    RefinementId refinement = check(scope, while_->condition).refinement;

    ScopePtr whileScope = childScope(while_, scope);
    applyRefinements(whileScope, while_->condition->location, refinement);

    visit(whileScope, while_->body);

    return ControlFlow::None;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatRepeat* repeat)
{
    ScopePtr repeatScope = childScope(repeat, scope);

    visitBlockWithoutChildScope(repeatScope, repeat->body);

    check(repeatScope, repeat->condition);

    return ControlFlow::None;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatLocalFunction* function)
{
    // Local
    // Global
    // Dotted path
    // Self?

    TypeId functionType = nullptr;
    auto ty = scope->lookup(function->name);
    LUAU_ASSERT(!ty.has_value()); // The parser ensures that every local function has a distinct Symbol for its name.

    functionType = arena->addType(BlockedType{});
    scope->bindings[function->name] = Binding{functionType, function->name->location};

    FunctionSignature sig = checkFunctionSignature(scope, function->func, /* expectedType */ std::nullopt, function->name->location);
    sig.bodyScope->bindings[function->name] = Binding{sig.signature, function->func->location};

    BreadcrumbId bc = dfg->getBreadcrumb(function->name);
    scope->dcrRefinements[bc->def] = functionType;
    sig.bodyScope->dcrRefinements[bc->def] = sig.signature;

    Checkpoint start = checkpoint(this);
    checkFunctionBody(sig.bodyScope, function->func);
    Checkpoint end = checkpoint(this);

    NotNull<Scope> constraintScope{sig.signatureScope ? sig.signatureScope.get() : sig.bodyScope.get()};
    std::unique_ptr<Constraint> c =
        std::make_unique<Constraint>(constraintScope, function->name->location, GeneralizationConstraint{functionType, sig.signature});

    Constraint* previous = nullptr;
    forEachConstraint(start, end, this, [&c, &previous](const ConstraintPtr& constraint) {
        c->dependencies.push_back(NotNull{constraint.get()});

        if (auto psc = get<PackSubtypeConstraint>(*constraint); psc && psc->returns)
        {
            if (previous)
                constraint->dependencies.push_back(NotNull{previous});

            previous = constraint.get();
        }
    });

    addConstraint(scope, std::move(c));
    module->astTypes[function->func] = functionType;

    return ControlFlow::None;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatFunction* function)
{
    // Name could be AstStatLocal, AstStatGlobal, AstStatIndexName.
    // With or without self

    TypeId generalizedType = arena->addType(BlockedType{});

    Checkpoint start = checkpoint(this);
    FunctionSignature sig = checkFunctionSignature(scope, function->func, /* expectedType */ std::nullopt, function->name->location);

    std::unordered_set<Constraint*> excludeList;

    const NullableBreadcrumbId functionBreadcrumb = dfg->getBreadcrumb(function->name);

    if (AstExprLocal* localName = function->name->as<AstExprLocal>())
    {
        std::optional<TypeId> existingFunctionTy = scope->lookup(localName->local);
        if (existingFunctionTy)
        {
            addConstraint(scope, function->name->location, SubtypeConstraint{generalizedType, *existingFunctionTy});

            Symbol sym{localName->local};
            scope->bindings[sym].typeId = generalizedType;
        }
        else
            scope->bindings[localName->local] = Binding{generalizedType, localName->location};

        sig.bodyScope->bindings[localName->local] = Binding{sig.signature, localName->location};

        if (functionBreadcrumb)
            sig.bodyScope->dcrRefinements[functionBreadcrumb->def] = sig.signature;
    }
    else if (AstExprGlobal* globalName = function->name->as<AstExprGlobal>())
    {
        std::optional<TypeId> existingFunctionTy = scope->lookup(globalName->name);
        if (!existingFunctionTy)
            ice->ice("prepopulateGlobalScope did not populate a global name", globalName->location);

        generalizedType = *existingFunctionTy;

        sig.bodyScope->bindings[globalName->name] = Binding{sig.signature, globalName->location};

        if (functionBreadcrumb)
            sig.bodyScope->dcrRefinements[functionBreadcrumb->def] = sig.signature;
    }
    else if (AstExprIndexName* indexName = function->name->as<AstExprIndexName>())
    {
        Checkpoint check1 = checkpoint(this);
        TypeId lvalueType = checkLValue(scope, indexName);
        Checkpoint check2 = checkpoint(this);

        forEachConstraint(check1, check2, this, [&excludeList](const ConstraintPtr& c) {
            excludeList.insert(c.get());
        });

        // TODO figure out how to populate the location field of the table Property.

        if (get<FreeType>(lvalueType))
            asMutable(lvalueType)->ty.emplace<BoundType>(generalizedType);
        else
            addConstraint(scope, indexName->location, SubtypeConstraint{lvalueType, generalizedType});
    }
    else if (AstExprError* err = function->name->as<AstExprError>())
    {
        generalizedType = builtinTypes->errorRecoveryType();
    }

    if (generalizedType == nullptr)
        ice->ice("generalizedType == nullptr", function->location);

    if (functionBreadcrumb)
        scope->dcrRefinements[functionBreadcrumb->def] = generalizedType;

    checkFunctionBody(sig.bodyScope, function->func);
    Checkpoint end = checkpoint(this);

    NotNull<Scope> constraintScope{sig.signatureScope ? sig.signatureScope.get() : sig.bodyScope.get()};
    std::unique_ptr<Constraint> c =
        std::make_unique<Constraint>(constraintScope, function->name->location, GeneralizationConstraint{generalizedType, sig.signature});

    Constraint* previous = nullptr;
    forEachConstraint(start, end, this, [&c, &excludeList, &previous](const ConstraintPtr& constraint) {
        if (!excludeList.count(constraint.get()))
            c->dependencies.push_back(NotNull{constraint.get()});

        if (auto psc = get<PackSubtypeConstraint>(*constraint); psc && psc->returns)
        {
            if (previous)
                constraint->dependencies.push_back(NotNull{previous});

            previous = constraint.get();
        }
    });

    addConstraint(scope, std::move(c));

    return ControlFlow::None;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatReturn* ret)
{
    // At this point, the only way scope->returnType should have anything
    // interesting in it is if the function has an explicit return annotation.
    // If this is the case, then we can expect that the return expression
    // conforms to that.
    std::vector<std::optional<TypeId>> expectedTypes;
    for (TypeId ty : scope->returnType)
        expectedTypes.push_back(ty);

    TypePackId exprTypes = checkPack(scope, ret->list, expectedTypes).tp;
    addConstraint(scope, ret->location, PackSubtypeConstraint{exprTypes, scope->returnType, /*returns*/ true});

    return ControlFlow::Returns;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatBlock* block)
{
    ScopePtr innerScope = childScope(block, scope);

    ControlFlow flow = visitBlockWithoutChildScope(innerScope, block);
    scope->inheritRefinements(innerScope);

    return flow;
}

// TODO Clip?
static void bindFreeType(TypeId a, TypeId b)
{
    FreeType* af = getMutable<FreeType>(a);
    FreeType* bf = getMutable<FreeType>(b);

    LUAU_ASSERT(af || bf);

    if (!bf)
        asMutable(a)->ty.emplace<BoundType>(b);
    else if (!af)
        asMutable(b)->ty.emplace<BoundType>(a);
    else if (subsumes(bf->scope, af->scope))
        asMutable(a)->ty.emplace<BoundType>(b);
    else if (subsumes(af->scope, bf->scope))
        asMutable(b)->ty.emplace<BoundType>(a);
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatAssign* assign)
{
    std::vector<TypeId> varTypes = checkLValues(scope, assign->vars);

    std::vector<std::optional<TypeId>> expectedTypes;
    expectedTypes.reserve(varTypes.size());

    for (TypeId ty : varTypes)
    {
        ty = follow(ty);
        if (get<FreeType>(ty))
            expectedTypes.push_back(std::nullopt);
        else
            expectedTypes.push_back(ty);
    }

    TypePackId exprPack = checkPack(scope, assign->values, expectedTypes).tp;
    TypePackId varPack = arena->addTypePack({varTypes});

    addConstraint(scope, assign->location, PackSubtypeConstraint{exprPack, varPack});

    return ControlFlow::None;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatCompoundAssign* assign)
{
    // We need to tweak the BinaryConstraint that we emit, so we cannot use the
    // strategy of falsifying an AST fragment.
    TypeId varTy = checkLValue(scope, assign->var);
    TypeId valueTy = check(scope, assign->value).ty;

    TypeId resultType = arena->addType(BlockedType{});
    addConstraint(scope, assign->location,
        BinaryConstraint{assign->op, varTy, valueTy, resultType, assign, &module->astOriginalCallTypes, &module->astOverloadResolvedTypes});
    addConstraint(scope, assign->location, SubtypeConstraint{resultType, varTy});

    return ControlFlow::None;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatIf* ifStatement)
{
    RefinementId refinement = check(scope, ifStatement->condition, ValueContext::RValue, std::nullopt).refinement;

    ScopePtr thenScope = childScope(ifStatement->thenbody, scope);
    applyRefinements(thenScope, ifStatement->condition->location, refinement);

    ScopePtr elseScope = childScope(ifStatement->elsebody ? ifStatement->elsebody : ifStatement, scope);
    applyRefinements(elseScope, ifStatement->elseLocation.value_or(ifStatement->condition->location), refinementArena.negation(refinement));

    ControlFlow thencf = visit(thenScope, ifStatement->thenbody);
    ControlFlow elsecf = ControlFlow::None;
    if (ifStatement->elsebody)
        elsecf = visit(elseScope, ifStatement->elsebody);

    if (matches(thencf, ControlFlow::Returns | ControlFlow::Throws) && elsecf == ControlFlow::None)
        scope->inheritRefinements(elseScope);
    else if (thencf == ControlFlow::None && matches(elsecf, ControlFlow::Returns | ControlFlow::Throws))
        scope->inheritRefinements(thenScope);

    if (matches(thencf, ControlFlow::Returns | ControlFlow::Throws) && matches(elsecf, ControlFlow::Returns | ControlFlow::Throws))
        return ControlFlow::Returns;
    else
        return ControlFlow::None;
}

static bool occursCheck(TypeId needle, TypeId haystack)
{
    LUAU_ASSERT(get<BlockedType>(needle));
    haystack = follow(haystack);

    auto checkHaystack = [needle](TypeId haystack) {
        return occursCheck(needle, haystack);
    };

    if (needle == haystack)
        return true;
    else if (auto ut = get<UnionType>(haystack))
        return std::any_of(begin(ut), end(ut), checkHaystack);
    else if (auto it = get<IntersectionType>(haystack))
        return std::any_of(begin(it), end(it), checkHaystack);

    return false;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatTypeAlias* alias)
{
    ScopePtr* defnScope = astTypeAliasDefiningScopes.find(alias);

    std::unordered_map<Name, TypeFun>* typeBindings;
    if (alias->exported)
        typeBindings = &scope->exportedTypeBindings;
    else
        typeBindings = &scope->privateTypeBindings;

    // These will be undefined if the alias was a duplicate definition, in which
    // case we just skip over it.
    auto bindingIt = typeBindings->find(alias->name.value);
    if (bindingIt == typeBindings->end() || defnScope == nullptr)
        return ControlFlow::None;

    TypeId ty = resolveType(*defnScope, alias->type, /* inTypeArguments */ false);

    TypeId aliasTy = bindingIt->second.type;
    LUAU_ASSERT(get<BlockedType>(aliasTy));

    if (occursCheck(aliasTy, ty))
    {
        asMutable(aliasTy)->ty.emplace<BoundType>(builtinTypes->anyType);
        reportError(alias->nameLocation, OccursCheckFailed{});
    }
    else
        asMutable(aliasTy)->ty.emplace<BoundType>(ty);

    std::vector<TypeId> typeParams;
    for (auto tyParam : createGenerics(*defnScope, alias->generics, /* useCache */ true, /* addTypes */ false))
        typeParams.push_back(tyParam.second.ty);

    std::vector<TypePackId> typePackParams;
    for (auto tpParam : createGenericPacks(*defnScope, alias->genericPacks, /* useCache */ true, /* addTypes */ false))
        typePackParams.push_back(tpParam.second.tp);

    addConstraint(scope, alias->type->location,
        NameConstraint{
            ty,
            alias->name.value,
            /*synthetic=*/false,
            std::move(typeParams),
            std::move(typePackParams),
        });

    return ControlFlow::None;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatDeclareGlobal* global)
{
    LUAU_ASSERT(global->type);

    TypeId globalTy = resolveType(scope, global->type, /* inTypeArguments */ false);
    Name globalName(global->name.value);

    module->declaredGlobals[globalName] = globalTy;
    rootScope->bindings[global->name] = Binding{globalTy, global->location};

    BreadcrumbId bc = dfg->getBreadcrumb(global);
    rootScope->dcrRefinements[bc->def] = globalTy;

    return ControlFlow::None;
}

static bool isMetamethod(const Name& name)
{
    return name == "__index" || name == "__newindex" || name == "__call" || name == "__concat" || name == "__unm" || name == "__add" ||
           name == "__sub" || name == "__mul" || name == "__div" || name == "__mod" || name == "__pow" || name == "__tostring" ||
           name == "__metatable" || name == "__eq" || name == "__lt" || name == "__le" || name == "__mode" || name == "__iter" || name == "__len";
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatDeclareClass* declaredClass)
{
    std::optional<TypeId> superTy = std::make_optional(builtinTypes->classType);
    if (declaredClass->superName)
    {
        Name superName = Name(declaredClass->superName->value);
        std::optional<TypeFun> lookupType = scope->lookupType(superName);

        if (!lookupType)
        {
            reportError(declaredClass->location, UnknownSymbol{superName, UnknownSymbol::Type});
            return ControlFlow::None;
        }

        // We don't have generic classes, so this assertion _should_ never be hit.
        LUAU_ASSERT(lookupType->typeParams.size() == 0 && lookupType->typePackParams.size() == 0);
        superTy = lookupType->type;

        if (!get<ClassType>(follow(*superTy)))
        {
            reportError(declaredClass->location,
                GenericError{format("Cannot use non-class type '%s' as a superclass of class '%s'", superName.c_str(), declaredClass->name.value)});

            return ControlFlow::None;
        }
    }

    Name className(declaredClass->name.value);

    TypeId classTy = arena->addType(ClassType(className, {}, superTy, std::nullopt, {}, {}, module->name));
    ClassType* ctv = getMutable<ClassType>(classTy);

    TypeId metaTy = arena->addType(TableType{TableState::Sealed, scope->level, scope.get()});
    TableType* metatable = getMutable<TableType>(metaTy);

    ctv->metatable = metaTy;

    scope->exportedTypeBindings[className] = TypeFun{{}, classTy};

    if (FFlag::LuauParseDeclareClassIndexer && declaredClass->indexer)
    {
        RecursionCounter counter{&recursionCount};

        if (recursionCount >= FInt::LuauCheckRecursionLimit)
        {
            reportCodeTooComplex(declaredClass->indexer->location);
        }
        else
        {
            ctv->indexer = TableIndexer{
                resolveType(scope, declaredClass->indexer->indexType, /* inTypeArguments */ false),
                resolveType(scope, declaredClass->indexer->resultType, /* inTypeArguments */ false),
            };
        }
    }

    for (const AstDeclaredClassProp& prop : declaredClass->props)
    {
        Name propName(prop.name.value);
        TypeId propTy = resolveType(scope, prop.ty, /* inTypeArguments */ false);

        bool assignToMetatable = isMetamethod(propName);

        // Function types always take 'self', but this isn't reflected in the
        // parsed annotation. Add it here.
        if (prop.isMethod)
        {
            if (FunctionType* ftv = getMutable<FunctionType>(propTy))
            {
                ftv->argNames.insert(ftv->argNames.begin(), FunctionArgument{"self", {}});
                ftv->argTypes = arena->addTypePack(TypePack{{classTy}, ftv->argTypes});

                ftv->hasSelf = true;
            }
        }

        if (ctv->props.count(propName) == 0)
        {
            if (assignToMetatable)
                metatable->props[propName] = {propTy};
            else
                ctv->props[propName] = {propTy};
        }
        else
        {
            TypeId currentTy = assignToMetatable ? metatable->props[propName].type() : ctv->props[propName].type();

            // We special-case this logic to keep the intersection flat; otherwise we
            // would create a ton of nested intersection types.
            if (const IntersectionType* itv = get<IntersectionType>(currentTy))
            {
                std::vector<TypeId> options = itv->parts;
                options.push_back(propTy);
                TypeId newItv = arena->addType(IntersectionType{std::move(options)});

                if (assignToMetatable)
                    metatable->props[propName] = {newItv};
                else
                    ctv->props[propName] = {newItv};
            }
            else if (get<FunctionType>(currentTy))
            {
                TypeId intersection = arena->addType(IntersectionType{{currentTy, propTy}});

                if (assignToMetatable)
                    metatable->props[propName] = {intersection};
                else
                    ctv->props[propName] = {intersection};
            }
            else
            {
                reportError(declaredClass->location, GenericError{format("Cannot overload non-function class member '%s'", propName.c_str())});
            }
        }
    }

    return ControlFlow::None;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatDeclareFunction* global)
{
    std::vector<std::pair<Name, GenericTypeDefinition>> generics = createGenerics(scope, global->generics);
    std::vector<std::pair<Name, GenericTypePackDefinition>> genericPacks = createGenericPacks(scope, global->genericPacks);

    std::vector<TypeId> genericTys;
    genericTys.reserve(generics.size());
    for (auto& [name, generic] : generics)
    {
        genericTys.push_back(generic.ty);
    }

    std::vector<TypePackId> genericTps;
    genericTps.reserve(genericPacks.size());
    for (auto& [name, generic] : genericPacks)
    {
        genericTps.push_back(generic.tp);
    }

    ScopePtr funScope = scope;
    if (!generics.empty() || !genericPacks.empty())
        funScope = childScope(global, scope);

    TypePackId paramPack = resolveTypePack(funScope, global->params, /* inTypeArguments */ false);
    TypePackId retPack = resolveTypePack(funScope, global->retTypes, /* inTypeArguments */ false);
    TypeId fnType = arena->addType(FunctionType{TypeLevel{}, funScope.get(), std::move(genericTys), std::move(genericTps), paramPack, retPack});
    FunctionType* ftv = getMutable<FunctionType>(fnType);

    ftv->argNames.reserve(global->paramNames.size);
    for (const auto& el : global->paramNames)
        ftv->argNames.push_back(FunctionArgument{el.first.value, el.second});

    Name fnName(global->name.value);

    module->declaredGlobals[fnName] = fnType;
    scope->bindings[global->name] = Binding{fnType, global->location};

    BreadcrumbId bc = dfg->getBreadcrumb(global);
    rootScope->dcrRefinements[bc->def] = fnType;

    return ControlFlow::None;
}

ControlFlow ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatError* error)
{
    for (AstStat* stat : error->statements)
        visit(scope, stat);
    for (AstExpr* expr : error->expressions)
        check(scope, expr);

    return ControlFlow::None;
}

InferencePack ConstraintGraphBuilder::checkPack(
    const ScopePtr& scope, AstArray<AstExpr*> exprs, const std::vector<std::optional<TypeId>>& expectedTypes)
{
    std::vector<TypeId> head;
    std::optional<TypePackId> tail;

    for (size_t i = 0; i < exprs.size; ++i)
    {
        AstExpr* expr = exprs.data[i];
        if (i < exprs.size - 1)
        {
            std::optional<TypeId> expectedType;
            if (i < expectedTypes.size())
                expectedType = expectedTypes[i];
            head.push_back(check(scope, expr, ValueContext::RValue, expectedType).ty);
        }
        else
        {
            std::vector<std::optional<TypeId>> expectedTailTypes;
            if (i < expectedTypes.size())
                expectedTailTypes.assign(begin(expectedTypes) + i, end(expectedTypes));
            tail = checkPack(scope, expr, expectedTailTypes).tp;
        }
    }

    if (head.empty() && tail)
        return InferencePack{*tail};
    else
        return InferencePack{arena->addTypePack(TypePack{std::move(head), tail})};
}

InferencePack ConstraintGraphBuilder::checkPack(const ScopePtr& scope, AstExpr* expr, const std::vector<std::optional<TypeId>>& expectedTypes)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportCodeTooComplex(expr->location);
        return InferencePack{builtinTypes->errorRecoveryTypePack()};
    }

    InferencePack result;

    if (AstExprCall* call = expr->as<AstExprCall>())
        result = checkPack(scope, call);
    else if (AstExprVarargs* varargs = expr->as<AstExprVarargs>())
    {
        if (scope->varargPack)
            result = InferencePack{*scope->varargPack};
        else
            result = InferencePack{builtinTypes->errorRecoveryTypePack()};
    }
    else
    {
        std::optional<TypeId> expectedType;
        if (!expectedTypes.empty())
            expectedType = expectedTypes[0];
        TypeId t = check(scope, expr, ValueContext::RValue, expectedType).ty;
        result = InferencePack{arena->addTypePack({t})};
    }

    LUAU_ASSERT(result.tp);
    module->astTypePacks[expr] = result.tp;
    return result;
}

InferencePack ConstraintGraphBuilder::checkPack(const ScopePtr& scope, AstExprCall* call)
{
    std::vector<AstExpr*> exprArgs;

    std::vector<RefinementId> returnRefinements;
    std::vector<std::optional<TypeId>> discriminantTypes;

    if (call->self)
    {
        AstExprIndexName* indexExpr = call->func->as<AstExprIndexName>();
        if (!indexExpr)
            ice->ice("method call expression has no 'self'");

        exprArgs.push_back(indexExpr->expr);

        if (auto bc = dfg->getBreadcrumb(indexExpr->expr))
        {
            TypeId discriminantTy = arena->addType(BlockedType{});
            returnRefinements.push_back(refinementArena.proposition(NotNull{bc}, discriminantTy));
            discriminantTypes.push_back(discriminantTy);
        }
        else
            discriminantTypes.push_back(std::nullopt);
    }

    for (AstExpr* arg : call->args)
    {
        exprArgs.push_back(arg);

        if (auto bc = dfg->getBreadcrumb(arg))
        {
            TypeId discriminantTy = arena->addType(BlockedType{});
            returnRefinements.push_back(refinementArena.proposition(NotNull{bc}, discriminantTy));
            discriminantTypes.push_back(discriminantTy);
        }
        else
            discriminantTypes.push_back(std::nullopt);
    }

    Checkpoint startCheckpoint = checkpoint(this);
    TypeId fnType = check(scope, call->func).ty;
    Checkpoint fnEndCheckpoint = checkpoint(this);

    std::vector<std::optional<TypeId>> expectedTypesForCall = getExpectedCallTypesForFunctionOverloads(fnType);

    module->astOriginalCallTypes[call->func] = fnType;
    module->astOriginalCallTypes[call] = fnType;

    TypePackId expectedArgPack = arena->freshTypePack(scope.get());
    TypePackId expectedRetPack = arena->freshTypePack(scope.get());
    TypeId expectedFunctionType = arena->addType(FunctionType{expectedArgPack, expectedRetPack, std::nullopt, call->self});

    TypeId instantiatedFnType = arena->addType(BlockedType{});
    addConstraint(scope, call->location, InstantiationConstraint{instantiatedFnType, fnType});

    NotNull<Constraint> extractArgsConstraint = addConstraint(scope, call->location, SubtypeConstraint{instantiatedFnType, expectedFunctionType});

    // Fully solve fnType, then extract its argument list as expectedArgPack.
    forEachConstraint(startCheckpoint, fnEndCheckpoint, this, [extractArgsConstraint](const ConstraintPtr& constraint) {
        extractArgsConstraint->dependencies.emplace_back(constraint.get());
    });

    const AstExpr* lastArg = exprArgs.size() ? exprArgs[exprArgs.size() - 1] : nullptr;
    const bool needTail = lastArg && (lastArg->is<AstExprCall>() || lastArg->is<AstExprVarargs>());

    TypePack expectedArgs;

    if (!needTail)
        expectedArgs = extendTypePack(*arena, builtinTypes, expectedArgPack, exprArgs.size(), expectedTypesForCall);
    else
        expectedArgs = extendTypePack(*arena, builtinTypes, expectedArgPack, exprArgs.size() - 1, expectedTypesForCall);

    std::vector<TypeId> args;
    std::optional<TypePackId> argTail;
    std::vector<RefinementId> argumentRefinements;

    Checkpoint argCheckpoint = checkpoint(this);

    for (size_t i = 0; i < exprArgs.size(); ++i)
    {
        AstExpr* arg = exprArgs[i];
        std::optional<TypeId> expectedType;
        if (i < expectedArgs.head.size())
            expectedType = expectedArgs.head[i];

        if (i == 0 && call->self)
        {
            // The self type has already been computed as a side effect of
            // computing fnType.  If computing that did not cause us to exceed a
            // recursion limit, we can fetch it from astTypes rather than
            // recomputing it.
            TypeId* selfTy = module->astTypes.find(exprArgs[0]);
            if (selfTy)
                args.push_back(*selfTy);
            else
                args.push_back(freshType(scope));
        }
        else if (i < exprArgs.size() - 1 || !(arg->is<AstExprCall>() || arg->is<AstExprVarargs>()))
        {
            auto [ty, refinement] = check(scope, arg, ValueContext::RValue, expectedType);
            args.push_back(ty);
            argumentRefinements.push_back(refinement);
        }
        else
        {
            auto [tp, refis] = checkPack(scope, arg, {});
            argTail = tp;
            argumentRefinements.insert(argumentRefinements.end(), refis.begin(), refis.end());
        }
    }

    Checkpoint argEndCheckpoint = checkpoint(this);

    // Do not solve argument constraints until after we have extracted the
    // expected types from the callable.
    forEachConstraint(argCheckpoint, argEndCheckpoint, this, [extractArgsConstraint](const ConstraintPtr& constraint) {
        constraint->dependencies.push_back(extractArgsConstraint);
    });

    if (matchSetmetatable(*call))
    {
        TypePack argTailPack;
        if (argTail && args.size() < 2)
            argTailPack = extendTypePack(*arena, builtinTypes, *argTail, 2 - args.size());

        TypeId target = nullptr;
        TypeId mt = nullptr;

        if (args.size() + argTailPack.head.size() == 2)
        {
            target = args.size() > 0 ? args[0] : argTailPack.head[0];
            mt = args.size() > 1 ? args[1] : argTailPack.head[args.size() == 0 ? 1 : 0];
        }
        else
        {
            std::vector<TypeId> unpackedTypes;
            if (args.size() > 0)
                target = args[0];
            else
            {
                target = arena->addType(BlockedType{});
                unpackedTypes.emplace_back(target);
            }

            mt = arena->addType(BlockedType{});
            unpackedTypes.emplace_back(mt);
            TypePackId mtPack = arena->addTypePack(std::move(unpackedTypes));

            addConstraint(scope, call->location, UnpackConstraint{mtPack, *argTail});
        }

        LUAU_ASSERT(target);
        LUAU_ASSERT(mt);

        AstExpr* targetExpr = call->args.data[0];

        MetatableType mtv{target, mt};
        TypeId resultTy = arena->addType(mtv);

        if (AstExprLocal* targetLocal = targetExpr->as<AstExprLocal>())
        {
            scope->bindings[targetLocal->local].typeId = resultTy;

            BreadcrumbId bc = dfg->getBreadcrumb(targetLocal);
            scope->dcrRefinements[bc->def] = resultTy; // TODO: typestates: track this as an assignment
        }

        return InferencePack{arena->addTypePack({resultTy}), {refinementArena.variadic(returnRefinements)}};
    }
    else
    {
        if (matchAssert(*call) && !argumentRefinements.empty())
            applyRefinements(scope, call->args.data[0]->location, argumentRefinements[0]);

        // TODO: How do expectedTypes play into this?  Do they?
        TypePackId rets = arena->addTypePack(BlockedTypePack{});
        TypePackId argPack = arena->addTypePack(TypePack{args, argTail});
        FunctionType ftv(TypeLevel{}, scope.get(), argPack, rets, std::nullopt, call->self);

        NotNull<Constraint> fcc = addConstraint(scope, call->func->location,
            FunctionCallConstraint{
                fnType,
                argPack,
                rets,
                call,
                std::move(discriminantTypes),
                &module->astOverloadResolvedTypes,
            });

        // We force constraints produced by checking function arguments to wait
        // until after we have resolved the constraint on the function itself.
        // This ensures, for instance, that we start inferring the contents of
        // lambdas under the assumption that their arguments and return types
        // will be compatible with the enclosing function call.
        forEachConstraint(fnEndCheckpoint, argEndCheckpoint, this, [fcc](const ConstraintPtr& constraint) {
            fcc->dependencies.emplace_back(constraint.get());
        });

        return InferencePack{rets, {refinementArena.variadic(returnRefinements)}};
    }
}

Inference ConstraintGraphBuilder::check(
    const ScopePtr& scope, AstExpr* expr, ValueContext context, std::optional<TypeId> expectedType, bool forceSingleton)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportCodeTooComplex(expr->location);
        return Inference{builtinTypes->errorRecoveryType()};
    }

    Inference result;

    if (auto group = expr->as<AstExprGroup>())
        result = check(scope, group->expr, ValueContext::RValue, expectedType, forceSingleton);
    else if (auto stringExpr = expr->as<AstExprConstantString>())
        result = check(scope, stringExpr, expectedType, forceSingleton);
    else if (expr->is<AstExprConstantNumber>())
        result = Inference{builtinTypes->numberType};
    else if (auto boolExpr = expr->as<AstExprConstantBool>())
        result = check(scope, boolExpr, expectedType, forceSingleton);
    else if (expr->is<AstExprConstantNil>())
        result = Inference{builtinTypes->nilType};
    else if (auto local = expr->as<AstExprLocal>())
        result = check(scope, local, context);
    else if (auto global = expr->as<AstExprGlobal>())
        result = check(scope, global);
    else if (expr->is<AstExprVarargs>())
        result = flattenPack(scope, expr->location, checkPack(scope, expr));
    else if (auto call = expr->as<AstExprCall>())
        result = flattenPack(scope, expr->location, checkPack(scope, call)); // TODO: needs predicates too
    else if (auto a = expr->as<AstExprFunction>())
        result = check(scope, a, expectedType);
    else if (auto indexName = expr->as<AstExprIndexName>())
        result = check(scope, indexName);
    else if (auto indexExpr = expr->as<AstExprIndexExpr>())
        result = check(scope, indexExpr);
    else if (auto table = expr->as<AstExprTable>())
        result = check(scope, table, expectedType);
    else if (auto unary = expr->as<AstExprUnary>())
        result = check(scope, unary);
    else if (auto binary = expr->as<AstExprBinary>())
        result = check(scope, binary, expectedType);
    else if (auto ifElse = expr->as<AstExprIfElse>())
        result = check(scope, ifElse, expectedType);
    else if (auto typeAssert = expr->as<AstExprTypeAssertion>())
        result = check(scope, typeAssert);
    else if (auto interpString = expr->as<AstExprInterpString>())
        result = check(scope, interpString);
    else if (auto err = expr->as<AstExprError>())
    {
        // Open question: Should we traverse into this?
        for (AstExpr* subExpr : err->expressions)
            check(scope, subExpr);

        result = Inference{builtinTypes->errorRecoveryType()};
    }
    else
    {
        LUAU_ASSERT(0);
        result = Inference{freshType(scope)};
    }

    LUAU_ASSERT(result.ty);
    module->astTypes[expr] = result.ty;
    if (expectedType)
        module->astExpectedTypes[expr] = *expectedType;
    return result;
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprConstantString* string, std::optional<TypeId> expectedType, bool forceSingleton)
{
    if (forceSingleton)
        return Inference{arena->addType(SingletonType{StringSingleton{std::string{string->value.data, string->value.size}}})};

    if (expectedType)
    {
        const TypeId expectedTy = follow(*expectedType);
        if (get<BlockedType>(expectedTy) || get<PendingExpansionType>(expectedTy) || get<FreeType>(expectedTy))
        {
            TypeId ty = arena->addType(BlockedType{});
            TypeId singletonType = arena->addType(SingletonType(StringSingleton{std::string(string->value.data, string->value.size)}));
            addConstraint(scope, string->location, PrimitiveTypeConstraint{ty, expectedTy, singletonType, builtinTypes->stringType});
            return Inference{ty};
        }
        else if (maybeSingleton(expectedTy))
            return Inference{arena->addType(SingletonType{StringSingleton{std::string{string->value.data, string->value.size}}})};

        return Inference{builtinTypes->stringType};
    }

    return Inference{builtinTypes->stringType};
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprConstantBool* boolExpr, std::optional<TypeId> expectedType, bool forceSingleton)
{
    const TypeId singletonType = boolExpr->value ? builtinTypes->trueType : builtinTypes->falseType;
    if (forceSingleton)
        return Inference{singletonType};

    if (expectedType)
    {
        const TypeId expectedTy = follow(*expectedType);

        if (get<BlockedType>(expectedTy) || get<PendingExpansionType>(expectedTy))
        {
            TypeId ty = arena->addType(BlockedType{});
            addConstraint(scope, boolExpr->location, PrimitiveTypeConstraint{ty, expectedTy, singletonType, builtinTypes->booleanType});
            return Inference{ty};
        }
        else if (maybeSingleton(expectedTy))
            return Inference{singletonType};

        return Inference{builtinTypes->booleanType};
    }

    return Inference{builtinTypes->booleanType};
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprLocal* local, ValueContext context)
{
    BreadcrumbId bc = dfg->getBreadcrumb(local);

    if (auto ty = scope->lookup(bc->def); ty && context == ValueContext::RValue)
        return Inference{*ty, refinementArena.proposition(bc, builtinTypes->truthyType)};
    else if (auto ty = scope->lookup(local->local))
        return Inference{*ty, refinementArena.proposition(bc, builtinTypes->truthyType)};
    else
        ice->ice("AstExprLocal came before its declaration?");
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprGlobal* global)
{
    BreadcrumbId bc = dfg->getBreadcrumb(global);

    /* prepopulateGlobalScope() has already added all global functions to the environment by this point, so any
     * global that is not already in-scope is definitely an unknown symbol.
     */
    if (auto ty = scope->lookup(bc->def))
        return Inference{*ty, refinementArena.proposition(bc, builtinTypes->truthyType)};
    else if (auto ty = scope->lookup(global->name))
    {
        rootScope->dcrRefinements[bc->def] = *ty;
        return Inference{*ty, refinementArena.proposition(bc, builtinTypes->truthyType)};
    }
    else
    {
        reportError(global->location, UnknownSymbol{global->name.value});
        return Inference{builtinTypes->errorRecoveryType()};
    }
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprIndexName* indexName)
{
    TypeId obj = check(scope, indexName->expr).ty;
    TypeId result = arena->addType(BlockedType{});

    NullableBreadcrumbId bc = dfg->getBreadcrumb(indexName);
    if (bc)
    {
        if (auto ty = scope->lookup(bc->def))
            return Inference{*ty, refinementArena.proposition(NotNull{bc}, builtinTypes->truthyType)};

        scope->dcrRefinements[bc->def] = result;
    }

    addConstraint(scope, indexName->expr->location, HasPropConstraint{result, obj, indexName->index.value});

    if (bc)
        return Inference{result, refinementArena.proposition(NotNull{bc}, builtinTypes->truthyType)};
    else
        return Inference{result};
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprIndexExpr* indexExpr)
{
    TypeId obj = check(scope, indexExpr->expr).ty;
    TypeId indexType = check(scope, indexExpr->index).ty;
    TypeId result = freshType(scope);

    NullableBreadcrumbId bc = dfg->getBreadcrumb(indexExpr);
    if (bc)
    {
        if (auto ty = scope->lookup(bc->def))
            return Inference{*ty, refinementArena.proposition(NotNull{bc}, builtinTypes->truthyType)};

        scope->dcrRefinements[bc->def] = result;
    }

    TableIndexer indexer{indexType, result};
    TypeId tableType = arena->addType(TableType{TableType::Props{}, TableIndexer{indexType, result}, TypeLevel{}, scope.get(), TableState::Free});

    addConstraint(scope, indexExpr->expr->location, SubtypeConstraint{obj, tableType});

    if (bc)
        return Inference{result, refinementArena.proposition(NotNull{bc}, builtinTypes->truthyType)};
    else
        return Inference{result};
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprFunction* func, std::optional<TypeId> expectedType)
{
    Checkpoint startCheckpoint = checkpoint(this);
    FunctionSignature sig = checkFunctionSignature(scope, func, expectedType);
    checkFunctionBody(sig.bodyScope, func);
    Checkpoint endCheckpoint = checkpoint(this);

    TypeId generalizedTy = arena->addType(BlockedType{});
    NotNull<Constraint> gc = addConstraint(sig.signatureScope, func->location, GeneralizationConstraint{generalizedTy, sig.signature});

    Constraint* previous = nullptr;
    forEachConstraint(startCheckpoint, endCheckpoint, this, [gc, &previous](const ConstraintPtr& constraint) {
        gc->dependencies.emplace_back(constraint.get());

        if (auto psc = get<PackSubtypeConstraint>(*constraint); psc && psc->returns)
        {
            if (previous)
                constraint->dependencies.push_back(NotNull{previous});

            previous = constraint.get();
        }
    });

    return Inference{generalizedTy};
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprUnary* unary)
{
    auto [operandType, refinement] = check(scope, unary->expr);
    TypeId resultType = arena->addType(BlockedType{});
    addConstraint(scope, unary->location, UnaryConstraint{unary->op, operandType, resultType});

    if (unary->op == AstExprUnary::Not)
        return Inference{resultType, refinementArena.negation(refinement)};
    else
        return Inference{resultType};
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprBinary* binary, std::optional<TypeId> expectedType)
{
    auto [leftType, rightType, refinement] = checkBinary(scope, binary, expectedType);

    if (binary->op == AstExprBinary::Op::Add)
    {
        TypeId resultType = arena->addType(TypeFamilyInstanceType{
            NotNull{&kBuiltinTypeFamilies.addFamily},
            {leftType, rightType},
            {},
        });
        addConstraint(scope, binary->location, ReduceConstraint{resultType});
        return Inference{resultType, std::move(refinement)};
    }

    TypeId resultType = arena->addType(BlockedType{});
    addConstraint(scope, binary->location,
        BinaryConstraint{binary->op, leftType, rightType, resultType, binary, &module->astOriginalCallTypes, &module->astOverloadResolvedTypes});
    return Inference{resultType, std::move(refinement)};
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprIfElse* ifElse, std::optional<TypeId> expectedType)
{
    ScopePtr condScope = childScope(ifElse->condition, scope);
    RefinementId refinement = check(condScope, ifElse->condition).refinement;

    ScopePtr thenScope = childScope(ifElse->trueExpr, scope);
    applyRefinements(thenScope, ifElse->trueExpr->location, refinement);
    TypeId thenType = check(thenScope, ifElse->trueExpr, ValueContext::RValue, expectedType).ty;

    ScopePtr elseScope = childScope(ifElse->falseExpr, scope);
    applyRefinements(elseScope, ifElse->falseExpr->location, refinementArena.negation(refinement));
    TypeId elseType = check(elseScope, ifElse->falseExpr, ValueContext::RValue, expectedType).ty;

    return Inference{expectedType ? *expectedType : simplifyUnion(builtinTypes, arena, thenType, elseType).result};
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprTypeAssertion* typeAssert)
{
    check(scope, typeAssert->expr, ValueContext::RValue, std::nullopt);
    return Inference{resolveType(scope, typeAssert->annotation, /* inTypeArguments */ false)};
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprInterpString* interpString)
{
    for (AstExpr* expr : interpString->expressions)
        check(scope, expr);

    return Inference{builtinTypes->stringType};
}

std::tuple<TypeId, TypeId, RefinementId> ConstraintGraphBuilder::checkBinary(
    const ScopePtr& scope, AstExprBinary* binary, std::optional<TypeId> expectedType)
{
    if (binary->op == AstExprBinary::And)
    {
        std::optional<TypeId> relaxedExpectedLhs;

        if (expectedType)
            relaxedExpectedLhs = arena->addType(UnionType{{builtinTypes->falsyType, *expectedType}});

        auto [leftType, leftRefinement] = check(scope, binary->left, ValueContext::RValue, relaxedExpectedLhs);

        ScopePtr rightScope = childScope(binary->right, scope);
        applyRefinements(rightScope, binary->right->location, leftRefinement);
        auto [rightType, rightRefinement] = check(rightScope, binary->right, ValueContext::RValue, expectedType);

        return {leftType, rightType, refinementArena.conjunction(leftRefinement, rightRefinement)};
    }
    else if (binary->op == AstExprBinary::Or)
    {
        std::optional<TypeId> relaxedExpectedLhs;

        if (expectedType)
            relaxedExpectedLhs = arena->addType(UnionType{{builtinTypes->falsyType, *expectedType}});

        auto [leftType, leftRefinement] = check(scope, binary->left, ValueContext::RValue, relaxedExpectedLhs);

        ScopePtr rightScope = childScope(binary->right, scope);
        applyRefinements(rightScope, binary->right->location, refinementArena.negation(leftRefinement));
        auto [rightType, rightRefinement] = check(rightScope, binary->right, ValueContext::RValue, expectedType);

        return {leftType, rightType, refinementArena.disjunction(leftRefinement, rightRefinement)};
    }
    else if (auto typeguard = matchTypeGuard(binary))
    {
        TypeId leftType = check(scope, binary->left).ty;
        TypeId rightType = check(scope, binary->right).ty;

        NullableBreadcrumbId bc = dfg->getBreadcrumb(typeguard->target);
        if (!bc)
            return {leftType, rightType, nullptr};
        auto augmentForErrorSupression = [&](TypeId ty) -> TypeId {
            return arena->addType(UnionType{{ty, builtinTypes->errorType}});
        };

        TypeId discriminantTy = builtinTypes->neverType;
        if (typeguard->type == "nil")
            discriminantTy = builtinTypes->nilType;
        else if (typeguard->type == "string")
            discriminantTy = builtinTypes->stringType;
        else if (typeguard->type == "number")
            discriminantTy = builtinTypes->numberType;
        else if (typeguard->type == "boolean")
            discriminantTy = builtinTypes->booleanType;
        else if (typeguard->type == "thread")
            discriminantTy = builtinTypes->threadType;
        else if (typeguard->type == "table")
            discriminantTy = augmentForErrorSupression(builtinTypes->tableType);
        else if (typeguard->type == "function")
            discriminantTy = augmentForErrorSupression(builtinTypes->functionType);
        else if (typeguard->type == "userdata")
        {
            // For now, we don't really care about being accurate with userdata if the typeguard was using typeof.
            discriminantTy = builtinTypes->classType;
        }
        else if (!typeguard->isTypeof && typeguard->type == "vector")
            discriminantTy = builtinTypes->neverType; // TODO: figure out a way to deal with this quirky type
        else if (!typeguard->isTypeof)
            discriminantTy = builtinTypes->neverType;
        else if (auto typeFun = globalScope->lookupType(typeguard->type); typeFun && typeFun->typeParams.empty() && typeFun->typePackParams.empty())
        {
            TypeId ty = follow(typeFun->type);

            // We're only interested in the root class of any classes.
            if (auto ctv = get<ClassType>(ty); !ctv || ctv->parent == builtinTypes->classType)
                discriminantTy = ty;
        }

        RefinementId proposition = refinementArena.proposition(NotNull{bc}, discriminantTy);
        if (binary->op == AstExprBinary::CompareEq)
            return {leftType, rightType, proposition};
        else if (binary->op == AstExprBinary::CompareNe)
            return {leftType, rightType, refinementArena.negation(proposition)};
        else
            ice->ice("matchTypeGuard should only return a Some under `==` or `~=`!");
    }
    else if (binary->op == AstExprBinary::CompareEq || binary->op == AstExprBinary::CompareNe)
    {
        // We are checking a binary expression of the form a op b
        // Just because a op b is epxected to return a bool, doesn't mean a, b are expected to be bools too
        TypeId leftType = check(scope, binary->left, ValueContext::RValue, {}, true).ty;
        TypeId rightType = check(scope, binary->right, ValueContext::RValue, {}, true).ty;

        RefinementId leftRefinement = nullptr;
        if (auto bc = dfg->getBreadcrumb(binary->left))
            leftRefinement = refinementArena.proposition(NotNull{bc}, rightType);

        RefinementId rightRefinement = nullptr;
        if (auto bc = dfg->getBreadcrumb(binary->right))
            rightRefinement = refinementArena.proposition(NotNull{bc}, leftType);

        if (binary->op == AstExprBinary::CompareNe)
        {
            leftRefinement = refinementArena.negation(leftRefinement);
            rightRefinement = refinementArena.negation(rightRefinement);
        }

        return {leftType, rightType, refinementArena.equivalence(leftRefinement, rightRefinement)};
    }
    else
    {
        TypeId leftType = check(scope, binary->left, ValueContext::RValue).ty;
        TypeId rightType = check(scope, binary->right, ValueContext::RValue).ty;
        return {leftType, rightType, nullptr};
    }
}

std::vector<TypeId> ConstraintGraphBuilder::checkLValues(const ScopePtr& scope, AstArray<AstExpr*> exprs)
{
    std::vector<TypeId> types;
    types.reserve(exprs.size);

    for (AstExpr* expr : exprs)
        types.push_back(checkLValue(scope, expr));

    return types;
}

static bool isIndexNameEquivalent(AstExpr* expr)
{
    if (expr->is<AstExprIndexName>())
        return true;

    AstExprIndexExpr* e = expr->as<AstExprIndexExpr>();
    if (e == nullptr)
        return false;

    if (!e->index->is<AstExprConstantString>())
        return false;

    return true;
}

/**
 * This function is mostly about identifying properties that are being inserted into unsealed tables.
 *
 * If expr has the form name.a.b.c
 */
TypeId ConstraintGraphBuilder::checkLValue(const ScopePtr& scope, AstExpr* expr)
{
    if (auto indexExpr = expr->as<AstExprIndexExpr>(); indexExpr && !indexExpr->index->is<AstExprConstantString>())
    {
        // An indexer is only interesting in an lvalue-ey way if it is at the
        // tail of an expression.
        //
        // If the indexer is not at the tail, then we are not interested in
        // augmenting the lhs data structure with a new indexer.  Constraint
        // generation can treat it as an ordinary lvalue.
        //
        // eg
        //
        // a.b.c[1] = 44 -- lvalue
        // a.b[4].c = 2 -- rvalue

        TypeId resultType = arena->addType(BlockedType{});
        TypeId subjectType = check(scope, indexExpr->expr).ty;
        TypeId indexType = check(scope, indexExpr->index).ty;
        TypeId propType = arena->addType(BlockedType{});
        addConstraint(scope, expr->location, SetIndexerConstraint{resultType, subjectType, indexType, propType});

        module->astTypes[expr] = propType;

        return propType;
    }
    else if (!isIndexNameEquivalent(expr))
        return check(scope, expr, ValueContext::LValue).ty;

    Symbol sym;
    std::vector<std::string> segments;
    std::vector<AstExpr*> exprs;

    AstExpr* e = expr;
    while (e)
    {
        if (auto global = e->as<AstExprGlobal>())
        {
            sym = global->name;
            break;
        }
        else if (auto local = e->as<AstExprLocal>())
        {
            sym = local->local;
            break;
        }
        else if (auto indexName = e->as<AstExprIndexName>())
        {
            segments.push_back(indexName->index.value);
            exprs.push_back(e);
            e = indexName->expr;
        }
        else if (auto indexExpr = e->as<AstExprIndexExpr>())
        {
            // We need to populate the type for the index value
            check(scope, indexExpr->index, ValueContext::RValue);
            if (auto strIndex = indexExpr->index->as<AstExprConstantString>())
            {
                segments.push_back(std::string(strIndex->value.data, strIndex->value.size));
                exprs.push_back(e);
                e = indexExpr->expr;
            }
            else
            {
                return check(scope, expr, ValueContext::LValue).ty;
            }
        }
        else
            return check(scope, expr, ValueContext::LValue).ty;
    }

    LUAU_ASSERT(!segments.empty());

    std::reverse(begin(segments), end(segments));
    std::reverse(begin(exprs), end(exprs));

    auto lookupResult = scope->lookupEx(sym);
    if (!lookupResult)
        return check(scope, expr, ValueContext::LValue).ty;
    const auto [subjectBinding, symbolScope] = std::move(*lookupResult);
    TypeId subjectType = subjectBinding->typeId;

    TypeId propTy = freshType(scope);

    std::vector<std::string> segmentStrings(begin(segments), end(segments));

    TypeId updatedType = arena->addType(BlockedType{});
    addConstraint(scope, expr->location, SetPropConstraint{updatedType, subjectType, std::move(segmentStrings), propTy});

    TypeId prevSegmentTy = updatedType;
    for (size_t i = 0; i < segments.size(); ++i)
    {
        TypeId segmentTy = arena->addType(BlockedType{});
        module->astTypes[exprs[i]] = segmentTy;
        addConstraint(scope, expr->location, HasPropConstraint{segmentTy, prevSegmentTy, segments[i]});
        prevSegmentTy = segmentTy;
    }

    module->astTypes[expr] = prevSegmentTy;
    module->astTypes[e] = updatedType;

    if (!subjectType->persistent)
    {
        symbolScope->bindings[sym].typeId = updatedType;

        // This can fail if the user is erroneously trying to augment a builtin
        // table like os or string.
        if (auto bc = dfg->getBreadcrumb(e))
            symbolScope->dcrRefinements[bc->def] = updatedType;
    }

    return propTy;
}

Inference ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprTable* expr, std::optional<TypeId> expectedType)
{
    const bool expectedTypeIsFree = expectedType && get<FreeType>(follow(*expectedType));

    TypeId ty = arena->addType(TableType{});
    TableType* ttv = getMutable<TableType>(ty);
    LUAU_ASSERT(ttv);

    ttv->state = TableState::Unsealed;
    ttv->scope = scope.get();

    auto createIndexer = [this, scope, ttv](const Location& location, TypeId currentIndexType, TypeId currentResultType) {
        if (!ttv->indexer)
        {
            TypeId indexType = this->freshType(scope);
            TypeId resultType = this->freshType(scope);
            ttv->indexer = TableIndexer{indexType, resultType};
        }

        addConstraint(scope, location, SubtypeConstraint{ttv->indexer->indexType, currentIndexType});
        addConstraint(scope, location, SubtypeConstraint{ttv->indexer->indexResultType, currentResultType});
    };

    std::optional<TypeId> annotatedKeyType;
    std::optional<TypeId> annotatedIndexResultType;

    if (expectedType)
    {
        if (const TableType* ttv = get<TableType>(follow(*expectedType)))
        {
            if (ttv->indexer)
            {
                annotatedKeyType.emplace(follow(ttv->indexer->indexType));
                annotatedIndexResultType.emplace(ttv->indexer->indexResultType);
            }
        }
    }

    bool isIndexedResultType = false;
    std::optional<TypeId> pinnedIndexResultType;


    for (const AstExprTable::Item& item : expr->items)
    {
        std::optional<TypeId> expectedValueType;
        if (item.kind == AstExprTable::Item::Kind::General || item.kind == AstExprTable::Item::Kind::List)
            isIndexedResultType = true;

        if (item.key && expectedType && !expectedTypeIsFree)
        {
            if (auto stringKey = item.key->as<AstExprConstantString>())
            {
                ErrorVec errorVec;
                std::optional<TypeId> propTy =
                    findTablePropertyRespectingMeta(builtinTypes, errorVec, follow(*expectedType), stringKey->value.data, item.value->location);
                if (propTy)
                    expectedValueType = propTy;
                else
                {
                    expectedValueType = arena->addType(BlockedType{});
                    addConstraint(scope, item.value->location,
                        HasPropConstraint{*expectedValueType, *expectedType, stringKey->value.data, /*suppressSimplification*/ true});
                }
            }
        }

        // We'll resolve the expected index result type here with the following priority:
        // 1. Record table types - in which key, value pairs must be handled on a k,v pair basis.
        // In this case, the above if-statement will populate expectedValueType
        // 2. Someone places an annotation on a General or List table
        // Trust the annotation and have the solver inform them if they get it wrong
        // 3. Someone omits the annotation on a general or List table
        // Use the type of the first indexResultType as the expected type
        std::optional<TypeId> checkExpectedIndexResultType;
        if (expectedValueType)
        {
            checkExpectedIndexResultType = expectedValueType;
        }
        else if (annotatedIndexResultType)
        {
            checkExpectedIndexResultType = annotatedIndexResultType;
        }
        else if (pinnedIndexResultType)
        {
            checkExpectedIndexResultType = pinnedIndexResultType;
        }

        TypeId itemTy = check(scope, item.value, ValueContext::RValue, checkExpectedIndexResultType).ty;

        if (isIndexedResultType && !pinnedIndexResultType)
            pinnedIndexResultType = itemTy;

        if (item.key)
        {
            // Even though we don't need to use the type of the item's key if
            // it's a string constant, we still want to check it to populate
            // astTypes.
            TypeId keyTy = check(scope, item.key, ValueContext::RValue, annotatedKeyType).ty;

            if (AstExprConstantString* key = item.key->as<AstExprConstantString>())
            {
                ttv->props[key->value.begin()] = {itemTy};
            }
            else
            {
                createIndexer(item.key->location, keyTy, itemTy);
            }
        }
        else
        {
            TypeId numberType = builtinTypes->numberType;
            // FIXME?  The location isn't quite right here.  Not sure what is
            // right.
            createIndexer(item.value->location, numberType, itemTy);
        }
    }

    return Inference{ty};
}

ConstraintGraphBuilder::FunctionSignature ConstraintGraphBuilder::checkFunctionSignature(
    const ScopePtr& parent, AstExprFunction* fn, std::optional<TypeId> expectedType, std::optional<Location> originalName)
{
    ScopePtr signatureScope = nullptr;
    ScopePtr bodyScope = nullptr;
    TypePackId returnType = nullptr;

    std::vector<TypeId> genericTypes;
    std::vector<TypePackId> genericTypePacks;

    if (expectedType)
        expectedType = follow(*expectedType);

    bool hasGenerics = fn->generics.size > 0 || fn->genericPacks.size > 0;

    signatureScope = childScope(fn, parent);

    // We need to assign returnType before creating bodyScope so that the
    // return type gets propogated to bodyScope.
    returnType = freshTypePack(signatureScope);
    signatureScope->returnType = returnType;

    bodyScope = childScope(fn->body, signatureScope);

    if (hasGenerics)
    {
        std::vector<std::pair<Name, GenericTypeDefinition>> genericDefinitions = createGenerics(signatureScope, fn->generics);
        std::vector<std::pair<Name, GenericTypePackDefinition>> genericPackDefinitions = createGenericPacks(signatureScope, fn->genericPacks);

        // We do not support default values on function generics, so we only
        // care about the types involved.
        for (const auto& [name, g] : genericDefinitions)
        {
            genericTypes.push_back(g.ty);
        }

        for (const auto& [name, g] : genericPackDefinitions)
        {
            genericTypePacks.push_back(g.tp);
        }

        // Local variable works around an odd gcc 11.3 warning: <anonymous> may be used uninitialized
        std::optional<TypeId> none = std::nullopt;
        expectedType = none;
    }

    std::vector<TypeId> argTypes;
    std::vector<std::optional<FunctionArgument>> argNames;
    TypePack expectedArgPack;

    const FunctionType* expectedFunction = expectedType ? get<FunctionType>(*expectedType) : nullptr;
    // This check ensures that expectedType is precisely optional and not any (since any is also an optional type)
    if (expectedType && isOptional(*expectedType) && !get<AnyType>(*expectedType))
    {
        auto ut = get<UnionType>(*expectedType);
        for (auto u : ut)
        {
            if (get<FunctionType>(u) && !isNil(u))
            {
                expectedFunction = get<FunctionType>(u);
                break;
            }
        }
    }

    if (expectedFunction)
    {
        expectedArgPack = extendTypePack(*arena, builtinTypes, expectedFunction->argTypes, fn->args.size);

        genericTypes = expectedFunction->generics;
        genericTypePacks = expectedFunction->genericPacks;
    }

    if (fn->self)
    {
        TypeId selfType = freshType(signatureScope);
        argTypes.push_back(selfType);
        argNames.emplace_back(FunctionArgument{fn->self->name.value, fn->self->location});
        signatureScope->bindings[fn->self] = Binding{selfType, fn->self->location};

        BreadcrumbId bc = dfg->getBreadcrumb(fn->self);
        signatureScope->dcrRefinements[bc->def] = selfType;
    }

    for (size_t i = 0; i < fn->args.size; ++i)
    {
        AstLocal* local = fn->args.data[i];

        TypeId argTy = nullptr;
        if (local->annotation)
            argTy = resolveType(signatureScope, local->annotation, /* inTypeArguments */ false, /* replaceErrorWithFresh*/ true);
        else
        {
            argTy = freshType(signatureScope);

            if (i < expectedArgPack.head.size())
                addConstraint(signatureScope, local->location, SubtypeConstraint{argTy, expectedArgPack.head[i]});
        }

        argTypes.push_back(argTy);
        argNames.emplace_back(FunctionArgument{local->name.value, local->location});
        signatureScope->bindings[local] = Binding{argTy, local->location};

        BreadcrumbId bc = dfg->getBreadcrumb(local);
        signatureScope->dcrRefinements[bc->def] = argTy;
    }

    TypePackId varargPack = nullptr;

    if (fn->vararg)
    {
        if (fn->varargAnnotation)
        {
            TypePackId annotationType =
                resolveTypePack(signatureScope, fn->varargAnnotation, /* inTypeArguments */ false, /* replaceErrorWithFresh */ true);
            varargPack = annotationType;
        }
        else if (expectedArgPack.tail && get<VariadicTypePack>(*expectedArgPack.tail))
            varargPack = *expectedArgPack.tail;
        else
            varargPack = builtinTypes->anyTypePack;

        signatureScope->varargPack = varargPack;
        bodyScope->varargPack = varargPack;
    }
    else
    {
        varargPack = arena->addTypePack(VariadicTypePack{builtinTypes->anyType, /*hidden*/ true});
        // We do not add to signatureScope->varargPack because ... is not valid
        // in functions without an explicit ellipsis.

        signatureScope->varargPack = std::nullopt;
        bodyScope->varargPack = std::nullopt;
    }

    LUAU_ASSERT(nullptr != varargPack);

    // If there is both an annotation and an expected type, the annotation wins.
    // Type checking will sort out any discrepancies later.
    if (fn->returnAnnotation)
    {
        TypePackId annotatedRetType =
            resolveTypePack(signatureScope, *fn->returnAnnotation, /* inTypeArguments */ false, /* replaceErrorWithFresh*/ true);
        // We bind the annotated type directly here so that, when we need to
        // generate constraints for return types, we have a guarantee that we
        // know the annotated return type already, if one was provided.
        LUAU_ASSERT(get<FreeTypePack>(returnType));
        asMutable(returnType)->ty.emplace<BoundTypePack>(annotatedRetType);
    }
    else if (expectedFunction)
    {
        asMutable(returnType)->ty.emplace<BoundTypePack>(expectedFunction->retTypes);
    }

    // TODO: Preserve argument names in the function's type.

    FunctionType actualFunction{TypeLevel{}, parent.get(), arena->addTypePack(argTypes, varargPack), returnType};
    actualFunction.generics = std::move(genericTypes);
    actualFunction.genericPacks = std::move(genericTypePacks);
    actualFunction.argNames = std::move(argNames);
    actualFunction.hasSelf = fn->self != nullptr;

    FunctionDefinition defn;
    defn.definitionModuleName = module->name;
    defn.definitionLocation = fn->location;
    defn.varargLocation = fn->vararg ? std::make_optional(fn->varargLocation) : std::nullopt;
    defn.originalNameLocation = originalName.value_or(Location(fn->location.begin, 0));
    actualFunction.definition = defn;

    TypeId actualFunctionType = arena->addType(std::move(actualFunction));
    LUAU_ASSERT(actualFunctionType);
    module->astTypes[fn] = actualFunctionType;

    if (expectedType && get<FreeType>(*expectedType))
        bindFreeType(*expectedType, actualFunctionType);

    return {
        /* signature */ actualFunctionType,
        /* signatureScope */ signatureScope,
        /* bodyScope */ bodyScope,
    };
}

void ConstraintGraphBuilder::checkFunctionBody(const ScopePtr& scope, AstExprFunction* fn)
{
    visitBlockWithoutChildScope(scope, fn->body);

    // If it is possible for execution to reach the end of the function, the return type must be compatible with ()

    if (nullptr != getFallthrough(fn->body))
    {
        TypePackId empty = arena->addTypePack({}); // TODO we could have CGB retain one of these forever
        addConstraint(scope, fn->location, PackSubtypeConstraint{scope->returnType, empty});
    }
}

TypeId ConstraintGraphBuilder::resolveType(const ScopePtr& scope, AstType* ty, bool inTypeArguments, bool replaceErrorWithFresh)
{
    TypeId result = nullptr;

    if (auto ref = ty->as<AstTypeReference>())
    {
        if (FFlag::DebugLuauMagicTypes)
        {
            if (ref->name == "_luau_ice")
                ice->ice("_luau_ice encountered", ty->location);
            else if (ref->name == "_luau_print")
            {
                if (ref->parameters.size != 1 || !ref->parameters.data[0].type)
                {
                    reportError(ty->location, GenericError{"_luau_print requires one generic parameter"});
                    module->astResolvedTypes[ty] = builtinTypes->errorRecoveryType();
                    return builtinTypes->errorRecoveryType();
                }
                else
                    return resolveType(scope, ref->parameters.data[0].type, inTypeArguments);
            }
        }

        std::optional<TypeFun> alias;

        if (ref->prefix.has_value())
        {
            alias = scope->lookupImportedType(ref->prefix->value, ref->name.value);
        }
        else
        {
            alias = scope->lookupType(ref->name.value);
        }

        if (alias.has_value())
        {
            // If the alias is not generic, we don't need to set up a blocked
            // type and an instantiation constraint.
            if (alias.has_value() && alias->typeParams.empty() && alias->typePackParams.empty())
            {
                result = alias->type;
            }
            else
            {
                std::vector<TypeId> parameters;
                std::vector<TypePackId> packParameters;

                for (const AstTypeOrPack& p : ref->parameters)
                {
                    // We do not enforce the ordering of types vs. type packs here;
                    // that is done in the parser.
                    if (p.type)
                    {
                        parameters.push_back(resolveType(scope, p.type, /* inTypeArguments */ true));
                    }
                    else if (p.typePack)
                    {
                        packParameters.push_back(resolveTypePack(scope, p.typePack, /* inTypeArguments */ true));
                    }
                    else
                    {
                        // This indicates a parser bug: one of these two pointers
                        // should be set.
                        LUAU_ASSERT(false);
                    }
                }

                result = arena->addType(PendingExpansionType{ref->prefix, ref->name, parameters, packParameters});

                // If we're not in a type argument context, we need to create a constraint that expands this.
                // The dispatching of the above constraint will queue up additional constraints for nested
                // type function applications.
                if (!inTypeArguments)
                    addConstraint(scope, ty->location, TypeAliasExpansionConstraint{/* target */ result});
            }
        }
        else
        {
            result = builtinTypes->errorRecoveryType();
            if (replaceErrorWithFresh)
                result = freshType(scope);
        }
    }
    else if (auto tab = ty->as<AstTypeTable>())
    {
        TableType::Props props;
        std::optional<TableIndexer> indexer;

        for (const AstTableProp& prop : tab->props)
        {
            std::string name = prop.name.value;
            // TODO: Recursion limit.
            TypeId propTy = resolveType(scope, prop.type, inTypeArguments);
            // TODO: Fill in location.
            props[name] = {propTy};
        }

        if (tab->indexer)
        {
            // TODO: Recursion limit.
            indexer = TableIndexer{
                resolveType(scope, tab->indexer->indexType, inTypeArguments),
                resolveType(scope, tab->indexer->resultType, inTypeArguments),
            };
        }

        result = arena->addType(TableType{props, indexer, scope->level, scope.get(), TableState::Sealed});
    }
    else if (auto fn = ty->as<AstTypeFunction>())
    {
        // TODO: Recursion limit.
        bool hasGenerics = fn->generics.size > 0 || fn->genericPacks.size > 0;
        ScopePtr signatureScope = nullptr;

        std::vector<TypeId> genericTypes;
        std::vector<TypePackId> genericTypePacks;

        // If we don't have generics, we do not need to generate a child scope
        // for the generic bindings to live on.
        if (hasGenerics)
        {
            signatureScope = childScope(fn, scope);

            std::vector<std::pair<Name, GenericTypeDefinition>> genericDefinitions = createGenerics(signatureScope, fn->generics);
            std::vector<std::pair<Name, GenericTypePackDefinition>> genericPackDefinitions = createGenericPacks(signatureScope, fn->genericPacks);

            for (const auto& [name, g] : genericDefinitions)
            {
                genericTypes.push_back(g.ty);
            }

            for (const auto& [name, g] : genericPackDefinitions)
            {
                genericTypePacks.push_back(g.tp);
            }
        }
        else
        {
            // To eliminate the need to branch on hasGenerics below, we say that
            // the signature scope is the parent scope if we don't have
            // generics.
            signatureScope = scope;
        }

        TypePackId argTypes = resolveTypePack(signatureScope, fn->argTypes, inTypeArguments, replaceErrorWithFresh);
        TypePackId returnTypes = resolveTypePack(signatureScope, fn->returnTypes, inTypeArguments, replaceErrorWithFresh);

        // TODO: FunctionType needs a pointer to the scope so that we know
        // how to quantify/instantiate it.
        FunctionType ftv{TypeLevel{}, scope.get(), {}, {}, argTypes, returnTypes};

        // This replicates the behavior of the appropriate FunctionType
        // constructors.
        ftv.generics = std::move(genericTypes);
        ftv.genericPacks = std::move(genericTypePacks);

        ftv.argNames.reserve(fn->argNames.size);
        for (const auto& el : fn->argNames)
        {
            if (el)
            {
                const auto& [name, location] = *el;
                ftv.argNames.push_back(FunctionArgument{name.value, location});
            }
            else
            {
                ftv.argNames.push_back(std::nullopt);
            }
        }

        result = arena->addType(std::move(ftv));
    }
    else if (auto tof = ty->as<AstTypeTypeof>())
    {
        // TODO: Recursion limit.
        TypeId exprType = check(scope, tof->expr).ty;
        result = exprType;
    }
    else if (auto unionAnnotation = ty->as<AstTypeUnion>())
    {
        std::vector<TypeId> parts;
        for (AstType* part : unionAnnotation->types)
        {
            // TODO: Recursion limit.
            parts.push_back(resolveType(scope, part, inTypeArguments));
        }

        result = arena->addType(UnionType{parts});
    }
    else if (auto intersectionAnnotation = ty->as<AstTypeIntersection>())
    {
        std::vector<TypeId> parts;
        for (AstType* part : intersectionAnnotation->types)
        {
            // TODO: Recursion limit.
            parts.push_back(resolveType(scope, part, inTypeArguments));
        }

        result = arena->addType(IntersectionType{parts});
    }
    else if (auto boolAnnotation = ty->as<AstTypeSingletonBool>())
    {
        result = arena->addType(SingletonType(BooleanSingleton{boolAnnotation->value}));
    }
    else if (auto stringAnnotation = ty->as<AstTypeSingletonString>())
    {
        result = arena->addType(SingletonType(StringSingleton{std::string(stringAnnotation->value.data, stringAnnotation->value.size)}));
    }
    else if (ty->is<AstTypeError>())
    {
        result = builtinTypes->errorRecoveryType();
        if (replaceErrorWithFresh)
            result = freshType(scope);
    }
    else
    {
        LUAU_ASSERT(0);
        result = builtinTypes->errorRecoveryType();
    }

    module->astResolvedTypes[ty] = result;
    return result;
}

TypePackId ConstraintGraphBuilder::resolveTypePack(const ScopePtr& scope, AstTypePack* tp, bool inTypeArgument, bool replaceErrorWithFresh)
{
    TypePackId result;
    if (auto expl = tp->as<AstTypePackExplicit>())
    {
        result = resolveTypePack(scope, expl->typeList, inTypeArgument, replaceErrorWithFresh);
    }
    else if (auto var = tp->as<AstTypePackVariadic>())
    {
        TypeId ty = resolveType(scope, var->variadicType, inTypeArgument, replaceErrorWithFresh);
        result = arena->addTypePack(TypePackVar{VariadicTypePack{ty}});
    }
    else if (auto gen = tp->as<AstTypePackGeneric>())
    {
        if (std::optional<TypePackId> lookup = scope->lookupPack(gen->genericName.value))
        {
            result = *lookup;
        }
        else
        {
            reportError(tp->location, UnknownSymbol{gen->genericName.value, UnknownSymbol::Context::Type});
            result = builtinTypes->errorRecoveryTypePack();
        }
    }
    else
    {
        LUAU_ASSERT(0);
        result = builtinTypes->errorRecoveryTypePack();
    }

    module->astResolvedTypePacks[tp] = result;
    return result;
}

TypePackId ConstraintGraphBuilder::resolveTypePack(const ScopePtr& scope, const AstTypeList& list, bool inTypeArguments, bool replaceErrorWithFresh)
{
    std::vector<TypeId> head;

    for (AstType* headTy : list.types)
    {
        head.push_back(resolveType(scope, headTy, inTypeArguments, replaceErrorWithFresh));
    }

    std::optional<TypePackId> tail = std::nullopt;
    if (list.tailType)
    {
        tail = resolveTypePack(scope, list.tailType, inTypeArguments, replaceErrorWithFresh);
    }

    return arena->addTypePack(TypePack{head, tail});
}

std::vector<std::pair<Name, GenericTypeDefinition>> ConstraintGraphBuilder::createGenerics(
    const ScopePtr& scope, AstArray<AstGenericType> generics, bool useCache, bool addTypes)
{
    std::vector<std::pair<Name, GenericTypeDefinition>> result;
    for (const auto& generic : generics)
    {
        TypeId genericTy = nullptr;

        if (auto it = scope->parent->typeAliasTypeParameters.find(generic.name.value); useCache && it != scope->parent->typeAliasTypeParameters.end())
            genericTy = it->second;
        else
        {
            genericTy = arena->addType(GenericType{scope.get(), generic.name.value});
            scope->parent->typeAliasTypeParameters[generic.name.value] = genericTy;
        }

        std::optional<TypeId> defaultTy = std::nullopt;

        if (generic.defaultValue)
            defaultTy = resolveType(scope, generic.defaultValue, /* inTypeArguments */ false);

        if (addTypes)
            scope->privateTypeBindings[generic.name.value] = TypeFun{genericTy};

        result.push_back({generic.name.value, GenericTypeDefinition{genericTy, defaultTy}});
    }

    return result;
}

std::vector<std::pair<Name, GenericTypePackDefinition>> ConstraintGraphBuilder::createGenericPacks(
    const ScopePtr& scope, AstArray<AstGenericTypePack> generics, bool useCache, bool addTypes)
{
    std::vector<std::pair<Name, GenericTypePackDefinition>> result;
    for (const auto& generic : generics)
    {
        TypePackId genericTy;

        if (auto it = scope->parent->typeAliasTypePackParameters.find(generic.name.value);
            useCache && it != scope->parent->typeAliasTypePackParameters.end())
            genericTy = it->second;
        else
        {
            genericTy = arena->addTypePack(TypePackVar{GenericTypePack{scope.get(), generic.name.value}});
            scope->parent->typeAliasTypePackParameters[generic.name.value] = genericTy;
        }

        std::optional<TypePackId> defaultTy = std::nullopt;

        if (generic.defaultValue)
            defaultTy = resolveTypePack(scope, generic.defaultValue, /* inTypeArguments */ false);

        if (addTypes)
            scope->privateTypePackBindings[generic.name.value] = genericTy;

        result.push_back({generic.name.value, GenericTypePackDefinition{genericTy, defaultTy}});
    }

    return result;
}

Inference ConstraintGraphBuilder::flattenPack(const ScopePtr& scope, Location location, InferencePack pack)
{
    const auto& [tp, refinements] = pack;
    RefinementId refinement = nullptr;
    if (!refinements.empty())
        refinement = refinements[0];

    if (auto f = first(tp))
        return Inference{*f, refinement};

    TypeId typeResult = arena->addType(BlockedType{});
    TypePackId resultPack = arena->addTypePack({typeResult}, arena->freshTypePack(scope.get()));
    addConstraint(scope, location, UnpackConstraint{resultPack, tp});

    return Inference{typeResult, refinement};
}

void ConstraintGraphBuilder::reportError(Location location, TypeErrorData err)
{
    errors.push_back(TypeError{location, module->name, std::move(err)});

    if (logger)
        logger->captureGenerationError(errors.back());
}

void ConstraintGraphBuilder::reportCodeTooComplex(Location location)
{
    errors.push_back(TypeError{location, module->name, CodeTooComplex{}});

    if (logger)
        logger->captureGenerationError(errors.back());
}

struct GlobalPrepopulator : AstVisitor
{
    const NotNull<Scope> globalScope;
    const NotNull<TypeArena> arena;

    GlobalPrepopulator(NotNull<Scope> globalScope, NotNull<TypeArena> arena)
        : globalScope(globalScope)
        , arena(arena)
    {
    }

    bool visit(AstStatFunction* function) override
    {
        if (AstExprGlobal* g = function->name->as<AstExprGlobal>())
            globalScope->bindings[g->name] = Binding{arena->addType(BlockedType{})};

        return true;
    }
};

void ConstraintGraphBuilder::prepopulateGlobalScope(const ScopePtr& globalScope, AstStatBlock* program)
{
    GlobalPrepopulator gp{NotNull{globalScope.get()}, arena};

    if (prepareModuleScope)
        prepareModuleScope(module->name, globalScope);

    program->visit(&gp);
}

std::vector<std::optional<TypeId>> ConstraintGraphBuilder::getExpectedCallTypesForFunctionOverloads(const TypeId fnType)
{
    std::vector<TypeId> funTys;
    if (auto it = get<IntersectionType>(follow(fnType)))
    {
        for (TypeId intersectionComponent : it)
        {
            funTys.push_back(intersectionComponent);
        }
    }

    std::vector<std::optional<TypeId>> expectedTypes;
    // For a list of functions f_0 : e_0 -> r_0, ... f_n : e_n -> r_n,
    // emit a list of arguments that the function could take at each position
    // by unioning the arguments at each place
    auto assignOption = [this, &expectedTypes](size_t index, TypeId ty) {
        if (index == expectedTypes.size())
            expectedTypes.push_back(ty);
        else if (ty)
        {
            auto& el = expectedTypes[index];

            if (!el)
                el = ty;
            else
            {
                std::vector<TypeId> result = reduceUnion({*el, ty});
                if (result.empty())
                    el = builtinTypes->neverType;
                else if (result.size() == 1)
                    el = result[0];
                else
                    el = module->internalTypes.addType(UnionType{std::move(result)});
            }
        }
    };

    for (const TypeId overload : funTys)
    {
        if (const FunctionType* ftv = get<FunctionType>(follow(overload)))
        {
            auto [argsHead, argsTail] = flatten(ftv->argTypes);
            size_t start = ftv->hasSelf ? 1 : 0;
            size_t index = 0;
            for (size_t i = start; i < argsHead.size(); ++i)
                assignOption(index++, argsHead[i]);
            if (argsTail)
            {
                argsTail = follow(*argsTail);
                if (const VariadicTypePack* vtp = get<VariadicTypePack>(*argsTail))
                {
                    while (index < funTys.size())
                        assignOption(index++, vtp->ty);
                }
            }
        }
    }

    // TODO vvijay Feb 24, 2023 apparently we have to demote the types here?

    return expectedTypes;
}

std::vector<NotNull<Constraint>> borrowConstraints(const std::vector<ConstraintPtr>& constraints)
{
    std::vector<NotNull<Constraint>> result;
    result.reserve(constraints.size());

    for (const auto& c : constraints)
        result.emplace_back(c.get());

    return result;
}

} // namespace Luau
