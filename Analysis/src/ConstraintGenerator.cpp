// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ConstraintGenerator.h"

#include "Luau/Ast.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/Constraint.h"
#include "Luau/ControlFlow.h"
#include "Luau/DcrLogger.h"
#include "Luau/Def.h"
#include "Luau/DenseHash.h"
#include "Luau/ModuleResolver.h"
#include "Luau/NotNull.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Refinement.h"
#include "Luau/Scope.h"
#include "Luau/Simplify.h"
#include "Luau/StringUtils.h"
#include "Luau/TableLiteralInference.h"
#include "Luau/TimeTrace.h"
#include "Luau/Type.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"
#include "Luau/VisitType.h"

#include <algorithm>
#include <memory>

LUAU_FASTINT(LuauCheckRecursionLimit)
LUAU_FASTFLAG(DebugLuauLogSolverToJson)
LUAU_FASTFLAG(DebugLuauMagicTypes)
LUAU_FASTFLAG(DebugLuauEqSatSimplification)
LUAU_FASTFLAG(LuauTypestateBuiltins2)

LUAU_FASTFLAGVARIABLE(LuauNewSolverVisitErrorExprLvalues)
LUAU_FASTFLAGVARIABLE(LuauNewSolverPrePopulateClasses)
LUAU_FASTFLAGVARIABLE(LuauUserTypeFunExportedAndLocal)
LUAU_FASTFLAGVARIABLE(LuauNewSolverPopulateTableLocations)

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

struct TypeGuard
{
    bool isTypeof;
    AstExpr* target;
    std::string type;
};

static std::optional<TypeGuard> matchTypeGuard(const AstExprBinary::Op op, AstExpr* left, AstExpr* right)
{
    if (op != AstExprBinary::CompareEq && op != AstExprBinary::CompareNe)
        return std::nullopt;

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

namespace
{

struct Checkpoint
{
    size_t offset;
};

Checkpoint checkpoint(const ConstraintGenerator* cg)
{
    return Checkpoint{cg->constraints.size()};
}

template<typename F>
void forEachConstraint(const Checkpoint& start, const Checkpoint& end, const ConstraintGenerator* cg, F f)
{
    for (size_t i = start.offset; i < end.offset; ++i)
        f(cg->constraints[i]);
}

struct HasFreeType : TypeOnceVisitor
{
    bool result = false;

    HasFreeType() {}

    bool visit(TypeId ty) override
    {
        if (result || ty->persistent)
            return false;
        return true;
    }

    bool visit(TypePackId tp) override
    {
        if (result)
            return false;
        return true;
    }

    bool visit(TypeId ty, const ClassType&) override
    {
        return false;
    }

    bool visit(TypeId ty, const FreeType&) override
    {
        result = true;
        return false;
    }

    bool visit(TypePackId ty, const FreeTypePack&) override
    {
        result = true;
        return false;
    }
};

bool hasFreeType(TypeId ty)
{
    HasFreeType hft{};
    hft.traverse(ty);
    return hft.result;
}

} // namespace

ConstraintGenerator::ConstraintGenerator(
    ModulePtr module,
    NotNull<Normalizer> normalizer,
    NotNull<Simplifier> simplifier,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    NotNull<ModuleResolver> moduleResolver,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<InternalErrorReporter> ice,
    const ScopePtr& globalScope,
    std::function<void(const ModuleName&, const ScopePtr&)> prepareModuleScope,
    DcrLogger* logger,
    NotNull<DataFlowGraph> dfg,
    std::vector<RequireCycle> requireCycles
)
    : module(module)
    , builtinTypes(builtinTypes)
    , arena(normalizer->arena)
    , rootScope(nullptr)
    , dfg(dfg)
    , normalizer(normalizer)
    , simplifier(simplifier)
    , typeFunctionRuntime(typeFunctionRuntime)
    , moduleResolver(moduleResolver)
    , ice(ice)
    , globalScope(globalScope)
    , prepareModuleScope(std::move(prepareModuleScope))
    , requireCycles(std::move(requireCycles))
    , logger(logger)
{
    LUAU_ASSERT(module);
}

void ConstraintGenerator::visitModuleRoot(AstStatBlock* block)
{
    LUAU_TIMETRACE_SCOPE("ConstraintGenerator::visitModuleRoot", "Typechecking");

    LUAU_ASSERT(scopes.empty());
    LUAU_ASSERT(rootScope == nullptr);
    ScopePtr scope = std::make_shared<Scope>(globalScope);
    rootScope = scope.get();
    scopes.emplace_back(block->location, scope);
    rootScope->location = block->location;
    module->astScopes[block] = NotNull{scope.get()};

    rootScope->returnType = freshTypePack(scope);

    TypeId moduleFnTy = arena->addType(FunctionType{TypeLevel{}, rootScope, builtinTypes->anyTypePack, rootScope->returnType});
    interiorTypes.emplace_back();

    prepopulateGlobalScope(scope, block);

    Checkpoint start = checkpoint(this);

    ControlFlow cf = visitBlockWithoutChildScope(scope, block);
    if (cf == ControlFlow::None)
        addConstraint(scope, block->location, PackSubtypeConstraint{builtinTypes->emptyTypePack, rootScope->returnType});

    Checkpoint end = checkpoint(this);

    TypeId result = arena->addType(BlockedType{});
    NotNull<Constraint> genConstraint =
        addConstraint(scope, block->location, GeneralizationConstraint{result, moduleFnTy, std::move(interiorTypes.back())});
    getMutable<BlockedType>(result)->setOwner(genConstraint);
    forEachConstraint(
        start,
        end,
        this,
        [genConstraint](const ConstraintPtr& c)
        {
            genConstraint->dependencies.push_back(NotNull{c.get()});
        }
    );

    interiorTypes.pop_back();

    fillInInferredBindings(scope, block);

    if (logger)
        logger->captureGenerationModule(module);

    for (const auto& [ty, domain] : localTypes)
    {
        // FIXME: This isn't the most efficient thing.
        TypeId domainTy = builtinTypes->neverType;
        for (TypeId d : domain)
        {
            d = follow(d);
            if (d == ty)
                continue;
            domainTy = simplifyUnion(scope, Location{}, domainTy, d);
        }

        LUAU_ASSERT(get<BlockedType>(ty));
        asMutable(ty)->ty.emplace<BoundType>(domainTy);
    }
}

void ConstraintGenerator::visitFragmentRoot(const ScopePtr& resumeScope, AstStatBlock* block)
{
    // We prepopulate global data in the resumeScope to avoid writing data into the old modules scopes
    prepopulateGlobalScopeForFragmentTypecheck(globalScope, resumeScope, block);
    // Pre
    // We need to pop the interior types,
    interiorTypes.emplace_back();
    visitBlockWithoutChildScope(resumeScope, block);
    // Post
    interiorTypes.pop_back();

    fillInInferredBindings(resumeScope, block);

    if (logger)
        logger->captureGenerationModule(module);

    for (const auto& [ty, domain] : localTypes)
    {
        // FIXME: This isn't the most efficient thing.
        TypeId domainTy = builtinTypes->neverType;
        for (TypeId d : domain)
        {
            d = follow(d);
            if (d == ty)
                continue;
            domainTy = simplifyUnion(resumeScope, resumeScope->location, domainTy, d);
        }

        LUAU_ASSERT(get<BlockedType>(ty));
        asMutable(ty)->ty.emplace<BoundType>(domainTy);
    }
}

TypeId ConstraintGenerator::freshType(const ScopePtr& scope)
{
    return Luau::freshType(arena, builtinTypes, scope.get());
}

TypePackId ConstraintGenerator::freshTypePack(const ScopePtr& scope)
{
    FreeTypePack f{scope.get()};
    return arena->addTypePack(TypePackVar{std::move(f)});
}

TypePackId ConstraintGenerator::addTypePack(std::vector<TypeId> head, std::optional<TypePackId> tail)
{
    if (head.empty())
    {
        if (tail)
            return *tail;
        else
            return builtinTypes->emptyTypePack;
    }
    else
        return arena->addTypePack(TypePack{std::move(head), tail});
}

ScopePtr ConstraintGenerator::childScope(AstNode* node, const ScopePtr& parent)
{
    auto scope = std::make_shared<Scope>(parent);
    scopes.emplace_back(node->location, scope);
    scope->location = node->location;

    scope->returnType = parent->returnType;
    scope->varargPack = parent->varargPack;

    parent->children.push_back(NotNull{scope.get()});
    module->astScopes[node] = scope.get();

    return scope;
}

std::optional<TypeId> ConstraintGenerator::lookup(const ScopePtr& scope, Location location, DefId def, bool prototype)
{
    if (get<Cell>(def))
        return scope->lookup(def);
    if (auto phi = get<Phi>(def))
    {
        if (auto found = scope->lookup(def))
            return *found;
        else if (!prototype && phi->operands.size() == 1)
            return lookup(scope, location, phi->operands.at(0), prototype);
        else if (!prototype)
            return std::nullopt;

        TypeId res = builtinTypes->neverType;

        for (DefId operand : phi->operands)
        {
            // `scope->lookup(operand)` may return nothing because we only bind a type to that operand
            // once we've seen that particular `DefId`. In this case, we need to prototype those types
            // and use those at a later time.
            std::optional<TypeId> ty = lookup(scope, location, operand, /*prototype*/ false);
            if (!ty)
            {
                ty = arena->addType(BlockedType{});
                localTypes.try_insert(*ty, {});
                rootScope->lvalueTypes[operand] = *ty;
            }

            res = makeUnion(scope, location, res, *ty);
        }

        scope->lvalueTypes[def] = res;
        return res;
    }
    else
        ice->ice("ConstraintGenerator::lookup is inexhaustive?");
}

NotNull<Constraint> ConstraintGenerator::addConstraint(const ScopePtr& scope, const Location& location, ConstraintV cv)
{
    return NotNull{constraints.emplace_back(new Constraint{NotNull{scope.get()}, location, std::move(cv)}).get()};
}

NotNull<Constraint> ConstraintGenerator::addConstraint(const ScopePtr& scope, std::unique_ptr<Constraint> c)
{
    return NotNull{constraints.emplace_back(std::move(c)).get()};
}

void ConstraintGenerator::unionRefinements(
    const ScopePtr& scope,
    Location location,
    const RefinementContext& lhs,
    const RefinementContext& rhs,
    RefinementContext& dest,
    std::vector<ConstraintV>* constraints
)
{
    const auto intersect = [&](const std::vector<TypeId>& types)
    {
        if (1 == types.size())
            return types[0];
        else if (2 == types.size())
            return makeIntersect(scope, location, types[0], types[1]);

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
        dest.get(def)->discriminantTypes.push_back(makeUnion(scope, location, leftDiscriminantTy, rightDiscriminantTy));
        dest.get(def)->shouldAppendNilType |= partition.shouldAppendNilType || rhsIt->second.shouldAppendNilType;
    }
}

void ConstraintGenerator::computeRefinement(
    const ScopePtr& scope,
    Location location,
    RefinementId refinement,
    RefinementContext* refis,
    bool sense,
    bool eq,
    std::vector<ConstraintV>* constraints
)
{
    if (!refinement)
        return;
    else if (auto variadic = get<Variadic>(refinement))
    {
        for (RefinementId refi : variadic->refinements)
            computeRefinement(scope, location, refi, refis, sense, eq, constraints);
    }
    else if (auto negation = get<Negation>(refinement))
        return computeRefinement(scope, location, negation->refinement, refis, !sense, eq, constraints);
    else if (auto conjunction = get<Conjunction>(refinement))
    {
        RefinementContext lhsRefis;
        RefinementContext rhsRefis;

        computeRefinement(scope, location, conjunction->lhs, sense ? refis : &lhsRefis, sense, eq, constraints);
        computeRefinement(scope, location, conjunction->rhs, sense ? refis : &rhsRefis, sense, eq, constraints);

        if (!sense)
            unionRefinements(scope, location, lhsRefis, rhsRefis, *refis, constraints);
    }
    else if (auto disjunction = get<Disjunction>(refinement))
    {
        RefinementContext lhsRefis;
        RefinementContext rhsRefis;

        computeRefinement(scope, location, disjunction->lhs, sense ? &lhsRefis : refis, sense, eq, constraints);
        computeRefinement(scope, location, disjunction->rhs, sense ? &rhsRefis : refis, sense, eq, constraints);

        if (sense)
            unionRefinements(scope, location, lhsRefis, rhsRefis, *refis, constraints);
    }
    else if (auto equivalence = get<Equivalence>(refinement))
    {
        computeRefinement(scope, location, equivalence->lhs, refis, sense, true, constraints);
        computeRefinement(scope, location, equivalence->rhs, refis, sense, true, constraints);
    }
    else if (auto proposition = get<Proposition>(refinement))
    {
        TypeId discriminantTy = proposition->discriminantTy;

        // if we have a negative sense, then we need to negate the discriminant
        if (!sense)
            discriminantTy = arena->addType(NegationType{discriminantTy});

        if (eq)
            discriminantTy = createTypeFunctionInstance(builtinTypeFunctions().singletonFunc, {discriminantTy}, {}, scope, location);

        for (const RefinementKey* key = proposition->key; key; key = key->parent)
        {
            refis->insert(key->def, {});
            refis->get(key->def)->discriminantTypes.push_back(discriminantTy);

            // Reached leaf node
            if (!key->propName)
                break;

            TypeId nextDiscriminantTy = arena->addType(TableType{});
            NotNull<TableType> table{getMutable<TableType>(nextDiscriminantTy)};
            // When we fully support read-write properties (i.e. when we allow properties with
            // completely disparate read and write types), then the following property can be
            // set to read-only since refinements only tell us about what we read. This cannot
            // be allowed yet though because it causes read and write types to diverge.
            table->props[*key->propName] = Property::rw(discriminantTy);
            table->scope = scope.get();
            table->state = TableState::Sealed;

            discriminantTy = nextDiscriminantTy;
        }

        // When the top-level expression is `t[x]`, we want to refine it into `nil`, not `never`.
        LUAU_ASSERT(refis->get(proposition->key->def));
        refis->get(proposition->key->def)->shouldAppendNilType = (sense || !eq) && containsSubscriptedDefinition(proposition->key->def);
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

enum RefinementsOpKind
{
    Intersect,
    Refine,
    None
};

void ConstraintGenerator::applyRefinements(const ScopePtr& scope, Location location, RefinementId refinement)
{
    if (!refinement)
        return;

    RefinementContext refinements;
    std::vector<ConstraintV> constraints;
    computeRefinement(scope, location, refinement, &refinements, /*sense*/ true, /*eq*/ false, &constraints);
    auto flushConstraints = [this, &scope, &location](RefinementsOpKind kind, TypeId ty, std::vector<TypeId>& discriminants)
    {
        if (discriminants.empty())
            return ty;
        if (kind == RefinementsOpKind::None)
        {
            LUAU_ASSERT(false);
            return ty;
        }
        std::vector<TypeId> args = {ty};
        const TypeFunction& func = kind == RefinementsOpKind::Intersect ? builtinTypeFunctions().intersectFunc : builtinTypeFunctions().refineFunc;
        LUAU_ASSERT(!func.name.empty());
        args.insert(args.end(), discriminants.begin(), discriminants.end());
        TypeId resultType = createTypeFunctionInstance(func, args, {}, scope, location);
        discriminants.clear();
        return resultType;
    };

    for (auto& [def, partition] : refinements)
    {
        if (std::optional<TypeId> defTy = lookup(scope, location, def))
        {
            TypeId ty = *defTy;
            if (partition.shouldAppendNilType)
                ty = arena->addType(UnionType{{ty, builtinTypes->nilType}});
            // Intersect ty with every discriminant type. If either type is not
            // sufficiently solved, we queue the intersection up via an
            // IntersectConstraint.
            // For each discriminant ty, we accumulated it onto ty, creating a longer and longer
            // sequence of refine constraints. On every loop of this we called mustDeferIntersection.
            // For sufficiently large types, we would blow the stack.
            // Instead, we record all the discriminant types in sequence
            // and then dispatch a single refine constraint with multiple arguments. This helps us avoid
            // the potentially expensive check on mustDeferIntersection
            std::vector<TypeId> discriminants;
            RefinementsOpKind kind = RefinementsOpKind::None;
            bool mustDefer = mustDeferIntersection(ty);
            for (TypeId dt : partition.discriminantTypes)
            {
                mustDefer = mustDefer || mustDeferIntersection(dt);
                if (mustDefer)
                {
                    if (kind == RefinementsOpKind::Intersect)
                        ty = flushConstraints(kind, ty, discriminants);
                    kind = RefinementsOpKind::Refine;

                    discriminants.push_back(dt);
                }
                else
                {
                    ErrorSuppression status = shouldSuppressErrors(normalizer, ty);
                    if (status == ErrorSuppression::NormalizationFailed)
                        reportError(location, NormalizationTooComplex{});
                    if (kind == RefinementsOpKind::Refine)
                        ty = flushConstraints(kind, ty, discriminants);
                    kind = RefinementsOpKind::Intersect;

                    discriminants.push_back(dt);

                    if (status == ErrorSuppression::Suppress)
                    {
                        ty = flushConstraints(kind, ty, discriminants);
                        ty = makeUnion(scope, location, ty, builtinTypes->errorType);
                    }
                }
            }

            // Finalize - if there are any discriminants left, make one big constraint for refining them
            if (kind != RefinementsOpKind::None)
                ty = flushConstraints(kind, ty, discriminants);

            scope->rvalueRefinements[def] = ty;
        }
    }

    for (auto& c : constraints)
        addConstraint(scope, location, c);
}

void ConstraintGenerator::checkAliases(const ScopePtr& scope, AstStatBlock* block)
{
    std::unordered_map<Name, Location> aliasDefinitionLocations;
    std::unordered_map<Name, Location> classDefinitionLocations;

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

            // A type alias might have no name if the code is syntactically
            // illegal. We mustn't prepopulate anything in this case.
            if (alias->name == kParseNameError || alias->name == "typeof")
                continue;

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
        else if (auto function = stat->as<AstStatTypeFunction>())
        {
            // If a type function w/ same name has already been defined, error for having duplicates
            if (scope->exportedTypeBindings.count(function->name.value) || scope->privateTypeBindings.count(function->name.value))
            {
                auto it = aliasDefinitionLocations.find(function->name.value);
                LUAU_ASSERT(it != aliasDefinitionLocations.end());
                reportError(function->location, DuplicateTypeDefinition{function->name.value, it->second});
                continue;
            }

            if (!FFlag::LuauUserTypeFunExportedAndLocal && scope->parent != globalScope)
            {
                reportError(function->location, GenericError{"Local user-defined functions are not supported yet"});
                continue;
            }

            ScopePtr defnScope = childScope(function, scope);

            // Create TypeFunctionInstanceType

            std::vector<TypeId> typeParams;
            typeParams.reserve(function->body->args.size);

            std::vector<GenericTypeDefinition> quantifiedTypeParams;
            quantifiedTypeParams.reserve(function->body->args.size);

            for (size_t i = 0; i < function->body->args.size; i++)
            {
                std::string name = format("T%zu", i);
                TypeId ty = arena->addType(GenericType{name});
                typeParams.push_back(ty);

                GenericTypeDefinition genericTy{ty};
                quantifiedTypeParams.push_back(genericTy);
            }

            if (std::optional<std::string> error = typeFunctionRuntime->registerFunction(function))
                reportError(function->location, GenericError{*error});

            UserDefinedFunctionData udtfData;

            if (FFlag::LuauUserTypeFunExportedAndLocal)
            {
                udtfData.owner = module;
                udtfData.definition = function;
            }

            TypeId typeFunctionTy = arena->addType(
                TypeFunctionInstanceType{NotNull{&builtinTypeFunctions().userFunc}, std::move(typeParams), {}, function->name, udtfData}
            );

            TypeFun typeFunction{std::move(quantifiedTypeParams), typeFunctionTy};

            // Set type bindings and definition locations for this user-defined type function
            if (FFlag::LuauUserTypeFunExportedAndLocal && function->exported)
                scope->exportedTypeBindings[function->name.value] = std::move(typeFunction);
            else
                scope->privateTypeBindings[function->name.value] = std::move(typeFunction);

            aliasDefinitionLocations[function->name.value] = function->location;
        }
        else if (auto classDeclaration = stat->as<AstStatDeclareClass>())
        {
            if (!FFlag::LuauNewSolverPrePopulateClasses)
                continue;

            if (scope->exportedTypeBindings.count(classDeclaration->name.value))
            {
                auto it = classDefinitionLocations.find(classDeclaration->name.value);
                LUAU_ASSERT(it != classDefinitionLocations.end());
                reportError(classDeclaration->location, DuplicateTypeDefinition{classDeclaration->name.value, it->second});
                continue;
            }

            // A class might have no name if the code is syntactically
            // illegal. We mustn't prepopulate anything in this case.
            if (classDeclaration->name == kParseNameError)
                continue;

            ScopePtr defnScope = childScope(classDeclaration, scope);

            TypeId initialType = arena->addType(BlockedType{});
            TypeFun initialFun{initialType};
            scope->exportedTypeBindings[classDeclaration->name.value] = std::move(initialFun);

            classDefinitionLocations[classDeclaration->name.value] = classDeclaration->location;
        }
    }

    if (FFlag::LuauUserTypeFunExportedAndLocal)
    {
        // Additional pass for user-defined type functions to fill in their environments completely
        for (AstStat* stat : block->body)
        {
            if (auto function = stat->as<AstStatTypeFunction>())
            {
                // Find the type function we have already created
                TypeFunctionInstanceType* mainTypeFun = nullptr;

                if (auto it = scope->privateTypeBindings.find(function->name.value); it != scope->privateTypeBindings.end())
                    mainTypeFun = getMutable<TypeFunctionInstanceType>(it->second.type);

                if (!mainTypeFun)
                {
                    if (auto it = scope->exportedTypeBindings.find(function->name.value); it != scope->exportedTypeBindings.end())
                        mainTypeFun = getMutable<TypeFunctionInstanceType>(it->second.type);
                }

                // Fill it with all visible type functions
                if (mainTypeFun)
                {
                    UserDefinedFunctionData& userFuncData = mainTypeFun->userFuncData;

                    for (Scope* curr = scope.get(); curr; curr = curr->parent.get())
                    {
                        for (auto& [name, tf] : curr->privateTypeBindings)
                        {
                            if (userFuncData.environment.find(name))
                                continue;

                            if (auto ty = get<TypeFunctionInstanceType>(tf.type); ty && ty->userFuncData.definition)
                                userFuncData.environment[name] = ty->userFuncData.definition;
                        }

                        for (auto& [name, tf] : curr->exportedTypeBindings)
                        {
                            if (userFuncData.environment.find(name))
                                continue;

                            if (auto ty = get<TypeFunctionInstanceType>(tf.type); ty && ty->userFuncData.definition)
                                userFuncData.environment[name] = ty->userFuncData.definition;
                        }
                    }
                }
            }
        }
    }
}

ControlFlow ConstraintGenerator::visitBlockWithoutChildScope(const ScopePtr& scope, AstStatBlock* block)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportCodeTooComplex(block->location);
        return ControlFlow::None;
    }

    checkAliases(scope, block);

    std::optional<ControlFlow> firstControlFlow;
    for (AstStat* stat : block->body)
    {
        ControlFlow cf = visit(scope, stat);
        if (cf != ControlFlow::None && !firstControlFlow)
            firstControlFlow = cf;
    }

    return firstControlFlow.value_or(ControlFlow::None);
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStat* stat)
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
    else if (stat->is<AstStatBreak>())
        return ControlFlow::Breaks;
    else if (stat->is<AstStatContinue>())
        return ControlFlow::Continues;
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
    else if (auto f = stat->as<AstStatTypeFunction>())
        return visit(scope, f);
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

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatLocal* statLocal)
{
    std::vector<TypeId> annotatedTypes;
    annotatedTypes.reserve(statLocal->vars.size);
    bool hasAnnotation = false;

    std::vector<std::optional<TypeId>> expectedTypes;
    expectedTypes.reserve(statLocal->vars.size);

    std::vector<TypeId> assignees;
    assignees.reserve(statLocal->vars.size);

    // Used to name the first value type, even if it's not placed in varTypes,
    // for the purpose of synthetic name attribution.
    std::optional<TypeId> firstValueType;

    for (AstLocal* local : statLocal->vars)
    {
        const Location location = local->location;

        TypeId assignee = arena->addType(BlockedType{});
        localTypes.try_insert(assignee, {});

        assignees.push_back(assignee);

        if (!firstValueType)
            firstValueType = assignee;

        if (local->annotation)
        {
            hasAnnotation = true;
            TypeId annotationTy = resolveType(scope, local->annotation, /* inTypeArguments */ false);
            annotatedTypes.push_back(annotationTy);
            expectedTypes.push_back(annotationTy);

            scope->bindings[local] = Binding{annotationTy, location};
        }
        else
        {
            // annotatedTypes must contain one type per local.  If a particular
            // local has no annotation at, assume the most conservative thing.
            annotatedTypes.push_back(builtinTypes->unknownType);

            expectedTypes.push_back(std::nullopt);
            scope->bindings[local] = Binding{builtinTypes->unknownType, location};

            inferredBindings[local] = {scope.get(), location, {assignee}};
        }

        DefId def = dfg->getDef(local);
        scope->lvalueTypes[def] = assignee;
    }

    Checkpoint start = checkpoint(this);
    TypePackId rvaluePack = checkPack(scope, statLocal->values, expectedTypes).tp;
    Checkpoint end = checkpoint(this);

    if (hasAnnotation)
    {
        for (size_t i = 0; i < statLocal->vars.size; ++i)
        {
            LUAU_ASSERT(get<BlockedType>(assignees[i]));
            TypeIds* localDomain = localTypes.find(assignees[i]);
            LUAU_ASSERT(localDomain);
            localDomain->insert(annotatedTypes[i]);
        }

        TypePackId annotatedPack = arena->addTypePack(std::move(annotatedTypes));
        addConstraint(scope, statLocal->location, PackSubtypeConstraint{rvaluePack, annotatedPack});
    }
    else
    {
        std::vector<TypeId> valueTypes;
        valueTypes.reserve(statLocal->vars.size);

        auto [head, tail] = flatten(rvaluePack);

        if (head.size() >= statLocal->vars.size)
        {
            for (size_t i = 0; i < statLocal->vars.size; ++i)
                valueTypes.push_back(head[i]);
        }
        else
        {
            for (size_t i = 0; i < statLocal->vars.size; ++i)
                valueTypes.push_back(arena->addType(BlockedType{}));

            auto uc = addConstraint(scope, statLocal->location, UnpackConstraint{valueTypes, rvaluePack});

            forEachConstraint(
                start,
                end,
                this,
                [&uc](const ConstraintPtr& runBefore)
                {
                    uc->dependencies.push_back(NotNull{runBefore.get()});
                }
            );

            for (TypeId t : valueTypes)
                getMutable<BlockedType>(t)->setOwner(uc);
        }

        for (size_t i = 0; i < statLocal->vars.size; ++i)
        {
            LUAU_ASSERT(get<BlockedType>(assignees[i]));
            TypeIds* localDomain = localTypes.find(assignees[i]);
            LUAU_ASSERT(localDomain);
            localDomain->insert(valueTypes[i]);
        }
    }

    if (statLocal->vars.size == 1 && statLocal->values.size == 1 && firstValueType && scope.get() == rootScope && !hasAnnotation)
    {
        AstLocal* var = statLocal->vars.data[0];
        AstExpr* value = statLocal->values.data[0];

        if (value->is<AstExprTable>())
            addConstraint(scope, value->location, NameConstraint{*firstValueType, var->name.value, /*synthetic*/ true});
        else if (const AstExprCall* call = value->as<AstExprCall>())
        {
            if (FFlag::LuauTypestateBuiltins2)
            {
                if (matchSetMetatable(*call))
                    addConstraint(scope, value->location, NameConstraint{*firstValueType, var->name.value, /*synthetic*/ true});
            }
            else
            {
                if (const AstExprGlobal* global = call->func->as<AstExprGlobal>(); global && global->name == "setmetatable")
                {
                    addConstraint(scope, value->location, NameConstraint{*firstValueType, var->name.value, /*synthetic*/ true});
                }
            }
        }
    }

    if (statLocal->values.size > 0)
    {
        // To correctly handle 'require', we need to import the exported type bindings into the variable 'namespace'.
        for (size_t i = 0; i < statLocal->values.size && i < statLocal->vars.size; ++i)
        {
            const AstExprCall* call = statLocal->values.data[i]->as<AstExprCall>();
            if (!call)
                continue;

            auto maybeRequire = matchRequire(*call);
            if (!maybeRequire)
                continue;

            AstExpr* require = *maybeRequire;

            auto moduleInfo = moduleResolver->resolveModuleInfo(module->name, *require);
            if (!moduleInfo)
                continue;

            ModulePtr module = moduleResolver->getModule(moduleInfo->name);
            if (!module)
                continue;

            const Name name{statLocal->vars.data[i]->name.value};
            scope->importedTypeBindings[name] = module->exportedTypeBindings;
            scope->importedModules[name] = moduleInfo->name;

            // Imported types of requires that transitively refer to current module have to be replaced with 'any'
            for (const auto& [location, path] : requireCycles)
            {
                if (path.empty() || path.front() != moduleInfo->name)
                    continue;

                for (auto& [name, tf] : scope->importedTypeBindings[name])
                    tf = TypeFun{{}, {}, builtinTypes->anyType};
            }
        }
    }

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatFor* for_)
{
    TypeId annotationTy = builtinTypes->numberType;
    if (for_->var->annotation)
        annotationTy = resolveType(scope, for_->var->annotation, /* inTypeArguments */ false);

    auto inferNumber = [&](AstExpr* expr)
    {
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

    DefId def = dfg->getDef(for_->var);
    forScope->lvalueTypes[def] = annotationTy;
    forScope->rvalueRefinements[def] = annotationTy;

    visit(forScope, for_->body);

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatForIn* forIn)
{
    ScopePtr loopScope = childScope(forIn, scope);
    TypePackId iterator = checkPack(scope, forIn->values).tp;

    std::vector<TypeId> variableTypes;
    variableTypes.reserve(forIn->vars.size);

    for (AstLocal* var : forIn->vars)
    {
        TypeId assignee = arena->addType(BlockedType{});
        variableTypes.push_back(assignee);

        TypeId loopVar = arena->addType(BlockedType{});
        localTypes[loopVar].insert(assignee);

        if (var->annotation)
        {
            TypeId annotationTy = resolveType(loopScope, var->annotation, /*inTypeArguments*/ false);
            loopScope->bindings[var] = Binding{annotationTy, var->location};
            addConstraint(scope, var->location, SubtypeConstraint{loopVar, annotationTy});
        }
        else
            loopScope->bindings[var] = Binding{loopVar, var->location};

        DefId def = dfg->getDef(var);
        loopScope->lvalueTypes[def] = loopVar;
    }

    auto iterable = addConstraint(
        loopScope, getLocation(forIn->values), IterableConstraint{iterator, variableTypes, forIn->values.data[0], &module->astForInNextTypes}
    );

    for (TypeId var : variableTypes)
    {
        auto bt = getMutable<BlockedType>(var);
        LUAU_ASSERT(bt);
        bt->setOwner(iterable);
    }

    Checkpoint start = checkpoint(this);
    visit(loopScope, forIn->body);
    Checkpoint end = checkpoint(this);

    // This iter constraint must dispatch first.
    forEachConstraint(
        start,
        end,
        this,
        [&iterable](const ConstraintPtr& runLater)
        {
            runLater->dependencies.push_back(iterable);
        }
    );

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatWhile* while_)
{
    RefinementId refinement = check(scope, while_->condition).refinement;

    ScopePtr whileScope = childScope(while_, scope);
    applyRefinements(whileScope, while_->condition->location, refinement);

    visit(whileScope, while_->body);

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatRepeat* repeat)
{
    ScopePtr repeatScope = childScope(repeat, scope);

    visitBlockWithoutChildScope(repeatScope, repeat->body);

    check(repeatScope, repeat->condition);

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatLocalFunction* function)
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
    sig.bodyScope->bindings[function->name] = Binding{sig.signature, function->name->location};

    bool sigFullyDefined = !hasFreeType(sig.signature);
    if (sigFullyDefined)
        emplaceType<BoundType>(asMutable(functionType), sig.signature);

    DefId def = dfg->getDef(function->name);
    scope->lvalueTypes[def] = functionType;
    scope->rvalueRefinements[def] = functionType;
    sig.bodyScope->lvalueTypes[def] = sig.signature;
    sig.bodyScope->rvalueRefinements[def] = sig.signature;

    Checkpoint start = checkpoint(this);
    checkFunctionBody(sig.bodyScope, function->func);
    Checkpoint end = checkpoint(this);

    if (!sigFullyDefined)
    {
        NotNull<Scope> constraintScope{sig.signatureScope ? sig.signatureScope.get() : sig.bodyScope.get()};
        std::unique_ptr<Constraint> c =
            std::make_unique<Constraint>(constraintScope, function->name->location, GeneralizationConstraint{functionType, sig.signature});

        Constraint* previous = nullptr;
        forEachConstraint(
            start,
            end,
            this,
            [&c, &previous](const ConstraintPtr& constraint)
            {
                c->dependencies.push_back(NotNull{constraint.get()});

                if (auto psc = get<PackSubtypeConstraint>(*constraint); psc && psc->returns)
                {
                    if (previous)
                        constraint->dependencies.push_back(NotNull{previous});

                    previous = constraint.get();
                }
            }
        );

        getMutable<BlockedType>(functionType)->setOwner(addConstraint(scope, std::move(c)));
        module->astTypes[function->func] = functionType;
    }
    else
        module->astTypes[function->func] = sig.signature;

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatFunction* function)
{
    // Name could be AstStatLocal, AstStatGlobal, AstStatIndexName.
    // With or without self

    Checkpoint start = checkpoint(this);
    FunctionSignature sig = checkFunctionSignature(scope, function->func, /* expectedType */ std::nullopt, function->name->location);
    bool sigFullyDefined = !hasFreeType(sig.signature);

    checkFunctionBody(sig.bodyScope, function->func);
    Checkpoint end = checkpoint(this);

    TypeId generalizedType = arena->addType(BlockedType{});
    if (sigFullyDefined)
        emplaceType<BoundType>(asMutable(generalizedType), sig.signature);
    else
    {
        const ScopePtr& constraintScope = sig.signatureScope ? sig.signatureScope : sig.bodyScope;

        NotNull<Constraint> c = addConstraint(constraintScope, function->name->location, GeneralizationConstraint{generalizedType, sig.signature});
        getMutable<BlockedType>(generalizedType)->setOwner(c);

        Constraint* previous = nullptr;
        forEachConstraint(
            start,
            end,
            this,
            [&c, &previous](const ConstraintPtr& constraint)
            {
                c->dependencies.push_back(NotNull{constraint.get()});

                if (auto psc = get<PackSubtypeConstraint>(*constraint); psc && psc->returns)
                {
                    if (previous)
                        constraint->dependencies.push_back(NotNull{previous});

                    previous = constraint.get();
                }
            }
        );
    }

    DefId def = dfg->getDef(function->name);
    std::optional<TypeId> existingFunctionTy = follow(lookup(scope, function->name->location, def));

    if (AstExprLocal* localName = function->name->as<AstExprLocal>())
    {
        visitLValue(scope, localName, generalizedType);

        scope->bindings[localName->local] = Binding{sig.signature, localName->location};
        scope->lvalueTypes[def] = sig.signature;
    }
    else if (AstExprGlobal* globalName = function->name->as<AstExprGlobal>())
    {
        if (!existingFunctionTy)
            ice->ice("prepopulateGlobalScope did not populate a global name", globalName->location);

        // Sketchy: We're specifically looking for BlockedTypes that were
        // initially created by ConstraintGenerator::prepopulateGlobalScope.
        if (auto bt = get<BlockedType>(*existingFunctionTy); bt && nullptr == bt->getOwner())
            emplaceType<BoundType>(asMutable(*existingFunctionTy), generalizedType);

        scope->bindings[globalName->name] = Binding{sig.signature, globalName->location};
        scope->lvalueTypes[def] = sig.signature;
    }
    else if (AstExprIndexName* indexName = function->name->as<AstExprIndexName>())
    {
        visitLValue(scope, indexName, generalizedType);
    }
    else if (AstExprError* err = function->name->as<AstExprError>())
    {
        generalizedType = builtinTypes->errorRecoveryType();
    }

    if (generalizedType == nullptr)
        ice->ice("generalizedType == nullptr", function->location);

    scope->rvalueRefinements[def] = generalizedType;

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatReturn* ret)
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

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatBlock* block)
{
    ScopePtr innerScope = childScope(block, scope);

    ControlFlow flow = visitBlockWithoutChildScope(innerScope, block);

    // An AstStatBlock has linear control flow, i.e. one entry and one exit, so we can inherit
    // all the changes to the environment occurred by the statements in that block.
    scope->inheritRefinements(innerScope);
    scope->inheritAssignments(innerScope);

    return flow;
}

// TODO Clip?
static void bindFreeType(TypeId a, TypeId b)
{
    FreeType* af = getMutable<FreeType>(a);
    FreeType* bf = getMutable<FreeType>(b);

    LUAU_ASSERT(af || bf);

    if (!bf)
        emplaceType<BoundType>(asMutable(a), b);
    else if (!af)
        emplaceType<BoundType>(asMutable(b), a);
    else if (subsumes(bf->scope, af->scope))
        emplaceType<BoundType>(asMutable(a), b);
    else if (subsumes(af->scope, bf->scope))
        emplaceType<BoundType>(asMutable(b), a);
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatAssign* assign)
{
    TypePackId resultPack = checkPack(scope, assign->values).tp;

    std::vector<TypeId> valueTypes;
    valueTypes.reserve(assign->vars.size);

    auto [head, tail] = flatten(resultPack);
    if (head.size() >= assign->vars.size)
    {
        // If the resultPack is definitely long enough for each variable, we can
        // skip the UnpackConstraint and use the result types directly.

        for (size_t i = 0; i < assign->vars.size; ++i)
            valueTypes.push_back(head[i]);
    }
    else
    {
        // We're not sure how many types are produced by the right-side
        // expressions.  We'll use an UnpackConstraint to defer this until
        // later.
        for (size_t i = 0; i < assign->vars.size; ++i)
            valueTypes.push_back(arena->addType(BlockedType{}));

        auto uc = addConstraint(scope, assign->location, UnpackConstraint{valueTypes, resultPack});

        for (TypeId t : valueTypes)
            getMutable<BlockedType>(t)->setOwner(uc);
    }

    for (size_t i = 0; i < assign->vars.size; ++i)
    {
        visitLValue(scope, assign->vars.data[i], valueTypes[i]);
    }

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatCompoundAssign* assign)
{
    TypeId resultTy = checkAstExprBinary(scope, assign->location, assign->op, assign->var, assign->value, std::nullopt).ty;
    module->astCompoundAssignResultTypes[assign] = resultTy;

    TypeId lhsType = check(scope, assign->var).ty;
    visitLValue(scope, assign->var, lhsType);

    follow(lhsType);
    follow(resultTy);

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatIf* ifStatement)
{
    RefinementId refinement = [&]()
    {
        InConditionalContext flipper{&typeContext};
        return check(scope, ifStatement->condition, std::nullopt).refinement;
    }();

    ScopePtr thenScope = childScope(ifStatement->thenbody, scope);
    applyRefinements(thenScope, ifStatement->condition->location, refinement);

    ScopePtr elseScope = childScope(ifStatement->elsebody ? ifStatement->elsebody : ifStatement, scope);
    applyRefinements(elseScope, ifStatement->elseLocation.value_or(ifStatement->condition->location), refinementArena.negation(refinement));

    ControlFlow thencf = visit(thenScope, ifStatement->thenbody);
    ControlFlow elsecf = ControlFlow::None;
    if (ifStatement->elsebody)
        elsecf = visit(elseScope, ifStatement->elsebody);

    if (thencf != ControlFlow::None && elsecf == ControlFlow::None)
        scope->inheritRefinements(elseScope);
    else if (thencf == ControlFlow::None && elsecf != ControlFlow::None)
        scope->inheritRefinements(thenScope);

    if (thencf == ControlFlow::None)
        scope->inheritAssignments(thenScope);
    if (elsecf == ControlFlow::None)
        scope->inheritAssignments(elseScope);

    if (thencf == elsecf)
        return thencf;
    else if (matches(thencf, ControlFlow::Returns | ControlFlow::Throws) && matches(elsecf, ControlFlow::Returns | ControlFlow::Throws))
        return ControlFlow::Returns;
    else
        return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatTypeAlias* alias)
{
    if (alias->name == kParseNameError)
        return ControlFlow::None;

    if (alias->name == "typeof")
    {
        reportError(alias->location, GenericError{"Type aliases cannot be named typeof"});
        return ControlFlow::None;
    }

    scope->typeAliasLocations[alias->name.value] = alias->location;
    scope->typeAliasNameLocations[alias->name.value] = alias->nameLocation;

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

    TypeId ty = resolveType(*defnScope, alias->type, /* inTypeArguments */ false, /* replaceErrorWithFresh */ false);

    TypeId aliasTy = bindingIt->second.type;
    LUAU_ASSERT(get<BlockedType>(aliasTy));
    if (occursCheck(aliasTy, ty))
    {
        emplaceType<BoundType>(asMutable(aliasTy), builtinTypes->anyType);
        reportError(alias->nameLocation, OccursCheckFailed{});
    }
    else
        emplaceType<BoundType>(asMutable(aliasTy), ty);

    std::vector<TypeId> typeParams;
    for (auto tyParam : createGenerics(*defnScope, alias->generics, /* useCache */ true, /* addTypes */ false))
        typeParams.push_back(tyParam.second.ty);

    std::vector<TypePackId> typePackParams;
    for (auto tpParam : createGenericPacks(*defnScope, alias->genericPacks, /* useCache */ true, /* addTypes */ false))
        typePackParams.push_back(tpParam.second.tp);

    addConstraint(
        scope,
        alias->type->location,
        NameConstraint{
            ty,
            alias->name.value,
            /*synthetic=*/false,
            std::move(typeParams),
            std::move(typePackParams),
        }
    );

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatTypeFunction* function)
{
    // If a type function with the same name was already defined, we skip over
    auto bindingIt = scope->privateTypeBindings.find(function->name.value);
    if (bindingIt == scope->privateTypeBindings.end())
        return ControlFlow::None;

    TypeFun typeFunction = bindingIt->second;

    // Adding typeAliasExpansionConstraint on user-defined type function for the constraint solver
    if (auto typeFunctionTy = get<TypeFunctionInstanceType>(follow(typeFunction.type)))
    {
        TypeId expansionTy = arena->addType(PendingExpansionType{{}, function->name, typeFunctionTy->typeArguments, typeFunctionTy->packArguments});
        addConstraint(scope, function->location, TypeAliasExpansionConstraint{/* target */ expansionTy});
    }

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatDeclareGlobal* global)
{
    LUAU_ASSERT(global->type);

    TypeId globalTy = resolveType(scope, global->type, /* inTypeArguments */ false);
    Name globalName(global->name.value);

    module->declaredGlobals[globalName] = globalTy;
    rootScope->bindings[global->name] = Binding{globalTy, global->location};

    DefId def = dfg->getDef(global);
    rootScope->lvalueTypes[def] = globalTy;
    rootScope->rvalueRefinements[def] = globalTy;

    return ControlFlow::None;
}

static bool isMetamethod(const Name& name)
{
    return name == "__index" || name == "__newindex" || name == "__call" || name == "__concat" || name == "__unm" || name == "__add" ||
           name == "__sub" || name == "__mul" || name == "__div" || name == "__mod" || name == "__pow" || name == "__tostring" ||
           name == "__metatable" || name == "__eq" || name == "__lt" || name == "__le" || name == "__mode" || name == "__iter" || name == "__len" ||
           name == "__idiv";
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatDeclareClass* declaredClass)
{
    // If a class with the same name was already defined, we skip over
    auto bindingIt = scope->exportedTypeBindings.find(declaredClass->name.value);
    if (FFlag::LuauNewSolverPrePopulateClasses && bindingIt == scope->exportedTypeBindings.end())
        return ControlFlow::None;

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
        if (FFlag::LuauNewSolverPrePopulateClasses)
            superTy = follow(lookupType->type);
        else
            superTy = lookupType->type;

        if (!get<ClassType>(follow(*superTy)))
        {
            reportError(
                declaredClass->location,
                GenericError{format("Cannot use non-class type '%s' as a superclass of class '%s'", superName.c_str(), declaredClass->name.value)}
            );

            return ControlFlow::None;
        }
    }

    Name className(declaredClass->name.value);

    TypeId classTy = arena->addType(ClassType(className, {}, superTy, std::nullopt, {}, {}, module->name, declaredClass->location));
    ClassType* ctv = getMutable<ClassType>(classTy);

    TypeId metaTy = arena->addType(TableType{TableState::Sealed, scope->level, scope.get()});
    TableType* metatable = getMutable<TableType>(metaTy);

    ctv->metatable = metaTy;


    if (FFlag::LuauNewSolverPrePopulateClasses)
    {
        TypeId classBindTy = bindingIt->second.type;
        emplaceType<BoundType>(asMutable(classBindTy), classTy);
    }
    else
        scope->exportedTypeBindings[className] = TypeFun{{}, classTy};

    if (declaredClass->indexer)
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
                ftv->argTypes = addTypePack({classTy}, ftv->argTypes);

                ftv->hasSelf = true;

                FunctionDefinition defn;

                defn.definitionModuleName = module->name;
                defn.definitionLocation = prop.location;
                // No data is preserved for varargLocation
                defn.originalNameLocation = prop.nameLocation;

                ftv->definition = defn;
            }
        }

        TableType::Props& props = assignToMetatable ? metatable->props : ctv->props;

        if (props.count(propName) == 0)
        {
            props[propName] = {propTy, /*deprecated*/ false, /*deprecatedSuggestion*/ "", prop.location};
        }
        else
        {
            Luau::Property& prop = props[propName];
            TypeId currentTy = prop.type();

            // We special-case this logic to keep the intersection flat; otherwise we
            // would create a ton of nested intersection types.
            if (const IntersectionType* itv = get<IntersectionType>(currentTy))
            {
                std::vector<TypeId> options = itv->parts;
                options.push_back(propTy);
                TypeId newItv = arena->addType(IntersectionType{std::move(options)});

                prop.readTy = newItv;
                prop.writeTy = newItv;
            }
            else if (get<FunctionType>(currentTy))
            {
                TypeId intersection = arena->addType(IntersectionType{{currentTy, propTy}});

                prop.readTy = intersection;
                prop.writeTy = intersection;
            }
            else
            {
                reportError(declaredClass->location, GenericError{format("Cannot overload non-function class member '%s'", propName.c_str())});
            }
        }
    }

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatDeclareFunction* global)
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

    FunctionDefinition defn;

    defn.definitionModuleName = module->name;
    defn.definitionLocation = global->location;
    defn.varargLocation = global->vararg ? std::make_optional(global->varargLocation) : std::nullopt;
    defn.originalNameLocation = global->nameLocation;

    TypeId fnType = arena->addType(FunctionType{TypeLevel{}, funScope.get(), std::move(genericTys), std::move(genericTps), paramPack, retPack, defn});
    FunctionType* ftv = getMutable<FunctionType>(fnType);
    ftv->isCheckedFunction = global->isCheckedFunction();

    ftv->argNames.reserve(global->paramNames.size);
    for (const auto& el : global->paramNames)
        ftv->argNames.push_back(FunctionArgument{el.first.value, el.second});

    Name fnName(global->name.value);

    module->declaredGlobals[fnName] = fnType;
    scope->bindings[global->name] = Binding{fnType, global->location};

    DefId def = dfg->getDef(global);
    rootScope->lvalueTypes[def] = fnType;
    rootScope->rvalueRefinements[def] = fnType;

    return ControlFlow::None;
}

ControlFlow ConstraintGenerator::visit(const ScopePtr& scope, AstStatError* error)
{
    for (AstStat* stat : error->statements)
        visit(scope, stat);
    for (AstExpr* expr : error->expressions)
        check(scope, expr);

    return ControlFlow::None;
}

InferencePack ConstraintGenerator::checkPack(const ScopePtr& scope, AstArray<AstExpr*> exprs, const std::vector<std::optional<TypeId>>& expectedTypes)
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
            head.push_back(check(scope, expr, expectedType).ty);
        }
        else
        {
            std::vector<std::optional<TypeId>> expectedTailTypes;
            if (i < expectedTypes.size())
                expectedTailTypes.assign(begin(expectedTypes) + i, end(expectedTypes));
            tail = checkPack(scope, expr, expectedTailTypes).tp;
        }
    }

    return InferencePack{addTypePack(std::move(head), tail)};
}

InferencePack ConstraintGenerator::checkPack(
    const ScopePtr& scope,
    AstExpr* expr,
    const std::vector<std::optional<TypeId>>& expectedTypes,
    bool generalize
)
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
        TypeId t = check(scope, expr, expectedType, /*forceSingletons*/ false, generalize).ty;
        result = InferencePack{arena->addTypePack({t})};
    }

    LUAU_ASSERT(result.tp);
    module->astTypePacks[expr] = result.tp;
    return result;
}

InferencePack ConstraintGenerator::checkPack(const ScopePtr& scope, AstExprCall* call)
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

        if (auto key = dfg->getRefinementKey(indexExpr->expr))
        {
            TypeId discriminantTy = arena->addType(BlockedType{});
            returnRefinements.push_back(refinementArena.proposition(key, discriminantTy));
            discriminantTypes.push_back(discriminantTy);
        }
        else
            discriminantTypes.push_back(std::nullopt);
    }

    for (AstExpr* arg : call->args)
    {
        exprArgs.push_back(arg);

        if (auto key = dfg->getRefinementKey(arg))
        {
            TypeId discriminantTy = arena->addType(BlockedType{});
            returnRefinements.push_back(refinementArena.proposition(key, discriminantTy));
            discriminantTypes.push_back(discriminantTy);
        }
        else
            discriminantTypes.push_back(std::nullopt);
    }

    Checkpoint funcBeginCheckpoint = checkpoint(this);

    TypeId fnType = check(scope, call->func).ty;

    Checkpoint funcEndCheckpoint = checkpoint(this);

    std::vector<std::optional<TypeId>> expectedTypesForCall = getExpectedCallTypesForFunctionOverloads(fnType);

    module->astOriginalCallTypes[call->func] = fnType;

    Checkpoint argBeginCheckpoint = checkpoint(this);

    std::vector<TypeId> args;
    std::optional<TypePackId> argTail;
    std::vector<RefinementId> argumentRefinements;

    for (size_t i = 0; i < exprArgs.size(); ++i)
    {
        AstExpr* arg = exprArgs[i];

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
            auto [ty, refinement] = check(scope, arg, /*expectedType*/ std::nullopt, /*forceSingleton*/ false, /*generalize*/ false);
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

    if (matchSetMetatable(*call))
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

            auto c = addConstraint(scope, call->location, UnpackConstraint{unpackedTypes, *argTail});
            getMutable<BlockedType>(mt)->setOwner(c);
            if (auto b = getMutable<BlockedType>(target); b && b->getOwner() == nullptr)
                b->setOwner(c);
        }

        LUAU_ASSERT(target);
        LUAU_ASSERT(mt);

        target = follow(target);

        AstExpr* targetExpr = call->args.data[0];

        TypeId resultTy = nullptr;

        if (isTableUnion(target))
        {
            const UnionType* targetUnion = get<UnionType>(target);
            std::vector<TypeId> newParts;

            for (TypeId ty : targetUnion)
                newParts.push_back(arena->addType(MetatableType{ty, mt}));

            resultTy = arena->addType(UnionType{std::move(newParts)});
        }
        else
            resultTy = arena->addType(MetatableType{target, mt});

        if (AstExprLocal* targetLocal = targetExpr->as<AstExprLocal>())
        {
            scope->bindings[targetLocal->local].typeId = resultTy;

            DefId def = dfg->getDef(targetLocal);
            scope->lvalueTypes[def] = resultTy;       // TODO: typestates: track this as an assignment
            scope->rvalueRefinements[def] = resultTy; // TODO: typestates: track this as an assignment

            // HACK: If we have a targetLocal, it has already been added to the
            // inferredBindings table.  We want to replace it so that we don't
            // infer a weird union like tbl | { @metatable something, tbl }
            if (InferredBinding* ib = inferredBindings.find(targetLocal->local))
                ib->types.erase(target);

            recordInferredBinding(targetLocal->local, resultTy);
        }

        return InferencePack{arena->addTypePack({resultTy}), {refinementArena.variadic(returnRefinements)}};
    }

    if (FFlag::LuauTypestateBuiltins2 && shouldTypestateForFirstArgument(*call) && call->args.size > 0 && isLValue(call->args.data[0]))
    {
        AstExpr* targetExpr = call->args.data[0];
        auto resultTy = arena->addType(BlockedType{});

        if (auto def = dfg->getDefOptional(targetExpr))
        {
            scope->lvalueTypes[*def] = resultTy;
            scope->rvalueRefinements[*def] = resultTy;
        }
    }

    if (matchAssert(*call) && !argumentRefinements.empty())
        applyRefinements(scope, call->args.data[0]->location, argumentRefinements[0]);

    // TODO: How do expectedTypes play into this?  Do they?
    TypePackId rets = arena->addTypePack(BlockedTypePack{});
    TypePackId argPack = addTypePack(std::move(args), argTail);
    FunctionType ftv(TypeLevel{}, scope.get(), argPack, rets, std::nullopt, call->self);

    /*
     * To make bidirectional type checking work, we need to solve these constraints in a particular order:
     *
     * 1. Solve the function type
     * 2. Propagate type information from the function type to the argument types
     * 3. Solve the argument types
     * 4. Solve the call
     */

    NotNull<Constraint> checkConstraint = addConstraint(
        scope, call->func->location, FunctionCheckConstraint{fnType, argPack, call, NotNull{&module->astTypes}, NotNull{&module->astExpectedTypes}}
    );

    forEachConstraint(
        funcBeginCheckpoint,
        funcEndCheckpoint,
        this,
        [checkConstraint](const ConstraintPtr& constraint)
        {
            checkConstraint->dependencies.emplace_back(constraint.get());
        }
    );

    NotNull<Constraint> callConstraint = addConstraint(
        scope,
        call->func->location,
        FunctionCallConstraint{
            fnType,
            argPack,
            rets,
            call,
            std::move(discriminantTypes),
            &module->astOverloadResolvedTypes,
        }
    );

    getMutable<BlockedTypePack>(rets)->owner = callConstraint.get();

    callConstraint->dependencies.push_back(checkConstraint);

    forEachConstraint(
        argBeginCheckpoint,
        argEndCheckpoint,
        this,
        [checkConstraint, callConstraint](const ConstraintPtr& constraint)
        {
            constraint->dependencies.emplace_back(checkConstraint);

            callConstraint->dependencies.emplace_back(constraint.get());
        }
    );

    return InferencePack{rets, {refinementArena.variadic(returnRefinements)}};
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExpr* expr, std::optional<TypeId> expectedType, bool forceSingleton, bool generalize)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportCodeTooComplex(expr->location);
        return Inference{builtinTypes->errorRecoveryType()};
    }

    Inference result;

    if (auto group = expr->as<AstExprGroup>())
        result = check(scope, group->expr, expectedType, forceSingleton, generalize);
    else if (auto stringExpr = expr->as<AstExprConstantString>())
        result = check(scope, stringExpr, expectedType, forceSingleton);
    else if (expr->is<AstExprConstantNumber>())
        result = Inference{builtinTypes->numberType};
    else if (auto boolExpr = expr->as<AstExprConstantBool>())
        result = check(scope, boolExpr, expectedType, forceSingleton);
    else if (expr->is<AstExprConstantNil>())
        result = Inference{builtinTypes->nilType};
    else if (auto local = expr->as<AstExprLocal>())
        result = check(scope, local);
    else if (auto global = expr->as<AstExprGlobal>())
        result = check(scope, global);
    else if (expr->is<AstExprVarargs>())
        result = flattenPack(scope, expr->location, checkPack(scope, expr));
    else if (auto call = expr->as<AstExprCall>())
        result = flattenPack(scope, expr->location, checkPack(scope, call)); // TODO: needs predicates too
    else if (auto a = expr->as<AstExprFunction>())
        result = check(scope, a, expectedType, generalize);
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

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprConstantString* string, std::optional<TypeId> expectedType, bool forceSingleton)
{
    if (forceSingleton)
        return Inference{arena->addType(SingletonType{StringSingleton{std::string{string->value.data, string->value.size}}})};

    FreeType ft = FreeType{scope.get()};
    ft.lowerBound = arena->addType(SingletonType{StringSingleton{std::string{string->value.data, string->value.size}}});
    ft.upperBound = builtinTypes->stringType;
    const TypeId freeTy = arena->addType(ft);
    addConstraint(scope, string->location, PrimitiveTypeConstraint{freeTy, expectedType, builtinTypes->stringType});
    return Inference{freeTy};
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprConstantBool* boolExpr, std::optional<TypeId> expectedType, bool forceSingleton)
{
    const TypeId singletonType = boolExpr->value ? builtinTypes->trueType : builtinTypes->falseType;
    if (forceSingleton)
        return Inference{singletonType};

    FreeType ft = FreeType{scope.get()};
    ft.lowerBound = singletonType;
    ft.upperBound = builtinTypes->booleanType;
    const TypeId freeTy = arena->addType(ft);
    addConstraint(scope, boolExpr->location, PrimitiveTypeConstraint{freeTy, expectedType, builtinTypes->booleanType});
    return Inference{freeTy};
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprLocal* local)
{
    const RefinementKey* key = dfg->getRefinementKey(local);
    std::optional<DefId> rvalueDef = dfg->getRValueDefForCompoundAssign(local);
    LUAU_ASSERT(key || rvalueDef);

    std::optional<TypeId> maybeTy;

    // if we have a refinement key, we can look up its type.
    if (key)
        maybeTy = lookup(scope, local->location, key->def);

    // if the current def doesn't have a type, we might be doing a compound assignment
    // and therefore might need to look at the rvalue def instead.
    if (!maybeTy && rvalueDef)
        maybeTy = lookup(scope, local->location, *rvalueDef);

    if (maybeTy)
    {
        TypeId ty = follow(*maybeTy);

        recordInferredBinding(local->local, ty);

        return Inference{ty, refinementArena.proposition(key, builtinTypes->truthyType)};
    }
    else
        ice->ice("CG: AstExprLocal came before its declaration?");
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprGlobal* global)
{
    const RefinementKey* key = dfg->getRefinementKey(global);
    std::optional<DefId> rvalueDef = dfg->getRValueDefForCompoundAssign(global);
    LUAU_ASSERT(key || rvalueDef);

    // we'll use whichever of the two definitions we have here.
    DefId def = key ? key->def : *rvalueDef;

    /* prepopulateGlobalScope() has already added all global functions to the environment by this point, so any
     * global that is not already in-scope is definitely an unknown symbol.
     */
    if (auto ty = lookup(scope, global->location, def, /*prototype=*/false))
    {
        rootScope->lvalueTypes[def] = *ty;
        return Inference{*ty, refinementArena.proposition(key, builtinTypes->truthyType)};
    }
    else
        return Inference{builtinTypes->errorRecoveryType()};
}

Inference ConstraintGenerator::checkIndexName(
    const ScopePtr& scope,
    const RefinementKey* key,
    AstExpr* indexee,
    const std::string& index,
    Location indexLocation
)
{
    TypeId obj = check(scope, indexee).ty;
    TypeId result = nullptr;

    // We optimize away the HasProp constraint in simple cases so that we can
    // reason about updates to unsealed tables more accurately.

    const TableType* tt = getTableType(obj);

    // This is a little bit iffy but I *believe* it is okay because, if the
    // local's domain is going to be extended at all, it will be someplace after
    // the current lexical position within the script.
    if (!tt)
    {
        if (TypeIds* localDomain = localTypes.find(obj); localDomain && 1 == localDomain->size())
            tt = getTableType(*localDomain->begin());
    }

    if (tt)
    {
        auto it = tt->props.find(index);
        if (it != tt->props.end() && it->second.readTy.has_value())
            result = *it->second.readTy;
    }

    if (!result)
    {
        result = arena->addType(BlockedType{});

        auto c = addConstraint(
            scope, indexee->location, HasPropConstraint{result, obj, std::move(index), ValueContext::RValue, inConditional(typeContext)}
        );
        getMutable<BlockedType>(result)->setOwner(c);
    }

    if (key)
    {
        if (auto ty = lookup(scope, indexLocation, key->def))
            return Inference{*ty, refinementArena.proposition(key, builtinTypes->truthyType)};

        scope->rvalueRefinements[key->def] = result;
    }

    if (key)
        return Inference{result, refinementArena.proposition(key, builtinTypes->truthyType)};
    else
        return Inference{result};
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprIndexName* indexName)
{
    const RefinementKey* key = dfg->getRefinementKey(indexName);
    return checkIndexName(scope, key, indexName->expr, indexName->index.value, indexName->indexLocation);
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprIndexExpr* indexExpr)
{
    if (auto constantString = indexExpr->index->as<AstExprConstantString>())
    {
        module->astTypes[indexExpr->index] = builtinTypes->stringType;
        const RefinementKey* key = dfg->getRefinementKey(indexExpr);
        return checkIndexName(scope, key, indexExpr->expr, constantString->value.data, indexExpr->location);
    }

    TypeId obj = check(scope, indexExpr->expr).ty;
    TypeId indexType = check(scope, indexExpr->index).ty;

    TypeId result = arena->addType(BlockedType{});

    const RefinementKey* key = dfg->getRefinementKey(indexExpr);
    if (key)
    {
        if (auto ty = lookup(scope, indexExpr->location, key->def))
            return Inference{*ty, refinementArena.proposition(key, builtinTypes->truthyType)};

        scope->rvalueRefinements[key->def] = result;
    }

    auto c = addConstraint(scope, indexExpr->expr->location, HasIndexerConstraint{result, obj, indexType});
    getMutable<BlockedType>(result)->setOwner(c);

    if (key)
        return Inference{result, refinementArena.proposition(key, builtinTypes->truthyType)};
    else
        return Inference{result};
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprFunction* func, std::optional<TypeId> expectedType, bool generalize)
{
    Checkpoint startCheckpoint = checkpoint(this);
    FunctionSignature sig = checkFunctionSignature(scope, func, expectedType);

    interiorTypes.push_back(std::vector<TypeId>{});
    checkFunctionBody(sig.bodyScope, func);
    Checkpoint endCheckpoint = checkpoint(this);

    TypeId generalizedTy = arena->addType(BlockedType{});
    NotNull<Constraint> gc =
        addConstraint(sig.signatureScope, func->location, GeneralizationConstraint{generalizedTy, sig.signature, std::move(interiorTypes.back())});
    getMutable<BlockedType>(generalizedTy)->setOwner(gc);
    interiorTypes.pop_back();

    Constraint* previous = nullptr;
    forEachConstraint(
        startCheckpoint,
        endCheckpoint,
        this,
        [gc, &previous](const ConstraintPtr& constraint)
        {
            gc->dependencies.emplace_back(constraint.get());

            if (auto psc = get<PackSubtypeConstraint>(*constraint); psc && psc->returns)
            {
                if (previous)
                    constraint->dependencies.push_back(NotNull{previous});

                previous = constraint.get();
            }
        }
    );

    if (generalize && hasFreeType(sig.signature))
    {
        return Inference{generalizedTy};
    }
    else
    {
        return Inference{sig.signature};
    }
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprUnary* unary)
{
    auto [operandType, refinement] = check(scope, unary->expr);

    switch (unary->op)
    {
    case AstExprUnary::Op::Not:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().notFunc, {operandType}, {}, scope, unary->location);
        return Inference{resultType, refinementArena.negation(refinement)};
    }
    case AstExprUnary::Op::Len:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().lenFunc, {operandType}, {}, scope, unary->location);
        return Inference{resultType, refinementArena.negation(refinement)};
    }
    case AstExprUnary::Op::Minus:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().unmFunc, {operandType}, {}, scope, unary->location);
        return Inference{resultType, refinementArena.negation(refinement)};
    }
    default: // msvc can't prove that this is exhaustive.
        LUAU_UNREACHABLE();
    }
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprBinary* binary, std::optional<TypeId> expectedType)
{
    return checkAstExprBinary(scope, binary->location, binary->op, binary->left, binary->right, expectedType);
}

Inference ConstraintGenerator::checkAstExprBinary(
    const ScopePtr& scope,
    const Location& location,
    AstExprBinary::Op op,
    AstExpr* left,
    AstExpr* right,
    std::optional<TypeId> expectedType
)
{
    auto [leftType, rightType, refinement] = checkBinary(scope, op, left, right, expectedType);

    switch (op)
    {
    case AstExprBinary::Op::Add:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().addFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::Sub:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().subFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::Mul:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().mulFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::Div:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().divFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::FloorDiv:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().idivFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::Pow:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().powFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::Mod:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().modFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::Concat:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().concatFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::And:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().andFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::Or:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().orFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::CompareLt:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().ltFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::CompareGe:
    {
        TypeId resultType = createTypeFunctionInstance(
            builtinTypeFunctions().ltFunc,
            {rightType, leftType}, // lua decided that `__ge(a, b)` is instead just `__lt(b, a)`
            {},
            scope,
            location
        );
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::CompareLe:
    {
        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().leFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::CompareGt:
    {
        TypeId resultType = createTypeFunctionInstance(
            builtinTypeFunctions().leFunc,
            {rightType, leftType}, // lua decided that `__gt(a, b)` is instead just `__le(b, a)`
            {},
            scope,
            location
        );
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::CompareEq:
    case AstExprBinary::Op::CompareNe:
    {
        DefId leftDef = dfg->getDef(left);
        DefId rightDef = dfg->getDef(right);
        bool leftSubscripted = containsSubscriptedDefinition(leftDef);
        bool rightSubscripted = containsSubscriptedDefinition(rightDef);

        if (leftSubscripted && rightSubscripted)
        {
            // we cannot add nil in this case because then we will blindly accept comparisons that we should not.
        }
        else if (leftSubscripted)
            leftType = makeUnion(scope, location, leftType, builtinTypes->nilType);
        else if (rightSubscripted)
            rightType = makeUnion(scope, location, rightType, builtinTypes->nilType);

        TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().eqFunc, {leftType, rightType}, {}, scope, location);
        return Inference{resultType, std::move(refinement)};
    }
    case AstExprBinary::Op::Op__Count:
        ice->ice("Op__Count should never be generated in an AST.");
    default: // msvc can't prove that this is exhaustive.
        LUAU_UNREACHABLE();
    }
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprIfElse* ifElse, std::optional<TypeId> expectedType)
{
    RefinementId refinement = [&]()
    {
        InConditionalContext flipper{&typeContext};
        ScopePtr condScope = childScope(ifElse->condition, scope);
        return check(condScope, ifElse->condition).refinement;
    }();

    ScopePtr thenScope = childScope(ifElse->trueExpr, scope);
    applyRefinements(thenScope, ifElse->trueExpr->location, refinement);
    TypeId thenType = check(thenScope, ifElse->trueExpr, expectedType).ty;

    ScopePtr elseScope = childScope(ifElse->falseExpr, scope);
    applyRefinements(elseScope, ifElse->falseExpr->location, refinementArena.negation(refinement));
    TypeId elseType = check(elseScope, ifElse->falseExpr, expectedType).ty;

    return Inference{expectedType ? *expectedType : makeUnion(scope, ifElse->location, thenType, elseType)};
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprTypeAssertion* typeAssert)
{
    check(scope, typeAssert->expr, std::nullopt);
    return Inference{resolveType(scope, typeAssert->annotation, /* inTypeArguments */ false)};
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprInterpString* interpString)
{
    for (AstExpr* expr : interpString->expressions)
        check(scope, expr);

    return Inference{builtinTypes->stringType};
}

std::tuple<TypeId, TypeId, RefinementId> ConstraintGenerator::checkBinary(
    const ScopePtr& scope,
    AstExprBinary::Op op,
    AstExpr* left,
    AstExpr* right,
    std::optional<TypeId> expectedType
)
{
    if (op == AstExprBinary::And)
    {
        std::optional<TypeId> relaxedExpectedLhs;

        if (expectedType)
            relaxedExpectedLhs = arena->addType(UnionType{{builtinTypes->falsyType, *expectedType}});

        auto [leftType, leftRefinement] = check(scope, left, relaxedExpectedLhs);

        ScopePtr rightScope = childScope(right, scope);
        applyRefinements(rightScope, right->location, leftRefinement);
        auto [rightType, rightRefinement] = check(rightScope, right, expectedType);

        return {leftType, rightType, refinementArena.conjunction(leftRefinement, rightRefinement)};
    }
    else if (op == AstExprBinary::Or)
    {
        std::optional<TypeId> relaxedExpectedLhs;

        if (expectedType)
            relaxedExpectedLhs = arena->addType(UnionType{{builtinTypes->falsyType, *expectedType}});

        auto [leftType, leftRefinement] = check(scope, left, relaxedExpectedLhs);

        ScopePtr rightScope = childScope(right, scope);
        applyRefinements(rightScope, right->location, refinementArena.negation(leftRefinement));
        auto [rightType, rightRefinement] = check(rightScope, right, expectedType);

        return {leftType, rightType, refinementArena.disjunction(leftRefinement, rightRefinement)};
    }
    else if (auto typeguard = matchTypeGuard(op, left, right))
    {
        TypeId leftType = check(scope, left).ty;
        TypeId rightType = check(scope, right).ty;

        const RefinementKey* key = dfg->getRefinementKey(typeguard->target);
        if (!key)
            return {leftType, rightType, nullptr};

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
        else if (typeguard->type == "buffer")
            discriminantTy = builtinTypes->bufferType;
        else if (typeguard->type == "table")
            discriminantTy = builtinTypes->tableType;
        else if (typeguard->type == "function")
            discriminantTy = builtinTypes->functionType;
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
            if (auto ctv = get<ClassType>(ty); ctv && (ctv->parent == builtinTypes->classType || hasTag(ty, kTypeofRootTag)))
                discriminantTy = ty;
        }

        RefinementId proposition = refinementArena.proposition(key, discriminantTy);
        if (op == AstExprBinary::CompareEq)
            return {leftType, rightType, proposition};
        else if (op == AstExprBinary::CompareNe)
            return {leftType, rightType, refinementArena.negation(proposition)};
        else
            ice->ice("matchTypeGuard should only return a Some under `==` or `~=`!");
    }
    else if (op == AstExprBinary::CompareEq || op == AstExprBinary::CompareNe)
    {
        // We are checking a binary expression of the form a op b
        // Just because a op b is epxected to return a bool, doesn't mean a, b are expected to be bools too
        TypeId leftType = check(scope, left, {}, true).ty;
        TypeId rightType = check(scope, right, {}, true).ty;

        RefinementId leftRefinement = refinementArena.proposition(dfg->getRefinementKey(left), rightType);
        RefinementId rightRefinement = refinementArena.proposition(dfg->getRefinementKey(right), leftType);

        if (op == AstExprBinary::CompareNe)
        {
            leftRefinement = refinementArena.negation(leftRefinement);
            rightRefinement = refinementArena.negation(rightRefinement);
        }

        return {leftType, rightType, refinementArena.equivalence(leftRefinement, rightRefinement)};
    }
    else
    {
        TypeId leftType = check(scope, left).ty;
        TypeId rightType = check(scope, right).ty;
        return {leftType, rightType, nullptr};
    }
}

void ConstraintGenerator::visitLValue(const ScopePtr& scope, AstExpr* expr, TypeId rhsType)
{
    if (auto e = expr->as<AstExprLocal>())
        visitLValue(scope, e, rhsType);
    else if (auto e = expr->as<AstExprGlobal>())
        visitLValue(scope, e, rhsType);
    else if (auto e = expr->as<AstExprIndexName>())
        visitLValue(scope, e, rhsType);
    else if (auto e = expr->as<AstExprIndexExpr>())
        visitLValue(scope, e, rhsType);
    else if (auto e = expr->as<AstExprError>())
    {
        if (FFlag::LuauNewSolverVisitErrorExprLvalues)
        {
            // If we end up with some sort of error expression in an lvalue
            // position, at least go and check the expressions so that when
            // we visit them later, there aren't any invalid assumptions.
            for (auto subExpr : e->expressions)
            {
                check(scope, subExpr);
            }
        }
    }
    else
        ice->ice("Unexpected lvalue expression", expr->location);
}

void ConstraintGenerator::visitLValue(const ScopePtr& scope, AstExprLocal* local, TypeId rhsType)
{
    std::optional<TypeId> annotatedTy = scope->lookup(local->local);
    LUAU_ASSERT(annotatedTy);

    const DefId defId = dfg->getDef(local);
    std::optional<TypeId> ty = scope->lookupUnrefinedType(defId);

    if (ty)
    {
        TypeIds* localDomain = localTypes.find(*ty);
        if (localDomain)
            localDomain->insert(rhsType);
    }
    else
    {
        ty = arena->addType(BlockedType{});
        localTypes[*ty].insert(rhsType);

        if (annotatedTy)
        {
            switch (shouldSuppressErrors(normalizer, *annotatedTy))
            {
            case ErrorSuppression::DoNotSuppress:
                break;
            case ErrorSuppression::Suppress:
                ty = simplifyUnion(scope, local->location, *ty, builtinTypes->errorType);
                break;
            case ErrorSuppression::NormalizationFailed:
                reportError(local->local->annotation->location, NormalizationTooComplex{});
                break;
            }
        }

        scope->lvalueTypes[defId] = *ty;
    }

    recordInferredBinding(local->local, *ty);

    if (annotatedTy)
        addConstraint(scope, local->location, SubtypeConstraint{rhsType, *annotatedTy});

    if (TypeIds* localDomain = localTypes.find(*ty))
        localDomain->insert(rhsType);
}

void ConstraintGenerator::visitLValue(const ScopePtr& scope, AstExprGlobal* global, TypeId rhsType)
{
    std::optional<TypeId> annotatedTy = scope->lookup(Symbol{global->name});
    if (annotatedTy)
    {
        DefId def = dfg->getDef(global);
        rootScope->lvalueTypes[def] = rhsType;

        addConstraint(scope, global->location, SubtypeConstraint{rhsType, *annotatedTy});
    }
}

void ConstraintGenerator::visitLValue(const ScopePtr& scope, AstExprIndexName* expr, TypeId rhsType)
{
    TypeId lhsTy = check(scope, expr->expr).ty;
    TypeId propTy = arena->addType(BlockedType{});
    module->astTypes[expr] = propTy;

    bool incremented = recordPropertyAssignment(lhsTy);

    auto apc =
        addConstraint(scope, expr->location, AssignPropConstraint{lhsTy, expr->index.value, rhsType, expr->indexLocation, propTy, incremented});
    getMutable<BlockedType>(propTy)->setOwner(apc);
}

void ConstraintGenerator::visitLValue(const ScopePtr& scope, AstExprIndexExpr* expr, TypeId rhsType)
{
    if (auto constantString = expr->index->as<AstExprConstantString>())
    {
        TypeId lhsTy = check(scope, expr->expr).ty;
        TypeId propTy = arena->addType(BlockedType{});
        module->astTypes[expr] = propTy;
        module->astTypes[expr->index] = builtinTypes->stringType; // FIXME? Singleton strings exist.
        std::string propName{constantString->value.data, constantString->value.size};

        bool incremented = recordPropertyAssignment(lhsTy);

        auto apc = addConstraint(
            scope, expr->location, AssignPropConstraint{lhsTy, std::move(propName), rhsType, expr->index->location, propTy, incremented}
        );
        getMutable<BlockedType>(propTy)->setOwner(apc);

        return;
    }

    TypeId lhsTy = check(scope, expr->expr).ty;
    TypeId indexTy = check(scope, expr->index).ty;
    TypeId propTy = arena->addType(BlockedType{});
    module->astTypes[expr] = propTy;
    auto aic = addConstraint(scope, expr->location, AssignIndexConstraint{lhsTy, indexTy, rhsType, propTy});
    getMutable<BlockedType>(propTy)->setOwner(aic);
}

Inference ConstraintGenerator::check(const ScopePtr& scope, AstExprTable* expr, std::optional<TypeId> expectedType)
{
    TypeId ty = arena->addType(TableType{});
    TableType* ttv = getMutable<TableType>(ty);
    LUAU_ASSERT(ttv);

    ttv->state = TableState::Unsealed;
    ttv->definitionModuleName = module->name;
    if (FFlag::LuauNewSolverPopulateTableLocations)
    {
        ttv->definitionLocation = expr->location;
    }
    ttv->scope = scope.get();

    interiorTypes.back().push_back(ty);

    TypeIds indexKeyLowerBound;
    TypeIds indexValueLowerBound;

    auto createIndexer = [&indexKeyLowerBound, &indexValueLowerBound](const Location& location, TypeId currentIndexType, TypeId currentResultType)
    {
        indexKeyLowerBound.insert(follow(currentIndexType));
        indexValueLowerBound.insert(follow(currentResultType));
    };

    TypeIds valuesLowerBound;

    for (const AstExprTable::Item& item : expr->items)
    {
        // Expected types are threaded through table literals separately via the
        // function matchLiteralType.

        TypeId itemTy = check(scope, item.value).ty;

        if (item.key)
        {
            // Even though we don't need to use the type of the item's key if
            // it's a string constant, we still want to check it to populate
            // astTypes.
            TypeId keyTy = check(scope, item.key).ty;

            if (AstExprConstantString* key = item.key->as<AstExprConstantString>())
            {
                std::string propName{key->value.data, key->value.size};
                ttv->props[propName] = {itemTy, /*deprecated*/ false, {}, key->location};
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

    if (!indexKeyLowerBound.empty())
    {
        LUAU_ASSERT(!indexValueLowerBound.empty());

        TypeId indexKey = indexKeyLowerBound.size() == 1
                              ? *indexKeyLowerBound.begin()
                              : arena->addType(UnionType{std::vector(indexKeyLowerBound.begin(), indexKeyLowerBound.end())});

        TypeId indexValue = indexValueLowerBound.size() == 1
                                ? *indexValueLowerBound.begin()
                                : arena->addType(UnionType{std::vector(indexValueLowerBound.begin(), indexValueLowerBound.end())});

        ttv->indexer = TableIndexer{indexKey, indexValue};
    }

    if (expectedType)
    {
        Unifier2 unifier{arena, builtinTypes, NotNull{scope.get()}, ice};
        std::vector<TypeId> toBlock;
        // This logic is incomplete as we want to re-run this
        // _after_ blocked types have resolved, but this
        // allows us to do some bidirectional inference.
        toBlock = findBlockedTypesIn(expr, NotNull{&module->astTypes});

        if (toBlock.empty())
        {
            matchLiteralType(
                NotNull{&module->astTypes},
                NotNull{&module->astExpectedTypes},
                builtinTypes,
                arena,
                NotNull{&unifier},
                *expectedType,
                ty,
                expr,
                toBlock
                );
                // The visitor we ran prior should ensure that there are no
                // blocked types that we would encounter while matching on
                // this expression.
                LUAU_ASSERT(toBlock.empty());
        }
    }

    return Inference{ty};
}

ConstraintGenerator::FunctionSignature ConstraintGenerator::checkFunctionSignature(
    const ScopePtr& parent,
    AstExprFunction* fn,
    std::optional<TypeId> expectedType,
    std::optional<Location> originalName
)
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
        if (auto ut = get<UnionType>(*expectedType))
        {
            for (auto u : ut)
            {
                if (get<FunctionType>(u) && !isNil(u))
                {
                    expectedFunction = get<FunctionType>(u);
                    break;
                }
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

        DefId def = dfg->getDef(fn->self);
        signatureScope->lvalueTypes[def] = selfType;
        signatureScope->rvalueRefinements[def] = selfType;
    }

    for (size_t i = 0; i < fn->args.size; ++i)
    {
        AstLocal* local = fn->args.data[i];

        TypeId argTy = nullptr;
        if (local->annotation)
            argTy = resolveType(signatureScope, local->annotation, /* inTypeArguments */ false, /* replaceErrorWithFresh*/ true);
        else
        {
            if (i < expectedArgPack.head.size())
                argTy = expectedArgPack.head[i];
            else
                argTy = freshType(signatureScope);
        }

        argTypes.push_back(argTy);
        argNames.emplace_back(FunctionArgument{local->name.value, local->location});

        signatureScope->bindings[local] = Binding{argTy, local->location};

        DefId def = dfg->getDef(local);
        signatureScope->lvalueTypes[def] = argTy;
        signatureScope->rvalueRefinements[def] = argTy;
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
        emplaceTypePack<BoundTypePack>(asMutable(returnType), annotatedRetType);
    }
    else if (expectedFunction)
    {
        emplaceTypePack<BoundTypePack>(asMutable(returnType), expectedFunction->retTypes);
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

void ConstraintGenerator::checkFunctionBody(const ScopePtr& scope, AstExprFunction* fn)
{
    // If it is possible for execution to reach the end of the function, the return type must be compatible with ()
    ControlFlow cf = visitBlockWithoutChildScope(scope, fn->body);
    if (cf == ControlFlow::None)
        addConstraint(scope, fn->location, PackSubtypeConstraint{builtinTypes->emptyTypePack, scope->returnType});
}

TypeId ConstraintGenerator::resolveReferenceType(
    const ScopePtr& scope,
    AstType* ty,
    AstTypeReference* ref,
    bool inTypeArguments,
    bool replaceErrorWithFresh
)
{
    TypeId result = nullptr;

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
                    TypePackId tp = resolveTypePack(scope, p.typePack, /*inTypeArguments*/ true);

                    // If we need more regular types, we can use single element type packs to fill those in
                    if (parameters.size() < alias->typeParams.size() && size(tp) == 1 && finite(tp) && first(tp))
                        parameters.push_back(*first(tp));
                    else
                        packParameters.push_back(tp);
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

    return result;
}

TypeId ConstraintGenerator::resolveTableType(const ScopePtr& scope, AstType* ty, AstTypeTable* tab, bool inTypeArguments, bool replaceErrorWithFresh)
{
    TableType::Props props;
    std::optional<TableIndexer> indexer;

    for (const AstTableProp& prop : tab->props)
    {
        TypeId propTy = resolveType(scope, prop.type, inTypeArguments);

        Property& p = props[prop.name.value];
        p.typeLocation = prop.location;

        switch (prop.access)
        {
        case AstTableAccess::ReadWrite:
            p.readTy = propTy;
            p.writeTy = propTy;
            break;
        case AstTableAccess::Read:
            p.readTy = propTy;
            break;
        case AstTableAccess::Write:
            reportError(*prop.accessLocation, GenericError{"write keyword is illegal here"});
            p.readTy = propTy;
            p.writeTy = propTy;
            break;
        default:
            ice->ice("Unexpected property access " + std::to_string(int(prop.access)));
            break;
        }
    }

    if (AstTableIndexer* astIndexer = tab->indexer)
    {
        if (astIndexer->access == AstTableAccess::Read)
            reportError(astIndexer->accessLocation.value_or(Location{}), GenericError{"read keyword is illegal here"});
        else if (astIndexer->access == AstTableAccess::Write)
            reportError(astIndexer->accessLocation.value_or(Location{}), GenericError{"write keyword is illegal here"});
        else if (astIndexer->access == AstTableAccess::ReadWrite)
        {
            indexer = TableIndexer{
                resolveType(scope, astIndexer->indexType, inTypeArguments),
                resolveType(scope, astIndexer->resultType, inTypeArguments),
            };
        }
        else
            ice->ice("Unexpected property access " + std::to_string(int(astIndexer->access)));
    }

    TypeId tableTy = arena->addType(TableType{props, indexer, scope->level, scope.get(), TableState::Sealed});
    TableType* ttv = getMutable<TableType>(tableTy);

    if (FFlag::LuauNewSolverPopulateTableLocations)
    {
        ttv->definitionModuleName = module->name;
        ttv->definitionLocation = tab->location;
    }

    return tableTy;
}

TypeId ConstraintGenerator::resolveFunctionType(
    const ScopePtr& scope,
    AstType* ty,
    AstTypeFunction* fn,
    bool inTypeArguments,
    bool replaceErrorWithFresh
)
{
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
    ftv.isCheckedFunction = fn->isCheckedFunction();

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

    return arena->addType(std::move(ftv));
}

TypeId ConstraintGenerator::resolveType(const ScopePtr& scope, AstType* ty, bool inTypeArguments, bool replaceErrorWithFresh)
{
    TypeId result = nullptr;

    if (auto ref = ty->as<AstTypeReference>())
    {
        result = resolveReferenceType(scope, ty, ref, inTypeArguments, replaceErrorWithFresh);
    }
    else if (auto tab = ty->as<AstTypeTable>())
    {
        result = resolveTableType(scope, ty, tab, inTypeArguments, replaceErrorWithFresh);
    }
    else if (auto fn = ty->as<AstTypeFunction>())
    {
        result = resolveFunctionType(scope, ty, fn, inTypeArguments, replaceErrorWithFresh);
    }
    else if (auto tof = ty->as<AstTypeTypeof>())
    {
        TypeId exprType = check(scope, tof->expr).ty;
        result = exprType;
    }
    else if (auto unionAnnotation = ty->as<AstTypeUnion>())
    {
        std::vector<TypeId> parts;
        for (AstType* part : unionAnnotation->types)
        {
            parts.push_back(resolveType(scope, part, inTypeArguments));
        }

        result = arena->addType(UnionType{parts});
    }
    else if (auto intersectionAnnotation = ty->as<AstTypeIntersection>())
    {
        std::vector<TypeId> parts;
        for (AstType* part : intersectionAnnotation->types)
        {
            parts.push_back(resolveType(scope, part, inTypeArguments));
        }

        result = arena->addType(IntersectionType{parts});
    }
    else if (auto boolAnnotation = ty->as<AstTypeSingletonBool>())
    {
        if (boolAnnotation->value)
            result = builtinTypes->trueType;
        else
            result = builtinTypes->falseType;
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

TypePackId ConstraintGenerator::resolveTypePack(const ScopePtr& scope, AstTypePack* tp, bool inTypeArgument, bool replaceErrorWithFresh)
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

TypePackId ConstraintGenerator::resolveTypePack(const ScopePtr& scope, const AstTypeList& list, bool inTypeArguments, bool replaceErrorWithFresh)
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

    return addTypePack(std::move(head), tail);
}

std::vector<std::pair<Name, GenericTypeDefinition>> ConstraintGenerator::createGenerics(
    const ScopePtr& scope,
    AstArray<AstGenericType> generics,
    bool useCache,
    bool addTypes
)
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

std::vector<std::pair<Name, GenericTypePackDefinition>> ConstraintGenerator::createGenericPacks(
    const ScopePtr& scope,
    AstArray<AstGenericTypePack> generics,
    bool useCache,
    bool addTypes
)
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

Inference ConstraintGenerator::flattenPack(const ScopePtr& scope, Location location, InferencePack pack)
{
    const auto& [tp, refinements] = pack;
    RefinementId refinement = nullptr;
    if (!refinements.empty())
        refinement = refinements[0];

    if (auto f = first(tp))
        return Inference{*f, refinement};

    TypeId typeResult = arena->addType(BlockedType{});
    auto c = addConstraint(scope, location, UnpackConstraint{{typeResult}, tp});
    getMutable<BlockedType>(typeResult)->setOwner(c);

    return Inference{typeResult, refinement};
}

void ConstraintGenerator::reportError(Location location, TypeErrorData err)
{
    errors.push_back(TypeError{location, module->name, std::move(err)});

    if (logger)
        logger->captureGenerationError(errors.back());
}

void ConstraintGenerator::reportCodeTooComplex(Location location)
{
    errors.push_back(TypeError{location, module->name, CodeTooComplex{}});

    if (logger)
        logger->captureGenerationError(errors.back());
}

TypeId ConstraintGenerator::makeUnion(const ScopePtr& scope, Location location, TypeId lhs, TypeId rhs)
{
    if (get<NeverType>(follow(lhs)))
        return rhs;
    if (get<NeverType>(follow(rhs)))
        return lhs;

    TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().unionFunc, {lhs, rhs}, {}, scope, location);

    return resultType;
}

TypeId ConstraintGenerator::makeIntersect(const ScopePtr& scope, Location location, TypeId lhs, TypeId rhs)
{
    TypeId resultType = createTypeFunctionInstance(builtinTypeFunctions().intersectFunc, {lhs, rhs}, {}, scope, location);

    return resultType;
}

struct FragmentTypeCheckGlobalPrepopulator : AstVisitor
{
    const NotNull<Scope> globalScope;
    const NotNull<Scope> currentScope;
    const NotNull<const DataFlowGraph> dfg;
    const NotNull<TypeArena> arena;

    FragmentTypeCheckGlobalPrepopulator(
        NotNull<Scope> globalScope,
        NotNull<Scope> currentScope,
        NotNull<const DataFlowGraph> dfg,
        NotNull<TypeArena> arena
    )
        : globalScope(globalScope)
        , currentScope(currentScope)
        , dfg(dfg)
        , arena(arena)
    {
    }

    bool visit(AstExprGlobal* global) override
    {
        if (auto ty = globalScope->lookup(global->name))
        {
            DefId def = dfg->getDef(global);
            // We only want to write into the current scope the type of the global
            currentScope->lvalueTypes[def] = *ty;
        }
        else if (auto ty = currentScope->lookup(global->name))
        {
            // We are trying to create a binding for a brand new function, so we actually do have to write it into the scope.
            DefId def = dfg->getDef(global);
            // We only want to write into the current scope the type of the global
            currentScope->lvalueTypes[def] = *ty;
        }

        return true;
    }

    bool visit(AstStatFunction* function) override
    {
        if (AstExprGlobal* g = function->name->as<AstExprGlobal>())
        {
            if (auto ty = globalScope->lookup(g->name))
            {
                currentScope->bindings[g->name] = Binding{*ty};
            }
            else
            {
                // Hasn't existed since a previous typecheck
                TypeId bt = arena->addType(BlockedType{});
                currentScope->bindings[g->name] = Binding{bt};
            }
        }

        return true;
    }
};

struct GlobalPrepopulator : AstVisitor
{
    const NotNull<Scope> globalScope;
    const NotNull<TypeArena> arena;
    const NotNull<const DataFlowGraph> dfg;

    GlobalPrepopulator(NotNull<Scope> globalScope, NotNull<TypeArena> arena, NotNull<const DataFlowGraph> dfg)
        : globalScope(globalScope)
        , arena(arena)
        , dfg(dfg)
    {
    }

    bool visit(AstExprGlobal* global) override
    {
        if (auto ty = globalScope->lookup(global->name))
        {
            DefId def = dfg->getDef(global);
            globalScope->lvalueTypes[def] = *ty;
        }

        return true;
    }

    bool visit(AstStatFunction* function) override
    {
        if (AstExprGlobal* g = function->name->as<AstExprGlobal>())
        {
            TypeId bt = arena->addType(BlockedType{});
            globalScope->bindings[g->name] = Binding{bt};
        }

        return true;
    }

    bool visit(AstType*) override
    {
        return true;
    }

    bool visit(class AstTypePack* node) override
    {
        return true;
    }
};

void ConstraintGenerator::prepopulateGlobalScopeForFragmentTypecheck(const ScopePtr& globalScope, const ScopePtr& resumeScope, AstStatBlock* program)
{
    FragmentTypeCheckGlobalPrepopulator gp{NotNull{globalScope.get()}, NotNull{resumeScope.get()}, dfg, arena};
    if (prepareModuleScope)
        prepareModuleScope(module->name, resumeScope);
    program->visit(&gp);
}

void ConstraintGenerator::prepopulateGlobalScope(const ScopePtr& globalScope, AstStatBlock* program)
{
    GlobalPrepopulator gp{NotNull{globalScope.get()}, arena, dfg};

    if (prepareModuleScope)
        prepareModuleScope(module->name, globalScope);

    program->visit(&gp);
}

bool ConstraintGenerator::recordPropertyAssignment(TypeId ty)
{
    DenseHashSet<TypeId> seen{nullptr};
    VecDeque<TypeId> queue;

    queue.push_back(ty);

    bool incremented = false;

    while (!queue.empty())
    {
        const TypeId t = follow(queue.front());
        queue.pop_front();

        if (seen.find(t))
            continue;
        seen.insert(t);

        if (auto tt = getMutable<TableType>(t); tt && tt->state == TableState::Unsealed)
        {
            tt->remainingProps += 1;
            incremented = true;
        }
        else if (auto mt = get<MetatableType>(t))
            queue.push_back(mt->table);
        else if (TypeIds* localDomain = localTypes.find(t))
        {
            for (TypeId domainTy : *localDomain)
                queue.push_back(domainTy);
        }
        else if (auto ut = get<UnionType>(t))
        {
            for (TypeId part : ut)
                queue.push_back(part);
        }
    }

    return incremented;
}

void ConstraintGenerator::recordInferredBinding(AstLocal* local, TypeId ty)
{
    if (InferredBinding* ib = inferredBindings.find(local))
        ib->types.insert(ty);
}

void ConstraintGenerator::fillInInferredBindings(const ScopePtr& globalScope, AstStatBlock* block)
{
    for (const auto& [symbol, p] : inferredBindings)
    {
        const auto& [scope, location, types] = p;

        std::vector<TypeId> tys(types.begin(), types.end());
        if (tys.size() == 1)
            scope->bindings[symbol] = Binding{tys.front(), location};
        else
        {
            TypeId ty = createTypeFunctionInstance(builtinTypeFunctions().unionFunc, std::move(tys), {}, globalScope, location);

            scope->bindings[symbol] = Binding{ty, location};
        }
    }
}

std::vector<std::optional<TypeId>> ConstraintGenerator::getExpectedCallTypesForFunctionOverloads(const TypeId fnType)
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
    auto assignOption = [this, &expectedTypes](size_t index, TypeId ty)
    {
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

TypeId ConstraintGenerator::createTypeFunctionInstance(
    const TypeFunction& function,
    std::vector<TypeId> typeArguments,
    std::vector<TypePackId> packArguments,
    const ScopePtr& scope,
    Location location
)
{
    TypeId result = arena->addTypeFunction(function, typeArguments, packArguments);
    addConstraint(scope, location, ReduceConstraint{result});
    return result;
}

TypeId ConstraintGenerator::simplifyUnion(const ScopePtr& scope, Location location, TypeId left, TypeId right)
{
    if (FFlag::DebugLuauEqSatSimplification)
    {
        TypeId ty = arena->addType(UnionType{{left, right}});
        std::optional<EqSatSimplificationResult> res = eqSatSimplify(simplifier, ty);
        if (!res)
            return ty;

        for (TypeId tyFun : res->newTypeFunctions)
            addConstraint(scope, location, ReduceConstraint{tyFun});

        return res->result;
    }
    else
        return ::Luau::simplifyUnion(builtinTypes, arena, left, right).result;
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
