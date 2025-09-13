// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "AutocompleteCore.h"

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/AutocompleteTypes.h"

#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/FileResolver.h"
#include "Luau/Frontend.h"
#include "Luau/TimeTrace.h"
#include "Luau/ToString.h"
#include "Luau/Subtyping.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"

#include <algorithm>
#include <array>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTINT(LuauTypeInferIterationLimit)
LUAU_FASTINT(LuauTypeInferRecursionLimit)
LUAU_FASTFLAGVARIABLE(DebugLuauMagicVariableNames)
LUAU_FASTFLAGVARIABLE(LuauIncludeBreakContinueStatements)
LUAU_FASTFLAGVARIABLE(LuauSuggestHotComments)
LUAU_FASTFLAG(LuauAutocompleteAttributes)

static constexpr std::array<std::string_view, 12> kStatementStartingKeywords =
    {"while", "if", "local", "repeat", "function", "do", "for", "return", "break", "continue", "type", "export"};

static constexpr std::array<std::string_view, 6> kHotComments = {"nolint", "nocheck", "nonstrict", "strict", "optimize", "native"};

static const std::string kKnownAttributes[] = {"checked", "deprecated", "native"};

namespace Luau
{

static bool alreadyHasParens(const std::vector<AstNode*>& nodes)
{
    auto iter = nodes.rbegin();
    while (iter != nodes.rend() &&
           ((*iter)->is<AstExprLocal>() || (*iter)->is<AstExprGlobal>() || (*iter)->is<AstExprIndexName>() || (*iter)->is<AstExprIndexExpr>()))
    {
        iter++;
    }

    if (iter == nodes.rend() || iter == nodes.rbegin())
    {
        return false;
    }

    if (AstExprCall* call = (*iter)->as<AstExprCall>())
    {
        return call->func == *(iter - 1);
    }

    return false;
}

static ParenthesesRecommendation getParenRecommendationForFunc(const FunctionType* func, const std::vector<AstNode*>& nodes)
{
    if (alreadyHasParens(nodes))
    {
        return ParenthesesRecommendation::None;
    }

    auto idxExpr = nodes.back()->as<AstExprIndexName>();
    bool hasImplicitSelf = idxExpr && idxExpr->op == ':';
    auto [argTypes, argVariadicPack] = Luau::flatten(func->argTypes);

    if (argVariadicPack.has_value() && isVariadic(*argVariadicPack))
        return ParenthesesRecommendation::CursorInside;

    bool noArgFunction = argTypes.empty() || (hasImplicitSelf && argTypes.size() == 1);
    return noArgFunction ? ParenthesesRecommendation::CursorAfter : ParenthesesRecommendation::CursorInside;
}

static ParenthesesRecommendation getParenRecommendationForIntersect(const IntersectionType* intersect, const std::vector<AstNode*>& nodes)
{
    ParenthesesRecommendation rec = ParenthesesRecommendation::None;
    for (Luau::TypeId partId : intersect->parts)
    {
        partId = follow(partId);
        if (auto partFunc = Luau::get<FunctionType>(partId))
        {
            rec = std::max(rec, getParenRecommendationForFunc(partFunc, nodes));
        }
        else
        {
            return ParenthesesRecommendation::None;
        }
    }
    return rec;
}

static ParenthesesRecommendation getParenRecommendation(TypeId id, const std::vector<AstNode*>& nodes, TypeCorrectKind typeCorrect)
{
    // If element is already type-correct, even a function should be inserted without parenthesis
    if (typeCorrect == TypeCorrectKind::Correct)
        return ParenthesesRecommendation::None;

    id = Luau::follow(id);
    if (auto func = get<FunctionType>(id))
    {
        return getParenRecommendationForFunc(func, nodes);
    }
    else if (auto intersect = get<IntersectionType>(id))
    {
        return getParenRecommendationForIntersect(intersect, nodes);
    }
    return ParenthesesRecommendation::None;
}

static std::optional<TypeId> findExpectedTypeAt(const Module& module, AstNode* node, Position position)
{
    auto expr = node->asExpr();
    if (!expr)
        return std::nullopt;

    // Extra care for first function call argument location
    // When we don't have anything inside () yet, we also don't have an AST node to base our lookup
    if (AstExprCall* exprCall = expr->as<AstExprCall>())
    {
        if (exprCall->args.size == 0 && exprCall->argLocation.contains(position))
        {
            auto it = module.astTypes.find(exprCall->func);

            if (!it)
                return std::nullopt;

            const FunctionType* ftv = get<FunctionType>(follow(*it));

            if (!ftv)
                return std::nullopt;

            auto [head, tail] = flatten(ftv->argTypes);
            unsigned index = exprCall->self ? 1 : 0;

            if (index < head.size())
                return head[index];

            return std::nullopt;
        }
    }

    auto it = module.astExpectedTypes.find(expr);
    if (!it)
        return std::nullopt;

    return *it;
}

static bool checkTypeMatch(
    const Module& module,
    TypeId subTy,
    TypeId superTy,
    NotNull<Scope> scope,
    TypeArena* typeArena,
    NotNull<BuiltinTypes> builtinTypes
)
{
    InternalErrorReporter iceReporter;
    UnifierSharedState unifierState(&iceReporter);
    SimplifierPtr simplifier = newSimplifier(NotNull{typeArena}, builtinTypes);
    Normalizer normalizer{typeArena, builtinTypes, NotNull{&unifierState}, module.checkedInNewSolver ? SolverMode::New : SolverMode::Old};
    if (module.checkedInNewSolver)
    {
        TypeCheckLimits limits;
        TypeFunctionRuntime typeFunctionRuntime{
            NotNull{&iceReporter}, NotNull{&limits}
        }; // TODO: maybe subtyping checks should not invoke user-defined type function runtime

        unifierState.counters.recursionLimit = FInt::LuauTypeInferRecursionLimit;
        unifierState.counters.iterationLimit = FInt::LuauTypeInferIterationLimit;

        Subtyping subtyping{
            builtinTypes, NotNull{typeArena}, NotNull{simplifier.get()}, NotNull{&normalizer}, NotNull{&typeFunctionRuntime}, NotNull{&iceReporter}
        };

        return subtyping.isSubtype(subTy, superTy, scope).isSubtype;
    }
    else
    {
        Unifier unifier(NotNull<Normalizer>{&normalizer}, scope, Location(), Variance::Covariant);

        // Cost of normalization can be too high for autocomplete response time requirements
        unifier.normalize = false;
        unifier.checkInhabited = false;

        unifierState.counters.recursionLimit = FInt::LuauTypeInferRecursionLimit;
        unifierState.counters.iterationLimit = FInt::LuauTypeInferIterationLimit;

        return unifier.canUnify(subTy, superTy).empty();
    }
}

static TypeCorrectKind checkTypeCorrectKind(
    const Module& module,
    TypeArena* typeArena,
    NotNull<BuiltinTypes> builtinTypes,
    AstNode* node,
    Position position,
    TypeId ty
)
{
    ty = follow(ty);

    LUAU_ASSERT(module.hasModuleScope());

    NotNull<Scope> moduleScope{module.getModuleScope().get()};

    auto typeAtPosition = findExpectedTypeAt(module, node, position);

    if (!typeAtPosition)
        return TypeCorrectKind::None;

    TypeId expectedType = follow(*typeAtPosition);

    auto checkFunctionType = [typeArena, builtinTypes, moduleScope, &expectedType, &module](const FunctionType* ftv)
    {
        if (std::optional<TypeId> firstRetTy = first(ftv->retTypes))
            return checkTypeMatch(module, *firstRetTy, expectedType, moduleScope, typeArena, builtinTypes);

        return false;
    };

    // We also want to suggest functions that return compatible result
    if (const FunctionType* ftv = get<FunctionType>(ty); ftv && checkFunctionType(ftv))
    {
        return TypeCorrectKind::CorrectFunctionResult;
    }
    else if (const IntersectionType* itv = get<IntersectionType>(ty))
    {
        for (TypeId id : itv->parts)
        {
            id = follow(id);

            if (const FunctionType* ftv = get<FunctionType>(id); ftv && checkFunctionType(ftv))
            {
                return TypeCorrectKind::CorrectFunctionResult;
            }
        }
    }

    return checkTypeMatch(module, ty, expectedType, moduleScope, typeArena, builtinTypes) ? TypeCorrectKind::Correct : TypeCorrectKind::None;
}

enum class PropIndexType
{
    Point,
    Colon,
    Key,
};

static void autocompleteProps(
    const Module& module,
    TypeArena* typeArena,
    NotNull<BuiltinTypes> builtinTypes,
    TypeId rootTy,
    TypeId ty,
    PropIndexType indexType,
    const std::vector<AstNode*>& nodes,
    AutocompleteEntryMap& result,
    std::unordered_set<TypeId>& seen,
    std::optional<const ExternType*> containingExternType = std::nullopt
)
{
    rootTy = follow(rootTy);
    ty = follow(ty);

    if (seen.count(ty))
        return;
    seen.insert(ty);

    auto isWrongIndexer = [typeArena, builtinTypes, &module, rootTy, indexType](Luau::TypeId type)
    {
        if (indexType == PropIndexType::Key)
            return false;

        bool calledWithSelf = indexType == PropIndexType::Colon;

        auto isCompatibleCall = [typeArena, builtinTypes, &module, rootTy, calledWithSelf](const FunctionType* ftv)
        {
            // Strong match with definition is a success
            if (calledWithSelf == ftv->hasSelf)
                return true;

            // Calls on extern types require strict match between how function is declared and how it's called
            if (get<ExternType>(rootTy))
                return false;

            // When called with ':', but declared without 'self', it is invalid if a function has incompatible first argument or no arguments at all
            // When called with '.', but declared with 'self', it is considered invalid if first argument is compatible
            if (std::optional<TypeId> firstArgTy = first(ftv->argTypes))
            {
                if (checkTypeMatch(module, rootTy, *firstArgTy, NotNull{module.getModuleScope().get()}, typeArena, builtinTypes))
                    return calledWithSelf;
            }

            return !calledWithSelf;
        };

        if (const FunctionType* ftv = get<FunctionType>(type))
            return !isCompatibleCall(ftv);

        // For intersections, any part that is successful makes the whole call successful
        if (const IntersectionType* itv = get<IntersectionType>(type))
        {
            for (auto subType : itv->parts)
            {
                if (const FunctionType* ftv = get<FunctionType>(Luau::follow(subType)))
                {
                    if (isCompatibleCall(ftv))
                        return false;
                }
            }
        }

        return calledWithSelf;
    };

    auto fillProps = [&](const ExternType::Props& props)
    {
        for (const auto& [name, prop] : props)
        {
            // We are walking up the class hierarchy, so if we encounter a property that we have
            // already populated, it takes precedence over the property we found just now.
            if (result.count(name) == 0 && name != kParseNameError)
            {
                Luau::TypeId type;

                if (FFlag::LuauSolverV2)
                {
                    if (auto ty = prop.readTy)
                        type = follow(*ty);
                    else
                        continue;
                }
                else
                    type = follow(prop.type_DEPRECATED());

                TypeCorrectKind typeCorrect = indexType == PropIndexType::Key
                                                  ? TypeCorrectKind::Correct
                                                  : checkTypeCorrectKind(module, typeArena, builtinTypes, nodes.back(), {{}, {}}, type);

                ParenthesesRecommendation parens =
                    indexType == PropIndexType::Key ? ParenthesesRecommendation::None : getParenRecommendation(type, nodes, typeCorrect);

                result[name] = AutocompleteEntry{
                    AutocompleteEntryKind::Property,
                    type,
                    prop.deprecated,
                    isWrongIndexer(type),
                    typeCorrect,
                    containingExternType,
                    &prop,
                    prop.documentationSymbol,
                    {},
                    parens,
                    {},
                    indexType == PropIndexType::Colon
                };
            }
        }
    };

    auto fillMetatableProps = [&](const TableType* mtable)
    {
        auto indexIt = mtable->props.find("__index");
        if (indexIt != mtable->props.end())
        {
            TypeId followed;
            if (FFlag::LuauSolverV2)
                followed = follow(*indexIt->second.readTy);
            else
                followed = follow(indexIt->second.type_DEPRECATED());
            if (get<TableType>(followed) || get<MetatableType>(followed))
            {
                autocompleteProps(module, typeArena, builtinTypes, rootTy, followed, indexType, nodes, result, seen);
            }
            else if (auto indexFunction = get<FunctionType>(followed))
            {
                std::optional<TypeId> indexFunctionResult = first(indexFunction->retTypes);
                if (indexFunctionResult)
                    autocompleteProps(module, typeArena, builtinTypes, rootTy, *indexFunctionResult, indexType, nodes, result, seen);
            }
        }
    };

    if (auto cls = get<ExternType>(ty))
    {
        containingExternType = containingExternType.value_or(cls);
        fillProps(cls->props);
        if (cls->parent)
            autocompleteProps(module, typeArena, builtinTypes, rootTy, *cls->parent, indexType, nodes, result, seen, containingExternType);
    }
    else if (auto tbl = get<TableType>(ty))
        fillProps(tbl->props);
    else if (auto mt = get<MetatableType>(ty))
    {
        autocompleteProps(module, typeArena, builtinTypes, rootTy, mt->table, indexType, nodes, result, seen);

        if (auto mtable = get<TableType>(follow(mt->metatable)))
            fillMetatableProps(mtable);
    }
    else if (auto i = get<IntersectionType>(ty))
    {
        // Complete all properties in every variant
        for (TypeId ty : i->parts)
        {
            AutocompleteEntryMap inner;
            std::unordered_set<TypeId> innerSeen = seen;

            autocompleteProps(module, typeArena, builtinTypes, rootTy, ty, indexType, nodes, inner, innerSeen);

            for (auto& pair : inner)
                result.insert(pair);
        }
    }
    else if (auto u = get<UnionType>(ty))
    {
        // Complete all properties common to all variants
        auto iter = begin(u);
        auto endIter = end(u);

        while (iter != endIter)
        {
            if (isNil(*iter))
                ++iter;
            else
                break;
        }

        if (iter == endIter)
            return;

        autocompleteProps(module, typeArena, builtinTypes, rootTy, *iter, indexType, nodes, result, seen);

        ++iter;

        while (iter != endIter)
        {
            AutocompleteEntryMap inner;
            std::unordered_set<TypeId> innerSeen;

            // If we don't do this, and we have the misfortune of receiving a
            // recursive union like:
            //
            //  t1 where t1 = t1 | ExternType
            //
            // Then we are on a one way journey to a stack overflow.
            for (auto ty : seen)
            {
                if (is<UnionType, IntersectionType>(ty))
                    innerSeen.insert(ty);
            }

            if (isNil(*iter))
            {
                ++iter;
                continue;
            }

            autocompleteProps(module, typeArena, builtinTypes, rootTy, *iter, indexType, nodes, inner, innerSeen);

            std::unordered_set<std::string> toRemove;

            for (const auto& [k, v] : result)
            {
                (void)v;
                if (!inner.count(k))
                    toRemove.insert(k);
            }

            for (const std::string& k : toRemove)
                result.erase(k);

            ++iter;
        }
    }
    else if (auto pt = get<PrimitiveType>(ty))
    {
        if (pt->metatable)
        {
            if (auto mtable = get<TableType>(*pt->metatable))
                fillMetatableProps(mtable);
        }
    }
    else if (get<StringSingleton>(get<SingletonType>(ty)))
    {
        autocompleteProps(module, typeArena, builtinTypes, rootTy, builtinTypes->stringType, indexType, nodes, result, seen);
    }
}

static void autocompleteKeywords(const std::vector<AstNode*>& ancestry, Position position, AutocompleteEntryMap& result)
{
    LUAU_ASSERT(!ancestry.empty());

    AstNode* node = ancestry.back();

    if (!node->is<AstExprFunction>() && node->asExpr())
    {
        // This is not strictly correct. We should recommend `and` and `or` only after
        // another expression, not at the start of a new one. We should only recommend
        // `not` at the start of an expression. Detecting either case reliably is quite
        // complex, however; this is good enough for now.

        // These are not context-sensitive keywords, so we can unconditionally assign.
        result["and"] = {AutocompleteEntryKind::Keyword};
        result["or"] = {AutocompleteEntryKind::Keyword};
        result["not"] = {AutocompleteEntryKind::Keyword};
    }
}

static void autocompleteProps(
    const Module& module,
    TypeArena* typeArena,
    NotNull<BuiltinTypes> builtinTypes,
    TypeId ty,
    PropIndexType indexType,
    const std::vector<AstNode*>& nodes,
    AutocompleteEntryMap& result
)
{
    std::unordered_set<TypeId> seen;
    autocompleteProps(module, typeArena, builtinTypes, ty, ty, indexType, nodes, result, seen);
}

AutocompleteEntryMap autocompleteProps(
    const Module& module,
    TypeArena* typeArena,
    NotNull<BuiltinTypes> builtinTypes,
    TypeId ty,
    PropIndexType indexType,
    const std::vector<AstNode*>& nodes
)
{
    AutocompleteEntryMap result;
    autocompleteProps(module, typeArena, builtinTypes, ty, indexType, nodes, result);
    return result;
}

AutocompleteEntryMap autocompleteModuleTypes(const Module& module, const ScopePtr& scopeAtPosition, Position position, std::string_view moduleName)
{
    AutocompleteEntryMap result;
    ScopePtr startScope = scopeAtPosition;
    for (ScopePtr& scope = startScope; scope; scope = scope->parent)
    {
        if (auto it = scope->importedTypeBindings.find(std::string(moduleName)); it != scope->importedTypeBindings.end())
        {
            for (const auto& [name, ty] : it->second)
                result[name] = AutocompleteEntry{AutocompleteEntryKind::Type, ty.type};

            break;
        }
    }

    return result;
}

static void autocompleteStringSingleton(TypeId ty, bool addQuotes, AstNode* node, Position position, AutocompleteEntryMap& result)
{
    if (position == node->location.begin || position == node->location.end)
    {
        if (auto str = node->as<AstExprConstantString>(); str && str->isQuoted())
            return;
        else if (node->is<AstExprInterpString>())
            return;
    }

    auto formatKey = [addQuotes](const std::string& key)
    {
        if (addQuotes)
            return "\"" + escape(key) + "\"";

        return escape(key);
    };

    ty = follow(ty);

    if (auto ss = get<StringSingleton>(get<SingletonType>(ty)))
    {
        // This is purposefully `try_emplace` as we don't want to override any existing entries.
        result.try_emplace(formatKey(ss->value), AutocompleteEntry{AutocompleteEntryKind::String, ty, false, false, TypeCorrectKind::Correct});
    }
    else if (auto uty = get<UnionType>(ty))
    {
        for (auto el : uty)
        {
            if (auto ss = get<StringSingleton>(get<SingletonType>(el)))
            {
                // This is purposefully `try_emplace` as we don't want to override any existing entries.
                result.try_emplace(
                    formatKey(ss->value), AutocompleteEntry{AutocompleteEntryKind::String, ty, false, false, TypeCorrectKind::Correct}
                );
            }
        }
    }
};

static bool canSuggestInferredType(ScopePtr scope, TypeId ty)
{
    ty = follow(ty);

    // No point in suggesting 'any', invalid to suggest others
    if (get<AnyType>(ty) || get<ErrorType>(ty) || get<GenericType>(ty) || get<FreeType>(ty))
        return false;

    // No syntax for unnamed tables with a metatable
    if (get<MetatableType>(ty))
        return false;

    if (const TableType* ttv = get<TableType>(ty))
    {
        if (ttv->name)
            return true;

        if (ttv->syntheticName)
            return false;
    }

    // We might still have a type with cycles or one that is too long, we'll check that later
    return true;
}

// Walk complex type trees to find the element that is being edited
static std::optional<TypeId> findTypeElementAt(AstType* astType, TypeId ty, Position position);

static std::optional<TypeId> findTypeElementAt(const AstTypeList& astTypeList, TypePackId tp, Position position)
{
    for (size_t i = 0; i < astTypeList.types.size; i++)
    {
        AstType* type = astTypeList.types.data[i];

        if (type->location.containsClosed(position))
        {
            auto [head, _] = flatten(tp);

            if (i < head.size())
                return findTypeElementAt(type, head[i], position);
        }
    }

    if (AstTypePack* argTp = astTypeList.tailType)
    {
        if (auto variadic = argTp->as<AstTypePackVariadic>())
        {
            if (variadic->location.containsClosed(position))
            {
                auto [_, tail] = flatten(tp);

                if (tail)
                {
                    if (const VariadicTypePack* vtp = get<VariadicTypePack>(follow(*tail)))
                        return findTypeElementAt(variadic->variadicType, vtp->ty, position);
                }
            }
        }
    }

    return {};
}

static std::optional<TypeId> findTypeElementAt(AstTypePack* astTypePack, TypePackId tp, Position position)
{
    if (const auto typePack = astTypePack->as<AstTypePackExplicit>())
    {
        return findTypeElementAt(typePack->typeList, tp, position);
    }
    else if (const auto variadic = astTypePack->as<AstTypePackVariadic>())
    {
        if (variadic->location.containsClosed(position))
        {
            auto [_, tail] = flatten(tp);

            if (tail)
            {
                if (const VariadicTypePack* vtp = get<VariadicTypePack>(follow(*tail)))
                    return findTypeElementAt(variadic->variadicType, vtp->ty, position);
            }
        }
    }

    return {};
}

static std::optional<TypeId> findTypeElementAt(AstType* astType, TypeId ty, Position position)
{
    ty = follow(ty);

    if (astType->is<AstTypeReference>())
        return ty;

    if (astType->is<AstTypeError>())
        return ty;

    if (AstTypeFunction* type = astType->as<AstTypeFunction>())
    {
        const FunctionType* ftv = get<FunctionType>(ty);

        if (!ftv)
            return {};

        if (auto element = findTypeElementAt(type->argTypes, ftv->argTypes, position))
            return element;

        if (auto element = findTypeElementAt(type->returnTypes, ftv->retTypes, position))
            return element;
    }

    // It's possible to walk through other types like intrsection and unions if we find value in doing that
    return {};
}

std::optional<TypeId> getLocalTypeInScopeAt(const Module& module, const ScopePtr& scopeAtPosition, Position position, AstLocal* local)
{
    if (ScopePtr scope = scopeAtPosition)
    {
        for (const auto& [name, binding] : scope->bindings)
        {
            if (name == local)
                return binding.typeId;
        }
    }

    return {};
}

template<typename T>
static std::optional<std::string> tryToStringDetailed(const ScopePtr& scope, T ty, bool functionTypeArguments)
{
    ToStringOptions opts;
    opts.useLineBreaks = false;
    opts.hideTableKind = true;
    opts.functionTypeArguments = functionTypeArguments;
    opts.scope = scope;
    ToStringResult name = toStringDetailed(ty, opts);

    if (name.error || name.invalid || name.cycle || name.truncated)
        return std::nullopt;

    return name.name;
}

static std::optional<Name> tryGetTypeNameInScope(ScopePtr scope, TypeId ty, bool functionTypeArguments = false)
{
    if (!canSuggestInferredType(scope, ty))
        return std::nullopt;

    return tryToStringDetailed(scope, ty, functionTypeArguments);
}

static bool tryAddTypeCorrectSuggestion(AutocompleteEntryMap& result, ScopePtr scope, AstType* topType, TypeId inferredType, Position position)
{
    std::optional<TypeId> ty;

    if (topType)
        ty = findTypeElementAt(topType, inferredType, position);
    else
        ty = inferredType;

    if (!ty)
        return false;

    if (auto name = tryGetTypeNameInScope(std::move(scope), *ty))
    {
        if (auto it = result.find(*name); it != result.end())
            it->second.typeCorrect = TypeCorrectKind::Correct;
        else
            result[*name] = AutocompleteEntry{AutocompleteEntryKind::Type, *ty, false, false, TypeCorrectKind::Correct};

        return true;
    }

    return false;
}

static std::optional<TypeId> tryGetTypePackTypeAt(TypePackId tp, size_t index)
{
    auto [tpHead, tpTail] = flatten(tp);

    if (index < tpHead.size())
        return tpHead[index];

    // Infinite tail
    if (tpTail)
    {
        if (const VariadicTypePack* vtp = get<VariadicTypePack>(follow(*tpTail)))
            return vtp->ty;
    }

    return {};
}

template<typename T>
std::optional<const T*> returnFirstNonnullOptionOfType(const UnionType* utv)
{
    std::optional<const T*> ret;
    for (TypeId subTy : utv)
    {
        if (isNil(subTy))
            continue;

        if (const T* ftv = get<T>(follow(subTy)))
        {
            if (ret.has_value())
            {
                return std::nullopt;
            }
            ret = ftv;
        }
        else
        {
            return std::nullopt;
        }
    }
    return ret;
}

static std::optional<bool> functionIsExpectedAt(const Module& module, AstNode* node, Position position)
{
    auto typeAtPosition = findExpectedTypeAt(module, node, position);

    if (!typeAtPosition)
        return std::nullopt;

    TypeId expectedType = follow(*typeAtPosition);

    if (get<FunctionType>(expectedType))
        return true;

    if (const IntersectionType* itv = get<IntersectionType>(expectedType))
    {
        return std::all_of(
            begin(itv->parts),
            end(itv->parts),
            [](auto&& ty)
            {
                return get<FunctionType>(Luau::follow(ty)) != nullptr;
            }
        );
    }

    if (const UnionType* utv = get<UnionType>(expectedType))
        return returnFirstNonnullOptionOfType<FunctionType>(utv).has_value();

    return false;
}

AutocompleteEntryMap autocompleteTypeNames(
    const Module& module,
    const ScopePtr& scopeAtPosition,
    Position& position,
    const std::vector<AstNode*>& ancestry
)
{
    AutocompleteEntryMap result;

    ScopePtr startScope = scopeAtPosition;

    for (ScopePtr scope = startScope; scope; scope = scope->parent)
    {
        for (const auto& [name, ty] : scope->exportedTypeBindings)
        {
            if (!result.count(name))
                result[name] = AutocompleteEntry{
                    AutocompleteEntryKind::Type,
                    ty.type,
                    false,
                    false,
                    TypeCorrectKind::None,
                    std::nullopt,
                    std::nullopt,
                    ty.type->documentationSymbol
                };
        }

        for (const auto& [name, ty] : scope->privateTypeBindings)
        {
            if (!result.count(name))
                result[name] = AutocompleteEntry{
                    AutocompleteEntryKind::Type,
                    ty.type,
                    false,
                    false,
                    TypeCorrectKind::None,
                    std::nullopt,
                    std::nullopt,
                    ty.type->documentationSymbol
                };
        }

        for (const auto& [name, _] : scope->importedTypeBindings)
        {
            if (auto binding = scope->linearSearchForBinding(name, true))
            {
                if (!result.count(name))
                    result[name] = AutocompleteEntry{AutocompleteEntryKind::Module, binding->typeId};
            }
        }
    }

    AstNode* parent = nullptr;
    AstType* topType = nullptr; // TODO: rename?

    for (auto it = ancestry.rbegin(), e = ancestry.rend(); it != e; ++it)
    {
        if (AstType* asType = (*it)->asType())
        {
            topType = asType;
        }
        else
        {
            parent = *it;
            break;
        }
    }

    if (!parent)
        return result;

    if (AstStatLocal* node = parent->as<AstStatLocal>()) // Try to provide inferred type of the local
    {
        // Look at which of the variable types we are defining
        for (size_t i = 0; i < node->vars.size; i++)
        {
            AstLocal* var = node->vars.data[i];

            if (var->annotation && var->annotation->location.containsClosed(position))
            {
                if (node->values.size == 0)
                    break;

                unsigned tailPos = 0;

                // For multiple return values we will try to unpack last function call return type pack
                if (i >= node->values.size)
                {
                    tailPos = int(i) - int(node->values.size) + 1;
                    i = int(node->values.size) - 1;
                }

                AstExpr* expr = node->values.data[i]->asExpr();

                if (!expr)
                    break;

                TypeId inferredType = nullptr;

                if (AstExprCall* exprCall = expr->as<AstExprCall>())
                {
                    if (auto it = module.astTypes.find(exprCall->func))
                    {
                        if (const FunctionType* ftv = get<FunctionType>(follow(*it)))
                        {
                            if (auto ty = tryGetTypePackTypeAt(ftv->retTypes, tailPos))
                                inferredType = *ty;
                        }
                    }
                }
                else
                {
                    if (tailPos != 0)
                        break;

                    if (auto it = module.astTypes.find(expr))
                        inferredType = *it;
                }

                if (inferredType)
                    tryAddTypeCorrectSuggestion(result, std::move(startScope), topType, inferredType, position);

                break;
            }
        }
    }
    else if (AstExprFunction* node = parent->as<AstExprFunction>())
    {
        // For lookup inside expected function type if that's available
        auto tryGetExpectedFunctionType = [](const Module& module, AstExpr* expr) -> const FunctionType*
        {
            auto it = module.astExpectedTypes.find(expr);

            if (!it)
                return nullptr;

            TypeId ty = follow(*it);

            if (const FunctionType* ftv = get<FunctionType>(ty))
                return ftv;

            // Handle optional function type
            if (const UnionType* utv = get<UnionType>(ty))
            {
                return returnFirstNonnullOptionOfType<FunctionType>(utv).value_or(nullptr);
            }

            return nullptr;
        };

        // Find which argument type we are defining
        for (size_t i = 0; i < node->args.size; i++)
        {
            AstLocal* arg = node->args.data[i];

            if (arg->annotation && arg->annotation->location.containsClosed(position))
            {
                if (const FunctionType* ftv = tryGetExpectedFunctionType(module, node))
                {
                    if (auto ty = tryGetTypePackTypeAt(ftv->argTypes, i))
                        tryAddTypeCorrectSuggestion(result, startScope, topType, *ty, position);
                }
                // Otherwise, try to use the type inferred by typechecker
                else if (auto inferredType = getLocalTypeInScopeAt(module, scopeAtPosition, position, arg))
                {
                    tryAddTypeCorrectSuggestion(result, startScope, topType, *inferredType, position);
                }

                break;
            }
        }

        if (AstTypePack* argTp = node->varargAnnotation)
        {
            if (auto variadic = argTp->as<AstTypePackVariadic>())
            {
                if (variadic->location.containsClosed(position))
                {
                    if (const FunctionType* ftv = tryGetExpectedFunctionType(module, node))
                    {
                        if (auto ty = tryGetTypePackTypeAt(ftv->argTypes, ~0u))
                            tryAddTypeCorrectSuggestion(result, startScope, topType, *ty, position);
                    }
                }
            }
        }

        if (!node->returnAnnotation)
            return result;

        if (const auto typePack = node->returnAnnotation->as<AstTypePackExplicit>())
        {
            for (size_t i = 0; i < typePack->typeList.types.size; i++)
            {
                AstType* ret = typePack->typeList.types.data[i];

                if (ret->location.containsClosed(position))
                {
                    if (const FunctionType* ftv = tryGetExpectedFunctionType(module, node))
                    {
                        if (auto ty = tryGetTypePackTypeAt(ftv->retTypes, i))
                            tryAddTypeCorrectSuggestion(result, startScope, topType, *ty, position);
                    }

                    // TODO: with additional type information, we could suggest inferred return type here
                    break;
                }
            }

            if (AstTypePack* retTp = typePack->typeList.tailType)
            {
                if (auto variadic = retTp->as<AstTypePackVariadic>())
                {
                    if (variadic->location.containsClosed(position))
                    {
                        if (const FunctionType* ftv = tryGetExpectedFunctionType(module, node))
                        {
                            if (auto ty = tryGetTypePackTypeAt(ftv->retTypes, ~0u))
                                tryAddTypeCorrectSuggestion(result, std::move(startScope), topType, *ty, position);
                        }
                    }
                }
            }
        }
        else if (auto variadic = node->returnAnnotation->as<AstTypePackVariadic>())
        {
            if (variadic->location.containsClosed(position))
            {
                if (const FunctionType* ftv = tryGetExpectedFunctionType(module, node))
                {
                    if (auto ty = tryGetTypePackTypeAt(ftv->retTypes, ~0u))
                        tryAddTypeCorrectSuggestion(result, std::move(startScope), topType, *ty, position);
                }
            }
        }
    }

    return result;
}

static bool isInLocalNames(const std::vector<AstNode*>& ancestry, Position position)
{
    for (auto iter = ancestry.rbegin(); iter != ancestry.rend(); iter++)
    {
        if (auto statLocal = (*iter)->as<AstStatLocal>())
        {
            for (auto var : statLocal->vars)
            {
                if (var->location.containsClosed(position))
                {
                    return true;
                }
            }
        }
        else if (auto funcExpr = (*iter)->as<AstExprFunction>())
        {
            if (funcExpr->argLocation && funcExpr->argLocation->contains(position))
            {
                return true;
            }
        }
        else if (auto localFunc = (*iter)->as<AstStatLocalFunction>())
        {
            return localFunc->name->location.containsClosed(position);
        }
        else if (auto block = (*iter)->as<AstStatBlock>())
        {
            if (block->body.size > 0)
            {
                return false;
            }
        }
        else if ((*iter)->asStat())
        {
            return false;
        }
    }
    return false;
}

static bool isIdentifier(AstNode* node)
{
    return node->is<AstExprGlobal>() || node->is<AstExprLocal>();
}

static bool isBeingDefined(const std::vector<AstNode*>& ancestry, const Symbol& symbol)
{
    // Current set of rules only check for local binding match
    if (!symbol.local)
        return false;

    for (auto iter = ancestry.rbegin(); iter != ancestry.rend(); iter++)
    {
        if (auto statLocal = (*iter)->as<AstStatLocal>())
        {
            for (auto var : statLocal->vars)
            {
                if (symbol.local == var)
                    return true;
            }
        }
    }

    return false;
}

template<typename T>
T* extractStat(const std::vector<AstNode*>& ancestry)
{
    AstNode* node = ancestry.size() >= 1 ? ancestry.rbegin()[0] : nullptr;
    if (!node)
        return nullptr;

    if (T* t = node->as<T>())
        return t;

    AstNode* parent = ancestry.size() >= 2 ? ancestry.rbegin()[1] : nullptr;
    if (!parent)
        return nullptr;

    AstNode* grandParent = ancestry.size() >= 3 ? ancestry.rbegin()[2] : nullptr;
    AstNode* greatGrandParent = ancestry.size() >= 4 ? ancestry.rbegin()[3] : nullptr;

    if (!grandParent)
        return nullptr;

    if (T* t = parent->as<T>(); t && grandParent->is<AstStatBlock>())
        return t;

    if (!greatGrandParent)
        return nullptr;

    if (T* t = greatGrandParent->as<T>(); t && grandParent->is<AstStatBlock>() && parent->is<AstStatError>() && isIdentifier(node))
        return t;

    return nullptr;
}

static bool isBindingLegalAtCurrentPosition(const Symbol& symbol, const Binding& binding, Position pos)
{
    if (symbol.local)
        return binding.location.end < pos;

    // Builtin globals have an empty location; for defined globals, we want pos to be outside of the definition range to suggest it
    return binding.location == Location() || !binding.location.containsClosed(pos);
}

static bool isValidBreakContinueContext(const std::vector<AstNode*>& ancestry, Position position)
{
    LUAU_ASSERT(FFlag::LuauIncludeBreakContinueStatements);
    for (auto it = ancestry.rbegin(); it != ancestry.rend(); ++it)
    {
        if ((*it)->is<AstStatFunction>() || (*it)->is<AstStatLocalFunction>() || (*it)->is<AstExprFunction>() || (*it)->is<AstStatTypeFunction>() ||
            (*it)->is<AstTypeFunction>())
            return false;

        if (auto statWhile = (*it)->as<AstStatWhile>(); statWhile && statWhile->body->location.contains(position))
            return true;
        else if (auto statFor = (*it)->as<AstStatFor>(); statFor && statFor->body->location.contains(position))
            return true;
        else if (auto statForIn = (*it)->as<AstStatForIn>(); statForIn && statForIn->body->location.contains(position))
            return true;
        else if (auto statRepeat = (*it)->as<AstStatRepeat>(); statRepeat && statRepeat->body->location.contains(position))
            return true;
    }

    return false;
}

static AutocompleteEntryMap autocompleteStatement(
    const Module& module,
    const std::vector<AstNode*>& ancestry,
    const ScopePtr& scopeAtPosition,
    Position& position
)
{
    // This is inefficient. :(
    ScopePtr scope = scopeAtPosition;

    AutocompleteEntryMap result;

    if (isInLocalNames(ancestry, position))
    {
        autocompleteKeywords(ancestry, position, result);
        return result;
    }

    while (scope)
    {
        for (const auto& [name, binding] : scope->bindings)
        {
            if (!isBindingLegalAtCurrentPosition(name, binding, position))
                continue;

            std::string n = toString(name);
            if (!result.count(n))
                result[n] = {
                    AutocompleteEntryKind::Binding,
                    binding.typeId,
                    binding.deprecated,
                    false,
                    TypeCorrectKind::None,
                    std::nullopt,
                    std::nullopt,
                    binding.documentationSymbol,
                    {},
                    getParenRecommendation(binding.typeId, ancestry, TypeCorrectKind::None)
                };
        }

        scope = scope->parent;
    }

    if (FFlag::LuauIncludeBreakContinueStatements)
    {
        bool shouldIncludeBreakAndContinue = isValidBreakContinueContext(ancestry, position);
        for (const std::string_view kw : kStatementStartingKeywords)
        {
            if ((kw != "break" && kw != "continue") || shouldIncludeBreakAndContinue)
                result.emplace(kw, AutocompleteEntry{AutocompleteEntryKind::Keyword});
        }
    }
    else
    {
        for (const std::string_view kw : kStatementStartingKeywords)
            result.emplace(kw, AutocompleteEntry{AutocompleteEntryKind::Keyword});
    }

    for (auto it = ancestry.rbegin(); it != ancestry.rend(); ++it)
    {
        if (AstStatForIn* statForIn = (*it)->as<AstStatForIn>(); statForIn && !statForIn->body->hasEnd)
            result.emplace("end", AutocompleteEntry{AutocompleteEntryKind::Keyword});
        else if (AstStatFor* statFor = (*it)->as<AstStatFor>(); statFor && !statFor->body->hasEnd)
            result.emplace("end", AutocompleteEntry{AutocompleteEntryKind::Keyword});
        else if (AstStatIf* statIf = (*it)->as<AstStatIf>())
        {
            bool hasEnd = statIf->thenbody->hasEnd;
            if (statIf->elsebody)
            {
                if (AstStatBlock* elseBlock = statIf->elsebody->as<AstStatBlock>())
                    hasEnd = elseBlock->hasEnd;
            }

            if (!hasEnd)
                result.emplace("end", AutocompleteEntry{AutocompleteEntryKind::Keyword});
        }
        else if (AstStatWhile* statWhile = (*it)->as<AstStatWhile>(); statWhile && !statWhile->body->hasEnd)
            result.emplace("end", AutocompleteEntry{AutocompleteEntryKind::Keyword});
        else if (AstExprFunction* exprFunction = (*it)->as<AstExprFunction>(); exprFunction && !exprFunction->body->hasEnd)
            result.emplace("end", AutocompleteEntry{AutocompleteEntryKind::Keyword});
        if (AstStatBlock* exprBlock = (*it)->as<AstStatBlock>(); exprBlock && !exprBlock->hasEnd)
            result.emplace("end", AutocompleteEntry{AutocompleteEntryKind::Keyword});
    }

    if (ancestry.size() >= 2)
    {
        AstNode* parent = ancestry.rbegin()[1];
        if (AstStatIf* statIf = parent->as<AstStatIf>())
        {
            if (!statIf->elsebody || (statIf->elseLocation && statIf->elseLocation->containsClosed(position)))
            {
                result.emplace("else", AutocompleteEntry{AutocompleteEntryKind::Keyword});
                result.emplace("elseif", AutocompleteEntry{AutocompleteEntryKind::Keyword});
            }
        }

        if (AstStatRepeat* statRepeat = parent->as<AstStatRepeat>(); statRepeat && !statRepeat->body->hasEnd)
            result.emplace("until", AutocompleteEntry{AutocompleteEntryKind::Keyword});
    }

    if (ancestry.size() >= 4)
    {
        auto iter = ancestry.rbegin();
        if (AstStatIf* statIf = iter[3]->as<AstStatIf>();
            statIf != nullptr && !statIf->elsebody && iter[2]->is<AstStatBlock>() && iter[1]->is<AstStatError>() && isIdentifier(iter[0]))
        {
            result.emplace("else", AutocompleteEntry{AutocompleteEntryKind::Keyword});
            result.emplace("elseif", AutocompleteEntry{AutocompleteEntryKind::Keyword});
        }
    }

    if (AstStatRepeat* statRepeat = extractStat<AstStatRepeat>(ancestry); statRepeat && !statRepeat->body->hasEnd)
        result.emplace("until", AutocompleteEntry{AutocompleteEntryKind::Keyword});

    return result;
}

// Returns true iff `node` was handled by this function (completions, if any, are returned in `outResult`)
static bool autocompleteIfElseExpression(
    const AstNode* node,
    const std::vector<AstNode*>& ancestry,
    const Position& position,
    AutocompleteEntryMap& outResult
)
{
    AstNode* parent = ancestry.size() >= 2 ? ancestry.rbegin()[1] : nullptr;
    if (!parent)
        return false;

    if (node->is<AstExprIfElse>())
    {
        // Don't try to complete when the current node is an if-else expression (i.e. only try to complete when the node is a child of an if-else
        // expression).
        return true;
    }

    AstExprIfElse* ifElseExpr = parent->as<AstExprIfElse>();
    if (!ifElseExpr || ifElseExpr->condition->location.containsClosed(position))
    {
        return false;
    }
    else if (!ifElseExpr->hasThen)
    {
        outResult["then"] = {AutocompleteEntryKind::Keyword};
        return true;
    }
    else if (ifElseExpr->trueExpr->location.containsClosed(position))
    {
        return false;
    }
    else if (!ifElseExpr->hasElse)
    {
        outResult["else"] = {AutocompleteEntryKind::Keyword};
        outResult["elseif"] = {AutocompleteEntryKind::Keyword};
        return true;
    }
    else
    {
        return false;
    }
}

static AutocompleteContext autocompleteExpression(
    const Module& module,
    NotNull<BuiltinTypes> builtinTypes,
    TypeArena* typeArena,
    const std::vector<AstNode*>& ancestry,
    const ScopePtr& scopeAtPosition,
    Position position,
    AutocompleteEntryMap& result
)
{
    LUAU_ASSERT(!ancestry.empty());

    AstNode* node = ancestry.rbegin()[0];

    if (FFlag::DebugLuauMagicVariableNames)
    {
        InternalErrorReporter ice;
        if (auto local = node->as<AstExprLocal>(); local && local->local->name == "_luau_autocomplete_ice")
            ice.ice("_luau_autocomplete_ice encountered", local->location);
        if (auto global = node->as<AstExprGlobal>(); global && global->name == "_luau_autocomplete_ice")
            ice.ice("_luau_autocomplete_ice encountered", global->location);
    }

    if (node->is<AstExprIndexName>())
    {
        if (auto it = module.astTypes.find(node->asExpr()))
            autocompleteProps(module, typeArena, builtinTypes, *it, PropIndexType::Point, ancestry, result);
    }
    else if (autocompleteIfElseExpression(node, ancestry, position, result))
        return AutocompleteContext::Keyword;
    else if (node->is<AstExprFunction>())
        return AutocompleteContext::Unknown;
    else
    {
        // This is inefficient. :(
        ScopePtr scope = scopeAtPosition;

        while (scope)
        {
            for (const auto& [name, binding] : scope->bindings)
            {
                if (!isBindingLegalAtCurrentPosition(name, binding, position))
                    continue;

                if (isBeingDefined(ancestry, name))
                    continue;

                std::string n = toString(name);
                if (!result.count(n))
                {
                    TypeCorrectKind typeCorrect = checkTypeCorrectKind(module, typeArena, builtinTypes, node, position, binding.typeId);

                    result[n] = {
                        AutocompleteEntryKind::Binding,
                        binding.typeId,
                        binding.deprecated,
                        false,
                        typeCorrect,
                        std::nullopt,
                        std::nullopt,
                        binding.documentationSymbol,
                        {},
                        getParenRecommendation(binding.typeId, ancestry, typeCorrect)
                    };
                }
            }

            scope = scope->parent;
        }

        TypeCorrectKind correctForNil = checkTypeCorrectKind(module, typeArena, builtinTypes, node, position, builtinTypes->nilType);
        TypeCorrectKind correctForTrue = checkTypeCorrectKind(module, typeArena, builtinTypes, node, position, builtinTypes->trueType);
        TypeCorrectKind correctForFalse = checkTypeCorrectKind(module, typeArena, builtinTypes, node, position, builtinTypes->falseType);
        TypeCorrectKind correctForFunction =
            functionIsExpectedAt(module, node, position).value_or(false) ? TypeCorrectKind::Correct : TypeCorrectKind::None;

        result["if"] = {AutocompleteEntryKind::Keyword, std::nullopt, false, false};
        result["true"] = {AutocompleteEntryKind::Keyword, builtinTypes->booleanType, false, false, correctForTrue};
        result["false"] = {AutocompleteEntryKind::Keyword, builtinTypes->booleanType, false, false, correctForFalse};
        result["nil"] = {AutocompleteEntryKind::Keyword, builtinTypes->nilType, false, false, correctForNil};
        result["not"] = {AutocompleteEntryKind::Keyword};
        result["function"] = {AutocompleteEntryKind::Keyword, std::nullopt, false, false, correctForFunction};

        if (auto ty = findExpectedTypeAt(module, node, position))
            autocompleteStringSingleton(*ty, true, node, position, result);
    }

    return AutocompleteContext::Expression;
}

static AutocompleteResult autocompleteExpression(
    const Module& module,
    NotNull<BuiltinTypes> builtinTypes,
    TypeArena* typeArena,
    const std::vector<AstNode*>& ancestry,
    const ScopePtr& scopeAtPosition,
    Position position
)
{
    AutocompleteEntryMap result;
    AutocompleteContext context = autocompleteExpression(module, builtinTypes, typeArena, ancestry, scopeAtPosition, position, result);
    return {std::move(result), ancestry, context};
}

static std::optional<const ExternType*> getMethodContainingExternType(const ModulePtr& module, AstExpr* funcExpr)
{
    AstExpr* parentExpr = nullptr;
    if (auto indexName = funcExpr->as<AstExprIndexName>())
    {
        parentExpr = indexName->expr;
    }
    else if (auto indexExpr = funcExpr->as<AstExprIndexExpr>())
    {
        parentExpr = indexExpr->expr;
    }
    else
    {
        return std::nullopt;
    }

    auto parentIt = module->astTypes.find(parentExpr);
    if (!parentIt)
    {
        return std::nullopt;
    }

    Luau::TypeId parentType = Luau::follow(*parentIt);

    if (auto parentExternType = Luau::get<ExternType>(parentType))
    {
        return parentExternType;
    }

    if (auto parentUnion = Luau::get<UnionType>(parentType))
    {
        return returnFirstNonnullOptionOfType<ExternType>(parentUnion);
    }

    return std::nullopt;
}

static bool stringPartOfInterpString(const AstNode* node, Position position)
{
    const AstExprInterpString* interpString = node->as<AstExprInterpString>();
    if (!interpString)
    {
        return false;
    }

    for (const AstExpr* expression : interpString->expressions)
    {
        if (expression->location.containsClosed(position))
        {
            return false;
        }
    }

    return true;
}

static bool isSimpleInterpolatedString(const AstNode* node)
{
    const AstExprInterpString* interpString = node->as<AstExprInterpString>();
    return interpString != nullptr && interpString->expressions.size == 0;
}

static std::optional<std::string> getStringContents(const AstNode* node)
{
    if (const AstExprConstantString* string = node->as<AstExprConstantString>())
    {
        return std::string(string->value.data, string->value.size);
    }
    else if (const AstExprInterpString* interpString = node->as<AstExprInterpString>(); interpString && interpString->expressions.size == 0)
    {
        LUAU_ASSERT(interpString->strings.size == 1);
        return std::string(interpString->strings.data->data, interpString->strings.data->size);
    }
    else
    {
        return std::nullopt;
    }
}

static std::optional<AutocompleteEntryMap> convertRequireSuggestionsToAutocompleteEntryMap(std::optional<RequireSuggestions> suggestions)
{
    if (!suggestions)
        return std::nullopt;

    AutocompleteEntryMap result;
    for (RequireSuggestion& suggestion : *suggestions)
    {
        AutocompleteEntry entry = {AutocompleteEntryKind::RequirePath};
        entry.insertText = std::move(suggestion.fullPath);
        entry.tags = std::move(suggestion.tags);
        result[std::move(suggestion.label)] = std::move(entry);
    }
    return result;
}

static std::optional<AutocompleteEntryMap> autocompleteStringParams(
    const ModulePtr& module,
    const std::vector<AstNode*>& nodes,
    Position position,
    FileResolver* fileResolver,
    StringCompletionCallback callback
)
{
    if (nodes.size() < 2)
    {
        return std::nullopt;
    }

    if (!nodes.back()->is<AstExprConstantString>() && !isSimpleInterpolatedString(nodes.back()) && !nodes.back()->is<AstExprError>())
    {
        return std::nullopt;
    }

    if (!nodes.back()->is<AstExprError>())
    {
        if (nodes.back()->location.end == position || nodes.back()->location.begin == position)
        {
            return std::nullopt;
        }
    }

    AstExprCall* candidate = nodes.at(nodes.size() - 2)->as<AstExprCall>();
    if (!candidate)
    {
        return std::nullopt;
    }

    // HACK: All current instances of 'magic string' params are the first parameter of their functions,
    // so we encode that here rather than putting a useless member on the FunctionType struct.
    if (candidate->args.size > 1 && !candidate->args.data[0]->location.contains(position))
    {
        return std::nullopt;
    }

    auto it = module->astTypes.find(candidate->func);
    if (!it)
    {
        return std::nullopt;
    }

    std::optional<std::string> candidateString = getStringContents(nodes.back());

    auto performCallback = [&](const FunctionType* funcType) -> std::optional<AutocompleteEntryMap>
    {
        for (const std::string& tag : funcType->tags)
        {
            if (tag == kRequireTagName && fileResolver)
            {
                return convertRequireSuggestionsToAutocompleteEntryMap(fileResolver->getRequireSuggestions(module->name, candidateString));
            }
            if (std::optional<AutocompleteEntryMap> ret = callback(tag, getMethodContainingExternType(module, candidate->func), candidateString))
            {
                return ret;
            }
        }
        return std::nullopt;
    };

    auto followedId = Luau::follow(*it);
    if (auto functionType = Luau::get<FunctionType>(followedId))
    {
        return performCallback(functionType);
    }

    if (auto intersect = Luau::get<IntersectionType>(followedId))
    {
        for (TypeId part : intersect->parts)
        {
            part = follow(part);
            if (auto candidateFunctionType = Luau::get<FunctionType>(part))
            {
                if (std::optional<AutocompleteEntryMap> ret = performCallback(candidateFunctionType))
                {
                    return ret;
                }
            }
        }
    }

    return std::nullopt;
}

static AutocompleteResult autocompleteWhileLoopKeywords(std::vector<AstNode*> ancestry)
{
    AutocompleteEntryMap ret;
    ret["do"] = {AutocompleteEntryKind::Keyword};
    ret["and"] = {AutocompleteEntryKind::Keyword};
    ret["or"] = {AutocompleteEntryKind::Keyword};
    return {std::move(ret), std::move(ancestry), AutocompleteContext::Keyword};
}

static std::string makeAnonymous(const ScopePtr& scope, const FunctionType& funcTy)
{
    std::string result = "function(";

    auto [args, tail] = Luau::flatten(funcTy.argTypes);

    bool first = true;
    // Skip the implicit 'self' argument if call is indexed with ':'
    for (size_t argIdx = 0; argIdx < args.size(); ++argIdx)
    {
        if (!first)
            result += ", ";
        else
            first = false;

        std::string name;
        if (argIdx < funcTy.argNames.size() && funcTy.argNames[argIdx])
            name = funcTy.argNames[argIdx]->name;
        else
            name = "a" + std::to_string(argIdx);

        if (std::optional<Name> type = tryGetTypeNameInScope(scope, args[argIdx], true))
            result += name + ": " + *type;
        else
            result += name;
    }

    if (tail && (Luau::isVariadic(*tail) || Luau::get<Luau::FreeTypePack>(Luau::follow(*tail))))
    {
        if (!first)
            result += ", ";

        std::optional<std::string> varArgType;
        if (const VariadicTypePack* pack = get<VariadicTypePack>(follow(*tail)))
        {
            if (std::optional<std::string> res = tryToStringDetailed(scope, pack->ty, true))
                varArgType = std::move(res);
        }

        if (varArgType)
            result += "...: " + *varArgType;
        else
            result += "...";
    }

    result += ")";

    auto [rets, retTail] = Luau::flatten(funcTy.retTypes);
    if (const size_t totalRetSize = rets.size() + (retTail ? 1 : 0); totalRetSize > 0)
    {
        if (std::optional<std::string> returnTypes = tryToStringDetailed(scope, funcTy.retTypes, true))
        {
            result += ": ";
            bool wrap = totalRetSize != 1;
            if (wrap)
                result += "(";
            result += *returnTypes;
            if (wrap)
                result += ")";
        }
    }
    result += "  end";
    return result;
}

static std::optional<AutocompleteEntry> makeAnonymousAutofilled(
    const ModulePtr& module,
    const ScopePtr& scopeAtPosition,
    Position position,
    const AstNode* node,
    const std::vector<AstNode*>& ancestry
)
{
    const AstExprCall* call = node->as<AstExprCall>();
    if (!call && ancestry.size() > 1)
        call = ancestry[ancestry.size() - 2]->as<AstExprCall>();

    if (!call)
        return std::nullopt;

    if (!call->location.containsClosed(position) || call->func->location.containsClosed(position))
        return std::nullopt;

    TypeId* typeIter = module->astTypes.find(call->func);
    if (!typeIter)
        return std::nullopt;

    const FunctionType* outerFunction = get<FunctionType>(follow(*typeIter));
    if (!outerFunction)
        return std::nullopt;

    size_t argument = 0;
    for (size_t i = 0; i < call->args.size; ++i)
    {
        if (call->args.data[i]->location.containsClosed(position))
        {
            argument = i;
            break;
        }
    }

    if (call->self)
        argument++;

    std::optional<TypeId> argType;
    auto [args, tail] = flatten(outerFunction->argTypes);
    if (argument < args.size())
        argType = args[argument];

    if (!argType)
        return std::nullopt;

    TypeId followed = follow(*argType);
    const FunctionType* type = get<FunctionType>(followed);
    if (!type)
    {
        if (const UnionType* unionType = get<UnionType>(followed))
        {
            if (std::optional<const FunctionType*> nonnullFunction = returnFirstNonnullOptionOfType<FunctionType>(unionType))
                type = *nonnullFunction;
        }
    }

    if (!type)
        return std::nullopt;

    const ScopePtr scope = scopeAtPosition;
    if (!scope)
        return std::nullopt;

    AutocompleteEntry entry;
    entry.kind = AutocompleteEntryKind::GeneratedFunction;
    entry.typeCorrect = TypeCorrectKind::Correct;
    entry.type = argType;
    entry.insertText = makeAnonymous(scope, *type);
    return std::make_optional(std::move(entry));
}

AutocompleteResult autocomplete_(
    const ModulePtr& module,
    NotNull<BuiltinTypes> builtinTypes,
    TypeArena* typeArena,
    std::vector<AstNode*>& ancestry,
    Scope* globalScope, // [TODO] This is unused argument, do we really need this?
    const ScopePtr& scopeAtPosition,
    Position position,
    FileResolver* fileResolver,
    StringCompletionCallback callback,
    bool isInHotComment
)
{
    LUAU_TIMETRACE_SCOPE("Luau::autocomplete_", "AutocompleteCore");

    if (FFlag::LuauSuggestHotComments)
    {
        if (isInHotComment)
        {
            AutocompleteEntryMap result;

            for (const std::string_view hc : kHotComments)
                result.emplace(hc, AutocompleteEntry{AutocompleteEntryKind::HotComment});
            return {std::move(result), ancestry, AutocompleteContext::HotComment};
        }
    }

    AstNode* node = ancestry.back();

    AstExprConstantNil dummy{Location{}};
    AstNode* parent = ancestry.size() >= 2 ? ancestry.rbegin()[1] : &dummy;

    // If we are inside a body of a function that doesn't have a completed argument list, ignore the body node
    if (auto exprFunction = parent->as<AstExprFunction>(); exprFunction && !exprFunction->argLocation && node == exprFunction->body)
    {
        ancestry.pop_back();

        node = ancestry.back();
        parent = ancestry.size() >= 2 ? ancestry.rbegin()[1] : &dummy;
    }

    if (auto indexName = node->as<AstExprIndexName>())
    {
        auto it = module->astTypes.find(indexName->expr);
        if (!it)
            return {};

        TypeId ty = follow(*it);
        PropIndexType indexType = indexName->op == ':' ? PropIndexType::Colon : PropIndexType::Point;

        return {autocompleteProps(*module, typeArena, builtinTypes, ty, indexType, ancestry), ancestry, AutocompleteContext::Property};
    }
    else if (auto typeReference = node->as<AstTypeReference>())
    {
        if (typeReference->prefix)
            return {autocompleteModuleTypes(*module, scopeAtPosition, position, typeReference->prefix->value), ancestry, AutocompleteContext::Type};
        else
            return {autocompleteTypeNames(*module, scopeAtPosition, position, ancestry), ancestry, AutocompleteContext::Type};
    }
    else if (node->is<AstTypeError>())
    {
        return {autocompleteTypeNames(*module, scopeAtPosition, position, ancestry), ancestry, AutocompleteContext::Type};
    }
    else if (AstStatLocal* statLocal = node->as<AstStatLocal>())
    {
        if (statLocal->vars.size == 1 && (!statLocal->equalsSignLocation || position < statLocal->equalsSignLocation->begin))
            return {{{"function", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, ancestry, AutocompleteContext::Unknown};
        else if (statLocal->equalsSignLocation && position >= statLocal->equalsSignLocation->end)
            return autocompleteExpression(*module, builtinTypes, typeArena, ancestry, scopeAtPosition, position);
        else
            return {};
    }

    else if (AstStatFor* statFor = extractStat<AstStatFor>(ancestry))
    {
        if (!statFor->hasDo || position < statFor->doLocation.begin)
        {
            if (statFor->from->location.containsClosed(position) || statFor->to->location.containsClosed(position) ||
                (statFor->step && statFor->step->location.containsClosed(position)))
                return autocompleteExpression(*module, builtinTypes, typeArena, ancestry, scopeAtPosition, position);

            if (!statFor->from->is<AstExprError>() && !statFor->to->is<AstExprError>() && (!statFor->step || !statFor->step->is<AstExprError>()))
                return {{{"do", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, ancestry, AutocompleteContext::Keyword};
            return {};
        }

        return {autocompleteStatement(*module, ancestry, scopeAtPosition, position), ancestry, AutocompleteContext::Statement};
    }

    else if (AstStatForIn* statForIn = parent->as<AstStatForIn>(); statForIn && (node->is<AstStatBlock>() || isIdentifier(node)))
    {
        if (!statForIn->hasIn || position <= statForIn->inLocation.begin)
        {
            AstLocal* lastName = statForIn->vars.data[statForIn->vars.size - 1];
            if (lastName->name == kParseNameError || lastName->location.containsClosed(position))
            {
                // Here we are either working with a missing binding (as would be the case in a bare "for" keyword) or
                // the cursor is still touching a binding name.  The user is still typing a new name, so we should not offer
                // any suggestions.
                return {};
            }

            return {{{"in", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, ancestry, AutocompleteContext::Keyword};
        }

        if (!statForIn->hasDo || position <= statForIn->doLocation.begin)
        {
            LUAU_ASSERT(statForIn->values.size > 0);
            AstExpr* lastExpr = statForIn->values.data[statForIn->values.size - 1];

            if (lastExpr->location.containsClosed(position))
                return autocompleteExpression(*module, builtinTypes, typeArena, ancestry, scopeAtPosition, position);

            if (position > lastExpr->location.end)
                return {{{"do", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, ancestry, AutocompleteContext::Keyword};

            return {}; // Not sure what this means
        }
    }
    else if (AstStatForIn* statForIn = extractStat<AstStatForIn>(ancestry))
    {
        // The AST looks a bit differently if the cursor is at a position where only the "do" keyword is allowed.
        // ex "for f in f do"
        if (!statForIn->hasDo)
            return {{{"do", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, ancestry, AutocompleteContext::Keyword};

        return {autocompleteStatement(*module, ancestry, scopeAtPosition, position), ancestry, AutocompleteContext::Statement};
    }

    else if (AstStatWhile* statWhile = parent->as<AstStatWhile>(); node->is<AstStatBlock>() && statWhile)
    {
        if (!statWhile->hasDo && !statWhile->condition->is<AstStatError>() && position > statWhile->condition->location.end)
        {
            return autocompleteWhileLoopKeywords(ancestry);
        }

        if (!statWhile->hasDo || position < statWhile->doLocation.begin)
            return autocompleteExpression(*module, builtinTypes, typeArena, ancestry, scopeAtPosition, position);

        if (statWhile->hasDo && position > statWhile->doLocation.end)
            return {autocompleteStatement(*module, ancestry, scopeAtPosition, position), ancestry, AutocompleteContext::Statement};
    }

    else if (AstStatWhile* statWhile = extractStat<AstStatWhile>(ancestry);
             (statWhile && (!statWhile->hasDo || statWhile->doLocation.containsClosed(position)) && statWhile->condition &&
              !statWhile->condition->location.containsClosed(position)))
    {
        return autocompleteWhileLoopKeywords(ancestry);
    }
    else if (AstStatIf* statIf = node->as<AstStatIf>(); statIf && !statIf->elseLocation.has_value())
    {
        return {
            {{"else", AutocompleteEntry{AutocompleteEntryKind::Keyword}}, {"elseif", AutocompleteEntry{AutocompleteEntryKind::Keyword}}},
            ancestry,
            AutocompleteContext::Keyword
        };
    }
    else if (AstStatIf* statIf = parent->as<AstStatIf>(); statIf && node->is<AstStatBlock>())
    {
        if (statIf->condition->is<AstExprError>())
            return autocompleteExpression(*module, builtinTypes, typeArena, ancestry, scopeAtPosition, position);
        else if (!statIf->thenLocation || statIf->thenLocation->containsClosed(position))
            return {{{"then", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, ancestry, AutocompleteContext::Keyword};
    }
    else if (AstStatIf* statIf = extractStat<AstStatIf>(ancestry); statIf &&
                                                                   (!statIf->thenLocation || statIf->thenLocation->containsClosed(position)) &&
                                                                   (statIf->condition && !statIf->condition->location.containsClosed(position)))
    {
        AutocompleteEntryMap ret;
        ret["then"] = {AutocompleteEntryKind::Keyword};
        ret["and"] = {AutocompleteEntryKind::Keyword};
        ret["or"] = {AutocompleteEntryKind::Keyword};
        return {std::move(ret), ancestry, AutocompleteContext::Keyword};
    }
    else if (AstStatRepeat* statRepeat = node->as<AstStatRepeat>(); statRepeat && statRepeat->condition->is<AstExprError>())
        return autocompleteExpression(*module, builtinTypes, typeArena, ancestry, scopeAtPosition, position);
    else if (AstStatRepeat* statRepeat = extractStat<AstStatRepeat>(ancestry); statRepeat)
        return {autocompleteStatement(*module, ancestry, scopeAtPosition, position), ancestry, AutocompleteContext::Statement};
    else if (AstExprTable* exprTable = parent->as<AstExprTable>();
             exprTable && (node->is<AstExprGlobal>() || node->is<AstExprConstantString>() || node->is<AstExprInterpString>()))
    {
        for (const auto& [kind, key, value] : exprTable->items)
        {
            // If item doesn't have a key, maybe the value is actually the key
            if (key ? key == node : node->is<AstExprGlobal>() && value == node)
            {
                if (auto it = module->astExpectedTypes.find(exprTable))
                {
                    auto result = autocompleteProps(*module, typeArena, builtinTypes, *it, PropIndexType::Key, ancestry);

                    if (auto nodeIt = module->astExpectedTypes.find(node->asExpr()))
                        autocompleteStringSingleton(*nodeIt, !node->is<AstExprConstantString>(), node, position, result);

                    if (!key)
                    {
                        // If there is "no key," it may be that the user
                        // intends for the current token to be the key, but
                        // has yet to type the `=` sign.
                        //
                        // If the key type is a union of singleton strings,
                        // suggest those too.
                        if (auto ttv = get<TableType>(follow(*it)); ttv && ttv->indexer)
                        {
                            autocompleteStringSingleton(ttv->indexer->indexType, false, node, position, result);
                        }
                    }

                    // Remove keys that are already completed
                    for (const auto& item : exprTable->items)
                    {
                        if (!item.key)
                            continue;

                        if (auto stringKey = item.key->as<AstExprConstantString>())
                            result.erase(std::string(stringKey->value.data, stringKey->value.size));
                    }

                    // If we know for sure that a key is being written, do not offer general expression suggestions
                    if (!key)
                        autocompleteExpression(*module, builtinTypes, typeArena, ancestry, scopeAtPosition, position, result);

                    return {std::move(result), ancestry, AutocompleteContext::Property};
                }

                break;
            }
        }
    }
    else if (AstExprTable* exprTable = node->as<AstExprTable>())
    {
        AutocompleteEntryMap result;

        if (auto it = module->astExpectedTypes.find(exprTable))
        {
            result = autocompleteProps(*module, typeArena, builtinTypes, *it, PropIndexType::Key, ancestry);

            // If the key type is a union of singleton strings,
            // suggest those too.
            if (auto ttv = get<TableType>(follow(*it)); ttv && ttv->indexer)
            {
                autocompleteStringSingleton(ttv->indexer->indexType, false, node, position, result);
            }

            // Remove keys that are already completed
            for (const auto& item : exprTable->items)
            {
                if (!item.key)
                    continue;

                if (auto stringKey = item.key->as<AstExprConstantString>())
                    result.erase(std::string(stringKey->value.data, stringKey->value.size));
            }
        }

        // Also offer general expression suggestions
        autocompleteExpression(*module, builtinTypes, typeArena, ancestry, scopeAtPosition, position, result);

        return {std::move(result), ancestry, AutocompleteContext::Property};
    }
    else if (isIdentifier(node) && (parent->is<AstStatExpr>() || parent->is<AstStatError>()))
        return {autocompleteStatement(*module, ancestry, scopeAtPosition, position), ancestry, AutocompleteContext::Statement};

    if (std::optional<AutocompleteEntryMap> ret = autocompleteStringParams(module, ancestry, position, fileResolver, std::move(callback)))
    {
        return {*ret, ancestry, AutocompleteContext::String};
    }
    else if (node->is<AstExprConstantString>() || isSimpleInterpolatedString(node))
    {
        AutocompleteEntryMap result;

        if (ancestry.size() >= 2)
        {
            if (auto idxExpr = ancestry.at(ancestry.size() - 2)->as<AstExprIndexExpr>())
            {
                if (auto it = module->astTypes.find(idxExpr->expr))
                    autocompleteProps(*module, typeArena, builtinTypes, follow(*it), PropIndexType::Point, ancestry, result);
            }
            else if (auto binExpr = ancestry.at(ancestry.size() - 2)->as<AstExprBinary>())
            {
                if (binExpr->op == AstExprBinary::CompareEq || binExpr->op == AstExprBinary::CompareNe)
                {
                    if (auto it = module->astTypes.find(node == binExpr->left ? binExpr->right : binExpr->left))
                        autocompleteStringSingleton(*it, false, node, position, result);
                }
            }
        }

        if (auto it = module->astExpectedTypes.find(node->asExpr()))
            autocompleteStringSingleton(*it, false, node, position, result);

        return {std::move(result), ancestry, AutocompleteContext::String};
    }
    else if (stringPartOfInterpString(node, position))
    {
        // We're not a simple interpolated string, we're something like `a{"b"}@1`, and we
        // can't know what to format to
        AutocompleteEntryMap map;
        return {std::move(map), ancestry, AutocompleteContext::String};
    }
    else if (AstExprFunction* func = node->as<AstExprFunction>())
    {
        if (FFlag::LuauAutocompleteAttributes)
        {
            for (AstAttr* attr : func->attributes)
            {
                if (attr->location.begin <= position && position <= attr->location.end && attr->type == AstAttr::Type::Unknown)
                {
                    AutocompleteEntryMap ret;
                    for (const auto& attr : kKnownAttributes)
                        ret[attr.c_str()] = {AutocompleteEntryKind::Keyword};
                    return {std::move(ret), std::move(ancestry), AutocompleteContext::Keyword};
                }
            }
        }
    }

    if (node->is<AstExprConstantNumber>())
        return {};

    if (node->asExpr())
    {
        AutocompleteResult ret = autocompleteExpression(*module, builtinTypes, typeArena, ancestry, scopeAtPosition, position);
        if (std::optional<AutocompleteEntry> generated = makeAnonymousAutofilled(module, scopeAtPosition, position, node, ancestry))
            ret.entryMap[kGeneratedAnonymousFunctionEntryName] = std::move(*generated);
        return ret;
    }
    else if (node->asStat())
        return {autocompleteStatement(*module, ancestry, scopeAtPosition, position), ancestry, AutocompleteContext::Statement};

    return {};
}

} // namespace Luau
