// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Autocomplete.h"

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Frontend.h"
#include "Luau/ToString.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"

#include <algorithm>
#include <unordered_set>
#include <utility>

LUAU_FASTFLAG(LuauUseCommittingTxnLog)
LUAU_FASTFLAGVARIABLE(LuauAutocompleteAvoidMutation, false);
LUAU_FASTFLAGVARIABLE(LuauCompleteBrokenStringParams, false);
LUAU_FASTFLAGVARIABLE(LuauMissingFollowACMetatables, false);
LUAU_FASTFLAGVARIABLE(PreferToCallFunctionsForIntersects, false);

static const std::unordered_set<std::string> kStatementStartingKeywords = {
    "while", "if", "local", "repeat", "function", "do", "for", "return", "break", "continue", "type", "export"};

namespace Luau
{

struct NodeFinder : public AstVisitor
{
    const Position pos;
    std::vector<AstNode*> ancestry;

    explicit NodeFinder(Position pos, AstNode* root)
        : pos(pos)
    {
    }

    bool visit(AstExpr* expr) override
    {
        if (expr->location.begin < pos && pos <= expr->location.end)
        {
            ancestry.push_back(expr);
            return true;
        }
        return false;
    }

    bool visit(AstStat* stat) override
    {
        if (stat->location.begin < pos && pos <= stat->location.end)
        {
            ancestry.push_back(stat);
            return true;
        }
        return false;
    }

    bool visit(AstType* type) override
    {
        if (type->location.begin < pos && pos <= type->location.end)
        {
            ancestry.push_back(type);
            return true;
        }
        return false;
    }

    bool visit(AstTypeError* type) override
    {
        // For a missing type, match the whole range including the start position
        if (type->isMissing && type->location.containsClosed(pos))
        {
            ancestry.push_back(type);
            return true;
        }
        return false;
    }

    bool visit(class AstTypePack* typePack) override
    {
        return true;
    }

    bool visit(AstStatBlock* block) override
    {
        // If ancestry is empty, we are inspecting the root of the AST.  Its extent is considered to be infinite.
        if (ancestry.empty())
        {
            ancestry.push_back(block);
            return true;
        }

        // AstExprIndexName nodes are nested outside-in, so we want the outermost node in the case of nested nodes.
        // ex foo.bar.baz is represented in the AST as IndexName{ IndexName {foo, bar}, baz}
        if (!ancestry.empty() && ancestry.back()->is<AstExprIndexName>())
            return false;

        // Type annotation error might intersect the block statement when the function header is being written,
        // annotation takes priority
        if (!ancestry.empty() && ancestry.back()->is<AstTypeError>())
            return false;

        // If the cursor is at the end of an expression or type and simultaneously at the beginning of a block,
        // the expression or type wins out.
        // The exception to this is if we are in a block under an AstExprFunction.  In this case, we consider the position to
        // be within the block.
        if (block->location.begin == pos && !ancestry.empty())
        {
            if (ancestry.back()->asExpr() && !ancestry.back()->is<AstExprFunction>())
                return false;

            if (ancestry.back()->asType())
                return false;
        }

        if (block->location.begin <= pos && pos <= block->location.end)
        {
            ancestry.push_back(block);
            return true;
        }
        return false;
    }
};

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

static ParenthesesRecommendation getParenRecommendationForFunc(const FunctionTypeVar* func, const std::vector<AstNode*>& nodes)
{
    if (alreadyHasParens(nodes))
    {
        return ParenthesesRecommendation::None;
    }

    auto idxExpr = nodes.back()->as<AstExprIndexName>();
    bool hasImplicitSelf = idxExpr && idxExpr->op == ':';
    auto args = Luau::flatten(func->argTypes);
    bool noArgFunction = (args.first.empty() || (hasImplicitSelf && args.first.size() == 1)) && !args.second.has_value();
    return noArgFunction ? ParenthesesRecommendation::CursorAfter : ParenthesesRecommendation::CursorInside;
}

static ParenthesesRecommendation getParenRecommendationForIntersect(const IntersectionTypeVar* intersect, const std::vector<AstNode*>& nodes)
{
    ParenthesesRecommendation rec = ParenthesesRecommendation::None;
    for (Luau::TypeId partId : intersect->parts)
    {
        if (auto partFunc = Luau::get<FunctionTypeVar>(partId))
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
    if (auto func = get<FunctionTypeVar>(id))
    {
        return getParenRecommendationForFunc(func, nodes);
    }
    else if (auto intersect = get<IntersectionTypeVar>(id))
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

            const FunctionTypeVar* ftv = get<FunctionTypeVar>(follow(*it));

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

static TypeCorrectKind checkTypeCorrectKind(const Module& module, TypeArena* typeArena, AstNode* node, Position position, TypeId ty)
{
    ty = follow(ty);

    auto canUnify = [&typeArena, &module](TypeId subTy, TypeId superTy) {
        InternalErrorReporter iceReporter;
        UnifierSharedState unifierState(&iceReporter);
        Unifier unifier(typeArena, Mode::Strict, module.getModuleScope(), Location(), Variance::Covariant, unifierState);

        if (FFlag::LuauAutocompleteAvoidMutation && !FFlag::LuauUseCommittingTxnLog)
        {
            SeenTypes seenTypes;
            SeenTypePacks seenTypePacks;
            CloneState cloneState;
            superTy = clone(superTy, *typeArena, seenTypes, seenTypePacks, cloneState);
            subTy = clone(subTy, *typeArena, seenTypes, seenTypePacks, cloneState);

            auto errors = unifier.canUnify(subTy, superTy);
            return errors.empty();
        }
        else
        {
            unifier.tryUnify(subTy, superTy);

            bool ok = unifier.errors.empty();

            if (!FFlag::LuauUseCommittingTxnLog)
                unifier.DEPRECATED_log.rollback();

            return ok;
        }
    };

    auto typeAtPosition = findExpectedTypeAt(module, node, position);

    if (!typeAtPosition)
        return TypeCorrectKind::None;

    TypeId expectedType = follow(*typeAtPosition);

    if (FFlag::PreferToCallFunctionsForIntersects)
    {
        auto checkFunctionType = [&canUnify, &expectedType](const FunctionTypeVar* ftv) {
            auto [retHead, retTail] = flatten(ftv->retType);

            if (!retHead.empty() && canUnify(retHead.front(), expectedType))
                return true;

            // We might only have a variadic tail pack, check if the element is compatible
            if (retTail)
            {
                if (const VariadicTypePack* vtp = get<VariadicTypePack>(follow(*retTail)); vtp && canUnify(vtp->ty, expectedType))
                    return true;
            }

            return false;
        };

        // We also want to suggest functions that return compatible result
        if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty); ftv && checkFunctionType(ftv))
        {
            return TypeCorrectKind::CorrectFunctionResult;
        }
        else if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(ty))
        {
            for (TypeId id : itv->parts)
            {
                if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(id); ftv && checkFunctionType(ftv))
                {
                    return TypeCorrectKind::CorrectFunctionResult;
                }
            }
        }
    }
    else
    {
        // We also want to suggest functions that return compatible result
        if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty))
        {
            auto [retHead, retTail] = flatten(ftv->retType);

            if (!retHead.empty() && canUnify(retHead.front(), expectedType))
                return TypeCorrectKind::CorrectFunctionResult;

            // We might only have a variadic tail pack, check if the element is compatible
            if (retTail)
            {
                if (const VariadicTypePack* vtp = get<VariadicTypePack>(follow(*retTail)); vtp && canUnify(vtp->ty, expectedType))
                    return TypeCorrectKind::CorrectFunctionResult;
            }
        }
    }

    return canUnify(ty, expectedType) ? TypeCorrectKind::Correct : TypeCorrectKind::None;
}

enum class PropIndexType
{
    Point,
    Colon,
    Key,
};

static void autocompleteProps(const Module& module, TypeArena* typeArena, TypeId ty, PropIndexType indexType, const std::vector<AstNode*>& nodes,
    AutocompleteEntryMap& result, std::unordered_set<TypeId>& seen, std::optional<const ClassTypeVar*> containingClass = std::nullopt)
{
    ty = follow(ty);

    if (seen.count(ty))
        return;
    seen.insert(ty);

    auto isWrongIndexer = [indexType, useStrictFunctionIndexers = !!get<ClassTypeVar>(ty)](Luau::TypeId type) {
        if (indexType == PropIndexType::Key)
            return false;

        bool colonIndex = indexType == PropIndexType::Colon;

        if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(type))
        {
            return useStrictFunctionIndexers ? colonIndex != ftv->hasSelf : false;
        }
        else if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(type))
        {
            bool allHaveSelf = true;
            for (auto subType : itv->parts)
            {
                if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(Luau::follow(subType)))
                {
                    allHaveSelf &= ftv->hasSelf;
                }
                else
                {
                    return colonIndex;
                }
            }
            return useStrictFunctionIndexers ? colonIndex != allHaveSelf : false;
        }
        else
        {
            return colonIndex;
        }
    };

    auto fillProps = [&](const ClassTypeVar::Props& props) {
        for (const auto& [name, prop] : props)
        {
            // We are walking up the class hierarchy, so if we encounter a property that we have
            // already populated, it takes precedence over the property we found just now.
            if (result.count(name) == 0 && name != Parser::errorName)
            {
                Luau::TypeId type = Luau::follow(prop.type);
                TypeCorrectKind typeCorrect = indexType == PropIndexType::Key ? TypeCorrectKind::Correct
                                                                              : checkTypeCorrectKind(module, typeArena, nodes.back(), {{}, {}}, type);
                ParenthesesRecommendation parens =
                    indexType == PropIndexType::Key ? ParenthesesRecommendation::None : getParenRecommendation(type, nodes, typeCorrect);

                result[name] = AutocompleteEntry{
                    AutocompleteEntryKind::Property,
                    type,
                    prop.deprecated,
                    isWrongIndexer(type),
                    typeCorrect,
                    containingClass,
                    &prop,
                    prop.documentationSymbol,
                    {},
                    parens,
                };
            }
        }
    };

    if (auto cls = get<ClassTypeVar>(ty))
    {
        containingClass = containingClass.value_or(cls);
        fillProps(cls->props);
        if (cls->parent)
            autocompleteProps(module, typeArena, *cls->parent, indexType, nodes, result, seen, cls);
    }
    else if (auto tbl = get<TableTypeVar>(ty))
        fillProps(tbl->props);
    else if (auto mt = get<MetatableTypeVar>(ty))
    {
        autocompleteProps(module, typeArena, mt->table, indexType, nodes, result, seen);

        auto mtable = get<TableTypeVar>(mt->metatable);
        if (!mtable)
            return;

        auto indexIt = mtable->props.find("__index");
        if (indexIt != mtable->props.end())
        {
            if (FFlag::LuauMissingFollowACMetatables)
            {
                TypeId followed = follow(indexIt->second.type);
                if (get<TableTypeVar>(followed) || get<MetatableTypeVar>(followed))
                    autocompleteProps(module, typeArena, followed, indexType, nodes, result, seen);
                else if (auto indexFunction = get<FunctionTypeVar>(followed))
                {
                    std::optional<TypeId> indexFunctionResult = first(indexFunction->retType);
                    if (indexFunctionResult)
                        autocompleteProps(module, typeArena, *indexFunctionResult, indexType, nodes, result, seen);
                }
            }
            else
            {
                if (get<TableTypeVar>(indexIt->second.type) || get<MetatableTypeVar>(indexIt->second.type))
                    autocompleteProps(module, typeArena, indexIt->second.type, indexType, nodes, result, seen);
                else if (auto indexFunction = get<FunctionTypeVar>(indexIt->second.type))
                {
                    std::optional<TypeId> indexFunctionResult = first(indexFunction->retType);
                    if (indexFunctionResult)
                        autocompleteProps(module, typeArena, *indexFunctionResult, indexType, nodes, result, seen);
                }
            }
        }
    }
    else if (auto i = get<IntersectionTypeVar>(ty))
    {
        // Complete all properties in every variant
        for (TypeId ty : i->parts)
        {
            AutocompleteEntryMap inner;
            std::unordered_set<TypeId> innerSeen = seen;

            autocompleteProps(module, typeArena, ty, indexType, nodes, inner, innerSeen);

            for (auto& pair : inner)
                result.insert(pair);
        }
    }
    else if (auto u = get<UnionTypeVar>(ty))
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

        autocompleteProps(module, typeArena, *iter, indexType, nodes, result, seen);

        ++iter;

        while (iter != endIter)
        {
            AutocompleteEntryMap inner;
            std::unordered_set<TypeId> innerSeen = seen;

            if (isNil(*iter))
            {
                ++iter;
                continue;
            }

            autocompleteProps(module, typeArena, *iter, indexType, nodes, inner, innerSeen);

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
}

static void autocompleteKeywords(
    const SourceModule& sourceModule, const std::vector<AstNode*>& ancestry, Position position, AutocompleteEntryMap& result)
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
    const Module& module, TypeArena* typeArena, TypeId ty, PropIndexType indexType, const std::vector<AstNode*>& nodes, AutocompleteEntryMap& result)
{
    std::unordered_set<TypeId> seen;
    autocompleteProps(module, typeArena, ty, indexType, nodes, result, seen);
}

AutocompleteEntryMap autocompleteProps(
    const Module& module, TypeArena* typeArena, TypeId ty, PropIndexType indexType, const std::vector<AstNode*>& nodes)
{
    AutocompleteEntryMap result;
    autocompleteProps(module, typeArena, ty, indexType, nodes, result);
    return result;
}

AutocompleteEntryMap autocompleteModuleTypes(const Module& module, Position position, std::string_view moduleName)
{
    AutocompleteEntryMap result;

    for (ScopePtr scope = findScopeAtPosition(module, position); scope; scope = scope->parent)
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

static bool canSuggestInferredType(ScopePtr scope, TypeId ty)
{
    ty = follow(ty);

    // No point in suggesting 'any', invalid to suggest others
    if (get<AnyTypeVar>(ty) || get<ErrorTypeVar>(ty) || get<GenericTypeVar>(ty) || get<FreeTypeVar>(ty))
        return false;

    // No syntax for unnamed tables with a metatable
    if (get<MetatableTypeVar>(ty))
        return false;

    if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
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

static std::optional<TypeId> findTypeElementAt(AstType* astType, TypeId ty, Position position)
{
    ty = follow(ty);

    if (astType->is<AstTypeReference>())
        return ty;

    if (astType->is<AstTypeError>())
        return ty;

    if (AstTypeFunction* type = astType->as<AstTypeFunction>())
    {
        const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty);

        if (!ftv)
            return {};

        if (auto element = findTypeElementAt(type->argTypes, ftv->argTypes, position))
            return element;

        if (auto element = findTypeElementAt(type->returnTypes, ftv->retType, position))
            return element;
    }

    // It's possible to walk through other types like intrsection and unions if we find value in doing that
    return {};
}

std::optional<TypeId> getLocalTypeInScopeAt(const Module& module, Position position, AstLocal* local)
{
    if (ScopePtr scope = findScopeAtPosition(module, position))
    {
        for (const auto& [name, binding] : scope->bindings)
        {
            if (name == local)
                return binding.typeId;
        }
    }

    return {};
}

static std::optional<Name> tryGetTypeNameInScope(ScopePtr scope, TypeId ty)
{
    if (!canSuggestInferredType(scope, ty))
        return std::nullopt;

    ToStringOptions opts;
    opts.useLineBreaks = false;
    opts.hideTableKind = true;
    opts.scope = scope;
    ToStringResult name = toStringDetailed(ty, opts);

    if (name.error || name.invalid || name.cycle || name.truncated)
        return std::nullopt;

    return name.name;
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

    if (auto name = tryGetTypeNameInScope(scope, *ty))
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
std::optional<const T*> returnFirstNonnullOptionOfType(const UnionTypeVar* utv)
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

    if (get<FunctionTypeVar>(expectedType))
        return true;

    if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(expectedType))
    {
        return std::all_of(begin(itv->parts), end(itv->parts), [](auto&& ty) {
            return get<FunctionTypeVar>(Luau::follow(ty)) != nullptr;
        });
    }

    if (const UnionTypeVar* utv = get<UnionTypeVar>(expectedType))
        return returnFirstNonnullOptionOfType<FunctionTypeVar>(utv).has_value();

    return false;
}

AutocompleteEntryMap autocompleteTypeNames(const Module& module, Position position, const std::vector<AstNode*>& ancestry)
{
    AutocompleteEntryMap result;

    ScopePtr startScope = findScopeAtPosition(module, position);

    for (ScopePtr scope = startScope; scope; scope = scope->parent)
    {
        for (const auto& [name, ty] : scope->exportedTypeBindings)
        {
            if (!result.count(name))
                result[name] = AutocompleteEntry{AutocompleteEntryKind::Type, ty.type, false, false, TypeCorrectKind::None, std::nullopt,
                    std::nullopt, ty.type->documentationSymbol};
        }

        for (const auto& [name, ty] : scope->privateTypeBindings)
        {
            if (!result.count(name))
                result[name] = AutocompleteEntry{AutocompleteEntryKind::Type, ty.type, false, false, TypeCorrectKind::None, std::nullopt,
                    std::nullopt, ty.type->documentationSymbol};
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
    AstType* topType = nullptr;

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
                        if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(follow(*it)))
                        {
                            if (auto ty = tryGetTypePackTypeAt(ftv->retType, tailPos))
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
                    tryAddTypeCorrectSuggestion(result, startScope, topType, inferredType, position);

                break;
            }
        }
    }
    else if (AstExprFunction* node = parent->as<AstExprFunction>())
    {
        // For lookup inside expected function type if that's available
        auto tryGetExpectedFunctionType = [](const Module& module, AstExpr* expr) -> const FunctionTypeVar* {
            auto it = module.astExpectedTypes.find(expr);

            if (!it)
                return nullptr;

            TypeId ty = follow(*it);

            if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty))
                return ftv;

            // Handle optional function type
            if (const UnionTypeVar* utv = get<UnionTypeVar>(ty))
            {
                return returnFirstNonnullOptionOfType<FunctionTypeVar>(utv).value_or(nullptr);
            }

            return nullptr;
        };

        // Find which argument type we are defining
        for (size_t i = 0; i < node->args.size; i++)
        {
            AstLocal* arg = node->args.data[i];

            if (arg->annotation && arg->annotation->location.containsClosed(position))
            {
                if (const FunctionTypeVar* ftv = tryGetExpectedFunctionType(module, node))
                {
                    if (auto ty = tryGetTypePackTypeAt(ftv->argTypes, i))
                        tryAddTypeCorrectSuggestion(result, startScope, topType, *ty, position);
                }
                // Otherwise, try to use the type inferred by typechecker
                else if (auto inferredType = getLocalTypeInScopeAt(module, position, arg))
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
                    if (const FunctionTypeVar* ftv = tryGetExpectedFunctionType(module, node))
                    {
                        if (auto ty = tryGetTypePackTypeAt(ftv->argTypes, ~0u))
                            tryAddTypeCorrectSuggestion(result, startScope, topType, *ty, position);
                    }
                }
            }
        }

        for (size_t i = 0; i < node->returnAnnotation.types.size; i++)
        {
            AstType* ret = node->returnAnnotation.types.data[i];

            if (ret->location.containsClosed(position))
            {
                if (const FunctionTypeVar* ftv = tryGetExpectedFunctionType(module, node))
                {
                    if (auto ty = tryGetTypePackTypeAt(ftv->retType, i))
                        tryAddTypeCorrectSuggestion(result, startScope, topType, *ty, position);
                }

                // TODO: with additional type information, we could suggest inferred return type here
                break;
            }
        }

        if (AstTypePack* retTp = node->returnAnnotation.tailType)
        {
            if (auto variadic = retTp->as<AstTypePackVariadic>())
            {
                if (variadic->location.containsClosed(position))
                {
                    if (const FunctionTypeVar* ftv = tryGetExpectedFunctionType(module, node))
                    {
                        if (auto ty = tryGetTypePackTypeAt(ftv->retType, ~0u))
                            tryAddTypeCorrectSuggestion(result, startScope, topType, *ty, position);
                    }
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

    if (T* t = parent->as<T>(); t && parent->is<AstStatBlock>())
        return t;

    AstNode* grandParent = ancestry.size() >= 3 ? ancestry.rbegin()[2] : nullptr;
    AstNode* greatGrandParent = ancestry.size() >= 4 ? ancestry.rbegin()[3] : nullptr;
    if (!grandParent || !greatGrandParent)
        return nullptr;

    if (T* t = greatGrandParent->as<T>(); t && grandParent->is<AstStatBlock>() && parent->is<AstStatError>() && isIdentifier(node))
        return t;

    return nullptr;
}

static bool isBindingLegalAtCurrentPosition(const Binding& binding, Position pos)
{
    // Default Location used for global bindings, which are always legal.
    return binding.location == Location() || binding.location.end < pos;
}

static AutocompleteEntryMap autocompleteStatement(
    const SourceModule& sourceModule, const Module& module, const std::vector<AstNode*>& ancestry, Position position)
{
    // This is inefficient. :(
    ScopePtr scope = findScopeAtPosition(module, position);

    AutocompleteEntryMap result;

    if (isInLocalNames(ancestry, position))
    {
        autocompleteKeywords(sourceModule, ancestry, position, result);
        return result;
    }

    while (scope)
    {
        for (const auto& [name, binding] : scope->bindings)
        {
            if (!isBindingLegalAtCurrentPosition(binding, position))
                continue;

            std::string n = toString(name);
            if (!result.count(n))
                result[n] = {AutocompleteEntryKind::Binding, binding.typeId, binding.deprecated, false, TypeCorrectKind::None, std::nullopt,
                    std::nullopt, binding.documentationSymbol, {}, getParenRecommendation(binding.typeId, ancestry, TypeCorrectKind::None)};
        }

        scope = scope->parent;
    }

    for (const auto& kw : kStatementStartingKeywords)
        result.emplace(kw, AutocompleteEntry{AutocompleteEntryKind::Keyword});

    for (auto it = ancestry.rbegin(); it != ancestry.rend(); ++it)
    {
        if (AstStatForIn* statForIn = (*it)->as<AstStatForIn>(); statForIn && !statForIn->hasEnd)
            result.emplace("end", AutocompleteEntry{AutocompleteEntryKind::Keyword});
        if (AstStatFor* statFor = (*it)->as<AstStatFor>(); statFor && !statFor->hasEnd)
            result.emplace("end", AutocompleteEntry{AutocompleteEntryKind::Keyword});
        if (AstStatIf* statIf = (*it)->as<AstStatIf>(); statIf && !statIf->hasEnd)
            result.emplace("end", AutocompleteEntry{AutocompleteEntryKind::Keyword});
        if (AstStatWhile* statWhile = (*it)->as<AstStatWhile>(); statWhile && !statWhile->hasEnd)
            result.emplace("end", AutocompleteEntry{AutocompleteEntryKind::Keyword});
        if (AstExprFunction* exprFunction = (*it)->as<AstExprFunction>(); exprFunction && !exprFunction->hasEnd)
            result.emplace("end", AutocompleteEntry{AutocompleteEntryKind::Keyword});
    }

    if (ancestry.size() >= 2)
    {
        AstNode* parent = ancestry.rbegin()[1];
        if (AstStatIf* statIf = parent->as<AstStatIf>())
        {
            if (!statIf->elsebody || (statIf->hasElse && statIf->elseLocation.containsClosed(position)))
            {
                result.emplace("else", AutocompleteEntry{AutocompleteEntryKind::Keyword});
                result.emplace("elseif", AutocompleteEntry{AutocompleteEntryKind::Keyword});
            }
        }

        if (AstStatRepeat* statRepeat = parent->as<AstStatRepeat>(); statRepeat && !statRepeat->hasUntil)
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

    if (AstStatRepeat* statRepeat = extractStat<AstStatRepeat>(ancestry); statRepeat && !statRepeat->hasUntil)
        result.emplace("until", AutocompleteEntry{AutocompleteEntryKind::Keyword});

    return result;
}

// Returns true if completions were generated (completions will be inserted into 'outResult')
// Returns false if no completions were generated
static bool autocompleteIfElseExpression(
    const AstNode* node, const std::vector<AstNode*>& ancestry, const Position& position, AutocompleteEntryMap& outResult)
{
    AstNode* parent = ancestry.size() >= 2 ? ancestry.rbegin()[1] : nullptr;
    if (!parent)
        return false;

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

static void autocompleteExpression(const SourceModule& sourceModule, const Module& module, const TypeChecker& typeChecker, TypeArena* typeArena,
    const std::vector<AstNode*>& ancestry, Position position, AutocompleteEntryMap& result)
{
    LUAU_ASSERT(!ancestry.empty());

    AstNode* node = ancestry.rbegin()[0];

    if (node->is<AstExprIndexName>())
    {
        if (auto it = module.astTypes.find(node->asExpr()))
            autocompleteProps(module, typeArena, *it, PropIndexType::Point, ancestry, result);
    }
    else if (autocompleteIfElseExpression(node, ancestry, position, result))
        return;
    else if (node->is<AstExprFunction>())
        return;
    else
    {
        // This is inefficient. :(
        ScopePtr scope = findScopeAtPosition(module, position);

        while (scope)
        {
            for (const auto& [name, binding] : scope->bindings)
            {
                if (!isBindingLegalAtCurrentPosition(binding, position))
                    continue;

                if (isBeingDefined(ancestry, name))
                    continue;

                std::string n = toString(name);
                if (!result.count(n))
                {
                    TypeCorrectKind typeCorrect = checkTypeCorrectKind(module, typeArena, node, position, binding.typeId);

                    result[n] = {AutocompleteEntryKind::Binding, binding.typeId, binding.deprecated, false, typeCorrect, std::nullopt, std::nullopt,
                        binding.documentationSymbol, {}, getParenRecommendation(binding.typeId, ancestry, typeCorrect)};
                }
            }

            scope = scope->parent;
        }

        TypeCorrectKind correctForNil = checkTypeCorrectKind(module, typeArena, node, position, typeChecker.nilType);
        TypeCorrectKind correctForBoolean = checkTypeCorrectKind(module, typeArena, node, position, typeChecker.booleanType);
        TypeCorrectKind correctForFunction =
            functionIsExpectedAt(module, node, position).value_or(false) ? TypeCorrectKind::Correct : TypeCorrectKind::None;

        result["if"] = {AutocompleteEntryKind::Keyword, std::nullopt, false, false};
        result["true"] = {AutocompleteEntryKind::Keyword, typeChecker.booleanType, false, false, correctForBoolean};
        result["false"] = {AutocompleteEntryKind::Keyword, typeChecker.booleanType, false, false, correctForBoolean};
        result["nil"] = {AutocompleteEntryKind::Keyword, typeChecker.nilType, false, false, correctForNil};
        result["not"] = {AutocompleteEntryKind::Keyword};
        result["function"] = {AutocompleteEntryKind::Keyword, std::nullopt, false, false, correctForFunction};
    }
}

static AutocompleteEntryMap autocompleteExpression(const SourceModule& sourceModule, const Module& module, const TypeChecker& typeChecker,
    TypeArena* typeArena, const std::vector<AstNode*>& ancestry, Position position)
{
    AutocompleteEntryMap result;
    autocompleteExpression(sourceModule, module, typeChecker, typeArena, ancestry, position, result);
    return result;
}

static std::optional<const ClassTypeVar*> getMethodContainingClass(const ModulePtr& module, AstExpr* funcExpr)
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

    if (auto parentClass = Luau::get<ClassTypeVar>(parentType))
    {
        return parentClass;
    }

    if (auto parentUnion = Luau::get<UnionTypeVar>(parentType))
    {
        return returnFirstNonnullOptionOfType<ClassTypeVar>(parentUnion);
    }

    return std::nullopt;
}

static std::optional<AutocompleteEntryMap> autocompleteStringParams(const SourceModule& sourceModule, const ModulePtr& module,
    const std::vector<AstNode*>& nodes, Position position, StringCompletionCallback callback)
{
    if (nodes.size() < 2)
    {
        return std::nullopt;
    }

    if (!nodes.back()->is<AstExprConstantString>() && (!FFlag::LuauCompleteBrokenStringParams || !nodes.back()->is<AstExprError>()))
    {
        return std::nullopt;
    }

    AstExprCall* candidate = nodes.at(nodes.size() - 2)->as<AstExprCall>();
    if (!candidate)
    {
        return std::nullopt;
    }

    // HACK: All current instances of 'magic string' params are the first parameter of their functions,
    // so we encode that here rather than putting a useless member on the FunctionTypeVar struct.
    if (candidate->args.size > 1 && !candidate->args.data[0]->location.contains(position))
    {
        return std::nullopt;
    }

    auto it = module->astTypes.find(candidate->func);
    if (!it)
    {
        return std::nullopt;
    }

    auto performCallback = [&](const FunctionTypeVar* funcType) -> std::optional<AutocompleteEntryMap> {
        for (const std::string& tag : funcType->tags)
        {
            if (std::optional<AutocompleteEntryMap> ret = callback(tag, getMethodContainingClass(module, candidate->func)))
            {
                return ret;
            }
        }
        return std::nullopt;
    };

    auto followedId = Luau::follow(*it);
    if (auto functionType = Luau::get<FunctionTypeVar>(followedId))
    {
        return performCallback(functionType);
    }

    if (auto intersect = Luau::get<IntersectionTypeVar>(followedId))
    {
        for (TypeId part : intersect->parts)
        {
            if (auto candidateFunctionType = Luau::get<FunctionTypeVar>(part))
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

static AutocompleteResult autocomplete(const SourceModule& sourceModule, const ModulePtr& module, const TypeChecker& typeChecker,
    TypeArena* typeArena, Position position, StringCompletionCallback callback)
{
    if (isWithinComment(sourceModule, position))
        return {};

    NodeFinder finder{position, sourceModule.root};
    sourceModule.root->visit(&finder);
    LUAU_ASSERT(!finder.ancestry.empty());
    AstNode* node = finder.ancestry.back();

    AstExprConstantNil dummy{Location{}};
    AstNode* parent = finder.ancestry.size() >= 2 ? finder.ancestry.rbegin()[1] : &dummy;

    // If we are inside a body of a function that doesn't have a completed argument list, ignore the body node
    if (auto exprFunction = parent->as<AstExprFunction>(); exprFunction && !exprFunction->argLocation && node == exprFunction->body)
    {
        finder.ancestry.pop_back();

        node = finder.ancestry.back();
        parent = finder.ancestry.size() >= 2 ? finder.ancestry.rbegin()[1] : &dummy;
    }

    if (auto indexName = node->as<AstExprIndexName>())
    {
        auto it = module->astTypes.find(indexName->expr);
        if (!it)
            return {};

        TypeId ty = follow(*it);
        PropIndexType indexType = indexName->op == ':' ? PropIndexType::Colon : PropIndexType::Point;

        if (isString(ty))
            return {autocompleteProps(*module, typeArena, typeChecker.globalScope->bindings[AstName{"string"}].typeId, indexType, finder.ancestry),
                finder.ancestry};
        else
            return {autocompleteProps(*module, typeArena, ty, indexType, finder.ancestry), finder.ancestry};
    }
    else if (auto typeReference = node->as<AstTypeReference>())
    {
        if (typeReference->hasPrefix)
            return {autocompleteModuleTypes(*module, position, typeReference->prefix.value), finder.ancestry};
        else
            return {autocompleteTypeNames(*module, position, finder.ancestry), finder.ancestry};
    }
    else if (node->is<AstTypeError>())
    {
        return {autocompleteTypeNames(*module, position, finder.ancestry), finder.ancestry};
    }
    else if (AstStatLocal* statLocal = node->as<AstStatLocal>())
    {
        if (statLocal->vars.size == 1 && (!statLocal->hasEqualsSign || position < statLocal->equalsSignLocation.begin))
            return {{{"function", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, finder.ancestry};
        else if (statLocal->hasEqualsSign && position >= statLocal->equalsSignLocation.end)
            return {autocompleteExpression(sourceModule, *module, typeChecker, typeArena, finder.ancestry, position), finder.ancestry};
        else
            return {};
    }

    else if (AstStatFor* statFor = extractStat<AstStatFor>(finder.ancestry))
    {
        if (!statFor->hasDo || position < statFor->doLocation.begin)
        {
            if (!statFor->from->is<AstExprError>() && !statFor->to->is<AstExprError>() && (!statFor->step || !statFor->step->is<AstExprError>()))
                return {{{"do", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, finder.ancestry};

            if (statFor->from->location.containsClosed(position) || statFor->to->location.containsClosed(position) ||
                (statFor->step && statFor->step->location.containsClosed(position)))
                return {autocompleteExpression(sourceModule, *module, typeChecker, typeArena, finder.ancestry, position), finder.ancestry};

            return {};
        }

        return {autocompleteStatement(sourceModule, *module, finder.ancestry, position), finder.ancestry};
    }

    else if (AstStatForIn* statForIn = parent->as<AstStatForIn>(); statForIn && (node->is<AstStatBlock>() || isIdentifier(node)))
    {
        if (!statForIn->hasIn || position <= statForIn->inLocation.begin)
        {
            AstLocal* lastName = statForIn->vars.data[statForIn->vars.size - 1];
            if (lastName->name == Parser::errorName || lastName->location.containsClosed(position))
            {
                // Here we are either working with a missing binding (as would be the case in a bare "for" keyword) or
                // the cursor is still touching a binding name.  The user is still typing a new name, so we should not offer
                // any suggestions.
                return {};
            }

            return {{{"in", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, finder.ancestry};
        }

        if (!statForIn->hasDo || position <= statForIn->doLocation.begin)
        {
            LUAU_ASSERT(statForIn->values.size > 0);
            AstExpr* lastExpr = statForIn->values.data[statForIn->values.size - 1];

            if (lastExpr->location.containsClosed(position))
                return {autocompleteExpression(sourceModule, *module, typeChecker, typeArena, finder.ancestry, position), finder.ancestry};

            if (position > lastExpr->location.end)
                return {{{"do", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, finder.ancestry};

            return {}; // Not sure what this means
        }
    }
    else if (AstStatForIn* statForIn = extractStat<AstStatForIn>(finder.ancestry))
    {
        // The AST looks a bit differently if the cursor is at a position where only the "do" keyword is allowed.
        // ex "for f in f do"
        if (!statForIn->hasDo)
            return {{{"do", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, finder.ancestry};

        return {autocompleteStatement(sourceModule, *module, finder.ancestry, position), finder.ancestry};
    }

    else if (AstStatWhile* statWhile = parent->as<AstStatWhile>(); node->is<AstStatBlock>() && statWhile)
    {
        if (!statWhile->hasDo && !statWhile->condition->is<AstStatError>() && position > statWhile->condition->location.end)
            return {{{"do", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, finder.ancestry};

        if (!statWhile->hasDo || position < statWhile->doLocation.begin)
            return {autocompleteExpression(sourceModule, *module, typeChecker, typeArena, finder.ancestry, position), finder.ancestry};

        if (statWhile->hasDo && position > statWhile->doLocation.end)
            return {autocompleteStatement(sourceModule, *module, finder.ancestry, position), finder.ancestry};
    }

    else if (AstStatWhile* statWhile = extractStat<AstStatWhile>(finder.ancestry); statWhile && !statWhile->hasDo)
        return {{{"do", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, finder.ancestry};

    else if (AstStatIf* statIf = node->as<AstStatIf>(); statIf && !statIf->hasElse)
    {
        return {{{"else", AutocompleteEntry{AutocompleteEntryKind::Keyword}}, {"elseif", AutocompleteEntry{AutocompleteEntryKind::Keyword}}},
            finder.ancestry};
    }
    else if (AstStatIf* statIf = parent->as<AstStatIf>(); statIf && node->is<AstStatBlock>())
    {
        if (statIf->condition->is<AstExprError>())
            return {autocompleteExpression(sourceModule, *module, typeChecker, typeArena, finder.ancestry, position), finder.ancestry};
        else if (!statIf->hasThen || statIf->thenLocation.containsClosed(position))
            return {{{"then", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, finder.ancestry};
    }
    else if (AstStatIf* statIf = extractStat<AstStatIf>(finder.ancestry);
             statIf && (!statIf->hasThen || statIf->thenLocation.containsClosed(position)))
        return {{{"then", AutocompleteEntry{AutocompleteEntryKind::Keyword}}}, finder.ancestry};
    else if (AstStatRepeat* statRepeat = node->as<AstStatRepeat>(); statRepeat && statRepeat->condition->is<AstExprError>())
        return {autocompleteExpression(sourceModule, *module, typeChecker, typeArena, finder.ancestry, position), finder.ancestry};
    else if (AstStatRepeat* statRepeat = extractStat<AstStatRepeat>(finder.ancestry); statRepeat)
        return {autocompleteStatement(sourceModule, *module, finder.ancestry, position), finder.ancestry};
    else if (AstExprTable* exprTable = parent->as<AstExprTable>(); exprTable && (node->is<AstExprGlobal>() || node->is<AstExprConstantString>()))
    {
        for (const auto& [kind, key, value] : exprTable->items)
        {
            // If item doesn't have a key, maybe the value is actually the key
            if (key ? key == node : node->is<AstExprGlobal>() && value == node)
            {
                if (auto it = module->astExpectedTypes.find(exprTable))
                {
                    auto result = autocompleteProps(*module, typeArena, *it, PropIndexType::Key, finder.ancestry);

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
                        autocompleteExpression(sourceModule, *module, typeChecker, typeArena, finder.ancestry, position, result);

                    return {result, finder.ancestry};
                }

                break;
            }
        }
    }
    else if (isIdentifier(node) && (parent->is<AstStatExpr>() || parent->is<AstStatError>()))
        return {autocompleteStatement(sourceModule, *module, finder.ancestry, position), finder.ancestry};

    if (std::optional<AutocompleteEntryMap> ret = autocompleteStringParams(sourceModule, module, finder.ancestry, position, callback))
    {
        return {*ret, finder.ancestry};
    }
    else if (node->is<AstExprConstantString>())
    {
        if (finder.ancestry.size() >= 2)
        {
            if (auto idxExpr = finder.ancestry.at(finder.ancestry.size() - 2)->as<AstExprIndexExpr>())
            {
                if (auto it = module->astTypes.find(idxExpr->expr))
                {
                    return {autocompleteProps(*module, typeArena, follow(*it), PropIndexType::Point, finder.ancestry), finder.ancestry};
                }
            }
        }
        return {};
    }

    if (node->is<AstExprConstantNumber>())
    {
        return {};
    }

    if (node->asExpr())
        return {autocompleteExpression(sourceModule, *module, typeChecker, typeArena, finder.ancestry, position), finder.ancestry};
    else if (node->asStat())
        return {autocompleteStatement(sourceModule, *module, finder.ancestry, position), finder.ancestry};

    return {};
}

AutocompleteResult autocomplete(Frontend& frontend, const ModuleName& moduleName, Position position, StringCompletionCallback callback)
{
    // FIXME: We can improve performance here by parsing without checking.
    // The old type graph is probably fine. (famous last words!)
    // FIXME: We don't need to typecheck for script analysis here, just for autocomplete.
    frontend.check(moduleName);

    const SourceModule* sourceModule = frontend.getSourceModule(moduleName);
    if (!sourceModule)
        return {};

    TypeChecker& typeChecker = (frontend.options.typecheckTwice ? frontend.typeCheckerForAutocomplete : frontend.typeChecker);
    ModulePtr module = (frontend.options.typecheckTwice ? frontend.moduleResolverForAutocomplete.getModule(moduleName)
                                                        : frontend.moduleResolver.getModule(moduleName));

    if (!module)
        return {};

    AutocompleteResult autocompleteResult = autocomplete(*sourceModule, module, typeChecker, &frontend.arenaForAutocomplete, position, callback);

    frontend.arenaForAutocomplete.clear();

    return autocompleteResult;
}

OwningAutocompleteResult autocompleteSource(Frontend& frontend, std::string_view source, Position position, StringCompletionCallback callback)
{
    auto sourceModule = std::make_unique<SourceModule>();
    ParseOptions parseOptions;
    parseOptions.captureComments = true;
    ParseResult result = Parser::parse(source.data(), source.size(), *sourceModule->names, *sourceModule->allocator, parseOptions);

    if (!result.root)
        return {AutocompleteResult{}, {}, nullptr};

    sourceModule->name = "FRAGMENT_SCRIPT";
    sourceModule->root = result.root;
    sourceModule->mode = Mode::Strict;
    sourceModule->commentLocations = std::move(result.commentLocations);

    TypeChecker& typeChecker = (frontend.options.typecheckTwice ? frontend.typeCheckerForAutocomplete : frontend.typeChecker);

    ModulePtr module = typeChecker.check(*sourceModule, Mode::Strict);

    OwningAutocompleteResult autocompleteResult = {
        autocomplete(*sourceModule, module, typeChecker, &frontend.arenaForAutocomplete, position, callback), std::move(module),
        std::move(sourceModule)};

    frontend.arenaForAutocomplete.clear();

    return autocompleteResult;
}

} // namespace Luau
