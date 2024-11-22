// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Differ.h"
#include "Luau/Common.h"
#include "Luau/Error.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/Unifiable.h"
#include <optional>
#include <string>
#include <unordered_set>
#include <vector>

namespace Luau
{

std::string DiffPathNode::toString() const
{
    switch (kind)
    {
    case DiffPathNode::Kind::TableProperty:
    {
        if (!tableProperty.has_value())
            throw InternalCompilerError{"DiffPathNode has kind TableProperty but tableProperty is nullopt"};
        return *tableProperty;
        break;
    }
    case DiffPathNode::Kind::FunctionArgument:
    {
        if (!index.has_value())
            return "Arg[Variadic]";
        // Add 1 because Lua is 1-indexed
        return "Arg[" + std::to_string(*index + 1) + "]";
    }
    case DiffPathNode::Kind::FunctionReturn:
    {
        if (!index.has_value())
            return "Ret[Variadic]";
        // Add 1 because Lua is 1-indexed
        return "Ret[" + std::to_string(*index + 1) + "]";
    }
    case DiffPathNode::Kind::Negation:
    {
        return "Negation";
    }
    default:
    {
        throw InternalCompilerError{"DiffPathNode::toString is not exhaustive"};
    }
    }
}

DiffPathNode DiffPathNode::constructWithTableProperty(Name tableProperty)
{
    return DiffPathNode{DiffPathNode::Kind::TableProperty, tableProperty, std::nullopt};
}

DiffPathNode DiffPathNode::constructWithKindAndIndex(Kind kind, size_t index)
{
    return DiffPathNode{kind, std::nullopt, index};
}

DiffPathNode DiffPathNode::constructWithKind(Kind kind)
{
    return DiffPathNode{kind, std::nullopt, std::nullopt};
}

DiffPathNodeLeaf DiffPathNodeLeaf::detailsNormal(TypeId ty)
{
    return DiffPathNodeLeaf{ty, std::nullopt, std::nullopt, false, std::nullopt};
}

DiffPathNodeLeaf DiffPathNodeLeaf::detailsTableProperty(TypeId ty, Name tableProperty)
{
    return DiffPathNodeLeaf{ty, tableProperty, std::nullopt, false, std::nullopt};
}

DiffPathNodeLeaf DiffPathNodeLeaf::detailsUnionIndex(TypeId ty, size_t index)
{
    return DiffPathNodeLeaf{ty, std::nullopt, std::nullopt, false, index};
}

DiffPathNodeLeaf DiffPathNodeLeaf::detailsLength(int minLength, bool isVariadic)
{
    return DiffPathNodeLeaf{std::nullopt, std::nullopt, minLength, isVariadic, std::nullopt};
}

DiffPathNodeLeaf DiffPathNodeLeaf::nullopts()
{
    return DiffPathNodeLeaf{std::nullopt, std::nullopt, std::nullopt, false, std::nullopt};
}

std::string DiffPath::toString(bool prependDot) const
{
    std::string pathStr;
    bool isFirstInForLoop = !prependDot;
    for (auto node = path.rbegin(); node != path.rend(); node++)
    {
        if (isFirstInForLoop)
        {
            isFirstInForLoop = false;
        }
        else
        {
            pathStr += ".";
        }
        pathStr += node->toString();
    }
    return pathStr;
}
std::string DiffError::toStringALeaf(std::string rootName, const DiffPathNodeLeaf& leaf, const DiffPathNodeLeaf& otherLeaf, bool multiLine) const
{
    std::string conditionalNewline = multiLine ? "\n" : " ";
    std::string conditionalIndent = multiLine ? "    " : "";
    std::string pathStr{rootName + diffPath.toString(true)};
    switch (kind)
    {
    case DiffError::Kind::Normal:
    {
        checkNonMissingPropertyLeavesHaveNulloptTableProperty();
        return pathStr + conditionalNewline + "has type" + conditionalNewline + conditionalIndent + Luau::toString(*leaf.ty);
    }
    case DiffError::Kind::MissingTableProperty:
    {
        if (leaf.ty.has_value())
        {
            if (!leaf.tableProperty.has_value())
                throw InternalCompilerError{"leaf.tableProperty is nullopt"};
            return pathStr + "." + *leaf.tableProperty + conditionalNewline + "has type" + conditionalNewline + conditionalIndent +
                   Luau::toString(*leaf.ty);
        }
        else if (otherLeaf.ty.has_value())
        {
            if (!otherLeaf.tableProperty.has_value())
                throw InternalCompilerError{"otherLeaf.tableProperty is nullopt"};
            return pathStr + conditionalNewline + "is missing the property" + conditionalNewline + conditionalIndent + *otherLeaf.tableProperty;
        }
        throw InternalCompilerError{"Both leaf.ty and otherLeaf.ty is nullopt"};
    }
    case DiffError::Kind::MissingUnionMember:
    {
        // TODO: do normal case
        if (leaf.ty.has_value())
        {
            if (!leaf.unionIndex.has_value())
                throw InternalCompilerError{"leaf.unionIndex is nullopt"};
            return pathStr + conditionalNewline + "is a union containing type" + conditionalNewline + conditionalIndent + Luau::toString(*leaf.ty);
        }
        else if (otherLeaf.ty.has_value())
        {
            return pathStr + conditionalNewline + "is a union missing type" + conditionalNewline + conditionalIndent + Luau::toString(*otherLeaf.ty);
        }
        throw InternalCompilerError{"Both leaf.ty and otherLeaf.ty is nullopt"};
    }
    case DiffError::Kind::MissingIntersectionMember:
    {
        // TODO: better message for intersections
        // An intersection of just functions is always an "overloaded function"
        // An intersection of just tables is always a "joined table"
        if (leaf.ty.has_value())
        {
            if (!leaf.unionIndex.has_value())
                throw InternalCompilerError{"leaf.unionIndex is nullopt"};
            return pathStr + conditionalNewline + "is an intersection containing type" + conditionalNewline + conditionalIndent +
                   Luau::toString(*leaf.ty);
        }
        else if (otherLeaf.ty.has_value())
        {
            return pathStr + conditionalNewline + "is an intersection missing type" + conditionalNewline + conditionalIndent +
                   Luau::toString(*otherLeaf.ty);
        }
        throw InternalCompilerError{"Both leaf.ty and otherLeaf.ty is nullopt"};
    }
    case DiffError::Kind::LengthMismatchInFnArgs:
    {
        if (!leaf.minLength.has_value())
            throw InternalCompilerError{"leaf.minLength is nullopt"};
        return pathStr + conditionalNewline + "takes " + std::to_string(*leaf.minLength) + (leaf.isVariadic ? " or more" : "") + " arguments";
    }
    case DiffError::Kind::LengthMismatchInFnRets:
    {
        if (!leaf.minLength.has_value())
            throw InternalCompilerError{"leaf.minLength is nullopt"};
        return pathStr + conditionalNewline + "returns " + std::to_string(*leaf.minLength) + (leaf.isVariadic ? " or more" : "") + " values";
    }
    default:
    {
        throw InternalCompilerError{"DiffPath::toStringALeaf is not exhaustive"};
    }
    }
}

void DiffError::checkNonMissingPropertyLeavesHaveNulloptTableProperty() const
{
    if (left.tableProperty.has_value() || right.tableProperty.has_value())
        throw InternalCompilerError{"Non-MissingProperty DiffError should have nullopt tableProperty in both leaves"};
}

std::string getDevFixFriendlyName(const std::optional<std::string>& maybeSymbol, TypeId ty)
{
    if (maybeSymbol.has_value())
        return *maybeSymbol;

    if (auto table = get<TableType>(ty))
    {
        if (table->name.has_value())
            return *table->name;
        else if (table->syntheticName.has_value())
            return *table->syntheticName;
    }
    if (auto metatable = get<MetatableType>(ty))
    {
        if (metatable->syntheticName.has_value())
        {
            return *metatable->syntheticName;
        }
    }
    return "<unlabeled-symbol>";
}

std::string DifferEnvironment::getDevFixFriendlyNameLeft() const
{
    return getDevFixFriendlyName(externalSymbolLeft, rootLeft);
}

std::string DifferEnvironment::getDevFixFriendlyNameRight() const
{
    return getDevFixFriendlyName(externalSymbolRight, rootRight);
}

std::string DiffError::toString(bool multiLine) const
{
    std::string conditionalNewline = multiLine ? "\n" : " ";
    std::string conditionalIndent = multiLine ? "    " : "";
    switch (kind)
    {
    case DiffError::Kind::IncompatibleGeneric:
    {
        std::string diffPathStr{diffPath.toString(true)};
        return "DiffError: these two types are not equal because the left generic at" + conditionalNewline + conditionalIndent + leftRootName +
               diffPathStr + conditionalNewline + "cannot be the same type parameter as the right generic at" + conditionalNewline +
               conditionalIndent + rightRootName + diffPathStr;
    }
    default:
    {
        return "DiffError: these two types are not equal because the left type at" + conditionalNewline + conditionalIndent +
               toStringALeaf(leftRootName, left, right, multiLine) + "," + conditionalNewline + "while the right type at" + conditionalNewline +
               conditionalIndent + toStringALeaf(rightRootName, right, left, multiLine);
    }
    }
}

void DiffError::checkValidInitialization(const DiffPathNodeLeaf& left, const DiffPathNodeLeaf& right)
{
    if (!left.ty.has_value() || !right.ty.has_value())
    {
        // TODO: think about whether this should be always thrown!
        // For example, Kind::Primitive doesn't make too much sense to have a TypeId
        // throw InternalCompilerError{"Left and Right fields are leaf nodes and must have a TypeId"};
    }
}

void DifferResult::wrapDiffPath(DiffPathNode node)
{
    if (!diffError.has_value())
    {
        throw InternalCompilerError{"Cannot wrap diffPath because there is no diffError"};
    }

    diffError->diffPath.path.push_back(node);
}

static DifferResult diffUsingEnv(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffTable(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffMetatable(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffPrimitive(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffSingleton(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffFunction(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffGeneric(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffNegation(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffClass(DifferEnvironment& env, TypeId left, TypeId right);
struct FindSeteqCounterexampleResult
{
    // nullopt if no counterexample found
    std::optional<size_t> mismatchIdx;
    // true if counterexample is in the left, false if cex is in the right
    bool inLeft;
};
static FindSeteqCounterexampleResult findSeteqCounterexample(
    DifferEnvironment& env,
    const std::vector<TypeId>& left,
    const std::vector<TypeId>& right
);
static DifferResult diffUnion(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffIntersection(DifferEnvironment& env, TypeId left, TypeId right);
/**
 * The last argument gives context info on which complex type contained the TypePack.
 */
static DifferResult diffTpi(DifferEnvironment& env, DiffError::Kind possibleNonNormalErrorKind, TypePackId left, TypePackId right);
static DifferResult diffCanonicalTpShape(
    DifferEnvironment& env,
    DiffError::Kind possibleNonNormalErrorKind,
    const std::pair<std::vector<TypeId>, std::optional<TypePackId>>& left,
    const std::pair<std::vector<TypeId>, std::optional<TypePackId>>& right
);
static DifferResult diffHandleFlattenedTail(DifferEnvironment& env, DiffError::Kind possibleNonNormalErrorKind, TypePackId left, TypePackId right);
static DifferResult diffGenericTp(DifferEnvironment& env, TypePackId left, TypePackId right);

static DifferResult diffTable(DifferEnvironment& env, TypeId left, TypeId right)
{
    const TableType* leftTable = get<TableType>(left);
    const TableType* rightTable = get<TableType>(right);
    LUAU_ASSERT(leftTable);
    LUAU_ASSERT(rightTable);

    for (auto const& [field, value] : leftTable->props)
    {
        if (rightTable->props.find(field) == rightTable->props.end())
        {
            // left has a field the right doesn't
            return DifferResult{DiffError{
                DiffError::Kind::MissingTableProperty,
                DiffPathNodeLeaf::detailsTableProperty(value.type(), field),
                DiffPathNodeLeaf::nullopts(),
                env.getDevFixFriendlyNameLeft(),
                env.getDevFixFriendlyNameRight(),
            }};
        }
    }
    for (auto const& [field, value] : rightTable->props)
    {
        if (leftTable->props.find(field) == leftTable->props.end())
        {
            // right has a field the left doesn't
            return DifferResult{DiffError{
                DiffError::Kind::MissingTableProperty,
                DiffPathNodeLeaf::nullopts(),
                DiffPathNodeLeaf::detailsTableProperty(value.type(), field),
                env.getDevFixFriendlyNameLeft(),
                env.getDevFixFriendlyNameRight()
            }};
        }
    }
    // left and right have the same set of keys
    for (auto const& [field, leftValue] : leftTable->props)
    {
        auto const& rightValue = rightTable->props.at(field);
        DifferResult differResult = diffUsingEnv(env, leftValue.type(), rightValue.type());
        if (differResult.diffError.has_value())
        {
            differResult.wrapDiffPath(DiffPathNode::constructWithTableProperty(field));
            return differResult;
        }
    }
    return DifferResult{};
}

static DifferResult diffMetatable(DifferEnvironment& env, TypeId left, TypeId right)
{
    const MetatableType* leftMetatable = get<MetatableType>(left);
    const MetatableType* rightMetatable = get<MetatableType>(right);
    LUAU_ASSERT(leftMetatable);
    LUAU_ASSERT(rightMetatable);

    DifferResult diffRes = diffUsingEnv(env, leftMetatable->table, rightMetatable->table);
    if (diffRes.diffError.has_value())
    {
        return diffRes;
    }

    diffRes = diffUsingEnv(env, leftMetatable->metatable, rightMetatable->metatable);
    if (diffRes.diffError.has_value())
    {
        diffRes.wrapDiffPath(DiffPathNode::constructWithTableProperty("__metatable"));
        return diffRes;
    }
    return DifferResult{};
}

static DifferResult diffPrimitive(DifferEnvironment& env, TypeId left, TypeId right)
{
    const PrimitiveType* leftPrimitive = get<PrimitiveType>(left);
    const PrimitiveType* rightPrimitive = get<PrimitiveType>(right);
    LUAU_ASSERT(leftPrimitive);
    LUAU_ASSERT(rightPrimitive);

    if (leftPrimitive->type != rightPrimitive->type)
    {
        return DifferResult{DiffError{
            DiffError::Kind::Normal,
            DiffPathNodeLeaf::detailsNormal(left),
            DiffPathNodeLeaf::detailsNormal(right),
            env.getDevFixFriendlyNameLeft(),
            env.getDevFixFriendlyNameRight(),
        }};
    }
    return DifferResult{};
}

static DifferResult diffSingleton(DifferEnvironment& env, TypeId left, TypeId right)
{
    const SingletonType* leftSingleton = get<SingletonType>(left);
    const SingletonType* rightSingleton = get<SingletonType>(right);
    LUAU_ASSERT(leftSingleton);
    LUAU_ASSERT(rightSingleton);

    if (*leftSingleton != *rightSingleton)
    {
        return DifferResult{DiffError{
            DiffError::Kind::Normal,
            DiffPathNodeLeaf::detailsNormal(left),
            DiffPathNodeLeaf::detailsNormal(right),
            env.getDevFixFriendlyNameLeft(),
            env.getDevFixFriendlyNameRight(),
        }};
    }
    return DifferResult{};
}

static DifferResult diffFunction(DifferEnvironment& env, TypeId left, TypeId right)
{
    const FunctionType* leftFunction = get<FunctionType>(left);
    const FunctionType* rightFunction = get<FunctionType>(right);
    LUAU_ASSERT(leftFunction);
    LUAU_ASSERT(rightFunction);

    DifferResult differResult = diffTpi(env, DiffError::Kind::LengthMismatchInFnArgs, leftFunction->argTypes, rightFunction->argTypes);
    if (differResult.diffError.has_value())
        return differResult;
    return diffTpi(env, DiffError::Kind::LengthMismatchInFnRets, leftFunction->retTypes, rightFunction->retTypes);
}

static DifferResult diffGeneric(DifferEnvironment& env, TypeId left, TypeId right)
{
    LUAU_ASSERT(get<GenericType>(left));
    LUAU_ASSERT(get<GenericType>(right));
    // Try to pair up the generics
    bool isLeftFree = !env.genericMatchedPairs.contains(left);
    bool isRightFree = !env.genericMatchedPairs.contains(right);
    if (isLeftFree && isRightFree)
    {
        env.genericMatchedPairs[left] = right;
        env.genericMatchedPairs[right] = left;
        return DifferResult{};
    }
    else if (isLeftFree || isRightFree)
    {
        return DifferResult{DiffError{
            DiffError::Kind::IncompatibleGeneric,
            DiffPathNodeLeaf::nullopts(),
            DiffPathNodeLeaf::nullopts(),
            env.getDevFixFriendlyNameLeft(),
            env.getDevFixFriendlyNameRight(),
        }};
    }

    // Both generics are already paired up
    if (*env.genericMatchedPairs.find(left) == right)
        return DifferResult{};

    return DifferResult{DiffError{
        DiffError::Kind::IncompatibleGeneric,
        DiffPathNodeLeaf::nullopts(),
        DiffPathNodeLeaf::nullopts(),
        env.getDevFixFriendlyNameLeft(),
        env.getDevFixFriendlyNameRight(),
    }};
}

static DifferResult diffNegation(DifferEnvironment& env, TypeId left, TypeId right)
{
    const NegationType* leftNegation = get<NegationType>(left);
    const NegationType* rightNegation = get<NegationType>(right);
    LUAU_ASSERT(leftNegation);
    LUAU_ASSERT(rightNegation);

    DifferResult differResult = diffUsingEnv(env, leftNegation->ty, rightNegation->ty);
    if (!differResult.diffError.has_value())
        return DifferResult{};

    differResult.wrapDiffPath(DiffPathNode::constructWithKind(DiffPathNode::Kind::Negation));
    return differResult;
}

static DifferResult diffClass(DifferEnvironment& env, TypeId left, TypeId right)
{
    const ClassType* leftClass = get<ClassType>(left);
    const ClassType* rightClass = get<ClassType>(right);
    LUAU_ASSERT(leftClass);
    LUAU_ASSERT(rightClass);

    if (leftClass == rightClass)
    {
        return DifferResult{};
    }

    return DifferResult{DiffError{
        DiffError::Kind::Normal,
        DiffPathNodeLeaf::detailsNormal(left),
        DiffPathNodeLeaf::detailsNormal(right),
        env.getDevFixFriendlyNameLeft(),
        env.getDevFixFriendlyNameRight(),
    }};
}

static FindSeteqCounterexampleResult findSeteqCounterexample(
    DifferEnvironment& env,
    const std::vector<TypeId>& left,
    const std::vector<TypeId>& right
)
{
    std::unordered_set<size_t> unmatchedRightIdxes;
    for (size_t i = 0; i < right.size(); i++)
        unmatchedRightIdxes.insert(i);
    for (size_t leftIdx = 0; leftIdx < left.size(); leftIdx++)
    {
        bool leftIdxIsMatched = false;
        auto unmatchedRightIdxIt = unmatchedRightIdxes.begin();
        while (unmatchedRightIdxIt != unmatchedRightIdxes.end())
        {
            DifferResult differResult = diffUsingEnv(env, left[leftIdx], right[*unmatchedRightIdxIt]);
            if (differResult.diffError.has_value())
            {
                unmatchedRightIdxIt++;
                continue;
            }
            // unmatchedRightIdxIt is matched with current leftIdx
            env.recordProvenEqual(left[leftIdx], right[*unmatchedRightIdxIt]);
            leftIdxIsMatched = true;
            unmatchedRightIdxIt = unmatchedRightIdxes.erase(unmatchedRightIdxIt);
        }
        if (!leftIdxIsMatched)
        {
            return FindSeteqCounterexampleResult{leftIdx, true};
        }
    }
    if (unmatchedRightIdxes.empty())
        return FindSeteqCounterexampleResult{std::nullopt, false};
    return FindSeteqCounterexampleResult{*unmatchedRightIdxes.begin(), false};
}

static DifferResult diffUnion(DifferEnvironment& env, TypeId left, TypeId right)
{
    const UnionType* leftUnion = get<UnionType>(left);
    const UnionType* rightUnion = get<UnionType>(right);
    LUAU_ASSERT(leftUnion);
    LUAU_ASSERT(rightUnion);

    FindSeteqCounterexampleResult findSeteqCexResult = findSeteqCounterexample(env, leftUnion->options, rightUnion->options);
    if (findSeteqCexResult.mismatchIdx.has_value())
    {
        if (findSeteqCexResult.inLeft)
            return DifferResult{DiffError{
                DiffError::Kind::MissingUnionMember,
                DiffPathNodeLeaf::detailsUnionIndex(leftUnion->options[*findSeteqCexResult.mismatchIdx], *findSeteqCexResult.mismatchIdx),
                DiffPathNodeLeaf::nullopts(),
                env.getDevFixFriendlyNameLeft(),
                env.getDevFixFriendlyNameRight(),
            }};
        else
            return DifferResult{DiffError{
                DiffError::Kind::MissingUnionMember,
                DiffPathNodeLeaf::nullopts(),
                DiffPathNodeLeaf::detailsUnionIndex(rightUnion->options[*findSeteqCexResult.mismatchIdx], *findSeteqCexResult.mismatchIdx),
                env.getDevFixFriendlyNameLeft(),
                env.getDevFixFriendlyNameRight(),
            }};
    }

    // TODO: somehow detect mismatch index, likely using heuristics

    return DifferResult{};
}

static DifferResult diffIntersection(DifferEnvironment& env, TypeId left, TypeId right)
{
    const IntersectionType* leftIntersection = get<IntersectionType>(left);
    const IntersectionType* rightIntersection = get<IntersectionType>(right);
    LUAU_ASSERT(leftIntersection);
    LUAU_ASSERT(rightIntersection);

    FindSeteqCounterexampleResult findSeteqCexResult = findSeteqCounterexample(env, leftIntersection->parts, rightIntersection->parts);
    if (findSeteqCexResult.mismatchIdx.has_value())
    {
        if (findSeteqCexResult.inLeft)
            return DifferResult{DiffError{
                DiffError::Kind::MissingIntersectionMember,
                DiffPathNodeLeaf::detailsUnionIndex(leftIntersection->parts[*findSeteqCexResult.mismatchIdx], *findSeteqCexResult.mismatchIdx),
                DiffPathNodeLeaf::nullopts(),
                env.getDevFixFriendlyNameLeft(),
                env.getDevFixFriendlyNameRight(),
            }};
        else
            return DifferResult{DiffError{
                DiffError::Kind::MissingIntersectionMember,
                DiffPathNodeLeaf::nullopts(),
                DiffPathNodeLeaf::detailsUnionIndex(rightIntersection->parts[*findSeteqCexResult.mismatchIdx], *findSeteqCexResult.mismatchIdx),
                env.getDevFixFriendlyNameLeft(),
                env.getDevFixFriendlyNameRight(),
            }};
    }

    // TODO: somehow detect mismatch index, likely using heuristics

    return DifferResult{};
}

static DifferResult diffUsingEnv(DifferEnvironment& env, TypeId left, TypeId right)
{
    left = follow(left);
    right = follow(right);

    if (left->ty.index() != right->ty.index())
    {
        return DifferResult{DiffError{
            DiffError::Kind::Normal,
            DiffPathNodeLeaf::detailsNormal(left),
            DiffPathNodeLeaf::detailsNormal(right),
            env.getDevFixFriendlyNameLeft(),
            env.getDevFixFriendlyNameRight(),
        }};
    }

    // Both left and right are the same variant

    // Check cycles & caches
    if (env.isAssumedEqual(left, right) || env.isProvenEqual(left, right))
        return DifferResult{};

    if (isSimple(left))
    {
        if (auto lp = get<PrimitiveType>(left))
            return diffPrimitive(env, left, right);
        else if (auto ls = get<SingletonType>(left))
        {
            return diffSingleton(env, left, right);
        }
        else if (auto la = get<AnyType>(left))
        {
            // Both left and right must be Any if either is Any for them to be equal!
            return DifferResult{};
        }
        else if (auto lu = get<UnknownType>(left))
        {
            return DifferResult{};
        }
        else if (auto ln = get<NeverType>(left))
        {
            return DifferResult{};
        }
        else if (auto ln = get<NegationType>(left))
        {
            return diffNegation(env, left, right);
        }
        else if (auto lc = get<ClassType>(left))
        {
            return diffClass(env, left, right);
        }

        throw InternalCompilerError{"Unimplemented Simple TypeId variant for diffing"};
    }

    // Both left and right are the same non-Simple
    // Non-simple types must record visits in the DifferEnvironment
    env.pushVisiting(left, right);

    if (auto lt = get<TableType>(left))
    {
        DifferResult diffRes = diffTable(env, left, right);
        if (!diffRes.diffError.has_value())
        {
            env.recordProvenEqual(left, right);
        }
        env.popVisiting();
        return diffRes;
    }
    if (auto lm = get<MetatableType>(left))
    {
        env.popVisiting();
        return diffMetatable(env, left, right);
    }
    if (auto lf = get<FunctionType>(left))
    {
        DifferResult diffRes = diffFunction(env, left, right);
        if (!diffRes.diffError.has_value())
        {
            env.recordProvenEqual(left, right);
        }
        env.popVisiting();
        return diffRes;
    }
    if (auto lg = get<GenericType>(left))
    {
        DifferResult diffRes = diffGeneric(env, left, right);
        if (!diffRes.diffError.has_value())
        {
            env.recordProvenEqual(left, right);
        }
        env.popVisiting();
        return diffRes;
    }
    if (auto lu = get<UnionType>(left))
    {
        DifferResult diffRes = diffUnion(env, left, right);
        if (!diffRes.diffError.has_value())
        {
            env.recordProvenEqual(left, right);
        }
        env.popVisiting();
        return diffRes;
    }
    if (auto li = get<IntersectionType>(left))
    {
        DifferResult diffRes = diffIntersection(env, left, right);
        if (!diffRes.diffError.has_value())
        {
            env.recordProvenEqual(left, right);
        }
        env.popVisiting();
        return diffRes;
    }
    if (auto le = get<ErrorType>(left))
    {
        // TODO: return debug-friendly result state
        env.popVisiting();
        return DifferResult{};
    }

    throw InternalCompilerError{"Unimplemented non-simple TypeId variant for diffing"};
}

static DifferResult diffTpi(DifferEnvironment& env, DiffError::Kind possibleNonNormalErrorKind, TypePackId left, TypePackId right)
{
    left = follow(left);
    right = follow(right);

    // Canonicalize
    std::pair<std::vector<TypeId>, std::optional<TypePackId>> leftFlatTpi = flatten(left);
    std::pair<std::vector<TypeId>, std::optional<TypePackId>> rightFlatTpi = flatten(right);

    // Check for shape equality
    DifferResult diffResult = diffCanonicalTpShape(env, possibleNonNormalErrorKind, leftFlatTpi, rightFlatTpi);
    if (diffResult.diffError.has_value())
    {
        return diffResult;
    }

    // Left and Right have the same shape
    for (size_t i = 0; i < leftFlatTpi.first.size(); i++)
    {
        DifferResult differResult = diffUsingEnv(env, leftFlatTpi.first[i], rightFlatTpi.first[i]);
        if (!differResult.diffError.has_value())
            continue;

        switch (possibleNonNormalErrorKind)
        {
        case DiffError::Kind::LengthMismatchInFnArgs:
        {
            differResult.wrapDiffPath(DiffPathNode::constructWithKindAndIndex(DiffPathNode::Kind::FunctionArgument, i));
            return differResult;
        }
        case DiffError::Kind::LengthMismatchInFnRets:
        {
            differResult.wrapDiffPath(DiffPathNode::constructWithKindAndIndex(DiffPathNode::Kind::FunctionReturn, i));
            return differResult;
        }
        default:
        {
            throw InternalCompilerError{"Unhandled Tpi diffing case with same shape"};
        }
        }
    }
    if (!leftFlatTpi.second.has_value())
        return DifferResult{};

    return diffHandleFlattenedTail(env, possibleNonNormalErrorKind, *leftFlatTpi.second, *rightFlatTpi.second);
}

static DifferResult diffCanonicalTpShape(
    DifferEnvironment& env,
    DiffError::Kind possibleNonNormalErrorKind,
    const std::pair<std::vector<TypeId>, std::optional<TypePackId>>& left,
    const std::pair<std::vector<TypeId>, std::optional<TypePackId>>& right
)
{
    if (left.first.size() == right.first.size() && left.second.has_value() == right.second.has_value())
        return DifferResult{};

    return DifferResult{DiffError{
        possibleNonNormalErrorKind,
        DiffPathNodeLeaf::detailsLength(int(left.first.size()), left.second.has_value()),
        DiffPathNodeLeaf::detailsLength(int(right.first.size()), right.second.has_value()),
        env.getDevFixFriendlyNameLeft(),
        env.getDevFixFriendlyNameRight(),
    }};
}

static DifferResult diffHandleFlattenedTail(DifferEnvironment& env, DiffError::Kind possibleNonNormalErrorKind, TypePackId left, TypePackId right)
{
    left = follow(left);
    right = follow(right);

    if (left->ty.index() != right->ty.index())
    {
        return DifferResult{DiffError{
            DiffError::Kind::Normal,
            DiffPathNodeLeaf::detailsNormal(env.visitingBegin()->first),
            DiffPathNodeLeaf::detailsNormal(env.visitingBegin()->second),
            env.getDevFixFriendlyNameLeft(),
            env.getDevFixFriendlyNameRight(),
        }};
    }

    // Both left and right are the same variant

    if (auto lv = get<VariadicTypePack>(left))
    {
        auto rv = get<VariadicTypePack>(right);
        DifferResult differResult = diffUsingEnv(env, lv->ty, rv->ty);
        if (!differResult.diffError.has_value())
            return DifferResult{};

        switch (possibleNonNormalErrorKind)
        {
        case DiffError::Kind::LengthMismatchInFnArgs:
        {
            differResult.wrapDiffPath(DiffPathNode::constructWithKind(DiffPathNode::Kind::FunctionArgument));
            return differResult;
        }
        case DiffError::Kind::LengthMismatchInFnRets:
        {
            differResult.wrapDiffPath(DiffPathNode::constructWithKind(DiffPathNode::Kind::FunctionReturn));
            return differResult;
        }
        default:
        {
            throw InternalCompilerError{"Unhandled flattened tail case for VariadicTypePack"};
        }
        }
    }
    if (auto lg = get<GenericTypePack>(left))
    {
        DifferResult diffRes = diffGenericTp(env, left, right);
        if (!diffRes.diffError.has_value())
            return DifferResult{};
        switch (possibleNonNormalErrorKind)
        {
        case DiffError::Kind::LengthMismatchInFnArgs:
        {
            diffRes.wrapDiffPath(DiffPathNode::constructWithKind(DiffPathNode::Kind::FunctionArgument));
            return diffRes;
        }
        case DiffError::Kind::LengthMismatchInFnRets:
        {
            diffRes.wrapDiffPath(DiffPathNode::constructWithKind(DiffPathNode::Kind::FunctionReturn));
            return diffRes;
        }
        default:
        {
            throw InternalCompilerError{"Unhandled flattened tail case for GenericTypePack"};
        }
        }
    }

    throw InternalCompilerError{"Unhandled tail type pack variant for flattened tails"};
}

static DifferResult diffGenericTp(DifferEnvironment& env, TypePackId left, TypePackId right)
{
    LUAU_ASSERT(get<GenericTypePack>(left));
    LUAU_ASSERT(get<GenericTypePack>(right));
    // Try to pair up the generics
    bool isLeftFree = !env.genericTpMatchedPairs.contains(left);
    bool isRightFree = !env.genericTpMatchedPairs.contains(right);
    if (isLeftFree && isRightFree)
    {
        env.genericTpMatchedPairs[left] = right;
        env.genericTpMatchedPairs[right] = left;
        return DifferResult{};
    }
    else if (isLeftFree || isRightFree)
    {
        return DifferResult{DiffError{
            DiffError::Kind::IncompatibleGeneric,
            DiffPathNodeLeaf::nullopts(),
            DiffPathNodeLeaf::nullopts(),
            env.getDevFixFriendlyNameLeft(),
            env.getDevFixFriendlyNameRight(),
        }};
    }

    // Both generics are already paired up
    if (*env.genericTpMatchedPairs.find(left) == right)
        return DifferResult{};

    return DifferResult{DiffError{
        DiffError::Kind::IncompatibleGeneric,
        DiffPathNodeLeaf::nullopts(),
        DiffPathNodeLeaf::nullopts(),
        env.getDevFixFriendlyNameLeft(),
        env.getDevFixFriendlyNameRight(),
    }};
}

bool DifferEnvironment::isProvenEqual(TypeId left, TypeId right) const
{
    return provenEqual.find({left, right}) != provenEqual.end();
}

bool DifferEnvironment::isAssumedEqual(TypeId left, TypeId right) const
{
    return visiting.find({left, right}) != visiting.end();
}

void DifferEnvironment::recordProvenEqual(TypeId left, TypeId right)
{
    provenEqual.insert({left, right});
    provenEqual.insert({right, left});
}

void DifferEnvironment::pushVisiting(TypeId left, TypeId right)
{
    LUAU_ASSERT(visiting.find({left, right}) == visiting.end());
    LUAU_ASSERT(visiting.find({right, left}) == visiting.end());
    visitingStack.push_back({left, right});
    visiting.insert({left, right});
    visiting.insert({right, left});
}

void DifferEnvironment::popVisiting()
{
    auto tyPair = visitingStack.back();
    visiting.erase({tyPair.first, tyPair.second});
    visiting.erase({tyPair.second, tyPair.first});
    visitingStack.pop_back();
}

std::vector<std::pair<TypeId, TypeId>>::const_reverse_iterator DifferEnvironment::visitingBegin() const
{
    return visitingStack.crbegin();
}

std::vector<std::pair<TypeId, TypeId>>::const_reverse_iterator DifferEnvironment::visitingEnd() const
{
    return visitingStack.crend();
}


DifferResult diff(TypeId ty1, TypeId ty2)
{
    DifferEnvironment differEnv{ty1, ty2, std::nullopt, std::nullopt};
    return diffUsingEnv(differEnv, ty1, ty2);
}


DifferResult diffWithSymbols(TypeId ty1, TypeId ty2, std::optional<std::string> symbol1, std::optional<std::string> symbol2)
{
    DifferEnvironment differEnv{ty1, ty2, symbol1, symbol2};
    return diffUsingEnv(differEnv, ty1, ty2);
}

bool isSimple(TypeId ty)
{
    ty = follow(ty);
    // TODO: think about GenericType, etc.
    return get<PrimitiveType>(ty) || get<SingletonType>(ty) || get<AnyType>(ty) || get<NegationType>(ty) || get<ClassType>(ty) ||
           get<UnknownType>(ty) || get<NeverType>(ty);
}

} // namespace Luau
