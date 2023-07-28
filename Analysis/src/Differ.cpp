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
std::string DiffError::toStringALeaf(std::string rootName, const DiffPathNodeLeaf& leaf, const DiffPathNodeLeaf& otherLeaf) const
{
    std::string pathStr{rootName + diffPath.toString(true)};
    switch (kind)
    {
    case DiffError::Kind::Normal:
    {
        checkNonMissingPropertyLeavesHaveNulloptTableProperty();
        return pathStr + " has type " + Luau::toString(*leaf.ty);
    }
    case DiffError::Kind::MissingTableProperty:
    {
        if (leaf.ty.has_value())
        {
            if (!leaf.tableProperty.has_value())
                throw InternalCompilerError{"leaf.tableProperty is nullopt"};
            return pathStr + "." + *leaf.tableProperty + " has type " + Luau::toString(*leaf.ty);
        }
        else if (otherLeaf.ty.has_value())
        {
            if (!otherLeaf.tableProperty.has_value())
                throw InternalCompilerError{"otherLeaf.tableProperty is nullopt"};
            return pathStr + " is missing the property " + *otherLeaf.tableProperty;
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
            return pathStr + " is a union containing type " + Luau::toString(*leaf.ty);
        }
        else if (otherLeaf.ty.has_value())
        {
            return pathStr + " is a union missing type " + Luau::toString(*otherLeaf.ty);
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
            return pathStr + " is an intersection containing type " + Luau::toString(*leaf.ty);
        }
        else if (otherLeaf.ty.has_value())
        {
            return pathStr + " is an intersection missing type " + Luau::toString(*otherLeaf.ty);
        }
        throw InternalCompilerError{"Both leaf.ty and otherLeaf.ty is nullopt"};
    }
    case DiffError::Kind::LengthMismatchInFnArgs:
    {
        if (!leaf.minLength.has_value())
            throw InternalCompilerError{"leaf.minLength is nullopt"};
        return pathStr + " takes " + std::to_string(*leaf.minLength) + (leaf.isVariadic ? " or more" : "") + " arguments";
    }
    case DiffError::Kind::LengthMismatchInFnRets:
    {
        if (!leaf.minLength.has_value())
            throw InternalCompilerError{"leaf.minLength is nullopt"};
        return pathStr + " returns " + std::to_string(*leaf.minLength) + (leaf.isVariadic ? " or more" : "") + " values";
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

std::string getDevFixFriendlyName(TypeId ty)
{
    if (auto table = get<TableType>(ty))
    {
        if (table->name.has_value())
            return *table->name;
        else if (table->syntheticName.has_value())
            return *table->syntheticName;
    }
    // else if (auto primitive = get<PrimitiveType>(ty))
    //{
    //    return "<unlabeled-symbol>";
    //}
    return "<unlabeled-symbol>";
}

std::string DiffError::toString() const
{
    switch (kind)
    {
    case DiffError::Kind::IncompatibleGeneric:
    {
        std::string diffPathStr{diffPath.toString(true)};
        return "DiffError: these two types are not equal because the left generic at " + leftRootName + diffPathStr +
               " cannot be the same type parameter as the right generic at " + rightRootName + diffPathStr;
    }
    default:
    {
        return "DiffError: these two types are not equal because the left type at " + toStringALeaf(leftRootName, left, right) +
               ", while the right type at " + toStringALeaf(rightRootName, right, left);
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
static DifferResult diffPrimitive(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffSingleton(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffFunction(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffGeneric(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffNegation(DifferEnvironment& env, TypeId left, TypeId right);
struct FindSeteqCounterexampleResult
{
    // nullopt if no counterexample found
    std::optional<size_t> mismatchIdx;
    // true if counterexample is in the left, false if cex is in the right
    bool inLeft;
};
static FindSeteqCounterexampleResult findSeteqCounterexample(
    DifferEnvironment& env, const std::vector<TypeId>& left, const std::vector<TypeId>& right);
static DifferResult diffUnion(DifferEnvironment& env, TypeId left, TypeId right);
static DifferResult diffIntersection(DifferEnvironment& env, TypeId left, TypeId right);
/**
 * The last argument gives context info on which complex type contained the TypePack.
 */
static DifferResult diffTpi(DifferEnvironment& env, DiffError::Kind possibleNonNormalErrorKind, TypePackId left, TypePackId right);
static DifferResult diffCanonicalTpShape(DifferEnvironment& env, DiffError::Kind possibleNonNormalErrorKind,
    const std::pair<std::vector<TypeId>, std::optional<TypePackId>>& left, const std::pair<std::vector<TypeId>, std::optional<TypePackId>>& right);
static DifferResult diffHandleFlattenedTail(DifferEnvironment& env, DiffError::Kind possibleNonNormalErrorKind, TypePackId left, TypePackId right);

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
                getDevFixFriendlyName(env.rootLeft),
                getDevFixFriendlyName(env.rootRight),
            }};
        }
    }
    for (auto const& [field, value] : rightTable->props)
    {
        if (leftTable->props.find(field) == leftTable->props.end())
        {
            // right has a field the left doesn't
            return DifferResult{DiffError{DiffError::Kind::MissingTableProperty, DiffPathNodeLeaf::nullopts(),
                DiffPathNodeLeaf::detailsTableProperty(value.type(), field), getDevFixFriendlyName(env.rootLeft),
                getDevFixFriendlyName(env.rootRight)}};
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
            getDevFixFriendlyName(env.rootLeft),
            getDevFixFriendlyName(env.rootRight),
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
            getDevFixFriendlyName(env.rootLeft),
            getDevFixFriendlyName(env.rootRight),
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
            getDevFixFriendlyName(env.rootLeft),
            getDevFixFriendlyName(env.rootRight),
        }};
    }

    // Both generics are already paired up
    if (*env.genericMatchedPairs.find(left) == right)
        return DifferResult{};

    return DifferResult{DiffError{
        DiffError::Kind::IncompatibleGeneric,
        DiffPathNodeLeaf::nullopts(),
        DiffPathNodeLeaf::nullopts(),
        getDevFixFriendlyName(env.rootLeft),
        getDevFixFriendlyName(env.rootRight),
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

static FindSeteqCounterexampleResult findSeteqCounterexample(
    DifferEnvironment& env, const std::vector<TypeId>& left, const std::vector<TypeId>& right)
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
                getDevFixFriendlyName(env.rootLeft),
                getDevFixFriendlyName(env.rootRight),
            }};
        else
            return DifferResult{DiffError{
                DiffError::Kind::MissingUnionMember,
                DiffPathNodeLeaf::nullopts(),
                DiffPathNodeLeaf::detailsUnionIndex(rightUnion->options[*findSeteqCexResult.mismatchIdx], *findSeteqCexResult.mismatchIdx),
                getDevFixFriendlyName(env.rootLeft),
                getDevFixFriendlyName(env.rootRight),
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
                getDevFixFriendlyName(env.rootLeft),
                getDevFixFriendlyName(env.rootRight),
            }};
        else
            return DifferResult{DiffError{
                DiffError::Kind::MissingIntersectionMember,
                DiffPathNodeLeaf::nullopts(),
                DiffPathNodeLeaf::detailsUnionIndex(rightIntersection->parts[*findSeteqCexResult.mismatchIdx], *findSeteqCexResult.mismatchIdx),
                getDevFixFriendlyName(env.rootLeft),
                getDevFixFriendlyName(env.rootRight),
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
            getDevFixFriendlyName(env.rootLeft),
            getDevFixFriendlyName(env.rootRight),
        }};
    }

    // Both left and right are the same variant

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
        else if (auto ln = get<NegationType>(left))
        {
            return diffNegation(env, left, right);
        }

        throw InternalCompilerError{"Unimplemented Simple TypeId variant for diffing"};
    }

    // Both left and right are the same non-Simple

    if (auto lt = get<TableType>(left))
    {
        return diffTable(env, left, right);
    }
    if (auto lf = get<FunctionType>(left))
    {
        return diffFunction(env, left, right);
    }
    if (auto lg = get<GenericType>(left))
    {
        return diffGeneric(env, left, right);
    }
    if (auto lu = get<UnionType>(left))
    {
        return diffUnion(env, left, right);
    }
    if (auto li = get<IntersectionType>(left))
    {
        return diffIntersection(env, left, right);
    }
    if (auto le = get<Luau::Unifiable::Error>(left))
    {
        // TODO: return debug-friendly result state
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

static DifferResult diffCanonicalTpShape(DifferEnvironment& env, DiffError::Kind possibleNonNormalErrorKind,
    const std::pair<std::vector<TypeId>, std::optional<TypePackId>>& left, const std::pair<std::vector<TypeId>, std::optional<TypePackId>>& right)
{
    if (left.first.size() == right.first.size() && left.second.has_value() == right.second.has_value())
        return DifferResult{};

    return DifferResult{DiffError{
        possibleNonNormalErrorKind,
        DiffPathNodeLeaf::detailsLength(int(left.first.size()), left.second.has_value()),
        DiffPathNodeLeaf::detailsLength(int(right.first.size()), right.second.has_value()),
        getDevFixFriendlyName(env.rootLeft),
        getDevFixFriendlyName(env.rootRight),
    }};
}

static DifferResult diffHandleFlattenedTail(DifferEnvironment& env, DiffError::Kind possibleNonNormalErrorKind, TypePackId left, TypePackId right)
{
    left = follow(left);
    right = follow(right);

    if (left->ty.index() != right->ty.index())
    {
        throw InternalCompilerError{"Unhandled case where the tail of 2 normalized typepacks have different variants"};
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

    throw InternalCompilerError{"Unhandled tail type pack variant for flattened tails"};
}

DifferResult diff(TypeId ty1, TypeId ty2)
{
    DifferEnvironment differEnv{ty1, ty2, DenseHashMap<TypeId, TypeId>{nullptr}};
    return diffUsingEnv(differEnv, ty1, ty2);
}

bool isSimple(TypeId ty)
{
    ty = follow(ty);
    // TODO: think about GenericType, etc.
    return get<PrimitiveType>(ty) || get<SingletonType>(ty) || get<AnyType>(ty) || get<NegationType>(ty);
}

} // namespace Luau
