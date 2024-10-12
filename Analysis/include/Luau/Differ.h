// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash.h"
#include "Luau/TypeFwd.h"
#include "Luau/UnifierSharedState.h"

#include <optional>
#include <string>
#include <unordered_set>

namespace Luau
{
struct DiffPathNode
{
    // TODO: consider using Variants to simplify toString implementation
    enum Kind
    {
        TableProperty,
        FunctionArgument,
        FunctionReturn,
        Union,
        Intersection,
        Negation,
    };
    Kind kind;
    // non-null when TableProperty
    std::optional<Name> tableProperty;
    // non-null when FunctionArgument (unless variadic arg), FunctionReturn (unless variadic arg), Union, or Intersection (i.e. anonymous fields)
    std::optional<size_t> index;

    /**
     * Do not use for leaf nodes
     */
    DiffPathNode(Kind kind)
        : kind(kind)
    {
    }

    DiffPathNode(Kind kind, std::optional<Name> tableProperty, std::optional<size_t> index)
        : kind(kind)
        , tableProperty(tableProperty)
        , index(index)
    {
    }

    std::string toString() const;

    static DiffPathNode constructWithTableProperty(Name tableProperty);

    static DiffPathNode constructWithKindAndIndex(Kind kind, size_t index);

    static DiffPathNode constructWithKind(Kind kind);
};

struct DiffPathNodeLeaf
{
    std::optional<TypeId> ty;
    std::optional<Name> tableProperty;
    std::optional<int> minLength;
    bool isVariadic;
    // TODO: Rename to anonymousIndex, for both union and Intersection
    std::optional<size_t> unionIndex;
    DiffPathNodeLeaf(
        std::optional<TypeId> ty,
        std::optional<Name> tableProperty,
        std::optional<int> minLength,
        bool isVariadic,
        std::optional<size_t> unionIndex
    )
        : ty(ty)
        , tableProperty(tableProperty)
        , minLength(minLength)
        , isVariadic(isVariadic)
        , unionIndex(unionIndex)
    {
    }

    static DiffPathNodeLeaf detailsNormal(TypeId ty);

    static DiffPathNodeLeaf detailsTableProperty(TypeId ty, Name tableProperty);

    static DiffPathNodeLeaf detailsUnionIndex(TypeId ty, size_t index);

    static DiffPathNodeLeaf detailsLength(int minLength, bool isVariadic);

    static DiffPathNodeLeaf nullopts();
};

struct DiffPath
{
    std::vector<DiffPathNode> path;

    std::string toString(bool prependDot) const;
};
struct DiffError
{
    enum Kind
    {
        Normal,
        MissingTableProperty,
        MissingUnionMember,
        MissingIntersectionMember,
        IncompatibleGeneric,
        LengthMismatchInFnArgs,
        LengthMismatchInFnRets,
    };
    Kind kind;

    DiffPath diffPath;
    DiffPathNodeLeaf left;
    DiffPathNodeLeaf right;

    std::string leftRootName;
    std::string rightRootName;

    DiffError(Kind kind, DiffPathNodeLeaf left, DiffPathNodeLeaf right, std::string leftRootName, std::string rightRootName)
        : kind(kind)
        , left(left)
        , right(right)
        , leftRootName(leftRootName)
        , rightRootName(rightRootName)
    {
        checkValidInitialization(left, right);
    }
    DiffError(Kind kind, DiffPath diffPath, DiffPathNodeLeaf left, DiffPathNodeLeaf right, std::string leftRootName, std::string rightRootName)
        : kind(kind)
        , diffPath(diffPath)
        , left(left)
        , right(right)
        , leftRootName(leftRootName)
        , rightRootName(rightRootName)
    {
        checkValidInitialization(left, right);
    }

    std::string toString(bool multiLine = false) const;

private:
    std::string toStringALeaf(std::string rootName, const DiffPathNodeLeaf& leaf, const DiffPathNodeLeaf& otherLeaf, bool multiLine) const;
    void checkValidInitialization(const DiffPathNodeLeaf& left, const DiffPathNodeLeaf& right);
    void checkNonMissingPropertyLeavesHaveNulloptTableProperty() const;
};

struct DifferResult
{
    std::optional<DiffError> diffError;

    DifferResult() {}
    DifferResult(DiffError diffError)
        : diffError(diffError)
    {
    }

    void wrapDiffPath(DiffPathNode node);
};
struct DifferEnvironment
{
    TypeId rootLeft;
    TypeId rootRight;
    std::optional<std::string> externalSymbolLeft;
    std::optional<std::string> externalSymbolRight;
    DenseHashMap<TypeId, TypeId> genericMatchedPairs;
    DenseHashMap<TypePackId, TypePackId> genericTpMatchedPairs;

    DifferEnvironment(
        TypeId rootLeft,
        TypeId rootRight,
        std::optional<std::string> externalSymbolLeft,
        std::optional<std::string> externalSymbolRight
    )
        : rootLeft(rootLeft)
        , rootRight(rootRight)
        , externalSymbolLeft(externalSymbolLeft)
        , externalSymbolRight(externalSymbolRight)
        , genericMatchedPairs(nullptr)
        , genericTpMatchedPairs(nullptr)
    {
    }

    bool isProvenEqual(TypeId left, TypeId right) const;
    bool isAssumedEqual(TypeId left, TypeId right) const;
    void recordProvenEqual(TypeId left, TypeId right);
    void pushVisiting(TypeId left, TypeId right);
    void popVisiting();
    std::vector<std::pair<TypeId, TypeId>>::const_reverse_iterator visitingBegin() const;
    std::vector<std::pair<TypeId, TypeId>>::const_reverse_iterator visitingEnd() const;
    std::string getDevFixFriendlyNameLeft() const;
    std::string getDevFixFriendlyNameRight() const;

private:
    // TODO: consider using DenseHashSet
    std::unordered_set<std::pair<TypeId, TypeId>, TypeIdPairHash> provenEqual;
    // Ancestors of current types
    std::unordered_set<std::pair<TypeId, TypeId>, TypeIdPairHash> visiting;
    std::vector<std::pair<TypeId, TypeId>> visitingStack;
};
DifferResult diff(TypeId ty1, TypeId ty2);
DifferResult diffWithSymbols(TypeId ty1, TypeId ty2, std::optional<std::string> symbol1, std::optional<std::string> symbol2);

/**
 * True if ty is a "simple" type, i.e. cannot contain types.
 * string, number, boolean are simple types.
 * function and table are not simple types.
 */
bool isSimple(TypeId ty);

} // namespace Luau
