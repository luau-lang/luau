// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Type.h"
#include <optional>
#include <string>

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
    };
    Kind kind;
    // non-null when TableProperty
    std::optional<Name> tableProperty;
    // non-null when FunctionArgument, FunctionReturn, Union, or Intersection (i.e. anonymous fields)
    std::optional<int> index;

    /**
     * Do not use for leaf nodes
     */
    DiffPathNode(Kind kind)
        : kind(kind)
    {
    }

    DiffPathNode(Kind kind, std::optional<Name> tableProperty, std::optional<int> index)
        : kind(kind)
        , tableProperty(tableProperty)
        , index(index)
    {
    }

    std::string toString() const;

    static DiffPathNode constructWithTableProperty(Name tableProperty);
};
struct DiffPathNodeLeaf
{
    std::optional<TypeId> ty;
    std::optional<Name> tableProperty;
    DiffPathNodeLeaf(std::optional<TypeId> ty, std::optional<Name> tableProperty)
        : ty(ty)
        , tableProperty(tableProperty)
    {
    }

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
        MissingProperty,
        LengthMismatchInFnArgs,
        LengthMismatchInFnRets,
        LengthMismatchInUnion,
        LengthMismatchInIntersection,
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

    std::string toString() const;

private:
    std::string toStringALeaf(std::string rootName, const DiffPathNodeLeaf& leaf, const DiffPathNodeLeaf& otherLeaf) const;
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
};
DifferResult diff(TypeId ty1, TypeId ty2);

/**
 * True if ty is a "simple" type, i.e. cannot contain types.
 * string, number, boolean are simple types.
 * function and table are not simple types.
 */
bool isSimple(TypeId ty);

} // namespace Luau
