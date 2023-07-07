// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Differ.h"
#include "Luau/Error.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include <optional>

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

DiffPathNodeLeaf DiffPathNodeLeaf::nullopts()
{
    return DiffPathNodeLeaf{std::nullopt, std::nullopt};
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
    case DiffError::Kind::MissingProperty:
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
    default:
    {
        throw InternalCompilerError{"DiffPath::toStringWithLeaf is not exhaustive"};
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
    std::string msg = "DiffError: these two types are not equal because the left type at " + toStringALeaf(leftRootName, left, right) +
                      ", while the right type at " + toStringALeaf(rightRootName, right, left);
    return msg;
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

static DifferResult diffTable(DifferEnvironment& env, TypeId left, TypeId right)
{
    const TableType* leftTable = get<TableType>(left);
    const TableType* rightTable = get<TableType>(right);

    for (auto const& [field, value] : leftTable->props)
    {
        if (rightTable->props.find(field) == rightTable->props.end())
        {
            // left has a field the right doesn't
            return DifferResult{DiffError{
                DiffError::Kind::MissingProperty,
                DiffPathNodeLeaf{value.type(), field},
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
            return DifferResult{DiffError{DiffError::Kind::MissingProperty, DiffPathNodeLeaf::nullopts(), DiffPathNodeLeaf{value.type(), field},
                getDevFixFriendlyName(env.rootLeft), getDevFixFriendlyName(env.rootRight)}};
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

    if (leftPrimitive->type != rightPrimitive->type)
    {
        return DifferResult{DiffError{
            DiffError::Kind::Normal,
            DiffPathNodeLeaf{left, std::nullopt},
            DiffPathNodeLeaf{right, std::nullopt},
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

    if (*leftSingleton != *rightSingleton)
    {
        return DifferResult{DiffError{
            DiffError::Kind::Normal,
            DiffPathNodeLeaf{left, std::nullopt},
            DiffPathNodeLeaf{right, std::nullopt},
            getDevFixFriendlyName(env.rootLeft),
            getDevFixFriendlyName(env.rootRight),
        }};
    }
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
            DiffPathNodeLeaf{left, std::nullopt},
            DiffPathNodeLeaf{right, std::nullopt},
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

        throw InternalCompilerError{"Unimplemented Simple TypeId variant for diffing"};
    }

    // Both left and right are the same non-Simple

    if (auto lt = get<TableType>(left))
    {
        return diffTable(env, left, right);
    }
    throw InternalCompilerError{"Unimplemented non-simple TypeId variant for diffing"};
}

DifferResult diff(TypeId ty1, TypeId ty2)
{
    DifferEnvironment differEnv{ty1, ty2};
    return diffUsingEnv(differEnv, ty1, ty2);
}

bool isSimple(TypeId ty)
{
    ty = follow(ty);
    // TODO: think about GenericType, etc.
    return get<PrimitiveType>(ty) || get<SingletonType>(ty);
}

} // namespace Luau
