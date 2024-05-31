// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Simplify.h"

#include "Luau/DenseHash.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Set.h"
#include "Luau/TypeArena.h"
#include "Luau/TypePairHash.h"
#include "Luau/TypeUtils.h"

#include <algorithm>

LUAU_FASTINT(LuauTypeReductionRecursionLimit)
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)
LUAU_DYNAMIC_FASTINTVARIABLE(LuauSimplificationComplexityLimit, 8);

namespace Luau
{

using SimplifierSeenSet = Set<std::pair<TypeId, TypeId>, TypePairHash>;

struct TypeSimplifier
{
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<TypeArena> arena;

    DenseHashSet<TypeId> blockedTypes{nullptr};

    int recursionDepth = 0;

    TypeId mkNegation(TypeId ty);

    TypeId intersectFromParts(std::set<TypeId> parts);

    TypeId intersectUnionWithType(TypeId unionTy, TypeId right);
    TypeId intersectUnions(TypeId left, TypeId right);
    TypeId intersectNegatedUnion(TypeId unionTy, TypeId right);

    TypeId intersectTypeWithNegation(TypeId a, TypeId b);
    TypeId intersectNegations(TypeId a, TypeId b);

    TypeId intersectIntersectionWithType(TypeId left, TypeId right);

    // Attempt to intersect the two types.  Does not recurse.  Does not handle
    // unions, intersections, or negations.
    std::optional<TypeId> basicIntersect(TypeId left, TypeId right);

    TypeId intersect(TypeId ty, TypeId discriminant);
    TypeId union_(TypeId ty, TypeId discriminant);

    TypeId simplify(TypeId ty);
    TypeId simplify(TypeId ty, DenseHashSet<TypeId>& seen);
};

// Match the exact type false|nil
static bool isFalsyType(TypeId ty)
{
    ty = follow(ty);
    const UnionType* ut = get<UnionType>(ty);
    if (!ut)
        return false;

    bool hasFalse = false;
    bool hasNil = false;

    auto it = begin(ut);
    if (it == end(ut))
        return false;

    TypeId t = follow(*it);

    if (auto pt = get<PrimitiveType>(t); pt && pt->type == PrimitiveType::NilType)
        hasNil = true;
    else if (auto st = get<SingletonType>(t); st && st->variant == BooleanSingleton{false})
        hasFalse = true;
    else
        return false;

    ++it;
    if (it == end(ut))
        return false;

    t = follow(*it);

    if (auto pt = get<PrimitiveType>(t); pt && pt->type == PrimitiveType::NilType)
        hasNil = true;
    else if (auto st = get<SingletonType>(t); st && st->variant == BooleanSingleton{false})
        hasFalse = true;
    else
        return false;

    ++it;
    if (it != end(ut))
        return false;

    return hasFalse && hasNil;
}

// Match the exact type ~(false|nil)
bool isTruthyType(TypeId ty)
{
    ty = follow(ty);

    const NegationType* nt = get<NegationType>(ty);
    if (!nt)
        return false;

    return isFalsyType(nt->ty);
}

Relation flip(Relation rel)
{
    switch (rel)
    {
    case Relation::Subset:
        return Relation::Superset;
    case Relation::Superset:
        return Relation::Subset;
    default:
        return rel;
    }
}

// FIXME: I'm not completely certain that this function is theoretically reasonable.
Relation combine(Relation a, Relation b)
{
    switch (a)
    {
    case Relation::Disjoint:
        switch (b)
        {
        case Relation::Disjoint:
            return Relation::Disjoint;
        case Relation::Coincident:
            return Relation::Superset;
        case Relation::Intersects:
            return Relation::Intersects;
        case Relation::Subset:
            return Relation::Intersects;
        case Relation::Superset:
            return Relation::Intersects;
        }
    case Relation::Coincident:
        switch (b)
        {
        case Relation::Disjoint:
            return Relation::Coincident;
        case Relation::Coincident:
            return Relation::Coincident;
        case Relation::Intersects:
            return Relation::Superset;
        case Relation::Subset:
            return Relation::Coincident;
        case Relation::Superset:
            return Relation::Intersects;
        }
    case Relation::Superset:
        switch (b)
        {
        case Relation::Disjoint:
            return Relation::Superset;
        case Relation::Coincident:
            return Relation::Superset;
        case Relation::Intersects:
            return Relation::Intersects;
        case Relation::Subset:
            return Relation::Intersects;
        case Relation::Superset:
            return Relation::Superset;
        }
    case Relation::Subset:
        switch (b)
        {
        case Relation::Disjoint:
            return Relation::Subset;
        case Relation::Coincident:
            return Relation::Coincident;
        case Relation::Intersects:
            return Relation::Intersects;
        case Relation::Subset:
            return Relation::Subset;
        case Relation::Superset:
            return Relation::Intersects;
        }
    case Relation::Intersects:
        switch (b)
        {
        case Relation::Disjoint:
            return Relation::Intersects;
        case Relation::Coincident:
            return Relation::Superset;
        case Relation::Intersects:
            return Relation::Intersects;
        case Relation::Subset:
            return Relation::Intersects;
        case Relation::Superset:
            return Relation::Intersects;
        }
    }

    LUAU_UNREACHABLE();
    return Relation::Intersects;
}

// Given A & B, what is A & ~B?
Relation invert(Relation r)
{
    switch (r)
    {
    case Relation::Disjoint:
        return Relation::Subset;
    case Relation::Coincident:
        return Relation::Disjoint;
    case Relation::Intersects:
        return Relation::Intersects;
    case Relation::Subset:
        return Relation::Disjoint;
    case Relation::Superset:
        return Relation::Intersects;
    }

    LUAU_UNREACHABLE();
    return Relation::Intersects;
}

static bool isTypeVariable(TypeId ty)
{
    return get<FreeType>(ty) || get<GenericType>(ty) || get<BlockedType>(ty) || get<PendingExpansionType>(ty);
}

Relation relate(TypeId left, TypeId right, SimplifierSeenSet& seen);

Relation relateTables(TypeId left, TypeId right, SimplifierSeenSet& seen)
{
    NotNull<const TableType> leftTable{get<TableType>(left)};
    NotNull<const TableType> rightTable{get<TableType>(right)};
    LUAU_ASSERT(1 == rightTable->props.size());
    // Disjoint props have nothing in common
    // t1 with props p1's cannot appear in t2 and t2 with props p2's cannot appear in t1
    bool foundPropFromLeftInRight = std::any_of(begin(leftTable->props), end(leftTable->props), [&](auto prop) {
        return rightTable->props.count(prop.first) > 0;
    });
    bool foundPropFromRightInLeft = std::any_of(begin(rightTable->props), end(rightTable->props), [&](auto prop) {
        return leftTable->props.count(prop.first) > 0;
    });

    if (!foundPropFromLeftInRight && !foundPropFromRightInLeft && leftTable->props.size() >= 1 && rightTable->props.size() >= 1)
        return Relation::Disjoint;

    const auto [propName, rightProp] = *begin(rightTable->props);

    auto it = leftTable->props.find(propName);
    if (it == leftTable->props.end())
    {
        // Every table lacking a property is a supertype of a table having that
        // property but the reverse is not true.
        return Relation::Superset;
    }

    const Property leftProp = it->second;

    if (!leftProp.isShared() || !rightProp.isShared())
        return Relation::Intersects;

    Relation r = relate(leftProp.type(), rightProp.type(), seen);
    if (r == Relation::Coincident && 1 != leftTable->props.size())
    {
        // eg {tag: "cat", prop: string} & {tag: "cat"}
        return Relation::Subset;
    }
    else
        return r;
}

// A cheap and approximate subtype test
Relation relate(TypeId left, TypeId right, SimplifierSeenSet& seen)
{
    // TODO nice to have: Relate functions of equal argument and return arity

    left = follow(left);
    right = follow(right);

    if (left == right)
        return Relation::Coincident;

    std::pair<TypeId, TypeId> typePair{left, right};
    if (!seen.insert(typePair))
    {
        // TODO: is this right at all?
        // The thinking here is that this is a cycle if we get here, and therefore its coincident.
        return Relation::Coincident;
    }

    if (get<UnknownType>(left))
    {
        if (get<AnyType>(right))
            return Relation::Subset;
        else if (get<UnknownType>(right))
            return Relation::Coincident;
        else if (get<ErrorType>(right))
            return Relation::Disjoint;
        else
            return Relation::Superset;
    }

    if (get<UnknownType>(right))
        return flip(relate(right, left, seen));

    if (get<AnyType>(left))
    {
        if (get<AnyType>(right))
            return Relation::Coincident;
        else
            return Relation::Superset;
    }

    if (get<AnyType>(right))
        return flip(relate(right, left, seen));

    // Type variables
    // * FreeType
    // * GenericType
    // * BlockedType
    // * PendingExpansionType

    // Tops and bottoms
    // * ErrorType
    // * AnyType
    // * NeverType
    // * UnknownType

    // Concrete
    // * PrimitiveType
    // * SingletonType
    // * FunctionType
    // * TableType
    // * MetatableType
    // * ClassType
    // * UnionType
    // * IntersectionType
    // * NegationType

    if (isTypeVariable(left) || isTypeVariable(right))
        return Relation::Intersects;

    if (get<ErrorType>(left))
    {
        if (get<ErrorType>(right))
            return Relation::Coincident;
        else if (get<AnyType>(right))
            return Relation::Subset;
        else
            return Relation::Disjoint;
    }
    if (get<ErrorType>(right))
        return flip(relate(right, left, seen));

    if (get<NeverType>(left))
    {
        if (get<NeverType>(right))
            return Relation::Coincident;
        else
            return Relation::Subset;
    }
    if (get<NeverType>(right))
        return flip(relate(right, left, seen));

    if (auto ut = get<IntersectionType>(left))
        return Relation::Intersects;
    else if (auto ut = get<IntersectionType>(right))
        return Relation::Intersects;

    if (auto ut = get<UnionType>(left))
        return Relation::Intersects;
    else if (auto ut = get<UnionType>(right))
    {
        std::vector<Relation> opts;
        for (TypeId part : ut)
        {
            Relation r = relate(left, part, seen);

            if (r == Relation::Subset || r == Relation::Coincident)
                return Relation::Subset;
        }
        return Relation::Intersects;
    }

    if (auto rnt = get<NegationType>(right))
    {
        Relation a = relate(left, rnt->ty, seen);
        switch (a)
        {
        case Relation::Coincident:
            // number & ~number
            return Relation::Disjoint;
        case Relation::Disjoint:
            if (get<NegationType>(left))
            {
                // ~number & ~string
                return Relation::Intersects;
            }
            else
            {
                // number & ~string
                return Relation::Subset;
            }
        case Relation::Intersects:
            // ~(false?) & ~boolean
            return Relation::Intersects;
        case Relation::Subset:
            // "hello" & ~string
            return Relation::Disjoint;
        case Relation::Superset:
            // ~function & ~(false?)  -> ~function
            // boolean & ~(false?)    -> true
            // string & ~"hello"      -> string & ~"hello"
            return Relation::Intersects;
        }
    }
    else if (get<NegationType>(left))
        return flip(relate(right, left, seen));

    if (auto lp = get<PrimitiveType>(left))
    {
        if (auto rp = get<PrimitiveType>(right))
        {
            if (lp->type == rp->type)
                return Relation::Coincident;
            else
                return Relation::Disjoint;
        }

        if (auto rs = get<SingletonType>(right))
        {
            if (lp->type == PrimitiveType::String && rs->variant.get_if<StringSingleton>())
                return Relation::Superset;
            else if (lp->type == PrimitiveType::Boolean && rs->variant.get_if<BooleanSingleton>())
                return Relation::Superset;
            else
                return Relation::Disjoint;
        }

        if (lp->type == PrimitiveType::Function)
        {
            if (get<FunctionType>(right))
                return Relation::Superset;
            else
                return Relation::Disjoint;
        }
        if (lp->type == PrimitiveType::Table)
        {
            if (get<TableType>(right))
                return Relation::Superset;
            else
                return Relation::Disjoint;
        }

        if (get<FunctionType>(right) || get<TableType>(right) || get<MetatableType>(right) || get<ClassType>(right))
            return Relation::Disjoint;
    }

    if (auto ls = get<SingletonType>(left))
    {
        if (get<FunctionType>(right) || get<TableType>(right) || get<MetatableType>(right) || get<ClassType>(right))
            return Relation::Disjoint;

        if (get<PrimitiveType>(right))
            return flip(relate(right, left, seen));
        if (auto rs = get<SingletonType>(right))
        {
            if (ls->variant == rs->variant)
                return Relation::Coincident;
            else
                return Relation::Disjoint;
        }
    }

    if (get<FunctionType>(left))
    {
        if (auto rp = get<PrimitiveType>(right))
        {
            if (rp->type == PrimitiveType::Function)
                return Relation::Subset;
            else
                return Relation::Disjoint;
        }
        else
            return Relation::Intersects;
    }

    if (auto lt = get<TableType>(left))
    {
        if (auto rp = get<PrimitiveType>(right))
        {
            if (rp->type == PrimitiveType::Table)
                return Relation::Subset;
            else
                return Relation::Disjoint;
        }
        else if (auto rt = get<TableType>(right))
        {
            // TODO PROBABLY indexers and metatables.
            if (1 == rt->props.size())
            {
                Relation r = relateTables(left, right, seen);
                /*
                 * A reduction of these intersections is certainly possible, but
                 * it would require minting new table types. Also, I don't think
                 * it's super likely for this to arise from a refinement.
                 *
                 * Time will tell!
                 *
                 * ex we simplify this
                 *     {tag: string} & {tag: "cat"}
                 * but not this
                 *     {tag: string, prop: number} & {tag: "cat"}
                 */
                if (lt->props.size() > 1 && r == Relation::Superset)
                    return Relation::Intersects;
                else
                    return r;
            }
            else if (1 == lt->props.size())
                return flip(relate(right, left, seen));
            else
                return Relation::Intersects;
        }
        // TODO metatables

        return Relation::Disjoint;
    }

    if (auto ct = get<ClassType>(left))
    {
        if (auto rct = get<ClassType>(right))
        {
            if (isSubclass(ct, rct))
                return Relation::Subset;
            else if (isSubclass(rct, ct))
                return Relation::Superset;
            else
                return Relation::Disjoint;
        }

        return Relation::Disjoint;
    }

    return Relation::Intersects;
}

// A cheap and approximate subtype test
Relation relate(TypeId left, TypeId right)
{
    SimplifierSeenSet seen{{}};
    return relate(left, right, seen);
}

TypeId TypeSimplifier::mkNegation(TypeId ty)
{
    TypeId result = nullptr;

    if (ty == builtinTypes->truthyType)
        result = builtinTypes->falsyType;
    else if (ty == builtinTypes->falsyType)
        result = builtinTypes->truthyType;
    else if (auto ntv = get<NegationType>(ty))
        result = follow(ntv->ty);
    else
        result = arena->addType(NegationType{ty});

    return result;
}

TypeId TypeSimplifier::intersectFromParts(std::set<TypeId> parts)
{
    if (0 == parts.size())
        return builtinTypes->neverType;
    else if (1 == parts.size())
        return *begin(parts);

    {
        auto it = begin(parts);
        while (it != end(parts))
        {
            TypeId t = follow(*it);

            auto copy = it;
            ++it;

            if (auto ut = get<IntersectionType>(t))
            {
                for (TypeId part : ut)
                    parts.insert(part);
                parts.erase(copy);
            }
        }
    }

    std::set<TypeId> newParts;

    /*
     * It is possible that the parts of the passed intersection are themselves
     * reducable.
     *
     * eg false & boolean
     *
     * We do a comparison between each pair of types and look for things that we
     * can elide.
     */
    for (TypeId part : parts)
    {
        if (newParts.empty())
        {
            newParts.insert(part);
            continue;
        }

        auto it = begin(newParts);
        while (it != end(newParts))
        {
            TypeId p = *it;

            switch (relate(part, p))
            {
            case Relation::Disjoint:
                // eg boolean & string
                return builtinTypes->neverType;
            case Relation::Subset:
            {
                /* part is a subset of p.  Remove p from the set and replace it
                 * with part.
                 *
                 * eg boolean & true
                 */
                auto saveIt = it;
                ++it;
                newParts.erase(saveIt);
                continue;
            }
            case Relation::Coincident:
            case Relation::Superset:
            {
                /* part is coincident or a superset of p.  We do not need to
                 * include part in the final intersection.
                 *
                 * ex true & boolean
                 */
                ++it;
                continue;
            }
            case Relation::Intersects:
            {
                /* It's complicated!  A simplification may still be possible,
                 * but we have to pull the types apart to figure it out.
                 *
                 * ex boolean & ~false
                 */
                std::optional<TypeId> simplified = basicIntersect(part, p);

                auto saveIt = it;
                ++it;

                if (simplified)
                {
                    newParts.erase(saveIt);
                    newParts.insert(*simplified);
                }
                else
                    newParts.insert(part);
                continue;
            }
            }
        }
    }

    if (0 == newParts.size())
        return builtinTypes->neverType;
    else if (1 == newParts.size())
        return *begin(newParts);
    else
        return arena->addType(IntersectionType{std::vector<TypeId>{begin(newParts), end(newParts)}});
}

TypeId TypeSimplifier::intersectUnionWithType(TypeId left, TypeId right)
{
    const UnionType* leftUnion = get<UnionType>(left);
    LUAU_ASSERT(leftUnion);

    bool changed = false;
    std::set<TypeId> newParts;

    if (leftUnion->options.size() > (size_t)DFInt::LuauSimplificationComplexityLimit)
        return arena->addType(IntersectionType{{left, right}});

    for (TypeId part : leftUnion)
    {
        TypeId simplified = intersect(right, part);
        changed |= simplified != part;

        if (get<NeverType>(simplified))
        {
            changed = true;
            continue;
        }

        newParts.insert(simplified);
    }

    if (!changed)
        return left;
    else if (newParts.empty())
        return builtinTypes->neverType;
    else if (newParts.size() == 1)
        return *begin(newParts);
    else
        return arena->addType(UnionType{std::vector<TypeId>(begin(newParts), end(newParts))});
}

TypeId TypeSimplifier::intersectUnions(TypeId left, TypeId right)
{
    const UnionType* leftUnion = get<UnionType>(left);
    LUAU_ASSERT(leftUnion);

    const UnionType* rightUnion = get<UnionType>(right);
    LUAU_ASSERT(rightUnion);

    std::set<TypeId> newParts;

    // Combinatorial blowup moment!!

    // combination size
    size_t optionSize = (int)leftUnion->options.size() * rightUnion->options.size();
    size_t maxSize = DFInt::LuauSimplificationComplexityLimit;

    if (optionSize > maxSize)
        return arena->addType(IntersectionType{{left, right}});

    for (TypeId leftPart : leftUnion)
    {
        for (TypeId rightPart : rightUnion)
        {
            TypeId simplified = intersect(leftPart, rightPart);
            if (get<NeverType>(simplified))
                continue;

            newParts.insert(simplified);
        }
    }

    if (newParts.empty())
        return builtinTypes->neverType;
    else if (newParts.size() == 1)
        return *begin(newParts);
    else
        return arena->addType(UnionType{std::vector<TypeId>(begin(newParts), end(newParts))});
}

TypeId TypeSimplifier::intersectNegatedUnion(TypeId left, TypeId right)
{
    // ~(A | B) & C
    // (~A & C) & (~B & C)

    const NegationType* leftNegation = get<NegationType>(left);
    LUAU_ASSERT(leftNegation);

    TypeId negatedTy = follow(leftNegation->ty);

    const UnionType* negatedUnion = get<UnionType>(negatedTy);
    LUAU_ASSERT(negatedUnion);

    bool changed = false;
    std::set<TypeId> newParts;

    for (TypeId part : negatedUnion)
    {
        Relation r = relate(part, right);
        switch (r)
        {
        case Relation::Disjoint:
            // If A is disjoint from B, then ~A & B is just B.
            //
            // ~(false?) & true
            // (~false & true) & (~nil & true)
            // true & true
            newParts.insert(right);
            break;
        case Relation::Coincident:
            // If A is coincident with or a superset of B, then ~A & B is never.
            //
            // ~(false?) & false
            // (~false & false) & (~nil & false)
            // never & false
            //
            // fallthrough
        case Relation::Superset:
            // If A is a superset of B, then ~A & B is never.
            //
            // ~(boolean | nil) & true
            // (~boolean & true) & (~boolean & nil)
            // never & nil
            return builtinTypes->neverType;
        case Relation::Subset:
        case Relation::Intersects:
            // If A is a subset of B, then ~A & B is a bit more complicated.  We need to think harder.
            //
            // ~(false?) & boolean
            // (~false & boolean) & (~nil & boolean)
            // true & boolean
            TypeId simplified = intersectTypeWithNegation(mkNegation(part), right);
            changed |= simplified != right;
            if (get<NeverType>(simplified))
                changed = true;
            else
                newParts.insert(simplified);
            break;
        }
    }

    if (!changed)
        return right;
    else
        return intersectFromParts(std::move(newParts));
}

TypeId TypeSimplifier::intersectTypeWithNegation(TypeId left, TypeId right)
{
    const NegationType* leftNegation = get<NegationType>(left);
    LUAU_ASSERT(leftNegation);

    TypeId negatedTy = follow(leftNegation->ty);

    if (negatedTy == right)
        return builtinTypes->neverType;

    if (auto ut = get<UnionType>(negatedTy))
    {
        // ~(A | B) & C
        // (~A & C) & (~B & C)

        bool changed = false;
        std::set<TypeId> newParts;

        for (TypeId part : ut)
        {
            Relation r = relate(part, right);
            switch (r)
            {
            case Relation::Coincident:
                // ~(false?) & nil
                // (~false & nil) & (~nil & nil)
                // nil & never
                //
                // fallthrough
            case Relation::Superset:
                // ~(boolean | string) & true
                // (~boolean & true) & (~boolean & string)
                // never & string

                return builtinTypes->neverType;

            case Relation::Disjoint:
                // ~nil & boolean
                newParts.insert(right);
                break;

            case Relation::Subset:
                // ~false & boolean
                // fallthrough
            case Relation::Intersects:
                // FIXME: The mkNegation here is pretty unfortunate.
                // Memoizing this will probably be important.
                changed = true;
                newParts.insert(right);
                newParts.insert(mkNegation(part));
            }
        }

        if (!changed)
            return right;
        else
            return intersectFromParts(std::move(newParts));
    }

    if (auto rightUnion = get<UnionType>(right))
    {
        // ~A & (B | C)
        bool changed = false;
        std::set<TypeId> newParts;

        for (TypeId part : rightUnion)
        {
            Relation r = relate(negatedTy, part);
            switch (r)
            {
            case Relation::Coincident:
                changed = true;
                continue;
            case Relation::Disjoint:
                newParts.insert(part);
                break;
            case Relation::Superset:
                changed = true;
                continue;
            case Relation::Subset:
                // fallthrough
            case Relation::Intersects:
                changed = true;
                newParts.insert(arena->addType(IntersectionType{{left, part}}));
            }
        }

        if (!changed)
            return right;
        else if (0 == newParts.size())
            return builtinTypes->neverType;
        else if (1 == newParts.size())
            return *begin(newParts);
        else
            return arena->addType(UnionType{std::vector<TypeId>{begin(newParts), end(newParts)}});
    }

    if (auto pt = get<PrimitiveType>(right); pt && pt->type == PrimitiveType::Boolean)
    {
        if (auto st = get<SingletonType>(negatedTy))
        {
            if (st->variant == BooleanSingleton{true})
                return builtinTypes->falseType;
            else if (st->variant == BooleanSingleton{false})
                return builtinTypes->trueType;
            else
                // boolean & ~"hello"
                return builtinTypes->booleanType;
        }
    }

    Relation r = relate(negatedTy, right);

    switch (r)
    {
    case Relation::Disjoint:
        // ~boolean & string
        return right;
    case Relation::Coincident:
        // ~string & string
        // fallthrough
    case Relation::Superset:
        // ~string & "hello"
        return builtinTypes->neverType;
    case Relation::Subset:
        // ~string & unknown
        // ~"hello" & string
        // fallthrough
    case Relation::Intersects:
        // ~("hello" | boolean) & string
        // fallthrough
    default:
        return arena->addType(IntersectionType{{left, right}});
    }
}

TypeId TypeSimplifier::intersectNegations(TypeId left, TypeId right)
{
    const NegationType* leftNegation = get<NegationType>(left);
    LUAU_ASSERT(leftNegation);

    if (get<UnionType>(follow(leftNegation->ty)))
        return intersectNegatedUnion(left, right);

    const NegationType* rightNegation = get<NegationType>(right);
    LUAU_ASSERT(rightNegation);

    if (get<UnionType>(follow(rightNegation->ty)))
        return intersectNegatedUnion(right, left);

    Relation r = relate(leftNegation->ty, rightNegation->ty);

    switch (r)
    {
    case Relation::Coincident:
        // ~true & ~true
        return left;
    case Relation::Subset:
        // ~true & ~boolean
        return right;
    case Relation::Superset:
        // ~boolean & ~true
        return left;
    case Relation::Intersects:
    case Relation::Disjoint:
    default:
        // ~boolean & ~string
        return arena->addType(IntersectionType{{left, right}});
    }
}

TypeId TypeSimplifier::intersectIntersectionWithType(TypeId left, TypeId right)
{
    const IntersectionType* leftIntersection = get<IntersectionType>(left);
    LUAU_ASSERT(leftIntersection);

    if (leftIntersection->parts.size() > (size_t)DFInt::LuauSimplificationComplexityLimit)
        return arena->addType(IntersectionType{{left, right}});

    bool changed = false;
    std::set<TypeId> newParts;

    for (TypeId part : leftIntersection)
    {
        Relation r = relate(part, right);
        switch (r)
        {
        case Relation::Disjoint:
            return builtinTypes->neverType;
        case Relation::Coincident:
            newParts.insert(part);
            continue;
        case Relation::Subset:
            newParts.insert(part);
            continue;
        case Relation::Superset:
            newParts.insert(right);
            changed = true;
            continue;
        default:
            newParts.insert(part);
            newParts.insert(right);
            changed = true;
            continue;
        }
    }

    // It is sometimes the case that an intersection operation will result in
    // clipping a free type from the result.
    //
    // eg (number & 'a) & string --> never
    //
    // We want to only report the free types that are part of the result.
    for (TypeId part : newParts)
    {
        if (isTypeVariable(part))
            blockedTypes.insert(part);
    }

    if (!changed)
        return left;
    return intersectFromParts(std::move(newParts));
}

std::optional<TypeId> TypeSimplifier::basicIntersect(TypeId left, TypeId right)
{
    if (get<AnyType>(left) && get<ErrorType>(right))
        return right;
    if (get<AnyType>(right) && get<ErrorType>(left))
        return left;
    if (get<AnyType>(left))
        return arena->addType(UnionType{{right, builtinTypes->errorType}});
    if (get<AnyType>(right))
        return arena->addType(UnionType{{left, builtinTypes->errorType}});
    if (get<UnknownType>(left))
        return right;
    if (get<UnknownType>(right))
        return left;
    if (get<NeverType>(left))
        return left;
    if (get<NeverType>(right))
        return right;

    if (auto pt = get<PrimitiveType>(left); pt && pt->type == PrimitiveType::Boolean)
    {
        if (auto st = get<SingletonType>(right); st && st->variant.get_if<BooleanSingleton>())
            return right;
        if (auto nt = get<NegationType>(right))
        {
            if (auto st = get<SingletonType>(follow(nt->ty)); st && st->variant.get_if<BooleanSingleton>())
            {
                if (st->variant == BooleanSingleton{true})
                    return builtinTypes->falseType;
                else
                    return builtinTypes->trueType;
            }
        }
    }
    else if (auto pt = get<PrimitiveType>(right); pt && pt->type == PrimitiveType::Boolean)
    {
        if (auto st = get<SingletonType>(left); st && st->variant.get_if<BooleanSingleton>())
            return left;
        if (auto nt = get<NegationType>(left))
        {
            if (auto st = get<SingletonType>(follow(nt->ty)); st && st->variant.get_if<BooleanSingleton>())
            {
                if (st->variant == BooleanSingleton{true})
                    return builtinTypes->falseType;
                else
                    return builtinTypes->trueType;
            }
        }
    }

    if (const auto [lt, rt] = get2<TableType, TableType>(left, right); lt && rt)
    {
        if (1 == lt->props.size())
        {
            const auto [propName, leftProp] = *begin(lt->props);

            auto it = rt->props.find(propName);
            if (it != rt->props.end() && leftProp.isShared() && it->second.isShared())
            {
                Relation r = relate(leftProp.type(), it->second.type());

                switch (r)
                {
                case Relation::Disjoint:
                    return builtinTypes->neverType;
                case Relation::Coincident:
                    return right;
                default:
                    break;
                }
            }
        }
        else if (1 == rt->props.size())
            return basicIntersect(right, left);
    }

    Relation relation = relate(left, right);
    if (left == right || Relation::Coincident == relation)
        return left;

    if (relation == Relation::Disjoint)
        return builtinTypes->neverType;
    else if (relation == Relation::Subset)
        return left;
    else if (relation == Relation::Superset)
        return right;

    return std::nullopt;
}

TypeId TypeSimplifier::intersect(TypeId left, TypeId right)
{
    RecursionLimiter rl(&recursionDepth, 15);

    left = simplify(left);
    right = simplify(right);

    if (left == right)
        return left;

    if (get<AnyType>(left) && get<ErrorType>(right))
        return right;
    if (get<AnyType>(right) && get<ErrorType>(left))
        return left;
    if (get<UnknownType>(left) && !get<ErrorType>(right))
        return right;
    if (get<UnknownType>(right) && !get<ErrorType>(left))
        return left;
    if (get<AnyType>(left))
        return arena->addType(UnionType{{right, builtinTypes->errorType}});
    if (get<AnyType>(right))
        return arena->addType(UnionType{{left, builtinTypes->errorType}});
    if (get<UnknownType>(left))
        return right;
    if (get<UnknownType>(right))
        return left;
    if (get<NeverType>(left))
        return left;
    if (get<NeverType>(right))
        return right;

    if (auto lf = get<FreeType>(left))
    {
        Relation r = relate(lf->upperBound, right);
        if (r == Relation::Subset || r == Relation::Coincident)
            return left;
    }
    else if (auto rf = get<FreeType>(right))
    {
        Relation r = relate(left, rf->upperBound);
        if (r == Relation::Superset || r == Relation::Coincident)
            return right;
    }

    if (isTypeVariable(left))
    {
        blockedTypes.insert(left);
        return arena->addType(IntersectionType{{left, right}});
    }

    if (isTypeVariable(right))
    {
        blockedTypes.insert(right);
        return arena->addType(IntersectionType{{left, right}});
    }

    if (auto ut = get<UnionType>(left))
    {
        if (get<UnionType>(right))
            return intersectUnions(left, right);
        else
            return intersectUnionWithType(left, right);
    }
    else if (auto ut = get<UnionType>(right))
        return intersectUnionWithType(right, left);

    if (auto it = get<IntersectionType>(left))
        return intersectIntersectionWithType(left, right);
    else if (auto it = get<IntersectionType>(right))
        return intersectIntersectionWithType(right, left);

    if (get<NegationType>(left))
    {
        if (get<NegationType>(right))
            return intersectNegations(left, right);
        else
            return intersectTypeWithNegation(left, right);
    }
    else if (get<NegationType>(right))
        return intersectTypeWithNegation(right, left);

    std::optional<TypeId> res = basicIntersect(left, right);
    if (res)
        return *res;
    else
        return arena->addType(IntersectionType{{left, right}});
}

TypeId TypeSimplifier::union_(TypeId left, TypeId right)
{
    RecursionLimiter rl(&recursionDepth, 15);

    left = simplify(left);
    right = simplify(right);

    if (get<NeverType>(left))
        return right;
    if (get<NeverType>(right))
        return left;

    if (auto leftUnion = get<UnionType>(left))
    {
        bool changed = false;
        std::set<TypeId> newParts;
        for (TypeId part : leftUnion)
        {
            if (get<NeverType>(part))
            {
                changed = true;
                continue;
            }

            Relation r = relate(part, right);
            switch (r)
            {
            case Relation::Coincident:
            case Relation::Superset:
                return left;
            case Relation::Subset:
                newParts.insert(right);
                changed = true;
                break;
            default:
                newParts.insert(part);
                newParts.insert(right);
                changed = true;
                break;
            }
        }

        if (!changed)
            return left;
        if (0 == newParts.size())
        {
            // If the left-side is changed but has no parts, then the left-side union is uninhabited.
            return right;
        }
        else if (1 == newParts.size())
            return *begin(newParts);
        else
            return arena->addType(UnionType{std::vector<TypeId>{begin(newParts), end(newParts)}});
    }
    else if (get<UnionType>(right))
        return union_(right, left);

    Relation r = relate(left, right);
    if (left == right || r == Relation::Coincident || r == Relation::Superset)
        return left;

    if (r == Relation::Subset)
        return right;

    if (auto as = get<SingletonType>(left))
    {
        if (auto abs = as->variant.get_if<BooleanSingleton>())
        {
            if (auto bs = get<SingletonType>(right))
            {
                if (auto bbs = bs->variant.get_if<BooleanSingleton>())
                {
                    if (abs->value != bbs->value)
                        return builtinTypes->booleanType;
                }
            }
        }
    }

    return arena->addType(UnionType{{left, right}});
}

TypeId TypeSimplifier::simplify(TypeId ty)
{
    DenseHashSet<TypeId> seen{nullptr};
    return simplify(ty, seen);
}

TypeId TypeSimplifier::simplify(TypeId ty, DenseHashSet<TypeId>& seen)
{
    RecursionLimiter limiter(&recursionDepth, 60);

    ty = follow(ty);

    if (seen.find(ty))
        return ty;
    seen.insert(ty);

    if (auto nt = get<NegationType>(ty))
    {
        TypeId negatedTy = follow(nt->ty);
        if (get<AnyType>(negatedTy))
            return arena->addType(UnionType{{builtinTypes->neverType, builtinTypes->errorType}});
        else if (get<UnknownType>(negatedTy))
            return builtinTypes->neverType;
        else if (get<NeverType>(negatedTy))
            return builtinTypes->unknownType;
        if (auto nnt = get<NegationType>(negatedTy))
            return simplify(nnt->ty, seen);
    }

    // Promote {x: never} to never
    if (auto tt = get<TableType>(ty))
    {
        if (1 == tt->props.size())
        {
            if (std::optional<TypeId> readTy = begin(tt->props)->second.readTy)
            {
                TypeId propTy = simplify(*readTy, seen);
                if (get<NeverType>(propTy))
                    return builtinTypes->neverType;
            }
        }
    }

    return ty;
}

SimplifyResult simplifyIntersection(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, TypeId left, TypeId right)
{
    LUAU_ASSERT(FFlag::DebugLuauDeferredConstraintResolution);

    TypeSimplifier s{builtinTypes, arena};

    // fprintf(stderr, "Intersect %s and %s ...\n", toString(left).c_str(), toString(right).c_str());

    TypeId res = s.intersect(left, right);

    // fprintf(stderr, "Intersect %s and %s -> %s\n", toString(left).c_str(), toString(right).c_str(), toString(res).c_str());

    return SimplifyResult{res, std::move(s.blockedTypes)};
}

SimplifyResult simplifyIntersection(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, std::set<TypeId> parts)
{
    LUAU_ASSERT(FFlag::DebugLuauDeferredConstraintResolution);

    TypeSimplifier s{builtinTypes, arena};

    TypeId res = s.intersectFromParts(std::move(parts));

    return SimplifyResult{res, std::move(s.blockedTypes)};
}

SimplifyResult simplifyUnion(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, TypeId left, TypeId right)
{
    LUAU_ASSERT(FFlag::DebugLuauDeferredConstraintResolution);

    TypeSimplifier s{builtinTypes, arena};

    TypeId res = s.union_(left, right);

    // fprintf(stderr, "Union %s and %s -> %s\n", toString(left).c_str(), toString(right).c_str(), toString(res).c_str());

    return SimplifyResult{res, std::move(s.blockedTypes)};
}

} // namespace Luau
