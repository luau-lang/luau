// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeReduction.h"

#include "Luau/Common.h"
#include "Luau/Error.h"
#include "Luau/RecursionCounter.h"
#include "Luau/VisitType.h"

#include <numeric>
#include <deque>

LUAU_FASTINTVARIABLE(LuauTypeReductionCartesianProductLimit, 100'000)
LUAU_FASTINTVARIABLE(LuauTypeReductionRecursionLimit, 700)
LUAU_FASTFLAGVARIABLE(DebugLuauDontReduceTypes, false)

namespace Luau
{

namespace
{

using detail::ReductionContext;

template<typename A, typename B, typename Thing>
std::pair<const A*, const B*> get2(const Thing& one, const Thing& two)
{
    const A* a = get<A>(one);
    const B* b = get<B>(two);
    return a && b ? std::make_pair(a, b) : std::make_pair(nullptr, nullptr);
}

struct TypeReducer
{
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<InternalErrorReporter> handle;

    DenseHashMap<TypeId, ReductionContext<TypeId>>* memoizedTypes;
    DenseHashMap<TypePackId, ReductionContext<TypePackId>>* memoizedTypePacks;
    DenseHashSet<TypeId>* cyclicTypes;

    int depth = 0;

    TypeId reduce(TypeId ty);
    TypePackId reduce(TypePackId tp);

    std::optional<TypeId> intersectionType(TypeId left, TypeId right);
    std::optional<TypeId> unionType(TypeId left, TypeId right);
    TypeId tableType(TypeId ty);
    TypeId functionType(TypeId ty);
    TypeId negationType(TypeId ty);

    bool isIrreducible(TypeId ty);
    bool isIrreducible(TypePackId tp);

    TypeId memoize(TypeId ty, TypeId reducedTy);
    TypePackId memoize(TypePackId tp, TypePackId reducedTp);

    // It's either cyclic with no memoized result, so we should terminate, or
    // there is a memoized result but one that's being reduced top-down, so
    // we need to return the root of that memoized result to tighten up things.
    TypeId memoizedOr(TypeId ty) const;
    TypePackId memoizedOr(TypePackId tp) const;

    using BinaryFold = std::optional<TypeId> (TypeReducer::*)(TypeId, TypeId);
    using UnaryFold = TypeId (TypeReducer::*)(TypeId);

    template<typename T>
    LUAU_NOINLINE std::pair<TypeId, T*> copy(TypeId ty, const T* t)
    {
        ty = follow(ty);

        if (auto ctx = memoizedTypes->find(ty))
            return {ctx->type, getMutable<T>(ctx->type)};

        TypeId copiedTy = arena->addType(*t);
        (*memoizedTypes)[ty] = {copiedTy, false};
        (*memoizedTypes)[copiedTy] = {copiedTy, false};
        return {copiedTy, getMutable<T>(copiedTy)};
    }

    template<typename T, typename Iter>
    void foldl_impl(Iter it, Iter endIt, BinaryFold f, std::vector<TypeId>* result, bool* didReduce)
    {
        RecursionLimiter rl{&depth, FInt::LuauTypeReductionRecursionLimit};

        while (it != endIt)
        {
            TypeId right = reduce(*it);
            *didReduce |= right != follow(*it);

            // We're hitting a case where the `currentTy` returned a type that's the same as `T`.
            // e.g. `(string?) & ~(false | nil)` became `(string?) & (~false & ~nil)` but the current iterator we're consuming doesn't know this.
            // We will need to recurse and traverse that first.
            if (auto t = get<T>(right))
            {
                foldl_impl<T>(begin(t), end(t), f, result, didReduce);
                ++it;
                continue;
            }

            bool replaced = false;
            auto resultIt = result->begin();
            while (resultIt != result->end())
            {
                TypeId left = *resultIt;
                if (left == right)
                {
                    replaced = true;
                    ++resultIt;
                    continue;
                }

                std::optional<TypeId> reduced = (this->*f)(left, right);
                if (reduced)
                {
                    *resultIt = *reduced;
                    ++resultIt;
                    replaced = true;
                }
                else
                {
                    ++resultIt;
                    continue;
                }
            }

            if (!replaced)
                result->push_back(right);

            *didReduce |= replaced;
            ++it;
        }
    }

    template<typename T>
    TypeId flatten(std::vector<TypeId>&& types)
    {
        if (types.size() == 1)
            return types[0];
        else
            return arena->addType(T{std::move(types)});
    }

    template<typename T, typename Iter>
    TypeId foldl(Iter it, Iter endIt, std::optional<TypeId> ty, BinaryFold f)
    {
        std::vector<TypeId> result;
        bool didReduce = false;
        foldl_impl<T>(it, endIt, f, &result, &didReduce);
        if (!didReduce && ty)
            return *ty;
        else
        {
            // If we've done any reduction, then we'll need to reduce it again, e.g.
            // `"a" | "b" | string` is reduced into `string | string`, which is then reduced into `string`.
            return reduce(flatten<T>(std::move(result)));
        }
    }

    template<typename T>
    TypeId apply(BinaryFold f, TypeId left, TypeId right)
    {
        left = follow(left);
        right = follow(right);

        if (get<T>(left) || get<T>(right))
        {
            std::vector<TypeId> types{left, right};
            return foldl<T>(begin(types), end(types), std::nullopt, f);
        }
        else if (auto reduced = (this->*f)(left, right))
            return *reduced;
        else
            return arena->addType(T{{left, right}});
    }

    template<typename Into, typename Over>
    TypeId distribute(TypeIterator<Over> it, TypeIterator<Over> endIt, BinaryFold f, TypeId ty)
    {
        std::vector<TypeId> result;
        while (it != endIt)
        {
            result.push_back(apply<Into>(f, *it, ty));
            ++it;
        }
        return flatten<Over>(std::move(result));
    }
};

TypeId TypeReducer::reduce(TypeId ty)
{
    ty = follow(ty);

    if (auto ctx = memoizedTypes->find(ty); ctx && ctx->irreducible)
        return ctx->type;
    else if (auto cyclicTy = cyclicTypes->find(ty))
        return *cyclicTy;

    RecursionLimiter rl{&depth, FInt::LuauTypeReductionRecursionLimit};

    TypeId result = nullptr;
    if (auto i = get<IntersectionType>(ty))
        result = foldl<IntersectionType>(begin(i), end(i), ty, &TypeReducer::intersectionType);
    else if (auto u = get<UnionType>(ty))
        result = foldl<UnionType>(begin(u), end(u), ty, &TypeReducer::unionType);
    else if (get<TableType>(ty) || get<MetatableType>(ty))
        result = tableType(ty);
    else if (get<FunctionType>(ty))
        result = functionType(ty);
    else if (get<NegationType>(ty))
        result = negationType(ty);
    else
        result = ty;

    return memoize(ty, result);
}

TypePackId TypeReducer::reduce(TypePackId tp)
{
    tp = follow(tp);

    if (auto ctx = memoizedTypePacks->find(tp); ctx && ctx->irreducible)
        return ctx->type;

    RecursionLimiter rl{&depth, FInt::LuauTypeReductionRecursionLimit};

    bool didReduce = false;
    TypePackIterator it = begin(tp);

    std::vector<TypeId> head;
    while (it != end(tp))
    {
        TypeId reducedTy = reduce(*it);
        head.push_back(reducedTy);
        didReduce |= follow(*it) != follow(reducedTy);
        ++it;
    }

    std::optional<TypePackId> tail = it.tail();
    if (tail)
    {
        if (auto vtp = get<VariadicTypePack>(follow(*it.tail())))
        {
            TypeId reducedTy = reduce(vtp->ty);
            if (follow(vtp->ty) != follow(reducedTy))
            {
                tail = arena->addTypePack(VariadicTypePack{reducedTy, vtp->hidden});
                didReduce = true;
            }
        }
    }

    if (!didReduce)
        return memoize(tp, tp);
    else if (head.empty() && tail)
        return memoize(tp, *tail);
    else
        return memoize(tp, arena->addTypePack(TypePack{std::move(head), tail}));
}

std::optional<TypeId> TypeReducer::intersectionType(TypeId left, TypeId right)
{
    LUAU_ASSERT(!get<IntersectionType>(left));
    LUAU_ASSERT(!get<IntersectionType>(right));

    if (get<NeverType>(left))
        return left; // never & T ~ never
    else if (get<NeverType>(right))
        return right; // T & never ~ never
    else if (get<UnknownType>(left))
        return right; // unknown & T ~ T
    else if (get<UnknownType>(right))
        return left; // T & unknown ~ T
    else if (get<AnyType>(left))
        return right; // any & T ~ T
    else if (get<AnyType>(right))
        return left; // T & any ~ T
    else if (get<FreeType>(left))
        return std::nullopt; // 'a & T ~ 'a & T
    else if (get<FreeType>(right))
        return std::nullopt; // T & 'a ~ T & 'a
    else if (get<GenericType>(left))
        return std::nullopt; // G & T ~ G & T
    else if (get<GenericType>(right))
        return std::nullopt; // T & G ~ T & G
    else if (get<ErrorType>(left))
        return std::nullopt; // error & T ~ error & T
    else if (get<ErrorType>(right))
        return std::nullopt; // T & error ~ T & error
    else if (auto ut = get<UnionType>(left))
        return reduce(distribute<IntersectionType>(begin(ut), end(ut), &TypeReducer::intersectionType, right)); // (A | B) & T ~ (A & T) | (B & T)
    else if (get<UnionType>(right))
        return intersectionType(right, left); // T & (A | B) ~ (A | B) & T
    else if (auto [p1, p2] = get2<PrimitiveType, PrimitiveType>(left, right); p1 && p2)
    {
        if (p1->type == p2->type)
            return left; // P1 & P2 ~ P1 iff P1 == P2
        else
            return builtinTypes->neverType; // P1 & P2 ~ never iff P1 != P2
    }
    else if (auto [p, s] = get2<PrimitiveType, SingletonType>(left, right); p && s)
    {
        if (p->type == PrimitiveType::String && get<StringSingleton>(s))
            return right; // string & "A" ~ "A"
        else if (p->type == PrimitiveType::Boolean && get<BooleanSingleton>(s))
            return right; // boolean & true ~ true
        else
            return builtinTypes->neverType; // string & true ~ never
    }
    else if (auto [s, p] = get2<SingletonType, PrimitiveType>(left, right); s && p)
        return intersectionType(right, left); // S & P ~ P & S
    else if (auto [p, f] = get2<PrimitiveType, FunctionType>(left, right); p && f)
    {
        if (p->type == PrimitiveType::Function)
            return right; // function & () -> () ~ () -> ()
        else
            return builtinTypes->neverType; // string & () -> () ~ never
    }
    else if (auto [f, p] = get2<FunctionType, PrimitiveType>(left, right); f && p)
        return intersectionType(right, left); // () -> () & P ~ P & () -> ()
    else if (auto [s1, s2] = get2<SingletonType, SingletonType>(left, right); s1 && s2)
    {
        if (*s1 == *s2)
            return left; // "a" & "a" ~ "a"
        else
            return builtinTypes->neverType; // "a" & "b" ~ never
    }
    else if (auto [c1, c2] = get2<ClassType, ClassType>(left, right); c1 && c2)
    {
        if (isSubclass(c1, c2))
            return left; // Derived & Base ~ Derived
        else if (isSubclass(c2, c1))
            return right; // Base & Derived ~ Derived
        else
            return builtinTypes->neverType; // Base & Unrelated ~ never
    }
    else if (auto [f1, f2] = get2<FunctionType, FunctionType>(left, right); f1 && f2)
        return std::nullopt; // TODO
    else if (auto [t1, t2] = get2<TableType, TableType>(left, right); t1 && t2)
    {
        if (t1->state == TableState::Free || t2->state == TableState::Free)
            return std::nullopt; // '{ x: T } & { x: U } ~ '{ x: T } & { x: U }
        else if (t1->state == TableState::Generic || t2->state == TableState::Generic)
            return std::nullopt; // '{ x: T } & { x: U } ~ '{ x: T } & { x: U }

        if (cyclicTypes->find(left))
            return std::nullopt; // (t1 where t1 = { p: t1 }) & {} ~ t1 & {}
        else if (cyclicTypes->find(right))
            return std::nullopt; // {} & (t1 where t1 = { p: t1 }) ~ {} & t1

        TypeId resultTy = arena->addType(TableType{});
        TableType* table = getMutable<TableType>(resultTy);
        table->state = t1->state == TableState::Sealed || t2->state == TableState::Sealed ? TableState::Sealed : TableState::Unsealed;

        for (const auto& [name, prop] : t1->props)
        {
            // TODO: when t1 has properties, we should also intersect that with the indexer in t2 if it exists,
            // even if we have the corresponding property in the other one.
            if (auto other = t2->props.find(name); other != t2->props.end())
            {
                TypeId propTy = apply<IntersectionType>(&TypeReducer::intersectionType, prop.type, other->second.type);
                if (get<NeverType>(propTy))
                    return builtinTypes->neverType; // { p : string } & { p : number } ~ { p : string & number } ~ { p : never } ~ never
                else
                    table->props[name] = {propTy}; // { p : string } & { p : ~"a" } ~ { p : string & ~"a" }
            }
            else
                table->props[name] = prop; // { p : string } & {} ~ { p : string }
        }

        for (const auto& [name, prop] : t2->props)
        {
            // TODO: And vice versa, t2 properties against t1 indexer if it exists,
            // even if we have the corresponding property in the other one.
            if (!t1->props.count(name))
                table->props[name] = {reduce(prop.type)}; // {} & { p : string & string } ~ { p : string }
        }

        if (t1->indexer && t2->indexer)
        {
            TypeId keyTy = apply<IntersectionType>(&TypeReducer::intersectionType, t1->indexer->indexType, t2->indexer->indexType);
            if (get<NeverType>(keyTy))
                return std::nullopt; // { [string]: _ } & { [number]: _ } ~ { [string]: _ } & { [number]: _ }

            TypeId valueTy = apply<IntersectionType>(&TypeReducer::intersectionType, t1->indexer->indexResultType, t2->indexer->indexResultType);
            if (get<NeverType>(valueTy))
                return builtinTypes->neverType; // { [_]: string } & { [_]: number } ~ { [_]: string & number } ~ { [_]: never } ~ never

            table->indexer = TableIndexer{keyTy, valueTy};
        }
        else if (t1->indexer)
        {
            TypeId keyTy = reduce(t1->indexer->indexType);
            TypeId valueTy = reduce(t1->indexer->indexResultType);
            table->indexer = TableIndexer{keyTy, valueTy}; // { [number]: boolean } & { p : string } ~ { p : string, [number]: boolean }
        }
        else if (t2->indexer)
        {
            TypeId keyTy = reduce(t2->indexer->indexType);
            TypeId valueTy = reduce(t2->indexer->indexResultType);
            table->indexer = TableIndexer{keyTy, valueTy}; // { p : string } & { [number]: boolean } ~ { p : string, [number]: boolean }
        }

        return resultTy;
    }
    else if (auto [mt, tt] = get2<MetatableType, TableType>(left, right); mt && tt)
        return std::nullopt; // TODO
    else if (auto [tt, mt] = get2<TableType, MetatableType>(left, right); tt && mt)
        return intersectionType(right, left); // T & M ~ M & T
    else if (auto [m1, m2] = get2<MetatableType, MetatableType>(left, right); m1 && m2)
        return std::nullopt; // TODO
    else if (auto nl = get<NegationType>(left))
    {
        // These should've been reduced already.
        TypeId nlTy = follow(nl->ty);
        LUAU_ASSERT(!get<UnknownType>(nlTy));
        LUAU_ASSERT(!get<NeverType>(nlTy));
        LUAU_ASSERT(!get<AnyType>(nlTy));
        LUAU_ASSERT(!get<IntersectionType>(nlTy));
        LUAU_ASSERT(!get<UnionType>(nlTy));

        if (auto [np, p] = get2<PrimitiveType, PrimitiveType>(nlTy, right); np && p)
        {
            if (np->type == p->type)
                return builtinTypes->neverType; // ~P1 & P2 ~ never iff P1 == P2
            else
                return right; // ~P1 & P2 ~ P2 iff P1 != P2
        }
        else if (auto [ns, s] = get2<SingletonType, SingletonType>(nlTy, right); ns && s)
        {
            if (*ns == *s)
                return builtinTypes->neverType; // ~"A" & "A" ~ never
            else
                return right; // ~"A" & "B" ~ "B"
        }
        else if (auto [ns, p] = get2<SingletonType, PrimitiveType>(nlTy, right); ns && p)
        {
            if (get<StringSingleton>(ns) && p->type == PrimitiveType::String)
                return std::nullopt; // ~"A" & string ~ ~"A" & string
            else if (get<BooleanSingleton>(ns) && p->type == PrimitiveType::Boolean)
            {
                // Because booleans contain a fixed amount of values (2), we can do something cooler with this one.
                const BooleanSingleton* b = get<BooleanSingleton>(ns);
                return arena->addType(SingletonType{BooleanSingleton{!b->value}}); // ~false & boolean ~ true
            }
            else
                return right; // ~"A" & number ~ number
        }
        else if (auto [np, s] = get2<PrimitiveType, SingletonType>(nlTy, right); np && s)
        {
            if (np->type == PrimitiveType::String && get<StringSingleton>(s))
                return builtinTypes->neverType; // ~string & "A" ~ never
            else if (np->type == PrimitiveType::Boolean && get<BooleanSingleton>(s))
                return builtinTypes->neverType; // ~boolean & true ~ never
            else
                return right; // ~P & "A" ~ "A"
        }
        else if (auto [np, f] = get2<PrimitiveType, FunctionType>(nlTy, right); np && f)
        {
            if (np->type == PrimitiveType::Function)
                return builtinTypes->neverType; // ~function & () -> () ~ never
            else
                return right; // ~string & () -> () ~ () -> ()
        }
        else if (auto [nc, c] = get2<ClassType, ClassType>(nlTy, right); nc && c)
        {
            if (isSubclass(nc, c))
                return std::nullopt; // ~Derived & Base ~ ~Derived & Base
            else if (isSubclass(c, nc))
                return builtinTypes->neverType; // ~Base & Derived ~ never
            else
                return right; // ~Base & Unrelated ~ Unrelated
        }
        else
            return std::nullopt; // TODO
    }
    else if (get<NegationType>(right))
        return intersectionType(right, left); // T & ~U ~ ~U & T
    else
        return builtinTypes->neverType; // for all T and U except the ones handled above, T & U ~ never
}

std::optional<TypeId> TypeReducer::unionType(TypeId left, TypeId right)
{
    LUAU_ASSERT(!get<UnionType>(left));
    LUAU_ASSERT(!get<UnionType>(right));

    if (get<NeverType>(left))
        return right; // never | T ~ T
    else if (get<NeverType>(right))
        return left; // T | never ~ T
    else if (get<UnknownType>(left))
        return left; // unknown | T ~ unknown
    else if (get<UnknownType>(right))
        return right; // T | unknown ~ unknown
    else if (get<AnyType>(left))
        return left; // any | T ~ any
    else if (get<AnyType>(right))
        return right; // T | any ~ any
    else if (get<ErrorType>(left))
        return std::nullopt; // error | T ~ error | T
    else if (get<ErrorType>(right))
        return std::nullopt; // T | error ~ T | error
    else if (auto [p1, p2] = get2<PrimitiveType, PrimitiveType>(left, right); p1 && p2)
    {
        if (p1->type == p2->type)
            return left; // P1 | P2 ~ P1 iff P1 == P2
        else
            return std::nullopt; // P1 | P2 ~ P1 | P2 iff P1 != P2
    }
    else if (auto [p, s] = get2<PrimitiveType, SingletonType>(left, right); p && s)
    {
        if (p->type == PrimitiveType::String && get<StringSingleton>(s))
            return left; // string | "A" ~ string
        else if (p->type == PrimitiveType::Boolean && get<BooleanSingleton>(s))
            return left; // boolean | true ~ boolean
        else
            return std::nullopt; // string | true ~ string | true
    }
    else if (auto [s, p] = get2<SingletonType, PrimitiveType>(left, right); s && p)
        return unionType(right, left); // S | P ~ P | S
    else if (auto [p, f] = get2<PrimitiveType, FunctionType>(left, right); p && f)
    {
        if (p->type == PrimitiveType::Function)
            return left; // function | () -> () ~ function
        else
            return std::nullopt; // P | () -> () ~ P | () -> ()
    }
    else if (auto [f, p] = get2<FunctionType, PrimitiveType>(left, right); f && p)
        return unionType(right, left); // () -> () | P ~ P | () -> ()
    else if (auto [s1, s2] = get2<SingletonType, SingletonType>(left, right); s1 && s2)
    {
        if (*s1 == *s2)
            return left; // "a" | "a" ~ "a"
        else
            return std::nullopt; // "a" | "b" ~ "a" | "b"
    }
    else if (auto [c1, c2] = get2<ClassType, ClassType>(left, right); c1 && c2)
    {
        if (isSubclass(c1, c2))
            return right; // Derived | Base ~ Base
        else if (isSubclass(c2, c1))
            return left; // Base | Derived ~ Base
        else
            return std::nullopt; // Base | Unrelated ~ Base | Unrelated
    }
    else if (auto [nt, it] = get2<NegationType, IntersectionType>(left, right); nt && it)
        return reduce(distribute<UnionType>(begin(it), end(it), &TypeReducer::unionType, left)); // ~T | (A & B) ~ (~T | A) & (~T | B)
    else if (auto [it, nt] = get2<IntersectionType, NegationType>(left, right); it && nt)
        return unionType(right, left); // (A & B) | ~T ~ ~T | (A & B)
    else if (auto [nl, nr] = get2<NegationType, NegationType>(left, right); nl && nr)
    {
        // These should've been reduced already.
        TypeId nlTy = follow(nl->ty);
        TypeId nrTy = follow(nr->ty);
        LUAU_ASSERT(!get<UnknownType>(nlTy) && !get<UnknownType>(nrTy));
        LUAU_ASSERT(!get<NeverType>(nlTy) && !get<NeverType>(nrTy));
        LUAU_ASSERT(!get<AnyType>(nlTy) && !get<AnyType>(nrTy));
        LUAU_ASSERT(!get<IntersectionType>(nlTy) && !get<IntersectionType>(nrTy));
        LUAU_ASSERT(!get<UnionType>(nlTy) && !get<UnionType>(nrTy));

        if (auto [npl, npr] = get2<PrimitiveType, PrimitiveType>(nlTy, nrTy); npl && npr)
        {
            if (npl->type == npr->type)
                return left; // ~P1 | ~P2 ~ ~P1 iff P1 == P2
            else
                return builtinTypes->unknownType; // ~P1 | ~P2 ~ ~P1 iff P1 != P2
        }
        else if (auto [nsl, nsr] = get2<SingletonType, SingletonType>(nlTy, nrTy); nsl && nsr)
        {
            if (*nsl == *nsr)
                return left; // ~"A" | ~"A" ~ ~"A"
            else
                return builtinTypes->unknownType; // ~"A" | ~"B" ~ unknown
        }
        else if (auto [ns, np] = get2<SingletonType, PrimitiveType>(nlTy, nrTy); ns && np)
        {
            if (get<StringSingleton>(ns) && np->type == PrimitiveType::String)
                return left; // ~"A" | ~string ~ ~"A"
            else if (get<BooleanSingleton>(ns) && np->type == PrimitiveType::Boolean)
                return left; // ~false | ~boolean ~ ~false
            else
                return builtinTypes->unknownType; // ~"A" | ~P ~ unknown
        }
        else if (auto [np, ns] = get2<PrimitiveType, SingletonType>(nlTy, nrTy); np && ns)
            return unionType(right, left); // ~P | ~S ~ ~S | ~P
        else
            return std::nullopt; // TODO!
    }
    else if (auto nl = get<NegationType>(left))
    {
        // These should've been reduced already.
        TypeId nlTy = follow(nl->ty);
        LUAU_ASSERT(!get<UnknownType>(nlTy));
        LUAU_ASSERT(!get<NeverType>(nlTy));
        LUAU_ASSERT(!get<AnyType>(nlTy));
        LUAU_ASSERT(!get<IntersectionType>(nlTy));
        LUAU_ASSERT(!get<UnionType>(nlTy));

        if (auto [np, p] = get2<PrimitiveType, PrimitiveType>(nlTy, right); np && p)
        {
            if (np->type == p->type)
                return builtinTypes->unknownType; // ~P1 | P2 ~ unknown iff P1 == P2
            else
                return left; // ~P1 | P2 ~ ~P1 iff P1 != P2
        }
        else if (auto [ns, s] = get2<SingletonType, SingletonType>(nlTy, right); ns && s)
        {
            if (*ns == *s)
                return builtinTypes->unknownType; // ~"A" | "A" ~ unknown
            else
                return left; // ~"A" | "B" ~ ~"A"
        }
        else if (auto [ns, p] = get2<SingletonType, PrimitiveType>(nlTy, right); ns && p)
        {
            if (get<StringSingleton>(ns) && p->type == PrimitiveType::String)
                return builtinTypes->unknownType; // ~"A" | string ~ unknown
            else if (get<BooleanSingleton>(ns) && p->type == PrimitiveType::Boolean)
                return builtinTypes->unknownType; // ~false | boolean ~ unknown
            else
                return left; // ~"A" | T ~ ~"A"
        }
        else if (auto [np, s] = get2<PrimitiveType, SingletonType>(nlTy, right); np && s)
        {
            if (np->type == PrimitiveType::String && get<StringSingleton>(s))
                return std::nullopt; // ~string | "A" ~ ~string | "A"
            else if (np->type == PrimitiveType::Boolean && get<BooleanSingleton>(s))
            {
                const BooleanSingleton* b = get<BooleanSingleton>(s);
                return negationType(arena->addType(SingletonType{BooleanSingleton{!b->value}})); // ~boolean | false ~ ~true
            }
            else
                return left; // ~P | "A" ~ ~P
        }
        else if (auto [nc, c] = get2<ClassType, ClassType>(nlTy, right); nc && c)
        {
            if (isSubclass(nc, c))
                return builtinTypes->unknownType; // ~Derived | Base ~ unknown
            else if (isSubclass(c, nc))
                return std::nullopt; // ~Base | Derived ~ ~Base | Derived
            else
                return left; // ~Base | Unrelated ~ ~Base
        }
        else
            return std::nullopt; // TODO
    }
    else if (get<NegationType>(right))
        return unionType(right, left); // T | ~U ~ ~U | T
    else
        return std::nullopt; // for all T and U except the ones handled above, T | U ~ T | U
}

TypeId TypeReducer::tableType(TypeId ty)
{
    if (auto mt = get<MetatableType>(ty))
    {
        auto [copiedTy, copied] = copy(ty, mt);
        copied->table = reduce(mt->table);
        copied->metatable = reduce(mt->metatable);
        return copiedTy;
    }
    else if (auto tt = get<TableType>(ty))
    {
        // Because of `typeof()`, we need to preserve pointer identity of free/unsealed tables so that
        // all mutations that occurs on this will be applied without leaking the implementation details.
        // As a result, we'll just use the type instead of cloning it if it's free/unsealed.
        //
        // We could choose to do in-place reductions here, but to be on the safer side, I propose that we do not.
        if (tt->state == TableState::Free || tt->state == TableState::Unsealed)
            return ty;

        auto [copiedTy, copied] = copy(ty, tt);

        for (auto& [name, prop] : copied->props)
        {
            TypeId propTy = reduce(prop.type);
            if (get<NeverType>(propTy))
                return builtinTypes->neverType;
            else
                prop.type = propTy;
        }

        if (copied->indexer)
        {
            TypeId keyTy = reduce(copied->indexer->indexType);
            TypeId valueTy = reduce(copied->indexer->indexResultType);
            copied->indexer = TableIndexer{keyTy, valueTy};
        }

        for (TypeId& ty : copied->instantiatedTypeParams)
            ty = reduce(ty);

        for (TypePackId& tp : copied->instantiatedTypePackParams)
            tp = reduce(tp);

        return copiedTy;
    }
    else
        handle->ice("TypeReducer::tableType expects a TableType or MetatableType");
}

TypeId TypeReducer::functionType(TypeId ty)
{
    const FunctionType* f = get<FunctionType>(ty);
    if (!f)
        handle->ice("TypeReducer::functionType expects a FunctionType");

    // TODO: once we have bounded quantification, we need to be able to reduce the generic bounds.
    auto [copiedTy, copied] = copy(ty, f);
    copied->argTypes = reduce(f->argTypes);
    copied->retTypes = reduce(f->retTypes);
    return copiedTy;
}

TypeId TypeReducer::negationType(TypeId ty)
{
    const NegationType* n = get<NegationType>(ty);
    if (!n)
        return arena->addType(NegationType{ty});

    if (auto nn = get<NegationType>(n->ty))
        return nn->ty; // ~~T ~ T
    else if (get<NeverType>(n->ty))
        return builtinTypes->unknownType; // ~never ~ unknown
    else if (get<UnknownType>(n->ty))
        return builtinTypes->neverType; // ~unknown ~ never
    else if (get<AnyType>(n->ty))
        return builtinTypes->anyType; // ~any ~ any
    else if (auto ni = get<IntersectionType>(n->ty))
    {
        std::vector<TypeId> options;
        for (TypeId part : ni)
            options.push_back(negationType(arena->addType(NegationType{part})));
        return reduce(flatten<UnionType>(std::move(options))); // ~(T & U) ~ (~T | ~U)
    }
    else if (auto nu = get<UnionType>(n->ty))
    {
        std::vector<TypeId> parts;
        for (TypeId option : nu)
            parts.push_back(negationType(arena->addType(NegationType{option})));
        return reduce(flatten<IntersectionType>(std::move(parts))); // ~(T | U) ~ (~T & ~U)
    }
    else
        return ty; // for all T except the ones handled above, ~T ~ ~T
}

bool TypeReducer::isIrreducible(TypeId ty)
{
    ty = follow(ty);

    // Only does shallow check, the TypeReducer itself already does deep traversal.
    if (auto ctx = memoizedTypes->find(ty); ctx && ctx->irreducible)
        return true;
    else if (get<FreeType>(ty) || get<BlockedType>(ty) || get<PendingExpansionType>(ty))
        return false;
    else if (auto tt = get<TableType>(ty); tt && (tt->state == TableState::Free || tt->state == TableState::Unsealed))
        return false;
    else
        return true;
}

bool TypeReducer::isIrreducible(TypePackId tp)
{
    tp = follow(tp);

    // Only does shallow check, the TypeReducer itself already does deep traversal.
    if (auto ctx = memoizedTypePacks->find(tp); ctx && ctx->irreducible)
        return true;
    else if (get<FreeTypePack>(tp) || get<BlockedTypePack>(tp))
        return false;
    else if (auto vtp = get<VariadicTypePack>(tp))
        return isIrreducible(vtp->ty);
    else
        return true;
}

TypeId TypeReducer::memoize(TypeId ty, TypeId reducedTy)
{
    ty = follow(ty);
    reducedTy = follow(reducedTy);

    // The irreducibility of this [`reducedTy`] depends on whether its contents are themselves irreducible.
    // We don't need to recurse much further than that, because we already record the irreducibility from
    // the bottom up.
    bool irreducible = isIrreducible(reducedTy);
    if (auto it = get<IntersectionType>(reducedTy))
    {
        for (TypeId part : it)
            irreducible &= isIrreducible(part);
    }
    else if (auto ut = get<UnionType>(reducedTy))
    {
        for (TypeId option : ut)
            irreducible &= isIrreducible(option);
    }
    else if (auto tt = get<TableType>(reducedTy))
    {
        for (auto& [k, p] : tt->props)
            irreducible &= isIrreducible(p.type);

        if (tt->indexer)
        {
            irreducible &= isIrreducible(tt->indexer->indexType);
            irreducible &= isIrreducible(tt->indexer->indexResultType);
        }

        for (auto ta : tt->instantiatedTypeParams)
            irreducible &= isIrreducible(ta);

        for (auto tpa : tt->instantiatedTypePackParams)
            irreducible &= isIrreducible(tpa);
    }
    else if (auto mt = get<MetatableType>(reducedTy))
    {
        irreducible &= isIrreducible(mt->table);
        irreducible &= isIrreducible(mt->metatable);
    }
    else if (auto ft = get<FunctionType>(reducedTy))
    {
        irreducible &= isIrreducible(ft->argTypes);
        irreducible &= isIrreducible(ft->retTypes);
    }
    else if (auto nt = get<NegationType>(reducedTy))
        irreducible &= isIrreducible(nt->ty);

    (*memoizedTypes)[ty] = {reducedTy, irreducible};
    (*memoizedTypes)[reducedTy] = {reducedTy, irreducible};
    return reducedTy;
}

TypePackId TypeReducer::memoize(TypePackId tp, TypePackId reducedTp)
{
    tp = follow(tp);
    reducedTp = follow(reducedTp);

    bool irreducible = isIrreducible(reducedTp);
    TypePackIterator it = begin(tp);
    while (it != end(tp))
    {
        irreducible &= isIrreducible(*it);
        ++it;
    }

    if (it.tail())
        irreducible &= isIrreducible(*it.tail());

    (*memoizedTypePacks)[tp] = {reducedTp, irreducible};
    (*memoizedTypePacks)[reducedTp] = {reducedTp, irreducible};
    return reducedTp;
}

TypeId TypeReducer::memoizedOr(TypeId ty) const
{
    ty = follow(ty);

    if (auto ctx = memoizedTypes->find(ty))
        return ctx->type;
    else
        return ty;
};

TypePackId TypeReducer::memoizedOr(TypePackId tp) const
{
    tp = follow(tp);

    if (auto ctx = memoizedTypePacks->find(tp))
        return ctx->type;
    else
        return tp;
};

struct MarkCycles : TypeVisitor
{
    DenseHashSet<TypeId> cyclicTypes{nullptr};

    void cycle(TypeId ty) override
    {
        cyclicTypes.insert(ty);
    }

    bool visit(TypeId ty) override
    {
        return !cyclicTypes.find(ty);
    }
};

} // namespace

TypeReduction::TypeReduction(
    NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<InternalErrorReporter> handle, const TypeReductionOptions& opts)
    : arena(arena)
    , builtinTypes(builtinTypes)
    , handle(handle)
    , options(opts)
{
}

std::optional<TypeId> TypeReduction::reduce(TypeId ty)
{
    ty = follow(ty);

    if (FFlag::DebugLuauDontReduceTypes)
        return ty;
    else if (!options.allowTypeReductionsFromOtherArenas && ty->owningArena != arena)
        return ty;
    else if (auto ctx = memoizedTypes.find(ty); ctx && ctx->irreducible)
        return ctx->type;
    else if (hasExceededCartesianProductLimit(ty))
        return std::nullopt;

    try
    {
        MarkCycles finder;
        finder.traverse(ty);

        TypeReducer reducer{arena, builtinTypes, handle, &memoizedTypes, &memoizedTypePacks, &finder.cyclicTypes};
        return reducer.reduce(ty);
    }
    catch (const RecursionLimitException&)
    {
        return std::nullopt;
    }
}

std::optional<TypePackId> TypeReduction::reduce(TypePackId tp)
{
    tp = follow(tp);

    if (FFlag::DebugLuauDontReduceTypes)
        return tp;
    else if (!options.allowTypeReductionsFromOtherArenas && tp->owningArena != arena)
        return tp;
    else if (auto ctx = memoizedTypePacks.find(tp); ctx && ctx->irreducible)
        return ctx->type;
    else if (hasExceededCartesianProductLimit(tp))
        return std::nullopt;

    try
    {
        MarkCycles finder;
        finder.traverse(tp);

        TypeReducer reducer{arena, builtinTypes, handle, &memoizedTypes, &memoizedTypePacks, &finder.cyclicTypes};
        return reducer.reduce(tp);
    }
    catch (const RecursionLimitException&)
    {
        return std::nullopt;
    }
}

std::optional<TypeFun> TypeReduction::reduce(const TypeFun& fun)
{
    if (FFlag::DebugLuauDontReduceTypes)
        return fun;

    // TODO: once we have bounded quantification, we need to be able to reduce the generic bounds.
    if (auto reducedTy = reduce(fun.type))
        return TypeFun{fun.typeParams, fun.typePackParams, *reducedTy};

    return std::nullopt;
}

size_t TypeReduction::cartesianProductSize(TypeId ty) const
{
    ty = follow(ty);

    auto it = get<IntersectionType>(follow(ty));
    if (!it)
        return 1;

    return std::accumulate(begin(it), end(it), size_t(1), [](size_t acc, TypeId ty) {
        if (auto ut = get<UnionType>(ty))
            return acc * std::distance(begin(ut), end(ut));
        else if (get<NeverType>(ty))
            return acc * 0;
        else
            return acc * 1;
    });
}

bool TypeReduction::hasExceededCartesianProductLimit(TypeId ty) const
{
    return cartesianProductSize(ty) >= size_t(FInt::LuauTypeReductionCartesianProductLimit);
}

bool TypeReduction::hasExceededCartesianProductLimit(TypePackId tp) const
{
    TypePackIterator it = begin(tp);

    while (it != end(tp))
    {
        if (hasExceededCartesianProductLimit(*it))
            return true;

        ++it;
    }

    if (auto tail = it.tail())
    {
        if (auto vtp = get<VariadicTypePack>(follow(*tail)))
        {
            if (hasExceededCartesianProductLimit(vtp->ty))
                return true;
        }
    }

    return false;
}

} // namespace Luau
