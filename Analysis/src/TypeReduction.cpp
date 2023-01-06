// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeReduction.h"

#include "Luau/Common.h"
#include "Luau/Error.h"
#include "Luau/RecursionCounter.h"

#include <numeric>
#include <deque>

LUAU_FASTINTVARIABLE(LuauTypeReductionCartesianProductLimit, 100'000)
LUAU_FASTINTVARIABLE(LuauTypeReductionRecursionLimit, 700)
LUAU_FASTFLAGVARIABLE(DebugLuauDontReduceTypes, false)

namespace Luau
{

namespace
{

struct RecursionGuard : RecursionLimiter
{
    std::deque<const void*>* seen;

    RecursionGuard(int* count, int limit, std::deque<const void*>* seen)
        : RecursionLimiter(count, limit)
        , seen(seen)
    {
        // count has been incremented, which should imply that seen has already had an element pushed in.
        LUAU_ASSERT(size_t(*count) == seen->size());
    }

    ~RecursionGuard()
    {
        LUAU_ASSERT(!seen->empty()); // It is UB to pop_back() on an empty deque.
        seen->pop_back();
    }
};

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

    TypeId reduce(TypeId ty);
    TypePackId reduce(TypePackId tp);

    std::optional<TypeId> intersectionType(TypeId left, TypeId right);
    std::optional<TypeId> unionType(TypeId left, TypeId right);
    TypeId tableType(TypeId ty);
    TypeId functionType(TypeId ty);
    TypeId negationType(TypeId ty);

    std::deque<const void*> seen;
    int depth = 0;

    RecursionGuard guard(TypeId ty);
    RecursionGuard guard(TypePackId tp);

    std::unordered_map<TypeId, TypeId> copies;

    template<typename T>
    LUAU_NOINLINE std::pair<TypeId, T*> copy(TypeId ty, const T* t)
    {
        if (auto it = copies.find(ty); it != copies.end())
            return {it->second, getMutable<T>(it->second)};

        TypeId copiedTy = arena->addType(*t);
        copies[ty] = copiedTy;
        return {copiedTy, getMutable<T>(copiedTy)};
    }

    using Folder = std::optional<TypeId> (TypeReducer::*)(TypeId, TypeId);

    template<typename T, typename Iter>
    void foldl_impl(Iter it, Iter endIt, Folder f, NotNull<std::vector<TypeId>> result)
    {
        while (it != endIt)
        {
            bool replaced = false;
            TypeId currentTy = reduce(*it);
            RecursionGuard rg = guard(*it);

            // We're hitting a case where the `currentTy` returned a type that's the same as `T`.
            // e.g. `(string?) & ~(false | nil)` became `(string?) & (~false & ~nil)` but the current iterator we're consuming doesn't know this.
            // We will need to recurse and traverse that first.
            if (auto t = get<T>(currentTy))
            {
                foldl_impl<T>(begin(t), end(t), f, result);
                ++it;
                continue;
            }

            auto resultIt = result->begin();
            while (resultIt != result->end())
            {
                TypeId& ty = *resultIt;

                std::optional<TypeId> reduced = (this->*f)(ty, currentTy);
                if (reduced && replaced)
                {
                    // We want to erase any other elements that occurs after the first replacement too.
                    // e.g. `"a" | "b" | string` where `"a"` and `"b"` is in the `result` vector, then `string` replaces both `"a"` and `"b"`.
                    // If we don't erase redundant elements, `"b"` may be kept or be replaced by `string`, leaving us with `string | string`.
                    resultIt = result->erase(resultIt);
                }
                else if (reduced && !replaced)
                {
                    ++resultIt;
                    replaced = true;
                    ty = *reduced;
                }
                else
                {
                    ++resultIt;
                    continue;
                }
            }

            if (!replaced)
                result->push_back(currentTy);

            ++it;
        }
    }

    template<typename T, typename Iter>
    TypeId foldl(Iter it, Iter endIt, Folder f)
    {
        std::vector<TypeId> result;
        foldl_impl<T>(it, endIt, f, NotNull{&result});
        if (result.size() == 1)
            return result[0];
        else
            return arena->addType(T{std::move(result)});
    }
};

TypeId TypeReducer::reduce(TypeId ty)
{
    ty = follow(ty);

    if (std::find(seen.begin(), seen.end(), ty) != seen.end())
        return ty;

    RecursionGuard rg = guard(ty);

    if (auto i = get<IntersectionType>(ty))
        return foldl<IntersectionType>(begin(i), end(i), &TypeReducer::intersectionType);
    else if (auto u = get<UnionType>(ty))
        return foldl<UnionType>(begin(u), end(u), &TypeReducer::unionType);
    else if (get<TableType>(ty) || get<MetatableType>(ty))
        return tableType(ty);
    else if (get<FunctionType>(ty))
        return functionType(ty);
    else if (auto n = get<NegationType>(ty))
        return negationType(follow(n->ty));
    else
        return ty;
}

TypePackId TypeReducer::reduce(TypePackId tp)
{
    tp = follow(tp);

    if (std::find(seen.begin(), seen.end(), tp) != seen.end())
        return tp;

    RecursionGuard rg = guard(tp);

    TypePackIterator it = begin(tp);

    std::vector<TypeId> head;
    while (it != end(tp))
    {
        head.push_back(reduce(*it));
        ++it;
    }

    std::optional<TypePackId> tail = it.tail();
    if (tail)
    {
        if (auto vtp = get<VariadicTypePack>(follow(*it.tail())))
            tail = arena->addTypePack(VariadicTypePack{reduce(vtp->ty), vtp->hidden});
    }

    return arena->addTypePack(TypePack{std::move(head), tail});
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
    else if (get<ErrorType>(left))
        return std::nullopt; // error & T ~ error & T
    else if (get<ErrorType>(right))
        return std::nullopt; // T & error ~ T & error
    else if (auto ut = get<UnionType>(left))
    {
        std::vector<TypeId> options;
        for (TypeId option : ut)
        {
            if (auto result = intersectionType(option, right))
                options.push_back(*result);
            else
                options.push_back(arena->addType(IntersectionType{{option, right}}));
        }

        return foldl<UnionType>(begin(options), end(options), &TypeReducer::unionType); // (A | B) & T ~ (A & T) | (B & T)
    }
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
    {
        if (std::find(seen.begin(), seen.end(), left) != seen.end())
            return std::nullopt;
        else if (std::find(seen.begin(), seen.end(), right) != seen.end())
            return std::nullopt;

        return std::nullopt; // TODO
    }
    else if (auto [t1, t2] = get2<TableType, TableType>(left, right); t1 && t2)
    {
        if (t1->state == TableState::Free || t2->state == TableState::Free)
            return std::nullopt; // '{ x: T } & { x: U } ~ '{ x: T } & { x: U }
        else if (t1->state == TableState::Generic || t2->state == TableState::Generic)
            return std::nullopt; // '{ x: T } & { x: U } ~ '{ x: T } & { x: U }

        if (std::find(seen.begin(), seen.end(), left) != seen.end())
            return std::nullopt;
        else if (std::find(seen.begin(), seen.end(), right) != seen.end())
            return std::nullopt;

        TypeId resultTy = arena->addType(TableType{});
        TableType* table = getMutable<TableType>(resultTy);
        table->state = t1->state == TableState::Sealed || t2->state == TableState::Sealed ? TableState::Sealed : TableState::Unsealed;

        for (const auto& [name, prop] : t1->props)
        {
            // TODO: when t1 has properties, we should also intersect that with the indexer in t2 if it exists,
            // even if we have the corresponding property in the other one.
            if (auto other = t2->props.find(name); other != t2->props.end())
            {
                std::vector<TypeId> parts{prop.type, other->second.type};
                TypeId propTy = foldl<IntersectionType>(begin(parts), end(parts), &TypeReducer::intersectionType);
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
                table->props[name] = prop; // {} & { p : string } ~ { p : string }
        }

        if (t1->indexer && t2->indexer)
        {
            std::vector<TypeId> keyParts{t1->indexer->indexType, t2->indexer->indexType};
            TypeId keyTy = foldl<IntersectionType>(begin(keyParts), end(keyParts), &TypeReducer::intersectionType);
            if (get<NeverType>(keyTy))
                return builtinTypes->neverType; // { [string]: _ } & { [number]: _ } ~ { [string & number]: _ } ~ { [never]: _ } ~ never

            std::vector<TypeId> valueParts{t1->indexer->indexResultType, t2->indexer->indexResultType};
            TypeId valueTy = foldl<IntersectionType>(begin(valueParts), end(valueParts), &TypeReducer::intersectionType);
            if (get<NeverType>(valueTy))
                return builtinTypes->neverType; // { [_]: string } & { [_]: number } ~ { [_]: string & number } ~ { [_]: never } ~ never

            table->indexer = TableIndexer{keyTy, valueTy};
        }
        else if (t1->indexer)
            table->indexer = t1->indexer; // { [number]: boolean } & { p : string } ~ { p : string, [number]: boolean }
        else if (t2->indexer)
            table->indexer = t2->indexer; // { p : string } & { [number]: boolean } ~ { p : string, [number]: boolean }

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
    {
        std::vector<TypeId> parts;
        for (TypeId option : it)
        {
            if (auto result = unionType(left, option))
                parts.push_back(*result);
            else
            {
                // TODO: does there exist a reduced form such that `~T | A` hasn't already reduced it, if `A & B` is irreducible?
                // I want to say yes, but I can't generate a case that hits this code path.
                parts.push_back(arena->addType(UnionType{{left, option}}));
            }
        }

        return foldl<IntersectionType>(begin(parts), end(parts), &TypeReducer::intersectionType); // ~T | (A & B) ~ (~T | A) & (~T | B)
    }
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
    RecursionGuard rg = guard(ty);

    if (auto mt = get<MetatableType>(ty))
    {
        auto [copiedTy, copied] = copy(ty, mt);
        copied->table = reduce(mt->table);
        copied->metatable = reduce(mt->metatable);
        return copiedTy;
    }
    else if (auto tt = get<TableType>(ty))
    {
        auto [copiedTy, copied] = copy(ty, tt);

        for (auto& [name, prop] : copied->props)
            prop.type = reduce(prop.type);

        if (auto& indexer = copied->indexer)
        {
            indexer->indexType = reduce(indexer->indexType);
            indexer->indexResultType = reduce(indexer->indexResultType);
        }

        for (TypeId& ty : copied->instantiatedTypeParams)
            ty = reduce(ty);

        for (TypePackId& tp : copied->instantiatedTypePackParams)
            tp = reduce(tp);

        return copiedTy;
    }
    else
        handle->ice("Unexpected type in TypeReducer::tableType");
}

TypeId TypeReducer::functionType(TypeId ty)
{
    RecursionGuard rg = guard(ty);

    const FunctionType* f = get<FunctionType>(ty);
    if (!f)
        handle->ice("TypeReducer::reduce expects a FunctionType");

    // TODO: once we have bounded quantification, we need to be able to reduce the generic bounds.
    auto [copiedTy, copied] = copy(ty, f);
    copied->argTypes = reduce(f->argTypes);
    copied->retTypes = reduce(f->retTypes);
    return copiedTy;
}

TypeId TypeReducer::negationType(TypeId ty)
{
    RecursionGuard rg = guard(ty);

    if (auto nn = get<NegationType>(ty))
        return nn->ty; // ~~T ~ T
    else if (get<NeverType>(ty))
        return builtinTypes->unknownType; // ~never ~ unknown
    else if (get<UnknownType>(ty))
        return builtinTypes->neverType; // ~unknown ~ never
    else if (get<AnyType>(ty))
        return builtinTypes->anyType; // ~any ~ any
    else if (auto ni = get<IntersectionType>(ty))
    {
        std::vector<TypeId> options;
        for (TypeId part : ni)
            options.push_back(negationType(part));
        return foldl<UnionType>(begin(options), end(options), &TypeReducer::unionType); // ~(T & U) ~ (~T | ~U)
    }
    else if (auto nu = get<UnionType>(ty))
    {
        std::vector<TypeId> parts;
        for (TypeId option : nu)
            parts.push_back(negationType(option));
        return foldl<IntersectionType>(begin(parts), end(parts), &TypeReducer::intersectionType); // ~(T | U) ~ (~T & ~U)
    }
    else
        return arena->addType(NegationType{ty}); // for all T except the ones handled above, ~T ~ ~T
}

RecursionGuard TypeReducer::guard(TypeId ty)
{
    seen.push_back(ty);
    return RecursionGuard{&depth, FInt::LuauTypeReductionRecursionLimit, &seen};
}

RecursionGuard TypeReducer::guard(TypePackId tp)
{
    seen.push_back(tp);
    return RecursionGuard{&depth, FInt::LuauTypeReductionRecursionLimit, &seen};
}

} // namespace

TypeReduction::TypeReduction(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<InternalErrorReporter> handle)
    : arena(arena)
    , builtinTypes(builtinTypes)
    , handle(handle)
{
}

std::optional<TypeId> TypeReduction::reduce(TypeId ty)
{
    if (auto found = cachedTypes.find(ty))
        return *found;

    if (auto reduced = reduceImpl(ty))
    {
        cachedTypes[ty] = *reduced;
        return *reduced;
    }

    return std::nullopt;
}

std::optional<TypePackId> TypeReduction::reduce(TypePackId tp)
{
    if (auto found = cachedTypePacks.find(tp))
        return *found;

    if (auto reduced = reduceImpl(tp))
    {
        cachedTypePacks[tp] = *reduced;
        return *reduced;
    }

    return std::nullopt;
}

std::optional<TypeId> TypeReduction::reduceImpl(TypeId ty)
{
    if (FFlag::DebugLuauDontReduceTypes)
        return ty;

    if (hasExceededCartesianProductLimit(ty))
        return std::nullopt;

    try
    {
        TypeReducer reducer{arena, builtinTypes, handle};
        return reducer.reduce(ty);
    }
    catch (const RecursionLimitException&)
    {
        return std::nullopt;
    }
}

std::optional<TypePackId> TypeReduction::reduceImpl(TypePackId tp)
{
    if (FFlag::DebugLuauDontReduceTypes)
        return tp;

    if (hasExceededCartesianProductLimit(tp))
        return std::nullopt;

    try
    {
        TypeReducer reducer{arena, builtinTypes, handle};
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
