// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypePack.h"

#include "Luau/TxnLog.h"

#include <stdexcept>

LUAU_FASTFLAG(LuauUseCommittingTxnLog)

namespace Luau
{

TypePackVar::TypePackVar(const TypePackVariant& tp)
    : ty(tp)
{
}

TypePackVar::TypePackVar(TypePackVariant&& tp)
    : ty(std::move(tp))
{
}

TypePackVar::TypePackVar(TypePackVariant&& tp, bool persistent)
    : ty(std::move(tp))
    , persistent(persistent)
{
}

bool TypePackVar::operator==(const TypePackVar& rhs) const
{
    SeenSet seen;
    return areEqual(seen, *this, rhs);
}

TypePackVar& TypePackVar::operator=(TypePackVariant&& tp)
{
    ty = std::move(tp);
    return *this;
}

TypePackIterator::TypePackIterator(TypePackId typePack)
    : TypePackIterator(typePack, TxnLog::empty())
{
}

TypePackIterator::TypePackIterator(TypePackId typePack, const TxnLog* log)
    : currentTypePack(follow(typePack))
    , tp(get<TypePack>(currentTypePack))
    , currentIndex(0)
    , log(log)
{
    while (tp && tp->head.empty())
    {
        if (FFlag::LuauUseCommittingTxnLog)
        {
            currentTypePack = tp->tail ? log->follow(*tp->tail) : nullptr;
            tp = currentTypePack ? log->getMutable<TypePack>(currentTypePack) : nullptr;
        }
        else
        {
            currentTypePack = tp->tail ? follow(*tp->tail) : nullptr;
            tp = currentTypePack ? get<TypePack>(currentTypePack) : nullptr;
        }
    }
}

TypePackIterator& TypePackIterator::operator++()
{
    LUAU_ASSERT(tp);

    ++currentIndex;
    while (tp && currentIndex >= tp->head.size())
    {
        if (FFlag::LuauUseCommittingTxnLog)
        {
            currentTypePack = tp->tail ? log->follow(*tp->tail) : nullptr;
            tp = currentTypePack ? log->getMutable<TypePack>(currentTypePack) : nullptr;
        }
        else
        {
            currentTypePack = tp->tail ? follow(*tp->tail) : nullptr;
            tp = currentTypePack ? get<TypePack>(currentTypePack) : nullptr;
        }

        currentIndex = 0;
    }

    return *this;
}

TypePackIterator TypePackIterator::operator++(int)
{
    TypePackIterator copy = *this;
    ++*this;
    return copy;
}

bool TypePackIterator::operator!=(const TypePackIterator& rhs)
{
    return !(*this == rhs);
}

bool TypePackIterator::operator==(const TypePackIterator& rhs)
{
    return tp == rhs.tp && currentIndex == rhs.currentIndex;
}

const TypeId& TypePackIterator::operator*()
{
    LUAU_ASSERT(tp);
    return tp->head[currentIndex];
}

std::optional<TypePackId> TypePackIterator::tail()
{
    LUAU_ASSERT(!tp);
    return currentTypePack ? std::optional<TypePackId>{currentTypePack} : std::nullopt;
}

TypePackIterator begin(TypePackId tp)
{
    return TypePackIterator{tp};
}

TypePackIterator begin(TypePackId tp, TxnLog* log)
{
    return TypePackIterator{tp, log};
}

TypePackIterator end(TypePackId tp)
{
    return TypePackIterator{};
}

bool areEqual(SeenSet& seen, const TypePackVar& lhs, const TypePackVar& rhs)
{
    TypePackId lhsId = const_cast<TypePackId>(&lhs);
    TypePackId rhsId = const_cast<TypePackId>(&rhs);
    TypePackIterator lhsIter = begin(lhsId);
    TypePackIterator rhsIter = begin(rhsId);
    TypePackIterator lhsEnd = end(lhsId);
    TypePackIterator rhsEnd = end(rhsId);
    while (lhsIter != lhsEnd && rhsIter != rhsEnd)
    {
        if (!areEqual(seen, **lhsIter, **rhsIter))
            return false;
        ++lhsIter;
        ++rhsIter;
    }

    if (lhsIter != lhsEnd || rhsIter != rhsEnd)
        return false;

    if (!lhsIter.tail() && !rhsIter.tail())
        return true;
    if (!lhsIter.tail() || !rhsIter.tail())
        return false;

    TypePackId lhsTail = *lhsIter.tail();
    TypePackId rhsTail = *rhsIter.tail();

    {
        const Unifiable::Free* lf = get_if<Unifiable::Free>(&lhsTail->ty);
        const Unifiable::Free* rf = get_if<Unifiable::Free>(&rhsTail->ty);
        if (lf && rf)
            return lf->index == rf->index;
    }

    {
        const Unifiable::Bound<TypePackId>* lb = get_if<Unifiable::Bound<TypePackId>>(&lhsTail->ty);
        const Unifiable::Bound<TypePackId>* rb = get_if<Unifiable::Bound<TypePackId>>(&rhsTail->ty);
        if (lb && rb)
            return areEqual(seen, *lb->boundTo, *rb->boundTo);
    }

    {
        const Unifiable::Generic* lg = get_if<Unifiable::Generic>(&lhsTail->ty);
        const Unifiable::Generic* rg = get_if<Unifiable::Generic>(&rhsTail->ty);
        if (lg && rg)
            return lg->index == rg->index;
    }

    {
        const VariadicTypePack* lv = get_if<VariadicTypePack>(&lhsTail->ty);
        const VariadicTypePack* rv = get_if<VariadicTypePack>(&rhsTail->ty);
        if (lv && rv)
            return areEqual(seen, *lv->ty, *rv->ty);
    }

    return false;
}

TypePackId follow(TypePackId tp)
{
    return follow(tp, [](TypePackId t) {
        return t;
    });
}

TypePackId follow(TypePackId tp, std::function<TypePackId(TypePackId)> mapper)
{
    auto advance = [&mapper](TypePackId ty) -> std::optional<TypePackId> {
        if (const Unifiable::Bound<TypePackId>* btv = get<Unifiable::Bound<TypePackId>>(mapper(ty)))
            return btv->boundTo;
        else
            return std::nullopt;
    };

    TypePackId cycleTester = tp; // Null once we've determined that there is no cycle
    if (auto a = advance(cycleTester))
        cycleTester = *a;
    else
        return tp;

    while (true)
    {
        auto a1 = advance(tp);
        if (a1)
            tp = *a1;
        else
            return tp;

        if (nullptr != cycleTester)
        {
            auto a2 = advance(cycleTester);
            if (a2)
            {
                auto a3 = advance(*a2);
                if (a3)
                    cycleTester = *a3;
                else
                    cycleTester = nullptr;
            }
            else
                cycleTester = nullptr;

            if (tp == cycleTester)
                throw std::runtime_error("Luau::follow detected a TypeVar cycle!!");
        }
    }
}

size_t size(TypePackId tp)
{
    if (auto pack = get<TypePack>(follow(tp)))
        return size(*pack);
    else
        return 0;
}

bool finite(TypePackId tp)
{
    tp = follow(tp);

    if (auto pack = get<TypePack>(tp))
        return pack->tail ? finite(*pack->tail) : true;

    if (get<VariadicTypePack>(tp))
        return false;

    return true;
}

size_t size(const TypePack& tp)
{
    size_t result = tp.head.size();
    if (tp.tail)
    {
        const TypePack* tail = get<TypePack>(follow(*tp.tail));
        if (tail)
            result += size(*tail);
    }
    return result;
}

std::optional<TypeId> first(TypePackId tp)
{
    auto it = begin(tp);
    auto endIter = end(tp);

    if (it != endIter)
        return *it;

    if (auto tail = it.tail())
    {
        if (auto vtp = get<VariadicTypePack>(*tail))
            return vtp->ty;
    }

    return std::nullopt;
}

bool isEmpty(TypePackId tp)
{
    tp = follow(tp);
    if (auto tpp = get<TypePack>(tp))
    {
        return tpp->head.empty() && (!tpp->tail || isEmpty(*tpp->tail));
    }

    return false;
}

std::pair<std::vector<TypeId>, std::optional<TypePackId>> flatten(TypePackId tp)
{
    std::vector<TypeId> res;

    auto iter = begin(tp);
    auto endIter = end(tp);
    while (iter != endIter)
    {
        res.push_back(*iter);
        ++iter;
    }

    return {res, iter.tail()};
}

TypePackVar* asMutable(TypePackId tp)
{
    return const_cast<TypePackVar*>(tp);
}

TypePack* asMutable(const TypePack* tp)
{
    return const_cast<TypePack*>(tp);
}
} // namespace Luau
