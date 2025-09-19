// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypePack.h"

#include "Luau/Error.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeArena.h"

LUAU_FASTFLAG(LuauReturnMappedGenericPacksFromSubtyping3)
LUAU_FASTFLAG(LuauSubtypingGenericPacksDoesntUseVariance)

namespace Luau
{

FreeTypePack::FreeTypePack(TypeLevel level)
    : index(Unifiable::freshIndex())
    , level(level)
    , scope(nullptr)
{
}

FreeTypePack::FreeTypePack(Scope* scope, Polarity polarity)
    : index(Unifiable::freshIndex())
    , level{}
    , scope(scope)
    , polarity(polarity)
{
}

FreeTypePack::FreeTypePack(Scope* scope, TypeLevel level)
    : index(Unifiable::freshIndex())
    , level(level)
    , scope(scope)
{
}

GenericTypePack::GenericTypePack()
    : index(Unifiable::freshIndex())
    , name("g" + std::to_string(index))
{
}

GenericTypePack::GenericTypePack(TypeLevel level)
    : index(Unifiable::freshIndex())
    , level(level)
    , name("g" + std::to_string(index))
{
}

GenericTypePack::GenericTypePack(const Name& name)
    : index(Unifiable::freshIndex())
    , name(name)
    , explicitName(true)
{
}

GenericTypePack::GenericTypePack(Scope* scope, Polarity polarity)
    : index(Unifiable::freshIndex())
    , scope(scope)
    , polarity(polarity)
{
}

GenericTypePack::GenericTypePack(TypeLevel level, const Name& name)
    : index(Unifiable::freshIndex())
    , level(level)
    , name(name)
    , explicitName(true)
{
}

GenericTypePack::GenericTypePack(Scope* scope, const Name& name)
    : index(Unifiable::freshIndex())
    , scope(scope)
    , name(name)
    , explicitName(true)
{
}

BlockedTypePack::BlockedTypePack()
    : index(++nextIndex)
{
}

size_t BlockedTypePack::nextIndex = 0;

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

TypePackVar& TypePackVar::operator=(const TypePackVar& rhs)
{
    LUAU_ASSERT(owningArena == rhs.owningArena);
    LUAU_ASSERT(!rhs.persistent);

    reassign(rhs);

    return *this;
}

TypePackIterator::TypePackIterator(TypePackId typePack)
    : TypePackIterator(typePack, TxnLog::empty())
{
}

TypePackIterator::TypePackIterator(TypePackId typePack, const TxnLog* log)
    : currentTypePack(log->follow(typePack))
    , tp(log->get<TypePack>(currentTypePack))
    , currentIndex(0)
    , log(log)
{
    while (tp && tp->head.empty())
    {
        currentTypePack = tp->tail ? log->follow(*tp->tail) : nullptr;
        tp = currentTypePack ? log->getMutable<TypePack>(currentTypePack) : nullptr;
    }
}

TypePackIterator& TypePackIterator::operator++()
{
    LUAU_ASSERT(tp);

    ++currentIndex;
    while (tp && currentIndex >= tp->head.size())
    {
        currentTypePack = tp->tail ? log->follow(*tp->tail) : nullptr;
        tp = currentTypePack ? log->getMutable<TypePack>(currentTypePack) : nullptr;

        if (tp)
        {
            // Step twice on each iteration to detect cycles
            tailCycleCheck = tp->tail ? log->follow(*tp->tail) : nullptr;

            if (currentTypePack == tailCycleCheck)
                throw InternalCompilerError("TypePackIterator detected a type pack cycle");
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

TypePackIterator begin(TypePackId tp, const TxnLog* log)
{
    return TypePackIterator{tp, log};
}

TypePackIterator end(TypePackId tp)
{
    return TypePackIterator{};
}

TypePackId getTail(TypePackId tp)
{
    DenseHashSet<TypePackId> seen{nullptr};
    while (tp)
    {
        tp = follow(tp);

        if (seen.contains(tp))
            break;
        seen.insert(tp);

        if (auto pack = get<TypePack>(tp); pack && pack->tail)
            tp = *pack->tail;
        else
            break;
    }

    return follow(tp);
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
        const FreeTypePack* lf = get_if<FreeTypePack>(&lhsTail->ty);
        const FreeTypePack* rf = get_if<FreeTypePack>(&rhsTail->ty);
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
        const GenericTypePack* lg = get_if<GenericTypePack>(&lhsTail->ty);
        const GenericTypePack* rg = get_if<GenericTypePack>(&rhsTail->ty);
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
    return follow(
        tp,
        nullptr,
        [](const void*, TypePackId t)
        {
            return t;
        }
    );
}

TypePackId follow(TypePackId tp, const void* context, TypePackId (*mapper)(const void*, TypePackId))
{
    auto advance = [context, mapper](TypePackId ty) -> std::optional<TypePackId>
    {
        TypePackId mapped = mapper(context, ty);

        if (const Unifiable::Bound<TypePackId>* btv = get<Unifiable::Bound<TypePackId>>(mapped))
            return btv->boundTo;
        else if (const TypePack* tp = get<TypePack>(mapped); tp && tp->head.empty())
            return tp->tail;
        else
            return std::nullopt;
    };

    TypePackId cycleTester = tp; // Null once we've determined that there is no cycle
    if (auto a = advance(cycleTester))
        cycleTester = *a;
    else
        return tp;

    if (!advance(cycleTester)) // Short circuit traversal for the rather common case when advance(advance(t)) == null
        return cycleTester;

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
                throw InternalCompilerError("Luau::follow detected a Type cycle!!");
        }
    }
}

size_t size(TypePackId tp, TxnLog* log)
{
    tp = log ? log->follow(tp) : follow(tp);
    if (auto pack = get<TypePack>(tp))
        return size(*pack, log);
    else
        return 0;
}

bool finite(TypePackId tp, TxnLog* log)
{
    tp = log ? log->follow(tp) : follow(tp);

    if (auto pack = get<TypePack>(tp))
        return pack->tail ? finite(*pack->tail, log) : true;

    if (get<VariadicTypePack>(tp))
        return false;

    return true;
}

size_t size(const TypePack& tp, TxnLog* log)
{
    size_t result = tp.head.size();
    if (tp.tail)
    {
        const TypePack* tail = get<TypePack>(log ? log->follow(*tp.tail) : follow(*tp.tail));
        if (tail)
            result += size(*tail, log);
    }
    return result;
}

std::optional<TypeId> first(TypePackId tp, bool ignoreHiddenVariadics)
{
    auto it = begin(tp);
    auto endIter = end(tp);

    if (it != endIter)
        return *it;

    if (auto tail = it.tail())
    {
        if (auto vtp = get<VariadicTypePack>(*tail); vtp && (!vtp->hidden || !ignoreHiddenVariadics))
            return vtp->ty;
    }

    return std::nullopt;
}

TypePackVar* asMutable(TypePackId tp)
{
    return const_cast<TypePackVar*>(tp);
}

TypePack* asMutable(const TypePack* tp)
{
    return const_cast<TypePack*>(tp);
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

std::pair<std::vector<TypeId>, std::optional<TypePackId>> flatten(TypePackId tp, const TxnLog& log)
{
    tp = log.follow(tp);

    std::vector<TypeId> flattened;
    std::optional<TypePackId> tail = std::nullopt;

    TypePackIterator it(tp, &log);

    for (; it != end(tp); ++it)
    {
        flattened.push_back(*it);
    }

    tail = it.tail();

    return {flattened, tail};
}

std::pair<std::vector<TypeId>, std::optional<TypePackId>> flatten_DEPRECATED(
    TypePackId tp,
    const DenseHashMap<TypePackId, TypePackId>& mappedGenericPacks
)
{
    LUAU_ASSERT(FFlag::LuauReturnMappedGenericPacksFromSubtyping3);
    LUAU_ASSERT(!FFlag::LuauSubtypingGenericPacksDoesntUseVariance);

    tp = mappedGenericPacks.contains(tp) ? *mappedGenericPacks.find(tp) : tp;

    std::vector<TypeId> flattened;
    std::optional<TypePackId> tail = std::nullopt;
    DenseHashSet<TypePackId> seenGenericPacks{nullptr};

    while (tp)
    {
        TypePackIterator it(tp);

        for (; it != end(tp); ++it)
            flattened.push_back(*it);

        if (const auto tpTail = it.tail(); tpTail && !seenGenericPacks.contains(*tpTail) && mappedGenericPacks.contains(*tpTail))
        {
            tp = *mappedGenericPacks.find(*tpTail);
            seenGenericPacks.insert(*tpTail);
            continue;
        }

        tail = it.tail();
        break;
    }

    return {flattened, tail};
}

bool isVariadic(TypePackId tp)
{
    return isVariadic(tp, *TxnLog::empty());
}

bool isVariadic(TypePackId tp, const TxnLog& log)
{
    std::optional<TypePackId> tail = flatten(tp, log).second;

    if (!tail)
        return false;

    return isVariadicTail(*tail, log);
}

bool isVariadicTail(TypePackId tp, const TxnLog& log, bool includeHiddenVariadics)
{
    if (log.get<GenericTypePack>(tp))
        return true;

    if (auto vtp = log.get<VariadicTypePack>(tp); vtp && (includeHiddenVariadics || !vtp->hidden))
        return true;

    return false;
}

bool containsNever(TypePackId tp)
{
    auto it = begin(tp);
    auto endIt = end(tp);

    while (it != endIt)
    {
        if (get<NeverType>(follow(*it)))
            return true;
        ++it;
    }

    if (auto tail = it.tail())
    {
        if (auto vtp = get<VariadicTypePack>(*tail); vtp && get<NeverType>(follow(vtp->ty)))
            return true;
    }

    return false;
}

template<>
LUAU_NOINLINE Unifiable::Bound<TypePackId>* emplaceTypePack<BoundTypePack>(TypePackVar* ty, TypePackId& tyArg)
{
    LUAU_ASSERT(ty != follow(tyArg));
    return &ty->ty.emplace<BoundTypePack>(tyArg);
}

TypePackId sliceTypePack(
    const size_t sliceIndex,
    const TypePackId toBeSliced,
    const std::vector<TypeId>& head,
    const std::optional<TypePackId> tail,
    const NotNull<BuiltinTypes> builtinTypes,
    const NotNull<TypeArena> arena
)
{
    LUAU_ASSERT(FFlag::LuauSubtypingGenericPacksDoesntUseVariance);

    if (sliceIndex == 0)
        return toBeSliced;
    else if (sliceIndex == head.size())
        return tail.value_or(builtinTypes->emptyTypePack);
    else
    {
        auto superHeadIter = begin(head);
        for (size_t i = 0; i < sliceIndex; ++i)
            ++superHeadIter;
        std::vector<TypeId> headSlice(std::move(superHeadIter), end(head));
        return arena->addTypePack(std::move(headSlice), tail);
    }
}

} // namespace Luau
