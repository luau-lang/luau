// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TxnLog.h"

#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/TypeArena.h"
#include "Luau/TypePack.h"

#include <algorithm>
#include <stdexcept>

LUAU_FASTFLAGVARIABLE(LuauOccursCheckInCommit)

namespace Luau
{

const std::string nullPendingResult = "<nullptr>";

std::string toString(PendingType* pending)
{
    if (pending == nullptr)
        return nullPendingResult;

    return toString(pending->pending);
}

std::string dump(PendingType* pending)
{
    if (pending == nullptr)
    {
        printf("%s\n", nullPendingResult.c_str());
        return nullPendingResult;
    }

    ToStringOptions opts;
    opts.exhaustive = true;
    opts.functionTypeArguments = true;
    std::string result = toString(pending->pending, opts);
    printf("%s\n", result.c_str());
    return result;
}

std::string toString(PendingTypePack* pending)
{
    if (pending == nullptr)
        return nullPendingResult;

    return toString(pending->pending);
}

std::string dump(PendingTypePack* pending)
{
    if (pending == nullptr)
    {
        printf("%s\n", nullPendingResult.c_str());
        return nullPendingResult;
    }

    ToStringOptions opts;
    opts.exhaustive = true;
    opts.functionTypeArguments = true;
    std::string result = toString(pending->pending, opts);
    printf("%s\n", result.c_str());
    return result;
}

static const TxnLog emptyLog;

const TxnLog* TxnLog::empty()
{
    return &emptyLog;
}

void TxnLog::concat(TxnLog rhs)
{
    for (auto& [ty, rep] : rhs.typeVarChanges)
    {
        if (rep->dead)
            continue;
        typeVarChanges[ty] = std::move(rep);
    }

    for (auto& [tp, rep] : rhs.typePackChanges)
        typePackChanges[tp] = std::move(rep);

    radioactive |= rhs.radioactive;
}

void TxnLog::concatAsUnion(TxnLog rhs, NotNull<TypeArena> arena)
{
    /*
     * Check for cycles.
     *
     * We must not combine a log entry that binds 'a to 'b with a log that
     * binds 'b to 'a.
     *
     * Of the two, identify the one with the 'bigger' scope and eliminate the
     * entry that rebinds it.
     */
    for (const auto& [rightTy, rightRep] : rhs.typeVarChanges)
    {
        if (rightRep->dead)
            continue;

        // We explicitly use get_if here because we do not wish to do anything
        // if the uncommitted type is already bound to something else.
        const FreeType* rf = get_if<FreeType>(&rightTy->ty);
        if (!rf)
            continue;

        const BoundType* rb = Luau::get<BoundType>(&rightRep->pending);
        if (!rb)
            continue;

        const TypeId leftTy = rb->boundTo;
        const FreeType* lf = get_if<FreeType>(&leftTy->ty);
        if (!lf)
            continue;

        auto leftRep = typeVarChanges.find(leftTy);
        if (!leftRep)
            continue;

        if ((*leftRep)->dead)
            continue;

        const BoundType* lb = Luau::get<BoundType>(&(*leftRep)->pending);
        if (!lb)
            continue;

        if (lb->boundTo == rightTy)
        {
            // leftTy has been bound to rightTy, but rightTy has also been bound
            // to leftTy. We find the one that belongs to the more deeply nested
            // scope and remove it from the log.
            const bool discardLeft = lf->level.subsumes(rf->level);

            if (discardLeft)
                (*leftRep)->dead = true;
            else
                rightRep->dead = true;
        }
    }

    for (auto& [ty, rightRep] : rhs.typeVarChanges)
    {
        if (rightRep->dead)
            continue;

        if (auto leftRep = typeVarChanges.find(ty); leftRep && !(*leftRep)->dead)
        {
            TypeId leftTy = arena->addType((*leftRep)->pending.clone());
            TypeId rightTy = arena->addType(rightRep->pending.clone());

            if (follow(leftTy) == follow(rightTy))
                typeVarChanges[ty] = std::move(rightRep);
            else
                typeVarChanges[ty]->pending.ty = UnionType{{leftTy, rightTy}};
        }
        else
            typeVarChanges[ty] = std::move(rightRep);
    }

    for (auto& [tp, rep] : rhs.typePackChanges)
        typePackChanges[tp] = std::move(rep);

    radioactive |= rhs.radioactive;
}

// Like follow(), but only takes a single step.
//
// This is potentailly performance sensitive, so we use nullptr rather than an
// optional<TypeId> for the return type here.
static TypeId followOnce(TxnLog& log, TypeId ty)
{
    if (auto bound = log.get<BoundType>(ty))
        return bound->boundTo;
    if (auto tt = log.get<TableType>(ty))
        return tt->boundTo.value_or(nullptr);

    return nullptr;
}

// We must take extra care not to replace a type with a BoundType to itself. We
// check each BoundType along the chain
//
// This function returns true if any of the bound types pointed at by 'needle'
// point at 'haystack'.
static bool occurs(TxnLog& log, TypeId needle, TypeId haystack)
{
    TypeId tortoise = needle;
    TypeId hare = needle;

    while (true)
    {
        if (tortoise == haystack)
            return true;

        TypeId g = followOnce(log, tortoise);
        if (!g)
            return false;
        tortoise = g;

        // Cycle detection: The hare steps twice for each step that the tortoise takes.
        // If ever the two meet, it can only be because the track is cyclic.
        // When we hit the end of the chain, hare becomes nullptr.
        if (hare)
        {
            hare = followOnce(log, hare);
            if (hare)
            {
                hare = followOnce(log, hare);

                if (hare == tortoise)
                    return true;
            }
        }
    }
}

void TxnLog::commit()
{
    LUAU_ASSERT(!radioactive);

    for (auto& [ty, rep] : typeVarChanges)
    {
        if (!rep->dead)
        {
            const TypeId unfollowed = &rep.get()->pending;

            if (FFlag::LuauOccursCheckInCommit)
            {
                if (!occurs(*this, unfollowed, ty))
                    asMutable(ty)->reassign(*unfollowed);
            }
            else
                asMutable(ty)->reassign(*unfollowed);
        }
    }

    for (auto& [tp, rep] : typePackChanges)
        asMutable(tp)->reassign(rep.get()->pending);

    clear();
}

void TxnLog::clear()
{
    typeVarChanges.clear();
    typePackChanges.clear();
}

TxnLog TxnLog::inverse()
{
    TxnLog inversed(sharedSeen);

    for (auto& [ty, _rep] : typeVarChanges)
    {
        if (!_rep->dead)
            inversed.typeVarChanges[ty] = std::make_unique<PendingType>(ty->clone());
    }

    for (auto& [tp, _rep] : typePackChanges)
        inversed.typePackChanges[tp] = std::make_unique<PendingTypePack>(*tp);

    inversed.radioactive = radioactive;

    return inversed;
}

bool TxnLog::haveSeen(TypeId lhs, TypeId rhs) const
{
    return haveSeen((TypeOrPackId)lhs, (TypeOrPackId)rhs);
}

void TxnLog::pushSeen(TypeId lhs, TypeId rhs)
{
    pushSeen((TypeOrPackId)lhs, (TypeOrPackId)rhs);
}

void TxnLog::popSeen(TypeId lhs, TypeId rhs)
{
    popSeen((TypeOrPackId)lhs, (TypeOrPackId)rhs);
}

bool TxnLog::haveSeen(TypePackId lhs, TypePackId rhs) const
{
    return haveSeen((TypeOrPackId)lhs, (TypeOrPackId)rhs);
}

void TxnLog::pushSeen(TypePackId lhs, TypePackId rhs)
{
    pushSeen((TypeOrPackId)lhs, (TypeOrPackId)rhs);
}

void TxnLog::popSeen(TypePackId lhs, TypePackId rhs)
{
    popSeen((TypeOrPackId)lhs, (TypeOrPackId)rhs);
}

bool TxnLog::haveSeen(TypeOrPackId lhs, TypeOrPackId rhs) const
{
    const std::pair<TypeOrPackId, TypeOrPackId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    if (sharedSeen->end() != std::find(sharedSeen->begin(), sharedSeen->end(), sortedPair))
    {
        return true;
    }

    return false;
}

void TxnLog::pushSeen(TypeOrPackId lhs, TypeOrPackId rhs)
{
    const std::pair<TypeOrPackId, TypeOrPackId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    sharedSeen->push_back(sortedPair);
}

void TxnLog::popSeen(TypeOrPackId lhs, TypeOrPackId rhs)
{
    const std::pair<TypeOrPackId, TypeOrPackId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    LUAU_ASSERT(sortedPair == sharedSeen->back());
    sharedSeen->pop_back();
}

PendingType* TxnLog::queue(TypeId ty)
{
    if (ty->persistent)
        radioactive = true;

    // Explicitly don't look in ancestors. If we have discovered something new
    // about this type, we don't want to mutate the parent's state.
    auto& pending = typeVarChanges[ty];
    if (!pending || (*pending).dead)
    {
        pending = std::make_unique<PendingType>(ty->clone());
        pending->pending.owningArena = nullptr;
    }

    return pending.get();
}

PendingTypePack* TxnLog::queue(TypePackId tp)
{
    if (tp->persistent)
        radioactive = true;

    // Explicitly don't look in ancestors. If we have discovered something new
    // about this type, we don't want to mutate the parent's state.
    auto& pending = typePackChanges[tp];
    if (!pending)
    {
        pending = std::make_unique<PendingTypePack>(*tp);
        pending->pending.owningArena = nullptr;
    }

    return pending.get();
}

PendingType* TxnLog::pending(TypeId ty) const
{
    // This function will technically work if `this` is nullptr, but this
    // indicates a bug, so we explicitly assert.
    LUAU_ASSERT(static_cast<const void*>(this) != nullptr);

    for (const TxnLog* current = this; current; current = current->parent)
    {
        if (auto it = current->typeVarChanges.find(ty); it && !(*it)->dead)
            return it->get();
    }

    return nullptr;
}

PendingTypePack* TxnLog::pending(TypePackId tp) const
{
    // This function will technically work if `this` is nullptr, but this
    // indicates a bug, so we explicitly assert.
    LUAU_ASSERT(static_cast<const void*>(this) != nullptr);

    for (const TxnLog* current = this; current; current = current->parent)
    {
        if (auto it = current->typePackChanges.find(tp))
            return it->get();
    }

    return nullptr;
}

PendingType* TxnLog::replace(TypeId ty, Type replacement)
{
    PendingType* newTy = queue(ty);
    newTy->pending.reassign(replacement);
    return newTy;
}

PendingTypePack* TxnLog::replace(TypePackId tp, TypePackVar replacement)
{
    PendingTypePack* newTp = queue(tp);
    newTp->pending.reassign(replacement);
    return newTp;
}

PendingType* TxnLog::bindTable(TypeId ty, std::optional<TypeId> newBoundTo)
{
    LUAU_ASSERT(get<TableType>(ty));
    LUAU_ASSERT(ty != newBoundTo);

    PendingType* newTy = queue(ty);
    if (TableType* ttv = Luau::getMutable<TableType>(newTy))
        ttv->boundTo = newBoundTo;

    return newTy;
}

PendingType* TxnLog::changeLevel(TypeId ty, TypeLevel newLevel)
{
    LUAU_ASSERT(get<FreeType>(ty) || get<TableType>(ty) || get<FunctionType>(ty));

    PendingType* newTy = queue(ty);
    if (FreeType* ftv = Luau::getMutable<FreeType>(newTy))
    {
        ftv->level = newLevel;
    }
    else if (TableType* ttv = Luau::getMutable<TableType>(newTy))
    {
        LUAU_ASSERT(ttv->state == TableState::Free || ttv->state == TableState::Generic);
        ttv->level = newLevel;
    }
    else if (FunctionType* ftv = Luau::getMutable<FunctionType>(newTy))
    {
        ftv->level = newLevel;
    }

    return newTy;
}

PendingTypePack* TxnLog::changeLevel(TypePackId tp, TypeLevel newLevel)
{
    LUAU_ASSERT(get<FreeTypePack>(tp));

    PendingTypePack* newTp = queue(tp);
    if (FreeTypePack* ftp = Luau::getMutable<FreeTypePack>(newTp))
    {
        ftp->level = newLevel;
    }

    return newTp;
}

PendingType* TxnLog::changeIndexer(TypeId ty, std::optional<TableIndexer> indexer)
{
    LUAU_ASSERT(get<TableType>(ty));

    PendingType* newTy = queue(ty);
    if (TableType* ttv = Luau::getMutable<TableType>(newTy))
    {
        ttv->indexer = indexer;
    }

    return newTy;
}

std::optional<TypeLevel> TxnLog::getLevel(TypeId ty) const
{
    if (FreeType* ftv = getMutable<FreeType>(ty))
        return ftv->level;
    else if (TableType* ttv = getMutable<TableType>(ty); ttv && (ttv->state == TableState::Free || ttv->state == TableState::Generic))
        return ttv->level;
    else if (FunctionType* ftv = getMutable<FunctionType>(ty))
        return ftv->level;

    return std::nullopt;
}

TypeId TxnLog::follow(TypeId ty) const
{
    return Luau::follow(
        ty,
        this,
        [](const void* ctx, TypeId ty) -> TypeId
        {
            const TxnLog* self = static_cast<const TxnLog*>(ctx);
            PendingType* state = self->pending(ty);

            if (state == nullptr)
                return ty;

            // Ugly: Fabricate a TypeId that doesn't adhere to most of the invariants
            // that normally apply. This is safe because follow will only call get<>
            // on the returned pointer.
            return const_cast<const Type*>(&state->pending);
        }
    );
}

TypePackId TxnLog::follow(TypePackId tp) const
{
    return Luau::follow(
        tp,
        this,
        [](const void* ctx, TypePackId tp) -> TypePackId
        {
            const TxnLog* self = static_cast<const TxnLog*>(ctx);
            PendingTypePack* state = self->pending(tp);

            if (state == nullptr)
                return tp;

            // Ugly: Fabricate a TypePackId that doesn't adhere to most of the
            // invariants that normally apply. This is safe because follow will
            // only call get<> on the returned pointer.
            return const_cast<const TypePackVar*>(&state->pending);
        }
    );
}

} // namespace Luau
