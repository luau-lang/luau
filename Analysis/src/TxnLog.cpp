// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TxnLog.h"

#include "Luau/ToString.h"
#include "Luau/TypePack.h"

#include <algorithm>
#include <stdexcept>

LUAU_FASTFLAGVARIABLE(LuauTxnLogPreserveOwner, false)
LUAU_FASTFLAGVARIABLE(LuauJustOneCallFrameForHaveSeen, false)

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
        typeVarChanges[ty] = std::move(rep);

    for (auto& [tp, rep] : rhs.typePackChanges)
        typePackChanges[tp] = std::move(rep);
}

void TxnLog::commit()
{
    if (FFlag::LuauTxnLogPreserveOwner)
    {
        for (auto& [ty, rep] : typeVarChanges)
        {
            TypeArena* owningArena = ty->owningArena;
            TypeVar* mtv = asMutable(ty);
            *mtv = rep.get()->pending;
            mtv->owningArena = owningArena;
        }

        for (auto& [tp, rep] : typePackChanges)
        {
            TypeArena* owningArena = tp->owningArena;
            TypePackVar* mpv = asMutable(tp);
            *mpv = rep.get()->pending;
            mpv->owningArena = owningArena;
        }
    }
    else
    {
        for (auto& [ty, rep] : typeVarChanges)
            *asMutable(ty) = rep.get()->pending;

        for (auto& [tp, rep] : typePackChanges)
            *asMutable(tp) = rep.get()->pending;
    }

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
        inversed.typeVarChanges[ty] = std::make_unique<PendingType>(*ty);

    for (auto& [tp, _rep] : typePackChanges)
        inversed.typePackChanges[tp] = std::make_unique<PendingTypePack>(*tp);

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
    if (FFlag::LuauJustOneCallFrameForHaveSeen && !FFlag::LuauTypecheckOptPass)
    {
        // This function will technically work if `this` is nullptr, but this
        // indicates a bug, so we explicitly assert.
        LUAU_ASSERT(static_cast<const void*>(this) != nullptr);

        const std::pair<TypeOrPackId, TypeOrPackId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);

        for (const TxnLog* current = this; current; current = current->parent)
        {
            if (current->sharedSeen->end() != std::find(current->sharedSeen->begin(), current->sharedSeen->end(), sortedPair))
                return true;
        }

        return false;
    }
    else
    {
        const std::pair<TypeOrPackId, TypeOrPackId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
        if (sharedSeen->end() != std::find(sharedSeen->begin(), sharedSeen->end(), sortedPair))
        {
            return true;
        }

        if (!FFlag::LuauTypecheckOptPass && parent)
        {
            return parent->haveSeen(lhs, rhs);
        }

        return false;
    }
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
    LUAU_ASSERT(!ty->persistent);

    // Explicitly don't look in ancestors. If we have discovered something new
    // about this type, we don't want to mutate the parent's state.
    auto& pending = typeVarChanges[ty];
    if (!pending)
        pending = std::make_unique<PendingType>(*ty);

    return pending.get();
}

PendingTypePack* TxnLog::queue(TypePackId tp)
{
    LUAU_ASSERT(!tp->persistent);

    // Explicitly don't look in ancestors. If we have discovered something new
    // about this type, we don't want to mutate the parent's state.
    auto& pending = typePackChanges[tp];
    if (!pending)
        pending = std::make_unique<PendingTypePack>(*tp);

    return pending.get();
}

PendingType* TxnLog::pending(TypeId ty) const
{
    // This function will technically work if `this` is nullptr, but this
    // indicates a bug, so we explicitly assert.
    LUAU_ASSERT(static_cast<const void*>(this) != nullptr);

    for (const TxnLog* current = this; current; current = current->parent)
    {
        if (auto it = current->typeVarChanges.find(ty))
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

PendingType* TxnLog::replace(TypeId ty, TypeVar replacement)
{
    PendingType* newTy = queue(ty);
    newTy->pending = replacement;
    return newTy;
}

PendingTypePack* TxnLog::replace(TypePackId tp, TypePackVar replacement)
{
    PendingTypePack* newTp = queue(tp);
    newTp->pending = replacement;
    return newTp;
}

PendingType* TxnLog::bindTable(TypeId ty, std::optional<TypeId> newBoundTo)
{
    LUAU_ASSERT(get<TableTypeVar>(ty));

    PendingType* newTy = queue(ty);
    if (TableTypeVar* ttv = Luau::getMutable<TableTypeVar>(newTy))
        ttv->boundTo = newBoundTo;

    return newTy;
}

PendingType* TxnLog::changeLevel(TypeId ty, TypeLevel newLevel)
{
    LUAU_ASSERT(get<FreeTypeVar>(ty) || get<TableTypeVar>(ty) || get<FunctionTypeVar>(ty));

    PendingType* newTy = queue(ty);
    if (FreeTypeVar* ftv = Luau::getMutable<FreeTypeVar>(newTy))
    {
        ftv->level = newLevel;
    }
    else if (TableTypeVar* ttv = Luau::getMutable<TableTypeVar>(newTy))
    {
        LUAU_ASSERT(ttv->state == TableState::Free || ttv->state == TableState::Generic);
        ttv->level = newLevel;
    }
    else if (FunctionTypeVar* ftv = Luau::getMutable<FunctionTypeVar>(newTy))
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
    LUAU_ASSERT(get<TableTypeVar>(ty));

    PendingType* newTy = queue(ty);
    if (TableTypeVar* ttv = Luau::getMutable<TableTypeVar>(newTy))
    {
        ttv->indexer = indexer;
    }

    return newTy;
}

std::optional<TypeLevel> TxnLog::getLevel(TypeId ty) const
{
    if (FreeTypeVar* ftv = getMutable<FreeTypeVar>(ty))
        return ftv->level;
    else if (TableTypeVar* ttv = getMutable<TableTypeVar>(ty); ttv && (ttv->state == TableState::Free || ttv->state == TableState::Generic))
        return ttv->level;
    else if (FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(ty))
        return ftv->level;

    return std::nullopt;
}

TypeId TxnLog::follow(TypeId ty) const
{
    return Luau::follow(ty, [this](TypeId ty) {
        PendingType* state = this->pending(ty);

        if (state == nullptr)
            return ty;

        // Ugly: Fabricate a TypeId that doesn't adhere to most of the invariants
        // that normally apply. This is safe because follow will only call get<>
        // on the returned pointer.
        return const_cast<const TypeVar*>(&state->pending);
    });
}

TypePackId TxnLog::follow(TypePackId tp) const
{
    return Luau::follow(tp, [this](TypePackId tp) {
        PendingTypePack* state = this->pending(tp);

        if (state == nullptr)
            return tp;

        // Ugly: Fabricate a TypePackId that doesn't adhere to most of the
        // invariants that normally apply. This is safe because follow will
        // only call get<> on the returned pointer.
        return const_cast<const TypePackVar*>(&state->pending);
    });
}

} // namespace Luau
