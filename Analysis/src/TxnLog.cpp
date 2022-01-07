// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TxnLog.h"

#include "Luau/TypePack.h"

#include <algorithm>
#include <stdexcept>

LUAU_FASTFLAGVARIABLE(LuauUseCommittingTxnLog, false)

namespace Luau
{

void DEPRECATED_TxnLog::operator()(TypeId a)
{
    LUAU_ASSERT(!FFlag::LuauUseCommittingTxnLog);
    typeVarChanges.emplace_back(a, *a);
}

void DEPRECATED_TxnLog::operator()(TypePackId a)
{
    LUAU_ASSERT(!FFlag::LuauUseCommittingTxnLog);
    typePackChanges.emplace_back(a, *a);
}

void DEPRECATED_TxnLog::operator()(TableTypeVar* a)
{
    LUAU_ASSERT(!FFlag::LuauUseCommittingTxnLog);
    tableChanges.emplace_back(a, a->boundTo);
}

void DEPRECATED_TxnLog::rollback()
{
    LUAU_ASSERT(!FFlag::LuauUseCommittingTxnLog);
    for (auto it = typeVarChanges.rbegin(); it != typeVarChanges.rend(); ++it)
        std::swap(*asMutable(it->first), it->second);

    for (auto it = typePackChanges.rbegin(); it != typePackChanges.rend(); ++it)
        std::swap(*asMutable(it->first), it->second);

    for (auto it = tableChanges.rbegin(); it != tableChanges.rend(); ++it)
        std::swap(it->first->boundTo, it->second);

    LUAU_ASSERT(originalSeenSize <= sharedSeen->size());
    sharedSeen->resize(originalSeenSize);
}

void DEPRECATED_TxnLog::concat(DEPRECATED_TxnLog rhs)
{
    LUAU_ASSERT(!FFlag::LuauUseCommittingTxnLog);
    typeVarChanges.insert(typeVarChanges.end(), rhs.typeVarChanges.begin(), rhs.typeVarChanges.end());
    rhs.typeVarChanges.clear();

    typePackChanges.insert(typePackChanges.end(), rhs.typePackChanges.begin(), rhs.typePackChanges.end());
    rhs.typePackChanges.clear();

    tableChanges.insert(tableChanges.end(), rhs.tableChanges.begin(), rhs.tableChanges.end());
    rhs.tableChanges.clear();
}

bool DEPRECATED_TxnLog::haveSeen(TypeId lhs, TypeId rhs)
{
    LUAU_ASSERT(!FFlag::LuauUseCommittingTxnLog);
    const std::pair<TypeId, TypeId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    return (sharedSeen->end() != std::find(sharedSeen->begin(), sharedSeen->end(), sortedPair));
}

void DEPRECATED_TxnLog::pushSeen(TypeId lhs, TypeId rhs)
{
    LUAU_ASSERT(!FFlag::LuauUseCommittingTxnLog);
    const std::pair<TypeId, TypeId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    sharedSeen->push_back(sortedPair);
}

void DEPRECATED_TxnLog::popSeen(TypeId lhs, TypeId rhs)
{
    LUAU_ASSERT(!FFlag::LuauUseCommittingTxnLog);
    const std::pair<TypeId, TypeId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    LUAU_ASSERT(sortedPair == sharedSeen->back());
    sharedSeen->pop_back();
}

static const TxnLog emptyLog;

const TxnLog* TxnLog::empty()
{
    return &emptyLog;
}

void TxnLog::concat(TxnLog rhs)
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

    for (auto& [ty, rep] : rhs.typeVarChanges)
        typeVarChanges[ty] = std::move(rep);

    for (auto& [tp, rep] : rhs.typePackChanges)
        typePackChanges[tp] = std::move(rep);
}

void TxnLog::commit()
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

    for (auto& [ty, rep] : typeVarChanges)
        *asMutable(ty) = rep.get()->pending;

    for (auto& [tp, rep] : typePackChanges)
        *asMutable(tp) = rep.get()->pending;

    clear();
}

void TxnLog::clear()
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

    typeVarChanges.clear();
    typePackChanges.clear();
}

TxnLog TxnLog::inverse()
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

    TxnLog inversed(sharedSeen);

    for (auto& [ty, _rep] : typeVarChanges)
        inversed.typeVarChanges[ty] = std::make_unique<PendingType>(*ty);

    for (auto& [tp, _rep] : typePackChanges)
        inversed.typePackChanges[tp] = std::make_unique<PendingTypePack>(*tp);

    return inversed;
}

bool TxnLog::haveSeen(TypeId lhs, TypeId rhs) const
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

    const std::pair<TypeId, TypeId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    if (sharedSeen->end() != std::find(sharedSeen->begin(), sharedSeen->end(), sortedPair))
    {
        return true;
    }

    if (parent)
    {
        return parent->haveSeen(lhs, rhs);
    }

    return false;
}

void TxnLog::pushSeen(TypeId lhs, TypeId rhs)
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

    const std::pair<TypeId, TypeId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    sharedSeen->push_back(sortedPair);
}

void TxnLog::popSeen(TypeId lhs, TypeId rhs)
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

    const std::pair<TypeId, TypeId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    LUAU_ASSERT(sortedPair == sharedSeen->back());
    sharedSeen->pop_back();
}

PendingType* TxnLog::queue(TypeId ty)
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);
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
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);
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
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

    for (const TxnLog* current = this; current; current = current->parent)
    {
        if (auto it = current->typeVarChanges.find(ty); it != current->typeVarChanges.end())
            return it->second.get();
    }

    return nullptr;
}

PendingTypePack* TxnLog::pending(TypePackId tp) const
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

    for (const TxnLog* current = this; current; current = current->parent)
    {
        if (auto it = current->typePackChanges.find(tp); it != current->typePackChanges.end())
            return it->second.get();
    }

    return nullptr;
}

PendingType* TxnLog::replace(TypeId ty, TypeVar replacement)
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

    PendingType* newTy = queue(ty);
    newTy->pending = replacement;
    return newTy;
}

PendingTypePack* TxnLog::replace(TypePackId tp, TypePackVar replacement)
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

    PendingTypePack* newTp = queue(tp);
    newTp->pending = replacement;
    return newTp;
}

PendingType* TxnLog::bindTable(TypeId ty, std::optional<TypeId> newBoundTo)
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);
    LUAU_ASSERT(get<TableTypeVar>(ty));

    PendingType* newTy = queue(ty);
    if (TableTypeVar* ttv = Luau::getMutable<TableTypeVar>(newTy))
        ttv->boundTo = newBoundTo;

    return newTy;
}

PendingType* TxnLog::changeLevel(TypeId ty, TypeLevel newLevel)
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);
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
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);
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
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);
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
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

    if (FreeTypeVar* ftv = getMutable<FreeTypeVar>(ty))
        return ftv->level;
    else if (TableTypeVar* ttv = getMutable<TableTypeVar>(ty); ttv && (ttv->state == TableState::Free || ttv->state == TableState::Generic))
        return ttv->level;
    else if (FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(ty))
        return ftv->level;

    return std::nullopt;
}

TypeId TxnLog::follow(TypeId ty)
{
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

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
    LUAU_ASSERT(FFlag::LuauUseCommittingTxnLog);

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
