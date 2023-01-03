// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TxnLog.h"

#include "Luau/ToString.h"
#include "Luau/TypeArena.h"
#include "Luau/TypePack.h"

#include <algorithm>
#include <stdexcept>

LUAU_FASTFLAG(LuauUnknownAndNeverType)

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

void TxnLog::concatAsIntersections(TxnLog rhs, NotNull<TypeArena> arena)
{
    for (auto& [ty, rightRep] : rhs.typeVarChanges)
    {
        if (auto leftRep = typeVarChanges.find(ty))
        {
            TypeId leftTy = arena->addType((*leftRep)->pending);
            TypeId rightTy = arena->addType(rightRep->pending);
            typeVarChanges[ty]->pending.ty = IntersectionType{{leftTy, rightTy}};
        }
        else
            typeVarChanges[ty] = std::move(rightRep);
    }

    for (auto& [tp, rep] : rhs.typePackChanges)
        typePackChanges[tp] = std::move(rep);
}

void TxnLog::concatAsUnion(TxnLog rhs, NotNull<TypeArena> arena)
{
    for (auto& [ty, rightRep] : rhs.typeVarChanges)
    {
        if (auto leftRep = typeVarChanges.find(ty))
        {
            TypeId leftTy = arena->addType((*leftRep)->pending);
            TypeId rightTy = arena->addType(rightRep->pending);
            typeVarChanges[ty]->pending.ty = UnionType{{leftTy, rightTy}};
        }
        else
            typeVarChanges[ty] = std::move(rightRep);
    }

    for (auto& [tp, rep] : rhs.typePackChanges)
        typePackChanges[tp] = std::move(rep);
}

void TxnLog::commit()
{
    for (auto& [ty, rep] : typeVarChanges)
        asMutable(ty)->reassign(rep.get()->pending);

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
    LUAU_ASSERT(!ty->persistent);

    // Explicitly don't look in ancestors. If we have discovered something new
    // about this type, we don't want to mutate the parent's state.
    auto& pending = typeVarChanges[ty];
    if (!pending)
    {
        pending = std::make_unique<PendingType>(*ty);
        pending->pending.owningArena = nullptr;
    }

    return pending.get();
}

PendingTypePack* TxnLog::queue(TypePackId tp)
{
    LUAU_ASSERT(!tp->persistent);

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

PendingType* TxnLog::changeScope(TypeId ty, NotNull<Scope> newScope)
{
    LUAU_ASSERT(get<FreeType>(ty) || get<TableType>(ty) || get<FunctionType>(ty));

    PendingType* newTy = queue(ty);
    if (FreeType* ftv = Luau::getMutable<FreeType>(newTy))
    {
        ftv->scope = newScope;
    }
    else if (TableType* ttv = Luau::getMutable<TableType>(newTy))
    {
        LUAU_ASSERT(ttv->state == TableState::Free || ttv->state == TableState::Generic);
        ttv->scope = newScope;
    }
    else if (FunctionType* ftv = Luau::getMutable<FunctionType>(newTy))
    {
        ftv->scope = newScope;
    }

    return newTy;
}

PendingTypePack* TxnLog::changeScope(TypePackId tp, NotNull<Scope> newScope)
{
    LUAU_ASSERT(get<FreeTypePack>(tp));

    PendingTypePack* newTp = queue(tp);
    if (FreeTypePack* ftp = Luau::getMutable<FreeTypePack>(newTp))
    {
        ftp->scope = newScope;
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
    return Luau::follow(ty, [this](TypeId ty) {
        PendingType* state = this->pending(ty);

        if (state == nullptr)
            return ty;

        // Ugly: Fabricate a TypeId that doesn't adhere to most of the invariants
        // that normally apply. This is safe because follow will only call get<>
        // on the returned pointer.
        return const_cast<const Type*>(&state->pending);
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

std::pair<std::vector<TypeId>, std::vector<TypePackId>> TxnLog::getChanges() const
{
    std::pair<std::vector<TypeId>, std::vector<TypePackId>> result;

    for (const auto& [typeId, _newState] : typeVarChanges)
        result.first.push_back(typeId);
    for (const auto& [typePackId, _newState] : typePackChanges)
        result.second.push_back(typePackId);

    return result;
}

} // namespace Luau
