// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Normalize.h"

#include <algorithm>

#include "Luau/Clone.h"
#include "Luau/Substitution.h"
#include "Luau/Unifier.h"
#include "Luau/VisitTypeVar.h"

LUAU_FASTFLAGVARIABLE(DebugLuauCopyBeforeNormalizing, false)

// This could theoretically be 2000 on amd64, but x86 requires this.
LUAU_FASTINTVARIABLE(LuauNormalizeIterationLimit, 1200);
LUAU_FASTFLAGVARIABLE(LuauNormalizeCombineTableFix, false);
LUAU_FASTFLAGVARIABLE(LuauNormalizeFlagIsConservative, false);
LUAU_FASTFLAGVARIABLE(LuauNormalizeCombineEqFix, false);
LUAU_FASTFLAGVARIABLE(LuauReplaceReplacer, false);
LUAU_FASTFLAG(LuauQuantifyConstrained)

namespace Luau
{

namespace
{

struct Replacer : Substitution
{
    TypeId sourceType;
    TypeId replacedType;
    DenseHashMap<TypeId, TypeId> replacedTypes{nullptr};
    DenseHashMap<TypePackId, TypePackId> replacedPacks{nullptr};

    Replacer(TypeArena* arena, TypeId sourceType, TypeId replacedType)
        : Substitution(TxnLog::empty(), arena)
        , sourceType(sourceType)
        , replacedType(replacedType)
    {
    }

    bool isDirty(TypeId ty) override
    {
        if (!sourceType)
            return false;

        auto vecHasSourceType = [sourceType = sourceType](const auto& vec) {
            return end(vec) != std::find(begin(vec), end(vec), sourceType);
        };

        // Walk every kind of TypeVar and find pointers to sourceType
        if (auto t = get<FreeTypeVar>(ty))
            return false;
        else if (auto t = get<GenericTypeVar>(ty))
            return false;
        else if (auto t = get<ErrorTypeVar>(ty))
            return false;
        else if (auto t = get<PrimitiveTypeVar>(ty))
            return false;
        else if (auto t = get<ConstrainedTypeVar>(ty))
            return vecHasSourceType(t->parts);
        else if (auto t = get<SingletonTypeVar>(ty))
            return false;
        else if (auto t = get<FunctionTypeVar>(ty))
        {
            if (vecHasSourceType(t->generics))
                return true;

            return false;
        }
        else if (auto t = get<TableTypeVar>(ty))
        {
            if (t->boundTo)
                return *t->boundTo == sourceType;

            for (const auto& [_name, prop] : t->props)
            {
                if (prop.type == sourceType)
                    return true;
            }

            if (auto indexer = t->indexer)
            {
                if (indexer->indexType == sourceType || indexer->indexResultType == sourceType)
                    return true;
            }

            if (vecHasSourceType(t->instantiatedTypeParams))
                return true;

            return false;
        }
        else if (auto t = get<MetatableTypeVar>(ty))
            return t->table == sourceType || t->metatable == sourceType;
        else if (auto t = get<ClassTypeVar>(ty))
            return false;
        else if (auto t = get<AnyTypeVar>(ty))
            return false;
        else if (auto t = get<UnionTypeVar>(ty))
            return vecHasSourceType(t->options);
        else if (auto t = get<IntersectionTypeVar>(ty))
            return vecHasSourceType(t->parts);
        else if (auto t = get<LazyTypeVar>(ty))
            return false;

        LUAU_ASSERT(!"Luau::Replacer::isDirty internal error: Unknown TypeVar type");
        LUAU_UNREACHABLE();
    }

    bool isDirty(TypePackId tp) override
    {
        if (auto it = replacedPacks.find(tp))
            return false;

        if (auto pack = get<TypePack>(tp))
        {
            for (TypeId ty : pack->head)
            {
                if (ty == sourceType)
                    return true;
            }
            return false;
        }
        else if (auto vtp = get<VariadicTypePack>(tp))
            return vtp->ty == sourceType;
        else
            return false;
    }

    TypeId clean(TypeId ty) override
    {
        LUAU_ASSERT(sourceType && replacedType);

        // Walk every kind of TypeVar and create a copy with sourceType replaced by replacedType
        // Before returning, memoize the result for later use.

        // Helpfully, Substitution::clone() only shallow-clones the kinds of types that we care to work with.  This
        // function returns the identity for things like primitives.
        TypeId res = clone(ty);

        if (auto t = get<FreeTypeVar>(res))
            LUAU_ASSERT(!"Impossible");
        else if (auto t = get<GenericTypeVar>(res))
            LUAU_ASSERT(!"Impossible");
        else if (auto t = get<ErrorTypeVar>(res))
            LUAU_ASSERT(!"Impossible");
        else if (auto t = get<PrimitiveTypeVar>(res))
            LUAU_ASSERT(!"Impossible");
        else if (auto t = getMutable<ConstrainedTypeVar>(res))
        {
            for (TypeId& part : t->parts)
            {
                if (part == sourceType)
                    part = replacedType;
            }
        }
        else if (auto t = get<SingletonTypeVar>(res))
            LUAU_ASSERT(!"Impossible");
        else if (auto t = getMutable<FunctionTypeVar>(res))
        {
            // The constituent typepacks are cleaned separately.  We just need to walk the generics array.
            for (TypeId& g : t->generics)
            {
                if (g == sourceType)
                    g = replacedType;
            }
        }
        else if (auto t = getMutable<TableTypeVar>(res))
        {
            for (auto& [_key, prop] : t->props)
            {
                if (prop.type == sourceType)
                    prop.type = replacedType;
            }
        }
        else if (auto t = getMutable<MetatableTypeVar>(res))
        {
            if (t->table == sourceType)
                t->table = replacedType;
            if (t->metatable == sourceType)
                t->table = replacedType;
        }
        else if (auto t = get<ClassTypeVar>(res))
            LUAU_ASSERT(!"Impossible");
        else if (auto t = get<AnyTypeVar>(res))
            LUAU_ASSERT(!"Impossible");
        else if (auto t = getMutable<UnionTypeVar>(res))
        {
            for (TypeId& option : t->options)
            {
                if (option == sourceType)
                    option = replacedType;
            }
        }
        else if (auto t = getMutable<IntersectionTypeVar>(res))
        {
            for (TypeId& part : t->parts)
            {
                if (part == sourceType)
                    part = replacedType;
            }
        }
        else if (auto t = get<LazyTypeVar>(res))
            LUAU_ASSERT(!"Impossible");
        else
            LUAU_ASSERT(!"Luau::Replacer::clean internal error: Unknown TypeVar type");

        replacedTypes[ty] = res;
        return res;
    }

    TypePackId clean(TypePackId tp) override
    {
        TypePackId res = clone(tp);

        if (auto pack = getMutable<TypePack>(res))
        {
            for (TypeId& type : pack->head)
            {
                if (type == sourceType)
                    type = replacedType;
            }
        }
        else if (auto vtp = getMutable<VariadicTypePack>(res))
        {
            if (vtp->ty == sourceType)
                vtp->ty = replacedType;
        }

        replacedPacks[tp] = res;
        return res;
    }

    TypeId smartClone(TypeId t)
    {
        if (FFlag::LuauReplaceReplacer)
        {
            // The new smartClone is just a memoized clone()
            // TODO: Remove the Substitution base class and all other methods from this struct.
            // Add DenseHashMap<TypeId, TypeId> newTypes;
            t = log->follow(t);
            TypeId* res = newTypes.find(t);
            if (res)
                return *res;

            TypeId result = shallowClone(t, *arena, TxnLog::empty());
            newTypes[t] = result;
            newTypes[result] = result;

            return result;
        }
        else
        {
            std::optional<TypeId> res = replace(t);
            LUAU_ASSERT(res.has_value()); // TODO think about this
            if (*res == t)
                return clone(t);
            return *res;
        }
    }
};

} // anonymous namespace

bool isSubtype(TypeId subTy, TypeId superTy, InternalErrorReporter& ice)
{
    UnifierSharedState sharedState{&ice};
    TypeArena arena;
    Unifier u{&arena, Mode::Strict, Location{}, Covariant, sharedState};
    u.anyIsTop = true;

    u.tryUnify(subTy, superTy);
    const bool ok = u.errors.empty() && u.log.empty();
    return ok;
}

bool isSubtype(TypePackId subPack, TypePackId superPack, InternalErrorReporter& ice)
{
    UnifierSharedState sharedState{&ice};
    TypeArena arena;
    Unifier u{&arena, Mode::Strict, Location{}, Covariant, sharedState};
    u.anyIsTop = true;

    u.tryUnify(subPack, superPack);
    const bool ok = u.errors.empty() && u.log.empty();
    return ok;
}

template<typename T>
static bool areNormal_(const T& t, const std::unordered_set<void*>& seen, InternalErrorReporter& ice)
{
    int count = 0;
    auto isNormal = [&](TypeId ty) {
        ++count;
        if (count >= FInt::LuauNormalizeIterationLimit)
            ice.ice("Luau::areNormal hit iteration limit");

        if (FFlag::LuauNormalizeFlagIsConservative)
            return ty->normal;
        else
        {
            // The follow is here because a bound type may not be normal, but the bound type is normal.
            return ty->normal || follow(ty)->normal || seen.find(asMutable(ty)) != seen.end();
        }
    };

    return std::all_of(begin(t), end(t), isNormal);
}

static bool areNormal(const std::vector<TypeId>& types, const std::unordered_set<void*>& seen, InternalErrorReporter& ice)
{
    return areNormal_(types, seen, ice);
}

static bool areNormal(TypePackId tp, const std::unordered_set<void*>& seen, InternalErrorReporter& ice)
{
    tp = follow(tp);
    if (get<FreeTypePack>(tp))
        return false;

    auto [head, tail] = flatten(tp);

    if (!areNormal_(head, seen, ice))
        return false;

    if (!tail)
        return true;

    if (auto vtp = get<VariadicTypePack>(*tail))
        return vtp->ty->normal || follow(vtp->ty)->normal || seen.find(asMutable(vtp->ty)) != seen.end();

    return true;
}

#define CHECK_ITERATION_LIMIT(...) \
    do \
    { \
        if (iterationLimit > FInt::LuauNormalizeIterationLimit) \
        { \
            limitExceeded = true; \
            return __VA_ARGS__; \
        } \
        ++iterationLimit; \
    } while (false)

struct Normalize final : TypeVarVisitor
{
    using TypeVarVisitor::Set;

    Normalize(TypeArena& arena, InternalErrorReporter& ice)
        : arena(arena)
        , ice(ice)
    {
    }

    TypeArena& arena;
    InternalErrorReporter& ice;

    int iterationLimit = 0;
    bool limitExceeded = false;

    bool visit(TypeId ty, const FreeTypeVar&) override
    {
        LUAU_ASSERT(!ty->normal);
        return false;
    }

    bool visit(TypeId ty, const BoundTypeVar& btv) override
    {
        // A type could be considered normal when it is in the stack, but we will eventually find out it is not normal as normalization progresses.
        // So we need to avoid eagerly saying that this bound type is normal if the thing it is bound to is in the stack.
        if (seen.find(asMutable(btv.boundTo)) != seen.end())
            return false;

        // It should never be the case that this TypeVar is normal, but is bound to a non-normal type, except in nontrivial cases.
        LUAU_ASSERT(!ty->normal || ty->normal == btv.boundTo->normal);

        asMutable(ty)->normal = btv.boundTo->normal;
        return !ty->normal;
    }

    bool visit(TypeId ty, const PrimitiveTypeVar&) override
    {
        LUAU_ASSERT(ty->normal);
        return false;
    }

    bool visit(TypeId ty, const GenericTypeVar&) override
    {
        if (!ty->normal)
            asMutable(ty)->normal = true;

        return false;
    }

    bool visit(TypeId ty, const ErrorTypeVar&) override
    {
        if (!ty->normal)
            asMutable(ty)->normal = true;
        return false;
    }

    bool visit(TypeId ty, const ConstrainedTypeVar& ctvRef) override
    {
        CHECK_ITERATION_LIMIT(false);
        LUAU_ASSERT(!ty->normal);

        ConstrainedTypeVar* ctv = const_cast<ConstrainedTypeVar*>(&ctvRef);

        std::vector<TypeId> parts = std::move(ctv->parts);

        // We might transmute, so it's not safe to rely on the builtin traversal logic of visitTypeVar
        for (TypeId part : parts)
            traverse(part);

        std::vector<TypeId> newParts = normalizeUnion(parts);

        if (FFlag::LuauQuantifyConstrained)
        {
            ctv->parts = std::move(newParts);
        }
        else
        {
            const bool normal = areNormal(newParts, seen, ice);

            if (newParts.size() == 1)
                *asMutable(ty) = BoundTypeVar{newParts[0]};
            else
                *asMutable(ty) = UnionTypeVar{std::move(newParts)};

            asMutable(ty)->normal = normal;
        }

        return false;
    }

    bool visit(TypeId ty, const FunctionTypeVar& ftv) override
    {
        CHECK_ITERATION_LIMIT(false);

        if (ty->normal)
            return false;

        traverse(ftv.argTypes);
        traverse(ftv.retTypes);

        asMutable(ty)->normal = areNormal(ftv.argTypes, seen, ice) && areNormal(ftv.retTypes, seen, ice);

        return false;
    }

    bool visit(TypeId ty, const TableTypeVar& ttv) override
    {
        CHECK_ITERATION_LIMIT(false);

        if (ty->normal)
            return false;

        bool normal = true;

        auto checkNormal = [&](TypeId t) {
            // if t is on the stack, it is possible that this type is normal.
            // If t is not normal and it is not on the stack, this type is definitely not normal.
            if (!t->normal && seen.find(asMutable(t)) == seen.end())
                normal = false;
        };

        if (ttv.boundTo)
        {
            traverse(*ttv.boundTo);
            asMutable(ty)->normal = (*ttv.boundTo)->normal;
            return false;
        }

        for (const auto& [_name, prop] : ttv.props)
        {
            traverse(prop.type);
            checkNormal(prop.type);
        }

        if (ttv.indexer)
        {
            traverse(ttv.indexer->indexType);
            checkNormal(ttv.indexer->indexType);
            traverse(ttv.indexer->indexResultType);
            checkNormal(ttv.indexer->indexResultType);
        }

        // An unsealed table can never be normal, ditto for free tables iff the type it is bound to is also not normal.
        if (FFlag::LuauQuantifyConstrained)
        {
            if (ttv.state == TableState::Generic || ttv.state == TableState::Sealed || (ttv.state == TableState::Free && follow(ty)->normal))
                asMutable(ty)->normal = normal;
        }
        else
            asMutable(ty)->normal = normal;

        return false;
    }

    bool visit(TypeId ty, const MetatableTypeVar& mtv) override
    {
        CHECK_ITERATION_LIMIT(false);

        if (ty->normal)
            return false;

        traverse(mtv.table);
        traverse(mtv.metatable);

        asMutable(ty)->normal = mtv.table->normal && mtv.metatable->normal;

        return false;
    }

    bool visit(TypeId ty, const ClassTypeVar& ctv) override
    {
        if (!ty->normal)
            asMutable(ty)->normal = true;
        return false;
    }

    bool visit(TypeId ty, const AnyTypeVar&) override
    {
        LUAU_ASSERT(ty->normal);
        return false;
    }

    bool visit(TypeId ty, const UnionTypeVar& utvRef) override
    {
        CHECK_ITERATION_LIMIT(false);

        if (ty->normal)
            return false;

        UnionTypeVar* utv = &const_cast<UnionTypeVar&>(utvRef);
        std::vector<TypeId> options = std::move(utv->options);

        // We might transmute, so it's not safe to rely on the builtin traversal logic of visitTypeVar
        for (TypeId option : options)
            traverse(option);

        std::vector<TypeId> newOptions = normalizeUnion(options);

        const bool normal = areNormal(newOptions, seen, ice);

        LUAU_ASSERT(!newOptions.empty());

        if (newOptions.size() == 1)
            *asMutable(ty) = BoundTypeVar{newOptions[0]};
        else
            utv->options = std::move(newOptions);

        asMutable(ty)->normal = normal;

        return false;
    }

    bool visit(TypeId ty, const IntersectionTypeVar& itvRef) override
    {
        CHECK_ITERATION_LIMIT(false);

        if (ty->normal)
            return false;

        IntersectionTypeVar* itv = &const_cast<IntersectionTypeVar&>(itvRef);

        std::vector<TypeId> oldParts = std::move(itv->parts);

        for (TypeId part : oldParts)
            traverse(part);

        std::vector<TypeId> tables;
        for (TypeId part : oldParts)
        {
            part = follow(part);
            if (get<TableTypeVar>(part))
                tables.push_back(part);
            else
            {
                Replacer replacer{&arena, nullptr, nullptr}; // FIXME this is super super WEIRD
                combineIntoIntersection(replacer, itv, part);
            }
        }

        // Don't allocate a new table if there's just one in the intersection.
        if (tables.size() == 1)
            itv->parts.push_back(tables[0]);
        else if (!tables.empty())
        {
            const TableTypeVar* first = get<TableTypeVar>(tables[0]);
            LUAU_ASSERT(first);

            TypeId newTable = arena.addType(TableTypeVar{first->state, first->level});
            TableTypeVar* ttv = getMutable<TableTypeVar>(newTable);
            for (TypeId part : tables)
            {
                // Intuition: If combineIntoTable() needs to clone a table, any references to 'part' are cyclic and need
                // to be rewritten to point at 'newTable' in the clone.
                Replacer replacer{&arena, part, newTable};
                combineIntoTable(replacer, ttv, part);
            }

            itv->parts.push_back(newTable);
        }

        asMutable(ty)->normal = areNormal(itv->parts, seen, ice);

        if (itv->parts.size() == 1)
        {
            TypeId part = itv->parts[0];
            *asMutable(ty) = BoundTypeVar{part};
        }

        return false;
    }

    std::vector<TypeId> normalizeUnion(const std::vector<TypeId>& options)
    {
        if (options.size() == 1)
            return options;

        std::vector<TypeId> result;

        for (TypeId part : options)
            combineIntoUnion(result, part);

        return result;
    }

    void combineIntoUnion(std::vector<TypeId>& result, TypeId ty)
    {
        ty = follow(ty);
        if (auto utv = get<UnionTypeVar>(ty))
        {
            for (TypeId t : utv)
                combineIntoUnion(result, t);
            return;
        }

        for (TypeId& part : result)
        {
            if (isSubtype(ty, part, ice))
                return; // no need to do anything
            else if (isSubtype(part, ty, ice))
            {
                part = ty; // replace the less general type by the more general one
                return;
            }
        }

        result.push_back(ty);
    }

    /**
     * @param replacer knows how to clone a type such that any recursive references point at the new containing type.
     * @param result is an intersection that is safe for us to mutate in-place.
     */
    void combineIntoIntersection(Replacer& replacer, IntersectionTypeVar* result, TypeId ty)
    {
        // Note: this check guards against running out of stack space
        // so if you increase the size of a stack frame, you'll need to decrease the limit.
        CHECK_ITERATION_LIMIT();

        ty = follow(ty);
        if (auto itv = get<IntersectionTypeVar>(ty))
        {
            for (TypeId part : itv->parts)
                combineIntoIntersection(replacer, result, part);
            return;
        }

        // Let's say that the last part of our result intersection is always a table, if any table is part of this intersection
        if (get<TableTypeVar>(ty))
        {
            if (result->parts.empty())
                result->parts.push_back(arena.addType(TableTypeVar{TableState::Sealed, TypeLevel{}}));

            TypeId theTable = result->parts.back();

            if (!get<TableTypeVar>(follow(theTable)))
            {
                result->parts.push_back(arena.addType(TableTypeVar{TableState::Sealed, TypeLevel{}}));
                theTable = result->parts.back();
            }

            TypeId newTable = replacer.smartClone(theTable);
            result->parts.back() = newTable;

            combineIntoTable(replacer, getMutable<TableTypeVar>(newTable), ty);
        }
        else if (auto ftv = get<FunctionTypeVar>(ty))
        {
            bool merged = false;
            for (TypeId& part : result->parts)
            {
                if (isSubtype(part, ty, ice))
                {
                    merged = true;
                    break; // no need to do anything
                }
                else if (isSubtype(ty, part, ice))
                {
                    merged = true;
                    part = ty; // replace the less general type by the more general one
                    break;
                }
            }

            if (!merged)
                result->parts.push_back(ty);
        }
        else
            result->parts.push_back(ty);
    }

    TableState combineTableStates(TableState lhs, TableState rhs)
    {
        if (lhs == rhs)
            return lhs;

        if (lhs == TableState::Free || rhs == TableState::Free)
            return TableState::Free;

        if (lhs == TableState::Unsealed || rhs == TableState::Unsealed)
            return TableState::Unsealed;

        return lhs;
    }

    /**
     * @param replacer gives us a way to clone a type such that recursive references are rewritten to the new
     * "containing" type.
     * @param table always points into a table that is safe for us to mutate.
     */
    void combineIntoTable(Replacer& replacer, TableTypeVar* table, TypeId ty)
    {
        // Note: this check guards against running out of stack space
        // so if you increase the size of a stack frame, you'll need to decrease the limit.
        CHECK_ITERATION_LIMIT();

        LUAU_ASSERT(table);

        ty = follow(ty);

        TableTypeVar* tyTable = getMutable<TableTypeVar>(ty);
        LUAU_ASSERT(tyTable);

        for (const auto& [propName, prop] : tyTable->props)
        {
            if (auto it = table->props.find(propName); it != table->props.end())
            {
                /**
                 * If we are going to recursively merge intersections of tables, we need to ensure that we never mutate
                 * a table that comes from somewhere else in the type graph.
                 *
                 * smarClone() does some nice things for us: It will perform a clone that is as shallow as possible
                 * while still rewriting any cyclic references back to the new 'root' table.
                 *
                 * replacer also keeps a mapping of types that have previously been copied, so we have the added
                 * advantage here of knowing that, whether or not a new copy was actually made, the resulting TypeVar is
                 * safe for us to mutate in-place.
                 */
                TypeId clone = replacer.smartClone(it->second.type);
                it->second.type = combine(replacer, clone, prop.type);
            }
            else
                table->props.insert({propName, prop});
        }

        table->state = combineTableStates(table->state, tyTable->state);
        table->level = max(table->level, tyTable->level);
    }

    /**
     * @param a is always cloned by the caller.  It is safe to mutate in-place.
     * @param b will never be mutated.
     */
    TypeId combine(Replacer& replacer, TypeId a, TypeId b)
    {
        if (FFlag::LuauNormalizeCombineEqFix)
            b = follow(b);

        if (FFlag::LuauNormalizeCombineTableFix && a == b)
            return a;

        if (!get<IntersectionTypeVar>(a) && !get<TableTypeVar>(a))
        {
            if (!FFlag::LuauNormalizeCombineTableFix && a == b)
                return a;
            else
                return arena.addType(IntersectionTypeVar{{a, b}});
        }

        if (auto itv = getMutable<IntersectionTypeVar>(a))
        {
            combineIntoIntersection(replacer, itv, b);
            return a;
        }
        else if (auto ttv = getMutable<TableTypeVar>(a))
        {
            if (FFlag::LuauNormalizeCombineTableFix && !get<TableTypeVar>(FFlag::LuauNormalizeCombineEqFix ? b : follow(b)))
                return arena.addType(IntersectionTypeVar{{a, b}});
            combineIntoTable(replacer, ttv, b);
            return a;
        }

        LUAU_ASSERT(!"Impossible");
        LUAU_UNREACHABLE();
    }
};

#undef CHECK_ITERATION_LIMIT

/**
 * @returns A tuple of TypeId and a success indicator. (true indicates that the normalization completed successfully)
 */
std::pair<TypeId, bool> normalize(TypeId ty, TypeArena& arena, InternalErrorReporter& ice)
{
    CloneState state;
    if (FFlag::DebugLuauCopyBeforeNormalizing)
        (void)clone(ty, arena, state);

    Normalize n{arena, ice};
    n.traverse(ty);

    return {ty, !n.limitExceeded};
}

// TODO: Think about using a temporary arena and cloning types out of it so that we
// reclaim memory used by wantonly allocated intermediate types here.
// The main wrinkle here is that we don't want clone() to copy a type if the source and dest
// arena are the same.
std::pair<TypeId, bool> normalize(TypeId ty, const ModulePtr& module, InternalErrorReporter& ice)
{
    return normalize(ty, module->internalTypes, ice);
}

/**
 * @returns A tuple of TypeId and a success indicator. (true indicates that the normalization completed successfully)
 */
std::pair<TypePackId, bool> normalize(TypePackId tp, TypeArena& arena, InternalErrorReporter& ice)
{
    CloneState state;
    if (FFlag::DebugLuauCopyBeforeNormalizing)
        (void)clone(tp, arena, state);

    Normalize n{arena, ice};
    n.traverse(tp);

    return {tp, !n.limitExceeded};
}

std::pair<TypePackId, bool> normalize(TypePackId tp, const ModulePtr& module, InternalErrorReporter& ice)
{
    return normalize(tp, module->internalTypes, ice);
}

} // namespace Luau
