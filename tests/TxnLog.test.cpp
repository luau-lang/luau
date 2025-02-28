// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "doctest.h"

#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"

#include "ScopedFlags.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)

struct TxnLogFixture
{
    TxnLog log;
    TxnLog log2;
    TypeArena arena;
    BuiltinTypes builtinTypes;

    ScopePtr globalScope = std::make_shared<Scope>(builtinTypes.anyTypePack);
    ScopePtr childScope = std::make_shared<Scope>(globalScope);

    TypeId a = freshType(NotNull{&arena}, NotNull{&builtinTypes}, globalScope.get());
    TypeId b = freshType(NotNull{&arena}, NotNull{&builtinTypes}, globalScope.get());
    TypeId c = freshType(NotNull{&arena}, NotNull{&builtinTypes}, childScope.get());

    TypeId g = arena.addType(GenericType{"G"});
};

TEST_SUITE_BEGIN("TxnLog");

TEST_CASE_FIXTURE(TxnLogFixture, "colliding_union_incoming_type_has_lesser_scope")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    log.replace(a, BoundType{c});
    log2.replace(c, BoundType{a});

    CHECK(nullptr != log.pending(a));

    log.concatAsUnion(std::move(log2), NotNull{&arena});

    // 'a has greater scope than 'c, so we expect the binding of 'a to be
    // discarded, and for that of 'c to be brought in.

    CHECK(nullptr == log.pending(a));

    const PendingType* pt = log.pending(c);
    REQUIRE(pt != nullptr);

    CHECK(!pt->dead);
    const BoundType* bt = get_if<BoundType>(&pt->pending.ty);

    CHECK(a == bt->boundTo);

    log.commit();

    REQUIRE(get<FreeType>(a));

    const BoundType* bound = get<BoundType>(c);
    REQUIRE(bound);
    CHECK(a == bound->boundTo);
}

TEST_CASE_FIXTURE(TxnLogFixture, "colliding_coincident_logs_do_not_create_degenerate_unions")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    log.replace(a, BoundType{b});
    log2.replace(a, BoundType{b});

    log.concatAsUnion(std::move(log2), NotNull{&arena});

    log.commit();

    CHECK("'a" == toString(a));
    CHECK("'a" == toString(b));
}

TEST_CASE_FIXTURE(TxnLogFixture, "replacing_persistent_types_is_allowed_but_makes_the_log_radioactive")
{
    persist(g);

    log.replace(g, BoundType{a});

    CHECK(log.radioactive);
}

TEST_SUITE_END();
