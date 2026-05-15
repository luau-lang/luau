// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Compiler.h"
#include "Luau/BytecodeBuilder.h"

#include "lua.h"
#include "lualib.h"
#include "lstate.h"

#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

#include <cstdlib>

LUAU_FASTINT(LuauInlineHitsThreshold)
LUAU_FASTFLAG(LuauCallFeedback)
LUAU_FASTFLAG(LuauEmitCallFeedback)

using namespace Luau;

void* alloc(void* ud, void* ptr, size_t osize, size_t nsize)
{
    (void)ud;
    (void)osize;
    if (nsize == 0)
    {
        std::free(ptr);
        return NULL;
    }
    else
        return std::realloc(ptr, nsize);
}

struct FeedbackVectorFixture
{
    BytecodeBuilder bcb;
    std::unique_ptr<lua_State, void (*)(lua_State*)> L;
    Proto* (*onInline)(lua_State*, Closure*, Closure*, uint32_t) = nullptr;

    FeedbackVectorFixture()
        : L(lua_newstate(alloc, NULL), lua_close)
    {
    }

    void compile(std::string source)
    {
        bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
        CompileOptions opts;
        opts.optimizationLevel = 0;
        compileOrThrow(bcb, source, opts);
    }

    Proto* load()
    {
        std::string bytecode = bcb.getBytecode();
        int res = luau_load(L.get(), "=FeedbackVectorTest", bytecode.data(), bytecode.size(), 0);
        LUAU_ASSERT(res == 0 && lua_isfunction(L.get(), -1));
        Closure* top = clvalue(L->top - 1);
        return top->l.p;
    }

    void run()
    {
        L->global->ecb.inlinefunction = onInline;
        int status = lua_resume(L.get(), nullptr, 0);
        LUAU_ASSERT(status == 0);
    }
};

TEST_SUITE_BEGIN("FeedbackVector");

Proto* idInliner(lua_State* L, Closure* caller, Closure* target, uint32_t pc)
{
    return caller->l.p;
}

Proto* sealingInliner(lua_State* L, Closure* caller, Closure* target, uint32_t pc)
{
    return nullptr;
}

struct AssertInlinerData
{
    Proto* proto;
    Proto* target;
    uint32_t pc;
    bool called = false;
};

Proto* idInlinerWithAssert(lua_State* L, Closure* caller, Closure* target, uint32_t pc)
{
    auto data = reinterpret_cast<AssertInlinerData*>(L->global->ecbdata);
    CHECK_EQ(data->proto, caller->l.p);
    CHECK_EQ(data->target, target->l.p);
    CHECK_EQ(data->pc, pc);
    data->called = true;
    return caller->l.p;
}

TEST_CASE_FIXTURE(FeedbackVectorFixture, "simple_call")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};
    ScopedFastFlag callFb{FFlag::LuauCallFeedback, true};
    ScopedFastInt inlineThreshold{FInt::LuauInlineHitsThreshold, 2};

    compile(R"(
        local function g() return 1 end
        local function f() return g() + 1 end
        f()
        f()
    )");

    CHECK_EQ("\n" + bcb.dumpFunction(1), R"(
GETUPVAL R1 0
CALLFB R1 0 1 [0]
LOADK R2 K0 [1]
ADD R0 R1 R2
RETURN R0 1
)");

    Proto* top = load();
    Proto* g = top->p[0];
    CHECK_NE(g->flags & LPF_INLINABLE, 0);
    Proto* f = top->p[1];

    CHECK_EQ(f->feedbackvecsize, 1);

    FeedbackVectorSlot& fbslot = f->feedbackvec[0];
    CHECK_EQ(fbslot.kind, FeedbackVectorSlotKind::CALL_TARGET);
    CHECK_EQ(fbslot.call_target.pc, 1);
    CHECK_EQ(fbslot.call_target.proto, 0);
    CHECK_EQ(fbslot.call_target.hits, 0);
    CHECK_EQ(f->code[fbslot.call_target.pc + 1], 0);

    auto data = reinterpret_cast<AssertInlinerData*>(L->global->ecbdata);
    data->proto = f;
    data->target = g;
    data->pc = fbslot.call_target.pc;
    onInline = idInlinerWithAssert;

    run();

    CHECK_EQ(fbslot.call_target.pc, 1);
    CHECK_EQ(fbslot.call_target.proto, g->funid);
    CHECK_EQ(fbslot.call_target.hits, 2);
    CHECK_EQ(data->called, true);
}

TEST_CASE_FIXTURE(FeedbackVectorFixture, "simple_call_sealed")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};
    ScopedFastFlag callFb{FFlag::LuauCallFeedback, true};
    ScopedFastInt inlineThreshold{FInt::LuauInlineHitsThreshold, 2};

    compile(R"(
        local function g() return 1 end
        local function f() return g() + 1 end
        f()
        f()
    )");

    Proto* top = load();
    Proto* f = top->p[1];
    FeedbackVectorSlot& fbslot = f->feedbackvec[0];

    CHECK_EQ(f->code[fbslot.call_target.pc + 1], 0);
    // Sealing the slot
    f->code[fbslot.call_target.pc + 1] = 0xFFFFFFFF;

    onInline = idInliner;

    run();

    CHECK_EQ(fbslot.call_target.proto, 0);
    CHECK_EQ(fbslot.call_target.hits, 0);
}

TEST_CASE_FIXTURE(FeedbackVectorFixture, "simple_call_sealed_on_inline")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};
    ScopedFastFlag callFb{FFlag::LuauCallFeedback, true};
    ScopedFastInt inlineThreshold{FInt::LuauInlineHitsThreshold, 2};

    compile(R"(
        local function g() return 1 end
        local function f() return g() + 1 end
        f()
        f()
    )");

    Proto* top = load();
    Proto* f = top->p[1];
    FeedbackVectorSlot& fbslot = f->feedbackvec[0];

    CHECK_EQ(f->code[fbslot.call_target.pc + 1], 0);

    onInline = sealingInliner;

    run();

    CHECK_EQ(f->code[fbslot.call_target.pc + 1], 0xFFFFFFFF);
}

TEST_CASE_FIXTURE(FeedbackVectorFixture, "high_order_call")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};
    ScopedFastFlag callFb{FFlag::LuauCallFeedback, true};
    ScopedFastInt inlineThreshold{FInt::LuauInlineHitsThreshold, 2};

    compile(R"(
        local function g() return 1 end
        local function f(h) return h() + 1 end
        f(g)
        f(g)
    )");

    CHECK_EQ("\n" + bcb.dumpFunction(1), R"(
MOVE R2 R0
CALLFB R2 0 1 [0]
LOADK R3 K0 [1]
ADD R1 R2 R3
RETURN R1 1
)");

    Proto* top = load();
    Proto* g = top->p[0];
    CHECK_NE(g->flags & LPF_INLINABLE, 0);
    Proto* f = top->p[1];

    CHECK_EQ(f->feedbackvecsize, 1);

    FeedbackVectorSlot& fbslot = f->feedbackvec[0];
    CHECK_EQ(fbslot.kind, FeedbackVectorSlotKind::CALL_TARGET);
    CHECK_EQ(fbslot.call_target.pc, 1);
    CHECK_EQ(fbslot.call_target.proto, 0);
    CHECK_EQ(fbslot.call_target.hits, 0);
    CHECK_EQ(f->code[fbslot.call_target.pc + 1], 0);

    auto data = reinterpret_cast<AssertInlinerData*>(L->global->ecbdata);
    data->proto = f;
    data->target = g;
    data->pc = fbslot.call_target.pc;
    onInline = idInlinerWithAssert;

    run();

    CHECK_EQ(fbslot.call_target.pc, 1);
    CHECK_EQ(fbslot.call_target.proto, g->funid);
    CHECK_EQ(fbslot.call_target.hits, 2);
    CHECK_EQ(data->called, true);
}

TEST_CASE_FIXTURE(FeedbackVectorFixture, "polymorphic_call_sealed")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};
    ScopedFastFlag callFb{FFlag::LuauCallFeedback, true};
    ScopedFastInt inlineThreshold{FInt::LuauInlineHitsThreshold, 2};

    compile(R"(
        local function g() return 1 end
        local function y() return 2 end
        local function f(h) return h() + 1 end
        f(g)
        f(y)
    )");

    Proto* top = load();
    Proto* f = top->p[2];
    FeedbackVectorSlot& fbslot = f->feedbackvec[0];

    CHECK_EQ(f->code[fbslot.call_target.pc + 1], 0);

    onInline = idInliner;

    run();

    CHECK_EQ(f->code[fbslot.call_target.pc + 1], 0xFFFFFFFF);
}

TEST_CASE_FIXTURE(FeedbackVectorFixture, "c_call_sealed")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};
    ScopedFastFlag callFb{FFlag::LuauCallFeedback, true};
    ScopedFastInt inlineThreshold{FInt::LuauInlineHitsThreshold, 2};

    compile(R"(
        local function f(h) return h(1) + 1 end
        f(tostring)
    )");

    Proto* top = load();
    Proto* f = top->p[0];
    FeedbackVectorSlot& fbslot = f->feedbackvec[0];

    CHECK_EQ(f->code[fbslot.call_target.pc + 1], 0);

    onInline = idInliner;

    // for tostring
    lua_pop(L.get(), luaopen_base(L.get()));

    run();

    CHECK_EQ(f->code[fbslot.call_target.pc + 1], 0xFFFFFFFF);
}

TEST_CASE_FIXTURE(FeedbackVectorFixture, "metamethod_call_sealed")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};
    ScopedFastFlag callFb{FFlag::LuauCallFeedback, true};
    ScopedFastInt inlineThreshold{FInt::LuauInlineHitsThreshold, 2};

    compile(R"(
        local function f(h) return h(1) + 1 end

        local callableTable = {}

        setmetatable(callableTable, { __call = function(self, arg) return arg + 42 end })

        f(callableTable)
    )");

    Proto* top = load();
    Proto* f = top->p[0];
    FeedbackVectorSlot& fbslot = f->feedbackvec[0];

    CHECK_EQ(f->code[fbslot.call_target.pc + 1], 0);

    onInline = idInliner;

    // for setmetatable
    lua_pop(L.get(), luaopen_base(L.get()));

    run();

    CHECK_EQ(f->code[fbslot.call_target.pc + 1], 0xFFFFFFFF);
}

TEST_CASE_FIXTURE(FeedbackVectorFixture, "namecall")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};
    ScopedFastFlag callFb{FFlag::LuauCallFeedback, true};
    ScopedFastInt inlineThreshold{FInt::LuauInlineHitsThreshold, 2};

    compile(R"(
        local t = { x = 1 }
        function t.g(self) return self.x end
        local function f(t) return t:g() + 1 end
        f(t)
        f(t)
    )");

    CHECK_EQ("\n" + bcb.dumpFunction(1), R"(
NAMECALL R2 R0 K0 ['g']
CALLFB R2 1 1 [0]
LOADK R3 K1 [1]
ADD R1 R2 R3
RETURN R1 1
)");

    Proto* top = load();
    Proto* g = top->p[0];
    CHECK_NE(g->flags & LPF_INLINABLE, 0);
    Proto* f = top->p[1];

    CHECK_EQ(f->feedbackvecsize, 1);

    FeedbackVectorSlot& fbslot = f->feedbackvec[0];
    CHECK_EQ(fbslot.kind, FeedbackVectorSlotKind::CALL_TARGET);
    CHECK_EQ(fbslot.call_target.pc, 2);
    CHECK_EQ(fbslot.call_target.proto, 0);
    CHECK_EQ(fbslot.call_target.hits, 0);
    CHECK_EQ(f->code[fbslot.call_target.pc + 1], 0);

    auto data = reinterpret_cast<AssertInlinerData*>(L->global->ecbdata);
    data->proto = f;
    data->target = g;
    data->pc = fbslot.call_target.pc;
    onInline = idInlinerWithAssert;

    run();

    CHECK_EQ(fbslot.call_target.proto, g->funid);
    CHECK_EQ(fbslot.call_target.hits, 2);
    CHECK_EQ(data->called, true);
}

TEST_SUITE_END();
