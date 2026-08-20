// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Bytecode.h"
#include "Luau/BytecodeGraph.h"
#include "Luau/Sccp.h"

#include "lmem.h"
#include "lobject.h"
#include "lstate.h"

#include <optional>
#include <vector>

namespace Luau
{
namespace Bytecode
{

constexpr size_t kDefaultBackingSize = 8;

struct TempTValueBacking
{
    lua_State* L;
    std::vector<TValue*> chunks;
    size_t chunkSize = kDefaultBackingSize;
    size_t countInChunk = 0;
    uint8_t memcat;

    explicit TempTValueBacking(lua_State* L)
        : L(L)
        , memcat(L->activememcat)
    {
        allocateChunk();
    }

    TempTValueBacking(const TempTValueBacking&) = delete;
    TempTValueBacking(TempTValueBacking&&) = delete;

    TempTValueBacking& operator=(const TempTValueBacking&) = delete;
    TempTValueBacking& operator=(TempTValueBacking&&) = delete;

    ~TempTValueBacking() noexcept
    {
        for (TValue* chunk : chunks)
            luaM_freearray(L, chunk, chunkSize, TValue, memcat);
    }

    TValue* nextTValue()
    {
        if (countInChunk >= chunkSize)
            allocateChunk();

        return &chunks.back()[countInChunk++];
    }

private:
    void allocateChunk()
    {
        TValue* chunk = luaM_newarray(L, chunkSize, TValue, memcat);
        chunks.push_back(chunk);
        countInChunk = 0;
    }
};

// VmConstOps implementation backed by runtime TValue* constants.
// evaluate and makeNil allocate scratch TValues via backing; backing is mutable
// because the VmConstOps interface declares these methods const.
struct TValueVmConstImpl : public VmConstOps
{
    BcFunction<TValue*>& func;
    mutable TempTValueBacking backing;

    explicit TValueVmConstImpl(lua_State* L, BcFunction<TValue*>& func)
        : VmConstOps()
        , func(func)
        , backing(L)
    {
    }

    std::optional<BcOp> evaluate(const BcOp& lhsOp, const BcOp& rhsOp, LuauOpcode op) const override;
    bool falsey(const BcOp& falseyOp) const override;

    int cmp(const BcOp& lhsOp, const BcOp& rhsOp) const override;
    int cmp(const BcOp& lhsOp, const BcImm& rhs) const override;

    BcOp makeNil() const override;
    BcImm makeImm(bool value) const override;
    BcImm makeImm(int32_t value) const override;
    BcRef<BcImm> asImm(BcOp op) const override;

    bool isOrderable(const BcOp& vmConstOp) const override;
    bool kindEquals(const BcOp& lhsOp, const BcOp& rhsOp) const override;

    std::optional<bool> eq(const BcOp& lhsOp, const BcOp& rhsOp) const override;
    std::optional<bool> eq(const BcOp& lhsOp, bool rhs) const override;
    std::optional<bool> eq(const BcOp& lhsOp, int32_t rhs) const override;

    bool isArithmeticConstant(const BcOp& vmConstOp) const override;

    double asNumber(const BcOp& vmConstOp) const override;
};

} // namespace Bytecode
} // namespace Luau
