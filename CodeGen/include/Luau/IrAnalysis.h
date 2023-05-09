// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <bitset>
#include <utility>
#include <vector>

#include <stdint.h>

namespace Luau
{
namespace CodeGen
{

struct IrBlock;
struct IrFunction;

void updateUseCounts(IrFunction& function);

void updateLastUseLocations(IrFunction& function);

uint32_t getNextInstUse(IrFunction& function, uint32_t targetInstIdx, uint32_t startInstIdx);

// Returns how many values are coming into the block (live in) and how many are coming out of the block (live out)
std::pair<uint32_t, uint32_t> getLiveInOutValueCount(IrFunction& function, IrBlock& block);
uint32_t getLiveInValueCount(IrFunction& function, IrBlock& block);
uint32_t getLiveOutValueCount(IrFunction& function, IrBlock& block);

struct RegisterSet
{
    std::bitset<256> regs;

    // If variadic sequence is active, we track register from which it starts
    bool varargSeq = false;
    uint8_t varargStart = 0;
};

void requireVariadicSequence(RegisterSet& sourceRs, const RegisterSet& defRs, uint8_t varargStart);

struct CfgInfo
{
    std::vector<uint32_t> predecessors;
    std::vector<uint32_t> predecessorsOffsets;

    std::vector<uint32_t> successors;
    std::vector<uint32_t> successorsOffsets;

    // VM registers that are live when the block is entered
    // Additionally, an active variadic sequence can exist at the entry of the block
    std::vector<RegisterSet> in;

    // VM registers that are defined inside the block
    // It can also contain a variadic sequence definition if that hasn't been consumed inside the block
    // Note that this means that checking 'def' set might not be enough to say that register has not been written to
    std::vector<RegisterSet> def;

    // VM registers that are coming out from the block
    // These might be registers that are defined inside the block or have been defined at the entry of the block
    // Additionally, an active variadic sequence can exist at the exit of the block
    std::vector<RegisterSet> out;

    // VM registers captured by nested closures
    // This set can never have an active variadic sequence
    RegisterSet captured;
};

void computeCfgInfo(IrFunction& function);

struct BlockIteratorWrapper
{
    const uint32_t* itBegin = nullptr;
    const uint32_t* itEnd = nullptr;

    bool empty() const
    {
        return itBegin == itEnd;
    }

    size_t size() const
    {
        return size_t(itEnd - itBegin);
    }

    const uint32_t* begin() const
    {
        return itBegin;
    }

    const uint32_t* end() const
    {
        return itEnd;
    }
};

BlockIteratorWrapper predecessors(const CfgInfo& cfg, uint32_t blockIdx);
BlockIteratorWrapper successors(const CfgInfo& cfg, uint32_t blockIdx);

} // namespace CodeGen
} // namespace Luau
