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

struct CfgInfo
{
    std::vector<uint32_t> predecessors;
    std::vector<uint32_t> predecessorsOffsets;

    std::vector<uint32_t> successors;
    std::vector<uint32_t> successorsOffsets;

    std::vector<RegisterSet> in;
    std::vector<RegisterSet> def;
    std::vector<RegisterSet> out;

    RegisterSet captured;
};

void computeCfgInfo(IrFunction& function);

struct BlockIteratorWrapper
{
    uint32_t* itBegin = nullptr;
    uint32_t* itEnd = nullptr;

    bool empty() const
    {
        return itBegin == itEnd;
    }

    size_t size() const
    {
        return size_t(itEnd - itBegin);
    }

    uint32_t* begin() const
    {
        return itBegin;
    }

    uint32_t* end() const
    {
        return itEnd;
    }
};

BlockIteratorWrapper predecessors(CfgInfo& cfg, uint32_t blockIdx);
BlockIteratorWrapper successors(CfgInfo& cfg, uint32_t blockIdx);

} // namespace CodeGen
} // namespace Luau
