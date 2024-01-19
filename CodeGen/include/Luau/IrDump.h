// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/IrData.h"

#include <string>
#include <vector>

namespace Luau
{
namespace CodeGen
{

struct CfgInfo;

const char* getCmdName(IrCmd cmd);
const char* getBlockKindName(IrBlockKind kind);

struct IrToStringContext
{
    std::string& result;
    const std::vector<IrBlock>& blocks;
    const std::vector<IrConst>& constants;
    const CfgInfo& cfg;
};

void toString(IrToStringContext& ctx, const IrInst& inst, uint32_t index);
void toString(IrToStringContext& ctx, const IrBlock& block, uint32_t index); // Block title
void toString(IrToStringContext& ctx, IrOp op);

void toString(std::string& result, IrConst constant);
void toString(std::string& result, const BytecodeTypes& bcTypes);

void toStringDetailed(IrToStringContext& ctx, const IrBlock& block, uint32_t blockIdx, const IrInst& inst, uint32_t instIdx, bool includeUseInfo);
void toStringDetailed(
    IrToStringContext& ctx, const IrBlock& block, uint32_t blockIdx, bool includeUseInfo, bool includeCfgInfo, bool includeRegFlowInfo);

std::string toString(const IrFunction& function, bool includeUseInfo);

std::string dump(const IrFunction& function);

std::string toDot(const IrFunction& function, bool includeInst);
std::string toDotCfg(const IrFunction& function);
std::string toDotDjGraph(const IrFunction& function);

std::string dumpDot(const IrFunction& function, bool includeInst);

} // namespace CodeGen
} // namespace Luau
