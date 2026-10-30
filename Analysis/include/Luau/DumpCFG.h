// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once


#include "Luau/ControlFlowGraph.h"
#include "Luau/NotNull.h"

#include <string>

namespace Luau
{

std::string dumpCFG(const ControlFlowGraph& cfg);

// Emits a single-line JSON representation of the CFG in the format consumed by
// mozilla-spidermonkey/iongraph. Paste the output into a file and upload it to
// the iongraph viewer here: https://mozilla-spidermonkey.github.io/iongraph/
std::string dumpCFGJson(const ControlFlowGraph& cfg);

// lldb helpers — call from the debugger: `p dump(def)`pp, `p dump(instr)`, etc.
std::string dump(const Definition* def);
std::string dump(const Instruction* inst);
void dump(const Block* block);

std::string dump(SSAId def);
std::string dump(InstructionId inst);
void dump(BlockId block);

} // namespace Luau
