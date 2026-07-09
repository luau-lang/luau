// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>

namespace Luau::CFG
{
struct Block;
struct ControlFlowGraph;
} // namespace Luau::CFG

namespace Luau
{

std::string dumpCFG(const CFG::ControlFlowGraph& cfg);

// Emits a single-line JSON representation of the CFG in the format consumed by
// mozilla-spidermonkey/iongraph. Paste the output into a file and upload it to
// the iongraph viewer here: https://mozilla-spidermonkey.github.io/iongraph/
std::string dumpCFGJson(const CFG::ControlFlowGraph& cfg);

} // namespace Luau
