// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ControlFlowGraph.h"
#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/DumpCFG.h"
#include "Luau/Parser.h"

#include "ScopedFlags.h"
#include "doctest.h"
#include "Fixture.h"

#include <string>

#ifdef _WIN32
#include <ostream> // You need this include on MSVC for IWYU reasons
#endif
#include <cstdio>
#include <initializer_list>
#include <optional>
#include <string_view>

LUAU_FASTFLAG(DebugLuauLogCFG)
LUAU_FASTFLAG(DebugLuauDumpCFGJson)
LUAU_FASTFLAG(DebugLuauFreezeArena)
LUAU_FASTFLAG(DebugLuauCFG)

using namespace Luau;
using namespace Luau::CFG;
namespace
{

template<typename T>
T* requireInst(Block* block, size_t idx)
{
    REQUIRE(idx < block->getInstructions().size());
    Instruction* inst = block->getInstructions()[idx];
    T* typed = inst->get_if<T>();
    REQUIRE(typed != nullptr);
    return typed;
}

size_t blockIndex(const ControlFlowGraph& cfg, Block* b)
{
    for (size_t i = 0; i < cfg.blocks.size(); i++)
    {
        if (cfg.blocks[i] == b)
            return i;
    }
    REQUIRE(false);
    return 0;
}

void checkSuccessors(const ControlFlowGraph& cfg, Block* block, std::initializer_list<size_t> expected)
{
    const std::vector<BlockId>& succs = block->getSuccessors();
    REQUIRE(succs.size() == expected.size());
    auto it = expected.begin();
    for (size_t i = 0; i < succs.size(); i++, ++it)
        CHECK(blockIndex(cfg, succs[i]) == *it);
}

void checkPredecessors(const ControlFlowGraph& cfg, Block* block, std::initializer_list<size_t> expected)
{
    const std::vector<BlockId>& preds = block->getPredecessors();
    REQUIRE(preds.size() == expected.size());
    auto it = expected.begin();
    for (size_t i = 0; i < preds.size(); i++, ++it)
        CHECK(blockIndex(cfg, preds[i]) == *it);
}

void checkJoin(Join* j, std::string_view def, std::initializer_list<std::string_view> operands)
{
    CHECK(j->definition->versionedName() == def);
    REQUIRE(j->operands.size() == operands.size());
    auto it = operands.begin();
    for (size_t i = 0; i < j->operands.size(); i++, ++it)
        CHECK(j->operands[i]->versionedName() == *it);
}

// `type=nullopt` denotes a truthy/falsy refinement (no type guard); `isTypeof` is
// only consulted when `type` is provided.
void checkRefine(
    Refine* r,
    std::string_view def,
    std::string_view source,
    bool sense,
    std::optional<std::string_view> type = std::nullopt,
    bool isTypeof = false
)
{
    CHECK(r->definition->versionedName() == def);
    CHECK(r->toRefine->versionedName() == source);
    CHECK(r->sense == sense);
    if (type)
    {
        REQUIRE(r->type.has_value());
        CHECK(*r->type == *type);
        CHECK(r->isTypeof == isTypeof);
    }
    else
    {
        CHECK(!r->type.has_value());
    }
}

} // namespace

struct CFGFixture
{
    ScopedFastFlag freezeArena{FFlag::DebugLuauFreezeArena, true};
    Allocator allocator;
    AstNameTable names{allocator};
    CFGAllocator cfgAllocator;
    AstStatBlock* root = nullptr;

    AstStatBlock* parse(const std::string& code)
    {
        ParseResult result = Parser::parse(code.c_str(), code.size(), names, allocator);
        if (!result.errors.empty())
            throw ParseErrors(std::move(result.errors));
        return result.root;
    }

    std::unique_ptr<ControlFlowGraph> build(const std::string& code)
    {
        root = parse(code);
        auto cfg = CFGBuilder::makeCFG(NotNull{&cfgAllocator}, root);
        if (FFlag::DebugLuauLogCFG)
            printf("%s", dumpCFG(*cfg).c_str());
        if (FFlag::DebugLuauDumpCFGJson)
            printf("%s\n", dumpCFGJson(*cfg).c_str());
        return cfg;
    }

    // Looks up the AstExpr at `pos` and returns the def the CFG recorded for it.
    // Use this to assert which def reaches a particular use of a variable.
    Definition* getDefinitionAtPos(const ControlFlowGraph& cfg, Position pos)
    {
        REQUIRE(root != nullptr);
        AstNode* node = findNodeAtPosition(root, pos);
        REQUIRE(node != nullptr);
        AstExpr* expr = node->asExpr();
        REQUIRE(expr != nullptr);
        Definition* def = cfg.getUseDef(expr);
        REQUIRE(def != nullptr);
        return def;
    }
};

// Asserts that the use at `pos` resolves to the def named `expected` (e.g. "a-0").
// Requires `cfg` (a ControlFlowGraph& or unique_ptr) to be in scope.
#define CHECK_REACHING_DEF(pos, expected) CHECK(getDefinitionAtPos(*cfg, (pos))->versionedName() == (expected))

TEST_SUITE_BEGIN("CFGConstruction");

TEST_CASE_FIXTURE(CFGFixture, "single_local")
{
    auto cfg = build(R"(
        local x = 4
    )");

    REQUIRE(cfg->blocks.size() == 1);
    Block* entry = cfg->blocks[0];
    CHECK(entry->kind == BlockKind::Entry);
    REQUIRE(entry->getInstructions().size() == 1);

    auto* decl = requireInst<Declare>(entry, 0);
    CHECK(decl->def->versionedName() == "x-0");
}

TEST_CASE_FIXTURE(CFGFixture, "two_locals")
{
    auto cfg = build(R"(
        local x = 4
        local y = 5
    )");

    REQUIRE(cfg->blocks.size() == 1);
    Block* entry = cfg->blocks[0];

    auto* declX = requireInst<Declare>(entry, 0);
    CHECK(declX->def->versionedName() == "x-0");
    auto* declY = requireInst<Declare>(entry, 1);
    CHECK(declY->def->versionedName() == "y-0");
}

TEST_CASE_FIXTURE(CFGFixture, "simple_reassignment")
{
    auto cfg = build(R"(
        local x = 4
        x = 5
    )");

    REQUIRE(cfg->blocks.size() == 1);
    Block* entry = cfg->blocks[0];

    auto* decl = requireInst<Declare>(entry, 0);
    CHECK(decl->def->versionedName() == "x-0");
    auto* assign = requireInst<Assign>(entry, 1);
    CHECK(assign->def->versionedName() == "x-1");
}

TEST_CASE_FIXTURE(CFGFixture, "reassignment_from_local")
{
    auto cfg = build(R"(
        local x = 1
        local y = 2
        x = y
    )");

    REQUIRE(cfg->blocks.size() == 1);
    Block* entry = cfg->blocks[0];

    CHECK(requireInst<Declare>(entry, 0)->def->versionedName() == "x-0");
    CHECK(requireInst<Declare>(entry, 1)->def->versionedName() == "y-0");
    CHECK(requireInst<Assign>(entry, 2)->def->versionedName() == "x-1");
}

TEST_CASE_FIXTURE(CFGFixture, "multi_assignment")
{
    auto cfg = build(R"(
        local a, b = 1, 2
        a, b = b, a
    )");

    REQUIRE(cfg->blocks.size() == 1);
    Block* entry = cfg->blocks[0];

    CHECK(requireInst<Declare>(entry, 0)->def->versionedName() == "a-0");
    CHECK(requireInst<Declare>(entry, 1)->def->versionedName() == "b-0");
    // RHS of `a, b = b, a` is evaluated left-to-right against pre-existing defs:
    CHECK(requireInst<Assign>(entry, 2)->def->versionedName() == "a-1");
    CHECK(requireInst<Assign>(entry, 3)->def->versionedName() == "b-1");

    // RHS reads happen before any LHS rebinds, so `b` reaches b-0 and `a` reaches a-0.
    CHECK_REACHING_DEF(Position(2, 15), "b-0");
    CHECK_REACHING_DEF(Position(2, 18), "a-0");
}

TEST_CASE_FIXTURE(CFGFixture, "basic_join")
{
    auto cfg = build(R"(
        local t = 8
        if true then
            t = 9
        else
            t = "hello"
        end
        local y = t
    )");

    REQUIRE(cfg->blocks.size() == 4);
    Block* entry = cfg->blocks[0];
    Block* thenBlk = cfg->blocks[1];
    Block* elseBlk = cfg->blocks[2];
    Block* merge = cfg->blocks[3];

    CHECK(entry->kind == BlockKind::Entry);
    CHECK(thenBlk->kind == BlockKind::Linear);
    CHECK(elseBlk->kind == BlockKind::Linear);
    CHECK(merge->kind == BlockKind::Linear);

    checkSuccessors(*cfg, entry, {1, 2});
    checkSuccessors(*cfg, thenBlk, {3});
    checkSuccessors(*cfg, elseBlk, {3});
    checkPredecessors(*cfg, merge, {1, 2});

    CHECK(requireInst<Declare>(entry, 0)->def->versionedName() == "t-0");
    CHECK(requireInst<Assign>(thenBlk, 0)->def->versionedName() == "t-1");
    CHECK(requireInst<Assign>(elseBlk, 0)->def->versionedName() == "t-2");

    auto* phi = requireInst<Join>(merge, 0);
    checkJoin(phi, "t-3", {"t-1", "t-2"});

    auto* declY = requireInst<Declare>(merge, 1);
    CHECK(declY->def->versionedName() == "y-0");
}

TEST_CASE_FIXTURE(CFGFixture, "while_loop")
{
    auto cfg = build(R"(
        local x = nil
        while not x do
            x = 5
        end
        local y = x
    )");

    REQUIRE(cfg->blocks.size() == 4);
    Block* entry = cfg->blocks[0];
    Block* header = cfg->blocks[1];
    Block* body = cfg->blocks[2];
    Block* exit = cfg->blocks[3];

    CHECK(entry->kind == BlockKind::Entry);
    CHECK(header->kind == BlockKind::Condition);
    CHECK(body->kind == BlockKind::Linear);
    CHECK(exit->kind == BlockKind::Linear);

    checkSuccessors(*cfg, entry, {1});
    checkSuccessors(*cfg, header, {2, 3});
    checkSuccessors(*cfg, body, {1});
    // Header's predecessors are entry (forward) and body (back-edge).
    checkPredecessors(*cfg, header, {0, 2});

    CHECK(requireInst<Declare>(entry, 0)->def->versionedName() == "x-0");

    // The header block has two predecessors - the loop block and the entry block
    auto* phi = requireInst<Join>(header, 0);
    checkJoin(phi, "x-1", {"x-0", "x-3"});

    // `not x` truthy means x is falsy — body sigma takes the falsy refinement.
    auto* bodyRefine = requireInst<Refine>(body, 0);
    checkRefine(bodyRefine, "x-2", "x-1", /*sense*/ false);
    CHECK(requireInst<Assign>(body, 1)->def->versionedName() == "x-3");

    // `not x` falsy on exit means x is truthy.
    auto* exitRefine = requireInst<Refine>(exit, 0);
    checkRefine(exitRefine, "x-4", "x-1", /*sense*/ true);
    CHECK(requireInst<Declare>(exit, 1)->def->versionedName() == "y-0");
}

TEST_CASE_FIXTURE(CFGFixture, "call_expression_records_uses")
{
    auto cfg = build(R"(
        local f = nil
        local x = 1
        local y = f(x)
    )");

    REQUIRE(cfg->blocks.size() == 1);
    // f and x are read on the RHS of `local y = f(x)`
    CHECK_REACHING_DEF(Position(3, 18), "f-0");
    CHECK_REACHING_DEF(Position(3, 20), "x-0");
}

TEST_CASE_FIXTURE(CFGFixture, "grouped_expression_records_use")
{
    auto cfg = build(R"(
        local x = 1
        local y = (x)
    )");

    REQUIRE(cfg->blocks.size() == 1);
    CHECK_REACHING_DEF(Position(2, 19), "x-0");
}

TEST_CASE_FIXTURE(CFGFixture, "trivial_phi_if_else_unmodified")
{
    auto cfg = build(R"(
        local x = 1
        if true then
            local y = 2
        else
            local z = 3
        end
        local w = x
    )");

    // x is never modified on either branch, so the phi at the merge is trivial.
    // The read of x in `local w = x` should resolve directly to x-0.
    CHECK_REACHING_DEF(Position(7, 18), "x-0");
}

TEST_CASE_FIXTURE(CFGFixture, "trivial_phi_while_loop_unmodified")
{
    auto cfg = build(R"(
        local x = 1
        while true do
            local y = x
        end
    )");

    // x is never modified in the loop body, so the loop header phi is trivial.
    // The read of x inside the loop should resolve directly to x-0.
    CHECK_REACHING_DEF(Position(3, 22), "x-0");
}

TEST_CASE_FIXTURE(CFGFixture, "nontrivial_phi_one_branch_modifies")
{
    auto cfg = build(R"(
        local x = 1
        if true then
            x = 2
        end
        local w = x
    )");

    // x is modified in the then branch (x-1) but not else (x-0).
    // The phi is non-trivial — the read should resolve to the phi's def, not x-0.
    Block* merge = cfg->blocks[3];
    auto* phi = requireInst<Join>(merge, 0);
    checkJoin(phi, "x-2", {"x-1", "x-0"});
    CHECK_REACHING_DEF(Position(5, 18), "x-2");
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("CFGRefinement");

TEST_CASE_FIXTURE(CFGFixture, "if_truthy_both_branches")
{
    auto cfg = build(R"(
        local x = nil
        if x then
            local y = x
        else
            local z = x
        end

        local y = x
    )");

    REQUIRE(cfg->blocks.size() == 4);
    Block* thenBlk = cfg->blocks[1];
    Block* elseBlk = cfg->blocks[2];
    Block* merge = cfg->blocks[3];

    checkRefine(requireInst<Refine>(thenBlk, 0), "x-1", "x-0", /*sense*/ true);
    CHECK(requireInst<Declare>(thenBlk, 1)->def->versionedName() == "y-0");

    checkRefine(requireInst<Refine>(elseBlk, 0), "x-2", "x-0", /*sense*/ false);
    CHECK(requireInst<Declare>(elseBlk, 1)->def->versionedName() == "z-0");

    auto* phi = requireInst<Join>(merge, 0);
    checkJoin(phi, "x-3", {"x-1", "x-2"});
    CHECK(requireInst<Declare>(merge, 1)->def->versionedName() == "y-0");
}

TEST_CASE_FIXTURE(CFGFixture, "if_falsy_single_branch")
{
    auto cfg = build(R"(
        local x = nil
        if not x then
            local y = x
        end
        local z = x
    )");

    REQUIRE(cfg->blocks.size() == 4);
    Block* thenBlk = cfg->blocks[1];
    Block* elseBlk = cfg->blocks[2];
    Block* merge = cfg->blocks[3];

    // `not x` truthy → x is falsy in the then branch.
    checkRefine(requireInst<Refine>(thenBlk, 0), "x-1", "x-0", /*sense*/ false);
    // Negation flips on the else side — x is truthy.
    checkRefine(requireInst<Refine>(elseBlk, 0), "x-2", "x-0", /*sense*/ true);

    auto* phi = requireInst<Join>(merge, 0);
    checkJoin(phi, "x-3", {"x-1", "x-2"});
}

TEST_CASE_FIXTURE(CFGFixture, "typeof_guard_emits_type_proposition")
{
    auto cfg = build(R"(
        local x = nil
        if typeof(x) == "string" then
            local y = x
        end
    )");

    REQUIRE(cfg->blocks.size() == 4);
    Block* thenBlk = cfg->blocks[1];
    Block* elseBlk = cfg->blocks[2];

    checkRefine(requireInst<Refine>(thenBlk, 0), "x-1", "x-0", /*sense*/ true, "string", /*isTypeof*/ true);
    checkRefine(requireInst<Refine>(elseBlk, 0), "x-2", "x-0", /*sense*/ false, "string", /*isTypeof*/ true);
}

TEST_CASE_FIXTURE(CFGFixture, "dump_renders_type_guard_as_a_call")
{
    // Regression test for a formatting bug in DumpCFG.cpp's dumpRefinement():
    // a `typeof(x) == "string"` guard must be rendered with call syntax
    // `typeof(x-0) == "string"`, not as the malformed `x-0 typeof == "string"`.
    auto cfg = build(R"(
        local x = nil
        if typeof(x) == "string" then
            local y = x
        end
    )");

    std::string dump = dumpCFG(*cfg);

    // The well-formed rendering puts the guard name before the variable, as a call.
    CHECK_MESSAGE(
        dump.find("typeof(x-0) == \"string\"") != std::string::npos,
        "dumpCFG produced malformed refinement text:\n",
        dump
    );

    // And it must NOT contain the broken juxtaposition where the variable precedes the guard.
    CHECK(dump.find("x-0 typeof") == std::string::npos);
}

TEST_CASE_FIXTURE(CFGFixture, "type_guard_inequality_flips_sense")
{
    auto cfg = build(R"(
        local x = nil
        if type(x) ~= "string" then
            local y = x
        end
    )");

    REQUIRE(cfg->blocks.size() == 4);
    Block* thenBlk = cfg->blocks[1];
    Block* elseBlk = cfg->blocks[2];

    checkRefine(requireInst<Refine>(thenBlk, 0), "x-1", "x-0", /*sense*/ false, "string", /*isTypeof*/ false);
    checkRefine(requireInst<Refine>(elseBlk, 0), "x-2", "x-0", /*sense*/ true, "string", /*isTypeof*/ false);
}

TEST_CASE_FIXTURE(CFGFixture, "conjunction_emits_flow_per_side")
{
    auto cfg = build(R"(
        local x = nil
        local y = nil
        if x and y then
            local z = x
        end
    )");

    REQUIRE(cfg->blocks.size() == 4);
    Block* thenBlk = cfg->blocks[1];

    checkRefine(requireInst<Refine>(thenBlk, 0), "x-1", "x-0", /*sense*/ true);
    checkRefine(requireInst<Refine>(thenBlk, 1), "y-1", "y-0", /*sense*/ true);
    CHECK(requireInst<Declare>(thenBlk, 2)->def->versionedName() == "z-0");

    // Falsy side of conjunction is a disjunction (~x \/ ~y) which doesn't decompose yet
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("CFGTypeCheckTest");

TEST_CASE_FIXTURE(Fixture, "is_truthy_constraint")
{
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local v : string?
if v then
    local s = v
else
    local s = v
end
)");
    CHECK_EQ("string", toString(requireTypeAtPosition({3, 14})));
    CHECK_EQ("nil", toString(requireTypeAtPosition({5, 14})));
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "invert_is_truthy_constraint")
{
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local v : string?
if not v then
    local s = v
else
    local s = v
end
)");
    CHECK_EQ("nil", toString(requireTypeAtPosition({3, 14})));
    CHECK_EQ("string", toString(requireTypeAtPosition({5, 14})));
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "parenthesized_expressions_are_followed_through")
{
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local v : string?
if (not v) then
    local s = v
else
    local s = v
end
)");
    CHECK_EQ("nil", toString(requireTypeAtPosition({3, 14})));
    CHECK_EQ("string", toString(requireTypeAtPosition({5, 14})));
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "and_constraint")
{
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local a : string?
local b : number?
if a and b then
    local x = a
    local y = b
else
    local x = a
    local y = b
end
)");
    CHECK_EQ("string", toString(requireTypeAtPosition({4, 14})));
    CHECK_EQ("number", toString(requireTypeAtPosition({5, 14})));
    CHECK_EQ("string?", toString(requireTypeAtPosition({7, 14})));
    CHECK_EQ("number?", toString(requireTypeAtPosition({8, 14})));
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "not_and_constraint")
{
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local a : string?
local b : number?
if not (a and b) then
    local x = a
    local y = b
else
    local x = a
    local y = b
end
)");
    CHECK_EQ("string?", toString(requireTypeAtPosition({4, 14})));
    CHECK_EQ("number?", toString(requireTypeAtPosition({5, 14})));
    CHECK_EQ("string", toString(requireTypeAtPosition({7, 14})));
    CHECK_EQ("number", toString(requireTypeAtPosition({8, 14})));
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "is_truthy_while_loop")
{
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local v : string?
while v do
    local s = v
end
)");
    CHECK_EQ("string", toString(requireTypeAtPosition({3, 14})));
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "invert_is_truthy_while_loop")
{
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local v : string?
while not v do
    local s = v
end
)");
    CHECK_EQ("nil", toString(requireTypeAtPosition({3, 14})));
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "assert_truthy")
{
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local foo : string?
assert(foo)
local bar : string = foo
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "assert_truthy_then_type_guard")
{
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local a : (number | string)?
assert(a)
local b = a
assert(type(a) == "number")
local c = a
)");
    CHECK_EQ("number | string", toString(requireTypeAtPosition({3, 10})));
    CHECK_EQ("number", toString(requireTypeAtPosition({5, 10})));
    LUAU_REQUIRE_NO_ERRORS(result);
}

// Begin - stuff we aren't able to infer with the existing type inference system
TEST_CASE_FIXTURE(Fixture, "interesting_refinement")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local x : number?
if not x then
    x = 0
end
local y = x + 4
)");
    CHECK_EQ("number", toString(requireTypeAtPosition({5, 10})));
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "while_back_edge")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local x = 42
local e = 0
while e > 1 do
  local y = x
  x = "foo"
end
)");

    CHECK_EQ("number | string", toString(requireTypeAtPosition({4, 12})));
    LUAU_REQUIRE_NO_ERRORS(result);
}

// End - stuff we aren't able to infer with the existing type inference system
TEST_SUITE_END();
