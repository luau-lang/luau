// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ControlFlowGraph.h"
#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/DumpCFG.h"
#include "Luau/Error.h"
#include "Luau/Parser.h"

#include "ScopedFlags.h"
#include "doctest.h"
#include "Fixture.h"

#include <string>

#ifdef _WIN32
#include <ostream> // You need this include on MSVC for IWYU reasons
#endif
#include <algorithm>
#include <cstdio>
#include <initializer_list>
#include <optional>
#include <string_view>
#include <vector>

LUAU_FASTFLAG(DebugLuauLogCFG)
LUAU_FASTFLAG(DebugLuauDumpCFGJson)
LUAU_FASTFLAG(DebugLuauFreezeArena)
LUAU_FASTFLAG(DebugLuauCFG)

using namespace Luau;
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

    std::vector<std::string> actual;
    actual.reserve(j->operands.size());
    for (Definition* operand : j->operands)
        actual.emplace_back(operand->versionedName());

    std::vector<std::string> expected;
    expected.reserve(operands.size());
    for (std::string_view operand : operands)
        expected.emplace_back(operand);

    std::sort(actual.begin(), actual.end());
    std::sort(expected.begin(), expected.end());
    CHECK(actual == expected);
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
    CHECK(r->getTarget()->versionedName() == source);

    if (type)
    {
        if (sense)
        {
            auto* eq = r->constraint.get_if<TypeEq>();
            REQUIRE(eq != nullptr);
            CHECK(eq->type == *type);
            CHECK(eq->isTypeOf == isTypeof);
        }
        else
        {
            auto* neq = r->constraint.get_if<TypeNeq>();
            REQUIRE(neq != nullptr);
            CHECK(neq->type == *type);
            CHECK(neq->isTypeOf == isTypeof);
        }
    }
    else
    {
        if (sense)
            CHECK(r->constraint.get_if<Truthy>() != nullptr);
        else
            CHECK(r->constraint.get_if<Falsy>() != nullptr);
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
    Block* thenBlock = cfg->blocks[1];
    Block* elseBlk = cfg->blocks[2];
    Block* merge = cfg->blocks[3];

    CHECK(entry->kind == BlockKind::Entry);
    CHECK(thenBlock->kind == BlockKind::Linear);
    CHECK(elseBlk->kind == BlockKind::Linear);
    CHECK(merge->kind == BlockKind::Linear);

    checkSuccessors(*cfg, entry, {1, 2});
    checkSuccessors(*cfg, thenBlock, {3});
    checkSuccessors(*cfg, elseBlk, {3});
    checkPredecessors(*cfg, merge, {1, 2});

    CHECK(requireInst<Declare>(entry, 0)->def->versionedName() == "t-0");
    CHECK(requireInst<Assign>(thenBlock, 0)->def->versionedName() == "t-1");
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
    checkJoin(phi, "x-1", {"x-0", "x-4"});

    // `not x` truthy means x is falsy — body sigma takes the falsy refinement.
    auto* bodyRefine = requireInst<Refine>(body, 0);
    checkRefine(bodyRefine, "x-2", "x-1", /*sense*/ false);
    CHECK(requireInst<Assign>(body, 1)->def->versionedName() == "x-4");

    // `not x` falsy on exit means x is truthy.
    auto* exitRefine = requireInst<Refine>(exit, 0);
    checkRefine(exitRefine, "x-3", "x-1", /*sense*/ true);
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
    checkJoin(phi, "x-2", {"x-0", "x-1"});
    CHECK_REACHING_DEF(Position(5, 18), "x-2");
}

TEST_CASE_FIXTURE(CFGFixture, "while_break")
{
    auto cfg = build(R"(
        local x = 1
        while true do
            if x > 0 then
                break
            end
            x = x + 1
        end
        local w = x
    )");

    // blocks[0]: Entry — declare x-0
    // blocks[1]: while-loop condition — join x-1 = (x-0, x-2)
    // blocks[2]: while-loop body
    // blocks[3]: while-loop exit — declare w-0
    // blocks[4]: then block — break edges to loopExit
    // blocks[5]: else block
    // blocks[6]: merge — assign x-2
    REQUIRE(cfg->blocks.size() == 7);

    Block* loopHeader = cfg->blocks[1];
    Block* loopExit = cfg->blocks[3];
    Block* thenBlock = cfg->blocks[4];
    Block* merge = cfg->blocks[6];

    // The break in the then-branch should NOT wire to merge — only to loopExit.
    checkSuccessors(*cfg, thenBlock, {3});
    // Merge only receives the else-branch (break prevents then from falling through).
    checkPredecessors(*cfg, merge, {5});
    // Loop exit receives from header (condition false) and break (then-branch).
    checkPredecessors(*cfg, loopExit, {1, 4});
    // Back-edge from merge to loop header.
    checkSuccessors(*cfg, merge, {1});
    checkPredecessors(*cfg, loopHeader, {0, 6});

    // Loop header phi merges entry (x-0) and back-edge (x-2).
    auto* phi = requireInst<Join>(loopHeader, 0);
    checkJoin(phi, "x-1", {"x-0", "x-2"});

    // The assignment x = x + 1 lives in the merge block.
    CHECK(requireInst<Assign>(merge, 0)->def->versionedName() == "x-2");

    // `local w = x` sees x-1 (the loop header phi) because the trivial join
    // at loopExit (both predecessors provide x-1) is eliminated.
    CHECK_REACHING_DEF(Position(8, 18), "x-1");
}

TEST_CASE_FIXTURE(CFGFixture, "while_continue")
{
    auto cfg = build(R"(
        local x = 1
        while true do
            if x > 0 then
                continue
            end
            x = x + 1
        end
        local w = x
    )");

    // blocks[0]: Entry — declare x-0
    // blocks[1]: while-loop condition — join x-1 = (x-0, x-2)
    // blocks[2]: while-loop body
    // blocks[3]: while-loop exit
    // blocks[4]: then block — continue edges to loopHeader
    // blocks[5]: else block
    // blocks[6]: merge — assign x-2
    REQUIRE(cfg->blocks.size() == 7);

    Block* loopHeader = cfg->blocks[1];
    Block* loopExit = cfg->blocks[3];
    Block* thenBlock = cfg->blocks[4];
    Block* merge = cfg->blocks[6];

    // Continue in the then-branch should edge to loopHeader, NOT merge.
    checkSuccessors(*cfg, thenBlock, {1});
    // Merge only receives the else-branch.
    checkPredecessors(*cfg, merge, {5});
    // Loop header receives from entry, continue (then-block), and back-edge (merge).
    checkPredecessors(*cfg, loopHeader, {0, 4, 6});
    // Loop exit only receives from header (no break).
    checkPredecessors(*cfg, loopExit, {1});

    // The continue path does not change x, so the header join only records the distinct incoming defs.
    auto* phi = requireInst<Join>(loopHeader, 0);
    checkJoin(phi, "x-1", {"x-0", "x-2"});

    // The assignment x = x + 1 lives in the merge block.
    CHECK(requireInst<Assign>(merge, 0)->def->versionedName() == "x-2");

    // `local w = x` resolves to x-1 (single predecessor path from loopHeader).
    CHECK_REACHING_DEF(Position(8, 18), "x-1");
}

TEST_CASE_FIXTURE(CFGFixture, "repeat_until")
{
    auto cfg = build(R"(
        local x = nil
        repeat
            x = 5
        until x
        local y = x
    )");

    // blocks[0]: Entry — declare x-0
    // blocks[1]: repeat-loop body — assign x-1
    // blocks[2]: repeat-loop condition
    // blocks[3]: repeat-loop exit — declare y-0
    // blocks[4]: condition truthy — refine x-2, exits loop
    // blocks[5]: condition falsy — refine x-3, back-edge to body
    REQUIRE(cfg->blocks.size() == 6);

    Block* entry = cfg->blocks[0];
    Block* body = cfg->blocks[1];
    Block* cond = cfg->blocks[2];
    Block* exit = cfg->blocks[3];
    Block* truthy = cfg->blocks[4];
    Block* falsy = cfg->blocks[5];

    CHECK(body->kind == BlockKind::Linear);
    CHECK(cond->kind == BlockKind::Condition);
    CHECK(exit->kind == BlockKind::Linear);

    checkSuccessors(*cfg, entry, {1});
    checkSuccessors(*cfg, body, {2});
    checkSuccessors(*cfg, cond, {4, 5});
    checkSuccessors(*cfg, truthy, {3});
    checkSuccessors(*cfg, falsy, {1});
    checkPredecessors(*cfg, body, {0, 5});
    checkPredecessors(*cfg, exit, {4});

    CHECK(requireInst<Declare>(entry, 0)->def->versionedName() == "x-0");
    CHECK(requireInst<Assign>(body, 0)->def->versionedName() == "x-1");
    checkRefine(requireInst<Refine>(truthy, 0), "x-2", "x-1", /*sense*/ true);
    checkRefine(requireInst<Refine>(falsy, 0), "x-3", "x-1", /*sense*/ false);
    CHECK(requireInst<Declare>(exit, 0)->def->versionedName() == "y-0");

    CHECK_REACHING_DEF(Position(5, 18), "x-2");
}

TEST_CASE_FIXTURE(CFGFixture, "repeat_break")
{
    auto cfg = build(R"(
        local x = 1
        repeat
            if x > 0 then
                break
            end
            x = x + 1
        until x
        local w = x
    )");

    // blocks[0]: Entry — declare x-0
    // blocks[1]: repeat-loop body — join x-1 = (x-0, x-4)
    // blocks[2]: repeat-loop condition
    // blocks[3]: repeat-loop exit — join x-5 = (x-1, x-3), declare w-0
    // blocks[4]: then block — break to exit
    // blocks[5]: else block
    // blocks[6]: merge — assign x-2
    // blocks[7]: condition truthy — refine x-3, exits loop
    // blocks[8]: condition falsy — refine x-4, back-edge to body
    REQUIRE(cfg->blocks.size() == 9);

    Block* body = cfg->blocks[1];
    Block* cond = cfg->blocks[2];
    Block* exit = cfg->blocks[3];
    Block* thenBlock = cfg->blocks[4];
    Block* merge = cfg->blocks[6];
    Block* truthy = cfg->blocks[7];
    Block* falsy = cfg->blocks[8];

    checkSuccessors(*cfg, thenBlock, {3});
    checkSuccessors(*cfg, merge, {2});
    checkSuccessors(*cfg, cond, {7, 8});
    checkSuccessors(*cfg, truthy, {3});
    checkSuccessors(*cfg, falsy, {1});

    checkPredecessors(*cfg, body, {0, 8});
    checkPredecessors(*cfg, cond, {6});
    checkPredecessors(*cfg, exit, {4, 7});

    checkJoin(requireInst<Join>(body, 0), "x-1", {"x-0", "x-4"});
    CHECK(requireInst<Assign>(merge, 0)->def->versionedName() == "x-2");
    checkRefine(requireInst<Refine>(truthy, 0), "x-3", "x-2", /*sense*/ true);
    checkRefine(requireInst<Refine>(falsy, 0), "x-4", "x-2", /*sense*/ false);
    checkJoin(requireInst<Join>(exit, 0), "x-5", {"x-1", "x-3"});
    CHECK(requireInst<Declare>(exit, 1)->def->versionedName() == "w-0");

    CHECK_REACHING_DEF(Position(8, 18), "x-5");
}

TEST_CASE_FIXTURE(CFGFixture, "repeat_continue")
{
    auto cfg = build(R"(
        local x = 1
        repeat
            if x > 0 then
                continue
            end
            x = x + 1
        until x
        local w = x
    )");

    // blocks[0]: Entry — declare x-0
    // blocks[1]: repeat-loop body — join x-1 = (x-0, x-5)
    // blocks[2]: repeat-loop condition — join x-3 = (x-1, x-2)
    // blocks[3]: repeat-loop exit — declare w-0
    // blocks[4]: then block — continue to condition
    // blocks[5]: else block
    // blocks[6]: merge — assign x-2
    // blocks[7]: condition truthy — refine x-4, exits loop
    // blocks[8]: condition falsy — refine x-5, back-edge to body
    REQUIRE(cfg->blocks.size() == 9);

    Block* body = cfg->blocks[1];
    Block* cond = cfg->blocks[2];
    Block* exit = cfg->blocks[3];
    Block* thenBlock = cfg->blocks[4];
    Block* merge = cfg->blocks[6];
    Block* truthy = cfg->blocks[7];
    Block* falsy = cfg->blocks[8];

    checkSuccessors(*cfg, thenBlock, {2});
    checkSuccessors(*cfg, merge, {2});
    checkSuccessors(*cfg, cond, {7, 8});
    checkSuccessors(*cfg, truthy, {3});
    checkSuccessors(*cfg, falsy, {1});

    checkPredecessors(*cfg, body, {0, 8});
    checkPredecessors(*cfg, cond, {4, 6});
    checkPredecessors(*cfg, exit, {7});

    checkJoin(requireInst<Join>(body, 0), "x-1", {"x-0", "x-5"});
    CHECK(requireInst<Assign>(merge, 0)->def->versionedName() == "x-2");
    checkJoin(requireInst<Join>(cond, 0), "x-3", {"x-1", "x-2"});
    checkRefine(requireInst<Refine>(truthy, 0), "x-4", "x-3", /*sense*/ true);
    checkRefine(requireInst<Refine>(falsy, 0), "x-5", "x-3", /*sense*/ false);
    CHECK(requireInst<Declare>(exit, 0)->def->versionedName() == "w-0");

    CHECK_REACHING_DEF(Position(8, 18), "x-4");
}

TEST_CASE_FIXTURE(CFGFixture, "for_loop")
{
    auto cfg = build(R"(
        local x = 0
        for i = 1, 10 do
            x = x + i
        end
        local y = x
    )");

    // blocks[0]: Entry — declare x-0
    // blocks[1]: for-loop condition (header)
    // blocks[2]: for-loop exit
    // blocks[3]: for-loop body — declare i-0, assign x-2
    REQUIRE(cfg->blocks.size() == 4);

    Block* header = cfg->blocks[1];
    Block* body = cfg->blocks[3];

    CHECK(header->kind == BlockKind::Condition);
    checkSuccessors(*cfg, header, {2, 3});
    checkPredecessors(*cfg, header, {0, 3});
    checkSuccessors(*cfg, body, {1});

    // Loop variable declared in body.
    CHECK(requireInst<Declare>(body, 0)->def->versionedName() == "i-0");

    // Header phi for x: merges entry (x-0) and back-edge (x-2).
    auto* phi = requireInst<Join>(header, 0);
    checkJoin(phi, "x-1", {"x-0", "x-2"});

    CHECK(requireInst<Assign>(body, 1)->def->versionedName() == "x-2");

    // After the loop, x resolves to the header phi.
    CHECK_REACHING_DEF(Position(5, 18), "x-1");
}

TEST_CASE_FIXTURE(CFGFixture, "for_break")
{
    auto cfg = build(R"(
        local x = 0
        for i = 1, 10 do
            if x > 0 then
                break
            end
            x = x + 1
        end
        local w = x
    )");

    // blocks[0]: Entry — declare x-0
    // blocks[1]: for-loop condition (header)
    // blocks[2]: for-loop exit
    // blocks[3]: for-loop body — declare i-0
    // blocks[4]: then block — break to exit
    // blocks[5]: else block
    // blocks[6]: merge — assign x-2
    REQUIRE(cfg->blocks.size() == 7);

    Block* header = cfg->blocks[1];
    Block* exit = cfg->blocks[2];
    Block* thenBlock = cfg->blocks[4];
    Block* merge = cfg->blocks[6];

    // Break wires then to exit, not merge.
    checkSuccessors(*cfg, thenBlock, {2});
    checkPredecessors(*cfg, merge, {5});

    // Exit receives from header (loop done) and break.
    checkPredecessors(*cfg, exit, {1, 4});
    // Header phi: x-1 = join(x-0, x-2).
    auto* phi = requireInst<Join>(header, 0);
    checkJoin(phi, "x-1", {"x-0", "x-2"});

    // `local w = x` — trivial join at exit collapses (both preds provide x-1).
    CHECK_REACHING_DEF(Position(8, 18), "x-1");
}

TEST_CASE_FIXTURE(CFGFixture, "for_in_break")
{
    auto cfg = build(R"(
        local x = 0
        local t
        for k, v in t do
            if x > 0 then
                break
            end
            x = x + 1
        end
        local w = x
    )");

    // blocks[0]: Entry — declare x-0
    // blocks[1]: for-in-loop condition (header)
    // blocks[2]: for-in-loop exit
    // blocks[3]: for-in-loop body — declare k-0, v-0
    // blocks[4]: then block — break to exit
    // blocks[5]: else block
    // blocks[6]: merge — assign x-2
    REQUIRE(cfg->blocks.size() == 7);

    Block* header = cfg->blocks[1];
    Block* exit = cfg->blocks[2];
    Block* body = cfg->blocks[3];
    Block* thenBlock = cfg->blocks[4];
    Block* merge = cfg->blocks[6];

    // Break wires then to exit, not merge.
    checkSuccessors(*cfg, thenBlock, {2});
    checkPredecessors(*cfg, merge, {5});

    // Exit receives from header and break.
    checkPredecessors(*cfg, exit, {1, 4});

    // Loop variables declared in body.
    CHECK(requireInst<Declare>(body, 0)->def->versionedName() == "k-0");
    CHECK(requireInst<Declare>(body, 1)->def->versionedName() == "v-0");

    // Header phi: x-1 = join(x-0, x-2).
    auto* phi = requireInst<Join>(header, 0);
    checkJoin(phi, "x-1", {"x-0", "x-2"});

    // `local w = x` — trivial join at exit collapses.
    CHECK_REACHING_DEF(Position(9, 18), "x-1");
}

TEST_CASE_FIXTURE(CFGFixture, "if_else_expression")
{
    auto cfg = build(R"(
        local x = nil
        local y = if x then x else x
    )");

    // blocks[0]: Entry — declare x-0
    // blocks[1]: if-else expr then — refine x-1 (truthy)
    // blocks[2]: if-else expr else — refine x-2 (falsy)
    // blocks[3]: if-else expr merge — declare y-0
    REQUIRE(cfg->blocks.size() == 4);

    Block* condBlk = cfg->blocks[0];
    Block* thenBlock = cfg->blocks[1];
    Block* elseBlk = cfg->blocks[2];
    Block* merge = cfg->blocks[3];

    checkSuccessors(*cfg, condBlk, {1, 2});
    checkPredecessors(*cfg, merge, {1, 2});

    // Then branch: x refined to truthy.
    auto* thenRefine = requireInst<Refine>(thenBlock, 0);
    checkRefine(thenRefine, "x-1", "x-0", /*sense*/ true);

    // Else branch: x refined to falsy.
    auto* elseRefine = requireInst<Refine>(elseBlk, 0);
    checkRefine(elseRefine, "x-2", "x-0", /*sense*/ false);

    // The uses of x in the then/else branches see the refined defs.
    //         local y = if x then x else x
    CHECK_REACHING_DEF(Position(2, 28), "x-1");
    CHECK_REACHING_DEF(Position(2, 35), "x-2");
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
    Block* thenBlock = cfg->blocks[1];
    Block* elseBlk = cfg->blocks[2];
    Block* merge = cfg->blocks[3];

    checkRefine(requireInst<Refine>(thenBlock, 0), "x-1", "x-0", /*sense*/ true);
    CHECK(requireInst<Declare>(thenBlock, 1)->def->versionedName() == "y-0");

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
    Block* thenBlock = cfg->blocks[1];
    Block* elseBlk = cfg->blocks[2];
    Block* merge = cfg->blocks[3];

    // `not x` truthy → x is falsy in the then branch.
    checkRefine(requireInst<Refine>(thenBlock, 0), "x-1", "x-0", /*sense*/ false);
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
    Block* thenBlock = cfg->blocks[1];
    Block* elseBlk = cfg->blocks[2];

    checkRefine(requireInst<Refine>(thenBlock, 0), "x-1", "x-0", /*sense*/ true, "string", /*isTypeof*/ true);
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
    CHECK_MESSAGE(dump.find("typeof(x-0) == \"string\"") != std::string::npos, "dumpCFG produced malformed refinement text:\n", dump);

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
    Block* thenBlock = cfg->blocks[1];
    Block* elseBlk = cfg->blocks[2];

    checkRefine(requireInst<Refine>(thenBlock, 0), "x-1", "x-0", /*sense*/ false, "string", /*isTypeof*/ false);
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

    REQUIRE(cfg->blocks.size() == 8);
    Block* thenBlock = cfg->blocks[1];
    Block* elseBlk = cfg->blocks[2];
    Block* andRhs = cfg->blocks[3];
    Block* lhsFalsy = cfg->blocks[4];
    Block* rhsFalsy = cfg->blocks[5];
    Block* rhsTruthy = cfg->blocks[6];
    Block* merge = cfg->blocks[7];

    checkSuccessors(*cfg, cfg->blocks[0], {3, 4});
    checkSuccessors(*cfg, andRhs, {5, 6});
    checkSuccessors(*cfg, lhsFalsy, {2});
    checkSuccessors(*cfg, rhsFalsy, {2});
    checkSuccessors(*cfg, rhsTruthy, {1});
    checkPredecessors(*cfg, thenBlock, {6});
    checkPredecessors(*cfg, elseBlk, {4, 5});
    checkPredecessors(*cfg, merge, {1, 2});

    checkRefine(requireInst<Refine>(andRhs, 0), "x-1", "x-0", /*sense*/ true);
    checkRefine(requireInst<Refine>(lhsFalsy, 0), "x-2", "x-0", /*sense*/ false);
    checkRefine(requireInst<Refine>(rhsFalsy, 0), "y-1", "y-0", /*sense*/ false);
    checkRefine(requireInst<Refine>(rhsTruthy, 0), "y-2", "y-0", /*sense*/ true);
    CHECK(requireInst<Declare>(thenBlock, 0)->def->versionedName() == "z-0");
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("CFGTypeCheckTest");

TEST_CASE_FIXTURE(Fixture, "is_truthy_constraint")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
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
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
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
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
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
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
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
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
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

TEST_CASE_FIXTURE(Fixture, "or_predicate_with_truthy_predicates")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local a: string?
local b: number?
if a or b then
    local x = a
    local y = b
else
    local x = a
    local y = b
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string?", toString(requireTypeAtPosition({4, 14})));
    CHECK_EQ("number?", toString(requireTypeAtPosition({5, 14})));

    CHECK_EQ("nil", toString(requireTypeAtPosition({7, 14})));
    CHECK_EQ("nil", toString(requireTypeAtPosition({8, 14})));
}

TEST_CASE_FIXTURE(Fixture, "truthy_constraint_on_properties")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local t: {x: number?}

if t.x then
    local y = t.x
    local e = y
end

local bar = t.x
)");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number", toString(requireTypeAtPosition({5, 14})));
    CHECK_EQ("number?", toString(requireType("bar")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_on_a_refined_property")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local t: {x: {y: string}?}

if t.x then
    local y = t.x.y
    local e = y
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string", toString(requireTypeAtPosition({5, 14})));
}

TEST_CASE_FIXTURE(Fixture, "a_and_b_or_a_and_c_simplified")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local a: string?
local b: number?
local c: boolean = true
if (a and b) or (a and c) then
    local baz = c
else
    local baz = c
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("boolean", toString(requireTypeAtPosition({5, 16})));
    CHECK_EQ("boolean", toString(requireTypeAtPosition({7, 16})));
}

TEST_CASE_FIXTURE(Fixture, "a_and_b_or_a_and_c")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local a: string?
local b: number?
local c: boolean = true
if (a and b) or (a and c) then
    local foo = a
    local bar = b
    local baz = c
else
    local foo = a
    local bar = b
    local baz = c
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string", toString(requireTypeAtPosition({5, 16})));
    CHECK_EQ("number?", toString(requireTypeAtPosition({6, 16})));

    // CLI-233402 - Refinements are wonky under the new solver x cfg
    // This sucks .. TODO: figure out why
    if (FFlag::DebugLuauCFG)
        CHECK_EQ("boolean", toString(requireTypeAtPosition({7, 16}))); // oh no! :(

    CHECK_EQ("string?", toString(requireTypeAtPosition({9, 16})));
    CHECK_EQ("number?", toString(requireTypeAtPosition({10, 16})));
    CHECK_EQ("boolean", toString(requireTypeAtPosition({11, 16})));
}

TEST_CASE_FIXTURE(Fixture, "type_assertion_expr_carry_its_constraints")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local a: number?
local b: string?
if (a :: any) and (b :: any) then
    local x = a
    local y = b
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number?", toString(requireTypeAtPosition({4, 14})));
    CHECK_EQ("string?", toString(requireTypeAtPosition({5, 14})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typeguard_in_if_condition_position")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local s: any
local t: unknown
if type(s) == "number" then
    local n = s
end
if type(t) == "number" then
    local n = t
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("*error-type* | number", toString(requireTypeAtPosition({4, 14})));
    CHECK_EQ("number", toString(requireTypeAtPosition({7, 14})));
}


TEST_CASE_FIXTURE(BuiltinsFixture, "refine_unknown_to_table_then_test_a_prop")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local x: unknown
if typeof(x) == "table" then
    if typeof(x.foo) == "string" then
        local y = x.foo
    end
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "and_test_table_refine_subprop")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local x: unknown
if typeof(x) == "table" and typeof(x.foo) == "string" then
        local y = x.foo
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "and_test_table_refine_subprop_mismatch")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local x: unknown
if typeof(x) == "table" and typeof(x.foo) == "string" then
        local y : number = x.foo
end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    LUAU_REQUIRE_ERROR(result, TypeMismatch);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "refine_unknown_to_table_then_test_a_nested_prop")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local x: unknown
if typeof(x) == "table" then
    if typeof(x.foo.bar) == "string" then
        local y = x.foo.bar
    end
end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const UnknownProperty* up = get<UnknownProperty>(result.errors[0]);
    REQUIRE_EQ("bar", up->key);
    REQUIRE_EQ("unknown", toString(up->table));
}

TEST_CASE_FIXTURE(Fixture, "invert_is_truthy_while_loop")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
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
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
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
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
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

TEST_CASE_FIXTURE(Fixture, "discriminate_tag")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
        type Cat = {tag: "Cat", name: string, catfood: string}
        type Dog = {tag: "Dog", name: string, dogfood: string}
        type Animal = Cat | Dog
        local animal : Animal
        if animal.tag == "Cat" then
            local cat = animal
        elseif animal.tag == "Dog" then
            local dog = animal
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Cat", toString(requireTypeAtPosition({6, 24})));
    CHECK_EQ("Dog", toString(requireTypeAtPosition({8, 24})));
}

TEST_CASE_FIXTURE(Fixture, "discriminate_tag_with_implicit_else")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
        type Cat = {tag: "Cat", name: string, catfood: string}
        type Dog = {tag: "Dog", name: string, dogfood: string}
        type Animal = Cat | Dog
        local animal : Animal
        if animal.tag == "Cat" then
            local cat = animal
        else
            local dog = animal
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Cat", toString(requireTypeAtPosition({6, 24})));
    CHECK_EQ("Dog", toString(requireTypeAtPosition({8, 24})));
}

TEST_CASE_FIXTURE(Fixture, "narrow_boolean_to_true_or_false")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
        local x : boolean
        if x then
            local foo = x
        else
            local foo = x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("true", toString(requireTypeAtPosition({3, 24})));
    CHECK_EQ("false", toString(requireTypeAtPosition({5, 24})));
}

TEST_CASE_FIXTURE(Fixture, "discriminate_on_properties_of_disjoint_tables_where_that_property_is_true_or_false")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
        type Ok<T> = { ok: true, value: T }
        type Err<E> = { ok: false, error: E }
        type Result<T, E> = Ok<T> | Err<E>
        local t : Result<number, string>
        if t.ok then
            local a = t.value
            local _ = a
        else
            local b = t.error
            local _ = b
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("Ok<number>", toString(requireTypeAtPosition({6, 22})));
    CHECK_EQ("number", toString(requireTypeAtPosition({7, 22})));
    CHECK_EQ("Err<string>", toString(requireTypeAtPosition({9, 22})));
    CHECK_EQ("string", toString(requireTypeAtPosition({10, 22})));
}

TEST_CASE_FIXTURE(Fixture, "new_functions_work")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    ScopedFastFlag sff{FFlag::DebugLuauCFG, true};
    CheckResult result = check(R"(
local function f(x: number?)
  if not x then
      x = 0
  end
  local e = x > 0
end
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}



TEST_SUITE_END();


TEST_SUITE_BEGIN("NewCFGInference");
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


TEST_CASE_FIXTURE(Fixture, "is_truthy_while_loop")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
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
// End - stuff we aren't able to infer with the existing type inference system
TEST_SUITE_END();
