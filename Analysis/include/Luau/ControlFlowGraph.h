// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/DenseHash.h"
#include "Luau/NotNull.h"
#include "Luau/Symbol.h"
#include "Luau/TypedAllocator.h"
#include "Luau/Variant.h"

#include <memory>
#include <optional>
#include <unordered_set>
#include <utility>

using namespace Luau;

namespace CFG
{

// The control flow graph is a layer over the existing AST that models
// explicit control flow for a single file of luau code
// The CFG maintains an SSA mapping from a source AST to definition version

struct Block;
struct SymDef;
struct CFGAllocator;
using Definition = SymDef;

struct Declare;
struct Assign;
struct Join;
struct Refine;
using Instruction = Variant<Declare, Assign, Join, Refine>;

using BlockId = NotNull<Block>;
using DefId = NotNull<Definition>;
using InstrId = NotNull<Instruction>;

namespace CFGRefinement
{

struct Proposition;
struct Conjunction;
struct Disjunction;
struct Negation;

using Refinement = Variant<Conjunction, Disjunction, Negation, Proposition>;
using RefinementId = NotNull<Refinement>;

// Proposition about a def:
//   - if type is not nil: sense = true => ptr == type, sense = false => ptr ~- type
//   - `type` nullopt: `def` & truthy when sense, `def` & falsy when !sense
struct Proposition
{
    DefId ptr;
    std::optional<std::string> type;
    // Distinguishes between type and typeof
    bool isTypeof;
    bool sense;
};

struct Conjunction
{
    RefinementId lhs;
    RefinementId rhs;
};

struct Disjunction
{
    RefinementId lhs;
    RefinementId rhs;
};

struct Negation
{
    RefinementId refinement;
};

template<typename T>
const T* get(RefinementId r)
{
    return get_if<T>(r.get());
}

struct RefinementArena
{
    RefinementId proposition(DefId def, bool sense);
    RefinementId typeProposition(DefId def, std::optional<std::string> type, bool isTypeof, bool sense);

    RefinementId conjunction(RefinementId lhs, RefinementId rhs);
    RefinementId disjunction(RefinementId lhs, RefinementId rhs);
    RefinementId negation(RefinementId r);

    void freeze();

private:
    TypedAllocator<Refinement> allocator;
};

} // namespace CFGRefinement

// A versioned identity for a local variable at a specific point in the program
struct SymDef
{
    // Use a Symbol because this can wrap both globals and locals
    Symbol sym;
    size_t version;

    explicit SymDef(Symbol sym, size_t version)
        : sym(sym)
        , version(version)
    {
    }

    std::string name() const
    {
        return sym.c_str();
    }

    std::string versionedName() const
    {
        return name() + "-" + std::to_string(version);
    }

    bool operator==(const SymDef& other) const
    {
        return sym == other.sym && version == other.version;
    }

    bool operator!=(const SymDef& other) const
    {
        return !(*this == other);
    }
};

// Declarations turn into one declaration per line
// E.g. local x, y, z ...
// local x_i
// local y_j
// local z_k
// Each of these are defs
struct Declare
{
    Declare(DefId def, AstStatLocal* source)
        : def(def)
        , source(source)
    {
    }

    DefId def;
    AstStatLocal* source;
};

// Assignments turn into one 're-def' per line
// E.g. x, y, z = ....
// x_i .
// y_j .
// z_k .
struct Assign
{
    Assign(DefId def, AstStatAssign* source)
        : def(def)
        , source(source)
    {
    }

    DefId def;
    AstStatAssign* source;
};

// phi nodes - When multiple control flow paths hit this point, this denotes that
// a definition is influenced by multiple distinct control flow paths.
struct Join
{
    explicit Join(DefId definition)
        : definition(definition)
    {
    }
    DefId definition;
    std::vector<DefId> operands;
};

// Refine instructions attach type refinements to def. The attached
// Proposition tells you how to interpret the def. The Proposition is owned by
// the RefinementArena (lives inside a Refinement variant slot).
struct Refine
{
    Refine(DefId definition, NotNull<const CFGRefinement::Refinement> prop)
        : definition(definition)
        , prop(prop)
    {
    }

    DefId definition;
    NotNull<const CFGRefinement::Refinement> prop;
};

enum class BlockKind
{
    Entry,
    Linear,
    Condition,
};

struct Block
{
    explicit Block(BlockKind kind, std::string debugName);

    void addSuccessor(Block* target);

    bool containsDefinition(Symbol sym) const;
    Definition* getReachingDefinition(Symbol sym) const;
    void setReachingDefinition(Symbol sym, DefId def);


    const std::vector<InstrId>& getInstructions() const;
    const std::vector<BlockId>& getPredecessors() const;
    const std::vector<BlockId>& getSuccessors() const;

    BlockKind kind;
    std::string debugName;

private:
    std::vector<InstrId> instructions;
    std::vector<BlockId> predecessors;
    std::vector<BlockId> successors;
    DenseHashMap<Symbol, Definition*> reachingDefinitions{Symbol{}};

    friend struct CFGBuilder;
};

struct CFGAllocator
{
    Block* newBlock(BlockKind kind, std::string debugName);

    template<typename T, typename... Args>
    InstrId newInstruction(Args&&... args)
    {
        LUAU_ASSERT(!frozen);
        Instruction* inst = instructions.allocate(T{std::forward<Args>(args)...});
        return NotNull{inst};
    }

    DefId newDefinition(Symbol sym, size_t version);
    CFGRefinement::RefinementArena refinementArena;

    void freeze();

private:
    TypedAllocator<Block> block;
    TypedAllocator<Instruction> instructions;
    TypedAllocator<Definition> defs;
    bool frozen = false;
};

struct ControlFlowGraph
{
    explicit ControlFlowGraph(NotNull<CFGAllocator> allocator)
        : allocator(allocator)
    {
    }

    // Maps each use of a local variable (AstExprLocal*) to the Definition*
    // that was live at that point in the program.
    // what 'Definition' am I referencing
    DenseHashMap<AstExpr*, Definition*> useDefs{nullptr};

    std::vector<BlockId> blocks;
    size_t entryIdx = 0;

private:
    BlockId newBlock(BlockKind kind, std::string debugName = "");
    NotNull<CFGAllocator> allocator;
    friend struct CFGBuilder;
};

struct CFGBuilder
{
    static std::unique_ptr<ControlFlowGraph> makeCFG(NotNull<CFGAllocator> allocator, AstStatBlock* block);

private:
    explicit CFGBuilder(NotNull<CFGAllocator> allocator);

    void lower(AstStat* statement);
    void lower(AstStatBlock* statement);
    void lower(AstStatLocal* local);
    void lower(AstStatAssign* assn);
    void lower(AstStatIf* statIf);
    void lower(AstStatWhile* statWhile);
    void lowerExpr(AstExpr* expression);
    void lowerExpr(AstExprLocal* local);

    // Returns a refinement tree describing the truthy interpretation of `condition`,
    // or nullopt if no refinement can be extracted. Records use->def for any reads.
    std::optional<CFGRefinement::RefinementId> resolveCondition(AstExpr* condition);

    // Walks `refinement` and emits one Refine instruction per Proposition into `block`,
    // each with a fresh def for the refined symbol.
    void emitRefineInstruction(Block* block, CFGRefinement::RefinementId refinement);

    // Allocates a fresh unsealed block. If `pred` is non-null, wires `pred -> b`.
    Block* newBlock(BlockKind kind, std::string debugName, Block* pred = nullptr);

    // Allocates an Instruction of type T and appends it to `block`.
    template<typename T, typename... Args>
    NotNull<T> emit(Block* block, Args&&... args)
    {
        InstrId inst = allocator->newInstruction<T>(std::forward<Args>(args)...);
        block->instructions.emplace_back(inst);
        return NotNull{inst->template get_if<T>()};
    }

    // Emits an incomplete phi for `sym` in `block` with a fresh def. Operands are
    // filled when `block` is sealed (see fillJoinOperands).
    Join* emitJoin(Block* block, Symbol sym);

    // Mints a fresh SymDef with a monotonically increasing version per symbol.
    DefId newDefinition(Symbol sym);

    // Def lookup for `sym` at `block`.
    // If the block is unsealed: emits an incomplete phi and returns that def, since the block might
    // be given a predecessor that would causes us to gain a new phi operand
    // If block is sealed + single predecessor: recurses into the pred.
    // If block is sealed + multiple predecessors: emits a complete phi and fills operands.
    DefId readVariable(BlockId block, Symbol sym);

    // Reads `sym` from each predecessor of `block` to populate `j->operands`,
    // then attempts to trim if the join collapses to a single def.

    void fillJoinOperands(Block* block, Join* j);

    // TODO CLI-203195: collapse phis whose operands all reduce to a single def.
    void trimTrivialJoin(Join* j);

    // Returns the next version index for `sym`; first call returns 0.
    size_t nextVersionIndex(Symbol sym);

    // True iff `seal` has been called on `b` (no more predecessors will be added).
    bool isSealed(Block* b);
    // Marks `b` as sealed and flushes any incomplete phis by filling their operands.
    void seal(Block* b);

    // RAII guard: switches currentBlock on construction, restores on destruction.
    // Read currentBlock before the scope ends to capture the exit block.
    struct BlockScope
    {
        BlockScope(CFGBuilder& builder, Block* target)
            : builder(builder)
            , saved(builder.currentBlock.get())
        {
            builder.currentBlock = NotNull{target};
        }

        ~BlockScope()
        {
            builder.currentBlock = NotNull{saved};
        }

        BlockScope(const BlockScope&) = delete;
        BlockScope& operator=(const BlockScope&) = delete;

        CFGBuilder& builder;
        Block* saved;
    };

    std::unique_ptr<ControlFlowGraph> cfg;
    NotNull<CFGAllocator> allocator;
    NotNull<Block> currentBlock;
    DenseHashSet<Block*> sealedBlocks{nullptr};
    DenseHashMap<Block*, std::unordered_set<Join*>> incompleteJoins{nullptr};
    DenseHashMap<Symbol, size_t> versionCounter{Symbol{}};
};

} // namespace CFG
