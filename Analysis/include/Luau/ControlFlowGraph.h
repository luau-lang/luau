// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/ControlFlow.h"
#include "Luau/DenseHash.h"
#include "Luau/HashUtil.h"
#include "Luau/NotNull.h"
#include "Luau/InsertionOrderedMap.h"
#include "Luau/Symbol.h"
#include "Luau/TypedAllocator.h"
#include "Luau/Variant.h"

#include <optional>

namespace Luau
{

// The control flow graph is a layer over the existing AST that models
// explicit control flow for a single file of luau code
// The CFG maintains an SSA mapping from a source AST to definition version
struct SymDef;
struct Definition;
struct Block;
struct CFGAllocator;

struct Declare;
struct LocalFunction;
struct GlobalFunction;

struct Arg;
struct Upvalue;

struct Assign;
struct Join;
struct Refine;
struct Dead;

using Instruction = Variant<Declare, LocalFunction, GlobalFunction, Arg, Upvalue, Assign, Join, Refine, Dead>;

using BlockId = NotNull<Block>;
using SSAId = NotNull<Definition>;
using InstructionId = NotNull<Instruction>;

struct Index
{
    Symbol source;
    AstExpr* sourceExpr = nullptr;
    std::vector<std::string> path;

    // Walks an index chain (x.y.z or x["y"]["z"]) back to a root local/global.
    // Returns nullopt if the chain has a non-static index or non-local/global root.
    static std::optional<Index> fromExpr(AstExpr* expr);
};

using LHS = Variant<Symbol, AstExpr*>;

struct LHSHash
{
    size_t operator()(const LHS& lvalue) const
    {
        size_t seed = std::hash<int>()(lvalue.index());
        if (auto* sym = lvalue.get_if<Symbol>())
            hashCombine(seed, std::hash<Symbol>()(*sym));
        else if (auto* expr = lvalue.get_if<AstExpr*>())
            hashCombine(seed, DenseHashPointer()(*expr));
        return seed;
    }
};

struct Truthy
{
    bool operator==(const Truthy&) const
    {
        return true;
    }
};

struct Falsy
{
    bool operator==(const Falsy&) const
    {
        return true;
    }
};

struct TypeEq
{
    std::string type;
    bool isTypeOf;

    bool operator==(const TypeEq& other) const
    {
        return type == other.type && isTypeOf == other.isTypeOf;
    }
};

struct TypeNeq
{
    std::string type;
    bool isTypeOf;

    bool operator==(const TypeNeq& other) const
    {
        return type == other.type && isTypeOf == other.isTypeOf;
    }
};

struct StringSingletonValue
{
    std::string value;

    bool operator==(const StringSingletonValue& other) const
    {
        return value == other.value;
    }
};

struct BooleanSingletonValue
{
    bool value;

    bool operator==(const BooleanSingletonValue& other) const
    {
        return value == other.value;
    }
};

struct NilSingletonValue
{
    bool operator==(const NilSingletonValue& other) const
    {
        return true;
    }
};

using SingletonValue = Variant<StringSingletonValue, BooleanSingletonValue, NilSingletonValue>;

struct SingletonEq
{
    SingletonValue v;

    bool operator==(const SingletonEq& other) const
    {
        return v == other.v;
    }
};

struct SingletonNeq
{
    SingletonValue v;
    bool operator==(const SingletonNeq& other) const
    {
        return v == other.v;
    }
};

using LeafConstraint = Variant<Truthy, Falsy, TypeEq, TypeNeq, SingletonEq, SingletonNeq>;

struct HasShape
{
    std::vector<std::string> path;
    LeafConstraint constraint;

    bool operator==(const HasShape& other) const
    {
        return path == other.path && constraint == other.constraint;
    }
};

using NarrowingConstraint = Variant<Truthy, Falsy, TypeEq, TypeNeq, SingletonEq, SingletonNeq, HasShape>;

} // namespace Luau

template<>
struct std::hash<Luau::NarrowingConstraint>
{
    size_t operator()(const Luau::NarrowingConstraint& c) const
    {
        size_t seed = std::hash<int>()(c.index());
        auto hashSingletonValue = [&seed](const Luau::SingletonValue& value)
        {
            Luau::hashCombine(seed, std::hash<int>()(value.index()));
            if (auto* stringValue = value.get_if<Luau::StringSingletonValue>())
                Luau::hashCombine(seed, std::hash<std::string>()(stringValue->value));
            else if (auto* booleanValue = value.get_if<Luau::BooleanSingletonValue>())
                Luau::hashCombine(seed, std::hash<bool>()(booleanValue->value));
        };

        if (auto* eq = c.get_if<Luau::TypeEq>())
        {
            Luau::hashCombine(seed, std::hash<std::string>()(eq->type));
            Luau::hashCombine(seed, std::hash<bool>()(eq->isTypeOf));
        }
        else if (auto* neq = c.get_if<Luau::TypeNeq>())
        {
            Luau::hashCombine(seed, std::hash<std::string>()(neq->type));
            Luau::hashCombine(seed, std::hash<bool>()(neq->isTypeOf));
        }
        else if (auto eq = c.get_if<Luau::SingletonEq>())
        {
            hashSingletonValue(eq->v);
        }
        else if (auto* neq = c.get_if<Luau::SingletonNeq>())
        {
            hashSingletonValue(neq->v);
        }
        else if (auto* hs = c.get_if<Luau::HasShape>())
        {
            for (const auto& p : hs->path)
                Luau::hashCombine(seed, std::hash<std::string>()(p));
            Luau::hashCombine(seed, std::hash<int>()(hs->constraint.index()));
            if (auto* eq = hs->constraint.get_if<Luau::SingletonEq>())
                hashSingletonValue(eq->v);
            else if (auto* neq = hs->constraint.get_if<Luau::SingletonNeq>())
                hashSingletonValue(neq->v);
        }
        return seed;
    }
};

namespace Luau
{

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

struct Definition
{
    explicit Definition(SymDef&& d)
        : d(std::move(d))
    {
    }
    std::string versionedName() const
    {
        return d.versionedName();
    }

    std::string name() const
    {
        return d.name();
    }

    Symbol getSym() const
    {
        return d.sym;
    }

    void redirect(Definition* other)
    {
        this->follow = other;
    }

    Definition* materialize()
    {
        Definition* curr = this;
        while (curr->follow != nullptr)
            curr = curr->follow;
        return curr;
    }

private:
    SymDef d;
    Definition* follow = nullptr;
};

// v1 := Declare
// Declare instructions observe a `local/const ...` statement and introduce a new SSA definition for the declared variable.
struct Declare
{
    explicit Declare(SSAId def)
        : def(def)
    {
    }

    SSAId def;
};

// v1 := LocalFunction
// Local functions introduce a new definition for their name's domain, but unlike
// Declare they have no rhs expression: the function's type is produced by the
// ConstraintGenerator, which owns and binds the domain placeholder directly.
struct LocalFunction
{
    explicit LocalFunction(SSAId def)
        : def(def)
    {
    }

    SSAId def;
};

// v1 := GlobalFunction
// Global functions introduce a new definition for their name's domain, but unlike
// Declare they have no rhs expression: the function's type is produced by the
// ConstraintGenerator, which owns and binds the domain placeholder directly.
struct GlobalFunction
{
    explicit GlobalFunction(SSAId def)
        : def(def)
    {
    }

    SSAId def;
};

// Function parameters introduce a new definition for their name's domain like a
// Declare, but their initial value has no rhs expression: the ConstraintGenerator
// binds it from the function signature. The parameter remains a mutable local, so
// its domain can still be extended by later assignments.
struct Arg
{
    explicit Arg(SSAId def)
        : def(def)
    {
    }

    SSAId def;
};

// def := Upvalue symbol
// Upvalues are captured values in functions that must not participate in type stating.
// They are introduced as SSA definitions for the captured variable.
struct Upvalue
{
    explicit Upvalue(SSAId def)
        : def(def)
    {
    }
    SSAId def;
    Symbol s;
};


// def := Assign
// Assign instructions observe the result of assignments in the source ast
struct Assign
{
    explicit Assign(SSAId def)
        : def(def)
    {
    }

    SSAId def;
};

// A dead instruction is one that is eliminated from the final CFG
// Dead instructions are produced as a result of the braun SSA algorithm
// eliminating trivial phi nodes.
struct Dead
{
};

// v2 := Join(v_0, v_1)
// phi nodes - When multiple control flow paths hit this point, this denotes that
// a definition is influenced by multiple distinct control flow paths.
struct Join
{
    explicit Join(SSAId definition)
        : definition(definition)
    {
    }
    SSAId definition;
    std::vector<SSAId> operands;
};


// definition := Refine target constraint
// where constraint is a NarrowingConstraint
// e.g. v1 := Refine v0 HasShape { x = { y = { z isTruthy }}}
// in other words v1 is the result of refining v0.x.y.z and applying the `isTruthy` constraint
// to the property
struct Refine
{
    Refine(SSAId definition, SSAId target, NarrowingConstraint&& c)
        : definition(definition)
        , target(target)
        , constraint(std::move(c))
    {
    }
    SSAId getTarget() const;

    SSAId definition;
    SSAId target;
    NarrowingConstraint constraint;
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
    void setReachingDefinition(Symbol sym, SSAId def);


    const std::vector<InstructionId>& getInstructions() const;
    const std::vector<BlockId>& getPredecessors() const;
    const std::vector<BlockId>& getSuccessors() const;

    BlockKind kind;
    std::string debugName;

private:
    std::vector<InstructionId> instructions;
    std::vector<BlockId> predecessors;
    std::vector<BlockId> successors;
    DenseHashMap<Symbol, Definition*> reachingDefinitions;
    friend struct CFGBuilder;
};

struct CFGAllocator
{
    Block* newBlock(BlockKind kind, std::string debugName);

    template<typename T, typename... Args>
    InstructionId newInstruction(Args&&... args)
    {
        LUAU_ASSERT(!frozen);
        Instruction* inst = instructions.allocate(T{std::forward<Args>(args)...});
        return NotNull{inst};
    }

    SSAId newDefinition(Symbol sym);

    void freeze();

private:
    size_t nextVersionIndex(Symbol sym)
    {
        if (auto ref = versionCounter.find(sym))
        {
            *ref += 1;
            return *ref;
        }

        versionCounter[sym] = 0;
        return 0;
    }


    TypedAllocator<Block> block;
    TypedAllocator<Instruction> instructions;
    TypedAllocator<Definition> defs;
    bool frozen = false;

    DenseHashMap<Symbol, size_t> versionCounter;
};

struct ControlFlowGraph
{
    explicit ControlFlowGraph(NotNull<CFGAllocator> allocator)
        : allocator(allocator)
    {
    }

    Definition* getUseDef(AstExpr* expr) const;
    Definition* getLhsDef(const LHS& lv) const;

    std::vector<BlockId> blocks;
    std::vector<BlockId> functionBlocks;

    size_t entryIdx = 0;

    const std::vector<BlockId>& rpo() const
    {
        return rpoOrder;
    }

private:
    DenseHashMap<AstExpr*, Definition*> useDefs;
    DenseHashMap<LHS, Definition*, LHSHash> lhsDefs;

    std::vector<BlockId> postOrderAtRoot(BlockId root, DenseHashSet<BlockId>& visited);
    BlockId newBlock(BlockKind kind, std::string debugName = "");
    void computeRPO();

    std::vector<BlockId> rpoOrder;
    NotNull<CFGAllocator> allocator;
    friend struct CFGBuilder;
};

struct LoweringResult
{
    LoweringResult(SSAId def, NarrowingConstraint&& c)
        : def(def)
        , constraint(std::move(c))
    {
    }

    explicit LoweringResult(SSAId def)
        : def(def)
        , constraint(std::nullopt)
    {
    }

    SSAId def;
    std::optional<NarrowingConstraint> constraint = std::nullopt;
};

struct LoweringExprContext
{
    Block* thenBlock = nullptr;
    Block* elseBlock = nullptr;
    bool inConditionalContext = false;
    bool sense = true;
};

struct CFGBuilder
{
    static std::unique_ptr<ControlFlowGraph> makeCFG(NotNull<CFGAllocator> allocator, AstStatBlock* block);


private:
    explicit CFGBuilder(NotNull<CFGAllocator> allocator);
    void finalize(AstStatBlock* root);
    void validate(AstStatBlock* root);

    ControlFlow lower(AstStat* statement);
    ControlFlow lower(AstStatBlock* block);
    ControlFlow lower(AstStatLocal* local);
    ControlFlow lower(AstStatAssign* assn);
    ControlFlow lower(AstStatIf* statIf);
    ControlFlow lower(AstStatWhile* statWhile);
    ControlFlow lower(AstStatRepeat* statRepeat);
    ControlFlow lower(AstStatBreak* statBreak);
    ControlFlow lower(AstStatContinue* statContinue);
    ControlFlow lower(AstStatReturn* statReturn);
    ControlFlow lower(AstStatExpr* stat);
    ControlFlow lower(AstStatFor* statFor);
    ControlFlow lower(AstStatForIn* statForIn);
    ControlFlow lower(AstStatCompoundAssign* compAssn);
    ControlFlow lower(AstStatFunction* statFn);
    ControlFlow lower(AstStatLocalFunction* localFn);
    ControlFlow lower(AstStatTypeAlias* typeAlias);
    ControlFlow lower(AstStatTypeFunction* typeFn);
    ControlFlow lower(AstStatDeclareGlobal* declGlobal);
    ControlFlow lower(AstStatDeclareFunction* declFn);
    ControlFlow lower(AstStatClass* declClass);
    ControlFlow lower(AstStatDeclareExternType* declExternType);
    ControlFlow lower(AstStatError* err);


    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExpr* expression);

    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprGroup* group);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprLocal* local);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprGlobal* global);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprVarargs* varargs);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprCall* call);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprIndexName* indexName);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprIndexExpr* indexExpr);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprFunction* func);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprTable* table);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprUnary* unary);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprBinary* binop);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprTypeAssertion* typeAssertion);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprIfElse* ifElse);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprInterpString* interpStr);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprInstantiate* instantiate);
    std::optional<LoweringResult> lowerExpr(LoweringExprContext& ctx, AstExprError* err);

    // Discriminates the LValue form and emits the appropriate SSA instruction.
    void lowerLValue(LoweringExprContext& ctx, AstExpr* target);

    std::optional<LoweringResult> handleAssert(LoweringExprContext& ctx, const AstExprCall& call);

    // Wires currentBlock to truthyDest/falsyDest and emits Refine instructions
    // using the LowerResult's constraint (and its inverse).
    void emitConditionalRefine(LoweringResult& result, Block* truthyDest, Block* falsyDest);

    // Allocates a fresh unsealed block. If `pred` is non-null, wires `pred -> b`.
    BlockId newUnsealedBlock(BlockKind kind, std::string debugName, Block* pred = nullptr);
    // Allocates a freshly sealed block.
    BlockId newSealedBlock(BlockKind kind, std::string debugName, Block* pred = nullptr);

    // Allocates an Instruction of type T and appends it to `block`.
    template<typename T, typename... Args>
    InstructionId emit(Block* block, Args&&... args)
    {
        InstructionId inst = allocator->newInstruction<T>(std::forward<Args>(args)...);
        block->instructions.emplace_back(inst);
        return inst;
    }

    template<typename T, typename... Args>
    SSAId emitDefHelper(Symbol symbol, Block* block, Args&&... args)
    {
        static_assert(std::is_constructible_v<T, SSAId, Args&&...>, "defining instructions take their SSAId first");
        SSAId def = newDefinition(symbol);
        emit<T>(block, def, std::forward<Args>(args)...);
        block->setReachingDefinition(symbol, def);
        return def;
    };

    std::pair<InstructionId, NotNull<Join>> emitJoin(BlockId block, Symbol sym);

    // Mints a fresh SymDef with a monotonically increasing version per symbol.
    SSAId newDefinition(Symbol sym);

    // Def lookup for `sym` at `block`.
    // If the block is unsealed: emits an incomplete phi and returns that def, since the block might
    // be given a predecessor that would causes us to gain a new phi operand
    // If block is sealed + single predecessor: recurses into the pred.
    // If block is sealed + multiple predecessors: emits a complete phi and fills operands.
    SSAId readVariable(BlockId block, Symbol sym);

    // Reads `sym` from each predecessor of `block` to populate `j->operands`,
    // then attempts to trim if the join collapses to a single def.
    SSAId fillJoinOperands(Block* block, InstructionId instr, Join* j);

    SSAId trimTrivialJoin(InstructionId inst, Join* j);

    // Returns the next version index for `sym`; first call returns 0.
    size_t nextVersionIndex(Symbol sym);

    // True iff `seal` has been called on `b` (no more predecessors will be added).
    bool isSealed(BlockId b);
    // Marks `b` as sealed and flushes any incomplete phis by filling their operands.
    void seal(BlockId b);

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

    struct LoopContext
    {
        LoopContext(BlockId loopHeader, BlockId loopExit)
            : loopHeader(loopHeader)
            , loopExit(loopExit)
        {
        }
        BlockId loopHeader;
        BlockId loopExit;
    };

    std::vector<LoopContext> loopStack;


    struct PushLoopContext
    {
        PushLoopContext(std::vector<LoopContext>& context, BlockId loopHeader, BlockId loopExit)
            : context(context)
        {
            context.emplace_back(loopHeader, loopExit);
        }

        ~PushLoopContext()
        {
            context.pop_back();
        }

        PushLoopContext(const PushLoopContext&) = delete;
        PushLoopContext& operator=(const PushLoopContext&) = delete;

        std::vector<LoopContext>& context;
    };

    LoopContext* currentLoop()
    {
        if (loopStack.empty())
        {
            return nullptr;
        }
        return &loopStack.back();
    }


    std::unique_ptr<ControlFlowGraph> cfg;
    NotNull<CFGAllocator> allocator;

    DenseHashSet<BlockId> sealedBlocks;
    DenseHashMap<BlockId, DenseHashSet<InstructionId>> incompleteJoins;
    NotNull<Block> currentBlock;
    DenseHashMap<BlockId, DenseHashSet<Symbol>> functionCaptures;
};

} // namespace Luau
