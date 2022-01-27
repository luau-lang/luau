// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Compiler.h"

#include "Luau/Parser.h"
#include "Luau/BytecodeBuilder.h"
#include "Luau/Common.h"
#include "Luau/TimeTrace.h"

#include "Builtins.h"
#include "ConstantFolding.h"
#include "TableShape.h"
#include "ValueTracking.h"

#include <algorithm>
#include <bitset>
#include <math.h>

LUAU_FASTFLAGVARIABLE(LuauCompileTableIndexOpt, false)
LUAU_FASTFLAG(LuauCompileSelectBuiltin)

namespace Luau
{

using namespace Luau::Compile;

static const uint32_t kMaxRegisterCount = 255;
static const uint32_t kMaxUpvalueCount = 200;
static const uint32_t kMaxLocalCount = 200;

CompileError::CompileError(const Location& location, const std::string& message)
    : location(location)
    , message(message)
{
}

CompileError::~CompileError() throw() {}

const char* CompileError::what() const throw()
{
    return message.c_str();
}

const Location& CompileError::getLocation() const
{
    return location;
}

// NOINLINE is used to limit the stack cost of this function due to std::string object / exception plumbing
LUAU_NOINLINE void CompileError::raise(const Location& location, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    std::string message = vformat(format, args);
    va_end(args);

    throw CompileError(location, message);
}

static BytecodeBuilder::StringRef sref(AstName name)
{
    LUAU_ASSERT(name.value);
    return {name.value, strlen(name.value)};
}

static BytecodeBuilder::StringRef sref(AstArray<char> data)
{
    LUAU_ASSERT(data.data);
    return {data.data, data.size};
}

struct Compiler
{
    struct RegScope;

    Compiler(BytecodeBuilder& bytecode, const CompileOptions& options)
        : bytecode(bytecode)
        , options(options)
        , functions(nullptr)
        , locals(nullptr)
        , globals(AstName())
        , variables(nullptr)
        , constants(nullptr)
        , tableShapes(nullptr)
    {
    }

    uint8_t getLocal(AstLocal* local)
    {
        Local* l = locals.find(local);
        LUAU_ASSERT(l);
        LUAU_ASSERT(l->allocated);

        return l->reg;
    }

    uint8_t getUpval(AstLocal* local)
    {
        for (size_t uid = 0; uid < upvals.size(); ++uid)
            if (upvals[uid] == local)
                return uint8_t(uid);

        if (upvals.size() >= kMaxUpvalueCount)
            CompileError::raise(
                local->location, "Out of upvalue registers when trying to allocate %s: exceeded limit %d", local->name.value, kMaxUpvalueCount);

        // mark local as captured so that closeLocals emits LOP_CLOSEUPVALS accordingly
        Variable* v = variables.find(local);

        if (v && v->written)
            locals[local].captured = true;

        upvals.push_back(local);

        return uint8_t(upvals.size() - 1);
    }

    bool allPathsEndWithReturn(AstStat* node)
    {
        if (AstStatBlock* stat = node->as<AstStatBlock>())
            return stat->body.size > 0 && allPathsEndWithReturn(stat->body.data[stat->body.size - 1]);
        else if (node->is<AstStatReturn>())
            return true;
        else if (AstStatIf* stat = node->as<AstStatIf>())
            return stat->elsebody && allPathsEndWithReturn(stat->thenbody) && allPathsEndWithReturn(stat->elsebody);
        else
            return false;
    }

    void emitLoadK(uint8_t target, int32_t cid)
    {
        LUAU_ASSERT(cid >= 0);

        if (cid < 32768)
        {
            bytecode.emitAD(LOP_LOADK, target, int16_t(cid));
        }
        else
        {
            bytecode.emitAD(LOP_LOADKX, target, 0);
            bytecode.emitAux(cid);
        }
    }

    uint32_t compileFunction(AstExprFunction* func)
    {
        LUAU_TIMETRACE_SCOPE("Compiler::compileFunction", "Compiler");

        if (func->debugname.value)
            LUAU_TIMETRACE_ARGUMENT("name", func->debugname.value);

        LUAU_ASSERT(!functions.contains(func));
        LUAU_ASSERT(regTop == 0 && stackSize == 0 && localStack.empty() && upvals.empty());

        RegScope rs(this);

        bool self = func->self != 0;
        uint32_t fid = bytecode.beginFunction(uint8_t(self + func->args.size), func->vararg);

        setDebugLine(func);

        if (func->vararg)
            bytecode.emitABC(LOP_PREPVARARGS, uint8_t(self + func->args.size), 0, 0);

        uint8_t args = allocReg(func, self + unsigned(func->args.size));

        if (func->self)
            pushLocal(func->self, args);

        for (size_t i = 0; i < func->args.size; ++i)
            pushLocal(func->args.data[i], uint8_t(args + self + i));

        AstStatBlock* stat = func->body;

        for (size_t i = 0; i < stat->body.size; ++i)
            compileStat(stat->body.data[i]);

        // valid function bytecode must always end with RETURN
        // we elide this if we're guaranteed to hit a RETURN statement regardless of the control flow
        if (!allPathsEndWithReturn(stat))
        {
            setDebugLineEnd(stat);
            closeLocals(0);

            bytecode.emitABC(LOP_RETURN, 0, 1, 0);
        }

        // constant folding may remove some upvalue refs from bytecode, so this puts them back
        if (options.optimizationLevel >= 1 && options.debugLevel >= 2)
            gatherConstUpvals(func);

        bytecode.setDebugFunctionLineDefined(func->location.begin.line + 1);

        if (options.debugLevel >= 1 && func->debugname.value)
            bytecode.setDebugFunctionName(sref(func->debugname));

        if (options.debugLevel >= 2 && !upvals.empty())
        {
            for (AstLocal* l : upvals)
                bytecode.pushDebugUpval(sref(l->name));
        }

        if (options.optimizationLevel >= 1)
            bytecode.foldJumps();

        bytecode.expandJumps();

        popLocals(0);

        bytecode.endFunction(uint8_t(stackSize), uint8_t(upvals.size()));

        stackSize = 0;

        Function& f = functions[func];
        f.id = fid;
        f.upvals = std::move(upvals);

        return fid;
    }

    // note: this doesn't just clobber target (assuming it's temp), but also clobbers *all* allocated registers >= target!
    // this is important to be able to support "multret" semantics due to Lua call frame structure
    bool compileExprTempMultRet(AstExpr* node, uint8_t target)
    {
        if (AstExprCall* expr = node->as<AstExprCall>())
        {
            // We temporarily swap out regTop to have targetTop work correctly...
            // This is a crude hack but it's necessary for correctness :(
            RegScope rs(this, target);
            compileExprCall(expr, target, /* targetCount= */ 0, /* targetTop= */ true, /* multRet= */ true);
            return true;
        }
        else if (AstExprVarargs* expr = node->as<AstExprVarargs>())
        {
            // We temporarily swap out regTop to have targetTop work correctly...
            // This is a crude hack but it's necessary for correctness :(
            RegScope rs(this, target);
            compileExprVarargs(expr, target, /* targetCount= */ 0, /* multRet= */ true);
            return true;
        }
        else
        {
            compileExprTemp(node, target);
            return false;
        }
    }

    // note: this doesn't just clobber target (assuming it's temp), but also clobbers *all* allocated registers >= target!
    // this is important to be able to emit code that takes fewer registers and runs faster
    void compileExprTempTop(AstExpr* node, uint8_t target)
    {
        // We temporarily swap out regTop to have targetTop work correctly...
        // This is a crude hack but it's necessary for performance :(
        // It makes sure that nested call expressions can use targetTop optimization and don't need to have too many registers
        RegScope rs(this, target + 1);
        compileExprTemp(node, target);
    }

    void compileExprVarargs(AstExprVarargs* expr, uint8_t target, uint8_t targetCount, bool multRet = false)
    {
        LUAU_ASSERT(!multRet || unsigned(target + targetCount) == regTop);

        setDebugLine(expr); // normally compileExpr sets up line info, but compileExprCall can be called directly

        bytecode.emitABC(LOP_GETVARARGS, target, multRet ? 0 : uint8_t(targetCount + 1), 0);
    }

    void compileExprSelectVararg(AstExprCall* expr, uint8_t target, uint8_t targetCount, bool targetTop, bool multRet, uint8_t regs)
    {
        LUAU_ASSERT(FFlag::LuauCompileSelectBuiltin);
        LUAU_ASSERT(targetCount == 1);
        LUAU_ASSERT(!expr->self);
        LUAU_ASSERT(expr->args.size == 2 && expr->args.data[1]->is<AstExprVarargs>());

        AstExpr* arg = expr->args.data[0];

        uint8_t argreg;

        if (isExprLocalReg(arg))
            argreg = getLocal(arg->as<AstExprLocal>()->local);
        else
        {
            argreg = uint8_t(regs + 1);
            compileExprTempTop(arg, argreg);
        }

        size_t fastcallLabel = bytecode.emitLabel();

        bytecode.emitABC(LOP_FASTCALL1, LBF_SELECT_VARARG, argreg, 0);

        // note, these instructions are normally not executed and are used as a fallback for FASTCALL
        // we can't use TempTop variant here because we need to make sure the arguments we already computed aren't overwritten
        compileExprTemp(expr->func, regs);

        bytecode.emitABC(LOP_GETVARARGS, uint8_t(regs + 2), 0, 0);

        size_t callLabel = bytecode.emitLabel();
        if (!bytecode.patchSkipC(fastcallLabel, callLabel))
            CompileError::raise(expr->func->location, "Exceeded jump distance limit; simplify the code to compile");

        // note, this is always multCall (last argument is variadic)
        bytecode.emitABC(LOP_CALL, regs, 0, multRet ? 0 : uint8_t(targetCount + 1));

        // if we didn't output results directly to target, we need to move them
        if (!targetTop)
        {
            for (size_t i = 0; i < targetCount; ++i)
                bytecode.emitABC(LOP_MOVE, uint8_t(target + i), uint8_t(regs + i), 0);
        }
    }

    void compileExprFastcallN(AstExprCall* expr, uint8_t target, uint8_t targetCount, bool targetTop, bool multRet, uint8_t regs, int bfid)
    {
        LUAU_ASSERT(!expr->self);
        LUAU_ASSERT(expr->args.size <= 2);

        LuauOpcode opc = expr->args.size == 1 ? LOP_FASTCALL1 : LOP_FASTCALL2;

        uint32_t args[2] = {};

        for (size_t i = 0; i < expr->args.size; ++i)
        {
            if (i > 0)
            {
                if (int32_t cid = getConstantIndex(expr->args.data[i]); cid >= 0)
                {
                    opc = LOP_FASTCALL2K;
                    args[i] = cid;
                    break;
                }
            }

            if (isExprLocalReg(expr->args.data[i]))
                args[i] = getLocal(expr->args.data[i]->as<AstExprLocal>()->local);
            else
            {
                args[i] = uint8_t(regs + 1 + i);
                compileExprTempTop(expr->args.data[i], uint8_t(args[i]));
            }
        }

        size_t fastcallLabel = bytecode.emitLabel();

        bytecode.emitABC(opc, uint8_t(bfid), uint8_t(args[0]), 0);
        if (opc != LOP_FASTCALL1)
            bytecode.emitAux(args[1]);

        // Set up a traditional Lua stack for the subsequent LOP_CALL.
        // Note, as with other instructions that immediately follow FASTCALL, these are normally not executed and are used as a fallback for
        // these FASTCALL variants.
        for (size_t i = 0; i < expr->args.size; ++i)
        {
            if (i > 0 && opc == LOP_FASTCALL2K)
            {
                emitLoadK(uint8_t(regs + 1 + i), args[i]);
                break;
            }

            if (args[i] != regs + 1 + i)
                bytecode.emitABC(LOP_MOVE, uint8_t(regs + 1 + i), uint8_t(args[i]), 0);
        }

        // note, these instructions are normally not executed and are used as a fallback for FASTCALL
        // we can't use TempTop variant here because we need to make sure the arguments we already computed aren't overwritten
        compileExprTemp(expr->func, regs);

        size_t callLabel = bytecode.emitLabel();

        // FASTCALL will skip over the instructions needed to compute function and jump over CALL which must immediately follow the instruction
        // sequence after FASTCALL
        if (!bytecode.patchSkipC(fastcallLabel, callLabel))
            CompileError::raise(expr->func->location, "Exceeded jump distance limit; simplify the code to compile");

        bytecode.emitABC(LOP_CALL, regs, uint8_t(expr->args.size + 1), multRet ? 0 : uint8_t(targetCount + 1));

        // if we didn't output results directly to target, we need to move them
        if (!targetTop)
        {
            for (size_t i = 0; i < targetCount; ++i)
                bytecode.emitABC(LOP_MOVE, uint8_t(target + i), uint8_t(regs + i), 0);
        }
    }

    void compileExprCall(AstExprCall* expr, uint8_t target, uint8_t targetCount, bool targetTop = false, bool multRet = false)
    {
        LUAU_ASSERT(!targetTop || unsigned(target + targetCount) == regTop);

        setDebugLine(expr); // normally compileExpr sets up line info, but compileExprCall can be called directly

        RegScope rs(this);

        unsigned int regCount = std::max(unsigned(1 + expr->self + expr->args.size), unsigned(targetCount));

        // Optimization: if target points to the top of the stack, we can start the call at oldTop - 1 and won't need MOVE at the end
        uint8_t regs = targetTop ? allocReg(expr, regCount - targetCount) - targetCount : allocReg(expr, regCount);

        uint8_t selfreg = 0;

        int bfid = -1;

        if (options.optimizationLevel >= 1)
        {
            Builtin builtin = getBuiltin(expr->func, globals, variables);
            bfid = getBuiltinFunctionId(builtin, options);
        }

        if (bfid == LBF_SELECT_VARARG)
        {
            LUAU_ASSERT(FFlag::LuauCompileSelectBuiltin);
            // Optimization: compile select(_, ...) as FASTCALL1; the builtin will read variadic arguments directly
            // note: for now we restrict this to single-return expressions since our runtime code doesn't deal with general cases
            if (multRet == false && targetCount == 1 && expr->args.size == 2 && expr->args.data[1]->is<AstExprVarargs>())
                return compileExprSelectVararg(expr, target, targetCount, targetTop, multRet, regs);
            else
                bfid = -1;
        }

        // Optimization: for 1/2 argument fast calls use specialized opcodes
        if (!expr->self && bfid >= 0 && expr->args.size >= 1 && expr->args.size <= 2)
        {
            AstExpr* last = expr->args.data[expr->args.size - 1];
            if (!last->is<AstExprCall>() && !last->is<AstExprVarargs>())
                return compileExprFastcallN(expr, target, targetCount, targetTop, multRet, regs, bfid);
        }

        if (expr->self)
        {
            AstExprIndexName* fi = expr->func->as<AstExprIndexName>();
            LUAU_ASSERT(fi);

            // Optimization: use local register directly in NAMECALL if possible
            if (isExprLocalReg(fi->expr))
            {
                selfreg = getLocal(fi->expr->as<AstExprLocal>()->local);
            }
            else
            {
                // Note: to be able to compile very deeply nested self call chains (obj:method1():method2():...), we need to be able to do this in
                // finite stack space NAMECALL will happily move object from regs to regs+1 but we need to compute it into regs so that
                // compileExprTempTop doesn't increase stack usage for every recursive call
                selfreg = regs;

                compileExprTempTop(fi->expr, selfreg);
            }
        }
        else if (bfid < 0)
        {
            compileExprTempTop(expr->func, regs);
        }

        bool multCall = false;

        for (size_t i = 0; i < expr->args.size; ++i)
            if (i + 1 == expr->args.size)
                multCall = compileExprTempMultRet(expr->args.data[i], uint8_t(regs + 1 + expr->self + i));
            else
                compileExprTempTop(expr->args.data[i], uint8_t(regs + 1 + expr->self + i));

        setDebugLineEnd(expr->func);

        if (expr->self)
        {
            AstExprIndexName* fi = expr->func->as<AstExprIndexName>();
            LUAU_ASSERT(fi);

            setDebugLine(fi->indexLocation);

            BytecodeBuilder::StringRef iname = sref(fi->index);
            int32_t cid = bytecode.addConstantString(iname);
            if (cid < 0)
                CompileError::raise(fi->location, "Exceeded constant limit; simplify the code to compile");

            bytecode.emitABC(LOP_NAMECALL, regs, selfreg, uint8_t(BytecodeBuilder::getStringHash(iname)));
            bytecode.emitAux(cid);
        }
        else if (bfid >= 0)
        {
            size_t fastcallLabel = bytecode.emitLabel();
            bytecode.emitABC(LOP_FASTCALL, uint8_t(bfid), 0, 0);

            // note, these instructions are normally not executed and are used as a fallback for FASTCALL
            // we can't use TempTop variant here because we need to make sure the arguments we already computed aren't overwritten
            compileExprTemp(expr->func, regs);

            size_t callLabel = bytecode.emitLabel();

            // FASTCALL will skip over the instructions needed to compute function and jump over CALL which must immediately follow the instruction
            // sequence after FASTCALL
            if (!bytecode.patchSkipC(fastcallLabel, callLabel))
                CompileError::raise(expr->func->location, "Exceeded jump distance limit; simplify the code to compile");
        }

        bytecode.emitABC(LOP_CALL, regs, multCall ? 0 : uint8_t(expr->self + expr->args.size + 1), multRet ? 0 : uint8_t(targetCount + 1));

        // if we didn't output results directly to target, we need to move them
        if (!targetTop)
        {
            for (size_t i = 0; i < targetCount; ++i)
                bytecode.emitABC(LOP_MOVE, uint8_t(target + i), uint8_t(regs + i), 0);
        }
    }

    bool shouldShareClosure(AstExprFunction* func)
    {
        const Function* f = functions.find(func);
        if (!f)
            return false;

        for (AstLocal* uv : f->upvals)
        {
            Variable* ul = variables.find(uv);

            if (!ul)
                return false;

            if (ul->written)
                return false;

            // it's technically safe to share closures whenever all upvalues are immutable
            // this is because of a runtime equality check in DUPCLOSURE.
            // however, this results in frequent deoptimization and increases the set of reachable objects, making some temporary objects permanent
            // instead we apply a heuristic: we share closures if they refer to top-level upvalues, or closures that refer to top-level upvalues
            // this will only deoptimize (outside of fenv changes) if top level code is executed twice with different results.
            if (uv->functionDepth != 0 || uv->loopDepth != 0)
            {
                AstExprFunction* uf = ul->init ? ul->init->as<AstExprFunction>() : nullptr;
                if (!uf)
                    return false;

                if (uf != func && !shouldShareClosure(uf))
                    return false;
            }
        }

        return true;
    }

    void compileExprFunction(AstExprFunction* expr, uint8_t target)
    {
        const Function* f = functions.find(expr);
        LUAU_ASSERT(f);

        // when the closure has upvalues we'll use this to create the closure at runtime
        // when the closure has no upvalues, we use constant closures that technically don't rely on the child function list
        // however, it's still important to add the child function because debugger relies on the function hierarchy when setting breakpoints
        int16_t pid = bytecode.addChildFunction(f->id);
        if (pid < 0)
            CompileError::raise(expr->location, "Exceeded closure limit; simplify the code to compile");

        bool shared = false;

        // Optimization: when closure has no upvalues, or upvalues are safe to share, instead of allocating it every time we can share closure
        // objects (this breaks assumptions about function identity which can lead to setfenv not working as expected, so we disable this when it
        // is used)
        if (options.optimizationLevel >= 1 && shouldShareClosure(expr) && !setfenvUsed)
        {
            int32_t cid = bytecode.addConstantClosure(f->id);

            if (cid >= 0 && cid < 32768)
            {
                bytecode.emitAD(LOP_DUPCLOSURE, target, int16_t(cid));
                shared = true;
            }
        }

        if (!shared)
            bytecode.emitAD(LOP_NEWCLOSURE, target, pid);

        for (AstLocal* uv : f->upvals)
        {
            LUAU_ASSERT(uv->functionDepth < expr->functionDepth);

            Variable* ul = variables.find(uv);
            bool immutable = !ul || !ul->written;

            if (uv->functionDepth == expr->functionDepth - 1)
            {
                // get local variable
                uint8_t reg = getLocal(uv);

                bytecode.emitABC(LOP_CAPTURE, uint8_t(immutable ? LCT_VAL : LCT_REF), reg, 0);
            }
            else
            {
                // get upvalue from parent frame
                // note: this will add uv to the current upvalue list if necessary
                uint8_t uid = getUpval(uv);

                bytecode.emitABC(LOP_CAPTURE, LCT_UPVAL, uid, 0);
            }
        }
    }

    LuauOpcode getUnaryOp(AstExprUnary::Op op)
    {
        switch (op)
        {
        case AstExprUnary::Not:
            return LOP_NOT;

        case AstExprUnary::Minus:
            return LOP_MINUS;

        case AstExprUnary::Len:
            return LOP_LENGTH;

        default:
            LUAU_ASSERT(!"Unexpected unary operation");
            return LOP_NOP;
        }
    }

    LuauOpcode getBinaryOpArith(AstExprBinary::Op op, bool k = false)
    {
        switch (op)
        {
        case AstExprBinary::Add:
            return k ? LOP_ADDK : LOP_ADD;

        case AstExprBinary::Sub:
            return k ? LOP_SUBK : LOP_SUB;

        case AstExprBinary::Mul:
            return k ? LOP_MULK : LOP_MUL;

        case AstExprBinary::Div:
            return k ? LOP_DIVK : LOP_DIV;

        case AstExprBinary::Mod:
            return k ? LOP_MODK : LOP_MOD;

        case AstExprBinary::Pow:
            return k ? LOP_POWK : LOP_POW;

        default:
            LUAU_ASSERT(!"Unexpected binary operation");
            return LOP_NOP;
        }
    }

    LuauOpcode getJumpOpCompare(AstExprBinary::Op op, bool not_ = false)
    {
        switch (op)
        {
        case AstExprBinary::CompareNe:
            return not_ ? LOP_JUMPIFEQ : LOP_JUMPIFNOTEQ;

        case AstExprBinary::CompareEq:
            return not_ ? LOP_JUMPIFNOTEQ : LOP_JUMPIFEQ;

        case AstExprBinary::CompareLt:
        case AstExprBinary::CompareGt:
            return not_ ? LOP_JUMPIFNOTLT : LOP_JUMPIFLT;

        case AstExprBinary::CompareLe:
        case AstExprBinary::CompareGe:
            return not_ ? LOP_JUMPIFNOTLE : LOP_JUMPIFLE;

        default:
            LUAU_ASSERT(!"Unexpected binary operation");
            return LOP_NOP;
        }
    }

    bool isConstant(AstExpr* node)
    {
        const Constant* cv = constants.find(node);

        return cv && cv->type != Constant::Type_Unknown;
    }

    bool isConstantTrue(AstExpr* node)
    {
        const Constant* cv = constants.find(node);

        return cv && cv->type != Constant::Type_Unknown && cv->isTruthful();
    }

    bool isConstantFalse(AstExpr* node)
    {
        const Constant* cv = constants.find(node);

        return cv && cv->type != Constant::Type_Unknown && !cv->isTruthful();
    }

    size_t compileCompareJump(AstExprBinary* expr, bool not_ = false)
    {
        RegScope rs(this);
        LuauOpcode opc = getJumpOpCompare(expr->op, not_);

        bool isEq = (opc == LOP_JUMPIFEQ || opc == LOP_JUMPIFNOTEQ);
        AstExpr* left = expr->left;
        AstExpr* right = expr->right;

        bool operandIsConstant = isConstant(right);
        if (isEq && !operandIsConstant)
        {
            operandIsConstant = isConstant(left);
            if (operandIsConstant)
                std::swap(left, right);
        }

        uint8_t rl = compileExprAuto(left, rs);
        int32_t rr = -1;

        if (isEq && operandIsConstant)
        {
            if (opc == LOP_JUMPIFEQ)
                opc = LOP_JUMPIFEQK;
            else if (opc == LOP_JUMPIFNOTEQ)
                opc = LOP_JUMPIFNOTEQK;

            rr = getConstantIndex(right);
            LUAU_ASSERT(rr >= 0);
        }
        else
            rr = compileExprAuto(right, rs);

        size_t jumpLabel = bytecode.emitLabel();

        if (expr->op == AstExprBinary::CompareGt || expr->op == AstExprBinary::CompareGe)
        {
            bytecode.emitAD(opc, uint8_t(rr), 0);
            bytecode.emitAux(rl);
        }
        else
        {
            bytecode.emitAD(opc, rl, 0);
            bytecode.emitAux(rr);
        }

        return jumpLabel;
    }

    int32_t getConstantNumber(AstExpr* node)
    {
        const Constant* c = constants.find(node);

        if (c && c->type == Constant::Type_Number)
        {
            int cid = bytecode.addConstantNumber(c->valueNumber);
            if (cid < 0)
                CompileError::raise(node->location, "Exceeded constant limit; simplify the code to compile");

            return cid;
        }

        return -1;
    }

    int32_t getConstantIndex(AstExpr* node)
    {
        const Constant* c = constants.find(node);

        if (!c)
            return -1;

        int cid = -1;

        switch (c->type)
        {
        case Constant::Type_Nil:
            cid = bytecode.addConstantNil();
            break;

        case Constant::Type_Boolean:
            cid = bytecode.addConstantBoolean(c->valueBoolean);
            break;

        case Constant::Type_Number:
            cid = bytecode.addConstantNumber(c->valueNumber);
            break;

        case Constant::Type_String:
            cid = bytecode.addConstantString(sref(c->getString()));
            break;

        default:
            LUAU_ASSERT(!"Unexpected constant type");
            return -1;
        }

        if (cid < 0)
            CompileError::raise(node->location, "Exceeded constant limit; simplify the code to compile");

        return cid;
    }

    // compile expr to target temp register
    // if the expr (or not expr if onlyTruth is false) is truthy, jump via skipJump
    // if the expr (or not expr if onlyTruth is false) is falsy, fall through (target isn't guaranteed to be updated in this case)
    // if target is omitted, then the jump behavior is the same - skipJump or fallthrough depending on the truthiness of the expression
    void compileConditionValue(AstExpr* node, const uint8_t* target, std::vector<size_t>& skipJump, bool onlyTruth)
    {
        // Optimization: we don't need to compute constant values
        const Constant* cv = constants.find(node);

        if (cv && cv->type != Constant::Type_Unknown)
        {
            // note that we only need to compute the value if it's truthy; otherwise we cal fall through
            if (cv->isTruthful() == onlyTruth)
            {
                if (target)
                    compileExprTemp(node, *target);

                skipJump.push_back(bytecode.emitLabel());
                bytecode.emitAD(LOP_JUMP, 0, 0);
            }
            return;
        }

        if (AstExprBinary* expr = node->as<AstExprBinary>())
        {
            switch (expr->op)
            {
            case AstExprBinary::And:
            case AstExprBinary::Or:
            {
                // disambiguation: there's 4 cases (we only need truthy or falsy results based on onlyTruth)
                // onlyTruth = 1: a and b transforms to a ? b : dontcare
                // onlyTruth = 1: a or b transforms to a ? a : a
                // onlyTruth = 0: a and b transforms to !a ? a : b
                // onlyTruth = 0: a or b transforms to !a ? b : dontcare
                if (onlyTruth == (expr->op == AstExprBinary::And))
                {
                    // we need to compile the left hand side, and skip to "dontcare" (aka fallthrough of the entire statement) if it's not the same as
                    // onlyTruth if it's the same then the result of the expression is the right hand side because of this, we *never* care about the
                    // result of the left hand side
                    std::vector<size_t> elseJump;
                    compileConditionValue(expr->left, nullptr, elseJump, !onlyTruth);

                    // fallthrough indicates that we need to compute & return the right hand side
                    // we use compileConditionValue again to process any extra and/or statements directly
                    compileConditionValue(expr->right, target, skipJump, onlyTruth);

                    size_t elseLabel = bytecode.emitLabel();

                    patchJumps(expr, elseJump, elseLabel);
                }
                else
                {
                    // we need to compute the left hand side first; note that we will jump to skipJump if we know the answer
                    compileConditionValue(expr->left, target, skipJump, onlyTruth);

                    // we will fall through if computing the left hand didn't give us an "interesting" result
                    // we still use compileConditionValue to recursively optimize any and/or/compare statements
                    compileConditionValue(expr->right, target, skipJump, onlyTruth);
                }
                return;
            }
            break;

            case AstExprBinary::CompareNe:
            case AstExprBinary::CompareEq:
            case AstExprBinary::CompareLt:
            case AstExprBinary::CompareLe:
            case AstExprBinary::CompareGt:
            case AstExprBinary::CompareGe:
            {
                if (target)
                {
                    // since target is a temp register, we'll initialize it to 1, and then jump if the comparison is true
                    // if the comparison is false, we'll fallthrough and target will still be 1 but target has unspecified value for falsy results
                    // when we only care about falsy values instead of truthy values, the process is the same but with flipped conditionals
                    bytecode.emitABC(LOP_LOADB, *target, onlyTruth ? 1 : 0, 0);
                }

                size_t jumpLabel = compileCompareJump(expr, /* not= */ !onlyTruth);

                skipJump.push_back(jumpLabel);
                return;
            }
            break;

            // fall-through to default path below
            default:;
            }
        }

        if (AstExprUnary* expr = node->as<AstExprUnary>())
        {
            // if we *do* need to compute the target, we'd have to inject "not" ops on every return path
            // this is possible but cumbersome; so for now we only optimize not expression when we *don't* need the value
            if (!target && expr->op == AstExprUnary::Not)
            {
                compileConditionValue(expr->expr, target, skipJump, !onlyTruth);
                return;
            }
        }

        if (AstExprGroup* expr = node->as<AstExprGroup>())
        {
            compileConditionValue(expr->expr, target, skipJump, onlyTruth);
            return;
        }

        RegScope rs(this);
        uint8_t reg;

        if (target)
        {
            reg = *target;
            compileExprTemp(node, reg);
        }
        else
        {
            reg = compileExprAuto(node, rs);
        }

        skipJump.push_back(bytecode.emitLabel());
        bytecode.emitAD(onlyTruth ? LOP_JUMPIF : LOP_JUMPIFNOT, reg, 0);
    }

    // checks if compiling the expression as a condition value generates code that's faster than using compileExpr
    bool isConditionFast(AstExpr* node)
    {
        const Constant* cv = constants.find(node);

        if (cv && cv->type != Constant::Type_Unknown)
            return true;

        if (AstExprBinary* expr = node->as<AstExprBinary>())
        {
            switch (expr->op)
            {
            case AstExprBinary::And:
            case AstExprBinary::Or:
                return true;

            case AstExprBinary::CompareNe:
            case AstExprBinary::CompareEq:
            case AstExprBinary::CompareLt:
            case AstExprBinary::CompareLe:
            case AstExprBinary::CompareGt:
            case AstExprBinary::CompareGe:
                return true;

            default:
                return false;
            }
        }

        if (AstExprGroup* expr = node->as<AstExprGroup>())
            return isConditionFast(expr->expr);

        return false;
    }

    void compileExprAndOr(AstExprBinary* expr, uint8_t target, bool targetTemp)
    {
        bool and_ = (expr->op == AstExprBinary::And);

        RegScope rs(this);

        // Optimization: when left hand side is a constant, we can emit left hand side or right hand side
        const Constant* cl = constants.find(expr->left);

        if (cl && cl->type != Constant::Type_Unknown)
        {
            compileExpr(and_ == cl->isTruthful() ? expr->right : expr->left, target, targetTemp);
            return;
        }

        // Note: two optimizations below can lead to inefficient codegen when the left hand side is a condition
        if (!isConditionFast(expr->left))
        {
            // Optimization: when right hand side is a local variable, we can use AND/OR
            if (isExprLocalReg(expr->right))
            {
                uint8_t lr = compileExprAuto(expr->left, rs);
                uint8_t rr = getLocal(expr->right->as<AstExprLocal>()->local);

                bytecode.emitABC(and_ ? LOP_AND : LOP_OR, target, lr, rr);
                return;
            }

            // Optimization: when right hand side is a constant, we can use ANDK/ORK
            int32_t cid = getConstantIndex(expr->right);

            if (cid >= 0 && cid <= 255)
            {
                uint8_t lr = compileExprAuto(expr->left, rs);

                bytecode.emitABC(and_ ? LOP_ANDK : LOP_ORK, target, lr, uint8_t(cid));
                return;
            }
        }

        // Optimization: if target is a temp register, we can clobber it which allows us to compute the result directly into it
        // If it's not a temp register, then something like `a = a > 1 or a + 2` may clobber `a` while evaluating left hand side, and `a+2` will break
        uint8_t reg = targetTemp ? target : allocReg(expr, 1);

        std::vector<size_t> skipJump;
        compileConditionValue(expr->left, &reg, skipJump, /* onlyTruth= */ !and_);

        compileExprTemp(expr->right, reg);

        size_t moveLabel = bytecode.emitLabel();

        patchJumps(expr, skipJump, moveLabel);

        if (target != reg)
            bytecode.emitABC(LOP_MOVE, target, reg, 0);
    }

    void compileExprUnary(AstExprUnary* expr, uint8_t target)
    {
        RegScope rs(this);

        uint8_t re = compileExprAuto(expr->expr, rs);

        bytecode.emitABC(getUnaryOp(expr->op), target, re, 0);
    }

    static void unrollConcats(std::vector<AstExpr*>& args)
    {
        for (;;)
        {
            AstExprBinary* be = args.back()->as<AstExprBinary>();

            if (!be || be->op != AstExprBinary::Concat)
                break;

            args.back() = be->left;
            args.push_back(be->right);
        }
    }

    void compileExprBinary(AstExprBinary* expr, uint8_t target, bool targetTemp)
    {
        RegScope rs(this);

        switch (expr->op)
        {
        case AstExprBinary::Add:
        case AstExprBinary::Sub:
        case AstExprBinary::Mul:
        case AstExprBinary::Div:
        case AstExprBinary::Mod:
        case AstExprBinary::Pow:
        {
            int32_t rc = getConstantNumber(expr->right);

            if (rc >= 0 && rc <= 255)
            {
                uint8_t rl = compileExprAuto(expr->left, rs);

                bytecode.emitABC(getBinaryOpArith(expr->op, /* k= */ true), target, rl, uint8_t(rc));
            }
            else
            {
                uint8_t rl = compileExprAuto(expr->left, rs);
                uint8_t rr = compileExprAuto(expr->right, rs);

                bytecode.emitABC(getBinaryOpArith(expr->op), target, rl, rr);
            }
        }
        break;

        case AstExprBinary::Concat:
        {
            std::vector<AstExpr*> args = {expr->left, expr->right};

            // unroll the tree of concats down the right hand side to be able to do multiple ops
            unrollConcats(args);

            uint8_t regs = allocReg(expr, unsigned(args.size()));

            for (size_t i = 0; i < args.size(); ++i)
                compileExprTemp(args[i], uint8_t(regs + i));

            bytecode.emitABC(LOP_CONCAT, target, regs, uint8_t(regs + args.size() - 1));
        }
        break;

        case AstExprBinary::CompareNe:
        case AstExprBinary::CompareEq:
        case AstExprBinary::CompareLt:
        case AstExprBinary::CompareLe:
        case AstExprBinary::CompareGt:
        case AstExprBinary::CompareGe:
        {
            size_t jumpLabel = compileCompareJump(expr);

            // note: this skips over the next LOADB instruction because of "1" in the C slot
            bytecode.emitABC(LOP_LOADB, target, 0, 1);

            size_t thenLabel = bytecode.emitLabel();

            bytecode.emitABC(LOP_LOADB, target, 1, 0);

            patchJump(expr, jumpLabel, thenLabel);
        }
        break;

        case AstExprBinary::And:
        case AstExprBinary::Or:
        {
            compileExprAndOr(expr, target, targetTemp);
        }
        break;

        default:
            LUAU_ASSERT(!"Unexpected binary operation");
        }
    }

    void compileExprIfElse(AstExprIfElse* expr, uint8_t target, bool targetTemp)
    {
        if (isConstant(expr->condition))
        {
            if (isConstantTrue(expr->condition))
            {
                compileExpr(expr->trueExpr, target, targetTemp);
            }
            else
            {
                compileExpr(expr->falseExpr, target, targetTemp);
            }
        }
        else
        {
            std::vector<size_t> elseJump;
            compileConditionValue(expr->condition, nullptr, elseJump, false);
            compileExpr(expr->trueExpr, target, targetTemp);

            // Jump over else expression evaluation
            size_t thenLabel = bytecode.emitLabel();
            bytecode.emitAD(LOP_JUMP, 0, 0);

            size_t elseLabel = bytecode.emitLabel();
            compileExpr(expr->falseExpr, target, targetTemp);
            size_t endLabel = bytecode.emitLabel();

            patchJumps(expr, elseJump, elseLabel);
            patchJump(expr, thenLabel, endLabel);
        }
    }

    static uint8_t encodeHashSize(unsigned int hashSize)
    {
        size_t hashSizeLog2 = 0;
        while ((1u << hashSizeLog2) < hashSize)
            hashSizeLog2++;

        return hashSize == 0 ? 0 : uint8_t(hashSizeLog2 + 1);
    }

    void compileExprTable(AstExprTable* expr, uint8_t target, bool targetTemp)
    {
        // Optimization: if the table is empty, we can compute it directly into the target
        if (expr->items.size == 0)
        {
            TableShape shape = tableShapes[expr];

            bytecode.emitABC(LOP_NEWTABLE, target, encodeHashSize(shape.hashSize), 0);
            bytecode.emitAux(shape.arraySize);
            return;
        }

        unsigned int arraySize = 0;
        unsigned int hashSize = 0;
        unsigned int recordSize = 0;
        unsigned int indexSize = 0;

        for (size_t i = 0; i < expr->items.size; ++i)
        {
            const AstExprTable::Item& item = expr->items.data[i];

            arraySize += (item.kind == AstExprTable::Item::List);
            hashSize += (item.kind != AstExprTable::Item::List);
            recordSize += (item.kind == AstExprTable::Item::Record);
        }

        // Optimization: allocate sequential explicitly specified numeric indices ([1]) as arrays
        if (arraySize == 0 && hashSize > 0)
        {
            for (size_t i = 0; i < expr->items.size; ++i)
            {
                const AstExprTable::Item& item = expr->items.data[i];
                LUAU_ASSERT(item.key); // no list portion => all items have keys

                if (FFlag::LuauCompileTableIndexOpt)
                {
                    const Constant* ckey = constants.find(item.key);

                    indexSize += (ckey && ckey->type == Constant::Type_Number && ckey->valueNumber == double(indexSize + 1));
                }
                else
                {
                    AstExprConstantNumber* ckey = item.key->as<AstExprConstantNumber>();

                    indexSize += (ckey && ckey->value == double(indexSize + 1));
                }
            }

            // we only perform the optimization if we don't have any other []-keys
            // technically it's "safe" to do this even if we have other keys, but doing so changes iteration order and may break existing code
            if (hashSize == recordSize + indexSize)
                hashSize = recordSize;
            else
                indexSize = 0;
        }

        int encodedHashSize = encodeHashSize(hashSize);

        RegScope rs(this);

        // Optimization: if target is a temp register, we can clobber it which allows us to compute the result directly into it
        uint8_t reg = targetTemp ? target : allocReg(expr, 1);

        // Optimization: when all items are record fields, use template tables to compile expression
        if (arraySize == 0 && indexSize == 0 && hashSize == recordSize && recordSize >= 1 && recordSize <= BytecodeBuilder::TableShape::kMaxLength)
        {
            BytecodeBuilder::TableShape shape;

            for (size_t i = 0; i < expr->items.size; ++i)
            {
                const AstExprTable::Item& item = expr->items.data[i];
                LUAU_ASSERT(item.kind == AstExprTable::Item::Record);

                AstExprConstantString* ckey = item.key->as<AstExprConstantString>();
                LUAU_ASSERT(ckey);

                int cid = bytecode.addConstantString(sref(ckey->value));
                if (cid < 0)
                    CompileError::raise(ckey->location, "Exceeded constant limit; simplify the code to compile");

                LUAU_ASSERT(shape.length < BytecodeBuilder::TableShape::kMaxLength);
                shape.keys[shape.length++] = int16_t(cid);
            }

            int32_t tid = bytecode.addConstantTable(shape);
            if (tid < 0)
                CompileError::raise(expr->location, "Exceeded constant limit; simplify the code to compile");

            if (tid < 32768)
            {
                bytecode.emitAD(LOP_DUPTABLE, reg, int16_t(tid));
            }
            else
            {
                bytecode.emitABC(LOP_NEWTABLE, reg, uint8_t(encodedHashSize), 0);
                bytecode.emitAux(0);
            }
        }
        else
        {
            // Optimization: instead of allocating one extra element when the last element of the table literal is ..., let SETLIST allocate the
            // correct amount of storage
            const AstExprTable::Item* last = expr->items.size > 0 ? &expr->items.data[expr->items.size - 1] : nullptr;

            bool trailingVarargs = last && last->kind == AstExprTable::Item::List && last->value->is<AstExprVarargs>();
            LUAU_ASSERT(!trailingVarargs || arraySize > 0);

            bytecode.emitABC(LOP_NEWTABLE, reg, uint8_t(encodedHashSize), 0);
            bytecode.emitAux(arraySize - trailingVarargs + indexSize);
        }

        unsigned int arrayChunkSize = std::min(16u, arraySize);
        uint8_t arrayChunkReg = allocReg(expr, arrayChunkSize);
        unsigned int arrayChunkCurrent = 0;

        unsigned int arrayIndex = 1;
        bool multRet = false;

        for (size_t i = 0; i < expr->items.size; ++i)
        {
            const AstExprTable::Item& item = expr->items.data[i];

            AstExpr* key = item.key;
            AstExpr* value = item.value;

            // some key/value pairs don't require us to compile the expressions, so we need to setup the line info here
            setDebugLine(value);

            if (options.coverageLevel >= 2)
            {
                bytecode.emitABC(LOP_COVERAGE, 0, 0, 0);
            }

            // flush array chunk on overflow or before hash keys to maintain insertion order
            if (arrayChunkCurrent > 0 && (key || arrayChunkCurrent == arrayChunkSize))
            {
                bytecode.emitABC(LOP_SETLIST, reg, arrayChunkReg, uint8_t(arrayChunkCurrent + 1));
                bytecode.emitAux(arrayIndex);
                arrayIndex += arrayChunkCurrent;
                arrayChunkCurrent = 0;
            }

            // items with a key are set one by one via SETTABLE/SETTABLEKS/SETTABLEN
            if (key)
            {
                RegScope rsi(this);

                if (FFlag::LuauCompileTableIndexOpt)
                {
                    LValue lv = compileLValueIndex(reg, key, rsi);
                    uint8_t rv = compileExprAuto(value, rsi);

                    compileAssign(lv, rv);
                }
                else
                {
                    // Optimization: use SETTABLEKS/SETTABLEN for literal keys, this happens often as part of usual table construction syntax
                    if (AstExprConstantString* ckey = key->as<AstExprConstantString>())
                    {
                        BytecodeBuilder::StringRef cname = sref(ckey->value);
                        int32_t cid = bytecode.addConstantString(cname);
                        if (cid < 0)
                            CompileError::raise(expr->location, "Exceeded constant limit; simplify the code to compile");

                        uint8_t rv = compileExprAuto(value, rsi);

                        bytecode.emitABC(LOP_SETTABLEKS, rv, reg, uint8_t(BytecodeBuilder::getStringHash(cname)));
                        bytecode.emitAux(cid);
                    }
                    else if (AstExprConstantNumber* ckey = key->as<AstExprConstantNumber>();
                             ckey && ckey->value >= 1 && ckey->value <= 256 && double(int(ckey->value)) == ckey->value)
                    {
                        uint8_t rv = compileExprAuto(value, rsi);

                        bytecode.emitABC(LOP_SETTABLEN, rv, reg, uint8_t(int(ckey->value) - 1));
                    }
                    else
                    {
                        uint8_t rk = compileExprAuto(key, rsi);
                        uint8_t rv = compileExprAuto(value, rsi);

                        bytecode.emitABC(LOP_SETTABLE, rv, reg, rk);
                    }
                }
            }
            // items without a key are set using SETLIST so that we can initialize large arrays quickly
            else
            {
                uint8_t temp = uint8_t(arrayChunkReg + arrayChunkCurrent);

                if (i + 1 == expr->items.size)
                    multRet = compileExprTempMultRet(value, temp);
                else
                    compileExprTempTop(value, temp);

                arrayChunkCurrent++;
            }
        }

        // flush last array chunk; note that this needs multret handling if the last expression was multret
        if (arrayChunkCurrent)
        {
            bytecode.emitABC(LOP_SETLIST, reg, arrayChunkReg, multRet ? 0 : uint8_t(arrayChunkCurrent + 1));
            bytecode.emitAux(arrayIndex);
        }

        if (target != reg)
            bytecode.emitABC(LOP_MOVE, target, reg, 0);
    }

    bool canImport(AstExprGlobal* expr)
    {
        return options.optimizationLevel >= 1 && getGlobalState(globals, expr->name) != Global::Written;
    }

    bool canImportChain(AstExprGlobal* expr)
    {
        return options.optimizationLevel >= 1 && getGlobalState(globals, expr->name) == Global::Default;
    }

    void compileExprIndexName(AstExprIndexName* expr, uint8_t target)
    {
        setDebugLine(expr); // normally compileExpr sets up line info, but compileExprIndexName can be called directly

        // Optimization: index chains that start from global variables can be compiled into GETIMPORT statement
        AstExprGlobal* importRoot = 0;
        AstExprIndexName* import1 = 0;
        AstExprIndexName* import2 = 0;

        if (AstExprIndexName* index = expr->expr->as<AstExprIndexName>())
        {
            importRoot = index->expr->as<AstExprGlobal>();
            import1 = index;
            import2 = expr;
        }
        else
        {
            importRoot = expr->expr->as<AstExprGlobal>();
            import1 = expr;
        }

        if (importRoot && canImportChain(importRoot))
        {
            int32_t id0 = bytecode.addConstantString(sref(importRoot->name));
            int32_t id1 = bytecode.addConstantString(sref(import1->index));
            int32_t id2 = import2 ? bytecode.addConstantString(sref(import2->index)) : -1;

            if (id0 < 0 || id1 < 0 || (import2 && id2 < 0))
                CompileError::raise(expr->location, "Exceeded constant limit; simplify the code to compile");

            // Note: GETIMPORT encoding is limited to 10 bits per object id component
            if (id0 < 1024 && id1 < 1024 && id2 < 1024)
            {
                uint32_t iid = import2 ? BytecodeBuilder::getImportId(id0, id1, id2) : BytecodeBuilder::getImportId(id0, id1);
                int32_t cid = bytecode.addImport(iid);

                if (cid >= 0 && cid < 32768)
                {
                    bytecode.emitAD(LOP_GETIMPORT, target, int16_t(cid));
                    bytecode.emitAux(iid);
                    return;
                }
            }
        }

        RegScope rs(this);
        uint8_t reg = compileExprAuto(expr->expr, rs);

        setDebugLine(expr->indexLocation);

        BytecodeBuilder::StringRef iname = sref(expr->index);
        int32_t cid = bytecode.addConstantString(iname);
        if (cid < 0)
            CompileError::raise(expr->location, "Exceeded constant limit; simplify the code to compile");

        bytecode.emitABC(LOP_GETTABLEKS, target, reg, uint8_t(BytecodeBuilder::getStringHash(iname)));
        bytecode.emitAux(cid);
    }

    void compileExprIndexExpr(AstExprIndexExpr* expr, uint8_t target)
    {
        RegScope rs(this);

        const Constant* cv = constants.find(expr->index);

        if (cv && cv->type == Constant::Type_Number && double(int(cv->valueNumber)) == cv->valueNumber && cv->valueNumber >= 1 &&
            cv->valueNumber <= 256)
        {
            uint8_t rt = compileExprAuto(expr->expr, rs);
            uint8_t i = uint8_t(int(cv->valueNumber) - 1);

            if (FFlag::LuauCompileTableIndexOpt)
                setDebugLine(expr->index);

            bytecode.emitABC(LOP_GETTABLEN, target, rt, i);
        }
        else if (cv && cv->type == Constant::Type_String)
        {
            uint8_t rt = compileExprAuto(expr->expr, rs);

            BytecodeBuilder::StringRef iname = sref(cv->getString());
            int32_t cid = bytecode.addConstantString(iname);
            if (cid < 0)
                CompileError::raise(expr->location, "Exceeded constant limit; simplify the code to compile");

            if (FFlag::LuauCompileTableIndexOpt)
                setDebugLine(expr->index);

            bytecode.emitABC(LOP_GETTABLEKS, target, rt, uint8_t(BytecodeBuilder::getStringHash(iname)));
            bytecode.emitAux(cid);
        }
        else
        {
            uint8_t rt = compileExprAuto(expr->expr, rs);
            uint8_t ri = compileExprAuto(expr->index, rs);

            bytecode.emitABC(LOP_GETTABLE, target, rt, ri);
        }
    }

    void compileExprGlobal(AstExprGlobal* expr, uint8_t target)
    {
        // Optimization: builtin globals can be retrieved using GETIMPORT
        if (canImport(expr))
        {
            int32_t id0 = bytecode.addConstantString(sref(expr->name));
            if (id0 < 0)
                CompileError::raise(expr->location, "Exceeded constant limit; simplify the code to compile");

            // Note: GETIMPORT encoding is limited to 10 bits per object id component
            if (id0 < 1024)
            {
                uint32_t iid = BytecodeBuilder::getImportId(id0);
                int32_t cid = bytecode.addImport(iid);

                if (cid >= 0 && cid < 32768)
                {
                    bytecode.emitAD(LOP_GETIMPORT, target, int16_t(cid));
                    bytecode.emitAux(iid);
                    return;
                }
            }
        }

        BytecodeBuilder::StringRef gname = sref(expr->name);
        int32_t cid = bytecode.addConstantString(gname);
        if (cid < 0)
            CompileError::raise(expr->location, "Exceeded constant limit; simplify the code to compile");

        bytecode.emitABC(LOP_GETGLOBAL, target, 0, uint8_t(BytecodeBuilder::getStringHash(gname)));
        bytecode.emitAux(cid);
    }

    void compileExprConstant(AstExpr* node, const Constant* cv, uint8_t target)
    {
        switch (cv->type)
        {
        case Constant::Type_Nil:
            bytecode.emitABC(LOP_LOADNIL, target, 0, 0);
            break;

        case Constant::Type_Boolean:
            bytecode.emitABC(LOP_LOADB, target, cv->valueBoolean, 0);
            break;

        case Constant::Type_Number:
        {
            double d = cv->valueNumber;

            if (d >= std::numeric_limits<int16_t>::min() && d <= std::numeric_limits<int16_t>::max() && double(int16_t(d)) == d &&
                !(d == 0.0 && signbit(d)))
            {
                // short number encoding: doesn't require a table entry lookup
                bytecode.emitAD(LOP_LOADN, target, int16_t(d));
            }
            else
            {
                // long number encoding: use generic constant path
                int32_t cid = bytecode.addConstantNumber(d);
                if (cid < 0)
                    CompileError::raise(node->location, "Exceeded constant limit; simplify the code to compile");

                emitLoadK(target, cid);
            }
        }
        break;

        case Constant::Type_String:
        {
            int32_t cid = bytecode.addConstantString(sref(cv->getString()));
            if (cid < 0)
                CompileError::raise(node->location, "Exceeded constant limit; simplify the code to compile");

            emitLoadK(target, cid);
        }
        break;

        default:
            LUAU_ASSERT(!"Unexpected constant type");
        }
    }

    void compileExpr(AstExpr* node, uint8_t target, bool targetTemp = false)
    {
        setDebugLine(node);

        if (options.coverageLevel >= 2 && needsCoverage(node))
        {
            bytecode.emitABC(LOP_COVERAGE, 0, 0, 0);
        }

        // Optimization: if expression has a constant value, we can emit it directly
        if (const Constant* cv = constants.find(node))
        {
            if (cv->type != Constant::Type_Unknown)
            {
                compileExprConstant(node, cv, target);
                return;
            }
        }

        if (AstExprGroup* expr = node->as<AstExprGroup>())
        {
            compileExpr(expr->expr, target, targetTemp);
        }
        else if (node->is<AstExprConstantNil>())
        {
            bytecode.emitABC(LOP_LOADNIL, target, 0, 0);
        }
        else if (AstExprConstantBool* expr = node->as<AstExprConstantBool>())
        {
            bytecode.emitABC(LOP_LOADB, target, expr->value, 0);
        }
        else if (AstExprConstantNumber* expr = node->as<AstExprConstantNumber>())
        {
            int32_t cid = bytecode.addConstantNumber(expr->value);
            if (cid < 0)
                CompileError::raise(expr->location, "Exceeded constant limit; simplify the code to compile");

            emitLoadK(target, cid);
        }
        else if (AstExprConstantString* expr = node->as<AstExprConstantString>())
        {
            int32_t cid = bytecode.addConstantString(sref(expr->value));
            if (cid < 0)
                CompileError::raise(expr->location, "Exceeded constant limit; simplify the code to compile");

            emitLoadK(target, cid);
        }
        else if (AstExprLocal* expr = node->as<AstExprLocal>())
        {
            if (expr->upvalue)
            {
                uint8_t uid = getUpval(expr->local);

                bytecode.emitABC(LOP_GETUPVAL, target, uid, 0);
            }
            else
            {
                uint8_t reg = getLocal(expr->local);

                bytecode.emitABC(LOP_MOVE, target, reg, 0);
            }
        }
        else if (AstExprGlobal* expr = node->as<AstExprGlobal>())
        {
            compileExprGlobal(expr, target);
        }
        else if (AstExprVarargs* expr = node->as<AstExprVarargs>())
        {
            compileExprVarargs(expr, target, /* targetCount= */ 1);
        }
        else if (AstExprCall* expr = node->as<AstExprCall>())
        {
            // Optimization: when targeting temporary registers, we can compile call in a special mode that doesn't require extra register moves
            if (targetTemp && target == regTop - 1)
                compileExprCall(expr, target, 1, /* targetTop= */ true);
            else
                compileExprCall(expr, target, /* targetCount= */ 1);
        }
        else if (AstExprIndexName* expr = node->as<AstExprIndexName>())
        {
            compileExprIndexName(expr, target);
        }
        else if (AstExprIndexExpr* expr = node->as<AstExprIndexExpr>())
        {
            compileExprIndexExpr(expr, target);
        }
        else if (AstExprFunction* expr = node->as<AstExprFunction>())
        {
            compileExprFunction(expr, target);
        }
        else if (AstExprTable* expr = node->as<AstExprTable>())
        {
            compileExprTable(expr, target, targetTemp);
        }
        else if (AstExprUnary* expr = node->as<AstExprUnary>())
        {
            compileExprUnary(expr, target);
        }
        else if (AstExprBinary* expr = node->as<AstExprBinary>())
        {
            compileExprBinary(expr, target, targetTemp);
        }
        else if (AstExprTypeAssertion* expr = node->as<AstExprTypeAssertion>())
        {
            compileExpr(expr->expr, target, targetTemp);
        }
        else if (AstExprIfElse* expr = node->as<AstExprIfElse>())
        {
            compileExprIfElse(expr, target, targetTemp);
        }
        else
        {
            LUAU_ASSERT(!"Unknown expression type");
        }
    }

    void compileExprTemp(AstExpr* node, uint8_t target)
    {
        return compileExpr(node, target, /* targetTemp= */ true);
    }

    uint8_t compileExprAuto(AstExpr* node, RegScope&)
    {
        // Optimization: directly return locals instead of copying them to a temporary
        if (isExprLocalReg(node))
            return getLocal(node->as<AstExprLocal>()->local);

        // note: the register is owned by the parent scope
        uint8_t reg = allocReg(node, 1);

        compileExprTemp(node, reg);

        return reg;
    }

    // initializes target..target+targetCount-1 range using expressions from the list
    // if list has fewer expressions, and last expression is a call, we assume the call returns the rest of the values
    // if list has fewer expressions, and last expression isn't a call, we fill the rest with nil
    // assumes target register range can be clobbered and is at the top of the register space
    void compileExprListTop(const AstArray<AstExpr*>& list, uint8_t target, uint8_t targetCount)
    {
        // we assume that target range is at the top of the register space and can be clobbered
        // this is what allows us to compile the last call expression - if it's a call - using targetTop=true
        LUAU_ASSERT(unsigned(target + targetCount) == regTop);

        if (list.size == targetCount)
        {
            for (size_t i = 0; i < list.size; ++i)
                compileExprTemp(list.data[i], uint8_t(target + i));
        }
        else if (list.size > targetCount)
        {
            for (size_t i = 0; i < targetCount; ++i)
                compileExprTemp(list.data[i], uint8_t(target + i));

            // compute expressions with values that go nowhere; this is required to run side-effecting code if any
            for (size_t i = targetCount; i < list.size; ++i)
            {
                RegScope rsi(this);
                compileExprAuto(list.data[i], rsi);
            }
        }
        else if (list.size > 0)
        {
            for (size_t i = 0; i < list.size - 1; ++i)
                compileExprTemp(list.data[i], uint8_t(target + i));

            AstExpr* last = list.data[list.size - 1];

            if (AstExprCall* expr = last->as<AstExprCall>())
            {
                compileExprCall(expr, uint8_t(target + list.size - 1), uint8_t(targetCount - (list.size - 1)), /* targetTop= */ true);
            }
            else if (AstExprVarargs* expr = last->as<AstExprVarargs>())
            {
                compileExprVarargs(expr, uint8_t(target + list.size - 1), uint8_t(targetCount - (list.size - 1)));
            }
            else
            {
                compileExprTemp(last, uint8_t(target + list.size - 1));

                for (size_t i = list.size; i < targetCount; ++i)
                    bytecode.emitABC(LOP_LOADNIL, uint8_t(target + i), 0, 0);
            }
        }
        else
        {
            for (size_t i = 0; i < targetCount; ++i)
                bytecode.emitABC(LOP_LOADNIL, uint8_t(target + i), 0, 0);
        }
    }

    struct LValue
    {
        enum Kind
        {
            Kind_Local,
            Kind_Upvalue,
            Kind_Global,
            Kind_IndexName,
            Kind_IndexNumber,
            Kind_IndexExpr,
        };

        Kind kind;
        uint8_t reg; // register for local (Local) or table (Index*)
        uint8_t upval;
        uint8_t index;  // register for index in IndexExpr
        uint8_t number; // index-1 (0-255) in IndexNumber
        BytecodeBuilder::StringRef name;
        Location location;
    };

    LValue compileLValueIndex(uint8_t reg, AstExpr* index, RegScope& rs)
    {
        const Constant* cv = constants.find(index);

        if (cv && cv->type == Constant::Type_Number && cv->valueNumber >= 1 && cv->valueNumber <= 256 &&
            double(int(cv->valueNumber)) == cv->valueNumber)
        {
            LValue result = {LValue::Kind_IndexNumber};
            result.reg = reg;
            result.number = uint8_t(int(cv->valueNumber) - 1);
            result.location = index->location;

            return result;
        }
        else if (cv && cv->type == Constant::Type_String)
        {
            LValue result = {LValue::Kind_IndexName};
            result.reg = reg;
            result.name = sref(cv->getString());
            result.location = index->location;

            return result;
        }
        else
        {
            LValue result = {LValue::Kind_IndexExpr};
            result.reg = reg;
            result.index = compileExprAuto(index, rs);
            result.location = index->location;

            return result;
        }
    }

    LValue compileLValue(AstExpr* node, RegScope& rs)
    {
        setDebugLine(node);

        if (AstExprLocal* expr = node->as<AstExprLocal>())
        {
            if (expr->upvalue)
            {
                LValue result = {LValue::Kind_Upvalue};
                result.upval = getUpval(expr->local);
                result.location = node->location;

                return result;
            }
            else
            {
                LValue result = {LValue::Kind_Local};
                result.reg = getLocal(expr->local);
                result.location = node->location;

                return result;
            }
        }
        else if (AstExprGlobal* expr = node->as<AstExprGlobal>())
        {
            LValue result = {LValue::Kind_Global};
            result.name = sref(expr->name);
            result.location = node->location;

            return result;
        }
        else if (AstExprIndexName* expr = node->as<AstExprIndexName>())
        {
            LValue result = {LValue::Kind_IndexName};
            result.reg = compileExprAuto(expr->expr, rs);
            result.name = sref(expr->index);
            result.location = node->location;

            return result;
        }
        else if (AstExprIndexExpr* expr = node->as<AstExprIndexExpr>())
        {
            uint8_t reg = compileExprAuto(expr->expr, rs);

            return compileLValueIndex(reg, expr->index, rs);
        }
        else
        {
            LUAU_ASSERT(!"Unknown assignment expression");

            return LValue();
        }
    }

    void compileLValueUse(const LValue& lv, uint8_t reg, bool set)
    {
        if (FFlag::LuauCompileTableIndexOpt)
            setDebugLine(lv.location);

        switch (lv.kind)
        {
        case LValue::Kind_Local:
            if (set)
                bytecode.emitABC(LOP_MOVE, lv.reg, reg, 0);
            else
                bytecode.emitABC(LOP_MOVE, reg, lv.reg, 0);
            break;

        case LValue::Kind_Upvalue:
            bytecode.emitABC(set ? LOP_SETUPVAL : LOP_GETUPVAL, reg, lv.upval, 0);
            break;

        case LValue::Kind_Global:
        {
            int32_t cid = bytecode.addConstantString(lv.name);
            if (cid < 0)
                CompileError::raise(lv.location, "Exceeded constant limit; simplify the code to compile");

            bytecode.emitABC(set ? LOP_SETGLOBAL : LOP_GETGLOBAL, reg, 0, uint8_t(BytecodeBuilder::getStringHash(lv.name)));
            bytecode.emitAux(cid);
        }
        break;

        case LValue::Kind_IndexName:
        {
            int32_t cid = bytecode.addConstantString(lv.name);
            if (cid < 0)
                CompileError::raise(lv.location, "Exceeded constant limit; simplify the code to compile");

            bytecode.emitABC(set ? LOP_SETTABLEKS : LOP_GETTABLEKS, reg, lv.reg, uint8_t(BytecodeBuilder::getStringHash(lv.name)));
            bytecode.emitAux(cid);
        }
        break;

        case LValue::Kind_IndexNumber:
            bytecode.emitABC(set ? LOP_SETTABLEN : LOP_GETTABLEN, reg, lv.reg, lv.number);
            break;

        case LValue::Kind_IndexExpr:
            bytecode.emitABC(set ? LOP_SETTABLE : LOP_GETTABLE, reg, lv.reg, lv.index);
            break;

        default:
            LUAU_ASSERT(!"Unknown lvalue kind");
        }
    }

    void compileAssign(const LValue& lv, uint8_t source)
    {
        compileLValueUse(lv, source, /* set= */ true);
    }

    bool isExprLocalReg(AstExpr* expr)
    {
        AstExprLocal* le = expr->as<AstExprLocal>();
        if (!le || le->upvalue)
            return false;

        Local* l = locals.find(le->local);

        return l && l->allocated;
    }

    bool isStatBreak(AstStat* node)
    {
        if (AstStatBlock* stat = node->as<AstStatBlock>())
            return stat->body.size == 1 && stat->body.data[0]->is<AstStatBreak>();

        return node->is<AstStatBreak>();
    }

    AstStatContinue* extractStatContinue(AstStatBlock* block)
    {
        if (block->body.size == 1)
            return block->body.data[0]->as<AstStatContinue>();
        else
            return nullptr;
    }

    void compileStatIf(AstStatIf* stat)
    {
        // Optimization: condition is always false => we only need the else body
        if (isConstantFalse(stat->condition))
        {
            if (stat->elsebody)
                compileStat(stat->elsebody);
            return;
        }

        // Optimization: body is a "break" statement with no "else" => we can directly break out of the loop in "then" case
        if (!stat->elsebody && isStatBreak(stat->thenbody) && !areLocalsCaptured(loops.back().localOffset))
        {
            // fallthrough = continue with the loop as usual
            std::vector<size_t> elseJump;
            compileConditionValue(stat->condition, nullptr, elseJump, true);

            for (size_t jump : elseJump)
                loopJumps.push_back({LoopJump::Break, jump});
            return;
        }

        AstStat* continueStatement = extractStatContinue(stat->thenbody);

        // Optimization: body is a "continue" statement with no "else" => we can directly continue in "then" case
        if (!stat->elsebody && continueStatement != nullptr && !areLocalsCaptured(loops.back().localOffset))
        {
            if (loops.back().untilCondition)
                validateContinueUntil(continueStatement, loops.back().untilCondition);

            // fallthrough = proceed with the loop body as usual
            std::vector<size_t> elseJump;
            compileConditionValue(stat->condition, nullptr, elseJump, true);

            for (size_t jump : elseJump)
                loopJumps.push_back({LoopJump::Continue, jump});
            return;
        }

        std::vector<size_t> elseJump;
        compileConditionValue(stat->condition, nullptr, elseJump, false);

        compileStat(stat->thenbody);

        if (stat->elsebody && elseJump.size() > 0)
        {
            // we don't need to skip past "else" body if "then" ends with return
            // this is important because, if "else" also ends with return, we may *not* have any statement to skip to!
            if (allPathsEndWithReturn(stat->thenbody))
            {
                size_t elseLabel = bytecode.emitLabel();

                compileStat(stat->elsebody);

                patchJumps(stat, elseJump, elseLabel);
            }
            else
            {
                size_t thenLabel = bytecode.emitLabel();

                bytecode.emitAD(LOP_JUMP, 0, 0);

                size_t elseLabel = bytecode.emitLabel();

                compileStat(stat->elsebody);

                size_t endLabel = bytecode.emitLabel();

                patchJumps(stat, elseJump, elseLabel);
                patchJump(stat, thenLabel, endLabel);
            }
        }
        else
        {
            size_t endLabel = bytecode.emitLabel();

            patchJumps(stat, elseJump, endLabel);
        }
    }

    void compileStatWhile(AstStatWhile* stat)
    {
        // Optimization: condition is always false => there's no loop!
        if (isConstantFalse(stat->condition))
            return;

        size_t oldJumps = loopJumps.size();
        size_t oldLocals = localStack.size();

        loops.push_back({oldLocals, nullptr});

        size_t loopLabel = bytecode.emitLabel();

        std::vector<size_t> elseJump;
        compileConditionValue(stat->condition, nullptr, elseJump, false);

        compileStat(stat->body);

        size_t contLabel = bytecode.emitLabel();

        size_t backLabel = bytecode.emitLabel();

        setDebugLine(stat->condition);

        // Note: this is using JUMPBACK, not JUMP, since JUMPBACK is interruptible and we want all loops to have at least one interruptible
        // instruction
        bytecode.emitAD(LOP_JUMPBACK, 0, 0);

        size_t endLabel = bytecode.emitLabel();

        patchJump(stat, backLabel, loopLabel);
        patchJumps(stat, elseJump, endLabel);

        patchLoopJumps(stat, oldJumps, endLabel, contLabel);
        loopJumps.resize(oldJumps);

        loops.pop_back();
    }

    void compileStatRepeat(AstStatRepeat* stat)
    {
        size_t oldJumps = loopJumps.size();
        size_t oldLocals = localStack.size();

        loops.push_back({oldLocals, stat->condition});

        size_t loopLabel = bytecode.emitLabel();

        // note: we "inline" compileStatBlock here so that we can close/pop locals after evaluating condition
        // this is necessary because condition can access locals declared inside the repeat..until body
        AstStatBlock* body = stat->body;

        RegScope rs(this);

        for (size_t i = 0; i < body->body.size; ++i)
            compileStat(body->body.data[i]);

        size_t contLabel = bytecode.emitLabel();

        size_t endLabel;

        setDebugLine(stat->condition);

        if (isConstantTrue(stat->condition))
        {
            closeLocals(oldLocals);

            endLabel = bytecode.emitLabel();
        }
        else
        {
            std::vector<size_t> skipJump;
            compileConditionValue(stat->condition, nullptr, skipJump, true);

            // we close locals *after* we compute loop conditionals because during computation of condition it's (in theory) possible that user code
            // mutates them
            closeLocals(oldLocals);

            size_t backLabel = bytecode.emitLabel();

            // Note: this is using JUMPBACK, not JUMP, since JUMPBACK is interruptible and we want all loops to have at least one interruptible
            // instruction
            bytecode.emitAD(LOP_JUMPBACK, 0, 0);

            size_t skipLabel = bytecode.emitLabel();

            // we need to close locals *again* after the loop ends because the first closeLocals would be jumped over on the last iteration
            closeLocals(oldLocals);

            endLabel = bytecode.emitLabel();

            patchJump(stat, backLabel, loopLabel);
            patchJumps(stat, skipJump, skipLabel);
        }

        popLocals(oldLocals);

        patchLoopJumps(stat, oldJumps, endLabel, contLabel);
        loopJumps.resize(oldJumps);

        loops.pop_back();
    }

    void compileStatReturn(AstStatReturn* stat)
    {
        RegScope rs(this);

        uint8_t temp = 0;
        bool multRet = false;

        // Optimization: return local value directly instead of copying it into a temporary
        if (stat->list.size == 1 && isExprLocalReg(stat->list.data[0]))
        {
            AstExprLocal* le = stat->list.data[0]->as<AstExprLocal>();
            LUAU_ASSERT(le);

            temp = getLocal(le->local);
        }
        else if (stat->list.size > 0)
        {
            temp = allocReg(stat, unsigned(stat->list.size));

            // Note: if the last element is a function call or a vararg specifier, then we need to somehow return all values that that call returned
            for (size_t i = 0; i < stat->list.size; ++i)
                if (i + 1 == stat->list.size)
                    multRet = compileExprTempMultRet(stat->list.data[i], uint8_t(temp + i));
                else
                    compileExprTempTop(stat->list.data[i], uint8_t(temp + i));
        }

        closeLocals(0);

        bytecode.emitABC(LOP_RETURN, uint8_t(temp), multRet ? 0 : uint8_t(stat->list.size + 1), 0);
    }

    bool areLocalsRedundant(AstStatLocal* stat)
    {
        // Extra expressions may have side effects
        if (stat->values.size > stat->vars.size)
            return false;

        for (AstLocal* local : stat->vars)
        {
            Variable* v = variables.find(local);

            if (!v || !v->constant)
                return false;
        }

        return true;
    }

    void compileStatLocal(AstStatLocal* stat)
    {
        // Optimization: we don't need to allocate and assign const locals, since their uses will be constant-folded
        if (options.optimizationLevel >= 1 && options.debugLevel <= 1 && areLocalsRedundant(stat))
            return;

        // note: allocReg in this case allocates into parent block register - note that we don't have RegScope here
        uint8_t vars = allocReg(stat, unsigned(stat->vars.size));

        compileExprListTop(stat->values, vars, uint8_t(stat->vars.size));

        for (size_t i = 0; i < stat->vars.size; ++i)
            pushLocal(stat->vars.data[i], uint8_t(vars + i));
    }

    void compileStatFor(AstStatFor* stat)
    {
        RegScope rs(this);

        size_t oldLocals = localStack.size();
        size_t oldJumps = loopJumps.size();

        loops.push_back({oldLocals, nullptr});

        // register layout: limit, step, index
        uint8_t regs = allocReg(stat, 3);

        // if the iteration index is assigned from within the loop, we need to protect the internal index from the assignment
        // to do that, we will copy the index into an actual local variable on each iteration
        // this makes sure the code inside the loop can't interfere with the iteration process (other than modifying the table we're iterating
        // through)
        uint8_t varreg = regs + 2;

        if (Variable* il = variables.find(stat->var); il && il->written)
            varreg = allocReg(stat, 1);

        compileExprTemp(stat->from, uint8_t(regs + 2));
        compileExprTemp(stat->to, uint8_t(regs + 0));

        if (stat->step)
            compileExprTemp(stat->step, uint8_t(regs + 1));
        else
            bytecode.emitABC(LOP_LOADN, uint8_t(regs + 1), 1, 0);

        size_t forLabel = bytecode.emitLabel();

        bytecode.emitAD(LOP_FORNPREP, regs, 0);

        size_t loopLabel = bytecode.emitLabel();

        if (varreg != regs + 2)
            bytecode.emitABC(LOP_MOVE, varreg, regs + 2, 0);

        pushLocal(stat->var, varreg);

        compileStat(stat->body);

        closeLocals(oldLocals);
        popLocals(oldLocals);

        setDebugLine(stat);

        size_t contLabel = bytecode.emitLabel();

        size_t backLabel = bytecode.emitLabel();

        bytecode.emitAD(LOP_FORNLOOP, regs, 0);

        size_t endLabel = bytecode.emitLabel();

        patchJump(stat, forLabel, endLabel);
        patchJump(stat, backLabel, loopLabel);

        patchLoopJumps(stat, oldJumps, endLabel, contLabel);
        loopJumps.resize(oldJumps);

        loops.pop_back();
    }

    void compileStatForIn(AstStatForIn* stat)
    {
        RegScope rs(this);

        size_t oldLocals = localStack.size();
        size_t oldJumps = loopJumps.size();

        loops.push_back({oldLocals, nullptr});

        // register layout: generator, state, index, variables...
        uint8_t regs = allocReg(stat, 3);

        // this puts initial values of (generator, state, index) into the loop registers
        compileExprListTop(stat->values, regs, 3);

        // for the general case, we will execute a CALL for every iteration that needs to evaluate "variables... = generator(state, index)"
        // this requires at least extra 3 stack slots after index
        // note that these stack slots overlap with the variables so we only need to reserve them to make sure stack frame is large enough
        reserveReg(stat, 3);

        // note that we reserve at least 2 variables; this allows our fast path to assume that we need 2 variables instead of 1 or 2
        uint8_t vars = allocReg(stat, std::max(unsigned(stat->vars.size), 2u));
        LUAU_ASSERT(vars == regs + 3);

        // Optimization: when we iterate through pairs/ipairs, we generate special bytecode that optimizes the traversal using internal iteration
        // index These instructions dynamically check if generator is equal to next/inext and bail out They assume that the generator produces 2
        // variables, which is why we allocate at least 2 above (see vars assignment)
        LuauOpcode skipOp = LOP_JUMP;
        LuauOpcode loopOp = LOP_FORGLOOP;

        if (options.optimizationLevel >= 1 && stat->vars.size <= 2)
        {
            if (stat->values.size == 1 && stat->values.data[0]->is<AstExprCall>())
            {
                Builtin builtin = getBuiltin(stat->values.data[0]->as<AstExprCall>()->func, globals, variables);

                if (builtin.isGlobal("ipairs")) // for .. in ipairs(t)
                {
                    skipOp = LOP_FORGPREP_INEXT;
                    loopOp = LOP_FORGLOOP_INEXT;
                }
                else if (builtin.isGlobal("pairs")) // for .. in pairs(t)
                {
                    skipOp = LOP_FORGPREP_NEXT;
                    loopOp = LOP_FORGLOOP_NEXT;
                }
            }
            else if (stat->values.size == 2)
            {
                Builtin builtin = getBuiltin(stat->values.data[0], globals, variables);

                if (builtin.isGlobal("next")) // for .. in next,t
                {
                    skipOp = LOP_FORGPREP_NEXT;
                    loopOp = LOP_FORGLOOP_NEXT;
                }
            }
        }

        // first iteration jumps into FORGLOOP instruction, but for ipairs/pairs it does extra preparation that makes the cost of an extra instruction
        // worthwhile
        size_t skipLabel = bytecode.emitLabel();

        bytecode.emitAD(skipOp, regs, 0);

        size_t loopLabel = bytecode.emitLabel();

        for (size_t i = 0; i < stat->vars.size; ++i)
            pushLocal(stat->vars.data[i], uint8_t(vars + i));

        compileStat(stat->body);

        closeLocals(oldLocals);
        popLocals(oldLocals);

        setDebugLine(stat);

        size_t contLabel = bytecode.emitLabel();

        size_t backLabel = bytecode.emitLabel();

        bytecode.emitAD(loopOp, regs, 0);

        // note: FORGLOOP needs variable count encoded in AUX field, other loop instructions assume a fixed variable count
        if (loopOp == LOP_FORGLOOP)
            bytecode.emitAux(uint32_t(stat->vars.size));

        size_t endLabel = bytecode.emitLabel();

        patchJump(stat, skipLabel, backLabel);
        patchJump(stat, backLabel, loopLabel);

        patchLoopJumps(stat, oldJumps, endLabel, contLabel);
        loopJumps.resize(oldJumps);

        loops.pop_back();
    }

    void resolveAssignConflicts(AstStat* stat, std::vector<LValue>& vars)
    {
        // regsUsed[i] is true if we have assigned the register during earlier assignments
        // regsRemap[i] is set to the register where the original (pre-assignment) copy was made
        // note: regsRemap is uninitialized intentionally to speed small assignments up; regsRemap[i] is valid iff regsUsed[i]
        std::bitset<256> regsUsed;
        uint8_t regsRemap[256];

        for (size_t i = 0; i < vars.size(); ++i)
        {
            LValue& li = vars[i];

            if (li.kind == LValue::Kind_Local)
            {
                if (!regsUsed[li.reg])
                {
                    regsUsed[li.reg] = true;
                    regsRemap[li.reg] = li.reg;
                }
            }
            else if (li.kind == LValue::Kind_IndexName || li.kind == LValue::Kind_IndexNumber || li.kind == LValue::Kind_IndexExpr)
            {
                // we're looking for assignments before this one that invalidate any of the registers involved
                if (regsUsed[li.reg])
                {
                    // the register may have been evacuated previously, but if it wasn't - move it now
                    if (regsRemap[li.reg] == li.reg)
                    {
                        uint8_t reg = allocReg(stat, 1);
                        bytecode.emitABC(LOP_MOVE, reg, li.reg, 0);

                        regsRemap[li.reg] = reg;
                    }

                    li.reg = regsRemap[li.reg];
                }

                if (li.kind == LValue::Kind_IndexExpr && regsUsed[li.index])
                {
                    // the register may have been evacuated previously, but if it wasn't - move it now
                    if (regsRemap[li.index] == li.index)
                    {
                        uint8_t reg = allocReg(stat, 1);
                        bytecode.emitABC(LOP_MOVE, reg, li.index, 0);

                        regsRemap[li.index] = reg;
                    }

                    li.index = regsRemap[li.index];
                }
            }
        }
    }

    void compileStatAssign(AstStatAssign* stat)
    {
        RegScope rs(this);

        // Optimization: one to one assignments don't require complex conflict resolution machinery and allow us to skip temporary registers for
        // locals
        if (stat->vars.size == 1 && stat->values.size == 1)
        {
            LValue var = compileLValue(stat->vars.data[0], rs);

            // Optimization: assign to locals directly
            if (var.kind == LValue::Kind_Local)
            {
                compileExpr(stat->values.data[0], var.reg);
            }
            else
            {
                uint8_t reg = compileExprAuto(stat->values.data[0], rs);

                setDebugLine(stat->vars.data[0]);
                compileAssign(var, reg);
            }
            return;
        }

        // compute all l-values: note that this doesn't assign anything yet but it allocates registers and computes complex expressions on the left
        // hand side for example, in "a[expr] = foo" expr will get evaluated here
        std::vector<LValue> vars(stat->vars.size);

        for (size_t i = 0; i < stat->vars.size; ++i)
            vars[i] = compileLValue(stat->vars.data[i], rs);

        // perform conflict resolution: if any lvalue refers to a local reg that will be reassigned before that, we save the local variable in a
        // temporary reg
        resolveAssignConflicts(stat, vars);

        // compute values into temporaries
        uint8_t regs = allocReg(stat, unsigned(stat->vars.size));

        compileExprListTop(stat->values, regs, uint8_t(stat->vars.size));

        // assign variables that have associated values; note that if we have fewer values than variables, we'll assign nil because compileExprListTop
        // will generate nils
        for (size_t i = 0; i < stat->vars.size; ++i)
        {
            setDebugLine(stat->vars.data[i]);
            compileAssign(vars[i], uint8_t(regs + i));
        }
    }

    void compileStatCompoundAssign(AstStatCompoundAssign* stat)
    {
        RegScope rs(this);

        LValue var = compileLValue(stat->var, rs);

        // Optimization: assign to locals directly
        uint8_t target = (var.kind == LValue::Kind_Local) ? var.reg : allocReg(stat, 1);

        switch (stat->op)
        {
        case AstExprBinary::Add:
        case AstExprBinary::Sub:
        case AstExprBinary::Mul:
        case AstExprBinary::Div:
        case AstExprBinary::Mod:
        case AstExprBinary::Pow:
        {
            if (var.kind != LValue::Kind_Local)
                compileLValueUse(var, target, /* set= */ false);

            int32_t rc = getConstantNumber(stat->value);

            if (rc >= 0 && rc <= 255)
            {
                bytecode.emitABC(getBinaryOpArith(stat->op, /* k= */ true), target, target, uint8_t(rc));
            }
            else
            {
                uint8_t rr = compileExprAuto(stat->value, rs);

                bytecode.emitABC(getBinaryOpArith(stat->op), target, target, rr);
            }
        }
        break;

        case AstExprBinary::Concat:
        {
            std::vector<AstExpr*> args = {stat->value};

            // unroll the tree of concats down the right hand side to be able to do multiple ops
            unrollConcats(args);

            uint8_t regs = allocReg(stat, unsigned(1 + args.size()));

            compileLValueUse(var, regs, /* set= */ false);

            for (size_t i = 0; i < args.size(); ++i)
                compileExprTemp(args[i], uint8_t(regs + 1 + i));

            bytecode.emitABC(LOP_CONCAT, target, regs, uint8_t(regs + args.size()));
        }
        break;

        default:
            LUAU_ASSERT(!"Unexpected compound assignment operation");
        }

        if (var.kind != LValue::Kind_Local)
            compileAssign(var, target);
    }

    void compileStatFunction(AstStatFunction* stat)
    {
        // Optimization: compile value expresion directly into target local register
        if (isExprLocalReg(stat->name))
        {
            AstExprLocal* le = stat->name->as<AstExprLocal>();
            LUAU_ASSERT(le);

            compileExpr(stat->func, getLocal(le->local));
            return;
        }

        RegScope rs(this);
        uint8_t reg = allocReg(stat, 1);

        compileExprTemp(stat->func, reg);

        LValue var = compileLValue(stat->name, rs);
        compileAssign(var, reg);
    }

    void compileStat(AstStat* node)
    {
        setDebugLine(node);

        if (options.coverageLevel >= 1 && needsCoverage(node))
        {
            bytecode.emitABC(LOP_COVERAGE, 0, 0, 0);
        }

        if (AstStatBlock* stat = node->as<AstStatBlock>())
        {
            RegScope rs(this);

            size_t oldLocals = localStack.size();

            for (size_t i = 0; i < stat->body.size; ++i)
                compileStat(stat->body.data[i]);

            closeLocals(oldLocals);

            popLocals(oldLocals);
        }
        else if (AstStatIf* stat = node->as<AstStatIf>())
        {
            compileStatIf(stat);
        }
        else if (AstStatWhile* stat = node->as<AstStatWhile>())
        {
            compileStatWhile(stat);
        }
        else if (AstStatRepeat* stat = node->as<AstStatRepeat>())
        {
            compileStatRepeat(stat);
        }
        else if (node->is<AstStatBreak>())
        {
            LUAU_ASSERT(!loops.empty());

            // before exiting out of the loop, we need to close all local variables that were captured in closures since loop start
            // normally they are closed by the enclosing blocks, including the loop block, but we're skipping that here
            closeLocals(loops.back().localOffset);

            size_t label = bytecode.emitLabel();

            bytecode.emitAD(LOP_JUMP, 0, 0);

            loopJumps.push_back({LoopJump::Break, label});
        }
        else if (AstStatContinue* stat = node->as<AstStatContinue>())
        {
            LUAU_ASSERT(!loops.empty());

            if (loops.back().untilCondition)
                validateContinueUntil(stat, loops.back().untilCondition);

            // before continuing, we need to close all local variables that were captured in closures since loop start
            // normally they are closed by the enclosing blocks, including the loop block, but we're skipping that here
            closeLocals(loops.back().localOffset);

            size_t label = bytecode.emitLabel();

            bytecode.emitAD(LOP_JUMP, 0, 0);

            loopJumps.push_back({LoopJump::Continue, label});
        }
        else if (AstStatReturn* stat = node->as<AstStatReturn>())
        {
            compileStatReturn(stat);
        }
        else if (AstStatExpr* stat = node->as<AstStatExpr>())
        {
            // Optimization: since we don't need to read anything from the stack, we can compile the call to not return anything which saves register
            // moves
            if (AstExprCall* expr = stat->expr->as<AstExprCall>())
            {
                uint8_t target = uint8_t(regTop);

                compileExprCall(expr, target, /* targetCount= */ 0);
            }
            else
            {
                RegScope rs(this);
                compileExprAuto(stat->expr, rs);
            }
        }
        else if (AstStatLocal* stat = node->as<AstStatLocal>())
        {
            compileStatLocal(stat);
        }
        else if (AstStatFor* stat = node->as<AstStatFor>())
        {
            compileStatFor(stat);
        }
        else if (AstStatForIn* stat = node->as<AstStatForIn>())
        {
            compileStatForIn(stat);
        }
        else if (AstStatAssign* stat = node->as<AstStatAssign>())
        {
            compileStatAssign(stat);
        }
        else if (AstStatCompoundAssign* stat = node->as<AstStatCompoundAssign>())
        {
            compileStatCompoundAssign(stat);
        }
        else if (AstStatFunction* stat = node->as<AstStatFunction>())
        {
            compileStatFunction(stat);
        }
        else if (AstStatLocalFunction* stat = node->as<AstStatLocalFunction>())
        {
            uint8_t var = allocReg(stat, 1);

            pushLocal(stat->name, var);
            compileExprFunction(stat->func, var);

            Local& l = locals[stat->name];

            // we *have* to pushLocal before we compile the function, since the function may refer to the local as an upvalue
            // however, this means the debugpc for the local is at an instruction where the local value hasn't been computed yet
            // to fix this we just move the debugpc after the local value is established
            l.debugpc = bytecode.getDebugPC();
        }
        else if (node->is<AstStatTypeAlias>())
        {
            // do nothing
        }
        else
        {
            LUAU_ASSERT(!"Unknown statement type");
        }
    }

    void validateContinueUntil(AstStat* cont, AstExpr* condition)
    {
        UndefinedLocalVisitor visitor(this);
        condition->visit(&visitor);

        if (visitor.undef)
            CompileError::raise(condition->location,
                "Local %s used in the repeat..until condition is undefined because continue statement on line %d jumps over it",
                visitor.undef->name.value, cont->location.begin.line + 1);
    }

    void gatherConstUpvals(AstExprFunction* func)
    {
        ConstUpvalueVisitor visitor(this);
        func->body->visit(&visitor);

        for (AstLocal* local : visitor.upvals)
            getUpval(local);
    }

    void pushLocal(AstLocal* local, uint8_t reg)
    {
        if (localStack.size() >= kMaxLocalCount)
            CompileError::raise(
                local->location, "Out of local registers when trying to allocate %s: exceeded limit %d", local->name.value, kMaxLocalCount);

        localStack.push_back(local);

        Local& l = locals[local];

        LUAU_ASSERT(!l.allocated);

        l.reg = reg;
        l.allocated = true;
        l.debugpc = bytecode.getDebugPC();
    }

    bool areLocalsCaptured(size_t start)
    {
        LUAU_ASSERT(start <= localStack.size());

        for (size_t i = start; i < localStack.size(); ++i)
        {
            Local* l = locals.find(localStack[i]);
            LUAU_ASSERT(l);

            if (l->captured)
                return true;
        }

        return false;
    }

    void closeLocals(size_t start)
    {
        LUAU_ASSERT(start <= localStack.size());

        bool captured = false;
        uint8_t captureReg = 255;

        for (size_t i = start; i < localStack.size(); ++i)
        {
            Local* l = locals.find(localStack[i]);
            LUAU_ASSERT(l);

            if (l->captured)
            {
                captured = true;
                captureReg = std::min(captureReg, l->reg);
            }
        }

        if (captured)
        {
            bytecode.emitABC(LOP_CLOSEUPVALS, captureReg, 0, 0);
        }
    }

    void popLocals(size_t start)
    {
        LUAU_ASSERT(start <= localStack.size());

        for (size_t i = start; i < localStack.size(); ++i)
        {
            Local* l = locals.find(localStack[i]);
            LUAU_ASSERT(l);
            LUAU_ASSERT(l->allocated);

            l->allocated = false;

            if (options.debugLevel >= 2)
            {
                uint32_t debugpc = bytecode.getDebugPC();

                bytecode.pushDebugLocal(sref(localStack[i]->name), l->reg, l->debugpc, debugpc);
            }
        }

        localStack.resize(start);
    }

    void patchJump(AstNode* node, size_t label, size_t target)
    {
        if (!bytecode.patchJumpD(label, target))
            CompileError::raise(node->location, "Exceeded jump distance limit; simplify the code to compile");
    }

    void patchJumps(AstNode* node, std::vector<size_t>& labels, size_t target)
    {
        for (size_t l : labels)
            patchJump(node, l, target);
    }

    void patchLoopJumps(AstNode* node, size_t oldJumps, size_t endLabel, size_t contLabel)
    {
        LUAU_ASSERT(oldJumps <= loopJumps.size());

        for (size_t i = oldJumps; i < loopJumps.size(); ++i)
        {
            const LoopJump& lj = loopJumps[i];

            switch (lj.type)
            {
            case LoopJump::Break:
                patchJump(node, lj.label, endLabel);
                break;

            case LoopJump::Continue:
                patchJump(node, lj.label, contLabel);
                break;

            default:
                LUAU_ASSERT(!"Unknown loop jump type");
            }
        }
    }

    uint8_t allocReg(AstNode* node, unsigned int count)
    {
        unsigned int top = regTop;
        if (top + count > kMaxRegisterCount)
            CompileError::raise(node->location, "Out of registers when trying to allocate %d registers: exceeded limit %d", count, kMaxRegisterCount);

        regTop += count;
        stackSize = std::max(stackSize, regTop);

        return uint8_t(top);
    }

    void reserveReg(AstNode* node, unsigned int count)
    {
        if (regTop + count > kMaxRegisterCount)
            CompileError::raise(node->location, "Out of registers when trying to allocate %d registers: exceeded limit %d", count, kMaxRegisterCount);

        stackSize = std::max(stackSize, regTop + count);
    }

    void setDebugLine(AstNode* node)
    {
        if (options.debugLevel >= 1)
            bytecode.setDebugLine(node->location.begin.line + 1);
    }

    void setDebugLine(const Location& location)
    {
        if (options.debugLevel >= 1)
            bytecode.setDebugLine(location.begin.line + 1);
    }

    void setDebugLineEnd(AstNode* node)
    {
        if (options.debugLevel >= 1)
            bytecode.setDebugLine(node->location.end.line + 1);
    }

    bool needsCoverage(AstNode* node)
    {
        return !node->is<AstStatBlock>() && !node->is<AstStatTypeAlias>();
    }

    struct FenvVisitor : AstVisitor
    {
        bool& getfenvUsed;
        bool& setfenvUsed;

        FenvVisitor(bool& getfenvUsed, bool& setfenvUsed)
            : getfenvUsed(getfenvUsed)
            , setfenvUsed(setfenvUsed)
        {
        }

        bool visit(AstExprGlobal* node) override
        {
            if (node->name == "getfenv")
                getfenvUsed = true;
            if (node->name == "setfenv")
                setfenvUsed = true;

            return false;
        }
    };

    struct FunctionVisitor : AstVisitor
    {
        Compiler* self;
        std::vector<AstExprFunction*>& functions;

        FunctionVisitor(Compiler* self, std::vector<AstExprFunction*>& functions)
            : self(self)
            , functions(functions)
        {
        }

        bool visit(AstExprFunction* node) override
        {
            node->body->visit(this);

            // this makes sure all functions that are used when compiling this one have been already added to the vector
            functions.push_back(node);

            return false;
        }
    };

    struct UndefinedLocalVisitor : AstVisitor
    {
        UndefinedLocalVisitor(Compiler* self)
            : self(self)
            , undef(nullptr)
        {
        }

        void check(AstLocal* local)
        {
            Local& l = self->locals[local];

            if (!l.allocated && !undef)
                undef = local;
        }

        bool visit(AstExprLocal* node) override
        {
            if (!node->upvalue)
                check(node->local);

            return false;
        }

        bool visit(AstExprFunction* node) override
        {
            const Function* f = self->functions.find(node);
            LUAU_ASSERT(f);

            for (AstLocal* uv : f->upvals)
            {
                LUAU_ASSERT(uv->functionDepth < node->functionDepth);

                if (uv->functionDepth == node->functionDepth - 1)
                    check(uv);
            }

            return false;
        }

        Compiler* self;
        AstLocal* undef;
    };

    struct ConstUpvalueVisitor : AstVisitor
    {
        ConstUpvalueVisitor(Compiler* self)
            : self(self)
        {
        }

        bool visit(AstExprLocal* node) override
        {
            if (node->upvalue && self->isConstant(node))
            {
                upvals.push_back(node->local);
            }

            return false;
        }

        bool visit(AstExprFunction* node) override
        {
            // short-circuits the traversal to make it faster
            return false;
        }

        Compiler* self;
        std::vector<AstLocal*> upvals;
    };

    struct RegScope
    {
        RegScope(Compiler* self)
            : self(self)
            , oldTop(self->regTop)
        {
        }

        // This ctor is useful to forcefully adjust the stack frame in case we know that registers after a certain point are scratch and can be
        // discarded
        RegScope(Compiler* self, unsigned int top)
            : self(self)
            , oldTop(self->regTop)
        {
            LUAU_ASSERT(top <= self->regTop);
            self->regTop = top;
        }

        ~RegScope()
        {
            self->regTop = oldTop;
        }

        Compiler* self;
        unsigned int oldTop;
    };

    struct Function
    {
        uint32_t id;
        std::vector<AstLocal*> upvals;
    };

    struct Local
    {
        uint8_t reg = 0;
        bool allocated = false;
        bool captured = false;
        uint32_t debugpc = 0;
    };

    struct LoopJump
    {
        enum Type
        {
            Break,
            Continue
        };

        Type type;
        size_t label;
    };

    struct Loop
    {
        size_t localOffset;

        AstExpr* untilCondition;
    };

    BytecodeBuilder& bytecode;

    CompileOptions options;

    DenseHashMap<AstExprFunction*, Function> functions;
    DenseHashMap<AstLocal*, Local> locals;
    DenseHashMap<AstName, Global> globals;
    DenseHashMap<AstLocal*, Variable> variables;
    DenseHashMap<AstExpr*, Constant> constants;
    DenseHashMap<AstExprTable*, TableShape> tableShapes;

    unsigned int regTop = 0;
    unsigned int stackSize = 0;

    bool getfenvUsed = false;
    bool setfenvUsed = false;

    std::vector<AstLocal*> localStack;
    std::vector<AstLocal*> upvals;
    std::vector<LoopJump> loopJumps;
    std::vector<Loop> loops;
};

void compileOrThrow(BytecodeBuilder& bytecode, AstStatBlock* root, const AstNameTable& names, const CompileOptions& options)
{
    LUAU_TIMETRACE_SCOPE("compileOrThrow", "Compiler");

    Compiler compiler(bytecode, options);

    // since access to some global objects may result in values that change over time, we block imports from non-readonly tables
    assignMutable(compiler.globals, names, options.mutableGlobals);

    // this pass analyzes mutability of locals/globals and associates locals with their initial values
    trackValues(compiler.globals, compiler.variables, root);

    if (options.optimizationLevel >= 1)
    {
        // this pass analyzes constantness of expressions
        foldConstants(compiler.constants, compiler.variables, root);

        // this pass analyzes table assignments to estimate table shapes for initially empty tables
        predictTableShapes(compiler.tableShapes, root);
    }

    // this visitor tracks calls to getfenv/setfenv and disables some optimizations when they are found
    if (options.optimizationLevel >= 1 && (names.get("getfenv").value || names.get("setfenv").value))
    {
        Compiler::FenvVisitor fenvVisitor(compiler.getfenvUsed, compiler.setfenvUsed);
        root->visit(&fenvVisitor);
    }

    // gathers all functions with the invariant that all function references are to functions earlier in the list
    // for example, function foo() return function() end end will result in two vector entries, [0] = anonymous and [1] = foo
    std::vector<AstExprFunction*> functions;
    Compiler::FunctionVisitor functionVisitor(&compiler, functions);
    root->visit(&functionVisitor);

    for (AstExprFunction* expr : functions)
        compiler.compileFunction(expr);

    AstExprFunction main(root->location, /*generics= */ AstArray<AstGenericType>(), /*genericPacks= */ AstArray<AstGenericTypePack>(),
        /* self= */ nullptr, AstArray<AstLocal*>(), /* vararg= */ Luau::Location(), root, /* functionDepth= */ 0, /* debugname= */ AstName());
    uint32_t mainid = compiler.compileFunction(&main);

    bytecode.setMainFunction(mainid);
    bytecode.finalize();
}

void compileOrThrow(BytecodeBuilder& bytecode, const std::string& source, const CompileOptions& options, const ParseOptions& parseOptions)
{
    Allocator allocator;
    AstNameTable names(allocator);
    ParseResult result = Parser::parse(source.c_str(), source.size(), names, allocator, parseOptions);

    if (!result.errors.empty())
        throw ParseErrors(result.errors);

    AstStatBlock* root = result.root;

    compileOrThrow(bytecode, root, names, options);
}

std::string compile(const std::string& source, const CompileOptions& options, const ParseOptions& parseOptions, BytecodeEncoder* encoder)
{
    LUAU_TIMETRACE_SCOPE("compile", "Compiler");

    Allocator allocator;
    AstNameTable names(allocator);
    ParseResult result = Parser::parse(source.c_str(), source.size(), names, allocator, parseOptions);

    if (!result.errors.empty())
    {
        // Users of this function expect only a single error message
        const Luau::ParseError& parseError = result.errors.front();
        std::string error = format(":%d: %s", parseError.getLocation().begin.line + 1, parseError.what());

        return BytecodeBuilder::getError(error);
    }

    try
    {
        BytecodeBuilder bcb(encoder);
        compileOrThrow(bcb, result.root, names, options);

        return bcb.getBytecode();
    }
    catch (CompileError& e)
    {
        std::string error = format(":%d: %s", e.getLocation().begin.line + 1, e.what());
        return BytecodeBuilder::getError(error);
    }
}

} // namespace Luau
