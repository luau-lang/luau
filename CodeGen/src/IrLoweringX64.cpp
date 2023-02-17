// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrLoweringX64.h"

#include "Luau/CodeGen.h"
#include "Luau/DenseHash.h"
#include "Luau/IrAnalysis.h"
#include "Luau/IrDump.h"
#include "Luau/IrUtils.h"

#include "EmitCommonX64.h"
#include "EmitInstructionX64.h"
#include "NativeState.h"

#include "lstate.h"

#include <algorithm>

namespace Luau
{
namespace CodeGen
{

static const RegisterX64 kGprAllocOrder[] = {rax, rdx, rcx, rbx, rsi, rdi, r8, r9, r10, r11};

IrLoweringX64::IrLoweringX64(AssemblyBuilderX64& build, ModuleHelpers& helpers, NativeState& data, Proto* proto, IrFunction& function)
    : build(build)
    , helpers(helpers)
    , data(data)
    , proto(proto)
    , function(function)
{
    freeGprMap.fill(true);
    freeXmmMap.fill(true);

    // In order to allocate registers during lowering, we need to know where instruction results are last used
    updateLastUseLocations(function);
}

void IrLoweringX64::lower(AssemblyOptions options)
{
    // While we will need a better block ordering in the future, right now we want to mostly preserve build order with fallbacks outlined
    std::vector<uint32_t> sortedBlocks;
    sortedBlocks.reserve(function.blocks.size());
    for (uint32_t i = 0; i < function.blocks.size(); i++)
        sortedBlocks.push_back(i);

    std::sort(sortedBlocks.begin(), sortedBlocks.end(), [&](uint32_t idxA, uint32_t idxB) {
        const IrBlock& a = function.blocks[idxA];
        const IrBlock& b = function.blocks[idxB];

        // Place fallback blocks at the end
        if ((a.kind == IrBlockKind::Fallback) != (b.kind == IrBlockKind::Fallback))
            return (a.kind == IrBlockKind::Fallback) < (b.kind == IrBlockKind::Fallback);

        // Try to order by instruction order
        return a.start < b.start;
    });

    DenseHashMap<uint32_t, uint32_t> bcLocations{~0u};

    // Create keys for IR assembly locations that original bytecode instruction are interested in
    for (const auto& [irLocation, asmLocation] : function.bcMapping)
    {
        if (irLocation != ~0u)
            bcLocations[irLocation] = 0;
    }

    DenseHashMap<uint32_t, uint32_t> indexIrToBc{~0u};
    bool outputEnabled = options.includeAssembly || options.includeIr;

    if (outputEnabled && options.annotator)
    {
        // Create reverse mapping from IR location to bytecode location
        for (size_t i = 0; i < function.bcMapping.size(); ++i)
        {
            uint32_t irLocation = function.bcMapping[i].irLocation;

            if (irLocation != ~0u)
                indexIrToBc[irLocation] = uint32_t(i);
        }
    }

    IrToStringContext ctx{build.text, function.blocks, function.constants};

    // We use this to skip outlined fallback blocks from IR/asm text output
    size_t textSize = build.text.length();
    uint32_t codeSize = build.getCodeSize();
    bool seenFallback = false;

    IrBlock dummy;
    dummy.start = ~0u;

    for (size_t i = 0; i < sortedBlocks.size(); ++i)
    {
        uint32_t blockIndex = sortedBlocks[i];

        IrBlock& block = function.blocks[blockIndex];
        LUAU_ASSERT(block.start != ~0u);

        if (block.kind == IrBlockKind::Dead)
            continue;

        // If we want to skip fallback code IR/asm, we'll record when those blocks start once we see them
        if (block.kind == IrBlockKind::Fallback && !seenFallback)
        {
            textSize = build.text.length();
            codeSize = build.getCodeSize();
            seenFallback = true;
        }

        if (options.includeIr)
        {
            build.logAppend("# ");
            toStringDetailed(ctx, block, blockIndex);
        }

        build.setLabel(block.label);

        for (uint32_t index = block.start; true; index++)
        {
            LUAU_ASSERT(index < function.instructions.size());

            // If IR instruction is the first one for the original bytecode, we can annotate it with source code text
            if (outputEnabled && options.annotator)
            {
                if (uint32_t* bcIndex = indexIrToBc.find(index))
                    options.annotator(options.annotatorContext, build.text, proto->bytecodeid, *bcIndex);
            }

            // If bytecode needs the location of this instruction for jumps, record it
            if (uint32_t* bcLocation = bcLocations.find(index))
                *bcLocation = build.getCodeSize();

            IrInst& inst = function.instructions[index];

            // Skip pseudo instructions, but make sure they are not used at this stage
            // This also prevents them from getting into text output when that's enabled
            if (isPseudo(inst.cmd))
            {
                LUAU_ASSERT(inst.useCount == 0);
                continue;
            }

            if (options.includeIr)
            {
                build.logAppend("# ");
                toStringDetailed(ctx, inst, index);
            }

            IrBlock& next = i + 1 < sortedBlocks.size() ? function.blocks[sortedBlocks[i + 1]] : dummy;

            lowerInst(inst, index, next);

            freeLastUseRegs(inst, index);

            if (isBlockTerminator(inst.cmd))
            {
                if (options.includeIr)
                    build.logAppend("#\n");
                break;
            }
        }
    }

    if (outputEnabled && !options.includeOutlinedCode && seenFallback)
    {
        build.text.resize(textSize);

        if (options.includeAssembly)
            build.logAppend("; skipping %u bytes of outlined code\n", build.getCodeSize() - codeSize);
    }

    // Copy assembly locations of IR instructions that are mapped to bytecode instructions
    for (auto& [irLocation, asmLocation] : function.bcMapping)
    {
        if (irLocation != ~0u)
            asmLocation = bcLocations[irLocation];
    }
}

void IrLoweringX64::lowerInst(IrInst& inst, uint32_t index, IrBlock& next)
{
    switch (inst.cmd)
    {
    case IrCmd::LOAD_TAG:
        inst.regX64 = allocGprReg(SizeX64::dword);

        if (inst.a.kind == IrOpKind::VmReg)
            build.mov(inst.regX64, luauRegTag(inst.a.index));
        else if (inst.a.kind == IrOpKind::VmConst)
            build.mov(inst.regX64, luauConstantTag(inst.a.index));
        // If we have a register, we assume it's a pointer to TValue
        // We might introduce explicit operand types in the future to make this more robust
        else if (inst.a.kind == IrOpKind::Inst)
            build.mov(inst.regX64, dword[regOp(inst.a) + offsetof(TValue, tt)]);
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::LOAD_POINTER:
        inst.regX64 = allocGprReg(SizeX64::qword);

        if (inst.a.kind == IrOpKind::VmReg)
            build.mov(inst.regX64, luauRegValue(inst.a.index));
        else if (inst.a.kind == IrOpKind::VmConst)
            build.mov(inst.regX64, luauConstantValue(inst.a.index));
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::LOAD_DOUBLE:
        inst.regX64 = allocXmmReg();

        if (inst.a.kind == IrOpKind::VmReg)
            build.vmovsd(inst.regX64, luauRegValue(inst.a.index));
        else if (inst.a.kind == IrOpKind::VmConst)
            build.vmovsd(inst.regX64, luauConstantValue(inst.a.index));
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::LOAD_INT:
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);

        inst.regX64 = allocGprReg(SizeX64::dword);

        build.mov(inst.regX64, luauRegValueInt(inst.a.index));
        break;
    case IrCmd::LOAD_TVALUE:
        inst.regX64 = allocXmmReg();

        if (inst.a.kind == IrOpKind::VmReg)
            build.vmovups(inst.regX64, luauReg(inst.a.index));
        else if (inst.a.kind == IrOpKind::VmConst)
            build.vmovups(inst.regX64, luauConstant(inst.a.index));
        else if (inst.a.kind == IrOpKind::Inst)
            build.vmovups(inst.regX64, xmmword[regOp(inst.a)]);
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::LOAD_NODE_VALUE_TV:
        inst.regX64 = allocXmmReg();

        build.vmovups(inst.regX64, luauNodeValue(regOp(inst.a)));
        break;
    case IrCmd::LOAD_ENV:
        inst.regX64 = allocGprReg(SizeX64::qword);

        build.mov(inst.regX64, sClosure);
        build.mov(inst.regX64, qword[inst.regX64 + offsetof(Closure, env)]);
        break;
    case IrCmd::GET_ARR_ADDR:
        if (inst.b.kind == IrOpKind::Inst)
        {
            inst.regX64 = allocGprRegOrReuse(SizeX64::qword, index, {inst.b});

            if (dwordReg(inst.regX64) != regOp(inst.b))
                build.mov(dwordReg(inst.regX64), regOp(inst.b));

            build.shl(dwordReg(inst.regX64), kTValueSizeLog2);
            build.add(inst.regX64, qword[regOp(inst.a) + offsetof(Table, array)]);
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            inst.regX64 = allocGprRegOrReuse(SizeX64::qword, index, {inst.a});

            build.mov(inst.regX64, qword[regOp(inst.a) + offsetof(Table, array)]);

            if (intOp(inst.b) != 0)
                build.lea(inst.regX64, addr[inst.regX64 + intOp(inst.b) * sizeof(TValue)]);
        }
        else
        {
            LUAU_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::GET_SLOT_NODE_ADDR:
    {
        inst.regX64 = allocGprReg(SizeX64::qword);

        ScopedReg tmp{*this, SizeX64::qword};

        getTableNodeAtCachedSlot(build, tmp.reg, inst.regX64, regOp(inst.a), uintOp(inst.b));
        break;
    }
    case IrCmd::STORE_TAG:
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);

        if (inst.b.kind == IrOpKind::Constant)
            build.mov(luauRegTag(inst.a.index), tagOp(inst.b));
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::STORE_POINTER:
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);

        build.mov(luauRegValue(inst.a.index), regOp(inst.b));
        break;
    case IrCmd::STORE_DOUBLE:
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);

        if (inst.b.kind == IrOpKind::Constant)
        {
            ScopedReg tmp{*this, SizeX64::xmmword};

            build.vmovsd(tmp.reg, build.f64(doubleOp(inst.b)));
            build.vmovsd(luauRegValue(inst.a.index), tmp.reg);
        }
        else if (inst.b.kind == IrOpKind::Inst)
        {
            build.vmovsd(luauRegValue(inst.a.index), regOp(inst.b));
        }
        else
        {
            LUAU_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::STORE_INT:
    {
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);

        if (inst.b.kind == IrOpKind::Constant)
            build.mov(luauRegValueInt(inst.a.index), intOp(inst.b));
        else if (inst.b.kind == IrOpKind::Inst)
            build.mov(luauRegValueInt(inst.a.index), regOp(inst.b));
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    }
    case IrCmd::STORE_TVALUE:
        if (inst.a.kind == IrOpKind::VmReg)
            build.vmovups(luauReg(inst.a.index), regOp(inst.b));
        else if (inst.a.kind == IrOpKind::Inst)
            build.vmovups(xmmword[regOp(inst.a)], regOp(inst.b));
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::STORE_NODE_VALUE_TV:
        build.vmovups(luauNodeValue(regOp(inst.a)), regOp(inst.b));
        break;
    case IrCmd::ADD_INT:
        inst.regX64 = allocGprRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.regX64 == regOp(inst.a) && intOp(inst.b) == 1)
            build.inc(inst.regX64);
        else if (inst.regX64 == regOp(inst.a))
            build.add(inst.regX64, intOp(inst.b));
        else
            build.lea(inst.regX64, addr[regOp(inst.a) + intOp(inst.b)]);
        break;
    case IrCmd::SUB_INT:
        inst.regX64 = allocGprRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.regX64 == regOp(inst.a) && intOp(inst.b) == 1)
            build.dec(inst.regX64);
        else if (inst.regX64 == regOp(inst.a))
            build.sub(inst.regX64, intOp(inst.b));
        else
            build.lea(inst.regX64, addr[regOp(inst.a) - intOp(inst.b)]);
        break;
    case IrCmd::ADD_NUM:
        inst.regX64 = allocXmmRegOrReuse(index, {inst.a, inst.b});

        build.vaddsd(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b));
        break;
    case IrCmd::SUB_NUM:
        inst.regX64 = allocXmmRegOrReuse(index, {inst.a, inst.b});

        build.vsubsd(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b));
        break;
    case IrCmd::MUL_NUM:
        inst.regX64 = allocXmmRegOrReuse(index, {inst.a, inst.b});

        build.vmulsd(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b));
        break;
    case IrCmd::DIV_NUM:
        inst.regX64 = allocXmmRegOrReuse(index, {inst.a, inst.b});

        build.vdivsd(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b));
        break;
    case IrCmd::MOD_NUM:
    {
        inst.regX64 = allocXmmRegOrReuse(index, {inst.a, inst.b});

        RegisterX64 lhs = regOp(inst.a);

        if (inst.b.kind == IrOpKind::Inst)
        {
            ScopedReg tmp{*this, SizeX64::xmmword};

            build.vdivsd(tmp.reg, lhs, memRegDoubleOp(inst.b));
            build.vroundsd(tmp.reg, tmp.reg, tmp.reg, RoundingModeX64::RoundToNegativeInfinity);
            build.vmulsd(tmp.reg, tmp.reg, memRegDoubleOp(inst.b));
            build.vsubsd(inst.regX64, lhs, tmp.reg);
        }
        else
        {
            ScopedReg tmp1{*this, SizeX64::xmmword};
            ScopedReg tmp2{*this, SizeX64::xmmword};

            build.vmovsd(tmp1.reg, memRegDoubleOp(inst.b));
            build.vdivsd(tmp2.reg, lhs, tmp1.reg);
            build.vroundsd(tmp2.reg, tmp2.reg, tmp2.reg, RoundingModeX64::RoundToNegativeInfinity);
            build.vmulsd(tmp1.reg, tmp2.reg, tmp1.reg);
            build.vsubsd(inst.regX64, lhs, tmp1.reg);
        }
        break;
    }
    case IrCmd::POW_NUM:
    {
        inst.regX64 = allocXmmRegOrReuse(index, {inst.a, inst.b});

        RegisterX64 lhs = regOp(inst.a);

        if (inst.b.kind == IrOpKind::Inst)
        {
            // TODO: this doesn't happen with current local-only register allocation, but has to be handled in the future
            LUAU_ASSERT(regOp(inst.b) != xmm0);

            if (lhs != xmm0)
                build.vmovsd(xmm0, lhs, lhs);

            if (regOp(inst.b) != xmm1)
                build.vmovsd(xmm1, regOp(inst.b), regOp(inst.b));

            build.call(qword[rNativeContext + offsetof(NativeContext, libm_pow)]);

            if (inst.regX64 != xmm0)
                build.vmovsd(inst.regX64, xmm0, xmm0);
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            double rhs = doubleOp(inst.b);

            if (rhs == 2.0)
            {
                build.vmulsd(inst.regX64, lhs, lhs);
            }
            else if (rhs == 0.5)
            {
                build.vsqrtsd(inst.regX64, lhs, lhs);
            }
            else if (rhs == 3.0)
            {
                ScopedReg tmp{*this, SizeX64::xmmword};

                build.vmulsd(tmp.reg, lhs, lhs);
                build.vmulsd(inst.regX64, lhs, tmp.reg);
            }
            else
            {
                if (lhs != xmm0)
                    build.vmovsd(xmm0, xmm0, lhs);

                build.vmovsd(xmm1, build.f64(rhs));
                build.call(qword[rNativeContext + offsetof(NativeContext, libm_pow)]);

                if (inst.regX64 != xmm0)
                    build.vmovsd(inst.regX64, xmm0, xmm0);
            }
        }
        else
        {
            if (lhs != xmm0)
                build.vmovsd(xmm0, lhs, lhs);

            build.vmovsd(xmm1, memRegDoubleOp(inst.b));
            build.call(qword[rNativeContext + offsetof(NativeContext, libm_pow)]);

            if (inst.regX64 != xmm0)
                build.vmovsd(inst.regX64, xmm0, xmm0);
        }

        break;
    }
    case IrCmd::UNM_NUM:
    {
        inst.regX64 = allocXmmRegOrReuse(index, {inst.a});

        RegisterX64 src = regOp(inst.a);

        if (inst.regX64 == src)
        {
            build.vxorpd(inst.regX64, inst.regX64, build.f64(-0.0));
        }
        else
        {
            build.vmovsd(inst.regX64, src, src);
            build.vxorpd(inst.regX64, inst.regX64, build.f64(-0.0));
        }

        break;
    }
    case IrCmd::NOT_ANY:
    {
        // TODO: if we have a single user which is a STORE_INT, we are missing the opportunity to write directly to target
        inst.regX64 = allocGprRegOrReuse(SizeX64::dword, index, {inst.a, inst.b});

        Label saveone, savezero, exit;

        build.cmp(regOp(inst.a), LUA_TNIL);
        build.jcc(ConditionX64::Equal, saveone);

        build.cmp(regOp(inst.a), LUA_TBOOLEAN);
        build.jcc(ConditionX64::NotEqual, savezero);

        build.cmp(regOp(inst.b), 0);
        build.jcc(ConditionX64::Equal, saveone);

        build.setLabel(savezero);
        build.mov(inst.regX64, 0);
        build.jmp(exit);

        build.setLabel(saveone);
        build.mov(inst.regX64, 1);

        build.setLabel(exit);
        break;
    }
    case IrCmd::JUMP:
        jumpOrFallthrough(blockOp(inst.a), next);
        break;
    case IrCmd::JUMP_IF_TRUTHY:
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);

        jumpIfTruthy(build, inst.a.index, labelOp(inst.b), labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.c), next);
        break;
    case IrCmd::JUMP_IF_FALSY:
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);

        jumpIfFalsy(build, inst.a.index, labelOp(inst.b), labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.c), next);
        break;
    case IrCmd::JUMP_EQ_TAG:
    {
        LUAU_ASSERT(inst.b.kind == IrOpKind::Inst || inst.b.kind == IrOpKind::Constant);
        OperandX64 opb = inst.b.kind == IrOpKind::Inst ? regOp(inst.b) : OperandX64(tagOp(inst.b));

        build.cmp(memRegTagOp(inst.a), opb);

        if (isFallthroughBlock(blockOp(inst.d), next))
        {
            build.jcc(ConditionX64::Equal, labelOp(inst.c));
            jumpOrFallthrough(blockOp(inst.d), next);
        }
        else
        {
            build.jcc(ConditionX64::NotEqual, labelOp(inst.d));
            jumpOrFallthrough(blockOp(inst.c), next);
        }
        break;
    }
    case IrCmd::JUMP_EQ_INT:
        build.cmp(regOp(inst.a), intOp(inst.b));

        build.jcc(ConditionX64::Equal, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::JUMP_EQ_POINTER:
        build.cmp(regOp(inst.a), regOp(inst.b));

        build.jcc(ConditionX64::Equal, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::JUMP_CMP_NUM:
    {
        LUAU_ASSERT(inst.c.kind == IrOpKind::Condition);

        IrCondition cond = IrCondition(inst.c.index);

        ScopedReg tmp{*this, SizeX64::xmmword};

        // TODO: jumpOnNumberCmp should work on IrCondition directly
        jumpOnNumberCmp(build, tmp.reg, memRegDoubleOp(inst.a), memRegDoubleOp(inst.b), getX64Condition(cond), labelOp(inst.d));
        jumpOrFallthrough(blockOp(inst.e), next);
        break;
    }
    case IrCmd::JUMP_CMP_ANY:
    {
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Condition);

        IrCondition cond = IrCondition(inst.c.index);

        jumpOnAnyCmpFallback(build, inst.a.index, inst.b.index, getX64Condition(cond), labelOp(inst.d));
        jumpOrFallthrough(blockOp(inst.e), next);
        break;
    }
    case IrCmd::TABLE_LEN:
        inst.regX64 = allocXmmReg();

        build.mov(rArg1, regOp(inst.a));
        build.call(qword[rNativeContext + offsetof(NativeContext, luaH_getn)]);
        build.vcvtsi2sd(inst.regX64, inst.regX64, eax);
        break;
    case IrCmd::NEW_TABLE:
        inst.regX64 = allocGprReg(SizeX64::qword);

        build.mov(rArg1, rState);
        build.mov(dwordReg(rArg2), uintOp(inst.a));
        build.mov(dwordReg(rArg3), uintOp(inst.b));
        build.call(qword[rNativeContext + offsetof(NativeContext, luaH_new)]);

        if (inst.regX64 != rax)
            build.mov(inst.regX64, rax);
        break;
    case IrCmd::DUP_TABLE:
        inst.regX64 = allocGprReg(SizeX64::qword);

        // Re-ordered to avoid register conflict
        build.mov(rArg2, regOp(inst.a));
        build.mov(rArg1, rState);
        build.call(qword[rNativeContext + offsetof(NativeContext, luaH_clone)]);

        if (inst.regX64 != rax)
            build.mov(inst.regX64, rax);
        break;
    case IrCmd::NUM_TO_INDEX:
    {
        inst.regX64 = allocGprReg(SizeX64::dword);

        ScopedReg tmp{*this, SizeX64::xmmword};

        convertNumberToIndexOrJump(build, tmp.reg, regOp(inst.a), inst.regX64, labelOp(inst.b));
        break;
    }
    case IrCmd::INT_TO_NUM:
        inst.regX64 = allocXmmReg();

        build.vcvtsi2sd(inst.regX64, inst.regX64, regOp(inst.a));
        break;
    case IrCmd::DO_ARITH:
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg || inst.c.kind == IrOpKind::VmConst);

        if (inst.c.kind == IrOpKind::VmReg)
            callArithHelper(build, inst.a.index, inst.b.index, luauRegAddress(inst.c.index), TMS(intOp(inst.d)));
        else
            callArithHelper(build, inst.a.index, inst.b.index, luauConstantAddress(inst.c.index), TMS(intOp(inst.d)));
        break;
    case IrCmd::DO_LEN:
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);

        callLengthHelper(build, inst.a.index, inst.b.index);
        break;
    case IrCmd::GET_TABLE:
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);

        if (inst.c.kind == IrOpKind::VmReg)
        {
            callGetTable(build, inst.b.index, luauRegAddress(inst.c.index), inst.a.index);
        }
        else if (inst.c.kind == IrOpKind::Constant)
        {
            TValue n;
            setnvalue(&n, uintOp(inst.c));
            callGetTable(build, inst.b.index, build.bytes(&n, sizeof(n)), inst.a.index);
        }
        else
        {
            LUAU_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::SET_TABLE:
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);

        if (inst.c.kind == IrOpKind::VmReg)
        {
            callSetTable(build, inst.b.index, luauRegAddress(inst.c.index), inst.a.index);
        }
        else if (inst.c.kind == IrOpKind::Constant)
        {
            TValue n;
            setnvalue(&n, uintOp(inst.c));
            callSetTable(build, inst.b.index, build.bytes(&n, sizeof(n)), inst.a.index);
        }
        else
        {
            LUAU_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::GET_IMPORT:
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);

        emitInstGetImportFallback(build, inst.a.index, uintOp(inst.b));
        break;
    case IrCmd::CONCAT:
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);

        build.mov(rArg1, rState);
        build.mov(dwordReg(rArg2), uintOp(inst.b));
        build.mov(dwordReg(rArg3), inst.a.index + uintOp(inst.b) - 1);
        build.call(qword[rNativeContext + offsetof(NativeContext, luaV_concat)]);

        emitUpdateBase(build);
        break;
    case IrCmd::GET_UPVALUE:
    {
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmUpvalue);

        ScopedReg tmp1{*this, SizeX64::qword};
        ScopedReg tmp2{*this, SizeX64::xmmword};

        build.mov(tmp1.reg, sClosure);
        build.add(tmp1.reg, offsetof(Closure, l.uprefs) + sizeof(TValue) * inst.b.index);

        // uprefs[] is either an actual value, or it points to UpVal object which has a pointer to value
        Label skip;
        // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
        build.cmp(dword[tmp1.reg + offsetof(TValue, tt)], LUA_TUPVAL);
        build.jcc(ConditionX64::NotEqual, skip);

        // UpVal.v points to the value (either on stack, or on heap inside each UpVal, but we can deref it unconditionally)
        build.mov(tmp1.reg, qword[tmp1.reg + offsetof(TValue, value.gc)]);
        build.mov(tmp1.reg, qword[tmp1.reg + offsetof(UpVal, v)]);

        build.setLabel(skip);

        build.vmovups(tmp2.reg, xmmword[tmp1.reg]);
        build.vmovups(luauReg(inst.a.index), tmp2.reg);
        break;
    }
    case IrCmd::SET_UPVALUE:
    {
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmUpvalue);
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);

        Label next;
        ScopedReg tmp1{*this, SizeX64::qword};
        ScopedReg tmp2{*this, SizeX64::qword};
        ScopedReg tmp3{*this, SizeX64::xmmword};

        build.mov(tmp1.reg, sClosure);
        build.mov(tmp2.reg, qword[tmp1.reg + offsetof(Closure, l.uprefs) + sizeof(TValue) * inst.a.index + offsetof(TValue, value.gc)]);

        build.mov(tmp1.reg, qword[tmp2.reg + offsetof(UpVal, v)]);
        build.vmovups(tmp3.reg, luauReg(inst.b.index));
        build.vmovups(xmmword[tmp1.reg], tmp3.reg);

        callBarrierObject(build, tmp1.reg, tmp2.reg, inst.b.index, next);
        build.setLabel(next);
        break;
    }
    case IrCmd::PREPARE_FORN:
        callPrepareForN(build, inst.a.index, inst.b.index, inst.c.index);
        break;
    case IrCmd::CHECK_TAG:
        if (inst.a.kind == IrOpKind::Inst)
        {
            build.cmp(regOp(inst.a), tagOp(inst.b));
            build.jcc(ConditionX64::NotEqual, labelOp(inst.c));
        }
        else if (inst.a.kind == IrOpKind::VmReg)
        {
            jumpIfTagIsNot(build, inst.a.index, lua_Type(tagOp(inst.b)), labelOp(inst.c));
        }
        else
        {
            LUAU_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::CHECK_READONLY:
        jumpIfTableIsReadOnly(build, regOp(inst.a), labelOp(inst.b));
        break;
    case IrCmd::CHECK_NO_METATABLE:
        jumpIfMetatablePresent(build, regOp(inst.a), labelOp(inst.b));
        break;
    case IrCmd::CHECK_SAFE_ENV:
    {
        ScopedReg tmp{*this, SizeX64::qword};

        jumpIfUnsafeEnv(build, tmp.reg, labelOp(inst.a));
        break;
    }
    case IrCmd::CHECK_ARRAY_SIZE:
        if (inst.b.kind == IrOpKind::Inst)
            build.cmp(dword[regOp(inst.a) + offsetof(Table, sizearray)], regOp(inst.b));
        else if (inst.b.kind == IrOpKind::Constant)
            build.cmp(dword[regOp(inst.a) + offsetof(Table, sizearray)], intOp(inst.b));
        else
            LUAU_ASSERT(!"Unsupported instruction form");

        build.jcc(ConditionX64::BelowEqual, labelOp(inst.c));
        break;
    case IrCmd::CHECK_SLOT_MATCH:
    {
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmConst);

        ScopedReg tmp{*this, SizeX64::qword};

        jumpIfNodeKeyNotInExpectedSlot(build, tmp.reg, regOp(inst.a), luauConstantValue(inst.b.index), labelOp(inst.c));
        break;
    }
    case IrCmd::INTERRUPT:
        emitInterrupt(build, uintOp(inst.a));
        break;
    case IrCmd::CHECK_GC:
    {
        Label skip;
        callCheckGc(build, -1, false, skip);
        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_OBJ:
    {
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);

        Label skip;
        ScopedReg tmp{*this, SizeX64::qword};

        callBarrierObject(build, tmp.reg, regOp(inst.a), inst.b.index, skip);
        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_TABLE_BACK:
    {
        Label skip;

        callBarrierTableFast(build, regOp(inst.a), skip);
        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_TABLE_FORWARD:
    {
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);

        Label skip;
        ScopedReg tmp{*this, SizeX64::qword};

        callBarrierTable(build, tmp.reg, regOp(inst.a), inst.b.index, skip);
        build.setLabel(skip);
        break;
    }
    case IrCmd::SET_SAVEDPC:
    {
        // This is like emitSetSavedPc, but using register allocation instead of relying on rax/rdx
        ScopedReg tmp1{*this, SizeX64::qword};
        ScopedReg tmp2{*this, SizeX64::qword};

        build.mov(tmp2.reg, sCode);
        build.add(tmp2.reg, uintOp(inst.a) * sizeof(Instruction));
        build.mov(tmp1.reg, qword[rState + offsetof(lua_State, ci)]);
        build.mov(qword[tmp1.reg + offsetof(CallInfo, savedpc)], tmp2.reg);
        break;
    }
    case IrCmd::CLOSE_UPVALS:
    {
        LUAU_ASSERT(inst.a.kind == IrOpKind::VmReg);

        Label next;
        ScopedReg tmp1{*this, SizeX64::qword};
        ScopedReg tmp2{*this, SizeX64::qword};

        // L->openupval != 0
        build.mov(tmp1.reg, qword[rState + offsetof(lua_State, openupval)]);
        build.test(tmp1.reg, tmp1.reg);
        build.jcc(ConditionX64::Zero, next);

        // ra <= L->openuval->v
        build.lea(tmp2.reg, addr[rBase + inst.a.index * sizeof(TValue)]);
        build.cmp(tmp2.reg, qword[tmp1.reg + offsetof(UpVal, v)]);
        build.jcc(ConditionX64::Above, next);

        if (rArg2 != tmp2.reg)
            build.mov(rArg2, tmp2.reg);

        build.mov(rArg1, rState);
        build.call(qword[rNativeContext + offsetof(NativeContext, luaF_close)]);

        build.setLabel(next);
        break;
    }
    case IrCmd::CAPTURE:
        // No-op right now
        break;

        // Fallbacks to non-IR instruction implementations
    case IrCmd::LOP_SETLIST:
    {
        const Instruction* pc = proto->code + uintOp(inst.a);
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::Constant);
        LUAU_ASSERT(inst.e.kind == IrOpKind::Constant);

        Label next;
        emitInstSetList(build, pc, next);
        build.setLabel(next);
        break;
    }
    case IrCmd::LOP_NAMECALL:
    {
        const Instruction* pc = proto->code + uintOp(inst.a);
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);

        emitInstNameCall(build, pc, uintOp(inst.a), proto->k, blockOp(inst.d).label, blockOp(inst.e).label);
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    }
    case IrCmd::LOP_CALL:
    {
        const Instruction* pc = proto->code + uintOp(inst.a);
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);
        LUAU_ASSERT(inst.d.kind == IrOpKind::Constant);

        emitInstCall(build, helpers, pc, uintOp(inst.a));
        break;
    }
    case IrCmd::LOP_RETURN:
    {
        const Instruction* pc = proto->code + uintOp(inst.a);
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);

        emitInstReturn(build, helpers, pc, uintOp(inst.a));
        break;
    }
    case IrCmd::LOP_FASTCALL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);

        emitInstFastCall(build, proto->code + uintOp(inst.a), uintOp(inst.a), labelOp(inst.d));
        break;
    case IrCmd::LOP_FASTCALL1:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);

        emitInstFastCall1(build, proto->code + uintOp(inst.a), uintOp(inst.a), labelOp(inst.d));
        break;
    case IrCmd::LOP_FASTCALL2:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmReg);

        emitInstFastCall2(build, proto->code + uintOp(inst.a), uintOp(inst.a), labelOp(inst.e));
        break;
    case IrCmd::LOP_FASTCALL2K:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitInstFastCall2K(build, proto->code + uintOp(inst.a), uintOp(inst.a), labelOp(inst.e));
        break;
    case IrCmd::LOP_FORGLOOP:
        emitinstForGLoop(build, proto->code + uintOp(inst.a), uintOp(inst.a), labelOp(inst.b), labelOp(inst.c), labelOp(inst.d));
        break;
    case IrCmd::LOP_FORGLOOP_FALLBACK:
        emitinstForGLoopFallback(build, proto->code + uintOp(inst.a), uintOp(inst.a), labelOp(inst.b));
        build.jmp(labelOp(inst.c));
        break;
    case IrCmd::LOP_FORGPREP_XNEXT_FALLBACK:
        emitInstForGPrepXnextFallback(build, proto->code + uintOp(inst.a), uintOp(inst.a), labelOp(inst.b));
        break;
    case IrCmd::LOP_AND:
        emitInstAnd(build, proto->code + uintOp(inst.a));
        break;
    case IrCmd::LOP_ANDK:
        emitInstAndK(build, proto->code + uintOp(inst.a));
        break;
    case IrCmd::LOP_OR:
        emitInstOr(build, proto->code + uintOp(inst.a));
        break;
    case IrCmd::LOP_ORK:
        emitInstOrK(build, proto->code + uintOp(inst.a));
        break;
    case IrCmd::LOP_COVERAGE:
        emitInstCoverage(build, uintOp(inst.a));
        break;

        // Full instruction fallbacks
    case IrCmd::FALLBACK_GETGLOBAL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        emitFallback(build, data, LOP_GETGLOBAL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETGLOBAL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        emitFallback(build, data, LOP_SETGLOBAL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETTABLEKS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitFallback(build, data, LOP_GETTABLEKS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETTABLEKS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitFallback(build, data, LOP_SETTABLEKS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_NAMECALL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitFallback(build, data, LOP_NAMECALL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_PREPVARARGS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::Constant);

        emitFallback(build, data, LOP_PREPVARARGS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETVARARGS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);

        emitFallback(build, data, LOP_GETVARARGS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_NEWCLOSURE:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);

        emitFallback(build, data, LOP_NEWCLOSURE, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_DUPCLOSURE:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        emitFallback(build, data, LOP_DUPCLOSURE, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_FORGPREP:
        emitFallback(build, data, LOP_FORGPREP, uintOp(inst.a));
        break;
    default:
        LUAU_ASSERT(!"Not supported yet");
        break;
    }
}

bool IrLoweringX64::isFallthroughBlock(IrBlock target, IrBlock next)
{
    return target.start == next.start;
}

void IrLoweringX64::jumpOrFallthrough(IrBlock& target, IrBlock& next)
{
    if (!isFallthroughBlock(target, next))
        build.jmp(target.label);
}

OperandX64 IrLoweringX64::memRegDoubleOp(IrOp op) const
{
    switch (op.kind)
    {
    case IrOpKind::Inst:
        return regOp(op);
    case IrOpKind::Constant:
        return build.f64(doubleOp(op));
    case IrOpKind::VmReg:
        return luauRegValue(op.index);
    case IrOpKind::VmConst:
        return luauConstantValue(op.index);
    default:
        LUAU_ASSERT(!"Unsupported operand kind");
    }

    return noreg;
}

OperandX64 IrLoweringX64::memRegTagOp(IrOp op) const
{
    switch (op.kind)
    {
    case IrOpKind::Inst:
        return regOp(op);
    case IrOpKind::VmReg:
        return luauRegTag(op.index);
    case IrOpKind::VmConst:
        return luauConstantTag(op.index);
    default:
        LUAU_ASSERT(!"Unsupported operand kind");
    }

    return noreg;
}

RegisterX64 IrLoweringX64::regOp(IrOp op) const
{
    return function.instOp(op).regX64;
}

IrConst IrLoweringX64::constOp(IrOp op) const
{
    return function.constOp(op);
}

uint8_t IrLoweringX64::tagOp(IrOp op) const
{
    return function.tagOp(op);
}

bool IrLoweringX64::boolOp(IrOp op) const
{
    return function.boolOp(op);
}

int IrLoweringX64::intOp(IrOp op) const
{
    return function.intOp(op);
}

unsigned IrLoweringX64::uintOp(IrOp op) const
{
    return function.uintOp(op);
}

double IrLoweringX64::doubleOp(IrOp op) const
{
    return function.doubleOp(op);
}

IrBlock& IrLoweringX64::blockOp(IrOp op) const
{
    return function.blockOp(op);
}

Label& IrLoweringX64::labelOp(IrOp op) const
{
    return blockOp(op).label;
}

RegisterX64 IrLoweringX64::allocGprReg(SizeX64 preferredSize)
{
    LUAU_ASSERT(
        preferredSize == SizeX64::byte || preferredSize == SizeX64::word || preferredSize == SizeX64::dword || preferredSize == SizeX64::qword);

    for (RegisterX64 reg : kGprAllocOrder)
    {
        if (freeGprMap[reg.index])
        {
            freeGprMap[reg.index] = false;
            return RegisterX64{preferredSize, reg.index};
        }
    }

    LUAU_ASSERT(!"Out of GPR registers to allocate");
    return noreg;
}

RegisterX64 IrLoweringX64::allocXmmReg()
{
    for (size_t i = 0; i < freeXmmMap.size(); ++i)
    {
        if (freeXmmMap[i])
        {
            freeXmmMap[i] = false;
            return RegisterX64{SizeX64::xmmword, uint8_t(i)};
        }
    }

    LUAU_ASSERT(!"Out of XMM registers to allocate");
    return noreg;
}

RegisterX64 IrLoweringX64::allocGprRegOrReuse(SizeX64 preferredSize, uint32_t index, std::initializer_list<IrOp> oprefs)
{
    for (IrOp op : oprefs)
    {
        if (op.kind != IrOpKind::Inst)
            continue;

        IrInst& source = function.instructions[op.index];

        if (source.lastUse == index && !source.reusedReg)
        {
            LUAU_ASSERT(source.regX64.size != SizeX64::xmmword);
            LUAU_ASSERT(source.regX64 != noreg);

            source.reusedReg = true;
            return RegisterX64{preferredSize, source.regX64.index};
        }
    }

    return allocGprReg(preferredSize);
}

RegisterX64 IrLoweringX64::allocXmmRegOrReuse(uint32_t index, std::initializer_list<IrOp> oprefs)
{
    for (IrOp op : oprefs)
    {
        if (op.kind != IrOpKind::Inst)
            continue;

        IrInst& source = function.instructions[op.index];

        if (source.lastUse == index && !source.reusedReg)
        {
            LUAU_ASSERT(source.regX64.size == SizeX64::xmmword);
            LUAU_ASSERT(source.regX64 != noreg);

            source.reusedReg = true;
            return source.regX64;
        }
    }

    return allocXmmReg();
}

void IrLoweringX64::freeReg(RegisterX64 reg)
{
    if (reg.size == SizeX64::xmmword)
    {
        LUAU_ASSERT(!freeXmmMap[reg.index]);
        freeXmmMap[reg.index] = true;
    }
    else
    {
        LUAU_ASSERT(!freeGprMap[reg.index]);
        freeGprMap[reg.index] = true;
    }
}

void IrLoweringX64::freeLastUseReg(IrInst& target, uint32_t index)
{
    if (target.lastUse == index && !target.reusedReg)
    {
        // Register might have already been freed if it had multiple uses inside a single instruction
        if (target.regX64 == noreg)
            return;

        freeReg(target.regX64);
        target.regX64 = noreg;
    }
}

void IrLoweringX64::freeLastUseRegs(const IrInst& inst, uint32_t index)
{
    auto checkOp = [this, index](IrOp op) {
        if (op.kind == IrOpKind::Inst)
            freeLastUseReg(function.instructions[op.index], index);
    };

    checkOp(inst.a);
    checkOp(inst.b);
    checkOp(inst.c);
    checkOp(inst.d);
    checkOp(inst.e);
}

ConditionX64 IrLoweringX64::getX64Condition(IrCondition cond) const
{
    // TODO: this function will not be required when jumpOnNumberCmp starts accepting an IrCondition
    switch (cond)
    {
    case IrCondition::Equal:
        return ConditionX64::Equal;
    case IrCondition::NotEqual:
        return ConditionX64::NotEqual;
    case IrCondition::Less:
        return ConditionX64::Less;
    case IrCondition::NotLess:
        return ConditionX64::NotLess;
    case IrCondition::LessEqual:
        return ConditionX64::LessEqual;
    case IrCondition::NotLessEqual:
        return ConditionX64::NotLessEqual;
    case IrCondition::Greater:
        return ConditionX64::Greater;
    case IrCondition::NotGreater:
        return ConditionX64::NotGreater;
    case IrCondition::GreaterEqual:
        return ConditionX64::GreaterEqual;
    case IrCondition::NotGreaterEqual:
        return ConditionX64::NotGreaterEqual;
    default:
        LUAU_ASSERT(!"unsupported condition");
        break;
    }

    return ConditionX64::Count;
}

IrLoweringX64::ScopedReg::ScopedReg(IrLoweringX64& owner, SizeX64 size)
    : owner(owner)
{
    if (size == SizeX64::xmmword)
        reg = owner.allocXmmReg();
    else
        reg = owner.allocGprReg(size);
}

IrLoweringX64::ScopedReg::~ScopedReg()
{
    if (reg != noreg)
        owner.freeReg(reg);
}

void IrLoweringX64::ScopedReg::free()
{
    LUAU_ASSERT(reg != noreg);
    owner.freeReg(reg);
    reg = noreg;
}

} // namespace CodeGen
} // namespace Luau
