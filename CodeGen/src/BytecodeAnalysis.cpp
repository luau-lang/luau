// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BytecodeAnalysis.h"

#include "Luau/BytecodeUtils.h"
#include "Luau/CodeGen.h"
#include "Luau/IrData.h"
#include "Luau/IrUtils.h"

#include "lobject.h"
#include "lstate.h"

#include <algorithm>

LUAU_FASTFLAGVARIABLE(LuauCodegenUserdataOps, false)
LUAU_FASTFLAGVARIABLE(LuauCodegenFastcall3, false)

namespace Luau
{
namespace CodeGen
{

template<typename T>
static T read(uint8_t* data, size_t& offset)
{
    T result;
    memcpy(&result, data + offset, sizeof(T));
    offset += sizeof(T);

    return result;
}

static uint32_t readVarInt(uint8_t* data, size_t& offset)
{
    uint32_t result = 0;
    uint32_t shift = 0;

    uint8_t byte;

    do
    {
        byte = read<uint8_t>(data, offset);
        result |= (byte & 127) << shift;
        shift += 7;
    } while (byte & 128);

    return result;
}

void loadBytecodeTypeInfo(IrFunction& function)
{
    Proto* proto = function.proto;

    if (!proto)
        return;

    BytecodeTypeInfo& typeInfo = function.bcTypeInfo;

    // If there is no typeinfo, we generate default values for arguments and upvalues
    if (!proto->typeinfo)
    {
        typeInfo.argumentTypes.resize(proto->numparams, LBC_TYPE_ANY);
        typeInfo.upvalueTypes.resize(proto->nups, LBC_TYPE_ANY);
        return;
    }

    uint8_t* data = proto->typeinfo;
    size_t offset = 0;

    uint32_t typeSize = readVarInt(data, offset);
    uint32_t upvalCount = readVarInt(data, offset);
    uint32_t localCount = readVarInt(data, offset);

    if (typeSize != 0)
    {
        uint8_t* types = (uint8_t*)data + offset;

        CODEGEN_ASSERT(typeSize == uint32_t(2 + proto->numparams));
        CODEGEN_ASSERT(types[0] == LBC_TYPE_FUNCTION);
        CODEGEN_ASSERT(types[1] == proto->numparams);

        typeInfo.argumentTypes.resize(proto->numparams);

        // Skip two bytes of function type introduction
        memcpy(typeInfo.argumentTypes.data(), types + 2, proto->numparams);
        offset += typeSize;
    }

    if (upvalCount != 0)
    {
        CODEGEN_ASSERT(upvalCount == unsigned(proto->nups));

        typeInfo.upvalueTypes.resize(upvalCount);

        uint8_t* types = (uint8_t*)data + offset;
        memcpy(typeInfo.upvalueTypes.data(), types, upvalCount);
        offset += upvalCount;
    }

    if (localCount != 0)
    {
        typeInfo.regTypes.resize(localCount);

        for (uint32_t i = 0; i < localCount; i++)
        {
            BytecodeRegTypeInfo& info = typeInfo.regTypes[i];

            info.type = read<uint8_t>(data, offset);
            info.reg = read<uint8_t>(data, offset);
            info.startpc = readVarInt(data, offset);
            info.endpc = info.startpc + readVarInt(data, offset);
        }
    }

    CODEGEN_ASSERT(offset == size_t(proto->sizetypeinfo));
}

static void prepareRegTypeInfoLookups(BytecodeTypeInfo& typeInfo)
{
    // Sort by register first, then by end PC
    std::sort(typeInfo.regTypes.begin(), typeInfo.regTypes.end(), [](const BytecodeRegTypeInfo& a, const BytecodeRegTypeInfo& b) {
        if (a.reg != b.reg)
            return a.reg < b.reg;

        return a.endpc < b.endpc;
    });

    // Prepare data for all registers as 'regTypes' might be missing temporaries
    typeInfo.regTypeOffsets.resize(256 + 1);

    for (size_t i = 0; i < typeInfo.regTypes.size(); i++)
    {
        const BytecodeRegTypeInfo& el = typeInfo.regTypes[i];

        // Data is sorted by register order, so when we visit register Rn last time
        // If means that register Rn+1 starts one after the slot where Rn ends
        typeInfo.regTypeOffsets[el.reg + 1] = uint32_t(i + 1);
    }

    // Fill in holes with the offset of the previous register
    for (size_t i = 1; i < typeInfo.regTypeOffsets.size(); i++)
    {
        uint32_t& el = typeInfo.regTypeOffsets[i];

        if (el == 0)
            el = typeInfo.regTypeOffsets[i - 1];
    }
}

static BytecodeRegTypeInfo* findRegType(BytecodeTypeInfo& info, uint8_t reg, int pc)
{
    auto b = info.regTypes.begin() + info.regTypeOffsets[reg];
    auto e = info.regTypes.begin() + info.regTypeOffsets[reg + 1];

    // Doen't have info
    if (b == e)
        return nullptr;

    // No info after the last live range
    if (pc >= (e - 1)->endpc)
        return nullptr;

    for (auto it = b; it != e; ++it)
    {
        CODEGEN_ASSERT(it->reg == reg);

        if (pc >= it->startpc && pc < it->endpc)
            return &*it;
    }

    return nullptr;
}

static void refineRegType(BytecodeTypeInfo& info, uint8_t reg, int pc, uint8_t ty)
{
    if (ty != LBC_TYPE_ANY)
    {
        if (BytecodeRegTypeInfo* regType = findRegType(info, reg, pc))
        {
            // Right now, we only refine register types that were unknown
            if (regType->type == LBC_TYPE_ANY)
                regType->type = ty;
        }
        else if (reg < info.argumentTypes.size())
        {
            if (info.argumentTypes[reg] == LBC_TYPE_ANY)
                info.argumentTypes[reg] = ty;
        }
    }
}

static void refineUpvalueType(BytecodeTypeInfo& info, int up, uint8_t ty)
{
    if (ty != LBC_TYPE_ANY)
    {
        if (size_t(up) < info.upvalueTypes.size())
        {
            if (info.upvalueTypes[up] == LBC_TYPE_ANY)
                info.upvalueTypes[up] = ty;
        }
    }
}

static uint8_t getBytecodeConstantTag(Proto* proto, unsigned ki)
{
    TValue protok = proto->k[ki];

    switch (protok.tt)
    {
    case LUA_TNIL:
        return LBC_TYPE_NIL;
    case LUA_TBOOLEAN:
        return LBC_TYPE_BOOLEAN;
    case LUA_TLIGHTUSERDATA:
        return LBC_TYPE_USERDATA;
    case LUA_TNUMBER:
        return LBC_TYPE_NUMBER;
    case LUA_TVECTOR:
        return LBC_TYPE_VECTOR;
    case LUA_TSTRING:
        return LBC_TYPE_STRING;
    case LUA_TTABLE:
        return LBC_TYPE_TABLE;
    case LUA_TFUNCTION:
        return LBC_TYPE_FUNCTION;
    case LUA_TUSERDATA:
        return LBC_TYPE_USERDATA;
    case LUA_TTHREAD:
        return LBC_TYPE_THREAD;
    case LUA_TBUFFER:
        return LBC_TYPE_BUFFER;
    }

    return LBC_TYPE_ANY;
}

static void applyBuiltinCall(int bfid, BytecodeTypes& types)
{
    switch (bfid)
    {
    case LBF_NONE:
    case LBF_ASSERT:
        types.result = LBC_TYPE_ANY;
        break;
    case LBF_MATH_ABS:
    case LBF_MATH_ACOS:
    case LBF_MATH_ASIN:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        break;
    case LBF_MATH_ATAN2:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_MATH_ATAN:
    case LBF_MATH_CEIL:
    case LBF_MATH_COSH:
    case LBF_MATH_COS:
    case LBF_MATH_DEG:
    case LBF_MATH_EXP:
    case LBF_MATH_FLOOR:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        break;
    case LBF_MATH_FMOD:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_MATH_FREXP:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        break;
    case LBF_MATH_LDEXP:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_MATH_LOG10:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        break;
    case LBF_MATH_LOG:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER; // We can mark optional arguments
        break;
    case LBF_MATH_MAX:
    case LBF_MATH_MIN:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER; // We can mark optional arguments
        break;
    case LBF_MATH_MODF:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        break;
    case LBF_MATH_POW:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_MATH_RAD:
    case LBF_MATH_SINH:
    case LBF_MATH_SIN:
    case LBF_MATH_SQRT:
    case LBF_MATH_TANH:
    case LBF_MATH_TAN:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        break;
    case LBF_BIT32_ARSHIFT:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_BIT32_BAND:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER; // We can mark optional arguments
        break;
    case LBF_BIT32_BNOT:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        break;
    case LBF_BIT32_BOR:
    case LBF_BIT32_BXOR:
    case LBF_BIT32_BTEST:
    case LBF_BIT32_EXTRACT:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER; // We can mark optional arguments
        break;
    case LBF_BIT32_LROTATE:
    case LBF_BIT32_LSHIFT:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_BIT32_REPLACE:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER; // We can mark optional arguments
        break;
    case LBF_BIT32_RROTATE:
    case LBF_BIT32_RSHIFT:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_TYPE:
        types.result = LBC_TYPE_STRING;
        break;
    case LBF_STRING_BYTE:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_STRING;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_STRING_CHAR:
        types.result = LBC_TYPE_STRING;

        // We can mark optional arguments
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER;
        break;
    case LBF_STRING_LEN:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_STRING;
        break;
    case LBF_TYPEOF:
        types.result = LBC_TYPE_STRING;
        break;
    case LBF_STRING_SUB:
        types.result = LBC_TYPE_STRING;
        types.a = LBC_TYPE_STRING;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER;
        break;
    case LBF_MATH_CLAMP:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER;
        break;
    case LBF_MATH_SIGN:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        break;
    case LBF_MATH_ROUND:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        break;
    case LBF_RAWGET:
        types.result = LBC_TYPE_ANY;
        types.a = LBC_TYPE_TABLE;
        break;
    case LBF_RAWEQUAL:
        types.result = LBC_TYPE_BOOLEAN;
        break;
    case LBF_TABLE_UNPACK:
        types.result = LBC_TYPE_ANY;
        types.a = LBC_TYPE_TABLE;
        types.b = LBC_TYPE_NUMBER; // We can mark optional arguments
        break;
    case LBF_VECTOR:
        types.result = LBC_TYPE_VECTOR;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER;
        break;
    case LBF_BIT32_COUNTLZ:
    case LBF_BIT32_COUNTRZ:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        break;
    case LBF_SELECT_VARARG:
        types.result = LBC_TYPE_ANY;
        break;
    case LBF_RAWLEN:
        types.result = LBC_TYPE_NUMBER;
        break;
    case LBF_BIT32_EXTRACTK:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_GETMETATABLE:
        types.result = LBC_TYPE_TABLE;
        break;
    case LBF_TONUMBER:
        types.result = LBC_TYPE_NUMBER;
        break;
    case LBF_TOSTRING:
        types.result = LBC_TYPE_STRING;
        break;
    case LBF_BIT32_BYTESWAP:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_NUMBER;
        break;
    case LBF_BUFFER_READI8:
    case LBF_BUFFER_READU8:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_BUFFER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_BUFFER_WRITEU8:
        types.result = LBC_TYPE_NIL;
        types.a = LBC_TYPE_BUFFER;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER;
        break;
    case LBF_BUFFER_READI16:
    case LBF_BUFFER_READU16:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_BUFFER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_BUFFER_WRITEU16:
        types.result = LBC_TYPE_NIL;
        types.a = LBC_TYPE_BUFFER;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER;
        break;
    case LBF_BUFFER_READI32:
    case LBF_BUFFER_READU32:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_BUFFER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_BUFFER_WRITEU32:
        types.result = LBC_TYPE_NIL;
        types.a = LBC_TYPE_BUFFER;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER;
        break;
    case LBF_BUFFER_READF32:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_BUFFER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_BUFFER_WRITEF32:
        types.result = LBC_TYPE_NIL;
        types.a = LBC_TYPE_BUFFER;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER;
        break;
    case LBF_BUFFER_READF64:
        types.result = LBC_TYPE_NUMBER;
        types.a = LBC_TYPE_BUFFER;
        types.b = LBC_TYPE_NUMBER;
        break;
    case LBF_BUFFER_WRITEF64:
        types.result = LBC_TYPE_NIL;
        types.a = LBC_TYPE_BUFFER;
        types.b = LBC_TYPE_NUMBER;
        types.c = LBC_TYPE_NUMBER;
        break;
    case LBF_TABLE_INSERT:
        types.result = LBC_TYPE_NIL;
        types.a = LBC_TYPE_TABLE;
        break;
    case LBF_RAWSET:
        types.result = LBC_TYPE_ANY;
        types.a = LBC_TYPE_TABLE;
        break;
    case LBF_SETMETATABLE:
        types.result = LBC_TYPE_TABLE;
        types.a = LBC_TYPE_TABLE;
        types.b = LBC_TYPE_TABLE;
        break;
    }
}

static HostMetamethod opcodeToHostMetamethod(LuauOpcode op)
{
    switch (op)
    {
    case LOP_ADD:
        return HostMetamethod::Add;
    case LOP_SUB:
        return HostMetamethod::Sub;
    case LOP_MUL:
        return HostMetamethod::Mul;
    case LOP_DIV:
        return HostMetamethod::Div;
    case LOP_IDIV:
        return HostMetamethod::Idiv;
    case LOP_MOD:
        return HostMetamethod::Mod;
    case LOP_POW:
        return HostMetamethod::Pow;
    case LOP_ADDK:
        return HostMetamethod::Add;
    case LOP_SUBK:
        return HostMetamethod::Sub;
    case LOP_MULK:
        return HostMetamethod::Mul;
    case LOP_DIVK:
        return HostMetamethod::Div;
    case LOP_IDIVK:
        return HostMetamethod::Idiv;
    case LOP_MODK:
        return HostMetamethod::Mod;
    case LOP_POWK:
        return HostMetamethod::Pow;
    case LOP_SUBRK:
        return HostMetamethod::Sub;
    case LOP_DIVRK:
        return HostMetamethod::Div;
    default:
        CODEGEN_ASSERT(!"opcode is not assigned to a host metamethod");
    }

    return HostMetamethod::Add;
}

void buildBytecodeBlocks(IrFunction& function, const std::vector<uint8_t>& jumpTargets)
{
    Proto* proto = function.proto;
    CODEGEN_ASSERT(proto);

    std::vector<BytecodeBlock>& bcBlocks = function.bcBlocks;

    // Using the same jump targets, create VM bytecode basic blocks
    bcBlocks.push_back(BytecodeBlock{0, -1});

    int previ = 0;

    for (int i = 0; i < proto->sizecode;)
    {
        const Instruction* pc = &proto->code[i];
        LuauOpcode op = LuauOpcode(LUAU_INSN_OP(*pc));

        int nexti = i + getOpLength(op);

        // If instruction is a jump target, begin new block starting from it
        if (i != 0 && jumpTargets[i])
        {
            bcBlocks.back().finishpc = previ;
            bcBlocks.push_back(BytecodeBlock{i, -1});
        }

        int target = getJumpTarget(*pc, uint32_t(i));

        // Implicit fallthroughs terminate the block and might start a new one
        if (target >= 0 && !isFastCall(op))
        {
            bcBlocks.back().finishpc = i;

            // Start a new block if there was no explicit jump for the fallthrough
            if (!jumpTargets[nexti])
                bcBlocks.push_back(BytecodeBlock{nexti, -1});
        }
        // Returns just terminate the block
        else if (op == LOP_RETURN)
        {
            bcBlocks.back().finishpc = i;
        }

        previ = i;
        i = nexti;
        CODEGEN_ASSERT(i <= proto->sizecode);
    }
}

void analyzeBytecodeTypes(IrFunction& function, const HostIrHooks& hostHooks)
{
    Proto* proto = function.proto;
    CODEGEN_ASSERT(proto);

    BytecodeTypeInfo& bcTypeInfo = function.bcTypeInfo;

    prepareRegTypeInfoLookups(bcTypeInfo);

    // Setup our current knowledge of type tags based on arguments
    uint8_t regTags[256];
    memset(regTags, LBC_TYPE_ANY, 256);

    function.bcTypes.resize(proto->sizecode);

    // Now that we have VM basic blocks, we can attempt to track register type tags locally
    for (const BytecodeBlock& block : function.bcBlocks)
    {
        CODEGEN_ASSERT(block.startpc != -1);
        CODEGEN_ASSERT(block.finishpc != -1);

        // At the block start, reset or knowledge to the starting state
        // In the future we might be able to propagate some info between the blocks as well
        for (size_t i = 0; i < bcTypeInfo.argumentTypes.size(); i++)
        {
            uint8_t et = bcTypeInfo.argumentTypes[i];

            // TODO: if argument is optional, this might force a VM exit unnecessarily
            regTags[i] = et & ~LBC_TYPE_OPTIONAL_BIT;
        }

        for (int i = proto->numparams; i < proto->maxstacksize; ++i)
            regTags[i] = LBC_TYPE_ANY;

        LuauBytecodeType knownNextCallResult = LBC_TYPE_ANY;

        for (int i = block.startpc; i <= block.finishpc;)
        {
            const Instruction* pc = &proto->code[i];
            LuauOpcode op = LuauOpcode(LUAU_INSN_OP(*pc));

            // Assign known register types from local type information
            // TODO: this is an expensive walk for each instruction
            // TODO: it's best to lookup when register is actually used in the instruction
            for (BytecodeRegTypeInfo& el : bcTypeInfo.regTypes)
            {
                if (el.type != LBC_TYPE_ANY && i >= el.startpc && i < el.endpc)
                    regTags[el.reg] = el.type;
            }

            BytecodeTypes& bcType = function.bcTypes[i];

            switch (op)
            {
            case LOP_NOP:
                break;
            case LOP_LOADNIL:
            {
                int ra = LUAU_INSN_A(*pc);
                regTags[ra] = LBC_TYPE_NIL;
                bcType.result = regTags[ra];
                break;
            }
            case LOP_LOADB:
            {
                int ra = LUAU_INSN_A(*pc);
                regTags[ra] = LBC_TYPE_BOOLEAN;
                bcType.result = regTags[ra];

                refineRegType(bcTypeInfo, ra, i, bcType.result);
                break;
            }
            case LOP_LOADN:
            {
                int ra = LUAU_INSN_A(*pc);
                regTags[ra] = LBC_TYPE_NUMBER;
                bcType.result = regTags[ra];

                refineRegType(bcTypeInfo, ra, i, bcType.result);
                break;
            }
            case LOP_LOADK:
            {
                int ra = LUAU_INSN_A(*pc);
                int kb = LUAU_INSN_D(*pc);
                bcType.a = getBytecodeConstantTag(proto, kb);
                regTags[ra] = bcType.a;
                bcType.result = regTags[ra];

                refineRegType(bcTypeInfo, ra, i, bcType.result);
                break;
            }
            case LOP_LOADKX:
            {
                int ra = LUAU_INSN_A(*pc);
                int kb = int(pc[1]);
                bcType.a = getBytecodeConstantTag(proto, kb);
                regTags[ra] = bcType.a;
                bcType.result = regTags[ra];

                refineRegType(bcTypeInfo, ra, i, bcType.result);
                break;
            }
            case LOP_MOVE:
            {
                int ra = LUAU_INSN_A(*pc);
                int rb = LUAU_INSN_B(*pc);
                bcType.a = regTags[rb];
                regTags[ra] = regTags[rb];
                bcType.result = regTags[ra];

                refineRegType(bcTypeInfo, ra, i, bcType.result);
                break;
            }
            case LOP_GETTABLE:
            {
                int rb = LUAU_INSN_B(*pc);
                int rc = LUAU_INSN_C(*pc);
                bcType.a = regTags[rb];
                bcType.b = regTags[rc];
                break;
            }
            case LOP_SETTABLE:
            {
                int rb = LUAU_INSN_B(*pc);
                int rc = LUAU_INSN_C(*pc);
                bcType.a = regTags[rb];
                bcType.b = regTags[rc];
                break;
            }
            case LOP_GETTABLEKS:
            {
                int ra = LUAU_INSN_A(*pc);
                int rb = LUAU_INSN_B(*pc);
                uint32_t kc = pc[1];

                bcType.a = regTags[rb];
                bcType.b = getBytecodeConstantTag(proto, kc);

                regTags[ra] = LBC_TYPE_ANY;

                if (FFlag::LuauCodegenUserdataOps)
                {
                    TString* str = gco2ts(function.proto->k[kc].value.gc);
                    const char* field = getstr(str);

                    if (bcType.a == LBC_TYPE_VECTOR)
                    {
                        if (str->len == 1)
                        {
                            // Same handling as LOP_GETTABLEKS block in lvmexecute.cpp - case-insensitive comparison with "X" / "Y" / "Z"
                            char ch = field[0] | ' ';

                            if (ch == 'x' || ch == 'y' || ch == 'z')
                                regTags[ra] = LBC_TYPE_NUMBER;
                        }

                        if (regTags[ra] == LBC_TYPE_ANY && hostHooks.vectorAccessBytecodeType)
                            regTags[ra] = hostHooks.vectorAccessBytecodeType(field, str->len);
                    }
                    else if (isCustomUserdataBytecodeType(bcType.a))
                    {
                        if (regTags[ra] == LBC_TYPE_ANY && hostHooks.userdataAccessBytecodeType)
                            regTags[ra] = hostHooks.userdataAccessBytecodeType(bcType.a, field, str->len);
                    }
                }
                else
                {
                    if (bcType.a == LBC_TYPE_VECTOR)
                    {
                        TString* str = gco2ts(function.proto->k[kc].value.gc);
                        const char* field = getstr(str);

                        if (str->len == 1)
                        {
                            // Same handling as LOP_GETTABLEKS block in lvmexecute.cpp - case-insensitive comparison with "X" / "Y" / "Z"
                            char ch = field[0] | ' ';

                            if (ch == 'x' || ch == 'y' || ch == 'z')
                                regTags[ra] = LBC_TYPE_NUMBER;
                        }

                        if (regTags[ra] == LBC_TYPE_ANY && hostHooks.vectorAccessBytecodeType)
                            regTags[ra] = hostHooks.vectorAccessBytecodeType(field, str->len);
                    }
                }

                bcType.result = regTags[ra];
                break;
            }
            case LOP_SETTABLEKS:
            {
                int rb = LUAU_INSN_B(*pc);
                bcType.a = regTags[rb];
                bcType.b = LBC_TYPE_STRING;
                break;
            }
            case LOP_GETTABLEN:
            case LOP_SETTABLEN:
            {
                int rb = LUAU_INSN_B(*pc);
                bcType.a = regTags[rb];
                bcType.b = LBC_TYPE_NUMBER;
                break;
            }
            case LOP_ADD:
            case LOP_SUB:
            {
                int ra = LUAU_INSN_A(*pc);
                int rb = LUAU_INSN_B(*pc);
                int rc = LUAU_INSN_C(*pc);

                bcType.a = regTags[rb];
                bcType.b = regTags[rc];

                regTags[ra] = LBC_TYPE_ANY;

                if (bcType.a == LBC_TYPE_NUMBER && bcType.b == LBC_TYPE_NUMBER)
                    regTags[ra] = LBC_TYPE_NUMBER;
                else if (bcType.a == LBC_TYPE_VECTOR && bcType.b == LBC_TYPE_VECTOR)
                    regTags[ra] = LBC_TYPE_VECTOR;
                else if (FFlag::LuauCodegenUserdataOps && hostHooks.userdataMetamethodBytecodeType &&
                         (isCustomUserdataBytecodeType(bcType.a) || isCustomUserdataBytecodeType(bcType.b)))
                    regTags[ra] = hostHooks.userdataMetamethodBytecodeType(bcType.a, bcType.b, opcodeToHostMetamethod(op));

                bcType.result = regTags[ra];
                break;
            }
            case LOP_MUL:
            case LOP_DIV:
            case LOP_IDIV:
            {
                int ra = LUAU_INSN_A(*pc);
                int rb = LUAU_INSN_B(*pc);
                int rc = LUAU_INSN_C(*pc);

                bcType.a = regTags[rb];
                bcType.b = regTags[rc];

                regTags[ra] = LBC_TYPE_ANY;

                if (bcType.a == LBC_TYPE_NUMBER)
                {
                    if (bcType.b == LBC_TYPE_NUMBER)
                        regTags[ra] = LBC_TYPE_NUMBER;
                    else if (bcType.b == LBC_TYPE_VECTOR)
                        regTags[ra] = LBC_TYPE_VECTOR;
                }
                else if (bcType.a == LBC_TYPE_VECTOR)
                {
                    if (bcType.b == LBC_TYPE_NUMBER || bcType.b == LBC_TYPE_VECTOR)
                        regTags[ra] = LBC_TYPE_VECTOR;
                }
                else if (FFlag::LuauCodegenUserdataOps && hostHooks.userdataMetamethodBytecodeType &&
                         (isCustomUserdataBytecodeType(bcType.a) || isCustomUserdataBytecodeType(bcType.b)))
                {
                    regTags[ra] = hostHooks.userdataMetamethodBytecodeType(bcType.a, bcType.b, opcodeToHostMetamethod(op));
                }

                bcType.result = regTags[ra];
                break;
            }
            case LOP_MOD:
            case LOP_POW:
            {
                int ra = LUAU_INSN_A(*pc);
                int rb = LUAU_INSN_B(*pc);
                int rc = LUAU_INSN_C(*pc);

                bcType.a = regTags[rb];
                bcType.b = regTags[rc];

                regTags[ra] = LBC_TYPE_ANY;

                if (bcType.a == LBC_TYPE_NUMBER && bcType.b == LBC_TYPE_NUMBER)
                    regTags[ra] = LBC_TYPE_NUMBER;
                else if (FFlag::LuauCodegenUserdataOps && hostHooks.userdataMetamethodBytecodeType &&
                         (isCustomUserdataBytecodeType(bcType.a) || isCustomUserdataBytecodeType(bcType.b)))
                    regTags[ra] = hostHooks.userdataMetamethodBytecodeType(bcType.a, bcType.b, opcodeToHostMetamethod(op));

                bcType.result = regTags[ra];
                break;
            }
            case LOP_ADDK:
            case LOP_SUBK:
            {
                int ra = LUAU_INSN_A(*pc);
                int rb = LUAU_INSN_B(*pc);
                int kc = LUAU_INSN_C(*pc);

                bcType.a = regTags[rb];
                bcType.b = getBytecodeConstantTag(proto, kc);

                regTags[ra] = LBC_TYPE_ANY;

                if (bcType.a == LBC_TYPE_NUMBER && bcType.b == LBC_TYPE_NUMBER)
                    regTags[ra] = LBC_TYPE_NUMBER;
                else if (bcType.a == LBC_TYPE_VECTOR && bcType.b == LBC_TYPE_VECTOR)
                    regTags[ra] = LBC_TYPE_VECTOR;
                else if (FFlag::LuauCodegenUserdataOps && hostHooks.userdataMetamethodBytecodeType &&
                         (isCustomUserdataBytecodeType(bcType.a) || isCustomUserdataBytecodeType(bcType.b)))
                    regTags[ra] = hostHooks.userdataMetamethodBytecodeType(bcType.a, bcType.b, opcodeToHostMetamethod(op));

                bcType.result = regTags[ra];
                break;
            }
            case LOP_MULK:
            case LOP_DIVK:
            case LOP_IDIVK:
            {
                int ra = LUAU_INSN_A(*pc);
                int rb = LUAU_INSN_B(*pc);
                int kc = LUAU_INSN_C(*pc);

                bcType.a = regTags[rb];
                bcType.b = getBytecodeConstantTag(proto, kc);

                regTags[ra] = LBC_TYPE_ANY;

                if (bcType.a == LBC_TYPE_NUMBER)
                {
                    if (bcType.b == LBC_TYPE_NUMBER)
                        regTags[ra] = LBC_TYPE_NUMBER;
                    else if (bcType.b == LBC_TYPE_VECTOR)
                        regTags[ra] = LBC_TYPE_VECTOR;
                }
                else if (bcType.a == LBC_TYPE_VECTOR)
                {
                    if (bcType.b == LBC_TYPE_NUMBER || bcType.b == LBC_TYPE_VECTOR)
                        regTags[ra] = LBC_TYPE_VECTOR;
                }
                else if (FFlag::LuauCodegenUserdataOps && hostHooks.userdataMetamethodBytecodeType &&
                         (isCustomUserdataBytecodeType(bcType.a) || isCustomUserdataBytecodeType(bcType.b)))
                {
                    regTags[ra] = hostHooks.userdataMetamethodBytecodeType(bcType.a, bcType.b, opcodeToHostMetamethod(op));
                }

                bcType.result = regTags[ra];
                break;
            }
            case LOP_MODK:
            case LOP_POWK:
            {
                int ra = LUAU_INSN_A(*pc);
                int rb = LUAU_INSN_B(*pc);
                int kc = LUAU_INSN_C(*pc);

                bcType.a = regTags[rb];
                bcType.b = getBytecodeConstantTag(proto, kc);

                regTags[ra] = LBC_TYPE_ANY;

                if (bcType.a == LBC_TYPE_NUMBER && bcType.b == LBC_TYPE_NUMBER)
                    regTags[ra] = LBC_TYPE_NUMBER;
                else if (FFlag::LuauCodegenUserdataOps && hostHooks.userdataMetamethodBytecodeType &&
                         (isCustomUserdataBytecodeType(bcType.a) || isCustomUserdataBytecodeType(bcType.b)))
                    regTags[ra] = hostHooks.userdataMetamethodBytecodeType(bcType.a, bcType.b, opcodeToHostMetamethod(op));

                bcType.result = regTags[ra];
                break;
            }
            case LOP_SUBRK:
            {
                int ra = LUAU_INSN_A(*pc);
                int kb = LUAU_INSN_B(*pc);
                int rc = LUAU_INSN_C(*pc);

                bcType.a = getBytecodeConstantTag(proto, kb);
                bcType.b = regTags[rc];

                regTags[ra] = LBC_TYPE_ANY;

                if (bcType.a == LBC_TYPE_NUMBER && bcType.b == LBC_TYPE_NUMBER)
                    regTags[ra] = LBC_TYPE_NUMBER;
                else if (bcType.a == LBC_TYPE_VECTOR && bcType.b == LBC_TYPE_VECTOR)
                    regTags[ra] = LBC_TYPE_VECTOR;
                else if (FFlag::LuauCodegenUserdataOps && hostHooks.userdataMetamethodBytecodeType &&
                         (isCustomUserdataBytecodeType(bcType.a) || isCustomUserdataBytecodeType(bcType.b)))
                    regTags[ra] = hostHooks.userdataMetamethodBytecodeType(bcType.a, bcType.b, opcodeToHostMetamethod(op));

                bcType.result = regTags[ra];
                break;
            }
            case LOP_DIVRK:
            {
                int ra = LUAU_INSN_A(*pc);
                int kb = LUAU_INSN_B(*pc);
                int rc = LUAU_INSN_C(*pc);

                bcType.a = getBytecodeConstantTag(proto, kb);
                bcType.b = regTags[rc];

                regTags[ra] = LBC_TYPE_ANY;

                if (bcType.a == LBC_TYPE_NUMBER)
                {
                    if (bcType.b == LBC_TYPE_NUMBER)
                        regTags[ra] = LBC_TYPE_NUMBER;
                    else if (bcType.b == LBC_TYPE_VECTOR)
                        regTags[ra] = LBC_TYPE_VECTOR;
                }
                else if (bcType.a == LBC_TYPE_VECTOR)
                {
                    if (bcType.b == LBC_TYPE_NUMBER || bcType.b == LBC_TYPE_VECTOR)
                        regTags[ra] = LBC_TYPE_VECTOR;
                }
                else if (FFlag::LuauCodegenUserdataOps && hostHooks.userdataMetamethodBytecodeType &&
                         (isCustomUserdataBytecodeType(bcType.a) || isCustomUserdataBytecodeType(bcType.b)))
                {
                    regTags[ra] = hostHooks.userdataMetamethodBytecodeType(bcType.a, bcType.b, opcodeToHostMetamethod(op));
                }

                bcType.result = regTags[ra];
                break;
            }
            case LOP_NOT:
            {
                int ra = LUAU_INSN_A(*pc);
                int rb = LUAU_INSN_B(*pc);

                bcType.a = regTags[rb];

                regTags[ra] = LBC_TYPE_BOOLEAN;
                bcType.result = regTags[ra];
                break;
            }
            case LOP_MINUS:
            {
                int ra = LUAU_INSN_A(*pc);
                int rb = LUAU_INSN_B(*pc);

                bcType.a = regTags[rb];

                regTags[ra] = LBC_TYPE_ANY;

                if (bcType.a == LBC_TYPE_NUMBER)
                    regTags[ra] = LBC_TYPE_NUMBER;
                else if (bcType.a == LBC_TYPE_VECTOR)
                    regTags[ra] = LBC_TYPE_VECTOR;
                else if (FFlag::LuauCodegenUserdataOps && hostHooks.userdataMetamethodBytecodeType && isCustomUserdataBytecodeType(bcType.a))
                    regTags[ra] = hostHooks.userdataMetamethodBytecodeType(bcType.a, LBC_TYPE_ANY, HostMetamethod::Minus);

                bcType.result = regTags[ra];
                break;
            }
            case LOP_LENGTH:
            {
                int ra = LUAU_INSN_A(*pc);
                int rb = LUAU_INSN_B(*pc);

                bcType.a = regTags[rb];

                regTags[ra] = LBC_TYPE_NUMBER; // Even if it's a custom __len, it's ok to assume a sane result
                bcType.result = regTags[ra];
                break;
            }
            case LOP_NEWTABLE:
            case LOP_DUPTABLE:
            {
                int ra = LUAU_INSN_A(*pc);
                regTags[ra] = LBC_TYPE_TABLE;
                bcType.result = regTags[ra];
                break;
            }
            case LOP_FASTCALL:
            {
                int bfid = LUAU_INSN_A(*pc);
                int skip = LUAU_INSN_C(*pc);

                Instruction call = pc[skip + 1];
                CODEGEN_ASSERT(LUAU_INSN_OP(call) == LOP_CALL);
                int ra = LUAU_INSN_A(call);

                applyBuiltinCall(bfid, bcType);
                regTags[ra + 1] = bcType.a;
                regTags[ra + 2] = bcType.b;
                regTags[ra + 3] = bcType.c;
                regTags[ra] = bcType.result;

                refineRegType(bcTypeInfo, ra, i, bcType.result);
                break;
            }
            case LOP_FASTCALL1:
            case LOP_FASTCALL2K:
            {
                int bfid = LUAU_INSN_A(*pc);
                int skip = LUAU_INSN_C(*pc);

                Instruction call = pc[skip + 1];
                CODEGEN_ASSERT(LUAU_INSN_OP(call) == LOP_CALL);
                int ra = LUAU_INSN_A(call);

                applyBuiltinCall(bfid, bcType);

                regTags[LUAU_INSN_B(*pc)] = bcType.a;
                regTags[ra] = bcType.result;

                refineRegType(bcTypeInfo, ra, i, bcType.result);
                break;
            }
            case LOP_FASTCALL2:
            {
                int bfid = LUAU_INSN_A(*pc);
                int skip = LUAU_INSN_C(*pc);

                Instruction call = pc[skip + 1];
                CODEGEN_ASSERT(LUAU_INSN_OP(call) == LOP_CALL);
                int ra = LUAU_INSN_A(call);

                applyBuiltinCall(bfid, bcType);

                regTags[LUAU_INSN_B(*pc)] = bcType.a;
                regTags[int(pc[1])] = bcType.b;
                regTags[ra] = bcType.result;

                refineRegType(bcTypeInfo, ra, i, bcType.result);
                break;
            }
            case LOP_FASTCALL3:
            {
                CODEGEN_ASSERT(FFlag::LuauCodegenFastcall3);

                int bfid = LUAU_INSN_A(*pc);
                int skip = LUAU_INSN_C(*pc);
                int aux = pc[1];

                Instruction call = pc[skip + 1];
                CODEGEN_ASSERT(LUAU_INSN_OP(call) == LOP_CALL);
                int ra = LUAU_INSN_A(call);

                applyBuiltinCall(bfid, bcType);

                regTags[LUAU_INSN_B(*pc)] = bcType.a;
                regTags[aux & 0xff] = bcType.b;
                regTags[(aux >> 8) & 0xff] = bcType.c;
                regTags[ra] = bcType.result;

                refineRegType(bcTypeInfo, ra, i, bcType.result);
                break;
            }
            case LOP_FORNPREP:
            {
                int ra = LUAU_INSN_A(*pc);

                regTags[ra] = LBC_TYPE_NUMBER;
                regTags[ra + 1] = LBC_TYPE_NUMBER;
                regTags[ra + 2] = LBC_TYPE_NUMBER;

                refineRegType(bcTypeInfo, ra, i, regTags[ra]);
                refineRegType(bcTypeInfo, ra + 1, i, regTags[ra + 1]);
                refineRegType(bcTypeInfo, ra + 2, i, regTags[ra + 2]);
                break;
            }
            case LOP_FORNLOOP:
            {
                int ra = LUAU_INSN_A(*pc);

                // These types are established by LOP_FORNPREP and we reinforce that here
                regTags[ra] = LBC_TYPE_NUMBER;
                regTags[ra + 1] = LBC_TYPE_NUMBER;
                regTags[ra + 2] = LBC_TYPE_NUMBER;
                break;
            }
            case LOP_CONCAT:
            {
                int ra = LUAU_INSN_A(*pc);
                regTags[ra] = LBC_TYPE_STRING;
                bcType.result = regTags[ra];
                break;
            }
            case LOP_NEWCLOSURE:
            case LOP_DUPCLOSURE:
            {
                int ra = LUAU_INSN_A(*pc);
                regTags[ra] = LBC_TYPE_FUNCTION;
                bcType.result = regTags[ra];
                break;
            }
            case LOP_NAMECALL:
            {
                int ra = LUAU_INSN_A(*pc);
                int rb = LUAU_INSN_B(*pc);
                uint32_t kc = pc[1];

                bcType.a = regTags[rb];
                bcType.b = getBytecodeConstantTag(proto, kc);

                // While namecall might result in a callable table, we assume the function fast path
                regTags[ra] = LBC_TYPE_FUNCTION;

                // Namecall places source register into target + 1
                regTags[ra + 1] = bcType.a;

                bcType.result = LBC_TYPE_FUNCTION;

                if (FFlag::LuauCodegenUserdataOps)
                {
                    TString* str = gco2ts(function.proto->k[kc].value.gc);
                    const char* field = getstr(str);

                    if (bcType.a == LBC_TYPE_VECTOR && hostHooks.vectorNamecallBytecodeType)
                        knownNextCallResult = LuauBytecodeType(hostHooks.vectorNamecallBytecodeType(field, str->len));
                    else if (isCustomUserdataBytecodeType(bcType.a) && hostHooks.userdataNamecallBytecodeType)
                        knownNextCallResult = LuauBytecodeType(hostHooks.userdataNamecallBytecodeType(bcType.a, field, str->len));
                }
                else
                {
                    if (bcType.a == LBC_TYPE_VECTOR && hostHooks.vectorNamecallBytecodeType)
                    {
                        TString* str = gco2ts(function.proto->k[kc].value.gc);
                        const char* field = getstr(str);

                        knownNextCallResult = LuauBytecodeType(hostHooks.vectorNamecallBytecodeType(field, str->len));
                    }
                }
                break;
            }
            case LOP_CALL:
            {
                int ra = LUAU_INSN_A(*pc);

                if (knownNextCallResult != LBC_TYPE_ANY)
                {
                    bcType.result = knownNextCallResult;

                    knownNextCallResult = LBC_TYPE_ANY;

                    regTags[ra] = bcType.result;
                }

                refineRegType(bcTypeInfo, ra, i, bcType.result);
                break;
            }
            case LOP_GETUPVAL:
            {
                int ra = LUAU_INSN_A(*pc);
                int up = LUAU_INSN_B(*pc);

                bcType.a = LBC_TYPE_ANY;

                if (size_t(up) < bcTypeInfo.upvalueTypes.size())
                {
                    uint8_t et = bcTypeInfo.upvalueTypes[up];

                    // TODO: if argument is optional, this might force a VM exit unnecessarily
                    bcType.a = et & ~LBC_TYPE_OPTIONAL_BIT;
                }

                regTags[ra] = bcType.a;
                bcType.result = regTags[ra];
                break;
            }
            case LOP_SETUPVAL:
            {
                int ra = LUAU_INSN_A(*pc);
                int up = LUAU_INSN_B(*pc);

                refineUpvalueType(bcTypeInfo, up, regTags[ra]);
                break;
            }
            case LOP_GETGLOBAL:
            case LOP_SETGLOBAL:
            case LOP_RETURN:
            case LOP_JUMP:
            case LOP_JUMPBACK:
            case LOP_JUMPIF:
            case LOP_JUMPIFNOT:
            case LOP_JUMPIFEQ:
            case LOP_JUMPIFLE:
            case LOP_JUMPIFLT:
            case LOP_JUMPIFNOTEQ:
            case LOP_JUMPIFNOTLE:
            case LOP_JUMPIFNOTLT:
            case LOP_JUMPX:
            case LOP_JUMPXEQKNIL:
            case LOP_JUMPXEQKB:
            case LOP_JUMPXEQKN:
            case LOP_JUMPXEQKS:
            case LOP_SETLIST:
            case LOP_CLOSEUPVALS:
            case LOP_FORGLOOP:
            case LOP_FORGPREP_NEXT:
            case LOP_FORGPREP_INEXT:
            case LOP_AND:
            case LOP_ANDK:
            case LOP_OR:
            case LOP_ORK:
            case LOP_COVERAGE:
            case LOP_GETIMPORT:
            case LOP_CAPTURE:
            case LOP_PREPVARARGS:
            case LOP_GETVARARGS:
            case LOP_FORGPREP:
                break;
            default:
                CODEGEN_ASSERT(!"Unknown instruction");
            }

            i += getOpLength(op);
        }
    }
}

} // namespace CodeGen
} // namespace Luau
