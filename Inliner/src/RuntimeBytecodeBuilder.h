// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Bytecode.h"
#include "Luau/BytecodeGraph.h"

#include "lgc.h"
#include "lmem.h"
#include "lobject.h"

#include <cmath>

namespace Luau
{
namespace JitInliner
{

struct CodeData
{
    std::vector<Instruction> code;
    int linegaplog2 = 0;
    uint8_t* lineinfo = nullptr;
    int* abslineinfo = nullptr;
    uint32_t sizelineinfo = 0;
    std::vector<uint32_t> fbSlotPCs;
};

struct RuntimeBytecodeBuilder : public BytecodeBuilder
{
    lua_State* L;
    std::vector<TValue*>& runtimeConstants;
    std::vector<Proto*>& protos;

    explicit RuntimeBytecodeBuilder(lua_State* L, std::vector<TValue*>& constants, std::vector<Proto*>& protos, BytecodeEncoder* encoder = nullptr)
        : BytecodeBuilder(encoder)
        , L(L)
        , runtimeConstants(constants)
        , protos(protos)
    {
    }

    void validateConst(int32_t v) const override
    {
        LUAU_ASSERT(unsigned(v) < runtimeConstants.size());
    }

    int constTypeToTT(Constant::Type constType) const
    {
        switch (constType)
        {
        case Constant::Type_Nil:
            return LUA_TNIL;
        case Constant::Type_Boolean:
            return LUA_TBOOLEAN;
        case Constant::Type_Number:
            return LUA_TNUMBER;
        case Constant::Type_Integer:
            return LUA_TINTEGER;
        case Constant::Type_Vector:
            return LUA_TVECTOR;
        case Constant::Type_String:
            return LUA_TSTRING;
        case Constant::Type_Table:
            return LUA_TTABLE;
        case Constant::Type_Closure:
            return LUA_TFUNCTION;
        case Constant::Type_ClassShape:
            return LUA_TCLASS;
        default:
            return -1;
        }
    }

    void validateConst(int32_t v, Constant::Type constType) const override
    {
        int tt = constTypeToTT(constType);
        LUAU_ASSERT(unsigned(v) < runtimeConstants.size() && (tt < 0 || runtimeConstants[v]->tt == tt));
    }

    uint8_t validateProto(int32_t pid) const override
    {
        LUAU_ASSERT(unsigned(pid) < protos.size());
        return protos[pid]->nups;
    }

    uint8_t validateClosure(int32_t cid) const override
    {
        Closure* ccl = clvalue(runtimeConstants[cid]);
        return ccl->nupvalues;
    }

    bool printableStringConstant(const char* str, size_t len) const
    {
        for (size_t i = 0; i < len; ++i)
        {
            if (unsigned(str[i]) < ' ')
                return false;
        }

        return true;
    }

    void dumpConstant(std::string& result, int k, bool detailed) const override
    {
        LUAU_ASSERT(unsigned(k) < runtimeConstants.size());
        TValue* c = runtimeConstants[k];

        switch (c->tt)
        {
        case LUA_TNIL:
            formatAppend(result, "nil");
            break;
        case LUA_TBOOLEAN:
            formatAppend(result, "%s", bvalue(c) ? "true" : "false");
            break;
        case LUA_TNUMBER:
            formatAppend(result, "%.17g", nvalue(c));
            break;
        case LUA_TINTEGER:
            formatAppend(result, "%lld", (long long)(int64_t)lvalue(c));
            break;
        case LUA_TVECTOR:
        {
            float* vec = vvalue(c);
            formatAppend(result, "%.9g, %.9g, %.9g", vec[0], vec[1], vec[2]);
            break;
        }
        case LUA_TSTRING:
        {
            TString* str = tsvalue(c);

            if (printableStringConstant(str->data, str->len))
            {
                if (str->len < 32)
                    formatAppend(result, "'%.*s'", str->len, str->data);
                else
                    formatAppend(result, "'%.*s'...", 32, str->data);
            }
            else
            {
                formatAppend(result, "'");

                for (size_t i = 0; i < str->len && i < 32; ++i)
                {
                    if (unsigned(str->data[i]) < ' ')
                        formatAppend(result, "\\x%02X", uint8_t(str->data[i]));
                    else
                        formatAppend(result, "%c", str->data[i]);
                }

                if (str->len >= 32)
                    formatAppend(result, "'...");
                else
                    formatAppend(result, "'");
            }
            break;
        }
        case LUA_TTABLE:
            formatAppend(result, "{...}");
            break;
        case LUA_TFUNCTION:
        {
            Closure* ccl = clvalue(c);

            const char* debugname = nullptr;
            if (ccl->isC != 0)
                debugname = ccl->c.debugname;
            else
            {
                TString* str = ccl->l.p->debugname;
                if (str != nullptr)
                    debugname = str->data;
            }
            formatAppend(result, "'%s'", debugname != nullptr ? debugname : "<unknown>");
            break;
        }
        case LUA_TCLASS:
        {
            formatAppend(result, "class {...}");
            break;
        }
        default:
            formatAppend(result, "K%d", k);
        }
    }

    CodeData finishAndDumpCode(uint8_t maxstacksize, uint8_t numupvalues)
    {
        LUAU_ASSERT(currentFunction != ~0u);

        Function& func = functions[currentFunction];

        func.maxstacksize = maxstacksize;
        func.numupvalues = numupvalues;

#ifdef LUAU_ASSERTENABLED
        validate();
#endif

        // this call is indirect to make sure we only gain link time dependency on dumpCurrentFunction when needed
        if (dumpFunctionPtr)
            func.dump = (this->*dumpFunctionPtr)(func.dumpinstoffs);

        CodeData result;
        result.code = insns;

        if (encoder)
            encoder->encode(result.code.data(), result.code.size());

        // Pack line info.
        int span = calcLinesSpan();
        result.linegaplog2 = std::log2(span);
        int intervals = ((insns.size() - 1) >> result.linegaplog2) + 1;
        int absoffset = (insns.size() + 3) & ~3;

        const int sizelineinfo = absoffset + intervals * sizeof(int);
        result.lineinfo = luaM_newarray(L, sizelineinfo, uint8_t, L->activememcat);
        result.sizelineinfo = sizelineinfo;
        result.abslineinfo = (int*)(result.lineinfo + absoffset);

        fillBaselineInfo(span, result.abslineinfo, intervals);

        for (size_t i = 0; i < lines.size(); ++i)
            result.lineinfo[i] = lines[i] - result.abslineinfo[i >> result.linegaplog2];

        clearState();

        return result;
    }
};

} // namespace JitInliner
} // namespace Luau
