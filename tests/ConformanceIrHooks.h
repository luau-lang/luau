// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/IrBuilder.h"

inline uint8_t vectorAccessBytecodeType(const char* member, size_t memberLength)
{
    using namespace Luau::CodeGen;

    if (memberLength == strlen("Magnitude") && strcmp(member, "Magnitude") == 0)
        return LBC_TYPE_NUMBER;

    if (memberLength == strlen("Unit") && strcmp(member, "Unit") == 0)
        return LBC_TYPE_VECTOR;

    return LBC_TYPE_ANY;
}

inline bool vectorAccess(Luau::CodeGen::IrBuilder& build, const char* member, size_t memberLength, int resultReg, int sourceReg, int pcpos)
{
    using namespace Luau::CodeGen;

    if (memberLength == strlen("Magnitude") && strcmp(member, "Magnitude") == 0)
    {
        IrOp x = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(sourceReg), build.constInt(0));
        IrOp y = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(sourceReg), build.constInt(4));
        IrOp z = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(sourceReg), build.constInt(8));

        IrOp x2 = build.inst(IrCmd::MUL_NUM, x, x);
        IrOp y2 = build.inst(IrCmd::MUL_NUM, y, y);
        IrOp z2 = build.inst(IrCmd::MUL_NUM, z, z);

        IrOp sum = build.inst(IrCmd::ADD_NUM, build.inst(IrCmd::ADD_NUM, x2, y2), z2);

        IrOp mag = build.inst(IrCmd::SQRT_NUM, sum);

        build.inst(IrCmd::STORE_DOUBLE, build.vmReg(resultReg), mag);
        build.inst(IrCmd::STORE_TAG, build.vmReg(resultReg), build.constTag(LUA_TNUMBER));

        return true;
    }

    if (memberLength == strlen("Unit") && strcmp(member, "Unit") == 0)
    {
        IrOp x = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(sourceReg), build.constInt(0));
        IrOp y = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(sourceReg), build.constInt(4));
        IrOp z = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(sourceReg), build.constInt(8));

        IrOp x2 = build.inst(IrCmd::MUL_NUM, x, x);
        IrOp y2 = build.inst(IrCmd::MUL_NUM, y, y);
        IrOp z2 = build.inst(IrCmd::MUL_NUM, z, z);

        IrOp sum = build.inst(IrCmd::ADD_NUM, build.inst(IrCmd::ADD_NUM, x2, y2), z2);

        IrOp mag = build.inst(IrCmd::SQRT_NUM, sum);
        IrOp inv = build.inst(IrCmd::DIV_NUM, build.constDouble(1.0), mag);

        IrOp xr = build.inst(IrCmd::MUL_NUM, x, inv);
        IrOp yr = build.inst(IrCmd::MUL_NUM, y, inv);
        IrOp zr = build.inst(IrCmd::MUL_NUM, z, inv);

        build.inst(IrCmd::STORE_VECTOR, build.vmReg(resultReg), xr, yr, zr);
        build.inst(IrCmd::STORE_TAG, build.vmReg(resultReg), build.constTag(LUA_TVECTOR));

        return true;
    }

    return false;
}

inline uint8_t vectorNamecallBytecodeType(const char* member, size_t memberLength)
{
    if (memberLength == strlen("Dot") && strcmp(member, "Dot") == 0)
        return LBC_TYPE_NUMBER;

    if (memberLength == strlen("Cross") && strcmp(member, "Cross") == 0)
        return LBC_TYPE_VECTOR;

    return LBC_TYPE_ANY;
}

inline bool vectorNamecall(
    Luau::CodeGen::IrBuilder& build, const char* member, size_t memberLength, int argResReg, int sourceReg, int params, int results, int pcpos)
{
    using namespace Luau::CodeGen;

    if (memberLength == strlen("Dot") && strcmp(member, "Dot") == 0 && params == 2 && results <= 1)
    {
        build.loadAndCheckTag(build.vmReg(argResReg + 2), LUA_TVECTOR, build.vmExit(pcpos));

        IrOp x1 = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(sourceReg), build.constInt(0));
        IrOp x2 = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(argResReg + 2), build.constInt(0));
        IrOp xx = build.inst(IrCmd::MUL_NUM, x1, x2);

        IrOp y1 = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(sourceReg), build.constInt(4));
        IrOp y2 = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(argResReg + 2), build.constInt(4));
        IrOp yy = build.inst(IrCmd::MUL_NUM, y1, y2);

        IrOp z1 = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(sourceReg), build.constInt(8));
        IrOp z2 = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(argResReg + 2), build.constInt(8));
        IrOp zz = build.inst(IrCmd::MUL_NUM, z1, z2);

        IrOp sum = build.inst(IrCmd::ADD_NUM, build.inst(IrCmd::ADD_NUM, xx, yy), zz);

        build.inst(IrCmd::STORE_DOUBLE, build.vmReg(argResReg), sum);
        build.inst(IrCmd::STORE_TAG, build.vmReg(argResReg), build.constTag(LUA_TNUMBER));

        // If the function is called in multi-return context, stack has to be adjusted
        if (results == LUA_MULTRET)
            build.inst(IrCmd::ADJUST_STACK_TO_REG, build.vmReg(argResReg), build.constInt(1));

        return true;
    }

    if (memberLength == strlen("Cross") && strcmp(member, "Cross") == 0 && params == 2 && results <= 1)
    {
        build.loadAndCheckTag(build.vmReg(argResReg + 2), LUA_TVECTOR, build.vmExit(pcpos));

        IrOp x1 = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(sourceReg), build.constInt(0));
        IrOp x2 = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(argResReg + 2), build.constInt(0));

        IrOp y1 = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(sourceReg), build.constInt(4));
        IrOp y2 = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(argResReg + 2), build.constInt(4));

        IrOp z1 = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(sourceReg), build.constInt(8));
        IrOp z2 = build.inst(IrCmd::LOAD_FLOAT, build.vmReg(argResReg + 2), build.constInt(8));

        IrOp y1z2 = build.inst(IrCmd::MUL_NUM, y1, z2);
        IrOp z1y2 = build.inst(IrCmd::MUL_NUM, z1, y2);
        IrOp xr = build.inst(IrCmd::SUB_NUM, y1z2, z1y2);

        IrOp z1x2 = build.inst(IrCmd::MUL_NUM, z1, x2);
        IrOp x1z2 = build.inst(IrCmd::MUL_NUM, x1, z2);
        IrOp yr = build.inst(IrCmd::SUB_NUM, z1x2, x1z2);

        IrOp x1y2 = build.inst(IrCmd::MUL_NUM, x1, y2);
        IrOp y1x2 = build.inst(IrCmd::MUL_NUM, y1, x2);
        IrOp zr = build.inst(IrCmd::SUB_NUM, x1y2, y1x2);

        build.inst(IrCmd::STORE_VECTOR, build.vmReg(argResReg), xr, yr, zr);
        build.inst(IrCmd::STORE_TAG, build.vmReg(argResReg), build.constTag(LUA_TVECTOR));

        // If the function is called in multi-return context, stack has to be adjusted
        if (results == LUA_MULTRET)
            build.inst(IrCmd::ADJUST_STACK_TO_REG, build.vmReg(argResReg), build.constInt(1));

        return true;
    }

    return false;
}
