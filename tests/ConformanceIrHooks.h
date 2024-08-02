// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/IrBuilder.h"

static const char* kUserdataRunTypes[] = {"extra", "color", "vec2", "mat3", nullptr};

constexpr uint8_t kUserdataExtra = 0;
constexpr uint8_t kUserdataColor = 1;
constexpr uint8_t kUserdataVec2 = 2;
constexpr uint8_t kUserdataMat3 = 3;

// Userdata tags can be different from userdata bytecode type indices
constexpr uint8_t kTagVec2 = 12;

struct Vec2
{
    float x;
    float y;
};

inline bool compareMemberName(const char* member, size_t memberLength, const char* str)
{
    return memberLength == strlen(str) && strcmp(member, str) == 0;
}

inline uint8_t typeToUserdataIndex(uint8_t type)
{
    // Underflow will push the type into a value that is not comparable to any kUserdata* constants
    return type - LBC_TYPE_TAGGED_USERDATA_BASE;
}

inline uint8_t userdataIndexToType(uint8_t userdataIndex)
{
    return LBC_TYPE_TAGGED_USERDATA_BASE + userdataIndex;
}

inline uint8_t vectorAccessBytecodeType(const char* member, size_t memberLength)
{
    using namespace Luau::CodeGen;

    if (compareMemberName(member, memberLength, "Magnitude"))
        return LBC_TYPE_NUMBER;

    if (compareMemberName(member, memberLength, "Unit"))
        return LBC_TYPE_VECTOR;

    return LBC_TYPE_ANY;
}

inline bool vectorAccess(Luau::CodeGen::IrBuilder& build, const char* member, size_t memberLength, int resultReg, int sourceReg, int pcpos)
{
    using namespace Luau::CodeGen;

    if (compareMemberName(member, memberLength, "Magnitude"))
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

    if (compareMemberName(member, memberLength, "Unit"))
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
    if (compareMemberName(member, memberLength, "Dot"))
        return LBC_TYPE_NUMBER;

    if (compareMemberName(member, memberLength, "Cross"))
        return LBC_TYPE_VECTOR;

    return LBC_TYPE_ANY;
}

inline bool vectorNamecall(
    Luau::CodeGen::IrBuilder& build,
    const char* member,
    size_t memberLength,
    int argResReg,
    int sourceReg,
    int params,
    int results,
    int pcpos
)
{
    using namespace Luau::CodeGen;

    if (compareMemberName(member, memberLength, "Dot") && params == 2 && results <= 1)
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

    if (compareMemberName(member, memberLength, "Cross") && params == 2 && results <= 1)
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

inline uint8_t userdataAccessBytecodeType(uint8_t type, const char* member, size_t memberLength)
{
    switch (typeToUserdataIndex(type))
    {
    case kUserdataColor:
        if (compareMemberName(member, memberLength, "R"))
            return LBC_TYPE_NUMBER;

        if (compareMemberName(member, memberLength, "G"))
            return LBC_TYPE_NUMBER;

        if (compareMemberName(member, memberLength, "B"))
            return LBC_TYPE_NUMBER;
        break;
    case kUserdataVec2:
        if (compareMemberName(member, memberLength, "X"))
            return LBC_TYPE_NUMBER;

        if (compareMemberName(member, memberLength, "Y"))
            return LBC_TYPE_NUMBER;

        if (compareMemberName(member, memberLength, "Magnitude"))
            return LBC_TYPE_NUMBER;

        if (compareMemberName(member, memberLength, "Unit"))
            return userdataIndexToType(kUserdataVec2);
        break;
    case kUserdataMat3:
        if (compareMemberName(member, memberLength, "Row1"))
            return LBC_TYPE_VECTOR;

        if (compareMemberName(member, memberLength, "Row2"))
            return LBC_TYPE_VECTOR;

        if (compareMemberName(member, memberLength, "Row3"))
            return LBC_TYPE_VECTOR;
        break;
    }

    return LBC_TYPE_ANY;
}

inline bool userdataAccess(
    Luau::CodeGen::IrBuilder& build,
    uint8_t type,
    const char* member,
    size_t memberLength,
    int resultReg,
    int sourceReg,
    int pcpos
)
{
    using namespace Luau::CodeGen;

    switch (typeToUserdataIndex(type))
    {
    case kUserdataColor:
        break;
    case kUserdataVec2:
        if (compareMemberName(member, memberLength, "X"))
        {
            IrOp udata = build.inst(IrCmd::LOAD_POINTER, build.vmReg(sourceReg));
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata, build.constInt(kTagVec2), build.vmExit(pcpos));

            IrOp value = build.inst(IrCmd::BUFFER_READF32, udata, build.constInt(offsetof(Vec2, x)), build.constTag(LUA_TUSERDATA));

            build.inst(IrCmd::STORE_DOUBLE, build.vmReg(resultReg), value);
            build.inst(IrCmd::STORE_TAG, build.vmReg(resultReg), build.constTag(LUA_TNUMBER));
            return true;
        }

        if (compareMemberName(member, memberLength, "Y"))
        {
            IrOp udata = build.inst(IrCmd::LOAD_POINTER, build.vmReg(sourceReg));
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata, build.constInt(kTagVec2), build.vmExit(pcpos));

            IrOp value = build.inst(IrCmd::BUFFER_READF32, udata, build.constInt(offsetof(Vec2, y)), build.constTag(LUA_TUSERDATA));

            build.inst(IrCmd::STORE_DOUBLE, build.vmReg(resultReg), value);
            build.inst(IrCmd::STORE_TAG, build.vmReg(resultReg), build.constTag(LUA_TNUMBER));
            return true;
        }

        if (compareMemberName(member, memberLength, "Magnitude"))
        {
            IrOp udata = build.inst(IrCmd::LOAD_POINTER, build.vmReg(sourceReg));
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata, build.constInt(kTagVec2), build.vmExit(pcpos));

            IrOp x = build.inst(IrCmd::BUFFER_READF32, udata, build.constInt(offsetof(Vec2, x)), build.constTag(LUA_TUSERDATA));
            IrOp y = build.inst(IrCmd::BUFFER_READF32, udata, build.constInt(offsetof(Vec2, y)), build.constTag(LUA_TUSERDATA));

            IrOp x2 = build.inst(IrCmd::MUL_NUM, x, x);
            IrOp y2 = build.inst(IrCmd::MUL_NUM, y, y);

            IrOp sum = build.inst(IrCmd::ADD_NUM, x2, y2);

            IrOp mag = build.inst(IrCmd::SQRT_NUM, sum);

            build.inst(IrCmd::STORE_DOUBLE, build.vmReg(resultReg), mag);
            build.inst(IrCmd::STORE_TAG, build.vmReg(resultReg), build.constTag(LUA_TNUMBER));
            return true;
        }

        if (compareMemberName(member, memberLength, "Unit"))
        {
            IrOp udata = build.inst(IrCmd::LOAD_POINTER, build.vmReg(sourceReg));
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata, build.constInt(kTagVec2), build.vmExit(pcpos));

            IrOp x = build.inst(IrCmd::BUFFER_READF32, udata, build.constInt(offsetof(Vec2, x)), build.constTag(LUA_TUSERDATA));
            IrOp y = build.inst(IrCmd::BUFFER_READF32, udata, build.constInt(offsetof(Vec2, y)), build.constTag(LUA_TUSERDATA));

            IrOp x2 = build.inst(IrCmd::MUL_NUM, x, x);
            IrOp y2 = build.inst(IrCmd::MUL_NUM, y, y);

            IrOp sum = build.inst(IrCmd::ADD_NUM, x2, y2);

            IrOp mag = build.inst(IrCmd::SQRT_NUM, sum);
            IrOp inv = build.inst(IrCmd::DIV_NUM, build.constDouble(1.0), mag);

            IrOp xr = build.inst(IrCmd::MUL_NUM, x, inv);
            IrOp yr = build.inst(IrCmd::MUL_NUM, y, inv);

            build.inst(IrCmd::CHECK_GC);
            IrOp udatar = build.inst(IrCmd::NEW_USERDATA, build.constInt(sizeof(Vec2)), build.constInt(kTagVec2));

            build.inst(IrCmd::BUFFER_WRITEF32, udatar, build.constInt(offsetof(Vec2, x)), xr, build.constTag(LUA_TUSERDATA));
            build.inst(IrCmd::BUFFER_WRITEF32, udatar, build.constInt(offsetof(Vec2, y)), yr, build.constTag(LUA_TUSERDATA));

            build.inst(IrCmd::STORE_POINTER, build.vmReg(resultReg), udatar);
            build.inst(IrCmd::STORE_TAG, build.vmReg(resultReg), build.constTag(LUA_TUSERDATA));
            return true;
        }
        break;
    case kUserdataMat3:
        break;
    }

    return false;
}

inline uint8_t userdataMetamethodBytecodeType(uint8_t lhsTy, uint8_t rhsTy, Luau::CodeGen::HostMetamethod method)
{
    switch (method)
    {
    case Luau::CodeGen::HostMetamethod::Add:
    case Luau::CodeGen::HostMetamethod::Sub:
    case Luau::CodeGen::HostMetamethod::Mul:
    case Luau::CodeGen::HostMetamethod::Div:
        if (typeToUserdataIndex(lhsTy) == kUserdataVec2 || typeToUserdataIndex(rhsTy) == kUserdataVec2)
            return userdataIndexToType(kUserdataVec2);
        break;
    case Luau::CodeGen::HostMetamethod::Minus:
        if (typeToUserdataIndex(lhsTy) == kUserdataVec2)
            return userdataIndexToType(kUserdataVec2);
        break;
    default:
        break;
    }

    return LBC_TYPE_ANY;
}

inline bool userdataMetamethod(
    Luau::CodeGen::IrBuilder& build,
    uint8_t lhsTy,
    uint8_t rhsTy,
    int resultReg,
    Luau::CodeGen::IrOp lhs,
    Luau::CodeGen::IrOp rhs,
    Luau::CodeGen::HostMetamethod method,
    int pcpos
)
{
    using namespace Luau::CodeGen;

    switch (method)
    {
    case Luau::CodeGen::HostMetamethod::Add:
        if (typeToUserdataIndex(lhsTy) == kUserdataVec2 && typeToUserdataIndex(rhsTy) == kUserdataVec2)
        {
            build.loadAndCheckTag(lhs, LUA_TUSERDATA, build.vmExit(pcpos));
            build.loadAndCheckTag(rhs, LUA_TUSERDATA, build.vmExit(pcpos));

            IrOp udata1 = build.inst(IrCmd::LOAD_POINTER, lhs);
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata1, build.constInt(kTagVec2), build.vmExit(pcpos));

            IrOp udata2 = build.inst(IrCmd::LOAD_POINTER, rhs);
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata2, build.constInt(kTagVec2), build.vmExit(pcpos));

            IrOp x1 = build.inst(IrCmd::BUFFER_READF32, udata1, build.constInt(offsetof(Vec2, x)), build.constTag(LUA_TUSERDATA));
            IrOp x2 = build.inst(IrCmd::BUFFER_READF32, udata2, build.constInt(offsetof(Vec2, x)), build.constTag(LUA_TUSERDATA));
            IrOp mx = build.inst(IrCmd::ADD_NUM, x1, x2);

            IrOp y1 = build.inst(IrCmd::BUFFER_READF32, udata1, build.constInt(offsetof(Vec2, y)), build.constTag(LUA_TUSERDATA));
            IrOp y2 = build.inst(IrCmd::BUFFER_READF32, udata2, build.constInt(offsetof(Vec2, y)), build.constTag(LUA_TUSERDATA));
            IrOp my = build.inst(IrCmd::ADD_NUM, y1, y2);

            build.inst(IrCmd::CHECK_GC);
            IrOp udatar = build.inst(IrCmd::NEW_USERDATA, build.constInt(sizeof(Vec2)), build.constInt(kTagVec2));

            build.inst(IrCmd::BUFFER_WRITEF32, udatar, build.constInt(offsetof(Vec2, x)), mx, build.constTag(LUA_TUSERDATA));
            build.inst(IrCmd::BUFFER_WRITEF32, udatar, build.constInt(offsetof(Vec2, y)), my, build.constTag(LUA_TUSERDATA));

            build.inst(IrCmd::STORE_POINTER, build.vmReg(resultReg), udatar);
            build.inst(IrCmd::STORE_TAG, build.vmReg(resultReg), build.constTag(LUA_TUSERDATA));

            return true;
        }
        break;
    case Luau::CodeGen::HostMetamethod::Mul:
        if (typeToUserdataIndex(lhsTy) == kUserdataVec2 && typeToUserdataIndex(rhsTy) == kUserdataVec2)
        {
            build.loadAndCheckTag(lhs, LUA_TUSERDATA, build.vmExit(pcpos));
            build.loadAndCheckTag(rhs, LUA_TUSERDATA, build.vmExit(pcpos));

            IrOp udata1 = build.inst(IrCmd::LOAD_POINTER, lhs);
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata1, build.constInt(kTagVec2), build.vmExit(pcpos));

            IrOp udata2 = build.inst(IrCmd::LOAD_POINTER, rhs);
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata2, build.constInt(kTagVec2), build.vmExit(pcpos));

            IrOp x1 = build.inst(IrCmd::BUFFER_READF32, udata1, build.constInt(offsetof(Vec2, x)), build.constTag(LUA_TUSERDATA));
            IrOp x2 = build.inst(IrCmd::BUFFER_READF32, udata2, build.constInt(offsetof(Vec2, x)), build.constTag(LUA_TUSERDATA));
            IrOp mx = build.inst(IrCmd::MUL_NUM, x1, x2);

            IrOp y1 = build.inst(IrCmd::BUFFER_READF32, udata1, build.constInt(offsetof(Vec2, y)), build.constTag(LUA_TUSERDATA));
            IrOp y2 = build.inst(IrCmd::BUFFER_READF32, udata2, build.constInt(offsetof(Vec2, y)), build.constTag(LUA_TUSERDATA));
            IrOp my = build.inst(IrCmd::MUL_NUM, y1, y2);

            build.inst(IrCmd::CHECK_GC);
            IrOp udatar = build.inst(IrCmd::NEW_USERDATA, build.constInt(sizeof(Vec2)), build.constInt(kTagVec2));

            build.inst(IrCmd::BUFFER_WRITEF32, udatar, build.constInt(offsetof(Vec2, x)), mx, build.constTag(LUA_TUSERDATA));
            build.inst(IrCmd::BUFFER_WRITEF32, udatar, build.constInt(offsetof(Vec2, y)), my, build.constTag(LUA_TUSERDATA));

            build.inst(IrCmd::STORE_POINTER, build.vmReg(resultReg), udatar);
            build.inst(IrCmd::STORE_TAG, build.vmReg(resultReg), build.constTag(LUA_TUSERDATA));

            return true;
        }
        break;
    case Luau::CodeGen::HostMetamethod::Minus:
        if (typeToUserdataIndex(lhsTy) == kUserdataVec2)
        {
            build.loadAndCheckTag(lhs, LUA_TUSERDATA, build.vmExit(pcpos));

            IrOp udata1 = build.inst(IrCmd::LOAD_POINTER, lhs);
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata1, build.constInt(kTagVec2), build.vmExit(pcpos));

            IrOp x = build.inst(IrCmd::BUFFER_READF32, udata1, build.constInt(offsetof(Vec2, x)), build.constTag(LUA_TUSERDATA));
            IrOp y = build.inst(IrCmd::BUFFER_READF32, udata1, build.constInt(offsetof(Vec2, y)), build.constTag(LUA_TUSERDATA));
            IrOp mx = build.inst(IrCmd::UNM_NUM, x);
            IrOp my = build.inst(IrCmd::UNM_NUM, y);

            build.inst(IrCmd::CHECK_GC);
            IrOp udatar = build.inst(IrCmd::NEW_USERDATA, build.constInt(sizeof(Vec2)), build.constInt(kTagVec2));

            build.inst(IrCmd::BUFFER_WRITEF32, udatar, build.constInt(offsetof(Vec2, x)), mx, build.constTag(LUA_TUSERDATA));
            build.inst(IrCmd::BUFFER_WRITEF32, udatar, build.constInt(offsetof(Vec2, y)), my, build.constTag(LUA_TUSERDATA));

            build.inst(IrCmd::STORE_POINTER, build.vmReg(resultReg), udatar);
            build.inst(IrCmd::STORE_TAG, build.vmReg(resultReg), build.constTag(LUA_TUSERDATA));

            return true;
        }
        break;
    default:
        break;
    }

    return false;
}

inline uint8_t userdataNamecallBytecodeType(uint8_t type, const char* member, size_t memberLength)
{
    switch (typeToUserdataIndex(type))
    {
    case kUserdataColor:
        break;
    case kUserdataVec2:
        if (compareMemberName(member, memberLength, "Dot"))
            return LBC_TYPE_NUMBER;

        if (compareMemberName(member, memberLength, "Min"))
            return userdataIndexToType(kUserdataVec2);
        break;
    case kUserdataMat3:
        break;
    }

    return LBC_TYPE_ANY;
}

inline bool userdataNamecall(
    Luau::CodeGen::IrBuilder& build,
    uint8_t type,
    const char* member,
    size_t memberLength,
    int argResReg,
    int sourceReg,
    int params,
    int results,
    int pcpos
)
{
    using namespace Luau::CodeGen;

    switch (typeToUserdataIndex(type))
    {
    case kUserdataColor:
        break;
    case kUserdataVec2:
        if (compareMemberName(member, memberLength, "Dot"))
        {
            IrOp udata1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(sourceReg));
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata1, build.constInt(kTagVec2), build.vmExit(pcpos));

            build.loadAndCheckTag(build.vmReg(argResReg + 2), LUA_TUSERDATA, build.vmExit(pcpos));

            IrOp udata2 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(argResReg + 2));
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata2, build.constInt(kTagVec2), build.vmExit(pcpos));

            IrOp x1 = build.inst(IrCmd::BUFFER_READF32, udata1, build.constInt(offsetof(Vec2, x)), build.constTag(LUA_TUSERDATA));
            IrOp x2 = build.inst(IrCmd::BUFFER_READF32, udata2, build.constInt(offsetof(Vec2, x)), build.constTag(LUA_TUSERDATA));
            IrOp xx = build.inst(IrCmd::MUL_NUM, x1, x2);

            IrOp y1 = build.inst(IrCmd::BUFFER_READF32, udata1, build.constInt(offsetof(Vec2, y)), build.constTag(LUA_TUSERDATA));
            IrOp y2 = build.inst(IrCmd::BUFFER_READF32, udata2, build.constInt(offsetof(Vec2, y)), build.constTag(LUA_TUSERDATA));
            IrOp yy = build.inst(IrCmd::MUL_NUM, y1, y2);

            IrOp sum = build.inst(IrCmd::ADD_NUM, xx, yy);

            build.inst(IrCmd::STORE_DOUBLE, build.vmReg(argResReg), sum);
            build.inst(IrCmd::STORE_TAG, build.vmReg(argResReg), build.constTag(LUA_TNUMBER));

            // If the function is called in multi-return context, stack has to be adjusted
            if (results == LUA_MULTRET)
                build.inst(IrCmd::ADJUST_STACK_TO_REG, build.vmReg(argResReg), build.constInt(1));

            return true;
        }

        if (compareMemberName(member, memberLength, "Min"))
        {
            IrOp udata1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(sourceReg));
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata1, build.constInt(kTagVec2), build.vmExit(pcpos));

            build.loadAndCheckTag(build.vmReg(argResReg + 2), LUA_TUSERDATA, build.vmExit(pcpos));

            IrOp udata2 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(argResReg + 2));
            build.inst(IrCmd::CHECK_USERDATA_TAG, udata2, build.constInt(kTagVec2), build.vmExit(pcpos));

            IrOp x1 = build.inst(IrCmd::BUFFER_READF32, udata1, build.constInt(offsetof(Vec2, x)), build.constTag(LUA_TUSERDATA));
            IrOp x2 = build.inst(IrCmd::BUFFER_READF32, udata2, build.constInt(offsetof(Vec2, x)), build.constTag(LUA_TUSERDATA));
            IrOp mx = build.inst(IrCmd::MIN_NUM, x1, x2);

            IrOp y1 = build.inst(IrCmd::BUFFER_READF32, udata1, build.constInt(offsetof(Vec2, y)), build.constTag(LUA_TUSERDATA));
            IrOp y2 = build.inst(IrCmd::BUFFER_READF32, udata2, build.constInt(offsetof(Vec2, y)), build.constTag(LUA_TUSERDATA));
            IrOp my = build.inst(IrCmd::MIN_NUM, y1, y2);

            build.inst(IrCmd::CHECK_GC);
            IrOp udatar = build.inst(IrCmd::NEW_USERDATA, build.constInt(sizeof(Vec2)), build.constInt(kTagVec2));

            build.inst(IrCmd::BUFFER_WRITEF32, udatar, build.constInt(offsetof(Vec2, x)), mx, build.constTag(LUA_TUSERDATA));
            build.inst(IrCmd::BUFFER_WRITEF32, udatar, build.constInt(offsetof(Vec2, y)), my, build.constTag(LUA_TUSERDATA));

            build.inst(IrCmd::STORE_POINTER, build.vmReg(argResReg), udatar);
            build.inst(IrCmd::STORE_TAG, build.vmReg(argResReg), build.constTag(LUA_TUSERDATA));

            // If the function is called in multi-return context, stack has to be adjusted
            if (results == LUA_MULTRET)
                build.inst(IrCmd::ADJUST_STACK_TO_REG, build.vmReg(argResReg), build.constInt(1));

            return true;
        }
        break;
    case kUserdataMat3:
        break;
    }

    return false;
}
