// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "NativeState.h"

#include "lobject.h"
#include "lstate.h"

namespace Luau
{
namespace CodeGen
{

// Here we define helper functions to wrap interaction with Luau custom execution API so that it works with or without LUA_CUSTOM_EXECUTION

#if LUA_CUSTOM_EXECUTION

inline lua_ExecutionCallbacks* getExecutionCallbacks(lua_State* L)
{
    return &L->global->ecb;
}

inline NativeState* getNativeState(lua_State* L)
{
    lua_ExecutionCallbacks* ecb = getExecutionCallbacks(L);
    return (NativeState*)ecb->context;
}

inline void setNativeState(lua_State* L, NativeState* nativeState)
{
    lua_ExecutionCallbacks* ecb = getExecutionCallbacks(L);
    ecb->context = nativeState;
}

inline NativeState* createNativeState(lua_State* L)
{
    NativeState* state = new NativeState();
    setNativeState(L, state);
    return state;
}

inline void destroyNativeState(lua_State* L)
{
    NativeState* state = getNativeState(L);
    setNativeState(L, nullptr);
    delete state;
}

inline NativeProto* getProtoExecData(Proto* proto)
{
    return (NativeProto*)proto->execdata;
}

inline void setProtoExecData(Proto* proto, NativeProto* nativeProto)
{
    if (nativeProto)
        LUAU_ASSERT(proto->execdata == nullptr);

    proto->execdata = nativeProto;
}

#define offsetofProtoExecData offsetof(Proto, execdata)

#else

inline lua_ExecutionCallbacks* getExecutionCallbacks(lua_State* L)
{
    return nullptr;
}

inline NativeState* getNativeState(lua_State* L)
{
    return nullptr;
}

inline void setNativeState(lua_State* L, NativeState* nativeState) {}

inline NativeState* createNativeState(lua_State* L)
{
    return nullptr;
}

inline void destroyNativeState(lua_State* L) {}

inline NativeProto* getProtoExecData(Proto* proto)
{
    return nullptr;
}

inline void setProtoExecData(Proto* proto, NativeProto* nativeProto) {}

#define offsetofProtoExecData 0

#endif

inline int getOpLength(LuauOpcode op)
{
    switch (op)
    {
    case LOP_GETGLOBAL:
    case LOP_SETGLOBAL:
    case LOP_GETIMPORT:
    case LOP_GETTABLEKS:
    case LOP_SETTABLEKS:
    case LOP_NAMECALL:
    case LOP_JUMPIFEQ:
    case LOP_JUMPIFLE:
    case LOP_JUMPIFLT:
    case LOP_JUMPIFNOTEQ:
    case LOP_JUMPIFNOTLE:
    case LOP_JUMPIFNOTLT:
    case LOP_NEWTABLE:
    case LOP_SETLIST:
    case LOP_FORGLOOP:
    case LOP_LOADKX:
    case LOP_FASTCALL2:
    case LOP_FASTCALL2K:
    case LOP_JUMPXEQKNIL:
    case LOP_JUMPXEQKB:
    case LOP_JUMPXEQKN:
    case LOP_JUMPXEQKS:
        return 2;

    default:
        return 1;
    }
}

} // namespace CodeGen
} // namespace Luau
