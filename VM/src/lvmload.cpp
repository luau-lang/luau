// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lclass.h"
#include "lvm.h"

#include "lstate.h"
#include "ltable.h"
#include "lfunc.h"
#include "lobject.h"
#include "lstring.h"
#include "lvector.h"

#include "lgc.h"
#include "lmem.h"
#include "lbytecode.h"
#include "lapi.h"

#include <string.h>

LUAU_FASTFLAGVARIABLE(LuauCostModel)
LUAU_FASTFLAGVARIABLE(LuauLoadRemapOptionalUserdata)
LUAU_FASTFLAGVARIABLE(LuauLoadExplicitGcDisable)

template<typename T>
struct TempBuffer
{
    lua_State* L;
    T* data;
    size_t count;

    TempBuffer()
        : L(NULL)
        , data(NULL)
        , count(0)
    {
    }

    TempBuffer(const TempBuffer&) = delete;
    TempBuffer(TempBuffer&&) = delete;

    TempBuffer& operator=(const TempBuffer&) = delete;
    TempBuffer& operator=(TempBuffer&&) = delete;

    ~TempBuffer() noexcept
    {
        if (data)
            luaM_freearray(L, data, count, T, 0);
    }

    void allocate(lua_State* L, size_t count)
    {
        LUAU_ASSERT(this->L == nullptr);
        this->L = L;
        this->data = luaM_newarray(L, count, T, 0);
        this->count = count;
    }

    T& operator[](size_t index)
    {
        LUAU_ASSERT(index < count);
        return data[index];
    }
};

struct ScopedDisableGC
{
public:
    ScopedDisableGC(global_State* global, size_t newThreshold_DEPRECATED) noexcept
        : global{global}
    {
        if (FFlag::LuauLoadExplicitGcDisable)
        {
            originalGcDisabled = global->gcdisabled;
            global->gcdisabled = 1;
        }
        else
        {
            originalThreshold_DEPRECATED = global->GCthreshold;
            global->GCthreshold = newThreshold_DEPRECATED;
        }
    }

    ScopedDisableGC(const ScopedDisableGC&) = delete;
    ScopedDisableGC(ScopedDisableGC&&) = delete;

    ScopedDisableGC& operator=(const ScopedDisableGC&) = delete;
    ScopedDisableGC& operator=(ScopedDisableGC&&) = delete;

    ~ScopedDisableGC() noexcept
    {
        if (FFlag::LuauLoadExplicitGcDisable)
            global->gcdisabled = originalGcDisabled;
        else
            global->GCthreshold = originalThreshold_DEPRECATED;
    }

private:
    global_State* global = nullptr;
    size_t originalThreshold_DEPRECATED = 0;
    uint8_t originalGcDisabled = 0;
};

void luaV_getimport(lua_State* L, LuaTable* env, TValue* k, StkId res, uint32_t id, bool propagatenil)
{
    int count = id >> 30;
    LUAU_ASSERT(count > 0);

    int id0 = int(id >> 20) & 1023;
    int id1 = int(id >> 10) & 1023;
    int id2 = int(id) & 1023;

    // after the first call to luaV_gettable, res may be invalid, and env may (sometimes) be garbage collected
    // we take care to not use env again and to restore res before every consecutive use
    ptrdiff_t resp = savestack(L, res);

    // global lookup for id0
    TValue g;
    sethvalue(L, &g, env);
    luaV_gettable(L, &g, &k[id0], res);

    // table lookup for id1
    if (count < 2)
        return;

    res = restorestack(L, resp);
    if (!propagatenil || !ttisnil(res))
        luaV_gettable(L, res, &k[id1], res);

    // table lookup for id2
    if (count < 3)
        return;

    res = restorestack(L, resp);
    if (!propagatenil || !ttisnil(res))
        luaV_gettable(L, res, &k[id2], res);
}

template<typename T>
static T read(const char* data, size_t size, size_t& offset)
{
    T result;
    memcpy(&result, data + offset, sizeof(T));
    offset += sizeof(T);

    return result;
}

static unsigned int readVarInt(const char* data, size_t size, size_t& offset)
{
    unsigned int result = 0;
    unsigned int shift = 0;

    uint8_t byte;

    do
    {
        byte = read<uint8_t>(data, size, offset);
        result |= (byte & 127) << shift;
        shift += 7;
    } while (byte & 128);

    return result;
}

static uint64_t readVarInt64(const char* data, size_t size, size_t& offset)
{
    uint64_t result = 0;
    unsigned int shift = 0;

    uint8_t byte;

    do
    {
        byte = read<uint8_t>(data, size, offset);
        result |= ((uint64_t)(byte & 127)) << shift;
        shift += 7;
    } while (byte & 128);

    return result;
}

static TString* readString(TempBuffer<TString*>& strings, const char* data, size_t size, size_t& offset)
{
    unsigned int id = readVarInt(data, size, offset);

    return id == 0 ? NULL : strings[id - 1];
}

static void resolveImportSafe(lua_State* L, LuaTable* env, TValue* k, uint32_t id)
{
    struct ResolveImport
    {
        LuaTable* env;
        TValue* k;
        uint32_t id;

        static void run(lua_State* L, void* ud)
        {
            ResolveImport* self = static_cast<ResolveImport*>(ud);

            // note: we call getimport with nil propagation which means that accesses to table chains like A.B.C will resolve in nil
            // this is technically not necessary but it reduces the number of exceptions when loading scripts that rely on getfenv/setfenv for global
            // injection
            // allocate a stack slot so that we can do table lookups
            luaD_checkstack(L, 1);
            setnilvalue(L->top);
            L->top++;

            luaV_getimport(L, self->env, self->k, L->top - 1, self->id, /* propagatenil= */ true);
        }
    };

    ResolveImport ri = {env, k, id};
    if (env->safeenv)
    {
        // luaD_pcall will make sure that if any C/Lua calls during import resolution fail, the thread state is restored back
        int oldTop = lua_gettop(L);
        int status = luaD_pcall(L, &ResolveImport::run, &ri, savestack(L, L->top), 0);
        LUAU_ASSERT(oldTop + 1 == lua_gettop(L)); // if an error occurred, luaD_pcall saves it on stack

        if (status != 0)
        {
            // replace error object with nil
            setnilvalue(L->top - 1);
        }
    }
    else
    {
        setnilvalue(L->top);
        L->top++;
    }
}

static uint8_t remapUserdataType(uint8_t type, uint8_t* userdataRemapping, uint32_t count)
{
    LUAU_ASSERT(FFlag::LuauLoadRemapOptionalUserdata);
    uint8_t tag = type & ~LBC_TYPE_OPTIONAL_BIT;
    uint8_t optional = type & LBC_TYPE_OPTIONAL_BIT;
    uint32_t index = uint32_t(tag - LBC_TYPE_TAGGED_USERDATA_BASE);

    if (index < count)
        return userdataRemapping[index] | optional;

    return type;
}

static void remapUserdataTypes(char* data, size_t size, uint8_t* userdataRemapping, uint32_t count)
{
    size_t offset = 0;

    uint32_t typeSize = readVarInt(data, size, offset);
    uint32_t upvalCount = readVarInt(data, size, offset);
    uint32_t localCount = readVarInt(data, size, offset);

    if (typeSize != 0)
    {
        uint8_t* types = (uint8_t*)data + offset;

        // Skip two bytes of function type introduction
        for (uint32_t i = 2; i < typeSize; i++)
        {
            if (FFlag::LuauLoadRemapOptionalUserdata)
            {
                types[i] = remapUserdataType(types[i], userdataRemapping, count);
            }
            else
            {
                uint32_t index = uint32_t(types[i] - LBC_TYPE_TAGGED_USERDATA_BASE);

                if (index < count)
                    types[i] = userdataRemapping[index];
            }
        }

        offset += typeSize;
    }

    if (upvalCount != 0)
    {
        uint8_t* types = (uint8_t*)data + offset;

        for (uint32_t i = 0; i < upvalCount; i++)
        {
            if (FFlag::LuauLoadRemapOptionalUserdata)
            {
                types[i] = remapUserdataType(types[i], userdataRemapping, count);
            }
            else
            {
                uint32_t index = uint32_t(types[i] - LBC_TYPE_TAGGED_USERDATA_BASE);

                if (index < count)
                    types[i] = userdataRemapping[index];
            }
        }

        offset += upvalCount;
    }

    if (localCount != 0)
    {
        for (uint32_t i = 0; i < localCount; i++)
        {
            if (FFlag::LuauLoadRemapOptionalUserdata)
            {
                data[offset] = char(remapUserdataType(uint8_t(data[offset]), userdataRemapping, count));
            }
            else
            {
                uint32_t index = uint32_t(data[offset] - LBC_TYPE_TAGGED_USERDATA_BASE);

                if (index < count)
                    data[offset] = userdataRemapping[index];
            }

            offset += 2;
            readVarInt(data, size, offset);
            readVarInt(data, size, offset);
        }
    }

    LUAU_ASSERT(offset == size);
}

/*
** Lazy loading (luau_loadlazy)
**
** Only main is built at load; every other function starts as a stub, a Proto with just the header fields and one static NATIVECALL
** instruction as its code. Interpreter, C and native calls all enter a Luau function at code or codeentry, so the first call reaches
** LOP_NATIVECALL, which builds the function (luaV_buildlazyproto) and continues as if it had just been entered. The children of a
** built function are stubs again, so a function that never runs costs a stub, or nothing if its parent never ran.
**
** The chunk keeps the offset of every string and function in the caller-owned bytecode. Each stub holds a reference to it; the
** bytecode is released when the last stub is built or collected.
*/

static const Instruction kLazyEntryInsn = LOP_NATIVECALL;

static Proto* newLazyStub(lua_State* L, LuauLazyChunk* chunk, Proto* parent, uint32_t fid)
{
    LUAU_ASSERT(fid < chunk->protoCount);

    Proto* p = luaF_newproto(L);
    p->source = parent->source;
    p->bytecodeid = int(fid);
    p->funid = L->global->lastprotoid == 0 ? 0 : L->global->lastprotoid++;

    p->userdata = parent->userdata;

    const char* header = chunk->data + chunk->protoOffsets[fid];
    p->maxstacksize = uint8_t(header[0]);
    p->numparams = uint8_t(header[1]);
    p->nups = uint8_t(header[2]);
    p->is_vararg = uint8_t(header[3]);
    if (chunk->version >= 4)
        p->flags = uint8_t(header[4]);

    p->code = const_cast<Instruction*>(&kLazyEntryInsn);
    p->codeentry = &kLazyEntryInsn;
    p->sizecode = 1;

    p->lazychunk = chunk;
    chunk->refs++;

    return p;
}

// String ids: luau_load creates every string up front, luau_loadlazy (chunk set) creates each when a function that uses it is built
struct LoadStrings
{
    lua_State* L;
    TempBuffer<TString*>* strings;
    const LuauLazyChunk* chunk;
};

static TString* readString(LoadStrings& strings, const char* data, size_t size, size_t& offset)
{
    if (!strings.chunk)
        return readString(*strings.strings, data, size, offset);

    unsigned int id = readVarInt(data, size, offset);

    if (id == 0)
        return NULL;

    size_t stringOffset = strings.chunk->stringOffsets[id - 1];
    unsigned int length = readVarInt(data, size, stringOffset);

    return luaS_newlstr(strings.L, data + stringOffset, length);
}

// Function ids: luau_load loads every function before the functions that reference it, luau_loadlazy (chunk set) creates stubs
struct LoadProtos
{
    TempBuffer<Proto*>* protos;
    LuauLazyChunk* chunk;
    Proto* parent;

    // luau_loadlazy: stubs of the parent's closure constants, open addressing on bytecodeid (NULL = empty)
    TempBuffer<Proto*> closures;

    Proto** findClosure(uint32_t fid)
    {
        size_t mask = closures.count - 1;

        for (size_t i = fid & mask;; i = (i + 1) & mask)
            if (!closures[i] || uint32_t(closures[i]->bytecodeid) == fid)
                return &closures[i];
    }

    Proto* closure(lua_State* L, uint32_t fid)
    {
        if (!chunk)
            return (*protos)[fid];

        // a closure constant is one of sizek constants, so the table stays at most half full
        if (!closures.L)
        {
            size_t capacity = 2;
            while (capacity < 2 * size_t(parent->sizek))
                capacity *= 2;

            closures.allocate(L, capacity);

            for (size_t i = 0; i < capacity; ++i)
                closures[i] = NULL;
        }

        Proto** slot = findClosure(fid);
        if (!*slot)
            *slot = newLazyStub(L, chunk, parent, fid);

        return *slot;
    }

    Proto* child(lua_State* L, uint32_t fid)
    {
        if (!chunk)
            return (*protos)[fid];

        if (closures.L)
        {
            if (Proto* p = *findClosure(fid))
                return p;
        }

        return newLazyStub(L, chunk, parent, fid);
    }
};

// Reads the format versions; returns 1 with an error message pushed if it can't be loaded
static int loadHeader(
    lua_State* L,
    const char* chunkname,
    const char* data,
    size_t size,
    int env,
    size_t& offset,
    uint8_t& version,
    uint8_t& typesversion
)
{
    version = read<uint8_t>(data, size, offset);

    // 0 means the rest of the bytecode is the error message
    if (version == 0)
    {
        char chunkbuf[LUA_IDSIZE];
        const char* chunkid = luaO_chunkid(chunkbuf, sizeof(chunkbuf), chunkname, strlen(chunkname));
        lua_pushfstring(L, "%s%.*s", chunkid, int(size - offset), data + offset);
        return 1;
    }

    if ((version < LBC_VERSION_MIN || version > LBC_VERSION_MAX) && version != LBC_VERSION_CLASSES)
    {
        char chunkbuf[LUA_IDSIZE];
        const char* chunkid = luaO_chunkid(chunkbuf, sizeof(chunkbuf), chunkname, strlen(chunkname));
        lua_pushfstring(L, "%s: bytecode version mismatch (expected [%d..%d], got %d)", chunkid, LBC_VERSION_MIN, LBC_VERSION_MAX, version);
        return 1;
    }

    typesversion = 0;

    if (version >= 4)
    {
        typesversion = read<uint8_t>(data, size, offset);

        if (typesversion < LBC_TYPE_VERSION_MIN || typesversion > LBC_TYPE_VERSION_MAX)
        {
            char chunkbuf[LUA_IDSIZE];
            const char* chunkid = luaO_chunkid(chunkbuf, sizeof(chunkbuf), chunkname, strlen(chunkname));
            lua_pushfstring(
                L, "%s: bytecode type version mismatch (expected [%d..%d], got %d)", chunkid, LBC_TYPE_VERSION_MIN, LBC_TYPE_VERSION_MAX, typesversion
            );
            return 1;
        }
    }

    return 0;
}

static void readUserdataRemapping(
    lua_State* L,
    uint8_t typesversion,
    LoadStrings& strings,
    uint8_t* userdataRemapping,
    const char* data,
    size_t size,
    size_t& offset
)
{
    // for unknown userdata types, the entry will remap to common 'userdata' type
    if (typesversion == 3)
    {
        memset(userdataRemapping, LBC_TYPE_USERDATA, userdataTypeLimit);

        uint8_t index = read<uint8_t>(data, size, offset);

        while (index != 0)
        {
            TString* name = readString(strings, data, size, offset);

            if (uint32_t(index - 1) < userdataTypeLimit)
            {
                if (auto cb = L->global->ecb.gettypemapping)
                    userdataRemapping[index - 1] = cb(L, getstr(name), name->len);
            }

            index = read<uint8_t>(data, size, offset);
        }
    }
}

// Reads one function into p, starting from its maxstacksize byte
static void loadProto(
    lua_State* L,
    Proto* p,
    uint8_t version,
    uint8_t typesversion,
    uint8_t* userdataRemapping,
    LoadStrings& strings,
    LoadProtos& protos,
    LuaTable* envt,
    LuaTable* importenv,
    const char* data,
    size_t size,
    size_t& offset
)
{
    p->maxstacksize = read<uint8_t>(data, size, offset);
    p->numparams = read<uint8_t>(data, size, offset);
    p->nups = read<uint8_t>(data, size, offset);
    p->is_vararg = read<uint8_t>(data, size, offset);

    if (version >= 4)
    {
        p->flags = read<uint8_t>(data, size, offset);

        if (typesversion == 1)
        {
            uint32_t typesize = readVarInt(data, size, offset);

            if (typesize)
            {
                uint8_t* types = (uint8_t*)data + offset;

                LUAU_ASSERT(typesize == unsigned(2 + p->numparams));
                LUAU_ASSERT(types[0] == LBC_TYPE_FUNCTION);
                LUAU_ASSERT(types[1] == p->numparams);

                // transform v1 into v2 format
                int headersize = typesize > 127 ? 4 : 3;

                p->typeinfo = luaM_newarray(L, headersize + typesize, uint8_t, p->memcat);
                p->sizetypeinfo = headersize + typesize;

                if (headersize == 4)
                {
                    p->typeinfo[0] = (typesize & 127) | (1 << 7);
                    p->typeinfo[1] = typesize >> 7;
                    p->typeinfo[2] = 0;
                    p->typeinfo[3] = 0;
                }
                else
                {
                    p->typeinfo[0] = uint8_t(typesize);
                    p->typeinfo[1] = 0;
                    p->typeinfo[2] = 0;
                }

                memcpy(p->typeinfo + headersize, types, typesize);
            }

            offset += typesize;
        }
        else if (typesversion == 2 || typesversion == 3)
        {
            uint32_t typesize = readVarInt(data, size, offset);

            if (typesize)
            {
                uint8_t* types = (uint8_t*)data + offset;

                p->typeinfo = luaM_newarray(L, typesize, uint8_t, p->memcat);
                p->sizetypeinfo = typesize;
                memcpy(p->typeinfo, types, typesize);
                offset += typesize;

                if (typesversion == 3)
                {
                    remapUserdataTypes((char*)(uint8_t*)p->typeinfo, p->sizetypeinfo, userdataRemapping, userdataTypeLimit);
                }
            }
        }
    }

    const int sizecode = readVarInt(data, size, offset);
    p->code = luaM_newarray(L, sizecode, Instruction, p->memcat);
    p->sizecode = sizecode;

    for (int j = 0; j < p->sizecode; ++j)
        p->code[j] = read<uint32_t>(data, size, offset);

    p->codeentry = p->code;

    const int sizek = readVarInt(data, size, offset);
    p->k = luaM_newarray(L, sizek, TValue, p->memcat);
    p->sizek = sizek;

    // Initialize the constants to nil to ensure they have a valid state
    // in the event that some operation in the following loop fails with
    // an exception.
    for (int j = 0; j < p->sizek; ++j)
    {
        setnilvalue(&p->k[j]);
    }

    for (int j = 0; j < p->sizek; ++j)
    {
        switch (read<uint8_t>(data, size, offset))
        {
        case LBC_CONSTANT_NIL:
            // All constants have already been pre-initialized to nil
            break;

        case LBC_CONSTANT_BOOLEAN:
        {
            uint8_t v = read<uint8_t>(data, size, offset);
            setbvalue(&p->k[j], v);
            break;
        }

        case LBC_CONSTANT_NUMBER:
        {
            double v = read<double>(data, size, offset);
            setnvalue(&p->k[j], v);
            break;
        }

        case LBC_CONSTANT_VECTOR:
        {
            float x = read<float>(data, size, offset);
            float y = read<float>(data, size, offset);
            float z = read<float>(data, size, offset);
            float w = read<float>(data, size, offset);
            (void)w;
            setvvalue(L, &p->k[j], x, y, z, w);
            break;
        }

        case LBC_CONSTANT_VECTORD:
        {
            double x = read<double>(data, size, offset);
            double y = read<double>(data, size, offset);
            double z = read<double>(data, size, offset);
            double w = read<double>(data, size, offset);
            (void)w;
            setvvalue(L, &p->k[j], x, y, z, w);
            break;
        }

        case LBC_CONSTANT_STRING:
        {
            TString* v = readString(strings, data, size, offset);
            setsvalue(L, &p->k[j], v);
            break;
        }

        case LBC_CONSTANT_IMPORT:
        {
            uint32_t iid = read<uint32_t>(data, size, offset);
            resolveImportSafe(L, importenv, p->k, iid);
            setobj(L, &p->k[j], L->top - 1);
            L->top--;
            break;
        }

        case LBC_CONSTANT_TABLE:
        {
            int keys = readVarInt(data, size, offset);
            LuaTable* h = luaH_new(L, 0, keys);
            for (int i = 0; i < keys; ++i)
            {
                int key = readVarInt(data, size, offset);
                TValue* val = luaH_set(L, h, &p->k[key]);
                setnvalue(val, 0.0);
            }
            sethvalue(L, &p->k[j], h);
            break;
        }

        case LBC_CONSTANT_TABLE_WITH_CONSTANTS:
        {
            uint32_t keys = readVarInt(data, size, offset);
            LuaTable* h = luaH_new(L, 0, keys);

            TempBuffer<int32_t> nilKeys;
            nilKeys.allocate(L, keys);
            size_t nilKeysSize = 0;

            for (uint32_t i = 0; i < keys; ++i)
            {
                int32_t key = readVarInt(data, size, offset);
                TValue* val = luaH_set(L, h, &p->k[key]);
                int32_t constantIdx = read<int32_t>(data, size, offset);
                if (constantIdx >= 0)
                {
                    TValue* constant = &p->k[constantIdx];
                    if (ttisnil(constant))
                    {
                        nilKeys[nilKeysSize++] = key;
                    }
                    else
                    {
                        setobj2t(L, val, constant);
                        luaC_barriert(L, h, constant);
                        continue;
                    }
                }
                setnvalue(val, 0.0);
            }

            for (size_t idx = 0; idx < nilKeysSize; idx++)
            {
                int32_t key = nilKeys[idx];
                TValue* val = luaH_set(L, h, &p->k[key]);
                setnilvalue(val);
            }

            sethvalue(L, &p->k[j], h);
            break;
        }

        case LBC_CONSTANT_CLOSURE:
        {
            uint32_t fid = readVarInt(data, size, offset);
            Proto* cp = protos.closure(L, fid);
            Closure* cl = luaF_newLclosure(L, cp->nups, envt, cp);
            cl->preload = (cl->nupvalues > 0);
            setclvalue(L, &p->k[j], cl);
            break;
        }

        case LBC_CONSTANT_CLASS_SHAPE:
        {
            uint32_t cnid = readVarInt(data, size, offset);
            TValue* classname = &p->k[cnid];
            LUAU_ASSERT(ttisstring(classname));
            uint8_t numPrivateFields = read<uint8_t>(data, size, offset);
            uint8_t numPrivateMethods = read<uint8_t>(data, size, offset);
            uint32_t numProperties = readVarInt(data, size, offset);
            uint32_t numMethods = readVarInt(data, size, offset);
            uint32_t numMembers = numMethods + numProperties;
            uint32_t membersWithoutPrivateMethods = numMembers - static_cast<uint32_t>(numPrivateMethods);
            TString** offsetToMember = luaM_newarray(L, membersWithoutPrivateMethods, TString*, L->activememcat);
            memset(offsetToMember, 0, membersWithoutPrivateMethods * sizeof(TString*));
            LuaTable* membersToOffset = luaH_new(L, 0, membersWithoutPrivateMethods - static_cast<uint32_t>(numPrivateFields));

            uint32_t idx = 0;
            // Set public field names
            for (; idx < numProperties - static_cast<uint32_t>(numPrivateFields); idx++)
            {
                uint32_t mid = readVarInt(data, size, offset);
                TValue* memberName = &p->k[mid];
                LUAU_ASSERT(ttisstring(memberName));
                offsetToMember[idx] = tsvalue(memberName);
                TValue* val = luaH_setstr(L, membersToOffset, tsvalue(memberName));
                setnvalue(val, idx);
            }

            // Set public method names
            for (idx = numProperties; idx < membersWithoutPrivateMethods; idx++)
            {
                uint32_t mid = readVarInt(data, size, offset);
                TValue* memberName = &p->k[mid];
                LUAU_ASSERT(ttisstring(memberName));
                offsetToMember[idx] = tsvalue(memberName);
                TValue* val = luaH_setstr(L, membersToOffset, tsvalue(memberName));
                setnvalue(val, idx);
            }

            membersToOffset->readonly = true;

            LuauClass* lco = luaR_newclass(
                L, tsvalue(classname), membersToOffset, offsetToMember, numProperties, numMethods, numPrivateFields, numPrivateMethods, envt
            );
            setclassvalue(L, &p->k[j], lco);
            break;
        }

        default:
            LUAU_ASSERT(!"Unexpected constant kind");
        }
    }

    for (Instruction* instruction = p->code; instruction < p->code + p->sizecode;)
    {
        int targetOp = -1;

        switch (LUAU_INSN_OP(*instruction))
        {
        case LOP_GETTABLEKS:
            targetOp = LOP_GETUDATAKS;
            break;

        case LOP_SETTABLEKS:
            targetOp = LOP_SETUDATAKS;
            break;

        case LOP_NAMECALL:
            targetOp = LOP_NAMECALLUDATA;
            break;
        }

        if (targetOp != -1)
        {
            LUAU_ASSERT(instruction[1] < uint32_t(sizek));

            // We take over the upper 16 bits of AUX - so no constants with big indices.
            if (instruction[1] < 0x10000)
            {
                TValue* k = &p->k[instruction[1]];
                TString* s = tsvalue(k);

                luaS_updateatom(L, s);

                if (s->atom >= 0)
                    *instruction = (*instruction & 0xffffff00) | targetOp;
            }
        }

        instruction += Luau::getOpLength(LuauOpcode(LUAU_INSN_OP(*instruction)));
    }

    const int sizep = readVarInt(data, size, offset);
    p->p = luaM_newarray(L, sizep, Proto*, p->memcat);
    p->sizep = sizep;

    // luau_loadlazy creates children and strings while filling the arrays below: keep a valid state in case of an exception
    for (int j = 0; j < p->sizep; ++j)
        p->p[j] = NULL;

    for (int j = 0; j < p->sizep; ++j)
    {
        uint32_t fid = readVarInt(data, size, offset);
        p->p[j] = protos.child(L, fid);
    }

    p->linedefined = readVarInt(data, size, offset);
    p->debugname = readString(strings, data, size, offset);

    uint8_t lineinfo = read<uint8_t>(data, size, offset);

    if (lineinfo)
    {
        p->linegaplog2 = read<uint8_t>(data, size, offset);

        int intervals = ((p->sizecode - 1) >> p->linegaplog2) + 1;
        int absoffset = (p->sizecode + 3) & ~3;

        const int sizelineinfo = absoffset + intervals * sizeof(int);
        p->lineinfo = luaM_newarray(L, sizelineinfo, uint8_t, p->memcat);
        p->sizelineinfo = sizelineinfo;

        p->abslineinfo = (int*)(p->lineinfo + absoffset);

        uint8_t lastoffset = 0;
        for (int j = 0; j < p->sizecode; ++j)
        {
            lastoffset += read<uint8_t>(data, size, offset);
            p->lineinfo[j] = lastoffset;
        }

        int lastline = 0;
        for (int j = 0; j < intervals; ++j)
        {
            lastline += read<int32_t>(data, size, offset);
            p->abslineinfo[j] = lastline;
        }
    }

    uint8_t debuginfo = read<uint8_t>(data, size, offset);

    if (debuginfo)
    {
        const int sizelocvars = readVarInt(data, size, offset);
        p->locvars = luaM_newarray(L, sizelocvars, LocVar, p->memcat);
        p->sizelocvars = sizelocvars;

        for (int j = 0; j < p->sizelocvars; ++j)
            p->locvars[j].varname = NULL;

        for (int j = 0; j < p->sizelocvars; ++j)
        {
            p->locvars[j].varname = readString(strings, data, size, offset);
            p->locvars[j].startpc = readVarInt(data, size, offset);
            p->locvars[j].endpc = readVarInt(data, size, offset);
            p->locvars[j].reg = read<uint8_t>(data, size, offset);
        }

        const int sizeupvalues = readVarInt(data, size, offset);
        LUAU_ASSERT(sizeupvalues == p->nups);

        p->upvalues = luaM_newarray(L, sizeupvalues, TString*, p->memcat);
        p->sizeupvalues = sizeupvalues;

        for (int j = 0; j < p->sizeupvalues; ++j)
            p->upvalues[j] = NULL;

        for (int j = 0; j < p->sizeupvalues; ++j)
        {
            p->upvalues[j] = readString(strings, data, size, offset);
        }
    }

    if (version >= 11)
    {
        p->feedbackvecsize = readVarInt(data, size, offset);

        if (p->feedbackvecsize > 0)
        {
            p->feedbackvec = luaM_newarray(L, p->feedbackvecsize, FeedbackVectorSlot, p->memcat);
        }
        for (uint32_t j = 0; j < p->feedbackvecsize; j++)
        {
            uint8_t slottype = read<uint8_t>(data, size, offset);

            FeedbackVectorSlot& slot = p->feedbackvec[j];
            slot.kind = static_cast<FeedbackVectorSlotKind>(slottype);

            if (slottype == LFT_CALLTARGET)
            {
                slot.call_target.pc = readVarInt(data, size, offset);
                slot.call_target.proto = 0;
                slot.call_target.hits = 0;
            }
            else if (slottype == LFT_CONSTRUCT)
            {
                slot.construct.shape = readVarInt(data, size, offset);
                slot.construct.classid = 0;
                slot.construct.match = LCM_MISMATCH;
            }
            else
            {
                LUAU_ASSERT(!"unknown feedback slot type");
            }
        }
    }

    if (version >= 12)
    {
        if ((p->flags & LPF_INLINABLE) != 0)
            p->cost = readVarInt64(data, size, offset);
    }
}

static int loadsafe(
    lua_State* L,
    TempBuffer<TString*>& strings,
    TempBuffer<Proto*>& protos,
    const char* chunkname,
    const char* data,
    size_t size,
    int env
)
{
    size_t offset = 0;

    uint8_t version = 0;
    uint8_t typesversion = 0;

    if (int err = loadHeader(L, chunkname, data, size, env, offset, version, typesversion))
        return err;

    // env is 0 for current environment and a stack index otherwise
    LuaTable* envt = (env == 0) ? L->gt : hvalue(luaA_toobject(L, env));

    TString* source = luaS_new(L, chunkname);

    // string table
    unsigned int stringCount = readVarInt(data, size, offset);
    strings.allocate(L, stringCount);

    for (unsigned int i = 0; i < stringCount; ++i)
    {
        unsigned int length = readVarInt(data, size, offset);

        strings[i] = luaS_newlstr(L, data + offset, length);
        offset += length;
    }

    // userdata type remapping table
    uint8_t userdataRemapping[userdataTypeLimit];
    LoadStrings loadStrings = {L, &strings, NULL};
    readUserdataRemapping(L, typesversion, loadStrings, userdataRemapping, data, size, offset);

    // proto table
    unsigned int protoCount = readVarInt(data, size, offset);
    protos.allocate(L, protoCount);

    LoadProtos loadProtos = {&protos, NULL, NULL};

    for (unsigned int i = 0; i < protoCount; ++i)
    {
        uint32_t protoSize = 0;
        if (version >= 12)
            protoSize = readVarInt(data, size, offset);
        size_t protoStartOffset = offset;
        Proto* p = luaF_newproto(L);
        p->source = source;
        p->bytecodeid = int(i);
        p->funid = L->global->lastprotoid == 0 ? 0 : L->global->lastprotoid++;

        loadProto(L, p, version, typesversion, userdataRemapping, loadStrings, loadProtos, envt, L->gt, data, size, offset);

        if (version >= 12)
        {
            // Potentially skipping unknown data at the end of Proto.
            offset = protoStartOffset + protoSize;
        }

        protos[i] = p;
    }

    // "main" proto is pushed to Lua stack
    uint32_t mainid = readVarInt(data, size, offset);
    Proto* main = protos[mainid];

    luaC_threadbarrier(L);

    Closure* cl = luaF_newLclosure(L, 0, envt, main);
    setclvalue(L, L->top, cl);
    incr_top(L);

    return 0;
}

int luau_load(lua_State* L, const char* chunkname, const char* data, size_t size, int env)
{
    // we will allocate a fair amount of memory so check GC before we do
    luaC_checkGC(L);

    // pause GC for the duration of deserialization - some objects we're creating aren't rooted
    const ScopedDisableGC pauseGC{L->global, SIZE_MAX};

    struct LoadContext
    {
        TempBuffer<TString*> strings;
        TempBuffer<Proto*> protos;
        const char* chunkname;
        const char* data;
        size_t size;
        int env;

        int result;

        static void run(lua_State* L, void* ud)
        {
            LoadContext* ctx = (LoadContext*)ud;

            ctx->result = loadsafe(L, ctx->strings, ctx->protos, ctx->chunkname, ctx->data, ctx->size, ctx->env);
        }
    } ctx = {
        {},
        {},
        chunkname,
        data,
        size,
        env,
    };

    int status = luaD_rawrunprotected(L, &LoadContext::run, &ctx);

    // load can either succeed or get an OOM error, any other errors should be handled internally
    LUAU_ASSERT(status == LUA_OK || status == LUA_ERRMEM);

    if (status == LUA_ERRMEM)
    {
        lua_pushstring(L, LUA_MEMERRMSG); // out-of-memory error message doesn't require an allocation
        return 1;
    }

    return ctx.result;
}

void luaV_releaselazychunk(lua_State* L, LuauLazyChunk* chunk)
{
    LUAU_ASSERT(chunk->refs > 0);

    if (--chunk->refs != 0)
        return;

    if (chunk->release)
        chunk->release(chunk->owner);

    luaM_freearray(L, chunk->stringOffsets, chunk->stringCount, uint32_t, chunk->memcat);
    luaM_freearray(L, chunk->protoOffsets, chunk->protoCount, uint32_t, chunk->memcat);
    luaM_freearray(L, chunk, 1, LuauLazyChunk, chunk->memcat);
}

// Builds function p->bytecodeid into p; its children become stubs
static void buildLazyProto(lua_State* L, LuauLazyChunk* chunk, Proto* p, LuaTable* envt, LuaTable* importenv)
{
    size_t offset = chunk->protoOffsets[p->bytecodeid];

    LoadStrings strings = {L, NULL, chunk};
    LoadProtos protos = {NULL, chunk, p};

    loadProto(
        L, p, chunk->version, chunk->typesversion, chunk->userdataRemapping, strings, protos, envt, importenv, chunk->data, chunk->size, offset
    );
}

// Returns 0 with main pushed, 1 with an error pushed, or 2 (nothing pushed) when the chunk must be loaded with luau_load
static int loadlazysafe(lua_State* L, LuauLazyChunk* chunk, const char* chunkname, int env)
{
    const char* data = chunk->data;
    size_t size = chunk->size;
    size_t offset = 0;

    if (int err = loadHeader(L, chunkname, data, size, env, offset, chunk->version, chunk->typesversion))
        return err;

    // functions are found through the size prefix that version 12 added
    if (chunk->version < 12)
        return 2;

    // env is 0 for current environment and a stack index otherwise
    LuaTable* envt = (env == 0) ? L->gt : hvalue(luaA_toobject(L, env));

    TString* source = luaS_new(L, chunkname);

    // string table: just the offsets, a string is created when a function that uses it is built
    unsigned int stringCount = readVarInt(data, size, offset);
    chunk->stringOffsets = luaM_newarray(L, stringCount, uint32_t, chunk->memcat);
    chunk->stringCount = stringCount;

    for (unsigned int i = 0; i < stringCount; ++i)
    {
        chunk->stringOffsets[i] = uint32_t(offset);

        unsigned int length = readVarInt(data, size, offset);
        offset += length;
    }

    LoadStrings strings = {L, NULL, chunk};
    readUserdataRemapping(L, chunk->typesversion, strings, chunk->userdataRemapping, data, size, offset);

    unsigned int protoCount = readVarInt(data, size, offset);
    chunk->protoOffsets = luaM_newarray(L, protoCount, uint32_t, chunk->memcat);
    chunk->protoCount = protoCount;

    for (unsigned int i = 0; i < protoCount; ++i)
    {
        uint32_t protoSize = readVarInt(data, size, offset);

        chunk->protoOffsets[i] = uint32_t(offset);

        // native code generation needs every function of the chunk built
        uint8_t flags = uint8_t(data[offset + 4]);
        if (flags & (LPF_NATIVE_MODULE | LPF_NATIVE_FUNCTION))
            return 2;

        offset += protoSize;
    }

    uint32_t mainid = readVarInt(data, size, offset);

    Proto* main = luaF_newproto(L);
    main->source = source;
    main->bytecodeid = int(mainid);
    main->funid = L->global->lastprotoid == 0 ? 0 : L->global->lastprotoid++;

    chunk->envt = envt;
    chunk->importenv = L->gt;

    buildLazyProto(L, chunk, main, chunk->envt, chunk->importenv);

    luaC_threadbarrier(L);

    Closure* cl = luaF_newLclosure(L, 0, envt, main);
    setclvalue(L, L->top, cl);
    incr_top(L);

    return 0;
}

struct LazyLoadContext
{
    LuauLazyChunk* chunk;
    const char* chunkname;
    const char* data;
    size_t size;
    int env;
    void* owner;
    void (*release)(void* owner);

    int result;
};

static void runLazyLoad(lua_State* L, void* ud)
{
    LazyLoadContext* ctx = static_cast<LazyLoadContext*>(ud);

    LuauLazyChunk* chunk = luaM_newarray(L, 1, LuauLazyChunk, L->activememcat);
    memset(chunk, 0, sizeof(LuauLazyChunk));
    chunk->data = ctx->data;
    chunk->size = ctx->size;
    chunk->owner = ctx->owner;
    chunk->release = ctx->release;
    chunk->refs = 1;
    chunk->memcat = L->activememcat;
    ctx->chunk = chunk;

    ctx->result = loadlazysafe(L, chunk, ctx->chunkname, ctx->env);
}

int luau_loadlazy(lua_State* L, const char* chunkname, const char* data, size_t size, int env, void* owner, void (*release)(void* owner))
{
    // we will allocate a fair amount of memory so check GC before we do
    luaC_checkGC(L);

    // pause GC for the duration of deserialization - some objects we're creating aren't rooted
    const ScopedDisableGC pauseGC{L->global, SIZE_MAX};

    LazyLoadContext ctx = {
        NULL,
        chunkname,
        data,
        size,
        env,
        owner,
        release,
    };

    int status = luaD_rawrunprotected(L, &runLazyLoad, &ctx);

    // load can either succeed or get an OOM error, any other errors should be handled internally
    LUAU_ASSERT(status == LUA_OK || status == LUA_ERRMEM);

    if (status == LUA_OK && ctx.result == 2)
        ctx.result = luau_load(L, chunkname, data, size, env);

    // drop the load's reference: the stubs keep the chunk and the bytecode alive
    if (ctx.chunk)
        luaV_releaselazychunk(L, ctx.chunk);
    else if (release)
        release(owner);

    if (status == LUA_ERRMEM)
    {
        lua_pushstring(L, LUA_MEMERRMSG); // out-of-memory error message doesn't require an allocation
        return 1;
    }

    return ctx.result;
}

// Moves what loadProto read into `from` to the stub `to`; `from` is left without allocations, so collecting it frees nothing
static void moveProtoBody(Proto* to, Proto* from)
{
    to->code = from->code;
    to->codeentry = from->codeentry;
    to->sizecode = from->sizecode;
    to->k = from->k;
    to->sizek = from->sizek;
    to->p = from->p;
    to->sizep = from->sizep;
    to->lineinfo = from->lineinfo;
    to->abslineinfo = from->abslineinfo;
    to->sizelineinfo = from->sizelineinfo;
    to->linegaplog2 = from->linegaplog2;
    to->linedefined = from->linedefined;
    to->debugname = from->debugname;
    to->locvars = from->locvars;
    to->sizelocvars = from->sizelocvars;
    to->upvalues = from->upvalues;
    to->sizeupvalues = from->sizeupvalues;
    to->typeinfo = from->typeinfo;
    to->sizetypeinfo = from->sizetypeinfo;
    to->feedbackvec = from->feedbackvec;
    to->feedbackvecsize = from->feedbackvecsize;
    to->cost = from->cost;

    from->code = NULL;
    from->sizecode = 0;
    from->k = NULL;
    from->sizek = 0;
    from->p = NULL;
    from->sizep = 0;
    from->lineinfo = NULL;
    from->sizelineinfo = 0;
    from->locvars = NULL;
    from->sizelocvars = 0;
    from->upvalues = NULL;
    from->sizeupvalues = 0;
    from->typeinfo = NULL;
    from->sizetypeinfo = 0;
    from->feedbackvec = NULL;
    from->feedbackvecsize = 0;
}

struct LazyBuildContext
{
    LuauLazyChunk* chunk;
    Proto* p;
    Proto* built;
};

static void runLazyBuild(lua_State* L, void* ud)
{
    LazyBuildContext* ctx = static_cast<LazyBuildContext*>(ud);

    Proto* built = luaF_newproto(L);
    built->source = ctx->p->source;
    built->bytecodeid = ctx->p->bytecodeid;
    built->userdata = ctx->p->userdata;
    ctx->built = built;

    buildLazyProto(L, ctx->chunk, built, ctx->chunk->envt, ctx->chunk->importenv);
}

void luaV_buildlazyproto(lua_State* L, Proto* p)
{
    LuauLazyChunk* chunk = p->lazychunk;
    LUAU_ASSERT(chunk);

    // the function is built into a temporary proto, so p stays a valid stub if the build runs out of memory or calls p again (import
    // resolution can run Luau code)
    LazyBuildContext ctx = {chunk, p, NULL};

    // like luau_load, everything the build allocates belongs to the memory category of the chunk's load
    const uint8_t activememcat = L->activememcat;
    L->activememcat = p->memcat;

    // a call from import resolution can build p and drop p's reference while this build still reads the chunk; if p was the last
    // stub that would free the chunk under this build, so the build holds a reference of its own
    chunk->refs++;

    int status = 0;
    {
        // pause GC for the duration of deserialization - some objects we're creating aren't rooted
        const ScopedDisableGC pauseGC{L->global, SIZE_MAX};

        status = luaD_rawrunprotected(L, &runLazyBuild, &ctx);
    }

    L->activememcat = activememcat;

    // a build can only fail with an OOM error, any other errors are handled internally
    LUAU_ASSERT(status == LUA_OK || status == LUA_ERRMEM);

    if (status != LUA_OK)
    {
        luaV_releaselazychunk(L, chunk);
        luaD_throw(L, status);
    }

    // a call from import resolution has built p already; this build is garbage
    if (p->lazychunk)
    {
        moveProtoBody(p, ctx.built);
        p->lazychunk = nullptr;

        // p may already be black in this GC cycle, and everything it references now is new
        luaC_barrierfast(L, p);

        // p's reference
        luaV_releaselazychunk(L, chunk);
    }

    // the build's reference
    luaV_releaselazychunk(L, chunk);
}
