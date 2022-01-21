// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lmem.h"

#include "lstate.h"
#include "ldo.h"
#include "ldebug.h"

#include <string.h>

LUAU_FASTFLAG(LuauGcPagedSweep)

#ifndef __has_feature
#define __has_feature(x) 0
#endif

#if __has_feature(address_sanitizer) || defined(LUAU_ENABLE_ASAN)
#include <sanitizer/asan_interface.h>
#define ASAN_POISON_MEMORY_REGION(addr, size) __asan_poison_memory_region((addr), (size))
#define ASAN_UNPOISON_MEMORY_REGION(addr, size) __asan_unpoison_memory_region((addr), (size))
#else
#define ASAN_POISON_MEMORY_REGION(addr, size) (void)0
#define ASAN_UNPOISON_MEMORY_REGION(addr, size) (void)0
#endif

/*
 * The sizes of Luau objects aren't crucial for code correctness, but they are crucial for memory efficiency
 * To prevent some of them accidentally growing and us losing memory without realizing it, we're going to lock
 * the sizes of all critical structures down.
 */
#if defined(__APPLE__) && !defined(__MACH__)
#define ABISWITCH(x64, ms32, gcc32) (sizeof(void*) == 8 ? x64 : gcc32)
#else
// Android somehow uses a similar ABI to MSVC, *not* to iOS...
#define ABISWITCH(x64, ms32, gcc32) (sizeof(void*) == 8 ? x64 : ms32)
#endif

#if LUA_VECTOR_SIZE == 4
static_assert(sizeof(TValue) == ABISWITCH(24, 24, 24), "size mismatch for value");
static_assert(sizeof(LuaNode) == ABISWITCH(48, 48, 48), "size mismatch for table entry");
#else
static_assert(sizeof(TValue) == ABISWITCH(16, 16, 16), "size mismatch for value");
static_assert(sizeof(LuaNode) == ABISWITCH(32, 32, 32), "size mismatch for table entry");
#endif

static_assert(offsetof(TString, data) == ABISWITCH(24, 20, 20), "size mismatch for string header");
// TODO (FFlagLuauGcPagedSweep): this will become ABISWITCH(16, 16, 16)
static_assert(offsetof(Udata, data) == ABISWITCH(24, 16, 16), "size mismatch for userdata header");
// TODO (FFlagLuauGcPagedSweep): this will become ABISWITCH(48, 32, 32)
static_assert(sizeof(Table) == ABISWITCH(56, 36, 36), "size mismatch for table header");

// TODO (FFlagLuauGcPagedSweep): new code with old 'next' pointer requires that GCObject start at the same point as TString/UpVal
static_assert(offsetof(GCObject, uv) == 0, "UpVal data must be located at the start of the GCObject");
static_assert(offsetof(GCObject, ts) == 0, "TString data must be located at the start of the GCObject");

const size_t kSizeClasses = LUA_SIZECLASSES;
const size_t kMaxSmallSize = 512;
const size_t kPageSize = 16 * 1024 - 24; // slightly under 16KB since that results in less fragmentation due to heap metadata
const size_t kBlockHeader = sizeof(double) > sizeof(void*) ? sizeof(double) : sizeof(void*); // suitable for aligning double & void* on all platforms
// TODO (FFlagLuauGcPagedSweep): when 'next' is removed, 'kBlockHeader' can be used unconditionally
const size_t kGCOHeader = sizeof(GCheader) > kBlockHeader ? sizeof(GCheader) : kBlockHeader;

struct SizeClassConfig
{
    int sizeOfClass[kSizeClasses];
    int8_t classForSize[kMaxSmallSize + 1];
    int classCount = 0;

    SizeClassConfig()
    {
        memset(sizeOfClass, 0, sizeof(sizeOfClass));
        memset(classForSize, -1, sizeof(classForSize));

        // we use a progressive size class scheme:
        // - all size classes are aligned by 8b to satisfy pointer alignment requirements
        // - we first allocate sizes classes in multiples of 8
        // - after the first cutoff we allocate size classes in multiples of 16
        // - after the second cutoff we allocate size classes in multiples of 32
        // this balances internal fragmentation vs external fragmentation
        for (int size = 8; size < 64; size += 8)
            sizeOfClass[classCount++] = size;

        for (int size = 64; size < 256; size += 16)
            sizeOfClass[classCount++] = size;

        for (int size = 256; size <= 512; size += 32)
            sizeOfClass[classCount++] = size;

        LUAU_ASSERT(size_t(classCount) <= kSizeClasses);

        // fill the lookup table for all classes
        for (int klass = 0; klass < classCount; ++klass)
            classForSize[sizeOfClass[klass]] = int8_t(klass);

        // fill the gaps in lookup table
        for (int size = kMaxSmallSize - 1; size >= 0; --size)
            if (classForSize[size] < 0)
                classForSize[size] = classForSize[size + 1];
    }
};

const SizeClassConfig kSizeClassConfig;

// size class for a block of size sz
#define sizeclass(sz) (size_t((sz)-1) < kMaxSmallSize ? kSizeClassConfig.classForSize[sz] : -1)

// metadata for a block is stored in the first pointer of the block
#define metadata(block) (*(void**)(block))
#define freegcolink(block) (*(void**)((char*)block + kGCOHeader))

/*
** About the realloc function:
** void * frealloc (void *ud, void *ptr, size_t osize, size_t nsize);
** (`osize' is the old size, `nsize' is the new size)
**
** Lua ensures that (ptr == NULL) iff (osize == 0).
**
** * frealloc(ud, NULL, 0, x) creates a new block of size `x'
**
** * frealloc(ud, p, x, 0) frees the block `p'
** (in this specific case, frealloc must return NULL).
** particularly, frealloc(ud, NULL, 0, 0) does nothing
** (which is equivalent to free(NULL) in ANSI C)
**
** frealloc returns NULL if it cannot create or reallocate the area
** (any reallocation to an equal or smaller size cannot fail!)
*/

struct lua_Page
{
    // list of pages with free blocks
    lua_Page* prev;
    lua_Page* next;

    // list of all gco pages
    lua_Page* gcolistprev;
    lua_Page* gcolistnext;

    int busyBlocks;
    int blockSize;

    void* freeList;
    int freeNext;

    int pageSize;

    union
    {
        char data[1];
        double align1;
        void* align2;
    };
};

l_noret luaM_toobig(lua_State* L)
{
    luaG_runerror(L, "memory allocation error: block too big");
}

static lua_Page* luaM_newpage(lua_State* L, uint8_t sizeClass)
{
    LUAU_ASSERT(!FFlag::LuauGcPagedSweep);

    global_State* g = L->global;
    lua_Page* page = (lua_Page*)(*g->frealloc)(L, g->ud, NULL, 0, kPageSize);
    if (!page)
        luaD_throw(L, LUA_ERRMEM);

    int blockSize = kSizeClassConfig.sizeOfClass[sizeClass] + kBlockHeader;
    int blockCount = (kPageSize - offsetof(lua_Page, data)) / blockSize;

    ASAN_POISON_MEMORY_REGION(page->data, blockSize * blockCount);

    // setup page header
    page->prev = NULL;
    page->next = NULL;

    page->gcolistprev = NULL;
    page->gcolistnext = NULL;

    page->busyBlocks = 0;
    page->blockSize = blockSize;

    // note: we start with the last block in the page and move downward
    // either order would work, but that way we don't need to store the block count in the page
    // additionally, GC stores objects in singly linked lists, and this way the GC lists end up in increasing pointer order
    page->freeList = NULL;
    page->freeNext = (blockCount - 1) * blockSize;

    // prepend a page to page freelist (which is empty because we only ever allocate a new page when it is!)
    LUAU_ASSERT(!g->freepages[sizeClass]);
    g->freepages[sizeClass] = page;

    return page;
}

static lua_Page* newpage(lua_State* L, lua_Page** gcopageset, int pageSize, int blockSize, int blockCount)
{
    LUAU_ASSERT(FFlag::LuauGcPagedSweep);

    global_State* g = L->global;

    LUAU_ASSERT(pageSize - int(offsetof(lua_Page, data)) >= blockSize * blockCount);

    lua_Page* page = (lua_Page*)(*g->frealloc)(L, g->ud, NULL, 0, pageSize);
    if (!page)
        luaD_throw(L, LUA_ERRMEM);

    ASAN_POISON_MEMORY_REGION(page->data, blockSize * blockCount);

    // setup page header
    page->prev = NULL;
    page->next = NULL;

    page->gcolistprev = NULL;
    page->gcolistnext = NULL;

    page->busyBlocks = 0;
    page->blockSize = blockSize;

    // note: we start with the last block in the page and move downward
    // either order would work, but that way we don't need to store the block count in the page
    // additionally, GC stores objects in singly linked lists, and this way the GC lists end up in increasing pointer order
    page->freeList = NULL;
    page->freeNext = (blockCount - 1) * blockSize;

    page->pageSize = pageSize;

    if (gcopageset)
    {
        page->gcolistnext = *gcopageset;
        if (page->gcolistnext)
            page->gcolistnext->gcolistprev = page;
        *gcopageset = page;
    }

    return page;
}

static lua_Page* newclasspage(lua_State* L, lua_Page** freepageset, lua_Page** gcopageset, uint8_t sizeClass, bool storeMetadata)
{
    LUAU_ASSERT(FFlag::LuauGcPagedSweep);

    int blockSize = kSizeClassConfig.sizeOfClass[sizeClass] + (storeMetadata ? kBlockHeader : 0);
    int blockCount = (kPageSize - offsetof(lua_Page, data)) / blockSize;

    lua_Page* page = newpage(L, gcopageset, kPageSize, blockSize, blockCount);

    // prepend a page to page freelist (which is empty because we only ever allocate a new page when it is!)
    LUAU_ASSERT(!freepageset[sizeClass]);
    freepageset[sizeClass] = page;

    return page;
}

static void luaM_freepage(lua_State* L, lua_Page* page, uint8_t sizeClass)
{
    LUAU_ASSERT(!FFlag::LuauGcPagedSweep);

    global_State* g = L->global;

    // remove page from freelist
    if (page->next)
        page->next->prev = page->prev;

    if (page->prev)
        page->prev->next = page->next;
    else if (g->freepages[sizeClass] == page)
        g->freepages[sizeClass] = page->next;

    // so long
    (*g->frealloc)(L, g->ud, page, kPageSize, 0);
}

static void freepage(lua_State* L, lua_Page** gcopageset, lua_Page* page)
{
    LUAU_ASSERT(FFlag::LuauGcPagedSweep);

    global_State* g = L->global;

    if (gcopageset)
    {
        // remove page from alllist
        if (page->gcolistnext)
            page->gcolistnext->gcolistprev = page->gcolistprev;

        if (page->gcolistprev)
            page->gcolistprev->gcolistnext = page->gcolistnext;
        else if (*gcopageset == page)
            *gcopageset = page->gcolistnext;
    }

    // so long
    (*g->frealloc)(L, g->ud, page, page->pageSize, 0);
}

static void freeclasspage(lua_State* L, lua_Page** freepageset, lua_Page** gcopageset, lua_Page* page, uint8_t sizeClass)
{
    LUAU_ASSERT(FFlag::LuauGcPagedSweep);

    // remove page from freelist
    if (page->next)
        page->next->prev = page->prev;

    if (page->prev)
        page->prev->next = page->next;
    else if (freepageset[sizeClass] == page)
        freepageset[sizeClass] = page->next;

    freepage(L, gcopageset, page);
}

static void* luaM_newblock(lua_State* L, int sizeClass)
{
    global_State* g = L->global;
    lua_Page* page = g->freepages[sizeClass];

    // slow path: no page in the freelist, allocate a new one
    if (!page)
    {
        if (FFlag::LuauGcPagedSweep)
            page = newclasspage(L, g->freepages, NULL, sizeClass, true);
        else
            page = luaM_newpage(L, sizeClass);
    }

    LUAU_ASSERT(!page->prev);
    LUAU_ASSERT(page->freeList || page->freeNext >= 0);
    LUAU_ASSERT(size_t(page->blockSize) == kSizeClassConfig.sizeOfClass[sizeClass] + kBlockHeader);

    void* block;

    if (page->freeNext >= 0)
    {
        block = &page->data + page->freeNext;
        ASAN_UNPOISON_MEMORY_REGION(block, page->blockSize);

        page->freeNext -= page->blockSize;
        page->busyBlocks++;
    }
    else
    {
        block = page->freeList;
        ASAN_UNPOISON_MEMORY_REGION(block, page->blockSize);

        page->freeList = metadata(block);
        page->busyBlocks++;
    }

    // the first word in a block point back to the page
    metadata(block) = page;

    // if we allocate the last block out of a page, we need to remove it from free list
    if (!page->freeList && page->freeNext < 0)
    {
        g->freepages[sizeClass] = page->next;
        if (page->next)
            page->next->prev = NULL;
        page->next = NULL;
    }

    // the user data is right after the metadata
    return (char*)block + kBlockHeader;
}

static void* luaM_newgcoblock(lua_State* L, int sizeClass)
{
    LUAU_ASSERT(FFlag::LuauGcPagedSweep);

    global_State* g = L->global;
    lua_Page* page = g->freegcopages[sizeClass];

    // slow path: no page in the freelist, allocate a new one
    if (!page)
        page = newclasspage(L, g->freegcopages, &g->allgcopages, sizeClass, false);

    LUAU_ASSERT(!page->prev);
    LUAU_ASSERT(page->freeList || page->freeNext >= 0);
    LUAU_ASSERT(page->blockSize == kSizeClassConfig.sizeOfClass[sizeClass]);

    void* block;

    if (page->freeNext >= 0)
    {
        block = &page->data + page->freeNext;
        ASAN_UNPOISON_MEMORY_REGION(block, page->blockSize);

        page->freeNext -= page->blockSize;
        page->busyBlocks++;
    }
    else
    {
        // when separate block metadata is not used, free list link is stored inside the block data itself
        block = (char*)page->freeList - kGCOHeader;

        ASAN_UNPOISON_MEMORY_REGION((char*)block + kGCOHeader, page->blockSize - kGCOHeader);

        page->freeList = freegcolink(block);
        page->busyBlocks++;
    }

    // if we allocate the last block out of a page, we need to remove it from free list
    if (!page->freeList && page->freeNext < 0)
    {
        g->freegcopages[sizeClass] = page->next;
        if (page->next)
            page->next->prev = NULL;
        page->next = NULL;
    }

    // the user data is right after the metadata
    return (char*)block;
}

static void luaM_freeblock(lua_State* L, int sizeClass, void* block)
{
    global_State* g = L->global;

    // the user data is right after the metadata
    LUAU_ASSERT(block);
    block = (char*)block - kBlockHeader;

    lua_Page* page = (lua_Page*)metadata(block);
    LUAU_ASSERT(page && page->busyBlocks > 0);
    LUAU_ASSERT(size_t(page->blockSize) == kSizeClassConfig.sizeOfClass[sizeClass] + kBlockHeader);

    // if the page wasn't in the page free list, it should be now since it got a block!
    if (!page->freeList && page->freeNext < 0)
    {
        LUAU_ASSERT(!page->prev);
        LUAU_ASSERT(!page->next);

        page->next = g->freepages[sizeClass];
        if (page->next)
            page->next->prev = page;
        g->freepages[sizeClass] = page;
    }

    // add the block to the free list inside the page
    metadata(block) = page->freeList;
    page->freeList = block;

    ASAN_POISON_MEMORY_REGION(block, page->blockSize);

    page->busyBlocks--;

    // if it's the last block in the page, we don't need the page
    if (page->busyBlocks == 0)
    {
        if (FFlag::LuauGcPagedSweep)
            freeclasspage(L, g->freepages, NULL, page, sizeClass);
        else
            luaM_freepage(L, page, sizeClass);
    }
}

static void luaM_freegcoblock(lua_State* L, int sizeClass, void* block, lua_Page* page)
{
    LUAU_ASSERT(FFlag::LuauGcPagedSweep);

    global_State* g = L->global;

    // if the page wasn't in the page free list, it should be now since it got a block!
    if (!page->freeList && page->freeNext < 0)
    {
        LUAU_ASSERT(!page->prev);
        LUAU_ASSERT(!page->next);

        page->next = g->freegcopages[sizeClass];
        if (page->next)
            page->next->prev = page;
        g->freegcopages[sizeClass] = page;
    }

    // when separate block metadata is not used, free list link is stored inside the block data itself
    freegcolink(block) = page->freeList;
    page->freeList = (char*)block + kGCOHeader;

    ASAN_POISON_MEMORY_REGION((char*)block + kGCOHeader, page->blockSize - kGCOHeader);

    page->busyBlocks--;

    // if it's the last block in the page, we don't need the page
    if (page->busyBlocks == 0)
        freeclasspage(L, g->freegcopages, &g->allgcopages, page, sizeClass);
}

void* luaM_new_(lua_State* L, size_t nsize, uint8_t memcat)
{
    global_State* g = L->global;

    int nclass = sizeclass(nsize);

    void* block = nclass >= 0 ? luaM_newblock(L, nclass) : (*g->frealloc)(L, g->ud, NULL, 0, nsize);
    if (block == NULL && nsize > 0)
        luaD_throw(L, LUA_ERRMEM);

    g->totalbytes += nsize;
    g->memcatbytes[memcat] += nsize;

    return block;
}

GCObject* luaM_newgco_(lua_State* L, size_t nsize, uint8_t memcat)
{
    if (!FFlag::LuauGcPagedSweep)
        return (GCObject*)luaM_new_(L, nsize, memcat);

    global_State* g = L->global;

    int nclass = sizeclass(nsize);

    void* block = NULL;

    if (nclass >= 0)
    {
        LUAU_ASSERT(nsize > 8);

        block = luaM_newgcoblock(L, nclass);
    }
    else
    {
        lua_Page* page = newpage(L, &g->allgcopages, offsetof(lua_Page, data) + int(nsize), int(nsize), 1);

        block = &page->data;
        ASAN_UNPOISON_MEMORY_REGION(block, page->blockSize);

        page->freeNext -= page->blockSize;
        page->busyBlocks++;
    }

    if (block == NULL && nsize > 0)
        luaD_throw(L, LUA_ERRMEM);

    g->totalbytes += nsize;
    g->memcatbytes[memcat] += nsize;

    return (GCObject*)block;
}

void luaM_free_(lua_State* L, void* block, size_t osize, uint8_t memcat)
{
    global_State* g = L->global;
    LUAU_ASSERT((osize == 0) == (block == NULL));

    int oclass = sizeclass(osize);

    if (oclass >= 0)
        luaM_freeblock(L, oclass, block);
    else
        (*g->frealloc)(L, g->ud, block, osize, 0);

    g->totalbytes -= osize;
    g->memcatbytes[memcat] -= osize;
}

void luaM_freegco_(lua_State* L, GCObject* block, size_t osize, uint8_t memcat, lua_Page* page)
{
    if (!FFlag::LuauGcPagedSweep)
    {
        luaM_free_(L, block, osize, memcat);
        return;
    }

    global_State* g = L->global;
    LUAU_ASSERT((osize == 0) == (block == NULL));

    int oclass = sizeclass(osize);

    if (oclass >= 0)
    {
        block->gch.tt = LUA_TNIL;

        luaM_freegcoblock(L, oclass, block, page);
    }
    else
    {
        LUAU_ASSERT(page->busyBlocks == 1);

        freepage(L, &g->allgcopages, page);
    }

    g->totalbytes -= osize;
    g->memcatbytes[memcat] -= osize;
}

void* luaM_realloc_(lua_State* L, void* block, size_t osize, size_t nsize, uint8_t memcat)
{
    global_State* g = L->global;
    LUAU_ASSERT((osize == 0) == (block == NULL));

    int nclass = sizeclass(nsize);
    int oclass = sizeclass(osize);
    void* result;

    // if either block needs to be allocated using a block allocator, we can't use realloc directly
    if (nclass >= 0 || oclass >= 0)
    {
        result = nclass >= 0 ? luaM_newblock(L, nclass) : (*g->frealloc)(L, g->ud, NULL, 0, nsize);
        if (result == NULL && nsize > 0)
            luaD_throw(L, LUA_ERRMEM);

        if (osize > 0 && nsize > 0)
            memcpy(result, block, osize < nsize ? osize : nsize);

        if (oclass >= 0)
            luaM_freeblock(L, oclass, block);
        else
            (*g->frealloc)(L, g->ud, block, osize, 0);
    }
    else
    {
        result = (*g->frealloc)(L, g->ud, block, osize, nsize);
        if (result == NULL && nsize > 0)
            luaD_throw(L, LUA_ERRMEM);
    }

    LUAU_ASSERT((nsize == 0) == (result == NULL));
    g->totalbytes = (g->totalbytes - osize) + nsize;
    g->memcatbytes[memcat] += nsize - osize;
    return result;
}

void luaM_getpagewalkinfo(lua_Page* page, char** start, char** end, int* busyBlocks, int* blockSize)
{
    LUAU_ASSERT(FFlag::LuauGcPagedSweep);

    int blockCount = (page->pageSize - offsetof(lua_Page, data)) / page->blockSize;

    *start = page->data + page->freeNext + page->blockSize;
    *end = page->data + blockCount * page->blockSize;
    *busyBlocks = page->busyBlocks;
    *blockSize = page->blockSize;
}

lua_Page* luaM_getnextgcopage(lua_Page* page)
{
    return page->gcolistnext;
}

void luaM_visitpage(lua_Page* page, void* context, bool (*visitor)(void* context, lua_Page* page, GCObject* gco))
{
    LUAU_ASSERT(FFlag::LuauGcPagedSweep);

    char* start;
    char* end;
    int busyBlocks;
    int blockSize;
    luaM_getpagewalkinfo(page, &start, &end, &busyBlocks, &blockSize);

    for (char* pos = start; pos != end; pos += blockSize)
    {
        GCObject* gco = (GCObject*)pos;

        // skip memory blocks that are already freed
        if (gco->gch.tt == LUA_TNIL)
            continue;

        // when true is returned it means that the element was deleted
        if (visitor(context, page, gco))
        {
            // if the last block was removed, page would be removed as well
            if (--busyBlocks == 0)
                break;
        }
    }
}

void luaM_visitgco(lua_State* L, void* context, bool (*visitor)(void* context, lua_Page* page, GCObject* gco))
{
    LUAU_ASSERT(FFlag::LuauGcPagedSweep);

    global_State* g = L->global;

    for (lua_Page* curr = g->allgcopages; curr;)
    {
        lua_Page* next = curr->gcolistnext; // page blockvisit might destroy the page

        luaM_visitpage(curr, context, visitor);

        curr = next;
    }
}
