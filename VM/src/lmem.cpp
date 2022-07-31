// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#pragma once

#include "lstate.h"
#include "ldo.h"
#include "ldebug.h"

//#include "..\..\..\..\Security\XorString.h"
#include <Windows.h>
#include <string.h>

uintptr_t unbaseaddr(uintptr_t address)
{
    return address - 0x400000 + reinterpret_cast<uintptr_t>(GetModuleHandleA(NULL));
}

uintptr_t sizeofclass_address = 0x40E3F38;
uintptr_t SizeOfClassMod = *(uintptr_t*)unbaseaddr(sizeofclass_address);

#ifndef __has_feature
#define __has_feature(x) 0
#endif

#if __has_feature(address_sanitizer) || defined(lluz_ENABLE_ASAN)
#include <sanitizer/asan_interface.h>
#define ASAN_POISON_MEMORY_REGION(addr, size) __asan_poison_memory_region((addr), (size))
#define ASAN_UNPOISON_MEMORY_REGION(addr, size) __asan_unpoison_memory_region((addr), (size))
#else
#define ASAN_POISON_MEMORY_REGION(addr, size) (void)0
#define ASAN_UNPOISON_MEMORY_REGION(addr, size) (void)0
#endif

/*
 * The sizes of lluz objects aren't crucial for code correctness, but they are crucial for memory efficiency
 * To prevent some of them accidentally growing and us losing memory without realizing it, we're going to lock
 * the sizes of all critical structures down.
 */
#if defined(__APPLE__)
#define ABISWITCH(x64, ms32, gcc32) (sizeof(void*) == 8 ? x64 : gcc32)
#elif defined(__i386__) && !defined(_MSC_VER)
#define ABISWITCH(x64, ms32, gcc32) (gcc32)
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
static_assert(offsetof(Udata, data) == ABISWITCH(16, 16, 12), "size mismatch for userdata header");
static_assert(sizeof(Table) == ABISWITCH(48, 32, 32), "size mismatch for table header");

const size_t kSizeClasses = LUA_SIZECLASSES;
const size_t kMaxSmallSize = 512;
const size_t kPageSize = 16 * 1024 - 24; // slightly under 16KB since that results in less fragmentation due to heap metadata

const size_t kBlockHeader = sizeof(double) > sizeof(void*) ? sizeof(double) : sizeof(void*); // suitable for aligning double & void* on all platforms
const size_t kGCOLinkOffset = (sizeof(GCheader) + sizeof(void*) - 1) & ~(sizeof(void*) - 1); // GCO pages contain freelist links after the GC header

struct SizeClassConfig
{
    int* sizeOfClass;
    int8_t* classForSize;
};

SizeClassConfig kSizeClassConfig;

void setup_luau_mem()
{
    *(uintptr_t*)&kSizeClassConfig.sizeOfClass = SizeOfClassMod - (sizeof(int) * 32); // int NOT void*
    *(uintptr_t*)&kSizeClassConfig.classForSize = SizeOfClassMod;
}

// size class for a block of size sz; returns -1 for size=0 because empty allocations take no space
#define sizeclass(sz) (size_t((sz)-1) < kMaxSmallSize ? kSizeClassConfig.classForSize[sz] : -1)

// metadata for a block is stored in the first pointer of the block
#define metadata(block) (*(void**)(block))
#define freegcolink(block) (*(void**)((char*)block + kGCOLinkOffset))

struct lua_Page
{
    // list of pages with free blocks
    lua_Page* prev;
    lua_Page* next;

    // list of all gco pages
    lua_Page* gcolistprev;
    lua_Page* gcolistnext;

    int pageSize;  // page size in bytes, including page header
    int blockSize; // block size in bytes, including block header (for non-GCO)

    void* freeList; // next free block in this page; linked with metadata()/freegcolink()
    int freeNext;   // next free block offset in this page, in bytes; when negative, freeList is used instead
    int busyBlocks; // number of blocks allocated out of this page

    union
    {
        char data[1];
        double align1;
        void* align2;
    };
};

l_noret luaM_toobig(lua_State* L)
{
    luaG_runerror(L, XorStr("memory allocation error: block too big"));
}

static lua_Page* newpage(lua_State* L, lua_Page** gcopageset, int pageSize, int blockSize, int blockCount)
{
    global_State* g = L->global;

    lluz_ASSERT(pageSize - int(offsetof(lua_Page, data)) >= blockSize * blockCount);

    lua_Page* page = (lua_Page*)(*g->frealloc)(g->ud, NULL, 0, pageSize);
    if (!page)
        luaD_throw(L, LUA_ERRMEM);

    ASAN_POISON_MEMORY_REGION(page->data, blockSize * blockCount);

    // setup page header
    page->prev = NULL;
    page->next = NULL;

    page->gcolistprev = NULL;
    page->gcolistnext = NULL;

    page->pageSize = pageSize;
    page->blockSize = blockSize;

    // note: we start with the last block in the page and move downward
    // either order would work, but that way we don't need to store the block count in the page
    // additionally, GC stores objects in singly linked lists, and this way the GC lists end up in increasing pointer order
    page->freeList = NULL;
    page->freeNext = (blockCount - 1) * blockSize;
    page->busyBlocks = 0;

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
    int blockSize = kSizeClassConfig.sizeOfClass[sizeClass] + (storeMetadata ? kBlockHeader : 0);
    int blockCount = (kPageSize - offsetof(lua_Page, data)) / blockSize;

    lua_Page* page = newpage(L, gcopageset, kPageSize, blockSize, blockCount);

    // prepend a page to page freelist (which is empty because we only ever allocate a new page when it is!)
    lluz_ASSERT(!freepageset[sizeClass]);
    freepageset[sizeClass] = page;

    return page;
}

static void freepage(lua_State* L, lua_Page** gcopageset, lua_Page* page)
{
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
    (*g->frealloc)(g->ud, page, page->pageSize, 0);
}

static void freeclasspage(lua_State* L, lua_Page** freepageset, lua_Page** gcopageset, lua_Page* page, uint8_t sizeClass)
{
    // remove page from freelist
    if (page->next)
        page->next->prev = page->prev;

    if (page->prev)
        page->prev->next = page->next;
    else if (freepageset[sizeClass] == page)
        freepageset[sizeClass] = page->next;

    freepage(L, gcopageset, page);
}

static void* newblock(lua_State* L, int sizeClass)
{
    global_State* g = L->global;
    lua_Page* page = g->freepages[sizeClass];

    // slow path: no page in the freelist, allocate a new one
    if (!page)
        page = newclasspage(L, g->freepages, NULL, sizeClass, true);

    lluz_ASSERT(!page->prev);
    lluz_ASSERT(page->freeList || page->freeNext >= 0);
    lluz_ASSERT(size_t(page->blockSize) == kSizeClassConfig.sizeOfClass[sizeClass] + kBlockHeader);

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

static void* newgcoblock(lua_State* L, int sizeClass)
{
    global_State* g = L->global;
    lua_Page* page = g->freegcopages[sizeClass];

    // slow path: no page in the freelist, allocate a new one
    if (!page)
        page = newclasspage(L, g->freegcopages, &g->allgcopages, sizeClass, false);

    lluz_ASSERT(!page->prev);
    lluz_ASSERT(page->freeList || page->freeNext >= 0);
    lluz_ASSERT(page->blockSize == kSizeClassConfig.sizeOfClass[sizeClass]);

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
        ASAN_UNPOISON_MEMORY_REGION((char*)block + sizeof(GCheader), page->blockSize - sizeof(GCheader));

        // when separate block metadata is not used, free list link is stored inside the block data itself
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

    return block;
}

static void freeblock(lua_State* L, int sizeClass, void* block)
{
    global_State* g = L->global;

    // the user data is right after the metadata
    lluz_ASSERT(block);
    block = (char*)block - kBlockHeader;

    lua_Page* page = (lua_Page*)metadata(block);
    lluz_ASSERT(page && page->busyBlocks > 0);
    lluz_ASSERT(size_t(page->blockSize) == kSizeClassConfig.sizeOfClass[sizeClass] + kBlockHeader);
    lluz_ASSERT(block >= page->data && block < (char*)page + page->pageSize);

    // if the page wasn't in the page free list, it should be now since it got a block!
    if (!page->freeList && page->freeNext < 0)
    {
        lluz_ASSERT(!page->prev);
        lluz_ASSERT(!page->next);

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
        freeclasspage(L, g->freepages, NULL, page, sizeClass);
}

static void freegcoblock(lua_State* L, int sizeClass, void* block, lua_Page* page)
{
    lluz_ASSERT(page && page->busyBlocks > 0);
    lluz_ASSERT(page->blockSize == kSizeClassConfig.sizeOfClass[sizeClass]);
    lluz_ASSERT(block >= page->data && block < (char*)page + page->pageSize);

    global_State* g = L->global;

    // if the page wasn't in the page free list, it should be now since it got a block!
    if (!page->freeList && page->freeNext < 0)
    {
        lluz_ASSERT(!page->prev);
        lluz_ASSERT(!page->next);

        page->next = g->freegcopages[sizeClass];
        if (page->next)
            page->next->prev = page;
        g->freegcopages[sizeClass] = page;
    }

    // when separate block metadata is not used, free list link is stored inside the block data itself
    freegcolink(block) = page->freeList;
    page->freeList = block;

    ASAN_POISON_MEMORY_REGION((char*)block + sizeof(GCheader), page->blockSize - sizeof(GCheader));

    page->busyBlocks--;

    // if it's the last block in the page, we don't need the page
    if (page->busyBlocks == 0)
        freeclasspage(L, g->freegcopages, &g->allgcopages, page, sizeClass);
}

void* luaM_new_(lua_State* L, size_t nsize, uint8_t memcat)
{
    global_State* g = L->global;

    int nclass = sizeclass(nsize);

    void* block = nclass >= 0 ? newblock(L, nclass) : (*g->frealloc)(g->ud, NULL, 0, nsize);
    if (block == NULL && nsize > 0)
        luaD_throw(L, LUA_ERRMEM);

    g->totalbytes += nsize;
    g->memcatbytes[memcat] += nsize;

    return block;
}

GCObject* luaM_newgco_(lua_State* L, size_t nsize, uint8_t memcat)
{
    // we need to accommodate space for link for free blocks (freegcolink)
    lluz_ASSERT(nsize >= kGCOLinkOffset + sizeof(void*));

    global_State* g = L->global;

    int nclass = sizeclass(nsize);

    void* block = NULL;

    if (nclass >= 0)
    {
        block = newgcoblock(L, nclass);
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
    lluz_ASSERT((osize == 0) == (block == NULL));

    int oclass = sizeclass(osize);

    if (oclass >= 0)
        freeblock(L, oclass, block);
    else
        (*g->frealloc)(g->ud, block, osize, 0);

    g->totalbytes -= osize;
    g->memcatbytes[memcat] -= osize;
}

void luaM_freegco_(lua_State* L, GCObject* block, size_t osize, uint8_t memcat, lua_Page* page)
{
    global_State* g = L->global;
    lluz_ASSERT((osize == 0) == (block == NULL));

    int oclass = sizeclass(osize);

    if (oclass >= 0)
    {
        block->gch.tt = LUA_TNIL;

        freegcoblock(L, oclass, block, page);
    }
    else
    {
        lluz_ASSERT(page->busyBlocks == 1);
        lluz_ASSERT(size_t(page->blockSize) == osize);
        lluz_ASSERT((void*)block == page->data);

        freepage(L, &g->allgcopages, page);
    }

    g->totalbytes -= osize;
    g->memcatbytes[memcat] -= osize;
}

void* luaM_realloc_(lua_State* L, void* block, size_t osize, size_t nsize, uint8_t memcat)
{
    global_State* g = L->global;
    lluz_ASSERT((osize == 0) == (block == NULL));

    int nclass = sizeclass(nsize);
    int oclass = sizeclass(osize);
    void* result;

    // if either block needs to be allocated using a block allocator, we can't use realloc directly
    if (nclass >= 0 || oclass >= 0)
    {
        result = nclass >= 0 ? newblock(L, nclass) : (*g->frealloc)(g->ud, NULL, 0, nsize);
        if (result == NULL && nsize > 0)
            luaD_throw(L, LUA_ERRMEM);

        if (osize > 0 && nsize > 0)
            memcpy(result, block, osize < nsize ? osize : nsize);

        if (oclass >= 0)
            freeblock(L, oclass, block);
        else
            (*g->frealloc)(g->ud, block, osize, 0);
    }
    else
    {
        result = (*g->frealloc)(g->ud, block, osize, nsize);
        if (result == NULL && nsize > 0)
            luaD_throw(L, LUA_ERRMEM);
    }

    lluz_ASSERT((nsize == 0) == (result == NULL));
    g->totalbytes = (g->totalbytes - osize) + nsize;
    g->memcatbytes[memcat] += nsize - osize;
    return result;
}

void luaM_getpagewalkinfo(lua_Page* page, char** start, char** end, int* busyBlocks, int* blockSize)
{
    int blockCount = (page->pageSize - offsetof(lua_Page, data)) / page->blockSize;

    lluz_ASSERT(page->freeNext >= -page->blockSize && page->freeNext <= (blockCount - 1) * page->blockSize);

    char* data = page->data; // silences ubsan when indexing page->data

    *start = data + page->freeNext + page->blockSize;
    *end = data + blockCount * page->blockSize;
    *busyBlocks = page->busyBlocks;
    *blockSize = page->blockSize;
}

lua_Page* luaM_getnextgcopage(lua_Page* page)
{
    return page->gcolistnext;
}

void luaM_visitpage(lua_Page* page, void* context, bool (*visitor)(void* context, lua_Page* page, GCObject* gco))
{
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
            lluz_ASSERT(busyBlocks > 0);

            // if the last block was removed, page would be removed as well
            if (--busyBlocks == 0)
                break;
        }
    }
}

void luaM_visitgco(lua_State* L, void* context, bool (*visitor)(void* context, lua_Page* page, GCObject* gco))
{
    global_State* g = L->global;

    for (lua_Page* curr = g->allgcopages; curr;)
    {
        lua_Page* next = curr->gcolistnext; // block visit might destroy the page

        luaM_visitpage(curr, context, visitor);

        curr = next;
    }
}
