// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Coverage.h"

#include "lua.h"

#include <string>
#include <vector>

struct Coverage
{
    lua_State* L = nullptr;
    std::vector<int> functions;
} gCoverage;

void coverageInit(lua_State* L)
{
    gCoverage.L = lua_mainthread(L);
}

bool coverageActive()
{
    return gCoverage.L != nullptr;
}

void coverageTrack(lua_State* L, int funcindex)
{
    int ref = lua_ref(L, funcindex);
    gCoverage.functions.push_back(ref);
}

static void coverageCallback(void* context, const char* function, int linedefined, int depth, const int* hits, size_t size)
{
    FILE* f = static_cast<FILE*>(context);

    std::string name;

    if (depth == 0)
        name = "<main>";
    else if (function)
        name = std::string(function) + ":" + std::to_string(linedefined);
    else
        name = "<anonymous>:" + std::to_string(linedefined);

    fprintf(f, "FN:%d,%s\n", linedefined, name.c_str());

    for (size_t i = 0; i < size; ++i)
        if (hits[i] != -1)
        {
            fprintf(f, "FNDA:%d,%s\n", hits[i], name.c_str());
            break;
        }

    for (size_t i = 0; i < size; ++i)
        if (hits[i] != -1)
            fprintf(f, "DA:%d,%d\n", int(i), hits[i]);
}

void coverageDump(const char* path)
{
    lua_State* L = gCoverage.L;

    FILE* f = fopen(path, "w");
    if (!f)
    {
        fprintf(stderr, "Error opening coverage %s\n", path);
        return;
    }

    fprintf(f, "TN:\n");

    for (int fref : gCoverage.functions)
    {
        lua_getref(L, fref);

        lua_Debug ar = {};
        lua_getinfo(L, -1, "s", &ar);

        fprintf(f, "SF:%s\n", ar.short_src);
        lua_getcoverage(L, -1, f, coverageCallback);
        fprintf(f, "end_of_record\n");

        lua_pop(L, 1);
    }

    fclose(f);

    printf("Coverage dump written to %s (%d functions)\n", path, int(gCoverage.functions.size()));
}
