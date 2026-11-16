// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"
#include "Luau/Common.h"

#include "lua.h"
#include "lualib.h"

#include <stdint.h>
#include <string.h>

using namespace Luau::TableFormat;

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
    {
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;
    }

    lua_State* L = luaL_newstate();

    DeserializeResult result = deserialize(L, std::string_view(reinterpret_cast<const char*>(Data), Size));
    LUAU_ASSERT(lua_gettop(L) == (result.errors.empty() ? 1 : 0));

    if (result.errors.empty())
    {
        // Anything that can be read can be written, and writing what was written reads back identically.
        SerializeResult written = serialize(L, -1);
        LUAU_ASSERT(written.source);

        LUAU_ASSERT(deserialize(L, *written.source).errors.empty());

        SerializeResult rewritten = serialize(L, -1);
        LUAU_ASSERT(rewritten.source == written.source);
    }

    lua_close(L);
    return 0;
}
