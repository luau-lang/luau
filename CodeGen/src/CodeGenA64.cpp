// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "CodeGenA64.h"

#include "Luau/AssemblyBuilderA64.h"
#include "Luau/UnwindBuilder.h"

#include "CustomExecUtils.h"
#include "NativeState.h"

#include "lstate.h"

namespace Luau
{
namespace CodeGen
{
namespace A64
{

bool initEntryFunction(NativeState& data)
{
    AssemblyBuilderA64 build(/* logText= */ false);
    UnwindBuilder& unwind = *data.unwindBuilder.get();

    unwind.start();
    unwind.allocStack(8); // TODO: this is only necessary to align stack by 16 bytes, as start() allocates 8b return pointer

    // TODO: prologue goes here

    unwind.finish();

    size_t prologueSize = build.setLabel().location;

    // Setup native execution environment
    // TODO: figure out state layout

    // Jump to the specified instruction; further control flow will be handled with custom ABI with register setup from EmitCommonX64.h
    build.br(x2);

    // Even though we jumped away, we will return here in the end
    Label returnOff = build.setLabel();

    // Cleanup and exit
    // TODO: epilogue

    build.ret();

    build.finalize();

    LUAU_ASSERT(build.data.empty());

    if (!data.codeAllocator.allocate(build.data.data(), int(build.data.size()), reinterpret_cast<const uint8_t*>(build.code.data()),
            int(build.code.size() * sizeof(build.code[0])), data.gateData, data.gateDataSize, data.context.gateEntry))
    {
        LUAU_ASSERT(!"failed to create entry function");
        return false;
    }

    // Set the offset at the begining so that functions in new blocks will not overlay the locations
    // specified by the unwind information of the entry function
    unwind.setBeginOffset(prologueSize);

    data.context.gateExit = data.context.gateEntry + returnOff.location;

    return true;
}

} // namespace A64
} // namespace CodeGen
} // namespace Luau
