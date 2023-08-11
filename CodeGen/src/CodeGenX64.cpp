// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "CodeGenX64.h"

#include "Luau/AssemblyBuilderX64.h"
#include "Luau/UnwindBuilder.h"

#include "NativeState.h"
#include "EmitCommonX64.h"

#include "lstate.h"

/* An overview of native environment stack setup that we are making in the entry function:
 * Each line is 8 bytes, stack grows downwards.
 *
 * | ... previous frames ...
 * | rdx home space | (unused)
 * | rcx home space | (unused)
 * | return address |
 * | ... saved non-volatile registers ... <-- rsp + kStackSize + kLocalsSize
 * | unused         | for 16 byte alignment of the stack
 * | sCode          |
 * | sClosure       | <-- rsp + kStackSize
 * | argument 6     | <-- rsp + 40
 * | argument 5     | <-- rsp + 32
 * | r9 home space  |
 * | r8 home space  |
 * | rdx home space |
 * | rcx home space | <-- rsp points here
 *
 * Arguments to our entry function are saved to home space only on Windows.
 * Space for arguments to function we call is always reserved, but used only on Windows.
 *
 * Right now we use a frame pointer, but because of a fixed layout we can omit it in the future
 */

namespace Luau
{
namespace CodeGen
{
namespace X64
{

struct EntryLocations
{
    Label start;
    Label prologueEnd;
    Label epilogueStart;
};

static EntryLocations buildEntryFunction(AssemblyBuilderX64& build, UnwindBuilder& unwind)
{
    EntryLocations locations;

    build.align(kFunctionAlignment, X64::AlignmentDataX64::Ud2);

    locations.start = build.setLabel();
    unwind.startFunction();

    RegisterX64 rArg1 = (build.abi == ABIX64::Windows) ? rcx : rdi;
    RegisterX64 rArg2 = (build.abi == ABIX64::Windows) ? rdx : rsi;
    RegisterX64 rArg3 = (build.abi == ABIX64::Windows) ? r8 : rdx;
    RegisterX64 rArg4 = (build.abi == ABIX64::Windows) ? r9 : rcx;

    // Save common non-volatile registers
    if (build.abi == ABIX64::SystemV)
    {
        // We need to use a standard rbp-based frame setup for debuggers to work with JIT code
        build.push(rbp);
        build.mov(rbp, rsp);
    }

    build.push(rbx);
    build.push(r12);
    build.push(r13);
    build.push(r14);
    build.push(r15);

    if (build.abi == ABIX64::Windows)
    {
        // Save non-volatile registers that are specific to Windows x64 ABI
        build.push(rdi);
        build.push(rsi);

        // On Windows, rbp is available as a general-purpose non-volatile register; we currently don't use it, but we need to push an even number
        // of registers for stack alignment...
        build.push(rbp);

        // TODO: once we start using non-volatile SIMD registers on Windows, we will save those here
    }

    // Allocate stack space (reg home area + local data)
    build.sub(rsp, kStackSize + kLocalsSize);

    locations.prologueEnd = build.setLabel();

    uint32_t prologueSize = build.getLabelOffset(locations.prologueEnd) - build.getLabelOffset(locations.start);

    if (build.abi == ABIX64::SystemV)
        unwind.prologueX64(prologueSize, kStackSize + kLocalsSize, /* setupFrame= */ true, {rbx, r12, r13, r14, r15});
    else if (build.abi == ABIX64::Windows)
        unwind.prologueX64(prologueSize, kStackSize + kLocalsSize, /* setupFrame= */ false, {rbx, r12, r13, r14, r15, rdi, rsi, rbp});

    // Setup native execution environment
    build.mov(rState, rArg1);
    build.mov(rNativeContext, rArg4);
    build.mov(rBase, qword[rState + offsetof(lua_State, base)]); // L->base
    build.mov(rax, qword[rState + offsetof(lua_State, ci)]);     // L->ci
    build.mov(rax, qword[rax + offsetof(CallInfo, func)]);       // L->ci->func
    build.mov(rax, qword[rax + offsetof(TValue, value.gc)]);     // L->ci->func->value.gc aka cl
    build.mov(sClosure, rax);
    build.mov(rConstants, qword[rArg2 + offsetof(Proto, k)]); // proto->k
    build.mov(rax, qword[rArg2 + offsetof(Proto, code)]);     // proto->code
    build.mov(sCode, rax);

    // Jump to the specified instruction; further control flow will be handled with custom ABI with register setup from EmitCommonX64.h
    build.jmp(rArg3);

    // Even though we jumped away, we will return here in the end
    locations.epilogueStart = build.setLabel();

    // Cleanup and exit
    build.add(rsp, kStackSize + kLocalsSize);

    if (build.abi == ABIX64::Windows)
    {
        build.pop(rbp);
        build.pop(rsi);
        build.pop(rdi);
    }

    build.pop(r15);
    build.pop(r14);
    build.pop(r13);
    build.pop(r12);
    build.pop(rbx);

    if (build.abi == ABIX64::SystemV)
        build.pop(rbp);

    build.ret();

    // Our entry function is special, it spans the whole remaining code area
    unwind.finishFunction(build.getLabelOffset(locations.start), kFullBlockFuncton);

    return locations;
}

bool initHeaderFunctions(NativeState& data)
{
    AssemblyBuilderX64 build(/* logText= */ false);
    UnwindBuilder& unwind = *data.unwindBuilder.get();

    unwind.startInfo(UnwindBuilder::X64);

    EntryLocations entryLocations = buildEntryFunction(build, unwind);

    build.finalize();

    unwind.finishInfo();

    LUAU_ASSERT(build.data.empty());

    uint8_t* codeStart = nullptr;
    if (!data.codeAllocator.allocate(
            build.data.data(), int(build.data.size()), build.code.data(), int(build.code.size()), data.gateData, data.gateDataSize, codeStart))
    {
        LUAU_ASSERT(!"Failed to create entry function");
        return false;
    }

    // Set the offset at the begining so that functions in new blocks will not overlay the locations
    // specified by the unwind information of the entry function
    unwind.setBeginOffset(build.getLabelOffset(entryLocations.prologueEnd));

    data.context.gateEntry = codeStart + build.getLabelOffset(entryLocations.start);
    data.context.gateExit = codeStart + build.getLabelOffset(entryLocations.epilogueStart);

    return true;
}

void assembleHelpers(X64::AssemblyBuilderX64& build, ModuleHelpers& helpers)
{
    if (build.logText)
        build.logAppend("; updatePcAndContinueInVm\n");
    build.setLabel(helpers.updatePcAndContinueInVm);
    emitUpdatePcForExit(build);

    if (build.logText)
        build.logAppend("; exitContinueVmClearNativeFlag\n");
    build.setLabel(helpers.exitContinueVmClearNativeFlag);
    emitClearNativeFlag(build);

    if (build.logText)
        build.logAppend("; exitContinueVm\n");
    build.setLabel(helpers.exitContinueVm);
    emitExit(build, /* continueInVm */ true);

    if (build.logText)
        build.logAppend("; exitNoContinueVm\n");
    build.setLabel(helpers.exitNoContinueVm);
    emitExit(build, /* continueInVm */ false);

    if (build.logText)
        build.logAppend("; continueCallInVm\n");
    build.setLabel(helpers.continueCallInVm);
    emitContinueCallInVm(build);

    if (build.logText)
        build.logAppend("; interrupt\n");
    build.setLabel(helpers.interrupt);
    emitInterrupt(build);

    if (build.logText)
        build.logAppend("; return\n");
    build.setLabel(helpers.return_);
    emitReturn(build, helpers);
}

} // namespace X64
} // namespace CodeGen
} // namespace Luau
