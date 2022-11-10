// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "CodeGenX64.h"

#include "Luau/AssemblyBuilderX64.h"
#include "Luau/UnwindBuilder.h"

#include "CustomExecUtils.h"
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
namespace x64
{

bool initEntryFunction(NativeState& data)
{
    AssemblyBuilderX64 build(/* logText= */ false);
    UnwindBuilder& unwind = *data.unwindBuilder.get();

    unwind.start();

    // Save common non-volatile registers
    build.push(rbp);
    unwind.save(rbp);

    if (build.abi == ABIX64::SystemV)
    {
        build.mov(rbp, rsp);
        unwind.setupFrameReg(rbp, 0);
    }

    build.push(rbx);
    unwind.save(rbx);
    build.push(r12);
    unwind.save(r12);
    build.push(r13);
    unwind.save(r13);
    build.push(r14);
    unwind.save(r14);
    build.push(r15);
    unwind.save(r15);

    if (build.abi == ABIX64::Windows)
    {
        // Save non-volatile registers that are specific to Windows x64 ABI
        build.push(rdi);
        unwind.save(rdi);
        build.push(rsi);
        unwind.save(rsi);

        // TODO: once we start using non-volatile SIMD registers on Windows, we will save those here
    }

    // Allocate stack space (reg home area + local data)
    build.sub(rsp, kStackSize + kLocalsSize);
    unwind.allocStack(kStackSize + kLocalsSize);

    unwind.finish();

    size_t prologueSize = build.setLabel().location;

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
    Label returnOff = build.setLabel();

    // Cleanup and exit
    build.add(rsp, kStackSize + kLocalsSize);

    if (build.abi == ABIX64::Windows)
    {
        build.pop(rsi);
        build.pop(rdi);
    }

    build.pop(r15);
    build.pop(r14);
    build.pop(r13);
    build.pop(r12);
    build.pop(rbx);
    build.pop(rbp);
    build.ret();

    build.finalize();

    LUAU_ASSERT(build.data.empty());

    if (!data.codeAllocator.allocate(build.data.data(), int(build.data.size()), build.code.data(), int(build.code.size()), data.gateData,
            data.gateDataSize, data.context.gateEntry))
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

} // namespace x64
} // namespace CodeGen
} // namespace Luau
