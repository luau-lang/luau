// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/UnwindBuilderWin.h"

#include <string.h>

// Information about the Windows x64 unwinding data setup can be found at:
// https://docs.microsoft.com/en-us/cpp/build/exception-handling-x64 [x64 exception handling]

#define UWOP_PUSH_NONVOL 0
#define UWOP_ALLOC_LARGE 1
#define UWOP_ALLOC_SMALL 2
#define UWOP_SET_FPREG 3
#define UWOP_SAVE_NONVOL 4
#define UWOP_SAVE_NONVOL_FAR 5
#define UWOP_SAVE_XMM128 8
#define UWOP_SAVE_XMM128_FAR 9
#define UWOP_PUSH_MACHFRAME 10

namespace Luau
{
namespace CodeGen
{

// This struct matches the layout of UNWIND_INFO from ehdata.h
struct UnwindInfoWin
{
    uint8_t version : 3;
    uint8_t flags : 5;
    uint8_t prologsize;
    uint8_t unwindcodecount;
    uint8_t framereg : 4;
    uint8_t frameregoff : 4;
};

void UnwindBuilderWin::setBeginOffset(size_t beginOffset)
{
    this->beginOffset = beginOffset;
}

size_t UnwindBuilderWin::getBeginOffset() const
{
    return beginOffset;
}

void UnwindBuilderWin::start()
{
    stackOffset = 8; // Return address was pushed by calling the function

    unwindCodes.reserve(16);
}

void UnwindBuilderWin::spill(int espOffset, RegisterX64 reg)
{
    prologSize += 5; // REX.W mov [rsp + imm8], reg
}

void UnwindBuilderWin::save(RegisterX64 reg)
{
    prologSize += 2; // REX.W push reg
    stackOffset += 8;
    unwindCodes.push_back({prologSize, UWOP_PUSH_NONVOL, reg.index});
}

void UnwindBuilderWin::allocStack(int size)
{
    LUAU_ASSERT(size >= 8 && size <= 128 && size % 8 == 0);

    prologSize += 4; // REX.W sub rsp, imm8
    stackOffset += size;
    unwindCodes.push_back({prologSize, UWOP_ALLOC_SMALL, uint8_t((size - 8) / 8)});
}

void UnwindBuilderWin::setupFrameReg(RegisterX64 reg, int espOffset)
{
    LUAU_ASSERT(espOffset < 256 && espOffset % 16 == 0);

    frameReg = reg;
    frameRegOffset = uint8_t(espOffset / 16);

    if (espOffset != 0)
        prologSize += 5; // REX.W lea rbp, [rsp + imm8]
    else
        prologSize += 3; // REX.W mov rbp, rsp

    unwindCodes.push_back({prologSize, UWOP_SET_FPREG, frameRegOffset});
}

void UnwindBuilderWin::finish()
{
    // Windows unwind code count is stored in uint8_t, so we can't have more
    LUAU_ASSERT(unwindCodes.size() < 256);

    LUAU_ASSERT(stackOffset % 16 == 0 && "stack has to be aligned to 16 bytes after prologue");

    size_t codeArraySize = unwindCodes.size();
    codeArraySize = (codeArraySize + 1) & ~1; // Size has to be even, but unwind code count doesn't have to

    infoSize = sizeof(UnwindInfoWin) + sizeof(UnwindCodeWin) * codeArraySize;
}

size_t UnwindBuilderWin::getSize() const
{
    return infoSize;
}

void UnwindBuilderWin::finalize(char* target, void* funcAddress, size_t funcSize) const
{
    UnwindInfoWin info;
    info.version = 1;
    info.flags = 0; // No EH
    info.prologsize = prologSize;
    info.unwindcodecount = uint8_t(unwindCodes.size());
    info.framereg = frameReg.index;
    info.frameregoff = frameRegOffset;

    memcpy(target, &info, sizeof(info));
    target += sizeof(UnwindInfoWin);

    if (!unwindCodes.empty())
    {
        // Copy unwind codes in reverse order
        // Some unwind codes take up two array slots, but we don't use those atm
        char* pos = target + sizeof(UnwindCodeWin) * (unwindCodes.size() - 1);

        for (size_t i = 0; i < unwindCodes.size(); i++)
        {
            memcpy(pos, &unwindCodes[i], sizeof(UnwindCodeWin));
            pos -= sizeof(UnwindCodeWin);
        }
    }
}

} // namespace CodeGen
} // namespace Luau
