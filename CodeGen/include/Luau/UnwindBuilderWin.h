// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/RegisterX64.h"
#include "UnwindBuilder.h"

#include <vector>

namespace Luau
{
namespace CodeGen
{

// This struct matches the layout of UNWIND_CODE from ehdata.h
struct UnwindCodeWin
{
    uint8_t offset;
    uint8_t opcode : 4;
    uint8_t opinfo : 4;
};

class UnwindBuilderWin : public UnwindBuilder
{
public:
    void setBeginOffset(size_t beginOffset) override;
    size_t getBeginOffset() const override;

    void start() override;

    void spill(int espOffset, RegisterX64 reg) override;
    void save(RegisterX64 reg) override;
    void allocStack(int size) override;
    void setupFrameReg(RegisterX64 reg, int espOffset) override;

    void finish() override;

    size_t getSize() const override;

    void finalize(char* target, void* funcAddress, size_t funcSize) const override;

private:
    size_t beginOffset = 0;

    // Windows unwind codes are written in reverse, so we have to collect them all first
    std::vector<UnwindCodeWin> unwindCodes;

    uint8_t prologSize = 0;
    RegisterX64 frameReg = rax; // rax means that frame register is not used
    uint8_t frameRegOffset = 0;
    uint32_t stackOffset = 0;

    size_t infoSize = 0;
};

} // namespace CodeGen
} // namespace Luau
