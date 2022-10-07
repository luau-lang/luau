// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/RegisterX64.h"
#include "UnwindBuilder.h"

namespace Luau
{
namespace CodeGen
{

class UnwindBuilderDwarf2 : public UnwindBuilder
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

    static const unsigned kRawDataLimit = 128;
    uint8_t rawData[kRawDataLimit];
    uint8_t* pos = rawData;

    uint32_t stackOffset = 0;

    // We will remember the FDE location to write some of the fields like entry length, function start and size later
    uint8_t* fdeEntryStart = nullptr;
};

} // namespace CodeGen
} // namespace Luau
