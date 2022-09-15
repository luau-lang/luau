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
    void start() override;

    void spill(int espOffset, RegisterX64 reg) override;
    void save(RegisterX64 reg) override;
    void allocStack(int size) override;
    void setupFrameReg(RegisterX64 reg, int espOffset) override;

    void finish() override;

    size_t getSize() const override;

    void finalize(char* target, void* funcAddress, size_t funcSize) const override;

private:
    static const unsigned kRawDataLimit = 128;
    char rawData[kRawDataLimit];
    char* pos = rawData;

    uint32_t stackOffset = 0;

    // We will remember the FDE location to write some of the fields like entry length, function start and size later
    char* fdeEntryStart = nullptr;
};

} // namespace CodeGen
} // namespace Luau
