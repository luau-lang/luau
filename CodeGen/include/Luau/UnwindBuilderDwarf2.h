// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/RegisterX64.h"
#include "UnwindBuilder.h"

#include <vector>

namespace Luau
{
namespace CodeGen
{

struct UnwindFunctionDwarf2
{
    uint32_t beginOffset;
    uint32_t endOffset;
    uint32_t fdeEntryStartPos;
};

class UnwindBuilderDwarf2 : public UnwindBuilder
{
public:
    void setBeginOffset(size_t beginOffset) override;
    size_t getBeginOffset() const override;

    void startInfo() override;

    void startFunction() override;
    void spill(int espOffset, X64::RegisterX64 reg) override;
    void save(X64::RegisterX64 reg) override;
    void allocStack(int size) override;
    void setupFrameReg(X64::RegisterX64 reg, int espOffset) override;
    void finishFunction(uint32_t beginOffset, uint32_t endOffset) override;

    void finishInfo() override;

    size_t getSize() const override;
    size_t getFunctionCount() const override;

    void finalize(char* target, size_t offset, void* funcAddress, size_t funcSize) const override;

private:
    size_t beginOffset = 0;

    std::vector<UnwindFunctionDwarf2> unwindFunctions;

    static const unsigned kRawDataLimit = 1024;
    uint8_t rawData[kRawDataLimit];
    uint8_t* pos = rawData;

    uint32_t stackOffset = 0;

    // We will remember the FDE location to write some of the fields like entry length, function start and size later
    uint8_t* fdeEntryStart = nullptr;
};

} // namespace CodeGen
} // namespace Luau
