// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/IrData.h"
#include "Luau/LogBuilder.h"

#include <array>

namespace Luau
{
namespace CodeGen
{

struct IrValueLocationTracking
{
    IrValueLocationTracking(LogBuilder* logger, IrFunction& function);

    void setRestoreCallback(void* context, void (*callback)(void* context, IrInst& inst));

    bool canBeRematerialized(IrCmd cmd);
    bool canRematerializeArguments(IrInst& inst);

    void processStoreLocationHint(const StoreLocationHint* hint);

    void beforeInstLowering(IrInst& inst);
    void afterInstLowering(IrInst& inst, uint32_t instIdx);

    void recordRestoreOp(uint32_t instIdx, IrOp location);
    void invalidateRestoreOp(IrOp location, bool skipValueInvalidation);
    void invalidateRestoreVmRegs(int start, int count);

    LogBuilder* logger = nullptr;
    IrFunction& function;

    std::array<uint32_t, 256> vmRegValue;

    // When a rematerializable value is stored through a conversion (like NUM_TO_UINT) we record it here to
    // invalidate both values when VM reg is invalidated
    std::array<uint32_t, 256> vmRegDependent;

    // For range/full invalidations, we only want to visit a limited number of data that we have recorded
    int maxReg = 0;

    void* restoreCallbackCtx = nullptr;
    void (*restoreCallback)(void* context, IrInst& inst) = nullptr;
};

} // namespace CodeGen
} // namespace Luau
