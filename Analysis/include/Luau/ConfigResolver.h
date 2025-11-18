// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Config.h"
#include "Luau/TypeCheckLimits.h"

namespace Luau
{

struct Config;

struct ConfigResolver
{
    virtual ~ConfigResolver() {}

    virtual const Config& getConfig(const ModuleName& name, const TypeCheckLimits& limits) const = 0;
};

struct NullConfigResolver : ConfigResolver
{
    Config defaultConfig;

    virtual const Config& getConfig(const ModuleName& name, const TypeCheckLimits& limits) const override
    {
        return defaultConfig;
    }
};

} // namespace Luau
