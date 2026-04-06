// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeFunctionError.h"

#include "Luau/StringUtils.h"
#include "Luau/ToString.h"

namespace Luau
{

bool UnsupportedType::operator==(const UnsupportedType& rhs) const
{
    return type == rhs.type;
}

bool UnsupportedTypePack::operator==(const UnsupportedTypePack& rhs) const
{
    return pack == rhs.pack;
}

bool RuntimeError::operator==(const RuntimeError& rhs) const
{
    return message == rhs.message;
}

bool FailedToCompile::operator==(const FailedToCompile& rhs) const
{
    return functionName == rhs.functionName && compileError == rhs.compileError;
}

bool TypeFunctionMissing::operator==(const TypeFunctionMissing& rhs) const
{
    return functionName == rhs.functionName;
}

bool TypeFunctionError::operator==(const TypeFunctionError& rhs) const
{
    return location == rhs.location && moduleName == rhs.moduleName && data == rhs.data;
}

struct TypeFunctionErrorConverter
{
    std::string operator()(const UnsupportedType& e) const
    {
        return format("Type functions do not currently support types of the form '%s'", toString(e.type).c_str());
    }

    std::string operator()(const UnsupportedTypePack& e) const
    {
        return format("Type functions do not currently support types of the form '%s'", toString(e.pack).c_str());
    }

    std::string operator()(const RuntimeError& e) const
    {
        return e.message;
    }

    std::string operator()(const FailedToCompile& e) const
    {
        return format("'%s' type function failed to compile with error message: %s", e.functionName.c_str(), e.compileError.c_str());
    }

    std::string operator()(const TypeFunctionMissing& e) const
    {
        return format("Could not find '%s' type function in the global scope", e.functionName.c_str());
    }
};

std::string toString(const TypeFunctionError& error)
{
    TypeFunctionErrorConverter converter;
    return visit(converter, error.data);
}

} // namespace Luau
