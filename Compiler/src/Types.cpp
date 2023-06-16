// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BytecodeBuilder.h"

#include "Types.h"

namespace Luau
{

static LuauBytecodeEncodedType getType(AstType* ty)
{
    if (AstTypeReference* ref = ty->as<AstTypeReference>())
    {
        if (ref->name == "nil")
            return LBC_TYPE_NIL;
        else if (ref->name == "boolean")
            return LBC_TYPE_BOOLEAN;
        else if (ref->name == "number")
            return LBC_TYPE_NUMBER;
        else if (ref->name == "string")
            return LBC_TYPE_STRING;
        else if (ref->name == "thread")
            return LBC_TYPE_THREAD;
        else if (ref->name == "any" || ref->name == "unknown")
            return LBC_TYPE_ANY;
    }
    else if (AstTypeTable* table = ty->as<AstTypeTable>())
    {
        return LBC_TYPE_TABLE;
    }
    else if (AstTypeFunction* func = ty->as<AstTypeFunction>())
    {
        return LBC_TYPE_FUNCTION;
    }
    else if (AstTypeUnion* un = ty->as<AstTypeUnion>())
    {
        bool optional = false;
        LuauBytecodeEncodedType type = LBC_TYPE_INVALID;

        for (AstType* ty : un->types)
        {
            LuauBytecodeEncodedType et = getType(ty);

            if (et == LBC_TYPE_NIL)
            {
                optional = true;
                continue;
            }

            if (type == LBC_TYPE_INVALID)
            {
                type = et;
                continue;
            }

            if (type != et)
                return LBC_TYPE_ANY;
        }

        if (type == LBC_TYPE_INVALID)
            return LBC_TYPE_ANY;

        return LuauBytecodeEncodedType(type | (optional && (type != LBC_TYPE_ANY) ? LBC_TYPE_OPTIONAL_BIT : 0));
    }
    else if (AstTypeIntersection* inter = ty->as<AstTypeIntersection>())
    {
        return LBC_TYPE_ANY;
    }

    return LBC_TYPE_ANY;
}

std::string getFunctionType(const AstExprFunction* func)
{
    if (func->vararg || func->generics.size || func->genericPacks.size)
        return {};

    bool self = func->self != 0;

    std::string typeInfo;
    typeInfo.reserve(func->args.size + self + 2);

    typeInfo.push_back(LBC_TYPE_FUNCTION);
    typeInfo.push_back(uint8_t(self + func->args.size));

    if (self)
        typeInfo.push_back(LBC_TYPE_TABLE);

    bool haveNonAnyParam = false;
    for (AstLocal* arg : func->args)
    {
        LuauBytecodeEncodedType ty = arg->annotation ? getType(arg->annotation) : LBC_TYPE_ANY;

        if (ty != LBC_TYPE_ANY)
            haveNonAnyParam = true;

        typeInfo.push_back(ty);
    }

    // If all parameters simplify to any, we can just omit type info for this function
    if (!haveNonAnyParam)
        return {};

    return typeInfo;
}

} // namespace Luau