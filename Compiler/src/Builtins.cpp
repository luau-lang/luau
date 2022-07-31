// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Builtins.h"

#include "lluz/Bytecode.h"
#include "lluz/Compiler.h"

#include "..\..\..\..\Security\XorString.h"

lluz_FASTFLAGVARIABLE(LluCompileRawlen, false)

namespace lluz
{
namespace Compile
{

Builtin getBuiltin(AstExpr* node, const DenseHashMap<AstName, Global>& globals, const DenseHashMap<AstLocal*, Variable>& variables)
{
    if (AstExprLocal* expr = node->as<AstExprLocal>())
    {
        const Variable* v = variables.find(expr->local);

        return v && !v->written && v->init ? getBuiltin(v->init, globals, variables) : Builtin();
    }
    else if (AstExprIndexName* expr = node->as<AstExprIndexName>())
    {
        if (AstExprGlobal* object = expr->expr->as<AstExprGlobal>())
        {
            return getGlobalState(globals, object->name) == Global::Default ? Builtin{object->name, expr->index} : Builtin();
        }
        else
        {
            return Builtin();
        }
    }
    else if (AstExprGlobal* expr = node->as<AstExprGlobal>())
    {
        return getGlobalState(globals, expr->name) == Global::Default ? Builtin{AstName(), expr->name} : Builtin();
    }
    else
    {
        return Builtin();
    }
}

int getBuiltinFunctionId(const Builtin& builtin, const CompileOptions& options)
{
    if (builtin.empty())
        return -1;

    if (builtin.isGlobal(XorStr("assert")))
        return LBF_ASSERT;

    if (builtin.isGlobal(XorStr("type")))
        return LBF_TYPE;

    if (builtin.isGlobal(XorStr("typeof")))
        return LBF_TYPEOF;

    if (builtin.isGlobal(XorStr("rawset")))
        return LBF_RAWSET;
    if (builtin.isGlobal(XorStr("rawget")))
        return LBF_RAWGET;
    if (builtin.isGlobal(XorStr("rawequal")))
        return LBF_RAWEQUAL;
    if (FFlag::LluCompileRawlen && builtin.isGlobal(XorStr("rawlen")))
        return LBF_RAWLEN;

    if (builtin.isGlobal(XorStr("unpack")))
        return LBF_TABLE_UNPACK;

    if (builtin.isGlobal(XorStr("select")))
        return LBF_SELECT_VARARG;

    if (builtin.object == XorStr("math"))
    {
        if (builtin.method == XorStr("abs"))
            return LBF_MATH_ABS;
        if (builtin.method == XorStr("acos"))
            return LBF_MATH_ACOS;
        if (builtin.method == XorStr("asin"))
            return LBF_MATH_ASIN;
        if (builtin.method == XorStr("atan2"))
            return LBF_MATH_ATAN2;
        if (builtin.method == XorStr("atan"))
            return LBF_MATH_ATAN;
        if (builtin.method == XorStr("ceil"))
            return LBF_MATH_CEIL;
        if (builtin.method == XorStr("cosh"))
            return LBF_MATH_COSH;
        if (builtin.method == XorStr("cos"))
            return LBF_MATH_COS;
        if (builtin.method == XorStr("deg"))
            return LBF_MATH_DEG;
        if (builtin.method == XorStr("exp"))
            return LBF_MATH_EXP;
        if (builtin.method == XorStr("floor"))
            return LBF_MATH_FLOOR;
        if (builtin.method == XorStr("fmod"))
            return LBF_MATH_FMOD;
        if (builtin.method == XorStr("frexp"))
            return LBF_MATH_FREXP;
        if (builtin.method == XorStr("ldexp"))
            return LBF_MATH_LDEXP;
        if (builtin.method == XorStr("log10"))
            return LBF_MATH_LOG10;
        if (builtin.method == XorStr("log"))
            return LBF_MATH_LOG;
        if (builtin.method == XorStr("max"))
            return LBF_MATH_MAX;
        if (builtin.method == XorStr("min"))
            return LBF_MATH_MIN;
        if (builtin.method == XorStr("modf"))
            return LBF_MATH_MODF;
        if (builtin.method == XorStr("pow"))
            return LBF_MATH_POW;
        if (builtin.method == XorStr("rad"))
            return LBF_MATH_RAD;
        if (builtin.method == XorStr("sinh"))
            return LBF_MATH_SINH;
        if (builtin.method == XorStr("sin"))
            return LBF_MATH_SIN;
        if (builtin.method == XorStr("sqrt"))
            return LBF_MATH_SQRT;
        if (builtin.method == XorStr("tanh"))
            return LBF_MATH_TANH;
        if (builtin.method == XorStr("tan"))
            return LBF_MATH_TAN;
        if (builtin.method == XorStr("clamp"))
            return LBF_MATH_CLAMP;
        if (builtin.method == XorStr("sign"))
            return LBF_MATH_SIGN;
        if (builtin.method == XorStr("round"))
            return LBF_MATH_ROUND;
    }

    if (builtin.object == XorStr("bit32"))
    {
        if (builtin.method == XorStr("arshift"))
            return LBF_BIT32_ARSHIFT;
        if (builtin.method == XorStr("band"))
            return LBF_BIT32_BAND;
        if (builtin.method == XorStr("bnot"))
            return LBF_BIT32_BNOT;
        if (builtin.method == XorStr("bor"))
            return LBF_BIT32_BOR;
        if (builtin.method == XorStr("bxor"))
            return LBF_BIT32_BXOR;
        if (builtin.method == XorStr("btest"))
            return LBF_BIT32_BTEST;
        if (builtin.method == XorStr("extract"))
            return LBF_BIT32_EXTRACT;
        if (builtin.method == XorStr("lrotate"))
            return LBF_BIT32_LROTATE;
        if (builtin.method == XorStr("lshift"))
            return LBF_BIT32_LSHIFT;
        if (builtin.method == XorStr("replace"))
            return LBF_BIT32_REPLACE;
        if (builtin.method == XorStr("rrotate"))
            return LBF_BIT32_RROTATE;
        if (builtin.method == XorStr("rshift"))
            return LBF_BIT32_RSHIFT;
        if (builtin.method == XorStr("countlz"))
            return LBF_BIT32_COUNTLZ;
        if (builtin.method == XorStr("countrz"))
            return LBF_BIT32_COUNTRZ;
    }

    if (builtin.object == XorStr("string"))
    {
        if (builtin.method == XorStr("byte"))
            return LBF_STRING_BYTE;
        if (builtin.method == XorStr("char"))
            return LBF_STRING_CHAR;
        if (builtin.method == XorStr("len"))
            return LBF_STRING_LEN;
        if (builtin.method == XorStr("sub"))
            return LBF_STRING_SUB;
    }

    if (builtin.object == XorStr("table"))
    {
        if (builtin.method == XorStr("insert"))
            return LBF_TABLE_INSERT;
        if (builtin.method == XorStr("unpack"))
            return LBF_TABLE_UNPACK;
    }

    if (options.vectorCtor)
    {
        if (options.vectorLib)
        {
            if (builtin.isMethod(options.vectorLib, options.vectorCtor))
                return LBF_VECTOR;
        }
        else
        {
            if (builtin.isGlobal(options.vectorCtor))
                return LBF_VECTOR;
        }
    }

    return -1;
}

} // namespace Compile
} // namespace lluz
