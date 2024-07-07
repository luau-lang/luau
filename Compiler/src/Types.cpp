// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Types.h"

#include "Luau/BytecodeBuilder.h"

LUAU_FASTFLAG(LuauCompileUserdataInfo)

namespace Luau
{

static bool isGeneric(AstName name, const AstArray<AstGenericType>& generics)
{
    for (const AstGenericType& gt : generics)
        if (gt.name == name)
            return true;

    return false;
}

static LuauBytecodeType getPrimitiveType(AstName name)
{
    if (name == "nil")
        return LBC_TYPE_NIL;
    else if (name == "boolean")
        return LBC_TYPE_BOOLEAN;
    else if (name == "number")
        return LBC_TYPE_NUMBER;
    else if (name == "string")
        return LBC_TYPE_STRING;
    else if (name == "thread")
        return LBC_TYPE_THREAD;
    else if (name == "buffer")
        return LBC_TYPE_BUFFER;
    else if (name == "any" || name == "unknown")
        return LBC_TYPE_ANY;
    else
        return LBC_TYPE_INVALID;
}

static LuauBytecodeType getType(const AstType* ty, const AstArray<AstGenericType>& generics,
    const DenseHashMap<AstName, AstStatTypeAlias*>& typeAliases, bool resolveAliases, const char* vectorType,
    const DenseHashMap<AstName, uint8_t>& userdataTypes, BytecodeBuilder& bytecode)
{
    if (const AstTypeReference* ref = ty->as<AstTypeReference>())
    {
        if (ref->prefix)
            return LBC_TYPE_ANY;

        if (AstStatTypeAlias* const* alias = typeAliases.find(ref->name); alias && *alias)
        {
            // note: we only resolve aliases to the depth of 1 to avoid dealing with recursive aliases
            if (resolveAliases)
                return getType((*alias)->type, (*alias)->generics, typeAliases, /* resolveAliases= */ false, vectorType, userdataTypes, bytecode);
            else
                return LBC_TYPE_ANY;
        }

        if (isGeneric(ref->name, generics))
            return LBC_TYPE_ANY;

        if (vectorType && ref->name == vectorType)
            return LBC_TYPE_VECTOR;

        if (LuauBytecodeType prim = getPrimitiveType(ref->name); prim != LBC_TYPE_INVALID)
            return prim;

        if (FFlag::LuauCompileUserdataInfo)
        {
            if (const uint8_t* userdataIndex = userdataTypes.find(ref->name))
            {
                bytecode.useUserdataType(*userdataIndex);
                return LuauBytecodeType(LBC_TYPE_TAGGED_USERDATA_BASE + *userdataIndex);
            }
        }

        // not primitive or alias or generic => host-provided, we assume userdata for now
        return LBC_TYPE_USERDATA;
    }
    else if (const AstTypeTable* table = ty->as<AstTypeTable>())
    {
        return LBC_TYPE_TABLE;
    }
    else if (const AstTypeFunction* func = ty->as<AstTypeFunction>())
    {
        return LBC_TYPE_FUNCTION;
    }
    else if (const AstTypeUnion* un = ty->as<AstTypeUnion>())
    {
        bool optional = false;
        LuauBytecodeType type = LBC_TYPE_INVALID;

        for (AstType* ty : un->types)
        {
            LuauBytecodeType et = getType(ty, generics, typeAliases, resolveAliases, vectorType, userdataTypes, bytecode);

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

        return LuauBytecodeType(type | (optional && (type != LBC_TYPE_ANY) ? LBC_TYPE_OPTIONAL_BIT : 0));
    }
    else if (const AstTypeIntersection* inter = ty->as<AstTypeIntersection>())
    {
        return LBC_TYPE_ANY;
    }

    return LBC_TYPE_ANY;
}

static std::string getFunctionType(const AstExprFunction* func, const DenseHashMap<AstName, AstStatTypeAlias*>& typeAliases, const char* vectorType,
    const DenseHashMap<AstName, uint8_t>& userdataTypes, BytecodeBuilder& bytecode)
{
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
        LuauBytecodeType ty =
            arg->annotation ? getType(arg->annotation, func->generics, typeAliases, /* resolveAliases= */ true, vectorType, userdataTypes, bytecode)
                            : LBC_TYPE_ANY;

        if (ty != LBC_TYPE_ANY)
            haveNonAnyParam = true;

        typeInfo.push_back(ty);
    }

    // If all parameters simplify to any, we can just omit type info for this function
    if (!haveNonAnyParam)
        return {};

    return typeInfo;
}

static bool isMatchingGlobal(const DenseHashMap<AstName, Compile::Global>& globals, AstExpr* node, const char* name)
{
    if (AstExprGlobal* expr = node->as<AstExprGlobal>())
        return Compile::getGlobalState(globals, expr->name) == Compile::Global::Default && expr->name == name;

    return false;
}

struct TypeMapVisitor : AstVisitor
{
    DenseHashMap<AstExprFunction*, std::string>& functionTypes;
    DenseHashMap<AstLocal*, LuauBytecodeType>& localTypes;
    DenseHashMap<AstExpr*, LuauBytecodeType>& exprTypes;
    const char* vectorType;
    const DenseHashMap<AstName, uint8_t>& userdataTypes;
    const BuiltinTypes& builtinTypes;
    const DenseHashMap<AstExprCall*, int>& builtinCalls;
    const DenseHashMap<AstName, Compile::Global>& globals;
    BytecodeBuilder& bytecode;

    DenseHashMap<AstName, AstStatTypeAlias*> typeAliases;
    std::vector<std::pair<AstName, AstStatTypeAlias*>> typeAliasStack;
    DenseHashMap<AstLocal*, const AstType*> resolvedLocals;
    DenseHashMap<AstExpr*, const AstType*> resolvedExprs;

    TypeMapVisitor(DenseHashMap<AstExprFunction*, std::string>& functionTypes, DenseHashMap<AstLocal*, LuauBytecodeType>& localTypes,
        DenseHashMap<AstExpr*, LuauBytecodeType>& exprTypes, const char* vectorType, const DenseHashMap<AstName, uint8_t>& userdataTypes,
        const BuiltinTypes& builtinTypes, const DenseHashMap<AstExprCall*, int>& builtinCalls, const DenseHashMap<AstName, Compile::Global>& globals,
        BytecodeBuilder& bytecode)
        : functionTypes(functionTypes)
        , localTypes(localTypes)
        , exprTypes(exprTypes)
        , vectorType(vectorType)
        , userdataTypes(userdataTypes)
        , builtinTypes(builtinTypes)
        , builtinCalls(builtinCalls)
        , globals(globals)
        , bytecode(bytecode)
        , typeAliases(AstName())
        , resolvedLocals(nullptr)
        , resolvedExprs(nullptr)
    {
    }

    size_t pushTypeAliases(AstStatBlock* block)
    {
        size_t aliasStackTop = typeAliasStack.size();

        for (AstStat* stat : block->body)
            if (AstStatTypeAlias* alias = stat->as<AstStatTypeAlias>())
            {
                AstStatTypeAlias*& prevAlias = typeAliases[alias->name];

                typeAliasStack.push_back(std::make_pair(alias->name, prevAlias));
                prevAlias = alias;
            }

        return aliasStackTop;
    }

    void popTypeAliases(size_t aliasStackTop)
    {
        while (typeAliasStack.size() > aliasStackTop)
        {
            std::pair<AstName, AstStatTypeAlias*>& top = typeAliasStack.back();

            typeAliases[top.first] = top.second;
            typeAliasStack.pop_back();
        }
    }

    const AstType* resolveAliases(const AstType* ty)
    {
        if (const AstTypeReference* ref = ty->as<AstTypeReference>())
        {
            if (ref->prefix)
                return ty;

            if (AstStatTypeAlias* const* alias = typeAliases.find(ref->name); alias && *alias)
                return (*alias)->type;
        }

        return ty;
    }

    const AstTableIndexer* tryGetTableIndexer(AstExpr* expr)
    {
        if (const AstType** typePtr = resolvedExprs.find(expr))
        {
            if (const AstTypeTable* tableTy = (*typePtr)->as<AstTypeTable>())
                return tableTy->indexer;
        }

        return nullptr;
    }

    LuauBytecodeType recordResolvedType(AstExpr* expr, const AstType* ty)
    {
        ty = resolveAliases(ty);

        resolvedExprs[expr] = ty;

        LuauBytecodeType bty = getType(ty, {}, typeAliases, /* resolveAliases= */ true, vectorType, userdataTypes, bytecode);
        exprTypes[expr] = bty;
        return bty;
    }

    LuauBytecodeType recordResolvedType(AstLocal* local, const AstType* ty)
    {
        ty = resolveAliases(ty);

        resolvedLocals[local] = ty;

        LuauBytecodeType bty = getType(ty, {}, typeAliases, /* resolveAliases= */ true, vectorType, userdataTypes, bytecode);

        if (bty != LBC_TYPE_ANY)
            localTypes[local] = bty;

        return bty;
    }

    bool visit(AstStatBlock* node) override
    {
        size_t aliasStackTop = pushTypeAliases(node);

        for (AstStat* stat : node->body)
            stat->visit(this);

        popTypeAliases(aliasStackTop);

        return false;
    }

    // repeat..until scoping rules are such that condition (along with any possible functions declared in it) has aliases from repeat body in scope
    bool visit(AstStatRepeat* node) override
    {
        size_t aliasStackTop = pushTypeAliases(node->body);

        for (AstStat* stat : node->body->body)
            stat->visit(this);

        node->condition->visit(this);

        popTypeAliases(aliasStackTop);

        return false;
    }

    // for...in statement can contain type annotations on locals (we might even infer some for ipairs/pairs/generalized iteration)
    bool visit(AstStatForIn* node) override
    {
        for (AstExpr* expr : node->values)
            expr->visit(this);

        // This is similar to how Compiler matches builtin iteration, but we also handle generalized iteration case
        if (node->vars.size == 2 && node->values.size == 1)
        {
            if (AstExprCall* call = node->values.data[0]->as<AstExprCall>(); call && call->args.size == 1)
            {
                AstExpr* func = call->func;
                AstExpr* arg = call->args.data[0];

                if (isMatchingGlobal(globals, func, "ipairs"))
                {
                    if (const AstTableIndexer* indexer = tryGetTableIndexer(arg))
                    {
                        recordResolvedType(node->vars.data[0], &builtinTypes.numberType);
                        recordResolvedType(node->vars.data[1], indexer->resultType);
                    }
                }
                else if (isMatchingGlobal(globals, func, "pairs"))
                {
                    if (const AstTableIndexer* indexer = tryGetTableIndexer(arg))
                    {
                        recordResolvedType(node->vars.data[0], indexer->indexType);
                        recordResolvedType(node->vars.data[1], indexer->resultType);
                    }
                }
            }
            else if (const AstTableIndexer* indexer = tryGetTableIndexer(node->values.data[0]))
            {
                recordResolvedType(node->vars.data[0], indexer->indexType);
                recordResolvedType(node->vars.data[1], indexer->resultType);
            }
        }

        for (size_t i = 0; i < node->vars.size; i++)
        {
            AstLocal* var = node->vars.data[i];

            if (AstType* annotation = var->annotation)
                recordResolvedType(var, annotation);
        }

        node->body->visit(this);

        return false;
    }

    bool visit(AstExprFunction* node) override
    {
        std::string type = getFunctionType(node, typeAliases, vectorType, userdataTypes, bytecode);

        if (!type.empty())
            functionTypes[node] = std::move(type);

        return true; // Let generic visitor step into all expressions
    }

    bool visit(AstExprLocal* node) override
    {
        AstLocal* local = node->local;

        if (AstType* annotation = local->annotation)
        {
            LuauBytecodeType ty = recordResolvedType(node, annotation);

            if (ty != LBC_TYPE_ANY)
                localTypes[local] = ty;
        }
        else if (const AstType** typePtr = resolvedLocals.find(local))
        {
            localTypes[local] = recordResolvedType(node, *typePtr);
        }

        return false;
    }

    bool visit(AstStatLocal* node) override
    {
        for (AstExpr* expr : node->values)
            expr->visit(this);

        for (size_t i = 0; i < node->vars.size; i++)
        {
            AstLocal* var = node->vars.data[i];

            // Propagate from the value that's being assigned
            // This simple propagation doesn't handle type packs in tail position
            if (var->annotation == nullptr)
            {
                if (i < node->values.size)
                {
                    if (const AstType** typePtr = resolvedExprs.find(node->values.data[i]))
                        resolvedLocals[var] = *typePtr;
                }
            }
        }

        return false;
    }

    bool visit(AstExprIndexExpr* node) override
    {
        node->expr->visit(this);
        node->index->visit(this);

        if (const AstTableIndexer* indexer = tryGetTableIndexer(node->expr))
            recordResolvedType(node, indexer->resultType);

        return false;
    }

    bool visit(AstExprIndexName* node) override
    {
        node->expr->visit(this);

        if (const AstType** typePtr = resolvedExprs.find(node->expr))
        {
            if (const AstTypeTable* tableTy = (*typePtr)->as<AstTypeTable>())
            {
                for (const AstTableProp& prop : tableTy->props)
                {
                    if (prop.name == node->index)
                    {
                        recordResolvedType(node, prop.type);
                        return false;
                    }
                }
            }
        }

        if (LuauBytecodeType* typeBcPtr = exprTypes.find(node->expr))
        {
            if (*typeBcPtr == LBC_TYPE_VECTOR)
            {
                if (node->index == "X" || node->index == "Y" || node->index == "Z")
                    recordResolvedType(node, &builtinTypes.numberType);
            }
        }

        return false;
    }

    bool visit(AstExprUnary* node) override
    {
        node->expr->visit(this);

        switch (node->op)
        {
        case AstExprUnary::Not:
            recordResolvedType(node, &builtinTypes.booleanType);
            break;
        case AstExprUnary::Minus:
        {
            const AstType** typePtr = resolvedExprs.find(node->expr);
            LuauBytecodeType* bcTypePtr = exprTypes.find(node->expr);

            if (!typePtr || !bcTypePtr)
                return false;

            if (*bcTypePtr == LBC_TYPE_VECTOR)
                recordResolvedType(node, *typePtr);
            else if (*bcTypePtr == LBC_TYPE_NUMBER)
                recordResolvedType(node, *typePtr);

            break;
        }
        case AstExprUnary::Len:
            recordResolvedType(node, &builtinTypes.numberType);
            break;
        }

        return false;
    }

    bool visit(AstExprBinary* node) override
    {
        node->left->visit(this);
        node->right->visit(this);

        // Comparisons result in a boolean
        if (node->op == AstExprBinary::CompareNe || node->op == AstExprBinary::CompareEq || node->op == AstExprBinary::CompareLt ||
            node->op == AstExprBinary::CompareLe || node->op == AstExprBinary::CompareGt || node->op == AstExprBinary::CompareGe)
        {
            recordResolvedType(node, &builtinTypes.booleanType);
            return false;
        }

        if (node->op == AstExprBinary::Concat || node->op == AstExprBinary::And || node->op == AstExprBinary::Or)
            return false;

        const AstType** leftTypePtr = resolvedExprs.find(node->left);
        LuauBytecodeType* leftBcTypePtr = exprTypes.find(node->left);

        if (!leftTypePtr || !leftBcTypePtr)
            return false;

        const AstType** rightTypePtr = resolvedExprs.find(node->right);
        LuauBytecodeType* rightBcTypePtr = exprTypes.find(node->right);

        if (!rightTypePtr || !rightBcTypePtr)
            return false;

        if (*leftBcTypePtr == LBC_TYPE_VECTOR)
            recordResolvedType(node, *leftTypePtr);
        else if (*rightBcTypePtr == LBC_TYPE_VECTOR)
            recordResolvedType(node, *rightTypePtr);
        else if (*leftBcTypePtr == LBC_TYPE_NUMBER && *rightBcTypePtr == LBC_TYPE_NUMBER)
            recordResolvedType(node, *leftTypePtr);

        return false;
    }

    bool visit(AstExprGroup* node) override
    {
        node->expr->visit(this);

        if (const AstType** typePtr = resolvedExprs.find(node->expr))
            recordResolvedType(node, *typePtr);

        return false;
    }

    bool visit(AstExprTypeAssertion* node) override
    {
        node->expr->visit(this);

        recordResolvedType(node, node->annotation);

        return false;
    }

    bool visit(AstExprConstantBool* node) override
    {
        recordResolvedType(node, &builtinTypes.booleanType);

        return false;
    }

    bool visit(AstExprConstantNumber* node) override
    {
        recordResolvedType(node, &builtinTypes.numberType);

        return false;
    }

    bool visit(AstExprConstantString* node) override
    {
        recordResolvedType(node, &builtinTypes.stringType);

        return false;
    }

    bool visit(AstExprInterpString* node) override
    {
        recordResolvedType(node, &builtinTypes.stringType);

        return false;
    }

    bool visit(AstExprIfElse* node) override
    {
        node->condition->visit(this);
        node->trueExpr->visit(this);
        node->falseExpr->visit(this);

        const AstType** trueTypePtr = resolvedExprs.find(node->trueExpr);
        LuauBytecodeType* trueBcTypePtr = exprTypes.find(node->trueExpr);
        LuauBytecodeType* falseBcTypePtr = exprTypes.find(node->falseExpr);

        // Optimistic check that both expressions are of the same kind, as AstType* cannot be compared
        if (trueTypePtr && trueBcTypePtr && falseBcTypePtr && *trueBcTypePtr == *falseBcTypePtr)
            recordResolvedType(node, *trueTypePtr);

        return false;
    }

    bool visit(AstExprCall* node) override
    {
        if (const int* bfid = builtinCalls.find(node))
        {
            switch (LuauBuiltinFunction(*bfid))
            {
            case LBF_NONE:
            case LBF_ASSERT:
            case LBF_RAWSET:
            case LBF_RAWGET:
            case LBF_TABLE_INSERT:
            case LBF_TABLE_UNPACK:
            case LBF_SELECT_VARARG:
            case LBF_GETMETATABLE:
            case LBF_SETMETATABLE:
            case LBF_BUFFER_WRITEU8:
            case LBF_BUFFER_WRITEU16:
            case LBF_BUFFER_WRITEU32:
            case LBF_BUFFER_WRITEF32:
            case LBF_BUFFER_WRITEF64:
                break;
            case LBF_MATH_ABS:
            case LBF_MATH_ACOS:
            case LBF_MATH_ASIN:
            case LBF_MATH_ATAN2:
            case LBF_MATH_ATAN:
            case LBF_MATH_CEIL:
            case LBF_MATH_COSH:
            case LBF_MATH_COS:
            case LBF_MATH_DEG:
            case LBF_MATH_EXP:
            case LBF_MATH_FLOOR:
            case LBF_MATH_FMOD:
            case LBF_MATH_FREXP:
            case LBF_MATH_LDEXP:
            case LBF_MATH_LOG10:
            case LBF_MATH_LOG:
            case LBF_MATH_MAX:
            case LBF_MATH_MIN:
            case LBF_MATH_MODF:
            case LBF_MATH_POW:
            case LBF_MATH_RAD:
            case LBF_MATH_SINH:
            case LBF_MATH_SIN:
            case LBF_MATH_SQRT:
            case LBF_MATH_TANH:
            case LBF_MATH_TAN:
            case LBF_BIT32_ARSHIFT:
            case LBF_BIT32_BAND:
            case LBF_BIT32_BNOT:
            case LBF_BIT32_BOR:
            case LBF_BIT32_BXOR:
            case LBF_BIT32_BTEST:
            case LBF_BIT32_EXTRACT:
            case LBF_BIT32_LROTATE:
            case LBF_BIT32_LSHIFT:
            case LBF_BIT32_REPLACE:
            case LBF_BIT32_RROTATE:
            case LBF_BIT32_RSHIFT:
            case LBF_STRING_BYTE:
            case LBF_STRING_LEN:
            case LBF_MATH_CLAMP:
            case LBF_MATH_SIGN:
            case LBF_MATH_ROUND:
            case LBF_BIT32_COUNTLZ:
            case LBF_BIT32_COUNTRZ:
            case LBF_RAWLEN:
            case LBF_BIT32_EXTRACTK:
            case LBF_TONUMBER:
            case LBF_BIT32_BYTESWAP:
            case LBF_BUFFER_READI8:
            case LBF_BUFFER_READU8:
            case LBF_BUFFER_READI16:
            case LBF_BUFFER_READU16:
            case LBF_BUFFER_READI32:
            case LBF_BUFFER_READU32:
            case LBF_BUFFER_READF32:
            case LBF_BUFFER_READF64:
                recordResolvedType(node, &builtinTypes.numberType);
                break;

            case LBF_TYPE:
            case LBF_STRING_CHAR:
            case LBF_TYPEOF:
            case LBF_STRING_SUB:
            case LBF_TOSTRING:
                recordResolvedType(node, &builtinTypes.stringType);
                break;

            case LBF_RAWEQUAL:
                recordResolvedType(node, &builtinTypes.booleanType);
                break;

            case LBF_VECTOR:
                recordResolvedType(node, &builtinTypes.vectorType);
                break;
            }
        }

        return true; // Let generic visitor step into all expressions
    }

    // AstExpr classes that are not covered:
    // * AstExprConstantNil is not resolved to 'nil' because that doesn't help codegen operations and often used as an initializer before real value
    // * AstExprGlobal is not supported as we don't have info on globals
    // * AstExprVarargs cannot be resolved to a testable type
    // * AstExprTable cannot be reconstructed into a specific AstTypeTable and table annotations don't really help codegen
    // * AstExprCall is very complex (especially if builtins and registered globals are included), will be extended in the future
};

void buildTypeMap(DenseHashMap<AstExprFunction*, std::string>& functionTypes, DenseHashMap<AstLocal*, LuauBytecodeType>& localTypes,
    DenseHashMap<AstExpr*, LuauBytecodeType>& exprTypes, AstNode* root, const char* vectorType, const DenseHashMap<AstName, uint8_t>& userdataTypes,
    const BuiltinTypes& builtinTypes, const DenseHashMap<AstExprCall*, int>& builtinCalls, const DenseHashMap<AstName, Compile::Global>& globals,
    BytecodeBuilder& bytecode)
{
    TypeMapVisitor visitor(functionTypes, localTypes, exprTypes, vectorType, userdataTypes, builtinTypes, builtinCalls, globals, bytecode);
    root->visit(&visitor);
}

} // namespace Luau
