// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Types.h"

#include "Luau/BytecodeBuilder.h"

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
    else if (name == "any" || name == "unknown")
        return LBC_TYPE_ANY;
    else
        return LBC_TYPE_INVALID;
}

static LuauBytecodeType getType(AstType* ty, const AstArray<AstGenericType>& generics, const DenseHashMap<AstName, AstStatTypeAlias*>& typeAliases,
    bool resolveAliases, const char* vectorType)
{
    if (AstTypeReference* ref = ty->as<AstTypeReference>())
    {
        if (ref->prefix)
            return LBC_TYPE_ANY;

        if (AstStatTypeAlias* const* alias = typeAliases.find(ref->name); alias && *alias)
        {
            // note: we only resolve aliases to the depth of 1 to avoid dealing with recursive aliases
            if (resolveAliases)
                return getType((*alias)->type, (*alias)->generics, typeAliases, /* resolveAliases= */ false, vectorType);
            else
                return LBC_TYPE_ANY;
        }

        if (isGeneric(ref->name, generics))
            return LBC_TYPE_ANY;

        if (vectorType && ref->name == vectorType)
            return LBC_TYPE_VECTOR;

        if (LuauBytecodeType prim = getPrimitiveType(ref->name); prim != LBC_TYPE_INVALID)
            return prim;

        // not primitive or alias or generic => host-provided, we assume userdata for now
        return LBC_TYPE_USERDATA;
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
        LuauBytecodeType type = LBC_TYPE_INVALID;

        for (AstType* ty : un->types)
        {
            LuauBytecodeType et = getType(ty, generics, typeAliases, resolveAliases, vectorType);

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
    else if (AstTypeIntersection* inter = ty->as<AstTypeIntersection>())
    {
        return LBC_TYPE_ANY;
    }

    return LBC_TYPE_ANY;
}

static std::string getFunctionType(const AstExprFunction* func, const DenseHashMap<AstName, AstStatTypeAlias*>& typeAliases, const char* vectorType)
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
            arg->annotation ? getType(arg->annotation, func->generics, typeAliases, /* resolveAliases= */ true, vectorType) : LBC_TYPE_ANY;

        if (ty != LBC_TYPE_ANY)
            haveNonAnyParam = true;

        typeInfo.push_back(ty);
    }

    // If all parameters simplify to any, we can just omit type info for this function
    if (!haveNonAnyParam)
        return {};

    return typeInfo;
}

struct TypeMapVisitor : AstVisitor
{
    DenseHashMap<AstExprFunction*, std::string>& typeMap;
    const char* vectorType;

    DenseHashMap<AstName, AstStatTypeAlias*> typeAliases;
    std::vector<std::pair<AstName, AstStatTypeAlias*>> typeAliasStack;

    TypeMapVisitor(DenseHashMap<AstExprFunction*, std::string>& typeMap, const char* vectorType)
        : typeMap(typeMap)
        , vectorType(vectorType)
        , typeAliases(AstName())
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

    bool visit(AstExprFunction* node) override
    {
        std::string type = getFunctionType(node, typeAliases, vectorType);

        if (!type.empty())
            typeMap[node] = std::move(type);

        return true;
    }
};

void buildTypeMap(DenseHashMap<AstExprFunction*, std::string>& typeMap, AstNode* root, const char* vectorType)
{
    TypeMapVisitor visitor(typeMap, vectorType);
    root->visit(&visitor);
}

} // namespace Luau
