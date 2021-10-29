// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeAttach.h"

#include "Luau/Error.h"
#include "Luau/Module.h"
#include "Luau/Parser.h"
#include "Luau/RecursionCounter.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/TypeVar.h"

#include <string>

LUAU_FASTFLAG(LuauGenericFunctions)

static char* allocateString(Luau::Allocator& allocator, std::string_view contents)
{
    char* result = (char*)allocator.allocate(contents.size() + 1);
    memcpy(result, contents.data(), contents.size());
    result[contents.size()] = '\0';

    return result;
}

template<typename... Data>
static char* allocateString(Luau::Allocator& allocator, const char* format, Data... data)
{
    int len = snprintf(nullptr, 0, format, data...);
    char* result = (char*)allocator.allocate(len + 1);
    snprintf(result, len + 1, format, data...);
    return result;
}

namespace Luau
{

class TypeRehydrationVisitor
{
    mutable std::map<void*, int> seen;
    mutable int count = 0;

    bool hasSeen(const void* tv) const
    {
        void* ttv = const_cast<void*>(tv);
        auto it = seen.find(ttv);
        if (it != seen.end() && it->second < count)
            return true;

        seen[ttv] = count;
        return false;
    }

public:
    TypeRehydrationVisitor(Allocator* alloc, const TypeRehydrationOptions& options = TypeRehydrationOptions())
        : allocator(alloc)
        , options(options)
    {
    }

    AstType* operator()(const PrimitiveTypeVar& ptv) const
    {
        switch (ptv.type)
        {
        case PrimitiveTypeVar::NilType:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("nil"));
        case PrimitiveTypeVar::Boolean:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("boolean"));
        case PrimitiveTypeVar::Number:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("number"));
        case PrimitiveTypeVar::String:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("string"));
        case PrimitiveTypeVar::Thread:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("thread"));
        default:
            return nullptr;
        }
    }
    AstType* operator()(const AnyTypeVar&) const
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("any"));
    }
    AstType* operator()(const TableTypeVar& ttv) const
    {
        RecursionCounter counter(&count);

        if (ttv.name && options.bannedNames.find(*ttv.name) == options.bannedNames.end())
        {
            AstArray<AstType*> generics;
            generics.size = ttv.instantiatedTypeParams.size();
            generics.data = static_cast<AstType**>(allocator->allocate(sizeof(AstType*) * generics.size));

            for (size_t i = 0; i < ttv.instantiatedTypeParams.size(); ++i)
            {
                generics.data[i] = Luau::visit(*this, ttv.instantiatedTypeParams[i]->ty);
            }

            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName(ttv.name->c_str()), generics);
        }

        if (hasSeen(&ttv))
        {
            if (ttv.name)
                return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName(ttv.name->c_str()));
            else
                return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("<Cycle>"));
        }

        AstArray<AstTableProp> props;
        props.size = ttv.props.size();
        props.data = static_cast<AstTableProp*>(allocator->allocate(sizeof(AstTableProp) * props.size));
        int idx = 0;
        for (const auto& [propName, prop] : ttv.props)
        {
            RecursionCounter counter(&count);

            char* name = allocateString(*allocator, propName);

            props.data[idx].name = AstName(name);
            props.data[idx].type = Luau::visit(*this, prop.type->ty);
            props.data[idx].location = Location();
            idx++;
        }

        AstTableIndexer* indexer = nullptr;
        if (ttv.indexer)
        {
            RecursionCounter counter(&count);

            indexer = allocator->alloc<AstTableIndexer>();
            indexer->indexType = Luau::visit(*this, ttv.indexer->indexType->ty);
            indexer->resultType = Luau::visit(*this, ttv.indexer->indexResultType->ty);
        }
        return allocator->alloc<AstTypeTable>(Location(), props, indexer);
    }

    AstType* operator()(const MetatableTypeVar& mtv) const
    {
        return Luau::visit(*this, mtv.table->ty);
    }

    AstType* operator()(const ClassTypeVar& ctv) const
    {
        RecursionCounter counter(&count);

        char* name = allocateString(*allocator, ctv.name);

        if (!options.expandClassProps || hasSeen(&ctv) || count > 1)
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName{name});

        AstArray<AstTableProp> props;
        props.size = ctv.props.size();
        props.data = static_cast<AstTableProp*>(allocator->allocate(sizeof(AstTableProp) * props.size));

        int idx = 0;
        for (const auto& [propName, prop] : ctv.props)
        {
            char* name = allocateString(*allocator, propName);

            props.data[idx].name = AstName{name};
            props.data[idx].type = Luau::visit(*this, prop.type->ty);
            props.data[idx].location = Location();
            idx++;
        }

        return allocator->alloc<AstTypeTable>(Location(), props);
    }

    AstType* operator()(const FunctionTypeVar& ftv) const
    {
        RecursionCounter counter(&count);

        if (hasSeen(&ftv))
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("<Cycle>"));

        AstArray<AstName> generics;
        if (FFlag::LuauGenericFunctions)
        {
            generics.size = ftv.generics.size();
            generics.data = static_cast<AstName*>(allocator->allocate(sizeof(AstName) * generics.size));
            size_t i = 0;
            for (auto it = ftv.generics.begin(); it != ftv.generics.end(); ++it)
            {
                if (auto gtv = get<GenericTypeVar>(*it))
                    generics.data[i++] = AstName(gtv->name.c_str());
            }
        }
        else
        {
            generics.size = 0;
            generics.data = nullptr;
        }

        AstArray<AstName> genericPacks;
        if (FFlag::LuauGenericFunctions)
        {
            genericPacks.size = ftv.genericPacks.size();
            genericPacks.data = static_cast<AstName*>(allocator->allocate(sizeof(AstName) * genericPacks.size));
            size_t i = 0;
            for (auto it = ftv.genericPacks.begin(); it != ftv.genericPacks.end(); ++it)
            {
                if (auto gtv = get<GenericTypeVar>(*it))
                    genericPacks.data[i++] = AstName(gtv->name.c_str());
            }
        }
        else
        {
            generics.size = 0;
            generics.data = nullptr;
        }

        AstArray<AstType*> argTypes;
        const auto& [argVector, argTail] = flatten(ftv.argTypes);
        argTypes.size = argVector.size();
        argTypes.data = static_cast<AstType**>(allocator->allocate(sizeof(AstType*) * argTypes.size));
        for (size_t i = 0; i < argTypes.size; ++i)
        {
            RecursionCounter counter(&count);

            argTypes.data[i] = Luau::visit(*this, (argVector[i])->ty);
        }

        AstTypePack* argTailAnnotation = nullptr;
        if (argTail)
        {
            TypePackId tail = *argTail;
            if (const VariadicTypePack* vtp = get<VariadicTypePack>(tail))
            {
                argTailAnnotation = allocator->alloc<AstTypePackVariadic>(Location(), Luau::visit(*this, vtp->ty->ty));
            }
        }

        AstArray<std::optional<AstArgumentName>> argNames;
        argNames.size = ftv.argNames.size();
        argNames.data = static_cast<std::optional<AstArgumentName>*>(allocator->allocate(sizeof(std::optional<AstArgumentName>) * argNames.size));
        size_t i = 0;
        for (const auto& el : ftv.argNames)
        {
            if (el)
                argNames.data[i++] = {AstName(el->name.c_str()), el->location};
            else
                argNames.data[i++] = {};
        }

        AstArray<AstType*> returnTypes;
        const auto& [retVector, retTail] = flatten(ftv.retType);
        returnTypes.size = retVector.size();
        returnTypes.data = static_cast<AstType**>(allocator->allocate(sizeof(AstType*) * returnTypes.size));
        for (size_t i = 0; i < returnTypes.size; ++i)
        {
            RecursionCounter counter(&count);

            returnTypes.data[i] = Luau::visit(*this, (retVector[i])->ty);
        }

        AstTypePack* retTailAnnotation = nullptr;
        if (retTail)
        {
            TypePackId tail = *retTail;
            if (const VariadicTypePack* vtp = get<VariadicTypePack>(tail))
            {
                retTailAnnotation = allocator->alloc<AstTypePackVariadic>(Location(), Luau::visit(*this, vtp->ty->ty));
            }
        }

        return allocator->alloc<AstTypeFunction>(
            Location(), generics, genericPacks, AstTypeList{argTypes, argTailAnnotation}, argNames, AstTypeList{returnTypes, retTailAnnotation});
    }
    AstType* operator()(const Unifiable::Error&) const
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("Unifiable<Error>"));
    }
    AstType* operator()(const GenericTypeVar& gtv) const
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName(gtv.name.c_str()));
    }
    AstType* operator()(const Unifiable::Bound<TypeId>& bound) const
    {
        return Luau::visit(*this, bound.boundTo->ty);
    }
    AstType* operator()(Unifiable::Free ftv) const
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("free"));
    }
    AstType* operator()(const UnionTypeVar& uv) const
    {
        AstArray<AstType*> unionTypes;
        unionTypes.size = uv.options.size();
        unionTypes.data = static_cast<AstType**>(allocator->allocate(sizeof(AstType*) * unionTypes.size));
        for (size_t i = 0; i < unionTypes.size; ++i)
        {
            unionTypes.data[i] = Luau::visit(*this, uv.options[i]->ty);
        }
        return allocator->alloc<AstTypeUnion>(Location(), unionTypes);
    }
    AstType* operator()(const IntersectionTypeVar& uv) const
    {
        AstArray<AstType*> intersectionTypes;
        intersectionTypes.size = uv.parts.size();
        intersectionTypes.data = static_cast<AstType**>(allocator->allocate(sizeof(AstType*) * intersectionTypes.size));
        for (size_t i = 0; i < intersectionTypes.size; ++i)
        {
            intersectionTypes.data[i] = Luau::visit(*this, uv.parts[i]->ty);
        }
        return allocator->alloc<AstTypeIntersection>(Location(), intersectionTypes);
    }
    AstType* operator()(const LazyTypeVar& ltv) const
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("<Lazy?>"));
    }

private:
    Allocator* allocator;
    const TypeRehydrationOptions& options;
};

class TypeAttacher : public AstVisitor
{
public:
    TypeAttacher(Module& checker, Luau::Allocator* alloc)
        : module(checker)
        , allocator(alloc)
    {
    }
    ScopePtr getScope(const Location& loc)
    {
        Location scopeLocation;
        ScopePtr scope = nullptr;
        for (const auto& s : module.scopes)
        {
            if (s.first.encloses(loc))
            {
                if (!scope || scopeLocation.encloses(s.first))
                {
                    scopeLocation = s.first;
                    scope = s.second;
                }
            }
        }

        return scope;
    }

    AstType* typeAst(std::optional<TypeId> type)
    {
        if (!type)
            return nullptr;
        return Luau::visit(TypeRehydrationVisitor(allocator), (*type)->ty);
    }

    AstArray<Luau::AstType*> typeAstPack(TypePackId type)
    {
        const auto& [v, tail] = flatten(type);

        AstArray<AstType*> result;
        result.size = v.size();
        result.data = static_cast<AstType**>(allocator->allocate(sizeof(AstType*) * v.size()));
        for (size_t i = 0; i < v.size(); ++i)
        {
            result.data[i] = Luau::visit(TypeRehydrationVisitor(allocator), v[i]->ty);
        }
        return result;
    }

    virtual bool visit(AstStatLocal* al) override
    {
        for (size_t i = 0; i < al->vars.size; ++i)
        {
            visitLocal(al->vars.data[i]);
        }
        return true;
    }

    virtual bool visitLocal(AstLocal* local)
    {
        AstType* annotation = local->annotation;
        if (!annotation)
        {
            if (auto result = getScope(local->location)->lookup(local))
                local->annotation = typeAst(*result);
        }
        return true;
    }

    virtual bool visit(AstExprLocal* al) override
    {
        return visitLocal(al->local);
    }
    virtual bool visit(AstExprFunction* fn) override
    {
        // TODO: add generics if the inferred type of the function is generic CLI-39908
        for (size_t i = 0; i < fn->args.size; ++i)
        {
            AstLocal* arg = fn->args.data[i];
            visitLocal(arg);
        }

        if (!fn->hasReturnAnnotation)
        {
            if (auto result = getScope(fn->body->location))
            {
                TypePackId ret = result->returnType;
                fn->hasReturnAnnotation = true;

                AstTypePack* variadicAnnotation = nullptr;
                const auto& [v, tail] = flatten(ret);

                if (tail)
                {
                    TypePackId tailPack = *tail;
                    if (const VariadicTypePack* vtp = get<VariadicTypePack>(tailPack))
                        variadicAnnotation = allocator->alloc<AstTypePackVariadic>(Location(), typeAst(vtp->ty));
                }

                fn->returnAnnotation = AstTypeList{typeAstPack(ret), variadicAnnotation};
            }
        }

        return true;
    }

private:
    Module& module;
    Allocator* allocator;
};

void attachTypeData(SourceModule& source, Module& result)
{
    TypeAttacher ta(result, source.allocator.get());
    source.root->visit(&ta);
}

AstType* rehydrateAnnotation(TypeId type, Allocator* allocator, const TypeRehydrationOptions& options)
{
    return Luau::visit(TypeRehydrationVisitor(allocator, options), type->ty);
}

} // namespace Luau
