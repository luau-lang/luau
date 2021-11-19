// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeAttach.h"

#include "Luau/Error.h"
#include "Luau/Module.h"
#include "Luau/Parser.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/TypeVar.h"

#include <string>

LUAU_FASTFLAG(LuauTypeAliasPacks)

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

using SyntheticNames = std::unordered_map<const void*, char*>;

namespace Luau
{

static const char* getName(Allocator* allocator, SyntheticNames* syntheticNames, const Unifiable::Generic& gen)
{
    size_t s = syntheticNames->size();
    char*& n = (*syntheticNames)[&gen];
    if (!n)
    {
        std::string str = gen.explicitName ? gen.name : generateName(s);
        n = static_cast<char*>(allocator->allocate(str.size() + 1));
        strcpy(n, str.c_str());
    }

    return n;
}

class TypeRehydrationVisitor
{
    std::map<void*, int> seen;
    int count = 0;

    bool hasSeen(const void* tv)
    {
        void* ttv = const_cast<void*>(tv);
        auto it = seen.find(ttv);
        if (it != seen.end() && it->second < count)
            return true;

        seen[ttv] = count;
        return false;
    }

public:
    TypeRehydrationVisitor(Allocator* alloc, SyntheticNames* syntheticNames, const TypeRehydrationOptions& options = TypeRehydrationOptions())
        : allocator(alloc)
        , syntheticNames(syntheticNames)
        , options(options)
    {
    }

    AstTypePack* rehydrate(TypePackId tp);

    AstType* operator()(const PrimitiveTypeVar& ptv)
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

    AstType* operator()(const SingletonTypeVar& stv)
    {
        if (const BoolSingleton* bs = get<BoolSingleton>(&stv))
            return allocator->alloc<AstTypeSingletonBool>(Location(), bs->value);
        else if (const StringSingleton* ss = get<StringSingleton>(&stv))
        {
            AstArray<char> value;
            value.data = const_cast<char*>(ss->value.c_str());
            value.size = strlen(value.data);
            return allocator->alloc<AstTypeSingletonString>(Location(), value);
        }
        else
            return nullptr;
    }

    AstType* operator()(const AnyTypeVar&)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("any"));
    }
    AstType* operator()(const TableTypeVar& ttv)
    {
        RecursionCounter counter(&count);

        if (ttv.name && options.bannedNames.find(*ttv.name) == options.bannedNames.end())
        {
            AstArray<AstTypeOrPack> parameters;
            parameters.size = ttv.instantiatedTypeParams.size();
            parameters.data = static_cast<AstTypeOrPack*>(allocator->allocate(sizeof(AstTypeOrPack) * parameters.size));

            for (size_t i = 0; i < ttv.instantiatedTypeParams.size(); ++i)
            {
                parameters.data[i] = {Luau::visit(*this, ttv.instantiatedTypeParams[i]->ty), {}};
            }

            if (FFlag::LuauTypeAliasPacks)
            {
                for (size_t i = 0; i < ttv.instantiatedTypePackParams.size(); ++i)
                {
                    parameters.data[i] = {{}, rehydrate(ttv.instantiatedTypePackParams[i])};
                }
            }

            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName(ttv.name->c_str()), parameters.size != 0, parameters);
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

    AstType* operator()(const MetatableTypeVar& mtv)
    {
        return Luau::visit(*this, mtv.table->ty);
    }

    AstType* operator()(const ClassTypeVar& ctv)
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

    AstType* operator()(const FunctionTypeVar& ftv)
    {
        RecursionCounter counter(&count);

        if (hasSeen(&ftv))
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("<Cycle>"));

        AstArray<AstName> generics;
        generics.size = ftv.generics.size();
        generics.data = static_cast<AstName*>(allocator->allocate(sizeof(AstName) * generics.size));
        size_t numGenerics = 0;
        for (auto it = ftv.generics.begin(); it != ftv.generics.end(); ++it)
        {
            if (auto gtv = get<GenericTypeVar>(*it))
                generics.data[numGenerics++] = AstName(gtv->name.c_str());
        }

        AstArray<AstName> genericPacks;
        genericPacks.size = ftv.genericPacks.size();
        genericPacks.data = static_cast<AstName*>(allocator->allocate(sizeof(AstName) * genericPacks.size));
        size_t numGenericPacks = 0;
        for (auto it = ftv.genericPacks.begin(); it != ftv.genericPacks.end(); ++it)
        {
            if (auto gtv = get<GenericTypeVar>(*it))
                genericPacks.data[numGenericPacks++] = AstName(gtv->name.c_str());
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
            if (FFlag::LuauTypeAliasPacks)
            {
                argTailAnnotation = rehydrate(*argTail);
            }
            else
            {
                TypePackId tail = *argTail;
                if (const VariadicTypePack* vtp = get<VariadicTypePack>(tail))
                {
                    argTailAnnotation = allocator->alloc<AstTypePackVariadic>(Location(), Luau::visit(*this, vtp->ty->ty));
                }
            }
        }

        AstArray<std::optional<AstArgumentName>> argNames;
        argNames.size = ftv.argNames.size();
        argNames.data = static_cast<std::optional<AstArgumentName>*>(allocator->allocate(sizeof(std::optional<AstArgumentName>) * argNames.size));
        size_t i = 0;
        for (const auto& el : ftv.argNames)
        {
            std::optional<AstArgumentName>* arg = &argNames.data[i++];

            if (el)
                new (arg) std::optional<AstArgumentName>(AstArgumentName(AstName(el->name.c_str()), el->location));
            else
                new (arg) std::optional<AstArgumentName>();
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
            if (FFlag::LuauTypeAliasPacks)
            {
                retTailAnnotation = rehydrate(*retTail);
            }
            else
            {
                TypePackId tail = *retTail;
                if (const VariadicTypePack* vtp = get<VariadicTypePack>(tail))
                {
                    retTailAnnotation = allocator->alloc<AstTypePackVariadic>(Location(), Luau::visit(*this, vtp->ty->ty));
                }
            }
        }

        return allocator->alloc<AstTypeFunction>(
            Location(), generics, genericPacks, AstTypeList{argTypes, argTailAnnotation}, argNames, AstTypeList{returnTypes, retTailAnnotation});
    }
    AstType* operator()(const Unifiable::Error&)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("Unifiable<Error>"));
    }
    AstType* operator()(const GenericTypeVar& gtv)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName(getName(allocator, syntheticNames, gtv)));
    }
    AstType* operator()(const Unifiable::Bound<TypeId>& bound)
    {
        return Luau::visit(*this, bound.boundTo->ty);
    }
    AstType* operator()(const FreeTypeVar& ftv)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("free"));
    }
    AstType* operator()(const UnionTypeVar& uv)
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
    AstType* operator()(const IntersectionTypeVar& uv)
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
    AstType* operator()(const LazyTypeVar& ltv)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("<Lazy?>"));
    }

private:
    Allocator* allocator;
    SyntheticNames* syntheticNames;
    const TypeRehydrationOptions& options;
};

class TypePackRehydrationVisitor
{
public:
    TypePackRehydrationVisitor(Allocator* allocator, SyntheticNames* syntheticNames, TypeRehydrationVisitor* typeVisitor)
        : allocator(allocator)
        , syntheticNames(syntheticNames)
        , typeVisitor(typeVisitor)
    {
        LUAU_ASSERT(allocator);
        LUAU_ASSERT(syntheticNames);
        LUAU_ASSERT(typeVisitor);
    }

    AstTypePack* operator()(const BoundTypePack& btp) const
    {
        return Luau::visit(*this, btp.boundTo->ty);
    }

    AstTypePack* operator()(const TypePack& tp) const
    {
        AstArray<AstType*> head;
        head.size = tp.head.size();
        head.data = static_cast<AstType**>(allocator->allocate(sizeof(AstType*) * tp.head.size()));

        for (size_t i = 0; i < tp.head.size(); i++)
            head.data[i] = Luau::visit(*typeVisitor, tp.head[i]->ty);

        AstTypePack* tail = nullptr;

        if (tp.tail)
            tail = Luau::visit(*this, (*tp.tail)->ty);

        return allocator->alloc<AstTypePackExplicit>(Location(), AstTypeList{head, tail});
    }

    AstTypePack* operator()(const VariadicTypePack& vtp) const
    {
        return allocator->alloc<AstTypePackVariadic>(Location(), Luau::visit(*typeVisitor, vtp.ty->ty));
    }

    AstTypePack* operator()(const GenericTypePack& gtp) const
    {
        return allocator->alloc<AstTypePackGeneric>(Location(), AstName(getName(allocator, syntheticNames, gtp)));
    }

    AstTypePack* operator()(const FreeTypePack& gtp) const
    {
        return allocator->alloc<AstTypePackGeneric>(Location(), AstName("free"));
    }

    AstTypePack* operator()(const Unifiable::Error&) const
    {
        return allocator->alloc<AstTypePackGeneric>(Location(), AstName("Unifiable<Error>"));
    }

private:
    Allocator* allocator;
    SyntheticNames* syntheticNames;
    TypeRehydrationVisitor* typeVisitor;
};

AstTypePack* TypeRehydrationVisitor::rehydrate(TypePackId tp)
{
    TypePackRehydrationVisitor tprv(allocator, syntheticNames, this);
    return Luau::visit(tprv, tp->ty);
}

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
        return Luau::visit(TypeRehydrationVisitor(allocator, &syntheticNames), (*type)->ty);
    }

    AstArray<Luau::AstType*> typeAstPack(TypePackId type)
    {
        const auto& [v, tail] = flatten(type);

        AstArray<AstType*> result;
        result.size = v.size();
        result.data = static_cast<AstType**>(allocator->allocate(sizeof(AstType*) * v.size()));
        for (size_t i = 0; i < v.size(); ++i)
        {
            result.data[i] = Luau::visit(TypeRehydrationVisitor(allocator, &syntheticNames), v[i]->ty);
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
                    if (FFlag::LuauTypeAliasPacks)
                    {
                        variadicAnnotation = TypeRehydrationVisitor(allocator, &syntheticNames).rehydrate(*tail);
                    }
                    else
                    {
                        TypePackId tailPack = *tail;
                        if (const VariadicTypePack* vtp = get<VariadicTypePack>(tailPack))
                            variadicAnnotation = allocator->alloc<AstTypePackVariadic>(Location(), typeAst(vtp->ty));
                    }
                }

                fn->returnAnnotation = AstTypeList{typeAstPack(ret), variadicAnnotation};
            }
        }

        return true;
    }

private:
    Module& module;
    Allocator* allocator;
    SyntheticNames syntheticNames;
};

void attachTypeData(SourceModule& source, Module& result)
{
    TypeAttacher ta(result, source.allocator.get());
    source.root->visit(&ta);
}

AstType* rehydrateAnnotation(TypeId type, Allocator* allocator, const TypeRehydrationOptions& options)
{
    SyntheticNames syntheticNames;
    return Luau::visit(TypeRehydrationVisitor(allocator, &syntheticNames, options), type->ty);
}

} // namespace Luau
