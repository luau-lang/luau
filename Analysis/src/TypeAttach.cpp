// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeAttach.h"

#include "Luau/Ast.h"
#include "Luau/Module.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/Type.h"
#include "Luau/TypeFunction.h"

#include <string>

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

static const char* getName(Allocator* allocator, SyntheticNames* syntheticNames, const GenericType& gen)
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

static const char* getName(Allocator* allocator, SyntheticNames* syntheticNames, const GenericTypePack& gen)
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

    AstType* operator()(const PrimitiveType& ptv)
    {
        switch (ptv.type)
        {
        case PrimitiveType::NilType:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("nil"), std::nullopt, Location());
        case PrimitiveType::Boolean:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("boolean"), std::nullopt, Location());
        case PrimitiveType::Number:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("number"), std::nullopt, Location());
        case PrimitiveType::String:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("string"), std::nullopt, Location());
        case PrimitiveType::Thread:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("thread"), std::nullopt, Location());
        case PrimitiveType::Buffer:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("buffer"), std::nullopt, Location());
        case PrimitiveType::Function:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("function"), std::nullopt, Location());
        case PrimitiveType::Table:
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("table"), std::nullopt, Location());
        default:
            LUAU_ASSERT(false); // this should be unreachable.
            return nullptr;
        }
    }

    AstType* operator()(const BlockedType& btv)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("*blocked*"), std::nullopt, Location());
    }

    AstType* operator()(const PendingExpansionType& petv)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("*pending-expansion*"), std::nullopt, Location());
    }

    AstType* operator()(const SingletonType& stv)
    {
        if (const BooleanSingleton* bs = get<BooleanSingleton>(&stv))
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

    AstType* operator()(const AnyType&)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("any"), std::nullopt, Location());
    }

    AstType* operator()(const NoRefineType&)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("*no-refine*"), std::nullopt, Location());
    }

    AstType* operator()(const TableType& ttv)
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

            for (size_t i = 0; i < ttv.instantiatedTypePackParams.size(); ++i)
            {
                parameters.data[i] = {{}, rehydrate(ttv.instantiatedTypePackParams[i])};
            }

            return allocator->alloc<AstTypeReference>(
                Location(), std::nullopt, AstName(ttv.name->c_str()), std::nullopt, Location(), parameters.size != 0, parameters
            );
        }

        if (hasSeen(&ttv))
        {
            if (ttv.name)
                return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName(ttv.name->c_str()), std::nullopt, Location());
            else
                return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("<Cycle>"), std::nullopt, Location());
        }

        AstArray<AstTableProp> props;
        props.size = ttv.props.size();
        props.data = static_cast<AstTableProp*>(allocator->allocate(sizeof(AstTableProp) * props.size));
        int idx = 0;
        for (const auto& [propName, prop] : ttv.props)
        {
            RecursionCounter counter(&count);

            char* name = allocateString(*allocator, propName);

            if (prop.isShared())
            {
                props.data[idx].name = AstName(name);
                props.data[idx].type = Luau::visit(*this, (*prop.readTy)->ty);
                props.data[idx].access = AstTableAccess::ReadWrite;
                props.data[idx].location = Location();
                idx++;
            }
            else
            {
                if (prop.readTy)
                {
                    props.data[idx].name = AstName(name);
                    props.data[idx].type = Luau::visit(*this, (*prop.readTy)->ty);
                    props.data[idx].access = AstTableAccess::Read;
                    props.data[idx].location = Location();
                    idx++;
                }

                if (prop.writeTy)
                {
                    props.data[idx].name = AstName(name);
                    props.data[idx].type = Luau::visit(*this, (*prop.writeTy)->ty);
                    props.data[idx].access = AstTableAccess::Write;
                    props.data[idx].location = Location();
                    idx++;
                }
            }
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

    AstType* operator()(const MetatableType& mtv)
    {
        return Luau::visit(*this, mtv.table->ty);
    }

    AstType* operator()(const ExternType& etv)
    {
        RecursionCounter counter(&count);

        char* name = allocateString(*allocator, etv.name);

        if (!options.expandExternTypeProps || hasSeen(&etv) || count > 1)
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName{name}, std::nullopt, Location());

        AstArray<AstTableProp> props;
        props.size = etv.props.size();
        props.data = static_cast<AstTableProp*>(allocator->allocate(sizeof(AstTableProp) * props.size));

        int idx = 0;
        for (const auto& [propName, prop] : etv.props)
        {
            char* name = allocateString(*allocator, propName);

            if (prop.isShared())
            {
                props.data[idx].name = AstName(name);
                props.data[idx].type = Luau::visit(*this, (*prop.readTy)->ty);
                props.data[idx].access = AstTableAccess::ReadWrite;
                props.data[idx].location = Location();
                idx++;
            }
            else
            {
                if (prop.readTy)
                {
                    props.data[idx].name = AstName(name);
                    props.data[idx].type = Luau::visit(*this, (*prop.readTy)->ty);
                    props.data[idx].access = AstTableAccess::Read;
                    props.data[idx].location = Location();
                    idx++;
                }

                if (prop.writeTy)
                {
                    props.data[idx].name = AstName(name);
                    props.data[idx].type = Luau::visit(*this, (*prop.writeTy)->ty);
                    props.data[idx].access = AstTableAccess::Write;
                    props.data[idx].location = Location();
                    idx++;
                }
            }
        }

        AstTableIndexer* indexer = nullptr;
        if (etv.indexer)
        {
            RecursionCounter counter(&count);

            indexer = allocator->alloc<AstTableIndexer>();
            indexer->indexType = Luau::visit(*this, etv.indexer->indexType->ty);
            indexer->resultType = Luau::visit(*this, etv.indexer->indexResultType->ty);
        }

        return allocator->alloc<AstTypeTable>(Location(), props, indexer);
    }

    AstType* operator()(const FunctionType& ftv)
    {
        RecursionCounter counter(&count);

        if (hasSeen(&ftv))
            return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("<Cycle>"), std::nullopt, Location());

        AstArray<AstGenericType*> generics;
        generics.size = ftv.generics.size();
        generics.data = static_cast<AstGenericType**>(allocator->allocate(sizeof(AstGenericType) * generics.size));
        size_t numGenerics = 0;
        for (auto it = ftv.generics.begin(); it != ftv.generics.end(); ++it)
        {
            if (auto gtv = get<GenericType>(*it))
                generics.data[numGenerics++] = allocator->alloc<AstGenericType>(Location(), AstName(gtv->name.c_str()), nullptr);
        }

        AstArray<AstGenericTypePack*> genericPacks;
        genericPacks.size = ftv.genericPacks.size();
        genericPacks.data = static_cast<AstGenericTypePack**>(allocator->allocate(sizeof(AstGenericTypePack) * genericPacks.size));
        size_t numGenericPacks = 0;
        for (auto it = ftv.genericPacks.begin(); it != ftv.genericPacks.end(); ++it)
        {
            if (auto gtv = get<GenericTypePack>(*it))
                genericPacks.data[numGenericPacks++] = allocator->alloc<AstGenericTypePack>(Location(), AstName(gtv->name.c_str()), nullptr);
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
            argTailAnnotation = rehydrate(*argTail);

        AstArray<std::optional<AstArgumentName>> argNames;
        argNames.size = ftv.argNames.size();
        argNames.data = static_cast<std::optional<AstArgumentName>*>(allocator->allocate(sizeof(std::optional<AstArgumentName>) * argNames.size));
        size_t i = 0;
        for (const auto& el : ftv.argNames)
        {
            std::optional<AstArgumentName>* arg = &argNames.data[i++];

            if (el)
                new (arg) std::optional<AstArgumentName>(AstArgumentName(AstName(el->name.c_str()), Location()));
            else
                new (arg) std::optional<AstArgumentName>();
        }

        AstArray<AstType*> returnTypes;
        const auto& [retVector, retTail] = flatten(ftv.retTypes);
        returnTypes.size = retVector.size();
        returnTypes.data = static_cast<AstType**>(allocator->allocate(sizeof(AstType*) * returnTypes.size));
        for (size_t i = 0; i < returnTypes.size; ++i)
        {
            RecursionCounter counter(&count);

            returnTypes.data[i] = Luau::visit(*this, (retVector[i])->ty);
        }

        AstTypePack* retTailAnnotation = nullptr;
        if (retTail)
            retTailAnnotation = rehydrate(*retTail);

        auto returnAnnotation = allocator->alloc<AstTypePackExplicit>(Location(), AstTypeList{returnTypes, retTailAnnotation});
        return allocator->alloc<AstTypeFunction>(
            Location(), generics, genericPacks, AstTypeList{argTypes, argTailAnnotation}, argNames, returnAnnotation
        );
    }
    AstType* operator()(const ErrorType&)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("Unifiable<Error>"), std::nullopt, Location());
    }
    AstType* operator()(const GenericType& gtv)
    {
        return allocator->alloc<AstTypeReference>(
            Location(), std::nullopt, AstName(getName(allocator, syntheticNames, gtv)), std::nullopt, Location()
        );
    }
    AstType* operator()(const Unifiable::Bound<TypeId>& bound)
    {
        return Luau::visit(*this, bound.boundTo->ty);
    }
    AstType* operator()(const FreeType& ft)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("free"), std::nullopt, Location());
    }
    AstType* operator()(const UnionType& uv)
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
    AstType* operator()(const IntersectionType& uv)
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
    AstType* operator()(const LazyType& ltv)
    {
        if (TypeId unwrapped = ltv.unwrapped.load())
            return Luau::visit(*this, unwrapped->ty);

        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("<Lazy?>"), std::nullopt, Location());
    }
    AstType* operator()(const UnknownType& ttv)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName{"unknown"}, std::nullopt, Location());
    }
    AstType* operator()(const NeverType& ttv)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName{"never"}, std::nullopt, Location());
    }
    AstType* operator()(const NegationType& ntv)
    {
        AstArray<AstTypeOrPack> params;
        params.size = 1;
        params.data = static_cast<AstTypeOrPack*>(allocator->allocate(sizeof(AstType*)));
        params.data[0] = AstTypeOrPack{Luau::visit(*this, ntv.ty->ty), nullptr};

        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName("negate"), std::nullopt, Location(), true, params);
    }
    AstType* operator()(const TypeFunctionInstanceType& tfit)
    {
        return allocator->alloc<AstTypeReference>(Location(), std::nullopt, AstName{tfit.function->name.c_str()}, std::nullopt, Location());
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

    AstTypePack* operator()(const BlockedTypePack& btp) const
    {
        return allocator->alloc<AstTypePackGeneric>(Location(), AstName("*blocked*"));
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
        if (vtp.hidden)
            return nullptr;

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

    AstTypePack* operator()(const ErrorTypePack&) const
    {
        return allocator->alloc<AstTypePackGeneric>(Location(), AstName("Unifiable<Error>"));
    }

    AstTypePack* operator()(const TypeFunctionInstanceTypePack& tfitp) const
    {
        return allocator->alloc<AstTypePackGeneric>(Location(), AstName(tfitp.function->name.c_str()));
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
            if (auto scope = getScope(local->location))
            {
                if (auto result = scope->lookup(local))
                    local->annotation = typeAst(*result);
            }
        }
        return true;
    }

    virtual bool visit(AstExprLocal* al) override
    {
        return visitLocal(al->local);
    }

    virtual bool visit(AstStatFor* stat) override
    {
        visitLocal(stat->var);
        return true;
    }

    virtual bool visit(AstStatForIn* stat) override
    {
        for (size_t i = 0; i < stat->vars.size; ++i)
            visitLocal(stat->vars.data[i]);
        return true;
    }

    virtual bool visit(AstExprFunction* fn) override
    {
        // TODO: add generics if the inferred type of the function is generic CLI-39908
        for (size_t i = 0; i < fn->args.size; ++i)
        {
            AstLocal* arg = fn->args.data[i];
            visitLocal(arg);
        }

        if (!fn->returnAnnotation)
        {
            if (auto result = getScope(fn->body->location))
            {
                TypePackId ret = result->returnType;

                AstTypePack* variadicAnnotation = nullptr;
                const auto& [v, tail] = flatten(ret);

                if (tail)
                    variadicAnnotation = TypeRehydrationVisitor(allocator, &syntheticNames).rehydrate(*tail);

                fn->returnAnnotation = allocator->alloc<AstTypePackExplicit>(Location(), AstTypeList{typeAstPack(ret), variadicAnnotation});
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
