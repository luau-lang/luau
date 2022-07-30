// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Scope.h"

namespace Luau
{

Scope::Scope(TypePackId returnType)
    : parent(nullptr)
    , returnType(returnType)
    , level(TypeLevel())
{
}

Scope::Scope(const ScopePtr& parent, int subLevel)
    : parent(parent)
    , returnType(parent->returnType)
    , level(parent->level.incr())
{
    level = level.incr();
    level.subLevel = subLevel;
}

std::optional<TypeFun> Scope::lookupType(const Name& name)
{
    const Scope* scope = this;
    while (true)
    {
        auto it = scope->exportedTypeBindings.find(name);
        if (it != scope->exportedTypeBindings.end())
            return it->second;

        it = scope->privateTypeBindings.find(name);
        if (it != scope->privateTypeBindings.end())
            return it->second;

        if (scope->parent)
            scope = scope->parent.get();
        else
            return std::nullopt;
    }
}

std::optional<TypeFun> Scope::lookupImportedType(const Name& moduleAlias, const Name& name)
{
    const Scope* scope = this;
    while (scope)
    {
        auto it = scope->importedTypeBindings.find(moduleAlias);
        if (it == scope->importedTypeBindings.end())
        {
            scope = scope->parent.get();
            continue;
        }

        auto it2 = it->second.find(name);
        if (it2 == it->second.end())
        {
            scope = scope->parent.get();
            continue;
        }

        return it2->second;
    }

    return std::nullopt;
}

std::optional<TypePackId> Scope::lookupPack(const Name& name)
{
    const Scope* scope = this;
    while (true)
    {
        auto it = scope->privateTypePackBindings.find(name);
        if (it != scope->privateTypePackBindings.end())
            return it->second;

        if (scope->parent)
            scope = scope->parent.get();
        else
            return std::nullopt;
    }
}

std::optional<Binding> Scope::linearSearchForBinding(const std::string& name, bool traverseScopeChain)
{
    Scope* scope = this;

    while (scope)
    {
        for (const auto& [n, binding] : scope->bindings)
        {
            if (n.local && n.local->name == name.c_str())
                return binding;
            else if (n.global.value && n.global == name.c_str())
                return binding;
        }

        scope = scope->parent.get();

        if (!traverseScopeChain)
            break;
    }

    return std::nullopt;
}

std::optional<TypeId> Scope::lookup(Symbol sym)
{
    Scope* s = this;

    while (true)
    {
        auto it = s->bindings.find(sym);
        if (it != s->bindings.end())
            return it->second.typeId;

        if (s->parent)
            s = s->parent.get();
        else
            return std::nullopt;
    }
}

std::optional<TypeId> Scope::lookupTypeBinding(const Name& name)
{
    Scope* s = this;
    while (s)
    {
        auto it = s->typeBindings.find(name);
        if (it != s->typeBindings.end())
            return it->second;

        s = s->parent.get();
    }

    return std::nullopt;
}

std::optional<TypePackId> Scope::lookupTypePackBinding(const Name& name)
{
    Scope* s = this;
    while (s)
    {
        auto it = s->typePackBindings.find(name);
        if (it != s->typePackBindings.end())
            return it->second;

        s = s->parent.get();
    }

    return std::nullopt;
}

} // namespace Luau
