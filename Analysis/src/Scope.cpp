// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Scope.h"

LUAU_FASTFLAG(LuauTwoPassAliasDefinitionFix);

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
    if (FFlag::LuauTwoPassAliasDefinitionFix)
        level = level.incr();
    level.subLevel = subLevel;
}

std::optional<TypeId> Scope::lookup(const Symbol& name)
{
    Scope* scope = this;

    while (scope)
    {
        auto it = scope->bindings.find(name);
        if (it != scope->bindings.end())
            return it->second.typeId;

        scope = scope->parent.get();
    }

    return std::nullopt;
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

} // namespace Luau
