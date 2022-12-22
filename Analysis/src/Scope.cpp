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

void Scope::addBuiltinTypeBinding(const Name& name, const TypeFun& tyFun)
{
    exportedTypeBindings[name] = tyFun;
    builtinTypeNames.insert(name);
}

std::optional<TypeId> Scope::lookup(Symbol sym) const
{
    auto r = const_cast<Scope*>(this)->lookupEx(sym);
    if (r)
        return r->first;
    else
        return std::nullopt;
}

std::optional<std::pair<TypeId, Scope*>> Scope::lookupEx(Symbol sym)
{
    Scope* s = this;

    while (true)
    {
        auto it = s->bindings.find(sym);
        if (it != s->bindings.end())
            return std::pair{it->second.typeId, s};

        if (s->parent)
            s = s->parent.get();
        else
            return std::nullopt;
    }
}

// TODO: We might kill Scope::lookup(Symbol) once data flow is fully fleshed out with type states and control flow analysis.
std::optional<TypeId> Scope::lookup(DefId def) const
{
    for (const Scope* current = this; current; current = current->parent.get())
    {
        if (auto ty = current->dcrRefinements.find(def))
            return *ty;
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

std::optional<Binding> Scope::linearSearchForBinding(const std::string& name, bool traverseScopeChain) const
{
    const Scope* scope = this;

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

bool subsumesStrict(Scope* left, Scope* right)
{
    while (right)
    {
        if (right->parent.get() == left)
            return true;

        right = right->parent.get();
    }

    return false;
}

bool subsumes(Scope* left, Scope* right)
{
    return left == right || subsumesStrict(left, right);
}

} // namespace Luau
