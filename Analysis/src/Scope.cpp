// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Scope.h"

LUAU_FASTFLAG(LuauSolverV2);

LUAU_FASTFLAGVARIABLE(LuauNoScopeShallNotSubsumeAll)
LUAU_FASTFLAG(LuauNameConstraintRestrictRecursiveTypes)

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
        return r->first->typeId;
    else
        return std::nullopt;
}

std::optional<std::pair<TypeId, Scope*>> Scope::lookupEx(DefId def)
{
    Scope* s = this;

    while (true)
    {
        if (TypeId* it = s->lvalueTypes.find(def))
            return std::pair{*it, s};
        else if (TypeId* it = s->rvalueRefinements.find(def))
            return std::pair{*it, s};

        if (s->parent)
            s = s->parent.get();
        else
            return std::nullopt;
    }
}

std::optional<std::pair<Binding*, Scope*>> Scope::lookupEx(Symbol sym)
{
    Scope* s = this;

    while (true)
    {
        auto it = s->bindings.find(sym);
        if (it != s->bindings.end())
            return std::pair{&it->second, s};

        if (s->parent)
            s = s->parent.get();
        else
            return std::nullopt;
    }
}

std::optional<TypeId> Scope::lookupUnrefinedType(DefId def) const
{
    for (const Scope* current = this; current; current = current->parent.get())
    {
        if (auto ty = current->lvalueTypes.find(def))
            return *ty;
    }

    return std::nullopt;
}

std::optional<TypeId> Scope::lookupRValueRefinementType(DefId def) const
{
    for (const Scope* current = this; current; current = current->parent.get())
    {
        if (auto ty = current->rvalueRefinements.find(def))
            return *ty;
    }

    return std::nullopt;
}

std::optional<TypeId> Scope::lookup(DefId def) const
{
    for (const Scope* current = this; current; current = current->parent.get())
    {
        if (auto ty = current->rvalueRefinements.find(def))
            return *ty;
        if (auto ty = current->lvalueTypes.find(def))
            return *ty;
    }

    return std::nullopt;
}

std::optional<TypeFun> Scope::lookupType(const Name& name) const
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

std::optional<TypeFun> Scope::lookupImportedType(const Name& moduleAlias, const Name& name) const
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

std::optional<TypePackId> Scope::lookupPack(const Name& name) const
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

std::optional<std::pair<Symbol, Binding>> Scope::linearSearchForBindingPair(const std::string& name, bool traverseScopeChain) const
{
    const Scope* scope = this;

    while (scope)
    {
        for (auto& [n, binding] : scope->bindings)
        {
            if (n.local && n.local->name == name.c_str())
                return {{n, binding}};
            else if (n.global.value && n.global == name.c_str())
                return {{n, binding}};
        }

        scope = scope->parent.get();

        if (!traverseScopeChain)
            break;
    }

    return std::nullopt;
}

// Updates the `this` scope with the assignments from the `childScope` including ones that doesn't exist in `this`.
void Scope::inheritAssignments(const ScopePtr& childScope)
{
    for (const auto& [k, a] : childScope->lvalueTypes)
        lvalueTypes[k] = a;
}

// Updates the `this` scope with the refinements from the `childScope` excluding ones that doesn't exist in `this`.
void Scope::inheritRefinements(const ScopePtr& childScope)
{
    for (const auto& [k, a] : childScope->rvalueRefinements)
    {
        if (lookup(NotNull{k}))
            rvalueRefinements[k] = a;
    }

    for (const auto& [k, a] : childScope->refinements)
    {
        Symbol symbol = getBaseSymbol(k);
        if (lookup(symbol))
            refinements[k] = a;
    }
}

bool Scope::shouldWarnGlobal(std::string name) const
{
    for (const Scope* current = this; current; current = current->parent.get())
    {
        if (current->globalsToWarn.contains(name))
            return true;
    }
    return false;
}

bool Scope::isInvalidTypeAliasName(const std::string& name) const
{
    LUAU_ASSERT(FFlag::LuauNameConstraintRestrictRecursiveTypes);

    for (auto scope = this; scope; scope = scope->parent.get())
    {
        if (scope->invalidTypeAliasNames.contains(name))
            return true;
    }

    return false;
}

NotNull<Scope> Scope::findNarrowestScopeContaining(Location location)
{
    Scope* bestScope = this;

    bool didNarrow;
    do
    {
        didNarrow = false;
        for (auto scope : bestScope->children)
        {
            if (scope->location.encloses(location))
            {
                bestScope = scope.get();
                didNarrow = true;
                break;
            }
        }
    } while (didNarrow && bestScope->children.size() > 0);

    return NotNull{bestScope};
}


bool subsumesStrict(Scope* left, Scope* right)
{
    if (FFlag::LuauNoScopeShallNotSubsumeAll)
    {
        if (!left || !right)
            return false;
    }

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
    if (FFlag::LuauNoScopeShallNotSubsumeAll)
    {
        if (!left || !right)
            return false;
    }

    return left == right || subsumesStrict(left, right);
}

} // namespace Luau
