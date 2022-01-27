// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/FileResolver.h"
#include "Luau/TypePack.h"
#include "Luau/TypedAllocator.h"
#include "Luau/ParseOptions.h"
#include "Luau/Error.h"
#include "Luau/Parser.h"

#include <memory>
#include <vector>
#include <unordered_map>
#include <optional>

LUAU_FASTFLAG(LuauPrepopulateUnionOptionsBeforeAllocation)

namespace Luau
{

struct Module;

using ScopePtr = std::shared_ptr<struct Scope>;
using ModulePtr = std::shared_ptr<Module>;

/// Root of the AST of a parsed source file
struct SourceModule
{
    ModuleName name; // DataModel path if possible.  Filename if not.
    SourceCode::Type type = SourceCode::None;
    std::optional<std::string> environmentName;
    bool cyclic = false;

    std::unique_ptr<Allocator> allocator;
    std::unique_ptr<AstNameTable> names;
    std::vector<ParseError> parseErrors;

    AstStatBlock* root = nullptr;
    std::optional<Mode> mode;
    uint64_t ignoreLints = 0;

    std::vector<Comment> commentLocations;

    SourceModule()
        : allocator(new Allocator)
        , names(new AstNameTable(*allocator))
    {
    }
};

bool isWithinComment(const SourceModule& sourceModule, Position pos);

struct TypeArena
{
    TypedAllocator<TypeVar> typeVars;
    TypedAllocator<TypePackVar> typePacks;

    void clear();

    template<typename T>
    TypeId addType(T tv)
    {
        if (FFlag::LuauPrepopulateUnionOptionsBeforeAllocation)
        {
            if constexpr (std::is_same_v<T, UnionTypeVar>)
                LUAU_ASSERT(tv.options.size() >= 2);
        }

        return addTV(TypeVar(std::move(tv)));
    }

    TypeId addTV(TypeVar&& tv);

    TypeId freshType(TypeLevel level);

    TypePackId addTypePack(std::initializer_list<TypeId> types);
    TypePackId addTypePack(std::vector<TypeId> types);
    TypePackId addTypePack(TypePack pack);
    TypePackId addTypePack(TypePackVar pack);
};

void freeze(TypeArena& arena);
void unfreeze(TypeArena& arena);

// Only exposed so they can be unit tested.
using SeenTypes = std::unordered_map<TypeId, TypeId>;
using SeenTypePacks = std::unordered_map<TypePackId, TypePackId>;

struct CloneState
{
    int recursionCount = 0;
    bool encounteredFreeType = false;
};

TypePackId clone(TypePackId tp, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, CloneState& cloneState);
TypeId clone(TypeId tp, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, CloneState& cloneState);
TypeFun clone(const TypeFun& typeFun, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, CloneState& cloneState);

struct Module
{
    ~Module();

    TypeArena interfaceTypes;
    TypeArena internalTypes;

    std::vector<std::pair<Location, ScopePtr>> scopes; // never empty

    DenseHashMap<const AstExpr*, TypeId> astTypes{nullptr};
    DenseHashMap<const AstExpr*, TypeId> astExpectedTypes{nullptr};
    DenseHashMap<const AstExpr*, TypeId> astOriginalCallTypes{nullptr};
    DenseHashMap<const AstExpr*, TypeId> astOverloadResolvedTypes{nullptr};

    std::unordered_map<Name, TypeId> declaredGlobals;
    ErrorVec errors;
    Mode mode;
    SourceCode::Type type;

    ScopePtr getModuleScope() const;

    // Once a module has been typechecked, we clone its public interface into a separate arena.
    // This helps us to force TypeVar ownership into a DAG rather than a DCG.
    // Returns true if there were any free types encountered in the public interface. This
    // indicates a bug in the type checker that we want to surface.
    bool clonePublicInterface();
};

} // namespace Luau
