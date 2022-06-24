// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Error.h"
#include "Luau/FileResolver.h"
#include "Luau/ParseOptions.h"
#include "Luau/ParseResult.h"
#include "Luau/Scope.h"
#include "Luau/TypeArena.h"

#include <memory>
#include <vector>
#include <unordered_map>
#include <optional>

namespace Luau
{

struct Module;

using ScopePtr = std::shared_ptr<struct Scope>;
using ModulePtr = std::shared_ptr<Module>;

class AstType;
class AstTypePack;

/// Root of the AST of a parsed source file
struct SourceModule
{
    ModuleName name; // DataModel path if possible.  Filename if not.
    SourceCode::Type type = SourceCode::None;
    std::optional<std::string> environmentName;
    bool cyclic = false;

    std::shared_ptr<Allocator> allocator;
    std::shared_ptr<AstNameTable> names;
    std::vector<ParseError> parseErrors;

    AstStatBlock* root = nullptr;
    std::optional<Mode> mode;

    std::vector<HotComment> hotcomments;
    std::vector<Comment> commentLocations;

    SourceModule()
        : allocator(new Allocator)
        , names(new AstNameTable(*allocator))
    {
    }
};

bool isWithinComment(const SourceModule& sourceModule, Position pos);

struct RequireCycle
{
    Location location;
    std::vector<ModuleName> path; // one of the paths for a require() to go all the way back to the originating module
};

struct Module
{
    ~Module();

    TypeArena interfaceTypes;
    TypeArena internalTypes;

    // Scopes and AST types refer to parse data, so we need to keep that alive
    std::shared_ptr<Allocator> allocator;
    std::shared_ptr<AstNameTable> names;

    std::vector<std::pair<Location, ScopePtr>> scopes; // never empty
    std::vector<std::pair<Location, std::unique_ptr<Scope2>>> scope2s; // never empty

    DenseHashMap<const AstExpr*, TypeId> astTypes{nullptr};
    DenseHashMap<const AstExpr*, TypePackId> astTypePacks{nullptr};
    DenseHashMap<const AstExpr*, TypeId> astExpectedTypes{nullptr};
    DenseHashMap<const AstExpr*, TypeId> astOriginalCallTypes{nullptr};
    DenseHashMap<const AstExpr*, TypeId> astOverloadResolvedTypes{nullptr};
    DenseHashMap<const AstType*, TypeId> astResolvedTypes{nullptr};
    DenseHashMap<const AstTypePack*, TypePackId> astResolvedTypePacks{nullptr};

    std::unordered_map<Name, TypeId> declaredGlobals;
    ErrorVec errors;
    Mode mode;
    SourceCode::Type type;
    bool timeout = false;

    ScopePtr getModuleScope() const;
    Scope2* getModuleScope2() const;

    // Once a module has been typechecked, we clone its public interface into a separate arena.
    // This helps us to force TypeVar ownership into a DAG rather than a DCG.
    void clonePublicInterface(InternalErrorReporter& ice);
};

} // namespace Luau
