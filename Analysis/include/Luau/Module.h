// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Error.h"
#include "Luau/Linter.h"
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
    ModuleName name; // Module identifier or a filename
    std::string humanReadableName;

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
bool isWithinComment(const ParseResult& result, Position pos);

struct RequireCycle
{
    Location location;
    std::vector<ModuleName> path; // one of the paths for a require() to go all the way back to the originating module
};

struct Module
{
    ~Module();

    ModuleName name;
    std::string humanReadableName;

    TypeArena interfaceTypes;
    TypeArena internalTypes;

    // Scopes and AST types refer to parse data, so we need to keep that alive
    std::shared_ptr<Allocator> allocator;
    std::shared_ptr<AstNameTable> names;

    std::vector<std::pair<Location, ScopePtr>> scopes; // never empty

    DenseHashMap<const AstExpr*, TypeId> astTypes{nullptr};
    DenseHashMap<const AstExpr*, TypePackId> astTypePacks{nullptr};
    DenseHashMap<const AstExpr*, TypeId> astExpectedTypes{nullptr};

    // For AST nodes that are function calls, this map provides the
    // unspecialized type of the function that was called. If a function call
    // resolves to a __call metamethod application, this map will point at that
    // metamethod.
    //
    // This is useful for type checking and Signature Help.
    DenseHashMap<const AstNode*, TypeId> astOriginalCallTypes{nullptr};

    // The specialization of a function that was selected.  If the function is
    // generic, those generic type parameters will be replaced with the actual
    // types that were passed.  If the function is an overload, this map will
    // point at the specific overloads that were selected.
    DenseHashMap<const AstNode*, TypeId> astOverloadResolvedTypes{nullptr};

    // Only used with for...in loops.  The computed type of the next() function
    // is kept here for type checking.
    DenseHashMap<const AstNode*, TypeId> astForInNextTypes{nullptr};

    DenseHashMap<const AstType*, TypeId> astResolvedTypes{nullptr};
    DenseHashMap<const AstTypePack*, TypePackId> astResolvedTypePacks{nullptr};

    DenseHashMap<TypeId, std::vector<std::pair<Location, TypeId>>> upperBoundContributors{nullptr};

    // Map AST nodes to the scope they create.  Cannot be NotNull<Scope> because
    // we need a sentinel value for the map.
    DenseHashMap<const AstNode*, Scope*> astScopes{nullptr};

    std::unordered_map<Name, TypeId> declaredGlobals;
    ErrorVec errors;
    LintResult lintResult;
    Mode mode;
    SourceCode::Type type;
    double checkDurationSec = 0.0;
    bool timeout = false;
    bool cancelled = false;

    TypePackId returnType = nullptr;
    std::unordered_map<Name, TypeFun> exportedTypeBindings;

    bool hasModuleScope() const;
    ScopePtr getModuleScope() const;

    // Once a module has been typechecked, we clone its public interface into a
    // separate arena. This helps us to force Type ownership into a DAG rather
    // than a DCG.
    void clonePublicInterface(NotNull<BuiltinTypes> builtinTypes, InternalErrorReporter& ice);
};

} // namespace Luau
