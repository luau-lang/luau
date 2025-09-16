// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Error.h"
#include "Luau/Linter.h"
#include "Luau/FileResolver.h"
#include "Luau/ParseOptions.h"
#include "Luau/ParseResult.h"
#include "Luau/Scope.h"
#include "Luau/TypeArena.h"
#include "Luau/DataFlowGraph.h"

#include <memory>
#include <vector>
#include <unordered_map>
#include <optional>

namespace Luau
{

using LogLuauProc = void (*)(std::string_view, std::string_view);
extern LogLuauProc logLuau;

void setLogLuau(LogLuauProc ll);
void resetLogLuauProc();

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

bool isWithinComment(const std::vector<Comment>& commentLocations, Position pos);
bool isWithinComment(const SourceModule& sourceModule, Position pos);
bool isWithinComment(const ParseResult& result, Position pos);

bool isWithinHotComment(const std::vector<HotComment>& hotComments, Position pos);
bool isWithinHotComment(const SourceModule& sourceModule, Position pos);
bool isWithinHotComment(const ParseResult& result, Position pos);

struct RequireCycle
{
    Location location;
    std::vector<ModuleName> path; // one of the paths for a require() to go all the way back to the originating module
};

struct Module
{
    ~Module();

    // TODO: Clip this when we clip FFlagLuauSolverV2
    bool checkedInNewSolver = false;

    ModuleName name;
    std::string humanReadableName;

    TypeArena interfaceTypes;
    TypeArena internalTypes;

    // Scopes and AST types refer to parse data, so we need to keep that alive
    std::shared_ptr<Allocator> allocator;
    std::shared_ptr<AstNameTable> names;
    AstStatBlock* root = nullptr;

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

    // The computed result type of a compound assignment. (eg foo += 1)
    //
    // Type checking uses this to check that the result of such an operation is
    // actually compatible with the left-side operand.
    DenseHashMap<const AstStat*, TypeId> astCompoundAssignResultTypes{nullptr};

    DenseHashMap<TypeId, std::vector<std::pair<Location, TypeId>>> upperBoundContributors{nullptr};

    // Map AST nodes to the scope they create.  Cannot be NotNull<Scope> because
    // we need a sentinel value for the map.
    DenseHashMap<const AstNode*, Scope*> astScopes{nullptr};

    // Stable references for type aliases registered in the environment
    std::vector<std::unique_ptr<TypeFun>> typeFunctionAliases;

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

    // Arenas related to the DFG must persist after the DFG no longer exists, as
    // Module objects maintain raw pointers to objects in these arenas.
    DefArena defArena;
    RefinementKeyArena keyArena;

    bool hasModuleScope() const;
    ScopePtr getModuleScope() const;

    // Once a module has been typechecked, we clone its public interface into a
    // separate arena. This helps us to force Type ownership into a DAG rather
    // than a DCG.
    void clonePublicInterface_DEPRECATED(NotNull<BuiltinTypes> builtinTypes, InternalErrorReporter& ice);

    void clonePublicInterface(NotNull<BuiltinTypes> builtinTypes, InternalErrorReporter& ice, SolverMode mode);

    bool constraintGenerationDidNotComplete = true;
};

} // namespace Luau
