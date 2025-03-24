// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/FragmentAutocomplete.h"

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Autocomplete.h"
#include "Luau/Common.h"
#include "Luau/EqSatSimplification.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Parser.h"
#include "Luau/ParseOptions.h"
#include "Luau/Module.h"
#include "Luau/TimeTrace.h"
#include "Luau/UnifierSharedState.h"
#include "Luau/TypeFunction.h"
#include "Luau/DataFlowGraph.h"
#include "Luau/ConstraintGenerator.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/Frontend.h"
#include "Luau/Parser.h"
#include "Luau/ParseOptions.h"
#include "Luau/Module.h"
#include "Luau/Clone.h"
#include "AutocompleteCore.h"

LUAU_FASTINT(LuauTypeInferRecursionLimit);
LUAU_FASTINT(LuauTypeInferIterationLimit);
LUAU_FASTINT(LuauTarjanChildLimit)
LUAU_FASTFLAG(LuauAutocompleteRefactorsForIncrementalAutocomplete)

LUAU_FASTFLAGVARIABLE(LuauIncrementalAutocompleteBugfixes)
LUAU_FASTFLAGVARIABLE(LuauMixedModeDefFinderTraversesTypeOf)
LUAU_FASTFLAGVARIABLE(LuauCloneIncrementalModule)
LUAU_FASTFLAGVARIABLE(LogFragmentsFromAutocomplete)
LUAU_FASTFLAGVARIABLE(LuauBetterCursorInCommentDetection)
LUAU_FASTFLAGVARIABLE(LuauAllFreeTypesHaveScopes)
LUAU_FASTFLAGVARIABLE(LuauFragmentAcSupportsReporter)
LUAU_FASTFLAGVARIABLE(LuauPersistConstraintGenerationScopes)
LUAU_FASTFLAG(LuauModuleHoldsAstRoot)
LUAU_FASTFLAGVARIABLE(LuauCloneTypeAliasBindings)
LUAU_FASTFLAGVARIABLE(LuauCloneReturnTypePack)
LUAU_FASTFLAGVARIABLE(LuauIncrementalAutocompleteDemandBasedCloning)
LUAU_FASTFLAG(LuauUserTypeFunTypecheck)
LUAU_FASTFLAGVARIABLE(LuauFragmentNoTypeFunEval)

namespace
{
template<typename T>
void copyModuleVec(std::vector<T>& result, const std::vector<T>& input)
{
    result.insert(result.end(), input.begin(), input.end());
}

template<typename K, typename V>
void copyModuleMap(Luau::DenseHashMap<K, V>& result, const Luau::DenseHashMap<K, V>& input)
{
    for (auto [k, v] : input)
        result[k] = v;
}

} // namespace

namespace Luau
{

template<typename K, typename V>
void cloneModuleMap_DEPRECATED(TypeArena& destArena, CloneState& cloneState, const Luau::DenseHashMap<K, V>& source, Luau::DenseHashMap<K, V>& dest)
{
    for (auto [k, v] : source)
    {
        dest[k] = Luau::clone(v, destArena, cloneState);
    }
}

template<typename K, typename V>
void cloneModuleMap(
    TypeArena& destArena,
    CloneState& cloneState,
    const Luau::DenseHashMap<K, V>& source,
    Luau::DenseHashMap<K, V>& dest,
    Scope* freshScopeForFreeType
)
{
    for (auto [k, v] : source)
    {
        dest[k] = Luau::cloneIncremental(v, destArena, cloneState, freshScopeForFreeType);
    }
}

struct UsageFinder : public AstVisitor
{

    explicit UsageFinder(NotNull<DataFlowGraph> dfg)
        : dfg(dfg)
    {
        // We explicitly suggest that the usage finder propulate types for instance and enum by default
        // These are common enough types that sticking them in the environment is a good idea
        // and it lets magic functions work correctly too.
        referencedBindings.emplace_back("Instance");
        referencedBindings.emplace_back("Enum");
    }

    bool visit(AstExprConstantString* expr) override
    {
        // Populating strings in the referenced bindings is nice too, because it means that magic functions that look
        // up types by names will work correctly too.
        // Only if the actual type alias exists will we populate it over, otherwise, the strings will just get ignored
        referencedBindings.emplace_back(expr->value.data, expr->value.size);
        return true;
    }

    bool visit(AstType* node) override
    {
        return true;
    }

    bool visit(AstStatTypeAlias* alias) override
    {
        declaredAliases.insert(std::string(alias->name.value));
        return true;
    }

    bool visit(AstTypeReference* ref) override
    {
        if (std::optional<AstName> prefix = ref->prefix)
            referencedImportedBindings.emplace_back(prefix->value, ref->name.value);
        else
            referencedBindings.emplace_back(ref->name.value);

        return true;
    }

    bool visit(AstExpr* expr) override
    {
        if (auto opt = dfg->getDefOptional(expr))
            mentionedDefs.insert(opt->get());
        if (auto ref = dfg->getRefinementKey(expr))
            mentionedDefs.insert(ref->def);
        if (auto local = expr->as<AstExprLocal>())
            localBindingsReferenced.emplace_back(dfg->getDef(local), local->local);
        return true;
    }

    NotNull<DataFlowGraph> dfg;
    DenseHashSet<Name> declaredAliases{""};
    std::vector<std::pair<const Def*, AstLocal*>> localBindingsReferenced;
    DenseHashSet<const Def*> mentionedDefs{nullptr};
    std::vector<Name> referencedBindings{""};
    std::vector<std::pair<Name, Name>> referencedImportedBindings{{"", ""}};
};

// Runs the `UsageFinder` traversal on the fragment and grabs all of the types that are
// referenced in the fragment. We'll clone these and place them in the appropriate spots
// in the scope so that they are available during typechecking.
void cloneTypesFromFragment(
    CloneState& cloneState,
    const Scope* staleScope,
    const ModulePtr& staleModule,
    NotNull<TypeArena> destArena,
    NotNull<DataFlowGraph> dfg,
    AstStatBlock* program,
    Scope* destScope
)
{
    LUAU_TIMETRACE_SCOPE("Luau::cloneTypesFromFragment", "FragmentAutocomplete");

    UsageFinder f{dfg};
    program->visit(&f);
    // These are defs that have been mentioned. find the appropriate lvalue type and rvalue types and place them in the scope
    // First - any locals that have been mentioned in the fragment need to be placed in the bindings and lvalueTypes secionts.

    for (const auto& d : f.mentionedDefs)
    {
        if (std::optional<TypeId> rValueRefinement = staleScope->lookupRValueRefinementType(NotNull{d}))
        {
            destScope->rvalueRefinements[d] = Luau::cloneIncremental(*rValueRefinement, *destArena, cloneState, destScope);
        }

        if (std::optional<TypeId> lValue = staleScope->lookupUnrefinedType(NotNull{d}))
        {
            destScope->lvalueTypes[d] = Luau::cloneIncremental(*lValue, *destArena, cloneState, destScope);
        }
    }
    for (const auto& [d, loc] : f.localBindingsReferenced)
    {
        if (std::optional<std::pair<Symbol, Binding>> pair = staleScope->linearSearchForBindingPair(loc->name.value, true))
        {
            destScope->lvalueTypes[d] = Luau::cloneIncremental(pair->second.typeId, *destArena, cloneState, destScope);
            destScope->bindings[pair->first] = Luau::cloneIncremental(pair->second, *destArena, cloneState, destScope);
        }
    }

    // Second - any referenced type alias bindings need to be placed in scope so type annotation can be resolved.
    // If the actual type alias appears in the fragment on the lhs as a definition (in declaredAliases), it will be processed during typechecking
    // anyway
    for (const auto& x : f.referencedBindings)
    {
        if (f.declaredAliases.contains(x))
            continue;
        if (std::optional<TypeFun> tf = staleScope->lookupType(x))
        {
            destScope->privateTypeBindings[x] = Luau::cloneIncremental(*tf, *destArena, cloneState, destScope);
        }
    }

    // Third - any referenced imported type bindings need to be imported in
    for (const auto& [mod, name] : f.referencedImportedBindings)
    {
        if (std::optional<TypeFun> tf = staleScope->lookupImportedType(mod, name))
        {
            destScope->importedTypeBindings[mod].insert_or_assign(name, Luau::cloneIncremental(*tf, *destArena, cloneState, destScope));
        }
    }

    // Finally - clone the returnType on the staleScope. This helps avoid potential leaks of free types.
    if (staleScope->returnType)
        destScope->returnType = Luau::cloneIncremental(staleScope->returnType, *destArena, cloneState, destScope);
}


struct MixedModeIncrementalTCDefFinder : public AstVisitor
{

    bool visit(AstExprLocal* local) override
    {
        referencedLocalDefs.emplace_back(local->local, local);
        return true;
    }

    bool visit(AstTypeTypeof* node) override
    {
        // We need to traverse typeof expressions because they may refer to locals that we need
        // to populate the local environment for fragment typechecking. For example, `typeof(m)`
        // requires that we find the local/global `m` and place it in the environment.
        // The default behaviour here is to return false, and have individual visitors override
        // the specific behaviour they need.
        return FFlag::LuauMixedModeDefFinderTraversesTypeOf;
    }

    bool visit(AstStatTypeAlias* alias) override
    {
        if (FFlag::LuauCloneTypeAliasBindings)
            declaredAliases.insert(std::string(alias->name.value));
        return true;
    }

    // ast defs is just a mapping from expr -> def in general
    // will get built up by the dfg builder

    // localDefs, we need to copy over
    std::vector<std::pair<AstLocal*, AstExpr*>> referencedLocalDefs;
    DenseHashSet<Name> declaredAliases{""};
};

void cloneAndSquashScopes_DEPRECATED(
    CloneState& cloneState,
    const Scope* staleScope,
    const ModulePtr& staleModule,
    NotNull<TypeArena> destArena,
    NotNull<DataFlowGraph> dfg,
    AstStatBlock* program,
    Scope* destScope
)
{
    LUAU_TIMETRACE_SCOPE("Luau::cloneAndSquashScopes", "FragmentAutocomplete");
    std::vector<const Scope*> scopes;
    for (const Scope* current = staleScope; current; current = current->parent.get())
    {
        scopes.emplace_back(current);
    }

    // in reverse order (we need to clone the parents and override defs as we go down the list)
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it)
    {
        const Scope* curr = *it;
        // Clone the lvalue types
        for (const auto& [def, ty] : curr->lvalueTypes)
            destScope->lvalueTypes[def] = Luau::clone(ty, *destArena, cloneState);
        // Clone the rvalueRefinements
        for (const auto& [def, ty] : curr->rvalueRefinements)
            destScope->rvalueRefinements[def] = Luau::clone(ty, *destArena, cloneState);
        for (const auto& [n, m] : curr->importedTypeBindings)
        {
            std::unordered_map<Name, TypeFun> importedBindingTypes;
            for (const auto& [v, tf] : m)
                importedBindingTypes[v] = Luau::clone(tf, *destArena, cloneState);
            destScope->importedTypeBindings[n] = m;
        }

        // Finally, clone up the bindings
        for (const auto& [s, b] : curr->bindings)
        {
            destScope->bindings[s] = Luau::clone(b, *destArena, cloneState);
        }
    }

    // The above code associates defs with TypeId's in the scope
    // so that lookup to locals will succeed.
    MixedModeIncrementalTCDefFinder finder;
    program->visit(&finder);
    std::vector<std::pair<AstLocal*, AstExpr*>> locals = std::move(finder.referencedLocalDefs);
    for (auto [loc, expr] : locals)
    {
        if (std::optional<Binding> binding = staleScope->linearSearchForBinding(loc->name.value, true))
        {
            destScope->lvalueTypes[dfg->getDef(expr)] = Luau::clone(binding->typeId, *destArena, cloneState);
        }
    }
    return;
}

void cloneAndSquashScopes(
    CloneState& cloneState,
    const Scope* staleScope,
    const ModulePtr& staleModule,
    NotNull<TypeArena> destArena,
    NotNull<DataFlowGraph> dfg,
    AstStatBlock* program,
    Scope* destScope
)
{
    LUAU_TIMETRACE_SCOPE("Luau::cloneAndSquashScopes", "FragmentAutocomplete");
    std::vector<const Scope*> scopes;
    for (const Scope* current = staleScope; current; current = current->parent.get())
    {
        scopes.emplace_back(current);
    }

    MixedModeIncrementalTCDefFinder finder;

    if (FFlag::LuauCloneTypeAliasBindings)
        program->visit(&finder);
    // in reverse order (we need to clone the parents and override defs as we go down the list)
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it)
    {
        const Scope* curr = *it;
        // Clone the lvalue types
        for (const auto& [def, ty] : curr->lvalueTypes)
            destScope->lvalueTypes[def] = Luau::cloneIncremental(ty, *destArena, cloneState, destScope);
        // Clone the rvalueRefinements
        for (const auto& [def, ty] : curr->rvalueRefinements)
            destScope->rvalueRefinements[def] = Luau::cloneIncremental(ty, *destArena, cloneState, destScope);

        if (FFlag::LuauCloneTypeAliasBindings)
        {
            for (const auto& [n, tf] : curr->exportedTypeBindings)
            {
                if (!finder.declaredAliases.contains(n))
                    destScope->exportedTypeBindings[n] = Luau::cloneIncremental(tf, *destArena, cloneState, destScope);
            }

            for (const auto& [n, tf] : curr->privateTypeBindings)
            {
                if (!finder.declaredAliases.contains(n))
                    destScope->privateTypeBindings[n] = Luau::cloneIncremental(tf, *destArena, cloneState, destScope);
            }
        }
        for (const auto& [n, m] : curr->importedTypeBindings)
        {
            std::unordered_map<Name, TypeFun> importedBindingTypes;
            for (const auto& [v, tf] : m)
                importedBindingTypes[v] = Luau::cloneIncremental(tf, *destArena, cloneState, destScope);
            destScope->importedTypeBindings[n] = std::move(importedBindingTypes);
        }

        // Finally, clone up the bindings
        for (const auto& [s, b] : curr->bindings)
        {
            destScope->bindings[s] = Luau::cloneIncremental(b, *destArena, cloneState, destScope);
        }
    }

    if (!FFlag::LuauCloneTypeAliasBindings)
        program->visit(&finder);
    // The above code associates defs with TypeId's in the scope
    // so that lookup to locals will succeed.

    std::vector<std::pair<AstLocal*, AstExpr*>> locals = std::move(finder.referencedLocalDefs);
    for (auto [loc, expr] : locals)
    {
        if (std::optional<Binding> binding = staleScope->linearSearchForBinding(loc->name.value, true))
        {
            destScope->lvalueTypes[dfg->getDef(expr)] = Luau::cloneIncremental(binding->typeId, *destArena, cloneState, destScope);
        }
    }

    if (FFlag::LuauCloneReturnTypePack && destScope->returnType)
        destScope->returnType = Luau::cloneIncremental(destScope->returnType, *destArena, cloneState, destScope);

    return;
}

static FrontendModuleResolver& getModuleResolver(Frontend& frontend, std::optional<FrontendOptions> options)
{
    if (FFlag::LuauSolverV2 || !options)
        return frontend.moduleResolver;

    return options->forAutocomplete ? frontend.moduleResolverForAutocomplete : frontend.moduleResolver;
}

bool statIsBeforePos(const AstNode* stat, const Position& cursorPos)
{
    if (FFlag::LuauIncrementalAutocompleteBugfixes)
    {
        return (stat->location.begin < cursorPos);
    }

    return stat->location.begin < cursorPos && stat->location.begin.line < cursorPos.line;
}

FragmentAutocompleteAncestryResult findAncestryForFragmentParse(AstStatBlock* root, const Position& cursorPos)
{
    std::vector<AstNode*> ancestry = findAncestryAtPositionForAutocomplete(root, cursorPos);
    // Should always contain the root AstStat
    LUAU_ASSERT(ancestry.size() >= 1);
    DenseHashMap<AstName, AstLocal*> localMap{AstName()};
    std::vector<AstLocal*> localStack;
    AstStat* nearestStatement = nullptr;
    for (AstNode* node : ancestry)
    {
        if (auto block = node->as<AstStatBlock>())
        {
            for (auto stat : block->body)
            {
                if (stat->location.begin <= cursorPos)
                    nearestStatement = stat;
            }
        }
    }
    if (!nearestStatement)
        nearestStatement = ancestry[0]->asStat();
    LUAU_ASSERT(nearestStatement);

    for (AstNode* node : ancestry)
    {
        if (auto block = node->as<AstStatBlock>())
        {
            for (auto stat : block->body)
            {
                if (statIsBeforePos(stat, FFlag::LuauIncrementalAutocompleteBugfixes ? nearestStatement->location.begin : cursorPos))
                {
                    // This statement precedes the current one
                    if (auto statLoc = stat->as<AstStatLocal>())
                    {
                        for (auto v : statLoc->vars)
                        {
                            localStack.push_back(v);
                            localMap[v->name] = v;
                        }
                    }
                    else if (auto locFun = stat->as<AstStatLocalFunction>())
                    {
                        localStack.push_back(locFun->name);
                        localMap[locFun->name->name] = locFun->name;
                        if (locFun->location.contains(cursorPos))
                        {
                            for (AstLocal* loc : locFun->func->args)
                            {
                                localStack.push_back(loc);
                                localMap[loc->name] = loc;
                            }
                        }
                    }
                    else if (auto globFun = stat->as<AstStatFunction>())
                    {
                        if (globFun->location.contains(cursorPos))
                        {
                            for (AstLocal* loc : globFun->func->args)
                            {
                                localStack.push_back(loc);
                                localMap[loc->name] = loc;
                            }
                        }
                    }
                    else if (auto typeFun = stat->as<AstStatTypeFunction>(); typeFun && FFlag::LuauUserTypeFunTypecheck)
                    {
                        if (typeFun->location.contains(cursorPos))
                        {
                            for (AstLocal* loc : typeFun->body->args)
                            {
                                localStack.push_back(loc);
                                localMap[loc->name] = loc;
                            }
                        }
                    }
                }
            }
        }
        if (FFlag::LuauIncrementalAutocompleteBugfixes)
        {
            if (auto exprFunc = node->as<AstExprFunction>())
            {
                if (exprFunc->location.contains(cursorPos))
                {
                    for (auto v : exprFunc->args)
                    {
                        localStack.push_back(v);
                        localMap[v->name] = v;
                    }
                }
            }
        }
    }

    return {std::move(localMap), std::move(localStack), std::move(ancestry), std::move(nearestStatement)};
}

/**
 * Get document offsets is a function that takes a source text document as well as a start position and end position(line, column) in that
 * document and attempts to get the concrete text between those points. It returns a pair of:
 * - start offset that represents an index in the source `char*` corresponding to startPos
 * - length, that represents how many more bytes to read to get to endPos.
 * Example - your document is "foo bar baz" and getDocumentOffsets is passed (0, 4), (0, 8). This function returns the pair {3, 5}
 * which corresponds to the string " bar "
 */
std::pair<size_t, size_t> getDocumentOffsets(const std::string_view& src, const Position& startPos, const Position& endPos)
{
    size_t lineCount = 0;
    size_t colCount = 0;

    size_t docOffset = 0;
    size_t startOffset = 0;
    size_t endOffset = 0;
    bool foundStart = false;
    bool foundEnd = false;

    for (char c : src)
    {
        if (foundStart && foundEnd)
            break;

        if (startPos.line == lineCount && startPos.column == colCount)
        {
            foundStart = true;
            startOffset = docOffset;
        }

        if (endPos.line == lineCount && endPos.column == colCount)
        {
            endOffset = docOffset;
            while (endOffset < src.size() && src[endOffset] != '\n')
                endOffset++;
            foundEnd = true;
        }

        // We put a cursor position that extends beyond the extents of the current line
        if (foundStart && !foundEnd && (lineCount > endPos.line))
        {
            foundEnd = true;
            endOffset = docOffset - 1;
        }

        if (c == '\n')
        {
            lineCount++;
            colCount = 0;
        }
        else
        {
            colCount++;
        }
        docOffset++;
    }

    if (foundStart && !foundEnd)
        endOffset = src.length();

    size_t min = std::min(startOffset, endOffset);
    size_t len = std::max(startOffset, endOffset) - min;
    return {min, len};
}

ScopePtr findClosestScope(const ModulePtr& module, const AstStat* nearestStatement)
{
    LUAU_ASSERT(module->hasModuleScope());

    ScopePtr closest = module->getModuleScope();

    // find the scope the nearest statement belonged to.
    for (auto [loc, sc] : module->scopes)
    {
        if (loc.encloses(nearestStatement->location) && closest->location.begin <= loc.begin)
            closest = sc;
    }

    return closest;
}

std::optional<FragmentParseResult> parseFragment(
    AstStatBlock* root,
    AstNameTable* names,
    std::string_view src,
    const Position& cursorPos,
    std::optional<Position> fragmentEndPosition
)
{
    FragmentAutocompleteAncestryResult result = findAncestryForFragmentParse(root, cursorPos);
    AstStat* nearestStatement = result.nearestStatement;

    const Location& rootSpan = root->location;
    // Did we append vs did we insert inline
    bool appended = cursorPos >= rootSpan.end;
    // statement spans multiple lines
    bool multiline = nearestStatement->location.begin.line != nearestStatement->location.end.line;

    const Position endPos = fragmentEndPosition.value_or(cursorPos);

    // We start by re-parsing everything (we'll refine this as we go)
    Position startPos = root->location.begin;

    // If we added to the end of the sourceModule, use the end of the nearest location
    if (appended && multiline)
        startPos = nearestStatement->location.end;
    // Statement spans one line && cursorPos is either on the same line or after
    else if (!multiline && cursorPos.line >= nearestStatement->location.end.line)
        startPos = nearestStatement->location.begin;
    else if (multiline && nearestStatement->location.end.line < cursorPos.line)
        startPos = nearestStatement->location.end;
    else
        startPos = nearestStatement->location.begin;

    auto [offsetStart, parseLength] = getDocumentOffsets(src, startPos, endPos);
    const char* srcStart = src.data() + offsetStart;
    std::string_view dbg = src.substr(offsetStart, parseLength);
    FragmentParseResult fragmentResult;
    fragmentResult.fragmentToParse = std::string(dbg.data(), parseLength);
    // For the duration of the incremental parse, we want to allow the name table to re-use duplicate names
    if (FFlag::LogFragmentsFromAutocomplete)
        logLuau(dbg);

    ParseOptions opts;
    opts.allowDeclarationSyntax = false;
    opts.captureComments = true;
    opts.parseFragment = FragmentParseResumeSettings{std::move(result.localMap), std::move(result.localStack), startPos};
    ParseResult p = Luau::Parser::parse(srcStart, parseLength, *names, *fragmentResult.alloc, opts);
    // This means we threw a ParseError and we should decline to offer autocomplete here.
    if (p.root == nullptr)
        return std::nullopt;

    std::vector<AstNode*> fabricatedAncestry = std::move(result.ancestry);

    // Get the ancestry for the fragment at the offset cursor position.
    // Consumers have the option to request with fragment end position, so we cannot just use the end position of our parse result as the
    // cursor position. Instead, use the cursor position calculated as an offset from our start position.
    std::vector<AstNode*> fragmentAncestry = findAncestryAtPositionForAutocomplete(p.root, cursorPos);
    fabricatedAncestry.insert(fabricatedAncestry.end(), fragmentAncestry.begin(), fragmentAncestry.end());
    if (nearestStatement == nullptr)
        nearestStatement = p.root;
    fragmentResult.root = std::move(p.root);
    fragmentResult.ancestry = std::move(fabricatedAncestry);
    fragmentResult.nearestStatement = nearestStatement;
    fragmentResult.commentLocations = std::move(p.commentLocations);
    return fragmentResult;
}

ModulePtr cloneModule_DEPRECATED(CloneState& cloneState, const ModulePtr& source, std::unique_ptr<Allocator> alloc)
{
    LUAU_TIMETRACE_SCOPE("Luau::cloneModule", "FragmentAutocomplete");
    freeze(source->internalTypes);
    freeze(source->interfaceTypes);
    ModulePtr incremental = std::make_shared<Module>();
    incremental->name = source->name;
    incremental->humanReadableName = source->humanReadableName;
    incremental->allocator = std::move(alloc);
    //  Clone types
    cloneModuleMap_DEPRECATED(incremental->internalTypes, cloneState, source->astTypes, incremental->astTypes);
    cloneModuleMap_DEPRECATED(incremental->internalTypes, cloneState, source->astTypePacks, incremental->astTypePacks);
    cloneModuleMap_DEPRECATED(incremental->internalTypes, cloneState, source->astExpectedTypes, incremental->astExpectedTypes);

    cloneModuleMap_DEPRECATED(incremental->internalTypes, cloneState, source->astOverloadResolvedTypes, incremental->astOverloadResolvedTypes);

    cloneModuleMap_DEPRECATED(incremental->internalTypes, cloneState, source->astForInNextTypes, incremental->astForInNextTypes);

    copyModuleMap(incremental->astScopes, source->astScopes);

    return incremental;
}

ModulePtr cloneModule(CloneState& cloneState, const ModulePtr& source, std::unique_ptr<Allocator> alloc, Scope* freeTypeFreshScope)
{
    LUAU_TIMETRACE_SCOPE("Luau::cloneModule", "FragmentAutocomplete");
    freeze(source->internalTypes);
    freeze(source->interfaceTypes);
    ModulePtr incremental = std::make_shared<Module>();
    incremental->name = source->name;
    incremental->humanReadableName = source->humanReadableName;
    incremental->allocator = std::move(alloc);
    //  Clone types
    cloneModuleMap(incremental->internalTypes, cloneState, source->astTypes, incremental->astTypes, freeTypeFreshScope);
    cloneModuleMap(incremental->internalTypes, cloneState, source->astTypePacks, incremental->astTypePacks, freeTypeFreshScope);
    cloneModuleMap(incremental->internalTypes, cloneState, source->astExpectedTypes, incremental->astExpectedTypes, freeTypeFreshScope);

    cloneModuleMap(
        incremental->internalTypes, cloneState, source->astOverloadResolvedTypes, incremental->astOverloadResolvedTypes, freeTypeFreshScope
    );

    cloneModuleMap(incremental->internalTypes, cloneState, source->astForInNextTypes, incremental->astForInNextTypes, freeTypeFreshScope);

    copyModuleMap(incremental->astScopes, source->astScopes);

    return incremental;
}

ModulePtr copyModule(const ModulePtr& result, std::unique_ptr<Allocator> alloc)
{
    ModulePtr incrementalModule = std::make_shared<Module>();
    incrementalModule->name = result->name;
    incrementalModule->humanReadableName = "Incremental$" + result->humanReadableName;
    incrementalModule->internalTypes.owningModule = incrementalModule.get();
    incrementalModule->interfaceTypes.owningModule = incrementalModule.get();
    incrementalModule->allocator = std::move(alloc);
    // Don't need to keep this alive (it's already on the source module)
    copyModuleVec(incrementalModule->scopes, result->scopes);
    copyModuleMap(incrementalModule->astTypes, result->astTypes);
    copyModuleMap(incrementalModule->astTypePacks, result->astTypePacks);
    copyModuleMap(incrementalModule->astExpectedTypes, result->astExpectedTypes);
    // Don't need to clone astOriginalCallTypes
    copyModuleMap(incrementalModule->astOverloadResolvedTypes, result->astOverloadResolvedTypes);
    // Don't need to clone astForInNextTypes
    copyModuleMap(incrementalModule->astForInNextTypes, result->astForInNextTypes);
    // Don't need to clone astResolvedTypes
    // Don't need to clone astResolvedTypePacks
    // Don't need to clone upperBoundContributors
    copyModuleMap(incrementalModule->astScopes, result->astScopes);
    // Don't need to clone declared Globals;
    return incrementalModule;
}

void mixedModeCompatibility(
    const ScopePtr& bottomScopeStale,
    const ScopePtr& myFakeScope,
    const ModulePtr& stale,
    NotNull<DataFlowGraph> dfg,
    AstStatBlock* program
)
{
    // This code does the following
    // traverse program
    // look for ast refs for locals
    // ask for the corresponding defId from dfg
    // given that defId, and that expression, in the incremental module, map lvalue types from defID to

    MixedModeIncrementalTCDefFinder finder;
    program->visit(&finder);
    std::vector<std::pair<AstLocal*, AstExpr*>> locals = std::move(finder.referencedLocalDefs);
    for (auto [loc, expr] : locals)
    {
        if (std::optional<Binding> binding = bottomScopeStale->linearSearchForBinding(loc->name.value, true))
        {
            myFakeScope->lvalueTypes[dfg->getDef(expr)] = binding->typeId;
        }
    }
}

static void reportWaypoint(IFragmentAutocompleteReporter* reporter, FragmentAutocompleteWaypoint type)
{
    if (!FFlag::LuauFragmentAcSupportsReporter || !reporter)
        return;

    reporter->reportWaypoint(type);
}

static void reportFragmentString(IFragmentAutocompleteReporter* reporter, std::string_view fragment)
{
    if (!FFlag::LuauFragmentAcSupportsReporter || !reporter)
        return;

    reporter->reportFragmentString(fragment);
}

FragmentTypeCheckResult typecheckFragmentHelper_DEPRECATED(
    Frontend& frontend,
    AstStatBlock* root,
    const ModulePtr& stale,
    const ScopePtr& closestScope,
    const Position& cursorPos,
    std::unique_ptr<Allocator> astAllocator,
    const FrontendOptions& opts,
    IFragmentAutocompleteReporter* reporter
)
{
    LUAU_TIMETRACE_SCOPE("Luau::typecheckFragment_", "FragmentAutocomplete");

    freeze(stale->internalTypes);
    freeze(stale->interfaceTypes);
    reportWaypoint(reporter, FragmentAutocompleteWaypoint::CloneModuleStart);
    CloneState cloneState{frontend.builtinTypes};
    std::shared_ptr<Scope> freshChildOfNearestScope = std::make_shared<Scope>(closestScope);
    ModulePtr incrementalModule = nullptr;
    if (FFlag::LuauAllFreeTypesHaveScopes)
        incrementalModule = cloneModule(cloneState, stale, std::move(astAllocator), freshChildOfNearestScope.get());
    else if (FFlag::LuauCloneIncrementalModule)
        incrementalModule = cloneModule_DEPRECATED(cloneState, stale, std::move(astAllocator));
    else
        incrementalModule = copyModule(stale, std::move(astAllocator));

    reportWaypoint(reporter, FragmentAutocompleteWaypoint::CloneModuleEnd);
    incrementalModule->checkedInNewSolver = true;
    unfreeze(incrementalModule->internalTypes);
    unfreeze(incrementalModule->interfaceTypes);

    /// Setup typecheck limits
    TypeCheckLimits limits;
    if (opts.moduleTimeLimitSec)
        limits.finishTime = TimeTrace::getClock() + *opts.moduleTimeLimitSec;
    else
        limits.finishTime = std::nullopt;
    limits.cancellationToken = opts.cancellationToken;

    /// Icehandler
    NotNull<InternalErrorReporter> iceHandler{&frontend.iceHandler};
    /// Make the shared state for the unifier (recursion + iteration limits)
    UnifierSharedState unifierState{iceHandler};
    unifierState.counters.recursionLimit = FInt::LuauTypeInferRecursionLimit;
    unifierState.counters.iterationLimit = limits.unifierIterationLimit.value_or(FInt::LuauTypeInferIterationLimit);

    /// Initialize the normalizer
    Normalizer normalizer{&incrementalModule->internalTypes, frontend.builtinTypes, NotNull{&unifierState}};

    /// User defined type functions runtime
    TypeFunctionRuntime typeFunctionRuntime(iceHandler, NotNull{&limits});

    /// Create a DataFlowGraph just for the surrounding context
    DataFlowGraph dfg = DataFlowGraphBuilder::build(root, NotNull{&incrementalModule->defArena}, NotNull{&incrementalModule->keyArena}, iceHandler);
    reportWaypoint(reporter, FragmentAutocompleteWaypoint::DfgBuildEnd);

    SimplifierPtr simplifier = newSimplifier(NotNull{&incrementalModule->internalTypes}, frontend.builtinTypes);

    FrontendModuleResolver& resolver = getModuleResolver(frontend, opts);

    /// Contraint Generator
    ConstraintGenerator cg{
        incrementalModule,
        NotNull{&normalizer},
        NotNull{simplifier.get()},
        NotNull{&typeFunctionRuntime},
        NotNull{&resolver},
        frontend.builtinTypes,
        iceHandler,
        stale->getModuleScope(),
        frontend.globals.globalTypeFunctionScope,
        nullptr,
        nullptr,
        NotNull{&dfg},
        {}
    };

    reportWaypoint(reporter, FragmentAutocompleteWaypoint::CloneAndSquashScopeStart);
    if (FFlag::LuauCloneIncrementalModule)
    {
        incrementalModule->scopes.emplace_back(root->location, freshChildOfNearestScope);
        cg.rootScope = freshChildOfNearestScope.get();

        if (FFlag::LuauAllFreeTypesHaveScopes)
            cloneAndSquashScopes(
                cloneState, closestScope.get(), stale, NotNull{&incrementalModule->internalTypes}, NotNull{&dfg}, root, freshChildOfNearestScope.get()
            );
        else
            cloneAndSquashScopes_DEPRECATED(
                cloneState, closestScope.get(), stale, NotNull{&incrementalModule->internalTypes}, NotNull{&dfg}, root, freshChildOfNearestScope.get()
            );

        reportWaypoint(reporter, FragmentAutocompleteWaypoint::CloneAndSquashScopeEnd);
        cg.visitFragmentRoot(freshChildOfNearestScope, root);

        if (FFlag::LuauPersistConstraintGenerationScopes)
        {
            for (auto p : cg.scopes)
                incrementalModule->scopes.emplace_back(std::move(p));
        }
    }
    else
    {
        // Any additions to the scope must occur in a fresh scope
        cg.rootScope = stale->getModuleScope().get();
        incrementalModule->scopes.emplace_back(root->location, freshChildOfNearestScope);
        mixedModeCompatibility(closestScope, freshChildOfNearestScope, stale, NotNull{&dfg}, root);
        // closest Scope -> children = { ...., freshChildOfNearestScope}
        // We need to trim nearestChild from the scope hierarchy
        closestScope->children.emplace_back(freshChildOfNearestScope.get());
        cg.visitFragmentRoot(freshChildOfNearestScope, root);
        // Trim nearestChild from the closestScope
        Scope* back = closestScope->children.back().get();
        LUAU_ASSERT(back == freshChildOfNearestScope.get());
        closestScope->children.pop_back();
    }
    reportWaypoint(reporter, FragmentAutocompleteWaypoint::ConstraintSolverStart);

    if (FFlag::LuauAllFreeTypesHaveScopes)
    {
        if (Scope* sc = freshChildOfNearestScope.get())
        {
            if (!sc->interiorFreeTypes.has_value())
                sc->interiorFreeTypes.emplace();
        }
    }

    /// Initialize the constraint solver and run it
    ConstraintSolver cs{
        NotNull{&normalizer},
        NotNull{simplifier.get()},
        NotNull{&typeFunctionRuntime},
        NotNull(cg.rootScope),
        borrowConstraints(cg.constraints),
        NotNull{&cg.scopeToFunction},
        incrementalModule->name,
        NotNull{&resolver},
        {},
        nullptr,
        NotNull{&dfg},
        limits
    };

    try
    {
        cs.run();
    }
    catch (const TimeLimitError&)
    {
        stale->timeout = true;
    }
    catch (const UserCancelError&)
    {
        stale->cancelled = true;
    }

    reportWaypoint(reporter, FragmentAutocompleteWaypoint::ConstraintSolverEnd);

    // In frontend we would forbid internal types
    // because this is just for autocomplete, we don't actually care
    // We also don't even need to typecheck - just synthesize types as best as we can

    freeze(incrementalModule->internalTypes);
    freeze(incrementalModule->interfaceTypes);
    return {std::move(incrementalModule), std::move(freshChildOfNearestScope)};
}

FragmentTypeCheckResult typecheckFragment_(
    Frontend& frontend,
    AstStatBlock* root,
    const ModulePtr& stale,
    const ScopePtr& closestScope,
    const Position& cursorPos,
    std::unique_ptr<Allocator> astAllocator,
    const FrontendOptions& opts,
    IFragmentAutocompleteReporter* reporter
)
{
    LUAU_TIMETRACE_SCOPE("Luau::typecheckFragment_", "FragmentAutocomplete");

    freeze(stale->internalTypes);
    freeze(stale->interfaceTypes);
    ModulePtr incrementalModule = std::make_shared<Module>();
    incrementalModule->name = stale->name;
    incrementalModule->humanReadableName = "Incremental$" + stale->humanReadableName;
    incrementalModule->internalTypes.owningModule = incrementalModule.get();
    incrementalModule->interfaceTypes.owningModule = incrementalModule.get();
    incrementalModule->allocator = std::move(astAllocator);
    incrementalModule->checkedInNewSolver = true;
    unfreeze(incrementalModule->internalTypes);
    unfreeze(incrementalModule->interfaceTypes);

    /// Setup typecheck limits
    TypeCheckLimits limits;
    if (opts.moduleTimeLimitSec)
        limits.finishTime = TimeTrace::getClock() + *opts.moduleTimeLimitSec;
    else
        limits.finishTime = std::nullopt;
    limits.cancellationToken = opts.cancellationToken;

    /// Icehandler
    NotNull<InternalErrorReporter> iceHandler{&frontend.iceHandler};
    /// Make the shared state for the unifier (recursion + iteration limits)
    UnifierSharedState unifierState{iceHandler};
    unifierState.counters.recursionLimit = FInt::LuauTypeInferRecursionLimit;
    unifierState.counters.iterationLimit = limits.unifierIterationLimit.value_or(FInt::LuauTypeInferIterationLimit);

    /// Initialize the normalizer
    Normalizer normalizer{&incrementalModule->internalTypes, frontend.builtinTypes, NotNull{&unifierState}};

    /// User defined type functions runtime
    TypeFunctionRuntime typeFunctionRuntime(iceHandler, NotNull{&limits});

    if (FFlag::LuauFragmentNoTypeFunEval || FFlag::LuauUserTypeFunTypecheck)
        typeFunctionRuntime.allowEvaluation = false;

    /// Create a DataFlowGraph just for the surrounding context
    DataFlowGraph dfg = DataFlowGraphBuilder::build(root, NotNull{&incrementalModule->defArena}, NotNull{&incrementalModule->keyArena}, iceHandler);
    reportWaypoint(reporter, FragmentAutocompleteWaypoint::DfgBuildEnd);

    SimplifierPtr simplifier = newSimplifier(NotNull{&incrementalModule->internalTypes}, frontend.builtinTypes);

    FrontendModuleResolver& resolver = getModuleResolver(frontend, opts);

    /// Contraint Generator
    ConstraintGenerator cg{
        incrementalModule,
        NotNull{&normalizer},
        NotNull{simplifier.get()},
        NotNull{&typeFunctionRuntime},
        NotNull{&resolver},
        frontend.builtinTypes,
        iceHandler,
        stale->getModuleScope(),
        frontend.globals.globalTypeFunctionScope,
        nullptr,
        nullptr,
        NotNull{&dfg},
        {}
    };

    CloneState cloneState{frontend.builtinTypes};
    std::shared_ptr<Scope> freshChildOfNearestScope = std::make_shared<Scope>(nullptr);
    incrementalModule->scopes.emplace_back(root->location, freshChildOfNearestScope);
    freshChildOfNearestScope->interiorFreeTypes.emplace();
    cg.rootScope = freshChildOfNearestScope.get();

    if (FFlag::LuauUserTypeFunTypecheck)
    {
        // Create module-local scope for the type function environment
        ScopePtr localTypeFunctionScope = std::make_shared<Scope>(cg.typeFunctionScope);
        localTypeFunctionScope->location = root->location;
        cg.typeFunctionRuntime->rootScope = localTypeFunctionScope;
    }

    reportWaypoint(reporter, FragmentAutocompleteWaypoint::CloneAndSquashScopeStart);
    cloneTypesFromFragment(
        cloneState, closestScope.get(), stale, NotNull{&incrementalModule->internalTypes}, NotNull{&dfg}, root, freshChildOfNearestScope.get()
    );
    reportWaypoint(reporter, FragmentAutocompleteWaypoint::CloneAndSquashScopeEnd);

    cg.visitFragmentRoot(freshChildOfNearestScope, root);

    for (auto p : cg.scopes)
        incrementalModule->scopes.emplace_back(std::move(p));


    reportWaypoint(reporter, FragmentAutocompleteWaypoint::ConstraintSolverStart);

    /// Initialize the constraint solver and run it
    ConstraintSolver cs{
        NotNull{&normalizer},
        NotNull{simplifier.get()},
        NotNull{&typeFunctionRuntime},
        NotNull(cg.rootScope),
        borrowConstraints(cg.constraints),
        NotNull{&cg.scopeToFunction},
        incrementalModule->name,
        NotNull{&resolver},
        {},
        nullptr,
        NotNull{&dfg},
        limits
    };

    try
    {
        cs.run();
    }
    catch (const TimeLimitError&)
    {
        stale->timeout = true;
    }
    catch (const UserCancelError&)
    {
        stale->cancelled = true;
    }

    reportWaypoint(reporter, FragmentAutocompleteWaypoint::ConstraintSolverEnd);

    // In frontend we would forbid internal types
    // because this is just for autocomplete, we don't actually care
    // We also don't even need to typecheck - just synthesize types as best as we can

    freeze(incrementalModule->internalTypes);
    freeze(incrementalModule->interfaceTypes);
    freshChildOfNearestScope->parent = closestScope;
    return {std::move(incrementalModule), std::move(freshChildOfNearestScope)};
}


std::pair<FragmentTypeCheckStatus, FragmentTypeCheckResult> typecheckFragment(
    Frontend& frontend,
    const ModuleName& moduleName,
    const Position& cursorPos,
    std::optional<FrontendOptions> opts,
    std::string_view src,
    std::optional<Position> fragmentEndPosition,
    IFragmentAutocompleteReporter* reporter
)
{
    LUAU_TIMETRACE_SCOPE("Luau::typecheckFragment", "FragmentAutocomplete");
    LUAU_TIMETRACE_ARGUMENT("name", moduleName.c_str());

    if (!frontend.allModuleDependenciesValid(moduleName, opts && opts->forAutocomplete))
        return {FragmentTypeCheckStatus::SkipAutocomplete, {}};

    FrontendModuleResolver& resolver = getModuleResolver(frontend, opts);
    ModulePtr module = resolver.getModule(moduleName);
    if (!module)
    {
        LUAU_ASSERT(!"Expected Module for fragment typecheck");
        return {};
    }

    std::optional<FragmentParseResult> tryParse;
    if (FFlag::LuauModuleHoldsAstRoot)
    {
        tryParse = parseFragment(module->root, module->names.get(), src, cursorPos, fragmentEndPosition);
    }
    else
    {
        const SourceModule* sourceModule = frontend.getSourceModule(moduleName);
        if (!sourceModule)
        {
            LUAU_ASSERT(!"Expected Source Module for fragment typecheck");
            return {};
        }

        if (FFlag::LuauIncrementalAutocompleteBugfixes)
        {
            if (sourceModule->allocator.get() != module->allocator.get())
            {
                return {FragmentTypeCheckStatus::SkipAutocomplete, {}};
            }
        }

        tryParse = parseFragment(sourceModule->root, sourceModule->names.get(), src, cursorPos, fragmentEndPosition);
        reportWaypoint(reporter, FragmentAutocompleteWaypoint::ParseFragmentEnd);
    }

    if (!tryParse)
        return {FragmentTypeCheckStatus::SkipAutocomplete, {}};

    FragmentParseResult& parseResult = *tryParse;

    if (isWithinComment(parseResult.commentLocations, fragmentEndPosition.value_or(cursorPos)))
        return {FragmentTypeCheckStatus::SkipAutocomplete, {}};

    FrontendOptions frontendOptions = opts.value_or(frontend.options);
    const ScopePtr& closestScope = findClosestScope(module, parseResult.nearestStatement);
    FragmentTypeCheckResult result =
        FFlag::LuauIncrementalAutocompleteDemandBasedCloning
            ? typecheckFragment_(frontend, parseResult.root, module, closestScope, cursorPos, std::move(parseResult.alloc), frontendOptions, reporter)
            : typecheckFragmentHelper_DEPRECATED(
                  frontend, parseResult.root, module, closestScope, cursorPos, std::move(parseResult.alloc), frontendOptions, reporter
              );
    result.ancestry = std::move(parseResult.ancestry);
    reportFragmentString(reporter, tryParse->fragmentToParse);
    return {FragmentTypeCheckStatus::Success, result};
}

FragmentAutocompleteStatusResult tryFragmentAutocomplete(
    Frontend& frontend,
    const ModuleName& moduleName,
    Position cursorPosition,
    FragmentContext context,
    StringCompletionCallback stringCompletionCB
)
{
    if (FFlag::LuauBetterCursorInCommentDetection)
    {
        if (isWithinComment(context.freshParse.commentLocations, cursorPosition))
            return {FragmentAutocompleteStatus::Success, std::nullopt};
    }
    // TODO: we should calculate fragmentEnd position here, by using context.newAstRoot and cursorPosition
    try
    {
        Luau::FragmentAutocompleteResult fragmentAutocomplete = Luau::fragmentAutocomplete(
            frontend,
            context.newSrc,
            moduleName,
            cursorPosition,
            context.opts,
            std::move(stringCompletionCB),
            context.DEPRECATED_fragmentEndPosition,
            FFlag::LuauFragmentAcSupportsReporter ? context.reporter : nullptr
        );
        return {FragmentAutocompleteStatus::Success, std::move(fragmentAutocomplete)};
    }
    catch (const Luau::InternalCompilerError& e)
    {
        if (FFlag::LogFragmentsFromAutocomplete)
            logLuau(e.what());
        return {FragmentAutocompleteStatus::InternalIce, std::nullopt};
    }
}

FragmentAutocompleteResult fragmentAutocomplete(
    Frontend& frontend,
    std::string_view src,
    const ModuleName& moduleName,
    Position cursorPosition,
    std::optional<FrontendOptions> opts,
    StringCompletionCallback callback,
    std::optional<Position> fragmentEndPosition,
    IFragmentAutocompleteReporter* reporter
)
{
    LUAU_ASSERT(FFlag::LuauAutocompleteRefactorsForIncrementalAutocomplete);
    LUAU_TIMETRACE_SCOPE("Luau::fragmentAutocomplete", "FragmentAutocomplete");
    LUAU_TIMETRACE_ARGUMENT("name", moduleName.c_str());

    if (!FFlag::LuauModuleHoldsAstRoot)
    {
        const SourceModule* sourceModule = frontend.getSourceModule(moduleName);
        if (!sourceModule)
        {
            LUAU_ASSERT(!"Expected Source Module for fragment typecheck");
            return {};
        }

        // If the cursor is within a comment in the stale source module we should avoid providing a recommendation
        if (isWithinComment(*sourceModule, fragmentEndPosition.value_or(cursorPosition)))
            return {};
    }

    auto [tcStatus, tcResult] = typecheckFragment(frontend, moduleName, cursorPosition, opts, src, fragmentEndPosition, reporter);
    if (tcStatus == FragmentTypeCheckStatus::SkipAutocomplete)
        return {};

    reportWaypoint(reporter, FragmentAutocompleteWaypoint::TypecheckFragmentEnd);
    auto globalScope = (opts && opts->forAutocomplete) ? frontend.globalsForAutocomplete.globalScope.get() : frontend.globals.globalScope.get();
    if (FFlag::LogFragmentsFromAutocomplete)
        logLuau(src);
    TypeArena arenaForFragmentAutocomplete;
    auto result = Luau::autocomplete_(
        tcResult.incrementalModule,
        frontend.builtinTypes,
        &arenaForFragmentAutocomplete,
        tcResult.ancestry,
        globalScope,
        tcResult.freshScope,
        cursorPosition,
        frontend.fileResolver,
        callback
    );

    reportWaypoint(reporter, FragmentAutocompleteWaypoint::AutocompleteEnd);
    return {std::move(tcResult.incrementalModule), tcResult.freshScope.get(), std::move(arenaForFragmentAutocomplete), std::move(result)};
}

} // namespace Luau
