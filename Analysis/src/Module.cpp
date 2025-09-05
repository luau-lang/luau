// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Module.h"

#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/ConstraintGenerator.h"
#include "Luau/Normalize.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/Type.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/VisitType.h"

#include <algorithm>

LUAU_FASTFLAG(LuauSolverV2);
LUAU_FASTFLAG(LuauUseWorkspacePropToChooseSolver)
LUAU_FASTFLAG(LuauLimitDynamicConstraintSolving3)
LUAU_FASTFLAGVARIABLE(LuauEmplaceNotPushBack)
LUAU_FASTFLAG(LuauSuggestHotComments)

namespace Luau
{

static void defaultLogLuau(std::string_view context, std::string_view input)
{
    // The default is to do nothing because we don't want to mess with
    // the xml parsing done by the dcr script.
}

Luau::LogLuauProc logLuau = &defaultLogLuau;

void setLogLuau(LogLuauProc ll)
{
    logLuau = ll;
}

void resetLogLuauProc()
{
    logLuau = &defaultLogLuau;
}

static bool contains(Position pos, Comment comment)
{
    if (comment.location.contains(pos))
        return true;
    else if (comment.type == Lexeme::BrokenComment && comment.location.begin <= pos) // Broken comments are broken specifically because they don't
                                                                                     // have an end
        return true;
    // comments actually span the whole line - in incremental mode, we could pass a cursor outside of the current parsed comment range span, but it
    // would still be 'within' the comment So, the cursor must be on the same line and the comment itself must come strictly after the `begin`
    else if (comment.type == Lexeme::Comment && comment.location.end.line == pos.line && comment.location.begin <= pos)
        return true;
    else
        return false;
}

bool isWithinComment(const std::vector<Comment>& commentLocations, Position pos)
{
    auto iter = std::lower_bound(
        commentLocations.begin(),
        commentLocations.end(),
        Comment{Lexeme::Comment, Location{pos, pos}},
        [](const Comment& a, const Comment& b)
        {
            if (a.type == Lexeme::Comment)
                return a.location.end.line < b.location.end.line;
            return a.location.end < b.location.end;
        }
    );

    if (iter == commentLocations.end())
        return false;

    if (contains(pos, *iter))
        return true;

    // Due to the nature of std::lower_bound, it is possible that iter points at a comment that ends
    // at pos.  We'll try the next comment, if it exists.
    ++iter;
    if (iter == commentLocations.end())
        return false;

    return contains(pos, *iter);
}

bool isWithinComment(const SourceModule& sourceModule, Position pos)
{
    return isWithinComment(sourceModule.commentLocations, pos);
}

bool isWithinComment(const ParseResult& result, Position pos)
{
    return isWithinComment(result.commentLocations, pos);
}

bool isWithinHotComment(const std::vector<HotComment>& hotComments, Position pos)
{
    for (const HotComment& hotComment : hotComments)
    {
        if (hotComment.location.containsClosed(pos))
            return true;
    }

    return false;
}

bool isWithinHotComment(const SourceModule& sourceModule, Position pos)
{
    return isWithinHotComment(sourceModule.hotcomments, pos);
}

bool isWithinHotComment(const ParseResult& result, Position pos)
{
    return isWithinHotComment(result.hotcomments, pos);
}

struct ClonePublicInterface : Substitution
{
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<Module> module;
    // NOTE: This can be made non-optional after
    // LuauUseWorkspacePropToChooseSolver is clipped.
    std::optional<SolverMode> solverMode{std::nullopt};
    bool internalTypeEscaped = false;

    ClonePublicInterface(const TxnLog* log, NotNull<BuiltinTypes> builtinTypes, Module* module)
        : Substitution(log, &module->interfaceTypes)
        , builtinTypes(builtinTypes)
        , module(module)
    {
        LUAU_ASSERT(module);
    }

    ClonePublicInterface(const TxnLog* log, NotNull<BuiltinTypes> builtinTypes, Module* module, SolverMode solverMode)
        : Substitution(log, &module->interfaceTypes)
        , builtinTypes(builtinTypes)
        , module(module)
        , solverMode(solverMode)
    {
        LUAU_ASSERT(module);
    }

    bool isNewSolver() const
    {
        return FFlag::LuauSolverV2 || (FFlag::LuauUseWorkspacePropToChooseSolver && solverMode == SolverMode::New);
    }

    bool isDirty(TypeId ty) override
    {
        if (ty->owningArena == &module->internalTypes)
            return true;

        if (const FunctionType* ftv = get<FunctionType>(ty))
            return ftv->level.level != 0;
        if (const TableType* ttv = get<TableType>(ty))
            return ttv->level.level != 0;
        return false;
    }

    bool isDirty(TypePackId tp) override
    {
        return tp->owningArena == &module->internalTypes;
    }

    bool ignoreChildrenVisit(TypeId ty) override
    {
        if (ty->owningArena != &module->internalTypes)
            return true;

        return false;
    }

    bool ignoreChildrenVisit(TypePackId tp) override
    {
        if (tp->owningArena != &module->internalTypes)
            return true;

        return false;
    }

    TypeId clean(TypeId ty) override
    {
        TypeId result = clone(ty);

        if (FunctionType* ftv = getMutable<FunctionType>(result))
        {
            if (ftv->generics.empty() && ftv->genericPacks.empty())
            {
                GenericTypeFinder marker;
                marker.traverse(result);

                if (!marker.found)
                    ftv->hasNoFreeOrGenericTypes = true;
            }

            ftv->level = TypeLevel{0, 0};
        }
        else if (TableType* ttv = getMutable<TableType>(result))
        {
            ttv->level = TypeLevel{0, 0};
            if (isNewSolver())
            {
                ttv->scope = nullptr;
                if (FFlag::LuauLimitDynamicConstraintSolving3)
                    ttv->state = TableState::Sealed;
            }
        }

        if (isNewSolver())
        {
            if (FFlag::LuauLimitDynamicConstraintSolving3)
            {
                if (is<FreeType, BlockedType, PendingExpansionType>(ty))
                {
                    internalTypeEscaped = true;
                    result = builtinTypes->errorType;
                }
                else if (auto genericty = getMutable<GenericType>(result))
                {
                    genericty->scope = nullptr;
                }
            }
            else
            {
                if (auto freety = getMutable<FreeType>(result))
                {
                    module->errors.emplace_back(
                        freety->scope->location,
                        module->name,
                        InternalError{"Free type is escaping its module; please report this bug at "
                                      "https://github.com/luau-lang/luau/issues"}
                    );
                    result = builtinTypes->errorType;
                }
                else if (auto genericty = getMutable<GenericType>(result))
                {
                    genericty->scope = nullptr;
                }
            }
        }

        return result;
    }

    TypePackId clean(TypePackId tp) override
    {
        if (isNewSolver())
        {
            if (FFlag::LuauLimitDynamicConstraintSolving3)
            {
                if (is<FreeTypePack, BlockedTypePack>(tp))
                {
                    internalTypeEscaped = true;
                    return builtinTypes->errorTypePack;
                }

                auto clonedTp = clone(tp);
                if (auto gtp = getMutable<GenericTypePack>(clonedTp))
                    gtp->scope = nullptr;
                return clonedTp;
            }

            auto clonedTp = clone(tp);
            if (auto ftp = getMutable<FreeTypePack>(clonedTp))
            {
                module->errors.emplace_back(
                    ftp->scope->location,
                    module->name,
                    InternalError{"Free type pack is escaping its module; please report this bug at "
                                  "https://github.com/luau-lang/luau/issues"}
                );
                clonedTp = builtinTypes->errorTypePack;
            }
            else if (auto gtp = getMutable<GenericTypePack>(clonedTp))
                gtp->scope = nullptr;
            return clonedTp;
        }
        else
        {
            return clone(tp);
        }
    }

    TypeId cloneType(TypeId ty)
    {
        std::optional<TypeId> result = substitute(ty);
        if (result)
        {
            return *result;
        }
        else
        {

            if (FFlag::LuauEmplaceNotPushBack)
                module->errors.emplace_back(module->scopes[0].first, UnificationTooComplex{});
            else
                module->errors.push_back(TypeError{module->scopes[0].first, UnificationTooComplex{}});
            return builtinTypes->errorType;
        }
    }

    TypePackId cloneTypePack(TypePackId tp)
    {
        std::optional<TypePackId> result = substitute(tp);
        if (result)
        {
            return *result;
        }
        else
        {
            if (FFlag::LuauEmplaceNotPushBack)
                module->errors.emplace_back(module->scopes[0].first, UnificationTooComplex{});
            else
                module->errors.push_back(TypeError{module->scopes[0].first, UnificationTooComplex{}});
            return builtinTypes->errorTypePack;
        }
    }

    TypeFun cloneTypeFun(const TypeFun& tf)
    {
        std::vector<GenericTypeDefinition> typeParams;
        std::vector<GenericTypePackDefinition> typePackParams;

        for (GenericTypeDefinition typeParam : tf.typeParams)
        {
            TypeId ty = cloneType(typeParam.ty);
            std::optional<TypeId> defaultValue;

            if (typeParam.defaultValue)
                defaultValue = cloneType(*typeParam.defaultValue);

            typeParams.push_back(GenericTypeDefinition{ty, defaultValue});
        }

        for (GenericTypePackDefinition typePackParam : tf.typePackParams)
        {
            TypePackId tp = cloneTypePack(typePackParam.tp);
            std::optional<TypePackId> defaultValue;

            if (typePackParam.defaultValue)
                defaultValue = cloneTypePack(*typePackParam.defaultValue);

            typePackParams.push_back(GenericTypePackDefinition{tp, defaultValue});
        }

        TypeId type = cloneType(tf.type);

        return TypeFun{std::move(typeParams), std::move(typePackParams), type, tf.definitionLocation};
    }
};

Module::~Module()
{
    unfreeze(interfaceTypes);
    unfreeze(internalTypes);
}

void Module::clonePublicInterface_DEPRECATED(NotNull<BuiltinTypes> builtinTypes, InternalErrorReporter& ice)
{
    CloneState cloneState{builtinTypes};

    ScopePtr moduleScope = getModuleScope();

    TypePackId returnType = moduleScope->returnType;
    std::optional<TypePackId> varargPack = FFlag::LuauSolverV2 ? std::nullopt : moduleScope->varargPack;

    TxnLog log;
    ClonePublicInterface clonePublicInterface{&log, builtinTypes, this};

    returnType = clonePublicInterface.cloneTypePack(returnType);

    moduleScope->returnType = returnType;
    if (varargPack)
    {
        varargPack = clonePublicInterface.cloneTypePack(*varargPack);
        moduleScope->varargPack = varargPack;
    }

    for (auto& [name, tf] : moduleScope->exportedTypeBindings)
    {
        tf = clonePublicInterface.cloneTypeFun(tf);
    }

    for (auto& [name, ty] : declaredGlobals)
    {
        ty = clonePublicInterface.cloneType(ty);
    }

    for (auto& tf : typeFunctionAliases)
    {
        *tf = clonePublicInterface.cloneTypeFun(*tf);
    }

    if (FFlag::LuauLimitDynamicConstraintSolving3 && clonePublicInterface.internalTypeEscaped)
    {
        errors.emplace_back(
            Location{}, // Not amazing but the best we can do.
            name,
            InternalError{"An internal type is escaping this module; please report this bug at "
                          "https://github.com/luau-lang/luau/issues"}
        );
    }

    // Copy external stuff over to Module itself
    this->returnType = moduleScope->returnType;
    this->exportedTypeBindings = moduleScope->exportedTypeBindings;
}

void Module::clonePublicInterface(NotNull<BuiltinTypes> builtinTypes, InternalErrorReporter& ice, SolverMode mode)
{
    CloneState cloneState{builtinTypes};

    ScopePtr moduleScope = getModuleScope();

    TypePackId returnType = moduleScope->returnType;
    std::optional<TypePackId> varargPack = mode == SolverMode::New ? std::nullopt : moduleScope->varargPack;

    TxnLog log;
    ClonePublicInterface clonePublicInterface{&log, builtinTypes, this, mode};

    returnType = clonePublicInterface.cloneTypePack(returnType);

    moduleScope->returnType = returnType;
    if (varargPack)
    {
        varargPack = clonePublicInterface.cloneTypePack(*varargPack);
        moduleScope->varargPack = varargPack;
    }

    for (auto& [name, tf] : moduleScope->exportedTypeBindings)
    {
        tf = clonePublicInterface.cloneTypeFun(tf);
    }

    for (auto& [name, ty] : declaredGlobals)
    {
        ty = clonePublicInterface.cloneType(ty);
    }

    for (auto& tf : typeFunctionAliases)
    {
        *tf = clonePublicInterface.cloneTypeFun(*tf);
    }

    if (FFlag::LuauLimitDynamicConstraintSolving3 && clonePublicInterface.internalTypeEscaped)
    {
        errors.emplace_back(
            Location{}, // Not amazing but the best we can do.
            name,
            InternalError{"An internal type is escaping this module; please report this bug at "
                          "https://github.com/luau-lang/luau/issues"}
        );
    }

    // Copy external stuff over to Module itself
    this->returnType = moduleScope->returnType;
    this->exportedTypeBindings = moduleScope->exportedTypeBindings;
}

bool Module::hasModuleScope() const
{
    return !scopes.empty();
}

ScopePtr Module::getModuleScope() const
{
    LUAU_ASSERT(hasModuleScope());
    return scopes.front().second;
}

} // namespace Luau
