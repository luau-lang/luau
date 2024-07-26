// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AnyTypeSummary.h"

#include "Luau/BuiltinDefinitions.h"
#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/Config.h"
#include "Luau/ConstraintGenerator.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/DataFlowGraph.h"
#include "Luau/DcrLogger.h"
#include "Luau/Module.h"
#include "Luau/Parser.h"
#include "Luau/Scope.h"
#include "Luau/StringUtils.h"
#include "Luau/TimeTrace.h"
#include "Luau/ToString.h"
#include "Luau/Transpiler.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeChecker2.h"
#include "Luau/NonStrictTypeChecker.h"
#include "Luau/TypeInfer.h"
#include "Luau/Variant.h"
#include "Luau/VisitType.h"
#include "Luau/TypePack.h"
#include "Luau/TypeOrPack.h"

#include <algorithm>
#include <memory>
#include <chrono>
#include <condition_variable>
#include <exception>
#include <mutex>
#include <stdexcept>
#include <string>
#include <iostream>


#include <stdio.h>

LUAU_FASTFLAGVARIABLE(StudioReportLuauAny, false);
LUAU_FASTINTVARIABLE(LuauAnySummaryRecursionLimit, 300);

LUAU_FASTFLAG(DebugLuauMagicTypes);

namespace Luau
{

// TODO: instead of pair just type for solver? generated type
// TODO: see lookupAnnotation in typechecker2. is cleaner than resolvetype
// or delay containsAny() check and do not return pair.
// quick flag in typeid saying was annotation or inferred, would be solid
std::optional<TypeOrPack> getInferredType(AstExpr* expr, Module* module)
{
    std::optional<TypeOrPack> inferredType;

    if (module->astTypePacks.contains(expr))
    {
        inferredType = *module->astTypePacks.find(expr);
    }
    else if (module->astTypes.contains(expr))
    {
        inferredType = *module->astTypes.find(expr);
    }

    return inferredType;
}

void AnyTypeSummary::traverse(Module* module, AstStat* src, NotNull<BuiltinTypes> builtinTypes)
{
    Scope* scope = findInnerMostScope(src->location, module);
    visit(scope, src, module, builtinTypes);
}

void AnyTypeSummary::visit(Scope* scope, AstStat* stat, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    RecursionLimiter limiter{&recursionCount, FInt::LuauAnySummaryRecursionLimit};

    if (auto s = stat->as<AstStatBlock>())
        return visit(scope, s, module, builtinTypes);
    else if (auto i = stat->as<AstStatIf>())
        return visit(scope, i, module, builtinTypes);
    else if (auto s = stat->as<AstStatWhile>())
        return visit(scope, s, module, builtinTypes);
    else if (auto s = stat->as<AstStatRepeat>())
        return visit(scope, s, module, builtinTypes);
    else if (auto r = stat->as<AstStatReturn>())
        return visit(scope, r, module, builtinTypes);
    else if (auto e = stat->as<AstStatExpr>())
        return visit(scope, e, module, builtinTypes);
    else if (auto s = stat->as<AstStatLocal>())
        return visit(scope, s, module, builtinTypes);
    else if (auto s = stat->as<AstStatFor>())
        return visit(scope, s, module, builtinTypes);
    else if (auto s = stat->as<AstStatForIn>())
        return visit(scope, s, module, builtinTypes);
    else if (auto a = stat->as<AstStatAssign>())
        return visit(scope, a, module, builtinTypes);
    else if (auto a = stat->as<AstStatCompoundAssign>())
        return visit(scope, a, module, builtinTypes);
    else if (auto f = stat->as<AstStatFunction>())
        return visit(scope, f, module, builtinTypes);
    else if (auto f = stat->as<AstStatLocalFunction>())
        return visit(scope, f, module, builtinTypes);
    else if (auto a = stat->as<AstStatTypeAlias>())
        return visit(scope, a, module, builtinTypes);
    else if (auto s = stat->as<AstStatDeclareGlobal>())
        return visit(scope, s, module, builtinTypes);
    else if (auto s = stat->as<AstStatDeclareFunction>())
        return visit(scope, s, module, builtinTypes);
    else if (auto s = stat->as<AstStatDeclareClass>())
        return visit(scope, s, module, builtinTypes);
    else if (auto s = stat->as<AstStatError>())
        return visit(scope, s, module, builtinTypes);
}

void AnyTypeSummary::visit(Scope* scope, AstStatBlock* block, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LuauAnySummaryRecursionLimit)
        return; // don't report

    for (AstStat* stat : block->body)
        visit(scope, stat, module, builtinTypes);
}

void AnyTypeSummary::visit(Scope* scope, AstStatIf* ifStatement, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    if (ifStatement->thenbody)
    {
        Scope* thenScope = findInnerMostScope(ifStatement->thenbody->location, module);
        visit(thenScope, ifStatement->thenbody, module, builtinTypes);
    }

    if (ifStatement->elsebody)
    {
        Scope* elseScope = findInnerMostScope(ifStatement->elsebody->location, module);
        visit(elseScope, ifStatement->elsebody, module, builtinTypes);
    }
}

void AnyTypeSummary::visit(Scope* scope, AstStatWhile* while_, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    Scope* whileScope = findInnerMostScope(while_->location, module);
    visit(whileScope, while_->body, module, builtinTypes);
}

void AnyTypeSummary::visit(Scope* scope, AstStatRepeat* repeat, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    Scope* repeatScope = findInnerMostScope(repeat->location, module);
    visit(repeatScope, repeat->body, module, builtinTypes);
}

void AnyTypeSummary::visit(Scope* scope, AstStatReturn* ret, Module* module, NotNull<BuiltinTypes> builtinTypes)
{   
    // Scope* outScope = findOuterScope(ret->location, module);
    Scope* retScope = findInnerMostScope(ret->location, module);

    for (auto val : ret->list)
    {
        if (isAnyCall(retScope, val, module, builtinTypes))
        {
            TelemetryTypePair types;
            types.inferredType = toString(lookupType(val, module, builtinTypes));
            TypeInfo ti{Pattern::FuncApp, toString(ret), types};
            typeInfo.push_back(ti);
        }

        if (isAnyCast(retScope, val, module, builtinTypes))
        {
            if (auto cast = val->as<AstExprTypeAssertion>())
            {
                TelemetryTypePair types;

                types.annotatedType = toString(lookupAnnotation(cast->annotation, module, builtinTypes));
                auto inf = getInferredType(cast->expr, module);
                if (inf)    
                    types.inferredType = toString(*inf);

                TypeInfo ti{Pattern::Casts, toString(ret), types};
                typeInfo.push_back(ti);
            }
        }
    }
}

void AnyTypeSummary::visit(Scope* scope, AstStatLocal* local, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    TypePackId values = reconstructTypePack(local->values, module, builtinTypes);
    auto [head, tail] = flatten(values);

    size_t posn = 0;
    for (AstLocal* loc : local->vars)
    {
        if (local->vars.data[0] == loc && posn < local->values.size)
        {
            if (loc->annotation)
            {
                auto annot = lookupAnnotation(loc->annotation, module, builtinTypes);
                if (containsAny(annot))
                {
                    TelemetryTypePair types;

                    types.annotatedType = toString(annot);

                    auto inf = getInferredType(local->values.data[posn], module);
                    if (inf)
                        types.inferredType = toString(*inf);

                    TypeInfo ti{Pattern::VarAnnot, toString(local), types};
                    typeInfo.push_back(ti);
                }
            }
        }
        else
        {
            if (std::min(local->values.size - 1, posn) < head.size())
            {
                if (loc->annotation)
                {
                    auto annot = lookupAnnotation(loc->annotation, module, builtinTypes);
                    if (containsAny(annot))
                    {
                        TelemetryTypePair types;

                        types.annotatedType = toString(annot);
                        types.inferredType = toString(head[std::min(local->values.size - 1, posn)]);

                        TypeInfo ti{Pattern::VarAnnot, toString(local), types};
                        typeInfo.push_back(ti);
                    }
                }
            }
            else
            {
                if (tail)
                {
                    if (containsAny(*tail))
                    {
                        TelemetryTypePair types;

                        types.inferredType = toString(*tail);

                        TypeInfo ti{Pattern::VarAny, toString(local), types};
                        typeInfo.push_back(ti);
                    }
                }
            }
        }

        ++posn;
    }
}

void AnyTypeSummary::visit(Scope* scope, AstStatFor* for_, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    Scope* forScope = findInnerMostScope(for_->location, module);
    visit(forScope, for_->body, module, builtinTypes);
}

void AnyTypeSummary::visit(Scope* scope, AstStatForIn* forIn, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    Scope* loopScope = findInnerMostScope(forIn->location, module);
    visit(loopScope, forIn->body, module, builtinTypes);
}

void AnyTypeSummary::visit(Scope* scope, AstStatAssign* assign, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    TypePackId values = reconstructTypePack(assign->values, module, builtinTypes);
    auto [head, tail] = flatten(values);

    size_t posn = 0;
    for (AstExpr* var : assign->vars)
    {
        TypeId tp = lookupType(var, module, builtinTypes);
        if (containsAny(tp))
        {
            TelemetryTypePair types;

            types.annotatedType = toString(tp);

            auto loc = std::min(assign->vars.size - 1, posn);
            if (head.size() >= assign->vars.size)
            {
                types.inferredType = toString(head[posn]);
            }
            else if (loc < head.size())
                types.inferredType = toString(head[loc]);
            else
                types.inferredType = toString(builtinTypes->nilType);

            TypeInfo ti{Pattern::Assign, toString(assign), types};
            typeInfo.push_back(ti);
        }
        ++posn;
    }

    for (AstExpr* val : assign->values)
    {
        if (isAnyCall(scope, val, module, builtinTypes))
        {
            TelemetryTypePair types;

            auto inf = getInferredType(val, module);
            if (inf)
                types.inferredType = toString(*inf);

            TypeInfo ti{Pattern::FuncApp, toString(assign), types};
            typeInfo.push_back(ti);
        }

        if (isAnyCast(scope, val, module, builtinTypes))
        {
            if (auto cast = val->as<AstExprTypeAssertion>())
            {
                TelemetryTypePair types;

                types.annotatedType = toString(lookupAnnotation(cast->annotation, module, builtinTypes));
                auto inf = getInferredType(val, module);
                if (inf)    
                    types.inferredType = toString(*inf);

                TypeInfo ti{Pattern::Casts, toString(assign), types};
                typeInfo.push_back(ti);
            }
        }
    }

    if (tail) 
    {
        if (containsAny(*tail))
        {
            TelemetryTypePair types;

            types.inferredType = toString(*tail);

            TypeInfo ti{Pattern::Assign, toString(assign), types};
            typeInfo.push_back(ti);
        }
    }
}

void AnyTypeSummary::visit(Scope* scope, AstStatCompoundAssign* assign, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    TelemetryTypePair types;

    types.inferredType = toString(lookupType(assign->value, module, builtinTypes));
    types.annotatedType = toString(lookupType(assign->var, module, builtinTypes));

    if (module->astTypes.contains(assign->var))
    {
        if (containsAny(*module->astTypes.find(assign->var)))
        {
            TypeInfo ti{Pattern::Assign, toString(assign), types};
            typeInfo.push_back(ti);
        }
    }
    else if (module->astTypePacks.contains(assign->var))
    {
        if (containsAny(*module->astTypePacks.find(assign->var)))
        {
            TypeInfo ti{Pattern::Assign, toString(assign), types};
            typeInfo.push_back(ti);
        }
    }

    if (isAnyCall(scope, assign->value, module, builtinTypes))
    {
        TypeInfo ti{Pattern::FuncApp, toString(assign), types};
        typeInfo.push_back(ti);
    }

    if (isAnyCast(scope, assign->value, module, builtinTypes))
    {
        if (auto cast = assign->value->as<AstExprTypeAssertion>())
        {
            types.annotatedType = toString(lookupAnnotation(cast->annotation, module, builtinTypes));
            auto inf = getInferredType(cast->expr, module);
            if (inf)    
                types.inferredType = toString(*inf);

            TypeInfo ti{Pattern::Casts, toString(assign), types};
            typeInfo.push_back(ti);
        }
    }
}

void AnyTypeSummary::visit(Scope* scope, AstStatFunction* function, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    TelemetryTypePair types;
    types.inferredType = toString(lookupType(function->func, module, builtinTypes));

    if (hasVariadicAnys(scope, function->func, module, builtinTypes))
    {
        TypeInfo ti{Pattern::VarAny, toString(function), types};
        typeInfo.push_back(ti);
    }

    if (hasArgAnys(scope, function->func, module, builtinTypes))
    {
        TypeInfo ti{Pattern::FuncArg, toString(function), types};
        typeInfo.push_back(ti);
    }

    if (hasAnyReturns(scope, function->func, module, builtinTypes))
    {
        TypeInfo ti{Pattern::FuncRet, toString(function), types};
        typeInfo.push_back(ti);
    }

    if (function->func->body->body.size > 0)
        visit(scope, function->func->body, module, builtinTypes);
}

void AnyTypeSummary::visit(Scope* scope, AstStatLocalFunction* function, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    TelemetryTypePair types;
    types.inferredType = toString(lookupType(function->func, module, builtinTypes));

    if (hasVariadicAnys(scope, function->func, module, builtinTypes))
    {
        TypeInfo ti{Pattern::VarAny, toString(function), types};
        typeInfo.push_back(ti);
    }

    if (hasArgAnys(scope, function->func, module, builtinTypes))
    {
        TypeInfo ti{Pattern::FuncArg, toString(function), types};
        typeInfo.push_back(ti);
    }

    if (hasAnyReturns(scope, function->func, module, builtinTypes))
    {
        TypeInfo ti{Pattern::FuncRet, toString(function), types};
        typeInfo.push_back(ti);
    }

    if (function->func->body->body.size > 0)
        visit(scope, function->func->body, module, builtinTypes);
}

void AnyTypeSummary::visit(Scope* scope, AstStatTypeAlias* alias, Module* module, NotNull<BuiltinTypes> builtinTypes)
{

    auto annot = lookupAnnotation(alias->type, module, builtinTypes);
    if (containsAny(annot))
    {
        // no expr => no inference for aliases
        TelemetryTypePair types;

        types.annotatedType = toString(annot);
        TypeInfo ti{Pattern::Alias, toString(alias), types};
        typeInfo.push_back(ti);
    }
}

void AnyTypeSummary::visit(Scope* scope, AstStatExpr* expr, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    if (isAnyCall(scope, expr->expr, module, builtinTypes))
    {
        TelemetryTypePair types;

        types.inferredType = toString(lookupType(expr->expr, module, builtinTypes));

        TypeInfo ti{Pattern::FuncApp, toString(expr), types};
        typeInfo.push_back(ti);
    }
}

void AnyTypeSummary::visit(Scope* scope, AstStatDeclareGlobal* declareGlobal, Module* module, NotNull<BuiltinTypes> builtinTypes) {}

void AnyTypeSummary::visit(Scope* scope, AstStatDeclareClass* declareClass, Module* module, NotNull<BuiltinTypes> builtinTypes) {}

void AnyTypeSummary::visit(Scope* scope, AstStatDeclareFunction* declareFunction, Module* module, NotNull<BuiltinTypes> builtinTypes) {}

void AnyTypeSummary::visit(Scope* scope, AstStatError* error, Module* module, NotNull<BuiltinTypes> builtinTypes) {}

TypeId AnyTypeSummary::checkForFamilyInhabitance(TypeId instance, Location location)
{
    if (seenTypeFamilyInstances.find(instance))
        return instance;

    seenTypeFamilyInstances.insert(instance);
    return instance;
}

TypeId AnyTypeSummary::lookupType(AstExpr* expr, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    TypeId* ty = module->astTypes.find(expr);
    if (ty)
        return checkForFamilyInhabitance(follow(*ty), expr->location);

    TypePackId* tp = module->astTypePacks.find(expr);
    if (tp)
    {
        if (auto fst = first(*tp, /*ignoreHiddenVariadics*/ false))
            return checkForFamilyInhabitance(*fst, expr->location);
        else if (finite(*tp) && size(*tp) == 0)
            return checkForFamilyInhabitance(builtinTypes->nilType, expr->location);
    }

    return builtinTypes->errorRecoveryType();
}

TypePackId AnyTypeSummary::reconstructTypePack(AstArray<AstExpr*> exprs, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    if (exprs.size == 0)
        return arena.addTypePack(TypePack{{}, std::nullopt});

    std::vector<TypeId> head;

    for (size_t i = 0; i < exprs.size - 1; ++i)
    {
        head.push_back(lookupType(exprs.data[i], module, builtinTypes));
    }

    TypePackId* tail = module->astTypePacks.find(exprs.data[exprs.size - 1]);
    if (tail)
        return arena.addTypePack(TypePack{std::move(head), follow(*tail)});
    else
        return arena.addTypePack(TypePack{std::move(head), builtinTypes->errorRecoveryTypePack()});
}

bool AnyTypeSummary::isAnyCall(Scope* scope, AstExpr* expr, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    if (auto call = expr->as<AstExprCall>())
    {
        TypePackId args = reconstructTypePack(call->args, module, builtinTypes);
        if (containsAny(args))
            return true;

        TypeId func = lookupType(call->func, module, builtinTypes);
        if (containsAny(func))
            return true;
    }
    return false;
}

bool AnyTypeSummary::hasVariadicAnys(Scope* scope, AstExprFunction* expr, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    if (expr->vararg && expr->varargAnnotation)
    {
        auto annot = lookupPackAnnotation(expr->varargAnnotation, module);
        if (annot && containsAny(*annot))
        {
            return true;
        }
    }
    return false;
}

bool AnyTypeSummary::hasArgAnys(Scope* scope, AstExprFunction* expr, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    if (expr->args.size > 0)
    {
        for (const AstLocal* arg : expr->args)
        {
            if (arg->annotation)
            {
                auto annot = lookupAnnotation(arg->annotation, module, builtinTypes);
                if (containsAny(annot))
                {
                    return true;
                }
            }
        }
    }
    return false;
}

bool AnyTypeSummary::hasAnyReturns(Scope* scope, AstExprFunction* expr, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    if (!expr->returnAnnotation)
    {
        return false;
    }

    for (AstType* ret : expr->returnAnnotation->types)
    {
        if (containsAny(lookupAnnotation(ret, module, builtinTypes)))
        {
            return true;
        }
    }

    if (expr->returnAnnotation->tailType)
    {
        auto annot = lookupPackAnnotation(expr->returnAnnotation->tailType, module);
        if (annot && containsAny(*annot))
        {
            return true;
        }
    }

    return false;
}

bool AnyTypeSummary::isAnyCast(Scope* scope, AstExpr* expr, Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    if (auto cast = expr->as<AstExprTypeAssertion>())
    {
        auto annot = lookupAnnotation(cast->annotation, module, builtinTypes);
        if (containsAny(annot))
        {
            return true;
        }
    }
    return false;
}

TypeId AnyTypeSummary::lookupAnnotation(AstType* annotation, Module* module, NotNull<BuiltinTypes> builtintypes)
{
    if (FFlag::DebugLuauMagicTypes)
    {
        if (auto ref = annotation->as<AstTypeReference>(); ref && ref->parameters.size > 0)
        {
            if (auto ann = ref->parameters.data[0].type)
            {
                TypeId argTy = lookupAnnotation(ref->parameters.data[0].type, module, builtintypes);
                return follow(argTy);
            }
        }
    }

    TypeId* ty = module->astResolvedTypes.find(annotation);
    if (ty)
        return checkForTypeFunctionInhabitance(follow(*ty), annotation->location);
    else
        return checkForTypeFunctionInhabitance(builtintypes->errorRecoveryType(), annotation->location);
}

TypeId AnyTypeSummary::checkForTypeFunctionInhabitance(TypeId instance, Location location)
{
    if (seenTypeFunctionInstances.find(instance))
        return instance;
    seenTypeFunctionInstances.insert(instance);

    return instance;
}

std::optional<TypePackId> AnyTypeSummary::lookupPackAnnotation(AstTypePack* annotation, Module* module)
{
    TypePackId* tp = module->astResolvedTypePacks.find(annotation);
    if (tp != nullptr)
        return {follow(*tp)};
    return {};
}

bool AnyTypeSummary::containsAny(TypeId typ)
{
    typ = follow(typ);

    if (auto t = seen.find(typ); t && !*t)
    {
        return false;
    }

    seen[typ] = false;

    RecursionCounter counter{&recursionCount};
    if (recursionCount >= FInt::LuauAnySummaryRecursionLimit)
    {
        return false;
    }

    bool found = false;

    if (auto ty = get<AnyType>(typ))
    {
        found = true;
    }
    else if (auto ty = get<UnknownType>(typ))
    {
        found = true;
    }
    else if (auto ty = get<TableType>(typ))
    {
        for (auto& [_name, prop] : ty->props)
        {
            if (FFlag::DebugLuauDeferredConstraintResolution)
            {
                if (auto newT = follow(prop.readTy))
                {
                    if (containsAny(*newT))
                        found = true;
                }
                else if (auto newT = follow(prop.writeTy))
                {
                    if (containsAny(*newT))
                        found = true;
                }
            }
            else
            {
                if (containsAny(prop.type()))
                    found = true;
            }
        }
    }
    else if (auto ty = get<IntersectionType>(typ))
    {
        for (auto part : ty->parts)
        {
            if (containsAny(part))
            {
                found = true;
            }
        }
    }
    else if (auto ty = get<UnionType>(typ))
    {
        for (auto option : ty->options)
        {
            if (containsAny(option))
            {
                found = true;
            }
        }
    }
    else if (auto ty = get<FunctionType>(typ))
    {
        if (containsAny(ty->argTypes))
            found = true;
        else if (containsAny(ty->retTypes))
            found = true;
    }

    seen[typ] = found;

    return found;
}

bool AnyTypeSummary::containsAny(TypePackId typ)
{
    typ = follow(typ);

    if (auto t = seen.find(typ); t && !*t)
    {
        return false;
    }

    seen[typ] = false;

    auto [head, tail] = flatten(typ);
    bool found = false;

    for (auto tp : head)
    {
        if (containsAny(tp))
            found = true;
    }

    if (tail)
    {
        if (auto vtp = get<VariadicTypePack>(tail))
        {
            if (auto ty = get<AnyType>(follow(vtp->ty)))
            {
                found = true;
            }
        }
        else if (auto tftp = get<TypeFunctionInstanceTypePack>(tail))
        {

            for (TypePackId tp : tftp->packArguments)
            {
                if (containsAny(tp))
                {
                    found = true;
                }
            }

            for (TypeId t : tftp->typeArguments)
            {
                if (containsAny(t))
                {
                    found = true;
                }
            }
        }
    }

    seen[typ] = found;

    return found;
}

Scope* AnyTypeSummary::findInnerMostScope(Location location, Module* module)
{
    Scope* bestScope = module->getModuleScope().get();

    bool didNarrow = false;
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

    return bestScope;
}

AnyTypeSummary::TypeInfo::TypeInfo(Pattern code, std::string node, TelemetryTypePair type)
    : code(code)
    , node(node)
    , type(type)
{
}

AnyTypeSummary::AnyTypeSummary() {}

} // namespace Luau