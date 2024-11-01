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

LUAU_FASTFLAGVARIABLE(StudioReportLuauAny2);
LUAU_FASTINTVARIABLE(LuauAnySummaryRecursionLimit, 300);

LUAU_FASTFLAG(DebugLuauMagicTypes);

namespace Luau
{

void AnyTypeSummary::traverse(const Module* module, AstStat* src, NotNull<BuiltinTypes> builtinTypes)
{
    visit(findInnerMostScope(src->location, module), src, module, builtinTypes);
}

void AnyTypeSummary::visit(const Scope* scope, AstStat* stat, const Module* module, NotNull<BuiltinTypes> builtinTypes)
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

void AnyTypeSummary::visit(const Scope* scope, AstStatBlock* block, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LuauAnySummaryRecursionLimit)
        return; // don't report

    for (AstStat* stat : block->body)
        visit(scope, stat, module, builtinTypes);
}

void AnyTypeSummary::visit(const Scope* scope, AstStatIf* ifStatement, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    if (ifStatement->thenbody)
    {
        const Scope* thenScope = findInnerMostScope(ifStatement->thenbody->location, module);
        visit(thenScope, ifStatement->thenbody, module, builtinTypes);
    }

    if (ifStatement->elsebody)
    {
        const Scope* elseScope = findInnerMostScope(ifStatement->elsebody->location, module);
        visit(elseScope, ifStatement->elsebody, module, builtinTypes);
    }
}

void AnyTypeSummary::visit(const Scope* scope, AstStatWhile* while_, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    const Scope* whileScope = findInnerMostScope(while_->location, module);
    visit(whileScope, while_->body, module, builtinTypes);
}

void AnyTypeSummary::visit(const Scope* scope, AstStatRepeat* repeat, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    const Scope* repeatScope = findInnerMostScope(repeat->location, module);
    visit(repeatScope, repeat->body, module, builtinTypes);
}

void AnyTypeSummary::visit(const Scope* scope, AstStatReturn* ret, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    const Scope* retScope = findInnerMostScope(ret->location, module);

    auto ctxNode = getNode(rootSrc, ret);
    bool seenTP = false;

    for (auto val : ret->list)
    {
        if (isAnyCall(retScope, val, module, builtinTypes))
        {
            TelemetryTypePair types;
            types.inferredType = toString(lookupType(val, module, builtinTypes));
            TypeInfo ti{Pattern::FuncApp, toString(ctxNode), types};
            typeInfo.push_back(ti);
        }

        if (isAnyCast(retScope, val, module, builtinTypes))
        {
            if (auto cast = val->as<AstExprTypeAssertion>())
            {
                TelemetryTypePair types;

                types.annotatedType = toString(lookupAnnotation(cast->annotation, module, builtinTypes));
                types.inferredType = toString(lookupType(cast->expr, module, builtinTypes));

                TypeInfo ti{Pattern::Casts, toString(ctxNode), types};
                typeInfo.push_back(ti);
            }
        }

        if (ret->list.size > 1 && !seenTP)
        {
            if (containsAny(retScope->returnType))
            {
                seenTP = true;

                TelemetryTypePair types;

                types.inferredType = toString(retScope->returnType);

                TypeInfo ti{Pattern::TypePk, toString(ctxNode), types};
                typeInfo.push_back(ti);
            }
        }
    }

}

void AnyTypeSummary::visit(const Scope* scope, AstStatLocal* local, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    auto ctxNode = getNode(rootSrc, local);

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
                    types.inferredType = toString(lookupType(local->values.data[posn], module, builtinTypes));

                    TypeInfo ti{Pattern::VarAnnot, toString(ctxNode), types};
                    typeInfo.push_back(ti);
                }
            }

            const AstExprTypeAssertion* maybeRequire = local->values.data[posn]->as<AstExprTypeAssertion>();
            if (!maybeRequire)
                continue;

            if (std::min(local->values.size - 1, posn) < head.size())
            {
                if (isAnyCast(scope, local->values.data[posn], module, builtinTypes))
                {
                    TelemetryTypePair types;

                    types.inferredType = toString(head[std::min(local->values.size - 1, posn)]);

                    TypeInfo ti{Pattern::Casts, toString(ctxNode), types};
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

                        TypeInfo ti{Pattern::VarAnnot, toString(ctxNode), types};
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

                        TypeInfo ti{Pattern::VarAny, toString(ctxNode), types};
                        typeInfo.push_back(ti);
                    }
                }
            }
        }

        ++posn;
    }
}

void AnyTypeSummary::visit(const Scope* scope, AstStatFor* for_, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    const Scope* forScope = findInnerMostScope(for_->location, module);
    visit(forScope, for_->body, module, builtinTypes);
}

void AnyTypeSummary::visit(const Scope* scope, AstStatForIn* forIn, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    const Scope* loopScope = findInnerMostScope(forIn->location, module);
    visit(loopScope, forIn->body, module, builtinTypes);
}

void AnyTypeSummary::visit(const Scope* scope, AstStatAssign* assign, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    auto ctxNode = getNode(rootSrc, assign);

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
            if (head.size() >= assign->vars.size && posn < head.size())
            {
                types.inferredType = toString(head[posn]);
            }
            else if (loc < head.size())
                types.inferredType = toString(head[loc]);
            else
                types.inferredType = toString(builtinTypes->nilType);

            TypeInfo ti{Pattern::Assign, toString(ctxNode), types};
            typeInfo.push_back(ti);
        }
        ++posn;
    }

    for (AstExpr* val : assign->values)
    {
        if (isAnyCall(scope, val, module, builtinTypes))
        {
            TelemetryTypePair types;

            types.inferredType = toString(lookupType(val, module, builtinTypes));

            TypeInfo ti{Pattern::FuncApp, toString(ctxNode), types};
            typeInfo.push_back(ti);
        }

        if (isAnyCast(scope, val, module, builtinTypes))
        {
            if (auto cast = val->as<AstExprTypeAssertion>())
            {
                TelemetryTypePair types;

                types.annotatedType = toString(lookupAnnotation(cast->annotation, module, builtinTypes));
                types.inferredType = toString(lookupType(val, module, builtinTypes));

                TypeInfo ti{Pattern::Casts, toString(ctxNode), types};
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

            TypeInfo ti{Pattern::Assign, toString(ctxNode), types};
            typeInfo.push_back(ti);
        }
    }
}

void AnyTypeSummary::visit(const Scope* scope, AstStatCompoundAssign* assign, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    auto ctxNode = getNode(rootSrc, assign);

    TelemetryTypePair types;

    types.inferredType = toString(lookupType(assign->value, module, builtinTypes));
    types.annotatedType = toString(lookupType(assign->var, module, builtinTypes));

    if (module->astTypes.contains(assign->var))
    {
        if (containsAny(*module->astTypes.find(assign->var)))
        {
            TypeInfo ti{Pattern::Assign, toString(ctxNode), types};
            typeInfo.push_back(ti);
        }
    }
    else if (module->astTypePacks.contains(assign->var))
    {
        if (containsAny(*module->astTypePacks.find(assign->var)))
        {
            TypeInfo ti{Pattern::Assign, toString(ctxNode), types};
            typeInfo.push_back(ti);
        }
    }

    if (isAnyCall(scope, assign->value, module, builtinTypes))
    {
        TypeInfo ti{Pattern::FuncApp, toString(ctxNode), types};
        typeInfo.push_back(ti);
    }

    if (isAnyCast(scope, assign->value, module, builtinTypes))
    {
        if (auto cast = assign->value->as<AstExprTypeAssertion>())
        {
            types.annotatedType = toString(lookupAnnotation(cast->annotation, module, builtinTypes));
            types.inferredType = toString(lookupType(cast->expr, module, builtinTypes));

            TypeInfo ti{Pattern::Casts, toString(ctxNode), types};
            typeInfo.push_back(ti);
        }
    }
}

void AnyTypeSummary::visit(const Scope* scope, AstStatFunction* function, const Module* module, NotNull<BuiltinTypes> builtinTypes)
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

void AnyTypeSummary::visit(const Scope* scope, AstStatLocalFunction* function, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    TelemetryTypePair types;

    if (hasVariadicAnys(scope, function->func, module, builtinTypes))
    {
        types.inferredType = toString(lookupType(function->func, module, builtinTypes));
        TypeInfo ti{Pattern::VarAny, toString(function), types};
        typeInfo.push_back(ti);
    }

    if (hasArgAnys(scope, function->func, module, builtinTypes))
    {
        types.inferredType = toString(lookupType(function->func, module, builtinTypes));
        TypeInfo ti{Pattern::FuncArg, toString(function), types};
        typeInfo.push_back(ti);
    }

    if (hasAnyReturns(scope, function->func, module, builtinTypes))
    {
        types.inferredType = toString(lookupType(function->func, module, builtinTypes));
        TypeInfo ti{Pattern::FuncRet, toString(function), types};
        typeInfo.push_back(ti);
    }

    if (function->func->body->body.size > 0)
        visit(scope, function->func->body, module, builtinTypes);
}

void AnyTypeSummary::visit(const Scope* scope, AstStatTypeAlias* alias, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    auto ctxNode = getNode(rootSrc, alias);

    auto annot = lookupAnnotation(alias->type, module, builtinTypes);
    if (containsAny(annot))
    {
        // no expr => no inference for aliases
        TelemetryTypePair types;

        types.annotatedType = toString(annot);
        TypeInfo ti{Pattern::Alias, toString(ctxNode), types};
        typeInfo.push_back(ti);
    }
}

void AnyTypeSummary::visit(const Scope* scope, AstStatExpr* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    auto ctxNode = getNode(rootSrc, expr);

    if (isAnyCall(scope, expr->expr, module, builtinTypes))
    {
        TelemetryTypePair types;
        types.inferredType = toString(lookupType(expr->expr, module, builtinTypes));

        TypeInfo ti{Pattern::FuncApp, toString(ctxNode), types};
        typeInfo.push_back(ti);
    }
}

void AnyTypeSummary::visit(const Scope* scope, AstStatDeclareGlobal* declareGlobal, const Module* module, NotNull<BuiltinTypes> builtinTypes) {}

void AnyTypeSummary::visit(const Scope* scope, AstStatDeclareClass* declareClass, const Module* module, NotNull<BuiltinTypes> builtinTypes) {}

void AnyTypeSummary::visit(const Scope* scope, AstStatDeclareFunction* declareFunction, const Module* module, NotNull<BuiltinTypes> builtinTypes) {}

void AnyTypeSummary::visit(const Scope* scope, AstStatError* error, const Module* module, NotNull<BuiltinTypes> builtinTypes) {}

TypeId AnyTypeSummary::checkForFamilyInhabitance(const TypeId instance, const Location location)
{
    if (seenTypeFamilyInstances.find(instance))
        return instance;

    seenTypeFamilyInstances.insert(instance);
    return instance;
}

TypeId AnyTypeSummary::lookupType(const AstExpr* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    const TypeId* ty = module->astTypes.find(expr);
    if (ty)
        return checkForFamilyInhabitance(follow(*ty), expr->location);

    const TypePackId* tp = module->astTypePacks.find(expr);
    if (tp)
    {
        if (auto fst = first(*tp, /*ignoreHiddenVariadics*/ false))
            return checkForFamilyInhabitance(*fst, expr->location);
        else if (finite(*tp) && size(*tp) == 0)
            return checkForFamilyInhabitance(builtinTypes->nilType, expr->location);
    }

    return builtinTypes->errorRecoveryType();
}

TypePackId AnyTypeSummary::reconstructTypePack(AstArray<AstExpr*> exprs, const Module* module, NotNull<BuiltinTypes> builtinTypes)
{
    if (exprs.size == 0)
        return arena.addTypePack(TypePack{{}, std::nullopt});

    std::vector<TypeId> head;

    for (size_t i = 0; i < exprs.size - 1; ++i)
    {
        head.push_back(lookupType(exprs.data[i], module, builtinTypes));
    }

    const TypePackId* tail = module->astTypePacks.find(exprs.data[exprs.size - 1]);
    if (tail)
        return arena.addTypePack(TypePack{std::move(head), follow(*tail)});
    else
        return arena.addTypePack(TypePack{std::move(head), builtinTypes->errorRecoveryTypePack()});
}

bool AnyTypeSummary::isAnyCall(const Scope* scope, AstExpr* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes)
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

bool AnyTypeSummary::hasVariadicAnys(const Scope* scope, AstExprFunction* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes)
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

bool AnyTypeSummary::hasArgAnys(const Scope* scope, AstExprFunction* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes)
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

bool AnyTypeSummary::hasAnyReturns(const Scope* scope, AstExprFunction* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes)
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

bool AnyTypeSummary::isAnyCast(const Scope* scope, AstExpr* expr, const Module* module, NotNull<BuiltinTypes> builtinTypes)
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

TypeId AnyTypeSummary::lookupAnnotation(AstType* annotation, const Module* module, NotNull<BuiltinTypes> builtintypes)
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

    const TypeId* ty = module->astResolvedTypes.find(annotation);
    if (ty)
        return checkForTypeFunctionInhabitance(follow(*ty), annotation->location);
    else
        return checkForTypeFunctionInhabitance(builtintypes->errorRecoveryType(), annotation->location);
}

TypeId AnyTypeSummary::checkForTypeFunctionInhabitance(const TypeId instance, const Location location)
{
    if (seenTypeFunctionInstances.find(instance))
        return instance;
    seenTypeFunctionInstances.insert(instance);

    return instance;
}

std::optional<TypePackId> AnyTypeSummary::lookupPackAnnotation(AstTypePack* annotation, const Module* module)
{
    const TypePackId* tp = module->astResolvedTypePacks.find(annotation);
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
            if (FFlag::LuauSolverV2)
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

const Scope* AnyTypeSummary::findInnerMostScope(const Location location, const Module* module)
{
    const Scope* bestScope = module->getModuleScope().get();

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

std::optional<AstExpr*> AnyTypeSummary::matchRequire(const AstExprCall& call)
{
    const char* require = "require";

    if (call.args.size != 1)
        return std::nullopt;

    const AstExprGlobal* funcAsGlobal = call.func->as<AstExprGlobal>();
    if (!funcAsGlobal || funcAsGlobal->name != require)
        return std::nullopt;

    if (call.args.size != 1)
        return std::nullopt;

    return call.args.data[0];
}

AstNode* AnyTypeSummary::getNode(AstStatBlock* root, AstNode* node)
{
    FindReturnAncestry finder(node, root->location.end);
    root->visit(&finder);

    if (!finder.currNode)
        finder.currNode = node;

    LUAU_ASSERT(finder.found && finder.currNode);
    return finder.currNode;
}

bool AnyTypeSummary::FindReturnAncestry::visit(AstStatLocalFunction* node)
{
    currNode = node;
    return !found;
}

bool AnyTypeSummary::FindReturnAncestry::visit(AstStatFunction* node)
{
    currNode = node;
    return !found;
}

bool AnyTypeSummary::FindReturnAncestry::visit(AstType* node)
{
    return !found;
}

bool AnyTypeSummary::FindReturnAncestry::visit(AstNode* node)
{
    if (node == stat)
    {
        found = true;
    }

    if (node->location.end == rootEnd && stat->location.end >= rootEnd)
    {
        currNode = node;
        found = true;
    }

    return !found;
}


AnyTypeSummary::TypeInfo::TypeInfo(Pattern code, std::string node, TelemetryTypePair type)
    : code(code)
    , node(node)
    , type(type)
{
}

AnyTypeSummary::FindReturnAncestry::FindReturnAncestry(AstNode* stat, Position rootEnd)
    : stat(stat)
    , rootEnd(rootEnd)
{
}

AnyTypeSummary::AnyTypeSummary() {}

} // namespace Luau