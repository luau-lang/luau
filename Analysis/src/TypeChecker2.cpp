
#include "Luau/TypeChecker2.h"

#include <algorithm>

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Clone.h"
#include "Luau/Instantiation.h"
#include "Luau/Normalize.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeUtils.h"
#include "Luau/TypeVar.h"
#include "Luau/Unifier.h"
#include "Luau/ToString.h"

namespace Luau
{

struct TypeChecker2 : public AstVisitor
{
    const SourceModule* sourceModule;
    Module* module;
    InternalErrorReporter ice; // FIXME accept a pointer from Frontend
    SingletonTypes& singletonTypes;

    TypeChecker2(const SourceModule* sourceModule, Module* module)
        : sourceModule(sourceModule)
        , module(module)
        , singletonTypes(getSingletonTypes())
    {
    }

    using AstVisitor::visit;

    TypePackId lookupPack(AstExpr* expr)
    {
        // If a type isn't in the type graph, it probably means that a recursion limit was exceeded.
        // We'll just return anyType in these cases.  Typechecking against any is very fast and this
        // allows us not to think about this very much in the actual typechecking logic.
        TypePackId* tp = module->astTypePacks.find(expr);
        if (tp)
            return follow(*tp);
        else
            return singletonTypes.anyTypePack;
    }

    TypeId lookupType(AstExpr* expr)
    {
        // If a type isn't in the type graph, it probably means that a recursion limit was exceeded.
        // We'll just return anyType in these cases.  Typechecking against any is very fast and this
        // allows us not to think about this very much in the actual typechecking logic.
        TypeId* ty = module->astTypes.find(expr);
        if (ty)
            return follow(*ty);

        TypePackId* tp = module->astTypePacks.find(expr);
        if (tp)
            return flattenPack(*tp);

        return singletonTypes.anyType;
    }

    TypeId lookupAnnotation(AstType* annotation)
    {
        TypeId* ty = module->astResolvedTypes.find(annotation);
        LUAU_ASSERT(ty);
        return follow(*ty);
    }

    TypePackId lookupPackAnnotation(AstTypePack* annotation)
    {
        TypePackId* tp = module->astResolvedTypePacks.find(annotation);
        LUAU_ASSERT(tp);
        return follow(*tp);
    }

    TypePackId reconstructPack(AstArray<AstExpr*> exprs, TypeArena& arena)
    {
        if (exprs.size == 0)
            return arena.addTypePack(TypePack{{}, std::nullopt});

        std::vector<TypeId> head;

        for (size_t i = 0; i < exprs.size - 1; ++i)
        {
            head.push_back(lookupType(exprs.data[i]));
        }

        TypePackId tail = lookupPack(exprs.data[exprs.size - 1]);
        return arena.addTypePack(TypePack{head, tail});
    }

    Scope* findInnermostScope(Location location)
    {
        Scope* bestScope = module->getModuleScope().get();
        Location bestLocation = module->scopes[0].first;

        for (size_t i = 0; i < module->scopes.size(); ++i)
        {
            auto& [scopeBounds, scope] = module->scopes[i];
            if (scopeBounds.encloses(location))
            {
                if (scopeBounds.begin > bestLocation.begin || scopeBounds.end < bestLocation.end)
                {
                    bestScope = scope.get();
                    bestLocation = scopeBounds;
                }
            }
            else if (scopeBounds.begin > location.end)
            {
                // TODO: Is this sound? This relies on the fact that scopes are inserted
                // into the scope list in the order that they appear in the AST.
                break;
            }
        }

        return bestScope;
    }

    bool visit(AstStatLocal* local) override
    {
        for (size_t i = 0; i < local->values.size; ++i)
        {
            AstExpr* value = local->values.data[i];
            if (i == local->values.size - 1)
            {
                if (i < local->values.size)
                {
                    TypePackId valueTypes = lookupPack(value);
                    auto it = begin(valueTypes);
                    for (size_t j = i; j < local->vars.size; ++j)
                    {
                        if (it == end(valueTypes))
                        {
                            break;
                        }

                        AstLocal* var = local->vars.data[i];
                        if (var->annotation)
                        {
                            TypeId varType = lookupAnnotation(var->annotation);
                            if (!isSubtype(*it, varType, ice))
                            {
                                reportError(TypeMismatch{varType, *it}, value->location);
                            }
                        }

                        ++it;
                    }
                }
            }
            else
            {
                TypeId valueType = lookupType(value);
                AstLocal* var = local->vars.data[i];

                if (var->annotation)
                {
                    TypeId varType = lookupAnnotation(var->annotation);
                    if (!isSubtype(varType, valueType, ice))
                    {
                        reportError(TypeMismatch{varType, valueType}, value->location);
                    }
                }
            }
        }

        return true;
    }

    bool visit(AstStatAssign* assign) override
    {
        size_t count = std::min(assign->vars.size, assign->values.size);

        for (size_t i = 0; i < count; ++i)
        {
            AstExpr* lhs = assign->vars.data[i];
            TypeId lhsType = lookupType(lhs);

            AstExpr* rhs = assign->values.data[i];
            TypeId rhsType = lookupType(rhs);

            if (!isSubtype(rhsType, lhsType, ice))
            {
                reportError(TypeMismatch{lhsType, rhsType}, rhs->location);
            }
        }

        return true;
    }

    bool visit(AstStatReturn* ret) override
    {
        Scope* scope = findInnermostScope(ret->location);
        TypePackId expectedRetType = scope->returnType;

        TypeArena arena;
        TypePackId actualRetType = reconstructPack(ret->list, arena);

        UnifierSharedState sharedState{&ice};
        Unifier u{&arena, Mode::Strict, ret->location, Covariant, sharedState};
        u.anyIsTop = true;

        u.tryUnify(actualRetType, expectedRetType);
        const bool ok = u.errors.empty() && u.log.empty();

        if (!ok)
        {
            for (const TypeError& e : u.errors)
                reportError(e);
        }

        return true;
    }

    bool visit(AstExprCall* call) override
    {
        TypeArena arena;
        Instantiation instantiation{TxnLog::empty(), &arena, TypeLevel{}};

        TypePackId expectedRetType = lookupPack(call);
        TypeId functionType = lookupType(call->func);
        TypeId instantiatedFunctionType = instantiation.substitute(functionType).value_or(nullptr);
        LUAU_ASSERT(functionType);

        TypePack args;
        for (const auto& arg : call->args)
        {
            TypeId argTy = module->astTypes[arg];
            LUAU_ASSERT(argTy);
            args.head.push_back(argTy);
        }

        TypePackId argsTp = arena.addTypePack(args);
        FunctionTypeVar ftv{argsTp, expectedRetType};
        TypeId expectedType = arena.addType(ftv);
        if (!isSubtype(expectedType, instantiatedFunctionType, ice))
        {
            unfreeze(module->interfaceTypes);
            CloneState cloneState;
            expectedType = clone(expectedType, module->interfaceTypes, cloneState);
            freeze(module->interfaceTypes);
            reportError(TypeMismatch{expectedType, functionType}, call->location);
        }

        return true;
    }

    bool visit(AstExprFunction* fn) override
    {
        TypeId inferredFnTy = lookupType(fn);
        const FunctionTypeVar* inferredFtv = get<FunctionTypeVar>(inferredFnTy);
        LUAU_ASSERT(inferredFtv);

        auto argIt = begin(inferredFtv->argTypes);
        for (const auto& arg : fn->args)
        {
            if (argIt == end(inferredFtv->argTypes))
                break;

            if (arg->annotation)
            {
                TypeId inferredArgTy = *argIt;
                TypeId annotatedArgTy = lookupAnnotation(arg->annotation);

                if (!isSubtype(annotatedArgTy, inferredArgTy, ice))
                {
                    reportError(TypeMismatch{annotatedArgTy, inferredArgTy}, arg->location);
                }
            }

            ++argIt;
        }

        return true;
    }

    bool visit(AstExprIndexName* indexName) override
    {
        TypeId leftType = lookupType(indexName->expr);
        TypeId resultType = lookupType(indexName);

        // leftType must have a property called indexName->index

        std::optional<TypeId> ty = getIndexTypeFromType(module->getModuleScope(), leftType, indexName->index.value, indexName->location, /* addErrors */ true);
        if (ty)
        {
            if (!isSubtype(resultType, *ty, ice))
            {
                reportError(TypeMismatch{resultType, *ty}, indexName->location);
            }
        }

        return true;
    }

    bool visit(AstExprConstantNumber* number) override
    {
        TypeId actualType = lookupType(number);
        TypeId numberType = getSingletonTypes().numberType;

        if (!isSubtype(numberType, actualType, ice))
        {
            reportError(TypeMismatch{actualType, numberType}, number->location);
        }

        return true;
    }

    bool visit(AstExprConstantString* string) override
    {
        TypeId actualType = lookupType(string);
        TypeId stringType = getSingletonTypes().stringType;

        if (!isSubtype(stringType, actualType, ice))
        {
            reportError(TypeMismatch{actualType, stringType}, string->location);
        }

        return true;
    }

    bool visit(AstExprTypeAssertion* expr) override
    {
        TypeId annotationType = lookupAnnotation(expr->annotation);
        TypeId computedType = lookupType(expr->expr);

        // Note: As an optimization, we try 'number <: number | string' first, as that is the more likely case.
        if (isSubtype(annotationType, computedType, ice))
            return true;

        if (isSubtype(computedType, annotationType, ice))
            return true;

        reportError(TypesAreUnrelated{computedType, annotationType}, expr->location);
        return true;
    }

    /** Extract a TypeId for the first type of the provided pack.
     *
     * Note that this may require modifying some types.  I hope this doesn't cause problems!
     */
    TypeId flattenPack(TypePackId pack)
    {
        pack = follow(pack);

        while (true)
        {
            auto tp = get<TypePack>(pack);
            if (tp && tp->head.empty() && tp->tail)
                pack = *tp->tail;
            else
                break;
        }

        if (auto ty = first(pack))
            return *ty;
        else if (auto vtp = get<VariadicTypePack>(pack))
            return vtp->ty;
        else if (auto ftp = get<FreeTypePack>(pack))
        {
            TypeId result = module->internalTypes.addType(FreeTypeVar{ftp->scope});
            TypePackId freeTail = module->internalTypes.addTypePack(FreeTypePack{ftp->scope});

            TypePack& resultPack = asMutable(pack)->ty.emplace<TypePack>();
            resultPack.head.assign(1, result);
            resultPack.tail = freeTail;

            return result;
        }
        else if (get<Unifiable::Error>(pack))
            return singletonTypes.errorRecoveryType();
        else
            ice.ice("flattenPack got a weird pack!");
    }

    bool visit(AstType* ty) override
    {
        return true;
    }

    bool visit(AstTypeReference* ty) override
    {
        Scope* scope = findInnermostScope(ty->location);
        LUAU_ASSERT(scope);

        // TODO: Imported types

        std::optional<TypeFun> alias = scope->lookupType(ty->name.value);

        if (alias.has_value())
        {
            size_t typesRequired = alias->typeParams.size();
            size_t packsRequired = alias->typePackParams.size();

            bool hasDefaultTypes = std::any_of(alias->typeParams.begin(), alias->typeParams.end(), [](auto&& el) {
                return el.defaultValue.has_value();
            });

            bool hasDefaultPacks = std::any_of(alias->typePackParams.begin(), alias->typePackParams.end(), [](auto&& el) {
                return el.defaultValue.has_value();
            });

            if (!ty->hasParameterList)
            {
                if ((!alias->typeParams.empty() && !hasDefaultTypes) || (!alias->typePackParams.empty() && !hasDefaultPacks))
                {
                    reportError(GenericError{"Type parameter list is required"}, ty->location);
                }
            }

            size_t typesProvided = 0;
            size_t extraTypes = 0;
            size_t packsProvided = 0;

            for (const AstTypeOrPack& p : ty->parameters)
            {
                if (p.type)
                {
                    if (packsProvided != 0)
                    {
                        reportError(GenericError{"Type parameters must come before type pack parameters"}, ty->location);
                    }

                    if (typesProvided < typesRequired)
                    {
                        typesProvided += 1;
                    }
                    else
                    {
                        extraTypes += 1;
                    }
                }
                else if (p.typePack)
                {
                    TypePackId tp = lookupPackAnnotation(p.typePack);

                    if (typesProvided < typesRequired && size(tp) == 1 && finite(tp) && first(tp))
                    {
                        typesProvided += 1;
                    }
                    else
                    {
                        packsProvided += 1;
                    }
                }
            }

            if (extraTypes != 0 && packsProvided == 0)
            {
                packsProvided += 1;
            }

            for (size_t i = typesProvided; i < typesRequired; ++i)
            {
                if (alias->typeParams[i].defaultValue)
                {
                    typesProvided += 1;
                }
            }

            for (size_t i = packsProvided; i < packsProvided; ++i)
            {
                if (alias->typePackParams[i].defaultValue)
                {
                    packsProvided += 1;
                }
            }

            if (extraTypes == 0 && packsProvided + 1 == packsRequired)
            {
                packsProvided += 1;
            }

            if (typesProvided != typesRequired || packsProvided != packsRequired)
            {
                reportError(IncorrectGenericParameterCount{
                                /* name */ ty->name.value,
                                /* typeFun */ *alias,
                                /* actualParameters */ typesProvided,
                                /* actualPackParameters */ packsProvided,
                            },
                    ty->location);
            }
        }
        else
        {
            if (scope->lookupPack(ty->name.value))
            {
                reportError(
                    SwappedGenericTypeParameter{
                        ty->name.value,
                        SwappedGenericTypeParameter::Kind::Type,
                    },
                    ty->location);
            }
            else
            {
                reportError(UnknownSymbol{ty->name.value, UnknownSymbol::Context::Type}, ty->location);
            }
        }

        return true;
    }

    bool visit(AstTypePack*) override
    {
        return true;
    }

    bool visit(AstTypePackGeneric* tp) override
    {
        Scope* scope = findInnermostScope(tp->location);
        LUAU_ASSERT(scope);

        std::optional<TypePackId> alias = scope->lookupPack(tp->genericName.value);
        if (!alias.has_value())
        {
            if (scope->lookupType(tp->genericName.value))
            {
                reportError(
                    SwappedGenericTypeParameter{
                        tp->genericName.value,
                        SwappedGenericTypeParameter::Kind::Pack,
                    },
                    tp->location);
            }
            else
            {
                reportError(UnknownSymbol{tp->genericName.value, UnknownSymbol::Context::Type}, tp->location);
            }
        }

        return true;
    }

    void reportError(TypeErrorData&& data, const Location& location)
    {
        module->errors.emplace_back(location, sourceModule->name, std::move(data));
    }

    void reportError(TypeError e)
    {
        module->errors.emplace_back(std::move(e));
    }

    std::optional<TypeId> getIndexTypeFromType(
        const ScopePtr& scope, TypeId type, const Name& name, const Location& location, bool addErrors)
    {
        type = follow(type);

        if (get<ErrorTypeVar>(type) || get<AnyTypeVar>(type) || get<NeverTypeVar>(type))
            return type;

        if (auto f = get<FreeTypeVar>(type))
            *asMutable(type) = TableTypeVar{TableState::Free, f->level};

        if (isString(type))
        {
            std::optional<TypeId> mtIndex = Luau::findMetatableEntry(module->errors, singletonTypes.stringType, "__index", location);
            LUAU_ASSERT(mtIndex);
            type = *mtIndex;
        }

        if (TableTypeVar* tableType = getMutableTableType(type))
        {

            return findTablePropertyRespectingMeta(module->errors, type, name, location);
        }
        else if (const ClassTypeVar* cls = get<ClassTypeVar>(type))
        {
            const Property* prop = lookupClassProp(cls, name);
            if (prop)
                return prop->type;
        }
        else if (const UnionTypeVar* utv = get<UnionTypeVar>(type))
        {
            std::vector<TypeId> goodOptions;
            std::vector<TypeId> badOptions;

            for (TypeId t : utv)
            {
                // TODO: we should probably limit recursion here?
                // RecursionLimiter _rl(&recursionCount, FInt::LuauTypeInferRecursionLimit);

                // Not needed when we normalize types.
                if (get<AnyTypeVar>(follow(t)))
                    return t;

                if (std::optional<TypeId> ty = getIndexTypeFromType(scope, t, name, location, /* addErrors= */ false))
                    goodOptions.push_back(*ty);
                else
                    badOptions.push_back(t);
            }

            if (!badOptions.empty())
            {
                if (addErrors)
                {
                    if (goodOptions.empty())
                        reportError(UnknownProperty{type, name}, location);
                    else
                        reportError(MissingUnionProperty{type, badOptions, name}, location);
                }
                return std::nullopt;
            }

            std::vector<TypeId> result = reduceUnion(goodOptions);
            if (result.empty())
                return singletonTypes.neverType;

            if (result.size() == 1)
                return result[0];

            return module->internalTypes.addType(UnionTypeVar{std::move(result)});
        }
        else if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(type))
        {
            std::vector<TypeId> parts;

            for (TypeId t : itv->parts)
            {
                // TODO: we should probably limit recursion here?
                // RecursionLimiter _rl(&recursionCount, FInt::LuauTypeInferRecursionLimit);

                if (std::optional<TypeId> ty = getIndexTypeFromType(scope, t, name, location, /* addErrors= */ false))
                    parts.push_back(*ty);
            }

            // If no parts of the intersection had the property we looked up for, it never existed at all.
            if (parts.empty())
            {
                if (addErrors)
                    reportError(UnknownProperty{type, name}, location);
                return std::nullopt;
            }

            if (parts.size() == 1)
                return parts[0];

            return module->internalTypes.addType(IntersectionTypeVar{std::move(parts)}); // Not at all correct.
        }

        if (addErrors)
            reportError(UnknownProperty{type, name}, location);

        return std::nullopt;
    }

    std::vector<TypeId> reduceUnion(const std::vector<TypeId>& types)
    {
        std::vector<TypeId> result;
        for (TypeId t : types)
        {
            t = follow(t);
            if (get<NeverTypeVar>(t))
                continue;

            if (get<ErrorTypeVar>(t) || get<AnyTypeVar>(t))
                return {t};

            if (const UnionTypeVar* utv = get<UnionTypeVar>(t))
            {
                for (TypeId ty : utv)
                {
                    ty = follow(ty);
                    if (get<NeverTypeVar>(ty))
                        continue;
                    if (get<ErrorTypeVar>(ty) || get<AnyTypeVar>(ty))
                        return {ty};

                    if (result.end() == std::find(result.begin(), result.end(), ty))
                        result.push_back(ty);
                }
            }
            else if (std::find(result.begin(), result.end(), t) == result.end())
                result.push_back(t);
        }

        return result;
    }
};

void check(const SourceModule& sourceModule, Module* module)
{
    TypeChecker2 typeChecker{&sourceModule, module};

    sourceModule.root->visit(&typeChecker);
}

} // namespace Luau
