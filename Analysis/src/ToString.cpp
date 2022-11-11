// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ToString.h"

#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"

#include <algorithm>
#include <stdexcept>

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)
LUAU_FASTFLAG(LuauLvaluelessPath)
LUAU_FASTFLAG(LuauUnknownAndNeverType)
LUAU_FASTFLAGVARIABLE(LuauFunctionReturnStringificationFixup, false)
LUAU_FASTFLAGVARIABLE(LuauUnseeArrayTtv, false)

/*
 * Prefix generic typenames with gen-
 * Additionally, free types will be prefixed with free- and suffixed with their level.  eg free-a-4
 * Fair warning: Setting this will break a lot of Luau unit tests.
 */
LUAU_FASTFLAGVARIABLE(DebugLuauVerboseTypeNames, false)

namespace Luau
{

namespace
{

struct FindCyclicTypes final : TypeVarVisitor
{
    FindCyclicTypes() = default;
    FindCyclicTypes(const FindCyclicTypes&) = delete;
    FindCyclicTypes& operator=(const FindCyclicTypes&) = delete;

    bool exhaustive = false;
    std::unordered_set<TypeId> visited;
    std::unordered_set<TypePackId> visitedPacks;
    std::set<TypeId> cycles;
    std::set<TypePackId> cycleTPs;

    void cycle(TypeId ty) override
    {
        cycles.insert(ty);
    }

    void cycle(TypePackId tp) override
    {
        cycleTPs.insert(tp);
    }

    bool visit(TypeId ty) override
    {
        return visited.insert(ty).second;
    }

    bool visit(TypePackId tp) override
    {
        return visitedPacks.insert(tp).second;
    }

    bool visit(TypeId ty, const TableTypeVar& ttv) override
    {
        if (!visited.insert(ty).second)
            return false;

        if (ttv.name || ttv.syntheticName)
        {
            for (TypeId itp : ttv.instantiatedTypeParams)
                traverse(itp);

            for (TypePackId itp : ttv.instantiatedTypePackParams)
                traverse(itp);

            return exhaustive;
        }

        return true;
    }

    bool visit(TypeId ty, const ClassTypeVar&) override
    {
        return false;
    }
};

template<typename TID>
void findCyclicTypes(std::set<TypeId>& cycles, std::set<TypePackId>& cycleTPs, TID ty, bool exhaustive)
{
    FindCyclicTypes fct;
    fct.exhaustive = exhaustive;
    fct.traverse(ty);

    cycles = std::move(fct.cycles);
    cycleTPs = std::move(fct.cycleTPs);
}

} // namespace

static std::pair<bool, std::optional<Luau::Name>> canUseTypeNameInScope(ScopePtr scope, const std::string& name)
{
    for (ScopePtr curr = scope; curr; curr = curr->parent)
    {
        for (const auto& [importName, nameTable] : curr->importedTypeBindings)
        {
            if (nameTable.count(name))
                return {true, importName};
        }

        if (curr->exportedTypeBindings.count(name))
            return {true, std::nullopt};
    }

    return {false, std::nullopt};
}

struct StringifierState
{
    ToStringOptions& opts;
    ToStringResult& result;

    std::unordered_map<TypeId, std::string> cycleNames;
    std::unordered_map<TypePackId, std::string> cycleTpNames;
    std::unordered_set<void*> seen;
    std::unordered_set<std::string> usedNames;
    size_t indentation = 0;

    bool exhaustive;

    StringifierState(ToStringOptions& opts, ToStringResult& result)
        : opts(opts)
        , result(result)
        , exhaustive(opts.exhaustive)
    {
        for (const auto& [_, v] : opts.nameMap.typeVars)
            usedNames.insert(v);
        for (const auto& [_, v] : opts.nameMap.typePacks)
            usedNames.insert(v);
    }

    bool hasSeen(const void* tv)
    {
        void* ttv = const_cast<void*>(tv);
        if (seen.find(ttv) != seen.end())
            return true;

        seen.insert(ttv);
        return false;
    }

    void unsee(const void* tv)
    {
        void* ttv = const_cast<void*>(tv);
        auto iter = seen.find(ttv);
        if (iter != seen.end())
            seen.erase(iter);
    }

    std::string getName(TypeId ty)
    {
        const size_t s = opts.nameMap.typeVars.size();
        std::string& n = opts.nameMap.typeVars[ty];
        if (!n.empty())
            return n;

        for (int count = 0; count < 256; ++count)
        {
            std::string candidate = generateName(usedNames.size() + count);
            if (!usedNames.count(candidate))
            {
                usedNames.insert(candidate);
                n = candidate;
                return candidate;
            }
        }

        return generateName(s);
    }

    int previousNameIndex = 0;

    std::string getName(TypePackId ty)
    {
        const size_t s = opts.nameMap.typePacks.size();
        std::string& n = opts.nameMap.typePacks[ty];
        if (!n.empty())
            return n;

        for (int count = 0; count < 256; ++count)
        {
            std::string candidate = generateName(previousNameIndex + count);
            if (!usedNames.count(candidate))
            {
                previousNameIndex += count;
                usedNames.insert(candidate);
                n = candidate;
                return candidate;
            }
        }

        return generateName(s);
    }

    void emit(const std::string& s)
    {
        if (opts.maxTypeLength > 0 && result.name.length() > opts.maxTypeLength)
            return;

        result.name += s;
    }

    void emitLevel(Scope* scope)
    {
        size_t count = 0;
        for (Scope* s = scope; s; s = s->parent.get())
            ++count;

        emit(count);
        emit("-");
        char buffer[16];
        uint32_t s = uint32_t(intptr_t(scope) & 0xFFFFFF);
        snprintf(buffer, sizeof(buffer), "0x%x", s);
        emit(buffer);
    }

    void emit(TypeLevel level)
    {
        emit(std::to_string(level.level));
        emit("-");
        emit(std::to_string(level.subLevel));
    }

    void emit(const char* s)
    {
        if (opts.maxTypeLength > 0 && result.name.length() > opts.maxTypeLength)
            return;

        result.name += s;
    }

    void emit(int i)
    {
        emit(std::to_string(i).c_str());
    }

    void emit(size_t i)
    {
        emit(std::to_string(i).c_str());
    }

    void indent()
    {
        indentation += 4;
    }

    void dedent()
    {
        indentation -= 4;
    }

    void newline()
    {
        if (!opts.useLineBreaks)
            return emit(" ");

        emit("\n");
        emitIndentation();
    }

private:
    void emitIndentation()
    {
        if (!opts.indent)
            return;

        emit(std::string(indentation, ' '));
    }
};

struct TypeVarStringifier
{
    StringifierState& state;

    explicit TypeVarStringifier(StringifierState& state)
        : state(state)
    {
    }

    void stringify(TypeId tv)
    {
        if (state.opts.maxTypeLength > 0 && state.result.name.length() > state.opts.maxTypeLength)
            return;

        if (tv->ty.valueless_by_exception())
        {
            state.result.error = true;
            state.emit("* VALUELESS BY EXCEPTION *");
            return;
        }

        auto it = state.cycleNames.find(tv);
        if (it != state.cycleNames.end())
        {
            state.emit(it->second);
            return;
        }

        Luau::visit(
            [this, tv](auto&& t)
            {
                return (*this)(tv, t);
            },
            tv->ty);
    }

    void stringify(TypePackId tp);
    void stringify(TypePackId tpid, const std::vector<std::optional<FunctionArgument>>& names);

    void stringify(const std::vector<TypeId>& types, const std::vector<TypePackId>& typePacks)
    {
        if (types.size() == 0 && typePacks.size() == 0)
            return;

        if (types.size() || typePacks.size())
            state.emit("<");

        bool first = true;

        for (TypeId ty : types)
        {
            if (!first)
                state.emit(", ");
            first = false;

            stringify(ty);
        }

        bool singleTp = typePacks.size() == 1;

        for (TypePackId tp : typePacks)
        {
            if (isEmpty(tp) && singleTp)
                continue;

            if (!first)
                state.emit(", ");
            else
                first = false;

            bool wrap = !singleTp && get<TypePack>(follow(tp));

            if (wrap)
                state.emit("(");

            stringify(tp);

            if (wrap)
                state.emit(")");
        }

        if (types.size() || typePacks.size())
            state.emit(">");
    }

    void operator()(TypeId ty, const Unifiable::Free& ftv)
    {
        state.result.invalid = true;
        if (FFlag::DebugLuauVerboseTypeNames)
            state.emit("free-");
        state.emit(state.getName(ty));

        if (FFlag::DebugLuauVerboseTypeNames)
        {
            state.emit("-");
            if (FFlag::DebugLuauDeferredConstraintResolution)
                state.emitLevel(ftv.scope);
            else
                state.emit(ftv.level);
        }
    }

    void operator()(TypeId, const BoundTypeVar& btv)
    {
        stringify(btv.boundTo);
    }

    void operator()(TypeId ty, const GenericTypeVar& gtv)
    {
        if (gtv.explicitName)
        {
            state.usedNames.insert(gtv.name);
            state.opts.nameMap.typeVars[ty] = gtv.name;
            state.emit(gtv.name);
        }
        else
            state.emit(state.getName(ty));

        if (FFlag::DebugLuauVerboseTypeNames)
        {
            state.emit("-");
            if (FFlag::DebugLuauDeferredConstraintResolution)
                state.emitLevel(gtv.scope);
            else
                state.emit(gtv.level);
        }
    }

    void operator()(TypeId, const BlockedTypeVar& btv)
    {
        state.emit("*blocked-");
        state.emit(btv.index);
        state.emit("*");
    }

    void operator()(TypeId ty, const PendingExpansionTypeVar& petv)
    {
        state.emit("*pending-expansion-");
        state.emit(petv.index);
        state.emit("*");
    }

    void operator()(TypeId, const PrimitiveTypeVar& ptv)
    {
        switch (ptv.type)
        {
        case PrimitiveTypeVar::NilType:
            state.emit("nil");
            return;
        case PrimitiveTypeVar::Boolean:
            state.emit("boolean");
            return;
        case PrimitiveTypeVar::Number:
            state.emit("number");
            return;
        case PrimitiveTypeVar::String:
            state.emit("string");
            return;
        case PrimitiveTypeVar::Thread:
            state.emit("thread");
            return;
        case PrimitiveTypeVar::Function:
            state.emit("function");
            return;
        default:
            LUAU_ASSERT(!"Unknown primitive type");
            throwRuntimeError("Unknown primitive type " + std::to_string(ptv.type));
        }
    }

    void operator()(TypeId, const SingletonTypeVar& stv)
    {
        if (const BooleanSingleton* bs = Luau::get<BooleanSingleton>(&stv))
            state.emit(bs->value ? "true" : "false");
        else if (const StringSingleton* ss = Luau::get<StringSingleton>(&stv))
        {
            state.emit("\"");
            state.emit(escape(ss->value));
            state.emit("\"");
        }
        else
        {
            LUAU_ASSERT(!"Unknown singleton type");
            throwRuntimeError("Unknown singleton type");
        }
    }

    void operator()(TypeId, const FunctionTypeVar& ftv)
    {
        if (state.hasSeen(&ftv))
        {
            state.result.cycle = true;
            state.emit("*CYCLE*");
            return;
        }

        // We should not be respecting opts.hideNamedFunctionTypeParameters here.
        if (ftv.generics.size() > 0 || ftv.genericPacks.size() > 0)
        {
            state.emit("<");
            bool comma = false;
            for (auto it = ftv.generics.begin(); it != ftv.generics.end(); ++it)
            {
                if (comma)
                    state.emit(", ");
                comma = true;
                stringify(*it);
            }
            for (auto it = ftv.genericPacks.begin(); it != ftv.genericPacks.end(); ++it)
            {
                if (comma)
                    state.emit(", ");
                comma = true;
                stringify(*it);
            }
            state.emit(">");
        }

        state.emit("(");

        if (state.opts.functionTypeArguments)
            stringify(ftv.argTypes, ftv.argNames);
        else
            stringify(ftv.argTypes);

        state.emit(") -> ");

        bool plural = true;

        if (FFlag::LuauFunctionReturnStringificationFixup)
        {
            auto retBegin = begin(ftv.retTypes);
            auto retEnd = end(ftv.retTypes);
            if (retBegin != retEnd)
            {
                ++retBegin;
                if (retBegin == retEnd && !retBegin.tail())
                    plural = false;
            }
        }
        else
        {
            if (auto retPack = get<TypePack>(follow(ftv.retTypes)))
            {
                if (retPack->head.size() == 1 && !retPack->tail)
                    plural = false;
            }
        }

        if (plural)
            state.emit("(");

        stringify(ftv.retTypes);

        if (plural)
            state.emit(")");

        state.unsee(&ftv);
    }

    void operator()(TypeId, const TableTypeVar& ttv)
    {
        if (ttv.boundTo)
            return stringify(*ttv.boundTo);

        if (!state.exhaustive)
        {
            if (ttv.name)
            {
                // If scope if provided, add module name and check visibility
                if (state.opts.scope)
                {
                    auto [success, moduleName] = canUseTypeNameInScope(state.opts.scope, *ttv.name);

                    if (!success)
                        state.result.invalid = true;

                    if (moduleName)
                    {
                        state.emit(*moduleName);
                        state.emit(".");
                    }
                }

                state.emit(*ttv.name);
                stringify(ttv.instantiatedTypeParams, ttv.instantiatedTypePackParams);
                return;
            }
            if (ttv.syntheticName)
            {
                state.result.invalid = true;
                state.emit(*ttv.syntheticName);
                stringify(ttv.instantiatedTypeParams, ttv.instantiatedTypePackParams);
                return;
            }
        }

        if (state.hasSeen(&ttv))
        {
            state.result.cycle = true;
            state.emit("*CYCLE*");
            return;
        }

        std::string openbrace = "@@@";
        std::string closedbrace = "@@@?!";
        switch (state.opts.hideTableKind ? TableState::Unsealed : ttv.state)
        {
        case TableState::Sealed:
            state.result.invalid = true;
            openbrace = "{|";
            closedbrace = "|}";
            break;
        case TableState::Unsealed:
            openbrace = "{";
            closedbrace = "}";
            break;
        case TableState::Free:
            state.result.invalid = true;
            openbrace = "{-";
            closedbrace = "-}";
            break;
        case TableState::Generic:
            state.result.invalid = true;
            openbrace = "{+";
            closedbrace = "+}";
            break;
        }

        // If this appears to be an array, we want to stringify it using the {T} syntax.
        if (ttv.indexer && ttv.props.empty() && isNumber(ttv.indexer->indexType))
        {
            state.emit("{");
            stringify(ttv.indexer->indexResultType);
            state.emit("}");

            if (FFlag::LuauUnseeArrayTtv)
                state.unsee(&ttv);

            return;
        }

        state.emit(openbrace);
        state.indent();

        bool comma = false;
        if (ttv.indexer)
        {
            state.newline();
            state.emit("[");
            stringify(ttv.indexer->indexType);
            state.emit("]: ");
            stringify(ttv.indexer->indexResultType);
            comma = true;
        }

        size_t index = 0;
        size_t oldLength = state.result.name.length();
        for (const auto& [name, prop] : ttv.props)
        {
            if (comma)
            {
                state.emit(",");
                state.newline();
            }
            else
                state.newline();

            size_t length = state.result.name.length() - oldLength;

            if (state.opts.maxTableLength > 0 && (length - 2 * index) >= state.opts.maxTableLength)
            {
                state.emit("... ");
                state.emit(std::to_string(ttv.props.size() - index));
                state.emit(" more ...");
                break;
            }

            if (isIdentifier(name))
                state.emit(name);
            else
            {
                state.emit("[\"");
                state.emit(escape(name));
                state.emit("\"]");
            }
            state.emit(": ");
            stringify(prop.type);
            comma = true;
            ++index;
        }

        state.dedent();
        if (comma)
            state.newline();
        else
            state.emit("  ");
        state.emit(closedbrace);

        state.unsee(&ttv);
    }

    void operator()(TypeId, const MetatableTypeVar& mtv)
    {
        state.result.invalid = true;
        if (!state.exhaustive && mtv.syntheticName)
        {
            state.emit(*mtv.syntheticName);
            return;
        }

        state.emit("{ @metatable ");
        stringify(mtv.metatable);
        state.emit(",");
        state.newline();
        stringify(mtv.table);
        state.emit(" }");
    }

    void operator()(TypeId, const ClassTypeVar& ctv)
    {
        state.emit(ctv.name);
    }

    void operator()(TypeId, const AnyTypeVar&)
    {
        state.emit("any");
    }

    void operator()(TypeId, const UnionTypeVar& uv)
    {
        if (state.hasSeen(&uv))
        {
            state.result.cycle = true;
            state.emit("*CYCLE*");
            return;
        }

        bool optional = false;

        std::vector<std::string> results = {};
        for (auto el : &uv)
        {
            el = follow(el);

            if (isNil(el))
            {
                optional = true;
                continue;
            }

            std::string saved = std::move(state.result.name);

            bool needParens = !state.cycleNames.count(el) && (get<IntersectionTypeVar>(el) || get<FunctionTypeVar>(el));

            if (needParens)
                state.emit("(");

            stringify(el);

            if (needParens)
                state.emit(")");

            results.push_back(std::move(state.result.name));
            state.result.name = std::move(saved);
        }

        state.unsee(&uv);

        std::sort(results.begin(), results.end());

        if (optional && results.size() > 1)
            state.emit("(");

        bool first = true;
        for (std::string& ss : results)
        {
            if (!first)
            {
                state.newline();
                state.emit("| ");
            }
            state.emit(ss);
            first = false;
        }

        if (optional)
        {
            const char* s = "?";
            if (results.size() > 1)
                s = ")?";

            state.emit(s);
        }
    }

    void operator()(TypeId, const IntersectionTypeVar& uv)
    {
        if (state.hasSeen(&uv))
        {
            state.result.cycle = true;
            state.emit("*CYCLE*");
            return;
        }

        std::vector<std::string> results = {};
        for (auto el : uv.parts)
        {
            el = follow(el);

            std::string saved = std::move(state.result.name);

            bool needParens = !state.cycleNames.count(el) && (get<UnionTypeVar>(el) || get<FunctionTypeVar>(el));

            if (needParens)
                state.emit("(");

            stringify(el);

            if (needParens)
                state.emit(")");

            results.push_back(std::move(state.result.name));
            state.result.name = std::move(saved);
        }

        state.unsee(&uv);

        std::sort(results.begin(), results.end());

        bool first = true;
        for (std::string& ss : results)
        {
            if (!first)
            {
                state.newline();
                state.emit("& ");
            }
            state.emit(ss);
            first = false;
        }
    }

    void operator()(TypeId, const ErrorTypeVar& tv)
    {
        state.result.error = true;
        state.emit(FFlag::LuauUnknownAndNeverType ? "*error-type*" : "*unknown*");
    }

    void operator()(TypeId, const LazyTypeVar& ltv)
    {
        state.result.invalid = true;
        state.emit("lazy?");
    }

    void operator()(TypeId, const UnknownTypeVar& ttv)
    {
        state.emit("unknown");
    }

    void operator()(TypeId, const NeverTypeVar& ttv)
    {
        state.emit("never");
    }

    void operator()(TypeId, const NegationTypeVar& ntv)
    {
        state.emit("~");

        // The precedence of `~` should be less than `|` and `&`.
        TypeId followed = follow(ntv.ty);
        bool parens = get<UnionTypeVar>(followed) || get<IntersectionTypeVar>(followed);

        if (parens)
            state.emit("(");

        stringify(ntv.ty);

        if (parens)
            state.emit(")");
    }
};

struct TypePackStringifier
{
    StringifierState& state;

    const std::vector<std::optional<FunctionArgument>> elemNames;
    static inline const std::vector<std::optional<FunctionArgument>> dummyElemNames = {};
    unsigned elemIndex = 0;

    explicit TypePackStringifier(StringifierState& state, const std::vector<std::optional<FunctionArgument>>& elemNames)
        : state(state)
        , elemNames(elemNames)
    {
    }

    explicit TypePackStringifier(StringifierState& state)
        : state(state)
        , elemNames(dummyElemNames)
    {
    }

    void stringify(TypeId tv)
    {
        TypeVarStringifier tvs{state};
        tvs.stringify(tv);
    }

    void stringify(TypePackId tp)
    {
        if (state.opts.maxTypeLength > 0 && state.result.name.length() > state.opts.maxTypeLength)
            return;

        if (tp->ty.valueless_by_exception())
        {
            state.result.error = true;
            state.emit("* VALUELESS TP BY EXCEPTION *");
            return;
        }

        auto it = state.cycleTpNames.find(tp);
        if (it != state.cycleTpNames.end())
        {
            state.emit(it->second);
            return;
        }

        Luau::visit(
            [this, tp](auto&& t)
            {
                return (*this)(tp, t);
            },
            tp->ty);
    }

    void operator()(TypePackId, const TypePack& tp)
    {
        if (state.hasSeen(&tp))
        {
            state.result.cycle = true;
            state.emit("*CYCLETP*");
            return;
        }

        bool first = true;

        for (const auto& typeId : tp.head)
        {
            if (first)
                first = false;
            else
                state.emit(", ");

            // Do not respect opts.namedFunctionOverrideArgNames here
            if (elemIndex < elemNames.size() && elemNames[elemIndex])
            {
                state.emit(elemNames[elemIndex]->name);
                state.emit(": ");
            }

            elemIndex++;

            stringify(typeId);
        }

        if (tp.tail && !isEmpty(*tp.tail))
        {
            TypePackId tail = follow(*tp.tail);
            if (auto vtp = get<VariadicTypePack>(tail); !vtp || (!FFlag::DebugLuauVerboseTypeNames && !vtp->hidden))
            {
                if (first)
                    first = false;
                else
                    state.emit(", ");

                stringify(tail);
            }
        }

        state.unsee(&tp);
    }

    void operator()(TypePackId, const Unifiable::Error& error)
    {
        state.result.error = true;
        state.emit(FFlag::LuauUnknownAndNeverType ? "*error-type*" : "*unknown*");
    }

    void operator()(TypePackId, const VariadicTypePack& pack)
    {
        state.emit("...");
        if (FFlag::DebugLuauVerboseTypeNames && pack.hidden)
        {
            state.emit("*hidden*");
        }
        stringify(pack.ty);
    }

    void operator()(TypePackId tp, const GenericTypePack& pack)
    {
        if (FFlag::DebugLuauVerboseTypeNames)
            state.emit("gen-");
        if (pack.explicitName)
        {
            state.usedNames.insert(pack.name);
            state.opts.nameMap.typePacks[tp] = pack.name;
            state.emit(pack.name);
        }
        else
        {
            state.emit(state.getName(tp));
        }
        state.emit("...");
    }

    void operator()(TypePackId tp, const FreeTypePack& pack)
    {
        state.result.invalid = true;
        if (FFlag::DebugLuauVerboseTypeNames)
            state.emit("free-");
        state.emit(state.getName(tp));

        if (FFlag::DebugLuauVerboseTypeNames)
        {
            state.emit("-");
            if (FFlag::DebugLuauDeferredConstraintResolution)
                state.emitLevel(pack.scope);
            else
                state.emit(pack.level);
        }

        state.emit("...");
    }

    void operator()(TypePackId, const BoundTypePack& btv)
    {
        stringify(btv.boundTo);
    }

    void operator()(TypePackId, const BlockedTypePack& btp)
    {
        state.emit("*blocked-tp-");
        state.emit(btp.index);
        state.emit("*");
    }
};

void TypeVarStringifier::stringify(TypePackId tp)
{
    TypePackStringifier tps(state);
    tps.stringify(tp);
}

void TypeVarStringifier::stringify(TypePackId tpid, const std::vector<std::optional<FunctionArgument>>& names)
{
    TypePackStringifier tps(state, names);
    tps.stringify(tpid);
}

static void assignCycleNames(const std::set<TypeId>& cycles, const std::set<TypePackId>& cycleTPs,
    std::unordered_map<TypeId, std::string>& cycleNames, std::unordered_map<TypePackId, std::string>& cycleTpNames, bool exhaustive)
{
    int nextIndex = 1;

    for (TypeId cycleTy : cycles)
    {
        std::string name;

        // TODO: use the stringified type list if there are no cycles
        if (auto ttv = get<TableTypeVar>(follow(cycleTy)); !exhaustive && ttv && (ttv->syntheticName || ttv->name))
        {
            // If we have a cycle type in type parameters, assign a cycle name for this named table
            if (std::find_if(ttv->instantiatedTypeParams.begin(), ttv->instantiatedTypeParams.end(),
                    [&](auto&& el)
                    {
                        return cycles.count(follow(el));
                    }) != ttv->instantiatedTypeParams.end())
                cycleNames[cycleTy] = ttv->name ? *ttv->name : *ttv->syntheticName;

            continue;
        }

        name = "t" + std::to_string(nextIndex);
        ++nextIndex;

        cycleNames[cycleTy] = std::move(name);
    }

    for (TypePackId tp : cycleTPs)
    {
        std::string name = "tp" + std::to_string(nextIndex);
        ++nextIndex;
        cycleTpNames[tp] = std::move(name);
    }
}

ToStringResult toStringDetailed(TypeId ty, ToStringOptions& opts)
{
    /*
     * 1. Walk the TypeVar and track seen TypeIds.  When you reencounter a TypeId, add it to a set of seen cycles.
     * 2. Generate some names for each cycle.  For a starting point, we can just call them t0, t1 and so on.
     * 3. For each seen cycle, stringify it like we do now, but replace each known cycle with its name.
     * 4. Print out the root of the type using the same algorithm as step 3.
     */
    ty = follow(ty);

    ToStringResult result;

    StringifierState state{opts, result};

    std::set<TypeId> cycles;
    std::set<TypePackId> cycleTPs;

    findCyclicTypes(cycles, cycleTPs, ty, opts.exhaustive);

    assignCycleNames(cycles, cycleTPs, state.cycleNames, state.cycleTpNames, opts.exhaustive);

    TypeVarStringifier tvs{state};

    if (!opts.exhaustive)
    {
        if (auto ttv = get<TableTypeVar>(ty); ttv && (ttv->name || ttv->syntheticName))
        {
            if (ttv->syntheticName)
                result.invalid = true;

            // If scope if provided, add module name and check visibility
            if (ttv->name && opts.scope)
            {
                auto [success, moduleName] = canUseTypeNameInScope(opts.scope, *ttv->name);

                if (!success)
                    result.invalid = true;

                if (moduleName)
                    result.name = format("%s.", moduleName->c_str());
            }

            result.name += ttv->name ? *ttv->name : *ttv->syntheticName;

            tvs.stringify(ttv->instantiatedTypeParams, ttv->instantiatedTypePackParams);

            return result;
        }
        else if (auto mtv = get<MetatableTypeVar>(ty); mtv && mtv->syntheticName)
        {
            result.invalid = true;
            result.name = *mtv->syntheticName;
            return result;
        }
    }

    /* If the root itself is a cycle, we special case a little.
     * We go out of our way to print the following:
     *
     * t1 where t1 = the_whole_root_type
     */
    auto it = state.cycleNames.find(ty);
    if (it != state.cycleNames.end())
        state.emit(it->second);
    else
        tvs.stringify(ty);

    if (!state.cycleNames.empty())
    {
        result.cycle = true;
        state.emit(" where ");
    }

    state.exhaustive = true;

    std::vector<std::pair<TypeId, std::string>> sortedCycleNames{state.cycleNames.begin(), state.cycleNames.end()};
    std::sort(sortedCycleNames.begin(), sortedCycleNames.end(),
        [](const auto& a, const auto& b)
        {
            return a.second < b.second;
        });

    bool semi = false;
    for (const auto& [cycleTy, name] : sortedCycleNames)
    {
        if (semi)
            state.emit(" ; ");

        state.emit(name);
        state.emit(" = ");
        Luau::visit(
            [&tvs, cycleTy = cycleTy](auto&& t)
            {
                return tvs(cycleTy, t);
            },
            cycleTy->ty);

        semi = true;
    }

    if (opts.maxTypeLength > 0 && result.name.length() > opts.maxTypeLength)
    {
        result.truncated = true;

        result.name += "... *TRUNCATED*";
    }

    return result;
}

ToStringResult toStringDetailed(TypePackId tp, ToStringOptions& opts)
{
    /*
     * 1. Walk the TypeVar and track seen TypeIds.  When you reencounter a TypeId, add it to a set of seen cycles.
     * 2. Generate some names for each cycle.  For a starting point, we can just call them t0, t1 and so on.
     * 3. For each seen cycle, stringify it like we do now, but replace each known cycle with its name.
     * 4. Print out the root of the type using the same algorithm as step 3.
     */
    ToStringResult result;
    StringifierState state{opts, result};

    std::set<TypeId> cycles;
    std::set<TypePackId> cycleTPs;

    findCyclicTypes(cycles, cycleTPs, tp, opts.exhaustive);

    assignCycleNames(cycles, cycleTPs, state.cycleNames, state.cycleTpNames, opts.exhaustive);

    TypeVarStringifier tvs{state};

    /* If the root itself is a cycle, we special case a little.
     * We go out of our way to print the following:
     *
     * t1 where t1 = the_whole_root_type
     */
    auto it = state.cycleTpNames.find(tp);
    if (it != state.cycleTpNames.end())
        state.emit(it->second);
    else
        tvs.stringify(tp);

    if (!cycles.empty())
    {
        result.cycle = true;
        state.emit(" where ");
    }

    state.exhaustive = true;

    std::vector<std::pair<TypeId, std::string>> sortedCycleNames{state.cycleNames.begin(), state.cycleNames.end()};
    std::sort(sortedCycleNames.begin(), sortedCycleNames.end(),
        [](const auto& a, const auto& b)
        {
            return a.second < b.second;
        });

    bool semi = false;
    for (const auto& [cycleTy, name] : sortedCycleNames)
    {
        if (semi)
            state.emit(" ; ");

        state.emit(name);
        state.emit(" = ");
        Luau::visit(
            [&tvs, cycleTy = cycleTy](auto t)
            {
                return tvs(cycleTy, t);
            },
            cycleTy->ty);

        semi = true;
    }

    if (opts.maxTypeLength > 0 && result.name.length() > opts.maxTypeLength)
    {
        result.name += "... *TRUNCATED*";
    }

    return result;
}

std::string toString(TypeId ty, ToStringOptions& opts)
{
    return toStringDetailed(ty, opts).name;
}

std::string toString(TypePackId tp, ToStringOptions& opts)
{
    return toStringDetailed(tp, opts).name;
}

std::string toString(const TypeVar& tv, ToStringOptions& opts)
{
    return toString(const_cast<TypeId>(&tv), opts);
}

std::string toString(const TypePackVar& tp, ToStringOptions& opts)
{
    return toString(const_cast<TypePackId>(&tp), opts);
}

std::string toStringNamedFunction(const std::string& funcName, const FunctionTypeVar& ftv, ToStringOptions& opts)
{
    ToStringResult result;
    StringifierState state{opts, result};
    TypeVarStringifier tvs{state};

    state.emit(funcName);

    if (!opts.hideNamedFunctionTypeParameters)
        tvs.stringify(ftv.generics, ftv.genericPacks);

    state.emit("(");

    auto argPackIter = begin(ftv.argTypes);

    bool first = true;
    size_t idx = 0;
    while (argPackIter != end(ftv.argTypes))
    {
        // ftv takes a self parameter as the first argument, skip it if specified in option
        if (idx == 0 && ftv.hasSelf && opts.hideFunctionSelfArgument)
        {
            ++argPackIter;
            ++idx;
            continue;
        }

        if (!first)
            state.emit(", ");
        first = false;

        // We don't respect opts.functionTypeArguments
        if (idx < opts.namedFunctionOverrideArgNames.size())
        {
            state.emit(opts.namedFunctionOverrideArgNames[idx] + ": ");
        }
        else if (idx < ftv.argNames.size() && ftv.argNames[idx])
        {
            state.emit(ftv.argNames[idx]->name + ": ");
        }
        else
        {
            state.emit("_: ");
        }
        tvs.stringify(*argPackIter);

        ++argPackIter;
        ++idx;
    }

    if (argPackIter.tail())
    {
        if (auto vtp = get<VariadicTypePack>(*argPackIter.tail()); !vtp || !vtp->hidden)
        {
            if (!first)
                state.emit(", ");

            state.emit("...: ");

            if (vtp)
                tvs.stringify(vtp->ty);
            else
                tvs.stringify(*argPackIter.tail());
        }
    }

    state.emit("): ");

    size_t retSize = size(ftv.retTypes);
    bool hasTail = !finite(ftv.retTypes);
    bool wrap = get<TypePack>(follow(ftv.retTypes)) && (hasTail ? retSize != 0 : retSize != 1);

    if (wrap)
        state.emit("(");

    tvs.stringify(ftv.retTypes);

    if (wrap)
        state.emit(")");

    return result.name;
}

std::string dump(TypeId ty)
{
    ToStringOptions opts;
    opts.exhaustive = true;
    opts.functionTypeArguments = true;
    std::string s = toString(ty, opts);
    printf("%s\n", s.c_str());
    return s;
}

std::string dump(TypePackId ty)
{
    ToStringOptions opts;
    opts.exhaustive = true;
    opts.functionTypeArguments = true;
    std::string s = toString(ty, opts);
    printf("%s\n", s.c_str());
    return s;
}

std::string dump(const ScopePtr& scope, const char* name)
{
    auto binding = scope->linearSearchForBinding(name);
    if (!binding)
    {
        printf("No binding %s\n", name);
        return {};
    }

    TypeId ty = binding->typeId;
    ToStringOptions opts;
    opts.exhaustive = true;
    opts.functionTypeArguments = true;
    std::string s = toString(ty, opts);
    printf("%s\n", s.c_str());
    return s;
}

std::string generateName(size_t i)
{
    std::string n;
    n = char('a' + i % 26);
    if (i >= 26)
        n += std::to_string(i / 26);
    return n;
}

std::string toString(const Constraint& constraint, ToStringOptions& opts)
{
    auto go = [&opts](auto&& c) -> std::string {
        using T = std::decay_t<decltype(c)>;

        auto tos = [&opts](auto&& a)
        {
            return toString(a, opts);
        };

        if constexpr (std::is_same_v<T, SubtypeConstraint>)
        {
            std::string subStr = tos(c.subType);
            std::string superStr = tos(c.superType);
            return subStr + " <: " + superStr;
        }
        else if constexpr (std::is_same_v<T, PackSubtypeConstraint>)
        {
            std::string subStr = tos(c.subPack);
            std::string superStr = tos(c.superPack);
            return subStr + " <: " + superStr;
        }
        else if constexpr (std::is_same_v<T, GeneralizationConstraint>)
        {
            std::string subStr = tos(c.generalizedType);
            std::string superStr = tos(c.sourceType);
            return subStr + " ~ gen " + superStr;
        }
        else if constexpr (std::is_same_v<T, InstantiationConstraint>)
        {
            std::string subStr = tos(c.subType);
            std::string superStr = tos(c.superType);
            return subStr + " ~ inst " + superStr;
        }
        else if constexpr (std::is_same_v<T, UnaryConstraint>)
        {
            std::string resultStr = tos(c.resultType);
            std::string operandStr = tos(c.operandType);

            return resultStr + " ~ Unary<" + toString(c.op) + ", " + operandStr + ">";
        }
        else if constexpr (std::is_same_v<T, BinaryConstraint>)
        {
            std::string resultStr = tos(c.resultType);
            std::string leftStr = tos(c.leftType);
            std::string rightStr = tos(c.rightType);

            return resultStr + " ~ Binary<" + toString(c.op) + ", " + leftStr + ", " + rightStr + ">";
        }
        else if constexpr (std::is_same_v<T, IterableConstraint>)
        {
            std::string iteratorStr = tos(c.iterator);
            std::string variableStr = tos(c.variables);

            return variableStr + " ~ Iterate<" + iteratorStr + ">";
        }
        else if constexpr (std::is_same_v<T, NameConstraint>)
        {
            std::string namedStr = tos(c.namedType);
            return "@name(" + namedStr + ") = " + c.name;
        }
        else if constexpr (std::is_same_v<T, TypeAliasExpansionConstraint>)
        {
            std::string targetStr = tos(c.target);
            return "expand " + targetStr;
        }
        else if constexpr (std::is_same_v<T, FunctionCallConstraint>)
        {
            return "call " + tos(c.fn) + " with { result = " + tos(c.result) + " }";
        }
        else if constexpr (std::is_same_v<T, PrimitiveTypeConstraint>)
        {
            return tos(c.resultType) + " ~ prim " + tos(c.expectedType) + ", " + tos(c.singletonType) + ", " +
                   tos(c.multitonType);
        }
        else if constexpr (std::is_same_v<T, HasPropConstraint>)
        {
            return tos(c.resultType) + " ~ hasProp " + tos(c.subjectType) + ", \"" + c.prop + "\"";
        }
        else if constexpr (std::is_same_v<T, SingletonOrTopTypeConstraint>)
        {
            std::string result = tos(c.resultType);
            std::string discriminant = tos(c.discriminantType);

            return result + " ~ if isSingleton D then ~D else unknown where D = " + discriminant;
        }
        else
            static_assert(always_false_v<T>, "Non-exhaustive constraint switch");
    };

    return visit(go, constraint.c);
}

std::string dump(const Constraint& c)
{
    ToStringOptions opts;
    opts.exhaustive = true;
    opts.functionTypeArguments = true;
    std::string s = toString(c, opts);
    printf("%s\n", s.c_str());
    return s;
}

std::string toString(const LValue& lvalue)
{
    LUAU_ASSERT(!FFlag::LuauLvaluelessPath);

    std::string s;
    for (const LValue* current = &lvalue; current; current = baseof(*current))
    {
        if (auto field = get<Field>(*current))
            s = "." + field->key + s;
        else if (auto symbol = get<Symbol>(*current))
            s = toString(*symbol) + s;
        else
            LUAU_ASSERT(!"Unknown LValue");
    }

    return s;
}

std::optional<std::string> getFunctionNameAsString(const AstExpr& expr)
{
    LUAU_ASSERT(FFlag::LuauLvaluelessPath);

    const AstExpr* curr = &expr;
    std::string s;

    for (;;)
    {
        if (auto local = curr->as<AstExprLocal>())
            return local->local->name.value + s;

        if (auto global = curr->as<AstExprGlobal>())
            return global->name.value + s;

        if (auto indexname = curr->as<AstExprIndexName>())
        {
            curr = indexname->expr;

            s = "." + std::string(indexname->index.value) + s;
        }
        else if (auto group = curr->as<AstExprGroup>())
        {
            curr = group->expr;
        }
        else
        {
            return std::nullopt;
        }
    }

    return s;
}
} // namespace Luau
