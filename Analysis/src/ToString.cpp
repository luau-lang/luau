// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ToString.h"

#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"

#include <algorithm>
#include <stdexcept>

LUAU_FASTFLAG(LuauToStringFollowsBoundTo)
LUAU_FASTFLAG(LuauExtraNilRecovery)
LUAU_FASTFLAG(LuauOccursCheckOkWithRecursiveFunctions)
LUAU_FASTFLAGVARIABLE(LuauInstantiatedTypeParamRecursion, false)

namespace Luau
{

namespace
{

struct FindCyclicTypes
{
    FindCyclicTypes() = default;
    FindCyclicTypes(const FindCyclicTypes&) = delete;
    FindCyclicTypes& operator=(const FindCyclicTypes&) = delete;

    bool exhaustive = false;
    std::unordered_set<TypeId> visited;
    std::unordered_set<TypePackId> visitedPacks;
    std::unordered_set<TypeId> cycles;
    std::unordered_set<TypePackId> cycleTPs;

    void cycle(TypeId ty)
    {
        cycles.insert(ty);
    }

    void cycle(TypePackId tp)
    {
        cycleTPs.insert(tp);
    }

    template<typename T>
    bool operator()(TypeId ty, const T&)
    {
        return visited.insert(ty).second;
    }

    bool operator()(TypeId ty, const TableTypeVar& ttv) = delete;

    bool operator()(TypeId ty, const TableTypeVar& ttv, std::unordered_set<void*>& seen)
    {
        if (!visited.insert(ty).second)
            return false;

        if (ttv.name || ttv.syntheticName)
        {
            for (TypeId itp : ttv.instantiatedTypeParams)
                visitTypeVar(itp, *this, seen);
            return exhaustive;
        }

        return true;
    }

    bool operator()(TypeId, const ClassTypeVar&)
    {
        return false;
    }

    template<typename T>
    bool operator()(TypePackId tp, const T&)
    {
        return visitedPacks.insert(tp).second;
    }
};

template<typename TID>
void findCyclicTypes(std::unordered_set<TypeId>& cycles, std::unordered_set<TypePackId>& cycleTPs, TID ty, bool exhaustive)
{
    FindCyclicTypes fct;
    fct.exhaustive = exhaustive;
    visitTypeVar(ty, fct);

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
    const ToStringOptions& opts;
    ToStringResult& result;

    std::unordered_map<TypeId, std::string> cycleNames;
    std::unordered_map<TypePackId, std::string> cycleTpNames;
    std::unordered_set<void*> seen;
    std::unordered_set<std::string> usedNames;

    bool exhaustive;

    StringifierState(const ToStringOptions& opts, ToStringResult& result, const std::optional<ToStringNameMap>& nameMap)
        : opts(opts)
        , result(result)
        , exhaustive(opts.exhaustive)
    {
        if (nameMap)
            result.nameMap = *nameMap;

        for (const auto& [_, v] : result.nameMap.typeVars)
            usedNames.insert(v);
        for (const auto& [_, v] : result.nameMap.typePacks)
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

    static std::string generateName(size_t i)
    {
        std::string n;
        n = char('a' + i % 26);
        if (i >= 26)
            n += std::to_string(i / 26);
        return n;
    }

    std::string getName(TypeId ty)
    {
        const size_t s = result.nameMap.typeVars.size();
        std::string& n = result.nameMap.typeVars[ty];
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

    std::string getName(TypePackId ty)
    {
        const size_t s = result.nameMap.typePacks.size();
        std::string& n = result.nameMap.typePacks[ty];
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

    void emit(const std::string& s)
    {
        if (opts.maxTypeLength > 0 && result.name.length() > opts.maxTypeLength)
            return;

        result.name += s;
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
            state.emit("< VALUELESS BY EXCEPTION >");
            return;
        }

        auto it = state.cycleNames.find(tv);
        if (it != state.cycleNames.end())
        {
            state.emit(it->second);
            return;
        }

        if (!FFlag::LuauAddMissingFollow)
        {
            if (get<FreeTypeVar>(tv))
            {
                state.emit(state.getName(tv));
                return;
            }
        }

        Luau::visit(
            [this, tv](auto&& t) {
                return (*this)(tv, t);
            },
            tv->ty);
    }

    void stringify(TypePackId tp);
    void stringify(TypePackId tpid, const std::vector<std::optional<FunctionArgument>>& names);

    void stringify(const std::vector<TypeId>& types)
    {
        if (types.size() == 0)
            return;

        if (types.size())
            state.emit("<");

        for (size_t i = 0; i < types.size(); ++i)
        {
            if (i > 0)
                state.emit(", ");

            stringify(types[i]);
        }

        if (types.size())
            state.emit(">");
    }

    void operator()(TypeId ty, const Unifiable::Free& ftv)
    {
        state.result.invalid = true;

        if (FFlag::LuauAddMissingFollow)
            state.emit(state.getName(ty));
        else
            state.emit("<FREE>");
    }

    void operator()(TypeId, const BoundTypeVar& btv)
    {
        stringify(btv.boundTo);
    }

    void operator()(TypeId ty, const Unifiable::Generic& gtv)
    {
        if (gtv.explicitName)
        {
            state.result.nameMap.typeVars[ty] = gtv.name;
            state.emit(gtv.name);
        }
        else
            state.emit(state.getName(ty));
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
        default:
            LUAU_ASSERT(!"Unknown primitive type");
            throw std::runtime_error("Unknown primitive type " + std::to_string(ptv.type));
        }
    }

    void operator()(TypeId, const FunctionTypeVar& ftv)
    {
        if (state.hasSeen(&ftv))
        {
            state.result.cycle = true;
            state.emit("<CYCLE>");
            return;
        }

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
        if (auto retPack = get<TypePack>(follow(ftv.retType)))
        {
            if (retPack->head.size() == 1 && !retPack->tail)
                plural = false;
        }

        if (plural)
            state.emit("(");

        stringify(ftv.retType);

        if (plural)
            state.emit(")");

        state.unsee(&ftv);
    }

    void operator()(TypeId, const TableTypeVar& ttv)
    {
        if (FFlag::LuauToStringFollowsBoundTo && ttv.boundTo)
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
                stringify(ttv.instantiatedTypeParams);
                return;
            }
            if (ttv.syntheticName)
            {
                state.result.invalid = true;
                state.emit(*ttv.syntheticName);
                stringify(ttv.instantiatedTypeParams);
                return;
            }
        }

        if (state.hasSeen(&ttv))
        {
            state.result.cycle = true;
            state.emit("<CYCLE>");
            return;
        }

        std::string openbrace = "@@@";
        std::string closedbrace = "@@@?!";
        switch (state.opts.hideTableKind ? TableState::Unsealed : ttv.state)
        {
        case TableState::Sealed:
            state.result.invalid = true;
            openbrace = "{| ";
            closedbrace = " |}";
            break;
        case TableState::Unsealed:
            openbrace = "{ ";
            closedbrace = " }";
            break;
        case TableState::Free:
            state.result.invalid = true;
            openbrace = "{- ";
            closedbrace = " -}";
            break;
        case TableState::Generic:
            state.result.invalid = true;
            openbrace = "{+ ";
            closedbrace = " +}";
            break;
        }

        // If this appears to be an array, we want to stringify it using the {T} syntax.
        if (ttv.indexer && ttv.props.empty() && isNumber(ttv.indexer->indexType))
        {
            state.emit("{");
            stringify(ttv.indexer->indexResultType);
            state.emit("}");
            return;
        }

        state.emit(openbrace);

        bool comma = false;
        if (ttv.indexer)
        {
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
                state.emit(state.opts.useLineBreaks ? ",\n" : ", ");

            size_t length = state.result.name.length() - oldLength;

            if (state.opts.maxTableLength > 0 && (length - 2 * index) >= state.opts.maxTableLength)
            {
                state.emit("... ");
                state.emit(std::to_string(ttv.props.size() - index));
                state.emit(" more ...");
                break;
            }

            state.emit(name);
            state.emit(": ");
            stringify(prop.type);
            comma = true;
            ++index;
        }

        state.emit(closedbrace);

        state.unsee(&ttv);
    }

    void operator()(TypeId, const MetatableTypeVar& mtv)
    {
        state.result.invalid = true;
        state.emit("{ @metatable ");
        stringify(mtv.metatable);
        state.emit(state.opts.useLineBreaks ? ",\n" : ", ");
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
            state.emit("<CYCLE>");
            return;
        }

        bool optional = false;

        std::vector<std::string> results = {};
        for (auto el : &uv)
        {
            if (FFlag::LuauExtraNilRecovery || FFlag::LuauAddMissingFollow)
                el = follow(el);

            if (isNil(el))
            {
                optional = true;
                continue;
            }

            std::string saved = std::move(state.result.name);

            bool needParens = FFlag::LuauOccursCheckOkWithRecursiveFunctions
                                  ? !state.cycleNames.count(el) && (get<IntersectionTypeVar>(el) || get<FunctionTypeVar>(el))
                                  : get<IntersectionTypeVar>(el) || get<FunctionTypeVar>(el);

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
                state.emit(" | ");
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
            state.emit("<CYCLE>");
            return;
        }

        std::vector<std::string> results = {};
        for (auto el : uv.parts)
        {
            if (FFlag::LuauExtraNilRecovery || FFlag::LuauAddMissingFollow)
                el = follow(el);

            std::string saved = std::move(state.result.name);

            bool needParens = FFlag::LuauOccursCheckOkWithRecursiveFunctions
                                  ? !state.cycleNames.count(el) && (get<UnionTypeVar>(el) || get<FunctionTypeVar>(el))
                                  : get<UnionTypeVar>(el) || get<FunctionTypeVar>(el);

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
                state.emit(" & ");
            state.emit(ss);
            first = false;
        }
    }

    void operator()(TypeId, const ErrorTypeVar& tv)
    {
        state.result.error = true;
        state.emit("*unknown*");
    }

    void operator()(TypeId, const LazyTypeVar& ltv)
    {
        state.result.invalid = true;
        state.emit("lazy?");
    }

}; // namespace

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
            state.emit("< VALUELESS TP BY EXCEPTION >");
            return;
        }

        if (!FFlag::LuauAddMissingFollow)
        {
            if (get<FreeTypePack>(tp))
            {
                state.emit(state.getName(tp));
                state.emit("...");
                return;
            }
        }

        auto it = state.cycleTpNames.find(tp);
        if (it != state.cycleTpNames.end())
        {
            state.emit(it->second);
            return;
        }

        Luau::visit(
            [this, tp](auto&& t) {
                return (*this)(tp, t);
            },
            tp->ty);
    }

    void operator()(TypePackId, const TypePack& tp)
    {
        if (state.hasSeen(&tp))
        {
            state.result.cycle = true;
            state.emit("<CYCLETP>");
            return;
        }

        bool first = true;

        for (const auto& typeId : tp.head)
        {
            if (first)
                first = false;
            else
                state.emit(", ");

            LUAU_ASSERT(elemNames.empty() || elemIndex < elemNames.size());

            if (!elemNames.empty() && elemNames[elemIndex])
            {
                state.emit(elemNames[elemIndex]->name);
                state.emit(": ");
            }
            elemIndex++;

            stringify(typeId);
        }

        if (tp.tail && !isEmpty(*tp.tail))
        {
            const auto& tail = *tp.tail;
            if (first)
                first = false;
            else
                state.emit(", ");

            stringify(tail);
        }

        state.unsee(&tp);
    }

    void operator()(TypePackId, const Unifiable::Error& error)
    {
        state.result.error = true;
        state.emit("*unknown*");
    }

    void operator()(TypePackId, const VariadicTypePack& pack)
    {
        state.emit("...");
        stringify(pack.ty);
    }

    void operator()(TypePackId tp, const GenericTypePack& pack)
    {
        if (pack.explicitName)
        {
            state.result.nameMap.typePacks[tp] = pack.name;
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

        if (FFlag::LuauAddMissingFollow)
        {
            state.emit(state.getName(tp));
            state.emit("...");
        }
        else
        {
            state.emit("<FREETP>");
        }
    }

    void operator()(TypePackId, const BoundTypePack& btv)
    {
        stringify(btv.boundTo);
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

static void assignCycleNames(const std::unordered_set<TypeId>& cycles, const std::unordered_set<TypePackId>& cycleTPs,
    std::unordered_map<TypeId, std::string>& cycleNames, std::unordered_map<TypePackId, std::string>& cycleTpNames, bool exhaustive)
{
    int nextIndex = 1;

    std::vector<TypeId> sortedCycles{cycles.begin(), cycles.end()};
    std::sort(sortedCycles.begin(), sortedCycles.end(), std::less<TypeId>{});

    for (TypeId cycleTy : sortedCycles)
    {
        std::string name;

        // TODO: use the stringified type list if there are no cycles
        if (FFlag::LuauInstantiatedTypeParamRecursion)
        {
            if (auto ttv = get<TableTypeVar>(follow(cycleTy)); !exhaustive && ttv && (ttv->syntheticName || ttv->name))
            {
                // If we have a cycle type in type parameters, assign a cycle name for this named table
                if (std::find_if(ttv->instantiatedTypeParams.begin(), ttv->instantiatedTypeParams.end(), [&](auto&& el) {
                        return cycles.count(follow(el));
                    }) != ttv->instantiatedTypeParams.end())
                    cycleNames[cycleTy] = ttv->name ? *ttv->name : *ttv->syntheticName;

                continue;
            }
        }
        else
        {
            if (auto ttv = get<TableTypeVar>(follow(cycleTy)); !exhaustive && ttv && (ttv->syntheticName || ttv->name))
                continue;
        }

        name = "t" + std::to_string(nextIndex);
        ++nextIndex;

        cycleNames[cycleTy] = std::move(name);
    }

    std::vector<TypePackId> sortedCycleTps{cycleTPs.begin(), cycleTPs.end()};
    std::sort(sortedCycleTps.begin(), sortedCycleTps.end(), std::less<TypePackId>());

    for (TypePackId tp : sortedCycleTps)
    {
        std::string name = "tp" + std::to_string(nextIndex);
        ++nextIndex;
        cycleTpNames[tp] = std::move(name);
    }
}

ToStringResult toStringDetailed(TypeId ty, const ToStringOptions& opts)
{
    /*
     * 1. Walk the TypeVar and track seen TypeIds.  When you reencounter a TypeId, add it to a set of seen cycles.
     * 2. Generate some names for each cycle.  For a starting point, we can just call them t0, t1 and so on.
     * 3. For each seen cycle, stringify it like we do now, but replace each known cycle with its name.
     * 4. Print out the root of the type using the same algorithm as step 3.
     */
    ty = follow(ty);

    ToStringResult result;

    if (!FFlag::LuauInstantiatedTypeParamRecursion && !opts.exhaustive)
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

            if (ttv->instantiatedTypeParams.empty())
                return result;

            std::vector<std::string> params;
            for (TypeId tp : ttv->instantiatedTypeParams)
                params.push_back(toString(tp));

            result.name += "<" + join(params, ", ") + ">";
            return result;
        }
        else if (auto mtv = get<MetatableTypeVar>(ty); mtv && mtv->syntheticName)
        {
            result.invalid = true;
            result.name = *mtv->syntheticName;
            return result;
        }
    }

    StringifierState state{opts, result, opts.nameMap};

    std::unordered_set<TypeId> cycles;
    std::unordered_set<TypePackId> cycleTPs;

    findCyclicTypes(cycles, cycleTPs, ty, opts.exhaustive);

    assignCycleNames(cycles, cycleTPs, state.cycleNames, state.cycleTpNames, opts.exhaustive);

    TypeVarStringifier tvs{state};

    if (FFlag::LuauInstantiatedTypeParamRecursion && !opts.exhaustive)
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

            if (ttv->instantiatedTypeParams.empty())
                return result;

            result.name += "<";

            bool first = true;
            for (TypeId ty : ttv->instantiatedTypeParams)
            {
                if (!first)
                    result.name += ", ";
                else
                    first = false;

                tvs.stringify(ty);
            }

            if (opts.maxTypeLength > 0 && result.name.length() > opts.maxTypeLength)
            {
                result.truncated = true;
                result.name += "... <TRUNCATED>";
            }
            else
            {
                result.name += ">";
            }

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
    std::sort(sortedCycleNames.begin(), sortedCycleNames.end(), [](const auto& a, const auto& b) {
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
            [&tvs, cycleTy = cycleTy](auto&& t) {
                return tvs(cycleTy, t);
            },
            cycleTy->ty);

        semi = true;
    }

    if (opts.maxTypeLength > 0 && result.name.length() > opts.maxTypeLength)
    {
        result.truncated = true;
        result.name += "... <TRUNCATED>";
    }

    return result;
}

ToStringResult toStringDetailed(TypePackId tp, const ToStringOptions& opts)
{
    /*
     * 1. Walk the TypeVar and track seen TypeIds.  When you reencounter a TypeId, add it to a set of seen cycles.
     * 2. Generate some names for each cycle.  For a starting point, we can just call them t0, t1 and so on.
     * 3. For each seen cycle, stringify it like we do now, but replace each known cycle with its name.
     * 4. Print out the root of the type using the same algorithm as step 3.
     */
    ToStringResult result;
    StringifierState state{opts, result, opts.nameMap};

    std::unordered_set<TypeId> cycles;
    std::unordered_set<TypePackId> cycleTPs;

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
    std::sort(sortedCycleNames.begin(), sortedCycleNames.end(), [](const auto& a, const auto& b) {
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
            [&tvs, cycleTy = cycleTy](auto&& t) {
                return tvs(cycleTy, t);
            },
            cycleTy->ty);

        semi = true;
    }

    if (opts.maxTypeLength > 0 && result.name.length() > opts.maxTypeLength)
        result.name += "... <TRUNCATED>";

    return result;
}

std::string toString(TypeId ty, const ToStringOptions& opts)
{
    return toStringDetailed(ty, opts).name;
}

std::string toString(TypePackId tp, const ToStringOptions& opts)
{
    return toStringDetailed(tp, opts).name;
}

std::string toString(const TypeVar& tv, const ToStringOptions& opts)
{
    return toString(const_cast<TypeId>(&tv), std::move(opts));
}

std::string toString(const TypePackVar& tp, const ToStringOptions& opts)
{
    return toString(const_cast<TypePackId>(&tp), std::move(opts));
}

void dump(TypeId ty)
{
    ToStringOptions opts;
    opts.exhaustive = true;
    opts.functionTypeArguments = true;
    printf("%s\n", toString(ty, opts).c_str());
}

void dump(TypePackId ty)
{
    ToStringOptions opts;
    opts.exhaustive = true;
    opts.functionTypeArguments = true;
    printf("%s\n", toString(ty, opts).c_str());
}

} // namespace Luau
