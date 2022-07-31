// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lluz/ToString.h"

#include "lluz/Scope.h"
#include "lluz/TypeInfer.h"
#include "lluz/TypePack.h"
#include "lluz/TypeVar.h"
#include "lluz/VisitTypeVar.h"

#include <algorithm>
#include <stdexcept>

lluz_FASTFLAG(LluLowerBoundsCalculation)

/*
 * Prefix generic typenames with gen-
 * Additionally, free types will be prefixed with free- and suffixed with their level.  eg free-a-4
 * Fair warning: Setting this will break a lot of lluz unit tests.
 */
lluz_FASTFLAGVARIABLE(DebugLluVerboseTypeNames, false)
lluz_FASTFLAGVARIABLE(LluToStringTableBracesNewlines, false)

namespace lluz
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

static std::pair<bool, std::optional<lluz::Name>> canUseTypeNameInScope(ScopePtr scope, const std::string& name)
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
    size_t indentation = 0;

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

    int previousNameIndex = 0;

    std::string getName(TypePackId ty)
    {
        const size_t s = result.nameMap.typePacks.size();
        std::string& n = result.nameMap.typePacks[ty];
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

    void emit(TypeLevel level)
    {
        emit(std::to_string(level.level));
        emit(XorStr("-"));
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
            return emit(XorStr(" "));

        emit(XorStr("\n"));
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
            state.emit(XorStr("< VALUELESS BY EXCEPTION >"));
            return;
        }

        auto it = state.cycleNames.find(tv);
        if (it != state.cycleNames.end())
        {
            state.emit(it->second);
            return;
        }

        lluz::visit(
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
            state.emit(XorStr("<"));

        bool first = true;

        for (TypeId ty : types)
        {
            if (!first)
                state.emit(XorStr(", "));
            first = false;

            stringify(ty);
        }

        bool singleTp = typePacks.size() == 1;

        for (TypePackId tp : typePacks)
        {
            if (isEmpty(tp) && singleTp)
                continue;

            if (!first)
                state.emit(XorStr(", "));
            else
                first = false;

            bool wrap = !singleTp && get<TypePack>(follow(tp));

            if (wrap)
                state.emit(XorStr("("));

            stringify(tp);

            if (wrap)
                state.emit(XorStr(")"));
        }

        if (types.size() || typePacks.size())
            state.emit(XorStr(">"));
    }

    void operator()(TypeId ty, const Unifiable::Free& ftv)
    {
        state.result.invalid = true;
        if (FFlag::DebugLluVerboseTypeNames)
            state.emit(XorStr("free-"));
        state.emit(state.getName(ty));

        if (FFlag::DebugLluVerboseTypeNames)
        {
            state.emit(XorStr("-"));
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
            state.result.nameMap.typeVars[ty] = gtv.name;
            state.emit(gtv.name);
        }
        else
            state.emit(state.getName(ty));
    }

    void operator()(TypeId, const ConstrainedTypeVar& ctv)
    {
        state.result.invalid = true;

        state.emit(XorStr("["));
        if (FFlag::DebugLluVerboseTypeNames)
            state.emit(ctv.level);
        state.emit(XorStr("["));

        bool first = true;
        for (TypeId ty : ctv.parts)
        {
            if (first)
                first = false;
            else
                state.emit(XorStr("|"));

            stringify(ty);
        }

        state.emit(XorStr("]]"));
    }

    void operator()(TypeId, const BlockedTypeVar& btv)
    {
        state.emit(XorStr("*blocked-"));
        state.emit(btv.index);
        state.emit(XorStr("*"));
    }

    void operator()(TypeId, const PrimitiveTypeVar& ptv)
    {
        switch (ptv.type)
        {
        case PrimitiveTypeVar::NilType:
            state.emit(XorStr("nil"));
            return;
        case PrimitiveTypeVar::Boolean:
            state.emit(XorStr("boolean"));
            return;
        case PrimitiveTypeVar::Number:
            state.emit(XorStr("number"));
            return;
        case PrimitiveTypeVar::String:
            state.emit(XorStr("string"));
            return;
        case PrimitiveTypeVar::Thread:
            state.emit(XorStr("thread"));
            return;
        default:
            lluz_ASSERT(!XorStr("Unknown primitive type"));
            throw std::runtime_error("Unknown primitive type " + std::to_string(ptv.type));
        }
    }

    void operator()(TypeId, const SingletonTypeVar& stv)
    {
        if (const BooleanSingleton* bs = lluz::get<BooleanSingleton>(&stv))
            state.emit(bs->value ? XorStr("true" : "false"));
        else if (const StringSingleton* ss = lluz::get<StringSingleton>(&stv))
        {
            state.emit(XorStr("\""));
            state.emit(escape(ss->value));
            state.emit(XorStr("\""));
        }
        else
        {
            lluz_ASSERT(!XorStr("Unknown singleton type"));
            throw std::runtime_error(XorStr("Unknown singleton type"));
        }
    }

    void operator()(TypeId, const FunctionTypeVar& ftv)
    {
        if (state.hasSeen(&ftv))
        {
            state.result.cycle = true;
            state.emit(XorStr("<CYCLE>"));
            return;
        }

        // We should not be respecting opts.hideNamedFunctionTypeParameters here.
        if (ftv.generics.size() > 0 || ftv.genericPacks.size() > 0)
        {
            state.emit(XorStr("<"));
            bool comma = false;
            for (auto it = ftv.generics.begin(); it != ftv.generics.end(); ++it)
            {
                if (comma)
                    state.emit(XorStr(", "));
                comma = true;
                stringify(*it);
            }
            for (auto it = ftv.genericPacks.begin(); it != ftv.genericPacks.end(); ++it)
            {
                if (comma)
                    state.emit(XorStr(", "));
                comma = true;
                stringify(*it);
            }
            state.emit(XorStr(">"));
        }

        state.emit(XorStr("("));

        if (state.opts.functionTypeArguments)
            stringify(ftv.argTypes, ftv.argNames);
        else
            stringify(ftv.argTypes);

        state.emit(XorStr(") -> "));

        bool plural = true;

        if (FFlag::LluLowerBoundsCalculation)
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
            state.emit(XorStr("("));

        stringify(ftv.retTypes);

        if (plural)
            state.emit(XorStr(")"));

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
                        state.emit(XorStr("."));
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
            state.emit(XorStr("<CYCLE>"));
            return;
        }

        std::string openbrace = "@@@";
        std::string closedbrace = "@@@?!";
        switch (state.opts.hideTableKind ? TableState::Unsealed : ttv.state)
        {
        case TableState::Sealed:
            state.result.invalid = true;
            if (FFlag::LluToStringTableBracesNewlines)
            {
                openbrace = "{|";
                closedbrace = "|}";
            }
            else
            {
                openbrace = "{| ";
                closedbrace = " |}";
            }
            break;
        case TableState::Unsealed:
            if (FFlag::LluToStringTableBracesNewlines)
            {
                openbrace = "{";
                closedbrace = "}";
            }
            else
            {
                openbrace = "{ ";
                closedbrace = " }";
            }
            break;
        case TableState::Free:
            state.result.invalid = true;
            if (FFlag::LluToStringTableBracesNewlines)
            {
                openbrace = "{-";
                closedbrace = "-}";
            }
            else
            {
                openbrace = "{- ";
                closedbrace = " -}";
            }
            break;
        case TableState::Generic:
            state.result.invalid = true;
            if (FFlag::LluToStringTableBracesNewlines)
            {
                openbrace = "{+";
                closedbrace = "+}";
            }
            else
            {
                openbrace = "{+ ";
                closedbrace = " +}";
            }
            break;
        }

        // If this appears to be an array, we want to stringify it using the {T} syntax.
        if (ttv.indexer && ttv.props.empty() && isNumber(ttv.indexer->indexType))
        {
            state.emit(XorStr("{"));
            stringify(ttv.indexer->indexResultType);
            state.emit(XorStr("}"));
            return;
        }

        state.emit(openbrace);
        state.indent();

        bool comma = false;
        if (ttv.indexer)
        {
            if (FFlag::LluToStringTableBracesNewlines)
                state.newline();
            state.emit(XorStr("["));
            stringify(ttv.indexer->indexType);
            state.emit(XorStr("]: "));
            stringify(ttv.indexer->indexResultType);
            comma = true;
        }

        size_t index = 0;
        size_t oldLength = state.result.name.length();
        for (const auto& [name, prop] : ttv.props)
        {
            if (comma)
            {
                state.emit(XorStr(","));
                state.newline();
            }
            else if (FFlag::LluToStringTableBracesNewlines)
            {
                state.newline();
            }

            size_t length = state.result.name.length() - oldLength;

            if (state.opts.maxTableLength > 0 && (length - 2 * index) >= state.opts.maxTableLength)
            {
                state.emit(XorStr("... "));
                state.emit(std::to_string(ttv.props.size() - index));
                state.emit(XorStr(" more ..."));
                break;
            }

            if (isIdentifier(name))
                state.emit(name);
            else
            {
                state.emit(XorStr("[\""));
                state.emit(escape(name));
                state.emit(XorStr("\"]"));
            }
            state.emit(XorStr(": "));
            stringify(prop.type);
            comma = true;
            ++index;
        }

        state.dedent();
        if (FFlag::LluToStringTableBracesNewlines)
        {
            if (comma)
                state.newline();
            else
                state.emit(XorStr("  "));
        }
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

        state.emit(XorStr("{ @metatable "));
        stringify(mtv.metatable);
        state.emit(XorStr(","));
        state.newline();
        stringify(mtv.table);
        state.emit(XorStr(" }"));
    }

    void operator()(TypeId, const ClassTypeVar& ctv)
    {
        state.emit(ctv.name);
    }

    void operator()(TypeId, const AnyTypeVar&)
    {
        state.emit(XorStr("any"));
    }

    void operator()(TypeId, const UnionTypeVar& uv)
    {
        if (state.hasSeen(&uv))
        {
            state.result.cycle = true;
            state.emit(XorStr("<CYCLE>"));
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
                state.emit(XorStr("("));

            stringify(el);

            if (needParens)
                state.emit(XorStr(")"));

            results.push_back(std::move(state.result.name));
            state.result.name = std::move(saved);
        }

        state.unsee(&uv);

        std::sort(results.begin(), results.end());

        if (optional && results.size() > 1)
            state.emit(XorStr("("));

        bool first = true;
        for (std::string& ss : results)
        {
            if (!first)
            {
                state.newline();
                state.emit(XorStr("| "));
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
            state.emit(XorStr("<CYCLE>"));
            return;
        }

        std::vector<std::string> results = {};
        for (auto el : uv.parts)
        {
            el = follow(el);

            std::string saved = std::move(state.result.name);

            bool needParens = !state.cycleNames.count(el) && (get<UnionTypeVar>(el) || get<FunctionTypeVar>(el));

            if (needParens)
                state.emit(XorStr("("));

            stringify(el);

            if (needParens)
                state.emit(XorStr(")"));

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
                state.emit(XorStr("& "));
            }
            state.emit(ss);
            first = false;
        }
    }

    void operator()(TypeId, const ErrorTypeVar& tv)
    {
        state.result.error = true;
        state.emit(XorStr("*unknown*"));
    }

    void operator()(TypeId, const LazyTypeVar& ltv)
    {
        state.result.invalid = true;
        state.emit(XorStr("lazy?"));
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
            state.emit(XorStr("< VALUELESS TP BY EXCEPTION >"));
            return;
        }

        auto it = state.cycleTpNames.find(tp);
        if (it != state.cycleTpNames.end())
        {
            state.emit(it->second);
            return;
        }

        lluz::visit(
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
            state.emit(XorStr("<CYCLETP>"));
            return;
        }

        bool first = true;

        for (const auto& typeId : tp.head)
        {
            if (first)
                first = false;
            else
                state.emit(XorStr(", "));

            // Do not respect opts.namedFunctionOverrideArgNames here
            if (elemIndex < elemNames.size() && elemNames[elemIndex])
            {
                state.emit(elemNames[elemIndex]->name);
                state.emit(XorStr(": "));
            }

            elemIndex++;

            stringify(typeId);
        }

        if (tp.tail && !isEmpty(*tp.tail))
        {
            TypePackId tail = follow(*tp.tail);
            if (auto vtp = get<VariadicTypePack>(tail); !vtp || (!FFlag::DebugLluVerboseTypeNames && !vtp->hidden))
            {
                if (first)
                    first = false;
                else
                    state.emit(XorStr(", "));

                stringify(tail);
            }
        }

        state.unsee(&tp);
    }

    void operator()(TypePackId, const Unifiable::Error& error)
    {
        state.result.error = true;
        state.emit(XorStr("*unknown*"));
    }

    void operator()(TypePackId, const VariadicTypePack& pack)
    {
        state.emit(XorStr("..."));
        if (FFlag::DebugLluVerboseTypeNames && pack.hidden)
            state.emit(XorStr("<hidden>"));
        stringify(pack.ty);
    }

    void operator()(TypePackId tp, const GenericTypePack& pack)
    {
        if (FFlag::DebugLluVerboseTypeNames)
            state.emit(XorStr("gen-"));
        if (pack.explicitName)
        {
            state.usedNames.insert(pack.name);
            state.result.nameMap.typePacks[tp] = pack.name;
            state.emit(pack.name);
        }
        else
        {
            state.emit(state.getName(tp));
        }
        state.emit(XorStr("..."));
    }

    void operator()(TypePackId tp, const FreeTypePack& pack)
    {
        state.result.invalid = true;
        if (FFlag::DebugLluVerboseTypeNames)
            state.emit(XorStr("free-"));
        state.emit(state.getName(tp));

        if (FFlag::DebugLluVerboseTypeNames)
        {
            state.emit(XorStr("-"));
            state.emit(pack.level);
        }

        state.emit(XorStr("..."));
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

    StringifierState state{opts, result, opts.nameMap};

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
                    result.name = format(XorStr("%s."), moduleName->c_str());
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
        state.emit(XorStr(" where "));
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
            state.emit(XorStr(" ; "));

        state.emit(name);
        state.emit(XorStr(" = "));
        lluz::visit(
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
        state.emit(XorStr(" where "));
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
            state.emit(XorStr(" ; "));

        state.emit(name);
        state.emit(XorStr(" = "));
        lluz::visit(
            [&tvs, cycleTy = cycleTy](auto t)
            {
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

std::string toStringNamedFunction(const std::string& funcName, const FunctionTypeVar& ftv, const ToStringOptions& opts)
{
    ToStringResult result;
    StringifierState state(opts, result, opts.nameMap);
    TypeVarStringifier tvs{state};

    state.emit(funcName);

    if (!opts.hideNamedFunctionTypeParameters)
        tvs.stringify(ftv.generics, ftv.genericPacks);

    state.emit(XorStr("("));

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
            state.emit(XorStr(", "));
        first = false;

        // We don't respect opts.functionTypeArguments
        if (idx < opts.namedFunctionOverrideArgNames.size())
        {
            state.emit(opts.namedFunctionOverrideArgNames[idx] + XorStr(": "));
        }
        else if (idx < ftv.argNames.size() && ftv.argNames[idx])
        {
            state.emit(ftv.argNames[idx]->name + XorStr(": "));
        }
        else
        {
            state.emit(XorStr("_: "));
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
                state.emit(XorStr(", "));

            state.emit(XorStr("...: "));

            if (vtp)
                tvs.stringify(vtp->ty);
            else
                tvs.stringify(*argPackIter.tail());
        }
    }

    state.emit(XorStr("): "));

    size_t retSize = size(ftv.retTypes);
    bool hasTail = !finite(ftv.retTypes);
    bool wrap = get<TypePack>(follow(ftv.retTypes)) && (hasTail ? retSize != 0 : retSize != 1);

    if (wrap)
        state.emit(XorStr("("));

    tvs.stringify(ftv.retTypes);

    if (wrap)
        state.emit(XorStr(")"));

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
    auto go = [&opts](auto&& c) {
        using T = std::decay_t<decltype(c)>;

        if constexpr (std::is_same_v<T, SubtypeConstraint>)
        {
            ToStringResult subStr = toStringDetailed(c.subType, opts);
            opts.nameMap = std::move(subStr.nameMap);
            ToStringResult superStr = toStringDetailed(c.superType, opts);
            opts.nameMap = std::move(superStr.nameMap);
            return subStr.name + " <: " + superStr.name;
        }
        else if constexpr (std::is_same_v<T, PackSubtypeConstraint>)
        {
            ToStringResult subStr = toStringDetailed(c.subPack, opts);
            opts.nameMap = std::move(subStr.nameMap);
            ToStringResult superStr = toStringDetailed(c.superPack, opts);
            opts.nameMap = std::move(superStr.nameMap);
            return subStr.name + " <: " + superStr.name;
        }
        else if constexpr (std::is_same_v<T, GeneralizationConstraint>)
        {
            ToStringResult subStr = toStringDetailed(c.generalizedType, opts);
            opts.nameMap = std::move(subStr.nameMap);
            ToStringResult superStr = toStringDetailed(c.sourceType, opts);
            opts.nameMap = std::move(superStr.nameMap);
            return subStr.name + " ~ gen " + superStr.name;
        }
        else if constexpr (std::is_same_v<T, InstantiationConstraint>)
        {
            ToStringResult subStr = toStringDetailed(c.subType, opts);
            opts.nameMap = std::move(subStr.nameMap);
            ToStringResult superStr = toStringDetailed(c.superType, opts);
            opts.nameMap = std::move(superStr.nameMap);
            return subStr.name + " ~ inst " + superStr.name;
        }
        else if constexpr (std::is_same_v<T, UnaryConstraint>)
        {
            ToStringResult resultStr = toStringDetailed(c.resultType, opts);
            opts.nameMap = std::move(resultStr.nameMap);
            ToStringResult operandStr = toStringDetailed(c.operandType, opts);
            opts.nameMap = std::move(operandStr.nameMap);

            return resultStr.name + " ~ Unary<" + toString(c.op) + ", " + operandStr.name + ">";
        }
        else if constexpr (std::is_same_v<T, BinaryConstraint>)
        {
            ToStringResult resultStr = toStringDetailed(c.resultType);
            opts.nameMap = std::move(resultStr.nameMap);
            ToStringResult leftStr = toStringDetailed(c.leftType);
            opts.nameMap = std::move(leftStr.nameMap);
            ToStringResult rightStr = toStringDetailed(c.rightType);
            opts.nameMap = std::move(rightStr.nameMap);

            return resultStr.name + " ~ Binary<" + toString(c.op) + ", " + leftStr.name + ", " + rightStr.name + ">";
        }
        else if constexpr (std::is_same_v<T, NameConstraint>)
        {
            ToStringResult namedStr = toStringDetailed(c.namedType, opts);
            opts.nameMap = std::move(namedStr.nameMap);
            return XorStr("@name(") + namedStr.name + ") = " + c.name;
        }
        else
            static_assert(always_false_v<T>, XorStr("Non-exhaustive constraint switch"));
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

} // namespace lluz
