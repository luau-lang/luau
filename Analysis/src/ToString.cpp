// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ToString.h"

#include "Luau/Common.h"
#include "Luau/Constraint.h"
#include "Luau/DenseHash.h"
#include "Luau/Location.h"
#include "Luau/Scope.h"
#include "Luau/Set.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/Type.h"
#include "Luau/TypeFunction.h"
#include "Luau/VisitType.h"
#include "Luau/TypeOrPack.h"

#include <algorithm>
#include <string>

LUAU_FASTFLAGVARIABLE(LuauEnableDenseTableAlias)

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauExplicitSkipBoundTypes)

/*
 * Enables increasing levels of verbosity for Luau type names when stringifying.
 * After level 2, test cases will break unpredictably because a pointer to their
 * scope will be included in the stringification of generic and free types.
 *
 * Supported values:
 *
 * 0: Disabled, no changes.
 *
 * 1: Prefix free/generic types with free- and gen-, respectively. Also reveal
 * hidden variadic tails. Display block count for local types.
 *
 * 2: Suffix free/generic types with their scope depth.
 *
 * 3: Suffix free/generic types with their scope pointer, if present.
 */
LUAU_FASTINTVARIABLE(DebugLuauVerboseTypeNames, 0)
LUAU_FASTFLAGVARIABLE(DebugLuauToStringNoLexicalSort)

namespace Luau
{

namespace
{

struct FindCyclicTypes final : TypeVisitor
{
    FindCyclicTypes()
        : TypeVisitor("FindCyclicTypes", FFlag::LuauExplicitSkipBoundTypes)
    {
    }

    FindCyclicTypes(const FindCyclicTypes&) = delete;
    FindCyclicTypes& operator=(const FindCyclicTypes&) = delete;

    bool exhaustive = false;
    Luau::Set<TypeId> visited{{}};
    Luau::Set<TypePackId> visitedPacks{{}};
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
        return visited.insert(ty);
    }

    bool visit(TypePackId tp) override
    {
        return visitedPacks.insert(tp);
    }

    bool visit(TypeId ty, const FreeType& ft) override
    {
        if (!visited.insert(ty))
            return false;
        LUAU_ASSERT(ft.lowerBound);
        LUAU_ASSERT(ft.upperBound);
        traverse(ft.lowerBound);
        traverse(ft.upperBound);
        return false;
    }

    bool visit(TypeId ty, const TableType& ttv) override
    {
        if (!visited.insert(ty))
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

    bool visit(TypeId ty, const ExternType&) override
    {
        return false;
    }

    bool visit(TypeId, const PendingExpansionType&) override
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
    for (ScopePtr curr = std::move(scope); curr; curr = curr->parent)
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

    DenseHashMap<TypeId, std::string> cycleNames{{}};
    DenseHashMap<TypePackId, std::string> cycleTpNames{{}};
    Set<void*> seen{{}};
    // `$$$` was chosen as the tombstone for `usedNames` since it is not a valid name syntactically and is relatively short for string comparison
    // reasons.
    DenseHashSet<std::string> usedNames{"$$$"};
    size_t indentation = 0;

    bool exhaustive;

    StringifierState(ToStringOptions& opts, ToStringResult& result)
        : opts(opts)
        , result(result)
        , exhaustive(opts.exhaustive)
    {
        for (const auto& [_, v] : opts.nameMap.types)
            usedNames.insert(v);
        for (const auto& [_, v] : opts.nameMap.typePacks)
            usedNames.insert(v);
    }

    bool hasSeen(const void* tv)
    {
        void* ttv = const_cast<void*>(tv);
        if (seen.contains(ttv))
            return true;

        seen.insert(ttv);
        return false;
    }

    void unsee(const void* tv)
    {
        void* ttv = const_cast<void*>(tv);

        if (seen.contains(ttv))
            seen.erase(ttv);
    }

    std::string getName(TypeId ty)
    {
        const size_t s = opts.nameMap.types.size();
        std::string& n = opts.nameMap.types[ty];
        if (!n.empty())
            return n;

        for (int count = 0; count < 256; ++count)
        {
            std::string candidate = generateName(usedNames.size() + count);
            if (!usedNames.contains(candidate))
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
            if (!usedNames.contains(candidate))
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

        if (FInt::DebugLuauVerboseTypeNames >= 3)
        {
            emit("-");
            char buffer[16];
            uint32_t s = uint32_t(intptr_t(scope) & 0xFFFFFF);
            snprintf(buffer, sizeof(buffer), "0x%x", s);
            emit(buffer);
        }
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

    void emit(Polarity p)
    {
        switch (p)
        {
        case Polarity::None:
            emit("  ");
            break;
        case Polarity::Negative:
            emit(" -");
            break;
        case Polarity::Positive:
            emit("+ ");
            break;
        case Polarity::Mixed:
            emit("+-");
            break;
        default:
            emit("!!");
            break;
        }
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
        if (!opts.useLineBreaks)
            return;

        emit(std::string(indentation, ' '));
    }
};

struct TypeStringifier
{
    StringifierState& state;

    explicit TypeStringifier(StringifierState& state)
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

        if (auto p = state.cycleNames.find(tv))
        {
            state.emit(*p);
            return;
        }

        Luau::visit(
            [this, tv](auto&& t)
            {
                return (*this)(tv, t);
            },
            tv->ty
        );
    }

    void emitKey(const std::string& name)
    {
        if (isIdentifier(name))
            state.emit(name);
        else
        {
            state.emit("[\"");
            state.emit(escape(name));
            state.emit("\"]");
        }
        state.emit(": ");
    }

    void _newStringify(const std::string& name, const Property& prop)
    {
        bool comma = false;
        if (prop.isShared())
        {
            emitKey(name);
            stringify(*prop.readTy);
            return;
        }

        if (prop.readTy)
        {
            state.emit("read ");
            emitKey(name);
            stringify(*prop.readTy);
            comma = true;
        }
        if (prop.writeTy)
        {
            if (comma)
            {
                state.emit(",");
                state.newline();
            }

            state.emit("write ");
            emitKey(name);
            stringify(*prop.writeTy);
        }
    }

    void stringify(const std::string& name, const Property& prop)
    {
        return _newStringify(name, prop);
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

            wrap &= !isEmpty(tp);

            if (wrap)
                state.emit("(");

            stringify(tp);

            if (wrap)
                state.emit(")");
        }

        if (types.size() || typePacks.size())
            state.emit(">");
    }

    void operator()(TypeId ty, const FreeType& ftv)
    {
        state.result.invalid = true;

        // Free types are guaranteed to have upper and lower bounds now.
        LUAU_ASSERT(ftv.lowerBound);
        LUAU_ASSERT(ftv.upperBound);
        const TypeId lowerBound = follow(ftv.lowerBound);
        const TypeId upperBound = follow(ftv.upperBound);
        if (get<NeverType>(lowerBound) && get<UnknownType>(upperBound))
        {
            state.emit("'");
            state.emit(state.getName(ty));
            if (FInt::DebugLuauVerboseTypeNames >= 1)
                state.emit(ftv.polarity);
        }
        else
        {
            state.emit("(");
            if (!get<NeverType>(lowerBound))
            {
                stringify(lowerBound);
                state.emit(" <: ");
            }
            state.emit("'");
            state.emit(state.getName(ty));
            
            if (FInt::DebugLuauVerboseTypeNames >= 1)
                state.emit(ftv.polarity);
            
            if (!get<UnknownType>(upperBound))
            {
                state.emit(" <: ");
                stringify(upperBound);
            }
            state.emit(")");
        }
        return;
    }

    void operator()(TypeId, const BoundType& btv)
    {
        stringify(btv.boundTo);
    }

    void operator()(TypeId ty, const GenericType& gtv)
    {
        if (FInt::DebugLuauVerboseTypeNames >= 1)
            state.emit("gen-");

        if (gtv.explicitName)
        {
            state.usedNames.insert(gtv.name);
            state.opts.nameMap.types[ty] = gtv.name;
            state.emit(gtv.name);
        }
        else
            state.emit(state.getName(ty));

        if (FInt::DebugLuauVerboseTypeNames >= 1)
            state.emit(gtv.polarity);

        if (FInt::DebugLuauVerboseTypeNames >= 2)
        {
            state.emit("-");
            state.emitLevel(gtv.scope);
        }
    }

    void operator()(TypeId, const BlockedType& btv)
    {
        state.emit("*blocked-");
        state.emit(btv.index);
        state.emit("*");
    }

    void operator()(TypeId ty, const PendingExpansionType& petv)
    {
        state.emit("*pending-expansion-");
        state.emit(petv.index);
        state.emit("*");
    }

    void operator()(TypeId, const PrimitiveType& ptv)
    {
        switch (ptv.type)
        {
        case PrimitiveType::NilType:
            state.emit("nil");
            return;
        case PrimitiveType::Boolean:
            state.emit("boolean");
            return;
        case PrimitiveType::Number:
            state.emit("number");
            return;
        case PrimitiveType::String:
            state.emit("string");
            return;
        case PrimitiveType::Thread:
            state.emit("thread");
            return;
        case PrimitiveType::Buffer:
            state.emit("buffer");
            return;
        case PrimitiveType::Function:
            state.emit("function");
            return;
        case PrimitiveType::Table:
            state.emit("table");
            return;
        default:
            LUAU_ASSERT(!"Unknown primitive type");
            throw InternalCompilerError("Unknown primitive type " + std::to_string(ptv.type));
        }
    }

    void operator()(TypeId, const SingletonType& stv)
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
            throw InternalCompilerError("Unknown singleton type");
        }
    }

    void operator()(TypeId, const FunctionType& ftv)
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

        if (ftv.isCheckedFunction)
            state.emit("@checked ");

        state.emit("(");

        if (isEmpty(ftv.argTypes))
        {
            // if we've got an empty argument pack, we're done.
        }
        else if (state.opts.functionTypeArguments)
            stringify(ftv.argTypes, ftv.argNames);
        else
            stringify(ftv.argTypes);

        state.emit(") -> ");

        bool plural = !isEmpty(ftv.retTypes);

        auto retBegin = begin(ftv.retTypes);
        auto retEnd = end(ftv.retTypes);
        if (retBegin != retEnd)
        {
            ++retBegin;
            if (retBegin == retEnd && !retBegin.tail())
                plural = false;
        }

        if (plural)
            state.emit("(");

        stringify(ftv.retTypes);

        if (plural)
            state.emit(")");

        state.unsee(&ftv);
    }

    void operator()(TypeId, const TableType& ttv)
    {
        if (ttv.boundTo)
            return stringify(*ttv.boundTo);

        bool showName = !state.exhaustive;
        if (FFlag::LuauEnableDenseTableAlias)
        {
            // if hide table alias expansions are enabled and there is a name found for the table, use it
            showName = !state.exhaustive || state.opts.hideTableAliasExpansions;
        }
        if (showName)
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
        }

        if (!state.exhaustive)
        {
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
        switch (state.opts.hideTableKind ? TableState::Sealed : ttv.state)
        {
        case TableState::Sealed:
            openbrace = "{";
            closedbrace = "}";
            break;
        case TableState::Unsealed:
            state.result.invalid = true;
            openbrace = "{|";
            closedbrace = "|}";
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

            stringify(name, prop);

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

    void operator()(TypeId, const MetatableType& mtv)
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

    void operator()(TypeId, const ExternType& etv)
    {
        state.emit(etv.name);
    }

    void operator()(TypeId, const AnyType&)
    {
        state.emit("any");
    }

    void operator()(TypeId, const NoRefineType&)
    {
        state.emit("*no-refine*");
    }

    void operator()(TypeId, const UnionType& uv)
    {
        if (state.hasSeen(&uv))
        {
            state.result.cycle = true;
            state.emit("*CYCLE*");
            return;
        }

        LUAU_ASSERT(uv.options.size() > 1);

        bool optional = false;
        bool hasNonNilDisjunct = false;

        std::vector<std::string> results = {};
        size_t resultsLength = 0;
        bool lengthLimitHit = false;

        for (auto el : &uv)
        {
            el = follow(el);

            if (state.opts.useQuestionMarks && isNil(el))
            {
                optional = true;
                continue;
            }
            else
            {
                hasNonNilDisjunct = true;
            }

            std::string saved = std::move(state.result.name);

            bool needParens = !state.cycleNames.contains(el) && (get<IntersectionType>(el) || get<FunctionType>(el));

            if (needParens)
                state.emit("(");

            stringify(el);

            if (needParens)
                state.emit(")");

            resultsLength += state.result.name.length();
            results.push_back(std::move(state.result.name));

            state.result.name = std::move(saved);

            lengthLimitHit = state.opts.maxTypeLength > 0 && resultsLength > state.opts.maxTypeLength;

            if (lengthLimitHit)
                break;
        }

        state.unsee(&uv);

        if (!lengthLimitHit && !FFlag::DebugLuauToStringNoLexicalSort)
            std::sort(results.begin(), results.end());

        if (optional && results.size() > 1)
            state.emit("(");

        bool first = true;
        bool shouldPlaceOnNewlines = results.size() > state.opts.compositeTypesSingleLineLimit;
        for (std::string& ss : results)
        {
            if (!first)
            {
                if (shouldPlaceOnNewlines)
                    state.newline();
                else
                    state.emit(" ");
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

            if (!hasNonNilDisjunct)
                s = "nil";

            state.emit(s);
        }
    }

    void operator()(TypeId ty, const IntersectionType& uv)
    {
        if (state.hasSeen(&uv))
        {
            state.result.cycle = true;
            state.emit("*CYCLE*");
            return;
        }

        std::vector<std::string> results = {};
        size_t resultsLength = 0;
        bool lengthLimitHit = false;

        for (auto el : uv.parts)
        {
            el = follow(el);

            std::string saved = std::move(state.result.name);

            bool needParens = !state.cycleNames.contains(el) && (get<UnionType>(el) || get<FunctionType>(el));

            if (needParens)
                state.emit("(");

            stringify(el);

            if (needParens)
                state.emit(")");

            resultsLength += state.result.name.length();
            results.push_back(std::move(state.result.name));

            state.result.name = std::move(saved);

            lengthLimitHit = state.opts.maxTypeLength > 0 && resultsLength > state.opts.maxTypeLength;

            if (lengthLimitHit)
                break;
        }

        state.unsee(&uv);

        if (!lengthLimitHit && !FFlag::DebugLuauToStringNoLexicalSort)
            std::sort(results.begin(), results.end());

        bool first = true;
        bool shouldPlaceOnNewlines = results.size() > state.opts.compositeTypesSingleLineLimit || isOverloadedFunction(ty);
        for (std::string& ss : results)
        {
            if (!first)
            {
                if (shouldPlaceOnNewlines)
                    state.newline();
                else
                    state.emit(" ");
                state.emit("& ");
            }
            state.emit(ss);
            first = false;
        }
    }

    void operator()(TypeId, const ErrorType& tv)
    {
        state.result.error = true;

        if (tv.synthetic)
        {
            state.emit("*error-type<");
            stringify(*tv.synthetic);
            state.emit(">*");
        }
        else
            state.emit("*error-type*");
    }

    void operator()(TypeId, const LazyType& ltv)
    {
        if (TypeId unwrapped = ltv.unwrapped.load())
        {
            stringify(unwrapped);
        }
        else
        {
            state.result.invalid = true;
            state.emit("lazy?");
        }
    }

    void operator()(TypeId, const UnknownType& ttv)
    {
        state.emit("unknown");
    }

    void operator()(TypeId, const NeverType& ttv)
    {
        state.emit("never");
    }

    void operator()(TypeId, const NegationType& ntv)
    {
        state.emit("~");

        // The precedence of `~` should be less than `|` and `&`.
        TypeId followed = follow(ntv.ty);
        bool parens = get<UnionType>(followed) || get<IntersectionType>(followed);

        if (parens)
            state.emit("(");

        stringify(ntv.ty);

        if (parens)
            state.emit(")");
    }

    void operator()(TypeId, const TypeFunctionInstanceType& tfitv)
    {
        if (tfitv.userFuncName) // Special stringification for user-defined type functions
            state.emit(tfitv.userFuncName->value);
        else
            state.emit(tfitv.function->name);

        state.emit("<");

        bool comma = false;
        for (TypeId ty : tfitv.typeArguments)
        {
            if (comma)
                state.emit(", ");

            comma = true;
            stringify(ty);
        }

        for (TypePackId tp : tfitv.packArguments)
        {
            if (comma)
                state.emit(", ");

            comma = true;
            stringify(tp);
        }

        state.emit(">");
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
        TypeStringifier tvs{state};
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

        if (auto p = state.cycleTpNames.find(tp))
        {
            state.emit(*p);
            return;
        }

        Luau::visit(
            [this, tp](auto&& t)
            {
                return (*this)(tp, t);
            },
            tp->ty
        );
    }

    void operator()(TypePackId, const TypePack& tp)
    {
        if (state.hasSeen(&tp))
        {
            state.result.cycle = true;
            state.emit("*CYCLETP*");
            return;
        }

        if (tp.head.empty() && (!tp.tail || isEmpty(*tp.tail)))
        {
            state.emit("()");
            state.unsee(&tp);
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
            if (auto vtp = get<VariadicTypePack>(tail); !vtp || (FInt::DebugLuauVerboseTypeNames < 1 && !vtp->hidden))
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

    void operator()(TypePackId, const ErrorTypePack& error)
    {
        state.result.error = true;

        if (error.synthetic)
        {
            state.emit("*");
            stringify(*error.synthetic);
            state.emit("*");
        }
        else
            state.emit("*error-type*");
    }

    void operator()(TypePackId, const VariadicTypePack& pack)
    {
        state.emit("...");
        if (FInt::DebugLuauVerboseTypeNames >= 1 && pack.hidden)
        {
            state.emit("*hidden*");
        }
        stringify(pack.ty);
    }

    void operator()(TypePackId tp, const GenericTypePack& pack)
    {
        if (FInt::DebugLuauVerboseTypeNames >= 1)
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

        if (FInt::DebugLuauVerboseTypeNames >= 1)
            state.emit(pack.polarity);

        if (FInt::DebugLuauVerboseTypeNames >= 2)
        {
            state.emit("-");
            state.emitLevel(pack.scope);
        }

        state.emit("...");
    }

    void operator()(TypePackId tp, const FreeTypePack& pack)
    {
        state.result.invalid = true;
        if (FInt::DebugLuauVerboseTypeNames >= 1)
            state.emit("free-");
        state.emit(state.getName(tp));

        if (FInt::DebugLuauVerboseTypeNames >= 1)
            state.emit(pack.polarity);

        if (FInt::DebugLuauVerboseTypeNames >= 2)
        {
            state.emit("-");
            state.emitLevel(pack.scope);
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

    void operator()(TypePackId, const TypeFunctionInstanceTypePack& tfitp)
    {
        state.emit(tfitp.function->name);
        state.emit("<");

        bool comma = false;
        for (TypeId p : tfitp.typeArguments)
        {
            if (comma)
                state.emit(", ");

            comma = true;
            stringify(p);
        }

        for (TypePackId p : tfitp.packArguments)
        {
            if (comma)
                state.emit(", ");

            comma = true;
            stringify(p);
        }

        state.emit(">");
    }
};

void TypeStringifier::stringify(TypePackId tp)
{
    TypePackStringifier tps(state);
    tps.stringify(tp);
}

void TypeStringifier::stringify(TypePackId tpid, const std::vector<std::optional<FunctionArgument>>& names)
{
    TypePackStringifier tps(state, names);
    tps.stringify(tpid);
}

static void assignCycleNames(
    const std::set<TypeId>& cycles,
    const std::set<TypePackId>& cycleTPs,
    DenseHashMap<TypeId, std::string>& cycleNames,
    DenseHashMap<TypePackId, std::string>& cycleTpNames,
    bool exhaustive
)
{
    int nextIndex = 1;

    for (TypeId cycleTy : cycles)
    {
        std::string name;

        // TODO: use the stringified type list if there are no cycles
        if (auto ttv = get<TableType>(follow(cycleTy)); !exhaustive && ttv && (ttv->syntheticName || ttv->name))
        {
            // If we have a cycle type in type parameters, assign a cycle name for this named table
            if (std::find_if(
                    ttv->instantiatedTypeParams.begin(),
                    ttv->instantiatedTypeParams.end(),
                    [&](auto&& el)
                    {
                        return cycles.count(follow(el));
                    }
                ) != ttv->instantiatedTypeParams.end())
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
     * 1. Walk the Type and track seen TypeIds.  When you reencounter a TypeId, add it to a set of seen cycles.
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

    TypeStringifier tvs{state};

    if (!opts.exhaustive)
    {
        if (auto ttv = get<TableType>(ty); ttv && (ttv->name || ttv->syntheticName))
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
        else if (auto mtv = get<MetatableType>(ty); mtv && mtv->syntheticName)
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
    if (auto p = state.cycleNames.find(ty))
        state.emit(*p);
    else
        tvs.stringify(ty);

    if (!state.cycleNames.empty() || !state.cycleTpNames.empty())
    {
        result.cycle = true;
        state.emit(" where ");
    }

    state.exhaustive = true;

    std::vector<std::pair<TypeId, std::string>> sortedCycleNames{state.cycleNames.begin(), state.cycleNames.end()};
    std::sort(
        sortedCycleNames.begin(),
        sortedCycleNames.end(),
        [](const auto& a, const auto& b)
        {
            return a.second < b.second;
        }
    );

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
            cycleTy->ty
        );

        semi = true;
    }

    std::vector<std::pair<TypePackId, std::string>> sortedCycleTpNames(state.cycleTpNames.begin(), state.cycleTpNames.end());
    std::sort(
        sortedCycleTpNames.begin(),
        sortedCycleTpNames.end(),
        [](const auto& a, const auto& b)
        {
            return a.second < b.second;
        }
    );

    TypePackStringifier tps{state};

    for (const auto& [cycleTp, name] : sortedCycleTpNames)
    {
        if (semi)
            state.emit(" ; ");

        state.emit(name);
        state.emit(" = ");
        Luau::visit(
            [&tps, cycleTy = cycleTp](auto&& t)
            {
                return tps(cycleTy, t);
            },
            cycleTp->ty
        );

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
     * 1. Walk the Type and track seen TypeIds.  When you reencounter a TypeId, add it to a set of seen cycles.
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

    TypeStringifier tvs{state};

    /* If the root itself is a cycle, we special case a little.
     * We go out of our way to print the following:
     *
     * t1 where t1 = the_whole_root_type
     */
    if (auto p = state.cycleTpNames.find(tp))
        state.emit(*p);
    else
        tvs.stringify(tp);

    if (!cycles.empty() || !cycleTPs.empty())
    {
        result.cycle = true;
        state.emit(" where ");
    }

    state.exhaustive = true;

    std::vector<std::pair<TypeId, std::string>> sortedCycleNames{state.cycleNames.begin(), state.cycleNames.end()};
    std::sort(
        sortedCycleNames.begin(),
        sortedCycleNames.end(),
        [](const auto& a, const auto& b)
        {
            return a.second < b.second;
        }
    );

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
            cycleTy->ty
        );

        semi = true;
    }

    std::vector<std::pair<TypePackId, std::string>> sortedCycleTpNames{state.cycleTpNames.begin(), state.cycleTpNames.end()};
    std::sort(
        sortedCycleTpNames.begin(),
        sortedCycleTpNames.end(),
        [](const auto& a, const auto& b)
        {
            return a.second < b.second;
        }
    );

    TypePackStringifier tps{tvs.state};

    for (const auto& [cycleTp, name] : sortedCycleTpNames)
    {
        if (semi)
            state.emit(" ; ");

        state.emit(name);
        state.emit(" = ");
        Luau::visit(
            [&tps, cycleTp = cycleTp](auto t)
            {
                return tps(cycleTp, t);
            },
            cycleTp->ty
        );

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

std::string toString(const Type& tv, ToStringOptions& opts)
{
    return toString(const_cast<TypeId>(&tv), opts);
}

std::string toString(const TypePackVar& tp, ToStringOptions& opts)
{
    return toString(const_cast<TypePackId>(&tp), opts);
}

std::string toStringNamedFunction(const std::string& funcName, const FunctionType& ftv, ToStringOptions& opts)
{
    ToStringResult result;
    StringifierState state{opts, result};
    TypeStringifier tvs{state};

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
    bool wrap = get<TypePack>(follow(ftv.retTypes)) && (hasTail ? retSize != 0 : retSize > 1);

    if (wrap)
        state.emit("(");

    tvs.stringify(ftv.retTypes);

    if (wrap)
        state.emit(")");


    return result.name;
}

static ToStringOptions& dumpOptions()
{
    static ToStringOptions opts = ([]() {
        ToStringOptions o;
        o.exhaustive = true;
        o.functionTypeArguments = true;
        o.maxTableLength = 0;
        o.maxTypeLength = 0;
        return o;
    })();

    return opts;
}

std::string dump(TypeId ty)
{
    std::string s = toString(ty, dumpOptions());
    printf("%s\n", s.c_str());
    return s;
}

std::string dump(const std::optional<TypeId>& ty)
{
    if (ty)
        return dump(*ty);

    printf("nullopt\n");
    return "nullopt";
}

std::string dump(TypePackId ty)
{
    std::string s = toString(ty, dumpOptions());
    printf("%s\n", s.c_str());
    return s;
}

std::string dump(const std::optional<TypePackId>& ty)
{
    if (ty)
        return dump(*ty);

    printf("nullopt\n");
    return "nullopt";
}

std::string dump(const std::vector<TypeId>& types)
{
    return toStringVector(types, dumpOptions());
}

std::string dump(DenseHashMap<TypeId, TypeId>& types)
{
    std::string s = "{";
    ToStringOptions& opts = dumpOptions();
    for (const auto& [key, value] : types)
    {
        if (s.length() > 1)
            s += ", ";
        s += toString(key, opts) + " : " + toString(value, opts);
    }
    s += "}";
    return s;
}

std::string dump(DenseHashMap<TypePackId, TypePackId>& types)
{
    std::string s = "{";
    ToStringOptions& opts = dumpOptions();
    for (const auto& [key, value] : types)
    {
        if (s.length() > 1)
            s += ", ";
        s += toString(key, opts) + " : " + toString(value, opts);
    }
    s += "}";
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
    std::string s = toString(ty, dumpOptions());
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

std::string toStringVector(const std::vector<TypeId>& types, ToStringOptions& opts)
{
    std::string s;
    for (TypeId ty : types)
    {
        if (!s.empty())
            s += ", ";
        s += toString(ty, opts);
    }
    return s;
}

std::string toString(const Constraint& constraint, ToStringOptions& opts)
{
    auto go = [&opts](auto&& c) -> std::string
    {
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
            return subStr + " <...: " + superStr;
        }
        else if constexpr (std::is_same_v<T, GeneralizationConstraint>)
        {
            std::string subStr = tos(c.generalizedType);
            std::string superStr = tos(c.sourceType);
            return subStr + " ~ gen " + superStr;
        }
        else if constexpr (std::is_same_v<T, IterableConstraint>)
        {
            std::string iteratorStr = tos(c.iterator);
            std::string variableStr = toStringVector(c.variables, opts);

            return variableStr + " ~ iterate " + iteratorStr;
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
            return "call " + tos(c.fn) + "( " + tos(c.argsPack) + " )" + " with { result = " + tos(c.result) + " }";
        }
        else if constexpr (std::is_same_v<T, FunctionCheckConstraint>)
        {
            return "function_check " + tos(c.fn) + " " + tos(c.argsPack);
        }
        else if constexpr (std::is_same_v<T, PrimitiveTypeConstraint>)
        {
            if (c.expectedType)
                return "prim " + tos(c.freeType) + "[expected: " + tos(*c.expectedType) + "] as " + tos(c.primitiveType);
            else
                return "prim " + tos(c.freeType) + " as " + tos(c.primitiveType);
        }
        else if constexpr (std::is_same_v<T, HasPropConstraint>)
        {
            std::string s = tos(c.resultType) + " ~ hasProp " + tos(c.subjectType) + ", \"" + c.prop + "\" ctx=" + std::to_string(int(c.context));
            if (c.inConditional)
                s += " (inConditional)";
            return s;
        }
        else if constexpr (std::is_same_v<T, HasIndexerConstraint>)
        {
            return tos(c.resultType) + " ~ hasIndexer " + tos(c.subjectType) + " " + tos(c.indexType);
        }
        else if constexpr (std::is_same_v<T, AssignPropConstraint>)
            return tos(c.propType) + " ~ assignProp " + tos(c.lhsType) + " " + c.propName + " " + tos(c.rhsType);
        else if constexpr (std::is_same_v<T, AssignIndexConstraint>)
            return "assignIndex " + tos(c.lhsType) + " " + tos(c.indexType) + " " + tos(c.rhsType);
        else if constexpr (std::is_same_v<T, UnpackConstraint>)
            return toStringVector(c.resultPack, opts) + " ~ ...unpack " + tos(c.sourcePack);
        else if constexpr (std::is_same_v<T, ReduceConstraint>)
            return "reduce " + tos(c.ty);
        else if constexpr (std::is_same_v<T, ReducePackConstraint>)
        {
            return "reduce " + tos(c.tp);
        }
        else if constexpr (std::is_same_v<T, EqualityConstraint>)
            return "equality: " + tos(c.resultType) + " ~ " + tos(c.assignmentType);
        else if constexpr (std::is_same_v<T, SimplifyConstraint>)
            return "simplify " + tos(c.ty);
        else if constexpr (std::is_same_v<T, PushFunctionTypeConstraint>)
            return "push_function_type " + tos(c.expectedFunctionType) + " => " + tos(c.functionType);
        else if constexpr (std::is_same_v<T, PushTypeConstraint>)
            return "push_type " + tos(c.expectedType) + " => " + tos(c.targetType);
        else
            static_assert(always_false_v<T>, "Non-exhaustive constraint switch");
    };

    return visit(go, constraint.c);
}

std::string toString(const Constraint& constraint)
{
    return toString(constraint, ToStringOptions{});
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

std::optional<std::string> getFunctionNameAsString(const AstExpr& expr)
{
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

std::string toString(const Position& position)
{
    return "{ line = " + std::to_string(position.line) + ", col = " + std::to_string(position.column) + " }";
}

std::string toString(const Location& location, int offset, bool useBegin)
{
    return "(" + std::to_string(location.begin.line + offset) + ", " + std::to_string(location.begin.column + offset) + ") - (" +
           std::to_string(location.end.line + offset) + ", " + std::to_string(location.end.column + offset) + ")";
}

std::string toString(const TypeOrPack& tyOrTp, ToStringOptions& opts)
{
    if (const TypeId* ty = get<TypeId>(tyOrTp))
        return toString(*ty, opts);
    else if (const TypePackId* tp = get<TypePackId>(tyOrTp))
        return toString(*tp, opts);
    else
        LUAU_UNREACHABLE();
}

std::string dump(const TypeOrPack& tyOrTp)
{
    ToStringOptions opts;
    opts.exhaustive = true;
    opts.functionTypeArguments = true;
    std::string s = toString(tyOrTp, opts);
    printf("%s\n", s.c_str());
    return s;
}

} // namespace Luau
