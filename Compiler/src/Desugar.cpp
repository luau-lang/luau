
#include "Luau/Desugar.h"

#include "Luau/Allocator.h"
#include "Luau/Ast.h"
#include "Luau/Lexer.h"

namespace Luau
{
template<typename T>
static AstArray<T> singleton(Allocator& allocator, T value)
{
    AstArray<T> res;
    res.size = 1;
    res.data = static_cast<T*>(allocator.allocate(sizeof(T)));
    new (res.data) T(value);
    return res;
}

static AstArray<char> astArray(Allocator& allocator, std::string_view s)
{
    AstArray<char> res;
    res.size = s.size();
    res.data = static_cast<char*>(allocator.allocate(sizeof(char) * s.size()));
    std::copy(s.begin(), s.end(), res.data);
    return res;
}

template<typename T>
static AstArray<T> astArray(Allocator& allocator, const std::vector<T>& elements)
{
    AstArray<T> res;
    res.size = elements.size();
    res.data = static_cast<T*>(allocator.allocate(sizeof(T) * elements.size()));
    auto it = begin(elements);
    for (size_t i = 0; i < elements.size(); ++i)
        new (res.data + i) T(*it++);

    return res;
}

template<typename T>
static AstArray<T> astArray(Allocator& allocator, std::initializer_list<T> elements)
{
    AstArray<T> res;
    res.size = elements.size();
    res.data = static_cast<T*>(allocator.allocate(sizeof(T) * elements.size()));
    auto it = begin(elements);
    for (size_t i = 0; i < elements.size(); ++i)
        new (res.data + i) T(*it++);

    return res;
}

static AstStatLocal* localTable(Allocator& allocator, AstLocal* local, const Location& location)
{
    return allocator.alloc<AstStatLocal>(
        location,
        singleton<AstLocal*>(allocator, local),
        singleton<AstExpr*>(allocator, allocator.alloc<AstExprTable>(location, AstArray<AstExprTable::Item>{})),
        std::nullopt
    );
}

DesugarResult desugar(AstStatBlock* program, AstNameTable& names)
{
    DesugarResult result;

    std::vector<AstStat*> stats;
    std::vector<AstExprTable::Item> tableItems;

    Allocator& a = result.allocator;

    for (AstStat* stat : program->body)
    {
        stats.clear();

        AstStatDataDeclaration* decl = stat->as<AstStatDataDeclaration>();
        if (!decl)
            continue;

        auto global = [&](AstName name)
        {
            return a.alloc<AstExprGlobal>(decl->name->location, name);
        };

        auto astLocal = [&](AstName name)
        {
            return a.alloc<AstLocal>(name, Location{}, nullptr, decl->name->functionDepth, decl->name->loopDepth, nullptr);
        };

        auto loc = [&](AstLocal* local, bool upvalue = false)
        {
            return a.alloc<AstExprLocal>(decl->name->location, local, upvalue);
        };

        /*
         * local DataType = {}
         * DataType.__index = DataType
         * do
         *     local __metatable__DataType = {}
         *     setmetatable(DataType, __metatable__DataType)
         *
         *     function __metatable__DataType.__call(self, t)
         *         return setmetatable(
         *             {
         *                 tag=DataType,
         *                 prop=t.prop, etc
         *             },
         *             DataType
         *         )
         *     end
         *     -- also __index and __newindex if we want to do static property access
         * end
         */

        AstLocal* declNameLocal = decl->name;

        AstStatLocal* local = localTable(a, declNameLocal, decl->name->location);
        stats.emplace_back(local);

        AstStatAssign* assignIndexMetaproperty = a.alloc<AstStatAssign>(
            decl->location,
            singleton<AstExpr*>(
                a, a.alloc<AstExprIndexName>(decl->name->location, loc(declNameLocal), names.getOrAdd("__index"), Location{}, Position{0, 0})
            ),
            singleton<AstExpr*>(a, loc(declNameLocal))
        );
        stats.emplace_back(assignIndexMetaproperty);

        std::string mtNameStr = "__metatable__";
        mtNameStr += decl->name->name.value;
        const AstName metatableName = names.getOrAdd(mtNameStr.data(), mtNameStr.length());
        AstLocal* metatableLocal = astLocal(metatableName);

        AstStatLocal* metatable = localTable(a, metatableLocal, decl->name->location);

        AstName setmetatable = names.getOrAdd("setmetatable");
        LUAU_ASSERT(setmetatable.value);

        AstExprCall* callSetMetatable = a.alloc<AstExprCall>(
            decl->location,
            global(setmetatable),
            astArray<AstExpr*>(a, {loc(decl->name, true), loc(metatableLocal)}),
            false,
            AstArray<AstTypeOrPack>{},
            decl->location
        );

        std::string_view propsParamName = "props";
        AstName propsParam = names.getOrAdd(propsParamName.data(), propsParamName.size());
        AstLocal* propsLocal = a.alloc<AstLocal>(propsParam, Location{}, nullptr, decl->name->functionDepth + 1, decl->name->loopDepth, nullptr);

        tableItems.clear();
        for (const AstDataProp& prop : decl->props)
        {
            std::string_view nameView = prop.name.value;
            AstArray<char> label = astArray(a, nameView);
            tableItems.push_back(
                AstExprTable::Item{
                    AstExprTable::Item::Record,
                    a.alloc<AstExprConstantString>(prop.nameLocation, label, AstExprConstantString::QuotedSimple),
                    a.alloc<AstExprIndexName>(decl->name->location, loc(propsLocal, true), prop.name, prop.nameLocation, Position{0, 0})
                }
            );
        }

        AstExprCall* innerSetMetatableCall = a.alloc<AstExprCall>(
            decl->location,
            global(setmetatable),
            astArray<AstExpr*>(a, {a.alloc<AstExprTable>(decl->location, astArray<AstExprTable::Item>(a, tableItems)), loc(decl->name, true)}),
            false,
            AstArray<AstTypeOrPack>{},
            decl->location
        );

        AstStatReturn* returnSetMetatable = a.alloc<AstStatReturn>(decl->location, singleton<AstExpr*>(a, innerSetMetatableCall));

        std::string debugNameStr = decl->name->name.value;
        debugNameStr += ".__call";
        AstName debugName = names.getOrAdd(debugNameStr.data(), debugNameStr.size());

        AstStatFunction* constructorFunction = a.alloc<AstStatFunction>(
            decl->location,
            a.alloc<AstExprIndexName>(decl->name->location, loc(metatableLocal), names.getOrAdd("__call"), decl->name->location, Position{0, 0}),
            a.alloc<AstExprFunction>(
                decl->location,
                AstArray<AstAttr*>{},
                AstArray<AstGenericType*>{},
                AstArray<AstGenericTypePack*>{},
                nullptr,
                astArray<AstLocal*>(a, {astLocal(names.getOrAdd("self")), propsLocal}),
                false,
                Location{},
                a.alloc<AstStatBlock>(decl->location, singleton<AstStat*>(a, returnSetMetatable)),
                decl->name->functionDepth + 1,
                debugName,
                nullptr
            )
        );

        AstStatBlock* doBlock = a.alloc<AstStatBlock>(
            decl->location, astArray<AstStat*>(a, {metatable, a.alloc<AstStatExpr>(decl->location, callSetMetatable), constructorFunction})
        );

        stats.emplace_back(doBlock);

        result.stats[decl] = stats;
    }

    return result;
}

}
