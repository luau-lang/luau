
#include "Luau/Desugar.h"

#include "Luau/Allocator.h"
#include "Luau/Ast.h"
#include "Luau/Lexer.h"

namespace Luau
{

struct Desugarer
{
    DesugarResult* result = nullptr;
    AstNameTable* names = nullptr;
    Allocator* allocator = &result->allocator;

    Location location;
    size_t functionDepth;
    size_t loopDepth;

    explicit Desugarer(DesugarResult* result, AstNameTable* names, Location location, size_t functionDepth, size_t loopDepth)
        : result(result)
        , names(names)
        , location(location)
        , functionDepth(functionDepth)
        , loopDepth(loopDepth)
    {
    }

    template<typename T>
    AstArray<T> singleton(T value)
    {
        AstArray<T> res;
        res.size = 1;
        res.data = static_cast<T*>(allocator->allocate(sizeof(T)));
        new (res.data) T(value);
        return res;
    }

    AstArray<char> astArray(std::string_view s)
    {
        AstArray<char> res;
        res.size = s.size();
        res.data = static_cast<char*>(allocator->allocate(sizeof(char) * s.size()));
        std::copy(s.begin(), s.end(), res.data);
        return res;
    }

    template<typename T>
    AstArray<T> astArray(const std::vector<T>& elements)
    {
        AstArray<T> res;
        res.size = elements.size();
        res.data = static_cast<T*>(allocator->allocate(sizeof(T) * elements.size()));
        auto it = begin(elements);
        for (size_t i = 0; i < elements.size(); ++i)
            new (res.data + i) T(*it++);

        return res;
    }

    template<typename T>
    AstArray<T> astArray(std::initializer_list<T> elements)
    {
        AstArray<T> res;
        res.size = elements.size();
        res.data = static_cast<T*>(allocator->allocate(sizeof(T) * elements.size()));
        auto it = begin(elements);
        for (size_t i = 0; i < elements.size(); ++i)
            new (res.data + i) T(*it++);

        return res;
    }

    AstExprGlobal* global(AstName name)
    {
        return allocator->alloc<AstExprGlobal>(location, name);
    }

    AstLocal* astLocal(AstName name)
    {
        return allocator->alloc<AstLocal>(name, Location{}, nullptr, functionDepth, loopDepth, nullptr);
    }

    AstExprLocal* exprLocal(AstLocal* local, bool upvalue = false)
    {
        return allocator->alloc<AstExprLocal>(location, local, upvalue);
    }

    AstExprIndexName* exprIndexName(AstExpr* lhs, std::string_view index)
    {
        AstName indexName = names->getOrAdd(index.data(), index.size());
        return allocator->alloc<AstExprIndexName>(location, lhs, indexName, location, Position{0, 0});
    }

    AstExprTable* emptyTable()
    {
        return allocator->alloc<AstExprTable>(location, AstArray<AstExprTable::Item>{});
    }

    AstStatLocal* statLocal(AstLocal* local, AstExpr* initializer)
    {
        return allocator->alloc<AstStatLocal>(
            location,
            singleton<AstLocal*>(local),
            singleton<AstExpr*>(emptyTable()),
            std::nullopt
        );
    }

    AstStatAssign* statAssign(AstExpr* lhs, AstExpr* rhs)
    {
        return allocator->alloc<AstStatAssign>(
            location,
            singleton(lhs),
            singleton(rhs)
        );
    }

    AstExprCall* exprCall(AstExpr* fn, std::initializer_list<AstExpr*> args)
    {
        return allocator->alloc<AstExprCall>(
            location,
            fn,
            astArray<AstExpr*>(args),
            false,
            AstArray<AstTypeOrPack>{},
            location
        );
    }

    AstStatReturn* statReturn(AstExpr* expr)
    {
        return allocator->alloc<AstStatReturn>(location, singleton(expr));
    }
};

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

        Desugarer d{&result, &names, decl->name->location, decl->name->functionDepth, decl->name->loopDepth};

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

        AstStatLocal* local = d.statLocal(declNameLocal, d.emptyTable());
        stats.emplace_back(local);

        AstStatAssign* assignIndexMetaproperty = d.statAssign(d.exprIndexName(d.exprLocal(declNameLocal), "__index"), d.exprLocal(declNameLocal));
        stats.emplace_back(assignIndexMetaproperty);

        std::string mtNameStr = "__metatable__";
        mtNameStr += decl->name->name.value;
        const AstName metatableName = names.getOrAdd(mtNameStr.data(), mtNameStr.length());
        AstLocal* metatableLocal = d.astLocal(metatableName);

        AstStatLocal* metatable = d.statLocal(metatableLocal, d.emptyTable());

        AstName setmetatable = names.getOrAdd("setmetatable");
        LUAU_ASSERT(setmetatable.value);

        AstExprCall* callSetMetatable = d.exprCall(d.global(setmetatable), {d.exprLocal(decl->name, true), d.exprLocal(metatableLocal)});

        std::string_view propsParamName = "props";
        AstName propsParam = names.getOrAdd(propsParamName.data(), propsParamName.size());
        AstLocal* propsLocal = a.alloc<AstLocal>(propsParam, Location{}, nullptr, decl->name->functionDepth + 1, decl->name->loopDepth, nullptr);

        tableItems.clear();
        for (const AstDataProp& prop : decl->props)
        {
            std::string_view nameView = prop.name.value;
            AstArray<char> label = d.astArray(nameView);
            tableItems.push_back(
                AstExprTable::Item{
                    AstExprTable::Item::Record,
                    a.alloc<AstExprConstantString>(prop.nameLocation, label, AstExprConstantString::QuotedSimple),
                    a.alloc<AstExprIndexName>(decl->name->location, d.exprLocal(propsLocal, true), prop.name, prop.nameLocation, Position{0, 0})
                }
            );
        }

        AstExprCall* innerSetMetatableCall = d.exprCall(
            d.global(setmetatable), {a.alloc<AstExprTable>(decl->location, d.astArray<AstExprTable::Item>(tableItems)), d.exprLocal(decl->name, true)}
        );

        AstStatReturn* returnSetMetatable = d.statReturn(innerSetMetatableCall);

        std::string debugNameStr = decl->name->name.value;
        debugNameStr += ".__call";
        AstName debugName = names.getOrAdd(debugNameStr.data(), debugNameStr.size());

        AstStatFunction* constructorFunction = a.alloc<AstStatFunction>(
            decl->location,
            a.alloc<AstExprIndexName>(decl->name->location, d.exprLocal(metatableLocal), names.getOrAdd("__call"), decl->name->location, Position{0, 0}),
            a.alloc<AstExprFunction>(
                decl->location,
                AstArray<AstAttr*>{},
                AstArray<AstGenericType*>{},
                AstArray<AstGenericTypePack*>{},
                nullptr,
                d.astArray<AstLocal*>({d.astLocal(names.getOrAdd("self")), propsLocal}),
                false,
                Location{},
                a.alloc<AstStatBlock>(decl->location, d.singleton<AstStat*>(returnSetMetatable)),
                decl->name->functionDepth + 1,
                debugName,
                nullptr
            )
        );

        AstStatBlock* doBlock = a.alloc<AstStatBlock>(
            decl->location, d.astArray<AstStat*>({metatable, a.alloc<AstStatExpr>(decl->location, callSetMetatable), constructorFunction})
        );

        stats.emplace_back(doBlock);

        result.stats[decl] = stats;
    }

    return result;
}

}
