
#include "Luau/Desugar.h"

#include "Luau/Allocator.h"
#include "Luau/Ast.h"
#include "Luau/Lexer.h"

/*
    local DataType = {}
    DataType.__index = DataType
    do
        local __metatable__DataType = {}
        setmetatable(DataType, __metatable__DataType)
        function __metatable__DataType.__call(self, t)
            return setmetatable(
                {
                    tag=DataType,
                    prop=t.prop, etc
                },
                DataType
            )
        end

        function DataType.__index(self, prop)
           if prop == "prop1" then
               return rawget(self, "prop1")
           end
           if prop == "prop2" then
               return rawget(self, "prop2")
           end

           -- etc

           local p = rawget(DataType, "prop")
           if p then
               return p
           end

           error("Cannot read property " .. tostring(prop))
        end
    end
 */

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

    AstExprGlobal* exprGlobal(AstName name)
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
        return allocator->alloc<AstStatLocal>(location, singleton<AstLocal*>(local), singleton<AstExpr*>(initializer), std::nullopt);
    }

    AstStatAssign* statAssign(AstExpr* lhs, AstExpr* rhs)
    {
        return allocator->alloc<AstStatAssign>(location, singleton(lhs), singleton(rhs));
    }

    AstExprCall* exprCall(AstExpr* fn, std::initializer_list<AstExpr*> args)
    {
        return allocator->alloc<AstExprCall>(location, fn, astArray<AstExpr*>(args), false, AstArray<AstTypeOrPack>{}, location);
    }

    AstStatExpr* statExpr(AstExpr* expr)
    {
        return allocator->alloc<AstStatExpr>(location, expr);
    }

    AstStatReturn* statReturn(AstExpr* expr)
    {
        return allocator->alloc<AstStatReturn>(location, singleton(expr));
    }

    AstStatFunction* statFunction(AstExpr* nameExpr, AstExprFunction* function)
    {
        return allocator->alloc<AstStatFunction>(location, nameExpr, function);
    }

    std::vector<AstExprTable::Item> tableItems;

    AstExprFunction* generateDataConstructor(AstStatDataDeclaration* decl)
    {
        std::string_view propsParamName = "props";
        AstName propsParam = names->getOrAdd(propsParamName.data(), propsParamName.size());
        AstLocal* propsLocal =
            allocator->alloc<AstLocal>(propsParam, Location{}, nullptr, decl->name->functionDepth + 1, decl->name->loopDepth, nullptr);

        tableItems.clear();
        for (const AstDataProp& prop : decl->props)
        {
            std::string_view nameView = prop.name.value;
            AstArray<char> label = astArray(nameView);
            tableItems.push_back(AstExprTable::Item{
                AstExprTable::Item::Record,
                allocator->alloc<AstExprConstantString>(prop.nameLocation, label, AstExprConstantString::QuotedSimple),
                allocator->alloc<AstExprIndexName>(decl->name->location, exprLocal(propsLocal, true), prop.name, prop.nameLocation, Position{0, 0})});
        }

        AstName setmetatable = names->getOrAdd("setmetatable");
        LUAU_ASSERT(setmetatable.value);

        AstExprCall* innerSetMetatableCall = exprCall(
            exprGlobal(setmetatable),
            {allocator->alloc<AstExprTable>(decl->location, astArray<AstExprTable::Item>(tableItems)), exprLocal(decl->name, true)}
        );

        AstStatReturn* returnSetMetatable = statReturn(innerSetMetatableCall);

        std::string debugNameStr = decl->name->name.value;
        debugNameStr += ".__call";
        AstName debugName = names->getOrAdd(debugNameStr.data(), debugNameStr.size());

        return allocator->alloc<AstExprFunction>(
            decl->location,
            AstArray<AstAttr*>{},
            AstArray<AstGenericType*>{},
            AstArray<AstGenericTypePack*>{},
            nullptr,
            astArray<AstLocal*>({astLocal(names->getOrAdd("self")), propsLocal}),
            false,
            Location{},
            allocator->alloc<AstStatBlock>(decl->location, singleton<AstStat*>(returnSetMetatable)),
            decl->name->functionDepth + 1,
            debugName,
            nullptr
        );
    }

    AstStatIf* generateReadPropTest(AstExpr* rawgetFunction, AstExpr* selfExpr, AstExpr* propVar, AstName propName)
    {
        AstExprConstantString* propStr =
            allocator->alloc<AstExprConstantString>(location, astArray(std::string_view(propName.value)), AstExprConstantString::QuotedSimple);

        AstExprBinary* compareToPropStr = allocator->alloc<AstExprBinary>(location, AstExprBinary::CompareEq, propVar, propStr);

        AstStatReturn* ret = statReturn(exprCall(rawgetFunction, {selfExpr, propStr}));

        AstStatBlock* thenBlock = allocator->alloc<AstStatBlock>(location, singleton<AstStat*>(ret));

        return allocator->alloc<AstStatIf>(location, compareToPropStr, thenBlock, nullptr, std::nullopt, std::nullopt);
    }

    AstExprFunction* generateIndexMetamethod(AstStatDataDeclaration* decl, AstLocal* declLocal)
    {
        std::vector<AstStat*> block;

        AstLocal* selfLocal =
            allocator->alloc<AstLocal>(names->getOrAdd("self"), location, nullptr, decl->name->functionDepth + 1, decl->name->loopDepth, nullptr);
        AstExpr* selfParam = exprLocal(selfLocal);

        AstLocal* propLocal =
            allocator->alloc<AstLocal>(names->getOrAdd("prop"), location, nullptr, decl->name->functionDepth + 1, decl->name->loopDepth, nullptr);
        AstExpr* propParam = exprLocal(propLocal);

        AstName rawgetName = names->getOrAdd("rawget");
        AstExpr* rawget = exprGlobal(rawgetName);

        // For each property, test for a match
        for (const auto& prop : decl->props)
            block.emplace_back(generateReadPropTest(exprGlobal(rawgetName), selfParam, propParam, prop.name));

        // If the property is on the metatable, return that.
        AstLocal* pLocal = allocator->alloc<AstLocal>(names->getOrAdd("p"), location, nullptr, decl->name->functionDepth + 1, decl->name->loopDepth, nullptr);
        AstStatLocal* initializeP = statLocal(pLocal, exprCall(exprGlobal(rawgetName), {exprLocal(declLocal, true), propParam}));
        block.push_back(initializeP);

        // AstExpr* print = exprGlobal(names->getOrAdd("print"));
        // block.push_back(statExpr(exprCall(print, {exprLocal(declLocal, true), propParam, exprLocal(pLocal)})));

        AstStatBlock* thenBlock = allocator->alloc<AstStatBlock>(location, singleton<AstStat*>(statReturn(exprLocal(pLocal))));
        AstStatIf* testP = allocator->alloc<AstStatIf>(location, exprLocal(pLocal), thenBlock, nullptr, std::nullopt, std::nullopt);
        block.push_back(testP);

        // Error if no property was found
        std::string errorMessage = "Record ";
        errorMessage += decl->name->name.value;
        errorMessage += " has no property ";

        AstExpr* error = exprGlobal(names->getOrAdd("error"));
        AstExprConstantString* errorPrefixStr = allocator->alloc<AstExprConstantString>(location, astArray(errorMessage), AstExprConstantString::QuotedSimple);

        AstExprBinary* errorStr = allocator->alloc<AstExprBinary>(location, AstExprBinary::Concat, errorPrefixStr, propParam);

        block.emplace_back(statExpr(exprCall(error, {errorStr})));
        //

        std::string debugNameStr = decl->name->name.value;
        debugNameStr += ".__index";
        AstName debugName = names->getOrAdd(debugNameStr.data(), debugNameStr.size());

        return allocator->alloc<AstExprFunction>(
            decl->location,
            AstArray<AstAttr*>{},
            AstArray<AstGenericType*>{},
            AstArray<AstGenericTypePack*>{},
            nullptr,
            astArray<AstLocal*>({selfLocal, propLocal}),
            false,
            Location{},
            allocator->alloc<AstStatBlock>(decl->location, astArray(block)),
            decl->name->functionDepth + 1,
            debugName,
            nullptr
        );
    }
};

DesugarResult desugar(AstStatBlock* program, AstNameTable& names)
{
    DesugarResult result;

    std::vector<AstStat*> stats;

    Allocator& a = result.allocator;

    for (AstStat* stat : program->body)
    {
        stats.clear();

        AstStatDataDeclaration* decl = stat->as<AstStatDataDeclaration>();
        if (!decl)
            continue;

        Desugarer d{&result, &names, decl->name->location, decl->name->functionDepth, decl->name->loopDepth};

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

        AstExprCall* callSetMetatable = d.exprCall(d.exprGlobal(setmetatable), {d.exprLocal(decl->name, true), d.exprLocal(metatableLocal)});

        AstStatFunction* constructorFunction = d.statFunction(
            a.alloc<AstExprIndexName>(
                decl->name->location, d.exprLocal(metatableLocal), names.getOrAdd("__call"), decl->name->location, Position{0, 0}
            ),
            d.generateDataConstructor(decl)
        );

        AstStatFunction* indexMetamethod = d.statFunction(
            a.alloc<AstExprIndexName>(
                decl->name->location, d.exprLocal(declNameLocal), names.getOrAdd("__index"), decl->name->location, Position{0, 0}
            ),
            d.generateIndexMetamethod(decl, declNameLocal)
        );

        AstStatBlock* doBlock = a.alloc<AstStatBlock>(
            decl->location, d.astArray<AstStat*>({metatable, a.alloc<AstStatExpr>(decl->location, callSetMetatable), constructorFunction, indexMetamethod})
        );

        stats.emplace_back(doBlock);

        result.stats[decl] = stats;
    }

    return result;
}

} // namespace Luau
