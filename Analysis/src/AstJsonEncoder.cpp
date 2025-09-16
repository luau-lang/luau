// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AstJsonEncoder.h"

#include "Luau/Ast.h"
#include "Luau/ParseResult.h"
#include "Luau/StringUtils.h"
#include "Luau/Common.h"

#include <math.h>

LUAU_FASTFLAG(LuauAutocompleteAttributes)

namespace Luau
{

struct AstJsonEncoder : public AstVisitor
{
    static constexpr int CHUNK_SIZE = 1024;
    std::vector<std::string> chunks;
    bool comma = false;

    AstJsonEncoder()
    {
        newChunk();
    }

    std::string str()
    {
        return join(chunks, "");
    }

    bool pushComma()
    {
        bool c = comma;
        comma = false;
        return c;
    }

    void popComma(bool c)
    {
        comma = c;
    }

    void newChunk()
    {
        chunks.emplace_back();
        chunks.back().reserve(CHUNK_SIZE);
    }

    void appendChunk(std::string_view sv)
    {
        if (sv.size() > CHUNK_SIZE)
        {
            chunks.emplace_back(sv);
            newChunk();
            return;
        }

        auto& chunk = chunks.back();
        if (chunk.size() + sv.size() < CHUNK_SIZE)
        {
            chunk.append(sv.data(), sv.size());
            return;
        }

        size_t prefix = CHUNK_SIZE - chunk.size();
        chunk.append(sv.data(), prefix);
        newChunk();

        chunks.back().append(sv.data() + prefix, sv.size() - prefix);
    }

    void writeRaw(std::string_view sv)
    {
        appendChunk(sv);
    }

    void writeRaw(char c)
    {
        writeRaw(std::string_view{&c, 1});
    }

    void writeType(std::string_view propValue)
    {
        write("type", propValue);
    }

    template<typename T>
    void write(std::string_view propName, const T& value)
    {
        if (comma)
            writeRaw(",");
        comma = true;
        writeRaw("\"");
        writeRaw(propName);
        writeRaw("\":");
        write(value);
    }

    void write(bool b)
    {
        if (b)
            writeRaw("true");
        else
            writeRaw("false");
    }

    void write(double d)
    {
        switch (fpclassify(d))
        {
        case FP_INFINITE:
            if (d < 0)
                writeRaw("-Infinity");
            else
                writeRaw("Infinity");
            break;

        case FP_NAN:
            writeRaw("NaN");
            break;

        case FP_NORMAL:
        case FP_SUBNORMAL:
        case FP_ZERO:
        default:
            char b[32];
            snprintf(b, sizeof(b), "%.17g", d);
            writeRaw(b);
            break;
        }
    }

    void writeString(std::string_view sv)
    {
        // TODO escape more accurately?
        writeRaw("\"");

        for (char c : sv)
        {
            if (c == '"')
                writeRaw("\\\"");
            else if (c == '\\')
                writeRaw("\\\\");
            else if (c < ' ')
                writeRaw(format("\\u%04x", c));
            else if (c == '\n')
                writeRaw("\\n");
            else
                writeRaw(c);
        }

        writeRaw("\"");
    }

    void write(char c)
    {
        writeString(std::string_view(&c, 1));
    }
    void write(int i)
    {
        writeRaw(std::to_string(i));
    }
    void write(long i)
    {
        writeRaw(std::to_string(i));
    }
    void write(long long i)
    {
        writeRaw(std::to_string(i));
    }
    void write(unsigned int i)
    {
        writeRaw(std::to_string(i));
    }
    void write(unsigned long i)
    {
        writeRaw(std::to_string(i));
    }
    void write(unsigned long long i)
    {
        writeRaw(std::to_string(i));
    }
    void write(std::nullptr_t)
    {
        writeRaw("null");
    }
    void write(std::string_view str)
    {
        writeString(str);
    }
    void write(std::optional<AstName> name)
    {
        if (name)
            write(*name);
        else
            writeRaw("null");
    }
    void write(AstName name)
    {
        writeString(name.value ? name.value : "");
    }
    void write(std::optional<AstArgumentName> name)
    {
        if (name)
            write(*name);
        else
            writeRaw("null");
    }
    void write(AstArgumentName name)
    {
        writeRaw("{");
        bool c = pushComma();
        writeType("AstArgumentName");
        write("name", name.first);
        write("location", name.second);
        popComma(c);
        writeRaw("}");
    }

    void write(const Position& position)
    {
        write(position.line);
        writeRaw(",");
        write(position.column);
    }

    void write(const Location& location)
    {
        writeRaw("\"");
        write(location.begin);
        writeRaw(" - ");
        write(location.end);
        writeRaw("\"");
    }

    void write(AstLocal* local)
    {
        writeRaw("{");
        bool c = pushComma();
        if (local->annotation != nullptr)
            write("luauType", local->annotation);
        else
            write("luauType", nullptr);
        write("name", local->name);
        writeType("AstLocal");
        write("location", local->location);
        popComma(c);
        writeRaw("}");
    }

    void writeNode(AstNode* node)
    {
        write("location", node->location);
    }

    template<typename F>
    void writeNode(AstNode* node, std::string_view name, F&& f)
    {
        writeRaw("{");
        bool c = pushComma();
        writeType(name);
        writeNode(node);
        f();
        popComma(c);
        writeRaw("}");
    }

    void write(AstNode* node)
    {
        node->visit(this);
    }

    void write(class AstExprGroup* node)
    {
        writeNode(
            node,
            "AstExprGroup",
            [&]()
            {
                write("expr", node->expr);
            }
        );
    }

    void write(class AstExprConstantNil* node)
    {
        writeNode(node, "AstExprConstantNil", []() {});
    }

    void write(class AstExprConstantBool* node)
    {
        writeNode(
            node,
            "AstExprConstantBool",
            [&]()
            {
                write("value", node->value);
            }
        );
    }

    void write(class AstExprConstantNumber* node)
    {
        writeNode(
            node,
            "AstExprConstantNumber",
            [&]()
            {
                write("value", node->value);
            }
        );
    }

    void write(class AstExprConstantString* node)
    {
        writeNode(
            node,
            "AstExprConstantString",
            [&]()
            {
                write("value", node->value);
            }
        );
    }

    void write(class AstExprLocal* node)
    {
        writeNode(
            node,
            "AstExprLocal",
            [&]()
            {
                write("local", node->local);
            }
        );
    }

    void write(class AstExprGlobal* node)
    {
        writeNode(
            node,
            "AstExprGlobal",
            [&]()
            {
                write("global", node->name);
            }
        );
    }

    void write(class AstExprVarargs* node)
    {
        writeNode(node, "AstExprVarargs", []() {});
    }

    template<typename T>
    void write(AstArray<T> arr)
    {
        writeRaw("[");
        bool comma = false;
        for (const auto& a : arr)
        {
            if (comma)
                writeRaw(",");
            else
                comma = true;

            write(a);
        }
        writeRaw("]");
    }

    void write(AstArray<char> arr)
    {
        write(std::string_view{arr.data, arr.size});
    }

#define PROP(prop) write(#prop, node->prop)

    void write(class AstExprCall* node)
    {
        writeNode(
            node,
            "AstExprCall",
            [&]()
            {
                PROP(func);
                PROP(args);
                PROP(self);
                PROP(argLocation);
            }
        );
    }

    void write(class AstExprIndexName* node)
    {
        writeNode(
            node,
            "AstExprIndexName",
            [&]()
            {
                PROP(expr);
                PROP(index);
                PROP(indexLocation);
                PROP(op);
            }
        );
    }

    void write(class AstExprIndexExpr* node)
    {
        writeNode(
            node,
            "AstExprIndexExpr",
            [&]()
            {
                PROP(expr);
                PROP(index);
            }
        );
    }

    void write(class AstExprFunction* node)
    {
        writeNode(
            node,
            "AstExprFunction",
            [&]()
            {
                PROP(attributes);
                PROP(generics);
                PROP(genericPacks);
                if (node->self)
                    PROP(self);
                PROP(args);
                if (node->returnAnnotation)
                    PROP(returnAnnotation);
                PROP(vararg);
                PROP(varargLocation);
                if (node->varargAnnotation)
                    PROP(varargAnnotation);

                PROP(body);
                PROP(functionDepth);
                PROP(debugname);
            }
        );
    }

    void write(const std::optional<AstTypeList>& typeList)
    {
        if (typeList)
            write(*typeList);
        else
            writeRaw("null");
    }

    void write(const AstTypeList& typeList)
    {
        writeRaw("{");
        bool c = pushComma();
        writeType("AstTypeList");
        write("types", typeList.types);
        if (typeList.tailType)
            write("tailType", typeList.tailType);
        popComma(c);
        writeRaw("}");
    }

    void write(class AstGenericType* genericType)
    {
        writeRaw("{");
        bool c = pushComma();
        writeType("AstGenericType");
        write("name", genericType->name);
        if (genericType->defaultValue)
            write("luauType", genericType->defaultValue);
        popComma(c);
        writeRaw("}");
    }

    void write(class AstGenericTypePack* genericTypePack)
    {
        writeRaw("{");
        bool c = pushComma();
        writeType("AstGenericTypePack");
        write("name", genericTypePack->name);
        if (genericTypePack->defaultValue)
            write("luauType", genericTypePack->defaultValue);
        popComma(c);
        writeRaw("}");
    }

    void write(AstExprTable::Item::Kind kind)
    {
        switch (kind)
        {
        case AstExprTable::Item::List:
            return writeString("item");
        case AstExprTable::Item::Record:
            return writeString("record");
        case AstExprTable::Item::General:
            return writeString("general");
        }
    }

    void write(const AstExprTable::Item& item)
    {
        writeRaw("{");
        bool c = pushComma();
        writeType("AstExprTableItem");
        write("kind", item.kind);
        switch (item.kind)
        {
        case AstExprTable::Item::List:
            write("value", item.value);
            break;
        default:
            write("key", item.key);
            write("value", item.value);
            break;
        }
        popComma(c);
        writeRaw("}");
    }

    void write(class AstExprIfElse* node)
    {
        writeNode(
            node,
            "AstExprIfElse",
            [&]()
            {
                PROP(condition);
                PROP(hasThen);
                PROP(trueExpr);
                PROP(hasElse);
                PROP(falseExpr);
            }
        );
    }

    void write(class AstExprInterpString* node)
    {
        writeNode(
            node,
            "AstExprInterpString",
            [&]()
            {
                PROP(strings);
                PROP(expressions);
            }
        );
    }

    void write(class AstExprTable* node)
    {
        writeNode(
            node,
            "AstExprTable",
            [&]()
            {
                PROP(items);
            }
        );
    }

    void write(AstExprUnary::Op op)
    {
        switch (op)
        {
        case AstExprUnary::Not:
            return writeString("Not");
        case AstExprUnary::Minus:
            return writeString("Minus");
        case AstExprUnary::Len:
            return writeString("Len");
        }
    }

    void write(class AstExprUnary* node)
    {
        writeNode(
            node,
            "AstExprUnary",
            [&]()
            {
                PROP(op);
                PROP(expr);
            }
        );
    }

    void write(AstExprBinary::Op op)
    {
        switch (op)
        {
        case AstExprBinary::Add:
            return writeString("Add");
        case AstExprBinary::Sub:
            return writeString("Sub");
        case AstExprBinary::Mul:
            return writeString("Mul");
        case AstExprBinary::Div:
            return writeString("Div");
        case AstExprBinary::FloorDiv:
            return writeString("FloorDiv");
        case AstExprBinary::Mod:
            return writeString("Mod");
        case AstExprBinary::Pow:
            return writeString("Pow");
        case AstExprBinary::Concat:
            return writeString("Concat");
        case AstExprBinary::CompareNe:
            return writeString("CompareNe");
        case AstExprBinary::CompareEq:
            return writeString("CompareEq");
        case AstExprBinary::CompareLt:
            return writeString("CompareLt");
        case AstExprBinary::CompareLe:
            return writeString("CompareLe");
        case AstExprBinary::CompareGt:
            return writeString("CompareGt");
        case AstExprBinary::CompareGe:
            return writeString("CompareGe");
        case AstExprBinary::And:
            return writeString("And");
        case AstExprBinary::Or:
            return writeString("Or");
        default:
            LUAU_ASSERT(!"Unknown Op");
        }
    }

    void write(class AstExprBinary* node)
    {
        writeNode(
            node,
            "AstExprBinary",
            [&]()
            {
                PROP(op);
                PROP(left);
                PROP(right);
            }
        );
    }

    void write(class AstExprTypeAssertion* node)
    {
        writeNode(
            node,
            "AstExprTypeAssertion",
            [&]()
            {
                PROP(expr);
                PROP(annotation);
            }
        );
    }

    void write(class AstExprError* node)
    {
        writeNode(
            node,
            "AstExprError",
            [&]()
            {
                PROP(expressions);
                PROP(messageIndex);
            }
        );
    }

    void write(class AstStatBlock* node)
    {
        writeNode(
            node,
            "AstStatBlock",
            [&]()
            {
                writeRaw(",\"hasEnd\":");
                write(node->hasEnd);
                writeRaw(",\"body\":[");
                bool comma = false;
                for (AstStat* stat : node->body)
                {
                    if (comma)
                        writeRaw(",");
                    else
                        comma = true;

                    write(stat);
                }
                writeRaw("]");
            }
        );
    }

    void write(class AstStatIf* node)
    {
        writeNode(
            node,
            "AstStatIf",
            [&]()
            {
                PROP(condition);
                PROP(thenbody);
                if (node->elsebody)
                    PROP(elsebody);
                write("hasThen", node->thenLocation.has_value());
            }
        );
    }

    void write(class AstStatWhile* node)
    {
        writeNode(
            node,
            "AstStatWhile",
            [&]()
            {
                PROP(condition);
                PROP(body);
                PROP(hasDo);
            }
        );
    }

    void write(class AstStatRepeat* node)
    {
        writeNode(
            node,
            "AstStatRepeat",
            [&]()
            {
                PROP(condition);
                PROP(body);
            }
        );
    }

    void write(class AstStatBreak* node)
    {
        writeNode(node, "AstStatBreak", []() {});
    }

    void write(class AstStatContinue* node)
    {
        writeNode(node, "AstStatContinue", []() {});
    }

    void write(class AstStatReturn* node)
    {
        writeNode(
            node,
            "AstStatReturn",
            [&]()
            {
                PROP(list);
            }
        );
    }

    void write(class AstStatExpr* node)
    {
        writeNode(
            node,
            "AstStatExpr",
            [&]()
            {
                PROP(expr);
            }
        );
    }

    void write(class AstStatLocal* node)
    {
        writeNode(
            node,
            "AstStatLocal",
            [&]()
            {
                PROP(vars);
                PROP(values);
            }
        );
    }

    void write(class AstStatFor* node)
    {
        writeNode(
            node,
            "AstStatFor",
            [&]()
            {
                PROP(var);
                PROP(from);
                PROP(to);
                if (node->step)
                    PROP(step);
                PROP(body);
                PROP(hasDo);
            }
        );
    }

    void write(class AstStatForIn* node)
    {
        writeNode(
            node,
            "AstStatForIn",
            [&]()
            {
                PROP(vars);
                PROP(values);
                PROP(body);
                PROP(hasIn);
                PROP(hasDo);
            }
        );
    }

    void write(class AstStatAssign* node)
    {
        writeNode(
            node,
            "AstStatAssign",
            [&]()
            {
                PROP(vars);
                PROP(values);
            }
        );
    }

    void write(class AstStatCompoundAssign* node)
    {
        writeNode(
            node,
            "AstStatCompoundAssign",
            [&]()
            {
                PROP(op);
                PROP(var);
                PROP(value);
            }
        );
    }

    void write(class AstStatFunction* node)
    {
        writeNode(
            node,
            "AstStatFunction",
            [&]()
            {
                PROP(name);
                PROP(func);
            }
        );
    }

    void write(class AstStatLocalFunction* node)
    {
        writeNode(
            node,
            "AstStatLocalFunction",
            [&]()
            {
                PROP(name);
                PROP(func);
            }
        );
    }

    void write(class AstStatTypeAlias* node)
    {
        writeNode(
            node,
            "AstStatTypeAlias",
            [&]()
            {
                PROP(name);
                PROP(generics);
                PROP(genericPacks);
                write("value", node->type);
                PROP(exported);
            }
        );
    }

    void write(class AstStatDeclareFunction* node)
    {
        writeNode(
            node,
            "AstStatDeclareFunction",
            [&]()
            {
                PROP(attributes);
                PROP(name);
                PROP(nameLocation);
                PROP(params);
                PROP(paramNames);
                PROP(vararg);
                PROP(varargLocation);
                PROP(retTypes);
                PROP(generics);
                PROP(genericPacks);
            }
        );
    }

    void write(class AstStatDeclareGlobal* node)
    {
        writeNode(
            node,
            "AstStatDeclareGlobal",
            [&]()
            {
                PROP(name);
                PROP(nameLocation);
                PROP(type);
            }
        );
    }

    void write(const AstDeclaredExternTypeProperty& prop)
    {
        writeRaw("{");
        bool c = pushComma();
        write("name", prop.name);
        write("nameLocation", prop.nameLocation);
        writeType("AstDeclaredClassProp");
        write("luauType", prop.ty);
        write("location", prop.location);
        popComma(c);
        writeRaw("}");
    }

    void write(class AstStatDeclareExternType* node)
    {
        writeNode(
            node,
            "AstStatDeclareClass",
            [&]()
            {
                PROP(name);
                if (node->superName)
                    write("superName", *node->superName);
                PROP(props);
                PROP(indexer);
            }
        );
    }

    void write(class AstStatError* node)
    {
        writeNode(
            node,
            "AstStatError",
            [&]()
            {
                PROP(expressions);
                PROP(statements);
            }
        );
    }

    void write(struct AstTypeOrPack node)
    {
        if (node.type)
            write(node.type);
        else
            write(node.typePack);
    }

    void write(class AstTypeReference* node)
    {
        writeNode(
            node,
            "AstTypeReference",
            [&]()
            {
                if (node->prefix)
                    PROP(prefix);
                if (node->prefixLocation)
                    write("prefixLocation", *node->prefixLocation);
                PROP(name);
                PROP(nameLocation);
                PROP(parameters);
            }
        );
    }

    void write(const AstTableProp& prop)
    {
        writeRaw("{");
        bool c = pushComma();

        write("name", prop.name);
        writeType("AstTableProp");
        write("location", prop.location);
        write("propType", prop.type);

        popComma(c);
        writeRaw("}");
    }

    void write(class AstTypeTable* node)
    {
        writeNode(
            node,
            "AstTypeTable",
            [&]()
            {
                PROP(props);
                PROP(indexer);
            }
        );
    }

    void write(struct AstTableIndexer* indexer)
    {
        if (indexer)
        {
            writeRaw("{");
            bool c = pushComma();
            write("location", indexer->location);
            write("indexType", indexer->indexType);
            write("resultType", indexer->resultType);
            popComma(c);
            writeRaw("}");
        }
        else
        {
            writeRaw("null");
        }
    }

    void write(class AstTypeFunction* node)
    {
        writeNode(
            node,
            "AstTypeFunction",
            [&]()
            {
                PROP(attributes);
                PROP(generics);
                PROP(genericPacks);
                PROP(argTypes);
                PROP(argNames);
                PROP(returnTypes);
            }
        );
    }

    void write(class AstTypeTypeof* node)
    {
        writeNode(
            node,
            "AstTypeTypeof",
            [&]()
            {
                PROP(expr);
            }
        );
    }

    void write(class AstTypeOptional* node)
    {
        writeNode(node, "AstTypeOptional", [&]() {});
    }

    void write(class AstTypeUnion* node)
    {
        writeNode(
            node,
            "AstTypeUnion",
            [&]()
            {
                PROP(types);
            }
        );
    }

    void write(class AstTypeIntersection* node)
    {
        writeNode(
            node,
            "AstTypeIntersection",
            [&]()
            {
                PROP(types);
            }
        );
    }

    void write(class AstTypeError* node)
    {
        writeNode(
            node,
            "AstTypeError",
            [&]()
            {
                PROP(types);
                PROP(messageIndex);
            }
        );
    }

    void write(class AstTypePackExplicit* node)
    {
        writeNode(
            node,
            "AstTypePackExplicit",
            [&]()
            {
                PROP(typeList);
            }
        );
    }

    void write(class AstTypePackVariadic* node)
    {
        writeNode(
            node,
            "AstTypePackVariadic",
            [&]()
            {
                PROP(variadicType);
            }
        );
    }

    void write(class AstTypePackGeneric* node)
    {
        writeNode(
            node,
            "AstTypePackGeneric",
            [&]()
            {
                PROP(genericName);
            }
        );
    }

    void write(AstAttr::Type type)
    {
        switch (type)
        {
        case AstAttr::Type::Checked:
            return writeString("checked");
        case AstAttr::Type::Native:
            return writeString("native");
        case AstAttr::Type::Deprecated:
            return writeString("deprecated");
        case AstAttr::Type::Unknown:
            return writeString("unknown");
        }
    }

    void write(class AstAttr* node)
    {
        writeNode(
            node,
            "AstAttr",
            [&]()
            {
                if (FFlag::LuauAutocompleteAttributes)
                    write("name", node->name);
                else
                    write("name", node->type);
            }
        );
    }

    bool visit(class AstTypeGroup* node) override
    {
        writeNode(
            node,
            "AstTypeGroup",
            [&]()
            {
                write("inner", node->type);
            }
        );
        return false;
    }

    bool visit(class AstTypeSingletonBool* node) override
    {
        writeNode(
            node,
            "AstTypeSingletonBool",
            [&]()
            {
                write("value", node->value);
            }
        );
        return false;
    }

    bool visit(class AstTypeSingletonString* node) override
    {
        writeNode(
            node,
            "AstTypeSingletonString",
            [&]()
            {
                write("value", node->value);
            }
        );
        return false;
    }

    bool visit(class AstExprGroup* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprConstantNil* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprConstantBool* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprConstantNumber* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprConstantString* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprIfElse* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprInterpString* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprLocal* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprGlobal* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprVarargs* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprCall* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprIndexName* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprIndexExpr* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprFunction* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprTable* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprUnary* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprBinary* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprTypeAssertion* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstExprError* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatBlock* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatIf* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatWhile* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatRepeat* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatBreak* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatContinue* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatReturn* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatExpr* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatLocal* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatFor* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatForIn* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatAssign* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatCompoundAssign* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatFunction* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatLocalFunction* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatTypeAlias* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatDeclareFunction* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatDeclareGlobal* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatDeclareExternType* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstStatError* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstTypeReference* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstTypeTable* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstTypeFunction* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstTypeTypeof* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstTypeOptional* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstTypeUnion* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstTypeIntersection* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstTypeError* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstTypePack* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstTypePackExplicit* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstTypePackVariadic* node) override
    {
        write(node);
        return false;
    }

    bool visit(class AstTypePackGeneric* node) override
    {
        write(node);
        return false;
    }

    void writeComments(std::vector<Comment> commentLocations)
    {
        bool commentComma = false;
        for (Comment comment : commentLocations)
        {
            if (commentComma)
            {
                writeRaw(",");
            }
            else
            {
                commentComma = true;
            }
            writeRaw("{");
            bool c = pushComma();
            switch (comment.type)
            {
            case Lexeme::Comment:
                writeType("Comment");
                break;
            case Lexeme::BlockComment:
                writeType("BlockComment");
                break;
            case Lexeme::BrokenComment:
                writeType("BrokenComment");
                break;
            default:
                break;
            }
            write("location", comment.location);
            popComma(c);
            writeRaw("}");
        }
    }
};

std::string toJson(AstNode* node)
{
    AstJsonEncoder encoder;
    node->visit(&encoder);
    return encoder.str();
}

std::string toJson(AstNode* node, const std::vector<Comment>& commentLocations)
{
    AstJsonEncoder encoder;
    encoder.writeRaw(R"({"root":)");
    node->visit(&encoder);
    encoder.writeRaw(R"(,"commentLocations":[)");
    encoder.writeComments(commentLocations);
    encoder.writeRaw("]}");
    return encoder.str();
}

} // namespace Luau
