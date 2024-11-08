// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Location.h"

#include <iterator>
#include <optional>
#include <functional>
#include <string>

#include <string.h>
#include <stdint.h>

namespace Luau
{

struct AstName
{
    const char* value;

    AstName()
        : value(nullptr)
    {
    }

    explicit AstName(const char* value)
        : value(value)
    {
    }

    bool operator==(const AstName& rhs) const
    {
        return value == rhs.value;
    }

    bool operator!=(const AstName& rhs) const
    {
        return value != rhs.value;
    }

    bool operator==(const char* rhs) const
    {
        return value && strcmp(value, rhs) == 0;
    }

    bool operator!=(const char* rhs) const
    {
        return !value || strcmp(value, rhs) != 0;
    }

    bool operator<(const AstName& rhs) const
    {
        return (value && rhs.value) ? strcmp(value, rhs.value) < 0 : value < rhs.value;
    }
};

class AstType;
class AstVisitor;
class AstStat;
class AstStatBlock;
class AstExpr;
class AstTypePack;
class AstAttr;
class AstExprTable;

struct AstLocal
{
    AstName name;
    Location location;
    AstLocal* shadow;
    size_t functionDepth;
    size_t loopDepth;

    AstType* annotation;

    AstLocal(const AstName& name, const Location& location, AstLocal* shadow, size_t functionDepth, size_t loopDepth, AstType* annotation)
        : name(name)
        , location(location)
        , shadow(shadow)
        , functionDepth(functionDepth)
        , loopDepth(loopDepth)
        , annotation(annotation)
    {
    }
};

template<typename T>
struct AstArray
{
    T* data;
    size_t size;

    const T* begin() const
    {
        return data;
    }

    const T* end() const
    {
        return data + size;
    }

    std::reverse_iterator<const T*> rbegin() const
    {
        return std::make_reverse_iterator(end());
    }

    std::reverse_iterator<const T*> rend() const
    {
        return std::make_reverse_iterator(begin());
    }
};

struct AstTypeList
{
    AstArray<AstType*> types;
    // Null indicates no tail, not an untyped tail.
    AstTypePack* tailType = nullptr;
};

using AstArgumentName = std::pair<AstName, Location>; // TODO: remove and replace when we get a common struct for this pair instead of AstName

struct AstGenericType
{
    AstName name;
    Location location;
    AstType* defaultValue = nullptr;
};

struct AstGenericTypePack
{
    AstName name;
    Location location;
    AstTypePack* defaultValue = nullptr;
};

extern int gAstRttiIndex;

template<typename T>
struct AstRtti
{
    static const int value;
};

template<typename T>
const int AstRtti<T>::value = ++gAstRttiIndex;

#define LUAU_RTTI(Class) \
    static int ClassIndex() \
    { \
        return AstRtti<Class>::value; \
    }

class AstNode
{
public:
    explicit AstNode(int classIndex, const Location& location)
        : classIndex(classIndex)
        , location(location)
    {
    }

    virtual void visit(AstVisitor* visitor) = 0;

    virtual AstExpr* asExpr()
    {
        return nullptr;
    }
    virtual AstStat* asStat()
    {
        return nullptr;
    }
    virtual AstType* asType()
    {
        return nullptr;
    }
    virtual AstAttr* asAttr()
    {
        return nullptr;
    }

    template<typename T>
    bool is() const
    {
        return classIndex == T::ClassIndex();
    }
    template<typename T>
    T* as()
    {
        return classIndex == T::ClassIndex() ? static_cast<T*>(this) : nullptr;
    }
    template<typename T>
    const T* as() const
    {
        return classIndex == T::ClassIndex() ? static_cast<const T*>(this) : nullptr;
    }

    const int classIndex;
    Location location;
};

class AstAttr : public AstNode
{
public:
    LUAU_RTTI(AstAttr)

    enum Type
    {
        Checked,
        Native,
    };

    AstAttr(const Location& location, Type type);

    AstAttr* asAttr() override
    {
        return this;
    }

    void visit(AstVisitor* visitor) override;

    Type type;
};

class AstExpr : public AstNode
{
public:
    explicit AstExpr(int classIndex, const Location& location)
        : AstNode(classIndex, location)
    {
    }

    AstExpr* asExpr() override
    {
        return this;
    }
};

class AstStat : public AstNode
{
public:
    explicit AstStat(int classIndex, const Location& location)
        : AstNode(classIndex, location)
        , hasSemicolon(false)
    {
    }

    AstStat* asStat() override
    {
        return this;
    }

    bool hasSemicolon;
};

class AstExprGroup : public AstExpr
{
public:
    LUAU_RTTI(AstExprGroup)

    explicit AstExprGroup(const Location& location, AstExpr* expr);

    void visit(AstVisitor* visitor) override;

    AstExpr* expr;
};

class AstExprConstantNil : public AstExpr
{
public:
    LUAU_RTTI(AstExprConstantNil)

    explicit AstExprConstantNil(const Location& location);

    void visit(AstVisitor* visitor) override;
};

class AstExprConstantBool : public AstExpr
{
public:
    LUAU_RTTI(AstExprConstantBool)

    AstExprConstantBool(const Location& location, bool value);

    void visit(AstVisitor* visitor) override;

    bool value;
};

enum class ConstantNumberParseResult
{
    Ok,
    Imprecise,
    Malformed,
    BinOverflow,
    HexOverflow,
};

class AstExprConstantNumber : public AstExpr
{
public:
    LUAU_RTTI(AstExprConstantNumber)

    AstExprConstantNumber(const Location& location, double value, ConstantNumberParseResult parseResult = ConstantNumberParseResult::Ok);

    void visit(AstVisitor* visitor) override;

    double value;
    ConstantNumberParseResult parseResult;
};

class AstExprConstantString : public AstExpr
{
public:
    LUAU_RTTI(AstExprConstantString)

    enum QuoteStyle
    {
        QuotedSimple,
        QuotedRaw,
        Unquoted
    };

    AstExprConstantString(const Location& location, const AstArray<char>& value, QuoteStyle quoteStyle);

    void visit(AstVisitor* visitor) override;
    bool isQuoted() const;

    AstArray<char> value;
    QuoteStyle quoteStyle;
};

class AstExprLocal : public AstExpr
{
public:
    LUAU_RTTI(AstExprLocal)

    AstExprLocal(const Location& location, AstLocal* local, bool upvalue);

    void visit(AstVisitor* visitor) override;

    AstLocal* local;
    bool upvalue;
};

class AstExprGlobal : public AstExpr
{
public:
    LUAU_RTTI(AstExprGlobal)

    AstExprGlobal(const Location& location, const AstName& name);

    void visit(AstVisitor* visitor) override;

    AstName name;
};

class AstExprVarargs : public AstExpr
{
public:
    LUAU_RTTI(AstExprVarargs)

    AstExprVarargs(const Location& location);

    void visit(AstVisitor* visitor) override;
};

class AstExprCall : public AstExpr
{
public:
    LUAU_RTTI(AstExprCall)

    AstExprCall(const Location& location, AstExpr* func, const AstArray<AstExpr*>& args, bool self, const Location& argLocation);

    void visit(AstVisitor* visitor) override;

    AstExpr* func;
    AstArray<AstExpr*> args;
    bool self;
    Location argLocation;
};

class AstExprIndexName : public AstExpr
{
public:
    LUAU_RTTI(AstExprIndexName)

    AstExprIndexName(
        const Location& location,
        AstExpr* expr,
        const AstName& index,
        const Location& indexLocation,
        const Position& opPosition,
        char op
    );

    void visit(AstVisitor* visitor) override;

    AstExpr* expr;
    AstName index;
    Location indexLocation;
    Position opPosition;
    char op = '.';
};

class AstExprIndexExpr : public AstExpr
{
public:
    LUAU_RTTI(AstExprIndexExpr)

    AstExprIndexExpr(const Location& location, AstExpr* expr, AstExpr* index);

    void visit(AstVisitor* visitor) override;

    AstExpr* expr;
    AstExpr* index;
};

class AstExprFunction : public AstExpr
{
public:
    LUAU_RTTI(AstExprFunction)

    AstExprFunction(
        const Location& location,
        const AstArray<AstAttr*>& attributes,
        const AstArray<AstGenericType>& generics,
        const AstArray<AstGenericTypePack>& genericPacks,
        AstLocal* self,
        const AstArray<AstLocal*>& args,
        bool vararg,
        const Location& varargLocation,
        AstStatBlock* body,
        size_t functionDepth,
        const AstName& debugname,
        const std::optional<AstTypeList>& returnAnnotation = {},
        AstTypePack* varargAnnotation = nullptr,
        const std::optional<Location>& argLocation = std::nullopt
    );

    void visit(AstVisitor* visitor) override;

    bool hasNativeAttribute() const;

    AstArray<AstAttr*> attributes;
    AstArray<AstGenericType> generics;
    AstArray<AstGenericTypePack> genericPacks;
    AstLocal* self;
    AstArray<AstLocal*> args;
    std::optional<AstTypeList> returnAnnotation;
    bool vararg = false;
    Location varargLocation;
    AstTypePack* varargAnnotation;

    AstStatBlock* body;

    size_t functionDepth;

    AstName debugname;

    std::optional<Location> argLocation;
};

class AstExprTable : public AstExpr
{
public:
    LUAU_RTTI(AstExprTable)

    struct Item
    {
        enum Kind
        {
            List,    // foo, in which case key is a nullptr
            Record,  // foo=bar, in which case key is a AstExprConstantString
            General, // [foo]=bar
        };

        Kind kind;

        AstExpr* key; // can be nullptr!
        AstExpr* value;
    };

    AstExprTable(const Location& location, const AstArray<Item>& items);

    void visit(AstVisitor* visitor) override;

    AstArray<Item> items;
};

class AstExprUnary : public AstExpr
{
public:
    LUAU_RTTI(AstExprUnary)

    enum Op
    {
        Not,
        Minus,
        Len
    };

    AstExprUnary(const Location& location, Op op, AstExpr* expr);

    void visit(AstVisitor* visitor) override;

    Op op;
    AstExpr* expr;
};

std::string toString(AstExprUnary::Op op);

class AstExprBinary : public AstExpr
{
public:
    LUAU_RTTI(AstExprBinary)

    enum Op
    {
        Add,
        Sub,
        Mul,
        Div,
        FloorDiv,
        Mod,
        Pow,
        Concat,
        CompareNe,
        CompareEq,
        CompareLt,
        CompareLe,
        CompareGt,
        CompareGe,
        And,
        Or,

        Op__Count
    };

    AstExprBinary(const Location& location, Op op, AstExpr* left, AstExpr* right);

    void visit(AstVisitor* visitor) override;

    Op op;
    AstExpr* left;
    AstExpr* right;
};

std::string toString(AstExprBinary::Op op);

class AstExprTypeAssertion : public AstExpr
{
public:
    LUAU_RTTI(AstExprTypeAssertion)

    AstExprTypeAssertion(const Location& location, AstExpr* expr, AstType* annotation);

    void visit(AstVisitor* visitor) override;

    AstExpr* expr;
    AstType* annotation;
};

class AstExprIfElse : public AstExpr
{
public:
    LUAU_RTTI(AstExprIfElse)

    AstExprIfElse(const Location& location, AstExpr* condition, bool hasThen, AstExpr* trueExpr, bool hasElse, AstExpr* falseExpr);

    void visit(AstVisitor* visitor) override;

    AstExpr* condition;
    bool hasThen;
    AstExpr* trueExpr;
    bool hasElse;
    AstExpr* falseExpr;
};

class AstExprInterpString : public AstExpr
{
public:
    LUAU_RTTI(AstExprInterpString)

    AstExprInterpString(const Location& location, const AstArray<AstArray<char>>& strings, const AstArray<AstExpr*>& expressions);

    void visit(AstVisitor* visitor) override;

    /// An interpolated string such as `foo{bar}baz` is represented as
    /// an array of strings for "foo" and "bar", and an array of expressions for "baz".
    /// `strings` will always have one more element than `expressions`.
    AstArray<AstArray<char>> strings;
    AstArray<AstExpr*> expressions;
};

class AstStatBlock : public AstStat
{
public:
    LUAU_RTTI(AstStatBlock)

    AstStatBlock(const Location& location, const AstArray<AstStat*>& body, bool hasEnd = true);

    void visit(AstVisitor* visitor) override;

    AstArray<AstStat*> body;

    /* Indicates whether or not this block has been terminated in a
     * syntactically valid way.
     *
     * This is usually but not always done with the 'end' keyword.  AstStatIf
     * and AstStatRepeat are the two main exceptions to this.
     *
     * The 'then' clause of an if statement can properly be closed by the
     * keywords 'else' or 'elseif'.  A 'repeat' loop's body is closed with the
     * 'until' keyword.
     */
    bool hasEnd = false;
};

class AstStatIf : public AstStat
{
public:
    LUAU_RTTI(AstStatIf)

    AstStatIf(
        const Location& location,
        AstExpr* condition,
        AstStatBlock* thenbody,
        AstStat* elsebody,
        const std::optional<Location>& thenLocation,
        const std::optional<Location>& elseLocation
    );

    void visit(AstVisitor* visitor) override;

    AstExpr* condition;
    AstStatBlock* thenbody;
    AstStat* elsebody;

    std::optional<Location> thenLocation;

    // Active for 'elseif' as well
    std::optional<Location> elseLocation;
};

class AstStatWhile : public AstStat
{
public:
    LUAU_RTTI(AstStatWhile)

    AstStatWhile(const Location& location, AstExpr* condition, AstStatBlock* body, bool hasDo, const Location& doLocation);

    void visit(AstVisitor* visitor) override;

    AstExpr* condition;
    AstStatBlock* body;

    bool hasDo = false;
    Location doLocation;
};

class AstStatRepeat : public AstStat
{
public:
    LUAU_RTTI(AstStatRepeat)

    AstStatRepeat(const Location& location, AstExpr* condition, AstStatBlock* body, bool DEPRECATED_hasUntil);

    void visit(AstVisitor* visitor) override;

    AstExpr* condition;
    AstStatBlock* body;

    bool DEPRECATED_hasUntil = false;
};

class AstStatBreak : public AstStat
{
public:
    LUAU_RTTI(AstStatBreak)

    AstStatBreak(const Location& location);

    void visit(AstVisitor* visitor) override;
};

class AstStatContinue : public AstStat
{
public:
    LUAU_RTTI(AstStatContinue)

    AstStatContinue(const Location& location);

    void visit(AstVisitor* visitor) override;
};

class AstStatReturn : public AstStat
{
public:
    LUAU_RTTI(AstStatReturn)

    AstStatReturn(const Location& location, const AstArray<AstExpr*>& list);

    void visit(AstVisitor* visitor) override;

    AstArray<AstExpr*> list;
};

class AstStatExpr : public AstStat
{
public:
    LUAU_RTTI(AstStatExpr)

    AstStatExpr(const Location& location, AstExpr* expr);

    void visit(AstVisitor* visitor) override;

    AstExpr* expr;
};

class AstStatLocal : public AstStat
{
public:
    LUAU_RTTI(AstStatLocal)

    AstStatLocal(
        const Location& location,
        const AstArray<AstLocal*>& vars,
        const AstArray<AstExpr*>& values,
        const std::optional<Location>& equalsSignLocation
    );

    void visit(AstVisitor* visitor) override;

    AstArray<AstLocal*> vars;
    AstArray<AstExpr*> values;

    std::optional<Location> equalsSignLocation;
};

class AstStatFor : public AstStat
{
public:
    LUAU_RTTI(AstStatFor)

    AstStatFor(
        const Location& location,
        AstLocal* var,
        AstExpr* from,
        AstExpr* to,
        AstExpr* step,
        AstStatBlock* body,
        bool hasDo,
        const Location& doLocation
    );

    void visit(AstVisitor* visitor) override;

    AstLocal* var;
    AstExpr* from;
    AstExpr* to;
    AstExpr* step;
    AstStatBlock* body;

    bool hasDo = false;
    Location doLocation;
};

class AstStatForIn : public AstStat
{
public:
    LUAU_RTTI(AstStatForIn)

    AstStatForIn(
        const Location& location,
        const AstArray<AstLocal*>& vars,
        const AstArray<AstExpr*>& values,
        AstStatBlock* body,
        bool hasIn,
        const Location& inLocation,
        bool hasDo,
        const Location& doLocation
    );

    void visit(AstVisitor* visitor) override;

    AstArray<AstLocal*> vars;
    AstArray<AstExpr*> values;
    AstStatBlock* body;

    bool hasIn = false;
    Location inLocation;

    bool hasDo = false;
    Location doLocation;
};

class AstStatAssign : public AstStat
{
public:
    LUAU_RTTI(AstStatAssign)

    AstStatAssign(const Location& location, const AstArray<AstExpr*>& vars, const AstArray<AstExpr*>& values);

    void visit(AstVisitor* visitor) override;

    AstArray<AstExpr*> vars;
    AstArray<AstExpr*> values;
};

class AstStatCompoundAssign : public AstStat
{
public:
    LUAU_RTTI(AstStatCompoundAssign)

    AstStatCompoundAssign(const Location& location, AstExprBinary::Op op, AstExpr* var, AstExpr* value);

    void visit(AstVisitor* visitor) override;

    AstExprBinary::Op op;
    AstExpr* var;
    AstExpr* value;
};

class AstStatFunction : public AstStat
{
public:
    LUAU_RTTI(AstStatFunction)

    AstStatFunction(const Location& location, AstExpr* name, AstExprFunction* func);

    void visit(AstVisitor* visitor) override;

    AstExpr* name;
    AstExprFunction* func;
};

class AstStatLocalFunction : public AstStat
{
public:
    LUAU_RTTI(AstStatLocalFunction)

    AstStatLocalFunction(const Location& location, AstLocal* name, AstExprFunction* func);

    void visit(AstVisitor* visitor) override;

    AstLocal* name;
    AstExprFunction* func;
};

class AstStatTypeAlias : public AstStat
{
public:
    LUAU_RTTI(AstStatTypeAlias)

    AstStatTypeAlias(
        const Location& location,
        const AstName& name,
        const Location& nameLocation,
        const AstArray<AstGenericType>& generics,
        const AstArray<AstGenericTypePack>& genericPacks,
        AstType* type,
        bool exported
    );

    void visit(AstVisitor* visitor) override;

    AstName name;
    Location nameLocation;
    AstArray<AstGenericType> generics;
    AstArray<AstGenericTypePack> genericPacks;
    AstType* type;
    bool exported;
};

class AstStatTypeFunction : public AstStat
{
public:
    LUAU_RTTI(AstStatTypeFunction);

    AstStatTypeFunction(const Location& location, const AstName& name, const Location& nameLocation, AstExprFunction* body, bool exported);

    void visit(AstVisitor* visitor) override;

    AstName name;
    Location nameLocation;
    AstExprFunction* body;
    bool exported;
};

class AstStatDeclareGlobal : public AstStat
{
public:
    LUAU_RTTI(AstStatDeclareGlobal)

    AstStatDeclareGlobal(const Location& location, const AstName& name, const Location& nameLocation, AstType* type);

    void visit(AstVisitor* visitor) override;

    AstName name;
    Location nameLocation;
    AstType* type;
};

class AstStatDeclareFunction : public AstStat
{
public:
    LUAU_RTTI(AstStatDeclareFunction)

    AstStatDeclareFunction(
        const Location& location,
        const AstName& name,
        const Location& nameLocation,
        const AstArray<AstGenericType>& generics,
        const AstArray<AstGenericTypePack>& genericPacks,
        const AstTypeList& params,
        const AstArray<AstArgumentName>& paramNames,
        bool vararg,
        const Location& varargLocation,
        const AstTypeList& retTypes
    );

    AstStatDeclareFunction(
        const Location& location,
        const AstArray<AstAttr*>& attributes,
        const AstName& name,
        const Location& nameLocation,
        const AstArray<AstGenericType>& generics,
        const AstArray<AstGenericTypePack>& genericPacks,
        const AstTypeList& params,
        const AstArray<AstArgumentName>& paramNames,
        bool vararg,
        const Location& varargLocation,
        const AstTypeList& retTypes
    );


    void visit(AstVisitor* visitor) override;

    bool isCheckedFunction() const;

    AstArray<AstAttr*> attributes;
    AstName name;
    Location nameLocation;
    AstArray<AstGenericType> generics;
    AstArray<AstGenericTypePack> genericPacks;
    AstTypeList params;
    AstArray<AstArgumentName> paramNames;
    bool vararg = false;
    Location varargLocation;
    AstTypeList retTypes;
};

struct AstDeclaredClassProp
{
    AstName name;
    Location nameLocation;
    AstType* ty = nullptr;
    bool isMethod = false;
    Location location;
};

enum class AstTableAccess
{
    Read = 0b01,
    Write = 0b10,
    ReadWrite = 0b11,
};

struct AstTableIndexer
{
    AstType* indexType;
    AstType* resultType;
    Location location;

    AstTableAccess access = AstTableAccess::ReadWrite;
    std::optional<Location> accessLocation;
};

class AstStatDeclareClass : public AstStat
{
public:
    LUAU_RTTI(AstStatDeclareClass)

    AstStatDeclareClass(
        const Location& location,
        const AstName& name,
        std::optional<AstName> superName,
        const AstArray<AstDeclaredClassProp>& props,
        AstTableIndexer* indexer = nullptr
    );

    void visit(AstVisitor* visitor) override;

    AstName name;
    std::optional<AstName> superName;

    AstArray<AstDeclaredClassProp> props;
    AstTableIndexer* indexer;
};

class AstType : public AstNode
{
public:
    AstType(int classIndex, const Location& location)
        : AstNode(classIndex, location)
    {
    }

    AstType* asType() override
    {
        return this;
    }
};

// Don't have Luau::Variant available, it's a bit of an overhead, but a plain struct is nice to use
struct AstTypeOrPack
{
    AstType* type = nullptr;
    AstTypePack* typePack = nullptr;
};

class AstTypeReference : public AstType
{
public:
    LUAU_RTTI(AstTypeReference)

    AstTypeReference(
        const Location& location,
        std::optional<AstName> prefix,
        AstName name,
        std::optional<Location> prefixLocation,
        const Location& nameLocation,
        bool hasParameterList = false,
        const AstArray<AstTypeOrPack>& parameters = {}
    );

    void visit(AstVisitor* visitor) override;

    bool hasParameterList;
    std::optional<AstName> prefix;
    std::optional<Location> prefixLocation;
    AstName name;
    Location nameLocation;
    AstArray<AstTypeOrPack> parameters;
};

struct AstTableProp
{
    AstName name;
    Location location;
    AstType* type;
    AstTableAccess access = AstTableAccess::ReadWrite;
    std::optional<Location> accessLocation;
};

class AstTypeTable : public AstType
{
public:
    LUAU_RTTI(AstTypeTable)

    AstTypeTable(const Location& location, const AstArray<AstTableProp>& props, AstTableIndexer* indexer = nullptr);

    void visit(AstVisitor* visitor) override;

    AstArray<AstTableProp> props;
    AstTableIndexer* indexer;
};

class AstTypeFunction : public AstType
{
public:
    LUAU_RTTI(AstTypeFunction)

    AstTypeFunction(
        const Location& location,
        const AstArray<AstGenericType>& generics,
        const AstArray<AstGenericTypePack>& genericPacks,
        const AstTypeList& argTypes,
        const AstArray<std::optional<AstArgumentName>>& argNames,
        const AstTypeList& returnTypes
    );

    AstTypeFunction(
        const Location& location,
        const AstArray<AstAttr*>& attributes,
        const AstArray<AstGenericType>& generics,
        const AstArray<AstGenericTypePack>& genericPacks,
        const AstTypeList& argTypes,
        const AstArray<std::optional<AstArgumentName>>& argNames,
        const AstTypeList& returnTypes
    );

    void visit(AstVisitor* visitor) override;

    bool isCheckedFunction() const;

    AstArray<AstAttr*> attributes;
    AstArray<AstGenericType> generics;
    AstArray<AstGenericTypePack> genericPacks;
    AstTypeList argTypes;
    AstArray<std::optional<AstArgumentName>> argNames;
    AstTypeList returnTypes;
};

class AstTypeTypeof : public AstType
{
public:
    LUAU_RTTI(AstTypeTypeof)

    AstTypeTypeof(const Location& location, AstExpr* expr);

    void visit(AstVisitor* visitor) override;

    AstExpr* expr;
};

class AstTypeUnion : public AstType
{
public:
    LUAU_RTTI(AstTypeUnion)

    AstTypeUnion(const Location& location, const AstArray<AstType*>& types);

    void visit(AstVisitor* visitor) override;

    AstArray<AstType*> types;
};

class AstTypeIntersection : public AstType
{
public:
    LUAU_RTTI(AstTypeIntersection)

    AstTypeIntersection(const Location& location, const AstArray<AstType*>& types);

    void visit(AstVisitor* visitor) override;

    AstArray<AstType*> types;
};

class AstExprError : public AstExpr
{
public:
    LUAU_RTTI(AstExprError)

    AstExprError(const Location& location, const AstArray<AstExpr*>& expressions, unsigned messageIndex);

    void visit(AstVisitor* visitor) override;

    AstArray<AstExpr*> expressions;
    unsigned messageIndex;
};

class AstStatError : public AstStat
{
public:
    LUAU_RTTI(AstStatError)

    AstStatError(const Location& location, const AstArray<AstExpr*>& expressions, const AstArray<AstStat*>& statements, unsigned messageIndex);

    void visit(AstVisitor* visitor) override;

    AstArray<AstExpr*> expressions;
    AstArray<AstStat*> statements;
    unsigned messageIndex;
};

class AstTypeError : public AstType
{
public:
    LUAU_RTTI(AstTypeError)

    AstTypeError(const Location& location, const AstArray<AstType*>& types, bool isMissing, unsigned messageIndex);

    void visit(AstVisitor* visitor) override;

    AstArray<AstType*> types;
    bool isMissing;
    unsigned messageIndex;
};

class AstTypeSingletonBool : public AstType
{
public:
    LUAU_RTTI(AstTypeSingletonBool)

    AstTypeSingletonBool(const Location& location, bool value);

    void visit(AstVisitor* visitor) override;

    bool value;
};

class AstTypeSingletonString : public AstType
{
public:
    LUAU_RTTI(AstTypeSingletonString)

    AstTypeSingletonString(const Location& location, const AstArray<char>& value);

    void visit(AstVisitor* visitor) override;

    const AstArray<char> value;
};

class AstTypePack : public AstNode
{
public:
    AstTypePack(int classIndex, const Location& location)
        : AstNode(classIndex, location)
    {
    }
};

class AstTypePackExplicit : public AstTypePack
{
public:
    LUAU_RTTI(AstTypePackExplicit)

    AstTypePackExplicit(const Location& location, AstTypeList typeList);

    void visit(AstVisitor* visitor) override;

    AstTypeList typeList;
};

class AstTypePackVariadic : public AstTypePack
{
public:
    LUAU_RTTI(AstTypePackVariadic)

    AstTypePackVariadic(const Location& location, AstType* variadicType);

    void visit(AstVisitor* visitor) override;

    AstType* variadicType;
};

class AstTypePackGeneric : public AstTypePack
{
public:
    LUAU_RTTI(AstTypePackGeneric)

    AstTypePackGeneric(const Location& location, AstName name);

    void visit(AstVisitor* visitor) override;

    AstName genericName;
};

class AstVisitor
{
public:
    virtual ~AstVisitor() {}

    virtual bool visit(class AstNode*)
    {
        return true;
    }

    virtual bool visit(class AstAttr* node)
    {
        return visit(static_cast<AstNode*>(node));
    }

    virtual bool visit(class AstExpr* node)
    {
        return visit(static_cast<AstNode*>(node));
    }

    virtual bool visit(class AstExprGroup* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprConstantNil* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprConstantBool* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprConstantNumber* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprConstantString* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprLocal* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprGlobal* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprVarargs* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprCall* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprIndexName* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprIndexExpr* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprFunction* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprTable* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprUnary* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprBinary* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprTypeAssertion* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprIfElse* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprInterpString* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }
    virtual bool visit(class AstExprError* node)
    {
        return visit(static_cast<AstExpr*>(node));
    }

    virtual bool visit(class AstStat* node)
    {
        return visit(static_cast<AstNode*>(node));
    }

    virtual bool visit(class AstStatBlock* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatIf* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatWhile* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatRepeat* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatBreak* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatContinue* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatReturn* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatExpr* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatLocal* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatFor* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatForIn* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatAssign* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatCompoundAssign* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatFunction* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatLocalFunction* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatTypeAlias* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatDeclareFunction* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatDeclareGlobal* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatDeclareClass* node)
    {
        return visit(static_cast<AstStat*>(node));
    }
    virtual bool visit(class AstStatError* node)
    {
        return visit(static_cast<AstStat*>(node));
    }

    // By default visiting type annotations is disabled; override this in your visitor if you need to!
    virtual bool visit(class AstType* node)
    {
        return false;
    }

    virtual bool visit(class AstTypeReference* node)
    {
        return visit(static_cast<AstType*>(node));
    }
    virtual bool visit(class AstTypeTable* node)
    {
        return visit(static_cast<AstType*>(node));
    }
    virtual bool visit(class AstTypeFunction* node)
    {
        return visit(static_cast<AstType*>(node));
    }
    virtual bool visit(class AstTypeTypeof* node)
    {
        return visit(static_cast<AstType*>(node));
    }
    virtual bool visit(class AstTypeUnion* node)
    {
        return visit(static_cast<AstType*>(node));
    }
    virtual bool visit(class AstTypeIntersection* node)
    {
        return visit(static_cast<AstType*>(node));
    }
    virtual bool visit(class AstTypeSingletonBool* node)
    {
        return visit(static_cast<AstType*>(node));
    }
    virtual bool visit(class AstTypeSingletonString* node)
    {
        return visit(static_cast<AstType*>(node));
    }
    virtual bool visit(class AstTypeError* node)
    {
        return visit(static_cast<AstType*>(node));
    }

    virtual bool visit(class AstTypePack* node)
    {
        return false;
    }
    virtual bool visit(class AstTypePackExplicit* node)
    {
        return visit(static_cast<AstTypePack*>(node));
    }
    virtual bool visit(class AstTypePackVariadic* node)
    {
        return visit(static_cast<AstTypePack*>(node));
    }
    virtual bool visit(class AstTypePackGeneric* node)
    {
        return visit(static_cast<AstTypePack*>(node));
    }
};

bool isLValue(const AstExpr*);
AstName getIdentifier(AstExpr*);
Location getLocation(const AstTypeList& typeList);

template<typename T> // AstNode, AstExpr, AstLocal, etc
Location getLocation(AstArray<T*> array)
{
    if (0 == array.size)
        return {};

    return Location{array.data[0]->location.begin, array.data[array.size - 1]->location.end};
}

#undef LUAU_RTTI

} // namespace Luau

namespace std
{

template<>
struct hash<Luau::AstName>
{
    size_t operator()(const Luau::AstName& value) const
    {
        // note: since operator== uses pointer identity, hashing function uses it as well
        // the hasher is the same as DenseHashPointer (DenseHash.h)
        return (uintptr_t(value.value) >> 4) ^ (uintptr_t(value.value) >> 9);
    }
};

} // namespace std
