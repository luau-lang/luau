// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Location.h"
#include "Luau/DenseHash.h"
#include "Luau/Common.h"

namespace Luau
{

class Allocator
{
public:
    Allocator();
    Allocator(Allocator&&);

    Allocator& operator=(Allocator&&) = delete;

    ~Allocator();

    void* allocate(size_t size);

    template<typename T, typename... Args>
    T* alloc(Args&&... args)
    {
        static_assert(std::is_trivially_destructible<T>::value, "Objects allocated with this allocator will never have their destructors run!");

        T* t = static_cast<T*>(allocate(sizeof(T)));
        new (t) T(std::forward<Args>(args)...);
        return t;
    }

private:
    struct Page
    {
        Page* next;

        char data[8192];
    };

    Page* root;
    size_t offset;
};

struct Lexeme
{
    enum Type
    {
        Eof = 0,

        // 1..255 means actual character values
        Char_END = 256,

        Equal,
        LessEqual,
        GreaterEqual,
        NotEqual,
        Dot2,
        Dot3,
        SkinnyArrow,
        DoubleColon,

        AddAssign,
        SubAssign,
        MulAssign,
        DivAssign,
        ModAssign,
        PowAssign,
        ConcatAssign,

        RawString,
        QuotedString,
        Number,
        Name,

        Comment,
        BlockComment,

        BrokenString,
        BrokenComment,
        BrokenUnicode,
        Error,

        Reserved_BEGIN,
        ReservedAnd = Reserved_BEGIN,
        ReservedBreak,
        ReservedDo,
        ReservedElse,
        ReservedElseif,
        ReservedEnd,
        ReservedFalse,
        ReservedFor,
        ReservedFunction,
        ReservedIf,
        ReservedIn,
        ReservedLocal,
        ReservedNil,
        ReservedNot,
        ReservedOr,
        ReservedRepeat,
        ReservedReturn,
        ReservedThen,
        ReservedTrue,
        ReservedUntil,
        ReservedWhile,
        Reserved_END
    };

    Type type;
    Location location;
    unsigned int length;

    union
    {
        const char* data;       // String, Number, Comment
        const char* name;       // Name
        unsigned int codepoint; // BrokenUnicode
    };

    Lexeme(const Location& location, Type type);
    Lexeme(const Location& location, char character);
    Lexeme(const Location& location, Type type, const char* data, size_t size);
    Lexeme(const Location& location, Type type, const char* name);

    std::string toString() const;
};

class AstNameTable
{
public:
    AstNameTable(Allocator& allocator);

    AstName addStatic(const char* name, Lexeme::Type type = Lexeme::Name);

    std::pair<AstName, Lexeme::Type> getOrAddWithType(const char* name, size_t length);
    std::pair<AstName, Lexeme::Type> getWithType(const char* name, size_t length) const;

    AstName getOrAdd(const char* name);
    AstName get(const char* name) const;

private:
    struct Entry
    {
        AstName value;
        uint32_t length;
        Lexeme::Type type;

        bool operator==(const Entry& other) const;
    };

    struct EntryHash
    {
        size_t operator()(const Entry& e) const;
    };

    DenseHashSet<Entry, EntryHash> data;

    Allocator& allocator;
};

class Lexer
{
public:
    Lexer(const char* buffer, std::size_t bufferSize, AstNameTable& names);

    void setSkipComments(bool skip);
    void setReadNames(bool read);

    const Location& previousLocation() const
    {
        return prevLocation;
    }

    const Lexeme& next();
    const Lexeme& next(bool skipComments, bool updatePrevLocation);
    void nextline();

    Lexeme lookahead();

    const Lexeme& current() const
    {
        return lexeme;
    }

    static bool isReserved(const std::string& word);

    static bool fixupQuotedString(std::string& data);
    static void fixupMultilineString(std::string& data);

private:
    char peekch() const;
    char peekch(unsigned int lookahead) const;

    Position position() const;

    void consume();

    Lexeme readCommentBody();

    // Given a sequence [===[ or ]===], returns:
    // 1. number of equal signs (or 0 if none present) between the brackets
    // 2. -1 if this is not a long comment/string separator
    // 3. -N if this is a malformed separator
    // Does *not* consume the closing brace.
    int skipLongSeparator();

    Lexeme readLongString(const Position& start, int sep, Lexeme::Type ok, Lexeme::Type broken);
    Lexeme readQuotedString();

    std::pair<AstName, Lexeme::Type> readName();

    Lexeme readNumber(const Position& start, unsigned int startOffset);

    Lexeme readUtf8Error();
    Lexeme readNext();

    const char* buffer;
    std::size_t bufferSize;

    unsigned int offset;

    unsigned int line;
    unsigned int lineOffset;

    Lexeme lexeme;

    Location prevLocation;

    AstNameTable& names;

    bool skipComments;
    bool readNames;
};

inline bool isSpace(char ch)
{
    return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' || ch == '\v' || ch == '\f';
}

} // namespace Luau
