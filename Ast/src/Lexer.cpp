// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Lexer.h"

#include "Luau/Confusables.h"
#include "Luau/StringUtils.h"

#include <limits.h>

namespace Luau
{

Allocator::Allocator()
    : root(static_cast<Page*>(operator new(sizeof(Page))))
    , offset(0)
{
    root->next = nullptr;
}

Allocator::Allocator(Allocator&& rhs)
    : root(rhs.root)
    , offset(rhs.offset)
{
    rhs.root = nullptr;
    rhs.offset = 0;
}

Allocator::~Allocator()
{
    Page* page = root;

    while (page)
    {
        Page* next = page->next;

        operator delete(page);

        page = next;
    }
}

void* Allocator::allocate(size_t size)
{
    constexpr size_t align = alignof(void*) > alignof(double) ? alignof(void*) : alignof(double);

    if (root)
    {
        uintptr_t data = reinterpret_cast<uintptr_t>(root->data);
        uintptr_t result = (data + offset + align - 1) & ~(align - 1);
        if (result + size <= data + sizeof(root->data))
        {
            offset = result - data + size;
            return reinterpret_cast<void*>(result);
        }
    }

    // allocate new page
    size_t pageSize = size > sizeof(root->data) ? size : sizeof(root->data);
    void* pageData = operator new(offsetof(Page, data) + pageSize);

    Page* page = static_cast<Page*>(pageData);

    page->next = root;

    root = page;
    offset = size;

    return page->data;
}

Lexeme::Lexeme(const Location& location, Type type)
    : type(type)
    , location(location)
    , length(0)
    , data(nullptr)
{
}

Lexeme::Lexeme(const Location& location, char character)
    : type(static_cast<Type>(static_cast<unsigned char>(character)))
    , location(location)
    , length(0)
    , data(nullptr)
{
}

Lexeme::Lexeme(const Location& location, Type type, const char* data, size_t size)
    : type(type)
    , location(location)
    , length(unsigned(size))
    , data(data)
{
    LUAU_ASSERT(type == RawString || type == QuotedString || type == Number || type == Comment || type == BlockComment);
}

Lexeme::Lexeme(const Location& location, Type type, const char* name)
    : type(type)
    , location(location)
    , length(0)
    , name(name)
{
    LUAU_ASSERT(type == Name || (type >= Reserved_BEGIN && type < Lexeme::Reserved_END));
}

static const char* kReserved[] = {"and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if", "in", "local", "nil", "not", "or",
    "repeat", "return", "then", "true", "until", "while"};

std::string Lexeme::toString() const
{
    switch (type)
    {
    case Eof:
        return "<eof>";

    case Equal:
        return "'=='";

    case LessEqual:
        return "'<='";

    case GreaterEqual:
        return "'>='";

    case NotEqual:
        return "'~='";

    case Dot2:
        return "'..'";

    case Dot3:
        return "'...'";

    case SkinnyArrow:
        return "'->'";

    case DoubleColon:
        return "'::'";

    case AddAssign:
        return "'+='";

    case SubAssign:
        return "'-='";

    case MulAssign:
        return "'*='";

    case DivAssign:
        return "'/='";

    case ModAssign:
        return "'%='";

    case PowAssign:
        return "'^='";

    case ConcatAssign:
        return "'..='";

    case RawString:
    case QuotedString:
        return data ? format("\"%.*s\"", length, data) : "string";

    case Number:
        return data ? format("'%.*s'", length, data) : "number";

    case Name:
        return name ? format("'%s'", name) : "identifier";

    case Comment:
        return "comment";

    case BrokenString:
        return "malformed string";

    case BrokenComment:
        return "unfinished comment";

    case BrokenUnicode:
        if (codepoint)
        {
            if (const char* confusable = findConfusable(codepoint))
                return format("Unicode character U+%x (did you mean '%s'?)", codepoint, confusable);

            return format("Unicode character U+%x", codepoint);
        }
        else
        {
            return "invalid UTF-8 sequence";
        }

    default:
        if (type < Char_END)
            return format("'%c'", type);
        else if (type >= Reserved_BEGIN && type < Reserved_END)
            return format("'%s'", kReserved[type - Reserved_BEGIN]);
        else
            return "<unknown>";
    }
}

bool AstNameTable::Entry::operator==(const Entry& other) const
{
    return length == other.length && memcmp(value.value, other.value.value, length) == 0;
}

size_t AstNameTable::EntryHash::operator()(const Entry& e) const
{
    // FNV1a
    uint32_t hash = 2166136261;

    for (size_t i = 0; i < e.length; ++i)
    {
        hash ^= uint8_t(e.value.value[i]);
        hash *= 16777619;
    }

    return hash;
}

AstNameTable::AstNameTable(Allocator& allocator)
    : data({AstName(""), 0, Lexeme::Eof}, 128)
    , allocator(allocator)
{
    static_assert(sizeof(kReserved) / sizeof(kReserved[0]) == Lexeme::Reserved_END - Lexeme::Reserved_BEGIN);

    for (int i = Lexeme::Reserved_BEGIN; i < Lexeme::Reserved_END; ++i)
        addStatic(kReserved[i - Lexeme::Reserved_BEGIN], static_cast<Lexeme::Type>(i));
}

AstName AstNameTable::addStatic(const char* name, Lexeme::Type type)
{
    AstNameTable::Entry entry = {AstName(name), uint32_t(strlen(name)), type};

    LUAU_ASSERT(!data.contains(entry));
    data.insert(entry);

    return entry.value;
}

std::pair<AstName, Lexeme::Type> AstNameTable::getOrAddWithType(const char* name, size_t length)
{
    AstNameTable::Entry key = {AstName(name), uint32_t(length), Lexeme::Eof};
    const Entry& entry = data.insert(key);

    // entry already was inserted
    if (entry.type != Lexeme::Eof)
        return std::make_pair(entry.value, entry.type);

    // we just inserted an entry with a non-owned pointer into the map
    // we need to correct it, *but* we need to be careful about not disturbing the hash value
    char* nameData = static_cast<char*>(allocator.allocate(length + 1));
    memcpy(nameData, name, length);
    nameData[length] = 0;

    const_cast<Entry&>(entry).value = AstName(nameData);
    const_cast<Entry&>(entry).type = Lexeme::Name;

    return std::make_pair(entry.value, entry.type);
}

std::pair<AstName, Lexeme::Type> AstNameTable::getWithType(const char* name, size_t length) const
{
    if (const Entry* entry = data.find({AstName(name), uint32_t(length), Lexeme::Eof}))
    {
        return std::make_pair(entry->value, entry->type);
    }
    return std::make_pair(AstName(), Lexeme::Name);
}

AstName AstNameTable::getOrAdd(const char* name)
{
    return getOrAddWithType(name, strlen(name)).first;
}

AstName AstNameTable::get(const char* name) const
{
    return getWithType(name, strlen(name)).first;
}

inline bool isAlpha(char ch)
{
    // use or trick to convert to lower case and unsigned comparison to do range check
    return unsigned((ch | ' ') - 'a') < 26;
}

inline bool isDigit(char ch)
{
    return unsigned(ch - '0') < 10;
}

inline bool isHexDigit(char ch)
{
    // use or trick to convert to lower case and unsigned comparison to do range check
    return unsigned(ch - '0') < 10 || unsigned((ch | ' ') - 'a') < 6;
}

inline bool isNewline(char ch)
{
    return ch == '\n';
}

static char unescape(char ch)
{
    switch (ch)
    {
    case 'a':
        return '\a';
    case 'b':
        return '\b';
    case 'f':
        return '\f';
    case 'n':
        return '\n';
    case 'r':
        return '\r';
    case 't':
        return '\t';
    case 'v':
        return '\v';
    default:
        return ch;
    }
}

Lexer::Lexer(const char* buffer, size_t bufferSize, AstNameTable& names)
    : buffer(buffer)
    , bufferSize(bufferSize)
    , offset(0)
    , line(0)
    , lineOffset(0)
    , lexeme(Location(Position(0, 0), 0), Lexeme::Eof)
    , names(names)
    , skipComments(false)
    , readNames(true)
{
}

void Lexer::setSkipComments(bool skip)
{
    skipComments = skip;
}

void Lexer::setReadNames(bool read)
{
    readNames = read;
}

const Lexeme& Lexer::next()
{
    return next(this->skipComments, true);
}

const Lexeme& Lexer::next(bool skipComments, bool updatePrevLocation)
{
    // in skipComments mode we reject valid comments
    do
    {
        // consume whitespace before the token
        while (isSpace(peekch()))
            consume();

        if (updatePrevLocation)
            prevLocation = lexeme.location;

        lexeme = readNext();
        updatePrevLocation = false;
    } while (skipComments && (lexeme.type == Lexeme::Comment || lexeme.type == Lexeme::BlockComment));

    return lexeme;
}

void Lexer::nextline()
{
    while (peekch() != 0 && peekch() != '\r' && !isNewline(peekch()))
        consume();

    next();
}

Lexeme Lexer::lookahead()
{
    unsigned int currentOffset = offset;
    unsigned int currentLine = line;
    unsigned int currentLineOffset = lineOffset;
    Lexeme currentLexeme = lexeme;
    Location currentPrevLocation = prevLocation;

    Lexeme result = next();

    offset = currentOffset;
    line = currentLine;
    lineOffset = currentLineOffset;
    lexeme = currentLexeme;
    prevLocation = currentPrevLocation;

    return result;
}

bool Lexer::isReserved(const std::string& word)
{
    for (int i = Lexeme::Reserved_BEGIN; i < Lexeme::Reserved_END; ++i)
        if (word == kReserved[i - Lexeme::Reserved_BEGIN])
            return true;

    return false;
}

LUAU_FORCEINLINE
char Lexer::peekch() const
{
    return (offset < bufferSize) ? buffer[offset] : 0;
}

LUAU_FORCEINLINE
char Lexer::peekch(unsigned int lookahead) const
{
    return (offset + lookahead < bufferSize) ? buffer[offset + lookahead] : 0;
}

Position Lexer::position() const
{
    return Position(line, offset - lineOffset);
}

void Lexer::consume()
{
    if (isNewline(buffer[offset]))
    {
        line++;
        lineOffset = offset + 1;
    }

    offset++;
}

Lexeme Lexer::readCommentBody()
{
    Position start = position();

    LUAU_ASSERT(peekch(0) == '-' && peekch(1) == '-');
    consume();
    consume();

    size_t startOffset = offset;

    if (peekch() == '[')
    {
        int sep = skipLongSeparator();

        if (sep >= 0)
        {
            return readLongString(start, sep, Lexeme::BlockComment, Lexeme::BrokenComment);
        }
    }

    // fall back to single-line comment
    while (peekch() != 0 && peekch() != '\r' && !isNewline(peekch()))
        consume();

    return Lexeme(Location(start, position()), Lexeme::Comment, &buffer[startOffset], offset - startOffset);
}

// Given a sequence [===[ or ]===], returns:
// 1. number of equal signs (or 0 if none present) between the brackets
// 2. -1 if this is not a long comment/string separator
// 3. -N if this is a malformed separator
// Does *not* consume the closing brace.
int Lexer::skipLongSeparator()
{
    char start = peekch();

    LUAU_ASSERT(start == '[' || start == ']');
    consume();

    int count = 0;

    while (peekch() == '=')
    {
        consume();
        count++;
    }

    return (start == peekch()) ? count : (-count) - 1;
}

Lexeme Lexer::readLongString(const Position& start, int sep, Lexeme::Type ok, Lexeme::Type broken)
{
    // skip (second) [
    LUAU_ASSERT(peekch() == '[');
    consume();

    unsigned int startOffset = offset;

    while (peekch())
    {
        if (peekch() == ']')
        {
            if (skipLongSeparator() == sep)
            {
                LUAU_ASSERT(peekch() == ']');
                consume(); // skip (second) ]

                unsigned int endOffset = offset - sep - 2;
                LUAU_ASSERT(endOffset >= startOffset);

                return Lexeme(Location(start, position()), ok, &buffer[startOffset], endOffset - startOffset);
            }
        }
        else
        {
            consume();
        }
    }

    return Lexeme(Location(start, position()), broken);
}

Lexeme Lexer::readQuotedString()
{
    Position start = position();

    char delimiter = peekch();
    LUAU_ASSERT(delimiter == '\'' || delimiter == '"');
    consume();

    unsigned int startOffset = offset;

    while (peekch() != delimiter)
    {
        switch (peekch())
        {
        case 0:
        case '\r':
        case '\n':
            return Lexeme(Location(start, position()), Lexeme::BrokenString);

        case '\\':
            consume();
            switch (peekch())
            {
            case '\r':
                consume();
                if (peekch() == '\n')
                    consume();
                break;

            case 0:
                break;

            case 'z':
                consume();
                while (isSpace(peekch()))
                    consume();
                break;

            default:
                consume();
            }
            break;

        default:
            consume();
        }
    }

    consume();

    return Lexeme(Location(start, position()), Lexeme::QuotedString, &buffer[startOffset], offset - startOffset - 1);
}

Lexeme Lexer::readNumber(const Position& start, unsigned int startOffset)
{
    LUAU_ASSERT(isDigit(peekch()));

    // This function does not do the number parsing - it only skips a number-like pattern.
    // It uses the same logic as Lua stock lexer; the resulting string is later converted
    // to a number with proper verification.
    do
    {
        consume();
    } while (isDigit(peekch()) || peekch() == '.' || peekch() == '_');

    if (peekch() == 'e' || peekch() == 'E')
    {
        consume();

        if (peekch() == '+' || peekch() == '-')
            consume();
    }

    while (isAlpha(peekch()) || isDigit(peekch()) || peekch() == '_')
        consume();

    return Lexeme(Location(start, position()), Lexeme::Number, &buffer[startOffset], offset - startOffset);
}

std::pair<AstName, Lexeme::Type> Lexer::readName()
{
    LUAU_ASSERT(isAlpha(peekch()) || peekch() == '_');

    unsigned int startOffset = offset;

    do
        consume();
    while (isAlpha(peekch()) || isDigit(peekch()) || peekch() == '_');

    return readNames ? names.getOrAddWithType(&buffer[startOffset], offset - startOffset)
                     : names.getWithType(&buffer[startOffset], offset - startOffset);
}

Lexeme Lexer::readNext()
{
    Position start = position();

    switch (peekch())
    {
    case 0:
        return Lexeme(Location(start, 0), Lexeme::Eof);

    case '-':
    {
        if (peekch(1) == '>')
        {
            consume();
            consume();
            return Lexeme(Location(start, 2), Lexeme::SkinnyArrow);
        }
        else if (peekch(1) == '=')
        {
            consume();
            consume();
            return Lexeme(Location(start, 2), Lexeme::SubAssign);
        }
        else if (peekch(1) == '-')
        {
            return readCommentBody();
        }
        else
        {
            consume();
            return Lexeme(Location(start, 1), '-');
        }
    }

    case '[':
    {
        int sep = skipLongSeparator();

        if (sep >= 0)
        {
            return readLongString(start, sep, Lexeme::RawString, Lexeme::BrokenString);
        }
        else if (sep == -1)
        {
            return Lexeme(Location(start, 1), '[');
        }
        else
        {
            return Lexeme(Location(start, position()), Lexeme::BrokenString);
        }
    }

    case '=':
    {
        consume();

        if (peekch() == '=')
        {
            consume();
            return Lexeme(Location(start, 2), Lexeme::Equal);
        }
        else
            return Lexeme(Location(start, 1), '=');
    }

    case '<':
    {
        consume();

        if (peekch() == '=')
        {
            consume();
            return Lexeme(Location(start, 2), Lexeme::LessEqual);
        }
        else
            return Lexeme(Location(start, 1), '<');
    }

    case '>':
    {
        consume();

        if (peekch() == '=')
        {
            consume();
            return Lexeme(Location(start, 2), Lexeme::GreaterEqual);
        }
        else
            return Lexeme(Location(start, 1), '>');
    }

    case '~':
    {
        consume();

        if (peekch() == '=')
        {
            consume();
            return Lexeme(Location(start, 2), Lexeme::NotEqual);
        }
        else
            return Lexeme(Location(start, 1), '~');
    }

    case '"':
    case '\'':
        return readQuotedString();

    case '.':
        consume();

        if (peekch() == '.')
        {
            consume();

            if (peekch() == '.')
            {
                consume();

                return Lexeme(Location(start, 3), Lexeme::Dot3);
            }
            else if (peekch() == '=')
            {
                consume();

                return Lexeme(Location(start, 3), Lexeme::ConcatAssign);
            }
            else
                return Lexeme(Location(start, 2), Lexeme::Dot2);
        }
        else
        {
            if (isDigit(peekch()))
            {
                return readNumber(start, offset - 1);
            }
            else
                return Lexeme(Location(start, 1), '.');
        }

    case '+':
        consume();

        if (peekch() == '=')
        {
            consume();
            return Lexeme(Location(start, 2), Lexeme::AddAssign);
        }
        else
            return Lexeme(Location(start, 1), '+');

    case '/':
        consume();

        if (peekch() == '=')
        {
            consume();
            return Lexeme(Location(start, 2), Lexeme::DivAssign);
        }
        else
            return Lexeme(Location(start, 1), '/');

    case '*':
        consume();

        if (peekch() == '=')
        {
            consume();
            return Lexeme(Location(start, 2), Lexeme::MulAssign);
        }
        else
            return Lexeme(Location(start, 1), '*');

    case '%':
        consume();

        if (peekch() == '=')
        {
            consume();
            return Lexeme(Location(start, 2), Lexeme::ModAssign);
        }
        else
            return Lexeme(Location(start, 1), '%');

    case '^':
        consume();

        if (peekch() == '=')
        {
            consume();
            return Lexeme(Location(start, 2), Lexeme::PowAssign);
        }
        else
            return Lexeme(Location(start, 1), '^');

    case ':':
    {
        consume();
        if (peekch() == ':')
        {
            consume();
            return Lexeme(Location(start, 2), Lexeme::DoubleColon);
        }
        else
            return Lexeme(Location(start, 1), ':');
    }

    case '(':
    case ')':
    case '{':
    case '}':
    case ']':
    case ';':
    case ',':
    case '#':
    {
        char ch = peekch();
        consume();

        return Lexeme(Location(start, 1), ch);
    }

    default:
        if (isDigit(peekch()))
        {
            return readNumber(start, offset);
        }
        else if (isAlpha(peekch()) || peekch() == '_')
        {
            std::pair<AstName, Lexeme::Type> name = readName();

            return Lexeme(Location(start, position()), name.second, name.first.value);
        }
        else if (peekch() & 0x80)
        {
            return readUtf8Error();
        }
        else
        {
            char ch = peekch();
            consume();

            return Lexeme(Location(start, 1), ch);
        }
    }
}

LUAU_NOINLINE Lexeme Lexer::readUtf8Error()
{
    Position start = position();
    uint32_t codepoint = 0;
    int size = 0;

    if ((peekch() & 0b10000000) == 0b00000000)
    {
        size = 1;
        codepoint = peekch() & 0x7F;
    }
    else if ((peekch() & 0b11100000) == 0b11000000)
    {
        size = 2;
        codepoint = peekch() & 0b11111;
    }
    else if ((peekch() & 0b11110000) == 0b11100000)
    {
        size = 3;
        codepoint = peekch() & 0b1111;
    }
    else if ((peekch() & 0b11111000) == 0b11110000)
    {
        size = 4;
        codepoint = peekch() & 0b111;
    }
    else
    {
        consume();
        return Lexeme(Location(start, position()), Lexeme::BrokenUnicode);
    }

    consume();

    for (int i = 1; i < size; ++i)
    {
        if ((peekch() & 0b11000000) != 0b10000000)
            return Lexeme(Location(start, position()), Lexeme::BrokenUnicode);

        codepoint = codepoint << 6;
        codepoint |= (peekch() & 0b00111111);
        consume();
    }

    Lexeme result(Location(start, position()), Lexeme::BrokenUnicode);
    result.codepoint = codepoint;
    return result;
}

static size_t toUtf8(char* data, unsigned int code)
{
    // U+0000..U+007F
    if (code < 0x80)
    {
        data[0] = char(code);
        return 1;
    }
    // U+0080..U+07FF
    else if (code < 0x800)
    {
        data[0] = char(0xC0 | (code >> 6));
        data[1] = char(0x80 | (code & 0x3F));
        return 2;
    }
    // U+0800..U+FFFF
    else if (code < 0x10000)
    {
        data[0] = char(0xE0 | (code >> 12));
        data[1] = char(0x80 | ((code >> 6) & 0x3F));
        data[2] = char(0x80 | (code & 0x3F));
        return 3;
    }
    // U+10000..U+10FFFF
    else if (code < 0x110000)
    {
        data[0] = char(0xF0 | (code >> 18));
        data[1] = char(0x80 | ((code >> 12) & 0x3F));
        data[2] = char(0x80 | ((code >> 6) & 0x3F));
        data[3] = char(0x80 | (code & 0x3F));
        return 4;
    }
    else
    {
        return 0;
    }
}

bool Lexer::fixupQuotedString(std::string& data)
{
    if (data.empty() || data.find('\\') == std::string::npos)
        return true;

    size_t size = data.size();
    size_t write = 0;

    for (size_t i = 0; i < size;)
    {
        if (data[i] != '\\')
        {
            data[write++] = data[i];
            i++;
            continue;
        }

        if (i + 1 == size)
            return false;

        char escape = data[i + 1];
        i += 2; // skip \e

        switch (escape)
        {
        case '\n':
            data[write++] = '\n';
            break;

        case '\r':
            data[write++] = '\n';
            if (i < size && data[i] == '\n')
                i++;
            break;

        case 0:
            return false;

        case 'x':
        {
            // hex escape codes are exactly 2 hex digits long
            if (i + 2 > size)
                return false;

            unsigned int code = 0;

            for (int j = 0; j < 2; ++j)
            {
                char ch = data[i + j];
                if (!isHexDigit(ch))
                    return false;

                // use or trick to convert to lower case
                code = 16 * code + (isDigit(ch) ? ch - '0' : (ch | ' ') - 'a' + 10);
            }

            data[write++] = char(code);
            i += 2;
            break;
        }

        case 'z':
        {
            while (i < size && isSpace(data[i]))
                i++;
            break;
        }

        case 'u':
        {
            // unicode escape codes are at least 3 characters including braces
            if (i + 3 > size)
                return false;

            if (data[i] != '{')
                return false;
            i++;

            if (data[i] == '}')
                return false;

            unsigned int code = 0;

            for (int j = 0; j < 16; ++j)
            {
                if (i == size)
                    return false;

                char ch = data[i];

                if (ch == '}')
                    break;

                if (!isHexDigit(ch))
                    return false;

                // use or trick to convert to lower case
                code = 16 * code + (isDigit(ch) ? ch - '0' : (ch | ' ') - 'a' + 10);
                i++;
            }

            if (i == size || data[i] != '}')
                return false;
            i++;

            size_t utf8 = toUtf8(&data[write], code);
            if (utf8 == 0)
                return false;

            write += utf8;
            break;
        }

        default:
        {
            if (isDigit(escape))
            {
                unsigned int code = escape - '0';

                for (int j = 0; j < 2; ++j)
                {
                    if (i == size || !isDigit(data[i]))
                        break;

                    code = 10 * code + (data[i] - '0');
                    i++;
                }

                if (code > UCHAR_MAX)
                    return false;

                data[write++] = char(code);
            }
            else
            {
                data[write++] = unescape(escape);
            }
        }
        }
    }

    LUAU_ASSERT(write <= size);
    data.resize(write);

    return true;
}

void Lexer::fixupMultilineString(std::string& data)
{
    if (data.empty())
        return;

    // Lua rules for multiline strings are as follows:
    // - standalone \r, \r\n, \n\r and \n are all considered newlines
    // - first newline in the multiline string is skipped
    // - all other newlines are normalized to \n

    // Since our lexer just treats \n as newlines, we apply a simplified set of rules that is sufficient to get normalized newlines for Windows/Unix:
    // - \r\n and \n are considered newlines
    // - first newline is skipped
    // - newlines are normalized to \n

    // This makes the string parsing behavior consistent with general lexing behavior - a standalone \r isn't considered a new line from the line
    // tracking perspective

    const char* src = data.c_str();
    char* dst = &data[0];

    // skip leading newline
    if (src[0] == '\r' && src[1] == '\n')
    {
        src += 2;
    }
    else if (src[0] == '\n')
    {
        src += 1;
    }

    // parse the rest of the string, converting newlines as we go
    while (*src)
    {
        if (src[0] == '\r' && src[1] == '\n')
        {
            *dst++ = '\n';
            src += 2;
        }
        else // note, this handles \n by just writing it without changes
        {
            *dst++ = *src;
            src += 1;
        }
    }

    data.resize(dst - &data[0]);
}

} // namespace Luau
