// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Parser.h"

#include "Luau/TimeTrace.h"

#include <algorithm>

// Warning: If you are introducing new syntax, ensure that it is behind a separate
// flag so that we don't break production games by reverting syntax changes.
// See docs/SyntaxChanges.md for an explanation.
LUAU_FASTINTVARIABLE(LuauRecursionLimit, 1000)
LUAU_FASTINTVARIABLE(LuauParseErrorLimit, 100)
LUAU_FASTFLAGVARIABLE(LuauParseRecoverUnexpectedPack, false)
LUAU_FASTFLAGVARIABLE(LuauParseLocationIgnoreCommentSkipInCapture, false)

namespace Luau
{

ParseError::ParseError(const Location& location, const std::string& message)
    : location(location)
    , message(message)
{
}

const char* ParseError::what() const throw()
{
    return message.c_str();
}

const Location& ParseError::getLocation() const
{
    return location;
}

const std::string& ParseError::getMessage() const
{
    return message;
}

// LUAU_NOINLINE is used to limit the stack cost of this function due to std::string object / exception plumbing
LUAU_NOINLINE void ParseError::raise(const Location& location, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    std::string message = vformat(format, args);
    va_end(args);

    throw ParseError(location, message);
}

ParseErrors::ParseErrors(std::vector<ParseError> errors)
    : errors(std::move(errors))
{
    LUAU_ASSERT(!this->errors.empty());

    if (this->errors.size() == 1)
        message = this->errors.front().what();
    else
        message = format("%d parse errors", int(this->errors.size()));
}

const char* ParseErrors::what() const throw()
{
    return message.c_str();
}

const std::vector<ParseError>& ParseErrors::getErrors() const
{
    return errors;
}

template<typename T>
TempVector<T>::TempVector(std::vector<T>& storage)
    : storage(storage)
    , offset(storage.size())
    , size_(0)
{
}

template<typename T>
TempVector<T>::~TempVector()
{
    LUAU_ASSERT(storage.size() == offset + size_);
    storage.erase(storage.begin() + offset, storage.end());
}

template<typename T>
const T& TempVector<T>::operator[](size_t index) const
{
    LUAU_ASSERT(index < size_);
    return storage[offset + index];
}

template<typename T>
const T& TempVector<T>::front() const
{
    LUAU_ASSERT(size_ > 0);
    return storage[offset];
}

template<typename T>
const T& TempVector<T>::back() const
{
    LUAU_ASSERT(size_ > 0);
    return storage.back();
}

template<typename T>
bool TempVector<T>::empty() const
{
    return size_ == 0;
}

template<typename T>
size_t TempVector<T>::size() const
{
    return size_;
}

template<typename T>
void TempVector<T>::push_back(const T& item)
{
    LUAU_ASSERT(storage.size() == offset + size_);
    storage.push_back(item);
    size_++;
}

static bool shouldParseTypePackAnnotation(Lexer& lexer)
{
    if (lexer.current().type == Lexeme::Dot3)
        return true;
    else if (lexer.current().type == Lexeme::Name && lexer.lookahead().type == Lexeme::Dot3)
        return true;

    return false;
}

ParseResult Parser::parse(const char* buffer, size_t bufferSize, AstNameTable& names, Allocator& allocator, ParseOptions options)
{
    LUAU_TIMETRACE_SCOPE("Parser::parse", "Parser");

    Parser p(buffer, bufferSize, names, allocator, options);

    try
    {
        AstStatBlock* root = p.parseChunk();

        return ParseResult{root, std::move(p.hotcomments), std::move(p.parseErrors), std::move(p.commentLocations)};
    }
    catch (ParseError& err)
    {
        // when catching a fatal error, append it to the list of non-fatal errors and return
        p.parseErrors.push_back(err);

        return ParseResult{nullptr, {}, p.parseErrors};
    }
}

Parser::Parser(const char* buffer, size_t bufferSize, AstNameTable& names, Allocator& allocator, const ParseOptions& options)
    : options(options)
    , lexer(buffer, bufferSize, names)
    , allocator(allocator)
    , recursionCounter(0)
    , endMismatchSuspect(Location(), Lexeme::Eof)
    , localMap(AstName())
{
    Function top;
    top.vararg = true;

    functionStack.reserve(8);
    functionStack.push_back(top);

    nameSelf = names.addStatic("self");
    nameNumber = names.addStatic("number");
    nameError = names.addStatic(kParseNameError);
    nameNil = names.getOrAdd("nil"); // nil is a reserved keyword

    matchRecoveryStopOnToken.assign(Lexeme::Type::Reserved_END, 0);
    matchRecoveryStopOnToken[Lexeme::Type::Eof] = 1;

    // required for lookahead() to work across a comment boundary and for nextLexeme() to work when captureComments is false
    lexer.setSkipComments(true);

    // read first lexeme (any hot comments get .header = true)
    LUAU_ASSERT(hotcommentHeader);
    nextLexeme();

    // all hot comments parsed after the first non-comment lexeme are special in that they don't affect type checking / linting mode
    hotcommentHeader = false;

    // preallocate some buffers that are very likely to grow anyway; this works around std::vector's inefficient growth policy for small arrays
    localStack.reserve(16);
    scratchStat.reserve(16);
    scratchExpr.reserve(16);
    scratchLocal.reserve(16);
    scratchBinding.reserve(16);
}

bool Parser::blockFollow(const Lexeme& l)
{
    return l.type == Lexeme::Eof || l.type == Lexeme::ReservedElse || l.type == Lexeme::ReservedElseif || l.type == Lexeme::ReservedEnd ||
           l.type == Lexeme::ReservedUntil;
}

AstStatBlock* Parser::parseChunk()
{
    AstStatBlock* result = parseBlock();

    if (lexer.current().type != Lexeme::Eof)
        expectAndConsumeFail(Lexeme::Eof, nullptr);

    return result;
}

// chunk ::= {stat [`;']} [laststat [`;']]
// block ::= chunk
AstStatBlock* Parser::parseBlock()
{
    unsigned int localsBegin = saveLocals();

    AstStatBlock* result = parseBlockNoScope();

    restoreLocals(localsBegin);

    return result;
}

static bool isStatLast(AstStat* stat)
{
    return stat->is<AstStatBreak>() || stat->is<AstStatContinue>() || stat->is<AstStatReturn>();
}

AstStatBlock* Parser::parseBlockNoScope()
{
    TempVector<AstStat*> body(scratchStat);

    const Position prevPosition = lexer.previousLocation().end;

    while (!blockFollow(lexer.current()))
    {
        unsigned int recursionCounterOld = recursionCounter;

        incrementRecursionCounter("block");

        AstStat* stat = parseStat();

        recursionCounter = recursionCounterOld;

        if (lexer.current().type == ';')
        {
            nextLexeme();
            stat->hasSemicolon = true;
        }

        body.push_back(stat);

        if (isStatLast(stat))
            break;
    }

    const Location location = Location(prevPosition, lexer.current().location.begin);

    return allocator.alloc<AstStatBlock>(location, copy(body));
}

// stat ::=
// varlist `=' explist |
// functioncall |
// do block end |
// while exp do block end |
// repeat block until exp |
// if exp then block {elseif exp then block} [else block] end |
// for binding `=' exp `,' exp [`,' exp] do block end |
// for namelist in explist do block end |
// function funcname funcbody |
// local function Name funcbody |
// local namelist [`=' explist]
// laststat ::= return [explist] | break
AstStat* Parser::parseStat()
{
    // guess the type from the token type
    switch (lexer.current().type)
    {
    case Lexeme::ReservedIf:
        return parseIf();
    case Lexeme::ReservedWhile:
        return parseWhile();
    case Lexeme::ReservedDo:
        return parseDo();
    case Lexeme::ReservedFor:
        return parseFor();
    case Lexeme::ReservedRepeat:
        return parseRepeat();
    case Lexeme::ReservedFunction:
        return parseFunctionStat();
    case Lexeme::ReservedLocal:
        return parseLocal();
    case Lexeme::ReservedReturn:
        return parseReturn();
    case Lexeme::ReservedBreak:
        return parseBreak();
    default:;
    }

    Location start = lexer.current().location;

    // we need to disambiguate a few cases, primarily assignment (lvalue = ...) vs statements-that-are calls
    AstExpr* expr = parsePrimaryExpr(/* asStatement= */ true);

    if (expr->is<AstExprCall>())
        return allocator.alloc<AstStatExpr>(expr->location, expr);

    // if the next token is , or =, it's an assignment (, means it's an assignment with multiple variables)
    if (lexer.current().type == ',' || lexer.current().type == '=')
        return parseAssignment(expr);

    // if the next token is a compound assignment operator, it's a compound assignment (these don't support multiple variables)
    if (std::optional<AstExprBinary::Op> op = parseCompoundOp(lexer.current()))
        return parseCompoundAssignment(expr, *op);

    // we know this isn't a call or an assignment; therefore it must be a context-sensitive keyword such as `type` or `continue`
    AstName ident = getIdentifier(expr);

    if (options.allowTypeAnnotations)
    {
        if (ident == "type")
            return parseTypeAlias(expr->location, /* exported =*/false);
        if (ident == "export" && lexer.current().type == Lexeme::Name && AstName(lexer.current().name) == "type")
        {
            nextLexeme();
            return parseTypeAlias(expr->location, /* exported =*/true);
        }
    }

    if (options.supportContinueStatement && ident == "continue")
        return parseContinue(expr->location);

    if (options.allowTypeAnnotations && options.allowDeclarationSyntax)
    {
        if (ident == "declare")
            return parseDeclaration(expr->location);
    }

    // skip unexpected symbol if lexer couldn't advance at all (statements are parsed in a loop)
    if (start == lexer.current().location)
        nextLexeme();

    return reportStatError(expr->location, copy({expr}), {}, "Incomplete statement: expected assignment or a function call");
}

// if exp then block {elseif exp then block} [else block] end
AstStat* Parser::parseIf()
{
    Location start = lexer.current().location;

    nextLexeme(); // if / elseif

    AstExpr* cond = parseExpr();

    Lexeme matchThen = lexer.current();
    std::optional<Location> thenLocation;
    if (expectAndConsume(Lexeme::ReservedThen, "if statement"))
        thenLocation = matchThen.location;

    AstStatBlock* thenbody = parseBlock();

    AstStat* elsebody = nullptr;
    Location end = start;
    std::optional<Location> elseLocation;
    bool hasEnd = false;

    if (lexer.current().type == Lexeme::ReservedElseif)
    {
        unsigned int recursionCounterOld = recursionCounter;
        incrementRecursionCounter("elseif");
        elseLocation = lexer.current().location;
        elsebody = parseIf();
        end = elsebody->location;
        hasEnd = elsebody->as<AstStatIf>()->hasEnd;
        recursionCounter = recursionCounterOld;
    }
    else
    {
        Lexeme matchThenElse = matchThen;

        if (lexer.current().type == Lexeme::ReservedElse)
        {
            elseLocation = lexer.current().location;
            matchThenElse = lexer.current();
            nextLexeme();

            elsebody = parseBlock();
            elsebody->location.begin = matchThenElse.location.end;
        }

        end = lexer.current().location;

        hasEnd = expectMatchEndAndConsume(Lexeme::ReservedEnd, matchThenElse);
    }

    return allocator.alloc<AstStatIf>(Location(start, end), cond, thenbody, elsebody, thenLocation, elseLocation, hasEnd);
}

// while exp do block end
AstStat* Parser::parseWhile()
{
    Location start = lexer.current().location;

    nextLexeme(); // while

    AstExpr* cond = parseExpr();

    Lexeme matchDo = lexer.current();
    bool hasDo = expectAndConsume(Lexeme::ReservedDo, "while loop");

    functionStack.back().loopDepth++;

    AstStatBlock* body = parseBlock();

    functionStack.back().loopDepth--;

    Location end = lexer.current().location;

    bool hasEnd = expectMatchEndAndConsume(Lexeme::ReservedEnd, matchDo);

    return allocator.alloc<AstStatWhile>(Location(start, end), cond, body, hasDo, matchDo.location, hasEnd);
}

// repeat block until exp
AstStat* Parser::parseRepeat()
{
    Location start = lexer.current().location;

    Lexeme matchRepeat = lexer.current();
    nextLexeme(); // repeat

    unsigned int localsBegin = saveLocals();

    functionStack.back().loopDepth++;

    AstStatBlock* body = parseBlockNoScope();

    functionStack.back().loopDepth--;

    bool hasUntil = expectMatchEndAndConsume(Lexeme::ReservedUntil, matchRepeat);

    AstExpr* cond = parseExpr();

    restoreLocals(localsBegin);

    return allocator.alloc<AstStatRepeat>(Location(start, cond->location), cond, body, hasUntil);
}

// do block end
AstStat* Parser::parseDo()
{
    Location start = lexer.current().location;

    Lexeme matchDo = lexer.current();
    nextLexeme(); // do

    AstStat* body = parseBlock();

    body->location.begin = start.begin;

    expectMatchEndAndConsume(Lexeme::ReservedEnd, matchDo);

    return body;
}

// break
AstStat* Parser::parseBreak()
{
    Location start = lexer.current().location;

    nextLexeme(); // break

    if (functionStack.back().loopDepth == 0)
        return reportStatError(start, {}, copy<AstStat*>({allocator.alloc<AstStatBreak>(start)}), "break statement must be inside a loop");

    return allocator.alloc<AstStatBreak>(start);
}

// continue
AstStat* Parser::parseContinue(const Location& start)
{
    if (functionStack.back().loopDepth == 0)
        return reportStatError(start, {}, copy<AstStat*>({allocator.alloc<AstStatContinue>(start)}), "continue statement must be inside a loop");

    // note: the token is already parsed for us!

    return allocator.alloc<AstStatContinue>(start);
}

// for binding `=' exp `,' exp [`,' exp] do block end |
// for bindinglist in explist do block end |
AstStat* Parser::parseFor()
{
    Location start = lexer.current().location;

    nextLexeme(); // for

    Binding varname = parseBinding();

    if (lexer.current().type == '=')
    {
        nextLexeme();

        AstExpr* from = parseExpr();

        expectAndConsume(',', "index range");

        AstExpr* to = parseExpr();

        AstExpr* step = nullptr;

        if (lexer.current().type == ',')
        {
            nextLexeme();

            step = parseExpr();
        }

        Lexeme matchDo = lexer.current();
        bool hasDo = expectAndConsume(Lexeme::ReservedDo, "for loop");

        unsigned int localsBegin = saveLocals();

        functionStack.back().loopDepth++;

        AstLocal* var = pushLocal(varname);

        AstStatBlock* body = parseBlock();

        functionStack.back().loopDepth--;

        restoreLocals(localsBegin);

        Location end = lexer.current().location;

        bool hasEnd = expectMatchEndAndConsume(Lexeme::ReservedEnd, matchDo);

        return allocator.alloc<AstStatFor>(Location(start, end), var, from, to, step, body, hasDo, matchDo.location, hasEnd);
    }
    else
    {
        TempVector<Binding> names(scratchBinding);
        names.push_back(varname);

        if (lexer.current().type == ',')
        {
            nextLexeme();

            parseBindingList(names);
        }

        Location inLocation = lexer.current().location;
        bool hasIn = expectAndConsume(Lexeme::ReservedIn, "for loop");

        TempVector<AstExpr*> values(scratchExpr);
        parseExprList(values);

        Lexeme matchDo = lexer.current();
        bool hasDo = expectAndConsume(Lexeme::ReservedDo, "for loop");

        unsigned int localsBegin = saveLocals();

        functionStack.back().loopDepth++;

        TempVector<AstLocal*> vars(scratchLocal);

        for (size_t i = 0; i < names.size(); ++i)
            vars.push_back(pushLocal(names[i]));

        AstStatBlock* body = parseBlock();

        functionStack.back().loopDepth--;

        restoreLocals(localsBegin);

        Location end = lexer.current().location;

        bool hasEnd = expectMatchEndAndConsume(Lexeme::ReservedEnd, matchDo);

        return allocator.alloc<AstStatForIn>(
            Location(start, end), copy(vars), copy(values), body, hasIn, inLocation, hasDo, matchDo.location, hasEnd);
    }
}

// function funcname funcbody |
// funcname ::= Name {`.' Name} [`:' Name]
AstStat* Parser::parseFunctionStat()
{
    Location start = lexer.current().location;

    Lexeme matchFunction = lexer.current();
    nextLexeme();

    AstName debugname = (lexer.current().type == Lexeme::Name) ? AstName(lexer.current().name) : AstName();

    // parse funcname into a chain of indexing operators
    AstExpr* expr = parseNameExpr("function name");

    unsigned int recursionCounterOld = recursionCounter;

    while (lexer.current().type == '.')
    {
        Position opPosition = lexer.current().location.begin;
        nextLexeme();

        Name name = parseName("field name");

        // while we could concatenate the name chain, for now let's just write the short name
        debugname = name.name;

        expr = allocator.alloc<AstExprIndexName>(Location(start, name.location), expr, name.name, name.location, opPosition, '.');

        // note: while the parser isn't recursive here, we're generating recursive structures of unbounded depth
        incrementRecursionCounter("function name");
    }

    recursionCounter = recursionCounterOld;

    // finish with :
    bool hasself = false;

    if (lexer.current().type == ':')
    {
        Position opPosition = lexer.current().location.begin;
        nextLexeme();

        Name name = parseName("method name");

        // while we could concatenate the name chain, for now let's just write the short name
        debugname = name.name;

        expr = allocator.alloc<AstExprIndexName>(Location(start, name.location), expr, name.name, name.location, opPosition, ':');

        hasself = true;
    }

    matchRecoveryStopOnToken[Lexeme::ReservedEnd]++;

    AstExprFunction* body = parseFunctionBody(hasself, matchFunction, debugname, {}).first;

    matchRecoveryStopOnToken[Lexeme::ReservedEnd]--;

    return allocator.alloc<AstStatFunction>(Location(start, body->location), expr, body);
}

// local function Name funcbody |
// local bindinglist [`=' explist]
AstStat* Parser::parseLocal()
{
    Location start = lexer.current().location;

    nextLexeme(); // local

    if (lexer.current().type == Lexeme::ReservedFunction)
    {
        Lexeme matchFunction = lexer.current();
        nextLexeme();

        // matchFunction is only used for diagnostics; to make it suitable for detecting missed indentation between
        // `local function` and `end`, we patch the token to begin at the column where `local` starts
        if (matchFunction.location.begin.line == start.begin.line)
            matchFunction.location.begin.column = start.begin.column;

        Name name = parseName("variable name");

        matchRecoveryStopOnToken[Lexeme::ReservedEnd]++;

        auto [body, var] = parseFunctionBody(false, matchFunction, name.name, name);

        matchRecoveryStopOnToken[Lexeme::ReservedEnd]--;

        Location location{start.begin, body->location.end};

        return allocator.alloc<AstStatLocalFunction>(location, var, body);
    }
    else
    {
        matchRecoveryStopOnToken['=']++;

        TempVector<Binding> names(scratchBinding);
        parseBindingList(names);

        matchRecoveryStopOnToken['=']--;

        TempVector<AstLocal*> vars(scratchLocal);

        TempVector<AstExpr*> values(scratchExpr);

        std::optional<Location> equalsSignLocation;

        if (lexer.current().type == '=')
        {
            equalsSignLocation = lexer.current().location;

            nextLexeme();

            parseExprList(values);
        }

        for (size_t i = 0; i < names.size(); ++i)
            vars.push_back(pushLocal(names[i]));

        Location end = values.empty() ? lexer.previousLocation() : values.back()->location;

        return allocator.alloc<AstStatLocal>(Location(start, end), copy(vars), copy(values), equalsSignLocation);
    }
}

// return [explist]
AstStat* Parser::parseReturn()
{
    Location start = lexer.current().location;

    nextLexeme();

    TempVector<AstExpr*> list(scratchExpr);

    if (!blockFollow(lexer.current()) && lexer.current().type != ';')
        parseExprList(list);

    Location end = list.empty() ? start : list.back()->location;

    return allocator.alloc<AstStatReturn>(Location(start, end), copy(list));
}

// type Name [`<' varlist `>'] `=' typeannotation
AstStat* Parser::parseTypeAlias(const Location& start, bool exported)
{
    // note: `type` token is already parsed for us, so we just need to parse the rest

    std::optional<Name> name = parseNameOpt("type name");

    // Use error name if the name is missing
    if (!name)
        name = Name(nameError, lexer.current().location);

    auto [generics, genericPacks] = parseGenericTypeList(/* withDefaultValues= */ true);

    expectAndConsume('=', "type alias");

    AstType* type = parseTypeAnnotation();

    return allocator.alloc<AstStatTypeAlias>(Location(start, type->location), name->name, generics, genericPacks, type, exported);
}

AstDeclaredClassProp Parser::parseDeclaredClassMethod()
{
    nextLexeme();
    Location start = lexer.current().location;
    Name fnName = parseName("function name");

    // TODO: generic method declarations CLI-39909
    AstArray<AstGenericType> generics;
    AstArray<AstGenericTypePack> genericPacks;
    generics.size = 0;
    generics.data = nullptr;
    genericPacks.size = 0;
    genericPacks.data = nullptr;

    Lexeme matchParen = lexer.current();
    expectAndConsume('(', "function parameter list start");

    TempVector<Binding> args(scratchBinding);

    std::optional<Location> vararg = std::nullopt;
    AstTypePack* varargAnnotation = nullptr;
    if (lexer.current().type != ')')
        std::tie(vararg, varargAnnotation) = parseBindingList(args, /* allowDot3 */ true);

    expectMatchAndConsume(')', matchParen);

    AstTypeList retTypes = parseOptionalReturnTypeAnnotation().value_or(AstTypeList{copy<AstType*>(nullptr, 0), nullptr});
    Location end = lexer.current().location;

    TempVector<AstType*> vars(scratchAnnotation);
    TempVector<std::optional<AstArgumentName>> varNames(scratchOptArgName);

    if (args.size() == 0 || args[0].name.name != "self" || args[0].annotation != nullptr)
    {
        return AstDeclaredClassProp{fnName.name,
            reportTypeAnnotationError(Location(start, end), {}, /*isMissing*/ false, "'self' must be present as the unannotated first parameter"),
            true};
    }

    // Skip the first index.
    for (size_t i = 1; i < args.size(); ++i)
    {
        varNames.push_back(AstArgumentName{args[i].name.name, args[i].name.location});

        if (args[i].annotation)
            vars.push_back(args[i].annotation);
        else
            vars.push_back(reportTypeAnnotationError(
                Location(start, end), {}, /*isMissing*/ false, "All declaration parameters aside from 'self' must be annotated"));
    }

    if (vararg && !varargAnnotation)
        report(start, "All declaration parameters aside from 'self' must be annotated");

    AstType* fnType = allocator.alloc<AstTypeFunction>(
        Location(start, end), generics, genericPacks, AstTypeList{copy(vars), varargAnnotation}, copy(varNames), retTypes);

    return AstDeclaredClassProp{fnName.name, fnType, true};
}

AstStat* Parser::parseDeclaration(const Location& start)
{
    // `declare` token is already parsed at this point
    if (lexer.current().type == Lexeme::ReservedFunction)
    {
        nextLexeme();
        Name globalName = parseName("global function name");

        auto [generics, genericPacks] = parseGenericTypeList(/* withDefaultValues= */ false);

        Lexeme matchParen = lexer.current();

        expectAndConsume('(', "global function declaration");

        TempVector<Binding> args(scratchBinding);

        std::optional<Location> vararg;
        AstTypePack* varargAnnotation = nullptr;

        if (lexer.current().type != ')')
            std::tie(vararg, varargAnnotation) = parseBindingList(args, /* allowDot3= */ true);

        expectMatchAndConsume(')', matchParen);

        AstTypeList retTypes = parseOptionalReturnTypeAnnotation().value_or(AstTypeList{copy<AstType*>(nullptr, 0)});
        Location end = lexer.current().location;

        TempVector<AstType*> vars(scratchAnnotation);
        TempVector<AstArgumentName> varNames(scratchArgName);

        for (size_t i = 0; i < args.size(); ++i)
        {
            if (!args[i].annotation)
                return reportStatError(Location(start, end), {}, {}, "All declaration parameters must be annotated");

            vars.push_back(args[i].annotation);
            varNames.push_back({args[i].name.name, args[i].name.location});
        }

        if (vararg && !varargAnnotation)
            return reportStatError(Location(start, end), {}, {}, "All declaration parameters must be annotated");

        return allocator.alloc<AstStatDeclareFunction>(
            Location(start, end), globalName.name, generics, genericPacks, AstTypeList{copy(vars), varargAnnotation}, copy(varNames), retTypes);
    }
    else if (AstName(lexer.current().name) == "class")
    {
        nextLexeme();
        Location classStart = lexer.current().location;
        Name className = parseName("class name");
        std::optional<AstName> superName = std::nullopt;

        if (AstName(lexer.current().name) == "extends")
        {
            nextLexeme();
            superName = parseName("superclass name").name;
        }

        TempVector<AstDeclaredClassProp> props(scratchDeclaredClassProps);

        while (lexer.current().type != Lexeme::ReservedEnd)
        {
            // There are two possibilities: Either it's a property or a function.
            if (lexer.current().type == Lexeme::ReservedFunction)
            {
                props.push_back(parseDeclaredClassMethod());
            }
            else
            {
                Name propName = parseName("property name");
                expectAndConsume(':', "property type annotation");
                AstType* propType = parseTypeAnnotation();
                props.push_back(AstDeclaredClassProp{propName.name, propType, false});
            }
        }

        Location classEnd = lexer.current().location;
        nextLexeme(); // skip past `end`

        return allocator.alloc<AstStatDeclareClass>(Location(classStart, classEnd), className.name, superName, copy(props));
    }
    else if (std::optional<Name> globalName = parseNameOpt("global variable name"))
    {
        expectAndConsume(':', "global variable declaration");

        AstType* type = parseTypeAnnotation();
        return allocator.alloc<AstStatDeclareGlobal>(Location(start, type->location), globalName->name, type);
    }
    else
    {
        return reportStatError(start, {}, {}, "declare must be followed by an identifier, 'function', or 'class'");
    }
}

static bool isExprLValue(AstExpr* expr)
{
    return expr->is<AstExprLocal>() || expr->is<AstExprGlobal>() || expr->is<AstExprIndexExpr>() || expr->is<AstExprIndexName>();
}

// varlist `=' explist
AstStat* Parser::parseAssignment(AstExpr* initial)
{
    if (!isExprLValue(initial))
        initial = reportExprError(initial->location, copy({initial}), "Assigned expression must be a variable or a field");

    TempVector<AstExpr*> vars(scratchExpr);
    vars.push_back(initial);

    while (lexer.current().type == ',')
    {
        nextLexeme();

        AstExpr* expr = parsePrimaryExpr(/* asStatement= */ true);

        if (!isExprLValue(expr))
            expr = reportExprError(expr->location, copy({expr}), "Assigned expression must be a variable or a field");

        vars.push_back(expr);
    }

    expectAndConsume('=', "assignment");

    TempVector<AstExpr*> values(scratchExprAux);
    parseExprList(values);

    return allocator.alloc<AstStatAssign>(Location(initial->location, values.back()->location), copy(vars), copy(values));
}

// var [`+=' | `-=' | `*=' | `/=' | `%=' | `^=' | `..='] exp
AstStat* Parser::parseCompoundAssignment(AstExpr* initial, AstExprBinary::Op op)
{
    if (!isExprLValue(initial))
    {
        initial = reportExprError(initial->location, copy({initial}), "Assigned expression must be a variable or a field");
    }

    nextLexeme();

    AstExpr* value = parseExpr();

    return allocator.alloc<AstStatCompoundAssign>(Location(initial->location, value->location), op, initial, value);
}

// funcbody ::= `(' [parlist] `)' [`:' ReturnType] block end
// parlist ::= bindinglist [`,' `...'] | `...'
std::pair<AstExprFunction*, AstLocal*> Parser::parseFunctionBody(
    bool hasself, const Lexeme& matchFunction, const AstName& debugname, std::optional<Name> localName)
{
    Location start = matchFunction.location;

    auto [generics, genericPacks] = parseGenericTypeList(/* withDefaultValues= */ false);

    Lexeme matchParen = lexer.current();
    expectAndConsume('(', "function");

    TempVector<Binding> args(scratchBinding);

    std::optional<Location> vararg;
    AstTypePack* varargAnnotation = nullptr;

    if (lexer.current().type != ')')
        std::tie(vararg, varargAnnotation) = parseBindingList(args, /* allowDot3= */ true);

    std::optional<Location> argLocation = matchParen.type == Lexeme::Type('(') && lexer.current().type == Lexeme::Type(')')
                                              ? std::make_optional(Location(matchParen.location.begin, lexer.current().location.end))
                                              : std::nullopt;
    expectMatchAndConsume(')', matchParen, true);

    std::optional<AstTypeList> typelist = parseOptionalReturnTypeAnnotation();

    AstLocal* funLocal = nullptr;

    if (localName)
        funLocal = pushLocal(Binding(*localName, nullptr));

    unsigned int localsBegin = saveLocals();

    Function fun;
    fun.vararg = vararg.has_value();

    functionStack.push_back(fun);

    AstLocal* self = nullptr;

    if (hasself)
        self = pushLocal(Binding(Name(nameSelf, start), nullptr));

    TempVector<AstLocal*> vars(scratchLocal);

    for (size_t i = 0; i < args.size(); ++i)
        vars.push_back(pushLocal(args[i]));

    AstStatBlock* body = parseBlock();

    functionStack.pop_back();

    restoreLocals(localsBegin);

    Location end = lexer.current().location;

    bool hasEnd = expectMatchEndAndConsume(Lexeme::ReservedEnd, matchFunction);

    return {allocator.alloc<AstExprFunction>(Location(start, end), generics, genericPacks, self, copy(vars), vararg, body, functionStack.size(),
                debugname, typelist, varargAnnotation, hasEnd, argLocation),
        funLocal};
}

// explist ::= {exp `,'} exp
void Parser::parseExprList(TempVector<AstExpr*>& result)
{
    result.push_back(parseExpr());

    while (lexer.current().type == ',')
    {
        nextLexeme();

        result.push_back(parseExpr());
    }
}

Parser::Binding Parser::parseBinding()
{
    std::optional<Name> name = parseNameOpt("variable name");

    // Use placeholder if the name is missing
    if (!name)
        name = Name(nameError, lexer.current().location);

    AstType* annotation = parseOptionalTypeAnnotation();

    return Binding(*name, annotation);
}

// bindinglist ::= (binding | `...') [`,' bindinglist]
std::pair<std::optional<Location>, AstTypePack*> Parser::parseBindingList(TempVector<Binding>& result, bool allowDot3)
{
    while (true)
    {
        if (lexer.current().type == Lexeme::Dot3 && allowDot3)
        {
            Location varargLocation = lexer.current().location;
            nextLexeme();

            AstTypePack* tailAnnotation = nullptr;
            if (lexer.current().type == ':')
            {
                nextLexeme();
                tailAnnotation = parseVariadicArgumentAnnotation();
            }

            return {varargLocation, tailAnnotation};
        }

        result.push_back(parseBinding());

        if (lexer.current().type != ',')
            break;
        nextLexeme();
    }

    return {std::nullopt, nullptr};
}

AstType* Parser::parseOptionalTypeAnnotation()
{
    if (options.allowTypeAnnotations && lexer.current().type == ':')
    {
        nextLexeme();
        return parseTypeAnnotation();
    }
    else
        return nullptr;
}

// TypeList ::= TypeAnnotation [`,' TypeList] | ...TypeAnnotation
AstTypePack* Parser::parseTypeList(TempVector<AstType*>& result, TempVector<std::optional<AstArgumentName>>& resultNames)
{
    while (true)
    {
        if (shouldParseTypePackAnnotation(lexer))
            return parseTypePackAnnotation();

        if (lexer.current().type == Lexeme::Name && lexer.lookahead().type == ':')
        {
            // Fill in previous argument names with empty slots
            while (resultNames.size() < result.size())
                resultNames.push_back({});

            resultNames.push_back(AstArgumentName{AstName(lexer.current().name), lexer.current().location});
            nextLexeme();

            expectAndConsume(':');
        }
        else if (!resultNames.empty())
        {
            // If we have a type with named arguments, provide elements for all types
            resultNames.push_back({});
        }

        result.push_back(parseTypeAnnotation());
        if (lexer.current().type != ',')
            break;
        nextLexeme();
    }

    return nullptr;
}

std::optional<AstTypeList> Parser::parseOptionalReturnTypeAnnotation()
{
    if (options.allowTypeAnnotations && lexer.current().type == ':')
    {
        nextLexeme();

        unsigned int oldRecursionCount = recursionCounter;

        auto [_location, result] = parseReturnTypeAnnotation();

        // At this point, if we find a , character, it indicates that there are multiple return types
        // in this type annotation, but the list wasn't wrapped in parentheses.
        if (lexer.current().type == ',')
        {
            report(lexer.current().location, "Expected a statement, got ','; did you forget to wrap the list of return types in parentheses?");

            nextLexeme();
        }

        recursionCounter = oldRecursionCount;

        return result;
    }

    return std::nullopt;
}

// ReturnType ::= TypeAnnotation | `(' TypeList `)'
std::pair<Location, AstTypeList> Parser::parseReturnTypeAnnotation()
{
    incrementRecursionCounter("type annotation");

    TempVector<AstType*> result(scratchAnnotation);
    TempVector<std::optional<AstArgumentName>> resultNames(scratchOptArgName);
    AstTypePack* varargAnnotation = nullptr;

    Lexeme begin = lexer.current();

    if (lexer.current().type != '(')
    {
        if (shouldParseTypePackAnnotation(lexer))
            varargAnnotation = parseTypePackAnnotation();
        else
            result.push_back(parseTypeAnnotation());

        Location resultLocation = result.size() == 0 ? varargAnnotation->location : result[0]->location;

        return {resultLocation, AstTypeList{copy(result), varargAnnotation}};
    }

    nextLexeme();

    Location innerBegin = lexer.current().location;

    matchRecoveryStopOnToken[Lexeme::SkinnyArrow]++;

    // possibly () -> ReturnType
    if (lexer.current().type != ')')
        varargAnnotation = parseTypeList(result, resultNames);

    const Location location{begin.location, lexer.current().location};

    expectMatchAndConsume(')', begin, true);

    matchRecoveryStopOnToken[Lexeme::SkinnyArrow]--;

    if (lexer.current().type != Lexeme::SkinnyArrow && resultNames.empty())
    {
        // If it turns out that it's just '(A)', it's possible that there are unions/intersections to follow, so fold over it.
        if (result.size() == 1)
        {
            AstType* returnType = parseTypeAnnotation(result, innerBegin);

            return {Location{location, returnType->location}, AstTypeList{copy(&returnType, 1), varargAnnotation}};
        }

        return {location, AstTypeList{copy(result), varargAnnotation}};
    }

    AstArray<AstGenericType> generics{nullptr, 0};
    AstArray<AstGenericTypePack> genericPacks{nullptr, 0};
    AstArray<AstType*> types = copy(result);
    AstArray<std::optional<AstArgumentName>> names = copy(resultNames);

    TempVector<AstType*> fallbackReturnTypes(scratchAnnotation);
    fallbackReturnTypes.push_back(parseFunctionTypeAnnotationTail(begin, generics, genericPacks, types, names, varargAnnotation));

    return {Location{location, fallbackReturnTypes[0]->location}, AstTypeList{copy(fallbackReturnTypes), varargAnnotation}};
}

// TableIndexer ::= `[' TypeAnnotation `]' `:' TypeAnnotation
AstTableIndexer* Parser::parseTableIndexerAnnotation()
{
    const Lexeme begin = lexer.current();
    nextLexeme(); // [

    AstType* index = parseTypeAnnotation();

    expectMatchAndConsume(']', begin);

    expectAndConsume(':', "table field");

    AstType* result = parseTypeAnnotation();

    return allocator.alloc<AstTableIndexer>(AstTableIndexer{index, result, Location(begin.location, result->location)});
}

// TableProp ::= Name `:' TypeAnnotation
// TablePropOrIndexer ::= TableProp | TableIndexer
// PropList ::= TablePropOrIndexer {fieldsep TablePropOrIndexer} [fieldsep]
// TableTypeAnnotation ::= `{' PropList `}'
AstType* Parser::parseTableTypeAnnotation()
{
    incrementRecursionCounter("type annotation");

    TempVector<AstTableProp> props(scratchTableTypeProps);
    AstTableIndexer* indexer = nullptr;

    Location start = lexer.current().location;

    Lexeme matchBrace = lexer.current();
    expectAndConsume('{', "table type");

    while (lexer.current().type != '}')
    {
        if (lexer.current().type == '[' && (lexer.lookahead().type == Lexeme::RawString || lexer.lookahead().type == Lexeme::QuotedString))
        {
            const Lexeme begin = lexer.current();
            nextLexeme(); // [
            std::optional<AstArray<char>> chars = parseCharArray();

            expectMatchAndConsume(']', begin);
            expectAndConsume(':', "table field");

            AstType* type = parseTypeAnnotation();

            // TODO: since AstName conains a char*, it can't contain null
            bool containsNull = chars && (strnlen(chars->data, chars->size) < chars->size);

            if (chars && !containsNull)
                props.push_back({AstName(chars->data), begin.location, type});
            else
                report(begin.location, "String literal contains malformed escape sequence");
        }
        else if (lexer.current().type == '[')
        {
            if (indexer)
            {
                // maybe we don't need to parse the entire badIndexer...
                // however, we either have { or [ to lint, not the entire table type or the bad indexer.
                AstTableIndexer* badIndexer = parseTableIndexerAnnotation();

                // we lose all additional indexer expressions from the AST after error recovery here
                report(badIndexer->location, "Cannot have more than one table indexer");
            }
            else
            {
                indexer = parseTableIndexerAnnotation();
            }
        }
        else if (props.empty() && !indexer && !(lexer.current().type == Lexeme::Name && lexer.lookahead().type == ':'))
        {
            AstType* type = parseTypeAnnotation();

            // array-like table type: {T} desugars into {[number]: T}
            AstType* index = allocator.alloc<AstTypeReference>(type->location, std::nullopt, nameNumber);
            indexer = allocator.alloc<AstTableIndexer>(AstTableIndexer{index, type, type->location});

            break;
        }
        else
        {
            std::optional<Name> name = parseNameOpt("table field");

            if (!name)
                break;

            expectAndConsume(':', "table field");

            AstType* type = parseTypeAnnotation();

            props.push_back({name->name, name->location, type});
        }

        if (lexer.current().type == ',' || lexer.current().type == ';')
        {
            nextLexeme();
        }
        else
        {
            if (lexer.current().type != '}')
                break;
        }
    }

    Location end = lexer.current().location;

    if (!expectMatchAndConsume('}', matchBrace))
        end = lexer.previousLocation();

    return allocator.alloc<AstTypeTable>(Location(start, end), copy(props), indexer);
}

// ReturnType ::= TypeAnnotation | `(' TypeList `)'
// FunctionTypeAnnotation ::= [`<' varlist `>'] `(' [TypeList] `)' `->` ReturnType
AstTypeOrPack Parser::parseFunctionTypeAnnotation(bool allowPack)
{
    incrementRecursionCounter("type annotation");

    bool monomorphic = lexer.current().type != '<';

    Lexeme begin = lexer.current();

    auto [generics, genericPacks] = parseGenericTypeList(/* withDefaultValues= */ false);

    Lexeme parameterStart = lexer.current();

    expectAndConsume('(', "function parameters");

    matchRecoveryStopOnToken[Lexeme::SkinnyArrow]++;

    TempVector<AstType*> params(scratchAnnotation);
    TempVector<std::optional<AstArgumentName>> names(scratchOptArgName);
    AstTypePack* varargAnnotation = nullptr;

    if (lexer.current().type != ')')
        varargAnnotation = parseTypeList(params, names);

    expectMatchAndConsume(')', parameterStart, true);

    matchRecoveryStopOnToken[Lexeme::SkinnyArrow]--;

    AstArray<AstType*> paramTypes = copy(params);

    // Not a function at all. Just a parenthesized type. Or maybe a type pack with a single element
    if (params.size() == 1 && !varargAnnotation && monomorphic && lexer.current().type != Lexeme::SkinnyArrow)
    {
        if (allowPack)
            return {{}, allocator.alloc<AstTypePackExplicit>(begin.location, AstTypeList{paramTypes, nullptr})};
        else
            return {params[0], {}};
    }

    if (lexer.current().type != Lexeme::SkinnyArrow && monomorphic && allowPack)
        return {{}, allocator.alloc<AstTypePackExplicit>(begin.location, AstTypeList{paramTypes, varargAnnotation})};

    AstArray<std::optional<AstArgumentName>> paramNames = copy(names);

    return {parseFunctionTypeAnnotationTail(begin, generics, genericPacks, paramTypes, paramNames, varargAnnotation), {}};
}

AstType* Parser::parseFunctionTypeAnnotationTail(const Lexeme& begin, AstArray<AstGenericType> generics, AstArray<AstGenericTypePack> genericPacks,
    AstArray<AstType*>& params, AstArray<std::optional<AstArgumentName>>& paramNames, AstTypePack* varargAnnotation)

{
    incrementRecursionCounter("type annotation");

    // Users occasionally write '()' as the 'unit' type when they actually want to use 'nil', here we'll try to give a more specific error
    if (lexer.current().type != Lexeme::SkinnyArrow && generics.size == 0 && genericPacks.size == 0 && params.size == 0)
    {
        report(Location(begin.location, lexer.previousLocation()), "Expected '->' after '()' when parsing function type; did you mean 'nil'?");

        return allocator.alloc<AstTypeReference>(begin.location, std::nullopt, nameNil);
    }
    else
    {
        expectAndConsume(Lexeme::SkinnyArrow, "function type");
    }

    auto [endLocation, returnTypeList] = parseReturnTypeAnnotation();

    AstTypeList paramTypes = AstTypeList{params, varargAnnotation};
    return allocator.alloc<AstTypeFunction>(Location(begin.location, endLocation), generics, genericPacks, paramTypes, paramNames, returnTypeList);
}

// typeannotation ::=
//      nil |
//      Name[`.' Name] [`<' namelist `>'] |
//      `{' [PropList] `}' |
//      `(' [TypeList] `)' `->` ReturnType
//      `typeof` typeannotation
AstType* Parser::parseTypeAnnotation(TempVector<AstType*>& parts, const Location& begin)
{
    LUAU_ASSERT(!parts.empty());

    incrementRecursionCounter("type annotation");

    bool isUnion = false;
    bool isIntersection = false;

    Location location = begin;

    while (true)
    {
        Lexeme::Type c = lexer.current().type;
        if (c == '|')
        {
            nextLexeme();
            parts.push_back(parseSimpleTypeAnnotation(/* allowPack= */ false).type);
            isUnion = true;
        }
        else if (c == '?')
        {
            Location loc = lexer.current().location;
            nextLexeme();
            parts.push_back(allocator.alloc<AstTypeReference>(loc, std::nullopt, nameNil));
            isUnion = true;
        }
        else if (c == '&')
        {
            nextLexeme();
            parts.push_back(parseSimpleTypeAnnotation(/* allowPack= */ false).type);
            isIntersection = true;
        }
        else if (FFlag::LuauParseRecoverUnexpectedPack && c == Lexeme::Dot3)
        {
            report(lexer.current().location, "Unexpected '...' after type annotation");
            nextLexeme();
        }
        else
            break;
    }

    if (parts.size() == 1)
        return parts[0];

    if (isUnion && isIntersection)
    {
        return reportTypeAnnotationError(Location(begin, parts.back()->location), copy(parts), /*isMissing*/ false,
            "Mixing union and intersection types is not allowed; consider wrapping in parentheses.");
    }

    location.end = parts.back()->location.end;

    if (isUnion)
        return allocator.alloc<AstTypeUnion>(location, copy(parts));

    if (isIntersection)
        return allocator.alloc<AstTypeIntersection>(location, copy(parts));

    LUAU_ASSERT(false);
    ParseError::raise(begin, "Composite type was not an intersection or union.");
}

AstTypeOrPack Parser::parseTypeOrPackAnnotation()
{
    unsigned int oldRecursionCount = recursionCounter;
    incrementRecursionCounter("type annotation");

    Location begin = lexer.current().location;

    TempVector<AstType*> parts(scratchAnnotation);

    auto [type, typePack] = parseSimpleTypeAnnotation(/* allowPack= */ true);

    if (typePack)
    {
        LUAU_ASSERT(!type);
        return {{}, typePack};
    }

    parts.push_back(type);

    recursionCounter = oldRecursionCount;

    return {parseTypeAnnotation(parts, begin), {}};
}

AstType* Parser::parseTypeAnnotation()
{
    unsigned int oldRecursionCount = recursionCounter;
    incrementRecursionCounter("type annotation");

    Location begin = lexer.current().location;

    TempVector<AstType*> parts(scratchAnnotation);
    parts.push_back(parseSimpleTypeAnnotation(/* allowPack= */ false).type);

    recursionCounter = oldRecursionCount;

    return parseTypeAnnotation(parts, begin);
}

// typeannotation ::= nil | Name[`.' Name] [ `<' typeannotation [`,' ...] `>' ] | `typeof' `(' expr `)' | `{' [PropList] `}'
//   | [`<' varlist `>'] `(' [TypeList] `)' `->` ReturnType
AstTypeOrPack Parser::parseSimpleTypeAnnotation(bool allowPack)
{
    incrementRecursionCounter("type annotation");

    Location begin = lexer.current().location;

    if (lexer.current().type == Lexeme::ReservedNil)
    {
        nextLexeme();
        return {allocator.alloc<AstTypeReference>(begin, std::nullopt, nameNil), {}};
    }
    else if (lexer.current().type == Lexeme::ReservedTrue)
    {
        nextLexeme();
        return {allocator.alloc<AstTypeSingletonBool>(begin, true)};
    }
    else if (lexer.current().type == Lexeme::ReservedFalse)
    {
        nextLexeme();
        return {allocator.alloc<AstTypeSingletonBool>(begin, false)};
    }
    else if (lexer.current().type == Lexeme::RawString || lexer.current().type == Lexeme::QuotedString)
    {
        if (std::optional<AstArray<char>> value = parseCharArray())
        {
            AstArray<char> svalue = *value;
            return {allocator.alloc<AstTypeSingletonString>(begin, svalue)};
        }
        else
            return {reportTypeAnnotationError(begin, {}, /*isMissing*/ false, "String literal contains malformed escape sequence")};
    }
    else if (lexer.current().type == Lexeme::BrokenString)
    {
        Location location = lexer.current().location;
        nextLexeme();
        return {reportTypeAnnotationError(location, {}, /*isMissing*/ false, "Malformed string")};
    }
    else if (lexer.current().type == Lexeme::Name)
    {
        std::optional<AstName> prefix;
        Name name = parseName("type name");

        if (lexer.current().type == '.')
        {
            Position pointPosition = lexer.current().location.begin;
            nextLexeme();

            prefix = name.name;
            name = parseIndexName("field name", pointPosition);
        }
        else if (FFlag::LuauParseRecoverUnexpectedPack && lexer.current().type == Lexeme::Dot3)
        {
            report(lexer.current().location, "Unexpected '...' after type name; type pack is not allowed in this context");
            nextLexeme();
        }
        else if (name.name == "typeof")
        {
            Lexeme typeofBegin = lexer.current();
            expectAndConsume('(', "typeof type");

            AstExpr* expr = parseExpr();

            Location end = lexer.current().location;

            expectMatchAndConsume(')', typeofBegin);

            return {allocator.alloc<AstTypeTypeof>(Location(begin, end), expr), {}};
        }

        bool hasParameters = false;
        AstArray<AstTypeOrPack> parameters{};

        if (lexer.current().type == '<')
        {
            hasParameters = true;
            parameters = parseTypeParams();
        }

        Location end = lexer.previousLocation();

        return {allocator.alloc<AstTypeReference>(Location(begin, end), prefix, name.name, hasParameters, parameters), {}};
    }
    else if (lexer.current().type == '{')
    {
        return {parseTableTypeAnnotation(), {}};
    }
    else if (lexer.current().type == '(' || lexer.current().type == '<')
    {
        return parseFunctionTypeAnnotation(allowPack);
    }
    else
    {
        Location location = lexer.current().location;

        // For a missing type annotation, capture 'space' between last token and the next one
        location = Location(lexer.previousLocation().end, lexer.current().location.begin);

        return {reportTypeAnnotationError(location, {}, /*isMissing*/ true, "Expected type, got %s", lexer.current().toString().c_str()), {}};
    }
}

AstTypePack* Parser::parseVariadicArgumentAnnotation()
{
    // Generic: a...
    if (lexer.current().type == Lexeme::Name && lexer.lookahead().type == Lexeme::Dot3)
    {
        Name name = parseName("generic name");
        Location end = lexer.current().location;

        // This will not fail because of the lookahead guard.
        expectAndConsume(Lexeme::Dot3, "generic type pack annotation");
        return allocator.alloc<AstTypePackGeneric>(Location(name.location, end), name.name);
    }
    // Variadic: T
    else
    {
        AstType* variadicAnnotation = parseTypeAnnotation();
        return allocator.alloc<AstTypePackVariadic>(variadicAnnotation->location, variadicAnnotation);
    }
}

AstTypePack* Parser::parseTypePackAnnotation()
{
    // Variadic: ...T
    if (lexer.current().type == Lexeme::Dot3)
    {
        Location start = lexer.current().location;
        nextLexeme();
        AstType* varargTy = parseTypeAnnotation();
        return allocator.alloc<AstTypePackVariadic>(Location(start, varargTy->location), varargTy);
    }
    // Generic: a...
    else if (lexer.current().type == Lexeme::Name && lexer.lookahead().type == Lexeme::Dot3)
    {
        Name name = parseName("generic name");
        Location end = lexer.current().location;

        // This will not fail because of the lookahead guard.
        expectAndConsume(Lexeme::Dot3, "generic type pack annotation");
        return allocator.alloc<AstTypePackGeneric>(Location(name.location, end), name.name);
    }

    // No type pack annotation exists here.
    return nullptr;
}

std::optional<AstExprUnary::Op> Parser::parseUnaryOp(const Lexeme& l)
{
    if (l.type == Lexeme::ReservedNot)
        return AstExprUnary::Not;
    else if (l.type == '-')
        return AstExprUnary::Minus;
    else if (l.type == '#')
        return AstExprUnary::Len;
    else
        return std::nullopt;
}

std::optional<AstExprBinary::Op> Parser::parseBinaryOp(const Lexeme& l)
{
    if (l.type == '+')
        return AstExprBinary::Add;
    else if (l.type == '-')
        return AstExprBinary::Sub;
    else if (l.type == '*')
        return AstExprBinary::Mul;
    else if (l.type == '/')
        return AstExprBinary::Div;
    else if (l.type == '%')
        return AstExprBinary::Mod;
    else if (l.type == '^')
        return AstExprBinary::Pow;
    else if (l.type == Lexeme::Dot2)
        return AstExprBinary::Concat;
    else if (l.type == Lexeme::NotEqual)
        return AstExprBinary::CompareNe;
    else if (l.type == Lexeme::Equal)
        return AstExprBinary::CompareEq;
    else if (l.type == '<')
        return AstExprBinary::CompareLt;
    else if (l.type == Lexeme::LessEqual)
        return AstExprBinary::CompareLe;
    else if (l.type == '>')
        return AstExprBinary::CompareGt;
    else if (l.type == Lexeme::GreaterEqual)
        return AstExprBinary::CompareGe;
    else if (l.type == Lexeme::ReservedAnd)
        return AstExprBinary::And;
    else if (l.type == Lexeme::ReservedOr)
        return AstExprBinary::Or;
    else
        return std::nullopt;
}

std::optional<AstExprBinary::Op> Parser::parseCompoundOp(const Lexeme& l)
{
    if (l.type == Lexeme::AddAssign)
        return AstExprBinary::Add;
    else if (l.type == Lexeme::SubAssign)
        return AstExprBinary::Sub;
    else if (l.type == Lexeme::MulAssign)
        return AstExprBinary::Mul;
    else if (l.type == Lexeme::DivAssign)
        return AstExprBinary::Div;
    else if (l.type == Lexeme::ModAssign)
        return AstExprBinary::Mod;
    else if (l.type == Lexeme::PowAssign)
        return AstExprBinary::Pow;
    else if (l.type == Lexeme::ConcatAssign)
        return AstExprBinary::Concat;
    else
        return std::nullopt;
}

std::optional<AstExprUnary::Op> Parser::checkUnaryConfusables()
{
    const Lexeme& curr = lexer.current();

    // early-out: need to check if this is a possible confusable quickly
    if (curr.type != '!')
        return {};

    // slow path: possible confusable
    Location start = curr.location;

    if (curr.type == '!')
    {
        report(start, "Unexpected '!', did you mean 'not'?");
        return AstExprUnary::Not;
    }

    return {};
}

std::optional<AstExprBinary::Op> Parser::checkBinaryConfusables(const BinaryOpPriority binaryPriority[], unsigned int limit)
{
    const Lexeme& curr = lexer.current();

    // early-out: need to check if this is a possible confusable quickly
    if (curr.type != '&' && curr.type != '|' && curr.type != '!')
        return {};

    // slow path: possible confusable
    Location start = curr.location;
    Lexeme next = lexer.lookahead();

    if (curr.type == '&' && next.type == '&' && curr.location.end == next.location.begin && binaryPriority[AstExprBinary::And].left > limit)
    {
        nextLexeme();
        report(Location(start, next.location), "Unexpected '&&', did you mean 'and'?");
        return AstExprBinary::And;
    }
    else if (curr.type == '|' && next.type == '|' && curr.location.end == next.location.begin && binaryPriority[AstExprBinary::Or].left > limit)
    {
        nextLexeme();
        report(Location(start, next.location), "Unexpected '||', did you mean 'or'?");
        return AstExprBinary::Or;
    }
    else if (curr.type == '!' && next.type == '=' && curr.location.end == next.location.begin &&
             binaryPriority[AstExprBinary::CompareNe].left > limit)
    {
        nextLexeme();
        report(Location(start, next.location), "Unexpected '!=', did you mean '~='?");
        return AstExprBinary::CompareNe;
    }

    return std::nullopt;
}

// subexpr -> (asexp | unop subexpr) { binop subexpr }
// where `binop' is any binary operator with a priority higher than `limit'
AstExpr* Parser::parseExpr(unsigned int limit)
{
    static const BinaryOpPriority binaryPriority[] = {
        {6, 6}, {6, 6}, {7, 7}, {7, 7}, {7, 7}, // `+' `-' `*' `/' `%'
        {10, 9}, {5, 4},                        // power and concat (right associative)
        {3, 3}, {3, 3},                         // equality and inequality
        {3, 3}, {3, 3}, {3, 3}, {3, 3},         // order
        {2, 2}, {1, 1}                          // logical (and/or)
    };

    unsigned int recursionCounterOld = recursionCounter;

    // this handles recursive calls to parseSubExpr/parseExpr
    incrementRecursionCounter("expression");

    const unsigned int unaryPriority = 8;

    Location start = lexer.current().location;

    AstExpr* expr;

    std::optional<AstExprUnary::Op> uop = parseUnaryOp(lexer.current());

    if (!uop)
        uop = checkUnaryConfusables();

    if (uop)
    {
        nextLexeme();

        AstExpr* subexpr = parseExpr(unaryPriority);

        expr = allocator.alloc<AstExprUnary>(Location(start, subexpr->location), *uop, subexpr);
    }
    else
    {
        expr = parseAssertionExpr();
    }

    // expand while operators have priorities higher than `limit'
    std::optional<AstExprBinary::Op> op = parseBinaryOp(lexer.current());

    if (!op)
        op = checkBinaryConfusables(binaryPriority, limit);

    while (op && binaryPriority[*op].left > limit)
    {
        nextLexeme();

        // read sub-expression with higher priority
        AstExpr* next = parseExpr(binaryPriority[*op].right);

        expr = allocator.alloc<AstExprBinary>(Location(start, next->location), *op, expr, next);
        op = parseBinaryOp(lexer.current());

        if (!op)
            op = checkBinaryConfusables(binaryPriority, limit);

        // note: while the parser isn't recursive here, we're generating recursive structures of unbounded depth
        incrementRecursionCounter("expression");
    }

    recursionCounter = recursionCounterOld;

    return expr;
}

// NAME
AstExpr* Parser::parseNameExpr(const char* context)
{
    std::optional<Name> name = parseNameOpt(context);

    if (!name)
        return allocator.alloc<AstExprError>(lexer.current().location, copy<AstExpr*>({}), unsigned(parseErrors.size() - 1));

    AstLocal* const* value = localMap.find(name->name);

    if (value && *value)
    {
        AstLocal* local = *value;

        return allocator.alloc<AstExprLocal>(name->location, local, local->functionDepth != functionStack.size() - 1);
    }

    return allocator.alloc<AstExprGlobal>(name->location, name->name);
}

// prefixexp -> NAME | '(' expr ')'
AstExpr* Parser::parsePrefixExpr()
{
    if (lexer.current().type == '(')
    {
        Location start = lexer.current().location;

        Lexeme matchParen = lexer.current();
        nextLexeme();

        AstExpr* expr = parseExpr();

        Location end = lexer.current().location;

        if (lexer.current().type != ')')
        {
            const char* suggestion = (lexer.current().type == '=') ? "; did you mean to use '{' when defining a table?" : nullptr;

            expectMatchAndConsumeFail(static_cast<Lexeme::Type>(')'), matchParen, suggestion);

            end = lexer.previousLocation();
        }
        else
        {
            nextLexeme();
        }

        return allocator.alloc<AstExprGroup>(Location(start, end), expr);
    }
    else
    {
        return parseNameExpr("expression");
    }
}

// primaryexp -> prefixexp { `.' NAME | `[' exp `]' | `:' NAME funcargs | funcargs }
AstExpr* Parser::parsePrimaryExpr(bool asStatement)
{
    Location start = lexer.current().location;

    AstExpr* expr = parsePrefixExpr();

    unsigned int recursionCounterOld = recursionCounter;

    while (true)
    {
        if (lexer.current().type == '.')
        {
            Position opPosition = lexer.current().location.begin;
            nextLexeme();

            Name index = parseIndexName(nullptr, opPosition);

            expr = allocator.alloc<AstExprIndexName>(Location(start, index.location), expr, index.name, index.location, opPosition, '.');
        }
        else if (lexer.current().type == '[')
        {
            Lexeme matchBracket = lexer.current();
            nextLexeme();

            AstExpr* index = parseExpr();

            Location end = lexer.current().location;

            expectMatchAndConsume(']', matchBracket);

            expr = allocator.alloc<AstExprIndexExpr>(Location(start, end), expr, index);
        }
        else if (lexer.current().type == ':')
        {
            Position opPosition = lexer.current().location.begin;
            nextLexeme();

            Name index = parseIndexName("method name", opPosition);
            AstExpr* func = allocator.alloc<AstExprIndexName>(Location(start, index.location), expr, index.name, index.location, opPosition, ':');

            expr = parseFunctionArgs(func, true, index.location);
        }
        else if (lexer.current().type == '(')
        {
            // This error is handled inside 'parseFunctionArgs' as well, but for better error recovery we need to break out the current loop here
            if (!asStatement && expr->location.end.line != lexer.current().location.begin.line)
            {
                report(lexer.current().location,
                    "Ambiguous syntax: this looks like an argument list for a function call, but could also be a start of "
                    "new statement; use ';' to separate statements");

                break;
            }

            expr = parseFunctionArgs(expr, false, Location());
        }
        else if (lexer.current().type == '{' || lexer.current().type == Lexeme::RawString || lexer.current().type == Lexeme::QuotedString)
        {
            expr = parseFunctionArgs(expr, false, Location());
        }
        else
        {
            break;
        }

        // note: while the parser isn't recursive here, we're generating recursive structures of unbounded depth
        incrementRecursionCounter("expression");
    }

    recursionCounter = recursionCounterOld;

    return expr;
}

// asexp -> simpleexp [`::' typeannotation]
AstExpr* Parser::parseAssertionExpr()
{
    Location start = lexer.current().location;
    AstExpr* expr = parseSimpleExpr();

    if (options.allowTypeAnnotations && lexer.current().type == Lexeme::DoubleColon)
    {
        nextLexeme();
        AstType* annotation = parseTypeAnnotation();
        return allocator.alloc<AstExprTypeAssertion>(Location(start, annotation->location), expr, annotation);
    }
    else
        return expr;
}

static bool parseNumber(double& result, const char* data)
{
    // binary literal
    if (data[0] == '0' && (data[1] == 'b' || data[1] == 'B') && data[2])
    {
        char* end = nullptr;
        unsigned long long value = strtoull(data + 2, &end, 2);

        result = double(value);
        return *end == 0;
    }
    // hexadecimal literal
    else if (data[0] == '0' && (data[1] == 'x' || data[1] == 'X') && data[2])
    {
        char* end = nullptr;
        unsigned long long value = strtoull(data + 2, &end, 16);

        result = double(value);
        return *end == 0;
    }
    else
    {
        char* end = nullptr;
        double value = strtod(data, &end);

        result = value;
        return *end == 0;
    }
}

// simpleexp -> NUMBER | STRING | NIL | true | false | ... | constructor | FUNCTION body | primaryexp
AstExpr* Parser::parseSimpleExpr()
{
    Location start = lexer.current().location;

    if (lexer.current().type == Lexeme::ReservedNil)
    {
        nextLexeme();

        return allocator.alloc<AstExprConstantNil>(start);
    }
    else if (lexer.current().type == Lexeme::ReservedTrue)
    {
        nextLexeme();

        return allocator.alloc<AstExprConstantBool>(start, true);
    }
    else if (lexer.current().type == Lexeme::ReservedFalse)
    {
        nextLexeme();

        return allocator.alloc<AstExprConstantBool>(start, false);
    }
    else if (lexer.current().type == Lexeme::ReservedFunction)
    {
        Lexeme matchFunction = lexer.current();
        nextLexeme();

        return parseFunctionBody(false, matchFunction, AstName(), {}).first;
    }
    else if (lexer.current().type == Lexeme::Number)
    {
        scratchData.assign(lexer.current().data, lexer.current().length);

        // Remove all internal _ - they don't hold any meaning and this allows parsing code to just pass the string pointer to strtod et al
        if (scratchData.find('_') != std::string::npos)
        {
            scratchData.erase(std::remove(scratchData.begin(), scratchData.end(), '_'), scratchData.end());
        }

        double value = 0;
        if (parseNumber(value, scratchData.c_str()))
        {
            nextLexeme();

            return allocator.alloc<AstExprConstantNumber>(start, value);
        }
        else
        {
            nextLexeme();

            return reportExprError(start, {}, "Malformed number");
        }
    }
    else if (lexer.current().type == Lexeme::RawString || lexer.current().type == Lexeme::QuotedString)
    {
        return parseString();
    }
    else if (lexer.current().type == Lexeme::BrokenString)
    {
        nextLexeme();
        return reportExprError(start, {}, "Malformed string");
    }
    else if (lexer.current().type == Lexeme::Dot3)
    {
        if (functionStack.back().vararg)
        {
            nextLexeme();

            return allocator.alloc<AstExprVarargs>(start);
        }
        else
        {
            nextLexeme();

            return reportExprError(start, {}, "Cannot use '...' outside of a vararg function");
        }
    }
    else if (lexer.current().type == '{')
    {
        return parseTableConstructor();
    }
    else if (lexer.current().type == Lexeme::ReservedIf)
    {
        return parseIfElseExpr();
    }
    else
    {
        return parsePrimaryExpr(/* asStatement= */ false);
    }
}

// args ::=  `(' [explist] `)' | tableconstructor | String
AstExpr* Parser::parseFunctionArgs(AstExpr* func, bool self, const Location& selfLocation)
{
    if (lexer.current().type == '(')
    {
        Position argStart = lexer.current().location.end;
        if (func->location.end.line != lexer.current().location.begin.line)
        {
            report(lexer.current().location, "Ambiguous syntax: this looks like an argument list for a function call, but could also be a start of "
                                             "new statement; use ';' to separate statements");
        }

        Lexeme matchParen = lexer.current();
        nextLexeme();

        TempVector<AstExpr*> args(scratchExpr);

        if (lexer.current().type != ')')
            parseExprList(args);

        Location end = lexer.current().location;
        Position argEnd = end.end;

        expectMatchAndConsume(')', matchParen);

        return allocator.alloc<AstExprCall>(Location(func->location, end), func, copy(args), self, Location(argStart, argEnd));
    }
    else if (lexer.current().type == '{')
    {
        Position argStart = lexer.current().location.end;
        AstExpr* expr = parseTableConstructor();
        Position argEnd = lexer.previousLocation().end;

        return allocator.alloc<AstExprCall>(Location(func->location, expr->location), func, copy(&expr, 1), self, Location(argStart, argEnd));
    }
    else if (lexer.current().type == Lexeme::RawString || lexer.current().type == Lexeme::QuotedString)
    {
        Location argLocation = lexer.current().location;
        AstExpr* expr = parseString();

        return allocator.alloc<AstExprCall>(Location(func->location, expr->location), func, copy(&expr, 1), self, argLocation);
    }
    else
    {
        if (self && lexer.current().location.begin.line != func->location.end.line)
        {
            return reportExprError(func->location, copy({func}), "Expected function call arguments after '('");
        }
        else
        {
            return reportExprError(Location(func->location.begin, lexer.current().location.begin), copy({func}),
                "Expected '(', '{' or <string> when parsing function call, got %s", lexer.current().toString().c_str());
        }
    }
}

// tableconstructor ::= `{' [fieldlist] `}'
// fieldlist ::= field {fieldsep field} [fieldsep]
// field ::= `[' exp `]' `=' exp | Name `=' exp | exp
// fieldsep ::= `,' | `;'
AstExpr* Parser::parseTableConstructor()
{
    TempVector<AstExprTable::Item> items(scratchItem);

    Location start = lexer.current().location;

    Lexeme matchBrace = lexer.current();
    expectAndConsume('{', "table literal");

    while (lexer.current().type != '}')
    {
        if (lexer.current().type == '[')
        {
            Lexeme matchLocationBracket = lexer.current();
            nextLexeme();

            AstExpr* key = parseExpr();

            expectMatchAndConsume(']', matchLocationBracket);

            expectAndConsume('=', "table field");

            AstExpr* value = parseExpr();

            items.push_back({AstExprTable::Item::General, key, value});
        }
        else if (lexer.current().type == Lexeme::Name && lexer.lookahead().type == '=')
        {
            Name name = parseName("table field");

            expectAndConsume('=', "table field");

            AstArray<char> nameString;
            nameString.data = const_cast<char*>(name.name.value);
            nameString.size = strlen(name.name.value);

            AstExpr* key = allocator.alloc<AstExprConstantString>(name.location, nameString);
            AstExpr* value = parseExpr();

            if (AstExprFunction* func = value->as<AstExprFunction>())
                func->debugname = name.name;

            items.push_back({AstExprTable::Item::Record, key, value});
        }
        else
        {
            AstExpr* expr = parseExpr();

            items.push_back({AstExprTable::Item::List, nullptr, expr});
        }

        if (lexer.current().type == ',' || lexer.current().type == ';')
        {
            nextLexeme();
        }
        else
        {
            if (lexer.current().type != '}')
                break;
        }
    }

    Location end = lexer.current().location;

    if (!expectMatchAndConsume('}', matchBrace))
        end = lexer.previousLocation();

    return allocator.alloc<AstExprTable>(Location(start, end), copy(items));
}

AstExpr* Parser::parseIfElseExpr()
{
    bool hasElse = false;
    Location start = lexer.current().location;

    nextLexeme(); // skip if / elseif

    AstExpr* condition = parseExpr();

    bool hasThen = expectAndConsume(Lexeme::ReservedThen, "if then else expression");

    AstExpr* trueExpr = parseExpr();
    AstExpr* falseExpr = nullptr;

    if (lexer.current().type == Lexeme::ReservedElseif)
    {
        unsigned int oldRecursionCount = recursionCounter;
        incrementRecursionCounter("expression");
        hasElse = true;
        falseExpr = parseIfElseExpr();
        recursionCounter = oldRecursionCount;
    }
    else
    {
        hasElse = expectAndConsume(Lexeme::ReservedElse, "if then else expression");
        falseExpr = parseExpr();
    }

    Location end = falseExpr->location;

    return allocator.alloc<AstExprIfElse>(Location(start, end), condition, hasThen, trueExpr, hasElse, falseExpr);
}

// Name
std::optional<Parser::Name> Parser::parseNameOpt(const char* context)
{
    if (lexer.current().type != Lexeme::Name)
    {
        reportNameError(context);

        return {};
    }

    Name result(AstName(lexer.current().name), lexer.current().location);

    nextLexeme();

    return result;
}

Parser::Name Parser::parseName(const char* context)
{
    if (std::optional<Name> name = parseNameOpt(context))
        return *name;

    Location location = lexer.current().location;
    location.end = location.begin;

    return Name(nameError, location);
}

Parser::Name Parser::parseIndexName(const char* context, const Position& previous)
{
    if (std::optional<Name> name = parseNameOpt(context))
        return *name;

    // If we have a reserved keyword next at the same line, assume it's an incomplete name
    if (lexer.current().type >= Lexeme::Reserved_BEGIN && lexer.current().type < Lexeme::Reserved_END &&
        lexer.current().location.begin.line == previous.line)
    {
        Name result(AstName(lexer.current().name), lexer.current().location);

        nextLexeme();

        return result;
    }

    Location location = lexer.current().location;
    location.end = location.begin;

    return Name(nameError, location);
}

std::pair<AstArray<AstGenericType>, AstArray<AstGenericTypePack>> Parser::parseGenericTypeList(bool withDefaultValues)
{
    TempVector<AstGenericType> names{scratchGenericTypes};
    TempVector<AstGenericTypePack> namePacks{scratchGenericTypePacks};

    if (lexer.current().type == '<')
    {
        Lexeme begin = lexer.current();
        nextLexeme();

        bool seenPack = false;
        bool seenDefault = false;

        while (true)
        {
            Location nameLocation = lexer.current().location;
            AstName name = parseName().name;
            if (lexer.current().type == Lexeme::Dot3 || seenPack)
            {
                seenPack = true;

                if (lexer.current().type != Lexeme::Dot3)
                    report(lexer.current().location, "Generic types come before generic type packs");
                else
                    nextLexeme();

                if (withDefaultValues && lexer.current().type == '=')
                {
                    seenDefault = true;
                    nextLexeme();

                    Lexeme packBegin = lexer.current();

                    if (shouldParseTypePackAnnotation(lexer))
                    {
                        AstTypePack* typePack = parseTypePackAnnotation();

                        namePacks.push_back({name, nameLocation, typePack});
                    }
                    else if (lexer.current().type == '(')
                    {
                        auto [type, typePack] = parseTypeOrPackAnnotation();

                        if (type)
                            report(Location(packBegin.location.begin, lexer.previousLocation().end), "Expected type pack after '=', got type");

                        namePacks.push_back({name, nameLocation, typePack});
                    }
                }
                else
                {
                    if (seenDefault)
                        report(lexer.current().location, "Expected default type pack after type pack name");

                    namePacks.push_back({name, nameLocation, nullptr});
                }
            }
            else
            {
                if (withDefaultValues && lexer.current().type == '=')
                {
                    seenDefault = true;
                    nextLexeme();

                    AstType* defaultType = parseTypeAnnotation();

                    names.push_back({name, nameLocation, defaultType});
                }
                else
                {
                    if (seenDefault)
                        report(lexer.current().location, "Expected default type after type name");

                    names.push_back({name, nameLocation, nullptr});
                }
            }

            if (lexer.current().type == ',')
                nextLexeme();
            else
                break;
        }

        expectMatchAndConsume('>', begin);
    }

    AstArray<AstGenericType> generics = copy(names);
    AstArray<AstGenericTypePack> genericPacks = copy(namePacks);
    return {generics, genericPacks};
}

AstArray<AstTypeOrPack> Parser::parseTypeParams()
{
    TempVector<AstTypeOrPack> parameters{scratchTypeOrPackAnnotation};

    if (lexer.current().type == '<')
    {
        Lexeme begin = lexer.current();
        nextLexeme();

        while (true)
        {
            if (shouldParseTypePackAnnotation(lexer))
            {
                AstTypePack* typePack = parseTypePackAnnotation();

                parameters.push_back({{}, typePack});
            }
            else if (lexer.current().type == '(')
            {
                auto [type, typePack] = parseTypeOrPackAnnotation();

                if (typePack)
                    parameters.push_back({{}, typePack});
                else
                    parameters.push_back({type, {}});
            }
            else if (lexer.current().type == '>' && parameters.empty())
            {
                break;
            }
            else
            {
                parameters.push_back({parseTypeAnnotation(), {}});
            }

            if (lexer.current().type == ',')
                nextLexeme();
            else
                break;
        }

        expectMatchAndConsume('>', begin);
    }

    return copy(parameters);
}

std::optional<AstArray<char>> Parser::parseCharArray()
{
    LUAU_ASSERT(lexer.current().type == Lexeme::QuotedString || lexer.current().type == Lexeme::RawString);

    scratchData.assign(lexer.current().data, lexer.current().length);

    if (lexer.current().type == Lexeme::QuotedString)
    {
        if (!Lexer::fixupQuotedString(scratchData))
        {
            nextLexeme();
            return std::nullopt;
        }
    }
    else
    {
        Lexer::fixupMultilineString(scratchData);
    }

    AstArray<char> value = copy(scratchData);
    nextLexeme();
    return value;
}

AstExpr* Parser::parseString()
{
    Location location = lexer.current().location;
    if (std::optional<AstArray<char>> value = parseCharArray())
        return allocator.alloc<AstExprConstantString>(location, *value);
    else
        return reportExprError(location, {}, "String literal contains malformed escape sequence");
}

AstLocal* Parser::pushLocal(const Binding& binding)
{
    const Name& name = binding.name;
    AstLocal*& local = localMap[name.name];

    local = allocator.alloc<AstLocal>(
        name.name, name.location, /* shadow= */ local, functionStack.size() - 1, functionStack.back().loopDepth, binding.annotation);

    localStack.push_back(local);

    return local;
}

unsigned int Parser::saveLocals()
{
    return unsigned(localStack.size());
}

void Parser::restoreLocals(unsigned int offset)
{
    for (size_t i = localStack.size(); i > offset; --i)
    {
        AstLocal* l = localStack[i - 1];

        localMap[l->name] = l->shadow;
    }

    localStack.resize(offset);
}

bool Parser::expectAndConsume(char value, const char* context)
{
    return expectAndConsume(static_cast<Lexeme::Type>(static_cast<unsigned char>(value)), context);
}

bool Parser::expectAndConsume(Lexeme::Type type, const char* context)
{
    if (lexer.current().type != type)
    {
        expectAndConsumeFail(type, context);

        // check if this is an extra token and the expected token is next
        if (lexer.lookahead().type == type)
        {
            // skip invalid and consume expected
            nextLexeme();
            nextLexeme();
        }

        return false;
    }
    else
    {
        nextLexeme();
        return true;
    }
}

// LUAU_NOINLINE is used to limit the stack cost of this function due to std::string objects, and to increase caller performance since this code is
// cold
LUAU_NOINLINE void Parser::expectAndConsumeFail(Lexeme::Type type, const char* context)
{
    std::string typeString = Lexeme(Location(Position(0, 0), 0), type).toString();
    std::string currLexemeString = lexer.current().toString();

    if (context)
        report(lexer.current().location, "Expected %s when parsing %s, got %s", typeString.c_str(), context, currLexemeString.c_str());
    else
        report(lexer.current().location, "Expected %s, got %s", typeString.c_str(), currLexemeString.c_str());
}

bool Parser::expectMatchAndConsume(char value, const Lexeme& begin, bool searchForMissing)
{
    Lexeme::Type type = static_cast<Lexeme::Type>(static_cast<unsigned char>(value));

    if (lexer.current().type != type)
    {
        expectMatchAndConsumeFail(type, begin);

        if (searchForMissing)
        {
            // previous location is taken because 'current' lexeme is already the next token
            unsigned currentLine = lexer.previousLocation().end.line;

            // search to the end of the line for expected token
            // we will also stop if we hit a token that can be handled by parsing function above the current one
            Lexeme::Type lexemeType = lexer.current().type;

            while (currentLine == lexer.current().location.begin.line && lexemeType != type && matchRecoveryStopOnToken[lexemeType] == 0)
            {
                nextLexeme();
                lexemeType = lexer.current().type;
            }

            if (lexemeType == type)
            {
                nextLexeme();

                return true;
            }
        }
        else
        {
            // check if this is an extra token and the expected token is next
            if (lexer.lookahead().type == type)
            {
                // skip invalid and consume expected
                nextLexeme();
                nextLexeme();

                return true;
            }
        }

        return false;
    }
    else
    {
        nextLexeme();

        return true;
    }
}

// LUAU_NOINLINE is used to limit the stack cost of this function due to std::string objects, and to increase caller performance since this code is
// cold
LUAU_NOINLINE void Parser::expectMatchAndConsumeFail(Lexeme::Type type, const Lexeme& begin, const char* extra)
{
    std::string typeString = Lexeme(Location(Position(0, 0), 0), type).toString();

    if (lexer.current().location.begin.line == begin.location.begin.line)
        report(lexer.current().location, "Expected %s (to close %s at column %d), got %s%s", typeString.c_str(), begin.toString().c_str(),
            begin.location.begin.column + 1, lexer.current().toString().c_str(), extra ? extra : "");
    else
        report(lexer.current().location, "Expected %s (to close %s at line %d), got %s%s", typeString.c_str(), begin.toString().c_str(),
            begin.location.begin.line + 1, lexer.current().toString().c_str(), extra ? extra : "");
}

bool Parser::expectMatchEndAndConsume(Lexeme::Type type, const Lexeme& begin)
{
    if (lexer.current().type != type)
    {
        expectMatchEndAndConsumeFail(type, begin);

        // check if this is an extra token and the expected token is next
        if (lexer.lookahead().type == type)
        {
            // skip invalid and consume expected
            nextLexeme();
            nextLexeme();

            return true;
        }

        return false;
    }
    else
    {
        // If the token matches on a different line and a different column, it suggests misleading indentation
        // This can be used to pinpoint the problem location for a possible future *actual* mismatch
        if (lexer.current().location.begin.line != begin.location.begin.line &&
            lexer.current().location.begin.column != begin.location.begin.column &&
            endMismatchSuspect.location.begin.line < begin.location.begin.line) // Only replace the previous suspect with more recent suspects
        {
            endMismatchSuspect = begin;
        }

        nextLexeme();

        return true;
    }
}

// LUAU_NOINLINE is used to limit the stack cost of this function due to std::string objects, and to increase caller performance since this code is
// cold
LUAU_NOINLINE void Parser::expectMatchEndAndConsumeFail(Lexeme::Type type, const Lexeme& begin)
{
    if (endMismatchSuspect.type != Lexeme::Eof && endMismatchSuspect.location.begin.line > begin.location.begin.line)
    {
        std::string suggestion =
            format("; did you forget to close %s at line %d?", endMismatchSuspect.toString().c_str(), endMismatchSuspect.location.begin.line + 1);

        expectMatchAndConsumeFail(type, begin, suggestion.c_str());
    }
    else
    {
        expectMatchAndConsumeFail(type, begin);
    }
}

template<typename T>
AstArray<T> Parser::copy(const T* data, size_t size)
{
    AstArray<T> result;

    result.data = size ? static_cast<T*>(allocator.allocate(sizeof(T) * size)) : nullptr;
    result.size = size;

    // This is equivalent to std::uninitialized_copy, but without the exception guarantee
    // since our types don't have destructors
    for (size_t i = 0; i < size; ++i)
        new (result.data + i) T(data[i]);

    return result;
}

template<typename T>
AstArray<T> Parser::copy(const TempVector<T>& data)
{
    return copy(data.empty() ? nullptr : &data[0], data.size());
}

template<typename T>
AstArray<T> Parser::copy(std::initializer_list<T> data)
{
    return copy(data.size() == 0 ? nullptr : data.begin(), data.size());
}

AstArray<char> Parser::copy(const std::string& data)
{
    AstArray<char> result = copy(data.c_str(), data.size() + 1);

    result.size = data.size();

    return result;
}

void Parser::incrementRecursionCounter(const char* context)
{
    recursionCounter++;

    if (recursionCounter > unsigned(FInt::LuauRecursionLimit))
    {
        ParseError::raise(lexer.current().location, "Exceeded allowed recursion depth; simplify your %s to make the code compile", context);
    }
}

void Parser::report(const Location& location, const char* format, va_list args)
{
    // To reduce number of errors reported to user for incomplete statements, we skip multiple errors at the same location
    // For example, consider 'local a = (((b + ' where multiple tokens haven't been written yet
    if (!parseErrors.empty() && location == parseErrors.back().getLocation())
        return;

    std::string message = vformat(format, args);

    // when limited to a single error, behave as if the error recovery is disabled
    if (FInt::LuauParseErrorLimit == 1)
        throw ParseError(location, message);

    parseErrors.emplace_back(location, message);

    if (parseErrors.size() >= unsigned(FInt::LuauParseErrorLimit))
        ParseError::raise(location, "Reached error limit (%d)", int(FInt::LuauParseErrorLimit));
}

void Parser::report(const Location& location, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    report(location, format, args);
    va_end(args);
}

LUAU_NOINLINE void Parser::reportNameError(const char* context)
{
    if (context)
        report(lexer.current().location, "Expected identifier when parsing %s, got %s", context, lexer.current().toString().c_str());
    else
        report(lexer.current().location, "Expected identifier, got %s", lexer.current().toString().c_str());
}

AstStatError* Parser::reportStatError(
    const Location& location, const AstArray<AstExpr*>& expressions, const AstArray<AstStat*>& statements, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    report(location, format, args);
    va_end(args);

    return allocator.alloc<AstStatError>(location, expressions, statements, unsigned(parseErrors.size() - 1));
}

AstExprError* Parser::reportExprError(const Location& location, const AstArray<AstExpr*>& expressions, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    report(location, format, args);
    va_end(args);

    return allocator.alloc<AstExprError>(location, expressions, unsigned(parseErrors.size() - 1));
}

AstTypeError* Parser::reportTypeAnnotationError(const Location& location, const AstArray<AstType*>& types, bool isMissing, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    report(location, format, args);
    va_end(args);

    return allocator.alloc<AstTypeError>(location, types, isMissing, unsigned(parseErrors.size() - 1));
}

void Parser::nextLexeme()
{
    if (options.captureComments)
    {
        Lexeme::Type type = lexer.next(/* skipComments= */ false, true).type;

        while (type == Lexeme::BrokenComment || type == Lexeme::Comment || type == Lexeme::BlockComment)
        {
            const Lexeme& lexeme = lexer.current();
            commentLocations.push_back(Comment{lexeme.type, lexeme.location});

            // Subtlety: Broken comments are weird because we record them as comments AND pass them to the parser as a lexeme.
            // The parser will turn this into a proper syntax error.
            if (lexeme.type == Lexeme::BrokenComment)
                return;

            // Comments starting with ! are called "hot comments" and contain directives for type checking / linting
            if (lexeme.type == Lexeme::Comment && lexeme.length && lexeme.data[0] == '!')
            {
                const char* text = lexeme.data;

                unsigned int end = lexeme.length;
                while (end > 0 && isSpace(text[end - 1]))
                    --end;

                hotcomments.push_back({hotcommentHeader, lexeme.location, std::string(text + 1, text + end)});
            }

            type = lexer.next(/* skipComments= */ false, !FFlag::LuauParseLocationIgnoreCommentSkipInCapture).type;
        }
    }
    else
        lexer.next();
}

} // namespace Luau
