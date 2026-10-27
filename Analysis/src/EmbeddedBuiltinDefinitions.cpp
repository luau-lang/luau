// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"
#include <initializer_list>
#include <sstream>
#include <string>
#include <string_view>

LUAU_FASTFLAG(LuauAllowGlobalDeclarationToBeCalledClass)
LUAU_FASTFLAG(DebugLuauBufferVectorLibrary)
LUAU_FASTFLAG(DebugLuauUserDefinedClasses)
LUAU_FASTFLAGVARIABLE(LuauCoroutineFinallyAnalysis)
LUAU_FASTFLAGVARIABLE(LuauEZAddBuiltins)
LUAU_FASTFLAG(DebugLuauBufferCompare)

namespace Luau
{

struct NameTypePair
{
    std::string_view name;
    std::string_view type;
};

struct TypeDefBuilder;
struct TableLikeDefinition
{
    TableLikeDefinition(const TableLikeDefinition&) = delete;
    TableLikeDefinition& operator=(const TableLikeDefinition&) = delete;
    TableLikeDefinition(TableLikeDefinition&&) = delete;
    TableLikeDefinition& operator=(TableLikeDefinition&&) = delete;

    ~TableLikeDefinition();

    // Copies the property text, so the input views need only remain valid for this call.
    void registerProp(std::string_view name, std::string_view type)
    {
        source << "    " << name << ": " << type << ",\n";
    }

    TableLikeDefinition(TypeDefBuilder* builder, std::string_view name, bool isType, bool isExported)
        : builder(builder)
    {
        if (isType)
        {
            if (isExported)
                source << "export type " << name << " = {\n";
            else
                source << "type " << name << " = {\n";
        }
        else
        {
            source << "declare " << name << ": {\n";
        }
    }

    TypeDefBuilder* builder;
    std::stringstream source;
};

struct TypeDefBuilder
{
    void declareCheckedFunction(std::string_view name, std::initializer_list<NameTypePair> params, std::string_view returnType)
    {
        source << "@checked ";
        declareFunction(name, {}, params, returnType);
    }

    void declareFunction(
        std::string_view name,
        std::initializer_list<std::string_view> genericParams,
        std::initializer_list<NameTypePair> params,
        std::string_view returnType
    )
    {
        source << "declare function " << name;
        if (genericParams.size() != 0)
        {
            source << '<';
            const char* separator = "";
            for (std::string_view genericParam : genericParams)
            {
                source << separator << genericParam;
                separator = ", ";
            }
            source << '>';
        }

        source << '(';
        const char* separator = "";
        for (const NameTypePair& param : params)
        {
            source << separator << param.name << ": " << param.type;
            separator = ", ";
        }
        source << ')';
        if (!returnType.empty())
        {
            source << ": " << returnType;
        }
        source << '\n';
    }

    void declareGlobal(std::string_view name, std::string_view type)
    {
        source << "declare " << name << ": " << type << '\n';
    }

    void declareMagic(std::string_view magic)
    {
        source << magic << '\n';
    }

    [[nodiscard]] TableLikeDefinition declareTablePartial(std::string_view name) &
    {
        return TableLikeDefinition{this, name, false, false};
    }
    [[nodiscard]] TableLikeDefinition declareTypePartial(std::string_view name, bool isExported) &
    {
        return TableLikeDefinition{this, name, true, isExported};
    }

    std::string str() const
    {
        return source.str();
    }

    void writeTable(std::initializer_list<NameTypePair> props)
    {
        source << "{\n";
        for (const NameTypePair& prop : props)
        {
            source << "    " << prop.name << ": " << prop.type << ",\n";
        }
        source << "}\n";
    }

    std::stringstream source;
};

TableLikeDefinition::~TableLikeDefinition()
{
    builder->source << source.rdbuf() << "}\n";
}

static constexpr const char* kBuiltinDefinitionBaseSrc = R"BUILTIN_SRC(

@checked declare function require(target: any): any

@checked declare function getfenv(target: any): { [string]: any }

declare _G: any
declare _VERSION: string

declare function gcinfo(): number

declare function print<T...>(...: T...)

declare function type<T>(value: T): string
declare function typeof<T>(value: T): string

-- `assert` has a magic function attached that will give more detailed type information
declare function assert<T>(value: T, errorMessage: string?): T
declare function error<T>(message: T, level: number?): never

declare function tostring<T>(value: T): string
declare function tonumber<T>(value: T, radix: number?): number?

declare function rawequal<T1, T2>(a: T1, b: T2): boolean
declare function rawget<K, V>(tab: {[K]: V}, k: K): V?
declare function rawset<K, V>(tab: {[K]: V}, k: K, v: V): {[K]: V}
declare function rawlen<K, V>(obj: {[K]: V} | string): number

declare function setfenv<T..., R...>(target: number | (T...) -> R..., env: {[string]: any}): ((T...) -> R...)?

declare function ipairs<V>(tab: {V}): (({V}, number) -> (number?, V), {V}, number)

declare function pcall<A..., R...>(f: (A...) -> R..., ...: A...): (boolean, R...)

-- FIXME: The actual type of `xpcall` is:
-- <E, A..., R1..., R2...>(f: (A...) -> R1..., err: (E) -> R2..., A...) -> (true, R1...) | (false, R2...)
-- Since we can't represent the return value, we use (boolean, R1...).
declare function xpcall<E, A..., R1..., R2...>(f: (A...) -> R1..., err: (E) -> R2..., ...: A...): (boolean, R1...)

-- `select` has a magic function attached to provide more detailed type information
declare function select<A...>(i: string | number, ...: A...): ...any

@checked declare function newproxy(mt: boolean?): any

-- Cannot use `typeof` here because it will produce a polytype when we expect a monotype.
declare function unpack<V>(tab: {V}, i: number?, j: number?): ...V

)BUILTIN_SRC";

static constexpr const char* kBuiltinDefinitionBit32Src = R"BUILTIN_SRC(

declare bit32: {
    band: @checked (...number) -> number,
    bor: @checked (...number) -> number,
    bxor: @checked (...number) -> number,
    btest: @checked (number, ...number) -> boolean,
    rrotate: @checked (x: number, disp: number) -> number,
    lrotate: @checked (x: number, disp: number) -> number,
    lshift: @checked (x: number, disp: number) -> number,
    arshift: @checked (x: number, disp: number) -> number,
    rshift: @checked (x: number, disp: number) -> number,
    bnot: @checked (x: number) -> number,
    extract: @checked (n: number, field: number, width: number?) -> number,
    replace: @checked (n: number, v: number, field: number, width: number?) -> number,
    countlz: @checked (n: number) -> number,
    countrz: @checked (n: number) -> number,
    byteswap: @checked (n: number) -> number,
}

)BUILTIN_SRC";

static constexpr const char* kBuiltinDefinitionMathSrc = R"BUILTIN_SRC(

declare math: {
    frexp: @checked (n: number) -> (number, number),
    ldexp: @checked (s: number, e: number) -> number,
    fmod: @checked (x: number, y: number) -> number,
    modf: @checked (n: number) -> (number, number),
    pow: @checked (x: number, y: number) -> number,
    exp: @checked (n: number) -> number,

    ceil: @checked (n: number) -> number,
    floor: @checked (n: number) -> number,
    abs: @checked (n: number) -> number,
    sqrt: @checked (n: number) -> number,

    log: @checked (n: number, base: number?) -> number,
    log10: @checked (n: number) -> number,

    rad: @checked (n: number) -> number,
    deg: @checked (n: number) -> number,

    sin: @checked (n: number) -> number,
    cos: @checked (n: number) -> number,
    tan: @checked (n: number) -> number,
    sinh: @checked (n: number) -> number,
    cosh: @checked (n: number) -> number,
    tanh: @checked (n: number) -> number,
    atan: @checked (n: number) -> number,
    acos: @checked (n: number) -> number,
    asin: @checked (n: number) -> number,
    atan2: @checked (y: number, x: number) -> number,

    min: @checked (number, ...number) -> number,
    max: @checked (number, ...number) -> number,

    pi: number,
    huge: number,
    nan: number,
    e: number,
    phi: number,
    sqrt2: number,
    tau: number,

    randomseed: @checked (seed: number) -> (),
    random: @checked (number?, number?) -> number,

    sign: @checked (n: number) -> number,
    clamp: @checked (n: number, min: number, max: number) -> number,
    noise: @checked (x: number, y: number?, z: number?) -> number,
    round: @checked (n: number) -> number,
    map: @checked (x: number, inmin: number, inmax: number, outmin: number, outmax: number) -> number,
    lerp: @checked (a: number, b: number, t: number) -> number,

    isnan: @checked (x: number) -> boolean,
    isinf: @checked (x: number) -> boolean,
    isfinite: @checked (x: number) -> boolean,
}

)BUILTIN_SRC";

static constexpr const char* kBuiltinDefinitionOsSrc = R"BUILTIN_SRC(

type DateTypeArg = {
    year: number,
    month: number,
    day: number,
    hour: number?,
    min: number?,
    sec: number?,
    isdst: boolean?,
}

type DateTypeResult = {
    year: number,
    month: number,
    wday: number,
    yday: number,
    day: number,
    hour: number,
    min: number,
    sec: number,
    isdst: boolean,
}

declare os: {
    time: (time: DateTypeArg?) -> number,
    date: ((formatString: "*t" | "!*t", time: number?) -> DateTypeResult) & ((formatString: string?, time: number?) -> string),
    difftime: (t2: DateTypeResult | number, t1: DateTypeResult | number) -> number,
    clock: () -> number,
}

)BUILTIN_SRC";

static constexpr const char* kBuiltinDefinitionCoroutineSrc_DEPRECATED = R"BUILTIN_SRC(

declare coroutine: {
    create: <A..., R...>(f: (A...) -> R...) -> thread,
    resume: <A..., R...>(co: thread, A...) -> (boolean, R...),
    running: () -> thread,
    status: @checked (co: thread) -> "dead" | "running" | "normal" | "suspended",
    wrap: <A..., R...>(f: (A...) -> R...) -> ((A...) -> R...),
    yield: <A..., R...>(A...) -> R...,
    isyieldable: () -> boolean,
    close: @checked (co: thread) -> (boolean, any)
}

)BUILTIN_SRC";

static constexpr const char* kBuiltinDefinitionCoroutineSrc = R"BUILTIN_SRC(

declare coroutine: {
    create: <A..., R...>(f: (A...) -> R...) -> thread,
    resume: <A..., R...>(co: thread, A...) -> (boolean, R...),
    running: () -> thread,
    status: @checked (co: thread) -> "dead" | "running" | "normal" | "suspended",
    wrap: <A..., R...>(f: (A...) -> R...) -> ((A...) -> R...),
    yield: <A..., R...>(A...) -> R...,
    isyieldable: () -> boolean,
    close: @checked (co: thread) -> (boolean, any),
    finally: (co: thread, callback: (status: "finished" | "error" | "cancelled", ...any) -> ()) -> ()
}

)BUILTIN_SRC";

static constexpr const char* kBuiltinDefinitionTableSrc = R"BUILTIN_SRC(

declare table: {
    concat: <V>(t: {V}, sep: string?, i: number?, j: number?) -> string,
    insert: (<V>(t: {V}, value: V) -> ()) & (<V>(t: {V}, pos: number, value: V) -> ()),
    maxn: <V>(t: {V}) -> number,
    remove: <V>(t: {V}, number?) -> V?,
    sort: <V>(t: {V}, comp: ((V, V) -> boolean)?) -> (),
    create: <V>(count: number, value: V?) -> {V},
    find: <V>(haystack: {V}, needle: V, init: number?) -> number?,

    unpack: <V>(list: {V}, i: number?, j: number?) -> ...V,
    pack: <V>(...V) -> { n: number, [number]: V },

    getn: <V>(t: {V}) -> number,
    foreach: <K, V>(t: {[K]: V}, f: (K, V) -> ()) -> (),
    foreachi: <V>({V}, (number, V) -> ()) -> (),

    move: <V>(src: {V}, a: number, b: number, t: number, dst: {V}?) -> {V},

    clear: (table: {}) -> (),
    isfrozen: (t: {}) -> boolean,
}

)BUILTIN_SRC";

static constexpr const char* kBuiltinDefinitionDebugSrc = R"BUILTIN_SRC(

declare debug: {
    info: ((thread: thread, level: number, options: string) -> ...any) & ((level: number, options: string) -> ...any) & (<A..., R1...>(func: (A...) -> R1..., options: string) -> ...any),
    traceback: ((message: string?, level: number?) -> string) & ((thread: thread, message: string?, level: number?) -> string),
}

)BUILTIN_SRC";

static constexpr const char* kBuiltinDefinitionUtf8Src = R"BUILTIN_SRC(

declare utf8: {
    char: @checked (...number) -> string,
    charpattern: string,
    codes: @checked (str: string) -> ((string, number) -> (number, number), string, number),
    codepoint: @checked (str: string, i: number?, j: number?) -> ...number,
    len: @checked (s: string, i: number?, j: number?) -> (number?, number?),
    offset: @checked (s: string, n: number?, i: number?) -> number,
}

)BUILTIN_SRC";

static constexpr const char* kBuiltinDefinitionBufferSrcPrefix = R"BUILTIN_SRC(
--- Buffer API
declare buffer: {
    create: @checked (size: number) -> buffer,
    fromstring: @checked (str: string) -> buffer,
    tostring: @checked (b: buffer) -> string,
    len: @checked (b: buffer) -> number,
    copy: @checked (target: buffer, targetOffset: number, source: buffer, sourceOffset: number?, count: number?) -> (),
    fill: @checked (b: buffer, offset: number, value: number, count: number?) -> (),
    readi8: @checked (b: buffer, offset: number) -> number,
    readu8: @checked (b: buffer, offset: number) -> number,
    readi16: @checked (b: buffer, offset: number) -> number,
    readu16: @checked (b: buffer, offset: number) -> number,
    readi32: @checked (b: buffer, offset: number) -> number,
    readu32: @checked (b: buffer, offset: number) -> number,
    readf32: @checked (b: buffer, offset: number) -> number,
    readf64: @checked (b: buffer, offset: number) -> number,
)BUILTIN_SRC";

static constexpr const char* kBuiltinDefinitionBufferVectorSrc = R"BUILTIN_SRC(
    readvectorf32x2: @checked (b: buffer, offset: number) -> vector,
    readvectorf32x3: @checked (b: buffer, offset: number) -> vector,
    writevectorf32x2: @checked (b: buffer, offset: number, value: vector) -> (),
    writevectorf32x3: @checked (b: buffer, offset: number, value: vector) -> (),
)BUILTIN_SRC";

static constexpr const char* kBuiltinDefinitionBufferSrcSuffix = R"BUILTIN_SRC(
    writei8: @checked (b: buffer, offset: number, value: number) -> (),
    writeu8: @checked (b: buffer, offset: number, value: number) -> (),
    writei16: @checked (b: buffer, offset: number, value: number) -> (),
    writeu16: @checked (b: buffer, offset: number, value: number) -> (),
    writei32: @checked (b: buffer, offset: number, value: number) -> (),
    writeu32: @checked (b: buffer, offset: number, value: number) -> (),
    writef32: @checked (b: buffer, offset: number, value: number) -> (),
    writef64: @checked (b: buffer, offset: number, value: number) -> (),
    readstring: @checked (b: buffer, offset: number, count: number) -> string,
    writestring: @checked (b: buffer, offset: number, value: string, count: number?) -> (),
    readbits: @checked (b: buffer, bitOffset: number, bitCount: number) -> number,
    writebits: @checked (b: buffer, bitOffset: number, bitCount: number, value: number) -> (),
}

)BUILTIN_SRC";

static const char* const kBuiltinDefinitionVectorSrc = R"BUILTIN_SRC(

-- While vector would have been better represented as a built-in primitive type, type solver extern type handling covers most of the properties
declare extern type vector with
    read x: number
    read y: number
    read z: number
end

declare vector: {
    create: @checked (x: number, y: number, z: number?) -> vector,
    magnitude: @checked (vec: vector) -> number,
    normalize: @checked (vec: vector) -> vector,
    cross: @checked (vec1: vector, vec2: vector) -> vector,
    dot: @checked (vec1: vector, vec2: vector) -> number,
    angle: @checked (vec1: vector, vec2: vector, axis: vector?) -> number,
    floor: @checked (vec: vector) -> vector,
    ceil: @checked (vec: vector) -> vector,
    abs: @checked (vec: vector) -> vector,
    sign: @checked (vec: vector) -> vector,
    clamp: @checked (vec: vector, min: vector, max: vector) -> vector,
    max: @checked (vector, ...vector) -> vector,
    min: @checked (vector, ...vector) -> vector,
    lerp: @checked (vec1: vector, vec2: vector, t: number) -> vector,

    zero: vector,
    one: vector,
}

)BUILTIN_SRC";

static const char* kBuiltinDefinitionClassSrc = R"CLASS_SRC(
declare class: {
    isinstance: @checked (o: unknown, c: class) -> boolean,
    classof: @checked (o: unknown) -> class?
}
)CLASS_SRC";

std::string buildBuiltinTypeDefinitions();

std::string getBuiltinDefinitionSource()
{
    if (FFlag::LuauEZAddBuiltins)
        return buildBuiltinTypeDefinitions();
    else
    {
        std::string result = kBuiltinDefinitionBaseSrc;

        result += kBuiltinDefinitionBit32Src;
        result += kBuiltinDefinitionMathSrc;
        result += kBuiltinDefinitionOsSrc;

        if (FFlag::LuauCoroutineFinallyAnalysis)
            result += kBuiltinDefinitionCoroutineSrc;
        else
            result += kBuiltinDefinitionCoroutineSrc_DEPRECATED;

        result += kBuiltinDefinitionTableSrc;
        result += kBuiltinDefinitionDebugSrc;
        result += kBuiltinDefinitionUtf8Src;
        result += kBuiltinDefinitionBufferSrcPrefix;
        if (FFlag::DebugLuauBufferVectorLibrary)
            result += kBuiltinDefinitionBufferVectorSrc;
        result += kBuiltinDefinitionBufferSrcSuffix;

        if (FFlag::DebugLuauBufferCompare)
        {
            const std::string anchor = "writebits: @checked (b: buffer, bitOffset: number, bitCount: number, value: number) -> (),\n";
            const std::string field = "    compare: @checked (b1: buffer, offset1: number, b2: buffer, offset2: number?, count: number?) -> number,\n";

            size_t pos = result.find(anchor);
            if (pos != std::string::npos)
                result.insert(pos + anchor.size(), field);
        }

        result += kBuiltinDefinitionVectorSrc;

        if (FFlag::DebugLuauUserDefinedClasses && FFlag::LuauAllowGlobalDeclarationToBeCalledClass)
        {
            result += kBuiltinDefinitionClassSrc;
        }

        return result;
    }
}

// TODO: split into separate tagged unions when the new solver can appropriately handle that.
static constexpr const char* kBuiltinDefinitionTypeMethodSrc = R"BUILTIN_SRC(

export type type = {
    tag: "nil" | "unknown" | "never" | "any" | "boolean" | "number" | "string" | "buffer" | "thread" |
         "singleton" | "negation" | "union" | "intersection" | "table" | "function" | "extern" | "generic",

    is: (self: type, arg: string) -> boolean,
    issubtypeof: (self: type, arg: type) -> boolean,

    -- for singleton type
    value: (self: type) -> (string | boolean | nil),

    -- for negation type
    inner: (self: type) -> type,

    -- for union and intersection types
    components: (self: type) -> {type},

    -- for table type
    setproperty: (self: type, key: type, value: type?) -> (),
    setreadproperty: (self: type, key: type, value: type?) -> (),
    setwriteproperty: (self: type, key: type, value: type?) -> (),
    readproperty: (self: type, key: type) -> type?,
    writeproperty: (self: type, key: type) -> type?,
    properties: (self: type) -> { [type]: { read: type?, write: type? } },
    setindexer: (self: type, index: type, result: type) -> (),
    setreadindexer: (self: type, index: type, result: type) -> (),
    setwriteindexer: (self: type, index: type, result: type) -> (),
    indexer: (self: type) -> { index: type, readresult: type, writeresult: type }?,
    readindexer: (self: type) -> { index: type, result: type }?,
    writeindexer: (self: type) -> { index: type, result: type }?,
    setmetatable: (self: type, arg: type) -> (),
    metatable: (self: type) -> type?,

    -- for function type
    setparameters: (self: type, head: {type}?, tail: type?) -> (),
    parameters: (self: type) -> { head: {type}?, tail: type? },
    setreturns: (self: type, head: {type}?, tail: type? ) -> (),
    returns: (self: type) -> { head: {type}?, tail: type? },
    setgenerics: (self: type, {type}?) -> (),
    generics: (self: type) -> {type},

    -- for class type
    -- 'properties', 'metatable', 'indexer', 'readindexer' and 'writeindexer' are shared with table type
    readparent: (self: type) -> type?,
    writeparent: (self: type) -> type?,

    -- for generic type
    name: (self: type) -> string?,
    ispack: (self: type) -> boolean,
}

)BUILTIN_SRC";

static constexpr const char* kBuiltinDefinitionTypesLibSrc = R"BUILTIN_SRC(

declare types: {
    unknown: type,
    never: type,
    any: type,
    boolean: type,
    number: type,
    string: type,
    thread: type,
    buffer: type,

    singleton: @checked (arg: string | boolean | nil) -> type,
    optional: @checked (arg: type) -> type,
    generic: @checked (name: string, ispack: boolean?) -> type,
    negationof: @checked (arg: type) -> type,
    unionof: @checked (...type) -> type,
    intersectionof: @checked (...type) -> type,
    newtable: @checked (props: {[type]: type} | {[type]: { read: type?, write: type? } }?, indexer: { index: type, readresult: type, writeresult: type }?, metatable: type?) -> type,
    newfunction: @checked (parameters: { head: {type}?, tail: type? }?, returns: { head: {type}?, tail: type? }?, generics: {type}?) -> type,
    copy: @checked (arg: type) -> type,
}
)BUILTIN_SRC";

std::string buildTypeFunctionDefinitionSource();

std::string getTypeFunctionDefinitionSource()
{
    if (FFlag::LuauEZAddBuiltins)
    {
        return buildTypeFunctionDefinitionSource();
    }
    else
    {
        std::string result;

        result += kBuiltinDefinitionTypeMethodSrc;
        result += kBuiltinDefinitionTypesLibSrc;

        return result;
    }
}

static void buildBaseDefinitions(TypeDefBuilder& builder)
{
    builder.declareCheckedFunction("require", {{"target", "any"}}, "any");
    builder.declareCheckedFunction("getfenv", {{"target", "any"}}, "{ [string]: any }");
    builder.declareGlobal("_G", "any");
    builder.declareGlobal("_VERSION", "string");
    builder.declareFunction("gcinfo", {}, {}, "number");
    builder.declareFunction("print", {"T..."}, {{"...", "T..."}}, "");
    builder.declareFunction("type", {"T"}, {{"value", "T"}}, "string");
    builder.declareFunction("typeof", {"T"}, {{"value", "T"}}, "string");
    builder.declareFunction("assert", {"T"}, {{"value", "T"}, {"errorMessage", "string?"}}, "T");
    builder.declareFunction("error", {"T"}, {{"message", "T"}, {"level", "number?"}}, "never");
    builder.declareFunction("tostring", {"T"}, {{"value", "T"}}, "string");
    builder.declareFunction("tonumber", {"T"}, {{"value", "T"}, {"radix", "number?"}}, "number?");
    builder.declareFunction("rawequal", {"T1", "T2"}, {{"a", "T1"}, {"b", "T2"}}, "boolean");
    builder.declareFunction("rawget", {"K", "V"}, {{"tab", "{[K]: V}"}, {"k", "K"}}, "V?");
    builder.declareFunction("rawset", {"K", "V"}, {{"tab", "{[K]: V}"}, {"k", "K"}, {"v", "V"}}, "{[K]: V}");
    builder.declareFunction("rawlen", {"K", "V"}, {{"obj", "{[K]: V} | string"}}, "number");
    builder.declareFunction("setfenv", {"T...", "R..."}, {{"target", "number | (T...) -> R..."}, {"env", "{[string]: any}"}}, "((T...) -> R...)?");
    builder.declareFunction("ipairs", {"V"}, {{"tab", "{V}"}}, "(({V}, number) -> (number?, V), {V}, number)");
    builder.declareFunction("pcall", {"A...", "R..."}, {{"f", "(A...) -> R..."}, {"...", "A..."}}, "(boolean, R...)");
    builder.declareFunction(
        "xpcall", {"E", "A...", "R1...", "R2..."}, {{"f", "(A...) -> R1..."}, {"err", "(E) -> R2..."}, {"...", "A..."}}, "(boolean, R1...)"
    );
    builder.declareFunction("select", {"A..."}, {{"i", "string | number"}, {"...", "A..."}}, "...any");
    builder.declareCheckedFunction("newproxy", {{"mt", "boolean?"}}, "any");
    builder.declareFunction("unpack", {"V"}, {{"tab", "{V}"}, {"i", "number?"}, {"j", "number?"}}, "...V");
}

static void buildBit32Definitions(TypeDefBuilder& builder)
{
    TableLikeDefinition table = builder.declareTablePartial("bit32");
    table.registerProp("band", "@checked (...number) -> number");
    table.registerProp("bor", "@checked (...number) -> number");
    table.registerProp("bxor", "@checked (...number) -> number");
    table.registerProp("btest", "@checked (number, ...number) -> boolean");
    table.registerProp("rrotate", "@checked (x: number, disp: number) -> number");
    table.registerProp("lrotate", "@checked (x: number, disp: number) -> number");
    table.registerProp("lshift", "@checked (x: number, disp: number) -> number");
    table.registerProp("arshift", "@checked (x: number, disp: number) -> number");
    table.registerProp("rshift", "@checked (x: number, disp: number) -> number");
    table.registerProp("bnot", "@checked (x: number) -> number");
    table.registerProp("extract", "@checked (n: number, field: number, width: number?) -> number");
    table.registerProp("replace", "@checked (n: number, v: number, field: number, width: number?) -> number");
    table.registerProp("countlz", "@checked (n: number) -> number");
    table.registerProp("countrz", "@checked (n: number) -> number");
    table.registerProp("byteswap", "@checked (n: number) -> number");
}

static void buildMathDefinitions(TypeDefBuilder& builder)
{
    TableLikeDefinition table = builder.declareTablePartial("math");
    table.registerProp("frexp", "@checked (n: number) -> (number, number)");
    table.registerProp("ldexp", "@checked (s: number, e: number) -> number");
    table.registerProp("fmod", "@checked (x: number, y: number) -> number");
    table.registerProp("modf", "@checked (n: number) -> (number, number)");
    table.registerProp("pow", "@checked (x: number, y: number) -> number");
    table.registerProp("exp", "@checked (n: number) -> number");
    table.registerProp("ceil", "@checked (n: number) -> number");
    table.registerProp("floor", "@checked (n: number) -> number");
    table.registerProp("abs", "@checked (n: number) -> number");
    table.registerProp("sqrt", "@checked (n: number) -> number");
    table.registerProp("log", "@checked (n: number, base: number?) -> number");
    table.registerProp("log10", "@checked (n: number) -> number");
    table.registerProp("rad", "@checked (n: number) -> number");
    table.registerProp("deg", "@checked (n: number) -> number");
    table.registerProp("sin", "@checked (n: number) -> number");
    table.registerProp("cos", "@checked (n: number) -> number");
    table.registerProp("tan", "@checked (n: number) -> number");
    table.registerProp("sinh", "@checked (n: number) -> number");
    table.registerProp("cosh", "@checked (n: number) -> number");
    table.registerProp("tanh", "@checked (n: number) -> number");
    table.registerProp("atan", "@checked (n: number) -> number");
    table.registerProp("acos", "@checked (n: number) -> number");
    table.registerProp("asin", "@checked (n: number) -> number");
    table.registerProp("atan2", "@checked (y: number, x: number) -> number");
    table.registerProp("min", "@checked (number, ...number) -> number");
    table.registerProp("max", "@checked (number, ...number) -> number");
    table.registerProp("pi", "number");
    table.registerProp("huge", "number");
    table.registerProp("nan", "number");
    table.registerProp("e", "number");
    table.registerProp("phi", "number");
    table.registerProp("sqrt2", "number");
    table.registerProp("tau", "number");
    table.registerProp("randomseed", "@checked (seed: number) -> ()");
    table.registerProp("random", "@checked (number?, number?) -> number");
    table.registerProp("sign", "@checked (n: number) -> number");
    table.registerProp("clamp", "@checked (n: number, min: number, max: number) -> number");
    table.registerProp("noise", "@checked (x: number, y: number?, z: number?) -> number");
    table.registerProp("round", "@checked (n: number) -> number");
    table.registerProp("map", "@checked (x: number, inmin: number, inmax: number, outmin: number, outmax: number) -> number");
    table.registerProp("lerp", "@checked (a: number, b: number, t: number) -> number");
    table.registerProp("isnan", "@checked (x: number) -> boolean");
    table.registerProp("isinf", "@checked (x: number) -> boolean");
    table.registerProp("isfinite", "@checked (x: number) -> boolean");
}

static void buildOsDefinitions(TypeDefBuilder& builder)
{
    {
        TableLikeDefinition dateTypeArg = builder.declareTypePartial("DateTypeArg", false);
        TableLikeDefinition dateTypeResult = builder.declareTypePartial("DateTypeResult", false);
        dateTypeArg.registerProp("year", "number");
        dateTypeArg.registerProp("month", "number");
        dateTypeArg.registerProp("day", "number");
        dateTypeArg.registerProp("hour", "number?");
        dateTypeArg.registerProp("min", "number?");
        dateTypeArg.registerProp("sec", "number?");
        dateTypeArg.registerProp("isdst", "boolean?");

        dateTypeResult.registerProp("year", "number");
        dateTypeResult.registerProp("month", "number");
        dateTypeResult.registerProp("wday", "number");
        dateTypeResult.registerProp("yday", "number");
        dateTypeResult.registerProp("day", "number");
        dateTypeResult.registerProp("hour", "number");
        dateTypeResult.registerProp("min", "number");
        dateTypeResult.registerProp("sec", "number");
        dateTypeResult.registerProp("isdst", "boolean");
    }

    TableLikeDefinition table = builder.declareTablePartial("os");
    table.registerProp("time", "(time: DateTypeArg?) -> number");
    table.registerProp(
        "date", "((formatString: \"*t\" | \"!*t\", time: number?) -> DateTypeResult) & ((formatString: string?, time: number?) -> string)"
    );
    table.registerProp("difftime", "(t2: DateTypeResult | number, t1: DateTypeResult | number) -> number");
    table.registerProp("clock", "() -> number");
}

static void buildCoroutineDefinitions(TypeDefBuilder& builder)
{
    TableLikeDefinition table = builder.declareTablePartial("coroutine");
    table.registerProp("create", "<A..., R...>(f: (A...) -> R...) -> thread");
    table.registerProp("resume", "<A..., R...>(co: thread, A...) -> (boolean, R...)");
    table.registerProp("running", "() -> thread");
    table.registerProp("status", "@checked (co: thread) -> \"dead\" | \"running\" | \"normal\" | \"suspended\"");
    table.registerProp("wrap", "<A..., R...>(f: (A...) -> R...) -> ((A...) -> R...)");
    table.registerProp("yield", "<A..., R...>(A...) -> R...");
    table.registerProp("isyieldable", "() -> boolean");
    table.registerProp("close", "@checked (co: thread) -> (boolean, any)");
    if (FFlag::LuauCoroutineFinallyAnalysis)
        table.registerProp("finally", "(co: thread, callback: (status: \"finished\" | \"error\" | \"cancelled\", ...any) -> ()) -> ()");
}

static void buildTableDefinitions(TypeDefBuilder& builder)
{
    TableLikeDefinition table = builder.declareTablePartial("table");
    table.registerProp("concat", "<V>(t: {V}, sep: string?, i: number?, j: number?) -> string");
    table.registerProp("insert", "(<V>(t: {V}, value: V) -> ()) & (<V>(t: {V}, pos: number, value: V) -> ())");
    table.registerProp("maxn", "<V>(t: {V}) -> number");
    table.registerProp("remove", "<V>(t: {V}, number?) -> V?");
    table.registerProp("sort", "<V>(t: {V}, comp: ((V, V) -> boolean)?) -> ()");
    table.registerProp("create", "<V>(count: number, value: V?) -> {V}");
    table.registerProp("find", "<V>(haystack: {V}, needle: V, init: number?) -> number?");
    table.registerProp("unpack", "<V>(list: {V}, i: number?, j: number?) -> ...V");
    table.registerProp("pack", "<V>(...V) -> { n: number, [number]: V }");
    table.registerProp("getn", "<V>(t: {V}) -> number");
    table.registerProp("foreach", "<K, V>(t: {[K]: V}, f: (K, V) -> ()) -> ()");
    table.registerProp("foreachi", "<V>({V}, (number, V) -> ()) -> ()");
    table.registerProp("move", "<V>(src: {V}, a: number, b: number, t: number, dst: {V}?) -> {V}");
    table.registerProp("clear", "(table: {}) -> ()");
    table.registerProp("isfrozen", "(t: {}) -> boolean");
}

static void buildDebugDefinitions(TypeDefBuilder& builder)
{
    TableLikeDefinition table = builder.declareTablePartial("debug");
    table.registerProp(
        "info",
        "((thread: thread, level: number, options: string) -> ...any) & ((level: number, options: string) -> "
        "...any) & (<A..., R1...>(func: (A...) -> R1..., options: string) -> ...any)"
    );
    table.registerProp(
        "traceback", "((message: string?, level: number?) -> string) & ((thread: thread, message: string?, level: number?) -> string)"
    );
}

static void buildUtf8Definitions(TypeDefBuilder& builder)
{
    TableLikeDefinition table = builder.declareTablePartial("utf8");
    table.registerProp("char", "@checked (...number) -> string");
    table.registerProp("charpattern", "string");
    table.registerProp("codes", "@checked (str: string) -> ((string, number) -> (number, number), string, number)");
    table.registerProp("codepoint", "@checked (str: string, i: number?, j: number?) -> ...number");
    table.registerProp("len", "@checked (s: string, i: number?, j: number?) -> (number?, number?)");
    table.registerProp("offset", "@checked (s: string, n: number?, i: number?) -> number");
}

static void buildBufferDefinitions(TypeDefBuilder& builder)
{
    TableLikeDefinition table = builder.declareTablePartial("buffer");
    table.registerProp("create", "@checked (size: number) -> buffer");
    table.registerProp("fromstring", "@checked (str: string) -> buffer");
    table.registerProp("tostring", "@checked (b: buffer) -> string");
    table.registerProp("len", "@checked (b: buffer) -> number");
    table.registerProp("copy", "@checked (target: buffer, targetOffset: number, source: buffer, sourceOffset: number?, count: number?) -> ()");
    table.registerProp("fill", "@checked (b: buffer, offset: number, value: number, count: number?) -> ()");
    table.registerProp("readi8", "@checked (b: buffer, offset: number) -> number");
    table.registerProp("readu8", "@checked (b: buffer, offset: number) -> number");
    table.registerProp("readi16", "@checked (b: buffer, offset: number) -> number");
    table.registerProp("readu16", "@checked (b: buffer, offset: number) -> number");
    table.registerProp("readi32", "@checked (b: buffer, offset: number) -> number");
    table.registerProp("readu32", "@checked (b: buffer, offset: number) -> number");
    table.registerProp("readf32", "@checked (b: buffer, offset: number) -> number");
    table.registerProp("readf64", "@checked (b: buffer, offset: number) -> number");
    if (FFlag::DebugLuauBufferVectorLibrary)
    {
        table.registerProp("readvectorf32x2", "@checked (b: buffer, offset: number) -> vector");
        table.registerProp("readvectorf32x3", "@checked (b: buffer, offset: number) -> vector");
        table.registerProp("writevectorf32x2", "@checked (b: buffer, offset: number, value: vector) -> ()");
        table.registerProp("writevectorf32x3", "@checked (b: buffer, offset: number, value: vector) -> ()");
    }
    table.registerProp("writei8", "@checked (b: buffer, offset: number, value: number) -> ()");
    table.registerProp("writeu8", "@checked (b: buffer, offset: number, value: number) -> ()");
    table.registerProp("writei16", "@checked (b: buffer, offset: number, value: number) -> ()");
    table.registerProp("writeu16", "@checked (b: buffer, offset: number, value: number) -> ()");
    table.registerProp("writei32", "@checked (b: buffer, offset: number, value: number) -> ()");
    table.registerProp("writeu32", "@checked (b: buffer, offset: number, value: number) -> ()");
    table.registerProp("writef32", "@checked (b: buffer, offset: number, value: number) -> ()");
    table.registerProp("writef64", "@checked (b: buffer, offset: number, value: number) -> ()");
    table.registerProp("readstring", "@checked (b: buffer, offset: number, count: number) -> string");
    table.registerProp("writestring", "@checked (b: buffer, offset: number, value: string, count: number?) -> ()");
    table.registerProp("readbits", "@checked (b: buffer, bitOffset: number, bitCount: number) -> number");
    table.registerProp("writebits", "@checked (b: buffer, bitOffset: number, bitCount: number, value: number) -> ()");
    if (FFlag::DebugLuauBufferCompare)
        table.registerProp("compare", "@checked (b1: buffer, offset1: number, b2: buffer, offset2: number?, count: number?) -> number");
}

static void buildVectorDefinitions(TypeDefBuilder& builder)
{
    builder.declareMagic(R"VECTOR_SRC(

-- While vector would have been better represented as a built-in primitive type, type solver extern type handling covers most of the properties
declare extern type vector with
    read x: number
    read y: number
    read z: number
end

)VECTOR_SRC");
    TableLikeDefinition table = builder.declareTablePartial("vector");
    table.registerProp("create", "@checked (x: number, y: number, z: number?) -> vector");
    table.registerProp("magnitude", "@checked (vec: vector) -> number");
    table.registerProp("normalize", "@checked (vec: vector) -> vector");
    table.registerProp("cross", "@checked (vec1: vector, vec2: vector) -> vector");
    table.registerProp("dot", "@checked (vec1: vector, vec2: vector) -> number");
    table.registerProp("angle", "@checked (vec1: vector, vec2: vector, axis: vector?) -> number");
    table.registerProp("floor", "@checked (vec: vector) -> vector");
    table.registerProp("ceil", "@checked (vec: vector) -> vector");
    table.registerProp("abs", "@checked (vec: vector) -> vector");
    table.registerProp("sign", "@checked (vec: vector) -> vector");
    table.registerProp("clamp", "@checked (vec: vector, min: vector, max: vector) -> vector");
    table.registerProp("max", "@checked (vector, ...vector) -> vector");
    table.registerProp("min", "@checked (vector, ...vector) -> vector");
    table.registerProp("lerp", "@checked (vec1: vector, vec2: vector, t: number) -> vector");
    table.registerProp("zero", "vector");
    table.registerProp("one", "vector");
}

static void buildClassDefinitions(TypeDefBuilder& builder)
{
    if (FFlag::DebugLuauUserDefinedClasses && FFlag::LuauAllowGlobalDeclarationToBeCalledClass)
    {
        TableLikeDefinition table = builder.declareTablePartial("class");
        table.registerProp("isinstance", "@checked (o: unknown, c: class) -> boolean");
        table.registerProp("classof", "@checked (o: unknown) -> class?");
    }
}

std::string buildBuiltinTypeDefinitions()
{
    TypeDefBuilder builder;
    buildBaseDefinitions(builder);
    buildBit32Definitions(builder);
    buildMathDefinitions(builder);
    buildOsDefinitions(builder);
    buildCoroutineDefinitions(builder);
    buildTableDefinitions(builder);
    buildDebugDefinitions(builder);
    buildUtf8Definitions(builder);
    buildBufferDefinitions(builder);
    buildVectorDefinitions(builder);
    buildClassDefinitions(builder);
    return builder.str();
}

static void buildTypeMethodDefinitions(TypeDefBuilder& builder)
{
    TableLikeDefinition table = builder.declareTypePartial("type", true);
    table.registerProp(
        "tag",
        "\"nil\" | \"unknown\" | \"never\" | \"any\" | \"boolean\" | \"number\" | \"string\" | \"buffer\" | \"thread\" | \"singleton\" | "
        "\"negation\" | \"union\" | \"intersection\" | \"table\" | \"function\" | \"extern\" | \"generic\""
    );
    table.registerProp("is", "(self: type, arg: string) -> boolean");
    table.registerProp("issubtypeof", "(self: type, arg: type) -> boolean");
    table.registerProp("value", "(self: type) -> (string | boolean | nil)");
    table.registerProp("inner", "(self: type) -> type");
    table.registerProp("components", "(self: type) -> {type}");
    table.registerProp("setproperty", "(self: type, key: type, value: type?) -> ()");
    table.registerProp("setreadproperty", "(self: type, key: type, value: type?) -> ()");
    table.registerProp("setwriteproperty", "(self: type, key: type, value: type?) -> ()");
    table.registerProp("readproperty", "(self: type, key: type) -> type?");
    table.registerProp("writeproperty", "(self: type, key: type) -> type?");
    table.registerProp("properties", "(self: type) -> { [type]: { read: type?, write: type? } }");
    table.registerProp("setindexer", "(self: type, index: type, result: type) -> ()");
    table.registerProp("setreadindexer", "(self: type, index: type, result: type) -> ()");
    table.registerProp("setwriteindexer", "(self: type, index: type, result: type) -> ()");
    table.registerProp("indexer", "(self: type) -> { index: type, readresult: type, writeresult: type }?");
    table.registerProp("readindexer", "(self: type) -> { index: type, result: type }?");
    table.registerProp("writeindexer", "(self: type) -> { index: type, result: type }?");
    table.registerProp("setmetatable", "(self: type, arg: type) -> ()");
    table.registerProp("metatable", "(self: type) -> type?");
    table.registerProp("setparameters", "(self: type, head: {type}?, tail: type?) -> ()");
    table.registerProp("parameters", "(self: type) -> { head: {type}?, tail: type? }");
    table.registerProp("setreturns", "(self: type, head: {type}?, tail: type? ) -> ()");
    table.registerProp("returns", "(self: type) -> { head: {type}?, tail: type? }");
    table.registerProp("setgenerics", "(self: type, {type}?) -> ()");
    table.registerProp("generics", "(self: type) -> {type}");
    table.registerProp("readparent", "(self: type) -> type?");
    table.registerProp("writeparent", "(self: type) -> type?");
    table.registerProp("name", "(self: type) -> string?");
    table.registerProp("ispack", "(self: type) -> boolean");
}

static void buildTypesLibraryDefinitions(TypeDefBuilder& builder)
{
    TableLikeDefinition table = builder.declareTablePartial("types");
    table.registerProp("unknown", "type");
    table.registerProp("never", "type");
    table.registerProp("any", "type");
    table.registerProp("boolean", "type");
    table.registerProp("number", "type");
    table.registerProp("string", "type");
    table.registerProp("thread", "type");
    table.registerProp("buffer", "type");
    table.registerProp("singleton", "@checked (arg: string | boolean | nil) -> type");
    table.registerProp("optional", "@checked (arg: type) -> type");
    table.registerProp("generic", "@checked (name: string, ispack: boolean?) -> type");
    table.registerProp("negationof", "@checked (arg: type) -> type");
    table.registerProp("unionof", "@checked (...type) -> type");
    table.registerProp("intersectionof", "@checked (...type) -> type");
    table.registerProp(
        "newtable",
        "@checked (props: {[type]: type} | {[type]: { read: type?, write: type? } }?, indexer: { index: type, readresult: "
        "type, writeresult: type? }?, metatable: type?) -> type"
    );
    table.registerProp(
        "newfunction",
        "@checked (parameters: { head: {type}?, tail: type? }?, returns: { head: {type}?, tail: type? }?, generics: {type}?) "
        "-> type"
    );
    table.registerProp("copy", "@checked (arg: type) -> type");
}

std::string buildTypeFunctionDefinitionSource()
{
    TypeDefBuilder builder;
    buildTypeMethodDefinitions(builder);
    buildTypesLibraryDefinitions(builder);
    return builder.str();
}

} // namespace Luau
