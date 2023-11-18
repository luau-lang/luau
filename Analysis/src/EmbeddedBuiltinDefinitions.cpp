// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"

LUAU_FASTFLAGVARIABLE(LuauBufferDefinitions, false)
LUAU_FASTFLAGVARIABLE(LuauBufferTypeck, false)

namespace Luau
{

static const std::string kBuiltinDefinitionBufferSrc_DEPRECATED = R"BUILTIN_SRC(

-- TODO: this will be replaced with a built-in primitive type
declare class buffer end

declare buffer: {
    create: (size: number) -> buffer,
    fromstring: (str: string) -> buffer,
    tostring: () -> string,
    len: (b: buffer) -> number,
    copy: (target: buffer, targetOffset: number, source: buffer, sourceOffset: number?, count: number?) -> (),
    fill: (b: buffer, offset: number, value: number, count: number?) -> (),
    readi8: (b: buffer, offset: number) -> number,
    readu8: (b: buffer, offset: number) -> number,
    readi16: (b: buffer, offset: number) -> number,
    readu16: (b: buffer, offset: number) -> number,
    readi32: (b: buffer, offset: number) -> number,
    readu32: (b: buffer, offset: number) -> number,
    readf32: (b: buffer, offset: number) -> number,
    readf64: (b: buffer, offset: number) -> number,
    writei8: (b: buffer, offset: number, value: number) -> (),
    writeu8: (b: buffer, offset: number, value: number) -> (),
    writei16: (b: buffer, offset: number, value: number) -> (),
    writeu16: (b: buffer, offset: number, value: number) -> (),
    writei32: (b: buffer, offset: number, value: number) -> (),
    writeu32: (b: buffer, offset: number, value: number) -> (),
    writef32: (b: buffer, offset: number, value: number) -> (),
    writef64: (b: buffer, offset: number, value: number) -> (),
    readstring: (b: buffer, offset: number, count: number) -> string,
    writestring: (b: buffer, offset: number, value: string, count: number?) -> (),
}

)BUILTIN_SRC";

static const std::string kBuiltinDefinitionBufferSrc = R"BUILTIN_SRC(

declare buffer: {
    create: (size: number) -> buffer,
    fromstring: (str: string) -> buffer,
    tostring: (b: buffer) -> string,
    len: (b: buffer) -> number,
    copy: (target: buffer, targetOffset: number, source: buffer, sourceOffset: number?, count: number?) -> (),
    fill: (b: buffer, offset: number, value: number, count: number?) -> (),
    readi8: (b: buffer, offset: number) -> number,
    readu8: (b: buffer, offset: number) -> number,
    readi16: (b: buffer, offset: number) -> number,
    readu16: (b: buffer, offset: number) -> number,
    readi32: (b: buffer, offset: number) -> number,
    readu32: (b: buffer, offset: number) -> number,
    readf32: (b: buffer, offset: number) -> number,
    readf64: (b: buffer, offset: number) -> number,
    writei8: (b: buffer, offset: number, value: number) -> (),
    writeu8: (b: buffer, offset: number, value: number) -> (),
    writei16: (b: buffer, offset: number, value: number) -> (),
    writeu16: (b: buffer, offset: number, value: number) -> (),
    writei32: (b: buffer, offset: number, value: number) -> (),
    writeu32: (b: buffer, offset: number, value: number) -> (),
    writef32: (b: buffer, offset: number, value: number) -> (),
    writef64: (b: buffer, offset: number, value: number) -> (),
    readstring: (b: buffer, offset: number, count: number) -> string,
    writestring: (b: buffer, offset: number, value: string, count: number?) -> (),
}

)BUILTIN_SRC";
static const std::string kBuiltinDefinitionLuaSrc = R"BUILTIN_SRC(

declare bit32: {
    band: (...number) -> number,
    bor: (...number) -> number,
    bxor: (...number) -> number,
    btest: (number, ...number) -> boolean,
    rrotate: (x: number, disp: number) -> number,
    lrotate: (x: number, disp: number) -> number,
    lshift: (x: number, disp: number) -> number,
    arshift: (x: number, disp: number) -> number,
    rshift: (x: number, disp: number) -> number,
    bnot: (x: number) -> number,
    extract: (n: number, field: number, width: number?) -> number,
    replace: (n: number, v: number, field: number, width: number?) -> number,
    countlz: (n: number) -> number,
    countrz: (n: number) -> number,
    byteswap: (n: number) -> number,
}

declare math: {
    frexp: (n: number) -> (number, number),
    ldexp: (s: number, e: number) -> number,
    fmod: (x: number, y: number) -> number,
    modf: (n: number) -> (number, number),
    pow: (x: number, y: number) -> number,
    exp: (n: number) -> number,

    ceil: (n: number) -> number,
    floor: (n: number) -> number,
    abs: (n: number) -> number,
    sqrt: (n: number) -> number,

    log: (n: number, base: number?) -> number,
    log10: (n: number) -> number,

    rad: (n: number) -> number,
    deg: (n: number) -> number,

    sin: (n: number) -> number,
    cos: (n: number) -> number,
    tan: (n: number) -> number,
    sinh: (n: number) -> number,
    cosh: (n: number) -> number,
    tanh: (n: number) -> number,
    atan: (n: number) -> number,
    acos: (n: number) -> number,
    asin: (n: number) -> number,
    atan2: (y: number, x: number) -> number,

    min: (number, ...number) -> number,
    max: (number, ...number) -> number,

    pi: number,
    huge: number,

    randomseed: (seed: number) -> (),
    random: (number?, number?) -> number,

    sign: (n: number) -> number,
    clamp: (n: number, min: number, max: number) -> number,
    noise: (x: number, y: number?, z: number?) -> number,
    round: (n: number) -> number,
}

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

declare function require(target: any): any

declare function getfenv(target: any): { [string]: any }

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
declare function rawget<K, V>(tab: {[K]: V}, k: K): V
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

-- FIXME: This type is not entirely correct - `loadstring` returns a function or
-- (nil, string).
declare function loadstring<A...>(src: string, chunkname: string?): (((A...) -> any)?, string?)

declare function newproxy(mt: boolean?): any

declare coroutine: {
    create: <A..., R...>(f: (A...) -> R...) -> thread,
    resume: <A..., R...>(co: thread, A...) -> (boolean, R...),
    running: () -> thread,
    status: (co: thread) -> "dead" | "running" | "normal" | "suspended",
    wrap: <A..., R...>(f: (A...) -> R...) -> ((A...) -> R...),
    yield: <A..., R...>(A...) -> R...,
    isyieldable: () -> boolean,
    close: (co: thread) -> (boolean, any)
}

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
    clear: <K, V>(table: {[K]: V}) -> (),

    isfrozen: <K, V>(t: {[K]: V}) -> boolean,
}

declare debug: {
    info: (<R...>(thread: thread, level: number, options: string) -> R...) & (<R...>(level: number, options: string) -> R...) & (<A..., R1..., R2...>(func: (A...) -> R1..., options: string) -> R2...),
    traceback: ((message: string?, level: number?) -> string) & ((thread: thread, message: string?, level: number?) -> string),
}

declare utf8: {
    char: (...number) -> string,
    charpattern: string,
    codes: (str: string) -> ((string, number) -> (number, number), string, number),
    codepoint: (str: string, i: number?, j: number?) -> ...number,
    len: (s: string, i: number?, j: number?) -> (number?, number?),
    offset: (s: string, n: number?, i: number?) -> number,
}

-- Cannot use `typeof` here because it will produce a polytype when we expect a monotype.
declare function unpack<V>(tab: {V}, i: number?, j: number?): ...V

)BUILTIN_SRC";

std::string getBuiltinDefinitionSource()
{
    std::string result = kBuiltinDefinitionLuaSrc;

    if (FFlag::LuauBufferTypeck)
        result = kBuiltinDefinitionBufferSrc + result;
    else if (FFlag::LuauBufferDefinitions)
        result = kBuiltinDefinitionBufferSrc_DEPRECATED + result;

    return result;
}

} // namespace Luau
