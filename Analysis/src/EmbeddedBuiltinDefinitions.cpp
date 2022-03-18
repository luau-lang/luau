// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"

namespace Luau
{

static const std::string kBuiltinDefinitionLuaSrc = R"BUILTIN_SRC(

declare bit32: {
    -- band, bor, bxor, and btest are declared in C++
    rrotate: (number, number) -> number,
    lrotate: (number, number) -> number,
    lshift: (number, number) -> number,
    arshift: (number, number) -> number,
    rshift: (number, number) -> number,
    bnot: (number) -> number,
    extract: (number, number, number?) -> number,
    replace: (number, number, number, number?) -> number,
    countlz: (number) -> number,
    countrz: (number) -> number,
}

declare math: {
    frexp: (number) -> (number, number),
    ldexp: (number, number) -> number,
    fmod: (number, number) -> number,
    modf: (number) -> (number, number),
    pow: (number, number) -> number,
    exp: (number) -> number,

    ceil: (number) -> number,
    floor: (number) -> number,
    abs: (number) -> number,
    sqrt: (number) -> number,

    log: (number, number?) -> number,
    log10: (number) -> number,

    rad: (number) -> number,
    deg: (number) -> number,

    sin: (number) -> number,
    cos: (number) -> number,
    tan: (number) -> number,
    sinh: (number) -> number,
    cosh: (number) -> number,
    tanh: (number) -> number,
    atan: (number) -> number,
    acos: (number) -> number,
    asin: (number) -> number,
    atan2: (number, number) -> number,

    -- min and max are declared in C++.

    pi: number,
    huge: number,

    randomseed: (number) -> (),
    random: (number?, number?) -> number,

    sign: (number) -> number,
    clamp: (number, number, number) -> number,
    noise: (number, number?, number?) -> number,
    round: (number) -> number,
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
    time: (DateTypeArg?) -> number,
    date: (string?, number?) -> DateTypeResult | string,
    difftime: (DateTypeResult | number, DateTypeResult | number) -> number,
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

declare function error<T>(message: T, level: number?)

declare function tostring<T>(value: T): string
declare function tonumber<T>(value: T, radix: number?): number?

declare function rawequal<T1, T2>(a: T1, b: T2): boolean
declare function rawget<K, V>(tab: {[K]: V}, k: K): V
declare function rawset<K, V>(tab: {[K]: V}, k: K, v: V): {[K]: V}

declare function setfenv<T..., R...>(target: number | (T...) -> R..., env: {[string]: any}): ((T...) -> R...)?

declare function ipairs<V>(tab: {V}): (({V}, number) -> (number, V), {V}, number)

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
    create: <A..., R...>((A...) -> R...) -> thread,
    resume: <A..., R...>(thread, A...) -> (boolean, R...),
    running: () -> thread,
    status: (thread) -> string,
    -- FIXME: This technically returns a function, but we can't represent this yet.
    wrap: <A..., R...>((A...) -> R...) -> any,
    yield: <A..., R...>(A...) -> R...,
    isyieldable: () -> boolean,
    close: (thread) -> (boolean, any)
}

declare table: {
    concat: <V>({V}, string?, number?, number?) -> string,
    insert: (<V>({V}, V) -> ()) & (<V>({V}, number, V) -> ()),
    maxn: <V>({V}) -> number,
    remove: <V>({V}, number?) -> V?,
    sort: <V>({V}, ((V, V) -> boolean)?) -> (),
    create: <V>(number, V?) -> {V},
    find: <V>({V}, V, number?) -> number?,

    unpack: <V>({V}, number?, number?) -> ...V,
    pack: <V>(...V) -> { n: number, [number]: V },

    getn: <V>({V}) -> number,
    foreach: <K, V>({[K]: V}, (K, V) -> ()) -> (),
    foreachi: <V>({V}, (number, V) -> ()) -> (),

    move: <V>({V}, number, number, number, {V}?) -> {V},
    clear: <K, V>({[K]: V}) -> (),

    isfrozen: <K, V>({[K]: V}) -> boolean,
}

declare debug: {
    info: (<R...>(thread, number, string) -> R...) & (<R...>(number, string) -> R...) & (<A..., R1..., R2...>((A...) -> R1..., string) -> R2...),
    traceback: ((string?, number?) -> string) & ((thread, string?, number?) -> string),
}

declare utf8: {
    char: (number, ...number) -> string,
    charpattern: string,
    codes: (string) -> ((string, number) -> (number, number), string, number),
    -- FIXME
    codepoint: (string, number?, number?) -> (number, ...number),
    len: (string, number?, number?) -> (number?, number?),
    offset: (string, number?, number?) -> number,
    nfdnormalize: (string) -> string,
    nfcnormalize: (string) -> string,
    graphemes: (string, number?, number?) -> (() -> (number, number)),
}

-- Cannot use `typeof` here because it will produce a polytype when we expect a monotype.
declare function unpack<V>(tab: {V}, i: number?, j: number?): ...V

)BUILTIN_SRC";

std::string getBuiltinDefinitionSource()
{
    return kBuiltinDefinitionLuaSrc;
}

} // namespace Luau
