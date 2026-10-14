-- https://github.com/luau-lang/luau/issues/1620

type TestA<T...> = {
    a: (T...) -> (),
    b: (number, T...) -> ()
}

local function testA<T...>(arg:TestA<T...>) end
local a:TestA<string> = nil::any
testA(a)

type TestB<T...> = {
    a: (T...) -> (),
    b: (number, boolean, T...) -> ()
}

local function testB<T...>(arg:TestB<T...>) end
local b:TestB<string> = nil::any
testB(b)
