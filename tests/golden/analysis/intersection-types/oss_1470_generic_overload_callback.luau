-- https://github.com/luau-lang/luau/issues/1470

type Atom<T> = (() -> T) & (val: T | (T) -> T) -> T

type Test = {
    foo: string,
    bar: number,
}

local function _check(atomFn: Atom<Test>)
    atomFn(function(v)
        return v
    end)
end
