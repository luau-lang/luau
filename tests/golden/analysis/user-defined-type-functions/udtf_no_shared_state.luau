type function foo()
    if not glob then
        glob = "a"
    else
        glob ..= "b"
    end

    return glob
end

type function bar(prefix)
    return types.singleton(tostring(prefix:value()) .. foo())
end

local function _ok1(idx: bar<"x">): nil
    return idx
end

local function _ok2(idx: bar<"y">): nil
    return idx
end
