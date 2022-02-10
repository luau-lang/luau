local function id(x)
  return x
end
local function comp(f)
  return function(g)
    return function(x)
      return f(g(x))
    end
  end
end
local id2 = comp(id)(id)
local nil2 = id2(nil)
return id2(nil2)
