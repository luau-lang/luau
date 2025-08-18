-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing native code generation with type annotations")

function call(fn, ...)
  local ok, res = pcall(fn, ...)
  assert(ok)
  return res
end

function ecall(fn, ...)
  local ok, err = pcall(fn, ...)
  assert(not ok)
  return err:sub(err:find(": ") + 2, #err)
end

local function add(a: number, b: number, native: boolean)
  assert(native == is_native())
  return a + b
end

call(add, 1, 3, true)
ecall(add, nil, 2, false)

local function isnil(x: nil)
  assert(is_native())
  return not x
end

call(isnil, nil)
ecall(isnil, 2)

local function isany(x: any, y: number)
  assert(is_native())
  return not not x
end

call(isany, nil, 1)
call(isany, 2, 1)
call(isany, {}, 1)

local function optstring(s: string?)
  assert(is_native())
  return if s then s..'2' else '3'
end

assert(call(optstring, nil) == '3')
assert(call(optstring, 'two: ') == 'two: 2')
ecall(optstring, 2)

local function checktable(a: {x:number}) assert(is_native()) end
local function checkfunction(a: () -> ()) assert(is_native()) end
local function checkthread(a: thread) assert(is_native()) end
local function checkuserdata(a: userdata) assert(is_native()) end
local function checkvector(a: vector) assert(is_native()) end
local function checkbuffer(a: buffer) assert(is_native()) end
local function checkoptbuffer(a: buffer?) assert(is_native()) end

call(checktable, {})
ecall(checktable, 2)

call(checkfunction, function() end)
ecall(checkfunction, 2)

call(checkthread, coroutine.create(function() end))
ecall(checkthread, 2)

call(checkuserdata, newproxy())
ecall(checkuserdata, 2)

call(checkvector, vector.create(1, 2, 3))
ecall(checkvector, 2)

call(checkbuffer, buffer.create(10))
ecall(checkbuffer, 2)
call(checkoptbuffer, buffer.create(10))
call(checkoptbuffer, nil)
ecall(checkoptbuffer, 2)

local function mutation_causes_bad_exit(a: number, count: number, sum: number)
  repeat
    a = 's'
    sum += count
    pcall(function() end)
    count -= 1
  until count == 0
  return sum
end

assert(call(mutation_causes_bad_exit, 5, 10, 0) == 55)

return('OK')
