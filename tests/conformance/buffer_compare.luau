-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing buffer.compare")

local function ecall(fn, ...)
  local ok, err = pcall(fn, ...)
  assert(not ok)
  return err:sub((err:find(": ") or -1) + 2, #err)
end

local function sgn(n: number): number
  return if n < 0 then -1 elseif n > 0 then 1 else 0
end

local function fill(bytes: { number }): buffer
  local b = buffer.create(#bytes)
  for i, v in bytes do
    buffer.writeu8(b, i - 1, v)
  end
  return b
end

local function equal_buffers()
  local a = fill({ 1, 2, 3, 4 })
  local b = fill({ 1, 2, 3, 4 })
  assert(buffer.compare(a, 0, b, 0) == 0)
  assert(buffer.compare(a, 0, b) == 0)
  assert(buffer.compare(a, 0, b, nil) == 0)
  assert(buffer.compare(a, 0, b, 0, nil) == 0)
end

local function empty_buffers()
  local empty = buffer.create(0)
  local data = fill({ 1 })
  assert(buffer.compare(empty, 0, buffer.create(0)) == 0)
  assert(sgn(buffer.compare(empty, 0, data)) == -1)
  assert(sgn(buffer.compare(data, 0, empty)) == 1)
end

local function unsigned_bytes()
  assert(sgn(buffer.compare(fill({ 0x80 }), 0, fill({ 0x7f }), 0)) == 1)
  assert(sgn(buffer.compare(fill({ 0x00 }), 0, fill({ 0xff }), 0)) == -1)
  assert(sgn(buffer.compare(fill({ 1, 0x00 }), 0, fill({ 1, 0xff }), 0, 2)) == -1)
end

local function explicit_count_ignores_the_tail()
  local a = fill({ 1, 2, 9 })
  local b = fill({ 1, 2, 3 })
  assert(buffer.compare(a, 0, b, 0, 2) == 0)
  assert(sgn(buffer.compare(a, 0, b, 0, 3)) == 1)
  assert(buffer.compare(a, 0, b, 0, 0) == 0)
end

local function default_count_orders_by_tail_length()
  local short = fill({ 1, 2 })
  local long = fill({ 1, 2, 3, 4 })
  assert(sgn(buffer.compare(short, 0, long, 0)) == -1)
  assert(sgn(buffer.compare(long, 0, short)) == 1)
  assert(buffer.compare(short, 0, long, 0, 2) == 0)
  assert(sgn(buffer.compare(fill({ 2 }), 0, fill({ 1, 9, 9 }))) == 1)
end

local function ranged_windows()
  local a = fill({ 0, 1, 2, 3 })
  local b = fill({ 9, 2, 3, 4 })
  assert(buffer.compare(a, 2, b, 1, 2) == 0)
  assert(sgn(buffer.compare(a, 1, b, 0, 1)) == -1)
  assert(buffer.compare(a, 4, b, 4, 0) == 0)
  assert(sgn(buffer.compare(a, 4, b, 0)) == -1)
end

local function same_buffer()
  local b = fill({ 1, 2, 1, 3 })
  assert(buffer.compare(b, 0, b, 2, 1) == 0)
  assert(sgn(buffer.compare(b, 1, b, 3, 1)) == -1)
  assert(buffer.compare(b, 0, b, 0) == 0)
end

local function out_of_bounds()
  local a = fill({ 1, 2 })
  local b = fill({ 1, 2 })
  assert(ecall(function()
      buffer.compare(a, -1, b, 0, 1)
  end) == "buffer access out of bounds")
  assert(ecall(function()
      buffer.compare(a, 0, b, -1, 1)
  end) == "buffer access out of bounds")
  assert(ecall(function()
      buffer.compare(a, 0, b, 0, -1)
  end) == "buffer access out of bounds")
  assert(ecall(function()
      buffer.compare(a, 0, b, 0, 3)
  end) == "buffer access out of bounds")
  assert(ecall(function()
      buffer.compare(a, 3, b, 0)
  end) == "buffer access out of bounds")
  assert(ecall(function()
      buffer.compare(a, 0, b, 3, 0)
  end) == "buffer access out of bounds")
end

local function wrong_types()
  local b = fill({ 1 })
  assert(ecall(function()
      buffer.compare(1, 0, b, 0)
  end) == "invalid argument #1 to 'compare' (buffer expected, got number)")
  assert(ecall(function()
      buffer.compare(b, 0, "x")
  end) == "invalid argument #3 to 'compare' (buffer expected, got string)")
end

equal_buffers()
empty_buffers()
unsigned_bytes()
explicit_count_ignores_the_tail()
default_count_orders_by_tail_length()
ranged_windows()
same_buffer()
out_of_bounds()
wrong_types()

return "OK"
