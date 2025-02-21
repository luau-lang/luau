-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing byte buffer library")

function call(fn, ...)
  local ok, res = pcall(fn, ...)
  assert(ok)
  return res
end

function ecall(fn, ...)
  local ok, err = pcall(fn, ...)
  assert(not ok)
  return err:sub((err:find(": ") or -1) + 2, #err)
end

local function simple_byte_reads()
  local b = buffer.create(1024)

  assert(buffer.len(b) == 1024)

  assert(buffer.readi8(b, 5) == 0)
  buffer.writei8(b, 10, 32)
  assert(buffer.readi8(b, 10) == 32)
  buffer.writei8(b, 15, 5)
  buffer.writei8(b, 14, 4)
  buffer.writei8(b, 13, 3)
  buffer.writei8(b, 12, 2)
  buffer.writei8(b, 11, 1)
  assert(buffer.readi8(b, 11) == 1)
  assert(buffer.readi8(b, 12) == 2)
  assert(buffer.readi8(b, 13) == 3)
  assert(buffer.readi8(b, 14) == 4)
  assert(buffer.readi8(b, 15) == 5)

  local x = buffer.readi8(b, 14) + buffer.readi8(b, 13)
  assert(x == 7)

  buffer.writei8(b, 16, x)
end

simple_byte_reads()

local function offset_byte_reads(start: number)
  local b = buffer.create(1024)

  buffer.writei8(b, start, 32)
  assert(buffer.readi8(b, start) == 32)
  buffer.writei8(b, start + 5, 5)
  buffer.writei8(b, start + 4, 4)
  buffer.writei8(b, start + 3, 3)
  buffer.writei8(b, start + 2, 2)
  buffer.writei8(b, start + 1, 1)
  assert(buffer.readi8(b, start + 1) == 1)
  assert(buffer.readi8(b, start + 2) == 2)
  assert(buffer.readi8(b, start + 3) == 3)
  assert(buffer.readi8(b, start + 4) == 4)
  assert(buffer.readi8(b, start + 5) == 5)

  local x = buffer.readi8(b, start + 4) + buffer.readi8(b, start + 3)
  assert(x == 7)
end

offset_byte_reads(5)
offset_byte_reads(30)

local function simple_float_reinterpret()
  local b = buffer.create(1024)

  buffer.writei32(b, 10, 0x3f800000)
  local one = buffer.readf32(b, 10)
  assert(one == 1.0)

  buffer.writef32(b, 10, 2.75197)
  local magic = buffer.readi32(b, 10)
  assert(magic == 0x40302047)

  buffer.writef32(b, 10, one)
  local magic2 = buffer.readi32(b, 10)

  assert(magic2 == 0x3f800000)
end

simple_float_reinterpret()

local function simple_double_reinterpret()
  local b = buffer.create(1024)

  buffer.writei32(b, 10, 0x00000000)
  buffer.writei32(b, 14, 0x3ff00000)
  local one = buffer.readf64(b, 10)
  assert(one == 1.0)

  buffer.writef64(b, 10, 1.437576533064206)
  local magic1 = buffer.readi32(b, 10)
  local magic2 = buffer.readi32(b, 14)

  assert(magic1 == 0x40302010)
  assert(magic2 == 0x3ff70050)

  buffer.writef64(b, 10, one)
  local magic3 = buffer.readi32(b, 10)
  local magic4 = buffer.readi32(b, 14)

  assert(magic3 == 0x00000000)
  assert(magic4 == 0x3ff00000)
end

simple_double_reinterpret()

local function simple_string_ops()
  local b = buffer.create(1024)

  buffer.writestring(b, 15, " world")
  buffer.writestring(b, 10, "hello")
  buffer.writei8(b, 21, string.byte('!'))
  assert(buffer.readstring(b, 10, 12) == "hello world!")

  buffer.writestring(b, 10, "hellommm", 5)
  assert(buffer.readstring(b, 10, 12) == "hello world!")

  buffer.writestring(b, 10, string.rep("hellommm", 1000), 5)
  assert(buffer.readstring(b, 10, 12) == "hello world!")
end

simple_string_ops()

local function simple_copy_ops()
  local b1 = buffer.create(1024)
  local b2 = buffer.create(1024)

  buffer.writestring(b1, 200, "hello")
  buffer.writestring(b1, 100, "world")

  buffer.copy(b1, 300, b1, 100, 5)

  buffer.writei8(b2, 35, string.byte(' '))
  buffer.writei8(b2, 41, string.byte('!'))

  buffer.copy(b2, 30, b1, 200, 5)
  buffer.copy(b2, 36, b1, 300, 5)

  assert(buffer.readstring(b2, 30, 12) == "hello world!")

  local b3 = buffer.create(9)
  buffer.writestring(b3, 0, "say hello")
  buffer.copy(b2, 36, b3, 4)
  assert(buffer.readstring(b2, 30, 12) == "hello hello!")

  local b4 = buffer.create(5)
  buffer.writestring(b4, 0, "world")
  buffer.copy(b2, 36, b4)
  assert(buffer.readstring(b2, 30, 12) == "hello world!")

  buffer.writestring(b1, 200, "abcdefgh");
  buffer.copy(b1, 200, b1, 202, 6)
  assert(buffer.readstring(b1, 200, 8) == "cdefghgh")
  buffer.copy(b1, 202, b1, 200, 6)
  assert(buffer.readstring(b1, 200, 8) == "cdcdefgh")
end

simple_copy_ops()

-- bounds checking

local function createchecks()
  assert(ecall(function() buffer.create(-1) end) == "invalid argument #1 to 'create' (size)")
  assert(ecall(function() buffer.create(-1000000) end) == "invalid argument #1 to 'create' (size)")
end

createchecks()

local function boundchecks()
  local b = buffer.create(1024)

  assert(call(function() return buffer.readi8(b, 1023) end) == 0)
  assert(ecall(function() buffer.readi8(b, 1024) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi8(b, -1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi8(b, -100000) end) == "buffer access out of bounds")

  call(function() buffer.writei8(b, 1023, 0) end)
  assert(ecall(function() buffer.writei8(b, 1024, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei8(b, -1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei8(b, -100000, 0) end) == "buffer access out of bounds")

  -- i16
  assert(call(function() return buffer.readi16(b, 1022) end) == 0)
  assert(ecall(function() buffer.readi16(b, 1023) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, -1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, -100000) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, 0x7fffffff) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, 0x7ffffffe) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, 0x7ffffffd) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, 0x80000000) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, 0x0fffffff) end) == "buffer access out of bounds")

  call(function() buffer.writei16(b, 1022, 0) end)
  assert(ecall(function() buffer.writei16(b, 1023, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei16(b, -1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei16(b, -100000, 0) end) == "buffer access out of bounds")

  -- i32
  assert(call(function() return buffer.readi32(b, 1020) end) == 0)
  assert(ecall(function() buffer.readi32(b, 1021) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi32(b, -1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi32(b, -100000) end) == "buffer access out of bounds")

  call(function() buffer.writei32(b, 1020, 0) end)
  assert(ecall(function() buffer.writei32(b, 1021, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei32(b, -1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei32(b, -100000, 0) end) == "buffer access out of bounds")

  -- f32
  assert(call(function() return buffer.readf32(b, 1020) end) == 0)
  assert(ecall(function() buffer.readf32(b, 1021) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf32(b, -1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf32(b, -100000) end) == "buffer access out of bounds")

  call(function() buffer.writef32(b, 1020, 0) end)
  assert(ecall(function() buffer.writef32(b, 1021, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef32(b, -1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef32(b, -100000, 0) end) == "buffer access out of bounds")

  -- f64
  assert(call(function() return buffer.readf64(b, 1016) end) == 0)
  assert(ecall(function() buffer.readf64(b, 1017) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf64(b, -1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf64(b, -100000) end) == "buffer access out of bounds")

  call(function() buffer.writef64(b, 1016, 0) end)
  assert(ecall(function() buffer.writef64(b, 1017, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef64(b, -1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef64(b, -100000, 0) end) == "buffer access out of bounds")

  -- string
  assert(call(function() return buffer.readstring(b, 1016, 8) end) == "\0\0\0\0\0\0\0\0")
  assert(ecall(function() buffer.readstring(b, 1017, 8) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readstring(b, -1, -8) end) == "invalid argument #3 to 'readstring' (size)")
  assert(ecall(function() buffer.readstring(b, -100000, 8) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readstring(b, -100000, 8) end) == "buffer access out of bounds")

  call(function() buffer.writestring(b, 1016, "abcdefgh") end)
  assert(ecall(function() buffer.writestring(b, 1017, "abcdefgh") end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writestring(b, -1, "abcdefgh") end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writestring(b, -100000, "abcdefgh") end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writestring(b, 100, "abcd", -5) end) == "invalid argument #4 to 'writestring' (count)")
  assert(ecall(function() buffer.writestring(b, 100, "abcd", 50) end) == "string length overflow")

  -- copy
  assert(ecall(function() buffer.copy(b, 30, b, 200, 1000) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.copy(b, 30, b, 200, -5) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.copy(b, 30, b, 2000, 10) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.copy(b, 30, b, -1, 10) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.copy(b, 30, b, -10, 10) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.copy(b, 30, b, -100000, 10) end) == "buffer access out of bounds")

  local b2 = buffer.create(1024)
  assert(ecall(function() buffer.copy(b, -200, b, 200, 200) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.copy(b, 825, b, 200, 200) end) == "buffer access out of bounds")
end

boundchecks()

local function boundchecksnonconst(size, minus1, minusbig, intmax)
  local b = buffer.create(size)

  assert(call(function() return buffer.readi8(b, size-1) end) == 0)
  assert(ecall(function() buffer.readi8(b, size) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi8(b, minus1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi8(b, minusbig) end) == "buffer access out of bounds")

  call(function() buffer.writei8(b, size-1, 0) end)
  assert(ecall(function() buffer.writei8(b, size, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei8(b, minus1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei8(b, minusbig, 0) end) == "buffer access out of bounds")

  -- i16
  assert(call(function() return buffer.readi16(b, size-2) end) == 0)
  assert(ecall(function() buffer.readi16(b, size-1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, minus1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, minusbig) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, intmax) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, intmax-1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, intmax-2) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, intmax+1) end) == "buffer access out of bounds")

  call(function() buffer.writei16(b, size-2, 0) end)
  assert(ecall(function() buffer.writei16(b, size-1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei16(b, minus1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei16(b, minusbig, 0) end) == "buffer access out of bounds")

  -- i32
  assert(call(function() return buffer.readi32(b, size-4) end) == 0)
  assert(ecall(function() buffer.readi32(b, size-3) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi32(b, minus1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi32(b, minusbig) end) == "buffer access out of bounds")

  call(function() buffer.writei32(b, size-4, 0) end)
  assert(ecall(function() buffer.writei32(b, size-3, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei32(b, minus1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei32(b, minusbig, 0) end) == "buffer access out of bounds")

  -- f32
  assert(call(function() return buffer.readf32(b, size-4) end) == 0)
  assert(ecall(function() buffer.readf32(b, size-3) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf32(b, minus1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf32(b, minusbig) end) == "buffer access out of bounds")

  call(function() buffer.writef32(b, size-4, 0) end)
  assert(ecall(function() buffer.writef32(b, size-3, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef32(b, minus1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef32(b, minusbig, 0) end) == "buffer access out of bounds")

  -- f64
  assert(call(function() return buffer.readf64(b, size-8) end) == 0)
  assert(ecall(function() buffer.readf64(b, size-7) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf64(b, minus1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf64(b, minusbig) end) == "buffer access out of bounds")

  call(function() buffer.writef64(b, size-8, 0) end)
  assert(ecall(function() buffer.writef64(b, size-7, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef64(b, minus1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef64(b, minusbig, 0) end) == "buffer access out of bounds")

  -- string
  assert(call(function() return buffer.readstring(b, size-8, 8) end) == "\0\0\0\0\0\0\0\0")
  assert(ecall(function() buffer.readstring(b, size-7, 8) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readstring(b, minus1, 8) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readstring(b, minusbig, 8) end) == "buffer access out of bounds")

  call(function() buffer.writestring(b, size-8, "abcdefgh") end)
  assert(ecall(function() buffer.writestring(b, size-7, "abcdefgh") end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writestring(b, minus1, "abcdefgh") end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writestring(b, minusbig, "abcdefgh") end) == "buffer access out of bounds")
end

boundchecksnonconst(1024, -1, -100000, 0x7fffffff)

local function boundcheckssmall()
  local b = buffer.create(1)

  assert(call(function() return buffer.readi8(b, 0) end) == 0)
  assert(ecall(function() buffer.readi8(b, 1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi8(b, -1) end) == "buffer access out of bounds")

  call(function() buffer.writei8(b, 0, 0) end)
  assert(ecall(function() buffer.writei8(b, 1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei8(b, -1, 0) end) == "buffer access out of bounds")

  -- i16
  assert(ecall(function() buffer.readi16(b, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, -1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, -2) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei16(b, 0, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei16(b, -1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei16(b, -2, 0) end) == "buffer access out of bounds")

  -- i32
  assert(ecall(function() buffer.readi32(b, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi32(b, -1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi32(b, -4) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei32(b, 0, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei32(b, -1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei32(b, -4, 0) end) == "buffer access out of bounds")

  -- f32
  assert(ecall(function() buffer.readf32(b, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf32(b, -1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf32(b, -4) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef32(b, 0, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef32(b, -1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef32(b, -4, 0) end) == "buffer access out of bounds")

  -- f64
  assert(ecall(function() buffer.readf64(b, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf64(b, -1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf64(b, -8) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef64(b, 0, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef64(b, -1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef64(b, -7, 0) end) == "buffer access out of bounds")

  -- string
  assert(ecall(function() buffer.readstring(b, 0, 8) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readstring(b, -1, 8) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readstring(b, -8, 8) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writestring(b, 0, "abcdefgh") end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writestring(b, -1, "abcdefgh") end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writestring(b, -7, "abcdefgh") end) == "buffer access out of bounds")
end

boundcheckssmall()

local function boundcheckssmallnonconst(zero, one, minus1, minus2, minus4, minus7, minus8)
  local b = buffer.create(1)

  assert(call(function() return buffer.readi8(b, 0) end) == 0)
  assert(ecall(function() buffer.readi8(b, one) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi8(b, minus1) end) == "buffer access out of bounds")

  call(function() buffer.writei8(b, 0, 0) end)
  assert(ecall(function() buffer.writei8(b, one, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei8(b, minus1, 0) end) == "buffer access out of bounds")

  -- i16
  assert(ecall(function() buffer.readi16(b, zero) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, minus1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi16(b, minus2) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei16(b, zero, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei16(b, minus1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei16(b, minus2, 0) end) == "buffer access out of bounds")

  -- i32
  assert(ecall(function() buffer.readi32(b, zero) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi32(b, minus1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi32(b, minus4) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei32(b, zero, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei32(b, minus1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei32(b, minus4, 0) end) == "buffer access out of bounds")

  -- f32
  assert(ecall(function() buffer.readf32(b, zero) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf32(b, minus1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf32(b, minus4) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef32(b, zero, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef32(b, minus1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef32(b, minus4, 0) end) == "buffer access out of bounds")

  -- f64
  assert(ecall(function() buffer.readf64(b, zero) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf64(b, minus1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf64(b, minus8) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef64(b, zero, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef64(b, minus1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writef64(b, minus7, 0) end) == "buffer access out of bounds")

  -- string
  assert(ecall(function() buffer.readstring(b, zero, 8) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readstring(b, minus1, 8) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readstring(b, minus8, 8) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writestring(b, zero, "abcdefgh") end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writestring(b, minus1, "abcdefgh") end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writestring(b, minus7, "abcdefgh") end) == "buffer access out of bounds")
end

boundcheckssmallnonconst(0, 1, -1, -2, -4, -7, -8)

local function boundchecksempty()
  local b = buffer.create(0) -- useless, but probably more generic

  assert(ecall(function() buffer.readi8(b, 1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi8(b, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi8(b, -1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei8(b, 1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei8(b, 0, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writei8(b, -1, 0) end) == "buffer access out of bounds")

  assert(ecall(function() buffer.readi16(b, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readi32(b, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf32(b, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readf64(b, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readstring(b, 0, 1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readstring(b, 0, 8) end) == "buffer access out of bounds")
end

boundchecksempty()

local function intuint()
  local b = buffer.create(32)

  buffer.writeu32(b, 0, 0xffffffff)
  assert(buffer.readi8(b, 0) == -1)
  assert(buffer.readu8(b, 0) == 255)
  assert(buffer.readi16(b, 0) == -1)
  assert(buffer.readu16(b, 0) == 65535)
  assert(buffer.readi32(b, 0) == -1)
  assert(buffer.readu32(b, 0) == 4294967295)

  buffer.writei32(b, 0, -1)
  assert(buffer.readi8(b, 0) == -1)
  assert(buffer.readu8(b, 0) == 255)
  assert(buffer.readi16(b, 0) == -1)
  assert(buffer.readu16(b, 0) == 65535)
  assert(buffer.readi32(b, 0) == -1)
  assert(buffer.readu32(b, 0) == 4294967295)

  buffer.writei16(b, 0, 65535)
  buffer.writei16(b, 2, -1)
  assert(buffer.readi8(b, 0) == -1)
  assert(buffer.readu8(b, 0) == 255)
  assert(buffer.readi16(b, 0) == -1)
  assert(buffer.readu16(b, 0) == 65535)
  assert(buffer.readi32(b, 0) == -1)
  assert(buffer.readu32(b, 0) == 4294967295)

  buffer.writeu16(b, 0, 65535)
  buffer.writeu16(b, 2, -1)
  assert(buffer.readi8(b, 0) == -1)
  assert(buffer.readu8(b, 0) == 255)
  assert(buffer.readi16(b, 0) == -1)
  assert(buffer.readu16(b, 0) == 65535)
  assert(buffer.readi32(b, 0) == -1)
  assert(buffer.readu32(b, 0) == 4294967295)
end

intuint()

local function intuinttricky()
  local b = buffer.create(32)

  buffer.writeu8(b, 0, 0xffffffff)
  assert(buffer.readi8(b, 0) == -1)
  assert(buffer.readu8(b, 0) == 255)
  assert(buffer.readi16(b, 0) == 255)
  assert(buffer.readu16(b, 0) == 255)
  assert(buffer.readi32(b, 0) == 255)
  assert(buffer.readu32(b, 0) == 255)

  buffer.writeu16(b, 0, 0xffffffff)
  assert(buffer.readi8(b, 0) == -1)
  assert(buffer.readu8(b, 0) == 255)
  assert(buffer.readi16(b, 0) == -1)
  assert(buffer.readu16(b, 0) == 65535)
  assert(buffer.readi32(b, 0) == 65535)
  assert(buffer.readu32(b, 0) == 65535)

  buffer.writei32(b, 8, 0xffffffff)
  buffer.writeu32(b, 12, 0xffffffff)
  assert(buffer.readstring(b, 8, 4) == buffer.readstring(b, 12, 4))

  buffer.writei32(b, 8, -2147483648)
  buffer.writeu32(b, 12, 0x80000000)
  assert(buffer.readstring(b, 8, 4) == buffer.readstring(b, 12, 4))
end

intuinttricky()

local function fromtostring()
  local b = buffer.fromstring("1234567890")
  assert(buffer.tostring(b) == "1234567890")

  buffer.writestring(b, 4, "xyz")
  assert(buffer.tostring(b) == "1234xyz890")

  local b2 = buffer.fromstring("abcd\0ef")
  assert(buffer.tostring(b2) == "abcd\0ef")
end

fromtostring()

local function fill()
  local b = buffer.create(10)

  buffer.fill(b, 0, 0x61)
  assert(buffer.tostring(b) == "aaaaaaaaaa")

  buffer.fill(b, 0, 0x62, 5)
  assert(buffer.tostring(b) == "bbbbbaaaaa")

  buffer.fill(b, 4, 0x63)
  assert(buffer.tostring(b) == "bbbbcccccc")

  buffer.fill(b, 6, 0x64, 3)
  assert(buffer.tostring(b) == "bbbbccdddc")

  buffer.fill(b, 2, 0xffffff65, 8)
  assert(buffer.tostring(b) == "bbeeeeeeee")

  -- out of bounds
  assert(ecall(function() buffer.fill(b, -10, 1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.fill(b, 11, 1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.fill(b, 0, 1, 11) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.fill(b, 5, 1, 6) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.fill(b, 5, 1, -1) end) == "buffer access out of bounds")
end

fill()

local function misc(t16)
  local b = buffer.create(1000)

  assert(select('#', buffer.writei32(b, 10, 40)) == 0)
  assert(select('#', buffer.writef32(b, 20, 40.0)) == 0)

  -- some extra operation to place '#t16' into a linear block
  t16[1] = 10
  t16[15] = 20

  buffer.writei32(b, #t16, 10)
  assert(buffer.readi32(b, 16) == 10)

  buffer.writeu8(b, 100, 0xff)
  buffer.writeu8(b, 110, 0x80)
  assert(buffer.readu32(b, 100) == 255)
  assert(buffer.readu32(b, 110) == 128)
  buffer.writeu16(b, 200, 0xffff)
  buffer.writeu16(b, 210, 0x8000)
  assert(buffer.readu32(b, 200) == 65535)
  assert(buffer.readu32(b, 210) == 32768)
end

misc(table.create(16, 0))

local function bitops(size, base)
  local b = buffer.create(size)

  buffer.writeu32(b, base / 8, 0x12345678)

  assert(buffer.readbits(b, base, 8) == buffer.readu8(b, base / 8))
  assert(buffer.readbits(b, base, 16) == buffer.readu16(b, base / 8))
  assert(buffer.readbits(b, base, 32) == buffer.readu32(b, base / 8))

  buffer.writebits(b, base, 32, 0)

  buffer.writebits(b, base, 1, 1)
  assert(buffer.readi8(b, base / 8) == 1)

  buffer.writebits(b, base + 1, 1, 1)
  assert(buffer.readi8(b, base / 8) == 3)

  -- construct 00000010 00000000_01000000_00010000_00001000 00001000_00010000_01000010_00100101
  buffer.writebits(b, base + 0, 1, 0b1)
  buffer.writebits(b, base + 1, 2, 0b10)
  buffer.writebits(b, base + 3, 3, 0b100)
  buffer.writebits(b, base + 6, 4, 0b1000)
  buffer.writebits(b, base + 10, 5, 0b10000)
  buffer.writebits(b, base + 15, 6, 0b100000)
  buffer.writebits(b, base + 21, 7, 0b1000000)
  buffer.writebits(b, base + 28, 8, 0b10000000)
  buffer.writebits(b, base + 36, 9, 0b100000000)
  buffer.writebits(b, base + 45, 10, 0b1000000000)
  buffer.writebits(b, base + 55, 11, 0b10000000000)

  assert(buffer.readbits(b, base + 0, 32) == 0b00001000_00010000_01000010_00100101)
  assert(buffer.readbits(b, base + 32, 32) == 0b00000000_01000000_00010000_00001000)
  
  assert(buffer.readu32(b, base / 8 + 0) == 0b00001000_00010000_01000010_00100101)
  assert(buffer.readu32(b, base / 8 + 4) == 0b00000000_01000000_00010000_00001000)

  -- slide the window to touch 5 bytes
  assert(buffer.readbits(b, base + 1, 32) == 0b00000100000010000010000100010010)
  assert(buffer.readbits(b, base + 2, 32) == 0b00000010000001000001000010001001)
  assert(buffer.readbits(b, base + 3, 32) == 0b00000001000000100000100001000100)
  assert(buffer.readbits(b, base + 4, 32) == 0b10000000100000010000010000100010)
  assert(buffer.readbits(b, base + 5, 32) == 0b01000000010000001000001000010001)
  assert(buffer.readbits(b, base + 6, 32) == 0b00100000001000000100000100001000)
  assert(buffer.readbits(b, base + 7, 32) == 0b00010000000100000010000010000100)
  assert(buffer.readbits(b, base + 8, 32) == 0b00001000000010000001000001000010)
  
  assert(buffer.readbits(b, base + 1, 15) == 0b010000100010010)
  assert(buffer.readbits(b, base + 2, 15) == 0b001000010001001)
  assert(buffer.readbits(b, base + 3, 15) == 0b000100001000100)
  assert(buffer.readbits(b, base + 4, 15) == 0b000010000100010)
  assert(buffer.readbits(b, base + 5, 15) == 0b000001000010001)
  assert(buffer.readbits(b, base + 6, 15) == 0b100000100001000)
  assert(buffer.readbits(b, base + 7, 15) == 0b010000010000100)
  assert(buffer.readbits(b, base + 8, 15) == 0b001000001000010)
  
  -- zero bit
  buffer.writebits(b, base, 0, 0b1)
  assert(buffer.readbits(b, base, 32) == 0b00001000_00010000_01000010_00100101)
  assert(buffer.readbits(b, base, 0) == 0)
  assert(buffer.readbits(b, size * 8, 0) == 0)

  -- bounds
  assert(ecall(function() buffer.readbits(b, -1, 0) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readbits(b, size * 8, 1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readbits(b, size * 8 - 1, 2) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.readbits(b, 0, 64) end) == "bit count is out of range of [0; 32]")

  assert(ecall(function() buffer.writebits(b, -1, 0, 1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writebits(b, size * 8, 1, 1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writebits(b, size * 8 - 1, 2, 1) end) == "buffer access out of bounds")
  assert(ecall(function() buffer.writebits(b, 0, 64, 1) end) == "bit count is out of range of [0; 32]")


  return b
end

do
  bitops(16, 0)
  bitops(17, 8)

  -- a very large buffer and bit offsets can now be over 32 bits
  bitops(1024 * 1024 * 1024, 6 * 1024 * 1024 * 1024)
end

local function testslowcalls()
  getfenv()

  simple_byte_reads()
  offset_byte_reads(5)
  offset_byte_reads(30)
  simple_float_reinterpret()
  simple_double_reinterpret()
  simple_string_ops()
  createchecks()
  boundchecks()
  boundchecksnonconst(1024, -1, -100000, 0x7fffffff)
  boundcheckssmall()
  boundcheckssmallnonconst(0, 1, -1, -2, -4, -7, -8)
  boundchecksempty()
  intuint()
  intuinttricky()
  fromtostring()
  fill()
  misc(table.create(16, 0))
  bitops(16, 0)
end

testslowcalls()

return('OK')
