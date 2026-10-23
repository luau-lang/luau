--!strict

-- Sparse page-table memory model.
-- Memory is organized as 4KB pages, allocated on demand.
-- Supports reading/writing 1/2/4/8-byte values at arbitrary addresses.

local Int = require("./integer")

local PAGE_BITS = 12
local PAGE_SIZE = 4096

local Memory = {}
Memory.__index = Memory

export type Memory = typeof(setmetatable({} :: {
    pages: { [number]: buffer },
}, Memory))

function Memory.new(): Memory
    local self = setmetatable({
        pages = {},
    }, Memory)
    return self
end

function Memory.pageOf(addr: integer): number
    return Int.toNumber(Int.shr(addr, PAGE_BITS))
end

function Memory.offsetOf(addr: integer): number
    return Int.toNumber(Int.band(addr, Int.from(PAGE_SIZE - 1)))
end

function Memory.ensurePage(self: Memory, pageNum: number): buffer
    local page = self.pages[pageNum]
    if not page then
        page = buffer.create(PAGE_SIZE)
        self.pages[pageNum] = page
    end
    return page
end

function Memory.getPage(self: Memory, pageNum: number): buffer?
    return self.pages[pageNum]
end

-- Load a chunk of data (as a string) into memory starting at `addr`.
function Memory.loadString(self: Memory, addr: integer, data: string)
    local len = #data
    local pos = 0
    while pos < len do
        local pageNum = Memory.pageOf(Int.add(addr, Int.from(pos)))
        local offset = Memory.offsetOf(Int.add(addr, Int.from(pos)))
        local page = self:ensurePage(pageNum)
        local bytesThisPage = math.min(PAGE_SIZE - offset, len - pos)
        local chunk = string.sub(data, pos + 1, pos + bytesThisPage)
        buffer.writestring(page, offset, chunk)
        pos += bytesThisPage
    end
end

-- Zero-fill memory from addr for `size` bytes (for BSS segments).
function Memory.zeroFill(self: Memory, addr: integer, size: number)
    local pos = 0
    while pos < size do
        local pageNum = Memory.pageOf(Int.add(addr, Int.from(pos)))
        local offset = Memory.offsetOf(Int.add(addr, Int.from(pos)))
        local page = self:ensurePage(pageNum)
        local bytesThisPage = math.min(PAGE_SIZE - offset, size - pos)
        buffer.fill(page, offset, 0, bytesThisPage)
        pos += bytesThisPage
    end
end

function Memory.readU8(self: Memory, addr: integer): number
    local pageNum = Memory.pageOf(addr)
    local offset = Memory.offsetOf(addr)
    local page = self:ensurePage(pageNum)
    return buffer.readu8(page, offset)
end

function Memory.readU16(self: Memory, addr: integer): number
    local offset = Memory.offsetOf(addr)
    if offset <= PAGE_SIZE - 2 then
        local page = self:ensurePage(Memory.pageOf(addr))
        return buffer.readu16(page, offset)
    end
    -- Crosses page boundary
    local b0 = self:readU8(addr)
    local b1 = self:readU8(Int.add(addr, Int.ONE))
    return b0 + b1 * 256
end

function Memory.readU32(self: Memory, addr: integer): number
    local offset = Memory.offsetOf(addr)
    if offset <= PAGE_SIZE - 4 then
        local page = self:ensurePage(Memory.pageOf(addr))
        return buffer.readu32(page, offset)
    end
    -- Crosses page boundary
    local b0 = self:readU16(addr)
    local b1 = self:readU16(Int.add(addr, Int.from(2)))
    return b0 + b1 * 65536
end

function Memory.readU64(self: Memory, addr: integer): integer
    local offset = Memory.offsetOf(addr)
    if offset <= PAGE_SIZE - 8 then
        local page = self:ensurePage(Memory.pageOf(addr))
        return buffer.readinteger(page, offset, 8)
    end
    -- Crosses page boundary
    local lo = Int.from(self:readU32(addr))
    local hi = Int.from(self:readU32(Int.add(addr, Int.from(4))))
    return Int.bor(lo, Int.shl(hi, 32))
end

function Memory.writeU8(self: Memory, addr: integer, val: number)
    local pageNum = Memory.pageOf(addr)
    local offset = Memory.offsetOf(addr)
    local page = self:ensurePage(pageNum)
    buffer.writeu8(page, offset, bit32.band(val, 0xFF))
end

function Memory.writeU16(self: Memory, addr: integer, val: number)
    local offset = Memory.offsetOf(addr)
    if offset <= PAGE_SIZE - 2 then
        local page = self:ensurePage(Memory.pageOf(addr))
        buffer.writeu16(page, offset, bit32.band(val, 0xFFFF))
        return
    end
    self:writeU8(addr, bit32.band(val, 0xFF))
    self:writeU8(Int.add(addr, Int.ONE), bit32.band(bit32.rshift(val, 8), 0xFF))
end

function Memory.writeU32(self: Memory, addr: integer, val: number)
    local offset = Memory.offsetOf(addr)
    if offset <= PAGE_SIZE - 4 then
        local page = self:ensurePage(Memory.pageOf(addr))
        buffer.writeu32(page, offset, val)
        return
    end
    self:writeU16(addr, bit32.band(val, 0xFFFF))
    self:writeU16(Int.add(addr, Int.from(2)), bit32.band(bit32.rshift(val, 16), 0xFFFF))
end

function Memory.writeU64(self: Memory, addr: integer, val: integer)
    local offset = Memory.offsetOf(addr)
    if offset <= PAGE_SIZE - 8 then
        local page = self:ensurePage(Memory.pageOf(addr))
        buffer.writeinteger(page, offset, val, 8)
        return
    end
    local lo = Int.toNumber(Int.band(val, Int.MASK32))
    local hi = Int.toNumber(Int.band(Int.shr(val, 32), Int.MASK32))
    self:writeU32(addr, lo)
    self:writeU32(Int.add(addr, Int.from(4)), hi)
end

-- Read `n` bytes as a string starting at addr.
function Memory.readString(self: Memory, addr: integer, n: number): string
    local parts = {}
    local pos = 0
    while pos < n do
        local pageNum = Memory.pageOf(Int.add(addr, Int.from(pos)))
        local offset = Memory.offsetOf(Int.add(addr, Int.from(pos)))
        local page = self:ensurePage(pageNum)
        local bytesThisPage = math.min(PAGE_SIZE - offset, n - pos)
        table.insert(parts, buffer.readstring(page, offset, bytesThisPage))
        pos += bytesThisPage
    end
    return table.concat(parts)
end

-- Write a string into memory starting at addr.
function Memory.writeString(self: Memory, addr: integer, s: string)
    self:loadString(addr, s)
end

return Memory
