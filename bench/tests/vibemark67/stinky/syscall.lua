--!strict

-- Linux ARM64 syscall emulation.
-- Handles the syscalls needed by statically-linked glibc programs.

local Int = require("./integer")
local CPU = require("./cpu")

local Syscall = {}

-- Syscall state (mutable, shared across calls)
local brkAddr: integer = Int.ZERO
local mmapBase: integer = Int.fromHex("7000000000") -- high address for mmap allocations
local nextTid = 1000
local outputLine: (string) -> () = print

-- Initialize syscall state. Called once before running a program.
function Syscall.init(highAddr: integer, output: (string) -> ())
    outputLine = output
    -- Align brk up to page boundary
    local pageSize = Int.from(4096)
    local aligned = Int.band(Int.add(highAddr, Int.sub(pageSize, Int.ONE)), Int.bnot(Int.sub(pageSize, Int.ONE)))
    brkAddr = aligned
    mmapBase = Int.fromHex("7000000000")
    nextTid = 1000
end

function Syscall.handle(cpu: CPU.CPU)
    local sysno = Int.toNumber(cpu:readX(8))
    local x0 = cpu:readX(0)
    local x1 = cpu:readX(1)
    local x2 = cpu:readX(2)
    local x3 = cpu:readX(3)
    local x4 = cpu:readX(4)
    local x5 = cpu:readX(5)

    local result: integer

    if sysno == 64 then -- write
        result = Syscall.sysWrite(cpu, x0, x1, x2)
    elseif sysno == 63 then -- read
        result = Syscall.sysRead(cpu, x0, x1, x2)
    elseif sysno == 66 then -- writev
        result = Syscall.sysWritev(cpu, x0, x1, x2)
    elseif sysno == 93 then -- exit
        cpu.halted = true
        cpu.exitCode = Int.toNumber(Int.band(x0, Int.MASK32))
        -- Treat as signed
        if cpu.exitCode > 0x7FFFFFFF then
            cpu.exitCode = cpu.exitCode - 0x100000000
        end
        result = Int.ZERO
    elseif sysno == 94 then -- exit_group
        cpu.halted = true
        cpu.exitCode = Int.toNumber(Int.band(x0, Int.MASK32))
        if cpu.exitCode > 0x7FFFFFFF then
            cpu.exitCode = cpu.exitCode - 0x100000000
        end
        result = Int.ZERO
    elseif sysno == 214 then -- brk
        result = Syscall.sysBrk(cpu, x0)
    elseif sysno == 222 then -- mmap
        result = Syscall.sysMmap(cpu, x0, x1, x2, x3, x4, x5)
    elseif sysno == 215 then -- munmap
        result = Int.ZERO -- pretend success
    elseif sysno == 226 then -- mprotect
        result = Int.ZERO -- pretend success
    elseif sysno == 233 then -- madvise
        result = Int.ZERO -- pretend success
    elseif sysno == 216 then -- mremap
        -- Just allocate new memory at a new address
        local newSize = x2
        mmapBase = Int.add(mmapBase, Int.from(4096))
        local addr = mmapBase
        local size = Int.toNumber(newSize)
        mmapBase = Int.add(mmapBase, Int.add(newSize, Int.from(4095)))
        mmapBase = Int.band(mmapBase, Int.bnot(Int.from(4095)))
        cpu.mem:zeroFill(addr, size)
        result = addr
    elseif sysno == 134 then -- rt_sigaction
        result = Int.ZERO -- ignore signal setup
    elseif sysno == 135 then -- rt_sigprocmask
        result = Int.ZERO -- ignore signal mask
    elseif sysno == 96 then -- set_tid_address
        result = Int.from(nextTid)
        nextTid += 1
    elseif sysno == 293 then -- rseq
        result = Int.neg(Int.from(38)) -- -ENOSYS
    elseif sysno == 172 then -- getpid
        result = Int.from(1000)
    elseif sysno == 178 then -- gettid
        result = Int.from(1000)
    elseif sysno == 167 then -- prctl
        result = Int.ZERO -- pretend success
    elseif sysno == 98 then -- futex
        -- Single-threaded: FUTEX_WAIT returns 0, FUTEX_WAKE returns 0
        result = Int.ZERO
    elseif sysno == 131 then -- tgkill
        -- Sending signal to ourselves - ignore
        result = Int.ZERO
    elseif sysno == 113 then -- clock_gettime
        -- Write zeros (time = 0)
        cpu.mem:writeU64(x1, Int.ZERO) -- tv_sec
        cpu.mem:writeU64(Int.add(x1, Int.from(8)), Int.ZERO) -- tv_nsec
        result = Int.ZERO
    elseif sysno == 80 then -- fstat
        -- Return a minimal stat struct (not a terminal — to avoid complex glibc paths)
        cpu.mem:zeroFill(x1, 128)
        -- st_mode = S_IFREG | 0644 (regular file)
        cpu.mem:writeU32(Int.add(x1, Int.from(16)), 0x81A4)
        -- st_blksize = 4096
        cpu.mem:writeU64(Int.add(x1, Int.from(56)), Int.from(4096))
        result = Int.ZERO
    elseif sysno == 78 then -- readlinkat
        result = Int.neg(Int.from(22)) -- -EINVAL
    elseif sysno == 99 then -- set_robust_list
        result = Int.ZERO
    elseif sysno == 29 then -- ioctl
        result = Int.neg(Int.from(25)) -- -ENOTTY
    elseif sysno == 17 then -- getcwd
        -- Return "/"
        cpu.mem:writeU8(x0, 47) -- '/'
        cpu.mem:writeU8(Int.add(x0, Int.ONE), 0)
        result = Int.from(2)
    elseif sysno == 56 then -- openat
        result = Int.neg(Int.from(2)) -- -ENOENT
    elseif sysno == 79 then -- fstatat/newfstatat
        result = Int.neg(Int.from(2)) -- -ENOENT
    elseif sysno == 25 then -- fcntl
        result = Int.ZERO
    elseif sysno == 261 then -- prlimit64
        -- Return some reasonable defaults
        local resource = Int.toNumber(x1)
        if not Int.isZero(x3) then
            -- Writing old limits: just write large values
            cpu.mem:writeU64(x3, Int.fromHex("FFFFFFFFFFFFFFFF")) -- rlim_cur
            cpu.mem:writeU64(Int.add(x3, Int.from(8)), Int.fromHex("FFFFFFFFFFFFFFFF")) -- rlim_max
        end
        result = Int.ZERO
    elseif sysno == 179 then -- sysinfo
        -- Fill with zeros (minimal sysinfo)
        for off = 0, 111 do
            cpu.mem:writeU8(Int.add(x0, Int.from(off)), 0)
        end
        result = Int.ZERO
    elseif sysno == 122 then -- sched_setaffinity
        result = Int.ZERO
    elseif sysno == 123 then -- sched_getaffinity
        -- Write a single CPU in the mask
        local bufsize = Int.toNumber(x1)
        local buf = x2
        cpu.mem:zeroFill(buf, bufsize)
        cpu.mem:writeU8(buf, 1) -- CPU 0
        result = Int.from(bufsize)
    elseif sysno == 119 then -- sched_setscheduler
        result = Int.ZERO
    elseif sysno == 120 then -- sched_getscheduler
        result = Int.ZERO
    elseif sysno == 121 then -- sched_getparam
        cpu.mem:writeU32(x1, 0) -- sched_priority = 0
        result = Int.ZERO
    elseif sysno == 124 then -- sched_yield
        result = Int.ZERO
    elseif sysno == 125 then -- sched_get_priority_max
        result = Int.from(99)
    elseif sysno == 126 then -- sched_get_priority_min
        result = Int.ZERO
    elseif sysno == 160 then -- uname
        -- Write minimal uname struct
        local buf = x0
        cpu.mem:zeroFill(buf, 390)
        local function writeField(off: number, s: string)
            for idx = 1, #s do
                cpu.mem:writeU8(Int.add(buf, Int.from(off + idx - 1)), string.byte(s, idx))
            end
        end
        writeField(0, "Linux")
        writeField(65, "emulator")
        writeField(130, "6.1.0")
        writeField(195, "#1")
        writeField(260, "aarch64")
        result = Int.ZERO
    elseif sysno == 278 then -- getrandom
        -- Fill buffer with deterministic "random" data
        local buf = x0
        local count = Int.toNumber(x1)
        for off = 0, count - 1 do
            cpu.mem:writeU8(Int.add(buf, Int.from(off)), (off * 7 + 13) % 256)
        end
        result = x1
    else
        -- Unknown syscall: return -ENOSYS
        local pcNum = Int.toNumber(cpu.PC)
        -- Uncomment for debugging:
        -- print(string.format("WARN: unhandled syscall %d at PC=0x%x", sysno, pcNum))
        result = Int.neg(Int.from(38)) -- -ENOSYS
    end

    cpu:writeX(0, result)
end

function Syscall.sysWrite(cpu: CPU.CPU, fdI: integer, bufAddr: integer, count: integer): integer
    local fd = Int.toNumber(fdI)
    local n = Int.toNumber(count)
    if fd == 1 or fd == 2 then
        local data = cpu.mem:readString(bufAddr, n)
        Syscall._writeToConsole(data, fd)
        return count
    end
    return Int.neg(Int.from(9)) -- -EBADF
end

-- Output buffer for stdout/stderr to handle partial lines
local outputBuf: { [number]: string } = { [1] = "", [2] = "" }

function Syscall._writeToConsole(data: string, fd: number)
    outputBuf[fd] = outputBuf[fd] .. data
    while true do
        local nl = string.find(outputBuf[fd], "\n", 1, true)
        if not nl then break end
        local line = string.sub(outputBuf[fd], 1, nl - 1)
        outputLine(line)
        outputBuf[fd] = string.sub(outputBuf[fd], nl + 1)
    end
end

function Syscall.flush()
    for fd = 1, 2 do
        if #outputBuf[fd] > 0 then
            outputLine(outputBuf[fd])
            outputBuf[fd] = ""
        end
    end
end

function Syscall.sysRead(cpu: CPU.CPU, fdI: integer, bufAddr: integer, count: integer): integer
    -- No stdin support; return 0 (EOF)
    return Int.ZERO
end

function Syscall.sysWritev(cpu: CPU.CPU, fdI: integer, iovAddr: integer, iovcnt: integer): integer
    local fd = Int.toNumber(fdI)
    local cnt = Int.toNumber(iovcnt)
    local totalWritten = 0

    for idx = 0, cnt - 1 do
        local iovBase = Int.add(iovAddr, Int.from(idx * 16))
        local bufAddr = cpu.mem:readU64(iovBase)
        local bufLen = Int.toNumber(cpu.mem:readU64(Int.add(iovBase, Int.from(8))))
        if bufLen > 0 then
            local written = Syscall.sysWrite(cpu, fdI, bufAddr, Int.from(bufLen))
            totalWritten += Int.toNumber(written)
        end
    end

    return Int.from(totalWritten)
end

function Syscall.sysBrk(cpu: CPU.CPU, addr: integer): integer
    if Int.isZero(addr) then
        return brkAddr
    end
    if Int.uge(addr, brkAddr) then
        -- Expand: zero-fill new pages
        local oldBrk = brkAddr
        local newBrk = Int.band(Int.add(addr, Int.from(4095)), Int.bnot(Int.from(4095)))
        local diff = Int.toNumber(Int.sub(newBrk, oldBrk))
        if diff > 0 then
            cpu.mem:zeroFill(oldBrk, diff)
        end
        brkAddr = addr
    end
    -- Shrinking brk is allowed but we just update the pointer
    brkAddr = addr
    return brkAddr
end

function Syscall.sysMmap(cpu: CPU.CPU, addr: integer, length: integer, prot: integer, flags: integer, fd: integer, offset: integer): integer
    local size = Int.toNumber(length)
    -- Align size to page boundary
    size = math.ceil(size / 4096) * 4096

    local mapAddr: integer
    if not Int.isZero(addr) and Int.toNumber(Int.band(flags, Int.from(0x10))) ~= 0 then
        -- MAP_FIXED: use the requested address
        mapAddr = addr
    else
        -- Allocate from our mmap region
        mmapBase = Int.add(mmapBase, Int.from(4096)) -- gap between allocations
        mapAddr = mmapBase
        mmapBase = Int.add(mmapBase, Int.from(size))
    end

    -- Zero-fill the mapped region
    cpu.mem:zeroFill(mapAddr, size)

    return mapAddr
end

return Syscall
