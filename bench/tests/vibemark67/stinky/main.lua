--!strict

-- ARM64 Linux Emulator - CLI entry point.
-- Usage: lute emu/main.luau <binary> [args...]

local Emu = require("./emu")

local process = require("@lute/process")
local fs = require("@lute/fs")

local args = process.args
if #args < 2 then
    print("Usage: lute emu/main.luau <binary> [args...]")
    process.exit(1)
end

local binaryPath = args[2]

local f = fs.open(binaryPath, "r")
local elfData = fs.read(f)
fs.close(f)

-- Build argv: program name + remaining args
local argv: { string } = {}
for idx = 2, #args do
    table.insert(argv, args[idx])
end

local exitCode = Emu.run(elfData, argv, print)

process.exit(exitCode)
