type Result = { type: "ok", value: unknown } | { type: "error" }

local function coinflip(): boolean return true end

local function readFromDB(): Result
    return if coinflip() then { type = "ok", value = 42 } else { type = "error" }
end
