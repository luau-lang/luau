-- exported bindings should follow normal local shadowing rules
local function foo()
    return "local"
end

export function foo()
    return "exported"
end

local fruit = "apple"
export local fruit

export local animal = "dog"
local animal = "cat"
animal = "bird"
