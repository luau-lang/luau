type MyTypeA = {Value: true}
type MyTypeB = {Value: false}

local function Func(input: number) : (MyTypeA | MyTypeB)
    if input == 1 then
        return {Value = true}
    else
        return {Value = false}
    end
end
