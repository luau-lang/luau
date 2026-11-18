type Cat = {tag: "Cat", meows: boolean}
type Dog = {tag: "Dog", barks: boolean}
type Animal = Cat | Dog

local function f(tag: "Cat" | "Dog"): Animal?
    if tag == "Cat" then
        local result = {tag = tag, meows = true}
        return result
    elseif tag == "Dog" then
        local result = {tag = tag, barks = true}
        return result
    else
        return nil
    end
end
