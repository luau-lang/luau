local function foo<T>(my_enum: "" | T): T
    return my_enum :: T
end

local var = foo("meow")

local _varType: _luau_print<typeof(var)> = var
