local function foo<T>(my_enum: true | T): T
    return my_enum :: T
end

local function bar<T>(my_enum: true & T): T
    return my_enum :: T
end

local var1 = foo(true)
local var2 = foo(false)

local var3 = bar(true)
local var4 = bar(false)

local _var1Type: _luau_print<typeof(var1)> = var1

local _var2Type: _luau_print<typeof(var2)> = var2

local _var3Type: _luau_print<typeof(var3)> = var3

local _var4Type: _luau_print<typeof(var4)> = var4
