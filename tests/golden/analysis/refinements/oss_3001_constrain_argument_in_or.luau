const function constrain(value: true?): ()
end

const function _meow(arg)
    constrain(arg)
    
    return arg
end

-- Parameter 'arg' has been reduced to never. This function is not callable with any possible value.
const function _mrrp(arg)
    -- Parameter 'arg' is required to be a subtype of 'true' here.
    -- Parameter 'arg' is required to be a subtype of 'nil' here.
    constrain(arg or true)
    
    return arg
end

local _: _luau_print<typeof(_meow)>
local _: _luau_print<typeof(_mrrp)>