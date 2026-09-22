type function checksubtype(subtype, supertype)
    if not subtype:issubtypeof(supertype) then
        error("Not a subtype!")
    end
    return subtype
end

local validnil: checksubtype<nil, string?>
local validstring: checksubtype<"Hello", string>
local invalidunion: checksubtype<string | vector | number, number>
local invalidboolean: checksubtype<boolean, number>
local invalidsingleton: checksubtype<false, nil>
