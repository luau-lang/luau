local function __remove(__: number?) end

function __removeItem(self, itemId: number)
    local index = self.getItem(itemId)
    if index then
       __remove(index)
    end
end

local _: _luau_print<typeof(__removeItem)>