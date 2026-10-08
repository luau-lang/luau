local table = {
    freeze = function(t)
        t.which = "early"
        return t
    end,
}

export local value = 42

local table = {
    freeze = function(t)
        t.which = "late"
        return t
    end,
}
