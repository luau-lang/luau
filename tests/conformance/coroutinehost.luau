-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print "testing coroutines with host finalizer API"

do
    local co = coroutine.create(function() return 123, "yay" end)

    local finalized = {}
    coroutine.finally(co, function(status, value, message)
        assert(coroutine.running() ~= co)
        assert(status == "finished")
        assert(value == 123)
        assert(message == "yay")
        table.insert(finalized, "first")
    end)
    coroutine.finally(co, function(status, value, message)
        assert(status == "finished")
        assert(value == 123)
        assert(message == "yay")
        table.insert(finalized, "second")
    end)

    local s, r1, r2 = hostresume(co)
    assert(s)
    assert(r1 == 123 and r2 == "yay")
    assert(finalized[1] == "second")
    assert(finalized[2] == "first")
end

do
    local co = coroutine.create(function() error("coroutine error") end)

    local finalized = false
    coroutine.finally(co, function(status, message)
        assert(status == "error")
        assert(string.find(message, "coroutine error"))
        finalized = true
    end)

    local s, r = hostresume(co)
    assert(not s)
    assert(string.find(r, "coroutine error"))
    assert(finalized)
end

do
    local count = 0
    local co = coroutine.create(function() coroutine.yield() end)

    coroutine.finally(co, function(status)
        assert(status == "finished")
        count += 1
    end)

    hostresume(co)
    assert(count == 0)

    local s, r = hostresume(co)
    assert(s)
    assert(r == nil)
    assert(count == 1)
end

do
    local co = coroutine.create(function() return 123 end)

    local ran_first, ran_second = false, false
    coroutine.finally(co, function()
        ran_first = true
    end)
    coroutine.finally(co, function()
        ran_second = true
        error("callback error")
    end)

    local s, r = hostresume(co)
    assert(not s)
    assert(string.find(r, "callback error"))
    assert(ran_second)
    assert(not ran_first)
end

do
    local co = coroutine.create(function()
        coroutine.yield("ignored")
        error("unreachable")
    end)

    local called = false
    coroutine.finally(co, function(status, ...)
        assert(status == "cancelled")
        assert(select("#", ...) == 0)
        assert(coroutine.status(co) == "dead")
        called = true
    end)

    hostresume(co)

    local s, r = hostcancel(co)
    assert(s)
    assert(r == nil)
    assert(called)
end

do
    local co = coroutine.create(function() error("unreachable") end)

    local called = false
    coroutine.finally(co, function(status, ...)
        assert(status == "cancelled")
        assert(select("#", ...) == 0)
        assert(coroutine.status(co) == "dead")
        called = true
    end)

    local s, r = hostcancel(co)
    assert(s)
    assert(r == nil)
    assert(called)
end

do
    local co = coroutine.create(function() end)

    local ran_first, ran_second = false, false
    coroutine.finally(co, function()
        ran_first = true
    end)
    coroutine.finally(co, function(status, ...)
        assert(status == "cancelled")
        assert(select("#", ...) == 0)
        ran_second = true
        error("cancel callback error")
    end)

    local s, r = hostcancel(co)
    assert(not s)
    assert(string.find(r, "cancel callback error"))
    assert(ran_second)
    assert(not ran_first)
end

-- finalizer function can only be called once
do
    local co = coroutine.create(function()
        return 2
    end)

    local checked
    coroutine.finally(co, function(status, value)
        local finalizer = debug.info(2, "f")
        local s, r = pcall(finalizer)
        assert(not s)
        assert(r == "finalizer function has already been called")
        checked = true
    end)

    local s, r = hostresume(co)
    assert(s)
    assert(r == 2)

    assert(checked)
end

return 'OK'
