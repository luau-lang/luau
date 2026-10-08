-- mutually recursive functions with exported forward declarations
export local a, b

function a()
    return b() + 1
end

function b()
    return 42
end
