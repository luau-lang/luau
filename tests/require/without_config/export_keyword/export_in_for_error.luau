-- export inside loops should error
for i = 0, 10 do
    export local inside = i
end
