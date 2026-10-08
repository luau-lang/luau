-- export inside elseif block should error
if false then
    print("skip")
elseif true then
    export local inside = "elseif block"
end
