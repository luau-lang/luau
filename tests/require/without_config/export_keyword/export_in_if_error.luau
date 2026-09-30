-- export inside if/elseif/else blocks should error
if false then
    print("skip")
elseif false then
    print("skip")
else
    export local inside = "else block"
end
