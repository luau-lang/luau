-- export inside repeat/until should error
repeat
    export local inside = "repeat"
until true
