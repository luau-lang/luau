type Ok<T> = {success: true, result: T}
type Err<T> = {success: false, error: T}
type Result<O, E> = Ok<O> | Err<E>

local a : Result<string, number> = {success = false, result = "hotdogs"}
