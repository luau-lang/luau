--!golden fflags=LuauDoNotCrashOnRecursiveSetType
x = if x then x elseif x then x else x
type T<A> = typeof(x)
type U = T<number>