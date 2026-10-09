-- https://github.com/luau-lang/luau/issues/2613

local function fn<T>(...: T & ("Lit1" | "Lit2")): ()
end

fn("Lit1", "Lit2")
fn("Lit1", "invalid literal")
fn("invalid literal 1", "invalid literal 2")
