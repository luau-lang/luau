return {
    luau = {
        aliases = {
            outer = "./",
            cyclicentry = "@cyclic1",
            cyclic1 = "@cyclic2",
            cyclic2 = "@cyclic3",
            cyclic3 = "@cyclic1"
        }
    }
}
