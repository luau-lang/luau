export type Foo = { Id: number }
export type Bar = { [string]: number }

export type Foobar = Foo & Bar

local tbl: Foobar = nil :: any

tbl.Id = 42 -- OK
tbl.Id = nil -- Not OK!