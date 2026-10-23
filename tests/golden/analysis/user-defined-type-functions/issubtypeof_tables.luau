type function issub(subtype, supertype)
    return types.singleton(subtype:issubtypeof(supertype))
end

type Wide = { a: number, b: string }
type Narrow = { a: number }
type Different = { a: string }
type ReadNumber = { read a: number }
type ReadNumberOrString = { read a: number | string }

local wideIsNarrow: issub<Wide, Narrow> = true
local narrowIsWide: issub<Narrow, Wide> = false
local wideIsDifferent: issub<Wide, Different> = false
local readCovariance: issub<ReadNumber, ReadNumberOrString> = true

assert(wideIsNarrow and not narrowIsWide and not wideIsDifferent and readCovariance)
