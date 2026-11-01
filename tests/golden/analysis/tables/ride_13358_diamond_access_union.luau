export type Details = { Id: number }
export type AssetDetails = Details | { Id: number }
export type BundleDetails = Details | { Id: number }
export type CatalogPage = { AssetDetails | BundleDetails }

local function _readwrite(tbl: CatalogPage)
    for _, detail in tbl do
        local _: _luau_print<typeof(detail.Id)>
        detail.Id = 42
    end
end