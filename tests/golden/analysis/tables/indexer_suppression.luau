local _: { read [number]: "hello" } = ... :: { read [any]: string } -- Not OK
local _: { read [number]: string } = ... :: { read [string]: any } -- Not OK
local _: { [number]: "hello" } = ... :: { [any]: string } -- Not OK
local _: { [number]: string } = ... :: { [string]: any } -- Not OK
local _: { x: number, [number]: "hello" } = ... :: { x: any, [any]: string } -- Not OK
local _: { read [number]: { read [number]: "hello" } } = ... :: { read [number]: { read [any]: string } } -- Not OK
local _: { [number]: string } = ... :: { read [any]: string }

local function takesHellos(_t: { read [number]: "hello" }) end
takesHellos(... :: { read [any]: string }) -- Not OK

-- Everything below this line is OK :)

local _: { read [number]: string } = ... :: { read [any]: string }
local _: { [number]: string } = ... :: { [any]: string }
local _: { read [number]: "hello" } = ... :: { read [any]: any }
local _: { read [number]: { read [number]: string } } = ... :: { read [number]: { read [any]: string } }

