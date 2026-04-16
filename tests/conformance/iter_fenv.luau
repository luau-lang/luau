-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- pairs/ipairs may be substituted through getfenv
-- however, they *must* be substituted with functions - we don't support them falling back to generalized iteration
-- next may be substituted, but only with a function or a table. we do not optimize next under the presence of get/setfenv
function testgetfenv()
  local env = getfenv(1)
  env.pairs = function() return "nope" end
  env.ipairs = function() return "nope" end
  env.next = "next"

  local ok, err = pcall(function() for k, v in pairs({}) do end end)
  assert(not ok and err:match("attempt to iterate over a string value"))

  local ok, err = pcall(function() for k, v in ipairs({}) do end end)
  assert(not ok and err:match("attempt to iterate over a string value"))

  local ok, err = pcall(function() for k, v in next, {} do end end)
  assert(not ok and err:match("attempt to iterate over a string value"))
end

testgetfenv()

return "OK"
