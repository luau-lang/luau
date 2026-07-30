local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

-- HTTP/1.1 Server Framework benchmark
-- Compatible with: Lute, Lua 5.x, LuaJIT
-- Tests: request parsing, routing, middleware, response building, JSON codec

-- ===== Local aliases for hot math/string functions =====
local floor = math.floor
local char = string.char
local byte = string.byte
local sub = string.sub
local find = string.find
local gsub = string.gsub
local format = string.format
local lower = string.lower
local upper = string.upper
local concat = table.concat
local insert = table.insert
local clock = os.clock

-- =========================================================================
-- URL percent-encoding / decoding
-- =========================================================================
function url_decode(str)
    str = gsub(str, "+", " ")
    str = gsub(str, "%%(%x%x)", function(h)
        return char(tonumber(h, 16))
    end)
    return str
end

function url_encode(str)
    str = gsub(str, "([^%w%-_.~])", function(c)
        return format("%%%02X", byte(c))
    end)
    return str
end

-- =========================================================================
-- Query string parser
-- =========================================================================
function parse_query_string(qs)
    local result = {}
    if not qs or qs == "" then return result end
    -- split on &
    local pos = 1
    while pos <= #qs do
        local amp = find(qs, "&", pos, true)
        local segment
        if amp then
            segment = sub(qs, pos, amp - 1)
            pos = amp + 1
        else
            segment = sub(qs, pos)
            pos = #qs + 1
        end
        local eq = find(segment, "=", 1, true)
        if eq then
            local key = url_decode(sub(segment, 1, eq - 1))
            local val = url_decode(sub(segment, eq + 1))
            result[key] = val
        else
            result[url_decode(segment)] = ""
        end
    end
    return result
end

-- =========================================================================
-- Header utilities
-- =========================================================================
function create_headers()
    return { _store = {}, _order = {} }
end

function headers_set(h, name, value)
    local lname = lower(name)
    if not h._store[lname] then
        insert(h._order, lname)
    end
    h._store[lname] = { name = name, values = { value } }
end

function headers_add(h, name, value)
    local lname = lower(name)
    if not h._store[lname] then
        insert(h._order, lname)
        h._store[lname] = { name = name, values = {} }
    end
    insert(h._store[lname].values, value)
end

function headers_get(h, name)
    local entry = h._store[lower(name)]
    if entry and #entry.values > 0 then
        return entry.values[1]
    end
    return nil
end

function headers_get_all(h, name)
    local entry = h._store[lower(name)]
    if entry then return entry.values end
    return {}
end

function headers_has(h, name)
    return h._store[lower(name)] ~= nil
end

function headers_serialize(h)
    local lines = {}
    for i = 1, #h._order do
        local lname = h._order[i]
        local entry = h._store[lname]
        for j = 1, #entry.values do
            insert(lines, entry.name .. ": " .. entry.values[j])
        end
    end
    return concat(lines, "\r\n")
end

-- =========================================================================
-- Cookie parser
-- =========================================================================
function parse_cookies(cookie_header)
    local cookies = {}
    if not cookie_header or cookie_header == "" then return cookies end
    local pos = 1
    while pos <= #cookie_header do
        local semi = find(cookie_header, ";", pos, true)
        local segment
        if semi then
            segment = sub(cookie_header, pos, semi - 1)
            pos = semi + 1
            -- skip space after semicolon
            if pos <= #cookie_header and sub(cookie_header, pos, pos) == " " then
                pos = pos + 1
            end
        else
            segment = sub(cookie_header, pos)
            pos = #cookie_header + 1
        end
        local eq = find(segment, "=", 1, true)
        if eq then
            local name = sub(segment, 1, eq - 1)
            local val = sub(segment, eq + 1)
            -- trim whitespace from name
            name = gsub(name, "^%s+", "")
            name = gsub(name, "%s+$", "")
            cookies[name] = val
        end
    end
    return cookies
end

-- =========================================================================
-- Set-Cookie builder
-- =========================================================================
function build_set_cookie(name, value, opts)
    local parts = { name .. "=" .. value }
    if opts then
        if opts.path then insert(parts, "Path=" .. opts.path) end
        if opts.domain then insert(parts, "Domain=" .. opts.domain) end
        if opts.max_age then insert(parts, "Max-Age=" .. tostring(opts.max_age)) end
        if opts.expires then insert(parts, "Expires=" .. opts.expires) end
        if opts.sekure then insert(parts, "Sekure") end
        if opts.httponly then insert(parts, "HttpOnly") end
        if opts.samesite then insert(parts, "SameSite=" .. opts.samesite) end
    end
    return concat(parts, "; ")
end

-- =========================================================================
-- Content negotiation (Accept header with q-values)
-- =========================================================================
function parse_accept_header(accept)
    local entries = {}
    if not accept or accept == "" then return entries end
    local pos = 1
    while pos <= #accept do
        local comma = find(accept, ",", pos, true)
        local segment
        if comma then
            segment = sub(accept, pos, comma - 1)
            pos = comma + 1
        else
            segment = sub(accept, pos)
            pos = #accept + 1
        end
        -- trim
        segment = gsub(segment, "^%s+", "")
        segment = gsub(segment, "%s+$", "")
        -- extract q value
        local media_type = segment
        local q = 1.0
        local semi = find(segment, ";", 1, true)
        if semi then
            media_type = sub(segment, 1, semi - 1)
            media_type = gsub(media_type, "%s+$", "")
            local qpart = sub(segment, semi + 1)
            local qval = find(qpart, "q=", 1, true)
            if qval then
                local qstr = sub(qpart, qval + 2)
                qstr = gsub(qstr, "%s+", "")
                q = tonumber(qstr) or 1.0
            end
        end
        insert(entries, { media_type = media_type, q = q })
    end
    -- sort by q descending
    table.sort(entries, function(a, b) return a.q > b.q end)
    return entries
end

function negotiate_content_type(accept_header, available)
    local prefs = parse_accept_header(accept_header)
    for i = 1, #prefs do
        local wanted = prefs[i].media_type
        for j = 1, #available do
            if wanted == available[j] or wanted == "*/*" then
                return available[j]
            end
            -- check type/* match
            local slash = find(wanted, "/", 1, true)
            if slash then
                local wtype = sub(wanted, 1, slash)
                if sub(wanted, slash + 1) == "*" then
                    if sub(available[j], 1, #wtype) == wtype then
                        return available[j]
                    end
                end
            end
        end
    end
    return available[1]
end

-- =========================================================================
-- HTTP Request Parser
-- =========================================================================
function parse_request(raw)
    local req = {}
    req.headers = create_headers()
    req.body = ""
    req.method = "GET"
    req.path = "/"
    req.version = "HTTP/1.1"
    req.query_string = ""
    req.query = {}

    -- Find end of request line
    local crlf = find(raw, "\r\n", 1, true)
    if not crlf then
        -- try just \n
        crlf = find(raw, "\n", 1, true)
        if not crlf then return req end
        local request_line = sub(raw, 1, crlf - 1)
        parse_request_line(req, request_line)
        parse_headers_and_body(req, raw, crlf + 1)
        return req
    end

    local request_line = sub(raw, 1, crlf - 1)
    parse_request_line(req, request_line)
    parse_headers_and_body(req, raw, crlf + 2)
    return req
end

function parse_request_line(req, line)
    -- METHOD PATH VERSION
    local sp1 = find(line, " ", 1, true)
    if not sp1 then return end
    req.method = sub(line, 1, sp1 - 1)
    local sp2 = find(line, " ", sp1 + 1, true)
    if sp2 then
        local full_path = sub(line, sp1 + 1, sp2 - 1)
        req.version = sub(line, sp2 + 1)
        -- split path and query
        local qmark = find(full_path, "?", 1, true)
        if qmark then
            req.path = sub(full_path, 1, qmark - 1)
            req.query_string = sub(full_path, qmark + 1)
            req.query = parse_query_string(req.query_string)
        else
            req.path = full_path
        end
    else
        req.path = sub(line, sp1 + 1)
    end
end

function parse_headers_and_body(req, raw, start)
    local pos = start
    local rawlen = #raw
    while pos <= rawlen do
        -- find end of this header line
        local eol = find(raw, "\r\n", pos, true)
        local next_pos
        if eol then
            next_pos = eol + 2
        else
            eol = find(raw, "\n", pos, true)
            if eol then
                next_pos = eol + 1
            else
                -- rest is one last header
                eol = rawlen + 1
                next_pos = rawlen + 1
            end
        end

        local line = sub(raw, pos, eol - 1)
        if line == "" then
            -- empty line = end of headers, rest is body
            req.body = sub(raw, next_pos)
            return
        end

        -- parse header
        local colon = find(line, ":", 1, true)
        if colon then
            local name = sub(line, 1, colon - 1)
            local value = sub(line, colon + 1)
            -- trim leading whitespace from value
            value = gsub(value, "^%s+", "")
            headers_add(req.headers, name, value)
        end

        pos = next_pos
    end
end

-- =========================================================================
-- Router
-- =========================================================================
function create_router()
    return { routes = {} }
end

function router_add(router, method, pattern, handler)
    insert(router.routes, {
        method = method,
        pattern = pattern,
        segments = split_path(pattern),
        handler = handler
    })
end

function split_path(path)
    local segs = {}
    if path == "/" then return segs end
    local pos = 1
    if sub(path, 1, 1) == "/" then pos = 2 end
    while pos <= #path do
        local sl = find(path, "/", pos, true)
        if sl then
            insert(segs, sub(path, pos, sl - 1))
            pos = sl + 1
        else
            insert(segs, sub(path, pos))
            pos = #path + 1
        end
    end
    return segs
end

function router_match(router, method, path)
    local path_segs = split_path(path)
    for i = 1, #router.routes do
        local route = router.routes[i]
        if route.method == method or route.method == "*" then
            local params = match_segments(route.segments, path_segs)
            if params then
                return route.handler, params
            end
        end
    end
    return nil, nil
end

function match_segments(pattern_segs, path_segs)
    local params = {}
    local pi = 1
    for i = 1, #pattern_segs do
        local seg = pattern_segs[i]
        if seg == "*" then
            -- wildcard matches rest
            local rest = {}
            for j = pi, #path_segs do
                insert(rest, path_segs[j])
            end
            params["*"] = concat(rest, "/")
            return params
        elseif sub(seg, 1, 1) == ":" then
            -- parameterized segment
            if pi > #path_segs then return nil end
            local param_name = sub(seg, 2)
            params[param_name] = path_segs[pi]
            pi = pi + 1
        else
            -- exact match
            if pi > #path_segs then return nil end
            if path_segs[pi] ~= seg then return nil end
            pi = pi + 1
        end
    end
    -- all pattern segments consumed, check path fully consumed
    if pi ~= #path_segs + 1 then return nil end
    return params
end

-- =========================================================================
-- Middleware chain
-- =========================================================================
function create_middleware_chain(middlewares, final_handler)
    -- Build chain from inside out
    local handler = final_handler
    local i = #middlewares
    while i >= 1 do
        local mw = middlewares[i]
        local next_handler = handler
        handler = function(req, res)
            return mw(req, res, next_handler)
        end
        i = i - 1
    end
    return handler
end

-- Logging middleware
function middleware_logging(req, res, next_handler)
    res._log = (res._log or "") .. "[LOG " .. req.method .. " " .. req.path .. "] "
    return next_handler(req, res)
end

-- Auth check middleware
function middleware_auth(req, res, next_handler)
    local auth = headers_get(req.headers, "Authorization")
    if auth then
        req.authenticated = true
        req.auth_token = auth
    else
        req.authenticated = false
        req.auth_token = ""
    end
    return next_handler(req, res)
end

-- CORS middleware
function middleware_cors(req, res, next_handler)
    headers_set(res.headers, "Access-Control-Allow-Origin", "*")
    headers_set(res.headers, "Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
    headers_set(res.headers, "Access-Control-Allow-Headers", "Content-Type, Authorization")
    return next_handler(req, res)
end

-- Rate limiting middleware (simulated)
function middleware_rate_limit(req, res, next_handler)
    req.rate_limited = false
    return next_handler(req, res)
end

-- =========================================================================
-- Response builder
-- =========================================================================
function create_response()
    local res = {}
    res.status = 200
    res.status_text = "OK"
    res.headers = create_headers()
    res.body = ""
    res._log = ""
    return res
end

function response_set_status(res, code, text)
    res.status = code
    res.status_text = text or get_status_text(code)
end

function get_status_text(code)
    if code == 200 then return "OK"
    elseif code == 201 then return "Created"
    elseif code == 204 then return "No Content"
    elseif code == 301 then return "Moved Permanently"
    elseif code == 302 then return "Found"
    elseif code == 304 then return "Not Modified"
    elseif code == 400 then return "Bad Request"
    elseif code == 401 then return "Unauthorized"
    elseif code == 403 then return "Forbidden"
    elseif code == 404 then return "Not Found"
    elseif code == 405 then return "Method Not Allowed"
    elseif code == 409 then return "Conflict"
    elseif code == 413 then return "Payload Too Large"
    elseif code == 415 then return "Unsupported Media Type"
    elseif code == 422 then return "Unprocessable Entity"
    elseif code == 429 then return "Too Many Requests"
    elseif code == 500 then return "Internal Server Error"
    elseif code == 502 then return "Bad Gateway"
    elseif code == 503 then return "Service Unavailable"
    else return "Unknown"
    end
end

function response_set_body(res, body, content_type)
    res.body = body
    headers_set(res.headers, "Content-Length", tostring(#body))
    if content_type then
        headers_set(res.headers, "Content-Type", content_type)
    end
end

function response_serialize(res)
    local parts = {}
    insert(parts, "HTTP/1.1 " .. tostring(res.status) .. " " .. res.status_text)
    insert(parts, "\r\n")
    local hdr_str = headers_serialize(res.headers)
    if hdr_str ~= "" then
        insert(parts, hdr_str)
        insert(parts, "\r\n")
    end
    insert(parts, "\r\n")
    if res.body and #res.body > 0 then
        insert(parts, res.body)
    end
    return concat(parts)
end

-- =========================================================================
-- JSON encoder
-- =========================================================================
function json_encode(val)
    local t = type(val)
    if val == nil then
        return "null"
    elseif t == "boolean" then
        return val and "true" or "false"
    elseif t == "number" then
        if val ~= val then return "null" end
        if val == math.huge or val == -math.huge then return "null" end
        if val == floor(val) and val > -1e15 and val < 1e15 then
            return format("%d", val)
        end
        return format("%.14g", val)
    elseif t == "string" then
        return json_encode_string(val)
    elseif t == "table" then
        -- check if array
        if is_json_array(val) then
            return json_encode_array(val)
        else
            return json_encode_object(val)
        end
    end
    return "null"
end

function json_encode_string(s)
    local buf = { '"' }
    for i = 1, #s do
        local c = byte(s, i)
        if c == 34 then insert(buf, '\\"')
        elseif c == 92 then insert(buf, '\\\\')
        elseif c == 10 then insert(buf, '\\n')
        elseif c == 13 then insert(buf, '\\r')
        elseif c == 9 then insert(buf, '\\t')
        elseif c < 32 then
            insert(buf, format('\\u%04x', c))
        else
            insert(buf, char(c))
        end
    end
    insert(buf, '"')
    return concat(buf)
end

function is_json_array(t)
    local n = #t
    if n == 0 then
        -- check if empty or object
        for _ in next, t do
            return false
        end
        return true
    end
    return true
end

function json_encode_array(arr)
    local parts = {}
    for i = 1, #arr do
        insert(parts, json_encode(arr[i]))
    end
    return "[" .. concat(parts, ",") .. "]"
end

function json_encode_object(obj)
    local parts = {}
    for k, v in next, obj do
        if type(k) == "string" then
            insert(parts, json_encode_string(k) .. ":" .. json_encode(v))
        end
    end
    -- sort for deterministic output
    table.sort(parts)
    return "{" .. concat(parts, ",") .. "}"
end

-- =========================================================================
-- JSON decoder
-- =========================================================================
function json_decode(str)
    local pos = 1
    local val
    val, pos = json_parse_value(str, pos)
    return val
end

function json_skip_whitespace(str, pos)
    while pos <= #str do
        local c = byte(str, pos)
        if c == 32 or c == 9 or c == 10 or c == 13 then
            pos = pos + 1
        else
            break
        end
    end
    return pos
end

function json_parse_value(str, pos)
    pos = json_skip_whitespace(str, pos)
    if pos > #str then return nil, pos end
    local c = byte(str, pos)
    if c == 34 then
        return json_parse_string(str, pos)
    elseif c == 123 then  -- {
        return json_parse_object(str, pos)
    elseif c == 91 then   -- [
        return json_parse_array(str, pos)
    elseif c == 116 then  -- t (true)
        return true, pos + 4
    elseif c == 102 then  -- f (false)
        return false, pos + 5
    elseif c == 110 then  -- n (null)
        return nil, pos + 4
    else
        return json_parse_number(str, pos)
    end
end

function json_parse_string(str, pos)
    pos = pos + 1  -- skip opening quote
    local buf = {}
    while pos <= #str do
        local c = byte(str, pos)
        if c == 34 then  -- closing quote
            return concat(buf), pos + 1
        elseif c == 92 then  -- backslash
            pos = pos + 1
            local esc = byte(str, pos)
            if esc == 34 then insert(buf, '"')
            elseif esc == 92 then insert(buf, '\\')
            elseif esc == 47 then insert(buf, '/')
            elseif esc == 110 then insert(buf, '\n')
            elseif esc == 114 then insert(buf, '\r')
            elseif esc == 116 then insert(buf, '\t')
            elseif esc == 98 then insert(buf, '\b')
            elseif esc == 102 then insert(buf, '\f')
            elseif esc == 117 then  -- \uXXXX
                local hex = sub(str, pos + 1, pos + 4)
                local codepoint = tonumber(hex, 16)
                if codepoint and codepoint < 128 then
                    insert(buf, char(codepoint))
                else
                    insert(buf, "?")
                end
                pos = pos + 4
            end
            pos = pos + 1
        else
            insert(buf, char(c))
            pos = pos + 1
        end
    end
    return concat(buf), pos
end

function json_parse_number(str, pos)
    local start = pos
    if byte(str, pos) == 45 then pos = pos + 1 end  -- minus
    while pos <= #str and byte(str, pos) >= 48 and byte(str, pos) <= 57 do
        pos = pos + 1
    end
    if pos <= #str and byte(str, pos) == 46 then  -- decimal point
        pos = pos + 1
        while pos <= #str and byte(str, pos) >= 48 and byte(str, pos) <= 57 do
            pos = pos + 1
        end
    end
    if pos <= #str and (byte(str, pos) == 101 or byte(str, pos) == 69) then  -- e/E
        pos = pos + 1
        if pos <= #str and (byte(str, pos) == 43 or byte(str, pos) == 45) then
            pos = pos + 1
        end
        while pos <= #str and byte(str, pos) >= 48 and byte(str, pos) <= 57 do
            pos = pos + 1
        end
    end
    local numstr = sub(str, start, pos - 1)
    return tonumber(numstr), pos
end

function json_parse_array(str, pos)
    local arr = {}
    pos = pos + 1  -- skip [
    pos = json_skip_whitespace(str, pos)
    if pos <= #str and byte(str, pos) == 93 then  -- ]
        return arr, pos + 1
    end
    while pos <= #str do
        local val
        val, pos = json_parse_value(str, pos)
        insert(arr, val)
        pos = json_skip_whitespace(str, pos)
        if pos > #str then break end
        local c = byte(str, pos)
        if c == 93 then  -- ]
            return arr, pos + 1
        elseif c == 44 then  -- ,
            pos = pos + 1
        end
    end
    return arr, pos
end

function json_parse_object(str, pos)
    local obj = {}
    pos = pos + 1  -- skip {
    pos = json_skip_whitespace(str, pos)
    if pos <= #str and byte(str, pos) == 125 then  -- }
        return obj, pos + 1
    end
    while pos <= #str do
        pos = json_skip_whitespace(str, pos)
        local key
        key, pos = json_parse_string(str, pos)
        pos = json_skip_whitespace(str, pos)
        pos = pos + 1  -- skip :
        local val
        val, pos = json_parse_value(str, pos)
        obj[key] = val
        pos = json_skip_whitespace(str, pos)
        if pos > #str then break end
        local c = byte(str, pos)
        if c == 125 then  -- }
            return obj, pos + 1
        elseif c == 44 then  -- ,
            pos = pos + 1
        end
    end
    return obj, pos
end

-- =========================================================================
-- Form parser (application/x-www-form-urlencoded)
-- =========================================================================
function parse_form_body(body)
    return parse_query_string(body)
end

-- =========================================================================
-- Multipart form parser (simplified boundary-based)
-- =========================================================================
function parse_multipart(body, boundary)
    local parts = {}
    local delim = "--" .. boundary
    local pos = 1
    -- Skip preamble - find first boundary
    local start = find(body, delim, pos, true)
    if not start then return parts end
    pos = start + #delim
    -- skip CRLF after boundary
    if sub(body, pos, pos + 1) == "\r\n" then pos = pos + 2
    elseif sub(body, pos, pos) == "\n" then pos = pos + 1
    end

    while pos <= #body do
        -- Find the next boundary
        local next_bound = find(body, delim, pos, true)
        if not next_bound then break end
        local part_data = sub(body, pos, next_bound - 1)
        -- Remove trailing CRLF before boundary
        if sub(part_data, -2) == "\r\n" then
            part_data = sub(part_data, 1, -3)
        end
        -- Parse part headers and body
        local part = parse_multipart_part(part_data)
        if part then insert(parts, part) end
        -- Move past boundary
        pos = next_bound + #delim
        -- Check for closing --
        if sub(body, pos, pos + 1) == "--" then break end
        -- skip CRLF
        if sub(body, pos, pos + 1) == "\r\n" then pos = pos + 2
        elseif sub(body, pos, pos) == "\n" then pos = pos + 1
        end
    end
    return parts
end

function parse_multipart_part(data)
    local part = { headers = {}, body = "" }
    -- Find header/body separator
    local sep = find(data, "\r\n\r\n", 1, true)
    if not sep then
        sep = find(data, "\n\n", 1, true)
        if not sep then
            part.body = data
            return part
        end
        local header_section = sub(data, 1, sep - 1)
        part.body = sub(data, sep + 2)
        parse_multipart_headers(part, header_section)
        return part
    end
    local header_section = sub(data, 1, sep - 1)
    part.body = sub(data, sep + 4)
    parse_multipart_headers(part, header_section)
    return part
end

function parse_multipart_headers(part, header_str)
    local pos = 1
    while pos <= #header_str do
        local eol = find(header_str, "\r\n", pos, true)
        if not eol then
            eol = find(header_str, "\n", pos, true)
            if not eol then eol = #header_str + 1 end
        end
        local line = sub(header_str, pos, eol - 1)
        local colon = find(line, ":", 1, true)
        if colon then
            local name = lower(sub(line, 1, colon - 1))
            local value = gsub(sub(line, colon + 1), "^%s+", "")
            part.headers[name] = value
            -- Extract name from content-disposition
            if name == "content-disposition" then
                local nm = find(value, 'name="', 1, true)
                if nm then
                    local nm_start = nm + 6
                    local nm_end = find(value, '"', nm_start, true)
                    if nm_end then
                        part.name = sub(value, nm_start, nm_end - 1)
                    end
                end
                local fn = find(value, 'filename="', 1, true)
                if fn then
                    local fn_start = fn + 10
                    local fn_end = find(value, '"', fn_start, true)
                    if fn_end then
                        part.filename = sub(value, fn_start, fn_end - 1)
                    end
                end
            end
        end
        if find(header_str, "\r\n", pos, true) == eol then
            pos = eol + 2
        else
            pos = eol + 1
        end
    end
end

-- =========================================================================
-- Simple template engine (mustache-like: {{variable}}, {{#if}}, {{#each}})
-- =========================================================================
function template_render(tmpl, context)
    local result = tmpl
    -- Replace simple variables {{name}}
    result = gsub(result, "{{([^#/}]+)}}", function(key)
        key = gsub(key, "^%s+", "")
        key = gsub(key, "%s+$", "")
        local val = template_lookup(context, key)
        if val == nil then return "" end
        return tostring(val)
    end)
    return result
end

function template_lookup(context, key)
    -- Support dotted paths: user.name
    local pos = 1
    local current = context
    while pos <= #key do
        local dot = find(key, ".", pos, true)
        local segment
        if dot then
            segment = sub(key, pos, dot - 1)
            pos = dot + 1
        else
            segment = sub(key, pos)
            pos = #key + 1
        end
        if type(current) ~= "table" then return nil end
        current = current[segment]
    end
    return current
end

function template_render_loop(tmpl, context, list_key, item_var)
    -- Render template for each item in context[list_key]
    local items = context[list_key]
    if not items then return "" end
    local parts = {}
    for i = 1, #items do
        local item_context = {}
        -- Copy parent context
        for k, v in next, context do
            item_context[k] = v
        end
        -- Add item
        if type(items[i]) == "table" then
            for k, v in next, items[i] do
                item_context[item_var .. "." .. k] = v
            end
            item_context[item_var] = items[i]
        else
            item_context[item_var] = items[i]
        end
        item_context["index"] = i
        insert(parts, template_render(tmpl, item_context))
    end
    return concat(parts)
end

-- =========================================================================
-- ETag generator (simple hash-based)
-- =========================================================================
function generate_etag(content)
    -- Simple FNV-1a-like hash for ETags
    local hash = 2166136261
    for i = 1, #content do
        hash = hash * 16777619
        hash = hash + byte(content, i)
        -- Keep in reasonable integer range
        hash = hash % 4294967296
    end
    return format('"%08x"', hash)
end

-- =========================================================================
-- Basic auth decoder
-- =========================================================================
function decode_basic_auth(auth_header)
    if not auth_header then return nil, nil end
    local scheme_end = find(auth_header, " ", 1, true)
    if not scheme_end then return nil, nil end
    local scheme = sub(auth_header, 1, scheme_end - 1)
    if lower(scheme) ~= "basic" then return nil, nil end
    local encoded = sub(auth_header, scheme_end + 1)
    -- Simple base64 decode (limited for benchmark purposes)
    local decoded = base64_decode(encoded)
    if not decoded then return nil, nil end
    local colon = find(decoded, ":", 1, true)
    if not colon then return decoded, "" end
    return sub(decoded, 1, colon - 1), sub(decoded, colon + 1)
end

-- Simplified base64 decode
function base64_decode(input)
    local b64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    local b64lookup = {}
    for i = 1, 64 do
        b64lookup[byte(b64chars, i)] = i - 1
    end
    b64lookup[byte("=", 1)] = 0

    local output = {}
    local i = 1
    while i <= #input do
        local c1 = b64lookup[byte(input, i)] or 0
        local c2 = b64lookup[byte(input, i + 1)] or 0
        local c3 = b64lookup[byte(input, i + 2)] or 0
        local c4 = b64lookup[byte(input, i + 3)] or 0

        local n = c1 * 262144 + c2 * 4096 + c3 * 64 + c4

        insert(output, char(floor(n / 65536) % 256))
        if i + 2 <= #input and sub(input, i + 2, i + 2) ~= "=" then
            insert(output, char(floor(n / 256) % 256))
        end
        if i + 3 <= #input and sub(input, i + 3, i + 3) ~= "=" then
            insert(output, char(n % 256))
        end
        i = i + 4
    end
    return concat(output)
end

-- Base64 encode
function base64_encode(input)
    local b64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    local output = {}
    local i = 1
    while i <= #input do
        local b1 = byte(input, i) or 0
        local b2 = (i + 1 <= #input) and byte(input, i + 1) or 0
        local b3 = (i + 2 <= #input) and byte(input, i + 2) or 0

        local n = b1 * 65536 + b2 * 256 + b3

        insert(output, sub(b64chars, floor(n / 262144) % 64 + 1, floor(n / 262144) % 64 + 1))
        insert(output, sub(b64chars, floor(n / 4096) % 64 + 1, floor(n / 4096) % 64 + 1))

        if i + 1 <= #input then
            insert(output, sub(b64chars, floor(n / 64) % 64 + 1, floor(n / 64) % 64 + 1))
        else
            insert(output, "=")
        end
        if i + 2 <= #input then
            insert(output, sub(b64chars, n % 64 + 1, n % 64 + 1))
        else
            insert(output, "=")
        end
        i = i + 3
    end
    return concat(output)
end

-- =========================================================================
-- Rate limiter (token bucket simulation)
-- =========================================================================
function create_rate_limiter(capacity, refill_rate)
    return {
        capacity = capacity,
        tokens = capacity,
        refill_rate = refill_rate,
        last_refill = 0
    }
end

function rate_limiter_allow(limiter, now)
    -- Refill tokens
    local elapsed = now - limiter.last_refill
    local new_tokens = elapsed * limiter.refill_rate
    limiter.tokens = limiter.tokens + new_tokens
    if limiter.tokens > limiter.capacity then
        limiter.tokens = limiter.capacity
    end
    limiter.last_refill = now

    if limiter.tokens >= 1 then
        limiter.tokens = limiter.tokens - 1
        return true
    end
    return false
end

-- =========================================================================
-- Cache (LRU-like with TTL)
-- =========================================================================
function create_cache(max_size)
    return {
        max_size = max_size or 100,
        store = {},
        order = {},
        count = 0
    }
end

function cache_get(cache, key)
    local entry = cache.store[key]
    if not entry then return nil end
    if entry.expires > 0 and entry.expires < clock() then
        cache_delete(cache, key)
        return nil
    end
    return entry.value
end

function cache_set(cache, key, value, ttl)
    if cache.store[key] then
        cache.store[key].value = value
        cache.store[key].expires = ttl and (clock() + ttl) or 0
        return
    end
    if cache.count >= cache.max_size then
        -- Evict oldest
        if #cache.order > 0 then
            local oldest = cache.order[1]
            table.remove(cache.order, 1)
            cache.store[oldest] = nil
            cache.count = cache.count - 1
        end
    end
    cache.store[key] = { value = value, expires = ttl and (clock() + ttl) or 0 }
    insert(cache.order, key)
    cache.count = cache.count + 1
end

function cache_delete(cache, key)
    if cache.store[key] then
        cache.store[key] = nil
        cache.count = cache.count - 1
        -- Remove from order
        for i = 1, #cache.order do
            if cache.order[i] == key then
                table.remove(cache.order, i)
                break
            end
        end
    end
end

-- =========================================================================
-- Request validation
-- =========================================================================
function validate_request(req, rules)
    local errors = {}
    for i = 1, #rules do
        local rule = rules[i]
        local value = nil
        if rule.source == "query" then
            value = req.query[rule.field]
        elseif rule.source == "body" then
            local data = json_decode(req.body)
            if data then value = data[rule.field] end
        elseif rule.source == "header" then
            value = headers_get(req.headers, rule.field)
        elseif rule.source == "params" then
            value = req.params[rule.field]
        end

        if rule.required and (value == nil or value == "") then
            insert(errors, rule.field .. " is required")
        end
        if rule.min_length and value and #tostring(value) < rule.min_length then
            insert(errors, rule.field .. " must be at least " .. tostring(rule.min_length) .. " characters")
        end
        if rule.max_length and value and #tostring(value) > rule.max_length then
            insert(errors, rule.field .. " must be at most " .. tostring(rule.max_length) .. " characters")
        end
        if rule.pattern and value then
            if not find(tostring(value), rule.pattern) then
                insert(errors, rule.field .. " has invalid format")
            end
        end
    end
    return errors
end

-- =========================================================================
-- Compression simulation (run-length encoding for benchmark purposes)
-- =========================================================================
function rle_compress(input)
    if #input == 0 then return "" end
    local output = {}
    local i = 1
    while i <= #input do
        local ch = sub(input, i, i)
        local count = 1
        while i + count <= #input and sub(input, i + count, i + count) == ch do
            count = count + 1
            if count >= 255 then break end
        end
        if count > 3 then
            insert(output, "#")
            insert(output, char(count))
            insert(output, ch)
        else
            for j = 1, count do
                insert(output, ch)
            end
        end
        i = i + count
    end
    return concat(output)
end

function rle_decompress(input)
    local output = {}
    local i = 1
    while i <= #input do
        if sub(input, i, i) == "#" and i + 2 <= #input then
            local count = byte(input, i + 1)
            local ch = sub(input, i + 2, i + 2)
            for j = 1, count do
                insert(output, ch)
            end
            i = i + 3
        else
            insert(output, sub(input, i, i))
            i = i + 1
        end
    end
    return concat(output)
end

-- =========================================================================
-- HTTP/1.1 chunked transfer encoding
-- =========================================================================
function encode_chunked(body, chunk_size)
    local parts = {}
    local pos = 1
    while pos <= #body do
        local chunk_end = pos + chunk_size - 1
        if chunk_end > #body then chunk_end = #body end
        local chunk = sub(body, pos, chunk_end)
        insert(parts, format("%x\r\n%s\r\n", #chunk, chunk))
        pos = chunk_end + 1
    end
    insert(parts, "0\r\n\r\n")
    return concat(parts)
end

function decode_chunked(encoded)
    local parts = {}
    local pos = 1
    while pos <= #encoded do
        -- Read chunk size line
        local eol = find(encoded, "\r\n", pos, true)
        if not eol then break end
        local size_str = sub(encoded, pos, eol - 1)
        local size = tonumber(size_str, 16)
        if not size or size == 0 then break end
        pos = eol + 2
        local chunk = sub(encoded, pos, pos + size - 1)
        insert(parts, chunk)
        pos = pos + size + 2  -- skip chunk data + CRLF
    end
    return concat(parts)
end

-- =========================================================================
-- HTTP Range request handling
-- =========================================================================
function parse_range_header(range_str, total_size)
    -- Parse: bytes=0-499 or bytes=500- or bytes=-500
    if not range_str then return nil end
    local prefix = sub(range_str, 1, 6)
    if prefix ~= "bytes=" then return nil end
    local spec = sub(range_str, 7)
    local dash = find(spec, "-", 1, true)
    if not dash then return nil end
    local range_start = sub(spec, 1, dash - 1)
    local range_end = sub(spec, dash + 1)

    local s, e
    if range_start == "" then
        -- suffix: last N bytes
        e = total_size - 1
        s = total_size - (tonumber(range_end) or 0)
        if s < 0 then s = 0 end
    elseif range_end == "" then
        s = tonumber(range_start) or 0
        e = total_size - 1
    else
        s = tonumber(range_start) or 0
        e = tonumber(range_end) or (total_size - 1)
    end

    if s > e or s >= total_size then return nil end
    if e >= total_size then e = total_size - 1 end
    return { start = s, finish = e, total = total_size }
end

-- =========================================================================
-- Server-Sent Events builder
-- =========================================================================
function build_sse_event(data, event_type, id)
    local parts = {}
    if id then insert(parts, "id: " .. tostring(id) .. "\n") end
    if event_type then insert(parts, "event: " .. event_type .. "\n") end
    -- Split data by newlines
    local pos = 1
    while pos <= #data do
        local nl = find(data, "\n", pos, true)
        if nl then
            insert(parts, "data: " .. sub(data, pos, nl - 1) .. "\n")
            pos = nl + 1
        else
            insert(parts, "data: " .. sub(data, pos) .. "\n")
            pos = #data + 1
        end
    end
    insert(parts, "\n")
    return concat(parts)
end

-- =========================================================================
-- WebSocket frame builder (simplified)
-- =========================================================================
function build_ws_frame(payload, opcode)
    opcode = opcode or 1  -- text frame
    local frame = {}
    local fin_and_opcode = 128 + opcode  -- FIN=1
    insert(frame, char(fin_and_opcode))
    local len = #payload
    if len <= 125 then
        insert(frame, char(len))
    elseif len <= 65535 then
        insert(frame, char(126))
        insert(frame, char(floor(len / 256)))
        insert(frame, char(len % 256))
    else
        insert(frame, char(127))
        -- 8 bytes for length (simplified - only use lower 4 bytes)
        insert(frame, char(0))
        insert(frame, char(0))
        insert(frame, char(0))
        insert(frame, char(0))
        insert(frame, char(floor(len / 16777216) % 256))
        insert(frame, char(floor(len / 65536) % 256))
        insert(frame, char(floor(len / 256) % 256))
        insert(frame, char(len % 256))
    end
    insert(frame, payload)
    return concat(frame)
end

function parse_ws_frame(data)
    if #data < 2 then return nil end
    local b1 = byte(data, 1)
    local b2 = byte(data, 2)
    local fin = b1 >= 128
    local opcode = b1 % 16
    local masked = b2 >= 128
    local payload_len = b2 % 128
    local offset = 3
    if payload_len == 126 then
        if #data < 4 then return nil end
        payload_len = byte(data, 3) * 256 + byte(data, 4)
        offset = 5
    elseif payload_len == 127 then
        if #data < 10 then return nil end
        payload_len = byte(data, 7) * 16777216 + byte(data, 8) * 65536 + byte(data, 9) * 256 + byte(data, 10)
        offset = 11
    end
    local payload = sub(data, offset, offset + payload_len - 1)
    return { fin = fin, opcode = opcode, masked = masked, payload = payload }
end

-- =========================================================================
-- MIME type lookup
-- =========================================================================
MIME_TYPES = {
    html = "text/html",
    htm = "text/html",
    css = "text/css",
    js = "application/javascript",
    json = "application/json",
    xml = "application/xml",
    txt = "text/plain",
    csv = "text/csv",
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    gif = "image/gif",
    svg = "image/svg+xml",
    ico = "image/x-icon",
    webp = "image/webp",
    pdf = "application/pdf",
    zip = "application/zip",
    gz = "application/gzip",
    mp3 = "audio/mpeg",
    mp4 = "video/mp4",
    woff = "font/woff",
    woff2 = "font/woff2",
    ttf = "font/ttf",
    eot = "application/vnd.ms-fontobject"
}

function get_mime_type(path)
    local dot = nil
    for i = #path, 1, -1 do
        if sub(path, i, i) == "." then
            dot = i
            break
        end
    end
    if not dot then return "application/octet-stream" end
    local ext = lower(sub(path, dot + 1))
    return MIME_TYPES[ext] or "application/octet-stream"
end

-- =========================================================================
-- Security: CSRF token generation/validation (simulated)
-- =========================================================================
function generate_csrf_token(session_id)
    -- Simple hash-based CSRF token
    local hash = 5381
    for i = 1, #session_id do
        hash = hash * 33 + byte(session_id, i)
        hash = hash % 4294967296
    end
    return format("%08x%08x", hash, hash * 2654435761 % 4294967296)
end

function validate_csrf_token(token, session_id)
    local expected = generate_csrf_token(session_id)
    return token == expected
end

-- =========================================================================
-- Request context builder (combines all parsed info)
-- =========================================================================
function build_request_context(req)
    local ctx = {}
    ctx.method = req.method
    ctx.path = req.path
    ctx.query = req.query
    ctx.cookies = req.cookies or {}
    ctx.authenticated = req.authenticated or false
    ctx.auth_token = req.auth_token or ""
    ctx.content_type = headers_get(req.headers, "Content-Type") or ""
    ctx.accept = headers_get(req.headers, "Accept") or "*/*"
    ctx.user_agent = headers_get(req.headers, "User-Agent") or ""
    ctx.host = headers_get(req.headers, "Host") or ""
    ctx.body_size = #req.body
    ctx.has_body = #req.body > 0
    return ctx
end

-- =========================================================================
-- Logging formatter
-- =========================================================================
function format_log_entry(req, res, duration_ms)
    return format("[%s] %s %s %d %d %.2fms",
        "2024-01-15T10:30:00Z",
        req.method,
        req.path,
        res.status,
        #res.body,
        duration_ms)
end

-- =========================================================================
-- HTTP/2 HPACK-like header compression (simplified static table)
-- =========================================================================
HPACK_STATIC_TABLE = {
    { name = ":authority", value = "" },
    { name = ":method", value = "GET" },
    { name = ":method", value = "POST" },
    { name = ":path", value = "/" },
    { name = ":path", value = "/index.html" },
    { name = ":scheme", value = "http" },
    { name = ":scheme", value = "https" },
    { name = ":status", value = "200" },
    { name = ":status", value = "204" },
    { name = ":status", value = "206" },
    { name = ":status", value = "304" },
    { name = ":status", value = "400" },
    { name = ":status", value = "404" },
    { name = ":status", value = "500" },
    { name = "accept-charset", value = "" },
    { name = "accept-encoding", value = "gzip, deflate" },
    { name = "accept-language", value = "" },
    { name = "accept-ranges", value = "" },
    { name = "accept", value = "" },
    { name = "access-control-allow-origin", value = "" },
    { name = "age", value = "" },
    { name = "allow", value = "" },
    { name = "authorization", value = "" },
    { name = "cache-control", value = "" },
    { name = "content-disposition", value = "" },
    { name = "content-encoding", value = "" },
    { name = "content-language", value = "" },
    { name = "content-length", value = "" },
    { name = "content-location", value = "" },
    { name = "content-range", value = "" },
    { name = "content-type", value = "" },
    { name = "cookie", value = "" },
    { name = "date", value = "" },
    { name = "etag", value = "" },
    { name = "expect", value = "" },
    { name = "expires", value = "" },
    { name = "from", value = "" },
    { name = "host", value = "" },
    { name = "if-match", value = "" },
    { name = "if-modified-since", value = "" },
    { name = "if-none-match", value = "" },
    { name = "if-range", value = "" },
    { name = "if-unmodified-since", value = "" },
    { name = "last-modified", value = "" },
    { name = "link", value = "" },
    { name = "location", value = "" },
    { name = "max-forwards", value = "" },
    { name = "proxy-authenticate", value = "" },
    { name = "proxy-authorization", value = "" },
    { name = "range", value = "" },
    { name = "referer", value = "" },
    { name = "refresh", value = "" },
    { name = "retry-after", value = "" },
    { name = "server", value = "" },
    { name = "set-cookie", value = "" },
    { name = "strict-transport-security", value = "" },
    { name = "transfer-encoding", value = "" },
    { name = "user-agent", value = "" },
    { name = "vary", value = "" },
    { name = "via", value = "" },
    { name = "www-authenticate", value = "" },
}

function hpack_find_static(name, value)
    for i = 1, #HPACK_STATIC_TABLE do
        local entry = HPACK_STATIC_TABLE[i]
        if entry.name == name then
            if value and entry.value == value then
                return i, true  -- full match
            end
            return i, false  -- name match only
        end
    end
    return nil, false
end

function hpack_encode_headers(headers_list)
    local encoded = {}
    for i = 1, #headers_list do
        local h = headers_list[i]
        local idx, full_match = hpack_find_static(h.name, h.value)
        if idx and full_match then
            -- Indexed header field
            insert(encoded, format("[I:%d]", idx))
        elseif idx then
            -- Literal with name reference
            insert(encoded, format("[R:%d=%s]", idx, h.value))
        else
            -- Literal new
            insert(encoded, format("[N:%s=%s]", h.name, h.value))
        end
    end
    return concat(encoded, " ")
end

-- =========================================================================
-- Redirect chain resolver (simulate following redirects)
-- =========================================================================
function resolve_redirect_chain(responses, max_redirects)
    max_redirects = max_redirects or 10
    local chain = {}
    local current = responses[1]
    local count = 0
    while current and count < max_redirects do
        insert(chain, { status = current.status, location = headers_get(current.headers, "Location") })
        if current.status >= 300 and current.status < 400 then
            local loc = headers_get(current.headers, "Location")
            if loc then
                -- Find matching response (simulated)
                count = count + 1
                local found = false
                for i = 2, #responses do
                    if responses[i].path == loc then
                        current = responses[i]
                        found = true
                        break
                    end
                end
                if not found then break end
            else
                break
            end
        else
            break
        end
    end
    return chain
end

-- =========================================================================
-- Path normalization
-- =========================================================================
function normalize_path(path)
    -- Remove double slashes, resolve . and ..
    local segments = split_path(path)
    local normalized = {}
    for i = 1, #segments do
        local seg = segments[i]
        if seg == "." then
            -- skip
        elseif seg == ".." then
            if #normalized > 0 then
                table.remove(normalized)
            end
        elseif seg ~= "" then
            insert(normalized, seg)
        end
    end
    if #normalized == 0 then return "/" end
    return "/" .. concat(normalized, "/")
end

-- =========================================================================
-- Framework: full request processing
-- =========================================================================
function create_framework()
    local fw = {}
    fw.router = create_router()
    fw.middlewares = {}
    return fw
end

function framework_use(fw, middleware)
    insert(fw.middlewares, middleware)
end

function framework_route(fw, method, pattern, handler)
    router_add(fw.router, method, pattern, handler)
end

function framework_handle_request(fw, raw_request)
    local req = parse_request(raw_request)
    local res = create_response()

    -- Parse cookies
    local cookie_hdr = headers_get(req.headers, "Cookie")
    if cookie_hdr then
        req.cookies = parse_cookies(cookie_hdr)
    else
        req.cookies = {}
    end

    -- Find handler
    local handler, params = router_match(fw.router, req.method, req.path)
    if handler then
        req.params = params or {}
        -- Build middleware chain
        local chain = create_middleware_chain(fw.middlewares, handler)
        chain(req, res)
    else
        -- 404
        response_set_status(res, 404, "Not Found")
        response_set_body(res, '{"error":"Not Found","path":"' .. req.path .. '"}', "application/json")
    end

    return res
end

-- =========================================================================
-- Setup the framework with routes and handlers
-- =========================================================================
function setup_framework()
    local fw = create_framework()

    -- Add middlewares
    framework_use(fw, middleware_logging)
    framework_use(fw, middleware_auth)
    framework_use(fw, middleware_cors)
    framework_use(fw, middleware_rate_limit)

    -- Route: GET /
    framework_route(fw, "GET", "/", function(req, res)
        response_set_status(res, 200, "OK")
        local body = json_encode({ message = "Welcome to the API", version = "1.0.0" })
        response_set_body(res, body, "application/json")
    end)

    -- Route: GET /health
    framework_route(fw, "GET", "/health", function(req, res)
        response_set_status(res, 200, "OK")
        local body = json_encode({ status = "healthy", uptime = 12345 })
        response_set_body(res, body, "application/json")
    end)

    -- Route: GET /users
    framework_route(fw, "GET", "/users", function(req, res)
        response_set_status(res, 200, "OK")
        local users = {
            { id = 1, name = "Alice", email = "alice@example.com" },
            { id = 2, name = "Bob", email = "bob@example.com" },
            { id = 3, name = "Charlie", email = "charlie@example.com" }
        }
        response_set_body(res, json_encode(users), "application/json")
    end)

    -- Route: GET /users/:id
    framework_route(fw, "GET", "/users/:id", function(req, res)
        local id = tonumber(req.params.id) or 0
        if id > 0 and id <= 3 then
            response_set_status(res, 200, "OK")
            local user = { id = id, name = "User" .. tostring(id), email = "user" .. tostring(id) .. "@example.com" }
            response_set_body(res, json_encode(user), "application/json")
        else
            response_set_status(res, 404, "Not Found")
            response_set_body(res, json_encode({ error = "User not found" }), "application/json")
        end
    end)

    -- Route: POST /users
    framework_route(fw, "POST", "/users", function(req, res)
        local ct = headers_get(req.headers, "Content-Type") or ""
        local data
        if find(ct, "application/json", 1, true) then
            data = json_decode(req.body)
        elseif find(ct, "application/x-www-form-urlencoded", 1, true) then
            data = parse_form_body(req.body)
        else
            data = { name = "unknown" }
        end
        if data and data.name then
            response_set_status(res, 201, "Created")
            local new_user = { id = 4, name = data.name, created = true }
            response_set_body(res, json_encode(new_user), "application/json")
        else
            response_set_status(res, 400, "Bad Request")
            response_set_body(res, json_encode({ error = "Name is required" }), "application/json")
        end
    end)

    -- Route: PUT /users/:id
    framework_route(fw, "PUT", "/users/:id", function(req, res)
        local id = tonumber(req.params.id) or 0
        local data = json_decode(req.body)
        if id > 0 and data then
            response_set_status(res, 200, "OK")
            local updated = { id = id, name = data.name or "Updated", updated = true }
            response_set_body(res, json_encode(updated), "application/json")
        else
            response_set_status(res, 400, "Bad Request")
            response_set_body(res, json_encode({ error = "Invalid request" }), "application/json")
        end
    end)

    -- Route: DELETE /users/:id
    framework_route(fw, "DELETE", "/users/:id", function(req, res)
        local id = tonumber(req.params.id) or 0
        if id > 0 then
            response_set_status(res, 200, "OK")
            response_set_body(res, json_encode({ deleted = true, id = id }), "application/json")
        else
            response_set_status(res, 400, "Bad Request")
            response_set_body(res, json_encode({ error = "Invalid ID" }), "application/json")
        end
    end)

    -- Route: GET /posts
    framework_route(fw, "GET", "/posts", function(req, res)
        response_set_status(res, 200, "OK")
        local posts = {}
        for i = 1, 5 do
            insert(posts, { id = i, title = "Post " .. tostring(i), body = "Content of post " .. tostring(i) })
        end
        response_set_body(res, json_encode(posts), "application/json")
    end)

    -- Route: GET /posts/:id
    framework_route(fw, "GET", "/posts/:id", function(req, res)
        local id = tonumber(req.params.id) or 0
        if id > 0 and id <= 5 then
            response_set_status(res, 200, "OK")
            local post = { id = id, title = "Post " .. tostring(id), body = "Content of post " .. tostring(id), author_id = 1 }
            response_set_body(res, json_encode(post), "application/json")
        else
            response_set_status(res, 404, "Not Found")
            response_set_body(res, json_encode({ error = "Post not found" }), "application/json")
        end
    end)

    -- Route: POST /posts
    framework_route(fw, "POST", "/posts", function(req, res)
        local data = json_decode(req.body)
        if data and data.title then
            response_set_status(res, 201, "Created")
            response_set_body(res, json_encode({ id = 6, title = data.title, created = true }), "application/json")
        else
            response_set_status(res, 400, "Bad Request")
            response_set_body(res, json_encode({ error = "Title required" }), "application/json")
        end
    end)

    -- Route: GET /comments/:id
    framework_route(fw, "GET", "/comments/:id", function(req, res)
        local id = tonumber(req.params.id) or 0
        response_set_status(res, 200, "OK")
        local comment = { id = id, text = "Comment " .. tostring(id), post_id = 1, author = "User1" }
        response_set_body(res, json_encode(comment), "application/json")
    end)

    -- Route: POST /login
    framework_route(fw, "POST", "/login", function(req, res)
        local data = json_decode(req.body)
        if data and data.username == "admin" and data.password == "secret" then
            response_set_status(res, 200, "OK")
            local token_body = json_encode({ token = "abc123xyz", expires_in = 3600 })
            response_set_body(res, token_body, "application/json")
            local cookie = build_set_cookie("session", "abc123xyz", {
                path = "/", httponly = true, sekure = true, max_age = 3600
            })
            headers_set(res.headers, "Set-Cookie", cookie)
        else
            response_set_status(res, 401, "Unauthorized")
            response_set_body(res, json_encode({ error = "Invalid credentials" }), "application/json")
        end
    end)

    -- Route: POST /logout
    framework_route(fw, "POST", "/logout", function(req, res)
        response_set_status(res, 200, "OK")
        response_set_body(res, json_encode({ message = "Logged out" }), "application/json")
        local cookie = build_set_cookie("session", "", { path = "/", max_age = 0 })
        headers_set(res.headers, "Set-Cookie", cookie)
    end)

    -- Route: GET /search
    framework_route(fw, "GET", "/search", function(req, res)
        local q = req.query.q or ""
        local page = tonumber(req.query.page) or 1
        local limit = tonumber(req.query.limit) or 10
        response_set_status(res, 200, "OK")
        local results = {}
        for i = 1, limit do
            insert(results, { id = (page - 1) * limit + i, title = "Result for: " .. q })
        end
        local body = json_encode({ query = q, page = page, total = 100, results = results })
        response_set_body(res, body, "application/json")
    end)

    -- Route: OPTIONS /users (CORS preflight)
    framework_route(fw, "OPTIONS", "/users", function(req, res)
        response_set_status(res, 204, "No Content")
        res.body = ""
        headers_set(res.headers, "Content-Length", "0")
    end)

    -- Route: GET /files/*
    framework_route(fw, "GET", "/files/*", function(req, res)
        local filepath = req.params["*"] or ""
        response_set_status(res, 200, "OK")
        response_set_body(res, json_encode({ file = filepath, size = #filepath * 100 }), "application/json")
    end)

    -- Route: PATCH /users/:id
    framework_route(fw, "PATCH", "/users/:id", function(req, res)
        local id = tonumber(req.params.id) or 0
        local data = json_decode(req.body)
        response_set_status(res, 200, "OK")
        local patched = { id = id, patched = true }
        if data and data.name then patched.name = data.name end
        response_set_body(res, json_encode(patched), "application/json")
    end)

    -- Route: GET /negotiate
    framework_route(fw, "GET", "/negotiate", function(req, res)
        local accept = headers_get(req.headers, "Accept") or "*/*"
        local chosen = negotiate_content_type(accept, {
            "application/json", "text/html", "text/plain"
        })
        response_set_status(res, 200, "OK")
        if chosen == "application/json" then
            response_set_body(res, json_encode({ format = "json" }), chosen)
        elseif chosen == "text/html" then
            response_set_body(res, "<html><body>HTML response</body></html>", chosen)
        else
            response_set_body(res, "Plain text response", chosen)
        end
    end)

    -- Route: POST /upload
    framework_route(fw, "POST", "/upload", function(req, res)
        local size = #req.body
        response_set_status(res, 200, "OK")
        response_set_body(res, json_encode({ uploaded = true, size = size }), "application/json")
    end)

    -- Route: GET /redirect
    framework_route(fw, "GET", "/redirect", function(req, res)
        response_set_status(res, 302, "Found")
        headers_set(res.headers, "Location", "/users")
        response_set_body(res, "", "text/plain")
    end)

    -- Route: GET /error
    framework_route(fw, "GET", "/error", function(req, res)
        response_set_status(res, 500, "Internal Server Error")
        response_set_body(res, json_encode({ error = "Something went wrong", code = 500 }), "application/json")
    end)

    -- Route: GET /api/v1/items
    framework_route(fw, "GET", "/api/v1/items", function(req, res)
        response_set_status(res, 200, "OK")
        local items = {}
        for i = 1, 10 do
            insert(items, { id = i, name = "Item" .. tostring(i), price = i * 9.99 })
        end
        response_set_body(res, json_encode(items), "application/json")
    end)

    -- Route: GET /api/v1/items/:id
    framework_route(fw, "GET", "/api/v1/items/:id", function(req, res)
        local id = tonumber(req.params.id) or 0
        response_set_status(res, 200, "OK")
        response_set_body(res, json_encode({ id = id, name = "Item" .. tostring(id), price = id * 9.99 }), "application/json")
    end)

    -- Route: POST /api/v1/orders
    framework_route(fw, "POST", "/api/v1/orders", function(req, res)
        local data = json_decode(req.body)
        response_set_status(res, 201, "Created")
        local order = { id = 1001, items = data and data.items or {}, total = 49.95, status = "pending" }
        response_set_body(res, json_encode(order), "application/json")
    end)

    -- Route: GET /headers
    framework_route(fw, "GET", "/headers", function(req, res)
        response_set_status(res, 200, "OK")
        local info = {
            user_agent = headers_get(req.headers, "User-Agent") or "unknown",
            accept = headers_get(req.headers, "Accept") or "*/*",
            host = headers_get(req.headers, "Host") or "unknown"
        }
        response_set_body(res, json_encode(info), "application/json")
    end)

    -- Route: GET /cookies
    framework_route(fw, "GET", "/cookies", function(req, res)
        response_set_status(res, 200, "OK")
        response_set_body(res, json_encode(req.cookies), "application/json")
    end)

    return fw
end

-- =========================================================================
-- Test HTTP requests (raw strings)
-- =========================================================================
function build_test_requests()
    local reqs = {}

    -- 1. Simple GET /
    insert(reqs, "GET / HTTP/1.1\r\nHost: localhost:8080\r\nUser-Agent: TestClient/1.0\r\nAccept: */*\r\n\r\n")

    -- 2. GET /health
    insert(reqs, "GET /health HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n")

    -- 3. GET /users
    insert(reqs, "GET /users HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\nAuthorization: Bearer token123\r\n\r\n")

    -- 4. GET /users/1
    insert(reqs, "GET /users/1 HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n")

    -- 5. GET /users/2
    insert(reqs, "GET /users/2 HTTP/1.1\r\nHost: localhost:8080\r\nAuthorization: Bearer mytoken\r\nAccept: application/json\r\n\r\n")

    -- 6. GET /users/999 (not found)
    insert(reqs, "GET /users/999 HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n")

    -- 7. POST /users with JSON body
    insert(reqs, "POST /users HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\nContent-Length: 27\r\n\r\n{\"name\":\"Dave\",\"age\":30}")

    -- 8. POST /users with form body
    insert(reqs, "POST /users HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: 18\r\n\r\nname=Eve&age=25")

    -- 9. PUT /users/1
    insert(reqs, "PUT /users/1 HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\nAuthorization: Bearer admin_token\r\n\r\n{\"name\":\"Alice Updated\",\"email\":\"alice_new@example.com\"}")

    -- 10. DELETE /users/2
    insert(reqs, "DELETE /users/2 HTTP/1.1\r\nHost: localhost:8080\r\nAuthorization: Bearer admin_token\r\n\r\n")

    -- 11. GET /posts
    insert(reqs, "GET /posts HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\nCookie: session=abc123; theme=dark\r\n\r\n")

    -- 12. GET /posts/1
    insert(reqs, "GET /posts/1 HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n")

    -- 13. GET /posts/3
    insert(reqs, "GET /posts/3 HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\nCookie: user=bob; lang=en\r\n\r\n")

    -- 14. GET /posts/99 (not found)
    insert(reqs, "GET /posts/99 HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n")

    -- 15. POST /posts with JSON
    insert(reqs, "POST /posts HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\n\r\n{\"title\":\"New Post\",\"body\":\"This is the content\"}")

    -- 16. POST /login success
    insert(reqs, "POST /login HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\n\r\n{\"username\":\"admin\",\"password\":\"secret\"}")

    -- 17. POST /login failure
    insert(reqs, "POST /login HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\n\r\n{\"username\":\"admin\",\"password\":\"wrong\"}")

    -- 18. POST /logout
    insert(reqs, "POST /logout HTTP/1.1\r\nHost: localhost:8080\r\nCookie: session=abc123xyz\r\n\r\n")

    -- 19. GET /search with query
    insert(reqs, "GET /search?q=hello+world&page=2&limit=5 HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n")

    -- 20. OPTIONS /users (CORS preflight)
    insert(reqs, "OPTIONS /users HTTP/1.1\r\nHost: localhost:8080\r\nOrigin: http://example.com\r\nAccess-Control-Request-Method: POST\r\n\r\n")

    -- 21. GET /files/documents/report.pdf
    insert(reqs, "GET /files/documents/report.pdf HTTP/1.1\r\nHost: localhost:8080\r\nAccept: */*\r\n\r\n")

    -- 22. GET /files/images/photo.jpg
    insert(reqs, "GET /files/images/photo.jpg HTTP/1.1\r\nHost: localhost:8080\r\nAccept: image/*\r\n\r\n")

    -- 23. PATCH /users/1
    insert(reqs, "PATCH /users/1 HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\nAuthorization: Bearer patchtoken\r\n\r\n{\"name\":\"Alice Patched\"}")

    -- 24. GET /negotiate (wants JSON)
    insert(reqs, "GET /negotiate HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json, text/html;q=0.9, */*;q=0.1\r\n\r\n")

    -- 25. GET /negotiate (wants HTML)
    insert(reqs, "GET /negotiate HTTP/1.1\r\nHost: localhost:8080\r\nAccept: text/html, application/json;q=0.5\r\n\r\n")

    -- 26. GET /negotiate (wants plain text)
    insert(reqs, "GET /negotiate HTTP/1.1\r\nHost: localhost:8080\r\nAccept: text/plain, */*;q=0.1\r\n\r\n")

    -- 27. POST /upload with body
    insert(reqs, "POST /upload HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/octet-stream\r\nContent-Length: 13\r\n\r\nHello, World!")

    -- 28. GET /redirect
    insert(reqs, "GET /redirect HTTP/1.1\r\nHost: localhost:8080\r\n\r\n")

    -- 29. GET /error
    insert(reqs, "GET /error HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n")

    -- 30. GET /nonexistent (404)
    insert(reqs, "GET /nonexistent HTTP/1.1\r\nHost: localhost:8080\r\n\r\n")

    -- 31. GET /api/v1/items
    insert(reqs, "GET /api/v1/items HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\nAuthorization: Bearer apikey\r\n\r\n")

    -- 32. GET /api/v1/items/5
    insert(reqs, "GET /api/v1/items/5 HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n")

    -- 33. POST /api/v1/orders
    insert(reqs, "POST /api/v1/orders HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\nAuthorization: Bearer ordertoken\r\n\r\n{\"items\":[1,2,3],\"shipping\":\"express\"}")

    -- 34. GET /headers
    insert(reqs, "GET /headers HTTP/1.1\r\nHost: api.example.com\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64)\r\nAccept: text/html,application/xhtml+xml\r\n\r\n")

    -- 35. GET /cookies with many cookies
    insert(reqs, "GET /cookies HTTP/1.1\r\nHost: localhost:8080\r\nCookie: session=xyz789; user=alice; pref=dark; lang=en; tz=UTC\r\n\r\n")

    -- 36. GET /users with complex headers
    insert(reqs, "GET /users HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\nAccept-Encoding: gzip, deflate, br\r\nAccept-Language: en-US,en;q=0.9,fr;q=0.8\r\nCache-Control: no-cache\r\nConnection: keep-alive\r\n\r\n")

    -- 37. POST /users with unicode-like content
    insert(reqs, "POST /users HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\n\r\n{\"name\":\"Test User\",\"bio\":\"Hello \\\"World\\\"\"}")

    -- 38. GET /search with encoded query
    insert(reqs, "GET /search?q=foo%20bar%26baz&page=1&limit=20 HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n")

    -- 39. DELETE /users/0 (invalid)
    insert(reqs, "DELETE /users/0 HTTP/1.1\r\nHost: localhost:8080\r\nAuthorization: Bearer del_token\r\n\r\n")

    -- 40. GET /comments/42
    insert(reqs, "GET /comments/42 HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\nCookie: session=mysession\r\n\r\n")

    -- 41. PUT /users/3
    insert(reqs, "PUT /users/3 HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\n\r\n{\"name\":\"Charlie Updated\"}")

    -- 42. GET /search with no query
    insert(reqs, "GET /search HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n")

    -- 43. POST /login with empty body
    insert(reqs, "POST /login HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\n\r\n{}")

    -- 44. GET /files/deep/nested/path/to/file.txt
    insert(reqs, "GET /files/deep/nested/path/to/file.txt HTTP/1.1\r\nHost: localhost:8080\r\n\r\n")

    -- 45. POST /users with missing name
    insert(reqs, "POST /users HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\n\r\n{\"age\":25}")

    -- 46. GET /users/3
    insert(reqs, "GET /users/3 HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\nIf-None-Match: \"abc123\"\r\n\r\n")

    -- 47. POST /upload large body
    insert(reqs, "POST /upload HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/octet-stream\r\nContent-Length: 100\r\n\r\n" .. string.rep("X", 100))

    -- 48. GET /headers with many headers
    insert(reqs, "GET /headers HTTP/1.1\r\nHost: localhost:8080\r\nUser-Agent: CustomBot/2.0\r\nAccept: */*\r\nX-Forwarded-For: 192.168.1.1\r\nX-Request-Id: req-12345\r\nX-Correlation-Id: corr-67890\r\n\r\n")

    -- 49. PUT /users/2 with complex JSON
    insert(reqs, "PUT /users/2 HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\n\r\n{\"name\":\"Bob Updated\",\"email\":\"bob_new@test.com\",\"roles\":[\"admin\",\"user\"]}")

    -- 50. GET /api/v1/items/10
    insert(reqs, "GET /api/v1/items/10 HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\nCache-Control: max-age=3600\r\n\r\n")

    return reqs
end

-- =========================================================================
-- Additional workload: URL encoding/decoding stress
-- =========================================================================
function url_encode_decode_workload(iterations)
    local test_strings = {
        "hello world",
        "foo=bar&baz=qux",
        "name=John Doe&city=New York",
        "/path/to/resource?key=value&other=123",
        "special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?",
        "unicode-like: cafe\tbar\nnewline",
        "email=user@domain.com&password=p@ss w0rd!",
        "query=SELECT * FROM users WHERE id=1",
        "path=/api/v2/users/123/posts?page=1&limit=10",
        "data=base64+encoded/data==&format=raw"
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_strings do
            local encoded = url_encode(test_strings[i])
            local decoded = url_decode(encoded)
            checksum = checksum + #encoded + #decoded
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: JSON encode/decode stress
-- =========================================================================
function json_codec_workload(iterations)
    local test_objects = {
        { id = 1, name = "Alice", active = true, score = 95.5 },
        { items = { 1, 2, 3, 4, 5 }, total = 15 },
        { nested = { deep = { value = "found" } }, arr = { "a", "b", "c" } },
        { empty_arr = {}, flag = false },
        { message = "Hello \"World\"", path = "/foo/bar" },
        { numbers = { 0, -1, 3.14, 1000000, 0.001 } },
        { mixed = { 1, "two", true, { four = 4 } } },
        { tags = { "lua", "benchmark", "http", "json" }, count = 4 },
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_objects do
            local encoded = json_encode(test_objects[i])
            local decoded = json_decode(encoded)
            checksum = checksum + #encoded
            if type(decoded) == "table" then
                -- count keys
                local n = 0
                for _ in next, decoded do n = n + 1 end
                checksum = checksum + n
            end
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: header parsing stress
-- =========================================================================
function header_parse_workload(iterations)
    local raw_headers = {
        "Content-Type: application/json\r\nContent-Length: 256\r\nX-Request-Id: abc123\r\n",
        "Accept: text/html, application/xhtml+xml, application/xml;q=0.9\r\nAccept-Language: en-US,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\n",
        "Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0\r\nCookie: session=abc; theme=dark; lang=en\r\n",
        "Cache-Control: no-cache, no-store, must-revalidate\r\nPragma: no-cache\r\nExpires: 0\r\nX-Powered-By: Luau\r\n",
        "Host: www.example.com:443\r\nConnection: keep-alive\r\nUpgrade-Insekure-Requests: 1\r\nDNT: 1\r\n",
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #raw_headers do
            local h = create_headers()
            local raw = raw_headers[i]
            local pos = 1
            while pos <= #raw do
                local eol = find(raw, "\r\n", pos, true)
                if not eol then break end
                local line = sub(raw, pos, eol - 1)
                local colon = find(line, ":", 1, true)
                if colon then
                    local name = sub(line, 1, colon - 1)
                    local value = gsub(sub(line, colon + 1), "^%s+", "")
                    headers_add(h, name, value)
                end
                pos = eol + 2
            end
            checksum = checksum + #h._order
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: routing stress
-- =========================================================================
function routing_workload(iterations)
    local router = create_router()
    -- Add many routes
    router_add(router, "GET", "/", function() end)
    router_add(router, "GET", "/users", function() end)
    router_add(router, "GET", "/users/:id", function() end)
    router_add(router, "POST", "/users", function() end)
    router_add(router, "PUT", "/users/:id", function() end)
    router_add(router, "DELETE", "/users/:id", function() end)
    router_add(router, "GET", "/posts", function() end)
    router_add(router, "GET", "/posts/:id", function() end)
    router_add(router, "GET", "/posts/:id/comments", function() end)
    router_add(router, "POST", "/posts/:id/comments", function() end)
    router_add(router, "GET", "/api/v1/items", function() end)
    router_add(router, "GET", "/api/v1/items/:id", function() end)
    router_add(router, "GET", "/api/v2/items", function() end)
    router_add(router, "GET", "/api/v2/items/:id", function() end)
    router_add(router, "GET", "/files/*", function() end)
    router_add(router, "GET", "/search", function() end)
    router_add(router, "GET", "/health", function() end)
    router_add(router, "GET", "/admin/dashboard", function() end)
    router_add(router, "GET", "/admin/users", function() end)
    router_add(router, "GET", "/admin/users/:id", function() end)

    local test_paths = {
        { "GET", "/" },
        { "GET", "/users" },
        { "GET", "/users/42" },
        { "POST", "/users" },
        { "PUT", "/users/7" },
        { "DELETE", "/users/3" },
        { "GET", "/posts" },
        { "GET", "/posts/10" },
        { "GET", "/posts/5/comments" },
        { "GET", "/api/v1/items" },
        { "GET", "/api/v1/items/99" },
        { "GET", "/api/v2/items/1" },
        { "GET", "/files/path/to/file.txt" },
        { "GET", "/search" },
        { "GET", "/health" },
        { "GET", "/admin/dashboard" },
        { "GET", "/admin/users/15" },
        { "GET", "/nonexistent" },
    }

    local matches = 0
    for iter = 1, iterations do
        for i = 1, #test_paths do
            local handler = router_match(router, test_paths[i][1], test_paths[i][2])
            if handler then matches = matches + 1 end
        end
    end
    return matches
end

-- =========================================================================
-- Additional workload: query string parsing stress
-- =========================================================================
function query_string_workload(iterations)
    local test_queries = {
        "q=hello&page=1&limit=10",
        "name=John+Doe&email=john%40example.com&age=30",
        "filter=active&sort=created_at&order=desc&page=3&per_page=25",
        "ids=1,2,3,4,5&expand=true&fields=id,name,email",
        "search=foo+bar+baz&category=tech&min_price=10&max_price=100&in_stock=true",
        "a=1&b=2&c=3&d=4&e=5&f=6&g=7&h=8&i=9&j=10",
        "token=abc123xyz&redirect=/dashboard&remember=true",
        "q=SELECT+*+FROM+users&format=json&pretty=true",
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_queries do
            local parsed = parse_query_string(test_queries[i])
            local count = 0
            for _ in next, parsed do count = count + 1 end
            checksum = checksum + count
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: cookie parsing stress
-- =========================================================================
function cookie_workload(iterations)
    local test_cookies = {
        "session=abc123; user=alice; theme=dark",
        "id=12345; token=eyJhbG; pref=compact; lang=en-US; tz=America/New_York",
        "a=1; b=2; c=3; d=4; e=5; f=6; g=7; h=8",
        "_ga=GA1.2.123456; _gid=GA1.2.654321; _fbp=fb.1.123",
        "session=s%3Aabc123.signature; csrf=token123; remember=true",
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_cookies do
            local cookies = parse_cookies(test_cookies[i])
            local count = 0
            for _ in next, cookies do count = count + 1 end
            checksum = checksum + count
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: response building stress
-- =========================================================================
function response_build_workload(iterations)
    local checksum = 0
    for iter = 1, iterations do
        -- Build various responses
        for code = 200, 204 do
            local res = create_response()
            response_set_status(res, code)
            headers_set(res.headers, "Content-Type", "application/json")
            headers_set(res.headers, "X-Request-Id", "req-" .. tostring(iter))
            headers_set(res.headers, "Cache-Control", "no-cache")
            local body = json_encode({ status = code, iteration = iter })
            response_set_body(res, body, "application/json")
            local serialized = response_serialize(res)
            checksum = checksum + #serialized
        end
        -- Error responses
        for _, code in next, { 400, 401, 403, 404, 500 } do
            local res = create_response()
            response_set_status(res, code)
            local body = json_encode({ error = get_status_text(code), code = code })
            response_set_body(res, body, "application/json")
            local serialized = response_serialize(res)
            checksum = checksum + #serialized
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: content negotiation stress
-- =========================================================================
function content_negotiation_workload(iterations)
    local accept_headers = {
        "application/json",
        "text/html, application/xhtml+xml, application/xml;q=0.9, */*;q=0.8",
        "text/plain",
        "application/json;q=0.9, text/html;q=0.8, text/plain;q=0.7",
        "image/webp, image/png, image/*;q=0.8, */*;q=0.5",
        "*/*",
        "text/html;q=1.0, application/json;q=0.9",
        "application/xml, application/json;q=0.9, text/plain;q=0.5",
    }
    local available = { "application/json", "text/html", "text/plain", "application/xml" }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #accept_headers do
            local chosen = negotiate_content_type(accept_headers[i], available)
            checksum = checksum + #chosen
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: multipart parsing stress
-- =========================================================================
function multipart_workload(iterations)
    local boundary = "----WebKitFormBoundary7MA4YWxkTrZu0gW"
    local test_body = "------WebKitFormBoundary7MA4YWxkTrZu0gW\r\n"
        .. "Content-Disposition: form-data; name=\"username\"\r\n\r\n"
        .. "testuser\r\n"
        .. "------WebKitFormBoundary7MA4YWxkTrZu0gW\r\n"
        .. "Content-Disposition: form-data; name=\"email\"\r\n\r\n"
        .. "test@example.com\r\n"
        .. "------WebKitFormBoundary7MA4YWxkTrZu0gW\r\n"
        .. "Content-Disposition: form-data; name=\"file\"; filename=\"test.txt\"\r\n"
        .. "Content-Type: text/plain\r\n\r\n"
        .. "This is the file content for testing purposes.\r\n"
        .. "------WebKitFormBoundary7MA4YWxkTrZu0gW\r\n"
        .. "Content-Disposition: form-data; name=\"description\"\r\n\r\n"
        .. "A test upload with multiple fields\r\n"
        .. "------WebKitFormBoundary7MA4YWxkTrZu0gW--\r\n"

    local checksum = 0
    for iter = 1, iterations do
        local parts = parse_multipart(test_body, "----WebKitFormBoundary7MA4YWxkTrZu0gW")
        checksum = checksum + #parts
        for i = 1, #parts do
            checksum = checksum + #parts[i].body
            if parts[i].name then checksum = checksum + #parts[i].name end
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: template rendering stress
-- =========================================================================
function template_workload(iterations)
    local templates = {
        "<html><head><title>{{title}}</title></head><body><h1>{{heading}}</h1><p>{{content}}</p></body></html>",
        "Hello {{user.name}}, your email is {{user.email}}. You have {{count}} messages.",
        "<div class=\"card\"><h2>{{title}}</h2><p>{{description}}</p><span>{{author}}</span></div>",
        "API Response: {\"status\": {{status}}, \"message\": \"{{message}}\", \"data\": \"{{data}}\"}",
        "<tr><td>{{id}}</td><td>{{name}}</td><td>{{email}}</td><td>{{role}}</td></tr>",
    }
    local contexts = {
        { title = "Home Page", heading = "Welcome", content = "This is the home page content." },
        { user = { name = "Alice", email = "alice@test.com" }, count = "42" },
        { title = "Product Card", description = "A great product for everyone", author = "Admin" },
        { status = "200", message = "Success", data = "result_data_here" },
        { id = "1", name = "Bob Smith", email = "bob@test.com", role = "admin" },
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #templates do
            local ctx = contexts[((i - 1) % #contexts) + 1]
            local rendered = template_render(templates[i], ctx)
            checksum = checksum + #rendered
        end
        -- Also test loop rendering
        local list_tmpl = "<li>{{item.name}} ({{item.id}})</li>"
        local list_ctx = {
            items = {
                { id = "1", name = "Item One" },
                { id = "2", name = "Item Two" },
                { id = "3", name = "Item Three" },
                { id = "4", name = "Item Four" },
                { id = "5", name = "Item Five" },
            }
        }
        local list_result = template_render_loop(list_tmpl, list_ctx, "items", "item")
        checksum = checksum + #list_result
    end
    return checksum
end

-- =========================================================================
-- Additional workload: base64 encode/decode stress
-- =========================================================================
function base64_workload(iterations)
    local test_strings = {
        "Hello, World!",
        "username:password",
        "The quick brown fox jumps over the lazy dog",
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9",
        "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        string.rep("benchmark data ", 10),
        "special: !@#$%^&*()_+-=[]{}|;':\",./<>?",
        string.rep("a", 100),
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_strings do
            local encoded = base64_encode(test_strings[i])
            local decoded = base64_decode(encoded)
            checksum = checksum + #encoded + #decoded
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: chunked transfer encoding stress
-- =========================================================================
function chunked_workload(iterations)
    local test_bodies = {
        "Short body",
        string.rep("Hello World! ", 20),
        json_encode({ users = { { id = 1, name = "Alice" }, { id = 2, name = "Bob" } }, total = 2 }),
        string.rep("0123456789", 50),
        "<html><body><h1>Hello</h1><p>" .. string.rep("content ", 30) .. "</p></body></html>",
    }
    local chunk_sizes = { 8, 16, 32, 64, 128 }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_bodies do
            local cs = chunk_sizes[((i - 1) % #chunk_sizes) + 1]
            local encoded = encode_chunked(test_bodies[i], cs)
            local decoded = decode_chunked(encoded)
            checksum = checksum + #encoded + #decoded
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: ETag and caching stress
-- =========================================================================
function etag_workload(iterations)
    local test_contents = {
        "Page content version 1",
        json_encode({ data = "response", version = 1 }),
        "<html><body>Static page</body></html>",
        string.rep("bulk data ", 50),
        "short",
        json_encode({ items = { 1, 2, 3, 4, 5 }, meta = { page = 1, total = 100 } }),
    }
    local checksum = 0
    for iter = 1, iterations do
        local cache = create_cache(50)
        for i = 1, #test_contents do
            local etag = generate_etag(test_contents[i])
            checksum = checksum + #etag
            cache_set(cache, "page_" .. tostring(i), { etag = etag, body = test_contents[i] }, 0)
        end
        -- Test cache hits/misses
        for i = 1, 10 do
            local key = "page_" .. tostring((i % #test_contents) + 1)
            local cached = cache_get(cache, key)
            if cached then checksum = checksum + #cached.etag end
        end
        -- Test eviction
        for i = 1, 60 do
            cache_set(cache, "extra_" .. tostring(i), { etag = "\"000\"", body = "x" }, 0)
        end
        checksum = checksum + cache.count
    end
    return checksum
end

-- =========================================================================
-- Additional workload: WebSocket frame building stress
-- =========================================================================
function websocket_workload(iterations)
    local test_messages = {
        "Hello",
        json_encode({ type = "message", content = "test", timestamp = 1234567890 }),
        string.rep("ping", 50),
        "a",
        json_encode({ type = "subscribe", channels = { "chat", "notifications", "updates" } }),
        string.rep("data block ", 30),
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_messages do
            local frame = build_ws_frame(test_messages[i], 1)
            checksum = checksum + #frame
            local parsed = parse_ws_frame(frame)
            if parsed then
                checksum = checksum + #parsed.payload
            end
        end
        -- Binary frames
        for i = 1, 3 do
            local binary = string.rep(char(i * 37 % 256), 200)
            local frame = build_ws_frame(binary, 2)
            checksum = checksum + #frame
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: HPACK header compression stress
-- =========================================================================
function hpack_workload(iterations)
    local test_header_sets = {
        {
            { name = ":method", value = "GET" },
            { name = ":path", value = "/" },
            { name = ":scheme", value = "https" },
            { name = "accept", value = "application/json" },
            { name = "user-agent", value = "TestClient/1.0" },
        },
        {
            { name = ":method", value = "POST" },
            { name = ":path", value = "/api/users" },
            { name = ":scheme", value = "https" },
            { name = "content-type", value = "application/json" },
            { name = "authorization", value = "Bearer token123" },
            { name = "content-length", value = "256" },
        },
        {
            { name = ":status", value = "200" },
            { name = "content-type", value = "application/json" },
            { name = "content-length", value = "1024" },
            { name = "cache-control", value = "max-age=3600" },
            { name = "etag", value = "\"abc123\"" },
            { name = "vary", value = "Accept-Encoding" },
        },
        {
            { name = ":status", value = "404" },
            { name = "content-type", value = "text/html" },
            { name = "content-length", value = "128" },
        },
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_header_sets do
            local encoded = hpack_encode_headers(test_header_sets[i])
            checksum = checksum + #encoded
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: path normalization stress
-- =========================================================================
function path_normalize_workload(iterations)
    local test_paths = {
        "/users/../admin/./dashboard",
        "/api/v1/../../v2/items",
        "///multiple///slashes///",
        "/a/b/c/d/e/f/../../g",
        "/./././normal/path",
        "/deep/nested/../../../shallow",
        "/stay/here/./please",
        "/root/sub1/sub2/../sub3/./file.txt",
        "/../../../etc/passwd",
        "/api/v1/users/./profile/../settings",
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_paths do
            local normalized = normalize_path(test_paths[i])
            checksum = checksum + #normalized
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: MIME type lookup stress
-- =========================================================================
function mime_type_workload(iterations)
    local test_files = {
        "/static/style.css",
        "/images/logo.png",
        "/scripts/app.js",
        "/data/export.json",
        "/docs/manual.pdf",
        "/fonts/roboto.woff2",
        "/media/video.mp4",
        "/archive/backup.zip",
        "/templates/index.html",
        "/data/records.csv",
        "/unknown/file.xyz",
        "/images/photo.jpeg",
        "/images/icon.svg",
        "/music/song.mp3",
        "/fonts/custom.ttf",
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_files do
            local mime = get_mime_type(test_files[i])
            checksum = checksum + #mime
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: rate limiter simulation stress
-- =========================================================================
function rate_limiter_workload(iterations)
    local checksum = 0
    for iter = 1, iterations do
        local limiter = create_rate_limiter(10, 2.0)  -- 10 capacity, 2 tokens/sec
        local allowed = 0
        local denied = 0
        local time_now = 0.0
        for req_num = 1, 50 do
            if rate_limiter_allow(limiter, time_now) then
                allowed = allowed + 1
            else
                denied = denied + 1
            end
            time_now = time_now + 0.1  -- 100ms between requests
        end
        checksum = checksum + allowed + denied * 2
    end
    return checksum
end

-- =========================================================================
-- Additional workload: RLE compression stress
-- =========================================================================
function compression_workload(iterations)
    local test_data = {
        string.rep("A", 100) .. string.rep("B", 50) .. string.rep("C", 30),
        "ABCABCABCABC",
        string.rep("X", 255) .. string.rep("Y", 200),
        "no repeats here at all!",
        string.rep("Z", 10) .. "break" .. string.rep("Z", 10),
        string.rep("AAABBB", 20),
        string.rep("1234567890", 10),
        "aaaaabbbbbcccccdddddeeeee",
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_data do
            local compressed = rle_compress(test_data[i])
            local decompressed = rle_decompress(compressed)
            checksum = checksum + #compressed + #decompressed
            -- Verify roundtrip
            if decompressed == test_data[i] then
                checksum = checksum + 1
            end
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: SSE event building stress
-- =========================================================================
function sse_workload(iterations)
    local events = {
        { data = "Hello World", event_type = "message", id = "1" },
        { data = json_encode({ user = "alice", text = "hi" }), event_type = "chat", id = "2" },
        { data = "heartbeat", event_type = "ping", id = "3" },
        { data = "line1\nline2\nline3", event_type = "multiline", id = "4" },
        { data = json_encode({ type = "update", items = { 1, 2, 3 } }), event_type = "data", id = "5" },
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #events do
            local evt = events[i]
            local built = build_sse_event(evt.data, evt.event_type, evt.id)
            checksum = checksum + #built
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: request validation stress
-- =========================================================================
function validation_workload(iterations)
    local rules = {
        { source = "body", field = "name", required = true, min_length = 2, max_length = 50 },
        { source = "body", field = "email", required = true, pattern = "@" },
        { source = "body", field = "age", required = false, min_length = 1 },
        { source = "header", field = "Authorization", required = true },
        { source = "query", field = "page", required = false },
    }
    local test_requests_for_validation = {
        { body = '{"name":"Alice","email":"alice@test.com","age":"30"}', headers = create_headers(), query = { page = "1" } },
        { body = '{"name":"B","email":"noemail"}', headers = create_headers(), query = {} },
        { body = '{"email":"test@test.com"}', headers = create_headers(), query = {} },
        { body = '{"name":"ValidName","email":"valid@email.com"}', headers = create_headers(), query = { page = "5" } },
    }
    -- Add auth header to some
    headers_set(test_requests_for_validation[1].headers, "Authorization", "Bearer token")
    headers_set(test_requests_for_validation[4].headers, "Authorization", "Bearer admin")

    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_requests_for_validation do
            local req = test_requests_for_validation[i]
            req.params = {}
            local errors = validate_request(req, rules)
            checksum = checksum + #errors
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: CSRF token stress
-- =========================================================================
function csrf_workload(iterations)
    local sessions = {
        "session_abc123",
        "session_xyz789",
        "user_session_12345",
        "admin_sess_001",
        "guest_temporary_session",
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #sessions do
            local token = generate_csrf_token(sessions[i])
            checksum = checksum + #token
            if validate_csrf_token(token, sessions[i]) then
                checksum = checksum + 1
            end
            -- Test invalid token
            if not validate_csrf_token("invalid", sessions[i]) then
                checksum = checksum + 1
            end
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: range request parsing stress
-- =========================================================================
function range_request_workload(iterations)
    local test_ranges = {
        { header = "bytes=0-499", size = 1000 },
        { header = "bytes=500-999", size = 1000 },
        { header = "bytes=500-", size = 1000 },
        { header = "bytes=-200", size = 1000 },
        { header = "bytes=0-0", size = 100 },
        { header = "bytes=0-99999", size = 500 },
        { header = nil, size = 1000 },
        { header = "invalid", size = 1000 },
    }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #test_ranges do
            local r = test_ranges[i]
            local range = parse_range_header(r.header, r.size)
            if range then
                checksum = checksum + range.start + range.finish + range.total
            else
                checksum = checksum + 1
            end
        end
    end
    return checksum
end

-- =========================================================================
-- Additional workload: logging formatter stress
-- =========================================================================
function logging_workload(iterations)
    local methods = { "GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS" }
    local paths = { "/", "/users", "/api/v1/items/5", "/search?q=test", "/files/doc.pdf" }
    local statuses = { 200, 201, 204, 301, 400, 401, 403, 404, 500 }
    local checksum = 0
    for iter = 1, iterations do
        for i = 1, #methods do
            for j = 1, #paths do
                local req = { method = methods[i], path = paths[j] }
                local res = { status = statuses[((i + j) % #statuses) + 1], body = string.rep("x", (i + j) * 10) }
                local entry = format_log_entry(req, res, 12.5 + i)
                checksum = checksum + #entry
            end
        end
    end
    return checksum
end

-- =========================================================================
-- HTTP method validation
-- =========================================================================
VALID_HTTP_METHODS = {
    GET = true,
    POST = true,
    PUT = true,
    DELETE = true,
    PATCH = true,
    OPTIONS = true,
    HEAD = true,
    TRACE = true,
    CONNECT = true,
}

function is_valid_method(method)
    return VALID_HTTP_METHODS[upper(method)] == true
end

-- =========================================================================
-- HTTP version parsing
-- =========================================================================
function parse_http_version(version_str)
    if not version_str then return 1, 1 end
    local slash = find(version_str, "/", 1, true)
    if not slash then return 1, 1 end
    local ver = sub(version_str, slash + 1)
    local dot = find(ver, ".", 1, true)
    if not dot then return tonumber(ver) or 1, 0 end
    local major = tonumber(sub(ver, 1, dot - 1)) or 1
    local minor = tonumber(sub(ver, dot + 1)) or 1
    return major, minor
end

-- =========================================================================
-- Connection management simulation
-- =========================================================================
function should_keep_alive(req)
    local connection = headers_get(req.headers, "Connection")
    if connection then
        if lower(connection) == "close" then return false end
        if lower(connection) == "keep-alive" then return true end
    end
    -- HTTP/1.1 defaults to keep-alive
    local major, minor = parse_http_version(req.version)
    return major >= 1 and minor >= 1
end

-- =========================================================================
-- Request fingerprinting (for rate limiting / abuse detection)
-- =========================================================================
function fingerprint_request(req)
    local parts = {}
    insert(parts, req.method)
    insert(parts, req.path)
    insert(parts, headers_get(req.headers, "User-Agent") or "")
    insert(parts, headers_get(req.headers, "Accept-Language") or "")
    local combined = concat(parts, "|")
    -- Simple hash
    local hash = 0
    for i = 1, #combined do
        hash = (hash * 31 + byte(combined, i)) % 4294967296
    end
    return format("%08x", hash)
end

-- =========================================================================
-- Security headers builder
-- =========================================================================
function add_security_headers(res)
    headers_set(res.headers, "X-Content-Type-Options", "nosniff")
    headers_set(res.headers, "X-Frame-Options", "DENY")
    headers_set(res.headers, "X-XSS-Protection", "1; mode=block")
    headers_set(res.headers, "Strict-Transport-Security", "max-age=31536000; includeSubDomains")
    headers_set(res.headers, "Referrer-Policy", "strict-origin-when-cross-origin")
    headers_set(res.headers, "Permissions-Policy", "camera=(), microphone=(), geolocation=()")
end

-- =========================================================================
-- Link header parser (for pagination)
-- =========================================================================
function parse_link_header(link_str)
    local links = {}
    if not link_str or link_str == "" then return links end
    local pos = 1
    while pos <= #link_str do
        local comma = find(link_str, ",", pos, true)
        local segment
        if comma then
            segment = sub(link_str, pos, comma - 1)
            pos = comma + 1
        else
            segment = sub(link_str, pos)
            pos = #link_str + 1
        end
        -- trim
        segment = gsub(segment, "^%s+", "")
        segment = gsub(segment, "%s+$", "")
        -- Extract URL from <...>
        local url_start = find(segment, "<", 1, true)
        local url_end = find(segment, ">", 1, true)
        if url_start and url_end then
            local url = sub(segment, url_start + 1, url_end - 1)
            -- Extract rel from rel="..."
            local rel_start = find(segment, 'rel="', 1, true)
            local rel = "unknown"
            if rel_start then
                local rel_end = find(segment, '"', rel_start + 5, true)
                if rel_end then
                    rel = sub(segment, rel_start + 5, rel_end - 1)
                end
            end
            links[rel] = url
        end
    end
    return links
end

-- =========================================================================
-- Build Link header for pagination
-- =========================================================================
function build_link_header(base_url, page, per_page, total)
    local last_page = math.ceil(total / per_page)
    local parts = {}
    if page > 1 then
        insert(parts, format('<%s?page=%d&per_page=%d>; rel="prev"', base_url, page - 1, per_page))
        insert(parts, format('<%s?page=1&per_page=%d>; rel="first"', base_url, per_page))
    end
    if page < last_page then
        insert(parts, format('<%s?page=%d&per_page=%d>; rel="next"', base_url, page + 1, per_page))
        insert(parts, format('<%s?page=%d&per_page=%d>; rel="last"', base_url, last_page, per_page))
    end
    return concat(parts, ", ")
end

-- =========================================================================
-- Main benchmark
-- =========================================================================
function run_benchmark()
    local fw = setup_framework()
    local test_requests = build_test_requests()
    local num_requests = #test_requests

    -- Determine iteration count to target ~200-800ms runtime
    local ITERATIONS = 10

    local t_start = clock()

    local total_status_checksum = 0
    local total_body_length = 0

    for iter = 1, ITERATIONS do
        for i = 1, num_requests do
            local res = framework_handle_request(fw, test_requests[i])
            total_status_checksum = total_status_checksum + res.status
            total_body_length = total_body_length + #res.body
        end
    end

    -- Run additional workloads
    local url_checksum = url_encode_decode_workload(500)
    local json_checksum = json_codec_workload(400)
    local header_checksum = header_parse_workload(500)
    local routing_checksum = routing_workload(800)
    local query_checksum = query_string_workload(500)
    local cookie_checksum = cookie_workload(500)
    local response_checksum = response_build_workload(150)
    local negotiation_checksum = content_negotiation_workload(500)
    local multipart_checksum = multipart_workload(300)
    local template_checksum = template_workload(400)
    local base64_checksum = base64_workload(400)
    local chunked_checksum = chunked_workload(300)
    local etag_checksum = etag_workload(200)
    local ws_checksum = websocket_workload(400)
    local hpack_checksum = hpack_workload(500)
    local path_checksum = path_normalize_workload(500)
    local mime_checksum = mime_type_workload(500)
    local ratelimit_checksum = rate_limiter_workload(300)
    local compress_checksum = compression_workload(300)
    local sse_checksum = sse_workload(500)
    local validate_checksum = validation_workload(300)
    local csrf_checksum = csrf_workload(400)
    local range_checksum = range_request_workload(500)
    local log_checksum = logging_workload(300)

    local t_end = clock()
    local elapsed = t_end - t_start

    local all_ok = true
    if total_status_checksum ~= 118250 then all_ok = false end
    if total_body_length ~= 42860 then all_ok = false end
    if url_checksum ~= 403000 then all_ok = false end
    if json_checksum ~= 142000 then all_ok = false end
    if header_checksum ~= 8000 then all_ok = false end
    if routing_checksum ~= 13600 then all_ok = false end
    if query_checksum ~= 17500 then all_ok = false end
    if cookie_checksum ~= 11000 then all_ok = false end
    if response_checksum ~= 201720 then all_ok = false end
    if negotiation_checksum ~= 53500 then all_ok = false end
    if multipart_checksum ~= 40800 then all_ok = false end
    if template_checksum ~= 215200 then all_ok = false end
    if base64_checksum ~= 433200 then all_ok = false end
    if chunked_checksum ~= 740100 then all_ok = false end
    if etag_checksum ~= 22000 then all_ok = false end
    if ws_checksum ~= 779200 then all_ok = false end
    if hpack_checksum ~= 151500 then all_ok = false end
    if path_checksum ~= 73000 then all_ok = false end
    if mime_checksum ~= 93000 then all_ok = false end
    if ratelimit_checksum ~= 24300 then all_ok = false end
    if compress_checksum ~= 373200 then all_ok = false end
    if sse_checksum ~= 124000 then all_ok = false end
    if validate_checksum ~= 1500 then all_ok = false end
    if csrf_checksum ~= 36000 then all_ok = false end
    if range_checksum ~= 5198500 then all_ok = false end
    if log_checksum ~= 483900 then all_ok = false end

    if all_ok then
        print(format("HTTP benchmark: all %d iterations passed.", ITERATIONS))
    else
        print("HTTP benchmark: FAILED - checksum mismatch")
        print("  status_checksum=" .. tostring(total_status_checksum))
        print("  body_length=" .. tostring(total_body_length))
        print("  url=" .. tostring(url_checksum))
        print("  json=" .. tostring(json_checksum))
        print("  header=" .. tostring(header_checksum))
        print("  routing=" .. tostring(routing_checksum))
        print("  query=" .. tostring(query_checksum))
        print("  cookie=" .. tostring(cookie_checksum))
        print("  response=" .. tostring(response_checksum))
        print("  negotiation=" .. tostring(negotiation_checksum))
        print("  multipart=" .. tostring(multipart_checksum))
        print("  template=" .. tostring(template_checksum))
        print("  base64=" .. tostring(base64_checksum))
        print("  chunked=" .. tostring(chunked_checksum))
        print("  etag=" .. tostring(etag_checksum))
        print("  ws=" .. tostring(ws_checksum))
        print("  hpack=" .. tostring(hpack_checksum))
        print("  path=" .. tostring(path_checksum))
        print("  mime=" .. tostring(mime_checksum))
        print("  ratelimit=" .. tostring(ratelimit_checksum))
        print("  compress=" .. tostring(compress_checksum))
        print("  sse=" .. tostring(sse_checksum))
        print("  validate=" .. tostring(validate_checksum))
        print("  csrf=" .. tostring(csrf_checksum))
        print("  range=" .. tostring(range_checksum))
        print("  log=" .. tostring(log_checksum))
        error("Incorrect results")
    end
end

run_benchmark()


end

bench.runCode(test, "http")
