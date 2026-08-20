-- Block-level parsing for Markdown

local ast = require("./ast")

local function trimRight(s: string): string
    return (string.gsub(s, "%s+$", ""))
end

local function trimLeft(s: string): string
    return (string.gsub(s, "^%s+", ""))
end

local function trim(s: string): string
    return trimLeft(trimRight(s))
end

local function startsWith(s: string, prefix: string): boolean
    return string.sub(s, 1, #prefix) == prefix
end

local function countLeading(line: string, char: string): number
    local count = 0
    for i = 1, #line do
        if string.sub(line, i, i) == char then
            count += 1
        else
            break
        end
    end
    return count
end

local function isBlankLine(line: string): boolean
    return trim(line) == ""
end

local function isThematicBreak(line: string): boolean
    local stripped = string.gsub(trim(line), " ", "")
    if #stripped < 3 then return false end
    local ch = string.sub(stripped, 1, 1)
    if ch ~= "*" and ch ~= "-" and ch ~= "_" then return false end
    for i = 1, #stripped do
        if string.sub(stripped, i, i) ~= ch then return false end
    end
    return true
end

local function isATXHeading(line: string): (number?, string?)
    local trimmed = trimLeft(line)
    local indent = #line - #trimmed
    if indent > 3 then return nil, nil end
    local level = countLeading(trimmed, "#")
    if level < 1 or level > 6 then return nil, nil end
    if #trimmed > level and string.sub(trimmed, level + 1, level + 1) ~= " " then
        return nil, nil
    end
    local content = trim(string.sub(trimmed, level + 1))
    content = string.gsub(content, "%s+#+%s*$", "")
    content = string.gsub(content, "^#+$", "")
    return level, content
end

local function isFencedCodeStart(line: string): (string?, string?)
    local trimmed = trimLeft(line)
    local indent = #line - #trimmed
    if indent > 3 then return nil, nil end
    local ch = string.sub(trimmed, 1, 1)
    if ch ~= "`" and ch ~= "~" then return nil, nil end
    local fenceLen = countLeading(trimmed, ch)
    if fenceLen < 3 then return nil, nil end
    local fence = string.rep(ch, fenceLen)
    local info = trim(string.sub(trimmed, fenceLen + 1))
    if ch == "`" and string.find(info, "`") then return nil, nil end
    return fence, info
end

local function isFencedCodeEnd(line: string, fence: string): boolean
    local trimmed = trimLeft(line)
    local indent = #line - #trimmed
    if indent > 3 then return false end
    local ch = string.sub(fence, 1, 1)
    local fenceLen = countLeading(trimmed, ch)
    if fenceLen < #fence then return false end
    local rest = trim(string.sub(trimmed, fenceLen + 1))
    return rest == ""
end

local function isBlockquoteLine(line: string): (boolean, string)
    local trimmed = trimLeft(line)
    local indent = #line - #trimmed
    if indent > 3 then return false, line end
    if string.sub(trimmed, 1, 1) == ">" then
        local rest = string.sub(trimmed, 2)
        if string.sub(rest, 1, 1) == " " then
            rest = string.sub(rest, 2)
        end
        return true, rest
    end
    return false, line
end

local function isOrderedListItem(line: string): (number?, string?)
    local trimmed = trimLeft(line)
    local indent = #line - #trimmed
    if indent > 3 then return nil, nil end
    local num, rest = string.match(trimmed, "^(%d+)([%.%)])")
    if num and rest then
        local afterMarker = string.sub(trimmed, #num + 2)
        if afterMarker == "" or string.sub(afterMarker, 1, 1) == " " then
            local content = if #afterMarker > 0 then string.sub(afterMarker, 2) else ""
            return tonumber(num), content
        end
    end
    return nil, nil
end

local function isBulletListItem(line: string): (string?, string?)
    local trimmed = trimLeft(line)
    local indent = #line - #trimmed
    if indent > 3 then return nil, nil end
    local ch = string.sub(trimmed, 1, 1)
    if ch == "-" or ch == "*" or ch == "+" then
        if #trimmed == 1 then return ch, "" end
        if string.sub(trimmed, 2, 2) == " " then
            return ch, string.sub(trimmed, 3)
        end
    end
    return nil, nil
end

local function isSetextHeading(line: string): number?
    local trimmed = trim(line)
    if #trimmed == 0 then return nil end
    local ch = string.sub(trimmed, 1, 1)
    if ch ~= "=" and ch ~= "-" then return nil end
    for i = 1, #trimmed do
        if string.sub(trimmed, i, i) ~= ch then return nil end
    end
    return if ch == "=" then 1 else 2
end

export type BlockParser = {
    lines: {string},
    pos: number,
    refDefs: {[string]: {dest: string, title: string}},
}

local function createBlockParser(text: string): BlockParser
    local lines: {string} = {}
    local current = 1
    while current <= #text do
        local nlPos = string.find(text, "\n", current, true)
        if nlPos then
            table.insert(lines, string.sub(text, current, nlPos - 1))
            current = nlPos + 1
        else
            table.insert(lines, string.sub(text, current))
            current = #text + 1
        end
    end
    return {
        lines = lines,
        pos = 1,
        refDefs = {},
    }
end

local function parseLinkRef(parser: BlockParser, startPos: number): boolean
    local line = parser.lines[startPos]
    if not line then return false end
    local label = string.match(line, "^%[([^%]]+)%]:%s*(.+)")
    if not label then return false end
    local rest = string.match(line, "^%[[^%]]+%]:%s*(.+)")
    if not rest then return false end
    local dest = string.match(rest, "^<?([^>%s]+)>?")
    if not dest then return false end
    parser.refDefs[string.lower(label)] = {dest = dest, title = ""}
    parser.pos = startPos + 1
    return true
end

local function parseBlocks(parser: BlockParser): ast.Node
    local doc = ast.createDocument()

    while parser.pos <= #parser.lines do
        local line = parser.lines[parser.pos]

        if isBlankLine(line) then
            parser.pos += 1
        elseif isThematicBreak(line) then
            ast.addChild(doc, ast.createThematicBreak())
            parser.pos += 1
        else
            local level, content = isATXHeading(line)
            if level and content then
                ast.addChild(doc, ast.createHeading(level, content))
                parser.pos += 1
            else
                local fence, info = isFencedCodeStart(line)
                if fence and info then
                    parser.pos += 1
                    local codeLines: {string} = {}
                    while parser.pos <= #parser.lines do
                        local codeLine = parser.lines[parser.pos]
                        if isFencedCodeEnd(codeLine, fence) then
                            parser.pos += 1
                            break
                        end
                        table.insert(codeLines, codeLine)
                        parser.pos += 1
                    end
                    ast.addChild(doc, ast.createCodeBlock(table.concat(codeLines, "\n"), info))
                else
                    local isQuote, quoteContent = isBlockquoteLine(line)
                    if isQuote then
                        local quoteLines: {string} = {}
                        while parser.pos <= #parser.lines do
                            local ql = parser.lines[parser.pos]
                            local isQ, qc = isBlockquoteLine(ql)
                            if isQ then
                                table.insert(quoteLines, qc)
                                parser.pos += 1
                            elseif not isBlankLine(ql) then
                                table.insert(quoteLines, ql)
                                parser.pos += 1
                            else
                                break
                            end
                        end
                        local subParser = createBlockParser(table.concat(quoteLines, "\n"))
                        local subDoc = parseBlocks(subParser)
                        local bq = ast.createBlockquote()
                        for _, child in subDoc.children do
                            ast.addChild(bq, child)
                        end
                        ast.addChild(doc, bq)
                    else
                        local bullet, bulletContent = isBulletListItem(line)
                        if bullet then
                            local list = ast.createList(false, 1)
                            while parser.pos <= #parser.lines do
                                local ll = parser.lines[parser.pos]
                                local b, bc = isBulletListItem(ll)
                                if b then
                                    local item = ast.createListItem()
                                    local itemLines = {bc}
                                    parser.pos += 1
                                    while parser.pos <= #parser.lines do
                                        local il = parser.lines[parser.pos]
                                        if isBlankLine(il) then
                                            list.tight = false
                                            parser.pos += 1
                                            break
                                        end
                                        local nb, _ = isBulletListItem(il)
                                        if nb then break end
                                        table.insert(itemLines, trimLeft(il))
                                        parser.pos += 1
                                    end
                                    local itemParser = createBlockParser(table.concat(itemLines, "\n"))
                                    local itemDoc = parseBlocks(itemParser)
                                    for _, child in itemDoc.children do
                                        ast.addChild(item, child)
                                    end
                                    ast.addChild(list, item)
                                else
                                    break
                                end
                            end
                            ast.addChild(doc, list)
                        else
                            local num, olContent = isOrderedListItem(line)
                            if num then
                                local list = ast.createList(true, num)
                                while parser.pos <= #parser.lines do
                                    local ll = parser.lines[parser.pos]
                                    local n, oc = isOrderedListItem(ll)
                                    if n then
                                        local item = ast.createListItem()
                                        local itemLines = {oc}
                                        parser.pos += 1
                                        while parser.pos <= #parser.lines do
                                            local il = parser.lines[parser.pos]
                                            if isBlankLine(il) then
                                                list.tight = false
                                                parser.pos += 1
                                                break
                                            end
                                            local nn, _ = isOrderedListItem(il)
                                            if nn then break end
                                            table.insert(itemLines, trimLeft(il))
                                            parser.pos += 1
                                        end
                                        local itemParser = createBlockParser(table.concat(itemLines, "\n"))
                                        local itemDoc = parseBlocks(itemParser)
                                        for _, child in itemDoc.children do
                                            ast.addChild(item, child)
                                        end
                                        ast.addChild(list, item)
                                    else
                                        break
                                    end
                                end
                                ast.addChild(doc, list)
                            else
                                if not parseLinkRef(parser, parser.pos) then
                                    local paraLines: {string} = {}
                                    while parser.pos <= #parser.lines do
                                        local pl = parser.lines[parser.pos]
                                        if isBlankLine(pl) then break end
                                        if isThematicBreak(pl) then break end
                                        local h, _ = isATXHeading(pl)
                                        if h then break end
                                        local f, _ = isFencedCodeStart(pl)
                                        if f then break end
                                        local bul, _ = isBulletListItem(pl)
                                        if bul then break end
                                        local oNum, _ = isOrderedListItem(pl)
                                        if oNum then break end

                                        if #paraLines > 0 then
                                            local setextLevel = isSetextHeading(pl)
                                            if setextLevel then
                                                local headingContent = table.concat(paraLines, " ")
                                                ast.addChild(doc, ast.createHeading(setextLevel, trim(headingContent)))
                                                parser.pos += 1
                                                paraLines = {}
                                                break
                                            end
                                        end

                                        table.insert(paraLines, pl)
                                        parser.pos += 1
                                    end
                                    if #paraLines > 0 then
                                        local content = table.concat(paraLines, "\n")
                                        ast.addChild(doc, ast.createParagraph(trim(content)))
                                    end
                                end
                            end
                        end
                    end
                end
            end
        end
    end

    return doc
end

return {
    createBlockParser = createBlockParser,
    parseBlocks = parseBlocks,
}
