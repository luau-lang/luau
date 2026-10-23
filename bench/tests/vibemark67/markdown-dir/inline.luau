-- Inline parsing for Markdown (emphasis, code spans, links, images)

local ast = require("./ast")

local function parseInlines(text: string, refDefs: {[string]: {dest: string, title: string}}?): {ast.Node}
    local nodes: {ast.Node} = {}
    local pos = 1
    local len = #text
    local currentText = ""

    local function flushText()
        if #currentText > 0 then
            table.insert(nodes, ast.createText(currentText))
            currentText = ""
        end
    end

    while pos <= len do
        local ch = string.sub(text, pos, pos)

        if ch == "\\" and pos < len then
            local next = string.sub(text, pos + 1, pos + 1)
            if string.find("\\`*_{}[]()#+-.!|~", next, 1, true) then
                currentText ..= next
                pos += 2
            else
                currentText ..= ch
                pos += 1
            end
        elseif ch == "`" then
            local tickCount = 0
            local tickStart = pos
            while pos <= len and string.sub(text, pos, pos) == "`" do
                tickCount += 1
                pos += 1
            end
            local closer = string.rep("`", tickCount)
            local closePos = string.find(text, closer, pos, true)
            if closePos then
                local isClose = true
                if closePos + tickCount <= len and string.sub(text, closePos + tickCount, closePos + tickCount) == "`" then
                    local actualEnd = closePos
                    while actualEnd <= len do
                        actualEnd = string.find(text, closer, actualEnd, true)
                        if not actualEnd then
                            isClose = false
                            break
                        end
                        if actualEnd + tickCount > len or string.sub(text, actualEnd + tickCount, actualEnd + tickCount) ~= "`" then
                            closePos = actualEnd
                            break
                        end
                        actualEnd += 1
                    end
                end
                if isClose and closePos then
                    flushText()
                    local code = string.sub(text, pos, closePos - 1)
                    code = string.gsub(code, "\n", " ")
                    if #code > 1 and string.sub(code, 1, 1) == " " and string.sub(code, -1) == " " then
                        code = string.sub(code, 2, -2)
                    end
                    table.insert(nodes, ast.createCode(code))
                    pos = closePos + tickCount
                else
                    currentText ..= string.sub(text, tickStart, tickStart + tickCount - 1)
                end
            else
                currentText ..= string.sub(text, tickStart, tickStart + tickCount - 1)
            end
        elseif (ch == "*" or ch == "_") then
            local delimChar = ch
            local delimCount = 0
            local delimStart = pos
            while pos <= len and string.sub(text, pos, pos) == delimChar do
                delimCount += 1
                pos += 1
            end

            local canOpen = pos <= len and string.sub(text, pos, pos) ~= " "
            local canClose = delimStart > 1 and string.sub(text, delimStart - 1, delimStart - 1) ~= " "

            if delimCount >= 2 and canOpen then
                local closePattern = string.rep(delimChar, 2)
                local closePos = string.find(text, closePattern, pos, true)
                if closePos then
                    flushText()
                    local innerText = string.sub(text, pos, closePos - 1)
                    local strong = ast.createStrong()
                    local innerNodes = parseInlines(innerText, refDefs)
                    for _, n in innerNodes do
                        ast.addChild(strong, n)
                    end
                    table.insert(nodes, strong)
                    pos = closePos + 2
                    if delimCount > 2 then
                        currentText ..= string.rep(delimChar, delimCount - 2)
                    end
                else
                    currentText ..= string.rep(delimChar, delimCount)
                end
            elseif delimCount >= 1 and canOpen then
                local closePos = string.find(text, delimChar, pos, true)
                if closePos then
                    flushText()
                    local innerText = string.sub(text, pos, closePos - 1)
                    local em = ast.createEmphasis()
                    local innerNodes = parseInlines(innerText, refDefs)
                    for _, n in innerNodes do
                        ast.addChild(em, n)
                    end
                    table.insert(nodes, em)
                    pos = closePos + 1
                    if delimCount > 1 then
                        currentText ..= string.rep(delimChar, delimCount - 1)
                    end
                else
                    currentText ..= string.rep(delimChar, delimCount)
                end
            else
                currentText ..= string.rep(delimChar, delimCount)
            end
        elseif ch == "[" or (ch == "!" and pos < len and string.sub(text, pos + 1, pos + 1) == "[") then
            local isImage = ch == "!"
            local labelStart = if isImage then pos + 2 else pos + 1
            local bracketDepth = 1
            local labelEnd = labelStart
            while labelEnd <= len and bracketDepth > 0 do
                local bc = string.sub(text, labelEnd, labelEnd)
                if bc == "[" then bracketDepth += 1
                elseif bc == "]" then bracketDepth -= 1
                end
                if bracketDepth > 0 then labelEnd += 1 end
            end

            if bracketDepth == 0 and labelEnd <= len then
                local label = string.sub(text, labelStart, labelEnd - 1)
                local afterLabel = labelEnd + 1
                if afterLabel <= len and string.sub(text, afterLabel, afterLabel) == "(" then
                    local parenEnd = string.find(text, ")", afterLabel, true)
                    if parenEnd then
                        local linkContent = string.sub(text, afterLabel + 1, parenEnd - 1)
                        local dest = linkContent
                        local title = ""
                        local titleMatch = string.match(linkContent, '^(.-)%s+"(.-)"$')
                        if titleMatch then
                            dest = titleMatch
                            title = string.match(linkContent, '"(.-)"$') or ""
                        end
                        dest = string.gsub(dest, "^%s+", "")
                        dest = string.gsub(dest, "%s+$", "")
                        if string.sub(dest, 1, 1) == "<" and string.sub(dest, -1) == ">" then
                            dest = string.sub(dest, 2, -2)
                        end

                        flushText()
                        local node
                        if isImage then
                            node = ast.createImage(dest, title)
                            ast.addChild(node, ast.createText(label))
                        else
                            node = ast.createLink(dest, title)
                            local linkInlines = parseInlines(label, refDefs)
                            for _, n in linkInlines do
                                ast.addChild(node, n)
                            end
                        end
                        table.insert(nodes, node)
                        pos = parenEnd + 1
                    else
                        currentText ..= ch
                        pos = if isImage then pos + 1 else pos + 1
                    end
                elseif afterLabel <= len and string.sub(text, afterLabel, afterLabel) == "[" then
                    local refEnd = string.find(text, "]", afterLabel + 1, true)
                    if refEnd then
                        local refLabel = string.sub(text, afterLabel + 1, refEnd - 1)
                        if refLabel == "" then refLabel = label end
                        local ref = refDefs and refDefs[string.lower(refLabel)]
                        if ref then
                            flushText()
                            local node
                            if isImage then
                                node = ast.createImage(ref.dest, ref.title)
                                ast.addChild(node, ast.createText(label))
                            else
                                node = ast.createLink(ref.dest, ref.title)
                                local linkInlines = parseInlines(label, refDefs)
                                for _, n in linkInlines do
                                    ast.addChild(node, n)
                                end
                            end
                            table.insert(nodes, node)
                            pos = refEnd + 1
                        else
                            currentText ..= ch
                            pos += 1
                        end
                    else
                        currentText ..= ch
                        pos += 1
                    end
                else
                    local ref = refDefs and refDefs[string.lower(label)]
                    if ref then
                        flushText()
                        local node
                        if isImage then
                            node = ast.createImage(ref.dest, ref.title)
                            ast.addChild(node, ast.createText(label))
                        else
                            node = ast.createLink(ref.dest, ref.title)
                            local linkInlines = parseInlines(label, refDefs)
                            for _, n in linkInlines do
                                ast.addChild(node, n)
                            end
                        end
                        table.insert(nodes, node)
                        pos = afterLabel
                    else
                        currentText ..= ch
                        pos += 1
                    end
                end
            else
                currentText ..= ch
                pos += 1
            end
        elseif ch == "&" then
            local entity = string.match(text, "^&(#?%w+);", pos)
            if entity then
                local decoded = ""
                if string.sub(entity, 1, 1) == "#" then
                    local numStr = string.sub(entity, 2)
                    local num: number?
                    if string.sub(numStr, 1, 1) == "x" or string.sub(numStr, 1, 1) == "X" then
                        num = tonumber(string.sub(numStr, 2), 16)
                    else
                        num = tonumber(numStr)
                    end
                    if num and num > 0 and num < 128 then
                        decoded = string.char(num)
                    else
                        decoded = "&" .. entity .. ";"
                    end
                else
                    local entities: {[string]: string} = {
                        amp = "&", lt = "<", gt = ">", quot = '"',
                        apos = "'", nbsp = " ", copy = "(c)", reg = "(r)",
                    }
                    decoded = entities[entity] or ("&" .. entity .. ";")
                end
                currentText ..= decoded
                pos += #entity + 2
            else
                currentText ..= ch
                pos += 1
            end
        elseif ch == "<" then
            local autolink = string.match(text, "^<([%w]+://[^>]+)>", pos)
            if autolink then
                flushText()
                local link = ast.createLink(autolink, "")
                ast.addChild(link, ast.createText(autolink))
                table.insert(nodes, link)
                pos += #autolink + 2
            else
                local emailLink = string.match(text, "^<([%w%.%-]+@[%w%.%-]+)>", pos)
                if emailLink then
                    flushText()
                    local link = ast.createLink("mailto:" .. emailLink, "")
                    ast.addChild(link, ast.createText(emailLink))
                    table.insert(nodes, link)
                    pos += #emailLink + 2
                else
                    currentText ..= ch
                    pos += 1
                end
            end
        elseif ch == "\n" then
            if #currentText >= 2 and string.sub(currentText, -2) == "  " then
                currentText = string.sub(currentText, 1, -3)
                flushText()
                table.insert(nodes, ast.createHardBreak())
            else
                flushText()
                table.insert(nodes, ast.createSoftBreak())
            end
            pos += 1
        else
            currentText ..= ch
            pos += 1
        end
    end

    flushText()
    return nodes
end

return {
    parseInlines = parseInlines,
}
