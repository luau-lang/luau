-- HTML rendering from Markdown AST

local ast = require("./ast")
local inline = require("./inline")

local function escapeHtml(s: string): string
    s = string.gsub(s, "&", "&amp;")
    s = string.gsub(s, "<", "&lt;")
    s = string.gsub(s, ">", "&gt;")
    s = string.gsub(s, '"', "&quot;")
    return s
end

local function renderInlineNodes(nodes: {ast.Node}): string
    local out = {}
    for _, node in nodes do
        if node.type == "text" then
            table.insert(out, escapeHtml(node.content))
        elseif node.type == "code" then
            table.insert(out, "<code>")
            table.insert(out, escapeHtml(node.content))
            table.insert(out, "</code>")
        elseif node.type == "emphasis" then
            table.insert(out, "<em>")
            table.insert(out, renderInlineNodes(node.children))
            table.insert(out, "</em>")
        elseif node.type == "strong" then
            table.insert(out, "<strong>")
            table.insert(out, renderInlineNodes(node.children))
            table.insert(out, "</strong>")
        elseif node.type == "link" then
            table.insert(out, '<a href="')
            table.insert(out, escapeHtml(node.destination))
            table.insert(out, '"')
            if node.title ~= "" then
                table.insert(out, ' title="')
                table.insert(out, escapeHtml(node.title))
                table.insert(out, '"')
            end
            table.insert(out, ">")
            table.insert(out, renderInlineNodes(node.children))
            table.insert(out, "</a>")
        elseif node.type == "image" then
            table.insert(out, '<img src="')
            table.insert(out, escapeHtml(node.destination))
            table.insert(out, '" alt="')
            if #node.children > 0 then
                table.insert(out, escapeHtml(node.children[1].content))
            end
            table.insert(out, '"')
            if node.title ~= "" then
                table.insert(out, ' title="')
                table.insert(out, escapeHtml(node.title))
                table.insert(out, '"')
            end
            table.insert(out, " />")
        elseif node.type == "softbreak" then
            table.insert(out, "\n")
        elseif node.type == "hardbreak" then
            table.insert(out, "<br />\n")
        end
    end
    return table.concat(out)
end

local renderNode: (ast.Node, {[string]: {dest: string, title: string}}) -> string

function renderNode(node: ast.Node, refDefs: {[string]: {dest: string, title: string}}): string
    local out = {}

    if node.type == "document" then
        for _, child in node.children do
            table.insert(out, renderNode(child, refDefs))
        end
    elseif node.type == "paragraph" then
        table.insert(out, "<p>")
        local inlineNodes = inline.parseInlines(node.content, refDefs)
        table.insert(out, renderInlineNodes(inlineNodes))
        table.insert(out, "</p>\n")
    elseif node.type == "heading" then
        local tag = "h" .. tostring(node.level)
        table.insert(out, "<" .. tag .. ">")
        local inlineNodes = inline.parseInlines(node.content, refDefs)
        table.insert(out, renderInlineNodes(inlineNodes))
        table.insert(out, "</" .. tag .. ">\n")
    elseif node.type == "code_block" then
        if node.info ~= "" then
            table.insert(out, '<pre><code class="language-')
            table.insert(out, escapeHtml(node.info))
            table.insert(out, '">')
        else
            table.insert(out, "<pre><code>")
        end
        table.insert(out, escapeHtml(node.content))
        table.insert(out, "</code></pre>\n")
    elseif node.type == "blockquote" then
        table.insert(out, "<blockquote>\n")
        for _, child in node.children do
            table.insert(out, renderNode(child, refDefs))
        end
        table.insert(out, "</blockquote>\n")
    elseif node.type == "bullet_list" then
        table.insert(out, "<ul>\n")
        for _, child in node.children do
            table.insert(out, renderNode(child, refDefs))
        end
        table.insert(out, "</ul>\n")
    elseif node.type == "ordered_list" then
        if node.start ~= 1 then
            table.insert(out, '<ol start="' .. tostring(node.start) .. '">\n')
        else
            table.insert(out, "<ol>\n")
        end
        for _, child in node.children do
            table.insert(out, renderNode(child, refDefs))
        end
        table.insert(out, "</ol>\n")
    elseif node.type == "list_item" then
        table.insert(out, "<li>")
        for i, child in node.children do
            if child.type == "paragraph" then
                local inlineNodes = inline.parseInlines(child.content, refDefs)
                table.insert(out, renderInlineNodes(inlineNodes))
            else
                table.insert(out, renderNode(child, refDefs))
            end
        end
        table.insert(out, "</li>\n")
    elseif node.type == "thematic_break" then
        table.insert(out, "<hr />\n")
    elseif node.type == "html_block" then
        table.insert(out, node.content)
        table.insert(out, "\n")
    end

    return table.concat(out)
end

local function render(doc: ast.Node, refDefs: {[string]: {dest: string, title: string}}): string
    return renderNode(doc, refDefs)
end

return {
    render = render,
    escapeHtml = escapeHtml,
}
