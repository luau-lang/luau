-- AST node types for CommonMark-like parser

export type Node = {
    type: string,
    children: {Node},
    content: string,
    level: number,
    info: string,
    tight: boolean,
    start: number,
    delimiter: string,
    title: string,
    destination: string,
    label: string,
}

local function createNode(nodeType: string): Node
    return {
        type = nodeType,
        children = {},
        content = "",
        level = 0,
        info = "",
        tight = true,
        start = 1,
        delimiter = "",
        title = "",
        destination = "",
        label = "",
    }
end

local function addChild(parent: Node, child: Node)
    table.insert(parent.children, child)
end

local function createDocument(): Node
    return createNode("document")
end

local function createParagraph(content: string): Node
    local n = createNode("paragraph")
    n.content = content
    return n
end

local function createHeading(level: number, content: string): Node
    local n = createNode("heading")
    n.level = level
    n.content = content
    return n
end

local function createCodeBlock(content: string, info: string): Node
    local n = createNode("code_block")
    n.content = content
    n.info = info
    return n
end

local function createBlockquote(): Node
    return createNode("blockquote")
end

local function createList(ordered: boolean, start: number): Node
    local n = createNode(if ordered then "ordered_list" else "bullet_list")
    n.start = start
    return n
end

local function createListItem(): Node
    return createNode("list_item")
end

local function createThematicBreak(): Node
    return createNode("thematic_break")
end

local function createText(text: string): Node
    local n = createNode("text")
    n.content = text
    return n
end

local function createEmphasis(): Node
    return createNode("emphasis")
end

local function createStrong(): Node
    return createNode("strong")
end

local function createCode(content: string): Node
    local n = createNode("code")
    n.content = content
    return n
end

local function createLink(dest: string, title: string): Node
    local n = createNode("link")
    n.destination = dest
    n.title = title
    return n
end

local function createImage(dest: string, title: string): Node
    local n = createNode("image")
    n.destination = dest
    n.title = title
    return n
end

local function createHtmlBlock(content: string): Node
    local n = createNode("html_block")
    n.content = content
    return n
end

local function createSoftBreak(): Node
    return createNode("softbreak")
end

local function createHardBreak(): Node
    return createNode("hardbreak")
end

return {
    createNode = createNode,
    addChild = addChild,
    createDocument = createDocument,
    createParagraph = createParagraph,
    createHeading = createHeading,
    createCodeBlock = createCodeBlock,
    createBlockquote = createBlockquote,
    createList = createList,
    createListItem = createListItem,
    createThematicBreak = createThematicBreak,
    createText = createText,
    createEmphasis = createEmphasis,
    createStrong = createStrong,
    createCode = createCode,
    createLink = createLink,
    createImage = createImage,
    createHtmlBlock = createHtmlBlock,
    createSoftBreak = createSoftBreak,
    createHardBreak = createHardBreak,
}
