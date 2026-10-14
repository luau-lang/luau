-- https://github.com/luau-lang/luau/issues/2475

local Operators = table.freeze{
    ["+"] = "Addition" :: "Addition",
    ["-"] = "Substraction" :: "Substraction",
    ["/"] = "Division" :: "Division",
    ["*"] = "Multiplication" :: "Multiplication",
}

type UnaryOperationExpression = {
    Type: "UnaryOperation",
    Operation: "Negation",
    Operand: Expression
}
type BinaryOperationExpression = {
    Type: "BinaryOperation",
    Operation: index<typeof(Operators), keyof<typeof(Operators)>>,
    Left: Expression,
    Right: Expression
}

type Expression = UnaryOperationExpression | BinaryOperationExpression

local function _parseExpression(): Expression
    local right: Expression = nil :: any

    local expr: Expression = {
        Type = "BinaryOperation",
        Operation = "Multiplication",
        Left = {} :: Expression,
        Right = right,
    }

    return expr
end
