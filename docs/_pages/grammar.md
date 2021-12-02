---
permalink: /grammar
title: Grammar
toc: true
---

```
chunk ::= {stat [`;']} [laststat [`;']]
block ::= chunk
stat ::= varlist `=' explist |
    var compoundop exp |
    functioncall |
    do block end |
    while exp do block end |
    repeat block until exp |
    if exp then block {elseif exp then block} [else block] end |
    for binding `=' exp `,' exp [`,' exp] do block end |
    for bindinglist in explist do block end |
    function funcname funcbody |
    local function Name funcbody |
    local bindinglist [`=' explist] |
    [export] type Name [`<' GenericTypeList `>'] `=' TypeAnnotation

laststat ::= return [explist] | break | continue

funcname ::= Name {`.' Name} [`:' Name]
funcbody ::= `(' [parlist] `)' [`:' ReturnType] block end
parlist ::= bindinglist [`,' `...'] | `...'
explist ::= {exp `,'} exp
namelist ::= Name {`,' Name}

binding ::= Name [`:' TypeAnnotation]
bindinglist ::= (binding | `...') [`,' bindinglist]

subexpr ::= (asexp | unop subexpr) { binop subexpr }
ifelseexp ::= if exp then exp {elseif exp then exp} else exp
prefixexp ::= NAME | '(' expr ')'
primaryexp ::= prefixexp { `.' NAME | `[' exp `]' | `:' NAME funcargs | funcargs }
asexp ::= simpleexp [`::' TypeAnnotation]
simpleexp ::= NUMBER | STRING | NIL | true | false | ... | constructor | FUNCTION body | primaryexp | ifelseexp
funcargs ::=  `(' [explist] `)' | tableconstructor | String

compoundop :: `+=' | `-=' | `*=' | `/=' | `%=' | `^=' | `..='
binop ::= `+' | `-' | `*' | `/' | `^' | `%' | `..' | `<' | `<=' | `>' | `>=' | `==' | `~=' | and | or
unop ::= `-' | not | `#´

tableconstructor ::= `{' [fieldlist] `}'
fieldlist ::= field {fieldsep field} [fieldsep]
field ::= `[' exp `]' `=' exp | Name `=' exp | exp
fieldsep ::= `,' | `;'

SimpleTypeAnnotation ::=
    nil |
    Name[`.' Name] [ `<' TypeAnnotation [`,' ...] `>' ] |
    `typeof' `(' expr `)' |
    `{' [PropList] `}' |
    FunctionTypeAnnotation

TypeAnnotation ::=
    SimpleTypeAnnotation [`?`] |
    SimpleTypeAnnotation [`|` TypeAnnotation] |
    SimpleTypeAnnotation [`&` TypeAnnotation]

GenericTypeList ::= NAME [`...'] {`,' NAME}
TypeList ::= TypeAnnotation [`,' TypeList] | ...TypeAnnotation
ReturnType ::= TypeAnnotation | `(' TypeList `)'
TableIndexer ::= `[' TypeAnnotation `]' `:' TypeAnnotation
TableProp ::= Name `:' TypeAnnotation
TablePropOrIndexer ::= TableProp | TableIndexer
PropList ::= TablePropOrIndexer {fieldsep TablePropOrIndexer} [fieldsep]
TableTypeAnnotation ::= `{' PropList `}'
FunctionTypeAnnotation ::= [`<' GenericTypeList `>'] `(' [TypeList] `)' `->` ReturnType
```
