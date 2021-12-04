---
permalink: /grammar
title: Grammar
toc: true
---

This is the complete syntax grammar for Luau in EBNF. More information about the terminal nodes String and Number
is available in the [syntax section](syntax).

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
    local function NAME funcbody |
    local bindinglist [`=' explist] |
    [export] type NAME [`<' GenericTypeList `>'] `=' TypeAnnotation

laststat ::= return [explist] | break | continue

funcname ::= NAME {`.' NAME} [`:' NAME]
funcbody ::= `(' [parlist] `)' [`:' ReturnType] block end
parlist ::= bindinglist [`,' `...'] | `...'

explist ::= {exp `,'} exp
namelist ::= NAME {`,' NAME}

binding ::= NAME [`:' TypeAnnotation]
bindinglist ::= binding [`,' bindinglist]

exp ::= (asexp | unop exp) { binop exp }
ifelseexp ::= if exp then exp {elseif exp then exp} else exp
prefixexp ::= NAME | '(' exp ')'
primaryexp ::= prefixexp { `.' NAME | `[' exp `]' | `:' NAME funcargs | funcargs }
asexp ::= simpleexp [`::' TypeAnnotation]
simpleexp ::= NUMBER | STRING | nil | true | false | `...' | tableconstructor | function body | primaryexp | ifelseexp
funcargs ::=  `(' [explist] `)' | tableconstructor | STRING

tableconstructor ::= `{' [fieldlist] `}'
fieldlist ::= field {fieldsep field} [fieldsep]
field ::= `[' exp `]' `=' exp | NAME `=' exp | exp
fieldsep ::= `,' | `;'

compoundop :: `+=' | `-=' | `*=' | `/=' | `%=' | `^=' | `..='
binop ::= `+' | `-' | `*' | `/' | `^' | `%' | `..' | `<' | `<=' | `>' | `>=' | `==' | `~=' | and | or
unop ::= `-' | not | `#'

SimpleTypeAnnotation ::=
    nil |
    NAME[`.' NAME] [ `<' TypeAnnotation [`,' ...] `>' ] |
    `typeof' `(' exp `)' |
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
TableProp ::= NAME `:' TypeAnnotation
TablePropOrIndexer ::= TableProp | TableIndexer
PropList ::= TablePropOrIndexer {fieldsep TablePropOrIndexer} [fieldsep]
TableTypeAnnotation ::= `{' PropList `}'
FunctionTypeAnnotation ::= [`<' GenericTypeList `>'] `(' [TypeList] `)' `->` ReturnType
```
