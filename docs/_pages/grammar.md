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
    for namelist in explist do block end |
    function funcname funcbody |
    local function Name funcbody |
    local namelist [`=' explist] |
    [export] type Name [`<' GenericTypeList `>'] `=' typeannotation

laststat ::= return [explist] | break | continue

funcname ::= Name {`.' Name} [`:' Name]
funcbody ::= `(' [parlist] `)' [`:' ReturnType] block end
parlist ::= bindinglist [`,' `...'] | `...'
explist ::= {exp `,'} exp
bindinglist ::= (binding | `...') [`,' bindinglist]

subexpr ::= (asexp | unop subexpr) { binop subexpr }
prefixexp ::= NAME | '(' expr ')'
primaryexp ::= prefixexp { `.' NAME | `[' exp `]' | `:' NAME funcargs | funcargs }
asexp ::= simpleexp [`::' typeannotation]
simpleexp ::= NUMBER | STRING | NIL | true | false | ... | constructor | FUNCTION body | primaryexp
args ::=  `(' [explist] `)' | tableconstructor | String

compoundop :: `+=' | `-=' | `*=' | `/=' | `%=' | `^=' | `..='
binop ::= `+' | `-' | `*' | `/' | `^' | `%' | `..' | `<' | `<=' | `>' | `>=' | `==' | `~=' | and | or
unop ::= `-' | not | `#´

tableconstructor ::= `{' [fieldlist] `}'
fieldlist ::= field {fieldsep field} [fieldsep]
field ::= `[' exp `]' `=' exp | Name `=' exp | exp
fieldsep ::= `,' | `;'

typeannotation ::=
      nil |
      Name[`.' Name] [`<' namelist `>'] |
     `{' [PropList] `}' |
      `(' [TypeList] `)' `->` ReturnType
      `typeof` typeannotation

typeannotation ::= nil | Name[`.' Name] [ `<' typeannotation [`,' ...] `>' ] | `typeof' `(' expr `)' | `{' [PropList] `}' | [`<' GenericTypeList `>'] `(' [TypeList] `)' `->` ReturnType

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
