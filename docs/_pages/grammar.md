---
permalink: /grammar
title: Grammar
toc: true
---

This is the complete syntax grammar for Luau in EBNF. More information about the terminal nodes String and Number
is available in the [syntax section](syntax).

> Note: this grammar is currently missing type pack syntax for generic arguments

```ebnf
chunk = block
block = {stat [';']} [laststat [';']]
stat = varlist '=' explist |
    var compoundop exp |
    functioncall |
    'do' block 'end' |
    'while' exp 'do' block 'end' |
    'repeat' block 'until' exp |
    'if' exp 'then' block {'elseif' exp 'then' block} ['else' block] 'end' |
    'for' binding '=' exp ',' exp [',' exp] 'do' block 'end' |
    'for' bindinglist 'in' explist 'do' block 'end' |
    'function' funcname funcbody |
    'local' 'function' NAME funcbody |
    'local' bindinglist ['=' explist] |
    ['export'] type NAME ['<' GenericTypeList '>'] '=' Type

laststat = 'return' [explist] | 'break' | 'continue'

funcname = NAME {'.' NAME} [':' NAME]
funcbody = '(' [parlist] ')' [':' ReturnType] block 'end'
parlist = bindinglist [',' '...'] | '...'

explist = {exp ','} exp
namelist = NAME {',' NAME}

binding = NAME [':' TypeAnnotation]
bindinglist = binding [',' bindinglist] (* equivalent of Lua 5.1 'namelist', except with optional type annotations *)

var = NAME | prefixexp '[' exp ']' | prefixexp '.' Name
varlist = var {',' var}
prefixexp = var | functioncall | '(' exp ')'
functioncall = prefixexp funcargs | prefixexp ':' NAME funcargs

exp = (asexp | unop exp) { binop exp }
ifelseexp = 'if' exp 'then' exp {'elseif' exp 'then' exp} 'else' exp
asexp = simpleexp ['::' Type]
simpleexp = NUMBER | STRING | 'nil' | 'true' | 'false' | '...' | tableconstructor | 'function' body | prefixexp | ifelseexp
funcargs =  '(' [explist] ')' | tableconstructor | STRING

tableconstructor = '{' [fieldlist] '}'
fieldlist = field {fieldsep field} [fieldsep]
field = '[' exp ']' '=' exp | NAME '=' exp | exp
fieldsep = ',' | ';'

compoundop :: '+=' | '-=' | '*=' | '/=' | '%=' | '^=' | '..='
binop = '+' | '-' | '*' | '/' | '^' | '%' | '..' | '<' | '<=' | '>' | '>=' | '==' | '~=' | 'and' | 'or'
unop = '-' | 'not' | '#'

SimpleType =
    'nil' |
    NAME ['.' NAME] [ '<' TypeList '>' ] |
    'typeof' '(' exp ')' |
    TableType |
    FunctionType

Type =
    SimpleType ['?'] |
    SimpleType ['|' Type] |
    SimpleType ['&' Type]

GenericTypeList = NAME ['...'] {',' NAME ['...']}
TypeList = Type [',' TypeList] | '...' Type
ReturnType = Type | '(' TypeList ')'
TableIndexer = '[' Type ']' ':' Type
TableProp = NAME ':' Type
TablePropOrIndexer = TableProp | TableIndexer
PropList = TablePropOrIndexer {fieldsep TablePropOrIndexer} [fieldsep]
TableType = '{' PropList '}'
FunctionType = ['<' GenericTypeList '>'] '(' [TypeList] ')' '->' ReturnType
```
