grammar Parser;


program
    : callout_decl* field_decl* method_decl*
    ;

callout_decl
    : 'callout' IDENTIFIER ';'
    ;

field_decl
    : type (IDENTIFIER | IDENTIFIER '[' INTLITERAL ']')
      (',' (IDENTIFIER | IDENTIFIER '[' INTLITERAL ']'))* ';'
    ;

method_decl
    : (type | 'void') IDENTIFIER
      '(' ((type IDENTIFIER) (',' type IDENTIFIER)*)? ')'
      block
    ;

block
    : '{' field_decl* statement* '}'
    ;

type
    : 'int' | 'boolean'
    ;

statement
    : location assign_op expr ';'
    | method_call ';'
    | 'if' '(' expr ')' block ('else' block)?
    | 'for' '(' IDENTIFIER '=' expr ',' expr (',' INTLITERAL)? ')' block
    | 'while' '(' expr ')' block
    | 'return' expr? ';'
    | 'break' ';'
    | 'continue' ';'
    ;

assign_op: '=' | '+=' | '-=' ;

method_call
    : method_name '(' (expr (',' expr)*)? ')'
    | method_name '(' (callout_arg (',' callout_arg)*)? ')'
    ;

method_name
    : IDENTIFIER
    ;

location
    : IDENTIFIER
    | IDENTIFIER '[' expr ']'
    ;

expr
    : location
    | method_call
    | literal
    | '@' IDENTIFIER
    | expr bin_op expr
    | '-' expr
    | '!' expr
    | '(' expr ')'
    | expr '?' expr ':' expr
    ;

callout_arg
    : expr
    | STRINGLITERAL
    ;

bin_op
    : arith_op
    | rel_op
    | eq_op
    | cond_op
    ;

arith_op: '+' | '-' | '*' | '/' | '%' ;

rel_op: '<' | '>' | '<=' | '>=';

eq_op: '==' | '!=' ;

cond_op: '==' | '!=';

literal
    : INTLITERAL
    | CHARLITERAL
    | BOOLEANLITERAL
    ;

CHARLITERAL: '\'' CHAR '\'';

INTLITERAL
    : DECIMAL_LITERAL
    | HEX_LITERAL
    ;

BOOLEANLITERAL: 'true' | 'false';

STRINGLITERAL: '"' CHAR* '"';

IDENTIFIER: ALPHA ALPHA_NUM* ;

fragment
ALPHA_NUM: ALPHA | DIGIT;

fragment
ALPHA: [a-zA-Z_];

fragment
CHAR
    : '\\' ('\'' | '\"' | '\\' | 't' | 'n' | 'r')
    | '\u0020'..'\u0021'
    | '\u0023'..'\u0026'
    | '\u0028'..'\u005b'
    | '\u005d'..'\u007e'
    ;

fragment
DIGIT: [0-9];

fragment
HEX_DIGIT: DIGIT | [a-fA-F];

fragment
DECIMAL_LITERAL: DIGIT DIGIT*;

fragment
HEX_LITERAL: '0x' HEX_DIGIT HEX_DIGIT*;


WS  :   [ \t\r\n]+ -> skip;
