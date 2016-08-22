grammar Parser;


program: ID EOF;

ID  :   [a-zA-Z]+ ;
INT :   [0-9]+ ;
NEWLINE :   '\r'? '\n' ;
WS  :   [ \t]+ -> skip;
