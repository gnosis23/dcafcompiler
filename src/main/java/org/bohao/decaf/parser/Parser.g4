grammar Parser;

@header {
package org.bohao.decaf.parser;

import org.bohao.decaf.util.Converter;
import org.bohao.decaf.ast.*;
}

@parser::members {
    private char toch(String text) {
        return Converter.toChar(text);
    }

    private boolean tobool(String text) {
        return Converter.toBool(text);
    }

    private String toString(String text) {
        return Converter.toString(text);
    }
}

program returns [ProgramNode ast]
locals [List<CalloutDeclNode> callouts, List<FieldDeclNode> fields, List<MethodDeclNode> methods]
@init {
    $callouts = new ArrayList<>();
    $fields = new ArrayList<>();
    $methods = new ArrayList<>();
}
    : (c=callout_decl   {$callouts.add($c.v);})*
      (f=field_decl     {$fields.add($f.v);})*
      (m=method_decl    {$methods.add($m.v);})*
      {$ast = new ProgramNode($callouts, $fields, $methods);}
    ;

callout_decl returns [CalloutDeclNode v]
    : 'callout' IDENTIFIER ';'
      { $v = new CalloutDeclNode($IDENTIFIER.text); }
    ;

field_decl returns [FieldDeclNode v]
locals [List<NameNode> names]
@init {
    $names = new ArrayList<>();
}
    : t=type n1=name {$names.add($n1.v);}
      (',' n2=name {$names.add($n2.v);})* ';'
      {$v = new FieldDeclNode($t.v, $names);}
    ;

name returns [NameNode v]
    : IDENTIFIER
      {$v = new VarNameNode(new VarNode($IDENTIFIER.text));}
    | IDENTIFIER '[' i=INTLITERAL ']'
      {$v = new ArrayNameNode(new VarNode($IDENTIFIER.text), new IntLiteralNode($i.text));}
    ;

method_decl returns [MethodDeclNode v]
locals [TypeNode t, List<ParamNode> params]
@init {
    $params = new ArrayList<>();
}
    : (x=type {$t = $x.v;} | 'void' {$t = new VoidTypeNode();})
      id=IDENTIFIER
      '(' (
            p1=param {$params.add($p1.v);}
            (',' p2=param {$params.add($p2.v);})*
          )? ')'
      b=block
      { $v = new MethodDeclNode($t, $id.text, $params, $b.v); }
    ;

param returns [ParamNode v]
    : t=type id=IDENTIFIER
      {$v = new ParamNode($t.v, new VarNode($id.text));}
    ;

block returns [BlockNode v]
locals [List<FieldDeclNode> fields, List<StmtNode> stmts]
@init {
    $fields = new ArrayList<>();
    $stmts = new ArrayList<>();
}
    : '{'
        (f=field_decl {$fields.add($f.v);})*
        (s=statement  {$stmts.add($s.v);})*
      '}'
      { $v = new BlockNode($fields, $stmts); }
    ;

type returns [TypeNode v]
    : 'int' {$v = new IntTypeNode();}
    | 'boolean' {$v = new BoolTypeNode();}
    ;

statement returns [StmtNode v]
locals [BlockNode b2, IntLiteralNode step, ExpNode ret]
@init {
    $b2 = null; $step = null; $ret = null;
}
    : l=location op=assign_op e=expr ';'
      {$v = new AssignStmtNode($l.v, new AssignOpNode($op.text), $e.v);}
    | m=method_call ';'
      {$v = new MethodCallStmtNode($m.v);}
    | 'if' '(' e=expr ')' b1=block ('else' bb=block {$b2 = $bb.v;})?
      {$v = new IfStmtNode($e.v, $b1.v, $b2);}
    | 'for' '(' id=IDENTIFIER '=' init=expr ',' end=expr
        (',' i=INTLITERAL {$step = new IntLiteralNode($i.text);})? ')'
        b1=block
      {$v = new ForStmtNode(new VarNode($id.text), $init.v, $end.v, $step, $b1.v); }
    | 'while' '(' e=expr ')' b1=block
      {$v = new WhileStmtNode($e.v, $b1.v);}
    | 'return' (e=expr {$ret = $e.v;})? ';'
      {$v = new ReturnStmtNode($ret);}
    | 'break' ';'
      {$v = new BreakStmtNode();}
    | 'continue' ';'
      {$v = new ContinueStmtNode();}
    ;

assign_op: '=' | '+=' | '-=' ;

method_call returns [MethodCallNode v]
locals [List<ExpNode> args, List<CalloutArgNode> args2]
@init {
    $args = new ArrayList<>();
    $args2 = new ArrayList<>();
}
    : n=method_name '('
      (
        e1=expr {$args.add($e1.v);}
        (',' e2=expr {$args.add($e2.v);})*
      )? ')'
      {$v = new ExpArgsMethodCallNode($n.v, $args);}
    | n=method_name '('
      (
        c1=callout_arg {$args2.add($c1.v);}
        (',' c2=callout_arg {$args2.add($c2.v);})*
      )? ')'
      {$v = new CalloutArgsMethodCallNode($n.v, $args2);}
    ;

method_name returns [MethodNameNode v]
    : IDENTIFIER {$v = new MethodNameNode(new VarNode($IDENTIFIER.text));}
    ;

location returns [LocationNode v]
    : IDENTIFIER {$v = new VarLocationExprNode(new VarNode($IDENTIFIER.text)); }
    | IDENTIFIER '[' e=expr ']'
      {$v = new VarArrayLocationExprNode(new VarNode($IDENTIFIER.text), $e.v);}
    ;

expr returns [ExpNode v]
    : l=location    {$v = new LocationExprNode($l.v);}
    | m=method_call {$v = new MethodCallExprNode($m.v);}
    | i=literal     {$v = new LiteralExprNode($i.v);}
    | '@' IDENTIFIER {$v = new IdExprNode(new VarNode($IDENTIFIER.text));}
    | e1=expr op=bin_op e2=expr
      {$v = new BinExprNode($op.v, $e1.v, $e2.v);}
    | '-' e=expr
      {$v = new UnaryExprNode("-", $e.v);}
    | '!' e=expr    {$v = new UnaryExprNode("!", $e.v);}
    | '(' e=expr ')' {$v = $e.v;}
    | c=expr '?' b1=expr ':' b2=expr
      {$v = new CondExprNode($c.v, $b1.v, $b2.v);}
    ;

callout_arg returns [CalloutArgNode v]
    : e=expr {$v = new ExprArgNode($e.v);}
    | STRINGLITERAL {$v = new StringArgNode(new StringLiteralNode(toString($STRINGLITERAL.text)));}
    ;

bin_op returns [OpNode v]
    : a1=arith_op   {$v = new ArithOpNode($a1.text);}
    | a2=rel_op     {$v = new RelOpNode($a2.text);}
    | a3=eq_op      {$v = new EqOpNode($a3.text);}
    | a4=cond_op    {$v = new CondOpNode($a4.text);}
    ;

arith_op: '+' | '-' | '*' | '/' | '%' ;

rel_op: '<' | '>' | '<=' | '>=';

eq_op: '==' | '!=' ;

cond_op: '&&' | '||';

literal returns [LiteralNode v]
    : INTLITERAL    { $v = new IntLiteralNode($INTLITERAL.text); }
    | CHARLITERAL   { $v = new CharLiteralNode(toch($CHARLITERAL.text)); }
    | BOOLEANLITERAL { $v = new BoolLiteralNode(tobool($BOOLEANLITERAL.text)); }
    ;

CHARLITERAL: '\'' CHAR '\'';

BADCHAR: '\'' . '\'';

INTLITERAL
    : DECIMAL_LITERAL
    | HEX_LITERAL
    ;

BOOLEANLITERAL: 'true' | 'false';

STRINGLITERAL: '"' CHAR* '"';

UNTERMINATED_STRING
      : '"' XCHAR*
      ;

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
XCHAR
    : '\\' ('\'' | '\"' | '\\' | 't' | 'r')
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


WS  :   [ \t\r\n]+ -> skip
    ;

LINE_COMMENT
    : '//' ~[\r\n]* '\r'? '\n' -> skip
    ;

ErrorChar : . ;
