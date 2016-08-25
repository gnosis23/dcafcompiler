grammar Parser;

@header {
package org.bohao.decaf.parser;

import org.bohao.decaf.util.Converter;
import org.bohao.decaf.ast.*;
}

@parser::members {
    private String fileName = "[Unknown]";

    public ParserParser(TokenStream input, String fileName) {
        this(input);
        fileName = fileName;
    }

    private char toch(String text) {
        return Converter.toChar(text);
    }

    private boolean tobool(String text) {
        return Converter.toBool(text);
    }

    private String toString(String text) {
        return Converter.toString(text);
    }

    private Location loc(Token t) {
        return new Location(t, fileName);
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
      {
        $ast = new ProgramNode($callouts, $fields, $methods);
      }
    ;

callout_decl returns [CalloutDeclNode v]
    : s='callout' IDENTIFIER ';'
      {
        $v = new CalloutDeclNode(loc($s), $IDENTIFIER.text);
      }
    ;

field_decl returns [FieldDeclNode v]
locals [List<NameNode> names]
@init {
    $names = new ArrayList<>();
}
    : t=type n1=name {$names.add($n1.v);}
      (',' n2=name {$names.add($n2.v);})* ';'
      {
        $v = new FieldDeclNode($t.v.location(), $t.v, $names);
      }
    ;

name returns [NameNode v]
    : id=IDENTIFIER
      {
        $v = new VarNameNode(loc($id), new VarNode(loc($id),$id.text));
      }
    | id=IDENTIFIER '[' i=INTLITERAL ']'
      {
        $v = new ArrayNameNode(loc($id), new VarNode(loc($id),$id.text),
                new IntLiteralNode(loc($id), $i.text));
      }
    ;

method_decl returns [MethodDeclNode v]
locals [TypeNode t, List<ParamNode> params]
@init {
    $params = new ArrayList<>();
}
    : (
          x=type {$t = $x.v;}
        | q='void' {$t = new VoidTypeNode(loc($q));}
      )
      id=IDENTIFIER
      '(' (
            p1=param {$params.add($p1.v);}
            (',' p2=param {$params.add($p2.v);})*
          )? ')'
      b=block
      {
        $v = new MethodDeclNode(loc($id), $t, $id.text, $params, $b.v);
      }
    ;

param returns [ParamNode v]
    : t=type id=IDENTIFIER
      {
        $v = new ParamNode($t.v.location(), $t.v, new VarNode(loc($id),$id.text));
      }
    ;

block returns [BlockNode v]
locals [List<FieldDeclNode> fields, List<StmtNode> stmts]
@init {
    $fields = new ArrayList<>();
    $stmts = new ArrayList<>();
}
    : q='{'
        (f=field_decl {$fields.add($f.v);})*
        (s=statement  {$stmts.add($s.v);})*
      '}'
      {
        $v = new BlockNode(loc($q), $fields, $stmts);
      }
    ;

type returns [TypeNode v]
    : s1='int' {$v = new IntTypeNode(loc($s1));}
    | s2='boolean' {$v = new BoolTypeNode(loc($s2));}
    ;

statement returns [StmtNode v]
locals [BlockNode b2, IntLiteralNode step, ExpNode ret]
@init {
    $b2 = null; $step = null; $ret = null;
}
    : l=location op=assign_op e=expr ';'
      {
        $v = new AssignStmtNode($l.v.location(), $l.v, new AssignOpNode(loc($op.start), $op.text), $e.v);
      }
    | m=method_call ';'
      {
        $v = new MethodCallStmtNode($m.v.location(), $m.v);
      }
    | start1='if' '(' e=expr ')' b1=block ('else' bb=block {$b2 = $bb.v;})?
      {
        $v = new IfStmtNode(loc($start1), $e.v, $b1.v, $b2);
      }
    | start2='for' '(' id=IDENTIFIER '=' init=expr ',' end=expr
        (',' i=INTLITERAL {$step = new IntLiteralNode(loc($i), $i.text);})? ')'
        b1=block
      {
        $v = new ForStmtNode(loc($start2), new VarNode(loc($id),$id.text), $init.v, $end.v, $step, $b1.v);
      }
    | start3='while' '(' e=expr ')' b1=block
      {
        $v = new WhileStmtNode(loc($start3), $e.v, $b1.v);
      }
    | start4='return' (e=expr {$ret = $e.v;})? ';'
      {
        $v = new ReturnStmtNode(loc($start4), $ret);
      }
    | start5='break' ';'
      {
        $v = new BreakStmtNode(loc($start5));
      }
    | start6='continue' ';'
      {
        $v = new ContinueStmtNode(loc($start6));
      }
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
      {
        $v = new ExpArgsMethodCallNode($n.v.location(), $n.v, $args);
      }
    | n=method_name '('
      (
        c1=callout_arg {$args2.add($c1.v);}
        (',' c2=callout_arg {$args2.add($c2.v);})*
      )? ')'
      {
        $v = new CalloutArgsMethodCallNode($n.v.location(), $n.v, $args2);
      }
    ;

method_name returns [MethodNameNode v]
    : id=IDENTIFIER {$v = new MethodNameNode(loc($id), new VarNode(loc($id), $id.text));}
    ;

location returns [LocationNode v]
    : id=IDENTIFIER
      {
        $v = new VarLocationExprNode(loc($id), new VarNode(loc($id),$id.text));
      }
    | id=IDENTIFIER '[' e=expr ']'
      {
        $v = new VarArrayLocationExprNode(loc($id), new VarNode(loc($id),$id.text), $e.v);
      }
    ;

expr returns [ExpNode v]
    : l=location
        {
            $v = new LocationExprNode($l.v.location(), $l.v);
        }
    | m=method_call {$v = new MethodCallExprNode($m.v.location(), $m.v);}
    | i=literal     {$v = new LiteralExprNode($i.v.location(), $i.v);}
    | start1='@' IDENTIFIER
      {
        $v = new IdExprNode(loc($start1), new VarNode(loc($IDENTIFIER),$IDENTIFIER.text));
      }
    | e1=expr op=bin_op e2=expr
      {
        $v = new BinExprNode($e1.v.location(), $op.v, $e1.v, $e2.v);
      }
    | start2='-' e=expr
      {
        $v = new UnaryExprNode(loc($start2), "-", $e.v);
      }
    | start3='!' e=expr
      {
        $v = new UnaryExprNode(loc($start3), "!", $e.v);
      }
    | '(' e=expr ')' {$v = $e.v;}
    | c=expr '?' b1=expr ':' b2=expr
      {
        $v = new CondExprNode($c.v.location(), $c.v, $b1.v, $b2.v);
      }
    ;

callout_arg returns [CalloutArgNode v]
    : e=expr {$v = new ExprArgNode(loc($e.start), $e.v);}
    | s=STRINGLITERAL {$v = new StringArgNode(loc($s), new StringLiteralNode(loc($s), toString($s.text)));}
    ;

bin_op returns [OpNode v]
    : a1=arith_op   {$v = new ArithOpNode(loc($a1.start), $a1.text);}
    | a2=rel_op     {$v = new RelOpNode(loc($a2.start), $a2.text);}
    | a3=eq_op      {$v = new EqOpNode(loc($a3.start), $a3.text);}
    | a4=cond_op    {$v = new CondOpNode(loc($a4.start), $a4.text);}
    ;

arith_op: '+' | '-' | '*' | '/' | '%' ;

rel_op: '<' | '>' | '<=' | '>=';

eq_op: '==' | '!=' ;

cond_op: '&&' | '||';

literal returns [LiteralNode v]
    : s=INTLITERAL    { $v = new IntLiteralNode(loc($s), $s.text); }
    | s=CHARLITERAL   { $v = new CharLiteralNode(loc($s), toch($s.text)); }
    | s=BOOLEANLITERAL { $v = new BoolLiteralNode(loc($s), tobool($s.text)); }
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
