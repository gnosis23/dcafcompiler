package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/23.
  */

abstract class Node {
//    def location(): Location
}

case class ProgramNode(callouts: List[CalloutDeclNode], fields: List[FieldDeclNode],
                       methods: List[MethodDeclNode]) extends Node

case class CalloutDeclNode(name: String) extends Node

case class MethodDeclNode(t: TypeNode, name: String, params: List[ParamNode])
    extends Node

case class ParamNode(t: TypeNode, variable: VarNode) extends Node

case class VarNode(name: String) extends Node

//
// location
//  : IDENTIFIER
//  | IDENTIFIER '[' expr ']'
//
abstract class LocationNode extends Node

case class VarLocationExprNode( variable: VarNode) extends LocationNode

case class VarArrayLocationExprNode(variable: VarNode, exp: ExpNode)
    extends LocationNode

//    statement
//    : location assign_op expr ';'
//    | method_call ';'
//    | 'if' '(' expr ')' block ('else' block)?
//    | 'for' '(' IDENTIFIER '=' expr ',' expr (',' INTLITERAL)? ')' block
//    | 'while' '(' expr ')' block
//    | 'return' expr? ';'
//    | 'break' ';'
//    | 'continue' ';'
//    ;
abstract class StmtNode extends Node

case class AssignStmtNode(location: LocationNode, op: AssignOpNode, expr: ExpNode)
    extends StmtNode

case class MethodCallStmtNode(call: MethodCallNode) extends StmtNode

case class IfStmtNode(cond: ExpNode, body: BlockNode, elseBody: BlockNode)
    extends StmtNode


case class ForStmtNode(id: VarNode, initExpr: ExpNode, endExpr: ExpNode,
                       step: ExpNode,
                       body: BlockNode) extends StmtNode

case class WhileStmtNode(cond: ExpNode, body: BlockNode) extends StmtNode

case class ReturnStmtNode(value: ExpNode) extends StmtNode

case object BreakStmtNode extends StmtNode

case object ContinueStmtNode extends StmtNode

case class AssignOpNode(op: String) extends Node

//    block
//    : '{' field_decl* statement* '}'
//    ;
case class BlockNode(decls: List[FieldDeclNode], Stmts: List[StmtNode]) extends Node

//    field_decl
//    : type (IDENTIFIER | IDENTIFIER '[' INTLITERAL ']')
//    (',' (IDENTIFIER | IDENTIFIER '[' INTLITERAL ']'))* ';'
//    ;
case class FieldDeclNode(t: TypeNode, names: List[NameNode]) extends Node

abstract class TypeNode extends Node

case object IntTypeNode extends TypeNode

case object BoolTypeNode extends TypeNode

case object VoidTypeNode extends TypeNode

abstract class NameNode extends Node

case class VarNameNode(varNode: VarNode) extends NameNode

case class ArrayNameNode(varNode: VarNode, size: IntLiteralNode) extends NameNode

//    expr
//    : location
//    | method_call
//    | literal
//    | '@' IDENTIFIER
//    | expr bin_op expr
//    | '-' expr
//    | '!' expr
//    | '(' expr ')'
//    | expr '?' expr ':' expr
//    ;

abstract class ExpNode extends Node

case class LocationExprNode(location: LocationNode) extends ExpNode

case class MethodCallExprNode(call: MethodCallNode) extends ExpNode

case class LiteralExprNode(value: LiteralNode) extends ExpNode

case class IdExprNode(id: VarNode) extends ExpNode

case class BinExprNode(op: OpNode, lhs: ExpNode, rhs: ExpNode)
    extends ExpNode

case class UnaryExprNode(op: String, exp: ExpNode) extends ExpNode

case class CondExpNode(cond: ExpNode, branch1: ExpNode, branch2: ExpNode)
    extends ExpNode

//    bin_op
//    : arith_op
//    | rel_op
//    | eq_op
//    | cond_op
//    ;
abstract class OpNode extends Node

case class ArithOpNode(op: String) extends OpNode

case class RelOpNode(op: String) extends OpNode

case class EqOpNode(op: String) extends OpNode

case class CondOpNode(op: String) extends OpNode

abstract class LiteralNode extends ExpNode

case class IntLiteralNode(text: String) extends LiteralNode

case class CharLiteralNode(value: Char) extends LiteralNode

case class BoolLiteralNode(value: Boolean) extends LiteralNode

//    method_call
//    : method_name '(' (expr (',' expr)*)? ')'
//    | method_name '(' (callout_arg (',' callout_arg)*)? ')'
//    ;
abstract class MethodCallNode extends Node

case class ExpArgsMethodCallNode(name: MethodNameNode, arguments: List[ExpNode])
    extends MethodCallNode

case class CalloutArgsMethodCallNode(name: MethodNameNode, arguments: List[CalloutArgNode])
    extends MethodCallNode

case class MethodNameNode(id: VarNode) extends Node

//    callout_arg
//    : expr
//    | STRINGLITERAL
//    ;
abstract class CalloutArgNode extends Node

case class ExprArgNode(exp: ExpNode) extends CalloutArgNode

case class StringArgNode(str: StringLiteralNode) extends CalloutArgNode

case class StringLiteralNode(str: String) extends Node