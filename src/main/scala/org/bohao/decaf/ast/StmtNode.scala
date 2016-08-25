package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
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
                       step: IntLiteralNode,
                       body: BlockNode) extends StmtNode

case class WhileStmtNode(cond: ExpNode, body: BlockNode) extends StmtNode

case class ReturnStmtNode(value: ExpNode) extends StmtNode

case class BreakStmtNode() extends StmtNode

case class ContinueStmtNode() extends StmtNode

