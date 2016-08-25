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

case class AssignStmtNode(loc: Location, locationNode: LocationNode,
                          op: AssignOpNode,
                          expr: ExpNode)
    extends StmtNode

case class MethodCallStmtNode(loc: Location, call: MethodCallNode) extends StmtNode

case class IfStmtNode(loc: Location, cond: ExpNode, body: BlockNode,
                      elseBody: BlockNode)
    extends StmtNode


case class ForStmtNode(loc: Location, id: VarNode, initExpr: ExpNode,
                       endExpr: ExpNode,
                       step: IntLiteralNode,
                       body: BlockNode) extends StmtNode

case class WhileStmtNode(loc: Location, cond: ExpNode, body: BlockNode) extends StmtNode

case class ReturnStmtNode(loc: Location, value: ExpNode) extends StmtNode

case class BreakStmtNode(loc: Location) extends StmtNode

case class ContinueStmtNode(loc: Location) extends StmtNode

