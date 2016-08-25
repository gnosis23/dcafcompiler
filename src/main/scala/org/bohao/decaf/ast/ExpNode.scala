package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */

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

case class CondExprNode(cond: ExpNode, branch1: ExpNode, branch2: ExpNode)
    extends ExpNode
