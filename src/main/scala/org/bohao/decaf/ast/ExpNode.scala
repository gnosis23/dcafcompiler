package org.bohao.decaf.ast

import org.bohao.decaf.types.IType

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

abstract class ExpNode extends Node {
    var nodeType: IType = null
}

case class LocationExprNode(loc: Location, locationNode: LocationNode) extends ExpNode {
}

case class MethodCallExprNode(loc: Location, call: MethodCallNode) extends ExpNode {
}

case class LiteralExprNode(loc: Location, value: LiteralNode) extends ExpNode {
}

case class IdExprNode(loc: Location, id: VarNode) extends ExpNode {
}

case class BinExprNode(loc: Location, op: OpNode, lhs: ExpNode, rhs: ExpNode)
    extends ExpNode {
}

case class UnaryExprNode(loc: Location, op: String, exp: ExpNode) extends ExpNode {
}

case class CondExprNode(loc: Location, cond: ExpNode, branch1: ExpNode, branch2: ExpNode)
    extends ExpNode {
}
