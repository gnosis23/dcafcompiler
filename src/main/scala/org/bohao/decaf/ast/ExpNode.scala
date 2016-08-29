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

case class BinExprNode(loc: Location, op: OpNode, var lhs: ExpNode, var rhs: ExpNode)
    extends ExpNode {
}

case class UnaryExprNode(loc: Location, op: String, var exp: ExpNode) extends ExpNode {
}

case class CondExprNode(loc: Location, cond: ExpNode,
                        var branch1: ExpNode,
                        var branch2: ExpNode)
    extends ExpNode {
}
