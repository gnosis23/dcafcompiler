package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
//    callout_arg
//    : expr
//    | STRINGLITERAL
//    ;
abstract class CalloutArgNode extends Node

case class ExprArgNode(loc: Location, var exp: ExpNode) extends CalloutArgNode {
}

case class StringArgNode(loc: Location, str: StringLiteralNode) extends CalloutArgNode {
}
