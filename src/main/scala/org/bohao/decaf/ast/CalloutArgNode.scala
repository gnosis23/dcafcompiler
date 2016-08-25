package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
//    callout_arg
//    : expr
//    | STRINGLITERAL
//    ;
abstract class CalloutArgNode extends Node

case class ExprArgNode(exp: ExpNode) extends CalloutArgNode

case class StringArgNode(str: StringLiteralNode) extends CalloutArgNode
