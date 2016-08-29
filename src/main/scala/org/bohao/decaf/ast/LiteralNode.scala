package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */

abstract class LiteralNode extends Node

case class IntLiteralNode(loc: Location, text: String) extends LiteralNode {
    var value: Option[Int] = None
}

case class CharLiteralNode(loc: Location, value: Char) extends LiteralNode

case class BoolLiteralNode(loc: Location, value: Boolean) extends LiteralNode
