package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */

abstract class LiteralNode extends Node

case class IntLiteralNode(text: String) extends LiteralNode

case class CharLiteralNode(value: Char) extends LiteralNode

case class BoolLiteralNode(value: Boolean) extends LiteralNode
