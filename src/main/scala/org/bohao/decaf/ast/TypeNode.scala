package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
abstract class TypeNode extends Node

case class IntTypeNode(loc: Location) extends TypeNode {
}

case class BoolTypeNode(loc: Location) extends TypeNode {
}

case class VoidTypeNode(loc: Location) extends TypeNode {
}