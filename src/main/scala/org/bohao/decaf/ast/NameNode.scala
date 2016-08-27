package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
abstract class NameNode extends Node

case class VarNameNode(loc: Location, name: String) extends NameNode

case class ArrayNameNode(loc: Location, name: String, size: IntLiteralNode)
    extends NameNode
