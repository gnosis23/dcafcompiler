package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
abstract class NameNode extends Node

case class VarNameNode(varNode: VarNode) extends NameNode

case class ArrayNameNode(varNode: VarNode, size: IntLiteralNode) extends NameNode
