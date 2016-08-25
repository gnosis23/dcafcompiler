package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
abstract class TypeNode extends Node

case class IntTypeNode() extends TypeNode

case class BoolTypeNode() extends TypeNode

case class VoidTypeNode() extends TypeNode