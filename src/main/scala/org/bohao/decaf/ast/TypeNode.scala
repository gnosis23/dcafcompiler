package org.bohao.decaf.ast

import org.bohao.decaf.types.{VoidType, BoolType, IntType, IType}

/**
  * Created by bohao on 2016/8/25.
  */
abstract class TypeNode extends Node {
    def itype: IType
}

case class IntTypeNode(loc: Location) extends TypeNode {
    override def itype: IType = IntType
}

case class BoolTypeNode(loc: Location) extends TypeNode {
    override def itype: IType = BoolType
}

case class VoidTypeNode(loc: Location) extends TypeNode {
    override def itype: IType = VoidType
}