package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
//
// location
//  : IDENTIFIER
//  | IDENTIFIER '[' expr ']'
//
abstract class LocationNode extends Node

case class VarLocationExprNode(loc: Location,  variable: VarNode) extends LocationNode {
}

case class VarArrayLocationExprNode(loc: Location, variable: VarNode, exp: ExpNode)
    extends LocationNode {
}

