package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
case class ParamNode(loc: Location, t: TypeNode, variable: String) extends Node {
}
