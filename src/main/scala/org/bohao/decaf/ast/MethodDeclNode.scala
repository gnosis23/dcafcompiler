package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
case class MethodDeclNode(loc: Location, t: TypeNode, name: String,
                          params: java.util.List[ParamNode],
                          block: BlockNode)
    extends Node
