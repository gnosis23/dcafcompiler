package org.bohao.decaf.ast

import org.bohao.decaf.types.{FunctionType, IType}

/**
  * Created by bohao on 2016/8/25.
  */
case class MethodDeclNode(loc: Location, t: TypeNode, name: String,
                          params: java.util.List[ParamNode],
                          block: BlockNode)
    extends Node
{
    var methodType: FunctionType = null
}
