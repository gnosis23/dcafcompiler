package org.bohao.decaf.ast

import org.bohao.decaf.symbol.ISymbol

/**
  * Created by bohao on 2016/8/25.
  */
case class VarNode(loc: Location, name: String) extends Node {
    var symbol: ISymbol = null
}
