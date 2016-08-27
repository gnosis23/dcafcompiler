package org.bohao.decaf.symbol

import org.bohao.decaf.types.IType

/**
  * Created by bohao on 2016/8/26.
  */
class ISymbol(val name: String, val kind: IType) {
    var scope: Env = null


    override def toString = s"ISymbol($name, $kind)"
}

object ISymbol {
    def apply(name: String, t: IType): ISymbol = new ISymbol(name, t)
}