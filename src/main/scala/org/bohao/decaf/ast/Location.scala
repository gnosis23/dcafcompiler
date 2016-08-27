package org.bohao.decaf.ast

import org.antlr.v4.runtime.Token

/**
  * Created by bohao on 2016/8/23.
  */
class Location(val line: Int, val col: Int,
               val fileName: String = "[Unknown]") {
    def this(token: Token, fileName: String) {
        this(token.getLine, token.getCharPositionInLine, fileName)
    }

    def this(loc: Location) {
        this(loc.line, loc.col, loc.fileName)
    }


    override def toString = s"[L$line, C$col]"
}
