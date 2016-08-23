package org.bohao.decaf.ast

import org.antlr.v4.runtime.Token

/**
  * Created by bohao on 2016/8/23.
  */
class Location(val line: Int, val col: Int) {
    def this(token: Token) {
        this(token.getLine, token.getCharPositionInLine)
    }

    def this(loc: Location) {
        this(loc.line, loc.col)
    }
}
