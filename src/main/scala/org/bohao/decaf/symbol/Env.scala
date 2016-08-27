package org.bohao.decaf.symbol

/**
  * Created by bohao on 2016/8/26.
  */
class Env(val parent: Option[Env]) {
    var scopes: List[Env] = List()
    var symbols: List[ISymbol] = List()

    def addSymbol(symbol: ISymbol): Boolean = {
        if (!findLocal(symbol.name)) {
            symbol.scope = this
            symbols = symbol :: symbols
            true
        } else {
            false
        }
    }

    def findLocal(name: String): Boolean = {
        symbols.exists(p => p.name == name)
    }

    def find(name: String): Option[ISymbol] = {
        val t = symbols.find(p => p.name == name)
        t match {
            case None => parent match {
                case None => None
                case Some(scope) => scope.find(name)
            }
            case Some(symb) => t
        }
    }

    def addChildScope(scope: Env): Unit = {
        scopes = scope :: scopes
    }

    def parentScope(): Option[Env] = parent

    def childrenScopes(): Option[List[Env]] = {
        if (scopes.nonEmpty) Some(scopes)
        else None
    }
}
