package samples

import org.bohao.decaf.symbol.{ISymbol, Env}
import org.bohao.decaf.types._
import org.scalatest.FunSpec

/**
  * Created by bohao on 2016/8/26.
  */
class SymbolTest extends FunSpec {
    describe("symbol test") {
        it("should pass") {
            val global = new Env(None)

            global.parentScope() match {
                case None =>
                case Some(x) => fail()
            }

            val x0 = new ISymbol("x", IntType)
            val y0 = new ISymbol("y", BoolType)
            global.addSymbol(x0)
            global.addSymbol(y0)

            global.find("y") match {
                case None => fail()
                case Some(x) =>
            }

            global.find("he") match {
                case None =>
                case Some(x) => fail()
            }

            val child1 = new Env(Some(global))
            global.addChildScope(child1)

            child1.find("x") match {
                case None => fail("find parent scope failed")
                case Some(x) => assert(x.kind == IntType)
            }
            assert(true)
        }
    }
}
