package samples

import org.bohao.decaf.ast.AstDumper
import org.bohao.decaf.compile.Compiler
import org.scalatest.FunSpec

/**
  * Created by bohao on 2016/8/31.
  */
class IrTest extends FunSpec {
    describe("ir") {
        it("ir1") {
            val path = getClass.getClassLoader.getResource("ir1.dcaf").toURI.getPath
            val ir = Compiler.asm(path)

            assert(ir != null)
        }
    }
}
