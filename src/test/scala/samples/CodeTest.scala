package samples

import org.scalatest.FunSpec
import org.bohao.decaf.compile.Compiler

/**
  * Created by bohao on 2016/9/11.
  */
class CodeTest extends FunSpec {
    describe("ir") {
        it("if") {
            val path = getClass.getClassLoader.getResource("code-1.dcaf").toURI.getPath
            val ir = Compiler.asm2(path)
            ir.dump()
            assert(ir != null)
        }
    }
}
