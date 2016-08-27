package samples

import org.bohao.decaf.ast.AstDumper
import org.scalatest.FunSpec
import org.bohao.decaf.compile._

/**
  * Created by bohao on 2016/8/27.
  */
class SemanticTest extends FunSpec{
    describe("semantic") {
        it("scan2") {
            val path = getClass.getClassLoader.getResource("parser1.dcaf").toURI.getPath
            val ast = Compiler.inter(path)

            // AstDumper.dump(ast)
//            assert(ast != null)
        }
    }
}
