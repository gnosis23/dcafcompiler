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

    describe("int") {
        it("int overflow") {
            val text = "9999999999999999"
            var value: Option[Int] = None
            try {
                value = Some(Integer.decode(text))
            } catch{
                case e: Exception =>
                    value = None
            }

            assert(value.isEmpty)
        }

        it("int hex overflow") {
            val text = "0xfffffffffffffffffffffffffff"
            var value: Option[Int] = None
            try {
                value = Some(Integer.decode(text))
            } catch{
                case e: Exception =>
                    value = None
            }

            assert(value.isEmpty)
        }


        it("int dec test") {
            val text = "1024"
            var value: Option[Int] = None
            try {
                value = Some(Integer.decode(text))
            } catch{
                case e: Exception =>
                    value = None
            }

            assert(value.get == 1024)
        }

        it("int hex test") {
            val text = "0xff"
            var value: Option[Int] = None
            try {
                value = Some(Integer.decode(text))
            } catch{
                case e: Exception =>
                    value = None
            }

            assert(value.get == 255)
        }
    }
}
