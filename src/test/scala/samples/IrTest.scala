package samples

import org.bohao.decaf.ast.AstDumper
import org.bohao.decaf.compile.Compiler
import org.scalatest.FunSpec

/**
  * Created by bohao on 2016/8/31.
  */
class IrTest extends FunSpec {
    describe("ir") {
        it("if") {
            val path = getClass.getClassLoader.getResource("ir-if.dcaf").toURI.getPath
            val ir = Compiler.asm(path)
            ir.start.foreach(println)
            assert(ir != null)
        }

        it("bang") {
            val path = getClass.getClassLoader.getResource("ir-bang.dcaf").toURI.getPath
            val ir = Compiler.asm(path)
            ir.start.foreach(println)
            assert(ir != null)
        }

        it("if else") {
            val path = getClass.getClassLoader.getResource("ir-ifelse.dcaf").toURI.getPath
            val ir = Compiler.asm(path)
            ir.start.foreach(println)
            assert(ir != null)
        }

        it("while") {
            val path = getClass.getClassLoader.getResource("ir-while.dcaf").toURI.getPath
            val ir = Compiler.asm(path)
            ir.start.foreach(println)
            assert(ir != null)
        }

        it("for") {
            val path = getClass.getClassLoader.getResource("ir-for.dcaf").toURI.getPath
            val ir = Compiler.asm(path)
            ir.start.foreach(println)
            assert(ir != null)
        }

        it("and") {
            val path = getClass.getClassLoader.getResource("ir-and.dcaf").toURI.getPath
            val ir = Compiler.asm(path)
            ir.start.foreach(println)
            assert(ir != null)
        }

        it("or") {
            val path = getClass.getClassLoader.getResource("ir-or.dcaf").toURI.getPath
            val ir = Compiler.asm(path)
            ir.start.foreach(println)
            assert(ir != null)
        }

        it("cond") {
            val path = getClass.getClassLoader.getResource("ir-cond.dcaf").toURI.getPath
            val ir = Compiler.asm(path)
            ir.start.foreach(println)
            assert(ir != null)
        }

        it("cond2") {
            val path = getClass.getClassLoader.getResource("ir-cond2.dcaf").toURI.getPath
            val ir = Compiler.asm(path)
            ir.start.foreach(println)
            assert(ir != null)
        }

        it("while + if") {
            val path = getClass.getClassLoader.getResource("ir1.dcaf").toURI.getPath
            val ir = Compiler.asm(path)
            ir.start.foreach(println)
            assert(ir != null)
        }

        it("array len") {
            val path = getClass.getClassLoader.getResource("ir-arraylen.dcaf").toURI.getPath
            val ir = Compiler.asm(path)
            ir.start.foreach(println)
            assert(ir != null)
        }
    }
}
