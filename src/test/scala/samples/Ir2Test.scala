package samples

import org.bohao.decaf.ast.{BoolTypeNode, ParamNode, IntTypeNode, MethodDeclNode}
import org.bohao.decaf.compile.{IrGenerator2, Compiler}
import org.bohao.decaf.ir.{FunctionType, IrType}
import org.bohao.decaf.ir.IrType.{VOID, INT64}
import org.scalatest.FunSpec
import scala.collection.JavaConversions._

/**
  * Created by bohao on 2016/9/7.
  */
class Ir2Test extends FunSpec {
    describe("ir2") {
        it("functiontype") {
            val generator = IrGenerator2

            val method = MethodDeclNode(
                loc = null,
                t = IntTypeNode(null),
                name = "f1",
                params = List(
                    ParamNode(null, IntTypeNode(null), "a1"),
                    ParamNode(null, BoolTypeNode(null), "a2"),
                    ParamNode(null, IntTypeNode(null), "a3")),
                block = null)

            val functype = generator.parseFunctionType(method)
            assert(functype.retType == INT64)
            assert(functype.args == List(("a1", INT64), ("a2", INT64), ("a3", INT64)))
        }

        it("functions") {
            val path = getClass.getClassLoader.getResource("functypes.dcaf").toURI.getPath
            val ir = Compiler.asm2(path)

            ir.dump()

            assert(ir.functions.get(0).name == "f1")
            assert(ir.functions.get(0).functionType ==
                FunctionType(VOID, List(("a", INT64))))

            assert(ir.functions.get(1).name == "f2")
            assert(ir.functions.get(1).functionType ==
                FunctionType(INT64, List(("a", INT64))))

            assert(ir.functions.get(2).name == "f3")
            assert(ir.functions.get(2).functionType ==
                FunctionType(INT64, List(("a", INT64), ("b", INT64), ("c", INT64))))

            assert(ir.functions.get(3).name == "main")
            assert(ir.functions.get(3).functionType ==
                FunctionType(INT64, List()))
        }

        it("simple1") {
            val path = getClass.getClassLoader.getResource("ir2-simple1.dcaf").toURI.getPath
            val ir = Compiler.asm2(path)

            ir.dump()
        }

        it("if-then-else") {
            val path = getClass.getClassLoader.getResource("ir2-if-then-else.dcaf").toURI.getPath
            val ir = Compiler.asm2(path)

            ir.dump()
        }

        it("for") {
            val path = getClass.getClassLoader.getResource("ir2-for.dcaf").toURI.getPath
            val ir = Compiler.asm2(path)

            ir.dump()
        }

        it("while") {
            val path = getClass.getClassLoader.getResource("ir2-while.dcaf").toURI.getPath
            val ir = Compiler.asm2(path)

            ir.dump()
        }

        it("break-continue-return") {
            val path = getClass.getClassLoader.getResource("ir2-continue-break-return.dcaf").toURI.getPath
            val ir = Compiler.asm2(path)

            ir.dump()
        }

        it("ir-test") {
            val path = getClass.getClassLoader.getResource("ir2-alloca.dcaf").toURI.getPath
            val ir = Compiler.asm2(path)

            ir.dump()
        }

        it("ir-andor") {
            val path = getClass.getClassLoader.getResource("ir2-andor.dcaf").toURI.getPath
            val ir = Compiler.asm2(path)

            ir.dump()
        }

        it("ir-callout") {
            val path = getClass.getClassLoader.getResource("ir2-callout.dcaf").toURI.getPath
            val ir = Compiler.asm2(path)

            ir.dump()
        }

        it("ir-array-len") {
            val path = getClass.getClassLoader.getResource("array-len.dcaf").toURI.getPath
            val ir = Compiler.asm2(path)

            ir.dump()
        }
    }
}
