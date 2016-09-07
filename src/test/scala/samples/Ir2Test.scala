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

    }
}
