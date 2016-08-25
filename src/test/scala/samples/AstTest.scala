package samples

import java.util

import org.bohao.decaf.ast._
import org.bohao.decaf.compile.Compiler
import org.scalatest.FunSpec
import java.util._

import scala.collection.JavaConverters._

/**
  * Created by bohao on 2016/8/23.
  */
class AstTest extends FunSpec{
    import sext._
    val testLocation = new Location(0, 0)

    describe("exp") {
        it("Usable") {
            val exps = CondExprNode(testLocation, LiteralExprNode(testLocation, BoolLiteralNode(testLocation, value = true)),
                BinExprNode(testLocation, ArithOpNode(testLocation, "+"),
                    IdExprNode(testLocation, VarNode(testLocation, "x")),
                    IdExprNode(testLocation, VarNode(testLocation, "y"))),
                BinExprNode(testLocation, ArithOpNode(testLocation, "-"),
                    IdExprNode(testLocation, VarNode(testLocation, "x")),
                    IdExprNode(testLocation, VarNode(testLocation, "y")))
            )

            println(exps.treeString)
            assert(true)
        }

        it("Method") {
            val list = new util.ArrayList[CalloutArgNode]()
            list.add(ExprArgNode(testLocation, LiteralExprNode(testLocation, IntLiteralNode(testLocation, "11111"))))
            list.add(StringArgNode(testLocation, StringLiteralNode(testLocation, "hehe")))
            val exps = MethodCallExprNode(testLocation,
                CalloutArgsMethodCallNode(testLocation, MethodNameNode(testLocation, VarNode(testLocation, "shit")), list)
            )

            println(exps.treeString)
            assert(true)
        }
    }

    describe("ast") {
        it("scan2") {
            val path = getClass.getClassLoader.getResource("parser1.dcaf").toURI.getPath
            val ast = Compiler.parse(path)

            AstDumper.dump(ast)
            assert(ast != null)
        }
    }

    describe("error") {
        it("error1") {
            val path = getClass.getClassLoader.getResource("parser2.dcaf").toURI.getPath
            val ast = Compiler.parse(path)

            assert(ast == null)
        }
    }
}
