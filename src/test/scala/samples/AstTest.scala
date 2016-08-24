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

    describe("exp") {
        it("Usable") {
            val exps = CondExprNode(BoolLiteralNode(true),
                BinExprNode(ArithOpNode("+"), IdExprNode(VarNode("x")), IdExprNode(VarNode("y"))),
                BinExprNode(ArithOpNode("-"), IdExprNode(VarNode("x")), IdExprNode(VarNode("y")))
            )

            println(exps.treeString)
            assert(true)
        }

        it("Method") {
            val list = new util.ArrayList[CalloutArgNode]()
            list.add(ExprArgNode(IntLiteralNode("11111")))
            list.add(StringArgNode(StringLiteralNode("hehe")))
            val exps = MethodCallExprNode(
                CalloutArgsMethodCallNode(MethodNameNode(VarNode("shit")), list)
            )

            println(exps.treeString)
            assert(true)
        }
    }

    describe("ast") {
        it("scan2") {
            val path = getClass.getClassLoader.getResource("parser1.dcaf").toURI.getPath
            val ast = Compiler.parse(path)

            assert(ast != null)
        }
    }
}
