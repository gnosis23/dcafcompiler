package samples

import org.bohao.decaf.ast._
import org.scalatest.FunSpec

/**
  * Created by bohao on 2016/8/23.
  */
class AstTest extends FunSpec{
    import sext._

    describe("exp") {
        it("Usable") {
            val exps = CondExpNode(BoolLiteralNode(true),
                BinExprNode(ArithOpNode("+"), IdExprNode(VarNode("x")), IdExprNode(VarNode("y"))),
                BinExprNode(ArithOpNode("-"), IdExprNode(VarNode("x")), IdExprNode(VarNode("y")))
            )

            println(exps.treeString)
            assert(true)
        }

        it("Method") {
            val exps = MethodCallExprNode(
                CalloutArgsMethodCallNode(
                    MethodNameNode(VarNode("shit")),
                    List(ExprArgNode(IntLiteralNode(1)), StringArgNode(StringLiteralNode("hehe"))))
            )

            println(exps.treeString)
            assert(true)
        }
    }
}
