package org.bohao.decaf.compile

import org.bohao.decaf.ast._
import org.bohao.decaf.ir._
import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * Created by bohao on 2016/8/31.
  */
object IrGenerator {
    def build(ast: ProgramNode): Ir = {
        // ast.methods.foreach(p => build(p))
        build(ast.methods.get(0))
    }

    def build(method: MethodDeclNode): Ir = {
        Ir(build(method.block))
    }

    def build(block: BlockNode): List[Quad] = {
        val templist = new mutable.MutableList[Quad]
        block.Stmts.foreach(p => {
            templist ++= build(p)
        })
        templist.toList
    }

    def build(stmt: StmtNode): mutable.MutableList[Quad] = {
        val list = new mutable.MutableList[Quad]
        stmt match {
            case AssignStmtNode(loc, locationNode, op, expr) =>
                val q1 = build(locationNode)
                val q2 = build(list, expr)
                val t = createQuad(QAssign(q1, open(q2)))
                list += t
            case MethodCallStmtNode(loc, call) =>
            case IfStmtNode(loc, cond, body, elseBody) =>
                val condQuad = build(list, cond)
                val labelTrue = QLabel()
                val labelFalse = QLabel()
                val labelEnd = QLabel()
                list += createQuad(QCJmp(open(condQuad), labelTrue, labelFalse))
                list += labelTrue
                list ++= build(body)
                list += createQuad(QJmp(labelEnd))
                list += labelFalse
                if (elseBody != null) {
                    list ++= build(elseBody)
                    list += createQuad(QJmp(labelEnd))
                }
                list += labelEnd
            case ForStmtNode(loc, id, initExpr, endExpr, step, body) =>
                val labelStart = QLabel()
                val labelTrue = QLabel()
                val labelFalse = QLabel()
                val index = VarOperand(id)
                val initQuad = build(list, initExpr)
                list += createQuad(QAssign(index, open(initQuad)))
                val endQuad = build(list, endExpr)
                list += labelStart
                val temp = TempVarOperand()
                list += createQuad(QSub(temp, index, open(endQuad)))
                list += createQuad(QCJmp(temp, labelTrue, labelFalse))
                list += labelTrue
                list ++= build(body)
                if (step != null) {
                    list += createQuad(QAdd(index, index, IntOperand(step.value.get)))
                }
                list += createQuad(QJmp(labelStart))
                list += labelFalse
            case WhileStmtNode(loc, cond, body) =>
                val labelStart = QLabel()
                val labelTrue = QLabel()
                val labelFalse = QLabel()
                list += labelStart
                val condQuad = build(list, cond)
                list += createQuad(QCJmp(open(condQuad), labelTrue, labelFalse))
                list += labelTrue
                list ++= build(body)
                list += QJmp(labelStart)
                list += labelFalse
            case ReturnStmtNode(loc, value) =>
            case BreakStmtNode(loc) =>
            case ContinueStmtNode(loc) =>
        }
        list
    }

    def build(basicBlock: mutable.MutableList[Quad], expr: ExpNode): Quad = {
        expr match {
            case LocationExprNode(loc, locationNode) =>
                locationNode match {
                    case VarLocationExprNode(_, variable) =>
                        return Q1(VarOperand(variable.name))
                    case VarArrayLocationExprNode(_, variable, exp) =>
                }
            case MethodCallExprNode(loc, call) =>
            case LiteralExprNode(loc, value) =>
                value match {
                    case e @ IntLiteralNode(_, text) =>
                        return Q1(IntOperand(e.value.get))
                    case CharLiteralNode(_, ch) =>
                        return Q1(IntOperand(ch))
                    case BoolLiteralNode(_, v) =>
                        return Q1(IntOperand(if(v) 1 else 0))
                }
            case IdExprNode(loc, id) =>
            case BinExprNode(loc, op, lhs, rhs) =>
                op match {
                    case ArithOpNode(_, op1) =>
                        val q1 = build(basicBlock, lhs)
                        val q2 = build(basicBlock, rhs)
                        var t: Quad = null
                        op1 match {
                            case "+" =>
                                t = createQuad(QAdd(TempVarOperand(), open(q1), open(q2)))
                            case "-" =>
                                t = createQuad(QSub(TempVarOperand(), open(q1), open(q2)))
                            case "*" =>
                                t = createQuad(QMul(TempVarOperand(), open(q1), open(q2)))
                            case "/" =>
                                t = createQuad(QDiv(TempVarOperand(), open(q1), open(q2)))
                            case "%" =>
                                t = createQuad(QMod(TempVarOperand(), open(q1), open(q2)))
                        }
                        basicBlock += t
                        return t
                    case RelOpNode(_, op1) =>
                        val q1 = build(basicBlock, lhs)
                        val q2 = build(basicBlock, rhs)
                        var t: Quad = null
                        op1 match {
                            case "<" =>
                                t = createQuad(QTestl(TempVarOperand(), open(q1), open(q2)))
                            case ">" =>
                                t = createQuad(QTestg(TempVarOperand(), open(q1), open(q2)))
                            case "<=" =>
                                t = createQuad(QTestle(TempVarOperand(), open(q1), open(q2)))
                            case ">=" =>
                                t = createQuad(QTestge(TempVarOperand(), open(q1), open(q2)))
                        }
                        basicBlock += t
                        return t
                    case EqOpNode(_, op1) =>
                        val q1 = build(basicBlock, lhs)
                        val q2 = build(basicBlock, rhs)
                        var t: Quad = null
                        op1 match {
                            case "==" =>
                                t = createQuad(QTeste(TempVarOperand(), open(q1), open(q2)))
                            case "!=" =>
                                t = createQuad(QTestne(TempVarOperand(), open(q1), open(q2)))
                        }
                        basicBlock += t
                        return t
                    case CondOpNode(_, op1) =>
                        op1 match {
                            case "&&" =>
                                val q1 = build(basicBlock, lhs)
                                val labelTrue = QLabel()
                                val labelFalse = QLabel()
                                val temp = TempVarOperand()
                                basicBlock += createQuad(QTestne(temp, open(q1), IntOperand(0)))
                                basicBlock += createQuad(QCJmp(temp, labelTrue, labelFalse))
                                basicBlock += labelTrue
                                val q2 = build(basicBlock, rhs)
                                basicBlock += createQuad(QTestne(temp, open(q2), IntOperand(0)))
                                basicBlock += labelFalse
                                return Q1(temp)
                            case "||" =>
                                val q1 = build(basicBlock, lhs)
                                val labelTrue = QLabel()
                                val labelFalse = QLabel()
                                val temp = TempVarOperand()

                                basicBlock += createQuad(QTestne(temp, open(q1), IntOperand(0)))
                                basicBlock += createQuad(QCJmp(temp, labelFalse, labelTrue))
                                basicBlock += labelTrue
                                val q2 = build(basicBlock, rhs)
                                basicBlock += createQuad(QTestne(temp, open(q2), IntOperand(0)))
                                basicBlock += labelFalse
                                return Q1(temp)
                        }
                }
            case UnaryExprNode(loc, op, exp) =>
            case CondExprNode(loc, cond, branch1, branch2) =>
        }
        null
    }

    def build(locationNode: LocationNode): Lhs = {
        locationNode match {
            case VarLocationExprNode(loc, variable) =>
                VarOperand(variable.name)
            case VarArrayLocationExprNode(loc, variable, exp) =>
                ???
        }
    }

    private def createQuad(quad: Quad): Quad = {
//        println(quad)
        quad
    }

    private def open(quad : Quad): Operand = {
        quad match {
            case QAdd(dest, src1, src2) => dest
            case QSub(dest, src1, src2) => dest
            case QMul(dest, src1, src2) => dest
            case QDiv(dest, src1, src2) => dest
            case QAssign(dest, src) => dest
            case Q1(dest) => dest
            case QTeste(dest, left, right) => dest
            case QTestne(dest, left, right) => dest
            case QTestg(dest, left, right) => dest
            case QTestge(dest, left, right) => dest
            case QTestl(dest, left, right) => dest
            case QTestle(dest, left, right) => dest
            case _ => throw new Error("unsupported expression return quad type" + quad)
        }
    }
}
