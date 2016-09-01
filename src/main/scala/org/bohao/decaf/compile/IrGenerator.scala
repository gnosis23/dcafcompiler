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
            case WhileStmtNode(loc, cond, body) =>
                build(body)
            case ReturnStmtNode(loc, value) =>
            case BreakStmtNode(loc) =>
            case ContinueStmtNode(loc) =>
            case _ =>
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
                val q1 = build(basicBlock, lhs)
                val q2 = build(basicBlock, rhs)
                val t = createQuad(QAdd(TempVarOperand(), open(q1), open(q2)))
                basicBlock.+=(t)
                return t
            case UnaryExprNode(loc, op, exp) =>
            case CondExprNode(loc, cond, branch1, branch2) =>
            case _ =>
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
            case QCJmp(cond, b1, b2) => ???
            case QJmp(dest) => ???
            case Q1(dest) => dest
            case _ => ???
        }
    }
}
