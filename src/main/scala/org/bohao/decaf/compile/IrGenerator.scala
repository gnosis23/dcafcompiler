package org.bohao.decaf.compile

import org.bohao.decaf.ast._
import org.bohao.decaf.ir._
import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * Created by bohao on 2016/8/31.
  */
object IrGenerator {
    var loopStack = new mutable.Stack[(QLabel, QLabel)]()

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
                val q1 = build(list, locationNode)
                val q2 = build(list, expr)
                val t = createQuad(QAssign(q1, open(q2)))
                list += t
                return list
            case MethodCallStmtNode(loc, call) =>
                build(list, call)
                return list
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
                return list
            case ForStmtNode(loc, id, initExpr, endExpr, step, body) =>
                val labelStart = QLabel()
                val labelTrue = QLabel()
                val labelFalse = QLabel()
                pushLoop(labelStart, labelFalse)
                val index = VarOperand(id)
                val initQuad = build(list, initExpr)
                list += createQuad(QAssign(index, open(initQuad)))
                val endQuad = build(list, endExpr)
                list += labelStart
                val temp = TempVarOperand()
                list += createQuad(QTestle(temp, index, open(endQuad)))
                list += createQuad(QCJmp(temp, labelTrue, labelFalse))
                list += labelTrue
                list ++= build(body)
                if (step != null) {
                    list += createQuad(QAdd(index, index, IntOperand(step.value.get)))
                }
                list += createQuad(QJmp(labelStart))
                list += labelFalse
                popLoop()
                return list
            case WhileStmtNode(loc, cond, body) =>
                val labelStart = QLabel()
                val labelTrue = QLabel()
                val labelFalse = QLabel()
                pushLoop(labelStart, labelFalse)
                list += labelStart
                val condQuad = build(list, cond)
                list += createQuad(QCJmp(open(condQuad), labelTrue, labelFalse))
                list += labelTrue
                list ++= build(body)
                list += QJmp(labelStart)
                list += labelFalse
                popLoop()
                return list
            case ReturnStmtNode(loc, value) =>
                if (value == null) {
                    list += QRet0
                } else {
                    val quad = build(list, value)
                    list += QRet1(open(quad))
                }
                return list
            case BreakStmtNode(loc) =>
                list += QBreak(topLoop()._2)
                return list
            case ContinueStmtNode(loc) =>
                list += QContinue(topLoop()._1)
                return list
        }
    }

    def build(basicBlock: mutable.MutableList[Quad], expr: ExpNode): Quad = {
        expr match {
            case LocationExprNode(loc, locationNode) =>
                return Q1(build(basicBlock, locationNode))
            case MethodCallExprNode(loc, call) =>
                return build(basicBlock, call)
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
                val quad = createQuad(QArrayLen(TempVarOperand(), VarOperand(id.name)))
                basicBlock += quad
                return quad
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
                op match {
                    case "!" =>
                        val q = build(basicBlock, exp)
                        val temp = TempVarOperand()
                        basicBlock += createQuad(QBang(temp, open(q)))
                        return Q1(temp)
                    case "-" =>
                        val q = build(basicBlock, exp)
                        val temp = TempVarOperand()
                        basicBlock += createQuad(QNeg(temp, open(q)))
                        return Q1(temp)
                }
            case CondExprNode(loc, cond, branch1, branch2) =>
                val condQuad = build(basicBlock, cond)
                val labelTrue = QLabel()
                val labelFalse = QLabel()
                val labelEnd = QLabel()
                var quad: Quad = null
                val temp = TempVarOperand()
                basicBlock += createQuad(QCJmp(open(condQuad), labelTrue, labelFalse))
                basicBlock += labelTrue
                quad = build(basicBlock, branch1)
                basicBlock += createQuad(QAssign(temp, open(quad)))
                basicBlock += createQuad(QJmp(labelEnd))
                basicBlock += labelFalse
                quad = build(basicBlock, branch2)
                basicBlock += createQuad(QAssign(temp, open(quad)))
                basicBlock += labelEnd
                return Q1(temp)
        }
        throw new Error("build expression error")
    }

    def build(basicBlock: mutable.MutableList[Quad], call: MethodCallNode): Quad = {
        call match {
            case ExpArgsMethodCallNode(_, name, arguments) =>
                val quads: List[TempVarOperand] = arguments.map(p => {
                    val src = open(build(basicBlock, p))
                    val temp = TempVarOperand()
                    basicBlock += QAssign(temp, src)
                    temp
                }).toList
                quads.foreach(p => {
                    basicBlock += QParam(p)
                })
                val temp = TempVarOperand()
                basicBlock += QCall(temp, VarOperand(name.id.name),
                    IntOperand(quads.length))
                return  Q1(temp)
            case CalloutArgsMethodCallNode(_, name, arguments) =>
                val quads: List[TempVarOperand] = arguments.map(p => {
                    val src = p match {
                        case ExprArgNode(_, exp) =>
                            open(build(basicBlock, exp))
                        case StringArgNode(_, str) =>
                            StrOperand(str.str)
                    }
                    val temp = TempVarOperand()
                    basicBlock += QAssign(temp, src)
                    temp
                }).toList
                quads.foreach(p => {
                    basicBlock += QParam(p)
                })
                val temp = TempVarOperand()
                basicBlock += QCall(temp, VarOperand(name.id.name),
                    IntOperand(quads.length))
                return Q1(temp)
        }
    }

    def build(quads: mutable.MutableList[Quad], locationNode: LocationNode): Lhs = {
        locationNode match {
            case VarLocationExprNode(loc, variable) =>
                VarOperand(variable.name)
            case VarArrayLocationExprNode(loc, variable, exp) =>
                val quad = build(quads, exp)
                ArrayOperand(variable.name, open(quad))
        }
    }

    private def createQuad(quad: Quad): Quad = {
        quad
    }

    private def pushLoop(in: QLabel, out: QLabel): Unit = {
        loopStack.push((in, out))
    }

    private def popLoop(): Unit = {
        loopStack.pop()
    }

    /**
      *
      * @return (again label, out label)
      */
    private def topLoop(): (QLabel, QLabel) = {
        if (loopStack.isEmpty) {
            throw new Error("bad loop")
        }
        loopStack.top
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
            case QNeg(dest, src) => dest
            case QBang(dest, src) => dest
            case QArrayLen(dest, src) => dest
            case QCall(dest, src, src2) => dest
            case QParam(dest) => dest
            case _ => throw new Error("unsupported expression return quad type" + quad)
        }
    }
}
