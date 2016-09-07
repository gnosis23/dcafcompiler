package org.bohao.decaf.compile

import org.bohao.decaf.ast._
import org.bohao.decaf.ir.IrType.IrType
import org.bohao.decaf.ir._
import org.bohao.decaf.ir.IrType._
import org.bohao.decaf.types.{IType, IntType, VoidType, BoolType}
import scala.collection.JavaConversions._

import scala.collection.mutable.ListBuffer

/**
  * Created by bohao on 2016/9/7.
  */
object IrGenerator2 {
    var ir: Ir2 = null
    var functions: Map[String, IFunction] = Map()
    val builder = new IrBuilder
    var namedValues = List[(String, Lhs)]()

    def build(ast: ProgramNode): Ir2 = {
        ast.methods.foreach(m => {
            functions += parsePrototype(m)
        })

        ast.methods.foreach(m => {
            codegen(m)
        })

        ir = new Ir2(functions.values.toList)
        ir
    }

    def codegen(method: MethodDeclNode): Unit = {
        // FIXME: failed if duplicate name
        val function = functions.get(method.name).get

        val entryBlock = BasicBlock.create("entry", function)
        builder.setInsertPoint(entryBlock)

        // TODO: Global variables
        val oldValues = namedValues
        namedValues = Nil
        method.params.foreach(p => {
            namedValues = (p.variable, ParamOperand(p.variable)) :: namedValues
        })

        codegen(method.block)

        namedValues = oldValues
    }

    def codegen(block: BlockNode): Unit = {
        block.Stmts.foreach(s => {
            codegen(s)
        })
    }

    def codegen(stmt: StmtNode): Unit = {
        stmt match {
            case AssignStmtNode(loc, locationNode, op, expr) =>
            case MethodCallStmtNode(loc, call) =>
            case IfStmtNode(loc, cond, body, elseBody) =>
            case ForStmtNode(loc, id, initExpr, endExpr, step, body) =>
            case WhileStmtNode(loc, cond, body) =>
            case ReturnStmtNode(_, value) =>
                if (value == null) {
                    builder.createRet()
                } else {
                    val quad = codegen(value)
                    builder.createRet(target(quad))
                }
            case BreakStmtNode(loc) =>
            case ContinueStmtNode(loc) =>
            case _ => throw new Error("stmt codegen")
        }
    }

    def codegen(expr: ExpNode): Quad2 = {
        expr match {
            case LocationExprNode(_, locationNode) =>
                locationNode match {
                    case VarLocationExprNode(loc, variable) =>
                        val src = namedValues.find(n => n._1 == variable.name).get._2
                        return T1(src)
                    case VarArrayLocationExprNode(loc, variable, exp) =>
                }
            case MethodCallExprNode(loc, call) =>
                call match {
                    case ExpArgsMethodCallNode(_, name, arguments) =>
                        val argsOperand = arguments.map(x => target(codegen(x))).toList
                        return builder.createCall(builder.getInsertBlock.parent, argsOperand)
                    case CalloutArgsMethodCallNode(_, name, arguments) =>
                }
            case LiteralExprNode(_, v) =>
                v match {
                    case e @ IntLiteralNode(_, text) =>
                        return T1(IntOperand(e.value.get))
                    case CharLiteralNode(_, value) =>
                        return T1(StrOperand(value + ""))
                    case BoolLiteralNode(_, value) =>
                        return T1(IntOperand(if(value) 1 else 0))
                }
            case IdExprNode(loc, id) =>
            case BinExprNode(_, op0, lhs, rhs) =>
                op0 match {
                    case ArithOpNode(_, op) =>
                        val lquad = codegen(lhs)
                        val rquad = codegen(rhs)
                        op match {
                            case "+" =>
                                return builder.createIAdd(target(lquad), target(rquad))
                            case "*" =>
                                return builder.createIMul(target(lquad), target(rquad))
                            case _ => throw new Error(s"unimplemented op $op")
                        }
                    case RelOpNode(_, op) =>
                    case EqOpNode(_, op) =>
                    case CondOpNode(_, op) =>
                    case _ =>
                }
            case UnaryExprNode(loc, op, exp) =>
            case CondExprNode(loc, cond, branch1, branch2) =>
            case _ =>
        }
        throw new RuntimeException("unsupported expr: " + expr)
        null
    }

    def parsePrototype(m: MethodDeclNode): (String, IFunction) = {
        val func = IFunction(parseFunctionType(m), m.name)
        (m.name, func)
    }

    def parseFunctionType(m: MethodDeclNode): FunctionType = {
        val retType = typeConvert(m.t.itype)
        val argTypes = m.params.map(x =>
            (x.variable, typeConvert(x.t.itype))
        )
        FunctionType(retType, argTypes.toList)
    }

    def typeConvert(astType: IType): IrType = {
        astType match {
            case BoolType => INT64
            case VoidType => VOID
            case IntType => INT64
            case _ => throw new Error("argument type error: " + astType)
        }
    }

    private def target(quad: Quad2): Operand = {
        quad match {
            case Br(block) => block
            case Store(mem, startVal) => mem
            case Load(dest, mem) => dest
            case IAdd(dest, src1, src2) => dest
            case ISub(dest, src1, src2) => dest
            case IMul(dest, src1, src2) => dest
            case ICmp(dest, src1) => dest
            case T1(dest) => dest
//            case Ret(value) =>
            case Call(retValue, func, args) => retValue
            case _ => throw new Error("unsupported open quad return " + quad)
        }
    }
}
