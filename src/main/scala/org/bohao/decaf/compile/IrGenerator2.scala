package org.bohao.decaf.compile

import org.bohao.decaf.ast._
import org.bohao.decaf.ir.IrType.{IrType, _}
import org.bohao.decaf.ir._
import org.bohao.decaf.types.{BoolType, IType, IntType, VoidType}

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * Created by bohao on 2016/9/7.
  */
object IrGenerator2 {
    var ir: Ir2 = null
    var functions: Map[String, IFunction] = Map()
    var currentFunction: IFunction = null
    val builder = new IrBuilder
    var namedValues = List[(String, Lhs)]()
    val blockStack = mutable.Stack[(BasicBlock, BasicBlock)]()

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
        currentFunction = function

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
            case AssignStmtNode(_, locationNode, op, expr) =>
                locationNode match {
                    case VarLocationExprNode(loc, variable) =>
                        val src = namedValues.find(n => n._1 == variable.name).get._2
                        val value = codegen(expr)
                        op.op match {
                            case "=" => builder.createAssign(src, target(value))
                        }
                    case VarArrayLocationExprNode(loc, variable, exp) =>

                }
            case MethodCallStmtNode(loc, call) =>
                call match {
                    case ExpArgsMethodCallNode(_, name, arguments) =>
                        val argsOperand = arguments.map(x => target(codegen(x))).toList
                        builder.createCall(currentFunction, argsOperand)
                    case CalloutArgsMethodCallNode(_, name, arguments) =>
                }
            case IfStmtNode(_, cond, body, elseBody) =>
                if (elseBody != null) {
                    val thenBlock = BasicBlock.create("then", currentFunction)
                    val elseBlock = BasicBlock.create("else")
                    val mergeBlock = BasicBlock.create("ifcont")

                    val condValue = codegen(cond)
                    builder.createCondBr(target(condValue), BasicBlockOperand(thenBlock),
                        BasicBlockOperand(elseBlock))

                    builder.setInsertPoint(thenBlock)
                    codegen(body)
                    builder.createBr(BasicBlockOperand(mergeBlock))

                    currentFunction.addBlock(elseBlock)
                    builder.setInsertPoint(elseBlock)
                    codegen(elseBody)
                    builder.createBr(BasicBlockOperand(mergeBlock))

                    currentFunction.addBlock(mergeBlock)
                    builder.setInsertPoint(mergeBlock)
                } else {
                    val thenBlock = BasicBlock.create("then", currentFunction)
                    val mergeBlock = BasicBlock.create("ifcont")

                    val condValue = codegen(cond)
                    builder.createCondBr(target(condValue), BasicBlockOperand(thenBlock),
                        BasicBlockOperand(mergeBlock))

                    builder.setInsertPoint(thenBlock)
                    codegen(body)
                    builder.createBr(BasicBlockOperand(mergeBlock))

                    currentFunction.addBlock(mergeBlock)
                    builder.setInsertPoint(mergeBlock)
                }
            case ForStmtNode(loc, id, initExpr, endExpr, step, body) =>
                val condBlock = BasicBlock.create("cond", currentFunction)
                val thenBlock = BasicBlock.create("loop")
                val endBlock = BasicBlock.create("endloop")
                val index = VarOperand(id)

                pushStack(thenBlock, endBlock)

                // init
                val initValue = codegen(initExpr)
                namedValues = (id, index) :: namedValues
                builder.createAssign(index, target(initValue))
                builder.createBr(BasicBlockOperand(condBlock))

                builder.setInsertPoint(condBlock)
                val quad0 = codegen(endExpr)
                val t0 = builder.createITestle(index, target(quad0))
                builder.createCondBr(target(t0), BasicBlockOperand(thenBlock),
                    BasicBlockOperand(endBlock))

                currentFunction.addBlock(thenBlock)
                builder.setInsertPoint(thenBlock)
                codegen(body)
                if (step != null) {
                    val t2 = builder.createIAdd(index, IntOperand(step.value.get))
                    builder.createAssign(index, target(t2))
                } else {
                    val t2 = builder.createIAdd(index, IntOperand(1))
                    builder.createAssign(index, target(t2))
                }
                builder.createBr(BasicBlockOperand(condBlock))

                currentFunction.addBlock(endBlock)
                builder.setInsertPoint(endBlock)

                popStack()

            case WhileStmtNode(loc, cond, body) =>
                val condBlock = BasicBlock.create("cond", currentFunction)
                val thenBlock = BasicBlock.create("while-body")
                val endBlock = BasicBlock.create("end-while")

                pushStack(thenBlock, endBlock)

                // init
                builder.createBr(BasicBlockOperand(condBlock))

                builder.setInsertPoint(condBlock)
                val quad0 = codegen(cond)
                builder.createCondBr(target(quad0), BasicBlockOperand(thenBlock),
                    BasicBlockOperand(endBlock))

                currentFunction.addBlock(thenBlock)
                builder.setInsertPoint(thenBlock)
                codegen(body)
                builder.createBr(BasicBlockOperand(condBlock))

                currentFunction.addBlock(endBlock)
                builder.setInsertPoint(endBlock)

                popStack()

            case ReturnStmtNode(_, value) =>
                if (value == null) {
                    builder.createRet()
                } else {
                    val quad = codegen(value)
                    builder.createRet(target(quad))
                }
            case BreakStmtNode(loc) =>
                // get false block
                val topBlock = blockStack.top._2
                builder.createBr(BasicBlockOperand(topBlock))
            case ContinueStmtNode(loc) =>
                // get true block
                val topBlock = blockStack.top._1
                builder.createBr(BasicBlockOperand(topBlock))
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
                        return builder.createCall(currentFunction, argsOperand)
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
                        val lquad = codegen(lhs)
                        val rquad = codegen(rhs)
                        op match {
                            case ">" =>
                                return builder.createITestg(target(lquad), target(rquad))
                            case ">=" =>
                                return builder.createITestge(target(lquad), target(rquad))
                            case "<" =>
                                return builder.createITestl(target(lquad), target(rquad))
                            case "<=" =>
                                return builder.createITestle(target(lquad), target(rquad))
                        }
                    case EqOpNode(_, op) =>
                        val lquad = codegen(lhs)
                        val rquad = codegen(rhs)
                        op match {
                            case "==" =>
                                return builder.createITesteq(target(lquad), target(rquad))
                            case "!=" =>
                                return builder.createITestneq(target(lquad), target(rquad))
                        }
                    case CondOpNode(_, op) =>
                }
            case UnaryExprNode(loc, op, exp) =>
            case CondExprNode(loc, cond, branch1, branch2) =>
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

    private def pushStack(blockTrue: BasicBlock, blockFalse: BasicBlock): Unit = {
        blockStack.push((blockTrue, blockFalse))
    }

    private def popStack(): Unit = {
        blockStack.pop()
    }

    private def target(quad: Quad2): Operand = {
        quad match {
            case Br(block) => block
            case Store(mem, startVal) => mem
            case Load(dest, mem) => dest
            case IAdd(dest, src1, src2) => dest
            case ISub(dest, src1, src2) => dest
            case IMul(dest, src1, src2) => dest
            case ITesteq(dest, src1, src2) => dest
            case ITestneq(dest, src1, src2) => dest
            case ITestg(dest, src1, src2) => dest
            case ITestge(dest, src1, src2) => dest
            case ITestl(dest, src1, src2) => dest
            case ITestle(dest, src1, src2) => dest
            case ICmp(dest, src1) => dest
            case T1(dest) => dest
//            case Ret(value) =>
            case Call(retValue, func, args) => retValue
            case _ => throw new Error("unsupported open quad return " + quad)
        }
    }
}
