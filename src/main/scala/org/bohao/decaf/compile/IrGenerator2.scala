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
    var namedValues = List[(String, MemoryPointerOperand)]()
    val blockStack = mutable.Stack[(BasicBlock, BasicBlock)]()

    def build(ast: ProgramNode): Ir2 = {
        ast.methods.foreach(m => {
            functions += parsePrototype(m)
        })

        // TODO: Global variables
        namedValues = Nil

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

        val oldValues = namedValues

        // store parameters to the stack
        method.params.foreach(p => {
            val address = alloc(p.variable)
            builder.createStore(ParamOperand(p.variable), address)
        })

        codegen(method.block)

        namedValues = oldValues
    }

    def codegen(block: BlockNode): Unit = {
        val oldValues = namedValues

        block.decls.foreach(field => {
            // all cast to int
            field.names.foreach {
                case VarNameNode(_, name) =>
                    alloc(name)
                case ArrayNameNode(_, name, size) =>
                    allocArray(name, size.value.get)
            }
        })

        block.Stmts.foreach(s => {
            codegen(s)
        })

        namedValues = oldValues
    }

    def codegen(stmt: StmtNode): Unit = {
        stmt match {
            case AssignStmtNode(_, locationNode, op, expr) =>
                locationNode match {
                    case VarLocationExprNode(loc, variable) =>
                        val src = variableAddress(variable.name)
                        val value = codegen(expr)
                        // TODO: += -=
                        op.op match {
                            case "=" => builder.createStore(target(value), src)
                        }
                    case VarArrayLocationExprNode(loc, variable, exp) =>
                        val mem = variableAddress(variable.name)
                        val index = codegen(exp)
                        val value = codegen(expr)
                        // TODO: += -=
                        op.op match {
                            case "=" =>
                                val temp = builder.createGetElement(mem, target(index))
                                builder.createStore(target(value),
                                    target(temp).asInstanceOf[MemoryPointerOperand])
                        }
                }
            case MethodCallStmtNode(loc, call) =>
                call match {
                    case ExpArgsMethodCallNode(_, name, arguments) =>
                        val argsOperand = arguments.map(x => target(codegen(x))).toList
                        builder.createCall(name.id.name, argsOperand)
                    case CalloutArgsMethodCallNode(_, name, arguments) =>
                        val argsOperand = arguments.map {
                            case ExprArgNode(_, exp) =>
                                target(codegen(exp))
                            case StringArgNode(_, str) =>
                                StrOperand(str.str)
                        }.toList
                        builder.createCall(name.id.name, argsOperand)
                }
            case IfStmtNode(_, cond, body, elseBody) =>
                if (elseBody != null) {
                    val thenBlock = BasicBlock.create("if0then")
                    val elseBlock = BasicBlock.create("if0else")
                    val mergeBlock = BasicBlock.create("if0end")

                    val condValue = codegen(cond)
                    builder.createCondBr(target(condValue), BasicBlockOperand(thenBlock),
                        BasicBlockOperand(elseBlock))

                    currentFunction.addBlock(thenBlock)
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
                    val thenBlock = BasicBlock.create("if1then")
                    val mergeBlock = BasicBlock.create("if1end")

                    val condValue = codegen(cond)
                    builder.createCondBr(target(condValue), BasicBlockOperand(thenBlock),
                        BasicBlockOperand(mergeBlock))

                    currentFunction.addBlock(thenBlock)
                    builder.setInsertPoint(thenBlock)
                    codegen(body)
                    builder.createBr(BasicBlockOperand(mergeBlock))

                    currentFunction.addBlock(mergeBlock)
                    builder.setInsertPoint(mergeBlock)
                }
            case ForStmtNode(loc, id, initExpr, endExpr, step, body) =>
                val condBlock = BasicBlock.create("forcond")
                val thenBlock = BasicBlock.create("forbody")
                val endBlock = BasicBlock.create("forend")

                val oldNamedValues = namedValues
                pushStack(thenBlock, endBlock)

                // alloc memory
                val indexAddress = alloc(id)

                // init
                val initValue = codegen(initExpr)
                builder.createStore(target(initValue), indexAddress)
                builder.createBr(BasicBlockOperand(condBlock))

                currentFunction.addBlock(condBlock)
                builder.setInsertPoint(condBlock)
                val quad0 = codegen(endExpr)
                val a1 = builder.createLoad(indexAddress)
                val t0 = builder.createITestl(target(a1), target(quad0))
                builder.createCondBr(target(t0), BasicBlockOperand(thenBlock),
                    BasicBlockOperand(endBlock))

                currentFunction.addBlock(thenBlock)
                builder.setInsertPoint(thenBlock)
                codegen(body)
                if (step != null) {
                    val a2 = builder.createLoad(indexAddress)
                    val t2 = builder.createIAdd(target(a2), IntOperand(step.value.get))
                    builder.createStore(target(t2), indexAddress)
                } else {
                    val a2 = builder.createLoad(indexAddress)
                    val t2 = builder.createIAdd(target(a2), IntOperand(1))
                    builder.createStore(target(t2), indexAddress)
                }
                builder.createBr(BasicBlockOperand(condBlock))

                currentFunction.addBlock(endBlock)
                builder.setInsertPoint(endBlock)

                popStack()
                namedValues = oldNamedValues

            case WhileStmtNode(loc, cond, body) =>
                val condBlock = BasicBlock.create("whilecond")
                val thenBlock = BasicBlock.create("whilebody")
                val endBlock = BasicBlock.create("whileend")

                pushStack(thenBlock, endBlock)

                // init
                builder.createBr(BasicBlockOperand(condBlock))

                currentFunction.addBlock(condBlock)
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
                        val address = variableAddress(variable.name)
                        return builder.createLoad(address)
                    case VarArrayLocationExprNode(loc, variable, exp) =>
                        val address = variableAddress(variable.name)
                        val index = codegen(exp)
                        val mem = builder.createGetElement(address, target(index))
                        return builder.createLoad(target(mem).asInstanceOf[MemoryPointerOperand])
                }
            case MethodCallExprNode(loc, call) =>
                call match {
                    case ExpArgsMethodCallNode(_, name, arguments) =>
                        val argsOperand = arguments.map(x => target(codegen(x))).toList
                        return builder.createCall(name.id.name, argsOperand)
                    case CalloutArgsMethodCallNode(_, name, arguments) =>
                        val argsOperand = arguments.map {
                            case ExprArgNode(_, exp) =>
                                target(codegen(exp))
                            case StringArgNode(_, str) =>
                                StrOperand(str.str)
                        }.toList
                        return builder.createCall(name.id.name, argsOperand)
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
                val addr = variableAddress(id.name)
                return T1(ArrayLenOperand(addr))
            case BinExprNode(_, op0, lhs, rhs) =>
                op0 match {
                    case ArithOpNode(_, op) =>
                        val lquad = codegen(lhs)
                        val rquad = codegen(rhs)
                        op match {
                            case "+" =>
                                return builder.createIAdd(target(lquad), target(rquad))
                            case "-" =>
                                return builder.createISub(target(lquad), target(rquad))
                            case "*" =>
                                return builder.createIMul(target(lquad), target(rquad))
                            case "/" =>
                                return builder.createIDiv(target(lquad), target(rquad))
                            case "%" =>
                                return builder.createIMod(target(lquad), target(rquad))
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
                        // shortcut , don't eval first
                        op match {
                            case "&&" =>
                                val thenBlock = BasicBlock.create("Andnext")
                                val endBlock = BasicBlock.create("Andend")

                                val temp = TempVarOperand()
                                val lquad = codegen(lhs)
                                builder.createAssign(temp, target(lquad))

                                builder.createCondBr(temp, BasicBlockOperand(thenBlock),
                                    BasicBlockOperand(endBlock))

                                currentFunction.addBlock(thenBlock)
                                builder.setInsertPoint(thenBlock)
                                val rquad = codegen(rhs)
                                builder.createAssign(temp, target(rquad))
                                builder.createBr(BasicBlockOperand(endBlock))

                                currentFunction.addBlock(endBlock)
                                builder.setInsertPoint(endBlock)
                                return T1(temp)
                            case "||" =>
                                val thenBlock = BasicBlock.create("Ornext")
                                val endBlock = BasicBlock.create("Orend")

                                val temp = TempVarOperand()
                                val lquad = codegen(lhs)
                                builder.createAssign(temp, target(lquad))

                                builder.createCondBr(temp, BasicBlockOperand(endBlock),
                                    BasicBlockOperand(thenBlock))

                                currentFunction.addBlock(thenBlock)
                                builder.setInsertPoint(thenBlock)
                                val rquad = codegen(rhs)
                                builder.createAssign(temp, target(rquad))
                                builder.createBr(BasicBlockOperand(endBlock))

                                currentFunction.addBlock(endBlock)
                                builder.setInsertPoint(endBlock)
                                return T1(temp)
                        }
                }
            case UnaryExprNode(_, op, exp) =>
                op match {
                    case "!" =>
                        val value = codegen(exp)
                        return builder.createRev(TempVarOperand(), target(value))
                    case "-" =>
                        val value = codegen(exp)
                        return builder.createNeg(TempVarOperand(), target(value))
                }
            case CondExprNode(_, cond, branch1, branch2) =>
                val value = codegen(cond)
                val temp = TempVarOperand()
                val b1 = BasicBlock.create("ternarya")
                val b2 = BasicBlock.create("ternaryb")
                val endblock = BasicBlock.create("ternaryend")

                builder.createAssign(temp, IntOperand(0))
                builder.createCondBr(target(value), BasicBlockOperand(b1),
                    BasicBlockOperand(b2))

                currentFunction.addBlock(b1)
                builder.setInsertPoint(b1)
                var t0 = codegen(branch1)
                builder.createAssign(temp, target(t0))
                builder.createBr(BasicBlockOperand(endblock))

                currentFunction.addBlock(b2)
                builder.setInsertPoint(b2)
                t0 = codegen(branch2)
                builder.createAssign(temp, target(t0))
                builder.createBr(BasicBlockOperand(endblock))

                currentFunction.addBlock(endblock)
                builder.setInsertPoint(endblock)
                return T1(temp)
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

    /**
      * add the alloca instruction to the function,
      * put the memory address to the symbol tables: namedValues
      * @param id variable name
      * @return memory operand
      */
    private def alloc(id: String): MemoryPointerOperand = {
        val quad = Alloca(MemoryPointerOperand(id))
        currentFunction.addAlloca(quad)
        namedValues = (id, quad.dest) :: namedValues
        quad.dest
    }

    private def allocArray(id: String, size: Int): MemoryPointerOperand = {
        val quad = AllocaArray(MemoryPointerOperand(id), IntOperand(size))
        currentFunction.addAlloca(quad)
        namedValues = (id, quad.dest) :: namedValues
        quad.dest
    }

    private def variableAddress(id: String): MemoryPointerOperand = {
        val ret = namedValues.find(p => p._1 == id)
        ret match {
            case None => throw new Exception(s"bug : find $namedValues $id")
            case Some((_, mem)) => mem
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
            case GetElement(dest, mem, index) => dest
            case Neg(dest, _) => dest
            case Rev(dest, _) => dest
            case _ => throw new Error("unsupported open quad return " + quad)
        }
    }
}
