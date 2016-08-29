package org.bohao.decaf.compile

import java.util

import org.bohao.decaf.ast._
import org.bohao.decaf.symbol.{ISymbol, Env}
import org.bohao.decaf.types._
import scala.collection.JavaConversions._


/**
  * Created by bohao on 2016/8/27.
  */
object SemanticChecker {
    var errorHandler: ErrorHandler = null
    var currentMethod: MethodDeclNode = null
    val DefaultType = IntType
    var breakStack = new scala.collection.mutable.Stack[Int]()

    def pushBreak() = breakStack.push(0)

    def popBreak() = breakStack.pop()

    def check(ast: ProgramNode, errhandler: ErrorHandler): ProgramNode = {
        errorHandler = errhandler

        val global = new Env(None)

        ast.callouts.foreach(p => checkCallout(global, p))

        ast.fields.foreach(p => checkField(global, p))

        ast.methods.foreach(p => {
            currentMethod = p
            checkMethod(global, p)}
        )

        assert(breakStack.isEmpty, "bad break stack.")
        checkMain(ast)

        ast
    }

    def checkMain(ast: ProgramNode): Unit = {
        val method = ast.methods.find(p => p.name == "main")
        if (method.isEmpty || method.get.methodType.paramTypeList.nonEmpty) {
            log(s"error C0003: The program must contain a definition for a method called main that has no parameters")
        }
    }

    def checkCallout(scope: Env, node: CalloutDeclNode): Unit = {
        val symbol = new ISymbol(node.name, CalloutType)
        val result = scope.addSymbol(symbol)
        if (!result)
            log(s"error C0001 ${node.loc}: callout ${node.name} already defined, " +
                s"No identifier is declared twice in the same scope.")
    }

    def checkIntLiteral(intNode: IntLiteralNode): Unit = {
        try {
            intNode.value = Some(Integer.decode(intNode.text))
        } catch {
            case e: Exception =>
                intNode.value = None
                log(s"error C0024 ${intNode.loc}: int ${intNode.text} overflow")
        }
    }

    def checkField(scope: Env, node: FieldDeclNode) = {
        val fieldType = convert(node.t)
        node.names.foreach {
            case VarNameNode(loc, name) =>
                val symbol = new ISymbol(name, fieldType)
                val result = scope.addSymbol(symbol)
                if (!result)
                    log(s"error C0001 ${node.loc}: field $name already defined, " +
                        s"No identifier is declared twice in the same scope.")
            case ArrayNameNode(loc, name, size) =>
                checkIntLiteral(size)
                val len = size.value.getOrElse(1)
                val symbol = new ISymbol(name, new ArrayType(fieldType, len))
                val result = scope.addSymbol(symbol)
                if (!result)
                    log(s"${node.loc}: field $name already defined")
        }
    }

    def checkMethod(scope: Env, methodNode: MethodDeclNode): Unit = {
        val tSymbol: ISymbol = methodToSymbol(methodNode)
        scope.addSymbol(tSymbol)
        methodNode.methodType = tSymbol.kind.asInstanceOf[FunctionType]

        val newEnv = createScope(scope)

        methodNode.params.foreach(p => checkParam(newEnv, p))

        checkBlock(newEnv, methodNode.block)
    }

    def methodToSymbol(methodNode: MethodDeclNode): ISymbol = {
        val retType = convert(methodNode.t)
        val typelist: List[IType] = methodNode.params.map(p => {
            convert(p.t)
        }).toList

        val funcType = FunctionType(typelist, retType)
        ISymbol(methodNode.name, funcType)
    }

    def checkParam(scope: Env, paramNode: ParamNode) = {
        val s = ISymbol(paramNode.variable, convert(paramNode.t))
        scope.addSymbol(s)
    }

    def checkBlock(scope: Env, blockNode: BlockNode): Unit = {
        blockNode.decls.foreach(p => checkField(scope, p))

        blockNode.Stmts.foreach {
            case AssignStmtNode(loc, locationNode, op, expr) =>
                checkLocation(scope, locationNode)
                checkExpression(scope, expr)

                op.op match {
                    case "=" =>
                        checkAssignStmtEqual(locationNode, expr)
                    case "+=" =>
                        checkAssignStmtInt(locationNode, expr)
                    case "-=" =>
                        checkAssignStmtInt(locationNode, expr)
                }

            case MethodCallStmtNode(loc, call) =>
                checkMethodCall(scope, call)
            case IfStmtNode(loc, cond, body, elseBody) =>
                checkExpression(scope, cond)
                if (cond.nodeType != BoolType) {
                    log(s"error C0013 ${cond.loc}: the expression in an if statement " +
                        s"must have type bool")
                }

                var childScope = createScope(scope)
                checkBlock(childScope, body)

                if (elseBody != null) {
                    childScope = createScope(scope)
                    checkBlock(childScope, elseBody)
                }

            case ForStmtNode(loc, id, initExpr, endExpr, step, body) =>

                checkExpression(scope, initExpr)
                checkExpression(scope, endExpr)
                if (initExpr.nodeType != IntType || endExpr.nodeType != IntType) {
                    log(s"error C0021 ${initExpr.loc}: the initial expression and " +
                    s"the ending expression of for must have type int.")
                }
                if (step != null) {
                    checkIntLiteral(step)
                }

                pushBreak()
                val childScope = createScope(scope)
                childScope.addSymbol(ISymbol(id, IntType))
                checkBlock(childScope, body)
                popBreak()

            case WhileStmtNode(loc, cond, body) =>
                checkExpression(scope, cond)
                if (cond.nodeType != BoolType) {
                    log(s"error C0013 ${cond.loc}: the expression in a while statement " +
                        s"must have type bool")
                }

                pushBreak()
                val childScope = createScope(scope)
                checkBlock(childScope, body)
                popBreak()

            case ReturnStmtNode(loc, value) =>
                if (value != null) {
                    checkExpression(scope, value)
                }
                checkReturn(loc, value)
            case BreakStmtNode(loc) =>
                if (breakStack.isEmpty) {
                    log(s"error C0023 $loc: All break statements must be in a for/while")
                }
            case ContinueStmtNode(loc) =>
                if (breakStack.isEmpty) {
                    log(s"error C0023 $loc: All continue statements must be in a for/while")
                }
        }
    }

    def checkAssignStmtEqual(locationNode: LocationNode, expr: ExpNode): Unit = {
        locationNode match {
            case VarLocationExprNode(loc, variable) =>
                if (variable.symbol != null && variable.symbol.kind != expr.nodeType) {
                    log(s"error C0019 ${locationNode.loc}: the assignment statement must have " +
                        s"the same type")
                }
            case VarArrayLocationExprNode(loc, variable, _) =>
                if (variable.symbol != null) {
                    val arrType = variable.symbol.kind
                    arrType match {
                        case ArrayType(baseType, size) =>
                            if (baseType != expr.nodeType) {
                                log(s"error C0019 ${locationNode.loc}: the assignment statement " +
                                    s"must have the same type")
                            }
                        case _ =>
                    }
                }
        }
    }

    def checkAssignStmtInt(locationNode: LocationNode, expr: ExpNode): Unit = {
        locationNode match {
            case VarLocationExprNode(loc, variable) =>
                if (variable.symbol != null) {
                    if (variable.symbol.kind != IntType || expr.nodeType != IntType) {
                        log(s"error C0020 ${locationNode.loc}: " +
                            s"the increment/decrement statement must have " +
                            s"type int")
                    }
                }
            case VarArrayLocationExprNode(loc, variable, _) =>
                if (variable.symbol != null) {
                    val arrType = variable.symbol.kind
                    arrType match {
                        case ArrayType(baseType, size) =>
                            if (baseType != IntType || expr.nodeType != IntType) {
                                log(s"error C0019 ${locationNode.loc}: " +
                                    s"the increment/decrement statement " +
                                    s"must have type int")
                            }
                        case _ =>
                    }
                }
        }
    }

    def checkReturn(loc: Location, expNode: ExpNode): Unit = {
        if (expNode == null) {
            if (currentMethod.methodType.retType != VoidType) {
                log(s"error C0009 $loc: return void, expected ${currentMethod.methodType.retType}")
            }
        } else {
            if (currentMethod.methodType.retType == VoidType) {
                log(s"error C0009 $loc: A return statement must not have a return value " +
                    s"unless it appears in the body of a method " +
                    s"that is declared to return a value.")
            }
        }
    }

    def checkLocation(scope: Env, locationNode: LocationNode): Option[ISymbol] = {
        locationNode match {
            case VarLocationExprNode(loc, variable) =>
                val symb = checkVariable(scope, variable)
                if (symb.isDefined) {
                    checkLocationId(locationNode, variable.name, symb.get)
                    return symb
                }
                None
            case VarArrayLocationExprNode(loc, variable, exp) =>
                val symb = checkVariable(scope, variable)
                checkExpression(scope, exp)
                if (symb.isDefined) {
                    checkLocationArray(locationNode, variable, symb.get, exp)
                    return symb
                }
                None
        }
    }

    /**
      * An id used as a location must name a declared local/global variable or formal parameter.
      *
      */
    def checkLocationId(locationNode: LocationNode, idName: String, symbol: ISymbol): Unit = {
        val t = symbol.kind
        if (t.isInstanceOf[FunctionType] || t == VoidType) {
            log(s"error C0010 ${locationNode.loc}: $idName must name a declared local/global variable or formal parameter.")
        }
    }

    /**
      * For all locations of the form id[expr]
      * - id must be an array variable
      * - the type of expr must be int
      */
    def checkLocationArray(locationNode: LocationNode, variable: VarNode, symbol: ISymbol,
                           exp: ExpNode) = {
        if (!symbol.kind.isInstanceOf[ArrayType]) {
            log(s"error C0011 ${locationNode.loc}: ${variable.name} must be an array variable")
        }
        if (exp.nodeType != IntType) {
            log(s"error C0011 ${locationNode.loc}: array index must be int")
        }
    }



    def checkExpression(scope: Env, expNode: ExpNode): Unit = {
        expNode match {
            case LocationExprNode(loc, locationNode) =>
                val symb = checkLocation(scope, locationNode)
                if (symb.isDefined) {
                    symb.get.kind match {
                        case t: ArrayType =>
                            expNode.nodeType = t.baseType
                        case _ =>
                            expNode.nodeType = symb.get.kind
                    }
                } else {
                    expNode.nodeType = DefaultType
                }
            case MethodCallExprNode(loc, call) =>
                val symb = checkMethodCall(scope, call, checkRet = true)
                if (symb.isDefined) {
                    expNode.nodeType = symb.get
                } else {
                    expNode.nodeType = DefaultType
                }
            case LiteralExprNode(loc, value) =>
                value match {
                    case e @ IntLiteralNode(_, text) =>
                        checkIntLiteral(e)
                        expNode.nodeType = IntType
                    case CharLiteralNode(_, v) =>
                        expNode.nodeType = CharType
                    case BoolLiteralNode(_, v) =>
                        expNode.nodeType = BoolType
                }
            case IdExprNode(loc, id) =>
                // '@' ID
                val symb = checkVariable(scope, id)
                if (symb.isDefined) {
                    checkArrayLengthExpression(expNode, id, symb.get)
                }
                expNode.nodeType = IntType
            case BinExprNode(loc, op, lhs, rhs) =>
                checkExpression(scope, lhs)
                checkExpression(scope, rhs)
                checkOp(op, lhs, rhs)
                op match {
                    case ArithOpNode(loc1, op1) =>
                        expNode.nodeType = IntType
                    case RelOpNode(loc1, op1) =>
                        expNode.nodeType = BoolType
                    case EqOpNode(loc1, op1) =>
                        expNode.nodeType = BoolType
                    case CondOpNode(loc1, op1) =>
                        expNode.nodeType = BoolType
                }
            case UnaryExprNode(loc, op, exp) =>
                checkExpression(scope, exp)
                op match {
                    case "!" =>
                        if (exp.nodeType != BoolType) {
                            log(s"error C0018 $loc: the operands of '!' must be bool.")
                        }
                        expNode.nodeType = BoolType
                    case "-" =>
                        expNode.nodeType = IntType
                }

            case CondExprNode(loc, cond, branch1, branch2) =>
                checkExpression(scope, cond)
                if (cond.nodeType != BoolType) {
                    log(s"error C0014 $loc: the first expression in the ternary expression " +
                        s"must have type bool.")
                }
                checkExpression(scope, branch1)
                checkExpression(scope, branch2)
                if (branch1.nodeType != branch2.nodeType) {
                    log(s"error C0015 $loc: The other two expressions in a ternary conditional " +
                        s"expression must have the same type (integer or boolean)")
                }
                expNode.nodeType = branch1.nodeType
        }
    }

    def checkOp(op: OpNode, lhs: ExpNode, rhs: ExpNode): Unit = {
        op match {
            case ArithOpNode(loc, op1) =>
                if (lhs.nodeType != IntType || rhs.nodeType != IntType) {
                    log(s"error C0016 ${op.loc}: the operands of arithmetic expressions must have type int")
                }
            case RelOpNode(loc, op1) =>
                if (lhs.nodeType != IntType || rhs.nodeType != IntType) {
                    log(s"error C0016 ${op.loc}: the operands of arithmetic expressions must have type int")
                }
            case EqOpNode(loc, op1) =>
                if (lhs.nodeType != rhs.nodeType) {
                    log(s"error C0017 ${op.loc}: the operands of eq must be the same time")
                }
            case CondOpNode(loc, op1) =>
                if (lhs.nodeType != BoolType || rhs.nodeType != BoolType) {
                    log(s"error C0018 ${op.loc}: the operands of &&,|| must be bool.")
                }
        }
    }

    /**
      * The argument of the @ operator must be an array variable
      */
    def checkArrayLengthExpression(expNode: ExpNode, id: VarNode, symb: ISymbol): Unit = {
        if (!symb.kind.isInstanceOf[ArrayType]) {
            log(s"error C0012 ${expNode.loc}: the argument of '@' operator must be an array type")
        }
    }

    def checkParameterLengthAndType(call: MethodCallNode, name: String,
                                    funcDef: FunctionType,
                                    arguments: util.List[ExpNode]): Unit =
    {
        assert(funcDef.paramTypeList != null, "null paramlist")
        assert(arguments != null, "null method call paramlist")

        if (funcDef.paramTypeList.length != arguments.length) {
            log(s"error C0005 ${call.loc}: function $name has" +
                s" ${funcDef.paramTypeList.length} parameter(s), got ${arguments.length}")
        } else {
            val plist = funcDef.paramTypeList
            for (i <- 0 until  arguments.length) {
                if (plist.get(i) != arguments.get(i).nodeType) {
                    log(s"error C0005 ${call.loc}: function $name parameter ${i+1} expected" +
                        s" ${plist.get(i)} , actual ${arguments.get(i).nodeType}")
                }
            }
        }
    }

    /**
      *
      * @return function return type
      */
    def checkMethodCall(scope: Env, methodCallNode: MethodCallNode, checkRet: Boolean = false): Option[IType] = {
        methodCallNode match {
            case ExpArgsMethodCallNode(loc, name, arguments) =>
                val funcDefinition = checkVariable(scope, name.id)
                arguments.foreach(p => checkExpression(scope, p))
                if (funcDefinition.isDefined) {
                    val symbolType = funcDefinition.get.kind
                    symbolType match {
                        case fd: FunctionType =>
                            checkParameterLengthAndType(methodCallNode, name.id.name, fd, arguments)
                            // 6. If a method call is used as an expression, the method must return a result.
                            if (checkRet && fd.retType == VoidType) {
                                log(s"error C0006 ${methodCallNode.loc}: function ${name.id.name} must return a value")
                                return None
                            }
                            return Some(fd.retType)
                        case _ =>
                            log(s"error C0001 ${methodCallNode.loc}: ${name.id.name} is not a function")
                            return None
                    }
                } else {
                    None
                }
            case CalloutArgsMethodCallNode(loc, name, arguments) =>
                checkVariable(scope, name.id)
                arguments.foreach(p => checkCalloutArg(scope, p))
                // cannot check callout functions
                None
        }
    }

    /**
      *
      * @param scope  symbol table
      * @param varNode variable node
      * @return if find return Some(symbol), or None
      */
    def checkVariable(scope: Env, varNode: VarNode): Option[ISymbol] = {
        val symbol = scope.find(varNode.name)
        symbol match {
            case None =>
                log(s"error C0002 ${varNode.loc}: variable '${varNode.name}' undefined")
            case Some(sym) =>
                if (varNode.symbol == null) varNode.symbol = sym
//                Console.out.println(s"VAR ${varNode.loc}: $sym")
        }
        symbol
    }

    def checkCalloutArg(scope: Env, calloutArgNode: CalloutArgNode) = {
        calloutArgNode match {
            case ExprArgNode(loc, exp) =>
                checkExpression(scope, exp)
            case StringArgNode(loc, str) =>
        }
    }

    private def convert(astType: TypeNode): IType = {
        astType match {
            case IntTypeNode(loc) => IntType
            case BoolTypeNode(loc) => BoolType
            case VoidTypeNode(loc) => VoidType
        }
    }

    private def createScope(parent: Env): Env = {
        val t = new Env(Some(parent))
        parent.addChildScope(t)
        t
    }

    private def log(msg: String): Unit = {
        errorHandler.addError(msg)
    }
}
