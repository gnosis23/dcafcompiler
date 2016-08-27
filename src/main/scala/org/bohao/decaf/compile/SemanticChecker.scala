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

    def check(ast: ProgramNode, errhandler: ErrorHandler): ProgramNode = {
        errorHandler = errhandler

        val global = new Env(None)

        ast.callouts.foreach(p => checkCallout(global, p))

        ast.fields.foreach(p => checkField(global, p))

        ast.methods.foreach(p => {
            currentMethod = p
            checkMethod(global, p)}
        )

        checkMain(ast)

        ast
    }

    def checkMain(ast: ProgramNode): Unit = {
        val method = ast.methods.find(p => p.name == "main")
        if (method.isEmpty || method.get.methodType.paramTypeList.nonEmpty) {
            log(s"The program must contain a definition for a method called main that has no parameters")
        }
    }

    def checkCallout(scope: Env, node: CalloutDeclNode): Unit = {
        val symbol = new ISymbol(node.name, CalloutType)
        val result = scope.addSymbol(symbol)
        if (!result)
            log(s"${node.loc}: callout ${node.name} already defined, " +
                s"No identifier is declared twice in the same scope.")
    }

    def checkField(scope: Env, node: FieldDeclNode) = {
        val fieldType = convert(node.t)
        node.names.foreach {
            case VarNameNode(loc, name) =>
                val symbol = new ISymbol(name, fieldType)
                val result = scope.addSymbol(symbol)
                if (!result)
                    log(s"${node.loc}: field $name already defined, " +
                        s"No identifier is declared twice in the same scope.")
            case ArrayNameNode(loc, name, size) =>
                // FIXME ...
                val len = Integer.valueOf(size.text)
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
            case MethodCallStmtNode(loc, call) =>
                checkMethodCall(scope, call)
            case IfStmtNode(loc, cond, body, elseBody) =>
                checkExpression(scope, cond)

                var childScope = createScope(scope)
                checkBlock(childScope, body)

                if (elseBody != null) {
                    childScope = createScope(scope)
                    checkBlock(childScope, elseBody)
                }

            case ForStmtNode(loc, id, initExpr, endExpr, step, body) =>
                val childScope = createScope(scope)
                // TODO: int only?
                childScope.addSymbol(ISymbol(id, IntType))
                checkBlock(childScope, body)

            case WhileStmtNode(loc, cond, body) =>
                checkExpression(scope, cond)
                val childScope = createScope(scope)
                checkBlock(childScope, body)

            case ReturnStmtNode(loc, value) =>
                if (value != null) {
                    checkExpression(scope, value)
                }
                checkReturn(loc, value)
            case BreakStmtNode(loc) =>
            case ContinueStmtNode(loc) =>
        }
    }

    def checkReturn(loc: Location, expNode: ExpNode): Unit = {
        if (expNode == null) {
            if (currentMethod.methodType.retType != VoidType) {
                log(s"$loc: return void, expected ${currentMethod.methodType.retType}")
            }
        } else {
            if (currentMethod.methodType.retType == VoidType) {
                log(s"$loc: A return statement must not have a return value " +
                    s"unless it appears in the body of a method " +
                    s"that is declared to return a value.")
            }
        }
    }

    def checkLocation(scope: Env, locationNode: LocationNode) = {
        locationNode match {
            case VarLocationExprNode(loc, variable) =>
                checkVariable(scope, variable)
            case VarArrayLocationExprNode(loc, variable, exp) =>
                checkVariable(scope, variable)
        }
    }

    def checkExpression(scope: Env, expNode: ExpNode): Unit = {
        expNode match {
            case LocationExprNode(loc, locationNode) =>
                checkLocation(scope, locationNode)
            case MethodCallExprNode(loc, call) =>
                checkMethodCall(scope, call, checkRet = true)
            case LiteralExprNode(loc, value) =>
                value match {
                    case IntLiteralNode(_, text) =>
                        expNode.nodeType = IntType
                    case CharLiteralNode(_, v) =>
                        expNode.nodeType = CharType
                    case BoolLiteralNode(_, v) =>
                        expNode.nodeType = BoolType
                }
            case IdExprNode(loc, id) =>
                checkVariable(scope, id)
            case BinExprNode(loc, op, lhs, rhs) =>
                checkExpression(scope, lhs)
                checkExpression(scope, rhs)
                op match {
                    case ArithOpNode(loc1, op1) =>
                        expNode.nodeType = IntType
                    case RelOpNode(loc1, op1) =>
                        expNode.nodeType = IntType
                    case EqOpNode(loc1, op1) =>
                        expNode.nodeType = BoolType
                        // FIXME int or bool
                    case CondOpNode(loc1, op1) =>
                        expNode.nodeType = BoolType
                }
            case UnaryExprNode(loc, op, exp) =>
                checkExpression(scope, exp)
            case CondExprNode(loc, cond, branch1, branch2) =>
                checkExpression(scope, cond)
                checkExpression(scope, branch1)
                checkExpression(scope, branch2)

        }
    }

    def checkParameterLengthAndType(call: MethodCallNode, name: String,
                                    funcDef: FunctionType,
                                    arguments: util.List[ExpNode]): Unit =
    {
        assert(funcDef.paramTypeList != null, "null paramlist")
        assert(arguments != null, "null method call paramlist")

        if (funcDef.paramTypeList.length != arguments.length) {
            log(s"${call.loc}: function $name has" +
                s" ${funcDef.paramTypeList.length} parameter(s), got ${arguments.length}")
        } else {
            val plist = funcDef.paramTypeList
            for (i <- 0 until  arguments.length) {
                if (plist.get(i).toString != arguments.get(i).nodeType.toString) {
                    log(s"${call.loc}: function $name parameter ${i+1} expected" +
                        s" ${plist.get(i)} , actual ${arguments.get(i).nodeType}")
                }
            }
        }
    }

    def checkMethodCall(scope: Env, methodCallNode: MethodCallNode, checkRet: Boolean = false): Unit = {
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
                                log(s"${methodCallNode.loc}: function ${name.id.name} must return a value")
                            }
                        case _ =>
                            log(s"${methodCallNode.loc}: ${name.id.name} is not a function")
                    }
                }
            case CalloutArgsMethodCallNode(loc, name, arguments) =>
                val funcDefinition = checkVariable(scope, name.id)
                arguments.foreach(p => checkCalloutArg(scope, p))
                // cannot check callout functions
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
                log(s"${varNode.loc}: variable '${varNode.name}' undefined")
            case Some(sym) => Console.out.println(s"VAR ${varNode.loc}: $sym")
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
