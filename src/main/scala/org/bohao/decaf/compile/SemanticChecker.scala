package org.bohao.decaf.compile

import org.bohao.decaf.ast._
import org.bohao.decaf.symbol.{ISymbol, Env}
import org.bohao.decaf.types._
import scala.collection.JavaConversions._


/**
  * Created by bohao on 2016/8/27.
  */
object SemanticChecker {
    var errorHandler: ErrorHandler = null

    def check(ast: ProgramNode, errhandler: ErrorHandler): ProgramNode = {
        errorHandler = errhandler

        val global = new Env(None)

        ast.callouts.foreach(p => checkCallout(global, p))

        ast.fields.foreach(p => checkField(global, p))

        ast.methods.foreach(p => {
            checkMethod(global, p)}
        )

        ast
    }

    def checkCallout(scope: Env, node: CalloutDeclNode): Unit = {
        val symbol = new ISymbol(node.name, CalloutType)
        scope.addSymbol(symbol)
    }

    def checkField(scope: Env, node: FieldDeclNode) = {
        val fieldType = convert(node.t)
        node.names.foreach {
            case VarNameNode(loc, name) =>
                val symbol = new ISymbol(name, fieldType)
                scope.addSymbol(symbol)
            case ArrayNameNode(loc, name, size) =>
                // FIXME ...
                val len = Integer.valueOf(size.text)
                val symbol = new ISymbol(name, new ArrayType(fieldType, len))
                scope.addSymbol(symbol)
        }
    }

    def checkMethod(scope: Env, methodNode: MethodDeclNode): Unit = {
        scope.addSymbol(methodToSymbol(methodNode))

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

            case BreakStmtNode(loc) =>
            case ContinueStmtNode(loc) =>
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
                checkMethodCall(scope, call)
            case LiteralExprNode(loc, value) =>
            case IdExprNode(loc, id) =>
                checkVariable(scope, id)
            case BinExprNode(loc, op, lhs, rhs) =>
                checkExpression(scope, lhs)
                checkExpression(scope, rhs)
            case UnaryExprNode(loc, op, exp) =>
                checkExpression(scope, exp)
            case CondExprNode(loc, cond, branch1, branch2) =>
                checkExpression(scope, cond)
                checkExpression(scope, branch1)
                checkExpression(scope, branch2)

        }
    }

    def checkMethodCall(scope: Env, methodCallNode: MethodCallNode): Unit = {
        methodCallNode match {
            case ExpArgsMethodCallNode(loc, name, arguments) =>
                checkVariable(scope, name.id)
                arguments.foreach(p => checkExpression(scope, p))
            case CalloutArgsMethodCallNode(loc, name, arguments) =>
                checkVariable(scope, name.id)
                arguments.foreach(p => checkCalloutArg(scope, p))
        }
    }

    def checkVariable(scope: Env, varNode: VarNode) = {
        val symbol = scope.find(varNode.name)
        symbol match {
            case None =>
                errorHandler.addError(s"${varNode.loc}: variable '${varNode.name}' undefined")
            case Some(sym) => Console.out.println(s"VAR ${varNode.loc}: $sym")
        }
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
}
