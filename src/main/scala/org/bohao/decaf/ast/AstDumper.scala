package org.bohao.decaf.ast

import java.io.{OutputStream, BufferedWriter, PrintStream, Writer}
import java.util
import scala.collection.JavaConversions._

/**
  * Created by bohao on 2016/8/24.
  */
object AstDumper {
    val INDENT_STR = "|   "

    private class IndentPrintStream(out: OutputStream) extends PrintStream(out)
    {
        var padding: String = ""

        def indent() = padding = padding + INDENT_STR

        def unindent() = padding = padding.substring(4)

        override def println(x: String): Unit = {
            super.println(padding + x)
        }
    }

    def dump(program: ProgramNode): Unit = {
        if (program == null)
            Console.out.println("null")
        else
            dump(program, new IndentPrintStream(Console.out))
    }

    def dump(variable: VarNode, out: IndentPrintStream): Unit = {
        out.println(s"Variable ${variable.name}")
    }

    def dump(param: ParamNode, out: IndentPrintStream): Unit = {
        out.println("<ParamNode>")
        out.indent()
        dump(param.t, out)
        dump(param.variable, out)
        out.unindent()
    }

    def dump(lhs: LocationNode, out: IndentPrintStream): Unit = {
        lhs match {
            case VarLocationExprNode(variable) =>
                out.println("<VarLocationExprNode>")
                out.indent()
                dump(variable, out)
                out.unindent()
            case VarArrayLocationExprNode(variable, exp) =>
                out.println("<VarArrayLocationExprNode>")
                out.indent()
                dump(variable, out)
                dump(exp, out)
                out.unindent()
        }
    }

    def dump(op: AssignOpNode, out: IndentPrintStream): Unit = {
        out.println(s"<AssignOpNode> ${op.op}")
    }

    def dump(value: LiteralNode, out: IndentPrintStream): Unit = {
        value match {
            case IntLiteralNode(text) =>
                out.println(s"<IntLiteralNode> $text")
            case CharLiteralNode(ch) =>
                out.println(s"<CharLiteralNode> $ch")
            case BoolLiteralNode(boo) =>
                out.println(s"<BoolLiteralNode> $boo")
        }
    }

    def dump(name: MethodNameNode, out: IndentPrintStream): Unit = {
        out.println("<MethodNameNode>")
        out.indent()
        dump(name.id, out)
        out.unindent()
    }

    def dump1(arguments: util.List[ExpNode], out: IndentPrintStream): Unit = {
        out.println(s"[arguments ${arguments.size()}]")
        out.indent()
        arguments.foreach(a => dump(a, out))
        out.unindent()
    }

    def dump(str: StringLiteralNode, out: IndentPrintStream): Unit = {
        out.println(s"<StringLiteralNode> ${str.str}")
    }

    def dump(arg: CalloutArgNode, out: IndentPrintStream): Unit = {
        arg match {
            case ExprArgNode(exp) =>
                out.println("<ExprArgNode>")
                out.indent()
                dump(exp, out)
                out.unindent()
            case StringArgNode(str) =>
                out.println("<StringArgNode>")
                out.indent()
                dump(str, out)
                out.unindent()
        }
    }

    def dump2(arguments: util.List[CalloutArgNode], out: IndentPrintStream): Unit = {
        out.println(s"[arguments ${arguments.size()}]")
        out.indent()
        arguments.foreach(a => dump(a, out))
        out.unindent()
    }

    def dump(call: MethodCallNode, out: IndentPrintStream): Unit = {
        call match {
            case ExpArgsMethodCallNode(name, arguments) =>
                out.println("<ExpArgsMethodCallNode>")
                out.indent()
                dump(name, out)
                dump1(arguments, out)
                out.unindent()
            case CalloutArgsMethodCallNode(name, arguments) =>
                out.println("<CalloutArgsMethodCallNode>")
                out.indent()
                dump(name, out)
                dump2(arguments, out)
                out.unindent()
        }
    }

    def dump(op0: OpNode, out: IndentPrintStream): Unit = {
        op0 match {
            case ArithOpNode(op) =>
                out.println(s"<ArithOpNode> $op")
            case RelOpNode(op) =>
                out.println(s"<RelOpNode> $op")
            case EqOpNode(op) =>
                out.println(s"<EqOpNode> $op")
            case CondOpNode(op) =>
                out.println(s"<CondOpNode> $op")
        }
    }

    def dump(expr: ExpNode, out: IndentPrintStream): Unit = {
        if (expr == null) { out.println(null); return }
        expr match {
            case LocationExprNode(location) =>
                out.println("<LocationExprNode>")
                out.indent()
                dump(location, out)
                out.unindent()
            case MethodCallExprNode(call) =>
                out.println("<MethodCallExprNode>")
                out.indent()
                dump(call, out)
                out.unindent()
            case LiteralExprNode(value) =>
                out.println(s"<LiteralExprNode>")
                out.indent()
                dump(value, out)
                out.unindent()
            case IdExprNode(id) =>
                out.println(s"<IdExprNode>")
                out.indent()
                dump(id, out)
                out.unindent()
            case BinExprNode(op, lhs, rhs) =>
                out.println(s"<BinExprNode>")
                out.indent()
                dump(op, out)
                dump(lhs, out)
                dump(rhs, out)
                out.unindent()
            case UnaryExprNode(op, exp) =>
                out.println("<UnaryExprNode>")
                out.indent()
                out.println(s"op $op")
                dump(exp, out)
                out.unindent()
            case CondExprNode(cond, branch1, branch2) =>
                out.println("<CondExprNode>")
                out.indent()
                dump(cond, out)
                dump(branch1, out)
                dump(branch2, out)
                out.unindent()
        }
    }

    def dump(stmt: StmtNode, out: IndentPrintStream): Unit = {
        stmt match {
            case AssignStmtNode(location, op, expr) =>
                out.println("<AssignStmtNode>")
                out.indent()
                dump(location, out)
                dump(op, out)
                dump(expr, out)
                out.unindent()
            case MethodCallStmtNode(call) =>
                out.println("<MethodCallStmtCall>")
                out.indent()
                dump(call, out)
                out.unindent()
            case IfStmtNode(cond, body, elseBody) =>
                out.println("<IfStmtNode>")
                out.indent()
                dump(cond, out)
                dump(body, out)
                dump(elseBody, out)
                out.unindent()
            case ForStmtNode(id, initExpr, endExpr, step, body) =>
                out.println("<ForStmtNode>")
                out.indent()
                dump(id, out)
                dump(initExpr, out)
                dump(endExpr, out)
                dump(step, out)
                dump(body, out)
                out.unindent()
            case WhileStmtNode(cond, body) =>
                out.println("<WhileStmtNode>")
                out.indent()
                dump(cond, out)
                dump(body, out)
                out.unindent()
            case ReturnStmtNode(value) =>
                out.println("<ReturnStmtNode>")
                out.indent()
                dump(value, out)
                out.unindent()
            case BreakStmtNode() =>
                out.println("<BreakStmtNode>")
            case ContinueStmtNode() =>
                out.println("<ContinueStmtNode>")
        }
    }

    def dump(block: BlockNode, out: IndentPrintStream): Unit = {
        out.println("<BlockNode>")
        out.indent()

        if (block.decls != null) block.decls.foreach(m => dump(m, out))
        if (block.Stmts != null) block.Stmts.foreach(stmt => dump(stmt, out))

        out.unindent()
    }

    def dump(method: MethodDeclNode, out: IndentPrintStream): Unit = {
        out.println(s"<MethodDeclNode> ${method.name}")
        out.indent()

        dump(method.t, out)

        out.println(s"[Params ${method.params.size()}]")
        out.indent()
        method.params.foreach(m => dump(m, out))
        out.unindent()

        dump(method.block, out)

        out.unindent()
    }

    def dump(program: ProgramNode, out: IndentPrintStream): Unit = {
        out.println("<ProgramNode>")
        out.indent()

        out.println(s"[callout decls ${program.callouts.size()}]")
        program.callouts.foreach(m => dump(m, out))

        out.println(s"[field decls ${program.fields.size()}]")
        program.fields.foreach(f => dump(f, out))

        out.println(s"[method decls ${program.methods.size()}]")
        program.methods.foreach(m => dump(m, out))

        out.unindent()
    }

    def dump(decl: CalloutDeclNode, out: IndentPrintStream): Unit = {
        out.println(s"<CalloutDeclNode> ${decl.name}")
    }

    def dump(t: TypeNode, out: IndentPrintStream): Unit = {
        t match {
            case IntTypeNode() =>
                out.println("Int Type")
            case BoolTypeNode() =>
                out.println("Bool Type")
            case VoidTypeNode() =>
                out.println("Void Type")
        }
    }

    def dump(name: NameNode, out: IndentPrintStream): Unit = {
        name match {
            case VarNameNode(varNode) =>
                out.println(s"<NameNode> ${varNode.name}")
            case ArrayNameNode(varNode, size) =>
                out.println(s"<NameNode> ${varNode.name}[${size.text}]")
        }
    }

    def dump(decl: FieldDeclNode, out: IndentPrintStream): Unit = {
        out.println(s"<FieldDeclNode>")
        out.indent()
        dump(decl.t, out)
        decl.names.foreach(name => dump(name, out))
        out.unindent()
    }
}