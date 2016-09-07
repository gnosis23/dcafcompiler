package org.bohao.decaf.compile

import org.bohao.decaf.ast.{MethodDeclNode, ProgramNode}
import org.bohao.decaf.ir.{IFunction, Ir2, FunctionType}
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

    def build(ast: ProgramNode): Ir2 = {
        ast.methods.foreach(m => {
            functions += parsePrototype(m)
        })

        ir = new Ir2(functions.values.toList)
        ir
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
}
