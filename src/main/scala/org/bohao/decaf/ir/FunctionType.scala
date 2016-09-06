package org.bohao.decaf.ir

import org.bohao.decaf.ir.IrType.IrType

/**
  * Created by bohao on 2016/9/6.
  */
class FunctionType(var retType: IrType, var args: Seq[IrType],
                   var argNames: Seq[String] = null)
{

}

object FunctionType {
    def apply(retType: IrType, argTypes: List[IrType]): FunctionType = {
        new FunctionType(retType, argTypes)
    }
}