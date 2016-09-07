package org.bohao.decaf.ir

import org.bohao.decaf.ir.IrType.IrType

/**
  * Created by bohao on 2016/9/6.
  */
case class FunctionType(var retType: IrType, var args: List[(String, IrType)])
{

}
