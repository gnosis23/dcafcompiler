package org.bohao.decaf.ir

/**
  * Created by bohao on 2016/9/6.
  */
object IrType extends Enumeration {
    type IrType = Value
    val INT32, INT64, STRING, CHAR, VOID = Value
}
