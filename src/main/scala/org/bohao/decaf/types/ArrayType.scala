package org.bohao.decaf.types

/**
  * Created by bohao on 2016/8/26.
  */
class ArrayType(val baseType: IType, val size: Int) extends IType {

    override def toString = s"ArrayType($baseType, $size)"
}
