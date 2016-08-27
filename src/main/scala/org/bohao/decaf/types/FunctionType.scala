package org.bohao.decaf.types

/**
  * Created by bohao on 2016/8/26.
  */
class FunctionType(val paramTypeList: List[IType], val retType: IType) extends IType{

    override def toString = s"FunctionType($paramTypeList, $retType)"
}

object FunctionType {
    def apply(plist: List[IType], retType: IType): FunctionType = {
        new FunctionType(plist, retType)
    }
}