package org.bohao.decaf.ir

import org.bohao.decaf.symbol.ISymbol

/**
  * Created by bohao on 2016/8/31.
  */
abstract class Operand {

}

trait Lhs extends Operand {

}


case class VarOperand(name: String) extends Lhs {
    var symbol: ISymbol = null
}

case class ArrayOperand(name: String, index: Operand) extends Lhs {

}

case class IntOperand(value: Int) extends Operand {

}

case class StrOperand(value: String) extends Operand

case class TempVarOperand(id: Int) extends Lhs {

}

object TempVarOperand {
    var id = 0
    def apply(): TempVarOperand = {
        id += 1
        TempVarOperand(id)
    }
}

case class BasicBlockOperand(block: BasicBlock) extends Operand

case class MemoryPointerOperand(id: Int) extends Lhs

object MemoryPointerOperand {
    var id = 0
    def apply(): MemoryPointerOperand = {
        id += 1
        MemoryPointerOperand(id)
    }
}