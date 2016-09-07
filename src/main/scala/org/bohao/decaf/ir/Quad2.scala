package org.bohao.decaf.ir

/**
  * Created by bohao on 2016/9/6.
  */
abstract class Quad2 {
    var name = ""
}

case class Br(block: BasicBlockOperand) extends Quad2

case class CondBr(cond: Operand, thenBody: BasicBlockOperand, elseBody: BasicBlockOperand)
    extends Quad2

case class Store(mem: MemoryPointerOperand, startVal: Operand) extends Quad2

case class Load(dest: Lhs, mem: MemoryPointerOperand) extends Quad2

case class IAdd(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class ISub(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class IMul(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class ICmp(dest: Lhs, src1: Operand) extends Quad2

case class Ret(value: Operand) extends Quad2

object Ret0 extends Quad2

case class Call(retValue: Lhs, func: IFunction, args: List[Operand]) extends Quad2

case class T1(value: Operand) extends Quad2