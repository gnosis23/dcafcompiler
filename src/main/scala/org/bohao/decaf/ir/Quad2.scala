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

case class Alloca(dest: MemoryPointerOperand) extends Quad2

case class Store(mem: MemoryPointerOperand, startVal: Operand) extends Quad2

case class Load(dest: Lhs, mem: MemoryPointerOperand) extends Quad2

case class AllocaArray(dest: MemoryPointerOperand, size: Operand) extends Quad2

case class GetElement(dest: MemoryPointerOperand, mem: MemoryPointerOperand, index: Operand)
    extends Quad2

case class IAdd(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class ISub(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class IMul(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class IDiv(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class IMod(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class ITesteq(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class ITestneq(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class ITestl(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class ITestle(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class ITestg(dest: Lhs, src1: Operand, src2: Operand) extends Quad2

case class ITestge(dest: Lhs, src1: Operand, src2: Operand) extends Quad2


case class Ret(value: Operand) extends Quad2

object Ret0 extends Quad2

case class Call(retValue: Lhs, func: String, args: List[Operand]) extends Quad2

case class T1(value: Operand) extends Quad2

case class Assign(dest: Lhs, value: Operand) extends Quad2

case class Neg(dest: Lhs, value: Operand) extends Quad2

case class Rev(dest: Lhs, value: Operand) extends Quad2
