package org.bohao.decaf.ir

/**
  * Created by bohao on 2016/8/31.
  */
abstract class Quad() {
    var prev: Quad = null
    var next: Quad = null
}

case class Q1(dest: Operand) extends Quad

case class QAdd(dest: Lhs, src1: Operand, src2: Operand) extends Quad {

}

case class QSub(dest: Lhs, src1: Operand, src2: Operand) extends Quad {

}

case class QMul(dest: Lhs, src1: Operand, src2: Operand) extends Quad {

}

case class QDiv(dest: Lhs, src1: Operand, src2: Operand) extends Quad {

}

case class QAssign(dest: Lhs, src: Operand) extends Quad {

}



case class QCJmp(cond: Operand, b1: BasicBlockOperand, b2: BasicBlockOperand) extends Quad{

}

case class QJmp(dest: BasicBlockOperand) extends Quad {

}
