package org.bohao.decaf.ir

/**
  * Created by bohao on 2016/8/31.
  */
abstract class Quad() {
    var prev: Quad = null
    var next: Quad = null
}

case class Q1(dest: Operand) extends Quad

case class QArrayLen(dest: Lhs, src1: Operand) extends Quad

case class QAdd(dest: Lhs, src1: Operand, src2: Operand) extends Quad {

}

case class QSub(dest: Lhs, src1: Operand, src2: Operand) extends Quad {

}

case class QMul(dest: Lhs, src1: Operand, src2: Operand) extends Quad {

}

case class QDiv(dest: Lhs, src1: Operand, src2: Operand) extends Quad {

}

case class QMod(dest: Lhs, src1: Operand, src2: Operand) extends Quad {

}

case class QAssign(dest: Lhs, src: Operand) extends Quad {

}

case class QTeste(dest: Lhs, left: Operand, right: Operand) extends Quad

case class QTestne(dest: Lhs, left: Operand, right: Operand) extends Quad

case class QTestg(dest: Lhs, left: Operand, right: Operand) extends Quad

case class QTestge(dest: Lhs, left: Operand, right: Operand) extends Quad

case class QTestl(dest: Lhs, left: Operand, right: Operand) extends Quad

case class QTestle(dest: Lhs, left: Operand, right: Operand) extends Quad


case class QCJmp(cond: Operand, b1: QLabel, b2: QLabel) extends Quad{

}

case class QNeg(dest: Lhs, src: Operand) extends Quad

case class QBang(dest: Lhs, src: Operand) extends Quad

case class QJmp(dest: QLabel) extends Quad {

}

case class QCall(dest: Lhs, name: Operand, count: IntOperand) extends Quad

case class QParam(dest: Lhs) extends Quad

case class QLabel(val id: String) extends Quad {

}

object QLabel {
    var id = 0

    def apply(): QLabel = {
        id += 1
        QLabel("L" + id)
    }
}

object QRet0 extends Quad

case class QRet1(src: Operand) extends Quad

case class QBreak(label: QLabel) extends Quad

case class QContinue(label: QLabel) extends Quad