package org.bohao.decaf.asm

/**
  * Created by bohao on 2016/9/11.
  */
abstract class Inst

// Copying values
case class Mov(src : Op, dest : Op) extends Inst

case class Cmove(src : Register, dest : Register) extends Inst

case class Cmovne(src : Register, dest : Register) extends Inst

case class Cmovg(src : Register, dest : Register) extends Inst

case class Cmovl(src : Register, dest : Register) extends Inst

case class Cmovge(src : Register, dest : Register) extends Inst

case class Cmovle(src : Register, dest : Register) extends Inst

// Stack management
case class Enter(x : Imm) extends Inst

case object Leave extends Inst

case class Push(src : Op) extends Inst

case class Pop(dest : Op) extends Inst


// Control flow
case class Call(func: Func) extends Inst

case object Ret extends Inst

case class Jmp(label: Label) extends Inst

case class Je(label: Label) extends Inst

case class Jne(label: Label) extends Inst

// Arithmetic and logic
case class Add(src : Op, dest : Op) extends Inst

case class Sub(src : Op, dest : Op) extends Inst

case class Imul(src : Op, dest : Op) extends Inst

case class Idiv(src : Op, dest : Op) extends Inst

case class Shr(src : Op, dest : Op) extends Inst

case class Shl(src : Op, dest : Op) extends Inst

case class Ror(src : Op, dest : Op) extends Inst

case class Cmp(src : Op, dest : Op) extends Inst

// Label
case class Tag(label: Label) extends Inst