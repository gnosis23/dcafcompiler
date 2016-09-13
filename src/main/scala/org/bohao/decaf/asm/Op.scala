package org.bohao.decaf.asm

import org.bohao.decaf.asm.RegisterType.RegisterType

/**
  * Created by bohao on 2016/9/11.
  */
abstract class Op

// Immediately value
case class Imm(v : Int) extends Op

case class ImmStr(var v : String) extends Op
// relative memory address
case class RelativeMem(reg : RegisterType, offset : Int = 0) extends Op

case class ArrayRelativeMem(reg : RegisterType, offset : Int, index : Register) extends Op

case class Register(reg : RegisterType) extends Op

case class Func(name : String) extends Op

case class Label(var name : String = "") extends Op
